# Functions and utilities for handling the `rack.schema` node. The `rack.schema`
# node is not a standalone node. Instead, it is referenced by IO-related nodes
# such as `rack.ws`.
module Ww::Rack::Schema
end

struct Ww::Rack::Schema::JSON
  # :nodoc:
  getter entries : Hash(Term, Array(Entry))

  # :nodoc:
  def initialize(@entries)
  end

  # Constructs an empty JSON schema.
  def self.empty : self
    new({} of Term => Array(Entry))
  end

  alias Op = Blank | Literal | FieldSet | Seq | ArrayOf | Choice | RuleRef

  defrecord Blank, type : TermType
  defrecord Literal, term : Term
  defrecord FieldSet, fields : Hash(String, Field), min : UInt32, open : Bool
  defrecord Seq, values : Slice(Op)
  defcase ArrayOf, value : Op, min : UInt32, max : UInt32
  defrecord Choice, options : Slice(Op)
  defrecord RuleRef, name : Term

  def FieldSet.new(fields : Hash(String, Field), open : Bool)
    min = fields.count { |_, field| field.is_a?(RequiredField) }.to_u32
    FieldSet.new(fields, min, open)
  end

  alias Field = RequiredField | OptionalField

  defrecord RequiredField, key : Term, name : Term, value : Op
  defrecord OptionalField, key : Term, name : Term, value : Op, default : Term?

  private def self.repr(term : Term) : String
    term.type.string? ? term.to(String) : ML.compact(term)
  end

  private def self.fields(rows : Indexable(Term)) : Hash(String, Field)
    fields = {} of String => Field

    rows.each do |row|
      field = Term.case(row) do
        # |@ rack.schema.field
        #
        # |@pattern
        # (field key_ value_ ⍊ ⋮name)
        #
        # |@key key
        # The key is treated literally (not an operator!)
        #
        # |@key value rack.schema.operator
        #
        # |@key name
        # Allows you to rename the key.
        matchpi %{[field key_ value_]} do
          name = row[:as]? || key
          RequiredField.new(key, name, operator(value))
        end

        # |@ rack.schema.field
        #
        # |@pattern
        # (field? key_ value_ ⍊ ⋮name ⋮default)
        #
        # |@key key
        # The key is treated literally (not an operator!)
        #
        # |@key value rack.schema.operator
        #
        # |@key name
        # Allows you to rename the key.
        #
        # |@key default
        # If the field is missing and *default* is not set, the entry will be
        # omitted from the resulting term. If *default* is set, the entry will
        # be created.
        matchpi %{[field? key_ value_]} do
          name = row[:as]? || key
          default = row[:default]?
          OptionalField.new(key, name, operator(value), default)
        end

        otherwise { }
      end

      next if field.nil?

      fields[repr(field.key)] = field
    end

    fields
  end

  def self.operator(term : Term) : Op
    # |@ rack.schema.operator

    Term.case(term) do
      # |@ rack.schema.operator.array
      #
      # |@pattern
      # (array value_ ⍊ min⋮ 0 max⋮ 1024)
      #
      # |@key value rack.schema.operator
      matchpi %{[array value_]} do
        min = term[:min]?.as_n?.try(&.index32?) || 0u32
        max = term[:max]?.as_n?.try(&.index32?) || 1024u32

        ArrayOf.new(operator(value), min, max)
      end

      # |@ rack.schema.operator.any
      #
      # |@pattern
      # [any options_*]
      #
      # |@key options rack.schema.operator
      matchpi %{[any _*]} do
        subterms = term.items.move(1)
        options = subterms.to_readonly_slice { |subterm| operator(subterm) }
        Choice.new(options)
      end

      # |@ rack.schema.operator.literal
      #
      # |@pattern
      # [literal term_]
      matchpi %{[literal term_]} do
        Literal.new(term)
      end

      # |@ rack.schema.operator.literal
      #
      # |@pattern
      # [seq values_*]
      #
      # |@key values rack.schema.operator
      matchpi %{[seq _*]} do
        subterms = term.items.move(1)
        Seq.new(subterms.to_readonly_slice { |subterm| operator(subterm) })
      end

      # |@ rack.schema.operator.object
      #
      # |@pattern
      # [object fields_* ...]
      #
      # |@key fields rack.schema.field
      matchpi %{[object _* ...]} do
        FieldSet.new(fields(term.items.move(1)), open: true)
      end

      # |@ rack.schema.operator.object
      #
      # |@pattern
      # [object fields_*]
      #
      # |@key fields rack.schema.field
      matchpi %{[object _*]} do
        FieldSet.new(fields(term.items.move(1)), open: false)
      end

      # |@ rack.schema.operator.ref
      #
      # |@pattern
      # [ref name_]
      matchpi %{[ref name_]} do
        RuleRef.new(name)
      end

      # |@ rack.schema.operator.blank
      #
      # |@pattern
      # (%symbol blank)
      matchpi %{_symbol} do
        continue unless blank = term.as_sym.blank?
        continue unless blank.singular?

        Blank.new(blank.type)
      end

      # |@ rack.schema.operator.literal
      #
      # |@pattern
      # _number
      # _string
      # _symbol
      # _boolean
      # _dict
      # _blob
      otherwise do
        Literal.new(term)
      end
    end
  end

  alias Entry = Rule

  defrecord Rule, op : Op

  def self.new(document : Term) : self
    unless document = document.as_d?
      return empty
    end

    entries = {} of Term => Array(Entry)

    document.items.each do |item|
      Term.case(item) do
        matchpi %{[rule name_ op_]} do
          entry = Rule.new(operator(op))
          overloads = entries.put_if_absent(name) { [] of Entry }
          overloads << entry
        end

        otherwise { }
      end
    end

    new(entries)
  end

  # A generic / fallback term reading function.
  private def self.read?(parser : ::JSON::PullParser) : Term?
    case parser.kind
    in .null?
      parser.read_null
    in .bool?
      Term.of(parser.read_bool)
    in .int?
      Term.of(parser.read_int)
    in .float?
      Term.of(parser.read_float)
    in .string?
      Term.of(parser.read_string)
    in .begin_array?
      result = Term::Dict.build do |commit|
        parser.read_array do
          commit << read?(parser)
        end
      end

      Term.of(result)
    in .begin_object?
      result = Term::Dict.build do |commit|
        parser.read_object do |key, _|
          commit.with(Term.of(key), read?(parser))
        end
      end

      Term.of(result)
    in .end_array?, .end_object?, .eof?
      parser.raise("unexpected input")
    end
  end

  private def self.read(op : Term::Num.class, parser) : Term
    case parser.kind
    when .int?   then Term.of(parser.read_int)
    when .float? then Term.of(parser.read_float)
    else
      parser.raise("expected a number")
    end
  end

  private def self.read(op : Term::Str.class, parser) : Term
    Term.of(parser.read_string)
  end

  private def self.read(op : Term::Sym.class, parser) : Term
    Term.of(Term::Sym.new(parser.read_string))
  end

  private def self.read(op : Term::Boolean.class, parser) : Term
    Term.of(parser.read_bool)
  end

  private def self.read(op : Term::Dict.class, parser) : Term
    case parser.kind
    when .begin_array?, .begin_object?
      read?(parser) || unreachable
    else
      parser.raise("expected an object")
    end
  end

  private def self.read(op : Term::Blob.class, parser) : Term
    data = parser.read_string

    result = Term::Blob.build do |io|
      Base64.decode(data, io)
    rescue Base64::Error
      parser.raise("invalid base64 string")
    end

    Term.of(result)
  end

  private def self.read?(schema, op : Blank, parser) : Term?
    case op.type
    in .any?     then read?(parser)
    in .number?  then read(Term::Num, parser)
    in .string?  then read(Term::Str, parser)
    in .symbol?  then read(Term::Sym, parser)
    in .boolean? then read(Term::Boolean, parser)
    in .dict?    then read(Term::Dict, parser)
    in .blob?    then read(Term::Blob, parser)
    end
  end

  private def self.read?(schema, op : Literal, parser) : Term?
    expected = Term[op.term]

    # FIXME: This doesn't work for dictionaries! I think we must decompose
    # dictionaries at compile-time into primitive reads, sequences, etc.
    case {expected, parser.kind}
    when {Term::Num, .int?},
         {Term::Num, .float?},
         {Term::Str, .string?},
         {Term::Sym, .string?},
         {Term::Boolean, .bool?},
         {Term::Dict, .begin_object?},
         {Term::Dict, .begin_array?},
         {Term::Blob, .string?}
      term = read(expected.class, parser)
      unless expected == term
        parser.raise("unexpected value")
      end
    end

    op.term
  end

  private def self.read?(schema, op : FieldSet, parser) : Term?
    result = Term::Dict.build do |commit|
      parser.read_object do |key, _|
        unless field = op.fields[key]?
          if op.open
            parser.skip
            next
          end

          parser.raise("unexpected key")
        end

        value = read?(schema, field.value, parser)

        # See also:
        # https://stackoverflow.com/questions/21832701/does-json-syntax-allow-duplicate-keys-in-an-object
        commit.with(field.name, value)
      end

      op.fields.each do |_, field|
        next unless field.is_a?(OptionalField)
        next unless default = field.default
        next if field.name.in?(commit)

        commit.with(field.name, default)
      end

      if commit.size < op.min
        parser.raise("missing keys")
      end
    end

    Term.of(result)
  end

  private def self.read?(schema, op : ArrayOf, parser) : Term?
    result = Term::Dict.build do |commit|
      parser.read_array do
        if commit.size + 1 > op.max
          parser.raise("array is too large")
        end

        commit << read?(schema, op.value, parser)
      end

      if commit.size < op.min
        parser.raise("array is too small")
      end
    end

    Term.of(result)
  end

  private def self.read?(schema, op : Seq, parser) : Term?
    result = Term::Dict.build do |commit|
      parser.read_begin_array
      op.values.each do |value|
        commit << read?(schema, value, parser)
      end
      parser.read_end_array
    end

    Term.of(result)
  end

  private def self.read?(schema, op : Choice, parser) : Term?
    choose(schema, op.options, parser) do |option, subparser|
      read?(schema, option, subparser)
    end
  end

  private def self.read?(schema, op : RuleRef, parser) : Term?
    read?(schema, op.name, parser)
  end

  private def self.read?(schema, top : Rule, parser) : Term?
    read?(schema, top.op, parser)
  end

  private def self.read?(schema, top : Term, parser) : Term?
    unless overloads = schema.entries[top]?
      # FIXME: Why are we doing this at runtime? Do a compile-time check
      # while building Schema!
      parser.raise("no such rule: `#{top}`")
    end

    choose(schema, overloads, parser) do |subparser, overload|
      read?(schema, overload, subparser)
    end
  end

  # TODO: this is probably really slow! We can at least try using `parser.skip` but
  # then Crystal's PullParser doesn't expose byte offsets; which makes me think we
  # need a custom, properly designed parser with cheap backtracking. Read_raw seems
  # to have a pretty expensive implementation...
  private def self.choose(schema, options, parser : ::JSON::PullParser, &)
    # Fast path.
    if option = options.single?
      return read?(schema, option, parser)
    end

    raw = parser.read_raw

    options.each do |option|
      subparser = ::JSON::PullParser.new(raw)
      begin
        return read?(schema, option, subparser)
      rescue ::JSON::ParseException
      end
    end

    parser.raise("no matching option")
  end

  def self.read(schema : self, top : Term, document : String) : Term
    parser = ::JSON::PullParser.new(document)
    read?(schema, top, parser) || parser.raise("toplevel null not allowed")
  end

  def self.read(document : String) : Term
    parser = ::JSON::PullParser.new(document)
    read?(parser) || parser.raise("toplevel null not allowed")
  end
end
