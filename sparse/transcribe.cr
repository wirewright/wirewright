module Ww::Sparse::TermAST
  extend self

  # Evaluates escape sequences in *text*.
  private def unescape(string : String)
    return string if string.empty?

    String.build do |io|
      reader = Char::Reader.new(string)

      while true
        case chr = reader.current_char
        when '\\'
          reader.next_char

          case reader.current_char
          when '"'  then chr = '"'
          when '\\' then chr = '\\'
          when 'b'  then chr = '\b'
          when 'f'  then chr = '\f'
          when 'n'  then chr = '\n'
          when 'r'  then chr = '\r'
          when 't'  then chr = '\t'
          when 'u'
            u1 = reader.next_char
            u2 = reader.next_char
            u3 = reader.next_char
            u4 = reader.next_char
            u5 = reader.next_char
            u6 = reader.next_char
            chr = {u1, u2, u3, u4, u5, u6}.join.to_i(16).chr
          end
        end

        io << chr

        break unless reader.next_char?
      end
    end
  end

  private macro transcribe_case(tree_id, &defs)
    ComptimeStringTrie.match?({{tree_id.id}}) do
      {% for call in defs.body.expressions %}
        {% unless call.is_a?(Call) %}
          {% call.raise "unexpected expression: expected Call" %}
        {% end %}

        {% unless call.name == :given && call.args.size >= 1 && call.args.all?(&.is_a?(StringLiteral)) && call.block %}
          {% call.raise "unexpected call: expected given(*tree ids to accept : StringLiteral, &)" %}
        {% end %}

        {% ids = call.args %}
        {% block = call.block %}

        {% for id in ids %}
          map({{id}}, begin
            {% if block.body %}
              {% if block.body.is_a?(Expressions) %}
                {% for expr in block.body.expressions %}
                  {{expr}}
                {% end %}
              {% else %}
                {{block.body}}
              {% end %}
            {% end %}
          end)
        {% end %}
      {% end %}
    end
  end

  # Configurable transcription of Synthax children view.
  #
  # Yields each transcription of child to the block, followed by its index
  # (within *start_at* and *stop_after*). Expects the block to transform it
  # somehow, or return it as is.
  #
  # - *start_at* specifies the index of the first child to be transcribed
  #   (negative counts from the end)
  # - *stop_after* specifies the index of the child *after* the last one that
  #   should be transcribed (negative counts from the end).
  # - *prefixes* is an optional enumerable of terms to prefix transcription of
  #   children with.
  # - *postfixes* is an optional enumerable of terms to put after the
  #   transcriptions of children.
  # - *lax* redirects transcription to the only child in *children*.
  # - *extend* is an optional enumerable of key-value pairs that are added to
  #   the resulting dictionary.
  private def transcribe?(
    source : String,
    children : Sthx::Tree::ChildrenView,
    *,
    start_at = 0,
    stop_after = -1,
    prefixes = nil,
    postfixes = nil,
    lax = false,
    extend extensions = nil,
    & : Term, Int32 -> Term
  ) : Term?
    if lax && children.size == 1
      return unless child = transcribe?(source, children[0])
      return yield child, 0
    end

    # Support negative indices, snap to begin.
    start_at += children.size if start_at < 0
    start_at = 0 unless 0 <= start_at < children.size

    # Support negative indices, snap to end.
    stop_after += children.size if stop_after < 0
    stop_after = children.size - 1 unless 0 <= stop_after < children.size

    dict = Term::Dict.build do |commit|
      prefixes.try &.each { |prefix| commit.with(commit.size, prefix) }

      (start_at..stop_after).each do |index|
        return unless term = transcribe?(source, children[index])

        commit.with(commit.size, (yield term, index))
      end

      postfixes.try &.each { |postfix| commit.with(commit.size, postfix) }
      extensions.try &.each { |k, v| commit.with(k, v) }
    end

    dict.upcast
  end

  private def transcribe?(source : String, children : Sthx::Tree::ChildrenView, **kwargs) : Term?
    transcribe?(source, children, **kwargs, &.itself)
  end

  private def transcribe?(source : String, tree : Sthx::Tree) : Term?
    transcribe_case(tree.id) do
      # Symbol literal, e.g. `'foo`.
      given "symbol" do
        Term.of(Term::Sym.new(source[tree.begin...tree.end]))
      end

      # String literal, e.g. `"hello world\n"`.
      given "string" do
        Term.of(unescape(source[tree.begin...tree.end]))
      end

      # Number literal (not fraction), e.g. `1234` or `-1234.5678`.
      given "number" do
        decimal = source[tree.begin...tree.end].to_big_d

        # Math is going to be much cheaper on an integer. But sadly we have to
        # do to_big_d anyway because Crystal's to_i seems to be unable to detect
        # overflow.
        Term.of(decimal.integer? && Int32::MIN <= decimal <= Int32::MAX ? decimal.to_i : decimal)
      end

      # Boolean literal `true`.
      given "true" { Term.of(true) }

      # Boolean literal `false`.
      given "false" { Term.of(false) }

      # Any (`any`, `all`, `*`, `_`) literal.
      given "any" { Term.of({:any?}) }

      # Represents a type constraint. In Sparse, the following type constraints
      # are available:
      #
      # - `any` aka `_` aka `all`, `*` at toplevel: allpass, the term may be of any type
      # - `number`: the term must be a number (`Term::Num`)
      # - `string`: the term must be a string (`Term::Str`)
      # - `symbol`: the term must be a symbol (`Term::Sym`)
      # - `dict`: the term must be a dictionary (`Term::Dict`)
      # - `boolean`: the term must be a boolean (`Term::Boolean`)
      given "typename" do
        case source[tree.begin...tree.end]
        when "any"     then Term.of({:any?})
        when "number"  then Term.of({:number?})
        when "string"  then Term.of({:string?})
        when "symbol"  then Term.of({:symbol?})
        when "dict"    then Term.of({:dict?})
        when "boolean" then Term.of({:boolean?})
        end
      end

      # Empty dictionary such as `[]` or `{}`.
      given "emptydict" do
        Term.of({:dict?})
      end

      # Fraction literal, e.g. `1/3`.
      given "fraction" do
        return unless num = tree.getattr?("num")
        return unless den = tree.getattr?("den")

        Term.of(BigRational.new(num.to_big_i, den.to_big_i))
      end

      # E.g.:
      # - `{ x 100 }`
      # - `{ a.b.c 100 }`
      # - `{ "foo" 100 }`
      # - `{ (@): 100 }`
      given "pair/entry" do
        transcribe?(source, tree.children, prefixes: {:pair})
      end

      # E.g.:
      # - `{ @a.b.c }` is the same as `{ c: @a.b.c }`
      # - `{ @a.b.(@(name or 100)) }` is the same as `{ @(name or 100): @a.b.(@(name or 100)) }`
      # - `{ @ }` is the same as `{ @: @ }`
      given "pair/hole" do
        return unless hole = transcribe?(source, tree.children[0]?)

        items = hole.items
        return unless items.size >= 2
        return unless items[0] == Term.of(:hole)

        Term.of(:pair, items[items.size - 1], hole)
      end

      # E.g. (note the dot at the end):
      # - `{ x. }`
      # - `{ a.b.c. }`
      # - `{ a.(@b).c. }`
      given "pair/key" do
        transcribe?(source, tree.children, prefixes: {:pair}, postfixes: { {:any?} })
      end

      # A conjunction of constraints. Conjunction is implicit syntactically
      # and is achieved by simply listing constraints in a row. For instance,
      # `0..=100 /? 10 /? 15` is a conjunction of three constraints: `0..=100`,
      # `/? 10`, and `/? 15`.
      #
      # Dictionary syntax e.g. `{ x 100, y 200, z 300 }` is a shorthand for a
      # conjunction of pairs, in this case `{ x 100 } { y 200 } { z 300 }`.
      given "conjunction", "dict" do
        transcribe?(source, tree.children, prefixes: {:&}, lax: true)
      end

      # Perform the following transformations:
      #
      # 1. `["x", "y", "z"]`
      # 2. `{ 0: "x", 1: "y", 2: "z" }`
      # 3. `{0: "x"} {1: "y"} {2: "z"}`
      given "list" do
        transcribe?(source, tree.children, prefixes: {:&}, lax: true) do |term, index|
          Term.of(:pair, Term.of(index), term)
        end
      end

      # Expects the term to be a number divisible by *factor*.
      #
      # The syntax for this is e.g. `/? 10`.
      given "div_by" do
        return unless factor = transcribe?(source, tree.children[0]?)

        Term.of(:"/?", factor)
      end

      # Expects the term to be a number that falls within some range. Either
      # end can be omitted. Both ends can be omitted as well. In such cases
      # the range is equivalent to `number`.
      #
      # Syntax:
      #
      # - Inclusive begin: `100`; exclusive begin: `100<`.
      # - Inclusive end: `=100`; exclusive end: `<100`.
      # - In range, begin and end are separated by `..`. For example, `100..=200`,
      #   `100<..<300`, `100<..=200`. For readability, the use of whitespace is
      #   advised: `100 .. = 200`, `100 < .. < 300`, `100 < .. = 200`.
      #
      # Sparse ranges are the "multi-tool" of comparison:
      #
      # - Less than N: `..< N`
      # - Less than or equal to N: `..= N`
      # - Greater than N: `N >..`
      # - Greater than or equal to N: `N ..` (or `N =..` if you prefer consistency).
      given "range" do
        b = transcribe?(source, tree.dig?(":begin:", 0))
        e = transcribe?(source, tree.dig?(":end:", 0))
        return Term.of({:number?}) unless b || e

        range = Term.of({:range})

        case tree.getattr?("bop")
        when "<"        then range = range.with(:bx, b)
        when "=", .nil? then range = range.with(:b, b)
        else
          return
        end

        case tree.getattr?("eop")
        when "<"        then range = range.with(:ex, e)
        when "=", .nil? then range = range.with(:e, e)
        else
          return
        end

        range.upcast
      end

      # Holes are a gateway to sensor modification at runtime. They're deeply
      # similar to *hydration* in web dev: you make a bunch of mountpoints
      # (holes) in the document (sensor), and populate them with HTML (Sparse) or
      # maintain them according to the logic of the app/framework you're using
      # (in Wirewright that'd be node kernel, and no framework thank goodness!).

      # E.g.:
      # - `@`
      # - `@ or 100`
      # - `@ or /? 10 /? 15` is the same as `(@ or /? 10) /? 15`
      # - `@ or (/? 10 /? 15)` is the same as `(@ or /? 10 /? 15)`
      given "hole/self" do
        Term.of(:hole,
          default: transcribe?(source, tree.children[0]?),
          escaped: tree.getattr("kind") == "@",
        )
      end

      # E.g.:
      #
      # - `@(a or 100)`
      # - `@(a.b.c or 100)`
      # - `@(a.(@foo).c or 100)`
      given "hole/default" do
        return unless default = transcribe?(source, tree.children[-1]?)

        transcribe?(source, tree.children,
          stop_after: -2,
          prefixes: {:hole},
          extend: {
            escaped: tree.getattr("kind") == "@",
            default: default,
          },
        )
      end

      # E.g.:
      #
      # - `@a`
      # - `@a.b.c`
      # - `@a.(@foo).c`
      given "hole/nodefault" do
        transcribe?(source, tree.children,
          prefixes: {:hole},
          extend: {escaped: tree.getattr("kind") == "@"}
        )
      end

      # Enables one to transform the input term using various helper functions,
      # such as size(), upcase(), sin(), etc.
      given "call" do
        return unless head = tree.children[0]?
        return unless name = head.getattr?("name")
        return unless signature = transcribe?(source, head.children, prefixes: {Term::Sym.new(name)})
        return unless call = transcribe?(source, tree.children, start_at: 1, prefixes: {:"->>", signature})

        call
      end

      # Rescuer offers a way to "rescue" invalid expressions (e.g. hole filled with
      # wrong value, hole not filled, etc.). The first valid expression out of the
      # variants presented is used.
      #
      # The syntax for this is e.g.: `/? @x rescue @y rescue 100`. If `/? @x` is invalid
      # `@y` will be used. If `@y` is invalid then `100` will be used.
      #
      # This is particularly useful for initializing things like min/max-finders, e.g.
      # `{ n ..<@min rescue number }`; followed by a mapper that maps `n` to `min`,
      # a sensor with such a query will first match any number `n` and then only those
      # that are smaller than the current value of `min`, which triggers an update
      # to `min` and so on.
      given "rescuer" do
        transcribe?(source, tree.children, prefixes: {:rescue}, lax: true)
      end

      # Top level, corresponds to a Sparse query: pattern followed by counterpatterns,
      # if any.
      given "sparse" do
        transcribe?(source, tree.children, prefixes: {:sparse})
      end
    end
  end

  # Pass `nil` through for convenience.
  private def transcribe?(source : String, tree : Nil) : Nil
  end

  # Converts a Synthax tree for a Sparse query or part thereof to the corresponding
  # TermAST. The latter is used from this point onwards as the single representation
  # of Sparse queries. It is also human-writable from WwML so you can machine-generate
  # queries easily (easier than composing strings).
  def transcribe(source : String, tree : Sthx::Tree) : Term
    transcribe?(source, tree) || raise "BUG: malformed tree, cannot transcribe"
  end
end
