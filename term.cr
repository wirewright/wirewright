module Ww
  class MathDomainError < Exception
  end

  # Lists the possible types of terms.
  enum TermType : UInt8
    # WARNING! This enum is assumed to contain at most 8 values (0-7)
    # by the rest of the code.

    Any     = 0
    Number  = 1
    String  = 2
    Symbol  = 3
    Boolean = 4
    Dict    = 5

    def self.parse(cls : Term::Num.class)
      TermType::Number
    end

    def self.parse(cls : Term::Str.class)
      TermType::String
    end

    def self.parse(cls : Term::Sym.class)
      TermType::Symbol
    end

    def self.parse(cls : Term::Boolean.class)
      TermType::Boolean
    end

    def self.parse(cls : Term::Dict.class)
      TermType::Dict
    end

    def self.parse(cls : Term.class)
      TermType::Any
    end

    def symbolize : ::Symbol
      case self
      in .any?     then :any
      in .number?  then :number
      in .string?  then :string
      in .symbol?  then :symbol
      in .boolean? then :boolean
      in .dict?    then :dict
      end
    end

    def check : Sparse::Unit::Base
      case self
      in .any?     then Sparse::Unit::IsAny.new
      in .number?  then Sparse::Unit::IsNum.new
      in .string?  then Sparse::Unit::IsStr.new
      in .symbol?  then Sparse::Unit::IsSym.new
      in .boolean? then Sparse::Unit::IsBoolean.new
      in .dict?    then Sparse::Unit::IsDict.new
      end
    end

    def subtype?(other : TermType)
      other.any? || self == other
    end
  end

  module ITerm
    def to_json(builder : JSON::Builder)
      upcast.to_json(builder)
    end

    # Attempts to convert this term to the given Crystal *type*. If unsuccessful,
    # returns `nil`.
    def to?(type)
    end

    # Same as `to?`, but raises `TypeCastError` instead of returning `nil`.
    def to(type)
      to?(type) || raise TypeCastError.new
    end

    def downcast
      self
    end

    def upcast : Term
      Term[self]
    end

    def ==(other : Term)
      self == other.downcast
    end

    def ===(other : Term)
      self === other.downcast
    end

    # Writes a string representation of this term to *io*.
    def to_s(io)
      inspect(io)
    end

    # Automatically upcasts `self` to `Term` and tries to run *call* on it.
    macro method_missing(call)
      {% unless Term.has_method?(call.name) %}
        {% raise "#{call}: no such method in Term, cannot automatically upcast" %}
      {% end %}

      # Oh Crystal gods, just add Def#callable_by?(Call) or something...
      {% candidate = Term.methods.find { |method| method.name == call.name && (method.args.size == call.args.size || method.splat_index) } %}
      {% unless candidate %}
        {% raise "#{call}: no such method in term, cannot automatically upcast" %}
      {% end %}

      {% if call.named_args && !candidate.double_splat %}
        {% raise "#{call}: giving named arguments to a double-splatless method is unsupported during automatic upcast" %}
      {% end %}

      upcast.{{call}}
    end
  end

  # Terms are thread-safe values, the primary operand in Wirewright. Sometimes
  # they are passed around as comprehensible *messages*, sometimes they are
  # serialized, packed and sent over the network, sometimes indexed, and sometimes
  # manipulated as data. Terms are also the code and data for WwML, a language
  # with S-expression-like syntax used to program nodes.
  #
  # All terms (including dictionary terms `Term::Dict`) are persistent, thread-safe,
  # and immutable.
  struct Term
    enum Tag : UInt8
      Dict    = 0u8 # << MUST be here
      NumRat  = 1u8
      NumInt  = 2u8
      Str     = 3u8
      Sym     = 4u8
      Boolean = 5u8
      # Left for future use: 6u8 7u8
    end

    # :nodoc:
    def initialize(@mem : Void*)
    end

    def self.new(parser : JSON::PullParser)
      Term[IR.new(parser)]
    end

    # Constructs a generic `Term` instance from the given number *term*.
    def self.[](term : Num) : Term
      Term.new(Pointer(Void).new(term.@k.@mem.address))
    end

    # :nodoc:
    def unsafe_as_n : Num
      Num.new(Num::Kernel.new(@mem))
    end

    # Constructs a generic `Term` instance from the given string *term*.
    def self.[](term : Str) : Term
      Term.new(Pointer(Void).new(term.@value.as(Void*).address | Tag::Str.value))
    end

    # :nodoc:
    def unsafe_as_s : Str
      Str.new(Pointer(Void).new(@mem.address >> 3 << 3).as(String))
    end

    # Constructs a generic `Term` from the given symbol *term*.
    def self.[](term : Sym) : Term
      data = term.@id.to_u64
      data |= term.@flags.value.to_u64 << 32
      data |= term.@type.value.to_u64 << 32 + 8
      Term.new(Pointer(Void).new((data << 3) | Tag::Sym.value))
    end

    # :nodoc:
    def unsafe_as_sym : Sym
      data = @mem.address >> 3
      id = (data & 0xffffffff).to_u32
      data >>= 32
      flags = Sym::Flags.new((data & 0xff).to_u8)
      data >>= 8
      type = TermType.new((data & 0xff).to_u8)
      Sym.new(id, type, flags)
    end

    # Constructs a generic `Term` from the given boolean *term*.
    def self.[](term : Boolean) : Term
      if term.true?
        Term.new(Pointer(Void).new(1 << 3 | Tag::Boolean.value))
      else
        Term.new(Pointer(Void).new(Tag::Boolean.value))
      end
    end

    # :nodoc:
    def unsafe_as_b : Boolean
      Boolean.new((@mem.address >> 3) == 1)
    end

    # Constructs a generic `Term` from the given dictionary *term*.
    def self.[](term : Dict) : Term
      Term.new(term.as(Void*))
    end

    # :nodoc:
    def unsafe_as_d : Dict
      @mem.as(Dict)
    end

    def to_json(builder : JSON::Builder)
      to_ir.to_json(builder)
    end

    # :nodoc:
    def tag : Tag
      Tag.new((@mem.address & 0b111).to_u8)
    end

    # Returns the `TermType` corresponding to this term.
    def type : TermType
      case tag
      in .num_int?, .num_rat? then TermType::Number
      in .str?                then TermType::String
      in .sym?                then TermType::Symbol
      in .boolean?            then TermType::Boolean
      in .dict?               then TermType::Dict
      end
    end

    # Returns `self`.
    def upcast : Term
      self
    end

    # Returns one of concrete structs corresponding to `Term`.
    def downcast : ITerm
      case tag
      in .num_int?, .num_rat? then unsafe_as_n
      in .str?                then unsafe_as_s
      in .boolean?            then unsafe_as_b
      in .sym?                then unsafe_as_sym
      in .dict?               then unsafe_as_d
      end
    end

    # Attempts to cast this term into a number term `Term::Num`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_n : Num
      case tag
      when .num_int?, .num_rat?
        unsafe_as_n
      else
        raise TypeCastError.new
      end
    end

    # Attempts to cast this term into a string term `Term::Str`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_s : Str
      tag.str? ? unsafe_as_s : raise TypeCastError.new
    end

    # Attempts to cast this term into a boolean term `Term::Boolean`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_b : Boolean
      tag.boolean? ? unsafe_as_b : raise TypeCastError.new
    end

    # Attempts to cast this term into a symbol term `Term::Sym`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_sym : Sym
      tag.sym? ? unsafe_as_sym : raise TypeCastError.new
    end

    # Attempts to cast this term into a dictionary term `Term::Dict`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_d : Dict
      tag.dict? ? unsafe_as_d : raise TypeCastError.new
    end

    # Returns a dictionary where *key* is bound to `self`, followed by
    # key-value pairs from *rest* (if any).
    def pack(key, **rest) : Dict
      Term[**rest].with(key, self)
    end

    # Computes and returns the hexdigest of this term using the given *algorithm*.
    def hexdigest(*, algorithm : Digest = Digest::SHA256.new) : String
      digest = IO::Digest.new(IO::Empty.new, algorithm, mode: IO::Digest::DigestMode::Write)
      inspect(digest)
      digest.final.hexstring
    end

    # Writes a string representation of this term to *io*.
    def to_s(io)
      inspect(io)
    end

    def inspect(io)
      downcast.inspect(io)
    end

    # Returns `true` if this and *other* terms are equal.
    def ==(other : Term) : Bool
      @mem == other.@mem || downcast == other.downcast
    end

    # :ditto:
    def ==(other : ITerm) : Bool
      downcast == other
    end

    def hash(hasher)
      downcast.hash(hasher)
    end

    # Automatically downcasts `self` to one of `ITerm` includers and tries to
    # run *call* on it.
    macro method_missing(call)
      {% found_some = false %}
      {% return_types = [] of ::NoReturn %}

      pass do
        %instance = downcast
        {% for candidate in ITerm.includers %}
          {% for method in candidate.methods %}
            {% if method.name == call.name %}
              {% found_some = true %}
              {% if method.return_type %}
                {% return_types << method.return_type.resolve %}
              {% end %}
              if %instance.is_a?({{candidate}})
                break %instance.{{call}}
              end
            {% end %}
          {% end %}
        {% end %}

        {% return_types = return_types.uniq %}

        {% if found_some && !return_types.empty? && call.name.ends_with?("?") %} # Question-method
          {% if return_types == [::Bool] %}
            break false # Question method supposed to return bool, return false
          {% elsif return_types[0].nilable? %}
            break # Question method supposed to return nil, return nil
          {% end %}
        {% end %}

        raise TypeCastError.new("method {{call}} not found on #{%instance.class}")
      end

      {% unless found_some %}
        {% raise "no such method in any ITerm includer: #{call.name}, cannot autocast" %}
      {% end %}
    end
  end

  # Smart constructors

  struct Term
    # Downcasts `Term` for compatibility with other `[]` constructors.
    def self.[](object : Term)
      object.downcast
    end

    # Constructs a number term from the given number *object*.
    def self.[](object : Number) : Num
      Num.new(object)
    end

    # Constructs a string term from the given string *object*.
    def self.[](object : String) : Str
      Str.new(object)
    end

    # Constructs a string term from the given character *object*.
    def self.[](object : Char) : Str
      Str.new(object.to_s)
    end

    # Constructs a symbol term from the given symbol *object*.
    def self.[](object : Symbol) : Sym
      Sym.new(object.to_s)
    end

    # Constructs a boolean term from the given boolean *object*.
    def self.[](object : Bool) : Boolean
      Boolean.new(object)
    end

    # Constructs a number term from the given enum *object*.
    def self.[](object : Enum) : Num
      Num.new(object.value)
    end

    # Constructs an indexed dictionary from the given enumerable *object*.
    # Elements of *object* receive successive keys 0, 1, 2, etc.
    #
    # See also: `#with`.
    def self.[](object : Enumerable) : Dict
      Dict.build do |commit|
        object.each_with_index do |el, i|
          next if el.nil?
          commit.with(i, el)
        end
      end
    end

    {% for cls in %w(Hash NamedTuple) %}
      # Constructs a dictionary from the given hash or named tuple *object*.
      #
      # See also: `#with`.
      def self.[](object : {{cls.id}}) : Dict
        Dict.build do |commit|
          object.each do |k, v|
            next if v.nil?
            commit.with(k, v)
          end
        end
      end
    {% end %}

    # Constructs a dictionary from the given `JSON::Any` *object*.
    #
    # Raises `ArgumentError` on `null`.
    def self.[](object : JSON::Any) : Term
      if (value = object.as_f? || object.as_s? || object.as_a? || object.as_h? || object.as_bool?).nil?
        raise ArgumentError.new
      end

      Term.of(value)
    end

    # Constructs a term from the given term IR *object*.
    #
    # Opposite of `#to_ir`.
    def self.[](object : IR) : Term
      Term[object.to_term]
    end

    # Passes `nil` through so you can safely construct off nilable types
    # and get a nilable term as the result.
    def self.[](object : Nil) : Nil
    end

    # :nodoc:
    def self.[](object)
      raise TypeCastError.new
    end

    # Constructs an empty dictionary.
    def self.[] : Dict
      Dict::EMPTY
    end

    # Constructs an items-only dictionary with *items* (each receiving successive
    # keys 0, 1, 2, etc.)
    def self.[](*items) : Dict
      Term[items]
    end

    # Constructs an entries-only dictionary with *entries*.
    def self.[](**entries) : Dict
      Term[entries]
    end

    # Constructs a dictionary that contains both *items* (each receiving successive
    # keys 0, 1, 2, etc.), and *entries*.
    def self.[](*items, **entries) : Dict
      Dict.build do |commit|
        items.each_with_index do |el, i|
          next if el.nil?
          commit.with(i, el)
        end
        entries.each do |k, v|
          next if v.nil?
          commit.with(k, v)
        end
      end
    end

    # Same as `.[]` but upcasts to generic `Term` for you.
    def self.of(*args, **kwargs) : Term
      Term[*args, **kwargs].upcast
    end
  end
end

require "./term/num"
require "./term/str"
require "./term/sym"
require "./term/boolean"
require "./term/dict"
require "./term/ir"
require "./term/match"
require "./term/trie"
require "./term/map"
