module Ww
  # Symbols are a bit like points in geometry: they represent an identity. In case of
  # symbols, the identity is their name. Symbols are designed to be cheap; most of
  # the things you can do with symbols are in nanosecond range (e.g. <20ns on my
  # machine for methods such as `new`).
  #
  # It is especially important that symbols are very cheap to compare and hash:
  # basically an integer comparison or hash, which are very fast on modern hardware.
  #
  # There is a subset of symbols called *blanks*: symbols that have special meaning
  # for pattern matching engines (e.g. `M0`, `M1`). Blank parsing is done inside
  # the symbol code, mostly for historical reasons and because it "just works", as
  # an idea, so there's no need to change it in any way.
  #
  # When written using WwML, *nonblank symbols* look like `x`, `qux`, `π`, etc.
  # A *blank symbol* could look like `x_number` (a *singular blank*, `<name>_<type?>`),
  # `xs_number*`, or `xs_number+` (*plural* blanks or *polyblanks*, with the multiplicity
  # of "zero-or-more" and one-or-more, correspondingly: `<name>_<type?><mult:*|+>`).
  #
  # NOTE: Due to optimizations related to blank parsing, you are discouraged from
  # using underscores `_` in symbol names, as in `foo_bar`. This will work and probably
  # will be almost as fast, but still, at least philosophically, you should stay away
  # from such symbols: they're too easily confused with blanks.
  @[Term::Assoc(TermType::Symbol, :unsafe_as_sym)]
  struct Term::Sym
    include Equality
    include AutoUpcast
    include TypeConversion

    # Represents blank multiplicity: how many terms a blank wants to match.
    enum BlankMult : UInt8
      One
      ZeroOrMore
      OneOrMore

      def plural? : Bool
        zero_or_more? || one_or_more?
      end
    end

    # :nodoc:
    def initialize(@bits : UInt64)
    end

    # :nodoc:
    def self.new(repr : Spec::Repr) : Sym
      new(Spec.pack(repr))
    end

    # Constructs a symbol term from the given *source* string.
    def self.new(string : String | Bytes) : Sym
      new(Spec.parse(string))
    end

    # Returns the empty symbol.
    def self.empty : Sym
      new(0u64)
    end

    # Constructs a blank symbol.
    def self.blank(name : String, type : TermType, mult : BlankMult = :one) : Sym
      new(Spec.pack(Spec.blank(name, type, mult)))
    end

    # Constructs a blank from an existing symbol *prev*. In some code paths
    # this can skip work (especially encoding the name) vs. the other overload.
    #
    # NOTE: If *prev* is a blank, its *type* and *mult* will be changed instead of
    # constructing a new symbol with *prev* as its name! Refer to the `String` overload
    # for that, use also `prev.to(String)`.
    def self.blank(prev : Sym, type : TermType, mult : BlankMult = :one) : Sym
      new(Spec.pack(Spec.blank(Spec.unpack(Spec::Repr, prev.@bits), type, mult)))
    end

    # Compares this and *other* symbols.
    #
    # Comparison is performed on their string representation (`inspect`).
    def <=>(other : Sym) : Int32
      Spec.compare(Spec.unpack(Spec::Repr, @bits), Spec.unpack(Spec::Repr, other.@bits))
    end

    # Returns `true` if this symbol is the empty symbol.
    def empty? : Bool
      self == Sym.empty
    end

    # Returns `true` if this symbol is prefixed by *chars*.
    #
    # "Prefixed by" means something must follow the prefix, visually: either
    # a blank definition or more symbol characters.
    def prefixed_by?(*chars : Char) : Bool
      repr = Spec.unpack(Spec::Repr, @bits)

      index = 0
      prefix = false

      Spec.each_name_char(repr) do |chr|
        unless current = chars[index]?
          # chars={'a', 'b'⏏}, ab⏏c
          prefix = true
          break
        end

        unless current == chr
          return false
        end

        index += 1
      end

      if index < chars.size
        # chars={'a', 'b'}, a
        return false
      end

      assert index == chars.size

      if prefix
        # chars={'a', 'b'}, abc
        return true
      end

      !!Spec.blank?(repr)
      # chars={'a','b'}, ab⏏         -- nothing is "prefixed by" `ab`, return false
      # chars={'a','b'}, ab⏏_number  -- `_number` is "prefixed by" `ab`, return true
    end

    # Drops at most *nchars* chars from this symbol's string representation,
    # and reparses the resulting string as a symbol.
    #
    # Effectively, this is an optimized way to do something along the lines of:
    # `Term::Sym.new(sym.to(StringView).ldrop(nchars).to_s)`.
    #
    # ```
    # sym = Term[:"^\qux"]
    # sym.ldrop(2)   # => qux
    # sym.ldrop(123) # => ⸝⸍ (empty symbol)
    # ```
    def ldrop(nchars : Int) : Sym
      assert nchars >= 0

      bytes = uninitialized UInt8[64]
      byteary = stack_alloc Pf::Kit::HybridArray(UInt8, 64).new(bytes.to_unsafe)

      repr = Spec.unpack(Spec::Repr, @bits)

      Spec.each_name_char(repr) do |chr|
        unless nchars == 0
          nchars -= 1
          next
        end

        chr.each_byte do |byte|
          byteary << byte
        end
      end

      if byteary.size > 64
        suffix = String.new(byteary.size) do |buffer|
          byteary.unsafe_copy_to(buffer)
          {byteary.size, 0} # 0 asks String to compute #size lazily
        end
      else
        suffix = Slice.new(bytes.to_unsafe, byteary.size, read_only: true)
      end

      Sym.new(suffix)
    end

    # Returns the *name* of this symbol.
    #
    # If it is a blank, its name part is returned. E.g. in `x_number`, the name
    # is `x`, and in `x_number_number`, the name is `x_number` (another, "nested", blank).
    #
    # If it is a nonblank, e.g., `x`, then its name is `self`.
    def name : Sym
      blank?.try(&.name) || self
    end

    # Represents the result of a successful interpretation of a symbol as a blank.
    struct Blank
      # Returns the name of this blank.
      #
      # - For instance, `x_` and `x_number` are named blanks (both are named `x`).
      # - On the other hand, `_` and `_number` are unnamed blanks (both names are
      #   the empty symbol).
      #
      # See also: `name?`.
      getter name : Sym

      # Returns the type associated with this blank.
      #
      # - For instance, `x_` has the type `TermType::Any`.
      # - On the other hand, `x_number` has the type `TermType::Number`, and `x_string`
      #   has the type `TermType::String`, etc.
      getter type : TermType

      # Returns the *multiplicity* of this blank. See `BlankMult`.
      getter mult : BlankMult

      # :nodoc:
      def initialize(@name, @type, @mult)
      end

      # Returns the name of this blank, or `nil` if its name is the empty symbol.
      def name? : Sym?
        name.empty? ? nil : name
      end

      # Returns `true` if this blank has a name.
      def named? : Bool
        !name.empty?
      end

      # Returns `true` if this blank has a type specified.
      def typed? : Bool
        !type.any?
      end

      # Returns `true` if this blank is intended for one and only one value.
      def singular? : Bool
        mult.one?
      end

      # Returns `true` if this blank is intended for matching multiple values.
      def plural? : Bool
        mult.plural?
      end
    end

    # Tries to parse this symbol as a blank. Returns `Blank` if successful,
    # `false` otherwise.
    @[Dncast]
    def blank? : Sym::Blank?
      repr = Spec.unpack(Spec::Repr, @bits)
      return unless blank = Spec.blank?(repr)

      if blank.amb
        name = Sym.new(Spec.parse(blank.name))
      else
        name = Sym.new(Spec.repr(blank.name))
      end

      Blank.new(name, blank.type, blank.mult)
    end

    # Raised by `blank` when the symbol it's called on is not a blank.
    class BlankError < Exception
      def initialize
        super("expected a blank")
      end
    end

    # Same as `blank?`, but raises `BlankError` instead of returning `nil`;
    # thereby asserting this symbol is a blank.
    @[Dncast]
    def blank : Sym::Blank
      blank? || raise BlankError.new
    end

    def to?(type : String.class) : String
      String.build do |io|
        Spec.write(io, Spec.unpack(Spec::Repr, @bits))
      end
    end

    def inspect(io)
      ML.compact(io, self)
    end

    def_equals @bits
  end

  # Frequently used symbols.

  SYM_EDGE = Term[:edge]

  SYM_BLANK_ANY     = Term[:_]
  SYM_BLANK_DICT    = Term[:_dict]
  SYM_BLANK_NUMBER  = Term[:_number]
  SYM_BLANK_SYMBOL  = Term[:_symbol]
  SYM_BLANK_STRING  = Term[:_string]
  SYM_BLANK_BOOLEAN = Term[:_boolean]
end

require "./sym/symcode"
require "./sym/spec"
