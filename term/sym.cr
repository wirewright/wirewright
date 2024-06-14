module Ww
  # Represents a symbol. Mainly for use in WwML.
  struct Term::Sym
    include ITerm

    ENCODE             = {} of String => UInt32
    DECODE             = [] of String
    ENCODE_DECODE_LOCK = Mutex.new

    # Represents the result of a successful interpretation of a symbol as a blank.
    struct Blank
      # Returns the type associated with this blank.
      #
      # - For instance, `x_` has the type `TermType::Any`.
      # - On the other hand, `x_number` has the type `TermType::Number`, and `x_string`
      #   has the type `TermType::String`, etc.
      getter type : TermType

      # Returns the name of this blank if it has one.
      #
      # - For instance, `x_` and `x_number` are named blanks (both are named `x`).
      # - On the other hand, `_` and `_number` are unnamed blanks (both names are `nil`).
      getter? name : Term::Sym?

      # Returns `true` if this blank is intended for one or more values.
      getter? poly : Bool

      def initialize(@type, @name, *, @poly)
      end
    end

    @[Flags]
    enum Flags : UInt8
      # This symbol is a blank.
      #
      # Makes no sense to combine this with `Polyblank`.
      Blank

      # This symbol is a polyblank.
      #
      # Makes no sense to combine this with `Blank`.
      Polyblank

      # This symbol is a named blank/polyblank.
      #
      # Must be combined with Blank or Polyblank, otherwise makes no sense.
      Named
    end

    # :nodoc:
    #
    # @id - 32 bits
    # @type - 8 bits
    # @flags - 8 bits
    # => sizeof(Sym) = 48 bits
    def initialize(@id : UInt32, @type : TermType, @flags : Flags)
    end

    private def self.parse(string : String) : {String, TermType, Flags}
      type = TermType::Any
      flags = Flags::None

      # The purpose of this ugly piece of code is to allocate just one string --
      # the one for the rchopped name, and hopefully not even it as most symbols
      # aren't blanks.
      #
      # This is a pretty hot place so we better not do anything too "smart" here.
      # Optimizing this shaved off 1s from WwML tests that I have right now, out of 2s
      # they took before -- to give you a feeling of how hot this place is.

      if string.bytesize > 1 && string.ends_with?('+')
        flags |= Flags::Polyblank
        if name = string.rchop?("_number+")
          type = TermType::Number
        elsif name = string.rchop?("_symbol+")
          type = TermType::Symbol
        elsif name = string.rchop?("_string+")
          type = TermType::String
        elsif name = string.rchop?("_boolean+")
          type = TermType::Boolean
        elsif name = string.rchop?("_dict+")
          type = TermType::Dict
        elsif name = string.rchop?("_+")
        else
          name = string.rchop('+')
        end
      elsif name = string.rchop?("_number")
        type = TermType::Number
      elsif name = string.rchop?("_symbol")
        type = TermType::Symbol
      elsif name = string.rchop?("_string")
        type = TermType::String
      elsif name = string.rchop?("_boolean")
        type = TermType::Boolean
      elsif name = string.rchop?("_dict")
        type = TermType::Dict
      elsif name = string.rchop?("_")
      end

      if name # Found _... possibly followed by +
        flags |= Flags::Named unless name.empty?
        flags |= Flags::Blank unless flags.polyblank?
        return name, type, flags
      end

      {string, TermType::Any, Flags::None}
    end

    def self.new(string : String)
      ENCODE_DECODE_LOCK.synchronize do
        name, type, flags = parse(string)

        id = ENCODE.put_if_absent(name) do
          DECODE << name
          DECODE.size.to_u32 - 1
        end

        new(id, type, flags)
      end
    end

    # Returns a `Blank` object if this symbol is a polyblank (blank intended
    # for one or more values). Returns `nil` otherwise.
    def polyblank? : Blank?
      return unless @flags.polyblank?

      Blank.new(@type, @flags.named? ? self : nil, poly: true)
    end

    # Returns a `Blank` object if this symbol is a blank (intended for a single
    # value to be put in). Returns `nil` otherwise.
    def blank? : Blank?
      return unless @flags.blank?

      Blank.new(@type, @flags.named? ? self : nil, poly: false)
    end

    # :inherit:
    def to_ir : IR
      IR.new(IR::Type::Symbol, DECODE[@id], children: nil)
    end

    def inspect(io)
      io << DECODE[@id]

      return unless @flags.blank? || @flags.polyblank?

      io << '_' # Humpf... What if the user omits it?

      case @type
      in .any?
      in .number?  then io << "number"
      in .string?  then io << "string"
      in .symbol?  then io << "symbol"
      in .boolean? then io << "boolean"
      in .dict?    then io << "dict"
      end

      return unless @flags.polyblank?

      io << '+'
    end

    def_equals_and_hash @id
  end

  # Frequently used symbols.

  SYMBOL_BIND_AT = Term[:"bind@"]
  SYMBOL_HELD_AT = Term[:"held@"]
  SYMBOL_HOLD    = Term[:hold]
  SYMBOL_PLACE   = Term[:place]
  SYMBOL_PASTE   = Term[:paste]
  SYMBOL_EMBED   = Term[:embed]
end
