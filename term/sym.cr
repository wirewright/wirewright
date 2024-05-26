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

      def initialize(@type, @name, @poly = false)
      end
    end

    # :nodoc:
    def initialize(@id : UInt32)
    end

    def self.new(id : String)
      ENCODE_DECODE_LOCK.synchronize do
        enc_id = ENCODE.put_if_absent(id) do
          DECODE << id
          DECODE.size.to_u32 - 1
        end

        new(enc_id)
      end
    end

    def string
      DECODE[@id]
    end

    # Returns a `Blank` object if this symbol is a polyblank (blank intended
    # for one or more values). Returns `nil` otherwise.
    def polyblank? : Blank?
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
      elsif string.size > 1 && (name = string.rchop?("_+") || string.rchop?("+"))
        type = TermType::Any
      elsif string == "_+"
        type = TermType::Any
      end

      return unless name && type

      Blank.new(type, name.empty? ? nil : Term::Sym.new(name), poly: true)
    end

    # Returns a `Blank` object if this symbol is a blank (intended for a single
    # value to be put in). Returns `nil` otherwise.
    def blank? : Blank?
      if name = string.rchop?("_number")
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
        type = TermType::Any
      end

      return unless name && type

      Blank.new(type, name.empty? ? nil : Term::Sym.new(name))
    end

    # :inherit:
    def to_ir : IR
      IR.new(IR::Type::Symbol, string, children: nil)
    end

    def inspect(io)
      io << string
    end

    def_equals_and_hash @id
  end
end
