module Ww::ML
  # Represents a WwML-specific classification of some Unicode character.
  #
  # Classification of characters is useful to rid the lexeme reader -- which
  # is likely to attempt to look at the same character "from different
  # perspectives" -- from extra work.
  struct Rune
    # Lists the available character categories.
    enum Category : UInt8
      Invalid
      Other
      BOI
      EOI
      Hspace
      Vspace
      SymbolicWeak
      SymbolicStrongLetter
      SymbolicStrongDigit
      SymbolicStrongMisc
      PairedLeft
      PairedRight
      SubDigit
      SubPlus
      SubMinus
      SupDigit
      SupPlus
      SupMinus
      Ideogram

      # Returns `true` if the character is a non-digit subscript character.
      # Returns `false` otherwise.
      def sub_symbol? : Bool
        sub_plus? || sub_minus?
      end

      # Returns `true` if the character is a non-digit superscript character.
      # Returns `false` otherwise.
      def sup_symbol? : Bool
        sup_plus? || sup_minus?
      end

      # Returns `true` if the character is a strong symbolic character.
      # Returns `false` otherwise.
      def symbolic_strong? : Bool
        symbolic_strong_letter? || symbolic_strong_digit? || symbolic_strong_misc?
      end

      # Returns `true` if the character is a weak or strong symbolic character.
      # Returns `false` otherwise.
      def symbolic? : Bool
        symbolic_weak? || symbolic_strong?
      end

      # Returns `true` if the character is vertical or horizontal whitespace.
      # Returns `false` otherwise.
      def space? : Bool
        hspace? || vspace?
      end

      # Returns `true` if the character is a WwML "content" character.
      def content? : Bool
        symbolic? || paired_left? || ideogram? || subscript? || superscript?
      end

      # Returns `true` if the character is a WwML "visual boundary" character.
      def visual_boundary? : Bool
        space? || paired_left? || paired_right? || eoi?
      end

      # Returns `true` if the character is a subscript digit or symbol.
      # Returns `false` otherwise.
      def subscript? : Bool
        sub_digit? || sub_symbol?
      end

      # Returns `true` if the character is a superscript digit or symbol.
      # Returns `false` otherwise.
      def superscript? : Bool
        sup_digit? || sup_symbol?
      end
    end

    # :nodoc:
    def initialize(@kernel : UInt32)
    end

    # :nodoc:
    def self.new(category : Category, chr : Char)
      new((chr.ord.to_u32 << 8) | category.value.to_u32)
    end

    # :nodoc:
    #
    # Memory is cheap these days so we can afford this monstrosity.
    TABLE = Slice(Rune).new(0xd7ff + 1) do |index|
      case chr = index.unsafe_chr
      when '(', '[', '{', '⟨', '"', '⎡', '⸢', '⸨', '⟦', '⸤', '⸍'
        new(:paired_left, chr)
      when ')', ']', '}', '⟩', '⎤', '⸣', '⸩', '⟧', '⸥', '⸝'
        new(:paired_right, chr)
      when ' ', '\t', '\r'
        new(:hspace, chr)
      when '\n'
        new(:vspace, chr)
      when .in_set?("'$%+\\-\\^<=>")
        new(:symbolic_weak, chr)
      when .in_set?("_!&*./#?~|∞°∈∉⊆⊂∪∩\\")
        new(:symbolic_strong_misc, chr)
      when .in_set?("0-9")
        new(:symbolic_strong_digit, chr)
      when .letter?
        new(:symbolic_strong_letter, chr)
      when '₀'..'₉'
        new(:sub_digit, chr)
      when '⁰', '¹', '²', '³', '⁴'..'⁹'
        # For whatever reason, they are out of order in Unicode...
        new(:sup_digit, chr)
      when '₊'
        new(:sub_plus, chr)
      when '₋'
        new(:sub_minus, chr)
      when '⁺'
        new(:sup_plus, chr)
      when '⁻'
        new(:sup_minus, chr)
      when '◇', '▢'
        new(:ideogram, chr)
      when '\0'
        new(:eoi, chr)
      else
        new(:other, chr)
      end
    end

    # Classifies *chr* and returns the resulting rune.
    def self.new(chr : Char) : Rune
      unless chr.ord <= 0xd7ff
        return new(:other, chr)
      end

      TABLE.unsafe_fetch(chr.ord)
    end

    # Returns the character associated with this rune.
    @[AlwaysInline]
    def chr : Char
      (@kernel >> 8).unsafe_chr
    end

    # Returns the category assigned to the character.
    @[AlwaysInline]
    def category : Category
      Category.new((@kernel & 0xff).to_u8)
    end

    # Returns the byte size of the underlying char.
    @[AlwaysInline]
    def byte_size : Int32
      chr.bytesize
    end

    # Returns `true` if this rune's `chr` is equal to *other*. Returns
    # `false` otherwise.
    def ==(other : Char) : Bool
      chr == other
    end

    # :ditto:
    def ===(other : Char) : Bool
      chr == other
    end

    # Queries this rune's `Category`.
    forward_missing_to category

    def inspect(io)
      io << "Rune(chr="
      chr.inspect(io)
      io << ", category="
      category.inspect(io)
      io << ")"
    end

    def to_s(io)
      inspect(io)
    end
  end

  struct ::Char
    # See `::Ww::ML::Rune#==`.
    def ==(other : ::Ww::ML::Rune)
      other == self
    end

    # See `::Ww::ML::Rune#===`.
    def ===(other : ::Ww::ML::Rune)
      other === self
    end
  end
end
