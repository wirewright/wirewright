module Ww
  # Represents a string.
  struct Term::Str
    include ITerm

    def initialize(@value : String)
    end

    # :nodoc:
    delegate :inspect, to: @value

    def to?(type : String.class) : String
      @value
    end

    # Returns the number of characters in this string.
    def charcount : Int32
      @value.size
    end

    # Returns the character at the given *index*, or `nil` if *index* is out
    # of bounds.
    def char_at?(index : Int) : Str?
      return unless chr = @value[index]?

      Str.new(chr.to_s)
    end

    # :nodoc:
    def stitch(other : Str) : Str
      Str.new(@value + other.@value)
    end

    # Concatenates ("stitches") this and *other* strings.
    def stitch(other : Term) : Str
      stitch(other.as_s)
    end

    # Returns an uppercase version of this string.
    def upcase : Str
      Term[@value.upcase]
    end

    # Returns a lowercase version of this string.
    def downcase : Str
      Term[@value.downcase]
    end

    # :inherit:
    def to_ir : IR
      IR.new(IR::Type::String, @value, children: nil)
    end

    def_equals_and_hash @value
  end
end
