module Ww
  # Represents a string.
  struct Term::Str
    include ITerm

    def initialize(@value : String)
    end

    # :nodoc:
    delegate :inspect, to: @value

    def single_byte? : Bool
      @value.bytesize == 1
    end

    def unsafe_byte : UInt8
      @value.to_unsafe[0]
    end

    def each_byte(& : UInt8 ->) : Nil
      @value.each_byte { |byte| yield byte }
    end

    def to?(type : String.class) : String
      @value
    end

    def to?(type : StringView.class) : StringView
      @value.view
    end

    # Returns the number of characters in this string.
    def charcount : Int32
      @value.size
    end

    # Returns the character at the given *index*, or `nil` if *index* is out
    # of bounds.
    def char_at?(index : Int) : Str?
      Term[@value[index]?]
    end

    # :nodoc:
    def stitch(other : Str) : Str
      Str.new(@value + other.@value)
    end

    # Concatenates ("stitches") this and *other* strings.
    def stitch(other) : Str
      stitch(Term.of(other).as_s)
    end

    # Returns an uppercase version of this string.
    def upcase : Str
      Term[@value.upcase]
    end

    # Returns a lowercase version of this string.
    def downcase : Str
      Term[@value.downcase]
    end

    def first : Str
      Term[@value[0]? || ""]
    end

    def rest : Str
      Term[@value.lchop]
    end

    def prior : Str
      Term[@value.rchop]
    end

    def last : Str
      Term[@value[-1]? || ""]
    end

    def_equals @value
  end
end

require "./str/substring"
