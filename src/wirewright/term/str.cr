module Ww
  # Represents a string.
  @[Term::Assoc(TermType::String, :unsafe_as_s)]
  class Term::Str
    include Equality
    include AutoUpcast
    include TypeConversion

    # Returns the 64-bit hash of this string.
    getter hashcode : UInt64

    def initialize(@value : String)
      # Strings use XXH3 to compute their hashcode.
      @hashcode = Term::LibXXH64.hashcode(@value.to_unsafe, @value.bytesize)
    end

    def self.new(value : Escaped)
      new(value.to_s)
    end

    def <=>(other : Str) : Int32
      @value <=> other.@value
    end

    def to_slice
      @value.to_slice
    end

    def to?(type : String.class) : String
      @value
    end

    def to?(type : StringView.class) : StringView
      @value.view
    end

    def to?(type : Path.class) : Path
      Path[@value]
    end

    # Returns the number of characters in this string.
    @[Dncast]
    def charcount : Int32
      @value.size
    end

    # Returns the character at the given *index*, or `nil` if *index* is out
    # of bounds.
    @[Dncast]
    def char_at?(index : Int) : Str?
      Term[@value[index]?]
    end

    # :nodoc:
    @[Dncast]
    def stitch(other : Str) : Str
      Str.new(@value + other.@value)
    end

    # Concatenates ("stitches") this and *other* strings.
    #
    # Reference: It turns out the name *stitching* in the context of strings
    # was borrowed by my unconscious from the depths of [Raku docs](https://docs.raku.org/language/rb-nutshell#+_String_concatenation).
    @[Dncast]
    def stitch(other) : Str
      stitch(Term.of(other).as_s)
    end

    # Returns an uppercase version of this string.
    @[Dncast]
    def upcase : Str
      Term[@value.upcase]
    end

    # Returns a lowercase version of this string.
    @[Dncast]
    def downcase : Str
      Term[@value.downcase]
    end

    struct Escaped
      def initialize(@value : String)
      end

      def inspect(io)
        io << "Escaped("
        to_s(io)
        io << ")"
      end

      # Writes an escaped representation of the string's content to *io*.
      def to_s(io)
        @value.each_char do |char|
          ML::Kit.escape(io, char)
        end
      end
    end

    # Refers to the WwML-escaped content of this string.
    #
    # See also: `ML::Kit.escape`.
    def escaped : Escaped
      Escaped.new(@value)
    end

    def inspect(io)
      ML.compact(io, self)
    end

    def to_s(io)
      inspect(io)
    end

    def_equals @value
  end
end
