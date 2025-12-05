module Ww
  # Represents a string.
  @[Term::Assoc(TermType::String, :unsafe_as_s)]
  class Term::Str
    include Equality
    include AutoUpcast
    include TypeConversion

    def initialize(@value : StringView)
    end

    def <=>(other : Str) : Int32
      @value <=> other.@value
    end

    @[Dncast]
    def after_end : Str
      Str.new(@value.after_end)
    end

    @[Dncast]
    def unsafe_byte : UInt8
      @value.to_unsafe[0]
    end

    @[Dncast]
    def each_byte(& : UInt8 ->) : Nil
      @value.each_byte { |byte| yield byte }
    end

    def to?(type : String.class) : String
      @value.to_s
    end

    def to?(type : StringView.class) : StringView
      @value
    end

    def to?(type : Path.class) : Path
      Path[@value.to_s]
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
    @[Dncast]
    def stitch(other) : Str
      stitch(Term.of(other).as_s)
    end

    # Returns an uppercase version of this string.
    @[Dncast]
    def upcase : Str
      Term[@value.to_s.upcase]
    end

    # Returns a lowercase version of this string.
    @[Dncast]
    def downcase : Str
      Term[@value.to_s.downcase]
    end

    @[Dncast]
    def first : Str
      Term[@value.first_or_empty]
    end

    @[Dncast]
    def rest : Str
      Term[@value.rest_or_empty]
    end

    @[Dncast]
    def prior : Str
      Term[@value.prior_or_empty]
    end

    @[Dncast]
    def last : Str
      Term[@value.last_or_empty]
    end

    # TODO: Move to `ML.compact`
    def inspect(io)
      io << '"'
      @value.each_char do |char|
        ML::Kit.escape(io, char)
      end
      io << '"'
    end

    def to_s(io)
      inspect(io)
    end

    def_equals @value
  end
end

require "./str/substring"
