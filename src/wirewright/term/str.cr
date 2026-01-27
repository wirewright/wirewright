module Ww
  # Represents a string.
  @[Term::Assoc(TermType::String, :unsafe_as_s)]
  class Term::Str
    include Equality
    include AutoUpcast
    include TypeConversion

    # FIXME: Storing a StringView here is a bad bad bad idea!!!! Views point god knows
    # where at this point -- most likely into the original source string, which could be HUGE,
    # and therefore keep it alive & waste memory. Even an empty view would keep
    # the parent string alive!!!
    def initialize(@value : StringView)
    end

    def self.new(value : Escaped)
      new(value.to_s.view)
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

    struct Escaped
      def initialize(@value : StringView)
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

require "./str/substring"
