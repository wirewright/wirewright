class Ww::Term::Dict
  # The histogram of a dictionary is a measure of how many terms of a given
  # type the dictionary itself and its dictionary values contain (recursively).
  #
  # Note that the histogram has limited capacity for each type. When reading
  # counts using methods such as `numbers` (which returns the amount of numbers
  # in the dictionary the histogram is for, as a `Magnitude`), make sure to
  # account for `Magnitude::INFINITY`, which is used by `Histogram` to say,
  # "I don't know", in the sense of, "too many to keep count of".
  #
  # If the count is *not* `Magnitude::INFINITY`, then the count is *precise*,
  # and can be used for both lower- and higher-bound rejection.
  struct Histogram
    # :nodoc:
    INFINITY = UInt8::MAX

    # :nodoc:
    def initialize(
      @numbers : UInt8,
      @strings : UInt8,
      @symbols : UInt8,
      @trues : UInt8,
      @falses : UInt8,
    )
    end

    # :nodoc:
    def_change

    {% for field in %w[numbers strings symbols trues falses] %}
      # Returns the amount of {{field.id}} in the dict this histogram is for.
      def {{field.id}} : Magnitude
        @{{field.id}} == INFINITY ? Magnitude::INFINITY : Magnitude.new(@{{field.id}})
      end
    {% end %}

    # Returns the "zero" or empty histogram, often used as an initial histogram.
    def self.zero : Histogram
      new(numbers: 0u8, strings: 0u8, symbols: 0u8, trues: 0u8, falses: 0u8)
    end

    # Returns the histogram of one *term*.
    def self.of(term : Term) : Histogram
      case term.type
      in .any?
        raise ArgumentError.new
      in .dict?
        term.unsafe_as_d.histogram
      in .number?
        zero.change(numbers: 1u8)
      in .string?
        zero.change(strings: 1u8)
      in .symbol?
        zero.change(symbols: 1u8)
      in .boolean?
        term.true? ? zero.change(trues: 1u8) : zero.change(falses: 1u8)
      end
    end

    # Returns the union of two histograms.
    def self.union(a : Histogram, b : Histogram) : Histogram
      Histogram.new(
        numbers: add(a.@numbers, b.@numbers),
        strings: add(a.@strings, b.@strings),
        symbols: add(a.@symbols, b.@symbols),
        trues: add(a.@trues, b.@trues),
        falses: add(a.@falses, b.@falses),
      )
    end

    private def self.add(a : UInt8, b : UInt8)
      if b > UInt8::MAX - a # add will overflow
        return INFINITY
      end

      a &+ b
    end

    # Returns the sum total of accounted terms. This method may return
    # `Magnitude::INFINITY` (aka unknown, aka too many to keep track of)
    # if one of the counts is infinite.
    def total : Magnitude
      numbers + strings + symbols + trues + falses
    end
  end
end
