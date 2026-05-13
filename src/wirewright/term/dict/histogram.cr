class Ww::Term::Dict
  # The histogram of a dictionary is a measure of how many terms of a given
  # type the dictionary itself and its dictionary values contain (recursively).
  #
  # Note that the histogram has limited capacity for each type. When reading
  # counts using methods such as `numbers` (which returns the amount of numbers
  # in the dictionary the histogram describes, as a `Magnitude`), make sure to
  # account for `Magnitude::INFINITY`, which is used by `Histogram` to say,
  # "I don't know", in the sense of, "too many to keep count of".
  #
  # If the count is *not* `Magnitude::INFINITY`, then the count is *precise*,
  # and can be used for both lower- and higher-bound rejection.
  struct Histogram
    # :nodoc
    #
    #    MSB ...             LSB
    # 0x 00 00 00 00 00 00 00 00
    #           n  q  s  t  f  b
    #
    # n - number
    # q - string
    # s - symbol
    # t - true
    # f - false
    # b - blob
    getter bits : UInt64

    # :nodoc:
    def initialize(@bits : UInt64)
    end

    # :nodoc:
    ONE_NUMBER = new(0x01_00_00_00_00_00u64)
    # :nodoc:
    ONE_STRING = new(0x00_01_00_00_00_00u64)
    # :nodoc:
    ONE_SYMBOL = new(0x00_00_01_00_00_00u64)
    # :nodoc:
    ONE_TRUE = new(0x00_00_00_01_00_00u64)
    # :nodoc:
    ONE_FALSE = new(0x00_00_00_00_01_00u64)
    # :nodoc:
    ONE_BLOB = new(0x00_00_00_00_00_01u64)

    @[AlwaysInline]
    def self.zero : Histogram
      new(0u64)
    end

    @[AlwaysInline]
    def self.of(term : Term) : Histogram
      case term.type
      in .any?
        raise ArgumentError.new
      in .dict?
        term.unsafe_as_d.summary.histogram
      in .number?
        ONE_NUMBER
      in .string?
        ONE_STRING
      in .symbol?
        ONE_SYMBOL
      in .boolean?
        term.true? ? ONE_TRUE : ONE_FALSE
      in .blob?
        ONE_BLOB
      end
    end

    @[AlwaysInline]
    def self.union(a : Histogram, b : Histogram) : Histogram
      Histogram.new(addsb(a.bits, b.bits))
    end

    # Saturating byte-wise addition of two u64s. Unfortunately I wasn't (yet?) able
    # to get LLVM to codegen this for me even with very straightforward code. So I've
    # hard-coded it in asm. Otherwise we'd be having to do some SWAR trickery and/or bit
    # repacking which is absolutely pointless given the fact there is an instruction
    # *specifically* for what we want here, and it's *ancient*. Note also that this
    # function is blazing hot; moving from straightforward if-checks and a struct with
    # separate fields to this... oddity shaved about 10ms off of a benchmark in debug
    # build (think 28ms -> 19ms) and about 100µs in release build. These numbers mean
    # nothing, of course, especially in the long run as I'm optimizing things; but just
    # to understand the impact of this function, they're semi-useful. Interestingly,
    # doing something similarly smart in Summary.of produces very bad results. Thus,
    # we can only afford to be smart *here*. *There*, there's more overhead for being
    # smart than not.
    @[AlwaysInline]
    private def self.addsb(a : UInt64, b : UInt64) : UInt64
      sum = uninitialized UInt64

      asm(
        "movq xmm0, $1
         movq xmm1, $2
         paddusb xmm1, xmm0
         movq  $0, xmm1" :

        "=r"(sum) :
        "r"(a), "r"(b) :
        "xmm0", "xmm1" :
        "intel"
      )

      sum
    end

    # Saturating UInt8 to Magnitude.
    private def sat_byte_to_magn(byte : UInt64) : Magnitude
      byte == 0xffu64 ? Magnitude::INFINITY : Magnitude.new(byte)
    end

    # Returns the amount of numbers in the dict this histogram describes.
    def numbers : Magnitude
      sat_byte_to_magn((@bits & 0xff_00_00_00_00_00u64) >> 5*8)
    end

    # Returns the amount of strings in the dict this histogram describes.
    def strings : Magnitude
      sat_byte_to_magn((@bits & 0x00_ff_00_00_00_00u64) >> 4*8)
    end

    # Returns the amount of symbols in the dict this histogram describes.
    def symbols : Magnitude
      sat_byte_to_magn((@bits & 0x00_00_ff_00_00_00u64) >> 3*8)
    end

    # Returns the amount of `true` booleans in the dict this histogram describes.
    def trues : Magnitude
      sat_byte_to_magn((@bits & 0x00_00_00_ff_00_00u64) >> 2*8)
    end

    # Returns the amount of `false` booleans in the dict this histogram describes.
    def falses : Magnitude
      sat_byte_to_magn((@bits & 0x00_00_00_00_ff_00u64) >> 1*8)
    end

    # Returns the amount of blobs in the dict this histogram describes.
    def blobs : Magnitude
      sat_byte_to_magn((@bits & 0x00_00_00_00_00_ffu64) >> 0*8)
    end

    # Returns the sum total of accounted terms. This method may return
    # `Magnitude::INFINITY` (aka unknown, aka too many to keep track of)
    # if one of the counts is infinite.
    def total : Magnitude
      numbers + strings + symbols + trues + falses + blobs
    end

    def subset_of?(other : Histogram) : Bool
      return false unless numbers <= other.numbers
      return false unless strings <= other.strings
      return false unless symbols <= other.symbols
      return false unless trues <= other.trues
      return false unless falses <= other.falses
      return false unless blobs <= other.blobs

      true
    end

    def inspect(io)
      io << "Histogram("
      io << "numbers=" << numbers << ", "
      io << "strings=" << strings << ", "
      io << "symbols=" << symbols << ", "
      io << "trues=" << trues << ", "
      io << "falses=" << falses << ", "
      io << "blobs=" << blobs
      io << ")"
    end
  end
end
