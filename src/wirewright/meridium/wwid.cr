module Ww::Meridium
  # A globally unique identifier crafted specifically for Wirewright.
  #
  # - Timestamp and randomness form *conid*, used to find & contact the owner
  #   of *slot*.
  # - The 4-byte value of *slot* is globally irrelevant; it is only useful
  #   to the conid that was contacted through the first 10 bytes, to address
  #   the surface of interest. One important assumption encoded in the order
  #   of fields in the id is that the *slot* is time-sortable -- higher *slot*
  #   values are assumed to have been generated at a later point in time vs.
  #   those with a lower *slot* value.
  # - CRC16 is used as the algorithm to compute the two-byte *checksum*.
  #
  # A `0`-valued *slot* usually acts as an id "origin" for a conid, and `succ`
  # is used to obtain successive WWIDs under that conid.
  #
  # ```text
  #                     slot                  checksum
  #                 -----------                -----
  #  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  #  --------------             --------------
  #     timestamp                  randomness
  #  ms since 1 Jan 2025
  # ```
  #
  # The reasoning is as follows:
  #
  # - 5-byte timestamp with millisecond precision gives us time up to 2059, which
  #   is good enough for now.
  # - 5-byte randomness (2**40 possible values) further divide each millisecond
  #   into just over 1 million slots (before collision chance is >50%, due to
  #   the birthday paradox).
  # - 4-byte slot gives max 4 billion surfaces per connection (e.g. per client).
  # - 2-byte checksum lets us catch an occasional bit flip or two, if there's noise
  #   during reading or synthesis of WWIDs.
  struct WWID
    BYTESIZE = 16

    # Time from which we start counting the milliseconds.
    WW_EPOCH = Time.utc(year: 2025, month: 1, day: 1)

    # Raised when `from_slice_be` fails to parse the byteslice it is given
    # into a seemingly valid WWID.
    class ParseError < Exception
    end

    # :nodoc:
    def initialize(@order : UInt64, @disorder : UInt64, @slot : UInt32)
    end

    # Generates a new WWID.
    def self.new : WWID
      order = (Time.utc - WW_EPOCH).total_milliseconds.floor.to_u64
      disorder = (0u64..U40_MAX).sample(Random::Secure)

      new(order, disorder, 0u32)
    end

    # :nodoc:
    U40_MAX = 0xff_ff_ff_ff_ffu64

    # :nodoc:
    def self.encode_u40_be(int : UInt64, target : Bytes) : Nil
      if target.size < 5
        raise ArgumentError.new("target too small to write u40")
      end

      if int > U40_MAX
        raise OverflowError.new("number exceeds u40 max")
      end

      scratch = uninitialized UInt8[8] # 64 bits

      IO::ByteFormat::BigEndian.encode(int, scratch.to_slice)

      encoding = scratch.to_slice[3, 5] # 40 least significant bits
      encoding.copy_to(target)
    end

    # :nodoc:
    def self.decode_u40_be(target : Bytes) : UInt64
      if target.size < 5
        raise ArgumentError.new("target too small to contain u40")
      end

      scratch = uninitialized UInt8[8] # 64 bits

      # Initialize 3 high bytes to 0.
      scratch[0] = 0
      scratch[1] = 0
      scratch[2] = 0

      # Copy low 5 bytes.
      target[0, 5].copy_to(scratch.to_slice + 3)

      IO::ByteFormat::BigEndian.decode(UInt64, scratch.to_slice)
    end

    # Parses the bytes in *source* into a WWID.
    #
    # Raises `ParseError` if *source* is malformed.
    #
    # NOTE: *source* must be exactly `BYTESIZE` bytes long; any other bytesize is
    # considered a `ParseError`.
    def self.from_slice_be(source : Bytes) : WWID
      unless source.size == BYTESIZE
        raise ParseError.new("bytesize #{source.size} != #{BYTESIZE}")
      end

      offset = 0

      order = decode_u40_be(source + offset)
      offset += 5 # bytes

      slot = IO::ByteFormat::BigEndian.decode(UInt32, source + offset)
      offset += 4 # bytes

      disorder = decode_u40_be(source + offset)
      offset += 5 # bytes

      checksum0 = Digest::CRC16.checksum(source[0, offset])
      checksum1 = IO::ByteFormat::BigEndian.decode(UInt16, source + offset)
      offset += 2 # bytes

      unless checksum0 == checksum1
        raise ParseError.new("wrong checksum #{checksum1} (its) != #{checksum0} (my)")
      end

      instance = new(order, disorder, slot)

      now = Time.utc
      if now < instance.created_at
        raise ParseError.new("id is from the future")
      end

      instance
    end

    # Writes this WWID to a byteslice *target*, using big-endian to encode
    # the integer components of the id. Returns *target*.
    def to_slice_be(target = Bytes.new(BYTESIZE)) : Bytes
      offset = 0

      WWID.encode_u40_be(@order, target + offset)
      offset += 5 # bytes

      IO::ByteFormat::BigEndian.encode(@slot, target + offset)
      offset += 4 # bytes

      WWID.encode_u40_be(@disorder, target + offset)
      offset += 5 # bytes

      checksum = Digest::CRC16.checksum(target[0, offset])

      IO::ByteFormat::BigEndian.encode(checksum, target + offset)
      offset += 2 # bytes

      target
    end

    # Returns the time at which this WWID was created with millisecond precision.
    def created_at : Time
      WW_EPOCH + @order.milliseconds
    end

    # Returns a copy of this WWID with slot set to *slot*.
    def with_slot(slot : UInt32) : WWID
      WWID.new(@order, @disorder, slot)
    end

    # Returns the next-slot WWID.
    def succ : WWID
      WWID.new(@order, @disorder, @slot + 1)
    end

    def inspect(io)
      io << "#("
      @order.to_s(io, base: 32, precision: 9, upcase: true)
      io << "|"
      @disorder.to_s(io, base: 32, precision: 9, upcase: true)
      io << "|"
      @slot.to_s(io, base: 16, precision: 8)
      io << ")"
    end
  end
end
