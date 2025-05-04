module Ww::Meridium
  # Slot of `WWID`. Extracted for brevity.
  alias Slot = UInt32

  # Time from which we start counting most timestamps.
  WW_EPOCH = Time.utc(year: 2025, month: 1, day: 1)

  # A globally unique identifier crafted specifically for Wirewright.
  #
  # - Timestamp and randomness form *conid*, used to find & contact the owner
  #   of *slot*.
  # - The 4-byte value of *slot* is globally irrelevant; it is only useful
  #   to the conid that was contacted through the first 10 bytes, to address
  #   the surface of interest.
  # - CRC16 is used as the algorithm to compute the two-byte *checksum*.
  #
  # In other words, you can think of the *timestamp*-*randomness* combo as a
  # "host" of sorts; whereas the slot is the "port".
  #
  # A `0`-valued *slot* usually acts as an id "origin" for a conid, and `succ`
  # is used to obtain successive WWIDs under that conid.
  #
  # ```text
  #                   randomness              checksum
  #                 --------------             -----
  #  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  #  --------------                ------------
  #     timestamp                      slot
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
    include Comparable(WWID)

    {% begin %}
      BYTESIZE = {{5 + sizeof(Slot) + 5 + 2}}
    {% end %}

    # Raised when `from_slice_be` fails to parse the byteslice it is given
    # into a seemingly valid WWID.
    class ParseError < Exception
    end

    # Returns the slot of this WWID.
    getter slot : Slot

    # :nodoc:
    def initialize(@order : UInt64, @disorder : UInt64, @slot : Slot)
    end

    # Generates a new WWID.
    def self.new : WWID
      order = (Time.utc - WW_EPOCH).total_milliseconds.floor.to_u64
      disorder = (0u64..U40_MAX).sample(Random::Secure)

      new(order, disorder, slot: Slot.new(0))
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

      disorder = decode_u40_be(source + offset)
      offset += 5 # bytes

      slot = IO::ByteFormat::BigEndian.decode(Slot, source + offset)
      offset += sizeof(Slot) # bytes

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
    def to_slice_be(&)
      scratch = uninitialized UInt8[BYTESIZE]

      offset = 0

      WWID.encode_u40_be(@order, scratch.to_slice + offset)
      offset += 5 # bytes

      WWID.encode_u40_be(@disorder, scratch.to_slice + offset)
      offset += 5 # bytes

      IO::ByteFormat::BigEndian.encode(@slot, scratch.to_slice + offset)
      offset += sizeof(Slot) # bytes

      checksum = Digest::CRC16.checksum(scratch.to_slice[0, offset])

      IO::ByteFormat::BigEndian.encode(checksum, scratch.to_slice + offset)
      offset += 2 # bytes

      yield scratch.to_slice
    end

    def to_slice_be(target = Bytes.new(BYTESIZE)) : Bytes
      to_slice_be do |slice|
        slice.copy_to(target)
        slice
      end
    end

    # Shorthand for `with_slot(0)`.
    def conid : WWID
      with_slot(0)
    end

    # Two WWIDs are compared by their order component (timestamp).
    def <=>(other : WWID)
      @order <=> other.@order
    end

    # Returns the time at which this WWID was created with millisecond precision.
    def created_at : Time
      WW_EPOCH + @order.milliseconds
    end

    # Returns a copy of this WWID with slot set to *slot*.
    def with_slot(ord : Slot) : WWID
      WWID.new(@order, @disorder, ord)
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

    def_equals_and_hash @order, @disorder, @slot
  end

  # A nanosecond-precision timestamp identifying the moment at which something
  # was created. Normally that "something" is a `Surface`; `Conn` and `Node` are
  # the main users & sources of `IWWID`s, and thus of `Instant`s too. They use
  # it to make sure some surface info is newer than the one they have already.
  record Instant, timestamp : UInt64 do
    include Comparable(Instant)

    BYTESIZE = 8

    def self.new : Instant
      dt = Time.utc - WW_EPOCH

      new(timestamp: dt.total_nanoseconds.floor.to_u64)
    end

    def self.from_slice_be(slice : Bytes) : Instant
      timestamp = IO::ByteFormat::BigEndian.decode(UInt64, slice)

      new(timestamp)
    end

    def <=>(other : Instant)
      timestamp <=> other.timestamp
    end

    def to_slice_be(&)
      scratch = uninitialized UInt8[sizeof(Instant)]

      IO::ByteFormat::BigEndian.encode(timestamp, scratch.to_slice)

      yield scratch.to_slice
    end
  end

  # An IWWID is a `WWID` equipped with an `Instant` at which it was created.
  record IWWID, wwid : WWID, instant : Instant do
    {% begin %}
      BYTESIZE = {{WWID::BYTESIZE + Instant::BYTESIZE}}
    {% end %}

    delegate :conid, :slot, to: @wwid

    def self.new : IWWID
      new(WWID.new, Instant.new)
    end

    def self.from_slice_be(slice : Bytes) : IWWID
      unless slice.size == BYTESIZE
        raise ArgumentError.new("invalid slice size")
      end

      new(
        wwid: WWID.from_slice_be(slice[0, WWID::BYTESIZE]),
        instant: Instant.from_slice_be(slice[WWID::BYTESIZE, Instant::BYTESIZE]),
      )
    end

    def to_slice_be(&)
      wwid.to_slice_be { |slice| yield slice }
      instant.to_slice_be { |slice| yield slice }
    end
  end
end
