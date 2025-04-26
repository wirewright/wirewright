alias Fingerprint = Bytes

FINGERPRINT_BYTESIZE = 32
LABEL_BYTESIZE = 16

record Label, value : UInt128 do
  include Comparable(Label)

  class_getter zero = Label.new(0u128)

  def <=>(other : Label)
    value <=> other.value
  end

  # Returns the Big Endian encoding of the raw label value.
  def to_slice_be : Bytes
    slice = Bytes.new(value.byte_size)

    append_be(slice)

    slice
  end

  def append_be(dst : Bytes) : Bytes
    IO::ByteFormat::BigEndian.encode(value, dst)

    dst + Label.bytesize
  end

  def append_be(dst : Array(UInt8)) : Nil
    buffer = uninitialized UInt8[16]

    IO::ByteFormat::BigEndian.encode(value, buffer.to_slice)

    dst.concat(buffer)
  end

  def self.bytesize
    16
  end

  def self.from_slice_be?(bytes : Bytes) : Label?
    return unless bytes.size == Label.bytesize

    new(IO::ByteFormat::BigEndian.decode(UInt128, bytes))
  end

  def self.from_slice_be(bytes : Bytes) : Label
    from_slice_be?(bytes) || raise ArgumentError.new
  end

  def complete(digit, *, base, index)
    Label.new(value &+ (digit &* base**index))
  end

  def each_prefix_with_index(*, base, max, &)
    state = 0u128

    # NOTE: since we're using UUIDs (ish) as opposed to a counter, we don't
    # have the "long zeros prefix" problem where we store lots of zeros redundantly.
    # So we don't have to think about using a variable length encoding.
    (0...max).reverse_each do |index|
      digit = (value // (base ** index)) % base
      state &+= digit &* base**index
      yield Label.new(state), index.to_u8
    end
  end

  def inspect(io)
    io << "#'"
    value.to_s(io, base: 62, precision: 22)
  end

  def to_s(io)
    inspect(io)
  end

  # Reference: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235778_109848
  # Reference: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235789_109848
  def hash(hasher)
    a = (@value >> 64).to_u64
    b = (@value << 64 >> 64).to_u64

    (a.rotate_left(1) &+ b).hash(hasher)
  end
end

alias LabelGenerator = (-> Label) | ILabelGenerator

module ILabelGenerator
  abstract def call : Label
end

# An extremely simple globally unique id source.
#
# - The first 64 bits are used for nanoseconds since the Unix epoch.
# - The second 64 bits are randomness.
#
# Time comes first (most significant) to allow for better packing, since
# prefix digits are likely to be very similar if not exactly the same.
#
# An obvious problem is maliciously rolling the time back. However, we still
# have randomness to protect us in terms of uniqueness. The time component
# is used to check if one surface (e.g. a sensor) was added before another.
# Let's say if a sensor S sees an appearance A was added after S was inserted,
# then S won't report A and instead rely on A finding S. So if the time
# component of S or A is incorrect, S will see A when it shouldn't have --
# not a *huge* problem since A is matching S anyway, and has a lot of filtering
# to go through.
#
# TODO: use something more battle-tested
struct WWID
  extend ILabelGenerator

  def self.makes_sense?(label : Label)
    t_lo = (Time.utc - 1.year).to_unix_ns.to_u128
    t_hi = (Time.utc + 1.day).to_unix_ns.to_u128

    t = label.value >> 64
    t_lo <= t <= t_hi
  end

  def self.call : Label
    order = Time.utc.to_unix_ns.to_u128
    randomness = Random::Secure.rand(UInt64)

    Label.new((order << 64) | randomness)
  end
end

alias Strand = Slice(Ubase::Any)
alias StrandList = Slice(Strand)
alias BranchList = Slice(StrandList)

# Ubases are tiny gate-keeper nodes for `Utrie`.
#
# Arbitrary M1 patterns are broken down into `BranchList` (so DNF, which has
# terrible scaling characteristics but still works!) Each branch in the branch
# list is a `StrandList` (so a conjunction of strands; we're DNF, remember?)
# A strand is a sequence of Ubases that create, in effect, a "chain of filters".
# Each Ubase, then, is such a filter. E.g. `IsNum` filters number terms; `Literal`
# filters literal matches. The `At` Ubase, on the other hand, is interesting
# because its output is different from its input.
#
# See `Utrie` to learn more.
module Ubase
  alias Any = Begin | End | At | IsSym | IsStr | IsNum | IsBool | IsDict | Literal

  # Passes a dictionary term's value for *key* forward.
  record At, key : Term do
    # FIXME: rename key to term
    def term : Term
      key
    end
  end

  # Anchor put at the beginning of all strands.
  record Begin

  # Indicates an abrupt (non-literal) stop. This base is not emitted if the strand
  # ends with `Literal`.
  record End

  # Passes only symbol terms forward.
  record IsSym do
    def self.code
      Bytes[0]
    end
  end

  # Passes only string terms forward.
  record IsStr do
    def self.code
      Bytes[1]
    end
  end

  # Passes only number terms forward.
  record IsNum do
    def self.code
      Bytes[2]
    end
  end

  # Passes only boolean terms forward.
  record IsBool do
    def self.code
      Bytes[3]
    end
  end

  # Passes only dictionary terms forward.
  record IsDict do
    def self.code
      Bytes[4]
    end
  end

  # Passes foward only terms that match *value* exactly.
  record Literal, value : Term do
    # FIXME: rename value to term
    def term : Term
      value
    end
  end

  # Returns the is-type `Ubase` (e.g. `IsNum`) that corresponds to the given
  # `TermType` *type*.
  #
  # Raises `ArgumentError` if *type* is `TermType::Any`.
  def self.from(type : TermType) : Ubase::Any
    case type
    in .any?     then raise ArgumentError.new
    in .boolean? then IsBool.new
    in .number?  then IsNum.new
    in .string?  then IsStr.new
    in .symbol?  then IsSym.new
    in .dict?    then IsDict.new
    end
  end

  def self.update(digest, base : End)
    digest.update(Bytes[0])
  end


  def self.update(digest, base : Begin)
    digest.update(Bytes[1])
  end

  def self.update(digest, base : IsSym)
    digest.update(Bytes[2])
  end

  def self.update(digest, base : IsStr)
    digest.update(Bytes[3])
  end

  def self.update(digest, base : IsNum)
    digest.update(Bytes[4])
  end

  def self.update(digest, base : IsBool)
    digest.update(Bytes[5])
  end

  def self.update(digest, base : IsDict)
    digest.update(Bytes[6])
  end

  # TODO: optimize
  def self.update(digest, base : Literal)
    digest.update(Bytes[7])
    digest.update(ML.compact(base.value))
  end

  # TODO: optimize
  def self.update(digest, base : At)
    digest.update(Bytes[8])
    digest.update(ML.compact(base.key))
  end
end

