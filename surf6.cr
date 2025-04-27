require "./src/wirewright"
require "log"
require "./blake3"
require "digest"
require "wait_group"

alias BitList = DynamicBitArray

# + Utrie    ;; maps bases to endpoints
# + Xgraph   ;; maps endpoint conjunctions to conjunction apex through binary conjunctions
# + BytesMultimap
#   + SensorRegistry     ;; maps conjunction apex to sensor ids
#   + AppearanceRegistry ;; maps term strands to appearance ids
#
# LATER: protect BytesMultimap, and Xgraph from lying sets by emitting two
# types of sanity check queries:
# - Since we have Entity already we can introduce NegXgraph NegSensorRegistry etc.,
#   elements of which are always absent (else the set is insane.) This tests how sanely
#   the set generates negative responses.
# - We can also do randomized lookback -- i.e. remember a random `true` query from the past
#   and emit it sometime in the future. This tests how sanely the set generates positive
#   reponses -- if it says `false` for something that was `true` before and we're working
#   on the consequences of that right now, we halt.

enum Entity : UInt8
  Utrie
  Xgraph
  SensorRegistry
  AppearanceRegistry
end

# Atom is currently a 256-bit hash split into 4 64-bit blocks. For some reason
# this appears to be faster than passing the bytes as-is (e.g. as `u8[16]`).
struct Atom
  include Comparable(Atom)

  BYTESIZE = 32

  def initialize(@blk0 : UInt64, @blk1 : UInt64, @blk2 : UInt64, @blk3 : UInt64)
  end

  def self.of(digest : Bytes)
    unless digest.size == 32
      raise ArgumentError.new("expected digest to be 32 bytes")
    end

    blk0, blk1, blk2, blk3 = digest.unsafe_slice_of(UInt64)

    new(blk0, blk1, blk2, blk3)
  end

  def self.of(string : String)
    of(Blake3.final(string))
  end

  def <=>(other : Atom)
    {@blk0, @blk1, @blk2, @blk3} <=> {other.@blk0, other.@blk1, other.@blk2, other.@blk3}
  end

  def copy_hash_to(target : Bytes) : Nil
    blks = target.unsafe_slice_of(UInt64)
    blks[0] = @blk0
    blks[1] = @blk1
    blks[2] = @blk2
    blks[3] = @blk3
  end

  def hash(hasher)
    @blk0.hash(hasher)
  end

  def inspect(io)
    io << "Atom["
    @blk0.to_s(io, base: 32, precision: 13, upcase: true)
    io << "-"
    @blk1.to_s(io, base: 32, precision: 13, upcase: true)
    io << "-"
    @blk2.to_s(io, base: 32, precision: 13, upcase: true)
    io << "-"
    @blk3.to_s(io, base: 32, precision: 13, upcase: true)
    io << "]"
  end

  def_equals @blk0, @blk1, @blk2, @blk3
end

# The first 12 bytes are always conid, used to find & contact the owner of
# *slot*. The 4-byte value of slot is globally irrelevant; it is only useful
# to the conid that was contacted through the first 12 bytes, to resolve the
# surface of interest. A 0-slot conid usually acts as an id "origin" and `succ`
# is used to obtain successive WWIDs under that conid.
#
# ```text
#                   randomness              checksum
#                 ---------------            -----
#  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
#  --------------                -----------
#  ms since 1 Jan 2025        slot (client-defined)
# ```
struct WWID
  WW_EPOCH = Time.utc(year: 2025, month: 1, day: 1)
  BYTESIZE = 16

  class ParseError < Exception
  end

  def initialize(@order : UInt64, @disorder : UInt64, @slot : UInt32)
  end

  # Generates a new WWID.
  def self.next : WWID
    order = (Time.utc - WW_EPOCH).total_milliseconds.floor.to_u64
    disorder = (0u64..U40_MAX).sample(Random::Secure)

    new(order, disorder, 0u32)
  end

  U40_MAX = 0xff_ff_ff_ff_ffu64

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

  def self.from_slice_be(source : Bytes) : WWID?
    unless source.size == BYTESIZE
      raise ParseError.new("bytesize #{source.size} != #{BYTESIZE}")
    end

    offset = 0

    order = decode_u40_be(source + offset)
    offset += 5 # bytes

    slot = IO::ByteFormat::BigEndian.decode(UInt32, source + offset)
    offset += sizeof(UInt32)

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

  def to_slice_be(target = Bytes.new(BYTESIZE)) : Bytes
    offset = 0

    # The order is weird to allow the id to be sortable by raw bytes. We use
    # a different order on the struct to have it properly aligned.

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

  def created_at : Time
    WW_EPOCH + @order.milliseconds
  end

  def with_slot(slot : UInt32)
    WWID.new(@order, @disorder, slot)
  end

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

def h0 : Atom
  hasher = Blake3.new
  scratch = uninitialized UInt8[Atom::BYTESIZE]
  hasher.final(scratch.to_slice)
  Atom.of(scratch.to_slice)
end

NULL_HASH_ATOM = h0

def h(hasherptr) : Atom
  NULL_HASH_ATOM
end

def h(hasherptr, a : Nil) : Atom
  h(hasherptr)
end

def h(hasherptr, a : Entity) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  hasherptr.value.reset
  hasherptr.value.update(a.value)
  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def h(hasherptr, a : Term) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  hasherptr.value.reset

  io = IO::ByteStream.new { |slice| hasherptr.value.update(slice) }
  ML.compact(io, a)

  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def h(hasherptr, a : Atom, b : Bytes) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  a.copy_hash_to(scratch.to_slice)

  hasherptr.value.reset
  hasherptr.value.update(scratch.to_slice)
  hasherptr.value.update(b)
  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def h(hasherptr, a : Atom, b : Atom) : Atom
  {% begin %}
    scratch = uninitialized UInt8[{{Atom::BYTESIZE * 2}}]

    a.copy_hash_to(scratch.to_slice[0, Atom::BYTESIZE])
    b.copy_hash_to(scratch.to_slice[Atom::BYTESIZE, Atom::BYTESIZE])

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)
    hasherptr.value.final(scratch.to_slice)

    Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
  {% end %}
end

def h(hasherptr, a : Atom, b : UInt8) : Atom
  {% begin %}
    scratch = uninitialized UInt8[{{Atom::BYTESIZE + 1}}]

    a.copy_hash_to(scratch.to_slice[0, Atom::BYTESIZE])
    scratch[-1] = b

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)
    hasherptr.value.final(scratch.to_slice[0, Atom::BYTESIZE])

    Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
  {% end %}
end

def h(hasherptr, a : UInt8, b : Atom) : Atom
  {% begin %}
    scratch = uninitialized UInt8[{{Atom::BYTESIZE + 1}}]

    scratch[0] = a
    b.copy_hash_to(scratch.to_slice[1, Atom::BYTESIZE])

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)
    hasherptr.value.final(scratch.to_slice[0, Atom::BYTESIZE])

    Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
  {% end %}
end

def h(hasherptr, a : Atom, b : Ubase::Any) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  a.copy_hash_to(scratch.to_slice)

  hasherptr.value.reset
  hasherptr.value.update(scratch.to_slice)

  hupdate(hasherptr, b)

  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def hupdate(hasherptr, ubase : Ubase::Begin) : Nil
  hasherptr.value.update(Uopcode::Begin.value)
end

def hupdate(hasherptr, ubase : Ubase::End) : Nil
  hasherptr.value.update(Uopcode::End.value)
end

{% for base in %w[IsSym IsStr IsNum IsBool IsDict] %}
  def hupdate(hasherptr, ubase : Ubase::{{base.id}}) : Nil
    hasherptr.value.update(Uopcode::{{base.id}}.value)
  end
{% end %}

{% for base in %w[At Literal] %}
  def hupdate(hasherptr, ubase : Ubase::{{base.id}}) : Nil
    hasherptr.value.update(Uopcode::Hashed{{base.id}}.value)

    digestion = IO::ByteStream.new { |slice| hasherptr.value.update(slice) }

    ML.compact(digestion, ubase.term)
  end
{% end %}

def h(hasherptr, a : Atom, b : Nil) : Atom
  h(hasherptr, a, h(hasherptr))
end

def h(hasherptr, entity : Entity, a, b) : Atom
  h(hasherptr, h(hasherptr, entity.value, a), b)
end

def secret_to_bytes(secret : Term) : Bytes
  io = IO::Memory.new
  io.write_byte(1)
  ML.compact(io, secret)
  io.to_slice
end

def secret_to_bytes(secret : Nil) : Bytes
  Bytes[0]
end

# :nodoc:
ST = Fiber::ExecutionContext::SingleThreaded.new("Meridium single-threaded")

# :nodoc:
MT = Fiber::ExecutionContext::MultiThreaded.new("Meridium multi-threaded", System.cpu_count.to_i)

class Completion
  OFFSET_BEGIN = 62u8
  OFFSET_END   = 0u8

  def initialize(@blocks = Slice(UInt64).empty, @block = 0u64, @offset = OFFSET_BEGIN)
  end

  def append(digit : UInt8) : Completion
    offset = @offset

    @block |= digit.to_u64 << offset

    if offset == OFFSET_END
      @blocks = @blocks.append(@block)
      @block = 0u64
      @offset = OFFSET_BEGIN
    else
      @offset -= 2 # one base-4 digit
    end

    self
  end

  def bytesize(key : Bytes, prefix : Bytes) : Int32
    ntailbytes, ntailbits = (OFFSET_BEGIN - @offset).divmod(8)

    key.size + prefix.size + @blocks.size*8 + ntailbytes + (ntailbits.zero? ? 0 : 1)
  end

  def final_to(target : Bytes, key : Bytes, prefix : Bytes) : Bytes
    ntailbytes, ntailbits = (OFFSET_BEGIN - @offset).divmod(8)

    cursor = target

    cursor.copy_from(key)
    cursor += key.size

    cursor.copy_from(prefix)
    cursor += prefix.size

    @blocks.each do |block|
      IO::ByteFormat::BigEndian.encode(block, cursor)
      cursor += 8 # bytes
    end

    if ntailbytes + ntailbits > 0
      scratch = uninitialized UInt8[8]

      IO::ByteFormat::BigEndian.encode(@block, scratch.to_slice)

      cursor.copy_from(scratch.to_slice[0, ntailbytes])
      cursor += ntailbytes

      if ntailbits > 0
        cursor[0] = scratch[ntailbytes]
        cursor += 1 # byte
      end
    end

    target
  end

  def final(key : Bytes, prefix : Bytes) : Bytes
    final_to(Bytes.new(bytesize(key, prefix)), key, prefix)
  end

  def_equals_and_hash @blocks, @block, @offset
end

# Implements an emergent *bytes multimap*: effectively a general-purpose digit
# trie that can store any number of arbitrary byte sequences into an `Atom` set;
# and can then complete any prefix up to all of the stored sequences with
# that prefix.
#
# Base 4 was chosen as the digit base; so much so that it is hard-coded into
# the implementation. This is because it appears to be a suitable compromise
# between very little guessing (as with base 2, you only have two choices ->
# two questions to ask at each level) vs. atom count (with base 2 you get
# breadth=2, and thus very deep tries that require a lot of "road signs" -- atoms).
#
# This implementation is *extremely* experimental, and rather slow even after
# my efforts to optimize it. Thankfully, it is at least *somewhat* parallelizable
# (how much depends on the underlying set). The majority of time we spend in Blake3,
# hashing recursively or appending digits; thus multi-threading helps and is able
# to (in some cases?) get us the <no. of cores>x speedup expected of "perfectly
# parallelizable" problems such as this one.
module BytesMultimap
  extend self

  private def each_b4_digit(data : Bytes, & : UInt8 ->)
    reader = BitReader.new(data)

    loop do
      # Consume one base-4 digit.
      bit0 = reader.consume? || break
      bit1 = reader.consume? || 0u8
      digit = (bit0 << 1) | bit1
      yield digit
    end
  end

  # Adds *data* to the multimap under *key* (which can be empty). We hash *key*
  # in but do not include hints for its generation on query, so *key* can be used
  # as a kind of "password" for *data*.
  #
  # *entity* is the "entity" to which the *key*-*data* pair belongs. It acts as
  # a "scope" for *key*-*data*.
  #
  # NOTE: You can execute multiple calls to `add` in parallel for better performance;
  # since the majority of the time is spent hashing *data*, even a crude lock-protected
  # *atoms* set would do.
  def add(atoms, entity : Entity, key : Bytes, data : Bytes) : Atom
    hasher = Blake3.new

    h0 = h(pointerof(hasher))
    h0 = h(pointerof(hasher), h0, entity.value)
    h0 = h(pointerof(hasher), h0, key)

    add(atoms, h0, data)
  end

  def add(atoms, h0, data) : Atom
    hasher = Blake3.new

    each_b4_digit(data) do |digit|
      h0 = h(pointerof(hasher), h0, digit)
      atoms << h0
    end

    h0
  end

  def append(h0, data) : Atom
    hasher = Blake3.new

    each_b4_digit(data) do |digit|
      h0 = h(pointerof(hasher), h0, digit)
    end

    h0
  end

  # Each exploration fiber is running this method.
  private def explore(wg, atoms, completion, h0 : Atom, fn : Completion, Atom ->) : Nil
    hasher = Blake3.new

    loop do
      candidates = {0u8, 1u8, 2u8, 3u8}.map do |digit|
        h(pointerof(hasher), h0, digit.to_u8)
      end

      first = nil

      answer = atoms.present?(candidates, &.itself)
      answer.each_with_index do |exists, digit|
        next unless exists

        digit = digit.to_u8
        candidate = candidates[digit]

        unless first
          first = {candidate, digit}
          next
        end

        completion1 = completion.dup.append(digit)

        wg.add

        # Spawn further fibers to explore the other branches if their
        # atoms were found to exist.
        spawn explore(wg, atoms, completion1, candidate, fn)
      end

      unless first
        fn.call(completion, h0)
        wg.done
        return
      end

      # Reuse the same fiber for exploring one of the branches. This was supposed
      # to be a tail call, but since Crystal doesn't guarantee those would be
      # optimized, we write it as a loop explicitly.
      h0, digit = first
      completion = completion.append(digit)
    end
  rescue e : Exception
    # Crash gracefully (humph?!)
    wg.done
    raise e
  end

  # Calls *fn* with each possible completion for *key*-*prefix* under the given
  # *entity*. See also: `bind`.
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  #
  # WARNING: *fn* will be called from another fiber, perhaps running on another
  # thread if *mt* is `true` (it is by default). Thus make sure to either have
  # fully compartmentalized *fn*, or *fn* that talks to the outside world in a
  # thread-safe manner.
  #
  # The performance of this method in multi-threaded mode should *ideally*
  # be <no. of cpu cores>x vs. single-threaded; but this depends heavily on
  # how well `Fiber::ExecutionContext` schedules things as well as on the implementation
  # of the underlying *atoms* set. In my case, I'm getting ~3.5x speedup over
  # single-threaded mode with a very simple bucketized set (1024 buckets, each
  # with a lock). There's a lot of variables involved though, and we cannot
  # guarantee a lot (other than the underlying implementation is friendly toward
  # parallelization in general). As a general observation, it appears that the less
  # values there are in the multimap, the more comparable *mt* becomes to *st*; which
  # is actually expected, since the underlying algorithm adapts to the contents of
  # the set.
  def complete(atoms, entity : Entity, key : Bytes, prefix : Bytes, *, mt : Bool = true, &fn : Completion, Atom ->) : Nil
    hasher = Blake3.new

    h0 = h(pointerof(hasher))
    h0 = h(pointerof(hasher), h0, entity.value)
    h0 = h(pointerof(hasher), h0, key)

    each_b4_digit(prefix) do |digit|
      h0 = h(pointerof(hasher), h0, digit)
    end

    completion0 = Completion.new

    wg = WaitGroup.new
    wg.add

    ctx = mt ? MT : ST
    ctx.spawn { explore(wg, atoms, completion0, h0, fn) }

    wg.wait
  end

  defcase Quad, a : Atom, b : Atom, c : Atom, d : Atom do
    include Enumerable(Atom)

    def each(& : Atom ->)
      {a, b, c, d}.each { |atom| yield atom }
    end
  end

  record QuadMask, bits : UInt8 do
    def self.new(a : Bool, b : Bool, c : Bool, d : Bool) : QuadMask
      bits = 0u8
      bits |= 0b1 if a
      bits |= 0b10 if b
      bits |= 0b100 if c
      bits |= 0b1000 if d
      new(bits)
    end

    def none? : Bool
      bits.zero?
    end

    def select_with_digit(quad : Quad, & : Atom, UInt8 ->) : Nil
      yield quad.a, 0u8 if bits.bit_set?(0)
      yield quad.b, 1u8 if bits.bit_set?(1)
      yield quad.c, 2u8 if bits.bit_set?(2)
      yield quad.d, 3u8 if bits.bit_set?(3)
    end

    def &(other : QuadMask)
      QuadMask.new(bits & other.bits)
    end
  end

  alias Arm = {Completion, Atom}

  private def quad(hasherptr, atom : Atom)
    Quad.new(
      h(hasherptr, atom, 0u8),
      h(hasherptr, atom, 1u8),
      h(hasherptr, atom, 2u8),
      h(hasherptr, atom, 3u8),
    )
  end

  alias Expanded = {Completion, Quad}

  def expand(hasherptr, arms : Array({Completion, Atom})) : Array(Expanded)
    arms.map do |completion, atom|
      {completion, quad(hasherptr, atom)}
    end
  end

  alias Marked = {Completion, Quad, QuadMask}

  def mark(atoms, expansions : Array(Expanded)) : Array(Marked)
    answer = atoms.present?(expansions) do |_, quad|
      {quad.a, quad.b, quad.c, quad.d}
    end

    cursor = 0

    expansions.map do |completion, quad|
      mask = QuadMask.new(
        answer[cursor],
        answer[cursor + 1],
        answer[cursor + 2],
        answer[cursor + 3],
      )

      cursor += 4

      {completion, quad, mask}
    end
  end

  def prune(marked : Array(Marked), & : Completion ->) : Array({Completion, Atom})
    arms = [] of {Completion, Atom}

    marked.each do |completion, quad, mask|
      if mask.none?
        yield completion
        next
      end

      mask.select_with_digit(quad) do |atom, digit|
        arms << {completion.dup.append(digit), atom}
      end
    end

    arms
  end

  def prune(marked)
    prune(marked) { }
  end
end

alias Checksum = UInt32

module Utrie
  extend self

  # We hash bases recursively. For instance, the following strand:
  #
  #    Begin - IsDict - At[0] - IsNum - Literal[100]
  #
  # Will be hashed as:
  #
  #    H0 = hash(entity utrie)
  #    H1 = hash(H0 x Begin)
  #    H2 = hash(H1 x IsDict)
  #    H3 = hash(H2 x At[0])
  #    H4 = hash(H3 x IsNum)
  #    H5 = hash(H4 x Literal[100])
  #
  # H5 is the endpoint of the strand. It is then fed to Xgraph and so on.
  private def mount1(atoms, strand : Enumerable(Ubase::Any)) : Atom
    hasher = Blake3.new

    h0 = h(pointerof(hasher), :utrie)

    strand.each do |base|
      h0 = h(pointerof(hasher), h0, base)
      atoms << h0
    end

    h0
  end

  # Mounts the atoms of strands in the strands enumerable *strands* to the Utrie
  # in *atoms*. Each strand is mounted concurrently with the others. Returns a
  # **disordered** array of endpoints corresponding to *strands*.
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  def mount(atoms, strands : Enumerable(Enumerable(Ubase::Any)), *, mt : Bool = true) : Array(Atom)
    wg = WaitGroup.new(strands.size)
    ctx = mt ? MT : ST

    endpoints = [] of Atom
    lock = Mutex.new

    strands.each do |strand|
      ctx.spawn do
        endpoint = mount1(atoms, strand)

        lock.synchronize { endpoints << endpoint }
      ensure
        wg.done
      end
    end

    wg.wait

    endpoints
  end

  # :nodoc:
  @[Flags]
  enum State : UInt8
    BeforeTypecheck
    BeforeKeys
    BeforeLiteral
    BeforeEnd
    End
  end

  # :nodoc:
  record Arm, atom : Atom, arg : Term, state : State

  private def seed(hasherptr, term : Term) : Array(Arm)
    h0 = h(hasherptr, :utrie)
    h0 = h(hasherptr, h0, Ubase::Begin.new)

    [Arm.new(h0, term, State::BeforeTypecheck | State::BeforeEnd)]
  end

  private def sweep(atoms, gen0 : Array(Arm), gen1 : Array(Arm)) : Nil
    answer = atoms.present?(gen0, &.atom)

    gen1.clear
    gen0.each_with_index do |arm, index|
      next unless answer[index] # exists

      gen1 << arm
    end
  end

  private def advance(atoms, hasherptr, gen0, gen1, & : Atom ->) : Nil
    gen0.clear
    gen1.each do |arm|
      if arm.state.before_typecheck?
        case arm.arg.type
        in .any?     then unreachable
        in .symbol?  then base, state1 = Ubase::IsSym.new, State::BeforeLiteral
        in .number?  then base, state1 = Ubase::IsNum.new, State::BeforeLiteral
        in .string?  then base, state1 = Ubase::IsStr.new, State::BeforeLiteral
        in .boolean? then base, state1 = Ubase::IsBool.new, State::BeforeLiteral
        in .dict?    then base, state1 = Ubase::IsDict.new, State::BeforeKeys
        end

        atom1 = h(hasherptr, arm.atom, base)

        # We can stop at e.g. IsNum - End or IsDict - End so add the BeforeEnd
        # state as well.
        gen0 << Arm.new(atom1, arm.arg, state1 | State::BeforeEnd)
      end

      if arm.state.before_end?
        atom1 = h(hasherptr, arm.atom, Ubase::End.new)

        gen0 << Arm.new(atom1, arm.arg, State::End)
      end

      if arm.state.before_keys?
        #   This state is only reachable through BeforeTypechec where we make sure
        # v it is in fact a dict.
        dict = arm.arg.as_d
        dict.each_entry do |key, value|
          atom1 = h(hasherptr, arm.atom, Ubase::At.new(key))

          # We can stop at e.g. IsDict - At(0) - End so add the BeforeEnd state
          # as well.
          gen0 << Arm.new(atom1, value, State::BeforeTypecheck | State::BeforeEnd)
        end
      end

      if arm.state.before_literal?
        atom1 = h(hasherptr, arm.atom, Ubase::Literal.new(arm.arg))

        # We do not put End after Literal. Switch to End state right away.
        gen0 << Arm.new(atom1, arm.arg, State::End)
      end

      if arm.state.end?
        yield arm.atom
      end
    end
  end

  # Yields endpoint atoms for strands from the Utrie in *atoms* that match
  # the given *term*.
  #
  # Currently the implementation is rather sequential, and instead of mult-threading
  # relies on checking for the existence of large batches of atoms at a time. How large
  # depends on the Utrie in *atoms* and on *term*.
  def each_endpoint(atoms, term : Term, & : Atom ->) : Nil
    hasher = Blake3.new

    gen0 = seed(pointerof(hasher), term)
    gen1 = [] of Arm
    lock = Mutex.new

    until gen0.empty?
      sweep(atoms, gen0, gen1)
      advance(atoms, pointerof(hasher), gen0, gen1) { |endpoint| yield endpoint }
    end
  end

  def endpoints(atoms, term : Term) : Array(Atom)
    endpoints = [] of Atom
    each_endpoint(atoms, term) do |endpoint|
      endpoints << endpoint
    end
    endpoints
  end
end

# An emergent graph of intersections. Materialized by "hints" about which binary
# conjunctions exist. The querying side can then "climb" this "ladder of hints",
# inferring higher and higher conjunctions by induction. Sensors can be looked
# up under their top binary conjunction (called the sensor's conjunction *apex*)
# using `SensorRegistry`.
#
# There is a certain trade-off in how smart the Xgraph is and how much packing
# it provides. This is a particularly simple (one may even say crude) implementation;
# it does not care about packing at all. We sort out of necessity, and this will
# provide some kind of packing for very similar conjunctions; but divergence at any
# point will derail this thing completely, and it will proceed to create newer and
# newer nodes. Apparently I'm not smart enough to improve this :^)
module Xgraph
  extend self

  # Returns the apex atom for *conj*, mounting "road sign" atoms along the way
  # that will lead the querying side to the apex.
  #
  # ```text
  #  a      b     c      d      e  [conj]
  #
  #     ab           cd     de
  #
  #          abcd       cdde
  #
  #              abcdcdde [apex]
  # ```
  def mount(atoms, conj : Enumerable(Atom)) : Atom
    hasher = Blake3.new

    gen0 = conj.to_a(&.itself)
    gen1 = [] of Atom

    while gen0.size > 1
      # NOTE: we're sorting by hash. The order won't be obvious. But "rulial"
      # clustering should be preserved (i.e. clustering within rule bounds).
      gen0.unstable_sort!

      cursor = 0
      while cursor < gen0.size
        u = gen0[cursor]
        unless v = gen0[cursor + 1]?
          v = u
          u = gen0[cursor - 1]
        end

        atom = h(pointerof(hasher), :xgraph, u, v)
        gen1 << atom
        atoms << atom

        cursor += 2
      end

      gen0, gen1 = gen1, gen0
      gen1.clear
    end

    gen0.first # apex
  end

  private def explore(wg, atoms, u, vs, promoted, lock) : Nil
    hasher = Blake3.new

    # NOTE: we assume here that allocation is more expensive than hashing.
    # Whether it actually is I'm not sure; I guess it depends on how many
    # positive answers we get. If we get many then we've done twice the job
    # hashing -- bad; if we get little then we've saved some memory on all
    # the negative hashes -- good.

    answer = atoms.present?(vs) { |v| h(pointerof(hasher), :xgraph, u, v) }
    answer.each_with_index do |exists, index|
      next unless exists

      atom = h(pointerof(hasher), :xgraph, u, vs[index])
      lock.synchronize do
        promoted << atom
      end
    end
  end

  # Yields conjunction vertices that exist in *hits* and are part of the Xgraph
  # in *atoms*. The yielded vertices may or may not be conjunction apexes; it
  # is your responsibility to track and check that, if necessary. Note that this
  # method may yield a lot of atoms; how much depends on the size of *hits* and
  # the exhaustiveness of the Xgraph in *atoms*.
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  def each_conjv(atoms, hits : Enumerable(Atom), *, mt : Bool = true, & : Atom ->) : Nil
    gen0 = hits.to_a(&.itself)
    gen1 = [] of Atom

    wg = WaitGroup.new
    ctx = mt ? MT : ST
    lock = Mutex.new

    while gen0.size > 1
      gen0.unstable_sort!
      gen0.each { |conjv| yield conjv }

      wg.add(gen0.size - 1)

      (0...gen0.size - 1).each do |index|
        u = gen0[index]
        vs = gen0.to_readonly_slice[index + 1..]

        ctx.spawn do
          explore(wg, atoms, u, vs, gen1, lock)
        ensure
          wg.done
        end
      end

      wg.wait

      gen0, gen1 = gen1, gen0
      gen1.clear
    end

    if top = gen0.first?
      yield top
    end
  end

  def conjvs(atoms, hits : Enumerable(Atom), **kwargs) : Array(Atom)
    conjvs = [] of Atom
    each_conjv(atoms, hits, **kwargs) do |conjv|
      conjvs << conjv
    end
    conjvs
  end
end

# A sensor registry is an emergent data structure that associates a sensor
# conjunction apex to a sensor id. It is effectively a table with
# the following columns:
#
# ```text
#    secret     apex    sensor id     checksum
#   primary   primary
# ```
#
# Both the secret and the conjunction apex are assumed to be known by
# the querying side implicitly. The underlying bytes multimap only stores
# starting from the sensor id, assuming the prefix of secret followed
# by apex implicitly. This provides an arguable degree of cryptographic
# security to entries in the registry: only querying sides that know
# the secret and were able to derive the apex as well are able to access
# the sensor id.
#
# The sensor id acts as a connection pointer as well. In fact, only the last
# four bytes of the sensor id identify the sensor itself; the former 12 bytes
# identify the connection to which this sensor belongs, something akin to an
# IP address.
module SensorRegistry
  extend self

  Log = ::Log.for(self)

  # Creates a record in the sensor registry, pointing each of *apexes* to
  # the given *sensor* under *secret*.
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  def register(atoms, secret : Term?, apexes : Indexable(Atom), sensor : WWID, *, mt : Bool = true) : Nil
    secret_slice = secret_to_bytes(secret)

    wg = WaitGroup.new(apexes.size)
    ctx = mt ? MT : ST

    apexes.each do |apex|
      ctx.spawn do
        entry = cursor = Bytes.new(secret_slice.size + Atom::BYTESIZE + WWID::BYTESIZE + sizeof(Checksum))

        cursor.copy_from(secret_slice)
        cursor += secret_slice.size

        apex.copy_hash_to(cursor)
        cursor += Atom::BYTESIZE

        sensor.to_slice_be(cursor)
        cursor += WWID::BYTESIZE

        # Compute checksum. Note how we leave the secret unhashed. This acts as a
        # tiny protection against hash collision for secrets: now both the hash
        # and the checksum computed from raw secret must collide, which is a bit
        # less likely I suppose. Although we're walking on very shaky ground
        # here anyway.
        checksum = Digest::CRC32.checksum(entry[...-sizeof(Checksum)])

        IO::ByteFormat::BigEndian.encode(checksum, cursor)
        cursor += sizeof(Checksum)

        key = entry[0, secret_slice.size + Atom::BYTESIZE]
        data = entry[secret_slice.size + Atom::BYTESIZE, WWID::BYTESIZE + sizeof(Checksum)]

        BytesMultimap.add(atoms, :sensor_registry, key, data)
      ensure
        wg.done
      end
    end

    wg.wait
  end

  # Calls *fn* with each sensor registered at each of *apexes* under *secret*.
  # Sensors may repeat if one sensor is registered at multiple *apexes*.
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  #
  # WARNING: *fn* will be called from another fiber, perhaps running on another
  # thread if *mt* is `true` (it is by default). Thus make sure to either have
  # fully compartmentalized *fn*, or *fn* that talks to the outside world in a
  # thread-safe manner.
  def each_sensor(atoms, secret : Term?, apexes : Indexable(Atom), *, mt : Bool = true, &fn : WWID ->) : Nil
    secret_slice = secret_to_bytes(secret)

    ctx = mt ? MT : ST
    wg = WaitGroup.new(apexes.size)

    apexes.each do |apex|
      ctx.spawn do
        key = cursor = Bytes.new(secret_slice.size + Atom::BYTESIZE)

        cursor.copy_from(secret_slice)
        cursor += secret_slice.size

        apex.copy_hash_to(cursor)
        cursor += Atom::BYTESIZE

        # Key is readonly from this point onward. It must be, since we access
        # it from complete() callback which could be called from another
        # fiber/thread.

        BytesMultimap.complete(atoms, :sensor_registry, key, prefix: Bytes.empty, mt: mt) do |completion, _|
          entry = completion.final(key, prefix: Bytes.empty)

          next if entry.size == key.size # No completions

          # Verify size
          unless entry.size == (expected = key.size + WWID::BYTESIZE + sizeof(Checksum))
            Log.debug { "reject entry: size too small (#{entry.size} != #{expected})" }
            next
          end

          # Check checksum
          checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, entry[-sizeof(Checksum)..])
          checksum1 = Digest::CRC32.checksum(entry[...-sizeof(Checksum)])

          unless checksum0 == checksum1
            Log.debug { "reject entry: checksum mismatch (my #{checksum1} != its #{checksum0})" }
            next
          end

          # Decode sensor id
          sensor_slice = entry[key.size, WWID::BYTESIZE]

          begin
            sensor = WWID.from_slice_be(sensor_slice)
          rescue e : WWID::ParseError
            Log.debug(exception: e) { "reject entry" }
            next
          end

          # Call fn with sensor id
          fn.call(sensor)
        end
      ensure
        wg.done
      end
    end

    wg.wait
  end
end

# Ubases are tiny gate-keeper nodes for `Utrie` and the internal term trie
# created by `AppearanceRegistry`.
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

  # Passes a dictionary term's value for key *term* forward.
  record At, term : Term

  # Anchor put at the beginning of all strands.
  record Begin

  # Indicates an abrupt (non-literal) stop. This base is not emitted if the strand
  # ends with `Literal`.
  record End

  # Passes only symbol terms forward.
  record IsSym

  # Passes only string terms forward.
  record IsStr

  # Passes only number terms forward.
  record IsNum

  # Passes only boolean terms forward.
  record IsBool

  # Passes only dictionary terms forward.
  record IsDict

  # Passes foward only terms that match *term* exactly.
  record Literal, term : Term
end

# :nodoc:
enum Uopcode : UInt8
  Begin
  End
  IsSym
  IsStr
  IsNum
  IsBool
  IsDict
  HashedAt
  QuotedAt
  HashedLiteral
  QuotedLiteral
end

module AppearanceRegistry
  extend self

  private def ubases(keypath : Stack(Term), leaf : Term) : Array(Ubase::Any)
    strand = [] of Ubase::Any
    strand << Ubase::Begin.new

    keypath.each do |key|
      strand << Ubase::IsDict.new
      strand << Ubase::At.new(key)
    end

    case leaf.type
    in .any?     then unreachable
    in .symbol?  then strand << Ubase::IsSym.new
    in .string?  then strand << Ubase::IsStr.new
    in .number?  then strand << Ubase::IsNum.new
    in .boolean? then strand << Ubase::IsBool.new
    in .dict?    then strand << Ubase::IsDict.new
    end

    strand << Ubase::Literal.new(leaf)
    strand
  end

  private def upack(io, ubase : Ubase::Begin) : Nil
    io.write_byte(Uopcode::Begin.value)
  end

  private def upack(io, ubase : Ubase::End) : Nil
  end

  {% for base in %w[IsSym IsStr IsNum IsBool IsDict] %}
    private def upack(io, ubase : Ubase::{{base.id}}) : Nil
      io.write_byte(Uopcode::{{base.id}}.value)
    end
  {% end %}

  {% for base in %w[At Literal] %}
    private def upack(io, ubase : Ubase::{{base.id}}) : Nil
      if ML.compact_bytesize(ubase.term) <= Atom::BYTESIZE
        io.write_byte(Uopcode::Quoted{{base.id}}.value)

        ML.compact(io, ubase.term)
      else
        io.write_byte(Uopcode::Hashed{{base.id}}.value)

        scratch = uninitialized UInt8[Atom::BYTESIZE]
        hasher = Blake3.new

        digester = IO::ByteStream.new { |slice| hasher.update(slice) }
        ML.compact(digester, ubase.term)

        hasher.final(scratch.to_slice)

        io.write(scratch.to_slice)
      end
    end
  {% end %}

  private def upack(strand : Array(Ubase::Any)) : Bytes
    io = IO::Memory.new

    strand.each do |base|
      upack(io, base)
    end

    io.to_slice
  end

  # :nodoc:
  APPEARANCE_SET_GAP = "appearances".to_slice

  private def mount1(wg, atoms, secret_slice : Bytes, ubases : Array(Ubase::Any), appearance : WWID) : Nil
    data = upack(ubases)

    terminal = BytesMultimap.add(atoms, :appearance_registry, secret_slice, data: data)

    # Insert an artificial gap after the terminal atom in data. After this gap we
    # will have the appearances subscribed to the strand.
    terminal = BytesMultimap.append(terminal, APPEARANCE_SET_GAP)

    # Append the appearance id after the gap. The gap is implicit. The other side
    # will need to pass it on its own.
    scratch = uninitialized UInt8[WWID::BYTESIZE]

    appearance.to_slice_be(scratch.to_slice)

    BytesMultimap.add(atoms, terminal, scratch.to_slice)
  ensure
    wg.done
  end

  # Subscribes *appearance* to perceptions of *value* under *secret*.
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  def mount(atoms, secret : Term?, value : Term, appearance : WWID, *, mt : Bool = true) : Nil
    secret_slice = secret_to_bytes(secret)

    ctx = mt ? MT : ST
    wg = WaitGroup.new

    Term.each_keypath_and_leaf(value) do |keypath, leaf|
      ubases = ubases(keypath, leaf)

      wg.add
      ctx.spawn do
        mount1(wg, atoms, secret_slice, ubases, appearance)
      end

      true # continue
    end

    wg.wait
  end

  alias Row = {Completion, Atom}

  private def bundleof(atoms, secret_slice : Bytes, strand : Array(Ubase::Any), mt : Bool) : Array(Row)
    prefix = upack(strand)

    # The completion callback runs on different threads (that is, it may).
    # So we must synchronize somehow.
    bundle = [] of Row
    lock = Mutex.new

    BytesMultimap.complete(atoms, :appearance_registry, secret_slice, prefix, mt: mt) do |completion, atom|
      # Remember that we insert an artificial gap between the strand bytes and the set
      # of appearances subscribed to that strand (represented as a digit trie). To fill
      # this gap we have to complete 0, by an implicit mount-query consensus; there are
      # no explicit hints for us to do that in the set, so complete() terminates -- not
      # knowing what to do. We know, though -- we have to complete 0.
      row = {Completion.new, BytesMultimap.append(atom, APPEARANCE_SET_GAP)}

      lock.synchronize { bundle << row }
    end

    # At this point we know all completion fibers have terminated. We can use
    # row without a lock safely.

    bundle
  end

  def each_appearance(atoms, secret : Term?, strands, *, mt : Bool = true, &fn : WWID ->) : Nil
    secret_slice = secret_to_bytes(secret)

    wg = WaitGroup.new
    ctx = mt ? MT : ST

    # Convert strands to bundles concurrently.
    bundles = [] of Array(Row)
    lock = Mutex.new

    strands.each do |strand|
      wg.add

      ctx.spawn do
        bundle = bundleof(atoms, secret_slice, strand, mt)

        lock.synchronize { bundles << bundle }
      ensure
        wg.done
      end
    end

    wg.wait

    hasher = Blake3.new
    marked = [] of Array(BytesMultimap::Marked)
    expanded = [] of Array(BytesMultimap::Expanded)
    populations = [] of Set(Completion)

    (WWID::BYTESIZE*4 + 1).times do |ord|
      return if bundles.empty?

      # Expand each bundle with possible digit completions.
      expanded.clear
      expanded.concat(bundles) { |bundle| BytesMultimap.expand(pointerof(hasher), bundle) }

      # FIXME: we need to run mark() in concurrently. Otherwise mark()
      # would block for each bundle -- nonsense!!

      # Mark each digit completion according to whether it is present in *atoms*.
      marked.clear
      marked.concat(expanded) { |bundle| BytesMultimap.mark(atoms, bundle) }

      unless ord == WWID::BYTESIZE*4
        # Prune all dead-end completions.
        bundles.clear
        bundles.concat(marked) { |bundle| BytesMultimap.prune(bundle) }

        # Index for cheap intersection
        populations.clear
        populations.concat(bundles) do |bundle|
          bundle.to_set { |completion, _| completion }
        end

        # Select only those completions that are present in all other bundles.
        bundles.each do |bundle0|
          xsect = bundle0.select! do |completion, _|
            populations.all? { |population| completion.in?(population) }
          end

          # If any bundle ends up being empty, then all other bundles will
          # be empty and so on. No point in continuing to complete.
          return if xsect.empty?
        end

        next
      end

      marked.each do |bundle|
        # Dead-end completions at this point are valid completions. Process them.
        BytesMultimap.prune(bundle) do |completion|
          bytesize = completion.bytesize(key: Bytes.empty, prefix: Bytes.empty)
          unless bytesize == WWID::BYTESIZE
            pp completion
            Log.debug { "reject entry: unexpected entry bytesize #{bytesize}" }
            next
          end

          scratch = uninitialized UInt8[WWID::BYTESIZE]
          entry = scratch.to_slice
          completion.final_to(entry, key: Bytes.empty, prefix: Bytes.empty)

          begin
            appearance = WWID.from_slice_be(entry)
          rescue e : WWID::ParseError
            Log.debug(exception: e) { "reject entry" }
            next
          end

          fn.call(appearance)
        end
      end

      break
    end
  end
end

struct AtomSink
  def initialize(@sink : Atom ->)
  end

  def <<(atom : Atom) : self
    @sink.call(atom)

    self
  end
end

module Surface
  # Calls *sink* with atoms that constitute `self`. Atoms may repeat with intent.
  # This can be used for e.g. refcounting (instead of using a set, you can use
  # a multiset and have much less disruptive removals later on).
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  #
  # WARNING: *sink* must be thread-safe -- it will be called from different fibers.
  # If you don't want to be bothered with thread-safety, use `atoms` which returns
  # a set of atoms and does the thread-safety stuff on its own.
  abstract def each_atom(instant : WWID, *, mt : Bool = true, &sink : Atom ->) : Nil

  def atoms_to(instant, object, **kwargs) : Nil
    each_atom(instant, **kwargs) do |atom|
      object << atom
    end
  end

  # Returns the set of atoms that constitute `self`.
  #
  # See `each_atom` for *kwargs*.
  def atoms(instant, **kwargs) : Set(Atom)
    atoms = Set(Atom).new
    lock = Mutex.new

    each_atom(instant, **kwargs) do |atom|
      lock.synchronize { atoms << atom }
    end

    atoms
  end

  def complements(atoms, **kwargs) : Set(WWID)
    complements = Set(WWID).new
    lock = Mutex.new

    each_complement(atoms, **kwargs) do |complement|
      lock.synchronize { complements << complement }
    end

    complements
  end
end

class Sensor
  include Surface

  alias Strand = Array(Ubase::Any)
  alias StrandList = Array(Strand)
  alias BranchList = Array(StrandList)

  protected def initialize(@pattern : Term, @secret : Term?, @branches : BranchList)
    if @branches.empty?
      raise ArgumentError.new("branches list must contain at least one branch")
    end
  end

  private SK_ANY  = Term.of({:"%any"})
  private SK_SYM  = Term.of({:"%symbol"})
  private SK_STR  = Term.of({:"%string"})
  private SK_NUM  = Term.of({:"%number", :_})
  private SK_DICT = Term.of({:"%dict"})
  private SK_BOOL = Term.of({:"%boolean"})

  # Converts skeleton strand *bases* to a `Strand`.
  private def self.strand(bases : Term::Dict) : Strand
    strand = Strand.new
    state = :start

    bases.items.each do |base|
      case state
      when :start
        unless base == SK_ANY
          raise "BUG: unexpected base #{base}, expected (%any)"
        end
        strand << Ubase::Begin.new
        state = :typecheck
      when :typecheck
        case base
        when SK_DICT
          strand << Ubase::IsDict.new
          state = :key
        when SK_SYM
          strand << Ubase::IsSym.new
          state = :literal
        when SK_NUM
          strand << Ubase::IsNum.new
          state = :literal
        when SK_STR
          strand << Ubase::IsStr.new
          state = :literal
        when SK_BOOL
          strand << Ubase::IsBool.new
          state = :literal
        else
          raise "BUG: unexpected base #{base}, expected typecheck"
        end
      when :key
        Term.case(base) do
          matchpi %{(%'%literal ())} do # empty dict
            strand << Ubase::Literal.new(Term.of)
            state = :after_literal
          end

          matchpi %{(%'%value (%'%literal term_))} do
            strand << Ubase::At.new(term)
            state = :typecheck
          end

          otherwise do
            raise "BUG: unexpected base #{base}, expected %value %literal"
          end
        end
      when :literal
        Term.case(base) do
          matchpi %{(%'%literal term_)} do
            strand << Ubase::Literal.new(term)
            state = :after_literal
          end

          otherwise do
            raise "BUG: unexpected base #{base}, expected %literal"
          end
        end
      when :after_literal
        raise "BUG: expected end-of-strand after literal, but found base #{base}"
      end
    end

    # Indicate abrupt end (as in e.g. `Begin - *` or `Begin - IsDict - At(0) - *`) by
    # an explicit End base. If we had literal we treat it as end-of-strand regardless.
    unless state == :after_literal
      strand << Ubase::End.new
    end

    strand
  end

  # Breaks *branch* down into its constituent strands and so on.
  private def self.strand_list(branch : Term) : StrandList
    strands = StrandList.new
    M1.strands(branch) do |bases|
      strands << strand(bases)
    end
    strands
  end

  # Breaks *skeleton* down into its constituent branches and so on.
  private def self.branch_list(skeleton : Term) : BranchList
    branches = BranchList.new
    M1.branches(skeleton) do |branch|
      branches << strand_list(branch)
    end
    branches
  end

  # Returns the skeleton of *pattern*.
  #
  # See also: `M1.skeleton`.
  private def self.skeleton(pattern : Term) : Term
    pipe(pattern, M1.normal, M1.skeleton)
  end

  def self.new(pattern : Term, secret : Term? = nil) : Sensor
    branches = pipe(pattern, skeleton, branch_list)

    new(pattern, secret, branches)
  end

  def each_atom(instant : WWID, *, mt : Bool = true, &sink : Atom ->) : Nil
    atoms = AtomSink.new(sink)

    if @branches.size == 1
      strands = @branches[0]
      endpoints = Utrie.mount(atoms, strands, mt: mt)
      apexes = {Xgraph.mount(atoms, endpoints)}
    else
      wg = WaitGroup.new(@branches.size)
      ctx = mt ? MT : ST

      apexes = [] of Atom
      lock = Mutex.new

      @branches.each do |strands|
        ctx.spawn do
          endpoints = Utrie.mount(atoms, strands, mt: mt)
          apex = Xgraph.mount(atoms, endpoints)

          lock.synchronize { apexes << apex }
        ensure
          wg.done
        end
      end

      wg.wait
    end

    SensorRegistry.register(atoms, @secret, apexes, instant, mt: mt)
  end

  def each_complement(atoms, *, mt : Bool = true, &sink : WWID ->) : Nil
    if @branches.size == 1
      strands = @branches[0]
      AppearanceRegistry.each_appearance(atoms, @secret, strands, mt: mt, &sink)
      return
    end

    wg = WaitGroup.new(@branches.size)
    ctx = mt ? MT : ST

    @branches.each do |strands|
      ctx.spawn do
        AppearanceRegistry.each_appearance(atoms, @secret, strands, mt: mt, &sink)
      ensure
        wg.done
      end
    end

    wg.wait
  end
end

class Appearance
  include Surface

  def initialize(@value : Term, @secret : Term? = nil)
  end

  def each_atom(instant : WWID, *, mt : Bool = true, &sink : Atom ->) : Nil
    AppearanceRegistry.mount(AtomSink.new(sink), @secret, @value, instant, mt: mt)
  end

  def each_complement(atoms, *, mt : Bool = true, &sink : WWID ->) : Nil
    hits = Utrie.endpoints(atoms, @value)
    conjvs = Xgraph.conjvs(atoms, hits, mt: mt)

    SensorRegistry.each_sensor(atoms, @secret, conjvs, mt: mt, &sink)
  end
end

class MySet
  def initialize(@n : Int32)
    @sets = Slice(Set(Atom)).new(@n) { Set(Atom).new }
    @locks = Slice(Mutex).new(@n) { Mutex.new }
  end

  def <<(atom : Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket] << atom
    end
  end

  def present?(atom : Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket].includes?(atom)
    end
  end

  def size
    @sets.sum(&.size)
  end

  def present?(objects : Enumerable(T), & : T -> Atom | Enumerable(Atom)) : BitList forall T
    answer = BitList.new

    objects.each do |object|
      ee = yield object

      unless ee.is_a?(Enumerable(Atom))
        ee = {ee}
      end

      ee.each do |atom|
        answer << present?(atom)
      end
    end

    answer
  end
end

Log.setup_from_env(default_level: :debug)

tspace = MySet.new(4096)

# trunk = WWID.next

# puts "Generate appearances"

# appearances = (0...100).flat_map do |x|
#   (0...100).map do |y|
#     Appearance.new(Term.of(type: "pixel", x: x, y: y, color: {rand(UInt8), rand(UInt8), rand(UInt8)}))
#   end
# end.to_readonly_slice

# puts "Insert appearances into termspace"

# wg = WaitGroup.new

# wg.spawn do
#   slot = 0u32
#   piece = appearances[0...2500]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 1 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.spawn do
#   slot = 2500u32
#   piece = appearances[2500...5000]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 2 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.spawn do
#   slot = 5000u32
#   piece = appearances[5000...7500]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 3 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.spawn do
#   slot = 7500u32
#   piece = appearances[7500...10000]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 4 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.wait

# # # appearances.each_with_index do |appearance, index|
# # #   if index % 1000 == 0
# # #     puts "Insert #{index}/#{appearances.size}"
# # #   end

# # # end

# puts "Done, 100x100 set cost is #{tspace.size}"

# s = Sensor.new(Term.of(color: {0, :_, :_}))

# 100.times do
# dt, comp = Time.measured do
#   s.complements(tspace)
# end

# pp comp
# puts "Took #{dt.total_milliseconds}ms"
# end

# while true
#   puts "Enter sensor pattern ML"

#   q = ML.term(gets || break)

#   sensor = Sensor.new(q)

#   pp sensor

#   complements = Set(WWID).new
#   dt = Time.measure do
#     complements = sensor.complements(tspace)
#   end

#   puts "#{complements.size} complement(s). Done in #{dt.total_milliseconds}ms"
# end

# {% skip_file %}

trunk = WWID.next
aid0 = trunk = trunk.succ
aid1 = trunk = trunk.succ
aid2 = trunk = trunk.succ
aid3 = trunk = trunk.succ
sid0 = trunk = trunk.succ
sid1 = trunk = trunk.succ

s0 = Sensor.new(Term.of(type: "pixel", x: {:"%any", 0, 1}, y: :y_number))
s1 = Sensor.new(Term.of(type: "pixel", color: {:_, :_, 255}))

a1 = Appearance.new(Term.of(type: "pixel", x: 0, y: 100, color: {255, 0, 0}))
a2 = Appearance.new(Term.of(type: "pixel", x: 1, y: 200, color: {0, 255, 0}))
a3 = Appearance.new(Term.of(type: "pixel", x: 2, y: 300, color: {0, 0, 255}))
a4 = Appearance.new(Term.of(type: "pixel", x: 0, y: 400, color: {255, 0, 255}))

dt = Time.measure do
  s0.atoms_to(sid0, tspace)
  s1.atoms_to(sid1, tspace)
  a1.atoms_to(aid0, tspace)
  a2.atoms_to(aid1, tspace)
  a3.atoms_to(aid2, tspace)
  a4.atoms_to(aid3, tspace)

  scomps = s0.complements(tspace)
  expect scomps == Set{aid0, aid1, aid3}

  scomps = s1.complements(tspace)
  expect scomps == Set{aid2, aid3}

  acomps = a3.complements(tspace)
  expect acomps == Set{sid1}

  acomps = a2.complements(tspace)
  expect acomps == Set{sid0}

  acomps = a4.complements(tspace)
  expect acomps == Set{sid0, sid1}
end

puts "OK in #{dt.total_milliseconds}ms"

# # acomps = a.complements(atoms)
# # pp acomps

# {% skip_file unless flag?(:tail) %}
# atoms = MySet(1).new
# endpoints = Utrie.mount(atoms, [
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(0))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(1))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(2))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(3))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(4))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(5))},
# ])
# pp endpoints
# Utrie.each_endpoint(atoms, Term.of(x: 3)) do |ep|
#   pp ep
# end

# {% if flag?(:test_xgraph) %}
#   atoms = MySet(1024).new

#   nletters = 30
#   nwords = 1000
#   maxwordlen = 20
#   nchoices = 300
#   nepochs = 1000

#   # Generate letters
#   alphabet = [] of Atom
#   (0...nletters).each do |letter|
#     alphabet << Atom.of("#{letter}")
#   end

#   # Generate rules (words)
#   words = {} of Atom => Array(Atom)

#   (0...nwords).each do
#     length = (1...maxwordlen).sample
#     word = alphabet.sample(length)
#     apex = Xgraph.mount(atoms, word)
#     unless words.put?(apex, word)
#       Log.debug { "generated duplicate word #{word}" }
#       next
#     end
#   end

#   nepochs.times do |epoch|
#     puts "Epoch #{epoch}/#{nepochs} (#{((epoch/nepochs) * 100).round(2)}%)"
#     # Pick N random words
#     choices = words.sample(nchoices)

#     expected = Set(Atom).new
#     pool = Set(Atom).new

#     choices.each do |word, letters|
#       expected << word
#       pool.concat(letters)
#     end

#     hit = Set(Atom).new
#     dt = Time.measure do
#       Xgraph.each_conjv(atoms, pool) do |conjv|
#         hit << conjv
#       end
#     end

#     expect expected.subset_of?(hit)

#     puts "OK in #{dt.total_milliseconds}ms!"
#   end
# {% end %}

# require "benchmark"

# Benchmark.ips do |x|
#   x.report("gen") do

# WWID.next
#   end
# end
# require "benchmark"


set = MySet.new(1024)
conid = WWID.next
origin = conid
aid0 = origin = origin.succ
aid1 = origin = origin.succ
aid2 = origin = origin.succ

pp! aid0
pp! aid1
pp! aid2

require "benchmark"

lock = Mutex.new

AppearanceRegistry.mount(set, nil, Term.of(:add, 1, 2, 3, 4, 5), aid0)
AppearanceRegistry.mount(set, nil, Term.of(:sub, 1, 2), aid1)
AppearanceRegistry.mount(set, nil, Term.of(:sub, "hello", 4), aid2)

# _
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new] }) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# _dict
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new] }) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# (_)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0))] }) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# (_symbol)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new] }) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# (add)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:add))] }) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0}

# (sub)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))] }) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid1, aid2}

# (sub _)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, {
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1))],
}) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid1, aid2}

# (sub _number)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, {
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsNum.new],
}) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid1}

# (sub _string)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, {
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsStr.new],
}) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid2}

puts "OK"
require "benchmark"

Benchmark.ips do |x|
  x.report("do it") do
# 1000.times do
 seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, {
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsStr.new],
}) do |aid|
  lock.synchronize { seen << aid }
end
end
end

#   end
# end
# p = Xgraph.mount(set, Deque{Atom.of("a"), Atom.of("b")})
# q = Xgraph.mount(set, Deque{Atom.of("a"), Atom.of("b"), Atom.of("c")})
# r = Xgraph.mount(set, Deque{Atom.of("b"), Atom.of("c")})
# pp! p
# pp! q
# pp! r

# Xgraph.each_conjv(set, Deque{Atom.of("a"), Atom.of("b"), Atom.of("c"), Atom.of("d")}) do |conjv|
#   pp conjv
# end

# sid0 = WWID.next
# sid1 = WWID.next
# SensorRegistry.register(set, nil, {Atom.of(Blake3.final("a")), Atom.of(Blake3.final("b")), Atom.of(Blake3.final("c"))}, sid0)
# SensorRegistry.register(set, nil, {Atom.of(Blake3.final("d"))}, sid1)
# apexes = {Atom.of(Blake3.final("a")), Atom.of(Blake3.final("b")), Atom.of(Blake3.final("c")), Atom.of(Blake3.final("d"))}
# require "benchmark"

#  n =Atomic.new(0)
# Benchmark.ips do |x|
#   x.report("speed") do
#     n.set(0)
# SensorRegistry.each_sensor(set, nil, apexes) do |s|
#   n.add(1)
# end
#   end
# end
# pp n

# pp set.@sets[0].size

# (0...10_000).each do |x|
#   BytesMultimap.add(set, :sensor_registry, "".to_slice, ('a'..'z').sample(16).to_readonly_slice(&.ord.to_u8))
# end

# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "1 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "2 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "3 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "4 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "5 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "6 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)


# require "benchmark"

# n = Atomic.new(0)
# Benchmark.ips do |x|
#   x.report("receive 10000 st") do
#     n.set(0)
#  BytesMultimap.complete(set, :sensor_registry, "".to_slice, "".to_slice, mt: false) do |comp|
#    n.add(1)
# end
#   end
#   x.report("receive 10000 mt") do
#     n.set(0)
#  BytesMultimap.complete(set, :sensor_registry, "".to_slice, "".to_slice) do |comp|
#    n.add(1)
# end
#   end
# end
# # pp atoms.size

# pp n
# # puts
# # pp atoms.size


