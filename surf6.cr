require "./src/wirewright"
require "log"
require "./blake3"
require "digest"
require "wait_group"

# - Utrie    ;; maps bases to endpoints
# + Xgraph   ;; maps endpoint conjunctions to conjunction apex through binary conjunctions
# + BytesMultimap
#   + SensorRegistry     ;; maps conjunction apex to sensor ids
#   - AppearanceRegistry ;; maps term strands to appearance ids

enum Entity : UInt8
  SensorRegistry
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

  def_equals @blk0, @blk1, @blk2, @blk3
end

# ```text
#                        randomness
#                    -----------------
#  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
#  -----------------                   -----------
#  ms since 1 Jan 2025                    slot (client-defined sequ)
# ```
struct WWID
  @@slot = Atomic(UInt32).new(0u32)

  WW_EPOCH = Time.utc(year: 2025, month: 1, day: 1)
  WW_ORDER_MAX = 0xff_ff_ff_ff_ff_ffu64

  BYTESIZE = 16

  def initialize(@raw : UInt128)
  end

  # Generates a new WWID.
  def self.next : WWID
    order = (Time.utc - WW_EPOCH).total_milliseconds.ceil.to_u128
    if order > WW_ORDER_MAX
      raise OverflowError.new("order overflow")
    end

    disorder = Random::Secure.rand(UInt64)
    slot = @@slot.add(1, :relaxed)
    raw = (order << 10*8) | (disorder << 4*8) | slot

    new(raw)
  end

  def self.from_slice_be?(slice : Bytes) : WWID?
    return unless slice.size == BYTESIZE

    raw = IO::ByteFormat::BigEndian.decode(UInt128, slice)

    instance = new(raw)

    now = Time.local
    if now < instance.created_at # Order is in the future
      return
    end

    instance
  end

  def to_slice_be(target = Bytes.new(16)) : Bytes
    IO::ByteFormat::BigEndian.encode(@raw, target)

    target
  end

  def created_at : Time
    order = @raw >> (6*8 + 4*8)

    WW_EPOCH + order.milliseconds
  end

  def inspect(io)
    io << "#'"

    @raw.to_s(io, base: 62, precision: 22)
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

# :nodoc:
ST = Fiber::ExecutionContext::SingleThreaded.new("Meridium single-threaded")

# :nodoc:
MT = Fiber::ExecutionContext::MultiThreaded.new("Meridium multi-threaded", System.cpu_count.to_i)

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
  def add(atoms, entity : Entity, key, data : Bytes) : Nil
    hasher = Blake3.new

    h0 = h(pointerof(hasher))
    h0 = h(pointerof(hasher), h0, entity.value)
    h0 = h(pointerof(hasher), h0, key)

    each_b4_digit(data) do |digit|
      h0 = h(pointerof(hasher), h0, digit)
      atoms << h0
    end
  end

  # :nodoc:
  #
  # TODO: split *data* into blocks to avoid copying all data on clone. E.g.
  # blocks of 4 bytes or 8 bytes etc. So that we only copy the last e.g. 8 bytes
  # on clone and the blocks are only copied on block append.
  record Completion, key : Bytes, prefix : Bytes, data = [] of UInt8, tip = 0u8, cursor = 0u8 do
    def final : Bytes
      @data << tip unless cursor.zero?

      result = cursor = Bytes.new(key.size + prefix.size + data.size)

      cursor.copy_from(key)
      cursor += key.size

      cursor.copy_from(prefix)
      cursor += prefix.size

      cursor.copy_from(data.to_readonly_slice)
      cursor += data.size

      result
    end

    def clone : Completion
      copy_with(data: data.dup, tip: tip, cursor: cursor)
    end

    def append(digit : UInt8) : Completion
      unless 0 <= digit <= 3
        raise ArgumentError.new
      end

      tip, cursor = @tip, @cursor
      tip |= digit << (6 - cursor)
      cursor += 2 # one base-4 digit

      if cursor == 8
        @data << tip
        tip = cursor = 0u8
      end

      copy_with(tip: tip, cursor: cursor)
    end
  end

  # Each exploration fiber is running this method.
  private def explore(wg, atoms, completion, h0 : Atom, fn : Bytes ->) : Nil
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

        completion1 = completion.clone.append(digit)

        wg.add

        # Spawn further fibers to explore the other branches if their
        # atoms were found to exist.
        spawn explore(wg, atoms, completion1, candidate, fn)
      end

      unless first
        fn.call(completion.final)
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
  def complete(atoms, entity : Entity, key : Bytes, prefix : Bytes, *, mt : Bool = true, &fn : Bytes ->) : Nil
    hasher = Blake3.new

    h0 = h(pointerof(hasher))
    h0 = h(pointerof(hasher), h0, entity.value)
    h0 = h(pointerof(hasher), h0, key)

    each_b4_digit(prefix) do |digit|
      h0 = h(pointerof(hasher), h0, digit)
    end

    completion0 = Completion.new(key, prefix)

    wg = WaitGroup.new
    wg.add

    ctx = mt ? MT : ST
    ctx.spawn { explore(wg, atoms, completion0, h0, fn) }

    wg.wait
  end
end

alias Checksum = UInt32

# FIXME: h0 - entity consensus prefix to scope things off!!!!

# An emergent graph of intersections. Materialized by "hints" about which binary
# conjunctions exist. The querying side can then "climb" this "ladder of hints",
# inferring higher and higher conjunctions by induction. Sensors can be looked
# up under their top binary conjunction (called the sensor's conjunction *apex*)
# using `SensorRegistry`.
#
# There is a certain trade-off in how smart the Xgraph is vs. how much packing
# it provides. This is a particularly simple (one may even say crude) implementation;
# it does not care about packing at all. We sort out of necessity, and this will
# provide some kind of packing for very similar conjunctions; but divergence at any
# point will derail this thing completely, and it will proceed to create newer and
# newer nodes. I'm not smart enough to improve this :^)
#
# I'm still not sure whether this implementation actually works. It appears to.
module Xgraph
  extend self

  # Mounts the given conjunction *conj* in the Xgraph. Returns its apex vertex.
  #
  # TODO: parallelize
  def mount(atoms, conj : Deque(Atom)) : Atom
    if conj.empty?
      raise ArgumentError.new("expected a nonempty conjunction")
    end

    hasher = Blake3.new

    conj.unstable_sort!

    while conj.size > 1
      u = conj.shift
      v = conj.shift
      conjv = h(pointerof(hasher), u, v)
      atoms << conjv
      conj << conjv
    end

    conj.first # apex
  end

  # Yields conjunction vertices that exist in *hits* and are part of the Xgraph
  # in *atoms*. The yielded vertices may or may not be conjunction apexes; it
  # is your responsibility to track and check that, if necessary.
  #
  # TODO: parallelize
  def each_conjv(atoms, hits : Deque(Atom), & : Atom ->) : Nil
    if hits.empty?
      raise ArgumentError.new("hits must contain at least one vertex")
    end

    hasher = Blake3.new

    hits.unstable_sort!

    while hits.size > 1
      u = hits.shift

      yield u

      query = hits.map { |v| h(pointerof(hasher), u, v) }

      answer = atoms.present?(query, &.itself)
      answer.each_with_index do |exists, index|
        next unless exists

        hits << query[index]
      end
    end

    yield hits.first
  end
end

# A sensor registry is an emergent data structure that associates a sensor
# conjunction apex to a sensor id. It is effectively a table with
# the following rows:
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
    if secret
      io = IO::Memory.new
      io.write_byte(1)
      ML.compact(io, secret)
      secret_slice = io.to_slice
    else
      secret_slice = Bytes[0]
    end

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
    if secret
      io = IO::Memory.new
      io.write_byte(1)
      ML.compact(io, secret)
      secret_slice = io.to_slice
    else
      secret_slice = Bytes[0]
    end

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

        BytesMultimap.complete(atoms, :sensor_registry, key, prefix: Bytes.empty, mt: mt) do |entry|
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

          unless sensor = WWID.from_slice_be?(sensor_slice)
            Log.debug { "reject entry: invalid or nonsensical sensor byte sequence: #{sensor_slice.hexstring}" }
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

class MySet(N)
  def initialize
    @sets = StaticArray(Set(Atom), N).new { Set(Atom).new }
    @locks = StaticArray(Mutex, N).new { Mutex.new }
  end

  def <<(atom : Atom)
    bucket = atom.@blk0 % N
    @locks[bucket].synchronize do
      @sets[bucket] << atom
    end
  end

  def present?(atom : Atom)
    bucket = atom.@blk0 % N
    @locks[bucket].synchronize do
      @sets[bucket].includes?(atom)
    end
  end

  def present?(objects : Enumerable(T), & : T -> Atom) : BitList forall T
    answer = BitList.new

    objects.each do |object|
      answer << present?(yield object)
    end

    answer
  end
end

# require "benchmark"

# Benchmark.ips do |x|
#   x.report("gen") do

# WWID.next
#   end
# end
# require "benchmark"

alias BitList = DynamicBitArray

Log.setup_from_env(default_level: :debug)

set = MySet(1).new

p = Xgraph.mount(set, Deque{Atom.of("a"), Atom.of("b")})
q = Xgraph.mount(set, Deque{Atom.of("a"), Atom.of("b"), Atom.of("c")})
r = Xgraph.mount(set, Deque{Atom.of("b"), Atom.of("c")})
pp! p
pp! q
pp! r

Xgraph.each_conjv(set, Deque{Atom.of("a"), Atom.of("b"), Atom.of("c"), Atom.of("d")}) do |conjv|
  pp conjv
end

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


