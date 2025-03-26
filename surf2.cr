require "log"
require "simple_rpc"
require "digest/sha256"
require "json"
require "./src/wirewright"
require "./surf_common"

Log.setup_from_env(default_level: :debug)

record Label, value : UInt128 do
  include Comparable(Label)

  class_getter zero = Label.new(0u128)

  def <=>(other : Label)
    value <=> other.value
  end

  def encode(otype : Term.class) : Term
    Term.of(value)
  end

  def self.decode(value : Term)
    new(value.as_n.to(UInt128))
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

  # Inspired by: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235778_109848
  # Inspired by: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235789_109848
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
struct WWID
  extend ILabelGenerator

  def self.call : Label
    order = Time.utc.to_unix_ns.to_u128
    randomness = Random::Secure.rand(UInt64)

    Label.new((order << 64) | randomness)
  end
end

module IMap(K, V)
  # Returns the latest value of *key*, or `nil` if absent.
  #
  # "Latest" means "at the time of fetch". At the same time as the result of
  # the fetch (e.g. `nil`) is being returned to the caller, *key* might have
  # been added.
  abstract def latest?(referrer : Label, key : K) : V?

  # Returns an approximate number of entries in this map. Implementations
  # are allowed to return `0` unconditionally if they cannot determine
  # the size (e.g. in a distributed, decentralized setting).
  abstract def size : Int32

  # Atomically registers a reference of *referrer* to *key*. If *key* is absent,
  # creates it and sets its value to *default*. Returns the value read at
  # the time of assignment. This is an INCREF. Each call will increment the reference
  # count of *key* by *referrer*.
  abstract def inc(referrer : Label, key : K, default : V) : V

  # Atomically removes *referrer*'s reference to *key*, removing the underlying
  # key-value pair if necessary. This is a DECREF; i.e., if you called `inc`
  # N times you'd have to call `dec` N times.
  abstract def dec(referrer : Label, key : K) : Nil

  # Constructs a submap (see `SubMap`).
  def submap(k : Sk.class, v : Sv.class) forall Sk, Sv
    SubMap(K, V, Sk, Sv).new(self)
  end
end

# A view of an `IMap(Pk, Pv)` with a narrowed key-value type `(K, V)`,
# assuming `K` and `V` are safely castable to `Pk` and `Pv`, respectively.
#
# `SubMap` delegates all operations to the underlying map, performing
# type conversions as needed.
struct SubMap(Pk, Pv, K, V)
  include IMap(K, V)

  def initialize(@map : IMap(Pk, Pv))
  end

  def latest?(referrer : Label, key : K) : V?
    @map.latest?(referrer, key.as(Pk)).as?(V)
  end

  def size : Int32
    @map.size
  end

  def inc(referrer : Label, key : K, default : V) : V
    @map.inc(referrer, key.as(Pk), default.as(Pv)).as(V)
  end

  def dec(referrer : Label, key : K) : Nil
    @map.dec(referrer, key.as(Pk))
  end
end

# A synchronous in-memory implementation of `IMap(K, V)`. Uses a lock for
# thread-safety.
class SyncInMemoryMap(K, V)
  include IMap(K, V)

  record Cell(T), refs : Bag(Label), value : T

  @data = {} of K => Cell(V)
  @lock = Mutex.new

  def latest?(referrer : Label, key : K) : V?
    return unless cell = @lock.synchronize { @data[key]? }

    cell.value
  end

  def size : Int32
    @data.size
  end

  def inc(referrer : Label, key : K, default : V) : V
    @lock.synchronize do
      if cell = @data[key]?
        cell.refs.add(referrer)
        cell.value
      else
        @data[key] = Cell.new(Bag{referrer}, default)

        default
      end
    end
  end

  def dec(referrer : Label, key : K) : Nil
    @lock.synchronize do
      return unless cell = @data[key]?
      return unless cell.refs.delete?(referrer)
      return unless cell.refs.empty?

      @data.delete(key)
    end
  end

  def pretty_print(pp)
    @lock.synchronize do
      pp.list("{", @data, "}") do |key, cell|
        pp.group do
          key.pretty_print(pp)
          pp.text ": "
          pp.nest do
            pp.breakable
            cell.value.pretty_print(pp)
          end
        end
      end
    end
  end
end

# Raised by `TermMap` if it cannot decode a term into a `V` type. This means
# `V`'s `decode?` was not able to decode the term; or if `V` is a union, none
# of its members were able to `decode?` the term.
class TermDecodeError < Exception
end

# An `IMap(K, V)` backed by an `IMap(Term, Term)`, encoding keys and values
# as `Term`s.
#
# Used primarily as an intermediate step for (de)serialization (since `Term`s are
# the lingua franca of Wirewright).
#
# `TermMap` ensures that stored values can be decoded back into `V`, raising
# `TermDecodeError` if decoding fails.
class TermMap(K, V)
  include IMap(K, V)

  def initialize(@map : IMap(Term, Term))
  end

  private def decode(term : Term) : V
    {% for type in V.union_types %}
      if object = {{type}}.decode?(term)
        return object.as(V)
      end
    {% end %}

    raise TermDecodeError.new
  end

  def latest?(referrer : Label, key : K) : V?
    return unless value = @map.latest?(referrer, key.encode(Term))

    decode(value)
  end

  def size : Int32
    @map.size
  end

  def inc(referrer : Label, key : K, default : V) : V
    value = @map.inc(referrer, key.encode(Term), default.encode(Term))

    decode(value)
  end

  def dec(referrer : Label, key : K) : Nil
    @map.dec(referrer, key.encode(Term))
  end
end

# An `IMap(Term, Term)` backed by an `IMap(String, String)`, using CompactML for storage.
#
# CompactML is a human-readable, space-efficient subset of WwML, designed to
# represent `Term`s in a compact string format.
#
# `CompactMLMap` serializes terms into their CompactML string representation for
# efficient storage and retrieval, and deserializes them back to `Term`s on access.
class CompactMLMap
  include IMap(Term, Term)

  def initialize(@map : IMap(String, String))
  end

  def latest?(referrer : Label, key : Term) : Term?
    return unless value = @map.latest?(referrer, ML.compact(key))

    ML.term(value)
  end

  def size : Int32
    @map.size
  end

  def inc(referrer : Label, key : Term, default : Term) : Term
    value = @map.inc(referrer, ML.compact(key), ML.compact(default))

    ML.term(value)
  end

  def dec(referrer : Label, key : K) : Nil
    @map.dec(referrer, ML.compact(key))
  end
end

# An `IMap(K, V)` backed by an `IMap(String, V)`, storing keys as their digests.
#
# `DigestedKeyMap` hashes keys using the specified digest algorithm
# (default: SHA-256) and uses the resulting string as the storage key.
class DigestedKeyMap(K, V)
  include IMap(K, V)

  def initialize(@map : IMap(String, V), @algorithm : Digest::ClassMethods = Digest::SHA256)
  end

  private def digest(key : K) : String
    key.digest(@algorithm, base: 64)
  end

  def latest?(referrer : Label, key : K) : V?
    @map.latest?(referrer, digest(key))
  end

  def size : Int32
    @map.size
  end

  def inc(referrer : Label, key : K, default : V) : V
    @map.inc(referrer, digest(key), default)
  end

  def dec(referrer : Label, key : K) : Nil
    @map.dec(referrer, digest(key))
  end
end

# Represents a single, immutable map bucket.
class StringBucket
  # :nodoc:
  alias Refcount = UInt32

  # :nodoc:
  record Cell, refs : Pf::Map(Label, Refcount), value : String

  def initialize
    initialize(entries: Pf::Map(String, Cell).new, seen: Pf::Map(Label, Time).new)
  end

  # :nodoc:
  def initialize(@entries : Pf::Map(String, Cell), @seen : Pf::Map(Label, Time))
  end

  private def_change

  protected def renew(referrer : Label) : StringBucket
    change(seen: @seen.assoc(referrer, Time.utc))
  end

  # Returns the amount of entries in the bucket.
  #
  # Complexity: O(1).
  def size : Int32
    @entries.size
  end

  # Returns the latest value of *key* in the bucket, or `nil` if *key* is absent.
  # Records the activity of *referrer* (thus delaying its decay).
  #
  # Rough complexity: O(1).
  def latest?(referrer : Label, key : String) : {StringBucket, String?}
    {renew(referrer), @entries[key]?.try(&.value)}
  end

  protected def inc0(referrer : Label, key : String, default : String) : {StringBucket, String}
    if cell0 = @entries[key]?
      refs0 = cell0.refs
      refs1 = refs0.assoc(referrer, (refs0[referrer]? || Refcount.new(0)) + 1)
      cell1 = cell0.copy_with(refs: refs1)
    else
      cell1 = Cell.new(Pf::Map.assoc(referrer, Refcount.new(1)), default)
    end

    {change(entries: @entries.assoc(key, cell1)), cell1.value}
  end

  # Increments *referrer*'s reference count for *key*. If *key* is absent, a new entry
  # is created and assigned the given *default* value. Returns a tuple containing
  # the updated bucket and the current value of *key*.
  #
  # Records the activity of *referrer* (thus delaying its decay).
  #
  # Rough complexity: O(1).
  def inc(referrer : Label, key : String, default : String) : {StringBucket, String}
    renew(referrer).inc0(referrer, key, default)
  end

  protected def dec0(referrer : Label, key : String) : StringBucket
    return self unless cell0 = @entries[key]?

    refs0 = cell0.refs

    return self unless refcount = refs0[referrer]?

    if refcount == 1
      refs1 = refs0.dissoc(referrer)
    else
      refs1 = refs0.assoc(referrer, refcount - 1)
    end

    if refs1.empty?
      return change(entries: @entries.dissoc(key))
    end

    cell1 = cell0.copy_with(refs: refs1)

    change(entries: @entries.assoc(key, cell1))
  end

  # Decrements *referrer*'s reference count for *key*. Returns the updated bucket.
  #
  # Records the activity of *referrer* (thus delaying its decay).
  #
  # Rough complexity: O(1).
  def dec(referrer : Label, key : String) : StringBucket
    renew(referrer).dec0(referrer, key)
  end

  # Decays the bucket by removing inactive referrers and their associated entries.
  #
  # A referrer is considered inactive if the time since its last recorded activity
  # exceeds the specified *lifespan*.
  #
  # Returns the updated bucket.
  #
  # Rough complexity: O(S + E*R), where S - number of referrers in the seen map,
  # E - number of entries in the bucket, R - average number of referrers per entry.
  def decay(*, lifespan = 30.seconds, now = Time.utc) : StringBucket
    bucket = self
    expired = Set(Label).new

    seen1 = @seen

    @seen.each do |referrer, accessed|
      next unless now - accessed >= lifespan

      seen1 = seen1.dissoc(referrer)
      expired << referrer
    end

    entries1 = @entries

    @entries.each do |key, cell0|
      refs0 = cell0.refs
      refs1 = refs0.reject { |referrer, _| referrer.in?(expired) }
      next if refs0.same?(refs1)

      if refs1.empty?
        entries1 = entries1.dissoc(key)
      else
        cell1 = cell0.copy_with(refs: refs1)
        entries1 = entries1.assoc(key, cell1)
      end
    end

    change(entries: entries1, seen: seen1)
  end
end

# Thread-safe, mutable wrapper around `StringBucket` (powered by Atomics).
class ConcurrentStringBucket
  include IMap(String, String)

  def initialize
    @bucket = Atomic(StringBucket).new(StringBucket.new)
    @state = Atomic(Int32).new(0)
  end

  def self.spawn(ctx : ExecutionContext, *, lifespan = 30.seconds, running = Channel(Bool).new) : {ConcurrentStringBucket, Channel(Bool)}
    bucket = new

    ctx.spawn do
      while true
        select
        when running.receive? # nil
          break
        when timeout(lifespan)
          bucket.decay(lifespan: lifespan)
        end
      end
    end

    {bucket, running}
  end

  # Makes sure `decay` doesn't run during the block.
  private def nodecay(&)
    state0 = @state.get(:acquire)

    # If state is -1, busy-wait until it is not, and increment.
    while true
      if state0 == -1
        state0 = @state.get(:acquire)

        Intrinsics.pause

        next
      end

      state1 = state0 + 1
      state0, ok = @state.compare_and_set(state0, state1, :release, :acquire)
      break if ok

      Intrinsics.pause
    end

    begin
      yield
    ensure
      @state.sub(1, :release)
    end
  end

  # Locks `@bucket` for decay during the block.
  private def decay(&)
    state0 = @state.get(:acquire)
    if state0 == -1
      raise "invalid state: expected just one decay fiber"
    end

    # If state is nonzero, busy-wait until it zero, and decrement.
    while true
      _, ok = @state.compare_and_set(0, -1, :release, :acquire)
      break if ok

      Intrinsics.pause
    end

    begin
      yield
    ensure
      @state.add(1, :release)
    end
  end

  private def assign(& : StringBucket -> T) forall T
    nodecay do
      bucket0 = @bucket.get(:acquire)

      while true
        bucket1 = bucket0
        result = nil

        {% if T.has_method?(:[]) %}
          bucket1, result = yield bucket0
        {% else %}
          bucket1 = yield bucket0
        {% end %}

        bucket0, ok = @bucket.compare_and_set(bucket0, bucket1, :release, :acquire)

        return result if ok

        Intrinsics.pause
      end
    end
  end

  def size : Int32
    bucket = @bucket.get(:acquire)
    bucket.size
  end

  def latest?(referrer : Label, key : String) : String?
    assign &.latest?(referrer, key)
  end

  def inc(referrer : Label, key : String, default : String) : String
    assign &.inc(referrer, key, default)
  end

  def dec(referrer : Label, key : String) : Nil
    assign &.dec(referrer, key)
  end

  def decay(*, lifespan = 30.seconds, now = Time.utc) : Nil
    decay do
      bucket0 = @bucket.get(:acquire)

      while true
        bucket1 = bucket0.decay(lifespan: lifespan, now: now)
        bucket0, ok = @bucket.compare_and_set(bucket0, bucket1, :release, :acquire)
        return if ok
      end
    end
  end
end

# A concurrent `IMap(String, String)` with entry storage distributed across `N` buckets.
#
# `ConcurrentStringMap` partitions keys across `N` independent `ConcurrentStringBucket`s
# based on their hash. Each bucket is assigned a fiber that performs decay for that bucket
# (see also: `ConcurrentStringBucket#decay`). The sleep times and referrer lifespans are
# randomized for each bucket.
class ConcurrentStringMap(N)
  include IMap(String, String)

  def initialize(ctx : ExecutionContext, min_lifespan = 30.seconds, max_lifespan = 1.minute)
    min_lifespan_ms = min_lifespan.total_milliseconds
    max_lifespan_ms = max_lifespan.total_milliseconds

    @running = Channel(Bool).new
    @buckets = StaticArray(ConcurrentStringBucket, N).new do
      lifespan = (min_lifespan_ms..max_lifespan_ms).sample.milliseconds
      bucket, _ = ConcurrentStringBucket.spawn(ctx, lifespan: lifespan, running: @running)
      bucket
    end
  end

  # Stops all decay fibers. Can only be called once.
  def nodecay : Nil
    @running.close
  end

  def size : Int32
    @buckets.sum(&.size)
  end

  def latest?(referrer : Label, key : String) : String?
    bucket = @buckets[key.hash % N]
    bucket.latest?(referrer, key)
  end

  def inc(referrer : Label, key : String, default : String) : String
    bucket = @buckets[key.hash % N]
    bucket.inc(referrer, key, default)
  end

  def dec(referrer : Label, key : String) : Nil
    bucket = @buckets[key.hash % N]
    bucket.dec(referrer, key)
  end
end

struct StringMapRPC
  include SimpleRpc::Proto

  @@ctx = ExecutionContext::MultiThreaded.new("bucket decay", System.cpu_count.to_i)
  @@map = ConcurrentStringMap(128).new(@@ctx)

  def latest(referrer : String, key : String) : String?
    @@map.latest?(Label.new(referrer.to_u128), key)
  end

  def size : Int32
    @@map.size
  end

  def inc(referrer : String, key : String, default : String) : String
    @@map.inc(Label.new(referrer.to_u128), key, default)
  end

  def dec(referrer : String, key : String) : Nil
    @@map.dec(Label.new(referrer.to_u128), key)
  end
end

# A remote `IMap(String, String)` client using RPC for communication.
#
# `RemoteStringMap` connects to a remote string map service via `StringMapRPC::Client`.
class RemoteStringMap
  include IMap(String, String)

  def initialize(host : String, port : Int32, *, pool_size = 50, pool_timeout = 1)
    @client = StringMapRPC::Client.new(host, port, mode: :pool, pool_size: pool_size, pool_timeout: pool_timeout)
  end

  def latest?(referrer : Label, key : String) : String?
    @client.latest!(referrer.value.to_s, key)
  end

  def size : Int32
    @client.size!
  end

  def inc(referrer : Label, key : String, default : String) : String
    @client.inc!(referrer.value.to_s, key, default)
  end

  def dec(referrer : Label, key : String) : Nil
    @client.dec!(referrer.value.to_s, key)
  end
end

if ARGV[0]? == "serve"
  port = (ARGV[1]? || 9000).to_i
  puts "Server listen on #{port} port"
  StringMapRPC::Server.new("127.0.0.1", port).run
end

struct Utrie
  alias Key = Origin | Step

  record Origin, base : Ubase::Any do
    def encode(otype : Term.class) : Term
      Term.of("utrie origin key", base.term)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("utrie origin key" base_)}) do
        new(Ubase.parse(base))
      end
    end
  end

  record Step, pred : Label, base : Ubase::Any do
    def encode(otype : Term.class) : Term
      Term.of("utrie step key", pred.encode(Term), base.term)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("utrie step key" pred_number base_)}) do
        new(Label.decode(pred), Ubase.parse(base))
      end
    end
  end

  record Value, succ : Label do
    def encode(otype : Term.class) : Term
      Term.of("utrie value", succ.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("utrie value" succ_number)}) do
        new(Label.decode(succ))
      end
    end
  end

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  # Mounts a *strand* of `Ubase`s for *referrer*. Returns a set of seen pairs
  # (called *dependencies*; used for removal or maintenance) and the id of
  # the endpoint thus reached.
  def mount(referrer : Label, strand : Strand, *, deps = Bag({Key, Value}).new)
    key = Origin.new(strand[0])
    origin = @map.inc(referrer, key, Value.new(@fresh.call))
    pred = origin.succ
    deps << {key, origin}

    strand[1..].each do |base|
      key = Step.new(pred, base)
      step = @map.inc(referrer, key, Value.new(@fresh.call))
      pred = step.succ
      deps << {key, step}
    end

    {deps, pred}
  end

  private def successor?(referrer : Label, key : Key) : Label?
    @map.latest?(referrer, key).try(&.succ)
  end

  {% for type, base in {Term::Num => Ubase::IsNum, Term::Str => Ubase::IsStr, Term::Sym => Ubase::IsSym, Term::Boolean => Ubase::IsBool} %}
    private def query(referrer : Label, pred : Label, term : {{type}}, sink : Label ->) : Nil
      return unless succ0 = successor?(referrer, Step.new(pred, {{base}}.new))

      sink.call(succ0)

      if succ1 = successor?(referrer, Step.new(succ0, Ubase::Literal.new(Term.of(term))))
        sink.call(succ1)
      end
    end
  {% end %}

  # NOTE: dictionaries must be normalized into IsDict - At(), even literal ones.
  # We do not handle Literal(dict).
  private def query(referrer : Label, pred : Label, term : Term::Dict, sink : Label ->) : Nil
    return unless succ0 = successor?(referrer, Step.new(pred, Ubase::IsDict.new))

    sink.call(succ0)

    term.each_entry do |key, value|
      next unless succ1 = successor?(referrer, Step.new(succ0, Ubase::At.new(key)))

      sink.call(succ1)

      query(referrer, succ1, value.downcast, sink)
    end
  end

  private def query(referrer : Label, term : Term, sink) : Nil
    return unless succ = successor?(referrer, Origin.new(Ubase::IsAny.new))

    sink.call(succ)

    query(referrer, succ, term.downcast, sink)
  end

  # Calls *sink* with all ids activated by *term*.
  #
  # Due to the nature of the underlying map, key-value pairs may randomly disappear,
  # thus blocking the passage for activations by *term*. This means parts of Utrie
  # may be temporarily unreachable during `query`, depending on the health of the map.
  # In other words, depending on the health of the underlying map, *sink* may be called
  # with extra (outdated) labels, or with too few labels (parts of the trie unreachable).
  # If the Origin of the trie degenerates, until it is restored, the entire trie will
  # be unreachable. This cannot be fixed on the "higher-order data structure" level.
  # Introducing replication at the underlying map level should help in practice, however.
  # Instead of storing trie Origin on one node, store it on three, or ten; so there's
  # always someone to fall back on instead of immediate absence report.
  def query(referrer : Label, term : Term, &sink : Label ->) : Nil
    query(referrer, term, sink)
  end
end

struct Xgraph
  record Key, a : Label, b : Label do
    def encode(otype : Term.class) : Term
      Term.of("xgraph key", a.encode(Term), b.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("xgraph key" a_number b_number)}) do
        new(Label.decode(a), Label.decode(b))
      end
    end
  end

  record Value, succ : Label do
    def encode(otype : Term.class) : Term
      Term.of("xgraph value", succ.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("xgraph value" succ_number)}) do
        new(Label.decode(succ))
      end
    end
  end

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def mount(referrer : Label, xrule : Deque(Label), *, deps = Bag({Key, Value}).new)
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      key = Key.new(a, b)
      value = @map.inc(referrer, key, Value.new(@fresh.call))
      deps << {key, value}

      xrule << value.succ
    end

    {deps, xrule[0]}
  end

  private def conjs(referrer : Label, vertices : Deque(Label), sink : Label ->)
    while a = vertices.shift?
      sink.call(a)

      (0...vertices.size).each do |i|
        b = vertices.unsafe_fetch(i)
        next unless value = @map.latest?(referrer, Key.new(a, b))

        vertices << value.succ
      end
    end
  end

  # Calls *sink* with all mounted conjunction vertices in *vertices*.
  #
  # NOTE: *vertices* must be pre-sorted ascending. You lose ownership of *vertices*
  # by passing it to this method.
  #
  # Similarly to `Utrie#query`, this method may call *sink* with partial or even
  # no results even if the underlying map contains them in some form, due to map
  # degeneration. See `Utrie#query` to learn more.
  def conjs(referrer : Label, vertices : Deque(Label), &sink : Label ->)
    conjs(referrer, vertices, sink)
  end
end

struct Ttrie
  alias Key = Origin | Step

  record Origin do
    def encode(otype : Term.class) : Term
      Term.of({"ttrie origin key"})
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("ttrie origin key")}) do
        new
      end
    end
  end

  record Step, pred : Label, base : Ubase::Any do
    def encode(otype : Term.class) : Term
      Term.of("ttrie step key", pred.encode(Term), base.term)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("ttrie step key" pred_number base_)}) do
        new(Label.decode(pred), Ubase.parse(base))
      end
    end
  end

  record Value, succ : Label do
    def encode(otype : Term.class) : Term
      Term.of("ttrie value", succ.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("ttrie value" succ_number)}) do
        new(Label.decode(succ))
      end
    end
  end

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def mount(referrer : Label, strand : Enumerable(Term), endpoint : Label, *, deps = Bag({Key, Value}).new)
    tip = nil
    path = [] of Label

    mount = ->(key : Key) do
      value = @map.inc(referrer, key, Value.new(@fresh.call))
      deps << {key, value}
      path << value.succ
    end

    mount.call(Origin.new)
    mount.call(Step.new(path.last, Ubase::IsAny.new))

    strand.each do |term|
      if tip
        mount.call(Step.new(path.last, Ubase::IsDict.new))
        mount.call(Step.new(path.last, Ubase::At.new(tip)))
      end

      tip = term
    end

    if tip
      mount.call(Step.new(path.last, Ubase.from(tip.type)))
      mount.call(Step.new(path.last, Ubase::Literal.new(tip)))
    end

    path << endpoint

    {deps, path}
  end

  def query?(referrer : Label, strand : Enumerable(Ubase::Any)) : Label?
    return unless origin = @map.latest?(referrer, Origin.new)

    pred = origin.succ

    strand.each do |base|
      # The strand embedded in @map must be >= the query strand.
      return unless value = @map.latest?(referrer, Step.new(pred, base))

      pred = value.succ
    end

    pred
  end
end

struct Etrace
  # :nodoc:
  #
  # The choice of base is very important for Etrace; it determines the fundamental
  # tradeoff between how much we are willing to store vs. compute. The larger the base,
  # the less digits we'll have to store; the more guesses we'll have to make for each
  # digit, and thus, the more lookups (computation). Both base 3 and 4 seem to be good
  # tradeoff points. Base 2 requires too much storage (remember we're using u128 for ids,
  # so 128 entries per id) but very little guessing per digit (you're basically asking
  # the network, "yes or no?") The benefit of larger bases diminishes quickly while
  # increasing guesswork.
  BASE = 4u128

  # :nodoc:
  BASE_DIGITS = {0u128, 1u128, 2u128, 3u128}

  # :nodoc:
  BASE_LENGTH_U128 = 64u8

  record Key, scope : Label, state : Label, digitno : UInt8 do
    def encode(otype : Term.class) : Term
      Term.of("etrace key", scope.encode(Term), state.encode(Term), digitno)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("etrace key" scope_number state_number digitno_number)}) do
        new(Label.decode(scope), Label.decode(state), digitno.to(UInt8))
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of({"etrace value"})
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("etrace value")}) { new }
    end
  end

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def mount(referrer : Label, scope : Label, succ : Label, *, deps = Bag({Key, Value}).new)
    succ.each_prefix_with_index(base: BASE, max: BASE_LENGTH_U128) do |prefix, index|
      key = Key.new(scope, prefix, index)
      value = @map.inc(referrer, key, Value.new)
      deps << {key, value}
    end

    deps
  end

  def mount(referrer : Label, path : Array(Label), *, deps = Bag({Key, Value}).new)
    path.each_cons_pair do |u, v|
      _ = mount(referrer, u, v, deps: deps)
    end

    deps
  end

  # TODO: what this method is doing appears to be "embarassingly parallel". Parallelize!
  def each_successor(referrer : Label, scope : Label, &sink : Label ->) : Nil
    queue = Deque{ {Label.zero, BASE_LENGTH_U128 - 1} }

    while entry = queue.shift?
      state, digitno = entry

      BASE_DIGITS.each do |choice|
        completion = state.complete(choice, base: BASE, index: digitno)

        next unless @map.latest?(referrer, Key.new(scope, completion, digitno))

        if digitno == 0
          sink.call(completion)
        else
          queue << {completion, digitno - 1}
        end
      end
    end
  end

  def walk(referrer : Label, origin : Label, &sink : Label ->) : Nil
    sink.call(origin)

    each_successor(referrer, origin) do |successor|
      walk(referrer, successor, &sink)
    end
  end
end

struct StrandSet
  record Key, vertex : Label do
    def encode(otype : Term.class) : Term
      Term.of("strand set key", vertex.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("strand set key" vertex_number)}) do
        new(Label.decode(vertex))
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of({"strand set value"})
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("strand set value")}) { new }
    end
  end

  def initialize(@map : IMap(Key, Value))
  end

  def mount(referrer : Label, vertex : Label, *, deps = Bag({Key, Value}).new)
    key = Key.new(vertex)
    value = @map.inc(referrer, key, Value.new)
    deps << {key, value}
    deps
  end

  def strand?(referrer : Label, vertex : Label) : Bool
    !!@map.latest?(referrer, Key.new(vertex))
  end
end

struct AppearanceSet
  record Key, vertex : Label do
    def encode(otype : Term.class) : Term
      Term.of("appearance set key", vertex.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("appearance set key" vertex_number)}) do
        new(Label.decode(vertex))
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of({"appearance set value"})
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("appearance set value")}) { new }
    end
  end

  def initialize(@map : IMap(Key, Value))
  end

  def mount(referrer : Label, vertex : Label, *, deps = Bag({Key, Value}).new)
    key = Key.new(vertex)
    value = @map.inc(referrer, key, Value.new)
    deps << {key, value}
    deps
  end

  def appearance?(referrer : Label, vertex : Label) : Bool
    !!@map.latest?(referrer, Key.new(vertex))
  end
end

struct SensorDecoder
  # :nodoc:
  BASE = 4u128

  # :nodoc:
  BASE_DIGITS = {0u128, 1u128, 2u128, 3u128}

  # :nodoc:
  BASE_LENGTH_U128 = 64u8

  record Key, scope : Label, state : Label, digitno : UInt8 do
    def encode(otype : Term.class) : Term
      Term.of("sensor decoder key", scope.encode(Term), state.encode(Term), digitno)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("sensor decoder key" scope_number state_number digitno_number)}) do
        new(Label.decode(scope), Label.decode(state), digitno.to(UInt8))
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of({"sensor decoder value"})
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("sensor decoder value")}) { new }
    end
  end

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def mount(referrer : Label, scope : Label, succ : Label, *, deps = Bag({Key, Value}).new)
    succ.each_prefix_with_index(base: BASE, max: BASE_LENGTH_U128) do |prefix, index|
      key = Key.new(scope, prefix, index)
      value = @map.inc(referrer, key, Value.new)
      deps << {key, value}
    end

    deps
  end

  # TODO: what this method is doing appears to be "embarassingly parallel". Parallelize!
  def each_successor(referrer : Label, scope : Label, &sink : Label ->) : Nil
    queue = Deque{ {Label.zero, BASE_LENGTH_U128 - 1} }

    while entry = queue.shift?
      state, digitno = entry

      BASE_DIGITS.each do |choice|
        completion = state.complete(choice, base: BASE, index: digitno)

        next unless @map.latest?(referrer, Key.new(scope, completion, digitno))

        if digitno == 0
          sink.call(completion)
        else
          queue << {completion, digitno - 1}
        end
      end
    end
  end

  def decode(referrer : Label, sensor : Label, &sink : Label ->)
    each_successor(referrer, sensor, &sink)
  end
end

class SurfaceNotMountedError < Exception
end

struct Tbase
  alias Key = Utrie::Key | Xgraph::Key | Ttrie::Key | Etrace::Key | StrandSet::Key | AppearanceSet::Key | SensorDecoder::Key
  alias Value = Utrie::Value | Xgraph::Value | Ttrie::Value | Etrace::Value | StrandSet::Value | AppearanceSet::Value | SensorDecoder::Value

  # Sensor surface requirements of `Tbase`.
  module Sensor
    abstract def id : Label
    abstract def strands : StrandList
  end

  # Appearance surface requirements of `Tbase`.
  module Appearance
    abstract def id : Label

    # Returns the value of this appearance.
    abstract def value : Term
  end

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def utrie : Utrie
    Utrie.new(@fresh, @map.submap(Utrie::Key, Utrie::Value))
  end

  def ttrie : Ttrie
    Ttrie.new(@fresh, @map.submap(Ttrie::Key, Ttrie::Value))
  end

  def etrace : Etrace
    Etrace.new(@fresh, @map.submap(Etrace::Key, Etrace::Value))
  end

  def strands : StrandSet
    StrandSet.new(@map.submap(StrandSet::Key, StrandSet::Value))
  end

  def xgraph : Xgraph
    Xgraph.new(@fresh, @map.submap(Xgraph::Key, Xgraph::Value))
  end

  def sensors : SensorDecoder
    SensorDecoder.new(@fresh, @map.submap(SensorDecoder::Key, SensorDecoder::Value))
  end

  def appearances : AppearanceSet
    AppearanceSet.new(@map.submap(AppearanceSet::Key, AppearanceSet::Value))
  end

  def mount(referrer : Label, subject : Sensor, *, deps = Bag({Key, Value}).new)
    rule = Deque(Label).new

    subject.strands.each do |strand|
      _, endpoint = utrie.mount(referrer, strand, deps: deps)

      # Endpoint points to the end of the utrie strand. We need to register
      # endpoint as a strand.
      _ = strands.mount(referrer, endpoint, deps: deps)

      rule << endpoint
    end

    # Pre-sort ascending as the Xgraph requires.
    rule.unstable_sort!

    # Mount the rule in the Xgraph.
    _, conjv = xgraph.mount(referrer, rule, deps: deps)

    # Subscribe the id to the conjunction vertex. This acts as a point-of-commitment,
    # the instant registration finishes the sensor is public.
    _ = sensors.mount(referrer, conjv, subject.id, deps: deps)

    deps
  end

  def mount(referrer : Label, subject : Appearance, *, deps = Bag({Key, Value}).new)
    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)

      # Mount the appearance into the Ttrie, saving the path to `subject.id`
      # (ids of nodes which lie along the path).
      _, path = ttrie.mount(referrer, keypath, subject.id, deps: deps)

      keypath.pop

      # Mount `path` in Etrace for iteration at query-time.
      _ = etrace.mount(referrer, path, deps: deps)

      true # continue
    end

    # Register `subject.id` as that of an appearance. This acts as a point-of-
    # commitment, the instant registration finishes the sensor is public.
    _ = appearances.mount(referrer, subject.id, deps: deps)

    deps
  end

  def each_complement(client : Label, subject : Sensor, *, successors : Bool, &sink : Label ->) : Nil
    sets = [] of Set(Label)

    subject.strands.each do |strand|
      return unless endpoint = ttrie.query?(client, strand)

      hits = Set(Label).new

      etrace.walk(client, endpoint) do |candidate|
        next unless appearances.appearance?(client, candidate)
        next if !successors && candidate > subject.id

        hits << candidate
      end

      return if hits.empty?

      sets << hits
    end

    return unless subject.strands.size == sets.size

    sets.unstable_sort_by!(&.size)
    sets[0].each do |candidate|
      next unless (1...sets.size).all? { |index| candidate.in?(sets[index]) }

      # Send candidates that are in all sets (match all strands of the sensor)
      # to the sink.
      sink.call(candidate)
    end
  end

  def each_complement(client : Label, subject : Appearance, *, successors : Bool, &sink : Label ->) : Nil
    hits = Deque(Label).new

    # Find out which Utrie vertices are activated by the subject.
    utrie.query(client, subject.value) do |hit|
      # Keep only strand vertices.
      next unless strands.strand?(client, hit)

      hits << hit
    end

    # Pre-sort ascending as the Xgraph requires.
    hits.unstable_sort!

    # Find out which conjunctions are activated by the subject.
    xgraph.conjs(client, hits) do |conjv|
      # Filter proper (decodable) sensor vertices.
      sensors.decode(client, conjv) do |candidate|
        next if !successors && candidate > subject.id

        # Send matching candidates to sink.
        sink.call(candidate)
      end
    end
  end
end

record SensorSubject, id : Label, strands : StrandList do
  include Tbase::Sensor

  # Calls *fn* with each sensor in *pattern*.
  #
  # An arbitrary M1 *pattern* can contain branches (e.g. `%any`) so it is considered
  # to contain multiple sensors.
  def self.each(fresh : LabelGenerator, pattern : Term, &fn : SensorSubject ->) : Nil
    skeleton = pipe(pattern, M1.normal, M1.skeleton)

    strands = [] of Strand

    M1.branches(skeleton) do |branch|
      M1.strands(branch) do |strand|
        strands << strand.items.to_readonly_slice { |base| Ubase.parse(base) }
      end

      sensor = new(fresh.call, strands.to_readonly_slice(&.itself))

      fn.call(sensor)

      strands.clear
    end
  end
end

record AppearanceSubject, id : Label, value : Term do
  include Tbase::Appearance
end

record SensorData,
  client : Label,
  instant : Label,
  identity : Identity,
  address : String,
  selector : Term?

struct SensorData
  def encode(otype : Term.class) : Term
    Term.of(
      client: client.encode(Term),
      instant: instant.encode(Term),
      identity: identity,
      address: address,
      selector: selector,
    )
  end

  def self.decode(object : Term) : SensorData
    new(
      Label.decode(object[:client]),
      Label.decode(object[:instant]),
      object[:identity].to(Identity),
      object[:address].to(String),
      object[:selector]?,
    )
  end
end

record AppearanceData,
  client : Label,
  instant : Label,
  identity : Identity,
  value : Term,
  selector : Term?,
  tombstone : Term?

struct AppearanceData
  def encode(otype : Term.class) : Term
    Term.of(
      client: client.encode(Term),
      instant: instant.encode(Term),
      identity: identity,
      value: value,
      selector: selector,
      tombstone: tombstone,
    )
  end

  def self.decode(object : Term) : AppearanceData
    new(
      Label.decode(object[:client]),
      Label.decode(object[:instant]),
      object[:identity].to(Identity),
      object[:value],
      object[:selector]?,
      object[:tombstone]?,
    )
  end
end

struct SensorBase
  record Key, instant : Label do
    def encode(otype : Term.class) : Term
      Term.of("sensor base key", instant.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("sensor base key" instant_number)}) do
        new(Label.decode(scope))
      end
    end
  end

  record Value, data : SensorData do
    def encode(otype : Term.class) : Term
      Term.of("sensor base value", data.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("sensor base value" data_)}) do
        new(SensorData.decode(data))
      end
    end
  end

  def initialize(@map : IMap(Key, Value))
  end

  def mount(referrer : Label, instant : Label, data : SensorData, *, deps = Bag({Key, Value}).new)
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @map.inc(referrer, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps << {key, value0}
    deps
  end

  def query?(referrer : Label, instant : Label) : SensorData?
    return unless value = @map.latest?(referrer, Key.new(instant))

    value.data
  end
end

struct AppearanceBase
  record Key, instant : Label do
    def encode(otype : Term.class) : Term
      Term.of("appearance base key", instant.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{("appearance base key" instant_number)}) do
        new(Label.decode(instant))
      end
    end
  end

  record Value, data : AppearanceData do
    def encode(otype : Term.class) : Term
      Term.of("appearance base value", data.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{("appearance base value" data_)}) do
        new(AppearanceData.decode(data))
      end
    end
  end

  def initialize(@map : IMap(Key, Value))
  end

  def mount(referrer : Label, instant : Label, data : AppearanceData, *, deps = Bag({Key, Value}).new)
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @map.inc(referrer, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps << {key, value0}
    deps
  end

  def query?(referrer : Label, instant : Label) : AppearanceData?
    return unless value = @map.latest?(referrer, Key.new(instant))

    value.data
  end
end

alias Identity = UInt32
alias Activation = StimulusPresence | StimulusAbsence

record StimulusPresence, recv_client : Label, recv_instant : Label, instant : Label, identity : Identity, value : Term
record StimulusAbsence, instant : Label, client : Label, identity : Identity, tombstone : Term?

struct Tspace
  alias Address = String

  alias Key = Tbase::Key | SensorBase::Key | AppearanceBase::Key
  alias Value = Tbase::Value | SensorBase::Value | AppearanceBase::Value

  alias Dismiss = ->

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def tbase : Tbase
    Tbase.new(@fresh, @map.submap(Tbase::Key, Tbase::Value))
  end

  def sensors : SensorBase
    SensorBase.new(@map.submap(SensorBase::Key, SensorBase::Value))
  end

  def appearances : AppearanceBase
    AppearanceBase.new(@map.submap(AppearanceBase::Key, AppearanceBase::Value))
  end

  def summon(
    client : Label,
    subject : SensorSubject,
    identity : Identity,
    address : Address,
    selector : Term? = nil,
    &sink : AppearanceData ->
  ) : Dismiss
    sdata = SensorData.new(client, subject.id, identity, address, selector)
    deps = Bag({Key, Value}).new

    _ = sensors.mount(client, subject.id, sdata, deps: deps)
    _ = tbase.mount(client, subject, deps: deps)

    tbase.each_complement(client, subject, successors: false) do |appearance|
      next unless adata = appearances.query?(client, appearance)
      next unless sdata.selector == adata.selector

      sink.call(adata)
    end

    mounted = true

    Dismiss.new do
      unless mounted
        raise SurfaceNotMountedError.new
      end

      mounted = false

      deps.each { |(key, _)| @map.dec(client, key) }
    end
  end

  def summon(
    client : Label,
    subject : AppearanceSubject,
    identity : Identity,
    selector : Term? = nil,
    tombstone : Term? = nil,
    &sink : Address, Label, AppearanceData ->
  ) : ->
    adata = AppearanceData.new(client, subject.id, identity, subject.value, selector, tombstone)
    deps = Bag({Key, Value}).new

    _ = appearances.mount(client, subject.id, adata, deps: deps)
    _ = tbase.mount(client, subject, deps: deps)

    tbase.each_complement(client, subject, successors: false) do |sensor|
      next unless sdata = sensors.query?(client, sensor)
      next unless sdata.selector == adata.selector

      sink.call(sdata.address, sdata.instant, adata)
    end

    mounted = true

    Dismiss.new do
      unless mounted
        raise SurfaceNotMountedError.new
      end

      mounted = false

      deps.each { |(key, _)| @map.dec(client, key) }
    end
  end
end

# Tconn maintains the illusion of persistence of sensors and appearances
# throughout periodic keepalive reinsertion.
class Tconn
  Log = ::Log.for("Tconn")

  record Sensor, pattern : Term, selector : Term? = nil do
    def self.new(pattern : String, **kwargs) : Sensor
      new(ML.term(pattern), **kwargs)
    end
  end

  record Appearance, value : Term, selector : Term? = nil, tombstone : Term? = nil do
    def self.new(value : String, **kwargs) : Appearance
      new(ML.term(value), **kwargs)
    end
  end

  record SurfaceData, destructors : Array(Tspace::Dismiss), version : UInt32

  record Task, start : Time, fn : -> do
    def ready?(now : Time) : Bool
      now >= start
    end

    def execute : Nil
      fn.call
    end
  end

  @client : Label

  def initialize(
    @fresh : LabelGenerator,
    @map : IMap(Tspace::Key, Tspace::Value),
    @sink : Activation ->,
    *,
    scheduler_rate = 1.second,
    @reinsert_min = 10.seconds,
    @reinsert_max = 1.minute,
  )
    @client = @fresh.call
    @address = "address of #{@client}" # FIXME: ?!

    @surfaces = {} of Identity => SurfaceData
    @surfaces_lock = Mutex.new

    ctx = ExecutionContext::MultiThreaded.new("Tconn refresher", 1)

    @tasks = [] of Task
    @tasks_lock = Mutex.new

    @running = Channel(Bool).new

    ctx.spawn do
      taskq = Deque(Task).new

      while true
        select
        when @running.receive? # nil
          break
        when timeout(scheduler_rate)
          now = Time.utc

          @tasks_lock.synchronize do
            # Copy ready tasks outside of the lock. Leave nonready tasks.
            @tasks.reject! do |task|
              if ready = task.ready?(now)
                taskq << task
              end

              ready
            end
          end

          while task = taskq.shift?
            task.execute
          end
        end
      end
    end
  end

  def self.open(*args, **kwargs, &)
    conn = new(*args, **kwargs)

    begin
      yield conn
    ensure
      conn.close
    end
  end

  def tspace : Tspace
    Tspace.new(@fresh, @map)
  end

  def close
    # Terminate reinsert fiber.
    @running.close

    # Call destructors for all surfaces -- gracefully leave the termspace.
    @surfaces_lock.synchronize do
      @surfaces.each do |_, data|
        data.destructors.each(&.call)
      end
    end
  end

  alias AfterRec = AfterRec ->

  private def after(span : Time::Span, &fn : AfterRec ->)
    task = Task.new(start: Time.utc + span, fn: ->{ fn.call(fn) })

    @tasks_lock.synchronize { @tasks << task }
  end

  def []=(identity : Identity, surface : Sensor) : Nil
    version = 0u32

    sink = ->(adata : AppearanceData) do
      Log.debug { "#{@client}: update view of #{surface} at #{identity} based on #{adata}!!" }
    end

    @surfaces_lock.synchronize do
      if data = @surfaces[identity]?
        data.destructors.each(&.call)
        data.destructors.clear
        data = data.copy_with(version: data.version + 1)
      else
        data = SurfaceData.new(destructors: [] of Tspace::Dismiss, version: 0u32)
      end

      # Save current version in the closure.
      version = data.version

      # Perform initial insert.
      SensorSubject.each(@fresh, surface.pattern) do |subject|
        destructor = tspace.summon(@client, subject, identity, @address, surface.selector, &sink)

        data.destructors << destructor
      end

      @surfaces[identity] = data
    end

    Log.info { "#{@client}: inserted sensor #{surface} at #{identity}" }

    reinsert_min_ms = @reinsert_min.total_milliseconds
    reinsert_max_ms = @reinsert_max.total_milliseconds

    after((reinsert_min_ms...reinsert_max_ms).sample.milliseconds) do |rec|
      @surfaces_lock.lock

      # First of all we should check whether our local state of identity
      # is consistent with the current state of identity.
      unless data = @surfaces[identity]?
        @surfaces_lock.unlock
        next
      end

      # If version numbers do not match we simply quit. Identity was modified
      # and another reinsert fiber is now responsible for reinsertion.
      unless version == data.version
        @surfaces_lock.unlock
        next
      end

      begin
        data.destructors.each(&.call)
        data.destructors.clear

        # Perform reinsert.
        SensorSubject.each(@fresh, surface.pattern) do |subject|
          destructor = tspace.summon(@client, subject, identity, @address, surface.selector, &sink)

          data.destructors << destructor
        end

        @surfaces[identity] = data
      ensure
        @surfaces_lock.unlock
      end

      Log.info { "#{@client}: keep alive sensor #{surface} at #{identity}" }

      # Recursively reschedule again.
      after((reinsert_min_ms...reinsert_max_ms).sample.milliseconds) { rec.call(rec) }
    end
  end

  def []=(identity : Identity, surface : Appearance) : Nil
    version = 0u32

    sink = ->(raddr : Tspace::Address, rsensor : Label, adata : AppearanceData) do
      Log.debug { "#{@client}: send #{adata} to #{raddr}'s sensor #{rsensor}" }
    end

    @surfaces_lock.synchronize do
      if data = @surfaces[identity]?
        data.destructors.each(&.call)
        data.destructors.clear
        data = data.copy_with(version: data.version + 1)
      else
        data = SurfaceData.new(destructors: [] of Tspace::Dismiss, version: 0u32)
      end

      # Save current version in the closure.
      version = data.version

      # Perform initial insert.
      subject = AppearanceSubject.new(@fresh.call, surface.value)
      destructor = tspace.summon(@client, subject, identity, surface.selector, surface.tombstone, &sink)
      data.destructors << destructor

      @surfaces[identity] = data
    end

    Log.info { "#{@client}: inserted appearance #{surface} at #{identity}" }

    reinsert_min_ms = @reinsert_min.total_milliseconds
    reinsert_max_ms = @reinsert_max.total_milliseconds

    after((reinsert_min_ms...reinsert_max_ms).sample.milliseconds) do |rec|
      @surfaces_lock.lock

      # First of all we should check whether our local state of identity
      # is consistent with the current state of identity.
      unless data = @surfaces[identity]?
        @surfaces_lock.unlock
        next
      end

      # If version numbers do not match we simply quit. Identity was modified
      # and another reinsert fiber is now responsible for reinsertion.
      unless version == data.version
        @surfaces_lock.unlock
        next
      end

      begin
        data.destructors.each(&.call)
        data.destructors.clear

        # Perform reinsert.
        subject = AppearanceSubject.new(@fresh.call, surface.value)
        destructor = tspace.summon(@client, subject, identity, surface.selector, surface.tombstone, &sink)
        data.destructors << destructor

        @surfaces[identity] = data
      ensure
        @surfaces_lock.unlock
      end

      Log.info { "#{@client}: keep alive appearance #{surface} at #{identity}" }

      # Recursively reschedule again.
      after((reinsert_min_ms...reinsert_max_ms).sample.milliseconds) { rec.call(rec) }
    end
  end

  def delete(identity : Identity) : Nil
    @surfaces_lock.synchronize do
      return unless data = @surfaces.delete(identity)

      data.destructors.each(&.call)
    end

    Log.info { "#{@client}: removed surface #{identity}" }
  end
end

# map = SyncInMemoryMap(Tspace::Key, Tspace::Value).new
map = TermMap(Tspace::Key, Tspace::Value).new(CompactMLMap.new(DigestedKeyMap(String, String).new(RemoteStringMap.new("127.0.0.1", 9000))))

sink = ->(act : Activation) do
  pp act
end

Tconn.open(WWID, map, sink) do |conn|
  conn[0] = Tconn::Sensor.new(%{((%any div mod) a_number (%all b_number (%not 0)))})
  conn[1] = Tconn::Appearance.new(Term.of(:div, 100, 200))
  conn[2] = Tconn::Appearance.new(Term.of(:mod, 300, 400))
  conn[2] = Tconn::Appearance.new(Term.of(:qux, 123))
  conn[1] = Tconn::Sensor.new(%{(qux x_)})

# conn.delete(0)
# conn.delete(2)
# conn.delete(1)

  sleep
end


# sleep

# TODO: StimulusAbsence is sent by Tconn to itself (simulated). Tconn will have to
# periodically reinsert to keep itself alive. Reinsert must create completely new
# sensors/appearances (instants) rather than reusing old ones, to ensure buckets'
# reappearance does not disrupt anything (so that we do not end up with copies of
# the same entries in the scenario where A holds B's pairs, A disappears, B reinserts,
# A appears with B's old pairs). When the same sensor identity across two consecutive
# instants (I and I+1 after keepalive reinsert) detects dismissal of an appearance,
# it sends StimulusAbsence.
# TODO: what are addresses going to be?
# TODO: I'm still completely unsure!!! about whether reinsertion works. It kind of
#   does, but then, does it? Besides duplication, we must do heavy filtering on
#   the basis of instants in sensor sink callback. That is, we must only accept from
#   adata directed at the current instant. What I am worried about is IMap#dec. When we
#   remove we DECREF, so if different surfaces reuse the same part, we'd could get decref
#   wrong?! or could we? assuming it's on the same bucket?
#     I think part of this could be fixed by using surface instant id instead of client id
#     as referrer. Since we're reinserting surfaces it's going to be all or nothing.

{% skip_file %}

notify = ->(address : Tspace::Address, act : Activation) do
  puts "Send #{act} to #{address}"
end
fresh = WWID
storage = TermMap(Tspace::Key, Tspace::Value).new(CompactMLMap.new(DigestedKeyMap(String, String).new(RemoteStringMap.new("127.0.0.1", 9000))))
tspace = Tspace.new(fresh, storage)

client0 = fresh.call
client1 = fresh.call
client2 = fresh.call

dismiss = [] of ->

n = 0
Sensor.each(fresh, ML.term %{(div a_number b_number)}) do |sensor|
  s = tspace.summon(client0, sensor, 0u32, "address(sensor-#{n})", &notify)
  n += 1
  dismiss << s
end

ap1 = tspace.summon(client1, Appearance.new(fresh.call, Term.of(:div, 100, 200)), 0u32, &notify)
dismiss << ap1
ap2 = tspace.summon(client2, Appearance.new(fresh.call, Term.of(:mod, 100, 200)), 0u32, &notify)
dismiss << ap2

Sensor.each(fresh, ML.term %{(mod a_number b_number)}) do |sensor|
  s = tspace.summon(client0, sensor, 0u32, "address(sensor-#{n})", &notify)
  dismiss << s
  n += 1
end

# puts "Dismissing!!!"
dismiss.each &.call

{% skip_file %}

fresh = WWID
# storage = SerializedValue(Tbase::Key, Tbase::Value).new(DigestedKey(Tbase::Key, String).new(SyncInMemoryMap(String, String).new))
# storage = DigestedKey(Tbase::Key, Tbase::Value).new(SyncInMemoryMap(String, Tbase::Value).new)
storage = SyncInMemoryMap(Tbase::Key | Tspace::Key, Tbase::Value | Tspace::Value).new
tbase = Tbase.new(fresh, storage.submap(Tbase::Key, Tbase::Value))
tspace = Tspace.new(fresh, storage.submap(Tspace::Key, Tspace::Value))

client0 = fresh.call
client1 = fresh.call
client2 = fresh.call
sensors = [] of {Tbase::Surface, SensorData}
Sensor.each(fresh, Term.of({:"%any", :div, :mod}, :a_number, :b_number)) do |sensor|
  surf = tbase.mount(client0, sensor)
  data = SensorData.new(client0, 0u32, nil, "IP address of sensor 0 owner")
  tspace.sensors.mount(client0, surf.subject.id, data)

  sensors << {surf, data}
end

ap1_value = Term.of(:mod, 100, 200)
ap1 = tbase.mount(client1, Appearance.new(fresh.call, ap1_value))
ap1_data = AppearanceData.new(client1, 0u32, ap1_value, nil, nil)
tspace.appearances.mount(client1, ap1.subject.id, ap1_data)
ap2_value = Term.of(:div, 100, 200)
ap2 = tbase.mount(client2, Appearance.new(fresh.call, ap2_value))
ap2_data = AppearanceData.new(client2, 0u32, ap2_value, nil, nil)
tspace.appearances.mount(client2, ap2.subject.id, ap2_data)

ap1.each_complementary do |sensor|
  next unless sdata = tspace.sensors.query?(sensor)
  next unless sdata.selector == ap1_data.selector

  puts "Send #{StimulusPresence.new(ap1.subject.id, ap1_data.client, ap1_data.identity, ap1_data.value)} to #{sdata.address}"
end

pp! ap2.complement

sensors.each do |surface, data|
  surface.each_complementary(successors: true) do |appearance|
    next unless adata = tspace.appearances.query?(appearance)
    next unless adata.selector == data.selector

    puts "Send #{StimulusPresence.new(appearance, adata.client, adata.identity, adata.value)} to #{data.address}"
  end
end

# etrace = Etrace.new(fresh, storage)

# client0 = fresh.call
# foo = fresh.call
# bar = fresh.call
# a = fresh.call
# b = fresh.call
# c = fresh.call
# d = fresh.call
# e = fresh.call

# etrace.mount(client0, [foo, bar, a, b, c])
# etrace.mount(client0, [foo, bar, a, b, d])
# etrace.mount(client0, [foo, bar, a, e])

# etrace.walk(foo) do |label|
#   pp({foo => "foo", bar => "bar", a => "a", b => "b", c => "c", d => "d", e => "e"}[label])
# end

# lst.mount(client0, foo, a)
# lst.mount(client0, foo, b)
# lst.mount(client0, bar, c)
# pp storage.@data.size

# lst.query(bar) do |succ|
#   pp ({a => "a", b => "b", c => "c"})[succ]
# end

{% skip_file %}

pattern = ML.term %{((%any div mod) a_ (%all b_number (%not 0)) ¦ precision⋮ 3)}
normp = M1.normal(pattern)
skeleton = Skeleton.pattern(normp)
strands = [] of Strand
branches = [] of StrandList

branches(skeleton) do |branch|
  strands(branch) do |strand|
    strands << strand.items.to_readonly_slice { |base| Ubase.parse(base) }
  end
  branches << strands.to_readonly_slice(&.itself)
  strands.clear
end
sensor = branches.to_readonly_slice(&.itself)


# ---

fresh = -> { WWID.call }

client0 = fresh.call
client1 = fresh.call

storage = SyncInMemoryMap(Utrie::Key | Xgraph::Key | Ttrie::Key | Etrace::Key | StrandSet::Key | AppearanceSet::Key | SensorDecoder::Key, Utrie::Value | Xgraph::Value | Ttrie::Value | Etrace::Value | StrandSet::Value | AppearanceSet::Value | SensorDecoder::Value).new

utrie = Utrie.new(fresh, storage.submap(Utrie::Key, Utrie::Value))
strandset = StrandSet.new(storage.submap(StrandSet::Key, StrandSet::Value))
sensor_decoder = SensorDecoder.new(fresh, storage.submap(SensorDecoder::Key, SensorDecoder::Value))

pp sensor[0]

rule = Deque(Label).new
deps = Set({Utrie::Key | Xgraph::Key | Ttrie::Key | Etrace::Key | StrandSet::Key | AppearanceSet::Key | SensorDecoder::Key, Utrie::Value | Xgraph::Value | Ttrie::Value | Etrace::Value | StrandSet::Value | AppearanceSet::Value | SensorDecoder::Value}).new

sensor[0].each do |strand|
  subdeps, endpoint = utrie.mount(client0, strand)
  deps.concat(strandset.mount(client0, endpoint))
  rule << endpoint
  deps.concat(subdeps)
end

xgraph = Xgraph.new(fresh, storage.submap(Xgraph::Key, Xgraph::Value))

rule.unstable_sort!

subdeps, rulepoint = xgraph.mount(client0, rule)
deps.concat(subdeps)

sensor_id = fresh.call

deps.concat(sensor_decoder.mount(client0, rulepoint, sensor_id))

puts "Sensor is #{sensor_id.to_s(32, precision: 26)}"

# ---
found = Deque(Label).new
utrie.query(Term.of(:mod, 100, 200)) do |label|
  next unless strandset.strand?(label)
  found << label
end
found.unstable_sort!
xgraph.conjs(found) do |conj|
  sensor_decoder.decode(conj) do |sensor_id_decoded|
    puts "Hit sensor #{sensor_id_decoded.to_s(32, precision: 26)}"
  end
end

# ----

ttrie = Ttrie.new(fresh, storage.submap(Ttrie::Key, Ttrie::Value))
etrace = Etrace.new(fresh, storage.submap(Etrace::Key, Etrace::Value))
appearance_set = AppearanceSet.new(storage.submap(AppearanceSet::Key, AppearanceSet::Value))

x = Term.of(:mod, 100, 200)
id = fresh.call
Term.each_keypath_and_leaf(x) do |keypath, leaf|
  keypath.push(leaf)
  subdeps, path = ttrie.mount(client0, keypath, id)
  deps.concat(subdeps)
  keypath.pop

  deps.concat(etrace.mount(client0, path))

  true # Continue
end
deps.concat(appearance_set.mount(client0, id))
puts "Appearance is #{id.to_s(32, precision: 26)}"

pp storage.@data.size

# pp ttrie.query?({Ubase::IsAny.new})
endpoint = ttrie.query?({Ubase::IsAny.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:mod))})

if endpoint
  etrace.walk(endpoint) do |hit|
    next unless appearance_set.appearance?(hit)
    puts "Hit #{hit.to_s(32, precision: 26)}"
  end
end

