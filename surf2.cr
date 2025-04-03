require "log"
require "simple_rpc"
require "digest/sha256"
require "compress/gzip"
require "json"
require "./src/wirewright"
require "./surf_common"

include Meridium

Log.setup_from_env(default_level: :error)

record Label, value : UInt128 do
  include Comparable(Label)

  class_getter zero = Label.new(0u128)

  def <=>(other : Label)
    value <=> other.value
  end

  def encode(otype : Term.class) : Term
    Term.of(value)
  end

  def self.decode?(value : Term) : Label?
    Term.matchpi?(value, %{(%number u128)}) do
      new(value.to(UInt128))
    end
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

  def self.call : Label
    order = Time.utc.to_unix_ns.to_u128
    randomness = Random::Secure.rand(UInt64)

    Label.new((order << 64) | randomness)
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

# TODO: rewrite using locks, should end up being simpler...

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

  def decall(referrer : Label, keys : Array(String)) : Nil
    keys.each { |key| dec(referrer, key) }
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

  def decall(referrer : String, keys : Array(String)) : Nil
    @@map.decall(Label.new(referrer.to_u128), keys)
  end

  def gzdecall(referrer : String, zipped : String) : Nil
    io = IO::Memory.new(zipped)

    unzipped = Compress::Gzip::Reader.open(io) do |gzip|
      gzip.gets_to_end
    end

    # FIXME: something more reliable than newline separation
    keys = unzipped.split('\n')

    @@map.decall(Label.new(referrer.to_u128), keys)
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

  def decall(referrer : Label, keys : Array(String)) : Nil
    # FIXME: something more reliable than newline separation
    unzipped = keys.join('\n')
    zipped = String.build do |io|
      Compress::Gzip::Writer.open(io) do |gzip|
        gzip << unzipped
      end
    end

    @client.gzdecall!(referrer.value.to_s, zipped)
  end
end

module IChat(M)
  alias Unsubscribe = ->

  abstract def subscribe(address : Label, &recv : M ->) : Unsubscribe
  abstract def send(to receiver : Label, message : M) : Nil
end

class SyncInMemoryChat(M)
  include IChat(M)

  @subscribers = {} of Label => Set(M ->)
  @lock = Mutex.new

  def subscribe(address : Label, &recv : M ->) : Unsubscribe
    @lock.synchronize do
      recvs = @subscribers.put_if_absent(address) { Set(M ->).new }
      recvs << recv
    end

    Unsubscribe.new do
      @lock.synchronize do
        next unless recvs = @subscribers[address]?
        next unless recvs.delete(recv)
        next unless recvs.empty?

        @subscribers.delete(address)
      end
    end
  end

  def send(to receiver : Label, message : M) : Nil
    recvs = @lock.synchronize do
      # Copy receiver procs (if any) so that we can call them outside of the lock,
      # and so that they're "frozen in time".
      @subscribers[receiver]?.try(&.dup)
    end

    return unless recvs

    recvs.each &.call(message)
  end
end

class TermChat(M)
  include IChat(M)

  def initialize(@chat : IChat(Term))
  end

  def subscribe(address : Label, &recv : M ->) : Unsubscribe
    @chat.subscribe(address) do |message|
      recv.call(M.decode?(message) || raise TermDecodeError.new)
    end
  end

  def send(to receiver : Label, message : M) : Nil
    @chat.send(receiver, message.encode(Term))
  end
end

class CompactMLChat
  include IChat(Term)

  def initialize(@chat : IChat(String))
  end

  def subscribe(address : Label, &recv : Term ->) : Unsubscribe
    @chat.subscribe(address) do |message|
      recv.call(ML.term(message))
    end
  end

  def send(to receiver : Label, message : Term) : Nil
    @chat.send(receiver, ML.compact(message))
  end
end

struct Utrie
  alias Key = Origin | Step

  record Origin, base : Ubase::Any do
    def encode(otype : Term.class) : Term
      Term.of(:utrie, :key, :origin, base.term)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(utrie key origin base_)}) do
        new(Ubase.parse(base))
      end
    end
  end

  record Step, pred : Label, base : Ubase::Any do
    def encode(otype : Term.class) : Term
      Term.of(:utrie, :key, :step, pred.encode(Term), base.term)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(utrie key step pred_ base_)}) do
        new(Label.decode?(pred) || return, Ubase.parse(base))
      end
    end
  end

  record Value, succ : Label do
    def encode(otype : Term.class) : Term
      Term.of(:utrie, :value, :primary, succ.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(utrie value primary succ_)}) do
        new(Label.decode?(succ) || return)
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
      Term.of(:xgraph, :key, :primary, a.encode(Term), b.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(xgraph key primary a_ b_)}) do
        new(Label.decode?(a) || return, Label.decode?(b) || return)
      end
    end
  end

  record Value, succ : Label do
    def encode(otype : Term.class) : Term
      Term.of(:xgraph, :value, :primary, succ.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(xgraph value primary succ_)}) do
        new(Label.decode?(succ) || return)
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
      Term.of(:ttrie, :key, :origin)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(ttrie key origin)}) do
        new
      end
    end
  end

  record Step, pred : Label, base : Ubase::Any do
    def encode(otype : Term.class) : Term
      Term.of(:ttrie, :key, :step, pred.encode(Term), base.term)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(ttrie key step pred_ base_)}) do
        new(Label.decode?(pred) || return, Ubase.parse(base))
      end
    end
  end

  record Value, succ : Label do
    def encode(otype : Term.class) : Term
      Term.of(:ttrie, :value, :primary, succ.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(ttrie value primary succ_)}) do
        new(Label.decode?(succ) || return)
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
      Term.of(:etrace, :key, :primary, scope.encode(Term), state.encode(Term), digitno)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(etrace key primary scope_ state_ digitno←(%number u8))}) do
        new(Label.decode?(scope) || return, Label.decode?(state) || return, digitno.to(UInt8))
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of(:etrace, :value, :primary)
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(etrace value primary)}) { new }
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
      Term.of(:"strand-set", :key, :primary, vertex.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(strand-set key primary vertex_)}) do
        new(Label.decode?(vertex) || return)
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of(:"strand-set", :value, :primary)
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(strand-set value primary)}) { new }
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
      Term.of(:"appearance-set", :key, :primary, vertex.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(appearance-set key primary vertex_)}) do
        new(Label.decode?(vertex) || return)
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of(:"appearance-set", :value, :primary)
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(appearance-set value primary)}) { new }
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

# One-to-many map for decoding a conjunction vertex into the sensors that
# were bound to it.
# TODO: extract common with `Etrace`
struct SensorMultimap
  # :nodoc:
  BASE = 4u128

  # :nodoc:
  BASE_DIGITS = {0u128, 1u128, 2u128, 3u128}

  # :nodoc:
  BASE_LENGTH_U128 = 64u8

  record Key, scope : Label, state : Label, digitno : UInt8 do
    def encode(otype : Term.class) : Term
      Term.of(:"sensor-decoder", :key, :primary, scope.encode(Term), state.encode(Term), digitno)
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(sensor-decoder key primary scope_ state_ digitno←(%number u8))}) do
        new(Label.decode?(scope), Label.decode?(state), digitno.to(UInt8))
      end
    end
  end

  record Value do
    def encode(otype : Term.class) : Term
      Term.of(:"sensor-decoder", :value, :primary)
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(sensor-decoder value primary)}) { new }
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

record SensorData, conid : Label, groupid : Label, identity : Identity, instant : Label, selector : Term? do
  def encode(otype : Term.class) : Term
    Term.of(:"sensor-data", conid.encode(Term), groupid.encode(Term), identity, instant.encode(Term), selector: selector)
  end

  def self.decode?(object : Term) : SensorData?
    Term.matchpi?(object, %{(sensor-data conid_ groupid_ identity←(%number u32) instant_ ¦ (%keypool selector))}) do
      new(
        Label.decode?(conid) || return,
        Label.decode?(groupid) || return,
        identity.to(Identity),
        Label.decode?(instant) || return,
        object[:selector]?,
      )
    end
  end
end

record AppearanceData,
  conid : Label,
  identity : Identity,
  instant : Label,
  value : Term,
  selector : Term?

struct AppearanceData
  def encode(otype : Term.class) : Term
    Term.of(:"appearance-data", conid.encode(Term), identity, instant.encode(Term), value, selector: selector)
  end

  def self.decode?(object : Term) : AppearanceData?
    Term.matchpi?(object, %{(appearance-data conid_ identity←(%number u32) instant_ value_ ¦ (%keypool selector))}) do
      new(
        Label.decode?(conid) || return,
        identity.to(Identity),
        Label.decode?(instant) || return,
        value,
        object[:selector]?,
      )
    end
  end
end

struct SensorDataMap
  record Key, instant : Label do
    def encode(otype : Term.class) : Term
      Term.of(:"sensor-base", :key, :primary, instant.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(sensor-base key instant_)}) do
        new(Label.decode?(scope) || return)
      end
    end
  end

  record Value, data : SensorData do
    def encode(otype : Term.class) : Term
      Term.of(:"sensor-base", :value, :primary, data.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(sensor-base value primary data_)}) do
        new(SensorData.decode?(data) || return)
      end
    end
  end

  def initialize(@map : IMap(Key, Value))
  end

  def mount(instant : Label, data : SensorData, *, deps = Bag({Key, Value}).new)
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @map.inc(instant, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps << {key, value0}
    deps
  end

  def query?(instant : Label) : SensorData?
    return unless value = @map.latest?(instant, Key.new(instant))

    value.data
  end
end

struct AppearanceDataMap
  record Key, instant : Label do
    def encode(otype : Term.class) : Term
      Term.of(:"appearance-base", :key, :primary, instant.encode(Term))
    end

    def self.decode?(term : Term) : Key?
      Term.matchpi?(term, %{(appearance-base key instant_)}) do
        new(Label.decode?(instant) || return)
      end
    end
  end

  record Value, data : AppearanceData do
    def encode(otype : Term.class) : Term
      Term.of(:"appearance-base", :value, :primary, data.encode(Term))
    end

    def self.decode?(term : Term) : Value?
      Term.matchpi?(term, %{(appearance-base value primary data_)}) do
        new(AppearanceData.decode?(data) || return)
      end
    end
  end

  def initialize(@map : IMap(Key, Value))
  end

  def mount(instant : Label, data : AppearanceData, *, deps = Bag({Key, Value}).new)
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @map.inc(instant, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps << {key, value0}
    deps
  end

  def query?(instant : Label) : AppearanceData?
    return unless value = @map.latest?(instant, Key.new(instant))

    value.data
  end
end

alias Identity = UInt32

record Activation, kind : Kind, sdata : SensorData, adata : AppearanceData do
  enum Kind : UInt8
    StimulusPresence
    StimulusAbsence
  end

  def encode(otype : Term.class) : Term
    Term.of(:activation, kind, sdata.encode(Term), adata.encode(Term))
  end

  def self.decode?(term : Term) : Activation?
    Term.matchpi?(term, %{(activation kind←(%number u8) sdata_ adata_)}) do
      new(kind.to(Kind), SensorData.decode?(sdata) || return, AppearanceData.decode?(adata) || return)
    end
  end
end

class SurfaceNotMountedError < Exception
end

struct Tspace
  alias Key = Tbase::Key | SensorDataMap::Key | AppearanceDataMap::Key
  alias Value = Tbase::Value | SensorDataMap::Value | AppearanceDataMap::Value
  alias Dismiss = ->

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def tbase : Tbase
    Tbase.new(@fresh, @map.submap(Tbase::Key, Tbase::Value))
  end

  def sensors : SensorDataMap
    SensorDataMap.new(@map.submap(SensorDataMap::Key, SensorDataMap::Value))
  end

  def appearances : AppearanceDataMap
    AppearanceDataMap.new(@map.submap(AppearanceDataMap::Key, AppearanceDataMap::Value))
  end

  def summon(sdata : SensorData, subject : Tbase::Sensor, *, seen : S = [] of AppearanceData) : {S, Dismiss} forall S
    deps = Bag({Key, Value}).new

    _ = sensors.mount(subject.id, sdata, deps: deps)
    _ = tbase.mount(subject, deps: deps)

    each_complement(subject, sdata.selector, &->seen.<<(AppearanceData))

    mounted = true

    dismiss = Dismiss.new do
      unless mounted
        raise SurfaceNotMountedError.new
      end

      mounted = false

      @map.decall(subject.id, deps.to_a { |(key, _)| key })

      nil
    end

    {seen, dismiss}
  end

  def summon(adata : AppearanceData, subject : Tbase::Appearance, &excite : SensorData ->) : Dismiss
    deps = Bag({Key, Value}).new

    _ = appearances.mount(subject.id, adata, deps: deps)
    _ = tbase.mount(subject, deps: deps)

    each_complement(subject, adata.selector, &excite)

    mounted = true

    Dismiss.new do
      unless mounted
        raise SurfaceNotMountedError.new
      end

      mounted = false

      @map.decall(subject.id, deps.to_a { |(key, _)| key })

      nil
    end
  end

  def each_complement(subject : Tbase::Sensor, selector : Term?, &sink : AppearanceData ->)
    tbase.each_complement(subject) do |appearance|
      next unless adata = appearances.query?(appearance)
      next unless selector == adata.selector

      sink.call(adata)
    end
  end

  def each_complement(subject : Tbase::Appearance, selector : Term?, &sink : SensorData ->)
    tbase.each_complement(subject) do |sensor|
      next unless sdata = sensors.query?(sensor)
      next unless sdata.selector == selector

      sink.call(sdata)
    end
  end
end

class Tconn
end

# An object that manages keepalive & keepalive periods for `Tconn`.
struct Tconn::Keepalive
  alias Task = Task ->

  Log = ::Log.for("Tconn keepalive")

  # Period range used for sensors by default.
  DEFAULT_SENSOR_PERIOD = 40.seconds..2.minutes

  # Period range used for appearances by default.
  DEFAULT_APPEARANCE_PERIOD = 20.seconds..50.seconds

  # Used to specify which of the periods one wants to address in e.g.
  # `min_for`, `max_for`, `schedule`.
  enum Period : UInt8
    # Use the sensor period bounds.
    Sensor

    # Use the appearance period bounds.
    Appearance
  end

  # Constructs a Tconn keepalive object.
  #
  # - Keepalive fibers are spawned in *ctx*.
  # - *sensor period* specifies minimum and maximum waiting time before
  #   running sensor keepalive.
  # - *appearance period* specifies minimum and maximum waiting time before
  #   running appearance keepalive.
  def initialize(@ctx : ExecutionContext = ExecutionContext::SingleThreaded.new("Tconn keepalive"),
                 @sensor_period = DEFAULT_SENSOR_PERIOD,
                 @appearance_period = DEFAULT_APPEARANCE_PERIOD)
    if @sensor_period.exclusive?
      raise ArgumentError.new("sensor period range must be inclusive")
    end

    if @appearance_period.exclusive?
      raise ArgumentError.new("appearance period range must be inclusive")
    end

    @running = Channel(Bool).new

    @prng = Random::PCG32.new
    @prng_lock = Mutex.new
  end

  # Initializes both sensor period and appearance period to *period*.
  def initialize(*args, period : Range(Time::Span, Time::Span), **kwargs)
    initialize(*args, **kwargs, sensor_period: period, appearance_period: period)
  end

  # Returns the minimum waiting time for *period*.
  def min_for(period : Period) : Time::Span
    case period
    in .sensor?     then @sensor_period.begin
    in .appearance? then @appearance_period.begin
    end
  end

  # Returns the maximum waiting time for *period*.
  def max_for(period : Period) : Time::Span
    case period
    in .sensor?     then @sensor_period.end
    in .appearance? then @appearance_period.end
    end
  end

  # Terminates all scheduled tasks.
  #
  # Can only be called once. `schedule` will not work after calling this method.
  def stop : Nil
    Log.debug { "stop" }

    @running.close
  end

  # Schedules *task* using minimum/maximum waiting time for *period*.
  def schedule(task : Task, period : Period) : Nil
    tmin = min_for(period).total_milliseconds
    tmax = max_for(period).total_milliseconds
    t = @prng_lock.synchronize { (tmin..tmax).sample(random: @prng) }.milliseconds

    Log.debug { "schedule task #{task} after #{t.total_seconds}s" }

    @ctx.spawn do
      select
      when @running.receive? # nil
      when timeout(t)
        task.call(task)
      end
    end
  end

  # :ditto:
  def schedule(period : Period, &task : Task ->) : Nil
    schedule(task, period)
  end
end

class Tview
  record Stimulus, instant : Label, matches : Array(Term::Dict)

  getter groupid : Label
  getter pattern : Term

  def initialize(@groupid : Label, @pattern, @stimuli = Pf::Map({Label, Identity}, Stimulus).new)
  end

  private def_change

  def self.build(instant : Label, pattern : Term, adatas : Array(AppearanceData)) : Tview
    adatas.reduce(new(instant, pattern)) { |view, adata| view.present(adata) }
  end

  def empty? : Bool
    @stimuli.empty?
  end

  protected def present(adata : AppearanceData) : Tview
    pid = {adata.conid, adata.identity}

    if stimulus = @stimuli[pid]?
      # Make sure the activation is about a newer version of the appearance
      # than the one we're observing.
      return self if stimulus.instant > adata.instant
    end

    stimuli1 = @stimuli

    matches = M1.matches(@pattern, adata.value)
    if matches.empty?
      # Consider the activation removed if it does not match the pattern, regardless
      # of whether we were observing it before.
      stimuli1 = stimuli1.dissoc(pid)
    else
      stimuli1 = stimuli1.assoc(pid, Stimulus.new(adata.instant, matches))
    end

    change(stimuli: stimuli1)
  end

  protected def absent(adata : AppearanceData) : Tview
    pid = {adata.conid, adata.identity}

    return self unless stimulus = @stimuli[pid]?

    # Make sure the activation is about a newer or the same version of
    # the appearance that we're observing.
    return self unless stimulus.instant <= adata.instant

    change(stimuli: @stimuli.dissoc(pid))
  end

  def advance(act : Activation) : Tview
    case act.kind
    in .stimulus_presence? then present(act.adata)
    in .stimulus_absence?  then absent(act.adata)
    end
  end

  # Returns a dict multiset of match envs in this view.
  def dict_multiset : Term::Dict
    Term::Dict.build do |commit|
      @stimuli.each do |_, stimulus|
        stimulus.matches.each do |env|
          commit.with(env, (commit[env]? || 0) + 1)
        end
      end
    end
  end
end

# Tconn maintains the illusion of persistence of sensors and appearances
# throughout periodic keepalive reinsertion.
class Tconn
  Log = ::Log.for("Tconn")

  alias Map = IMap(Tspace::Key, Tspace::Value)
  alias Chat = IChat(Activation)

  record Sensor, pattern : Term, selector : Term? = nil do
    def self.new(pattern : String, **kwargs) : Sensor
      new(ML.term(pattern), **kwargs)
    end
  end

  record Appearance, value : Term, selector : Term? = nil do
    def self.new(value : String, **kwargs) : Appearance
      new(ML.term(value), **kwargs)
    end
  end

  alias Surface = Sensor | Appearance

  # NOTE: *sink* may be called with the same `Overview` multiple times in a row;
  # it is your responsibility to suppress repetitions if necessary.
  record Spec,
    map : Map,
    chat : Chat,
    sink : Sink,
    fresh : LabelGenerator = WWID,
    keepalive : Keepalive? = nil

  alias Overview = Pf::Map(Identity, Tview)

  alias Sink = Overview ->
  alias Destructor = Bool ->

  record SurfaceData, instant : Label, destructors : Array(Destructor)

  @conid : Label

  @unsubscribe : IChat::Unsubscribe

  @surfaces_lock = Mutex.new(:reentrant)

  class ClosedError < Exception
  end

  def initialize(spec : Spec)
    # Extract ivars from spec.
    @map = spec.map
    @chat = spec.chat
    @sink = spec.sink
    @fresh = spec.fresh
    @keepalive = spec.keepalive

    @conid = @fresh.call

    @overview = Overview.new
    @surfaces = {} of Identity => SurfaceData
    @last_alive_at = {} of Label => Time::Span

    @unsubscribe = @chat.subscribe(@conid) do |act|
      Log.debug { "#{@conid}: receive from chat: #{act}" }

      @surfaces_lock.synchronize do
        # If we can fetch view for act's sensor identity, then we know it's still
        # a sensor.
        next unless view0 = @overview[act.sdata.identity]?
        next unless act.sdata.groupid == view0.groupid

        # Record activity. If someone was able to reach us via the termspace, this
        # means we're alive and keepalive can be postponed.
        @last_alive_at[act.sdata.groupid] = Time.monotonic

        view1 = view0.advance(act)

        # Update the view.
        @overview = @overview.assoc(act.sdata.identity, view1)

        @sink.call(@overview)
      end
    end
  end

  def self.multisets(&sink : Term::Dict ->) : Sink
    multisets0 = Term[]

    Sink.new do |overview|
      multisets1 = Term::Dict.build do |commit|
        overview.each do |key, view|
          next if view.empty?

          commit.with(key, view.dict_multiset)
        end
      end

      # TODO: check equality of overviews instead! And optimize Tview equality
      # (e.g. have a dirty flag?)
      next if multisets0 == multisets1

      multisets0 = multisets1

      sink.call(multisets0)
    end
  end

  def self.open(spec : Spec, & : Tconn -> T) : T forall T
    conn = new(spec)

    begin
      yield conn
    ensure
      conn.close
    end
  end

  def self.open(*args, **kwargs, & : Tconn -> T) : T forall T
    open(Spec.new(*args, **kwargs)) { |conn| yield conn }
  end

  def tspace : Tspace
    Tspace.new(@fresh, @map)
  end

  def close
    # Terminate keepalive fibers.
    @keepalive.try(&.stop)

    # Unsubscribe from network messages.
    @unsubscribe.call

    # Call destructors for all surfaces -- gracefully leave the termspace.
    @surfaces_lock.synchronize do
      @surfaces.each do |_, data|
        data.destructors.each &.call(true) # final
      end
    end
  end

  private def present(sdata : SensorData, adata : AppearanceData) : Nil
    Log.debug { "#{@conid}: send stimulus presence #{adata} to #{sdata.conid}" }

    @chat.send(to: sdata.conid, message: Activation.new(:stimulus_presence, sdata, adata))
  end

  private def absent(sdata : SensorData, adata : AppearanceData) : Nil
    Log.debug { "#{@conid}: send stimulus absence #{adata} to #{sdata.conid}" }

    @chat.send(to: sdata.conid, message: Activation.new(:stimulus_absence, sdata, adata))
  end

  # Queries which sensors the appearance excites, and sends them an absence message.
  #
  # This is only true for graceful appearance exits; if the appearance did not
  # exit gracefully the sensors are expected to find out themselves (after
  # some period of time).
  private def absence(identity : Identity, adata : AppearanceData, subject : Tbase::Appearance)
    tspace.each_complement(subject, adata.selector) do |sdata|
      absent(sdata, adata)
    end
  end

  private def destructor(identity : Identity, sdata : SensorData, subject : Tbase::Sensor, dismiss : Tspace::Dismiss)
    Destructor.new { |_final| dismiss.call }
  end

  private def destructor(identity : Identity, adata : AppearanceData, subject : Tbase::Appearance, dismiss : Tspace::Dismiss)
    Destructor.new do |final|
      if final
        absence(identity, adata, subject)
      end

      dismiss.call
    end
  end

  # WARNING: Assumes the surfaces lock is taken.
  private def insert(identity : Identity, surface : Sensor, groupid : Label) : Nil
    Log.trace { "#{@conid}: begin insert of sensor group #{groupid} (surface: #{surface}) at #{identity}" }

    data = SurfaceData.new(instant: groupid, destructors: [] of Destructor)
    seen = [] of AppearanceData

    # NOTE: a `Tconn::Sensor` surface can be broken down (and is broken down here)
    # into multiple `Tbase::Sensor`s. See `Tbase::Sensor.each`.
    Tbase::Sensor.each(@fresh, surface.pattern) do |subject|
      sdata = SensorData.new(@conid, groupid, identity, subject.id, surface.selector)
      _, dismiss = tspace.summon(sdata, subject, seen: seen)
      data.destructors << destructor(identity, sdata, subject, dismiss)
    end

    view = Tview.build(groupid, surface.pattern, seen)

    @surfaces[identity] = data
    @overview = @overview.assoc(identity, view)
    @sink.call(@overview)

    Log.debug { "#{@conid}: inserted sensor group #{groupid}" }
  end

  # WARNING: Assumes the surfaces lock is taken.
  private def insert(identity : Identity, surface : Appearance, groupid : Label) : Nil
    Log.trace { "#{@conid}: begin insert of appearance #{groupid} (surface: #{surface}) at #{identity}" }

    data = SurfaceData.new(instant: groupid, destructors: [] of Destructor)

    subject = Tbase::Appearance.new(groupid, surface.value)
    adata = AppearanceData.new(@conid, identity, subject.id, subject.value, surface.selector)

    dismiss = tspace.summon(adata, subject) do |sdata|
      present(sdata, adata)
    end

    data.destructors << destructor(identity, adata, subject, dismiss)

    @surfaces[identity] = data

    Log.debug { "#{@conid}: inserted appearance #{groupid} (surface: #{surface}) at #{identity}" }
  end

  # WARNING: Assumes the surfaces lock is taken.
  private def delete(identity : Identity, *, final : Bool) : Nil
    Log.trace { "#{@conid}: begin delete of surface #{identity}" }

    unless data = @surfaces.delete(identity)
      Log.trace { "#{@conid}: delete early exit: surface #{identity} does not exist" }
      return
    end

    data.destructors.each &.call(final)

    overview1 = @overview.dissoc(identity)
    unless @overview.same?(overview1)
      @overview = overview1
      @sink.call(@overview)
    end

    Log.debug { "#{@conid}: removed surface #{identity}" }
  end

  def []=(identity : Identity, surface : Surface) : Nil
    groupid = @fresh.call

    @surfaces_lock.synchronize do
      delete(identity, final: true)
      insert(identity, surface, groupid)
    end

    return unless keepalive = @keepalive

    case surface
    in Sensor
      period = Keepalive::Period::Sensor
    in Appearance
      period = Keepalive::Period::Appearance
    end

    keepalive.schedule(period) do |this|
      Log.debug { "#{@conid}: run keepalive of #{identity}" }

      @surfaces_lock.synchronize do
        # Make sure identity did not change in the meantime. If it did,
        # then someone else is now responsible for keeping it alive.
        unless data = @surfaces[identity]?
          Log.trace { "#{@conid}: keepalive of #{identity}: exit noreschedule: identity absent" }
          next
        end

        unless groupid == data.instant
          Log.trace { "#{@conid}: keepalive of #{identity}: exit noreschedule: identity changed: #{groupid} != #{data.instant}" }
          next
        end

        # If surface is a sensor that was interacted with in the last N seconds,
        # early exit BUT reschedule.
        if last_alive_at = @last_alive_at.delete(groupid)
          activity = Time.monotonic - last_alive_at

          if activity < keepalive.min_for(:sensor) # ago
            Log.trace { "#{@conid}: keepalive of #{identity}: exit reschedule postpone keepalive: sensor was alive #{activity.total_seconds}s ago" }
            keepalive.schedule(this, period)
            next
          end
        end

        groupid = @fresh.call

        delete(identity, final: false)
        insert(identity, surface, groupid)

        keepalive.schedule(this, period)
      end
    end
  end

  def delete(identity : Identity) : Nil
    @surfaces_lock.synchronize do
      delete(identity, final: true)
    end
  end

  def pretty_print(pp)
    @surfaces_lock.synchronize do
      pp.list("Tconn{", @surfaces, "}") do |identity, data|
        pp.group do
          identity.pretty_print(pp)
          pp.text ": "
          pp.nest do
            pp.breakable
            data.pretty_print(pp)
          end
        end
      end
    end
  end
end

class RemoteStringChat
  include IChat(String)

  def initialize(host : String, port : Int32, ctx = ExecutionContext::SingleThreaded.new("remote chat"))
    @socket = TCPSocket.new(host, port)
    @chat = SyncInMemoryChat(String).new

    @inbound = Channel(String).new

    ctx.spawn do
      while message = @socket.gets
        message = message.chomp
        if rest = message.lchop?("MSG ")
          topic, body = rest.split(" ", limit: 2)
          ctx.spawn { @chat.send(Label.new(topic.to_u128), body) }
        else
          @inbound.send(message)
        end
      end
    end
  end

  def close : Nil
    @socket.close
  end

  def subscribe(address : Label, &recv : String ->) : Unsubscribe
    unsub = @chat.subscribe(address, &recv)
    @socket.puts "+SUB #{address.value}"
    unless @inbound.receive == "OK"
      raise "oh noes, something wrong happened on the chat server!!! i didnt get an OK"
    end
    Unsubscribe.new do
      unsub.call
      @socket.puts "-SUB #{address.value}"
      unless @inbound.receive == "OK"
        raise "oh noes, something wrong happened on the chat server!!! i didnt get an OK"
      end
    end
  end

  def send(to receiver : Label, message : String) : Nil
    @socket.puts "SEND #{receiver.value} #{message}"
    unless @inbound.receive == "OK"
      raise "oh noes, something wrong happened on the chat server!!! i didnt get an OK"
    end
  end
end

{% skip_file unless flag?(:surf2) %}

Log.setup_from_env(default_level: :trace)

ServerLog = ::Log.for("Server")

def handle(chat, unsubs, unsubs_lock, socket)
  while message = socket.gets
    message = message.chomp
    ServerLog.debug { "#{socket}: #{message}" }
    if topic = message.lchop?("+SUB ")
      topic_ = topic
      socket.puts "OK" # assume subscribe cannot fail
      unsub = chat.subscribe(Label.new(topic.to_u128)) do |message|
        socket.puts("MSG #{topic_} #{message}")
      end
      unsubs_lock.synchronize do
        unsubs[{topic, socket}] = unsub
      end
    elsif topic = message.lchop?("-SUB ")
      socket.puts "OK" # assume unsubscribe cannot fail
      if unsub = unsubs_lock.synchronize { unsubs.delete({topic, socket}) }
        unsub.call
      end
    elsif send = message.lchop?("SEND ")
      socket.puts "OK" # assume send cannot fail
      topic, message = send.split(" ", limit: 2)
      chat.send(Label.new(topic.to_u128), message)
    elsif message == "LIST"
      socket.puts "LISTING"
      topics = Set(String).new
      unsubs_lock.synchronize do
        unsubs.each do |(topic, _), _|
          topics << topic
        end
      end
      topics.each do |topic|
        socket.puts "TOPIC #{topic}"
      end
      socket.puts "OK"
    else
      socket.puts "ERR"
    end
  end
ensure
  ServerLog.info { "cleanup after #{socket}" }
  unsubs_lock.synchronize do
    unsubs.reject! do |(topic, its_socket), unsub|
      if reject = its_socket == socket
        unsub.call
      end
      reject
    end
  end
end

def serve_chat(host, port)
  chat = SyncInMemoryChat(String).new
  unsubs = {} of {String, TCPSocket} => IChat::Unsubscribe
  unsubs_lock = Mutex.new

  ctx = ExecutionContext::MultiThreaded.new("server", 4)
  server = TCPServer.new("127.0.0.1", 9811)
  while client = server.accept?
    ctx.spawn { handle(chat, unsubs, unsubs_lock, client) }
  end
end

if ARGV[0]? == "serve"
  ctx = ExecutionContext::MultiThreaded.new("Serve Threads", 2)
  ctx.spawn { StringMapRPC::Server.new("127.0.0.1", 9810).run }
  ctx.spawn { serve_chat("127.0.0.1", 9811) }

  puts "Map on port 9810"
  puts "Chat on port 9811"
  sleep
elsif ARGV[0]? == "join"
  # map = SyncInMemoryMap(Tspace::Key, Tspace::Value).new
  map = TermMap(Tspace::Key, Tspace::Value).new(CompactMLMap.new(KeyDigestMap(String, String).new(RemoteStringMap.new("127.0.0.1", 9810))))
  # chat = SyncInMemoryChat(Activation).new
  chat = TermChat(Activation).new(CompactMLChat.new(RemoteStringChat.new("127.0.0.1", 9811)))

  sink = ->(multisets : Term::Dict) do
    Tconn::Log.debug { ML.display(multisets) }
  end
  Tconn.open(map, chat, Tconn.multisets(&sink), keepalive: Tconn::Keepalive.new(period: 5.seconds..10.seconds)) do |conn|
    while input = (print "> "; gets)
      case input.strip
      when /^sensor\s+(\d+)\s+(.+)$/
        begin
          surface = Tconn::Sensor.new($2)
        rescue e : ML::SyntaxError
          puts "syntax error"
          next
        end
        conn[$1.to_u32] = surface
      when /^appearance\s+(\d+)\s+(.+)$/
        begin
          surface = Tconn::Appearance.new($2)
        rescue e : ML::SyntaxError
          puts "syntax error"
          next
        end
        conn[$1.to_u32] = surface
      when /^delete\s+(\d+)$/
        conn.delete($1.to_u32)
      else
        puts "invalid command: #{input}"
      end
    end

    # conn[0] = Tconn::Sensor.new(%{((%any div mod) a_number (%all b_number (%not 0)))})
    # conn[1] = Tconn::Appearance.new(Term.of(:div, 100, 200))
    # conn[2] = Tconn::Appearance.new(Term.of(:mod, 300, 400))
    # conn[2] = Tconn::Appearance.new(Term.of(:qux, 123))
    # conn[1] = Tconn::Sensor.new(%{(qux x_)})

    # conn.delete(0)
    # conn.delete(2)
    # conn.delete(1)
  end
end

# - send & compare selector as hash, never send plaintext selector
#   - at difficulty: easy selector is sent as (64-bit salt; sha256)
#   - at difficulty: medium selector is sent as (64-bit salt; sha512)
#   - at difficulty: hard selector is sent as argon2id (crypto secure hash)
# - improve Etrace/SensorMultimap ID storage efficiency by using some kind of a Patricia trie?
# - if map or chat connection is lost the Tconn must retire. Wrapping code should re-create
#   it with new id etc. for each attempt to reconnect. This should be invisible to clients.
# - rewrite the horrible horrible servers&clients. Have one server instead of two,
# either ditch my thing or the RPC. Preferably my thing but still.
