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

struct ::Ww::Term
  def self.encode(src : Label) : Term
    Term.of(src.value)
  end

  def self.decode?(dst : Label.class, term : Term) : Label?
    return unless num = term.as_n?
    return unless num.natural?
    return unless value = num.to?(UInt128)

    Label.new(value)
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

struct StringMapSetRPC
  include SimpleRpc::Proto

  @@map = SyncInMemoryMap(String, String).new
  @@set = SyncInMemorySet(String).new

  # The lamest way to do cleanup right here.
  spawn do
    while true
      select
      when timeout(30.seconds)
        @@map.clear
        @@set.clear
      end
    end
  end

  def latest(referrer : String, key : String) : String?
    @@map.latest?(Label.new(referrer.to_u128), key)
  end

  def size : Int32
    @@map.size
  end

  def ref(referrer : String, key : String, default : String) : String
    @@map.ref(Label.new(referrer.to_u128), key, default)
  end

  def unref(referrer : String, key : String) : Nil
    @@map.unref(Label.new(referrer.to_u128), key)
  end

  def includes(identity : String) : Bool
    @@set.includes?(identity)
  end

  def add(referrer : String, identity : String) : Nil
    @@set.add(Label.new(referrer.to_u128), identity)
  end

  def delete(referrer : String, identity : String) : Nil
    @@set.delete(Label.new(referrer.to_u128), identity)
  end
end

# A remote `IMap(String, String)` client using RPC for communication.
#
# `RemoteStringMap` connects to a remote string map service via `StringMapRPC::Client`.
class RemoteStringMapSet
  include ISet(String)
  include IMap(String, String)

  def initialize(host : String, port : Int32, *, pool_size = 50, pool_timeout = 1)
    @client = StringMapSetRPC::Client.new(host, port, mode: :pool, pool_size: pool_size, pool_timeout: pool_timeout)
  end

  def latest?(referrer : Label, key : String) : String?
    @client.latest!(referrer.value.to_s, key)
  end

  def size : Int32
    @client.size!
  end

  def ref(referrer : Label, key : String, default : String) : String
    @client.ref!(referrer.value.to_s, key, default)
  end

  def unref(referrer : Label, key : String) : Nil
    @client.unref!(referrer.value.to_s, key)
  end

  def includes?(identity : String) : Bool
    @client.includes!(identity)
  end

  def add(referrer : Label, identity : String) : Nil
    @client.add!(referrer.value.to_s, identity)
  end

  def delete(referrer : Label, identity : String) : Nil
    @client.delete!(referrer.value.to_s, identity)
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
      recv.call(Term.decode(M, message))
    end
  end

  def send(to receiver : Label, message : M) : Nil
    @chat.send(receiver, Term.encode(message))
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

struct Ttrie
  alias Key = Origin | Step

  record Origin
  record Step, pred : Label, base : Ubase::Any
  record Value, succ : Label

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
  end

  def mount(referrer : Label, strand : Enumerable(Term), endpoint : Label, deps : IDepSet) : Array(Label)
    tip = nil
    path = [] of Label

    mount = ->(key : Key) do
      value = @map.ref(referrer, key, Value.new(@fresh.call))
      deps.add(key, value)
      path << value.succ
    end

    mount.call(Origin.new)
    mount.call(Step.new(path.last, Ubase::Trunk.new))

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
    path
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

struct ::Ww::Term
  ENCODED_TTRIE_ORIGIN = Term.of(:ttrie, :key, :origin)

  def self.encode(src : Ttrie::Origin) : Term
    ENCODED_TTRIE_ORIGIN
  end

  def self.decode?(dst : Ttrie::Origin.class, term : Term) : Ttrie::Origin?
    term == ENCODED_TTRIE_ORIGIN ? Ttrie::Origin.new : nil
  end

  def self.encode(src : Ttrie::Step) : Term
    Term.of(:ttrie, :key, :step, encode(src.pred), encode(src.base))
  end

  def self.decode?(dst : Ttrie::Step.class, term : Term) : Ttrie::Step?
    matchpi?(term, %{(ttrie key step pred_ base_)}) do
      Ttrie::Step.new(decode?(Label, pred) || return, decode?(Ubase::Any, base) || return)
    end
  end

  def self.encode(src : Ttrie::Value) : Term
    Term.of(:ttrie, :value, :primary, encode(src.succ))
  end

  def self.decode?(dst : Ttrie::Value.class, term : Term) : Ttrie::Value?
    matchpi?(term, %{(ttrie value primary succ_)}) do
      Ttrie::Value.new(decode?(Label, succ) || return)
    end
  end
end

# TODO: create an implementor of IMap & ISet that is "buffered". We keep a buffer
# for adds and deletes in case of set, for ref! (buffered ref, returns nothing) &
# unref; flush on includes? for set and latest? for map. Also flush for map on
# unbuffered ref (ref). Use that as a temporary wrapper around the actual map in
# Tspace. Hard flush on quit when summon()ing or looking things up.

module EmergentLabelMultimap(Identity)
  # :nodoc:
  #
  # Sacrifice storage for less lookups (less guessing), because guessing is more
  # likely to go over the network & follows a "chain" order. Whereas with addition
  # we can simply batch-add everything and that's it.
  BASE = 2u128

  # :nodoc:
  BASE_DIGITS = {0u128, 1u128}

  # :nodoc:
  BASE_LENGTH_U128 = 128u8

  abstract def set : ISet(Identity)

  # Creates an association between *key* and *value* for the given *referrer*.
  def assign(referrer : Label, key : Label, value : Label, deps : IDepSet) : Nil
    value.each_prefix_with_index(base: BASE, max: BASE_LENGTH_U128) do |prefix, index|
      identity = Identity.new(key, prefix, index)
      set.add(referrer, identity)
      deps.add(identity)
    end
  end

  # TODO: what this method is doing appears to be "embarassingly parallel". Parallelize!
  # TODO: add error correction ± checksum
  def each_value(referrer : Label, key : Label, & : Label ->) : Nil
    queue = Deque{ {Label.zero, BASE_LENGTH_U128 - 1} }

    while entry = queue.shift?
      state, digitno = entry

      BASE_DIGITS.each do |choice|
        completion = state.complete(choice, base: BASE, index: digitno)

        next unless @set.includes?(Identity.new(key, completion, digitno))

        if digitno == 0
          yield completion
        else
          queue << {completion, digitno - 1}
        end
      end
    end
  end
end

struct Etrace
  record Identity, key : Label, state : Label, digitno : UInt8

  include EmergentLabelMultimap(Identity)

  def initialize(@fresh : LabelGenerator, @set : ISet(Identity))
  end

  private def set : ISet(Identity)
    @set
  end

  def mount(referrer : Label, path : Array(Label), deps : IDepSet)
    path.each_cons_pair do |u, v|
      assign(referrer, u, v, deps: deps)
    end

    deps
  end

  def walk(referrer : Label, origin : Label, &sink : Label ->) : Nil
    sink.call(origin)

    each_value(referrer, origin) do |successor|
      walk(referrer, successor, &sink)
    end
  end
end

struct ::Ww::Term
  def self.encode(src : Etrace::Identity) : Term
    Term.of(:etrace, :key, :primary, encode(src.key), encode(src.state), src.digitno)
  end

  def self.decode?(dst : Etrace::Identity.class, term : Term) : Etrace::Identity?
    matchpi?(term, %{(etrace key primary key_ state_ digitno←(%number u8))}) do
      Etrace::Identity.new(
        key: decode?(Label, key) || return,
        state: decode?(Label, state) || return,
        digitno: digitno.to(UInt8),
      )
    end
  end
end

struct StrandSet
  record Identity, vertex : Label

  def initialize(@set : ISet(Identity))
  end

  def mount(referrer : Label, vertex : Label, deps : IDepSet) : Nil
    key = Identity.new(vertex)

    @set.add(referrer, key)

    deps.add(key)
  end

  def strand?(referrer : Label, vertex : Label) : Bool
    @set.includes?(Identity.new(vertex))
  end
end

struct ::Ww::Term
  def self.encode(src : StrandSet::Identity) : Term
    Term.of(:"strand-set", :key, :primary, encode(src.vertex))
  end

  def self.decode?(dst : StrandSet::Identity.class, term : Term) : StrandSet::Identity?
    matchpi?(term, %{(strand-set key primary vertex_)}) do
      StrandSet::Identity.new(decode?(Label, vertex) || return)
    end
  end
end

struct AppearanceSet
  record Identity, vertex : Label

  def initialize(@set : ISet(Identity))
  end

  def mount(referrer : Label, vertex : Label, deps : IDepSet) : Nil
    key = Identity.new(vertex)

    @set.add(referrer, key)

    deps.add(key)
  end

  def appearance?(referrer : Label, vertex : Label) : Bool
    @set.includes?(Identity.new(vertex))
  end
end

struct ::Ww::Term
  def self.encode(src : AppearanceSet::Identity) : Term
    Term.of(:"appearance-set", :key, :primary, encode(src.vertex))
  end

  def self.decode?(dst : AppearanceSet::Identity.class, term : Term) : AppearanceSet::Identity?
    matchpi?(term, %{(appearance-set key primary vertex_)}) do
      AppearanceSet::Identity.new(decode?(Label, vertex) || return)
    end
  end
end

# One-to-many map for decoding a conjunction vertex into the sensors that
# were bound to it.
struct SensorMultimap
  record Identity, key : Label, state : Label, digitno : UInt8

  include EmergentLabelMultimap(Identity)

  def initialize(@fresh : LabelGenerator, @set : ISet(Identity))
  end

  private def set : ISet(Identity)
    @set
  end

  def mount(referrer : Label, conjv : Label, sensor : Label, deps : IDepSet) : Nil
    assign(referrer, conjv, sensor, deps)
  end

  def decode(referrer : Label, sensor : Label, &sink : Label ->)
    each_value(referrer, sensor, &sink)
  end
end

struct ::Ww::Term
  def self.encode(src : SensorMultimap::Identity) : Term
    Term.of(:"sensor-multimap", :key, :primary, encode(src.key), encode(src.state), src.digitno)
  end

  def self.decode?(dst : SensorMultimap::Identity.class, term : Term) : SensorMultimap::Identity?
    matchpi?(term, %{(sensor-multimap key primary key_ state_ digitno←(%number u8))}) do
      SensorMultimap::Identity.new(
        key: decode?(Label, key) || return,
        state: decode?(Label, state) || return,
        digitno: digitno.to(UInt8),
      )
    end
  end
end

record SensorData,
  conid : Label,
  groupid : Label,
  identity : Identity,
  instant : Label,
  selector : Term?

record AppearanceData,
  conid : Label,
  identity : Identity,
  instant : Label,
  value : Term,
  selector : Term?

struct ::Ww::Term
  def self.encode(src : SensorData) : Term
    Term.of(:"sensor-data", encode(src.conid), encode(src.groupid), src.identity, encode(src.instant), selector: src.selector)
  end

  def self.decode?(dst : SensorData.class, term : Term) : SensorData?
    matchpi?(term, %{(sensor-data conid_ groupid_ identity←(%number u32) instant_ ¦ (%keypool selector))}) do
      SensorData.new(
        conid: decode?(Label, conid) || return,
        groupid: decode?(Label, groupid) || return,
        identity: identity.to(Identity),
        instant: decode?(Label, instant) || return,
        selector: term[:selector]?,
      )
    end
  end

  def self.encode(src : AppearanceData) : Term
    Term.of(:"appearance-data", encode(src.conid), src.identity, encode(src.instant), src.value, selector: src.selector)
  end

  def self.decode?(dst : AppearanceData.class, term : Term) : AppearanceData?
    matchpi?(term, %{(appearance-data conid_ identity←(%number u32) instant_ value_ ¦ (%keypool selector))}) do
      AppearanceData.new(
        conid: decode?(Label, conid) || return,
        identity: identity.to(Identity),
        instant: decode?(Label, instant) || return,
        value: value,
        selector: term[:selector]?,
      )
    end
  end
end

struct SensorDataMap
  record Key, instant : Label
  record Value, data : SensorData

  def initialize(@map : IMap(Key, Value))
  end

  def mount(instant : Label, data : SensorData, deps : IDepSet) : Nil
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @map.ref(instant, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps.add(key, value0)
  end

  def query?(instant : Label) : SensorData?
    return unless value = @map.latest?(instant, Key.new(instant))

    value.data
  end
end

struct ::Ww::Term
  def self.encode(src : SensorDataMap::Key) : Term
    Term.of(:"sensor-data-map", :key, :primary, encode(src.instant))
  end

  def self.decode?(dst : SensorDataMap::Key.class, term : Term) : SensorDataMap::Key?
    matchpi?(term, %{(sensor-data-map key instant_)}) do
      SensorDataMap::Key.new(decode?(Label, scope) || return)
    end
  end

  def self.encode(src : SensorDataMap::Value) : Term
    Term.of(:"sensor-data-map", :value, :primary, encode(src.data))
  end

  def self.decode?(dst : SensorDataMap::Value.class, term : Term) : SensorDataMap::Value?
    matchpi?(term, %{(sensor-data-map value primary data_)}) do
      SensorDataMap::Value.new(decode?(SensorData, data) || return)
    end
  end
end

struct AppearanceDataMap
  record Key, instant : Label
  record Value, data : AppearanceData

  def initialize(@map : IMap(Key, Value))
  end

  def mount(instant : Label, data : AppearanceData, deps : IDepSet) : Nil
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @map.ref(instant, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps.add(key, value0)
  end

  def query?(instant : Label) : AppearanceData?
    return unless value = @map.latest?(instant, Key.new(instant))

    value.data
  end
end

struct ::Ww::Term
  def self.encode(src : AppearanceDataMap::Key) : Term
    Term.of(:"appearance-data-map", :key, :primary, encode(src.instant))
  end

  def self.decode?(dst : AppearanceDataMap::Key.class, term : Term) : AppearanceDataMap::Key?
    matchpi?(term, %{(appearance-data-map key instant_)}) do
      AppearanceDataMap::Key.new(decode?(Label, scope) || return)
    end
  end

  def self.encode(src : AppearanceDataMap::Value) : Term
    Term.of(:"appearance-data-map", :value, :primary, encode(src.data))
  end

  def self.decode?(dst : AppearanceDataMap::Value.class, term : Term) : AppearanceDataMap::Value?
    matchpi?(term, %{(appearance-data-map value primary data_)}) do
      AppearanceDataMap::Value.new(decode?(AppearanceData, data) || return)
    end
  end
end

alias Identity = UInt32

record Activation, kind : Kind, sdata : SensorData, adata : AppearanceData do
  enum Kind : UInt8
    StimulusPresence
    StimulusAbsence
  end
end

struct ::Ww::Term
  def self.encode(src : Activation) : Term
    Term.of(:activation, src.kind, encode(src.sdata), encode(src.adata))
  end

  def self.decode?(dst : Activation.class, term : Term) : Activation?
    matchpi?(term, %{(activation kind←(%number u8) sdata_ adata_)}) do
      Activation.new(
        kind: kind.to(Activation::Kind),
        sdata: decode?(SensorData, sdata) || return,
        adata: decode?(AppearanceData, adata) || return,
      )
    end
  end
end

class SurfaceNotMountedError < Exception
end

module IDepSet
end

struct DepSet(K, V, T)
  include IDepSet

  def initialize
    @entries = [] of {K, V}
    @identities = [] of T
  end

  def add(object : T) : Nil
    @identities << object
  end

  def add(key : K, value : V) : Nil
    @entries << {key, value}
  end

  def clear(referrer : Label, map : IMap(K, V), set : ISet(T)) : Nil
    @entries.each { |key, _| map.unref(referrer, key)  }
    @identities.each { |identity| set.delete(referrer, identity) }
    @entries.clear
    @identities.clear
  end
end

struct Tspace
  alias Key = Tbase::Key | SensorDataMap::Key | AppearanceDataMap::Key
  alias Value = Tbase::Value | SensorDataMap::Value | AppearanceDataMap::Value
  alias Identity = Tbase::Identity

  alias Dismiss = ->

  def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value), @set : ISet(Identity))
  end

  def tbase : Tbase
    Tbase.new(@fresh, @map.submap(Tbase::Key, Tbase::Value), @set)
  end

  def sensors : SensorDataMap
    SensorDataMap.new(@map.submap(SensorDataMap::Key, SensorDataMap::Value))
  end

  def appearances : AppearanceDataMap
    AppearanceDataMap.new(@map.submap(AppearanceDataMap::Key, AppearanceDataMap::Value))
  end

  def summon(sdata : SensorData, subject : Tbase::Sensor, *, seen : S = [] of AppearanceData) : {S, Dismiss} forall S
    deps = DepSet(Key, Value, Identity).new

    _ = sensors.mount(subject.id, sdata, deps: deps)
    _ = tbase.mount(subject, deps: deps)

    each_complement(subject, sdata.selector, &->seen.<<(AppearanceData))

    mounted = true

    dismiss = Dismiss.new do
      unless mounted
        raise SurfaceNotMountedError.new
      end

      mounted = false

      deps.clear(subject.id, @map, @set)

      nil
    end

    {seen, dismiss}
  end

  def summon(adata : AppearanceData, subject : Tbase::Appearance, &excite : SensorData ->) : Dismiss
    deps = DepSet(Key, Value, Identity).new

    _ = appearances.mount(subject.id, adata, deps: deps)
    _ = tbase.mount(subject, deps: deps)

    each_complement(subject, adata.selector, &excite)

    mounted = true

    Dismiss.new do
      unless mounted
        raise SurfaceNotMountedError.new
      end

      mounted = false

      deps.clear(subject.id, @map, @set)

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

# :nodoc:
#
# An object that manages keepalive & keepalive periods for `Tconn`.
struct Tconn::Keepalive
  alias Task = Task ->

  Log = ::Log.for("Tconn keepalive")

  # Used to specify which of the periods one wants to address in e.g.
  # `min_for`, `max_for`, `schedule`.
  enum Period : UInt8
    # Use the sensor period bounds.
    Sensor

    # Use the appearance period bounds.
    Appearance
  end

  @sensor_period : Range(Time::Span, Time::Span)
  @appearance_period : Range(Time::Span, Time::Span)

  def initialize(spec : KeepaliveSpec)
    @ctx = spec.ctx
    @sensor_period = spec.sensor_period
    @appearance_period = spec.appearance_period

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
  def schedule(task : Task, period : Period) : Channel(Bool)
    tmin = min_for(period).total_milliseconds
    tmax = max_for(period).total_milliseconds
    t = @prng_lock.synchronize { (tmin..tmax).sample(random: @prng) }.milliseconds

    Log.debug { "schedule task #{task} after #{t.total_seconds}s" }

    cancel = Channel(Bool).new

    @ctx.spawn do
      select
      when @running.receive? # nil
        Log.debug { "task #{task} canceled due to global close" }
      when cancel.receive?  # nil
        Log.debug { "task #{task} canceled due to targeted close" }
      when timeout(t)
        task.call(task)
      end
    end

    cancel
  end

  # :ditto:
  def schedule(period : Period, &task : Task ->) : Channel(Bool)
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

  # A blueprint for a `Tconn`.
  #
  # NOTE: *sink* may be called with the same `Overview` multiple times in a row;
  # it is your responsibility to suppress repetitions if necessary.
  record Spec,
    map : IMap(Tspace::Key, Tspace::Value),
    set : ISet(Tspace::Identity),
    chat : IChat(Activation),
    sink : Sink,
    fresh : LabelGenerator = WWID,
    keepalive : KeepaliveSpec? = nil

  struct Spec
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
  end

  # A blueprint for a `Tconn` keepalive.
  #
  # - Keepalive fibers are spawned in *ctx*.
  # - *sensor period* specifies minimum and maximum waiting time before
  #   running sensor keepalive.
  # - *appearance period* specifies minimum and maximum waiting time before
  #   running appearance keepalive.
  record KeepaliveSpec,
    ctx : ExecutionContext = ExecutionContext::SingleThreaded.new("Tconn keepalive"),
    sensor_period : Range(Time::Span, Time::Span) = DEFAULT_SENSOR_PERIOD,
    appearance_period : Range(Time::Span, Time::Span) = DEFAULT_APPEARANCE_PERIOD

  struct KeepaliveSpec
    # Period range used for sensors by default.
    DEFAULT_SENSOR_PERIOD = 40.seconds..2.minutes

    # Period range used for appearances by default.
    DEFAULT_APPEARANCE_PERIOD = 20.seconds..50.seconds

    # Initializes both sensor period and appearance period to *period*.
    def self.new(*args, period : Range(Time::Span, Time::Span), **kwargs)
      new(*args, **kwargs, sensor_period: period, appearance_period: period)
    end
  end

  alias Overview = Pf::Map(Identity, Tview)

  alias Sink = Overview ->
  alias Destructor = Bool ->

  record SurfaceData, instant : Label, destructors : Array(Destructor)

  @conid : Label

  @unsubscribe : IChat::Unsubscribe

  # FIXME: How can we make this non-reentrant??
  @surfaces_lock = Mutex.new(:reentrant)

  class ClosedError < Exception
  end

  @keepalive : Keepalive?

  def initialize(spec : Spec)
    # Extract ivars from spec.
    @map = spec.map
    @set = spec.set
    @chat = spec.chat
    @sink = spec.sink
    @fresh = spec.fresh

    if keepalive_spec = spec.keepalive
      @keepalive = Keepalive.new(keepalive_spec)
    end

    @conid = @fresh.call

    @overview = Overview.new
    @surfaces = {} of Identity => SurfaceData
    @cancel = {} of Identity => Channel(Bool)
    @last_alive_at = {} of Label => Time::Span

    @unsubscribe = @chat.subscribe(@conid) do |act|
      Log.debug { "#{@conid}: receive from chat: #{act}" }

      overview1 = @surfaces_lock.synchronize do
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
      end

      if overview1
        spec.sink.call(overview1)
      end
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
    Tspace.new(@fresh, @map, @set)
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

    @overview = @overview.dissoc(identity)

    if keepalive = @cancel.delete(identity)
      keepalive.close
    end

    Log.debug { "#{@conid}: removed surface #{identity}" }
  end

  def []=(identity : Identity, surface : Surface) : Nil
    groupid = @fresh.call

    ov1 = @surfaces_lock.synchronize do
      overview0 = @overview
      delete(identity, final: true)
      insert(identity, surface, groupid)
      overview1 = @overview
      overview0.same?(overview1) ? nil : overview1
    end

    if ov1
      @sink.call(ov1)
    end

    return unless keepalive = @keepalive

    case surface
    in Sensor
      period = Keepalive::Period::Sensor
    in Appearance
      period = Keepalive::Period::Appearance
    end

    cancel = keepalive.schedule(period) do |this|
      Log.debug { "#{@conid}: run keepalive of #{identity}" }

      ov4 = @surfaces_lock.synchronize do
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

        ov2 = @overview
        delete(identity, final: false)
        insert(identity, surface, groupid)
        ov3 = @overview
        keepalive.schedule(this, period)
        ov2.same?(ov3) ? nil : ov3
      end

      next unless ov4

      @sink.call(ov4)
    end

    @surfaces_lock.synchronize { @cancel[identity] = cancel }
  end

  def delete(identity : Identity) : Nil
    overview = @surfaces_lock.synchronize do
      overview0 = @overview
      delete(identity, final: true)
      overview1 = @overview
      overview0.same?(overview1) ? nil : overview1
    end

    return unless overview

    @sink.call(overview)
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
    @socket.flush
    unless @inbound.receive == "OK"
      raise "oh noes, something wrong happened on the chat server!!! i didnt get an OK"
    end
    Unsubscribe.new do
      unsub.call
      @socket.puts "-SUB #{address.value}"
      @socket.flush
      unless @inbound.receive == "OK"
        raise "oh noes, something wrong happened on the chat server!!! i didnt get an OK"
      end
    end
  end

  def send(to receiver : Label, message : String) : Nil
    @socket.puts "SEND #{receiver.value} #{message}"
    @socket.flush
    unless @inbound.receive == "OK"
      raise "oh noes, something wrong happened on the chat server!!! i didnt get an OK"
    end
  end
end

# struct EmergentStringMap
#   alias Atom = Value | Bytesize

#   record Value, key : String, byte_index : UInt32, state : UInt32
#   record Bytesize, key : String, byte_index : UInt32

#   def initialize(@set : ISet(Atom))
#   end

#   # TODO: include checksum/running hash in state
#   def latest?(key : String) : String?
#     String.build do |io|
#       byte_index = 0u32

#       until @set.includes?(Bytesize.new(key, byte_index))
#         byte_state = 0u32
#         byte_completed = false

#         (0u32...8u32).each do |bit_index|
#           {0u32, 1u32}.each do |bit|
#             completion = byte_state | (bit << bit_index)
#             next unless @set.includes?(Value.new(key, byte_index, completion))

#             byte_state = completion
#             byte_completed = true
#           end

#           break unless byte_completed
#         end

#         return if byte_state.zero?

#         io.write_byte(byte_state.to_u8)
#         byte_index += 1
#       end
#     end
#   end

#   # TODO: include checksum/running hash in state
#   def assign(referrer : Label, key : String, value : String, atoms : Array(Atom)) : Nil
#     # - We assume Unicode, encoded using UTF-8.
#     # - Each byte of UTF-8 is built up from smaller Value atoms, in Big Endian order.
#     (0...value.bytesize).each do |byte_index|
#       byte = value.byte_at(byte_index)
#       state = 0u32

#       (0...byte.bit_length).each do |bit_index|
#         digit = byte.bit(bit_index)
#         state |= digit << bit_index
#         atom = Value.new(key, byte_index.to_u32, state)
#         @set.add(referrer, atom)
#         atoms << atom
#       end
#     end

#     atom = Bytesize.new(key, value.bytesize.to_u32)
#     @set.add(referrer, atom)
#   end
# end

# struct ::Ww::Term
#   def self.encode(object : EmergentStringMap::Value)
#     Term.of(0, object.key, object.byte_index, object.state)
#   end

#   def self.encode(object : EmergentStringMap::Bytesize)
#     Term.of(1, object.key, object.byte_index)
#   end
# end

# class SetStringMap
#   include IMap(String, String)

#   def initialize(set : ISet(String))
#     set = TermSet(EmergentStringMap::Atom).new(CompactMLSet.new(DigestSet.new(set)))
#     @emap = EmergentStringMap.new(set)
#   end

#   def latest?(referrer : Label, key : String) : String?
#     @emap.latest?(key)
#   end

#   def size : Int32
#     -1
#   end

#   # "If value exists, return it; otherwise, assign to default"
#   def ref(referrer : Label, key : String, default : String) : String
#     @emap.put_if_absent(referrer, key, default, [] of EmergentStringMap::Atom)
#     # if value = @emap.value?(key)
#     #   return value
#     # end
#     # @emap.assign(referrer, key, default, [] of EmergentStringMap::Atom)
#     # default
#   end

#   def unref(referrer : Label, key : String) : Nil
#     # NOP (use atom array instead)
#   end
# end

# {% skip_file unless flag?(:surfmain) %}

# ref1 = WWID.call
# ref2 = WWID.call

# base_set = SyncInMemorySet(EmergentStringMap::Atom).new
# map = EmergentStringMap.new(base_set)
# map.put_if_absent(ref1, "John Doe", "0", [] of EmergentStringMap::Atom)
# pp map

# base_map = SetStringMap.new(base_set)
# set = TermSet(Tspace::Identity).new(CompactMLSet.new(DigestSet.new(base_set)))
# map = TermMap(Tspace::Key, Tspace::Value).new(CompactMLMap.new(base_map))

# # map.ref(ref1, "John Doe", "0")
# # map.ref(ref2, "Samantha Doe", "1")
# # pp map.latest?(ref1, "John Doe")
# # pp map.latest?(ref2, "Samantha Doe")
# # map.ref(ref1, "John Doe", "45")
# # map.ref(ref2, "John Doe", "50")

# # pp map

# # map.unref(ref1, "John Doe")

# # pp map
# # map.unref(ref2, "John Doe")
# # pp map
# chat = SyncInMemoryChat(Activation).new

# Tconn.open(set: set, map: map, sink: Tconn::Sink.new { |act| pp act }, chat: chat) do |conn|
#   conn[0] = Tconn::Appearance.new(Term.of(123))
#   conn[1] = Tconn::Sensor.new(Term.of(:x_number))
#   conn[0] = Tconn::Appearance.new(Term.of(456))
# end

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
      socket.flush
      unsub = chat.subscribe(Label.new(topic.to_u128)) do |message|
        socket.puts("MSG #{topic_} #{message}")
        socket.flush
      end
      unsubs_lock.synchronize do
        unsubs[{topic, socket}] = unsub
      end
    elsif topic = message.lchop?("-SUB ")
      socket.puts "OK" # assume unsubscribe cannot fail
      socket.flush
      if unsub = unsubs_lock.synchronize { unsubs.delete({topic, socket}) }
        unsub.call
      end
    elsif send = message.lchop?("SEND ")
      socket.puts "OK" # assume send cannot fail
      socket.flush
      topic, message = send.split(" ", limit: 2)
      chat.send(Label.new(topic.to_u128), message)
    elsif message == "LIST"
      socket.puts "LISTING"
      socket.flush
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
      socket.flush
    else
      socket.puts "ERR"
      socket.flush
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
  ctx.spawn { StringMapSetRPC::Server.new("127.0.0.1", 9810).run }
  ctx.spawn { serve_chat("127.0.0.1", 9811) }

  puts "Map on port 9810"
  puts "Chat on port 9811"
  sleep
elsif ARGV[0]? == "join"
  remote = RemoteStringMapSet.new("127.0.0.1", 9810)
  # map = SyncInMemoryMap(Tspace::Key, Tspace::Value).new
  map = TermMap(Tspace::Key, Tspace::Value).new(CompactMLMap.new(KeyDigestMap(String, String).new(remote)))
  # chat = SyncInMemoryChat(Activation).new
  chat = TermChat(Activation).new(CompactMLChat.new(RemoteStringChat.new("127.0.0.1", 9811)))
  set = TermSet(Tspace::Identity).new(CompactMLSet.new(DigestSet.new(remote)))

  sink = ->(multisets : Term::Dict) do
    Tconn::Log.debug { ML.display(multisets) }
  end
  Tconn.open(map, set, chat, Tconn::Spec.multisets(&sink), keepalive: Tconn::KeepaliveSpec.new(period: 5.seconds..10.seconds)) do |conn|
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
# - improve Etrace/SensorMultimap ID storage efficiency by using some kind of Patricia trie?
# - if map or chat connection is lost the Tconn must retire. Wrapping code should re-create
#   it with new id etc. for each attempt to reconnect. This should be invisible to clients.
# - rewrite the horrible horrible servers&clients. Have one server instead of two,
# either ditch my thing or the RPC. Preferably my thing but still.
