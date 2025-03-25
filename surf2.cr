require "digest/sha256"
require "./src/wirewright"
require "./surf_common"

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

module IStorage(K, V)
  # Returns the latest value of *key*, or `nil` if absent.
  #
  # "Latest" means "at the time of fetch". At the same time as the result of
  # the fetch (e.g. `nil`) is being returned to the caller, *key* might have
  # been added.
  abstract def latest?(key : K) : V?

  # Calls *fn* with the latest value of *key*, or `nil` if absent.
  #
  # This method is assumed to be the "promise" companion of `latest?`. That is,
  # as opposed to `latest?`, this method returns immediately, and *fn* is called
  # whenever the response is available, by another fiber.
  #
  # Whether this assumption holds depends on the implementation of `IStorage`,
  # but callers must use `latest` as if it always did.
  abstract def latest(key : K, &fn : V? ->) : Nil

  # Atomically registers a reference of *client* to *key*. If *key* is absent,
  # creates it and sets its value to *default*. Returns the cell value read
  # at the time of assignment.
  abstract def inc(client : Label, key : K, default : V) : V

  # Atomically removes *client*'s reference to *key*, removing the underlying
  # key-value pair if necessary.
  abstract def dec(client : Label, key : K) : Nil

  def submap(k : Sk.class, v : Sv.class) forall Sk, Sv
    Submap(K, V, Sk, Sv).new(self)
  end
end

struct Submap(K, V, Sk, Sv)
  include IStorage(Sk, Sv)

  def initialize(@map : IStorage(K, V))
  end

  def latest?(key : Sk) : Sv?
    @map.latest?(key.as(K)).as?(Sv)
  end

  def latest(key : Sk, &fn : Sv? ->) : Nil
    @map.latest(key.as(K)) do |value|
      fn.call(value.as?(Sv?))
    end
  end

  def inc(client : Label, key : Sk, default : Sv) : Sv
    @map.inc(client, key.as(K), default.as(V)).as(Sv)
  end

  def dec(client : Label, key : Sk) : Nil
    @map.dec(client, key.as(K))
  end
end

class MapStorage(K, V)
  include IStorage(K, V)

  record Cell(T), refs : Bag(Label), value : T

  @data = {} of K => Cell(V)

  def latest?(key : K) : V?
    return unless cell = @data[key]?

    cell.value
  end

  def latest(key : K, &fn : V? ->) : Nil
    fn.call(latest?(key))
  end

  def inc(client : Label, key : K, default : V) : V
    if cell = @data[key]?
      cell.refs.add(client)
      cell.value
    else
      @data[key] = Cell.new(Bag{client}, default)

      default
    end
  end

  def dec(client : Label, key : K) : Nil
    return unless cell = @data[key]?
    return unless cell.refs.delete?(client)
    return unless cell.refs.empty?

    @data.delete(key)
  end

  def pretty_print(pp)
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

# Forwards `K`'s digest as key to the underlying map.
class DigestedKey(K, V)
  include IStorage(K, V)

  def initialize(@map : IStorage(String, V))
  end

  def latest?(key : K) : V?
    @map.latest?(key.digest).as?(V)
  end

  def latest(key : K, &fn : V? ->) : Nil
    @map.latest(key.digest) do |value|
      fn.call(value.as?(V?))
    end
  end

  def inc(client : Label, key : K, default : V) : V
    @map.inc(client, key.digest, default)
  end

  def dec(client : Label, key : K) : Nil
    @map.dec(client, key.digest)
  end
end

struct Utrie
  alias Key = Origin | Step

  record Origin, base : Ubase::Any do
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "utrie origin #{base}"
      end
    end
  end

  record Step, pred : Label, base : Ubase::Any do
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "utrie step #{pred} #{base}"
      end
    end
  end

  record Value, succ : Label

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  # Mounts a *strand* of `Ubase`s for *client*. Returns a set of seen pairs
  # (called *dependencies*; used for removal or maintenance) and the id of
  # the endpoint thus reached.
  def mount(client : Label, strand : Strand, *, deps = Bag({Key, Value}).new)
    key = Origin.new(strand[0])
    origin = @storage.inc(client, key, Value.new(@fresh.call))
    pred = origin.succ
    deps << {key, origin}

    strand[1..].each do |base|
      key = Step.new(pred, base)
      step = @storage.inc(client, key, Value.new(@fresh.call))
      pred = step.succ
      deps << {key, step}
    end

    {deps, pred}
  end

  private def successor?(key : Key) : Label?
    @storage.latest?(key).try(&.succ)
  end

  {% for type, base in {Term::Num => Ubase::IsNum, Term::Str => Ubase::IsStr, Term::Sym => Ubase::IsSym, Term::Boolean => Ubase::IsBool} %}
    private def query(pred : Label, term : {{type}}, sink : Label ->) : Nil
      return unless succ0 = successor?(Step.new(pred, {{base}}.new))

      sink.call(succ0)

      if succ1 = successor?(Step.new(succ0, Ubase::Literal.new(Term.of(term))))
        sink.call(succ1)
      end
    end
  {% end %}

  # NOTE: dictionaries must be normalized into IsDict - At(), even literal ones.
  # We do not handle Literal(dict).
  private def query(pred : Label, term : Term::Dict, sink : Label ->) : Nil
    return unless succ0 = successor?(Step.new(pred, Ubase::IsDict.new))

    sink.call(succ0)

    term.each_entry do |key, value|
      next unless succ1 = successor?(Step.new(succ0, Ubase::At.new(key)))

      sink.call(succ1)

      query(succ1, value.downcast, sink)
    end
  end

  private def query(term : Term, sink) : Nil
    return unless succ = successor?(Origin.new(Ubase::IsAny.new))

    sink.call(succ)

    query(succ, term.downcast, sink)
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
  def query(term : Term, &sink : Label ->) : Nil
    query(term, sink)
  end
end

struct Xgraph
  record Key, a : Label, b : Label do
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "xgraph key #{a} #{b}"
      end
    end
  end

  record Value, succ : Label

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def mount(client : Label, xrule : Deque(Label), *, deps = Bag({Key, Value}).new)
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      key = Key.new(a, b)
      value = @storage.inc(client, key, Value.new(@fresh.call))
      deps << {key, value}

      xrule << value.succ
    end

    {deps, xrule[0]}
  end

  private def conjs(vertices : Deque(Label), sink : Label ->)
    while a = vertices.shift?
      sink.call(a)

      (0...vertices.size).each do |i|
        b = vertices.unsafe_fetch(i)
        next unless value = @storage.latest?(Key.new(a, b))

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
  def conjs(vertices : Deque(Label), &sink : Label ->)
    conjs(vertices, sink)
  end
end

struct Ttrie
  alias Key = Origin | Step

  record Origin do
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "ttrie origin"
      end
    end
  end

  record Step, pred : Label, base : Ubase::Any do
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "ttrie step #{pred} #{base}"
      end
    end
  end

  record Value, succ : Label

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def mount(client : Label, strand : Enumerable(Term), endpoint : Label, *, deps = Bag({Key, Value}).new)
    tip = nil
    path = [] of Label

    mount = ->(key : Key) do
      value = @storage.inc(client, key, Value.new(@fresh.call))
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

  def query?(strand : Enumerable(Ubase::Any)) : Label?
    return unless origin = @storage.latest?(Origin.new)

    pred = origin.succ

    strand.each do |base|
      # The strand embedded in @storage must be >= the query strand.
      return unless value = @storage.latest?(Step.new(pred, base))

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
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "etrace key #{scope} #{state} #{digitno}"
      end
    end
  end

  record Value

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def mount(client : Label, scope : Label, succ : Label, *, deps = Bag({Key, Value}).new)
    succ.each_prefix_with_index(base: BASE, max: BASE_LENGTH_U128) do |prefix, index|
      key = Key.new(scope, prefix, index)
      value = @storage.inc(client, key, Value.new)
      deps << {key, value}
    end

    deps
  end

  def mount(client : Label, path : Array(Label), *, deps = Bag({Key, Value}).new)
    path.each_cons_pair do |u, v|
      _ = mount(client, u, v, deps: deps)
    end

    deps
  end

  # TODO: what this method is doing appears to be "embarassingly parallel". Parallelize!
  def each_successor(scope : Label, &sink : Label ->) : Nil
    queue = Deque{ {Label.zero, BASE_LENGTH_U128 - 1} }

    while entry = queue.shift?
      state, digitno = entry

      BASE_DIGITS.each do |choice|
        completion = state.complete(choice, base: BASE, index: digitno)

        next unless @storage.latest?(Key.new(scope, completion, digitno))

        if digitno == 0
          sink.call(completion)
        else
          queue << {completion, digitno - 1}
        end
      end
    end
  end

  def walk(origin : Label, &sink : Label ->) : Nil
    sink.call(origin)

    each_successor(origin) { |successor| walk(successor, &sink) }
  end
end

# etrace = Etrace.new(WWID, MapStorage(Etrace::Key, Etrace::Value).new)
# etrace.mount(100u128, 1000u128, 123u128)
# etrace.mount(100u128, 1000u128, 456u128)
# etrace.mount(100u128, 3000u128, 789u128)

# etrace.each_successor(3000u128) do |succ|
#   pp succ
# end

# {% skip_file %}

struct StrandSet
  record Key, vertex : Label do
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "strand set key #{vertex}"
      end
    end
  end

  record Value

  def initialize(@storage : IStorage(Key, Value))
  end

  def mount(client : Label, vertex : Label, *, deps = Bag({Key, Value}).new)
    key = Key.new(vertex)
    value = @storage.inc(client, key, Value.new)
    deps << {key, value}
    deps
  end

  def strand?(vertex : Label) : Bool
    !!@storage.latest?(Key.new(vertex))
  end
end

struct AppearanceSet
  record Key, vertex : Label do
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "appearance set key #{vertex}"
      end
    end
  end

  record Value

  def initialize(@storage : IStorage(Key, Value))
  end

  def mount(client : Label, vertex : Label, *, deps = Bag({Key, Value}).new)
    key = Key.new(vertex)
    value = @storage.inc(client, key, Value.new)
    deps << {key, value}
    deps
  end

  def appearance?(vertex : Label) : Bool
    !!@storage.latest?(Key.new(vertex))
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
    def digest : String
      Digest::SHA256.base64digest do |ctx|
        ctx.update "sensor decoder vertex #{scope} #{state} #{digitno}"
      end
    end
  end

  record Value

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def mount(client : Label, scope : Label, succ : Label, *, deps = Bag({Key, Value}).new)
    succ.each_prefix_with_index(base: BASE, max: BASE_LENGTH_U128) do |prefix, index|
      key = Key.new(scope, prefix, index)
      value = @storage.inc(client, key, Value.new)
      deps << {key, value}
    end

    deps
  end

  # TODO: what this method is doing appears to be "embarassingly parallel". Parallelize!
  def each_successor(scope : Label, &sink : Label ->) : Nil
    queue = Deque{ {Label.zero, BASE_LENGTH_U128 - 1} }

    while entry = queue.shift?
      state, digitno = entry

      BASE_DIGITS.each do |choice|
        completion = state.complete(choice, base: BASE, index: digitno)

        next unless @storage.latest?(Key.new(scope, completion, digitno))

        if digitno == 0
          sink.call(completion)
        else
          queue << {completion, digitno - 1}
        end
      end
    end
  end

  def decode(sensor : Label, &sink : Label ->)
    each_successor(sensor, &sink)
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

  alias Subject = Sensor | Appearance

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def utrie : Utrie
    Utrie.new(@fresh, @storage.submap(Utrie::Key, Utrie::Value))
  end

  def ttrie : Ttrie
    Ttrie.new(@fresh, @storage.submap(Ttrie::Key, Ttrie::Value))
  end

  def etrace : Etrace
    Etrace.new(@fresh, @storage.submap(Etrace::Key, Etrace::Value))
  end

  def strands : StrandSet
    StrandSet.new(@storage.submap(StrandSet::Key, StrandSet::Value))
  end

  def xgraph : Xgraph
    Xgraph.new(@fresh, @storage.submap(Xgraph::Key, Xgraph::Value))
  end

  def sensors : SensorDecoder
    SensorDecoder.new(@fresh, @storage.submap(SensorDecoder::Key, SensorDecoder::Value))
  end

  def appearances : AppearanceSet
    AppearanceSet.new(@storage.submap(AppearanceSet::Key, AppearanceSet::Value))
  end

  def mount(client : Label, subject : Sensor, *, deps = Bag({Key, Value}).new)
    rule = Deque(Label).new

    subject.strands.each do |strand|
      _, endpoint = utrie.mount(client, strand, deps: deps)

      # Endpoint points to the end of the utrie strand. We need to register
      # endpoint as a strand.
      _ = strands.mount(client, endpoint, deps: deps)

      rule << endpoint
    end

    # Pre-sort ascending as the Xgraph requires.
    rule.unstable_sort!

    # Mount the rule in the Xgraph.
    _, conjv = xgraph.mount(client, rule, deps: deps)

    # Subscribe the id to the conjunction vertex. This acts as a point-of-commitment,
    # the instant registration finishes the sensor is public.
    _ = sensors.mount(client, conjv, subject.id, deps: deps)

    deps
  end

  def mount(client : Label, subject : Appearance, *, deps = Bag({Key, Value}).new)
    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)

      # Mount the appearance into the Ttrie, saving the path to `subject.id`
      # (ids of nodes which lie along the path).
      _, path = ttrie.mount(client, keypath, subject.id, deps: deps)

      keypath.pop

      # Mount `path` in Etrace for iteration at query-time.
      _ = etrace.mount(client, path, deps: deps)

      true # continue
    end

    # Register `subject.id` as that of an appearance. This acts as a point-of-
    # commitment, the instant registration finishes the sensor is public.
    _ = appearances.mount(client, subject.id, deps: deps)

    deps
  end

  def each_complement(subject : Sensor, *, successors : Bool, &sink : Label ->) : Nil
    sets = [] of Set(Label)

    subject.strands.each do |strand|
      next unless endpoint = ttrie.query?(strand)

      hits = Set(Label).new

      etrace.walk(endpoint) do |candidate|
        next unless appearances.appearance?(candidate)
        next if !successors && candidate > subject.id

        hits << candidate
      end

      return if hits.empty?

      sets << hits
    end

    return if sets.empty?

    sets.unstable_sort_by!(&.size)
    sets[0].each do |candidate|
      next unless (1...sets.size).all? { |index| candidate.in?(sets[index]) }

      # Send candidates that are in all sets (match all strands of the sensor)
      # to the sink.
      sink.call(candidate)
    end
  end

  def each_complement(subject : Appearance, *, successors : Bool, &sink : Label ->) : Nil
    hits = Deque(Label).new

    # Find out which Utrie vertices are activated by the subject.
    utrie.query(subject.value) do |hit|
      # Keep only strand vertices.
      next unless strands.strand?(hit)

      hits << hit
    end

    # Pre-sort ascending as the Xgraph requires.
    hits.unstable_sort!

    # Find out which conjunctions are activated by the subject.
    xgraph.conjs(hits) do |conjv|
      # Filter proper (decodable) sensor vertices.
      sensors.decode(conjv) do |candidate|
        next if !successors && candidate > subject.id

        # Send matching candidates to sink.
        sink.call(candidate)
      end
    end
  end
end

record Sensor, id : Label, strands : StrandList do
  include Tbase::Sensor

  # Calls *fn* with each sensor in *pattern*.
  #
  # An arbitrary M1 *pattern* can contain branches (e.g. `%any`) so it is considered
  # to contain multiple sensors.
  def self.each(fresh : LabelGenerator, pattern : Term, &fn : Sensor ->) : Nil
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

record Appearance, id : Label, value : Term do
  include Tbase::Appearance
end

record SensorData,
  client : Label,
  instant : Label,
  identity : UInt32,
  address : String,
  selector : Term?

record AppearanceData,
  client : Label,
  instant : Label,
  identity : UInt32,
  value : Term,
  selector : Term?,
  tombstone : Term?

struct SensorBase
  record Key, instant : Label
  record Value, data : SensorData

  def initialize(@storage : IStorage(Key, Value))
  end

  def mount(client : Label, instant : Label, data : SensorData, *, deps = Bag({Key, Value}).new)
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @storage.inc(client, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps << {key, value0}
    deps
  end

  def query?(instant : Label) : SensorData?
    return unless value = @storage.latest?(Key.new(instant))

    value.data
  end
end

struct AppearanceBase
  record Key, instant : Label
  record Value, data : AppearanceData

  def initialize(@storage : IStorage(Key, Value))
  end

  def mount(client : Label, instant : Label, data : AppearanceData, *, deps = Bag({Key, Value}).new)
    key = Key.new(instant)
    value0 = Value.new(data)
    value1 = @storage.inc(client, key, value0)
    unless value0 == value1
      raise ArgumentError.new("instant is not unique")
    end

    deps << {key, value0}
    deps
  end

  def query?(instant : Label) : AppearanceData?
    return unless value = @storage.latest?(Key.new(instant))

    value.data
  end
end

alias Activation = StimulusPresence | StimulusAbsence | SensorAbsence

record StimulusPresence, recv_client : Label, recv_instant : Label, instant : Label, identity : UInt32, value : Term
# FIXME: how are we going to send these in a distributed/decentralized setting ?! Who will be sending these?!
record StimulusAbsence, instant : Label, client : Label, identity : UInt32, tombstone : Term?
# FIXME: how are we going to send these in a distributed/decentralized setting ?! Who will be sending these?!
record SensorAbsence, instant : Label

struct Tspace
  alias Address = String

  alias Key = Tbase::Key | SensorBase::Key | AppearanceBase::Key
  alias Value = Tbase::Value | SensorBase::Value | AppearanceBase::Value

  # TODO: this is clearly misplaced!
  class Surface
    @mounted = true

    # :nodoc:
    def initialize(
      @fresh : LabelGenerator,
      @storage : IStorage(Key, Value),
      @client : Label,
      @subject : Tbase::Subject,
      @deps : Bag({Key, Value}),
    )
    end

    # Calls *sink* with surfaces complementary to `self`. That is, if `self`
    # is a sensor, calls *sink* with ids of appearances that `self` is excited
    # by; and if `self` is an appearance, calls *sink* with ids of sensors that
    # `self` excites.
    #
    # If *successors* is true, *sink* is called with surfaces that succeed `self`
    # in time. That is, a sensor will be able to be excited by appearances from
    # the future, and appearances from the past will be able to excite sensors
    # in the future.
    #
    # This is normally not desired: we prefer surfaces to only excite or be excited
    # by their predecessors.
    def each_complement(*, successors = false, &sink : Label ->) : Nil
      unless @mounted
        raise SurfaceNotMountedError.new
      end

      tbase = Tbase.new(@fresh, @storage.submap(Tbase::Key, Tbase::Value))
      tbase.each_complement(@subject, successors: successors, &sink)
    end

    # Collects the results of `each_complement` into an array.
    def complement(**kwargs) : Array(Label)
      complement = [] of Label
      each_complement(**kwargs) do |label|
        complement << label
      end
      complement
    end

    # Tears down the surface. You will no longer be able to call `unmount`
    # and `refresh`. This is the graceful way to unmount; the underlying map
    # does not depend on clients unmounting gracefully.
    def unmount : Nil
      unless @mounted
        raise SurfaceNotMountedError.new
      end

      @mounted = false
      @deps.each { |(key, _)| @storage.dec(@client, key) }
    end

    # Remounts the surface with randomization, blocking for *duration*.
    #
    # Remounting is a solution to deformations of the underlying map (random pairs
    # will get removed at random times in practice if we consider a distributed,
    # decentralized map). We also assume the underlying map implements timed deletion
    # (pairs "evaporate" after a certain amount of time). Thus remounting becomes not
    # only a way to ensure the surface is fully in the map throughout deformations;
    # but also as a way to prolong the lifetime of the surface (prevent deletion
    # of its constituent pairs).
    def remount(duration = 1.minute) : Nil
      unless @mounted
        raise SurfaceNotMountedError.new
      end

      raise "not implemented"
    end

    def inspect(io)
      case @subject
      in Tbase::Sensor
        io << "Sensor"
      in Tbase::Appearance
        io << "Appearance"
      end

      io << "("
      io << "id=" << @subject.id
      io << ", client=" << @client
      io << ", popcount=" << @deps.ntotal
      io << ", mounted=" << @mounted
      io << ")"
    end
  end

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def tbase : Tbase
    Tbase.new(@fresh, @storage.submap(Tbase::Key, Tbase::Value))
  end

  def sensors : SensorBase
    SensorBase.new(@storage.submap(SensorBase::Key, SensorBase::Value))
  end

  def appearances : AppearanceBase
    AppearanceBase.new(@storage.submap(AppearanceBase::Key, AppearanceBase::Value))
  end

  def summon(
    client : Label,
    subject : Sensor,
    identity : UInt32,
    address : Address,
    selector : Term? = nil,
    &sink : Address, Activation ->
  ) : Surface
    sdata = SensorData.new(client, subject.id, identity, address, selector)
    deps = Bag({Key, Value}).new

    _ = sensors.mount(client, subject.id, sdata, deps: deps)
    _ = tbase.mount(client, subject, deps: deps)

    surface = Surface.new(@fresh, @storage, client, subject, deps)
    surface.each_complement do |appearance|
      next unless adata = appearances.query?(appearance)
      next unless sdata.selector == adata.selector

      act = StimulusPresence.new(sdata.client, sdata.instant, adata.instant, adata.identity, adata.value)

      sink.call(sdata.address, act)
    end

    surface
  end

  def summon(
    client : Label,
    subject : Appearance,
    identity : UInt32,
    selector : Term? = nil,
    tombstone : Term? = nil,
    &sink : Address, Activation ->
  ) : Surface
    adata = AppearanceData.new(client, subject.id, identity, subject.value, selector, tombstone)
    deps = Bag({Key, Value}).new

    _ = appearances.mount(client, subject.id, adata, deps: deps)
    _ = tbase.mount(client, subject, deps: deps)

    surface = Surface.new(@fresh, @storage, client, subject, deps)
    surface.each_complement do |sensor|
      next unless sdata = sensors.query?(sensor)
      next unless sdata.selector == adata.selector

      act = StimulusPresence.new(sdata.client, sdata.instant, adata.instant, adata.identity, adata.value)

      sink.call(sdata.address, act)
    end

    surface
  end

  # TODO: dismiss
end

# TODO: analyze whether healing with Bag of deps actually works in a distributed setting!!!
# I'm getting the feeling it doesn't but I need to check! Important to remember that refcounts
# are stored on buckets so it's all or none (buckets don't DECREF randomly but can disappear
# and reappear randomly! the latter is probably the most problematic!)
# TODO: implement serialization for all Values
# TODO: use something RPC like to test out on a remote map. Or maybe use Redis
# or something like that!

notify = ->(address : Tspace::Address, act : Activation) do
  puts "Send #{act} to #{address}"
end
fresh = WWID
storage = MapStorage(Tspace::Key, Tspace::Value).new
tspace = Tspace.new(fresh, storage)

client0 = fresh.call
client1 = fresh.call
client2 = fresh.call

n = 0
Sensor.each(fresh, ML.term %{(div a_number b_number)}) do |sensor|
  s = tspace.summon(client0, sensor, 0u32, "address(sensor-#{n})", &notify)
  n += 1
  pp s
end

ap1 = tspace.summon(client1, Appearance.new(fresh.call, Term.of(:div, 100, 200)), 0u32, &notify)
ap2 = tspace.summon(client2, Appearance.new(fresh.call, Term.of(:mod, 100, 200)), 0u32, &notify)
pp ap1
pp ap2

Sensor.each(fresh, ML.term %{(mod a_number b_number)}) do |sensor|
  s = tspace.summon(client0, sensor, 0u32, "address(sensor-#{n})", &notify)
  n += 1
  pp s
end

{% skip_file %}

fresh = WWID
# storage = SerializedValue(Tbase::Key, Tbase::Value).new(DigestedKey(Tbase::Key, String).new(MapStorage(String, String).new))
# storage = DigestedKey(Tbase::Key, Tbase::Value).new(MapStorage(String, Tbase::Value).new)
storage = MapStorage(Tbase::Key | Tspace::Key, Tbase::Value | Tspace::Value).new
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

storage = MapStorage(Utrie::Key | Xgraph::Key | Ttrie::Key | Etrace::Key | StrandSet::Key | AppearanceSet::Key | SensorDecoder::Key, Utrie::Value | Xgraph::Value | Ttrie::Value | Etrace::Value | StrandSet::Value | AppearanceSet::Value | SensorDecoder::Value).new

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

