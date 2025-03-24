require "./src/wirewright"
require "./surf_common"

alias Label = UInt128
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

    (order << 64) | randomness
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
end

struct Utrie
  alias Key = Origin | Step

  record Origin, base : Ubase::Any
  record Step, pred : Label, base : Ubase::Any

  record Value, succ : Label

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  # Mounts a *strand* of `Ubase`s for *client*. Returns a set of seen pairs
  # (called *dependencies*; used for removal or maintenance) and the id of
  # the endpoint thus reached.
  def mount(client : Label, strand : Strand) : {Set({Key, Value}), Label}
    deps = Set({Key, Value}).new

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
  record Key, a : Label, b : Label
  record Value, succ : Label

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def mount(client : Label, xrule : Deque(Label)) : {Set({Key, Value}), Label}
    if xrule.empty?
      raise ArgumentError.new
    end

    deps = Set({Key, Value}).new

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

  record Origin
  record Step, pred : Label, base : Ubase::Any

  record Value, succ : Label

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def mount(client : Label, strand : Enumerable(Term), endpoint : Label) : {Set({Key, Value}), Array(Label)}
    tip = nil
    path = [] of Label
    deps = Set({Key, Value}).new

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
  # so 128 entries per id) but very little guessing (you're basically asking the network,
  # "yes or no?") The benefit of larger bases diminishes quickly while increasing guesswork.
  BASE = 4u128

  # :nodoc:
  BASE_DIGITS = {0u128, 1u128, 2u128, 3u128}

  # :nodoc:
  BASE_LENGTH_U128 = 64u8

  record Key, scope : Label, state : Label, digitno : UInt8
  record Value

  def initialize(@fresh : LabelGenerator, @storage : IStorage(Key, Value))
  end

  def mount(client : Label, scope : Label, succ : Label) : Set({Key, Value})
    deps = Set({Key, Value}).new
    state = 0u128

    # NOTE: since we're using UUIDs (ish) as opposed to a counter, we don't
    # have the "long zeros prefix" problem where we store lots of zeros redundantly.
    # So we don't have to think about using a variable length encoding.
    (0...BASE_LENGTH_U128).reverse_each do |index|
      digit = (succ // (BASE ** index)) % BASE
      state &+= digit &* BASE**index
      key = Key.new(scope, state, index.to_u8)
      value = @storage.inc(client, key, Value.new)
      deps << {key, value}
    end

    deps
  end

  def mount(client : Label, path : Array(Label)) : Set({Key, Value})
    deps = Set({Key, Value}).new

    path.each_cons_pair do |u, v|
      subdeps = mount(client, u, v)
      deps.concat(subdeps)
    end

    deps
  end

  private def digit(scope : Label, state : Label, digitno : UInt8, &sink : Label ->) : Nil
    BASE_DIGITS.each do |choice|
      completion = state &+ (choice &* BASE**digitno)

      # This is assumed to create concurrent requests if the underlying map
      # client supports such a thing.
      @storage.latest(Key.new(scope, completion, digitno)) do |exists|
        if exists
          sink.call(completion)
        end
      end
    end
  end

  private def label(scope : Label, state : Label, digitno : UInt8, &sink : Label ->) : Nil
    digit(scope, state, digitno) do |completion|
      if digitno == 0
        sink.call(completion)
      else
        label(scope, completion, digitno - 1, &sink)
      end
    end
  end

  def each_successor(scope : Label, &sink : Label ->) : Nil
    label(scope, state: 0u128, digitno: BASE_LENGTH_U128 - 1, &sink)
  end

  def walk(origin : Label, &sink : Label ->) : Nil
    sink.call(origin)

    each_successor(origin) { |successor| walk(successor, &sink) }
  end
end

# StrandSet

# AppearanceSet

# SensorEncoder

# SensorDecoder

# Tbase

# Tspace

# fresh = WWID
# storage = MapStorage(Etrace::Key, Etrace::Value).new

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

# {% skip_file %}

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

storage = MapStorage(Utrie::Key | Xgraph::Key | Ttrie::Key | Etrace::Key, Utrie::Value | Xgraph::Value | Ttrie::Value | Etrace::Value).new

utrie = Utrie.new(fresh, storage.submap(Utrie::Key, Utrie::Value))

pp sensor[0]

rule = Deque(Label).new
deps = Set({Utrie::Key | Xgraph::Key | Ttrie::Key | Etrace::Key, Utrie::Value | Xgraph::Value | Ttrie::Value | Etrace::Value}).new

sensor[0].each do |strand|
  subdeps, endpoint = utrie.mount(client0, strand)
  rule << endpoint
  deps.concat(subdeps)
end

xgraph = Xgraph.new(fresh, storage.submap(Xgraph::Key, Xgraph::Value))

rule.unstable_sort!

subdeps, rulepoint = xgraph.mount(client0, rule)
deps.concat(subdeps)

pp! rulepoint
pp! deps

# ---
found = Deque(Label).new
utrie.query(Term.of(:mod, 100, 200)) do |label|
  found << label
end
found.unstable_sort!
xgraph.conjs(found) do |conj|
  pp! conj
end

# ----

ttrie = Ttrie.new(fresh, storage.submap(Ttrie::Key, Ttrie::Value))
etrace = Etrace.new(fresh, storage.submap(Etrace::Key, Etrace::Value))

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
puts "Appearance is #{id.to_s(32, precision: 26)}"

pp storage.@data.size

# pp ttrie.query?({Ubase::IsAny.new})
endpoint = ttrie.query?({Ubase::IsAny.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:mod))})

if endpoint
  etrace.walk(endpoint) do |hit|
    puts "Hit #{hit.to_s(32, precision: 26)}"
  end
end

