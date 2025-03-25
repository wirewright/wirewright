require "./src/wirewright"
require "./surf_common"

module ExtrinsicMap(K, V)
  class Transaction(V)
    property! value : V?

    def initialize(@value)
    end

    def set(value : V) : Nil
      @value = value
    end

    def del? : V?
      value0, @value = @value, nil
      value0
    end

    def del : V
      del? || raise KeyError.new
    end
  end

  abstract def get?(key : K) : V?
  abstract def transaction(key : K, & : Transaction(V) -> T) forall T

  def set(key : K, value : V)
    transaction(key, &.set(value))
  end

  def del?(key : K) : V?
    transaction(key, &.del?)
  end

  def del(key : K) : V
    del?(key) || raise KeyError.new
  end

  def ref(key : K, &zero : -> V) : V
    zerov = nil

    transaction(key) do |tx|
      value0 = tx.value?
      value1 = value0 ? value0 : (zerov ||= yield)
      value1 = value1.incref
      tx.set(value1)
      value1
    end
  end

  def unref(key : K) : V
    transaction(key) do |tx|
      value1, zero = tx.value.decref?
      zero ? tx.del : tx.set(value1)
      value1
    end
  end
end

struct HashMap(K, V)
  include ExtrinsicMap(K, V)

  def initialize
    @map = {} of K => V
  end

  def get?(key : K) : V?
    @map[key]?
  end

  def transaction(key : K, & : Transaction(V) -> T) : T forall T
    value0 = @map[key]?
    tx = Transaction(V).new(value0)
    result = yield tx
    if value1 = tx.value?
      @map[key] = value1
    else
      @map.delete(key)
    end
    result
  end
end

class AtomicMap(K, V)
  include ExtrinsicMap(K, V)

  def initialize
    @map = Atomic(Pf::MapBox(K, V)).new(Pf::MapBox(K, V).new)
  end

  def get?(key : K) : V?
    @map.get(:relaxed)[key]?
  end

  def transaction(key : K, & : Transaction(V) -> T) : T forall T
    map0 = @map.get(:relaxed)

    while true
      value0 = map0[key]?

      tx = Transaction(V).new(value0)

      result = yield tx

      if value1 = tx.value?
        map1 = map0.assoc(key, value1)
      else
        map1 = map0.dissoc(key)
      end

      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)
      if ok
        return result
      end
    end
  end
end

struct Submap(K, V, Ks, Vs)
  include ExtrinsicMap(K, V)

  def initialize(@map : ExtrinsicMap(Ks, Vs))
  end

  def get?(key : K) : V?
    @map.get?(key.as(Ks)).as(V?)
  end

  def transaction(key : K, & : Transaction(V) -> T) : T forall T
    @map.transaction(key.as(Ks)) do |tx0|
      tx1 = Transaction(V).new(tx0.value?.as(V?))
      result = yield tx1
      tx0.value = tx1.value?.as(Vs?)
      result
    end
  end
end

struct Tbase
  # Union type for all keys that a Tbase might store in the data map.
  module Key
  end

  # Union type for all values that a Tbase might store in the data map.
  module Value
  end
end

macro def_incref_and_decref(field = refcount)
  def incref
    copy_with({{field.id}}: {{field.id}} + 1)
  end

  def decref?
    {copy_with({{field.id}}: {{field.id}} - 1), {{field.id}} == 1}
  end
end

macro def_unit(kind, *args, refcounting = nil)
  record {{args.splat}} do
    include {{kind}}

    {% if refcounting == true %}
      def_incref_and_decref
    {% elsif refcounting %}
      def_incref_and_decref {{refcounting}}
    {% end %}
  end
end

alias Label = UInt64
alias Refcount = UInt32

VERTEX_NONE = Label.new(0)
VERTEX_ROOT = VERTEX_NONE + 1
VERTEX_ZERO = VERTEX_ROOT + 1

struct Utrie
  def_unit Tbase::Key, Node, pred : Label, base : Ubase::Any
  def_unit Tbase::Value, Props, refcount : Refcount, successor : Label, refcounting: true

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Node, Props))
  end

  def mount(pred : Label, base : Ubase::Any) : Label
    props = @data.ref(Node.new(pred, base)) { Props.new(0u32, @fresh.call) }
    props.successor
  end

  def mount(strand : Enumerable(T), & : T -> Ubase::Any) : Label forall T
    strand.reduce(VERTEX_ROOT) { |pred, base| mount(pred, yield base) }
  end

  def mount(strand : Enumerable(Ubase::Any)) : Label
    mount(strand, &.itself)
  end

  # NOTE: the caller guarantees that *base* was mounted with *pred* before.
  def unmount(pred : Label, base : Ubase::Any) : {Label, Bool}
    props = @data.unref(Node.new(pred, base))

    {props.successor, props.refcount.zero?}
  end

  def unmount(strand : Enumerable(T), & : T -> Ubase::Any) : {Label, Bool} forall T
    removed0 = false
    vertex = strand.reduce(VERTEX_ROOT) do |pred, base|
      succ, removed = unmount(pred, yield base)
      # Removal along the path would mean obstruction of the rest of the path
      # where (assuming all refcounts are proper) we have all refcounts = 1.
      removed0 ||= removed
      succ
    end

    {vertex, removed0}
  end

  def unmount(strand : Enumerable(Ubase::Any)) : {Label, Bool}
    unmount(strand, &.itself)
  end

  private def successor?(node : Node) : Label?
    props = @data.get?(node)
    props ? props.successor : nil
  end

  {% for type, base in {Term::Num => Ubase::IsNum, Term::Str => Ubase::IsStr, Term::Sym => Ubase::IsSym, Term::Boolean => Ubase::IsBool} %}
    private def query(pred : Label, term : {{type}}, sink : Label ->) : Nil
      return unless successor0 = successor?(Node.new(pred, {{base}}.new))

      sink.call(successor0)

      if successor1 = successor?(Node.new(successor0, Ubase::Literal.new(Term.of(term))))
        sink.call(successor1)
      end
    end
  {% end %}

  # NOTE: dictionaries must be normalized into IsDict - At(), even literal ones.
  # We do not handle Literal(dict).
  private def query(pred : Label, term : Term::Dict, sink : Label ->) : Nil
    return unless successor0 = successor?(Node.new(pred, Ubase::IsDict.new))

    sink.call(successor0)

    term.each_entry do |key, value|
      next unless successor1 = successor?(Node.new(successor0, Ubase::At.new(key)))

      sink.call(successor1)

      query(successor1, value.downcast, sink)
    end
  end

  private def query(term : Term, sink : Label ->) : Nil
    return unless successor = successor?(Node.new(VERTEX_ROOT, Ubase::IsAny.new))

    sink.call(successor)

    query(successor, term.downcast, sink)
  end

  def query(term : Term, &sink : Label ->) : Nil
    query(term, sink)
  end
end

struct Xgraph
  def_unit Tbase::Key, Node, a : Label, b : Label
  def_unit Tbase::Value, Props, refcount : Refcount, successor : Label, refcounting: true

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Node, Props))
  end

  # Mounts an Xgraph rule *xrule*.
  #
  # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
  # by passing it to this method.
  def mount(xrule : Deque(Label))
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      props = @data.ref(Node.new(a, b)) { Props.new(0u32, @fresh.call) }

      xrule << props.successor
    end

    xrule[0]
  end

  # Unmounts an Xgraph rule *xrule*.
  #
  # Returns the vertex of the rule that was unmounted (so that the caller
  # perhaps deletes it in its own data structures).
  #
  # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
  # by passing it to this method.
  #
  # WARNING: the caller guarantees that *xrule* was mounted.
  def unmount(xrule : Deque(Label)) : Label
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      props = @data.unref(Node.new(a, b))

      xrule << props.successor
    end

    xrule[0]
  end

  # :nodoc:
  def conjs(vertices : Deque(Label), sink : Label ->)
    while a = vertices.shift?
      sink.call(a)

      (0...vertices.size).each do |i|
        b = vertices.unsafe_fetch(i)
        next unless props = @data.get?(Node.new(a, b)) || @data.get?(Node.new(b, a))

        vertices << props.successor
      end
    end
  end

  # Calls *sink* with all mounted conjunction vertices in *vertices*.
  #
  # NOTE: *vertices* must be pre-sorted ascending. You lose ownership of *vertices*
  # by passing it to this method.
  def conjs(vertices : Deque(Label), &sink : Label ->)
    conjs(vertices, sink)
  end
end

struct Ttrie
  def_unit Tbase::Key, Node, pred : Label, base : Ubase::Any
  def_unit Tbase::Value, Props, refcount : Refcount, successor : Label, refcounting: true

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Node, Props))
  end

  def mount(pred : Label, base : Ubase::Any) : Label
    props = @data.ref(Node.new(pred, base)) { Props.new(0u32, @fresh.call) }
    props.successor
  end

  # NOTE: the caller guarantees that *base* was mounted with *pred* before.
  def unmount(pred : Label, base : Ubase::Any) : Label
    props = @data.unref(Node.new(pred, base))
    props.successor
  end

  # Mounts *strand*.
  def mount(strand : Enumerable(Term), endpoint : Label) : Slice(Label)
    tip = nil
    path = [VERTEX_ROOT, mount(VERTEX_ROOT, Ubase::IsAny.new)]

    strand.each do |term|
      if tip
        path << mount(path.last, Ubase::IsDict.new)
        path << mount(path.last, Ubase::At.new(tip))
      end

      tip = term
    end

    if tip
      path << mount(path.last, Ubase.from(tip.type))
      path << mount(path.last, Ubase::Literal.new(tip))
    end

    path << endpoint
    path.to_readonly_slice
  end

  # Unmounts *strand*.
  #
  # Returns the path to its endpoint.
  def unmount(strand : Enumerable(Term), endpoint : Label) : Slice(Label)
    tip = nil

    path = [VERTEX_ROOT, unmount(VERTEX_ROOT, Ubase::IsAny.new)]

    strand.each do |term|
      if tip
        path << unmount(path.last, Ubase::IsDict.new)
        path << unmount(path.last, Ubase::At.new(tip))
      end

      tip = term
    end

    if tip
      path << unmount(path.last, Ubase.from(tip.type))
      path << unmount(path.last, Ubase::Literal.new(tip))
    end

    path << endpoint
    path.to_readonly_slice
  end

  # Calls *fn* with the set of endpoints at the end of *strand*.
  def query?(strand : Enumerable(Ubase::Any)) : {Label, Label}?
    pred0 = VERTEX_ROOT
    pred1 = VERTEX_ROOT

    strand.each do |base|
      # The strand embedded in @data must be >= the query strand.
      return unless props = @data.get?(Node.new(pred1, base))

      pred0 = pred1
      pred1 = props.successor
    end

    {pred0, pred1}
  end
end

struct Etrace
  module Key
    include Tbase::Key
  end

  module Value
    include Tbase::Value
  end

  def_unit Key, Node, pred : Label, vertex : Label
  def_unit Value, Props, refcount : Refcount, oid : Label, refcounting: true
  def_unit Key, Size, oid : Label
  def_unit Key, Item, oid : Label, index : Label
  def_unit Key, Membership, oid : Label, vertex : Label
  def_unit Value, Identity
  def_unit Value, SizeValue, value : Refcount, refcounting: value
  def_unit Value, ItemValue, vertex : Label

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Key, Value))
  end

  private def oidmap
    Submap(Node, Props, Key, Value).new(@data)
  end

  private def sizes
    Submap(Size, SizeValue, Key, Value).new(@data)
  end

  private def items
    Submap(Item, ItemValue, Key, Value).new(@data)
  end

  private def members
    Submap(Membership, Identity, Key, Value).new(@data)
  end

  def mount(path : Slice(Label))
    if path.size < 2
      raise ArgumentError.new
    end

    if path[0] == VERTEX_ROOT
      path = path[1..]
    end

    pred = VERTEX_ROOT

    # Create nodes for each step and incref. This way we'll make sure they're
    # not removed by someone else while we're working at them later on.
    oids = path.map do |step|
      node = Node.new(pred, step)
      pred = step
      props = oidmap.ref(node) { Props.new(0u32, @fresh.call) }
      props.oid
    end

    # If we succeed in adding a Membership, then we're responsible for
    # incrementing successor count and inserting into the Item.
    (0...path.size - 1).each do |index|
      oid = oids[index]

      successor = Membership.new(oid, w = path[index + 1])
      next if @data.get?(successor)

      @data.set(successor, Identity.new)

      count1 = sizes.ref(Size.new(oid)) { SizeValue.new(0u32) }

      @data.set(Item.new(oid, count1.value - 1), ItemValue.new(w))
    end
  end

  def unmount(path : Slice(Label))
    if path.size < 2
      raise ArgumentError.new
    end

    if path[0] == VERTEX_ROOT
      path = path[1..]
    end

    pred = VERTEX_ROOT

    oids = path.compact_map do |step|
      props = oidmap.unref(Node.new(pred, step))
      pred = step

      # Keep only oids which we've removed. We're responsible for their
      # cleanup then.
      props.refcount.zero? ? props.oid : nil
    end

    oids.each do |oid|
      # Successor list may not necessarily exist for endpoint vertices.
      # Deletion may fail, and we're fine with that.
      next unless count = sizes.del?(Size.new(oid))

      (0u32...count.value).each do |index|
        successor = items.del(Item.new(oid, index))
        members.del(Membership.new(oid, successor.vertex))
      end
    end
  end

  def walk(u : Label, v : Label, &fn : Label ->)
    fn.call(v)

    return unless props = oidmap.get?(Node.new(u, v))

    # NOTE: while walking, the node along with its attributes could get deleted/
    # be in the process of being deleted. So at any point where we're reading
    # from the map, we must handle the absence-case, even if it seems like it
    # is impossible.
    return unless count = sizes.get?(Size.new(props.oid))

    (0u32...count.value).each do |index|
      # We're fine with gaps. They could happen under some successor count
      # increment + successor list insert orderings. We're bounded anyway.
      next unless w = items.get?(Item.new(props.oid, index))

      walk(v, w.vertex, &fn)
    end
  end
end

# One-to-many map for decoding a conjunction vertex into the sensors that
# were bound to it.
struct SensorDecoder
  def_unit Tbase::Key, Node, conjv : Label, id : Label
  def_unit Tbase::Value, Props, sensor : Label, succ : Label, active : Bool

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Node, Props))
  end

  # Registers *sensor* as one of decodings of *conjv*.
  #
  # Worst-case O(N) relative to the highest-ever number of decodings of *conjv*.
  #
  # NOTE: binds and unbinds leave "tombstones" for *conjv*-*sensor* combos.
  # Make sure to `burn` *conjv* when you're sure it's never going to be used
  # again to not leak memory.
  #
  # NOTE: the caller guarantees *conjv* was never bound to *sensor* before.
  def bind(conjv : Label, sensor : Label) : Nil
    id = VERTEX_NONE

    running = true

    while running
      @data.transaction(Node.new(conjv, id)) do |tx|
        props0 = tx.value?

        if props0.nil?
          # If head is free we insert immediately and finish. Note how
          # we have to allocate the successor here.
          running = false

          tx.set Props.new(sensor, @fresh.call, active: true)
        elsif !props0.active
          # If head is inactive we replace its now-defunct sensor with
          # the bound sensor, activate, and finish.
          running = false

          tx.set props0.copy_with(sensor: sensor, active: true)
        else
          # Otherwise we proceed to the successor node.
          id = props0.succ
        end
      end
    end
  end

  # Unregisters *sensor* from being one of the decodings of *conjv*.
  #
  # Worst-case O(N) relative to the highest-ever number of decodings of *conjv*.
  #
  # NOTE: binds and unbinds leave "tombstones" for *conjv*-*sensor* combos.
  # Make sure to `burn` *conjv* when you're sure it's never going to be used
  # again to not leak memory.
  #
  # NOTE: the caller guarantees *conjv* was bound to *sensor* before (`bind`).
  def unbind(conjv : Label, sensor : Label) : Nil
    id = VERTEX_NONE

    running = true

    while running
      @data.transaction(Node.new(conjv, id)) do |tx|
        props0 = tx.value

        if props0.sensor == sensor
          # Deactivate the sensor if we've found a match.
          running = false
          tx.set(props0.copy_with(active: false))
          next
        end

        # Otherwise we proceed to the successor node.
        id = props0.succ
      end
    end
  end

  # NOTE: this method "burns" *conjv*; the caller guarantees that *conjv*
  # will never be passed to `bind` or `unbind` again.
  def burn(conjv : Label) : Nil
    id = VERTEX_NONE

    keys = [] of Node

    while props0 = @data.get?(Node.new(conjv, id))
      keys << Node.new(conjv, id)
      id = props0.succ
    end

    keys.each { |key| @data.del(key) }
  end

  # Yields all decodings associated with *conjv*.
  def decode(conjv : Label, & : Label ->) : Nil
    id = VERTEX_NONE

    while props0 = @data.get?(Node.new(conjv, id))
      if props0.active
        yield props0.sensor
      end
      id = props0.succ
    end
  end
end

# Tbase (short for *termbase*, whatever that is supposed to mean) is an internal
# object responsible for orchestrating objects that are even more internal; such
# as `Utrie`, `Xgraph`, `Ttrie`, and so on.
#
# This "orchestration" results in the emergence of *sensors* and *appearances*,
# conceptually grouped into *surfaces*; but througout the operation of Tbase
# referred to as *subjects*. With the help of Tbase, you can manipulate them;
# but be prepared to make *a ton* of guarantees -- breakage thereof risks
# the scary undefined behavior!
#
# `Tspace` wraps `Tbase` and exists to ensure all guarantees are satisfied (along
# with some other things).
#
# The notable thing about `Tbase` is that it only needs a single hash map (or,
# rather, an implementor of `ExtrinsicMap`). It is designed this way to defer
# synchronicity and distrubition to the map; and therefore make their presence
# user-configurable. Tbase simply does not care. As long as the map is thread-safe,
# Tbase is thread-safe; as long as the map is distributed, Tbase is distributed.
#
# NOTE: sometimes we mention that the caller must provide a certain "ownership"
# guarantee. What this means is that the caller must uniquely own the subject
# it provides to the method it calls. No other process in the world must be able
# to do anything with that subject, and by giving the subject to the callee, the caller
# is temporarily transfering the ownership. This can generally be achieved through
# the use of unique ids -- either GUIDs (e.g. Snowflake, since we require ordering
# guarantees) or a simple Tspace-bound `LabelGenerator`.
struct Tbase
  # Defines the requirements that a Tbase has to a "sensor" object.
  module Sensor
    # Returns a Tspace-unique id of this sensor, that was obtained from
    # a time-ordered source.
    abstract def id : Label

    # Returns the list of strands that this sensor is comprised of.
    abstract def strands : StrandList
  end

  # Defines the requirements that a Tbase has to an "appearance" object.
  module Appearance
    # Returns a Tspace-unique id of this appearance, that was obtained from
    # an increasing source.
    abstract def id : Label

    # Returns the term that is this appearance's value.
    abstract def value : Term
  end

  # :nodoc:
  module ConjvRef
    def_unit Key, Node, vertex : Label
    def_unit Value, Props, refcount : Refcount, refcounting: true
  end

  # :nodoc:
  def_unit Key, StrandVertex, vertex : Label
  # :nodoc:
  def_unit Key, AppearanceVertex, vertex : Label
  # :nodoc:
  def_unit Value, Identity

  # :nodoc:
  module SensorEncoder
    def_unit Key, Node, sensor : Label
    def_unit Value, Props, conjv : Label
  end

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Key, Value))
  end

  private def udata
    Submap(Utrie::Node, Utrie::Props, Key, Value).new(@data)
  end

  private def xdata
    Submap(Xgraph::Node, Xgraph::Props, Key, Value).new(@data)
  end

  private def tdata
    Submap(Ttrie::Node, Ttrie::Props, Key, Value).new(@data)
  end

  private def edata
    Submap(Etrace::Key, Etrace::Value, Key, Value).new(@data)
  end

  private def strands
    Submap(StrandVertex, Identity, Key, Value).new(@data)
  end

  private def conjvrefs
    Submap(ConjvRef::Node, ConjvRef::Props, Key, Value).new(@data)
  end

  private def sensor_encode
    Submap(SensorEncoder::Node, SensorEncoder::Props, Key, Value).new(@data)
  end

  private def sensor_decode
    Submap(SensorDecoder::Node, SensorDecoder::Props, Key, Value).new(@data)
  end

  private def appearances
    Submap(AppearanceVertex, Identity, Key, Value).new(@data)
  end

  # Adds a sensor *subject* to this Tbase. The instant this method returns,
  # the caller must be capable of reacting to queries resulting in *subject*
  # coming from other threads. This is usually achieved through registering
  # a queue for *subject* before calling this method, which helps to preserve
  # the messages for further reading.
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a time-ordered source;
  # - that *subject*'s id was never in use before.
  #
  # Behavior is undefined if these guarantees are broken. Breaking of these
  # guarantees must be handled at a higher level.
  def mount(subject : Sensor) : Nil
    utrie = Utrie.new(@fresh, udata)
    xgraph = Xgraph.new(@fresh, xdata)

    conj = Deque(Label).new

    subject.strands.each do |strand|
      uvertex = utrie.mount(strand)
      strands.set(StrandVertex.new(uvertex), Identity.new)
      conj << uvertex
    end

    conj.unstable_sort!
    conjv = xgraph.mount(conj)

    # "Publish" the sensor.
    #
    # NOTE: decode assignment MUST be done last because it serves as THE indication
    # of commitment. After the decode transaction finishes, the sensor becomes
    # reachable via querying.
    sensor_encode.set(
      SensorEncoder::Node.new(subject.id),
      SensorEncoder::Props.new(conjv),
    )

    conjvrefs.ref(ConjvRef::Node.new(conjv)) { ConjvRef::Props.new(0) }

    sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
    sensor_decoder.bind(conjv, subject.id)
  end

  # Deletes a sensor *subject* from this Tbase.
  #
  # NOTE: the caller guarantees that it mounted *subject* under the guarantees
  # given in `mount`. Behavior is undefined otherwise.
  #
  # NOTE: the caller must expect the visibility of *subject* to peers until this
  # method returns. Therefore, the caller is expected to somehow "blacklist"
  # *subject* on its end before calling this method, to ensure that *subject* is
  # unreachable through queries while this method is doing its work.
  def unmount(subject : Sensor) : Nil
    # "Unpublish" the sensor. We will need to know its conjunction vertex
    # first though.
    conjv = sensor_encode.del(SensorEncoder::Node.new(subject.id)).conjv

    sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
    sensor_decoder.unbind(conjv, subject.id)

    utrie = Utrie.new(@fresh, udata)
    xgraph = Xgraph.new(@fresh, xdata)

    conj = Deque(Label).new

    subject.strands.each do |strand|
      strand_vertex, removed = utrie.unmount(strand)
      if removed
        strands.del(StrandVertex.new(strand_vertex))
      end
      conj << strand_vertex
    end

    conj.unstable_sort!

    expect xgraph.unmount(conj) == conjv

    conjvref = conjvrefs.unref(ConjvRef::Node.new(conjv))

    return unless conjvref.refcount.zero?

    # If we've reached this point, `conjv` will never be used again. Thus we burn
    # the associated sensor encodings to avoid leaking memory.
    sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
    sensor_decoder.burn(conjv)
  end

  # Adds an appearance *subject* to this Tbase. The instant this method returns,
  # the caller must be capable of reacting to queries resulting in *subject*
  # coming from other threads. This is usually achieved through registering
  # a queue for *subject* before calling this method, which helps to preserve
  # the messages for further reading.
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a time-ordered source;
  # - that *subject*'s id was never in use before.
  #
  # Behavior is undefined if these guarantees are broken. Breaking of these
  # guarantees must be handled at a higher level.
  def mount(subject : Appearance) : Nil
    ttrie = Ttrie.new(@fresh, tdata)
    etrace = Etrace.new(@fresh, edata)

    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)
      path = ttrie.mount(keypath, subject.id)
      keypath.pop

      etrace.mount(path)

      true # Continue
    end

    # "Publish" the appearance
    appearances.set(AppearanceVertex.new(subject.id), Identity.new)
  end

  # Deletes an appearance *subject* from this Tbase.
  #
  # NOTE: the caller guarantees that it mounted *subject* under the guarantees
  # given in `mount`. Behavior is undefined otherwise.
  def unmount(subject : Appearance) : Nil
    # "Unpublish" the appearance. Since we're using subject ids we can do
    # that immediately.
    appearances.del(AppearanceVertex.new(subject.id))

    ttrie = Ttrie.new(@fresh, tdata)
    etrace = Etrace.new(@fresh, edata)

    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)
      path = ttrie.unmount(keypath, subject.id)
      keypath.pop

      etrace.unmount(path)

      true # Continue
    end
  end

  # Calls *fn* with appearance subject ids that the *subject* sensor matches.
  #
  # If *only_preds* is set to `true`, emits only subject ids that are older
  # than *subject* (that existed before *subject* was created).
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a time-ordered source;
  # - that it considers appearance subject ids given to *fn* immediately outdated.
  #   The caller must ensure that all actions taken upon them first check (or only
  #   proceed provided) the existence of the corresponding appearance.
  def query(subject : Sensor, *, only_preds = true, &fn : Label ->) : Nil
    ttrie = Ttrie.new(@fresh, tdata)
    etrace = Etrace.new(@fresh, edata)

    sets = [] of Set(Label)

    subject.strands.each do |strand|
      next unless edge = ttrie.query?(strand)

      hits = Set(Label).new
      sets << hits

      # NOTE: This walk is done asynchronously -- etrace is not driven by
      # a unified clock.
      etrace.walk(*edge) do |candidate|
        # Ensure candidate is a fully added appearance.
        next unless appearances.get?(AppearanceVertex.new(candidate))
        next if only_preds && subject.id <= candidate

        hits << candidate
      end
    end

    return if sets.empty?

    sets.unstable_sort_by!(&.size)
    sets[0].each do |subject_id|
      next unless (1...sets.size).all? { |index| subject_id.in?(sets[index]) }

      fn.call(subject_id)
    end
  end

  # Calls *fn* with sensor subject ids that the *subject* sensor matches.
  #
  # If *only_preds* is set to `true`, emits only subject ids that are older
  # than *subject* (that existed before *subject* was created).
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a time-ordered source;
  # - that it considers sensor subject ids given to *fn* immediately outdated.
  #   The caller must ensure that all actions taken upon them first check (or only
  #   proceed provided) the existence of the corresponding sensor.
  def query(subject : Appearance, *, only_preds = true, &fn : Label ->) : Nil
    utrie = Utrie.new(@fresh, udata)
    xgraph = Xgraph.new(@fresh, xdata)

    hits = Deque(Label).new

    utrie.query(subject.value) do |hit|
      # Ensure the vertex hit is a fully added strand.
      next unless strands.get?(StrandVertex.new(hit))

      hits << hit
    end

    hits.unstable_sort!

    xgraph.conjs(hits) do |candidate|
      # Ensure the vertex hit is a fully added sensor whose subject
      # id we know.
      sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
      sensor_decoder.decode(candidate) do |candidate_subject_id|
        next if only_preds && subject.id <= candidate_subject_id

        fn.call(candidate_subject_id)
      end
    end
  end
end

class Tspace
  def initialize(@fresh : LabelGenerator, @tbase : Tbase)
    @senders = AtomicMap(Label, {Label, Label, Term?, Term}).new
    @receivers = AtomicMap(Label, {Label, Term?, (Activation ->)}).new
  end

  def bind(outbox, subject : Sensor, identity identity0 : Label, selector selector0 : Term?, callback : Activation ->)
    @receivers.set(subject.id, {identity0, selector0, callback})

    @tbase.mount(subject)
    @tbase.query(subject) do |pred|
      next unless sender = @senders.get?(pred)

      trigger, identity1, selector1, value = sender

      next unless selector0 == selector1

      outbox[pred] = callback.partial(StimulusPresence.new(identity0, trigger, identity1, pred, value))
    end
  end

  def unbind(subject : Sensor)
    @receivers.del(subject.id)
    @tbase.unmount(subject)
  end

  def bind(outbox, subject : Appearance, trigger : Label, identity : Label, selector selector0 : Term?)
    @senders.set(subject.id, {trigger, identity, selector0, subject.value})

    @tbase.mount(subject)
    @tbase.query(subject) do |pred|
      next unless receiver = @receivers.get?(pred)

      sensor, selector1, callback = receiver

      next unless selector0 == selector1

      outbox[pred] = callback.partial(StimulusPresence.new(sensor, trigger, identity, subject.id, subject.value))
    end
  end

  def unbind(subject : Appearance)
    @senders.del(subject.id)
    @tbase.unmount(subject)
  end

  def depart(outbox, subject : Appearance, message : Term?, selector selector0 : Term?, trigger : Label, identity : Label)
    @tbase.query(subject, only_preds: false) do |pred|
      next unless receiver = @receivers.get?(pred)

      sensor, selector1, callback = receiver

      next unless selector0 == selector1

      outbox[pred] = callback.partial(StimulusAbsence.new(sensor, trigger, identity, subject.id, message))
    end
  end
end

alias Activation = StimulusPresence | StimulusAbsence | SensorAbsence

record StimulusPresence, sensor : Label, trigger : Label, identity : Label, instant : Label, value : Term

# NOTE: in `StimulusAbsence`, the *farewell* term does not necessarily
# match the receiver sensor's pattern. They are given for reference. If the receiver
# can handle it, they should. Otherwise they may handle the absence itself.
record StimulusAbsence, sensor : Label, trigger : Label, identity : Label, instant : Label, farewell : Term?

record SensorAbsence, sensor : Label

class Tconn
  alias SurfaceData = SensorData | AppearanceData

  record SensorData, identity : Label, instant : Label, pattern : StrandList, selector : Term?
  record AppearanceData, identity : Label, instant : Label, value : Term, selector : Term?, tombstone : Term?

  record SensorMemberData, instant : Label, pattern : StrandList
  record SensorGroupData, identity : Label, members : Slice(SensorMemberData), selector : Term?

  @conid : Label

  def initialize(@fresh : LabelGenerator, @tspace : Tspace, @callback : Activation ->)
    @conid = @fresh.call
    @sensors = {} of Label => SensorGroupData
    @appearances = {} of Label => AppearanceData
  end

  def initialize(fresh, tspace, &callback : Activation ->)
    initialize(fresh, tspace, callback)
  end

  private def summon(outbox, surface : SensorData)
    @tspace.bind(outbox,
      subject: Sensor.new(surface.instant, surface.pattern),
      selector: surface.selector,
      identity: surface.identity,
      callback: @callback,
    )
  end

  private def summon(outbox, data : SensorGroupData)
    data.members.each do |member|
      summon(outbox, SensorData.new(data.identity, member.instant, member.pattern, data.selector))
    end
  end

  private def dismiss(data : SensorData)
    @tspace.unbind(Sensor.new(data.instant, data.pattern))
  end

  private def dismiss(data : SensorGroupData)
    data.members.each do |member|
      dismiss(SensorData.new(data.identity, member.instant, member.pattern, data.selector))
    end
  end

  private def summon(outbox, data : AppearanceData)
    @tspace.bind(outbox,
      subject: Appearance.new(data.instant, data.value),
      selector: data.selector,
      trigger: @conid,
      identity: data.identity,
    )
  end

  private def dismiss(outbox, data : AppearanceData)
    subject = Appearance.new(data.instant, data.value)

    @tspace.unbind(subject)
    @tspace.depart(outbox,
      subject: subject,
      message: data.tombstone,
      selector: data.selector,
      trigger: @conid,
      identity: data.identity,
    )
  end

  private def changes?(data : AppearanceData, value : Term, selector : Term?, tombstone : Term?)
    {data.value, data.selector, data.tombstone} != {value, selector, tombstone}
  end

  private def changes?(data : SensorGroupData, pattern : BranchList, selector : Term?)
    return true unless data.members.size == pattern.size
    return true unless data.selector == selector

    # - Members in data and branches in pattern are unordered.
    # - Most often when this code is reached, both will have size=1.
    pattern.all? do |branch|
      data.members.any? { |member| member.pattern == branch }
    end
  end

  # :nodoc:
  #
  # Replaces the surface at *identity* with a sensor group matching the given
  # *pattern* branch list.
  def add_sensor(identity : Label, *, pattern : BranchList, selector : Term?) : Nil
    # Implicit assumptions:
    #   - We never notify the same vertex more than one time in summon() nor dismiss().
    #   - Crystal hash tables are ordered.
    outbox = {} of Label => ->

    if surface0 = @appearances[identity]?
      dismiss(outbox, surface0)
    elsif surface0 = @sensors[identity]?
      return unless changes?(surface0, pattern, selector)

      dismiss(surface0)
    end

    members = pattern.to_readonly_slice do |branch|
      SensorMemberData.new(@fresh.call, branch)
    end

    surface1 = SensorGroupData.new(identity, members, selector)

    @sensors[identity] = surface1

    summon(outbox, surface1)

    outbox.each { |_, act| act.call }
  end

  def add_sensor(identity : Label, *, pattern : Term, selector : Term?) : Nil
    skeleton = pipe(pattern, M1.normal, M1.skeleton)

    strands = [] of Strand
    branches = [] of StrandList

    M1.branches(skeleton) do |branch|
      M1.strands(branch) do |strand|
        strands << strand.items.to_readonly_slice { |base| Ubase.parse(base) }
      end
      branches << strands.to_readonly_slice(&.itself)
      strands.clear
    end

    # Arrays may over-allocate so we make an additional copy with to_readonly_slice
    # to possibly free the over-allocation.
    add_sensor(identity, pattern: branches.to_readonly_slice(&.itself), selector: selector)
  end

  def add_appearance(identity : Label, *, value : Term, selector : Term?, tombstone : Term?) : Nil
    # Implicit assumptions:
    #   - We never notify the same vertex more than one time in summon() nor dismiss().
    #   - Crystal hash tables are ordered.
    outbox = {} of Label => ->

    if surface0 = @sensors[identity]?
      dismiss(surface0)
    elsif surface0 = @appearances[identity]?
      return unless changes?(surface0, value, selector, tombstone)

      dismiss(outbox, surface0)
    end

    surface1 = AppearanceData.new(identity, @fresh.call, value, selector, tombstone)

    @appearances[identity] = surface1

    summon(outbox, surface1)

    outbox.each { |_, act| act.call }
  end

  def delete(identity : Label) : Nil
    if data = @sensors.delete(identity)
      dismiss(data)

      @callback.call(SensorAbsence.new(identity))

      return
    end

    unless data = @appearances.delete(identity)
      raise ArgumentError.new
    end

    # Implicit assumptions:
    #   - We never notify the same vertex more than one time in summon() nor dismiss().
    #   - Crystal hash tables are ordered.
    outbox = {} of Label => ->

    dismiss(outbox, data)

    outbox.each { |_, act| act.call }
  end
end

# A time-ordered Tspace-unique label generator. If the underlying Tbase
# is distributed, this means the generator must be globally unique.
alias LabelGenerator = Proc(Label) | ILabelGenerator

# :ditto:
module ILabelGenerator
  abstract def call : Label
end

record Sensor, id : Label, strands : StrandList do
  include Tbase::Sensor
end

record Appearance, id : Label, value : Term do
  include Tbase::Appearance
end

{% skip_file %}

class Counter
  include ILabelGenerator

  def initialize
    @counter = Atomic(Label).new(VERTEX_ZERO)
  end

  def call : Label
    @counter.add(1, :relaxed)
  end
end


fresh = Counter.new
data = HashMap(Tbase::Key, Tbase::Value).new
tbase = Tbase.new(fresh, data)
tspace = Tspace.new(fresh, tbase)

identity_view = Term[]

# TODO: in reality, the callback is potentially called from another thread,
# a sleepy queue is needed instead of doing things right away.
#
# TODO: in reality we will send activations, not views. It is upto the client
# to merge them into view / handle farewells / cleanup absent appearances.
#
# TODO: in reality, the sensor at client will have to match the full pattern.
# act only provides pattern skeleton matches, whereas client sensors may be
# much more tighter & include match envs.
smap = {0 => "A", 2 => "B", 3 => "C", 4 => "D"}
conn = Tconn.new(fresh, tspace) do |act|
  case act
  in StimulusPresence
    if act.pred != VERTEX_NONE
      identity_view = identity_view.morph({ act.sensor, {act.trigger, act.identity}, act.pred, nil })
    end

    identity_view = identity_view.morph({ act.sensor, {act.trigger, act.identity}, act.instant, act.value })
  in StimulusAbsence
    identity_view = identity_view.morph({ act.sensor, {act.trigger, act.identity}, nil })
  end
  pp render_view(identity_view, smap)
end


# conn.sensor 0, pattern: ML.term(%{_number}), selector: nil
# conn.sensor 1, pattern: ML.term(%{_number}), selector: nil

# pp tbase

# conn.delete 0
# conn.delete 1

# pp tbase
# {% skip_file %}

conn.add_appearance 1, value: ML.term(%{1}), selector: nil, tombstone: Term.of("bye bye")
conn.add_sensor 0, pattern: ML.term(%{_number}), selector: nil
conn.add_sensor 3, pattern: ML.term(%{(%any° _number "bye bye")}), selector: nil
conn.add_sensor 2, pattern: ML.term(%{(%any 2 4 6 8 9)}), selector: Term.of(:qux)

1000.times do |i|
  if i == 300
    conn.add_sensor 4, pattern: ML.term(%{_number}), selector: Term.of(:qux)
  elsif i == 500
    conn.delete 4
  elsif i == 900
    conn.add_sensor 4, pattern: ML.term(%{_number}), selector: Term.of(:qux)
  end
  conn.add_appearance 1, value: Term.of(i), selector: Term.of(:qux), tombstone: nil
end
conn.delete 1
conn.delete 0
conn.delete 2
conn.delete 3
conn.delete 4
pp tbase

{% skip_file %}

# ctx = ExecutionContext::MultiThreaded.new("MT", 4)

# counter = Atomic(Int32).new(0)

# # Note how each client has some parts overlapping with others (type: "pixel")
# # over which we have contention-by-content (irresolvable). On the other hand
# # the Xs and Ys are all independent.
# client = ->(ord : Int32) do
#   (10*ord...10*(ord + 1)).each do |i|
#     (10*ord...10*(ord + 1)).each do |j|
#       app = Appearance.new(fresh.call, Term.of(type: "pixel", x: i, y: j, ord: ord))
#       tbase.mount(app)
#       sleep 100.milliseconds
#     end
#   end

#   counter.add(1)
# end

# 1000.times do |i|
#   ctx.spawn { client.call(i) }
# end

# ctx.spawn do
#   start = Time.monotonic

#   until counter.get == 1000
#     puts counter.get
#     puts "-- in #{(Time.monotonic - start).seconds}s"
#     sleep 500.milliseconds
#   end

#   puts "All 1000 done!"
# end

# puts "Here"

# while input = gets
#   s = Sensor.parse(fresh.call, ML.term(input))
#   dt = Time.measure do
#     tbase.query(s) do |hit|
#       puts "Hit: #{hit}!"
#     end
#   end
#   puts "Took ~#{dt.total_milliseconds}ms"
# end

# {% skip_file %}
s0 = Sensor.parse(5000, %[{x: _, y: _}])
s1 = Sensor.parse(1001, %[{x: 100}])
s2 = Sensor.parse(1000, %[{y: _}])

a0 = Appearance.new(1234, Term.of(x: 100, y: 200))
a1 = Appearance.new(1235, Term.of(x: 100, y: 201))
a2 = Appearance.new(1236, Term.of(x: 101, y: 200))
a3 = Appearance.new(1237, Term.of(x: 101, y: 201))

tbase.mount(a0)
tbase.mount(a1)
tbase.mount(a2)
tbase.mount(a3)

tbase.mount(s0)
tbase.mount(s1)
tbase.mount(s2)

puts "Population: 1234 1235 1236 1237"

puts "Query #{s0}"
tbase.query(s0) { |hit| pp hit }
puts "Query #{s1}"
tbase.query(s1) { |hit| pp hit }
puts "Query #{s2}"
tbase.query(s2) { |hit| pp hit }

puts "Query #{a0}"
tbase.query(a0) { |hit| pp hit }
puts "Query #{a1}"
tbase.query(a1) { |hit| pp hit }
puts "Query #{a2}"
tbase.query(a2) { |hit| pp hit }
puts "Query #{a3}"
tbase.query(a3) { |hit| pp hit }

tbase.unmount(a0)

puts "Population: 1235 1236 1237"
tbase.query(s0) do |hit|
  pp hit
end

tbase.unmount(a3)

puts "Population: 1235 1236"
tbase.query(s0) do |hit|
  pp hit
end

tbase.unmount(a2)

puts "Population: 1235"
tbase.query(s0) do |hit|
  pp hit
end

tbase.unmount(a1)

puts "Population: "
tbase.query(s0) do |hit|
  pp hit
end

puts "Query #{a0} without s1"
tbase.unmount(s1)
tbase.query(a0) do |hit|
  pp hit
end

puts "Query #{a0} without s2"
tbase.unmount(s2)
tbase.query(a0) do |hit|
  pp hit
end
