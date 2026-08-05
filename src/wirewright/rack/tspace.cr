# Implements termspaces, which are a way to do global, content-based communication.
#
# Termspaces are complementary to the usual way of communication in Rack: edges.
# Edges are circuit-local; edge proximity is *not* semantic proximity. In a sense,
# edge proximity is "proximity by construction", just like cells in a grid are
# proximal to each other "by construction"; whereas in Termspaces, entities are
# proximal by predicate. In other words, to determine whether two entities are connected,
# in a standard hypergraph, we consult a predefined set; whereas with termspaces, we
# consult a predicate, which bothh parties participate in. If the predicate says yes,
# then there's a connection -- a kind of ephemeral hyperedge -- between the two parties.
#
# Termspaces consist of *sensors* and *appearances*. Sensors see terms,
# appearances show terms to sensors.
#
# See `rack.sensor`, `rack.appearance` to learn more about sensors and appearances.
# See `rack.tspace` to learn about termspaces in general.
module Ww::Rack::Tspace
  extend self

  # :nodoc:
  defcase State,
    indices : GenerationalCache(Term::Dict, IndexNode),
    appearance_msets : GenerationalCache(AppearanceSummary, Bag(Appearance)),
    appearance_mset : Bag(Appearance)

  class State
    setter appearance_mset
  end

  def state : State
    indices = GenerationalCache(Term::Dict, IndexNode).new
    appearance_msets = GenerationalCache(AppearanceSummary, Bag(Appearance)).new
    appearance_mset = Bag(Appearance).new
    State.new(indices, appearance_msets, appearance_mset)
  end

  def step(state : State, parser : D7::Parser, circuit circuit0 : Term, prepass) : Slice(Term)
    tree = parser.parse(circuit0)
    index = state.indices.epoch { index(state.indices, tree) }

    # TODO: Working with Bags like we do here is really, really inefficient in terms
    # of complexity, and is in fact O(N), where N is the maximum number of appearances
    # in l, r (-ish, I'm not sure if it's exactly that, I'm not in the mood
    # for analysis).
    #
    # In fact, I think this is the only place where we're O(N) unconditionally
    # in the Tspace pass. That's really sad because termspaces aren't supposed
    # to be O(N).
    appearances_added = Pf::Kit.stack_array(Appearance, 8)
    appearances_removed = Pf::Kit.stack_array(Appearance, 8)

    l = state.appearance_mset
    r = state.appearance_msets.epoch { appearance_mset(state.appearance_msets, index) }

    (l - r).each do |appearance|
      appearances_removed << appearance
    end

    (r - l).each do |appearance|
      appearances_added << appearance
    end

    state.appearance_mset = r

    events = Pf::Kit.stack_array(SurfaceEvent)
    consumed = Set(ParsePath).new

    pairings = pair(index)
    pairings.each do |path, pairing|
      emit(events, consumed, path, pairing.sensor, pairing.stimuli, pairing.consensus)
    end

    each(index, JournalSensor, guide: stateless_guide) do |sensor_path, sensor|
      news = Pf::Kit.stack_array(Term, 8)

      appearances_added.each do |appearance|
        next unless appearance.tspace == sensor.tspace
        next unless match = match?(sensor.pattern, appearance.value)

        news << Term.of(:appeared, match.value)
      end

      appearances_removed.each do |appearance|
        next unless appearance.tspace == sensor.tspace
        next unless match = match?(sensor.pattern, appearance.value)

        news << Term.of(:disappeared, match.value)
      end

      next if news.empty?

      # Make sure order is deterministic and human-comprehensible.
      news.sort! { |a, b| Term.compare(a, b) }

      events << JournalSensorNews.new(sensor_path, news.to_unsafe_readonly_slice!)
    end

    consumed.each do |path|
      events << AppearanceConsumed.new(path)
    end

    circuit1 = D7.collapse(dispatch(tree, events))

    Slice[circuit1]
  end

  alias Surface = Sensor | Appearance

  alias Sensor = StatefulSensor | StatelessSensor

  alias StatefulSensor = SingleSensor | SingleSensorCell | MultiSensor | MultiSensorCell |
                         ViewSensor | ViewSensorCell | QueueSensor

  alias StatelessSensor = JournalSensor

  # `SingleSensor`s can perceive and transfer just one value. One or more
  # appearances must show *the same* value for a `SingleSensor` to see it.
  # If the values are different, `SingleSensor` gets confused and doesn't
  # see anything.
  defrecord SingleSensor, tspace : Term, pattern : Term

  # Same as a `SingleSensor`, but stores the result of running *template*
  # with the match env from *pattern*, separately, in a `cell`.
  defrecord SingleSensorCell, tspace : Term, pattern : Term, template : Term

  # A `MultiSensor` can perceive and transfer multiple different values at
  # once; but it only does that if it's empty.
  defrecord MultiSensor, tspace : Term, pattern : Term

  # Like `SingleSensorCell` but for `MultiSensor`s.
  defrecord MultiSensorCell, tspace : Term, pattern : Term, template : Term

  # `ViewSensor`s perceive zero or more corresponding appearances without
  # transferring their value. In effect, they offer a "continuous" view of
  # appearances matching *pattern*. View sensors are the most inefficient kind
  # of sensors, because they have to constantly monitor *tspace* for *pattern*.
  # The runtime could optimize some of this monitoring, but definitely not all
  # of it.
  defrecord ViewSensor, tspace : Term, pattern : Term

  # Like `SingleSensorCell` but for `ViewSensor`s.
  defrecord ViewSensorCell, tspace : Term, pattern : Term, template : Term

  # Journal sensors detect efficiently when matching appearances *appear*
  # or *disappear*.
  defrecord JournalSensor, tspace : Term, pattern : Term

  # `QueueSensor`s can perceive and transfer multiple different values at once.
  # Values within the same batch are sorted. A batch corresponds to one tick of
  # time. Batches are *appended* to the sensor's structure. Therefore, the sensor'
  # structure contains batches (whose elements are ordered lexicographically)
  # ordered temporally.
  defrecord QueueSensor, tspace : Term, pattern : Term

  # An appearance shows values to sensors. Sensors can *transfer* ("steal")
  # values from appearances or they can provide *views* of them ("observe"
  # them) or their evolution (see e.g. `JournalSensor`).
  defrecord Appearance, tspace : Term, value : Term

  # Surface events are used to deliver some payload down to a specific
  # sensor or appearance node.
  #
  # - All surface events must respond to `#path : ParsePath`.
  # - All surface events must supprot`#copy_with`.
  alias SurfaceEvent = SingleSensorReceived | MultiSensorReceived | ViewSensorReceived |
                       JournalSensorNews | AppearanceConsumed

  defrecord SingleSensorReceived, path : ParsePath, match : Term, copying: true
  defrecord MultiSensorReceived, path : ParsePath, matches : Slice(Term), copying: true
  defrecord ViewSensorReceived, path : ParsePath, matches : Slice(Term), copying: true
  defrecord JournalSensorNews, path : ParsePath, news : Slice(Term), copying: true
  defrecord AppearanceConsumed, path : ParsePath, copying: true

  # Records the *pairing* of a stateful sensor with zero or more stimuli.
  # If a consensus match exists among stimuli, *consensus* is that value.
  # if stimuli matches disagree, *consensus* is `nil`.
  defrecord Pairing,
    sensor : StatefulSensor,
    stimuli : Array(Stimulus),
    consensus : Match?,
    copying: true

  # Represents a stateful sensor stimulus. *path* points to the `Appearance`
  # this stimulus is coming from.
  defrecord Stimulus, path : ParsePath, match : Match

  struct ParsePath
    # :nodoc:
    #
    # NOTE: Pf::UPath32 only supports append() so we have to simulate
    # prepends() by actually appending under the hood. Whereas ParsePath
    # uses the more intuitive (to me) root-to-leaf = left-to-right order:
    # 1-2-3 means root -> child 1 -> child 2 -> child 3 (leaf), reading
    # left-to-right means "descend". On the other hand, Pf::UPath32 requires
    # leaf-to-root order. So the same example would be 3-2-1, reading left-
    # to-right means "ascend". So internally it's child 3 (leaf) -> child 2 ->
    # child 1 -> root.
    def initialize(@rpath : Pf::UPath32)
    end

    def self.empty : ParsePath
      new(Pf::UPath32[])
    end

    def self.of(step : UInt32) : ParsePath
      empty.prepend(step)
    end

    def prepend(step : UInt32) : ParsePath
      ParsePath.new(@rpath.append(step))
    end

    def append(path : ParsePath) : ParsePath
      @rpath.each do |step|
        path = path.prepend(step)
      end

      path
    end

    def starts_with?(step : UInt32) : Bool
      @rpath.last? == step
    end

    def rest : ParsePath
      ParsePath.new(@rpath.prior)
    end

    def inspect(io)
      io << "ParsePath("

      remaining = @rpath

      (0...).each do |index|
        break unless step = remaining.last?

        io << "->" if index > 0
        io << step
        remaining = remaining.prior
      end

      io << ")"
    end
  end

  defrecord Match, env : Term::Dict, value : Term

  private def match?(pattern : Term, value : Term) : Match?
    return unless M1.probably_matches?(pattern, value)
    return unless env = M1.match?(pattern, value)

    if env.empty?
      # If pattern makes no captures, percept is the matchee.
      Match.new(env, value)
    elsif env.size == 1
      # If pattern makes one capture, percept is the value of that capture.
      _, match = env.ee.first
      Match.new(env, match)
    else
      # If pattern makes many captures, percept is the env itself.
      Match.new(env, Term.of(env))
    end
  end

  # Only enters into subtrees which contain a stateless sensor.
  private def stateless_guide : (Tspace -> Bool)
    ->(tspace : Tspace) do
      tspace.population.stateless_sensor?
    end
  end

  alias IndexNode = NoIndex | IndexLeaf | IndexLeafPath | IndexFanout
  alias IndexLeaf = Surface

  defrecord NoIndex

  defrecord IndexLeafPath,
    leaf : IndexLeaf,
    path : ParsePath

  defcase IndexFanout,
    fanout : Slice(IndexNode),
    tspaces : Hash(Term, Tspace),
    path : ParsePath,
    appearances : AppearanceSummary

  # We could just use SHA256, of course, but pessimistically that'd be slow. So
  # instead we use XXHash which we already link to anyway, and some auxiliary
  # "defensive" statistics to make collisions a bit less probable (I guess...)
  struct AppearanceSummary
    # :nodoc:
    def initialize(
      @hashcode : UInt128,
      @count : UInt32,
      @xor32 : UInt32,
      @min32 : UInt32,
      @max32 : UInt32,
    )
    end

    def self.zero : AppearanceSummary
      new(hashcode: 0u128, count: 0u32, xor32: 0u32, min32: UInt32::MAX, max32: 0u32)
    end

    def add(element : UInt128) : AppearanceSummary
      lo32 = (element & 0xff_ff_ff_ffu128).to_u32

      AppearanceSummary.new(
        hashcode: @hashcode &+ element,
        count: @count + 1,
        xor32: @xor32 ^ lo32,
        min32: Math.min(@min32, lo32),
        max32: Math.max(@max32, lo32),
      )
    end

    def |(other : AppearanceSummary)
      AppearanceSummary.new(
        hashcode: @hashcode &+ other.@hashcode,
        count: @count + other.@count,
        xor32: @xor32 ^ other.@xor32,
        min32: Math.min(@min32, other.@min32),
        max32: Math.max(@max32, other.@max32),
      )
    end
  end

  struct Tspace
    @[Flags]
    enum Population : UInt32
      StatefulSensor
      StatelessSensor
      Appearance
    end

    getter members : Pf::USet32
    getter population : Population

    # :nodoc:
    def initialize(@members : Pf::USet32, @population)
    end

    def self.empty : Tspace
      new(Pf::USet32.new, Population::None)
    end

    def add(member : UInt32, surface : StatefulSensor) : Tspace
      Tspace.new(@members.add(member), @population | Population::StatefulSensor)
    end

    def add(member : UInt32, surface : StatelessSensor) : Tspace
      Tspace.new(@members.add(member), @population | Population::StatelessSensor)
    end

    def add(member : UInt32, surface : Appearance) : Tspace
      Tspace.new(@members.add(member), @population | Population::Appearance)
    end

    def add(member : UInt32, tspace : Tspace) : Tspace
      Tspace.new(@members.add(member), @population | tspace.population)
    end
  end

  # Prepares pairings. We must first see how many appearances, globally, a sensor
  # is triggered by, in order to handle the semantics of SingleSensor vs. MultiSensor
  # and similar.
  private def pair(index : IndexNode) : Hash(ParsePath, Pairing)
    pairings = {} of ParsePath => Pairing

    each(index, StatefulSensor) do |sensor_path, sensor|
      stimuli = [] of Stimulus
      consensus = nil

      pass do
        # If there are no appearances in the sensor's termspace, then we're
        # able to skip it.
        if index.is_a?(IndexFanout) && (info = index.tspaces[sensor.tspace]?)
          next unless info.population.appearance?
        end

        each(index, Appearance, tspace: sensor.tspace) do |appearance_path, appearance|
          next unless match = match?(sensor.pattern, appearance.value)

          # Maintain a notion of a "consensus" value for efficiency to handle SingleSensor
          # semantics: it is allowed to perceive many appearances but only if they all
          # have the same value.
          if stimuli.empty?
            consensus = match
          elsif consensus && consensus != match
            consensus = nil
          end

          stimuli << Stimulus.new(appearance_path, match)
        end
      end

      # NOTE: We must still a pairing, even if it's empty, but only for view sensors.
      # For all other sensors we don't care.
      next unless stimuli.present? || sensor.is_a?(ViewSensor) || sensor.is_a?(ViewSensorCell)

      pairing = Pairing.new(sensor, stimuli, consensus)
      assert pairings.put?(sensor_path, pairing)
    end

    pairings
  end

  private def index(cache : ICache, tree : D7::InertLeaf) : IndexNode
    NoIndex.new
  end

  private def index(cache : ICache, tree : D7::GndLeaf) : IndexNode
    Term.case(tree.feature.node) do
      matchpi %{[sensor (tspace_ pattern_)]} do
        SingleSensor.new(tspace, pattern)
      end

      matchpi %{[sensor (many tspace_ pattern_)]} do
        MultiSensor.new(tspace, pattern)
      end

      matchpi %{[sensor (queue tspace_ pattern_)]} do
        QueueSensor.new(tspace, pattern)
      end

      matchpi %{[sensor (view tspace_ pattern_) _*]} do
        ViewSensor.new(tspace, pattern)
      end

      matchpi %{[sensor (journal tspace_ pattern_) _*]} do
        JournalSensor.new(tspace, pattern)
      end

      matchpi %{[appearance tspace_ value_]} do
        Appearance.new(tspace, value)
      end

      otherwise do
        NoIndex.new
      end
    end
  end

  private def index(cache : ICache, tree : D7::MixtureNode)
    Term.case(tree.feature.node) do
      matchpi %{[surface [cell @_] [sensor (tspace_ pattern_) template_]]} do
        SingleSensorCell.new(tspace, pattern, template)
      end

      matchpi %{[surface [cell @_] [sensor (many tspace_ pattern_) template_]]} do
        MultiSensorCell.new(tspace, pattern, template)
      end

      matchpi %{[surface [cell @_] [sensor (view tspace_ pattern_) template_]]} do
        ViewSensorCell.new(tspace, pattern, template)
      end

      otherwise do
        index(cache, tree.child)
      end
    end
  end

  private def index(cache : ICache, tree : D7::ScopeNode) : IndexNode
    index(cache, tree.child)
  end

  # :nodoc:
  SYM_SENSOR = Term[:sensor]
  # :nodoc:
  SYM_APPEARANCE = Term[:appearance]

  private def index(cache : ICache, tree : D7::ParentNode) : IndexNode
    node = tree.feature.node
    unless node.probably_includes?(SYM_SENSOR) || node.probably_includes?(SYM_APPEARANCE)
      return NoIndex.new
    end

    cache.put_if_absent(node) do
      fanout = Pf::Kit.stack_array(IndexNode, 8)
      tspaces = {} of Term => Tspace
      appearances = AppearanceSummary.zero

      tree.children.each_with_index do |child, slot|
        slot = slot.to_u32
        fanout_index = fanout.size.to_u32

        case successor = index(cache, child)
        in NoIndex
        in IndexLeaf
          fanout << IndexLeafPath.new(successor, ParsePath.of(slot))
          if successor.is_a?(Appearance)
            appearances = appearances.add(hash128(successor))
          end

          tspace0 = tspaces[successor.tspace]? || Tspace.empty
          tspace1 = tspace0.add(fanout_index, successor)
          tspaces[successor.tspace] = tspace1
        in IndexLeafPath
          leaf = successor.leaf

          fanout << IndexLeafPath.new(leaf, successor.path.prepend(slot))
          if leaf.is_a?(Appearance)
            appearances = appearances.add(hash128(leaf))
          end

          tspace0 = tspaces[leaf.tspace]? || Tspace.empty
          tspace1 = tspace0.add(fanout_index, leaf)
          tspaces[leaf.tspace] = tspace1
        in IndexFanout
          fanout << IndexFanout.new(successor.fanout, successor.tspaces, successor.path.prepend(slot), successor.appearances)
          appearances |= successor.appearances

          successor.tspaces.each do |tspace_id, tspace|
            tspace0 = tspaces[tspace_id]? || Tspace.empty
            tspace1 = tspace0.add(fanout_index, tspace)
            tspaces[tspace_id] = tspace1
          end
        end
      end

      if fanout.empty?
        NoIndex.new
      else
        fanout.single? || IndexFanout.new(fanout.to_unsafe_readonly_slice!, tspaces, ParsePath.empty, appearances)
      end
    end
  end

  private def appearance_mset(cache : ICache, index : NoIndex) : Bag(Appearance)
    Bag(Appearance).new
  end

  private def appearance_mset(cache : ICache, index : Appearance) : Bag(Appearance)
    Bag{index}
  end

  private def appearance_mset(cache : ICache, index : IndexLeaf) : Bag(Appearance)
    Bag(Appearance).new
  end

  private def appearance_mset(cache : ICache, index : IndexLeafPath) : Bag(Appearance)
    appearance_mset(cache, index.leaf)
  end

  private def appearance_mset(cache, index : IndexFanout) : Bag(Appearance)
    # Fast path if it does not contain any appearances at all.
    unless index.tspaces.any? { |_, tspace| tspace.population.appearance? }
      return Bag(Appearance).new
    end

    cache.put_if_absent(index.appearances) do
      appearance_mset = Bag(Appearance).new

      index.fanout.each do |successor|
        appearance_mset.concat(appearance_mset(cache, successor))
      end

      appearance_mset
    end
  end

  private def hash128(io, surface : SingleSensor) : Nil
    io << "Sg("
    ML.compact(io, surface.tspace)
    ML.compact(io, surface.pattern)
    io << ")"
  end

  private def hash128(io, surface : MultiSensor) : Nil
    io << "Mx("
    ML.compact(io, surface.tspace)
    ML.compact(io, surface.pattern)
    io << ")"
  end

  private def hash128(io, surface : QueueSensor) : Nil
    io << "Qx("
    ML.compact(io, surface.tspace)
    ML.compact(io, surface.pattern)
    io << ")"
  end

  private def hash128(io, surface : ViewSensor) : Nil
    io << "Vx("
    ML.compact(io, surface.tspace)
    ML.compact(io, surface.pattern)
    io << ")"
  end

  private def hash128(io, surface : JournalSensor) : Nil
    io << "J("
    ML.compact(io, surface.tspace)
    ML.compact(io, surface.pattern)
    io << ")"
  end

  private def hash128(io, surface : Appearance) : Nil
    io << "A("
    ML.compact(io, surface.tspace)
    ML.compact(io, surface.value)
    io << ")"
  end

  private def hash128(surface : Surface) : UInt128
    buffer = uninitialized UInt8[8192]
    io = IO::Digest128.new(buffer.to_slice)
    hash128(io, surface)
    io.flush
    io.digest
  end

  private def each(index : IndexNode, cls : T.class, **kwargs, &fn : ParsePath, T ->) : Nil forall T
    each(ParsePath.empty, index, cls, **kwargs, &fn)
  end

  private def each(path : ParsePath, index : IndexNode, cls : T.class, *, tspace : Term? = nil, guide : Tspace -> Bool = ->(tspace : Tspace) { true }, &fn : ParsePath, T ->) : Nil forall T
    walk(index, cls, path, tspace, guide, fn)
  end

  private def walk(index : NoIndex, cls : T.class, path, tspace, guide, fn) forall T
  end

  private def walk(index : IndexLeaf, cls : T.class, path, tspace, guide, fn) forall T
    return unless index.is_a?(T)
    return unless tspace.nil? || index.tspace == tspace

    fn.call(path, index)
  end

  private def walk(index : IndexLeafPath, cls : T.class, path, tspace, guide, fn) forall T
    return unless leaf = index.leaf.as?(T)
    return unless tspace.nil? || leaf.tspace == tspace

    fn.call(path.append(index.path), leaf)
  end

  private def walk(index : IndexFanout, cls : T.class, path, tspace : Nil, guide, fn) forall T
    subpath = path.append(index.path)

    # Fast path.
    if index.tspaces.size == 1
      info = index.tspaces.first_value
      return unless guide.call(info)

      assert info.members.size == index.fanout.size

      index.fanout.each do |successor|
        walk(successor, cls, subpath, tspace, guide, fn)
      end
      return
    end

    index.tspaces.each do |pivot, info|
      next unless guide.call(info)

      if info.members.size >= index.fanout.size//2 # more than half
        # Do it the dumb way.
        index.fanout.each_with_index do |successor, fanout_index|
          next unless fanout_index.to_u32.in?(info.members)

          walk(successor, cls, subpath, pivot, guide, fn)
        end
      else
        # Do it the smart way.
        info.members.each do |fanout_index|
          successor = index.fanout[fanout_index]
          walk(successor, cls, subpath, pivot, guide, fn)
        end
      end
    end
  end

  private def walk(index : IndexFanout, cls : T.class, path, tspace : Term, guide, fn) forall T
    return unless info = index.tspaces[tspace]?
    return unless guide.call(info)

    subpath = path.append(index.path)

    info.members.each do |fanout_index|
      successor = index.fanout[fanout_index]
      walk(successor, cls, subpath, tspace, guide, fn)
    end
  end

  private def emit(events, consumed, path, sensor : SingleSensor, stimuli, consensus : Match?) : Nil
    return unless consensus

    events << SingleSensorReceived.new(path, consensus.value)

    stimuli.each do |stimulus|
      consumed << stimulus.path
    end
  end

  private def emit(events, consumed, path, sensor : SingleSensorCell, stimuli, consensus : Match?) : Nil
    return unless consensus

    value = Alloy.render(consensus.env, sensor.template)
    events << SingleSensorReceived.new(path, value)

    stimuli.each do |stimulus|
      consumed << stimulus.path
    end
  end

  private def emit(events, consumed, path, sensor : MultiSensor | QueueSensor, stimuli, consensus : Match?) : Nil
    matches = stimuli.to_slice(&.match.value)
    # Make sure order is deterministic and human-comprehensible.
    matches.sort! { |a, b| Term.compare(a, b) }

    events << MultiSensorReceived.new(path, matches)

    stimuli.each do |stimulus|
      consumed << stimulus.path
    end
  end

  private def emit(events, consumed, path, sensor : MultiSensorCell | ViewSensorCell, stimuli, consensus : Match?) : Nil
    matches = stimuli.to_slice do |stimulus|
      if sensor.is_a?(MultiSensorCell)
        consumed << stimulus.path
      end

      Alloy.render(stimulus.match.env, sensor.template)
    end

    # Make sure order is deterministic and human-comprehensible.
    matches.sort! { |a, b| Term.compare(a, b) }

    events << SingleSensorReceived.new(path, Term.of(matches))
  end

  private def emit(events, consumed, path, sensor : ViewSensor, stimuli, consensus : Match?) : Nil
    matches = stimuli.to_slice(&.match.value)
    # Make sure order is deterministic and human-comprehensible.
    matches.sort! { |a, b| Term.compare(a, b) }

    events << ViewSensorReceived.new(path, matches)
  end

  private def dispatch(tree : D7::InertLeaf, events) : D7::RepairTree
    tree.feature.node
  end

  private def dispatch(tree : D7::GndLeaf, events) : D7::RepairTree
    if events.empty?
      return tree.feature.node # unchanged
    end

    assert event = events.single?

    case event
    in SingleSensorReceived
      # (sensor (_ _)) -> (sensor (_ _) _)
      #
      # NOTE: This is also used for SingleSensorCells. This works because in:
      #
      #   (surface (cell @_) (sensor _ _))
      #
      # ... we're actually in the cell, since the `dispatch` process doesn't
      # stop on `surface`s like `index` does:
      #
      #   (surface ⏏(cell @_)⏏ (sensor _ _))
      #
      # Surface is a mixture that resolves to (cell @_). So messages targeting
      # the surface will target the `cell`.
      Term.morph(tree.feature.node, {2, event.match})
    in MultiSensorReceived
      # (sensor (many _ _)) -> (sensor (many _ _) _*)
      node = tree.feature.node.as_d
      node = node.transaction do |commit|
        commit.concat(event.matches)
      end

      Term.of(node)
    in ViewSensorReceived
      # (sensor (view _ _) _*)
      node = tree.feature.node.as_d

      node = node.pairspart.transaction do |commit|
        commit << :sensor << node[1] # sensor (view _ _)
        commit.concat(event.matches)
      end

      Term.of(node)
    in JournalSensorNews
      # (sensor (journal _ _) _*)
      node = tree.feature.node.as_d
      node = node.transaction do |commit|
        commit.concat(event.news)
      end
      Term.of(node)
    in AppearanceConsumed
      # (appearance _ _) -> (appearance _)
      Term.morph(tree.feature.node, {2, nil})
    end
  end

  private def dispatch(tree : D7::MixtureNode | D7::ScopeNode, events) : D7::RepairTree
    D7.repair(tree) { |child| dispatch(child, events) }
  end

  private def dispatch(tree : D7::ParentNode, events) : D7::RepairTree
    if events.empty?
      return Term.of(tree.feature.node) # unchanged
    end

    scratch = Pf::Kit.stack_array(SurfaceEvent, 8)

    D7.repair(tree) do |child, slot|
      slot = slot.to_u32

      events.each do |event|
        next unless event.path.starts_with?(slot)

        scratch << event.copy_with(path: event.path.rest)
      end

      dispatch(child, scratch)
    ensure
      scratch.clear
    end
  end
end
