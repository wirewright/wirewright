require "./src/wirewright"

Log.setup_from_env(default_level: :debug)

include Meridium

module IAtomSet
  include IAtomsPresent

  abstract def commit(adds : Enumerable(Atom), dels : Enumerable(Atom)) : Nil
end

module IChat(M)
  alias Subscribe = ->
  alias Unsubscribe = ->

  abstract def connect(conid : WWID, &recv : M ->) : {Subscribe, Unsubscribe}
  abstract def send(conid : WWID, message : M) : Nil
end

class SyncInMemoryChat(M)
  include IChat(M)

  @subscribers = {} of WWID => Set(M ->)
  @lock = Mutex.new

  def connect(conid : WWID, &recv : M ->) : {Subscribe, Unsubscribe}
    sub = Subscribe.new do
      @lock.synchronize do
        recvs = @subscribers.put_if_absent(conid) { Set(M ->).new }
        recvs << recv
      end
    end

    unsub = Unsubscribe.new do
      @lock.synchronize do
        next unless recvs = @subscribers[conid]?
        next unless recvs.delete(recv)
        next unless recvs.empty?

        @subscribers.delete(conid)
      end
    end

    {sub, unsub}
  end

  def send(conid : WWID, message : M) : Nil
    recvs = @lock.synchronize do
      # Copy receiver procs (if any) so that we can call them outside of the lock,
      # and so that they're "frozen in time".
      @subscribers[conid]?.try(&.dup)
    end

    return unless recvs

    recvs.each &.call(message)
  end
end

struct MyMultiset
  def initialize
    @hash = {} of Atom => UInt32
  end

  def includes?(atom : Atom) : Bool
    @hash.has_key?(atom)
  end

  def <<(atom : Atom)
    @hash[atom] = (@hash[atom]? || 0u32) + 1

    self
  end

  def delete(atom : Atom)
    return unless tally = @hash[atom]?

    if tally == 1
      @hash.delete(atom)
    else
      @hash[atom] = tally - 1
    end
  end
end

class MySet
  include IAtomSet

  def initialize(@n : Int32)
    @sets = Slice(MyMultiset).new(@n) { MyMultiset.new }
    @locks = Slice(Mutex).new(@n) { Mutex.new }
  end

  def <<(atom : Meridium::Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket] << atom
    end
  end

  def commit(adds : Enumerable(Atom), dels : Enumerable(Atom)) : Nil
    dels.each do |atom|
      bucket = atom.@blk0 % @n
      @locks[bucket].synchronize do
        @sets[bucket].delete(atom)
      end
    end

    adds.each do |atom|
      self << atom
    end
  end

  def present?(atom : Meridium::Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket].includes?(atom)
    end
  end

  def size
    @sets.sum(&.size)
  end

  def present?(objects : Enumerable(T), & : T -> Meridium::Atom | Enumerable(Meridium::Atom)) : BitList forall T
    answer = BitList.new

    objects.each do |object|
      ee = yield object

      unless ee.is_a?(Enumerable(Meridium::Atom))
        ee = {ee}
      end

      ee.each do |atom|
        answer << present?(atom)
      end
    end

    answer
  end
end

record Instant, timestamp : UInt64 do
  include Comparable(Instant)

  def <=>(other : Instant)
    timestamp <=> other.timestamp
  end

  def self.now : Instant
    dt = Time.utc - WW_EPOCH

    new(timestamp: dt.total_nanoseconds.floor.to_u64)
  end
end

record IWWID, wwid : WWID, instant : Instant do
  delegate :conid, :slot, to: @wwid
end

alias Activation = StimulusPresence | StimulusAbsence | StimulusRequest | StimulusResponse

record StimulusPresence, sensor : WWID, appearance : IWWID, stimulus : Term
record StimulusAbsence, sensor : WWID, appearance : IWWID
record StimulusRequest, sensor : IWWID, appearance : WWID
record StimulusResponse, sensor : IWWID, appearance : IWWID, stimulus : Term do
  def to_stimulus_presence : StimulusPresence
    StimulusPresence.new(sensor.wwid, appearance, stimulus)
  end
end

alias Effect = SurfaceAddition | SurfaceDeletion | ViewChange | Stimulation

record ViewChange, view : View
record SurfaceAddition, id : IWWID, surface : Surface
record SurfaceDeletion, id : IWWID, surface : Surface
record Stimulation, surface : Appearance, sensor : IWWID, appearance : IWWID do
  def to_stimulus_response : StimulusResponse
    StimulusResponse.new(sensor, appearance, surface.value)
  end
end

# We distinguish *stimuli* and *percepts* in the following way.
#
# A *stimulus* is something *produced* and *extrinsic* from one's point of view.
# Stimuli are produced by appearances. It is stimuli that travel over
# the network, for example.
#
# A *percept*, on the other hand, is something that is the result of *perception* --
# namely, by a sensor. Percepts are of "local origin and use" -- they are *intrinsic*;
# percepts never escape to the network, unless converted to stimuli through an appearance.
#
# In Wirewright, *perception* is simply pattern matching -- breaking up a *stimulus*
# into a list of *features* based on the sensor's pattern; each *feature* being an
# M1 match env.
record Percept, instant : Instant, stimulus : Term, features : Array(Term::Dict)

# An internal object that maintains the percepts that a sensor perceives.
#
# See also: `Percept`.
class PerceptData
  # :nodoc:
  def initialize(@percepts = Pf::Map(WWID, Percept).new)
  end

  private def_change

  # Returns `true` if this object currently registers no percepts.
  def absent? : Bool
    @percepts.empty?
  end

  # :nodoc:
  def after(surface : Sensor, act : StimulusPresence) : {PerceptData, Bool}
    if percept = @percepts[act.appearance.wwid]?
      # Make sure WE are outdated, not act.
      unless percept.instant < act.appearance.instant
        Log.trace { "reject stimulus presence: instant #{percept.instant} (my) !< #{act.appearance.instant} (its)" }
        return self, false
      end

      features0 = percept.features
    end

    features1 = M1.matches(surface.pattern, act.stimulus)

    # If there is a pattern mismatch, and we've previously known the appearance,
    # this means the appearance changed its value to something we don't like
    # anymore. In that case we remove the appearance.
    if features1.empty?
      return change(percepts: @percepts.dissoc(act.appearance.wwid)), false
    end

    percept = Percept.new(act.appearance.instant, act.stimulus, features1)

    {change(percepts: @percepts.assoc(act.appearance.wwid, percept)), features0 != features1}
  end

  # :nodoc:
  def after(surface : Sensor, act : StimulusAbsence) : {PerceptData, Bool}
    unless percept = @percepts[act.appearance.wwid]?
      return self, false
    end

    # Make sure WE are outdated, not act.
    unless percept.instant <= act.appearance.instant
      Log.trace { "reject percept absence: instant #{percept.instant} (my) !< #{act.appearance.instant} (its)" }
      return self, false
    end

    {change(percepts: @percepts.dissoc(act.appearance.wwid)), true}
  end

  {% if flag?(:docs) %}
    # Registers the activation *act* targeted at the given sensor *surface*.
    # Returns the resulting copy of this object, followed by a boolean indicating
    # whether some percept changed due to *act*.
    def after(surface : Sensor, act : StimulusPresence | StimulusAbsence) : {PerceptData, Bool}
    end
  {% end %}

  # Updates this object according to an exhaustive set of *appearances*
  # perceived by the given sensor *surface*; in other words, removes all
  # perceived appearances **not** in the *appearances* set.
  def presence(surface : Sensor, appearances : Set(WWID)) : {PerceptData, Bool}
    percepts1 = @percepts.select { |id, _| id.in?(appearances) }

    {change(percepts: percepts1), @percepts != percepts1}
  end

  # Formats this object as a dict multiset of percept features.
  #
  # A *feature* is simply "local dialect" for M1 match env.
  #
  # See also: `Percept`.
  def dict_multiset : Term::Dict
    Term::Dict.build do |commit|
      @percepts.each do |_, percept|
        percept.features.each do |feature|
          commit.with(feature, (commit[feature]? || 0) + 1)
        end
      end
    end
  end
end

# An internal object that maintains the percept data for all of `Node`'s sensors.
class View
  alias Version = UInt32

  # A monotonically increasing, per-view integer that can be used to detect view
  # changes easily.
  getter version : Version

  def initialize(@map = Pf::Map(Slot, PerceptData).new, @version = Version.new(0))
  end

  private def_change

  # Yields each sensor slot and percept data object to the block.
  def each(& : Slot, PerceptData ->) : Nil
    @map.each { |slot, data| yield slot, data }
  end

  # Registers a sensor *surface* at *slot*. Returns the resulting copy of
  # this view.
  #
  # This is necessary to begin perceiving stimuli.
  def register(slot : Slot, surface : Sensor) : View
    map1 = @map.assoc(slot, PerceptData.new)
    map1.same?(@map) ? self : change(map: map1, version: @version + 1)
  end

  # Unregisters the sensor *surface* at *slot*. Returns the resulting copy of
  # this view.
  #
  # This is necessary to stop perceiving stimuli.
  def unregister(slot : Slot, surface : Sensor) : View
    map1 = @map.dissoc(slot)
    map1.same?(@map) ? self : change(map: map1, version: @version + 1)
  end

  # Updates the percept data of *surface* at *slot* based on an exhaustive
  # set of *appearances* it perceives at the moment. Returns the resulting
  # copy of this view.
  def presence(slot : Slot, surface : Sensor, appearances : Set(WWID)) : View
    unless stimuli0 = @map[slot]?
      return self
    end

    stimuli1, changed = stimuli0.presence(surface, appearances)

    change(map: @map.assoc(slot, stimuli1), version: changed ? @version + 1 : @version)
  end

  # Registers the activation *act* targeted at the given sensor *surface*.
  # Returns the resulting copy of this view,
  def after(surface : Sensor, act : StimulusPresence | StimulusAbsence) : View
    slot = act.sensor.slot

    unless stimuli0 = @map[slot]?
      return self
    end

    stimuli1, changed = stimuli0.after(surface, act)

    change(map: @map.assoc(slot, stimuli1), version: changed ? @version + 1 : @version)
  end

  # Formats this view as a dict, mapping slots to multisets of percept features
  # perceived by the sensor at that slot.
  def dict_multisets : Term::Dict
    Term::Dict.build do |commit|
      each do |slot, stimuli|
        next if stimuli.absent?

        commit.with(slot, stimuli.dict_multiset)
      end
    end
  end
end

class Node
  Log = ::Log.for(self)

  # Returns the connection id of this node.
  getter conid : WWID

  # Returns the latest view.
  getter view : View

  def initialize(@conid = WWID.new)
    unless @conid.slot.zero?
      raise ArgumentError.new("expected trunk conid (conid with slot=0)")
    end

    @view = View.new
    @surfaces = {} of Slot => Surface
    @instants = {} of Slot => Instant
  end

  private def review?(& : View -> View) : Bool
    view0 = @view
    @view = yield view0
    view0.version != @view.version
  end

  private def insert(slot, surface, & : Effect ->) : Nil
    @instants[slot] = instant = Instant.now
    @surfaces[slot] = surface

    yield SurfaceAddition.new(IWWID.new(@conid.with_slot(slot), instant), surface)

    return unless surface.is_a?(Sensor)

    if review? &.register(slot, surface)
      yield ViewChange.new(@view)
    end
  end

  # Returns the surface at *slot*, if any. Returns `nil` otherwise.
  def []?(slot : Slot) : Surface?
    @surfaces[slot]?
  end

  # Yields WWIDs and their corresponding surfaces.
  def each(& : WWID, Surface ->) : Nil
    @surfaces.each do |slot, surface|
      yield @conid.with_slot(slot), surface
    end
  end

  # Returns a slice of all slots occupied at this moment.
  def slots : Slice(Slot)
    slots = Pointer(Slot).malloc(@surfaces.size)

    @surfaces.each_with_index do |(slot, _), index|
      slots[index] = slot
    end

    slots.to_slice(@surfaces.size)
  end

  # Updates or inserts the surface at *slot*. Yields the effects of that
  # to the block.
  def put(slot : Slot, surface : Surface, & : Effect ->) : Nil
    delete(slot) { |effect| yield effect }
    insert(slot, surface) { |effect| yield effect }
  end

  # Removes the surface at *slot*. Yields the effects of that to the block.
  def delete(slot : Slot, & : Effect ->) : Nil
    return unless surface = @surfaces.delete(slot)

    unless instant = @instants.delete(slot)
      raise "BUG: surface was deleted but instant was not"
    end

    yield SurfaceDeletion.new(IWWID.new(@conid.with_slot(slot), instant), surface)

    return unless surface.is_a?(Sensor)

    if review? &.unregister(slot, surface)
      yield ViewChange.new(@view)
    end
  end

  # Updates the view according to an exhaustive set of appearances perceived
  # by the sensor at *slot*. Yields the effects of that to the block.
  def presence(slot : Slot, appearances : Set(WWID), & : Effect ->) : Nil
    unless surface = @surfaces[slot]?
      Log.debug { "presence was called for a slot that is absent" }
      return
    end

    unless surface.is_a?(Sensor)
      Log.debug { "presence was called for an appearance" }
      return
    end

    if review? &.presence(slot, surface, appearances)
      yield ViewChange.new(@view)
    end
  end

  # :nodoc:
  def receive(act : StimulusPresence | StimulusAbsence, & : Effect ->) : Nil
    unless act.sensor.conid == @conid
      Log.debug { "reject stimulus presence: conid of #{act.sensor} != my #{@conid}" }
      return
    end

    unless surface = @surfaces[act.sensor.slot]?
      Log.debug { "reject stimulus presence: slot of #{act.sensor} absent" }
      return
    end

    unless surface.is_a?(Sensor)
      Log.debug { "reject stimulus presence: slot of #{act.sensor} is no longer a sensor" }
      return
    end

    if review? &.after(surface, act)
      yield ViewChange.new(@view)
    end
  end

  # :nodoc:
  def receive(act : StimulusRequest, & : Effect ->) : Nil
    unless act.appearance.conid == @conid
      Log.debug { "reject stimulus request: conid of #{act.appearance} != my #{@conid}" }
      return
    end

    unless surface = @surfaces[act.appearance.slot]?
      Log.debug { "reject stimulus request: slot of #{act.appearance} absent" }
      return
    end

    unless surface.is_a?(Appearance)
      Log.debug { "reject stimulus request: slot of #{act.appearance} is no longer an appearance" }
      return
    end

    appearance = IWWID.new(act.appearance, @instants[act.appearance.slot])

    yield Stimulation.new(surface, act.sensor, appearance)
  end

  # :nodoc:
  def receive(act : StimulusResponse, & : Effect ->) : Nil
    # Stimulus response is only emitted when a sensor is inserted *after* an
    # appearance; therefore, the sensor must be *newer* than the appearance.
    unless act.sensor.instant > act.appearance.instant
      Log.debug { "reject stimulus response: expected stimulus presence" }
      return
    end

    unless act.sensor.conid == @conid
      Log.debug { "reject stimulus response: conid of #{act.sensor} != my #{@conid}" }
      return
    end

    unless surface = @surfaces[act.sensor.slot]?
      Log.debug { "reject stimulus response: slot of #{act.sensor} absent" }
      return
    end

    unless surface.is_a?(Sensor)
      Log.debug { "reject stimulus response: slot of #{act.sensor} is no longer a sensor" }
      return
    end

    instant = @instants[act.sensor.slot]

    unless act.sensor.instant == instant
      Log.debug { "reject stimulus response: instant of #{act.sensor} is outdated (currently #{instant})" }
      return
    end

    if review? &.after(surface, act.to_stimulus_presence)
      yield ViewChange.new(@view)
    end
  end

  {% if flag?(:docs) %}
    # Handles the given activation *act*. Yields the effects of that to
    # the block.
    def receive(act : Activation, & : Effect ->) : Nil
    end
  {% end %}
end

# The main way you can obtain a termspace view is by polling: just call `view`.
# If you want to be woken up on possible view change, you can use the alert
# callback for that.
class Conn
  Log = ::Log.for(self)

  @sub : IChat::Subscribe
  @unsub : IChat::Unsubscribe

  # :nodoc:
  class SyncAtomBuffer(N)
    include IAtomAppend
    include Enumerable(Atom)

    def initialize
      @buckets = Slice(Array(Atom)).new(N) { [] of Atom }
      @locks = Slice(Mutex).new(N) { Mutex.new }
    end

    def <<(atom : Atom) : self
      bucket = atom.@blk0 % N

      @locks[bucket].synchronize do
        @buckets[bucket] << atom
      end

      self
    end

    # WARNING: this method is not thread-safe. Make sure all `<<` calls terminate
    # before calling it.
    def each(& : Atom ->) : Nil
      @buckets.each do |bucket|
        bucket.each { |atom| yield atom }
      end
    end

    # WARNING: this method is not thread-safe. Make sure all `<<` calls terminate
    # before calling it.
    def clear : Nil
      @buckets.each(&.clear)
    end
  end

  # No. of buffer buckets in the connection's atom buffer (before atoms are
  # transferred to the atom set).
  NBUFBUCKETS = 32

  # WARNING: the implementations of *atoms* and *chat* must be thread-safe. *alert*
  # must also be thread-safe.
  def initialize(@atoms : IAtomSet, @chat : IChat(Activation), @alert : Conn ->)
    @node = Node.new
    @relook = {} of Slot => Channel(Nil)
    @adds = SyncAtomBuffer(NBUFBUCKETS).new
    @dels = SyncAtomBuffer(NBUFBUCKETS).new
    @lock = Mutex.new # < protects all of the above

    @sub, @unsub = @chat.connect(@node.conid, &->receive(Activation))
  end

  # :ditto:
  def self.new(*args, **kwargs, &alert : Conn ->) : Conn
    new(*args, alert, **kwargs)
  end

  private def receive(act : Activation) : Nil
    Log.trace { "received #{act} from chat" }

    transaction &.receive(act)
  end

  # NOTE: we split surface addition/removal into two phases: insert() and
  # activate(). This is needed so that we finish insertion before actually
  # querying the termspace. Otherwise the result of queries would be order
  # dependent if someone inserts a sensor and an appearance simultaneously
  # that can excite each other.

  private def insert(adds, dels, effect : SurfaceAddition) : Nil
    effect.surface.atoms_to(effect.id.wwid, target: adds)

    return unless surface = effect.surface.as?(Sensor)
    return unless period = surface.relook?

    slot = effect.id.slot
    cancel = Channel(Nil).new

    @lock.synchronize do
      @relook[slot] = cancel
    end

    spawn relook(slot, cancel, period, surface)
  end

  private def insert(adds, dels, effect : SurfaceDeletion) : Nil
    if surface = effect.surface.as?(Sensor)
      if surface.relook?
        @lock.synchronize do
          cancel = @relook.delete(effect.id.slot) || raise "BUG: relook sensor with no cancel chan"
          cancel.close
        end
      end
    end

    effect.surface.atoms_to(effect.id.wwid, target: dels)
  end

  private def insert(adds, dels, effect : ViewChange | Stimulation) : Nil
  end

  private def insert(adds, dels, effects : Enumerable(Effect)) : Nil
    effects.each { |effect| insert(adds, dels, effect) }
  end

  private def activate(effect : SurfaceAddition) : Nil
    show(effect.id, effect.surface)
  end

  private def activate(effect : SurfaceDeletion) : Nil
    hide(effect.id, effect.surface)
  end

  private def activate(effect : ViewChange) : Nil
  end

  private def activate(effect : Stimulation) : Nil
    @chat.send(effect.sensor.conid, effect.to_stimulus_response)
  end

  private def activate(effects : Enumerable(Effect)) : Nil
    effects.each { |effect| activate(effect) }
  end

  # NOTE: we do not perceive *presence* during relook. We are only interested
  # in *absence* of appearances we already know about.

  private def relook(slot, cancel, period, surface) : Nil
    Log.trace { "relook loop running for #{surface} at #{slot}" }

    while true
      select
      when cancel.receive? # nil
        Log.trace { "relook loop for #{surface} at #{slot} ended due to cancel" }
        break
      when timeout(period)
        Log.trace { "relook #{surface} at #{slot}" }

        relook(slot, surface)
      end
    end
  end

  private def relook(slot : Slot, surface : Sensor) : Nil
    appearances = surface.complement_set(@atoms)

    Log.trace { "relook resulted in #{appearances.size} appearance(s)" }

    transaction &.presence(slot, appearances)
  end

  private def show(id : IWWID, surface : Sensor) : Nil
    appearances = surface.complement_set(@atoms)
    appearances.each do |appearance|
      # If appearance is owned by a more recent conn than mine then it probably
      # already sent me stimulus presence. No need to overload the network.
      if appearance > id.wwid
        Log.trace { "skip appearance #{appearance} because it will probably notify me" }
        next
      end

      act = StimulusRequest.new(id, appearance)

      Log.trace { "send activation #{act} to #{appearance.conid}" }

      @chat.send(appearance.conid, act)
    end
  end

  private def show(id : IWWID, surface : Appearance) : Nil
    sensors = surface.complement_set(@atoms)
    sensors.each do |sensor|
      # If sensor is owned by a more recent conn than mine then it probably
      # already saw me. No need to overload the network.
      if sensor > id.wwid
        Log.trace { "skip sensor #{sensor} because it probably saw me" }
        next
      end

      act = StimulusPresence.new(sensor, id, surface.value)

      Log.trace { "send activation #{act} to #{sensor.conid}" }

      @chat.send(sensor.conid, act)
    end
  end

  private def hide(id : IWWID, surface : Sensor) : Nil
  end

  private def hide(id : IWWID, surface : Appearance) : Nil
    sensors = surface.complement_set(@atoms)
    sensors.each do |sensor|
      act = StimulusAbsence.new(sensor, id)

      Log.trace { "send activation #{act} to #{sensor.conid}" }

      @chat.send(sensor.conid, act)
    end
  end

  private def handle(effects : Indexable(Effect)) : Nil
    return if effects.empty?

    Log.trace { "handling #{effects.size} effect(s)" }

    # The lock makes sure only one handle() owns @adds/@dels throughout
    # the transaction; thus only its atoms are in there, and we clean
    # up afterwards.
    @lock.synchronize do
      insert(@adds, @dels, effects)

      @atoms.commit(@adds, @dels)
    ensure
      @adds.clear
      @dels.clear
    end

    activate(effects)

    return unless effects.any?(ViewChange)

    Log.trace { "effects contained ViewChange, call alert" }

    @alert.call(self)
  end

  # Returns the latest view of the termspace according to this connection.
  #
  # This method is thread-safe.
  def view : View
    @lock.synchronize { @node.view }
  end

  # Yields each occupied slot and the corresponding surface.
  #
  # This method is thread-safe, but will block until all surfaces have
  # been yielded. So you cannot e.g. call this method recursively.
  def each(& : Slot, Surface ->) : Nil
    @lock.synchronize do
      @node.each do |wwid, surface|
        yield wwid.slot, surface
      end
    end
  end

  # Silently inserts the atoms of this connection into the termspace.
  #
  # This method is thread-safe.
  def summon : Nil
    Log.trace { "summon" }

    @sub.call
    @lock.synchronize do
      @node.each do |wwid, surface|
        surface.atoms_to(wwid, @atoms)
      end
    end
  end

  # Silently removes the atoms of this connection from the termspace.
  #
  # This method is thread-safe.
  def dismiss : Nil
    Log.trace { "dismiss" }

    @unsub.call
    @lock.synchronize do
      @node.each do |wwid, surface|
        surface.each_atom(wwid) { |atom| @atoms.delete(atom) }
      end
    end
  end

  # Actions that can be grouped in a transaction.
  module Action
    # :nodoc:
    alias Any = Put | Delete | Receive | Presence

    # See `Conn#[]=`.
    record Put, slot : Slot, surface : Surface

    # See `Conn#delete`.
    record Delete, slot : Slot

    # :nodoc:
    record Receive, act : Activation

    # :nodoc:
    record Presence, slot : Slot, appearances : Set(WWID)
  end

  struct Commit
    # :nodoc:
    getter actions = [] of Action::Any

    {% for action in Action::Any.union_types %}
      {% name = action.id.split("::")[-1].downcase %}

      # See `Action`.
      def {{name.id}}(*args, **kwargs) : Nil
        @actions << {{action}}.new(*args, **kwargs)
      end
    {% end %}
  end

  # Transactions allow to group actions together so that they are sent to
  # the termspace as one big message vs. many small ones.
  def transaction(& : Commit ->) : Nil
    Log.trace { "transaction is collecting effects" }

    effects = @lock.synchronize do
      commit = Commit.new
      yield commit

      Log.trace { "transaction will process #{commit.actions.size} action(s)" }

      effects_ = Stack(Effect).new

      commit.actions.each do |action|
        case action
        in Action::Put
          @node.put(action.slot, action.surface) { |effect| effects_ << effect }
        in Action::Delete
          @node.delete(action.slot) { |effect| effects_ << effect }
        in Action::Receive
          @node.receive(action.act) { |effect| effects_ << effect }
        in Action::Presence
          @node.presence(action.slot, action.appearances) { |effect| effects_ << effect }
        end
      end

      effects_
    end

    handle(effects)
  end

  # Removes all surfaces from this connection. This is not the same as `dismiss`
  # because `clear` removes surfaces "loudly", with view updates etc.
  #
  # This method is thread-safe.
  def clear : Nil
    transaction do |commit|
      slots = @node.slots
      slots.each { |slot| commit.delete(slot) }
    end
  end

  # Updates or inserts the given *surface* at *slot*.
  #
  # This method is thread-safe.
  def []=(slot : Slot, surface : Surface) : Surface
    transaction &.put(slot, surface)

    surface
  end

  # Removes the surface at *slot*.
  #
  # This method is thread-safe.
  def delete(slot : Slot) : Nil
    transaction &.delete(slot, surface)
  end
end

MT.spawn do
  set = MySet.new(1024)
  chat = SyncInMemoryChat(Activation).new
  n = Atomic(Int32).new(0)
  conn = Conn.new(set, chat) do |c|
    # pp c.view.dict_multisets
    if n.add(1) % 1000 == 0
      Log.notice { "#{n}" }
    end
  end
  conn.summon

  conn[0] = Sensor.new(Term.of(:+, :a_number, :b_number), relook: nil)
  # try to trigger races bugs etc
  spawn do
    (0...100_000).each do |n|
      conn[1] = Appearance.new(Term.of(:+, n, n))
    end
  end
  spawn do
    (100_000...200_000).each do |n|
      conn[2] = Appearance.new(Term.of(:+, n, n))
    end
  end
  # conn.each do |slot, surface|
  #   puts "Conn has #{slot} #{surface}"
  # end

  # conn.clear
end
sleep
