module Ww::Meridium
  # Activations are messages directed inward from chat to conn, and from
  # conn to its "nucleus", `Node`.
  alias Activation = StimulusPresence | StimulusAbsence | StimulusRequest | StimulusResponse

  record StimulusPresence, sensor : WWID, appearance : IWWID, stimulus : Term
  record StimulusAbsence, sensor : WWID, appearance : IWWID
  record StimulusRequest, sensor : IWWID, appearance : WWID
  record StimulusResponse, sensor : IWWID, appearance : IWWID, stimulus : Term do
    def to_stimulus_presence : StimulusPresence
      StimulusPresence.new(sensor.wwid, appearance, stimulus)
    end
  end

  # Effects are messages directed outward from a conn's "nucleus" `Node` to that
  # conn, and then perhaps to the chat (or trigger addition of atoms to the atom set).
  alias Effect = SurfaceAddition | SurfaceDeletion | ViewChange | Stimulation

  record ViewChange, view : View
  record SurfaceAddition, id : IWWID, surface : Surface
  record SurfaceDeletion, id : IWWID, surface : Surface
  record Stimulation, surface : Appearance, sensor : IWWID, appearance : IWWID do
    def to_stimulus_response : StimulusResponse
      StimulusResponse.new(sensor, appearance, surface.value)
    end
  end

  # The "nucleus" of a `Conn` which implements all of its core behavior.
  #
  # The goal of `Node` is to handle `Activation`s coming from the connection that
  # it is enclosed in; and to produce `Effect`s that are in turn handled by
  # the enclosing connection.
  #
  # Nodes maintain `view`s for their sensors.
  #
  # WARNING: all methods except `conid` are thread-**unsafe**. One of the goals
  # of `Conn` is to guard its node and make sure it is accessed in a thread-safe
  # manner; `Node` itself doesn't care.
  class Node
    Log = ::Log.for(self)

    # Returns the connection id of this node.
    #
    # This method is the only thread-safe one because nobody ever modifies the conid.
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
      @instants[slot] = instant = Instant.new
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
end
