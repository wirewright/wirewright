module Ww::Meridium
  # Activations are messages directed inward from termspace to conn, and from
  # conn to its "nucleus", `Node`.
  alias Activation = StimulusPresence | StimulusAbsence | StimulusRequest | StimulusResponse | SecureStimulusPresence | SecureStimulusResponse

  defcase StimulusPresence, sensor : WWID, appearance : IWWID, stimulus : Term do
    include InspectToS

    def sender : WWID
      appearance.conid
    end

    def receiver : WWID
      sensor.conid
    end

    def to_secure?(alg : Secure::Alg, secret : Term) : SecureStimulusPresence?
      return unless response = Secure.to_secure?(alg, secret, stimulus)

      iv, ciphertext = response

      SecureStimulusPresence.new(sensor, appearance, alg, iv, ciphertext)
    end
  end

  defcase StimulusAbsence, sensor : WWID, appearance : IWWID do
    include InspectToS

    def sender : WWID
      appearance.conid
    end

    def receiver : WWID
      sensor.conid
    end
  end

  defcase StimulusRequest, sensor : IWWID, appearance : WWID do
    include InspectToS

    def sender : WWID
      sensor.conid
    end

    def receiver : WWID
      appearance.conid
    end
  end

  defcase StimulusResponse, sensor : IWWID, appearance : IWWID, stimulus : Term do
    include InspectToS

    def sender : WWID
      appearance.conid
    end

    def receiver : WWID
      sensor.conid
    end

    def to_stimulus_presence : StimulusPresence
      StimulusPresence.new(sensor.wwid, appearance, stimulus)
    end

    def to_secure?(alg : Secure::Alg, secret : Term) : SecureStimulusResponse?
      return unless response = Secure.to_secure?(alg, secret, stimulus)

      iv, ciphertext = response

      SecureStimulusResponse.new(sensor, appearance, alg, iv, ciphertext)
    end
  end

  defcase SecureStimulusPresence, sensor : WWID, appearance : IWWID, alg : Secure::Alg, iv : Bytes, ciphertext : Bytes do
    include InspectToS

    def sender : WWID
      appearance.conid
    end

    def receiver : WWID
      sensor.conid
    end

    def to_insecure?(secret : Term) : StimulusPresence?
      return unless stimulus = Secure.from_secure?(alg, secret, iv, ciphertext)

      StimulusPresence.new(sensor, appearance, stimulus)
    end
  end

  defcase SecureStimulusResponse, sensor : IWWID, appearance : IWWID, alg : Secure::Alg, iv : Bytes, ciphertext : Bytes do
    include InspectToS

    def sender : WWID
      appearance.conid
    end

    def receiver : WWID
      sensor.conid
    end

    def to_stimulus_presence : SecureStimulusPresence
      SecureStimulusPresence.new(sensor.wwid, appearance, alg, iv, ciphertext)
    end
  end

  # Effects are messages directed outward from a conn's "nucleus" `Node` to that
  # conn, and then perhaps to the termspace.
  alias Effect = SurfaceAdded | SurfaceRemoved | ViewChanged | Replied | Subscribed | Unsubscribed

  defcase ViewChanged, view : View do
    include InspectToS
  end

  defcase Subscribed, conid : WWID do
    include InspectToS
  end

  defcase Unsubscribed, conid : WWID do
    include InspectToS
  end

  defcase SurfaceAdded, id : IWWID, surface : Surface do
    include InspectToS
  end

  defcase SurfaceRemoved, id : IWWID, surface : Surface do
    include InspectToS
  end

  defcase Replied, sensor : IWWID, appearance : IWWID, surface : Appearance do
    include InspectToS
  end

  # The "nucleus" of a `Conn` which implements all of its core behavior.
  #
  # The goal of `Node` is to handle `Activation`s coming from the connection that
  # it is enclosed in; and to produce `Effect`s that are in turn handled by
  # the enclosing connection.
  #
  # Nodes maintain `view`s for their sensors.
  #
  # Nodes are immutable for simplicity.
  class Node
    # :nodoc:
    NO_REPLIES = Pf::Map(Slot, Pf::Set(Replied)).new

    @[Flags]
    enum State : UInt8
      Online
      Summoned
    end

    # Returns the connection id of this node.
    getter conid : WWID

    # Returns the curren state of this node.
    getter state : State

    # Returns the current view of this node.
    getter view : View

    def initialize(@conid : WWID)
      unless @conid.conid?
        raise ArgumentError.new("expected WWID trunk as conid (slot must be 0)")
      end

      @clock = Instant.new(0)
      @view = View.new
      @state = State::None
      @surfaces = Pf::Map(Slot, Surface).new
      @instants = Pf::Map(Slot, Instant).new
      @replies = NO_REPLIES
    end

    def initialize(@conid, @state, @clock, @view, @surfaces, @instants, @replies)
    end

    private def_change

    # Returns `true` if this node contains no surfaces. Returns `false` otherwise.
    def empty? : Bool
      @surfaces.empty?
    end

    # Returns the surface at *slot*, if any. Returns `nil` otherwise.
    def []?(slot : Slot) : Surface?
      @surfaces[slot]?
    end

    # Returns the `WWID` currently associated with *slot*, or `nil` if *slot*
    # is absent.
    def wwid?(slot : Slot) : WWID?
      return unless @surfaces.has_key?(slot)

      @conid.with_slot(slot)
    end

    # Returns the `IWWID` currently associated with *slot*, or `nil` if *slot*
    # is absent.
    def iwwid?(slot : Slot) : IWWID?
      return unless wwid = wwid?(slot)

      IWWID.new(wwid, @instants[slot])
    end

    # Yields WWIDs and their corresponding surfaces.
    def each(& : WWID, Surface ->) : Nil
      @surfaces.each do |slot, surface|
        yield @conid.with_slot(slot), surface
      end
    end

    protected def insert(slot : Slot, surface : Surface) : Node
      change(
        surfaces: @surfaces.assoc(slot, surface),
        instants: @instants.assoc(slot, @clock),
        clock: @clock.succ,
        view: surface.is_a?(Sensor) ? @view.register(slot, surface) : @view,
        # NOTE: we do not insert into @replies here simply to save a tiny
        # bit of space. We don't know if an entry there is going to be needed.
      )
    end

    # Adjusts the state of this node to signal it's online now.
    def online : Node
      change(state: @state | State::Online)
    end

    # Adjusts the state of this node to signal it's offline now.
    def offline : Node
      change(state: @state & ~State::Online, view: @view.clear, replies: NO_REPLIES)
    end

    # Adjusts the state of this node to signal that it should join the termspace
    # now. Note that (obviously) it will join only if it is online. Otherwise it
    # will wait until it is online and only then join.
    def summon : Node
      change(state: @state | State::Summoned)
    end

    # Adjusts the state of this node to signal that it should leave the termspace
    # now. If online, this will gracefully remove all appearances and notify everybody
    # that the node left. If offline, this is a noop, since we assume offline to mean
    # "connection and all associated state lost".
    def dismiss : Node
      change(state: @state & ~State::Summoned, view: @view.clear, replies: NO_REPLIES)
    end

    # Updates or inserts the surface at *slot*. Returns the modified copy of `self`.
    def put(slot : Slot, surface : Surface) : Node
      delete(slot).insert(slot, surface)
    end

    # Removes the surface at *slot*. Returns the modified copy of `self`.
    def delete(slot : Slot) : Node
      return self unless surface = @surfaces[slot]?

      change(
        surfaces: @surfaces.dissoc(slot),
        instants: @instants.dissoc(slot),
        view: surface.is_a?(Sensor) ? @view.unregister(slot, surface) : @view,
        replies: surface.is_a?(Appearance) ? @replies.dissoc(slot) : @replies,
      )
    end

    # Updates the view by excluding appearances not in the given *appearances*
    # set. Returns the modified copy of `self`.
    def presence(slot : Slot, appearances : Set(WWID)) : Node
      unless surface = @surfaces[slot]?
        Log.debug { "presence was called for a slot that is absent" }
        return self
      end

      unless surface.is_a?(Sensor)
        Log.debug { "presence was called for an appearance" }
        return self
      end

      change(view: @view.presence(slot, surface, appearances))
    end

    # :nodoc:
    def receive(act : StimulusPresence | StimulusAbsence) : Node
      unless @conid == act.sensor.conid
        Log.debug { "reject stimulus presence: conid of #{act.sensor} != my #{@conid}" }
        return self
      end

      unless surface = @surfaces[act.sensor.slot]?
        Log.debug { "reject stimulus presence: slot of #{act.sensor} absent" }
        return self
      end

      unless surface.is_a?(Sensor)
        Log.debug { "reject stimulus presence: slot of #{act.sensor} is no longer a sensor" }
        return self
      end

      change(view: @view.after(surface, act))
    end

    # :nodoc:
    def receive(act : SecureStimulusPresence) : Node
      unless @conid == act.sensor.conid
        Log.debug { "reject secure stimulus presence: conid of #{act.sensor} != my #{@conid}" }
        return self
      end

      unless surface = @surfaces[act.sensor.slot]?
        Log.debug { "reject secure stimulus presence: slot of #{act.sensor} absent" }
        return self
      end

      unless surface.is_a?(Sensor)
        Log.debug { "reject secure stimulus presence: slot of #{act.sensor} is no longer a sensor" }
        return self
      end

      unless secret = surface.secret?
        Log.debug { "reject secure stimulus presence: sensor lacks secret" }
        return self
      end

      unless decrypted = act.to_insecure?(secret)
        Log.debug { "reject secure stimulus presence: invalid secret (decrypt failed)" }
        return self
      end

      change(view: @view.after(surface, decrypted))
    end

    # :nodoc:
    def receive(act : StimulusRequest) : Node
      return self unless @state.online?

      unless @conid == act.appearance.conid
        Log.debug { "reject stimulus request: conid of #{act.appearance} != my #{@conid}" }
        return self
      end

      slot = act.appearance.slot

      unless surface = @surfaces[slot]?
        Log.debug { "reject stimulus request: slot of #{act.appearance} absent" }
        return self
      end

      unless surface.is_a?(Appearance)
        Log.debug { "reject stimulus request: slot of #{act.appearance} is no longer an appearance" }
        return self
      end

      reply = Replied.new(act.sensor, IWWID.new(act.appearance, @instants[slot]), surface)

      replies0 = @replies
      replies1 = @replies.extend(slot, Pf::Set(Replied).new, &.add(reply))

      change(replies: replies1)
    end

    # :nodoc:
    def receive(act : StimulusResponse | SecureStimulusResponse) : Node
      unless act.sensor.conid == @conid
        Log.debug { "reject stimulus response: conid of #{act.sensor} != my #{@conid}" }
        return self
      end

      unless surface = @surfaces[act.sensor.slot]?
        Log.debug { "reject stimulus response: slot of #{act.sensor} absent" }
        return self
      end

      unless surface.is_a?(Sensor)
        Log.debug { "reject stimulus response: slot of #{act.sensor} is no longer a sensor" }
        return self
      end

      instant = @instants[act.sensor.slot]

      unless act.sensor.instant == instant
        Log.debug { "reject stimulus response: instant of #{act.sensor} is outdated (currently #{instant})" }
        return self
      end

      receive(act.to_stimulus_presence)
    end

    {% if flag?(:docs) %}
      # Handles the given activation *act*. Returns the modified copy of this node.
      def receive(act : Activation) : Node
      end
    {% end %}

    protected def join(& : Effect ->) : Node
      yield Subscribed.new(@conid)

      @surfaces.each do |slot, surface|
        yield SurfaceAdded.new(iwwid?(slot).not_nil!("slot-instance discrepancy"), surface)
      end

      change(replies: NO_REPLIES)
    end

    protected def leave(& : Effect ->) : Node
      yield Unsubscribed.new(@conid)

      @surfaces.each do |slot, surface|
        yield SurfaceRemoved.new(iwwid?(slot).not_nil!("slot-instance discrepancy"), surface)
      end

      change(replies: NO_REPLIES)
    end

    protected def reply(& : Replied ->) : Node
      @replies.each do |_, effects|
        effects.each { |effect| yield effect }
      end

      change(replies: NO_REPLIES)
    end

    # Logically replaces `self` -- assumed to be an older, established node --
    # with its *successor*. Yields any associated effects. Returns the modified
    # copy of *successor*.
    def swap(successor : Node, & : Effect ->) : Node
      s0, s1 = @state, successor.@state

      # online | offline -> offline
      return successor unless s1.online?

      # offline -> online
      unless s0.online?
        if s1.summoned?
          return successor.join { |effect| yield effect }
        end

        return successor
      end

      # online -> online
      if s0.summoned? && s1.summoned?
        @surfaces.each do |slot, lhs|
          next if lhs == successor.@surfaces[slot]?
          yield SurfaceRemoved.new(iwwid?(slot).not_nil!("slot-instance discrepancy"), lhs)
        end

        successor.@surfaces.each do |slot, rhs|
          next if @surfaces[slot]? == rhs
          yield SurfaceAdded.new(successor.iwwid?(slot).not_nil!("slot-instance discrepancy"), rhs)
        end

        successor.reply { |effect| yield effect }
      elsif s0.summoned?
        successor.leave { |effect| yield effect }
      elsif s1.summoned?
        successor.join { |effect| yield effect }
      else
        successor
      end
    ensure
      unless @view.version == successor.@view.version
        yield ViewChanged.new(successor.@view)
      end
    end
  end
end
