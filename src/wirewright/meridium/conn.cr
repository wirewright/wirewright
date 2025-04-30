module Ww::Meridium
  # A Meridium connection is a "shell" around a `Node` that lets it communicate &
  # influence the outside world through an `IAtomSet` and an `IActivationChat`.
  #
  # NOTE: The main way you can obtain a termspace view is by polling: just call
  # `view`. If you want to be woken up on possible view change, you can use
  # the alert callback for that.
  class Conn
    Log = ::Log.for(self)

    @sub : IActivationChat::Subscribe
    @unsub : IActivationChat::Unsubscribe

    # WARNING: the implementations of *atoms* and *chat* must be thread-safe. *alert*
    # must also be thread-safe.
    def initialize(@atoms : IAtomSet, @chat : IActivationChat, @alert : Conn ->)
      @node = Node.new
      @relook = {} of Slot => Channel(Nil)
      @effects = Stack(Effect).new
      @summoned = false
      @lock = Mutex.new # < protects all of the above

      @sub, @unsub = @chat.connect(@node.conid, &->receive(Activation))
    end

    # WARNING: the implementation of *tspace* must be thread-safe. *alert* must
    # also be thread-safe.
    def initialize(tspace : Tspace, alert : Conn ->)
      initialize(tspace, tspace, alert)
    end

    # :ditto:
    def self.new(*args, **kwargs, &alert : Conn ->) : Conn
      new(*args, alert, **kwargs)
    end

    private def receive(act : Activation) : Nil
      Log.trace { "#{@node.conid}: received #{act} from chat" }

      transaction &.receive(act)
    end

    # NOTE: we split surface addition/removal into two phases: insert() and
    # activate(). This is needed so that we finish insertion before actually
    # querying the termspace. Otherwise the result of queries would be order
    # dependent if someone inserts a sensor and an appearance simultaneously
    # that can excite each other.

    # WARNING: assumes `@lock` is taken!
    private def insert(add, del, effect : SurfaceAddition) : Nil
      effect.surface.each_atom(effect.id.wwid) do |atom|
        add.call(atom)
      end

      return unless surface = effect.surface.as?(Sensor)
      return unless period = surface.relook?

      slot = effect.id.slot
      cancel = Channel(Nil).new

      @relook[slot] = cancel

      spawn relook(slot, cancel, period, surface)
    end

    # WARNING: assumes `@lock` is taken!
    private def insert(add, del, effect : SurfaceDeletion) : Nil
      if surface = effect.surface.as?(Sensor)
        if surface.relook?
          cancel = @relook.delete(effect.id.slot) || raise "BUG: relook sensor with no cancel chan"
          cancel.close
        end
      end

      effect.surface.each_atom(effect.id.wwid) do |atom|
        del.call(atom)
      end
    end

    # WARNING: assumes `@lock` is taken!
    private def insert(add, del, effect : ViewChange | Stimulation) : Nil
    end

    # WARNING: assumes `@lock` is taken!
    private def insert(add, del, effects : Enumerable(Effect)) : Nil
      effects.each { |effect| insert(add, del, effect) }
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
      Log.trace { "#{@node.conid}: relook loop running for #{surface} at #{slot}" }

      while true
        select
        when cancel.receive? # nil
          Log.trace { "#{@node.conid}: relook loop for #{surface} at #{slot} ended due to cancel" }
          break
        when timeout(period)
          Log.trace { "#{@node.conid}: relook #{surface} at #{slot}" }

          relook(slot, surface)
        end
      end
    end

    private def relook(slot : Slot, surface : Sensor) : Nil
      appearances = surface.complement_set(@atoms)

      Log.trace { "#{@node.conid}: relook resulted in #{appearances.size} appearance(s)" }

      transaction &.presence(slot, appearances)
    end

    private def show(id : IWWID, surface : Sensor) : Nil
      appearances = surface.complement_set(@atoms)
      appearances.each do |appearance|
        act = StimulusRequest.new(id, appearance)

        Log.trace { "#{@node.conid}: send activation #{act} to #{appearance.conid}" }

        @chat.send(appearance.conid, act)
      end
    end

    private def show(id : IWWID, surface : Appearance) : Nil
      sensors = surface.complement_set(@atoms)
      sensors.each do |sensor|
        act = StimulusPresence.new(sensor, id, surface.value)

        Log.trace { "#{@node.conid}: send activation #{act} to #{sensor.conid}" }

        @chat.send(sensor.conid, act)
      end
    end

    private def hide(id : IWWID, surface : Sensor) : Nil
    end

    private def hide(id : IWWID, surface : Appearance) : Nil
      sensors = surface.complement_set(@atoms)
      sensors.each do |sensor|
        act = StimulusAbsence.new(sensor, id)

        Log.trace { "#{@node.conid}: send activation #{act} to #{sensor.conid}" }

        @chat.send(sensor.conid, act)
      end
    end

    private def propagate : Nil
      handled = @lock.synchronize do
        return if @effects.empty?

        unless @summoned
          Log.debug do
            "#{@node.conid}: propagate aborted because the conn was not summoned yet, #{@effects.size} stale effect(s)"
          end
          return
        end

        Log.trace { "#{@node.conid}: handling #{@effects.size} effect(s)" }

        @atoms.transaction do |add, del|
          insert(add, del, @effects)
        end

        # Clear ("consume") all effects that we've handled.
        @effects, _ = Stack(Effect).new, @effects
      end

      activate(handled)

      if handled.any?(ViewChange)
        @alert.call(self)
      end
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

    # Inserts the atoms of this connection into the termspace.
    #
    # This method is thread-safe.
    def summon : Nil
      @lock.synchronize do
        return if @summoned

        Log.trace { "#{@node.conid}: summon" }

        @summoned = true

        @sub.call
        @atoms.transaction do |add, del|
          @node.each do |wwid, surface|
            surface.each_atom(wwid) { |atom| add.call(atom) }
          end
        end
      end

      propagate
    end

    # Silently removes the atoms of this connection from the termspace.
    #
    # This method is thread-safe.
    def dismiss : Nil
      @lock.synchronize do
        return unless @summoned

        Log.trace { "#{@node.conid}: dismiss" }

        @summoned = false

        @unsub.call
        @atoms.transaction do |add, del|
          @node.each do |wwid, surface|
            surface.each_atom(wwid) { |atom| del.call(atom) }
          end
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
      @lock.synchronize do
        Log.trace { "#{@node.conid}: transaction is collecting effects" }

        commit = Commit.new
        yield commit

        if commit.actions.size < 8
          Log.trace { "#{@node.conid}: transaction will process actions: #{commit.actions.join("; ")}" }
        else
          Log.trace { "#{@node.conid}: transaction will process #{commit.actions.size} action(s)" }
        end

        commit.actions.each do |action|
          case action
          in Action::Put
            @node.put(action.slot, action.surface) { |effect| @effects << effect }
          in Action::Delete
            @node.delete(action.slot) { |effect| @effects << effect }
          in Action::Receive
            @node.receive(action.act) { |effect| @effects << effect }
          in Action::Presence
            @node.presence(action.slot, action.appearances) { |effect| @effects << effect }
          end
        end
      end

      propagate
    end

    # Removes all surfaces from this connection. This is not the same as `dismiss`
    # because `clear` removes surfaces "loudly", with view updates etc.
    #
    # This method is thread-safe.
    def clear : Nil
      transaction do |commit|
        @node.each { |slot, _| commit.delete(slot) }
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
      transaction &.delete(slot)
    end
  end
end
