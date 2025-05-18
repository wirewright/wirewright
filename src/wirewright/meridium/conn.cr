module Ww::Meridium
  # A Meridium connection is a "shell" around a `Nucleus` that lets it communicate with
  # and influence the outside world through a so-called *termspace* (see also: `Tspace`).
  #
  # We don't *actually* give connections direct access to a termspace; instead,
  # they can "book an appointment" with the termspace. Connections are a bit like
  # workers in an office; and `Nucleus` is their "brain". A connection can schedule
  # a meeting with its "boss" -- the termspace; and report progress to it, whenever
  # the termspace decides to hear about it.
  #
  # NOTE: All public methods are thread-safe.
  #
  # NOTE: You can obtain a termspace view by polling (`view`) or using the view callback
  # which is called with the latest view automatically. The view callback must be as quick
  # as possible. It is normally executed while the conn has exclusive access to the termspace.
  # Obviously other conns won't be able to make progress until you do while you're in
  # the views callback.#
  #
  # WARNING: You should also absolutely **not** make calls to other conns in
  # the view callback. This is likely to cause a deadlock.
  #
  # See `Tspace::InMemory`, `Tspace::Axis` for examples.
  class Conn
    include IConn
    include Tspace::Meetable

    # :nodoc:
    class ViewStreamer
      @latest : View::Version?

      def initialize(@dest : View ->)
        @lock = Sync::Mutex.new
      end

      def send(view : View)
        @lock.synchronize do
          return if (latest = @latest) && latest >= view.version

          @latest = view.version
          @dest.call(view)
        end
      end
    end

    # Returns the id of this connection.
    getter conid : WWID

    def initialize(@conid : WWID, @tspace : Tspace::IFrontend, @views : ViewStreamer)
      @baseline = Nucleus.new(@conid)
      @staging = @baseline
      @relook = {} of Slot => Channel(Nil)
      @lock = Sync::RWLock.new
    end

    def self.new(*args, **kwargs, &views : View ->) : self
      new(*args, ViewStreamer.new(views), **kwargs)
    end

    # Returns the latest view of the termspace for this connection.
    def view : View
      @lock.read { @staging.view }
    end

    # This method hosts the relook loop, executed in the relook fiber: all it
    # does is it calls relook() periodically, until canceled.
    #
    # NOTE: we do not perceive *presence* during relook. We are only interested
    # in *absence* of appearances we already know about.
    private def relook(slot, cancel, period, surface) : Nil
      Log.trace { "#{conid}: relook loop running for #{surface} at #{slot}" }

      while true
        select
        when cancel.receive? # nil
          Log.trace { "#{conid}: relook loop for #{surface} at #{slot} ended due to cancel" }
          break
        when timeout(period)
          Log.trace { "#{conid}: relook #{surface} at #{slot}" }
          relook(slot, surface)
        end
      end
    end

    # This method is called from the relook fiber. Here the relook fiber requests
    # rendezvous with the termspace fiber, through `Relook`.
    private def relook(slot : Slot, surface : Sensor) : Nil
      @tspace.book(Relook.new(self, slot, surface))
    end

    # :nodoc:
    #
    # Conn's companion object that can schedule its own meetings with the "boss" --
    # tspace -- as well, on behalf of the connection. After doing its own business,
    # relook yields the floor to the connection.
    class Relook
      include Tspace::Meetable

      def initialize(@conn : Conn, @slot : Slot, @surface : Sensor)
      end

      def conid : WWID
        @conn.conid
      end

      def meet(tspace : Tspace) : Nil
        appearances = @surface.complement_set(tspace.presences(@conn))

        Log.trace { "#{conid}: relook resulted in #{appearances.size} appearance(s)" }

        @conn.presence(@slot, appearances)
        @conn.meet(tspace)
      end
    end

    # :nodoc:
    protected def presence(slot : Slot, appearances : Set(WWID))
      @lock.write do
        @staging = @staging.presence(slot, appearances)
      end
    end

    def meet(tspace : Tspace) : Nil
      effects = Stack(Effect).new

      @lock.write do
        Log.trace { "#{@staging.conid}: begin meeting for #{@baseline.state}->#{@staging.state}" }

        @baseline = @staging = @baseline.swap(@staging) do |effect|
          effects << effect
        end

        if effects.empty?
          Log.trace { "#{@staging.conid}: meeting resulted in no effects" }
          return
        end
      end

      Log.trace { "#{@staging.conid}: meeting resulted in effect(s): #{effects.join(&.inspect)}" }

      manage(tspace, effects)
      push(tspace, effects)
      notify(tspace, effects)
    end

    private def manage(tspace, effects : Enumerable) : Nil
      effects.each { |effect| manage(tspace, effect) }
    end

    private def manage(tspace, effect : Subscribed) : Nil
      unless conid == effect.conid
        raise ArgumentError.new("unexpected conid in Subscribed")
      end

      tspace.subscribe(self)
    end

    private def manage(tspace, effect : Unsubscribed) : Nil
      unless conid == effect.conid
        raise ArgumentError.new("unexpected conid in Unsubscribed")
      end

      tspace.unsubscribe(self)
    end

    private def manage(tspace, effect) : Nil
    end

    private def push(tspace, effects : Enumerable) : Nil
      tspace.transaction(self) do |add, del|
        effects.each { |effect| push(add, del, effect) }
      end
    end

    private def push(add, del, effect : SurfaceAdded) : Nil
      Log.trace { "#{conid}: push effect #{effect}" }

      surface = effect.surface
      surface.each_atom(effect.id.wwid) { |atom| add.call(atom) }

      return unless surface.is_a?(Sensor)
      return unless period = surface.relook?

      slot = effect.id.slot
      cancel = Channel(Nil).new
      @lock.write { @relook[slot] = cancel }

      spawn relook(slot, cancel, period, surface)
    end

    private def push(add, del, effect : SurfaceRemoved) : Nil
      Log.trace { "#{conid}: push effect #{effect}" }

      surface = effect.surface
      surface.each_atom(effect.id.wwid) { |atom| del.call(atom) }

      return unless surface.is_a?(Sensor)
      return unless surface.relook?

      cancel = @relook.delete(effect.id.slot).not_nil!("slot-relook discrepancy")
      cancel.close
    end

    private def push(add, del, effect) : Nil
    end

    private def notify(tspace, effects : Enumerable) : Nil
      effects.each { |effect| notify(tspace, effect) }
    end

    private def notify(tspace, effect : SurfaceAdded) : Nil
      show(tspace, effect.id, effect.surface)
    end

    private def notify(tspace, effect : SurfaceRemoved) : Nil
      hide(tspace, effect.id, effect.surface)
    end

    private def notify(tspace, effect : Replied) : Nil
      surface = effect.surface

      act = StimulusResponse.new(effect.sensor, effect.appearance, surface.value)
      if secret = surface.secret?
        act = act.to_secure?(Secure::Alg::ChaCha20_Poly1305, secret)
        unless act
          Log.warn { "#{conid}: will not reply, failed to encrypt" }
          return
        end
      end

      tspace.send(self, act)
    end

    private def notify(tspace, effect : ViewChanged)
      @views.send(effect.view)
    end

    private def notify(tspace, effect) : Nil
    end

    # Tells the termspace about *surface*.
    private def show(tspace, id : IWWID, surface : Sensor) : Nil
      Log.trace { "#{conid}: show #{surface} to the termspace" }

      appearances = surface.complement_set(tspace.presences(self))
      appearances.each do |appearance|
        tspace.send(self, StimulusRequest.new(id, appearance))
      end
    end

    # :ditto:
    private def show(tspace, id : IWWID, surface : Appearance) : Nil
      Log.trace { "#{conid}: show #{surface} to the termspace" }

      sensors = surface.complement_set(tspace.presences(self))
      sensors.each do |sensor|
        act = StimulusPresence.new(sensor, id, surface.value)
        if secret = surface.secret?
          act = act.to_secure?(Secure::Alg::ChaCha20_Poly1305, secret)
          unless act
            Log.warn { "#{conid}: will not send stimulus presence, failed to encrypt" }
            return
          end
        end

        tspace.send(self, act)
      end
    end

    # Tells the termspace that *surface* is no longer present.
    private def hide(tspace, id : IWWID, surface : Sensor) : Nil
    end

    # :ditto:
    private def hide(tspace, id : IWWID, surface : Appearance) : Nil
      Log.trace { "#{conid}: hide #{surface} from the termspace" }

      sensors = surface.complement_set(tspace.presences(self))
      sensors.each do |sensor|
        tspace.send(self, StimulusAbsence.new(sensor, id))
      end
    end

    def receive(tspace : Tspace, act : Activation) : Nil
      Log.trace { "receive activation #{act} from the termspace" }

      @lock.write do
        @staging = @staging.receive(act)
      end

      meet(tspace)
    end

    # Yields each occupied slot and the corresponding surface.
    #
    # This method will block until all surfaces have been yielded. So you
    # cannot e.g. call this method recursively.
    def each(& : Slot, Surface ->) : Nil
      @lock.read do
        @staging.each { |id, surface| yield id.slot, surface }
      end
    end

    # Returns `true` if the termspace appears to be aware of the surface currently
    # occupying *slot*. Returns `false` otherwise.
    #
    # Note that this can only *truly* be known through user-level feedback. For all
    # we know, we could be talking to emptyness. We can only know whether we *tried*
    # to talk and nothing in particular failed during the process.
    def sync?(slot : Slot) : Bool
      @lock.read do
        return false unless @baseline.state.online? && @staging.state.online?
        return false unless x = @baseline[slot]?
        return false unless y = @staging[slot]?

        x == y && @baseline.iwwid(slot) == @staging.iwwid(slot)
      end
    end

    # Inserts the surfaces of this connection into the termspace.
    def summon : Nil
      Log.trace { "#{conid}: summoned" }

      @lock.write { @staging = @staging.summon }
      @tspace.book(self)
    end

    # Removes the surfaces of this connection from the termspace.
    def dismiss : Nil
      Log.trace { "#{conid}: dismissed" }

      @lock.write { @staging = @staging.dismiss }
      @tspace.book(self)
    end

    def online : Nil
      Log.trace { "#{conid}: online" }

      @lock.write { @staging = @staging.online }
      @tspace.book(self)
    end

    def offline : Nil
      Log.trace { "#{conid}: offline" }

      # Note how we take over baseline merging here for a moment.
      # Normally it is the termspace that does this.
      view = @lock.write do
        @baseline = @staging = @staging.offline
        @staging.view
      end

      @views.send(view)
    end

    def connect : Nil
      @tspace.connect(self)
    end

    def disconnect : Nil
      @tspace.disconnect(self)
    end

    class Txn
      # :nodoc:
      def initialize(@head : Nucleus)
      end

      # See `Nucleus#put`.
      def put(slot : Slot, surface : Surface) : Nil
        @head = @head.put(slot, surface)
      end

      # See `Nucleus#delete`.
      def delete(slot : Slot) : Nil
        @head = @head.delete(slot)
      end
    end

    # Transactions allow to group actions together so that they are sent to
    # the termspace as one big message vs. many small ones.
    def transaction(& : Txn ->) : Nil
      Log.trace { "#{conid}: begin transaction" }

      @lock.write do
        yield txn = Txn.new(@staging)

        if @staging.same?(txn.@head)
          Log.trace { "#{conid}: transaction resulted in no change" }
          return
        end

        @staging = txn.@head
      end

      Log.trace { "#{conid}: transaction: book a meeting with tspace" }

      @tspace.book(self)
    end
  end
end
