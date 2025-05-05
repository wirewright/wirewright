module Ww::Meridium
  # A Meridium connection is a "shell" around a `Node` that lets it communicate with
  # and influence the outside world through a so-called *termspace*.
  #
  # We don't *actually* give connections direct access to a termspace; instead,
  # they can "book an appointment" with the termspace. Connections are a bit like
  # workers in an office; and `Node` is their "brain". A connection can schedule
  # a meeting with its "boss" -- the termspace; and report progress to it, whenever
  # the termspace decides to hear about it.
  #
  # NOTE: All public methods are thread-safe.
  #
  # NOTE: The main way you can obtain a termspace view is by polling: just call
  # `view`. If you want to be woken up on possible view change, you can use
  # the alert callback for that.
  class Conn
    include IConn
    include Tspace::Meetable

    Log = ::Log.for(self)

    # :nodoc:
    enum State : UInt8
      SyncClosed
      UnsyncClosed
      UnsyncOpen
      SyncOpen
    end

    def initialize(conid : WWID, @meeting : Tspace::IBookMeeting, @alert : self ->)
      @node = Node.new(conid)
      @relook = {} of Slot => Channel(Nil)
      @effects = Stack(Effect).new
      @state = State::SyncClosed
      @lock = Mutex.new
    end

    # :ditto:
    def self.new(*args, **kwargs, &alert : self ->) : self
      new(*args, alert, **kwargs)
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
      @meeting.book(Relook.new(self, slot, surface))
    end

    # :nodoc:
    #
    # Conn's companion object that can schedule its own meetings with the "boss"
    # as well, on behalf of the connection. After doing its own business, relook
    # yields the floor to the connection.
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
      @lock.synchronize do
        @node.presence(slot, appearances) { |effect| capture(effect) }
      end
    end

    # :nodoc:
    def meet(tspace : Tspace) : Nil
      acts = Stack(Activation).new

      alert = @lock.synchronize do
        Log.trace { "#{conid}: begin meeting with state=#{@state}" }

        case @state
        in .sync_closed?
          @effects.clear
          Log.debug { "#{conid}: state is sync_closed during meeting, abort meeting" }
          return
        in .unsync_closed?
          @state = State::SyncClosed
          tspace.unsubscribe(self)
          @effects.clear
          @node.clear { |effect| capture(effect) }
        in .unsync_open?
          @state = State::SyncOpen
          tspace.subscribe(self)
          @effects.clear
          @node.populate { |effect| capture(effect) }
        in .sync_open?
        end

        if @effects.empty?
          Log.trace { "#{conid}: nothing to say, end meeting" }
          return
        end

        report(tspace) { |act| acts << act }
      end

      # It is important that we call chat.send() outside of the lock; who
      # knows what it could be doing.
      if acts.present?
        Log.trace { "#{conid}: meeting: #{acts.size} activation(s) to send" }

        acts.each { |act| tspace.send(self, act) }
      end

      # Ditto for alert, we don't know what it'll do (most probably call
      # #view though).
      if alert
        Log.trace { "#{conid}: call alert" }

        @alert.call(self)
      end
    end

    # Reports about effects to *tspace*. Returns `true` if the view changed
    # during the report.
    #
    # NOTE: we split surface addition/removal into two phases: push() and
    # trigger(). This is needed so that we finish insertion before actually
    # querying the termspace. Otherwise the result of queries would be order
    # dependent if someone inserts a sensor and an appearance simultaneously
    # that can excite each other.
    #
    # WARNING: assumes @lock is taken!
    private def report(tspace, & : Activation ->) : Bool
      push(tspace)
      trigger(tspace) { |*args| yield *args }
      changed = @effects.any?(ViewChange)
      @effects.clear
      changed
    end

    private def push(tspace) : Nil
      Log.trace { "#{conid}: push #{@effects.size} effect(s)" }

      tspace.transaction(self) do |add, del|
        @effects.each { |effect| push(add, del, effect) }
      end
    end

    private def push(add, del, effect : SurfaceAddition) : Nil
      effect.surface.each_atom(effect.id.wwid) { |atom| add.call(atom) }
    end

    private def push(add, del, effect : SurfaceDeletion) : Nil
      effect.surface.each_atom(effect.id.wwid) { |atom| del.call(atom) }
    end

    private def push(add, del, effect : ViewChange | Stimulation) : Nil
    end

    private def trigger(tspace, &) : Nil
      Log.trace { "#{conid}: trigger #{@effects.size} effect(s)" }

      @effects.each do |effect|
        trigger(tspace, effect) { |*args| yield *args }
      end
    end

    private def trigger(tspace, effect : SurfaceAddition, &) : Nil
      show(tspace, effect.id, effect.surface) { |*args| yield *args }
    end

    private def trigger(tspace, effect : SurfaceDeletion, &) : Nil
      hide(tspace, effect.id, effect.surface) { |*args| yield *args }
    end

    private def trigger(tspace, effect : ViewChange, &) : Nil
    end

    private def trigger(tspace, effect : Stimulation, &) : Nil
      yield effect.to_stimulus_response
    end

    # Tells the termspace about *surface*.
    private def show(tspace, id : IWWID, surface : Sensor, &) : Nil
      appearances = surface.complement_set(tspace.presences(self))
      appearances.each { |appearance| yield StimulusRequest.new(id, appearance) }
    end

    # :ditto:
    private def show(tspace, id : IWWID, surface : Appearance, &) : Nil
      sensors = surface.complement_set(tspace.presences(self))
      sensors.each { |sensor| yield StimulusPresence.new(sensor, id, surface.value) }
    end

    # Tells the termspace that *surface* is no longer present.
    private def hide(tspace, id : IWWID, surface : Sensor, &) : Nil
    end

    # :ditto:
    private def hide(tspace, id : IWWID, surface : Appearance, &) : Nil
      sensors = surface.complement_set(tspace.presences(self))
      sensors.each { |sensor| yield StimulusAbsence.new(sensor, id) }
    end

    private def capture(effect : SurfaceAddition) : Nil
      @effects << effect

      return unless surface = effect.surface.as?(Sensor)
      return unless period = surface.relook?

      slot = effect.id.slot

      return if @relook.has_key?(slot)

      cancel = Channel(Nil).new

      @relook[slot] = cancel

      spawn relook(slot, cancel, period, surface)
    end

    private def capture(effect : SurfaceDeletion) : Nil
      @effects << effect

      return unless surface = effect.surface.as?(Sensor)

      if surface.relook?
        cancel = @relook.delete(effect.id.slot) || raise "BUG: relook sensor with no cancel chan"
        cancel.close
      end
    end

    private def capture(effect) : Nil
      @effects << effect
    end

    # :nodoc:
    #
    # NOTE: this isn't the same as summon! Summon assumes we're connected and
    # does some state "shorthand" stuff. I.e. summon(unsync close) -> open
    # without interacting with the termspace.
    def online : Nil
      Log.trace { "#{conid}: online" }

      @lock.synchronize { @state = State::UnsyncOpen }
      @meeting.book(self)
    end

    # :nodoc:
    #
    # NOTE: this isn't the same as dismiss! Dismiss assumes we're connected
    # and will try to remove atoms etc. Here we assume we're disconnected
    # already, for whatever reason; and just do the necessary post-factum
    # state adjustments.
    def offline : Nil
      Log.trace { "#{conid}: offline" }

      @lock.synchronize { @state = State::SyncClosed }
    end

    # :nodoc:
    def receive(tspace : Tspace, act : Activation) : Nil
      @lock.synchronize do
        @node.receive(act) { |effect| capture(effect) }
      end

      meet(tspace)
    end

    # Returns the id of this connection.
    def conid : WWID
      @node.conid
    end

    # Returns the latest view of the termspace according to this connection.
    def view : View
      @lock.synchronize { @node.view }
    end

    # Yields each occupied slot and the corresponding surface.
    #
    # This method will block until all surfaces have been yielded. So you
    # cannot e.g. call this method recursively.
    def each(& : Slot, Surface ->) : Nil
      @lock.synchronize do
        @node.each { |id, surface| yield id.slot, surface }
      end
    end

    # Inserts the atoms of this connection into the termspace.
    def summon : Nil
      @lock.synchronize do
        case @state
        in .sync_closed?
          @state = State::UnsyncOpen
        in .unsync_closed?
          @state = State::SyncOpen
        in .unsync_open?, .sync_open?
          return
        end
      end

      Log.trace { "#{conid}: summon: book a meeting with tspace" }

      @meeting.book(self)
    end

    # Removes the atoms of this connection from the termspace.
    def dismiss : Nil
      @lock.synchronize do
        case @state
        in .sync_closed?, .unsync_closed?
          return
        in .unsync_open?
          @state = State::SyncClosed
        in .sync_open?
          @state = State::UnsyncClosed
        end
      end

      Log.trace { "#{conid}: dismiss: book a meeting with tspace" }

      @meeting.book(self)
    end

    # Actions that can be performed during a transaction.
    module Action
      # :nodoc:
      alias Any = Put | Delete | Receive

      # See `Conn#[]=`.
      record Put, slot : Slot, surface : Surface

      # See `Conn#delete`.
      record Delete, slot : Slot

      # :nodoc:
      record Receive, act : Activation
    end

    # Transaction object yielded by `transaction`.
    struct Txn
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
    def transaction(& : Txn ->) : Nil
      @lock.synchronize do
        Log.trace { "#{conid}: transaction is collecting effects" }

        txn = Txn.new
        yield txn

        if txn.actions.size < 8
          Log.trace { "#{conid}: transaction will process actions: #{txn.actions.join("; ")}" }
        else
          Log.trace { "#{conid}: transaction will process #{txn.actions.size} action(s)" }
        end

        txn.actions.each do |action|
          case action
          in Action::Put
            @node.put(action.slot, action.surface) { |effect| capture(effect) }
          in Action::Delete
            @node.delete(action.slot) { |effect| capture(effect) }
          in Action::Receive
          end
        end
      end

      Log.trace { "#{conid}: transaction: book a meeting with tspace" }

      @meeting.book(self)
    end
  end
end
