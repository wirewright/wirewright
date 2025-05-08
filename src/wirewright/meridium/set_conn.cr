module Ww::Meridium
  # A *set conn* sits on top of a `Conn` and provides "set-like access" to it.
  #
  # The "elements" of a set conn are surfaces -- sensors and appearances. They are
  # no longer assigned an arbitrary numeric slot like in `Conn` -- by you; but rather,
  # their slot is assigned based on their *identity* -- by the set conn. In case of
  # sensors that would be their pattern, secret, and relook; in case of appearances,
  # that would be their value and secret.
  #
  # Since this entails the generation of new Conn slots on every change of e.g.
  # an appearance's value, what set conns do is they maintain a pool of free slots.
  #
  # When an appearance's value changes (let's say increments), its previous version
  # (with the old value) gets removed from the set conn; and its slot is released
  # to the slot pool. We then add to the set conn the appearance with the new
  # (incremented) value. This ends up taking the slot from the slot pool and reusing it,
  # which is beneficial primarily for sensors perceiving the appearance -- they
  # don't have to go through additional rounds of "un-seeing" the previous
  # appearance & its slot before seeing the new one (completely novel to them!)
  #
  # ```
  # tspace = Tspace::InMemory.new
  # conn = SetConn.new(WWID.new, tspace) do |view|
  #   # Do something non-blocking and thread-safe with view...
  # end
  #
  # # Initialize
  # conn.online
  # conn.summon
  #
  # # Do stuff
  # conn.transaction do |txn|
  #   txn << Appearance.new(Term.of("Hello World"))
  #   txn << Sensor.new(Term.of(:x_string))
  # end
  #
  # # Cleanup
  # conn.dismiss
  # conn.offline
  # ```
  class SetConn
    # Default capacity of SetConn's slot pool.
    #
    # See also: `SetConn`.
    DEFAULT_SLOT_POOL_CAPACITY = 16

    # You can optionally specify this set conn's *slot pool capacity*. Free slots
    # will be "burned" once this capacity is exceeded.
    def initialize(conid : WWID, tspace : Tspace::IFrontend, views : View ->, @slotcap = DEFAULT_SLOT_POOL_CAPACITY)
      @conn = Conn.new(conid, tspace, &views)
      @fresh = Slot.new(0)
      @free = Set(Slot).new
      @encoding = Bimap(Surface, Slot).new
      @lock = Mutex.new
    end

    def self.new(*args, **kwargs, &views : View ->) : SetConn
      new(*args, **kwargs, views: views)
    end

    # Delegates to the underlying `Conn`.
    delegate :summon, :dismiss, :online, :offline, :connect, :disconnect, to: @conn

    private def acquire_slot : Slot
      if slot = @free.first?
        @free.delete(slot)
        return slot
      end

      slot = @fresh
      @fresh += 1

      slot
    end

    private def release_slot(slot : Slot) : Nil
      if 0 < @slotcap <= @free.size
        (@slotcap..@free.size).each do
          @free.delete(@free.first)
        end
      end

      @free.add(slot)
    end

    protected def bind(surface : Surface) : Slot
      if slot = @encoding[surface]?
        return slot
      end

      @encoding[surface] = acquire_slot
    end

    protected def unbind(surface : Surface) : Slot?
      return unless slot = @encoding.delete(surface)

      release_slot(slot)

      slot
    end

    # Returns the surface currently occupying the given *slot*. Returns `nil`
    # if there is no such surface.
    def surface?(slot : Slot) : Surface?
      @lock.synchronize { @encoding[slot]? }
    end

    # See `Conn#sync?`.
    def sync?(surface : Surface) : Bool
      @lock.synchronize do
        return false unless slot = @encoding[surface]?

        @conn.sync?(slot)
      end
    end

    struct Txn
      # :nodoc:
      def initialize(@owner : SetConn, @conn : Conn::Txn)
      end

      # Adds *surface* to the set conn.
      def add(surface : Surface) : Nil
        slot = @owner.bind(surface)

        @conn.put(slot, surface)
      end

      # Alias of `add`. Returns `self`.
      def <<(surface : Surface) : self
        add(surface)

        self
      end

      # Removes *surface* from the set conn.
      def delete(surface : Surface) : Nil
        return unless slot = @owner.unbind(surface)

        @conn.delete(slot)
      end
    end

    # Yields a transaction object `Txn` to make changes to this set conn.
    def transaction(& : Txn ->) : Nil
      @lock.synchronize do
        @conn.transaction { |txn| yield Txn.new(self, txn) }
      end
    end

    def pretty_print(pp)
      @lock.synchronize do
        pp.list("SetConn{", @encoding, "}") do |surface, _|
          surface.pretty_print(pp)
        end
      end
    end
  end
end
