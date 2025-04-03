module Ww::Meridium
  # A *set conn* sits on top of a `Tconn` and provides a "set view" or "set-like access"
  # to it, remotely similar to how a set can be implemented on top of a hash map.
  #
  # The "elements" of the conn set are surfaces -- sensors and appearances. They are
  # no longer assigned an arbitrary numeric identity like in `Tconn`; but rather,
  # their identity is assigned based on their *content*. In case of sensors that'd
  # be pattern and selector; in case of appearances, their value and selector.
  #
  # Since this entails the generation of new Tconn identities on every change
  # of e.g. an appearance's value, what set conns do is they maintain a pool of
  # free ids.
  #
  # When an appearance's value changes (let's say increments), its previous version
  # (with the old value) gets removed from the set conn; and its identity is released
  # to the id pool. We then add to the set conn the appearance with the new
  # (incremented) value. This ends up taking the id from the id pool and reusing it,
  # which is beneficial primarily for sensors perceiving the appearance -- they
  # don't have to go through additional rounds of "un-seeing" the previous
  # appearance before seeing the new one (that will be completely novel to them!)
  #
  # NOTE: TsetConn is protected by a lock, and is therefore thread-safe. However,
  # this could cause problems because any change TsetConn makes will be immediately
  # caught by the underlying `Tconn`, with the potential of `Tconn::Sink` being called
  # while TsetConn's lock is held. You should therefore avoid doing anything that could
  # possibly refer back to the same TsetConn that caused the change. For this reason,
  # the lock that TsetConn is protected by is explicitly *non-reentrant*; in other words,
  # it will raise if locked recursively, so at least you'll have an exception instead of
  # some weird deadlock.
  class TsetConn
    # Default capacity of TsetConn's id pool.
    #
    # See also: `TsetConn`.
    DEFAULT_IDPOOL_CAPACITY = 16

    # Represents the identity of a sensor in a set conn. Essentially this data
    # is what sensors are compared by. Two equal `Sensor`s will map to the same
    # underlying `Tconn` sensor surface identity.
    record Sensor, pattern : Term, selector : Term? do
      def inspect(io)
        io << "sensor(pattern=" << pattern
        io << ", selector=" << selector if selector
        io << ")"
      end

      def to_s(io)
        inspect(io)
      end
    end

    # Represents the identity of an appearance in a set conn. Essentially this
    # data is what appearances are compared by. Two equal `Appearance`s will map
    # to the same underlying `Tconn` appearance surface identity.
    record Appearance, value : Term, selector : Term? do
      def inspect(io)
        io << "appearance(pattern=" << value
        io << ", selector=" << selector if selector
        io << ")"
      end

      def to_s(io)
        inspect(io)
      end
    end

    # Constructs this set conn on top of an existing `Tconn` *conn*.
    #
    # You can optionally specify this set conn's *id pool capacity*. Free identities
    # will be "burned" if past this capacity.
    def initialize(@conn : Tconn, *, @idpool_capacity = DEFAULT_IDPOOL_CAPACITY)
      @open = true
      @fresh = Identity.new(0)
      @idpool = Set(Identity).new
      @surfaces = Bimap(Sensor | Appearance, Identity).new
      @lock = Mutex.new
    end

    # Makes sure this set conn is open and acquires its lock for the duration
    # of the block.
    private def open(& : -> T) : T forall T
      @lock.synchronize do
        unless @open
          raise Tconn::ClosedError.new
        end

        yield
      end
    end

    # Clears this TconnSet and closes the underlying `Tconn`.
    #
    # Raises `Tconn::ClosedError` if this set conn is closed.
    def close : Nil
      open do
        @open = false
        @idpool.clear
        @surfaces.clear
        @conn.close
      end
    end

    # WARNING: assumes the set conn lock is acquired!
    private def acquire_id : Identity
      if identity = @idpool.first?
        @idpool.delete(identity)
        return identity
      end

      identity = @fresh
      @fresh += 1

      identity
    end

    # WARNING: assumes the set conn lock is acquired!
    private def release_id(identity : Identity) : Nil
      if 0 < @idpool_capacity <= @idpool.size
        (@idpool_capacity..@idpool.size).each do
          @idpool.delete(@idpool.first)
        end
      end

      @idpool.add(identity)
    end

    # Returns the sensor id corresponding to the underlying Tconn *identity*.
    #
    # Raises `Tconn::ClosedError` if this set conn is closed.
    def sensor?(identity : Identity) : Sensor?
      open { @surfaces[identity]?.as?(Sensor) }
    end

    # Returns the appearance id corresponding to the underlying Tconn *identity*.
    #
    # Raises `Tconn::ClosedError` if this set conn is closed.
    def appearance?(identity : Identity) : Appearance?
      open { @surfaces[identity]?.as?(Appearance) }
    end

    # Adds a sensor with the given *pattern* and *selector* to this set conn.
    # Noop if a sensor with the same identity is already in the set conn. Returns
    # the identity of the underlying Tconn sensor in either case.
    #
    # Raises `Tconn::ClosedError` if this set conn is closed.
    def add_sensor_with(pattern : Term, *, selector : Term? = nil) : Identity
      open do
        surface = Sensor.new(pattern, selector)

        if identity = @surfaces[surface]?
          return identity
        end

        identity = acquire_id

        @conn[identity] = Tconn::Sensor.new(pattern, selector: selector)
        @surfaces[surface] = identity

        identity
      end
    end

    # Adds an appearance with the given *value* and *selector* to this set conn.
    # Noop if an appearance with the same identity is already in the set conn. Returns
    # the identity of the underlying Tconn appearance in either case.
    #
    # Raises `Tconn::ClosedError` if this set conn is closed.
    def add_appearance_with(value : Term, *, selector : Term? = nil) : Identity
      open do
        surface = Appearance.new(value, selector)

        if identity = @surfaces[surface]?
          return identity
        end

        identity = acquire_id

        @conn[identity] = Tconn::Appearance.new(value, selector: selector)
        @surfaces[surface] = identity

        identity
      end
    end

    # Removes a sensor with the given *pattern* and *selector* from this set conn.
    # Noop if such a sensor is not in this set conn. Returns the identity of the
    # underlying Tconn sensor if removal was successful. Returns `nil` otherwise.
    #
    # Raises `Tconn::ClosedError` if this set conn is closed.
    def delete_sensor_with?(pattern : Term, *, selector : Term? = nil) : Identity?
      open do
        surface = Sensor.new(pattern, selector)
        return unless identity = @surfaces.delete(surface)

        @conn.delete(identity)

        release_id(identity)

        identity
      end
    end

    # Removes an appearance with the given *value* and *selector* from this set conn.
    # Noop if such an appearance is not in this set conn. Returns the identity of the
    # underlying Tconn appearance if removal was successful. Returns `nil` otherwise.
    #
    # Raises `Tconn::ClosedError` if this set conn is closed.
    def delete_appearance_with?(value : Term, *, selector : Term? = nil) : Identity?
      open do
        surface = Appearance.new(value, selector)
        return unless identity = @surfaces.delete(surface)

        @conn.delete(identity)

        release_id(identity)

        identity
      end
    end

    def pretty_print(pp)
      @lock.synchronize do
        unless @open
          pp.text("TconnSet{<closed>}")
          return
        end

        pp.list("TconnSet{", @surfaces, "}") do |surface, _|
          surface.pretty_print(pp)
        end
      end
    end
  end
end
