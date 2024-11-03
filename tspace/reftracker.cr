module Ww
  # Reference tracker maintains `Tconn::Id`-based reference count for objects of
  # type *T*. Assuming connections do not know the ids of each other but only know
  # their own id, this acts as a kind of "password protection" or "claiming mechanism"
  # for reference count, so that malicious connections cannot "steal" references by e.g.
  # `(- ref)`ing some frequently used object indefinitely in parallel with other,
  # well-behaved connections that are trying to use it.
  class RefTracker(T)
    # :nodoc:
    struct ConnTally
      # :nodoc:
      EMPTY = new(map: Pf::Map(Tconn::Id, UInt32).new)

      protected def initialize(@map : Pf::Map(Tconn::Id, UInt32))
      end

      private def_change

      # Constructs an empty connection tally.
      def self.[] : ConnTally
        EMPTY
      end

      # Returns `true` if this tally is empty.
      def empty? : Bool
        @map.empty?
      end

      # Returns `true` if *conn* has a non-zero number of references within this tally.
      # Returns `false` otherwise.
      def nonzero?(conn : Tconn::Id) : Bool
        @map.has_key?(conn)
      end

      # Increments the number of references for *conn*. Returns the modified copy
      # of this tally.
      def inc(conn : Tconn::Id) : ConnTally
        change(map: @map.extend(conn, 0, &.succ))
      end

      # Decrements the number of references for *conn*. Returns the modified copy
      # of this tally, or `nil` if *conn* had zero references.
      def dec?(conn : Tconn::Id) : ConnTally?
        return unless nrefs = @map[conn]?

        if nrefs == 1
          return change(map: @map.dissoc(conn))
        end

        change(map: @map.assoc(conn, nrefs - 1))
      end
    end

    protected def initialize(@refcount : Pf::Map(T, ConnTally))
    end

    # Constructs an empty reference tracker.
    def self.[] : RefTracker(T)
      new(refcount: Pf::Map(T, ConnTally).new)
    end

    private def_change

    # Returns `true` if *conn* has a non-zero number of references to *object*.
    # Returns `false` otherwise.
    def nonzero?(object : T, conn : Tconn::Id) : Bool
      return false unless tally = @refcount[object]? # zero if doesn't exist

      tally.nonzero?(conn)
    end

    # Returns `true` if *object* has a non-zero number of references. Returns
    # `false` otherwise.
    def nonzero?(object : T) : Bool
      @refcount.has_key?(object)
    end

    # Returns the number of tracked objects.
    def size : Int32
      @refcount.size
    end

    # Increments the number of references to *object* for *conn*. Returns
    # the modified copy of this reference tracker.
    def inc(object : T, conn : Tconn::Id) : RefTracker(T)
      change(refcount: @refcount.extend(object, ConnTally[], &.inc(conn)))
    end

    # Decrements the number of references to *object* for *conn*. Returns
    # the modified copy of this reference tracker and a boolean indicating
    # whether the number of references is now zero.
    #
    # Returns `nil` if *conn* does not own *object* or if *object* is not
    # tracked by this reference tracker.
    def dec?(object : T, conn : Tconn::Id) : {RefTracker(T), Bool}?
      return unless tally0 = @refcount[object]?
      return unless tally1 = tally0.dec?(conn)

      if tally1.empty?
        return change(refcount: @refcount.dissoc(object)), true
      end

      {change(refcount: @refcount.assoc(object, tally1)), false}
    end
  end
end
