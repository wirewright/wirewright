module Ww
  # Wraps `Utrie` and provides reference counting for unit strands and the
  # corresponding Uvertices.
  class Upolytrie
    # :nodoc:
    EMPTY = new(fresh: Uvertex::ZERO, trie: Utrie[], refcount: RefTracker(Uvertex)[])

    protected def initialize(@fresh : Uvertex, @trie : Utrie, @refcount : RefTracker(Uvertex))
    end

    # Constructs an empty unit polytrie.
    def self.[] : Upolytrie
      EMPTY
    end

    private def_change

    # Returns the underlying `Utrie`, useful for making queries.
    def utrie : Utrie
      @trie
    end

    # Returns `true` if *conn* has acquired one or more references to *uv*.
    # Returns `false` otherwise.
    def acquired?(conn : Tconn::Id, uv : Uvertex) : Bool
      @refcount.nonzero?(uv, conn)
    end

    # Returns the Uvertex corresponding to *strand*. Returns `Uvertex::NONE` if
    # *strand* is absent from this unit polytrie.
    def vertexof(strand : Array(Ubase)) : Uvertex
      @trie.vertexof(strand)
    end

    # If you happen to know the Uvertex *uv* corresponding to *strand*, you can call
    # this method for slightly better performance compared to `acquire(conn, strand)`.
    #
    # See also: `acquire(conn, strand)`.
    #
    # WARNING: The behavior of this method is not specified in case *strand* does not
    # correspond to *uv*.
    def acquire(conn : Tconn::Id, strand : Array(Ubase), uv : Uvertex) : Upolytrie
      if uv == Uvertex::NONE
        return change(
          fresh: @fresh + 1,
          trie: @trie.at(strand, &.bind(@fresh)),
          refcount: @refcount.inc(@fresh, conn),
        )
      end

      change(refcount: @refcount.inc(uv, conn))
    end

    # Increments the reference count of *strand* and its corresponding Uvertex
    # for *conn*. Returns the modified copy of this unit polytrie.
    def acquire(conn : Tconn::Id, strand : Array(Ubase)) : Upolytrie
      acquire(conn, strand, uv: vertexof(strand))
    end

    # If you happen to know the Uvertex *uv* corresponding to *strand*, you can call
    # this method for slightly better performance compared to `release(conn, strand)`.
    #
    # See also: `release(conn, strand)`.
    #
    # WARNING: The behavior of this method is not specified in case *strand* does not
    # correspond to *uv*.
    def release(conn : Tconn::Id, strand : Array(Ubase), vertex : Uvertex) : Upolytrie
      return self if vertex == Xvertex::NONE
      return self unless decresp = @refcount.dec?(vertex, conn)

      refcount, deleted = decresp

      if deleted
        return change(trie: @trie.at(strand, &.unbind), refcount: refcount)
      end

      change(refcount: refcount)
    end

    # Decrements the reference count of *strand* and its corresponding Uvertex
    # for *conn*.
    def release(conn : Tconn::Id, strand : Array(Ubase)) : Upolytrie
      release(conn, strand, uv: vertexof(strand))
    end
  end
end
