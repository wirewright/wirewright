module Ww::Meridium
  alias AtomSubmit = Atom ->

  # A *termspace* is an entity that acts both as an atom set and as a kind of router;
  # In other words, a *termspace* provides both *communication* and *set-like storage*
  # capabilities for connections. Implementations can range from simple in-memory ones
  # (`Tspace::InMemory`) to ones that use the client-server model (`Tspace::Axis`);
  # hub and spokes model; and beyond to distributed (e.g. through consensus) or even
  # emergent models.
  module Tspace
    module IFrontend
      abstract def book(meetable : Tspace::Meetable) : Nil
      abstract def connect(conn : IConn) : Nil
      abstract def disconnect(conn : IConn) : Nil
    end

    # WARNING: This method is guaranteed to be thread-safe.
    abstract def present?(conn : IConn, atoms : AtomSource) : BitList

    # WARNING: This method is **not** guaranteed to be thread-safe.
    abstract def send(conn : IConn, act : Activation) : Nil

    # Yields two procs: the first one is to *add*, and the second one is to
    # *remove* atoms. Whether each atom is submitted separately or all are
    # submitted in batch is implementation-defined; this method is simply
    # a hint for that. Similarly, the order of additions/removals is
    # implementation-defined. Callers aren't expected to care about that
    # at this point.
    #
    # WARNING: This method is **not** guaranteed to be thread-safe. However,
    # both `AtomSubmit` procs are guaranteed to be thread-safe.
    abstract def transaction(conn : IConn, & : AtomSubmit, AtomSubmit ->) : Nil

    # WARNING: This method is **not** guaranteed to be thread-safe.
    abstract def subscribe(conn : IConn) : Nil

    # WARNING: This method is **not** guaranteed to be thread-safe.
    abstract def unsubscribe(conn : IConn) : Nil

    # :nodoc:
    struct Presences
      include IAtomsPresent

      def initialize(@tspace : Tspace, @conn : IConn)
      end

      def present?(atoms : AtomSource) : BitList
        @tspace.present?(@conn, atoms)
      end
    end

    # Constructs an object that implements `IAtomsPresent` for this termspace.
    # The object is simply a wrapper around `present?` for *conn*.
    def presences(conn : IConn) : IAtomsPresent
      Presences.new(self, conn)
    end
  end

  # Implementations can `meet` with a termspace (`Tspace`).
  module Tspace::Meetable
    # Returns the id of the connection that this meetable corresponds to.
    abstract def conid : WWID

    # Conducts a meeting with *tspace*. Notifies *tspace* about the various
    # changes that occurred in the meantime. A meeting must be booked first.
    # When the termspace is ready, it will call the meeting. Only one meetable
    # can hold a meeting with a termspace at a time. This method is always called
    # from the termspace fiber (if any fiber at all!)
    abstract def meet(tspace : Tspace) : Nil
  end

  module IConn
    include Tspace::Meetable

    # Returns the id of this connection.
    abstract def conid : WWID

    # This method is called by a termspace when the connection switches to online.
    # It is expected to adjust its state accordingly, and to book a meeting with
    # `Tspace` for synchronization.
    abstract def online : Nil

    # This method is called by a termspace when the connection switches to offline.
    # It is expected to adjust its state accordingly.
    abstract def offline : Nil

    # *tspace* will notify this connection occasionally about *act*ivations
    # targeted at it through this method. This method is always called from
    # the termspace fiber (if any fiber at all!) You do not need to book
    # a meeting; in fact, we would most likely deadlock if you do. The meeting
    # is already being conducted, due to *act*; simply continue with `meet`
    # if that is needed.
    abstract def receive(tspace : Tspace, act : Activation) : Nil
  end

  # FIXME: do not include IAtomsPresent here; implement as a wrapper method or smth!!!!

  # Equips each `IAtomsPresent` presence query with a number of sanity checks;
  # namely *negative* and *consistency* checks. After the successor `presence?`
  # returns, verifies the resulting bit list -- makes sure the negative checks
  # and consistency checks pass before returning to the caller. If any check fails,
  # returns an all-zero bit list to make sure the caller halts quickly.
  class CheckedAtomsPresence
    include IAtomsPresent

    def initialize(@successor : IAtomsPresent, @random : Random = Random::Secure)
    end

    private def negatom(hasher : Atom::Hasher) : Atom
      Meridium.h(hasher, Meridium.h(hasher, :negative), Atom.rand(@random))
    end

    def present?(atoms : AtomSource) : BitList
      hasher = Atom::Hasher.new

      array = [] of {Atom, Int32}
      atoms.each_with_index do |atom, index|
        array << {atom, index}
      end

      size0 = array.size

      # - Sample 3%-5% of selection for consistency check.
      # - Emit 3%-5% of selection for negative check.
      check_percent = @random.rand(0.03..0.05)
      check_count = (check_percent * array.size).ceil.to_i

      checks_consistency = array.sample(check_count, @random)
      array.concat(checks_consistency)
      check_count.times do
        array << {negatom(hasher), array.size}
      end

      answer = @successor.present?(array) { |atom, _| atom }

      unless answer.size == array.size
        Log.debug { "reject present? response: invalid size (#{array.size})" }
        return BitList.zeros(size0)
      end

      if (answer.size - check_count...answer.size).any? { |i| answer[i] }
        Log.debug { "reject present? response: negative check failed" }
        return BitList.zeros(size0)
      end

      consistent = (0...check_count).all? do |offset|
        i = answer.size - check_count*2 + offset
        _, j = checks_consistency[offset]
        answer[i] == answer[j]
      end

      unless consistent
        Log.debug { "reject present? response: positive check failed" }
        return BitList.zeros(size0)
      end

      answer.resize(size0)
      answer
    end
  end
end
