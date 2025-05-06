module Ww::Meridium
  # An implementation of an in-memory termspace.
  #
  # NOTE: All methods of this implementation of `Tspace` are guaranteed to
  # be thread-safe; this is because there is no booking process to speak of.
  # We pretend to book stuff but the meetings are commenced immediately and
  # within the fiber that called for one. The principle that only one fiber
  # can work with the termspace still holds, however.
  #
  # ```
  # tspace = Tspace::InMemory.new
  #
  # conn = Conn.new(WWID.new, tspace) do |view|
  #   # Do something non-blocking and thread-safe with view...
  # end
  #
  # # Initialize
  # conn.online
  # conn.summon
  #
  # # Do stuff
  # conn.transaction do |txn|
  #   txn.put(0, Appearance.new(Term.of("Hello World")))
  #   txn.put(1, Sensor.new(Term.of(:x_string)))
  # end
  #
  # # Cleanup
  # conn.dismiss
  # conn.offline
  # ```
  class Tspace::InMemory
    include Tspace::IBookMeeting

    @atoms = {} of Atom => UInt32
    @routes = {} of WWID => IConn
    @lock = Mutex.new

    def book(meetable : Tspace::Meetable) : Nil
      @lock.synchronize do
        meetable.meet(Face.new(@atoms, @routes))
      end
    end

    # :nodoc:
    struct Face
      include Tspace

      def initialize(@atoms : Hash(Atom, UInt32), @routes : Hash(WWID, IConn))
      end

      # :nodoc:
      def present?(atom : Atom) : Bool
        @atoms.has_key?(atom)
      end

      def present?(conn : IConn, atoms : AtomSource) : BitList
        answer = BitList.new
        atoms.each { |atom| answer << present?(atom) }
        answer
      end

      # :nodoc:
      def add(atom : Atom) : Nil
        @atoms[atom] = (@atoms[atom]? || 0u32) + 1
      end

      # :nodoc:
      def delete(atom : Atom) : Nil
        return unless tally = @atoms[atom]?

        if tally == 1
          @atoms.delete(atom)
        else
          @atoms[atom] = tally - 1
        end
      end

      def transaction(conn : IConn, & : AtomSubmit, AtomSubmit ->) : Nil
        yield ->add(Atom), ->delete(Atom)
      end

      def subscribe(conn : IConn) : Nil
        @routes[conn.conid] = conn
      end

      def unsubscribe(conn : IConn) : Nil
        @routes.delete(conn.conid)
      end

      def send(conn : IConn, act : Activation) : Nil
        if receiver = @routes[act.receiver]?
          receiver.receive(self, act)
        end
      end
    end
  end
end
