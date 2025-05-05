module Ww::Meridium
  # NOTE: All methods of this implementation of `Tspace` are guaranteed to
  # be thread-safe; this is because there is no booking process to speak of.
  # We prented to book stuff but the meetings are commenced immediately and
  # within the fiber that called for it. The principle that only one fiber
  # can work with the termspace still holds, however, although it is a bit
  # more granular than that here.
  class Tspace::InMemory
    include Tspace
    include Tspace::IBookMeeting

    @atoms = {} of Atom => UInt32
    @routes = {} of WWID => IConn
    @lock = Mutex.new

    def book(meetable : Tspace::Meetable) : Nil
      meetable.meet(self)
    end

    # :nodoc:
    def present?(atom : Atom) : Bool
      @lock.synchronize { @atoms.has_key?(atom) }
    end

    def present?(conn : IConn, atoms : AtomSource) : BitList
      answer = BitList.new
      atoms.each { |atom| answer << present?(atom) }
      answer
    end

    def add(atom : Atom) : Nil
      @lock.synchronize do
        @atoms[atom] = (@atoms[atom]? || 0u32) + 1
      end
    end

    def delete(atom : Atom) : Nil
      @lock.synchronize do
        return unless tally = @atoms[atom]?

        if tally == 1
          @atoms.delete(atom)
        else
          @atoms[atom] = tally - 1
        end
      end
    end

    def transaction(conn : IConn, & : AtomSubmit, AtomSubmit ->) : Nil
      yield ->add(Atom), ->delete(Atom)
    end

    def subscribe(conn : IConn) : Nil
      @lock.synchronize { @routes[conn.conid] = conn }
    end

    def unsubscribe(conn : IConn) : Nil
      @lock.synchronize { @routes.delete(conn.conid) }
    end

    def send(conn : IConn, act : Activation) : Nil
      if receiver = @lock.synchronize { @routes[act.receiver]? }
        receiver.receive(self, act)
      end
    end
  end
end
