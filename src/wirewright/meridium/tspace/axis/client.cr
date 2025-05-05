module Ww::Meridium::Axis
  # :nodoc:
  struct Response
    def initialize(@r : Proto::Reader, @opcode : Proto::Token, @done : Channel(Nil))
    end

    def handle(& : Proto::Reader, Proto::Token -> T) : T forall T
      yield @r, @opcode
    ensure
      @done.send(nil)
      @done.close
    end
  end

  # An object that handles bookings; and thus gives each `IConn` an opportunity
  # to communicate with an Axis server in isolation. `Client` also keeps connection
  # state; an `IConn` or the client itself can discover that the connection is
  # offline now. If there is a protocol violation, the connection is suspended.
  class Client
    Log = ::Log.for(self)

    enum State : UInt8
      # The connection is online.
      Online

      # The connection is offline due to the other party's closure. Reconnect
      # attempts will be made, if possible.
      Offline

      # The connection is offline due to the client's closure.
      Closed

      # The connection is suspended due to the other party's protocol violation.
      Suspended
    end

    @@id = Atomic(UInt32).new(0)

    # Returns the process-unique id of this client.
    getter id : UInt32

    # :nodoc:
    def initialize(@bookings : Channel(Tspace::Meetable), @w : Proto::Writer)
      @id = @@id.add(1, :relaxed)

      @subs = {} of WWID => IConn
      @responses = Channel(Response).new

      @acts = Deque(Activation).new
      @state = State::Online

      # This lock protects mainly @acts and @state. It is also a write lock for @w.
      # @w has its own lock but it is never synchronized with here in Client;
      # since we are already synchronized with @lock.
      @lock = Mutex.new
    end

    # Constructs a new client and spawns the associated fibers on top of *io*.
    # Blocks until the client mainloop ends. Returns the resulting client state.
    # Guarantees to close *io* when the mainloop ends, regardless of the cause.
    #
    # Meetable objects can then book a meeting with the termspace using the *bookings*
    # channel. During a meeting, the object will be given an opportunity to talk
    # to the termspace.
    def self.run(io : IO, bookings : Channel(Tspace::Meetable)) : State
      w = Proto::Writer.new(io)
      r = Proto::Reader.new(io)

      instance = new(bookings, w)

      spawn { instance.recvloop(r) }

      instance.mainloop
    rescue e : Exception
      Log.error(exception: e) { "#{self}: client mainloop crashed" }

      State::Suspended
    ensure
      # This causes the recvloop to stop.
      io.close
    end

    # WARNING: assumes `@lock` is taken!
    private def halt(e : ProtocolError) : State
      Log.debug(exception: e) { "#{self}: HALT: protocol error, switch to suspended state" }

      @state = State::Suspended
    end

    # WARNING: assumes `@lock` is taken!
    private def halt(e : NetworkError) : State
      Log.debug(exception: e) { "#{self}: HALT: network error, switch to offline state" }

      # Offline is a "dangerous" state because we will attempt to reconnect;
      # thus if the current state is anything but online, we better have that
      # instead of Offline. Who knows what could have happened; we definitely
      # don't want to auto-reconnect on top of that.
      if @state.online?
        @state = State::Offline
      end

      @state
    end

    # "Quiet! I want to talk to the server."
    #
    # Only one caller can talk to the server at a time.
    private def tell(&) : Nil
      @lock.synchronize do
        raise NetworkError.new unless @state.online?

        yield
      rescue e : ProtocolError
        halt(e)
      rescue e : NetworkError
        halt(e)
      end
    end

    # `tell`s all of *objects* **without flushing**.
    private def say(*objects) : Nil
      tell { @w << objects }
    end

    # Applies default handling logic for *opcode*.
    #
    # May raise `NetworkError` or `ProtocolError`.
    def handle(r : Proto::Reader, opcode : AxT) : Nil
      Log.trace { "#{self}: receive #{opcode}" }

      case opcode
      when .stp?
        sensor, appearance, stimulus = r.read(WWID), r.read(IWWID), r.read(Term)
        r.expect(:ovr)

        @acts << StimulusPresence.new(sensor, appearance, stimulus)
      when .sta?
        sensor, appearance = r.read(WWID), r.read(IWWID)
        r.expect(:ovr)

        @acts << StimulusAbsence.new(sensor, appearance)
      when .srq?
        sensor, appearance = r.read(IWWID), r.read(WWID)
        r.expect(:ovr)

        @acts << StimulusRequest.new(sensor, appearance)
      when .srs?
        sensor, appearance, stimulus = r.read(IWWID), r.read(IWWID), r.read(Term)
        r.expect(:ovr)

        @acts << StimulusResponse.new(sensor, appearance, stimulus)
      else
        r.err "unexpected token #{opcode}"
      end
    end

    # Waits for *keyword* while handling all other opcodes using the default
    # handling logic. Yields the response reader after consuming *keyword*.
    #
    # May raise `NetworkError` or `ProtocolError`.
    def handle(keyword : AxT, *, timeout = READ_TIMEOUT, & : Proto::Reader ->) : Nil
      loop do
        select
        when response = @responses.receive?
          raise NetworkError.new unless response

          response.handle do |r, opcode|
            Log.trace { "#{self}: received #{opcode} while waiting for #{keyword}" }

            if opcode == keyword
              yield r
            else
              handle(r, opcode)
            end
          end
        when timeout(timeout)
          raise NetworkError.new("timed out")
        end
      end
    end

    # :nodoc:
    def mainloop : State
      Log.debug { "#{self}: client mainloop is running" }

      loop do
        while act = @acts.shift?
          next unless conn = @subs[act.receiver]?

          conn.receive(Face.new(self), act)
        end

        @w.flush

        select
        when response = @responses.receive?
          raise NetworkError.new unless response

          response.handle { |r, opcode| handle(r, opcode) }
        when meetable = @bookings.receive?
          unless meetable
            @state = State::Closed
            break
          end

          meetable.meet(Face.new(self))
        end

        @w.flush

        unless @lock.synchronize { @state }.online?
          raise NetworkError.new
        end
      end

      @lock.synchronize { @state }
    rescue e : ProtocolError
      @lock.synchronize { halt(e) }
    rescue e : NetworkError
      @lock.synchronize { halt(e) }
    end

    # :nodoc:
    def recvloop(r : Proto::Reader) : Nil
      Log.debug { "#{self}: client recvloop is running" }

      loop do
        opcode = r.read(AxT)
        Log.trace { "#{self}: recvloop received #{opcode}" }
        completed = Channel(Nil).new
        @responses.send(Response.new(r, opcode, completed))
        completed.receive
      end
    rescue e : ProtocolError
      @lock.synchronize { halt(e) }
    rescue e : NetworkError
      @lock.synchronize { halt(e) }
    ensure
      @responses.close
    end

    # :nodoc:
    def present?(conn : IConn, atoms : AtomSource) : BitList
      bitsize = nil

      tell do
        bitsize = 0

        # Send query to the server.
        @w.flush do |txn|
          txn << AxT::ASK
          atoms.each do |atom|
            txn << {AxT::HAS, atom}
            bitsize += 1
          end
          txn << AxT::OVR
        end

        handle(:ans) do |r|
          bitlist = r.read(BitList)
          r.expect(:ovr)

          unless bitsize == bitlist.size
            r.err "bitsize mismatch between ASK and ANS"
          end

          return bitlist
        end
      end

      unless bitsize
        bitsize = 0
        atoms.each { bitsize += 1 }
      end

      Log.debug { "#{self}: network or protocol error, return #{bitsize} zero(s)" }

      BitList.zeros(bitsize)
    end

    # :nodoc:
    def subscribe(conn : IConn) : Nil
      return unless @subs.put?(conn.conid, conn)

      say AxT::SUB, conn.conid, AxT::OVR
    end

    # :nodoc:
    def unsubscribe(conn : IConn) : Nil
      return unless @subs.delete(conn.conid)

      say AxT::UNS, conn.conid, AxT::OVR
    end

    # :nodoc:
    def send(conn : IConn, act : StimulusPresence) : Nil
      say AxT::STP, act.sensor, act.appearance, act.stimulus, AxT::OVR
    end

    # :nodoc:
    def send(conn : IConn, act : StimulusAbsence) : Nil
      say AxT::STA, act.sensor, act.appearance, AxT::OVR
    end

    # :nodoc:
    def send(conn : IConn, act : StimulusRequest) : Nil
      say AxT::SRQ, act.sensor, act.appearance, AxT::OVR
    end

    # :nodoc:
    def send(conn : IConn, act : StimulusResponse) : Nil
      say AxT::SRS, act.sensor, act.appearance, act.stimulus, AxT::OVR
    end

    # :nodoc:
    def transaction(conn : IConn, & : AtomSubmit, AtomSubmit ->) : Nil
      return unless @lock.synchronize { @state.online? }

      open = false
      openlock = Mutex.new

      # WARNING: these procs are called from multiple fibers.
      add = AtomSubmit.new do |atom|
        openlock.synchronize do
          next if open

          say AxT::TXN, conn.conid
          open = true
        end

        say AxT::ADD, atom
      end

      del = AtomSubmit.new do |atom|
        openlock.synchronize do
          next if open

          say AxT::TXN, conn.conid
          open = true
        end

        say AxT::DEL, atom
      end

      yield add, del

      if open
        say AxT::OVR
      end
    end

    # :nodoc:
    struct Face
      include Tspace

      def initialize(@client : Client)
      end

      def present?(conn : IConn, atoms : AtomSource) : BitList
        @client.present?(conn, atoms)
      end

      def send(conn : IConn, act : Activation) : Nil
        @client.send(conn, act)
      end

      def transaction(conn : IConn, & : AtomSubmit, AtomSubmit ->) : Nil
        @client.transaction(conn) { |add, del| yield add, del }
      end

      def subscribe(conn : IConn) : Nil
        @client.subscribe(conn)
      end

      def unsubscribe(conn : IConn) : Nil
        @client.unsubscribe(conn)
      end
    end

    def to_s(io)
      io << "client#" << @id
    end
  end
end
