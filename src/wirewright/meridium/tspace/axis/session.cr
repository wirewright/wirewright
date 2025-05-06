module Ww::Meridium::Axis
  # After a client connects to an Axis server, a *session* object is created
  # which handles requests from that client, and accesses shared state on that
  # client's behalf.
  class Session
    Log = ::Log.for(self)

    @@id = Atomic(UInt32).new(0u32)

    # Returns the process-unique id of this client.
    getter id : UInt32

    def initialize(@server : Server, @io : IO)
      @id = @@id.add(1, :relaxed)

      @r = Proto::Reader.new(@io)
      @w = Proto::Writer.new(@io)

      @conids = Set(WWID).new
    end

    # Gives write access to this session's IO to the block.
    def write(& : Proto::Writer -> T) : T forall T
      yield @w
    end

    # :nodoc:
    struct AskEE
      include Enumerable(Atom)

      def initialize(@r : Axis::Proto::Reader)
      end

      def each(& : Atom ->)
        loop do
          case @r.read(AxT)
          when .has?
            yield @r.read(Atom)
          when .ovr?
            break
          else
            @r.err "unexpected trailing data"
          end
        end
      end
    end

    private def handle(opcode : AxT) : Nil
      Log.trace { "#{self}: handle opcode #{opcode}" }

      case opcode
      when .sub?
        conid = @r.conid
        @r.expect(:ovr)

        unless @conids.add?(conid)
          Log.debug { "#{self}: attempt to subscribe twice for #{conid}" }
          return
        end

        @server.routes[conid] = self
      when .uns?
        conid = @r.conid
        @r.expect(:ovr)

        unless @conids.delete(conid)
          Log.debug { "#{self}: attempt to unsubscribe twice for #{conid}" }
          return
        end

        @server.routes.delete(conid)
      when .ask?
        bits = @server.atoms.present?(AskEE.new(@r))

        @w.send(AxT::ANS, bits, AxT::OVR)
      when .txn?
        conid = @r.conid

        adds = 0
        dels = 0

        loop do
          case @r.read(AxT)
          when .add?
            @server.atoms.incref(conid, @r.read(Atom))
            adds += 1
          when .del?
            @server.atoms.decref(conid, @r.read(Atom))
            dels += 1
          when .ovr?
            break
          else
            @r.err "unexpected trailing data"
          end
        end

        Log.trace { "#{self}: #{conid}: (+) #{adds} (-) #{dels} atom ref(s)" }
      when .stp?
        sensor, appearance, payload = @r.read(WWID), @r.read(IWWID), @r.read(Bytes)
        @r.expect(:ovr)

        Log.trace { "#{self}: relay STP to #{sensor.conid}" }

        @server.routes.find(sensor.conid) do |session|
          session.write &.send(AxT::STP, sensor, appearance, payload, AxT::OVR)
        end
      when .sta?
        sensor, appearance = @r.read(WWID), @r.read(IWWID)
        @r.expect(:ovr)

        Log.trace { "#{self}: relay STA to #{sensor.conid}" }

        @server.routes.find(sensor.conid) do |session|
          session.write &.send(AxT::STA, sensor, appearance, AxT::OVR)
        end
      when .srq?
        sensor, appearance = @r.read(IWWID), @r.read(WWID)
        @r.expect(:ovr)

        Log.trace { "#{self}: relay SRQ to #{appearance.conid}" }

        @server.routes.find(appearance.conid) do |session|
          session.write &.send(AxT::SRQ, sensor, appearance, AxT::OVR)
        end
      when .srs?
        sensor, appearance, payload = @r.read(IWWID), @r.read(IWWID), @r.read(Bytes)
        @r.expect(:ovr)

        Log.trace { "#{self}: relay SRS to #{sensor.conid}" }

        @server.routes.find(sensor.conid) do |session|
          session.write &.send(AxT::SRS, sensor, appearance, payload, AxT::OVR)
        end
      when .stps?
        sensor, appearance, alg, iv, ciphertext = @r.read(WWID), @r.read(IWWID), @r.read(Secure::Alg), @r.read(Bytes), @r.read(Bytes)
        @r.expect(:ovr)

        Log.trace { "#{self}: relay STPS to #{sensor.conid}" }

        @server.routes.find(sensor.conid) do |session|
          session.write &.send(AxT::STPS, sensor, appearance, alg, iv, ciphertext, AxT::OVR)
        end
      when .srss?
        sensor, appearance, alg, iv, ciphertext = @r.read(IWWID), @r.read(IWWID), @r.read(Secure::Alg), @r.read(Bytes), @r.read(Bytes)
        @r.expect(:ovr)

        Log.trace { "#{self}: relay SRSS to #{sensor.conid}" }

        @server.routes.find(sensor.conid) do |session|
          session.write &.send(AxT::SRSS, sensor, appearance, alg, iv, ciphertext, AxT::OVR)
        end
      when .err?
        @r.err "protocol error", quiet: true
      else
        @r.err "unexpected opcode"
      end
    end

    # Closes this session's IO, causing the session to end.
    def shutdown : Nil
      Log.trace { "#{self}: shutdown" }

      @io.close
    end

    # Starts the session listen loop. Blocks until the listen loop terminates.
    # The listen loop can be terminated by closing this session's IO (e.g. by
    # calling `shutdown`). This method guarantees to return with this session's
    # IO closed.
    def listen : Nil
      Log.trace { "#{self}: listen" }

      loop do
        handle(@r.read(AxT))
      end
    rescue e : ProtocolError
      # To avoid even the slightest potential of recursion / weirdness we do not reply
      # to ERRs from the client with an ERR, and instead just terminate quietly.
      unless e.quiet?
        write &.send(AxT::ERR)
      end
    ensure
      Log.trace { "#{self}: cleanup after listen" }

      @io.close
      @conids.each do |conid|
        @server.routes.delete(conid)
        swept = @server.atoms.sweep(conid)
        Log.trace { "#{self}: #{conid}: clear (-) #{swept} atom(s), 1 sub(s)" }
      end
    end

    def to_s(io)
      io << "session#" << @id
    end
  end
end
