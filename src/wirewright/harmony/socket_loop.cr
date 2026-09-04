class Ww::Harmony
  alias SocketQueue = BlockingQueue(SocketCommand)

  alias SocketCommand = SocketRxStarted | SocketRxReceived | SocketRxOver |
                        SocketRxCrashed | SocketSend | SocketAccept | SocketInformReady |
                        SocketInformBusy | Close

  defrecord SocketRxStarted
  defrecord SocketRxReceived, payload : Term::Blob
  defrecord SocketRxOver, detail : String
  defrecord SocketRxCrashed, cause : Exception
  defrecord SocketSend, payload : Term::Blob
  defrecord SocketAccept, msgid : MsgId
  defrecord SocketInformReady, capacity : UInt32
  defrecord SocketInformBusy

  # Processes messages from a `SocketQueue` for socket peers (`Harmony.peer`, `PeerLoop`)
  # and clients (`Harmony.client`, `ClientLoop`).
  #
  # Supports `HTTP::WebSocket` and `Socket`.
  #
  # Requires includers to have the following instance variables:
  # - `@link : Link`
  # - `@queue : SocketQueue`
  # - `@socket : HTTP::Socket | Socket`.
  module SocketLoop
    Log = ::Log.for(self)

    @link : Link
    @queue : SocketQueue
    @socket : HTTP::WebSocket | Socket

    # Gives the block an IO to write to. Returns `true` if the message was filed
    # successfully. Returns `false` if there is a problem with the underlying
    # socket, and the message was not filed or filed partially.
    def stream?(& : IO ->) : Bool
      case socket = @socket
      in HTTP::WebSocket
        socket.stream { |io| yield io }
      in Socket
        # NOTE: We have to know how long the string is going to be, so we must use an
        # in-memory buffer first. We can use a counting IO but then the block would
        # be called twice; I'm not sure that's appropriate here. If this ever becomes
        # a problem, we can get rid of the block and use a fixed format / type hierarchy
        # for streeaming, so that the size is known in advance or at least easily derivable.
        buffer = IO::Memory.new
        yield buffer

        NetString.encode(socket, buffer.to_slice)
      end

      true # Success
    rescue e : IO::Error | OpenSSL::Error | NetString::EncodeError
      Log.debug(exception: e) { "i/o error in SocketLoop#stream?" }

      false # Failure
    end

    enum CloseCode
      NormalClosure
      AbnormalClosure
    end

    # Closes the underlying socket. Returns `true` if closure succeeded.
    #
    # NOTE: *code* and *detail* make sense only for `HTTP::WebSocket`. They are
    # discarded for other socket types.
    def close?(code : CloseCode, detail : String) : Bool
      case socket = @socket
      in HTTP::WebSocket
        case code
        in .normal_closure?   then socket.close(:normal_closure, detail)
        in .abnormal_closure? then socket.close(:abnormal_closure, detail)
        end
      in Socket
        socket.close
      end

      true # Success
    rescue e : IO::Error | OpenSSL::Error
      Log.debug(exception: e) { "i/o error in SocketLoop#close?" }

      false # Failure
    end

    abstract def on_receive(msgid : MsgId, payload : Term::Blob) : Nil
    abstract def on_crash(exception : Exception) : Nil
    abstract def on_disconnect(detail : String) : Nil

    abstract def on_message_delivered_to_remote(payload : Term::Blob) : Nil
    abstract def on_message_declined_by_remote(payload : Term::Blob) : Nil
    abstract def on_message_from_remote_accepted(msgid : MsgId) : Nil

    abstract def on_remote_ready : Nil
    abstract def on_remote_busy : Nil
    abstract def on_sent_ready_to_remote : Nil
    abstract def on_sent_busy_to_remote : Nil

    alias HandleFlow = HandleContinue | HandleBreak | HandleAbort

    defrecord HandleContinue
    defrecord HandleBreak, detail : String
    defrecord HandleAbort, detail : String, cause : Exception? = nil

    def run : Nil
      machine = SocketLoop.machine(self, @link)

      loop do
        command = @queue.shift
        Log.debug { "SOCKET LOOP:0x#{object_id.to_s(base: 16)}: #{command}" }

        case flow = machine.handle(command)
        in HandleContinue
        in HandleBreak
          on_disconnect(flow.detail)
          break
        in HandleAbort
          _ = close?(:abnormal_closure, flow.detail)
          if cause = flow.cause
            raise cause
          end

          on_disconnect(flow.detail)
          break
        end
      end
    rescue e : IO::Error | OpenSSL::Error
      on_crash(e)
    ensure
      Log.debug { "SOCKET LOOP:0x#{object_id.to_s(base: 16)}: exited" }
    end

    def self.machine(instance : SocketLoop, link : StreamLink) : StreamLinkMachine
      StreamLinkMachine.new(instance)
    end

    def self.machine(instance : SocketLoop, link : HandoffLink) : HandoffLinkMachine
      HandoffLinkMachine.new(instance)
    end
  end

  # Handles messages for `StreamLink`.
  class SocketLoop::StreamLinkMachine
    def initialize(@loop : SocketLoop)
      @seq = 0u64
    end

    # They sent us something.
    def handle(command : SocketRxReceived) : HandleFlow
      msgid = MsgId.new(@seq)
      @seq += 1
      @loop.on_receive(msgid, command.payload)

      HandleContinue.new
    end

    # We want to send something.
    def handle(command : SocketSend) : HandleFlow
      unless @loop.stream?(&.write(command.payload.to_slice))
        @loop.on_message_declined_by_remote(command.payload)
        return HandleAbort.new("message not sent")
      end

      # In nonblocking mode, seeing the message "go toward & across the wire"
      # counts as a delivery.
      @loop.on_message_delivered_to_remote(command.payload)

      HandleContinue.new
    end

    # We want to confirm we've received their message.
    def handle(command : SocketAccept) : HandleFlow
      @loop.on_message_from_remote_accepted(command.msgid)

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(command : SocketInformReady) : HandleFlow
      @loop.on_sent_ready_to_remote

      HandleContinue.new
    end

    # We cannot accept messages anymore.
    def handle(command : SocketInformBusy) : HandleFlow
      @loop.on_sent_busy_to_remote

      HandleContinue.new
    end

    # The socket started.
    def handle(command : SocketRxStarted) : HandleFlow
      # If we don't do this here nobody would. In nonblocking mode, there is no
      # way to make the other side say, "I'm ready". Instead, we manufacture and
      # send this message locally. Basically, we're saying, "pretend the other
      # side said it's ready".
      @loop.on_remote_ready

      HandleContinue.new
    end

    # The Rx fiber crashed.
    def handle(command : SocketRxCrashed) : HandleFlow
      # The socket may not be closed since the crash is not necessarily related
      # to it. Therefore we have to use Abort which closes the socket.
      HandleAbort.new("rx error", command.cause)
    end

    # The Rx fiber terminated nominally.
    def handle(command : SocketRxOver) : HandleFlow
      # We know the socket is closed with RxOver so we don't have to close
      # it ourselves.
      HandleBreak.new(command.detail)
    end

    # We want to close the connection.
    def handle(command : Close) : HandleFlow
      _ = @loop.close?(:normal_closure, "")

      # SocketRxCrashed/SocketRxOver will handle actual closure. Here we only
      # will it to happen. So we have to continue handling commands.
      HandleContinue.new
    end
  end

  # Handles messages for `HandoffLink`.
  class SocketLoop::HandoffLinkMachine
    module Protocol
      extend self

      alias Frame = Data | Accepted | Declined | Ready

      defrecord Data, payload : Term::Blob
      defrecord Accepted
      defrecord Declined
      defrecord Ready

      def receive?(payload : Term::Blob) : Frame?
        io = payload.to_io

        header = io.gets(delimiter: ' ', limit: 8)
        case header
        when "DATA "
          # TODO: We should eventually be able to pass the IO to `Blob` as-is.
          Data.new(Term::Blob.new(io.getb_to_end))
        when "ACCEPTED"
          # assert io at end
          Accepted.new
        when "DECLINED"
          # assert io at end
          Declined.new
        when "READY"
          # assert io at end
          Ready.new
        end
      end

      def send(io, frame : Data) : Nil
        io << "DATA "
        IO.copy(src: frame.payload.to_io, dst: io)
      end

      def send(io, frame : Accepted) : Nil
        io << "ACCEPTED"
      end

      def send(io, frame : Declined) : Nil
        io << "DECLINED"
      end

      def send(io, frame : Ready) : Nil
        io << "READY"
      end
    end

    @outstanding : Term::Blob?

    def initialize(@loop : SocketLoop)
      @seq = 0u64
      @capacity = 0u32
      @peer_ready = true
    end

    private def send?(frame : Protocol::Frame) : Bool
      @loop.stream? { |io| Protocol.send(io, frame) }
    end

    # The socket started.
    def handle(command : SocketRxStarted) : HandleFlow
      HandleContinue.new
    end

    # They sent us something.
    def handle(command : SocketRxReceived) : HandleFlow
      unless frame = Protocol.receive?(command.payload)
        return HandleAbort.new("invalid handoff protocol frame")
      end

      handle(frame)
    end

    def handle(frame : Protocol::Data) : HandleFlow
      if @capacity.zero?
        unless send?(Protocol::Declined.new)
          return HandleAbort.new("DECLINED not sent")
        end

        return HandleContinue.new
      end

      msgid = MsgId.new(@seq)
      @seq += 1
      @capacity -= 1
      @loop.on_receive(msgid, frame.payload)

      HandleContinue.new
    end

    def handle(frame : Protocol::Accepted) : HandleFlow
      unless payload = @outstanding
        return HandleAbort.new("unexpected ACCEPTED")
      end

      @outstanding = nil
      @loop.on_message_delivered_to_remote(payload)

      HandleContinue.new
    end

    def handle(frame : Protocol::Declined) : HandleFlow
      unless payload = @outstanding
        return HandleAbort.new("unexpected DECLINED")
      end

      @peer_ready = false
      @outstanding = nil
      @loop.on_message_declined_by_remote(payload)
      @loop.on_remote_busy

      HandleContinue.new
    end

    def handle(frame : Protocol::Ready) : HandleFlow
      @peer_ready = true
      @loop.on_remote_ready

      HandleContinue.new
    end

    # We want to send something.
    def handle(command : SocketSend) : HandleFlow
      unless @outstanding.nil? && @peer_ready
        @loop.on_message_declined_by_remote(command.payload)
        return HandleContinue.new
      end

      unless send?(Protocol::Data.new(command.payload))
        return HandleAbort.new("DATA not sent")
      end

      @outstanding = command.payload

      HandleContinue.new
    end

    # We want to confirm we've received their message.
    def handle(command : SocketAccept) : HandleFlow
      unless send?(Protocol::Accepted.new)
        return HandleAbort.new("ACCEPTED not sent")
      end

      @loop.on_message_from_remote_accepted(command.msgid)

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(command : SocketInformReady) : HandleFlow
      @capacity = command.capacity

      unless send?(Protocol::Ready.new)
        return HandleAbort.new("READY not sent")
      end

      @loop.on_sent_ready_to_remote

      HandleContinue.new
    end

    # We cannot accept messages anymore.
    def handle(command : SocketInformBusy) : HandleFlow
      @capacity = 0u32
      @loop.on_sent_busy_to_remote

      HandleContinue.new
    end

    # The Rx fiber crashed.
    def handle(command : SocketRxCrashed) : HandleFlow
      # The socket may not be closed since the crash is not necessarily related
      # to it. Therefore we have to use Abort which closes the socket.
      HandleAbort.new("rx error", command.cause)
    end

    # The Rx fiber terminated nominally.
    def handle(command : SocketRxOver) : HandleFlow
      # We know the socket is closed with RxOver so we don't have to close
      # it ourselves.
      HandleBreak.new(command.detail)
    end

    # We want to close the connection.
    def handle(command : Close) : HandleFlow
      _ = @loop.close?(:normal_closure, "")

      # SocketRxCrashed/SocketRxOver will handle actual closure. Here we only
      # will it to happen. So we have to continue handling commands.
      HandleContinue.new
    end
  end
end
