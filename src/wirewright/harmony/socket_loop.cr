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
  defrecord SocketInformReady
  defrecord SocketInformBusy

  # Processes messages from a `SocketQueue` for socket peers (`Harmony.peer`, `PeerLoop`)
  # and clients (`Harmony.client`, `ClientLoop`).
  #
  # Supports `HTTP::WebSocket` and `Socket`.
  #
  # Requires includers to have the following instance variables:
  #  # - `@queue : SocketQueue`
  # - `@socket : HTTP::Socket | Socket`.
  module SocketLoop
    Log = ::Log.for(self)

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

    abstract def on_message_accepted(payload : Term::Blob) : Nil
    abstract def on_message_lost(payload : Term::Blob) : Nil
    abstract def on_message_handled(msgid : MsgId) : Nil

    abstract def on_receive_ready : Nil
    abstract def on_receive_busy : Nil
    abstract def on_informed_ready : Nil
    abstract def on_informed_busy : Nil

    @seq = 0u64
    @pending = {} of MsgId => Term::Blob

    alias HandleFlow = HandleContinue | HandleBreak | HandleAbort

    defrecord HandleContinue
    defrecord HandleBreak
    defrecord HandleAbort, detail : String, cause : Exception? = nil

    def run : Nil
      loop do
        command = @queue.shift
        Log.debug { "msgloop:0x#{object_id.to_s(base: 16)}: #{command}" }

        case flow = handle(@link, command)
        in HandleContinue
        in HandleBreak
          break
        in HandleAbort
          _ = close?(:abnormal_closure, flow.detail)
          if cause = flow.cause
            on_crash(cause)
          else
            on_disconnect(flow.detail)
          end
          break
        end
      end
    end

    # The socket started.
    def handle(link : PortalLink, command : SocketRxStarted) : HandleFlow
      HandleContinue.new
    end

    # :ditto:
    def handle(link : DirectLink, command : SocketRxStarted) : HandleFlow
      # If we don't do this here nobody would. In nonblocking mode, there is no
      # way to make the other side say, "I'm ready". Instead, we manufacture and
      # send this message locally. Basically, we're saying, "pretend the other
      # side said it's ready".
      on_receive_ready

      HandleContinue.new
    end

    # They sent us something.
    def handle(link : PortalLink, command : SocketRxReceived) : HandleFlow
      begin
        frame = Portal.deserialize(command.payload.to_slice)
      rescue e : Portal::Error
        return HandleAbort.new(e.message || "portal protocol error", e)
      end

      case frame
      in Portal::Data
        on_receive(frame.msgid, frame.payload)
      in Portal::Accept
        unless payload = @pending.delete(frame.msgid)
          Log.debug { "dropping ACCEPT for a missing message" }
          return HandleContinue.new
        end

        on_message_accepted(payload)
      in Portal::Ready
        on_receive_ready
      in Portal::Busy
        on_receive_busy
      end

      HandleContinue.new
    end

    # :ditto:
    def handle(link : DirectLink, command : SocketRxReceived) : HandleFlow
      msgid = MsgId.new(@seq)
      @seq += 1

      on_receive(msgid, command.payload)

      HandleContinue.new
    end

    # We want to send something.
    def handle(link : PortalLink, command : SocketSend) : HandleFlow
      msgid = MsgId.new(@seq)
      @seq += 1

      frame = Portal::Data.new(msgid, command.payload)
      unless stream? { |io| Portal.serialize(io, frame) }
        # On failure, report message loss and terminate. Perhaps we'll retry some
        # other time.
        #
        # NOTE: failure to send means something is wrong with the underlying IO. There's
        # no way we'll be able to use it. So we break here instead of continuing, and in
        # general tear everything down.
        on_message_lost(command.payload)
        return HandleAbort.new("DATA not sent")
      end

      # In blocking mode, if we see the message "go toward & across the wire", that's
      # not enough; we need the other side to confirm they've received the message.
      @pending[msgid] = command.payload

      HandleContinue.new
    end

    # :nodoc:
    def handle(link : DirectLink, command : SocketSend) : HandleFlow
      unless stream?(&.write(command.payload.to_slice))
        on_message_lost(command.payload)
        return HandleAbort.new("message not sent")
      end

      # In nonblocking mode, seeing it "go toward & across the wire" counts
      # as a successful send.
      on_message_accepted(command.payload)

      HandleContinue.new
    end

    # We want to confirm the receipt of their message.
    def handle(link : PortalLink, command : SocketAccept) : HandleFlow
      frame = Portal::Accept.new(command.msgid)
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("ACCEPT not sent")
      end

      on_message_handled(command.msgid)

      HandleContinue.new
    end

    # :ditto:
    def handle(link : DirectLink, command : SocketAccept) : HandleFlow
      on_message_handled(command.msgid)

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(link : DirectLink, command : SocketInformReady) : HandleFlow
      on_informed_ready

      HandleContinue.new
    end

    # :ditto:
    def handle(link : PortalLink, command : SocketInformReady) : HandleFlow
      frame = Portal::Ready.new
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("READY not sent")
      end

      on_informed_ready

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(link : DirectLink, command : SocketInformBusy) : HandleFlow
      on_informed_busy

      HandleContinue.new
    end

    # :ditto:
    def handle(link : PortalLink, command : SocketInformBusy) : HandleFlow
      frame = Portal::Busy.new
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("BUSY not sent")
      end

      on_informed_busy

      HandleContinue.new
    end

    # They closed the connection or crashed.
    def handle(link : Link, command : SocketRxCrashed) : HandleFlow
      # The socket may not be closed since the crash is not necessarily related
      # to it. Therefore we have to use Abort which closes the socket.
      HandleAbort.new("rx error", command.cause)
    end

    # :ditto:
    def handle(link : Link, command : SocketRxOver) : HandleFlow
      on_disconnect(command.detail)

      # We know the socket is closed with RxOver so we don't have to close
      # it ourselves.
      HandleBreak.new
    end

    # We want to close the connection.
    def handle(link : Link, command : Close) : HandleFlow
      _ = close?(:normal_closure, "")

      # SocketRxCrashed/SocketRxOver will handle actual closure. Here we only
      # command it. So we have to continue handling commands.
      HandleContinue.new
    end
  end
end
