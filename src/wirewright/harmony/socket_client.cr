class Ww::Harmony
  alias SocketClientDefn = TcpClientDefn | WsClientDefn | UnixClientDefn

  defrecord TcpClientDefn,
    host : String,
    port : UInt16,
    key : Term,
    link : Link,
    renew : Bool,
    brief: true

  defrecord WsClientDefn,
    host : String,
    port : UInt16,
    path : String,
    key : Term,
    security : TlsClientConfig?,
    link : Link,
    renew : Bool,
    brief: true

  defrecord UnixClientDefn,
    path : NormalPath,
    key : Term,
    link : Link,
    renew : Bool,
    brief: true

  {% if flag?(:docs) %}
    # Constructs a socket client according to the given *defn*, and manages it until
    # the client closes or crashes. Reports observations pertaining to the client to
    # *observations* (e.g., `SocketClientStarted`, `ClientStopped`, `SocketClientReceived`).
    #
    # You can communicate with a client through its `SocketQueue`, which can be obtained
    # from the `SocketClientStarted` observation issued for *defn*.
    def self.client(observations : ObservationQueue, defn : SocketClientDefn) : Nil
    end
  {% end %}

  # :nodoc:
  def self.client(observations : ObservationQueue, defn : TcpClientDefn) : Nil
    begin
      socket = TCPSocket.new(defn.host, defn.port.to_i)
      socket.tcp_nodelay = true # Disable the Nagle's algorithm
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    client(observations, defn, socket)
  end

  # :nodoc:
  def self.client(observations : ObservationQueue, defn : UnixClientDefn) : Nil
    begin
      socket = UNIXSocket.new(defn.path.unwrap)
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    client(observations, defn, socket)
  end

  # :nodoc:
  def self.client(observations : ObservationQueue, defn : WsClientDefn) : Nil
    begin
      if tls_config = defn.security
        tls_context = OpenSSL::SSL::Context::Client.new
        case tls_config.verify
        in true
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::PEER
        in false
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::NONE
        end

        socket = HTTP::WebSocket.new(defn.host, defn.path, defn.port.to_i, tls: tls_context)
      else
        socket = HTTP::WebSocket.new(defn.host, defn.path, defn.port.to_i, tls: nil)
      end

      # Disable the Nagle's algorithm on the underlying TCP socket.
      socket.nagle = false
    rescue e : IO::Error | OpenSSL::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    client(observations, defn, socket)
  end

  private def self.client(observations : ObservationQueue, defn : ClientDefn, socket : HTTP::WebSocket | Socket) : Nil
    id = ClientId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << SocketClientStarted.new(defn, id, queue)

    msgloop = ClientLoop.new(observations, defn, id, defn.link, queue, socket)
    msgloop.run
  end

  private def self.rxloop(queue : SocketQueue, socket : Socket) : Nil
    queue << SocketRxStarted.new

    while payload = NetString.decode?(socket, Term::Blob, timeout: 3.seconds)
      queue << SocketRxReceived.new(payload)
    end

    queue << SocketRxOver.new(detail: "connection closed by the other side")
  rescue e : IO::Error | OpenSSL::Error | NetString::DecodeError
    queue << SocketRxCrashed.new(cause: e)
  end

  private def self.rxloop(queue : SocketQueue, socket : HTTP::WebSocket) : Nil
    queue << SocketRxStarted.new

    socket.on_message do |string|
      queue << SocketRxReceived.new(Term::Blob.new(string))
    end

    socket.on_binary do |payload|
      queue << SocketRxReceived.new(Term::Blob.new(payload))
    end

    socket.on_close do |code, detail|
      queue << SocketRxOver.new(detail.present? ? detail : "connection closed by the other side")
    end

    socket.run
  rescue e : IO::Error | OpenSSL::Error
    queue << SocketRxCrashed.new(cause: e)
  end

  # Client message loop. It is more or less the same as the peer message loop, except
  # some types and observations differ (e.g. `SocketClientReceived` instead of `PeerReceived`,
  # `ClientId` instead of `PeerId`).
  class ClientLoop
    include SocketLoop

    def initialize(
      @observations : ObservationQueue,
      @defn : ClientDefn,
      @id : ClientId,
      @link : Link,
      @queue : SocketQueue,
      @socket : HTTP::WebSocket | Socket,
    )
    end

    def on_receive(msgid : MsgId, payload : Term::Blob) : Nil
      @observations << SocketClientReceived.new(@id, msgid, payload)
    end

    def on_crash(exception : Exception) : Nil
      @observations << ClientStopped.new(@defn, @id, exception.message || "i/o error")
    end

    def on_disconnect(detail : String) : Nil
      @observations << ClientStopped.new(@defn, @id, detail)
    end

    def on_remote_ready : Nil
      @observations << RemoteReady.new(@id)
    end

    def on_remote_busy : Nil
      @observations << RemoteBusy.new(@id)
    end

    def on_sent_ready_to_remote : Nil
      @observations << SentReadyToRemote.new(@id)
    end

    def on_sent_busy_to_remote : Nil
      @observations << SentBusyToRemote.new(@id)
    end

    def on_message_delivered_to_remote(payload : Term::Blob) : Nil
      @observations << MessageDeliveredToRemote.new(@id, payload)
    end

    def on_message_declined_by_remote(payload : Term::Blob) : Nil
      @observations << MessageDeclinedByRemote.new(@id, payload)
    end

    def on_message_from_remote_accepted(msgid : MsgId) : Nil
      @observations << MessageFromRemoteAccepted.new(@id, msgid)
    end
  end
end
