class Ww::Harmony
  alias SocketServerDefn = TcpServerDefn | UnixServerDefn

  defrecord TcpServerDefn,
    host : String,
    port : UInt16,
    link : Link,
    brief: true

  defrecord UnixServerDefn,
    path : NormalPath,
    link : Link,
    brief: true

  alias SocketServerQueue = BlockingQueue(SocketServerCommand)
  alias SocketServerCommand = Close

  {% if flag?(:docs) %}
    # Constructs a server according to the given *defn*, and manages it until it
    # closes or crashes. Reports observations pertaining to the server to *observations*.
    #
    # You can communicate with a server through its `SocketServerQueue`, which can be
    # obtained from the `SocketServerStarted` observation issued for *defn*.
    def self.server(observations : ObservationQueue, defn : SocketServerDefn) : Nil
    end
  {% end %}

  # :nodoc:
  def self.server(observations : ObservationQueue, defn : TcpServerDefn) : Nil
    begin
      server = TCPServer.new(defn.host, defn.port)
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerStartFailed.new(defn, e.message || "i/o error")
      return
    end

    id = ServerId.new(UUID.random)
    queue = SocketServerQueue.new

    spawn do
      observations << SocketServerStarted.new(defn, id, queue)
      while socket = server.accept?
        socket.tcp_nodelay = true # Disable Nagle's algorithm.
        spawn peer(observations, id, defn.link, socket)
      end
      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    end

    loop do
      command = queue.shift

      case command
      in Close
        server.close rescue nil
        break
      end
    end
  end

  # :nodoc:
  def self.server(observations : ObservationQueue, defn : UnixServerDefn) : Nil
    begin
      server = UNIXServer.new(defn.path.unwrap)
    rescue e : IO::Error
      observations << ServerStartFailed.new(defn, e.message || "i/o error")
      return
    end

    id = ServerId.new(UUID.random)
    queue = SocketServerQueue.new

    spawn do
      observations << SocketServerStarted.new(defn, id, queue)
      while socket = server.accept?
        spawn peer(observations, id, defn.link, socket)
      end
      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    end

    loop do
      command = queue.shift

      case command
      in Close
        server.close(delete: true) rescue nil
        break
      end
    end
  end

  # NOTE: WebSockets are special in that they are handled by the HTTP server. Therefore
  # there is not a WebSocketServerDefn. Please see `.server(ObservationQueue, HttpServerDefn)`.
end
