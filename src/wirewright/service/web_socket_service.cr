# The WebSocket server service centralizes web socket server handling
# in Wirewright.
#
# There is also this service's counterpart, the `WebSocketClientService`.
module Ww::WebSocketServerService
  extend self

  alias Status = Up | Dn | Pending

  alias Journal = Slice(ClientEvent)

  defrecord Up, journal : Journal, clients : Pf::Set(UUID)
  defrecord Dn, detail : String
  defrecord Pending

  alias ClientEvent = ClientConnected | ClientDisconnected | ClientReceived

  defrecord ClientConnected, id : UUID
  defrecord ClientDisconnected, id : UUID
  defrecord ClientReceived, id : UUID, message : String

  defcase Server,
    lock : Sync::Mutex,
    impl : HTTP::Server,
    sockets : Hash(UUID, HTTP::WebSocket),
    queues : Hash(UUID, BlockingQueue(OutboundQueueItem))

  alias OutboundQueueItem = String | OutboundInterrupt

  defrecord OutboundInterrupt

  @@lock = Sync::Mutex.new
  @@servers = {} of String => Server
  @@statuses = Pf::Map(String, Status).new
  @@subscriptions = Set(->).new

  # Adds a subscription to changes to statuses. When any status changes,
  # *callable* will be called.
  #
  # WARNING: *callable*s must not do heavy work. The expected use-case is to
  # pass `BlockingSignal` or a wrapper around it (such as e.g. `Automaton::Epoch`).
  def subscribe(callable : ->) : Nil
    @@lock.synchronize do
      @@subscriptions << callable
    end
  end

  def unsubscribe(callable : ->) : Nil
    @@lock.synchronize do
      @@subscriptions.delete(callable)
    end
  end

  private def log(binding : String, event : ClientEvent) : Nil
    # Make a record in the server's journal.
    @@lock.synchronize do
      unless status = @@statuses[binding]?.as?(Up) # ?!
        Log.trace { "server(#{binding}): status is not Up, unable to log #{event}" }
        next
      end

      journal1 = status.journal.append(event)

      clients1 = status.clients
      case event
      in ClientConnected
        clients1 = clients1.add(event.id)
      in ClientDisconnected
        clients1 = clients1.delete(event.id)
      in ClientReceived
      end

      @@statuses = @@statuses.assoc(binding, Up.new(journal1, clients1))
      @@subscriptions.each(&.call)
      Log.trace { "server(#{binding}): logged #{event}, called #{@@subscriptions.size} subscription(s)" }
    end
  end

  private def handle(binding : String, ws : HTTP::WebSocket, ctx : HTTP::Server::Context) : Nil
    return unless server = @@lock.synchronize { @@servers[binding]? }

    client_id = UUID.random

    Log.trace { "client(#{client_id}): connected" }

    server.lock.synchronize do
      # NOTE: We call close under server.lock so this lets us prevent the race
      # when the server is closed and all clients closed at the same time as
      # another client connects.
      if server.impl.closed?
        Log.trace { "client(#{client_id}): race with server closure, closing" }
        ws.close(:going_away)
        return
      end

      queue = BlockingQueue(OutboundQueueItem).new

      server.sockets[client_id] = ws
      server.queues[client_id] = queue

      spawn(name: "message relay for #{client_id}") do
        Log.trace { "client(#{client_id}): message relay is running" }

        loop do
          item = queue.shift
          Log.trace { "client(#{client_id}): #{item}" }

          case item
          in String
            begin
              ws.send(item)
            rescue e : Socket::Error | IO::Error | OpenSSL::SSL::Error
              unless ws.closed?
                ws.close(:abnormal_closure)
              end
              break
            end
          in OutboundInterrupt
            break
          end
        end

        Log.trace { "client(#{client_id}): message relay stopped" }
      end
    end

    log(binding, ClientConnected.new(client_id))

    ws.on_ping do
      ws.pong
    end

    ws.on_message do |message|
      log(binding, ClientReceived.new(client_id, message))
    end

    ws.on_close do
      server.lock.synchronize do
        server.sockets.delete(client_id)

        if queue = server.queues.delete(client_id)
          # Terminate the associated relay fiber.
          queue.interject(OutboundInterrupt.new)
        end
      end

      log(binding, ClientDisconnected.new(client_id))
    end
  end

  private def serve(binding : String)
    server = @@lock.synchronize do
      unless server_ = @@servers[binding]? # Canceled
        Log.trace { "server(#{binding}): canceled before having a chance to listen" }
        return
      end

      assert @@statuses.has_key?(binding)
      @@statuses = @@statuses.assoc(binding, Up.new(Journal.empty, clients: Pf::Set(UUID).new))
      @@subscriptions.each(&.call)

      Log.trace { "server(#{binding}): status set to Up, called #{@@subscriptions.size} subscription(s)" }

      server_
    end

    Log.trace { "server(#{binding}): listening" }
    server.impl.bind(binding)
    server.impl.listen
    Log.trace { "server(#{binding}): stopped listening" }
  end

  # Requests that a server at *binding* is started. If a server at *binding* was
  # already requested, returns its current status. If the server was requested
  # by this call, the status will be `Pending`.
  def start(binding : String) : Status
    @@lock.synchronize do
      if status = @@statuses[binding]?
        return status
      end

      # If status does not exist, then server does not exist.
      assert !@@servers.has_key?(binding)

      status = Pending.new

      # Set up the server (but do not launch it!)
      ws_handler = HTTP::WebSocketHandler.new do |ws, ctx|
        handle(binding, ws, ctx)
      end
      impl = HTTP::Server.new([ws_handler])

      # Add the server and the status.
      @@servers[binding] = Server.new(Sync::Mutex.new, impl,
        sockets: {} of UUID => HTTP::WebSocket,
        queues: {} of UUID => BlockingQueue(OutboundQueueItem),
      )
      @@statuses = @@statuses.assoc(binding, status)
      @@subscriptions.each(&.call)

      Log.trace { "spawn WebSocket server for #{binding}" }

      # Finally, launch the server in another fiber.
      spawn(name: "WebSocket server on `#{binding}`") do
        serve(binding)
      end

      status
    end
  end

  # Requests that a server at *binding* is stopped.
  def stop(binding : String) : Nil
    @@lock.synchronize do
      return unless status = @@statuses[binding]?

      case status
      in Up, Pending
        assert server = @@servers.delete(binding)

        server.lock.synchronize do
          Log.trace { "server(#{binding}): stop requested, closing server itself" }
          server.impl.close
        end
      in Dn
        # If it is already down we have nothing to do because there's no
        # server there.
        assert !@@servers.has_key?(binding)
      end

      @@statuses = @@statuses.dissoc(binding)
      @@subscriptions.each(&.call)
      Log.trace { "server(#{binding}): removed status, called #{@@subscriptions.size} subscription(s)" }
    end
  end

  # Observes the current status of *binding*.
  #
  # NOTE: Observing an `Up` status clears the server-side journal. In other
  # words, by checking out an Up status, the caller "consumes" the journal;
  # now it is the sole holder of it.
  def checkout?(binding : String) : Status?
    @@lock.synchronize do
      case status = @@statuses[binding]?
      in Nil, Pending, Dn
        status.as(Status?)
      in Up
        @@statuses = @@statuses.assoc(binding, Up.new(Journal.empty, status.clients))
        status.as(Status?)
      end
    end
  end

  # Sends *message* to *binding*'s client with the given *client id*. If
  # the server at *binding* is not running, or *client* does not exist,
  # does nothing.
  #
  # NOTE: This function does no IO. IO is done on dedicated per-client fibers.
  def send(binding : String, client_id : UUID, message : String) : Nil
    @@lock.synchronize do
      return unless server = @@servers[binding]?
      return unless queue = server.lock.synchronize { server.queues[client_id]? }

      queue << message
    end
  end

  def drop(binding : String, client_id : UUID) : Nil
    @@lock.synchronize do
      return unless server = @@servers[binding]?
      return unless ws = server.lock.synchronize { server.sockets[client_id]? }

      # Rely on the `on_close` callback to do cleanup.
      ws.close
    end
  end
end

# The WebSocket client service centralizes web socket client handling
# in Wirewright.
#
# There is also this service's counterpart, the `WebSocketServerService`.
module Ww::WebSocketClientService
  extend self

  Log = ::Log.for(self)

  defrecord Conn,
    host : String,
    port : UInt16,
    path : String,
    index : UInt32,
    secure : Bool,
    max_retries : UInt32

  DEFAULT_MAX_RETRIES = 10u32

  def conn?(term : Term) : Conn?
    return unless uriQ = term.as_s?
    return unless uri = URI.parse(uriQ.to(String))
    return unless host = uri.host
    return unless port = uri.port
    return unless UInt16::MIN <= port <= UInt16::MAX

    case uri.scheme
    when "ws"  then secure = false
    when "wss" then secure = true
    else
      return
    end

    index = uri.query_params["index"]?.try(&.to_u32?) || 0u32
    max_retries = uri.query_params["max-retries"]?.try(&.to_u32?) || DEFAULT_MAX_RETRIES

    Conn.new(host, port.to_u16, uri.path, index, secure, max_retries)
  end

  alias Command = Send | Disconnect

  defrecord Send, message : String
  defrecord Disconnect

  alias Journal = Slice(String)

  defrecord Up
  defrecord Dn, detail : String
  defrecord Pending

  @@lock = Sync::Mutex.new
  @@journals = {} of Conn => Journal
  @@controls = {} of Conn => BlockingQueue(Command)
  @@connections = {} of Conn => Up | Dn | Pending
  @@subscriptions = Set(->).new

  # WARNING: *callable*s must not do heavy work. The expected use-case is to
  # pass `BlockingSignal` or a wrapper around it (such as e.g. `Automaton::Epoch`).
  def subscribe(callable : ->) : Nil
    @@lock.synchronize do
      @@subscriptions << callable
    end
  end

  def unsubscribe(callable : ->) : Nil
    @@lock.synchronize do
      @@subscriptions.delete(callable)
    end
  end

  def connect(conn : Conn) : Nil
    @@lock.synchronize do
      return if @@connections.has_key?(conn)

      @@connections[conn] = Pending.new
      @@subscriptions.each(&.call)

      spawn(name: "WebSocketClientService client fiber") do
        runloop(conn)
      end
    end
  end

  def disconnect(conn : Conn) : Nil
    @@lock.synchronize do
      @@journals.delete(conn)
      @@connections.delete(conn)
      if control = @@controls.delete(conn)
        control << Disconnect.new
      end
    end
  end

  private def runloop(conn : Conn) : Nil
    min_retry_delay = 500.milliseconds
    max_retry_delay = 30.seconds
    retry_budget = conn.max_retries

    rng = Random::PCG32.new

    loop do
      Log.trace { "#{conn}: connection attempt with retry_budget=#{retry_budget}" }

      begin
        ws = HTTP::WebSocket.new(conn.host, conn.path, conn.port, tls: conn.secure ? true : nil)

        Log.trace { "#{conn}: connection established" }
        retry_budget = conn.max_retries

        handle(conn, ws)
      rescue e : Socket::ConnectError | IO::Error
        if retry_budget.zero? # Expended
          Log.trace { "#{conn}: max retries exceeded" }
          @@lock.synchronize do
            @@connections[conn] = Dn.new("max retries exceeded")
            @@subscriptions.each(&.call)
          end
          return
        end

        @@lock.synchronize do
          @@connections[conn] = Pending.new
          @@subscriptions.each(&.call)
        end

        attempt = conn.max_retries - retry_budget
        exp = Math.min(min_retry_delay * 2**attempt, max_retry_delay)
        delay = exp * (0.5..1.0).sample(rng) # With jitter

        Log.trace { "#{conn}: retry attempt with delay=#{delay}" }
        sleep delay

        retry_budget -= 1
      rescue e : Socket::Error | IO::Error | OpenSSL::SSL::Error
        Log.trace(exception: e) { "#{conn}: fail without retry attempts" }
        @@lock.synchronize do
          @@connections[conn] = Dn.new("connection failure: #{e.message}")
          @@subscriptions.each(&.call)
        end
      end
    end
  end

  private def handle(conn : Conn, ws : HTTP::WebSocket) : Nil
    control = BlockingQueue(Command).new

    @@lock.synchronize do
      return unless @@connections.has_key?(conn) # Closed before we can even do anything.

      @@journals[conn] = Journal.empty
      @@controls[conn] = control
      @@connections[conn] = Up.new
      @@subscriptions.each(&.call)
    end

    spawn(name: "WebSocketClientService control") do
      Log.trace { "#{conn}: control msgloop running" }

      loop do
        command = control.shift

        Log.trace { "#{conn}: #{command}" }

        case command
        in Send
          ws.send(command.message)
        in Disconnect
          unless ws.closed?
            ws.close(:normal_closure)
          end
          break
        end
      end

      Log.trace { "#{conn}: control msgloop stopped" }
    end

    ws.on_message do |message|
      @@lock.synchronize do
        next unless journal = @@journals[conn]?

        @@journals[conn] = journal.append(message)
        @@subscriptions.each(&.call)
      end
    end

    ws.on_close do |code, _|
      Log.trace { "#{conn}: closed with code=#{code}" }

      control << Disconnect.new

      @@lock.synchronize do
        @@journals.delete(conn)
        @@controls.delete(conn)
        @@connections.delete(conn)
        @@subscriptions.each(&.call)
      end
    end

    # If the socket was closed in the meantime, that will be caught by
    # `WebSocket#run`.

    Log.trace { "#{conn}: run" }
    ws.run
  end

  def checkout?(conn : Conn) : Up | Dn | Pending | Nil
    @@lock.synchronize do
      @@connections[conn]?
    end
  end

  def dequeue(conn : Conn) : Nil
    @@lock.synchronize do
      return unless journal = @@journals[conn]?
      return unless journal.present?

      @@journals[conn] = journal + 1
    end
  end

  def head?(conn : Conn) : String?
    return unless journal = @@lock.synchronize { @@journals[conn]? }

    journal.first?
  end

  def send?(conn : Conn, message : String) : Bool
    return false unless control = @@lock.synchronize { @@controls[conn]? }

    control << Send.new(message)

    true
  end
end
