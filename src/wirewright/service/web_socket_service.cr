# The WebSocket service centralizes web socket handling in Wirewright.
module Ww::WebSocketService
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

  # :nodoc:
  alias Subscription = ->

  @@lock = Sync::Mutex.new
  @@servers = {} of String => Server
  @@statuses = Pf::Map(String, Status).new
  @@subscriptions = Set(Subscription).new

  # Adds a subscription to changes to statuses. When any status changes,
  # *callable* will be called, so it must respond to `#call`. Returns
  # the subscription so that you can unsubscribe.
  #
  # WARNING: *callable*s must not do heavy work. The expected use-case is to
  # pass `BlockingSignal` or a wrapper around it (such as e.g. `Automaton::Epoch`).
  def subscribe(callable) : Subscription
    subscription = -> { callable.call }

    @@lock.synchronize do
      @@subscriptions << subscription
    end

    subscription
  end

  # Removes a *subscription* created using `subscribe`.
  def unsubscribe(subscription : Subscription) : Nil
    @@lock.synchronize do
      @@subscriptions.delete(subscription)
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
            ws.send(item)
          in OutboundInterrupt
            break
          end
        end
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
        status
      in Up
        @@statuses = @@statuses.assoc(binding, Up.new(Journal.empty, status.clients))
        status
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
