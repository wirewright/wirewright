class Ww::Harmony
  defrecord HttpServerDefn,
    host : String,
    port : ServerPort,
    key : Term,
    security : TlsServerConfig?,
    brief: true

  defrecord TlsServerConfig,
    cert : NormalPath,
    key : NormalPath,
    brief: true

  alias HttpServerQueue = BlockingQueue(HttpServerCommand)
  alias HttpServerCommand = HttpRxListening | HttpRxClosed | HttpRxCrashed | HttpRxRequest |
                            HttpRespond | HttpReject | HttpAddWebSocketHandler |
                            HttpRemoveWebSocketHandler | Close

  # HTTP server (receive) fiber is listening.
  defrecord HttpRxListening

  # HTTP server (receive) fiber stopped listening.
  defrecord HttpRxClosed

  # HTTP server (receive) fiber crashed with an `Exception`.
  defrecord HttpRxCrashed, cause : Exception

  # HTTP server (receive) fiber received a request and encoded it using
  # `HttpRequestLanguage`. The command fiber is expected to handle the request
  # and set *response* eventually (or fail it).
  defrecord HttpRxRequest, request_id : HttpRequestId, request : Term, response : Sync::Future(Term)

  # A response *term* was prepared for the request with the given *id*. The command
  # fiber is expected to interpret *term* using `HttpResponseLanguage` and respond
  # to the request with *request id*.
  defrecord HttpRespond, request_id : HttpRequestId, response : Term

  # A response with the given *id* must be rejected.
  defrecord HttpReject, request_id : HttpRequestId

  # Asks the HTTP server to add a WebSocket handler with Link *link*.
  defrecord HttpAddWebSocketHandler, link : Link

  # Asks the HTTP server to remove its WebSocket handler.
  defrecord HttpRemoveWebSocketHandler

  private class HttpRequestRejectedException < Exception
    @callstack = CallStack.empty
  end

  private class HttpClosingException < Exception
    @callstack = CallStack.empty
  end

  # WARNING: This handler MUST be the last one because it doesn't call the next handler.
  private class HttpQueueDelegateHandler
    include HTTP::Handler

    def initialize(@queue : HttpServerQueue)
    end

    def call(context)
      request_id = HttpRequestId.new(UUID.random)
      request_term = HttpRequestLanguage.encode(context.request)
      response_slot = Sync::Future(Term).new

      @queue << HttpRxRequest.new(request_id, request_term, response_slot)

      begin
        response = response_slot.get
      rescue HttpRequestRejectedException
        context.response.respond_with_status(:service_unavailable, "Rejected")
      rescue HttpClosingException
        context.response.respond_with_status(:internal_server_error, "Closing")
      else
        HttpResponseLanguage.decode(response, into: context.response)
      end
    end
  end

  # Toggleable WebSocket handling.
  private class HttpWebSocketHandler < HTTP::WebSocketHandler
    property? enabled = false

    def call(context)
      unless websocket_upgrade_request?(context.request)
        return call_next(context)
      end

      unless enabled?
        context.response.status = :misdirected_request
        return
      end

      super
    end
  end

  # Constructs an HTTP server according to the given *defn*, and manages it until it
  # closes or crashes. Reports observations pertaining to the server to *observations*.
  #
  # You can communicate with a server through its `HttpServerQueue`, which can be
  # obtained from the `HttpServerStarted` observation issued for *defn*.
  def self.server(observations : ObservationQueue, defn : HttpServerDefn) : Nil
    id = ServerId.new(UUID.random)
    queue = HttpServerQueue.new

    connect_default = ->(socket : HTTP::WebSocket, context : HTTP::Server::Context) do
      socket.close(:protocol_error)
    end

    connect = connect_default

    websocket_handler = HttpWebSocketHandler.new { |socket, ctx| connect.call(socket, ctx) }

    queue_delegate_handler = HttpQueueDelegateHandler.new(queue)

    begin
      server = HTTP::NodelayServer.new([
        websocket_handler,
        HTTP::ErrorHandler.new,
        HTTP::CompressHandler.new,
        queue_delegate_handler,
      ])

      port_cfg = defn.port

      if tls_config = defn.security
        # TODO: I'm sure there's a lot more configuration to it than this.
        context = OpenSSL::SSL::Context::Server.new
        context.certificate_chain = tls_config.cert.unwrap.to_s
        context.private_key = tls_config.key.unwrap.to_s

        case port_cfg
        in ExclusiveServerPort
          address = server.bind_tls(defn.host, port_cfg.port.to_i, context)
        in SharedServerPort
          address = server.bind_tls(defn.host, port_cfg.port.to_i, context, reuse_port: true)
        in AutoServerPort
          address = server.bind_tls(defn.host, context)
        end
      else
        case port_cfg
        in ExclusiveServerPort
          address = server.bind_tcp(defn.host, port_cfg.port.to_i)
        in SharedServerPort
          address = server.bind_tcp(defn.host, port_cfg.port.to_i, reuse_port: true)
        in AutoServerPort
          # Binding to port 0 binds to an OS-assigned port.
          #
          # Reference: https://www.man7.org/linux/man-pages/man2/bind.2.html
          address = server.bind_tcp(defn.host, 0)
        end
      end
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerStartFailed.new(defn, e.message || "i/o error")
      return
    end

    spawn do
      queue << HttpRxListening.new
      server.listen
      # Listen returns when the server is closed.
      queue << HttpRxClosed.new
    rescue e : IO::Error | OpenSSL::Error
      queue << HttpRxCrashed.new(cause: e)
    ensure
      server.close rescue nil
    end

    pending = {} of HttpRequestId => Sync::Future(Term)

    begin
      loop do
        command = queue.shift

        case command
        in HttpRxListening
          info = Term[]
          if port_cfg.is_a?(AutoServerPort)
            info = Term[port: address.port]
          end

          observations << HttpServerStarted.new(defn, id, queue, info)
        in HttpRxClosed
          break
        in HttpRxCrashed
          raise command.cause
        in HttpRxRequest
          pending[command.request_id] = command.response
          observations << HttpRequestReceived.new(id, command.request_id, command.request)
        in HttpRespond
          next unless response = pending.delete(command.request_id)

          response.set(command.response)
          observations << HttpRequestHandled.new(command.request_id)
        in HttpReject
          unless response = pending.delete(command.request_id)
            Log.debug { "ignoring an attempt to reject a nonexistent request" }
            next
          end

          response.fail(HttpRequestRejectedException.new)
          observations << HttpRequestHandled.new(command.request_id)
        in HttpAddWebSocketHandler
          begin
            next if websocket_handler.enabled?

            link = command.link
            connect = ->(socket : HTTP::WebSocket, ctx : HTTP::Server::Context) do
              peer(observations, id, link, socket)
            end

            websocket_handler.enabled = true
          ensure
            observations << WebSocketHandlerAdded.new(id)
          end
        in HttpRemoveWebSocketHandler
          begin
            websocket_handler.enabled = false
            connect = connect_default
          ensure
            observations << WebSocketHandlerRemoved.new(id)
          end
        in Close
          server.close rescue nil
          break
        end
      end

      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    ensure
      # Make sure all waiting connections close as well.
      pending.each { |_, response| response.fail(HttpClosingException.new) }
      pending.clear
    end
  end
end
