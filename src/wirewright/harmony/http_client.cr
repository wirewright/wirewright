class Ww::Harmony
  defrecord HttpClientDefn,
    host : String,
    port : UInt16,
    key : Term,
    security : TlsClientConfig?,
    brief: true

  defrecord TlsClientConfig, verify : Bool, brief: true

  alias HttpClientQueue = BlockingQueue(HttpClientCommand)
  alias HttpClientCommand = HttpSendRequest | HttpCancelRequest | Close

  defrecord HttpSendRequest, request : Term
  defrecord HttpCancelRequest, request : Term

  # Constructs an HTTP client according to the given *defn*, and manages it until
  # the client closes or crashes. Reports observations pertaining to the client to
  # *observations* (e.g., `HttpClientStarted`, `HttpClientStopped`, `HttpClientResponse`).
  #
  # You can communicate with a client through its `HttpClientQueue`, which can be obtained
  # from the `HttpClientStarted` observation issued for *defn*.
  def self.client(observations : ObservationQueue, defn : HttpClientDefn) : Nil
    # We'll reuse this one client for subsequent requests.
    begin
      if tls_config = defn.security
        tls_context = OpenSSL::SSL::Context::Client.new
        case tls_config.verify
        in true
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::PEER
        in false
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::NONE
        end

        client = HTTP::Client.new(defn.host, defn.port.to_i, tls: tls_context)
      else
        client = HTTP::Client.new(defn.host, defn.port.to_i, tls: nil)
      end
    rescue e : IO::Error | OpenSSL::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    id = ClientId.new(UUID.random)
    queue = HttpClientQueue.new
    observations << HttpClientStarted.new(defn, id, queue)

    worker = BlockingQueue(HttpSendRequest | Close).new

    spawn do
      loop do
        command = worker.shift

        case command
        in HttpSendRequest
          result = HttpResponseError.new("invalid request")

          pass do
            next unless request = HttpRequestLanguage.decode?(command.request, HTTP::Request)

            response = client.exec(request)
            result = HttpResponseLanguage.encode(response)
          rescue e : IO::Error | OpenSSL::Error
            result = HttpResponseError.new(e.message || "i/o error")
          end

          observations << HttpResponseReceived.new(id, command.request, HttpResponseResult.new(result))
        in Close
          client.close
          break
        end
      end
    end

    begin
      loop do
        command = queue.shift

        case command
        in HttpSendRequest
          worker << command
        in HttpCancelRequest
          client.close
        in Close
          worker << Close.new
          client.close
          break
        end
      end
    ensure
      observations << ClientStopped.new(defn, id, "stopped")
    end
  end
end
