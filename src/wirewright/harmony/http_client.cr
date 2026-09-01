class Ww::Harmony
  defrecord HttpClientDefn,
    host : String,
    port : UInt16,
    key : Term,
    security : TlsClientConfig?,
    brief: true

  defrecord TlsClientConfig, verify : Bool, brief: true

  alias HttpClientQueue = BlockingQueue(HttpClientCommand)
  alias HttpClientCommand = HttpSendRequest | HttpCancelRequest | HttpWorkerExecResult | Close

  defrecord HttpSendRequest, request : Term
  defrecord HttpCancelRequest, request : Term

  defrecord HttpWorkerExec, seq : UInt64, request : Term
  defrecord HttpWorkerExecResult, seq : UInt64, result : HttpResponseResult

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

    worker = Channel(HttpWorkerExec).new

    spawn do
      while command = worker.receive?
        Log.debug { "HTTP client worker received #{command}" }

        case command
        in HttpWorkerExec
          result = HttpResponseError.new("invalid request")

          pass do
            next unless request = HttpRequestLanguage.decode?(command.request, HTTP::Request)

            request.headers["Host"] ||= "#{defn.host}:#{defn.port}"
            request.headers["Connection"] ||= "keep-alive"

            response = client.exec(request)
            result = HttpResponseLanguage.encode(response)
          rescue e : IO::Error | OpenSSL::Error
            Log.debug(exception: e) { "exec() died" }
            result = HttpResponseError.new(e.message || "i/o error")
          end

          queue << HttpWorkerExecResult.new(command.seq, HttpResponseResult.new(result))
        end
      end
    end

    inflight : HttpWorkerExec? = nil
    pending = Deque(HttpWorkerExec).new
    seq = 0u64 # Avoid ABA with canceled requests

    begin
      loop do
        command = queue.shift
        Log.debug { "HTTP client received #{command}" }

        case command
        in HttpSendRequest
          work = HttpWorkerExec.new(seq, command.request)
          seq += 1

          select
          when worker.send(work)
            inflight = work
          else
            pending << work
          end
        in HttpWorkerExecResult
          # Discard results from canceled requests if they do arrive somehow.
          next unless inflight
          next unless inflight.seq == command.seq

          observations << HttpResponseReceived.new(id, inflight.request, command.result)

          # Send more work.
          if work = pending.shift?
            worker.send(work) # The worker should be free here so we sync.
          end
          inflight = work
        in HttpCancelRequest
          pending.reject! do |candidate|
            candidate.request == command.request
          end

          next unless inflight
          next unless inflight.request == command.request

          client.close

          # Send more work.
          if work = pending.shift?
            # The worker should be free here so we sync. Or we really want it
            # to be free.
            worker.send(work)
          end
          inflight = work
        in Close
          worker.close
          client.close
          break
        end
      end
    ensure
      observations << ClientStopped.new(defn, id, "stopped")
    end
  end
end
