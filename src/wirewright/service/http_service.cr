module Ww
  # A poll-oriented, global HTTP client service.
  #
  # Currently, only the HTTP GET method is supported. More methods may be
  # supported in the future.
  #
  # Responses are currently only cached in memory. They are invalidated due
  # to cache eviction (see `CACHE_CAPACITY` and `CACHE_THRESHOLD_BYTES`).
  # You can invalidate them manually using `invalidate`.
  module HTTPService
    extend self

    Log = ::Log.for(self)

    # Retry base for exponential delay.
    RETRY_BASE_DELAY = 300.milliseconds

    # The number of retry attempts to make before giving up. The delays between
    # each attempt are determined using exponential decay.
    MAX_RETRY_ATTEMPTS = 5

    # The number of redirect responses to follow before giving up.
    MAX_REDIRECT_CHAIN = 8

    # The number of slots allocated for the response cache.
    CACHE_CAPACITY = 512

    # Memory use (in bytes) to tolerate for the response cache (approximate).
    # See `ThresholdLRU` for more info on how `CACHE_CAPACITY` and
    # `CACHE_THRESHOLD_BYTES` interact.
    CACHE_THRESHOLD_BYTES = 512u64 * 1024 * 1024 # 512 MiB

    alias Response = Present | Absent | Aborted

    # Signals success: *status* is one of 2xx statuses, and *body* is the HTTP
    # response body.
    defrecord Present, status : HTTP::Status, body : Term::Blob

    # Signals failure associated with HTTP, including *status*es such as 4xx and 5xx.
    # *detail* may provide further explanation. *body* is the HTTP response body.
    defrecord Absent, detail : String, status : HTTP::Status, body : Term::Blob

    # Signals that the fetch algorithm stopped fetching a URI due to an error
    # not associated with HTTP. *detail* may provide further explanation.
    defrecord Aborted, detail : String

    alias Notification = ResponseReady

    # Signals that a `Response` for *uri* is ready; *uri*'s corresponding promise was
    # fulfilled and discarded.
    defrecord ResponseReady, uri : URI

    @@lock = Sync::Mutex.new
    @@running = false
    @@cache = Cache.new
    @@workspace = {} of URI => Sync::Future(Response)
    @@msgs = BlockingQueue(Msg).new

    # :nodoc:
    alias Msg = URIAdded

    # :nodoc:
    defrecord URIAdded, uri : URI

    # WARNING: Expects `@@lock` to be taken.
    private def ensure_running! : Nil
      return if @@running

      @@running = true

      spawn(name: "HTTPService message loop") do
        msgloop = Msgloop.new
        loop do
          msg = @@msgs.shift
          msgloop.receive(msg)
        end
      end
    end

    private class Msgloop
      def receive(msg : Msg)
        Log.trace { msg }

        handle(msg)
      end

      # NOTE: Obviously this assumes that URIAdded messages are unique (ish),
      # as in, nobody spams them for a particular URI. HTTPService certainly doesn't,
      # as URIAdded is only emitted on key absent -> key present transitions
      # in @@workspace, which in turn runs only if @@cache is missing the URI.
      private def handle(msg : URIAdded) : Nil
        spawn(name: "fetch(#{msg.uri})") do
          response = fetch(msg.uri)
          HTTPService.broadcast(msg.uri, response)
        end
      end

      defrecord Retry, cause : Absent | Aborted
      defrecord RetryAfter, cause : Absent | Aborted, nap : Time::Span
      defrecord Redirect, uri : URI

      private def fetch(uri : URI) : Response
        path = Slice[uri]
        retries = 0

        loop do
          if retries > 0
            nap = RETRY_BASE_DELAY * 2**retries * rand
            Log.debug { "fetch(#{path.first}): sleep for #{nap.humanize}" }
            sleep(nap)
          end

          Log.debug { "fetch(#{path.first}): fetch #{path.last}" }

          result = fetch_just(path.last)
          if result.is_a?(Response)
            return result
          end

          Log.debug { "fetch(#{path.first}): #{result}" }

          case result
          in Redirect
            if path.size >= MAX_REDIRECT_CHAIN
              return Aborted.new("redirect chain too long")
            end

            path = path.append(result.uri)
            retries = 0
          in Retry
            if retries + 1 > MAX_RETRY_ATTEMPTS
              return result.cause
            end

            retries += 1
          in RetryAfter
            sleep result.nap

            # This disables retries on the next iteration. The server responded, too,
            # so we reset the retry counter assuming someone's there on the other end
            # of the wire.
            retries = 0
          end
        end
      end

      # :nodoc:
      HTTP_DATE_FORMAT = "%a, %d %b %Y %T GMT"

      # References:
      # - https://recurohq.com/blog/retry-failed-http-requests-exponential-backoff
      # - https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Redirections
      # - https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Retry-After
      private def fetch_just(target : URI) : Response | Redirect | Retry | RetryAfter
        unless target.scheme.in?("http", "https")
          return Aborted.new("unsupported URI scheme, expected: http, https")
        end

        HTTP::Client.get(target, headers: HTTP::Headers{"User-Agent" => "Wirewright"}) do |response|
          Log.trace { "received response on #{target} with status: #{response.status}" }

          body = Term::Blob.build(classify: false) do |dst|
            IO.copy(response.body_io, dst)
          end

          # If the server tells us the MIME type, use that, otherwise, we'll try to
          # guess it.
          #
          # If the server says application/octet-stream, aka binary, then we'll try to
          # classify it ourselves as well. I'm not sure how good the idea is, but I'm
          # getting unreliable mime_types on some platforms without this.
          if (mime_type = response.mime_type) && mime_type.media_type != "application/octet-stream"
            classif = Term::Blob::Classif.of(mime_type)
          end
          body.classify!(classif)

          case response.status
          when .success?
            # HTTP 2xx
            Present.new(response.status, body)
          when .redirection?
            # HTTP 3xx: Redirection.
            unless location = response.headers["Location"]?
              return Aborted.new("HTTP redirection is missing the Location header")
            end

            successor = URI.parse(location)
            Redirect.new(successor)
          when .request_timeout?,
               .internal_server_error?,
               .bad_gateway?,
               .gateway_timeout?
            Retry.new(Absent.new("HTTP server error", response.status, body))
          when .too_many_requests?, .service_unavailable?
            unless retry_after = response.headers["Retry-After"]?
              return Retry.new(Absent.new("HTTP server error", response.status, body))
            end

            if retry_seconds = retry_after.to_i?
              retry_span = retry_seconds.seconds
            else
              begin
                retry_time = Time.parse(retry_after, HTTP_DATE_FORMAT, Time::Location::UTC)
                retry_span = retry_time - Time.utc
              rescue e : Time::Error
                return Retry.new(Absent.new("HTTP server error", response.status, body))
              end
            end

            RetryAfter.new(Absent.new("HTTP server error", response.status, body), retry_span)
          when .client_error?
            # HTTP 4xx
            Absent.new("HTTP client error", response.status, body)
          else
            Absent.new("unhandled HTTP status", response.status, body)
          end
        end
      rescue e : IO::Error
        Log.trace(exception: e) { "retry trigger on #{target}" }

        Retry.new(Aborted.new(e.message || "input/output error"))
      end
    end

    # NOTE: Right now, this is a very crude in-memory cache. We'd probably want
    # to evict some stuff to the disk. Obviously we're also missing Expires-At
    # handling among many other things.
    private class Cache
      defcase ResponseRef, response : Response do
        # NOTE: Approximate
        def bytesize : UInt64
          case tmp = response
          in Present then tmp.body.ubytesize64
          in Absent  then sizeof(Absent).to_u64 + tmp.body.ubytesize64
          in Aborted then sizeof(Aborted).to_u64
          end
        end
      end

      def initialize
        @lru = ThresholdLRU(URI, ResponseRef).new(CACHE_CAPACITY, CACHE_THRESHOLD_BYTES)
      end

      def get?(uri : URI) : Response?
        return unless response_ref = @lru.get?(uri)

        response_ref.response
      end

      def put(uri : URI, response : Response) : Response
        @lru.put(uri, ResponseRef.new(response))

        response
      end

      def delete(uri : URI) : Response?
        return unless response_ref = @lru.delete(uri)

        response_ref.response
      end
    end

    # :nodoc:
    def broadcast(uri : URI, response : Response) : Nil
      result = @@lock.synchronize do
        @@cache.put(uri, response)
        @@workspace[uri]?
      end

      return unless result # ?!

      result.set(response)

      @@lock.synchronize do
        @@workspace.delete(uri)
      end

      broadcast(ResponseReady.new(uri))
    end

    # Returns the `Response` for *uri*.
    #
    # The response is cached until eviction or invalidation (see `invalidate`).
    #
    # This function is poll-friendly: it can be called millions of times per second on
    # modern hardware, with very little overhead. IO and retries are performed on
    # separate fibers.
    #
    # You can execute multiple fetches simultaneosuly simply by scheduling them
    # before waiting on them:
    #
    # ```
    # requests = [
    #   HTTPService.get(URI.parse("http://127.0.0.1:5000/city.jpg")),
    #   HTTPService.get(URI.parse("http://127.0.0.1:5000/avatar-female1-32.jpg")),
    #   HTTPService.get(URI.parse("http://127.0.0.1:5000/soma-256x256-white.png")),
    #   HTTPService.get(URI.parse("http://127.0.0.1:5000/d7-logo.svg")),
    #   HTTPService.get(URI.parse("http://127.0.0.1:5000/Material%20Icons/MaterialIconsOutlined-Regular.otf")),
    #   HTTPService.get(URI.parse("http://127.0.0.1:5000/Julia%20Mono/JuliaMono-Regular.ttf")),
    #   HTTPService.get(URI.parse("https://picsum.photos/200/300")),
    # ]
    # requests.each &.wait
    # ```
    #
    # This applies less to polling since polling is non-blocking anyway.
    def get(uri : URI) : Promise(Response)
      @@lock.synchronize do
        ensure_running!

        if response = @@cache.get?(uri)
          return Promise(Response).resolved(response)
        end

        unless result = @@workspace[uri]?
          @@workspace[uri] = result = Sync::Future(Response).new
          @@msgs << URIAdded.new(uri)
        end

        Promise.new(result)
      end
    end

    # Invalidates all caches associated with *uri*.
    def invalidate(uri : URI) : Nil
      @@lock.synchronize { @@cache.delete(uri) }
    end

    include ServiceBroadcast(Notification)
  end
end
