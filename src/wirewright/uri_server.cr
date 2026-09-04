module Ww
  # URI server is going to do HTTP stuff (and maybe other URI-related stuff). Since
  # I am by no means an expert in HTTP, I have no idea what we're going to do here.
  # We'll see. Right now we only do basic fetch and retry if failed (with backoff).
  # Once we've fetched we permanently store the response in memory and never fetch
  # again. It's not the best way to do it but would work for now.
  #
  # DEPRECATED: Use `HTTPService` instead.
  module URIServer
    extend self

    Log = ::Log.for(self)

    defrecord Present, content : Term::Blob
    defrecord Absent, detail : String

    @@running : Atomic(Bool) = Atomic.new(false)

    private def ensure_server_running!
      return if @@running.swap(true)

      spawn(name: "URIServer rloop") { rloop }
    end

    @@r_lock = Sync::Mutex.new
    @@r_demand = Pf::Set(URI).new
    @@r_supply = Pf::Map(URI, Present | Absent).new
    @@r_retrying = Pf::Set(URI).new
    @@r_changed = BlockingSignal.new

    @@r_waiters_signal = BlockingSignal.new

    def rloop
      Log.debug { "rloop: running" }

      epoch = 0u64

      loop do
        epoch = @@r_changed.wait(epoch)

        Log.trace { "rloop: woke up" }

        demand = @@r_lock.synchronize { @@r_demand }
        next if demand.empty?

        Log.trace { "rloop: fetch #{demand.size} demand(s)" }

        wg = WaitGroup.new(demand.size)

        demand.each do |uri|
          spawn(name: "fetch worker") do
            case fetch(uri)
            in .fetched?
              wg.done
            in .retry_later?
              wg.done
              # Start retrying on this fiber instead of spawning a new one.
              retry(uri)
            end
          end
        end

        @@r_lock.synchronize do
          @@r_demand -= demand
        end

        wg.wait
      end
    end

    enum FetchResult
      # The initial URL was fetched and `@@r_supply` was updated appropriately.
      Fetched

      # The initial URL was not fetched. The request should be retried later,
      # if possible.
      RetryLater
    end

    # This should clamp cyclic redirects & general bad behavior.
    MAX_REQUEST_CHAIN_SIZE = 8

    private def fetch(initial_uri : URI) : FetchResult
      chain = [initial_uri]

      supply = nil
      fetch_result = FetchResult::Fetched

      MAX_REQUEST_CHAIN_SIZE.times do
        break if supply

        uri = chain.last

        unless follow?(uri)
          supply = Absent.new("could not follow URI #{uri}")
          break
        end

        begin
          Log.debug { "fetch(#{initial_uri}): requesting #{uri}" }

          HTTP::Client.get(uri, headers: HTTP::Headers{"User-Agent" => ::Ww::USER_AGENT}) do |response|
            Log.debug { "fetch(#{initial_uri}): received response on #{uri} with status: #{response.status}" }

            case response.status
            when .ok?
              # HTTP 200: Make a blob from the response body, and we're done.
              #
              # If the server tells us the MIME type, use that, otherwise, we'll try to
              # guess it on our end.
              content = Term::Blob.build(classif: Term::Blob::Classif.of(response.mime_type)) do |dst|
                IO.copy(response.body_io, dst)
              end

              supply = Present.new(content)
            when .redirection?
              # HTTP 300-399: Redirection.
              #
              # See https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Redirections
              #
              # Let's assume optimistically that all such requests have a Location. So
              # retry the request with that.
              unless location = response.headers["Location"]?
                supply = Absent.new("HTTP redirection missing Location header")
                next
              end

              chain << URI.parse(location)
            else
              # Something else. Just give up for now.
              supply = Absent.new("HTTP status code #{response.status}")
            end
          end
        rescue e : IO::Error
          Log.debug(exception: e) { "fetch(#{initial_uri}): failed to fetch #{uri}" }

          supply = Absent.new("internal error")
          fetch_result = FetchResult::RetryLater
        end
      end

      return fetch_result unless supply

      Log.debug { "fetch(#{initial_uri}): completing request chain: #{chain}" }

      @@r_lock.synchronize do
        # We record all URIs that we've fetched (due to redirects there may be many
        # of them). We'll assoc() the resulting Present/Absent response to *all* of
        # these URIs (for now; I don't know if that's the expected behavior generally).
        chain.each do |uri|
          next if @@r_supply[uri]? == supply

          @@r_supply = @@r_supply.assoc(uri, supply)
          @@r_waiters_signal.call
        end
      end

      fetch_result
    end

    private def follow?(uri : URI) : Bool
      uri.scheme.in?("http", "https") # FIXME: is this enough?
    end

    # Spawns a fiber to retry *uri* with exponential decay if not already retrying.
    # Remove *uri* from `r_retrying` to cancel.
    private def retry(uri : URI) : Nil
      @@r_lock.synchronize do
        return if uri.in?(@@r_retrying)

        @@r_retrying = @@r_retrying.add(uri)
      end

      rng = Random::PCG32.new

      (0..).each do |cycle|
        # Quite obviously I've no idea what I'm doing . . .
        case cycle
        when 0
          nap = (300..500).sample(rng).milliseconds
        when 1
          nap = (400..800).sample(rng).milliseconds
        when 2
          nap = (100..600).sample(rng).milliseconds
        else
          k = 2**Math.min(9, cycle) - 1
          nap = (k * 50..k * 80).sample(rng).milliseconds
        end

        Log.trace { "retrying #{uri} after #{nap.humanize}" }

        sleep nap

        break unless @@r_lock.synchronize { uri.in?(@@r_retrying) }

        case fetch(uri)
        in .fetched?
          @@r_lock.synchronize do
            @@r_retrying = @@r_retrying.delete(uri)
          end
          break
        in .retry_later?
          # OK, keep trying.
        end
      end

      Log.trace { "complete retrying #{uri}" }
    end

    defrecord Wait

    def get(uri : URI) : Present | Absent | Wait
      ensure_server_running!

      @@r_lock.synchronize do
        if status = @@r_supply[uri]?
          return status
        end

        @@r_demand = @@r_demand.add(uri)
        @@r_changed.call
      end

      Wait.new
    end

    def wait(epoch : UInt64)
      @@r_waiters_signal.wait(epoch)
    end
  end
end
