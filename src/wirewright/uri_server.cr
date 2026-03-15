module Ww
  # URI server is going to do HTTP stuff (and maybe other URI-related stuff). Since
  # I am by no means an expert in HTTP, I have no idea what we're going to do here.
  # We'll see. Right now we only do basic fetch and retry if failed (with backoff).
  # Once we've fetched we permanently store the response in memory and never fetch
  # again. It's not the best way to do it but would work for now.
  module URIServer
    extend self

    defrecord Present, content : Term::Blob
    defrecord Absent, detail : String

    @@r_lock = Sync::Mutex.new
    @@r_demand = Pf::Set(URI).new
    @@r_supply = Pf::Map(URI, Present | Absent).new
    @@r_retrying = Pf::Set(URI).new
    @@r_changed = BlockingSignal.new

    alias Msg = URIAdded

    defrecord URIAdded, uri : URI

    @@running : Atomic(Bool) = Atomic.new(false)

    def ensure_server_running!
      return if @@running.swap(true)

      spawn(name: "URIServer msgloop") { msgloop }
    end

    def msgloop
      epoch = 0u64

      loop do
        epoch = @@r_changed.wait(epoch)

        demand = @@r_lock.synchronize do
          demand0 = @@r_demand
          demand1 = Pf::Set(URI).new
          @@r_demand = demand1
          demand0
        end

        next if demand.empty?

        wg = WaitGroup.new(demand.size)

        demand.each do |uri|
          wg.spawn(name: "fetch worker") do
            unless fetch?(uri)
              retry(uri)
            end
          end
        end

        wg.wait
      end
    end

    # Returns `true` if *uri* was fetched and `@@r_supply` was updated appropriately.
    private def fetch?(uri : URI) : Bool
      begin
        status = 404

        content = Term::Blob.build do |dst|
          HTTP::Client.get(uri, headers: HTTP::Headers{"User-Agent" => "Wirewright"}) do |response|
            status = response.status

            IO.copy(response.body_io, dst)
          end
        end

        if status
          supply = Present.new(content)
        else
          supply = Absent.new("unrecognized HTTP status code #{status}")
        end

        result = true # fetched
      rescue e : IO::Error
        Log.debug(exception: e) { "fetch worker failed for #{uri}" }

        supply = Absent.new("internal error")
        result = false # did not fetch
      end

      @@r_lock.synchronize do
        @@r_supply = @@r_supply.assoc(uri, supply)
      end

      result
    end

    # Spawns a fiber to retry *uri* with exponential decay if not already retrying.
    # Remove *uri* from `r_retrying` to cancel.
    private def retry(uri : URI) : Nil
      @@r_lock.synchronize do
        return if uri.in?(@@r_retrying)

        @@r_retrying = @@r_retrying.add(uri)
      end

      spawn(name: "retry fiber for #{uri}") do
        rng = Random::PCG32.new

        (0..).each do |cycle|
          case cycle
          when 0
            nap = (50..100).sample(rng).milliseconds
          when 1
            nap = (100..300).sample(rng).milliseconds
          when 2
            nap = (50..500).sample(rng).milliseconds
          when 3..5
            k = 2**cycle - 1
            nap = (k * 50..k * 80).sample(rng).milliseconds
          else
            k = 2**6 - 1
            nap = (k * 50..k * 80).sample(rng).milliseconds
          end

          Log.trace { "retrying #{uri} after #{nap.humanize}" }

          sleep nap

          break unless @@r_lock.synchronize { uri.in?(@@r_retrying) }
          break if fetch?(uri)
        end

        Log.trace { "complete retrying #{uri}" }
      end
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
      @@r_changed.wait(epoch)
    end
  end
end
