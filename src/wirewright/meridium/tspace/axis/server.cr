module Ww::Meridium::Axis
  # :nodoc:
  #
  # Implements a simple lock-protected reference-counted atom multiset.
  struct AtomMultiset
    @atoms = {} of Atom => Hash(WWID, UInt32)
    @lock = Mutex.new

    def size : Int32
      @lock.synchronize { @atoms.size }
    end

    def present?(atom : Atom) : Bool
      @lock.synchronize { @atoms.has_key?(atom) }
    end

    def present?(atoms : Enumerable(Atom)) : BitList
      bits = BitList.new
      atoms.each do |atom|
        bits << present?(atom)
      end
      bits
    end

    def incref(referrer : WWID, atom : Atom) : Nil
      @lock.synchronize do
        refs = @atoms.put_if_absent(atom) { {} of WWID => UInt32 }
        refs[referrer] = (refs[referrer]? || 0u32) + 1
      end
    end

    def decref(referrer : WWID, atom : Atom) : Nil
      @lock.synchronize do
        return unless refs = @atoms[atom]?
        return unless refcount = refs[referrer]?

        unless refcount == 1
          refs[referrer] = refcount - 1
          return
        end

        refs.delete(referrer)
        if refs.empty?
          @atoms.delete(atom)
        end
      end
    end

    def sweep(referrer : WWID) : Int32
      @lock.synchronize do
        size0 = @atoms.size

        @atoms.reject! do |_, referrers|
          !!referrers.delete(referrer) && referrers.empty?
        end

        size0 - @atoms.size
      end
    end

    def clear : Nil
      @lock.synchronize { @atoms.clear }
    end
  end

  # :nodoc:
  #
  # Routes each atom to one of *N* `AtomMultiset`s based on the hash code. Used
  # to increase parallelism of the simple lock-based `AtomMultiset`.
  struct BucketizedAtomMultiset(N)
    def initialize
      @buckets = Pointer(AtomMultiset).malloc(N) { AtomMultiset.new }
    end

    private def bucket(atom : Atom) : AtomMultiset
      @buckets[atom.blk0 % N]
    end

    def approx_size : Int32
      @buckets.to_slice(N).sum(&.size)
    end

    def present?(atoms : Enumerable(Atom)) : BitList
      bits = BitList.new
      atoms.each do |atom|
        bits << bucket(atom).present?(atom)
      end
      bits
    end

    def incref(referrer : WWID, atom : Atom) : Nil
      bucket(atom).incref(referrer, atom)
    end

    def decref(referrer : WWID, atom : Atom) : Nil
      bucket(atom).decref(referrer, atom)
    end

    def sweep(referrer : WWID) : Int32
      wg = WaitGroup.new
      swept = Atomic(Int32).new(0)

      (0...N).in_subranges_of(N//System.cpu_count.to_i) do |subrange|
        Log.trace { "#{referrer}: sweep: spawn cleaner on subrange #{subrange}" }

        wg.spawn do
          subrange.each do |index|
            swept_ = @buckets[index].sweep(referrer)
            swept.add(swept_, :relaxed)
          end
        end
      end

      wg.wait

      swept.get(:relaxed)
    end

    def clear : Nil
      @buckets.to_slice(N).each(&.clear)
    end
  end

  class Server
    Log = ::Log.for(self)

    # :nodoc:
    getter routes
    # :nodoc:
    getter atoms

    # NOTE: the server will be closed and cleanup procedures will run after
    # *socket* is closed.
    def initialize(@socket : Socket)
      @routes = SyncHash(WWID, Session).new
      @atoms = BucketizedAtomMultiset(128).new
      @sessions = SyncHash(Session, Bool).new
    end

    # Returns a pair of procs: the first one starts the server and the second
    # one stops it if it is running. Both are nonblocking. The caller may proceed
    # to do whatever they want.
    def self.control(&fn : -> Socket) : {->, ->}
      socket = nil
      lock = Mutex.new
      running = Channel(Nil).new

      start = -> do
        lock.synchronize do
          if socket
            Log.debug { "control: will not start several times in a row" }
            next
          end

          socket = socket_ = fn.call
          server = Server.new(socket_)

          spawn server.listen

          spawn do
            loop do
              select
              when running.receive? # nil
                break
              when timeout(16.seconds)
                Log.debug { "control: server stats approx #{server.stats}" }
              end
            end
          end
        end
      end

      stop = -> do
        lock.synchronize do
          unless socket_ = socket
            Log.debug { "control: will not stop a server that is not running" }
            next
          end

          socket_.close
          socket = nil
          running.send(nil)
        end
      end

      {start, stop}
    end

    # Returns a `NamedTuple` with some useful stats about the server. Can be
    # logged periodically to check the server's health.
    def stats
      {routes: @routes.size, atoms: @atoms.approx_size}
    end

    # Starts the listen loop in the calling fiber.
    def listen : Nil
      Log.info { "server is accepting clients on #{@socket}" }

      while client = @socket.accept?
        Log.trace { "accepted #{client}" }

        Axis.configure(client)

        session = Session.new(self, client)

        @sessions[session] = true

        spawn do
          session.listen
        rescue e : NetworkError
          Log.trace(exception: e) { "session ended" }
        rescue e : Exception
          Log.debug(exception: e) { "session crashed" }
        ensure
          @sessions.delete(session)
        end
      end
    ensure
      Log.trace { "server is not accepting clients anymore" }

      @socket.close
      @sessions.each { |session, _| session.shutdown }
      @sessions.clear
    end
  end
end
