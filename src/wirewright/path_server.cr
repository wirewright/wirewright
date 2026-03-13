module Ww
  # A polling, in-memory API for accessing the file system.
  #
  # NOTE: This API will (probably!) work fine as long as you have many
  # small files, several large ones, or both. If everything adds up to maybe
  # <512MB you're fine, although the true limit is how much RAM the user machine
  # has. However, if you're trying to read large files, you should use something
  # else. This is an *in-memory* API so all files will be loaded into memory,
  # sometimes sparingly, their hashes computed, and so on. This will be very
  # expensive for large files and you'll be bounded by RAM.
  #
  # NOTE: We explicitly refuse to load files larger than `SAFE_FILE_BYTESIZE`.
  #
  # The main design goal of `PathServer` is to make calls to `converge` and `view`
  # very cheap, so that you can call the millions of times per second. The rest
  # of the design falls out naturally out of this.
  #
  # So what you're looking at is a state synchronization engine (in-memory cache vs.
  # file system) with eventual consistency on both the read and write end. So
  # when you call `converge` to write a file we can't guarantee you'll see it on
  # disk the next instant, but if you call it many times the probability rises,
  # and if you wait until `view` reports your file exists with the content you
  # wanted it to have, then everything is OK and the write is a success.
  #
  # Metaphorically speaking, jon't believe your arms moved (file system state changed)
  # on your command (`converge`) until you see it with your own eyes (`view`).
  module PathServer
    extend self

    Log = ::Log.for(self)

    alias Fact = IsFile | IsDir | FileEntryFact | DirEntryFact

    defrecord IsFile, content : Term::Blob
    defrecord IsDir

    alias FileEntryFact = FilePresent | FileAbsent

    defrecord FilePresent, name : String
    defrecord FileAbsent, name : String

    alias DirEntryFact = DirPresent | DirAbsent

    defrecord DirPresent, name : String
    defrecord DirAbsent, name : String

    alias Listing = DirListing | FileListing

    defrecord DirListing, timestamp : Time, entries : Array(DirListingEntry)
    defrecord FileListing, timestamp : Time, content : Term::Blob

    alias DirListingEntry = FileEntry | DirEntry

    defrecord FileEntry, path : Path, timestamp : Time
    defrecord DirEntry, path : Path, timestamp : Time

    # Returns `true` if two facts *a* and *b* contradict each other.
    private def contradicts?(a : Fact, b : Fact) : Bool
      return false if a == b

      case {a, b}
      when {IsDir, FileEntryFact},
           {FileEntryFact, IsDir},
           {IsDir, DirEntryFact},
           {DirEntryFact, IsDir}
        false
      when {FilePresent, DirAbsent},
           {DirAbsent, FilePresent}
        # E.g. (file "x") and (-dir "x") aren't contradictory.
        false
      when {FileEntryFact, FileEntryFact},
           {FileEntryFact, DirEntryFact},
           {DirEntryFact, FileEntryFact},
           {DirEntryFact, DirEntryFact}
        a.name == b.name
      else
        true
      end
    end

    # Repairs the set of facts *facts0* by removing contradictory facts, producing
    # a consensus expected state.
    private def repair(facts0 : Pf::Set(Fact)) : Pf::Set(Fact)
      facts0.transaction do |txn|
        facts0.each do |fact|
          next unless facts0.any? { |other| contradicts?(fact, other) }

          txn.delete(fact)
        end
      end
    end

    @@running = Atomic(Bool).new(false)

    private def ensure_server_running! : Nil
      return if @@running.swap(true)

      spawn(name: "PathServer read loop") { rloop }
      spawn(name: "PathServer write loop") { wloop }
      spawn(name: "PathServer monitor alarm loop") do
        loop do
          PathMonitor.wait

          @@r_alarm.call
        end
      end
    end

    @@r_lock = Sync::Mutex.new
    @@r_alarm = BlockingSignal.new
    @@r_demand = Pf::Map(Path, Time::Instant).new
    @@r_supply = Pf::Map(Path, Supply).new

    alias Supply = Present | Absent

    defrecord Present, listing : Listing, version : UInt64
    defrecord Absent

    # We refuse to read files past this bytesize. We assume a different subsystem exists
    # for reading such files. Since we load all files into memory and talk to the file system
    # in an unpredictable manner, we can't do this for arbitrary files; only for very
    # small files.
    SAFE_FILE_BYTESIZE = 32 * 1024 * 1024 # 32 MiB

    DEMAND_TTL                = 5.seconds
    DEMAND_TTL_ALMOST_EXPIRED = DEMAND_TTL * 0.6 # 60%

    # Read loop
    private def rloop : Nil
      Log.debug { "rloop: running" }

      loop do
        @@r_alarm.wait

        Log.trace { "rloop: woke up" }

        instant = Time.instant

        # Atomically decay entries in the demand map, and sample it and the current supply.
        #
        # Note that we're the only ones writing to @@r_supply. Everybody else's access
        # is read-only. On the other hand for @@r_demand, we can prune it and we can
        # read from it, that's it. More discrete modifications are not ours.
        demand, supply0 = @@r_lock.synchronize do
          @@r_demand = @@r_demand.transaction do |txn|
            @@r_demand.each do |path, accessed_at|
              next if instant - accessed_at <= DEMAND_TTL

              Log.debug { "#{path} was not accessed in #{DEMAND_TTL.humanize}, removing from demands" }

              txn.dissoc(path)
            end
          end

          {@@r_demand, @@r_supply}
        end

        # Process demands.
        supply1 = Pf::Map(Path, Supply).transaction do |txn|
          demand.each do |path, accessed_at|
            state = supply0[path]?

            case status = PathMonitor.status(path)
            in PathMonitor::Wait
              next
            in PathMonitor::Absent
              txn.assoc(path, Absent.new)
              next
            in PathMonitor::Present
            end

            # If we already have an entry, and versions are the same, then nothing
            # changed in the meantime; keep the existing entry.
            if state.is_a?(Present) && status.version == state.version
              txn.assoc(path, state)
              next
            end

            begin
              # If the version number changed, request metadata from disk.
              info = File.info(path)

              # If timestamps changed, proceed. If they haven't, we had some sort of
              # spurious version change which we ignore.
              next if state.is_a?(Present) && state.listing.timestamp == info.modification_time

              # Otherwise, something really did change and we re-inspect the underlying
              # file system entity.
              if info.directory?
                entries = [] of DirListingEntry

                Dir.each_child(path) do |entry|
                  entry_path = path / entry

                  begin
                    entry_info = File.info(entry_path)
                    if entry_info.file?
                      entries << FileEntry.new(entry_path, entry_info.modification_time)
                    elsif entry_info.directory?
                      entries << DirEntry.new(entry_path, entry_info.modification_time)
                    end
                  rescue e : File::Error
                    Log.trace(exception: e) { "file error while inspecting directory entry #{entry_path}" }

                    # Skip a cycle for this entry until things settle down.
                    next
                  end
                end

                listing = DirListing.new(info.modification_time, entries)
              elsif info.file?
                if info.size > SAFE_FILE_BYTESIZE
                  Log.info { "ignoring #{path} because it exceeds safe file size of #{SAFE_FILE_BYTESIZE.humanize_bytes}" }
                  next
                end

                content = File.open(path, "rb") do |src|
                  Term::Blob.build(classify: true) do |dst|
                    IO.copy(src, dst)
                  end
                end

                listing = FileListing.new(info.modification_time, content)
              else
                next
              end

              txn.assoc(path, Present.new(listing, status.version))
            rescue e : File::Error | IO::Error
              Log.trace(exception: e) { "error while inspecting #{path}" }

              # Skip a cycle for this path until things settle down.
            end
          end
        end

        # Commit new supply atomically.
        @@r_lock.synchronize do
          @@r_supply = supply1
        end

        Log.trace { "rloop: wait_cv broadcast" }

        @@wait_cv.broadcast
      end
    end

    @@w_alarm = BlockingSignal.new
    @@w_supply = Pf::Map(Path, Pf::Set(Fact)).new
    @@w_supply_lock = Sync::Mutex.new

    # These @@vars are only accessed in unify() which is called exclusively
    # by wloop so they don't need any protection.
    @@w_rng = Random::PCG32.new
    @@w_model = {} of Path => Bytes
    @@w_model_lock = Sync::Mutex.new

    # Write loop
    private def wloop : Nil
      Log.debug { "wloop: running" }

      # If nobody asked about paths recently, then wloop did not ask about paths
      # recently; thus they can be removed from @@w_model safely regardless of
      # whether they exist there.
      PathMonitor.paths_finalize do |paths|
        Log.debug { "wloop: finalize #{paths}" }

        @@w_model_lock.synchronize do
          paths.each do |path|
            @@w_model.delete(path)
          end
        end

        Log.debug { "wloop: finalized" }
      end

      loop do
        @@w_alarm.wait

        Log.trace { "wloop: woke up" }

        # Atomically read and clear the supply.
        supply0 = @@w_supply_lock.synchronize do
          supply = @@w_supply
          @@w_supply = Pf::Map(Path, Pf::Set(Fact)).new
          supply
        end

        next if supply0.empty?

        Log.trace { "wloop: process supply with #{supply0.size} entries" }

        # Repair & unify facts.
        @@w_model_lock.synchronize do
          supply0.each do |path, facts|
            facts = repair(facts)
            facts.each do |fact|
              unify(path, fact)
            end
          end
        end
      end
    end

    private def unify(path : Path, fact : IsFile) : Nil
      case status = PathMonitor.status(path)
      in PathMonitor::Wait
        @@w_model.delete(path)
        return
      in PathMonitor::Absent
        @@w_model.delete(path)
      in PathMonitor::Present
        # We do not write over changes done externally because, as I "discovered",
        # they're *very* hard to distinguish from writes of our own. It's very easy
        # to get into a feedback loop if we were, say, just comparing status.version.
        # Moreover, not writing over external changes is actually useful sometimes:
        # users can modify the file externally and the program can observe it and
        # "fix" the file instead of trying to enforce the old version.
        if digest = @@w_model[path]?
          return if digest == fact.content.digest
        end
      end

      Log.debug { "wloop: begin atomic write to #{path}" }

      tmp_file = File.tempfile(@@w_rng, tempdir: path.parent)
      tmp_path = tmp_file.path

      begin
        tmp_file.write(fact.content.bytes)
        tmp_file.fsync
      ensure
        tmp_file.close
      end

      File.rename(tmp_path, path)

      @@w_model[path] = fact.content.digest

      Log.debug { "wloop: wrote to #{path}: #{fact.content.digest}" }
    rescue e : File::Error
      Log.debug(exception: e) { "wloop: error while writing to #{path}" }
    end

    private def unify(path : Path, fact : IsDir) : Nil
      case PathMonitor.status(path)
      in PathMonitor::Wait
      in PathMonitor::Absent
        Log.debug { "wloop: creating directory #{path}" }

        begin
          Dir.mkdir(path)
        rescue e : File::Error
          Log.debug(exception: e) { "wloop: error while creating directory #{path}" }
        end
      in PathMonitor::Present
      end
    end

    private def unify(path : Path, fact : FilePresent) : Nil
      entry_path = path / fact.name

      case PathMonitor.status(entry_path)
      in PathMonitor::Wait
      in PathMonitor::Absent
        Log.debug { "wloop: creating file #{entry_path}" }

        begin
          File.touch(entry_path)
        rescue e : File::Error
          Log.debug(exception: e) { "wloop: error while creating file #{entry_path}" }
        end
      in PathMonitor::Present
      end
    end

    private def unify(path : Path, fact : FileAbsent) : Nil
      entry_path = path / fact.name

      case PathMonitor.status(entry_path)
      in PathMonitor::Wait
      in PathMonitor::Absent
      in PathMonitor::Present
        Log.debug { "wloop: removing file #{entry_path}" }

        begin
          File.delete(entry_path)
        rescue e : File::Error
          Log.debug(exception: e) { "wloop: error while removing file #{entry_path}" }
        end
      end
    end

    private def unify(path : Path, fact : DirPresent) : Nil
      entry_path = path / fact.name

      case PathMonitor.status(entry_path)
      in PathMonitor::Wait
      in PathMonitor::Absent
        Log.debug { "wloop: creating directory #{entry_path}" }

        begin
          Dir.mkdir(entry_path)
        rescue e : File::Error
          Log.debug(exception: e) { "wloop: error while creating directory #{entry_path}" }
        end
      in PathMonitor::Present
      end
    end

    private def unify(path : Path, fact : DirAbsent) : Nil
      entry_path = path / fact.name

      case PathMonitor.status(entry_path)
      in PathMonitor::Wait
      in PathMonitor::Absent
      in PathMonitor::Present
        Log.debug { "wloop: removing directory #{entry_path}" }

        begin
          Dir.delete(entry_path)
        rescue e : File::Error
          Log.debug(exception: e) { "wloop: error while removing directory #{entry_path}" }
        end
      end
    end

    # Indicates that the caller's request was acknowledged and is being processed
    # now. The caller should call later to get a `View`.
    defrecord Wait

    alias View = Listing | Absent

    # Returns a view of *path*. The returned view is a snapshot of the file
    # system at some unspecified point in time. The view is *eventually consistent*:
    # it may not reflect the instantaneous state of the file system.
    def view(path : Path) : View | Wait
      path = path.normalize

      ensure_server_running!

      @@r_lock.synchronize do
        @@r_demand = @@r_demand.assoc(path, Time.instant)

        case state = @@r_supply[path]?
        in Nil
        in Absent  then return state
        in Present then return state.listing
        end

        @@r_alarm.call

        Wait.new
      end
    end

    @@wait_cv_lock = Sync::Mutex.new
    @@wait_cv = Sync::ConditionVariable.new(@@wait_cv_lock)

    # Blocks the current fiber until the view of some path changes, or spuriously.
    #
    # In all cases, callers should call `view` to learn about the change or
    # resurrect paths they care about until it's too late. We assume each caller
    # knows what they're waiting for.
    def wait : Nil
      @@wait_cv_lock.synchronize { @@wait_cv.wait }
    end

    # Associates a set of *facts* with *path*.
    #
    # WARNING: Just one attempt at unification is made. Whether it succeeds *eventually*
    # or not, is not guaranteed. It is certainly not guaranteed that it succeeds immediately
    # after the call.
    #
    # To ensure persistence, this operation should be executed continuously until its
    # results are independently observed. That is, if you want a guarantee that e.g.
    # a file was written to disk, the only way you can know that for sure is by inspecting
    # the contents of the disk while calling this function. That is, you need a control loop.
    # The moment you see the file you're writing appear on disk, you stop calling `converge`;
    # done. This function basically single-steps toward "convergence" of file system state
    # and the desired state described by non-contradicting *facts*. Whether this succeeds on
    # this iteration, or on the next one, or never if e.g. some malicious agent is deleting
    # the file the moment we write it, is indeterminate.
    #
    # In other words, the only way to know you've saved something on disk is to read it
    # from the disk. `PathServer` and related embrace this fully; despite the inefficiencies
    # involved (esp. in having to read and keep files in memory). We believe the trade-offs
    # are in favor in the kilobyte to megabyte file range. You are expected to use a different
    # subsystem for handling large files.
    def converge(path : Path, facts facts1 : Pf::Set(Fact)) : Nil
      path = path.normalize

      ensure_server_running!

      @@w_supply_lock.synchronize do
        if facts0 = @@w_supply[path]?
          facts1 += facts0
        end

        @@w_supply = @@w_supply.assoc(path, facts1)
        @@w_alarm.call
      end
    end

    # A shorthand for when you know all facts ahead-of-time and don't want to
    # construct the set of facts manually.
    def converge(path : Path, *facts : Fact) : Nil
      converge(path, facts.map(&.as(Fact)).to_pf_set)
    end

    # Raised by high-level functions such as `read` and `write`. The polling
    # API never raises anything (unless there is an implementation error).
    class Error < Exception
    end

    # Loads the file at *path* into memory and returns its content, as a slice
    # of bytes. Raises `Error` if the file cannot be read.
    def read(path : Path) : Term::Blob
      loop do
        listing = view(path)
        if listing.is_a?(Wait)
          wait
          next
        end

        case listing
        in FileListing
          return listing.content
        in DirListing
          raise Error.new("could not read: path is a directory: #{path}")
        in Absent
          raise Error.new("could not read: file absent: #{path}")
        end
      end
    end

    # Returns the content of the file at *path* as a `String`.
    #
    # Raises `Error` if the file cannot be read.
    def read_string(path : Path, **kwargs) : String
      blob = read(path, **kwargs)

      String.new(blob.bytes)
    end

    # Overwrites the content of the file at *path* with *content*.
    #
    # NOTE: This function may block for an indefinite amount of time, since it
    # waits for the proof that the file really was written to disk.
    def write(path : Path, content : Term::Blob) : Nil
      digest = Digest::SHA256.hexdigest(content)

      loop do
        converge(path, IsFile.new(content))

        view = view(path)

        case view
        in Wait, Absent
        in FileListing
          return if digest == view.digest
        in DirListing
          raise Error.new("path is a directory")
        end

        wait
      end
    end

    # :ditto:
    def write(path : Path, content : String, **kwargs)
      write(path, content.to_slice, **kwargs)
    end

    # Removes the file at *path*.
    #
    # Noop if the file does not exist.
    #
    # NOTE: This function may block for an indefinite amount of time, since it
    # waits for the proof that the file really was removed.
    def delete(path : Path) : Nil
      loop do
        converge(path.parent, FileAbsent.new(path.basename))

        view = view(path)
        case view
        in Wait, Listing
          wait
        in Absent
          return # ok
        end
      end
    end
  end
end
