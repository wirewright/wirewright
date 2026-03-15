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

    alias Listing = DirListing | FileListing | LargeFileListing

    defrecord DirListing, timestamp : Time, entries : Array(DirListingEntry)
    defrecord FileListing, timestamp : Time, content : Term::Blob
    defrecord LargeFileListing, timestamp : Time, digest : Bytes, bytesize : Int64

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

      # Since PathMonitor is global we could get somebody else's paths here but then
      # they won't be present in r_demand or w_model so that's fine.
      PathMonitor.paths_finalize do |paths|
        Log.debug { "PathServer: finalize #{paths}" }

        @@r_lock.synchronize do
          @@r_demand = @@r_demand.reject(&.in?(paths))
        end

        @@w_model_lock.synchronize do
          paths.each do |path|
            @@w_model.delete(path)
          end
        end

        Log.debug { "PathServer: finalized" }
      end

      spawn(name: "PathServer read loop") { rloop }
      spawn(name: "PathServer write loop") { wloop }
      spawn(name: "PathServer monitor loop") do
        epoch = 0u64

        loop do
          epoch = PathMonitor.wait(epoch)

          @@r_world_changed.call
          @@r_waiters_signal.call
        end
      end
    end

    @@r_lock = Sync::Mutex.new
    @@r_submissions = Pf::Set(Path).new
    @@r_demand = Pf::Set(Path).new
    @@r_supply = Pf::Map(Path, Supply).new

    @@r_world_changed = BlockingSignal.new
    @@r_waiters_signal = BlockingSignal.new

    alias Supply = Present | Absent

    defrecord Present, listing : Listing, version : UInt64
    defrecord Absent

    # We refuse to read files past this bytesize. We assume a different subsystem exists
    # for reading such files. Since we load all files into memory and talk to the file system
    # in an unpredictable manner, we can't do this for arbitrary files; only for very
    # small files.
    SAFE_FILE_BYTESIZE = 32 * 1024 * 1024 # 32 MiB

    # Read loop
    private def rloop : Nil
      Log.debug { "rloop: running" }

      epoch = 0u64

      loop do
        epoch = @@r_world_changed.wait(epoch)

        Log.trace { "rloop: woke up" }

        # Move submissions to demands. Atomically read demands and supply.
        #
        # Note that we're the only ones writing to @@r_supply. Everybody else's access
        # is read-only. On the other hand for @@r_demand, we can prune it and we can
        # read from it, that's it. More discrete modifications are not ours.
        demand, supply0 = @@r_lock.synchronize do
          @@r_demand = @@r_demand.concat(@@r_submissions)
          @@r_submissions = Pf::Set(Path).new
          {@@r_demand, @@r_supply}
        end

        # Process demands.
        supply1 = Pf::Map(Path, Supply).transaction do |txn|
          demand.each do |path|
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
                  digest = File.open(path, "rb") do |src|
                    buffer = Bytes.new(8192)

                    Term::Blob::DIGEST_ALGORITHM.digest do |dst|
                      loop do
                        size = src.read(buffer)
                        break if size.zero?

                        dst.update(buffer.trim(size))
                      end
                    end
                  end

                  listing = LargeFileListing.new(info.modification_time, digest, info.size)
                else
                  content = File.open(path, "rb") do |src|
                    Term::Blob.build(classify: true) do |dst|
                      IO.copy(src, dst)
                    end
                  end

                  listing = FileListing.new(info.modification_time, content)
                end
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

        next if supply0 == supply1

        # Commit new supply atomically.
        @@r_lock.synchronize do
          @@r_supply = supply1
        end

        Log.trace { "rloop: wake up waiters (supply changed)" }

        @@r_waiters_signal.call
      end
    end

    @@w_supply_changed = BlockingSignal.new
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

      epoch = 0u64

      loop do
        epoch = @@w_supply_changed.wait(epoch)

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
      path = Ww.normalize(path)

      ensure_server_running!

      @@r_lock.synchronize do
        @@r_submissions = @@r_submissions.add(path)

        case state = @@r_supply[path]?
        in Nil
        in Absent  then return state
        in Present then return state.listing
        end

        @@r_world_changed.call

        Wait.new
      end
    end

    # Blocks the current fiber until the view of some path changes, or `PathMonitor` reports
    # a change, or spuriously.
    #
    # In all cases, callers should call `view` to learn about the change or
    # resurrect paths they care about until it's too late. We assume each caller
    # knows what they're waiting for.
    #
    # For more info (esp. on *epoch*), see `PathMonitor.wait`.
    def wait(epoch : UInt64) : UInt64
      @@r_waiters_signal.wait(epoch)
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
      path = Ww.normalize(path)

      ensure_server_running!

      @@w_supply_lock.synchronize do
        if facts0 = @@w_supply[path]?
          facts1 += facts0
        end

        @@w_supply = @@w_supply.assoc(path, facts1)
        @@w_supply_changed.call
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
      epoch = 0u64

      loop do
        listing = view(path)
        if listing.is_a?(Wait)
          epoch = wait(epoch)
          next
        end

        case listing
        in FileListing
          return listing.content
        in DirListing
          raise Error.new("could not read: path is a directory: #{path}")
        in LargeFileListing
          raise Error.new("file exceeds safe file size of #{SAFE_FILE_BYTESIZE.humanize_bytes}")
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
      epoch = 0u64

      loop do
        converge(path, IsFile.new(content))

        view = view(path)

        case view
        in Wait, Absent
        in FileListing
          return if content == view.content
        in DirListing
          raise Error.new("path is a directory")
        in LargeFileListing
          return if content.digest == view.digest
        end

        epoch = wait(epoch)
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
        in Wait, Listing, TooLarge
          wait
        in Absent
          return # ok
        end
      end
    end

    enum Presentation
      # Present as a string if UTF-8, otherwise as a blob.
      Auto
      # Present as a blob.
      Binary
      # Present as a string even if non-UTF-8.
      Text

      def present(blob : Term::Blob)
        if text? || (auto? && blob.classif.utf8?)
          return Term.of(String.new(blob.bytes))
        end

        blob
      end
    end

    # Converts *listing* to a term.
    def render(listing : Listing, *, presentation : Presentation = :binary) : Term
      render(listing, presentation)
    end

    # :nodoc:
    def render(listing : FileListing, presentation : Presentation) : Term
      Term.of(:file, presentation.present(listing.content), timestamp: listing.timestamp.to_s)
    end

    # :nodoc:
    def render(listing : LargeFileListing, presentation : Presentation) : Term
      Term.of(:file, timestamp: listing.timestamp.to_s, digest: listing.digest)
    end

    # :nodoc:
    def render(listing : DirListing, presentation : Presentation) : Term
      dict = Term::Dict.build do |commit|
        commit << :dir
        commit.with(:timestamp, listing.timestamp.to_s)
        commit.concat(listing.entries) { |entry| render(entry) }
      end

      Term.of(dict)
    end

    private def render(entry : FileEntry)
      Term.of(:file, entry.path, timestamp: entry.timestamp.to_s)
    end

    private def render(entry : DirEntry)
      Term.of(:dir, entry.path, timestamp: entry.timestamp.to_s)
    end

    # Parses a term *spec* into a set of facts. If *spec* itself is not recognized,
    # returns an empty set. If one of entries is not recognized, it is ignored.
    def parse(spec : Term) : Pf::Set(Fact)
      Pf::Set(Fact).transaction do |txn|
        Term.case(spec) do
          matchpi %{[file content_string]}, content: String do
            txn.add(IsFile.new(Term::Blob.new(content.to_slice)))
          end

          matchpiT %{[file content_blob]} do
            txn.add(IsFile.new(content))
          end

          matchpi %{[dir entries_*]} do
            txn.add(IsDir.new)

            entries.items.each do |entry|
              Term.case(entry) do
                matchpi %{[file name_string]}, name: String do
                  txn.add(FilePresent.new(name))
                end

                matchpi %{[dir name_string]}, name: String do
                  txn.add(DirPresent.new(name))
                end

                matchpi %{[-file name_string]}, name: String do
                  txn.add(FileAbsent.new(name))
                end

                matchpi %{[-dir name_string]}, name: String do
                  txn.add(DirAbsent.new(name))
                end

                otherwise { }
              end
            end
          end

          otherwise { }
        end
      end
    end
  end
end
