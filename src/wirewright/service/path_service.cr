module Ww
  # A poll-oriented, in-memory, global file system access service.
  #
  # `PathService` often acts as a reactive file system cache. I.e., when you
  # ask for a `report`, it performs some IO asynchronously and puts the result
  # in a cache. Further calls for the same report will hit the cache until
  # the report is evicted.
  #
  # *Reactivity* comes from the fact that `PathService` taps into the stream
  # of notifications produced by `PathMonitorService`. Notably, however, it
  # *does not manage watches for you*. In other words, it *passively* listens
  # to the notifications broadcast by `PathService`, using them to invalidate
  # its cache. From the outside, looks like reactivity.
  #
  # That is, if you happen to add a watch using `PathMonitorService#add`,
  # `PathService` will benefit from that watch; otherwise, the cached report
  # will remain stale until it is evicted.
  #
  # ```
  # # You are recommended to watch the parent directory of a file for more
  # # stability on atomic writes.
  # PathMonitorService.add(NormalPath["/tmp"])
  #
  # loop do
  #   pp PathService.read(NormalPath["/tmp/a"]).wait
  #   sleep 1.second
  # end
  #
  # # Prints the *up-to-date* content of /tmp/a every second.
  # ```
  #
  # The sleep is, of course, not necessary (in that case, however, you must
  # use a multi-threaded context!):
  #
  # ```
  # # ...
  #
  # loop do
  #   pp PathService.read(NormalPath["/tmp/a"]).wait
  # end
  #
  # # Prints the *up-to-date* content of /tmp/a really quickly. The runtime of
  # # read().wait above is expected to be sub-microsecond.
  # ```
  #
  # You can also manually invalidate the `PathService` cache using `invalidate`.
  #
  # ```
  # loop do
  #   PathService.invalidate(NormalPath["/tmp/a"])
  #   pp PathService.read(NormalPath["/tmp/a"]).wait
  #   sleep 1.second
  # end
  #
  # # Prints the *up-to-date* content of /tmp/a every second.
  # ```
  #
  # `PathService` is focused on *reads*: see `report`, `read`. The only way
  # to *write* is to write to a file (see `write`). This uses the standard atomic
  # write sequence: write to tmp, fsync, rename tmp to dst.
  #
  # The focus on reads is because writes are insanely racey in most file systems /
  # OSes. We expect to use something like SQLite to have sane persistence in
  # the future. Right now, just being able to write files is sufficient.
  #
  # `PathService` executes all IO operations on its message loop fiber (writes) or on
  # dedicated worker fibers (reads).
  #
  # `report` and `read` provide a cheap way to poll the file system. They can be
  # called millions of times per second on modern hardware. All IO work happens
  # in the message loop or on dedicated fibers, at its own pace, which is in turn
  # dictated by the disk and so on.
  #
  # Note that we currently do not do anything smart to lighten the IO/file system
  # work. In fact, we are pretty dumb here, preferring full scans over something
  # more "surgical". Whether or not this needs improvement is a question I am
  # not yet ready to answer.
  #
  # Instead of polling (e.g., every second, as in the example above), you can listen
  # for notifications from the service. See `listen`. You are recommended to use `listen`
  # for watching files (unless you have reasons to prefer `PathMonitorService#wait` and
  # its overloads).
  #
  # `listen` is recommended, in particular, because it is guaranteed to fire *after*
  # the invalidation of `PathService`'s cache. Both updates to the cache and removals
  # from it count as invalidations.
  #
  # This means you are guaranteed to receive the up-to-date reading/listing of your path
  # after `listen` tells you to. On the other hand, if you trigger re-reads based on some
  # other signal, `PathService` may not wake up quickly enough to invalidate before you
  # call `read` or `listing` on your own call. Thus, you'll receive an outdated (cached)
  # version and go to sleep again, ignoring the invalidation that PathService schedules
  # immediately after. You should design with this in mind; for example, by using
  # notifications sent by `listen` as an additional trigger for wakeups.
  module PathService
    extend self

    Log = ::Log.for(self)

    # The number of cache slots allocated for the `report` cache.
    REPORT_CACHE_CAPACITY = 256

    # The number of cache slots allocated for the `read` cache.
    READING_CACHE_CAPACITY = 256

    # Memory use (in bytes) to tolerate for the `read` cache (approximate).
    # See `ThresholdLRU` for more info on how `READING_CACHE_CAPACITY` and
    # `READING_CACHE_THRESHOLD_BYTES` interact.
    READING_CACHE_THRESHOLD_BYTES = 512u64 * 1024 * 1024 # 512 MiB

    # Files beyond this bytesize will be read as `DigestReading` instead of
    # `ContentReading` to make sure we do not run out of memory. It is still
    # possible to run out of memory, of course, in a kind of "death by a
    # thousand cuts". We mitigate the latter somewhat by also using `ThresholdLRU`
    # (see also: `READING_CACHE_CAPACITY`, `READING_CACHE_THRESHOLD_BYTES`).
    MAX_CONTENT_BYTESIZE = 64u64 * 1024 * 1024 # 64 MiB

    alias Report = Listing | Absent

    alias Listing = DirListing | FileListing

    defrecord DirListing, timestamp : Time, entries : Slice(DirListingEntry)
    defrecord FileListing, timestamp : Time, bytesize : Int64

    alias DirListingEntry = FileEntry | DirEntry

    defrecord FileEntry, path : NormalPath
    defrecord DirEntry, path : NormalPath

    alias WriteResult = Present | Absent

    defrecord Present
    defrecord Absent, detail : String

    alias Reading = ContentReading | DigestReading | Absent

    defrecord ContentReading, blob : Term::Blob
    defrecord DigestReading, digest : Bytes, bytesize : Int64

    alias Notification = ReportInvalid | ReadingInvalid | ReportReady | ReadingReady

    # Signals that the `Report` for *path* was invalidated.
    defrecord ReportInvalid, path : NormalPath

    # Signals that the `Reading` for *path* was invalidated.
    defrecord ReadingInvalid, path : NormalPath

    # Signals that a `Report` for *path* is ready; *path*'s corresponding promise was
    # fulfilled and discarded.
    defrecord ReportReady, path : NormalPath

    # Signals that a `Reading` for *path* is ready; *path*'s corresponding promise was
    # fulfilled and discarded.
    defrecord ReadingReady, path : NormalPath

    alias Msg = ReportWanted | ReadingWanted | Write | PathMonitorService::Notification

    defrecord ReportWanted, path : NormalPath
    defrecord ReadingWanted, path : NormalPath
    defrecord Write, path : NormalPath, content : Term::Blob, result : Sync::Future(WriteResult)

    @@msgs = BlockingQueue(Msg).new
    @@running = Atomic(Bool).new(false)

    private def ensure_running! : Nil
      return if @@running.swap(true)

      spawn(name: "PathService message loop") do
        msgloop = Msgloop.new
        loop do
          msg = @@msgs.shift
          msgloop.receive(msg)
        end
      end

      spawn(name: "PathService monitor relay") do
        PathMonitorService.listen do |notification|
          @@msgs << notification
        end
      end
    end

    private class Msgloop
      def initialize
        @rng = Random::PCG32.new
      end

      def receive(msg : Msg)
        Log.trace { msg }

        handle(msg)
      end

      private def handle(msg : ReportWanted) : Nil
        spawn(name: "report() fiber for #{msg.path}") do
          report = Msgloop.report(msg.path)
          PathService.broadcast(msg.path, report, as: Report)
        end
      end

      private def handle(msg : ReadingWanted) : Nil
        spawn(name: "read() fiber for #{msg.path}") do
          reading = Msgloop.read(msg.path)
          PathService.broadcast(msg.path, reading, as: Reading)
        end
      end

      private def handle(msg : Write) : Nil
        msg.result.set(write(msg.path, msg.content))
      end

      private def handle(msg : PathMonitorService::Notification) : Nil
        PathService.invalidate(msg.path, Report)
        if msg.is_a?(PathMonitorService::FileCommitted)
          PathService.invalidate(msg.path, Reading)
        end
      end

      def self.report(path : NormalPath) : Report
        Log.debug { "open() #{path}" }

        begin
          fd, blocking = Crystal::System::File.open(path.to_s, mode: "r", perm: File::DEFAULT_CREATE_PERMISSIONS, blocking: nil)
        rescue File::NotFoundError
          return Absent.new("path does not exist")
        end

        descriptor = IO::FileDescriptor.new(handle: fd)
        begin
          report(path, descriptor)
        ensure
          descriptor.close
        end
      end

      private def self.report(path : NormalPath, descriptor : IO::FileDescriptor)
        info = descriptor.info

        if info.file?
          report_file(path, descriptor, info)
        elsif info.directory?
          report_dir(path, descriptor, info)
        else
          Absent.new("unrecognized file system entry at path")
        end
      end

      private def self.report_file(path : NormalPath, descriptor : IO::FileDescriptor, info : File::Info)
        assert info.file?

        FileListing.new(info.modification_time, info.size)
      end

      private def self.report_dir(path : NormalPath, descriptor : IO::FileDescriptor, info : File::Info)
        assert info.directory?

        Log.debug { "opendir() #{path}" }

        unless dir = LibC.fdopendir(descriptor.fd)
          raise File::Error.from_errno("Error opening directory", file: path.unwrap)
        end

        entries = Pf::Kit.stack_array(DirListingEntry)

        while entry = Crystal::System::Dir.next_entry(dir, path.unwrap)
          next if entry.name.in?(".", "..")

          is_dir = entry.dir?
          next if is_dir.nil? # unknown

          if is_dir
            entries << DirEntry.new(NormalPath[path / entry.name])
          else
            entries << FileEntry.new(NormalPath[path / entry.name])
          end
        end

        entries.sort_by!(&.path)

        DirListing.new(info.modification_time, entries.to_unsafe_readonly_slice!)
      end

      def self.read(path : NormalPath) : Reading
        Log.trace { "read(#{path})" }

        File.open(path.unwrap, mode: "rb") do |file|
          info = file.info

          if info.size < MAX_CONTENT_BYTESIZE
            read_small(path, file, info)
          else
            read_large(path, file, info)
          end
        end
      rescue e : IO::Error
        Absent.new(e.message || "input/output error")
      end

      private def self.read_small(path, file, info)
        Log.debug { "read_small(#{path}) #{info.size.humanize_bytes}" }

        blob = Term::Blob.build do |blob|
          IO.copy(src: file, dst: blob, limit: info.size)
        end

        ContentReading.new(blob)
      end

      private def self.read_large(path, file, info)
        Log.debug { "read_large(#{path}) #{info.size.humanize_bytes}" }

        digest = IO::Digest.new(file, Term::Blob::DIGEST_ALGORITHM.new)
        IO.copy(src: file, dst: digest, limit: info.size)

        DigestReading.new(digest.final, info.size)
      end

      private def write(path : NormalPath, blob : Term::Blob) : WriteResult
        tmp_file = File.tempfile(@rng, tempdir: path.parent.unwrap)
        tmp_path = tmp_file.path

        begin
          tmp_file.write(blob.bytes)
          tmp_file.fsync
        ensure
          tmp_file.close
        end

        File.rename(tmp_path, path.unwrap)

        Present.new
      rescue e : IO::Error
        Absent.new(e.message || "input/output error")
      end
    end

    @@report_lock = Sync::Mutex.new
    @@report_workspace = {} of NormalPath => Sync::Future(Report)
    @@report_cache = LRU(NormalPath, Report).new(REPORT_CACHE_CAPACITY)

    @@read_lock = Sync::Mutex.new
    @@read_workspace = {} of NormalPath => Sync::Future(Reading)
    @@read_cache = ReadingCache.new

    private class ReadingCache
      defcase ReadingRef, reading : Reading do
        # NOTE: Approximate
        def bytesize : UInt64
          case tmp = reading
          in ContentReading then tmp.blob.ubytesize64
          in DigestReading  then tmp.digest.size.to_u64
          in Absent         then sizeof(Absent).to_u64
          end
        end
      end

      def initialize
        @lru = ThresholdLRU(NormalPath, ReadingRef).new(
          READING_CACHE_CAPACITY,
          READING_CACHE_THRESHOLD_BYTES,
        )
      end

      def get?(path : NormalPath) : Reading?
        return unless reading_ref = @lru.get?(path)

        reading_ref.reading
      end

      def put(path : NormalPath, reading : Reading) : Reading
        @lru.put(path, ReadingRef.new(reading))

        reading
      end

      def delete(path : NormalPath) : Reading?
        return unless reading_ref = @lru.delete(path)

        reading_ref.reading
      end
    end

    # :nodoc:
    def broadcast(path : NormalPath, report : Report, *, as cls : Report.class) : Nil
      result = @@report_lock.synchronize do
        @@report_cache.put(path, report)
        @@report_workspace[path]?
      end

      unless result
        Log.debug { "Report promise is missing (?!)" }
        return
      end

      result.set(report)

      @@report_lock.synchronize do
        @@report_workspace.delete(path)
      end

      broadcast(ReportReady.new(path))
    end

    # :nodoc:
    def broadcast(path : NormalPath, reading : Reading, *, as cls : Reading.class) : Nil
      result = @@read_lock.synchronize do
        @@read_cache.put(path, reading)
        @@read_workspace[path]?
      end

      unless result
        Log.debug { "Reading promise is missing (?!)" }
        return
      end

      result.set(reading)

      @@read_lock.synchronize do
        @@read_workspace.delete(path)
      end

      broadcast(ReadingReady.new(path))
    end

    # :nodoc:
    def invalidate(path : NormalPath, cls : Report.class) : Nil
      @@report_lock.synchronize do
        @@report_cache.delete(path)
      end

      broadcast(ReportInvalid.new(path))
    end

    # :nodoc:
    def invalidate(path : NormalPath, cls : Reading.class) : Nil
      @@read_lock.synchronize do
        @@read_cache.delete(path)
      end

      broadcast(ReadingInvalid.new(path))
    end

    # Returns the report for *path*. If not cached, produces the report on
    # a separate fiber, and caches it.
    #
    # `Report` cache is invalidated automatically by tapping into the stream
    # of notifications emitted by `PathMonitorService`. This assumes you or someone
    # else is monitoring *path* already (see `PathMonitorService.add`). You can
    # also invalidate the cache manually, see `invalidate`.
    #
    # You can generate multiple reports simultaneosuly simply by scheduling them
    # before waiting on them:
    #
    # ```
    # PathService.write(NormalPath["/tmp/a"], Term::Blob.new("John Doe")).wait
    # PathService.write(NormalPath["/tmp/b"], Term::Blob.new("Samantha Doe")).wait
    #
    # promises = [
    #   PathService.report(NormalPath["/tmp/a"]),
    #   PathService.report(NormalPath["/tmp/b"]),
    # ]
    #
    # readings = promises.map(&.wait.unwrap)
    # pp! readings # => [FileListing(@bytesize=8, ...), FileListing(@bytesize=12, ...)]
    # ```
    def report(path : NormalPath) : Promise(Report)
      ensure_running!

      @@report_lock.synchronize do
        if report = @@report_cache.get?(path)
          return Promise(Report).resolved(report)
        end

        unless result = @@report_workspace[path]?
          @@report_workspace[path] = result = Sync::Future(Report).new
          @@msgs << ReportWanted.new(path)
        end

        Promise.new(result)
      end
    end

    # Returns the listing for *path*. Absence is rejected.
    #
    # See also: `report`.
    def listing(path : NormalPath) : Promise(Listing)
      report(path).map do |report|
        case report
        in Listing then Promise(Listing).accepted(report)
        in Absent  then Promise(Listing).rejected(report.detail)
        end
      end
    end

    # Returns the reading for *path*. If not cached, produces the reading on
    # a separate fiber, and caches it.
    #
    # `Reading` cache is invalidated automatically by tapping into the stream
    # of notifications emitted by `PathMonitorService`. This assumes you or someone
    # else is monitoring *path* already (see `PathMonitorService.add`). You can
    # also invalidate the cache manually, see `invalidate`.
    #
    # You can generate multiple readings simultaneosuly simply by scheduling them
    # before waiting on them:
    #
    # ```
    # PathService.write(NormalPath["/tmp/a"], Term::Blob.new("John Doe")).wait
    # PathService.write(NormalPath["/tmp/b"], Term::Blob.new("Samantha Doe")).wait
    #
    # promises = [
    #   PathService.read_blob(NormalPath["/tmp/a"]),
    #   PathService.read_blob(NormalPath["/tmp/b"]),
    # ]
    #
    # readings = promises.map(&.wait.unwrap.to_string)
    # pp! readings # => ["John Doe", "Samantha Doe"]
    # ```
    def read(path : NormalPath) : Promise(Reading)
      ensure_running!

      @@read_lock.synchronize do
        if reading = @@read_cache.get?(path)
          return Promise(Reading).resolved(reading)
        end

        unless result = @@read_workspace[path]?
          @@read_workspace[path] = result = Sync::Future(Reading).new
          @@msgs << ReadingWanted.new(path)
        end

        Promise.new(result)
      end
    end

    # Returns the blob at *path*, assuming there is a file there and its content
    # is not too large (see `MAX_CONTENT_BYTESIZE`). Rejects otherwise.
    #
    # See also: `read`.
    def read_blob(path : NormalPath) : Promise(Term::Blob)
      read(path).map do |reading|
        case reading
        in ContentReading
          Promise(Term::Blob).accepted(reading.blob)
        in DigestReading
          Promise.rejected("file too large to load into memory (#{reading.bytesize.humanize_bytes})")
        in Absent
          Promise.rejected(reading.detail)
        end
      end
    end

    class Error < Exception
    end

    # Same as `read_blob`, but converts the resulting blob to a `String`
    # for convenience. Raises `Error` in case of an error.
    def read_string(path : NormalPath) : String
      result = read_blob(path).wait
      if result.is_a?(Promise::Rejected)
        raise Error.new(result.detail)
      end

      # result : Promise::Accepted
      blob = result.object
      blob.to_string
    end

    # Invalidates readings and reports for *path*, so that calling `read` and
    # `report` on it will result in a cache miss.
    def invalidate(path : NormalPath) : Nil
      invalidate(path, Report)
      invalidate(path, Reading)
    end

    include ServiceBroadcast(Notification)

    # Blocks the calling fiber until a notification mentions any path from
    # the given set of *paths*.
    #
    # *args* are forwarded to `listen`.
    def wait(paths : Set(NormalPath), *args) : Nil
      listen(*args) do |notification|
        next unless notification.path.in?(paths)
        break
      end
    end

    # Blocks the calling fiber until a notification whose class is in *mask*
    # mentions any path from the given set of *paths*.
    #
    # *args* are forwarded to `listen`.
    def wait(paths : Set(NormalPath), mask : Enumerable(Notification.class), *args) : Nil
      listen(*args) do |notification|
        next unless notification.class.in?(mask)
        next unless notification.path.in?(paths)
        break
      end
    end

    # Performs an atomic write of *blob* at *path* on the service's message loop
    # fiber. Returns a success status: by then, the file is either `Present`, or
    # `Absent` (due to an error of some sort).
    #
    # NOTE: Unlike `read` and `report`, this function is *not* poll-friendly.
    # It does not "cushion" your calls in any way. If you call it a million times,
    # it will schedule a million writes, and the message loop will faithfully
    # execute each one of them.
    def write(path : NormalPath, blob : Term::Blob) : Promise(WriteResult)
      ensure_running!

      result = Sync::Future(WriteResult).new
      @@msgs << Write.new(path, blob, result)

      Promise.new(result)
    end
  end
end
