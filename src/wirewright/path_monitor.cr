# Low-level bindings to inotify.
lib LibInotify
  @[Flags]
  enum Mask : UInt32
    # File was accessed.
    Access = 0x00000001
    # File was modified.
    Modify = 0x00000002
    # Metadata changed.
    Attrib = 0x00000004
    # Writtable file was closed.
    CloseWrite = 0x00000008
    # Unwrittable file closed.
    CloseNoWrite = 0x00000010
    # File was opened.
    Open = 0x00000020
    # File was moved from X.
    MovedFrom = 0x00000040
    # File was moved to Y.
    MovedTo = 0x00000080
    # Subfile was created.
    Create = 0x00000100
    # Subfile was deleted.
    Delete = 0x00000200
    # Self was deleted.
    DeleteSelf = 0x00000400
    # Self was moved.
    MoveSelf = 0x00000800

    # Events sent by the kernel.

    # Backing fs was unmounted.
    Unmount = 0x00002000
    # Event queued overflowed.
    Overflow = 0x00004000
    # File was ignored.
    Ignored = 0x00008000

    # Special flags.

    # Only watch the path if it is a directory.
    OnlyDir = 0x01000000
    # Do not follow a sym link.
    DontFollow = 0x02000000
    # Exclude events on unlinked objects.
    ExclUnlink = 0x04000000
    # Add to the mask of an already existing watch.
    MaskAdd = 0x20000000
    # Event occurred against dir.
    IsDir = 0x40000000
    # Only send event once.
    OneShot = 0x80000000
  end

  ALL_EVENTS = Mask::Access \
    | Mask::Modify \
    | Mask::Attrib \
    | Mask::CloseWrite \
    | Mask::CloseNoWrite \
    | Mask::Open \
    | Mask::MovedFrom \
    | Mask::MovedTo \
    | Mask::Create \
    | Mask::Delete \
    | Mask::DeleteSelf \
    | Mask::MoveSelf

  struct Event
    wd : LibC::Int
    mask : Mask
    cookie : UInt32
    len : UInt32
  end

  fun init = inotify_init : LibC::Int
  fun add_watch = inotify_add_watch(fd : LibC::Int, name : LibC::Char*, mask : Mask) : LibC::Int
  fun rm_watch = inotify_rm_watch(fd : LibC::Int, wd : LibC::Int) : LibC::Int
end

# Low-level wrapper for inotify.
module Inotify
  extend self

  # :nodoc:
  defrecord Context, io : IO::FileDescriptor

  # Represents an inotify watch descriptor.
  defrecord WatchRef, wd : Int32

  # Represents an inotify event.
  defrecord Event,
    ref : WatchRef,
    cookie : UInt32,
    mask : LibInotify::Mask,
    name : String

  class Error < RuntimeError
  end

  # Constructs an inotify context.
  def context : Context
    fd = LibInotify.init
    if fd == -1
      raise Error.from_errno("inotify")
    end

    io = IO::FileDescriptor.new(fd)
    IO::FileDescriptor.set_blocking(fd, false)

    Context.new(io)
  end

  # Tears down an inotify context *ctx*.
  def close(ctx : Context) : Nil
    ctx.io.close
  end

  # Yields an inotify context to the block. Tears it down after the block ends.
  def context(&)
    ctx = context

    begin
      yield ctx
    ensure
      close(ctx)
    end
  end

  # Inotify buffer size.
  BUFFER_SIZE = 4096

  # Yields inotify events. Blocks the current fiber while waiting for events.
  # Terminates when the context is closed (see `close`).
  def each(ctx : Context, & : Event ->) : Nil
    buffer = uninitialized UInt8[BUFFER_SIZE]
    slice = buffer.to_slice

    loop do
      # EAGAIN is handled by Crystal.
      #
      # See e.g.: https://github.com/crystal-lang/crystal/blob/15685dcf7c157662638b8e66ed8d365456cb165d/src/crystal/event_loop/polling.cr#L382
      begin
        size = ctx.io.read(slice)
      rescue e : IO::Error
        Log.debug(exception: e) { "each: no longer reading" }
        break
      end

      break if size.zero?

      data = slice[0...size]
      until data.empty?
        # Read event.
        eventptr = data.to_unsafe.as(LibInotify::Event*)
        event = eventptr.value
        data = data[sizeof(LibInotify::Event)..]

        # Read name.
        name = String.new(data[0...event.len.to_i], truncate_at_null: true)
        data = data[event.len.to_i..]

        yield Event.new(WatchRef.new(event.wd), event.cookie, event.mask, name)
      end
    end
  end

  # Starts watching *path*.
  def watch(ctx : Context, path : Path | String, mask : LibInotify::Mask) : WatchRef
    wd = LibInotify.add_watch(ctx.io.fd, path.to_s, mask)
    if wd == -1
      raise Error.from_errno("inotify")
    end

    WatchRef.new(wd)
  end

  # Stops watching *path*.
  def unwatch(ctx : Context, ref : WatchRef) : Nil
    wd = LibInotify.rm_watch(ctx.io.fd, ref.wd)
    if wd == -1
      raise Error.from_errno("inotify")
    end
  end
end

module Ww
  # A simple polling API for monitoring paths.
  #
  # We use inotify under the hood to provide the best precision, but fall back
  # to polling in cases where inotify doesn't work.
  #
  # The core goal is to make `status` extremely cheap so that callers can call
  # it millions of times per second (assuming single-threaded or uncontended usage;
  # due to global synchronization, the higher the contention, the worse it's
  # going to run, but it should still be in the microsceond range in worst cases).
  #
  # ```
  # pp PathMonitor.status(Path["/tmp/a"]) # => PathMonitor::Wait()
  #
  # # You can use wait to wait for the next status change if you don't want
  # # to poll; but it's as coarse as it gets. We expect event or rewrite
  # # loops to call wait() on fixpoint and resume polling for status() afterwards.
  # PathMonitor.wait
  #
  # pp PathMonitor.status(Path["/tmp/a"]) # => PathMonitor::Absent()
  #
  # spawn do
  #   sleep 3.seconds
  #
  #   File.write(Path["/tmp/a"], "Hello World")
  #
  #   sleep 3.seconds
  #
  #   File.write(Path["/tmp/a"], "Bye World")
  # end
  #
  # PathMonitor.wait
  #
  # pp PathMonitor.status(Path["/tmp/a"])
  # # => PathMonitor::Present(@version=0) [due to create with "Hello World"]
  #
  # PathMonitor.wait
  #
  # pp PathMonitor.status(Path["/tmp/a"])
  # # => PathMonitor::Present(@version=1) [due to update with "Bye World"]
  # ```
  #
  # Under the hood, subscriptions age and are garbage-collected when their age
  # exceeds some maximum. Before collecting them we mark them as garbage, to allow
  # waiting callers (if any) to resurrect paths they're still interested in.
  # An unaddressed garbage path is removed on the next turn of collection
  # unconditionally.
  module PathMonitor
    extend self

    Log = ::Log.for(self)

    alias Status = Absent | Present

    # Indicates that the path currently does not exist.
    defrecord Absent

    # Indicates that the path exists and its content's version is currently *version*.
    #
    # *version* is a monotonically increasing, process-unique id. Each process
    # starts with *version* `0`.
    defrecord Present, version : UInt64

    # :nodoc:
    defrecord Garbage, prev : Status

    @@running = Atomic(Bool).new(false)

    @@lock = Sync::Mutex.new
    @@clock = 0u64
    @@paths = Set(Path).new
    @@statuses = {} of Path => Status | Garbage
    @@ages = {} of Path => Int32
    @@finalizers = [] of (Array(Path) ->)

    @@wait_lock = Sync::Mutex.new
    @@wait_cv = Sync::ConditionVariable.new(@@wait_lock)

    private def ensure_server_running! : Nil
      return if @@running.swap(true)

      spawn(name: "PathMonitor message loop") do
        Inotify.context do |ictx|
          msgloop(ictx)
        end
      end
    end

    # :nodoc:
    INOTIFY_MASK = LibInotify::ALL_EVENTS ^
                   LibInotify::Mask::Access ^
                   LibInotify::Mask::Open ^
                   LibInotify::Mask::CloseWrite ^
                   LibInotify::Mask::CloseNoWrite

    # How much to wait between heartbeats.
    #
    # At heartbeat, we perform garbage collection and polling for changes. When
    # inotify is used, the latter is only relevant for absent paths.
    #
    # Note that in practice we may wait longer than that: if the message loop
    # is busy the heartbeat loop will stop and wait until the heartbeat it sent
    # is handled, which may take longer than what `HEARTBEAT` specifies.
    HEARTBEAT = 500.milliseconds

    # :nodoc:
    alias Msg = PathAdded | PathsRemoved | Heartbeat | Notification

    # :nodoc:
    defrecord PathAdded, path : Path
    # :nodoc:
    defrecord PathsRemoved, paths : Array(Path)
    # :nodoc:
    defrecord Heartbeat, ack : WaitGroup
    # :nodoc:
    defrecord Notification, event : Inotify::Event

    @@msgs = BlockingQueue(Msg).new

    # :nodoc:
    defcase MsgState,
      ictx : Inotify::Context,
      polling = Set(Path).new,
      polling_modified_at = {} of Path => Time,
      watching = Bimap(Path, Inotify::WatchRef).new,
      mutation: true

    # :nodoc:
    alias Command = SetAbsent | SetPresent | Prune

    # :nodoc:
    defrecord Prune
    # :nodoc:
    defrecord SetAbsent, path : Path
    # :nodoc:
    defrecord SetPresent, path : Path

    private def msgloop(ictx : Inotify::Context) : Nil
      spawn(name: "PathMonitor inotify read loop") { irloop(ictx) }
      spawn(name: "PathMonitor heartbeat") do
        Log.debug { "heartbeat: running" }

        loop do
          # Instead of spamming the message loop with Heartbeats (imagine it's very
          # busy right now so its work spans over multiple heartbeats), we wait until
          # it acknowledges.
          ack = WaitGroup.new(1)
          @@msgs << Heartbeat.new(ack)
          ack.wait

          sleep HEARTBEAT
        end
      end

      Log.debug { "msgloop: running" }

      state = MsgState.new(ictx)
      commands = [] of Command

      loop do
        msg = @@msgs.shift

        if msg.is_a?(Heartbeat)
          Log.trace { "msgloop: #{msg}" }
        else
          Log.debug { "msgloop: #{msg}" }
        end

        handle(state, msg) do |command|
          commands << command
        end

        next if commands.empty?

        @@lock.synchronize do
          any_notifies = false

          commands.each do |command|
            notify = execute?(command)
            any_notifies = true if notify
          end

          if any_notifies
            Log.trace { "msgloop: wait_cv broadcast" }

            @@wait_cv.broadcast
          end
        ensure
          commands.clear
        end
      end
    end

    private def irloop(ictx : Inotify::Context) : Nil
      Log.debug { "irloop: running" }

      Inotify.each(ictx) do |event|
        @@msgs << Notification.new(event)
      end
    end

    # NOTE: `handle` overloads can only access *state*. They don't access or modify @@vars
    # directly. Instead they emit Commands.

    private def handle(state : MsgState, msg : PathAdded, &sink : Command ->) : Nil
      return if msg.path.in?(state.watching) || msg.path.in?(state.polling)

      begin
        ref = Inotify.watch(state.ictx, msg.path, mask: INOTIFY_MASK)
      rescue e : Inotify::Error
        Log.debug { "inotify->poll: transfer #{msg.path}" }

        # The likely failure here is that the path is absent. It may be anything
        # else though. So generally, give it to the polling backend.
        state.polling << msg.path

        unless e.os_error.as?(Errno).try(&.enoent?)
          Log.debug(exception: e) { "inotify: non-ENOENT while creating watch" }
          return
        end

        # If it indeed was ENOENT, we also set the status to Absent for now and
        # notify everybody.
        sink.call(SetAbsent.new(msg.path))
      else
        Log.debug { "inotify: watching #{msg.path}" }

        # If adding an inotify watch succeeds, add to watch list and bind ref.
        #
        # If the watch is ignored immediately afterward, that'd be an inotify event,
        # Ignored, and it'll go through Notification and we'll remove everything
        # as expected when we handle it. So it seems no worries here wrt races
        # and the like.
        state.watching[msg.path] = ref

        # Also set the status. The file seems to exist at the moment so we mark
        # it as Present.
        sink.call(SetPresent.new(msg.path))
      end
    end

    private def handle(state : MsgState, msg : PathsRemoved, &sink : Command ->) : Nil
      msg.paths.each do |path|
        if state.polling.delete(path)
          Log.debug { "poll: removing #{path}" }

          state.polling_modified_at.delete(path)
          next
        end

        Log.debug { "inotify: removing #{path}" }

        ref = state.watching.delete!(path)

        begin
          Inotify.unwatch(state.ictx, ref)
        rescue e : Inotify::Error
          Log.error(exception: e) { "unexpected inotify error while removing watch" }
        end
      end
    end

    private def handle(state : MsgState, msg : Notification, &sink : Command ->) : Nil
      # Ignored means roughly that the watch handle was removed by the kernel,
      # possibly due to the file we're state.watching having been removed. Unfortunately
      # it also could mean we removed the watch ourselves. So we have to check whether
      # we're still handling the watch and if we are, transfer it to the polling monitor.
      if msg.event.mask.ignored?
        return unless path = state.watching.delete(msg.event.ref)

        # Retry immediately to reduce latency. Otherwise we'd have to wait for the first
        # heartbeat which could be a long time from now. Often IGNORED is generated by
        # an atomic rename() so the file is (more or less) immediately available afterwards.
        @@msgs.interject(PathAdded.new(path))
        return
      end

      path = state.watching[msg.event.ref]

      sink.call(SetPresent.new(path))
    end

    private def handle(state : MsgState, msg : Heartbeat, &sink : Command ->) : Nil
      sink.call(Prune.new)

      state.polling.select! do |path|
        begin
          ref = Inotify.watch(state.ictx, path, mask: INOTIFY_MASK)

          Log.debug { "poll->inotify: transfer #{path}" }

          state.watching[path] = ref
          sink.call(SetPresent.new(path))

          next false # Stop polling, we're watching it now
        rescue e : Inotify::Error
          if e.os_error.as?(Errno).try(&.enoent?)
            next true # Keep polling
          end

          Log.debug(exception: e) { "poll: inotify failed with non-ENOENT, trying stat" }
        end

        begin
          info = File.info(path)
        rescue e : File::Error
          Log.debug(exception: e) { "poll: error while stat()ting a file" }

          sink.call(SetAbsent.new(path))
          next true # keep polling
        end

        if state.polling_modified_at[path]? == info.modification_time
          next true # keep polling, it wasn't modified
        end

        Log.debug { "poll: #{path} changed (mtime)" }

        state.polling_modified_at[path] = info.modification_time
        sink.call(SetPresent.new(path))

        true # keep polling
      end

      msg.ack.done
    end

    # NOTE: execute? methods, on the other hand, are run while @@lock is held and
    # can read or modify @@vars. They can return `true` to send a notification to
    # to waiting fibers. This is usually done if @@statuses is changed, which is
    # really the only @@var waiters care about.

    private def execute?(command : SetPresent) : Bool
      status1 = Present.new(@@clock)

      if status0 = @@statuses[command.path]?
        if status0.is_a?(Garbage)
          status1 = Garbage.new(status1)
        end
      end

      @@statuses[command.path] = status1
      @@clock += 1

      true # notify
    end

    private def execute?(command : SetAbsent) : Bool
      status0 = @@statuses[command.path]?
      status1 = Absent.new
      if status0.is_a?(Garbage)
        status1 = Garbage.new(status1)
      end

      if status0 == status1
        return false # don't notify
      end

      @@statuses[command.path] = status1

      true # notify
    end

    # :nodoc:
    #
    # Maximum age of subscriptions, in heartbeats. Subscriptions past this age
    # "die" -- they are garbage collected. Accessing a subscription resets
    # its age.
    MAX_AGE_HBS = 8

    private def execute?(command : Prune) : Bool
      notify = false
      pruned = [] of Path

      @@ages.each do |path, age|
        unless age > MAX_AGE_HBS
          @@ages[path] = age + 1
          next
        end

        status = @@statuses[path]?
        if status.is_a?(Status)
          @@statuses[path] = Garbage.new(status)
          notify = true
          next
        end

        # status : Garbage | Nil

        pruned << path

        # Since this is garbage collection, we don't consider it a change when
        # we delete something from @@statuses. No waiter should care; otherwise
        # something is very wrong.
        @@paths.delete(path)
        @@statuses.delete(path)
      end

      if pruned.empty?
        return notify
      end

      pruned.each do |path|
        @@ages.delete(path)
      end

      @@finalizers.each &.call(pruned)
      @@msgs.interject(PathsRemoved.new(pruned))

      notify
    end

    # Indicates that the caller's request was acknowledged and is being processed
    # now. The caller should call later to get a `Status`.
    defrecord Wait

    # Returns the status of *path*. The returned status is a snapshot of the file
    # system at some unspecified point in time. The status is *eventually consistent*:
    # it may not reflect the instantaneous state of the file system.
    def status(path : Path) : Status | Wait
      ensure_server_running!

      path = path.normalize

      @@lock.synchronize do
        @@ages[path] = 0

        if status = @@statuses[path]?
          if status.is_a?(Garbage)
            # No it's not!
            @@statuses[path] = status.prev
            status = status.prev
          end

          return status
        end

        unless @@paths.add?(path)
          return Wait.new
        end

        @@msgs << PathAdded.new(path)

        Wait.new
      end
    end

    # Blocks the current fiber until the status of some path changes, or until
    # garbage collection marks some path as garbage, or spuriously.
    #
    # In all cases, callers should call `status` to learn about the change or
    # resurrect paths they care about until it's too late. We assume each caller
    # knows what they're waiting for.
    def wait : Nil
      @@wait_lock.synchronize { @@wait_cv.wait }
    end

    def paths_finalize(&fn : Array(Path) ->) : ->
      @@lock.synchronize { @@finalizers << fn }

      -> do
        @@lock.synchronize { @@finalizers.delete(fn) }
      end
    end
  end
end
