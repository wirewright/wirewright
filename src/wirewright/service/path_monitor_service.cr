module Ww
  # A global path monitoring service.
  #
  # Currently, this is a pretty thin wrapper for `Inotify`, with polling to wait
  # for missing file system entries. In the future, we'd want to have an alternative
  # (polling?) backend so that other systems can be supported besides Linux.
  module PathMonitorService
    extend self

    Log = ::Log.for(self)

    # :nodoc:
    alias Msg = PathAdded | PathRemoved | Heartbeat | Inotify::Event

    # :nodoc:
    defrecord PathAdded, path : NormalPath
    # :nodoc:
    defrecord PathRemoved, path : NormalPath
    # :nodoc:
    defrecord Heartbeat, wg : WaitGroup

    # Notifications sent by the service.
    alias Notification = EntryCreated | EntryChanged | EntryRemoved | FileModified | FileCommitted

    # The file system entry at *path* was created.
    defrecord EntryCreated, path : NormalPath

    # The file system entry at *path* was changed (e.g. timestamp, permissions).
    defrecord EntryChanged, path : NormalPath

    # The file system entry at *path* was removed.
    defrecord EntryRemoved, path : NormalPath

    # The file at *path* was modified. It is possible that the file is
    # being modified at the moment.
    #
    # See also: `FileCommitted`.
    defrecord FileModified, path : NormalPath

    # The file at *path* was closed after writing.
    defrecord FileCommitted, path : NormalPath

    @@lock = Sync::Mutex.new
    @@msgs = BlockingQueue(Msg).new
    @@watchtab = {} of NormalPath => Int32
    @@running = false

    # WARNING: Assumes `@@lock` is taken.
    #
    # TODO: We should probably require explicit initialization at the start of
    # the program, but I'm not sure.
    private def ensure_running!
      return if @@running

      @@running = true

      ctx = Inotify.context

      spawn(name: "PathMonitor message loop") do
        msgloop = Msgloop.new(ctx)

        loop do
          msg = @@msgs.shift
          msgloop.receive(msg)
        end
      end

      spawn(name: "PathMonitor inotify relay") do
        Inotify.each(ctx) do |event|
          @@msgs << event
        end
      end

      spawn(name: "PathMonitor heartbeat loop") do
        loop do
          sleep 1.second

          wg = WaitGroup.new(1)
          @@msgs << Heartbeat.new(wg)
          wg.wait
        end
      end
    end

    # :nodoc:
    class Msgloop
      def initialize(@ctx : Inotify::Context)
        @watching = Bimap(NormalPath, Inotify::WatchRef).new
        @polling = Set(NormalPath).new
      end

      def receive(msg : Msg) : Nil
        Log.trace { msg }

        handle(msg)
      end

      private def handle(msg : PathAdded) : Nil
        watch(msg.path)
      end

      private def handle(msg : PathRemoved) : Nil
        unwatch(msg.path)
      end

      private def handle(msg : Heartbeat) : Nil
        polling = @polling
        @polling = Set(NormalPath).new

        polling.each do |path|
          # This will either transfer path to @watching, or back to @polling.
          watch(path)
          next unless path.in?(@watching)

          PathMonitorService.broadcast(EntryCreated.new(path))
        end
      ensure
        msg.wg.done
      end

      private def handle(msg : Inotify::Event) : Nil
        return unless path = @watching[msg.ref]?

        if msg.name.empty?
          member = path
        else
          member = NormalPath[path / msg.name]
        end

        if msg.mask.create? || msg.mask.moved_to?
          PathMonitorService.broadcast(EntryCreated.new(member))
        end

        if msg.mask.attrib?
          PathMonitorService.broadcast(EntryChanged.new(member))
        end

        if msg.mask.delete? || msg.mask.moved_from?
          PathMonitorService.broadcast(EntryRemoved.new(member))
        end

        if msg.mask.modify?
          PathMonitorService.broadcast(FileModified.new(member))
        end

        if msg.mask.close_write?
          PathMonitorService.broadcast(FileCommitted.new(member))
        end

        if msg.mask.delete_self? || msg.mask.move_self?
          @watching.delete(path)

          # This will either transfer path back to @watching, or go to @polling.
          watch(path)

          PathMonitorService.broadcast(EntryCreated.new(path))
        end
      end

      WATCH_MASK = LibInotify::Mask::Attrib |
                   LibInotify::Mask::CloseWrite |
                   LibInotify::Mask::MovedFrom |
                   LibInotify::Mask::MovedTo |
                   LibInotify::Mask::Create |
                   LibInotify::Mask::Modify |
                   LibInotify::Mask::Delete |
                   LibInotify::Mask::MoveSelf |
                   LibInotify::Mask::DeleteSelf

      private def watch(path : NormalPath) : Nil
        return if path.in?(@watching) || path.in?(@polling)

        begin
          ref = Inotify.watch(@ctx, path.unwrap, mask: WATCH_MASK)
          @watching[path] = ref
          Log.trace { "watching #{path} #{ref}" }
        rescue e : Inotify::Error
          @polling << path
          Log.trace(exception: e) { "polling #{path}" }
        end
      end

      private def unwatch(path : NormalPath) : Nil
        if ref = @watching.delete(path)
          Log.trace { "stop watching #{path} #{ref}" }
          Inotify.unwatch(@ctx, ref)
          return
        end

        if @polling.delete(path)
          Log.trace { "stop polling #{path}" }
          return
        end

        Log.debug { "attempt to unwatch #{path}, which is not watched" }
      end
    end

    # Creates a watch for *path* if one does not exist. Increments its
    # reference count.
    def add(path : NormalPath) : Nil
      @@lock.synchronize do
        ensure_running!

        unless refcount = @@watchtab[path]?
          @@watchtab[path] = 1
          @@msgs << PathAdded.new(path)
          return
        end

        @@watchtab[path] = refcount + 1
      end
    end

    # Decrements the reference count for the watch associated with *path*. Removes
    # the watch when its reference count reaches zero.
    def delete(path : NormalPath) : Nil
      @@lock.synchronize do
        ensure_running!

        return unless refcount = @@watchtab[path]?

        if refcount == 1
          @@watchtab.delete(path)
          @@msgs << PathRemoved.new(path)
          return
        end

        @@watchtab[path] = refcount - 1
      end
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
  end
end
