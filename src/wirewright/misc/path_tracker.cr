# TODO: write our own inotify wrapper, this is horrendeous
class Inotify::BetterWatcher < Inotify::Watcher
  def initialize(@recursive : Bool = false)
    fd = LibInotify.init LibC::O_NONBLOCK
    raise Inotify::Error.from_errno "inotify init failed" if fd == -1
    Log.debug { "inotify init" }
    @io = IO::FileDescriptor.new(fd)
    IO::FileDescriptor.set_blocking(fd, false)
    Log.debug { "inotify IO created" }

    @event_channel = Channel(Inotify::Event).new
    @enabled = true
    @workers = WaitGroup.new
    @workers.spawn do
      lurk
    rescue e : IO::Error | Channel::ClosedError # < especially this
      Log.debug(exception: e) { "error in lurk" }
    end
  end

  def events
    @event_channel
  end

  def close : Nil
    super
    @event_channel.close
    Log.debug { "inotify: workers wait" }
    @workers.wait
    Log.debug { "inotify: workers done" }
  end
end

# Path tracker tracks file path changes using a combination of inotify
# for existing paths and `stat` polling for missing paths.
#
# The goal is not to precisely describe each change, but rather, to give
# you a more or less reliable wake-up mechanism for particular paths,
# regardless of whether they exist or not (e.g. inotify breaks UX-wise
# on removal).
#
# ```
# ctx = Fiber::ExecutionContext::Isolated.new("path tracker", spawn_context: MT) do
#   paths = PathTracker.watchset do |path|
#     # `path` changed...
#     pp path
#   end
#
#   paths << Path["/tmp/hello"]
#   paths << Path["/tmp/world"]
#   sleep
# end
#
# ctx.wait
# ```
module Ww::PathTracker
  extend self

  Log = ::Log.for(self)

  # :nodoc:
  record Message, type : Type, path : Path do
    enum Type
      PathChanged
      Watch
      Unwatch
      WatchExisting
      WatchMissing
      UnwatchMissing
    end
  end

  # :nodoc:
  module Fsync
  end

  record PathAction, type : Type, path : Path do
    enum Type
      Add
      Delete
    end
  end

  # Starts the tracker fibers. Sends changed paths to *changes*.
  #
  # Send actions to *actions* to track. Close *actions* to stop the tracker
  # and free all associated resources.
  #
  # *nap* specifies the polling rate for missing files.
  #
  # NOTE: this function **does not** close *changes* on shutdown.
  def track(actions : Channel(PathAction), changes : Channel(Path), *, nap = 500.milliseconds) : Nil
    notifier = Channel(Message).new
    poller = Channel(Message).new
    beats = Channel(Nil).new
    events = BlockingQueue(Message?).new

    workers = WaitGroup.new
    workers.spawn { notifier(events, notifier) }
    workers.spawn { poller(events, poller, beats) }
    workers.spawn { heartbeat(beats, nap) }
    # NOTE: This fiber will control all the channels & the queue from now on.
    # We can only use `events`.
    workers.spawn { dispatcher(events, notifier, poller, beats, changes) }

    while action = actions.receive?
      case action.type
      in .add?    then events << Message.new(:watch, action.path)
      in .delete? then events << Message.new(:unwatch, action.path)
      end
    end

    events << nil
    workers.wait
  end

  # An object wrapper around the tracking API.
  struct WatchSet
    # :nodoc:
    def initialize(@actions : Channel(PathAction))
    end

    # Adds *path* to the watchlist. It is your responsibility to add only
    # unique paths.
    def add(path : Path) : Nil
      @actions << PathAction.new(:add, path)
    end

    # Alias of `add`.
    def <<(path : Path) : self
      add(path)

      self
    end

    # Removes *path* from the watchlist. It is your responsibility to remove
    # only watched paths.
    def delete(path : Path) : Nil
      @actions << PathAction.new(:delete, path)
    end

    # Stops tracking and frees all associated resources.
    def close : Nil
      @actions.close
    end
  end

  # Constructs a `WatchSet` object for simplified API. Returns the watchset
  # object and a channel to receive changes on. The channel is closed when
  # the watchset is closed. This is a blocking alternative to `watchset`.
  #
  # *kwargs* are passed to `track`.
  def watchset_and_changes(**kwargs) : {WatchSet, Channel(Path)}
    actions = Channel(PathAction).new
    changes = Channel(Path).new

    spawn do
      track(actions, changes, **kwargs)
    ensure
      changes.close
    end

    {WatchSet.new(actions), changes}
  end

  # Constructs a `WatchSet` object for simplified API. *sink* is called on
  # each path change.
  #
  # *kwargs* are passed to `track`.
  #
  # NOTE: *sink* is called from another fiber.
  def watchset(**kwargs, &sink : Path ->) : WatchSet
    watchset, changes = watchset_and_changes(**kwargs)

    spawn do
      while change = changes.receive?
        sink.call(change)
      end
    end

    watchset
  end

  # Close *messages* to terminate this fiber and free all associated resources.
  private def notifier(events, messages) : Nil
    watcher = Inotify::BetterWatcher.new
    watching = Set(Path).new

    begin
      loop do
        select
        when event = watcher.events.receive
          next unless filename = event.path
          path = Path[filename]
          next unless path.in?(watching)

          case event.type
          when .attrib?, .create?, .modify?
            events << Message.new(:path_changed, path)
          when .ignored?
            # It's already unwatch'd.
            #
            # Reference: https://www.man7.org/linux/man-pages/man7/inotify.7.html

            begin
              # Try to recover immediately.
              watcher.watch(path.to_s)
            rescue e : Inotify::Error
              # Retry later with poll.
              watching.delete(path)
              events << Message.new(:watch_missing, path)
            end

            events << Message.new(:path_changed, path)
          end
        when message = messages.receive?
          unless message
            return
          end

          case message.type
          when .watch?, .watch_existing?
            next unless watching.add?(message.path)

            begin
              watcher.watch(message.path.to_s)
            rescue e : Inotify::Error
              # E.g. doesn't exist.
              watching.delete(message.path)
              events << Message.new(:watch_missing, message.path)
            end
          when .unwatch?
            unless watching.delete(message.path)
              events << Message.new(:unwatch_missing, message.path)
              next
            end

            watcher.unwatch(message.path.to_s)
          end
        end
      end
    ensure
      watching.clear
      watcher.close
    end
  end

  # Close *messages* to terminate this fiber and free all associated
  # resources. *Do not* close *beats*: it is closed by this fiber
  # automatically on shutdown.
  private def poller(events, messages, beats)
    watching = {} of Path => Time?

    loop do
      select
      when message = messages.receive?
        unless message
          beats.close
          return
        end

        case message.type
        when .watch_missing?
          assert !watching.has_key?(message.path)
          watching[message.path] = nil
        when .unwatch_missing?
          watching.delete(message.path)
        end
      when beats.receive
        expired = Set(Path).new

        watching.transform_values! do |timestamp0, path|
          if info = File.info?(path)
            timestamp1 = info.modification_time
          end

          unless timestamp0 == timestamp1
            events << Message.new(:path_changed, path)
            events << Message.new(:watch_existing, path)
            expired << path
          end

          timestamp1
        end

        watching.reject! { |path, _| path.in?(expired) }
      end
    end
  end

  # Close *beats* to terminate this fiber and free all associated resources.
  private def heartbeat(beats, nap)
    loop do
      select
      when beats.receive? # closed: other fibers may only call #close on beats
        return
      when timeout(nap)
        beats.send(nil)
      end
    end
  end

  private def dispatcher(events, notifier, poller, beats, changes)
    loop do
      unless event = events.shift
        notifier.close
        poller.close
        return
      end

      Log.trace { event }

      case event.type
      in .watch?, .unwatch?, .watch_existing?
        notifier << event
      in .watch_missing?, .unwatch_missing?
        poller << event
      in .path_changed?
        changes << event.path
      end
    end
  end
end
