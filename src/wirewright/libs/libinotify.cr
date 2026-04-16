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
