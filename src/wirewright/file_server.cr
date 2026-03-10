module Ww
  # Raised when a file server fails to operate on a file.
  class FileServerError < Exception
  end

  class ::File
    def self.tempfile(random : ::Random, *, tempdir : String | Path = Dir.tempdir)
      fileno, path, blocking = Crystal::System::File.mktemp(prefix: nil, suffix: nil, dir: tempdir.to_s, random: random)
      new(path, fileno, blocking: blocking)
    end
  end

  # Includers are *file servers*: they, well... *serve files*.
  #
  # Wirewright's file server abstraction is, in essence, about interacting
  # with some underlying file system, or maybe the network, to read and
  # write files.
  #
  # NOTE: This particular implementation is (by far) not the final one. We'd
  # like to represent files more "algebraically". There are bits and pieces
  # of this all over the code (esp. in Rack file server), which we'd like to
  # unify eventually.
  module FileServer
    enum WriteMode
      Overwrite
      Append
    end

    enum ReadCompression
      None
      Gzip

      def self.from_file_extension(path : Path) : ReadCompression
        from_file_extension(path.extension)
      end

      def self.from_file_extension(extension : String) : ReadCompression
        case extension
        when ".gz" then Gzip
        else
          None
        end
      end

      def source(io : IO, & : IO ->)
        case self
        in .none?
          yield io
        in .gzip?
          Compress::Gzip::Reader.open(io) { |gzip| yield gzip }
        end
      end
    end

    enum WriteCompression
      None
      GzipFast
      GzipBest

      def self.from_file_extension(path : Path) : ReadCompression
        from_file_extension(path.extension)
      end

      def self.from_file_extension(extension : String) : WriteCompression
        case extension
        when ".gz" then GzipFast
        else
          None
        end
      end

      def fast : WriteCompression
        case self
        in .none?      then self
        in .gzip_fast? then self
        in .gzip_best? then GzipFast
        end
      end

      def best : WriteCompression
        case self
        in .none?      then self
        in .gzip_fast? then GzipBest
        in .gzip_best? then self
        end
      end

      def sink(io : IO, & : IO ->)
        case self
        in .none?
          yield io
        in .gzip_fast?
          Compress::Gzip::Writer.open(io, level: Compress::Gzip::BEST_SPEED) { |gzip| yield gzip }
        in .gzip_best?
          Compress::Gzip::Writer.open(io, level: Compress::Gzip::BEST_COMPRESSION) { |gzip| yield gzip }
        end
      end
    end

    # Converts *path* to an absolute path, taking into account the file server's
    # own context (e.g. which path is set as base path).
    abstract def resolve(path : Path) : Path

    # Returns the modification time of the file at *path*.
    abstract def timestamp?(path : Path) : Time?

    # Returns the content of the file at *path*.
    #
    # *compression* sets the compression algorithm.
    #
    # Raises `FileServerError` if the file cannot be read.
    abstract def read(path : Path, *, compression : ReadCompression) : Bytes

    # Calls `read` with no compression.
    def read(path : Path) : Bytes
      read(path, compression: :none)
    end

    # Returns the content of the file at *path* as a `String`.
    #
    # Raises `FileServerError` if the file cannot be read.
    def read_string(path : Path, **kwargs) : String
      String.new(read(path, **kwargs))
    end

    # Writes to the file at *path* using the block (creating the file if necessary),
    # possibly with compression.
    #
    # Raises `FileServerError` in case of an error.
    abstract def write(path : Path, *, mode : WriteMode, compression : WriteCompression, & : IO ->) : Nil

    # :ditto:
    def write(path : Path, & : IO ->) : Nil
      write(path, mode: :overwrite, compression: :none) { |io| yield io }
    end

    def write(path : Path, content : Bytes, *, compression : WriteCompression = :none)
      write(path, mode: :overwrite, compression: :none, &.write(content))
    end

    # Removes the file at *path*.
    #
    # Raises `FileServerError` in case of an error. Noop if the file does not exist.
    abstract def delete(path : Path) : Nil
  end

  # Thread-unsafe `FileServer` implemented using Crystal's `File` API.
  struct Disk
    include FileServer

    Log = ::Log.for(self)

    # *base* is the base path used to expand relative paths. It must be an absolute
    # path, otherwise, `ArgumentError` is raised.
    def initialize(@base : Path)
      unless @base.absolute?
        raise ArgumentError.new
      end

      @rng = Random::PCG32.new
    end

    def resolve(path : Path) : Path
      path.expand(@base, expand_base: false)
    end

    def timestamp?(path : Path) : Time?
      path = resolve(path)

      if info = File.info?(path)
        info.modification_time
      end
    end

    def read(path : Path, *, compression : ReadCompression) : Bytes
      path = resolve(path)

      Log.debug { "read #{path}" }

      File.open(path, "rb") do |io|
        io.flock_exclusive do
          Log.debug { "read: flock'd, reading" }

          compression.source(io) do |src|
            src.getb_to_end
          end
        end
      end
    rescue e : File::Error
      raise FileServerError.new("unable to load file: #{e.message}", cause: e)
    end

    def write(path : Path, *, mode : WriteMode, compression : WriteCompression, & : IO ->) : Nil
      path = resolve(path)

      Log.debug { "write: path=`#{path}`, mode=`#{mode}`" }

      case mode
      in .append?
        File.open(path, "a") do |io|
          io.flock_exclusive do
            Log.debug { "write: append: flock'd, appending" }
            compression.sink(io) { |dst| yield dst }
          end
        end
        Log.debug { "write: append: ok" }
      in .overwrite?
        tmp_file = File.tempfile(@rng)
        tmp_path = tmp_file.path

        begin
          tmp_file.flock_exclusive do
            Log.debug { "write: tmp #{tmp_path} flock'd, writing" }
            compression.sink(tmp_file) { |dst| yield dst }
            tmp_file.fsync
          end
        rescue e
          raise FileServerError.new("could not write to tmp file", cause: e)
        ensure
          # We're done doing anything content-related with it.
          tmp_file.close
        end

        Log.debug { "write: rename tmp #{tmp_path} -> #{path}" }
        begin
          File.rename(tmp_path, path)
        rescue e : File::Error
          if e.os_error == Errno::EXDEV # EXDEV requires us to do a copy-delete
            Log.debug { "write: EXDEV, copy tmp #{tmp_path} -> #{path}" }
            File.copy(tmp_path, path)
            Log.debug { "write: delete tmp after copy" }
            File.delete?(tmp_path)
          else
            raise e
          end
        end
      end
    rescue e : File::Error | IO::Error
      raise FileServerError.new("unable to write file: #{e.message}", cause: e)
    end

    def delete(path : Path) : Nil
      path = resolve(path)

      Log.debug { "delete #{path}" }

      File.delete?(path)
    rescue e : File::Error
      raise FileServerError.new("unable to delete file: #{e.message}", cause: e)
    end
  end

  # Thread-safe wrapper around `Disk`.
  #
  # Access to the server is synchronized using a lock.
  struct SyncDisk
    include FileServer

    def initialize(base : Path)
      @disk = Disk.new(base)
      @lock = Sync::Mutex.new
    end

    def resolve(path : Path) : Path
      @lock.synchronize { @disk.resolve(path) }
    end

    def timestamp?(path : Path) : Time?
      @lock.synchronize { @disk.timestamp?(path) }
    end

    def read(path : Path, *, compression : ReadCompression) : Bytes
      @lock.synchronize { @disk.read(path, compression: compression) }
    end

    def write(path : Path, *, mode : WriteMode, compression : WriteCompression, & : IO ->) : Nil
      @lock.synchronize do
        @disk.write(path, mode: mode, compression: compression) do |io|
          yield io
        end
      end
    end

    def delete(path : Path) : Nil
      @lock.synchronize { @disk.delete(path) }
    end
  end
end
