module Ww
  # Raised when a file server fails to operate on a file.
  class FileServerError < Exception
  end

  class ::File
    def self.tempfile(random : ::Random)
      fileno, path, blocking = Crystal::System::File.mktemp(prefix: nil, suffix: nil, dir: Dir.tempdir, random: random)
      new(path, fileno, blocking: blocking)
    end
  end

  # Includers can operate on a file given its path.
  #
  # All methods of a file server are (supposed to be) thread-safe.
  module FileServer
    enum WriteMode
      Overwrite
      Append
    end

    enum Compression
      None
      GzipFast
      GzipBest

      def self.from_file_extension(extension : String) : Compression
        case extension
        when ".gz" then GzipFast
        else
          None
        end
      end

      def fast : Compression
        case self
        in .none?      then self
        in .gzip_fast? then self
        in .gzip_best? then GzipFast
        end
      end

      def best : Compression
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
          Compress::Gzip::Writer.open(io, level: Compress::Gzip::BEST_SPEED) { |io| yield io }
        in .gzip_best?
          Compress::Gzip::Writer.open(io, level: Compress::Gzip::BEST_COMPRESSION) { |io| yield io }
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
    # Raises `FileServerError` if the file cannot be read.
    abstract def read(path : Path) : Bytes

    # Returns the content of the file at *path* as a `String`.
    #
    # Raises `FileServerError` if the file cannot be read.
    def read_string(path : Path) : String
      String.new(read(path))
    end

    # Writes to the file at *path* using the block (creating the file if necessary),
    # possibly with compression.
    #
    # Raises `FileServerError` in case of an error.
    abstract def write(path : Path, *, mode : WriteMode, compression : Compression, & : IO ->) : Nil

    # :ditto:
    def write(path : Path, & : IO ->) : Nil
      write(path, mode: :overwrite, compression: :none) { |io| yield io }
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

    def read(path : Path) : Bytes
      path = resolve(path)

      Log.debug { "read #{path}" }

      File.open(path, "rb", &.getb_to_end)
    rescue e : File::Error
      raise FileServerError.new("unable to load file: #{e.message}", cause: e)
    end

    def write(path : Path, *, mode : FileServer::WriteMode, compression : FileServer::Compression, & : IO ->) : Nil
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

    def read(path : Path) : Bytes
      @lock.synchronize { @disk.read(path) }
    end

    def write(path : Path, *, mode : FileServer::WriteMode, compression : FileServer::Compression, & : IO ->) : Nil
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
