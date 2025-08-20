module Ww
  # Raised when a file server fails to operate on a file.
  class FileServerError < Exception
  end

  # Includers can operate on a file given its path.
  module FileServer
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

    # Returns the modification time of the file at *path*.
    abstract def modification_time?(path : Path) : Time?

    # Returns the content of the file at *path*.
    #
    # Raises `FileServerError` if the file cannot be read.
    abstract def read(path : Path) : Bytes

    # Writes to the file at *path* using the block, optionally compressing
    # whatever is written.
    #
    # Raises `FileServerError` in case of an error.
    abstract def write(path : Path, *, compression : Compression, & : IO ->) : Nil

    # :ditto:
    def write(path : Path, & : IO ->) : Nil
      write(path, compression: :none) { |io| yield io }
    end
  end

  # A crude implementation of `FileServer` that directly calls Crystal's `File` API.
  module Disk
    extend FileServer

    Log = ::Log.for(self)

    def self.modification_time?(path : Path) : Time?
      if info = File.info?(path)
        info.modification_time
      end
    end

    def self.read(path : Path) : Bytes
      Log.debug { "read #{path}" }

      File.open(path, "rb", &.getb_to_end)
    rescue e : File::Error
      raise FileServerError.new("unable to load file at #{path}", cause: e)
    end

    # TODO: copy over ".prev" if current exists to ensure transactionality
    def self.write(path : Path, *, compression : FileServer::Compression, & : IO ->) : Nil
      File.open(path, "w") do |io|
        compression.sink(io) do |io|
          yield io
        end
      end
    rescue e : File::Error | IO::Error
      raise FileServerError.new("error while writing to file at #{path}", cause: e)
    end
  end
end
