module Ww
  # Raised when a file server fails to operate on a file.
  class FileServerError < Exception
  end

  # Includers can operate on a file given its path.
  module FileServer
    # Returns the content of the file at *path*.
    #
    # Raises `FileServerError` if the file cannot be read.
    abstract def read(path : Path) : Bytes
  end

  # A crude implementation of `FileServer` that directly calls Crystal's `File` API.
  module Disk
    extend FileServer

    def self.read(path : Path) : Bytes
      File.open(path, "rb", &.getb_to_end)
    rescue e : File::Error
      raise FileServerError.new("unable to load file at #{path}", cause: e)
    end
  end
end
