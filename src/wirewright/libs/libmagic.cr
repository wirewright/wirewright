# Reference: https://github.com/naqvis/magic.cr
@[Link("magic")]
lib LibMagic
  type Handle = Void*

  # Flags for Magic open
  @[Flags]
  enum OpenFlags : LibC::Int
    # Turn on debugging
    DEBUG = 1
    # Follow symlinks
    SYMLINK = 2
    # Check inside compressed files
    COMPRESS = 4
    # Look at the contents of devices
    DEVICES = 8
    # Return the MIME type
    MIME_TYPE = 16
    # Return all matches
    CONTINUE = 32
    # Print warnings to stderr
    CHECK = 64
    # Restore access time on exit
    PRESERVE_ATIME = 128
    # Don't convert unprintable chars
    RAW = 256
    # Handle ENOENT etc as real errors
    ERROR = 512
    # Return the MIME encoding
    MIME_ENCODING = 1024

    def self.mime
      MIME_TYPE | MIME_ENCODING
    end

    # Return the Apple creator/type
    APPLE = 2048
    # Return a /-separated list of extensions
    EXTENSION = 16777216
    # Check inside compressed files but not report compression
    COMPRESS_TRANSP = 33554432

    def self.nodesc
      EXTENSION | MIME | APPLE
    end

    # Don't check for compressed files
    NO_CHECK_COMPRESS = 4096
    # Don't check for tar files
    NO_CHECK_TAR = 8192
    # Don't check magic entries
    NO_CHECK_SOFT = 16384
    # Don't check application type
    NO_CHECK_APPTYPE = 32768
    # Don't check for elf details
    NO_CHECK_ELF = 65536
    # Don't check for text files
    NO_CHECK_TEXT = 131072
    # Don't check for cdf files
    NO_CHECK_CDF = 262144
    # Don't check for CSV files
    NO_CHECK_CSV = 524288
    # Don't check tokens
    NO_CHECK_TOKENS = 1048576
    # Don't check text encodings
    NO_CHECK_ENCODING = 2097152
    # Don't check for JSON files
    NO_CHECK_JSON = 4194304

    # No built-in tests; only consult the magic file
    def self.no_check_builtin
      NO_CHECK_COMPRESS | NO_CHECK_TAR | NO_CHECK_APPTYPE |
        NO_CHECK_ELF | NO_CHECK_TEXT | NO_CHECK_CSV | NO_CHECK_CDF |
        NO_CHECK_TOKENS | NO_CHECK_ENCODING | NO_CHECK_JSON
    end
  end

  fun magic_open(flags : OpenFlags) : Handle
  fun magic_close(x0 : Handle)
  fun magic_buffer(x0 : Handle, x1 : Void*, x2 : LibC::SizeT) : LibC::Char*
  fun magic_error(x0 : Handle) : LibC::Char*
  fun magic_load(x0 : Handle, x1 : LibC::Char*) : LibC::Int
end

# Bindings to libmagic.
module Ww::Magic
  extend self

  # As I have absolutely no clue about what's going on inside libmagic wrt
  # thread-safety, let's assume it's thread-unsafe and synchronize all
  # access to it from our side.
  @@lock = Sync::Mutex.new
  @@handle : LibMagic::Handle?

  # WARNING: Assumes @@lock is taken.
  private def handle
    @@handle ||= begin
      handle = LibMagic.magic_open(LibMagic::OpenFlags.mime)
      if handle.nil?
        abort "libmagic: could not initialize"
      end

      LibMagic.magic_load(handle, nil)
      if error = LibMagic.magic_error(handle)
        abort String.new(error)
      end

      handle
    end
  end

  # Detects the MIME type of *slice*.
  def mime(slice : Bytes) : MIME::MediaType
    string = @@lock.synchronize do
      LibMagic.magic_buffer(handle, slice, slice.size) || raise "libmagic: could not classify buffer"
    end

    MIME::MediaType.parse(String.new(string))
  end
end
