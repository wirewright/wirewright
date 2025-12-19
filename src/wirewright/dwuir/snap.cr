module Ww::DwUIR
  # Raised when snapshotting fails.
  class SnapError < Exception
  end

  # Manages the supported snapshot/image formats.
  #
  # Each supported format is associated with a `Sink` -- a function that
  # writes a `PixelRect` to *io* using that format.
  #
  # TODO: png
  # TODO: jpeg
  module SnapFormat
    extend self

    alias Sink = IO, PixelRect ->

    private def ppm : Sink
      ->(io : IO, screen : PixelRect) do
        io << "P3\n"
        io << screen.width << " " << screen.height << "\n"
        io << "255\n"

        screen.region(screen.bounds).each_pixel_with_coords do |pixel, _, _|
          # Ignore alpha
          r, g, b, _ = pixel.rgba
          io << r << " " << g << " " << b << "\n"
        end
      end
    end

    private def rgb_zstd : Sink
      ->(io : IO, screen : PixelRect) do
        io << screen.width << ":" << screen.height << ":"

        # level: -7 (fast) -> 22 (best)
        Zstd::Compress::IO.open(io, level: 3) do |sink|
          screen.region(screen.bounds).each_pixel_with_coords do |pixel, _, _|
            r, g, b, _ = pixel.rgba8
            sink.write_byte(r)
            sink.write_byte(g)
            sink.write_byte(b)
          end
        end
      end
    end

    # Returns a sink function for *format*, or `nil` if unsupported.
    def []?(format : String) : Sink?
      case format
      when "ppm"      then ppm
      when "rgb-zstd" then rgb_zstd
      end
    end

    # Returns a sink function for *format*. Raises `ArgumentError` if unsupported.
    def self.[](format : String) : Sink
      self[format]? || raise ArgumentError.new("unsupported format #{format}")
    end

    # Returns a list of supported formats.
    def supported : Indexable(String)
      {"ppm", "rgb-zstd"}
    end
  end

  # Captures a snapshot based on *conf*, and appends the resulting image
  # to *io*.
  def snap(io : IO, ctx : Viewer::Context, conf : ShowConf, format : SnapFormat::Sink) : Nil
    image = DwUIR.show(ctx, conf)
    format.call(io, image)
  end

  # Captures a snapshot based on *conf*, and writes the resulting image
  # to *path* (accessed through the given file server *files*).
  def snap(ctx : Viewer::Context, conf : ShowConf, path : Path) : Nil
    extension = path.extension
    compression = FileServer::WriteCompression.from_file_extension(extension)
    unless compression.none?
      extension = Path[path.stem].extension
    end

    if extension.prefixed_by?('.')
      format = SnapFormat[extension.lchop]?
    end

    unless format
      raise SnapError.new("unsupported image extension `#{extension}`; supported extensions are: #{SnapFormat.supported.join(", ")}")
      return
    end

    ctx.platform.files.write(path, compression: compression.best) do |io|
      snap(io, ctx, conf, format)
    end
  end
end
