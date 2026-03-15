module Ww::DwUIR
  # A PlutoVG-based implementation of an image server.
  class PvgImageServer
    include ImageServer

    Log = ::Log.for(self)

    def initialize
      @cache = {} of Term => PvgImage
    end

    private def load!(src : Term) : PvgImage?
      Term.case(src) do
        matchpi %{(file path_string)} do |path|
          path = Path[path.to(String)]

          case response = ResourceServer.wait(ResourceServer.file(path))
          in ResourceServer::Present
            data = response.content.bytes
          in ResourceServer::Absent
            raise ImageServerError.new("could not read image file: #{response.detail}")
          end

          case path.extension
          when ".svg"
            PvgSvgImage.new(data)
          when ".png", ".jpg", ".jpeg", ".bmp", ".psd", ".gif", ".ppm"
            # |@ soma.dwuir.paint.image.plutovg
            #
            # |@block
            # PlutoVG uses stb-image under the hood. Thus the following formats are supported, citing
            # from stb-image v2.30:
            #
            #   - JPEG baseline & progressive (12 bpc/arithmetic not supported, same as stock IJG lib)
            #   - PNG 1/2/4/8/16-bit-per-channel
            #   - TGA (not sure what subset, if a subset)
            #   - BMP non-1bpp, non-RLE
            #   - PSD (composited view only, no extra channels, 8/16 bit-per-channel)
            #   - GIF (*comp always reports as 4-channel)
            #   - HDR (radiance rgbE format)
            #   - PIC (Softimage PIC)
            #   - PNM (PPM and PGM binary only)
            #
            # Note that Wirewright itself only lets the following file extensions through:
            # png, jpg, jpeg, bmp, psd, gif, ppm. The above applies to them.
            # |@endblock

            unless surface = PlutoVG.surface_load_from_image_data(data, data.size)
              raise ImageServerError.new("image file found but its content appears to be malformed (could not load)")
            end

            PvgRasterImage.new(surface)
          else
            raise ImageServerError.new("invalid or unsupported file extension: #{path.extension}")
          end
        end

        otherwise do
          raise ImageServerError.new("invalid or unsupported image src: #{src}")
        end
      end
    end

    def load(src : Term) : PvgImage
      @cache.put_if_absent(src) { load!(src) }
    end

    def unload(src : Term) : Nil
      @cache.delete(src)
    end
  end

  alias PvgImage = PvgRasterImage | PvgSvgImage

  # Represents a raster image. Points to the underlying PlutoVG surface, which
  # will therefore be reused for all instances of the image.
  class PvgRasterImage
    include Image

    getter size : Point

    # :nodoc:
    def initialize(@surface : PlutoVG::Surface)
      @size = Point[
        PlutoVG.surface_get_width(surface),
        PlutoVG.surface_get_height(surface),
      ]
    end

    def finalize
      PlutoVG.surface_destroy(@surface)
    end

    def to_unsafe : PlutoVG::Surface
      @surface
    end
  end

  # Represents an SVG image.
  #
  # It appears that PlutoSVG documents are expended by rendering them; therefore,
  # `document` yields a new document every time it is called. Only the underlying
  # SVG bytes (markup) are reused.
  class PvgSvgImage
    include Image

    # :nodoc:
    def initialize(@data : Bytes)
    end

    getter size : Point do
      # This looks really really expensive...

      unless document = PlutoSVG.document_load_from_data(@data, @data.size, 0, 0, nil, nil)
        raise ImageServerError.new("image file does not appear to be (a supported kind of) SVG")
      end

      unless PlutoSVG.document_extents(document, nil, out extents)
        return Point[0, 0]
      end

      Point.new(extents.w, extents.h)
    end

    # WARNING: you **must not** retain the yielded document. It is freed
    # after the block.
    def document(vwh : Point, & : PlutoSVG::Document ->)
      unless document = PlutoSVG.document_load_from_data(@data, @data.size, vwh.x, vwh.y, nil, nil)
        raise ImageServerError.new("image file does not appear to be (a supported kind of) SVG")
      end

      begin
        yield document
      ensure
        PlutoSVG.document_destroy(document)
      end
    end
  end
end
