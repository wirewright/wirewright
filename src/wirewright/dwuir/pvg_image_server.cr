module Ww::DwUIR
  alias PvgImage = PvgRasterImage | PvgSvgImage

  # Represents a raster image. Points to the underlying PlutoVG surface, which
  # will therefore be reused for all instances of the image.
  class PvgRasterImage
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
    getter size : Point

    # :nodoc:
    #
    # WARNING: The caller must ensure that *data* is a valid SVG document (such that
    # `PlutoSVG.document_load_from_data` is guaranteed to succeed).
    def initialize(@data : Bytes, @size : Point)
    end

    # WARNING: you **must not** retain the yielded document. It is destroyed
    # after the block.
    def document(vwh : Point, & : PlutoSVG::Document ->)
      document = PlutoSVG.document_load_from_data(@data, @data.size, vwh.x, vwh.y, nil, nil)
      assert document

      begin
        yield document
      ensure
        PlutoSVG.document_destroy(document)
      end
    end
  end

  # Serves PlutoVG and PlutoSVG image objects using `ResourceServer`.
  #
  # TODO: Images are currently kept in memory indefinitely (but overridden by
  # their newer versions on change). A mechanism to unload images is planned
  # but not implemented. Ideally, we'd like each DwUIR client, on each frame,
  # to declare the images it wants, so that we can unload those no one references.
  # A simple time-based cache could work as well. The exact design remains
  # unclear at the moment.
  module PvgImageServer
    extend self

    alias Response = PvgImage | Absent

    # Indicates that the requested image currently does not exist. Calling
    # `get` (or derived) later, however, may result in an positive response.
    defrecord Absent, detail : String

    # Indicates that the query is being processed. The caller must call `get`
    # (or derived) later to receive a positive or negative response.
    defrecord Wait

    @@lock = Sync::Mutex.new

    @@supply = {} of ResourceServer::Query => {Bytes, Response}
    @@supply_changed = BlockingSignal.new

    @@demand = Pf::Set({ResourceServer::Query, Term::Blob}).new
    @@demand_changed = BlockingSignal.new

    @@running = Atomic(Bool).new(false)

    MEDIA_TYPES_PNG  = {Term["image/png"]}
    MEDIA_TYPES_JPEG = {Term["image/jpeg"]}
    MEDIA_TYPES_BMP  = {Term["image/bmp"]}
    MEDIA_TYPES_GIF  = {Term["image/gif"]}
    MEDIA_TYPES_PPM  = {Term["image/x-portable-pixmap"]}
    MEDIA_TYPES_SVG  = {Term["image/svg+xml"]}

    private def ensure_server_running!
      return if @@running.swap(true)

      spawn(name: "PvgImageServer monitor loop") do
        epoch = 0u64

        loop do
          epoch = ResourceServer.wait(epoch)

          @@supply_changed.call
        end
      end

      spawn(name: "PvgImageServer image loader") do
        Log.debug { "running" }

        epoch = 0u64

        loop do
          epoch = @@demand_changed.wait(epoch)

          demand = @@lock.synchronize do
            demand0 = @@demand
            demand1 = Pf::Set({ResourceServer::Query, Term::Blob}).new
            demand0
          end

          next if demand.empty?

          Log.debug { "woke up with #{demand.size} demand(s)" }

          demand.each do |query, blob|
            response = Absent.new(
              "unrecognized or malformed image (expected well-formed .png, .jp[e]g, \
               .bmp, .gif, .ppm, or .svg)"
            )

            classif = Term::Blob.classif(blob)

            case classif.media_type
            when .in?(MEDIA_TYPES_PNG),
                 .in?(MEDIA_TYPES_JPEG),
                 .in?(MEDIA_TYPES_BMP),
                 .in?(MEDIA_TYPES_GIF),
                 .in?(MEDIA_TYPES_PPM)
              if surface = PlutoVG.surface_load_from_image_data(blob.bytes, blob.bytes.size)
                response = PvgRasterImage.new(surface)
              end
            when .in?(MEDIA_TYPES_SVG)
              if document = PlutoSVG.document_load_from_data(blob.bytes, blob.bytes.size, 0, 0, nil, nil)
                size = Point[0, 0]
                if PlutoSVG.document_extents(document, nil, out extents)
                  size = Point[extents.w, extents.h]
                end

                response = PvgSvgImage.new(blob.bytes, size)
              end
            end

            @@lock.synchronize do
              @@supply[query] = {blob.digest, response}
            end
          end

          Log.debug { "wake up waiters (supply changed)" }

          @@supply_changed.call
        end
      end
    end

    # Responds to *query*.
    #
    # *query* is first resolved using `ResourceServer`, so see it for more info.
    # The resulting blob is interpreted as an image: a raster image (`PvgRasterImage`),
    # or a vector image (`PvgSvgImage`). The decision is made based on the blob's
    # mime type (see `MEDIA_TYPES_*` constants).
    def get(query : ResourceServer::Query) : Response | Wait
      ensure_server_running!

      case response = ResourceServer.get(query)
      in ResourceServer::Wait
        Wait.new
      in ResourceServer::Absent
        Absent.new(response.detail)
      in ResourceServer::Present
        @@lock.synchronize do
          if entry = @@supply[query]?
            entry_digest, entry_response = entry
            if entry_digest == response.content.digest
              return entry_response
            end
          end

          @@demand = @@demand.add({query, response.content})
          @@demand_changed.call

          Wait.new
        end
      end
    end

    # Waits for `ResourceServer` (`ResourceServer.wait`) or the image server's own
    # data structures to change.
    def wait(epoch : UInt64) : UInt64
      @@supply_changed.wait(epoch)
    end

    # Parses *term* using `ResourceServer.query?`, and then blocks the caller
    # until the server responds positively or negatively.
    def wait(term : Term) : PvgImage | Absent
      unless query = ResourceServer.query?(term)
        return Absent.new("invalid query term")
      end

      epoch = 0u64

      loop do
        case response = get(query)
        in Wait
          epoch = wait(epoch)
        in PvgImage, Absent
          return response
        end
      end
    end
  end
end
