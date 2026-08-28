module Ww::Scenery
  # The asset subsystem of Scenery integrates `ResourceService` with internal
  # representations for assets (such as `PlutoVG::FontFace`).
  #
  # See also: `Asset::Any`, `Scenery.wait`, `Scenery.poll`.
  module Asset
    extend self

    alias Query = FontQuery | CodepointsQuery | ImageQuery | SvgQuery

    # The result of *response* will be interpreted as a `Font` asset.
    defrecord FontQuery, resource : ResourceService::Query

    # The result of *response* will be interpreted as a `Codepoint` asset.
    defrecord CodepointsQuery, resource : ResourceService::Query

    # The result of *response* will be interpreted as an `Image` asset.
    defrecord ImageQuery, resource : ResourceService::Query

    # The result of *response* will be interpreted as an `Svg` asset.
    defrecord SvgQuery, resource : ResourceService::Query

    # All asset types respond to `digest`, which returns its hash digest (to
    # learn about the algorithm used, see `Term::Blob::DIGEST_ALGORITHM`).
    #
    # This means all assets can be very cheaply compared. We use this particular
    # feature for dirt cheap and effortless cache invalidation. Do keep in mind
    # however that we must always talk to the resource server first. The resource
    # server gives us what it thinks is the latest blob for a particular asset,
    # and then we see if that blob's digest is the same as the one we have in
    # cache; if it is, perfect, reuse the asset. If it's not, we parse, producing
    # a new asset. Everything downstream will now see the new asset's digest is
    # different from the old one, producing a "cascade" of invalidation.
    #
    # NOTE: The logic described above is managed in `poll` and `wait`. `Asset`
    # only provides the data types for assets, and parsing (i.e., constructors
    # for those data types).
    alias Any = Font | PvgRasterImage | PvgSvgImage | CodepointsMap

    # Wraps a clamped font size, acts as a proof that you called `Font.clamp`.
    struct FontSize
      # Returns the underlying clamped font size.
      getter value : UInt32

      protected def initialize(@value)
      end
    end

    # Represents a font.
    class Font
      getter digest

      # :nodoc:
      def initialize(@ft_face : FreeType::Face, @digest : Bytes)
        @metrics = LRU(FontSize, FontMetrics).new(16)
        @indices = LRU(Char, UInt32).new(256)
        @measurements = LRU({UInt32, FontSize}, GlyphMeasurement).new(256)
      end

      # Clamps *size* into acceptable bounds.
      def self.clamp(size : Magnitude) : FontSize
        FontSize.new(size.clamp(Magnitude.new(5)..Magnitude.new(1024)).floor.to_u32)
      end

      def finalize
        FreeType.done_face(@ft_face)
      end

      # :nodoc:
      def as_ft : FreeType::Face
        @ft_face
      end

      def load_glyph(index : UInt32, size : FontSize) : Nil
        assert FreeType.set_char_size(@ft_face, size.value * FT_UNIT, 0, 0, 0).zero?
        assert FreeType.load_glyph(@ft_face, index, FreeType::LOAD_NO_BITMAP | FreeType::LOAD_NO_HINTING).zero?
      end

      # Returns the index of the glyph for *codepoint*. Returns `0` if there is no
      # such glyph.
      def index(codepoint : Char) : UInt32
        @indices.put_if_absent(codepoint) do
          FreeType.get_char_index(@ft_face, codepoint.ord)
        end
      end

      # Measures the glyph with the given *index* at *size*.
      def measure(index : UInt32, size : FontSize) : GlyphMeasurement
        @measurements.put_if_absent({index, size}) do
          load_glyph(index, size)

          FreeType.get_glyph_metrics(@ft_face, out advance, out extents_x, out extents_y, out extents_w, out extents_h)

          GlyphMeasurement.new(
            advance: (advance/FT_UNIT).to_f32,
            extents: Rect[
              x: extents_x/FT_UNIT,
              y: -(extents_y/FT_UNIT),
              w: extents_w/FT_UNIT,
              h: extents_h/FT_UNIT,
            ],
          )
        end
      end

      # Calculates the *spacing* for this font at *size*, which is defined as
      # the width (advance) of the whitespace character at that size.
      def spacing(size : FontSize) : Magnitude
        measurement = measure(index(' '), size)
        measurement.advance
      end

      # Returns the `FontMetrics` for this font at *size*.
      def metrics(size : FontSize) : FontMetrics
        @metrics.put_if_absent(size) do
          assert FreeType.set_char_size(@ft_face, size.value * FT_UNIT, 0, 0, 0).zero?

          FreeType.get_font_metrics(@ft_face, out ascent, out descent, out line_gap)

          FontMetrics.new(
            (ascent / FT_UNIT).to_f32,
            (descent / FT_UNIT).to_f32,
            (line_gap / FT_UNIT).to_f32,
          )
        end
      end

      def inspect(io)
        io << "font(" << @digest[...5].hexstring << "):0x" << object_id.to_s(base: 16)
      end

      def_equals_and_hash @digest
    end

    alias PvgImage = PvgRasterImage | PvgSvgImage

    # Represents a raster image. Points to the underlying PlutoVG surface, which
    # will therefore be reused for all instances of the image.
    class PvgRasterImage
      getter digest : Bytes
      getter size : Point

      # :nodoc:
      def initialize(@surface : PlutoVG::Surface, @digest)
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

      def inspect(io)
        io << "#<PvgRasterImage:0x"
        object_id.to_s(io, base: 16)
        io << " " << @digest.hexstring
        io << " [" << @size.x << " x " << @size.y << "]>"
      end

      def_equals_and_hash @digest
    end

    # Represents an SVG image.
    #
    # It looks like PlutoSVG documents are expended by rendering them; therefore,
    # `document` yields a new document every time it is called. Only the underlying
    # SVG bytes (markup) are reused.
    class PvgSvgImage
      getter digest : Bytes
      getter size : Point

      # :nodoc:
      #
      # WARNING: The caller must ensure that *data* is a valid SVG document (such that
      # `PlutoSVG.document_load_from_data` is guaranteed to succeed under nominal conditions).
      def initialize(@data : Bytes, @size : Point, @digest)
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

      def inspect(io)
        io << "#<PvgSvgImage:0x"
        object_id.to_s(io, base: 16)
        io << " " << @digest.hexstring
        io << " [" << @size.x << " x " << @size.y << "]>"
      end

      def_equals_and_hash @digest
    end

    # Represents a map of codepoint names to codepoints, the result of parsing
    # a `.codepoint` file.
    class CodepointsMap
      # :nodoc:
      def initialize(@map : Hash(String, Char), @digest : Bytes)
      end

      # Returns the codepoint with the given *name*.
      def []?(name : String) : Char?
        @map[name]?
      end

      def inspect(io)
        io << "#<CodepointsMap:0x"
        object_id.to_s(io, base: 16)
        io << " " << @digest.hexstring
        io << " [" << @map.size << " codepoint(s)" << "]>"
      end

      def_equals_and_hash @digest
    end

    # Media types supported by `Font`. See also: `PantoMIME`.
    MEDIA_TYPES_FONT = {
      Term["font/ttf"],
      Term["font/otf"],
    }

    @@ft : FreeType::Library? = nil

    # :nodoc:
    def parse(query : FontQuery, blob : Term::Blob) : Outcome::Accepted(Font?)
      classif = Term::Blob.classif(blob)

      response = pass do
        next unless classif.media_type.in?(MEDIA_TYPES_FONT)

        ft = @@ft ||= begin
          status = FreeType.init_freetype(out library)
          unless status.zero?
            abort "failed to initialize FreeType"
          end
          library
        end

        ttcindex = 0
        status = FreeType.new_memory_face(ft, blob.bytes, blob.bytes.size, ttcindex, out ft_face)
        next unless status.zero?

        Font.new(ft_face, blob.digest)
      end

      unless response
        return Outcome.ok_despite(nil.as(Font?), <<-MSG)
        unrecognized or malformed font with media type #{classif.media_type.to(String)}; \
        expected one of: #{MEDIA_TYPES_FONT.join(", ", &.to(String))}
        MSG
      end

      Outcome.ok(response.as(Font?))
    end

    # Media types supported by `PvgRasterImage`. See also: `PantoMIME`.
    MEDIA_TYPES_RASTER = {
      Term["image/png"],
      Term["image/jpeg"],
      Term["image/bmp"],
      Term["image/gif"],
      Term["image/x-portable-pixmap"],
    }

    # :nodoc:
    def parse(query : ImageQuery, blob : Term::Blob) : Outcome::Accepted(PvgRasterImage?)
      classif = Term::Blob.classif(blob)

      response = pass do
        next unless classif.media_type.in?(MEDIA_TYPES_RASTER)
        next unless surface = PlutoVG.surface_load_from_image_data(blob.bytes, blob.bytes.size)

        PvgRasterImage.new(surface, blob.digest)
      end

      unless response
        return Outcome.ok_despite(nil.as(PvgRasterImage?), <<-SVG)
         unrecognized or malformed image with media type #{classif.media_type.to(String)}; \
         expected one of #{MEDIA_TYPES_RASTER.join(", ", &.to(String))}
         SVG
      end

      Outcome.ok(response.as(PvgRasterImage?))
    end

    # Media types supported by `PvgSvgImage`. See also: `PantoMIME`.
    MEDIA_TYPES_SVG = {Term["image/svg+xml"]}

    # Constructs a `PvgSvgImage` asset from an in-memory *blob*. Returns `nil` if
    # *blob* is not a valid (supported) SVG image.
    def svg?(blob : Term::Blob) : PvgSvgImage?
      return unless document = PlutoSVG.document_load_from_data(blob.bytes, blob.bytes.size, 0, 0, nil, nil)

      size = Point[0, 0]
      if PlutoSVG.document_extents(document, nil, out extents)
        size = Point[extents.w, extents.h]
      end

      PvgSvgImage.new(blob.bytes, size, blob.digest)
    end

    # Same as `svg?`, but raises `ArgumentError` instead of returning `nil` if *blob* is
    # not a valid (supported) SVG image.
    def svg(blob : Term::Blob) : PvgSvgImage
      svg?(blob) || raise ArgumentError.new
    end

    # :nodoc:
    def parse(query : SvgQuery, blob : Term::Blob) : Outcome::Accepted(PvgSvgImage?)
      classif = Term::Blob.classif(blob)

      response = pass do
        next unless classif.media_type.in?(MEDIA_TYPES_SVG)

        svg?(blob)
      end

      unless response
        return Outcome.ok_despite(nil.as(PvgSvgImage?), <<-SVG)
        unrecognized or malformed SVG with media type #{classif.media_type.to(String)}; \
        expected one of #{MEDIA_TYPES_SVG.join(", ", &.to(String))}"
        SVG
      end

      Outcome.ok(response.as(PvgSvgImage?))
    end

    # :nodoc:
    def parse(query : CodepointsQuery, blob : Term::Blob) : Outcome::Accepted(CodepointsMap)
      Outcome.accumulate do |acc|
        map = {} of String => Char
        lineno = 1

        io = IO::Memory.new(blob.bytes)
        io.each_line do |line|
          parts = line.split(' ', limit: 2, remove_empty: true)
          unless parts.size == 2
            acc << Diagnostic.of("line #{lineno} does not contain a codepoint definition")
            next
          end

          name, hexcode = parts
          unless codepoint = hexcode.to_i?(base: 16)
            acc << Diagnostic.of("codepoint on line #{lineno} is not a valid hex number")
            next
          end

          map[name] = codepoint.chr
        ensure
          lineno += 1
        end

        Outcome.ok(CodepointsMap.new(map, blob.digest))
      end
    end

    alias Map = Hash(Query, Any)

    defrecord QueryRef, query : Query, includes: {Diagnostic::Spot}

    defrecord Pending,
      query : Query,
      promise : Promise(ResourceService::Response)
  end

  # Schedules the loading of assets queried by the given `QuerySet`.
  #
  # See also: `poll`.
  def schedule(queries : QuerySet) : Slice(Asset::Pending)
    queries.to_readonly_slice do |query|
      Asset::Pending.new(query, promise: ResourceService.get(query.resource))
    end
  end

  private def poll(cache, pending : Slice(Asset::Pending)) : Outcome::Accepted(Asset::Map)
    assets = {} of Asset::Query => Asset::Any

    Outcome.accumulate do |acc|
      pending.each do |entry|
        result = entry.promise.poll?

        case result
        in Nil
          # Pending
          next
        in Promise::Accepted
          response = result.object
        in Promise::Rejected
          # This is caused by Crystal-side rejections of some sort.
          acc << Outcome.elaborate(Asset::QueryRef.new(entry.query), Diagnostic.of("could not load asset: #{result.detail}"))
          next
        end

        case response
        in ResourceService::Absent
          # This is caused by things like HTTP 4xx or file system ENOENT.
          acc << Outcome.elaborate(Asset::QueryRef.new(entry.query), Diagnostic.of("could not load asset: #{response.detail}"))
        in ResourceService::Present
          parseout = cache.put_if_absent({entry.query, response.content}) do
            Asset.parse(entry.query, response.content).map(&.as(Asset::Any?))
          end

          asset = acc.unwrap(Outcome.elaborate(Asset::QueryRef.new(entry.query), parseout))
          next unless asset

          assets[entry.query] = asset
        end
      end

      Outcome.ok(assets)
    end
  end

  # Polls the resource server for *pending* queries. Queries that are *currently*
  # resolved (either as present, or absent) are added to the resulting asset map.
  #
  # Notes related to *queries* are attached as diagnostics to the outcome.
  def poll(cache : CacheSet, pending : Slice(Asset::Pending)) : Outcome::Accepted(Asset::Map)
    cache.assets.epoch { poll(cache.assets, pending) }
  end
end
