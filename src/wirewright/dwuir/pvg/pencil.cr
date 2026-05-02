module Ww::DwUIR
  # Encapsulates configuration for a `PvgPencil`.
  #
  # These do not change between pencil instances so it's better to keep them behind
  # a pointer vs. copying them with the pencil all the time.
  defcase PvgPencilConfig,
    face : PvgFontFace,
    size : Float32,
    wsstep : Float32,
    letter_spacing : Float32,
    line_height : Float32

  # An `IPencil` implementation that uses PlutoVG.
  struct PvgPencil
    include IPencil

    getter tip : Point

    # :nodoc:
    def initialize(@config : PvgPencilConfig, @tip : Point, @state : Char)
    end

    def self.new(face : PvgFontFace, size : Float32, leading = Magn.rel(1.0), tracking = Magn.rel(0.0))
      PlutoVG.face_get_glyph_metrics(face, size, ' '.ord, out wswidth0, nil, nil)

      letter_spacing = tracking.resolve(wswidth0)
      line_height = leading.resolve(size)
      wswidth = wswidth0 + letter_spacing

      config = PvgPencilConfig.new(face, size, wswidth, letter_spacing, line_height)

      new(config, tip: Point.new(0.0f32, size), state: '\0')
    end

    def origin : Point
      @tip - Point.new(0, @config.size)
    end

    def size : Float32
      @config.size
    end

    def tip(ch : Char) : Point
      @tip + Point.new(kerning(ch) + letter_spacing(ch), 0)
    end

    private def letter_spacing(ch : Char) : Float32
      if @state.in?('\0', '\n') # beginning of text or line
        return 0.0f32
      end

      @config.letter_spacing
    end

    private def kerning(ch : Char) : Float32
      @config.face.kerning(@state, ch, @config.size)
    end

    # :nodoc:
    def_copy_with

    def after_writing(ch : Char) : PvgPencil
      case ch
      when '\r', '\0'
        return self
      when '\n'
        return copy_with(state: ch, tip: Point.new(0.0f32, @tip.y + @config.line_height))
      when ' '
        return copy_with(state: ch, tip: @tip + Point.new(@config.wsstep, 0))
      when '\t'
        return copy_with(state: ch, tip: @tip + Point.new(@config.wsstep * 4, 0))
      end

      # NOTE: I'm not sure this way to measure things is correct; this is
      # the only one out of the ones I've tried that *looks* correct; although
      # obviously if we compare to e.g. browsers, it looks somewhat wrong (esp.
      # without ligatures etc., but I think the way we measure here is also
      # a problem.)

      PlutoVG.face_get_glyph_metrics(@config.face, @config.size, ch.ord, out advance, nil, out extents)

      step = Point.new(kerning(ch) + letter_spacing(ch) + advance, 0)

      copy_with(state: ch, tip: @tip + step)
    end
  end

  # Wrapper around a PlutoVG font face.
  class PvgFontFace
    @face : PlutoVG::FontFace

    def initialize(path : NormalPath)
      unless path.absolute?
        raise ArgumentError.new("expected an absolute path")
      end

      face = PlutoVG.face_from_file(path.to_s, ttcindex: 0)
      if face.null?
        raise ArgumentError.new("unable to load font face")
      end

      @face = face
    end

    def finalize
      PlutoVG.face_destroy(@face)
    end

    def to_unsafe
      @face
    end

    # Returns a `PvgPencil` that uses this font face.
    def pencil(*args, **kwargs) : PvgPencil
      PvgPencil.new(self, *args, **kwargs)
    end

    # Sets the capacity of the kerning cache for this font face.
    KERNING_CACHE_CAPACITY = 32 * 32

    @kerning = {} of {Char, Char, Float32} => Float32

    # Returns  the kerning between *ch1* and *ch2* using the given font size *size*.
    def kerning(ch1 : Char, ch2 : Char, size : Float32)
      if cached = @kerning[{ch1, ch2, size}]?
        return cached
      end

      if @kerning.size + 1 > KERNING_CACHE_CAPACITY
        @kerning.delete(@kerning.first_key)
      end

      PlutoVG.face_get_kerning(@face, size, ch1.ord, ch2.ord, out value)

      @kerning[{ch1, ch2, size}] = value.to_f32
    end
  end

  # Serves `PencilRequests` by loading & constructing `PvgFontFace`s
  # and `PvgPencil`s. Acts as a cache for `PvgFontFace`s.
  class PvgFontFaceStore
    # Returns the pencil request handling proc.
    getter pencils : PencilServer

    def initialize
      @faces = {} of NormalPath => PvgFontFace
      @pencils = ->(request : PencilRequest) do
        face = face(request.font)
        face.pencil(request.size, request.leading, request.tracking).as(IPencil)
      end
    end

    # Returns the cached `PvgFontFace` for *font*; or loads it from disk.
    def face(font : NormalPath) : PvgFontFace
      @faces.put_if_absent(font) { PvgFontFace.new(font) }
    end
  end
end
