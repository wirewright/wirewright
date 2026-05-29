module Ww::Scenery
  # :nodoc:
  FT_UNIT = 64

  # :nodoc:
  RQ_GLYPH_INDEX_NOTDEF = 0

  # :nodoc:
  defrecord ShapedGlyph,
    cluster : Int32,
    index : Int32,
    advance : Point,
    offset : Point

  struct ShapedGlyph
    def missing? : Bool
      index == RQ_GLYPH_INDEX_NOTDEF
    end
  end

  # Uses a Raqm handle *raqm* and *font* to shape *item*. Returns the resulting `ShapedGlyph`s.
  #
  # This is the [main] place where Wirewright's graphics stack talks to Raqm (and thus,
  # HarfBuzz and friends).
  #
  # WARNING: Both *raqm* and *font* are modified while this function runs. It's
  # more convenient to think of each of them as a machine with knobs, and this function
  # is allowed to turn them.
  private def shape(raqm : Raqm::Handle, font : Asset::PvgFont, size : Magnitude, tracking : Unit?, item : Pf::GraphemeSeln) : Slice(ShapedGlyph)
    assert FreeType.set_char_size(font.as_ft, size * FT_UNIT, 0, 0, 0).zero?

    Raqm.clear_contents(raqm)

    # NOTE: By passing `item*` and its bytesize we lose *item*'s original byte
    # offset. That is, Raqm doesn't know item is an offset into a larger string
    # and won't do the math for us when it gives us byte indices for `cluster`...
    assert Raqm.set_text_utf8(raqm, item.to_slice, item.bytesize)
    assert Raqm.set_ft_face(raqm, font.as_ft)

    if tracking
      letter_spacing = (tracking.resolve? || tracking.resolve(font.spacing(size))) * FT_UNIT

      assert Raqm.set_letter_spacing_range(raqm, letter_spacing, 0, item.bytesize)
    end

    assert Raqm.layout(raqm)

    glyphs_ptr = Raqm.get_glyphs(raqm, out glyphs_size)
    assert glyphs_size.zero? || !glyphs_ptr.null?

    glyphs = Slice.new(glyphs_ptr, glyphs_size)
    glyphs.to_readonly_slice do |glyph|
      ShapedGlyph.new(
        # ... so we have to offset the cluster manually.
        cluster: item.byte_start + glyph.cluster,
        index: glyph.index.to_i,
        advance: Point[
          Magnitude.new(glyph.x_advance) / FT_UNIT,
          Magnitude.new(glyph.y_advance) / FT_UNIT,
        ],
        offset: Point[
          Magnitude.new(glyph.x_offset) / FT_UNIT,
          Magnitude.new(glyph.y_offset) / FT_UNIT,
        ],
      )
    end
  end

  # :nodoc:
  defrecord GlyphTrain, cluster : Int32, glyphs : Slice(ShapedGlyph)

  # :nodoc:
  defrecord ShapedSemiStyledGlyph,
    info : ShapedGlyph,
    font : Asset::PvgFont,
    size : Magnitude,
    measurement : GlyphMeasurement

  private def shape(raqm : Raqm::Handle, fonts : Slice(Asset::PvgFont), size : Magnitude, tracking : Unit?, item : Pf::GraphemeSeln, &fn : ShapedSemiStyledGlyph ->)
    assert fonts.present?

    return if item.empty?

    font = fonts.first
    glyphs = shape(raqm, font, size, tracking, item)

    # Group adjacent glyphs belonging to the same cluster together into
    # a "glyph train".
    glyph_trains = glyphs.adjoin_by(&.cluster).map do |segment|
      # All segments are nonempty after adjoin.
      # All glyphs of segment will have the same cluster after adjoin.
      GlyphTrain.new(segment.first.cluster, segment)
    end

    # Determine which glyph trains are present and which ones are absent when
    # rendered using *font*.
    glyph_presence = glyph_trains.map do |train|
      fonts.size == 1 || train.glyphs.none?(&.missing?)
    end

    # Group adjacent present/absent glyph trains.
    glyph_trains_by_presence = glyph_trains.adjoin_by do |_, index|
      glyph_presence[index]
    end

    present = glyph_presence.first

    glyph_trains_by_presence.each do |trains|
      # If absent, try shaping the underlying graphemes with a different font.
      unless present
        byte_start = trains.min_of(&.cluster)
        byte_end = trains.max_of(&.cluster)
        byte_view = item.byte_select_abs_inclusive(byte_start, byte_end)
        shape(raqm, fonts + 1, size, tracking, byte_view, &fn)
        next
      end

      # Otherwise, emit final shaped glyphs.
      trains.each do |train|
        train.glyphs.each do |glyph|
          measurement = font.measure(glyph.index, size)

          fn.call(ShapedSemiStyledGlyph.new(glyph, font, size, measurement))
        end
      end
    ensure
      # Adjoin in glyph_trains_by_presence guarantees it's either true-false-true-false-...
      # or false-true-false-true-... (i.e., alternating).
      present = !present
    end
  end

  private def shape!(raqm : Raqm::Handle, fonts : Slice(Asset::PvgFont), size : Magnitude, tracking : Unit?, item : String) : Slice(ShapedSemiStyledGlyph)
    glyphs = Pf::Kit.stack_array(ShapedSemiStyledGlyph, 8)

    shape(raqm, fonts, size, tracking, Pf::GraphemeSeln.new(item)) do |glyph|
      glyphs << glyph
    end

    glyphs.to_readonly_slice(&.itself)
  end

  # :nodoc:
  #
  # A high-level representation for the input to the shaping algorithm.
  defrecord ShapeInput,
    fonts : Slice(Asset::PvgFont),
    size : Magnitude,
    tracking : Unit?,
    item : String

  private def shape(cache, raqm : Raqm::Handle, conf : ShapeInput) : Slice(ShapedSemiStyledGlyph)
    cache.shaped_items.put_if_absent(conf) do
      shape!(raqm, conf.fonts, conf.size, conf.tracking, conf.item)
    end
  end

  # :nodoc:
  defrecord GlyphFrag, text : Pf::GraphemeSeln, glyph : ShapedSemiStyledGlyph
  # :nodoc:
  defrecord EndlFrag, text : Pf::GraphemeSeln

  # IMPORTANT: must map graphemes 1:1. Otherwise everything downstream will break.
  private def pretty_repr(seln : Pf::GraphemeSeln) : String
    if seln.empty?
      return ""
    end

    repr = seln.to_s
    repr.gsub do |chr|
      case chr.ord
      when 0..0x1F then chr + 0x2400
      when 0x7F    then 0x2421.chr
      else
        chr
      end
    end
  end

  # Describes a decoration attached to a particular shaped glyph.
  defrecord GlyphDecoration,
    spec : Decoration,
    anchor_to_left : Bool,
    anchor_to_right : Bool

  # Splits *line* by one or more whitespace characters. Whitespace sticks to what
  # came before it (if anything). We do this primarily to make less calls to
  # the shaper and cache more, although whether this actually benefits us is
  # yet to be determined.
  private def chunkify(line : Pf::GraphemeSeln, &) : Nil
    unless line.size == line.bytesize
      # Don't risk it on weird-looking strings. The shaper always knows better.
      yield line
      return
    end

    line.split(' ') do |chunk|
      yield chunk
    end
  end

  private def shape(cache, raqm : Raqm::Handle, node : Text) : ShapedNode
    # Process the caption into a sequence of glyph fragments or [line]
    # break fragments.
    frags = [] of GlyphFrag | EndlFrag
    metrics = node.font_stack.first.metrics(node.size)

    # Split caption by lines.
    node.caption.byte_mask_split(node.unibreaks, :must_break) do |line, br|
      unless br == "\n"
        line += br
      end

      chunkify(line) do |frag|
        repr = pretty_repr(frag)
        repr_seln = Pf::GraphemeSeln.new(repr)

        shape_input = ShapeInput.new(node.font_stack, Asset::PvgFont.clamp(node.size), node.tracking, repr)

        glyphs = shape(cache, raqm, shape_input)
        glyphs.each do |glyph|
          # Glyph's cluster byte index points into the repr. The repr may have a different
          # byte size than frag. We want however to set the actual character, the one in
          # frag, not its repr, as the frag text. We do that by converting glyph cluster to
          # grapheme index in repr. Then comes the invariant that repr() is pure 1:1 substitution.
          # This means we can now get the frag grapheme.
          repr_text = repr_seln.at_byte(glyph.info.cluster)
          frag_text = frag.at(repr_text.begin)
          frags << GlyphFrag.new(frag_text, glyph)
          metrics = FontMetrics.max(metrics, glyph.font.metrics(glyph.size))
        end
      end

      next if br.empty?

      frags << EndlFrag.new(br)
    end

    # Now that we have the fragments, we need to figure out which decorations to
    # attach to each glyph. We cannot yet merge decorations in any sensible way,
    # because we've still got line wrapping ahead. We can only work at the glyph
    # level right now.
    seq = [] of ShapedItem

    frags.each do |frag|
      assert frag.text.size == 1

      ibeam : Selection? = nil
      decorations = Pf::Kit.stack_array(GlyphDecoration, 2)

      node.decorations.each do |decoration|
        if frag.text.begin.in?(decoration.range)
          anchor_to_left = frag.text.begin == decoration.anchor
          anchor_to_right = frag.text.end == decoration.anchor
          decorations << GlyphDecoration.new(decoration, anchor_to_left, anchor_to_right)
          next
        end

        next unless decoration.is_a?(Selection)
        next unless decoration.range.empty?

        if frag.text.begin == decoration.range.begin
          assert ibeam.nil?
          ibeam = decoration
        end
      end

      if ibeam
        seq << IBeam.new(ibeam)
      end

      if frag.is_a?(EndlFrag)
        seq << Endl.new(frag.text.begin, decorations.to_readonly_slice)
        next
      end

      unibreak = node.unibreaks[frag.text.byte_start]

      case unibreak
      in .must_break?
        # We emit virtual Endl breaks above. MustBreak here is simply an annoyance left
        # from invisible '\n' glyphs.
        break_policy = BreakPolicy::NoBreak
      in .allow_break?
        break_policy = frag.text == " " ? BreakPolicy::Collapse : BreakPolicy::Preserve
      in .no_break?, .unfinished?, .indeterminate?
        break_policy = BreakPolicy::NoBreak
      end

      seq << ShapedStyledGlyph.new(
        font: frag.glyph.font,
        grapheme_index: frag.text.begin,
        glyph_index: frag.glyph.info.index,
        break_policy: break_policy,
        advance: frag.glyph.info.advance,
        offset: frag.glyph.info.offset,
        size: frag.glyph.size,
        measurement: frag.glyph.measurement,
        color: node.color,
        decorations: decorations.to_readonly_slice,
      )
    end

    # Process ibeams and selections at the end of the caption.
    ibeam = nil

    node.selections.each do |selection|
      range = selection.range
      if range.empty? && range.begin == node.caption.end
        assert ibeam.nil?
        ibeam = selection
      end
    end

    if ibeam
      seq << IBeam.new(ibeam)
    end

    ShapedText.new(node.caption,
      seq.to_readonly_slice(&.itself),
      metrics: metrics,
      line_height: node.leading.resolve(metrics.line_height),
      selections: node.selections,
    )
  end

  private def shape!(cache, node : Text) : ShapedNode
    raqm = Raqm.create

    begin
      shape(cache, raqm, node)
    ensure
      Raqm.destroy(raqm)
    end
  end

  private def shape!(cache, node : Content | Floating | Limit | Clamp | Padding | Align | XYStack | ZStack | XYWrap | Composite | Transform | Viewport | Aim | Page | Overlay | Variant | Vantage | Gate) : ShapedNode
    children = node.children.to_readonly_slice { |child| shape(cache, child).as(ShapedNode) }

    node.copy_with(children: children)
  end

  private def shape(cache, node : Inert | RectShape | Pending | Img | Svg | IconGlyph) : ShapedNode
    node
  end

  private def shape(cache, node : Text | Content | Floating | Limit | Clamp | Padding | Align | XYStack | ZStack | XYWrap | Composite | Transform | Viewport | Aim | Page | Overlay | Variant | Vantage | Gate) : ShapedNode
    cache.shaping.put_if_absent(node) { shape!(cache, node) }
  end

  # Performs *text shaping* among other text- and font-related related rewrites
  # on *root*. Returns the resulting shaped root.
  #
  # References:
  #
  # - [Raqm](https://host-oman.github.io/libraqm/)
  # - [HarfBuzz](https://harfbuzz.github.io/)
  # - [FreeType](https://freetype.org/)
  def shape(cache : CacheSet, root : Root(AssetNode)) : Root(ShapedNode)
    cache.shaping.epoch do
      cache.shaped_items.epoch do
        Root(ShapedNode).new(shape(cache, root.node))
      end
    end
  end
end
