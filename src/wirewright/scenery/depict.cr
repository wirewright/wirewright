module Ww::Scenery
  # NOTE: All commands respond to `#bounds`, which returns their *visual* bounds.
  # At this point, layout (`Box`) bounds do not exist anymore; nor is the shape of
  # the draw command tree anything like the `depict`ed node tree.
  alias DrawCommand = DrawImage |
                      DrawSvg |
                      DrawGlyph |
                      DrawRoundedRect |
                      DrawRoundedRectFrame |
                      DrawRect |
                      DrawSeq |
                      DrawTransform |
                      DrawOpacity |
                      DrawClip

  # Draws a raster image at origin.
  defcase DrawImage,
    image : Asset::PvgRasterImage,
    fit : Img::Fit::Any,
    tile : Bool,
    opacity : Magnitude,
    rect : RoundedRect,
    target_size : Point,
    caches_hash: true

  class DrawImage
    def bounds : Rect
      rect.bounds
    end
  end

  # Draws an SVG at origin.
  defcase DrawSvg,
    image : Asset::PvgSvgImage,
    fit : Svg::Fit,
    color : Pigment::RGBA,
    bounds : Rect,
    caches_hash: true

  # Draws a glyph using *font* of the given pixel *size* at origin.
  #
  # - *index* is the glyph index in the font. See for instance `Asset::PvgFont#glyph_index`.
  # - *color* is the color to paint the glyph with.
  #
  # Notably:
  # - *pen* specifies the position of the pen. Put simply, this is where `PlutoVG.add_glyph_*`
  #   and similar should be called.
  # - *bounds*, on the other hand, is the bounding box of the glyph outline. This
  #   includes things like the overhang in an italic, serif "f". *bounds* is used
  #   to do hit testing and visibility testing.
  defcase DrawGlyph,
    font : Asset::PvgFont,
    index : Int32,
    size : Magnitude,
    color : Pigment::RGBA,
    pen : Point,
    bounds : Rect,
    caches_hash: true

  # Draws a rounded rectangle frame at origin.
  defcase DrawRoundedRectFrame,
    outer : RoundedRect,
    inner : RoundedRect,
    fill : Paint::Any,
    caches_hash: true

  class DrawRoundedRectFrame
    def bounds : Rect
      outer.bounds
    end
  end

  # Draws a rounded rectangle at origin.
  defcase DrawRoundedRect,
    rect : RoundedRect,
    fill : Paint::Any,
    caches_hash: true

  class DrawRoundedRect
    def bounds : Rect
      rect.bounds
    end
  end

  # Draws a rectangle at origin.
  defcase DrawRect,
    bounds : Rect,
    fill : Paint::Any,
    caches_hash: true

  # Applies a transform matrix `Tf` before drawing *child*.
  defcase DrawTransform,
    child : DrawCommand,
    tf : Tf,
    bounds : Rect,
    caches_hash: true

  class DrawTransform
    def self.new(child : DrawCommand, tf : Tf)
      # Collapse nested transforms.
      if child.is_a?(DrawTransform)
        return new(child.child, Tf[tf, child.tf], bounds: tf.map(child.bounds))
      end

      new(child, tf, bounds: tf.map(child.bounds))
    end
  end

  # Applies *opacity* to a *child*.
  defcase DrawOpacity,
    child : DrawCommand,
    opacity : Magnitude,
    caches_hash: true

  class DrawOpacity
    def bounds
      child.bounds
    end
  end

  # Clips *child* according to the rounded rect *visible*.
  defcase DrawClip,
    child : DrawCommand,
    visible : RoundedRect,
    bounds : Rect,
    caches_hash: true

  class DrawClip
    def self.new(child : DrawCommand, visible : RoundedRect)
      new(child, visible, bounds: Rect.intersection(child.bounds, visible.bounds))
    end
  end

  # Represents a sequence of draw commands.
  defcase DrawSeq,
    children : Slice(DrawCommand),
    bounds : Rect,
    caches_hash: true

  class DrawSeq
    @@empty : DrawSeq?

    def self.empty : DrawSeq
      @@empty ||= new(Slice(DrawCommand).empty, bounds: Rect.empty)
    end

    def self.new(*ixs : Indexable(DrawCommand)) : DrawCommand
      children = Pf::Kit.stack_array(DrawCommand)
      bounds = Rect.empty

      ixs.each do |ix|
        ix.each do |command|
          # Flatten nested DrawSeqs. There is very little point in keeping them
          # nested like this.
          if command.is_a?(DrawSeq)
            children.concat(command.children)
          else
            children << command
          end

          bounds = Rect.union(bounds, command.bounds)
        end
      end

      new(children.to_unsafe_readonly_slice!, bounds)
    end

    def self.new(children : Slice(DrawCommand)) : DrawCommand
      if child = children.single?
        return child
      end

      new({children})
    end

    def self.new(*commands : DrawCommand) : DrawCommand
      new(commands)
    end
  end

  private def depict!(cache, node : Inert, box : OriginBox) : DrawCommand
    DrawSeq.empty
  end

  private def depict!(cache, node : RectShape, box : OriginBox) : DrawCommand
    thickness, outer = node.resolve(box.bounds)

    inner = RoundedRect.new(
      bounds: box.bounds.pad(thickness),
      radii: {
        tl: Math.max(outer.tl - Math.max(thickness[:t], thickness[:l]), Magnitude.new(0)),
        tr: Math.max(outer.tr - Math.max(thickness[:t], thickness[:r]), Magnitude.new(0)),
        br: Math.max(outer.br - Math.max(thickness[:b], thickness[:r]), Magnitude.new(0)),
        bl: Math.max(outer.bl - Math.max(thickness[:b], thickness[:l]), Magnitude.new(0)),
      }
    )

    if Paint.transparent?(node.stroke)
      return DrawRoundedRect.new(inner, node.fill)
    end

    if Paint.transparent?(node.fill)
      return DrawRoundedRectFrame.new(outer, inner, node.stroke)
    end

    DrawSeq.new(
      DrawRoundedRectFrame.new(outer, inner, node.stroke),
      DrawRoundedRect.new(inner, node.fill),
    )
  end

  private def depict!(cache, node : Pending, box : OriginBox) : DrawCommand
    blame = node.blame
    placeholder = blame.placeholder || RectShape.new(
      thickness: RectThickness.new(l: Unit.px(0.0), t: Unit.px(0.0), r: Unit.px(0.0), b: Unit.px(0.0)),
      radii: RectRadii.new(tl: Unit.px(15.0), tr: Unit.px(15.0), bl: Unit.px(15.0), br: Unit.px(15.0)),
      fill: Paint::Solid.new(Pigment::RGBA.new(0.125, 0.125, 0.125, 1.0)),
      stroke: Paint::Solid.new(Pigment::RGBA.new(0.0, 0.0, 0.0, 0.0)),
    )

    unless blame.is_a?(Text)
      return depict(cache, placeholder, box)
    end

    x = 0
    y = 0
    commands = Pf::Kit.stack_array(DrawCommand, 8)
    line_height = blame.leading.resolve(blame.size)

    line_wrap(blame, at: box.bounds.w) do |line|
      if y > 0
        y += TEXT_PLACEHOLDER_LINE_GAP
      end

      commands << depict(cache, placeholder, Rect[x, y, line.advance, line_height])

      x = 0
      y += line_height
    end

    DrawSeq.new(commands)
  end

  private def depict!(cache, node : ShapedText, box : OriginBox) : DrawCommand
    TextDepictionMachine.depict(node, box)
  end

  # :nodoc:
  struct TextDepictionMachine
    defrecord SelectionSegment,
      offset : Magnitude,
      advance : Magnitude,
      copying: true

    defrecord UnderlineSegment,
      offset : Magnitude,
      advance : Magnitude,
      color : Pigment::RGBA,
      copying: true

    private def initialize(
      @node : ShapedText,
      @box : OriginBox,
      @cursors : Pf::Kit::HybridArray(DrawCommand, 4),
      @selections : Pf::Kit::HybridArray(DrawCommand, 2),
      @underlines : Pf::Kit::HybridArray(DrawCommand, 2),
      @glyphs : Pf::Kit::HybridArray(DrawCommand, 64),
    )
      @x = Magnitude.new(0)
      @y = Magnitude.new(0)
    end

    def self.depict(node : ShapedText, box : OriginBox)
      cursors = Pf::Kit.stack_array(DrawCommand, 4)
      selections = Pf::Kit.stack_array(DrawCommand, 2)
      underlines = Pf::Kit.stack_array(DrawCommand, 2)
      glyphs = Pf::Kit.stack_array(DrawCommand, 64)

      machine = new(node, box, cursors, selections, underlines, glyphs)
      machine.depict!
      machine.composite!
    end

    private def adjacent?(a : UnderlineSegment, b : UnderlineSegment) : Bool
      return false unless a.color == b.color

      (a.offset + a.advance).approx?(b.offset)
    end

    private def adjacent?(a : SelectionSegment, b : SelectionSegment) : Bool
      (a.offset + a.advance).approx?(b.offset)
    end

    private def join(a : Segment, b : Segment) forall Segment
      a.copy_with(advance: a.advance + b.advance)
    end

    private def each_contiguous_segment(segments : Indexable(SelectionSegment) | Indexable(UnderlineSegment), &) : Nil
      index = 0

      while segment0 = segments[index]?
        index += 1

        while segment1 = segments[index]?
          break unless adjacent?(segment0, segment1)

          segment0 = join(segment0, segment1)
          index += 1
        end

        yield segment0
      end
    end

    @selection_segments : Hash(Selection, Deque(SelectionSegment))?
    @underline_segments : Hash(Underline, Deque(UnderlineSegment))?

    private def submit(spec : Selection, segment : SelectionSegment) : Nil
      buckets = @selection_segments
      if buckets.nil?
        buckets = {} of Selection => Deque(SelectionSegment)
        buckets.compare_by_identity
        @selection_segments = buckets
      end

      bucket = buckets.put_if_absent(spec) { Deque(SelectionSegment).new }
      bucket << segment
    end

    private def submit(spec : Underline, segment : UnderlineSegment) : Nil
      buckets = @underline_segments
      if buckets.nil?
        buckets = {} of Underline => Deque(UnderlineSegment)
        buckets.compare_by_identity
        @underline_segments = buckets
      end

      bucket = buckets.put_if_absent(spec) { Deque(UnderlineSegment).new }
      bucket << segment
    end

    private def process(item : IBeam) : Nil
      selection = item.selection
      bounds = Rect[@x, @y, selection.thickness, @node.line_height]

      @cursors << DrawRect.new(bounds, fill: Paint::Solid.new(selection.fill))
    end

    private def process(item : Endl) : Nil
      item.decorations.each do |decoration|
        next unless spec = decoration.spec.as?(Selection)
        next unless spec.endl

        submit(spec, SelectionSegment.new(@x, spec.endl_width))
      end
    end

    private def process(item : ShapedStyledGlyph) : Nil
      color = item.color

      item.decorations.each do |decoration|
        next unless spec = decoration.spec.as?(Selection)

        color = spec.color
        break
      end

      item.decorations.each do |decoration|
        case spec = decoration.spec
        in Selection then submit(spec, SelectionSegment.new(@x, item.advance.x))
        in Underline then submit(spec, UnderlineSegment.new(@x, item.advance.x, color))
        end
      end

      ascent = item.font.metrics(item.size).ascent

      bounds = Rect[
        x: item.measurement.extents.x + item.offset.x + @x,
        y: item.measurement.extents.y + ascent + item.offset.y + @y,
        w: item.measurement.extents.w,
        h: item.measurement.extents.h,
      ]

      @glyphs << DrawGlyph.new(
        font: item.font,
        index: item.glyph_index,
        size: item.size,
        color: color,
        pen: bounds.tl - item.measurement.extents.tl,
        bounds: bounds,
      )

      @x += item.advance.x
    end

    private def process(line : ShapedLine) : Nil
      line.items.each { |item| process(item) }
    end

    def depict! : Nil
      Scenery.line_wrap(@node, at: @box.bounds.w) do |line|
        @selection_segments.try(&.clear)
        @underline_segments.try(&.clear)

        process(line)

        if buckets = @selection_segments
          buckets.each do |decoration, bucket|
            each_contiguous_segment(bucket) do |segment|
              bounds = Rect.new(
                tl: Point[segment.offset, @y],
                size: Point[segment.advance, @node.line_height],
              )

              @selections << DrawRoundedRect.new(
                rect: RoundedRect.new(
                  bounds: Rect.map(bounds, unit: decoration.extents),
                  radius: decoration.radius,
                ),
                fill: Paint::Solid.new(decoration.fill),
              )
            end
          end
        end

        if buckets = @underline_segments
          buckets.each do |decoration, bucket|
            each_contiguous_segment(bucket) do |segment|
              # TODO: This is a toy way to compute the offset. There must be a real way
              # to read the underline-offset off of the font.
              offset = @node.metrics.ascent - @node.metrics.descent*decoration.offset

              bounds = Rect.new(
                tl: Point[segment.offset, @y + offset],
                size: Point[segment.advance, decoration.thickness],
              )

              @underlines << DrawRect.new(bounds, fill: Paint::Solid.new(decoration.fill || segment.color))
            end
          end
        end

        @x = Magnitude.new(0)
        @y += @node.line_height
      end
    end

    def composite! : DrawCommand
      DrawSeq.new(
        @selections.to_unsafe_readonly_slice!,
        @underlines.to_unsafe_readonly_slice!,
        @glyphs.to_unsafe_readonly_slice!,
        @cursors.to_unsafe_readonly_slice!,
      )
    end
  end

  private def depict!(cache, node : IconGlyph, box : OriginBox) : DrawCommand
    ascent = node.font.metrics(node.size).ascent

    DrawGlyph.new(
      font: node.font,
      index: node.glyph_index,
      size: node.size,
      color: node.color,
      pen: Point[0, ascent],
      bounds: box.bounds,
    )
  end

  private def depict!(cache, node : Img, box : OriginBox) : DrawCommand
    target_w = node.resize_w.resolve(node.src.size.x)
    target_h = node.resize_h.resolve(node.src.size.y)

    DrawImage.new(
      image: node.src,
      fit: node.fit,
      tile: node.tile,
      opacity: node.opacity,
      rect: RoundedRect.new(box.bounds, node.radii),
      target_size: Point[target_w, target_h],
    )
  end

  private def depict!(cache, node : Svg, box : OriginBox) : DrawCommand
    DrawSvg.new(
      image: node.src,
      fit: node.fit,
      color: node.color,
      bounds: box.bounds,
    )
  end

  private def depict!(cache, node : Composite, box : OriginBox) : DrawCommand
    DrawOpacity.new(depict(cache, node.children, box.children), node.opacity)
  end

  private def depict!(cache, node : TransformMatrix, box : OriginBox) : DrawCommand
    DrawTransform.new(depict(cache, node.children, box.children), tf: node.tf)
  end

  private def depict!(cache, node : Clip, box : OriginBox) : DrawCommand
    DrawClip.new(
      child: DrawTransform.new(
        child: depict(cache, node.children, box.children),
        tf: Tf[Tf.translate(-node.offset)],
      ),
      visible: RoundedRect.new(box.bounds, node.radii),
    )
  end

  private def depict!(cache, node : Content | Floating | Limit | Padding | Align | XYStack | ZStack | Observer | Observable | Gate, box : OriginBox) : DrawCommand
    depict(cache, node.children, box.children)
  end

  private def depict(cache, node : AimedNode, box : OriginBox) : DrawCommand
    cache.put_if_absent({node, box}) { depict!(cache, node, box) }
  end

  private def depict(cache, node : AimedNode, box : Box) : DrawCommand
    draw_command = depict(cache, node, OriginBox.new(box.bounds.size, box.children))
    if box.at_origin?
      return draw_command
    end

    DrawTransform.new(draw_command, tf: Tf[Tf.translate(box.bounds.tl)])
  end

  private def depict(cache, node : Inert | RectShape | Pending | Img | Svg | IconGlyph | ShapedText, bounds : Rect) : DrawCommand
    depict(cache, node, Box.new(bounds, children: Slice(Box).empty))
  end

  private def depict(cache, nodes : Slice(AimedNode), boxes : Slice(Box)) : DrawCommand
    draw_commands = Pf::Kit.stack_array(DrawCommand)

    nodes.zip(boxes) do |node, box|
      draw_command = depict(cache, node, box)
      draw_commands << draw_command
    end

    DrawSeq.new(draw_commands)
  end

  # Converts *root* and its corresponding *box* to a tree of draw commands.
  # Returns the root draw command.
  def depict(cache : CacheSet, root : Root(AimedNode), box : OriginBox) : DrawCommand
    cache.depict.epoch { depict(cache.depict, root.node, box) }
  end
end
