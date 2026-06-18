module Ww::Scenery
  # See `Safe.diff`.
  def diff(command0 : DrawCommand, command1 : DrawCommand) : Slice(Rect)
    if command0 == command1
      return Slice(Rect).empty
    end

    diffx(command0, command1)
  end

  # The idea is to exclude the bounds of unchanged commands from the resulting
  # dirty rect, and include all other bounds (of commands removed or added/updated).
  private def idiff?(commands0 : Slice(DrawCommand), commands1 : Slice(DrawCommand)) : Slice(Rect)?
    # Hashes are usually large and usually random-ish so there's no point
    # in using e.g. USet32 here.
    #
    # Note also that all DrawCommands cache_hash so it's cheap to call #hash.
    l = Set(UInt64).new
    r = Set(UInt64).new

    commands0.each do |child|
      return unless l.add?(child.hash) # Duplicate or collision
    end

    commands1.each do |child|
      return unless r.add?(child.hash) # Duplicate or collision
    end

    # No duplicates or collisions in l and r.

    dirty_rects = Pf::Kit.stack_array(Rect, 8)

    commands0.each do |child|
      next if child.hash.in?(r)

      # Removed
      dirty_rects << child.bounds
    end

    commands1.each do |child|
      next if child.hash.in?(l)

      # Updated or created
      dirty_rects << child.bounds
    end

    dirty_rects.to_unsafe_readonly_slice!
  end

  private def diffx(command0 : DrawSeq, command1 : DrawSeq) : Slice(Rect)
    unless command0.children.size == command1.children.size
      dirty_rects = idiff?(command0.children, command1.children)
      dirty_rects ||= Slice[command0.bounds, command1.bounds]
      return dirty_rects
    end

    dirty_rects = Pf::Kit.stack_array(Rect, 8)

    command0.children.zip(command1.children) do |child0, child1|
      child_dirty_rects = diff(child0, child1)
      dirty_rects.concat(child_dirty_rects)
    end

    dirty_rects.to_unsafe_readonly_slice!
  end

  private def diffx(command0 : DrawTransform, command1 : DrawTransform) : Slice(Rect)
    unless command0.tf == command1.tf
      return Slice[command0.bounds, command1.bounds]
    end

    child_dirty_rects = diff(command0.child, command1.child)
    child_dirty_rects.map { |dirty_rect| command1.tf.map(dirty_rect) }
  end

  private def diffx(command0 : DrawOpacity, command1 : DrawOpacity) : Slice(Rect)
    unless command0.opacity == command1.opacity
      return Slice[command0.bounds, command1.bounds]
    end

    diff(command0.child, command1.child)
  end

  private def diffx(command0 : DrawClip, command1 : DrawClip) : Slice(Rect)
    unless command0.visible == command1.visible
      return Slice[command0.bounds, command1.bounds]
    end

    child_dirty_rects = diff(command0.child, command1.child)
    child_dirty_rects.map { |dirty_rect| Rect.intersection(command1.visible.bounds, dirty_rect) }
  end

  private def diffx(command0 : DrawCommand, command1 : DrawCommand) : Slice(Rect)
    Slice[command0.bounds, command1.bounds]
  end

  # See `Safe.rasterize`.
  def rasterize(screen : PixelRect, command : DrawCommand, backdrop : Pigment::RGBA) : Nil
    dirty_rects = Slice[Rect[0, 0, screen.width, screen.height]]

    rasterize(screen, command, backdrop, dirty_rects)
  end

  # :ditto:
  def rasterize(screen : PixelRect, command : DrawCommand, backdrop : Pigment::RGBA, dirty_rects : Slice(Rect)) : Nil
    return if dirty_rects.empty?

    unless surface = PlutoVG.surface_create_for_data(screen, screen.width, screen.height, screen.stride)
      raise "pvg: failed to create surface of size #{screen.width}x#{screen.height}"
    end

    unless canvas = PlutoVG.canvas_create(surface)
      raise "pvg: failed to create canvas for surface of size #{screen.width}x#{screen.height}"
    end

    begin
      dirty = Rect.empty

      dirty_rects.each do |dirty_rect|
        # Add 1px margin to conceal any float/rasterization artifacts.
        dirty_rect = dirty_rect.snap.margin(5)
        PlutoVG.canvas_add_rect(canvas, dirty_rect.x, dirty_rect.y, dirty_rect.w, dirty_rect.h)
        dirty = Rect.union(dirty, dirty_rect)
      end

      PlutoVG.canvas_clip_preserve(canvas)

      {% if flag?(:scenery_rasterize_dmg_debug) %}
        PlutoVG.canvas_set_rgba(canvas, 1.0, 0, 0, 0.1)
        PlutoVG.canvas_fill(canvas)
      {% else %}
        PlutoVG.canvas_set_rgba(canvas, *backdrop.rgba)
        PlutoVG.canvas_fill(canvas)
        rasterize(canvas, command, dirty)
      {% end %}
    ensure
      PlutoVG.canvas_destroy(canvas)
      PlutoVG.surface_destroy(surface)
    end
  end

  private def rasterize_set_paint(canvas, paint : Paint::Solid, bounds : Rect) : Nil
    PlutoVG.canvas_set_rgba(canvas, *paint.color.rgba)
  end

  private def rasterize_set_paint(canvas, paint : Paint::LinearGradient, bounds : Rect) : Nil
    stops = paint.stops.to_readonly_slice do |stop|
      PlutoVG::GradientStop.new(offset: stop.offset, color: stop.color.to_pvg)
    end

    tf = Tf.translate(bounds.tl)
    matrix = tf.to_pvg

    PlutoVG.canvas_set_linear_gradient(canvas,
      x1: paint.begin.x * bounds.w,
      y1: paint.begin.y * bounds.h,
      x2: paint.end.x * bounds.w,
      y2: paint.end.y * bounds.h,
      spread: PlutoVG::SpreadMethod::Pad,
      stops: stops,
      nstops: stops.size,
      matrix: pointerof(matrix),
    )
  end

  private def rasterize_set_paint(canvas, paint : Paint::RadialGradient, bounds : Rect)
    stops = paint.stops.to_readonly_slice do |stop|
      PlutoVG::GradientStop.new(offset: stop.offset, color: stop.color.to_pvg)
    end

    tf = Tf.translate(bounds.tl)
    matrix = tf.to_pvg

    # NOTE: the clamps here are due to the fact that for some reason, the gradient
    # blows up (visually) at 0 or 1 center/focus x/y, becomes a weird disfigured mess
    # of triangles and lines (?)
    PlutoVG.canvas_set_radial_gradient(canvas,
      cx: paint.center.x.clamp(0.01..0.99) * bounds.w,
      cy: paint.center.y.clamp(0.01..0.99) * bounds.h,
      cr: paint.center_radius * bounds.diagonal * 0.5,
      fx: paint.focus.x.clamp(0.01..0.99) * bounds.w,
      fy: paint.focus.y.clamp(0.01..0.99) * bounds.h,
      fr: paint.focus_radius * bounds.diagonal * 0.5,
      spread: PlutoVG::SpreadMethod::Pad,
      stops: stops,
      nstops: stops.size,
      matrix: pointerof(matrix),
    )
  end

  # Starts at h (top-right) and goes clockwise.
  private def rasterize_reverse_add_rrect(canvas : PlutoVG::Canvas, rect : RoundedRect) : Nil
    PlutoVG.canvas_move_to(canvas, *rect.h.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.tr2.xy, 0, false, true, *rect.g.xy)
    PlutoVG.canvas_line_to(canvas, *rect.f.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.br2.xy, 0, false, true, *rect.e.xy)
    PlutoVG.canvas_line_to(canvas, *rect.d.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.bl2.xy, 0, false, true, *rect.c.xy)
    PlutoVG.canvas_line_to(canvas, *rect.b.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.tl2.xy, 0, false, true, *rect.a.xy)
    PlutoVG.canvas_close_path(canvas)
  end

  # Starts at a (top-left) and goes counter-clockwise.
  private def rasterize_add_rrect(canvas : PlutoVG::Canvas, rect : RoundedRect) : Nil
    PlutoVG.canvas_move_to(canvas, *rect.a.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.tl2.xy, 0, false, false, *rect.b.xy)
    PlutoVG.canvas_line_to(canvas, *rect.c.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.bl2.xy, 0, false, false, *rect.d.xy)
    PlutoVG.canvas_line_to(canvas, *rect.e.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.br2.xy, 0, false, false, *rect.f.xy)
    PlutoVG.canvas_line_to(canvas, *rect.g.xy)
    PlutoVG.canvas_arc_to(canvas, *rect.tr2.xy, 0, false, false, *rect.h.xy)
    PlutoVG.canvas_close_path(canvas)
  end

  private def rasterize(canvas, command : DrawSeq, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    command.children.each { |child| rasterize(canvas, child, dirty) }
  end

  private def rasterize(canvas, command : DrawOpacity, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    PlutoVG.canvas_save(canvas)
    PlutoVG.canvas_set_opacity(canvas, command.opacity)
    rasterize(canvas, command.child, dirty)
    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawTransform, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    step_matrix = command.tf.to_pvg

    PlutoVG.canvas_save(canvas)
    PlutoVG.canvas_transform(canvas, pointerof(step_matrix))
    PlutoVG.canvas_get_matrix(canvas, out acc_matrix)

    acc_tf = Tf.new(acc_matrix)

    scale = acc_tf.scale

    # Only apply pixel alignment if scale is essentially 1.0.
    if (scale.x - 1.0).abs < 1e-5 && (scale.y - 1.0).abs < 1e-5
      # Check out where our origin lands on the screen given the accumulated transforms.
      origin = acc_tf.map(Point[0, 0])

      # Make sure we're aligned with the pixels on the screen.
      error_x = origin.x - origin.x.round
      error_y = origin.y - origin.y.round
      PlutoVG.canvas_translate(canvas, -error_x, -error_y)
    end

    # Since the dirty rect is in parent-space and we want it in child-space,
    # we use the inverse transform.
    rasterize(canvas, command.child, command.tf.inverse.map(dirty))

    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawClip, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    PlutoVG.canvas_save(canvas)
    rasterize_add_rrect(canvas, command.visible)
    PlutoVG.canvas_clip(canvas)
    rasterize(canvas, command.child, Rect.intersection(command.visible.bounds, dirty))
    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawRect, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    bounds = command.bounds.snap

    PlutoVG.canvas_save(canvas)
    PlutoVG.canvas_add_rect(canvas, bounds.x, bounds.y, bounds.w, bounds.h)
    PlutoVG.canvas_clip(canvas)
    rasterize_set_paint(canvas, command.fill, bounds)
    PlutoVG.canvas_paint(canvas)
    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawRoundedRect, dirty : Rect)
    rect = command.rect.snap

    PlutoVG.canvas_save(canvas)
    rasterize_add_rrect(canvas, rect)
    PlutoVG.canvas_clip(canvas)
    rasterize_set_paint(canvas, command.fill, rect.bounds)
    PlutoVG.canvas_paint(canvas)
    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawRoundedRectFrame, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    outer = command.outer.snap
    inner = command.inner.snap

    PlutoVG.canvas_save(canvas)
    rasterize_reverse_add_rrect(canvas, outer)
    rasterize_add_rrect(canvas, inner)
    PlutoVG.canvas_clip_preserve(canvas)
    rasterize_set_paint(canvas, command.fill, outer.bounds)
    PlutoVG.canvas_fill(canvas)
    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawImage, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    PlutoVG.canvas_save(canvas)

    rasterize_add_rrect(canvas, command.rect)
    PlutoVG.canvas_clip(canvas)

    type = PlutoVG::TextureType::Plain
    if command.tile
      type = PlutoVG::TextureType::Tiled
    end

    scale = command.target_size * command.image.size.normalized

    case fit = command.fit
    in Img::Fit::Align
      transform = Tf[
        Tf.scale(scale),
        Tf.translate(Rect.align(command.bounds, Rect.new(tl: Point[0, 0], size: command.target_size), fit.normpt).tl),
      ]
    in Img::Fit::Pan
      transform = Tf[
        Tf.scale(scale),
        Tf.translate(command.bounds.tl + fit.delta),
      ]
    in Img::Fit::Stretch
      transform = Tf[
        Tf.scale(command.bounds.size * command.target_size.normalized),
        Tf.translate(command.bounds.tl),
        Tf.scale(scale),
      ]
    end

    matrix = transform.to_pvg

    PlutoVG.canvas_set_texture(canvas, command.image, type, command.opacity, pointerof(matrix))
    PlutoVG.canvas_paint(canvas)

    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawSvg, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    PlutoVG.canvas_save(canvas)

    command.image.document(command.bounds.size) do |document|
      PlutoSVG.document_extents(document, nil, out svg_extents)

      svg_bounds = Rect[svg_extents.x, svg_extents.y, svg_extents.w, svg_extents.h]

      case command.fit
      in .clip?
        tf = Tf.new
      in .stretch?
        tf = Tf[
          Tf.scale(command.bounds.size * svg_bounds.size.normalized),
          Tf.translate(svg_bounds.tl),
        ]
      in .keep_ratio?
        ratio = svg_bounds.h/svg_bounds.w
        scale = Point[command.bounds.w / svg_bounds.w, (command.bounds.w * ratio) / svg_bounds.h]

        tf = Tf[
          Tf.scale(scale),
          # Center vertically.
          Tf.translate(Point[0, (command.bounds.h - svg_bounds.h * scale.y) * 0.5]),
          Tf.translate(svg_bounds.tl),
        ]
      end

      matrix = tf.to_pvg

      PlutoVG.canvas_clip_rect(canvas, 0, 0, command.bounds.w, command.bounds.h)
      PlutoVG.canvas_transform(canvas, pointerof(matrix))

      color = command.color.to_pvg

      PlutoSVG.document_render(document, nil, canvas, pointerof(color), nil, nil)
    end

    PlutoVG.canvas_restore(canvas)
  end

  private def rasterize(canvas, command : DrawGlyph, dirty : Rect)
    return unless command.bounds.intersects?(dirty)

    face = command.font.as_ft
    color = command.color.to_pvg

    command.font.load_glyph(command.index, command.size)

    fn_move_to = ->(to : FreeType::Vector*, user : Void*) do
      PlutoVG.canvas_move_to(user.as(PlutoVG::Canvas), to.value.x/FT_UNIT, -(to.value.y/FT_UNIT))

      0
    end

    fn_line_to = ->(to : FreeType::Vector*, user : Void*) do
      PlutoVG.canvas_line_to(user.as(PlutoVG::Canvas), to.value.x/FT_UNIT, -(to.value.y/FT_UNIT))

      0
    end

    fn_conic_to = ->(control : FreeType::Vector*, to : FreeType::Vector*, user : Void*) do
      PlutoVG.canvas_quad_to(user.as(PlutoVG::Canvas),
        control.value.x/FT_UNIT, -(control.value.y/FT_UNIT),
        to.value.x/FT_UNIT, -(to.value.y/FT_UNIT),
      )

      0
    end

    fn_cubic_to = ->(control1 : FreeType::Vector*, control2 : FreeType::Vector*, to : FreeType::Vector*, user : Void*) do
      PlutoVG.canvas_cubic_to(user.as(PlutoVG::Canvas),
        control1.value.x/FT_UNIT, -(control1.value.y/FT_UNIT),
        control2.value.x/FT_UNIT, -(control2.value.y/FT_UNIT),
        to.value.x/FT_UNIT, -(to.value.y/FT_UNIT),
      )

      0
    end

    funcs = FreeType::OutlineFuncs.new(
      move_to: fn_move_to,
      line_to: fn_line_to,
      conic_to: fn_conic_to,
      cubic_to: fn_cubic_to,
      shift: 0,
      delta: 0,
    )

    PlutoVG.canvas_save(canvas)

    PlutoVG.canvas_set_color(canvas, pointerof(color))
    # FIXME: The snapping logic from DrawTransform affects this in
    # a horrendous way. We should probably wrap DrawGlyphs in DrawTransforms
    # and remove #pen but I'm unsure whether that'd be too expensive.
    PlutoVG.canvas_translate(canvas, command.pen.x, command.pen.y)
    assert FreeType.outline_decompose_face(face, pointerof(funcs), canvas.as(Void*)).zero?
    PlutoVG.canvas_fill(canvas)

    # PlutoVG.canvas_translate(canvas, -command.pen.x, -command.pen.y)
    # PlutoVG.canvas_set_rgba(canvas, 1.0, 0, 0, 0.8)
    # PlutoVG.canvas_stroke_rect(canvas, command.bounds.x, command.bounds.y, command.bounds.w, command.bounds.h)

    PlutoVG.canvas_restore(canvas)
  end
end
