module Ww::Soma::DwUIR
  # A painter converts `DrawKey`s to `Layer`s. This is an implementation of
  # a painter that uses PlutoVG.
  struct PvgPainter
    # :nodoc:
    def initialize(@fonts : PvgFontFaceStore,
                   @resources : ResourceLoader,
                   width : Int32,
                   height : Int32)
      unless @surface = PlutoVG.surface_create(width, height)
        raise ArgumentError.new("failed to create surface of size #{width}x#{height}")
      end

      unless @canvas = PlutoVG.canvas_create(@surface)
        raise ArgumentError.new("failed to create canvas for surface")
      end
    end

    # :nodoc:
    def destroy : Nil
      PlutoVG.canvas_destroy(@canvas)
      PlutoVG.surface_destroy(@surface)
    end

    # Paints *key* and returns the resulting `Layer`.
    #
    # Uses the font store at *fonts* to load PlutoVG fonts.
    # Uses the resource loader at *resources* to fetch images etc.
    def self.layer_for(fonts : PvgFontFaceStore, resources : ResourceLoader, key : DrawKey) : Layer
      bounds = Rect.new(Point.new(0, 0), key.extent).round
      tfbounds = key.tf.map(bounds).round

      painter = new(fonts, resources, *tfbounds.iwh)

      begin
        painter.layer_for(key, bounds, tfbounds)
      ensure
        # We are not using finalizers here to make sure the GC doesn't free
        # @surface/@canvas until we're sure we've created  a layer from it.
        # Apparently @surface/@canvas are disconnected from PvgPainter in
        # the object graph (nor should they, they're from C!). So when the GC
        # thinks it's time to finalize PvgPainter is the moment when @canvas/
        # @surface are destroyed -- right under our feet.
        painter.destroy
      end
    end

    # :nodoc:
    def layer_for(key : DrawKey, bounds : Rect, tfbounds : Rect) : Layer
      matrix = to_pvg_matrix(key.tf)

      PlutoVG.canvas_translate(@canvas, *(-tfbounds.tl).xy)
      PlutoVG.canvas_transform(@canvas, pointerof(matrix))

      paint(key.shape, bounds)

      Layer.new(
        pixels: PlutoVG.surface_get_data(@surface),
        width: PlutoVG.surface_get_width(@surface),
        height: PlutoVG.surface_get_height(@surface),
        stride: PlutoVG.surface_get_stride(@surface),
      )
    end

    private def paint(shape : RectShape, bounds : Rect) : Nil
      shape = shape.clamp(bounds)

      outer = bounds
      inner = shape.border.pad(outer)
      inner_radii = RectRadii.new(
        tl: {shape.radii.tl - {shape.border.t, shape.border.l}.max, 0.0f32}.max,
        tr: {shape.radii.tr - {shape.border.t, shape.border.r}.max, 0.0f32}.max,
        br: {shape.radii.br - {shape.border.b, shape.border.r}.max, 0.0f32}.max,
        bl: {shape.radii.bl - {shape.border.b, shape.border.l}.max, 0.0f32}.max
      )

      unless shape.border.transparent?
        add_rrect(inner.pad(0.5), inner_radii)
        add_rrect(outer, shape.radii)
        PlutoVG.canvas_clip_preserve(@canvas)
        set_paint(shape.border.color, bounds)
        PlutoVG.canvas_set_fill_rule(@canvas, PlutoVG::FillRule::EvenOdd)
        PlutoVG.canvas_fill(@canvas)
        PlutoVG.canvas_set_fill_rule(@canvas, PlutoVG::FillRule::NonZero)
      end

      add_rrect(inner, inner_radii)
      PlutoVG.canvas_clip(@canvas)
      set_paint(shape.fill, bounds)
      PlutoVG.canvas_paint(@canvas)
    end

    private def paint(shape : FragShape, bounds : Rect) : Nil
      set_paint(shape.color, bounds)

      begin
        PlutoVG.canvas_save(@canvas)
        PlutoVG.canvas_set_font(@canvas, @fonts.face(shape.font), shape.size)

        pencil = @fonts.pencils.call(PencilRequest.new(shape.font, shape.size, tracking: shape.tracking))

        shape.string.each_char do |char|
          case char
          when ' ', '\t', '\n', '\r', '\0' # skip weird/control chars
          else
            tip = pencil.tip(char)

            PlutoVG.canvas_add_glyph(@canvas, char.ord, *(bounds.tl + tip).round(grain: Point.new(4, 1)).xy)
          end

          pencil = pencil.after_writing(char)
        end

        PlutoVG.canvas_clip(@canvas)
        PlutoVG.canvas_paint(@canvas)
      ensure
        PlutoVG.canvas_restore(@canvas)
      end

      # Draw underline
      if underline = shape.underline
        underline_bounds = Rect.new(
          tl: bounds.tl + Point.new(0, pencil.tip.y + underline.offset),
          size: Point.new(bounds.w, underline.thickness),
        )

        if color = underline.color
          PlutoVG.canvas_clip_rect(@canvas, *underline_bounds.xywh)
          set_paint(color, underline_bounds)
          PlutoVG.canvas_paint(@canvas)
        else
          PlutoVG.canvas_clip_rect(@canvas, *underline_bounds.xywh)
          PlutoVG.canvas_paint(@canvas)
        end
      end
    end

    # TODO: instead of using warnings, draw as a red rect with text saying
    # something went wrong! Similarly for Paint::Invalid.

    private def paint(shape : SvgShape, bounds : Rect) : Nil
      unless data = @resources.ref?(shape.src)
        Log.warn { "failed to load resource #{shape.src}" }
        return
      end

      unless document = PlutoSVG.document_load_from_data(data, data.size, *bounds.wh, nil, nil)
        Log.warn { "failed to load document from data" }
        return
      end

      PlutoSVG.document_extents(document, nil, out svg_extents)

      svgbounds = Rect[svg_extents.x, svg_extents.y, svg_extents.w, svg_extents.h]

      case shape.resize
      in .clip?
        transform = Tf.new
      in .stretch?
        transform = Tf[
          Tf.translate(svgbounds.tl),
          Tf.scale(bounds.size * svgbounds.size.normalized),
        ]
      in .keep_ratio?
        ratio = svgbounds.h/svgbounds.w
        scale = Point.new(bounds.w / svgbounds.w, (bounds.w * ratio) / svgbounds.h)

        transform = Tf[
          Tf.translate(svgbounds.tl),
          # Center vertically.
          Tf.translate(Point.new(0, (bounds.h - svgbounds.h * scale.y) * 0.5)),
          Tf.scale(scale),
        ]
      end

      tfmatrix = to_pvg_matrix(transform)

      PlutoVG.canvas_transform(@canvas, pointerof(tfmatrix))

      begin
        color = PlutoVG::Color.new(
          r: shape.color.ur,
          g: shape.color.ug,
          b: shape.color.ub,
          a: shape.color.ua,
        )

        unless PlutoSVG.document_render(document, nil, @canvas, pointerof(color), nil, nil)
          Log.warn { "failed to render document" }
          return
        end
      ensure
        PlutoSVG.document_destroy(document)
      end
    end

    private def add_rrect(bounds : Rect, radii : RectRadii) : Nil
      PlutoVG.canvas_move_to(@canvas, *bounds.rra(radii).xy)
      PlutoVG.canvas_arc_to(@canvas, *radii.tl2.xy, 0, false, false, *bounds.rrb(radii).xy)
      PlutoVG.canvas_line_to(@canvas, *bounds.rrc(radii).xy)
      PlutoVG.canvas_arc_to(@canvas, *radii.bl2.xy, 0, false, false, *bounds.rrd(radii).xy)
      PlutoVG.canvas_line_to(@canvas, *bounds.rre(radii).xy)
      PlutoVG.canvas_arc_to(@canvas, *radii.br2.xy, 0, false, false, *bounds.rrf(radii).xy)
      PlutoVG.canvas_line_to(@canvas, *bounds.rrg(radii).xy)
      PlutoVG.canvas_arc_to(@canvas, *radii.tr2.xy, 0, false, false, *bounds.rrh(radii).xy)
      PlutoVG.canvas_close_path(@canvas)
    end

    private def set_paint(paint : Paint::Invalid, bounds : Rect)
      set_paint(Paint::Solid.new(Color.rgba(0xff, 0, 0)), bounds)
    end

    private def set_paint(paint : Paint::Solid, bounds : Rect)
      PlutoVG.canvas_set_rgba(@canvas, *paint.color.urgba)
    end

    private def set_paint(paint : Paint::LinearGradient, bounds : Rect)
      stops = paint.stops.to_readonly_slice do |offset, color|
        PlutoVG::GradientStop.new(
          offset: offset,
          color: PlutoVG::Color.new(
            r: color.ur,
            g: color.ug,
            b: color.ub,
            a: color.ua,
          ),
        )
      end

      PlutoVG.matrix_init_identity(out tfmat)
      PlutoVG.matrix_translate(pointerof(tfmat), *bounds.tl.xy)
      PlutoVG.canvas_set_linear_gradient(@canvas,
        x1: paint.begin.x * bounds.w,
        y1: paint.begin.y * bounds.h,
        x2: paint.end.x * bounds.w,
        y2: paint.end.y * bounds.h,
        spread: PlutoVG::SpreadMethod::Pad,
        stops: stops,
        nstops: stops.size,
        matrix: pointerof(tfmat),
      )
    end

    private def set_paint(paint : Paint::RadialGradient, bounds : Rect)
      stops = paint.stops.to_readonly_slice do |offset, color|
        PlutoVG::GradientStop.new(
          offset: offset,
          color: PlutoVG::Color.new(
            r: color.ur,
            g: color.ug,
            b: color.ub,
            a: color.ua,
          ),
        )
      end

      PlutoVG.matrix_init_identity(out tfmat)
      PlutoVG.matrix_translate(pointerof(tfmat), *bounds.tl.xy)
      PlutoVG.canvas_set_radial_gradient(@canvas,
        cx: paint.center.x * bounds.w,
        cy: paint.center.y * bounds.h,
        cr: paint.center_radius * bounds.diagonal * 0.5,
        fx: paint.focus.x * bounds.w,
        fy: paint.focus.y * bounds.h,
        fr: paint.focus_radius * bounds.diagonal * 0.5,
        spread: PlutoVG::SpreadMethod::Pad,
        stops: stops,
        nstops: stops.size,
        matrix: pointerof(tfmat),
      )
    end

    # |@ soma.dwuir.painters.plutovg.image
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
    # |@endblock

    private def set_paint(paint : Paint::Image, bounds : Rect)
      if data = @resources.ref?(paint.src)
        surface = PlutoVG.surface_load_from_image_data(data, data.size)

        img_bounds = Rect.new(
          tl: Point.new(0, 0),
          size: Point.new(
            PlutoVG.surface_get_width(surface).to_f32,
            PlutoVG.surface_get_height(surface).to_f32,
          ),
        )

        unless surface.null?
          type = PlutoVG::TextureType::Plain

          if paint.tile
            type = PlutoVG::TextureType::Tiled
          end

          # Resize
          target_bounds = Rect.new(
            tl: Point.new(0, 0),
            size: Point.new(
              paint.resize_w.resolve(img_bounds.w),
              paint.resize_h.resolve(img_bounds.h),
            ),
          )

          scale = target_bounds.size * img_bounds.size.normalized

          # Fit
          case fit = paint.fit
          in Paint::ImageFit::Align
            transform = Tf[
              Tf.translate(bounds.align(target_bounds, fit.normpt).tl),
              Tf.scale(scale),
            ]
          in Paint::ImageFit::Pan
            transform = Tf[
              Tf.translate(bounds.tl + fit.delta),
              Tf.scale(scale),
            ]
          in Paint::ImageFit::Stretch
            transform = Tf[
              Tf.scale(scale),
              Tf.translate(bounds.tl),
              Tf.scale(bounds.size * target_bounds.size.normalized),
            ]
          end

          matrix = to_pvg_matrix(transform)

          PlutoVG.canvas_set_texture(@canvas, surface, type, paint.opacity, pointerof(matrix))
          return
        end

        Log.debug { "failed to load image: #{paint.src}" }
      end

      set_paint(Paint::Invalid.new, bounds)
    end

    private def to_pvg_matrix(transform : Tf)
      PlutoVG::Matrix.new(
        a: transform.@a,
        b: transform.@b,
        c: transform.@c,
        d: transform.@d,
        e: transform.@e,
        f: transform.@f,
      )
    end
  end
end
