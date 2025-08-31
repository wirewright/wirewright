module Ww::Soma::DwUIR
  # A text UI front-end for DwUIR.
  #
  # You can currently get DwUIR on screen in two ways:
  #
  # - Using the graphical framework (it is currently rather disorganized,
  #   but its entry point is usually `Viewer`).
  # - Using this text UI framework.
  #
  # Textual DwUIR is *a small subset of graphical DwUIR*. This means that
  # anything that textual DwUIR can draw, graphical DwUIR will draw; on
  # the other hand, some things that graphical DwUIR can draw, text DwUIR
  # cannot draw (e.g. images).
  #
  # Textual DwUIR provides a lossier, but compatible, way of visualizing
  # DwUIR structures in plain text environments while preserving upstream
  # abstractions such as Microfold and UIR.
  module Textual
    extend self

    # An implementation of `IPencil` that should be used when rendering
    # textual DwUIR.
    #
    # This implementation discards any font properties/metrics; and instead
    # simply moves by 1 X for each character horizontally, and by 1 Y vertically
    # on newline.
    struct Pencil
      include IPencil

      getter origin : Point

      def initialize(@origin = Point[0, 0])
      end

      def tip : Point
        origin + Point[0, 1]
      end

      def tip(ch : Char) : Point
        tip
      end

      def after_writing(ch : Char) : Pencil
        case ch
        when '\r' then self
        when '\t' then Pencil.new(@origin + Point[2, 0])
        when '\n' then Pencil.new(Point[0, @origin.y + 1])
        else
          Pencil.new(@origin + Point[1, 0])
        end
      end
    end

    # Lists the available text decoration attributes.
    @[Flags]
    enum TextDecoration : UInt8
      Bold
      Italic
      Underline
    end

    # Runes are used to represent each individual character/cell on screen.
    record Rune,
      chr : Char,
      fg : Color,
      decoration : TextDecoration = TextDecoration::None

    alias Drawable = Span | Fill | Stroke | IBeam

    # Represents an inline string of characters with a number of shared properties
    # (e.g. *fg*, *decoration*).
    record Span,
      bounds : Rect,
      caption : String | StringView,
      fg : Color,
      decoration : TextDecoration

    # Represents a rectangular fill.
    record Fill, bounds : Rect, bg : Color

    # Represents a stroke box whose `Sides` can be characters of different color.
    record Stroke, bounds : Rect, sides : Sides do
      record Sides,
        l : Side?, r : Side?, t : Side?, b : Side?,
        tl : Side?, tr : Side?, bl : Side?, br : Side?

      record Side, chr : Char, color : Color
    end

    # Represents an I-beam cursor.
    #
    # I am not aware of terminals that support multiple cursors. So you should
    # expect only one I-beam cursor to be drawn. Which one depends on layer ranks
    # (z-indices on the DwUIR side). Any emulation of on our end would
    # not work particularly because we're talking about an *I-beam*; and
    # I-beams are impossible to emulate in terminals; not e.g. a block which
    # we can draw any amounts of quite easily.
    record IBeam, point : Point

    # Drawables in a picture are guaranteed to be sorted in draw order.
    alias Picture = Array(Drawable)

    # Converts *dwuir* to a picture.
    def picture(dwuir : Term, viewport : Rect = Rect.inf) : Picture
      drawables = [] of {LayerRank, Rank, Drawable}

      DwUIR.walk(dwuir, viewport: viewport) do |ctx, node|
        drawables(ctx, node) do |layer, rank, drawable|
          drawables << {layer, rank, drawable}
        end
      end

      drawables.sort_by! { |layer, rank, _| {layer, rank} }
      drawables.map { |_, _, drawable| drawable }
    end

    # :nodoc:
    BORDER_X = "▎▍▌▋▊▉█"

    # :nodoc:
    BORDER_Y = "▁▂▃▄▅▆█"

    private def drawables(ctx : WalkContext, node : Term, &sink : LayerRank, Rank, Drawable ->) : WalkFlow
      Term.case(node) do
        matchpi %{[text]} do
          unless spec = DwUIR.text_spec?(node, space: ctx.bounds.size)
            return WalkFlow::Next
          end

          fg = to_solid_color(spec.color)
          decoration = TextDecoration::None
          if spec.weight > FontWeight::Text
            decoration |= TextDecoration::Bold
          end
          if spec.italic
            decoration |= TextDecoration::Italic
          end
          if spec.underline
            decoration |= TextDecoration::Underline
          end

          selection = false

          TextCommand.each_with_bounds(Pencil.new, spec.wrap, spec.caption, selection: spec.selection.try(&.range)) do |command, bounds_rel|
            bounds_abs = bounds_rel.translate(ctx.bounds.tl)
            next unless DwUIR.visible?(ctx, bounds_abs)

            case command
            in TextCommand::PushInline, TextCommand::PushVirtual
              unless selection
                span = Span.new(bounds_abs, command.view, fg, decoration)
                sink.call(ctx.layer, Rank::Mid, span)
                next
              end

              unless selection_spec = spec.selection
                unreachable("selection is on but selection spec is absent")
              end

              rect = Fill.new(bounds_abs, to_solid_color(selection_spec.fill))
              sink.call(ctx.layer, Rank::Back, rect)

              span = Span.new(bounds_abs, command.view, to_solid_color(selection_spec.color), decoration)
              sink.call(ctx.layer, Rank::Mid, span)
            in TextCommand::NextLine
            in TextCommand::PutCursor
              beam = IBeam.new(bounds_abs.tl)
              sink.call(ctx.layer, Rank::Front, beam)
            in TextCommand::BeginSelection
              selection = true
            in TextCommand::EndSelection
              selection = false
            in TextCommand::WriteInline
            end
          end

          WalkFlow::Next
        end

        matchpi(<<-WWML
        (rect
          ⍊ fill_⋮ (rgba 0 0 0 0)
            thickness-l_⋮ 0
            thickness-r_⋮ 0
            thickness-t_⋮ 0
            thickness-b_⋮ 0
            stroke_⋮ (rgba 0 0 0 0)
            radius-tl_⋮ 0
            radius-tr_⋮ 0
            radius-bl_⋮ 0
            radius-br_⋮ 0)
      WWML
        ) do
          fill_color = to_solid_color(Paint.term(fill))

          thicknesses = {
            l: thickness_l.to(Float32).ceil.clamp(0.0..7.0).to_i,
            r: thickness_r.to(Float32).ceil.clamp(0.0..7.0).to_i,
            t: thickness_t.to(Float32).ceil.clamp(0.0..7.0).to_i,
            b: thickness_b.to(Float32).ceil.clamp(0.0..7.0).to_i,
          }

          if thicknesses.values.all?(1)
            stroke_color = to_solid_color(Paint.term(stroke))

            # Use rounded corner glyph for nonzero radius corner.
            corner_tl = radius_tl.zero? ? '┌' : '╭'
            corner_tr = radius_tr.zero? ? '┐' : '╮'
            corner_bl = radius_bl.zero? ? '└' : '╰'
            corner_br = radius_br.zero? ? '┘' : '╯'

            sides = Stroke::Sides.new(
              l: Stroke::Side.new('│', stroke_color),
              r: Stroke::Side.new('│', stroke_color),
              t: Stroke::Side.new('─', stroke_color),
              b: Stroke::Side.new('─', stroke_color),
              tl: Stroke::Side.new(corner_tl, stroke_color),
              tr: Stroke::Side.new(corner_tr, stroke_color),
              bl: Stroke::Side.new(corner_bl, stroke_color),
              br: Stroke::Side.new(corner_br, stroke_color),
            )

            interior_rect = ctx.bounds.pad(1)

            sink.call(ctx.layer, Rank::Mid, Fill.new(interior_rect, fill_color))
            sink.call(ctx.layer, Rank::Front, Stroke.new(ctx.bounds, sides))
          elsif thicknesses.values.any? { |thickness| thickness > 0 }
            stroke_color = to_solid_color(Paint.term(stroke))

            if thicknesses[:l] > 0
              l = Stroke::Side.new(BORDER_X[thicknesses[:l]], stroke_color)
            end

            if thicknesses[:r] > 0
              r = Stroke::Side.new(BORDER_X[thicknesses[:r]], stroke_color)
            end

            if thicknesses[:t] > 0
              t = Stroke::Side.new(BORDER_Y[thicknesses[:t]], stroke_color)
            end

            if thicknesses[:b] > 0
              b = Stroke::Side.new(BORDER_Y[thicknesses[:b]], stroke_color)
            end

            # We will draw what corners we can, but there are no good glyphs for
            # the corners, so the full stroke box would look quite ugly with
            # thickness over 1.
            sides = Stroke::Sides.new(l, r, t, b,
              tl: t || l,
              tr: r && !t ? r : nil,
              bl: l,
              br: r,
            )

            # Shrink the interior rect we're going to fill so that the top border
            # and the right border "float" over backdrop and thus have the proper
            # background. We don't want them colored with the fill color because
            # their unfilled part points outside of the rect.
            interior_rect = ctx.bounds
              .grow(-Point[thicknesses[:r], thicknesses[:t]].min(Point[1, 1]))
              .translate(Point[0, Math.min(1, thicknesses[:t])])

            sink.call(ctx.layer, Rank::Mid, Fill.new(interior_rect, fill_color))
            sink.call(ctx.layer, Rank::Front, Stroke.new(ctx.bounds, sides))
          else
            sink.call(ctx.layer, Rank::Mid, Fill.new(ctx.bounds, fill_color))
          end

          WalkFlow::Next
        end

        otherwise do
          WalkFlow::Recurse
        end
      end
    end

    # :nodoc:
    def to_solid_color(paint : Paint::Solid)
      paint.color
    end

    # Terminals don't do gradients, and we won't bother emulating them --
    # so we simply convert any paint to a solid color, if possible.
    #
    # TODO: Linear gradient and radial gradient -- we can use one of the steps.
    def to_solid_color(paint : Paint::Any)
      Color.named("red")
    end

    # A screen consists of two layers: a `backdrop` layer, which stores background
    # colors of cells; and a `content` layer, which stores the `Rune`s to display
    # on top of the background.
    class Screen
      # Readable and writable.
      getter backdrop = {} of {UInt16, UInt16} => Color

      # Readable and writable.
      getter content = {} of {UInt16, UInt16} => Rune

      # Readable and writable I-beam cursor position.
      property? beam : {UInt16, UInt16}? = nil
    end

    private def draw(dst, ix : Int32, iy : Int32, object)
      dst[{ix.to_u16, iy.to_u16}] = object
    end

    private def draw(dst, ix : Int32, iy : Int32, object : Stroke::Side)
      draw(dst, ix, iy, Rune.new(object.chr, object.color))
    end

    private def draw(screen : Screen, span : Span)
      span.caption.each_char_with_index do |chr, index|
        ix, iy = span.bounds.snap.ixy

        draw(screen.content, ix + index, iy, Rune.new(chr, span.fg, span.decoration))
      end
    end

    private def draw(screen : Screen, fill : Fill)
      ix, iy, iw, ih = fill.bounds.snap.ixywh

      (ix...ix + iw).each do |i|
        (iy...iy + ih).each do |j|
          draw(screen.backdrop, i, j, fill.bg)
        end
      end
    end

    private def draw(screen : Screen, box : Stroke)
      ix, iy, iw, ih = box.bounds.snap.ixywh

      # Draw corners.
      if iw >= 2 && ih >= 2
        if corner = box.sides.tl
          draw(screen.content, ix, iy, corner)
        end
        if corner = box.sides.tr
          draw(screen.content, ix + iw - 1, iy, corner)
        end
        if corner = box.sides.br
          draw(screen.content, ix + iw - 1, iy + ih - 1, corner)
        end
        if corner = box.sides.bl
          draw(screen.content, ix, iy + ih - 1, corner)
        end
      end

      # Draw left edge.
      if (side = box.sides.l) && ih >= 3
        (iy + 1...iy + ih - 1).each do |y|
          draw(screen.content, ix, y, side)
        end
      end

      # Draw top edge.
      if (side = box.sides.t) && iw >= 3
        (ix + 1...ix + iw - 1).each do |x|
          draw(screen.content, x, iy, side)
        end
      end

      # Draw right edge.
      if (side = box.sides.r) && ih >= 3
        (iy + 1...iy + ih - 1).each do |y|
          draw(screen.content, ix + iw - 1, y, side)
        end
      end

      # Draw bottom edge.
      if (side = box.sides.b) && iw >= 3
        (ix + 1...ix + iw - 1).each do |x|
          draw(screen.content, x, iy + ih - 1, side)
        end
      end
    end

    private def draw(screen : Screen, cursor : IBeam)
      ix, iy = cursor.point.floor.ixy

      screen.beam = {ix.to_u16, iy.to_u16}
    end

    # Returns a screen with *picture* drawn on it.
    def screen(picture : Picture) : Screen
      screen = Screen.new
      picture.each do |drawable|
        draw(screen, drawable)
      end
      screen
    end
  end
end
