module Ww::DwUIR
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
  # the other hand, some things that graphical DwUIR can draw, textual DwUIR
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
      fg : Pigment::RGBA,
      decoration : TextDecoration = TextDecoration::None

    alias Drawable = Span | Fill | Stroke | IBeam

    # Represents an inline string of characters with a number of shared properties
    # (e.g. *fg*, *decoration*).
    record Span,
      bounds : Rect,
      caption : String | StringView,
      fg : Pigment::RGBA,
      decoration : TextDecoration

    # Represents a rectangular fill.
    record Fill, bounds : Rect, bg : Pigment::RGBA

    # Represents a stroke box whose `Sides` can be characters of different color.
    record Stroke, bounds : Rect, sides : Sides do
      record Sides,
        l : Side?, r : Side?, t : Side?, b : Side?,
        tl : Side?, tr : Side?, bl : Side?, br : Side?

      record Side, chr : Char, color : Pigment::RGBA
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

    # Groups a drawable and its corresponding view.
    #
    # NOTE: since we do not support things like rotation of viewports in text,
    # `view` is simply a rect.
    record DrawableView, dw : Drawable, view : Rect do
      def self.new(dw : Drawable, view : View)
        new(dw, Rect.xsect(view))
      end
    end

    # Drawables in a picture are guaranteed to be sorted in draw order.
    alias Picture = Array(DrawableView)

    # Converts *dwuir* to a picture.
    def picture(dwuir : Term, viewport : Rect = Rect.inf) : Picture
      drawables = [] of {LayerRank, Rank, DrawableView}

      DwUIR.walk(dwuir, viewport: viewport) do |ctx, node|
        drawables(ctx, node) do |layer, rank, drawable|
          drawables << {layer, rank, DrawableView.new(drawable, ctx.view)}
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
            case command
            in TextCommand::PushInline, TextCommand::PushVirtual
              bounds_abs = bounds_rel.translate(ctx.bounds.tl)
              next unless DwUIR.visible?(ctx, bounds_abs)

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
              bounds_abs = bounds_rel.translate(ctx.bounds.tl)
              next unless DwUIR.visible?(ctx, bounds_abs)

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
            thickness-l⋮ 0
            thickness-r⋮ 0
            thickness-t⋮ 0
            thickness-b⋮ 0
            stroke_⋮ (rgba 0 0 0 0)
            radius-tl_⋮ 0
            radius-tr_⋮ 0
            radius-bl_⋮ 0
            radius-br_⋮ 0)
        WWML
        ) do
          fill_color = to_solid_color(Paint.term(fill))
          stroke_color = to_solid_color(Paint.term(stroke))

          thicknesses = {
            l: thickness_l.to(Float32).ceil.clamp(0.0..7.0).to_i,
            r: thickness_r.to(Float32).ceil.clamp(0.0..7.0).to_i,
            t: thickness_t.to(Float32).ceil.clamp(0.0..7.0).to_i,
            b: thickness_b.to(Float32).ceil.clamp(0.0..7.0).to_i,
          }

          if stroke_color.transparent? || thicknesses.values.all?(0)
            sink.call(ctx.layer, Rank::Mid, Fill.new(ctx.bounds, fill_color))
            return WalkFlow::Next
          end

          # We have some borders. We have to figure out which glyphs to use now.

          rmax = {ctx.bounds.w, ctx.bounds.h}.max

          radii = {
            tl: Magn.abst(radius_tl, Magn.abs(0)).resolve(rmax),
            tr: Magn.abst(radius_tr, Magn.abs(0)).resolve(rmax),
            bl: Magn.abst(radius_bl, Magn.abs(0)).resolve(rmax),
            br: Magn.abst(radius_br, Magn.abs(0)).resolve(rmax),
          }

          has_x = thicknesses[:l] > 0 || thicknesses[:r] > 0
          has_y = thicknesses[:t] > 0 || thicknesses[:b] > 0

          has_tl = thicknesses[:l] > 0 && thicknesses[:t] > 0
          has_tr = thicknesses[:r] > 0 && thicknesses[:t] > 0
          has_bl = thicknesses[:l] > 0 && thicknesses[:b] > 0
          has_br = thicknesses[:r] > 0 && thicknesses[:b] > 0

          has_rounded = radii.values.any? { |radius| radius > 0 }

          fill_rect = ctx.bounds

          if {has_tl, has_tr, has_bl, has_br, has_rounded}.any?
            tl = Stroke::Side.new(radius_tl.zero? ? '┌' : '╭', stroke_color)
            tr = Stroke::Side.new(radius_tr.zero? ? '┐' : '╮', stroke_color)
            bl = Stroke::Side.new(radius_bl.zero? ? '└' : '╰', stroke_color)
            br = Stroke::Side.new(radius_br.zero? ? '┘' : '╯', stroke_color)

            if thicknesses[:l] > 0
              l = Stroke::Side.new('│', stroke_color)
              fill_rect = fill_rect.grow(dw: -1).translate(dx: 1)
            end

            if thicknesses[:r] > 0
              r = Stroke::Side.new('│', stroke_color)
              fill_rect = fill_rect.grow(dw: -1)
            end

            if thicknesses[:t] > 0
              t = Stroke::Side.new('─', stroke_color)
              fill_rect = fill_rect.grow(dh: -1).translate(dy: 1)
            end

            if thicknesses[:b] > 0
              b = Stroke::Side.new('─', stroke_color)
              fill_rect = fill_rect.grow(dh: -1)
            end
          else
            # Arrange it so that the top border and the right border "float" over
            # backdrop, and left and bottom border "float" over fill color. Thus
            # they will be properly blended.

            if thicknesses[:l] > 0
              l = tl = bl = Stroke::Side.new(BORDER_X[thicknesses[:l] - 1], stroke_color)
            end

            if thicknesses[:r] > 0
              r = tr = br = Stroke::Side.new(BORDER_X[thicknesses[:r] - 1], stroke_color)
              fill_rect = fill_rect.grow(dw: -1)
            end

            if thicknesses[:t] > 0
              t = tl = tr = Stroke::Side.new(BORDER_Y[thicknesses[:t] - 1], stroke_color)
              fill_rect = fill_rect.grow(dh: -1).translate(dy: 1)
            end

            if thicknesses[:b] > 0
              b = bl = br = Stroke::Side.new(BORDER_Y[thicknesses[:b] - 1], stroke_color)
            end
          end

          sides = Stroke::Sides.new(l, r, t, b, tl, tr, bl, br)

          sink.call(ctx.layer, Rank::Mid, Fill.new(fill_rect, fill_color))
          sink.call(ctx.layer, Rank::Front, Stroke.new(ctx.bounds, sides))

          WalkFlow::Next
        end

        otherwise do
          WalkFlow::Recurse
        end
      end
    end

    # DwUIR replier that should be used with the textual front-end.
    def reply(subject : Term) : Term
      # Fast paths for the vast majority of subjects.
      return subject unless subject.type.dict?
      return subject unless subject.includes?(:"dw-request")

      Term.of_case(subject) do
        matchpi %{(text ⍊ dw-request: (measure ±width oheight_symbol ⍊ status_symbol))} do
          space = Point.new(width.to(Float32), Float32::INFINITY)

          continue unless spec = DwUIR.text_spec?(subject, space)

          size = Rect.empty
          TextCommand.each_with_bounds(Pencil.new, spec.wrap, spec.caption, selection: nil) do |command, bounds|
            case command
            when TextCommand::PushInline, TextCommand::PushVirtual
              size |= bounds
            when TextCommand::NextLine
              size = size.grow(dh: 1)
            end
          end

          subject.morph(
            {status, :ok},
            {oheight, size.h.floor},
            {:"dw-request", nil},
          )
        end

        matchpi %{(text ⍊ dw-request: (measure owidth_symbol oheight_symbol ⍊ status_symbol))} do
          space = Point.inf

          continue unless spec = DwUIR.text_spec?(subject, space)

          size = Rect.empty
          TextCommand.each_with_bounds(Pencil.new, spec.wrap, spec.caption, selection: nil) do |command, bounds|
            case command
            when TextCommand::PushInline, TextCommand::PushVirtual
              size |= bounds
            when TextCommand::NextLine
              size = size.grow(dh: 1)
            end
          end

          subject.morph(
            {status, :ok},
            {owidth, size.w.floor},
            {oheight, size.h.floor},
            {:"dw-request", nil},
          )
        end

        otherwise { subject }
      end
    end

    private def to_solid_color(paint : Paint::Solid) : Pigment::RGBA
      paint.color
    end

    # Terminals don't do gradients, and we won't bother emulating them --
    # so we simply convert any paint to a solid color, if possible.
    #
    # TODO: Linear gradient and radial gradient -- we can use one of the steps.
    private def to_solid_color(paint : Paint::Any) : Pigment::RGBA
      Pigment.named("red")
    end

    # Represents a terminal cell: either a pure character cell, a color cell,
    # or a character cell on top of a color cell (character with a background).
    alias Cell = Rune | Pigment::RGBA | {Rune, Pigment::RGBA}

    # Groups objects related to the console screen.
    class Screen
      # Points to a hash mapping arbitrary cell coordinates to cells
      # currently occupying those.
      getter cells = {} of {Int16, Int16} => Cell

      # Stores the cursor I-beam position. The cursor is hidden if `nil`.
      property? beam : {Int16, Int16}? = nil

      # Stores a rectangle that defines which cells are visible. Can be
      # infinite: this means all cells will be drawn.
      property view : Rect = Rect.inf
    end

    # Write color on top of rune.
    private def blend(cell0 : Rune, cell1 : Pigment::RGBA) : Cell
      cell1.a < 255 ? {cell0, cell1} : cell1
    end

    # Write rune on top of color.
    private def blend(cell0 : Pigment::RGBA, cell1 : Rune) : Cell
      {cell1, cell0}
    end

    # Change rune (I'm not sure how correct this behavior is).
    private def blend(cell0 : {Rune, Pigment::RGBA}, cell1 : Rune) : Cell
      {cell1, cell0[1]}
    end

    private def blend(cell0, cell1) : Cell
      cell1
    end

    private def draw(screen, ix : Int32, iy : Int32, object : Cell) : Nil
      return if (Rect[ix, iy, 1, 1] & screen.view).empty?

      ix16 = ix.to_i16
      iy16 = iy.to_i16
      cell0 = screen.cells[{ix16, iy16}]?

      screen.cells[{ix16, iy16}] = blend(cell0, cell1: object)
    end

    private def draw(screen, ix : Int32, iy : Int32, object : Stroke::Side) : Nil
      draw(screen, ix, iy, Rune.new(object.chr, object.color))
    end

    private def draw(screen : Screen, span : Span) : Nil
      span.caption.each_char_with_index do |chr, index|
        ix, iy = span.bounds.floor.ixy

        draw(screen, ix + index, iy, Rune.new(chr, span.fg, span.decoration))
      end
    end

    private def draw(screen : Screen, fill : Fill) : Nil
      return if fill.bg.transparent?

      ix, iy, iw, ih = fill.bounds.snap.ixywh

      (ix...ix + iw).each do |i|
        (iy...iy + ih).each do |j|
          draw(screen, i, j, fill.bg)
        end
      end
    end

    private def draw(screen : Screen, box : Stroke) : Nil
      ix, iy, iw, ih = box.bounds.snap.ixywh

      if iw < 3 && ih < 3
        # Too small, noop.
        return
      end

      # Draw left side.
      if iw >= 2
        if corner = box.sides.tl || box.sides.l
          draw(screen, ix, iy, corner)
        end

        if (side = box.sides.l) && ih >= 3
          (iy + 1...iy + ih - 1).each do |y|
            draw(screen, ix, y, side)
          end
        end

        if corner = box.sides.bl || box.sides.l
          draw(screen, ix, iy + ih - 1, corner)
        end
      end

      # Draw right side.
      if iw >= 2
        if corner = box.sides.tr || box.sides.r
          draw(screen, ix + iw - 1, iy, corner)
        end

        if (side = box.sides.r) && ih >= 3
          (iy + 1...iy + ih - 1).each do |y|
            draw(screen, ix + iw - 1, y, side)
          end
        end

        if corner = box.sides.br || box.sides.r
          draw(screen, ix + iw - 1, iy + ih - 1, corner)
        end
      end

      # Draw top side.
      if iw >= 3 && ih >= 2 && (side = box.sides.t)
        (ix + 1...ix + iw - 1).each do |x|
          draw(screen, x, iy, side)
        end
      end

      # Draw bottom side.
      if iw >= 3 && ih >= 2 && (side = box.sides.b)
        (ix + 1...ix + iw - 1).each do |x|
          draw(screen, x, iy + ih - 1, side)
        end
      end
    end

    private def draw(screen : Screen, cursor : IBeam) : Nil
      ix, iy = cursor.point.floor.ixy

      screen.beam = {ix.to_i16, iy.to_i16}
    end

    private def draw(screen : Screen, dwv : DrawableView) : Nil
      view0 = screen.view
      view1 = dwv.view

      screen.view = view1

      begin
        draw(screen, dwv.dw)
      ensure
        screen.view = view0
      end
    end

    # Returns a screen with *picture* drawn on it.
    def screen(picture : Picture) : Screen
      screen = Screen.new
      picture.each do |drawable|
        draw(screen, drawable)
      end
      screen
    end

    # We need to clamp border insets to 1. We normally inset by border width, but that's
    # not how it works in the terminal; border width determines the choiceof a glyph,
    # but it's always one glyph.
    def insetfixR
      callR { |node| Rewrite.one(insetfix(node)) }
    end

    # :nodoc:
    SYM_PADDING = Term[:padding]
    # :nodoc:
    SYM_INSET = Term[:inset]

    # :nodoc:
    def insetfix(node : Term) : Term
      return node unless node.type.dict?
      return node unless node.probably_includes?(SYM_PADDING)

      result = node.transaction do |commit|
        if node.itemsize == 2 && node.pairsize >= 2 && node[0] == SYM_PADDING && node[SYM_INSET]? == Term.of(true)
          commit.with(:pl, 1) if node[:pl]?
          commit.with(:pr, 1) if node[:pr]?
          commit.with(:pt, 1) if node[:pt]?
          commit.with(:pb, 1) if node[:pb]?
        end

        node.each_item_with_index do |item, index|
          commit.with(index, insetfix(item))
        end
      end

      Term.of(result)
    end
  end
end
