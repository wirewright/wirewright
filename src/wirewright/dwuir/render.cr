module Ww::DwUIR
  alias TextSelectionRange = Range(Int32, Int32)?

  # Specifies how a selection should look like, and the selection range.
  record SelectionSpec,
    range : Range(Int32, Int32),
    fill : Paint::Any,
    color : Paint::Any,
    radius : Float32,
    extent : Float32

  defcase TextSpec,
    caption : String,
    family : String,
    weight : FontWeight,
    italic : Bool,
    size : Float32,
    color : Paint::Any,
    leading : Magn,
    tracking : Magn,
    wrap : WrapSpec,
    selection : SelectionSpec?,
    underline : UnderlineSpec?

  class TextSpec
    # Tries to find the font referenced in this text spec using `FontIndex`.
    getter? font : NormalPath? do
      FontIndex.path_to?(family, weight, italic: italic)
    end

    def each_text_drawable(pencil : IPencil, *, origin = Point.new(0, 0), &sink : TextDrawable::Any ->)
      TextDrawable.each(pencil, wrap, caption, selection.try(&.range)) do |dw|
        case dw
        in TextDrawable::InlineString
          dw = dw.copy_with(bounds: dw.bounds
            .grow(dh: underline.try { |u| u.offset + u.thickness } || 0.0f32)
            .translate(origin)
            .snap)
        in TextDrawable::Selection
          next unless sel = selection

          sel_height = pencil.tip.y + sel.extent*(pencil.line_height - pencil.tip.y)

          dw = dw.copy_with(bounds: dw.bounds
            .resize(h: sel_height)
            .translate(origin)
            .snap)
        end

        sink.call(dw)
      end
    end
  end

  # Attempts to recognize and parse a text node in *node*. Returns the resulting
  # `TextSpec` if recognized. Returns `nil` otherwise.
  #
  # *space* specifies the width/height of text to wrap at/until for the wrap
  # spec. Points with infinite width, height, or both are allowed as well.
  def text_spec?(node : Term, space : Point) : TextSpec?
    Term.case(node) do
      # |@ soma.dwuir.node.text
      #
      # |@block
      # The `text` node is drawn as a string of text with customizable styling and
      # layout attributes. It allows specification of the text content, dimensions,
      # font, size, weight, and other typographic attributes such as line spacing,
      # letter spacing, and color.
      # |@endblock
      #
      # |@key caption -- Specifies the string that should be displayed.
      #
      # |@key font -- Specifies the font name to use for the text.
      #
      # |@key size -- Specifies the font size to use for the text.
      #
      # |@key weight -- Specifies the font weight to use for the text.
      #
      # |@key italic -- Specifies whether to use an italic font.
      #
      # |@key leading soma.dwuir.magn -- Specifies the line spacing (named *leading*
      # after Tailwind) for the text. Raw numbers are treated relatively. Relative
      # measures are in terms of the size of the text.
      #
      # |@key tracking soma.dwuir.magn -- Specifies the letter spacing (named *tracking*
      # after Tailwind) for the text. Raw numbers are treated relatively. Relative
      # measures are in terms of the width of the whitespace character.
      #
      # |@key color soma.dwuir.paint -- Specifies the paint that should be used
      # for the text.
      matchpi(<<-WWML
        (text ¦ _
          caption_string
          font_string
          size_number
          weight: (%optional 400 weight←(%any 100 200 300 400 450 500 600 700 800 900))
          italic⋮ false
          leading⋮ 1.5
          tracking⋮ 0.0
          color_⋮ (rgb 0 0 0))
      WWML
      ) do |caption|
        text_family = font.to(String)
        text_weight = FontWeight.parse(weight.to(Int32))
        text_italic = italic.true?
        text_size = size.to(Float32)
        text_color = Paint.term(color)
        text_leading = Magn.relt(leading, Magn.rel(0))
        text_tracking = Magn.relt(tracking, Magn.rel(1))

        wrap = WrapSpec.nowrap
        selection = nil
        underline = nil

        caption = caption.to(String)

        Term.case(node) do
          # |@ soma.dwuir.node.text.underline
          #
          # |@block
          # Use `underline: true` to enable underline for the text.
          # |@endblock
          #
          # |@key underline-color soma.dwuir.paint -- Specifies the paint to
          # use for the underline. Use `auto` to let DwUIR pick it based on
          # the text.
          #
          # |@key underline-offset -- Specifies the offset from the baseline
          # of the text in pixels.
          #
          # |@key underline-thickness -- Specifies the thickness of the underline
          # in pixels.
          matchpi(<<-WWML
            {¦ underline: true
               underline-color_⋮ auto
               underline-offset⋮ 1
               underline-thickness⋮ 1}
          WWML
          ) do
            underline = UnderlineSpec.new(
              color: underline_color == Term[:auto] ? nil : Paint.term(underline_color),
              offset: underline_offset.to(Float32),
              thickness: underline_thickness.to(Float32),
            )

            continue
          end

          # |@ soma.dwuir.node.text.selection
          #
          # |@block
          # DwUIR text nodes can display selection spans. Use `selection: true` to
          # enable this feature. Define both `selection-anchor` and `selection-span`
          # show an I-beam (if span is `0`) or a selection range (if span is nonzero).
          # |@endblock
          #
          # |@key selection-anchor -- Specifies the character index where selection
          # should start.
          #
          # |@key selection-span -- Specifies the number of characters to the right
          # (if positive) or to the left (if negative) of the anchor to highlight.
          # If `0`, an I-beam will be shown *before* the character at `selection-anchor`.
          #
          # |@key selection-fill soma.dwuir.paint -- Specifies the background color
          # of the selection, or the color of the I-beam.
          #
          # |@key selection-color soma.dwuir.paint -- Changes the color of selected
          # text (use this to improve contrast).
          #
          # |@key selection-radius -- Specifies the radius of all four corners of
          # all selection rectangles drawn (e.g. multiline or wrapped selection
          # would lead to multiple such rectangles, each of which will have this
          # corner radius set to this number).
          #
          # |@key selection-extent -- Specifies how much the selection rect should *shrink*,
          # as a fraction of `leading`. Sometimes you don't want the selection rect
          # to span the whole line in height (e.g. in multiline texts with styled
          # selections). `0` means selection height equals baseline height. `1` means
          # selection height equals line height.
          matchpi(<<-WWML
            {¦ selection: true
               selection-anchor_: (%number +i32)
               selection-span_: (%number i32)
               selection-fill_⋮ (rgb 0 0 255)
               selection-color_⋮ (rgb 255 255 255)
               selection-radius⋮ 0
               selection-extent: (%optional 1 selection-extent←(%number 0 <= _ <= 1))}
          WWML
          ) do
            anchor = selection_anchor.to(Int32)
            span = selection_span.to(Int32)
            range_b, range_e = {anchor, anchor + span}.minmax

            selection = SelectionSpec.new(
              range: range_b...range_e,
              fill: Paint.term(selection_fill),
              color: Paint.term(selection_color),
              radius: selection_radius.to(Float32),
              extent: selection_extent.to(Float32),
            )

            continue
          end

          # |@ soma.dwuir.node.text.wrap
          #
          # |@block
          # DwUIR text nodes support automatic line wrapping. Use `wrap: true` to
          # enable this feature.
          # |@endblock
          #
          # |@key wrap-ellipsis -- Sets the character sequence that will be used to
          # indicate omission in case there is not enough space to fit the whole text.
          #
          # |@key wrap-history -- How many actions should the wrapping algorithm
          # will keep in its memory before committing them. `auto` means all
          # actions are kept. This is important mainly for the placement of
          # ellipsis. The longer your ellipsis, the larger your `wrap-history`
          # should be. Unless you're wrapping a very large text, you probably
          # shouldn't set this to anything other than `auto`.
          #
          # |@key wrap-on-words -- Enables or disables wrapping on word boundaries.
          #
          # |@key wrap-on-letters -- Enables or disables wrapping on letter boundaries.
          matchpi(<<-WWML
            {¦ wrap: true
               wrap-ellipsis⋮ "..."
               wrap-history: (%optional auto wrap-history←(%any° auto (%number +i32!)))
               wrap-on-words⋮ true
               wrap-on-letters⋮ true}
          WWML
          ) do
            wrap = WrapSpec.new(
              ellipsis: wrap_ellipsis.to(String),
              on_words: wrap_on_words.true?,
              on_letters: wrap_on_letters.true?,
              bounds: Rect.new(Point.new(0, 0), space),
              history: wrap_history == Term[:auto] ? WrapHistory::INFINITE : wrap_history.to(Int32),
            )

            continue
          end

          # |@ soma.dwuir.node.text.transform
          #
          # |@block
          # The `transform` attribute lets you apply a set of transformation
          # functions before displaying the text.
          #
          # NOTE: some of the transformation functions may be *destructive*; others
          # may be *additive*; most are *neutral*. We do not allow destructive and
          # additive transformation functions to be used alongside `selection`; this
          # is because that would introduce a discrepancy between what the `text` is
          # supposed to show (including the selection anchor at the appropriate position),
          # vs. what the user actually sees.
          # |@endblock

          # |@ soma.dwuir.node.text.transform.upcase
          #
          # |@block
          # Transforms all letters in the caption to uppercase.
          # |@endblock
          matchpi %[{¦ transform: {+¦ upcase}}] do
            caption = caption.upcase

            continue
          end

          # |@ soma.dwuir.node.text.transform.dncase
          #
          # |@block
          # Transforms all letters in the caption to lowercase.
          # |@endblock
          matchpi %[{¦ transform: {+¦ dncase}}] do
            caption = caption.downcase

            continue
          end

          # |@ soma.dwuir.node.text.transform.strip
          #
          # |@block
          # Removes leading and trailing whitespace, tab, newline characters from
          # the caption.
          #
          # NOTE: see `transform` to learn why this function cannot be used along
          # with `selection`.
          # |@endblock
          matchpi %[{¦ -selection transform: {+¦ strip}}] do
            caption = caption.strip

            continue
          end

          # |@ soma.dwuir.node.text.transform.pretty
          #
          # |@block
          # Useful for "prettifying" horribly formatted caption somewhat. Do not rely
          # too much on this function; if the text is too horribly formatted, *you*
          # are much better equipped at formatting it than this fairly generic
          # function is.
          #
          # - Removes leading and trailing whitespace, tab, and newline characters.
          # - Replaces all runs of whitespace, tab, and newline characters by one
          #   such character.
          #
          # NOTE: see `transform` to learn why this function cannot be used along
          # with `selection`.
          # |@endblock
          matchpi %[{¦ -selection transform: {+¦ pretty}}] do
            caption = caption.strip.squeeze(" \n")

            continue
          end

          otherwise { }
        end

        TextSpec.new(caption,
          text_family,
          text_weight,
          text_italic,
          text_size,
          text_color,
          text_leading,
          text_tracking,
          wrap,
          selection,
          underline,
        )
      end

      otherwise { }
    end
  end

  private def render(picture : Picture, pencils : PencilServer, context : WalkContext, node : Term) : WalkFlow
    Term.case(node) do
      # |@ soma.dwuir.node.composite
      #
      # |@block
      # The `composite` node lets you scope `z-index`, and apply *opacity* to any
      # number of children. All children are going to be drawn as a unit, and only
      # then will *opacity* be applied to them. Using `composite` is therefore
      # different from setting opacity of each individual child.
      # |@endblock
      #
      # |@key children soma.dwuir.node -- Children that will be part of the composite.
      #
      # |@key opacity -- Specifies the opacity (clamped to 0-1). `0` means fully
      # transparent, and `1` means fully opaque.
      matchpi %[(composite children_+ ¦ _ opacity⋮ 1)] do
        target = Picture.new

        walk(context.copy_with(layer: LayerRank[]), children) do |subcontext, child|
          render(target, pencils, subcontext, child)
        end

        target.finish

        command = DrawComposite.new(target,
          opacity: opacity.to(Float32).clamp(0.0f32..1.0f32),
          layer: context.layer,
        )

        picture << command

        WalkFlow::Next
      end

      # |@ soma.dwuir.node.rect
      #
      # |@block
      # The `rect` node is drawn as a rectangle.
      # |@endblock
      #
      # |@key fill soma.dwuir.paint -- Sets the fill (background) paint of
      # the rectangle.
      #
      # |@key thickness-l soma.dwuir.magn -- Sets the thickness of the left
      # border of the rectangle. Relative numbers are interpreted as fractions
      # of the rectangle's width. Raw numbers receive absolute treatment (pixels).
      #
      # |@key thickness-r soma.dwuir.magn -- Sets the thickness of the right
      # border of the rectangle. Relative numbers are interpreted as fractions
      # of the rectangle's width. Raw numbers receive absolute treatment (pixels).
      #
      # |@key thickness-t soma.dwuir.magn -- Sets the thickness of the top
      # border of the rectangle. Relative numbers are interpreted as fractions
      # of the rectangle's height. Raw numbers receive absolute treatment (pixels).
      #
      # |@key thickness-b soma.dwuir.magn -- Sets the thickness of the bottom
      # border of the rectangle. Relative numbers are interpreted as fractions
      # of the rectangle's height. Raw numbers receive absolute treatment (pixels).
      #
      # |@key stroke soma.dwuir.paint -- Sets the stroke (border) paint of
      # the rectangle.
      #
      # |@key radius-tl soma.dwuir.magn -- Sets the radius of the rectangle's
      # top-left corner. Relative numbers are interpreted as fractions of
      # the length of the rectangle's longest side. Raw numbers receive
      # absolute treatment (pixels).
      #
      # |@key radius-tr soma.dwuir.magn -- Sets the radius of the rectangle's
      # top-right corner. Relative numbers are interpreted as fractions of
      # the length of the rectangle's longest side. Raw numbers receive
      # absolute treatment (pixels).
      #
      # |@key radius-bl soma.dwuir.magn -- Sets the radius of the rectangle's
      # bottom-left corner. Relative numbers are interpreted as fractions of
      # the length of the rectangle's longest side. Raw numbers receive
      # absolute treatment (pixels).
      #
      # |@key radius-br soma.dwuir.magn -- Sets the radius of the rectangle's
      # bottom-right corner. Relative numbers are interpreted as fractions of
      # the length of the rectangle's longest side. Raw numbers receive
      # absolute treatment (pixels).
      matchpi(<<-WWML
        (rect ¦ _
          fill_⋮ (rgba 0 0 0 0)
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
        unless visible?(context)
          return WalkFlow::Next
        end

        border = RectBorder.new(
          l: Magn.abst(thickness_l, Magn.abs(0)).resolve(context.bounds.w),
          r: Magn.abst(thickness_r, Magn.abs(0)).resolve(context.bounds.w),
          t: Magn.abst(thickness_t, Magn.abs(0)).resolve(context.bounds.h),
          b: Magn.abst(thickness_b, Magn.abs(0)).resolve(context.bounds.h),
          color: Paint.term(stroke),
        )

        rmax = {context.bounds.w, context.bounds.h}.max

        radii = RectRadii.new(
          tl: Magn.abst(radius_tl, Magn.abs(0)).resolve(rmax),
          tr: Magn.abst(radius_tr, Magn.abs(0)).resolve(rmax),
          bl: Magn.abst(radius_bl, Magn.abs(0)).resolve(rmax),
          br: Magn.abst(radius_br, Magn.abs(0)).resolve(rmax),
        )

        shape = RectShape.new(Paint.term(fill), border, radii)
        command = DrawShape.new(context.view, context.bounds, context.tf, context.layer, :mid, shape)
        picture << command

        WalkFlow::Next
      end

      # See `text_spec?` for docs. We use a separate method for parsing texts
      # to allow the other methods (`hit`, `measure`, etc.) have access to it
      # as well.
      matchpi %{[text]} do
        unless spec = text_spec?(node, space: context.bounds.size)
          return WalkFlow::Next
        end

        unless font = spec.font?
          return WalkFlow::Next
        end

        pencil = pencils.call(PencilRequest.new(font, spec.size, spec.leading, spec.tracking))

        spec.each_text_drawable(pencil, origin: context.bounds.tl) do |dw|
          case dw
          in TextDrawable::InlineString
            next unless visible?(context, dw.bounds)

            shape = FragShape.new(
              string: dw.string,
              font: font,
              size: spec.size,
              tracking: spec.tracking,
              underline: spec.underline,
              color: dw.selected ? spec.selection.try(&.color) || spec.color : spec.color,
            )

            command = DrawShape.new(context.view, dw.bounds, context.tf, context.layer, :mid, shape)
          in TextDrawable::Selection
            next unless selection = spec.selection
            next unless visible?(context, dw.bounds)

            shape = RectShape.new(
              fill: selection.fill,
              border: RectBorder.new,
              radii: RectRadii.all(selection.radius),
            )

            command = DrawShape.new(context.view, dw.bounds, context.tf, context.layer, dw.rank, shape)
          end

          picture << command
        end

        WalkFlow::Next
      end

      # |@ soma.dwuir.node.svg
      #
      # |@block
      # Use the `svg` node to embed an SVG document into the DwUIR.
      #
      # Use the `final-w` and `final-h` attributes along with the `resize`
      # attribute to control how the document is sized.
      # |@endblock
      #
      # |@key src soma.dwuir.resource -- Specifies what SVG document to render.
      #
      # |@key color soma.dwuir.color -- Controls the `currentColor` property#
      # in the document.
      matchpi %{(svg ¦ _ src_ color_⋮ black)} do
        resize = SvgShape::Resize::Stretch

        Term.case(node) do
          # |@ soma.dwuir.node.svg.resize
          #
          # |@block
          # Defines how the SVG document should be resized to fit in the box
          # defined by `final-w` and `final-h`.
          # |@endblock

          # |@ soma.dwuir.node.svg.resize.clip
          #
          # |@block
          # The document will not be resized. `final-w` and `final-h` will used
          # to clip it.
          # |@endblock
          matchpi %[{¦ resize: clip}] do
            resize = SvgShape::Resize::Clip
          end

          # |@ soma.dwuir.node.svg.resize.stretch
          #
          # |@block
          # The document will be stretched to fit into the box defined by `final-w`
          # and `final-h`.
          # |@endblock
          matchpi %[{¦ resize: stretch}] do
            resize = SvgShape::Resize::Stretch
          end

          # |@ soma.dwuir.node.svg.resize.keep-ratio
          #
          # |@block
          # The document will be scaled horizontally so that its width becomes
          # `final-w`; its height will be scaled as appropriate to keep the aspect
          # ratio intact. The scaled document will be centered along the vertical
          # axis as defined by `final-h`.
          # |@endblock
          matchpi %[{¦ resize: keep-ratio}] do
            resize = SvgShape::Resize::KeepRatio
          end

          otherwise { }
        end

        unless context.bounds.empty?
          shape = SvgShape.new(src, Pigment.rgba(color), resize)
          command = DrawShape.new(context.view, context.bounds, context.tf, context.layer, :mid, shape)
          picture << command
        end

        WalkFlow::Next
      end

      matchpi %{_dict} do
        WalkFlow::Recurse
      end

      otherwise do
        WalkFlow::Next
      end
    end
  end

  # Converts drawable UIR term *dwuir* to a `Picture` object, which is essentially
  # an array of draw commands.
  #
  # We do this translation to simplify & smoothen the drawing process overall; otherwise
  # it would be just too overwhelming. At `DwUIR.picture`-time we do all sorts of nasty
  # things, such as:
  #
  # - Word wrapping.
  # - Figuring out where to put selection and cursor rectangles etc. (and text
  #   features in general).
  # - Figuring out what is visible and what is not, filtering out the latter.
  # - Sorting draw commands by layers for proper draw order.
  #
  # Each draw command is almost "overly-specified"; it carries with itself the full
  # knowledge about its context in *dwuir*. In other words, each draw command specifies
  # *everything* that it needs to be drawn correctly in isolation.
  #
  # *pencils* is a callback that should serve pencil requests for this function.
  #
  # *viewport* may specify the visible area of the screen; it acts as a toplevel
  # `viewport` (as in DwUIR viewport node). You are recommended to specify this
  # parameter since it will cause out-of-view top-level draw commands to be skipped --
  # which is an important optimization.
  def picture(dwuir : Term, pencils : PencilServer, viewport : Rect = Rect.inf) : Picture
    picture = Picture.new

    walk(dwuir, viewport: viewport) do |context, node|
      render(picture, pencils, context, node)
    end

    picture.finish
    picture
  end
end
