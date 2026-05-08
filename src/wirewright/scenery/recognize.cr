module Ww::Scenery
  # The Scenery KnowledgeBase provides answers to questions related to Scenery,
  # Scenery nodes, descriptions of Scenery nodes, etc.
  module KnowledgeBase
    extend self

    # TODO: Is there a way to generate this automatically. I'm afraid it's going
    # to be rather inconvenient to maintain this and recognize() in sync...

    # :nodoc:
    LEAF_HEADS = Set{
      Term.of(:rect),
      Term.of(:text),
      Term.of(:icon),
      Term.of(:svg),
      Term.of(:img),
    }

    # :nodoc:
    PARENT_HEADS = Set{
      Term.of(:"floating"),
      Term.of(:"content"),
      Term.of(:"limit"),
      Term.of(:"padding"),
      Term.of(:"align"),
      Term.of(:"x-stack"),
      Term.of(:"y-stack"),
      Term.of(:"z-stack"),
      Term.of(:"x-wrap"),
      Term.of(:"y-wrap"),
      Term.of(:"composite"),
      Term.of(:"transform"),
      Term.of(:"viewport"),
      Term.of(:"aim"),
      Term.of(:"page"),
      Term.of(:"overlay"),
      Term.of(:"suspense"),
      Term.of(:"variant"),
      Term.of(:"observable"),
      Term.of(:"gate"),
    }

    # Returns `true` if *head* is the head (e.g. `p` in `(p "hello")`) of
    # a description of a Scenery parent node (head is followed by children).
    def parent_head?(head : Term) : Bool
      head.in?(PARENT_HEADS)
    end

    # Returns `true` if *head* is the head (e.g. `p` in `(p "hello")`) of
    # a description of a Scenery leaf node (has no children).
    def leaf_head?(head : Term) : Bool
      head.in?(LEAF_HEADS)
    end
  end

  # Builds a tree of `RecognizedNode`s based on *document*. Returns the root query node.
  #
  # - If the document is not a dict, this is simply `Inert`.
  # - If it is a dict, its items are put in a `z-stack`.
  def recognize(cache : CacheSet, *, document : Term) : Root(RecognizedNode)
    cache.recognition.epoch do
      unless dict = document.as_d?
        return Root(RecognizedNode).new(Inert.new)
      end

      Root(RecognizedNode).new(ZStack.anon(recognize(cache, nodes: dict.items)))
    end
  end

  private def recognize!(cache : CacheSet, node : Term) : RecognizedNode
    Term.case(node) do
      # |@ scenery.rect
      #
      # |@pattern
      # (rect ⍊
      #   fill_⋮ transparent
      #   thickness-l_⋮ 0
      #   thickness-r_⋮ 0
      #   thickness-t_⋮ 0
      #   thickness-b_⋮ 0
      #   stroke_⋮ transparent
      #   radius-tl_⋮ 0
      #   radius-tr_⋮ 0
      #   radius-bl_⋮ 0
      #   radius-br_⋮ 0
      #   min-w⋮ 0
      #   min-h⋮ 0)
      #
      # |@key fill scenery.paint
      # The paint with which to fill the rectangle.
      #
      # |@key thickness-l scenery.unit
      # Thickness of the left border (pixels). Relative values are resolved
      # using the final width of the rect.
      #
      # |@key thickness-r scenery.unit
      # Thickness of the right border (pixels). Relative values are resolved
      # using the final width of the rect.
      #
      # |@key thickness-t scenery.unit
      # Thickness of the top border (pixels). Relative values are resolved
      # using the final height of the rect.
      #
      # |@key thickness-b scenery.unit
      # Thickness of the bottom border (pixels). Relative values are resolved
      # using the final height of the rect.
      #
      # |@key stroke scenery.paint
      # The paint with which to fill the borders.
      #
      # |@key radius-tl scenery.unit
      # Radius of the _t_op-_l_eft corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key radius-tr scenery.unit
      # Radius of the _t_op-_r_ight corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key radius-bl scenery.unit
      # Radius of the _b_ottom-_l_eft corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key radius-br scenery.unit
      # Radius of the _b_ottom-_r_ight corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key min-w
      # Minimum width of the rectangle, in pixels.
      #
      # |@key min-h
      # Minimum height of the rectangle, in pixels.
      #
      # |@block
      # Displays a rectangle with optional borders, corner radii, stroke, and fill.
      # Additionally, by setting all corner radii to `(* 1)` (i.e., 100%, max), you
      # can obtain a circle.
      #
      # ```
      # (rect
      #   fill: gray
      #   stroke: orange
      #   thickness-l: 3
      #   thickness-r: 3
      #   thickness-t: 3
      #   thickness-b: 3
      #   radius-tl: (* 1)
      #   radius-tr: (* 1)
      #   radius-bl: (* 1)
      #   radius-br: (* 1))
      # ```
      matchpi(<<-WWML) do
      (rect ⍊
         fill_⋮ transparent
         thickness-l_⋮ 0
         thickness-r_⋮ 0
         thickness-t_⋮ 0
         thickness-b_⋮ 0
         stroke_⋮ transparent
         radius-tl_⋮ 0
         radius-tr_⋮ 0
         radius-bl_⋮ 0
         radius-br_⋮ 0
         min-w⋮ 16
         min-h⋮ 16)
      WWML
        RectShape.new(
          thickness: RectThickness.new(
            l: Unit.px(thickness_l, fallback: Unit.px(0.0)),
            r: Unit.px(thickness_r, fallback: Unit.px(0.0)),
            t: Unit.px(thickness_t, fallback: Unit.px(0.0)),
            b: Unit.px(thickness_b, fallback: Unit.px(0.0)),
          ),
          radii: RectRadii.new(
            tl: Unit.px(radius_tl, fallback: Unit.px(0.0)),
            tr: Unit.px(radius_tr, fallback: Unit.px(0.0)),
            bl: Unit.px(radius_bl, fallback: Unit.px(0.0)),
            br: Unit.px(radius_br, fallback: Unit.px(0.0)),
          ),
          fill: Paint.recognize(fill),
          stroke: Paint.recognize(stroke),
          min_size: Point[min_w.to(Magnitude), min_h.to(Magnitude)]
        )
      end

      # |@ scenery.text
      #
      # |@pattern
      # (text ⍊
      #   caption_string
      #   size⋮ 16
      #   leading_⋮ 1
      #   tracking_⋮ normal
      #   color_⋮ black
      #   ⋮placeholder)
      #
      # |@key caption
      # The text that should be displayed.
      #
      # |@key size
      # The pixel size of the text.
      #
      # |@key leading scenery.unit
      # Line spacing (relative; resolved using line height) for both soft- and
      # hard-wrapped lines.
      #
      # Use `scenery.unit.px` to specify line height in pixels.
      #
      # |@key tracking scenery.unit
      # Letter spacing (relative; resolved using the width of the whitespace
      # character in the first matching font).
      #
      # Use `scenery.unit.px` to specify letter spacing in pixels.
      #
      # `normal` uses the font's preferred letter spacing.
      #
      # |@key color pigment
      # The color of the text.
      #
      # |@key placeholder scenery.rect
      # a rectangle to show while the fonts are loading or unavailable. The rectangle
      # is replicated across lines to mimic line wrapping etc. This is a more local
      # and wrapping-aware alternative to simply putting the text inside `scenery.suspense`.
      #
      # |@block
      # Displays a string of text. Supports font stacking, selections, and underlines.
      #
      # ```
      # (text caption: "Hello World")
      #
      # (text
      #   caption: "Hello World"
      #   color: white
      #   font:
      #     (list
      #       (font "IBM Plex Sans")
      #       (font "Noto Sans")))
      # ```
      matchpi(<<-WWML) do
      (text ⍊
        caption_string
        size⋮ 16
        leading_⋮ 1
        tracking_⋮ normal
        color_⋮ black)
      WWML
        queries = Term.case(node) do
          # |@ scenery.text.font
          #
          # |@pattern
          # (text ⍊ font: (list prefs_*))
          #
          # |@key prefs resource
          # A list of resource queries in the order of preference.
          #
          # |@block
          # Set `font` to a list of font queries to enable font stacking.
          matchpi %{{¦ font: (list prefs_*)}} do
            prefs.items.to_compact_readonly_slice do |term|
              next unless pref = ResourceService.query?(term)

              Asset::FontQuery.new(pref)
            end
          end

          # |@ scenery.text.font
          #
          # |@pattern
          # (text ⍊
          #   font⋮ "IBM Plex Sans"
          #   weight_: (%optional 400 (%number +i32))
          #   italic⋮ false)
          #
          # |@key font
          # The name of the font.
          #
          # |@key weight
          # The preferred font weight. 100 is, roughly, *extra thin*; and 900 is, roughly, *black*.
          #
          # |@key italic
          # Whether to prefer an italic versions.
          #
          # |@block
          # Tries to find the font in the runtime directory (this is basically a
          # `resource.font` query but flattened right into the text).
          matchpi(<<-WWML, font: String, weight: Int32, italic: Bool) do
          {¦ font⋮ "IBM Plex Sans"
             weight_: (%optional 400 (%number +i32))
             italic⋮ false}
          WWML
            query = ResourceService.font(font, weight, italic)

            Slice[Asset::FontQuery.new(query)]
          end

          # |@ scenery.text.font
          #
          # |@pattern
          # (text ⍊ font: query_)
          #
          # |@key query resource
          # The query that should be used to fetch the font.
          #
          # |@block
          # Tries to find the font using the given query.
          matchpi %{{¦ font: term_}} do
            continue unless query = ResourceService.query?(term)

            Slice[Asset::FontQuery.new(query)]
          end

          otherwise do
            Slice(Asset::FontQuery).empty
          end
        end

        return Inert.new if queries.empty?

        string = caption.to(String)

        Term.case(node) do
          # |@ scenery.text.case
          #
          # |@pattern
          # (text ⍊ case: upcase)
          #
          # |@block
          # Transforms all characters in the caption to uppercase.
          matchpi %{{¦ case: upcase}} do
            string = string.upcase
            continue
          end

          # |@ scenery.text.case
          #
          # |@pattern
          # (text ⍊ case: dncase)
          #
          # |@block
          # Transforms all characters in the caption to lowercase.
          matchpi %{{¦ case: dncase}} do
            string = string.downcase
            continue
          end

          # |@ scenery.text.case
          #
          # |@pattern
          # (text ⍊ case: title)
          #
          # |@block
          # Transforms the caption to titlecase.
          matchpi %{{¦ case: title}} do
            string = string.titleize
            continue
          end

          # |@ scenery.text.case
          #
          # |@pattern
          # (text ⍊ case: upcase-first)
          #
          # |@block
          # Transforms the first letter in the caption to uppercase.
          matchpi %{{¦ case: upcase-first}} do
            unless string.empty?
              string = string[0].upcase + string[1..]
            end
            continue
          end

          # |@ scenery.text.transform
          #
          # |@pattern
          # (text ⍊ transform: strip)
          #
          # |@block
          # Removes whitespace from the beginning and the end of the caption.
          #
          # This transform is incompatible with `underline` and `selection`,
          # since both of them require stable character indices.
          matchpi %{{¦ transform: strip}} do
            string = string.strip
            continue
          end

          # |@ scenery.text.transform
          #
          # |@pattern
          # (text ⍊ transform: pretty)
          #
          # |@block
          # Removes whitespace from the beginning and the end of the caption,
          # and replaces repeated whitespace and newlines with just one instance
          # of each.
          #
          # This transform is incompatible with `underline` and `selection`,
          # since both of them require stable character indices.
          matchpi %{{¦ transform: pretty}} do
            string = string.strip.squeeze(" \n")
            continue
          end

          otherwise { }
        end

        if placeholder = node[:placeholder]?
          placeholder = recognize(cache, node: placeholder).as?(RectShape)
        end

        seln = Pf::GraphemeSeln.new(string)

        unibreaks = Slice(Unibreak::LineBreak).new(seln.bytesize, Unibreak::LineBreak::Indeterminate)
        Unibreak.set_linebreaks_utf8(seln.to_slice, seln.bytesize, lang: "", brks: unibreaks)

        Text.new(seln, unibreaks, queries,
          size: size.to(Magnitude),
          color: Pigment.rgba(color),
          leading: Unit.rel(leading, Unit.rel(0)),
          tracking: tracking == Term.of(:normal) ? nil : Unit.rel(tracking, Unit.rel(1)),
          selections: merge(selections(node, seln)),
          underlines: merge(underlines(node, seln)),
          placeholder: placeholder,
        )
      end

      # |@ scenery.icon
      #
      # |@pattern
      # (icon ⍊
      #   name_string
      #   size⋮ 16
      #   color_⋮ black
      #   ⋮placeholder)
      #
      # |@key name
      # Codepoint name. The codepoint is fetched from the `.codepoints` file associated
      # with the icon font. See `scenery.icon.codepoints` to learn more.
      #
      # |@key size
      # The size of the icon, in pixels (like text size).
      #
      # |@key color pigment
      # The color to use when drawing the icon.
      #
      # |@key placeholder scenery.rect
      # Works like in `text` and other nodes: specifies a rectangle to show while
      # the icon is loading or unavailable.
      #
      # |@block
      # Displays an icon from an icon font. The icon is fetched based on its human-
      # readable name instead of the raw codepoint number. This, and performance
      # (icon is much faster than text as it circumvents all shaping and line wrapping
      # etc.), are the only reasons to use `icon` over `text`.
      #
      # ```
      # (icon name: "zoom_in" size: 14 color: white font: "Material Icons")
      # ```
      matchpi(<<-WWML) do
      (icon ⍊
        name_string
        size⋮ 16
        color_⋮ black)
      WWML
        codepoints_query = Term.case(node) do
          # |@ scenery.icon.codepoints
          #
          # |@pattern
          # (icon ⍊ font_string)
          #
          # |@key font
          # Retrieves the codepoints file by issuing a `resource.codepoints` query
          # for *font*. This should find codepoints files that are siblings of
          # the corresponding font.
          #
          # See `resource.codepoints` for more info.
          matchpi %{{¦ -codepoints font_string}} do
            ResourceService.codepoints(font.to(String))
          end

          # |@ scenery.icon.codepoints
          #
          # |@pattern
          # (icon ⍊ codepoints_)
          #
          # |@key codepoints resource
          # Retrieves the codepoints file by issuing an arbitrary resource query.
          #
          # See `resource.codepoints` for more info.
          matchpi %{{¦ codepoints: term_}} do
            ResourceService.query?(term)
          end

          otherwise { }
        end

        font_query = Term.case(node) do
          # |@ scenery.icon.font
          #
          # |@pattern
          # (icon ⍊ font_string)
          #
          # |@key font
          # The name of the font. Search is conducted in the runtime fonts directory.
          #
          # See `resource.font` for more info.
          matchpi %{{¦ font_string}} do
            ResourceService.font(font.to(String), weight: 400, italic: false)
          end

          # |@ scenery.icon.font
          #
          # |@pattern
          # (icon ⍊ font_)
          #
          # |@key font resource
          # An arbitrary resource query to retrieve the font.
          #
          # See `resource.font` for more info.
          matchpi %{{¦ font: term_}} do
            ResourceService.query?(term)
          end

          otherwise { }
        end

        return Inert.new unless font_query && codepoints_query

        if placeholder = node[:placeholder]?
          placeholder = recognize(cache, node: placeholder).as?(RectShape)
        end

        Icon.new(
          name: name.to(String),
          font: Asset::FontQuery.new(font_query),
          codepoints: Asset::CodepointsQuery.new(codepoints_query),
          size: size.to(Magnitude),
          color: Pigment.rgba(color),
          placeholder: placeholder,
        )
      end

      # |@ scenery.svg
      #
      # |@pattern
      # (svg ⍊
      #   src_
      #   color_⋮ black
      #   fit⋮ stretch
      #   ⋮placeholder)
      #
      # |@key src resource
      # The query to use to retrieve the SVG.
      #
      # |@key color pigment
      # The color to use as the `currentColor`.
      #
      # |@key fit
      # How the SVG should be resized:
      #
      # - `clip`: keep the SVG's intrinsic width and height, clip on overflow.
      # - `stretch`: stretch the SVG horizontally and vertically to fit.
      # - `keep-ratio`: stretch the SVG proportionally.
      #
      # |@key placeholder scenery.rect
      # The rectangle to show while the SVG is loading or unavailable.
      #
      # |@block
      # Displays an SVG image.
      #
      # Reference: [PlutoSVG](https://github.com/sammycage/plutosvg).
      #
      # ```
      # (svg
      #   src: '⎡<svg ...></svg>⎤
      #   fit: keep-ratio
      #   color: white)
      # ```
      matchpi(<<-WWML) do
      (svg ⍊
        src_
        color_⋮ black
        fit⋮ stretch)
      WWML
        return Inert.new unless query = ResourceService.query?(src)

        if placeholder = node[:placeholder]?
          placeholder = recognize(cache, node: placeholder).as?(RectShape)
        end

        case fit
        when Term.of(:clip)
          svg_fit = Svg::Fit::Clip
        when Term.of(:stretch)
          svg_fit = Svg::Fit::Stretch
        when Term.of(:"keep-ratio")
          svg_fit = Svg::Fit::KeepRatio
        else
          svg_fit = Svg::Fit::Stretch
        end

        Svg.new(
          src: Asset::SvgQuery.new(query),
          color: Pigment.rgba(color),
          fit: svg_fit,
          placeholder: placeholder,
        )
      end

      # |@ scenery.img
      #
      # |@pattern
      # (img ⍊
      #   src_
      #   opacity: (%optional 1 opacity←(%number 0 <= _ <= 1))
      #   radius-tl_⋮ 0
      #   radius-tr_⋮ 0
      #   radius-bl_⋮ 0
      #   radius-br_⋮ 0
      #   fit_⋮ stretch
      #   tile⋮ false
      #   resize-w_⋮ (* 1)
      #   resize-h_⋮ (* 1)
      #   ⋮placeholder)
      #
      # |@key src resource
      # The query to use to retrieve the image.
      #
      # The following media types are supported:
      #
      # - `image/png`
      # - `image/jpeg`
      # - `image/bmp`
      # - `image/gif`
      # - `image/x-portable-pixmap`
      #
      # |@key opacity
      # The opacity of the image (0-1).
      #
      # |@key radius-tl scenery.unit
      # Radius of the _t_op-_l_eft corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key radius-tr scenery.unit
      # Radius of the _t_op-_r_ight corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key radius-bl scenery.unit
      # Radius of the _b_ottom-_l_eft corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key radius-br scenery.unit
      # Radius of the _b_ottom-_r_ight corner (pixels). Relative values are resolved
      # using the maximum corner radius for the rect.
      #
      # |@key fit
      # How the image should be resized:
      #
      # - `(align ⍊ l⋮ 0 t⋮ 0)`: aligns the image's l, t point and the layout box's
      #   (l and t are 0-1).
      # - `(pan ⍊ l⋮ 0 t⋮ 0)`: moves the image right, down (positive); left, up (negative)
      #   by some number of pixels. In this mode, the image node acts as a kind of "viewport".
      # - `stretch`: stretch the image to fit in the layout box.
      #
      # |@key tile
      # Whether to tile the image.
      #
      # |@key resize-w scenery.unit
      # Target width of the image (pixels). Relative values are resolved using
      # the image's width. This lets you resize the image independently of the layout box.
      #
      # |@key resize-h scenery.unit
      # Target height of the image (pixels). Relative values are resolved using
      # the image's height. This lets you resize the image independently of the layout box.
      #
      # |@key placeholder scenery.rect
      # The rectangle to show while the image is loading or unavailable.
      #
      # |@block
      # Displays a raster image.
      #
      # ```
      # (img src: (uri "https://picsum.photos/300/300"))
      # ```
      matchpi(<<-WWML) do
      (img ⍊
        src_
        opacity: (%optional 1 opacity←(%number 0 <= _ <= 1))
        radius-tl_⋮ 0
        radius-tr_⋮ 0
        radius-bl_⋮ 0
        radius-br_⋮ 0
        fit_⋮ stretch
        tile⋮ false
        resize-w_⋮ (* 1)
        resize-h_⋮ (* 1))
      WWML
        return Inert.new unless query = ResourceService.query?(src)

        if placeholder = node[:placeholder]?
          placeholder = recognize(cache, node: placeholder).as?(RectShape)
        end

        img_fit = Term.case(fit) do
          matchpi %{(align ⍊ l⋮ 0 t⋮ 0)} do
            Img::Fit::Align.new(Point.new(l.to(Magnitude), t.to(Magnitude)))
          end

          matchpi %{(pan ⍊ l⋮ 0 t⋮ 0)} do
            Img::Fit::Pan.new(Point.new(l.to(Magnitude), t.to(Magnitude)))
          end

          matchpi %{stretch} do
            Img::Fit::Stretch.new
          end

          otherwise { Img::Fit::Stretch.new }
        end

        Img.new(
          src: Asset::ImageQuery.new(query),
          radii: RectRadii.new(
            tl: Unit.px(radius_tl, fallback: Unit.px(0)),
            tr: Unit.px(radius_tr, fallback: Unit.px(0)),
            bl: Unit.px(radius_bl, fallback: Unit.px(0)),
            br: Unit.px(radius_br, fallback: Unit.px(0)),
          ),
          fit: img_fit,
          tile: tile.true?,
          opacity: opacity.to(Magnitude),
          resize_w: Unit.px(resize_w, fallback: Unit.rel(1)),
          resize_h: Unit.px(resize_h, fallback: Unit.rel(1)),
          placeholder: placeholder,
        )
      end

      # |@ scenery.floating
      #
      # |@pattern
      # (floating children_+ ⍊ x⋮ false y⋮ false)
      #
      # |@key children scenery
      #
      # |@key x
      # Whether to disable downbound size propagation on the X-axis.
      #
      # |@key y
      # Whether to disable downbound size propagation on the Y-axis.
      #
      # |@block
      # Disables upbound size propagation for a z-stack of *children* on either
      # or both axes. This effectively zeros the upbound size of *children*.
      #
      # *children* will still participate in positioning. They will also continue
      # receiving downbound sizes from parents, so you may need a nested `content`
      # node as well if you intend to completely seal *children* off from layout.
      matchpi %{(floating subterms_+ ⍊ x⋮ false y⋮ false)} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Floating.new(children, x.to(Bool), y.to(Bool))
      end

      # |@ scenery.content
      #
      # |@pattern
      # (content children_+ ⍊ x⋮ false y⋮ false)
      #
      # |@key children scenery
      #
      # |@key x
      # Whether to disable upbound size propagation on the X-axis.
      #
      # |@key y
      # Whether to disable upbound size propagation on the Y-axis.
      #
      # |@block
      # Disables downbound size propagation for a z-stack of *children* on either or
      # both axes. This removes all size restrictions set for them by the parent. In
      # some cases, this may cause *children* to overflow. Such overflows are usually
      # handled by `viewport`.
      matchpi %{(content subterms_+ ⍊ x⋮ false y⋮ false)} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Content.new(children, x.to(Bool), y.to(Bool))
      end

      # |@ scenery.limit
      #
      # |@pattern
      # (limit children_+ ⍊
      #   min-w_⋮ 0
      #   min-h_⋮ 0
      #   max-w_⋮ ∞
      #   max-h_⋮ ∞)
      #
      # |@key children scenery
      #
      # |@key min-w scenery.unit
      # The minimum width constraint (pixels; set to `0` for effectively unconstrained).
      # Relative values are resolved in terms of the downbound maximum width, so e.g.
      # `max-w: (* 0.5)` is [very roughly] like `max-width: 50%` in CSS.
      #
      # |@key min-h scenery.unit
      # The minimum  height constraint (pixels; set to `0` for effectively unconstrained).
      # Relative values are resolved in terms of the downbound maximum height.
      #
      # |@key max-w scenery.unit
      # The maximum width constraint (pixels; or `∞` for unconstrained). Relative
      # values are resolved in terms of the downbound maximum width.
      #
      # |@key max-h scenery.unit
      # The maximum height constraint (pixels; or `∞` for unconstrained). Relative
      # values are resolved in terms of the downbound maximum height.
      #
      # |@block
      # Modifies the downbound sizes for a z-stack of *children* to fit in the range
      # defined by min-w, max-w (inclusive), and min-h, max-h (inclusive).
      #
      # NOTE: Setting max-w/h to `∞` is not the same as using `scenery.content`; the former
      # is advisory, the latter is forced. In other words, the children will be content-sized
      # only if no further restrictions are placed above or below this `limit`.
      #
      # NOTE: Limit is top-down. Most Scenery nodes have the behavior of filling all
      # available space. So if you set `max-w`, chances are, the children will size
      # themselves to `max-w`. If you want to set a bottom-up limit instead, use
      # `scenery.clamp`. Child sizes will interact with `clamp`, which will, in turn,
      # report the resulting clamped size to the parent, affecting its sizing decisions.
      # This is a slightly different flow from `scenery.limit`.
      matchpi(<<-WWML) do
      (limit subterms_+ ⍊
        min-w_⋮ 0
        min-h_⋮ 0
        max-w_⋮ ∞
        max-h_⋮ ∞)
      WWML
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        u_min_w = Unit.px(min_w, fallback: Unit.px(0))
        u_min_h = Unit.px(min_h, fallback: Unit.px(0))

        u_max_w = max_w == Term.of(:∞) ? nil : Unit.px(max_w, fallback: Unit.px(0))
        u_max_h = max_h == Term.of(:∞) ? nil : Unit.px(max_h, fallback: Unit.px(0))

        Limit.new(children, u_min_w, u_max_w, u_min_h, u_max_h)
      end

      matchpi(<<-WWML) do
      (clamp subterms_+ ⍊
        min-w_⋮ 0
        min-h_⋮ 0
        max-w_⋮ ∞
        max-h_⋮ ∞)
      WWML
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        u_min_w = Unit.px(min_w, fallback: Unit.px(0))
        u_min_h = Unit.px(min_h, fallback: Unit.px(0))

        u_max_w = max_w == Term.of(:∞) ? nil : Unit.px(max_w, fallback: Unit.px(0))
        u_max_h = max_h == Term.of(:∞) ? nil : Unit.px(max_h, fallback: Unit.px(0))

        Clamp.new(children, u_min_w, u_max_w, u_min_h, u_max_h)
      end

      # |@ scenery.padding
      #
      # |@pattern
      # (padding children_+ ⍊ pl⋮ 0 pr⋮ 0 pt⋮ 0 pb⋮ 0)
      #
      # |@key children scenery
      #
      # |@key pl
      # Padding on the left side (pixels).
      #
      # |@key pr
      # Padding on the right side (pixels).
      #
      # |@key pt
      # Padding on the top side (pixels).
      #
      # |@key pb
      # Padding on the bottom side (pixels).
      #
      # |@block
      # Pads a z-stack of *children* on the sides.
      matchpi %{(padding subterms_+ ⍊ pl⋮ 0 pr⋮ 0 pt⋮ 0 pb⋮ 0)} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Padding.new(children, pl.to(Magnitude), pr.to(Magnitude), pt.to(Magnitude), pb.to(Magnitude))
      end

      # |@ scenery.align
      #
      # |@pattern
      # (align children_+ ⍊ x⋮ 0 y⋮ 0)
      #
      # |@key children scenery
      #
      # |@key x
      # The X-coordinate of the point to align (0-1).
      #
      # |@key y
      # The Y-coordinate of the point to align (0-1).
      #
      # |@block
      # Moves a z-stack of *children* so that its *x*, *y* point (resolved using its
      # content size) aligns with that of the parent.
      #
      # ```
      # ;; Puts at the center of parent (read as: "align center points").
      # (align x: 0.5 y: 0.5
      #   (text caption: "Hello World"))
      #
      # ;; Puts at the bottom-right corner of parent (read as: "align bottom-right points").
      # (align x: 1 y: 1
      #   (text caption: "Hello World"))
      # ```
      matchpi %{(align subterms_+ ⍊ x⋮ 0 y⋮ 0)} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Align.new(children, pivot: Point[x.to(Magnitude), y.to(Magnitude)])
      end

      # |@ scenery.stack
      #
      # |@pattern
      # (x⫽y-stack children_+ ⍊ gap⋮ 0)
      #
      # |@key children scenery
      #
      # |@key gap
      # The gap between *children* (in pixels).
      #
      # |@block
      # Arranges *children* in a row (`x-stack`) or a column (`y-stack`), with
      # optional gaps in between.
      #
      # `x-stack` and `y-stack` are "bare bones" nodes which may overflow if you're
      # not careful. Use `x/y-wrap` nodes if you want wrapping; they expand into
      # `x/y-stack` nodes eventually.
      #
      # For x-stack, the *main* axis is X and the *cross* axis is Y.
      # For y-stack, the *main* axis is Y and the *cross* axis is X.
      #
      # Children may be assigned a *fraction*, `fr: _number`. Each fr item first
      # claims its minimum size, and the remaining space is then divided proportionally
      # according to `fr: _number`. For example, in `{fr: 2}, {}, {fr: 1}, {fr: 3}, {}`
      # the first fr item takes `2/6` of the remaining space, the second one `1/6`,
      # the third one `3/6`.
      #
      # If the stack is content-sized, `fr` is ignored and items size according to
      # their content.
      #
      # Due to minimum size claims, `fr` may sometimes behave unintuitively, in that it
      # may not force overflow when you might want it to. You can debug min-size claims
      # using `fr-0`, which is reserved to say "size exactly to your min-size on
      # the main axis".
      #
      # For example, if you have a y-stack with two children, `{fr: 1} {}`, representing,
      # perhaps, some content and a bottom bar, you may face a situation where the content
      # claims a lot of space for its minimum size, pushing the bar down; even though you'd
      # like the content to overflow, drawing on top of the bar. This isn't something you'd
      # want to have in a real UI, but while developing, this is what one's intuition would
      # expect. This is not the case in Scenery, however; instead, the bar is pushed down.
      #
      # An even less intuitive scenario is when you have a list of items. You make the list
      # `fr-1`. This may cause the list to take much more space than you might have expected.
      # This happens for the same reason as above: the items of the list claim lots of min
      # space ahead-of-time. Remember that e.g. for text, min-width is the minimum width
      # of a word in the text; and min-height is the height of the text wrapped at min-width.
      # This can claim lots of space when you factor in line height, gaps, between items and
      # so on.
      #
      # Personifying a little bit, stacks really don't want to overflow on the main axis;
      # so they claim as much space as possible, eagerly. Some items, such as `text`, may
      # report large values for the main axis.
      #
      # To fix this, you need to prove to your stack that the `fr` item won't overflow.
      # This can be done in several fairly intuitive ways, namely using nodes that handle
      # or otherwise work with overflow: `scenery.viewport`, `scenery.floating`, and so on.
      # Most likely you'd want `scenery.viewport`, as it cushions overflow *and* min-size claims.
      matchpi(
        %{(head←x-stack subterms_+ ⍊ gap⋮ 0)},
        %{(head←y-stack subterms_+ ⍊ gap⋮ 0)},
      ) do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        shares = subterms.items.to_readonly_slice do |subterm|
          Term.case(subterm) do
            matchpi %{{¦ ±fr}} { FrShare.new(fr.to(Magnitude)) }
            otherwise { ContentShare.new }
          end
        end

        axis = head == Term.of(:"x-stack") ? Axis::X : Axis::Y

        XYStack.new(axis, children, shares, gap.to(Magnitude))
      end

      # |@ scenery.wrap
      #
      # |@pattern
      # (x⫽y-wrap children_+ ⍊ gap⋮ 0 gap-x⋮ 0 gap-y⋮ 0)
      #
      # |@key children scenery
      #
      # |@key gap
      # The horizontal and vertical gap simultaneously (in pixels).
      #
      # |@key gap-x
      # The horizontal gap (in pixels).
      #
      # |@key gap-y
      # The vertical gap (in pixels).
      #
      # |@block
      # Arranges *children* in a column of rows (`x-wrap`) or a row of columns
      # (`y-wrap`), creating new columns (`x-wrap`) or rows (`y-wrap`) as needed
      #
      # This node expands into a `y-stack` of `x-stack` *line*s (`x-wrap`) or
      # an `x-stack` of `y-stack` *line*s (`y-wrap`), so e.g. `fr: _number` markings
      # on *children* apply within each *line*, negotiating the remaining space
      # with *children* that ended up in the same line.
      #
      # Setting *eq* to `true` makes *line*s `fr: 1`, meaning they will distribute
      # the remaining cross axis space equally. Otherwise, children will be content-sized
      # which is sometimes not what you want (as in e.g. a `y-wrap` of `x-wrap`s).
      matchpi %{(head←(%any x-wrap y-wrap) subterms_+ ⍊ gap⋮ 0 gap-x⋮ 0 gap-y⋮ 0 shares_⋮ content)} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        axis = head == Term.of(:"x-wrap") ? Axis::X : Axis::Y

        item_shares = subterms.items.to_readonly_slice do |subterm|
          Term.case(subterm) do
            matchpi %{{¦ ±fr}} { FrShare.new(fr.to(Magnitude)) }
            otherwise { ContentShare.new }
          end
        end

        line_shares = Term.case(shares) do
          # fr: 1 fr: 1 ... fr: 1
          matchpi %{fr} do
            ->(line_count : Int32) do
              Slice(Share).new(line_count, FrShare.new(1), read_only: true)
            end
          end

          # content content ... content
          otherwise do
            ->(line_count : Int32) do
              Slice(Share).new(line_count, ContentShare.new, read_only: true)
            end
          end
        end

        gap_point = Point[
          Math.max(gap.to(Magnitude), gap_x.to(Magnitude)),
          Math.max(gap.to(Magnitude), gap_y.to(Magnitude)),
        ]

        XYWrap.new(axis, children, item_shares, line_shares, gap_point)
      end

      # |@ scenery.composite
      #
      # |@pattern
      # (composite children_+ ⍊ opacity⋮ 1)
      #
      # |@key children scenery
      #
      # |@key opacity
      # The opacity for all of children (0-1).
      #
      # |@block
      # Sets the opacity for a z-stack of *children* in total (as a layer).
      matchpi %{(composite subterms_+ ⍊ opacity⋮ 1)} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Composite.new(children, opacity.to(Magnitude))
      end

      # |@ scenery.transform
      #
      # |@pattern
      # (transform children_+ ⍊
      #   origin-l⋮ 0
      #   origin-t⋮ 0
      #   pivot-l⋮ 0
      #   pivot-t⋮ 0
      #   dl_⋮ 0
      #   dt_⋮ 0
      #   angle: 0
      #   scale: 1)
      #
      # |@key children scenery
      #
      # |@key origin-l
      # Sets the X-coordinate (_l_eft) of the origin point (in pixels).
      #
      # |@key origin-t
      # Sets the Y-coordinate (_t_op) of the origin point (in pixels).
      #
      # |@key pivot-l
      # Sets the X-coordinate (_l_eft) of the pivot point (in pixels).
      #
      # |@key pivot-t
      # Sets the Y-coordinate (_t_op) of the pivot point (in pixels).
      #
      # |@key dl scenery.unit
      # Sets the translation offset when at the origin point, on the X-axis
      # (pixels; short for _d_elta _l_eft). Relative values are resolved using
      # *available X-axis space*, the difference between the size of *children*
      # and the size of `transform`'s own box.
      #
      # |@key dt scenery.unit
      # Sets the translation offset when at the origin point, on the Y-axis
      # (pixels; short for _d_elta _t_op). Relative values are resolved using
      # available Y-axis space.
      #
      # |@key angle
      # Sets the rotation angle.
      #
      # |@key scale
      # Sets the scale (`1` for the original size, `0.5` to make 50% smaller, `1.5` to
      # make 50% larger, `2` to make twice larger, etc.)
      #
      # |@block
      # Applies the configured transform to a z-stack of *children*.
      #
      # NOTE: The transform does not participate in layout. So for example, *dl* and
      # *dt* will not affect box size; neither will *scale* or *angle*.
      matchpi(<<-WWML) do
      (transform subterms_+ ⍊
        origin-l⋮ 0
        origin-t⋮ 0
        pivot-l⋮ 0
        pivot-t⋮ 0
        dl_⋮ 0
        dt_⋮ 0
        angle⋮ 0
        scale⋮ 1)
      WWML
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        translation_tf = Translation.new(
          dl: Unit.px(dl, fallback: Unit.px(0)),
          dt: Unit.px(dt, fallback: Unit.px(0)),
          origin: Point[origin_l.to(Magnitude), origin_t.to(Magnitude)],
        )

        rotation_tf = Rotation.new(
          angle: angle.to(Magnitude) % 360.0f32,
          origin: Point[pivot_l.to(Magnitude), pivot_t.to(Magnitude)],
        )

        scale_tf = Scale.new(factor: scale.to(Magnitude).clamp(0.1f32..8f32))

        Transform.new(children, translation_tf, rotation_tf, scale_tf)
      end

      # |@ scenery.viewport
      #
      # |@pattern
      # (viewport children_+ ⍊
      #   aim⋮ true
      #   page-x_⋮ 0
      #   page-y_⋮ 0
      #   offset-x_⋮ 0
      #   offset-y_⋮ 0
      #   radius-tl_⋮ 0
      #   radius-tr_⋮ 0
      #   radius-bl_⋮ 0
      #   radius-br_⋮ 0)
      #
      # |@key children scenery
      #
      # |@key aim
      # Whether to track nested `aim` nodes and `selection-aim` selections. If
      # enabled, aims are preferred over *page-x* and *page-y*.
      #
      # |@key page-x scenery.unit
      # TODO: How to describe this?
      #
      # |@key page-y scenery.unit
      # TODO: How to describe this?
      #
      # |@key offset-x scenery.unit
      # TODO: How to describe this?
      #
      # |@key offset-y scenery.unit
      # TODO: How to describe this?
      #
      # |@key radius-tl scenery.unit
      # Radius of the _t_op-_l_eft corner (pixels). Relative values are resolved
      # using the maximum corner radius for the viewport box.
      #
      # |@key radius-tr scenery.unit
      # Radius of the _t_op-_r_ight corner (pixels). Relative values are resolved
      # using the maximum corner radius for the viewport box.
      #
      # |@key radius-bl scenery.unit
      # Radius of the _b_ottom-_l_eft corner (pixels). Relative values are resolved
      # using the maximum corner radius for the viewport box.
      #
      # |@key radius-br scenery.unit
      # Radius of the _b_ottom-_r_ight corner (pixels). Relative values are resolved
      # using the maximum corner radius for the viewport box.
      #
      # |@block
      # Clips a z-stack of *children*. Follows the *aim rect*, which is a rect that is
      # the union of the visual bound rects of all `scenery.aim` nodes under the viewport
      # (i.e., not under nested viewports).
      #
      # In other words, a viewport can act as a "camera" that follows nested `aim` nodes
      # and anchors of `selection-aim: true` selections (I-beams).
      #
      # Aiming can be disabled by setting `aim: false`.
      #
      # A viewport absorbs all nested aims regardless of whether its `aim: true` or
      # `aim: false`.
      matchpi(<<-WWML) do
      (viewport subterms_+ ⍊
        aim⋮ true
        page-x_⋮ 0
        page-y_⋮ 0
        offset-x_⋮ 0
        offset-y_⋮ 0
        radius-tl_⋮ 0
        radius-tr_⋮ 0
        radius-bl_⋮ 0
        radius-br_⋮ 0)
      WWML
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Viewport.new(children,
          aim: aim.to(Bool),
          page_x: Unit.rel(page_x, fallback: Unit.rel(0)),
          page_y: Unit.rel(page_y, fallback: Unit.rel(0)),
          offset_x: Unit.rel(offset_x, fallback: Unit.rel(0)),
          offset_y: Unit.rel(offset_y, fallback: Unit.rel(0)),
          radii: RectRadii.new(
            tl: Unit.px(radius_tl, fallback: Unit.px(0)),
            tr: Unit.px(radius_tr, fallback: Unit.px(0)),
            bl: Unit.px(radius_bl, fallback: Unit.px(0)),
            br: Unit.px(radius_br, fallback: Unit.px(0)),
          ),
        )
      end

      # |@ scenery.aim
      #
      # |@pattern
      # [aim children_+]
      #
      # |@key children scenery
      #
      # |@block
      # Includes a z-stack of *children* into the enclosing `scenery.viewport`'s *aim rect*
      # (if such a viewport exists; otherwise, `aim` is a noop). Noop if the enclosing
      # viewport has disabled aiming.
      matchpi %{[aim subterms_+]} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Aim.new(children)
      end

      # |@ scenery.page
      #
      # |@pattern
      # [page children_+]
      #
      # |@key children scenery
      #
      # |@block
      # Handles nested `scenery.overlay`s by appending them to a z-stack of *children*.
      #
      # See `scenery.overlay` for more info.
      matchpi %{[page subterms_+]} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Page.new(children)
      end

      # |@ scenery.overlay
      #
      # |@pattern
      # [overlay children_+]
      #
      # |@key children scenery
      #
      # |@block
      # Sends a z-stack of children up, to be appended to the enclosing `scenery.page`.
      #
      # `z-stack`s are the node of choice when it comes to deterministic, local,
      # composable, "anonymous" (as opposed to e.g. CSS's z-index property) layering.
      #
      # Sometimes, however, you might want to have a node *here* to "bubble up" and
      # appear on top of everything else; while also wanting as much locality as is
      # allowed in such a circumstance. That's why `page` and `overlay` nodes exist:
      # `page` defines a "destination" for `overlay`s, and `overlay`s themselves are
      # "portals" that transport *children* to those destinations.
      #
      # Pages can be nested. Overlays can also be nested, although this would make
      # less sense, especially because nested overlays will be inserted before
      # the enclosing ones in the destination page, thus possibly being occluded
      # by them. It could be more meaningful to put a page + overlay inside an overlay,
      # which would behave expectedly.
      #
      # Since Scenery iterates over children in 0-∞ depth-first order, general
      # z-stacking is deterministic in that z-order is "left-to-right", unless
      # overridden explicitly using `z-stack` or `page` + `overlay`. This helps
      # resolve collisions between overlays: overlays later in the tree (under
      # the said ordering) win over prior overlays.
      matchpi %{[overlay subterms_+]} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        Overlay.new(children)
      end

      # |@ scenery.suspense
      #
      # |@pattern
      # [suspense placeholder_ children_*]
      #
      # |@key placeholder scenery
      # The node to show while *children* are loading or unavailable. If assets in
      # the placeholder itself are unavailable, expect standard behavior (namely,
      # built-in placeholders). You can put `suspense` nodes in *placeholder* to
      # mitigate this, although if that is needed, consider re-evaluating your design.
      #
      # |@key children scenery
      #
      # |@block
      # Shows a *placeholder* node while a z-stack of *children* is loading or unavailable.
      matchpi %{[suspense subterm_ subterms_*]} do
        placeholder = recognize(cache, node: subterm)
        children = recognize(cache, nodes: subterms.items)
        if placeholder.is_a?(Inert) && children.all?(Inert)
          return Inert.new
        end

        Suspense(RecognizedNode).new(ZStack.anon(children), placeholder)
      end

      # |@ scenery.variant
      #
      # |@pattern
      # (variant children_* ⍊ if: cond_)
      #
      # |@key children scenery
      #
      # |@key cond nitrene.expr
      # A Nitrene expression which should evaluate to a non-`false` for the variant to
      # be shown.
      #
      # The following variables are given to the expression:
      # - `min-w`: a number telling the parent's min-width constraint.
      # - `min-h`: a number telling the parent's min-height constraint.
      # - `max-w`: `∞` if unrestricted, otherwise a number telling the parent's
      #   min-width constraint.
      # - `max-h`: `∞` if unrestricted, otherwise a number telling the parent's
      #   min-height constraint.
      # - `w`: tells the child's content width.
      # - `h`: tells the child's content height.
      #
      # |@block
      # Shows a z-stack of *children* if a Nitrene *cond*ition equipped with layout-
      # related measurements evaluates to a non-`false`.
      #
      # ```
      # (z-stack
      #   (variant if: (in-range? max-w (500 ..< ∞))
      #     (text caption: "lg"))
      #   (variant if: (in-range? max-w (350 ..< 500))
      #     (text caption: "md"))
      #   (variant if: (in-range? max-w (200 ..< 350))
      #     (text caption: "sm"))
      #   (variant if: (in-range? max-w (-∞ ..< 200))
      #     (text caption: "xs")))
      # ```
      #
      # NOTE: This way of doing conditional layout isn't particularly efficient, since all
      # branches are loaded anyway (otherwise, there would be no way for the layout engine
      # to know which branch to pick). Just like with CSS media queries, consider alternative
      # means of responsivity (e.g., `x-wrap`) before choosing to use `variant`.
      matchpi %{(variant subterms_* ⍊ if: cond_)} do
        children = recognize(cache, nodes: subterms.items)
        if children.all?(Inert)
          return Inert.new
        end

        Variant(RecognizedNode).new(cond, children)
      end

      matchpi %{(observer subterms_+ ⍊ id_)} do
        children = recognize(cache, nodes: subterms.items)

        Observer.new(id, children)
      end

      matchpi %{[observable subterms_+]} do
        children = recognize(cache, nodes: subterms.items)

        Observable.new(children)
      end

      matchpi %{[gate subterms_+]} do
        children = recognize(cache, nodes: subterms.items)

        Gate.new(children)
      end

      # |@ scenery.stack
      #
      # |@pattern
      # [z-stack children_+]
      # [(%all _symbol (%not inert)) children_+]
      #
      # |@key children scenery
      #
      # |@block
      # Displays *children* on top of each other. The first (0th) *child* is
      # drawn on the background, the last *child* is drawn on the foreground.
      #
      # See also: `overlay`.
      #
      # Z-stack is the universal "fallback" node. Any node that looks like
      # it groups children is interpreted as a z-stack unless a more specific
      # treatment exists. So for example, `(qux (rect fill: red) (text caption: "A"))`
      # is recognized as a z-stack, and displays as an "A" on top of a red rect.
      matchpi %{[name←(%all _symbol (%not inert)) subterms_+]} do
        children = recognize(cache, nodes: subterms.items)
        return Inert.new if children.all?(Inert)

        unless name == Term.of(:"z-stack")
          info = ZInfo.new(name, node.pairspart)
        end

        ZStack.new(children, info)
      end

      # |@ scenery.inert
      #
      # |@pattern
      # [inert _*]
      # _
      #
      # |@block
      # Nodes not recognized by Scenery or explicitly labeled as `inert` are
      # *inert* nodes, meaning Scenery ignores them completely: they do not
      # affect sizing, positioning, and do not display as anything; nor are
      # they visited recursively.
      otherwise do
        Inert.new
      end
    end
  end

  private def recognize(cache : CacheSet, *, node : Term) : RecognizedNode
    cache.recognition.put_if_absent(node) { recognize!(cache, node) }
  end

  private def recognize(cache : CacheSet, *, nodes : Indexable(Term)) : Slice(RecognizedNode)
    # The cast is needed because for some reason (probably a bug in Crystal)
    # Crystal (at some point?) thinks recognize() above returns just Inert --
    # but that it's false doesn't stop it from mutilating all downstream types
    # and thus failing to compile. This is probably related to Term.case, which
    # generally isn't on the nice side when it comes to interacting with Crystal.
    nodes.to_readonly_slice { |node| recognize(cache, node: node).as(RecognizedNode) }
  end

  private def selection?(term : Term, caption : Pf::GraphemeSeln) : Selection?
    # |@ scenery.text.selection
    #
    # |@pattern
    # (text ⍊
    #   selection: true
    #   selection-anchor_: (%number i32)
    #   selection-span_: (%number i32)
    #   selection-fill_⋮ lightblue
    #   selection-color_⋮ white
    #   selection-radius⋮ 0
    #   selection-thickness⋮ 1
    #   selection-l⋮ 0
    #   selection-t⋮ 0
    #   selection-r⋮ 1
    #   selection-b⋮ 1
    #   selection-endl⋮ false
    #   selection-endl-w⋮ 8
    #   selection-aim⋮ false
    #   selection-clearance⋮ 8)
    #
    # |@key selection-anchor
    # The modular index of the starting grapheme of the selection.
    #
    # |@key selection-span
    # The number of graphemes before (negative) or after (positive) the anchor
    # to select. If zero, an I-beam is drawn at the anchor instead.
    #
    # |@key selection-fill pigment
    # The fill color for selection rect(s). Selection rects are drawn *below*
    # selected text.
    #
    # |@key selection-color pigment
    # The color of selected text.
    #
    # |@key selection-radius
    # The radius of all four corners for selection rect(s), in pixels.
    #
    # |@key selection-thickness
    # The thickness of the I-beam for zero-span selections, in pixels.
    #
    # |@key selection-l
    # Left coordinate in each selected glyph (0-1).
    #
    # |@key selection-t
    # Top coordinate in each selected glyph (0-1).
    #
    # |@key selection-r
    # Right coordinate in each selected glyph (0-1).
    #
    # |@key selection-b
    # Bottom coordinate in each selected glyph (0-1).
    #
    # |@key selection-endl
    # Whether to extend selection by *selection-endl-w* if an endl (end-of-line;
    # i.e., hard break) is selected.
    #
    # |@key selection-endl-w
    # The width of selection extension on endl (in pixels).
    #
    # |@key selection-aim
    # Whether an enclosing viewport should try to bring this selection into view.
    #
    # |@key selection-clearance
    # Requests some horizontal space on either side of this selection (in pixels)
    # from the enclosing viewport.
    #
    # |@block
    # Defines a master selection in the text. Use `selection: true selections: (...)` to
    # provide more selections. It is not necessary to have the master selection. You can
    # simply add selections via `selection: true selections: (...)`.
    #
    # Like underlines, selections use modulo caption size plus one arithmetic for
    # `selection-anchor` and `selection-span`.
    #
    # Overlapping selections are merged. The first selection in *selections* (and/or the master
    # selection) wins.
    #
    # ```
    # (text
    #   caption: "Input"
    #   selection: true
    #   selection-anchor: -1 ;; at end
    #   selection-span: 0) ;; I-beam
    #
    # ;; Multiple selections
    # (text
    #   caption: "Input"
    #   selection: true
    #   selections:
    #     ({selection-anchor: -1, selection-span: 0}
    #      {selection-anchor: 0, selection-span: 0}))
    # ```
    Term.matchpi?(term, <<-WWML) do
    {¦ selection-anchor_: (%number i32)
       selection-span_: (%number i32)
       selection-fill_⋮ lightblue
       selection-color_⋮ white
       selection-radius⋮ 0
       selection-thickness⋮ 1
       selection-l⋮ 0
       selection-t⋮ 0
       selection-r⋮ 1
       selection-b⋮ 1
       selection-endl⋮ false
       selection-endl-w⋮ 8
       selection-aim⋮ false
       selection-clearance⋮ 8}
    WWML
      anchor = selection_anchor.to(Int32) % (caption.size + 1)
      focus = (anchor + selection_span.to(Int32)) % (caption.size + 1)

      Selection.new(anchor, focus,
        color: Pigment.rgba(selection_color),
        fill: Pigment.rgba(selection_fill),
        radius: selection_radius.to(Magnitude),
        thickness: selection_thickness.to(Magnitude),
        extents: Rect.new(
          tl: Point[selection_l.to(Magnitude), selection_t.to(Magnitude)],
          br: Point[selection_r.to(Magnitude), selection_b.to(Magnitude)],
        ),
        aim: selection_aim.to(Bool),
        clearance: selection_clearance.to(Magnitude),
        endl: selection_endl.to(Bool),
        endl_width: selection_endl_w.to(Magnitude),
      )
    end
  end

  private def selections(pool : Term, caption : Pf::GraphemeSeln) : Slice(Selection)
    buffer = Pf::Kit.stack_array(Selection, 4)

    Term.case(pool) do
      matchpi %{{¦ selection: true selections_dict}} do
        selections.items.each do |item|
          next unless selection = selection?(item, caption)

          buffer << selection
        end

        continue
      end

      matchpi %{{¦ selection: true}} do
        next unless selection = selection?(pool, caption)

        buffer << selection
      end

      otherwise { }
    end

    buffer.to_unsafe_readonly_slice!
  end

  private def underline?(term : Term, caption : Pf::GraphemeSeln) : Underline?
    # |@ scenery.text.underline
    #
    # |@pattern
    # (text ⍊
    #   underline: true
    #   underline-anchor_: (%number i32)
    #   underline-span_: (%number i32)
    #   underline-fill_⋮ auto
    #   underline-offset⋮ 0.3
    #   underline-thickness⋮ 1)
    #
    # |@key underline-anchor
    # The modular index of the starting grapheme of the underline.
    #
    # |@key underline-span
    # The number of graphemes before (negative) or after (positive) the anchor
    # to draw the underline under.
    #
    # |@key underline-fill pigment
    # Sets the color of the underline. `auto` uses text color at that spot
    # instead (so this works with e.g. `selection-color`).
    #
    # |@key underline-offset
    # Currently a fraction of the font's descent. `0` means at the font's baseline,
    # `1` means at descent.
    #
    # |@key underline-thickness
    # The thickness of the underline, in pixels.
    #
    # |@block
    # Defines a master underline in the text. Use `underline: true underlines: (...)` to
    # provide more underlines. It is not necessary to have the master underline. You can
    # simply add underlines via `underline: true underlines: (...)`.
    #
    # Like selections, underlines use modulo caption size plus one arithmetic for
    # `underline-anchor` and `underline-span`.
    #
    # Overlapping underlines are merged. The first underline in *underlines* (and/or the master
    # underline) wins.
    #
    # ```
    # ;; One underline
    # (text caption: "Hello" underline: true underline-anchor: 0 underline-span: 4)
    #
    # ;; Many underlines
    # (text
    #   caption: "Hello"
    #   underline: true
    #   underlines:
    #     ({underline-anchor: 0 underline-span: 2}
    #      {underline-anchor: 3 underline-span: 2}))
    # ```
    Term.matchpi?(term, <<-WWML) do
    {¦ underline-anchor_: (%number i32)
       underline-span_: (%number i32)
       underline-fill_⋮ auto
       underline-offset⋮ 0.3
       underline-thickness⋮ 1}
    WWML
      anchor = underline_anchor.to(Int32) % (caption.size + 1)
      focus = (anchor + underline_span.to(Int32)) % (caption.size + 1)

      Underline.new(anchor, focus,
        fill: underline_fill == Term[:auto] ? nil : Pigment.rgba(underline_fill),
        offset: underline_offset.to(Magnitude),
        thickness: underline_thickness.to(Magnitude),
      )
    end
  end

  private def underlines(pool : Term, caption : Pf::GraphemeSeln) : Slice(Underline)
    buffer = Pf::Kit.stack_array(Underline, 4)

    Term.case(pool) do
      matchpi %{{¦ underline: true underlines_dict}} do
        underlines.items.each do |item|
          next unless underline = underline?(item, caption)

          buffer << underline
        end

        continue
      end

      matchpi %{{¦ underline: true}} do
        next unless underline = underline?(pool, caption)

        buffer << underline
      end

      otherwise { }
    end

    buffer.to_unsafe_readonly_slice!
  end

  # Merges overlapping decorations in *decorations*, a slice of `Decoration`.
  # Leftmost (closer to index 0) decorations win.
  private def merge(decorations : Slice(Selection) | Slice(Underline) | Slice(Decoration)) : Slice
    if decorations.size.in?(0, 1)
      return decorations
    end

    assert decorations.size >= 2

    merged = Pf::Kit.stack_array(typeof(decorations.first), 4)

    queue = decorations.to_a
    queue.sort_by!(&.range.begin)

    while current = queue.shift?
      left_leaning = current.focus == current.range.begin

      while ahead = queue.first?
        range = current.range
        break unless range == ahead.range || range.overlaps?(ahead.range)

        queue.shift

        if left_leaning
          current = current.copy_with(
            anchor: Math.max(current.anchor, ahead.anchor),
            focus: Math.min(current.focus, ahead.focus),
          )
        else
          current = current.copy_with(
            anchor: Math.min(current.anchor, ahead.anchor),
            focus: Math.max(current.focus, ahead.focus),
          )
        end
      end

      merged << current
    end

    merged.to_unsafe_readonly_slice!
  end
end
