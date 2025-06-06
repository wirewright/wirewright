module Ww::Soma::DwUIR
  # |@ soma.dwuir.paint
  #
  # |@block
  # Paints are used to define with what DwUIR nodes should be filled or stroked.
  # Several types of paints are available, from solid paints to gradient paints
  # to image paints.
  # |@endblock

  # Represets a *paint*. There are different kinds of paints: solid color paints,
  # gradient and image paints, etc.
  #
  # See also: `soma.dwuir.paint`.
  module Paint
    extend self

    alias Any = Invalid | Solid | LinearGradient | RadialGradient | Image

    # Represents a list of gradient stops.
    record GradientStopList, offsets : Slice(Float32), colors : Slice(Color) do
      include Indexable({Float32, Color})

      def size : Int32
        @offsets.size
      end

      def unsafe_fetch(index : Int) : {Float32, Color}
        {@offsets[index], @colors[index]}
      end
    end

    # Represents an invalid paint. Different painters may display this differently.
    # The recommended way, though, is to perhaps show some kind of error icon
    # or text saying "invalid paint".
    record Invalid

    # Represents a solid color paint.
    record Solid, color : Color

    # Represents a linear gradient paint.
    record LinearGradient,
      begin : Point,
      end : Point,
      stops : GradientStopList

    # Represents a radial gradient paint.
    record RadialGradient,
      center : Point,
      center_radius : Float32,
      focus : Point,
      focus_radius : Float32,
      stops : GradientStopList

    # Represents an image paint.
    #
    # - *src* describes how to obtain the image. This field is not interpreted
    #   by `Paint`; instead, it is left for the interpretation by the painter,
    #   since only the painter knows the kinds of images it can render, how, and
    #   and what it needs to know to render them.
    record Image,
      src : Term,
      fit : ImageFit::Any,
      tile : Bool,
      opacity : Float32,
      resize_w : Magn,
      resize_h : Magn

    # Represents the ways an image can occupy the painted area.
    module ImageFit
      extend self

      alias Any = Pan | Align | Stretch

      record Pan, delta : Point
      record Align, normpt : Point
      record Stretch

      Default = Stretch.new

      # Parses *term* as an `ImageFit` value; returns *fallback* if unable to.
      def term(term, fallback = Default) : Any
        Term.case(term) do
          # |@ soma.dwuir.paint.image-fit.align
          #
          # |@block
          # |@endblock
          matchpi %{(align ¦ _ l⋮ 0 t⋮ 0)} do
            Align.new(Point.new(l.to(Float32), t.to(Float32)))
          end

          matchpi %{(pan ¦ _ l⋮ 0 t⋮ 0)} do
            Pan.new(Point.new(l.to(Float32), t.to(Float32)))
          end

          # |@ soma.dwuir.paint.image-fit.stretch
          #
          # |@block
          # The image will be scaled up or down to fit in the painted area.
          # |@endblock
          matchpi %{stretch} { Stretch.new }

          otherwise { fallback }
        end
      end
    end

    # Parses *term* as one of `Paint::Any`.
    #
    # Using the doctool, visit `soma.dwuir.paint` to see the available paints.
    def term(term : Term) : Any
      Term.case(term) do
        # |@ soma.dwuir.paint.image
        #
        # |@block
        # `image` defines an image paint. The image loading process is abstracted
        # away; and only *src*, in effect a specification for how to load an image,
        # is accepted.
        # |@endblock
        #
        # |@key src soma.dwuir.resources.image -- The specification for how to load
        # an image. Different painters will support different specifications. See
        # `soma.dwuir.painters.plutovg.image` to learn about the supported formats.
        #
        # |@key opacity -- Specifies the opacity of the image, a number between `0`
        # (meaning fully transparent) and `1` (meaning fully opaque).
        #
        # |@key fit soma.dwuir.image-fit -- Specifies how the image should occupy
        # the painted area.
        #
        # |@key tile -- The image will be tiled across the painted area if `true`.
        #
        # |@key resize-w soma.dwuir.magn -- Changes the initial width of the image.
        #
        # |@key resize-h soma.dwuir.magn -- Changes the initial height of the image.
        matchpi(<<-WWML
          (image src_ ¦ _
            opacity: (%optional 1 opacity←(%number 0 <= _ <= 1))
            fit_⋮ stretch
            tile⋮ false
            resize-w_⋮ (* 1)
            resize-h_⋮ (* 1))
        WWML
        ) do
          Image.new(src,
            fit: ImageFit.term(fit),
            tile: tile.true?,
            opacity: opacity.to(Float32),
            resize_w: Magn.abst(resize_w, Magn.rel(1)),
            resize_h: Magn.abst(resize_h, Magn.rel(1)),
          )
        end

        # |@ soma.dwuir.paint.linear-gradient
        #
        # |@block
        # `linear-gradient` defines a linear gradient paint that transitions colors along
        # a straight line from a starting point to an ending point.
        # |@endblock
        #
        # |@key stops soma.dwuir.color -- Specifies a sequence of color stops, each with
        # a position (a number between 0 and 1) and a color value. At least one
        # stop is required.
        #
        # |@key begin-l -- Specifies the x-coordinate of the gradient's starting point;
        # numbers between `0` and `1` are expected although not enforced, relative to
        # the width of the painted area.
        #
        # |@key begin-t -- Specifies the y-coordinate of the gradient's starting point;
        # numbers between `0` and `1` are expected although not enforced, relative to
        # the height of the painted area.
        #
        # |@key end-l -- Specifies the x-coordinate of the gradient's ending point;
        # numbers between `0` and `1` are expected although not enforced, relative to
        # the width of the painted area.
        #
        # |@key end-t -- Specifies the y-coordinate of the gradient's ending point;
        # numbers between `0` and `1` are expected although not enforced, relative to
        # the height of the painted area.
        matchpi(<<-WWML
          (linear-gradient (%past (stop (%number 0 <= _ <= 1) _) min: 1) ¦ _
            begin-l⋮ 0.5
            begin-t⋮ 0
            end-l⋮ 0.5
            end-t⋮ 1)
        WWML
        ) do
          stops = term.items.move(1)

          colors = stops.to_readonly_slice { |(_, _, color)| Color.term(color) }
          offsets = stops.to_readonly_slice { |(_, offset, _)| offset.to(Float32) }

          b = Point.new(begin_l.to(Float32), begin_t.to(Float32))
          e = Point.new(end_l.to(Float32), end_t.to(Float32))

          LinearGradient.new(b, e, GradientStopList.new(offsets, colors))
        end

        # |@ soma.dwuir.paint.radial-gradient
        #
        # |@block
        # `radial-gradient` defines a radial gradient paint that transitions colors
        # outward in a circular or elliptical pattern from a central starting point
        # to an outer boundary.
        # |@endblock
        #
        # |@key stops soma.dwuir.color -- Specifies a sequence of color stops, each with
        # a position (a number between 0 and 1) and a color value. At least one stop
        # is required.
        #
        # |@key center-l -- Specifies the x-coordinate of the gradient's center point;
        # numbers between `0` and `1` are expected although not enforced, relative to
        # the width of the painted area.
        #
        # |@key center-t -- Specifies the y-coordinate of the gradient's center point;
        # numbers between `0` and `1` are expected although not enforced, relative to
        # the height of the painted area.
        #
        # |@key center-radius -- Specifies the radius of the gradient.
        #
        # |@key focus-l -- Specifies the x-coordinate of the gradient's focal point,
        # influencing the direction of the gradient's color transition within the radius.
        # Numbers between `0` and `1` are expected although not enforced, relative to
        # the width of the painted area.
        #
        # |@key focus-t -- Specifies the y-coordinate of the gradient's focal point,
        # influencing the direction of the gradient's color transition within the radius.
        # Numbers between `0` and `1` are expected although not enforced, relative to
        # the width of the painted area.
        #
        # |@key focus-radius -- Specifies the radius of the gradient's focus.
        matchpi(<<-WWML
          (radial-gradient (%past (stop (%number 0 <= _ <= 1) _) min: 1) ¦ _
            center-l⋮ 0.5
            center-t⋮ 0.5
            center-radius_number
            focus-l⋮ 0.5
            focus-t⋮ 0.5
            focus-radius_number)
        WWML
        ) do
          stops = term.items.move(1)

          colors = stops.to_readonly_slice { |(_, _, color)| Color.term(color) }
          offsets = stops.to_readonly_slice { |(_, offset, _)| offset.to(Float32) }

          center = Point.new(center_l.to(Float32), center_t.to(Float32))
          focus = Point.new(focus_l.to(Float32), focus_t.to(Float32))

          RadialGradient.new(
            center: center,
            center_radius: center_radius.to(Float32),
            focus: focus,
            focus_radius: focus_radius.to(Float32),
            stops: GradientStopList.new(offsets, colors),
          )
        end

        # |@ soma.dwuir.paint.solid
        #
        # |@block
        # See `soma.dwuir.color`.
        # |@endblock
        otherwise do
          Solid.new(Color.term(term))
        end
      end
    end

    # Returns a transparent paint.
    def transparent : Solid
      Solid.new(Color.rgba(0, 0, 0, 0))
    end

    # :nodoc:
    def opacity(paint : Invalid) : Float32
      1.0f32
    end

    # :nodoc:
    def opacity(paint : Solid) : Float32
      paint.color.ua
    end

    # :nodoc:
    def opacity(stops : GradientStopList) : Float32
      stops.colors.max_of(&.ua)
    end

    # :nodoc:
    def opacity(paint : LinearGradient | RadialGradient) : Float32
      opacity(paint.stops)
    end

    # :nodoc:
    def opacity(paint : Image) : Float32
      paint.opacity
    end

    {% if flag?(:docs) %}
      # Returns the opacity of *paint*
      def opacity(paint : Any) : Float32
      end
    {% end %}

    # Returns `true` if *paint* is fully transparent. Returns `false` otherwise.
    def transparent?(paint : Any) : Bool
      Approx.equals?(opacity(paint), 0.0f32)
    end

    # Returns `true` if *paint* is fully opaque. Returns `false` otherwise.
    def opaque?(paint : Any) : Bool
      Approx.equals?(opacity(paint), 1.0f32)
    end
  end
end
