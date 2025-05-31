module Ww::Soma
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
    record Image, src : Term, opacity : Float32, fit : ImageFit

    # Represents the ways an image can occupy the painted area.
    enum ImageFit : UInt8
      Origin
      Stretch
      Tile
      Center

      # Parses *term* as an `ImageFit` value; returns *fallback* if unable to.
      def self.term(term, fallback : ImageFit) : ImageFit
        Term.case(term) do
          # |@ soma.dwuir.paint.image-fit.origin
          #
          # |@block
          # The image will be put at the top-left corner of the painted area, and
          # clipped on overflow.
          # |@endblock
          matchpi %{origin} { Origin }

          # |@ soma.dwuir.paint.image-fit.stretch
          #
          # |@block
          # The image will be scaled up or down to fit in the painted area.
          # |@endblock
          matchpi %{stretch} { Stretch }

          # |@ soma.dwuir.paint.image-fit.tile
          #
          # |@block
          # The image will be tiled across the painted area.
          # |@endblock
          matchpi %{tile} { Tile }

          # |@ soma.dwuir.paint.image-fit.center
          #
          # |@block
          # The image's center will be aligned with the filled drawable's bounding
          # box center. The image will be cropped to fit in bounds, if necessary.
          # |@endblock
          matchpi %{center} { Center }

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
        # |@key src -- The specification for how to load an image. Different painters
        # will support different specifications; consult e.g. `soma.painter.plutovg`.
        #
        # |@key opacity -- Specifies the opacity of the image, a number between `0`
        # (meaning fully transparent) and `1` (meaning opaque).
        #
        # |@key fit soma.dwuir.image-fit -- Specifies how the image should occupy
        # the painted area.
        matchpi(<<-WWML
        (image src_ ¦ _
          opacity: (%optional 1 opacity←(%number 0 <= _ <= 1))
          fit⋮ origin)
        WWML
        ) do
          Image.new(src, opacity.to(Float32), ImageFit.term(fit, fallback: :origin))
        end

        # |@ soma.dwuir.paint.linear-gradient
        #
        # |@block
        # `linear-gradient` defines a linear gradient paint that transitions colors along
        # a straight line from a starting point to an ending point.
        # |@endblock
        #
        # |@key stops -- Specifies a sequence of color stops, each with a position
        # (a number between 0 and 1) and a color value. At least one stop is required.
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
          begin-l_number
          begin-t_number
          end-l_number
          end-t_number)
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
        # |@key stops -- Specifies a sequence of color stops, each with a position
        # (a number between 0 and 1) and a color value. At least one stop is required.
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
          center-l_number
          center-t_number
          center-radius_number
          focus-l_number
          focus-t_number
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
        # |@ref soma.color
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

    # Returns `true` if *paint* is opaque. Returns `false` otherwise.
    def opaque?(paint : Any) : Bool
      Approx.equals?(opacity(paint), 1.0f32)
    end
  end
end
