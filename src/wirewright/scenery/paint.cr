module Ww::Scenery
  # Data types and functions for working with a *paint*, which is most often
  # a `Solid` color, less often a `Gradient` of some kind.
  #
  # As philosophical as it can get, a paint is something you can "paint with";
  # currently the set of nodes that can be painted (as opposed to simply
  # colored) is small, and includes mainly `scenery.rect`, which allows both
  # it's stroke and fill to be a paint.
  module Paint
    extend self

    alias Any = Solid | Gradient

    alias Gradient = LinearGradient | RadialGradient

    # Represents a solid color paint. See `pigment` in the doctool.
    defrecord Solid, color : Pigment::RGBA

    # Represents a linear gradient. See `scenery.gradient`.
    defcase LinearGradient,
      begin : Point,
      end : Point,
      stops : Slice(GradientStop)

    # Represents a radial gradient. See `scenery.gradient`.
    defcase RadialGradient,
      center : Point,
      center_radius : Magnitude,
      focus : Point,
      focus_radius : Magnitude,
      stops : Slice(GradientStop)

    # Represents a gradient stop.
    defrecord GradientStop,
      offset : Magnitude,
      color : Pigment::RGBA

    # Recognizes the paint expressed in *term*. Returns *fallback* in case
    # no paint is recognized.
    def recognize(term : Term, fallback = Solid.new(Pigment.transparent)) : Any
      Term.case(term) do
        # |@ scenery.paint.gradient
        #
        # |@pattern
        # (linear-gradient (%past (stop _number _) min: 1) ⍊
        #   begin-l⋮ 0.5
        #   begin-t⋮ 0
        #   end-l⋮ 0.5
        #   end-t⋮ 1)
        #
        # |@key begin-l
        # The X-coordinate (_l_eft) of the begin point.
        #
        # |@key begin-t
        # The Y-coordinate (_t_op) of the begin point.
        #
        # |@key end-l
        # The X-coordinate (_l_eft) of the end point.
        #
        # |@key end-t
        # The Y-coordinate (_t_op) of the end point.
        #
        # |@block
        # Expresses a linear gradient paint that transitions colors along a straight
        # line from the begin point to the end point.
        #
        # Each gradient stop is expressed as `(stop offset_ color_)`, where *offset*
        # (most often in the unit range 0-1) sets the offset along the line, and *color*
        # is the `pigment` to use there.
        matchpi(<<-WWML) do
        (linear-gradient (%past (stop _number _) min: 1) ⍊
          begin-l⋮ 0.5
          begin-t⋮ 0
          end-l⋮ 0.5
          end-t⋮ 1)
        WWML
          stop_specs = term.items.move(1)

          stops = stop_specs.to_readonly_slice do |(_, offset, color)|
            GradientStop.new(offset.to(Magnitude), Pigment.rgba(color))
          end

          b = Point[begin_l.to(Magnitude), begin_t.to(Magnitude)]
          e = Point[end_l.to(Magnitude), end_t.to(Magnitude)]

          LinearGradient.new(b, e, stops)
        end

        # |@ scenery.paint.gradient
        #
        # |@pattern
        # (radial-gradient (%past (stop _number _) min: 1) ⍊
        #   ±center-radius
        #   focus-radius⋮ 0
        #   center-l⋮ 0.5
        #   center-t⋮ 0.5
        #   focus-l⋮ 0.5
        #   focus-t⋮ 0.5)
        #
        # |@key center-radius
        # The radius of the gradient.
        #
        # |@key focus-radius
        # The radius of the gradient's focus.
        #
        # |@key center-l
        # The X-coordinate (_l_eft) of the center point.
        #
        # |@key center-t
        # The Y-coordinate (_t_op) of the center point.
        #
        # |@key focus-l
        # The X-coordinate (_l_eft) of the focus point.
        #
        # |@key focus-t
        # The Y-coordinate (_t_op) of the focus point.
        #
        # |@block
        # Expresses a radial gradient paint that transitions colors outward in
        # a circular or elliptical pattern from a central point.
        #
        # Each gradient stop is expressed as `(stop offset_ color_)`, where *offset*
        # (most often in the unit range 0-1) sets the offset along the path, and *color*
        # is the `pigment` to use there.
        matchpi(<<-WWML) do
        (radial-gradient (%past (stop _number _) min: 1) ⍊
          ±center-radius
          focus-radius⋮ 0
          center-l⋮ 0.5
          center-t⋮ 0.5
          focus-l⋮ 0.5
          focus-t⋮ 0.5)
        WWML
          stop_specs = term.items.move(1)

          stops = stop_specs.to_readonly_slice do |(_, offset, color)|
            GradientStop.new(offset.to(Magnitude), Pigment.rgba(color))
          end

          center = Point[center_l.to(Magnitude), center_t.to(Magnitude)]
          focus = Point[focus_l.to(Magnitude), focus_t.to(Magnitude)]

          RadialGradient.new(center, center_radius.to(Magnitude), focus, focus_radius.to(Magnitude), stops)
        end

        # |@ scenery.paint.solid
        #
        # |@alias pigment
        otherwise do
          if rgba = Pigment.rgba?(term)
            return Solid.new(rgba)
          end

          fallback
        end
      end
    end

    # Returns the alpha of the given solid color.
    def alpha(paint : Solid) : Magnitude
      paint.color.a
    end

    # Returns the maximum alpha of all gradient stops.
    def alpha(paint : Gradient) : Magnitude
      alpha(paint.stops)
    end

    private def alpha(paint : Slice(GradientStop)) : Magnitude
      paint.max_of(&.color.a)
    end

    # Returns `true` if *paint* is transparent.
    def transparent?(paint : Any) : Bool
      alpha(paint).approx?(0)
    end
  end
end
