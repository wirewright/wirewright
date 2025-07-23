module Ww::Soma::DwUIR
  # Represents a segment defined by two `Point`s.
  #
  # Zero-length segments are allowed and treated as points.
  struct Segment
    def initialize(@a : Point, @b : Point)
    end

    # Returns the bounding box of this segment.
    def bounds : Rect
      Rect.new(tl: @a.min(@b), br: @a.max(@b))
    end

    # Returns `true` if *point* is located on this segment. Returns `false` otherwise.
    def includes?(point : Point) : Bool
      bounds.includes?(point) && collinear?(point)
    end

    private def collinear?(point : Point) : Bool
      Segment.new(@a, point).orientation(@b).collinear?
    end

    # See also: https://www.geeksforgeeks.org/dsa/orientation-3-ordered-points/
    enum Orientation
      Collinear
      Clockwise
      CounterClockwise
    end

    # Returns the orientation of this segment relative to *c*.
    def orientation(c : Point) : Orientation
      val = (@b - @a).x(c - @a)

      if val.abs < 1e-10 # ≈ 0
        return Orientation::Collinear
      end

      val.positive? ? Orientation::CounterClockwise : Orientation::Clockwise
    end

    # Returns `true` if this segment intersects *other*. Returns `false` otherwise.
    def intersects?(other : Segment) : Bool
      o1 = orientation(other.@a)
      o2 = orientation(other.@b)
      o3 = other.orientation(@a)
      o4 = other.orientation(@b)

      if o1 != o2 && o3 != o4
        return true
      end

      includes?(other.@a) || includes?(other.@b) || other.includes?(@a) || other.includes?(@b)
    end

    # Returns `true` if this segment intersects with the horizontal ray
    # originating from *point*. Returns `false` otherwise.
    def intersects_horizontal_ray?(point : Point) : Bool
      straddles_y?(point) && intersection_x(point) > point.x
    end

    private def straddles_y?(point : Point) : Bool
      (@a.y > point.y) != (@b.y > point.y)
    end

    private def intersection_x(point : Point) : Float32
      @a.x + (@b.x - @a.x) * (point.y - @a.y) / (@b.y - @a.y)
    end
  end
end
