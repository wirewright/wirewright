module Ww::Soma
  # Represents a segment defined by two `Point`s.
  struct Segment
    def initialize(@a : Point, @b : Point)
    end

    # Returns the bounding box of this segment.
    def bounds : Rect
      Rect.new(tl: @a, br: @b)
    end

    # Returns `true` if *point* is located on this segment. Returns `false` otherwise.
    def includes?(point : Point) : Bool
      bounds.includes?(point) && collinear?(point)
    end

    private def collinear?(point : Point) : Bool
      (point - @a).x(@b - @a).abs < 1e-10
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
