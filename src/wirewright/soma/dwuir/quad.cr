module Ww::Soma::DwUIR
  # Represents a quadrilateral.
  struct Quad
    # NOTE: points must be given in clockwise order.
    def initialize(@a : Point, @b : Point, @c : Point, @d : Point)
    end

    # Returns the segments that this quad is composed of.
    def segments : {Segment, Segment, Segment, Segment}
      {Segment.new(@a, @b),
       Segment.new(@b, @c),
       Segment.new(@c, @d),
       Segment.new(@d, @a)}
    end

    # Returns `true` if *point* is located on the boundary of this quad.
    # Returns `false` otherwise.
    def on_boundary?(point : Point) : Bool
      segments.any?(&.includes?(point))
    end

    # Returns `true` if *point* is located on the boundary or inside of
    # this quad. Returns `false` otherwise.
    def includes?(point : Point) : Bool
      on_boundary?(point) || segments.count(&.intersects_horizontal_ray?(point)).odd?
    end

    # Returns the bounding box of this quad.
    def bounds : Rect
      tlx = tly = Float32::MAX
      brx = bry = Float32::MIN

      {@a, @b, @c, @d}.each do |point|
        tlx = {tlx, point.x}.min
        tly = {tly, point.y}.min
        brx = {brx, point.x}.max
        bry = {bry, point.y}.max
      end

      Rect.new(tl: Point.new(tlx, tly), br: Point.new(brx, bry))
    end

    # Translates all of this quad's points by *delta*.
    def translate(delta : Point) : Quad
      Quad.new(@a + delta, @b + delta, @c + delta, @d + delta)
    end
  end
end
