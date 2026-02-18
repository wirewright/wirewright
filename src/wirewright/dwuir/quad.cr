module Ww::DwUIR
  # Represents a quadrilateral.
  #
  # Written almost entirely by an LLM, with major refactors on my end (but
  # with absolutely no understanding of the underlying math!)
  struct Quad
    @rect : Bool

    # NOTE: points must be given in clockwise order.
    def initialize(@a : Point, @b : Point, @c : Point, @d : Point)
      ab = b - a
      bc = c - b
      cd = d - c
      da = a - d

      right_angles =
        Approx.equals?(ab.dot(da), 0.0f32) &&
          Approx.equals?(bc.dot(ab), 0.0f32) &&
          Approx.equals?(cd.dot(bc), 0.0f32) &&
          Approx.equals?(da.dot(cd), 0.0f32)

      equal_sides =
        ab.length_squared == cd.length_squared &&
          bc.length_squared == da.length_squared

      @rect = right_angles && equal_sides
    end

    def points : {Point, Point, Point, Point}
      {@a, @b, @c, @d}
    end

    # Returns the segments that this quad is composed of.
    def segments : {Segment, Segment, Segment, Segment}
      {Segment.new(@a, @b),
       Segment.new(@b, @c),
       Segment.new(@c, @d),
       Segment.new(@d, @a)}
    end

    # Returns `true` if *point* is located on the boundary of this quad.
    def on_boundary?(point : Point) : Bool
      segments.any?(&.includes?(point))
    end

    # Returns `true` if *point* is located on the boundary or inside of
    # this quad.
    def includes?(point : Point) : Bool
      if @rect # Fast path
        return Rect.new(tl: @a, br: @c).includes?(point)
      end

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
