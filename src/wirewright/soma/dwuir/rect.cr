module Ww::Soma::DwUIR
  # Represents a 2D rectangle with a pair of `Point`s.
  struct Rect
    # Returns the top-left point of this rectangle.
    getter tl : Point

    # Returns the bottom-right point of this rectangle.
    getter br : Point

    def initialize(@tl, @br)
    end

    # Constructs a rectangle positioned at (0;0), with an infinite size.
    def self.inf : Rect
      new(tl: Point.new(0, 0), size: Point.inf)
    end

    # Constructs a rectangle with the given top-left point *tl* and *size*.
    def self.new(*, tl : Point, size : Point) : Rect
      new(tl, tl + size)
    end

    # Constructs an empty rectangle at (0;0).
    def self.empty : Rect
      new(Point.new(0, 0), Point.new(0, 0))
    end

    # Constructs a rectangle with its top-left corner at *x*, *y*; width *w*, and
    # height *h*.
    #
    # NOTE: All of *x*, *y*, *w*, *h* must respond to `to_f32`.
    def self.[](x, y, w, h) : Rect
      new(tl: Point[x, y], size: Point[w, h])
    end

    # Returns the union of *quads*' bounding boxes.
    def self.union(quads : Enumerable(Quad)) : Rect
      bounds = Rect.empty
      quads.each do |quad|
        bounds |= quad.bounds
      end
      bounds
    end

    # Returns the intersection of *quads*' bounding boxes.
    def self.xsect(quads : Enumerable(Quad)) : Rect
      bounds = Rect.inf
      quads.each do |quad|
        bounds &= quad.bounds
      end
      bounds
    end

    # Returns the size (width and height) of this rectangle.
    def size : Point
      br - tl
    end

    # Returns the bottom-left point of this rectangle.
    def bl : Point
      br.mv(x: tl.x)
    end

    # Returns the top-right point of this rectangle.
    def tr : Point
      br.mv(y: tl.y)
    end

    # Returns the center point of this rectangle.
    def mid : Point
      tl + size*0.5
    end

    # Returns this rectangle as a quad.
    def quad : Quad
      Quad.new(tl, tr, br, bl)
    end

    # Returns `true` if this rectangle has infinite size.
    def inf? : Bool
      br.inf?
    end

    # Returns `true` if this rectangle's bottom right corner is above or
    # to the left of its top-left corner,
    def negative? : Bool
      tl.x > br.x || tl.y > br.y
    end

    # Returns `true` if this rectangle has zero width or height. Returns
    # `false` otherwise.
    def empty? : Bool
      w <= 0 || h <= 0
    end

    # Returns `true` if this rectangle includes *object*.
    #
    # NOTE: *object* must respond to `to_point` (e.g. `Point#to_point`).
    def includes?(object) : Bool
      tl <= object.to_point <= br
    end

    # Returns `true` if this rectangle includes the x coordinate *x*.
    def includes_x?(x : Float32) : Bool
      tl.x <= x <= br.x
    end

    # Returns `true` if this rectangle includes the x coordinate of
    # the given *object*.
    #
    # NOTE: *object* must respond to `to_point` (e.g. `Point#to_point`).
    def includes_x?(object) : Bool
      includes_x?(object.to_point.x)
    end

    # Returns `true` if this rectangle includes the y coordinate *y*.
    def includes_y?(y : Float32) : Bool
      tl.y <= y <= br.y
    end

    # Returns `true` if this rectangle includes the y coordinate of
    # the given *object*.
    #
    # NOTE: *object* must respond to `to_point` (e.g. `Point#to_point`).
    def includes_y?(object) : Bool
      includes_y?(object.to_point.y)
    end

    # Shorthand for flattened `xy`, `wh`.
    def xywh : {Float32, Float32, Float32, Float32}
      {*xy, *wh}
    end

    # Shorthand for flattened `ixy`, `iwh`.
    def ixywh : {Int32, Int32, Int32, Int32}
      {*ixy, *iwh}
    end

    # Shorthand for a tuple `{x, y}`.
    def xy : {Float32, Float32}
      {x, y}
    end

    # Shorthand for a tuple `{ix, iy}`.
    def ixy : {Int32, Int32}
      {ix, iy}
    end

    # Shorthand for a tuple `{w, h}`.
    def wh : {Float32, Float32}
      {w, h}
    end

    # Shorthand for a tuple `{iw, ih}`.
    def iwh : {Int32, Int32}
      {iw, ih}
    end

    # Returns the x coordinate of this rectangle's top-left point.
    def x : Float32
      tl.x
    end

    # Returns the x coordinate of this rectangle's top-left point as an integer.
    # Removes the fractional part using the ceiling function.
    def ix : Int32
      tl.ix
    end

    # Returns the y coordinate of this rectangle's top-left point.
    def y : Float32
      tl.y
    end

    # Returns the y coordinate of this rectangle's top-left point as an integer.
    # Removes the fractional part using the ceiling function.
    def iy : Int32
      tl.iy
    end

    # Returns the width of this rectangle.
    def w : Float32
      size.x
    end

    # Returns the width of this rectangle as an integer. Removes the fractional
    # part using the ceiling function.
    def iw : Int32
      size.ix
    end

    # Returns the height of this rectangle.
    def h : Float32
      size.y
    end

    # Returns the height of this rectangle as an integer. Removes the fractional
    # part using the ceiling function.
    def ih : Int32
      size.iy
    end

    # Returns the size of this rectangle's diagonal.
    def diagonal : Float32
      Math.hypot(w, h)
    end

    # Returns the area of this rectangle.
    def area : Float32
      w * h
    end

    # Calculates the A point for this rectangle and *radii*.
    #
    # A-H points are needed for rounded rectangles, hence the prefix *rr*.
    # *radii* are used to compute the locations of these points. The following
    # diagram illustrates the rough location of these points.
    #
    # ```text
    #         A                                 H
    #
    #         │                                 │
    #     ┌───┼─────────────────────────────────┼────┐
    #     │   │                                 │    │
    #     │                                          │
    # B ──┼──                                      ──┼── G
    #     │                                          │
    #     │                                          │
    #     │               rounded rect               │
    #     │                                          │
    #     │    note: the points will shift slightly  │
    #     │              based on radii              │
    #     │                                          │
    #     │                                          │
    # C ──┼──                                      ──┼── F
    #     │                                          │
    #     │   │                                 │    │
    #     └───┼─────────────────────────────────┼────┘
    #         │                                 │
    #
    #         D                                 E
    # ```
    def rra(radii : RectRadii) : Point
      tl + Point.new(radii.tl, 0)
    end

    # Calculates the B point for this rectangle and *radii*. See `rra`.
    def rrb(radii : RectRadii) : Point
      tl + Point.new(0, radii.tl)
    end

    # Calculates the C point for this rectangle and *radii*. See `rra`.
    def rrc(radii : RectRadii) : Point
      bl - Point.new(0, radii.bl)
    end

    # Calculates the D point for this rectangle and *radii*. See `rra`.
    def rrd(radii : RectRadii) : Point
      bl + Point.new(radii.bl, 0)
    end

    # Calculates the E point for this rectangle and *radii*. See `rra`.
    def rre(radii : RectRadii) : Point
      br - Point.new(radii.br, 0)
    end

    # Calculates the F point for this rectangle and *radii*. See `rra`.
    def rrf(radii : RectRadii) : Point
      br - Point.new(0, radii.br)
    end

    # Calculates the G point for this rectangle and *radii*. See `rra`.
    def rrg(radii : RectRadii) : Point
      tr + Point.new(0, radii.tr)
    end

    # Calculates the H point for this rectangle and *radii*. See `rra`.
    def rrh(radii : RectRadii) : Point
      tr - Point.new(radii.tr, 0)
    end

    # Returns the intersection rect of this and *other* rectangles.
    def &(other : Rect) : Rect
      xrect = Rect.new(tl: tl.max(other.tl), br: br.min(other.br))
      xrect.negative? ? Rect.empty : xrect
    end

    # Returns the union rect of this and *other* rectangles.
    def |(other : Rect) : Rect
      Rect.new(tl.min(other.tl), br.max(other.br))
    end

    # Returns a copy of this rectangle whose location and size are rounded to
    # the nearest multiple as defined by the provided *grain*.
    def round(*, grain = Point.new(1, 1)) : Rect
      # NOTE: we cannot round br here because that could cause tl = br
      # due to rounding. Instead, round size, which guarantees we'll have at least
      # some sort of size.
      Rect.new(tl: tl.round(grain: grain), size: size.round(grain: grain))
    end

    # Returns a copy of this rectangle after removing fractional parts
    # using the ceiling function.
    def ceil : Rect
      Rect.new(tl: tl.ceil, size: size.ceil)
    end

    # Returns a copy of this rectangle after removing fractional parts
    # using the floor function.
    def floor : Rect
      Rect.new(tl: tl.floor, size: size.floor)
    end

    # Returns a rectangle aligned to the integer grid that fully encloses
    # this one.
    def snap : Rect
      Rect.new(tl: tl.floor, br: br.ceil)
    end

    # Returns a copy of this rectangle padded by *n*.
    def pad(n : Float32) : Rect
      Rect.new(tl: tl + n, br: br - n)
    end

    # Returns a copy of this rectangle with size changed to *w* and *h*.
    def resize(w = size.x, h = size.y) : Rect
      Rect.new(tl: tl, size: Point.new(w, h))
    end

    # Returns a copy of this rectangle with size increased by *delta*.
    def grow(delta : Point) : Rect
      Rect.new(tl, br + delta)
    end

    # Returns a copy of this rectangle with width and height increased by
    # *dw* and *dh*, respectively.
    def grow(*, dw : Float32 = 0, dh : Float32 = 0) : Rect
      grow(Point[dw, dh])
    end

    # Returns a copy of this rectangle translated (moved) by *delta*.
    def translate(delta : Point) : Rect
      Rect.new(tl + delta, br + delta)
    end

    # Returns a copy of this rectangle moved by *dx* on the X axis and by *dy*
    # on the Y axis.
    def translate(*, dx : Float32 = 0, dy : Float32 = 0) : Rect
      translate(Point[dx, dy])
    end

    # Converts *point* (whose components normally are, but not restricted to,
    # the unit range 0-1), into coordinates within this rectangle.
    #
    # For example, an `Point(0.5, 0.5)` point would be the same as the middle
    # point (`mid`) of this rectangle.
    def map(point : Point) : Point
      Point.new(x + point.x * w, y + point.y * h)
    end

    # Changes the X components of the top-left and bottom-right corners using
    # the block. Returns the resulting rectangle.
    def mapx(& : Float32 -> Float32) : Rect
      Rect.new(Point.new((yield tl.x), tl.y), Point.new((yield br.x), br.y))
    end

    # Changes the Y components of the top-left and bottom-right corners using
    # the block. Returns the resulting rectangle.
    def mapy(& : Float32 -> Float32) : Rect
      Rect.new(Point.new(tl.x, (yield tl.y)), Point.new(br.x, (yield br.y)))
    end

    # Maps a point from rectangle space into [0,1]x[0,1].
    def normalize(point : Point) : Point
      (point - tl) * size.normalized
    end

    # Maps a point from [0,1]x[0,1] back into rectangle space.
    def denormalize(point : Point) : Point
      tl + point * size
    end

    # Returns a translated copy of `self` so that normalized *point*s of
    # both rectangles match.
    def align(other : Rect, point : Point) : Rect
      translate(denormalize(point) - other.denormalize(point))
    end

    # Returns the four points that this rectangle is defined by.
    def points : {Point, Point, Point, Point}
      {tl, tr, bl, br}
    end

    # Returns the four segments that this rectangle is defined by.
    def segments : {Segment, Segment, Segment, Segment}
      {Segment.new(tl, tr),
       Segment.new(tr, br),
       Segment.new(br, bl),
       Segment.new(bl, tl)}
    end

    # Returns `true` if this rectangle intersects *quad*. Returns `false` otherwise.
    def intersects?(quad : Quad) : Bool
      return false if empty?
      return true if points.any? { |point| quad.includes?(point) }
      return true if quad.points.any? { |point| includes?(point) }

      quad.segments.each do |s1|
        segments.each do |s2|
          return true if s1.intersects?(s2)
        end
      end

      false
    end

    # Returns `true` if this rectangle intersects *all of* *quads*. Returns
    # `false` otherwise.
    def intersects?(quads : Slice(Quad)) : Bool
      quads.all? { |quad| intersects?(quad) }
    end
  end
end
