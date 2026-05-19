module Ww::Scenery
  # Represents a 2D rectangle using a pair of `Point`s.
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

    # Returns the union of two rectangles.
    def self.union(a : Rect, b : Rect) : Rect
      new(tl: Point.min(a.tl, b.tl), br: Point.max(a.br, b.br))
    end

    # Returns the intersection of two rectangles.
    def self.intersection(a : Rect, b : Rect) : Rect
      new(tl: Point.max(a.tl, b.tl), br: Point.min(a.br, b.br))
    end

    # Returns a translated copy of *a* so that normalized *point*s of
    # both rectangles match.
    def self.align(a : Rect, b : Rect, point : Point) : Rect
      a.translate(a.denormalize(point) - b.denormalize(point))
    end

    # Maps a *unit* rect into *rect*.
    def self.map(rect : Rect, unit : Rect) : Rect
      new(tl: rect.map(unit.tl), br: rect.map(unit.br))
    end

    # Returns the size (width and height) of this rectangle.
    def size : Point
      br - tl
    end

    # Returns the bottom-left point of this rectangle.
    def bl : Point
      Point[tl.x, br.y]
    end

    # Returns the top-right point of this rectangle.
    def tr : Point
      Point[br.x, tl.y]
    end

    # Returns the center point of this rectangle.
    def mid : Point
      tl + size*0.5
    end

    # Returns `true` if this rectangle has infinite size.
    def inf? : Bool
      size.inf?
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
    def includes?(point : Point) : Bool
      tl <= point <= br
    end

    # Shorthand for flattened `xy`, `wh`.
    def xywh : {Magnitude, Magnitude, Magnitude, Magnitude}
      {*xy, *wh}
    end

    # Shorthand for flattened `ixy`, `iwh`.
    def ixywh : {Int32, Int32, Int32, Int32}
      {*ixy, *iwh}
    end

    # Shorthand for a tuple `{x, y}`.
    def xy : {Magnitude, Magnitude}
      {x, y}
    end

    # Shorthand for a tuple `{ix, iy}`.
    def ixy : {Int32, Int32}
      {ix, iy}
    end

    # Shorthand for a tuple `{w, h}`.
    def wh : {Magnitude, Magnitude}
      {w, h}
    end

    # Shorthand for a tuple `{iw, ih}`.
    def iwh : {Int32, Int32}
      {iw, ih}
    end

    # Returns the x coordinate of this rectangle's top-left point.
    def x : Magnitude
      tl.x
    end

    # Returns the x coordinate of this rectangle's top-left point as an integer.
    # Removes the fractional part using the ceiling function.
    def ix : Int32
      tl.ix
    end

    # Returns the y coordinate of this rectangle's top-left point.
    def y : Magnitude
      tl.y
    end

    # Returns the y coordinate of this rectangle's top-left point as an integer.
    # Removes the fractional part using the ceiling function.
    def iy : Int32
      tl.iy
    end

    # Returns the width of this rectangle.
    def w : Magnitude
      size.x
    end

    # Returns the width of this rectangle as an integer. Removes the fractional
    # part using the ceiling function.
    def iw : Int32
      size.ix
    end

    # Returns the height of this rectangle.
    def h : Magnitude
      size.y
    end

    # Returns the height of this rectangle as an integer. Removes the fractional
    # part using the ceiling function.
    def ih : Int32
      size.iy
    end

    # Returns the size of this rectangle's diagonal.
    def diagonal : Magnitude
      size.length
    end

    # Returns the area of this rectangle.
    def area : Magnitude
      w * h
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
      Rect.new(tl: tl.floor, size: size.ceil)
    end

    # Returns a copy of this rectangle padded by *n*.
    def pad(n : Magnitude) : Rect
      Rect.new(tl: tl + n, br: br - n)
    end

    # Returns a copy of this rectangle padded on each *side*.
    def pad(sides : {l: Magnitude, r: Magnitude, t: Magnitude, b: Magnitude})
      Rect.new(
        tl: tl + Point[sides[:l], sides[:t]],
        size: size - Point[sides[:l] + sides[:r], sides[:t] + sides[:b]],
      )
    end

    # Returns a copy of this rectangle with a margin of *n* pixels on all sides.
    def margin(n : Magnitude) : Rect
      Rect.new(tl: tl - Point[n, n], size: size + Point[2*n, 2*n])
    end

    # Returns a copy of this rectangle with size increased by *delta*.
    def grow(delta : Point) : Rect
      Rect.new(tl, br + delta)
    end

    # Returns a copy of this rectangle with width and height increased by
    # *dw* and *dh*, respectively.
    def grow(*, dw : Magnitude = 0, dh : Magnitude = 0) : Rect
      grow(Point[dw, dh])
    end

    # Returns a copy of this rectangle translated (moved) by *delta*.
    def translate(delta : Point) : Rect
      Rect.new(tl + delta, br + delta)
    end

    # Returns a copy of this rectangle moved by *dx* on the X axis and by *dy*
    # on the Y axis.
    def translate(*, dx : Magnitude = 0, dy : Magnitude = 0) : Rect
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

    def abs : Rect
      Rect.new(tl: tl.min(br), size: size.abs)
    end

    # Maps a point from rectangle space into [0,1]x[0,1].
    def normalize(point : Point) : Point
      (point - tl) * size.normalized
    end

    # Maps a point from [0,1]x[0,1] back into rectangle space.
    def denormalize(point : Point) : Point
      tl + point * size
    end

    # Returns `true` if the intersection of this rect and other is nonnegative.
    def intersects?(other : Rect) : Bool
      !Rect.intersection(self, other).negative?
    end

    # Returns `true` if this rect's top-left and bottom-right coordinates are
    # so close to each other that the rect can be considered a single point.
    def point? : Bool
      tl.approx?(br)
    end

    def inspect(io)
      io << "■(tl: " << tl << ", br: " << br << ")"
    end
  end
end
