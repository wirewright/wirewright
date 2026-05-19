module Ww::Scenery
  # Represents a 2D point with a pair of `Magnitude`s.
  struct Point
    # Returns the x component of this point.
    getter x : Magnitude

    # Returns the y component of this point.
    getter y : Magnitude

    def initialize(@x : Magnitude, @y : Magnitude)
    end

    # Constructs a `Point` where both components are set to positive infinity.
    def self.inf : Point
      new(Magnitude::INFINITY, Magnitude::INFINITY)
    end

    # Constructs a point at *x*, *y*.
    #
    # NOTE: All of *x*, *y* must respond to `to_f32`.
    def self.[](x, y) : Point
      new(x.to_f32, y.to_f32)
    end

    # Returns the component-wise min for *a* and *b*.
    def self.min(a : Point, b : Point) : Point
      Point[Math.min(a.x, b.x), Math.min(a.y, b.y)]
    end

    # Returns the component-wise min for *a* and *b*, and *points*.
    def self.min(a, b, *points) : Point
      min(min(a, b), *points)
    end

    # Returns component-wise max for *a* and *b*.
    def self.max(a : Point, b : Point) : Point
      Point[Math.max(a.x, b.x), Math.max(a.y, b.y)]
    end

    # Returns the component-wise max for *a*, *b, and *points*.
    def self.max(a, b, *points) : Point
      max(max(a, b), *points)
    end

    # Returns `true` if this point is above and to the left of *other*.
    def <=(other : Point) : Bool
      x <= other.x && y <= other.y
    end

    # Checks whether both the x and y components are approximately zero.
    def zero? : Bool
      approx?(Point[0, 0])
    end

    # Checks for approximate equality with *point*. See also: `Float#approx?`.
    def approx?(other : Point) : Bool
      x.approx?(other.x) && y.approx?(other.y)
    end

    # Returns a new point with both x and y components negated.
    def - : Point
      Point[-x, -y]
    end

    # Adds this point to *other* by summing the corresponding components.
    def +(other : Point) : Point
      Point[x + other.x, y + other.y]
    end

    # Adds a scalar value to both components of this point.
    def +(other : Number) : Point
      Point[x + other, y + other]
    end

    # Subtracts *other* point or scalar from this point.
    def -(other) : Point
      self + -other
    end

    # Multiplies this point by *other* by multiplying the corresponding components.
    def *(other : Point) : Point
      Point[x * other.x, y: y * other.y]
    end

    # Multiplies both components of this point by a scalar value.
    def *(other : Number)
      Point[x * other, y * other]
    end

    # Calculates the cross product of this point and *other*.
    def cross(other : Point) : Magnitude
      x * other.y - y * other.x
    end

    # Calculates the dot product of this point and *other*.
    def dot(other : Point) : Magnitude
      x * other.x + y * other.y
    end

    # Returns the length of the line from origin to this point.
    def length : Magnitude
      Math.hypot(x, y)
    end

    # Returns the square of the length of the line from origin to this point.
    def length_squared : Magnitude
      x**2 + y**2
    end

    # Returns the per-axis scale factors that map this size into the unit square.
    def normalized : Point
      Point[1 / x, 1 / y]
    end

    # Rounds this point's components to the nearest multiple as defined
    # by the provided *grain*.
    def round(*, grain = Point.new(1, 1)) : Point
      Point[(x * grain.x).round / grain.x, (y * grain.y).round / grain.y]
    end

    # Rounds this point's x and y components using the floor function.
    def floor : Point
      Point[x.floor, y.floor]
    end

    # Rounds this point's x and y components using the ceiling function.
    def ceil : Point
      Point[x.ceil, y.ceil]
    end

    # Removes the sign from both axes.
    def abs : Point
      Point[x.abs, y.abs]
    end

    # Returns `true` if either component of the point is set to positive infinity.
    def inf? : Bool
      x == Magnitude::INFINITY || y == Magnitude::INFINITY
    end

    # Shorthand for a tuple of `{x, y}`.
    def xy : {Magnitude, Magnitude}
      {x, y}
    end

    # Shorthand for a tuple of `{ix, iy}`.
    def ixy : {Int32, Int32}
      {ix, iy}
    end

    # Returns the x component as an int; uses `ceil` to remove the fractional part.
    def ix : Int32
      x.ceil.to_i
    end

    # Returns the y component as an int; uses `ceil` to remove the fractional part.
    def iy : Int32
      y.ceil.to_i
    end

    def inspect(io)
      io << x << "●" << y
    end
  end
end
