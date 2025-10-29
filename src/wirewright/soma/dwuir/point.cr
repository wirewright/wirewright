module Ww::Soma::DwUIR
  # Represents a 2D point with a pair of `Float32`s.
  struct Point
    # Returns the x component of this point.
    getter x : Float32

    # Returns the y component of this point.
    getter y : Float32

    def initialize(@x : Float32, @y : Float32)
    end

    # Constructs a `Point` where both components are set to positive infinity.
    def self.inf : Point
      new(Float32::INFINITY, Float32::INFINITY)
    end

    # Constructs a point at *x*, *y*.
    #
    # NOTE: All of *x*, *y* must respond to `to_f32`.
    def self.[](x, y) : Point
      Point.new(x.to_f32, y.to_f32)
    end

    # Changes the *x* or *y* component of this point -- effectively moving
    # this point to another location.
    def mv(x : Float32 = @x, y : Float32 = @y) : Point
      Point.new(x, y)
    end

    # Returns `true` if this point is above and to the left of *other*.
    def <=(other : Point) : Bool
      x <= other.x && y <= other.y
    end

    # Returns `self`.
    def to_point : Point
      self
    end

    # Checks whether both the x and y components are approximately zero.
    def zero? : Bool
      Approx.equals?(x, 0.0f32) && Approx.equals?(y, 0.0f32)
    end

    # Returns a new point with both x and y components negated.
    def - : Point
      mv(x: -x, y: -y)
    end

    # Adds this point to *other* by summing the corresponding components.
    def +(other : Point) : Point
      mv(x: x + other.x, y: y + other.y)
    end

    # Adds a scalar value to both components of this point.
    def +(other : Number) : Point
      mv(x: x + other, y: y + other)
    end

    # Multiplies this point by *other* by multiplying the corresponding components.
    def *(other : Point) : Point
      mv(x: x * other.x, y: y * other.y)
    end

    # Multiplies both components of this point by a scalar value.
    def *(other : Number)
      mv(x: x * other, y: y * other)
    end

    # Returns the 2D cross product of this point and *other* (as vectors from origin).
    def cross(other : Point) : Float32
      x * other.y - y * other.x
    end

    def dot(other : Point) : Float32
      x * other.x + y * other.y
    end

    def length : Float32
      Math.hypot(x, y)
    end

    def length_squared : Float32
      x**2 + y**2
    end

    # Returns the per-axis scale factors that map this size into the unit square.
    def normalized : Point
      Point.new(1 / x, 1 / y)
    end

    # Compares this point to a reference point *ref*. This is effectively
    # a component-wise division of this point's coordinates by *ref*'s.
    def compare(ref : Point) : Point
      Point.new(x / ref.x, y / ref.y)
    end

    # Subtracts *other* point or scalar from this point.
    def -(other) : Point
      self + -other
    end

    # Constructs a point with the minimum x and y components among
    # `self` and *other*.
    def min(other : Point) : Point
      mv(x: Math.min(x, other.x), y: Math.min(y, other.y))
    end

    # Constructs a point with the maximum x and y components among
    # `self` and *other*.
    def max(other : Point) : Point
      mv(x: Math.max(x, other.x), y: Math.max(y, other.y))
    end

    # Rounds this point's components to the nearest multiple as defined
    # by the provided *grain*.
    def round(*, grain = Point.new(1, 1)) : Point
      mv(x: (x * grain.x).round / grain.x, y: (y * grain.y).round / grain.y)
    end

    # Rounds this point's x and y components using the floor function.
    def floor : Point
      mv(x: x.floor, y: y.floor)
    end

    # Rounds this point's x and y components using the ceiling function.
    def ceil : Point
      mv(x: x.ceil, y: y.ceil)
    end

    # Returns `true` if either component of the point is set to positive infinity.
    # Returns `false` otherwise.
    def inf? : Bool
      x == Float32::INFINITY || y == Float32::INFINITY
    end

    # Shorthand for a tuple of `{x, y}`.
    def xy : {Float32, Float32}
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
  end
end
