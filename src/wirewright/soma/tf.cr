module Ww::Soma
  # Represents a transformation matrix.
  #
  # Reference: https://github.com/sammycage/plutovg/blob/c6a1c3b7989cde72f21e09a74cfa6078528ff978/source/plutovg-matrix.c
  struct Tf
    # :nodoc:
    IDENTITY = new(1, 0, 0, 1, 0, 0)

    # :nodoc:
    def initialize(
      @a : Float32,
      @b : Float32,
      @c : Float32,
      @d : Float32,
      @e : Float32,
      @f : Float32,
    )
    end

    # Creates an empty transformation matrix.
    def self.new : Tf
      IDENTITY
    end

    # Constructs a transformation matrix by combining the given *actions*.
    def self.[](*actions : Tf) : Tf
      actions.reduce(new) { |tf, action| tf.append(action) }
    end

    # Constructs a translation matrix.
    def self.translate(point : Point) : Tf
      new(1, 0, 0, 1, point.x, point.y)
    end

    # Constructs a scaling matrix.
    def self.scale(point : Point) : Tf
      new(point.x, 0, 0, point.y, 0, 0)
    end

    # :ditto:
    def self.scale(amount : Float32) : Tf
      scale(Point.new(amount, amount))
    end

    # Constructs a rotation matrix.
    def self.rotate(degrees : Float32) : Tf
      radians = Math.deg2rad(degrees)

      c = Math.cos(radians)
      s = Math.sin(radians)

      new(c, s, -s, c, 0, 0)
    end

    # Constructs a shearing matrix.
    def self.shear(point : Point) : Tf
      new(1, Math.tan(point.y), Math.tan(point.x), 1, 0, 0)
    end

    # Composes this and *other* transformations. Returns the resulting transformation.
    def append(other : Tf) : Tf
      Tf.new(
        a: other.@a * @a + other.@b * @c,
        b: other.@a * @b + other.@b * @d,
        c: other.@c * @a + other.@d * @c,
        d: other.@c * @b + other.@d * @d,
        e: other.@e * @a + other.@f * @c + @e,
        f: other.@e * @b + other.@f * @d + @f,
      )
    end

    # Applies this transformation to *point*.
    def map(point : Point) : Point
      return point if point.inf?

      Point.new(
        x: point.x * @a + point.y * @c + @e,
        y: point.x * @b + point.y * @d + @f,
      )
    end

    # Applies this transformation to *rect* and returns the bounding box
    # of the resulting quad.
    def map(rect : Rect) : Rect
      return rect if rect.inf?

      quad(rect).bounds
    end

    # Applies this transformation to *rect* and returns the resulting quad.
    def quad(rect : Rect) : Quad
      Quad.new(map(rect.tl), map(rect.tr), map(rect.br), map(rect.bl))
    end
  end
end
