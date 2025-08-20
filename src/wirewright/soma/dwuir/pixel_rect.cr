module Ww::Soma::DwUIR
  # A pixel buffer of the given *width* and *height*.
  #
  # `PixelRect` translates global coordinates (called *absolute coordinates*)
  # into local (*relative*) indices into a buffer it owns when accessing
  # or modifying pixels.
  class PixelRect
    # Returns the width of this pixel rect.
    getter width : Int32

    # Returns the height of this pixel rect.
    getter height : Int32

    # Constructs a pixel rect of the given *width* and *height*.
    #
    # *x* and *y* define the absolute coordinates for this pixel rect's "virtual"
    # top-left corner. They will be used to translate absolute coordinates
    # given to e.g. `pixel` or `blend` into local ones, pointing into
    # the underlying *width* x *height* pixel buffer.
    def initialize(@x : Int32, @y : Int32, @width : Int32, @height : Int32)
      @pixels = Slice(UInt32).new(@width * @height)
    end

    # Returns the *absolute* bounding box of this pixel rect. Point coordinates
    # are guaranteed to have no fractional part.
    def bounds : Rect
      Rect[@x, @y, @width, @height]
    end

    # Clamps *rect* into the bounds of this pixel rect.
    def clamp(rect : Rect) : Rect
      bounds & rect
    end

    # Returns a smaller piece of this pixel rect whose *absolute* bounding box is
    # defined by *rect* (and clamped to this pixel rect's bounding box).
    def region(rect : Rect) : Region
      Region.new(self, *(bounds & rect).ixywh)
    end

    private def pixel_and_offset(x : Int32, y : Int32) : {Pixel, Int32}
      unless @x <= x < @x + @width && @y <= y < @y + @height
        raise IndexError.new
      end

      offset = @width * (y - @y) + (x - @x)

      {Pixel[@pixels.unsafe_fetch(offset)], offset}
    end

    # Returns the `Pixel` at the given *absolute* coordinates.
    #
    # Raises `IndexError` if coordinates are out of bounds.
    def pixel(x : Int32, y : Int32) : Pixel
      pixel, _ = pixel_and_offset(x, y)
      pixel
    end

    # Blends *pixel* over an existing pixel at the given *absolute* coordinates.
    #
    # Raises `IndexError` if coordinates are out of bounds.
    def blend(x : Int32, y : Int32, pixel src : Pixel)
      dst, offset = pixel_and_offset(x, y)
      res = src.blend(over: dst)

      @pixels.unsafe_put(offset, res.argb)
    end

    # Blends `self` over *dst* with the given *opacity*, thereby modifying *dst*.
    #
    # NOTE: *dst* must enclose `self` in terms of its absolute bounding box;
    # otherwise, this method will raise `IndexError`.
    def blend_over(dst : PixelRect, opacity : Float32) : Nil
      alpha = (opacity * 255).clamp(0.0..255.0).to_u32

      display = region(dst.bounds)
      display.each_pixel_with_coords do |srcpx, x, y|
        dst.blend(x, y, srcpx.alpha(alpha))
      end
    end

    # Clears this pixel rect with *color*.
    def fill(color : Color) : Nil
      fill(Pixel.of(color).argb)
    end

    protected def fill(argb : UInt32) : Nil
      @pixels.fill(argb)
    end

    protected def fill_row(argb : UInt32, x : Int32, y : Int32, w : Int32) : Nil
      @pixels.fill(argb, @width * y + x, w)
    end

    def inspect(io)
      io << "PixelRect([" << @width << "x" << @height

      if @x > 0 || @y > 0
        io << ", x=" << @x
        io << ", y=" << @y
      end

      io << "])"
    end
  end

  # Defines sub-areas of a `PixelRect`, still using absolute coordinates, and
  # supports limited drawing and iteration functionality over those.
  class PixelRect::Region
    # :nodoc:
    def initialize(@parent : PixelRect, @x : Int32, @y : Int32, @width : Int32, @height : Int32)
    end

    # Yields each pixel in this region along with its *absolute* X and Y
    # coordinates to the block.
    def each_pixel_with_coords(& : Pixel, Int32, Int32 ->)
      (@y...@y + @height).each do |y|
        (@x...@x + @width).each do |x|
          yield @parent.pixel(x, y), x, y
        end
      end
    end

    # Clears this region with *color*.
    def fill(color : Color) : Nil
      argb = Pixel.of(color).argb

      (@y...@y + @height).each do |y|
        @parent.fill_row(argb, @x, y, @width)
      end
    end
  end
end
