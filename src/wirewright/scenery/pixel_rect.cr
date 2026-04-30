module Ww::Scenery
  # A rectangle of `Pixel`s.
  class PixelRect
    # Returns the width of this pixel rect.
    getter width : Int32

    # Returns the height of this pixel rect.
    getter height : Int32

    # Returns the stride, aka pitch, of the texture (bytes).
    getter stride : Int32

    # :nodoc:
    def initialize(@pixels : UInt8*, @width, @height, @stride)
    end

    # The maximum PixelRect width (see `screen`).
    MAX_WIDTH = 16_000

    # The maximum PixelRect height (see `screen`).
    MAX_HEIGHT = 16_000

    # Turns an arbitrary *width* and *height* into *screen width* and *screen height*,
    # which can be safely used with `PixelRect`.
    def self.clamp(width : Magnitude, height : Magnitude) : {Int32, Int32}
      iwidth = width.to_i.clamp(0..MAX_WIDTH)
      iheight = height.to_i.clamp(0..MAX_HEIGHT)

      {iwidth, iheight}
    end

    def to_unsafe : UInt8*
      @pixels
    end

    # Yields each pixel in this rect, top-to-bottom, left-to-right.
    def each(& : Pixel ->) : Nil
      @height.times do |y|
        @width.times do |x|
          yield (@pixels + y*@stride + x*4).as(Pixel*).value
        end
      end
    end

    # Returns a blob representing the content of this pixel rect as a PPM image.
    def to_ppm : Term::Blob
      Term::Blob.build do |io|
        io << "P3\n"
        io << @width << " " << @height << "\n"
        io << "255\n"

        each do |pixel|
          r, g, b, _ = pixel.rgba
          io << r << " " << g << " " << b << "\n"
        end
      end
    end

    def inspect(io)
      io << "PixelRect(" << "pixels=<" << width << "x" << height << " bitmap>)"
    end
  end
end
