module Ww::Scenery
  # A rectangle of `Pixel`s.
  class PixelRect
    # Returns the width of this pixel rect.
    getter width : Int32

    # Returns the height of this pixel rect.
    getter height : Int32

    # Returns the clear color, which we call *backdrop*.
    getter backdrop : Pigment::RGBA

    def initialize(@pixels : Slice(Pixel), @width, @height, @backdrop, @clear : Bool)
    end

    def to_unsafe : UInt8*
      @pixels.unsafe_slice_of(UInt8).to_unsafe
    end

    # NOTE: Make sure to have backdrop's alpha at 255, otherwise it'll
    # mess up anti-aliasing.
    #
    # NOTE: You must `clear` manually (and redraw) if you want the change to apply.
    def backdrop=(backdrop : Pigment::RGBA) : Pigment::RGBA
      unless @backdrop == backdrop
        @clear = false
        @backdrop = backdrop
      end

      backdrop
    end

    # :nodoc:
    def stride
      @width * 4
    end

    # Marks this pixel rect as dirty.
    def dirty : Nil
      @clear = false
    end

    # Clears this pixel rect with the backdrop color if it is dirty.
    def clear
      return if @clear

      @pixels.fill(Pixel.of(@backdrop))
      @clear = true
    end

    # Clears the given *region* with the backdrop color if it is dirty.
    def clear(region : Rect) : Nil
      return if @clear

      # Snap to pixel coordinates.
      region = region.snap

      # Bound top-left point safely.
      tl_x = region.tl.x.clamp(Magnitude.new(0)..Magnitude.new(@width)).to_i
      tl_y = region.tl.y.clamp(Magnitude.new(0)..Magnitude.new(@height)).to_i

      # Bound bottom-right point safely.
      br_x = region.br.x.clamp(Magnitude.new(0)..Magnitude.new(@width)).to_i
      br_y = region.br.y.clamp(Magnitude.new(0)..Magnitude.new(@height)).to_i

      assert tl_x <= br_x
      assert tl_y <= br_y

      pixel = Pixel.of(@backdrop)

      (tl_y...br_y).each do |y|
        @pixels.fill(pixel, y * @width + tl_x, br_x - tl_x)
      end
    end

    # Returns a blob representing the content of this pixel rect as a PPM image.
    def to_ppm : Term::Blob
      Term::Blob.build do |io|
        io << "P3\n"
        io << @width << " " << @height << "\n"
        io << "255\n"

        @pixels.each do |pixel|
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
