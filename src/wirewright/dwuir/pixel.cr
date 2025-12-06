module Ww::DwUIR
  # A DwUIR *pixel* is a 32-bit unsigned integer representing a premultiplied
  # ARGB color.
  record Pixel, argb : UInt32 do
    # Alias of `new`.
    @[AlwaysInline]
    def self.[](argb : UInt32) : Pixel
      new(argb)
    end

    # Constructs a pixel from the given *color*.
    #
    # Reference: https://github.com/sammycage/plutovg/blob/c6a1c3b7989cde72f21e09a74cfa6078528ff978/source/plutovg-paint.c#L79
    # Reference: https://github.com/sammycage/plutovg/blob/c6a1c3b7989cde72f21e09a74cfa6078528ff978/source/plutovg-utils.h#L57
    def self.of(color : Pigment::RGBA) : Pixel
      r32, g32, b32, a32 = color.rgba8.map(&.to_u32)

      unless a32 == 255
        r32 = (r32 * a32) // 255
        g32 = (g32 * a32) // 255
        b32 = (b32 * a32) // 255
      end

      new((a32 << 24) | (r32 << 16) | (g32 << 8) | b32)
    end

    # Returns the alpha component.
    def a : UInt32
      argb >> 24
    end

    # Returns the premultiplied red component.
    def r : UInt32
      (argb >> 16) & 0xff
    end

    # Returns the premultiplied green component.
    def g : UInt32
      (argb >> 8) & 0xff
    end

    # Returns the premultiplied blue component.
    def b : UInt32
      argb & 0xff
    end

    # Converts this pixel's premultiplied ARGB value to RGBA.
    #
    # Reference: https://github.com/sammycage/plutovg/blob/c6a1c3b7989cde72f21e09a74cfa6078528ff978/source/plutovg-surface.c#L246
    def rgba : {UInt32, UInt32, UInt32, UInt32}
      a = self.a

      if a == 0u32
        return 0u32, 0u32, 0u32, 0u32
      end

      r, g, b = self.r, self.g, self.b

      unless a == 255u32
        r = (r &* 255u32) // a
        g = (g &* 255u32) // a
        b = (b &* 255u32) // a
      end

      {r, g, b, a}
    end

    def rgba8 : {UInt8, UInt8, UInt8, UInt8}
      rgba.map(&.to_u8)
    end

    # Formats the output of `rgba` as 32-bit integer in little endian order.
    def rgba_le : UInt32
      r, g, b, a = rgba

      (a << 24) | (b << 16) | (g << 8) | r
    end

    # Formats the output of `rgba` as 32-bit integer in little endian order.
    def argb_be : UInt32
      r, g, b, a = rgba

      (a << 24) | (r << 16) | (g << 8) | b
    end

    # Blends *src* pixel over *dst* pixel.
    def blend(over other : Pixel)
      Pixel[Pixel.pargb32_blend(argb, over: other.argb)]
    end

    # Multiplies this pixel by a single byte *alpha*.
    def alpha(alpha : UInt32) : Pixel
      Pixel[Pixel.pargb32_mul_byte(argb, alpha)]
    end

    # :nodoc:
    #
    # Reference: https://github.com/sammycage/plutovg/blob/c6a1c3b7989cde72f21e09a74cfa6078528ff978/source/plutovg-blend.c#L70
    def self.pargb32_mul_byte(x : UInt32, a : UInt32) : UInt32
      t = (x & 0xff00ffu32) &* a
      t = (t &+ ((t >> 8) & 0xff00ffu32) &+ 0x800080u32) >> 8
      t &= 0xff00ffu32
      x = ((x >> 8) & 0xff00ffu32) &* a
      x = (x &+ ((x >> 8) & 0xff00ffu32) &+ 0x800080u32)
      x &= 0xff00ff00u32
      x |= t
      x
    end

    # :nodoc:
    #
    # Reference: https://github.com/sammycage/plutovg/blob/c6a1c3b7989cde72f21e09a74cfa6078528ff978/source/plutovg-blend.c#L318
    def self.pargb32_blend(src : UInt32, *, over dst : UInt32) : UInt32
      alpha = src >> 24

      src &+ pargb32_mul_byte(dst, 255u32 - alpha)
    end
  end
end
