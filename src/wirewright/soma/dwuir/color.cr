module Ww::Soma::DwUIR
  # |@ soma.dwuir.color
  #
  # |@block
  # Wirewright implements a variety of ways to specify the color of stuff.
  # |@endblock

  # Represents a color using RGBA.
  struct Color
    # Returns the red component of this color.
    getter r : UInt8

    # Returns the green component of this color.
    getter g : UInt8

    # Returns the blue component of this color.
    getter b : UInt8

    # Returns the alpha component of this color.
    getter a : UInt8

    def initialize(@r, @g, @b, @a)
    end

    protected def_change

    # Constructs a `Color` from OkLCH lightness *l*, chroma *c*, hue *h*.
    #
    # See `Oklch.to_rgb` to learn what their value ranges are.
    def self.oklch(l : Float64, c : Float64, h : Float64) : Color
      rgba(*Oklch.to_rgb(l, c, h))
    end

    # Constructs s `Color` from HSL hue *h*, saturation *s*, lightness *l*.
    #
    # See `HSL.to_rgb` to learn what their value ranges are.
    def self.hsl(h : Float64, s : Float64, l : Float64) : Color
      rgba(*HSL.to_rgb(h, s, l))
    end

    # Constructs a `Color` from the given RGBA values (0-255).
    def self.rgba(r : UInt8, g : UInt8, b : UInt8, a : UInt8 = 255) : Color
      new(r, g, b, a)
    end

    # Constructs a `Color` by interpreting the four bytes of *value* as
    # R, G, B, and A correspondingly (in the order of most significant to
    # least significant).
    def self.rgba(value : UInt32) : Color
      r = (value >> 24) & 0xff
      g = (value >> 16) & 0xff
      b = (value >> 8) & 0xff
      a = (value >> 0) & 0xff

      new(r.to_u8, g.to_u8, b.to_u8, a.to_u8)
    end

    private def self.hexcolor?(r : Rtk::R)
      Rtk.skip(r, " ")
      return unless Rtk.ahead?(r, "#")

      Rtk.forward(r)

      # Try to read all the way up to RRGGBBAA.
      d0 = Rtk.hexdigit?(r)
      d1 = d0 && Rtk.hexdigit?(r)
      d2 = d1 && Rtk.hexdigit?(r)
      d3 = d2 && Rtk.hexdigit?(r)
      d4 = d3 && Rtk.hexdigit?(r)
      d5 = d4 && Rtk.hexdigit?(r)
      d6 = d5 && Rtk.hexdigit?(r)
      d7 = d6 && Rtk.hexdigit?(r)

      Rtk.skip(r, " ")
      return unless Rtk.at_end?(r)

      if d0 && d1 && d2 && d4.nil?
        r = (d0 << 4 | d0).to_u8
        g = (d1 << 4 | d1).to_u8
        b = (d2 << 4 | d2).to_u8
        a = d3 ? (d3 << 4 | d3).to_u8 : 255u8
        return r, g, b, a
      end

      if d0 && d1 && d2 && d3 && d4 && d5
        r = (d0 << 4 | d1).to_u8
        g = (d2 << 4 | d3).to_u8
        b = (d4 << 4 | d5).to_u8
        if d6.nil?
          return r, g, b, 255u8
        end
        if d7
          return r, g, b, (d6 << 4 | d7).to_u8
        end
      end
    end

    private def self.hexcolor?(string : String) : {UInt8, UInt8, UInt8, UInt8}?
      reader = Char::Reader.new(string)

      hexcolor?(pointerof(reader))
    end

    # Constructs a color by parsing the given *string*. Hex colors and named
    # colors (see `NAMED`) are supported.
    def self.named?(value : String) : Color?
      if color = NAMED[value]?
        return color
      end

      return unless rgba = hexcolor?(value)

      rgba(*rgba)
    end

    # Same as `named?`, but raises `ArgumentError` instead of returning `nil`.
    def self.named(value : String) : Color
      named?(value) || raise ArgumentError.new
    end

    private def self.opacity_to_alpha(opacity : Float64) : UInt8
      (opacity.clamp(0.0..1.0) * 255).to_u8
    end

    private def self.opacity_to_alpha(opacity : Term) : UInt8
      opacity_to_alpha(opacity.to(Float64))
    end

    # |@table soma.dwuir.color.oklch.hue
    OKLCH_HUES = {
      "red"     => 27.0,
      "orange"  => 50.0,
      "amber"   => 70.0,
      "yellow"  => 90.0,
      "lime"    => 120.0,
      "green"   => 145.0,
      "teal"    => 175.0,
      "cyan"    => 195.0,
      "sky"     => 220.0,
      "blue"    => 250.0,
      "indigo"  => 275.0,
      "violet"  => 295.0,
      "purple"  => 320.0,
      "magenta" => 340.0,
      "pink"    => 355.0,
      "crimson" => 10.0,
    }

    # Constructs a `Color` by parsing the given term.
    #
    # Using doctool, search for `soma.dwuir.color` to learn about the possible
    # values for *term*.
    #
    # *fallback* is returned when *term* cannot be parsed.
    def self.term(term : Term, fallback = rgba(0, 0, 0))
      Term.case(term) do
        # |@ soma.dwuir.color.oklch
        #
        # |@block
        # `oklch` defines a color using the OKLCH color space, which represents
        # colors with perceptual lightness, chroma, and hue.
        # |@endblock
        #
        # |@key l -- Specifies the lightness of the color, a number between 0 and 1.
        # 1 means light (e.g. white), 0 means dark (e.g. black).
        #
        # |@key c -- Specifies the chroma of the color, a number between 0 and 0.36.
        # Chroma is basically the "amount" of color -- how "colorful" a color is.
        #
        # |@key h -- Specifies the hue of the color, a number between 0 and 360 (degrees).
        matchpi %{(oklch l_number c_number h_number)} do
          oklch(l.to(Float64), c.to(Float64), h.to(Float64))
        end

        # |@ soma.dwuir.color.oklch
        #
        # |@key h soma.dwuir.color.oklch.hue -- Specifies the name of the hue of the color.
        matchpi(
          %{(oklch l_number c_number h_symbol)},
          %{(oklch l_number c_number h_string)},
        ) do
          oklch(l.to(Float64), c.to(Float64), OKLCH_HUES[h.to(String)]? || continue)
        end

        # |@ soma.dwuir.color.oklch
        #
        # |@key opacity -- Specifies the opacity of the color (a number between 0
        # and 1, where 0 means fully transparent and 1 means fully opaque).
        matchpi %{(oklch l_number c_number h_number opacity_number)} do
          color = oklch(l.to(Float64), c.to(Float64), h.to(Float64))
          color.change(a: opacity_to_alpha(opacity))
        end

        # |@ soma.dwuir.color.oklch
        #
        # |@key h soma.dwuir.color.oklch.hue -- Specifies the name of the hue of the color.
        #
        # |@key opacity -- Specifies the opacity of the color (a number between 0
        # and 1, where 0 means fully transparent and 1 means fully opaque).
        matchpi(
          %{(oklch l_number c_number h_symbol opacity_number)},
          %{(oklch l_number c_number h_string opacity_number)},
        ) do
          color = oklch(l.to(Float64), c.to(Float64), OKLCH_HUES[h.to(String)]? || continue)
          color.change(a: opacity_to_alpha(opacity))
        end

        # |@ soma.dwuir.color.rgb
        #
        # |@block
        # `rgb` defines a color using the RGB color space, specifying red, green,
        # and blue components.
        # |@endblock
        #
        # |@key r -- Specifies the red component of the color, an 8-bit unsigned
        # integer (0 to 255).
        #
        # |@key g -- Specifies the green component of the color, an 8-bit unsigned
        # integer (0 to 255).
        #
        # |@key b -- Specifies the blue component of the color, an 8-bit unsigned
        # integer (0 to 255).
        matchpi(
          %{(r←(%number u8) g←(%number u8) b←(%number u8))},
          %{(rgb r←(%number u8) g←(%number u8) b←(%number u8))},
        ) do
          rgba(r.to(UInt8), g.to(UInt8), b.to(UInt8))
        end

        # |@ soma.dwuir.color.rgb
        #
        # |@key opacity -- Specifies the opacity of the color (a number between 0
        # and 1, where 0 means fully transparent and 1 means fully opaque).
        matchpi %{(rgb r←(%number u8) g←(%number u8) b←(%number u8) opacity_number)} do
          color = rgba(r.to(UInt8), g.to(UInt8), b.to(UInt8))
          color.change(a: opacity_to_alpha(opacity))
        end

        # |@ soma.dwuir.color.rgb
        #
        # |@key a -- Specifies the alpha (opacity) component of the color, an 8-bit
        # unsigned integer (0 to 255).
        matchpi(
          %{(r←(%number u8) g←(%number u8) b←(%number u8) a←(%number u8))},
          %{(rgba r←(%number u8) g←(%number u8) b←(%number u8) a←(%number u8))},
        ) do
          rgba(r.to(UInt8), g.to(UInt8), b.to(UInt8), a.to(UInt8))
        end

        # |@ soma.dwuir.color.hsl
        #
        # |@block
        # `hsl` defines a color using the HSL color space, specifying hue,
        # saturation, and lightness.
        # |@endblock
        #
        # |@key h -- Specifies the hue of the color, a number between 0 and 360 (degrees).
        #
        # |@key s -- Specifies the saturation of the color, a number between 0 and 1.
        #
        # |@key l -- Specifies the lightness of the color, a number between 0 and 1.
        matchpi %{(hsl h_number s_number l_number)} do
          hsl(h.to(Float64), s.to(Float64), l.to(Float64))
        end

        # |@ soma.dwuir.color.hsl
        #
        # |@key a -- Specifies the opacity of the color (a number between 0 and 1,
        # where 0 means fully transparent and 1 means fully opaque).
        matchpi %{(hsl h_number s_number l_number opacity_number)} do
          color = hsl(h.to(Float64), s.to(Float64), l.to(Float64))
          color.change(a: opacity_to_alpha(opacity))
        end

        # |@ soma.dwuir.color.string
        #
        # |@block
        # String terms are treated in the following way (and order).
        #
        # 1. If the string appears to be a CSS named color (https://drafts.csswg.org/css-color/#named-color),
        #    it is resolved as such.
        # 2. If the string is a valid hex color of the form `#RGB[A]`, `#RRGGBB[AA]`,
        #    it is resolved as such.
        # 3. Otherwise, the string is ignored.
        #
        # Leading and trailing ASCII whitespace characters are omitted. Any other
        # characters will lead to the string being ignored.
        # |@endblock
        matchpi %{_symbol}, %{_string} do
          named?(term.to(String)) || continue
        end

        # |@ soma.dwuir.color.integer
        #
        # |@block
        # Number terms that fit within the unsigned 32-bit range are parsed as
        # "hex" colors.
        #
        # Obviously at this point we have no idea about the way they were
        # written; there is little "hex" about this color. Regardless, the color
        # is split into the constituent four bytes (i.e. `0xRRGGBBAA`); and
        # those bytes are then resolved as an RGBA color.
        #
        # NOTE: since this variant cannot distinguish between colors of the form
        # `0xRRGGBBAA` vs. `0xRRGGBB`, you **must** specify the alpha component. If you
        # don't like that, use the named/hex color variant (`soma.dwuir.color.string`).
        # |@endblock
        matchpi %{(%number u32)} do
          rgba(term.to(UInt32))
        end

        otherwise do
          Log.debug { "invalid color term: #{term}" }

          fallback
        end
      end
    end

    # Maps the red component into unit range (0-1).
    def ur : Float32
      @r/255.0f32
    end

    # Maps the green component into unit range (0-1).
    def ug : Float32
      @g/255.0f32
    end

    # Maps the blue component into unit range (0-1).
    def ub : Float32
      @b/255.0f32
    end

    # Maps the alpha component into unit range (0-1).
    #
    # `0` means fully transparent. `1` means fully opaque.
    def ua : Float32
      @a/255.0f32
    end

    # Returns four floats, representing the red, green, blue, and alpha components
    # mapped to unit range (0-1).
    def urgba : {Float32, Float32, Float32, Float32}
      {ur, ug, ub, ua}
    end

    # Shorthand for a tuple of `{r, g, b}`.
    def rgb : {UInt8, UInt8, UInt8}
      {r, g, b}
    end

    # Shorthand for a tuple of `{r, g, b, a}`.
    def rgba : {UInt8, UInt8, UInt8, UInt8}
      {*rgb, a}
    end

    # Returns `true` if this color is *fully* transparent; meaning its alpha value
    # is exactly `0`. Returns `false` otherwise.
    def transparent? : Bool
      @a.zero?
    end

    # Returns `true` if this color is fully opaque; meaning its alpha value is
    # exactly `255`. Returns `false` otherwise.
    def opaque? : Bool
      @a == 255
    end
  end
end

require "./color/named"
require "./color/hsl"
require "./color/oklch"
