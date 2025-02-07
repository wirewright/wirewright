# Ported from: https://gist.github.com/dkaraush/65d19d61396f5f3cd8ba7d1b4b3c9432
module Oklch
  extend self

  private M1 = {
    1, 0.3963377773761749, 0.2158037573099136,
    1, -0.1055613458156586, -0.0638541728258133,
    1, -0.0894841775298119, -1.2914855480194092,
  }

  private M2 = {
    1.2268798758459243, -0.5578149944602171, 0.2813910456659647,
    -0.0405757452148008, 1.1122868032803170, -0.0717110580655164,
    -0.0763729366746601, -0.4214933324022432, 1.5869240198367816,
  }

  private M3 = {
    0.8190224379967030, 0.3619062600528904, -0.1288737815209879,
    0.0329836539323885, 0.9292868615863434, 0.0361446663506424,
    0.0481771893596242, 0.2642395317527308, 0.6335478284694309,
  }

  private M4 = {
    0.2104542683093140, 0.7936177747023054, -0.0040720430116193,
    1.9779985324311684, -2.4285922420485799, 0.4505937096174110,
    0.0259040424655478, 0.7827717124575296, -0.8086757549230774,
  }

  private M5 = {
    3.2409699419045226, -1.537383177570094, -0.4986107602930034,
    -0.9692436362808796, 1.8759675015077202, 0.04155505740717559,
    0.05563007969699366, -0.20397695888897652, 1.0569715142428786,
  }

  private M6 = {
    0.41239079926595934, 0.357584339383878, 0.1804807884018343,
    0.21263900587151027, 0.715168678767756, 0.07219231536073371,
    0.01933081871559182, 0.11919477979462598, 0.9505321522496607,
  }

  private def matmul(a, b)
    {
      a[0]*b[0] + a[1]*b[1] + a[2]*b[2],
      a[3]*b[0] + a[4]*b[1] + a[5]*b[2],
      a[6]*b[0] + a[7]*b[1] + a[8]*b[2],
    }
  end

  private def oklch2oklab(lch)
    l, c, h = lch

    {l, c * Math.cos(h * Math::PI / 180), c * Math.sin(h * Math::PI / 180)}
  end

  private def oklab2oklch(lab)
    l, a, b = lab

    if a.abs < 0.0002 && b.abs < 0.0002
      h = 0.0
    else
      h = (Math.atan2(b, a) * 180) / Math::PI
      h %= 360
      h += 360
      h %= 360
    end

    {l, Math.hypot(a, b), h}
  end

  private def rgb2srgbl(rgb)
    rgb.map do |c|
      if c.abs <= 0.04045
        c / 12.92
      else
        c.sign * (((c.abs + 0.055) / 1.055) ** 2.4)
      end
    end
  end

  private def srgbl2rgb(rgb)
    rgb.map do |c|
      if c.abs > 0.0031308
        c.sign * (1.055 * (c.abs ** (1 / 2.4)) - 0.055)
      else
        12.92 * c
      end
    end
  end

  private def oklab2xyz(lab)
    lmsg = matmul(M1, lab)
    lms = lmsg.map { |val| val ** 3 }
    matmul(M2, lms)
  end

  private def xyz2oklab(xyz)
    lms = matmul(M3, xyz)
    lmsg = lms.map { |val| Math.cbrt(val) }
    matmul(M4, lmsg)
  end

  private def xyz2srgbl(xyz)
    matmul(M5, xyz)
  end

  private def srgbl2xyz(rgb)
    matmul(M6, rgb)
  end

  private def oklch2urgb(lch)
    pipe(lch, oklch2oklab, oklab2xyz, xyz2srgbl, srgbl2rgb)
  end

  private def urgb2oklch(rgb)
    pipe(rgb, rgb2srgbl, srgbl2xyz, xyz2oklab, oklab2oklch)
  end

  # Lists supported ways to fit a color into the sRGB gamut.
  enum FitMethod : UInt8
    # Clamps into sRGB.
    #
    # Not recommended: can mutilate colors. Use `Slow` instead.
    Clamp

    # Decreases chroma until in sRGB.
    #
    # Recommended but about 10x slower than `Clamp`. Still going to be roughly 1µs
    # though, whereas `Clamp` is roughly 100ns.
    Smart
  end

  # Returns R, G, B for the given OkLCH color.
  #
  # - *l* (lightness) is in percents, clamped to 0-100.
  # - *c* (chroma) is clamped to 0-0.36. It is theoretically unbounded but we
  #   lose sRGB well before 0.36 for most colors.
  # - *h* (hue) is in degrees, clamped to 0-360.
  def to_rgb(l : Float64, c : Float64, h : Float64, *, fit : FitMethod = :smart) : {UInt8, UInt8, UInt8}
    l = l.clamp(0.0..100.0)
    c = c.clamp(0.0..0.36)
    h = h.clamp(0.0..360)

    l /= 100

    r, g, b = oklch2urgb({l, c, h})

    case fit
    in .smart?
      unless 0.0 < r < 1.0 && 0.0 < g < 1.0 && 0.0 < b < 1.0
        # Decrease chroma by 0.1 until in range.
        #
        # We consider max(c) = 0.36, max(c) - 0.1 - 0.1 - 0.1 = 0.06.
        3.times do
          c = Math.max(c - 0.1, 0.0)
          r, g, b = oklch2urgb({l, c, h})
          break if {r, g, b}.all?(&.in?(0.0..1.0))
        end

        # Increase chroma by 0.01 until out of range.
        #
        # Since the range check flipped between a decrement of 0.1 we only need
        # to check in 0.01-0.1.
        10.times do
          break unless 0.0 < r < 1.0 && 0.0 < g < 1.0 && 0.0 < b < 1.0
          c = Math.min(c + 0.01, 0.36)
          r, g, b = oklch2urgb({l, c, h})
        end

        # Decrease chroma by 0.001 until in range.
        #
        # Sicne the range check flipped between an increment of 0.01 we only need
        # to check in 0.001-0.01.
        10.times do
          break if 0.0 < r < 1.0 && 0.0 < g < 1.0 && 0.0 < b < 1.0
          c = Math.max(c - 0.001, 0.0)
          r, g, b = oklch2urgb({l, c, h})
        end
      end
    in .clamp?
    end

    {(r*255).round.to_i.clamp(0..255).to_u8,
     (g*255).round.to_i.clamp(0..255).to_u8,
     (b*255).round.to_i.clamp(0..255).to_u8}
  end

  # Returns L, C, H for the given RGB color.
  #
  # - *r*, *g*, and *b* are 0-255.
  def from_rgb(r : UInt8, g : UInt8, b : UInt8) : {Float64, Float64, Float64}
    l, c, h = urgb2oklch({r/255.0, g/255.0, b/255.0})

    {l * 100, c, h}
  end
end
