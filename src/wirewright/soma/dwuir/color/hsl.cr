module Ww::Soma::DwUIR
  # Functions to convert HSL to RGB and vice versa.
  #
  # Reference: https://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
  module HSL
    extend self

    # Returns H, S, L for the given RGB color.
    #
    # - *r*, *g*, and *b* are 0-255.
    def self.from_rgb(r : UInt8, g : UInt8, b : UInt8) : {Float64, Float64, Float64}
      ur, ug, ub = r / 255, g / 255, b / 255
      min, max = {ur, ug, ub}.minmax

      l = (min + max)/2

      if min == max
        return 0.0, 0.0, l
      end

      if l <= 0.5
        s = (max - min)/(max + min)
      else # l > 0.5
        s = (max - min)/(2.0 - max - min)
      end

      case max
      when ur
        uh = (ug - ub) / (max - min)
      when ug
        uh = 2.0 + (ub - ur)/(max - min)
      when ub
        uh = 4.0 + (ur - ug)/(max - min)
      else
        unreachable
      end

      h = uh * 60
      if h < 0
        h += 360
      end

      {h, s, l}
    end

    # Returns R, G, B for the given HSL color.
    #
    # - *h* (hue) is in degrees, clamped to 0-360.
    # - *s* (saturation) is in percents, clamped to 0-1.
    # - *l* (saturation) is in percents, clamped to 0-1.
    def self.to_rgb(h : Float64, s : Float64, l : Float64) : {UInt8, UInt8, UInt8}
      h = h.clamp(0.0..360.0)
      s = s.clamp(0.0..1.0)
      l = l.clamp(0.0..1.0)

      if Approx.equals?(s, 0)
        shade = (l * 255).clamp(0.0..255.0).to_u8

        return shade, shade, shade
      end

      if l < 0.5
        a = l * (1.0 + s)
      else # l >= 0.5
        a = l + s - l * s
      end

      b = 2 * l - a
      uh = h / 360

      ur, ug, ub = {uh + 0.333, uh, uh - 0.333}.map do |v|
        if v < 0
          v += 1
        elsif v > 1
          v -= 1
        end

        if 6 * v < 1
          u = b + (a - b) * 6 * v
          next u if u <= 1
        end

        if 2 * v < 1
          u = a
          next u if u <= 1
        end

        if 3 * v < 2
          u = b + (a - b) * (0.666 - v)*6
          next u if u <= 1
        end

        b
      end

      r = (ur * 255).clamp(0.0..255.0).round.to_u8
      g = (ug * 255).clamp(0.0..255.0).round.to_u8
      b = (ub * 255).clamp(0.0..255.0).round.to_u8

      {r, g, b}
    end
  end
end
