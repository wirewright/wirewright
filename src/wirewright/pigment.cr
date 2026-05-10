# Pigment is a small language for expressing colors. It is used by various
# parts of Wirewright's visual stack.
#
# ```wwml
# (oklch 0.5 0.3 red)
# (translucent (rgb 0 255 0) 0.3)
# (mix (oklch 0.5 0.3 red) (translucent (oklch 0.2 0.1 green) 0.7) 0.4)
# ```
module Ww::Pigment
  extend self

  # Linear (0-1) RGBA color.
  struct RGBA
    getter r : Float32
    getter g : Float32
    getter b : Float32
    getter a : Float32

    def initialize(@r, @g, @b, @a)
    end

    def rgb : {Float32, Float32, Float32}
      {r, g, b}
    end

    def rgba : {Float32, Float32, Float32, Float32}
      {*rgb, a}
    end

    def r8 : UInt8
      (r * 255).floor.to_u8
    end

    def g8 : UInt8
      (g * 255).floor.to_u8
    end

    def b8 : UInt8
      (b * 255).floor.to_u8
    end

    def a8 : UInt8
      (a * 255).floor.to_u8
    end

    def rgb8 : {UInt8, UInt8, UInt8}
      {r8, g8, b8}
    end

    def rgba8 : {UInt8, UInt8, UInt8, UInt8}
      {*rgb8, a8}
    end

    def to_pvg
      PlutoVG::Color.new(r: r, g: g, b: b, a: a)
    end

    def transparent? : Bool
      a.approx?(0.0)
    end

    def translucent? : Bool
      a < 255.0
    end

    def inspect(io)
      io << "rgba("
      io << r << ", " << g << ", " << b << ", " << a
      io << ")"
    end

    def to_s(io)
      inspect(io)
    end
  end

  # Mixes two colors *a* and *b* according to *ratio*.
  #
  # This is the implementation of the `pigment.mix` color function.
  def mix(a : RGBA, b : RGBA, ratio : Float32) : RGBA
    ratio = ratio.clamp(0.0f32..1.0f32)

    RGBA.new(
      r: a.r + (b.r - a.r) * ratio,
      g: a.g + (b.g - a.g) * ratio,
      b: a.b + (b.b - a.b) * ratio,
      a: a.a + (b.a - a.a) * ratio,
    )
  end

  # Constructs an `RGBA` color from 8-bit components *r*, *g*, *b*, *a*.
  def rgba(r : UInt8, g : UInt8, b : UInt8, a : UInt8 = 255) : RGBA
    RGBA.new(r / 255.0f32, g / 255.0f32, b / 255.0f32, a / 255.0f32)
  end

  # Constructs an `RGBA` color from a CSS named color *name*. Uses *fallback* if not
  # found. See `CSSColor.named?`.
  def named(name : String, *, fallback : RGBA = rgba(0, 0, 0)) : RGBA
    unless rgba = CSSColor.named?(name)
      return fallback
    end

    rgba(*rgba)
  end

  # Constructs a white color.
  def white : RGBA
    RGBA.new(1.0, 1.0, 1.0, 1.0)
  end

  # Constructs a black color.
  def black : RGBA
    RGBA.new(0.0, 0.0, 0.0, 1.0)
  end

  # Constructs a transparent color.
  def transparent : RGBA
    RGBA.new(0.0, 0.0, 0.0, 0.0)
  end

  alias Out = Outcome::Accepted(RGBA?) | Outcome::Rejected

  private def ok(result : RGBA?)
    Outcome.ok(result.as(RGBA?))
  end

  private def ok_despite(result : RGBA?, *args)
    Outcome.ok_despite(result.as(RGBA?), *args)
  end

  private def ok_clamp(value : Float, range, *, despite : {_, _})
    key, detail = despite

    Outcome.ok_despite(value.clamp(range), detail).at(key)
  end

  private def rej
    Outcome.rej
  end

  HUES = {
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

  private def oklch(expr : Term) : Out
    # |@ pigment.oklch
    #
    # |@pattern
    # (oklch ±l ±c ±h)
    # (oklch ±l ±c h_symbol)
    # (oklch ±l ±c h_string)
    #
    # |@key l
    # Lightness of the color (0-1, i.e., 0-100%).
    #
    # |@key c
    # Chroma of the color (0-0.36).
    #
    # |@key h
    # Hue of the color (0-360, degrees) or its name (as a symbol or strins).
    # The following hue names are available:
    # - red
    # - orange
    # - amber
    # - yellow
    # - lime
    # - green
    # - teal
    # - cyan
    # - sky
    # - blue
    # - indigo
    # - violet
    # - purple
    # - magenta
    # - pink
    # - crimson
    #
    # |@block
    # Expresses a color in the Oklab color space using its *lightness*, *chroma*,
    # and *hue* components.
    #
    # Reference: https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/color_value/oklch
    #
    # OkLCH is the preferred way to express colors in Wirewright.
    #
    # ```
    # (oklch 0.3 0.2 red)
    # (oklch 0.3 0.2 green)
    # (oklch 0.8 0.15 330)
    # (translucent (oklch 0.3 0.2 green) 0.8)
    # ```
    Term.case(expr) do
      matchpi %{(oklch ±l ±c ±h)}, l: Float64, c: Float64, h: Float64 do |l, c, h|
        Outcome.accumulate do |acc|
          unless l.in?(0.0..1.0)
            l = acc.unwrap(ok_clamp(l, 0.0..1.0, despite: {1, "oklch lightness out of range 0-1 (i.e, 0-100%)"}))
          end

          unless c.in?(0.0..0.36)
            c = acc.unwrap(ok_clamp(c, 0.0..0.36, despite: {2, "oklch chroma out of range 0-0.36"}))
          end

          unless h.in?(0.0..360.0)
            h = acc.unwrap(ok_clamp(h, 0.0..360.0, despite: {3, "oklch hue out of range 0-360°"}))
          end

          r, g, b = Oklch.to_lrgb(l, c, h)

          ok(RGBA.new(r.to_f32, g.to_f32, b.to_f32, a: 1.0))
        end
      end

      matchpi %{(oklch ±l ±c (%any° h_string h_symbol))}, h: String do
        unless hv = HUES[h]?
          return ok_despite(nil, "unrecognized hue name").at(3)
        end

        oklch(Term.of(:oklch, l, c, hv))
      end

      matchpi %{(oklch _*)} do
        ok_despite(nil, "unrecognized `oklch` expression, expected (oklch _number _number _number)")
      end

      otherwise { rej }
    end
  end

  private def hsl(expr : Term) : Out
    # |@ pigment.hsl
    #
    # |@pattern
    # (hsl ±h ±s ±l)
    #
    # |@key h
    # Hue of the color (0-360, degrees).
    #
    # |@key s
    # Saturation of the color (0-1, i.e., 0-100%).
    #
    # |@key l
    # Lightness of the color (0-1, i.e., 0-100%).
    #
    # |@block
    # Expresses a color in the sRGB color space according to its hue, saturation,
    # and lightness components.
    #
    # Reference: https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/color_value/hsl
    #
    # ```
    # (hsl 17.44 0.54 0.62)
    # (translucent (hsl 17.44 0.54 0.62) 0.3)
    # ```
    Term.case(expr) do
      matchpi %{(hsl ±h ±s ±l)}, h: Float64, s: Float64, l: Float64 do |h, s, l|
        Outcome.accumulate do |acc|
          unless h.in?(0.0..360.0)
            h = acc.unwrap(ok_clamp(h, 0.0..360.0, despite: {1, "hsl hue out of range 0-360°"}))
          end

          unless s.in?(0.0..1.0)
            s = acc.unwrap(ok_clamp(s, 0.0..1.0, despite: {2, "hsl saturation out of range 0-1 (i.e. 0-100%)"}))
          end

          unless l.in?(0.0..1.0)
            l = acc.unwrap(ok_clamp(l, 0.0..1.0, despite: {3, "hsl lightness out of range 0-1 (i.e. 0-100%)"}))
          end

          r, g, b = HSL.to_lrgb(h, s, l)

          ok(RGBA.new(r.to_f32, g.to_f32, b.to_f32, a: 1.0))
        end
      end

      matchpi %{(hsl _*)} do
        ok_despite(nil, "unrecognized `hsl` expression, expected (hsl _number _number _number)")
      end

      otherwise { rej }
    end
  end

  private def rgb(expr : Term) : Out
    Term.case(expr) do
      # |@ pigment.rgb
      #
      # |@pattern
      # (rgb ±r ±g ±b)
      #
      # |@key r
      # The color's red component (0-255).
      #
      # |@key g
      # The color's green component (0-255).
      #
      # |@key b
      # The color's blue component (0-255).
      #
      # |@block
      # Expresses a color in the sRGB color space according to its red, green,
      # and blue components (0-255).
      #
      # ```
      # (rgb 0 255 0)
      # (translucent (rgb 32 64 128) 0.8)
      # ```
      matchpi %{(rgb ±r ±g ±b)}, r: Float32, g: Float32, b: Float32 do |r, g, b|
        Outcome.accumulate do |acc|
          unless r.in?(0.0..255.0)
            r = acc.unwrap(ok_clamp(r, 0.0f32..255.0f32, despite: {1, "rgb red out of range 0-255"}))
          end

          unless g.in?(0.0..255.0)
            g = acc.unwrap(ok_clamp(g, 0.0f32..255.0f32, despite: {2, "rgb green out of range 0-255"}))
          end

          unless b.in?(0.0..255.0)
            b = acc.unwrap(ok_clamp(b, 0.0f32..255.0f32, despite: {3, "rgb blue out of range 0-255"}))
          end

          ok(RGBA.new(r / 255, g / 255, b / 255, a: 1.0))
        end
      end

      # |@ pigment.rgba
      #
      # |@pattern
      # (rgba ±r ±g ±b ±a)
      #
      # |@key r
      # The color's red component (0-255).
      #
      # |@key g
      # The color's green component (0-255).
      #
      # |@key b
      # The color's blue component (0-255).
      #
      # |@key a
      # The color's alpha component (0-255).
      #
      # |@block
      # Expresses a color in the sRGB color space according to its red, green,
      # and blue components (0-255), and alpha (0-255) for opacity.
      #
      # The `rgba` variant and its alpha component are only recommended in cases
      # where you absolutely cannot avoid 0-255 alpha. You should otherwise use
      # the more idiomatic `translucent` color function.
      #
      # ```
      # (rgba 128 255 128 30)
      # ```
      matchpi %{(rgba ±r ±g ±b ±a)}, r: Float32, g: Float32, b: Float32, a: Float32 do |r, g, b, a|
        Outcome.accumulate do |acc|
          unless r.in?(0.0..255.0)
            r = acc.unwrap(ok_clamp(r, 0.0f32..255.0f32, despite: {1, "rgb red out of range 0-255"}))
          end

          unless g.in?(0.0..255.0)
            g = acc.unwrap(ok_clamp(g, 0.0f32..255.0f32, despite: {2, "rgb green out of range 0-255"}))
          end

          unless b.in?(0.0..255.0)
            b = acc.unwrap(ok_clamp(b, 0.0f32..255.0f32, despite: {3, "rgb blue out of range 0-255"}))
          end

          unless a.in?(0.0..255.0)
            a = acc.unwrap(ok_clamp(a, 0.0f32..255.0f32, despite: {4, "rgb alpha out of range 0-255"}))
          end

          ok(RGBA.new(r / 255, g / 255, b / 255, a / 255))
        end
      end

      # |@ pigment.lrgb
      #
      # |@key r
      # The color's red component (0-1).
      #
      # |@key g
      # The color's green component (0-1).
      #
      # |@key b
      # The color's blue component (0-1).
      #
      # |@block
      # Expresses a color in the sRGB color space according to its red, green,
      # and blue components (0-1).
      #
      # ```
      # (lrgb 0.5 1.0 0.5) ;; (rgb 128 255 128)
      # ```
      matchpi %{(lrgb ±r ±g ±b)}, r: Float32, g: Float32, b: Float32 do |r, g, b|
        Outcome.accumulate do |acc|
          unless r.in?(0.0..1.0)
            r = acc.unwrap(ok_clamp(r, 0.0f32..1.0f32, despite: {1, "lrgb red out of range 0-1"}))
          end

          unless g.in?(0.0..1.0)
            g = acc.unwrap(ok_clamp(g, 0.0f32..1.0f32, despite: {2, "lrgb green out of range 0-1"}))
          end

          unless b.in?(0.0..1.0)
            b = acc.unwrap(ok_clamp(b, 0.0f32..1.0f32, despite: {3, "lrgb blue out of range 0-1"}))
          end

          ok(RGBA.new(r, g, b, a: 1.0))
        end
      end

      matchpi %{[rgb _*]} do
        ok_despite(nil, "unrecognized `rgb` expression, expected `(rgb _number _number _number)`")
      end

      matchpi %{[rgba _*]} do
        ok_despite(nil, "unrecognized `rgba` expression, expected `(rgba _number _number _number _number)`")
      end

      matchpi %{[lrgb _*]} do
        ok_despite(nil, "unrecognized `lrgb` expression, expected `(lrgb _number _number _number)`")
      end

      otherwise { rej }
    end
  end

  private def css(term : Term) : Out
    Term.case(term) do
      # |@ pigment.named
      #
      # |@pattern
      # _symbol
      #
      # |@block
      # Specifies a color using its CSS `<named-color>` name.
      #
      # See https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/named-color for
      # info on named colors.
      #
      # ```
      # (translucent seagreen 0.3)
      # ```
      matchpi %{_symbol} do
        name = term.to(String)
        unless rgba = CSSColor.named?(name)
          return ok_despite(nil, "unrecognized color name")
        end

        r, g, b, a = rgba

        ok(RGBA.new(r / 255.0f32, g / 255.0f32, b / 255.0f32, a / 255.0f32))
      end

      # |@ pigment.hex
      #
      # |@pattern
      # _string
      #
      # |@block
      # Specifies a color using the CSS `<hex-color>` notation.a subset of CSS hex color notation.
      #
      # See https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/hex-color for info.
      #
      # ```
      # "#f09"
      # ```
      matchpi %{_string} do
        name = term.to(String)
        unless rgba = CSSColor.named?(name) || CSSColor.hexcolor?(name)
          return ok_despite(nil, "unrecognized string: expected a color name (e.g. `blue`) or a hexcolor (e.g. `#fff` or `#f0f0f0`)")
        end

        r, g, b, a = rgba

        ok(RGBA.new(r / 255.0f32, g / 255.0f32, b / 255.0f32, a / 255.0f32))
      end

      otherwise { rej }
    end
  end

  private def atom(expr : Term) : Out
    Outcome.choice(oklch(expr), hsl(expr), rgb(expr), css(expr))
  end

  private def fn(expr : Term) : Out
    Term.case(expr) do
      # |@ pigment.translucent
      #
      # |@pattern
      # (translucent color_ ±opacity)
      #
      # |@key color pigment
      # The base color.
      #
      # |@key opacity
      # The opacity multiplier (0-1). `0` means fully transparent. `1` means keep
      # *color*'s opacity (which is most likely `1`; thus, `1` would mean
      # fully opaque).
      #
      # |@block
      # Modifies the opacity of *color* by multiplying it by *opacity*.
      matchpi %{(translucent arg_ ±opacity)}, opacity: Float32 do |opacity|
        Outcome.accumulate do |acc|
          unless opacity.in?(0.0..1.0)
            opacity = acc.unwrap(ok_clamp(opacity, 0.0f32..1.0f32, despite: {2, "opacity out of range 0-1"}))
          end

          color = acc.unwrap(eval(arg).at(1)) { }

          if color
            ok(RGBA.new(color.r, color.g, color.b, color.a * opacity))
          else
            ok(nil)
          end
        end
      end

      # |@ pigment.mix
      #
      # |@pattern
      # (mix color0_ color1_ ±ratio)
      #
      # |@key color0 pigment
      # The first color.
      #
      # |@key color1 pigment
      # The second color.
      #
      # |@key ratio
      # The mix ratio (0-1). `0` means just the first color. `1` means just the second
      # one. Values between 0 and 1 mix.
      #
      # |@block
      # Mixes two colors to form one.
      #
      # ```
      # (mix red blue 0.3)
      # ```
      matchpi %{(mix arg0_ arg1_ ±ratio)}, ratio: Float32 do |ratio|
        Outcome.accumulate do |acc|
          unless ratio.in?(0.0..1.0)
            ratio = acc.unwrap(ok_clamp(ratio, 0.0f32..1.0f32, despite: {3, "ratio out of range 0-1"}))
          end

          color0 = acc.unwrap(eval(arg0).at(1)) { }
          color1 = acc.unwrap(eval(arg1).at(2)) { }

          if color0 && color1
            ok(mix(color0, color1, ratio))
          else
            ok(nil)
          end
        end
      end

      otherwise { rej }
    end
  end

  @@cache = SyncLRU(Term, Out).new(capacity: 256)

  # Returns the RGBA value of *expr*, if any, along with zero or more diagnostics.
  def eval(expr : Term) : Out
    @@cache.put_if_absent(expr) do
      Outcome.choice(atom(expr), fn(expr))
    end
  end

  # Returns the RGBA value of *expr*. Discards all diagnostics. Returns `nil`
  # if *expr* has no color value.
  #
  # Use `eval` instead if you want to obtain detailed diagnostics.
  def rgba?(expr : Term) : RGBA?
    eval(expr).unwrap?
  end

  # Returns the RGBA value of *expr*. Discards all diagnostics. Returns *fallback*
  # if *expr* has no color value.
  #
  # Use `eval` instead if you want to obtain detailed diagnostics.
  def rgba(term : Term, fallback : RGBA = rgba(0, 0, 0)) : RGBA
    rgba?(term) || fallback
  end
end
