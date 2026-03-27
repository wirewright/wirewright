# Pigment is a tiny language for describing colors, reused by various parts
# of Wirewright's visual stack.
#
# ```wwml
# (oklch 0.5 0.3 red)
# (translucent (rgb 0 255 0) 0.3)
# (mix (oklch 0.5 0.3 red) (translucent (oklch 0.2 0.1 green) 0.7) 0.4)
# ```
module Ww::Pigment
  extend self

  # Linear (0-1) RGBA color.
  record RGBA, r : Float32, g : Float32, b : Float32, a : Float32 do
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

    def transparent? : Bool
      a.approx?(0.0)
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

  # Represents the result of parsing a color.
  alias Π = RGBA | Parseout::Nok

  private def err
    Parseout::Err.new
  end

  private def rej
    Parseout::Rej.new
  end

  alias Cache = ICache(Term, RGBA)

  # <hue name>
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

  # ```grammar:pigment
  # <oklch>
  #   (oklch l_number c_number h_number)
  #   (oklch l_number c_number <hue name>)
  # ```
  def oklch(term : Term, issues : Issue::Sink) : Π
    Term.case(term) do
      matchpi %{(oklch ±l ±c ±h)} do
        fl = l.to(Float64)
        fc = c.to(Float64)
        fh = h.to(Float64)

        unless fl.in?(0.0..1.0)
          issues.major("oklch lightness out of range 0-1 (i.e., 0-100%): #{l}")
        end

        unless fc.in?(0.0..0.36)
          issues.minor("oklch chroma out of range 0-0.36 makes no sense: #{c}")
        end

        unless fh.in?(0.0..360.0)
          issues.major("oklch hue out of range 0-360°: #{h}")
        end

        r, g, b = Oklch.to_lrgb(fl, fc, fh)

        RGBA.new(r.to_f32, g.to_f32, b.to_f32, a: 1.0)
      end

      matchpi %{(oklch ±l ±c (%any° h_string h_symbol))} do
        unless hv = HUES[h.to(String)]?
          issues.major("unrecognized hue name: #{h}")
          return err
        end

        oklch(Term.of(:oklch, l, c, hv), issues)
      end

      otherwise { rej }
    end
  end

  # ```grammar:pigment
  # <hsl>
  #   (hsl h_number s_number l_number)
  # ```
  def hsl(term : Term, issues : Issue::Sink) : Π
    Term.case(term) do
      matchpi %{(hsl ±h ±s ±l)} do
        fh = h.to(Float64)
        fs = s.to(Float64)
        fl = l.to(Float64)

        unless fh.in?(0.0..360.0)
          issues.major("hsl hue out of range 0-360°: #{h}")
        end

        unless fs.in?(0.0..1.0)
          issues.minor("hsl saturation out of range 0-1 (i.e. 0-100%): #{s}")
        end

        unless fl.in?(0.0..1.0)
          issues.minor("hsl lightness out of range 0-1 (i.e. 0-100%): #{l}")
        end

        r, g, b = HSL.to_lrgb(fh, fs, fl)

        RGBA.new(r.to_f32, g.to_f32, b.to_f32, a: 1.0)
      end

      otherwise { rej }
    end
  end

  # ```grammar:pigment
  # <rgb>
  #   (rgb r_number g_number b_number)
  #     0-255 RGB
  #   (lrgb r_number g_number b_number)
  #     Linear (0-1) RGB
  # ```
  def rgb(term : Term, issues : Issue::Sink) : Π
    Term.case(term) do
      matchpi %{(rgb ±r ±g ±b)} do
        fr = r.to(Float32)
        fg = g.to(Float32)
        fb = b.to(Float32)

        unless fr.in?(0.0..255.0)
          issues.major("rgb red out of range 0-255: #{r}")
          fr = fr.clamp(0.0f32..255.0f32)
        end

        unless fg.in?(0.0..255.0)
          issues.major("rgb green out of range 0-255: #{g}")
          fg = fg.clamp(0.0f32..255.0f32)
        end

        unless fb.in?(0.0..255.0)
          issues.major("rgb blue out of range 0-255: #{b}")
          fb = fb.clamp(0.0f32..255.0f32)
        end

        RGBA.new(fr / 255, fg / 255, fb / 255, a: 1.0)
      end

      matchpi %{(rgba ±r ±g ±b ±a)} do
        fr = r.to(Float32)
        fg = g.to(Float32)
        fb = b.to(Float32)
        fa = a.to(Float32)

        unless fr.in?(0.0..255.0)
          issues.major("rgb red out of range 0-255: #{r}")
          fr = fr.clamp(0.0f32..255.0f32)
        end

        unless fg.in?(0.0..255.0)
          issues.major("rgb green out of range 0-255: #{g}")
          fg = fg.clamp(0.0f32..255.0f32)
        end

        unless fb.in?(0.0..255.0)
          issues.major("rgb blue out of range 0-255: #{b}")
          fb = fb.clamp(0.0f32..255.0f32)
        end

        unless fa.in?(0.0..255.0)
          issues.major("rgb alpha out of range 0-255: #{a}")
          fa = fa.clamp(0.0f32..255.0f32)
        end

        RGBA.new(fr / 255, fg / 255, fb / 255, fa / 255)
      end

      matchpi %{(lrgb ±r ±g ±b)} do
        fr = r.to(Float32)
        fg = g.to(Float32)
        fb = b.to(Float32)

        unless fr.in?(0.0..1.0)
          issues.major("rgb red out of range 0-1: #{r}")
          fr = fr.clamp(0.0f32..1.0f32)
        end

        unless fg.in?(0.0..1.0)
          issues.major("rgb green out of range 0-1: #{g}")
          fg = fg.clamp(0.0f32..1.0f32)
        end

        unless fb.in?(0.0..1.0)
          issues.major("rgb blue out of range 0-1: #{b}")
          fb = fb.clamp(0.0f32..1.0f32)
        end

        RGBA.new(fr, fg, fb, a: 1.0)
      end

      otherwise { rej }
    end
  end

  # ```grammar:pigment
  # <css>
  #   <hex color>
  #   <named color>
  # ```
  def css(term : Term, issues : Issue::Sink) : Π
    Term.case(term) do
      matchpi %{_string}, %{_symbol} do
        name = term.to(String)

        unless rgba = CSSColor.named?(name) || CSSColor.hexcolor?(name)
          return rej
        end

        r, g, b, a = rgba

        RGBA.new(r / 255.0f32, g / 255.0f32, b / 255.0f32, a / 255.0f32)
      end

      matchpi %{(%number u32)} do
        u32 = term.to(UInt32)

        r = (u32 >> 24) & 0xff
        g = (u32 >> 16) & 0xff
        b = (u32 >> 8) & 0xff
        a = (u32 >> 0) & 0xff

        RGBA.new(r / 255.0f32, g / 255.0f32, b / 255.0f32, a / 255.0f32)
      end

      otherwise { rej }
    end
  end

  # ```grammar:pigment
  # <base>
  #   <oklch>
  #   <hsl>
  #   <rgb>
  #   <css>
  # ```
  def base(term : Term, issues : Issue::Sink) : Π
    Parseout.try(
      oklch(term, issues),
      hsl(term, issues),
      rgb(term, issues),
      css(term, issues),
    )
  end

  # ```grammar:pigment
  # <transform>
  #   (translucent <color> n_number)
  #   (mix <color₁> <color₂> ratio_number)
  # ```
  def transform(cache : Cache, term : Term, issues : Issue::Sink) : Π
    Term.case(term) do
      matchpi %{(translucent arg_ ±opacity)} do
        fopacity = opacity.to(Float32)

        unless fopacity.in?(0.0..1.0)
          issues.major("opacity out of range 0-1: #{opacity}")
          fopacity = fopacity.clamp(0.0f32..1.0f32)
        end

        Parseout.map(color(cache, arg, issues)) do |color|
          color.copy_with(a: color.a * fopacity)
        end
      end

      matchpi %{(mix arg0_ arg1_ ±ratio)} do
        fratio = ratio.to(Float32)

        unless fratio.in?(0.0..1.0)
          issues.major("ratio out of range 0-1: #{ratio}")
          fratio = fratio.clamp(0.0f32..1.0f32)
        end

        Parseout.map(color(cache, arg0, issues), color(cache, arg1, issues)) do |color0, color1|
          r = color0.r + (color1.r - color0.r) * fratio
          g = color0.g + (color1.g - color0.g) * fratio
          b = color0.b + (color1.b - color0.b) * fratio
          a = color0.a + (color1.a - color0.a) * fratio

          RGBA.new(r, g, b, a)
        end
      end

      otherwise { rej }
    end
  end

  # ```grammar:pigment
  # <color>
  #   <base>
  #   <transform>
  # ```
  def color(cache : Cache, term : Term, issues : Issue::Sink) : Π
    Parseout.cached(cache, term, issues) do
      Parseout.try(
        base(term, issues),
        transform(cache, term, issues),
      )
    end
  end

  # Runs Pigment on *term*. Returns the resulting `RGBA` color. Returns `nil`
  # on failure. Reports issues to *issues*. Supply *cache* to memoize.
  def rgba?(term : Term, issues : Issue::Sink, *, cache : Cache = Uncached(Term, RGBA).new) : RGBA?
    color(cache, term, issues).as?(RGBA)
  end

  # Runs Pigment on *term*. Returns the resulting `RGBA` color. Returns *fallback*
  # on failure. Discards all issues. Supply *cache* to memoize.
  def rgba(term : Term, *, cache : Cache = Uncached(Term, RGBA).new, fallback : RGBA = rgba(0, 0, 0)) : RGBA
    rgba, _ = Issue.setup(severity: :quiet) do |sink|
      rgba?(term, sink) || fallback
    end
    rgba
  end
end
