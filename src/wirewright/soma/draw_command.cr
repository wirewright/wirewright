module Ww::Soma
  # Defines the kinds of shapes that can be drawn.
  alias DrawShape = RectShape | FragShape

  # Represents a rectangle shape. Optionally, *border* and corner *radii* can
  # be provided.
  #
  # - *fill* specifies the paint with which the rectangle will be filled.
  # - For borders, use `RectBorder#color`.
  record RectShape, fill : Paint::Any, border = RectBorder.new, radii = RectRadii.new do
    # Clamps the properties of this rectangle shape to respect *bounds*.
    def clamp(bounds : Rect) : RectShape
      copy_with(radii: radii.clamp(bounds), border: border.clamp(bounds))
    end
  end

  # Represents the four sides of a rectangle along with the paint that they should
  # be stroked with.
  record RectBorder, l = 0.0f32, r = 0.0f32, t = 0.0f32, b = 0.0f32, color : Paint::Any = Paint.transparent do
    # Multiplies all four sides by *scale*.
    def *(scale : Float32) : RectBorder
      copy_with(l: l * scale, r: r * scale, t: t * scale, b: b * scale)
    end

    # Pads *rect* by the sizes of each side.
    def pad(rect : Rect) : Rect
      Rect.new(
        rect.tl + Point.new(l, t),
        rect.br - Point.new(r, b),
      )
    end

    # Clamps the sizes of each side according to *bounds*.
    def clamp(bounds : Rect) : RectBorder
      copy_with(
        l: l.clamp(0.0f32..bounds.w/4),
        r: r.clamp(0.0f32..bounds.w/4),
        t: t.clamp(0.0f32..bounds.h/4),
        b: b.clamp(0.0f32..bounds.h/4),
      )
    end

    # Returns `true` if this border is transparent. Returns `false` otherwise.
    def transparent? : Bool
      return true if Approx.equals?(l + r + t + b, 0.0f32)

      Paint.transparent?(color)
    end
  end

  # Represents the radii of the four rectangle corners.
  record RectRadii, tl = 0.0f32, tr = 0.0f32, bl = 0.0f32, br = 0.0f32 do
    def self.all(radius r : Float32)
      new(r, r, r, r)
    end

    # Returns the top-left corner radius as a point.
    def tl2 : Point
      Point.new(tl, tl)
    end

    # Returns the top-right corner radius as a point.
    def tr2 : Point
      Point.new(tr, tr)
    end

    # Returns the bottom-left corner radius as a point.
    def bl2 : Point
      Point.new(bl, bl)
    end

    # Returns the bottom-right corner radius as a point.
    def br2 : Point
      Point.new(br, br)
    end

    # Clamps each radius according to *bounds*.
    def clamp(bounds : Rect) : RectRadii
      rmax = Math.min(bounds.w, bounds.h)/2

      copy_with(
        tl: tl.clamp(0.0f32..rmax),
        tr: tr.clamp(0.0f32..rmax),
        bl: bl.clamp(0.0f32..rmax),
        br: br.clamp(0.0f32..rmax),
      )
    end
  end

  # Represents a snippet of *inline*, *printable* text.
  #
  # - *font* specifies an absolute path to the font file in one of the supported formats.
  # - *size* specifies the font size.
  # - *tracking* specifies the letter spacing of the text (named after Tailwind).
  # - *underline* can optionally enable and configure the underline.
  # - *color* specifies the paint with which the text will be filled. You can use
  #   any `Paint`, including solid color, image, and gradient paint.
  record FragShape,
    string : String,
    font : Path,
    size : Float32,
    tracking : Magn,
    underline : UnderlineSpec?,
    color : Paint::Any

  # Configures the underline for `FragShape`.
  #
  # - If *color* is `nil`, the underline's color would be the same as the text
  #   color at that point.
  record UnderlineSpec,
    color : Paint::Any?,
    thickness : Float32,
    offset : Float32

  # Lists the available ranks for a draw command. Most shapes are in the `Mid`
  # rank. Something like a selection rect would be in the `Back` rank. Something
  # like a cursor rectangle would be in the `Front` rank.
  #
  # See also: `DrawCommand`.
  enum Rank : Int32
    Back
    Mid
    Front
  end

  # Provides an exhaustive context so as to how a `DrawShape` should actually be
  # drawn: how it should be clipped, what transformations should be applied to
  # it, etc.
  #
  # - *view* is the untransformed view (aka clipping) rect.
  # - *view tf* is the transformation that should be applied to *view*.
  # - *bounds* is the untransformed bounding box of *shape*.
  # - *bounds tf* is the transformation that should be applied to *bounds*; and also
  #   to *shape* when drawing.
  # - *layer* is the z-index of this command. Commands are sorted by *layer*; smaller
  #   layer is drawn before larger layer.
  # - *rank* is similar to *layer* in purpose. It is used to sort things such as cursor
  #   or selection rectangles which are otherwise members of the same *layer*.
  # - *opacity* specifies the opacity of the resulting shape.
  # - *shape* specifies the `DrawShape` itself.
  defcase DrawCommand,
    view : Rect,
    view_tf : Tf,
    bounds : Rect,
    bounds_tf : Tf,
    layer : Int32,
    rank : Rank,
    opacity : Float32,
    shape : DrawShape

  class DrawCommand
    # Extracts a `DrawKey` from this draw command.
    def key : DrawKey
      DrawKey.new(bounds.size.round, bounds_tf, shape)
    end
  end

  # A draw key is used to cache the results of rasterizing `DrawCommand`s. Therefore,
  # it must contain properties that would change how the shape is rasterized. This
  # includes the *shape* itself; its size (*extent*), and the transformations applied
  # to it (*tf*).
  #
  # Note that we do not include standalone translations in `DrawKey` nor in *tf*; this
  # means translating (moving) a draw command (aka drawing at a different origin) will
  # reuse the rasterization from an older position, if any. On the other hand, rotation
  # and scale do change *tf*, and thus will trigger a re-rasterization. The older cache
  # entry will likely be evicted from the cache, replaced by the new rotated/scaled one.
  record DrawKey, extent : Point, tf : Tf, shape : DrawShape
end
