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
    tracking : Measure,
    underline : UnderlineSpec?,
    color : Paint::Any

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
