module Ww::Soma::DwUIR
  alias View = Slice(Quad)

  # The *layer rank* of a draw command is an object which it uses to negotiate its
  # draw order among other draw commands. A layer rank is represented with a slice
  # of z-indices and a tip z-index. This helps create a crude kind of scope for
  # z-indices, more commonly referred to as the *z-scope*.
  struct LayerRank
    include Comparable(LayerRank)

    # :nodoc:
    def initialize(@zs : Slice(Int16), @z : Int16)
    end

    # Constructs an empty layer rank.
    def self.[] : LayerRank
      new(Slice(Int16).empty, 0i16)
    end

    # Compares two layer ranks.
    #
    # The prefixes of both layer ranks are compared as in `Slice#<=>`.
    #
    # The shortest size of this vs. *other* layer rank is picked as
    # the prefix size.
    #
    # If prefixes are equal, layer rank sizes are compared. Layer ranks with
    # larger size draw underneath ones with a smaller size.
    def <=>(other : LayerRank)
      min = {@zs.size, other.@zs.size}.min
      min.times do |i|
        cmp = @zs[i] <=> other.@zs[i]
        return cmp unless cmp == 0
      end

      cmp = (@zs[min]? || @z) <=> (other.@zs[min]? || other.@z)
      return cmp unless cmp == 0

      @zs.size <=> other.@zs.size
    end

    # Replaces the tip z-index with *z*. Returns the modified copy of
    # this layer rank.
    def assign(z : Int16) : LayerRank
      LayerRank.new(@zs, z: z)
    end

    # Introduces a new z-scope: appends the tip z-index to the slice of
    # z-indices, and resets the tip to `0`. Returns the modified copy
    # of this layer rank.
    def local : LayerRank
      LayerRank.new(@zs.append(@z), z: 0i16)
    end

    def inspect(io)
      io << "Z<"
      @zs.join(io, ",")
      io << "," unless @zs.empty?
      io << @z
      io << ">"
    end

    def_equals_and_hash @zs, @z
  end

  # Defines the kinds of shapes that can be drawn.
  alias Shape = RectShape | FragShape | SvgShape

  # Represents a rectangle shape. Optionally, *border* and corner *radii* can
  # be provided.
  #
  # - *fill* specifies the paint with which the rectangle will be filled.
  # - For borders, use `RectBorder#color`.
  defcase RectShape,
    fill : Paint::Any,
    border : RectBorder,
    radii : RectRadii

  class RectShape
    # Clamps the properties of this rectangle shape to respect *bounds*.
    def clamp(bounds : Rect) : RectShape
      copy_with(radii: radii.clamp(bounds), border: border.clamp(bounds))
    end

    # See `Picture#each_line_to_display`.
    def each_line_to_display(&fn : Int32, String ->) : Nil
      fn.call(0, "rect(fill=#{fill})")
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
  defcase FragShape,
    string : String,
    font : Path,
    size : Float32,
    tracking : Magn,
    underline : UnderlineSpec?,
    color : Paint::Any

  class FragShape
    # See `Picture#each_line_to_display`.
    def each_line_to_display(&fn : Int32, String ->) : Nil
      fn.call(0, "frag(text=#{string.dump})")
    end
  end

  # Configures the underline for `FragShape`.
  #
  # - If *color* is `nil`, the underline's color would be the same as the text
  #   color at that point.
  record UnderlineSpec,
    color : Paint::Any?,
    thickness : Float32,
    offset : Float32

  # Represents an SVG resource.
  #
  # - *src* specifies how to obtain it.
  # - *color* controls the `currentColor` property of the SVG.
  # - *resize* sets the resize mode (see `soma.dwuir.node.svg.resize` for more info).
  record SvgShape,
    src : Term,
    color : Color,
    resize : Resize

  struct SvgShape
    enum Resize
      Clip
      Stretch
      KeepRatio
    end

    # See `Picture#each_line_to_display`.
    def each_line_to_display(&fn : Int32, String ->) : Nil
      fn.call(0, "svg(src=#{ML.compact(src)})")
    end
  end

  abstract class DrawCommand
    DMG_RING_THICKNESS = 1.0f32

    # Returns the value that draw commands must be ordered (e.g. sorted) by.
    abstract def ord

    # Returns the bounding box of this command after applying its bounds transform.
    abstract def tfbounds : Rect

    # Returns the damage bounding box for this command.
    #
    # It is essentially a box that is slightly larger than `tfbounds`, to accomodate
    # for painting errors/imprecisions.
    def dmgbounds : Rect
      tfbounds.pad(-DMG_RING_THICKNESS).ceil
    end
  end

  # Provides an exhaustive context so as to how a `Shape` should actually be
  # drawn: how it should be clipped, what transformations should be applied to
  # it, etc.
  #
  # - *views* is the stack of view quads (view rects post-transform) that
  #   the shape must be visible in.
  # - *bounds* is the untransformed bounding box of *shape*.
  # - *bounds tf* is the transformation that should be applied to *bounds*; and also
  #   to *shape* when drawing.
  # - *layer* is the z-index of this command. Commands are sorted by *layer*; smaller
  #   layer is drawn before larger layer.
  # - *rank* is similar to *layer* in purpose. It is used to sort things such as cursor
  #   or selection rectangles which are otherwise members of the same *layer*.
  # - *shape* specifies the `Shape` itself.
  defcase DrawShape < DrawCommand,
    views : Slice(Quad),
    bounds : Rect,
    bounds_tf : Tf,
    layer : LayerRank,
    rank : Rank,
    shape : Shape

  class DrawShape
    # Extracts a `DrawKey` from this draw command.
    def key : DrawKey
      DrawKey.new(bounds.size.round, bounds_tf, shape)
    end

    def ord
      {layer, rank}
    end

    def tfbounds : Rect
      bounds_tf.map(bounds)
    end

    # See `Picture#each_line_to_display`.
    def each_line_to_display(&fn : Int32, String ->) : Nil
      fn.call(0, "shape(bounds=#{bounds.xywh}, layer=#{layer}, rank=#{rank})")

      shape.each_line_to_display do |nesting, line|
        fn.call(nesting + 2, line)
      end
    end
  end

  # Instructs the compositor to draw a *composite shape*.
  #
  # Composite shapes are basically nested *picture*s, with an optional *opacity*;
  # they also act as the strictest and most isolated kind of z-index scope.
  #
  # Composite shapes are useful when you want to isolate z-indices completely
  # from the outside world; or have a group of commands be rendered with
  # the same opacity; or both. With opacity, more specifically, composites
  # prevent their member commands from blending with each other when the group
  # as a whole is blended.
  #
  # Composites have a certain memory & runtime overhead right now: not only
  # is the content cached, but also the composite itself, as a whole, is cached.
  # This may or may not be beneficial or desired.
  defcase DrawComposite < DrawCommand,
    picture : Picture,
    opacity : Float32,
    layer : LayerRank

  class DrawComposite
    def ord
      {layer, Rank::Mid}
    end

    def tfbounds : Rect
      picture.tfbounds
    end

    # See `Picture#each_line_to_display`.
    def each_line_to_display(&fn : Int32, String ->)
      fn.call(0, "composite(opacity=#{opacity}, layer=#{layer})")

      picture.each_line_to_display do |indent, line|
        fn.call(indent + 2, line)
      end
    end
  end

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
  record DrawKey, extent : Point, tf : Tf, shape : Shape
end
