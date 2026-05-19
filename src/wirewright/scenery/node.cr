module Ww::Scenery
  # A qualifier to tag a node such as a `RecognizedNode` or an `ElevatedNode`
  # as a *root* node.
  defrecord Root(Node), node : Node

  # `RecognizedNode`s is obtained by calling `recognize` on a `Term`. Queries
  # are not yet resolved: we don't know whether & which images exist, we haven't
  # fetched fonts yet but just have the specifications for them, etc.
  alias RecognizedNode = Inert |
                         RectShape |
                         Img(Asset::ImageQuery) |
                         Svg(Asset::SvgQuery) |
                         Text(Asset::FontQuery) |
                         Icon |
                         Content(RecognizedNode) |
                         Floating(RecognizedNode) |
                         Limit(RecognizedNode) |
                         Clamp(RecognizedNode) |
                         Padding(RecognizedNode) |
                         Align(RecognizedNode) |
                         XYStack(RecognizedNode) |
                         ZStack(RecognizedNode) |
                         XYWrap(RecognizedNode) |
                         Composite(RecognizedNode) |
                         Transform(RecognizedNode) |
                         Viewport(RecognizedNode) |
                         Aim(RecognizedNode) |
                         Page(RecognizedNode) |
                         Overlay(RecognizedNode) |
                         Suspense(RecognizedNode) |
                         Variant(RecognizedNode) |
                         Vantage(RecognizedNode) |
                         Gate(RecognizedNode)

  # `AssetNode`s know about *assets*; the queries from `RecognizedNode` having been
  # resolved, each asset-dependent node now either has a valid asset attached to it
  # (downloaded from the network, or from the disk, etc.); or it is replaced
  # by `Pending`. `Suspense` nodes are subtracted also.
  alias AssetNode = Inert |
                    RectShape |
                    Pending |
                    Img(Asset::PvgRasterImage) |
                    Svg(Asset::PvgSvgImage) |
                    IconGlyph |
                    Text(Asset::PvgFont) |
                    Content(AssetNode) |
                    Floating(AssetNode) |
                    Limit(AssetNode) |
                    Clamp(AssetNode) |
                    Padding(AssetNode) |
                    Align(AssetNode) |
                    XYStack(AssetNode) |
                    ZStack(AssetNode) |
                    XYWrap(AssetNode) |
                    Composite(AssetNode) |
                    Transform(AssetNode) |
                    Viewport(AssetNode) |
                    Aim(AssetNode) |
                    Page(AssetNode) |
                    Overlay(AssetNode) |
                    Variant(AssetNode) |
                    Vantage(AssetNode) |
                    Gate(AssetNode)

  # `ShapedNode`s are `AssetNode`s after shaping (`shape`), which is, very roughly
  # speaking, *text layout*.
  #
  # During shaping, text nodes turn into "clouds" of glyphs properly positioned with
  # respect to each other: `Text` nodes turn into `ShapedText` nodes. After shaping,
  # we can't speak of "text" anymore; instead, we speak of "clouds" of glyphs *corresponding*
  # to different pieces of the original text.
  alias ShapedNode = Inert |
                     RectShape |
                     Pending |
                     Img(Asset::PvgRasterImage) |
                     Svg(Asset::PvgSvgImage) |
                     IconGlyph |
                     ShapedText |
                     Content(ShapedNode) |
                     Floating(ShapedNode) |
                     Limit(ShapedNode) |
                     Clamp(ShapedNode) |
                     Padding(ShapedNode) |
                     Align(ShapedNode) |
                     XYStack(ShapedNode) |
                     ZStack(ShapedNode) |
                     XYWrap(ShapedNode) |
                     Composite(ShapedNode) |
                     Transform(ShapedNode) |
                     Viewport(ShapedNode) |
                     Aim(ShapedNode) |
                     Page(ShapedNode) |
                     Overlay(ShapedNode) |
                     Variant(ShapedNode) |
                     Vantage(ShapedNode) |
                     Gate(ShapedNode)

  # `SizedNode`s are `ShapedNode`s after *sizing* (`size`). Things like `XYWrap` are
  # subtracted; `Transform`s become `TransformMatrix` nodes; and so on. SizedNodes
  # are emitted along the `Size` tree, which stores the actual sizes.
  alias SizedNode = Inert |
                    RectShape |
                    Pending |
                    Img(Asset::PvgRasterImage) |
                    Svg(Asset::PvgSvgImage) |
                    IconGlyph |
                    ShapedText |
                    Padding(SizedNode) |
                    Align(SizedNode) |
                    XYStack(SizedNode) |
                    ZStack(SizedNode) |
                    Composite(SizedNode) |
                    TransformMatrix(SizedNode) |
                    Viewport(SizedNode) |
                    Aim(SizedNode) |
                    Page(SizedNode) |
                    Overlay(SizedNode) |
                    Vantage(SizedNode) |
                    Gate(SizedNode)

  # `ElevatedNode`s are `SizedNode`s after *elevation* (`elevate`). During elevation,
  # `Overlay` nodes "bubble up" to enclosing `Page` nodes, and `Page` nodes, once
  # they know all their `Overlay`s, become simply `ZStack`s. Thus, importantly,
  # both `Overlay` and `Page` are subtracted from the type.
  alias ElevatedNode = Inert |
                       RectShape |
                       Pending |
                       Img(Asset::PvgRasterImage) |
                       Svg(Asset::PvgSvgImage) |
                       IconGlyph |
                       ShapedText |
                       Padding(ElevatedNode) |
                       Align(ElevatedNode) |
                       XYStack(ElevatedNode) |
                       ZStack(ElevatedNode) |
                       Composite(ElevatedNode) |
                       TransformMatrix(ElevatedNode) |
                       Viewport(ElevatedNode) |
                       Aim(ElevatedNode) |
                       Vantage(ElevatedNode) |
                       Gate(ElevatedNode)

  # `AimedNode`s are `ElevatedNode`s after *aiming* (`aim`). During aiming, viewport
  # nodes, which are at this point more like "cameras", find `Aim`ed nodes in their
  # immediate subtree, and configure themselves to aim at those nodes (if found some).
  # As a result, `Aim`ed nodes are replaced by simply `ZStack`, and thus subtracted,
  # whereas `Viewport`s become simply `Clip`s.
  #
  # `AimedNode`s are currently the lowest-level tree representation. You can `describe`
  # it ("rendering" it to symbolic structure) and `depict` it (rendering it to
  # a `PixelRect`); which are the two major output modes of `Scenery`.
  alias AimedNode = Inert |
                    RectShape |
                    Pending |
                    Img(Asset::PvgRasterImage) |
                    Svg(Asset::PvgSvgImage) |
                    IconGlyph |
                    ShapedText |
                    Padding(AimedNode) |
                    Align(AimedNode) |
                    XYStack(AimedNode) |
                    ZStack(AimedNode) |
                    Composite(AimedNode) |
                    TransformMatrix(AimedNode) |
                    Clip(AimedNode) |
                    Vantage(AimedNode) |
                    Gate(AimedNode)

  # Used for unrecognized nodes and tombstones.
  class Inert
    @@the : Inert?

    def self.new
      @@the ||= super
    end
  end

  defcase RectShape,
    thickness : RectThickness,
    radii : RectRadii,
    fill : Paint::Any,
    stroke : Paint::Any,
    min_size : Point,
    caches_hash: true

  class RectShape
    def resolve(bounds : Rect)
      resolved_l = @thickness.l.resolve(bounds.w)
      resolved_r = @thickness.r.resolve(bounds.w)
      resolved_t = @thickness.t.resolve(bounds.h)
      resolved_b = @thickness.b.resolve(bounds.h)

      resolved_x = resolved_l + resolved_r
      resolved_y = resolved_t + resolved_b

      w_factor = resolved_x > bounds.w ? (bounds.w / resolved_x) : Magnitude.new(1)
      h_factor = resolved_y > bounds.h ? (bounds.h / resolved_y) : Magnitude.new(1)

      thickness = {
        l: Math.max(0, resolved_l * w_factor),
        r: Math.max(0, resolved_r * w_factor),
        t: Math.max(0, resolved_t * h_factor),
        b: Math.max(0, resolved_b * h_factor),
      }

      {thickness, RoundedRect.new(bounds, @radii)}
    end
  end

  defrecord RectThickness, l : Unit, r : Unit, t : Unit, b : Unit
  defrecord RectRadii, tl : Unit, tr : Unit, bl : Unit, br : Unit

  defcase Pending,
    blame : Text(Asset::FontQuery) |
            Icon |
            Img(Asset::ImageQuery) |
            Svg(Asset::SvgQuery)

  defcase Img(C),
    src : C,
    radii : RectRadii,
    fit : Fit::Any,
    tile : Bool,
    opacity : Magnitude,
    resize_w : Unit,
    resize_h : Unit,
    placeholder : RectShape?,
    caches_hash: true

  module Img::Fit
    alias Any = Pan | Align | Stretch

    defrecord Pan, delta : Point
    defrecord Align, normpt : Point
    defrecord Stretch
  end

  defcase Svg(C),
    src : C,
    color : Pigment::RGBA,
    fit : Fit,
    placeholder : RectShape?,
    caches_hash: true

  enum Svg::Fit
    Clip
    Stretch
    KeepRatio
  end

  alias Decoration = Selection | Underline

  defcase Underline,
    anchor : Int32,
    focus : Int32,
    fill : Pigment::RGBA?,
    offset : Magnitude,
    thickness : Magnitude

  class Underline
    def range : Range(Int32, Int32)
      Math.min(anchor, focus)...Math.max(anchor, focus)
    end
  end

  defcase Selection,
    anchor : Int32,
    focus : Int32,
    color : Pigment::RGBA,
    fill : Pigment::RGBA,
    radius : Magnitude,
    thickness : Magnitude,
    extents : Rect,
    aim : Bool,
    clearance : Magnitude,
    endl : Bool,
    endl_width : Magnitude

  class Selection
    def range : Range(Int32, Int32)
      Math.min(anchor, focus)...Math.max(anchor, focus)
    end
  end

  # NOTE: At this point, *selections* and *underlines* are already non-overlapping.
  defcase Text(Font),
    caption : Pf::GraphemeSeln,
    unibreaks : Slice(Unibreak::LineBreak),
    font_stack : Slice(Font),
    size : Magnitude,
    color : Pigment::RGBA,
    leading : Unit,
    tracking : Unit?,
    selections : Slice(Selection),
    underlines : Slice(Underline),
    placeholder : RectShape?,
    caches_hash: true

  class Text
    # :nodoc:
    struct DecorationEE
      include Enumerable(Decoration)

      def initialize(@selections : Slice(Selection), @underlines : Slice(Underline))
      end

      def each(& : Decoration ->)
        @selections.each { |selection| yield selection }
        @underlines.each { |underline| yield underline }
      end
    end

    def decorations : Enumerable(Decoration)
      DecorationEE.new(selections, underlines)
    end
  end

  # Represents the relevant measurements of a font at a particular size.
  struct FontMetrics
    # Ascent is above baseline, more positive as it grows.
    getter ascent : Magnitude

    # Descent is below baseline, more negative as it grows.
    getter descent : Magnitude

    getter line_gap : Magnitude

    # :nodoc:
    def initialize(@ascent, @descent, @line_gap)
    end

    def self.zero : FontMetrics
      new(ascent: Magnitude.new(0), descent: Magnitude.new(0), line_gap: Magnitude.new(0))
    end

    def self.max(a : FontMetrics, b : FontMetrics) : FontMetrics
      new(
        ascent: Math.max(a.ascent, b.ascent),
        descent: Math.min(a.descent, b.descent),
        line_gap: Math.max(a.line_gap, b.line_gap),
      )
    end

    # Calculates the line height according to these measurements.
    def line_height : Magnitude
      ascent - descent + line_gap
    end
  end

  defcase ShapedText,
    caption : Pf::GraphemeSeln,
    seq : Slice(ShapedItem),
    metrics : FontMetrics,
    line_height : Magnitude,
    selections : Slice(Selection),
    caches_hash: true

  alias ShapedItem = ShapedStyledGlyph | Endl | IBeam

  # NOTE: This struct is rather large, and is intended for storage in groups
  # (e.g. `Slice`) rather than standalone.
  #
  # NOTE: *cluster* is absolute, meaning it refers into the full text
  # string (`Text#caption`) as opposed to, say, `ShapedParagraph#text`.
  defrecord ShapedStyledGlyph,
    font : Asset::PvgFont,
    grapheme_index : Int32,
    glyph_index : Int32,
    break_policy : BreakPolicy,
    advance : Point,
    offset : Point,
    size : Magnitude,
    measurement : GlyphMeasurement,
    color : Pigment::RGBA,
    decorations : Slice(GlyphDecoration)

  # Represents the relevant measurements of a glyph.
  defrecord GlyphMeasurement, advance : Magnitude, extents : Rect

  defrecord Endl,
    grapheme_index : Int32,
    decorations : Slice(GlyphDecoration)

  defrecord IBeam, selection : Selection

  enum BreakPolicy
    NoBreak
    Collapse
    Preserve
  end

  # NOTE: Lines may end with an `Endl`. We include it because it may contain decorations.
  # Clients are free to ignore it, though.
  defrecord ShapedLine, items : Slice(ShapedItem), advance : Magnitude

  struct ShapedLine
    def grapheme_range? : Range(Int32, Int32)?
      min = nil
      max = nil

      items.each do |item|
        next unless item.is_a?(ShapedStyledGlyph)

        min = min ? Math.min(min, item.grapheme_index) : item.grapheme_index
        max = max ? Math.max(max, item.grapheme_index) : item.grapheme_index
      end

      if min.nil? || max.nil?
        assert min.nil? && max.nil?
        return
      end

      min...max + 1
    end
  end

  defcase Icon,
    name : String,
    font : Asset::FontQuery,
    codepoints : Asset::CodepointsQuery,
    size : Magnitude,
    color : Pigment::RGBA,
    placeholder : RectShape?,
    caches_hash: true

  defcase IconGlyph,
    font : Asset::PvgFont,
    codepoint : Char,
    glyph_index : Int32,
    size : Magnitude,
    color : Pigment::RGBA,
    caches_hash: true

  defcase Limit(Node),
    children : Slice(Node),
    min_w : Unit,
    max_w : Unit?,
    min_h : Unit,
    max_h : Unit?,
    caches_hash: true

  defcase Clamp(Node),
    children : Slice(Node),
    min_w : Unit,
    max_w : Unit?,
    min_h : Unit,
    max_h : Unit?,
    caches_hash: true

  defcase Padding(Node),
    children : Slice(Node),
    pl : Magnitude,
    pr : Magnitude,
    pt : Magnitude,
    pb : Magnitude,
    caches_hash: true

  defcase Content(Node),
    children : Slice(Node),
    x : Bool,
    y : Bool,
    caches_hash: true

  defcase Floating(Node),
    children : Slice(Node),
    x : Bool,
    y : Bool,
    caches_hash: true

  defcase Align(Node),
    children : Slice(Node),
    pivot : Point,
    caches_hash: true

  defcase XYStack(Node),
    axis : Axis,
    children : Slice(Node),
    shares : Slice(Share),
    gap : Magnitude,
    caches_hash: true

  enum Axis
    X
    Y

    def inf?(cst : Cst) : Bool
      max(cst) == Magnitude::INFINITY
    end

    def max(cst : Cst)
      case self
      in .x? then cst.max_w
      in .y? then cst.max_h
      end
    end

    def put(m : Magnitude) : Point
      case self
      in .x? then Point[m, 0]
      in .y? then Point[0, m]
      end
    end

    def get(point : Point) : Magnitude
      case self
      in .x? then point.x
      in .y? then point.y
      end
    end

    def select(point : Point) : Point
      case self
      in .x? then Point[point.x, 0]
      in .y? then Point[0, point.y]
      end
    end

    def select(cst : Cst) : Cst
      case self
      in .x? then Cst.new(cst.min_w, cst.max_w, 0, Magnitude::INFINITY)
      in .y? then Cst.new(0, Magnitude::INFINITY, cst.min_h, cst.max_h)
      end
    end

    def cross
      case self
      in .x? then Y
      in .y? then X
      end
    end
  end

  alias Share = FrShare | ContentShare

  defrecord FrShare, num : Magnitude
  defrecord ContentShare

  defcase ZStack(Node),
    children : Slice(Node),
    info : ZInfo?

  class ZStack(Node)
    # Constructs an _anon_ymous z-stack.
    def self.anon(children : Slice(Node)) : ZStack(Node)
      ZStack(Node).new(children, info: nil)
    end
  end

  defrecord ZInfo, name : Term, pairs : Term::Dict

  defcase XYWrap(Node),
    axis : Axis,
    children : Slice(Node),
    item_shares : Slice(Share),
    line_shares : Int32 -> Slice(Share),
    gap : Point,
    caches_hash: true

  defcase Composite(Node),
    children : Slice(Node),
    opacity : Magnitude,
    caches_hash: true

  defcase Transform(Node),
    children : Slice(Node),
    translation : Translation,
    rotation : Rotation,
    scale : Scale,
    caches_hash: true

  defcase TransformMatrix(Node),
    children : Slice(Node),
    tf : Tf,
    caches_hash: true

  defrecord Translation, dl : Unit, dt : Unit, origin : Point
  defrecord Rotation, angle : Magnitude, origin : Point
  defrecord Scale, factor : Magnitude

  defcase Viewport(Node),
    children : Slice(Node),
    aim : ViewportAim,
    page_x : Unit,
    page_y : Unit,
    offset_x : Unit,
    offset_y : Unit,
    radii : RectRadii,
    caches_hash: true

  enum ViewportAim
    Off
    OffThrough
    On
    OnThrough
  end

  defcase Clip(Node),
    children : Slice(Node),
    offset : Point,
    radii : RectRadii,
    caches_hash: true

  defcase Aim(Node),
    children : Slice(Node),
    caches_hash: true

  defcase Page(Node),
    children : Slice(Node),
    caches_hash: true

  defcase Overlay(Node),
    children : Slice(Node),
    caches_hash: true

  defcase Suspense(Node),
    content : Node,
    placeholder : Node,
    caches_hash: true

  defcase Variant(Node),
    cond : Term,
    children : Slice(Node),
    caches_hash: true

  defcase Vantage(Node),
    id : Term,
    status : Status,
    children : Slice(Node),
    caches_hash: true

  enum Vantage::Status
    Inactive
    Active
    ActiveIfHit
  end

  defcase Gate(Node),
    children : Slice(Node),
    caches_hash: true
end
