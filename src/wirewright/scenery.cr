# Scenery handles interactive graphics for Wirewright.
#
# Scenery is the successor to UIR and DwUIR. Imagine Scenery as a combination of
# a layout engine and a vector graphics engine.
#
# Among the most major dependencies of Scenery are PlutoVG and PlutoSVG;
# they do the majority of vector graphics work. Additionally, Scenery uses
# Raqm for text shaping, and Unibreak for finding line breaks.
#
# NOTE: Scenery is **not** thread-safe, nor do we assume its dependencies are
# thread-safe. None of the objects under `Scenery` are thread-safe either
# (barring "accidental" thread-safety). You must use Scenery **only** through
# `Safe`, which centralizes all access to Scenery with a global lock, and must
# be the sole caller of functions defined under `Scenery` for stuff to work properly.
#
# Front-facing functions can be found in the module `Safe`. Functions directly under
# `Scenery` are public for documentation purposes only. Most public objects under
# the namespace `Scenery` are public for the same reason; you almost never
# interact with them directly.
module Ww::Scenery
  extend self

  # Contains caches used by Scenery.
  class CacheSet
    # :nodoc:
    getter recognition
    # :nodoc:
    getter query
    # :nodoc:
    getter resolution
    # :nodoc:
    getter assets
    # :nodoc:
    getter shaping
    # :nodoc:
    getter shaped_items
    # :nodoc:
    getter min_size
    # :nodoc:
    getter measurement
    # :nodoc:
    getter boxes
    # :nodoc:
    getter elevate
    # :nodoc:
    getter aim
    # :nodoc:
    getter vbox
    # :nodoc:
    getter depict

    # :nodoc:
    def initialize
      @recognition = GenerationalCache(Term, RecognizedNode).new
      @query = GenerationalCache(RecognizedNode, QuerySet).new
      @resolution = GenerationalCache({Asset::Map, RecognizedNode}, Resn::Any).new
      @assets = GenerationalCache({Asset::Query, Term::Blob}, Outcome::Accepted(Asset::Any?)).new
      @shaping = GenerationalCache(AssetNode, ShapedNode).new
      @shaped_items = GenerationalCache(ShapeInput, Slice(ShapedSemiStyledGlyph)).new
      @min_size = GenerationalCache(ShapedNode, Point).new
      @measurement = GenerationalCache({ShapedNode, Cst}, {SizedNode, Size}).new
      @boxes = GenerationalCache({SizedNode, Size}, OriginBox).new
      @elevate = GenerationalCache({SizedNode, OriginBox}, ElevateResponse).new
      @aim = GenerationalCache({ElevatedNode, OriginBox}, AimResponse).new
      @vbox = GenerationalCache({AimedNode, OriginBox}, VBox).new
      @depict = GenerationalCache({AimedNode, OriginBox}, DrawCommand).new
    end
  end

  # An immutable representation of a Scenery scene at a particular instant.
  #
  # It is safe to pass scenes around between different threads.
  class Scene
    # :nodoc:
    getter width : Magnitude
    # :nodoc:
    getter height : Magnitude
    # :nodoc:
    getter root : Root(AimedNode)
    # :nodoc:
    getter box : OriginBox
    # :nodoc:
    getter vbox : VBox

    # :nodoc:
    def initialize(@width, @height, @root, @box, @vbox)
    end
  end

  # Acts as a source of Scenery `Scene`s: represents the potential for a "time
  # sequence" of `Scene`s.
  #
  # Scene sources are immutable. It is safe to pass them around between different threads.
  class SceneSource
    # :nodoc:
    getter width : Magnitude

    # :nodoc:
    getter height : Magnitude

    # :nodoc:
    getter recognized_root : Root(RecognizedNode)

    # :nodoc:
    getter pending : Slice(Asset::Pending)

    # :nodoc:
    def initialize(@width, @height, @recognized_root, @pending)
    end

    # Scenes are compared by width, height, and content.
    def_equals_and_hash width, height, recognized_root
  end

  # Front-facing, thread-safe API for `Scenery`.
  module Safe
    extend self

    @@lock = Sync::Mutex.new

    # Constructs a cache set for use by Scenery. This is the closest thing Scenery
    # has to *state* or *retention*, as in *retained mode graphics*. Otherwise,
    # Scenery is fairly *immediate*, at least to the extent that I understand
    # the word.
    #
    # The returned cache set itself is **not** thread-safe. However, note several
    # things. First, you cannot mutate `CacheSet` from the outside; second, `Safe`
    # uses a global lock; third, only `Scenery` can make use of a `CacheSet`. Thus,
    # as long as you stick to passing the cache set to `Safe`, it is safe to use
    # the same cache set from different fibers-- since at any point, either `CacheSet`
    # is not being mutated, or it is being mutated by the single fiber that currently
    # has access to `Scenery` due to `Safe`.
    def cache : CacheSet
      CacheSet.new
    end

    # Compiles a *document* into a scene.
    #
    # - *cache* is the cache set to use (see `cache_set`).
    # - *width* is the width to use for layout at the top-level (e.g., screen or
    #   window width).
    # - *height* is the height to use for layout at the top-level (e.g., screen or
    #   window height).
    def scenesrc(cache : CacheSet, document : Term, width : Magnitude, height : Magnitude) : SceneSource
      @@lock.synchronize do
        recognized_root = Scenery.recognize(cache, document: document)
        queries = Scenery.queries(cache, recognized_root)
        pending = Scenery.schedule(queries)

        SceneSource.new(width, height, recognized_root, pending)
      end
    end

    # Polls assets and returns the resulting `Scene`. Pending assets usually show as
    # a placeholder of some sort.
    def poll(cache : CacheSet, scenesrc : SceneSource) : Outcome::Accepted(Scene)
      @@lock.synchronize do
        Scenery.poll(cache, scenesrc.pending).map do |assets|
          asset_resn = Scenery.resolve(cache, assets, scenesrc.recognized_root)
          shaped_root = Scenery.shape(cache, asset_resn)
          sized_root, size_root = Scenery.size(cache, shaped_root, Cst.new(0, scenesrc.width, 0, scenesrc.height))
          box_root = Scenery.box(cache, sized_root, size_root)
          elevated_root, elevated_box = Scenery.elevate(cache, sized_root, box_root)
          aimed_root = Scenery.aim(cache, elevated_root, elevated_box)
          vbox = Scenery.vbox(cache, aimed_root, elevated_box)

          Scene.new(scenesrc.width, scenesrc.height, aimed_root, elevated_box, vbox)
        end
      end
    end

    # Waits for all assets to load (or fail) and returns the resulting `Scene`.
    def wait(cache : CacheSet, scenesrc : SceneSource) : Outcome::Accepted(Scene)
      scenesrc.pending.each(&.promise.wait)

      poll(cache, scenesrc)
    end

    # A shorthand for constructing a scene source (see `scenesrc`) and waiting for
    # all assets to load.
    def scene(cache : CacheSet, document : Term, width : Magnitude, height : Magnitude) : Outcome::Accepted(Scene)
      wait(cache, scenesrc: scenesrc(cache, document, width, height))
    end

    # Converts a compiled *scene* to a tree of draw commands. The draw commands can
    # in turn be rasterized.
    #
    # *cache* is the cache set to use (see `cache_set`).
    def depict(cache : CacheSet, scene : Scene) : DrawCommand
      @@lock.synchronize do
        Scenery.depict(cache, scene.root, scene.box)
      end
    end

    # Given a pair of commands, returns a slice of dirty rectangles in root-space.
    def diff(command0 : DrawCommand, command1 : DrawCommand) : Slice(Rect)
      @@lock.synchronize do
        Scenery.diff(command0, command1)
      end
    end

    # Constructs a *screen*, which is basically a `PixelRect`.
    #
    # *width* and *height* are turned into PixelRect-safe sizes using `PixelRect.clamp`.
    def screen(width : Magnitude, height : Magnitude) : PixelRect
      iwidth, iheight = PixelRect.clamp(width, height)
      stride = iwidth * 4
      pixels = Slice(Pixel).new(iwidth * iheight, Pixel.of(Pigment.black))

      PixelRect.new(pixels.to_unsafe.as(UInt8*), iwidth, iheight, stride)
    end

    # Writes the raster image for *command* to *screen*.
    #
    # See `.rasterize(PixelRect, DrawCommand, Slice(Rect))` for more info.
    #
    # EXPERIMENTAL: *dirty_rects* support is experimental. It can improve performance
    # substantially, but I don't know whether what we do right now actually covers all
    # cases; my suspicion is that we may be filtering out commands that we shouldn't.
    def rasterize(screen : PixelRect, command : DrawCommand, backdrop : Pigment::RGBA, dirty_rects : Slice(Rect)) : Nil
      @@lock.synchronize do
        Scenery.rasterize(screen, command, backdrop, dirty_rects)
      end
    end

    # Writes the raster image for *command* to *screen*.
    #
    # Uses PlutoVG and PlutoSVG to rasterize *command*.
    #
    # *backdrop* is the clear color for the resulting pixel rect. It is highly
    # advised to have it be fully opaque. Having a transparent backdrop may harm
    # anti-aliasing.
    def rasterize(screen : PixelRect, command : DrawCommand, backdrop : Pigment::RGBA) : Nil
      @@lock.synchronize do
        Scenery.rasterize(screen, command, backdrop)
      end
    end

    # Rasterizes a compiled *scene* to a `PixelRect`.
    #
    # The width and height of the resulting pixel rect are the width and height
    # of the scene (see `scene`).
    #
    # *cache* is the cache set to use (see `cache_set`).
    #
    # NOTE: If you've already constructed a `screen`, do not use this function;
    # use `rasterize(PixelRect, DrawCommand, Pigment::RGBA)` instead. This function is meant for
    # one-shot use.
    def rasterize(cache : CacheSet, scene : Scene, backdrop : Pigment::RGBA) : PixelRect
      screen = screen(scene.width, scene.height)
      command = depict(cache, scene)
      rasterize(screen, command, backdrop)
      screen
    end

    # Rasterizes *document* at the given *width*, *height*, using *backdrop* as
    # the clear color.
    #
    # This is one of the most high-level functions in the Scenery API. In just one
    # function call, you exercise the entirety of Scenery.
    #
    # See `scene`, `depict`, `rasterize` for more info on the parameters.
    #
    # ```
    # document = ML.document(<<-'WWML')
    # (y-stack gap: 15
    #   (text
    #     caption: "1. atala"
    #     font: "Noto Sans"
    #     color: black
    #     size: 24)
    #   (text
    #     caption:
    #       "Gizon-emakume guztiak aske jaiotzen dira, duintasun eta eskubide berberak \
    #       dituztela; eta ezaguera eta kontzientzia dutenez gero, elkarren artean senide \
    #       legez jokatu beharra dute."
    #     color: (oklch 0.3 0 0)
    #     font: "Noto Sans"
    #     size: 18))
    # WWML
    #
    # picture = Scenery::Safe.rasterize(document, 450, 900, Pigment.white)
    # write_result = PathService.write(NormalPath["/tmp/hello.out.ppm"], picture.unwrap.to_ppm)
    # write_result.wait # => PathService::Present
    # ```
    #
    # NOTE: This function is intended for one-shot usage. If you are doing interactive
    # graphics, you almost certainly want to have a dedicated `scene` -> `depict` -> `diff` ->
    # `rasterize` (into an existing screen) pipeline, with its own cache, for performance.
    def rasterize(document : Term, width : Magnitude, height : Magnitude, backdrop : Pigment::RGBA) : Outcome::Accepted(PixelRect)
      cache = cache_set

      scene(cache, document, width, height).map do |scene|
        rasterize(cache, scene, backdrop)
      end
    end

    # A shorthand for `describe` with no hit queries.
    def describe(scene : Scene) : {Term, Slice(Term)}
      describe(scene, Slice(HitQuery).empty)
    end

    # Describes *scene* after hitting it (see `hit`) with the union of *queries*.
    # Returns a description of *scene* followed by the observers in it (if any).
    #
    # Each observer acts as a kind of "point of view" on the scene, letting
    # one see the scene, symbolically, from different locations.
    def describe(scene : Scene, queries : Enumerable(HitQuery)) : {Term, Slice(Term)}
      @@lock.synchronize do
        hit = HitEmpty.new
        queries.each do |query|
          hit = Scenery.union(hit, Scenery.hit(scene, query))
        end

        Scenery.describe(scene, hit)
      end
    end
  end
end

require "./scenery/point"
require "./scenery/rect"
require "./scenery/rounded_rect"
require "./scenery/tf"
require "./scenery/unit"
require "./scenery/paint"
require "./scenery/node"
require "./scenery/asset"
require "./scenery/recognize"
require "./scenery/resolve"
require "./scenery/shape"
require "./scenery/size"
require "./scenery/box"
require "./scenery/elevate"
require "./scenery/aim"
require "./scenery/vbox"
require "./scenery/depict"
require "./scenery/pixel"
require "./scenery/pixel_rect"
require "./scenery/rasterize"
require "./scenery/hit"
require "./scenery/describe"
