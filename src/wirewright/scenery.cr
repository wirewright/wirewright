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

  # An immutable representation of a compiled Scenery scene.
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
    def cache_set : CacheSet
      Scenery.cache_set
    end

    # Compiles a *document* into a scene.
    #
    # - *cache* is the cache set to use (see `cache_set`).
    # - *width* is the width to use for layout at the top-level (e.g., screen or
    #   window width).
    # - *height* is the height to use for layout at the top-level (e.g., screen or
    #   window height).
    # - *blocking* determines whether to wait (`true`) or poll (`false`) for assets.
    #   If `true`, this function blocks the calling fiber until all assets are loaded.
    #   If `false`, this function schedules the loading of *document*'s assets and
    #   polls once. Assets that are already loaded will be available to the scene.
    #   Nodes that depend on assets that aren't available will display a placeholder
    #   or something similar (see e.g. `scenery.suspense`). You usually set `blocking: true`
    #   for one-shot renders, and `blocking: false` for interactive graphics, calling
    #   `scene` on every frame.
    #
    # The returned scene is immutable. Therefore, it is safe to pass it around
    # between different threads.
    def scene(cache : CacheSet, document : Term, width : Magnitude, height : Magnitude, *, blocking : Bool = true) : Outcome::Accepted(Scene)
      @@lock.synchronize do
        query_tree = Scenery.recognize(cache, document: document)
        asset_queries = Scenery.queries(cache, query_tree)

        if blocking
          # TODO: Make it possible to release the lock while waiting for assets...
          asset_reply = Scenery.wait(cache, asset_queries)
        else
          asset_reply = Scenery.poll(cache, asset_queries)
        end

        asset_reply.map do |assets|
          asset_resn = Scenery.resolve(cache, assets, query_tree)
          shaped_tree = Scenery.shape(cache, asset_resn)
          sized_tree, size_tree = Scenery.size(cache, shaped_tree, Cst.new(0, width, 0, height))
          box_tree = Scenery.box(cache, sized_tree, size_tree)
          elevated_tree, elevated_box = Scenery.elevate(cache, sized_tree, box_tree)
          aimed_tree = Scenery.aim(cache, elevated_tree, elevated_box)
          vbox = Scenery.vbox(cache, aimed_tree, elevated_box)

          Scene.new(width, height, aimed_tree, elevated_box, vbox)
        end
      end
    end

    # Converts a compiled *scene* to a tree of draw commands. The draw commands can
    # in turn be rasterized.
    #
    # *cache* is the cache set to use (see `cache_set`).
    def depict(cache : CacheSet, scene : Scene) : DrawCommand
      @@lock.synchronize do
        command = Scenery.depict(cache, scene.root, scene.box)
        Scenery.prune(command, Rect[0, 0, scene.width, scene.height])
      end
    end

    # Given a pair of commands, returns a slice of dirty rectangles in root-space.
    def diff(command0 : DrawCommand, command1 : DrawCommand) : Slice(Rect)
      @@lock.synchronize do
        Scenery.diff(command0, command1)
      end
    end

    # The maximum screen width (see `screen`).
    MAX_SCREEN_WIDTH = 16_000

    # The maximum screen height (see `screen`).
    MAX_SCREEN_HEIGHT = 16_000

    # Constructs a *screen*, which is basically a `PixelRect`.
    #
    # - *width* is the width of the screen, clamped between 0 and `MAX_SCREEN_WIDTH`.
    # - *height* is the height of the screen, clamped between 0 and `MAX_SCREEN_HEIGHT`.
    # - *backdrop* is the clear color of the screen. See `PixelRect` for more info
    #   on what colors you are recommended to use for *backdrop*.
    def screen(width : Magnitude, height : Magnitude, backdrop : Pigment::RGBA) : PixelRect
      iwidth = width.to_i.clamp(0..MAX_SCREEN_WIDTH)
      iheight = height.to_i.clamp(0..MAX_SCREEN_HEIGHT)

      pixels = Slice(Pixel).new(iwidth * iheight, Pixel.of(backdrop))

      PixelRect.new(pixels, iwidth, iheight, backdrop, clear: true)
    end

    # Writes the raster image for *command* to *screen*.
    #
    # See `.rasterize(PixelRect, DrawCommand, Slice(Rect))` for more info.
    def rasterize(screen : PixelRect, command : DrawCommand, dirty_rects : Slice(Rect)) : Nil
      @@lock.synchronize do
        Scenery.rasterize(screen, command, dirty_rects)
      end
    end

    # Writes the raster image for *command* to *screen*.
    #
    # Uses PlutoVG and PlutoSVG to rasterize *command*.
    #
    # EXPERIMENTAL: *dirty_rects* support is experimental. It can improve performance
    # substantially, but I don't know whether what we do right now actually covers all
    # cases; my suspicion is that we may be filtering out commands that we shouldn't.
    def rasterize(screen : PixelRect, command : DrawCommand) : Nil
      @@lock.synchronize do
        Scenery.rasterize(screen, command)
      end
    end

    # Rasterizes a compiled *scene* to a `PixelRect`.
    #
    # The width and height of the resulting pixel rect are the width and height
    # of the scene (see `scene`).
    #
    # *cache* is the cache set to use (see `cache_set`).
    #
    # *backdrop* is the clear color for the resulting pixel rect. See `PixelRect` for more info
    # on what colors you are recommended to use for *backdrop*.
    #
    # NOTE: If you've already constructed a `screen`, do not use this function;
    # use `rasterize(PixelRect, DrawCommand)` instead. This function is meant for
    # one-shot use.
    def rasterize(cache : CacheSet, scene : Scene, *, backdrop : Pigment::RGBA) : PixelRect
      screen = screen(scene.width, scene.height, backdrop)
      command = depict(cache, scene)
      rasterize(screen, command)
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
    # write_result = PathService.write(Path["/tmp/hello.out.ppm"], picture.unwrap.to_ppm)
    # write_result.wait # => PathService::Present
    # ```
    #
    # NOTE: This function is intended for one-shot usage. If you are doing interactive
    # graphics, you almost certainly want to have a dedicated `scene` -> `depict` -> `diff` ->
    # `rasterize` (into an existing screen) pipeline, with its own cache, for performance.
    def rasterize(document : Term, width : Magnitude, height : Magnitude, backdrop : Pigment::RGBA) : Outcome::Accepted(PixelRect)
      cache = cache_set

      scene(cache, document, width, height).map do |scene|
        rasterize(cache, scene, backdrop: backdrop)
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
require "./scenery/cache"
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
require "./scenery/prune"
require "./scenery/pixel"
require "./scenery/pixel_rect"
require "./scenery/rasterize"
require "./scenery/hit"
require "./scenery/describe"
