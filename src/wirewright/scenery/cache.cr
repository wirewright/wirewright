module Ww::Scenery
  # A cache which maintains two "generations", the *active* and *surviving*
  # generation. In Scenery, they represent the previous frame and the current
  # in-progress frame, correspondingly.
  #
  # As the current frame is constructed, cache entries are "moved" from the previous
  # frame to the current frame. At the end of an `epoch`, we erase the remaining
  # entries for the previous frame, and swap.
  #
  # This means we only cache things that are reused between two frames, and drop
  # all other things (not all other things, though; e.g, assets can be retained
  # for a longer time).
  class GenerationalCache(K, V)
    def initialize
      @active = {} of K => V
      @surviving = {} of K => V
    end

    def put_if_absent(key : K, & : -> V) : V
      if value = @active.delete(key)
        @surviving[key] = value
        return value
      end

      if value = @surviving[key]?
        return value
      end

      @surviving.put_if_absent(key) { yield }
    end

    def epoch(&)
      yield
    ensure
      @active.clear
      @active, @surviving = @surviving, @active
    end
  end

  # The set of caches used by `Scenery`.
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

  # See `Safe.cache_set`.
  def cache_set : CacheSet
    CacheSet.new
  end
end
