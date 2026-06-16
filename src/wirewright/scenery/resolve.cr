module Ww::Scenery
  alias QuerySet = Pf::Set(Asset::Query)

  private def query_set : QuerySet
    QuerySet.new
  end

  private def query_set(objects : Enumerable, &) : QuerySet
    QuerySet.transaction do |commit|
      objects.each { |object| yield commit, object }
    end
  end

  private def query_set(*queries : Asset::Query) : QuerySet
    query_set(queries) { |commit, query| commit.add(query) }
  end

  private def queries!(cache, node : Inert | RectShape | Loading) : QuerySet
    query_set()
  end

  private def queries!(cache, node : Img | Svg) : QuerySet
    query_set(node.src)
  end

  private def queries!(cache, node : Text) : QuerySet
    query_set(node.font_stack) { |commit, query| commit.add(query) }
  end

  private def queries!(cache, node : Icon) : QuerySet
    query_set(node.font, node.codepoints)
  end

  private def queries!(cache, node : Content | Floating | Limit | Clamp | Padding | Align | XYStack | ZStack | XYWrap | Composite | Transform | Viewport | Aim | Page | Overlay | Variant | Vantage | Gate) : QuerySet
    query_set(node.children) do |commit, child|
      queries = queries(cache, child)
      queries.each { |query| commit.add(query) }
    end
  end

  private def queries!(cache, node : Suspense) : QuerySet
    queries(cache, node.content) + queries(cache, node.placeholder)
  end

  private def queries(cache, node : RecognizedNode) : QuerySet
    cache.put_if_absent(node) { queries!(cache, node) }
  end

  # Returns the set of queries present in *root* and its subtree.
  def queries(cache : CacheSet, root : Root(RecognizedNode)) : QuerySet
    cache.query.epoch { queries(cache.query, root.node) }
  end

  # :nodoc:
  #
  # Tags an `AssetNode` with its _res_olutio_n_ status.
  module Resn
    alias Any = Ready | Wait

    # *node*, its subtree, or both loaded all assets successfully.
    defrecord Ready, node : AssetNode

    # *node*, its subtree, or both did not load some assets (yet?)
    defrecord Wait, node : AssetNode
  end

  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Inert | RectShape) : Resn::Any
    Resn::Ready.new(node)
  end

  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Img) : Resn::Any
    asset = assets[node.src]?
    unless asset.is_a?(Asset::PvgRasterImage)
      return Resn::Wait.new(Pending.new(node))
    end

    Resn::Ready.new(node.copy_with(src: asset))
  end

  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Svg) : Resn::Any
    asset = assets[node.src]?
    unless asset.is_a?(Asset::PvgSvgImage)
      return Resn::Wait.new(Pending.new(node))
    end

    Resn::Ready.new(node.copy_with(src: asset))
  end

  # TODO: As fonts load, the font stack will be populated & more characters will show
  # (if some were missing). Only if no fonts are available will we mark Text as Pending.
  # I feel this kind of behavior should be user-configurable. Maybe they want to wait
  # until all fonts load...
  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Text) : Resn::Any
    font_asset_stack = node.font_stack.to_compact_readonly_slice do |query|
      next unless asset = assets[query]?

      asset.as?(Asset::Font)
    end

    if font_asset_stack.empty?
      return Resn::Wait.new(Pending.new(node))
    end

    Resn::Ready.new(node.copy_with(font_stack: font_asset_stack))
  end

  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Icon) : Resn::Any
    font = assets[node.font]?
    codepoints = assets[node.codepoints]?

    unless font.is_a?(Asset::Font)
      return Resn::Wait.new(Pending.new(node))
    end

    unless codepoints.is_a?(Asset::CodepointsMap)
      return Resn::Wait.new(Pending.new(node))
    end

    unless codepoint = codepoints[node.name]?
      return Resn::Wait.new(Pending.new(node))
    end

    icon_glyph = IconGlyph.new(
      font: font,
      codepoint: codepoint,
      glyph_index: font.index(codepoint),
      size: Asset::Font.clamp(node.size),
      color: node.color,
    )

    Resn::Ready.new(icon_glyph)
  end

  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Content | Floating | Limit | Clamp | Padding | Align | XYStack | ZStack | XYWrap | Composite | Transform | Viewport | Aim | Page | Overlay | Variant | Vantage | Gate) : Resn::Any
    cls = Resn::Ready

    children = node.children.to_readonly_slice do |child|
      case resn = resolve(cache, assets, child)
      in Resn::Wait then cls = Resn::Wait
      in Resn::Ready
      end

      resn.node.as(AssetNode)
    end

    cls.new(node.copy_with(children: children).as(AssetNode))
  end

  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Suspense) : Resn::Any
    resn = resolve(cache, assets, node.content)
    if resn.is_a?(Resn::Wait)
      return resolve(cache, assets, node.placeholder)
    end

    Resn::Ready.new(resn.node)
  end

  private def resolve!(cache : CacheSet, assets : Asset::Map, node : Loading) : Resn::Any
    Resn::Wait.new(Pending.new(node))
  end

  private def resolve(cache : CacheSet, assets : Asset::Map, node : RecognizedNode) : Resn::Any
    query_set = queries(cache.query, node)

    # Since we don't want the cache key to depend on global assets, but just
    # on the assets used in *node*, we have to explicitly pick them out.
    submap = assets.select { |key, _| key.in?(query_set) }

    cache.resolution.put_if_absent({submap, node}) do
      resolve!(cache, submap, node)
    end
  end

  # Rewrites *root* so that asset queries in it are replaced with the corresponding
  # asset, if any. Returns the resulting `AssetNode`.
  #
  # Nodes with queries that lack the corresponding asset are wrapped with a special
  # `Pending` node, which is a member of `AssetNode`; it is meant to act as a kind
  # of loading indicator, although the exact interpretation relies on how it is
  # processed downstream.
  #
  # `Suspense` nodes are expanded at this point: either their content branch, or
  # their placeholder branch is chosen depending on how the content branch resolves.
  def resolve(cache : CacheSet, assets : Asset::Map, root : Root(RecognizedNode)) : Root(AssetNode)
    cache.query.epoch do
      cache.resolution.epoch do
        resn = resolve(cache, assets, root.node)

        Root(AssetNode).new(resn.node)
      end
    end
  end
end
