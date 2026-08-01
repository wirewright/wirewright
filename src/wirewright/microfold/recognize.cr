module Ww::Microfold
  defrecord StyleThunk,
    features : FeatureSeq?,
    children : Slice(UncuedStyleNode) | Slice(StyleNode)

  alias StyleNode = UncuedStyleNode | CuedStyleNode

  # NOTE: I had to inline defcase because Crystal's def_equals breaks on nilable Slices,
  # making `==` go to `Comparable` instead of rejecting on type mismatch. That's one
  # of the reasons why I don't use modules the way Crystal stdlib uses them!! Modules
  # are for grouping, period, with extremely rare exceptions.

  class UncuedStyleNode
    getter present : Bool
    getter features : Slice(UncuedFeature)?
    getter children : Slice(UncuedStyleNode)

    def initialize(@present, @features, @children)
    end

    def_hash @present, @features, @children

    @_hash : Atomic(UInt64) = Atomic.new(0u64)

    def hash(hasher)
      hash = @_hash.get(:acquire)

      if hash == 0u64
        hash = previous_def(Crystal::Hasher.new).result
        @_hash.set(hash, :release)
      end

      hash.hash(hasher)
    end

    def ==(other : UncuedStyleNode) : Bool
      return false unless hash == other.hash
      return false unless @present == other.present

      if features0 = @features
        return false unless features1 = other.features
        return false unless features0 == features1
      else
        return false unless other.features.nil?
      end

      return false unless @children == other.children

      true
    end
  end

  defrecord CuedStyleNode,
    cue_membrane : Bool,
    features : Slice(CuedFeature)?,
    children : Slice(StyleNode)

  struct Location
    def initialize(@index : Int32, @total : Int32)
    end

    def first? : Bool
      @index.zero?
    end

    def last? : Bool
      @index == @total - 1
    end

    def only? : Bool
      @index.zero? && @total == 1
    end

    def period?(p : Int32) : Bool
      if p.zero?
        # E.g. `@period-0:bg-neutral-500` is the same as not writing anything
        # at all.
        return false
      end

      @index % p == 0
    end
  end

  defrecord EvalContext, location : Location?, pairspart : Term::Dict

  private def eval?(ctx : EvalContext, feature : Utility | Item) : UncuedFeature?
    feature
  end

  private def eval?(ctx : EvalContext, feature : Present | Absent | UpCue | DnCue) : CuedFeature?
    feature
  end

  private def eval?(ctx : EvalContext, feature : Cond) : CuedFeature | UncuedFeature | Nil
    case filter = eval(ctx, feature.filter)
    in true
      eval?(ctx, feature.body)
    in false
    in InCue, HasCue
      # If body evaluates to nil, this means some condition in the body is
      # definitely false, therefore, the feature as a whole can be eliminated.
      # E.g. in `in-error:active:bg-blue-300`, if active evaluates to false,
      # the entire feature can be omitted. We don't even have to check for
      # in-error and so on.
      return unless body = eval?(ctx, feature.body)

      case filter
      in InCue  then requirement = DnCue.new(filter.name)
      in HasCue then requirement = UpCue.new(filter.name)
      end

      case body
      in CuedAtom
        CueCond.new(Pf::Set[requirement.as(UpCue | DnCue)], body)
      in CueCond
        CueCond.new(body.requirements.add(requirement), body.body)
      end
    end
  end

  private def eval(ctx : EvalContext, filter : PairPresent) : Bool
    return false unless value = ctx.pairspart.follow?(filter.keypath)
    return false if value == Term[false]

    true
  end

  private def eval(ctx : EvalContext, filter : PairEq) : Bool
    return false unless value = ctx.pairspart.follow?(filter.keypath)
    return false unless value == filter.value

    true
  end

  # We could try to get rid of nil at the type level with some parsing and
  # narrowing of FeatureSeqs, then location- and non-location-EvalContexts,
  # but is it *really* worth the effort?

  private def eval(ctx : EvalContext, filter : IsFirst) : Bool
    (ctx.location || raise ArgumentError.new).first?
  end

  private def eval(ctx : EvalContext, filter : IsLast) : Bool
    (ctx.location || raise ArgumentError.new).last?
  end

  private def eval(ctx : EvalContext, filter : IsOnly) : Bool
    (ctx.location || raise ArgumentError.new).only?
  end

  private def eval(ctx : EvalContext, filter : IsPeriod) : Bool
    (ctx.location || raise ArgumentError.new).period?(filter.p)
  end

  private def eval(ctx : EvalContext, filter : InCue | HasCue) : InCue | HasCue
    filter
  end

  private def eval(ctx : EvalContext, filter : Not) : Bool
    !eval(ctx, filter.arg)
  end

  alias FeatureEvaluation = CuedFeatureEvaluation | UncuedFeatureEvaluation

  defrecord CuedFeatureEvaluation, features : Slice(CuedFeature)
  defrecord UncuedFeatureEvaluation, features : Slice(UncuedFeature), present : Bool

  private def eval(ctx : EvalContext, features : Slice(Feature)) : FeatureEvaluation
    cls = UncuedFeatureEvaluation
    cued = Pf::Kit.stack_array(CuedFeature)
    uncued = Pf::Kit.stack_array(UncuedFeature)
    present = true

    features.each do |feature|
      next unless result = eval?(ctx, feature)

      cued << result

      case result
      in UncuedFeature
        uncued << result
      in Present
        present = true
      in Absent
        present = false
      in CueCond, UpCue, DnCue
        cls = CuedFeatureEvaluation
      end
    end

    case cls
    in CuedFeatureEvaluation.class
      cls.new(cued.to_unsafe_readonly_slice!)
    in UncuedFeatureEvaluation.class
      cls.new(uncued.to_unsafe_readonly_slice!, present)
    end
  end

  # If one of the node's children changes, this invalidates the cache entry for the node
  # itself. But if its style hasn't changed, we don't want to waste time re-evaluating it.
  # Thus we make use of a separate eval cache.
  private def eval(codex : Codex, location : Location, pairspart : Term::Dict, features : FeatureSeq) : FeatureEvaluation
    # Suppress location for more cache hits.
    unless features.refers_to_location?
      location = nil
    end

    # Keep only entries that the feature seq actually refers to for more
    # cache hits. Unrelated changes in the pairspart shouldn't invalidate
    # our cache.
    pairspart = Term.select(pairspart, features.referred_keys)

    ctx = EvalContext.new(location, pairspart)

    codex.eval_cache.put_if_absent({ctx, features}) do
      eval(ctx, features.content)
    end
  end

  private def eval(codex : Codex, thunk : StyleThunk, location : Location, pairspart : Term::Dict) : StyleNode
    features = thunk.features
    children = thunk.children

    if features.nil? # An inert node
      case children
      in Slice(UncuedStyleNode)
        return UncuedStyleNode.new(present: true, features: nil, children: children)
      in Slice(StyleNode)
        return CuedStyleNode.new(features: nil, children: children, cue_membrane: false)
      end
    end

    evaln = eval(codex, location, pairspart, features)

    case {evaln, children}
    in {UncuedFeatureEvaluation, Slice(UncuedStyleNode)}
      UncuedStyleNode.new(evaln.present, evaln.features, children)
    in {UncuedFeatureEvaluation, Slice(StyleNode)}
      CuedStyleNode.new(features.cue_membrane?, evaln.features.map(&.as(CuedFeature)), children)
    in {CuedFeatureEvaluation, Slice(UncuedStyleNode)}
      CuedStyleNode.new(features.cue_membrane?, evaln.features, children.map(&.as(StyleNode)))
    in {CuedFeatureEvaluation, Slice(StyleNode)}
      CuedStyleNode.new(features.cue_membrane?, evaln.features, children)
    end
  end

  private def recognize_node!(codex : Codex, node : Term::Dict) : Outcome::Accepted(StyleThunk)
    Outcome.accumulate do |acc|
      leaf = false
      offset = 0
      children = Pf::Kit.stack_array(StyleNode, 8)

      # Parse features in style: `...` if present.
      features = pass do
        next unless head = node[0]?
        next unless head.type.symbol?

        offset = 1

        preset_features = codex.preset?(head)
        unless style = node[:style]?
          next preset_features
        end

        unless style = style.as_s?
          acc << Diagnostic.of("style is not a string")
          next preset_features
        end

        style_features = acc.unwrap(features(codex, :style, style.to(String)))

        if style_features.leaf?
          leaf = true
        end

        preset_features ? preset_features + style_features : style_features
      end

      unless leaf
        # Recognize children recursively.
        fanout = node.items.move(offset)
        fanout.each_with_index(offset) do |item, ref|
          next unless item_node = item.as_d?

          # First we ask them for their thunk...
          thunk = acc.unwrap(recognize_node(codex, item_node).at(ref))

          # Then we evaluate the thunk; this converts it to a StyledNode, which we
          # are allowed to have as a child.
          location = Location.new(index: ref - offset, total: fanout.size)
          child = eval(codex, thunk, location, item_node.pairspart)
          children << child
        end
      end

      if children.all?(UncuedStyleNode)
        children = children.to_readonly_slice(&.as(UncuedStyleNode))
      else
        children = children.to_unsafe_readonly_slice!
      end

      Outcome.ok(StyleThunk.new(features, children))
    end
  end

  private def recognize_node(codex : Codex, node : Term::Dict) : Outcome::Accepted(StyleThunk)
    codex.recognize_cache.put_if_absent(node) do
      recognize_node!(codex, node)
    end
  end

  # Interprets an arbitrary *node* as a Microfold node. Returns the resulting
  # `StyleNode` alongside diagnostics, which are rooted at *node*.
  def recognize(codex : Codex, node : Term::Dict) : Outcome::Accepted(StyleNode)
    codex.recognize_cache.epoch do
      codex.feature_cache.epoch do
        codex.eval_cache.epoch do
          thunk_out = recognize_node(codex, node)
          thunk_out.map do |thunk|
            eval(codex, thunk, location: Location.new(index: 0, total: 1), pairspart: node.pairspart)
          end
        end
      end
    end
  end
end
