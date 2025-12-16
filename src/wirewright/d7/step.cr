module Ww::D7
  # :nodoc:
  alias FoldRep0 = FoldContext, Feature, FoldRep0, FoldDefault -> Term

  # :nodoc:
  alias FoldRep = FoldRep0

  # :nodoc:
  alias FoldDefault = -> Term

  # :nodoc:
  record FoldContext,
    clf : Classifier,
    addr : NodeAddr,
    scope : NodeScope

  # Constructs the initial fold context.
  private def fold_context(clf : Classifier) : FoldContext
    FoldContext.new(clf, NodeAddr.empty, NodeScope.empty)
  end

  private def fold(ctx : FoldContext, node, &frep : FoldRep) : Term
    fold(ctx, node, frep)
  end

  private def fold(ctx : FoldContext, node : Term, frep : FoldRep) : Term
    fold(ctx, ctx.clf.call(node), frep)
  end

  private def fold(ctx : FoldContext, feature : Inert, frep : FoldRep) : Term
    feature.node
  end

  private def fold(ctx : FoldContext, feature : Gnd, frep : FoldRep) : Term
    frep.call(ctx, feature, frep, -> { feature.node })
  end

  private def fold(ctx : FoldContext, feature : Parent, frep : FoldRep) : Term
    default = -> do
      flattenT(feature.node, range: feature.range) do |child, index|
        fold(ctx.copy_with(addr: ctx.addr.append(index)), child, frep)
      end
    end

    frep.call(ctx, feature, frep, default)
  end

  private def fold(ctx : FoldContext, feature : Scope, frep : FoldRep) : Term
    default = -> do
      subscope = ctx.scope.append({ctx.addr, feature.bindings})

      fold(ctx.copy_with(scope: subscope), feature.cont, frep)
    end

    frep.call(ctx, feature, frep, default)
  end

  private def fold(ctx : FoldContext, feature : Mixture, frep : FoldRep) : Term
    default = -> do
      feature.mix.call(feature.node, fold(ctx, feature.defn, frep))
    end

    frep.call(ctx, feature, frep, default)
  end

  # NOTE: The default handling for a circuit is to evaluate its continuation. For
  # example, a `(circuit @x _)`'s default handling is `(cell @x _)`. Evaluation
  # is non-default behavior. That is, most likely, the default handling is to stop
  # at the circuit, without recursively folding it.
  private def fold(ctx : FoldContext, feature : Circuit, frep : FoldRep) : Term
    default = -> do
      fold(ctx, feature.cont.call(Term.of(feature.node)), frep)
    end

    frep.call(ctx, feature, frep, default)
  end

  private def flattenT(dict : Term::Dict, range : Range(Int32, Int32), & : Term, Int32 -> Term) : Term
    Term.of(flatten(dict, range) { |item, index| yield item, index })
  end

  private def flatten(dict : Term::Dict, range : Range(Int32, Int32), & : Term, Int32 -> Term) : Term::Dict
    dict.transaction do |commit|
      dict.each_item_with_index(within: range) do |item0, index|
        item1 = yield item0, index
        next if item0 == item1

        commit.with(index, item1)
      end
    end
  end

  # Annotates *edge* with scope info based on the current *scope*.
  private def hyperedge(scope : NodeScope, edge : Term) : Hyperedge
    while entry = scope.last?
      addr, bindings = entry

      unless exterior = bindings[edge]?
        return Hyperedge.new(addr, edge)
      end

      edge = exterior
      scope = scope[...-1]
    end

    Hyperedge.new(NodeAddr.empty, edge)
  end

  # Constructs a hypergraph of *scoped* nodes from *root*, continuing the fold
  # defined by *ctx*.
  private def hypergraph(ctx : FoldContext, root : Feature) : Hypergraph
    nodemap = [] of Term
    edgemap = [] of Slice(Hyperedge)
    trmap = [] of NodeAddr

    _ = fold(ctx, root) do |ctx, feature, rec, default|
      if feature.is_a?(Gnd)
        nodemap << feature.node
        edgemap << feature.edges.to_readonly_slice { |edge| hyperedge(ctx.scope, edge) }
        trmap << ctx.addr
      end

      default.call
    end

    Hypergraph.new(nodemap, edgemap, trmap)
  end

  private def apply(ctx : FoldContext, hg : Hypergraph, root : Feature, patch : Patch) : Term
    table = patch.transform_keys { |key| hg.addr(key) }

    fold(ctx, root) do |ctx, feature, _, default|
      feature.is_a?(Gnd) ? (table[ctx.addr]? || default.call) : default.call
    end
  end

  # :nodoc:
  def walk(clf : Classifier, regime : Regime, circuit : Term, &body : Regime::Body) : Term
    walk(clf, circuit) { |hg| regime.solve(hg, body) }
  end

  # :nodoc:
  def walk(clf : Classifier, circuit : Term, &propose : Hypergraph -> Patch)
    unless circuit.type.dict?
      return circuit
    end

    walk(fold_context(clf), circuit(circuit.as_d), propose)
  end

  private def walk(ctx : FoldContext, circuit : Circuit, propose : Hypergraph -> Patch)
    root = parent(circuit.node, circuit.range)
    hg = hypergraph(ctx, root)

    patch = propose.call(hg)
    result = patch.empty? ? root.node : apply(ctx, hg, root, patch)

    root1 = parent(result.as_d, circuit.range)
    fold(ctx, root1) do |ctx, feature, rec, default|
      feature.is_a?(Circuit) ? walk(ctx, feature, propose) : default.call
    end
  end

  # Calls *fn* with features in *circuit* and their addresses.
  #
  # *mix* lets you disable node split-mix (`Mixture`). If false, mixture nodes
  # are treated as ground nodes.
  #
  # NOTE: If *mix* is `false`, a node address is equivalent to the itempath to that
  # node starting from *circuit*. If *mix* is `true`, the address will still be fully
  # qualified for a node, but it will not be a valid itempath.
  def each_feature_with_addr(clf : Classifier, circuit : Term, *, mix : Bool = true, &fn : Feature, NodeAddr ->)
    return unless circuit.type.dict?

    fold(fold_context(clf), parent(circuit.as_d)) do |ctx, feature, rec, default|
      fn.call(feature, ctx.addr)

      case feature
      when Gnd, Inert
        default.call
      when Mixture
        if mix
          default.call
        else
          fold(ctx, gnd(feature.node), rec)
        end
      when Circuit
        fold(ctx, parent(feature.node, feature.range), rec)
      else
        default.call
      end
    end
  end

  # Returns a hash mapping addresses of nodes in *circuit* to identified features
  # of those nodes.
  #
  # See `each_feature_with_addr` for info on *kwargs* and related.
  def features(clf : Classifier, circuit : Term, **kwargs) : Hash(NodeAddr, Feature)
    features = {} of NodeAddr => Feature

    each_feature_with_addr(clf, circuit, **kwargs) do |feature, addr|
      features[addr] = feature
    end

    features
  end

  # Returns a hash mapping addresses of ground and inert nodes to those nodes
  # in *circuit*. This effectively "flattens" *circuit*.
  #
  # See `each_feature_with_addr` for info on *kwargs* and related.
  def leaves(clf : Classifier, circuit : Term, **kwargs) : Hash(NodeAddr, Term)
    nodes = {} of NodeAddr => Term

    each_feature_with_addr(clf, circuit, **kwargs) do |feature, addr|
      next unless feature.is_a?(Gnd) || feature.is_a?(Inert)

      nodes[addr] = feature.node
    end

    nodes
  end

  # :nodoc:
  REGIMES = SyncHash(UInt32, Regime).new(initial_capacity: 32)

  # :nodoc:
  REGIME_ID = [0u32]

  macro case(clf, circuit, &block)
    {%
      unless block
        raise "`D7.case` expects a block containing `rule` branches"
      end

      id = REGIME_ID[0]
      REGIME_ID[0] += 1

      stmts = block.body
      if stmts.is_a?(Expressions)
        stmts = stmts.expressions
      elsif stmts.is_a?(Nop)
        stmts = [] of ::NoReturn
      else
        stmts = [stmts]
      end

      branches = [] of ::NoReturn

      stmts.each do |stmt|
        unless stmt.is_a?(Call) && stmt.name == :rule && stmt.args.size >= 1 && stmt.block
          stmt.raise "regime: expected a call to `rule(*patterns : String, &)`"
        end

        stmt.args.each do |pattern|
          matches = pattern.scan(/\((?:one|many)\s(\w+)/)
          participants = matches.map { |match| match[1].id }
          branches << {pattern: pattern, participants: participants, body: stmt.block.body}
        end
      end

      if branches.empty?
        block.raise "expected at least one `rule` branch"
      end
    %}\

    %regime = {{@type}}::REGIMES.put_if_absent({{id}}) do
      %queries = Pointer(Term).malloc({{branches.size}})
      {% for branch, index in branches %}\
        %queries[{{index}}] = ::Ww::ML.terms({{branch[:pattern]}})
      {% end %}\

      {{@type}}::Regime.build(Slice(Term).new(%queries, {{branches.size}}, read_only: true))
    end

    D7.walk({{clf}}, %regime, {{circuit}}) do |%soln, %index|
      case %index
      {% for branch, index in branches %}\
      when {{index}}
        %participants{index} = {
          {% for participant in branch[:participants] %}\
            %soln.groups[Term.of({{participant.symbolize}})],
          {% end %}\
        }

        %result{index} = pass(*%participants{index}) do |{{branch[:participants].splat}}|
          {{branch[:body]}}
        end

        %result{index} || {{@type}}::Patch.new
      {% end %}\
      else
        raise ArgumentError.new
      end
    end
  end

  # Performs *subframe compaction*. *Subframe compaction* is a fancy way of saying
  # "If the next frame has all changes that the current one has, then we don't need
  # to show the current frame to the user; they'll see the changes in the next
  # frame anyway". In other words, if the next frame subsumes the current one,
  # the current one is skipped.
  #
  # Yields frames to show to the user.
  #
  # This method may yield duplicate consecutive frames, and it is the caller's
  # responsibility to filter them out. We do not do it here because the caller is likely
  # to do that at frame-level anyway, so there is no need to do the work on subframes.
  #
  # *seen* must be the last frame seen by the user. Usually this would be the last
  # frame yielded by this method. Otherwise it would be the very first circuit,
  # which the caller itself should show to the user as the first frame. This method
  # will never yield *seen* (unless as a duplicate).
  def squash(clf : Classifier, seen : Term, subframes : Indexable(Term), &) : Nil
    if subframes.empty?
      raise ArgumentError.new
    end

    changed = Set(NodeAddr).new

    ahead = Deque(Term).new
    ahead.concat(subframes)

    a = seen
    ns = leaves(clf, seen, mix: false)

    while b = ahead.shift?
      ms = leaves(clf, b, mix: false)

      # Cut if:
      # - New nodes were added or removed in the next subframe.
      # - A node that was already modified was modified in the next subframe.
      unless ns.size == ms.size && ns.all? { |addr, _| ms.has_key?(addr) } && ms.all? { |addr, m| !addr.in?(changed) || ns[addr] == m }
        yield a
        a = b
        ns = ms
        changed.clear
        next
      end

      # Changes are disjoint. We can skip showing A because B has all
      # the same changes.
      ms.each do |addr, m|
        n = ns[addr]?
        next if n == m

        changed << addr
      end

      a = b
      ns = ms
    end

    yield a
  end

  alias Pass = Classifier, Term -> Term

  # :nodoc:
  class FrameIterator
    include Iterator(Term)

    def initialize(@clf : Classifier, @circuit : Term, @passes : Indexable(Pass))
      @memo = @circuit
      @ahead = Deque{@circuit}
    end

    def next
      if circuit = @ahead.shift?
        return circuit
      end

      state = @circuit
      subframes = @passes.map { |pass| state = pass.call(@clf, state) }
      if @circuit == state
        return Iterator.stop
      end

      D7.squash(@clf, @memo, subframes) do |frame|
        next if @memo == frame

        @ahead << frame
        @memo = frame
      end

      @circuit = state
      @ahead.shift
    end
  end

  def frames(clf : Classifier, circuit : Term, passes : Indexable(Pass)) : Iterator(Term)
    FrameIterator.new(clf, circuit, passes)
  end

  def frames(clf : Classifier, circuit : Term, *passes : Pass) : Iterator(Term)
    frames(clf, circuit, passes)
  end
end
