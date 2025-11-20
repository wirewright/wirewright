module Ww::D7
  defrecord Top, queue : Term::Dict

  def step(clf : Classifier, regime : Regime, top : Top, circuit : Term, **kwargs, &tick : Term, NodeChat -> Reaction) : {Top, Term}
    unit(top, circuit) do |unit|
      circuit = clf.call(unit)
      assert circuit.is_a?(Circuit)

      step(fold_context(clf, **kwargs), regime, circuit, &tick)
    end
  end

  def step(ctx : FoldContext, regime : Regime, circuit : Circuit, &tick : Term, NodeChat -> Reaction) : Reaction
    # Step nested circuits.
    top0 = parent(circuit.node, circuit.range)
    toprxn0 = fold(ctx, top0) do |ctx, feature, rec, default|
      case feature
      when Chat    then fold(ctx, feature.cont, rec)
      when Circuit then step(ctx, regime, feature, &tick)
      else
        default.call
      end
    end

    top1 = parent(toprxn0.node, circuit.range)

    # Find and execute relations.
    reactions = relate(ctx, regime, top1, subcircuits: false)

    # Replace participants with their resulting forms and unscope.
    fold(ctx, top1) do |ctx, feature, _, default|
      unless feature.is_a?(Gnd)
        next default.call
      end

      unless rxn = reactions[ctx.addr]?
        scoped, _ = scoped(ctx.scope, feature.node, feature.edges)
        rxn = tick.call(scoped, ctx.chat)
      end

      unscope(ctx, rxn)
    end
  end

  # Finds and executes all possible ground node relations in *root* according
  # to *regime*. Returns a hash mapping each relation participant to its
  # resulting `Reaction` to that participation. Nodes in the returned map
  # are scoped.
  def relate(ctx : FoldContext, regime : Regime, root : Feature, *, subcircuits : Bool) : Hash(NodeAddr, Reaction)
    nodemap = [] of Term
    edgemap = [] of Slice(Edge)
    trmap = [] of NodeAddr
    chats = [] of NodeChat

    _ = fold(ctx, root) do |ctx, feature, rec, default|
      case feature
      when Gnd
        node, edges = scoped(ctx.scope, feature.node, feature.edges)
        nodemap << node
        edgemap << edges
        trmap << ctx.addr
        chats << ctx.chat

        default.call
      when Chat
        fold(ctx.copy_with(chat: NodeChat.new(msg: feature.queue.items.first? || Term.of(:cycle), enq: feature.enq)), feature.cont, rec)
      when Circuit
        if subcircuits
          fold(ctx, parent(feature.node, feature.range), rec)
        else
          default.call
        end
      else
        default.call
      end
    end

    if nodemap.empty?
      assert edgemap.empty?
      assert trmap.empty?

      return {} of NodeAddr => Reaction
    end

    hg = Hypergraph.new(nodemap, edgemap, trmap)

    rxns = regime.reactions(hg, chats.to_readonly_slice)
    rxns.transform_keys { |key| trmap[key] }
  end

  def unit(top : Top, circuit : Term, & : Term -> Reaction) : {Top, Term}
    unless circuit.type.dict?
      return top, circuit
    end

    # A circuit like:
    #
    #   (cell @x 100)
    #   (cell @y)
    #   (feed @x @y @x)
    #
    # ... turns into:
    #
    #   (unit
    #     (chat <toplevel queue>
    #       (cell @x 100)
    #       (cell @y)
    #       (feed @x @y @x)))
    #
    # ... at the top-level.

    chat = Term::Dict.build do |commit|
      commit << {:async, :chat} << top.queue
      commit.concat(circuit.items)
    end

    rxn = yield Term.of(:unit, chat)

    # NOTE: rxn's emission can be nonempty if the user wishes that messages
    # bubble up above the toplevel. We simply discard such messages.

    Term.matchpi(rxn.node, %{(unit ((async chat) queue_dict nodes_*))}) do
      {Top.new(queue.as_d), Term.of(nodes | circuit.pairspart)}
    end
  end

  def unscope(ctx : FoldContext, rxn : Reaction) : Reaction
    rxn(unscope(ctx, rxn.node), rxn.emission)
  end

  def unscope(ctx : FoldContext, node : Term) : Term
    if ctx.scope.empty?
      return node
    end

    rxn = fold(ctx, node) do |ctx, feature, rec, default|
      case feature
      when Gnd  then rxn(unscope(feature.node, feature.edges))
      when Chat then fold(ctx, feature.cont, rec)
      else
        default.call
      end
    end

    assert rxn.emission.empty?

    rxn.node
  end

  # Removes scope annotations from *edges* of *node*.
  def unscope(node : Term, edges : Slice(Edge)) : Term
    edges.each do |edge|
      unscoped = unscope(edge.term)
      next if edge.term == unscoped

      node = Term.morph(node, edge.path.to_readonly_slice { |i| Term.of(i) }) { unscoped }
    end

    node
  end

  # Removes scope annotations from *edge*.
  #
  # NOTE: this is purely conventional. Nothing stops the user from forging
  # these. You can validate-out edges that look like these upfront though. Use
  # `scoped?`.
  #
  # NOTE: assumes *edge* is an edge without doing any checks.
  def unscope(edge : Term) : Term
    _, id = edge
    return edge unless id = id.as_d?
    return edge unless id.itemsonly? && id.size == 2

    scope, name = id
    return edge unless scope.type.string?

    Term.of(:edge, name)
  end

  # Adds *scope* to *edges* of *node*. Returns *node* whose edges
  # are annotated with a scope, and a list of scoped edges.
  def scoped(scope : NodeScope, node : Term, edges : Slice(Edge)) : {Term, Slice(Edge)}
    if scope.empty?
      return node, edges
    end

    scoped = edges.to_readonly_slice do |edge|
      term = scoped(scope, edge.term)
      node = morphi(node, edge.path, term)

      edge.copy_with(term: term)
    end

    {node, scoped}
  end

  # Custom morph-item implementation for performance. scoped() is sometimes
  # a hot method so we want it to be as stupid as possible, within limits.
  private def morphi(node : Term, itempath : Slice(Int32), edge : Term) : Term
    return edge unless index = itempath[0]?
    return node unless dict0 = node.as_d?
    return node unless item0 = dict0.item_at?(index)

    item1 = morphi(item0, itempath[1..], edge)
    dict1 = dict0.with(index, item1)

    Term.of(dict1)
  end

  # Returns `true` if *edge* is of the conventional scoped form. Returns
  # `false` otherwise.
  def scoped?(edge : Term) : Bool
    Term.case(edge) do
      matchpi %{(%'edge (_string name_))} { true }
      otherwise { false }
    end
  end

  # Annotates *edge* with scope info based on the current *scope*.
  def scoped(scope : NodeScope, edge : Term) : Term
    while entry = scope.last?
      addr, bindings = entry

      unless exterior = bindings[edge]?
        return annotated(addr, edge)
      end

      edge = exterior
      scope = scope[...-1]
    end

    edge
  end

  # NOTE: assumes *edge* is an edge term without any checks.
  private def annotated(addr : NodeAddr, edge : Term) : Term
    hasher = Term::Hasher.new
    addr.each do |id|
      hasher << id
    end

    # We use Alpha48 because it is used pretty much everywhere else so clients should
    # find it "familiar".
    scope_id = Alpha48.encode(hasher.result)

    Term.of(:edge, {scope_id, edge[1]})
  end
end
