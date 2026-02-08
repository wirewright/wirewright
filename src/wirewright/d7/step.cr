module Ww::D7
  # Calls *frep* (a replacement function) with each `Flat` feature in
  # *circuit*; expects the function to return the replacement term.
  # Returns the modified version of *circuit*.
  #
  # - Each `Flat` feature corresponds to a node term in *circuit*.
  #   The replacement function is used to produce its offspring.
  # - *clf* is the classifier to use (see `Classifier`).
  # - *circuit* is the circuit term (a dict; this function is noop otherwise).
  # - *depth* sets the depth limit. `0` means `Flat` nodes in *circuit*, `1`
  #   means `Flat` nodes in *circuit*'s sub-`Circuit`s, `1` in sub-sub-`Circuit`s,
  #   and so on. In other words, *depth* is not about term depth but about
  #   *circuit depth*; it is about evaluating nested circuits at a specific depth
  #   (e.g. the `circuit` node in Rack), presupposing an IDDFS-like caller.
  # - Along with the feature, *frep*' is called with the node's address and scope
  #   (both are stable throughout changes to *depth*; both are unstable throughout
  #   changes to *circuit* and *clf*).
  def flatten(clf : Classifier, circuit : Term, *, depth : Int, &frep : NodeAddr, NodeScope, Flat -> Term) : Term
    assert depth >= 0

    unless nodes = circuit.as_d?
      return circuit
    end

    flatten(clf,
      addr: NodeAddr.empty,
      scope: NodeScope.empty,
      depth: depth,
      feature: parent(nodes, 0...nodes.itemsize),
      sink: frep,
    )
  end

  # :nodoc:
  def flatten(clf, addr, scope, depth, feature : Inert, sink) : Term
    unless depth.zero?
      return feature.node
    end

    sink.call(addr, scope, feature)
  end

  # :nodoc:
  def flatten(clf, addr, scope, depth, feature : Gnd, sink) : Term
    unless depth.zero?
      return feature.node # Not the target depth, so we shouldn't use defn!
    end

    sink.call(addr, scope, feature)
  end

  # :nodoc:
  def flatten(clf, addr, scope, depth, feature : Circuit, sink) : Term
    if depth.zero?
      return flatten(clf, addr, scope, depth, feature.leaf.call, sink)
    end

    assert depth > 0

    # NOTE: Circuits must create scopes to seal themselves from the outside
    # world completely. Otherwise, two circuits with the same depth could comm,
    # and that goes against our semantics.
    #
    #   ;; Must NOT work!
    #   (circuit @0 (cell @x 100))
    #   (circuit @1 (cell @y))
    #   (circuit @2 (feed @x @y))
    #
    flatten(clf, addr, scope.append({addr, Term[]}), depth - 1, parent(feature.node, feature.range), sink)
  end

  # :nodoc:
  def flatten(clf, addr, scope, depth, feature : Parent, sink) : Term
    node0 = feature.node
    node1 = flatten(node0, range: feature.range) do |child, index|
      flatten(clf, addr.append(index), scope, depth, child, sink)
    end

    Term.of(node1)
  end

  # :nodoc:
  def flatten(clf, addr, scope, depth, feature : Scope, sink) : Term
    flatten(clf, addr, scope.append({addr, feature.bindings}), depth, feature.cont, sink)
  end

  # :nodoc:
  def flatten(clf, addr, scope, depth, feature : Mixture, sink) : Term
    feature.mix.call(flatten(clf, addr, scope, depth, feature.defn, sink))
  end

  # :nodoc:
  def flatten(clf, addr, scope, depth, node : Term, sink) : Term
    flatten(clf, addr, scope, depth, clf.call(node), sink)
  end

  # :nodoc:
  def flatten(dict : Term::Dict, range : Range(Int32, Int32), &) : Term::Dict
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

  # :nodoc:
  #
  # The iterative deepening process in `step` is restricted because in theory,
  # one could design an abomination of a circuit that would somehow (I don't know
  # how, exactly) make parent circuits create sub-circuits that in turn create sub-
  # sub-circuits in the same way and so on, indefinitely. I'm not sure whether true
  # infinite recursion is possible; but I sure don't want to find out!
  #
  # 512 is huge and should be enough for anything anyway. With this limit, circuits
  # below depth 511 (0...512) are going to be "passive" (never evaluated; only seen
  # as `cell`s, in Rack terms).
  MAX_SUBSTEPS = 512

  # Executes one time-step for *circuit* (the *previous frame*). Returns
  # the resulting sequence of substeps, the last of which is the *next frame*.
  #
  # The algorithm runs an iterative-deepening circuit traversal in which ground
  # nodes at each consecutive target depth are assembled into a hypergraph.
  # The hypergraph is then solved by the block to obtain a patch (see `Regime#solve`
  # for relevant code). The algorithm applies the patch, producing a target depth-
  # patched *circuit*. The target depth-patched circuit is traversed on the next
  # iteration of deepening. Each target depth-patched circuit is recorded as
  # a substep, forming the resulting sequence of substeps.
  #
  # Replacement proceeds top-down (see `D7` for reasoning).
  #
  # See `D7` for terminology (e.g. subframe vs. substep).
  def step(clf : Classifier, circuit : Term, & : Hypergraph -> Patch) : Slice(Term)
    substeps = Pf::Kit.stack_array(Term, 8)

    MAX_SUBSTEPS.times do |depth|
      substeps << circuit

      hg = Hypergraph.build do |builder|
        _ = flatten(clf, circuit, depth: depth) do |addr, scope, flat|
          case flat
          in Inert
          in Gnd
            # Use Gnd#defn (the node's definition) rather than #node here.
            # The hypergraph should only ever see the defn.
            builder.submit(addr, flat.defn, flat.edges) do |edge|
              hyperedge(scope, edge)
            end
          end

          flat.node # Leave unchanged
        end
      end

      # No nodes found at *depth*, no nodes added, we're done.
      break if hg.empty?

      patch = yield hg
      next if patch.empty?

      addr_patch = patch.to_h do |node_id, replacement|
        {hg.addr(node_id), replacement}
      end

      circuit = flatten(clf, circuit, depth: depth) do |addr, scope, flat|
        case flat
        in Inert then flat.node
        in Gnd   then addr_patch[addr]? || flat.node
        end
      end
    end

    substeps.to_readonly_slice(&.itself)
  end

  # Calls *sink* with `Flat` features in *circuit* and their corresponding address.
  #
  # - Always descends into circuits (never runs their leaf function).
  # - Descends into `Mixture`'s definition if *unmix* is true, otherwise
  #   treats `Mixture`s as `Gnd` nodes.
  # - If *unmix* is `false`, the address of a node is guaranteed to be the itempath
  #   to that node starting at *circuit*. If *unmix* is `true`, on the other hand,
  #   the address is not guaranteed to be a valid itempath. In both cases, the addresses
  #   of all nodes uniquely identify them within the same *circuit*.
  def each_feature_with_addr(clf : Classifier, circuit : Term, *, unmix : Bool = true, &sink : Flat, NodeAddr ->) : Nil
    unless nodes = circuit.as_d?
      return circuit
    end

    each_feature_with_addr(clf, NodeAddr.empty, parent(nodes, 0...nodes.itemsize), unmix, sink)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Inert | Gnd, unmix, sink) : Nil
    sink.call(feature, addr)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Mixture, unmix : Bool, sink) : Nil
    if unmix
      successor = clf.call(feature.defn)
    else
      successor = gnd(feature.node)
    end

    each_feature_with_addr(clf, addr, successor, unmix, sink)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Scope, unmix, sink) : Nil
    each_feature_with_addr(clf, addr, feature.cont, unmix, sink)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Parent | Circuit, unmix, sink) : Nil
    items = feature.node.items

    feature.range.each do |index|
      item = items[index]

      each_feature_with_addr(clf, addr.append(index), clf.call(item), unmix, sink)
    end
  end

  # Returns a hash map of `Gnd` nodes in *circuit*.
  #
  # See also: `each_feature_with_addr`.
  def node_map(clf : Classifier, circuit : Term, **kwargs) : Hash(NodeAddr, Term)
    node_map = {} of NodeAddr => Term

    each_feature_with_addr(clf, circuit, **kwargs) do |feature, addr|
      next unless feature.is_a?(Gnd)

      node_map[addr] = feature.node
    end

    node_map
  end
end
