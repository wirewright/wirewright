module Ww::D7
  # The address of a node in a circuit.
  struct NodeAddr
    include Indexable(Int32)

    # :nodoc:
    def initialize(@addr : Pf::UPath32)
    end

    def self.new(objects : Enumerable(T), & : T -> Int32) : NodeAddr forall T
      objects.reduce(empty) { |addr, object| addr.append(yield object) }
    end

    def self.empty : NodeAddr
      NodeAddr.new(Pf::UPath32[])
    end

    def self.[](*indices : Int32) : NodeAddr
      new(indices, &.itself)
    end

    def size : Int32
      @addr.size
    end

    def unsafe_fetch(index : Int) : Int32
      @addr[index].to_i
    end

    def append(index : Int32) : NodeAddr
      NodeAddr.new(@addr.append(index.to_u32))
    end
  end

  # Records the scopes that the traversal process passes through. The addr
  # is that of the scope (e.g. `module`, but in general, see `Scope`), and
  # the dict is its bindings dict.
  struct NodeScope
    alias Any = OpenExcept | ClosedExcept

    defrecord OpenExcept, edges : Slice(Term)
    defrecord ClosedExcept, bindings : Term::Dict

    def initialize(@trace : Slice({NodeAddr, Any}))
    end

    def self.empty : NodeScope
      new(Slice({NodeAddr, Any}).empty)
    end

    def [](edge : Term) : {NodeAddr, Term}
      trace = @trace

      while entry = trace.last?
        addr, scope = entry

        case scope
        in OpenExcept
          if edge.in?(scope.edges)
            # In Rack:
            #  (local (⏏@dst⏏) ;; <<- WE ARE HERE, @dst found, so it's a local!
            #    (feed @src ⏏@dst⏏)
            return addr, edge
          end
          # Continue climbing. This edge falls into "open", thus outer-scoped,
          # not "except" and thus inner-scoped.
          #
          # In Rack:
          #  (local (@dst) ;; <<- WE ARE HERE, @src NOT found, so it's outerly-scoped.
          #    (feed ⏏@src⏏ @dst)
        in ClosedExcept
          unless exterior = scope.bindings[edge]?
            # In Rack:
            #  (module {@x: @y} ;; <<- WE ARE HERE, no @a, so it's a local!
            #    (feed ⏏@a⏏ @x)
            return addr, edge
          end

          # In Rack:
          #  (module {⏏@x⏏: @y} ;; @x found, its *exterior* is the outerly-scoped @y.
          #    (feed @a ⏏@x⏏)
          edge = exterior
        end

        trace = trace[...-1]
      end

      {NodeAddr.empty, edge}
    end

    def append(addr : NodeAddr, scope : Any) : NodeScope
      NodeScope.new(@trace.append({addr, scope}))
    end
  end

  # Calls *frep* (a replacement function) with each `Flat` feature in
  # *circuit*; expects the function to return the replacement term.
  # Returns the modified version of *circuit*.
  #
  # - Each `Flat` feature corresponds to a node term in *circuit*.
  #   The replacement function is used to produce its offspring.
  # - *clf* is the classifier to use (see `Classifier`).
  # - *circuit* is the circuit term (a dict; this function is noop otherwise).
  # - *depth* sets the depth limit. `0` means `Flat` nodes in *circuit*, `1`
  #   means `Flat` nodes in *circuit*'s sub-`Circuit`s, `2` in sub-sub-`Circuit`s,
  #   and so on. In other words, *depth* is not about term depth but about
  #   *circuit depth*; it is about evaluating nested circuits at a specific depth
  #   (e.g. the `circuit` node in Rack), presupposing an IDDFS-like caller.
  # - Along with the feature, *frep*' is called with the node's address and scope
  #   (both are stable throughout changes to *depth*; both are unstable throughout
  #   changes to *circuit* and *clf*).
  def update(clf : Classifier, circuit : Term, *, depth : Int, &frep : NodeAddr, NodeScope, Flat -> Term) : Term
    assert depth >= 0

    unless nodes = circuit.as_d?
      return circuit
    end

    update(clf,
      addr: NodeAddr.empty,
      scope: NodeScope.empty,
      depth: depth,
      feature: parent(nodes),
      sink: frep,
    )
  end

  # :nodoc:
  def update(clf, addr, scope, depth, feature : Inert, sink) : Term
    unless depth.zero?
      return feature.node
    end

    sink.call(addr, scope, feature)
  end

  # :nodoc:
  def update(clf, addr, scope, depth, feature : Gnd, sink) : Term
    unless depth.zero?
      return feature.node # Not the target depth, so we shouldn't use defn!
    end

    sink.call(addr, scope, feature)
  end

  # :nodoc:
  def update(clf, addr, scope, depth, feature : Circuit, sink) : Term
    if depth.zero?
      return update(clf, addr, scope, depth, feature.leaf.call, sink)
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
    update(clf, addr, scope.append(addr, NodeScope::ClosedExcept.new(Term[])), depth - 1, parent(feature.node, feature.range), sink)
  end

  # :nodoc:
  def update(clf, addr, scope, depth, feature : Parent, sink) : Term
    node0 = feature.node
    node1 = update(node0, range: feature.range) do |child, index|
      update(clf, addr.append(index), scope, depth, child, sink)
    end

    Term.of(node1)
  end

  # :nodoc:
  def update(clf, addr, scope, depth, feature : Scope, sink) : Term
    update(clf, addr, scope.append(addr, feature.scope), depth, feature.cont, sink)
  end

  # :nodoc:
  def update(clf, addr, scope, depth, feature : Mixture, sink) : Term
    feature.mix.call(update(clf, addr, scope, depth, feature.defn, sink))
  end

  # :nodoc:
  def update(clf, addr, scope, depth, node : Term, sink) : Term
    update(clf, addr, scope, depth, clf.call(node), sink)
  end

  # :nodoc:
  def update(dict : Term::Dict, range : Range(Int32, Int32), &) : Term::Dict
    assert range.exclusive?

    changes = Pf::Kit.stack_array({Term, Int32}, 8)

    range.each do |index|
      item0 = dict[index]
      item1 = yield item0, index
      next if item0 == item1

      changes << {item1, index}
    end

    # Fast, no-alloc path for cases when no changes were made to the dict.
    if changes.empty?
      return dict
    end

    dict.transaction do |commit|
      changes.each do |item, index|
        commit.with(index, item)
      end
    end
  end

  # Represents the three ways you can image a parent.
  #
  # - As its children (thus, the parent disappears);
  # - As itself, replacing children in its children range with their images (*node*);
  # - In a user-specific way, in which case you have access to the original
  #   *parent* and the images of *children*.
  record ParentImage, parent : Parent, children : Slice(Term) do
    def node : Term
      range = Term[parent.range.begin]...Term[parent.range.end]
      result = parent.node.replace(range, &.concat(children))

      Term.of(result)
    end
  end

  # Returns the view of *circuit* as if seen through a "lens". The lens is defined
  # by *clf* and *fn*. Only that structure is preserved which is visible through
  # the lens; its image possibly altered by the lens.
  #
  # This function backs one of the core metaphors of Wirewright and Rack in
  # particular: in that you can look at the same term (here, *circuit*) in lots
  # of different ways. You can look at it raw, or through a graphics "lens" that
  # renders it as a literal image; symbolically; or in some other way. The same
  # thing, seen with different "glasses" or "lenses", assumes different appearances,
  # some of them more useful than others at that particular instant. Importantly,
  # the metaphor assumes those appearances are bidirectional: you can "poke" them
  # and the raw object responds. This isn't particularly relevant here, but it is
  # relevant otherwise.
  def image(clf : Classifier, circuit : Term, &fn : NodeAddr, ParentImage | Inert | Gnd -> Slice(Term)) : Term
    unless nodes = circuit.as_d?
      return circuit
    end

    Term.of(image(clf, NodeAddr.empty, parent(nodes), fn))
  end

  # :nodoc:
  def image(clf, addr, feature : Inert | Gnd, fn) : Slice(Term)
    fn.call(addr, feature)
  end

  # :nodoc:
  def image(clf, addr, feature : Circuit, fn) : Slice(Term)
    image(clf, addr, parent(feature.node, feature.range), fn)
  end

  # :nodoc:
  def image(clf, addr, feature : Parent, fn) : Slice(Term)
    children = Pf::Kit.stack_array(Term)

    feature.range.each do |index|
      child = feature.node[index]
      image(clf, addr.append(index), child, fn).each do |seln|
        children << seln
      end
    end

    img = ParentImage.new(feature, children.to_readonly_slice(&.itself))
    fn.call(addr, img)
  end

  # :nodoc:
  def image(clf, addr, feature : Scope, fn) : Slice(Term)
    image(clf, addr, feature.cont, fn)
  end

  # :nodoc:
  def image(clf, addr, feature : Mixture, fn) : Slice(Term)
    image(clf, addr, feature.defn, fn)
  end

  # :nodoc:
  def image(clf, addr, node : Term, fn) : Slice(Term)
    image(clf, addr, clf.call(node), fn)
  end

  # Similar to `image`, but *replaces* nodes using *fn* instead.
  def map(clf : Classifier, circuit : Term, &fn : NodeAddr, Inert | Gnd | Parent -> Term) : Term
    unless nodes = circuit.as_d?
      return circuit
    end

    Term.of(map(clf, NodeAddr.empty, parent(nodes), fn))
  end

  # :nodoc:
  def map(clf, addr, feature : Inert | Gnd, fn) : Term
    fn.call(addr, feature)
  end

  # :nodoc:
  def map(clf, addr, feature : Circuit, fn) : Term
    map(clf, addr, parent(feature.node, feature.range), fn)
  end

  # :nodoc:
  def map(clf, addr, feature : Parent, fn) : Term
    result = update(feature.node, feature.range) do |child, index|
      map(clf, addr.append(index), child, fn)
    end

    fn.call(addr, parent(result, feature.range))
  end

  # :nodoc:
  def map(clf, addr, feature : Scope, fn) : Term
    map(clf, addr, feature.cont, fn)
  end

  # :nodoc:
  def map(clf, addr, feature : Mixture, fn) : Term
    feature.mix.call(map(clf, addr, feature.defn, fn))
  end

  # :nodoc:
  def map(clf, addr, node : Term, fn) : Term
    map(clf, addr, clf.call(node), fn)
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

  # Executes one time-step on *circuit* (the *previous frame*). Returns
  # the resulting sequence of substeps, the last of which is the *next frame* --
  # the given *circuit* at t+1.
  #
  # The algorithm runs a top-down iterative-deepening circuit traversal in which ground
  # nodes at each consecutive target depth are assembled into a hypergraph.
  # The hypergraph is then solved by the block to obtain a patch (see `Regime#solve`
  # for relevant code). The algorithm applies the patch, producing a target depth-
  # patched *circuit*. This target depth-patched circuit is traversed on the next
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

      hg = Hypergraph.new
      running = false

      # This update() can still do template expansion etc. -- even though
      # *we* do not change the circuit, *clf* might.
      circuit = update(clf, circuit, depth: depth) do |addr, scope, flat|
        running = true

        case flat
        in Inert
        in Gnd
          # Use Gnd#defn (the node's definition) rather than #node here.
          # The hypergraph should only ever see the defn.
          node_id = hg.add!(addr, scope, flat.defn)
          flat.edges.each do |edge|
            hg.join!(node_id, AbsEdge.new(*scope[edge]))
          end
        end

        flat.node # Leave unchanged
      end

      # No nodes in hypergraph => No nodes found at *depth* => We're done.
      break unless running

      patch = yield hg
      next if patch.empty?

      addr_patch = patch.to_h do |node_id, replacement|
        {hg.addr(node_id), replacement}
      end

      circuit = update(clf, circuit, depth: depth) do |addr, scope, flat|
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
  # - Descends into `Mixture`'s definition if *split* is true, otherwise
  #   treats `Mixture`s as `Gnd` nodes.
  # - If *split* is `false`, the address of a node is guaranteed to be the itempath
  #   to that node starting at *circuit*. If *split* is `true`, on the other hand,
  #   the address is not guaranteed to be a valid itempath. In both cases, the addresses
  #   of all nodes uniquely identify them within the same *circuit*.
  def each_feature_with_addr(clf : Classifier, circuit : Term, *, split : Bool = true, &sink : Flat, NodeAddr ->) : Nil
    unless nodes = circuit.as_d?
      return circuit
    end

    each_feature_with_addr(clf, NodeAddr.empty, parent(nodes), split, sink)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Inert | Gnd, split, sink) : Nil
    sink.call(feature, addr)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Mixture, split : Bool, sink) : Nil
    if split
      successor = clf.call(feature.defn)
    else
      successor = gnd(feature.node)
    end

    each_feature_with_addr(clf, addr, successor, split, sink)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Scope, split, sink) : Nil
    each_feature_with_addr(clf, addr, feature.cont, split, sink)
  end

  # :nodoc:
  def each_feature_with_addr(clf, addr, feature : Parent | Circuit, split, sink) : Nil
    items = feature.node.items

    feature.range.each do |index|
      item = items[index]

      each_feature_with_addr(clf, addr.append(index), clf.call(item), split, sink)
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
