module Ww::D7
  # Represents the address of a node in a circuit.
  #
  # Node addresses are sequences of item keys to follow to reach the referenced
  # node in the circuit term.
  struct NodeAddr
    include Indexable(UInt32)

    # :nodoc:
    def initialize(@addr : Pf::UPath32)
    end

    def self.new(objects : Enumerable(T), & : T -> UInt32) : NodeAddr forall T
      objects.reduce(empty) { |addr, object| addr.append(yield object) }
    end

    def self.empty : NodeAddr
      NodeAddr.new(Pf::UPath32[])
    end

    def self.[](*keys : UInt32) : NodeAddr
      new(keys, &.itself)
    end

    # Lexicographical comparison of two node addresses.
    def <=>(other : NodeAddr)
      compare(other) { |key0, key1| key0 <=> key1 }
    end

    def size : Int32
      @addr.size
    end

    def unsafe_fetch(index : Int) : UInt32
      @addr[index]
    end

    def append(key : UInt32) : NodeAddr
      NodeAddr.new(@addr.append(key))
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

    def resolve(edge : Term) : AbsEdge
      trace = @trace

      while entry = trace.last?
        addr, scope = entry

        case scope
        in OpenExcept
          if edge.in?(scope.edges)
            # In Rack:
            #  (local (⏏@dst⏏) ;; <<- WE ARE HERE, @dst found, so it's a local!
            #    (feed @src ⏏@dst⏏)
            return AbsEdge.new(addr, edge)
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
            return AbsEdge.new(addr, edge)
          end

          # In Rack:
          #  (module {⏏@x⏏: @y} ;; @x found, its *exterior* is the outerly-scoped @y.
          #    (feed @a ⏏@x⏏)
          edge = exterior
        end

        trace = trace[...-1]
      end

      AbsEdge.new(NodeAddr.empty, edge)
    end

    def append(addr : NodeAddr, scope : Any) : NodeScope
      NodeScope.new(@trace.append({addr, scope}))
    end
  end

  # :nodoc:
  def update(parser : Parser, circuit : Term, level : Int, &fn : NodeAddr, Flat -> Term) : Term
    feature_tree = parser.parse(circuit, reply: ParseTree)
    update(feature_tree, level, &fn)
  end

  # :nodoc:
  def update(tree : ParseTree, level : Int, &fn : NodeAddr, Flat -> Term) : Term
    assert level >= 0

    repair_tree = update(NodeAddr.empty, tree, level, fn)
    collapse(repair_tree)
  end

  private def update(addr, tree : InertLeaf | GndLeaf, level, fn) : RepairTree
    if level.zero?
      return fn.call(addr, tree.feature)
    end

    tree.feature.node
  end

  private def update(addr, tree : ScopeNode, level, fn) : RepairTree
    repair(tree) do |child|
      update(addr, child, level, fn)
    end
  end

  private def update(addr, tree : MixtureNode, level, fn) : RepairTree
    repair(tree) do |child|
      update(addr, child, level, fn)
    end
  end

  private def update(addr, tree : CircuitNode, level, fn) : RepairTree
    if level.zero?
      return update(addr, tree.leaf, level, fn)
    end

    assert level > 0

    if maxlevel(tree) < level
      # This branch cannot possibly contain circuits at the target level.
      return Term.of(tree.feature.node)
    end

    # NOTE: Circuits must surround themselves with scopes to seal themselves off
    # from the outside world completely. Otherwise, two circuits with the same
    # level would be able to communicate, and that would go against our semantics.
    #
    #   ;; Must NOT work!
    #   (circuit @0 (cell @x 100))
    #   (circuit @1 (cell @y))
    #   (circuit @2 (feed @x @y))
    #
    treatment = GroupNode.new(parent(tree.feature.node, tree.feature.range), tree.children)
    update(addr, treatment, level - 1, fn)
  end

  private def update(addr, tree : GroupNode, level, fn) : RepairTree
    if maxlevel(tree) < level
      # This branch cannot possibly contain circuits at the target level.
      return Term.of(tree.feature.node)
    end

    repair(tree) do |child, index|
      key = tree.feature.range.begin + index
      update(addr.append(key), child, level, fn)
    end
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
  MAX_SUBSTEPS = 512u32

  # Executes one time-step on *circuit* (the *previous frame*). Returns
  # the resulting sequence of substeps, the last of which is the *next frame* --
  # *circuit* at t+1.
  #
  # The `step` algorithm runs a top-down iterative-deepening circuit traversal in
  # which ground nodes at each consecutive *level* are assembled into a hypergraph.
  #
  # The hypergraph is then solved by the block to obtain a patch (see `Regime#solve`
  # for relevant code).
  #
  # The `step` algorithm applies the patch, producing a *level*-patched *circuit*.
  # It then deepens. Each level-patched circuit is recorded as a substep, forming
  # the resulting sequence of substeps.
  #
  # Replacement proceeds top-down (see `D7` for reasoning).
  #
  # See `D7` for terminology (e.g. subframe vs. substep).
  def step(parser : Parser, circuit : Term, &) : Slice(Term)
    substeps = Pf::Kit.stack_array(Term, 8)

    MAX_SUBSTEPS.times do |level|
      substeps << circuit

      tree = parser.parse(circuit, reply: D7::ParseTree)
      hg = Hypergraph.new(tree, level)

      # No nodes in hypergraph => No nodes found at *level* => We're done.
      break if hg.bottom?

      patch = yield hg
      next if patch.empty?

      # TODO: Remove this when it'd be possible to get rid of nodeids. We can
      # just use nodeaddrs. There's no need for nodeids.
      addr_patch = patch.to_h do |node_id, replacement|
        {hg[node_id].addr, replacement}
      end

      circuit = update(parser, circuit, level) do |addr, flat|
        case flat
        in Inert then flat.node
        in Gnd   then addr_patch[addr]? || flat.node
        end
      end
    end

    substeps.to_readonly_slice(&.itself)
  end
end
