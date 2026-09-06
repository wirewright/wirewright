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

    def trim(newsize : Int32) : NodeAddr
      assert newsize <= size

      newaddr = @addr
      (size - newsize).times do
        newaddr = newaddr.prior
      end

      NodeAddr.new(newaddr)
    end
  end

  # Applies *patch* to *hg*'s tree. Returns the resulting patched circuit.
  def apply(hg : Hypergraph, patch : Patch) : Term
    trie = {} of {UInt32, UInt32} => UInt32
    reps = {} of UInt32 => Term
    seq = 1u32 # 0 is root

    patch.each do |node_id, rep|
      node = hg[node_id]

      pred = 0u32 # root
      node.addr.each do |key|
        pred = trie.put_if_absent({pred, key}) do
          seq, _ = seq + 1, seq
        end
      end

      reps[pred] = rep
    end

    guidance = RepairGuidance.new(trie, reps, current: 0u32)
    apply(hg.tree, guidance)
  end

  # :nodoc:
  def apply(tree : ParseTree, guidance : RepairGuidance) : Term
    repair_tree = repair(tree, guidance)
    collapse(repair_tree)
  end

  private struct RepairGuidance
    def initialize(
      @trie : Hash({UInt32, UInt32}, UInt32),
      @reps : Hash(UInt32, Term),
      @current : UInt32,
    )
    end

    def []?(key : UInt32) : RepairGuidance?
      return unless successor = @trie[{@current, key}]?

      RepairGuidance.new(@trie, @reps, successor)
    end

    def has_rep? : Bool
      @reps.has_key?(@current)
    end

    def rep? : Term?
      @reps[@current]?
    end
  end

  private def repair(tree : InertLeaf | GndLeaf, guidance : RepairGuidance) : RepairTree
    guidance.rep? || tree.feature.node
  end

  private def repair(tree : ScopeNode | MixtureNode, guidance : RepairGuidance) : RepairTree
    repair(tree) { |child| repair(child, guidance) }
  end

  private def repair(tree : CircuitNode, guidance : RepairGuidance) : RepairTree
    if guidance.has_rep?
      return repair(tree.leaf, guidance)
    end

    treatment = GroupNode.new(parent(tree.feature.node, tree.feature.range), tree.children)
    repair(treatment, guidance)
  end

  private def repair(tree : GroupNode, guidance : RepairGuidance) : RepairTree
    repair(tree) do |child, index|
      key = tree.feature.range.begin + index
      successor = guidance[key]?
      successor ? repair(child, successor) : unchanged(child)
    end
  end

  private def unchanged(tree : InertLeaf | GndLeaf | CircuitNode | ParentNode | MixtureNode) : Term
    Term.of(tree.feature.node)
  end

  private def unchanged(tree : ScopeNode) : Term
    unchanged(tree.child)
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
  def step(parser : Parser, circuit : Term, required_heads : Indexable(Term) = Slice(Term).empty, &) : Slice(Term)
    tree = parser.parse(circuit)
    hg = Hypergraph.new(tree, 0)
    if required_heads.present? && required_heads.none? { |head| D7.summary(hg.tree).has_head?(head) }
      return Slice[circuit]
    end

    substeps = Pf::Kit.stack_array(Term, 8)
    substeps << circuit

    MAX_SUBSTEPS.times do |level|
      patch = yield hg

      pass do
        next if patch.empty?

        circuit = apply(hg, patch)
        next if substeps.last == circuit

        substeps << circuit
        tree = parser.parse(circuit)
      end

      hg = Hypergraph.new(tree, level + 1)

      # No nodes found at the next *level* => We're done.
      break if hg.bottom?
    end

    substeps.to_readonly_slice(&.itself)
  end
end
