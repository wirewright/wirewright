module Ww::D7
  # Applies *patch* to *hg*'s tree. Returns the resulting patched circuit.
  def apply(hg : Hypergraph, patch : Patch) : Term
    guidance = guidance(patch) do |(node_id, rep)|
      node = hg[node_id]
      {node.addr, rep}
    end

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

  def guidance(objects : Enumerable(T), & : T -> {NodeAddr, Term}) : RepairGuidance forall T
    trie = {} of {UInt32, UInt32} => UInt32
    reps = {} of UInt32 => Term
    seq = 1u32 # 0 is root

    objects.each do |object|
      addr, rep = yield object

      pred = 0u32 # root
      addr.each do |key|
        pred = trie.put_if_absent({pred, key}) do
          seq, _ = seq + 1, seq
        end
      end

      reps[pred] = rep
    end

    RepairGuidance.new(trie, reps, current: 0u32)
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

    treatment = tree.to_group
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

  alias GatherLevel = Array({NodeAddr, GroupNode})

  def gather(level : GatherLevel, tree : ParseTree, target_depth : UInt32) : Nil
    addr = NodeAddr.empty
    gather(level, addr, tree, target_depth)
  end

  # :nodoc:
  def gather(level : GatherLevel, addr : NodeAddr, tree : GndLeaf | InertLeaf, target_depth : UInt32) : Nil
  end

  # :nodoc:
  def gather(level : GatherLevel, addr : NodeAddr, tree : MixtureNode | ScopeNode, target_depth : UInt32) : Nil
    gather(level, addr, tree.child, target_depth)
  end

  # :nodoc:
  def gather(level : GatherLevel, addr : NodeAddr, tree : GroupNode, target_depth : UInt32) : Nil
    if target_depth.zero?
      level << {addr, tree}
      return
    end

    # Maxlevel is never 0. It is at least one. More than one if there are subcircuits.
    # We need to simulate subcircuits only if there are subcircuits!
    return if D7.maxlevel(tree) <= 1

    tree.children.each_with_index(offset: tree.feature.range.begin) do |child, key|
      gather(level, addr.append(key), child, target_depth)
    end
  end

  # :nodoc:
  def gather(level : GatherLevel, addr : NodeAddr, tree : CircuitNode, target_depth : UInt32) : Nil
    if target_depth.zero?
      level << {addr, tree.to_group}
      return
    end

    gather(level, addr, tree.to_group, target_depth - 1)
  end

  # :nodoc:
  def broadcast(changes : Deque({GroupNode, Term}), tree : GndLeaf | InertLeaf, target_depth : UInt32) : Term
    tree.feature.node # unchanged
  end

  # :nodoc:
  def broadcast(changes : Deque({GroupNode, Term}), tree : MixtureNode, target_depth : UInt32) : Term
    tree.feature.mix.call(broadcast(changes, tree.child, target_depth))
  end

  # :nodoc:
  def broadcast(changes : Deque({GroupNode, Term}), tree : ScopeNode, target_depth : UInt32) : Term
    broadcast(changes, tree.child, target_depth)
  end

  # :nodoc:
  def broadcast(changes : Deque({GroupNode, Term}), tree : GroupNode, target_depth : UInt32) : Term
    # If there are no changes left, we can safely unwind and ignore the rest of nodes.
    unless change = changes.first?
      return Term.of(tree.feature.node) # unchanged
    end

    change_group, change_term = change

    if target_depth.zero?
      if change_group.same?(tree)
        changes.shift
        return change_term
      end
      return Term.of(tree.feature.node) # unchanged
    end

    # Maxlevel is never 0. It is at least one. More than one if there are subcircuits.
    # We need to simulate subcircuits only if there are subcircuits!
    if D7.maxlevel(tree) < target_depth
      return Term.of(tree.feature.node) # unchanged
    end

    result = tree.feature.node.transaction do |commit|
      tree.children.each_with_index(offset: tree.feature.range.begin) do |child, key|
        commit.with(key, broadcast(changes, child, target_depth))
      end
    end

    Term.of(result)
  end

  # :nodoc:
  def broadcast(changes : Deque({GroupNode, Term}), tree : CircuitNode, target_depth : UInt32) : Term
    # If there are no changes left, we can safely unwind and ignore the rest of nodes.
    unless change = changes.first?
      return Term.of(tree.feature.node) # unchanged
    end

    change_group, change_term = change

    if target_depth.zero?
      if change_group.same?(tree)
        changes.shift
        return change_term
      end
      return Term.of(tree.feature.node) # unchanged
    end

    broadcast(changes, tree.to_group, target_depth - 1)
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

  # Evolves *circuit* (the *previous frame*) by one time-step  Returns the resulting
  # sequence of substeps, the last of which is the *next frame* -- the *circuit* at t+1.
  #
  # The `step` algorithm runs a top-down iterative-deepening circuit traversal in
  # which ground nodes at each consecutive *level* are assembled into a hypergraph.
  #
  # The hypergraph is then inspected by *fn* to obtain a patch.
  #
  # The `step` algorithm applies the patch to *circuit*, producing a *level-patched
  # circuit*. It then deepens. The sequence of ever so deeply level-patched circuits
  # forms the returned sequence of substeps.
  def step(parser : Parser, circuit : Term, required_heads : Indexable(Term) = Slice(Term).empty, &fn : Hypergraph, Array(Patch) ->) : Slice(Term)
    tree = parser.parse(circuit)
    if required_heads.present? && required_heads.none? { |head| D7.summary(tree).has_head?(head) }
      return Slice[circuit]
    end

    substeps = Pf::Kit.stack_array(Term, 8)
    substeps << circuit

    level = [] of {NodeAddr, GroupNode}
    changes = Deque({GroupNode, Term}).new

    # Reused in different calls to fn.
    patches = [] of Patch

    MAX_SUBSTEPS.times do |target_depth|
      assert level.empty?
      assert changes.empty?

      gather(level, tree, target_depth)
      break if level.empty?

      # TODO: in my sweetest dreams this is a parallel each. Currently though fn()
      # is not guaranteed to be thread-safe, nor is the choice of whether to go
      # parallel so easy. We'd need heuristics since it's not always cheap.
      level.each do |(addr, tree)|
        hg = Hypergraph.new(addr, tree, Hypergraph::CurrentLevel.new)
        fn.call(hg, patches)
        next if patches.empty?

        patch = D7.merge(hg, patches)
        patches.clear
        next if patch.empty?

        changes << {tree, D7.apply(hg, patch)}
      end

      level.clear
      next if changes.empty?

      circuit = broadcast(changes, tree, target_depth)
      substeps << circuit
      assert changes.empty?, "broadcast() should have cleared changes"

      # If the patch changed something, reparse to obtain the new tree. This heavily
      # relies on the parser's cache for performance.
      tree = parser.parse(circuit)
    end

    substeps.to_unsafe_readonly_slice!
  end
end
