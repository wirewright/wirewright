module Ww::D7
  # A classifier function "looks" at a circuit node term (more or less literally,
  # but using pattern matching instead of a pair of "eyes"). It decides what
  # the node's semantic function is (what the node "means"), primarily its
  # *structural* role, and represents the decision using one of `Feature`s.
  #
  # By *structural role*, I mean answers to questions such as "Should I descend
  # here or leave it as-is?", "Should I ignore this or pass it to the solver?".
  alias Classifier = Term -> Feature

  alias Feature = Flat | Mixture | Scope | Parent | Circuit

  # Features without a successor.
  alias Flat = Gnd | Inert

  # Lists annotations that can be used on an inert node.
  @[Flags]
  enum InertAnnotationSet
    Incomplete
  end

  # Represents an inert (data) node.
  defrecord Inert, node : Term, annotations : InertAnnotationSet

  # Constructs a no-operation or *inert* data node -- with respect to the regime,
  # which won't see such nodes. They may still carry useful info or be useful
  # themselves -- for instance, all graphical nodes and surfaces end up as *inert*.
  #
  # *annotations* can be used for semantic tagging.
  def inert(node : Term, annotations : Tuple = Tuple.new) : Inert
    Inert.new(node, InertAnnotationSet.with(annotations))
  end

  # :ditto:
  def inert(node : Term, annotations : InertAnnotationSet) : Inert
    Inert.new(node, annotations)
  end

  # Ground nodes form the hypergraph that is solved by a D7 `Regime`.
  #
  # Ground nodes can have a different, solver-oriented "look", defined
  # by *defn*. This is similar to `Mixture`, except for two things.
  #
  # First, with `Gnd`, *defn* is discarded after use by a regime; whereas
  # with Mixture, it is reinterpreted back into the original "look" by
  # the mix function. Here, in `Gnd`, the original look is saved under *node*,
  # and it is used on ascent without further processing.
  #
  # Second, `Gnd`'s *defn* is not treated any futher; whereas `Mixture`'s defn
  # receives recursive treatment.
  defrecord Gnd,
    node : Term,
    defn : Term,
    head : Term,
    edges : Set(Term)

  # Constructs a grounded node from an enumerable of *edges*.
  #
  # See `Gnd`.
  #
  # NOTE: If *edges* is a `Set`, it will be reused! Make sure to not mutate it;
  # if you will, create a copy and pass the copy instead.
  def gnd(node : Term, edges : Enumerable(Term), *, defn : Term = node) : Gnd
    unless head = node.as_d?.try(&.items.first?)
      raise ArgumentError.new("could not determine the head of node")
    end

    Gnd.new(node, defn, head, edges.as?(Set(Term)) || edges.to_set)
  end

  # Constructs a grounded node with the given *edges*.
  #
  # See `Gnd`.
  def gnd(node : Term, *edges : Term, **kwargs) : Gnd
    gnd(node, edges, **kwargs)
  end

  # Constructs a grounded node without edges.
  #
  # See `Gnd`.
  def gnd(node : Term, **kwargs) : Gnd
    gnd(node, **kwargs, edges: Slice(Term).empty)
  end

  # A decomposition of *node* into a definition *defn* with a *mix* function
  # to compose rewritten *defn* back into the next version of *node*.
  defrecord Mixture, node : Term, defn : Term, mix : Term -> Term

  # Constructs a mixture feature.
  #
  # See `Mixture`.
  def mixture(node : Term, defn, &mix : Term -> Term) : Mixture
    Mixture.new(node, Term.of(defn), mix)
  end

  # Wraps a continuation feature *cont* in a lexical scope qualified by *scope*.
  defcase Scope, scope : NodeScope, cont : Feature

  # Constructs a scope feature.
  #
  # See `Scope`.
  def scope(cont : Feature, *, bindings : Term::Dict) : Scope
    Scope.new(ScopeClosedExcept.new(bindings), cont)
  end

  # :ditto:
  def scope(cont : Feature, *, locals : Indexable(Term)) : Scope
    Scope.new(ScopeOpenExcept.new(locals.to_readonly_slice(&.itself)), cont)
  end

  alias NodeScope = ScopeOpenExcept | ScopeClosedExcept

  defrecord ScopeOpenExcept, edges : Slice(Term)
  defrecord ScopeClosedExcept, bindings : Term::Dict

  # A sentinel value used in `Parent#passable` to state that a parent is
  # passable. Some optimizations rely on this since they cannot look "into"
  # a `PassablePredicate` to determine whether it is something like `-> { true }`.
  module Passable
    extend self

    def call(hg : Hypergraph, addr : NodeAddr) : Bool
      true
    end
  end

  # Uses a function to determine whether a parent is passable.
  alias PassablePredicate = Hypergraph, NodeAddr -> Bool

  # Represents the children nodes of *node* found within an exclusive positive
  # range of its items.
  #
  # Used by e.g. `group`, `module`.
  defrecord Parent,
    node : Term::Dict,
    range : Range(UInt32, UInt32),
    passable : Passable.class | PassablePredicate

  # Constructs a parent feature.
  #
  # See `Parent`.
  def parent(node : Term::Dict)
    parent(node, 0u32...node.uitemsize)
  end

  # :ditto:
  def parent(node : Term::Dict, range : Range(UInt32, UInt32)) : Parent
    assert range.exclusive?

    Parent.new(node, range, Passable)
  end

  # :ditto:
  def parent(node : Term::Dict, range : Range(UInt32, UInt32), &passable : PassablePredicate) : Parent
    assert range.exclusive?

    Parent.new(node, range, passable)
  end

  # Used primarily by the `circuit` node; represents an isolated, nested
  # circuit whose children (as defined by *range*) are evaluated using
  # iterative deepening.
  #
  # *leaf* defines leaf treatment, when this circuit is among the nodes
  # at the target depth (so evaluation stops at it, without descending
  # into *range*).
  #
  # Importantly, it makes little sense for *leaf* to contain circuits recursively.
  # Even though they are permitted by the types and will work, the way they will
  # work is rather degenerate -- the evaluator will simply proceed into their
  # *leaf*s in turn; never considering them as circuits. In other words, if
  # *leaf* contains or is a circuit, that circuit is always also treated as a leaf,
  # and so its *leaf* is used, and so on, until some sort of base case.
  defcase Circuit,
    node : Term::Dict,
    range : Range(UInt32, UInt32),
    leaf : Feature

  # Constructs a circuit feature.
  #
  # See `Circuit`.
  def circuit(node : Term::Dict, range : Range(UInt32, UInt32), leaf : Feature) : Circuit
    assert range.exclusive? && range.subrange_of?(0u32...node.uitemsize)

    Circuit.new(node, range, leaf)
  end

  alias ParseTree = InertLeaf | GndLeaf | MixtureNode | ScopeNode | ParentNode

  defcase InertLeaf, feature : Inert

  defcase GndLeaf, feature : Gnd do
    @levels : Atomic(LevelSummary*) = Atomic.new(Pointer(LevelSummary).null)

    def levels : Slice(LevelSummary)
      if levelsptr = @levels.get(:acquire) # not null
        return Slice(LevelSummary).new(levelsptr, 1, read_only: true)
      end

      levels = Slice[LevelSummary.new(1u32, Set{feature.head}, feature.edges)]
      @levels.set(levels.to_unsafe, :release)

      levels
    end
  end

  defcase MixtureNode,
    feature : Mixture,
    child : ParseTree

  class ScopeNode
    getter feature : Scope
    getter child : InertLeaf | GndLeaf | MixtureNode | ParentNode

    def initialize(@feature, child : ParseTree)
      # Our lookup algorithm(s) rely on this so let's make ScopeNode maintain
      # this as an invariant.
      assert ScopeNode.has_addr?(child), "scope node child must have an address"

      @child = child.as(InertLeaf | GndLeaf | MixtureNode | ParentNode)
    end

    # :nodoc:
    def self.has_addr?(node : ScopeNode) : Bool
      false
    end

    # :nodoc:
    def self.has_addr?(node : MixtureNode) : Bool
      has_addr?(node.child)
    end

    # :nodoc:
    def self.has_addr?(node : InertLeaf | GndLeaf | ParentNode) : Bool
      true
    end

    def_equals_and_hash @feature, @child
  end

  alias ParentNode = GroupNode | CircuitNode

  defcase GroupNode,
    feature : Parent,
    children : Slice(ParseTree),
    levels : Slice(LevelSummary)

  defcase CircuitNode,
    feature : Circuit,
    children : Slice(ParseTree),
    leaf : ParseTree,
    levels : Slice(LevelSummary)

  # Smart constructor for `GroupNode`.
  def GroupNode.new(feature : Parent, children : Slice(ParseTree)) : GroupNode
    # An empty group, for example `(group)`.
    if children.empty?
      return GroupNode.new(feature, children, INERT_LEVELS)
    end

    if child = children.single?
      return GroupNode.new(feature, children, D7.levels(child))
    end

    maxlevel = children.max_of { |child| D7.maxlevel(child) }

    levels = (0...maxlevel).to_readonly_slice do |depth|
      children.reduce(LevelSummary.new) do |memo, child|
        level_index = -(maxlevel - depth)
        level = D7.levels(child)[level_index]?
        level ||= LevelSummary.new
        LevelSummary.union(memo, level)
      end
    end

    GroupNode.new(feature, children, levels)
  end

  # Smart constructor for `CircuitNode`.
  def CircuitNode.new(feature : Circuit, children : Slice(ParseTree), leaf) : CircuitNode
    leaf_levels = D7.levels(leaf)

    # FIXME: In theory we'd actually want to merge leaf_levels#prior into
    # children levels and then append leaf_levels#last to  the result. In
    # practice, however, `Circuit#leaf`s never contain subcircuits and they
    # wouldn't work either way; so I doubt it's worth spending the effort here.
    leaf_level = leaf_levels.last

    if children.empty?
      levels = Slice[LevelSummary.new, leaf_level]
      return CircuitNode.new(feature, children, leaf, levels)
    end

    if child = children.single?
      levels = D7.levels(child).append(leaf_level)
      return CircuitNode.new(feature, children, leaf, levels)
    end

    maxlevel = children.max_of { |child| D7.maxlevel(child) }

    levels = (0...maxlevel + 1).to_readonly_slice do |depth|
      # Append leaf_level.
      if depth == maxlevel
        next leaf_level
      end

      children.reduce(LevelSummary.new) do |memo, child|
        level_index = -(maxlevel - depth)
        level = D7.levels(child)[level_index]?
        level ||= LevelSummary.new
        LevelSummary.union(memo, level)
      end
    end

    assert levels.size >= 2

    CircuitNode.new(feature, children, leaf, levels)
  end

  # TODO: For heads we can use a large-ish Bloom filter. Things like
  # set of Term are already 64B+ of pure control overhead, we can safely use memory
  # sizes of that kind of magnitude for the bitmap (512-1024 bits).
  defrecord LevelSummary,
    population : UInt32,
    heads : Set(Term),
    edges : Set(Term)

  # :nodoc:
  LevelSummary::EMPTY = LevelSummary.new(0u32, Set(Term).new, Set(Term).new)

  # Constructs an empty level summary.
  def LevelSummary.new : LevelSummary
    EMPTY
  end

  # Returns the union of two level summaries.
  def LevelSummary.union(a : LevelSummary, b : LevelSummary) : LevelSummary
    LevelSummary.new(
      a.population + b.population,
      a.heads | b.heads,
      a.edges | b.edges,
    )
  end

  # :nodoc:
  INERT_LEVELS = Slice[LevelSummary.new]

  # :nodoc:
  def levels(tree : InertLeaf) : Slice(LevelSummary)
    INERT_LEVELS
  end

  # :nodoc:
  def levels(tree : ScopeNode) : Slice(LevelSummary)
    child_levels = levels(tree.child)
    child_level = child_levels.last

    edges = Set(Term).new

    case scope = tree.feature.scope
    in ScopeOpenExcept
      child_level.edges.each do |edge|
        # If edge is absent in edges, it's local, so it does not propagate outwards.
        next if edge.in?(scope.edges)

        edges << edge
      end
    in ScopeClosedExcept
      child_level.edges.each do |edge|
        # If no exterior is defined, the edge is local to the scope, and
        # does not propagate outwards.
        next unless exterior = scope.bindings[edge]?

        edges << exterior
      end
    end

    child_level = LevelSummary.new(child_level.population, child_level.heads, edges)
    child_levels.prior.append(child_level)
  end

  # :nodoc:
  def levels(tree : MixtureNode) : Slice(LevelSummary)
    levels(tree.child)
  end

  # :nodoc:
  def levels(tree : GndLeaf | GroupNode | CircuitNode) : Slice(LevelSummary)
    tree.levels
  end

  {% if flag?(:docs) %}
    # Returns level summaries for levels of *tree*.
    #
    # NOTE: The **last** summary is root-most.
    def levels(tree : ParseTree) : Slice(LevelSummary)
    end
  {% end %}

  # Returns the circuit depth of *tree*. It is at least `1` (toplevel circuit only),
  # but could be larger than one (e.g. `2` means toplevel circuit with subcircuits,
  # `3` means toplevel circuits + subcircuits + sub-subcircuits, etc.)
  def maxlevel(tree : ParseTree) : Int32
    case tree
    in InertLeaf, GndLeaf     then 1
    in ScopeNode, MixtureNode then maxlevel(tree.child)
    in GroupNode, CircuitNode then tree.levels.size
    end
  end

  # Calculates the population statistic for *tree* (the number of ground
  # nodes in it, irrespective of passability, level, etc). The latter means
  # children of impassable groups count, as do children and leaf representations
  # of circuits.
  def population(tree : ParseTree) : UInt32
    case tree
    in InertLeaf
      0u32
    in GndLeaf
      1u32
    in GroupNode, CircuitNode
      tree.levels.sum(&.population)
    in ScopeNode, MixtureNode
      population(tree.child)
    end
  end

  # Returns `true` if one or more nodes with the given *head* exist in *tree*.
  def head?(tree : ParseTree, head : Term) : Bool
    case tree
    in InertLeaf
      false
    in GndLeaf
      tree.head == head
    in GroupNode, CircuitNode
      tree.levels.present? && head.in?(tree.levels.last.heads)
    in ScopeNode, MixtureNode
      head?(tree.child, head)
    end
  end

  # A thin wrapper around `D7.parse` that manages parse caching.
  #
  # Parse caching (parse memoization) is a very important optimization.
  # Just like in UI, in D7/Rack, the successive evolution of a circuit
  # is almost exactly the same as that circuit, with very minor differences.
  # So reparsing circuits from scratch throughout their evolution would
  # be very wasteful. Insted, we (generally) use a `GenerationalCache`.
  #
  # A parse cache is inextricably linked to a `Classifier`. Using multiple
  # different classifiers with the same cache is not recommended and will
  # almost always cause bugs (on your end, not on `D7`'s!)
  struct Parser
    # Returns the classifier used by this parser.
    getter clf : Classifier

    # Constructs a parser which uses the given classifier *clf* and a custom *cache*
    # (not necessarily a generational cache).
    def initialize(@clf : Classifier, @cache : ICache(Term, ParseTree))
    end

    def self.new(clf : Classifier) : Parser
      new(clf, cache: GenerationalCache(Term, ParseTree).new)
    end

    # Returns the parse tree for *circuit*.
    #
    # See `D7.parse` for more info.
    def parse(circuit : Term) : ParseTree
      @cache.epoch do
        D7.parse(@clf, circuit, cache: @cache)
      end
    end
  end

  # Uses the classifier *clf* to convert a *circuit* into a parse tree.
  #
  # *circuit* is considered a `Parent` if it is a dict. You can provide
  # an explicit child *range* to pass through to the `Parent`; by default,
  # the range includes all items of *circuit*.
  #
  # NOTE: See `Parser` for more info on *cache*.
  def parse(clf : Classifier, circuit : Term, *, cache = Uncached(Term, ParseTree).new, range : Range(UInt32, UInt32)? = nil)
    unless nodes = circuit.as_d?
      return InertLeaf.new(feature: inert(circuit))
    end

    # Assuming you can't embed a circuit inside itself, of course ... Which you can't.
    cache.put_if_absent(circuit) do
      parse(clf, cache, parent(nodes, range || (0u32...nodes.uitemsize)))
    end
  end

  private def parse(clf, cache, feature : Inert)
    InertLeaf.new(feature)
  end

  private def parse(clf, cache, feature : Gnd)
    GndLeaf.new(feature)
  end

  private def parse(clf, cache, feature : Mixture)
    child = parse(clf, cache, clf.call(feature.defn))
    MixtureNode.new(feature, child)
  end

  private def parse(clf, cache, feature : Scope)
    child = parse(clf, cache, feature.cont)
    ScopeNode.new(feature, child)
  end

  private def parse(clf, cache, feature : Parent) : ParentNode
    children = parse(clf, cache, feature.range, feature.node)
    GroupNode.new(feature, children)
  end

  private def parse(clf, cache, feature : Circuit) : ParentNode
    children = parse(clf, cache, feature.range, feature.node)
    leaf = parse(clf, cache, feature.leaf)
    CircuitNode.new(feature, children, leaf)
  end

  private def parse(clf, cache, range : Range(UInt32, UInt32), node : Term::Dict)
    assert range.exclusive?
    assert range.begin <= range.end

    range.to_readonly_slice { |index| parse(clf, cache, node[index]) }
  end

  private def parse(clf, cache, term : Term)
    cache.put_if_absent(term) do
      parse(clf, cache, clf.call(term))
    end
  end

  # Replaces `GndLeaf` nodes in *tree* according to *replacements*.
  #
  # HACK: Avoid this function if you can because it makes ground nodes and their
  # parents go out of sync with `Term`s stored in `Feature#node`. The only use
  # case where `gnd_map` is appropriate is when you want to make a "shadow"
  # replacement of a ground node and you can guarantee that you'll discard
  # the returned parse tree eventually instead of `repair`ing it! Basically,
  # by `gnd_map`ing a *tree*, you invalidate all `Feature#node`s in it; so if
  # you plan on using them, you shouldn't `gnd_map`!
  def gnd_map(tree : ParseTree, replacements : Hash(NodeAddr, Gnd)) : ParseTree
    gnd_map(NodeAddr.empty, tree, replacements)
  end

  private def gnd_map(addr, tree : InertLeaf, replacements) : ParseTree
    tree
  end

  private def gnd_map(addr, tree : GndLeaf, replacements) : ParseTree
    if feature = replacements[addr]?
      return GndLeaf.new(feature)
    end

    tree
  end

  private def gnd_map(addr, tree : MixtureNode, replacements) : ParseTree
    child1 = gnd_map(addr, tree.child, replacements)
    if tree.child.same?(child1)
      return tree # unchanged
    end

    MixtureNode.new(tree.feature, child1)
  end

  private def gnd_map(addr, tree : ScopeNode, replacements) : ParseTree
    child1 = gnd_map(addr, tree.child, replacements)
    if tree.child.same?(child1)
      return tree # unchanged
    end

    ScopeNode.new(tree.feature, child1)
  end

  # :nodoc:
  GND_REPLACEMENTS_SMALL = 16

  private def gnd_map(addr, tree : GroupNode | CircuitNode, replacements) : ParseTree
    # Prune this branch if no replacements talk about it. Most often replacements
    # is very small so this should be cheap enough versus traversal down to
    # ground nodes.
    #
    # NOTE: If *tree* is impassable, we expect no replacements to exist for nodes
    # inside it. If some do, well, we carry them out...
    if replacements.size < GND_REPLACEMENTS_SMALL
      possibly_contains = false

      replacements.each_key do |replacement_addr|
        next unless replacement_addr.starts_with?(addr)
        possibly_contains = true
        break
      end

      unless possibly_contains
        return tree
      end
    end

    changed_indices = Pf::Kit.stack_array(Int32, 8)
    changed_children = Pf::Kit.stack_array(ParseTree, 8)

    tree.children.each_with_index do |child0, child_index|
      key = tree.feature.range.begin + child_index
      child1 = gnd_map(addr.append(key), child0, replacements)
      next if child0.same?(child1)

      changed_indices << child_index
      changed_children << child1
    end

    if changed_indices.empty?
      return tree # unchanged
    end

    # Apply changes to a mutable copy of children.
    children1 = tree.children.dup
    changed_children.zip(changed_indices) do |child, child_index|
      children1[child_index] = child
    end

    # Make the copy read-only.
    children1 = Slice.new(children1.to_unsafe, children1.size, read_only: true)

    case tree
    in CircuitNode
      CircuitNode.new(tree.feature, children1, gnd_map(addr, tree.leaf, replacements))
    in GroupNode
      GroupNode.new(tree.feature, children1)
    end
  end

  def follow?(hg : Hypergraph, tree : ParseTree, addr : NodeAddr) : {NodeId, ParseTree}?
    id_zero = NodeId.new(0)
    prefix = NodeAddr.empty

    addr.each do |index|
      loop do
        case tree
        in InertLeaf, GndLeaf
          return
        in ScopeNode, MixtureNode
          tree = tree.child
          next
        in GroupNode, CircuitNode
          # Edges defined inside impassable groups are unreachable.
          if tree.is_a?(GroupNode)
            predicate = tree.feature.passable
            return unless predicate.call(hg, prefix)
          end

          offset = tree.feature.range.begin
          assert index >= offset

          # Do not forget to shift the id of the child by all prior ids.
          prior = tree.children.trim(index - offset)
          prior.each do |child|
            id_zero += D7.population(child)
          end

          tree = tree.children[index - offset]
        end

        break
      end

      prefix = prefix.append(index)
    end

    {id_zero, tree}
  end

  def follow(hg : Hypergraph, tree : ParseTree, addr : NodeAddr) : {NodeId, ParseTree}
    follow?(hg, tree, addr) || raise KeyError.new
  end

  # `ParseTree`s are usually rewritten; we call that *repair*, and represent
  # its result with `RepairTree`.
  #
  # - `InertLeaf` and `GndLeaf` must become a `Term`.
  # - `MixtureNode` must become `MixtureRepair` or `Term` if not changed.
  # - `ScopeNode` must be omitted, since it is purely an augmentation; not an
  #   actual, structural node.
  # - `ParentNode` or `CircuitNode` must become `ParentRepair`.
  #
  # Most complexities of the above are handled by the set of `repair`
  # utility functions. You are advised to use them instead of employing
  # custom logic for achieving the above.
  alias RepairTree = Term | MixtureRepair | ParentRepair

  defcase MixtureRepair, feature : Mixture, child : RepairTree
  defrecord ParentRepair, feature : Parent | Circuit, children : Slice(RepairTree)

  private def changed?(before : GndLeaf | InertLeaf | MixtureNode | ParentNode, after : Term) : Bool
    before.feature.node != after
  end

  # Fallback
  private def changed?(before : ParseTree, after) : Bool
    true
  end

  # Handles repair of a `MixtureNode`. Yields its child to the block for repair.
  def repair(tree : MixtureNode, & : ParseTree -> RepairTree) : RepairTree
    child = yield tree.child

    if changed?(before: tree.child, after: child)
      return MixtureRepair.new(tree.feature, child)
    end

    tree.feature.node # unchanged
  end

  # Handles repair of a `ScopeNode`. Yields its child to the block for repair.
  def repair(tree : ScopeNode, & : ParseTree -> RepairTree) : RepairTree
    yield tree.child
  end

  # Handles repair of a `ParentNode`. Yields each child to the block for repair.
  def repair(tree : ParentNode, &) : RepairTree
    children = Pf::Kit.stack_array(RepairTree)
    changed = false

    tree.children.each_with_index do |child0, index|
      child1 = yield child0, index
      children << child1
      changed ||= changed?(before: child0, after: child1)
    end

    if changed
      return ParentRepair.new(tree.feature, children.to_unsafe_readonly_slice!)
    end

    Term.of(tree.feature.node)
  end

  # :nodoc:
  def collapse(repair : Term) : Term
    repair
  end

  # :nodoc:
  def collapse(repair : ParentRepair) : Term
    node = repair.feature.node

    result = node.transaction do |commit|
      repair.children.zip(repair.feature.range) do |child, index|
        item = node[index]
        commit.with(index, collapse(child))
      end
    end

    Term.of(result)
  end

  # :nodoc:
  def collapse(repair : MixtureRepair) : Term
    mix = repair.feature.mix
    mix.call(collapse(repair.child))
  end

  {% if flag?(:docs) %}
    # Finalizes repairs made in the given repair *tree*. Returns the resulting
    # circuit term.
    #
    # So the general flow is usually `Term` -> `parse(Term) : ParseTree` ->
    # `RepairTree` -> `collapse(RepairTree) : Term`.
    #
    # *tree* contains enough information to reconstruct the circuit. So you
    # don't need to provide the original circuit here.
    def collapse(tree : RepairTree) : Term
    end
  {% end %}

  # Calls *fn* for each `Flat` feature found in the given parse *tree*.
  def each_flat_feature(tree : ParseTree, &fn : Flat ->) : Nil
    case tree
    in InertLeaf, GndLeaf
      fn.call(tree.feature)
    in MixtureNode, ScopeNode
      each_flat_feature(tree.child, &fn)
    in ParentNode
      tree.children.each do |child|
        each_flat_feature(child, &fn)
      end
    end
  end

  # Calls *fn* for each `Flat` feature found in the given parse *tree*.
  def each_flat_feature_with_addr(tree : ParseTree, &fn : Flat, NodeAddr ->) : Nil
    each_flat_feature_with_addr(tree, NodeAddr.empty, fn)
  end

  private def each_flat_feature_with_addr(tree : InertLeaf | GndLeaf, addr, fn) : Nil
    fn.call(tree.feature, addr)
  end

  private def each_flat_feature_with_addr(tree : MixtureNode | ScopeNode, addr, fn) : Nil
    each_flat_feature_with_addr(tree.child, addr, fn)
  end

  private def each_flat_feature_with_addr(tree : ParentNode, addr, fn) : Nil
    tree.children.each_with_index do |child, index|
      key = tree.feature.range.begin + index
      each_flat_feature_with_addr(child, addr.append(key), fn)
    end
  end

  # Applies a perturbation *fn* to ground and parent node terms. Parents
  # are perturbed after their children.
  def perturb(tree : ParseTree, *, cue_disj : Indexable(Term::Sym) = Slice(Term::Sym).empty, &fn : Term, NodeAddr -> Term) : Term
    perturb(tree, cue_disj, NodeAddr.empty, fn)
  end

  private def perturb(tree : InertLeaf, cue_disj, addr, fn) : Term
    tree.feature.node # unchanged
  end

  private def perturb(tree : GndLeaf, cue_disj, addr, fn) : Term
    node = tree.feature.node

    unless dict = node.as_d?
      return node # unchanged
    end

    if cue_disj.present? && cue_disj.none? { |cue| dict.probably_includes?(cue) }
      return node # unchanged
    end

    fn.call(node, addr)
  end

  private def perturb(tree : MixtureNode | ScopeNode, cue_disj, addr, fn) : Term
    collapse(repair(tree) { |child| perturb(child, cue_disj, addr, fn) })
  end

  private def perturb(tree : ParentNode, cue_disj, addr, fn) : Term
    dict = tree.feature.node
    range = tree.feature.range

    if cue_disj.present? && cue_disj.none? { |cue| dict.probably_includes?(cue) }
      return Term.of(dict) # unchanged
    end

    repair = repair(tree) do |child, index|
      key = range.begin + index
      perturb(child, cue_disj, addr.append(key), fn)
    end

    # repair : ParentRepair
    fn.call(collapse(repair), addr)
  end
end
