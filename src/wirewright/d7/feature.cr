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
  defrecord Gnd, node : Term, defn : Term, edges : Slice(Term)

  # Constructs a grounded node from an enumerable of edges *ee*.
  #
  # See `Gnd`.
  def gnd(node : Term, edges : Enumerable(Term), *, defn : Term = node) : Gnd
    Gnd.new(node, defn, edges.to_readonly_slice(&.itself))
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
  defcase Scope, scope : NodeScope::Any, cont : Feature

  # Constructs a scope feature.
  #
  # See `Scope`.
  def scope(cont : Feature, *, bindings : Term::Dict) : Scope
    Scope.new(NodeScope::ClosedExcept.new(bindings), cont)
  end

  # :ditto:
  def scope(cont : Feature, *, locals : Indexable(Term)) : Scope
    Scope.new(NodeScope::OpenExcept.new(locals.to_readonly_slice(&.itself)), cont)
  end

  # Represents the children nodes of *node* found within an exclusive positive
  # range of its items.
  #
  # Used by e.g. `group`, `module`.
  defrecord Parent, node : Term::Dict, range : Range(UInt32, UInt32)

  # Constructs a parent feature.
  #
  # See `Parent`.
  def parent(node : Term::Dict, range : Range(UInt32, UInt32)) : Parent
    assert range.exclusive?

    Parent.new(node, range)
  end

  # :ditto:
  def parent(node : Term::Dict)
    parent(node, 0u32...node.uitemsize)
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

  # A tree of `Feature`s.
  alias ParseTree = InertLeaf | GndLeaf | MixtureNode | ScopeNode | ParentNode

  defrecord InertLeaf, feature : Inert
  defrecord GndLeaf, feature : Gnd
  defcase MixtureNode, feature : Mixture, child : ParseTree, maxlevel : Int32
  defcase ScopeNode, feature : Scope, child : ParseTree

  alias ParentNode = GroupNode | CircuitNode

  defrecord GroupNode, feature : Parent, children : Slice(ParseTree), maxlevel : Int32
  defcase CircuitNode, feature : Circuit, children : Slice(ParseTree), leaf : ParseTree, maxlevel : Int32

  # :nodoc:
  def maxlevel(tree : InertLeaf | GndLeaf) : Int32
    0
  end

  # :nodoc:
  def maxlevel(tree : ScopeNode) : Int32
    maxlevel(tree.child)
  end

  # :nodoc:
  def maxlevel(tree : MixtureNode | GroupNode) : Int32
    tree.maxlevel
  end

  # :nodoc:
  def maxlevel(tree : CircuitNode) : Int32
    tree.maxlevel + 1
  end

  # Smart constructor for `MixtureNode`.
  def MixtureNode.new(feature : Mixture, child : ParseTree) : MixtureNode
    MixtureNode.new(feature, child, D7.maxlevel(child))
  end

  # Smart constructor for `GroupNode`.
  def GroupNode.new(feature, children : Slice(ParseTree)) : GroupNode
    maxlevel = children.max_of? { |child| D7.maxlevel(child) } || 0

    GroupNode.new(feature, children, maxlevel)
  end

  # Smart constructor for `CircuitNode`.
  def CircuitNode.new(feature, children : Slice(ParseTree), leaf) : CircuitNode
    maxlevel = children.max_of? { |child| D7.maxlevel(child) } || 0
    maxlevel += 1

    CircuitNode.new(feature, children, leaf, maxlevel)
  end

  # *Unaugmented parse trees* are clear of "augmentation" features such as
  # `Scope`. `Mixture`s are treated as `Inert`. `Circuit`s are treated as `Parent`s.
  alias UnaugmentedParseTree = InertLeaf | GndLeaf | UnaugmentedParentNode

  defrecord UnaugmentedParentNode,
    feature : Parent,
    children : Slice(UnaugmentedParseTree)

  # A thin wrapper around `D7.parse` that also manages parse caches for `ParseTree`
  # and `UnaugmentedParseTree`.
  struct Parser
    # Returns the D7 classifier used by this parser.
    getter clf : Classifier

    def initialize(
      @clf : Classifier,
      @cache : ICache(Term, ParseTree),
      @u_cache : ICache(Term, UnaugmentedParseTree),
    )
    end

    def self.new(clf : Classifier) : Parser
      cache = GenerationalCache(Term, ParseTree).new
      u_cache = GenerationalCache(Term, UnaugmentedParseTree).new
      new(clf, cache, u_cache)
    end

    # See `D7.parse`.
    def parse(circuit : Term, reply : ParseTree.class) : ParseTree
      @cache.epoch do
        D7.parse(@clf, circuit, reply: ParseTree, cache: @cache)
      end
    end

    # See `D7.parse`.
    def parse(circuit : Term, reply : UnaugmentedParseTree.class) : UnaugmentedParseTree
      @u_cache.epoch do
        D7.parse(@clf, circuit, reply: UnaugmentedParseTree, cache: @u_cache)
      end
    end
  end

  # Uses the classifier *clf* to convert a *circuit* into a tree of the kind
  # defined by the *reply* type. *circuit* is considered a `Parent` if it
  # is a dict.
  #
  # You can also provide an explicit child *range* to pass through to `Parent`;
  # by default, all items are considered.
  def parse(clf : Classifier, circuit : Term, reply : ParseTree.class, *, cache = Uncached(Term, ParseTree).new, range : Range(UInt32, UInt32)? = nil)
    unless nodes = circuit.as_d?
      return InertLeaf.new(feature: inert(circuit))
    end

    # Assuming you can't embed a circuit inside itself, of course ... Which you can't.
    cache.put_if_absent(circuit) do
      parse(clf, cache, parent(nodes, range || (0u32...nodes.uitemsize)), reply)
    end
  end

  # :ditto:
  def parse(clf : Classifier, circuit : Term, reply : UnaugmentedParseTree.class, *, cache = Uncached(Term, UnaugmentedParseTree).new)
    unless nodes = circuit.as_d?
      return InertLeaf.new(feature: inert(circuit))
    end

    parse(clf, cache, parent(nodes), reply)
  end

  private def parse(clf, cache, feature : Inert, reply)
    InertLeaf.new(feature)
  end

  private def parse(clf, cache, feature : Gnd, reply)
    GndLeaf.new(feature)
  end

  private def parse(clf, cache, feature : Mixture, reply : ParseTree.class)
    child = parse(clf, cache, clf.call(feature.defn), reply)

    MixtureNode.new(feature, child)
  end

  private def parse(clf, cache, feature : Scope, reply : ParseTree.class)
    child = parse(clf, cache, feature.cont, reply)

    ScopeNode.new(feature, child)
  end

  private def parse(clf, cache, feature : Parent, reply : ParseTree.class) : ParentNode
    children = parse(clf, cache, feature.range, feature.node, reply)

    GroupNode.new(feature, children)
  end

  private def parse(clf, cache, feature : Circuit, reply : ParseTree.class) : ParentNode
    children = parse(clf, cache, feature.range, feature.node, reply)
    leaf = parse(clf, cache, feature.leaf, reply)

    CircuitNode.new(feature, children, leaf)
  end

  private def parse(clf, cache, feature : Mixture, reply : UnaugmentedParseTree.class)
    InertLeaf.new(inert(feature.node))
  end

  private def parse(clf, cache, feature : Scope, reply : UnaugmentedParseTree.class)
    parse(clf, cache, feature.cont, reply)
  end

  private def parse(clf, cache, feature : Parent, reply : UnaugmentedParseTree.class) : UnaugmentedParentNode
    children = parse(clf, cache, feature.range, feature.node, reply)

    UnaugmentedParentNode.new(feature, children)
  end

  private def parse(clf, cache, feature : Circuit, reply : UnaugmentedParseTree.class)
    parse(clf, cache, parent(feature.node, feature.range), reply)
  end

  private def parse(clf, cache, range : Range(UInt32, UInt32), node : Term::Dict, reply)
    assert range.exclusive?
    assert range.begin <= range.end

    range.to_readonly_slice { |index| parse(clf, cache, node[index], reply) }
  end

  private def parse(clf, cache, term : Term, reply)
    cache.put_if_absent(term) do
      parse(clf, cache, clf.call(term), reply)
    end
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

  private def changed?(before : GndLeaf | InertLeaf | MixtureNode | ParentNode | UnaugmentedParentNode, after : Term) : Bool
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
  def repair(tree : ParentNode | UnaugmentedParentNode, &) : RepairTree
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
  def each_flat_feature(tree : ParseTree | UnaugmentedParseTree, &fn : Flat ->) : Nil
    case tree
    in InertLeaf, GndLeaf
      fn.call(tree.feature)
    in MixtureNode, ScopeNode
      each_flat_feature(tree.child, &fn)
    in ParentNode, UnaugmentedParentNode
      tree.children.each do |child|
        each_flat_feature(child, &fn)
      end
    end
  end

  # Calls *fn* for each `Flat` feature found in the given parse *tree*.
  def each_flat_feature_with_addr(tree : ParseTree | UnaugmentedParseTree, &fn : Flat, NodeAddr ->) : Nil
    each_flat_feature_with_addr(tree, NodeAddr.empty, fn)
  end

  private def each_flat_feature_with_addr(tree : InertLeaf | GndLeaf, addr, fn) : Nil
    fn.call(tree.feature, addr)
  end

  private def each_flat_feature_with_addr(tree : MixtureNode | ScopeNode, addr, fn) : Nil
    each_flat_feature_with_addr(tree.child, addr, fn)
  end

  private def each_flat_feature_with_addr(tree : ParentNode | UnaugmentedParentNode, addr, fn) : Nil
    tree.children.each_with_index do |child, index|
      key = tree.feature.range.begin + index
      each_flat_feature_with_addr(child, addr.append(key), fn)
    end
  end

  # Applies a perturbation *fn* to ground and parent node terms. Parents
  # are perturbed after their children.
  def perturb(tree : ParseTree | UnaugmentedParentNode, *, cue_disj : Indexable(Term::Sym) = Slice(Term::Sym).empty, &fn : Term, NodeAddr -> Term) : Term
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

  private def perturb(tree : ParentNode | UnaugmentedParentNode, cue_disj, addr, fn) : Term
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
