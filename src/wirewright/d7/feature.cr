module Ww::D7
  # A classifier function "looks" at a circuit node term (more or less literally,
  # but using pattern matching instead of a pair of "eyes"). It decides what
  # the node's semantic function is (what the node "means"), primarily its
  # *structural* role, and represents the decision using one of `Feature`s.
  #
  # By *structural role*, I mean answers to questions such as "Should I descend
  # here or leave it as-is?", "Should I ignore this or pass it to the solver?".
  alias Classifier = Term -> Feature

  # Nodes from a circuit are *classified* into *features*.
  alias Feature = Ready | Nonready

  alias Ready = Flat | Mixture | Scope | Parent | Circuit

  # Features without a successor.
  alias Flat = Gnd | Inert

  # Lists annotations that can be used on an inert node.
  @[Flags]
  enum InertAnnotationSet
    # A request to ignore the inert node even though a pass might consider
    # it fitting.
    Ignore
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
  defcase Scope, scope : NodeScope::Any, cont : Ready

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
  defrecord Parent, node : Term::Dict, range : Range(Int32, Int32)

  # Constructs a parent feature.
  #
  # See `Parent`.
  def parent(node : Term::Dict, range : Range(Int32, Int32)) : Parent
    assert range.exclusive? && range.begin >= 0 && range.end >= 0

    Parent.new(node, range)
  end

  def parent(node : Term::Dict)
    parent(node, 0...node.itemsize)
  end

  # Used primarily by the `circuit` node; represents an isolated, nested
  # circuit whose children (as defined by *range*) are evaluated using
  # iterative deepening.
  #
  # *leaf* defines leaf treatment, when this circuit is among the nodes
  # at the target depth (so evaluation stops at it, without descending
  # into *range*).
  #
  # Importantly, it makes no sense for *cont* to contain circuits, recursively,
  # even though they are permitted by types and will work. The way they will work,
  # though, is rather degenerate -- the evaluator will simply proceed into their
  # *leaf*, recursively; never evaluating them as circuits. In other words, if
  # *leaf* emits a circuit, that circuit is always a leaf, and so its *leaf* function
  # is called, and so on, until some sort of base case where there is no circuit
  # (or infinitely if there is no base case).
  defrecord Circuit,
    node : Term::Dict,
    range : Range(Int32, Int32),
    leaf : -> Ready

  # Constructs a circuit feature.
  #
  # See `Circuit`.
  def circuit(node : Term::Dict, range : Range(Int32, Int32), &leaf : -> Ready) : Circuit
    assert range.exclusive? && range.subrange_of?(0...node.itemsize)

    Circuit.new(node, range, leaf)
  end

  def circuit(node : Term::Dict) : Circuit
    circuit(node, 0...node.itemsize) { inert(Term.of(node)) }
  end

  # `Nonready` represents a thunk which must be further classified by the caller.
  # `Nonready`s can be evaluated using `ready`.
  defrecord Nonready, node : Term

  # See `Nonready`.
  def nonready(node : Term) : Nonready
    Nonready.new(node)
  end

  # Classifies *node* using *clf*, evaluating `Nonready` thunks using *clf*
  # until they are `Ready`.
  #
  # The separation between `Ready` and `Nonready` is useful when you want to
  # wrap a classifier *clf* within another one. Especially during component
  # expansion or recursive classification, the nested classifier cannot call
  # the parent one to [tail-] recurse. To resolve this, we use `Nonready`
  # thunks, which "bubble up" to the outermost classifier and are then classified
  # by it, and so on down the chain if needed.
  def ready(clf : Classifier, node : Term) : Ready
    loop do
      case result = clf.call(node)
      in Ready
        return result
      in Nonready
        node = result.node
      end
    end
  end

  # A tree of `Feature`s.
  alias ParseTree = InertLeaf | GndLeaf | MixtureNode | ScopeNode | ParentNode

  defrecord InertLeaf, feature : Inert
  defrecord GndLeaf, feature : Gnd
  defcase MixtureNode, feature : Mixture, child : ParseTree
  defcase ScopeNode, feature : Scope, child : ParseTree
  defrecord ParentNode, feature : Parent, children : Slice(ParseTree)

  # *Unaugmented feature trees* are clear of "augmentation" features such as
  # `Scope`. `Mixture` are treated as `Inert`. `Circuit`s are treated as `Parent`s.
  alias UnaugmentedParseTree = InertLeaf | GndLeaf | UnaugmentedParentNode

  defrecord UnaugmentedParentNode, feature : Parent, children : Slice(UnaugmentedParseTree)

  # *Level feature trees* stop at `Circuit` features, calling their leaf function.
  module LevelParseTree
  end

  # *Full feature trees* proceed into `Circuit`s as if they were `Parent`s.
  module FullParseTree
  end

  # Uses the classifier *clf* to convert a *circuit* into a tree of the kind
  # defined by the given *reply* type.
  def parse(clf : Classifier, circuit : Term, reply : LevelParseTree.class | FullParseTree.class | UnaugmentedParseTree.class)
    unless nodes = circuit.as_d?
      return InertLeaf.new(feature: inert(circuit))
    end

    parse(clf, parent(nodes), reply)
  end

  private def parse(clf : Classifier, feature : Inert, reply)
    InertLeaf.new(feature)
  end

  private def parse(clf : Classifier, feature : Gnd, reply)
    GndLeaf.new(feature)
  end

  private def parse(clf : Classifier, feature : Mixture, reply : LevelParseTree.class | FullParseTree.class)
    child = parse(clf, ready(clf, feature.defn), reply)

    MixtureNode.new(feature, child)
  end

  private def parse(clf : Classifier, feature : Mixture, reply : UnaugmentedParseTree.class)
    InertLeaf.new(inert(feature.node))
  end

  private def parse(clf : Classifier, feature : Scope, reply : LevelParseTree.class | FullParseTree.class)
    child = parse(clf, feature.cont, reply)

    ScopeNode.new(feature, child)
  end

  private def parse(clf : Classifier, feature : Scope, reply : UnaugmentedParseTree.class)
    parse(clf, feature.cont, reply)
  end

  private def parse(clf : Classifier, feature : Parent, reply : LevelParseTree.class | FullParseTree.class)
    assert feature.range.exclusive?
    assert feature.range.begin <= feature.range.end

    children = Pf::Kit.stack_array(ParseTree)

    feature.range.each do |index|
      child = parse(clf, ready(clf, feature.node[index]), reply)
      children << child
    end

    ParentNode.new(feature, children.to_unsafe_readonly_slice!)
  end

  private def parse(clf : Classifier, feature : Parent, reply : UnaugmentedParseTree.class)
    assert feature.range.exclusive?
    assert feature.range.begin <= feature.range.end

    children = Pf::Kit.stack_array(UnaugmentedParseTree)

    feature.range.each do |index|
      child = parse(clf, ready(clf, feature.node[index]), reply)
      children << child
    end

    UnaugmentedParentNode.new(feature, children.to_unsafe_readonly_slice!)
  end

  private def parse(clf : Classifier, feature : Circuit, reply : LevelParseTree.class)
    parse(clf, feature.leaf.call, reply)
  end

  private def parse(clf : Classifier, feature : Circuit, reply : FullParseTree.class | UnaugmentedParseTree.class)
    parse(clf, parent(feature.node, feature.range), reply)
  end

  # `ParseTree` is usually rewritten; we call this *repair*, and represent
  # the result of repairing a `ParseTree` using `RepairTree`.
  #
  # - `InertLeaf` and `GndLeaf` must become a `Term`.
  # - `MixtureNode` must become `MixtureRepair` or `Term` if not changed.
  # - `ScopeNode` must be omitted, since it is purely an augmentation; not an
  #   actual, structural node.
  # - `ParentNode` must become `ParentRepair`.
  #
  # Most complexities in the above are handled by the set of `repair`
  # utility functions. You are advised to use them instead of employing
  # custom logic for achieving the above.
  alias RepairTree = Term | MixtureRepair | ParentRepair

  defcase MixtureRepair, feature : Mixture, child : RepairTree
  defrecord ParentRepair, feature : Parent, children : Slice(RepairTree)

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
  def repair(tree : ParentNode, & : ParseTree -> RepairTree) : RepairTree
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
end
