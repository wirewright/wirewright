module Ww::D7
  # A classifier function "looks" at a circuit node term (more or less literally,
  # but using pattern matching instead of a pair of "eyes"). It decides what
  # the node's semantic function is (what the node "means"), primarily its
  # *structural* function, and represents the decision using one of `Feature`s.
  #
  # By *structural function*, I mean an answer to questions such as "Should I descend
  # here or leave it as-is?", "Should I ignore this or pass it to the solver?".
  alias Classifier = Term -> Feature

  # Nodes from a circuit are *classified* into *features*.
  alias Feature = Flat | Mixture | Scope | Parent | Circuit

  # Features without a successor.
  alias Flat = Gnd | Inert

  # Represents an inert (data) node.
  defcase Inert, node : Term

  # Constructs a no-operation or inert data node.
  def inert(node : Term) : Inert
    Inert.new(node)
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
  defcase Gnd, node : Term, defn : Term, edges : Slice(Term)

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
  defcase Mixture, node : Term, defn : Term, mix : Term -> Term

  # Constructs a mixture feature.
  #
  # See `Mixture`.
  def mixture(node : Term, defn, &mix : Term -> Term) : Mixture
    Mixture.new(node, Term.of(defn), mix)
  end

  # Represents a lexical scope binding. Attaches bindings to a continuation
  # feature *cont*.
  defcase Scope, bindings : Term::Dict, cont : Feature

  # Constructs a scope feature.
  #
  # See `Scope`.
  def scope(bindings : Term::Dict, cont : Feature) : Scope
    Scope.new(bindings, cont)
  end

  # Represents the children nodes of *node* found within an exclusive positive
  # range of its items.
  #
  # Used by e.g. `group`, `module`.
  defcase Parent, node : Term::Dict, range : Range(Int32, Int32)

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
  # *cont* defines leaf treatment, when this circuit is among the nodes
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
  defcase Circuit,
    node : Term::Dict,
    range : Range(Int32, Int32),
    leaf : -> Feature

  # Constructs a circuit feature.
  #
  # See `Circuit`.
  def circuit(node : Term::Dict, range : Range(Int32, Int32), &leaf : -> Feature) : Circuit
    assert range.exclusive? && range.subrange_of?(0...node.itemsize)

    Circuit.new(node, range, leaf)
  end

  def circuit(node : Term::Dict) : Circuit
    circuit(node, 0...node.itemsize) { inert(Term.of(node)) }
  end
end
