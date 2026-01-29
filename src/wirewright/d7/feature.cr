module Ww::D7
  # A classifier function "looks" at a circuit node term (more or less literally,
  # but using pattern matching rather than "eyes"); and decides what its semantic
  # function is (what the node "means"), represented as one of `Feature`s.
  alias Classifier = Term -> Feature

  # Nodes from a circuit are *classified* into *features*.
  alias Feature = Inert | Gnd | Mixture | Scope | Parent | Circuit

  # Represents an inert (data) node.
  defcase Inert, node : Term

  # Constructs a no-operation or inert data node.
  def inert(node : Term) : Inert
    Inert.new(node)
  end

  # Represents a grounded node: a node to which no further recursive evaluation
  # should apply; a node which is part of the hypergraph that should be solved
  # by D7.
  defcase Gnd, node : Term, edges : Slice(Term)

  # Constructs a grounded node from an enumerable of edges *ee*.
  #
  # See `Gnd`.
  def gnd(node : Term, edges : Enumerable(Term)) : Gnd
    Gnd.new(node, edges: edges.to_readonly_slice(&.itself))
  end

  # Constructs a grounded node with the given *edges*.
  #
  # See `Gnd`.
  def gnd(node : Term, *edges : Term) : Gnd
    gnd(node, edges)
  end

  # Constructs a grounded node without edges.
  #
  # See `Gnd`.
  def gnd(node : Term) : Gnd
    Gnd.new(node, edges: Slice(Term).empty)
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

  # Used by e.g. `frag`, `unit`, and at the top-level to represent and
  # evaluate circuits.
  defcase Circuit,
    node : Term::Dict,
    range : Range(Int32, Int32),
    cont : -> Feature

  # Constructs a circuit feature.
  #
  # See `Circuit`.
  def circuit(node : Term::Dict, range : Range(Int32, Int32), &cont : -> Feature) : Circuit
    assert range.exclusive? && range.subrange_of?(0...node.itemsize)

    Circuit.new(node, range, cont)
  end

  def circuit(node : Term::Dict) : Circuit
    circuit(node, 0...node.itemsize) { inert(Term.of(node)) }
  end
end
