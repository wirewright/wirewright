module Ww::D7
  # A classifier function "looks" at a circuit node term (more or less literally,
  # but using pattern matching rather than "eyes"); and decides what its semantic
  # function is (what the node "means"), represented as one of `Feature`s.
  alias Classifier = Term -> Feature

  # Nodes from a circuit are *classified* into *features*.
  alias Feature = Inert | Gnd | Mixture | Scope | Parent | Chat | Circuit

  # Represents an inert (data) node.
  defcase Inert, node : Term

  # Constructs a no-operation or inert data node.
  def inert(node : Term) : Inert
    Inert.new(node)
  end

  # Represents a grounded node: a node to which no further recursive evaluation
  # should apply; a node which is part of the hypergraph that should be solved
  # by D7.
  defcase Gnd, node : Term, edges : Slice(Edge)

  # Constructs a grounded node from an enumerable of edges *ee*.
  #
  # See `Gnd`.
  def gnd(node : Term, edges : Enumerable(Edge)) : Gnd
    Gnd.new(node, edges: edges.to_readonly_slice(&.itself))
  end

  # Constructs a grounded node with the given *edges*. You can use `edge` to
  # construct edges.
  #
  # See `Gnd`.
  def gnd(node : Term, *edges : Edge) : Gnd
    gnd(node, edges)
  end

  # Constructs a grounded node without edges.
  #
  # See `Gnd`.
  def gnd(node : Term) : Gnd
    Gnd.new(node, edges: Slice(Edge).empty)
  end

  # Represents a node edge. *term* is the edge term itself, e.g. `@x`,
  # and *path* is the itempath from node to that edge. It must be a valid
  # itempath, otherwise, the solver will raise at runtime.
  record Edge, term : Term, path : Slice(Int32)

  # Constructs an edge object.
  #
  # See `Edge`.
  def edge(term : Term, *path : Int32) : Edge
    Edge.new(term, path.to_readonly_slice(&.itself))
  end

  # A decomposition of *node* into a definition *defn* with a *mix* function
  # to compose rewritten *defn* back into the next version of *node*.
  defcase Mixture, node : Term, defn : Term, mix : Term, Term -> Term

  # Constructs a mixture feature.
  #
  # See `Mixture`.
  def mixture(node : Term, defn, &mix : Term, Term -> Term) : Mixture
    Mixture.new(node, Term.of(defn), mix)
  end

  # Represents a lexical scope binding. Attaches bindings to a continuation
  # feature *cont*.
  defcase Scope, bindings : Term::Dict, cont : Feature

  # Constructs a scope feature.
  #
  # See `Scope`.
  def scope(bindings : Term | Term::Any, cont : Feature) : Scope
    Scope.new(bindings.as_d, cont)
  end

  # Represents the children nodes of *node* found within an exclusive positive
  # range of its items.
  #
  # Used by e.g. `group`, `module`.
  defcase Parent, node : Term::Dict, range : Range(Int32, Int32)

  # Constructs a parent feature.
  #
  # See `Parent`.
  def parent(node : Term | Term::Dict, range : Range(Int32, Int32)) : Parent
    assert range.exclusive? && range.begin.positive? && range.end.positive?
    assert node.type.dict?

    Parent.new(node.as_d, range)
  end

  # Represents a node that offers a place where its children can "chat",
  # being a kind of "chat room" for children.
  #
  # - *queue* is the message queue ("log of unread messages in the chat"; oldest
  #   unread message goes first).
  # - *enq* determines whether enqueue is allowed.
  # - *cont* is the continuation feature for *node*.
  # - *submit* is used to morph *node* (first arg) into its next shape once
  #   the updated message queue (second arg) is available, possibly blocking (third arg)
  #   the queue to prevent further enqueues for the time being.
  # - *asc* determines the ascent pattern. The chat will emit messages that match *asc*
  #   to the enclosing chat, letting them "bubble up".
  # - *desc* is, similarly, a pattern that accepts or declines a message from
  #   the enclosing chat to this chat. Matching messages are *enqueued*.
  #
  # *asc* and *desc* enable bidirectional message exchange between nested chats.
  defcase Chat,
    node : Term,
    queue : Term::Dict,
    enq : Bool,
    cont : Feature,
    submit : (Term, Term::Dict -> Term),
    asc : Term,
    desc : Term

  # Constructs a chat feature.
  #
  # See `Chat`.
  def chat(node : Term, queue : Term::Dict, cont : Feature, asc : Term, desc : Term, *, enq : Bool, &submit : Term, Term::Dict -> Term) : Chat
    Chat.new(node, queue, enq, cont, submit, asc, desc)
  end

  # Used by e.g. `frag`, `unit`, and at the top-level to represent and
  # evaluate circuits.
  defcase Circuit,
    node : Term::Dict,
    range : Range(Int32, Int32),
    cont : Term -> Feature

  # Constructs a circuit feature.
  #
  # See `Circuit`.
  def circuit(node : Term, range : Range(Int32, Int32), &cont : Term -> Feature) : Circuit
    assert range.exclusive? && range.subrange_of?(0...node.itemsize)
    assert node.type.dict?

    Circuit.new(node.as_d, range, cont)
  end
end
