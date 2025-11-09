# Wirewright Delta7 (D7 for short) is a *symbolic physics engine*. In a sense,
# it is just like a *physics engine* (think Box2D), but instead of working with
# bodies, it works with *symbols*. Rather than solving equations, D7 searches for
# relationships between symbols according to constraints. Instead of manipulating
# velocity and position, D7 *rewrites*.
#
# At its core, D7 is an attempt to model *autopoiesis* as described by Maturana,
# Varela, and others.
#
# See, for instance, *Autopoiesis: the organization of living systems, its
# characterization and a model* by Varela, Maturana & Uribe (1974). D7 is trying
# to check all the boxes in section 9, "Key".
#
# I think autopoiesis can be modeled in any "physics simulator". The only problem
# is that in practice, the physics simulators we build are too unstructured. It's
# "perceptually hard" to extract useful info from it, both for us as observers and
# for the entities within the simulation. Imagine how much intrinsic structure
# a particle simulator would require to start recognizing or matching on its own
# configuration or its parts? With D7, it's as simple as a pattern match on a fragment.
#
# Imagine a game. How hard would it be to make a car drive itself in that game, given
# only the game's visuals as output and keyboard press states as input -- that is,
# "from an outside agent's point of view"?
#
# We know the answer: very hard. That's why people resort to black box (ish) methods
# like neural networks. In the worst case, you'd need a human -- an intricate
# apparatus indeed.
#
# If only we had a *symbolic* physics simulator, with the same or similar kinds
# of behaviors, but with structures easy to pattern match and construct
# programmatically, "from an outside agent's point of view"...
#
# D7 is an attempt to build such a simulator.
#
# With D7, a *symbol* is an identity or a composition thereof. Such symbols are
# represented meaningfully with `Term`s.
#
# D7 programs -- called *circuits* -- form a hypergraph. A hypergraph is a graph
# whose edges -- hyperedges -- are *sets*. You can imagine a hyperedge as a group.
# Each node in a hypergraph participates in zero or more such groups.
#
# In D7, there is no difference between *running* a circuit and *building* it. There
# is no "runtime", nor is there "compile-time". D7 is more like a game, which you
# can pause, save, and return to in the future. Since D7 hypergraphs are persistent
# and immutable (they are `Term`s), you get time travel for free, too, which is
# very useful for debugging. Branching and other features come for free, too
# (think Git or rather, something crude and Git-like).
#
# A *D7 engine* to a D7 circuit is like a browser to a web page.
#
# D7 introduces the notion of *entanglement*. Entanglement is how D7 circuits interact
# with the outside world. The circuit may include symbolic objects recognized
# by the engine. Those objects are synced by the engine to their "outside-world"
# counterparts. In a sense, such objects are *percepts* (internal, inbound
# representations of outside-world entities) and *goals* (internal, outbound
# representations of outside-world actions or transformations).
#
# With entanglement, D7 lets you access files, communicate with processes,
# build server, graphical, and terminal apps and so on.
#
# Alongside edges, D7 also has *surfaces*: *sensors* and *appearances*.
# A sensor senses zero or more appearances. An appearance excites zero or more
# sensors. Surfaces live in a *termspace*. D7 circuits can include zero or more
# termspaces. A termspace can be local or global. A local termspace is bounded by
# the circuit. A global termspace is either circuit-global or remote. A remote
# termspace is like a multiplayer game, where each sensor and appearance is a tiny
# "player" and the termspace itself is like a world (think Minecraft).
#
# Surfaces complement hyperedges in that hyperedges are hard-coded connectivity
# (even if dynamically generated, especially with the help of D7 modules); whereas
# for surfaces, whether they are "connected" is highly dynamic and depends
# on the content itself.
#
# D7 circuits are graphs whose edges are *sets*; D7 termspaces are graphs whose
# edges are *functions*, or more specifically, *predicates*.
module Ww::D7
  extend self

  # :nodoc:
  alias NodeId = UInt32

  # A hypergraph is a graph whose edges can include any number of nodes; each edge
  # is a subset of the set of nodes in that graph. It's easier to think of a hypergraph
  # as a community of nodes. Each node can participate in zero or more groups, each
  # group formed from other nodes in the community.
  struct Hypergraph
    # - *nodemap* maps nodes to node ids (implicit, array index).
    # - *edgemap* maps node ids (implicit, array index) to hyperedges that node
    #   is participating in.
    # - *trmap* maps node ids (implicit, array index) to node qualpaths.
    def initialize(@nodemap : Array(Term), @edgemap : Array(Slice(Term)))
    end

    # Returns the number of nodes in this graph.
    def order
      @nodemap.size
    end

    # Returns the node associated with the given *id*.
    def [](id : NodeId) : Term
      @nodemap[id]
    end

    # Yields nodes of this hypergraph.
    def each_node_with_id(&) : Nil
      @nodemap.each_with_index do |node, id|
        yield node, NodeId.new(id)
      end
    end

    # Yields hyperedges of a node with the given node *id* (as yielded
    # by `each_node`).
    def each_edge(id : NodeId, &) : Nil
      edges = @edgemap[id]
      edges.each { |edge| yield edge }
    end

    # Returns `true` if *id* participates in the given hyperedge *edge*. Returns
    # `false` otherwise.
    def member?(id : NodeId, edge needle : Term) : Bool
      each_edge(id) do |edge|
        next unless edge == needle
        return true
      end

      false
    end

    # Converts `self` to an unordered graph through clique expansion: nodes that
    # share a hyperedge are connected; each unordered edge is represented with
    # a pair of edges going in opposite directions.
    def graph : Slice(Pf::USet32)
      groups = {} of Term => Pf::USet32

      each_node_with_id do |node, id|
        each_edge(id) do |edge|
          groups[edge] = (groups[edge]? || Pf::USet32.new).add(id)
        end
      end

      graph = Slice(Pf::USet32).new(order) { Pf::USet32.new }

      groups.each do |_, members|
        members.each do |u|
          members.each do |v|
            next if u == v

            vs = graph[u]
            next if v.in?(vs)

            graph[u] = vs.add(v)
          end
        end
      end

      graph.readonly
    end
  end

  # A classifier function "looks" at a circuit node term (more or less literally,
  # but using pattern matching rather "eyes"); and decides what its semantic
  # function is (what the node "means"), represented as one of `Feature`s.
  alias Classifier = Term -> Feature

  # Nodes from a circuit are *classified* into *features*.
  alias Feature = Inert | Gnd | Mixture | Scope | Parent | Subcircuit

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
  def gnd(node : Term, ee : Enumerable(Edge)) : Gnd
    Gnd.new(node, edges: ee.to_readonly_slice(&.itself))
  end

  # Constructs a grounded node with the given *edges*. You can use `edge` to
  # construct edges.
  def gnd(node : Term, *edges : Edge) : Gnd
    gnd(node, edges)
  end

  # Constructs a grounded node without edges.
  def gnd(node : Term) : Gnd
    Gnd.new(node, edges: Slice(Edge).empty)
  end

  # Represents a node edge. *term* is the edge term itself, e.g. `@x`,
  # and *path* is the itempath from node to that edge. It must be a valid
  # itempath, otherwise, the solver will raise at runtime.
  defcase Edge, term : Term, path : Slice(Int32)

  # Constructs an edge object.
  def edge(term : Term, *path : Int32) : Edge
    Edge.new(term, path.to_readonly_slice(&.itself))
  end

  # A decomposition of *node* into a definition *defn* with a *mix* function
  # to compose rewritten *defn* back into the next version of *node*.
  defcase Mixture, node : Term, defn : Term, mix : Term, Term -> Term

  # Constructs a mixture feature.
  def mixture(node : Term, defn, &mix : Term, Term -> Term) : Mixture
    Mixture.new(node, Term.of(defn), mix)
  end

  # Represents a lexical scope binding. Attaches bindings to a continuation
  # feature *cont*.
  defcase Scope, bindings : Term::Dict, cont : Feature

  # Constructs a scope feature.
  def scope(bindings : Term | ITerm, cont : Feature) : Scope
    Scope.new(bindings.as_d, cont)
  end

  # Represents the children nodes of *node* found within an exclusive positive
  # range of its items.
  #
  # Used by e.g. `group`, `module`.
  defcase Parent, node : Term::Dict, range : Range(Int32, Int32)

  # Constructs a parent feature.
  def parent(node : Term, range : Range(Int32, Int32)) : Parent
    assert range.exclusive? && range.begin.positive? && range.end.positive?
    assert node.type.dict?

    Parent.new(node.as_d, range)
  end

  # Represents the subcircuit nodes of *node* found within an exclusive positive
  # range of its items.
  #
  # Used by e.g. `frag`.
  defcase Subcircuit, node : Term::Dict, range : Range(Int32, Int32), cont : Term -> Feature

  # Constructs a subcircuit feature.
  def subcircuit(node : Term, range : Range(Int32, Int32), &cont : Term -> Feature) : Subcircuit
    assert range.exclusive? && range.subrange_of?(0...node.itemsize)
    assert node.type.dict?

    Subcircuit.new(node.as_d, range, cont)
  end

  # :nodoc:
  alias NodeAddr = Slice(Int32)

  # :nodoc:
  alias NodeScope = Slice({NodeAddr, Term::Dict})

  # :nodoc:
  defrecord Leaf, addr : NodeAddr, node : Term, edges : Slice(Term)

  private def solve(regime : Regime, &)
    nodemap = [] of Term
    edgemap = [] of Slice(Term)
    trmap = [] of NodeAddr

    frep = ->(leaf : Leaf) do
      trmap << leaf.addr
      nodemap << leaf.node
      edgemap << leaf.edges

      leaf.node # Leave unchanged.
    end

    result = yield frep

    if nodemap.empty?
      assert edgemap.empty?
      assert trmap.empty?

      return result, nil
    end

    hg = Hypergraph.new(nodemap, edgemap)

    patches = regime.patches(hg)
    if patches.empty?
      return result, nil
    end

    patches = patches.transform_keys { |key| trmap[key] }
    patch = ->(addr : NodeAddr, scope : NodeScope, gnd : Gnd) do
      patches[addr]? || gnd.node
    end

    {result, patch}
  end

  # Steps *circuit* forward one step in time. *regime* is the regime to use
  # for rewriting, classification, etc.
  def step(clf : Classifier, regime : Regime, circuit : Term) : Term
    grp(circuit) do |grp0|
      grp1, patch = solve(regime) { |frep| step0(clf, regime, grp0, frep) }
      patch ? walk0(clf, regime, grp1, patch) : grp1
    end
  end

  # Traverses a circuit term applying a user-supplied replacement function to
  # every grounded node.
  def walk(clf : Classifier, regime : Regime, circuit : Term, &frep : Term -> Term) : Term
    grp(circuit) { |grp| walk0(clf, regime, grp, walkf(frep)) }
  end

  private def walkf(frep : Term -> Term)
    ->(_addr : NodeAddr, _scope : NodeScope, gnd : Gnd) do
      frep.call(gnd.node)
    end
  end

  # Encloses the items of *term* (a dictionary) in a `group` for the duration
  # of the block. The group is yielded and a modified group is expected back.
  private def grp(term : Term, & : Term -> Term) : Term
    unless term.type.dict?
      return term
    end

    grp0 = Term.of(term.itemspart.prepend(:group))
    grp1 = yield grp0

    Term.matchpi(grp1, %{[group children_*]}) do
      Term.of(children | term.pairspart)
    end
  end

  private def step0(clf : Classifier, regime : Regime, root : Term, frep) : Term
    fold(clf, regime, root, &step0f(frep))
  end

  private def step0f(frep : Leaf -> Term)
    ->(ctx : FoldContext, feature : Gnd | Subcircuit) do
      case feature
      in Gnd
        node0, edges = scoped(ctx.scope, feature.node, feature.edges)
        node1 = frep.call(Leaf.new(ctx.addr, node0, edges))

        # No need to unscope if we are at toplevel or node did not change.
        if ctx.scope.empty?
          return node1
        elsif node0 == node1
          return feature.node
        end

        unscope = ->(_addr : NodeAddr, _scope : NodeScope, gnd : Gnd) do
          unscoped(gnd.node, gnd.edges)
        end

        fold(ctx.copy_with(frep: walk0f(unscope)), node1)
      in Subcircuit
        result = flattenT(feature.node, range: feature.range) do |child0, index|
          # NOTE: we pass scope as-is into subcircuits just in case. I'm not sure whether
          # this is correct but it's definitely not incorrect.
          subctx = ctx.copy_with(addr: ctx.addr.append(index))

          child1, patch = solve(ctx.regime) do |frep|
            fold(subctx.copy_with(frep: step0f(frep)), child0)
          end

          if patch
            child1 = fold(subctx.copy_with(frep: walk0f(patch)), child1)
          end

          child1
        end

        fold(ctx, feature.cont.call(result))
      end
    end
  end

  private def walk0(clf : Classifier, regime : Regime, root : Term, frep)
    fold(clf, regime, root, &walk0f(frep))
  end

  private def walk0f(frep : NodeAddr, NodeScope, Gnd -> Term)
    ->(ctx : FoldContext, feature : Gnd | Subcircuit) do
      case feature
      in Gnd
        node0 = feature.node
        node1 = frep.call(ctx.addr, ctx.scope, feature)

        # No need to unscope if we are at toplevel or node did not change.
        if ctx.scope.empty?
          return node1
        elsif node0 == node1
          return feature.node
        end

        unscope = ->(_addr : NodeAddr, _scope : NodeScope, gnd : Gnd) do
          unscoped(gnd.node, gnd.edges)
        end

        fold(ctx.copy_with(frep: walk0f(unscope)), node1)
      in Subcircuit
        # Reinterpret Subcircuit as Parent because the logic is exactly the same. We don't
        # have a solve step at walk()-time.
        node1 = fold(ctx, Parent.new(feature.node, feature.range))
        fold(ctx, feature.cont.call(node1))
      end
    end
  end

  # :nodoc:
  record FoldContext,
    clf : Classifier,
    regime : Regime,
    addr : NodeAddr,
    scope : NodeScope,
    frep : FoldContext, Subcircuit | Gnd -> Term

  private def subscope(ctx : FoldContext, bindings : Term::Dict) : NodeScope
    ctx.scope.append({ctx.addr, bindings})
  end

  private def fold(clf : Classifier, regime : Regime, node : Term, &frep : FoldContext, Subcircuit | Gnd -> Term) : Term
    fold(FoldContext.new(clf, regime, NodeAddr.empty, NodeScope.empty, frep), node)
  end

  private def fold(ctx : FoldContext, node : Term) : Term
    fold(ctx, ctx.clf.call(node))
  end

  private def fold(ctx : FoldContext, feature : Inert) : Term
    feature.node
  end

  private def fold(ctx : FoldContext, feature : Gnd) : Term
    ctx.frep.call(ctx, feature)
  end

  private def fold(ctx : FoldContext, feature : Parent) : Term
    flattenT(feature.node, range: feature.range) do |child, index|
      fold(ctx.copy_with(addr: ctx.addr.append(index)), child)
    end
  end

  private def fold(ctx : FoldContext, feature : Scope) : Term
    fold(ctx.copy_with(scope: subscope(ctx, feature.bindings)), feature.cont)
  end

  private def fold(ctx : FoldContext, feature : Mixture) : Term
    feature.mix.call(feature.node, fold(ctx, feature.defn))
  end

  private def fold(ctx : FoldContext, feature : Subcircuit) : Term
    ctx.frep.call(ctx, feature)
  end

  private def flattenT(dict : Term::Dict, range : Range(Int32, Int32), &) : Term
    Term.of(flatten(dict, range) { |item, index| yield item, index })
  end

  private def flatten(dict : Term::Dict, range : Range(Int32, Int32), &) : Term::Dict
    dict.transaction do |commit|
      dict.each_item_with_index(within: range) do |item0, index|
        commit.with(index, (yield item0, index))
      end
    end
  end

  # Removes scope annotations from *edges* of *node*.
  private def unscoped(node : Term, edges : Slice(Edge))
    edges.each do |edge|
      unscoped = unscoped(edge.term)
      next if edge.term == unscoped

      node = Term.morph(node, edge.path.to_readonly_slice { |i| Term.of(i) }) { unscoped }
    end

    node
  end

  # Adds *scope* to *edges* of *node*. Returns *node* whose edges
  # are annotated with a scope, and a list of scoped edges.
  def scoped(scope : NodeScope, node : Term, edges : Slice(Edge)) : {Term, Slice(Term)}
    if scope.empty?
      return node, edges.to_readonly_slice(&.term)
    end

    scopedlst = edges.to_readonly_slice { |edge| scoped(scope, edge.term) }
    scopedlst.zip(edges) do |scoped, edge|
      node = Term.morph(node, edge.path.to_readonly_slice { |i| Term.of(i) }) { scoped }
    end

    {node, scopedlst}
  end

  # Removes scope info from *edge*.
  #
  # NOTE: this is purely convention-based. Nothing stops the user from forging
  # these. You can validate-out edges that look like these upfront though. Use
  # `scoped?`.
  private def unscoped(edge : Term) : Term
    Term.case(edge) do
      matchpi %{(%'edge (_string name_))} { Term.of(:edge, name) }
      otherwise { edge }
    end
  end

  # Returns `true` if *edge* is of the conventional scoped form. Returns
  # `false` otherwise.
  private def scoped?(edge : Term) : Bool
    Term.case(edge) do
      matchpi %{(%'edge (_string name_))} { true }
      otherwise { false }
    end
  end

  private def scoped(scope : NodeScope, edge : Term) : Term
    while entry = scope.last?
      addr, bindings = entry

      unless exterior = bindings[edge]?
        return annotated(addr, edge)
      end

      edge = exterior
      scope = scope[...-1]
    end

    edge
  end

  private def annotated(addr : NodeAddr, edge : Term) : Term
    hasher = Term::Hasher.new
    addr.each do |id|
      hasher << id
    end

    # We use Alpha48 because it is used pretty much everywhere else so clients should
    # find it "familiar".
    scope_id = Alpha48.encode(hasher.result)

    Term.matchpi(edge, %{(%'edge name_)}) do
      Term.of(:edge, {scope_id, name})
    end
  end

  # Represents a *rewrite regime*. A rewrite regime encapsulates the rules of
  # rewriting and recognition as well as the indices required to do
  # that efficiently.
  struct Regime
    # Raised when `build` detects an invalid query.
    class QueryError < Exception
    end

    # :nodoc:
    alias Step = Append | Follow | Return | FollowMany

    # :nodoc:
    #
    # Go to node labeled with *label* and matching *pattern* in context. Store
    # *pattern*'s match env along with the node itself in captures.
    defrecord Append, key : Term, pattern : Term, label : UInt32

    # :nodoc:
    #
    # Follow all links captured by the origin node's *capture*. Do not accumulate
    # solutions: search should preceed independently in each successor found.
    defrecord Follow, capture : Term

    # :nodoc:
    #
    # Follow all links captured by the origin node's *capture*. Accumulate
    # and merge solutions.
    defrecord FollowMany, capture : Term

    # :nodoc:
    #
    # Return to the predecessor in search (e.g. after following a link). Keep
    # search progress. This step functions like "lookbehind" except while looking
    # behind, you can follow more links etc.
    defrecord Return

    # :nodoc:
    #
    # Rule search plan.
    #
    # - *steps* is the sequence of `Step`s to follow if this plan is feasible.
    # - *demands* specifies which labels must be present in a circuit for this
    #   plan to be feasible.
    defrecord Plan, steps : Slice(Step), demands : Pf::USet32 do
      assert steps.size > 0
      assert demands.size > 0
    end

    # :nodoc:
    NOP_CACHE_SIZE = 256

    # :nodoc:
    def initialize(@labeler : M1::ShapeIndex, @plans : Slice(Plan), @bodies : Slice(Body))
      assert @plans.size == @bodies.size

      @nop = Set(Term::H256).new(NOP_CACHE_SIZE)
    end

    private def self.bridge(a : Term, b : Term) : Term
      mid = edges(a) & edges(b)
      unless mid.size == 1
        raise QueryError.new(
          "linked subqueries #{ML.compact(a)} and #{ML.compact(b)} must share exactly \
         one edge, but they share #{mid.size} edge(s)")
      end

      mid.first
    end

    # Returns a set of capture names of edges captured in *pattern*.
    private def self.edges(pattern : Term) : Set(Term)
      edges = Set(Term).new
      edges(pattern) do |edge|
        edges << edge
      end
      edges
    end

    private def self.edges(pattern : Term, &sink : Term ->) : Nil
      edges(pattern, sink)
    end

    private def self.edges(pattern : Term, sink : Term ->) : Nil
      normp = M1.normal(pattern)

      M1.walk(normp) do |x|
        Term.case(x) do
          matchpi %{(%'%let (%'%capture id_) (%'%edge _))} do
            sink.call(id)

            M1::WalkDecision::Skip
          end

          otherwise { M1::WalkDecision::Continue }
        end
      end
    end

    private def self.compile(query : Term, plan : Term::Dict::Commit, origin : Term?) : Nil
      Term.case(query) do
        matchpi %{(one key_ pattern_)} do
          plan << {:follow, origin} if origin
          plan << {:append, key, pattern}
        end

        matchpi %{(many key_ pattern_)} do
          unless origin
            raise QueryError.new("`many` without a predecessor makes no sense (many where?)")
          end

          plan << {:"follow+", origin}
          plan << {:append, key, pattern}
        end

        matchpi %{(link head_ deps_+)} do
          compile(head, plan, origin)

          deps.items.each do |dep|
            compile(dep, plan, bridge(head, dep))

            plan << {:return}
          end
        end

        otherwise do
          raise QueryError.new("invalid query: `#{ML.compact(query)}`")
        end
      end
    end

    private def self.compile(query : Term) : Term::Dict
      unless query.type.dict?
        raise QueryError.new("query must be a dict")
      end

      Term::Dict.build do |commit|
        compile(Term.of(query.prepend(:link)), plan: commit, origin: nil)
      end
    end

    private def self.compile(queries : Slice(Term)) : Slice(Term::Dict)
      queries.to_readonly_slice { |query| compile(query) }
    end

    private def self.plans(cqueries : Slice(Term::Dict), transcript : Slice(UInt32)) : Slice(Plan)
      cursor = 0

      cqueries.to_readonly_slice do |cquery|
        demands = Pf::USet32.new

        steps = cquery.items.to_readonly_slice do |step|
          Term.case(step) do
            matchpi %{(append name_ pattern_)} do
              label = transcript[cursor]
              demands = demands.add(label)
              cursor += 1

              Append.new(name, pattern, label)
            end

            matchpi %{(return)} { Return.new }
            matchpi %{(follow capture_)} { Follow.new(capture) }
            matchpi %{(follow+ capture_)} { FollowMany.new(capture) }
          end
        end

        Plan.new(steps, demands)
      end
    end

    # Constructs a regime and the associated indices based on *queries*
    # and their corresponding *bodies*.
    #
    # You most likely want `D7.regime` which is a DSL for calling this method.
    # Refer to `D7.regime` for info on how *queries* are written etc.
    def self.build(queries : Slice(Term), bodies : Slice(Body)) : Regime
      assert queries.size == bodies.size

      patterns = [] of {Term, Int32}

      cqueries = compile(queries)
      cqueries.each_with_index do |cquery, id|
        cquery.items.compact_map do |step|
          Term.matchpi?(step, %{(append _ pattern_)}) do
            patterns << {pattern, id}
          end
        end
      end

      labeler, transcript = M1::ShapeIndex.build(patterns.map { |pattern, _| pattern })
      plans = plans(cqueries, transcript)

      new(labeler, plans, bodies)
    end

    # Represents the body associated with a query. It receives a solution and
    # must produce a patch. The patch it produces can be empty (signifying
    # no change). The size of the patch must not exceed the number of nodes
    # participating in the solution. The patch is only allowed to modify
    # participating nodes.
    alias Body = Soln -> Patch

    # Represents a replacement of some node. Each entry is `{<node id>, <node'>}`,
    # where `<node'>` is the replacement node.
    alias Patch = Slice({NodeId, Term})

    # - *node* is the node term from the circuit.
    # - *env* is the match env of the part of the query associated with
    #   the capture (i.e. `one` or `many`).
    defrecord NodeCapture, node : Term, env : Term::Dict

    # Maps participant node ids to their corresponding captures.
    alias NodeCaptureGroup = Pf::Map(NodeId, NodeCapture)

    # Represents a solution to a query. A solution associates captures made
    # in a query, identified with their respective *key*, to a `NodeCaptureGroup`.
    class Soln
      include Enumerable({Term, NodeCaptureGroup})

      # :nodoc:
      EMPTY = new(Pf::Map(Term, NodeCaptureGroup).new, Macc256.new)

      # :nodoc:
      def initialize(@groups : Pf::Map(Term, NodeCaptureGroup), @composition : Macc256)
      end

      # Constructs an empty solution.
      def self.new
        EMPTY
      end

      # A hash identifying the *composition* of this solution, i.e. its node
      # population (a bag aka multiset). This is effectively a hash of the bag
      # of nodes in this solution, except computed online with no actual bag
      # stored anywhere.
      #
      # Used for caching solutions irrespective of participant node ids.
      def composition : Term::H256
        @composition.h256
      end

      def each(& : {Term, NodeCaptureGroup} ->) : Nil
        @groups.each { |key, group| yield({key, group}) }
      end

      def [](key : Term) : NodeCaptureGroup
        @groups[key]
      end

      # :nodoc:
      def add(key : Term, id : NodeId, capture : NodeCapture) : Soln
        Soln.new(
          groups: @groups.extend(key, NodeCaptureGroup.new, &.assoc(id, capture)),
          composition: @composition.add(capture.node),
        )
      end

      # TODO: pass specificity to initialize from origin query
      def specificity
        0
      end

      # :nodoc:
      #
      # 1. Prefer solutions with most participants.
      # 2. Prefer solutions whose queries are more specific.
      def rank
        {-sum { |_, vs| vs.size }, specificity}
      end

      def_equals_and_hash @groups
    end

    # :nodoc:
    #
    # Continuation used for implementing `Return`.
    alias Ret = Slice(Step), Soln, Sink ->

    # :nodoc:
    alias Sink = Soln ->

    # :nodoc:
    defcase SearchContext,
      hg : Hypergraph,
      graph : Slice(Pf::USet32),
      decmap : Slice(Pf::USet32),
      idecmap : Hash(UInt32, Pf::USet32)

    # :nodoc:
    defcase Locus,
      node : UInt32,
      env : Term::Dict,
      adj : Pf::USet32

    private def locus(pivot : UInt32, adj : Pf::USet32) : Locus
      Locus.new(pivot, Term[], adj)
    end

    private def locus(ctx : SearchContext, pivot : UInt32) : Locus
      locus(pivot, ctx.graph[pivot])
    end

    # :nodoc:
    record Ahead,
      steps : Slice(Step),
      ret : Ret,
      sink : Sink

    private def forward(ahead : Ahead) : {Step, Ahead}
      {ahead.steps[0], ahead.copy_with(steps: ahead.steps[1..])}
    end

    private def search(ctx, locus, step : Append, soln, ahead)
      return unless step.label.in?(ctx.decmap[locus.node])
      return if soln.any? { |_, nodes| locus.node.in?(nodes) }
      return unless env = M1.match?(step.pattern, term = ctx.hg[locus.node])

      capture = NodeCapture.new(term, env)

      search(ctx, locus.copy_with(env: env), soln.add(step.key, locus.node, capture), ahead)
    end

    private def search(ctx, locus, step : Follow, soln, ahead)
      edge = locus.env[step.capture]

      ret = Ret.new do |steps, soln, sink|
        search(ctx, locus, soln, ahead.copy_with(steps: steps, sink: sink))
      end

      locus.adj.each do |neighbor|
        next unless ctx.hg.member?(neighbor, edge)

        search(ctx, locus(ctx, neighbor), soln, ahead.copy_with(ret: ret))
      end
    end

    private def search(ctx, locus, step : FollowMany, soln, ahead)
      edge = locus.env[step.capture]

      ret = Ret.new do |steps, soln, sink|
        search(ctx, locus, soln, ahead.copy_with(steps: steps, sink: sink))
      end

      solns = [] of Soln

      locus.adj.each do |neighbor|
        next unless ctx.hg.member?(neighbor, edge)

        sink = Sink.new { |fsoln| solns << fsoln }

        search(ctx, locus(ctx, neighbor), soln, ahead.copy_with(sink: sink, ret: ret))
      end

      return if solns.empty?

      palette = Soln.new

      solns.each do |soln|
        soln.each do |key, nodes|
          nodes.each do |node, capture|
            # TODO: assert that `merge` does not collide
            # TODO: more efficient impl: can't we reuse *something*?
            palette = palette.add(key, node, capture)
          end
        end
      end

      ahead.sink.call(palette)
    end

    private def search(ctx, locus, step : Return, soln, ahead)
      ahead.ret.call(ahead.steps, soln, ahead.sink)
    end

    private def search(ctx, locus, soln, ahead)
      if ahead.steps.empty?
        ahead.sink.call(soln)
        return
      end

      step, ahead1 = forward(ahead)

      search(ctx, locus, step, soln, ahead1)
    end

    private def search(ctx : SearchContext, plan : Plan, sink : Sink) : Nil
      ret = Ret.new do
        raise "BUG: Return without a predecessor"
      end

      # Optimization: start at appropriately labeled nodes right away.
      step = plan.steps[0]
      if step.is_a?(Append)
        nodes = ctx.idecmap[step.label]
        nodes.each do |pivot|
          search(ctx, locus(ctx, pivot), step, Soln.new, Ahead.new(plan.steps[1..], ret, sink))
        end
        return
      end

      ctx.graph.each_with_index do |adj, pivot|
        search(ctx, locus(pivot.to_u32, adj), step, Soln.new, Ahead.new(plan.steps[1..], ret, sink))
      end
    end

    private def original?(soln : Soln, id : NodeId) : Term?
      soln.each do |_, captures|
        next unless capture = captures[id]?
        return capture.node
      end
    end

    private def original(soln : Soln, id : NodeId) : Term
      original?(soln, id) || raise KeyError.new
    end

    private def patches(solns : Array({Soln, Body})) : Hash(NodeId, Term)
      patches = {} of NodeId => Term

      used = Pf::USet32.new

      solns.each do |soln, body|
        composition = soln.composition
        next if composition.in?(@nop)

        arity = soln.sum { |_, grp| grp.size }
        patch = body.call(soln)
        assert patch.size <= arity, "patch-solution arity mismatch (#{patch.size} > #{arity})"

        modifies = Pf::USet32.transaction do |commit|
          patch.each do |node, rep|
            next if original(soln, node) == rep

            commit << node
          end
        end

        # Cache nop solutions so that we can skip them for some time in
        # the future.
        if modifies.empty?
          if @nop.size > NOP_CACHE_SIZE
            @nop.delete(@nop.first)
          end
          @nop << composition
        end

        # Abort transaction if any node modified by the rule was modified by
        # someone else already.
        next if modifies.intersects?(used)

        # Commit.
        used |= modifies
        patch.each do |(node, rep)|
          assert patches.put?(node, rep)
        end
      end

      patches
    end

    private def patches(ctx : SearchContext, candidates : Array({Plan, Body})) : Hash(NodeId, Term)
      if candidates.empty?
        return {} of NodeId => Term
      end

      solns = [] of {Soln, Body}

      candidates.each do |plan, body|
        sink = Sink.new do |soln|
          solns << {soln, body}
        end

        search(ctx, plan, sink)
      end

      solns.sort_by! { |soln, _| soln.rank }

      patches(solns)
    end

    # Returns the patches (node assignments) to apply to nodes in *hg* in
    # its current state, according to this rewrite regime, to transition it
    # into the next time-step.
    #
    # This method performs one rewrite tick of this regime. The caller is
    # responsbile for actually merging changes back into the circuit, based
    # on node id correspondence etc.
    def patches(hg : Hypergraph) : Hash(NodeId, Term)
      decmap = Slice(Pf::USet32).new(hg.order) { Pf::USet32.new }
      idecmap = {} of UInt32 => Pf::USet32
      population = Pf::USet32.new

      hg.each_node_with_id do |node, id|
        decmap[id] = decomp = @labeler.decompose(node)
        population |= decomp

        decomp.each do |label|
          idecmap[label] = (idecmap[label]? || Pf::USet32.new).add(id)
        end
      end

      candidates = [] of {Plan, Body}

      @plans.zip(@bodies) do |plan, body|
        next unless plan.demands.subset_of?(population)

        candidates << {plan, body}
      end

      ctx = SearchContext.new(hg, hg.graph, decmap.readonly, idecmap)

      patches(ctx, candidates)
    end
  end

  # DSL for constructing a rewrite regime, `Regime`.
  #
  # ```
  # D7.regime do
  #   rule %{(one dev [discard @tgt_]) (many tgt [cell @tgt_ _])} do
  #     patch(tgt, &.morph({2, nil}))
  #   end
  # end
  # ```
  #
  # TODO: document query format.
  macro regime(&block)
    {%
      unless block
        raise "regime expects a block containing `rule` branches"
      end

      stmts = block.body
      if stmts.is_a?(Expressions)
        stmts = stmts.expressions
      elsif stmts.is_a?(Nop)
        stmts = [] of ::NoReturn
      else
        stmts = [stmts]
      end

      branches = [] of ::NoReturn

      stmts.each do |stmt|
        unless stmt.is_a?(Call) && stmt.name == :rule && stmt.args.size >= 1 && stmt.block
          stmt.raise "regime: expected a call to `rule(*patterns : String, &)`"
        end

        stmt.args.each do |pattern|
          matches = pattern.scan(/\((?:one|many)\s(\w+)/)
          participants = matches.map { |match| match[1].id }
          branches << {pattern: pattern, participants: participants, body: stmt.block.body}
        end
      end

      if branches.empty?
        block.raise "expected at least one `rule` branch"
      end
    %}\

    %queries = [
      {% for branch in branches %}\
        ::Ww::ML.terms({{branch[:pattern]}}),
      {% end %}\
    ]

    %bodies = [
      {% for branch in branches %}\
        {{@type}}::Regime::Body.new do |%soln|
          {% for participant in branch[:participants] %}\
            {{participant}} = %soln[Term.of({{participant.symbolize}})]
          {% end %}\

          %result = pass do
            {{branch[:body]}}
          end

          %result || Slice({UInt32, Term}).empty
        end,
      {% end %}\
    ]

    {{@type}}::Regime.build(%queries.to_readonly_slice(&.itself), %bodies.to_readonly_slice(&.itself))
  end
end
