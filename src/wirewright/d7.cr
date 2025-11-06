# Wirewright Delta7 (D7 for short) is a *symbolic physics engine*. In a sense,
# it is just like a *physics engine* (think Box2D), but instead of working with
# bodies, it works on *symbols*. Rather than solving equations, D7 searches for
# relationships between symbols according to constraints. Instead of manipualting
# velocity and position, D7 *rewrites*.
#
# At its core, D7 is an attempt to model *autopoiesis* as described by Maturana,
# Varela, and others.
#
# See, for instance, *Autopoiesis: the organization of living systems, its
# characterization and a model* by Varela, Maturana & Uribe (1974). D7 is trying
# to check all the boxes in section 9, "Key".
#
# I think autopoiesis can be modeled in any "physical simulator". The only problem
# is that in practice, the physical simulators we build are too unstructured. It's
# "perceptually hard" to extract useful info from it, both for us as observers and
# for the entities within the simulation. Imagine how much intrinsic structure
# a particle simulator would require to start recognizing or matching on its own
# configuration or its parts? With D7, it's as simple as a pattern match on a fragment.
#
# Imagine a game. How hard would it be to get a car to drive itself in that game, given
# only the game's visual output and keyboard input, that is, "as an outside agent"?
#
# We know the answer: very hard. That's why people resort to black box (ish) methods
# like neural networks.
#
# If only we had a *symbolic* physics simulator, with the same or similar kinds
# of behaviors, but with structures easy to pattern match and construct
# programmatically, "as an outside agent"...
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
  # :nodoc:
  alias NodeId = UInt32

  # A hypergraph is a graph whose edges can include any number of nodes; each edge
  # is a subset of the set of nodes in that graph. It's easier to think of a hypergraph
  # as a community of nodes. Each node can participate in zero or more groups, each
  # group formed from other nodes in the community.
  struct Hypergraph
    # - *nodemap* maps nodes to node ids (implcit, array index).
    # - *edgemap* maps node ids (implicit, array index) to hyperedges that node
    #   is participating in.
    # - *trmap* maps node ids (implicit, array index) to node qualpaths.
    def initialize(@nodemap : Array(Term), @edgemap : Array(Slice(Term)))
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
  end

  struct RegimeIndex
    defcase Pattern,
      index : Int32,
      term : Term,
      specificity : M1::Specificity

    defrecord Recipe,
      ingredients : Bag(UInt32),
      order : Array(UInt32),
      pattern : Pattern

    # :nodoc:
    def initialize(@ingredients : M1::ShapeIndex, @recipes : Array(Recipe))
    end

    # Constructs a regime index based on an indexable of *patterns*.
    #
    # NOTE: *patterns* is stored internally and is assumed to be immutable at this point.
    def self.build(patterns : Indexable(Term)) : RegimeIndex
      singulars = [] of {Term, Pattern}

      # Collect N-tuple elements and which pattern they come from to *singulars*.
      patterns.each_with_index do |pattern, index|
        normp = M1.normal(pattern)

        Term.case(normp) do
          matchpi(
            %{(%'%literal (_+))},
            %{(%'%itemseq (%past (%'%singular _) min: 1))},
          ) do
            object = Pattern.new(index, pattern, M1.specificity(normp, toplevel: true))

            pattern.items.each do |element|
              singulars << {element, object}
            end
          end

          otherwise do
            raise ArgumentError.new("unrecognized pattern, expected a singular-only itemseq: `#{ML.compact(pattern)}`")
          end
        end
      end

      recipes = {} of Int32 => Recipe

      # Build a shape index for *singulars*.
      ingredients, transcript = M1::ShapeIndex.build(singulars.map { |ingredient, _| ingredient })

      # Associate pattern indices with conjunctions of ingredients that we'll refer to as *recipes*.
      singulars.zip(transcript) do |(singular, pattern), ingredient|
        recipe = recipes.put_if_absent(pattern.index) { Recipe.new(Bag(UInt32).new, [] of UInt32, pattern) }
        recipe.ingredients << ingredient
        recipe.order << ingredient
      end

      new(ingredients, recipes.to_a { |_, recipe| recipe })
    end

    # A *reaction* is a conjunction of nodes from the hypergraph that satisfies
    # a *recipe* stored in the regime index. Effectively, `Recipe` represents
    # a hypergraph pattern match.
    #
    # - *index* is the index of the pattern that matched in the indexable of patterns
    #   passed to `build`.
    # - *pattern* is the M1 pattern term itself.
    # - *matchee* is a synthesized matchee to which the pattern should be
    #   applied for further checking.
    # - *participants* is a read-only slice of nodes from the hypergraph that
    #   participate in the reaction.
    #
    # NOTE: Like with `M1::ShapeIndex`, that a reaction was emitted does not
    # necessarily mean *pattern* will match *matchee*. The fact that a reaction
    # occured is a pre-match step to prune outright wrong (wrt the hypergraph)
    # node combinations early.
    defrecord Reaction,
      pattern : Pattern,
      matchee : Term,
      participants : Slice(NodeId)

    # :nodoc:
    class Assignment
      K_UNDEFINED = UInt32::MAX

      EMPTY = new(Pf::Map(UInt32, UInt32).new, vs: Pf::USet32.new)

      def initialize(@map : Pf::Map(UInt32, UInt32), @vs : Pf::USet32)
      end

      def self.new
        EMPTY
      end

      def uniq? : Bool
        @vs.size >= @map.size
      end

      def includes?(key : UInt32) : Bool
        @map.includes?(key)
      end

      def each(& : UInt32, UInt32 ->) : Nil
        @map.each { |key, value| yield key, value }
      end

      def invert : Hash(UInt32, UInt32)
        @map.to_h { |value, key| {key, value} }
      end

      def inverted_index : Hash(UInt32, Array(UInt32))
        index = Hash(UInt32, Array(UInt32)).new(initial_capacity: @vs.size)

        each do |node, ingredient|
          ingredients = index.put_if_absent(ingredient) { [] of UInt32 }
          ingredients << node
        end

        index
      end

      def assoc(key : UInt32, value : UInt32) : Assignment
        Assignment.new(@map.assoc(key, value), @vs.add(value))
      end

      def_equals_and_hash @map
    end

    # Prunes *recipes* based on an assignment. Yields final assignments.
    private def prune(recipes : Array(Recipe), assignment : Assignment, & : Recipe, Assignment ->) : Array(Recipe)
      ingredients = Bag(UInt32).new
      assignment.each do |_, ingredient|
        ingredients << ingredient
      end

      recipes.select do |recipe|
        subset = ingredients.subset_of?(recipe.ingredients)

        if subset && ingredients.size == recipe.ingredients.size
          yield recipe, assignment
          false # reject final
        elsif subset
          true # select valid
        else
          false # reject invalid
        end
      end
    end

    private def search(seen, recipes0, graph, ingredients, pivot, field, assignment0, sink)
      return if pivot.in?(assignment0) # Already assigned

      ingredients[pivot].each do |ingredient|
        assignment1 = assignment0.assoc(pivot, ingredient)
        next unless seen.add?(assignment1)

        recipes1 = prune(recipes0, assignment1, &sink)
        next if recipes1.empty?

        field.each do |relative|
          search(seen, recipes1, graph, ingredients, relative, field | graph[relative], assignment1, sink)
        end
      end
    end

    private def accept(hg, sink)
      ->(recipe : Recipe, a : Assignment) do
        # Fast path that's taken very frequently: all assignment values are
        # unique, no need for Cartesian product.
        if a.uniq?
          options = a.invert
          argmt = recipe.order.to_readonly_slice { |ingredient| options[ingredient] }
          matchee = Term::Dict.build &.concat(argmt) { |participant| hg[participant] }
          reaction = Reaction.new(recipe.pattern, Term.of(matchee), argmt)
          sink.call(reaction)
          return
        end

        # TODO: use our own each cartesian here to avoid allocating arrays
        options = a.inverted_index
        palette = recipe.order.map { |ingredient| options[ingredient] }

        Indexable.each_cartesian(palette, reuse: true) do |argmt|
          matchee = Term::Dict.build do |commit|
            commit.concat(argmt) { |participant| hg[participant] }
          end

          reaction = Reaction.new(recipe.pattern, Term.of(matchee), argmt.to_readonly_slice)

          sink.call(reaction)
        end
      end
    end

    private def ingredients_and_graph(hg : Hypergraph)
      groups = {} of Term => Pf::USet32
      ingredients = {} of NodeId => Pf::USet32

      hg.each_node_with_id do |node, id|
        ingredients[id] = @ingredients.decompose(node)

        hg.each_edge(id) do |edge|
          groups[edge] = (groups[edge]? || Pf::USet32.new).add(id)
        end
      end

      # Compute an unordered, clique-expanded graph from the *circuit* hypergraph:
      # nodes that share a hyperedge are connected, and each unordered edge is
      # represented with a pair of edges going in opposite directions.
      graph = {} of NodeId => Pf::USet32

      groups.each do |_, members|
        members.each do |u|
          members.each do |v|
            next if u == v

            vs = graph[u]? || Pf::USet32.new
            next if v.in?(vs)

            graph[u] = vs.add(v)
          end
        end
      end

      {ingredients, graph}
    end

    # Calls *sink* with reactions found in the hypergraph *hg*.
    def solve(hg : Hypergraph, &sink : Reaction ->) : Nil
      ingredients, graph = ingredients_and_graph(hg)
      handler = accept(hg, sink)
      seen = Set(Assignment).new
      graph.each do |pivot, field|
        search(seen, @recipes, graph, ingredients, pivot, field, Assignment.new, handler)
      end
    end
  end

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

    patches = regime.patch(hg)
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
  def step(regime : Regime, circuit : Term) : Term
    grp(circuit) do |grp0|
      grp1, patch = solve(regime) { |frep| step0(regime, grp0, frep) }
      patch ? walk0(regime, grp1, patch) : grp1
    end
  end

  # Traverses a circuit term applying a user-supplied replacement function to
  # every grounded node.
  def walk(regime : Regime, circuit : Term, &frep : Term -> Term) : Term
    grp(circuit) { |grp| walk0(regime, grp, walkf(frep)) }
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

  private def step0(regime : Regime, root : Term, frep) : Term
    fold(regime, root, &step0f(frep))
  end

  private def step0f(frep : Leaf -> Term)
    ->(ctx : FoldContext, feature : Gnd | Subcircuit) do
      case feature
      in Gnd
        node0, edges = scoped(ctx.scope, feature.node, feature.edges)
        node1 = frep.call(Leaf.new(ctx.addr, node0, edges))

        # It's not much, but skip doing the unscoping fold() if the node did
        # not change.
        if node0 == node1
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

  private def walk0(regime : Regime, root : Term, frep)
    fold(regime, root, &walk0f(frep))
  end

  private def walk0f(frep : NodeAddr, NodeScope, Gnd -> Term)
    ->(ctx : FoldContext, feature : Gnd | Subcircuit) do
      case feature
      in Gnd
        frep.call(ctx.addr, ctx.scope, feature)
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
    regime : Regime,
    addr : NodeAddr,
    scope : NodeScope,
    frep : FoldContext, Subcircuit | Gnd -> Term

  private def subscope(ctx : FoldContext, bindings : Term::Dict) : NodeScope
    ctx.scope.append({ctx.addr, bindings})
  end

  private def fold(regime : Regime, node : Term, &frep : FoldContext, Subcircuit | Gnd -> Term) : Term
    fold(FoldContext.new(regime, NodeAddr.empty, NodeScope.empty, frep), node)
  end

  private def fold(ctx : FoldContext, node : Term) : Term
    fold(ctx, ctx.regime.classify(node))
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

  # Adds *scope* to *edges* of *node*.
  private def scoped(scope : NodeScope, node : Term, edges : Slice(Edge))
    scopedlst = edges.to_readonly_slice { |edge| scoped(scope, edge.term) }
    scopedlst.zip(edges) do |scoped, edge|
      node = Term.morph(node, edge.path.to_readonly_slice { |i| Term.of(i) }) { scoped }
    end

    {node, scopedlst}
  end

  private def unscoped(edge : Term) : Term
    Term.case(edge) do
      matchpi %{(%'edge (#scope _ name_))} { Term.of(:edge, name) }
      otherwise { edge }
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
    Term.case(edge) do
      matchpi %{(%'edge name_)} { Term.of(:edge, {:"#scope", addr, name}) }
      otherwise { edge }
    end
  end

  # Represents a *rewrite regime*. A rewrite regime encapsulates the rules of
  # rewriting and recognition
  class Regime
    # :nodoc:
    def initialize(
      @classify : Term -> Feature,
      @index : RegimeIndex,
      @handle : Int32, Term::Dict -> Slice(Term),
      @dump : Int32 -> String,
    )
    end

    def classify(node : Term) : Feature
      @classify.call(node)
    end

    def patch(hg : Hypergraph) : Hash(NodeId, Term)
      # Collect reactions.
      reactions = [] of RegimeIndex::Reaction
      @index.solve(hg) do |reaction|
        reactions << reaction
      end

      # Prefer reactions with most participants. Prefer reactions that are
      # more specific. If both are the same, use randomness.
      reactions.sort_by! { |r| {-r.participants.size, r.pattern.specificity, rand} }

      # Collect patches to nodes.
      patches = {} of NodeId => Term

      _ = Pf::USet32.transaction do |used|
        reactions.each do |reaction|
          next if reaction.participants.any? &.in?(used)

          # NOTE: we must give the regime plenty of chances to back away. Pattern mismatch
          # is the obvious way; a less obvious way is "no change". The latter is used deliberately
          # on the regime side as a kind of "continue", as in: ignore me, some interior constraints
          # did not match, move on.
          next unless env = M1.match?(reaction.pattern.term, reaction.matchee)

          reps = @handle.call(reaction.pattern.index, env)
          assert reps.size == reaction.participants.size, @dump.call(reaction.pattern.index)

          reaction.participants.zip(reaction.matchee.items, reps) do |participant, orig, rep|
            next if orig == rep

            assert patches.put?(participant, rep)

            used << participant
          end
        end
      end

      patches
    end

    Term::Case.defcase build(classify) do |id, branches, sink|
      {% begin %}
        {% if sink %}
          {% sink.raise "`otherwise` makes no sense in Regime" %}
        {% end %}

        pass do
          %index = RegimeIndex.build([{{branches.map { |branch| branch[:pattern][:call] }.splat}}] of Term)

          %handle = ->(%index : Int32, %env : Term::Dict) do
            case %index
            {% for branch, i in branches %}
            when {{i}}
              pass do
                {% for capture, var, j in branch[:captures] %}\
                  %value{i, j} = %env[{{capture}}]? || raise("#{ {{branch[:location]}} }: missing capture `{{capture.id}}`")
                  {% if type = branch[:cast][var] %}\
                    {{var.id}} = %value{i, j}.to({{type}})
                  {% else %}\
                    {{var.id}} = %value{i, j}
                  {% end %}\
                {% end %}\
                {{branch[:body]}}
              end
            {% end %}
            else
              unreachable
            end
          end

          %dump = ->(%index : Int32) do
            { {{ branches.map { |branch| branch[:pattern][:src] }.splat }} }[%index]
          end

          {{@type}}.new({{classify}}, %index, %handle, %dump)
        end
      {% end %}
    end
  end
end
