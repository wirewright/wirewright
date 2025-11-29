module Ww::D7
  # An integer used to identify a node in a hypergraph. Node ids are
  # usually hypergraph-bounded rendition of circuit-bounded `NodeAddr`.
  alias NodeId = UInt32

  # Maps node ids to replacement terms. The granularity is a node; we
  # do not go lower than that.
  alias Patch = Hash(NodeId, Term)

  # The address of a node in a circuit.
  alias NodeAddr = Slice(Int32)

  # Records the scopes that the traversal process passes through. The addr
  # is that of the scope (e.g. `module`, but in general, see `Scope`), and
  # the dict is its bindings dict.
  alias NodeScope = Slice({NodeAddr, Term::Dict})

  # Represents a *rewrite regime*.
  #
  # A rewrite regime is lots of black magic for searching in and rewriting
  # hypergraphs according to a database of queries.
  class Regime
    # Raised when `build` detects an invalid query.
    class QueryError < Exception
    end

    # :nodoc:
    alias EdgeCapture = EdgeSingleton | EdgeList

    # :nodoc:
    defrecord EdgeSingleton, id : Term

    # :nodoc:
    defrecord EdgeList, id : Term

    # :nodoc:
    alias Step = Append | Follow | Return

    # :nodoc:
    #
    # Go to node labeled with *label* and matching *pattern* in context. Store
    # *pattern*'s match env along with the node itself in captures.
    defrecord Append, label : UInt32, key : Term, pattern : Term, edges : Set(EdgeCapture)

    # :nodoc:
    #
    # Follow all links captured by the origin node's *capture*. Accumulate
    # and merge solutions. *min* and *max* define bounds on the number of
    # solutions that are required (both inclusive), otherwise backtrack.
    defrecord Follow, capture : EdgeCapture, min : Int32, max : Int32

    # :nodoc:
    #
    # Return to the predecessor in search (e.g. after following a link), while
    # keeping search progress.
    defrecord Return

    # :nodoc:
    #
    # Rule search plan.
    #
    # - *steps* is the sequence of `Step`s to follow if this plan is feasible.
    # - *demands* specifies which labels must be present in the matched hypergraph
    #   for this plan to be feasible.
    defrecord Plan, steps : Slice(Step), demands : Pf::USet32 do
      assert steps.size > 0
      assert demands.size > 0
    end

    # :nodoc:
    def initialize(@labeler : M1::ShapeIndex, @plans : Slice(Plan))
    end

    # :nodoc:
    alias StepIR = Follow | AppendIR | Return

    # :nodoc:
    #
    # During the IR stage, we don't yet know the label.
    defrecord AppendIR, key : Term, pattern : Term, edges : Set(EdgeCapture)

    # :nodoc:
    defrecord NodeAdded
    # :nodoc:
    defrecord LinkAdded

    # Returns the associated with *query*.
    private def self.pattern(query : Term) : Term
      Term.case(query) do
        matchpi %{[one _ pattern_]} { pattern }
        matchpi %{[many _ pattern_]} { pattern }
        matchpi %{[link head_ _+]} { pattern(head) }
      end
    end

    private def self.edges(edgetab, pattern : Term) : Set(EdgeCapture)
      edgetab.put_if_absent(pattern) do
        edges = Set(EdgeCapture).new
        edges(pattern) { |edge| edges << edge }
        edges
      end
    end

    private def self.edges(pattern : Term, &sink : EdgeCapture ->) : Nil
      edges(pattern, sink)
    end

    private def self.edges(pattern : Term, sink : EdgeCapture ->) : Nil
      normp = M1.normal(pattern)

      M1.walk(normp) do |x|
        Term.case(x) do
          matchpi %{(%'%let (%'%capture id_) (%'%edge _))} do
            sink.call(EdgeSingleton.new(id))

            M1::WalkDecision::Skip
          end

          matchpi %{(%'%let (%'%capture id_) (%'%itemseq (%'%past %'(%singular (%edge _)) ⍊ min: 1)))} do
            sink.call(EdgeList.new(id))

            M1::WalkDecision::Skip
          end

          otherwise { M1::WalkDecision::Continue }
        end
      end
    end

    private def self.bridge?(l : Set(EdgeCapture), r : Set(EdgeCapture)) : EdgeCapture?
      l.each do |capture0|
        if r.any? { |capture1| capture0.id == capture1.id }
          return capture0
        end
      end
    end

    private def self.compile(edgetab, query : Term, steps : Array(StepIR), origin : EdgeCapture?) : NodeAdded | LinkAdded
      Term.case(query) do
        matchpi %{(one key_ pattern_)} do
          if origin
            steps << Follow.new(origin, min: 1, max: 1)
          end

          steps << AppendIR.new(key, pattern, edges(edgetab, pattern))

          NodeAdded.new
        end

        matchpi %{(many key_ pattern_ ⍊ min_: (%optional 1 (%number +i32!)))} do
          unless origin
            raise QueryError.new("`many` without a predecessor makes no sense (many where?)")
          end

          steps << Follow.new(origin, min: 1, max: Int32::MAX)
          steps << AppendIR.new(key, pattern, edges(edgetab, pattern))

          NodeAdded.new
        end

        matchpi %{(link head_ deps_+)} do
          status = compile(edgetab, head, steps, origin)
          unless status.is_a?(NodeAdded)
            raise QueryError.new("`link` must have a node at its head (`one` or `many`)")
          end

          edges0 = edges(edgetab, pattern(head))
          deps.items.each do |dep|
            edges1 = edges(edgetab, pattern(dep))

            unless bridge = bridge?(edges0, edges1)
              raise QueryError.new(
                "linked queries #{ML.compact(head)} and #{ML.compact(dep)} must share exactly \
                 one edge (bridging from one to the other), but they share no edges")
            end

            compile(edgetab, dep, steps, origin: bridge)

            steps << Return.new
          end

          LinkAdded.new
        end

        otherwise do
          raise QueryError.new("invalid query: `#{ML.compact(query)}`")
        end
      end
    end

    private def self.compile(edgetab, query : Term)
      unless query.type.dict?
        raise QueryError.new("query must be a dict")
      end

      steps = [] of StepIR

      if query.itemsize == 1
        compile(edgetab, query[0], steps, origin: nil)
      else
        compile(edgetab, Term.of(query.prepend(:link)), steps, origin: nil)
      end

      steps
    end

    private def self.compile(queries : Slice(Term))
      edgetab = Hash(Term, Set(EdgeCapture)).new(initial_capacity: 16)

      queries.to_readonly_slice { |query| compile(edgetab, query) }
    end

    # Constructs a regime and the associated indices based on *queries*
    # and a corresponding *body*. *body* will receive indices of the matching
    # query alongside the solution later on.
    def self.build(queries : Slice(Term)) : Regime
      patterns = [] of Term

      cqueries = compile(queries)
      cqueries.each do |cquery|
        cquery.each do |step|
          next unless step.is_a?(AppendIR)

          patterns << step.pattern
        end
      end

      labeler, transcript = M1::ShapeIndex.build(patterns)

      cursor = 0
      plans = cqueries.to_readonly_slice do |cquery|
        demands = Pf::USet32.new

        steps = cquery.to_readonly_slice do |step|
          case step
          in AppendIR
            label = transcript[cursor]
            demands = demands.add(label)
            cursor += 1 # Corresponds to additions to `patterns` above.

            Append.new(label, step.key, step.pattern, step.edges)
          in Follow, Return
            step
          end
        end

        Plan.new(steps, demands)
      end

      new(labeler, plans)
    end

    # Represents the body associated with a query. It receives a solution along
    # with the index of the associated query, and must produce a patch. The patch
    # can be empty (signifying no change). The size of the patch must not exceed
    # the number of nodes participating in the solution. The patch is only allowed
    # to modify participating nodes.
    alias Body = Soln, Int32 -> Patch

    # - *addr* is the address of the node.
    # - *node* is the node term from the circuit.
    # - *env* is the match env of the part of the query associated with
    #   the capture (i.e. `one` or `many`).
    # - *edges* is the edge population of *env*.
    defrecord NodeCapture,
      addr : NodeAddr,
      node : Term,
      env : Term::Dict,
      edges : Set(EdgeCapture)

    # Maps participant node ids to their corresponding captures.
    alias NodeCaptureGroup = Pf::Map(NodeId, NodeCapture)

    # Represents a solution to a query. A solution associates captures made
    # in a query, identified with their respective *key*, to a `NodeCaptureGroup`.
    class Soln
      # :nodoc:
      EMPTY = new(groups: Pf::Map(Term, NodeCaptureGroup).new, participants: Pf::USet32.new)

      getter groups : Pf::Map(Term, NodeCaptureGroup)
      getter participants : Pf::USet32

      # :nodoc:
      def initialize(@groups : Pf::Map(Term, NodeCaptureGroup), @participants : Pf::USet32)
      end

      # Constructs an empty solution.
      def self.new
        EMPTY
      end

      def self.union(solns : Indexable(Soln)) : Soln
        assert solns.present?

        groups = solns[0].groups
        participants = solns[0].participants

        solns.each(within: 1...solns.size) do |soln|
          # TODO: assert that `merge` does not collide
          groups = groups.merge(soln.groups) { |_, group0, group1| group0.merge(group1) }
          participants |= soln.participants
        end

        new(groups, participants)
      end

      def includes?(node : NodeId) : Bool
        participants.includes?(node)
      end

      # :nodoc:
      struct Captures
        include Enumerable({EdgeCapture, Term})

        def initialize(@soln : Soln)
        end

        def each(&) : Nil
          @soln.groups.each do |_, group|
            group.each do |_, nc|
              nc.edges.each do |edge|
                yield({edge, nc.env[edge.id]})
              end
            end
          end
        end
      end

      def captures : Enumerable({EdgeCapture, Term})
        Captures.new(self)
      end

      # :nodoc:
      def add(key : Term, id : NodeId, capture : NodeCapture) : Soln
        participants1, added = participants.add?(id)
        unless added
          raise ArgumentError.new("duplicate node assignment")
        end

        Soln.new(groups.extend(key, NodeCaptureGroup.new, &.assoc(id, capture)), participants1)
      end

      # TODO: pass specificity to initialize from origin query
      def specificity
        0
      end

      # Returns the rank of this solution.
      #
      # 1. Prefer solutions with most participants.
      # 2. Prefer solutions whose queries are more specific.
      #
      # NOTE: assumes sort is ascending.
      def rank
        {-groups.sum { |_, group| group.size }, specificity}
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
      addr : NodeAddr,
      node : NodeId,
      env : Term::Dict,
      adj : Pf::USet32

    private def locus(ctx : SearchContext, pivot : NodeId) : Locus
      Locus.new(addr: ctx.hg.addr(pivot), node: pivot, env: Term[], adj: ctx.graph[pivot])
    end

    # :nodoc:
    record Ahead, steps : Slice(Step), ret : Ret, sink : Sink

    private def forward(ahead : Ahead) : {Step, Ahead}
      {ahead.steps[0], ahead.copy_with(steps: ahead.steps[1..])}
    end

    private def edge_ids(env : Term::Dict, capture : EdgeSingleton) : Indexable(Term)
      {env[capture.id]}
    end

    private def edge_ids(env : Term::Dict, capture : EdgeList) : Indexable(Term)
      env[capture.id].items
    end

    private def consistent?(a : EdgeSingleton, av : Term, b : EdgeSingleton, bv : Term) : Bool
      a.id != b.id || av == bv
    end

    private def consistent?(a : EdgeSingleton, av : Term, b : EdgeList, bv : Term) : Bool
      a.id != b.id || (bv.type.dict? && bv.items.any?(av)) # ?!
    end

    private def consistent?(a : EdgeList, av : Term, b : EdgeSingleton, bv : Term) : Bool
      consistent?(b, bv, a, av)
    end

    private def consistent?(a : EdgeList, av : Term, b : EdgeList, bv : Term) : Bool
      a.id != b.id || av == bv # ?!
    end

    private def search(ctx, locus, step : Append, soln, ahead)
      return unless step.label.in?(ctx.decmap[locus.node])
      return if locus.node.in?(soln)
      return unless env = M1.match?(step.pattern, term = ctx.hg[locus.node])

      # NOTE: In patterns such as:
      #
      #   (one (qux @a_)) (one (mid @a_ @b_)) (one (qux @b_))
      #
      # We require @a and @b to be constraint-checked for correctness. But we're not
      # using M1 on the totality of the pattern, just on the pieces. Each `one`'s pattern
      # is distrinct, so M1's constraint checking machinery is of no use to us here.
      # We have to do it ourselves.
      #
      # Here we use info from the query compiler (mainly step.edges) to inspect each
      # previous env, making sure decisions we've already made are consistent with
      # the one we're about to make.
      consistent = step.edges.all? do |capture1|
        value1 = env[capture1.id]

        soln.captures.all? do |capture0, value0|
          consistent?(capture0, value0, capture1, value1)
        end
      end

      return unless consistent

      capture = NodeCapture.new(locus.addr, term, env, step.edges)

      search(ctx, locus.copy_with(env: env), soln.add(step.key, locus.node, capture), ahead)
    end

    private def search(ctx, locus, step : Follow, soln, ahead)
      edge_ids = edge_ids(locus.env, step.capture)

      # Find which hyperedges correspond to which captured edge ids for this
      # particular node.
      edges = edge_ids.to_readonly_slice do |edge_id|
        needle = nil

        ctx.hg.each_edge(locus.node) do |edge|
          next unless edge_id == edge.term

          needle = edge
          break
        end

        needle || raise Enumerable::NotFoundError.new
      end

      # Determine viable successors. Simultaneously, make sure that all edges we
      # want to Follow are present in neighbors, not just *some*.
      successors = Pf::USet32.transaction do |commit|
        edges.each do |edge|
          found = false

          locus.adj.each do |neighbor|
            next if neighbor.in?(soln)
            next unless ctx.hg.member?(neighbor, edge)

            found = true
            commit << neighbor
          end

          # We must have at least one neighbor on all edges that we want
          # to follow.
          return unless found
        end
      end

      solns = [] of Soln
      sink = Sink.new { |final| solns << final }
      ret = Ret.new do |steps, soln, sink|
        search(ctx, locus, soln, ahead.copy_with(steps: steps, sink: sink))
      end

      successors.each do |neighbor|
        search(ctx, locus(ctx, neighbor), soln, ahead.copy_with(sink: sink, ret: ret))
      end

      return unless step.min <= solns.size <= step.max

      ahead.sink.call(Soln.union(solns))
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

      ctx.graph.each_with_index do |_adj, pivot|
        # NOTE: even though we know adj here and could pass it to locus(), I don't
        # think it's expensive to fetch it twice, and the benefit for us is not
        # having odd locus() overloads.
        search(ctx, locus(ctx, pivot.to_u32), step, Soln.new, Ahead.new(plan.steps[1..], ret, sink))
      end
    end

    private def original?(soln : Soln, id : NodeId) : Term?
      soln.groups.each do |_, group|
        next unless capture = group[id]?
        return capture.node
      end
    end

    private def original(soln : Soln, id : NodeId) : Term
      original?(soln, id) || raise KeyError.new
    end

    private def solve(solns : Array({Soln, Int32}), body : Body) : Patch
      used = Pf::USet32.new
      reactions = {} of NodeId => Term

      solns.each do |soln, index|
        patch = body.call(soln, index)
        assert patch.size <= soln.participants.size, "size of patch exceeds the number of node participants (#{patch.size} > #{soln.participants.size} participants)"

        modifies = Pf::USet32.transaction do |commit|
          patch.each do |node, rep|
            next if original(soln, node) == rep

            commit << node
          end
        end

        # Abort transaction if any node modified by the rule was modified by
        # someone else already.
        next if modifies.intersects?(used)

        # Commit.
        used |= modifies
        modifies.each do |node|
          assert reactions.put?(node, patch[node])
        end
      end

      reactions
    end

    private def solve(ctx : SearchContext, candidates : Array({Plan, Int32}), body : Body) : Patch
      if candidates.empty?
        return Patch.new
      end

      solns = [] of {Soln, Int32}

      candidates.each do |plan, index|
        search(ctx, plan, sink: Sink.new { |soln| solns << {soln, index} })
      end

      solns.sort_by! { |soln, _| soln.rank }

      solve(solns, body)
    end

    # Returns a `Patch` that should be applied to nodes in *hg*. The patch
    # represents a transition to the next timestep, as defined by this rewrite
    # regime and *body*.
    #
    # This method performs one "rewrite tick" of this regime. The caller is
    # responsbile for actually merging the patch back into the circuit, based
    # on node id correspondence etc.
    def solve(hg : Hypergraph, body : Body) : Patch
      decmap = Slice(Pf::USet32).new(hg.order) { Pf::USet32.new }
      idecmap = {} of NodeId => Pf::USet32
      population = Pf::USet32.new

      hg.each_node_with_id do |node, id|
        decmap[id] = decomp = @labeler.decompose(node)
        population |= decomp

        decomp.each do |label|
          idecmap[label] = (idecmap[label]? || Pf::USet32.new).add(id)
        end
      end

      candidates = [] of {Plan, Int32}

      @plans.each_with_index do |plan, index|
        next unless plan.demands.subset_of?(population)

        candidates << {plan, index}
      end

      ctx = SearchContext.new(hg, hg.graph, decmap.readonly, idecmap)

      solve(ctx, candidates, body)
    end
  end
end
