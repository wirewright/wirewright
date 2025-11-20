module Ww::D7
  # :nodoc:
  alias NodeId = UInt32

  defrecord Reaction, node : Term, emission : Term::Dict

  # Constructs a reaction with *node* that has an empty emission.
  def rxn(node, queue = Term[]) : Reaction
    Reaction.new(Term.of(node), Term[queue])
  end

  # Represents a *rewrite regime*. A rewrite regime encapsulates the rules of
  # rewriting and recognition as well as the indices required to do
  # that efficiently.
  class Regime
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
    # and merge solutions. *min* or more solutions are required,
    # otherwise backtrack.
    defrecord FollowMany, capture : Term, min : Int32

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
    def initialize(@labeler : M1::ShapeIndex, @plans : Slice(Plan), @bodies : Slice(Body))
      assert @plans.size == @bodies.size
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

        matchpi %{(many key_ pattern_ ⍊ min_: (%optional 1 (%number +i32!)))} do
          unless origin
            raise QueryError.new("`many` without a predecessor makes no sense (many where?)")
          end

          plan << {:"follow+", origin, min}
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
            matchpiT %{(follow+ capture_ min←(%number +i32!))} { FollowMany.new(capture, min) }
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
    # must produce a patch. The patch it produces can be empty (signifying no change).
    # The size of the patch must not exceed the number of nodes participating in
    # the solution. The patch is only allowed to modify participating nodes.
    alias Body = Soln -> Patch

    # Represents a replacement of some node.
    alias Patch = Slice({NodeId, Reaction})

    # - *addr* is the address of the node.
    # - *node* is the node term from the circuit.
    # - *env* is the match env of the part of the query associated with
    #   the capture (i.e. `one` or `many`).
    defrecord NodeCapture, addr : NodeAddr, node : Term, env : Term::Dict, chat : NodeChat

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

      # :nodoc:
      #
      # 1. Prefer solutions with most participants.
      # 2. Prefer solutions whose queries are more specific.
      def rank
        {-groups.sum { |_, vs| vs.size }, specificity}
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
      idecmap : Hash(UInt32, Pf::USet32),
      chats : Slice(NodeChat)

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
    record Ahead,
      steps : Slice(Step),
      ret : Ret,
      sink : Sink

    private def forward(ahead : Ahead) : {Step, Ahead}
      {ahead.steps[0], ahead.copy_with(steps: ahead.steps[1..])}
    end

    private def search(ctx, locus, step : Append, soln, ahead)
      return unless step.label.in?(ctx.decmap[locus.node])
      return if locus.node.in?(soln)
      return unless env = M1.match?(step.pattern, term = ctx.hg[locus.node])

      chat = ctx.chats[locus.node]
      capture = NodeCapture.new(locus.addr, term, env, chat)

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

      return if solns.size < step.min

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

    private def reactions(solns : Array({Soln, Body})) : Hash(NodeId, Reaction)
      used = Pf::USet32.new
      reactions = {} of NodeId => Reaction

      solns.each do |soln, body|
        patch = body.call(soln)
        assert patch.size <= soln.participants.size, "patch-solution arity mismatch (#{patch.size} > #{soln.participants.size})"

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
        patch.each do |(node, reaction)|
          assert reactions.put?(node, reaction)
        end
      end

      reactions
    end

    private def reactions(ctx : SearchContext, candidates : Array({Plan, Body})) : Hash(NodeId, Reaction)
      if candidates.empty?
        return {} of NodeId => Reaction
      end

      solns = [] of {Soln, Body}

      candidates.each do |plan, body|
        sink = Sink.new do |soln|
          solns << {soln, body}
        end

        search(ctx, plan, sink)
      end

      solns.sort_by! { |soln, _| soln.rank }

      reactions(solns)
    end

    # Returns the reactions of nodes in *hg* in its current state, according to
    # this rewrite regime, to transition it into the next time-step.
    #
    # This method performs one rewrite tick of this regime. The caller is
    # responsbile for actually merging changes back into the circuit, based
    # on node id correspondence etc.
    def reactions(hg : Hypergraph, chats : Slice(NodeChat)) : Hash(NodeId, Reaction)
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

      candidates = [] of {Plan, Body}

      @plans.zip(@bodies) do |plan, body|
        next unless plan.demands.subset_of?(population)

        candidates << {plan, body}
      end

      ctx = SearchContext.new(hg, hg.graph, decmap.readonly, idecmap, chats)

      reactions(ctx, candidates)
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
            {{participant}} = %soln.groups[Term.of({{participant.symbolize}})]
          {% end %}\

          %result = pass do
            {{branch[:body]}}
          end

          %result || {{@type}}::Regime::Patch.empty
        end,
      {% end %}\
    ]

    {{@type}}::Regime.build(
      queries: %queries.to_readonly_slice(&.itself),
      bodies: %bodies.to_readonly_slice(&.itself),
    )
  end
end
