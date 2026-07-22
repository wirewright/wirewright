module Ww::Rack::Parser
  extend self

  # :nodoc:
  defcase State,
    grammars : SyncHash(Term, ParseKit::GrammarF),
    tasks : D7::TaskBoard(Automaton::Epoch, Task, Result)

  defrecord Task, source : Term::Str, ruleset : Term, top : Term::Sym

  alias Result = Term | ParseKit::Err

  def state(epoch : Automaton::Epoch) : State
    grammars = SyncHash(Term, ParseKit::GrammarF).new

    tasks = D7::TaskBoard(Automaton::Epoch, Task, Result).new(epoch) do |task, ping|
      execute(grammars, task, ping)
    end

    State.new(grammars, tasks)
  end

  private def execute(grammars, task : Task, ping) : Result
    view = task.source.to(StringView)

    grammar = grammars.put_if_absent(task.ruleset) do
      ParseKit.flatten(ParseKit.grammar(task.ruleset))
    end

    ctx = ParseKit.context(grammar, ping)
    ParseKit.resolve(ParseKit.parse(ctx, task.top, view))
  end

  def pending?(state : State) : Bool
    state.tasks.pending?
  end

  defrecord StepContext,
    rulesets : Set(Term),
    tasks : D7::TaskBoard::Rdv(Automaton::Epoch, Task, Result)

  def step(state : State, & : Proposer -> T) : T forall T
    seen_rulesets = Set(Term).new

    result = state.tasks.rdv do |tasks_rdv|
      ctx = StepContext.new(seen_rulesets, tasks_rdv)
      yield Proposer.new(state, ctx)
    end

    # Tasks cannot garbage collect grammars so we have to do it ourselves. Only
    # do it if there's a chance something changed, though (in terms of grammars).
    unless seen_rulesets.size == state.grammars.size
      state.grammars.select! do |ruleset, _|
        ruleset.in?(seen_rulesets)
      end
    end

    result
  end

  struct Proposer
    def initialize(@state : State, @ctx : StepContext)
    end

    def propose(hg : D7::Hypergraph, proposals) : Nil
      Parser.propose(@state, @ctx, hg, proposals)
    end
  end

  defrecord Transfer,
    input : D7::AbsEdge,
    top : Term::Sym,
    output : D7::AbsEdge,
    ruleset : Term

  defrecord TransferError,
    input : D7::AbsEdge,
    top : Term::Sym,
    output : D7::AbsEdge,
    error : D7::AbsEdge,
    ruleset : Term

  defrecord View,
    input : D7::AbsEdge,
    top : Term::Sym,
    output : D7::AbsEdge,
    ruleset : Term

  defrecord ViewError,
    input : D7::AbsEdge,
    top : Term::Sym,
    output : D7::AbsEdge,
    error : D7::AbsEdge,
    ruleset : Term

  # :nodoc:
  def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :parser) do |node|
      variant = nil

      Term.case(node.term) do
        matchpiT %{[parser (@input_ -> top_symbol -> @output_) ruleset_*]} do
          variant = Transfer.new(node.resolve(input), top, node.resolve(output), ruleset)
        end

        matchpiT %{[parser (@input_ -> top_symbol -> @output_ / @error_) ruleset_*]} do
          variant = TransferError.new(node.resolve(input), top, node.resolve(output), node.resolve(error), ruleset)
        end

        matchpiT %{[parser (@input_ - top_symbol - @output_) ruleset_*]} do
          variant = View.new(node.resolve(input), top, node.resolve(output), ruleset)
        end

        matchpiT %{[parser (@input_ - top_symbol - @output_ / @error_) ruleset_*]} do
          variant = ViewError.new(node.resolve(input), top, node.resolve(output), node.resolve(error), ruleset)
        end

        otherwise { }
      end

      next if variant.nil?

      # Mark ruleset as seen so its grammar is kept alive (if present).
      ctx.rulesets << variant.ruleset

      step(state, ctx, hg, node, variant)
    end
  end

  defrecord Source, node : D7::Node, value : Term::Str

  private def source?(hg : D7::Hypergraph, input : D7::AbsEdge) : Source?
    return unless cell = Rack.cell?(hg, input)
    return unless value = cell.value?.as_s?

    Source.new(cell.node, value)
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, node : D7::Node, variant : Transfer) : D7::Patch?
    # For human-comprehensible  behavior, we only support a single source. If
    # there are many candidates we're "confused". We could handle many candidates
    # but the behavior would likely be unintuitive.
    return unless source = source?(hg, variant.input)

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.output}) do |node|
      Term.matchpi?(node.term, %{[cell @_]}) do
        targets << node
      end
    end

    return if targets.empty?

    task = Task.new(source.value, variant.ruleset, variant.top)
    return unless result = checkout?(ctx, task)

    # Clear source and set target(s).
    case result
    in Term
      D7.patches(
        D7.patch(source.node, {2, nil}),
        D7.patches(targets, {2, result}),
      )
    in ParseKit::Err
      # Target cell(s) are already empty, we don't have to clear them.
      D7.patch(source.node, {2, nil})
    end
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, node : D7::Node, variant : TransferError) : D7::Patch?
    return unless source = source?(hg, variant.input)

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.output}) do |node|
      Term.matchpi?(node.term, %{[cell @_]}) do
        targets << node
      end
    end

    return if targets.empty?

    # Find empty error cell(s).
    errors = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.error}) do |node|
      Term.matchpi?(node.term, %{[cell @_]}) do
        errors << node
      end
    end

    return if errors.empty?

    task = Task.new(source.value, variant.ruleset, variant.top)
    return unless result = checkout?(ctx, task)

    # Clear source and set target(s).
    case result
    in Term
      D7.patches(
        D7.patch(source.node, {2, nil}),
        D7.patches(targets, {2, result}),
      )
    in ParseKit::Err
      error = Term.of(:err, result.detail)

      D7.patches(
        D7.patch(source.node, {2, nil}),
        D7.patches(errors, {2, error}),
      )
    end
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, node : D7::Node, variant : View) : D7::Patch?
    return unless source = source?(hg, variant.input)

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.output}) do |node|
      Term.matchpi?(node.term, %{[cell @_ _?]}) do
        targets << node
      end
    end

    return if targets.empty?

    task = Task.new(source.value, variant.ruleset, variant.top)
    return unless result = checkout?(ctx, task)

    # Clear source and set target(s).
    case result
    in Term
      D7.patches(targets, {2, result})
    in ParseKit::Err
      D7.patches(targets, {2, nil})
    end
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, node : D7::Node, variant : ViewError) : D7::Patch?
    return unless source = source?(hg, variant.input)

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.output}) do |node|
      Term.matchpi?(node.term, %{[cell @_ _?]}) do
        targets << node
      end
    end

    return if targets.empty?

    # Find error cell(s).
    errors = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.error}) do |node|
      Term.matchpi?(node.term, %{[cell @_ _?]}) do
        errors << node
      end
    end

    return if errors.empty?

    task = Task.new(source.value, variant.ruleset, variant.top)
    return unless result = checkout?(ctx, task)

    case result
    in Term
      D7.patches(
        D7.patches(targets, {2, result}),
        D7.patches(errors, {2, nil}),
      )
    in ParseKit::Err
      error = Term.of(:err, result.detail)

      D7.patches(
        D7.patches(targets, {2, nil}),
        D7.patches(errors, {2, error}),
      )
    end
  end

  private def checkout?(ctx : StepContext, task : Task) : Result?
    # Already computed.
    if result = ctx.tasks.result?(task)
      return result
    end

    # Try computing inline (returns Result) or schedule (returns Nil).
    ctx.tasks.publish?(task, throttle: 64u64, deadline: 100.microseconds)
  end
end
