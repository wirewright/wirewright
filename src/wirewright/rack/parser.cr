module Ww::Rack::Parser
  extend self

  # :nodoc:
  defcase State,
    errors : Set(Task),
    grammars : SyncHash(Term, ParseKit::GrammarF),
    tasks : D7::TaskBoard(Automaton::Epoch, Task, Result)

  class State
    setter errors
  end

  defrecord Task, source : Term::Str, ruleset : Term, top : Term::Sym

  alias Result = Term | ParseKit::Err

  def state(epoch : Automaton::Epoch) : State
    errors = Set(Task).new
    grammars = SyncHash(Term, ParseKit::GrammarF).new

    tasks = D7::TaskBoard(Automaton::Epoch, Task, Result).new(epoch) do |task, ping|
      execute(grammars, task, ping)
    end

    State.new(errors, grammars, tasks)
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
    errors : Set(Task),
    tasks : D7::TaskBoard::Rdv(Automaton::Epoch, Task, Result)

  def step(state : State, & : Propose -> T) : T forall T
    seen_rulesets = Set(Term).new
    seen_errors = Set(Task).new

    result = state.tasks.rdv do |tasks_rdv|
      ctx = StepContext.new(seen_rulesets, seen_errors, tasks_rdv)
      propose = Propose.new do |hg, proposals|
        propose(state, ctx, hg, proposals)
      end
      yield propose
    end

    state.errors = seen_errors

    # Tasks cannot garbage collect grammars so we have to do it ourselves. Only
    # do it if there's a chance something changed, though (in terms of grammars).
    unless seen_rulesets.size == state.grammars.size
      state.grammars.select! do |ruleset, _|
        ruleset.in?(seen_rulesets)
      end
    end

    result
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

  private def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :parser) do |node|
      variant = nil

      Term.case(node.term) do
        matchpiT %{[parser (@input_ -> top_symbol -> @output_) ruleset_*]} do
          variant = Transfer.new(hg.resolve(node.addr, input), top, hg.resolve(node.addr, output), ruleset)
        end

        matchpiT %{[parser (@input_ -> top_symbol -> @output_ / @error_) ruleset_*]} do
          variant = TransferError.new(hg.resolve(node.addr, input), top, hg.resolve(node.addr, output), hg.resolve(node.addr, error), ruleset)
        end

        matchpiT %{[parser (@input_ - top_symbol - @output_) ruleset_*]} do
          variant = View.new(hg.resolve(node.addr, input), top, hg.resolve(node.addr, output), ruleset)
        end

        matchpiT %{[parser (@input_ - top_symbol - @output_ / @error_) ruleset_*]} do
          variant = ViewError.new(hg.resolve(node.addr, input), top, hg.resolve(node.addr, output), hg.resolve(node.addr, error), ruleset)
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

    # Instead of rescheduling the task over and over in case of an error,
    # when we're clogged, simply remember the task is an error and wait
    # until the input cell is unclogged.
    if task.in?(state.errors)
      ctx.errors << task
      return
    end

    return unless result = checkout?(ctx, task)

    # Clear source and set target(s).
    case result
    in Term
      D7.patches(
        D7.patch(source.node, {2, nil}),
        D7.patches(targets, {2, result}),
      )
    in ParseKit::Err
      # If there's a parse error and it has nowhere to go we clog the input.
      ctx.errors << task
      nil
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
    source = source?(hg, variant.input)

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.output}) do |node|
      Term.matchpi?(node.term, %{[cell @_ _?]}) do
        targets << node
      end
    end

    return if targets.empty?

    unless source
      # If source cell is absent or empty, clear target cell(s).
      return D7.patches(targets, {2, nil})
    end

    task = Task.new(source.value, variant.ruleset, variant.top)
    unless result = checkout?(ctx, task)
      return # Not yet available. Do nothing.
    end

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
