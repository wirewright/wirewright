module Ww::Rack::Parser
  extend self

  # :nodoc:
  defcase State,
    epoch : Automaton::Epoch,
    lock : Sync::RWLock,
    running : Hash(ParseTask, ParseStatus),
    grammars : Hash(Term, ParseKit::GrammarF)

  defrecord ParseTask, source : Term::Str, ruleset : Term, top : Term::Sym

  alias ParseStatus = Completed | Pending

  defrecord Completed, result : Term | ParseKit::Err
  defrecord Pending

  def state(epoch : Automaton::Epoch) : State
    State.new(epoch,
      lock: Sync::RWLock.new,
      running: {} of ParseTask => ParseStatus,
      grammars: {} of Term => ParseKit::GrammarF,
    )
  end

  defrecord StepContext, tasks : Set(ParseTask), rulesets : Set(Term)

  def step(state : State, parser : D7::Parser, circuit : Term, prepass) : Slice(Term)
    seen_tasks = Set(ParseTask).new
    seen_rulesets = Set(Term).new

    subframes = D7.step(parser, circuit) do |hg|
      prepass.call(hg) do |hg|
        ctx = StepContext.new(seen_tasks, seen_rulesets)
        D7::Regime.merge(hg, proposals: step(state, ctx, hg))
      end
    end

    # See which parses / rulesets were canceled and remove them from
    # the associated tables in *state*.
    state.lock.write do
      state.running.select! do |task, _|
        task.in?(seen_tasks)
      end

      state.grammars.select! do |ruleset, _|
        ruleset.in?(seen_rulesets)
      end
    end

    subframes
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

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph) : Indexable(D7::Patch)
    hg.propose(:parser) do |node|
      Term.case(node.term) do
        matchpiT %{[parser (@input_ -> top_symbol -> @output_) ruleset_*]} do
          variant = Transfer.new(node.resolve(input), top, node.resolve(output), ruleset)
          step(state, ctx, hg, node, variant)
        end

        matchpiT %{[parser (@input_ -> top_symbol -> @output_ / @error_) ruleset_*]} do
          variant = TransferError.new(node.resolve(input), top, node.resolve(output), node.resolve(error), ruleset)
          step(state, ctx, hg, node, variant)
        end

        matchpiT %{[parser (@input_ - top_symbol - @output_) ruleset_*]} do
          variant = View.new(node.resolve(input), top, node.resolve(output), ruleset)
          step(state, ctx, hg, node, variant)
        end

        matchpiT %{[parser (@input_ - top_symbol - @output_ / @error_) ruleset_*]} do
          variant = ViewError.new(node.resolve(input), top, node.resolve(output), node.resolve(error), ruleset)
          step(state, ctx, hg, node, variant)
        end

        otherwise { }
      end
    end
  end

  defrecord Source, node : D7::Node, value : Term::Str

  private def source?(hg : D7::Hypergraph, input : D7::AbsEdge) : Source?
    # Find nonempty input cell(s).
    sources = Pf::Kit.stack_array(Source, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {input}) do |node|
      # Since cell has only one edge, `memberof:` above already covers
      # the edge check.
      Term.matchpiT?(node.term, %{[cell @_ value_string]}) do
        sources << Source.new(node, value)
      end
    end

    # For human-comprehensible  behavior, we only support a single source. If
    # there are many sources we're "confused". We could handle many sources
    # but the behavior would likely be unintuitive.
    sources.single?
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, node : D7::Node, variant : Transfer) : D7::Patch?
    return unless source = source?(hg, variant.input)

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.output}) do |node|
      Term.matchpi?(node.term, %{[cell @_]}) do
        targets << node
      end
    end

    return if targets.empty?

    task = ParseTask.new(source.value, variant.ruleset, variant.top)
    status = checkout(state, task)

    case status
    in Completed
      state.lock.write do
        state.running.delete(task)
      end

      # Clear source and set target(s).
      D7.patches(
        D7.patch(source.node, {2, nil}),
        D7.patches(targets) do |target|
          case result = status.result
          in Term
            D7.patch(target, {2, result})
          in ParseKit::Err
            D7::Patch.new
          end
        end,
      )
    in Pending
      ctx.tasks << task
      ctx.rulesets << variant.ruleset

      nil # No change (yet)
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

    task = ParseTask.new(source.value, variant.ruleset, variant.top)
    status = checkout(state, task)

    case status
    in Completed
      state.lock.write do
        state.running.delete(task)
      end

      # Clear source and set target(s).
      D7.patches(
        D7.patch(source.node, {2, nil}),
        case result = status.result
        in Term
          D7.patches(targets) { |target| D7.patch(target, {2, result}) }
        in ParseKit::Err
          D7.patches(errors) { |error| D7.patch(error, {2, Term.of(:err, result.detail)}) }
        end,
      )
    in Pending
      ctx.rulesets << variant.ruleset
      ctx.tasks << task

      nil # No change (yet)
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

    task = ParseTask.new(source.value, variant.ruleset, variant.top)
    status = checkout(state, task)

    case status
    in Completed
      state.lock.write do
        state.running.delete(task)
      end

      # Clear source and set target(s).
      D7.patches(targets) do |target|
        case result = status.result
        in Term
          D7.patch(target, {2, result})
        in ParseKit::Err
          D7.patch(target, {2, nil})
        end
      end
    in Pending
      ctx.tasks << task
      ctx.rulesets << variant.ruleset

      nil # No change (yet)
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

    task = ParseTask.new(source.value, variant.ruleset, variant.top)
    status = checkout(state, task)

    case status
    in Completed
      state.lock.write do
        state.running.delete(task)
      end

      case result = status.result
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
    in Pending
      ctx.tasks << task
      ctx.rulesets << variant.ruleset

      nil # No change (yet)
    end
  end

  private def checkout(state : State, task : ParseTask) : ParseStatus
    status = state.lock.read { state.running[task]? }
    if status.nil?
      # Task not scheduled. Schedule it.
      state.lock.write do
        state.running[task] = Pending.new
      end

      execute(state.lock, state.running, state.grammars, task, state.epoch)

      # Refresh status.
      #
      # NOTE: execute() doesn't remove statuses. So it should either give us Completed
      # (if parsed inline) or Pending (if scheduled on a worker).
      status = state.lock.read { state.running[task] }
    end

    status
  end

  private class Canceled < Exception
    @callstack = CallStack.empty
  end

  private def execute(lock, running, grammars, task : ParseTask, epoch)
    begin
      start = nil
      deadline = 128.microseconds

      parse(lock, running, grammars, task) do |clock|
        next if clock.zero?

        # For very small parses, we do not even do the initial Time.instant.
        # For longer ones, we do.
        if start.nil? && clock % 64 == 0
          start ||= Time.instant
          next
        end

        next unless clock % 256 == 0

        now = Time.instant
        duration = now - start.not_nil!
        if duration >= deadline
          raise Canceled.new
        end
      end
    rescue Canceled
      # If inline parsing is too slow we parse on a worker fiber. We only lose
      # the amount of work done during *deadline*.
      spawn(name: "Rack::Parser parse task") do
        parse(lock, running, grammars, task) do |clock|
          next if clock.zero?
          next unless clock % 256 == 0
          next unless lock.try_lock_read?

          begin
            next if running.has_key?(task)

            raise Canceled.new
          ensure
            lock.unlock_read
          end
        end
      rescue Canceled
        # Nothing to do. The running table already lacks *task*.
      ensure
        epoch.call
      end
    end
  end

  private def parse(lock, running, grammars, task : ParseTask, &checkpoint : UInt64 ->)
    view = task.source.to(StringView)

    # Grammar construction can take a long time so we offload it to the worker
    # fiber as well.
    grammar = lock.write do
      grammars.put_if_absent(task.ruleset) do
        ParseKit.flatten(ParseKit.grammar(task.ruleset))
      end
    end

    ctx = ParseKit.context(grammar, checkpoint)
    π = ParseKit.resolve(ParseKit.parse(ctx, task.top, view))

    lock.write do
      next unless running.has_key?(task) # Canceled

      running[task] = Completed.new(π)
    end
  end

  # Returns `true` if parses are ongoing at the moment.
  def pending?(state : State) : Bool
    state.lock.read do
      # There is no way that anything can be added to the running table
      # anymore. No parses are pending and we can quit.
      state.running.present?
    end
  end
end
