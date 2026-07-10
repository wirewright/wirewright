module Ww::Rack::Parser
  extend self

  # :nodoc:
  #
  # NOTE: *lock* applies to *table* and *grammars*. *seen_* sets are owned
  # by step() in a thread-unsafe way.
  defcase State,
    alarm : BlockingSignal,
    lock : Sync::Mutex,
    table : Hash(Parse, ParseStatus),
    grammars : Hash(Term, ParseKit::Grammar),
    seen_rulesets : Set(Term),
    seen_parses : Set(Parse)

  defrecord Parse, source : Term::Str, ruleset : Term, top : Term::Sym

  alias ParseStatus = Completed | Pending

  defrecord Completed, result : ParseKit::Parseout
  defrecord Pending

  def state(alarm : BlockingSignal) : State
    State.new(alarm,
      lock: Sync::Mutex.new,
      table: {} of Parse => ParseStatus,
      grammars: {} of Term => ParseKit::Grammar,
      seen_parses: Set(Parse).new,
      seen_rulesets: Set(Term).new,
    )
  end

  def step(state : State, parser : D7::Parser, circuit : Term, prepass) : Slice(Term)
    D7.step(parser, circuit) do |hg|
      prepass.call(hg) { |hg| step(state, hg) }
    end
  end

  defrecord Transfer,
    input : D7::AbsEdge,
    top : Term::Sym,
    output : D7::AbsEdge,
    ruleset : Term

  private def step(state : State, hg : D7::Hypergraph) : D7::Patch
    assert state.seen_parses.empty?
    assert state.seen_rulesets.empty?

    proposals = [] of D7::Patch

    hg.each_node_with_head(Term.of(:parser)) do |node|
      variant = nil

      Term.case(node.term) do
        matchpiT %{[parser (@input_ -> top_symbol -> @output_) ruleset_*]} do
          variant = Transfer.new(node.resolve(input), top, node.resolve(output), ruleset)
        end

        otherwise { }
      end

      next if variant.nil?

      proposal = step(state, hg, node, variant)
      next if proposal.nil?

      proposals << proposal
    end

    # See which parses / rulesets were canceled and remove them from
    # the associated tables.
    state.lock.synchronize do
      state.table.select! do |parse, _|
        parse.in?(state.seen_parses)
      end

      state.grammars.select! do |ruleset, _|
        ruleset.in?(state.seen_rulesets)
      end
    end

    state.seen_parses.clear
    state.seen_rulesets.clear

    D7::Regime.merge(hg, proposals)
  end

  private def step(state : State, hg : D7::Hypergraph, node : D7::Node, variant : Transfer) : D7::Patch?
    # Find nonempty input cell(s).
    sources = Pf::Kit.stack_array({node: D7::Node, value: Term::Str}, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.input}) do |node|
      # Since cell has only one edge, `memberof:` above already covers
      # the edge check.
      Term.matchpiT?(node.term, %{[cell @_ value_string]}) do
        sources << {node: node, value: value}
      end
    end

    # For human-predictable behavior, we only support a single value. If
    # there are many values we'd be "confused".
    return unless source = sources.single?

    # Find empty target cell(s).
    targets = Pf::Kit.stack_array(D7::Node, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.output}) do |node|
      Term.matchpi?(node.term, %{[cell @_]}) do
        targets << node
      end
    end

    return if targets.empty?

    parse = Parse.new(source[:value], variant.ruleset, variant.top)

    status = state.lock.synchronize { state.table[parse]? }
    if status.nil?
      # Parse not scheduled. Schedule it.
      state.lock.synchronize do
        state.table[parse] = Pending.new
      end

      run(state.lock, state.table, state.grammars, parse, state.alarm)

      # Refresh status.
      status = state.lock.synchronize { state.table[parse]? }
    end

    case status
    in Nil
      # run() should either give us Completed (if parsed inline) or Pending
      # (if scheduled on a worker). Receiving Nil here is unexpected.
    in Completed
      state.lock.synchronize do
        state.table.delete(parse)
      end

      # Clear source and set target(s).
      D7.patches(
        D7.patch(source[:node], {2, nil}),
        D7.patches(targets) do |target|
          case π = status.result
          in ParseKit::Ok
            D7.patch(target, {2, π.result})
          in ParseKit::Refusal, ParseKit::Err
            D7::Patch.new
          end
        end,
      )
    in Pending
      state.seen_rulesets << variant.ruleset
      state.seen_parses << parse

      nil # No change (yet)
    end
  end

  class Canceled < Exception
    @callstack = CallStack.empty
  end

  private def run(lock, table, grammars, parse : Parse, alarm)
    begin
      start = nil
      deadline = 128.microseconds

      parse(lock, table, grammars, parse) do |clock|
        next if clock.zero?

        # For very small parses, we do not even do the initial Time.instant.
        # For longer ones, we do.
        if start.nil? && clock % 64 == 0
          start ||= Time.instant
          next
        end

        next unless clock % 256 == 0

        now = Time.instant
        duration = start.not_nil! - now
        if duration >= deadline
          raise Canceled.new
        end
      end
    rescue Canceled
      # If inline parsing is too slow we parse on a worker fiber.
      spawn do
        parse(lock, table, grammars, parse) do |clock|
          next if clock.zero?
          next unless clock % 256 == 0

          lock.synchronize do
            next if table.has_key?(parse)

            # Canceled
            raise Canceled.new
          end
        end
      rescue Canceled
        # Nothing to do. The table already lacks *parse*.
      ensure
        alarm.call
      end
    end
  end

  private def parse(lock, table, grammars, parse : Parse, &checkpoint : UInt64 ->)
    view = parse.source.to(StringView)

    # Grammar construction can take a long time so we offload it to the worker
    # fiber as well.
    grammar = lock.synchronize do
      grammars.put_if_absent(parse.ruleset) do
        ParseKit.grammar(parse.ruleset)
      end
    end

    ctx = ParseKit.context(grammar, checkpoint)

    # Use ParseKit.skim() to populate the oracle.
    case π = ParseKit.skim(ctx, parse.top, view)
    in Pf::StringSeln # ok
      π = ParseKit.parse(ctx, parse.top, view)
    in ParseKit::Refusal, ParseKit::Err
      # If skim reports an error or refuses we don't have to parse() -- skim()
      # err and refusal is compatible with parse()'s.
    end

    lock.synchronize do
      next unless table.has_key?(parse) # Canceled

      table[parse] = Completed.new(π)
    end
  end

  # Returns `true` if parses are ongoing at the moment. The caller is expected
  # to wait for them (see `wait`). As an alternative to `wait`, the caller can
  # pass their own `BlockingSignal` to `state` (this is what e.g. MuSoma does).
  def pending?(state : State) : Bool
    state.lock.synchronize do
      # There is no way that anything can be added to the table anymore. No
      # parses are pending and we can quit.
      state.table.present?
    end
  end
end
