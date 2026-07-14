module Ww::Rack::Parser
  extend self

  # :nodoc:
  #
  # NOTE: *lock* applies to *table* and *grammars*. *seen_* sets are owned
  # by step() in a thread-unsafe way.
  defcase State,
    epoch : Automaton::Epoch,
    lock : Sync::Mutex,
    table : Hash(Parse, ParseStatus),
    grammars : Hash(Term, ParseKit::GrammarF),
    seen_rulesets : Set(Term),
    seen_parses : Set(Parse)

  defrecord Parse, source : Term::Str, ruleset : Term, top : Term::Sym

  alias ParseStatus = Completed | Pending

  defrecord Completed, result : Term | ParseKit::Refusal | ParseKit::Err
  defrecord Pending

  def state(epoch : Automaton::Epoch) : State
    State.new(epoch,
      lock: Sync::Mutex.new,
      table: {} of Parse => ParseStatus,
      grammars: {} of Term => ParseKit::GrammarF,
      seen_parses: Set(Parse).new,
      seen_rulesets: Set(Term).new,
    )
  end

  def step(state : State, parser : D7::Parser, circuit : Term, prepass) : Slice(Term)
    assert state.seen_parses.empty?
    assert state.seen_rulesets.empty?

    subframes = D7.step(parser, circuit) do |hg|
      prepass.call(hg) do |hg|
        D7::Regime.merge(hg, proposals: step(state, hg))
      end
    end

    # See which parses / rulesets were canceled and remove them from
    # the associated tables in *state*.
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

    subframes
  end

  defrecord Transfer,
    input : D7::AbsEdge,
    top : Term::Sym,
    output : D7::AbsEdge,
    ruleset : Term

  private def step(state : State, hg : D7::Hypergraph) : Slice(D7::Patch)
    hg.propose(:parser) do |node|
      Term.case(node.term) do
        matchpiT %{[parser (@input_ -> top_symbol -> @output_) ruleset_*]} do
          variant = Transfer.new(node.resolve(input), top, node.resolve(output), ruleset)
          step(state, hg, node, variant)
        end

        otherwise { }
      end
    end
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

      run(state.lock, state.table, state.grammars, parse, state.epoch)

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
          case result = status.result
          in Term
            D7.patch(target, {2, result})
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

  private def run(lock, table, grammars, parse : Parse, epoch)
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
        duration = now - start.not_nil!
        if duration >= deadline
          raise Canceled.new
        end
      end
    rescue Canceled
      # If inline parsing is too slow we parse on a worker fiber. We only lose
      # the amount of work done during *deadline*.
      spawn(name: "Rack::Parser worker") do
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
        epoch.call
      end
    end
  end

  private def parse(lock, table, grammars, parse : Parse, &checkpoint : UInt64 ->)
    view = parse.source.to(StringView)

    # Grammar construction can take a long time so we offload it to the worker
    # fiber as well.
    grammar = lock.synchronize do
      grammars.put_if_absent(parse.ruleset) do
        ParseKit.flatten(ParseKit.grammar(parse.ruleset))
      end
    end

    ctx = ParseKit.context(grammar, checkpoint)
    π = ParseKit.resolve(ParseKit.parse(ctx, parse.top, view))

    lock.synchronize do
      next unless table.has_key?(parse) # Canceled

      table[parse] = Completed.new(π)
    end
  end

  # Returns `true` if parses are ongoing at the moment.
  def pending?(state : State) : Bool
    state.lock.synchronize do
      # There is no way that anything can be added to the table anymore. No
      # parses are pending and we can quit.
      state.table.present?
    end
  end
end
