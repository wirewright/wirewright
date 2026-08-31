# Implements the `rewriter` nodes. See `rack.rewriter`.
module Ww::Rack::Rewriter
  extend self

  # :nodoc:
  defcase State,
    variantsQ : GenerationalCache(Term, VariantQ),
    rewriters : GenerationalCache({Term, Term}, Rho::Rewriter),
    tasks : D7::TaskBoard(Automaton::Epoch, Task, Result)

  # :nodoc:
  defrecord Task, rewriter : Rho::Rewriter, input : Term

  # :nodoc:
  alias Result = Term

  def state(epoch : Automaton::Epoch) : State
    variantsQ = GenerationalCache(Term, VariantQ).new
    rewriters = GenerationalCache({Term, Term}, Rho::Rewriter).new
    tasks = D7::TaskBoard(Automaton::Epoch, Task, Result).new(epoch) do |task, ping|
      execute(task, ping)
    end
    State.new(variantsQ, rewriters, tasks)
  end

  private def execute(task : Task, ping) : Result
    Rho.rewrite(task.rewriter, task.input, ping: ping)
  end

  def pending?(state : State) : Bool
    state.tasks.pending?
  end

  def step(state : State, & : Propose -> T) : T forall T
    state.variantsQ.epoch do
      state.rewriters.epoch do
        state.tasks.rdv do |tasks_rdv|
          propose = Propose.new do |hg, proposals|
            propose(state, tasks_rdv, hg, proposals)
          end
          yield propose
        end
      end
    end
  end

  private def propose(state, tasks, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :rewriter) do |node|
      next unless variantQ = variantQ?(state.variantsQ, node.term)

      variant = unquote(hg, node, variantQ)
      step(state, tasks, hg, node, variant)
    end
  end

  # :nodoc:
  #
  # *Q* is for quote, as in quote/unquote. Quoted variants can be cached
  # (and are cached!), because they do not depend on the larger hypergraph
  # context (e.g. NodeAddr).
  alias VariantQ = TransferQ | RelationQ | RegimeSingleQ | RegimeMultiQ

  # :nodoc:
  defrecord TransferQ,
    input_edge : Term,
    spec : Rho::Rewriter | SpecQ,
    output_edge : Term

  # :nodoc:
  defrecord RelationQ,
    input_edge : Term,
    spec : Rho::Rewriter | SpecQ,
    output_edge : Term

  # :nodoc:
  defrecord RegimeSingleQ,
    spec : Rho::Rewriter | SpecQ,
    target_edge : Term

  # :nodoc:
  defrecord RegimeMultiQ,
    spec : Rho::Rewriter | SpecQ,
    template : Term::Dict

  # :nodoc:
  defrecord SpecQ, edge : Term, data : Term

  private def variantQ?(variantsQ : ICache, term : Term) : VariantQ?
    if variant = variantsQ.get?(term)
      return variant
    end

    return unless variant = variantQ?(term)

    variantsQ.put(term, variant)
  end

  private def variantQ?(term : Term) : VariantQ?
    # NOTE: Specs being edges (the SpecQ branch) is relatively uncommon in
    # practice. Most commonly the spec is written inline, e.g.
    #   (rewriter (@x -> ⏏noR⏏ -> @y) ...)
    Term.case(term) do
      matchpi %{[rewriter (@input_ -> specQ_ -> @output_) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : Rho.rewriter!(specQ, data)
        TransferQ.new(input, spec, output)
      end

      matchpi %{[rewriter (@input_ - specQ_ - @output_) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : Rho.rewriter!(specQ, data)
        RelationQ.new(input, spec, output)
      end

      matchpi %{[rewriter (specQ_ - @target_) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : Rho.rewriter!(specQ, data)
        RegimeSingleQ.new(spec, target)
      end

      matchpiT %{[rewriter (specQ_ - template_dict) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : Rho.rewriter!(specQ, data)
        RegimeMultiQ.new(spec, template)
      end

      otherwise { }
    end
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : TransferQ) : Transfer
    input = hg.resolve(node.addr, quote.input_edge)
    output = hg.resolve(node.addr, quote.output_edge)
    spec = unquote(hg, node, quote.spec)
    Transfer.new(input, spec, output)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : RelationQ) : Relation
    input = hg.resolve(node.addr, quote.input_edge)
    output = hg.resolve(node.addr, quote.output_edge)
    spec = unquote(hg, node, quote.spec)
    Relation.new(input, spec, output)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : RegimeSingleQ) : RegimeSingle
    target = hg.resolve(node.addr, quote.target_edge)
    spec = unquote(hg, node, quote.spec)
    RegimeSingle.new(spec, target)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : RegimeMultiQ) : RegimeMulti
    spec = unquote(hg, node, quote.spec)
    RegimeMulti.new(spec, quote.template)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : Rho::Rewriter) : Rho::Rewriter
    quote
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : SpecQ) : Spec
    edge = hg.resolve(node.addr, quote.edge)
    Spec.new(edge, quote.data)
  end

  # :nodoc:
  alias Variant = Transfer | Relation | RegimeSingle | RegimeMulti

  # :nodoc:
  defrecord Transfer,
    input : D7::AbsEdge,
    spec : Rho::Rewriter | Spec,
    output : D7::AbsEdge

  # :nodoc:
  defrecord Relation,
    input : D7::AbsEdge,
    spec : Rho::Rewriter | Spec,
    output : D7::AbsEdge

  # :nodoc:
  defrecord RegimeSingle,
    spec : Rho::Rewriter | Spec,
    target : D7::AbsEdge

  # :nodoc:
  defrecord RegimeMulti,
    spec : Rho::Rewriter | Spec,
    template : Term::Dict

  # :nodoc:
  defrecord Spec, edge : D7::AbsEdge, data : Term

  private def rewriter?(rewriters : ICache, hg : D7::Hypergraph, spec : Rho::Rewriter) : Rho::Rewriter?
    spec
  end

  private def rewriter?(rewriters : ICache, hg : D7::Hypergraph, spec : Spec) : Rho::Rewriter?
    return unless rwspec = Rack.cell?(hg, spec.edge).try(&.value?)

    rewriters.put_if_absent({rwspec, spec.data}) do
      Rho.rewriter!(rwspec, spec.data)
    end
  end

  private def output?(tasks, rewriter : Rho::Rewriter, input : Term)
    task = Task.new(rewriter, input)
    output = tasks.result?(task)
    # Try computing inline (returns Result) or schedule (returns Nil).
    output ||= tasks.publish?(task, throttle: 64u64, deadline: 100.microseconds)
    output
  end

  private def step(state, tasks, hg, node : D7::Node, variant : Transfer) : D7::Patch?
    # Find a single nonempty input cell.
    return unless input_cell = Rack.cell?(hg, variant.input)
    return unless input = input_cell.value?

    # Find a single empty output cell.
    return unless output_cell = Rack.cell?(hg, variant.output)
    return unless output_cell.empty?

    return unless rewriter = rewriter?(state.rewriters, hg, variant.spec)
    return unless output = output?(tasks, rewriter, input)

    D7.patches(
      D7.patch(input_cell.node, {2, nil}),
      D7.patch(output_cell.node, {2, output}),
    )
  end

  private def step(state, tasks, hg, node : D7::Node, variant : Relation) : D7::Patch?
    # Find a single empty output cell.
    return unless output_cell = Rack.cell?(hg, variant.output)

    # Find a single nonempty input cell.
    unless input_cell = Rack.cell?(hg, variant.input)
      return D7.patch(output_cell.node, {2, nil}) # clear output (missing input cell)
    end

    unless input = input_cell.value?
      return D7.patch(output_cell.node, {2, nil}) # clear output (missing input)
    end

    unless rewriter = rewriter?(state.rewriters, hg, variant.spec)
      return D7.patch(output_cell.node, {2, nil}) # clear output (missing spec cell or spec)
    end

    return unless output = output?(tasks, rewriter, input)

    # If the current value at output is an extension of the output we've
    # just computed, e.g.,
    #
    #   current value at output:
    #     (+ 100 200 x: true y: true)
    #
    #   computed output:
    #     (+ 100 200)
    #
    # Then we leave the current value as-is. We did not update, and the current
    # value is more informative, so we leave it intact.
    #
    # This is basically like the `input == output` check below but more relaxed.
    pass do
      next unless current_output = output_cell.value?
      next unless Term.extension?(current_output, of: output)
      return
    end

    D7.patch(output_cell.node, {2, output})
  end

  private def step(state, tasks, hg, node : D7::Node, variant : RegimeSingle) : D7::Patch?
    return unless rewriter = rewriter?(state.rewriters, hg, variant.spec)
    return unless target_cell = Rack.cell?(hg, variant.target)
    return unless input = target_cell.value?
    return unless output = output?(tasks, rewriter, input)
    return if input == output # No change

    D7.patch(target_cell.node, {2, output})
  end

  private def step(state, tasks, hg, node : D7::Node, variant : RegimeMulti) : D7::Patch?
    return unless rewriter = rewriter?(state.rewriters, hg, variant.spec)

    targets = Pf::Kit.stack_array(FillTemplateTarget)

    input = Rack.fill?(hg, node.addr, variant.template) do |target|
      targets << target
    end

    return unless input

    # We can't do anything if the rewriter damaged the itemspart or the input
    # term itself beyond recognition.
    return unless output = output?(tasks, rewriter, Term.of(input)).as_d?
    return unless input.itemsize == output.itemsize

    # "Destructure" the rewriter's output and fill in the corresponding cells.
    D7.patches(targets) do |target|
      value1 = output[target.key]?
      next if target.value? == value1

      D7.patch(target.node, {2, value1})
    end
  end
end
