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
    # It can't quite be generational (mainly due to the latter being highly
    # synchronous in nature); nor is it dependent.
    #
    # FIXME: this will thrash will it not?
    cache = SyncLRU({Rho::RewriterId, Term}, Term::Rep).new(1024)

    variantsQ = GenerationalCache(Term, VariantQ).new
    rewriters = GenerationalCache({Term, Term}, Rho::Rewriter).new
    tasks = D7::TaskBoard(Automaton::Epoch, Task, Result).new(epoch) do |task, ping|
      execute(task, cache, ping)
    end
    State.new(variantsQ, rewriters, tasks)
  end

  private def execute(task : Task, cache : Rho::IRewriteCache, ping) : Result
    Rho.rewrite(task.rewriter, task.input, cache: cache, ping: ping)
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
  alias VariantQ = TransferQ | RelationQ | VersionedRelationQ | VersionedAsymmetricRelationQ |
                   RegimeSingleQ | RegimeMultiQ

  # :nodoc:
  defrecord TransferQ,
    input_edge : Term,
    spec : CompiledSpec | SpecQ,
    output_edge : Term

  # :nodoc:
  defrecord RelationQ,
    input_edge : Term,
    spec : CompiledSpec | SpecQ,
    output_edge : Term

  # :nodoc:
  defrecord VersionedRelationQ,
    input_edge : Term,
    spec : CompiledSpec | SpecQ,
    output_edge : Term,
    version_edge : Term

  # :nodoc:
  defrecord VersionedAsymmetricRelationQ,
    input_edge : Term,
    spec : CompiledSpec | SpecQ,
    output_edge : Term,
    version_edge : Term

  # :nodoc:
  defrecord RegimeSingleQ,
    spec : CompiledSpec | SpecQ,
    target_edge : Term

  # :nodoc:
  defrecord RegimeMultiQ,
    spec : CompiledSpec | SpecQ,
    edge_template : Term::Dict

  # :nodoc:
  defrecord SpecQ, edge : Term, data : Term

  # :nodoc:
  defrecord CompiledSpec, digest : UInt64, rewriter : Rho::Rewriter

  # FIXME: Use a stronger hashcode for digest!
  private def compiled_spec(specQ : Term, data : Term)
    digest = Int.mix(Term.hashcode(specQ), Term.hashcode(data))
    CompiledSpec.new(digest, Rho.rewriter!(specQ, data))
  end

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
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : compiled_spec(specQ, data)
        TransferQ.new(input, spec, output)
      end

      matchpi %{[rewriter (@input_ - specQ_ - @output_) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : compiled_spec(specQ, data)
        RelationQ.new(input, spec, output)
      end

      matchpi %{[rewriter ((@input_ @version_) - specQ_ - @output_) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : compiled_spec(specQ, data)
        VersionedRelationQ.new(input, spec, output, version)
      end

      matchpi %{[rewriter ((@input_ @version_) -> specQ_ - @output_) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : compiled_spec(specQ, data)
        VersionedAsymmetricRelationQ.new(input, spec, output, version)
      end

      matchpi %{[rewriter (specQ_ - @target_) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : compiled_spec(specQ, data)
        RegimeSingleQ.new(spec, target)
      end

      matchpiT %{[rewriter (specQ_ - template_dict) data_*]} do
        spec = Term.edge?(specQ) ? SpecQ.new(specQ, data) : compiled_spec(specQ, data)
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

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : VersionedRelationQ) : VersionedRelation
    input = hg.resolve(node.addr, quote.input_edge)
    output = hg.resolve(node.addr, quote.output_edge)
    version = hg.resolve(node.addr, quote.version_edge)
    spec = unquote(hg, node, quote.spec)
    VersionedRelation.new(input, spec, output, version)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : VersionedAsymmetricRelationQ) : VersionedAsymmetricRelation
    input = hg.resolve(node.addr, quote.input_edge)
    output = hg.resolve(node.addr, quote.output_edge)
    version = hg.resolve(node.addr, quote.version_edge)
    spec = unquote(hg, node, quote.spec)
    VersionedAsymmetricRelation.new(input, spec, output, version)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : RegimeSingleQ) : RegimeSingle
    target = hg.resolve(node.addr, quote.target_edge)
    spec = unquote(hg, node, quote.spec)
    RegimeSingle.new(spec, target)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : RegimeMultiQ) : RegimeMulti
    spec = unquote(hg, node, quote.spec)
    RegimeMulti.new(spec, quote.edge_template)
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : CompiledSpec) : CompiledSpec
    quote
  end

  private def unquote(hg : D7::Hypergraph, node : D7::Node, quote : SpecQ) : Spec
    edge = hg.resolve(node.addr, quote.edge)
    Spec.new(edge, quote.data)
  end

  # :nodoc:
  alias Variant = Transfer | Relation | VersionedRelation | VersionedAsymmetricRelation |
                  RegimeSingle | RegimeMulti

  # :nodoc:
  defrecord Transfer,
    input : D7::AbsEdge,
    spec : CompiledSpec | Spec,
    output : D7::AbsEdge

  # :nodoc:
  defrecord Relation,
    input : D7::AbsEdge,
    spec : CompiledSpec | Spec,
    output : D7::AbsEdge

  # :nodoc:
  defrecord VersionedRelation,
    input : D7::AbsEdge,
    spec : CompiledSpec | Spec,
    output : D7::AbsEdge,
    version : D7::AbsEdge

  # :nodoc:
  defrecord VersionedAsymmetricRelation,
    input : D7::AbsEdge,
    spec : CompiledSpec | Spec,
    output : D7::AbsEdge,
    version : D7::AbsEdge

  # :nodoc:
  defrecord RegimeSingle,
    spec : CompiledSpec | Spec,
    target : D7::AbsEdge

  # :nodoc:
  defrecord RegimeMulti,
    spec : CompiledSpec | Spec,
    edge_template : Term::Dict

  # :nodoc:
  defrecord Spec, edge : D7::AbsEdge, data : Term

  private def rewriter?(rewriters : ICache, hg : D7::Hypergraph, spec : CompiledSpec) : Rho::Rewriter?
    spec.rewriter
  end

  private def rewriter?(rewriters : ICache, hg : D7::Hypergraph, spec : Spec) : Rho::Rewriter?
    return unless rwspec = Rack.cell?(hg, spec.edge).try(&.value?)

    rewriters.put_if_absent({rwspec, spec.data}) do
      Rho.rewriter!(rwspec, spec.data)
    end
  end

  private def rewriter_and_digest?(rewriters : ICache, hg : D7::Hypergraph, spec : CompiledSpec) : {Rho::Rewriter, UInt64}?
    {spec.rewriter, spec.digest}
  end

  private def rewriter_and_digest?(rewriters : ICache, hg : D7::Hypergraph, spec : Spec) : {Rho::Rewriter, UInt64}?
    return unless rwspec = Rack.cell?(hg, spec.edge).try(&.value?)

    rewriter = rewriters.put_if_absent({rwspec, spec.data}) do
      Rho.rewriter!(rwspec, spec.data)
    end
    # FIXME: Use a stronger hashcode for digest!
    digest = Int.mix(Term.hashcode(rwspec), Term.hashcode(spec.data))

    {rewriter, digest}
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
    # Find a single output cell.
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
      next if Term.item_changed?(current_output, output)
      return
    end

    D7.patch(output_cell.node, {2, output})
  end

  # Returns the version digest for *spec* and *input*: the digest to place in
  # the version cell or compare with.
  #
  # *spec* is the rewriter spec itself. We include it in the digest to trigger
  # rewrites on changes to the rules.
  #
  # FIXME: Use a stronger hashcode for digest!
  private def vdigest(rewriter_digest : UInt64, input : Term) : Term
    Term.of(Int.mix(rewriter_digest, Term.hashcode(input)).to_s(base: 16))
  end

  private def step(state, tasks, hg, node : D7::Node, variant : VersionedRelation) : D7::Patch?
    # Find a single version cell.
    return unless version_cell = Rack.cell?(hg, variant.version)

    # Find a single output cell.
    unless output_cell = Rack.cell?(hg, variant.output)
      return D7.patch(version_cell.node, {2, nil}) # clear version (missing output cell)
    end

    # Find a single nonempty input cell.
    input_cell = Rack.cell?(hg, variant.input)
    input = input_cell.try(&.value?)
    if input_cell.nil? || input.nil?
      # Clear output and version (missing input cell or input).
      return D7.patches(
        D7.patch(version_cell.node, {2, nil}),
        D7.patch(output_cell.node, {2, nil}),
      )
    end

    unless row = rewriter_and_digest?(state.rewriters, hg, variant.spec)
      # Clear output and version (missing spec cell or spec).
      return D7.patches(
        D7.patch(version_cell.node, {2, nil}),
        D7.patch(output_cell.node, {2, nil}),
      )
    end

    rewriter, rewriter_digest = row
    digest = vdigest(rewriter_digest, input)

    if version_cell.value? == digest
      return # No change
    end

    unless output = output?(tasks, rewriter, input)
      return # Output not yet available, no change
    end

    D7.patches(
      D7.patch(version_cell.node, {2, digest}),
      D7.patch(output_cell.node, {2, output}),
    )
  end

  private def step(state, tasks, hg, node : D7::Node, variant : VersionedAsymmetricRelation) : D7::Patch?
    # Find a single version cell.
    return unless version_cell = Rack.cell?(hg, variant.version)

    # Find a single output cell.
    unless output_cell = Rack.cell?(hg, variant.output)
      return D7.patch(version_cell.node, {2, nil}) # clear version (missing output cell)
    end

    # Find a single nonempty input cell.
    input_cell = Rack.cell?(hg, variant.input)
    input = input_cell.try(&.value?)
    if input_cell.nil? || input.nil?
      return # No change
    end

    unless row = rewriter_and_digest?(state.rewriters, hg, variant.spec)
      # Clear output and version (missing spec cell or spec).
      return D7.patches(
        D7.patch(version_cell.node, {2, nil}),
        D7.patch(output_cell.node, {2, nil}),
      )
    end

    rewriter, rewriter_digest = row
    digest = vdigest(rewriter_digest, input)

    if version_cell.value? == digest
      # Input is the same as the one we've already seen, simply consume it.
      return D7.patch(input_cell.node, {2, nil})
    end

    unless output = output?(tasks, rewriter, input)
      return # Output not yet available, no change
    end

    D7.patches(
      D7.patch(input_cell.node, {2, nil}),
      D7.patch(version_cell.node, {2, digest}),
      D7.patch(output_cell.node, {2, output}),
    )
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

    members = Pf::Kit.stack_array(EdgeTemplateMember::Any)
    input = Rack.fill_edge_template?(hg, node.addr, variant.edge_template) do |member|
      members << member
    end

    return unless input

    # We can't do anything if the rewriter damaged the itemspart or the input
    # term itself beyond recognition.
    return unless output = output?(tasks, rewriter, Term.of(input)).as_d?
    return unless input.itemsize == output.itemsize

    # "Destructure" the rewriter's output and fill in the corresponding cells.
    D7.patches(members) do |member|
      member.patch(value1: output[member.key]?)
    end
  end
end
