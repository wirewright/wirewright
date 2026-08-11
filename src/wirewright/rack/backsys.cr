# Implements the `rack.backsys` node.
module Ww::Rack::Backsys
  extend self

  # :nodoc:
  defcase State, variantsQ : GenerationalCache(Term, VariantQ)

  def state : State
    variantsQ = GenerationalCache(Term, VariantQ).new
    State.new(variantsQ)
  end

  def pending?(state : State) : Bool
    false
  end

  def step(state : State, & : Propose -> T) : T forall T
    state.variantsQ.epoch do
      propose = ->(hg : D7::Hypergraph, proposals : Array(D7::Patch)) do
        propose(state.variantsQ, hg, proposals)
      end

      yield propose
    end
  end

  private def propose(variantsQ : ICache, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :backsys) do |node|
      next unless variantQ = variantQ?(variantsQ, node.term)

      step(hg, proposals, node, variantQ)
    end
  end

  alias VariantQ = SingleBacksysQ | TemplateBacksysQ

  defrecord SingleBacksysQ,
    target_edge : Term,
    backsys : Slice({M1::Op::Any, Term::Dict})

  defrecord TemplateBacksysQ,
    template : Term::Dict,
    backsys : Slice({M1::Op::Any, Term::Dict})

  private def variantQ?(variantsQ : ICache, node : Term) : VariantQ?
    if variantQ = variantsQ.get?(node)
      return variantQ
    end

    return unless variantQ = variantQ?(node)

    variantsQ.put(node, variantQ)
  end

  private def variantQ?(node : Term) : VariantQ?
    Term.case(node) do
      matchpi %{[backsys @edge_ backmaps_*]} do
        backsys = backmaps.items.to_compact_readonly_slice do |item|
          Term.matchpiT?(item, %{[backmap pattern_ backspec_dict]}) do
            {M1.operator(pattern), backspec}
          end
        end

        SingleBacksysQ.new(edge, backsys)
      end

      matchpiT %{[backsys template_dict backmaps_*]} do
        backsys = backmaps.items.to_compact_readonly_slice do |item|
          Term.matchpiT?(item, %{[backmap pattern_ backspec_dict]}) do
            {M1.operator(pattern), backspec}
          end
        end

        TemplateBacksysQ.new(template, backsys)
      end

      otherwise { }
    end
  end

  private def step(hg, proposals, node : D7::Node, variant : SingleBacksysQ) : D7::Patch?
    abs_target = hg.resolve(node.addr, variant.target_edge)
    return unless target_cell = Rack.cell?(hg, abs_target)
    return unless input = target_cell.value?
    return unless output = M1.backmap?(variant.backsys, input)
    return if input == output

    D7.patch(target_cell.node, {2, output})
  end

  private def step(hg, proposals, node : D7::Node, variant : TemplateBacksysQ) : D7::Patch?
    targets = Pf::Kit.stack_array(FillTemplateTarget)

    input = Rack.fill?(hg, node.addr, variant.template) do |target|
      targets << target
    end

    return unless input

    # We can't do anything if the backsys damaged the itemspart or the input
    # term itself beyond recognition.
    return unless output = M1.backmap?(variant.backsys, Term.of(input)).as_d?
    return unless input.itemsize == output.itemsize

    # "Destructure" the output and fill in the corresponding cells.
    D7.patches(targets) do |target|
      value1 = output[target.key]?
      next if target.value? == value1

      D7.patch(target.node, {2, value1})
    end
  end
end
