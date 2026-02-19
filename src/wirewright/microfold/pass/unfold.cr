module Ww::Microfold::Pass
  # :nodoc:
  module Unfold
    extend self

    def unfold(root : Term, theme : Theme, issues : Issue::Sink) : Term
      Pass.mapwalk(root) do |node0, keypath|
        Term.case(node0) do
          matchpi %[{¦ µ-designations: designations_dict}] do
            orders = designations.ee.compact_map do |box, designation|
              next unless rank = theme.box_rank?(box)
              next unless designation = designation.as_d?

              {rank, box, designation}
            end

            node1 = node0.as_d(&.without(:"µ-designations"))

            orders.sort_by! { |rank, _, _| rank }
            orders.each do |_, box, designation|
              node1 = unfold0(theme, box, designation, node1)
            end

            Term.case({node0, node1}) do
              # If all attributes on a *preset* node were consumed, we do not carry
              # it into UIR. It is very easy to prevent this rule from firing: just add
              # an attribute that Microfold doesn't understand, e.g. `keep: true`.
              givenpi %{[head_symbol _] (head_symbol child_)} do
                continue unless theme.has_preset?(head)

                child
              end

              otherwise { node1 }
            end
          end

          otherwise { node0 }
        end
      end
    end

    def unfold0(theme, box : Term, designation : Term::Dict, node0 : Term) : Term
      case box
      when Term.of(:"text-box")
        node1 = node0.transaction do |commit|
          children = node0.items.move(1)
          children.each_with_index(offset: 1) do |child0, index|
            if child0.type.string?
              child1 = unfold1(theme, box, designation, child0)
            else
              child1 = child0
            end

            commit.with(index, child1)
          end
        end

        Term.of(node1)
      else
        unfold1(theme, box, designation, node0)
      end
    end

    def unfold1(theme, box : Term, designation : Term::Dict, node : Term)
      request = Term.of(Term.union(Term[box, node], designation))
      responses = theme.box_ruleset.responses(request)
      responses.each do |response|
        pr, rule = response

        case pr
        in Pr::One  then env = pr.env
        in Pr::Many then env = pr.envs[0]
        end

        unless rule.is_a?(Rule::Template)
          unreachable("unexpected rule type in ruleset selection")
        end

        return Alloy.render(env, rule.body)
      end

      node
    end
  end

  # Performs the unfold pass on *root*
  #
  # During the unfold pass, nodes with `µ-designations` are replaced with
  # the corresponding hierarchy of boxes -- boxes are instantiated as needed;
  # and `µ-designations` are consumed.
  #
  # Reports any issues that arise during the pass to *issues*.
  def unfold(root : Term, theme : Theme, issues : Issue::Sink) : Term
    Unfold.unfold(root, theme, issues)
  end
end
