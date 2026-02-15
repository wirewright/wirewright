# Hosts implementations of passes that Microfold makes over the node tree.
module Ww::Microfold::Pass
  extend self

  # :nodoc:
  def mapwalk(node : Term, & : Term, Array(Term) -> Term) : Term
    itempaths = [] of Array(Term)

    Term.each_keypath_and_itemnode(node) do |keypath, item|
      itempaths << keypath.dup

      Term.case(item) do
        matchpi %{(guard _* ⍊ allow: {¦ -ufold})} do
          false # no descend
        end

        otherwise do
          true # descend
        end
      end
    end

    itempaths.unstable_sort! do |itempath0, itempath1|
      itempath1.compare(itempath0) do |a, b|
        a.as_n <=> b.as_n
      end
    end

    itempaths.each do |itempath|
      item = node.follow(itempath)
      node = Term.assign(node, itempath, to: (yield item, itempath))
    end

    node
  end

  # :nodoc:
  def mapwalk_preset_and_style(root : Term, issues : Issue::Sink, &)
    mapwalk(root) do |node, keypath|
      Term.of_case(node) do
        matchpi %[(tag_ _* ¦ pairs_ µ-preset: preset0_ µ-style: style0_)] do
          location = {
            Issue::Spot::KeypathRef.new(keypath),
            Issue::Spot::TermDetail.new("preset for", tag),
            Issue::Spot::TermDetail.new("style", preset0),
          }

          preset1 = issues.adjoin(*location) do |issues|
            yield keypath, pairs.unsafe_as_d, preset0, issues
          end

          location = {
            Issue::Spot::KeypathRef.new(keypath),
            Issue::Spot::TermDetail.new("style", style0),
          }

          style1 = issues.adjoin(*location) do |issues|
            yield keypath, pairs.unsafe_as_d, style0, issues
          end

          Term.morph(node, {:"µ-preset", preset1}, {:"µ-style", style1})
        end

        otherwise { node }
      end
    end
  end

  # Wraps *root* in the implicit root / parent `µ-root`.
  def wrap(root : Term) : Term
    Term.of(:"µ-root", root)
  end

  # Unwraps the node wrapped in the implicit root / parent `µ-root`.
  #
  # Raises `ArgumentError` if unwrapping fails.
  def unwrap(root : Term) : Term
    Term.case(root) do
      matchpi %{[µ-root node_]} { node }
      otherwise { raise ArgumentError.new }
    end
  end

  # Performs all Microfold passes in proper sequence on *root*.
  #
  # Reports any issues that arise during the passes to *issues*.
  def render(theme : Theme, root : Term, issues : Issue::Sink) : Term
    unless validate?(root, issues)
      return root
    end

    pipe(root,
      wrap,
      recognize(theme, issues),
      specialize(issues),
      parse(issues),
      flow(theme),
      cull,
      decompose(theme, issues),
      lookabove,
      cascade(theme),
      designate(theme),
      lower(issues),
      unfold(theme, issues),
      unwrap,
    )
  end
end

require "./pass/validate"
require "./pass/recognize"
require "./pass/specialize"
require "./pass/parse"
require "./pass/flow"
require "./pass/cull"
require "./pass/cascade"
require "./pass/decompose"
require "./pass/designate"
require "./pass/lower"
require "./pass/unfold"
