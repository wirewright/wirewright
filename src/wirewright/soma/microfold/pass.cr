# Hosts implementations of passes that Microfold makes over the node tree.
module Ww::Soma::Microfold::Pass
  extend self

  # :nodoc:
  def mapwalk(node : Term, & : Term, Stack(Term) -> Term) : Term
    # NOTE: keypaths are certainly not the fastest way to do this; note how
    # we require two deep `follow`s of the dict. Something like recursion would
    # be much faster. However, keypaths are much more flexible than recursion --
    # allowing to look up the tree and inspect our parents during iteration, if
    # necessary; and they are also much easier to understand and control
    # in terms of modification (i.e. where it happens, why, and in what order).
    Term.each_keypath_bottom_up(node) do |keypath|
      node = node.as_d do |dict|
        item0 = dict.follow(keypath)
        item1 = yield item0, keypath
        dict.follow(keypath) { item1 }
      end
    end

    node
  end

  # :nodoc:
  def mapwalk_preset_and_style(root : Term, issues : Issue::Sink, &)
    mapwalk(root) do |node, keypath|
      Term.of_case(node) do
        matchpi %[(tag_ _* ¦ pairs_ µ-preset: preset0_ µ-style: style0_)] do
          location = {
            Issue::Spot::Keypath.new(keypath),
            Issue::Spot::TermDetail.new("preset for", tag),
            Issue::Spot::TermDetail.new("style", preset0),
          }

          preset1 = issues.adjoin(*location) do |issues|
            yield keypath, pairs.unsafe_as_d, preset0, issues
          end

          location = {
            Issue::Spot::Keypath.new(keypath),
            Issue::Spot::TermDetail.new("style", style0),
          }

          style1 = issues.adjoin(*location) do |issues|
            yield keypath, pairs.unsafe_as_d, style0, issues
          end

          node.morph({:"µ-preset", preset1}, {:"µ-style", style1})
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
      designate,
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
