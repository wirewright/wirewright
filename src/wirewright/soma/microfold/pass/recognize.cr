module Ww::Soma::Microfold::Pass
  # Performs the recognition pass on *root*.
  #
  # During the recognition pass, Microfold-administered nodes are detected and marked.
  # The tree is prepared for subsequent processing by standardizing style-related metadata,
  # ensuring downstream passes can rely on the presence of `µ-preset` and `µ-style`.
  #
  # Reports any issues that arise during the pass to *issues*.
  def recognize(root : Term, theme : Theme, issues : Issue::Sink) : Term
    mapwalk(root) do |node|
      Term.of_case(node) do
        # When the style string is present, Microfold will define µ-style even if there
        # is no preset for that node.
        matchpi %[(tag_ ⁑ -µ-preset -µ-style style_string)] do
          node.morph(
            {:style, nil},
            {:"µ-preset", theme.preset?(tag, issues) || ""},
            {:"µ-style", style},
          )
        end

        # When the style string is absent, Microfold will not define µ-style unless
        # a preset exists for its tag.
        matchpi %[(tag_ ⁑ -µ-preset)] do
          continue unless preset = theme.preset?(tag, issues)

          node.morph(
            {:"µ-preset", theme.preset?(tag, issues) || ""},
            {:"µ-style", ""},
          )
        end

        # Microfold will ignore any other node.
        otherwise { node }
      end
    end
  end
end
