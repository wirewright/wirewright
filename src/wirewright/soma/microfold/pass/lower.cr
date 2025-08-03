module Ww::Soma::Microfold::Pass
  # Performs the lowering pass on *root*.
  #
  # During the lowering pass, Microfold removes Microfold-specific meaning
  # from certain nodes (e.g. a properly set up `icon` node).
  #
  # Reports any issues that arise during the pass to *issues*.
  def lower(root : Term, issues : Issue::Sink) : Term
    mapwalk(root) do |node, keypath|
      issues.adjoin(Issue::Spot::Keypath.new(keypath)) do |issues|
        Term.case(node) do
          # |@ soma.microfold.node.icon
          #
          # |@block
          # Use the `icon` node to create a `text` node whose caption is an icon
          # from an icon font. Most notably, `icon` lets you refer to the icon using
          # its codepoint name, sourced from `*.codepoints` file associated with
          # the icon font, if found.
          # |@endblock
          #
          # |@key name -- Name of the icon's codepoint.
          matchpi(%[
          (icon name_string
            ⍊ µ-designations:
                {¦ text-box:
                  {¦ font_string
                      weight_: (%number +i32)
                      icon-font-fallback: font-fallback_string
                      icon-fallback: fallback_string}})
          ]) do
            node = node.as_d do |dict|
              dict.morph(
                {:"µ-designations", :"text-box", :"icon-font-fallback", nil},
                {:"µ-designations", :"text-box", :"icon-fallback", nil},
              )
            end

            font_weight = DwUIR::FontWeight.parse(weight.to(Int32))

            if char = DwUIR::FontIndex.codepoint?(name.to(String), font.to(String), font_weight)
              node = node.as_d(&.morph({1, char}))
              next
            end

            issues.minor("icon `#{name.to(String)}` not found in the font `#{font.to(String)}` with weight `#{font_weight}`")

            node = node.as_d do |dict|
              dict.morph(
                {1, fallback},
                {:"µ-designations", :"text-box", :font, font_fallback},
              )
            end
          end

          otherwise { }
        end

        node
      end
    end
  end
end
