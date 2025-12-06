module Ww::Microfold::Pass
  # Performs the parsing pass.
  #
  # During the parsing pass, Microfold:
  #
  # - Identifies utilities (but does not look into the theme just yet!).
  # - Identifies pseudo-utilities (e.g. `present`, `absent`, `is-*`, `has-*`).
  # - Identifies cue conditionals.
  # - Identifies and evaluate conditionals and pseudo-conditionals.
  def parse(root : Term, issues : Issue::Sink) : Term
    mapwalk_preset_and_style(root, issues) do |keypath, pairs, style, issues|
      next style unless style.type.string?

      # WARNING: locus must not be persisted, because keypath will mutate.
      # Its lifetime must not exceed `Parse.style`.
      locus = Locus.new(root, keypath)

      Parse.style(locus, pairs, style.to(StringView), issues)
    end
  end
end
