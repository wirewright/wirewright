module Ww::Soma::Microfold::Pass
  # Performs the validation pass on *root*. Returns `true` if *root* is eligible
  # for futher passes; and `false` if it must be rejected.
  #
  # Reports any issues that arise during the pass to *issues*.
  def validate?(root : Term, issues : Issue::Sink) : Bool
    Term.each_leaf_thorough(root) do |leaf|
      next unless symbol = leaf.as_sym?
      next unless symbol.microfold?

      issues.adjoin(Issue::Spot::TermDetail.new("symbol", leaf)) do |issues|
        issues.fatal("symbols prefixed with `µ-` are reserved for Microfold and must not appear in the input tree")
      end

      return false
    end

    true
  end
end
