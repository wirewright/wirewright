# Microfold (µfold) is the engine that handles styles and the box model
# in Wirewright / Soma.
#
# Microfold is heavily inspired by Tailwind: its design system, inplace-
# ness, and naming are similar if not outright copied from Tailwind, with
# due reference where possible.
#
# Microfold is an equal participant in working with(in) Wirewright UI terms;
# along with `UIR` and `DwUIR`. In other words, Microfold, UIR, and DwUIR
# share the same representational space; unknowingly to each other, they
# cooperate to create a UI -- stigmergically, in a way, the UI term being
# their "world". Each contributes what it can: Microfold its styles, theming,
# its box model, and the flow of cues; UIR contributes layout; and DwUIR
# contributes display.
#
# Just like UIR and DwUIR, Microfold ignores everything it cannot understand.
# It's an opt-in. This is the basis of shared representational space construction.
# Most importantly, in practice, this means that you can arbitrarily mix and
# nest DwUIR, UIR, and Microfold.
#
# Microfold is centered around the `style: "..."` pair. It is the primary opt-in
# trigger for Microfold.
#
# At the conceptual core of Microfold are *utilities*, *conditions*, *cues*,
# and *boxes*.
#
# - *Utilities* target boxes with style mixins. `text-neutral-500`, as defined
#   in the default theme, will target `text-box` with an additive style mixin
#   `{color: (oklch ...)}`. Some utilities may be subtractive; for instance,
#   `opaque` will *remove* opacity from an element.
#
# - *Conditions* allow the `style: "..."` to look at its "neighboring" pairs
#   and enable utilities on that basis. `style: "hover:bg-blue-500" hover: true`
#   will enable the utility `bg-blue-500`; setting `hover: false` will disable it.
#   Conditions can be combined in an *and* relation: `active:hover:bg-blue-500` will
#   only enable the utility `bg-blue-500` if *both* `active` and `hover` exist and
#   are truthy.
#
# - *Cues* let UI-related state flow bidirectionally. Each style may look at its
#   "neighboring" pairs and emit cues that flow up (to ancestors; `is-*`, e.g. `is-active`,
#   as in "hey parent, I am active!"); or down (to children, `cue-*`, e.g. `cue-form-invalid`,
#   as in "hey children, you're in an invalid form!"). Children may react to cues from
#   their ancestors and/or children and enable/disable utilities (`in-*:*` for top-down
#   cues, as in `in-form-invalid:text-red-500`; `has-*:*` for bottom-up cues, as in
#   `has-active:border-blue-500`); or emit further cues, triggering small- or large-
#   scale chain reactions (e.g. parent says `has-active:cue-active` and children
#   react to `in-active:*` and may sometimes send `is-active` themselves). Cue
#   membranes `membrane` can be used to seal off parts of the tree and let cues
#   flow through selectively.
#
# - *Boxes* are a crude analog to HTML/CSS's box model. See `boxes.hierarchy` in
#   the default theme to learn more.
#
# The following is a Microfold-packed example of a UI term.
#
# ```wwml
# (group style: "membrane flow-col gap-3 has-invalid:cue-invalid"
#   (foo style: "p-1 bg-neutral-200 in-invalid:bg-red-200"
#     (some-input "John Doe" style: "valid-false:is-invalid" valid: true)
#     (p "Username is required" style: "text-red-300 absent in-invalid:present"))
#   (foo style: "p-1 bg-neutral-200 in-invalid:bg-red-200"
#     ;; This one fires is-invalid, which triggers a chain reaction to show both
#     ;; messages as invalid.
#     (some-input "jd@mail" style: "valid-false:is-invalid" valid: false)
#     (p "Email is required" style: "text-red-300 absent in-invalid:present")))
# ```
module Ww::Soma::Microfold
  extend self

  # Constructs a Microfold theme from the given *document* and root em
  # size (root font size) *rem*.
  def theme(document : Term, rem : Term::Num) : Theme
    theme(document.as_d? || Term[], rem)
  end

  # :ditto:
  def theme(document : Term::Dict, rem : Term::Num) : Theme
    Theme.new(document, rem)
  end

  # Runs Microfold on *root*.
  #
  # This function is the main public interface to the entirety of Microfold.
  #
  # - *theme* is the theme to use.
  # - *severity* specifies severity cutoff.
  #
  # Returns the resulting root term, and an array of issue backtraces if any
  # issues were found in *root*.
  #
  # Microfold does not fail (unless there is an implementation bug, of course);
  # instead, it points out issues, some of them minor, others major; others --
  # severe. At no point would Microfold give up, however; it is but one participant
  # shaping the given *root*, so it cannot just explode.
  def render(theme : Theme, root : Term, *, severity : Issue::Severity = :minor) : {Term, Array(Issue::Backtrace)}
    Issue.setup(severity: severity) do |issues|
      Pass.render(theme, root, issues)
    end
  end

  # Alias of the main overload of `render`. The arguments are flipped to enable
  # piping: e.g. `pipe(..., Microfold.render(theme), ...)`. Errors are suppressed.
  def render(root : Term, theme : Theme, **kwargs) : Term
    renderout, _ = render(theme, root, severity: :quiet)
    renderout
  end
end

require "./microfold/theme"
require "./microfold/locus"
require "./microfold/parse"
require "./microfold/pass"
