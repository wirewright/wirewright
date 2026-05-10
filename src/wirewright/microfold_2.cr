# Microfold (µfold) implements Tailwind-like styling for Wirewright.
#
# Microfold is heavily inspired by Tailwind: its design system, the focus on
# locality, and naming conventions are similar if not outright copied from
# Tailwind -- with due reference where possible.
#
# You are interested in two functions: `codex` and `render`. Everything else
# is (more or less) an implementation detail.
#
# ```
# ctx = Fiber::ExecutionContext::Isolated.new("App", spawn_context: MT) do
#   # Load codex.
#   codexdoc = ML.document(ResourceService.read_string(ResourceService.codex("ufold")))
#   codex = Microfold2.codex(codexdoc).unwrap
#
#   input = ML.term(<<-'WWML')
#   (window style: "backdrop-neutral-900 center"
#     (p "Kaixo, mundua!" style: "text-xl text-neutral-50"))
#   WWML
#
#   # Render using codex (you are recommended to create a codex per frame source
#   # since it contains caches).
#   #
#   # This is thread-safe.
#   spec = Microfold2.render(codex, input).unwrap
#
#   MediaService.publish(Term.of(:app), MediaService.window_spec(spec)).wait
#   MediaService.wait_until_all_closed(Set{Term.of(:app)})
#   MediaService.withdraw(Term.of(:app), MediaService::WindowSpec).wait
# end
#
# ctx.wait
# ```
#
# Microfold divides nodes into Microfold-administered nodes and other nodes.
# Nodes that have a `style: "..."` are Microfold-administered; as are nodes
# whose head (e.g., `p` in `(p "Kaixo")`) has an associated preset defined in
# the codex. Microfold passes through all other nodes without changing them.
# As a consequence, you can combine Microfold and Scenery, dropping down to
# Scenery when Microfold cannot express something, and vice versa, go up to
# Microfold in the middle of some Scenery markup.
#
# At the core of Microfold are *utilities*, *properties*, *conditions*, *cues*,
# and *boxes*.
#
# - *Utilities* target *boxes* with *designations*. For example, `text-neutral-500`
#   targets `text-box` with the designation `{color: (oklch ...)}`. Designations
#   targeting a particular box are merged (with order-sensitive overrides), producing
#   a dictionary of *settings*. Settings are then interpreted by the box. The box
#   may be *instantiated* as a consequence. If no settings target a box, it is not
#   *instantiated*. Utilities occur in the style string:
#   `(p "Hello World" style: "text-neutral-300 bg-neutral-950")`
#
# - *Properties* are similar to utilities but appear not as part of `style: "..."`,
#   but in the node pairspart. For example, the `µ-bg` property sets the fill color
#   of the bg-box: `(el style: "" µ-bg: (oklch 0.4 0.1 blue))`. The empty style is
#   necessary to signal to Microfold that it should administer the node.
#
# - *Conditions* can refer to the node's pairspart or its position in the tree
#   to enable or disable certain styles: `(p "Kaixo" style: "text-red-500 hover:text-blue-500" hover: false)`
#   results in a red text; whereas `(p "Kaixo" style: "text-red-500 hover:text-blue-500" hover: true)`
#   results in a blue one.
#
# - *Cues* allow UI-related state to flow bidirectionally. Each style may look
#   at the pairs of its node and emit cues that flow up (to ancestors; `is-*`,
#   e.g. `is-active`, as in, "hey parent, I am active!"); or down (to children,
#   `cue-*`, e.g. `cue-form-invalid`, as in, "hey children, you're in an invalid
#   form!"). Children may react to cues from their ancestors and/or children and
#   enable/disable utilities (`in-*:*` for top-down cues, as in `in-form-invalid:text-red-500`;
#   `has-*:*` for bottom-up cues, as in `has-active:border-blue-500`); or emit
#   further cues, triggering small- or large-scale chain reactions (e.g. parent
#   says `has-active:cue-active` and children react to `in-active:*` and may
#   sometimes send `is-active` themselves). Cue membranes `membrane` can be used
#   to seal off parts of the tree, let cues flow through selectively, or both.
#
# - A *box* is a function of children nodes and *settings*. It may wrap the children
#   (with e.g. `scenery.padding`, `scenery.stack`, etc.); add new children; extend
#   them; or do any combination of these. Boxes form a kind of "nesting doll"; if
#   the original node has one or more children, then it appears on top (boxes are nested
#   in it, e.g., `(el 1 2 3) -> (el (box 1 2 3))`); otherwise, it is surrounded
#   by boxes, e.g., `(el) -> (box (el))`.
module Ww::Microfold2
  extend self

  # A synchronous wrapper around `Codex`.
  @[Sync::Safe]
  struct SyncCodex
    # :nodoc:
    def initialize(@codex : Codex)
      @lock = Sync::Mutex.new
    end

    # :nodoc:
    def synchronize(&)
      @lock.synchronize { yield @codex }
    end

    # Returns `true` if *head* (e.g. `p` in `(p "hello")` is associated with
    # a Microfold style preset).
    def preset?(head : Term) : Bool
      synchronize { |codex| !!codex.preset?(head) }
    end

    # Forks this codex. See `Codex#fork` for more info.
    def fork : SyncCodex
      fork = synchronize(&.fork)

      SyncCodex.new(fork)
    end
  end

  # Constructs a Microfold codex from *document* and *rem*.
  #
  # *rem* sets the global font size, in pixels. It is the main unit of Microfold.
  # A lot of things are expressed in rems.
  #
  # The returned codex is accompanied by diagnostsics rooted at *document*.
  # The diagnostics point at codex compilation errors, if any.
  def codex(document : Term::Dict, *, rem : Term::Num = Term[16]) : Outcome::Accepted(SyncCodex)
    codex_out = Codex.compile(document, rem)
    codex_out.map { |codex| SyncCodex.new(codex) }
  end

  # :ditto:
  def codex(document : Term, **kwargs) : Outcome::Accepted(SyncCodex)
    codex(document.as_d? || Term[], **kwargs)
  end

  # Rewrites *root* using *codex*.
  #
  # This involves processing styles and Microfold properties in *root*, generating
  # designations, instantiating boxes and so on; resulting in the returned term.
  # The term is accompanied by diagnostics rooted at *root*. Diagnostics are helpful
  # in that they point to missing utilities and the like.
  def render(codex : SyncCodex, root : Term) : Outcome::Accepted(Term)
    unless root_dict = root.as_d?
      return Outcome.ok_despite(root, "root term is not a dict")
    end

    codex.synchronize do |unsafe_codex|
      designation_tree_out = recognize(unsafe_codex, root_dict)
        .map { |style_tree| {style_tree, solve(style_tree)} }
        .map { |style_tree, cues| propagate(style_tree, cues) }
        .map { |uncued_tree| designate?(unsafe_codex, uncued_tree) }

      designation_tree_out.bind do |designation_tree|
        if designation_tree.nil?
          next Outcome.ok(Term.of) # Root absent
        end

        instantiate(unsafe_codex, root_dict, designation_tree)
      end
    end
  end
end

require "./microfold2/feature"
require "./microfold2/parser"
require "./microfold2/codex"
require "./microfold2/recognize"
require "./microfold2/propagate"
require "./microfold2/designate"
require "./microfold2/instantiate"
