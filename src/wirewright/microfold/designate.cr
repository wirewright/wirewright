module Ww::Microfold
  # A *designation* is a collection of *settings* targeting a *box*.
  # Boxes are free to interpret *settings* however they want.
  defrecord Designation,
    box : Term,
    settings : Term::Dict,
    origin : StyleOrigin

  alias DirectedDesignation = ItemDesignation | NonItemDesignation
  alias NonItemDesignation = SelfDesignation | CascadingDesignation | RootDesignation

  # Targets the node it's attached to.
  defrecord SelfDesignation, successor : Designation

  # Targets the root of the tree the node it's attached to is part of.
  defrecord RootDesignation, settings : Term::Dict

  # Targets the children of the node it's attacheed to with *payload*.
  defrecord ItemDesignation, payload : SelfDesignation | CascadingDesignation

  # Targets the node it's attached to and all nodes below, except nodes whose
  # head is in *exceptions*.
  defrecord CascadingDesignation,
    successor : Designation,
    exceptions : Slice(Term::Sym)

  # NOTE: I had to inline defcase because Crystal's def_equals breaks on nilable Slices,
  # making `==` go to `Comparable` instead of rejecting on type mismatch. That's one
  # of the reasons why I don't use modules the way Crystal stdlib uses them!! Modules
  # are for grouping, period, with extremely rare exceptions.

  class DirectedDesignationNode
    getter designations : Slice(DirectedDesignation)?

    # A child is `nil` if it is absent. We skip such children to avoid processing
    # things that aren't going to be visible after instantiation.
    getter children : Slice(DirectedDesignationNode?)

    getter diagnostics : Slice(Diagnostic)

    def initialize(@designations, @children, @diagnostics)
    end

    def_hash @designations, @children, @diagnostics

    def ==(other : DirectedDesignationNode) : Bool
      if designations0 = @designations
        return false unless designations1 = other.designations
        return false unless designations0 == designations1
      else
        return false unless other.designations.nil?
      end

      return false unless @children == other.children
      return false unless @diagnostics == other.diagnostics

      true
    end

    @_hash : Atomic(UInt64) = Atomic.new(0u64)

    def hash(hasher)
      hash = @_hash.get(:acquire)

      if hash == 0u64
        hash = previous_def(Crystal::Hasher.new).result
        @_hash.set(hash, :release)
      end

      hash.hash(hasher)
    end
  end

  # :nodoc:
  SYM_ROOT_BOX = Term.of(:"root-box")

  private def designate(codex : Codex, vars : Term::Dict, defn : UtilityDefn, origin : StyleOrigin) : Slice(NonItemDesignation)
    settings = Alloy.render(Term.union(codex.globals, vars), defn.contrib)
    unless settings = settings.as_d?
      return Slice(NonItemDesignation).empty
    end

    if defn.box == SYM_ROOT_BOX
      return Slice(NonItemDesignation).of(RootDesignation.new(settings))
    end

    designation = Designation.new(defn.box, settings, origin)

    cascade_pref = defn.cascade_pref
    case {cascade_pref, codex.cascade?(defn.box)}
    in {CascadePrefUnset, false}
      Slice(NonItemDesignation).of(SelfDesignation.new(designation))
    in {CascadePrefUnset, true}, {CascadePrefAll, _}
      Slice(NonItemDesignation).of(CascadingDesignation.new(designation, exceptions: Slice(Term::Sym).empty))
    in {CascadePrefExcept, _}
      Slice(NonItemDesignation).of(CascadingDesignation.new(designation, cascade_pref.exceptions))
    end
  end

  private def designate(codex : Codex, vars : Term::Dict, defn : ShorthandDefn, origin : StyleOrigin) : Slice(NonItemDesignation)
    designations = Pf::Kit.stack_array(NonItemDesignation, 8)

    defn.calls.each do |call|
      next unless target = codex.find_by_name?(call.callee)

      args = Alloy.render(Term.union(codex.globals, vars), Term.of(call.args))
      designations.concat(designate(codex, args.as_d, target, origin))
    end

    designations.to_unsafe_readonly_slice!
  end

  private def designate(codex : Codex, feature : Utility) : Outcome::Accepted(Slice(NonItemDesignation))
    Outcome.accumulate do |acc|
      seen = nil
      matchee = feature.id
      designations = Pf::Kit.stack_array(NonItemDesignation, 8)

      loop do
        if defn = codex.alias?(matchee)
          seen ||= Set(String).new
          break unless seen.add?(matchee)

          matchee = defn.expansion
          next
        end

        match_out = codex.match?(matchee)
        if match_out.is_a?(Outcome::Rejected)
          acc << Diagnostic.of("utility `#{matchee}` not found")
          break
        end

        if match = acc.unwrap(match_out)
          designations.concat(designate(codex, match.vars, match.defn, feature.origin))
        end

        break
      end

      Outcome.ok(designations.to_unsafe_readonly_slice!)
    end
  end

  private def designate(codex : Codex, feature : Item) : Outcome::Accepted(Slice(ItemDesignation | RootDesignation))
    designate(codex, feature.payload).map do |designations|
      designations.to_readonly_slice do |designation|
        case designation
        in SelfDesignation, CascadingDesignation
          ItemDesignation.new(designation)
        in RootDesignation
          designation
        end
      end
    end
  end

  private def designate!(codex : Codex, node : UncuedStyleNode) : DirectedDesignationNode?
    return unless node.present

    diagnostics = Pf::Kit.stack_array(Diagnostic, 4)
    designations = Pf::Kit.stack_array(DirectedDesignation)
    children = Pf::Kit.stack_array(DirectedDesignationNode?)

    if features = node.features
      features.each do |feature|
        feature_designations_out = designate(codex, feature)
        feature_designations_out.unwrap.each do |designation|
          designations << designation
        end
        diagnostics.concat(feature_designations_out.diagnostics)
      end
    end

    node.children.each do |child|
      children << designate_node?(codex, child)
    end

    DirectedDesignationNode.new(
      features ? designations.to_unsafe_readonly_slice! : nil,
      children.to_unsafe_readonly_slice!,
      diagnostics.to_unsafe_readonly_slice!,
    )
  end

  private def designate_node?(codex : Codex, node : UncuedStyleNode) : DirectedDesignationNode?
    codex.designate_cache.put_if_absent(node) do
      designate!(codex, node)
    end
  end

  # Performs the designation rewrite on *node*, producing a `DirectedDesignationNode`.
  #
  # Each Microfold-administered node in a tree of directed designation nodes
  # contains a list of `DirectedDesignation`s, meaning `Designation`s that
  # also have a direction, i.e., they know where they're going to go (is it up?
  # down? or to self? any combination of these?)
  def designate?(codex : Codex, node : UncuedStyleNode) : DirectedDesignationNode?
    codex.designate_cache.epoch do
      designate_node?(codex, node)
    end
  end
end
