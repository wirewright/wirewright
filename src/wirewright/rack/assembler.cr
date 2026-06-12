# The assembler pass is responsible for handling `rule`s and `slot`s.
module Ww::Rack::Assembler
  extend self

  # A *recipe* is an interpretation of a rule body.
  alias Recipe = AlloyRecipe | ComponentRecipe

  # Represents Alloy recipes, e.g. `(Text caption_string) => (p ^caption)`.
  defrecord AlloyRecipe, template : Term

  # Represents a component recipe. Component recipes construct a module.
  # Component recipes introduce an auxiliary node, `template`, which can
  # be used anywhere a normal Rack node can be used, for islands of templating
  # (as opposed to Alloy recipes which introduce templating everywhere).
  defrecord ComponentRecipe,
    bindings : Term::Dict,
    tree : D7::ParseTree

  # Constructs a recipe from a rule *body*.
  #
  # This is the function that turns rule bodies, as in `x => ⏏100⏏`,
  # into `Recipe`s that can be instantiated at slots.
  def recipe(clf : D7::Classifier, body : Term) : Recipe
    Term.case(body) do
      matchpi %{[component bindings_dict interior_*]} do
        tree = D7.parse(clf, interior, reply: D7::ParseTree)

        ComponentRecipe.new(bindings.as_d, tree)
      end

      otherwise do
        AlloyRecipe.new(body)
      end
    end
  end

  # Holds **mutable** state for the assembler pass.
  #
  # `Assembler` is ultimately a stateful pass. We need to know which rules
  # have appeared and disappeared and changed at the beginning of each
  # cycle compared with the previous frame to work in an expected way;
  # and this requires us to have some kind of memory.
  #
  # Since we have state anyway, we also use it to cache various things,
  # so that scans on each cycle are as cheap as possible, and scale
  # roughly with the amount of `Assembler`-related change.
  defcase State, rules : Array(Rule)

  # Constructs a Assembler state object. See `State` for more info.
  def state : State
    State.new(rules: [] of Rule)
  end

  defrecord RuleLibrary, defns : Slice(RuleDefn)

  def RuleLibrary.empty : RuleLibrary
    RuleLibrary.new(Slice(RuleDefn).empty)
  end

  def library : RuleLibrary
    RuleLibrary.empty
  end

  def library(document : Term) : RuleLibrary
    unless dict = document.as_d?
      return RuleLibrary.empty
    end

    defns = dict.items.to_compact_readonly_slice do |item|
      Term.matchpi?(item, %{[rule pattern_ template_]}) do
        RuleDefn.new(RuleScope[], :none, pattern, template)
      end
    end

    RuleLibrary.new(defns)
  end

  # Represents a rule definition such as `x => 100` in the circuit. Such rule
  # definitions are inert for Rack. We carry over their *annotations* set.
  defrecord RuleDefn,
    scope : RuleScope,
    annotations : D7::InertAnnotationSet,
    pattern : Term,
    body : Term

  # Represents a compiled rule.
  defrecord Rule,
    defn : RuleDefn,
    op : M1::Op::Any,
    specificity : M1::Specificity,
    recipe : Recipe

  # Constructs a `Rule` from a rule definition *defn*.
  def rule(clf : D7::Classifier, defn : RuleDefn) : Rule
    normp = M1.normal(defn.pattern)
    specificity = M1.specificity(normp)
    op = M1.operator(normp)

    Rule.new(defn, op, specificity, recipe(clf, defn.body))
  end

  # Constructs a `Rule` from a previous rule *pred* and a new rule definition
  # *defn*. *predecessor*'s pattern must be the same as *defn*'s for this to
  # work, otherwise, you must use `rule(RuleDefn)`.
  def rule(clf : D7::Classifier, predecessor : Rule, defn : RuleDefn) : Rule
    assert predecessor.defn.pattern == defn.pattern

    Rule.new(defn, predecessor.op, predecessor.specificity, recipe(clf, defn.body))
  end

  # Finds rules definitions in the given feature *tree*.
  def each_rule_defn(tree : D7::ParseTree, &fn : RuleDefn ->) : Nil
    each_rule_defn(RuleScope[], RuleScope[], tree, fn)
  end

  private def each_rule_defn(scope, next_scope, tree : D7::InertLeaf, fn) : Nil
    Term.matchpi?(tree.feature.node, %{[rule pattern_ body_]}) do
      fn.call(RuleDefn.new(scope, tree.feature.annotations, pattern, body))
    end
  end

  private def each_rule_defn(scope, next_scope, tree : D7::GndLeaf, fn) : Nil
  end

  private def each_rule_defn(scope, next_scope, tree : D7::ScopeNode, fn) : Nil
    each_rule_defn(next_scope, next_scope, tree.child, fn)
  end

  private def each_rule_defn(scope, next_scope, tree : D7::MixtureNode, fn) : Nil
    each_rule_defn(scope, next_scope, tree.child, fn)
  end

  private def each_rule_defn(scope, next_scope, tree : D7::ParentNode, fn) : Nil
    return unless tree.feature.node.probably_includes?(Term[:rule])

    tree.children.each_with_index do |child, index|
      each_rule_defn(scope, next_scope.append(index.to_u32), child, fn)
    end
  end

  # Returns a list of rule definitions found in *tree*.
  def rule_defns(library : RuleLibrary, tree : D7::ParseTree) : Slice(RuleDefn)
    defns = Pf::Kit.stack_array(RuleDefn)
    each_rule_defn(tree) do |defn|
      defns << defn
    end
    defns.concat(library.defns)
    defns.to_unsafe_readonly_slice!
  end

  alias RuleEvent = RuleAdded | RuleRemoved | RuleBodyChanged | RuleScopeChanged

  defrecord RuleAdded, defn : RuleDefn
  defrecord RuleRemoved, rule : Rule, index : Int32
  defrecord RuleBodyChanged, rule : Rule, index : Int32, defn : RuleDefn
  defrecord RuleScopeChanged, rule : Rule, index : Int32, defn : RuleDefn

  # Tells what changed based on the last known list of *rules*, and a new
  # list of rule definitions *defns*.
  def diff(rules : Indexable(Rule), defns : Indexable(RuleDefn)) : Slice(RuleEvent)
    events = Pf::Kit.stack_array(RuleEvent, 8)

    # Find removed rules.
    rules.each_with_index do |rule, index|
      next if defns.any? { |defn| rule.defn.pattern == defn.pattern }

      events << RuleRemoved.new(rule, index)
    end

    # Find new rules.
    # Find rules whose patterns are the same but their bodies are different.
    defns.each do |defn|
      next if defn.annotations.incomplete?

      present = false

      rules.each_with_index do |rule, index|
        next unless rule.defn.pattern == defn.pattern

        present = true

        unless rule.defn.body == defn.body
          events << RuleBodyChanged.new(rule, index, defn)
          next
        end

        unless rule.defn.scope == defn.scope
          events << RuleScopeChanged.new(rule, index, defn)
          next
        end
      end

      next if present

      events << RuleAdded.new(defn)
    end

    events.to_unsafe_readonly_slice!
  end

  # Represents the invalidation of slots whose calls excite *op*.
  alias SlotInvalidation = RuleBodyInvalidation | RuleScopeInvalidation

  defrecord RuleBodyInvalidation, op : M1::Op::Any
  defrecord RuleScopeInvalidation, op : M1::Op::Any, scope : RuleScope

  # Mutates *state* to account for *events*. Returns a list of slot invalidations,
  # which should be shown to all slots in the circuit. Slots will then decide
  # whether to respond to a invalidation and re-instantiate.
  def update(clf : D7::Classifier, state : State, events : Indexable(RuleEvent)) : Slice(SlotInvalidation)
    invalidations = Pf::Kit.stack_array(SlotInvalidation, 8)

    removed = Pf::Kit.stack_array(Int32, 4)

    events.each do |event|
      case event
      in RuleAdded
        rule = rule(clf, event.defn)
        # Appends do not disrupt indices so we can commit them immediately.
        state.rules << rule
        invalidations << RuleBodyInvalidation.new(rule.op)
      in RuleRemoved
        # Removals disrupt indices so we must delay them.
        removed << event.index
        invalidations << RuleBodyInvalidation.new(event.rule.op)
      in RuleBodyChanged
        successor = rule(clf, event.rule, event.defn)
        # Assigns do not disrupt indices.
        state.rules[event.index] = successor
        invalidations << RuleBodyInvalidation.new(event.rule.op)
      in RuleScopeChanged
        rule = event.rule
        successor = Rule.new(event.defn, rule.op, rule.specificity, rule.recipe)
        # Assigns do not disrupt indices.
        state.rules[event.index] = successor
        invalidations << RuleScopeInvalidation.new(successor.op, successor.defn.scope)
      end
    end

    if removed.present?
      removed.unstable_sort! # ascending
      removed.reverse_each do |index|
        state.rules.delete_at(index)
      end
    end

    # Sort most specific first.
    state.rules.sort! { |a, b| b.specificity <=> a.specificity }

    invalidations.to_unsafe_readonly_slice!
  end

  # Slots beyond this depth are ignored by the instantiation process.
  # This limits infinite and deep recursion without forbidding practical
  # recursion and deep slot instantiation. This is basically Rack's notion
  # of a "call stack depth limit".
  SAFE_SLOT_DEPTH = 256

  # Broadcasts *invalidations* to nodes in the given feature *tree*. This
  # can trigger template instantiation using *state*.
  def broadcast(state : State, tree : D7::ParseTree, invalidations : Indexable(SlotInvalidation)) : D7::RepairTree
    depth = 0
    scope = next_scope = RuleScope[]
    broadcast(state, depth, scope, next_scope, tree, invalidations)
  end

  private def broadcast(state, depth, scope, next_scope, tree : D7::InertLeaf, invalidations) : D7::RepairTree
    tree.feature.node
  end

  private def broadcast(state, depth, scope, next_scope, tree : D7::GndLeaf, invalidations) : D7::RepairTree
    node = tree.feature.node

    unless depth <= SAFE_SLOT_DEPTH
      return node
    end

    Term.case(node) do
      matchpi %{[slot call_]} do
        # An uninitialized slot does not care about invalidations, it wants
        # to initialize itself no matter what.
        decision = decision(state, scope, call)
        follow(node, decision)
      end

      otherwise do
        node
      end
    end
  end

  private def broadcast(state, depth, scope, next_scope, tree : D7::ParentNode, invalidations) : D7::RepairTree
    node_dict = tree.feature.node
    node = Term.of(node_dict)

    unless depth <= SAFE_SLOT_DEPTH
      return node
    end

    unless node_dict.probably_includes?(Term[:slot])
      return node
    end

    Term.case(node) do
      matchpi %{[slot call_ _]} do
        continue unless decision = decision?(state, scope, call, invalidations)

        follow(node, decision)
      end

      otherwise do
        D7.repair(tree) do |child, index|
          broadcast(state, depth + 1, scope, next_scope.append(index.to_u32), child, invalidations)
        end
      end
    end
  end

  private def broadcast(state, depth, scope, next_scope, tree : D7::ScopeNode, invalidations) : D7::RepairTree
    D7.repair(tree) do |child|
      broadcast(state, depth, next_scope, next_scope, child, invalidations)
    end
  end

  private def broadcast(state, depth, scope, next_scope, tree : D7::MixtureNode, invalidations) : D7::RepairTree
    D7.repair(tree) do |child|
      broadcast(state, depth, scope, next_scope, child, invalidations)
    end
  end

  # :nodoc:
  alias SlotProposal = ReplaceInstance | EraseInstance

  # :nodoc:
  defrecord ReplaceInstance, replacement : Term
  # :nodoc:
  defrecord EraseInstance

  alias RuleScope = Pf::UPath32

  private def compatible_scopes?(parent : RuleScope, child : RuleScope) : Bool
    return false unless parent.size <= child.size

    # child.starts_with?(parent)
    parent.each_with_index do |id, index|
      return false unless id == child[index]
    end

    true
  end

  private def propose?(state : State, scope : RuleScope, call : Term, invalidation : RuleScopeInvalidation) : SlotProposal?
    return unless M1.probe?(Term[], invalidation.op, call)
    return if compatible_scopes?(invalidation.scope, scope)

    # The event invalidationed this slot. Re-evalaute the slot's content.
    propose(state, scope, call)
  end

  private def propose?(state : State, scope : RuleScope, call : Term, invalidation : RuleBodyInvalidation) : SlotProposal?
    return unless M1.probe?(Term[], invalidation.op, call)

    # The event invalidationed this slot. Re-evalaute the slot's content.
    propose(state, scope, call)
  end

  private def propose(state : State, scope : RuleScope, call : Term) : SlotProposal
    state.rules.each do |rule|
      next unless compatible_scopes?(rule.defn.scope, scope)
      next unless vars = M1.match?(Term[], rule.op, call)

      instance = instantiate(vars, rule.recipe)
      return ReplaceInstance.new(instance)
    end

    EraseInstance.new
  end

  private def decision?(state : State, scope : RuleScope, call : Term, invalidations : Indexable(SlotInvalidation)) : SlotProposal?
    proposals = Pf::Kit.stack_array(SlotProposal, 2)

    invalidations.each do |invalidation|
      next unless proposal = propose?(state, scope, call, invalidation)

      proposals << proposal
    end

    # If there are no proposals, none of the rule invalidations triggered
    # the slot. It should remain unchanged for this cycle.
    return if proposals.empty?

    unless decision = proposals.single?
      # Confused, erase.
      decision = EraseInstance.new
    end

    decision
  end

  private def decision(state : State, scope : RuleScope, call : Term) : SlotProposal
    propose(state, scope, call)
  end

  private def follow(node : Term, decision : EraseInstance) : Term
    Term.morph(node, {2, nil})
  end

  private def follow(node : Term, decision : ReplaceInstance) : Term
    Term.morph(node, {2, decision.replacement})
  end

  # Instantiates the given *recipe*.
  def instantiate(vars : Term::Dict, recipe : AlloyRecipe) : Term
    Alloy.render(vars, recipe.template)
  end

  # :ditto:
  def instantiate(vars : Term::Dict, recipe : ComponentRecipe) : Term
    repair_tree = instantiate(vars, recipe, recipe.tree)
    interior_instance = D7.collapse(repair_tree)

    instance = Term::Dict.build do |commit|
      # Components can be without a surface, such as:
      #
      #   (component
      #     (cell @a 0)
      #     (cell @b 0)
      #     (template
      #       (cell @c ^n)))
      #
      # This should be instantiated as (e.g., with n=123):
      #
      #   (module {}
      #     (cell @a 0)
      #     (cell @b 0)
      #     (cell @c 123))
      #
      # If the user tries to confuse a component by giving it multiple surfaces,
      # it, too, will have no surfaces, and will be instantiated like this.
      commit << :module

      bindings = Term::Dict.build do |commit|
        recipe.bindings.each_entry do |capture, inner|
          outer = vars[capture]?
          commit.with(inner, outer)
        end
      end

      commit << bindings
      commit.concat(interior_instance.items)
    end

    Term.of(instance)
  end

  private def instantiate(vars, recipe, tree : D7::InertLeaf) : D7::RepairTree
    node = tree.feature.node

    Term.case(node) do
      # E.g. (template (cell @x ^100)) => (cell @x 100)
      #
      # But (template (^splice 1 2 3)) => (group 1 2 3)
      matchpi %{(template expr_)} do
        rep = Alloy.render_rep(expr, locals: vars)
        if result = rep.single?
          return result
        end

        result = Term::Dict.build do |commit|
          commit << :group
          commit.concat(rep)
        end

        Term.of(result)
      end

      # E.g.
      #   (template
      #     (cell @x ^100)
      #     (cell @y ^200))
      # =>
      #   (group
      #     (cell @x 100)
      #     (cell @y 200))
      matchpi %{(template _+)} do
        Alloy.render(vars, Term.morph(node, {0, :group}))
      end

      otherwise do
        node
      end
    end
  end

  private def instantiate(vars, recipe, tree : D7::GndLeaf) : D7::RepairTree
    tree.feature.node
  end

  private def instantiate(vars, recipe, tree : D7::ScopeNode | D7::MixtureNode | D7::ParentNode) : D7::RepairTree
    D7.repair(tree) { |child| instantiate(vars, recipe, child) }
  end

  def step(rclf : D7::Classifier, rtree : D7::ParseTree, wtree : D7::ParseTree, state : State, library : RuleLibrary = RuleLibrary.empty) : Term
    # Find rule definitions. This is an unavoidable scan of the tree.
    # Cues help us skip some paths not containing a rule.
    defns = rule_defns(library, rtree)

    # See what changed.
    events = diff(state.rules, defns)

    # If anything changed, generate invalidations & sync state.
    invalidations = Slice(SlotInvalidation).empty
    if events.present?
      invalidations = update(rclf, state, events)
    end

    # Broadcast invalidations and produce a repair tree. This is another
    # unavoidable scan & rewrite of the tree. Most often this does nothing
    # or close to nothing, so we optimize for that. We use cues as well.
    repair_tree = broadcast(state, wtree, invalidations)

    D7.collapse(repair_tree)
  end

  def step(rclf : D7::Classifier, wclf : D7::Classifier, state : State, circuit : Term, library : RuleLibrary = RuleLibrary.empty)
    rtree = D7.parse(rclf, circuit, reply: D7::ParseTree)
    if rclf == wclf
      wtree = rtree
    else
      wtree = D7.parse(wclf, circuit, reply: D7::ParseTree)
    end

    step(rclf, rtree, wtree, state, library)
  end

  def pass(rclf : D7::Classifier, wclf : D7::Classifier, state : State) : D7::Pass
    D7::Pass.new { |circuit| Slice[step(rclf, wclf, state, circuit)] }
  end
end
