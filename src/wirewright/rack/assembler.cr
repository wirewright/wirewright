# The assembler pass is responsible for handling `rule`s and `slot`s.
module Ww::Rack::Assembler
  extend self

  # :nodoc:
  SYM_RULE = Term.of(:rule)

  # :nodoc:
  SYM_SLOT = Term.of(:slot)

  # A *recipe* is an interpretation of a rule body.
  alias Recipe = AlloyRecipe | ComponentRecipe | CallRecipe

  # Represents Alloy recipes, e.g. `(Text caption_string) => (p ^caption)`.
  defrecord AlloyRecipe, template : Term

  # A recipe which works like `AlloyRecipe` but processes the instance as
  # a slot call immediately, without the need for intermediate slots.
  defrecord CallRecipe, template : Term

  # Represents a component recipe. Component recipes construct a module.
  # Component recipes introduce an auxiliary node, `template`, which can
  # be used anywhere a normal Rack node can be used, for islands of templating
  # (as opposed to Alloy recipes which introduce templating everywhere).
  defrecord ComponentRecipe, bindings : Term::Dict, tree : D7::ParseTree

  # Constructs a recipe from a rule *body*.
  #
  # This is the function that turns rule bodies, as in `x => ⏏100⏏`,
  # into `Recipe`s that can be instantiated at slots.
  def recipe(clf : D7::Classifier, body : Term) : Recipe
    Term.case(body) do
      matchpi %{[component bindings_dict interior_*]} do
        tree = D7.parse(clf, interior)

        ComponentRecipe.new(bindings.as_d, tree)
      end

      matchpi %{[slot template_]} do
        CallRecipe.new(template)
      end

      otherwise do
        AlloyRecipe.new(body)
      end
    end
  end

  # Holds **mutable** state for the assembler pass.
  #
  # `Assembler` is ultimately a stateful pass. We need to know which rules
  # appeared, disappeared, and changed at the beginning of each frame compared
  # with the previous frame to work in an expected way; and this requires us
  # to have some kind of memory.
  #
  # Since we have state anyway, we also use it to cache various things,
  # so that scans on each frame are as cheap as possible, and scale roughly
  # with the amount of `Assembler`-related change.
  defcase State, rules : Array(Rule)

  # Constructs a Assembler state object. See `State` for more info.
  def state : State
    State.new(rules: [] of Rule)
  end

  defrecord RuleLibrary, defns : Slice(RuleDefn)

  # Constructs an empty rule library.
  def library : RuleLibrary
    RuleLibrary.new(Slice(RuleDefn).empty)
  end

  # Constructs a rule library from a *document* term.
  def library(document : Term) : RuleLibrary
    unless dict = document.as_d?
      return library
    end

    defns = dict.items.to_compact_readonly_slice do |item|
      Term.matchpi?(item, %{[rule pattern_ template_]}) do
        RuleDefn.new(RuleScope[], pattern, template)
      end
    end

    RuleLibrary.new(defns)
  end

  # Represents a rule definition such as `x => 100` in the circuit. Such rule
  # definitions are inert for Rack.
  defrecord RuleDefn,
    scope : RuleScope,
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

  private def each_rule_defn(scope, next_scope, tree : D7::ParseTree, fn) : Nil
    case tree
    in D7::InertLeaf
    in D7::GndLeaf
      Term.matchpi?(tree.feature.node, %{[rule pattern_ body_]}) do
        fn.call(RuleDefn.new(scope, pattern, body))
      end
    in D7::ScopeNode
      each_rule_defn(next_scope, next_scope, tree.child, fn)
    in D7::MixtureNode
      each_rule_defn(scope, next_scope, tree.child, fn)
    in D7::ParentNode
      return unless tree.summary.has_head?(SYM_RULE)

      tree.children.each_with_index do |child, index|
        each_rule_defn(scope, next_scope.append(index.to_u32), child, fn)
      end
    end
  end

  # Returns a list of rule definitions found in *tree*.
  def rule_defns(library : RuleLibrary, tree : D7::ParseTree) : Slice(RuleDefn)
    defns = Pf::Kit.stack_array(RuleDefn)
    each_rule_defn(tree) { |defn| defns << defn }
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
    # Find rules whose patterns are the same but their bodies are different. That's
    # an update for us.
    defns.each do |defn|
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

    unless depth < SAFE_SLOT_DEPTH
      return node
    end

    Term.case(node) do
      matchpi %{[slot call_]} do
        # An uninitialized slot does not care about invalidations, it wants
        # to initialize itself no matter what.
        case decision = decision(state, scope, call)
        in EraseInstance
          Term.morph(node, {2, nil})
        in ReplaceInstance
          Term.morph(node, {2, decision.replacement})
        end
      end

      otherwise do
        node
      end
    end
  end

  private def broadcast(state, depth, scope, next_scope, tree : D7::ParentNode, invalidations) : D7::RepairTree
    node = Term.of(tree.feature.node)

    unless depth < SAFE_SLOT_DEPTH
      return node
    end

    Term.case(node) do
      matchpi %{[slot call_ _]} do
        case decision = decision?(state, scope, call, invalidations)
        in Nil
          continue
        in EraseInstance
          Term.morph(node, {2, nil})
        in ReplaceInstance
          Term.morph(node, {2, decision.replacement})
        end
      end

      otherwise do
        unless tree.summary.has_head?(SYM_SLOT)
          return node
        end

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

    # The event invalidated this slot. Re-evaluate the slot's content.
    propose(state, scope, call)
  end

  private def propose?(state : State, scope : RuleScope, call : Term, invalidation : RuleBodyInvalidation) : SlotProposal?
    return unless M1.probe?(Term[], invalidation.op, call)

    # The event invalidated this slot. Re-evaluate the slot's content.
    propose(state, scope, call)
  end

  private def propose(state : State, scope : RuleScope, call : Term) : SlotProposal
    state.rules.zip(0u32...state.rules.size.to_u32) do |rule, rule_id|
      next unless compatible_scopes?(rule.defn.scope, scope)
      next unless vars = M1.match?(Term[], rule.op, call)

      instance = instantiate(state.rules, rule, rule_id, vars)
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

  def instantiate(rules : Array(Rule), rule : Rule, rule_id : UInt32, vars : Term::Dict) : Term
    instantiate(InstantiateContext.new(rules, rule.defn.scope, path: Pf::USet32[rule_id]), vars, rule.recipe)
  end

  defrecord InstantiateContext, rules : Array(Rule), scope : RuleScope, path : Pf::USet32

  private def instantiate(ctx : InstantiateContext, vars : Term::Dict, recipe : AlloyRecipe) : Term
    Alloy.render(vars, recipe.template)
  end

  private def instantiate(ctx : InstantiateContext, vars : Term::Dict, recipe : CallRecipe) : Term
    call = Alloy.render(vars, recipe.template)

    ctx.rules.zip(0u32...ctx.rules.size.to_u32) do |rule, rule_id|
      next if rule_id.in?(ctx.path)
      next unless compatible_scopes?(rule.defn.scope, ctx.scope)
      next unless call_vars = M1.match?(Term[], rule.op, call)

      # Prevent infinite recursion by keeping track of the path. Whenever we revisit
      # the same rule, we skip it. If no other rule matches, we emit `(slot ^call)` --
      # which *would* delay further recursion until the *next* tick. Such pathological
      # recursion is forced to stop at the maximum recursion depth.
      subctx = InstantiateContext.new(ctx.rules, ctx.scope, ctx.path.add(rule_id))
      return instantiate(subctx, call_vars, rule.recipe)
    end

    Term.of(:slot, call)
  end

  private def instantiate(ctx : InstantiateContext, vars : Term::Dict, recipe : ComponentRecipe) : Term
    repair_tree = render_component_templates(vars, recipe, recipe.tree)
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

  private def render_component_templates(vars, recipe, tree : D7::InertLeaf) : D7::RepairTree
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

  private def render_component_templates(vars, recipe, tree : D7::GndLeaf) : D7::RepairTree
    tree.feature.node
  end

  private def render_component_templates(vars, recipe, tree : D7::ScopeNode | D7::MixtureNode | D7::ParentNode) : D7::RepairTree
    D7.repair(tree) { |child| render_component_templates(vars, recipe, child) }
  end

  def step(state : State, parser : D7::Parser, library : RuleLibrary, circuit : Term) : Slice(Term)
    # The only way a slot can create another slot is through deepening (i.e., creating
    # or containing a child slot). Slot expansion terminates at `SAFE_SLOT_DEPTH`.
    # So we will need at most `SAFE_SLOT_DEPTH` expansion passes to reach
    # the bottom with each level growing down.
    SAFE_SLOT_DEPTH.times do
      tree = parser.parse(circuit)

      summary = D7.summary(tree)
      break unless summary.has_head?(SYM_SLOT)

      # Find rule definitions. This is an unavoidable scan of the tree.
      # Cues help us skip some paths not containing a rule.
      defns = library.defns
      if summary.has_head?(SYM_RULE)
        defns = rule_defns(library, tree)
      end

      # See what changed.
      events = diff(state.rules, defns)

      # If anything changed, generate invalidations & sync state.
      invalidations = Slice(SlotInvalidation).empty
      if events.present?
        invalidations = update(parser.clf, state, events)
      end

      # Skip broadcast if no invalidations -- provided there are no (slot _)s which
      # we would need to look at regardless of invalidations.
      break if invalidations.empty? && !summary.has_signature?(D7::NodeSignature.new(SYM_SLOT, arity: 2))

      # Broadcast invalidations and produce a repair tree. This is another
      # unavoidable scan & rewrite of the tree. Most often this does nothing
      # or close to nothing, so we optimize for that. We use cues as well.
      repair_tree = broadcast(state, tree, invalidations)

      circuit1 = D7.collapse(repair_tree)
      break if circuit == circuit1

      circuit = circuit1
    end

    Slice[circuit]
  end
end
