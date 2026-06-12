module Ww::Alloy
  defrecord Env, globals : Term::Dict, vars : Term::Dict

  defrecord RuleData,
    components : ComponentSet,
    maxdepth : UInt32

  alias RenderKey = {Term::Dict, Template}
  alias RenderCache = ICache(RenderKey, Term::Rep)

  alias Reftab = Hash(Template | CaseWhen, Set(Ref))

  defrecord RenderContext,
    cache : RenderCache,
    reftab : Reftab,
    primitive : Nitrene::Eval,
    composite : Nitrene::Eval

  protected def primitive(ctx : RenderContext, rdata : RuleData, globals : Term::Dict) : Nitrene::Eval
    Nitrene::Eval.new do |it, vars, expr|
      Term.case(expr) do
        matchpi %{(view term_)} do
          Term.collapse(rewrite(ctx, rdata, globals, term))
        end

        otherwise do
          ctx.primitive.call(it, vars, expr)
        end
      end
    end
  end

  protected def composite : Nitrene::Eval
    ->(it : Nitrene::Interpreter, vars : Term::Dict, expr : Term) do
      Term.case(expr) do
        matchpi %{(^ _)} do
          expr
        end

        otherwise do
          Nitrene.inert
        end
      end
    end
  end

  private VIEW_SKETCH = Term::Dict::Sketch.symbol(Term[:view], Term.hashcode(Term[:view]))

  def eval(ctx : RenderContext, rdata : RuleData, env : Env, expr : NiExpr) : Term
    primitive = ctx.primitive

    # Avoid allocating the primitive() eval proc if possible.
    pass do
      next unless dict = expr.term.as_d?

      summary = dict.summary
      next unless summary.size_set.includes?(2) # (view _)
      next unless VIEW_SKETCH.subset_of?(summary.symbol_sketch)

      primitive = Alloy.primitive(ctx, rdata, env.globals)
    end

    it = Nitrene::Interpreter.new(ctx.composite, primitive)
    Nitrene.eval(it, env.vars, expr.term)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : NiExpr) : Term::Rep
    Term.rep(eval(ctx, rdata, env, template))
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Var) : Term::Rep
    unless value = env.vars[template.name]?
      return Term.rep
    end

    Term.rep(value)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : DisplayVar) : Term::Rep
    unless value = env.vars[template.name]?
      return Term.rep
    end

    Term.rep_of(ML.display(value, endl: false))
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : SpliceVar) : Term::Rep
    unless value = env.vars[template.name]?.as_d?
      # Absent or e.g. ^*xs where xss: 100
      #
      # NOTE: We could replace it by 100, but then the error would be concealed.
      # So instead let's not render anything so that it's obvious something's wrong.
      return Term.rep
    end

    # E.g. ^*xs where xs: (1 2 3)
    Term.rep(value.items)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : NiSplice) : Term::Rep
    value = eval(ctx, rdata, env, template.expr)
    unless dict = value.as_d?
      # E.g. ^*100
      #
      # Ditto.
      return Term.rep
    end

    # ^*(1 2 3)
    Term.rep(dict.items)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Literal) : Term::Rep
    Term.rep(template.term)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Splice) : Term::Rep
    Term.flatten(template.children) do |child|
      render(ctx, rdata, env, child)
    end
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Module) : Term::Rep
    globals = Term::Dict.build do |commit|
      template.bindings.each do |inner, outer|
        next unless value = env.globals[outer]?

        commit.with(inner, value)
      end
    end

    vars = Term::Dict.build do |commit|
      template.bindings.each do |inner, outer|
        next unless value = env.vars[outer]?

        commit.with(inner, value)
      end
    end

    render(ctx, rdata, Env.new(globals, vars), template.body)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Case) : Term::Rep
    matchee = eval(ctx, rdata, env, template.expr)

    render(ctx, rdata, env, template.branches, matchee)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : VarsCase) : Term::Rep
    matchee = Term.of(env.vars)

    render(ctx, rdata, env, template.branches, matchee)
  end

  private def render(ctx : RenderContext, rdata : RuleData, env : Env, branches : Enumerable(CaseWhen), matchee : Term) : Term::Rep
    branches.each do |branch|
      matches = M1.matches(Term[], branch.pattern.op, matchee)
      next unless matches.present?

      rep = Term.flatten(matches) do |match|
        subenv = Env.new(env.globals, Term.union(env.vars, match))
        render(ctx, rdata, subenv, branch.body)
      end

      return rep
    end

    Term.rep
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Cond) : Term::Rep
    matchee = eval(ctx, rdata, env, template.expr)

    if matchee == Term.of(false)
      return render(ctx, rdata, env, template.falsey)
    end

    render(ctx, rdata, env, template.truthy)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : EachItem) : Term::Rep
    iterable = eval(ctx, rdata, env, template.iterable)
    unless iterable = iterable.as_d?
      return Term.rep
    end

    render(ctx, rdata, env, iterable.items, template.pattern, template.body, &.itself)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : EachItemEntry) : Term::Rep
    iterable = eval(ctx, rdata, env, template.iterable)
    unless iterable = iterable.as_d?
      return Term.rep
    end

    ee = iterable.itemspart.ee(ordered: true)
    render(ctx, rdata, env, ee, template.pattern, template.body) do |(key, value)|
      # NOTE: This is flipped for historical reasons: key is in fact index, and value
      # is in fact item, and it's flipped to (item index) because I'm too used to
      # Crystal's each_with_index which also gives |item, index|.
      Term.of(value, key)
    end
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : EachPairEntry) : Term::Rep
    iterable = eval(ctx, rdata, env, template.iterable)
    unless iterable = iterable.as_d?
      return Term.rep
    end

    ee = iterable.pairspart.ee(ordered: true)
    render(ctx, rdata, env, ee, template.pattern, template.body) do |(key, value)|
      Term.of(key, value)
    end
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : EachEntry) : Term::Rep
    iterable = eval(ctx, rdata, env, template.iterable)
    unless iterable = iterable.as_d?
      return Term.rep
    end

    ee = iterable.ee(ordered: true)
    render(ctx, rdata, env, ee, template.pattern, template.body) do |(key, value)|
      Term.of(key, value)
    end
  end

  private def render(ctx : RenderContext, rdata : RuleData, env, iterable : Enumerable, pattern : Pattern, body : Template, &) : Term::Rep
    Term.flatten(iterable) do |object|
      item = yield object

      matches = M1.matches(Term[], pattern.op, item)
      Term.flatten(matches) do |match|
        subenv = Env.new(env.globals, Term.union(env.vars, match))
        render(ctx, rdata, subenv, body)
      end
    end
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Let) : Term::Rep
    vars = env.vars.transaction do |commit|
      template.bindings.each do |key, expr|
        commit.with(key, eval(ctx, rdata, env, expr))
      end
    end

    render(ctx, rdata, Env.new(env.globals, vars), template.body)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Site) : Term::Rep
    dict1 = Term.flatten(template.dict, part: Term::Dict.entries) do |key, value|
      render(ctx, rdata, env, template.parts[key])
    end

    call(ctx, rdata, env.globals, Term.of(dict1))
  end

  private def call(ctx : RenderContext, rdata : RuleData, globals : Term::Dict, call : Term) : Term::Rep
    if rdata.maxdepth.zero?
      return Term.rep(call)
    end

    rdata.components.each_with_matches(call) do |component, matches|
      rep = Term.flatten(matches) do |match|
        rdata = RuleData.new(rdata.components, rdata.maxdepth - 1)
        env = Env.new(globals, Term.union(globals, match))

        render(ctx, rdata, env, component.template)
      end

      return rep
    end

    Term.rep(call)
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Extend) : Term::Rep
    extension = eval(ctx, rdata, env, template.extension)
    unless extension = extension.as_d?
      # (/ 1 2 ^… 100)
      return render(ctx, rdata, env, template.child)
    end

    unless child = template.child.as?(Site)
      # (/ 1 2 ^… {precision: 3})
      rep = Term.flatten(render(ctx, rdata, env, template.child)) do |item|
        if dict = item.as_d?
          Term.rep_of(Term.union(dict, extension))
        else
          Term.rep(item)
        end
      end

      return rep
    end

    # (/ ^a ^b ^… {precision: 3})
    dict1 = Term.flatten(child.dict, part: Term::Dict.entries) do |key, value|
      render(ctx, rdata, env, child.parts[key])
    end

    dict1 = Term.union(dict1, extension)

    call(ctx, rdata, env.globals, Term.of(dict1))
  end

  private def render!(ctx : RenderContext, rdata : RuleData, env : Env, template : Render) : Term::Rep
    rep = render(ctx, rdata, env, template.subordinate)

    matches = M1.matches(Term[], template.pattern.op, Term.of(rep))
    Term.flatten(matches) do |match|
      render(ctx, rdata, Env.new(env.globals, Term.union(env.vars, match)), template.body)
    end
  end

  private def render(ctx : RenderContext, rdata : RuleData, env : Env, template : Template)
    unless refs = ctx.reftab[template]?
      return render!(ctx, rdata, env, template)
    end

    deps = Term.select(env.vars, in: refs)

    ctx.cache.put_if_absent({deps, template}) do
      render!(ctx, rdata, env, template)
    end
  end

  # Defines the maximum depth for component evaluation. Components deeper
  # than this are left as-is.
  MAXDEPTH = 128u32

  # TODO: We should probably not use the call stack for recursion, since on
  # deep trees we can SEGFAULT, and SEGFAULTs aren't fun.
  private def render(ctx : RenderContext, globals : Term::Dict, locals : Term::Dict, unit : Unit) : Term::Rep
    rdata = RuleData.new(unit.components, MAXDEPTH)
    env = Env.new(globals, Term.union(globals, locals))

    render(ctx, rdata, env, unit.template)
  end
end
