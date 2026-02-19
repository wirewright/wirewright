module Ww::Alloy
  alias ComposeHook = ComposeContext, Proc(ComposeContext, Term, Term), Term -> Term

  # :nodoc:
  record ComposeContext,
    ruleset : Ruleset,
    globals : Term::Dict,
    hook : ComposeHook

  private def compose0(ctx : ComposeContext, vars : Term::Dict, template : Term, issues : Issue::Sink) : Term::Rep
    eval = Eval.new do |expr, default, _, issues|
      value = default.call(issues)

      view = ->(ctx : ComposeContext, arg : Term) do
        Term.collapse(compose0(ctx, arg, issues))
      end

      Term.case(value) do
        matchpi %{(view arg_)} { view.call(ctx, arg) }
        otherwise { ctx.hook.call(ctx, view, value) }
      end
    end

    refine = Refine.new do |term, issues|
      compose0?(ctx, term, issues) || Term.rep(term)
    end

    render0(Term.union(ctx.globals, vars), template, issues, eval: eval, refine: refine).as?(Term::Rep) || Term.rep
  end

  private def compose0?(ctx : ComposeContext, view : Term, issues : Issue::Sink) : Term::Rep?
    ctx.ruleset.query(view) do |envs, rule|
      assert envs.present?

      case {envs.size, rule}
      when {1, Rule::Template}
        # Found.
        issues.adjoin(Spot::Component.new(rule.pattern, rule.body)) do |issues|
          return compose0(ctx, envs.first, rule.body, issues)
        end
      when {_, Rule::Template}
        sink = Pf::Kit.stack_array(Term)

        envs.each do |env|
          issues.adjoin("match env", Term.of(env)) do |issues|
            expansion = compose0(ctx, env, rule.body, issues)
            expansion.each { |offspring| sink << offspring }
          end
        end

        # Found.
        return Term.rep(sink)
      end

      # TODO: Backmaps could run Alloy::Applier with (view _) available during eval.

      # Keep searching...
    end
  end

  private def compose0(ctx : ComposeContext, view : Term, issues : Issue::Sink) : Term::Rep
    if expansion = compose0?(ctx, view, issues)
      return expansion
    end

    result = Term.flatten(view, part: Term::Dict.entries) do |key, value|
      issues.adjoin(key: key, detail: "in key") do |issues|
        compose0(ctx, value, issues)
      end
    end

    Term.rep(result)
  end

  alias View = Template | Component

  defrecord Template, vars : Term::Dict, template : Term
  defrecord Component, component : Term

  # Constructs a `Template` view arg.
  #
  # When passed to `compose`, first expands *template* using *vars* and only then
  # starts rule search. This is useful for handling "toplevel" templates, such as
  # those for `window`.
  def template(vars : Term::Dict, template : Term) : Template
    Template.new(vars, template)
  end

  # Constructs a `Component` view arg.
  #
  # When passed to `compose`, triggers rule search immediately on raw *component*
  # instead of giving it to `Alloy.render` first.
  def component(component : Term) : Component
    Component.new(component)
  end

  # :nodoc:
  def compose0(
    ruleset : Ruleset,
    globals : Term::Dict,
    view : Template,
    issues : Issue::Sink,
    hook : ComposeHook,
  ) : Term::Rep
    issues.adjoin(Spot::View.new(view.template)) do |issues|
      compose0(ComposeContext.new(ruleset, globals, hook), view.vars, view.template, issues)
    end
  end

  # :nodoc:
  def compose0(
    ruleset : Ruleset,
    globals : Term::Dict,
    view : Component,
    issues : Issue::Sink,
    hook : ComposeHook,
  ) : Term::Rep
    issues.adjoin(Spot::View.new(view.component)) do |issues|
      compose0(ComposeContext.new(ruleset, globals, hook), view.component, issues)
    end
  end

  {% if flag?(:docs) %}
    # Recursively instantiates components in *view* (or *view* itself if it
    # is a `Component`).
    #
    # Components are defined as template rules in *ruleset*.
    #
    # - *globals* are mixed into each rule's env. The rule's env is preferred over
    #   *globals* in case of name collision.
    # - *issues* is the issue sink to report issues to.
    # - *hook* is a function that runs during Alloy expression evaluation in templates,
    #   on ascent (i.e., you get the term after evaluation). *hook* runs at the same time
    #   as `(view _)` expressions are instantiated recursively. You can use *hook* to
    #   provide your own recursive expansion similar to `view`.
    def compose0(
      ruleset : Ruleset,
      globals : Term::Dict,
      view : View,
      issues : Issue::Sink,
      hook : ComposeHook,
    ) : Term::Rep
    end
  {% end %}

  # Shorthand for the main overload of `compose0`; sets up the issue sink
  # for you.
  #
  # Suppresses issues below *severity*.
  def compose0(
    ruleset : Ruleset,
    globals : Term::Dict,
    view : View, *,
    severity : Issue::Severity,
    hook = ComposeHook.new { |_, _, value| value },
  ) : {Term::Rep, Array(Issue::Backtrace)}
    Issue.setup(severity: severity) do |issues|
      compose0(ruleset, globals, view, issues, hook)
    end
  end

  # Shorthand for `compose0` which `collapse`s the resulting expansion.
  def compose_with_issues(
    ruleset : Ruleset,
    globals : Term::Dict,
    view : View, *,
    severity : Issue::Severity = :minor,
    **kwargs,
  ) : {Term, Array(Issue::Backtrace)}
    expansion, issues = compose0(ruleset, globals, view, **kwargs, severity: severity)

    {collapse(expansion), issues}
  end

  # Shorthand for `compose_with_issues` that suppresses all issues.
  def compose(ruleset : Ruleset, globals : Term::Dict, view : View, **kwargs) : Term
    instance, _ = compose_with_issues(ruleset, globals, view, **kwargs, severity: Issue::Severity::QUIET)
    instance
  end
end
