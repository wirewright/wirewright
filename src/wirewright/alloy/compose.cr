module Ww::Alloy
  # :nodoc:
  record ComposeContext,
    cache : ExpansionCache,
    ruleset : Ruleset,
    globals : Term::Dict

  private def compose0(ctx : ComposeContext, vars : Term::Dict, template : Term, issues : Issue::Sink) : Ok
    builtin = ->(term : Term, issues : Issue::Sink) do
      Term.matchpi?(term, %{(view arg_)}) do
        case expansion = compose0(ctx, arg, issues)
        in Assign
          expansion.term
        in Splice
          if expansion.offspring.size == 1
            expansion.offspring[0]
          else
            Term.of(expansion.offspring)
          end
        end
      end
    end

    refine = Refine.new do |term, issues|
      compose0?(ctx, term, issues) || Assign.new(term)
    end

    render0(ctx.globals | vars, template, issues, builtin: builtin, refine: refine).as?(Ok) || Splice.new(Term[])
  end

  private def compose0?(ctx : ComposeContext, view : Term, issues : Issue::Sink) : Ok?
    responses = ctx.ruleset.responses(view)
    responses.each do |(pr, rule)|
      case {pr, rule}
      when {Pr::One, Rule::Template}
        # Found.
        issues.adjoin(Spot::Component.new(rule.pattern, rule.body)) do |issues|
          return compose0(ctx, pr.env, rule.body, issues)
        end
      when {Pr::Many, Rule::Template}
        offspring = Term::Dict.build do |commit|
          pr.envs.each do |env|
            issues.adjoin("match env", Term.of(env)) do |issues|
              case expansion = compose0(ctx, env, rule.body, issues)
              in Assign then commit << expansion.term
              in Splice then commit.concat(expansion.offspring.items)
              end
            end
          end
        end

        # Found.
        return Splice.new(offspring)
      end

      # Keep searching...
    end
  end

  private def compose0(ctx : ComposeContext, view : Term, issues : Issue::Sink) : Ok
    cached(ctx.cache, Term.of(ctx.globals, view), issues) do
      compose0?(ctx, view, issues) || flatten(view, issues) { |*args| compose0(ctx, *args) }
    end
  end

  # Recursively expands components in *view*: renders an Alloy view.
  #
  # Please see `rack.device.alloy/view` to learn more. I really want to avoid
  # duplicating the docs.
  def compose0(
    cache : ExpansionCache,
    ruleset : Ruleset,
    globals : Term::Dict,
    vars : Term::Dict,
    view : Term,
    issues : Issue::Sink,
  ) : Ok
    issues.adjoin(Spot::View.new(view)) do |issues|
      compose0(ComposeContext.new(cache, ruleset, globals), vars, view, issues)
    end
  end

  # Shorthand for the main overload of `compose0`; sets up the issue sink
  # for you.
  #
  # Suppresses issues below *severity*.
  def compose0(
    cache : ExpansionCache,
    ruleset : Ruleset,
    globals : Term::Dict,
    vars : Term::Dict,
    view : Term, *,
    severity : Issue::Severity,
  ) : {Ok, Array(Issue::Backtrace)}
    Issue.setup(severity: severity) do |issues|
      compose0(cache, ruleset, globals, vars, view, issues)
    end
  end

  # Shorthand for `compose0` which `collapse`s the resulting expansion.
  def compose_with_issues(
    ruleset : Ruleset,
    globals : Term::Dict,
    vars : Term::Dict,
    view : Term, *,
    severity : Issue::Severity = :minor,
    cache : ExpansionCache = ExpansionUncached.new,
    **kwargs,
  ) : {Term, Array(Issue::Backtrace)}
    expansion, issues = compose0(cache, ruleset, globals, vars, view, **kwargs, severity: severity)

    {collapse(expansion), issues}
  end

  # Shorthand for `compose_with_issues` that suppresses all issues.
  def compose(ruleset : Ruleset, globals : Term::Dict, vars : Term::Dict, view : Term, **kwargs) : Term
    instance, _ = compose_with_issues(ruleset, globals, vars, view, **kwargs, severity: Issue::Severity::QUIET)
    instance
  end
end
