module Ww::Alloy
  private def respond0?(ruleset : Ruleset, query : Term, issues : Issue::Sink) : Expansion?
    unless response = ruleset.call?(query)
      issues.note("no response")
      return
    end

    pr, rule = response

    case pr
    in Pr::One
      vars = pr.env
    in Pr::Many
      issues.major("rule must emit zero or one match env")
      return
    end

    case rule
    in Rule::Template
      template = rule.body
    in Rule::BackmapOne, Rule::BackmapMany
      issues.major("expected a template rule, but got a backmap")
      return
    end

    render0(vars, template, issues)
  end

  # Finds a template rule in *ruleset* that matches *query*, and renders its
  # body using Alloy. Uses the rule's pattern match env as the initial variables
  # dict during expansion.
  #
  # Caches query-expansion in *cache*.
  #
  # Reports issues to *issues*.
  def respond0?(
    ruleset : Ruleset,
    cache : ExpansionCache,
    query : Term,
    issues : Issue::Sink,
  ) : Expansion?
    cached(cache, query, issues) do
      # We have no way nor need (?) to cache nils.
      respond0?(ruleset, query, issues) || return
    end
  end

  # Shorthand for `respond0?` that constructs an issue sink for you.
  def respond0?(
    ruleset : Ruleset,
    cache : ExpansionCache,
    query : Term, *,
    severity : Issue::Severity,
  ) : {Expansion?, Array(Issue::Backtrace)}
    Issue.setup(severity: severity) do |issues|
      issues.adjoin(Spot::Response.new(query)) do |issues|
        respond0?(ruleset, cache, query, issues)
      end
    end
  end

  # Finds a template rule in *ruleset* that matches *query*, and renders its
  # body using Alloy. Collapses the resulting expansion (`collapse`). Expansion
  # is `nil` if no matching rule found.
  #
  # Suppresses issues below *severity*.
  #
  # Caches query-expansion pairs in *cache*.
  def respond_with_issues?(
    ruleset : Ruleset,
    query : Term,
    severity : Issue::Severity = :minor,
    cache : ExpansionCache = ExpansionUncached.new,
  ) : {Term?, Array(Issue::Backtrace)}
    expansion, issues = respond0?(ruleset, cache, query, severity: :minor)

    {expansion.try { |x| collapse(x) }, issues}
  end

  # Shorthand for `respond_with_issues?` with all issues suppressed.
  def respond?(ruleset : Ruleset, query : Term, **kwargs) : Term?
    expansion, _ = respond_with_issues?(ruleset, query, **kwargs, severity: :quiet)
    expansion
  end
end
