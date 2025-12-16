# Alloy is a general-purpose term templating engine for Wirewright.
#
# ```
# vars = Term[x: 100, y: 200]
# template = ML.term(%{(sum "of" ^x ^y is ^(+ x y))})
# renderout = Alloy.render(vars, template)
# renderout # => term (sum "of" 100 200 is 300)
# ```
#
# FIXME: Currently Alloy uses the call stack for recursive rewriting, in both
# `render` and `compose` (especially compose since it can be recursive, Alloy.render
# by itself is guaranteed to terminate since its input is finite and it always does only
# one pass). In practice, we'd like to avoid the call stack or at least "fence" it somehow.
# This won't solve infinite recursion in principle but will at least prevent us from
# segfaulting on it. To contain deep recursion we'd have to encode the notion of Aborted
# (as in e.g. Mathematica) as one possible result of expansion along with Ok and Err.
# We can abort when we exceed a certain "energy" budget (e.g. stack depth). Aborted
# returns the original expression, effectively Alloy is saying: "I gave up, can't do
# it, increase recursion limit and re-run *this* or give up too".
module Ww::Alloy
  extend self

  # Represents the result of template expansion. You usually do not have to
  # deal with this unless you want to support splicing of top-level Alloy
  # templates or views into your term-of-choice.
  alias Expansion = Ok | Err

  # Successful expansion.
  alias Ok = Assign | Splice

  # Represents the expansion of the current term into one offspring term
  # (possibly the same term).
  record Assign, term : Term

  # Represents the expansion of the current term into zero or more offspring
  # terms (possibly containing the original term).
  record Splice, offspring : Term::Dict

  # Represents the absence of an expansion due to an error (e.g. undefined variable).
  # Alloy nodes may choose to handle this differently in principle; but in practice,
  # for consistency, they *omit* them from the template. This has the nice property that
  # if there is a chain of multiple Alloy passes, errors in the previous pass are not
  # going to propagate into the next one and so on.
  #
  # See `collapse` for representatinal issues when used in practice.
  record Err

  # :nodoc:
  def collapse(expansion : Err) : Term
    Term.of
  end

  # :nodoc:
  def collapse(expansion : Assign) : Term
    expansion.term
  end

  # :nodoc:
  def collapse(expansion : Splice) : Term
    Term.of(expansion.offspring)
  end

  {% if flag?(:docs) %}
    # We collapse *expansion* to a term representation in the following way:
    #
    # - An `Err` collapses to `()`.
    # - An `Assign` collapses to its term.
    # - A `Splice` collapses to its offspring list term.
    #
    # Most importantly, this means that the empty dict `()` is highly ambiguous.
    # It may result from either of the three. Normal dictionaries are ambiguous
    # as well, because they may come from both `Assign` and `Splice`. In a sense,
    # by calling `collapse`, you "burn" information about what kind of *expansion*
    # you had.
    #
    # This is dirty, but it works in practice -- most of the times, you just don't
    # care. If you need full information, however, use `Expansion` as-is (each important
    # method has a non-collapsing overload). You can also encode `Expansion` yourself,
    # perhaps into something like `err/(assign _)/(splice _*)`.
    def collapse(expansion : Expansion) : Term
    end
  {% end %}

  alias ExpansionCache = ICache(Term, Ok)
  alias ExpansionUncached = Uncached(Term, Ok)

  private def cached(cache : ExpansionCache, key : Term, issues : Issue::Sink, & : -> Expansion) : Expansion
    if memo = cache[key]?
      return memo
    end

    version0 = issues.version
    expansion = yield
    unless expansion.is_a?(Ok)
      return expansion
    end
    version1 = issues.version

    # Do not cache if there were any issues.
    if version0 == version1
      cache[key] = expansion
    end

    expansion
  end

  # Contains Alloy-related `Issue::Spot`s.
  module Spot
    # Represents the difference between *env* and *vars*, *env* being a guaranteed
    # superset of *vars*.
    record VarDelta, vars : Term::Dict, env : Term::Dict do
      include Issue::Spot
    end

    # Marks the beginning of an Alloy view. Keypaths and other spots
    # below `ViewSpot` refer to *view*, the view that was passed
    # to `Alloy.compose`.
    record View, view : Term do
      include Issue::Spot
    end

    # Marks the beginning of an Alloy template. Keypaths and other spots
    # below `Template` refer to *template*, the template that was passed
    # to `Alloy.render`.
    record Template, template : Term do
      include Issue::Spot
    end

    # Marks the beginning of an Alloy component. Keypaths and other spots
    # below `Component` refer to *template*; with *pattern* provided as additional
    # information to search the ruleset, determine source location, or both.
    record Component, pattern : Term, template : Term do
      include Issue::Spot
    end

    # Marks the beginning of a response to *query*, the query that was passed
    # to `Alloy.respond?`.
    record Response, query : Term do
      include Issue::Spot
    end

    # Marks the beginning of an expansion of an Alloy node, e.g. `^match`.
    # The keypath is reset below expansion spots, because if it was not,
    # it would no longer point to a valid location in the template.
    record Expansion, detail : String, offspring : Term do
      include Issue::Spot
    end
  end

  # A backmap applier that supports Alloy.
  #
  # TODO: In the future, this will be the default applier for the backmap engine.
  # To ensure a smooth transition, this applier will gradually replace `DefaultApplier`.
  struct Applier
    def apply(up, dn, my, body)
      eval = Alloy::Eval.new do |expr, default, cont, issues|
        Term.case(expr) do
          matchpi %{(up capture_)} do
            unless value = up[capture]? || my[capture]?
              issues.major { "undefined capture #{capture}" }
              value = body
            end

            value
          end

          matchpi %{(dn capture_)} do
            unless value = up[capture]? || my[capture]?
              issues.major { "undefined capture #{capture}" }
              value = body
            end

            value
          end

          otherwise { default.call(issues) }
        end
      end

      # FIXME: backmaps must support Issue::Sink I suppose. We can't just throw
      # issues away like this.
      expansion, _ = Alloy.render0(my, body, eval: eval, severity: :quiet)

      case expansion
      in Err    then Rewrite.one(body) # ?!
      in Assign then Rewrite.one(expansion.term)
      in Splice then Rewrite.many(expansion.offspring)
      end
    end

    def call(up0, up1, down, my, matchee0 : Term?, body)
      Term.case(body) do
        matchpi %[($tr pred_ succ_)] do
          {up1.with(pred, matchee0), apply(up0, down, my, succ)}
        end

        otherwise do
          {up1, apply(up0, down, my, body)}
        end
      end
    end
  end
end

require "./alloy/flatten"
require "./alloy/render"
require "./alloy/respond"
require "./alloy/compose"
