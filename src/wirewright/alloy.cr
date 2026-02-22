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

  alias ExpansionCache = ICache(Term, Term::Rep)
  alias ExpansionUncached = Uncached(Term, Term::Rep)

  private def cached(cache : ExpansionCache, key : Term, issues : Issue::Sink, & : -> Term::Rep) : Term::Rep
    if memo = cache[key]?
      return memo
    end

    version0 = issues.version
    expansion = yield
    unless expansion.is_a?(Term::Rep)
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

    # Marks the beginning of an expansion of an Alloy node, e.g. `^match`.
    # The keypath is reset below expansion spots, because if it was not,
    # it would no longer point to a valid location in the template.
    record Expansion, detail : String, offspring : Term do
      include Issue::Spot
    end
  end
end

require "./alloy/render"
require "./alloy/compose"
