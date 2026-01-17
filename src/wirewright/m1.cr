# Wirewright M1 is a pattern matching and backmapping engine, along with a suite
# of related tools.
#
# Metaphorically speaking, M1 is the sensory organ of Wirewright. All data Wirewright
# knows (and cares) about is represented with `Term`s, which we consider the "matter".
#
# A network packet, a user event -- all that does not really exist for Wirewright.
# Rather, there are dedicated subsystems (e.g. the window manager) that "materialize"
# them -- as in turn them into "matter", into Wirewright-comprehensible `Term`s.
#
# Wirewright and most of its subsystems (e.g. `Alloy`, `DwUIR`, uiR) then require
# a sensory organ to perceive parts of such matter. The backmapping engine, on the other
# hand, is used for "actuation" -- to modify matter in response to perception.
#
# The processes of pattern matching and backmapping -- perception and actuation --
# are so tighly coupled in Wirewright that we group them under one subsystem, M1.
#
# Underlying M1's pattern matching algorithm is a variation on backtracking search
# with lightweight constraints (mostly equality constraints). The core idea is that
# the pattern is "linearized" on the go (a bit like a train laying tracks in front
# of itself); the current "match-point" (most likely an `Op`) then asks the rest
# of the pattern whether they approves whatever choice the operator wants to
# makes. The current "match-point" can do this any number of times; it can fork
# and collect answers, AND them by introducing match-points further ahead, and so on.
#
# I suspect M1's pattern matching algorithm is NP in the worst case, although this
# remains to be proven. For practical patterns, NP, if it's there, would be very hard --
# if possible at all -- to hit. Most practical patterns have optimized (possibly sub-
# microsecond) fast paths. The general expectation is that most patterns *match*
# in <10 microseconds. These numbers vary with machine and environment, of course,
# as well as with the underlying term. But I'm just giving some rough estimates here.
# The simplest patterns match in <1 microsecond, possibly in <500ns.
#
# I am emphasizing *match* because in fact, M1 is optimized for fast mismatches
# (negatives, rejections), and matches (positives) are sometimes pessimized. When
# you do backtracking search, rule search, etc., rejections are vastly more frequent
# than matches. Sometimes, for a thousand rules and a term, only a single rule or
# no rules match. M1 tries to take every opportunity at skipping work, which may
# make a genuine match slower, because of all the "tricky questions" the term would
# have to answer first. But then, those "tricky questions" helped filter out maybe 90%
# of other candidates, so overall time savings can be enormous. Most rejections in
# practice are expected to be well under 100ns.
#
# Refer to `m1.operator` in the doctool to read the docs for M1 operators.
#
# M1 uses the terminology of *pattern* (a description of what should be matched),
# *matchee* (the term being matched -- the current candidate term), and *match
# environment* (more commonly referred to as *match env* or simply *env*).
#
# Note that what Wirewright/M1 calls *matchee* is more correctly referred to
# as *scrutinee* (see e.g. [Wikipedia, Pattern matching, Terminology of patterns]
# (https://en.wikipedia.org/wiki/Pattern_matching#Terminology_of_patterns)).
# I find *scrutinee* very hard to type, however, and the word *matchee*, although
# a bit ugly and hard to pronounce, has a long history in Wirewright. It is
# therefore the preferred way of referring to the term being matched.
#
# NOTE: With M1, there's lots of "magic" involved, and even I can barely
# comprehend how it all works together (the parts are quite simple, however,
# almost trivial -- that's kind of the point with this design). I try to leave
# comments where appropriate -- meaning almost everywhere! -- so expect lots
# of them in the code.
module Ww::M1next
  extend self

  # Returns `true` if *op* probably matches *matchee*. Returns `false` if *op*
  # definitely does not match *matchee*.
  #
  # In other words, this function can give false positives but it will never
  # give false negatives.
  #
  # This function is an extremely valuable asset for "defending" or "guarding"
  # expensive patterns (matched via e.g. `matches` or even `probe?`) -- but
  # especially, backmaps (e.g. `backmap`).
  #
  # Most matches are rejections in practice. This function tries to minimize
  # work while maximizing rejections that it's responsible for, so that the vastly
  # more expensive `match` or `backmap` downstream isn't triggered without
  # due cause.
  #
  # Whereas `matches` employs backtracking search, bits of constraint satisfaction
  # and so on, `probably_matches?` is a simple hierarchical matcher that walks *op*
  # as a tree. Think of it as your normal AST interpreter `eval` except the result
  # is `true` or `false` instead of a value, and it can be a false positive.
  #
  # NOTE: M1 won't call `probably_matches?` for you, you'll have to do that yourself.
  # You always have more knowledge than M1, so you can choose whether and where
  # to call it for best performance.
  def probably_matches?(op : Op::Any, matchee : Term) : Bool
    # The overloads are in m1/match.cr.
    true
  end

  # Convenience function that calls `probably_matches?(M1::Operator, Term)` after
  # compiling *pattern* for you.
  #
  # Routes *kwargs* to `M1.operator`.
  #
  # ```
  # M1.probably_matches?(ML.term(%{(+ a_ b_)}), Term.of(:+, 1, 2))
  # # => true (truth)
  #
  # M1.probably_matches?(ML.term(%{(+ a_ a_)}), Term.of(:+, 1, 2))
  # # => true (false positive)
  #
  # M1.probably_matches?(ML.term(%{(+ a_ a_)}), Term.of(:-, 1, 2))
  # # => false (truth)
  # ```
  def probably_matches?(pattern : Term, matchee : Term, **kwargs) : Bool
    probably_matches?(M1.operator(pattern, **kwargs), matchee)
  end

  # Returns `true` if *op* definitely matches *matchee*. Uses *env* as the prototype
  # match env.
  def probe?(env : Term::Dict, op : Op::Any, matchee : Term) : Bool
    match(env, op, matchee, &.present?)
  end

  # Convenience function that calls `probe?(Term::Dict, M1::Operator, Term)` after
  # compiling *pattern* for you.
  #
  # Routes *kwargs* to `M1.operator`.
  #
  # ```
  # M1.probe?(ML.term(%{(+ a_ b_)}), Term.of(:+, 1, 2))
  # # => true (truth)
  #
  # M1.probe?(ML.term(%{(+ a_ a_)}), Term.of(:+, 1, 2))
  # # => false (truth)
  #
  # M1.probe?(ML.term(%{(+ a_ a_)}), Term.of(:-, 1, 2))
  # # => false (truth)
  # ```
  def probe?(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Bool
    probe?(env, M1.operator(pattern, **kwargs), matchee)
  end

  # Returns one of match envs after matching *op* against *matchee*. Returns `nil` if
  # *op* does not match *mathee*.
  #
  # NOTE: "One of" is not the same as "first" nor "last" nor "middle". It's
  # implementation-defined and depends on both *matchee* and *op*. You should
  # use this function only if you're fine with that (e.g. because you control
  # the pattern). Otherwise, just do `matches` and take the first match
  # (`matches` gives you lexicographic order).
  def match?(env : Term::Dict, op : Op::Any, matchee : Term) : Term::Dict?
    match(env, op, matchee) do |fb|
      fb.each do |response|
        env = Term::Dict.build do |commit|
          response.envtab.each do |key, value|
            commit.with(key, value.term)
          end
        end

        return env
      end
    end
  end

  # Convenience function that calls `match?(M1::Operator, Term)` after
  # compiling *pattern* for you.
  #
  # Routes *kwargs* to `M1.operator`.
  def match?(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Term::Dict?
    match?(env, M1.operator(pattern, **kwargs), matchee)
  end

  # Returns a read-only slice of match envs after showing *matchee* to *op*.
  # Uses *env* as the prototype match env. Empty slice means mismatch. Envs
  # in the slice are sorted using `Term.compare` to avoid having their order
  # be implementation-defined.
  #
  # ```
  # op = M1.operator(ML.term(%{⟨±n⟩°}))
  # matchee = Term.of(3, 5, 1, :x, :z, 100)
  #
  # M1::Operator.matches(Term[], op, matchee)
  # # => Slice[Term[n: 1], Term[n: 3], Term[n: 5], Term[n: 100]]
  # ```
  def matches(env : Term::Dict, op : Op::Any, matchee : Term) : Slice(Term::Dict)
    match(env, op, matchee) do |fb|
      buffer = Pf::Kit.stack_array(Term::Dict, 16)

      fb.each do |response|
        env = Term::Dict.build do |commit|
          response.envtab.each do |key, value|
            commit.with(key, value.term)
          end
        end

        buffer << env
      end

      buffer.sort! { |a, b| Term.compare(a, b) }
      buffer.to_readonly_slice(&.itself)
    end
  end

  # Convenience function that calls `matches(M1::Operator, Term)` after compiling
  # *pattern* for you.
  #
  # Routes *kwargs* to `M1.operator`.
  def matches(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Slice(Term::Dict)
    matches(env, M1.operator(pattern, **kwargs), matchee)
  end

  # Each entry in the log list maps a name to its corresponding sealed log. The log
  # may not point to an existing term in the matchee, as is the case for logs emitted
  # by so-called *refs*. For example, `{¦ ⏏-x_⏏}`, which checks for the *absence* of
  # an entry with the key *x* but produces a log to it anyway. On the other hand,
  # *captures* always produce logs that exist in the matchee.
  #
  # Whenever a capture has multiple logs associated with it, you'll see multiple
  # entries with the same name.
  alias LogList = Slice({Term, Log::SealedOne})

  # :nodoc:
  def matches_and_logs(env : Term::Dict, op : Op::Any, matchee : Term, &)
    match(env, op, matchee, log: true) do |fb|
      buffer = Pf::Kit.stack_array({Term::Dict, LogList}, 16)

      fb.each do |response|
        logs = Pf::Kit.stack_array({Term, Log::SealedOne}, 8)

        # Add captures and paths to them (if any).
        env = Term::Dict.build do |commit|
          response.envtab.each do |key, value|
            commit.with(key, value.term)

            Log.flatten(Log.simplify(value.log)) do |log|
              logs << {key, Log.seal(log)}
            end
          end
        end

        # Add refs (roughly speaking, named paths not associated with any
        # particular capture).
        response.reftab.each do |key, ref|
          Log.flatten(ref) do |ref_one|
            logs << {key, ref_one.as(Log::SealedOne)}
          end
        end

        buffer << {env, logs.to_readonly_slice(&.itself)}
      end

      yield buffer
    end
  end

  # A list of pairs, where each pair consists of an environment dict, and a list
  # of logs associated with that environment dict.
  alias EnvLogList = Slice({Term::Dict, LogList})

  # Returns a read-only slice of match envs paired with their corresponding
  # log lists.
  #
  # This function is the point of contact between the pattern matching part
  # of M1 and its backmap part. What this function returns is exactly what
  # a backmap engine needs as input.
  #
  # Pairs in the returned slice are sorted using `Term.compare` on envs, to
  # avoid having their order be implementation-defined.
  def matches_and_logs(env : Term::Dict, op : Op::Any, matchee : Term) : EnvLogList
    matches_and_logs(env, op, matchee) do |buffer|
      # Sort buffer by envs so that the order is definite.
      buffer.sort! { |(a, _), (b, _)| Term.compare(a, b) }
      buffer.to_readonly_slice(&.itself)
    end
  end

  # Runs the backmap engine on the given backsystem *backsys* and *matchee*.
  #
  # This overload lets you specify the backsystem as a list of associations between
  # an env log list (as emitted e.g. by `matches_and_logs`) and backspecs. Returns
  # the resulting replacement (see `Rep`).
  #
  # WARNING: You are not advised to use this overload because it relies without any
  # checks on the fact that match log lists in *backsys* really are pointing into
  # *matchee*. If they are not, the behavior of this function is not specified (not
  # in the UB sense, but in that it may or may not raise depending on how much logs
  # from *backsys* and *matchee* overlap).
  def backmapR(backsys : Enumerable({EnvLogList, Term::Dict}), matchee : Term) : Rep::Any
    agents = Pf::Kit.stack_array(Backmap::Agent, 8)

    backsys.each do |matches, backspec|
      next if matches.empty?

      agents << Backmap::Agent.new(matches, backspec)
    end

    Backmap.backmap(agents, matchee)
  end

  # Runs the backmap engine on the given backsystem *backsys* and *matchee*.
  #
  # This overload lets you specify the backsystem as a list of associations
  # between an operator and a backspec, and handles matching & selection of
  # matching operators for you. *env* is passed to the matching process as
  # the seed env.
  #
  # Returns the resulting replacement (see `Rep`). Returns `nil` if *none*
  # of the operators matched *matchee*.
  def backmapR?(backsys : Enumerable({Op::Any, Term::Dict}), matchee : Term, *, env : Term::Dict = Term[]) : Rep::Any?
    agents = Pf::Kit.stack_array(Backmap::Agent(EnvLogList), 8)

    backsys.each do |op, backspec|
      matches = matches_and_logs(env, op, matchee)
      next if matches.empty?

      agents << Backmap::Agent.new(matches, backspec)
    end

    return if agents.empty?

    Backmap.backmap(agents, matchee)
  end

  # Runs the backmap engine on the given backsystem *backsys* and *matchee*.
  #
  # This overload lets you specify the backsystem as a list of associations
  # between a pattern term and a backspec term. Patterns are compiled using
  # `M1.operator` (*kwargs* are routed to it), and matched (*env* is used as
  # seed env).
  #
  # Non-dict backspecs count as mismatch and are ignored. Mismatches are
  # filtered out.
  #
  # Returns the resulting replacement (see `Rep`), or `nil` if *none* of patterns
  # in *backspec* matched *matchee*.
  def backmapR?(backsys : Enumerable({Term, Term}), matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Rep::Any?
    ops = Pf::Kit.stack_array({Op::Any, Term::Dict}, 8)

    backsys.each do |pattern, backspec|
      next unless backspec = backspec.as_d?

      ops << {M1.operator(pattern, **kwargs), backspec}
    end

    backmapR?(ops, matchee, env: env)
  end

  # An optimized overload of `backmapR?` for running just one backmap rather than
  # a backsystem. The backmap is specified by providing its operator *op*
  # and *backspec*.
  def backmapR?(op : Op::Any, backspec : Term, matchee : Term, *, env : Term::Dict = Term[]) : Rep::Any?
    return unless backspec = backspec.as_d?

    matches_and_logs(env, op, matchee) do |matches|
      return if matches.empty?

      agent = Backmap::Agent.new(matches, backspec)
      agents = Slice.new(pointerof(agent), size: 1, read_only: true)
      Backmap.backmap(agents, matchee)
    end
  end

  # An optimized overload of `backmapR?` for running just one backmap rather than
  # a backsystem.
  #
  # The backmap is specified by providing its pattern term *pattern* and
  # a *backspec* term. Non-dict backspecs count as a mismatch.
  #
  # *kwargs* are routed to `M1.operator`.
  #
  # ```
  # pattern = ML.term(%{ g←(a_ b_) })
  # backspec = ML.term(%{ {a: ^b, b: ^a, (g): (^(up g) ^(up g))} })
  #
  # result = M1.backmapR?(pattern, backspec, ML.term(%{ (100 200) }))
  # pp result # => Rep::Many(@terms=Slice[(200 100), (200 100)])
  #
  # result = M1.backmapR?(pattern, backspec, ML.term(%{ qux }))
  # pp result # => nil
  # ```
  def backmapR?(pattern : Term, backspec : Term, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Rep::Any?
    backmapR?(M1.operator(pattern, **kwargs), backspec, matchee, env: env)
  end

  # Same as `backmapR?`, but collapses the resulting replacement to a term
  # using `Rep.collapse`.
  #
  # ```
  # backsys = Slice[
  #   {ML.term(%{ (a_ _) }), ML.term(%{ {a: ^(+ a 1)} })},
  #   {ML.term(%{ (_ b_) }), ML.term(%{ {b: ^(* b 2)} })},
  # ]
  #
  # result = M1.backmap?(backsys, ML.term(%{ (100 200) }))
  # pp result # => (101 400)
  #
  # result = M1.backmap?(pattern, backspec, ML.term(%{ qux }))
  # pp result # => nil
  # ```
  def backmap?(backsys, matchee : Term, **kwargs) : Term?
    return unless rep = backmapR?(backsys, matchee, **kwargs)

    Rep.collapse(rep)
  end

  # Same as `backmapR?`, but collapses the resulting replacement to a term
  # using `Rep.collapse`.
  #
  # ```
  # pattern = ML.term(%{ (a_ b_) })
  # backspec = ML.term(%{ {a: ^b, b: ^a} })
  #
  # result = M1.backmap?(pattern, backspec, ML.term(%{ (100 200) }))
  # pp result # => (200 100)
  #
  # result = M1.backmap?(pattern, backspec, ML.term(%{ qux }))
  # pp result # => nil
  # ```
  def backmap?(pattern, backspec : Term, matchee : Term, **kwargs) : Term?
    return unless rep = backmapR?(pattern, backspec, matchee, **kwargs)

    Rep.collapse(rep)
  end

  # Same as `backmap?`, but returns *matchee* on mismatch.
  #
  # ```
  # backsys = Slice[
  #   {ML.term(%{ (a_ _) }), ML.term(%{ {a: ^(+ a 1)} })},
  #   {ML.term(%{ (_ b_) }), ML.term(%{ {b: ^(* b 2)} })},
  # ]
  #
  # result = M1.backmap(backsys, ML.term(%{ (100 200) }))
  # pp result # => (101 400)
  #
  # result = M1.backmap(pattern, backspec, ML.term(%{ qux }))
  # pp result # => qux
  # ```
  def backmap(backsys, matchee : Term, **kwargs) : Term
    backmap?(backsys, matchee) || matchee
  end

  # Same as `backmap?`, but returns *matchee* on mismatch.
  #
  # ```
  # pattern = ML.term(%{ (a_ b_) })
  # backspec = ML.term(%{ {a: ^b, b: ^a} })
  #
  # result = M1.backmap(pattern, backspec, ML.term(%{ (100 200) }))
  # pp result # => (200 100)
  #
  # result = M1.backmap(pattern, backspec, ML.term(%{ qux }))
  # pp result # => qux
  # ```
  def backmap(pattern, backspec : Term, matchee : Term, **kwargs) : Term
    backmap?(pattern, backspec, matchee) || matchee
  end
end

require "./m1/log"
require "./m1/tzip"
require "./m1/context"
require "./m1/op"
require "./m1/match"
require "./m1/backmap"
