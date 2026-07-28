# |@ m1
#
# |@summary
# A pattern matching and backmapping engine.
#
# |@block
# M1 is a pattern matching and backmapping engine, and a suite of related tools.
#
# Metaphorically speaking, M1 is the sensory organ of Wirewright. All data Wirewright
# knows (and cares) about is represented with `Term`s, which we consider "matter".
#
# A network packet, a user event, a file -- all that does not really exist for Wirewright.
# Instead, there are dedicated subsystems (e.g. the window manager) that "materialize"
# them -- as in, turn them into "matter" -- Wirewright-comprehensible `Term`s.
#
# Wirewright and most of its subsystems (e.g. `Alloy`, `DwUIR`, uiR) then require
# a sensory organ to perceive parts of such matter and its various arrangements.
#
# The backmapping engine, on the other hand, is used for "actuation". In other words,
# it is used to manipulate matter *in response to perception*.
#
# The processes of pattern matching and backmapping -- perception and actuation --
# are so tighly coupled in Wirewright that we group them under one subsystem, M1.
#
# Underlying M1's pattern matching algorithm is a variation on backtracking search
# with lightweight constraints (mainly equality constraints). The core idea is that
# the pattern is "linearized" on the go (a bit like a train laying tracks in front
# of itself); the current "match-point" (most likely an `Op`) then asks the rest
# of the pattern whether they approves whatever choice the operator wants to
# makes. The current "match-point" can do this any number of times; it can fork
# and collect answers, AND by scheduling match-points of itself with advanced state
# in the future, and so on.
#
# I suspect M1's pattern matching algorithm is NP in the worst case, although this
# remains to be proven. For practical patterns, NP, if it's there, would be very hard --
# if possible -- to hit. Most practical patterns have optimized (possibly sub-
# microsecond) fast paths.
#
# The general expectation is that most patterns *match* in under 10 microseconds. This
# number varies with machine and environment, of course, and with the underlying term.
# But I'm just giving some very rough estimates here. The simplest patterns match in under
# 1 microsecond, and if you're lucky in under 500ns. At this point we're hitting very
# close to Dict performance.
#
# I am emphasizing *match* because M1 is actually optimized for fast *mismatches*
# (negatives, rejections). In fact, matches (positives) are sometimes pessimized.
#
# When you do backtracking search, rule search, etc., rejections are vastly more frequent
# than matches. Sometimes, for a thousand rules and a term, only a single rule or
# no rules match. With that in mind, M1 tries to take every opportunity at skipping
# work. This may make a genuine match slower, because the matchee must answer a lot
# of "tricky questions" first, before it gets to the "meat" of the matching algorithm.
#
# But then, if the "tricky questions" help filter out maybe 90% of candidates that do
# not match anyway, the overall time savings are expected to be enormous. So most
# rejections in practice are expected to be well under 100ns.
#
# Again, the main thing to keep in mind is M1 is an extremely stupid backtracking
# search "in disguise" of something much smarter. A stupid backtracking search is
# expected to generate a ton of wrong answers before it gets something right. Thus,
# again, figuratively speaking, we have 99% rejections; that's what we optimize for.
#
# Even then, do expect slow-downs. Although M1 is what I would consider well-tested,
# it is still not extensively tested; and the kinds of patterns that would cause
# bad performance are yet to be discovered, since, you know, I don't find myself
# writing bad patterns very often... Anyway. I suspect the cases where M1 struggles
# can be more or less trivially moved to rewriters or D7/Rack (for example deep search).
#
# Refer to `m1.operator` in the doctool to learn about the available M1 operators.
#
# M1 uses the terminology of *pattern* (roughly, a description of what should be matched),
# *matchee* (the term being matched -- the current candidate term), and *match
# environment* (more commonly referred to as *match env* or simply *env*).
#
# Note that what Wirewright/M1 calls *matchee* is more correctly called *scrutinee*
# (see e.g. [Wikipedia, Pattern matching, Terminology of patterns](https://en.wikipedia.org/wiki/Pattern_matching#Terminology_of_patterns)).
# However, I have a hard time typing the word *scrutinee*. Additionally, the word *matchee*,
# although a bit ugly and hard to pronounce, has a long history in Wirewright. *Matchee*
# is therefore the preferred way of referring to the term being matched.
#
# NOTE: With M1, there's lots of "magic" involved, and even I can barely
# articulate how the thing works together (the parts are quite simple, however,
# almost trivial -- that's kind of the point with this design). I try to leave
# comments where appropriate -- meaning almost everywhere! -- so expect lots
# of them in the code.
#
# ### On invalid patterns
#
# If someone has trouble understanding a metaphor or a joke -- grasping its
# intended meaning -- they do not "crash" or "raise an exception". They change
# their perspective and interpret the metaphor or joke more literally. This works
# like a spectrum: from close-to-the-intended meaning to letter-by-letter or
# sound-by-sound. The latter is true for foreign languages: we can't recognize
# the meaning, but we can still hear the sounds.
#
# Similarly, if the pattern engine cannot recognize the intended meaning of some
# pattern term, it will simply go "one level of meaning down" and interpret the term
# more literally; regardless of the amounts of confusion this creates (like in
# the real world with metaphors). In some sense, invalid patterns "fail to fold";
# an "emergent entity" is unable to appear, and we're left with the disorganized pieces.
# `(%literal 100)`, interpreted as an "emergent" literal operator, becomes just a list
# of symbols and numbers if we say `(%literal 100 200)`.
#
# Unfortunately, yes, this will inevitably cause problems at some point; and even bugs.
# This can be fixed, however, by diagnostics during normalization. We plan on adding those.
#
# For M1, any pattern, even an invalid one from the human point-of view, is meaningful.
# This is a hard rule. There must be no such thing as a "pattern matching engine crash"
# (minus the inevitable implementation errors on my end).
module Ww::M1
  extend self

  # :nodoc:
  module Token
    @dict : Term::Dict

    def_copy_with

    def map(& : Term::Dict -> Term::Dict)
      copy_with(dict: yield @dict)
    end

    def unwrap(& : Term::Dict ->)
      yield @dict
    end
  end

  # Wraps a normal pattern term. `Normp` acts as a proof that you really did
  # call `normal`: the only way to construct `Normp`s is through `normal(Term)`.
  struct Normp
    include Token

    @[Flags]
    enum Annotation
      Depths
      Bounds
      Literals
      Sketches
      Captures
      Keys
    end

    getter annotations : Annotation

    # :nodoc:
    def initialize(@dict, @annotations)
    end

    # :nodoc:
    def with_annotation(ann : Annotation)
      copy_with(annotations: @annotations | ann)
    end
  end

  # Wraps a guarded normal pattern term .`Guardedp` acts as a proof that you really
  # did call `guard`. Notably, either with or without optimization passes that are
  # necessary to produce the guards. So for instance, `optimal(_, O0)` just calls
  # `guard` without doing the passes, whereas `O1` and above do the passes first.
  struct Guardedp
    include Token

    # :nodoc:
    def initialize(@dict : Term::Dict)
    end
  end

  {% for cls in {Normp, Guardedp} %}
    # See `Kit#ascend`.
    def ascend(pattern : {{cls}}, &fn : {{cls}} -> {{cls}}) : {{cls}}
      pattern.unwrap do |op|
        Kit.ascend(op) do |member|
          fn.call(pattern.map { member })
        end
      end
    end

    # See `Kit#walk`.
    def walk(pattern : {{cls}}, &fn : {{cls}} ->) : Nil
      pattern.unwrap do |op|
        Kit.walk(op) do |member|
          fn.call(pattern.map { member })
        end
      end
    end

    # See `Kit#member`.
    def each_member(pattern : {{cls}}, & : {{cls}} ->) : Nil
      pattern.unwrap do |op|
        Kit.each_member(op) do |member|
          yield pattern.map { member }
        end
      end
    end

    # See `Kit#members`.
    def members(pattern : {{cls}}) : Array({{cls}})
      pattern.unwrap { |op| Kit.members(op) }
    end
  {% end %}

  # Returns the normal form of an M1 *pattern*.
  #
  # - The normal form consists of operators. Each operator is a dictionary whose
  #   first item is a symbol prefixed with `%`. For example, `100` becomes `(%literal 100)`.
  # - All operators in the normal form are dictionaries. Among other things,
  #   this lets us annotate normal operators arbitrarily, both from the child's
  #   and parent's end.
  # - Operators such as `(%let foo _)` or `(%item a_ b_ c_)` accept other operators
  #   (here, `_`; `a_`, `b_`, `c_`, correspondingly). In the normal form, such argument
  #   operators are generally referred to as *members* if there is a possibility for
  #   zero (or one) or more of them: e.g., *members* of `%item`; or *successor* if
  #   only one such operator is allowed (e.g., *successor* of `%let`).
  # - `(%capture name_)` marks captures. For example, a `%let` normalizes
  #   into `(%let (%capture name) value)`. Captures can have tags, which are
  #   subsequently picked up by `capturesp`: `(%capture foo tags: (a b c))`.
  # - `(%ref name_)` marks references. References are different from captures in that
  #   they do not have associated values at match-time. References are used
  #   primarily in backmaps. For example, a slot normalizes into `(%slot (%ref name))`.
  # - Item sequence (`%seq`) nodes are marked with `seq: true`.
  # - Entry nodes in `%layer` are marked with `entry: true`.
  # - `(%key name_)` marks dictionary keys expected in the current context.
  # - `(%payload term_)` marks a term that is part of an operator, must not be evaluated
  #   (i.e., marks it as a "non-member").
  # - `(%literal term_)` nodes represent literal (exact) matches and must not be visited.
  # - `sealed: true` on an operator means the operator and all its members must be
  #   evaluated in a context fully isolated from the outside one. In effect,
  #   `sealed: true` wraps the operator and its members in an impenetrable "membrane".
  #   For example, for sketches, this means the sketch of the outer pattern must not
  #   include (be union'd with) the sketch of the sealed operator and its members.
  # - `terminal: true` on an operator means the operator's items and pairs must
  #   not be evaluated.
  # - `guarded: true` marks operators that want to be guarded (e.g. at O1 with sketch checks,
  #   depth checks and so on).
  #
  # The normal form converts non-operators to operators ("normalizes" them; hence the name,
  # although at this point it is only kept for historical reasons & brevity).
  #
  # The normal form enriches the pattern with info related to optimization and reasoning
  # about the pattern. In general, the process of normalization is a bit like querying
  # a "generative fact base" about a pattern; plus validation. Its response is the normal
  # form of the pattern.
  #
  # As one of objectives in the design of the normal form, we want to have
  # structural encoding of facts about the pattern. This lets us rewrite
  # the pattern later on, at O2; annotate it with "features" such as its
  # sketch, population, type population, and so on, at O1; calculate the pattern's
  # specificity; and in general, reason about the pattern, and traverse it,
  # without each such stage having to know about all the operators supported
  # by M1.
  #
  # The normal form is an attempt to centralize both knowledge and docs for
  # all M1 operators.
  def normal(pattern : Term) : Normp
    Normp.new(normalize(Π.pattern(pattern)).as_d, :none)
  end

  alias Opt = O0.class | O1.class | O2.class | O2only.class

  # All optimizations are disabled.
  module O0
  end

  # Runs the following passes and their dependencies:
  #
  # - `depthp`
  # - `boundsp`
  # - `sketchp`
  #
  # ... finally feeding them to `guard`, which uses info obtained from
  # these passes to wrap `guarded: true` operators with guard operators
  # that do the depth/bounds/sketch check(s).
  #
  # These are key optimizations, they have major impact on performance. This
  # is because info from the leaves of the pattern propagates up, sometimes up
  # to the root. This reduces the work needed for rejections (mismatches), which
  # are much more frequent that matches in Wirewright (firstly, in general; and
  # second, because M1 uses backtracking search, which is very stupid most of
  # the times).
  module O1
  end

  # Runs `simplifyp` on the guarded normal pattern after `O1` until fixpoint.
  # Some simplifications are also made before `O1` (such as the `%split` ->
  # `%adjacent` rewrite), because `O1` can mess the tree up with guards.
  module O2
  end

  # O2 without O1.
  #
  # This level exists for testing purposes. You shouldn't use it in practice.
  module O2only
  end

  # Performs no optimizations.
  def optimal(pattern : Normp, level : O0.class) : Guardedp
    guard(pattern)
  end

  # Returns `O1`-optimized *pattern*.
  def optimal(pattern : Normp, level : O1.class) : Guardedp
    pipe(pattern, depthp, boundsp, literalp, keyp, sketchp, guard)
  end

  # Returns `O2`-optimized *pattern*.
  def optimal(pattern : Normp | Guardedp, level : O2.class)
    state0 = pattern

    loop do
      state1 = simplifyp(state0)
      if state0 == state1
        return state0
      end

      state0 = state1
    end
  end

  # Compiles the given *pattern*. Returns the resulting operator.
  def operator(pattern : Guardedp) : Op::Any
    pattern.unwrap do |op|
      compile(Π.pattern(Term.of(op)))
    end
  end

  # :nodoc:
  def operator(pattern : Normp, *, opt : O0.class) : Op::Any
    pipe(pattern, optimal(O0), operator)
  end

  # :nodoc:
  def operator(pattern : Normp, *, opt : O1.class) : Op::Any
    pipe(pattern, optimal(O1), operator)
  end

  # :nodoc:
  def operator(pattern : Normp, *, opt : O2.class) : Op::Any
    pipe(pattern, optimal(O2), optimal(O1), optimal(O2), operator)
  end

  # :nodoc:
  def operator(pattern : Normp, *, opt : O2only.class) : Op::Any
    pipe(pattern, optimal(O2), operator)
  end

  {% if flag?(:docs) %}
    # Compiles the given normal *pattern* after optimizing it with the given
    # optimization level *opt*.
    def operator(pattern : Normp, *, opt : Opt) : Op::Any
    end
  {% end %}

  # The default pattern optimization level.
  DEFAULT_OPT = O2

  # Compiles the given normal *pattern*.
  #
  # Uses the default optimization level (see `DEFAULT_OPT`).
  def operator(pattern : Normp) : Op::Any
    operator(pattern, opt: DEFAULT_OPT)
  end

  @@op_cache = SyncLRU(Term, Op::Any).new(capacity: 4096)

  # Compiles the given M1 *pattern* to an operator.
  #
  # This is the top-level function that calls the rest of the M1 pattern compiler
  # for you. This function performs normalization (`normal`), optimization
  # (`optimal`, *opt* sets the level), and finally the construction of an operator.
  #
  # Compilations are cached.
  def operator(pattern : Term, *, opt : Opt = DEFAULT_OPT) : Op::Any
    @@op_cache.put_if_absent(pattern) do
      operator(normal(pattern), opt: opt)
    end
  end

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
  # more expensive `matches` or `backmap` downstream isn't triggered without
  # due cause.
  #
  # Whereas `matches` employs backtracking search, bits of constraint satisfaction
  # and so on, `probably_matches?` is a simple hierarchical matcher that walks *op*
  # as a tree. Think of it as your normal AST interpreter `eval` except the result
  # is `true` or `false` instead of a value, and it can be a false positive.
  #
  # NOTE: M1 won't call `probably_matches?` for you at the top-level; you'll have
  # to do it yourself. You always have more knowledge than M1, so you can choose
  # whether and where to call it for best performance.
  def probably_matches?(op : Op::Any, matchee : Term) : Bool
    # The overloads are in m1/match.cr.
    true
  end

  # Convenience function that calls `probably_matches?(Op::Any, Term)` after
  # compiling *pattern* for you.
  #
  # Routes *kwargs* to `operator`.
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
    probably_matches?(operator(pattern, **kwargs), matchee)
  end

  # Returns `true` if *op* definitely matches *matchee*. Uses *env* as the prototype
  # match env.
  def probe?(env : Term::Dict, op : Op::Any, matchee : Term) : Bool
    match(env, op, matchee, &.present?)
  end

  # Convenience function that calls `probe?(Term::Dict, Op::Any, Term)` after
  # compiling *pattern* for you.
  #
  # Routes *kwargs* to `operator`.
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
    probe?(env, operator(pattern, **kwargs), matchee)
  end

  # Returns the first match env after matching *op* against *matchee*. Returns
  # `nil` if *op* does not match *mathee*. See also `matches` to learn about
  # env order.
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

  # Convenience function that calls `match?(Op::Any, Term)` after
  # compiling *pattern* for you.
  #
  # Routes *kwargs* to `operator`.
  def match?(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Term::Dict?
    match?(env, operator(pattern, **kwargs), matchee)
  end

  # Returns a read-only slice of match envs after showing *matchee* to *op*.
  # Uses *env* as the prototype match env. Empty slice means mismatch. Envs
  # in the slice are ordered according to source operators in the pattern,
  # but generally, *left-to-right*, both in terms of operator position in
  # the pattern, and in terms of how specific operators such as `%item°`
  # traverse structure.
  #
  # ```
  # op = M1.operator(ML.term(%{⟨±n⟩°}))
  # matchee = Term.of(3, 5, 1, :x, :z, 100)
  #
  # M1.matches(Term[], op, matchee)
  # # => Slice[Term[n: 3], Term[n: 5], Term[n: 1], Term[n: 100]]
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

      buffer.to_readonly_slice(&.itself)
    end
  end

  # Convenience function that calls `matches(Op::Any, Term)` after compiling
  # *pattern* for you.
  #
  # Routes *kwargs* to `operator`.
  def matches(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Slice(Term::Dict)
    matches(env, operator(pattern, **kwargs), matchee)
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

  private def matches_and_logs(env : Term::Dict, op : Op::Any, matchee : Term, &)
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
  # log lists. The order is as in `matches`.
  #
  # This function is the point of contact between the pattern matching part
  # of M1 and its backmap part. What this function returns is exactly what
  # a backmap engine needs as input.
  def matches_and_logs(env : Term::Dict, op : Op::Any, matchee : Term) : EnvLogList
    matches_and_logs(env, op, matchee, &.to_readonly_slice(&.itself))
  end

  # Runs the backmap engine on the given backsystem *backsys* and *matchee*.
  #
  # This overload lets you specify the backsystem as a list of associations between
  # an env log list (as emitted e.g. by `matches_and_logs`) and backspecs. Returns
  # the resulting replacement (see `Term::Rep`).
  #
  # WARNING: You are not advised to use this overload because it relies without any
  # checks on the fact that match log lists in *backsys* really are pointing into
  # *matchee*. If they are not, the behavior of this function is not specified (not
  # in the UB sense, but in that it may or may not raise depending on how much logs
  # from *backsys* and *matchee* overlap).
  def backmapR(backsys : Enumerable({EnvLogList, Term::Dict}), matchee : Term) : Term::Rep
    agents = Pf::Kit.stack_array(Backmap::Agent(EnvLogList), 8)

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
  # Returns the resulting replacement (see `Term::Rep`). Returns `nil` if *none*
  # of the operators matched *matchee*.
  def backmapR?(backsys : Enumerable({Op::Any, Term::Dict}), matchee : Term, *, env : Term::Dict = Term[]) : Term::Rep?
    agents = Pf::Kit.stack_array(Backmap::Agent(EnvLogList), 8)

    backsys.each do |op, backspec|
      next unless probably_matches?(op, matchee)

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
  # `operator` (*kwargs* are routed to it), and matched (*env* is used as
  # seed env).
  #
  # Non-dict backspecs count as mismatch and are ignored. Mismatches are
  # filtered out.
  #
  # Returns the resulting replacement (see `Term::Rep`), or `nil` if *none* of patterns
  # in *backspec* matched *matchee*.
  def backmapR?(backsys : Enumerable({Term, Term}), matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Term::Rep?
    ops = Pf::Kit.stack_array({Op::Any, Term::Dict}, 8)

    backsys.each do |pattern, backspec|
      next unless backspec = backspec.as_d?

      ops << {operator(pattern, **kwargs), backspec}
    end

    backmapR?(ops, matchee, env: env)
  end

  # An optimized overload of `backmapR?` for running just one backmap rather than
  # a backsystem. The backmap is specified by providing its operator *op*
  # and *backspec*.
  def backmapR?(op : Op::Any, backspec : Term, matchee : Term, *, env : Term::Dict = Term[]) : Term::Rep?
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
  # *kwargs* are routed to `operator`.
  #
  # ```
  # pattern = ML.term(%{ g←(a_ b_) })
  # backspec = ML.term(%{ {a: ^b, b: ^a, (g): (^(up g) ^(up g))} })
  #
  # result = M1.backmapR?(pattern, backspec, ML.term(%{ (100 200) }))
  # pp result # => Term::Rep[(200 100), (200 100)]
  #
  # result = M1.backmapR?(pattern, backspec, ML.term(%{ qux }))
  # pp result # => nil
  # ```
  def backmapR?(pattern : Term, backspec : Term, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Term::Rep?
    backmapR?(operator(pattern, **kwargs), backspec, matchee, env: env)
  end

  # Same as `backmapR?`, but collapses the resulting replacement to a term
  # using `Term.collapse`.
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

    Term.collapse(rep)
  end

  # Same as `backmapR?`, but collapses the resulting replacement to a term
  # using `Term.collapse`.
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

    Term.collapse(rep)
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
    backmap?(backsys, matchee, **kwargs) || matchee
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
    backmap?(pattern, backspec, matchee, **kwargs) || matchee
  end
end

require "./m1/log"
require "./m1/tzip"
require "./m1/context"
require "./m1/op"
require "./m1/production"
require "./m1/kit"
require "./m1/number_spec"
require "./m1/normalize"
require "./m1/range_calc"
require "./m1/o1"
require "./m1/o2"
require "./m1/compile"
require "./m1/head"
require "./m1/specificity"
require "./m1/match"
require "./m1/backmap"
require "./m1/pattern_set"
