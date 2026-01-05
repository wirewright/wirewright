# Wirewright M1 is a pattern matching and backmapping engine, along with a suite
# of related tools.
#
# Metaphorically speaking, M1 is the sensory organ of Wirewright. All data Wirewright
# knows (and cares) about is represented with `Term`s, which we consider the "matter".
#
# A network packet, a user event -- all that does not really exist for Wirewright.
# Rather, there are dedicated subsystems (e.g. the window manager) that "materialize"
# them -- as in make them into "matter", into Wirewright-comprehensible `Term`s.
#
# Wirewright and most of its subsystems (e.g. `Alloy`, `DwUIR`, uiR) then require
# a sensory organ to perceive parts of such matter. The backmapping engine, on the other
# hand, is used for "actuation" -- to modify matter in response to perception.
#
# The processes of pattern matching and backmapping -- perception and actuation --
# are so tighly linked and Wirewright that we group them under one subsystem, M1.
#
# Underlying M1's pattern matching algorithm is a variation on backtracking search
# with lightweight constraints (mostly equality constraints). The core idea is that
# the pattern is "linearized" on the go (a bit like a train laying tracks in front
# of itself); the current "match-point" (most likely an `Operator`) then asks
# the rest of the pattern whether it approves whatever choice it makes. The current
# "match-point" can do this any number of times; it can fork and collect answers,
# or AND by introducing match-points further ahead, and so on.
#
# I suspect M1's pattern matching algorithm is NP in the worst case, although this
# remains to be proven. For practical patterns NP, if it's there, would be very hard --
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
# a bit ugly and hard to pronounce, has long of history in Wirewright. It is
# therefore the preferred way of referring to the term being matched.
module Ww::M1next
  extend self

  alias O = M1::Operator

  # Returns `true` if *op* definitely matches *op*. Uses *env* as the prototype
  # match env.
  #
  # ```
  # op = M1.operator(ML.term(%{(+ a_ b_)}))
  #
  # M1::Operator.probe?(Term[], op, matchee: Term.of(:+, 1, 2)) # => true
  # M1::Operator.probe?(Term[], op, matchee: Term.of(:qux))     # => false
  # ```
  def probe?(env : Term::Dict, op : O::Any, matchee : Term) : Bool
    match(env, op, matchee, &.present?)
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
  def matches(env : Term::Dict, op : O::Any, matchee : Term) : Slice(Term::Dict)
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

  alias LogRef = Log::SealedOne | Ref::One

  # Each entry in the log list maps capture name to its corresponding sealed
  # log, or ref name to its corresponding `Ref::One`.
  #
  # Whenever a capture or ref has multiple logs associated with it, you'll see
  # multiple entries with the same capture name or ref name.
  alias LogList = Slice({Term, LogRef})

  # Returns a read-only slice of match envs paired with their corresponding
  # log lists.
  #
  # This function is the point of contact between the pattern matching part
  # of M1 and its backmap part. What this function returns is exactly what
  # a backmap engine needs as input.
  #
  # Pairs in the returned slice are sorted using `Term.compare` on envs, to
  # avoid having their order be implementation-defined.
  def matches_and_logs(env : Term::Dict, op : O::Any, matchee : Term) : Slice({Term::Dict, LogList})
    match(env, op, matchee, log: true) do |fb|
      buffer = Pf::Kit.stack_array({Term::Dict, LogList}, 16)

      fb.each do |response|
        logs = Pf::Kit.stack_array({Term, LogRef}, 8)

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
          Ref.flatten(ref) do |one|
            logs << {key, one}
          end
        end

        buffer << {env, logs.to_readonly_slice(&.itself)}
      end

      # Sort buffer by envs so that the order is definite.
      buffer.sort! { |(a, _), (b, _)| Term.compare(a, b) }
      buffer.to_readonly_slice(&.itself)
    end
  end
end

require "./m1/log"
require "./m1/tzip"
require "./m1/context"
require "./m1/match"
