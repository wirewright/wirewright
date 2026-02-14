# Wirewright Delta7 (D7 for short) is a rule search and application engine
# which backs Wirewright's approach to *symbolic physics*. D7 implements
# the machinery for *time-stepping* circuits. During a time-step, D7 finds
# which laws can apply, and where; and applies them. The laws themselves
# are provided in the form of *rewrite regimes*. Generic structure is
# *classified* into `Feature`s which D7 understands, and can traverse
# & search in.
#
# ### General architecture
#
# If D7 was a language frontend (it is not!), `ClassifierFactory`, `Classifier`,
# and `update` machinery would be the lexer.
#
# If D7 was a language frontend (and it is not!), `Regime` would be
# the parser, or, rather, a way to write parsers; a kind of parser combinator
# framework that works not across sequences of tokens (features), but
# across unstructured "bags" of them.
#
# The interpreter part, if D7 was a language *backend*, is in user-supplied
# bodies of rules. D7 does not define or restrict them in any way. Look for
# examples of rule bodies in `Rack`. The interpreter part benefits greatly
# from a number of helper functions, defined in d7/kit, namespaced directly
# under `D7` (such as `patch`, `patches`, `fetch`, and so on).
#
# One core difference that breaks this language analogy is D7 doesn't destroy
# information as it "lexes", "parses", and "evaluates". Any change applied during
# evaluation is merged back into the raw symbolic structure that D7 receives
# as input. In that sense, D7 is a bit like a "language" whose evaluation produces
# source code rather than "values", and those bits of source code it produces are
# plugged back in.
#
# ### Terminology
#
# A *rewrite regime* is like a "chemistry" of features. Each rewrite
# regime is a different kind of "chemistry".
#
# A *step* executes one tick of time for the rewrite regime. A step
# takes the *previous frame* as input and outputs the *next frame*.
# A *substep* is a partial frame. Time-stepping a frame F produces frame
# F+1. Time-stepping a partial frame F+0.5 is not guaranteed to produce
# the frame F+1. Substeps are mainly used for explanation and to trace
# why certain things ended up where they did. A *pass* is a reified
# time-step (see also: `Pass`).
#
# Multiple passes (time-steps) can be chained to compose a larger,
# *composite time-step*. The input of a composite time-step is still
# the *previous frame*, and the output is still the *next frame*.
# The intermediate frames (those each pass outputs which then are given
# to the next pass) are known as *subframes*.
#
# At the level of composite time-steps, we can identify three granularities:
# *coarse* (whereby a composite time-step produces exactly one frame,
# the *next frame*); *subframe* (whereby a composite time-step produces
# a sequence of subframes, the last of which is the next frame); and
# *fine* (whereby a composite time-step produces a sequence of substeps,
# the last of which is the next frame).
#
# ### More concretely
# Instead of building a general-purpose solver, which would necessarily be slow
# (if done naively; plus, matching subgraphs is NP-hard, see e.g. [Wikipedia, Subgraph isomorphism problem](https://en.wikipedia.org/wiki/Subgraph_isomorphism_problem)),
# I've identified the dominant rule shape in Rack through use. You can say we
# bootstrapped from a naive solver to something that reflects the *actual* problem
# well enough it's not NP. The rule shape looks like this:
#
# ```text
# PIVOT
#   -[edge0]-> dependency0
#   -[edge1]-> dependency1
#    .
#    .
#    .
#   -[edgeN]-> dependencyN
# ```
#
# Here, `dependency` matches zero or more adjacent nodes at that edge. All
# Rack nodes happen to conform to this shape: there is a "main" node, *the pivot*,
# or *the query node*; and it has some number of *dependencies*.
#
# One can imagine defining this structure recursively, but since each
# dependency matches zero-or-many, the definition would not be trivial,
# and is an unnecessary complication in practice.
#
# See `D7.case` for the actual DSL/usage. The DSL looks as similar to the diagram
# above as WwML can get.
#
# D7 is basically a high-performance matcher for rules that have the shape above.
# D7 does not support any other kind of rule shape -- specialization is a necessary
# compromise in getting rid of NP.
#
# The high-performance part is mostly a TODO at the moment, by the way. It's fast
# enough for this prototype stage; the problem is we lack heavy examples to benchmark
# it with & profile under.
module Ww::D7
  extend self

  # :nodoc:
  REGIMES = SyncHash(UInt32, Regime).new(initial_capacity: 32)

  # :nodoc:
  REGIME_ID = [0u32]

  # Constructs a D7 regime using a `Term.case`-like query DSL. Returns a proc that
  # should be passed as the block to `D7.step`.
  #
  # This macro is expected to be used the same way you use `Term.case`, i.e.,
  # you can use it anywhere at all, sparingly. The regimes are cached. Rule bodies
  # are blocks: using `return` in them returns out of the method or function
  # that contains the `D7.case`.
  #
  # Grammar:
  #
  # ```text
  # <query>
  #   <query pattern> <query name>
  #     -> <dep0>
  #     -> <dep1>
  #     .
  #     .
  #     .
  #     -> <depN>
  #
  # <dep>
  #   <link> <dependency pattern> <dependency options>
  #
  # <link>
  #   (one <edge capture in query pattern>)
  #     Follows an edge from a capture in the query pattern.
  #   (each <edge list capture in query pattern>
  #         <edge capture in dependency pattern>)
  #     Follows each edge from a list of edges captured by the query pattern.
  #
  # <query pattern>, <dependency pattern>
  #   <m1.operator>
  #
  # <dependency options>
  #   {;; Required: specifies the name of the `MatchGroup` which will be
  #    ;; populated with min to max `Match`es of this dependency.
  #    name: _symbol,
  #    ;; The minimum number of matches for this dependency (inclusive).
  #    min: (%optional 1 (%number +i32)),
  #    ;; The maximum number of matches for this dependency (inclusive).
  #    ;; Use `∞` for unlimited.
  #    max: (%optional 1 (%any° ∞ (%number +i32)))}
  # ```
  #
  # Example usage:
  #
  # ```
  # D7.case(clf, circuit) do
  #   #   use block args to "import" match groups from query
  #   #                 vvv  vvv  vvv
  #   rule(<<-WWML) do |dev, src, dst|
  #   [transfer inhibitors←((%past @_ min: 0)) srcs←((%past @_ min: 1)) pattern_ @dst_ template_] dev
  #     -> (each inhibitors inhibitor) [cell @inhibitor_ _] {name: inhibitor, min: 0, max: 0}
  #     -> (each srcs src) [cell @src_ value_] {name: src}
  #     -> (one dst) cell←[cell @dst_] {name: dst}
  #   WWML
  #     # dev : MatchGroup
  #     # src : MatchGroup
  #     # dst : MatchGroup
  #
  #     # ...
  #   end
  # end
  # ```
  macro case(clf, circuit, *, decorator = nil, &block)
    {%
      unless block
        raise "expected a block containing one or more `rule`s"
      end

      id = REGIME_ID[0]
      REGIME_ID[0] += 1

      stmts = block.body
      if stmts.is_a?(Expressions)
        stmts = stmts.expressions
      elsif stmts.is_a?(Nop)
        stmts = [] of ::NoReturn
      else
        stmts = [stmts]
      end

      branches = [] of ::NoReturn

      stmts.each do |stmt|
        unless stmt.is_a?(Call) && stmt.name == :rule && stmt.args.size >= 1 && stmt.block
          stmt.raise "regime: expected a call to `rule(*patterns : String, &)`"
        end

        stmt.args.each do |pattern|
          imports = stmt.block.args
          branches << {pattern: pattern, imports: imports, body: stmt.block.body}
        end
      end

      if branches.empty?
        block.raise "expected at least one `rule` branch"
      end
    %}

    %regime = {{@type}}::REGIMES.put_if_absent({{id}}) do
      %queries = Pointer({{@type}}::QueryIR).malloc({{branches.size}})
      {% for branch, index in branches %}
        %queries[{{index}}] = {{@type}}::QueryIR.parse(::Ww::ML.terms({{branch[:pattern]}}))
      {% end %}

      {{@type}}::Regime.new(Slice.new(%queries, {{branches.size}}, read_only: true))
    end

    %clf = {{clf}}

    {{@type}}.step(%clf, {{circuit}}) do |%hg|
      {% if decorator %}
      {{decorator}}(%hg) do |%hg|
      {% end %}
        %regime.solve(%hg) do |%match_table, %index|
          case %index
          {% for branch, index in branches %}
          when {{index}}
            %imports{index} = {
              {% for name in branch[:imports] %}
                {% if name.ends_with?("_tree") %}
                  {{@type}}.tree(%match_table, Term.of({{name[...-5].symbolize}})),
                {% else %}
                  {{@type}}.group(%match_table, Term.of({{name.symbolize}})),
                {% end %}
              {% end %}
            }

            %result{index} = pass(*%imports{index}) do |{{branch[:imports].splat}}|
              {{branch[:body]}}
            end

            %result{index}
          {% end %}
          else
            raise ArgumentError.new
          end
        {% if decorator %}
        end
        {% end %}
      end
    end
  end

  # A D7 pass takes a classifier and a circuit term (the previous *frame*),
  # and returns some number of *substeps*. The last substep is the next *frame*.
  #
  # See `D7` for general explanation & terminology.
  #
  # - The resulting slice is read-only.
  # - The resulting slice is guaranteed to contain at least one subframe.
  # - Substeps may repeat. Thus, the next frame may be equal to the previous frame.
  alias Pass = ClassifierFactory, Term -> Slice(Term)

  # A *classifier factory* constructs `Classifier` functions for particular
  # circuits, or if a circuit is not provided (represented as `nil`), returns
  # a general classifier, not tailored for a specific circuit.
  #
  # D7 itself does not define the difference between a circuit-specific and
  # a general classifier.
  #
  # For the sake of understanding, however, let's see how Rack uses this,
  # in particular `Rack.clf`.
  #
  # A circuit-specific classifier, in Rack, will find *components* defined
  # in the circuit; the resulting classifier function will, in turn, instantiate
  # those components as it classifies nodes. On the other hand, if you pass `nil`
  # to the factory returned by `Rack.clf`, you'll get a general classifier, which
  # doesn't know anything about components and won't instantiate them.
  alias ClassifierFactory = Term? -> Classifier

  private class CoarseFrameIterator
    include Iterator(Term)

    def initialize(@clf : ClassifierFactory, @circuit : Term, @passes : Indexable(Pass))
      @memo = @circuit
      @ahead = Deque{@circuit}
    end

    def next
      if circuit = @ahead.shift?
        return circuit
      end

      state = @circuit

      subframes = @passes.map do |pass|
        substeps = pass.call(@clf, state)
        state = substeps.last # Coarse
      end

      if @circuit == state
        return Iterator.stop
      end

      D7.fuse(@clf.call(nil), @memo, subframes) do |frame|
        next if @memo == frame

        @ahead << frame
        @memo = frame
      end

      @circuit = state
      @ahead.shift
    end
  end

  # Constructs an iterator for running a chain of *passes* in a single
  # step, fusing their frames coarsely (i.e., discarding prior subframes)
  # to obtain one or more "preview frames", which are subsequently produced
  # by the iterator.
  #
  # NOTE: Whether the iterator terminates depends on the given *circuit*. E.g.
  # if it oscillates, the iterator will not terminate.
  def coarse_frames(clf : ClassifierFactory, circuit : Term, passes : Indexable(Pass)) : Iterator(Term)
    CoarseFrameIterator.new(clf, circuit, passes)
  end

  # :ditto:
  def coarse_frames(clf : ClassifierFactory, circuit : Term, *passes : Pass) : Iterator(Term)
    coarse_frames(clf, circuit, passes)
  end
end

require "./d7/feature"
require "./d7/hypergraph"
require "./d7/kit"
require "./d7/regime"
require "./d7/step"
