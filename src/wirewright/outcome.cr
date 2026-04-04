module Ww
  # A *diagnostic* is an optional elaboration about an `Outcome::Accepted`.
  struct Diagnostic
    getter path : Slice(Spot)
    getter entity : Entity

    # :nodoc:
    def initialize(@path : Slice(Spot), @entity : Entity)
    end

    # Describes an object (entity) the evaluation process wants to highlight.
    module Entity
    end

    # Describes a step along the path to the object (entity) highlighted by
    # the evaluation process.
    module Spot
    end

    defrecord Text, detail : String, includes: {Entity, Spot}
    defrecord TermRef, detail : String, term : Term, includes: {Entity, Spot}
    defrecord KeyRef, key : Term, includes: {Spot}

    # Shorthand constructor for a bare string entity.
    def self.of(detail : String) : Entity
      Text.new(detail)
    end

    # Shorthand constructor for a `TermRef` entity.
    def self.of(detail : String, term : Term) : Entity
      TermRef.new(detail, term)
    end

    # Shorthand constructor for a bare string spot.
    def self.spot(detail : String) : Spot
      detail
    end

    # Shorthand constructor for a `TermRef` spot.
    def self.spot(detail : String, term : Term) : Spot
      TermRef.new(detail, term)
    end

    # Shorthand constructor for `KeyRef` spot.
    def self.key(object) : KeyRef
      KeyRef.new(Term.of(object))
    end
  end

  # Outcomes allow you to attach diagnostics to an object (`Accepted`), and supports
  # alternation (via `Rejected`).
  #
  # An outcome is not the same as a result, and `Rejected` is not the same as
  # an error. Instead, `Rejected` tells the caller to try the remaining alternatives;
  # and `Accepted` tells it that the callee processed the input, and the caller
  # should not try the remaining alternatives.
  #
  # Indeed, a result type (or something result-ish, such as a nilable type) can
  # be the payload for `Accepted`, as in `Accepted(Int32?)` or `Accepted(Term?)`.
  module Outcome
    extend self

    # A function, unit, agent, etc. evaluated the input successfully, providing zero
    # or more diagnostic messages alongside the result.
    defrecord Accepted(T), object : T, diagnostics : Slice(Diagnostic)

    # A function, unit, agent, etc. rejected the input without further elaboration:
    # it did not recognize the input in any meaningful way; the input "fell through".
    # The caller should try something else.
    defrecord Rejected

    def ok(object) : Accepted
      Accepted.new(object, diagnostics: Slice(Diagnostic).empty)
    end

    def ok_despite(object, *args) : Accepted
      Accepted.new(object, diagnostics: Slice[Diagnostic.new(Slice(Diagnostic::Spot).empty, Diagnostic.of(*args))])
    end

    def rej : Rejected
      Rejected.new
    end

    def at(key, outcome)
      elaborate(Diagnostic.key(key), outcome)
    end

    def unwrap?(outcome : Accepted)
      outcome.object
    end

    def unwrap?(outcome : Rejected)
    end

    def as_just_ok?(outcome : Accepted)
      outcome.diagnostics.empty? ? outcome.object : nil
    end

    def as_just_ok?(outcome : Rejected)
    end

    def fmap(outcome : Accepted, & : _ -> Accepted | Rejected)
      result = yield outcome.object
      unless result.is_a?(Accepted)
        return result
      end

      Accepted.new(result.object, outcome.diagnostics + result.diagnostics)
    end

    def fmap(outcome : Rejected, &)
      Rejected.new
    end

    macro fmap(outcome0, outcome1, *outcomes, &block)
      {{@type}}.fmap({{outcome0}}) do |%result|
        {{@type}}.fmap({{outcome1}}, {{outcomes.splat}}) do |*%results|
          pass(%result, *%results) {{block}}
        end
      end
    end

    macro choice(*branches)
      pass do
        {% for branch, index in branches %}
          if %var{index} = {{branch}}.as?({{@type}}::Accepted)
            next %var{index}
          end
        {% end %}

        {{@type}}.rej
      end
    end

    def elaborate(outcome : Accepted, & : -> Diagnostic::Spot)
      if outcome.diagnostics.empty?
        return outcome
      end

      spot = yield

      diagnostics = outcome.diagnostics.to_readonly_slice do |bt|
        elaborate(spot, bt)
      end

      Accepted.new(outcome.object, diagnostics)
    end

    def elaborate(outcome : Rejected, &)
      outcome
    end

    def elaborate(spot : Diagnostic::Spot, entity : Diagnostic::Entity) : Diagnostic
      Diagnostic.new(Slice[spot.as(Diagnostic::Spot)], entity)
    end

    def elaborate(spot : Diagnostic::Spot, diagnostic : Diagnostic) : Diagnostic
      Diagnostic.new(diagnostic.path.prepend(spot), diagnostic.entity)
    end

    def elaborate(spot : Diagnostic::Spot, outcome)
      elaborate(outcome) { spot }
    end

    struct Accumulator
      # :nodoc:
      def initialize(@diagnostics : Pf::Kit::HybridArray(Diagnostic, 16))
      end

      def <<(diagnostic : Diagnostic) : self
        @diagnostics << diagnostic

        self
      end

      def <<(entity : Diagnostic::Entity) : self
        @diagnostics << Diagnostic.new(Slice(Diagnostic::Spot).empty, entity)

        self
      end

      # Returns *outcome*'s object, accumulating diagnostics simultaneously.
      def unwrap(outcome : Accepted)
        @diagnostics.concat(outcome.diagnostics)

        outcome.object
      end

      def unwrap(outcome : Accepted, *, rej)
        unwrap(outcome)
      end

      def unwrap(outcome : Rejected, *, rej)
        rej
      end
    end

    # Lets you accumulate diagnostics.
    #
    # If *amend* is `true`, the accumulated diagnostics are discarded if the block
    # accpets `Accepted`. Otherwise, the block's diagnostics are merged with
    # the accumulated ones.
    def accumulate(*, amend : Bool = false, & : Accumulator -> Accepted | Rejected)
      diagnostics = Pf::Kit.stack_array(Diagnostic)

      result = yield Accumulator.new(diagnostics)
      if result.is_a?(Rejected)
        return result
      end

      if amend && result.diagnostics.empty?
        return result
      end

      diagnostics.concat(result.diagnostics)

      Accepted.new(result.object, diagnostics.to_unsafe_readonly_slice!)
    end
  end
end
