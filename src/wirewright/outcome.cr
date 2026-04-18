module Ww
  # A *diagnostic* is an optional elaboration about an `Outcome::Accepted`.
  struct Diagnostic
    getter path : Slice(Spot)
    getter entity : Entity

    # :nodoc:
    def initialize(@path : Slice(Spot), @entity : Entity)
    end

    # Describes an object (entity) an evaluation process wants to highlight.
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

  # Outcomes allow you to attach diagnostics to an object (`Accepted`), and support
  # alternation (via `Rejected`).
  #
  # An outcome is not the same as a result, and `Rejected` is not the same as
  # an error. Instead, `Rejected` tells the caller to try the remaining alternatives;
  # and `Accepted` tells it that the callee processed the input, and the caller
  # should not try the remaining alternatives.
  #
  # A result type (or something result-ish, such as a nilable type) is often
  # the payload of `Accepted` in practice, as in `Accepted(Int32?)`
  # or `Accepted(Term?)`.
  module Outcome
    extend self

    # A function, unit, agent, etc. evaluated the input successfully, providing zero
    # or more diagnostic messages alongside the result.
    struct Accepted(T)
      # :nodoc:
      getter result : T

      # Returns the diagnostics associated with this outcome.
      getter diagnostics : Slice(Diagnostic)

      # :nodoc:
      def initialize(@result : T, @diagnostics : Slice(Diagnostic))
      end

      # Shorthand for `Outcome.elaborate(Diagnostic.key(key), self)`.
      def at(key)
        if @diagnostics.empty? # Fast path
          return self
        end

        Outcome.elaborate(Diagnostic.key(key), self)
      end

      # Shorthand for `Outcome.amend(self, &)`.
      def amend(&)
        Outcome.amend(self) { |result| yield result }
      end

      # Shorthand for `Outcome.map(self, &)`.
      def map(&)
        Outcome.map(self) { |result| yield result }
      end

      # Shorthand for `Outcome.fmap(self, &)`.
      def fmap(&)
        Outcome.fmap(self) { |result| yield result }
      end

      def unwrap : T
        @result
      end
    end

    # A function, unit, agent, etc. rejected the input without further elaboration:
    # it did not recognize the input in any meaningful way; the input "fell through".
    # The caller should try something else.
    struct Rejected
      def unwrap
        raise ArgumentError.new
      end
    end

    # TODO: Some functions below belong to the instance-side of Accepted and Rejected!!

    def ok(result) : Accepted
      Accepted.new(result, diagnostics: Slice(Diagnostic).empty)
    end

    def ok_despite(result, *args) : Accepted
      Accepted.new(result, diagnostics: Slice[Diagnostic.new(Slice(Diagnostic::Spot).empty, Diagnostic.of(*args))])
    end

    def rej : Rejected
      Rejected.new
    end

    def at(key, outcome)
      elaborate(Diagnostic.key(key), outcome)
    end

    def unwrap?(outcome : Accepted)
      outcome.result
    end

    def unwrap?(outcome : Rejected)
    end

    def as_just_ok?(outcome : Accepted)
      outcome.diagnostics.empty? ? outcome.result : nil
    end

    def as_just_ok?(outcome : Rejected)
    end

    def map(outcome : Accepted, &)
      Accepted.new((yield outcome.result), outcome.diagnostics)
    end

    def map(outcome : Rejected, &)
      outcome
    end

    def fmap(outcome outcome0 : Accepted, &)
      outcome1 = yield outcome0.result
      unless outcome1.is_a?(Accepted)
        return outcome1
      end

      if outcome0.diagnostics.empty?
        return outcome1
      end

      Accepted.new(outcome1.result, outcome0.diagnostics + outcome1.diagnostics)
    end

    def fmap(outcome : Rejected, &)
      Rejected.new
    end

    def amend(outcome outcome0 : Accepted, &)
      outcome1 = yield outcome0.result
      unless outcome1.is_a?(Accepted)
        return outcome1
      end

      if outcome1.diagnostics.empty?
        return outcome1
      end

      Accepted.new(outcome1.result, outcome0.diagnostics + outcome1.diagnostics)
    end

    def amend(outcome : Rejected, &)
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

      Accepted.new(outcome.result, diagnostics)
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

      # Returns *outcome*'s result, accumulating diagnostics.
      def unwrap(outcome : Accepted)
        @diagnostics.concat(outcome.diagnostics)

        outcome.result
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

      outcome = yield Accumulator.new(diagnostics)
      if outcome.is_a?(Rejected)
        return outcome
      end

      if amend && outcome.diagnostics.empty?
        return outcome
      end

      diagnostics.concat(outcome.diagnostics)

      Accepted.new(outcome.result, diagnostics.to_unsafe_readonly_slice!)
    end
  end
end
