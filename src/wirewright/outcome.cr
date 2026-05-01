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
      # Prepends a sequence of spots constituting a path to this entity, which
      # results in a `Diagnostic`.
      def at(spot, *spots) : Diagnostic
        current = self
        spots.reverse_each do |spot|
          current = Outcome.elaborate(Diagnostic.key(spot), current)
        end

        Outcome.elaborate(Diagnostic.key(spot), current)
      end
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
      # Returns the diagnostics associated with this outcome.
      getter diagnostics : Slice(Diagnostic)

      # :nodoc:
      def initialize(@result : T, @diagnostics : Slice(Diagnostic))
      end

      # Prepends a sequence of *key* spots to all diagnostics in this outcome.
      def at(*keys) : Accepted
        if @diagnostics.empty? # Fast path
          return self
        end

        current = self
        keys.reverse_each do |key|
          current = Outcome.elaborate(Diagnostic.key(key), current)
        end

        current
      end

      # Shorthand for `Outcome.amend(self, &)`.
      def amend(&) : Accepted
        Outcome.amend(self) { |result| yield result }
      end

      # Shorthand for `Outcome.map(self, &)`.
      def map(&) : Accepted
        Accepted.new((yield @result), @diagnostics)
      end

      # Shorthand for `Outcome.bind(self, &)`.
      def bind(&)
        Outcome.bind(self) { |result| yield result }
      end

      # Returns the underlying object.
      def unwrap : T
        @result
      end

      # :ditto:
      def unwrap? : T?
        @result
      end
    end

    # A function, unit, agent, etc. rejected the input without further elaboration:
    # it did not recognize the input in any meaningful way; the input "fell through".
    # The caller should try something else.
    struct Rejected
      # Passthrough.
      def at(*keys) : Rejected
        self
      end

      # :ditto:
      def map(&) : Rejected
        self
      end

      # :ditto:
      def bind(&) : Rejected
        self
      end

      # Returns `nil`.
      def unwrap? : Nil
      end
    end

    # Constructs an `Accepted` outcome for *result*.
    def ok(result) : Accepted
      Accepted.new(result, diagnostics: Slice(Diagnostic).empty)
    end

    # Constructs an `Accepted` outcome with a diagnostic. *args* are passed to
    # `Diagnostic.of` to create the diagnostic.
    def ok_despite(result, *args) : Accepted
      Accepted.new(result, diagnostics: Slice[Diagnostic.new(Slice(Diagnostic::Spot).empty, Diagnostic.of(*args))])
    end

    # Constructs a `Rejected` outcome.
    def rej : Rejected
      Rejected.new
    end

    # Lets the block process the result of *outcome* and respond with an outcome.
    # Rejection wins; if the block accepts the result, diagnostics are concatenated.
    def bind(outcome outcome0 : Accepted, &)
      outcome1 = yield outcome0.unwrap
      unless outcome1.is_a?(Accepted)
        return outcome1
      end

      if outcome0.diagnostics.empty?
        return outcome1
      end

      Accepted.new(outcome1.unwrap, outcome0.diagnostics + outcome1.diagnostics)
    end

    # :ditto:
    def bind(outcome : Rejected, &)
      Rejected.new
    end

    # Lets the block process the result of *outcome* and respond with an outcome.
    # Rejection wins; if the block accepts the result with no diagnostics, all diagnostics
    # of *outcome* are discarded. Hence the name, "amend" -- you "forgive" any diagnostics in
    # *outcome* if the block succeeds.
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

    # :ditto:
    def amend(outcome : Rejected, &)
      Rejected.new
    end

    # Lets the block process the results of more than one outcome; otherwise
    # the same as `bind`.
    macro bind(outcome0, outcome1, *outcomes, &block)
      {{@type}}.bind({{outcome0}}) do |%result|
        {{@type}}.bind({{outcome1}}, {{outcomes.splat}}) do |*%results|
          pass(%result, *%results) {{block}}
        end
      end
    end

    # Returns the first `Accepted` branch. If no branch accepts, returns `Rejected`.
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

    # Returns the first `Accepted` branch. If no branch accepts, raises `ArgumentError`.
    # Therefore, the return type here is just `Accepted`.
    macro choice!(*branches)
      pass do
        {% for branch, index in branches %}
          if %var{index} = {{branch}}.as?({{@type}}::Accepted)
            next %var{index}
          end
        {% end %}

        raise ArgumentError.new
      end
    end

    # Prepends the spot returned by the block to all diagnostics in *outcome*.
    #
    # We accept the block instead of taking `Diagnostic::Spot` as-is to avoid
    # computing the spot (which may be expensive) in the happy path, which is by
    # far the most common path (i.e., no diagnostics).
    def elaborate(outcome : Accepted, & : -> Diagnostic::Spot) : Accepted
      if outcome.diagnostics.empty?
        return outcome
      end

      spot = yield

      diagnostics = outcome.diagnostics.to_readonly_slice do |bt|
        elaborate(spot, bt)
      end

      Accepted.new(outcome.unwrap, diagnostics)
    end

    # :ditto:
    def elaborate(outcome : Rejected, &) : Rejected
      outcome
    end

    # :nodoc:
    def elaborate(spot : Diagnostic::Spot, entity : Diagnostic::Entity) : Diagnostic
      Diagnostic.new(Slice[spot.as(Diagnostic::Spot)], entity)
    end

    # :nodoc:
    def elaborate(spot : Diagnostic::Spot, diagnostic : Diagnostic) : Diagnostic
      Diagnostic.new(diagnostic.path.prepend(spot), diagnostic.entity)
    end

    # Prepends *spot* to all diagnostics in *outcome*.
    def elaborate(spot : Diagnostic::Spot, outcome : Accepted | Rejected)
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

        outcome.unwrap
      end

      def unwrap(outcome : Accepted, &)
        unwrap(outcome)
      end

      def unwrap(outcome : Rejected, &)
        yield
      end
    end

    # Lets you accumulate diagnostics.
    #
    # If *amend* is `true`, the accumulated diagnostics are discarded if the block
    # accepts without diagnostics. Otherwise, the block's diagnostics are concatenated
    # to the accumulated ones.
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

      Accepted.new(outcome.unwrap, diagnostics.to_unsafe_readonly_slice!)
    end
  end
end
