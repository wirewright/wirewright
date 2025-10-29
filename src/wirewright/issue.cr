# Hosts issue-reporting machinery that is used by multiple subsystems of Wirewright,
# for instance by `Microfold` and `Alloy`.
module Ww::Issue
  extend self

  # Lists the available issue severity levels. Each level can also serve
  # as a cutoff -- only issues on that level and higher will be stored,
  # processed, and displayed later on.
  enum Severity
    # Matches all severities (lowest threshold).
    ANY

    # Messages about an algorithm's decision-making.
    Note

    # User issue with low emphasis. Perhaps caused by another issue.
    Minor

    # User issue with high emphasis. Perhaps can trigger a cascade of issues.
    Major

    # System/internal issue -- issue in a system resource (e.g. invalid rule
    # system, error in theme).
    Severe

    # An issue that terminates execution immediately and jumps back to
    # the calling/tracing point.
    Fatal

    # Suppresses all severities.
    QUIET
  end

  # Includers are different *issues spots*.
  #
  # One constructs chains of spots to point to the location of an issue.
  #
  # Spots are freely extensible (you can include `Spot` anytime). They do not
  # have any implementation requirements.
  #
  # We do not recommend filling spots with context that is usually available
  # anyway at backtrack printing-time. In other words, we recommend you to think
  # of spots as "instructions" to a "machine" that will read them left-to-right,
  # on error; spots are *not* ready and fully informative backtrace entries.
  # Then, make sure to give enough context to this "matchine", which should be
  # pretty straightforward; and use spots to "navigate" into different parts
  # of that context, collect info, and finally produce an actual, information-
  # saturated backtrace entry.
  #
  # Since such a "machine" -- a *spot printer* -- is usually very context-dependent,
  # we do not provide general-purpose printing; and instead expect clients to print
  # whichever spots they want to, and in ways they see fit. In other words, spot printing
  # is out of scope for `Ww::Issue` and `Spot` in particular.
  module Spot
    # Keypath into some term.
    #
    # NOTE: Unless you have an immutable *keypath* right away, please use `KeypathRef`
    # instead. `KeypathRef`s will be expanded into `Keypath` on clone.
    record Keypath, keypath : ThinArray(Term) do
      include Spot
    end

    # A reference to a mutable keypath.
    #
    # NOTE: This spot stores a reference to a mutable keypath, and its size
    # at the time of `KeypathRef`'s construction. Thus, it will only work if
    # the keypath never underflows that size while the spot is active, which
    # is usually the case since mutable keypaths behave much like call stacks.
    struct KeypathRef
      include Spot

      @size : Int32

      def initialize(@keypath : ThinArray(Term))
        @size = @keypath.size
      end

      def clone
        keypath1 = ThinArray(Term).new

        @keypath.each_with_index do |key, index|
          break if index >= @size
          keypath1 << key
        end

        Keypath.new(keypath1)
      end
    end

    # An emphasized string view *text* annotated with a *detail* string.
    #
    # No assumptions are made about where *text* points to.
    record StringDetail, detail : String, text : StringView do
      include Spot
    end

    # An emphasized *term* annotated with a *detail* string.
    record TermDetail, detail : String, term : Term do
      include Spot
    end

    # A static *string*.
    record Text, string : String do
      include Spot
    end

    # A key into some structure, possibly detailed in the previous spot(s).
    record Key, detail : String, key : Term do
      include Spot
    end
  end

  # Represents a backtrace.
  #
  # *spots* enhance *severity* and *detail* with contextual metadata. The leftmost
  # spot is the root-most spot and the rightmost spot is closest to the place where
  # the error occurred.
  record Backtrace, spots : Array(Spot), severity : Severity, detail : String

  # :nodoc:
  alias TraceEdge = None | Some

  # :nodoc:
  record None
  # :nodoc:
  record Some, prev : Sink, spot : Spot

  # Administers issue reports.
  #
  # See `setup`. Do not initialize manually; there is a lot of stack-allocated
  # `ReferenceStorage` madness to set this up properly.
  #
  # Internally, `Sink`s form a stack-allocated linked list through `TraceEdge`s.
  # Each `Sink` stores a `Spot` which will enhance any leaf `add` with contextual
  # metadata, were `add` to ever be called with the right severity; for that purpose,
  # on `add`, the linked list is traversed back-to-front, each spot cloned to form
  # a backtrace.
  #
  # We use stack allocation to avoid paying the allocation cost in the happy path.
  # Instead, like exceptions, we prefer a slower sad path (`add`).
  class Sink
    # :nodoc:
    def initialize(@severity : Severity, @backtraces : Array(Backtrace), @edge : TraceEdge)
    end

    def version : Int32
      # Since @backtraces is append-only, we can use its size as a sink-unique
      # version number.
      @backtraces.size
    end

    # Suppresses issue emission for the duration of the block.
    def suppress(&)
      tmp, @severity = @severity, Severity::QUIET
      begin
        yield
      ensure
        @severity = tmp
      end
    end

    # :nodoc:
    def adjoin(spot : Spot, & : Sink ->)
      slot = uninitialized ReferenceStorage(Sink)

      yield Sink.unsafe_construct(pointerof(slot), @severity, @backtraces, Some.new(self, spot))
    end

    # Adds one or more *spots* to the end of the trace for the duration
    # of the block.
    #
    # NOTE: The yielded sink must not outlive the block.
    def adjoin(spot : Spot, *spots : Spot, &)
      adjoin(spot) do |node|
        node.adjoin(*spots) do |tail|
          yield tail
        end
      end
    end

    # Shorthand for adjoining `Spot::Text`.
    def adjoin(text : String, &)
      adjoin(Spot::Text.new(text)) { |sink| yield sink }
    end

    # Shorthand for adjoining `Spot::TermDetail`.
    def adjoin(detail : String, term : Term, &)
      adjoin(Spot::TermDetail.new(detail, term)) { |sink| yield sink }
    end

    # Shorthand for adjoining `Spot::StringDetail`.
    def adjoin(detail : String, view : StringView, &)
      adjoin(Spot::StringDetail.new(detail, view)) { |sink| yield sink }
    end

    # Shorthand for adjoining `Spot::Key`.
    #
    # Uses `Term.of` to convert *key* to a term (if it's not a term already).
    def adjoin(*, key, detail : String = "key", &)
      adjoin(Spot::Key.new(detail, Term.of(key))) { |sink| yield sink }
    end

    # Reports an issue with the given *severity*.
    #
    # This is the `Issue` equivalent of a `raise`, which potentially
    # similar expenses.
    def add(severity : Severity, detail : String) : Nil
      add(severity) { detail }
    end

    def add(severity : Severity, & : -> String) : Nil
      return if severity < @severity

      backtrace = [] of Spot
      node = self

      while issue = node.@edge.as?(Some)
        backtrace.unshift(issue.spot.clone)
        node = issue.prev
      end

      @backtraces << Backtrace.new(backtrace, severity, yield)
    end

    {% for const in Severity.constants %}
      {% unless const == Severity::ANY || const == Severity::QUIET %}
        # Shorthand for `add({{const.id.underscore.symbolize}}, detail)`.
        def {{const.id.underscore}}(detail : String) : Nil
          add(Severity::{{const}}, detail)
        end

        # Shorthand for `add({{const.id.underscore.symbolize}}, &)`.
        def {{const.id.underscore}}(&)
          add(Severity::{{const}}) { yield }
        end
      {% end %}
    {% end %}
  end

  # Sets up the issue-reporting machinery. Yields a sink to report issues to.
  # Returns the block's result along with an array which will be populated
  # with backtraces for issues that were encountered (if any).
  #
  # WARNING: the yielded issue sink is allocated on the stack inside this method;
  # it **must not** under any circumstance outlive the block.
  def setup(*, severity : Severity, & : Issue::Sink -> T) : {T, Array(Backtrace)} forall T
    backtraces = [] of Issue::Backtrace

    buffer = uninitialized ReferenceStorage(Issue::Sink)
    issues = Issue::Sink.unsafe_construct(pointerof(buffer), severity, backtraces, Issue::None.new)

    result = yield issues

    {result, backtraces}
  end
end
