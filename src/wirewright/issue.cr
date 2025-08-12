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

  # Includers are different *issues spots*. A issue spot is roughly
  # the location part of a backtrace entry.
  module Spot
    # Formats this spot using the internal `Ww::Ω` terminal/stringformatting framework.
    abstract def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element

    # Keypath into the root term passed to `to_omega`.
    record Keypath, keypath : Stack(Term) do
      include Spot

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        copy = Term[keypath]
        view = srcmap[copy]?

        until view || copy.empty?
          copy = copy.without(copy.itemsize - 1)
          view = srcmap[copy]?
        end

        if view
          if keypath.size > copy.itemsize
            omega = Ω.row(Ω.text("subnode with keypath"), Ω.text(keypath.skip(copy.size).join(":"), :emphasis), gap: 1)
          end

          _, line, column = ML::SyntaxError.lookaround(view)

          omega = Ω.col(
            Ω.row(
              Ω.text("dict at"),
              Ω.text("#{filename}:#{line}:#{column}", :link),
              gap: 1,
            ),
            omega
          )
        else
          omega = Ω.row(Ω.text("node with keypath"), Ω.text(keypath.join(":"), :emphasis), gap: 1)
        end

        omega
      end
    end

    # :nodoc:
    struct KeypathView
      include Spot

      @size : Int32

      def initialize(@keypath : Stack(Term))
        @size = @keypath.size
      end

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        spot = Keypath.new(@keypath)
        spot.to_omega(root, srcmap, filename)
      end

      def clone
        keypath1 = Stack(Term).new

        @keypath.each_with_index do |key, index|
          break if index >= @size
          keypath1 << key
        end

        KeypathView.new(keypath1)
      end
    end

    # An emphasized string view *text* annotated with a *detail* string.
    #
    # No assumptions are made about where *text* points to.
    record StringDetail, detail : String, text : StringView do
      include Spot

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        Ω.flip(Ω.text(detail), Ω.text(text, :emphasis), threshold: 60, gap_x: 1)
      end
    end

    # An emphasized *term* annotated with a *detail* string.
    record TermDetail, detail : String, term : Term do
      include Spot

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        Ω.flip(Ω.text(detail), Ω.text(ML.compact(term), :emphasis), threshold: 60, gap_x: 1)
      end
    end

    # A static *string*.
    record Text, string : String do
      include Spot

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        Ω.text(string, :emphasis)
      end
    end
  end

  # Represents a backtrace.
  #
  # *spots* enhance *severity* and *detail* with contextual metadata. The leftmost
  # spot is the root-most spot and the rightmost spot is closest to the place where
  # the error occurred.
  record Backtrace, spots : Array(Spot), severity : Severity, detail : String do
    # Formats this backtrace using the internal `Ww::Ω` terminal/string
    # formatting framework.
    def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
      icon, title, style =
        case severity
        when .note?   then {"\ueb26", "NOTE", Ω::Style::Emphasis}
        when .minor?  then {"\uea6c", "MINOR", Ω::Style::Error}
        when .major?  then {"\uea87", "MAJOR", Ω::Style::ErrorEmphasis}
        when .severe? then {"\uea87", "SEVERE", Ω::Style::Failure}
        when .fatal?  then {"\uee15", "FATAL", Ω::Style::FailureEmphasis}
        else
          unreachable("unrecognized severity")
        end

      Ω.col(
        Ω.text("╻", style),
        Ω.line_prefix(
          Ω.text("┃", style),
          Ω.padding(
            Ω.col(
              Ω.row(Ω.text(icon, style), Ω.text(title, style), gap: 1),
              if spots.present?
                Ω.col(
                  Ω.text("Backtrace (closest to error is last):"),
                  Ω.padding(
                    Ω.col(spots) do |spot|
                      Ω.row(Ω.text("in", :dim), spot.to_omega(root, srcmap, filename), gap: 1)
                    end,
                    pl: 1,
                  ),
                  gap: 1,
                )
              end,
              Ω.row(Ω.text(detail), gap: 1),
              gap: 1,
            ),
            pl: 1
          ),
        ),
        Ω.text("╹", style)
      )
    end
  end

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

    # Reports a issue with the given *severity*.
    #
    # This is the `Issue` equivalent of a `raise`, which potentially
    # similar expenses.
    def add(severity : Severity, detail : String) : Nil
      return if severity < @severity

      backtrace = [] of Spot
      node = self

      while issue = node.@edge.as?(Some)
        backtrace.unshift(issue.spot.clone)
        node = issue.prev
      end

      @backtraces << Backtrace.new(backtrace, severity, detail)
    end

    {% for const in Severity.constants %}
      {% unless const == Severity::ANY || const == Severity::QUIET %}
        # Shorthand for `add({{const.id.underscore.symbolize}}, detail)`.
        def {{const.id.underscore}}(detail : String) : Nil
          add(Severity::{{const}}, detail)
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
