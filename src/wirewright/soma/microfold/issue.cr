# Hosts Microfold issue-reporting machinery.
#
# You generally shouldn't be interested in anything other than `Severity`
# and `Backtrace`.
module Ww::Soma::Microfold::Issue
  extend self

  # Lists the available issue severity levels. Each level can also serve
  # as a cutoff -- including that level and higher.
  enum Severity
    # Matches all severities (lowest threshold).
    ANY

    # Messages about the algorithm's decision-making.
    Note

    # Unexpected but possibly recovered-from issue.
    Minor

    # Non-recoverable or otherwise important issue in this branch.
    Major

    # Reserved for errors in the theme or environment.
    Severe

    # Reserved for validation errors.
    Fatal

    # Suppresses all output.
    QUIET
  end

  # Includers are different *issues spots*. A issue spot is roughly
  # the location part of a backtrace entry.
  module Spot
    # Formats this spot using the internal `Ww::Ω` terminal/stringformatting framework.
    abstract def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element

    # Keypath into root.
    record Keypath, keypath : Stack(Term) do
      include Spot

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        copy = Term::Dict.build do |commit|
          (1...keypath.size).each do |index|
            commit << keypath[index]
          end
        end

        view = srcmap[copy]?

        until view || copy.empty?
          copy = copy.without(copy.itemsize - 1)
          view = srcmap[copy]?
        end

        omega = Ω.row(Ω.text("node with keypath"), Ω.text(copy.items.join(":"), :emphasis), gap: 1)

        if view
          _, line, column = ML::SyntaxError.lookaround(view)

          omega = Ω.col(
            Ω.row(
              Ω.text("dict at"),
              Ω.text("#{filename}:#{line}:#{column}", :link),
              gap: 1,
            ),
            omega
          )
        end

        omega
      end
    end

    # No assumptions are made about where *text* points to.
    record Detail, detail : String, text : StringView do
      include Spot

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        Ω.row(Ω.text(detail), Ω.text(text, :emphasis), gap: 1)
      end
    end

    record TermDetail, detail : String, term : Term do
      include Spot

      def to_omega(root : Term, srcmap : ML::SrcMap, filename : String) : Ω::Element
        Ω.row(Ω.text(detail), Ω.text(ML.compact(term), :emphasis), gap: 1)
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

  # Sets up the issue-reporting machinery. Yields a sink to report issues to,
  # and an array which will be populated with backtraces as issues are encountered.
  #
  # WARNING: the yielded issue sink is allocated on the stack inside this method;
  # it **must not** under any circumstance outlive the block.
  def setup(*, severity : Severity, & : Issue::Sink, Array(Backtrace) ->)
    backtraces = [] of Issue::Backtrace

    buffer = uninitialized ReferenceStorage(Issue::Sink)
    issues = Issue::Sink.unsafe_construct(pointerof(buffer), severity, backtraces, Issue::None.new)

    yield issues, backtraces
  end
end
