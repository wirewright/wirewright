module Ww::DwUIR
  # Acts as a client of `TextCommand`; emits `InlineString` or `Selection`
  # where appropriate.
  module TextDrawable
    extend self

    alias Any = InlineString | Selection

    # Represents an optionally *selected*, inline string.
    #
    # The raw *segments* are given to you so that you can extract the indices
    # of non-virtual sequences of characters from the original string.
    record InlineString, bounds : Rect, segments : Array(Segment), selected : Bool do
      def string : String
        segments.join
      end
    end

    # Represents a selection rectangle.
    record Selection, bounds : Rect, rank : Rank

    private alias Tcmd = TextCommand

    alias Segment = VirtualSegment | InlineSegment

    record VirtualSegment, string : String do
      def to_s(io)
        io << string
      end
    end

    record InlineSegment, view : StringView do
      def to_s(io)
        io << view
      end
    end

    # Calls *sink* with each text drawable for *string*.
    def each(pencil : IPencil, spec : WrapSpec, string : String, selection : TextSelectionRange, &sink : Any ->)
      buffer = [] of Segment
      selected = false
      selrect = Rect.new(tl: Point.new(0, 0), size: Point.new(0, 0))

      Tcmd.each_with_bounds(pencil, spec, string, selection) do |command, bounds|
        case command
        in Tcmd::PushInline
          buffer << InlineSegment.new(command.view)
          next unless selected

          selrect = selrect.grow(dw: bounds.w)
        in Tcmd::PushVirtual
          buffer << VirtualSegment.new(command.string)
        in Tcmd::WriteInline
          sink.call(InlineString.new(bounds, buffer, selected))
          buffer = [] of Segment
        in Tcmd::NextLine
          next unless selected

          selrect = selrect.grow(dw: bounds.w)
          unless selrect.empty?
            sink.call(Selection.new(selrect, :back))
          end

          selrect = Rect.new(tl: Point.new(0, bounds.y + bounds.h), size: Point.new(0, bounds.h))
        in Tcmd::BeginSelection
          selected = true
          selrect = bounds
        in Tcmd::EndSelection
          unless selrect.empty?
            sink.call(Selection.new(selrect, :back))
          end

          selrect = bounds
          selected = false
        in Tcmd::PutCursor
          sink.call(Selection.new(bounds, :front))
        end
      end
    end
  end
end
