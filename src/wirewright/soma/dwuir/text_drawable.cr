module Ww::Soma::DwUIR
  # Acts as a client of `TextCommand`; emits `InlineString` or `Selection`
  # where appropriate.
  module TextDrawable
    extend self

    alias Any = InlineString | Selection

    # Represents an optionally *selected*, inline *string*.
    record InlineString, bounds : Rect, string : String, selected : Bool

    # Represents a selection rectangle.
    record Selection, bounds : Rect, rank : Rank

    private alias Tcmd = TextCommand

    # Calls *sink* with each text drawable for *string*.
    def each(pencil : IPencil, spec : WrapSpec, string : String, selection : TextSelectionRange, &sink : Any ->)
      buffer = [] of StringView
      selected = false
      selrect = Rect.new(tl: Point.new(0, 0), size: Point.new(0, 0))

      Tcmd.each_with_bounds(pencil, spec, string, selection) do |command, bounds|
        case command
        in Tcmd::PushInline
          buffer << command.view
          next unless selected

          selrect = selrect.grow(dw: bounds.w)
        in Tcmd::WriteInline
          bufstr = StringView.join(buffer).to_s
          sink.call(InlineString.new(bounds, bufstr, selected))
          buffer.clear
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
