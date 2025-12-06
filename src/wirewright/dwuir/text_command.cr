module Ww::DwUIR
  # Text commands tell the client imperatively what to do to render the text
  # elements that a string results in, while buffering consecutive pieces
  # for efficiency (if the client wishes to).
  module TextCommand
    extend self

    alias Any = WriteInline | NextLine | BeginSelection | EndSelection | PushInline | PushVirtual | PutCursor

    # The client should "flush" all pushed inlines and virtuals
    # in case it buffered them.
    record WriteInline

    # The client should move to a new line.
    #
    # The server guarantees to call this only after `WriteInline` (if pushed).
    record NextLine

    # The client should change its state so as to begin a selection.
    record BeginSelection

    # The client should change its state so as to end a selection that
    # was previously started with `BeginSelection`. The server guarantees
    # that this command will only be issued following `BeginSelection`.
    record EndSelection

    # The client should append *view* (a guaranteed inline string view
    # of the original string) to its buffer; or handle it immediately.
    record PushInline, view : StringView do
      # Constructs a string from `view` for compatibility with `PushVirtual`.
      def string : String
        view.to_s
      end
    end

    # The client should append *string* to its buffer; or handle it immediately.
    record PushVirtual, string : String do
      # Constructs a vie from `string` for compatibility with `PushInline`.
      def view : StringView
        string.view
      end
    end

    # The client should display the cursor I-beam at the current position.
    #
    # Note that this command is issued *before* the character pointed to by
    # the cursor if its position is interpreted as an index. If the client
    # wishes to do something other than an I-beam, it is their responsibility
    # to wait for the next character and highlight it (if any; the cursor
    # may be located "before" the imaginary end-of-string character, too.)
    #
    # This command may be issued at any point and within any state;
    # it is the client's responsibility to handle that appropriately.
    record PutCursor

    # Calls *sink* with each *text command* to render the given *string*.
    def each(pencil : IPencil, spec : WrapSpec, string : String, selection : TextSelectionRange, &sink : Any ->)
      selected0 = false
      flush = false

      TextElement.each(pencil, spec, string, selection) do |element|
        case element
        in TextElement::Cursor
          sink.call(PutCursor.new)
        in TextElement::Run
          token, selected1 = element.token, element.selected

          unless selected0 == selected1
            if flush
              sink.call(WriteInline.new)
              flush = false
            end

            sink.call(selected1 ? BeginSelection.new : EndSelection.new)
            selected0 = selected1
          end

          case token
          in WrapToken::Empty
          in WrapToken::InlineText
            sink.call(PushInline.new(token.view))
            flush = true
          in WrapToken::Ellipsis
            sink.call(PushVirtual.new(token.string))
            flush = true
          in WrapToken::LineBreak
            if flush
              sink.call(WriteInline.new)
              flush = false
            end
            sink.call(NextLine.new)
          in WrapToken::Over
            if flush
              sink.call(WriteInline.new)
              flush = false
            end

            if selected0
              sink.call(EndSelection.new)
              selected0 = false
            end
          end
        end
      end
    end

    # Calls *sink* with each text command, constructing an appropriate bounding
    # box for it.
    #
    # - `PushInline` and `PushVirtual` are put at the current position and sized
    #    according to its content.
    # - `WriteInline` is put at the beginning of line or at the beginning of the previous
    #   `WriteInline`, and measured up to the current position.
    # - `NextLine` is put at the current position (not on the next line, i.e. you still
    #    have to "type newline"). Its width is whitespace width, and its height is
    #    the line height for *pencil*.
    # - `BeginSelection` and `EndSelection` are put at the current position; their width
    #   is 0 and their height is the line height.
    # - `PutCursor` is put at the current position; its width is 1 and its height is
    #   the line height.
    def each_with_bounds(pencil : IPencil, spec : WrapSpec, string : String, selection : TextSelectionRange, &sink : Any, Rect ->)
      finger = pencil
      nlsize = Point.new(finger.space_width, finger.line_height)
      inline_start = finger.origin

      each(pencil, spec, string, selection) do |command|
        case command
        in PushInline, PushVirtual
          finger1 = finger.after_writing(command.string)
          bounds = Rect.new(tl: finger.origin, size: Point.new(finger1.origin.x - finger.origin.x, nlsize.y))
          finger = finger1
        in WriteInline
          bounds = Rect.new(tl: inline_start, size: Point.new(finger.origin.x - inline_start.x, nlsize.y))
          inline_start = finger.origin
        in NextLine
          bounds = Rect.new(tl: finger.origin, size: nlsize)
          finger = finger.after_writing('\n')
          inline_start = finger.origin
        in BeginSelection, EndSelection
          bounds = Rect.new(tl: finger.origin, size: Point.new(0, nlsize.y))
        in PutCursor
          bounds = Rect.new(tl: finger.origin, size: Point.new(1, nlsize.y))
        end

        sink.call(command, bounds)
      end
    end
  end
end
