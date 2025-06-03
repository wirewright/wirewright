module Ww::Soma::DwUIR
  # Configuration for the wrapping algorithm.
  #
  # - *ellipsis* is the ellipsis string. It will be inserted at the end of the wrapped
  #   text to indicate omission (if any).
  # - If *on words* is `true`, the wrapping algorithm will try to break on word boundaries.
  # - If *on letters* is `true`, the wrapping algorithm will try to break on letter boundaries.
  # - *bounds* specifies the bounding box; **its top-left corner must be at 0, 0**.
  #
  # TODO: currently "letter boundaries" means Crystal character boundaries; we'll have to
  # work with graphemes at some point.
  record WrapSpec,
    ellipsis : String,
    on_words : Bool,
    on_letters : Bool,
    bounds : Rect,
    history : Int32

  struct WrapSpec
    def self.nowrap : WrapSpec
      new(
        ellipsis: "",
        on_words: false,
        on_letters: false,
        bounds: Rect.inf,
        history: WrapHistory::INFINITE,
      )
    end
  end

  # :nodoc:
  struct WrapHistory
    INFINITE = 0

    def initialize(@limit : Int32, @sink : WrapToken::Any ->)
      @decisions = Deque({IPencil, WrapToken::Any}).new
    end

    delegate :empty?, :each, :pop, to: @decisions

    def <<(decision : {IPencil, WrapToken::Any}) : Nil
      if 0 < @limit < @decisions.size + 1
        _, token = @decisions.shift
        @sink.call(token)
      end

      @decisions << decision
    end
  end

  # A simple text wrapping algorithm.
  module WrapToken
    extend self

    alias Any = Empty | InlineText | LineBreak | Ellipsis | Over

    # Tokens that represent a line break of some kind.
    alias LineBreak = SpaceBreak | SoftBreak | HardBreak

    # Tokens that have no size.
    alias Virtual = Empty | SoftBreak | Ellipsis | Over

    # Represents "nothingness" as a wrap token. May be emitted by `partition`.
    record Empty

    # Represents a *guaranteed inline* string view (i.e. does not contain
    # newline characters).
    record InlineText, view : StringView

    # Represents a space turned into a line break by the wrapping algorithm.
    record SpaceBreak

    # Represents "nothingness" turned into a line break.
    record SoftBreak

    # Represents the client's newline character.
    record HardBreak

    # Represents an omission when there's not enough space for the full text.
    record Ellipsis, view : StringView

    # Represents end-of-text.
    record Over

    # :nodoc:
    def span(token : Virtual) : Int32
      0
    end

    # :nodoc:
    def span(token : SpaceBreak | HardBreak) : Int32
      1
    end

    # :nodoc:
    def span(token : InlineText) : Int32
      token.view.size
    end

    {% if flag?(:docs) %}
      # Returns the number of characters that *token* occupies in
      # the original string.
      def span(token : Any) : Int32
      end
    {% end %}

    # :nodoc:
    def partition(token : Virtual, index : Int32)
      raise IndexError.new
    end

    # :nodoc:
    def partition(token : SpaceBreak | HardBreak, index : Int32)
      case index
      when 0
        {Empty.new, token, Empty.new}
      when 1
        {token, Empty.new, Empty.new}
      else
        raise IndexError.new
      end
    end

    # :nodoc:
    def partition(token : InlineText, index : Int32)
      unless index.in?(0..token.view.size)
        raise IndexError.new
      end

      l, mid, r = token.view.partition(index)

      {l.empty? ? Empty.new : InlineText.new(l),
       mid.empty? ? Empty.new : InlineText.new(mid),
       r.empty? ? Empty.new : InlineText.new(r)}
    end

    {% if flag?(:docs) %}
      # Splits *token* into three parts: one before *index*, one at *index*,
      # and one after *index*.
      #
      # May raise `IndexError` if *index* is out of *token*'s bounds.
      def partition(token : Any, index : Int32)
      end
    {% end %}

    # :nodoc:
    def advance(token : InlineText, pencil : IPencil) : IPencil
      pencil.after_writing(token.view)
    end

    # :nodoc:
    def advance(token : LineBreak, pencil : IPencil) : IPencil
      pencil.after_writing('\n')
    end

    # Returns the copy of *pencil* after skipping *token*.
    def advance(token, pencil : IPencil) : IPencil
      pencil
    end

    private def wrap(pencil0, spec, line : StringView, *, to history)
      unless pencil0.in?(spec.bounds)
        return pencil0, false
      end

      line.each_word_with_index do |word, word_index|
        # Look if the word fits inline. If it does, commit.
        pencil1 = pencil0.after_writing(word)
        if pencil1.in?(spec.bounds) || (word_index.zero? && !spec.on_letters)
          history << {pencil0, InlineText.new(word)}
          pencil0 = pencil1
          next
        end

        # Look if the word fits on its own line. If it does, commit. Do not break
        # before the first word. Force-commit if letter breaking is disabled; this
        # will trigger clipping later on, but that's actually expected in this case
        # since there's nothing else legal to do.
        if word_index.nonzero? && spec.on_words
          chopped = word.blank? ? word : word.lchop(' ')

          pencil1 = pencil0.after_writing('\n')
          pencil2 = pencil1.after_writing(chopped)

          if spec.bounds.includes_y?(pencil2) && (spec.bounds.includes_x?(pencil2) || !spec.on_letters)
            if word == chopped
              history << {pencil0, SoftBreak.new}
            else
              history << {pencil0, SpaceBreak.new}
            end

            history << {pencil1, InlineText.new(chopped)}
            pencil0 = pencil2
            next
          end
        end

        unless spec.on_letters
          return pencil0, false
        end

        # Try continuing the current line for as long as possible, allowing
        # leading whitespace.
        word.each_char_with_index do |ch, ch_index|
          pencil1 = pencil0.after_writing(ch)
          if (word_index.zero? && ch_index.zero?) || pencil1.in?(spec.bounds)
            history << {pencil0, InlineText.new(ch.view)}
            pencil0 = pencil1
            next
          end

          # If we're unable to fit the character in the current line we force
          # a soft break. If the text is too small to even fit the line break/
          # single character on its own line, we'll see clipping -- i.e. let us
          # clip instead of hiding the characters altogether. That's the reason
          # we only check for whether the Y coordinate is in bounds here.

          pencil1 = pencil0.after_writing('\n')
          pencil2 = pencil1.after_writing(ch)

          unless spec.bounds.includes_y?(pencil2)
            return pencil0, false
          end

          if ch == ' '
            history << {pencil0, SpaceBreak.new}
          else
            history << {pencil0, SoftBreak.new}
            history << {pencil1, InlineText.new(ch.view)}
          end

          pencil0 = pencil2
        end
      end

      {pencil0, true}
    end

    # Wraps *string* according to the wrapping spec *spec*. Calls *sink* with
    # each resulting token.
    #
    # Returns *pencil* after writing, followed by a boolean indicating whether
    # *string* fits into *spec*'s bounding box.
    #
    # For simplicity, it is the caller's responsibility to track the position
    # of each token (starting from *pencil*).
    def each(pencil pencil0 : IPencil, spec : WrapSpec, string : String, &sink : Any ->) : {IPencil, Bool}
      history = WrapHistory.new(spec.history, sink)

      string.each_line_view do |line|
        if hard_break = line.ends_with?('\n')
          line = line.rstrip
        end

        pencil0, complete = wrap(pencil0, spec, line, to: history)

        if hard_break
          history << {pencil0, HardBreak.new}
          pencil0 = pencil0.after_writing('\n')
          complete = pencil0.in?(spec.bounds)
        end

        next if complete

        # Undo until we can fit ellipsis.
        loop do
          pencil1 = pencil0.after_writing(spec.ellipsis)
          if history.empty? || pencil1.in?(spec.bounds)
            pencil0 = pencil1
            break
          end

          pencil0, _ = history.pop
        end

        history.each { |_, token| sink.call(token) }

        sink.call(Ellipsis.new(spec.ellipsis.view))
        sink.call(Over.new)

        return pencil0, false
      end

      history.each { |_, token| sink.call(token) }

      sink.call(Over.new)

      {pencil0, true}
    end

    # Augments each wrap token from `wrap` with an exclusive character range for
    # that token in *string*.
    def each_with_range(pencil, spec, string, &sink : Any, Range(Int32, Int32) ->)
      b = 0

      each(pencil, spec, string) do |token|
        e = b + span(token)
        sink.call(token, b...e)
        b = e
      end
    end
  end
end
