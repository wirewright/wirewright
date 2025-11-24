module Ww
  alias Ω = Omega

  # Terminal output toolkit currently used by the test framework.
  #
  # For lack of a better name it is called Omega right now (as in O, as in Output,
  # as in terminal Output).
  module Omega
    extend self

    alias Element = Text | Row | Col | Padding | Paint | Flip | LinePrefix | LineFrags

    # :nodoc:
    record Frag, caption : String, style : Style

    # :nodoc:
    record Line, frags : Array(Frag) do
      class_getter zero = Line.new([] of Frag)

      def self.new(caption : String, style : Style)
        new([Frag.new(caption, style)])
      end

      def pad_left(amount : Int32)
        copy_with(frags: [Frag.new(" " * amount, style: :normal)] + frags)
      end

      def pad_right(amount : Int32)
        copy_with(frags: frags + [Frag.new(" " * amount, style: :normal)])
      end

      def paint(style : Style)
        copy_with(frags: frags.map { |frag| frag.copy_with(style: style) })
      end

      def each_char(& : Char ->)
        frags.each do |frag|
          frag.caption.each_char { |chr| yield chr }
        end
      end

      def each_char_with_index(& : Char, Int32 ->)
        index = 0
        each_char do |chr|
          yield chr, index
          index += 1
        end
      end

      def +(other : Line)
        Line.new(frags + other.frags)
      end

      def width
        frags.sum(&.caption.size)
      end
    end

    # Lists the available styles of elements.
    enum Style
      Code
      Normal
      Emphasis
      Dim
      Link
      Success
      Info
      Error
      ErrorEmphasis
      Failure
      FailureEmphasis
      Italic
    end

    # :nodoc:
    defcase Text, caption : String, style : Style
    # :nodoc:
    defcase LineFrags, line : Line
    # :nodoc:
    defcase Row, children : Array(Element), gap : Int32
    # :nodoc:
    defcase Col, children : Array(Element), gap : Int32
    # :nodoc:
    defcase Flip, children : Array(Element), threshold : Int32, gap_x : Int32, gap_y : Int32
    # :nodoc:
    defcase Padding, child : Element, pt : Int32, pb : Int32, pl : Int32, pr : Int32
    # :nodoc:
    defcase LinePrefix, prefix : Element, child : Element
    # :nodoc:
    defcase Paint, child : Element, style : Style

    # Displays as a string of text.
    def text(caption : String, style : Style = :normal, *, wrap = true)
      lines = (wrap ? wrap(caption, maxw: 80) : caption).lines
      if lines.size == 1
        return Text.new(caption, style)
      end

      Col.new(lines.map { |line| Text.new(line, style).as(Element) }, gap: 0)
    end

    # :ditto:
    def text(caption : StringView, style : Style = :normal, **kwargs)
      text(caption.to_s, style, **kwargs)
    end

    # :nodoc:
    def line_frags(line : Line)
      LineFrags.new(line)
    end

    # Paints the output of *child* using *style*.
    def painted(child, style : Style, **kwargs)
      Paint.new(child, style, **kwargs)
    end

    # Flips between `row` and `col` based on content width.
    #
    # - If content width is less than *threshold*, uses `row` with *gap_x*.
    # - Otherwise, uses `col` with *gap_y*.
    def flip(*args, threshold, gap_x = 0, gap_y = 0)
      Flip.new([*args] of Element, threshold, gap_x, gap_y)
    end

    # Semantic shorthand for `padding(_, pl: _)`.
    def indent(child, *, by amount : Int32)
      padding(child, pl: amount)
    end

    # Pads *child*.
    #
    # - *px* is added to *pl*, *pr*.
    # - *py* is added to *pt*, *pb*.
    def padding(child, *, px = 0, py = 0, pl = 0, pr = 0, pt = 0, pb = 0)
      Padding.new(child, pt: py + pt, pb: py + pb, pl: px + pl, pr: px + pr)
    end

    # Prefixes each line produced by *child* with an element *prefix*.
    def line_prefix(prefix, child)
      LinePrefix.new(prefix, child)
    end

    # Displays a row of elements with the given *gap* between them.
    def row(*els, gap = 0)
      children = [] of Element
      els.each do |el|
        next if el.nil?
        children << el
      end

      Row.new(children, gap)
    end

    # :ditto:
    def row(els : Array(Element), gap = 0)
      Row.new(els, gap)
    end

    # :ditto:
    def row(els : Enumerable(T), **kwargs, & : T -> Ω::Element) forall T
      row(els.map { |object| (yield object).as(Ω::Element) }, **kwargs)
    end

    # Displays a column of elements with the given *gap* between them.
    def col(*els, gap = 0)
      children = [] of Element
      els.each do |el|
        next if el.nil?
        children << el
      end

      Col.new(children, gap)
    end

    # :ditto:
    def col(els : Array(Element), gap = 0)
      Col.new(els, gap)
    end

    # :ditto:
    def col(els : Enumerable(T), **kwargs, & : T -> Ω::Element) forall T
      col(els.map { |object| (yield object).as(Ω::Element) }, **kwargs)
    end

    private def each_line(el : LineFrags, &sink : Line ->)
      sink.call(el.line)
    end

    private def each_line(el : Text, &sink : Line ->)
      sink.call(Line.new(el.caption, el.style))
    end

    private def each_line(el : Padding, &sink : Line ->)
      el.pt.times { sink.call(Line.new("", :normal)) }

      each_line(el.child) do |line|
        sink.call(line.pad_left(el.pl).pad_right(el.pr))
      end

      el.pb.times { sink.call(Line.new("", :normal)) }
    end

    private def each_line(el : LinePrefix, &sink : Line ->)
      each_line(el.child) do |line|
        each_line(row(el.prefix, line_frags(line)), &sink)
      end
    end

    private def each_line(el : Col, &sink : Line ->)
      lines = [] of Line

      el.children.each_with_index do |child, index|
        if index > 0
          el.gap.times { lines << Line.new("", :normal) }
        end

        each_line(child) do |line|
          lines << line
        end
      end

      return unless lines.present?

      max_line_width = lines.max_of(&.width)

      # Add padding to the right to make equally wide
      lines = lines.map { |line| line.pad_right(max_line_width - line.width) }
      lines.each(&sink)
    end

    private def each_line(el : Row, &sink : Line ->)
      outputs = [] of Array(Line)

      el.children.each_with_index do |child, index|
        output = [] of Line
        each_line(child) do |line|
          output << line
        end
        if index > 0 && el.gap > 0
          outputs << [Line.new(" " * el.gap, :normal)]
        end
        next unless output.present?
        outputs << output
      end

      return unless outputs.present?

      max_col_height = outputs.max_of(&.size)

      outputs.each do |output|
        width = output.max_of(&.width)

        # Add padding to the bottom to make equally high
        (max_col_height - output.size).times do
          output << Line.new(" " * width, :normal)
        end
      end

      outputs.transpose.each do |row|
        sink.call(row.sum)
      end
    end

    private def each_line(el : Paint, &sink : Line ->)
      each_line(el.child) do |line|
        sink.call(line.paint(el.style))
      end
    end

    private def each_line(el : Flip, &sink : Line ->)
      wtotal = 0

      el.children.each do |child|
        w = 0

        each_line(child) do |line|
          w = Math.max(w, line.width)
        end

        wtotal += w
        break if wtotal > el.threshold
      end

      if wtotal <= el.threshold
        each_line(row(el.children, gap: el.gap_x), &sink)
      else
        each_line(col(el.children, gap: el.gap_y), &sink)
      end
    end

    # Renders *el* to the given *io*.
    #
    # - Set *styled* to `false` to disable emission of ANSI escape sequences.
    def render(io, el : Element, *, styled : Bool = true) : Nil
      each_line(el) do |line|
        line.frags.each do |frag|
          unless styled
            io << frag.caption
            next
          end

          case frag.style
          in .normal?
          in .emphasis?
            io << "\e[0;1;97m" # reset, bold, white
          in .italic?
            io << "\e[0;3m" # reset, italic
          in .dim?
            io << "\e[0;90m" # reset, dark gray
          in .code?
            io << "\e[0;48;5;233m" # reset, set background, dark gray
          in .success?
            io << "\e[0;32m" # reset, green
          in .failure?
            io << "\e[0;31m" # reset, red
          in .failure_emphasis?
            io << "\e[0;1;31m" # reset, bold, red
          in .error?
            io << "\e[0;93m" # reset, dark yellow
          in .error_emphasis?
            io << "\e[0;1;93m" # reset, bold, dark yellow
          in .info?
            io << "\e[0;34m" # reset, blue
          in .link?
            io << "\e[0;97;4m" # reset, white, underline
          end

          io << frag.caption
          io << "\e[0m" # reset
        end

        io.puts
      end
    end
  end
end
