module Ww::MxRack
  # There was previously a separate `textoscope` node but I decided to go with `?nav[s]`
  # and `?aim` instead. Since I can't come up with another fitting name for what's happening
  # in this module, it's better to just keep the name `textoscope` for now.
  module Textoscope
    extend self

    # NOTE: The whole affair with TextTree and Array(TextUnit) is a rather crude and
    # inefficient way to implement the textoscope. I can't think of a better way
    # *that is immediately possible*, though; smarter solutions seem to require
    # [substantial or semi-substantial] modifications to Scenery. I don't want to
    # do them now; maybe later.

    alias TextLine = Array(TextWord)
    alias TextWord = Array(Glyph | Beam)

    defrecord TextTree, lines : Array(TextLine), word_count : UInt32, unit_count : UInt32

    struct TextTree
      def line_count : UInt32
        @lines.size.to_u32
      end
    end

    # Builds a tree of lines for *text* at the given *width*; each line consisting
    # of words, and each word of `Glyph`s or `Beam`s.
    def wrap(text : Scenery::ShapedText, width : Magnitude) : TextTree
      lines = [] of TextLine
      word_count = 0u32
      unit_count = 0u32
      last_index = 0u32

      Scenery.line_wrap(text, at: width, collapse: false) do |raw_line|
        line = [] of TextWord

        Scenery.each_word(raw_line, collapse: false) do |raw_word|
          next if raw_word.items.empty? # ?!

          word = [] of Glyph | Beam

          raw_word.items.each do |item|
            case item
            in Scenery::IBeam
              word << Beam.new
              unit_count += 1
            in Scenery::Endl, Scenery::ShapedStyledGlyph # Endl is for \n
              if text.selections.any? { |sel| sel.focus == item.grapheme_index && sel.anchor != sel.focus }
                word << Beam.new
                unit_count += 1
              end
              word << Glyph.new(item.grapheme_index.to_u32, item.grapheme_chrsize.to_u32)
              unit_count += 1
              last_index = item.grapheme_index + item.grapheme_chrsize
            end
          end

          line << word
          word_count += 1
        end

        lines << line
      end

      if text.selections.any? { |sel| sel.focus >= last_index && sel.anchor != sel.focus }
        unless line = lines.last?
          line = [] of TextWord
          lines << line
        end
        unless word = line.last?
          word = [] of Glyph | Beam
          line << word
          word_count += 1
        end
        word << Beam.new
        unit_count += 1
      end

      TextTree.new(lines, word_count, unit_count)
    end

    alias TextUnit = Glyph | Beam | VisualLineBegin | VisualLineEnd | WordBegin | WordEnd

    defrecord Glyph, grapheme_index : UInt32, chrsize : UInt32, copying: true
    defrecord Beam
    defrecord VisualLineBegin
    defrecord VisualLineEnd
    defrecord WordBegin
    defrecord WordEnd

    # Turns *tree* into a representation more suitable for linear navigation:
    # a flat sequence of `TextUnit`s, virtual (e.g. `VisualLineBegin`) and concrete
    # (e.g. `Glyph`).
    def flatten(tree : TextTree) : Array(TextUnit)
      # 2x for each line, for VisualLineBegin and VisualLineEnd.
      # 2x for each word, for WordBegin and WordEnd.
      # The rest for units.
      units = Array(TextUnit).new(2*tree.line_count + 2*tree.word_count + tree.unit_count)

      tree.lines.each_with_index do |line, line_index|
        units << VisualLineBegin.new
        line.each(within: ...-1) do |word|
          units << WordBegin.new
          word.each do |unit|
            units << unit
          end
          units << WordEnd.new
        end

        unless (last_word = line.last?) && (last_unit = last_word.last?)
          units << VisualLineEnd.new
          next
        end

        # Put word separator glyphs -- e.g. newline or whitespace -- after
        # the current visual line, but also before the next visual line.
        if last_unit.is_a?(Glyph) && line_index < tree.lines.size - 1
          units << WordBegin.new
          units.concat(last_word[...-1])
          units << WordEnd.new
          units << VisualLineEnd.new
          units << last_unit
          next
        end

        units << WordBegin.new
        units.concat(last_word)
        units << WordEnd.new
        units << VisualLineEnd.new
      end

      units
    end

    # Returns the flat sequence of `TextUnit`s for *text* at the given *width*.
    def flatten(text : Scenery::ShapedText, width : Magnitude) : Array(TextUnit)
      flatten(wrap(text, width))
    end

    struct Cursor
      # Returns the number of runes (`Char`s) this cursor stepped over from its
      # original index (the one it was initialized with). Negative if to the left
      # of the index, positive if to the right.
      getter stride : Int32

      getter index : UInt32

      # :nodoc:
      def initialize(@units : Array(TextUnit), @index : UInt32, @stride : Int32)
      end

      def self.new(units : Array(TextUnit), index : Int) : Cursor
        new(units, index.to_u32, stride: 0)
      end

      def before_first? : Bool
        @index.zero?
      end

      def after_last? : Bool
        @index == @units.size - 1
      end

      def before?(other : Cursor) : Bool
        @index < other.index
      end

      def after?(other : Cursor) : Bool
        @index > other.index
      end

      def at?(goal : T.class) : Bool forall T
        {% unless T < TextUnit %}
          {% T.raise "expected TextUnit, not #{T}" %}
        {% end %}

        @units[@index].is_a?(T)
      end

      def before?(goal : T.class) : Bool forall T
        pred.at?(goal)
      end

      def after?(goal : T.class) : Bool forall T
        succ.at?(goal)
      end

      def pred : Cursor
        if before_first?
          return self
        end

        # G0 I G1 -> I G0 G1
        # ^
        unit = @units[@index - 1]
        size = unit.is_a?(Glyph) ? unit.chrsize : 0u32
        Cursor.new(@units, @index - 1, @stride - size)
      end

      def succ : Cursor
        if after_last?
          return self
        end

        # G0 I G1 -> G0 G1 I
        #      ^
        unit = @units[@index + 1]
        size = unit.is_a?(Glyph) ? unit.chrsize : 0u32
        Cursor.new(@units, @index + 1, @stride + size)
      end

      def goto(goal : TextUnit.class) : Cursor
        cursor = Cursor.new(@units, @index)

        until cursor.after_last? || (cursor.stride != 0 && cursor.at?(goal))
          cursor = cursor.succ
        end

        Cursor.new(@units, cursor.index, @stride + cursor.stride)
      end

      def reverse_goto(goal : TextUnit.class) : Cursor
        cursor = Cursor.new(@units, @index)

        until cursor.before_first? || (cursor.stride != 0 && cursor.at?(goal))
          cursor = cursor.pred
        end

        Cursor.new(@units, cursor.index, @stride + cursor.stride)
      end

      def reverse_goto_unless(goal : TextUnit.class) : Cursor
        cursor = Cursor.new(@units, @index)

        until cursor.before_first? || cursor.at?(goal)
          cursor = cursor.pred
        end

        Cursor.new(@units, cursor.index, @stride + cursor.stride)
      end

      def skip(chrsize : UInt32, stopper : TextUnit.class) : Cursor
        cursor = Cursor.new(@units, @index)

        until cursor.stride >= chrsize || cursor.after_last? || cursor.at?(stopper)
          assert cursor.stride >= 0
          cursor = cursor.succ
        end

        Cursor.new(@units, cursor.index, @stride + cursor.stride)
      end
    end

    # :nodoc:
    def navs(node : Scenery::Inert | Scenery::RectShape | Scenery::HitArea | Scenery::Pending | Scenery::Img | Scenery::Svg | Scenery::IconGlyph, box : Scenery::Box) : Slice(Term)
      Slice(Term).empty
    end

    # :nodoc:
    def navs(node : Scenery::ShapedText, box : Scenery::Box) : Slice(Term)
      units = flatten(node, box.bounds.w)
      units.to_compact_readonly_slice do |unit, index|
        next unless unit.is_a?(Beam)

        cursor = Cursor.new(units, index)
        column = cursor.reverse_goto_unless(VisualLineBegin).stride.abs

        left_rune_stride = cursor.reverse_goto(Glyph).stride
        left_word_stride = cursor.reverse_goto(WordBegin).stride
        behind_stride = cursor.reverse_goto(VisualLineBegin).stride
        above_stride = cursor
          .reverse_goto_unless(VisualLineBegin)
          .reverse_goto(VisualLineBegin)
          .skip(column.to_u32, stopper: VisualLineEnd)
          .stride

        right_rune_stride = cursor.goto(Glyph).stride
        right_word_stride = cursor.goto(WordEnd).stride
        ahead_stride = cursor.goto(VisualLineEnd).stride
        below_stride = cursor
          .goto(VisualLineBegin)
          .skip(column.to_u32, stopper: VisualLineEnd)
          .stride

        l = Term[rune: left_rune_stride, word: left_word_stride, behind: behind_stride, above: above_stride]
        r = Term[rune: right_rune_stride, word: right_word_stride, ahead: ahead_stride, below: below_stride]
        Term.of(l, :"<I>", r)
      end
    end

    # :nodoc:
    def navs(node : Scenery::Vantage | Scenery::Padding | Scenery::Align | Scenery::XYStack | Scenery::ZStack | Scenery::Composite | Scenery::TransformMatrix | Scenery::Clip | Scenery::Gate, box : Scenery::Box) : Slice(Term)
      navs(node.children, box)
    end

    private def navs(nodes : Slice(Scenery::AimedNode), box : Scenery::Box) : Slice(Term)
      nav = Pf::Kit.stack_array(Term)
      nodes.zip(box.children) do |node, node_box|
        nav.concat(navs(node, node_box))
      end
      nav.to_unsafe_readonly_slice!
    end

    {% if flag?(:docs) %}
      # Returns zero or more I-beam navigation dicts for a text *node* or text children of
      # *node*. The order of navigation dicts matches text I-beam order (which is itself
      # a complicated and, unfortunately, undocumented affair, due to bidi writing among
      # other things); and then text order if there is more than one `text` in *node*.
      def navs(node : Scenery::AimedNode, box : Scenery::Box) : Slice(Term)
      end
    {% end %}

    def lookaround(text : Pf::GraphemeSeln, pivot : Int32) : {beam: Pf::GraphemeSeln, word: Pf::GraphemeSeln, line: Pf::GraphemeSeln}
      l = text.select(0...pivot)
      r = text.select(pivot...text.size)

      current = l
      until current.empty?
        chr = current.last.grapheme.to_s[0]
        break if chr.hspace? || Nitrene::WordTokenizer.wsep?(chr)
        break if chr == '\n' # line separator

        current = current.prior
      end

      word_prefix = current.after_end.through(l.after_end)

      until current.empty?
        chr = current.last.grapheme.to_s[0]
        break if chr == '\n'

        current = current.prior
      end

      line_prefix = current.after_end.through(l.after_end)

      current = r
      until current.empty?
        chr = current.first.grapheme.to_s[0]
        break if chr.hspace? || Nitrene::WordTokenizer.wsep?(chr)
        break if chr == '\n'

        current = current.rest
      end

      word_suffix = r.before_start.through(current.before_start)

      until current.empty?
        chr = current.first.grapheme.to_s[0]
        break if chr == '\n'

        current = current.rest
      end

      line_suffix = r.before_start.through(current.before_start)

      word = word_prefix + word_suffix
      line = line_prefix + line_suffix

      {beam: text.select(pivot...pivot), word: word, line: line}
    end

    alias TextAim = QualifiedTextAim | UnqualifiedTextAim

    defrecord UnqualifiedTextAim, description : Term
    defrecord QualifiedTextAim, addr : D7::NodeAddr, description : Term

    # :nodoc:
    def text_aims(node : Scenery::AimedNode, box : Scenery::Box, hit : Scenery::HitEmpty | Scenery::HitLeaf | Scenery::HitAreaLeaf) : Slice(TextAim)
      Slice(TextAim).empty
    end

    # :nodoc:
    def text_aims(node : Scenery::AimedNode, box : Scenery::Box, hit : Scenery::HitTextLeaf) : Slice(TextAim)
      assert node.is_a?(Scenery::ShapedText)

      lka = Textoscope.lookaround(node.caption, hit.focus)

      description = Term.of(
        rune: {lka[:beam].begin, :"..<", lka[:beam].end},
        word: {lka[:word].begin, :"..<", lka[:word].end},
        line: {lka[:line].begin, :"..<", lka[:line].end},
      )
      Slice(TextAim).of(UnqualifiedTextAim.new(description))
    end

    # :nodoc:
    def text_aims(node : Scenery::ZStack, box : Scenery::Box, hit : Scenery::HitGroup) : Slice(TextAim)
      aims = Pf::Kit.stack_array(TextAim, 8)

      node.children.zip(box.children, hit.children) do |child, box_child, hit_child|
        box_child = box_child.translate(box.bounds.tl)
        aims.concat(text_aims(child, box_child, hit_child))
      end

      pass do
        next unless info = node.info
        next unless value = info.pairs[:"?aim"]?.as_d?
        next unless addr = D7::NodeAddr.new?(value)

        aims.map! do |aim|
          case aim
          in QualifiedTextAim   then aim
          in UnqualifiedTextAim then QualifiedTextAim.new(addr, aim.description)
          end
        end
      end

      aims.to_unsafe_readonly_slice!
    end

    # :nodoc:
    def text_aims(node : Scenery::AimedNode, box : Scenery::Box, hit : Scenery::HitGroup) : Slice(TextAim)
      assert node.responds_to?(:children)

      aims = Pf::Kit.stack_array(TextAim, 8)

      node.children.zip(box.children, hit.children) do |child, box_child, hit_child|
        box_child = box_child.translate(box.bounds.tl)
        aims.concat(text_aims(child, box_child, hit_child))
      end

      aims.to_unsafe_readonly_slice!
    end
  end
end
