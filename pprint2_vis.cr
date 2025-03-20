# TODO: in the future, µsoma will be a system that takes events as input and produces
# draw commands as output. Both are Terms. However, right now, nothing works this way;
# everything is mixed and tightly coupled to Termbox.

require "./pprint2"
require "./colors"
require "./delta7_proto2"
require "./baz5_editor"
require "./libtermbox2"

BORDERSETS = {
  rounded: {
    tl: '╭',
    tr: '╮',
    bl: '╰',
    br: '╯',
    t: '─',
    hr: '─',
    lhr: '├',
    rhr: '┤',
    b: '─',
    l: '│',
    r: '│',
  },
  square: {
    tl: '┌',
    tr: '┐',
    bl: '└',
    br: '┘',
    t: '─',
    hr: '─',
    lhr: '├',
    rhr: '┤',
    b: '─',
    l: '│',
    r: '│',
  }
}

class Screen
  @bg : Termbox::Color
  @fg : Termbox::Color

  getter vw : Int32
  getter vh : Int32

  alias Layer = Hash({Int32, Int32}, {Char, Termbox::Color, Termbox::Color})

  getter fg0
  getter bg0

  def initialize(@bg0 : {UInt8, UInt8, UInt8}, @fg0 : {UInt8, UInt8, UInt8}, @vw, @vh)
    @bg = Termbox::Color.rgb(*bg0)
    @fg = Termbox::Color.rgb(*fg0)

    @layers = [] of {Layer, Int32}
    @overlays = Hash({Int32, Int32}, {Termbox::Color, Termbox::Color}).new

    @max_x = 0
    @max_y = 0
  end

  def resize(@vw, @vh)
  end

  def clear : Nil
    @overlays.clear
    @layers.clear
    @max_x = 0
    @max_y = 0
    Termbox.clear(fg: @fg, bg: @bg)
  end

  def set(ch : Char, x, y, fg = nil, bg = nil, z_index = 0) : Nil
    index = @layers.bsearch_index { |(_, candidate_z_index)| candidate_z_index >= z_index }

    if index && (layer_data = @layers[index]?)
      layer, layer_z = layer_data
      unless layer_z == z_index
        # Un-define so the latter code creates the layer.
        layer = nil
      end
    end

    unless layer
      index ||= @layers.size
      layer = Layer.new
      @layers.insert(index, {layer, z_index})
    end

    layer[{x, y}] = {ch, fg ? Termbox::Color.rgb(*fg) : @fg, bg ? Termbox::Color.rgb(*bg) : @bg}

    @max_x = Math.max(@max_x, x)
    @max_y = Math.max(@max_y, y)
  end

  def overlay(x, y, fg = nil, bg = nil)
    @overlays[{x, y}] = {fg ? Termbox::Color.rgb(*fg) : @fg, bg ? Termbox::Color.rgb(*bg) : @bg}

    @max_x = Math.max(@max_x, x)
    @max_y = Math.max(@max_y, y)
  end

  def max_scroll_x : Int32
    @max_x < @vw ? 0 : (@max_x - @vw) + 1 + SCREEN_PX*2
  end

  def max_scroll_y : Int32
    @max_y < @vh ? 0 : (@max_y - @vh) + 1 + SCREEN_PY*2
  end

  def present(px = 0, py = 0, scroll_x = 0, scroll_y = 0)
    clip_w = @vw
    clip_h = @vh

    clip_w -= px*2
    clip_h -= py*2

    @layers.each do |(layer, _)|
      layer.each do |(x, y), (char, fg, bg)|
        # Offset by padding
        screen_x = x + px
        screen_y = y + py

        next unless scroll_x <= screen_x <= scroll_x + clip_w
        next unless scroll_y <= screen_y <= scroll_y + clip_h

        fg, bg = @overlays[{x, y}]? || {fg, bg}

        Termbox.set(char, x: screen_x - scroll_x, y: screen_y - scroll_y, fg: fg, bg: bg)
      end
    end

    @overlays.each do |(x, y), (fg, bg)|
      next if @layers.any? { |layer, _| layer.has_key?({x, y}) }

      # Offset by padding
      screen_x = x + px
      screen_y = y + py

      next unless scroll_x <= screen_x <= scroll_x + clip_w
      next unless scroll_y <= screen_y <= scroll_y + clip_h

      Termbox.set(' ', x: screen_x - scroll_x, y: screen_y - scroll_y, fg: fg, bg: bg)
    end

    Termbox.present
  end
end

struct Cursor
  include Feature

  private def cursor_block(lhs : Term, rhs : Term)
    Term.of(:block, Term[:cursor, lhs, rhs], w: lhs.charcount + rhs.charcount, h: 1)
  end

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      # One general suggestion. Show it and its intro.
      matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions: (suggestions/list () ((name_string intro_string)) ()))} do
        suggestions_node = Term::Dict.build do |commit|
          commit << :suggestion
          commit.with(:shl, rhs.charcount)
          commit << name << intro
        end

        Term.of(:row,
          cursor_block(lhs, rhs),
          Term.of(:"block/floating", suggestions_node),
          Term.of(:frag, postfix))
      end

      # More than one general suggestion. Show all of them.
      matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions: (suggestions/list above←(_*) visible←((%past (_string _string) min: 1)) below←(_*)))} do
        suggestions_node = Term::Dict.build do |commit|
          commit << :suggestions
          commit.with(:shl, rhs.charcount)

          max = above.itemsize + visible.itemsize + below.itemsize
          if max > 5
            commit << String.build do |io|
              if above.itemsize > 0
                io << "▴"
              end
              io << above.itemsize << ".." << max - visible.itemsize
              if above.itemsize + visible.itemsize + 1 <= max
                io << "▾"
              end
            end
          end

          visible.items.each do |(name, _)|
            commit << name
          end
        end

        Term.of(:row,
          cursor_block(lhs, rhs),
          Term.of(:"block/floating", suggestions_node),
          Term.of(:frag, postfix))
      end

      # Group suggestion
      matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions_: (suggestions/group prefix←(_*) suffix←((head_string body_string) _*)))} do
        suggestions_node = Term::Dict.build do |commit|
          commit << :suggestion
          commit.with(:shl, rhs.charcount)

          commit << String.build do |io|
            if prefix.size + suffix.size > 1
              io << "["
              io << "▴" if prefix.size > 0
              io << prefix.size + 1 # Count from 1
              io << ".."
              io << prefix.size + suffix.size
              io << "▾" if suffix.size > 1
              io << "]"
            end
            io << head.to(String)
          end

          commit << body
        end

        Term.of(:row,
          cursor_block(lhs, rhs),
          Term.of(:"block/floating", suggestions_node),
          Term.of(:frag, postfix))
      end

      matchpi %{[lhs_string | rhs_string (_*) @user]} do
        Term.of(:row,
          cursor_block(lhs, rhs),
          Term.of(:frag, postfix))
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct Col
  include Feature

  def call(ctx, term, postfix, head, rest)
    Term.of_case(term) do
      matchpi %{(col (%plural children min: 2) ¦ gap⋮ 0)} do |children|
        continue if Rhodium.cursordepth(term, pairspart: true) == 1 # Allow cursor inside children.

        Term::Dict.build do |commit|
          commit << :col
          commit.with(:gap, gap)
          children.items.each_with_last do |child, last|
            commit << head.call(ctx, child, last ? postfix : "")
          end
        end
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct Row
  include Feature

  def call(ctx, term, postfix, head, rest)
    Term.of_case(term) do
      matchpi %{(row (%plural children min: 2) ¦ gap⋮ 0)} do |children|
        continue if Rhodium.cursordepth(term, pairspart: true) == 1 # Allow cursor inside children.

        Term::Dict.build do |commit|
          commit << :row
          commit.with(:gap, gap)
          children.items.each_with_last do |child, last|
            commit << head.call(ctx, child, last ? postfix : "")
          end
        end
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct Button
  include Feature

  private def button(term, caption : Term)
    if caption.type.string?
      caption_string = caption.to(String)
    else
      # TODO: use pretty print with forced inline
      caption_string = ML.display(caption, endl: false).gsub(/\s+/, ' ')
    end

    Term.of(:block,
      Term[:button, caption_string,
        enabled: true,
        mailbox: term[:mailbox]?],
      w: caption_string.size + 2,
      h: 1)
  end

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi(
        %{[button caption_ to @_ (_*)]},
        %{[button caption_ to @_ (_*) waiting @_]},
      ) do
        continue unless Rhodium.cursordepth(term, pairspart: true) == -1

        Term.of(:row, button(term, caption), Term.of(:frag, postfix))
      end

      matchpi(
        %{[button caption_ as _ to @_ (_*)]},
        %{[button caption_ as _ to @_ (_*) waiting @_]},
      ) do
        continue unless Rhodium.cursordepth(term, pairspart: true) == -1

        Term.of(:row, button(term, caption), Term.of(:frag, postfix))
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct Comment
  include Feature

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi %{(comment desc_string)} do
        wrapped_desc = wrap(desc.to(String), 60).chomp

        w = wrapped_desc.each_line.max_of? { |line| line.size + 3 } || 3 # Do not forget ";; "
        h = Math.max(wrapped_desc.each_line.size, 1)

        Term.of(:row, Term.of(:block, Term[:comment, wrapped_desc], w: w, h: h), Term.of(:frag, postfix))
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

# *Annotation* involves writing down the keypaths of things, etc., so that we can
# in the future, point back from a UI-tree to the S-expression that produced it
# in the D7 document.
def annotate(document : Term::Dict)
  nodepath = Stack(Int32).new

  while Rhodium.successor?(document, nodepath)
    node = Rhodium.follow(document, nodepath)

    Term.case(node) do
      matchpi(
        %{[button _ to @_ (_*)]},
        %{[button _ to @_ (_*) waiting @_]},
      ) do
        continue unless Rhodium.cursordepth(node, pairspart: true) == -1

        document = Rhodium.assign(document, nodepath, Term.of(node.with(:mailbox, Term[nodepath].append(4))))
      end

      matchpi(
        %{[button _ as _ to @_ (_*)]},
        %{[button _ as _ to @_ (_*) waiting @_]},
      ) do
        continue unless Rhodium.cursordepth(node, pairspart: true) == -1

        document = Rhodium.assign(document, nodepath, Term.of(node.with(:mailbox, Term[nodepath].append(6))))
      end

      otherwise { }
    end
  end

  document
end

def in_bounds?(ctx, unode, x, y)
  return if x.negative? || y.negative?

  # We assume unode's origin is (0; 0), since we're translating x, y in `probe`.
  # Thus its corner will be its width/height.
  w, h = measure(ctx, unode)

  0 <= x < w && 0 <= y < h
end

def probe(ctx, unode : Term, x : Int32, y : Int32, fn)
  return unless in_bounds?(ctx, unode, x, y)

  fn.call(unode)

  Term.case(unode) do
    # Leaves
    matchpi %{[frag _]} { }
    matchpi %{[block _]} { }
    matchpi %{[block/floating _]} { }
    matchpi %{[row]} { }
    matchpi %{[col]} { }

    matchpi %{[longer child_]} do
      probe(ctx, child, x, y, fn)
    end

    matchpi %{(indented child_ ¦ by: n←(%number +i32))} do
      probe(ctx, child, x - n.to(Int32), y, fn)
    end

    matchpi %[(row children_* ¦ gap: (%optional 0 gap←(%number +i32)))] do
      prev_floating = nil
      (0...children.itemsize).each do |index|
        child = children[index]
        child_width, _ = measure(ctx, child)
        floating = floating?(child)
        x -= gap.to(Int32) if index > 0 && !(floating || prev_floating)
        prev_floating = floating
        probe(ctx, child, x, y, fn)
        x -= child_width
      end
    end

    matchpi %[(col children_* ¦ gap: (%optional 0 gap←(%number +i32)))] do
      prev_floating = nil
      (0...children.itemsize).each do |index|
        child = children[index]
        _, child_height = measure(ctx, child)
        floating = floating?(child)
        y -= gap.to(Int32) if index > 0 && !(floating || prev_floating)
        prev_floating = floating
        probe(ctx, child, x, y, fn)
        y -= child_height
      end
    end
  end
end

def probe(ctx, unode, x, y, &fn : Term ->)
  probe(ctx, unode, x, y, fn)
end

SCREEN_PX = 2
SCREEN_PY = 1

class Soma
  def initialize
    @running = true
    @mt = ExecutionContext::MultiThreaded.new("Soma", 2)
    @nitrene = Nitrene::JobContext.new
  end

  private def screen(& : Screen ->) : Nil
    Termbox.init do
      Termbox.input_mode = Termbox::InputMode::Alt
      Termbox.output_mode = Termbox::OutputMode::Truecolor

      screen = Screen.new(
        bg0: Colors[:"gray-900"],
        fg0: Colors[:"gray-300"],
        vw: Termbox.width,
        vh: Termbox.height,
      )

      yield screen
    end
  end

  private def measure_suggestion_box(wrapped_sections : Enumerable(String), *, px = 0, py = 0) : {Int32, Int32}
    # Compute width and height.
    w = w1 = 0
    h = 0

    wrapped_sections.each_with_index do |section, index|
      h += 1 if index > 0 # Have a gap

      section.each_char do |char|
        case char
        when '\n'
          w = Math.max(w, w1)
          w1 = 0
          h += 1
        when '*'
          # We use asterisks for emphasis. Do not count them in width.
          next
        else
          w1 += 1
        end
      end

      w = Math.max(w, w1)
      w1 = 0
      h += 1
    end

    # Add space for borders on each side. I cannot reason about it
    # if it's not border-box :)
    w += 2
    h += 2

    # Add space for padding.
    w += px*2
    h += py*2

    {w, h}
  end

  private def compose_suggestion_box(wrapped_sections : Enumerable(String), w, h, *, px = 1, py = 0, &)
    borderset = BORDERSETS[:rounded]

    # Draw border

    # Draw corners.
    yield borderset[:tl], 0, 0, false
    yield borderset[:tr], w - 1, 0, false
    yield borderset[:bl], 0, h - 1, false
    yield borderset[:br], w - 1, h - 1, false

    # Top, bottom
    (1...w - 1).each do |i|
      yield borderset[:t], i, 0, false
      yield borderset[:b], i, h - 1, false
    end

    # Left, right
    (1...h - 1).each do |j|
      yield borderset[:l], 0, j, false
      yield borderset[:r], w - 1, j, false
    end

    # Fill with empty space.
    (1...h - 1).each do |j|
      (1...w - 1).each do |i|
        yield ' ', i, j, false
      end
    end

    # Now put text into the box. Do not forget we have borders! And padding!
    i = oi = 1 + px
    j = 1 + py
    em = false
    wrapped_sections.each_with_index do |section, index|
      if index > 0
        # Draw horizontal separator instead of whitespace gap
        (1...w - 1).each do |k|
          yield borderset[:hr], k, j, false
        end
        # Stomp over the border we've drawn before with lhr, rhr
        yield borderset[:lhr], 0, j, false
        yield borderset[:rhr], w - 1, j, false

        j += 1
      end

      section.each_char do |char|
        case char
        when '\n'
          i = oi
          j += 1
        when ' '
          i += 1
        when '*'
          em = !em
        else
          yield char, i, j, em
          i += 1
        end
      end

      i = oi
      j += 1
    end
  end

  private def draw_block(screen, block, x, y)
    Term.case(block) do
      matchpi %{[cursor lhs_string rhs_string]} do
        fg = Colors[:"gray-200"]
        bg = Colors[:"gray-700"]

        lhs.to(String).each_char do |char|
          screen.set(char, x, y, fg, bg)
          x += 1
        end

        screen.overlay(x, y, bg, fg)

        rhs.to(String).each_char_with_index do |char, index|
          screen.set(char, x, y, fg, bg)
          x += 1
        end
      end

      matchpi %{[comment desc_string]} do
        fg = Colors[:"gray-400"]
        bg = screen.bg0

        ox = x

        screen.set(';', x, y, fg, bg)
        x += 1
        screen.set(';', x, y, fg, bg)
        x += 2

        desc.to(String).each_char do |char|
          if char == '\n'
            x = ox
            y += 1
            screen.set(';', x, y, fg, bg)
            x += 1
            screen.set(';', x, y, fg, bg)
            x += 2
            next
          elsif char == '\t'
            x += 2
            next
          end
          screen.set(char, x, y, fg, bg)
          x += 1
        end

        y += 1

        {x, y}
      end

      matchpi %{(suggestions suggestions_string+ ¦ () shl_: (%number +i32))} do
        # Save. Suggestions "float". We'll restore later.
        x0 = x
        y0 = y

        ox = x - shl.to(Int32)

        sections = {suggestions.items.join('\n', &.to(String))}
        suggestions_w, suggestions_h = measure_suggestion_box(sections, px: 1, py: 0)

        offset_x = (screen.vw - SCREEN_PX*2 - 1) - (ox + suggestions_w)

        if ox <= (screen.vw - SCREEN_PX*2 - 1) && offset_x < 0
          ox += offset_x
        end

        # Overflows, flip
        if y + 1 + suggestions_h >= (screen.vh - SCREEN_PY*2 - 1)
          y -= suggestions_h
        else
          y += 1
        end

        fg_em = Colors[:"blue-400"]

        compose_suggestion_box(sections, suggestions_w, suggestions_h) do |char, i, j, em|
          screen.set(char, ox + i, y + j, em ? fg_em : screen.fg0, screen.bg0, 9)
        end

        x = x0
        y = y0
      end

      matchpi %{(suggestion name_string desc_string ¦ () shl_: (%number +i32))} do
        # Save. Suggestions "float". We'll restore later.
        x0 = x
        y0 = y

        ox = x - shl.to(Int32)

        fg_em = Colors[:"blue-400"]

        desc_punct = desc.to(String)
        if desc_punct[-1]?.try(&.letter?)
          desc_punct += '.'
        end

        sections = {name.to(String), desc_punct}
        wrapped_sections = sections.map { |section| wrap(section, maxwidth: (screen.vw - SCREEN_PX*2 - 1).clamp(24..60)).chomp } # ?!
        suggestions_w, suggestions_h = measure_suggestion_box(wrapped_sections, px: 1, py: 0)

        offset_x = (screen.vw - SCREEN_PX*2 - 1) - (ox + suggestions_w)

        if ox <= (screen.vw - SCREEN_PX*2 - 1) && offset_x < 0
          ox += offset_x
        end

        # If overflows, flip
        if y + 1 + suggestions_h >= (screen.vh - SCREEN_PY*2 - 1) && (y - suggestions_h) > 0
          y -= suggestions_h
        else
          y += 1
        end

        compose_suggestion_box(wrapped_sections, suggestions_w, suggestions_h) do |char, i, j, em|
          screen.set(char, ox + i, y + j, em ? fg_em : screen.fg0, screen.bg0, 9)
        end

        x = x0
        y = y0
      end

      matchpi %{(button caption_string ¦ _ enabled⋮ true)} do
        if enabled.true?
          fg = Colors[:"gray-200"]
          bg = Colors[:"gray-600"]
        else
          fg = Colors[:"gray-300"]
          bg = Colors[:"gray-800"]
        end

        button = " " + caption.to(String) + " "
        button.each_char do |char|
          screen.set(char, x, y, fg, bg)
          x += 1
        end
      end

      otherwise {}
    end

    {x, y}
  end

  private def draw(screen : Screen, ctx : DisplayContext, node : Term, x, y)
    Term.case(node) do
      matchpi %{(frag chars_string ¦ _ tag: number)} do
        chars.to(String).each_char do |char|
          screen.set(char, x, y, fg: Colors[:"violet-400"])
          x += 1
        end
      end

      matchpi %{(frag chars_string ¦ _ tag: string)} do
        chars.to(String).each_char do |char|
          screen.set(char, x, y, fg: Colors[:"yellow-600"])
          x += 1
        end
      end

      matchpi %{(frag chars_string ¦ _ tag: symbol)} do
        chars.to(String).each_char do |char|
          screen.set(char, x, y, fg: Colors[:"gray-200"])
          x += 1
        end
      end

      matchpi %{(frag chars_string ¦ _ tag: boolean)} do
        chars.to(String).each_char do |char|
          screen.set(char, x, y, fg: Colors[:"orange-600"])
          x += 1
        end
      end

      matchpi %{(frag chars_string ¦ _ tag: edge)} do
        chars.to(String).each_char do |char|
          screen.set(char, x, y, fg: Colors[:"green-400"])
          x += 1
        end
      end

      matchpi %{[frag chars_string]} do
        chars.to(String).each_char do |char|
          if char.in?('(', ')', '{', '}', '[', ']', '¦')
            screen.set(char, x, y, fg: Colors[:"gray-400"])
          else
            screen.set(char, x, y)
          end
          x += 1
        end
      end

      matchpi %{[block term_]}, %{[block/floating term_]} do
        x, y = draw_block(screen, term, x, y)
      end

      matchpi %[(row children_* ¦ _ gap: (%optional 0 gap←(%number +i32)))] do
        prev_floating = nil
        children.items.each_with_index do |child, index|
          floating = floating?(child)
          x += gap.to(Int32) if index > 0 && !(prev_floating || floating)
          prev_floating = floating
          draw(screen, ctx, child, x, y)
          child_width, _ = measure(ctx, child)
          x += child_width
        end
      end

      matchpi %[(col children_* ¦ _ gap: (%optional 0 gap←(%number +i32)))] do
        prev_floating = nil
        children.items.each_with_index do |child, index|
          floating = floating?(child)
          y += gap.to(Int32) if index > 0 && !(prev_floating || floating)
          prev_floating = floating
          draw(screen, ctx, child, x, y)
          _, child_height = measure(ctx, child)
          y += child_height
        end
      end

      matchpi %[(indented child_ ¦ _ by: (%optional 1 n←(%number +i32)))] do
        draw(screen, ctx, child, x + n.to(Int32), y)
      end
    end
  end

  # In order to draw a document on the screen, you first have to explain
  # to the draw method why do you want to draw, so that it chooses the appropriate
  # (and appropriately expensive) way to draw.
  enum DrawReason : UInt8
    Resize
    Scroll
    Forced
    CanDraw
  end

  @prev_draw_at = 0.milliseconds
  @currently_visible_document : Term::Dict? = nil
  @scroll_x = 0
  @scroll_y = 0

  # TODO: we should pretty print into an intermediate tree that contains
  # widths and heights and positions of everything! This way we won't need
  # display context and visible ui tree here!
  @ctx : DisplayContext?
  @visible_ui_tree = Term.of

  MAIN_CHAIN = ML::Display::MAIN_CHAIN.prepend(Cursor.new, Button.new, Col.new, Row.new, Comment.new)

  private def draw(screen : Screen, document : Term::Dict, reason : DrawReason) : Nil
    current_draw_time = Time.monotonic

    # Handle CanDraw
    if reason.can_draw? && current_draw_time - @prev_draw_at < 30.milliseconds
      return # Skip drawing, it's not the time.
    end

    @prev_draw_at = current_draw_time

    # Handle Scroll
    if reason.scroll?
      Termbox.clear # ?!
      screen.present(px: SCREEN_PX, py: SCREEN_PY, scroll_x: @scroll_x, scroll_y: @scroll_y)
      return
    end

    if @debug
      next_visible_document = document
    else
      next_visible_document = D7.visible(document)
    end

    # Handle Resize
    if !reason.resize? && @currently_visible_document == next_visible_document
      return # Skip drawing, visible document did not change.
    end

    @currently_visible_document = next_visible_document

    # Handle Forced
    screen.clear

    maxchars = (screen.vw * 0.8).floor.to_i

    annotated = annotate(next_visible_document)

    @ctx = ctx = DisplayContext.new(maxchars, maxchars*2, MAIN_CHAIN)

    # TODO: can we somehow standardize this in the pretty printer?

    # Allow only DictAligned for the document itself.
    tree = LayoutSet::All.thunk(Term.of(annotated), "", LayoutSet::DictAligned)
    flat, _ = flatten(ctx, tree)

    @visible_ui_tree = flat

    draw(screen, ctx, flat, 0, 0)

    screen.present(px: SCREEN_PX, py: SCREEN_PY, scroll_x: @scroll_x, scroll_y: @scroll_y)
  end

  private def step(screen : Screen, document document0 : Term::Dict) : Term::Dict
    document1 = peek(screen, document0)

    if @should_draw
      draw(screen, document1, :can_draw)
      @should_draw = false
    end

    document1
  end

  private def step(screen : Screen)
    D7::Step.new do |document|
      document = step(screen, document)

      # We do not modify the document and therefore we never trigger
      # a transition.
      {document, false}
    end
  end

  class KeyboardInterrupt < Exception
  end

  @debug = false

  private def handle(screen : Screen, document document0 : Term::Dict, event : Termbox::Event, *, settled : Bool) : Term::Dict
    document1 = document0

    case event.type
    when .resize?
      screen.resize(event.resize_w, event.resize_h)

      @scroll_x = Math.min(screen.max_scroll_x, @scroll_x)
      @scroll_y = Math.min(screen.max_scroll_y, @scroll_y)

      draw(screen, document0, :resize) if settled
    when .mouse?
      case event.key
      when .mouse_wheel_up?
        @scroll_y = Math.max(0, @scroll_y - 1)

        draw(screen, document0, :scroll) if settled
      when .mouse_wheel_down?
        @scroll_y = Math.min(screen.max_scroll_y, @scroll_y + 1)

        draw(screen, document0, :scroll) if settled
      when .mouse_release?
        # TODO: we should pretty print into an intermediate tree that contains
        # widths and heights and positions of everything! This way we won't need
        # display context and visible ui tree here!
        probe(@ctx.not_nil!, @visible_ui_tree, event.mouse_x - SCREEN_PX + @scroll_x, event.mouse_y - SCREEN_PY + @scroll_y) do |node|
          Term.case(node) do
            # Dispatch click event to mailbox
            matchpi %{[block {¦ mailbox: mailbox-path_dict}]} do
              keypath = Rhodium.keypath(document1, mailbox_path.unsafe_as_d.items)
              mailbox = Rhodium.follow(document1, keypath)
              document1 = Rhodium.assign(document1, keypath, Term.of(mailbox.append({:press})))
            end

            otherwise {}
          end
        end
      end
    when .key?
      if event.ch.zero? # Non-character key
        case event.key
        when .ctrl_s?
          # TODO: we should use pprint for this
          File.open("document#{Time.utc.to_s("%F-%H%M%S")}.wwml", "w") do |io|
            ML.display(io, document0)
          end
        when .esc?
          motion = Term.of(:key, :escape)
        when .tab?
          motion = Term.of(:key, :tab)
        when .home?
          motion = Term.of(:key, :home)
        when .end?
          motion = Term.of(:key, :end)
        when .enter?
          motion = Term.of(:key, :enter)
        when .backspace?, .backspace2?
          motion = Term.of(:key, :backspace)
        when .delete?
          motion = Term.of(:key, :delete)
        when .arrow_left?
          if event.mod.ctrl?
            motion = Term.of(:key, :"C-left")
          else
            motion = Term.of(:key, :left)
          end
        when .arrow_right?
          if event.mod.ctrl?
            motion = Term.of(:key, :"C-right")
          else
            motion = Term.of(:key, :right)
          end
        when .arrow_up?
          motion = Term.of(:key, :up)
        when .arrow_down?
          motion = Term.of(:key, :down)
        when .ctrl_c?
          raise KeyboardInterrupt.new
        when .f1?
          @debug = true

          draw(screen, document0, :forced) if settled
        when .f2?
          @debug = false

          draw(screen, document0, :forced) if settled
        when .pgup?
          if event.mod.shift?
            @scroll_x = Math.max(0, @scroll_x - 1)
          else
            @scroll_y = Math.max(0, @scroll_y - 1)
          end

          draw(screen, document0, :scroll) if settled
        when .pgdn?
          if event.mod.shift?
            @scroll_x = Math.min(screen.max_scroll_x, @scroll_x + 1)
          else
            @scroll_y = Math.min(screen.max_scroll_y, @scroll_y + 1)
          end

          draw(screen, document0, :scroll) if settled
        end
      else
        chr = event.ch.chr
        if chr.printable?
          motion = Term.of(:input, chr)
        end
      end

      if motion
        document1 = Rhodium::Q.of(document1, Rhodium::Events)
          .enqueue(:edit, {:edge, :user}, motion)
          .commit(document1, Rhodium::Events)
      end
    end

    document1
  end

  # Waits until an event occurs that modifies *document*. Returns
  # the modified document.
  private def wait(screen : Screen, document document0 : Term::Dict) : Term::Dict
    while true
      event = @events.receive
      if event.is_a?(Alarm)
        return document0
      end

      # Assume implicitly that we're settled if we're wait()ing.
      document1 = handle(screen, document0, event.tb, settled: true)
      unless document0.same?(document1)
        return document1
      end
    end
  end

  # Checks if an event is available and if it is, possibly modifies
  # *document* appropriately. Otherwise, returns the document unchanged.
  private def peek(screen : Screen, document : Term::Dict) : Term::Dict
    while true
      select
      when event = @events.receive
        next if event.is_a?(Alarm)

        # Assume implicitly that we're not settled if we're peek()ing.
        return handle(screen, document, event.tb, settled: false)
      else
        return document
      end
    end
  end

  # TODO: HACK. Before we had different granularity of draws. Now we
  # have very fine grain granularity of draws. This means we see and
  # are able to draw both the removal of suggestions by the cursor, and
  # the insertion of new (or re-insertion of old!) ones. Unfortunately,
  # this causes flickering of suggestions -- old ones disappear before new
  # (or the same) ones appear one frame later. To fix this we draw only
  # if edits were fully handled; that is, if there were no edits in
  # the previous cycle -- since we show suggestions on cycle only. This
  # solves flickering but may introduce unwanted delay. A better solution
  # is required.
  #
  # This is fine BUT, I have still not done the analysis of whether or not
  # feedback circuits are possible that do not trigger the cycle event. That
  # is, if it is possible to create a circuit that emits events forever,
  # without dependence on cycle.
  @should_draw = true
  @seen_edit = false

  private def check_should_draw
    D7::Step.new do |document|
      events = Rhodium::Q.of(document, Rhodium::Events)

      if event = events.first?
        Term.case(event) do
          matchpi %{(edit @_ _)} { @seen_edit = true }
          otherwise { }
        end
      else
        @should_draw = !@seen_edit
        @seen_edit = false
      end

      # We do not modify the document and therefore we never trigger
      # a transition.
      {document, false}
    end
  end

  alias Event = UserEvent | Alarm

  record UserEvent, tb : Termbox::Event
  record Alarm

  @events = Channel(Event).new(128)

  def run(seed : Term::Dict, *, initial : Bool) : Nil
    if @events.closed?
      raise "Can only call Soma#run once. This instance is expended. Please create another instance"
    end

    screen do |screen|
      settled = false

      @mt.spawn do
        while true
          event = Termbox.poll

          @events.send(UserEvent.new(event))

          # Since we cannot interrupt a poll, we must detect Ctrl-C ourselves
          # also and terminate the fiber.
          case event.type
          when .key?
            if event.key.ctrl_c?
              break
            end
          end
        end
      rescue Channel::ClosedError
        # Noop. We just stop polling.
      end

      @mt.spawn do
        while true
          @nitrene.alarm.receive
          @events.send(Alarm.new)
        end
      rescue Channel::ClosedError
        # Noop. We just stop polling.
      end

      while true
        # Force initial redraw and redraw before settling. On the latter,
        # since run() will only redraw periodically, it may happen that
        # we loose a frame due to it settling before it can draw. Forcing
        # a redraw after settling solves this.
        draw(screen, seed, :forced)

        @should_draw = false

        if settled
          seed = wait(screen, seed)
          settled = false
        end

        seed = D7.run(seed,
          log: D7::Log::None.new,
          transition: Rhodium.transition,
          step: D7.steps(check_should_draw, Rhodium.step, Nitrene.step(@nitrene), step(screen)),
          goal: D7::Goal.none,
          initial: initial,
        )

        initial = false
        settled = true
      end
    rescue KeyboardInterrupt
      @nitrene.alarm.close
      @events.close
    end
  end
end

seed = Term.of
initial = true

if filename = ARGV[0]?
  seed = ML.term(File.read(filename))
  initial = false
else
  {% if true || flag?(:release) %}
    seed = ML.terms <<-WWML
    (comment "Welcome to µsoma, a GUI for Wirewright")
    (comment "")
    (comment "µsoma to Wirewright is what a web browser is to the Internet")
    (comment "")
    (comment "You're looking at a *self-embodied program*. But it only contains comments right now. Hit left/right arrow to see for yourself. Or type `;;` and write your own!")
    (comment "")
    (comment "Try typing the following:")
    (comment "")
    (comment "\\t(cell 0 @count)")
    (comment "\\t(button \\"Increment\\" as 1 to @deltas ())")
    (comment "\\t(button \\"Decrement\\" as -1 to @deltas ())")
    (comment "\\t(transform @deltas to @counts with @count (+ count _))")
    (comment "\\t(latest @counts @count)")
    (comment "")
    (comment "Click on the buttons and see what happens! :^)")
    (comment "")
    (comment "- Use Ctrl-C to quit")
    (comment "- Use Page Up/Page Down or scroll if out of screen space")

    ("" | "" () @user)
    WWML
  {% else %}
    seed = ML.terms <<-WWML
    o
    .
    (lookaround @snapshots @relook
      ("" | "" () @rod)
      (button "Kickstart" to @relook ())
      (edit-cast @rod-commands to @rod)
      (transform @rod-commands to @relook true)
      (cell up @dir)
      (transform (@snapshots (behind_ ahead_)) to @stimuli with @dir (dir behind ahead))
      (map @stimuli to @actions
        ((up (_* o) (. _*)) dir-down)
        ((down (_* .) (o _*)) dir-up)
        ((up (_* .) _) keep-moving)
        ((down _ (. _*)) keep-moving))
      (transform (@actions dir-down) to @dirs down)
      (transform (@actions dir-up) to @dirs up)
      (transform (@actions keep-moving) to @dirs with @dir dir)
      (latest @dirs @dir)
      (map @dirs to @rod-commands (up (move-parent-behind)) (down (move-parent-ahead))))
    .
    o

    ("" | "" () @user)
    WWML
  {% end %}
end

soma = Soma.new
soma.run(seed.as_d, initial: initial)
