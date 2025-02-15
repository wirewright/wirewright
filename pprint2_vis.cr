require "./pprint2"
require "./oklch"
require "./delta7_proto"
require "./baz5_editor"

def oklch(l, c, h)
  Oklch.to_rgb(l*100, c, h)
end

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

def compose_suggestion_box(sections : Enumerable(String), &)
  sections = sections.map do |section|
    wrap(section, maxwidth: 60).chomp # ?!
  end

  content_px = 1

  # Compute width and height.
  w = w1 = 0
  h = 0

  sections.each_with_index do |section, index|
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
  w += content_px*2

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
  i = oi = 1 + content_px
  j = 1
  em = false
  sections.each_with_index do |section, index|
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

def draw_block(screen, block, x, y)
  Term.case(block) do
    matchpi %{[cursor lhs_string rhs_string]} do
      # text-gray-200 text-gray-700
      fg = oklch(0.928, 0.006, 264.531)
      bg = oklch(0.373, 0.034, 259.733)

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

    # TODO: ideally we'd want to show only the first few of them but right now
    # we won't be able to scroll. After we can scroll through suggestions (some cursor
    # coop needed here), we can show only top N suggestions.
    matchpi %{(suggestions suggestions_string+ ¦ () shl_: (%number +i32))} do
      # Save. Suggestions "float". We'll restore later.
      x0 = x
      y0 = y

      ox = x - shl.to(Int32)

      # Go below the cursor (well, assuming the cursor is the source of this node!)
      x = ox
      y += 1

      # TODO: here we should try to find (iteratively?) where we'd want to place this
      # node. Currently we're stupid and rely on clipping so if the hint is positioned
      # too much to the right/bottom it's clipped. The user has then to scroll to see
      # the hint. compose_suggestion_box's width/height computation will then have to
      # be moved here.

      # text-blue-200
      fg_em = oklch(0.809, 0.105, 251.813)

      compose_suggestion_box({suggestions.items.join('\n', &.to(String))}) do |char, i, j, em|
        screen.set(char, x + i, y + j, em ? fg_em : screen.fg0, screen.bg0, 9)
      end

      x = x0
      y = y0
    end

    matchpi %{(suggestion name_string desc_string ¦ () shl_: (%number +i32))} do
      # Save. Suggestions "float". We'll restore later.
      x0 = x
      y0 = y

      ox = x - shl.to(Int32)

      # Go below the cursor (well assuming the cursor is the source of this node!)
      x = ox
      y += 1

      # TODO: here we should try to find (iteratively?) where we'd want to place this
      # node. Currently we're stupid and rely on clipping so if the hint is positioned
      # too much to the right/bottom it's clipped. The user has then to scroll to see
      # the hint. compose_suggestion_box's width/height computation will then have to
      # be moved here.

      # text-blue-200
      fg_em = oklch(0.809, 0.105, 251.813)

      desc_punct = desc.to(String)
      if desc_punct[-1].letter?
        desc_punct += '.'
      end

      compose_suggestion_box({name.to(String), desc_punct}) do |char, i, j, em|
        screen.set(char, x + i, y + j, em ? fg_em : screen.fg0, screen.bg0, 9)
      end

      x = x0
      y = y0
    end

    matchpi %{(button caption_string ¦ _ enabled⋮ true)} do
      if enabled.true?
        # text-gray-200 text-gray-600
        fg = oklch(0.928, 0.006, 264.531)
        bg = oklch(0.446, 0.03, 256.802)
      else
        # text-gray-300 text-gray-800
        fg = oklch(0.872, 0.01, 258.338)
        bg = oklch(0.278, 0.033, 256.848)
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

def draw(ctx, screen, node : Term, x, y)
  Term.case(node) do
    matchpi %{(frag chars_string ¦ _ tag: number)} do
      chars.to(String).each_char do |char|
        screen.set(char, x, y, fg: oklch(0.702, 0.183, 293.541)) # text-violet-400
        x += 1
      end
    end

    matchpi %{(frag chars_string ¦ _ tag: string)} do
      chars.to(String).each_char do |char|
        screen.set(char, x, y, fg: oklch(0.681, 0.162, 75.834)) # text-yellow-600
        x += 1
      end
    end

    matchpi %{(frag chars_string ¦ _ tag: symbol)} do
      chars.to(String).each_char do |char|
        screen.set(char, x, y, fg: oklch(0.928, 0.006, 264.531)) # text-gray-200
        x += 1
      end
    end

    matchpi %{(frag chars_string ¦ _ tag: boolean)} do
      chars.to(String).each_char do |char|
        screen.set(char, x, y, fg: oklch(0.646, 0.222, 41.116)) # text-orange-600
        x += 1
      end
    end

    matchpi %{(frag chars_string ¦ _ tag: edge)} do
      chars.to(String).each_char do |char|
        screen.set(char, x, y, fg: oklch(0.792, 0.209, 151.711)) # text-green-400
        x += 1
      end
    end

    matchpi %{[frag chars_string]} do
      chars.to(String).each_char do |char|
        if char.in?('(', ')', '{', '}', '[', ']', '¦')
          screen.set(char, x, y, fg: oklch(0.707, 0.022, 261.325)) # text-gray-400
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
        draw(ctx, screen, child, x, y)
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
        draw(ctx, screen, child, x, y)
        _, child_height = measure(ctx, child)
        y += child_height
      end
    end

    matchpi %[(indented child_ ¦ _ by: (%optional 1 n←(%number +i32)))] do
      draw(ctx, screen, child, x + n.to(Int32), y)
    end
  end
end

class Screen
  @bg : Termbox::Color
  @fg : Termbox::Color

  getter viewport_w : Int32
  getter viewport_h : Int32

  alias Layer = Hash({Int32, Int32}, {Char, Termbox::Color, Termbox::Color})

  getter fg0
  getter bg0

  def initialize(@bg0 : {UInt8, UInt8, UInt8}, @fg0 : {UInt8, UInt8, UInt8}, @viewport_w, @viewport_h)
    @bg = Termbox::Color.rgb(*bg0)
    @fg = Termbox::Color.rgb(*fg0)

    @layers = [] of {Layer, Int32}
    @overlays = Hash({Int32, Int32}, {Termbox::Color, Termbox::Color}).new

    @max_x = 0
    @max_y = 0
  end

  def resize(@viewport_w, @viewport_h)
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
    @max_x < @viewport_w ? 0 : (@max_x - @viewport_w) + 1 + SCREEN_PX*2
  end

  def max_scroll_y : Int32
    @max_y < @viewport_h ? 0 : (@max_y - @viewport_h) + 1 + SCREEN_PY*2
  end

  def present(px = 0, py = 0, scroll_x = 0, scroll_y = 0)
    clip_w = @viewport_w
    clip_h = @viewport_h

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

  private def suggest?(user_string : String, candidate_string : String)
    return true if user_string.empty?

    candidate_string.downcase.starts_with?(user_string.downcase)
  end

  def call(ctx, term, postfix, head, rest)
    Term.matchpi(term, %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions⋮ ())}) do
      fullstring = lhs.to(String) + rhs.to(String)

      candidates = suggestions.items.select do |suggestion|
        Term.case(suggestion) do
          matchpi %{(name_string _string)} do
            suggest?(fullstring, name.to(String))
          end

          otherwise do
            false
          end
        end
      end

      if candidates.size > 1
        # (suggestions suggestions_string*)
        sugg = Term::Dict.build do |commit|
          commit << :suggestions
          commit.with(:shl, rhs.charcount)
          commit.concat(candidates) { |(name, _)| name }
        end
      elsif candidates.size == 1
        # (suggestion node_string desc_string)
        sugg = Term::Dict.build do |commit|
          commit << :suggestion
          commit.with(:shl, rhs.charcount)
          name, desc = candidates[0]
          commit << name << desc
        end
      end

      cursor_block = Term.of(:block,
        Term[:cursor, lhs, rhs,
          w: lhs.charcount + rhs.charcount,
          h: 1])

      # Do not emit empty suggestions.
      if sugg
        return Term.of(:row, cursor_block, Term.of(:"block/floating", sugg), Term.of(:frag, postfix))
      else
        return Term.of(:row, cursor_block, Term.of(:frag, postfix))
      end
    end

    rest.call(ctx, term, postfix)
  end
end

struct Col
  include Feature

  def call(ctx, term, postfix, head, rest)
    Term.of_case(term) do
      matchpi %{(col (%plural children min: 2) ¦ gap⋮ 0)} do |children|
        continue if D.cursordepth(term) == 1 # Allow cursor inside children.

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
    Term.matchpi(term, %{(row children_* ¦ gap⋮ 0)}) do
      continue if D.cursordepth(term) == 1 # Allow cursor inside children.

      return Term.of(Term::Dict.build do |commit|
        commit << :row
        commit.with(:gap, gap)
        children.items.each_with_last do |child, last|
          commit << head.call(ctx, child, last ? postfix : "")
        end
      end)
    end

    rest.call(ctx, term, postfix)
  end
end

struct Button
  include Feature

  private def button(term, caption)
    Term.of(:block,
      Term[:button, caption, w: caption.charcount + 2, h: 1, enabled: !term[:waiting]?, mailbox: term[:mailbox]?])
  end

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi %{[button caption_string to @_ (_*)]} do
        continue unless D.cursordepth(term) == -1

        Term.of(:row, button(term, caption), Term.of(:frag, postfix))
      end

      matchpi %{[button caption_string as msg_ to @_ (_*)]} do
        continue unless D.cursordepth(term) == -1

        Term.of(:row, button(term, caption), Term.of(:frag, postfix))
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

layout_chain = Chain(Layout).new(
  Layout::DictInline.new,
  Layout::CallColumn.new,
  Layout::CallKwargsInlineWithBlock.new,
  Layout::CallArgIndentedKwargs.new,
  Layout::CallKwargsColumnWithBlock.new,
  Layout::CallIndented.new,
  Layout::MapInline.new,
  Layout::MapMultiline.new,
  Layout::MapMultilineIndented.new,
  Layout::DictAligned.new,
)

ppairs_chain = Chain(Feature).new(
  Feature::PairspartLet.new,
  Feature::PairspartOptional.new,
  Feature::PairspartNegation.new,
  Feature::PairspartBlank.new,
  Feature::PairspartPair.new
)

feature_chain = Chain(Feature).new(
  Cursor.new, # << Custom features
  Button.new, # <<
  Col.new,    # <<
  Row.new,    # <<
  Feature::Backmap.new,
  Feature::Rule.new,
  Feature::Edge.new,
  Feature::BackrefMy.new,
  Feature::BackrefUp.new,
  Feature::BackrefDown.new,
  Feature::Hold.new,
  Feature::PatternSlot.new,
  Feature::PatternNonself.new,
  Feature::PatternLiteral.new,
  Feature::PatternLet.new,
  Feature::PatternItemFirst.new,
  Feature::PatternItemSource.new,
  Feature::PatternPairspart.new,
  Feature::SymbolLiteral.new,
  Feature::NumberLiteral.new,
  Feature::StringLiteral.new,
  Feature::BooleanLiteral.new,
  Feature::EmptyDict.new,
  Feature::CallLike.new,
  Feature::MapLike.new,
  Feature::DictLiteral.new,
)

require "./libtermbox2"
require "./rolling_set"

document = ML.terms <<-WWML
(cell 0 @count)
(button "Increment" as 1 to @deltas ())
(button "Decrement" as -1 to @deltas ())
(transform (@deltas delta_number) to @counts with @count (+ state delta))
(latest @counts @count)

(log @actions in ())
(col
  (row
    (button "1" as 1 to @actions ())
    (button "2" as 2 to @actions ())
    (button "3" as 3 to @actions ()))
  (row
    (button "4" as 4 to @actions ())
    (button "5" as 5 to @actions ())
    (button "6" as 6 to @actions ()))
  (row
    (button "7" as 7 to @actions ())
    (button "8" as 8 to @actions ())
    (button "9" as 9 to @actions ()))
  (row
    (button "←" as erase to @actions ())
    (button "0" as erase to @actions ())
    (button "→" as enter to @actions ())))

(transform (@actions digit_number) to @counts digit)

("" | "" () @user)
WWML

short_term_memory = RollingSet(Term, 8).new

# document = ML.terms File.read("./editor.soma.wwml")
prev_draw_at = 0.milliseconds
currently_visible_document = nil

visible_ui_tree = Term.of
ctx = nil
scroll_x = scroll_y = 0

# In order to draw a screen, you first have to explain to the draw function
# why it is that you want to draw.
enum DrawReason : UInt8
  DocumentChanged
  Resize
  Scroll
  Forced
  DrawChance
end

SCREEN_PX = 2
SCREEN_PY = 1

draw = ->(screen : Screen, reason : DrawReason) do
  current_draw_time = Time.monotonic

  # Handle DrawChance
  if reason.draw_chance? && current_draw_time - prev_draw_at < 30.milliseconds
    return # Skip drawing, it's not the time.
  end

  prev_draw_at = current_draw_time

  # Handle Scroll
  if reason.scroll?
    Termbox.clear # ?!
    screen.present(px: SCREEN_PX, py: SCREEN_PY, scroll_x: scroll_x, scroll_y: scroll_y)
    return
  end

  next_visible_document = D7.visible(document)

  # Handle Resize
  if !reason.resize? && currently_visible_document == next_visible_document
    return # Skip drawing, visible document did not change.
  end

  currently_visible_document = next_visible_document

  # Forced and DocumentChanged are handled implicitly here.

  screen.clear

  maxchars = (screen.viewport_w * 0.8).floor.to_i

  annotated = annotate(next_visible_document)
  ctx_ = ctx = DisplayContext.new(maxchars, maxchars*2, feature_chain, layout_chain, ppairs_chain)

  # TODO: can we somehow standardize this in the pretty printer?

  # Allow only DictAligned for the document itself.
  tree = LayoutSet::All.thunk(annotated, "", LayoutSet::DictAligned)
  flat, _ = flatten(ctx_, tree)

  visible_ui_tree = flat

  draw(ctx_, screen, flat, 0, 0)

  screen.present(px: SCREEN_PX, py: SCREEN_PY, scroll_x: scroll_x, scroll_y: scroll_y)
end

# *Annotation* involves writing down the keypaths of things, etc., so that we can
# in the future, point back from a UI-tree to the S-expression that produced it
# in the D7 document.
def annotate(root : Term)
  rangepath = Term[]

  while rangepath = D.successor?(root, rangepath)
    node = D.follow(root, rangepath)

    Term.case(node) do
      matchpi %{[button _ to @_ (_*)]} do
        continue unless D.cursordepth(node) == -1

        root = D.assign(root, rangepath, node.with(:mailbox, rangepath.append({4, 5})))
      end

      matchpi %{[button _ as _ to @_ (_*)]} do
        continue unless D.cursordepth(node) == -1

        root = D.assign(root, rangepath, node.with(:mailbox, rangepath.append({6, 7})))
      end

      otherwise { }
    end
  end

  root
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

Termbox.init do
  Termbox.input_mode = Termbox::InputMode::Alt
  Termbox.output_mode = Termbox::OutputMode::Truecolor

  # text-gray-900 text-gray-300
  screen = Screen.new(bg0: oklch(0.21, 0.034, 264.665), fg0: oklch(0.872, 0.01, 258.338), viewport_w: Termbox.width, viewport_h: Termbox.height)

  draw.call(screen, DrawReason::DocumentChanged)

  settled = true

  while true
    document0 = document1 = document

    if settled
      event = Termbox.poll
    else
      unless event = Termbox.peek?
        document1 = D7.next(document0)
        settled = !short_term_memory.add?(document1)
        document = document1
        # If settled is true, this means there was false -> true settled edge and
        # we need to force redraw. If settled false -> false, we only redraw periodically.
        draw.call(screen, settled ? DrawReason::Forced : DrawReason::DrawChance)
        next
      end
    end

    motion = nil

    case event.type
    when .resize?
      screen.not_nil!.resize(event.resize_w, event.resize_h)
      scroll_x = Math.min(screen.not_nil!.max_scroll_x, scroll_x)
      scroll_y = Math.min(screen.not_nil!.max_scroll_y, scroll_y)
      if settled # Force redraw if we're not doing so periodically
        draw.call(screen, DrawReason::Resize)
      end
    when .mouse?
      case event.key
      when .mouse_wheel_up?
        scroll_y = Math.max(0, scroll_y - 1)
        if settled
          draw.call(screen, DrawReason::Scroll)
        end
      when .mouse_wheel_down?
        scroll_y = Math.min(screen.not_nil!.max_scroll_y, scroll_y + 1)
        if settled
          draw.call(screen, DrawReason::Scroll)
        end
      when .mouse_release?
        probe(ctx.not_nil!, visible_ui_tree, event.mouse_x - SCREEN_PX + scroll_x, event.mouse_y - SCREEN_PY + scroll_y) do |node|
          Term.case(node) do
            matchpi %{[block {_ mailbox: mailbox-path_dict}]} do
              mailbox = D.follow(document1, mailbox_path.unsafe_as_d)
              # Dispatch click event to mailbox
              document1 = D.assign(document1, mailbox_path.unsafe_as_d, mailbox.append({:press}))
            end

            otherwise {}
          end
        end
      end
    when .key?
      if event.ch.zero? # Non-character key
        case event.key
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
          running = false
          break
        when .pgup?
          if event.mod.shift?
            scroll_x = Math.max(0, scroll_x - 1)
          else
            scroll_y = Math.max(0, scroll_y - 1)
          end
          if settled
            draw.call(screen, DrawReason::Scroll)
          end
        when .pgdn?
          if event.mod.shift?
            scroll_x = Math.min(screen.not_nil!.max_scroll_x, scroll_x + 1)
          else
            scroll_y = Math.min(screen.not_nil!.max_scroll_y, scroll_y + 1)
          end
          if settled
            draw.call(screen, DrawReason::Scroll)
          end
        when .ctrl_v?
        when .ctrl_a?
        when .ctrl_q?
        end
      else
        chr = event.ch.chr
        if chr.printable?
          motion = Term.of(:input, chr)
        end
      end
    end

    if motion
      document1 = D::Q.of(document1.as_d)
        .enqueue(:edit, {:edge, :user}, motion)
        .commit(document1.as_d)
        .upcast
    end

    next if settled && document0.same?(document1)

    document1 = D7.next(document1)
    settled = !short_term_memory.add?(document1)
    document = document1
    draw.call(screen, DrawReason::DocumentChanged)
  end
end

# pp flatten(ctx, feature_chain.call(ctx, Term.of(:"%item°", 100, 200, 300), ""))

# pp Feature::Edge.call(ctx, Term.of(:edge, 100), "))", ->(ctx : DisplayContext, term : Term, postfix : String) do
#                         raise "end of chain!!"
#                       end)

# ed = Term.of(JSON.parse(File.read("./data/people.json")))
# ed = ML.terms(File.read("./editor.soma.wwml"))# Term.of(:+, {:*, 3, 4}, {2})
# require "./"

# # str = String.build do |io|
# #   screen = Screen.new

#   ed.items.each do |sexp|
#     # screen.clear
#     ctx = DisplayContext.new(60, 120, feature_chain, layout_chain, ppairs_chain)
#     tree = feature_chain.call(ctx, sexp, "")
#     flat, excess = flatten(ctx, tree)
#     # puts excess
#     puts ML.display(flat)
#     # draw(ctx, screen, flat, 0, 0)
#     # screen.write(io)
#     # io.puts
#   end
# # end

# puts str
# puts str == File.read("./pprint1.out.1")
# pp ed == ML.terms(str)

# [x] x_: 100 => x: (%let x 100)
# [x] x⋮ 100 => x: (%optional 100 x_number) ;; infers type
# [x] x_⋮ 100 => x: (%optional 100 x_) ;; does not infer type
# [x] x_string⋮ 100 ;; invalid. Either infer or any-blank
# [x] ¦ ... w_ ... => ... w: w_ ...
# [x] ¦ ... w_number ... => ... w: w_number ...
# [x] -x_ => (%- _ x)
# [x] -x_number => (%- _number x)
