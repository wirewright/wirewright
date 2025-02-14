require "./pprint2"
require "./oklch"
require "./delta7_proto"
require "./baz5_editor"

def oklch(l, c, h)
  Oklch.to_rgb(l*100, c, h)
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

      ox = x

      button = (" " * (caption.charcount + 2)) \
             + ("\n " + caption.to(String) + " \n") \
             + (" " * (caption.charcount + 2))

      button.each_char do |char|
        if char == '\n'
          y += 1
          x = ox
          next
        end
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

    matchpi %{[block term_]} do
      x, y = draw_block(screen, term, x, y)
    end

    matchpi %[(row children_* ¦ _ gap: (%optional 0 gap←(%number +i32)))] do
      children.items.each_with_index do |child, index|
        x += gap.to(Int32) if index > 0
        draw(ctx, screen, child, x, y)
        child_width, _ = measure(ctx, child)
        x += child_width
      end
    end

    matchpi %[(col children_* ¦ _ gap: (%optional 0 gap←(%number +i32)))] do
      children.items.each_with_index do |child, index|
        y += gap.to(Int32) if index > 0
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

  def initialize(bg, fg)
    @bg = Termbox::Color.rgb(*bg)
    @fg = Termbox::Color.rgb(*fg)

    @cells = Hash({Int32, Int32}, {Char, Termbox::Color, Termbox::Color}).new
    @overlays = Hash({Int32, Int32}, {Termbox::Color, Termbox::Color}).new
  end

  def clear : Nil
    @cells.clear
    @overlays.clear
    Termbox.clear(fg: @fg, bg: @bg)
  end

  def set(ch : Char, x, y, fg = nil, bg = nil) : Nil
    @cells[{x, y}] = {ch, fg ? Termbox::Color.rgb(*fg) : @fg, bg ? Termbox::Color.rgb(*bg) : @bg}
  end

  def overlay(x, y, fg = nil, bg = nil)
    @overlays[{x, y}] = {fg ? Termbox::Color.rgb(*fg) : @fg, bg ? Termbox::Color.rgb(*bg) : @bg}
  end

  def present(clip_w = nil, clip_h = nil)
    @cells.each do |(x, y), (char, fg, bg)|
      next if clip_w && !(0 <= x < clip_w)
      next if clip_h && !(0 <= y < clip_h)
      fg, bg = @overlays[{x, y}]? || {fg, bg}
      Termbox.set(char, x: x, y: y, fg: fg, bg: bg)
    end
    @overlays.each do |(x, y), (fg, bg)|
      next if @cells.has_key?({x, y})
      next if clip_w && !(0 <= x < clip_w)
      next if clip_h && !(0 <= y < clip_h)
      Termbox.set(' ', x: x, y: y, fg: fg, bg: bg)
    end
    Termbox.present
  end
end

struct Cursor
  include Feature

  def call(ctx, term, postfix, head, rest)
    Term.matchpi(term, %{(lhs_string | rhs_string (_*) @user)}) do
      return Term.of(:row, Term.of(:block, Term[:cursor, lhs, rhs, w: lhs.charcount + rhs.charcount, h: 1]), Term.of(:frag, postfix))
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
    Term.of(:block, Term[:button, caption, w: caption.charcount + 2, h: 3, enabled: !term[:waiting]?])
  end

  def call(ctx, term, postfix, head, rest)
    # TODO: if cursor is nonempty in button, we must not draw the button
    # as a button!
    Term.case(term) do
      matchpi %{[button caption_string to @_ (_*)]} do
        Term.of(:row, button(term, caption), Term.of(:frag, postfix))
      end

      matchpi %{[button caption_string as msg_ to @_ (_*)]} do
        continue unless D.cursordepth(msg) == -1

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
("" | "" () @user)
(button "Increment" to @actions ())
(button "Decrement" as -1 to @deltas ())
WWML

short_term_memory = RollingSet(Term, 8).new

# document = ML.terms File.read("./editor.soma.wwml")

width = 0
height = 0

prev_draw_at = 0.milliseconds
prev_visible_document = nil

# text-gray-900 text-gray-300
screen = Screen.new(bg: oklch(0.21, 0.034, 264.665), fg: oklch(0.872, 0.01, 258.338))

draw = ->(forced : Bool) do
  current_draw_time = Time.monotonic

  unless forced || current_draw_time - prev_draw_at >= 30.milliseconds
    return # Skip drawing, it's not the time.
  end

  prev_draw_at = current_draw_time

  visible_document = D7.visible(document)
  if prev_visible_document == {visible_document, width, height}
    return # Skip drawing, visible document did not change.
  end

  prev_visible_document = {visible_document, width, height}

  screen.clear

  maxchars = (width * 0.8).floor.to_i

  ctx = DisplayContext.new(maxchars, maxchars*2, feature_chain, layout_chain, ppairs_chain)
  tree = feature_chain.call(ctx, visible_document, "")
  flat, _ = flatten(ctx, tree)
  draw(ctx, screen, flat, 0, 0)

  screen.present(clip_w: width, clip_h: height)
end

Termbox.init do
  Termbox.input_mode = Termbox::InputMode::Alt
  Termbox.output_mode = Termbox::OutputMode::Truecolor

  width = Termbox.width
  height = Termbox.height

  draw.call(true)

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
        draw.call(settled)
        next
      end
    end

    motion = nil

    case event.type
    when .resize?
      width = event.resize_w
      height = event.resize_h
      if settled # Force redraw if we're not doing so periodically
        draw.call(true)
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
    draw.call(true)
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
