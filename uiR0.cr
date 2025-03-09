# TODO: once we have a templating engine we'll be able to extract
# markup for e.g. button or comment etc. into separate .wwml files
# (into e.g. frags/) folder! Right now they're hard-coded. But in
# theory should be user-modifiable (even per project!)

require "./src/wirewright"
require "./pprint2"
require "./colors"
require "crsfml"
require "./sfml_util"

module ::Ww::Keypath
  # TODO: while loop with D7#successor?-like impl.
  def self.each_item_impl(term, fn, keypath)
    result = fn.call(keypath, term)
    if result == false
      return false
    end

    return unless dict = term.as_d?

    dict.each_item_with_index do |item, index|
      keypath.push(Term.of(index))
      if each_item_impl(item, fn, keypath) == false
        return false
      end
    ensure
      keypath.pop
    end
  end

  def self.each_item(term : Term, &fn : Stack(Term), Term -> Bool?)
    each_item_impl(term, fn, keypath: Stack(Term).new)
  end

  def self.ascend(root : Term, keypath : Stack(Term), &)
    stack = Stack(Term::Dict).new
    tip = root

    keypath.each do |step|
      return unless node0 = tip.as_d?
      return unless node1 = node0[step]?

      stack << node0
      tip = node1
    end

    stack.reverse_each do |parent|
      next unless yield parent
      return parent
    end
  end

  def self.find(needle : Term, haystack : Term) : Array(Term::Dict)
    keypaths = [] of Term::Dict

    Term.each_keypath_and_leaf(haystack) do |keypath, leaf|
      if leaf == needle
        keypaths << Term[keypath]
      end

      true # Continue
    end

    keypaths
  end

  def self.assign(value : Term, term : Term, keypath : Term::Dict)
    keypath.items.empty? ? value : term.as_d.follow(keypath.items) { value }
  end

  def self.assign(value : Term, term : Term, keypaths : Enumerable(Term::Dict))
    keypaths.reduce(term) { |state, keypath| assign(value, state, keypath) }
  end

  def self.plug(value : Term, needle : Term, haystack : Term) : Term
    assign(value, haystack, find(needle, haystack))
  end

  def self.follow(keypath : Term::Dict, term : Term)
    keypath.items.empty? ? term : term.as_d.follow(keypath.items)
  end
end

module TextKit
  module FontLoader
    WEIGHTS = {
      {100, "Thin"},
      {200, "ExtraLight"},
      {300, "Light"},
      {400, "Regular"},
      {450, "Text"},
      {500, "Medium"},
      {600, "SemiBold"},
      {700, "Bold"},
      {800, "ExtraBold"},
      {900, "Black"},
    }

    # :nodoc:
    def self.refs(font : String, postfix : String) : Indexable(Path)
      {Path["fonts"] / "#{font.delete(' ')}-#{postfix}.ttf",
       Path["fonts"] / "#{font.delete(' ')}-#{postfix}.otf"}
    end

    # :nodoc:
    def self.path0?(font : String, weight pivot : Int32) : Path?
      WEIGHTS.reverse_each do |weight, postfix|
        next unless weight <= pivot

        refs(font, postfix).each do |ref|
          next unless File.exists?(ref)
          return ref
        end
      end

      WEIGHTS.each do |weight, postfix|
        break if weight <= pivot

        refs(font, postfix).each do |ref|
          next unless File.exists?(ref)
          return ref
        end
      end
    end

    @@paths = {} of {String, Int32} => Path?
    @@paths_lock = Mutex.new

    # Returns the path to *font* with the given *weight*.
    #
    # - If *weight* does not exist for *font* tries to fall back to lower values
    #   of *weight*.
    # - If still nothing, tries to fall back on higher values of *weight*.
    # - If still nothing, returns `nil`.
    def self.path?(font : String, weight : Int32) : Path?
      @@paths_lock.synchronize do
        @@paths.put_if_absent({font, weight}) { path0?(font, weight) }
      end
    end

    @@cache = {} of Path => {SF::Font, Mutex}
    @@cache_lock = Mutex.new

    def self.font_at(path : Path, &)
      font, lock = @@cache_lock.synchronize do
        @@cache.put_if_absent(path) do
          {SF::Font.from_file(path.to_s), Mutex.new}
        end
      end

      lock.synchronize do
        yield font
      end
    end
  end

  record Measurer, font : SF::Font, size : Int32, leading : Float32, tracking : Float32 do
    def self.new(font, size)
      new(font, size, leading: 1.0, tracking: 1.0)
    end

    def glyph(char : Char)
      @font.get_glyph(char.ord, @size, bold: false)
    end

    def kerning(c1 : Char, c2 : Char)
      @font.get_kerning(c1.ord, c2.ord, @size, bold: false)
    end

    def line_spacing : Float32
      @font.get_line_spacing(@size)
    end

    def texture
      @font.get_texture(@size)
    end

    def wswidth0
      glyph(' ').advance
    end

    def wswidth
      wswidth0 + letter_spacing
    end

    def letter_spacing
      (wswidth0 / 3) * (tracking - 1)
    end

    def line_height
      (size * leading).ceil.to_i
    end

    def zoom(n : Int32)
      change(size: @size + n)
    end

    def measure(string : String, *, window = 0...string.size)
      reader = Char::Reader.new(string, pos: string.char_index_to_byte_index(window.begin) || raise IndexError.new)

      width = 0
      state = '\0'

      window.each do
        current = reader.current_char

        width += kerning(state, current)
        state = current

        case current
        when ' ', '\n'
          width += wswidth
        when '\t'
          width += 4 * wswidth
        else
          glyph = glyph(current)
          width += glyph.advance + letter_spacing
        end

        break unless reader.has_next?

        reader.next_char
      end

      width.ceil.to_i
    end

    def_equals_and_hash @sf, @size, @leading, @tracking
  end

  record Info, font : SF::Font, size : Int32, leading : Float32 do
    def self.from(term : Term, &)
      Term.case(term) do
        matchpi %[{¦ font_string weight_: (%any 100 200 300 400 450 500 600 700 800 900) size_: (%number u8) leading_number}] do
          next unless path = FontLoader.path?(font.to(String), weight.to(Int32))

          FontLoader.font_at(path) do |font|
            yield new(font, size.to(Int32), leading.to(Float32))
          end
        end

        otherwise { }
      end
    end
  end

  def self.measure(content : String, font : String, weight : Int32, size : Int32, leading : Float32) : {Int32, Int32}
    unless path = FontLoader.path?(font, weight)
      return 0, 0
    end

    FontLoader.font_at(path) do |font|
      measurer = Measurer.new(font, size, leading, tracking: 1.0f32)

      if content.empty?
        return 0, measurer.line_height
      end

      width = 0
      height = 0

      content.each_line(chomp: true) do |line|
        width = Math.max(width, measurer.measure(line))
        height += measurer.line_height
      end

      {width, height}
    end
  end
end

module UIR
  @@cache = SyncMemo.new(capacity: 16_384, preallocate: true)

  class_getter rewriter : Rewriter do
    staging = File.read("./uiR.soma.wwml")

    refR = dfsR(
      switchR(
        { %[($my rewritee_)], envR(Term.of(:"$my")) },
        { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
        { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
      )
    )

    primitives = ProcRuleset.build do
      rulepi1(
        %{(measure text_string
                   font_string
                   weight←(%any 100 200 300 400 450 500 600 700 800 900)
                   size←(%number u8)
                   leading_number)}
      ) do
        w, h = TextKit.measure(
          text.to(String),
          font.to(String),
          weight.to(Int32),
          size.to(Int32),
          leading.to(Float32),
        )

        {width: w, height: h}
      end
    end

    onceR = chainR(callR(primitives), callR(PRIMITIVES))

    evalR = dfsR(
      switchR(
        { %[($ rewritee_)], exhR(dfsR(onceR)) },
        { %[($once rewritee_)], onceR },
      )
    )

    backmapR = chainR(refR, evalR)

    selector = ML.term(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])

    baseR = ->(base : String) do
      set, rec = recR

      exhR(
        set.call memoR(@@cache,
          choiceR(
            rulesetR(Ruleset.select(selector, ML.terms(base)), noR, backmapR, noR),
            itemsR(rec),
          ),
        ),
      )
    end

    baseR.call(staging)
  end
end

def stack(head, children, **kwargs)
  Term::Dict.build do |commit|
    commit << head
    commit.concat(children.items) { |child| styletree(child) }
    kwargs.each { |k, v| commit.with(k, v) }
  end
end

# Renders a pretty-print tree into a style-tree.
# TODO: this produces a very shitty, nested tree. As an optimization we should simplify it.
# For example, this will produce a lot of nested x-stack's. We should have an additional
# simplify pass for *pptree*. That resolves nested rows, cols, etc.
def styletree(pptree : Term)
  Term.of_case(pptree) do
    matchpi %{(frag content_string)} do
      Term.of(:text, content, style: "font-mono text-neutral-400")
    end

    matchpi %{(frag content_string ¦ tag: symbol)} do
      Term.of(:text, content, style: "font-mono text-neutral-300")
    end

    matchpi %{(frag content_string ¦ tag: number)} do
      Term.of(:text, content, style: "font-mono text-violet-400")
    end

    matchpi %{(frag content_string ¦ tag: string)} do
      Term.of(:text, content, style: "font-mono text-yellow-600")
    end

    matchpi %{(frag content_string ¦ tag: boolean)} do
      Term.of(:text, content, style: "font-mono text-orange-600")
    end

    matchpi %{(frag content_string ¦ tag: edge)} do
      Term.of(:text, content, style: "font-mono text-green-400")
    end

    matchpi %{(indented child_ by: n←(%number (whole _)))} do
      Term.of(:padding, styletree(child), style: "pl-#{n}")
    end

    matchpi %{(row children_+)} do
      stack(:"x-stack", children)
    end

    matchpi %{(col children_+)} do
      stack(:"y-stack", children, style: "gap-2")
    end

    matchpi %{(row children_+ ¦ gap_: (%number (whole _) > 0))} do
      stack(:"x-stack", children, style: "gap-#{gap * 3}")
    end

    matchpi %{(col children_+ ¦ gap_: (%number (whole _) > 0))} do
      stack(:"y-stack", children, style: "gap-#{gap * 3}")
    end

    matchpi %{[block subtree_]} do
      subtree
    end
  end
end

# TODO: the style engine should be smarter! E.g. p-... utilities are only available
# on `padding`. If not run on padding the style engine should wrap in padding, and
# inherit sizing props.
#
#   E.g.  (rect style: "p-3 bg-neutral-500 z-10")
#   >>>
#         (layer w: max h: max z-index: 10
#           (padding pl: 3rem pr: 3rem pt: 3rem pb: 3rem w: max h: max
#             (rect bg: (rgb ...) w: max h: max) ;; ...
#
# In other words, it is obvious in the code below some properties "belong" to certain
# nodes and to no other nodes. If the style engine detects the property is not being
# applied on the expected node, it should create that node with proper "ascendancy".
module Style
  extend self

  record Settings, sans : String, mono : String, rem : Int32 = 16

  def defaults(node : Term) : String
    Term.case(node) do
      matchpi %{text} do
        "content font-sans text-base text-black font-text"
      end

      matchpi %{rect} do
        "max"
      end

      otherwise { "content" }
    end
  end

  def apply(commit, style : String, settings : Style::Settings) : Nil
    style.split(' ', remove_empty: true).each do |codeword|
      # TODO: use hash lookup
      case codeword
      when .starts_with?("gap-")
        next unless n = codeword[4..].to_i?

        commit.with(:gap, n * 0.25 * settings.rem)
      when "rounded-xs"
        commit.with(:"border-radius", 0.125 * settings.rem)
      when "rounded-sm"
        commit.with(:"border-radius", 0.25 * settings.rem)
      when "rounded-md", "rounded"
        commit.with(:"border-radius", 0.375 * settings.rem)
      when "rounded-lg"
        commit.with(:"border-radius", 0.5 * settings.rem)
      when "rounded-xl"
        commit.with(:"border-radius", 0.75 * settings.rem)
      when "rounded-2xl"
        commit.with(:"border-radius", 1 * settings.rem)
      when "rounded-3xl"
        commit.with(:"border-radius", 1.5 * settings.rem)
      when "ring-px"
        commit.with(:"ring-l", 1)
        commit.with(:"ring-r", 1)
        commit.with(:"ring-t", 1)
        commit.with(:"ring-b", 1)
      when "ring-x-px"
        commit.with(:"ring-l", 1)
        commit.with(:"ring-r", 1)
      when "ring-y-px"
        commit.with(:"ring-t", 1)
        commit.with(:"ring-b", 1)
      when "ring-l-px"
        commit.with(:"ring-l", 1)
      when "ring-r-px"
        commit.with(:"ring-r", 1)
      when "ring-t-px"
        commit.with(:"ring-t", 1)
      when "ring-b-px"
        commit.with(:"ring-b", 1)
      when .starts_with?("ring-l-")
        next unless n = codeword[7..].to_i?

        commit.with(:"ring-l", n * 0.25 * settings.rem)
      when .starts_with?("ring-r-")
        next unless n = codeword[7..].to_i?

        commit.with(:"ring-r", n * 0.25 * settings.rem)
      when .starts_with?("ring-t-")
        next unless n = codeword[7..].to_i?

        commit.with(:"ring-t", n * 0.25 * settings.rem)
      when .starts_with?("ring-b-")
        next unless n = codeword[7..].to_i?

        commit.with(:"ring-b", n * 0.25 * settings.rem)
      when .starts_with?("ring-")
        next unless n = codeword[5..].to_i?

        commit.with(:"ring", n * 0.25 * settings.rem)
      when "max"
        commit.with(:w, :max)
        commit.with(:h, :max)
      when "content"
        commit.with(:w, :content)
        commit.with(:h, :content)
      when "w-max"
        commit.with(:w, :max)
      when "h-max"
        commit.with(:h, :max)
      when "w-content"
        commit.with(:w, :content)
      when "h-content"
        commit.with(:h, :content)
      when "h-fr"
        commit.with(:h, :max)
        commit.with(:fr, 1)
      when "w-px"
        commit.with(:w, :max)
        commit.with(:"max-w", 1)
      when "h-px"
        commit.with(:h, :max)
        commit.with(:"max-h", 1)
      when "fr"
        commit.with(:fractions, true)
      when "min-sm"
        commit.with(:"min-w", 24 * settings.rem)
        commit.with(:"min-h", 24 * settings.rem)
      when "min-w-sm"
        commit.with(:"min-w", 24 * settings.rem)
      when "min-h-sm"
        commit.with(:"min-h", 24 * settings.rem)
      when .starts_with?("w-")
        next unless n = codeword[2..].to_i?

        commit.with(:w, n * 0.25 * settings.rem)
      when .starts_with?("h-")
        next unless n = codeword[2..].to_i?

        commit.with(:h, n * 0.25 * settings.rem)
      when .starts_with?("pl-")
        next unless n = codeword[3..].to_i?

        commit.with(:pl, n * 0.25 * settings.rem)
      when .starts_with?("pr-")
        next unless n = codeword[3..].to_i?

        commit.with(:pr, n * 0.25 * settings.rem)
      when .starts_with?("pt-")
        next unless n = codeword[3..].to_i?

        commit.with(:pt, n * 0.25 * settings.rem)
      when .starts_with?("pb-")
        next unless n = codeword[3..].to_i?

        commit.with(:pb, n * 0.25 * settings.rem)
      when .starts_with?("px-")
        next unless n = codeword[3..].to_i?

        commit.with(:pl, n * 0.25 * settings.rem)
        commit.with(:pr, n * 0.25 * settings.rem)
      when .starts_with?("py-")
        next unless n = codeword[3..].to_i?

        commit.with(:pt, n * 0.25 * settings.rem)
        commit.with(:pb, n * 0.25 * settings.rem)
      when .starts_with?("p-")
        next unless n = codeword[2..].to_i?

        commit.with(:pl, n * 0.25 * settings.rem)
        commit.with(:pr, n * 0.25 * settings.rem)
        commit.with(:pt, n * 0.25 * settings.rem)
        commit.with(:pb, n * 0.25 * settings.rem)
      when .starts_with?("dx-")
        next unless n = codeword[3..].to_i?

        commit.with(:x, n * 0.25 * settings.rem)
      when .starts_with?("dy-")
        next unless n = codeword[3..].to_i?

        commit.with(:y, n * 0.25 * settings.rem)
      when .starts_with?("d-")
        next unless n = codeword[2..].to_i?

        commit.with(:x, n * 0.25 * settings.rem)
        commit.with(:y, n * 0.25 * settings.rem)
      when "font-sans"
        commit.with(:font, settings.sans)
      when "font-mono"
        commit.with(:font, settings.mono)
      when "font-thin"
        commit.with(:"weight", 100)
      when "font-extralight"
        commit.with(:"weight", 200)
      when "font-light"
        commit.with(:"weight", 300)
      when "font-normal"
        commit.with(:"weight", 400)
      when "font-text"
        commit.with(:"weight", 450)
      when "font-medium"
        commit.with(:"weight", 500)
      when "font-semibold"
        commit.with(:"weight", 600)
      when "font-bold"
        commit.with(:"weight", 700)
      when "font-extrabold"
        commit.with(:"weight", 800)
      when "font-black"
        commit.with(:"weight", 900)
      when "text-xs"
        commit.with(:size, 0.75 * settings.rem)
        commit.with(:leading, 1 / 0.75)
      when "text-sm"
        commit.with(:size, 0.875 * settings.rem)
        commit.with(:leading, 1.25 / 0.875)
      when "text-base"
        commit.with(:size, 1 * settings.rem)
        commit.with(:leading, 1.5 / 1)
      when "text-lg"
        commit.with(:size, 1.125 * settings.rem)
        commit.with(:leading, 1.75 / 1.125)
      when "text-xl"
        commit.with(:size, 1.25 * settings.rem)
        commit.with(:leading, 1.75 / 1.25)
      when "text-2xl"
        commit.with(:size, 1.5 * settings.rem)
        commit.with(:leading, 1.75 / 1.5)
	    when "text-3xl"
        commit.with(:size, 1.5 * settings.rem)
        commit.with(:leading, 2.25 / 1.875)
	    when "text-4xl"
        commit.with(:size, 2.25 * settings.rem)
        commit.with(:leading, 2.5 / 2.25)
	    when "text-5xl"
        commit.with(:size, 3 * settings.rem)
        commit.with(:leading, 1)
	    when "text-6xl"
        commit.with(:size, 3.75 * settings.rem)
        commit.with(:leading, 1)
	    when "text-7xl"
        commit.with(:size, 4.5 * settings.rem)
        commit.with(:leading, 1)
	    when "text-8xl"
        commit.with(:size, 6 * settings.rem)
        commit.with(:leading, 1)
	    when "text-9xl"
        commit.with(:size, 8 * settings.rem)
        commit.with(:leading, 1)
      when .starts_with?("text-")
        color = Term::Sym.new(codeword[5..])
        if rgb = Colors.rgb?(color)
          commit.with(:color, rgb)
        end
      when .starts_with?("bg-")
        color = Term::Sym.new(codeword[3..])
        if rgb = Colors.rgb?(color)
          commit.with(:bg, rgb)
        end
      end
    end
  end
end

# Renders a style-tree into a UI-tree.
def uitree(framectx, styletree : Term, settings : Style::Settings)
  Term.of_case(styletree) do
    matchpi %{(ml child_ ¦ _ toplevel_boolean only-visible_boolean)} do
      chain = ML::Display::MAIN_CHAIN.prepend(Cursor.new).prepend(Button.new).prepend(Comment.new)
      ctx = DisplayContext.new(60, 120, chain, data: Term.of(framectx: framectx))

      if child.type.dict?
        ppinput = D7.visible(child.unsafe_as_d)
      else
        ppinput = child
      end
      # Allow only DictAligned for the document itself.
      tree = LayoutSet::All.thunk(Term.of(ppinput), "", toplevel.true? ? LayoutSet::DictAligned : LayoutSet::All)
      flat, _ = flatten(ctx, tree)

      uitree(framectx, styletree(flat), SETTINGS)
    end

    matchpi %{(node_ _* ¦ _ style⋮ "")} do
      part0 = Term::Dict.build do |commit|
        Style.apply(commit, Style.defaults(node), settings)
        Style.apply(commit, style.to(String), settings)
      end

      part1 = styletree.pairspart.transaction do |commit|
        commit.without(:style)

        styletree.items.each_with_index do |item, index|
          commit.with(index, uitree(framectx, item, settings))
        end
      end

      part0 | part1
    end

    otherwise { styletree }
  end
end

def offset(drawable, keypath) : {Term::Num, Term::Num}
  ox = oy = Term[0]

  Keypath.ascend(drawable, keypath) do |node|
    Term.case(node) do
      matchpi %{(viewport _ ¦ _ x: dx_number y: dy_number)} do
        ox += dx
        oy += dy
      end

      otherwise { }
    end
  end

  {ox, oy}
end

def hit(drawable, x : Term::Num, y : Term::Num, sink)
  Keypath.each_item(drawable) do |keypath, node|
    Term.case(node) do
      matchpi %[{¦ l_number t_number final-w: w_number final-h: h_number}] do |l, t, w, h|
        l, t, w, h = {l, t, w, h}.map(&.unsafe_as_n)

        dx, dy = offset(drawable, keypath)
        l -= dx
        t -= dy

        continue unless x.in?(l...l + w)
        continue unless y.in?(t...t + h)

        sink.call(keypath)
      end

      otherwise { }
    end

    true # continue
  end
end

def hit(drawable, x, y)
  Term::Dict.build do |commit|
    hit(drawable, x, y, ->commit.append(Stack(Term)))
  end
end

def color?(term)
  Term.case(term) do
    matchpi %{(r←(%number u8) g←(%number u8) b←(%number u8))} do
      SF::Color.new(r.to(UInt8), g.to(UInt8), b.to(UInt8))
    end

    otherwise { }
  end
end

def present(vote_cursor, layers, layer, frame : Term, dl, dt)
  Term.case(frame) do
    # Any node can specify the cursor.
    matchpi %[{¦ cursor: pointer}] do
      vote_cursor.call(HAND)

      continue
    end

    matchpi %[(text caption_string ¦ rest_ color_ l_: (%number i32) t_: (%number i32))] do
      TextKit::Info.from(rest) do |info|
        sf = SF::Text.new(caption.to(String), info.font, info.size)
        sf.line_spacing = info.leading
        sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)
        sf.color = color?(color) || SF::Color::Black
        sf.letter_spacing = 1

        layers.draw(layer, sf)
      end
    end

    matchpi(
      %[(rect ¦ _ bg_
                  l_: (%number i32)
                  t_: (%number i32)
                  final-w: w←(%number +i32)
                  final-h: h←(%number +i32)
                  border-radius: (%optional 0 border_radius←(%number (whole _) >= 0))
                  ring-l: (%optional 0 ring-l←(%number (whole _) >= 0))
                  ring-r: (%optional 0 ring-r←(%number (whole _) >= 0))
                  ring-t: (%optional 0 ring-t←(%number (whole _) >= 0))
                  ring-b: (%optional 0 ring-b←(%number (whole _) >= 0)))]
    ) do |bg|
      if border_radius.unsafe_as_n > 0
        sf = SF::RoundedRectangleShape.new
        sf.border_radius = border_radius.to(Float64)
        sf = sf.as(SF::Rectangular)
      else
        sf = SF::RectangleShape.new.as(SF::Rectangular)
      end

      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)
      sf.size = SF.vector2i(w.to(Int32), h.to(Int32))

      # Ring works like padding but it's intrinsic to the rect, and not accounted
      # during sizing.
      sf.position -= SF.vector2i(ring_l.to(Int32), ring_t.to(Int32))
      sf.size = sf.size.to_i + SF.vector2i(ring_l.to(Int32)+ring_r.to(Int32), ring_t.to(Int32)+ring_b.to(Int32))

      sf.update

      layers.draw(layer, sf)
    end

    matchpi %[(circle ¦ _ bg_ l_: (%number i32) t_: (%number i32) radius_: (%number +i32))] do |bg|
      sf = SF::CircleShape.new(radius.to(Int32))
      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)

      layers.draw(layer, sf)
    end

    matchpi %[(triangle ¦ _ bg_ l_: (%number i32) t_: (%number i32) final-w: w←(%number +i32) final-h: h←(%number +i32) pointing: left)] do |bg|
      sf = SF::ConvexShape.new
      sf.point_count = 3
      sf[0] = SF.vector2i(0, h.to(Int32) // 2)
      sf[1] = SF.vector2i(w.to(Int32), 0)
      sf[2] = SF.vector2i(w.to(Int32), h.to(Int32))
      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)

      layers.draw(layer, sf)
    end

    matchpi(
      %{(viewport child←{¦ final-w: full-w←(%number +i32)
                           final-h: full-h←(%number +i32)}
         ¦ _ bg_
             l_: (%number +i32)
             t_: (%number +i32)
             final-w: w←(%number +i32)
             final-h: h←(%number +i32)
             x_: (%number i32)
             y_: (%number i32))}
    ) do
      manager = LayerManager.new
      manager.create(0, x: l.to(Int32) + dl, y: t.to(Int32) + dt, w: Math.max(full_w.to(Int32), w.to(Int32)), h: Math.max(full_h.to(Int32), h.to(Int32)), bg: color?(bg) || SF::Color::White, clip: SF.float_rect(x.to(Int32), y.to(Int32), w.to(Int32), h.to(Int32)))

      present(vote_cursor, manager, 0, child, -l.to(Int32), -t.to(Int32))

      texture = manager.collapse(0)

      sf = SF::Sprite.new(texture)
      sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)

      layers.draw(layer, sf)
    end

    matchpi %{(layer child_ ¦ _ l_: (%number +i32) t_: (%number +i32) final-w: w←(%number +i32) final-h: h←(%number +i32) z-index: n←(%number +i32))} do
      return if w.zero? || h.zero?

      layers.create(n.to(Int32), x: l.to(Int32) + dl, y: t.to(Int32) + dt, w: w.to(Int32), h: h.to(Int32))

      present(vote_cursor, layers, n.to(Int32), child, dl: -l.to(Int32), dt: -t.to(Int32))
    end

    matchpi %[_dict] do
      frame.items.each { |child| present(vote_cursor, layers, layer, child, dl, dt) }
    end

    otherwise { }
  end
end

struct LayerManager
  record LayerData, x : Int32, y : Int32, z : Int32, target : SF::RenderTexture

  def initialize
    @layers = [] of LayerData
  end

  # If absent, allocates an absolutely positioned (*x*, *y*), *z*-th layer of width *w* and
  # height *h*. If the layer already exists, does nothing.
  #
  # Layers with higher *z* are drawn on top of those with a lower *z*.
  def create(z : Int32, *, x : Int32, y : Int32, w : Int32, h : Int32, bg = SF::Color::Transparent, clip : SF::FloatRect? = nil) : Nil
    index = @layers.bsearch_index { |other| other.z >= z }

    if index && (other = @layers[index]?)
      return if z == other.z # Already exists
    end

    index ||= @layers.size

    target = SF::RenderTexture.new(w, h)

    if clip
      target.view = SF::View.new(clip)
    end

    target.clear(bg)

    @layers.insert(index, LayerData.new(x, y, z, target))
  end

  def draw(z : Int32, sf : SF::Drawable) : Nil
    layer = @layers.bsearch { |layer| layer.z >= z }

    unless layer && layer.z == z
      raise ArgumentError.new("layer #{z} was not create()'d")
    end

    layer.target.draw(sf)
  end

  def collapse(primary : Int32) : SF::Texture
    base = @layers[primary].target

    @layers.each do |layer|
      next if layer.z == primary

      layer.target.display

      sprite = SF::Sprite.new(layer.target.texture)
      sprite.position = SF.vector2i(layer.x, layer.y)
      base.draw(sprite)
    end


    base.display
    base.texture
  end
end

ARROW = SF::Cursor.from_system(SF::Cursor::Type::Arrow)
HAND  = SF::Cursor.from_system(SF::Cursor::Type::Hand)

def cursor_and_texture(tree : Term) : {SF::Cursor, SF::Texture}
  Term.case(tree) do
    matchpi %{(window child_ ¦ _ bg_ final-w: w←(%number +i32) final-h: h←(%number +i32))} do
      layers = LayerManager.new
      layers.create(0, x: 0, y: 0, w: w.to(Int32), h: h.to(Int32), bg: color?(bg) || SF::Color::White)

      cursor = ARROW
      vote_cursor = ->(proposal : SF::Cursor) do
        return if cursor != ARROW && proposal == ARROW

        cursor = proposal
      end

      present(vote_cursor, layers, 0, child, 0, 0)

      {cursor, layers.collapse(0)}
    end
  end
end

def block(framectx, markup)
  drawable = drawable(framectx, markup)

  Term.case(drawable) do
    matchpi %[{¦ final-w: w←(%number +i32) final-h: h←(%number +i32)}] do
      Term.of(:block, drawable, w: w//SETTINGS.rem, h: h//SETTINGS.rem)
    end
  end
end

struct Cursor
  include Feature

  MARKUP_EMPTY = ML.term <<-WWML
  (x-stack
    (text "" style: "font-mono")
    (rect style: "w-px ring-y-1 bg-blue-500"))
  WWML

  MARKUP_LHS = ML.term <<-WWML
  (x-stack
    (z-stack
      (rect style: "ring-x-px ring-y-1 bg-neutral-700")
      (text ($slot 0) style: "font-mono text-neutral-400"))
    (rect style: "ring-y-1 w-px bg-blue-500"))
  WWML

  MARKUP_RHS = ML.term <<-WWML
  (x-stack
    (layer style: "w-px h-max" z-index: 10
      (rect style: "ring-y-1 bg-blue-500"))
    (z-stack
      (rect style: "ring-x-px ring-y-1 bg-neutral-700")
      (text ($slot 1) style: "font-mono text-neutral-400")))
  WWML

  MARKUP_FULL = ML.term <<-WWML
  (x-stack
    (z-stack style: "h-max"
      (rect style: "ring-x-px ring-y-1 bg-neutral-700")
      (text ($slot 0) style: "font-mono text-neutral-400"))
    (rect style: "ring-y-1 w-px bg-blue-500")
    (z-stack style: "h-max"
      (rect style: "ring-r-px ring-y-1 bg-neutral-700")
      (text ($slot 1) style: "font-mono text-neutral-400")))
  WWML

  private def cursor(lhs, rhs)
    case {lhs.charcount.zero?, rhs.charcount.zero?}
    in {false, false}
      template = MARKUP_FULL
    in {false, true}
      template = MARKUP_LHS
    in {true, false}
      template = MARKUP_RHS
    in {true, true}
      template = MARKUP_EMPTY
    end

    fill(template, lhs, rhs)
  end

  # FIXME: text for caption & hint must be w-max and x-expand mut be max-w-lg or something !!!!!
  #  After we can wrap text of course!
  MARKUP_SUGGESTION1 = ML.term <<-WWML
  (floating
    (layer z-index: 10
      (padding style: "p-1 pt-7"
        (z-stack
          (rect style: "ring-px bg-neutral-600 rounded")
          (rect style: "bg-neutral-800 rounded")
          (x-expand style: "min-w-sm"
            (y-stack style: "w-max"
              (padding style: "p-3"
                (text ($slot 0) style: "font-mono text-neutral-200 font-medium"))
              (rect style: "h-px bg-neutral-600")
              (padding style: "p-3"
                (text ($slot 1) style: "text-sm text-neutral-300"))))))))
  WWML

  private def suggestion1(framectx, name, intro)
    block(framectx, fill(MARKUP_SUGGESTION1, name, intro))
  end

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      # One general suggestion.
      matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions: (suggestions/list () ((name_string intro_string)) ()))} do
        b = block(ctx.data[:framectx], Term.of(:"z-stack", suggestion1(ctx.data[:framectx], name, intro), cursor(lhs, rhs)))
        postfixed(b, postfix)
      end

      matchpi %{[lhs_string | rhs_string (_*) @user]} do
        postfixed(block(ctx.data[:framectx], cursor(lhs, rhs)), postfix)
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct Button
  include Feature

  MARKUP_NORMAL = ML.term <<-WWML
  (z-stack events-to: ($slot 0) hover-id: ($slot 1)
    (rect style: "bg-neutral-700 rounded-sm")
    (padding style: "px-4 py-2"
      (text ($slot 2) style: "text-sm text-neutral-100 font-medium")))
  WWML

  MARKUP_HOVERED = ML.term <<-WWML
  (z-stack events-to: ($slot 0) hover-id: ($slot 1) cursor: pointer
    (rect style: "bg-blue-500 rounded-sm")
    (padding style: "px-4 py-2"
      (text ($slot 2) style: "text-sm text-white font-medium")))
  WWML

  private def button(framectx, mailpath, term, caption : Term, id, hovered : Bool)
    if caption.type.string?
      caption_s = caption.to(String)
    else
      # TODO: use pretty print with forced inline
      caption_s = ML.display(caption, endl: false).gsub(/\s+/, ' ')
    end

    markup = fill(hovered ? MARKUP_HOVERED : MARKUP_NORMAL, mailpath, id, caption_s)

    block(framectx, markup)
  end

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi(
        %{(button caption_ to @_ (_*) ¦ _ mailpath: mp_ id: id_)},
        %{(button caption_ as _ to @_ (_*) ¦ _ mailpath: mp_ id: id_)}
      ) do
        continue unless Rhodium.cursordepth(term, pairspart: true) == -1

        postfixed(button(ctx.data[:framectx], mp, term, caption, id, hovered: ctx.data[:framectx, :hovered]? == id), postfix)
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct Comment
  include Feature

  MARKUP = ML.term <<-WWML
  (x-stack
    (text ";; " style: "font-mono text-sm text-neutral-500")
    (text ($slot 0) style: "font-mono text-sm text-neutral-500"))
  WWML

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi %{(comment desc_string)} do
        wrapped_desc = wrap(desc.to(String), 60).chomp

        postfixed(block(ctx.data[:framectx], fill(MARKUP, wrapped_desc)), postfix)
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

def drawable(framectx, frame) : Term
  x = rewrite(uitree(framectx, frame, SETTINGS), UIR.rewriter)
  # puts ML.display(x)
  x
end

def get_by_id?(frame, id, keypath = Term[])
  Term.case(frame, env: Term[id: id]) do
    matchpi %[{¦ id_}] do
      keypath
    end

    matchpi %{_dict} do
      frame.items.each_with_index do |item, index|
        return get_by_id?(item, id, keypath.append(index)) || next
      end
    end

    otherwise { }
  end
end

def get_by_id(frame, id)
  get_by_id?(frame, id) || raise KeyError.new
end

def annotated(document : Term::Dict) : Term::Dict
  counter = 0
  nodepath = Stack(Int32).new

  while Rhodium.successor?(document, nodepath)
    node0 = Rhodium.follow(document, nodepath)
    node1 = node0

    Term.case(node0) do
      matchpi %{[button caption_ to @_ (_*)]} do
        node1 = Term.of(node0.morph({:"mailpath", Term.of(nodepath).append(4)}, {:"id", counter}))
        counter += 1
      end

      matchpi %{[button caption_ as _ to @_ (_*)]} do
        node1 = Term.of(node0.morph({:"mailpath", Term.of(nodepath).append(6)}, {:"id", counter}))
        counter += 1
      end

      otherwise { }
    end

    next if node0.same?(node1)

    document = Rhodium.assign(document, nodepath, node1)
  end

  document
end

# FIXME:  this should not be a global!!
SETTINGS = Style::Settings.new("IBM Plex Sans", "IBM Plex Mono")

module Soma
  extend self

  WINDOW_WIDTH0  = 1000
  WINDOW_HEIGHT0 = 800

  class AppInterrupt < Exception
  end

  def frame0 : Term
    ML.term <<-WWML
    (window max-w: vw_ max-h: vh_ l: 0 t: 0 style: "max bg-neutral-900"
      (y-stack style: "max fr"
        (z-stack style: "w-max"
          (rect style: "bg-neutral-800")
          (padding style: "p-1"
            (text "Wirewright µsoma" style: "text-xs text-neutral-400")))
        (viewport style: "w-max h-fr bg-neutral-900" x: viewX_ y: viewY_
          (padding style: "pt-16 pl-32"
            (ml toplevel: true only-visible: true
              document_)))))
    WWML
  end

  def instance(frame : Term, framectx : Term::Dict)
    M1.bsubst(frame, framectx.morph({:document, annotated(framectx[:document].as_d)}))
  end

  def handle(prompt : Term, framectx : Term::Dict)
    Term.case(prompt) do
      # Reflect size change in the frame context.
      matchpi %{(size w←(%number u16) h←(%number u16))} do
        continue if {w, h} == {framectx[:vw], framectx[:vh]}

        framectx.morph({:vw, w}, {:vh, h})
      end

      matchpi %{(key np8)} do
        framectx.morph({:viewY, framectx[:viewY] - 1})
      end

      matchpi %{(key S-np8)} do
        framectx.morph({:viewY, framectx[:viewY] - 10})
      end

      matchpi %{(key np4)} do
        framectx.morph({:viewX, framectx[:viewX] - 1})
      end

      matchpi %{(key S-np4)} do
        framectx.morph({:viewX, framectx[:viewX] - 10})
      end

      matchpi %{(key np2)} do
        framectx.morph({:viewY, framectx[:viewY] + 1})
      end

      matchpi %{(key S-np2)} do
        framectx.morph({:viewY, framectx[:viewY] + 10})
      end

      matchpi %{(key np6)} do
        framectx.morph({:viewX, framectx[:viewX] + 1})
      end

      matchpi %{(key S-np6)} do
        framectx.morph({:viewX, framectx[:viewX] + 10})
      end

      matchpi %{(key _)}, %{(input _)} do
        document0 = framectx[:document].as_d
        document1 = Rhodium::Q.of(document0, Rhodium::Events)
          .enqueue(Term.of(:edit, {:edge, :user}, prompt))
          .commit(document0, Rhodium::Events)

        framectx.morph({:document, document1})
      end

      matchpi %{job-completed} do
        # Bump up generation to trigger a re-run of the document.
        framectx.morph({:generation, framectx[:generation] + 1})
      end

      matchpi %{(mouse-press x_number y_number)} do
        handled = false

        # TODO: better ways to detect whether handled or not !!!
        keypaths = hit(framectx[:drawable], x.unsafe_as_n, y.unsafe_as_n)
        keypaths.items.each do |kp|
          target = framectx[:drawable].follow(kp.items)
          next unless mbp = target[:"events-to"]?
          next unless mbp = mbp.as_d?

          document0 = framectx[:document].as_d

          next unless mailbox0 = document0.follow?(mbp.items)
          next unless mailbox0 = mailbox0.as_d?

          handled = true
        end

        unless handled
          return framectx.morph({:pivot, {x, y}})
        end

        framectx
      end

      matchpi %{(mouse-release x_number y_number)} do
        if framectx[:pivot]?
          return framectx.without(:pivot)
        end

        keypaths = hit(framectx[:drawable], x.unsafe_as_n, y.unsafe_as_n)
        keypaths.items.each do |kp|
          target = framectx[:drawable].follow(kp.items)
          next unless mbp = target[:"events-to"]?
          next unless mbp = mbp.as_d?

          document0 = framectx[:document].as_d

          next unless mailbox0 = document0.follow?(mbp.items)
          next unless mailbox0 = mailbox0.as_d?

          mailbox1 = mailbox0.append({:press})
          document1 = document0.follow(mbp.items) { Term.of(mailbox1) }
          framectx = framectx.morph({:document, document1})
        end

        framectx
      end

      matchpi %{(motion x_number y_number)} do
        if pivot = framectx[:pivot]?
          px, py = pivot
          return framectx.morph(
            {:viewX, framectx[:viewX] + (px - x)},
            {:viewY, framectx[:viewY] + (py - y)},
            {:pivot, {x, y}},
          )
        end

        keypaths = hit(framectx[:drawable], x.unsafe_as_n, y.unsafe_as_n)
        framectx0 = framectx
        handled = false
        keypaths.items.each do |kp|
          target0 = framectx[:drawable].as_d.follow(kp.items)
          next unless id = target0[:"hover-id"]?

          handled = true
          framectx = framectx.morph({:hovered, id})
        end

        unless handled
          framectx = framectx.morph({:hovered, nil})
        end

        framectx
      end

      matchpi %{exit} do
        raise AppInterrupt.new
      end

      otherwise { framectx }
    end
  end

  def peek(prompts : Channel(Term), framectx : Term::Dict)
    select
    when prompt = prompts.receive
      handle(prompt, framectx)
    else
      framectx
    end
  end

  def wait(prompts : Channel(Term), framectx : Term::Dict)
    handle(prompts.receive, framectx)
  end

  # TODO: this is obviously too bloated & hacky. Improve. We also want to support
  #       multiple parallel documents!!!
  #
  # The primary thread is reacting to UI *prompts* and responding with *drawables*.
  # The primary thread also serves as the bridge between UI and delta7.
  def primary(prompts : Channel(Term), drawables : Channel(Term), nitrene : Nitrene::JobContext, seed : Term::Dict) : Nil
    frame = frame0

    framectx0 = Term[generation: 0, vw: WINDOW_WIDTH0, vh: WINDOW_HEIGHT0, viewX: 0, viewY: 0, document: seed, drawable: Term[]]

    should_draw = false

    show = -> do
      return unless should_draw

      dw = drawable(framectx0, instance(frame, framectx0))
      framectx0 = framectx0.morph({:drawable, dw})
      drawables.send(dw)
    end

    rendezvous = D7::Step.new do |document|
      framectx1 = peek(prompts, framectx0.morph({:document, document}))
      unless framectx0 == framectx1
        redraw = false

        if framectx0.without(:document) == framectx1.without(:document)
          # NOTE: currently, drawing is very slow compared to D7 (duh...). So we check
          # if the visible part of the document changed and if it did not (e.g. some events),
          # then we skip reporting the document. If it did change then we block until
          # the user has the chance to see the document.
          redraw = D7.visible(framectx0[:document].as_d) != D7.visible(framectx1[:document].as_d)
        else
          redraw = true
        end

        framectx0 = framectx1
        if redraw
          show.call
        end
      end

      # We do not modify the document and therefore never need to trigger
      # a transition.
      {framectx1[:document].as_d, false}
    end

    initial = true
    settled = false

    edited = false

    check_should_draw = D7::Step.new do |document|
      events = Rhodium::Q.of(document, Rhodium::Events)

      if event = events.first?
        Term.case(event) do
          matchpi %{(edit @_ _)} { edited = true }
          otherwise { }
        end
      else
        should_draw = !edited
        edited = false
      end

      # We do not modify the document and therefore we never trigger
      # a transition.
      {document, false}
    end

    while true
      should_draw = true

      # Force initial redraw; and redraw before settling. On the latter,
      # since rendezvous will only redraw periodically, it may happen that
      # we loose a frame due to it settling before it can draw. Forcing
      # a redraw after settling solves this.
      show.call

      should_draw = false

      while settled
        framectx1 = wait(prompts, framectx0)
        next if framectx0 == framectx1

        framectx0 = framectx1
        show.call

        settled = false
      end

      seed0 = framectx0[:document].as_d
      seed1 = D7.run(seed0,
        log: D7::Log::None.new,
        transition: Rhodium.transition,
        step: D7.steps(check_should_draw, Rhodium.step, Nitrene.step(nitrene), rendezvous),
        goal: D7::Goal.none,
        initial: initial,
      )

      framectx0 = framectx0.morph({:document, seed1})

      initial = false
      settled = true
    end
  rescue AppInterrupt
  end

  # The main thread is doing the majority of SFML stuff.
  #
  # - It's showing the window.
  # - It's receiving drawable frames from the primary thread (the one doing Soma).
  # - It's sending events to the primary thread via a channel (*prompts*).
  #
  # NOTE: we call SFML/UI events *prompts* to distinguish them from document events,
  # which are simply called *events*.
  def main(prompts : Channel(Term), drawables : Channel(Term)) : Nil
    window = SF::RenderWindow.new(SF::VideoMode.new(WINDOW_WIDTH0, WINDOW_HEIGHT0), title: "Wirewright µsoma", settings: SF::ContextSettings.new(depth: 24, antialiasing: 8))
    window.framerate_limit = 60

    # Hard-wait for the initial drawable.
    cursor, texture = cursor_and_texture(drawables.receive)

    window.mouse_cursor = cursor

    while window.open?
      while event = window.poll_event
        transcribed = nil

        case event
        when SF::Event::Closed
          window.close

          transcribed = Term.of(:exit)
        when SF::Event::Resized
          window.view = SF::View.new(SF.float_rect(0, 0, event.width, event.height))

          transcribed = Term.of(:size, event.width, event.height)
        when SF::Event::TextEntered
          chr = event.unicode.chr
          next unless chr.printable?

          transcribed = Term.of(:input, chr)
        when SF::Event::MouseButtonPressed
          transcribed = Term.of(:"mouse-press", event.x, event.y)
        when SF::Event::MouseButtonReleased
          transcribed = Term.of(:"mouse-release", event.x, event.y)
        when SF::Event::MouseMoved
          transcribed = Term.of(:motion, event.x, event.y)
        when SF::Event::KeyPressed
          keyname = nil
          case event.code
          when .escape?    then keyname = "escape"
          when .tab?       then keyname = "tab"
          when .home?      then keyname = "home"
          when .end?       then keyname = "end"
          when .enter?     then keyname = "enter"
          when .delete?    then keyname = "delete"
          when .left?      then keyname = "left"
          when .right?     then keyname = "right"
          when .up?        then keyname = "up"
          when .down?      then keyname = "down"
          when .backspace? then keyname = "backspace"
          when .numpad2?   then keyname = "np2"
          when .numpad4?   then keyname = "np4"
          when .numpad5?   then keyname = "np5"
          when .numpad6?   then keyname = "np6"
          when .numpad8?   then keyname = "np8"
          end

          if event.control
            case event.code
            when .c? then keyname = "c"
            when .v? then keyname = "v"
            end
          end

          next unless keyname

          keyname = "S-#{keyname}" if event.shift
          keyname = "C-#{keyname}" if event.control
          key = Term::Sym.new(keyname)

          transcribed = Term.of(:key, key)
        end

        if transcribed
          prompts.send(transcribed)
        end
      end

      select
      when drawable = drawables.receive
        cursor, texture = cursor_and_texture(drawable)
        window.mouse_cursor = cursor
      else
      end

      window.clear(SF::Color::White)
      sprite = SF::Sprite.new(texture)
      window.draw(sprite)
      window.display
    end
  end

  def seed : Term::Dict
    seed = ML.terms <<-WWML
      (comment "Welcome to Wirewright µsoma!")
      (cell 0 @count)
      (button "Increment" as 1 to @deltas ())
      (button "Decrement" as -1 to @deltas ())
      (transform (@deltas delta_number) to @counts with @count (+ count delta))
      (latest @counts @count)
      ("" | "" () @user)
    WWML

    seed.as_d
  end

  def launch : Nil
    prompts = Channel(Term).new(1024)
    drawables = Channel(Term).new

    nitrene = Nitrene::JobContext.new

    ctx0 = ExecutionContext::MultiThreaded.new("Nitrene Alarm", 1)
    ctx0.spawn do
      while true
        nitrene.alarm.receive
        prompts.send(Term.of(:"job-completed"))
      end
    rescue Channel::ClosedError
      # Noop. We just stop polling.
    end

    ctx1 = ExecutionContext::MultiThreaded.new("Soma: Primary", 1)
    ctx1.spawn do
      primary(prompts, drawables, nitrene, seed)
      nitrene.alarm.close
    end

    main(prompts, drawables)
  end
end

Soma.launch
