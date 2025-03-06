# TODO: how to support wrapping? items that do not fit into w: max should be able to wrap

require "./src/wirewright"
require "./pprint2"
require "./colors"
require "crsfml"
require "./sfml_util"

module ::Ww::Keypath
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
  # NOTE: methods of this module read and sometimes mutate global caches. They are
  # intentially not thread-safe. Interaction with SFML should be limited to the
  # main thread.
  module FontManager
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

    # Returns the path to *font* with the given *weight*.
    #
    # - If *weight* does not exist for *font* tries to fall back to lower values
    #   of *weight*.
    # - If still nothing, tries to fall back on higher values of *weight*.
    # - If still nothing, returns `nil`.
    def self.path?(font : String, weight : Int32) : Path?
      @@paths.put_if_absent({font, weight}) { path0?(font, weight) }
    end

    @@cache = {} of Path => SF::Font

    def self.font_at(path : Path) : SF::Font
      @@cache.put_if_absent(path) do
        SF::Font.from_file(path.to_s)
      end
    end
  end

  record Info, font : SF::Font, size : Int32, leading : Float32 do
    def self.from?(term : Term)
      Term.case(term) do
        matchpi %[{¦ font_string weight_: (%any 100 200 300 400 450 500 600 700 800 900) size_: (%number u8) leading_number}] do
          next unless path = FontManager.path?(font.to(String), weight.to(Int32))

          new(FontManager.font_at(path), size.to(Int32), leading.to(Float32))
        end

        otherwise { }
      end
    end
  end

  def self.measure(content : String, font : String, weight : Int32, points : Int32, leading : Float64) : {Int32, Int32}
    unless path = FontManager.path?(font, weight)
      return 0, 0
    end

    if content.empty?
      text = SF::Text.new(" ", FontManager.font_at(path), points)
      size = text.size
      {0, size.y}
    else
      text = SF::Text.new(content, FontManager.font_at(path), points)
      size = text.size
      {size.x, size.y}
    end
  end
end

module UIR
  # TODO: Q: where to put this?
  #       A: in the uiR.soma file. After we have precedence for <> ->. Inside
  #          a (stage preprocess ...). The rewriter should be there as well!
  PREPROCESS = <<-WWML
  ;; Cascade width/height to single children.
  (_ {¦ -w_} ¦ _ w: W_) <> {w: →W}
  (_ {¦ -h_} ¦ _ h: H_) <> {h: →H}

  ;; Map w/h: N to w/h: max max-w/h: N
  {¦ w_number -max-w_} <> {w: max, max-w: →w}
  {¦ h_number -max-h_} <> {h: max, max-h: →h}

  ;; Map w/h: (fr N) to w/h: max fr: N
  {¦ w_: (fr n_number) -fr_} <> {w: max, fr: →n}
  {¦ h_: (fr n_number) -fr_} <> {h: max, fr: →n}
  WWML

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
          leading.to(Float64),
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
        set.call choiceR(
          rulesetR(Ruleset.select(selector, ML.terms(base)), noR, backmapR, noR),
          itemsR(rec),
        ),
      )
    end

    chainR(baseR.call(PREPROCESS), baseR.call(staging))
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


module Style
  extend self

  record Settings, sans : String, mono : String, rem : Int32 = 16

  def defaults(node : Term) : String
    Term.case(node) do
      matchpi %{text} do
        "content font-sans text-base text-black font-text"
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
      when "rounded-md"
        commit.with(:"border-radius", 0.375 * settings.rem)
      when "rounded-lg"
        commit.with(:"border-radius", 0.5 * settings.rem)
      when "rounded-xl"
        commit.with(:"border-radius", 0.75 * settings.rem)
      when "rounded-2xl"
        commit.with(:"border-radius", 1 * settings.rem)
      when "rounded-3xl"
        commit.with(:"border-radius", 1.5 * settings.rem)
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
def uitree(styletree : Term, settings : Style::Settings)
  Term.of_case(styletree) do
    matchpi %{(ml child_ ¦ _ toplevel_boolean only-visible_boolean)} do
      chain = ML::Display::MAIN_CHAIN.prepend(Cursor.new).prepend(Button.new).prepend(Comment.new)
      if only_visible.true?
        pair_chain = ML::Display::PAIR_CHAIN.prepend(HiddenPairs.new)
      else
        pair_chain = ML::Display::PAIR_CHAIN
      end
      ctx = DisplayContext.new(60, 120, chain, pair: pair_chain)

      ppinput = Term.of(child)
      # Allow only DictAligned for the document itself.
      tree = LayoutSet::All.thunk(ppinput, "", toplevel.true? ? LayoutSet::DictAligned : LayoutSet::All)
      flat, _ = flatten(ctx, tree)

      uitree(styletree(flat), SETTINGS)
    end

    matchpi %{(node_ _* ¦ _ style⋮ "")} do
      part0 = Term::Dict.build do |commit|
        Style.apply(commit, Style.defaults(node), settings)
        Style.apply(commit, style.to(String), settings)
      end

      part1 = styletree.pairspart.transaction do |commit|
        commit.without(:style)

        styletree.items.each_with_index do |item, index|
          commit.with(index, uitree(item, settings))
        end
      end

      part0 | part1
    end

    otherwise { styletree }
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

def present(layers, layer, frame : Term)
  Term.case(frame) do
    matchpi %[(text caption_string ¦ rest_ color_ l_: (%number i32) t_: (%number i32))] do
      next unless info = TextKit::Info.from?(rest)

      sf = SF::Text.new(caption.to(String), info.font, info.size)
      sf.line_spacing = info.leading
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
      sf.color = color?(color) || SF::Color::Black
      sf.letter_spacing = 1

      layers.draw(layer, sf)
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
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
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
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))

      layers.draw(layer, sf)
    end

    matchpi %[(triangle ¦ _ bg_ l_: (%number i32) t_: (%number i32) final-w: w←(%number +i32) final-h: h←(%number +i32) pointing: left)] do |bg|
      sf = SF::ConvexShape.new
      sf.point_count = 3
      sf[0] = SF.vector2i(0, h.to(Int32) // 2)
      sf[1] = SF.vector2i(w.to(Int32), 0)
      sf[2] = SF.vector2i(w.to(Int32), h.to(Int32))
      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))

      layers.draw(layer, sf)
    end

    matchpi %{(layer child_ ¦ _ l_: (%number +i32) t_: (%number +i32) final-w: w←(%number +i32) final-h: h←(%number +i32) z-index: n←(%number +i32))} do
      return if w.zero? || h.zero?

      layers.create(n.to(Int32), x: l.to(Int32), y: t.to(Int32), w: w.to(Int32), h: h.to(Int32))

      present(layers, n.to(Int32), child)
    end

    matchpi %[_dict] do
      frame.items.each { |child| present(layers, layer, child) }
    end

    otherwise { }
  end
end

struct LayerManager
  record LayerData, x : Int32, y : Int32, z : Int32, target : SF::RenderTexture

  def initialize
    @layers = [] of LayerData
  end

  # Allocates an absolutely positioned (*x*, *y*), *z*-th layer of width *w* and
  # height *H* if absent. If the layer already exists, does nothing.
  #
  # Layers with higher *z* are drawn on top of those with a lower *z*.
  def create(z : Int32, *, x : Int32, y : Int32, w : Int32, h : Int32, bg = SF::Color::Transparent) : Nil
    index = @layers.bsearch_index { |other| other.z >= z }

    if index && (other = @layers[index]?)
      return if z == other.z # Already exists
    end

    index ||= @layers.size

    target = SF::RenderTexture.new(w, h)
    target.clear(bg)
    target.view = SF::View.new(SF.float_rect(x, y, w, h))

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

def texture(tree : Term) : SF::Texture
  Term.case(tree) do
    matchpi %{(window child_ ¦ _ bg_ final-w: w←(%number +i32) final-h: h←(%number +i32))} do
      layers = LayerManager.new
      layers.create(0, x: 0, y: 0, w: w.to(Int32), h: h.to(Int32), bg: color?(bg) || SF::Color::White)

      present(layers, 0, child)

      layers.collapse(0)
    end
  end
end

def block(markup)
  drawable = pipe(markup, uitree(SETTINGS), rewrite(UIR.rewriter))

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
    (rect w: 1 ring-t: 5 ring-b: 5 style: "h-max bg-blue-500"))
  WWML

  MARKUP_LHS = ML.term <<-WWML
  (x-stack
    (z-stack
      (rect ring-l: 1 ring-r: 1 ring-t: 5 ring-b: 5 style: "max bg-neutral-700")
      (text ($slot 0) style: "font-mono text-neutral-400"))
    (rect w: 1 ring-t: 5 ring-b: 5 style: "h-max bg-blue-500"))
  WWML

  MARKUP_RHS = ML.term <<-WWML
  (x-stack
    (layer w: 1 style: "h-max" z-index: 10
      (rect ring-t: 5 ring-b: 5 style: "max bg-blue-500"))
    (z-stack
      (rect ring-l: 1 ring-r: 1 ring-t: 5 ring-b: 5 style: "max bg-neutral-700")
      (text ($slot 1) style: "font-mono text-neutral-400")))
  WWML

  MARKUP_FULL = ML.term <<-WWML
  (x-stack
    (z-stack style: "h-max"
      (rect ring-l: 1 ring-r: 1 ring-t: 5 ring-b: 5 style: "max bg-neutral-700")
      (text ($slot 0) style: "font-mono text-neutral-400"))
    (rect w: 1 ring-t: 5 ring-b: 5 style: "h-max bg-blue-500")
    (z-stack style: "h-max"
      (rect ring-l: 0 ring-r: 1 ring-t: 5 ring-b: 5 style: "max bg-neutral-700")
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

    markup = fill(template) do |slot, commit|
      case slot
      when 0 then commit << lhs
      when 1 then commit << rhs
      end
    end

    block(markup)
  end

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi %{[lhs_string | rhs_string (_*) @user]} do
        postfixed(cursor(lhs, rhs), postfix)
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct Button
  include Feature

  MARKUP = ML.term <<-WWML
  (z-stack
    (rect style: "max bg-blue-500 rounded-sm")
    (padding style: "px-4 py-2"
      (text ($slot 0) style: "text-sm text-white font-medium")))
  WWML

  private def button(term, caption : Term)
    if caption.type.string?
      caption_s = caption.to(String)
    else
      # TODO: use pretty print with forced inline
      caption_s = ML.display(caption, endl: false).gsub(/\s+/, ' ')
    end

    markup = fill(MARKUP) do |slot, commit|
      case slot
      when 0 then commit << caption_s
      end
    end

    block(markup)
  end

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi(
        %{[button caption_ to @_ (_*)]},
        %{[button caption_ as _ to @_ (_*)]}
      ) do
        continue unless Rhodium.cursordepth(term, pairspart: true) == -1

        postfixed(button(term, caption), postfix)
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

        postfixed(block(fill(MARKUP, wrapped_desc)), postfix)
      end

      otherwise do
        rest.call(ctx, term, postfix)
      end
    end
  end
end

struct HiddenPairs
  include Feature

  def call(ctx, term, postfix, head, rest)
    Term.case(term) do
      matchpi %{(key_ _)}, %{(indented key_ _)} do
        continue unless Rhodium.internal_key?(key)

        Term.of(:frag, postfix)
      end

      otherwise { rest.call(ctx, term, postfix) }
    end
  end
end

def frame_texture(frame)
  pipe(frame, uitree(SETTINGS), rewrite(UIR.rewriter), texture)
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

frame = ML.term <<-WWML
(window w: $<vw> h: $<vh> l: 0 t: 0 style: "bg-neutral-900"
  (z-stack style: "w-max"
    ;;(layer z-index: 999
    ;;  (text "Hello World" id: printed style: "text-red-300 text-xs"))
    (y-stack style: "w-max" fractions: true
      (z-stack style: "w-max"
        (rect style: "w-max h-max bg-neutral-800")
        (padding style: "p-1"
          (text "Wirewright µsoma" style: "text-xs text-neutral-400")))
      (padding style: "pl-32 pt-16 h-max" fr: 1
        (ml id: document toplevel: true only-visible: true
          ((comment "Welcome to Wirewright µsoma!")
           (button "Increment" as 1 to @actions ())
           (button "Decrement" as -1 to @actions ())
           ("" | "" () @user)))))))
WWML

SETTINGS = Style::Settings.new("IBM Plex Sans", "IBM Plex Mono")

window = SF::RenderWindow.new(SF::VideoMode.new(800, 600), title: "Hello World")
window.framerate_limit = 60

frame1 = Keypath.plug(Term.of(window.size.x), Term.of(:"$<vw>"), frame)
frame1 = Keypath.plug(Term.of(window.size.y), Term.of(:"$<vh>"), frame1)
texture0 = frame_texture(frame1)

while window.open?
  docpath = get_by_id(frame, Term.of(:document)).append(1)
  document0 = document1 = Keypath.follow(docpath, frame).as_d
  force_redraw = false

  while event = window.poll_event
    motion = nil

    case event
    when SF::Event::Closed then window.close
    when SF::Event::Resized
      force_redraw = true
      visible_area = SF.float_rect(0, 0, event.width, event.height)
      window.view = SF::View.new(visible_area)
    when SF::Event::TextEntered
      chr = event.unicode.chr
      next unless chr.printable?
      motion = Term.of(:input, chr)
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
      when .numpad8?   then keyname = "np8"
      when .numpad5?   then keyname = "np5"
      when .numpad2?   then keyname = "np2"
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

      motion = Term.of(:key, key)
    end

    if motion
      document1 = Rhodium::Q.of(document1, Rhodium::Events)
        .enqueue(:edit, {:edge, :user}, motion)
        .commit(document1, Rhodium::Events)
    end
  end

  if force_redraw || !document0.same?(document1)
    document1 = D7.run(document1,
      log: D7::Log::None.new,
      transition: Rhodium.transition,
      step: Rhodium.step,
      goal: D7::Goal.none,
      initial: true,
    )

    # frame = Keypath.assign(Term.of(ML.display(Term.of(document1))), frame, get_by_id(frame, Term.of(:printed)).append(1))

    frame = Keypath.assign(Term.of(document1), frame, docpath)

    frame1 = Keypath.plug(Term.of(window.size.x), Term.of(:"$<vw>"), frame)
    frame1 = Keypath.plug(Term.of(window.size.y), Term.of(:"$<vh>"), frame1)
    texture0 = frame_texture(frame1)
  end

  window.clear(SF::Color::White)
  sprite = SF::Sprite.new(texture0)
  window.draw(sprite)
  window.display
end
# res = rewrite(tree, UIR.rewriter)
# puts ML.display(layers(res, 0))
