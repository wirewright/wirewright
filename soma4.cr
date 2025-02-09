require "crsfml"
require "./wirewright"
require "./baz5"
require "./sfml_util"
require "./oklch"

preprocess = <<-WWML
;; Convert two-child row to an `x-joint`.
[H←row/binary _ _] <> {H: x-joint}

;; Convert two-child column to a `y-joint`.
[H←col/binary _ _] <> {H: y-joint}

;; Convert two-child row with a gap to an `x-joint`.
(H←row/binary _ b_ ¦ () w_ h_ gap_number) <>
  {H: x-joint,
   b: (padding →b w: →w h: →h pt: 0 pb: 0 pl: →gap pr: 0),
   (gap): ()}

;; Convert two-child column with a gap to a `y-joint`.
(H←col/binary _ b_ ¦ () w_ h_ gap_number) <>
  {H: y-joint,
   b: (padding →b w: →w h: →h pt: →gap pb: 0 pl: 0 pr: 0),
   (gap): ()}

;; Binarize multi-child column.
(col/binary a_ b_ _+ ¦ () w_ h_)
  <> {a: (col/binary →a →b w: →w h: →h), (b): ()}
(col/binary a_ b_ _+ ¦ () gap_number w_ h_)
  <> {a: (col/binary →a →b w: →w h: →h gap: →gap), (b): ()}

;; Binarize multi-child row.
(row/binary a_ b_ _+ ¦ () w_ h_)
  <> {a: (row/binary →a →b w: →w h: →h), (b): ()}
(row/binary a_ b_ _+ ¦ () gap_number w_ h_)
  <> {a: (row/binary →a →b w: →w h: →h gap: →gap), (b): ()}

;; (row _ _ ...) -> (row/root (row/binary _ _ ...))
N←(H←row _+ ¦ () w_ h_) <> {H: row/binary, N: (row/root ↑N w: →w h: →h)}
N←(H←row _+ ¦ () w_ h_ gap_number) <> {H: row/binary, N: (row/root ↑N w: →w h: →h)}

;; (col _ _ ...) -> (col/root (col/binary _ _ ...))
N←(H←col _+ ¦ () w_ h_) <> {H: col/binary, N: (col/root ↑N w: →w h: →h)}
N←(H←col _+ ¦ () w_ h_ gap_number) <> {H: col/binary, N: (col/root ↑N w: →w h: →h)}

N←(button caption_string ¦ () bg⋮ (oklch 0.922 0 0) fg⋮ (oklch 0.371 0 0) w_ h_) <>
  {N: (box w: →w h: →h bg: →bg border-radius: 3
        (padding pl: 21 pr: 21 pt: 11 pb: 11
          (x-center
            (text w: content h: content fg: →fg font: "IBM Plex Sans" size: 14 weight: 500
              →caption))))}

N←(button.primary caption_string ¦ () w_ h_) <>
  {N: (button →caption fg: (oklch 1.0 0 0) bg: (oklch 0.623 0.214 259.815) w: →w h: →h)}

N←(button.secondary caption_string ¦ () w_ h_) <>
  {N: (button →caption fg: (oklch 0.922 0 0) bg: (oklch 0.269 0 0) w: →w h: →h)}
WWML

base1 = <<-WWML
(text caption_string ¦ _ -content-w_ -content-h_ font_string weight_: (%any 100 200 300 400 450 500 600 700 800 900) size_: (%number u8))
  <> {content-w: ($once (measure-width →caption →font →weight →size)),
      content-h: ($once (measure-height →caption →font →weight →size))}

(padding {_ content-w: w_number} ¦ _ content-w: (%- _ W) pl_number pr_number)
  <> {W: ($once (+ →pl →w →pr))}
(padding {_ content-h: h_number} ¦ _ content-h: (%- _ H) pt_number pb_number)
  <> {H: ($once (+ →pt →h →pb))}

;; Cascade width/height to single children.
(_ {_ -w_} ¦ _ w: W_) <> {w: →W}
(_ {_ -h_} ¦ _ h: H_) <> {h: →H}

(_ {_ content-w: w_number} ¦ _ content-w: (%- _ W)) <> {W: →w}
(_ {_ content-h: h_number} ¦ _ content-h: (%- _ H)) <> {H: →h}

(_ {_ max-w: (%- _ w)} ¦ _ max-w: W_number) <> {w: →W}
(_ {_ max-h: (%- _ h)} ¦ _ max-h: H_number) <> {h: →H}

(padding {_ max-w: (%- _ w)} ¦ _ max-w: W_number pl_number pr_number)
  <> {w: ($once (- →W →pl →pr))}
(padding {_ max-h: (%- _ h)} ¦ _ max-h: H_number pt_number pb_number)
  <> {h: ($once (- →H →pt →pb))}

{_ w: content content-w_number -final-w_} <> {final-w: →content-w}
{_ h: content content-h_number -final-h_} <> {final-h: →content-h}

{_ w: max max-w_number -final-w_} <> {final-w: →max-w}
{_ h: max max-h_number -final-h_} <> {final-h: →max-h}

(padding {_ -l_} ¦ _ l: L_number pl_number) <> {l: ($once (+ →L →pl))}
(padding {_ -t_} ¦ _ t: T_number pt_number) <> {t: ($once (+ →T →pt))}

;; Row root propagates max-sibling-h to children.
(row/root {_ max-sibling-h: (%- _ h)} ¦ _ content-h: H_number)
  <> {h: →H}

;; x-joint propagates max-sibling-h to both of its children.
;;
;; NOTE: as far as I understand, it is impossible to have one of them absent and
;; the other not.
(x-joint {_ max-sibling-h: (%- _ h)} {_ max-sibling-h: (%- _ h)} ¦ _ max-sibling-h: H_number)
  <> {h: →H}

(x-joint {_ content-w: w1_number} {_ content-w: w2_number} ¦ _ content-w: (%- _ W))
  <> {W: ($once (+ →w1 →w2))}
(x-joint {_ content-h: h1_number} {_ content-h: h2_number} ¦ _ content-h: (%- _ H))
  <> {H: ($once (max →h1 →h2))}

;; x-joint with h: content sets max-h of its children to maximum content-h of
;; its children.
(x-joint {_ -max-h_} _ ¦ _ h: content max-sibling-h: H_number)
  <> {max-h: →H}
(x-joint _ {_ -max-h_} ¦ _ h: content max-sibling-h: H_number)
  <> {max-h: →H}

;; TODO: handle growth factor?
(x-joint {_ -max-w_} _ ¦ _ max-w: W_number)
  <> {max-w: ($once (// →W 2))}
(x-joint _ {_ -max-w_} ¦ _ max-w: W_number)
  <> {max-w: ($once (// →W 2))}

;; Column root propagates max-sibling-w to children.
(col/root {_ max-sibling-w: (%- _ w)} ¦ _ content-w: W_number)
  <> {w: →W}

;; y-joint propagates max-sibling-w to both of its children.
;;
;; NOTE: as far as I understand, it is impossible to have one of them absent and
;; the other not.
(y-joint {_ max-sibling-w: (%- _ w)} {_ max-sibling-w: (%- _ w)} ¦ _ max-sibling-w: W_number)
  <> {w: →W}

(y-joint {_ content-w: w1_number} {_ content-w: w2_number} ¦ _ content-w: (%- _ W))
  <> {W: ($once (max →w1 →w2))}
(y-joint {_ content-h: h1_number} {_ content-h: h2_number} ¦ _ content-h: (%- _ H))
  <> {H: ($once (+ →h1 →h2))}

;; y-joint with w: content sets max-w of its children to max-sibling-w
(y-joint {_ -max-w_} _ ¦ _ w: content max-sibling-w: W_number)
  <> {max-w: →W}
(y-joint _ {_ -max-w_} ¦ _ w: content max-sibling-w: W_number)
  <> {max-w: →W}

;; FIXME: The moment you do something like this (%not) in a rule system, is exactly
;; the moment when the rule system becomes a COMPLETE AND UTTER PILE OF POO !
((%not x-center offset) {_ -l_} ¦ _ l: L_number) <> {l: →L}
((%not y-center offset) {_ -t_} ¦ _ t: T_number) <> {t: →T}

(x-center {_ -l_ final-w: w_number} ¦ _ l: L_number final-w: W_number)
  <> {l: ($ (+ →L (// (- →W →w) 2)))}
(y-center {_ -t_ final-h: h_number} ¦ _ t: T_number final-h: H_number)
  <> {t: ($ (+ →T (// (- →H →h) 2)))}

(x-joint {_ l: (%- _ l1) final-w: w1_number} {_ l: (%- _ l2)} ¦ _ l: L_number)
  <> {l1: →L, l2: ($once (+ →L →w1))}
(x-joint {_ -t_} {_ -t_} ¦ _ t: T_number)
  <> {t: →T}

(y-joint {_ -l_} {_ -l_} ¦ _ l: L_number)
  <> {l: →L}
(y-joint {_ t: (%- _ t1) final-h: h1_number} {_ t: (%- _ t2)} ¦ _ t: T_number)
  <> {t1: →T, t2: ($once (+ →T →h1))}

(offset {_ -l_} ¦ _ l: L_number x: dx_number) <> {l: ($once (+ →L →dx))}
(offset {_ -t_} ¦ _ t: T_number y: dy_number) <> {t: ($once (+ →T →dy))}
WWML

# CONFIGURE REWRITER

FONT_SANS = SF::Font.from_file("./fonts/IBMPlexSans-Text.otf")
FONT_MONO = SF::Font.from_file("./fonts/IBMPlexMono-Text.otf")

def measure(string : String, font : String, weight : Int32, character_size) : {Int32, Int32}
  unless path = FontManager.path?(font, weight)
    return 0, 0
  end

  text = SF::Text.new(string, FontManager.font_at(path), character_size)
  size = text.size
  {size.x, size.y}
end

primitives = ProcRuleset.build do
  rulepi1 %[(+ ns_number+)] { ns.items.reduce { |a, b| a + b } }
  rulepi1 %[(- ns_number+)] { ns.items.reduce { |a, b| a - b } }
  rulepi1 %[(* a_number b_number)] { a * b }
  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
  rulepi1 %[(// a_number (%all b_number (%not 0)))] { a // b }

  rulepi1 %[(> a_number b_number)] do
    a.unsafe_as_n > b.unsafe_as_n
  end

  rulepi1 %[(tally xs_dict)] do
    xs.unsafe_as_d.size
  end

  rulepi1 %[(min ns_number+)] do
    ns.items.min_by(&.unsafe_as_n)
  end

  rulepi1 %[(max ns_number+)] do
    ns.items.max_by(&.unsafe_as_n)
  end

  # rulepi %[(measure text_string limit: (w←(%number +i32) h←(%number +i32)))] do
  #   wrapped = wrap(text.to(String), w.to(Int32), h.to(Int32))
  #   wrapped_w = wrapped.each_line(chomp: true).max_of(&.size)
  #   wrapped_h = wrapped.each_line.size

  #   Rewrite.one({wrapped_w, wrapped_h})
  # end

  rulepi1 %[(measure-width text_string font_string weight←(%any 100 200 300 400 450 500 600 700 800 900) size←(%number u8))] do
    w, _ = measure(text.to(String), font.to(String), weight.to(Int32), size.to(Int32))
    w
  end

  rulepi1 %[(measure-height text_string font_string weight←(%any 100 200 300 400 450 500 600 700 800 900) size←(%number u8))] do
    _, h = measure(text.to(String), font.to(String), weight.to(Int32), size.to(Int32))
    h
  end
end

refR = dfsR(
  switchR(
    { %[($my rewritee_)], envR(Term.of(:"$my")) },
    { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
    { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
  )
)

evalR = dfsR(
  switchR(
    { %[($ rewritee_)], exhR(dfsR(callR(primitives))) },
    { %[($once rewritee_)], callR(primitives) },
  )
)

backmapR = chainR(refR, evalR)

selector = ML.term(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])

def baseR(backmapR, selector, base)
  set, rec = recR

  exhR(
    set.call choiceR(
      rulesetR(Ruleset.select(selector, ML.terms(base)), noR, backmapR, noR),
      itemsR(rec),
    ),
  )
end

uiR = chainR(
  baseR(backmapR, selector, preprocess),
  baseR(backmapR, selector, base1),
)

# SHOW

def color?(term : Term)
  Term.case(term) do
    matchpi %[(oklch l←(%number 0 <= _ <= 1) c←(%number 0 <= _ <= 0.36) h←(%number 0 <= _ <= 360))] do
      r, g, b = Oklch.to_rgb((l.to(Float64)*100).clamp(0.0..100.0), c.to(Float64), h.to(Float64))

      SF::Color.new(r, g, b)
    end

    otherwise { }
  end
end

# NOTE: methods of this module read and sometimes mutate global caches. They are
# intentially not thread-safe. Interaction with SFML should be limited to the
# main thread.
module FontManager
  WEIGHTS = {
    100 => "Thin",
    200 => "ExtraLight",
    300 => "Light",
    400 => "Regular",
    450 => "Text",
    500 => "Medium",
    600 => "SemiBold",
    700 => "Bold",
    800 => "ExtraBold",
    900 => "Black"
  }

  def self.path?(font : String, weight : Int32) : Path?
    return unless postfix = WEIGHTS[weight]?

    Path[__DIR__] / "fonts" / "#{font.delete(' ')}-#{postfix}.otf"
  end

  @@cache = {} of Path => SF::Font

  def self.font_at(path : Path) : SF::Font
    @@cache.put_if_absent(path) do
      SF::Font.from_file(path.to_s)
    end
  end
end

record TextInfo, font : SF::Font, size : Int32 do
  def self.from?(term : Term)
    Term.case(term) do
      matchpi %[{_ font_string weight_: (%any 100 200 300 400 450 500 600 700 800 900) size_: (%number u8)}] do
        next unless path = FontManager.path?(font.to(String), weight.to(Int32))

        new(FontManager.font_at(path), size.to(Int32))
      end

      otherwise { }
    end
  end
end

def show(window, frame : Term)
  Term.case(frame) do
    matchpi %[(viewport child_ ¦ _ max-w: w←(%number +i32) max-h: h←(%number +i32))] do
      view = SF::View.new(SF.float_rect(0, 0, w.to(Int32), h.to(Int32)))

      window.view = view

      show(window, child)
    end

    # TODO: wrapping
    matchpi %[(text caption_string ¦ rest_ fg_ l_: (%number i32) t_: (%number i32))] do
      next unless info = TextInfo.from?(rest)

      sf = SF::Text.new(caption.to(String), info.font, info.size)
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
      sf.color = color?(fg) || SF::Color::Black
      sf.letter_spacing = 1

      window.draw(sf)
    end

    matchpi %[(rect ¦ _ bg_ l_: (%number i32) t_: (%number i32) final-w: w←(%number +i32) final-h: h←(%number +i32) border-radius: border_radius←(%number (whole _) >= 0))] do |bg|
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

      sf.update

      window.draw(sf)
    end

    matchpi %[(box child_ ¦ _)] do
      show(window, Term.of(frame.morph({0, :rect}, {1, nil})))
      show(window, child)
    end

    matchpi %[(circle ¦ _ bg_ l_: (%number i32) t_: (%number i32) radius_: (%number +i32))] do |bg|
      sf = SF::CircleShape.new(radius.to(Int32))
      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
      window.draw(sf)
    end

    matchpi %[(triangle ¦ _ bg_ l_: (%number i32) t_: (%number i32) final-w: w←(%number +i32) final-h: h←(%number +i32) looking: left)] do |bg|
      sf = SF::ConvexShape.new
      sf.point_count = 3
      sf[0] = SF.vector2i(0, h.to(Int32) // 2)
      sf[1] = SF.vector2i(w.to(Int32), 0)
      sf[2] = SF.vector2i(w.to(Int32), h.to(Int32))
      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
      window.draw(sf)
    end

    matchpi %[_dict] do
      frame.items.each { |child| show(window, child) }
    end

    otherwise { }
  end
end

window = SF::RenderWindow.new(SF::VideoMode.new(800, 600), title: "µsoma", settings: SF::ContextSettings.new(depth: 24, antialiasing: 8))
window.vertical_sync_enabled = true

font = SF::Font.from_file("./fonts/IBMPlexMono-Text.otf")
text = SF::Text.new("", font, 11)
top_line = 0

#  TODO: it is still unclear exactly how flexible all of this is. I need to try
#   making something a bit more complex layout-wise before going all-in with this
#   prototype.

frame = ML.term(<<-WWML
(viewport w: max h: max max-w: 800 max-h: 600 l: 300 t: 0
  (col w: content h: content gap: 30
    (col w: content h: content gap: 10
      (text "Container-sized buttons" w: content h: content fg: (oklch 0.205 0 0) font: "IBM Plex Sans" size: 24 weight: 700)
      (button "Button" w: max h: content)
      (row w: content h: content gap: 10
        (button.primary "Primary button" w: max h: content)
        (button.secondary "Secondary button" w: max h: content)))
    (col w: content h: content gap: 10
      (row w: content h: content gap: 7
        (text "Self-sized buttons" w: content h: content fg: (oklch 0.205 0 0) font: "IBM Plex Sans" size: 24 weight: 700)
        (y-center w: content h: max
          (offset w: content h: content x: 0 y: 3
            (box bg: (oklch 0.871 0.15 154.449) border-radius: 3
              (padding pl: 3 pr: 3 pt: 3 pb: 3
                (text "NEW" fg: (oklch 0.393 0.095 152.535) font: "IBM Plex Sans" size: 9 weight: 700))))))
      (text w: content h: content fg: (oklch 0.556 0 0) font: "IBM Plex Sans" size: 16 weight: 400
        "Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi.")
      (button "Button" w: content h: content)
      (button.primary "Primary button" w: content h: content)
      (button.secondary "Secondary button" w: content h: content))))
WWML
)

while window.open?
  while event = window.poll_event
    case event
    when SF::Event::Closed then window.close
    when SF::Event::MouseWheelScrolled
      top_line = Math.max(top_line - event.delta.to_i, 0)
    end
  end

  window.clear(SF::Color::White)

  showable = Term.of

  # I measure to cry!!! How slowwww it is ...
  #
  # Yes we're rewriting on every frame. I want to make sure we're able to. We won't
  # rewrite on every frame in practice. But still.
  dt = Time.measure do
    showable = rewrite(frame, uiR)
  end

  show(window, showable)

  text.string = "~#{dt.total_milliseconds}ms per rewrite()\n#{ML.display(showable, maxwidth: 80).each_line.skip(top_line).join('\n')}"
  text.fill_color = SF::Color::Black
  window.draw(text)

  window.display
end
