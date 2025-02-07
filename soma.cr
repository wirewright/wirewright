require "crsfml"
require "./wirewright"
require "./baz5"
require "./sfml_util"
require "./oklch"

base1 = <<-WWML
;; Calculate ph from pl, pr if absent; pv from pt, pr if absent.
;; TODO: this does not belong here; this belongs to the preprocessing layer.
{_ pl⋮ 0 pr⋮ 0 -ph_} <> {ph: ($once (+ →pl →pr))}
{_ pt⋮ 0 pb⋮ 0 -pv_} <> {pv: ($once (+ →pt →pb))}

;; Expand w/h: _number into w/h: max max-w/h: _number
;; TODO: this does not belong here; this belongs to the preprocessing layer.
{_ w_number max-w: (%- w_ max-w)} <> {max-w: →w, w: max}
{_ h_number max-h: (%- h_ max-h)} <> {max-h: →h, h: max}

;; Calculate width/height of rect if it is fixed.
((%any rect triangle) ¦ _ w_number -inner-w_) <> {inner-w: →w, w: content}
((%any rect triangle) ¦ _ h_number -inner-h_) <> {inner-h: →h, h: content}

;; Calculate width/height of circle if its radius is known.
(circle ¦ _ radius_number -inner-w_) <> {inner-w: ($once (* →radius 2))}
(circle ¦ _ radius_number -inner-h_) <> {inner-h: ($once (* →radius 2))}

;; Calculate max-w/h for a single child.
(_ {_ -max-w_} ¦ _ max-w: W_number ph_number) <> {max-w: ($once (- →W →ph))}
(_ {_ -max-h_} ¦ _ max-h: H_number pv_number) <> {max-h: ($once (- →H →pv))}

;; Calculate max-w/h for a bordered box child -- subtract borders on both sides.
(box {_ -max-w_} ¦ _ border: true max-w: W_number ph_number) <> {max-w: ($once (- →W →ph 2))}
(box {_ -max-h_} ¦ _ border: true max-h: H_number pv_number) <> {max-h: ($once (- →H →pv 2))}

;; Learn inner-w/h from a single child.
(_ {_ outer-w_number} ¦ _ -inner-w_) <> {inner-w: →outer-w}
(_ {_ outer-h_number} ¦ _ -inner-h_) <> {inner-h: →outer-h}

;; Text measures its caption to compute inner-w/h.
(text caption_string ¦ _ size_: (%number u8) -inner-w_)
  <> {inner-w: ($once (measure-width →caption →size))}
(text caption_string ¦ _ size_: (%number u8) -inner-h_)
  <> {inner-h: ($once (measure-height →caption →size))}

;; Resolve w/h: content hint.
{_ w: content inner-w_number -outer-w_ ph_number} <> {outer-w: ($once (+ →ph →inner-w))}
{_ h: content inner-h_number -outer-h_ pv_number} <> {outer-h: ($once (+ →pv →inner-h))}

;; Resolve w/h: max hint.
{_ w: max max-w_number inner-w_number -outer-w_ ph_number} <> {outer-w: ($ (max (+ →ph →inner-w) →max-w))}
{_ h: max max-h_number inner-h_number -outer-h_ pv_number} <> {outer-h: ($ (max (+ →pv →inner-h) →max-h))}

;; Detect overflow.
{_ max-w_number outer-w_number -overflows-x_} <> {overflows-x: ($once (> →outer-w →max-w))}
{_ max-h_number outer-h_number -overflows-y_} <> {overflows-y: ($once (> →outer-h →max-h))}

;; Scrollbox takes care of overflow, clips to max-w/h in that case.
(scrollbox _ ¦ _ w: max overflows-x_: true max-w_number outer-w_number) <> {overflows-x: false, outer-w: →max-w}
(scrollbox _ ¦ _ h: max overflows-y_: true max-h_number outer-h_number) <> {overflows-y: false, outer-h: →max-h}

;; Calculate inner-w/h for col.
(%all [col _*] ⟨{_ outer-w_number -col/in-w_} ¦ _ inner-w⋮ 0⟩)
  <> {inner-w: ($once (max →inner-w →outer-w)), col/in-w: true}
(%all [col _*] ⟨{_ outer-h_number -col/in-h_} ¦ _ inner-h⋮ 0⟩)
  <> {inner-h: ($once (+ →inner-h →outer-h)), col/in-h: true}

;; Calculate inner-w/h for row.
(%all (row _* ¦ _ inner-w: (%optional 0 W_number)) ⟨{_ outer-w: w_number, -row/in-w_}⟩)
  <> {W: ($once (+ →W →w)), row/in-w: true}
(%all (row _* ¦ _ inner-h: (%optional 0 H_number)) ⟨{_ outer-h: h_number, -row/in-h_}⟩)
  <> {H: ($once (max →H →h)), row/in-h: true}

;; Calculate inner-w/h for content-sized `layers`. That's going to be max outer-w/h
;; of its children.
(%all (layers _* ¦ _ w: content inner-w: (%optional 0 W_number)) ⟨{_ outer-w: w_number}⟩)
  <> {W: ($once (max →W →w))}

(%all (layers _* ¦ _ h: content inner-h: (%optional 0 H_number)) ⟨{_ outer-h: h_number}⟩)
  <> {H: ($once (max →H →h))}

;; max-w/h for children of `layers` is the max-w/h of `layers` itself.
(%all (layers _* ¦ _ max-w: W_number) ⟨{_ -max-w_}⟩) <> {max-w: →W}
(%all (layers _* ¦ _ max-h: H_number) ⟨{_ -max-h_}⟩) <> {max-h: →H}

;; The following rule system will calculate fr/max for fr-children of fr: true parents
;; with a defined fr/avail (the amount of free space left).

(%all {_ fr: true -fr/n_} (%items F {_ fr: _number}))
  <> {fr/n: ($once (size →F))}

(%partition (%pipe tally (- 1) n_) {_ fr/n: n_ -fr/all_})
  <> {fr/all: true}

⟨{_ fr_number -fr/state_} ¦ _ fr/den⋮ 0 fr/n_number: (%not 0)⟩
  <> {fr/den: ($once (+ →fr/den →fr)),
      fr/state: in-den,
      fr/n: ($once (- →fr/n 1))}

⟨{_ fr/state: in-den fr_number -fr/max_} ¦ _ fr/avail_number fr/den_number: (%not 0) fr/n: 0⟩
  <> {fr/max: ($ (* →fr/avail (/ →fr →fr/den)))}

;; `col` interacts with this rule system by calculating fr/avail based on its
;; height. Its children are going to take fr/max as max-h.

(col _* ¦ _ fr: true max-h_number fr/all: true -fr/avail_)
  <> {fr/avail: →max-h}

(col _* ¦ _ fr: true max-h_number inner-h_number overflows-y: (%- true) -fr/avail_)
  <> {fr/avail: ($once (- →max-h →inner-h))}

(%all [col _*] ⟨{_ fr/max_number -max-h_}⟩)
  <> {max-h: →fr/max}

;; `row` interacts with this rule system by calculating fr/avail based on its
;; width. Its children are going to take fr/max as max-w.

(row _* ¦ _ fr: true max-w_number fr/all: true -fr/avail_)
  <> {fr/avail: →max-w}

(row _* ¦ _ fr: true max-w_number inner-w_number overflows-x: (%- true) -fr/avail_)
  <> {fr/avail: ($once (- →max-w →inner-w))}

(%all [row _*] ⟨{_ fr/max_number -max-w_}⟩)
  <> {max-w: →fr/max}
WWML

# TODO: decouple mx/my and l:/r: computation like we have outer-w/h and inner-w/h.
# Maybe inset-l/inset-t and origin-l/origin-t
base2 = <<-WWML
;; Calculate mx from ml, mr if absent; my from mt, mb if absent.
;; TODO: this does not belong here; this belongs to the preprocessing layer.
{_ ml⋮ 0 mr⋮ 0 -mx_} <> {mx: ($once (- →ml →mr))}
{_ mt⋮ 0 mb⋮ 0 -my_} <> {my: ($once (- →mt →mb))}

;; Propagate l/t to single child, offset by child's mx/my and parent's left/top padding.
(_ {_ mx_number -l_} ¦ _ l: L_number pl⋮ 0) <> {l: ($once (+ →L →mx →pl))}
(_ {_ my_number -t_} ¦ _ t: T_number pt⋮ 0) <> {t: ($once (+ →T →my →pt))}

;; Initialize bottom (b) and right (r) for col, row.
(col _* ¦ _ t_number -b_ pt⋮ 0) <> {b: ($once (+ →t →pt))}
(row _* ¦ _ l_number -r_ pl⋮ 0) <> {r: ($once (+ →l →pl))}

;; Propagate `l` offset by `mx` to col children.
;; TODO: there should be no defaults at this point.
(%all (col _* ¦ _ l: L_number pl⋮ 0) ⟨{_ -l_ mx_number}⟩)
  <> {l: ($once (+ →L →mx →pl))}

;; Calculate `t` offset by `my` for col children.
;; TODO: there should be no defaults at this point.
(%all (col _* ¦ _ b_number) ⟨{_ -t_ my_number outer-h_number}⟩)
  <> {t: ($once (+ →b →my)),
      b: ($once (+ →b →my →outer-h))}

;; Calculate `l` offset by `mx` for row children.
(%all (row _* ¦ _ r⋮ 0) ⟨{_ -l_ mx_number outer-w_number}⟩)
  <> {l: ($once (+ →r →mx)),
      r: ($once (+ →r →mx →outer-w))}

;; Propagate `t` offset by `my` to row children.
;; TODO: there should be no defaults at this point.
(%all (row _* ¦ _ t: T_number pt⋮ 0) ⟨{_ -t_ my_number}⟩)
  <> {t: ($once (+ →T →my →pt))}

;; Position the child of `center`
(center {_ -l_ -t_ outer-w: w_number outer-h: h_number} ¦ _ l: L_number t: T_number outer-w: W_number outer-h: H_number)
  <> {l: ($ (// (- →W →w) 2)),
      t: ($ (// (- →H →h) 2))}

;; For `layers`, propagate `l` and `t` to children as well.
;; TODO: there should be no defaults at this point.
(%all (layers _* ¦ _ l: L_number pl⋮ 0) ⟨{_ mx_number -l_}⟩) <> {l: ($once (+ →L →mx →pl))}
(%all (layers _* ¦ _ t: T_number pt⋮ 0) ⟨{_ my_number -t_}⟩) <> {t: ($once (+ →T →my →pt))}
WWML

# CONFIGURE REWRITER

FONT_SANS = SF::Font.from_file("./fonts/IBMPlexSans-Text.otf")
FONT_MONO = SF::Font.from_file("./fonts/IBMPlexMono-Text.otf")

def measure(string : String, character_size) : {Int32, Int32}
  text = SF::Text.new(string, FONT_SANS, character_size)
  size = text.size
  {size.x, size.y}
end

primitives = ProcRuleset.build do
  rulepi1 %[(+ ns_number+)] { ns.items.sum(Term[0]) }
  rulepi1 %[(- a_number b_number)] { a - b }
  rulepi1 %[(* a_number b_number)] { a * b }
  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
  rulepi1 %[(// a_number (%all b_number (%not 0)))] { a // b }

  rulepi1 %[(> a_number b_number)] do
    a.unsafe_as_n > b.unsafe_as_n
  end

  rulepi1 %[(size xs_dict)] do
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

  rulepi1 %[(measure-width text_string size←(%number u8))] do
    w, _ = measure(text.to(String), size.to(Int32))
    w
  end

  rulepi1 %[(measure-height text_string size←(%number u8))] do
    _, h = measure(text.to(String), size.to(Int32))
    h
  end

  # TODO: subject: rune
  # TODO: subject: word
  rulepi1 %[(span text_string subject: line)] do
    text.to(String).each_line.size
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

set1, rec1 = recR
set2, rec2 = recR
lyr = chainR(exhR(set1.call choiceR(
  rulesetR(Ruleset.select(selector, ML.terms(base1)), noR, backmapR, noR),
  itemsR(rec1),
)),
exhR(set2.call choiceR(
  rulesetR(Ruleset.select(selector, ML.terms(base2)), noR, backmapR, noR),
  itemsR(rec2),
)),
)

# SHOW

def color?(term : Term)
  Term.case(term) do
    matchpi %[(lch l←(%number 0 <= (whole _) <= 100) c←(%number 0 <= _ <= 0.36) h←(%number 0 <= (whole _) <= 360))] do
      r, g, b = Oklch.to_rgb(l.to(Int32), c.to(Float64), h.to(Int32))

      SF::Color.new(r, g, b)
    end

    otherwise { }
  end
end

def show(window, frame : Term)
  Term.case(frame) do
    matchpi %[(viewport child_ ¦ _ max-w: w←(%number +i32) max-h: h←(%number +i32))] do
      view = SF::View.new(SF.float_rect(0, 0, w.to(Int32), h.to(Int32)))

      window.view = view

      show(window, child)
    end

    matchpi %[(text caption_string ¦ _ fg_ size_: (%number u8) l_: (%number i32) t_: (%number i32))] do
      sf = SF::Text.new(caption.to(String), FONT_SANS, size.to(Int32))
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
      sf.color = color?(fg) || SF::Color::Black
      window.draw(sf)
    end

    matchpi %[(rect ¦ _ bg_ l_: (%number i32) t_: (%number i32) outer-w: w←(%number +i32) outer-h: h←(%number +i32))] do |bg|
      sf = SF::RectangleShape.new
      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
      sf.size = SF.vector2i(w.to(Int32), h.to(Int32))
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

    matchpi %[(triangle ¦ _ bg_ l_: (%number i32) t_: (%number i32) outer-w: w←(%number +i32) outer-h: h←(%number +i32) looking: left)] do |bg|
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

frame = ML.term(<<-WWML
(viewport max-w: 800 max-h: 600 l: 0 t: 0
  (center w: max h: max
    (layers w: content h: content
      ;;(rect w: 100 h: 100)
      (circle radius: 5 ml: -20 bg: (lch 40 0.074 283))
      (triangle w: 10 h: 10 looking: left ml: -10 bg: (lch 40 0.074 283))
      (box pl: 10 pt: 5 pb: 10 pr: 100 w: content h: content bg: (lch 40 0.074 283)
        (col w: content h: content
          (text "text-xs" size: 12 w: content h: content fg: (lch 90 0.074 283))
          (text "text-sm" size: 14 w: content h: content fg: (lch 90 0.074 283))
          (text "text-base" size: 16 w: content h: content fg: (lch 90 0.074 283))
          (text "text-lg" size: 18 w: content h: content fg: (lch 90 0.074 283))
          (text "text-xl" size: 20 w: content h: content fg: (lch 90 0.074 283)))))))
;;  (scrollbox h: 10 w: content
;;    (col fr: true w: content h: max
;;      (text w: content h: max fr: 2 "Hello World")
;;      (text w: content h: max fr: 1 "Bye World"))))
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

  dt = Time.measure do
    showable = rewrite(frame, lyr)
  end

  text.string = "~#{dt.total_milliseconds}ms per rewrite()\n#{ML.display(showable, maxwidth: 120).each_line.skip(top_line).join('\n')}"
  text.fill_color = SF::Color::Black
  window.draw(text)

  show(window, showable)

  window.display
end
