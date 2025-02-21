require "crsfml"
require "./src/wirewright"
require "./baz5"
require "./sfml_util"
require "./colors"

# I'm shifting more and more toward TUI. The way SDL/SFML render text
# is shitty. To do that myself would take a very long time. Doing this
# via a browser seems like an inefficient affair; we could use the browser
# (e.g. a webview) as a kind of "thin client", sending it draw commands
# and receiving events. But the amount of traffic that would generate, and
# the bloated-ness of the solution overall... astounding. So stupid that
# in this day and age there is no simple way to put rectangles and text
# on the screen without a boatload of "BUT"s and "YOU CANT"s.

preprocess = <<-WWML
;; Cascade width/height to single children.
(_ {_ -w_} ¦ _ w: W_) <> {w: →W}
(_ {_ -h_} ¦ _ h: H_) <> {h: →H}

;; Map w/h: N to w/h: max max-w/h: N
{_ w_number -max-w_} <> {w: max, max-w: →w}
{_ h_number -max-h_} <> {h: max, max-h: →h}

N←(box child_ ¦ () w_ h_ bg_ border-radius_)
  <> {N: (z-stack w: →w h: →h
           (rect w: max h: max bg: →bg border-radius: →border-radius)
           ;; Wrap to trigger cascade, if possible
           (wrapper w: →w h: →h
             →child))}

N←(button caption_string ¦ () bg_⋮ gray-200 fg_⋮ gray-700 w_ h_)
  <> {N: (box w: →w h: →h bg: →bg border-radius: 3
           (padding pl: 21 pr: 21 pt: 11 pb: 11
             (x-center
               (text w: content h: content fg: →fg font: "IBM Plex Sans" size: 14 weight: 500
                 →caption))))}

N←(box child_ ¦ () w_ h_ bg_ border-radius_ border-width_ border-color_)
  <> {N: (box w: →w h: →h bg: →border-color border-radius: →border-radius
           (padding pl: →border-width pr: →border-width pt: →border-width pb: →border-width
             (box bg: →bg border-radius: →border-radius
               →child)))}

N←(button.primary caption_string ¦ () w_ h_) <>
  {N: (button →caption fg: white bg: blue-500 w: →w h: →h)}

N←(button.secondary caption_string ¦ () w_ h_) <>
  {N: (button →caption fg: gray-200 bg: gray-700 w: →w h: →h)}
WWML

base1 = <<-WWML
(text caption_string ¦ _ w: content h: content -content-w_ -content-h_ font_string weight_: (%any 100 200 300 400 450 500 600 700 800 900) size_: (%number u8))
  <> {content-w: ($once (measure-width →caption →font →weight →size)),
      content-h: ($once (measure-height →caption →font →weight →size))}

;; TODO: text: handle the different configurations of w, h to obtain content-w and content-h,
;; employing wrapping if necessary:
;;   - [x] w: content h: content
;;   - [ ] w: content h: max
;;   - [ ] w: max h: content
;;   - [ ] w: max h: max

(padding {_ content-w: w_number} ¦ _ content-w: (%- _ W) pl_number pr_number)
  <> {W: ($once (+ →pl →w →pr))}
(padding {_ content-h: h_number} ¦ _ content-h: (%- _ H) pt_number pb_number)
  <> {H: ($once (+ →pt →h →pb))}

;; Learn the content width/height from a single child.
(_ {_ content-w: w_number} ¦ _ content-w: (%- _ W)) <> {W: →w}
(_ {_ content-h: h_number} ¦ _ content-h: (%- _ H)) <> {H: →h}

;; Propagate max-w/h if I know my max-w/h and I am w/h: max.
(_ {_ max-w: (%- _ w)} ¦ _ w: max max-w: W_number) <> {w: →W}
(_ {_ max-h: (%- _ h)} ¦ _ h: max max-h: H_number) <> {h: →H}

;; Propagate my content-w/h as max-w/h of children if I am w/h: content.
(_ {_ max-w: (%- _ w)} ¦ _ w: content content-w: W_number) <> {w: →W}
(_ {_ max-h: (%- _ h)} ¦ _ h: content content-h: H_number) <> {h: →H}

(padding {_ max-w: (%- _ w)} ¦ _ w: max max-w: W_number pl_number pr_number)
  <> {w: ($once (- →W →pl →pr))}
(padding {_ max-h: (%- _ h)} ¦ _ h: max max-h: H_number pt_number pb_number)
  <> {h: ($once (- →H →pt →pb))}

;; Set final-w/h based on w/h property.
{_ w: content content-w_number -final-w_} <> {final-w: →content-w}
{_ h: content content-h_number -final-h_} <> {final-h: →content-h}

{_ w: max max-w_number -final-w_} <> {final-w: →max-w}
{_ h: max max-h_number -final-h_} <> {final-h: →max-h}

(padding {_ -l_} ¦ _ l: L_number pl_number) <> {l: ($once (+ →L →pl))}
(padding {_ -t_} ¦ _ t: T_number pt_number) <> {t: ($once (+ →T →pt))}

;; FIXME: The moment you do something like this (%not) in a rule system, is exactly
;; the moment when the rule system becomes a COMPLETE AND UTTER PILE OF POO !
((%not x-center x-right offset) {_ -l_} ¦ _ l: L_number) <> {l: →L}
((%not y-center offset) {_ -t_} ¦ _ t: T_number) <> {t: →T}

(x-center {_ -l_ final-w: w_number} ¦ _ l: L_number final-w: W_number)
  <> {l: ($ (+ →L (// (- →W →w) 2)))}
(y-center {_ -t_ final-h: h_number} ¦ _ t: T_number final-h: H_number)
  <> {t: ($ (+ →T (// (- →H →h) 2)))}

(x-right {_ -l_ final-w: w_number} ¦ _ l: L_number final-w: W_number)
  <> {l: ($ (- (+ →L →W) →w))}

(offset {_ -l_} ¦ _ l: L_number shl: dx_number) <> {l: ($once (+ →L →dx))}
(offset {_ -t_} ¦ _ t: T_number sht: dy_number) <> {t: ($once (+ →T →dy))}

;; Compute the content-w of x-stack.
(x-stack (%many Cws {_ content-w: (%let 0 _number)}) ¦ _ content-w: (%- _ W) gap⋮ 0)
  <> {W: ($ (+ (sum (flatten →Cws)) (* →gap (- (tally →Cws) 1))))}

;; Compute the content-h of x-stack.
(x-stack (%many Chs {_ content-h: (%let 0 _number)}) ¦ _ content-h: (%- _ H))
  <> {H: ($ (max (flatten →Chs)))}

;; If x-stack is set to h: content then setting h: max in a child
;; would mean that the child's max-h is content-h of the x-stack
;; (i.e., maximum sibling height).
(%all (x-stack _+ ¦ _ h: content content-h: H_number) ⟨{_ h: max max-h: (%- _ h)}⟩°)
  <> {h: →H}

;; If x-stack is set to h: max then setting h: max in a child would
;; mean that the child's max-h is max-h of the x-stack.
(%all (x-stack _+ ¦ _ h: max max-h: H_number) ⟨{_ h: max max-h: (%- _ h)}⟩°)
  <> {h: →H}

;; In case of a x-stack, all children tops are shared and are exactly
;; x-stack's top.
(%all (x-stack _+ ¦ _ t: T_number) ⟨{_ -t_}⟩°)
  <> {t: →T}

;; The first child of x-stack has the same left position as the x-stack itself.
(x-stack {_ -l_} _* ¦ _ l: L_number)
  <> {l: →L}

;; Any consecutive child learns its left position from the previous child
;; plus the previous child's final-w.
;;
;; NOTE: we use the last child's left as a "flag" to signal that all children
;; had their lefts computed already and the rule engine should skip this rule.
;; This simplifies the job of the rule engine, making sure that it doesn't
;; have to go through all children every time before seeing that there is
;; no change, with regards to this particular rule.
(%all (x-stack _* {_ l: (%- _)} ¦ _ gap⋮ 0) ⟨{_ l: l0_number final-w: w0_number} {_ l: (%- _ l1)}⟩)
  <> {l1: ($once (+ →l0 →w0 →gap))}

;; Compute the content-w of y-stack.
(y-stack (%many Cws {_ content-w: (%let 0 _number)}) ¦ _ content-w: (%- _ W))
  <> {W: ($ (max (flatten →Cws)))}

;; Compute the content-h of y-stack.
(y-stack (%many Chs {_ content-h: (%let 0 _number)}) ¦ _ content-h: (%- _ H) gap⋮ 0)
  <> {H: ($ (+ (sum (flatten →Chs)) (* →gap (- (tally →Chs) 1))))}

;; If y-stack is set to w: content then setting w: max in a child
;; would mean that the child's max-w is content-w of the y-stack
;; (i.e., maximum sibling width).
(%all (y-stack _+ ¦ _ w: content content-w: W_number) ⟨{_ w: max max-w: (%- _ w)}⟩°)
  <> {w: →W}

;; If y-stack is set to w: max then setting w: max in a child would
;; mean that the child's max-w is max-w of the y-stack.
(%all (y-stack _+ ¦ _ w: max max-w: W_number) ⟨{_ w: max max-w: (%- _ w)}⟩°)
  <> {w: →W}

;; In case of a y-stack, all children lefts are shared and are exactly
;; y-stack's left.
(%all (y-stack _+ ¦ _ l: L_number) ⟨{_ -l_ }⟩°)
  <> {l: →L}

;; The first child of y-stack has the same top position as the y-stack itself.
(y-stack {_ -t_} _* ¦ _ t: T_number)
  <> {t: →T}

;; Any consecutive child learns its top position from the previous child
;; plus the previous child's content-h.
;;
;; NOTE: we use the last child's top as a "flag" to signal that all children
;; had their tops computed already and the rule engine should skip this rule.
;; This simplifies the job of the rule engine, making sure that it doesn't
;; have to go through all children every time before seeing that there is
;; no change, with regards to this particular rule.
(%all (y-stack _* {_ t: (%- _)} ¦ _ gap⋮ 0) ⟨{_ t: t0_number final-h: h0_number} {_ t: (%- _ t1)}⟩)
  <> {t1: ($once (+ →t0 →h0 →gap))}

;; Compute the content-w of z-stack.
(z-stack (%many Cws {_ content-w: (%let 0 _number)}) ¦ _ content-w: (%- _ W))
  <> {W: ($ (max (flatten →Cws)))}

;; Compute the content-h of z-stack.
(z-stack (%many Chs {_ content-h: (%let 0 _number)}) ¦ _ content-h: (%- _ H))
  <> {H: ($ (max (flatten →Chs)))}

;; If z-stack is set to w/h: content, this means the children receive z-stack's
;; content-w/h as max-w/h.
(%all (z-stack _+ ¦ _ w: content content-w: W_number) ⟨{_ max-w: (%- _ w)}⟩°) <> {w: →W}
(%all (z-stack _+ ¦ _ h: content content-h: H_number) ⟨{_ max-h: (%- _ h)}⟩°) <> {h: →H}

;; If z-stack is set to w/h: max, this means the children receive z-stack's
;; max-w/h as max-w/h.
(%all (z-stack _+ ¦ _ w: max max-w: W_number) ⟨{_ max-w: (%- _ w)}⟩°) <> {w: →W}
(%all (z-stack _+ ¦ _ h: max max-h: H_number) ⟨{_ max-h: (%- _ h)}⟩°) <> {h: →H}

;; The children of z-stack all receive left/top of the z-stack.
(%all (z-stack _+ ¦ _ l: L_number) ⟨{_ -l_}⟩°) <> {l: →L}
(%all (z-stack _+ ¦ _ t: T_number) ⟨{_ -t_}⟩°) <> {t: →T}

;; `rect` does not have content. Hard-code its content-w/h to 0.
(rect ¦ _ -content-w_) <> {content-w: 0}
(rect ¦ _ -content-h_) <> {content-h: 0}

(expander {_ content-w: w_number} ¦ _ min-w_number content-w: (%- _ W))
  <> {W: ($once (max →min-w →w))}
(expander {_ content-h: h_number} ¦ _ min-h_number content-h: (%- _ H))
  <> {H: ($once (max →min-h →h))}
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

  # Flattens itemspart of *xs*, its items and so on, recursively.
  rulepi1 %[(flatten xs_)] do
    Term::Dict.build do |commit|
      Term.each_keypath_and_item(xs) do |_, leaf|
        commit << leaf

        true # Continue
      end
    end
  end

  rulepi1 %[(tally xs_dict)] do
    xs.unsafe_as_d.size
  end

  rulepi1 %[(sum (ns_number+))] { ns.items.reduce { |a, b| a.unsafe_as_n + b.unsafe_as_n } }

  rulepi1 %[(min ns_number+)] { ns.items.min_by(&.unsafe_as_n) }
  rulepi1 %[(min (ns_number+))] { ns.items.min_by(&.unsafe_as_n) }

  rulepi1 %[(max ns_number+)] { ns.items.max_by(&.unsafe_as_n) }
  rulepi1 %[(max (ns_number+))] { ns.items.max_by(&.unsafe_as_n) }

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

    matchpi %{_symbol} do
      return unless rgb = Colors.rgb?(term)

      SF::Color.new(*rgb)
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
      # view = SF::View.new(SF.float_rect(0, 0, w.to(Int32), h.to(Int32)))

      # window.view = view

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

    matchpi %[(circle ¦ _ bg_ l_: (%number i32) t_: (%number i32) radius_: (%number +i32))] do |bg|
      sf = SF::CircleShape.new(radius.to(Int32))
      sf.fill_color = color?(bg) || SF::Color::Transparent
      sf.position = SF.vector2i(l.to(Int32), t.to(Int32))
      window.draw(sf)
    end

    matchpi %[(triangle ¦ _ bg_ l_: (%number i32) t_: (%number i32) final-w: w←(%number +i32) final-h: h←(%number +i32) pointing: left)] do |bg|
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

# Major TODOs:
#   - design cursor[x] & tooltip separately using the framework
#   - fractionals (fr) for x-stack and y-stack primary axis, taking care of <primary axis>: max on children
#   - experiment with capsize

frame = ML.term(<<-WWML
(viewport w: content h: content max-w: 500 max-h: 400 l: 200 t: 0
  (box w: content h: content bg: gray-800 border-radius: 3 border-width: 1 border-color: gray-600
    (y-stack w: content h: content
      (padding w: max h: content pl: 10 pr: 10 pt: 7 pb: 7
        (x-stack w: content h: content
          (text w: content h: content fg: gray-300 font: "IBM Plex Sans" size: 14 weight: 450
            "1-5 out of 20")
          ;; TODO: w: max so that it pushes the content right
          (x-right w: content h: content
            (x-stack gap: 5
              (text "↑" w: content h: content fg: gray-400 font: "IBM Plex Sans" size: 14 weight: 450)
              (text "↓" w: content h: content fg: blue-400 font: "IBM Plex Sans" size: 14 weight: 700)))))
      (rect w: max h: 1 bg: gray-600 border-radius: 0)
      (expander min-w: 150
        (padding w: max h: content pl: 10 pr: 10 pt: 10 pb: 10
          (y-stack w: content h: content gap: 10
            (text w: content h: content fg: gray-200 font: "IBM Plex Mono" size: 14 weight: 450
              "absence")
            (text w: content h: content fg: gray-200 font: "IBM Plex Mono" size: 14 weight: 450
              "blast")
            (text w: content h: content fg: gray-200 font: "IBM Plex Mono" size: 14 weight: 450
              "button")
            (text w: content h: content fg: gray-200 font: "IBM Plex Mono" size: 14 weight: 450
              "cell")
            (text w: content h: content fg: gray-200 font: "IBM Plex Mono" size: 14 weight: 450
              "changes")))))))

;; Cursor
;;(viewport w: content h: content max-w: 500 max-h: 400 l: 200 t: 0
;;  (y-stack w: content h: content gap: 10
;;    (x-stack w: content h: content
;;      (box w: content h: max bg: gray-600 border-radius: 0
;;        (padding pl: 2 pr: 2 pt: 3 pb: 3
;;          (text w: content h: content fg: gray-300 font: "IBM Plex Mono" size: 14 weight: 450
;;            "Lorem ipsum")))
;;      (rect w: 1 h: max bg: blue-400 border-radius: 0)
;;      (box w: content h: max bg: gray-600 border-radius: 0
;;        (padding pl: 2 pr: 2 pt: 3 pb: 3
;;          (text w: content h: content fg: gray-300 font: "IBM Plex Mono" size: 14 weight: 450
;;            "dolor"))))))


;;(viewport w: max h: content max-w: 500 max-h: 400 l: 200 t: 0
;;  (y-stack w: max h: content gap: 30
;;    (y-stack w: content h: content gap: 10
;;      (text w: content h: content fg: gray-800 font: "IBM Plex Sans" size: 24 weight: 700
;;        "Container-sized buttons")
;;      (button "Button" w: max h: content)
;;      (button.primary "Primary button" w: max h: content)
;;      (button.secondary "Secondary button" w: max h: content))
;;    (y-stack w: content h: content gap: 10
;;      (x-stack w: content h: content gap: 7
;;        (text w: content h: content fg: gray-800 font: "IBM Plex Sans" size: 24 weight: 700
;;          "Self-sized buttons")
;;        (y-center w: content h: max
;;          (offset w: content h: content shl: 0 sht: 3
;;            (box bg: green-300 border-radius: 3
;;              (padding pl: 3 pr: 3 pt: 3 pb: 3
;;                (text "NEW" w: content h: content fg: green-900 font: "IBM Plex Sans" size: 9 weight: 700))))))
;;      (text w: content h: content fg: gray-500 font: "IBM Plex Sans" size: 16 weight: 400
;;        "Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi.")
;;      (button "Button" w: content h: content)
;;      (button.primary "Primary button" w: content h: content)
;;      (button.secondary "Secondary button" w: content h: content))))
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

  window.clear(color?(Term.of(:"gray-900")).not_nil!)

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
