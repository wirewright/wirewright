require "./wirewright"
require "./baz4"

def measure(string : String) : {Int32, Int32}
  w = h = 0
  string.each_line(chomp: false) do |line|
    w = Math.max(w, line.size)
    h += 1
  end
  {w, h}
end

def wrap(text : String, w : Int32?, h : Int32?, *, pre = false) : String
  reader = Char::Reader.new(text)

  String.build do |io|
    (0...h).each do
      (0...w).each do
        chr = reader.current_char

        case chr
        when '\0'
          break
        when '\n'
          reader.next_char
          break
        else
          io << chr
        end

        break unless reader.next_char?
      end

      while !pre && reader.current_char == ' '
        reader.next_char
      end

      break if reader.current_char == '\0'

      io.puts
    end
  end
end

base = <<-WWML
;; Calculate px from pl, pr if absent; py from pt, pr if absent.
(node (_* ¦ _ pl: (%optional 0 pl_number) pr: (%optional 0 pr_number) px: (%- _ px)))
  <> {px: ($once (+ →pl →pr))}
(node (_* ¦ _ pt: (%optional 0 pt_number) pb: (%optional 0 pb_number) py: (%- _ py)))
  <> {py: ($once (+ →pt →pb))}

;; Expand w/h: _number into w/h: max max-w/h: _number
(node (_* ¦ _ w: w_number max-w: (%- _ mw))) <> {mw: →w, w: max}
(node (_* ¦ _ h: h_number max-h: (%- _ mh))) <> {mh: →h, h: max}

;; Calculate max-w/h for a single child.
(edge (_ _ ¦ _ max-w: W_number px: px_number) (_* ¦ _ max-w: (%- _ w)))
  <> {w: ($once (- →W →px))}
(edge (_ _ ¦ _ max-h: H_number py: py_number) (_* ¦ _ max-h: (%- _ h)))
  <> {h: ($once (- →H →py))}

;; Calculate max-w/h for a bordered box child.
(edge (box _ ¦ _ border: true max-w: W_number px: px_number) (_* ¦ _ max-w: (%- _ w)))
  <> {w: ($once (- →W →px 1))}
(edge (box _ ¦ _ border: true max-h: H_number py: py_number) (_* ¦ _ max-h: (%- _ h)))
  <> {h: ($once (- →H →py 1))}

;; Learn inner-w/h from a single child.
(edge (_ _ ¦ _ inner-w: (%- _ W)) (_* ¦ _ outer-w: w_number)) <> {W: →w}
(edge (_ _ ¦ _ inner-h: (%- _ H)) (_* ¦ _ outer-h: h_number)) <> {H: →h}

;; Text measures its caption to compute inner-w/h.
(node (text caption_string ¦ _ inner-w: (%- _ w) inner-h: (%- _ h)))
  <> {w: ($once (measure-width →caption)),
      h: ($once (measure-height →caption))}

;; Resolve w/h: content hint.
(node (_* ¦ _  w: content inner-w: iw_number outer-w: (%- _ w) px: px_number))
  <> {w: ($once (+ →px →iw))}
(node (_* ¦ _  h: content inner-h: ih_number outer-h: (%- _ h) py: py_number))
  <> {h: ($once (+ →py →ih))}

;; Resolve w/h: max hint.
(node (_* ¦ _ w: max max-w: mw_number outer-w: (%- _ w) inner-w: iw_number px: px_number))
  <> {w: ($ (max (+ →px →iw) →mw))}
(node (_* ¦ _ h: max max-h: mh_number outer-h: (%- _ h) inner-h: ih_number py: py_number))
  <> {h: ($ (max (+ →py →ih) →mh))}

;; Detect overflow.
(node (_* ¦ _ max-w: mw_number outer-w: w_number overflows-x: (%- _ ox)))
  <> {ox: ($once (> →w →mw))}
(node (_* ¦ _ max-h: mh_number outer-h: h_number overflows-y: (%- _ oy)))
  <> {oy: ($once (> →h →mh))}

;; Scrollbox takes care of overflow, clips to max-w/h in that case.
(node (scrollbox _ ¦ _ w: max overflows-x: ox←true max-w: mw_number outer-w: ow_number))
  <> {ox: false, ow: →mw}
(node (scrollbox _ ¦ _ h: max overflows-y: oy←true max-h: mh_number outer-h: oh_number))
  <> {oy: false, oh: →mh}

;; Calculate inner-w/h for col.
(edge (col _* ¦ _ inner-w: (%optional 0 W_number)) (_* ¦ _ outer-w: w_number))
  <> {W: ($once (max →W →w))}
(edge (col _* ¦ _ inner-h: (%optional 0 H_number)) (_* ¦ _ outer-h: h_number state: (%- _ state)))
  <> {H: ($once (+ →H →h)), state: member}

;; "Column can give you max-h if you specify fr: _number."

(node (%all (col _* ¦ _ nF: (%- _ n)) (%items Fch (_* ¦ _ fr: _number))))
  <> {n: ($once (size →Fch))}

(node (col _* ¦ _ max-h: mh_number inner-h: h_number overflows-y: (%- true) rem-h: (%- _ rh)))
  <> {rh: ($once (- →mh →h))}

(edge (col _* ¦ _ rem-h: _number frs: (%optional 0 frs_number) nF: n_number) (_* ¦ _ fr: fr_number state: (%- _ state)))
  <> {frs: ($once (+ →frs →fr)), state: distrib, n: ($once (- →n 1))}

(edge (col _* ¦ _ rem-h: H_number frs: frs_number nF: 0) (_* ¦ _ state: s←distrib fr: fr_number max-h: (%- _ h)))
  <> {h: ($ (* →H (/ →fr →frs))), s: member}

;; Calculate inner-w/h for row.
(edge (row _* ¦ _ inner-w: (%optional 0 W_number)) (_* ¦ _ outer-w: w_number state: (%- _ state)))
  <> {W: ($once (+ →W →w)), state: member}
(edge (row _* ¦ _ inner-h: (%optional 0 H_number)) (_* ¦ _ outer-h: h_number))
  <> {H: ($once (max →H →h))}
WWML

frame = <<-WWML
(viewport l: 0 t: 0 w: max h: max max-w: 64 max-h: 32 bg: (0 0 0)
  (scrollbox w: max h: max
    (col w: content h: max
      (text pre: true w: content h: 10 "Lorem ipsum dolor sit amet.")
      (text pre: true w: content fr: 1 h: max "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim.")
      (text pre: true w: content h: content "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat."))))
  ;; (center w: max h: max max-h: 10
  ;;   (box w: content h: content border: true
  ;;     (text max-w: 20 w: content h: content fg: (0 0 0) bg: (255 255 255) pre: true
  ;;       "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat."))))
WWML

def nodeR(changes, term term0, successor)
  subject = Term.of(:node, term0)

  case rewrite = successor.call(changes, Rewrite.one(subject))
  in Rewrite::None then term0
  in Rewrite::One  then _, _term1 = rewrite.term # (node term0) -> (node term1)
  in Rewrite::Many
    raise "nodeR: Rewrite::Many not implemented"
  end
end

def edgeR(changes, parent0, child0, successor)
  subject = Term.of(:edge, parent0, child0)

  case rewrite = successor.call(changes, Rewrite.one(subject))
  in Rewrite::None
    {parent0, child0}
  in Rewrite::One
    # (edge parent0 child0) -> (edge parent1 child1)
    _, parent1, child1 = rewrite.term 
    {parent1, child1}
  in Rewrite::Many
    raise "edgeR: Rewrite::Many is not implemented"
  end
end

def outerR(changes, term : Term, successor)
  unless dict0 = term.as_d?
    return term
  end

  while true
    dict1 = dict0
    dict0.each_item_with_index do |v0, k|
      # Rewrite node
      v0 = nodeR(changes, v0, successor)
      # Rewrite edge
      dict1, v0 = edgeR(changes, dict1, v0, successor)
      # Rewrite recursively
      v0 = outerR(changes, v0, successor)
      # Add changed v0 to dict1
      if dict1.type.dict?
        dict1 = dict1.unsafe_as_d.with(k, v0)
      end
    end
    break if dict0 == dict1
    break unless dict1.type.dict?
    dict0 = dict1.unsafe_as_d
  end

  Term.of(dict0)
end

primitives = ProcRuleset.build do
  rulepi1 %[(+ a_number b_number)] { a + b }
  rulepi1 %[(- a_number b_number)] { a - b }
  rulepi1 %[(* a_number b_number)] { a * b }
  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }

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

  rulepi1 %[(measure-width text_string)] do
    w, _ = measure(text.to(String))
    w
  end

  rulepi1 %[(measure-height text_string)] do
    _, h = measure(text.to(String))
    h
  end

  rulepi1 %[(wrap text_string w: w←(%number +i32) pre: pre_boolean)] do
    wrap(text.to(String), w: w.to(Int32), h: nil, pre: pre.true?)
  end

  # TODO: subject: rune
  # TODO: subject: word
  rulepi1 %[(span text_string subject: line)] do
    text.to(String).each_line.size
  end
end

def eventR(changes, term, successor)
  case rewrite = successor.call(changes, Rewrite.one(term))
  in Rewrite::None
    rewrite
  in Rewrite::One
    Term.case({term, rewrite.term}) do
      givenpi %[(node n0_) (node n1_)] do |n1|
        if evr = eventR(changes, Term.of(:event, n0, n1), successor).as?(Rewrite::One)
          _, _, n1 = evr.term
        end

        Rewrite.one(Term.of(:node, n1))
      end

      givenpi %[(edge p0_ c0_) (edge p1_ c1_)] do |p1, c1|
        unless p0 == p1
          if evr = eventR(changes, Term.of(:event, p0, p1), successor).as?(Rewrite::One)
            _, _, p1 = evr.term
          end
        end
        unless c0 == c1
          if evr = eventR(changes, Term.of(:event, c0, c1), successor).as?(Rewrite::One)
            _, _, c1 = evr.term
          end
        end

        Rewrite.one(Term.of(:edge, p1, c1))
      end

      givenpi %[(event _ t1_) (event _ t2_)] do
        eventR(changes, Term.of(:event, t1, t2), successor).as?(Rewrite::One) || rewrite
      end
    end
  in Rewrite::Many
    raise ""
  end
end

def eventR(successor)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| eventR(changes, term, successor) }
  end
end

def effectR(successor, &fn : Term ->)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce do |term|
      fn.call(term)

      successor.call(changes, operand)
    end
  end
end

dollarR = exhR(dfsR(callR(primitives)))
backmapR = dfsR(
  choiceR(
    selR(%[($ rewritee_)], dollarR),
    selR(%[($once rewritee_)], callR(primitives))
  ),
)
rules = exhR(eventR(effectR(rulesetR(Ruleset.select(SELECTOR, ML.parse(base)), noR, backmapR, noR)) do |term|
  # Term.case(term) do
  #   matchpi %[(event _*)] do
  #     puts ML.display(term)
  #   end
  #   otherwise {}
  # end
end))
# callable = ->(term : Term) { rules.call(->{ true }, Rewrite.one(term)).as?(Rewrite::One).try(&.term) || term }

# puts callable.call(Term.of({ Term.of(:foo, Term.dict(:bar), "max-w": 10), Term.dict(:bar) }))

# preview =      Changes::Preview.new do |before, rewrite|
#         puts "- Rewrite ------"
#         puts ML.display(before)
#         rewrite.each do |term|
#           puts "->"
#           puts ML.display(before)
#         end
#         gets
#         nil
#       end
preview = ->{ true }

puts ML.display(outerR(preview, Term.of(:root, ML.parse1(frame)), rules))


# puts rules.call(Term.of(100))

# puts ML.display(rewriter.call(preview, Rewrite.one(ML.parse1(frame))).as(Rewrite::One).term)

