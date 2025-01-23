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


# base = <<-WWML
# ;; Utility: calculate px from pl, pr if absent; py from pt, pr if absent.
# (_ (_* ¦ _ pl: (%optional 0 pl_number) pr: (%optional 0 pr_number) px: (%- _ px))) <> {px: ($once (+ →pl →pr))}
# (_ (_* ¦ _ pt: (%optional 0 pt_number) pb: (%optional 0 pb_number) py: (%- _ py))) <> {py: ($once (+ →pt →pb))}

# ;; Generic nodes pass down their width/height minus padding.
# ((_* ¦ _ max-w: Pmw_number px: px_number) (_* ¦ _ max-w: (%- _ Cmw))) <> {Cmw: ($once (- →Pmw →px))}
# ((_* ¦ _ max-h: Pmh_number py: py_number) (_* ¦ _ max-h: (%- _ Cmh))) <> {Cmh: ($once (- →Pmh →py))}

# ;; Bordered box nodes override that and also subtract their border.
# ((box _ ¦ _ border: true max-w: Pmw_number px: px_number) (_* ¦ _ max-w: (%- _ Cmw))) <> {Cmw: ($once (- →Pmw →px 2))}
# ((box _ ¦ _ border: true max-h: Pmh_number py: py_number) (_* ¦ _ max-h: (%- _ Cmh))) <> {Cmh: ($once (- →Pmh →py 2))}

# ;; Column nodes propagate even max-height constraints to children nodes.
# ;; ???

# ;; If width: max, set width = max w.
# (_ (_* ¦ _ w: w←max max-w: mw_number)) <> {w: →mw}
# (_ (_* ¦ _ h: h←max max-h: mh_number)) <> {h: →mh}

# ;; Calculate width/height of a node from its content width/height.
# (_ (_* ¦ _ w: W←(content w_number) px: px_number)) <> {W: ($once (+ →w →px))}
# (_ (_* ¦ _ h: H←(content h_number) py: py_number)) <> {H: ($once (+ →h →py))}

# ;; Bordered boxes add their borders to the content width/height.
# (_ (box _ ¦ _ border: true w: W←(content w_number) px: px_number)) <> {W: ($once (+ →w →px 2))}
# (_ (box _ ¦ _ border: true h: H←(content h_number) py: py_number)) <> {H: ($once (+ →h →py 2))}

# ;; Learn w/h: content from single child.
# ((_ _ ¦ _ w: W←content) (_* ¦ _ w: w_number)) <> {W: (content →w)}
# ((_ _ ¦ _ h: H←content) (_* ¦ _ h: h_number)) <> {H: (content →h)}

# ;; Calculate content width and height of a text node. The content width
# ;; does not take wrapping into account; so we're fairly loose here on restrictions.
# (_ (text caption_string ¦ _ max-w: mw_number max-h: mh_number measured: (%- _ measured))) <>
#   {measured: ($once (measure →caption limit: (→mw →mh)))}

# ;; Compute text w/h:content based on measured width/height.
# (_ (text caption_string ¦ _ w: w←content measured: (xw_number _))) <> {w: →xw}
# (_ (text caption_string ¦ _ h: h←content measured: (_ xh_number))) <> {h: →xh}
# WWML

# "Top-down flow of width" => max-w
# "Bottom up flow of width" => content-w
# THEN once both are known (if necessary) decide which one to use using w: ...
base = <<-WWML
;; Utility: calculate px from pl, pr if absent; py from pt, pr if absent.
(node (_* ¦ _ pl: (%optional 0 pl_number) pr: (%optional 0 pr_number) px: (%- _ px))) <> {px: ($once (+ →pl →pr))}
(node (_* ¦ _ pt: (%optional 0 pt_number) pb: (%optional 0 pb_number) py: (%- _ py))) <> {py: ($once (+ →pt →pb))}

;; Parents teach max-w/h to their children.
(edge (_* ¦ _ max-w: W_number px: px_number) (_* ¦ _ max-w: (%- _ w))) <> {w: ($once (- →W →px))}
(edge (_* ¦ _ max-h: H_number py: py_number) (_* ¦ _ max-h: (%- _ h))) <> {h: ($once (- →H →py))}

;; Parents learn content-w/h from their children.
(edge (_ _ ¦ _ content-w: (%- _ W) px: px_number) (_* ¦ _ content-w: w_number)) <> {W: ($once (+ →w →px))}
(edge (_ _ ¦ _ content-h: (%- _ H) py: py_number) (_* ¦ _ content-h: h_number)) <> {H: ($once (+ →h →py))}

;; Text node should measure its caption to obtain its content-w/h
(node (text caption_string ¦ _ content-w: (%- _ w) content-h: (%- _ h)))
  <> {w: ($once (measure-width →caption)),
      h: ($once (measure-height →caption))}

;; Each unaccounted child of column should compute max of itself and
;; the column's content-w.
(edge (col _+ ¦ _ content-w: (%optional 0 W_number)) (_* ¦ _ content-w: w_number state: state←in-h))
  <> {W: ($once (max →W →w)), state: in-wh}

;; Each unaccounted child of column should add itself to the column's content-h.
(edge (col _+ ¦ _ content-h: (%optional 0 H_number)) (_* ¦ _ content-h: h_number state: (%- _ state)))
  <> {H: ($once (+ →H →h)), state: in-h}

;; Resolve w/h: content as content-w/h clipped at max-w/h. Note that we make h:
;; depend on w: to have predictable ordering.
(node (_* ¦ _ max-w: mw_number content-w: cw_number w: w←content))
  <> {w: ($once (min →mw →cw))}

(node (_* ¦ _ max-h: mh_number content-h: ch_number w: _number h: h←content))
  <> {h: ($once (min →mh →ch))}

;; When a text learns its width, it should immediately wrap itself at that width.
(event (text _ ¦ _ w: (%- _number)) (text caption_string ¦ _ w: w_number pre: pre_boolean wrap-w: (%- _ wrap-w)))
  <> {caption: ($once (wrap →caption w: →w pre: →pre)), wrap-w: →w} 

;; When a text's wrapped width is known, the text should immediately compute
;; its wrapped height.
(event (text _ ¦ _ wrap-w: (%- _)) (text caption_string ¦ _ wrap-w: _number wrap-h: (%- _ wrap-h)))
  <> {wrap-h: ($once (span →caption subject: line))}

;; For a text node, we use its wrapped height instead of content-h to determine h: content.
(node (text _ ¦ _ max-h: mh_number wrap-h: wh_number h: h←content))
  <> {h: ($once (min →mh →wh))}
WWML

# base2 = <<-WWML
# (node (h←qux n_ m_)) <> {n: ($once (+ 1 →n)), h: qyx}
# (node (qyx n_ m_ seen: (%- _ m))) <> {m: true}
# ;;(event (qux n0_ m_) M←(qyx n1_ m_)) <> {M: (qyyx →n0 →n1 →m)}
# WWML

frame = <<-WWML
(viewport l: 0 t: 0 w: max h: max max-w: 64 max-h: 32 bg: (0 0 0)
  (col w: content h: content
    (text pre: true w: content h: content "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.")
    (text pre: true w: content h: content "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.")
    (text pre: true w: content h: content "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.")))
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

# def changeR(changes, term0, term1, successor)
#   if term0 == term1
#     return term0
#   end

#   subject = Term.of(:event, term0, term1)

#   case rewrite = successor.call(changes, Rewrite.one(subject))
#   in Rewrite::None then term1
#   in Rewrite::One  then _, _, _term2 = rewrite.term # (event term0 term1) -> (event term0 term2)
#   in Rewrite::Many
#     raise "changeR: Rewrite::Many is not implemented"
#   end
# end

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
      # # React to change
      # v0 = changeR(changes, v0, v1, successor)
      # Rewrite edge
      dict1, v0 = edgeR(changes, dict1, v0, successor)
      # # React to change
      # v0 = changeR(changes, v0, v1, successor)
      # Rewrite recursively
      v0 = outerR(changes, v0, successor)
      # # React to change
      # v0 = changeR(changes, v0, v1, successor)
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
  rulepi %[(+ ns_number+)] do
    Rewrite.one(ns.items.reduce { |s, n| s.unsafe_as_n + n.unsafe_as_n })
  end

  rulepi %[(- ns_number+)] do
    Rewrite.one(ns.items.reduce { |s, n| s.unsafe_as_n - n.unsafe_as_n })
  end

  rulepi %[(min ns_number+)] do
    Rewrite.one(ns.items.min_by(&.unsafe_as_n))
  end

  rulepi %[(max ns_number+)] do
    Rewrite.one(ns.items.max_by(&.unsafe_as_n))
  end

  # rulepi %[(measure text_string limit: (w←(%number +i32) h←(%number +i32)))] do
  #   wrapped = wrap(text.to(String), w.to(Int32), h.to(Int32))
  #   wrapped_w = wrapped.each_line(chomp: true).max_of(&.size)
  #   wrapped_h = wrapped.each_line.size

  #   Rewrite.one({wrapped_w, wrapped_h})
  # end

  rulepi %[(measure-width text_string)] do
    w, _ = measure(text.to(String))

    Rewrite.one(w)
  end

  rulepi %[(measure-height text_string)] do
    _, h = measure(text.to(String))

    Rewrite.one(h)
  end

  rulepi %[(wrap text_string w: w←(%number +i32) pre: pre_boolean)] do
    Rewrite.one(wrap(text.to(String), w: w.to(Int32), h: nil, pre: pre.true?))
  end

  # TODO: subject: rune
  # TODO: subject: word
  rulepi %[(span text_string subject: line)] do
    Rewrite.one(text.to(String).each_line.size)
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

