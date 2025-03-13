require "./ufold"

# FIXME: this does not belong here
module ::Ww::Keypath
  extend self

  # TODO: while loop with D7#successor?-like impl.
  def each_item_impl(term, fn, keypath)
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

  def each_item(term : Term, &fn : Stack(Term), Term -> Bool?)
    each_item_impl(term, fn, keypath: Stack(Term).new)
  end

  def ascend(root : Term, keypath : Stack(Term), &)
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
end

module UIR
  extend self

  @@cache = SyncMemo.new(capacity: 16_384, preallocate: true)

  # Returns the UIR rewriter.
  #
  # TODO: move to `uiR.soma.wwml` once the rewriter DSL is available.
  class_getter rewriter : Rewriter do
    base = File.read("./uiR.soma.wwml")

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
        w, h = Platform::Current.measure(
          text.to(String),
          font.to(String),
          weight.to(Int32),
          size.to(Int32),
          leading.to(Float32),
        )

        {width: w, height: h}
      end

      rulepi1 %[(wrap caption_string font_string weight←(%any 100 200 300 400 450 500 600 700 800 900) size←(%number u8) leading_number ¦ max-w_: (%number +i32))] do
        Platform::Current.wrap(caption.to(String), font.to(String), weight.to(Int32), size.to(Int32), leading.to(Float32), w: max_w.to(Int32))
      end

      rulepi1 %[(wrap caption_string font_string weight←(%any 100 200 300 400 450 500 600 700 800 900) size←(%number u8) leading_number ¦ max-w_: (%number +i32) max-h_: (%number +i32))] do
        Platform::Current.wrap(caption.to(String), font.to(String), weight.to(Int32), size.to(Int32), leading.to(Float32), w: max_w.to(Int32), h: max_h.to(Int32))
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

    set, rec = recR

    exhR(
      set.call memoR(@@cache,
        choiceR(
          rulesetR(Ruleset.select(selector, ML.terms(base)), noR, backmapR, noR),
          itemsR(rec),
        )
      )
    )
  end

  # Reducers produce a drawable given the previous drawable and an event.
  alias Reducer = Term, Term -> Term

  module Reducers
    extend self

    # Uses `Microfold` followed by `UIR.rewriter` to resolve the unit returned by *fn*.
    #
    # - The first argument to *fn* is the unit returned by *fn* previously
    #   (*initial* for the first time). It can be used for state-keeping.
    # - The second argument to *fn* is the drawable produced from that unit.
    #   It can be used for hit-testing (see `UIR.hit`).
    # - The third argument to *fn* is the event.
    def microfold(initial unit0 = Term.of, &fn : Term, Term, Term -> Term) : Reducer
      Reducer.new do |drawable0, event|
        unit1 = fn.call(unit0, drawable0, event)
        unit0 = unit1
        uir = Microfold.uir(Microfold::SPEC, unit1)

        rewrite(uir, UIR.rewriter)
      end
    end
  end

  # Includers are UIR *platforms*, capable of displaying UIR.
  module IPlatform
    abstract def measure(content : String, font : String, weight : Int32, size : Int32, leading : Float32) : {Int32, Int32}
    abstract def show(reducer : Reducer) : Nil
  end

  private def offset(drawable : Term, keypath : Stack(Term)) : {Term::Num, Term::Num}
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

  # Calls *sink* with keypaths (`Stack(Term)` *which you do not own*) of nodes
  # that include the point *x*, *y*.
  def hit(drawable : Term, x : Term::Num, y : Term::Num, sink) : Nil
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

  # :ditto:
  def hit(*args, **kwargs, &fn : Stack(Term) ->) : Nil
    hit(*args, **kwargs, sink: fn)
  end
end
