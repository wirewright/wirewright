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

  def follow(root : Term, keypath : Stack(Term)) : Term
    if keypath.empty?
      return root
    end

    root0 = root.as_d? || raise KeypathError.new
    root0.follow(keypath)
  end

  def assign(root : Term, keypath : Stack(Term), value : Term) : Term
    if keypath.empty?
      return value
    end

    root0 = root.as_d? || raise KeypathError.new
    root1 = root0.follow(keypath) { value }

    Term.of(root1)
  end
end

module UIR
  extend self

  @@cache = SyncMemo.new(capacity: 16_384, preallocate: true)

  # Returns the UIR rewriter.
  #
  # TODO: move to `uiR.soma.wwml` once the rewriter DSL is available.
  class_getter rewriter : Rewriter do
    base = File.read(RESOURCES / "uiR.soma.wwml")

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
          FontWeight.parse(weight.to(Int32)),
          size.to(Int32),
          leading.to(Float32),
        )

        {width: w, height: h}
      end

      rulepi1 %[(wrap caption_string font_string weight←(%any 100 200 300 400 450 500 600 700 800 900) size←(%number u8) leading_number ¦ max-w_: (%number +i32))] do
        Platform::Current.wrap(caption.to(String), font.to(String), FontWeight.parse(weight.to(Int32)), size.to(Int32), leading.to(Float32), w: max_w.to(Int32), h: nil)
      end

      rulepi1 %[(wrap caption_string font_string weight←(%any 100 200 300 400 450 500 600 700 800 900) size←(%number u8) leading_number ¦ max-w_: (%number +i32) max-h_: (%number +i32))] do
        Platform::Current.wrap(caption.to(String), font.to(String), FontWeight.parse(weight.to(Int32)), size.to(Int32), leading.to(Float32), w: max_w.to(Int32), h: max_h.to(Int32))
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
    abstract def wrap(content : String, font : String, weight : FontWeight, size : Int32, leading : Float32, w : Int32?, h : Int32?) : String
    abstract def measure(content : String, font : String, weight : FontWeight, size : Int32, leading : Float32) : {Int32, Int32}
    abstract def show(reducer : Reducer) : Nil
  end

  # :nodoc:
  def hit(drawable : Term, x : Term::Num, y : Term::Num, sink, keypath : Stack(Term)) : Nil
    inbounds = false

    Term.case(drawable) do
      matchpi %[{¦ l_number t_number final-w: w_number final-h: h_number}] do |l, t, w, h|
        l, t, w, h = {l, t, w, h}.map(&.unsafe_as_n)

        continue unless x.in?(l...l + w)
        continue unless y.in?(t...t + h)

        inbounds = true

        sink.call(keypath)

        # Fall through
        continue
      end

      matchpi %{(viewport child_ ¦ _ x: dx_number y: dy_number)} do
        # Fall through if viewport does not contain the point
        continue unless inbounds

        begin
          keypath.push(Term.of(1))

          # Yeeaah this reads strange...
          hit(child, x + dx, y + dy, sink, keypath)
        ensure
          keypath.pop
        end

        # Terminate
      end

      matchpi %{_dict} do
        # Fall through if drawable does not contain the point
        continue unless inbounds

        dict = drawable.unsafe_as_d
        dict.items.each_with_index do |child, index|
          keypath.push(Term.of(index))

          # Gosh
          hit(child, x, y, sink, keypath)
        ensure
          keypath.pop
        end
      end

      otherwise {}
    end
  end

  # Calls *sink* with keypaths (`Stack(Term)` *which you do not own*) of nodes
  # that include the point *x*, *y*.
  def hit(*args, **kwargs, &fn : Stack(Term) ->) : Nil
    hit(*args, **kwargs, sink: fn, keypath: Stack(Term).new)
  end
end
