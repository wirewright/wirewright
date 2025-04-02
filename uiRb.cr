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

  BASE_CAPACITY = ENV["UIR_BASE_CAP"]?.try(&.to_i) || 2**16
  CTRL_CAPACITY = ENV["UIR_CTRL_CAP"]?.try(&.to_i) || 2**12

  @@base_cache = SyncCache(Term, Rewrite::Any).new(capacity: BASE_CAPACITY, preallocate: true)
  @@control_cache = SyncCache(Term, Rewrite::Any).new(capacity: CTRL_CAPACITY, preallocate: true)

  # Returns the UIR rewriter.
  #
  # TODO: move to `uiR.soma.wwml` once the rewriter DSL is available.
  class_getter rewriter : Rewriter do
    base_main = File.read(RESOURCES / "uiR-main.soma.wwml")
    base_control = File.read(RESOURCES / "uiR-control.soma.wwml")

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

    set_exhevalr, rec_exhevalr = recR

    flowR = choiceR(
      allR(
        wrapR(%{(if in_ a_ b_)}, %{in_}, rec_exhevalr, %{out_}, %{(if out_ a_ b_)}),
        switchR(
          { %{(if false _ rewritee_)}, rec_exhevalr},
          { %{(if _ rewritee_ _)}, rec_exhevalr},
        )
      )
    )
    primR = chainR(callR(primitives), callR(PRIMITIVES))
    onceR = choiceR(flowR, primR)
    exhevalR = set_exhevalr.call exhR(dfsR(onceR))

    evalR = dfsR(
      switchR(
        { %[($ rewritee_)], exhevalR },
        { %[($once rewritee_)], onceR },
      )
    )

    set_backmapr, rec_backmapr = recR

    refR = dfsR(
      switchR(
        { %[($my rewritee←($ _))], chainR(rec_backmapr, envR(Term.of(:"$my"))) },
        { %[($my rewritee_)], envR(Term.of(:"$my")) },
        { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
        { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
      )
    )

    backmapR = set_backmapr.call chainR(refR, evalR)

    selector = ML.term(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])

    set_main, rec_main = recR

    mainR = exhR(
      set_main.call memoR(@@base_cache,
        choiceR(
          rulesetR(Ruleset.select(selector, ML.terms(base_main)), noR, backmapR, noR),
          itemsR(rec_main),
        )
      )
    )

    set_control, rec_control = recR

    controlR = exhR(
      set_control.call memoR(@@control_cache, choiceR(
          rulesetR(Ruleset.select(selector, ML.terms(base_control)), noR, backmapR, noR),
          itemsR(rec_control),
      ))
    )

    exhR(chainR(mainR, controlR))
  end

  def drawable(uir : Term) : Term
    rewrite(uir, UIR.rewriter)
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
    #
    # TODO: document `#model` behavior.
    def microfold(initial = Term.of, &fn : Term, Term, Term -> Term) : Reducer
      unit0 = initial

      Reducer.new do |drawable0, event|
        unit1 = fn.call(unit0, drawable0, event)

        if unit0.type.dict? && unit1.type.dict? && !unit0.same?(initial) && unit0.without(:"#model") == unit1.without(:"#model")
          unit0 = unit1

          drawable0
        else
          unit0 = unit1
          uir = Microfold.uir(Microfold::SPEC, unit0)
          UIR.drawable(uir)
        end
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
    Term.case(drawable) do
      matchpi %[{¦ dl_number dt_number final-w: w_number final-h: h_number}] do
        x -= dl.unsafe_as_n
        y -= dt.unsafe_as_n

        return unless x.in?(Term[0]...w.unsafe_as_n)
        return unless y.in?(Term[0]...h.unsafe_as_n)

        sink.call(keypath)

        # Fallthrough
        continue
      end

      matchpi %{(viewport child_ ¦ _ pan-x_number pan-y_number)} do
        keypath.push(Term.of(1))

        x -= pan_x.unsafe_as_n
        y -= pan_y.unsafe_as_n

        # Yeeaah this reads strange...
        hit(child, x, y, sink, keypath)

        # Terminate
      ensure
        keypath.pop
      end

      matchpi %{_dict} do
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
