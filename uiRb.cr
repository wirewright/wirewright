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

  def ascend(root : Term, keypath : Stack(Term), & : Term::Dict -> Bool)
    stack = Stack(Term::Dict).new
    tip = root

    keypath.each do |step|
      unless node0 = tip.as_d?
        raise KeypathError.new
      end

      unless node1 = node0[step]?
        raise KeypathError.new
      end

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

module UIR::Platform
end

module UIR
  extend self

  BASE_CAPACITY = ENV["UIR_BASE_CAP"]?.try(&.to_i) || 2**16
  CTRL_CAPACITY = ENV["UIR_CTRL_CAP"]?.try(&.to_i) || 2**12

  # FIXME: these caches will leak a whole bunch of memory ... Note terms on both
  # sides; they're pointers sometimes, so the GC won't be able to collect them and
  # so on. This must be a weak ref cache of WeakRef(Term::Dict) => Rewrite::Any ---
  # somehow!
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

    # Successor rewriter is only called with dicts (presumably UIR nodes) that do not
    # have the ready prop set. This is an example of how a rewriter circuit and a rule
    # system that can cooperate, in this case for performance. The latter computes `ready`
    # and the former is using it to direct the rewriting process.
    nonreadyR = ->(successor : Rewriter) do
      Rewriter.new do |ctx, staging|
        staging.reduce do |term|
          next Rewrite.none unless dict = term.as_d?
          next Rewrite.none if dict[:ready]?

          successor.call(ctx, Rewrite.one(dict))
        end
      end
    end

    set_main, rec_main = recR

    mainR = exhR(
      set_main.call nonreadyR.call(memoR(@@base_cache,
        choiceR(
          rulesetR(Ruleset.select(selector, ML.terms(base_main)), noR, backmapR, noR),
          itemsR(rec_main),
        )
      ))
    )

    set_control, rec_control = recR

    controlR = exhR(
      set_control.call nonreadyR.call(memoR(@@control_cache, choiceR(
          rulesetR(Ruleset.select(selector, ML.terms(base_control)), noR, backmapR, noR),
          itemsR(rec_control),
      )))
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

  private def hit(dwuir, x : Term::Num, y : Term::Num, sink, keypath, predicate) : Nil
    Term.case(dwuir) do
      matchpi %{(floating subnode_ ¦ _ dl_number dt_number)} do
        return unless predicate.call(keypath, dwuir)

        x -= dl.unsafe_as_n
        y -= dt.unsafe_as_n

        keypath.push(Term.of(1)) do
          hit(subnode, x, y, sink, keypath, predicate)
        end
      end

      matchpi %[{¦ dl_number dt_number final-w: w_number final-h: h_number}] do
        x -= dl.unsafe_as_n
        y -= dt.unsafe_as_n

        # Due to floating elements we'll have to visit subnodes anyway,
        # even if the parent does not contain the hit point. However, as
        # a slight optimization, do this only if the parent probably contains
        # `floating`. We'll either get a definite no (best) or a probable yes
        # (not good). Since sketches are hierarchical there's some chance
        # we'll not need to recurse too deep if there's no floating element.

        return if x.negative? || y.negative? # These are a definite no.

        if x.in?(Term[0]..w.unsafe_as_n) && y.in?(Term[0]..h.unsafe_as_n)
          sink << keypath.dup
        elsif !dwuir.probably_includes?(Term[:floating])
          return
        end

        # Fallthrough
        continue
      end

      matchpi %{(viewport subnode_ ¦ _ pan-x_number pan-y_number)} do
        return unless predicate.call(keypath, dwuir)

        keypath.push(Term.of(1)) do
          x -= pan_x.unsafe_as_n
          y -= pan_y.unsafe_as_n

          hit(subnode, x, y, sink, keypath, predicate)
        end
      end

      matchpi %{_dict} do
        return unless predicate.call(keypath, dwuir)

        dict = dwuir.unsafe_as_d
        dict.items.each_with_index do |subnode, index|
          keypath.push(Term.of(index)) do
            hit(subnode, x, y, sink, keypath, predicate)
          end
        end
      end

      otherwise {}
    end
  end

  def hit(*args, **kwargs, &predicate : Stack(Term), Term -> Bool) : Array(Stack(Term))
    sink = [] of Stack(Term)
    hit(*args, **kwargs, sink: sink, keypath: Stack(Term).new, predicate: predicate)
    sink
  end

  # Returns a hash of strata under point *x*, *y*. Strata are sorted
  # by their Z-index. The highest Z-index goes first. Each stratum is
  # a list of keypaths for elements hit in that stratum.
  def strata(dwuir : Term, x : Term::Num, y : Term::Num, &predicate : Stack(Term), Term -> Bool) : Hash(Term::Num, Array(Stack(Term)))
    hits = hit(dwuir, x, y, &predicate)
    hits = hits.map { |keypath| {z_index(dwuir, keypath), keypath} }

    # Sort by z-index descending.
    hits.unstable_sort! { |(z0, _), (z1, _)| z1 <=> z0 }

    # NOTE: assumes Crystal hash tables are ordered (they are).
    strata = {} of Term::Num => Array(Stack(Term))
    hits.each do |z, keypath|
      stratum = strata.put_if_absent(z) { [] of Stack(Term) }
      stratum << keypath
    end

    strata
  end

  def strata(*args, **kwargs)
    strata(*args, **kwargs) { true }
  end

  def z_index(dwuir : Term, keypath : Stack(Term)) : Term::Num
    zmax = Term[0]

    Keypath.ascend(dwuir, keypath) do |node|
      Term.case(node) do
        matchpi %{(layer _ ¦ _ z-index: z←(%number i32))} do
          zmax = Math.max(zmax, z.unsafe_as_n)

          true # break
        end

        otherwise do
          false # continue
        end
      end
    end

    zmax
  end

  private def node_and_coords?(dwuir, ox : Term::Num, oy : Term::Num, predicate : Term::Dict -> Bool)
    Term.case(dwuir) do
      matchpi %{(floating subnode←{¦ final-w: w_number final-h: h_number} ¦ _ dl_number dt_number)} do
        ox += dl.unsafe_as_n
        oy += dt.unsafe_as_n

        if predicate.call(dwuir.unsafe_as_d)
          return dwuir.unsafe_as_d, ox, oy
        end

        node_and_coords?(subnode, ox, oy, predicate)
      end

      matchpi %[{¦ dl_number dt_number final-w: w_number final-h: h_number}] do
        ox += dl.unsafe_as_n
        oy += dt.unsafe_as_n

        if predicate.call(dwuir.unsafe_as_d)
          return dwuir.unsafe_as_d, ox, oy
        end

        # Fallthrough
        continue
      end

      matchpi %{(viewport subnode_ ¦ _ pan-x_number pan-y_number)} do
        node_and_coords?(subnode, pan_x.unsafe_as_n, pan_y.unsafe_as_n, predicate)
      end

      matchpi %{_dict} do
        dict = dwuir.unsafe_as_d
        dict.items.each_with_index do |subnode, index|
          next unless response = node_and_coords?(subnode, ox, oy, predicate)
          return response
        end
      end

      otherwise {}
    end
  end

  def node_and_coords?(dwuir : Term, &predicate : Term::Dict -> Bool) : {Term::Dict, Term::Num, Term::Num}?
    node_and_coords?(dwuir, ox: Term[0], oy: Term[0], predicate: predicate)
  end
end
