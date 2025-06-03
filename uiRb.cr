# NOTE: the naming that evolved is quite confusing but I like it:
#  - UIR (all caps) is UI Representation
#  - uiR (lowercase, uppercase R) stands for UIR rewriter, it rewrites UIR to dwUIR
#  - dwUIR is short for drawable UIR.
#
# Some reorganization is required according to this naming schema for more consistency.

require "./ufold"

include Ww::Soma::DwUIR

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

record Point, x : Int32, y : Int32
record Rect, origin : Point, extent : Point

record Ring, l : UInt8, r : UInt8, t : UInt8, b : UInt8 do
  def x : UInt16
    l.to_u16 + r.to_u16
  end

  def y : UInt16
    t.to_u16 + b.to_u16
  end
end

enum Heading : UInt8
  Left
  Right
  Up
  Down

  def self.parse(term : Term)
    Term.case(term) do
      matchpi %{left} { Left }
      matchpi %{right} { Right }
      matchpi %{up} { Up }
      matchpi %{down} { Down }

      otherwise { Up }
    end
  end
end

abstract class DrawCommand
end

defcase FillTextSelection,
  anchor : UInt32,
  span : Int32,
  fill : Color,
  color : Color

# Draws a string of text.
#
# - *z* is the z-index of the text.
# - *caption* specifies the string value for the text.
# - *font*, *size*, *weight*, *leading*, and *tracking* are font properties.
# - *color* sets the color of the text.
# - *origin* sets the position where the text should be drawn.
defcase FillText < DrawCommand, z : Int32,
  origin : Point,
  caption : String,
  font : String,
  size : UInt16,
  weight : FontWeight,
  leading : Float32,
  tracking : Float32,
  color : Color,
  sel : FillTextSelection?

# Draws a filled rectangle.
#
# - *z* is the z-index of the rectangle.
# - *box* specifies its origin and extent.
# - *color* specifies its fill color.
# - *alpha* specifies the opacity (0 - transparent, 255 - opaque) of the fill.
# - *radius* specifies its corner radius (0 - square corners).
# - *ring* specifies its ring, which in CSS terms could be described as a fully
#   opaque box shadow. Setting to all-0 disables the ring.
defcase FillRect < DrawCommand,
  z : Int32,
  box : Rect,
  color : Color,
  alpha : UInt8,
  radius : UInt16,
  ring : Ring

# Draws a rectangle outline.
#
# - *z* is the z-index of the outline.
# - *box* specifies its origin and extent.
# - *color* specifies its color.
# - *thickness* specifies how thick the outline is.
# - *radius* specifies corner radius (0 - square corners).
defcase OutlineRect < DrawCommand,
  z : Int32,
  box : Rect,
  color : Color,
  thickness : UInt8,
  radius : UInt16

# Draws a filled circle.
#
# - *z* is the z-index of the circle.
# - *origin* specifies the location of its top-left corner.
# - *radius* is its radius.
# - *color* is the fill color.
defcase FillCircle < DrawCommand,
  z : Int32,
  origin : Point,
  radius : UInt16,
  color : Color

# Draws a filled triangle.
#
# - *z* is the z-index of the triangle.
# - *box* specifies its bounding box.
# - *heading* specifies where the triangle points (left, right, etc.)
# - *color* specifies the fill color of the triangle.
defcase FillTriangle < DrawCommand,
  z : Int32,
  box : Rect,
  heading : Heading,
  color : Color

# A limited view into the product of children draw commands *children*.
#
# - *z* is the z-index of the view itself. Its children may have different z-indices.
# - *box* is the bounding box of the view.
# - *color* is the clear-color of the view.
defcase View < DrawCommand,
  z : Int32,
  children : Array(DrawCommand),
  box : Rect,
  color : Color

enum Cursor : UInt8
  Arrow
  Pointer
  Grabbing
  Text

  def self.parse(term : Term)
    Term.case(term) do
      matchpi %{arrow} { Arrow }
      matchpi %{pointer} { Pointer }
      matchpi %{grabbing} { Grabbing }
      matchpi %{text} { Text }

      otherwise { Arrow }
    end
  end
end

defcase Window,
  cursor : Cursor,
  size : Point,
  color : Color,
  children : Array(DrawCommand)

# :nodoc:
record DrawContext, commands : Array(DrawCommand), setcursor : (Cursor ->)

# :nodoc:
#
# Appends draw commands associated with *node* to *ctx*.
#
# - *z* is the z-index of *node*.
def UIR::Platform.draw(ctx : DrawContext, node : Term, x : Int32, y : Int32, z : Int32) : Nil
  Term.case(node) do
    # Any node can specify the cursor.
    matchpi %[{¦ cursor_symbol}] do
      ctx.setcursor.call(Cursor.parse(cursor))

      continue
    end

    matchpi(
      %[(text ¦ _ color_ caption_string font_string leading_number
                  weight_: (%any 100 200 300 400 450 500 600 700 800 900)
                  size_: (%number u16)
                  dl_: (%number i32)
                  dt_: (%number i32))]
    ) do
      sel = nil
      sel_cfg = node.pluck(:"sel-anchor", :"sel-span", :"sel-fill", :"sel-color")
      unless sel_cfg.empty?
        Term.matchpi(sel_cfg, %[{¦ sel-color⋮ (rgb 0 0 255)
                                   sel-fill⋮ (rgb 255 255 255)
                                   sel-anchor: (%optional 0 sel-anchor←(%number u32))
                                   sel-span: (%optional 0 sel-span←(%number i32))}]
        ) do
          sel = FillTextSelection.new(sel_anchor.to(UInt32), sel_span.to(Int32), Color.term(sel_fill), Color.term(sel_color))
        end
      end

      ctx.commands << FillText.new(z,
        origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
        caption: caption.to(String),
        font: font.to(String),
        size: size.to(UInt16),
        weight: FontWeight.parse(weight.to(Int32)),
        leading: leading.to(Float32),
        tracking: 1.0f32,
        color: Color.term(color),
        sel: sel
      )
    end

    matchpi(
      %[(rect ¦ _ bg_
                  alpha: (%optional 255 alpha←(%number u8))
                  dl_: (%number i32)
                  dt_: (%number i32)
                  final-w: w←(%number +i32)
                  final-h: h←(%number +i32)
                  border-radius: (%optional 0 radius←(%number u16))
                  ring-l: (%optional 0 rl←(%number u8))
                  ring-r: (%optional 0 rr←(%number u8))
                  ring-t: (%optional 0 rt←(%number u8))
                  ring-b: (%optional 0 rb←(%number u8)))]
    ) do
      ctx.commands << FillRect.new(z,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
          extent: Point.new(w.to(Int32), h.to(Int32)),
        ),
        color: Color.term(bg),
        alpha: alpha.to(UInt8),
        radius: radius.to(UInt16),
        ring: Ring.new(rl.to(UInt8), rr.to(UInt8), rt.to(UInt8), rb.to(UInt8)),
      )
    end

    matchpi(
      %[(rect/outline ¦ _ bg_
                          dl_: (%number i32)
                          dt_: (%number i32)
                          final-w: w←(%number +i32)
                          final-h: h←(%number +i32)
                          border-width: thickness←(%number u8)
                          border-radius: (%optional 0 radius←(%number u16)))]
    ) do
      inset = thickness.to(Int32)

      ctx.commands << OutlineRect.new(z,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32) + inset, y + dt.to(Int32) + inset),
          extent: Point.new(w.to(Int32) - inset*2, h.to(Int32) - inset*2),
        ),
        color: Color.term(bg),
        thickness: inset.to_u8,
        radius: radius.to(UInt16),
      )
    end

    matchpi(
      %[(circle ¦ _ bg_
                    dl_: (%number i32)
                    dt_: (%number i32)
                    radius_: (%number u16))]
    ) do
      ctx.commands << FillCircle.new(z,
        origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
        radius: radius.to(UInt16),
        color: Color.term(bg),
      )
    end

    matchpi(
      %[(triangle ¦ _ bg_
                      dl_: (%number i32)
                      dt_: (%number i32)
                      final-w: w←(%number +i32)
                      final-h: h←(%number +i32)
                      pointing_: (%any left right up down))]
    ) do
      ctx.commands << FillTriangle.new(z,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
          extent: Point.new(w.to(Int32), h.to(Int32)),
        ),
        heading: Heading.parse(pointing),
        color: Color.term(bg),
      )
    end

    matchpi(
      %[(viewport child←{¦ final-w: cw←(%number +i32) final-h: ch←(%number +i32)}
         ¦ _ bg_
             dl_: (%number +i32)
             dt_: (%number +i32)
             final-w: w←(%number +i32)
             final-h: h←(%number +i32)
             pan-x_: (%number i32)
             pan-y_: (%number i32))]
    ) do
      children = [] of DrawCommand

      draw(ctx.copy_with(commands: children), child, pan_x.to(Int32), pan_y.to(Int32), z)

      # Sort children by layer (z-index) now that we know they're complete.
      #
      # Smaller z-index will be drawn on top of, so the default order (ASC) is fine.
      children.sort_by!(&.z)

      ctx.commands << View.new(z,
        children: children,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
          extent: Point.new(w.to(Int32), h.to(Int32)),
        ),
        color: Color.term(bg),
      )
    end

    matchpi(
      %[(layer child_ ¦ _ dl_: (%number i32)
                          dt_: (%number i32)
                          z-index: n←(%number i32))]
    ) do
      draw(ctx, child, x + dl.to(Int32), y + dt.to(Int32), z: n.to(Int32))
    end

    matchpi %[{¦ dl_: (%number i32) dt_: (%number i32)}] do
      node.items.each { |child| draw(ctx, child, x + dl.to(Int32), y + dt.to(Int32), z) }
    end

    otherwise { }
  end
end

# Converts uiR *markup* into a `Window` object. This object, among other
# things, contains an array of draw commands to be executed by a Painter
# to actually paint the window on the screen.
def UIR::Platform.draw(markup : Term) : Window
  Term.case(markup) do
    matchpi %{(window child_ ¦ _ bg_ final-w: w←(%number +i32) final-h: h←(%number +i32) cursor⋮ arrow)} do
      children = [] of DrawCommand
      cursor0 = Cursor.parse(cursor)

      setcursor = ->(proposal : Cursor) do
        # Only allow changing Arrow to any cursor.
        case {cursor0, proposal}
        when {Cursor::Arrow, _}
          cursor0 = proposal
        end
      end

      draw(DrawContext.new(children, setcursor), child, x: 0, y: 0, z: 0)

      # Sort children by layer (z-index) now that we know they're complete.
      #
      # Smaller z-index will be drawn on top of, so the default order (ASC) is fine.
      children.sort_by!(&.z)

      Window.new(cursor0, Point.new(w.to(Int32), h.to(Int32)), Color.term(bg), children)
    end

    # TODO: if markup is invalid, display an error window.
  end
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
          { %{(if false _ rewritee_)}, rec_exhevalr },
          { %{(if _ rewritee_ _)}, rec_exhevalr },
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

  # Includers are UIR *platforms*, capable of displaying *drawable UIR*
  # (known as *dwUIR* for short).
  module IPlatform
    abstract def font_extensions : Indexable(String)
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

      otherwise { }
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

  # TODO: this is lame, optimize!!
  def stratum(*args, **kwargs) : Array(Stack(Term))
    strata = strata(*args, **kwargs)
    unless row = strata.first?
      return [] of Stack(Term)
    end
    _, stratum = row
    stratum
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

      otherwise { }
    end
  end

  def node_and_coords?(dwuir : Term, &predicate : Term::Dict -> Bool) : {Term::Dict, Term::Num, Term::Num}?
    node_and_coords?(dwuir, ox: Term[0], oy: Term[0], predicate: predicate)
  end

  def approx_inline_charcount(uir : Term) : Int32
    Term.case(uir) do
      matchpi %{[floating _]} do
        0
      end

      matchpi %{(text ¦ _ caption_string)} do
        caption.charcount
      end

      matchpi %{[rect]}, %{[rect/outline]} do
        1
      end

      matchpi %{_dict} do
        uir.items.sum(0) { |node| approx_inline_charcount(node) }
      end

      otherwise { 0 }
    end
  end
end
