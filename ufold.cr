require "./src/wirewright"
require "./colors"

SPEC = ML.terms File.read("ufold.spec.wwml")

module Microfold
  extend self

  private def consume?(r : Char::Reader, target : Char) : Char::Reader?
    return unless r.current_char == target

    r.next_char
    r
  end

  private def consume?(r : Char::Reader, target : String) : Char::Reader?
    target.each_char do |char|
      return unless r.current_char == char

      r.next_char
    end

    r
  end

  private def consume?(r : Char::Reader, target : UInt32.class) : {Char::Reader, String}?
    b = r.pos
    n = 0

    r.each do |char|
      break unless char.number?
      n += 1
    end

    return unless n > 0

    {r, r.string.byte_slice(b, n)}
  end

  private def remainder?(r : Char::Reader) : String?
    rest = r.string.byte_slice(r.pos, r.string.bytesize - r.pos)
    rest.empty? ? nil : rest
  end

  # :nodoc:
  SYM_COLOR = Term[:color]
  # :nodoc:
  SYM_WORD = Term[:word]
  # :nodoc:
  SYM_NAT = Term[:nat]
  # :nodoc:
  SYM_INT = Term[:int]

  # :nodoc:
  record SheetContext,
    spec : Term::Dict,
    sheet : Term::Dict::Commit,
    attrs : Term::Dict,
    colors : Term::Dict,
    rem : Term::Num,
    blacklist = Pf::Set(Term).new

  private def word?(ctx : SheetContext, r : Char::Reader, leader : String) : String?
    return unless r = consume?(r, leader)
    return unless r = consume?(r, '-')
    return unless word = remainder?(r)

    if word.prefixed_by?('[') && word.postfixed_by?(']')
      begin
        key = ML.term(word[1...-1])
      rescue ML::SyntaxError
        return
      end

      return unless value = ctx.attrs[key]?

      case value.type
      when .number?
        return value.inspect
      when .string?, .symbol?
        return value.to(String)
      else
        return
      end
    end

    word
  end

  # Returns `true` if a style was applied. Returns `false` otherwise.
  private def var?(ctx : SheetContext, type : Term::Sym, leader : String, var : Term, body : Term, phrase : String) : Bool
    r = Char::Reader.new(phrase)

    case type
    when SYM_COLOR
      return false unless color = word?(ctx, r, leader)
      return false unless subst = ctx.colors[color]?
    when SYM_WORD
      return false unless word = word?(ctx, r, leader)

      subst = Term::Str.new(word)
    when SYM_NAT
      return false unless word = word?(ctx, r, leader)
      return false unless nat = word.to_u32?

      subst = Term::Num.new(nat)
    when SYM_INT
      negative = false
      if r1 = consume?(r, '-')
        negative = true
        r = r1
      end

      return false unless word = word?(ctx, r, leader)
      return false unless nat = word.to_i?

      subst = Term::Num.new(negative ? -nat : nat)
    end

    instance = M1.bsubst(body, Term[].with(var, subst))
    instance.each_pair { |key, value| sheet1(ctx, key, value) }

    true
  end

  private def subphrases?(ctx : SheetContext, type : Term::Sym, leader : String, subphrases : Term::Dict::ItemsView, phrase : String) : Bool
    r = Char::Reader.new(phrase)

    prefix = postfix = Term[""]

    # NOTE: we rely on the subphrases to actually validate whether an int/word/nat
    # was passed.
    case type
    when SYM_WORD, SYM_NAT
      return false unless postfix = word?(ctx, r, leader)
    when SYM_INT
      if r1 = consume?(r, '-')
        prefix = Term["-"]
        r = r1
      end

      return false unless postfix = word?(ctx, r, leader)
    end

    subphrases.all? do |subphrase|
      sum = prefix.stitch(subphrase).stitch("-").stitch(postfix)

      sheet1?(ctx, sum)
    end
  end

  private def apply?(ctx : SheetContext, rule : Term, phrase : Term::Str) : Bool
    return false if rule.in?(ctx.blacklist)

    Term.case(rule) do
      matchpi %{(_symbol _string _ _dict)} do
        type, leader, var, body = rule

        var?(ctx, type.unsafe_as_sym, leader.to(String), var, body, phrase.to(String))
      end

      matchpi %{(_symbol _string (+ _string+))} do
        type, leader, sum = rule
        subphrases = sum.items.move(1)

        subphrases?(ctx.copy_with(blacklist: ctx.blacklist.add(rule)), type.unsafe_as_sym, leader.to(String), subphrases, phrase.to(String))
      end
    end
  end

  private def sheet1(ctx : SheetContext, key : Term, value : Term) : Nil
    Term.case(value) do
      matchpi %{(rem n_number)} do
        ctx.sheet.with(key, n * ctx.rem)
      end

      matchpi %{(spacing n_number)} do
        ctx.sheet.with(key, n * ctx.rem * 0.25)
      end

      matchpi %{(oklch l←(%number 0 <= _ <= 1) c←(%number 0 <= _ <= 1) h←(%number 0 <= _ <= 360))} do
        ctx.sheet.with(key, oklch(l.to(Float64), c.to(Float64), h.to(Float64)))
      end

      matchpi %{(/ a_number (%all (%not 0) b_number))} do
        ctx.sheet.with(key, a / b)
      end

      matchpi %{_symbol}, %{_string}, %{_number}, %{_boolean} do
        ctx.sheet.with(key, value)
      end
    end
  end

  private def sheet1?(ctx : SheetContext, phrase : Term::Str) : Bool
    unless static = ctx.spec[phrase]?
      return ctx.spec.items.any? { |rule| apply?(ctx, rule, phrase) }
    end

    Term.case(static) do
      matchpi %{_string} do
        sheet1?(ctx, static.unsafe_as_s)
      end

      matchpi %{(+ _string+)} do
        subphrases = static.items.move(1)
        subphrases.all? { |subphrase| sheet1?(ctx, subphrase.unsafe_as_s) }
      end

      matchpi %{(¦ (%all (%not ()) _dict))} do
        static.each_pair { |key, value| sheet1(ctx, key, value) }

        true
      end
    end
  end

  # Parses the given *style* string into a *sheet*: a dictionary where utilities
  # (also called "phrases") from *style* are resolved.
  #
  # *rem* specifies the root font size on which the majority of size calculations
  # will be based.
  #
  # Sheets serve as sources of styles for units (see `uir`).
  def sheet(spec : Term::Dict, attrs : Term::Dict, style : String, *, rem = Term[16]) : Term::Dict
    Term::Dict.build do |sheet|
      ctx = SheetContext.new(spec, sheet, attrs, spec[:colors]?.try(&.as_d?).default(Term[]), rem)

      style.split(' ', remove_empty: true) do |phrase|
        sheet1?(ctx, Term::Str.new(phrase))
      end
    end
  end

  # :nodoc:
  record UnitContext,
    spec : Term::Dict,
    sheet : Term::Dict,
    rem : Term::Num

  # String and other non-dict children of units are displayed as text.
  private def text_box(ctx : UnitContext, caption : Term::Str)
    Term.of(:text, caption,
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
      font: ctx.sheet[:font]?,
      weight: ctx.sheet[:"font-weight"]?,
      size: ctx.sheet[:"text-size"]?,
      leading: ctx.sheet[:leading]?,
      color: ctx.sheet[:"text-color"]?
    )
  end

  # Interprets the children of a unit. A child can be a text box or another
  # (nested) unit.
  private def child_box(ctx : UnitContext, child : Term)
    Term.case(child) do
      matchpi %{_string} do
        text_box(ctx, child.unsafe_as_s)
      end

      # Note how we recurse here.
      otherwise do
        unit_box(ctx, child)
      end
    end
  end

  # Appears if there are more than one children. Handles `flow-row`/`flow-col`
  # prop as well as the `gap` prop.
  private def flow_box(ctx : UnitContext, children : Term)
    if children.size == 1
      return child_box(ctx, children[0])
    end

    flow = Term::Dict.build do |commit|
      commit.with(:gap, ctx.sheet[:gap]?)

      case ctx.sheet[:flow]?
      when Term.of(:col)
        commit << Term.of(:"y-stack")
        commit.concat(children.items) do |child|
          child_box(ctx.copy_with(sheet: ctx.sheet.with(:h, :content)), child)
        end
      else
        # In case of row as well as anything else (e.g. absence) we use row
        # flow (x-stack).
        commit << Term.of(:"x-stack")
        commit.concat(children.items) do |child|
          child_box(ctx.copy_with(sheet: ctx.sheet.with(:w, :content)), child)
        end
      end
    end

    Term.of(flow)
  end

  # Appears if at least one of the padding props (such as `pl`, `pt`) is present.
  # Contains the flow box.
  private def padding_box(ctx : UnitContext, children : Term)
    pl = ctx.sheet[:pl]?
    pr = ctx.sheet[:pr]?
    pt = ctx.sheet[:pt]?
    pb = ctx.sheet[:pb]?

    if {pl, pr, pt, pb}.none?
      return flow_box(ctx, children)
    end

    Term.of(:padding, flow_box(ctx, children),
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
      pl: pl || 0,
      pr: pr || 0,
      pt: pt || 0,
      pb: pb || 0,
    )
  end

  # Appears if the background prop (`bg`) is present. Handles the ring. Contains
  # the padding box.
  private def background_box(ctx : UnitContext, children : Term)
    unless bg = ctx.sheet[:bg]?
      return padding_box(ctx, children)
    end

    rl = ctx.sheet[:"ring-l"]?
    rr = ctx.sheet[:"ring-r"]?
    rt = ctx.sheet[:"ring-t"]?
    rb = ctx.sheet[:"ring-b"]?

    if {rl, rr, rt, rb}.any?
      r0 = 0
    end

    Term.of(:"z-stack",
      Term.of(:rect,
        w: :max,
        h: :max,
        bg: bg,
        "border-radius": ctx.sheet[:"border-radius"]?,
        rl: rl || r0,
        rr: rr || r0,
        rt: rt || r0,
        rb: rb || r0,
      ),
      padding_box(ctx, children),
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
    )
  end

  # Appears if at least one border prop (such as `bt`, `bl`, etc.) and border
  # color prop are present. Handles the ring. Contains the background box.
  private def border_box(ctx : UnitContext, children : Term)
    bl = ctx.sheet[:"border-l"]?
    br = ctx.sheet[:"border-r"]?
    bt = ctx.sheet[:"border-t"]?
    bb = ctx.sheet[:"border-b"]?
    border_color = ctx.sheet[:"border-color"]?

    unless {bl, br, bt, bb}.any? && border_color
      return background_box(ctx, children)
    end

    rl = ctx.sheet[:"ring-l"]?
    rr = ctx.sheet[:"ring-r"]?
    rt = ctx.sheet[:"ring-t"]?
    rb = ctx.sheet[:"ring-b"]?

    if {rl, rr, rt, rb}.any?
      r0 = 0
      ctx = ctx.copy_with(
        sheet: ctx.sheet.without(:"ring-l", :"ring-r", :"ring-t", :"ring-b")
      )
    end

    Term.of(:"z-stack",
      Term.of(:rect,
        w: :max,
        h: :max,
        bg: border_color,
        "border-radius": ctx.sheet[:"border-radius"]?,
        "ring-l": rl || r0,
        "ring-r": rr || r0,
        "ring-t": rt || r0,
        "ring-b": rb || r0,
      ),
      Term.of(:padding,
        background_box(ctx, children),
        w: :max,
        h: :max,
        pl: bl || 0,
        pr: br || 0,
        pt: bt || 0,
        pb: bb || 0,
      ),
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
    )
  end

  # Appears if the min-width (`min-w`) prop is present. Contains the border box.
  private def minw_box(ctx : UnitContext, children : Term)
    unless minw = ctx.sheet[:"min-w"]?
      return border_box(ctx, children)
    end

    Term.of(:"x-expand", border_box(ctx, children),
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
      "min-w": minw,
    )
  end

  # Appears if the min-height (`min-h`) prop is present. Contains the min-width box.
  private def minh_box(ctx : UnitContext, children : Term)
    unless minh = ctx.sheet[:"min-h"]?
      return minw_box(ctx, children)
    end

    Term.of(:"y-expand", minw_box(ctx, children),
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
      "min-h": minh,
    )
  end

  # Appears if the z-index (`z`) prop is present. Contains the min-height box.
  private def layer_box(ctx : UnitContext, children : Term)
    unless z = ctx.sheet[:z]?
      return minh_box(ctx, children)
    end

    Term.of(:layer, minh_box(ctx, children),
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
      "z-index": z,
    )
  end

  # Appears if at least one delta (`dt`, `dl`) prop is present. Contains the layer box.
  private def translate_box(ctx : UnitContext, children : Term)
    dl = ctx.sheet[:dl]?
    dt = ctx.sheet[:dt]?

    if {dl, dt}.none?
      return layer_box(ctx, children)
    end

    Term.of(:translate, layer_box(ctx, children),
      w: ctx.sheet[:w]?,
      h: ctx.sheet[:h]?,
      x: dl,
      y: dt,
    )
  end

  # Handles the unit-unit boundary.
  private def unit_box(ctx : UnitContext, unit : Term)
    uir(ctx.spec, unit, rem: ctx.rem, inherited: ctx.sheet.pluck(:font, :"font-weight"))
  end

  # Returns the UIR tree corresponding to *unit*.
  #
  # *Units* are represented as a series of nested boxes. Each box is instantiated
  # on demand to consume certain style properties if those properties are present.
  # For example, a padding box is instantiated if padding properties are present.
  # Units can be nested.
  def uir(spec : Term::Dict, unit : Term, *, rem = Term[16], inherited = Term[])
    Term.case(unit) do
      matchpi %{(node_symbol children_+ ¦ attrs_ style⋮ "")} do |style|
        # Based on the node we can have certain "default" styles, we call them
        # "nodal" (per-node, node-specific) styles.
        nodal = Term[]
        if (defaults = spec[:defaults, node]?) && (defaults = defaults.as_s?)
          nodal = sheet(spec, attrs.unsafe_as_d, defaults.to(String), rem: rem)
        end

        sheet = nodal | inherited | sheet(spec, attrs.unsafe_as_d, style.to(String), rem: rem)

        box = translate_box(UnitContext.new(spec, sheet, rem), children)

        # Attach toplevel props to the box.
        box = box.morph(
          {:fr, sheet[:fr]?},
          {:fractions, sheet[:fractions]?},
        )

        Term.of(box)
      end
    end
  end
end

puts ML.display(Microfold.uir(SPEC.as_d, Term.of(:x, Term.of(:p, "A"), Term.of(:p, "B"), Term.of(:p, "C"), style: "font-mono font-bold")))
