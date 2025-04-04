require "./src/wirewright"

# Microfold (µfold) is the engine that handles styles. It's the Tailwind
# of Wirewright, except it also emits nodes since the underlying rule system,
# UIR, is much simpler than HTML/CSS.
#
# For instance, via µfold, we can emulate a basic box model. Imagine if Tailwind
# automatically surrounded with nodes based on the presence of certain classes
# (`bg` creates a background box, `p` creates a padding box, etc.) That is exactly
# what µfold does.
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
  SYM_DYNAMIC = Term[:dynamic]

  # :nodoc:
  record SheetContext,
    spec : Term::Dict,
    sheet : Term::Dict::Commit,
    attrs : Term::Dict,
    colors : Term::Dict,
    rem : Term::Num,
    blacklist = Pf::Set(Term).new

  # NOTE:
  # - `[]` resolves as if it was part of the prop, e.g. bg-[qux], qux: red-500 => sheet: {bg: (oklch ...)}
  # - `{}` is pasted as-is into the sheet, e.g. bg-{qux}, qux: red-500 => sheet: {bg: red-500}

  private def dynamic?(ctx : SheetContext, r : Char::Reader, leader : String) : Term?
    return unless r = consume?(r, leader)
    return unless r = consume?(r, '-')
    return unless word = remainder?(r)
    return unless word.prefixed_by?('{') && word.postfixed_by?('}')

    begin
      key = ML.term(word[1...-1])
    rescue ML::SyntaxError
      return
    end

    ctx.attrs[key]?
  end

  private def word?(ctx : SheetContext, r : Char::Reader, leader : String) : String?
    return unless r = consume?(r, leader)
    return unless r = consume?(r, '-')
    return unless word = remainder?(r)

    unless word.prefixed_by?('[') && word.postfixed_by?(']')
      return word
    end

    begin
      key = ML.term(word[1...-1])
    rescue ML::SyntaxError
      return
    end

    return unless value = ctx.attrs[key]?

    case value.type
    when .number?
      value.inspect
    when .string?, .symbol?
      value.to(String)
    end
  end

  # Returns `true` if a style was applied. Returns `false` otherwise.
  private def var?(ctx : SheetContext, type : Term::Sym, leader : String, var : Term, body : Term, phrase : String) : Bool
    r = Char::Reader.new(phrase)

    case type
    when SYM_DYNAMIC
      return false unless subst = dynamic?(ctx, r, leader)
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
        ctx.sheet.with(key, (n * ctx.rem).ceil)
      end

      matchpi %{(spacing n_number)} do
        ctx.sheet.with(key, (n * ctx.rem * 0.25).ceil)
      end

      matchpi %{(oklch l←(%number 0 <= _ <= 1) c←(%number 0 <= _ <= 1) h←(%number 0 <= _ <= 360))} do
        ctx.sheet.with(key, oklch(l.to(Float64), c.to(Float64), h.to(Float64)))
      end

      matchpi %{(/ a_number (%all (%not 0) b_number))} do
        ctx.sheet.with(key, a / b)
      end

      matchpi %{(unset)} do
        ctx.sheet.without(key)
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
  # are based.
  #
  # *attrs* are the attributes that are going to be accessible from *style* using
  # the `[]` syntax: e.g. `p-[padding]` will read the value of *padding* (a number,
  # string, or symbol) from *attrs*, normalize that as a string, and use that. E.g.
  # `p-[padding]` with attrs `{padding: 3}` will resolve to `p-3`.
  #
  # Sheets serve as sources of styles for units (see `uir`).
  def sheet(spec : Term::Dict, attrs : Term::Dict, style : String, *, rem = Term[16], base = Term[]) : Term::Dict
    base.transaction do |sheet|
      ctx = SheetContext.new(spec, sheet, attrs, spec[:colors]?.try(&.as_d?).default(Term[]), rem)

      style.split(' ', remove_empty: true) do |phrase|
        parts = phrase.split(':', limit: 2)

        case parts.size
        when 0 # ?!
        when 1
          phrase = parts[0]
        when 2
          group, phrase = parts
          next if phrase.empty? # ?!
          next if attrs[Term::Sym.new(group)]?.in?(nil, Term[false])
        end

        sheet1?(ctx, Term::Str.new(phrase))
      end
    end
  end

  # :nodoc:
  record UnitContext, spec : Term::Dict, sheet : Term::Dict, rem : Term::Num, collapse : Bool, nested : Bool do
    # Merges child output contexts *coctxs* into parent's output context
    # originating from *ictx*.
    #
    # The principle is, if at least one child consumed a prop, it is removed
    # from parent's octx.
    def self.octx(ictx : UnitContext, coctxs : Enumerable(UnitContext)) : UnitContext
      octx = ictx

      ictx.sheet.each_entry do |key, value|
        if coctxs.any? { |coctx| !key.in?(coctx.sheet) } # consumed
          octx = octx.override(key, value: nil)
        end
      end

      octx
    end

    def override(prop, value)
      copy_with(sheet: sheet.with(prop, value))
    end

    def consume_some?(*props)
      subctx = self
      values = props.map do |prop|
        value = subctx.sheet[prop]?
        subctx = subctx.override(prop, value: nil)
        value
      end

      return if values.none?

      {subctx, *values}
    end

    def consume_all?(*props)
      subctx = self
      values = props.map do |prop|
        return unless value = subctx.sheet[prop]?
        subctx = subctx.override(prop, value: nil)
        value
      end

      return if values.none?

      {subctx, *values}
    end

    def consume(*props)
      subctx = self
      values = props.map do |prop|
        value = subctx.sheet[prop]?
        subctx = subctx.override(prop, value: nil)
        value
      end

      {subctx, *values}
    end
  end

  # Sits at the beginning/in the middle of a hierarchy, has a subbox.
  module NodeBox
    abstract def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
  end

  # Sits at the bottom of a hierarchy, does not have a subbox.
  module LeafBox
    abstract def call(ctx : UnitContext, subject : Term) : {UnitContext, Term}
  end

  # Sits at the edge between the bottom of one hierarchy and the beginning
  # of another, nested one (or some other kind of content; in fact, it is
  # precisely the purpose of `BoxEdges` to determine whether to e.g. recurse
  # or handle content otherwise).
  module BoxEdge
    abstract def call(ctx : UnitContext, child : Term) : {UnitContext, Term}
  end

  struct FloatingBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:floating)
        return subbox.call(ctx, subject)
      end

      ictx, floating = response

      if floating == Term[false]
        return subbox.call(ictx, subject)
      end

      octx, inner = subbox.call(ictx.copy_with(nested: true), subject)

      {octx, Term.of(:floating, inner, w: :content, h: :content)}
    end
  end

  # Appears if the z-index (`z`) prop is present.
  struct LayerBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:z)
        return subbox.call(ctx, subject)
      end

      ictx, z = response
      octx, inner = subbox.call(ictx.copy_with(nested: true), subject)

      {octx, Term.of(:layer, inner, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?, "z-index": z)}
    end
  end

  struct MarginBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:ml, :mr, :mt, :mb)
        return subbox.call(ctx, subject)
      end

      ictx, ml, mr, mt, mb = response
      octx, inner = subbox.call(ictx.copy_with(nested: true), subject)

      if mt || mb
        inner = Term.of(:"y-padding", inner, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?, pt: mt, pb: mb)
      end

      if ml || mr
        inner = Term.of(:"x-padding", inner, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?, pl: ml, pr: mr)
      end

      {octx, inner}
    end
  end

  # Appears if the min-height (`min-h`) prop is present.
  struct MinHeightBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:"min-h")
        return subbox.call(ctx, subject)
      end

      ictx, minh = response
      octx, inner = subbox.call(ictx.override(:h, :max).copy_with(nested: true), subject)

      {octx, Term.of(:"y-expand", inner, "min-h": minh, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?)}
    end
  end

  # Appears if the min-width (`min-w`) prop is present.
  struct MinWidthBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:"min-w")
        return subbox.call(ctx, subject)
      end

      ictx, minw = response
      octx, inner = subbox.call(ictx.override(:w, :max).copy_with(nested: true), subject)

      {octx, Term.of(:"x-expand", inner, "min-w": minw, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?)}
    end
  end

  # Appears if the border-width prop and border color prop are present.
  struct BorderBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_all?(:"border-width", :"border-color")
        return subbox.call(ctx, subject)
      end

      ictx, border_width, border_color = response
      octx, inner = subbox.call(ictx.copy_with(nested: true), subject)

      {octx, Term.of(:"z-stack",
        Term.of(:"rect/outline",
          w: :max,
          h: :max,
          bg: border_color,
          "border-width": border_width,
          "border-radius": ctx.sheet[:"border-radius"]?,
        ),
        Term.of(:"y-padding",
          Term.of(:"x-padding",
            inner,
            w: :max,
            h: :max,
            pl: border_width,
            pr: border_width,
          ),
          w: :max,
          h: :max,
          pt: border_width,
          pb: border_width,
        ),
        w: ctx.sheet[:w]?,
        h: ctx.sheet[:h]?,
      )}
    end
  end

  # Appears if the background prop (`bg`) is present. Handles the ring and opacity.
  struct BackgroundBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:bg)
        return subbox.call(ctx, subject)
      end

      ictx, bg = response
      ictx, rl, rr, rt, rb, opacity = ictx.consume(:"ring-l", :"ring-r", :"ring-t", :"ring-b", :opacity)
      octx, inner = subbox.call(ictx.copy_with(nested: true), subject)

      if {rl, rr, rt, rb}.any?
        r0 = 0
      end

      if opacity && (opacity = opacity.as_n?) && opacity.natural? && opacity <= Term[100]
        alpha = (opacity/100 * Term[255]).floor
      else
        alpha = Term[255]
      end

      {octx, Term.of(:"z-stack",
        Term.of(:rect,
          w: :max,
          h: :max,
          bg: bg,
          alpha: alpha,
          "border-radius": ctx.sheet[:"border-radius"]?,
          rl: rl || r0,
          rr: rr || r0,
          rt: rt || r0,
          rb: rb || r0,
        ),
        inner,
        w: ctx.sheet[:w]?,
        h: ctx.sheet[:h]?,
      )}
    end
  end

  struct PaddingBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:pl, :pr, :pt, :pb)
        return subbox.call(ctx, subject)
      end

      ictx, pl, pr, pt, pb = response
      octx, inner = subbox.call(ictx.copy_with(nested: true), subject)

      if pt || pb
        inner = Term.of(:"y-padding", inner, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?, pt: pt, pb: pb)
      end

      if pl || pr
        inner = Term.of(:"x-padding", inner, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?, pl: pl, pr: pr)
      end

      {octx, inner}
    end
  end

  struct AlignYBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:"align-y")
        return subbox.call(ctx, subject)
      end

      ictx, point = response
      octx, inner = subbox.call(ictx.override(:h, :content).copy_with(nested: true), subject)

      {octx, Term.of(:"y-align", inner, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?, to: point)}
    end
  end

  struct AlignXBox
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      unless response = ctx.consume_some?(:"align-x")
        return subbox.call(ctx, subject)
      end

      ictx, point = response
      octx, inner = subbox.call(ictx.override(:w, :content).copy_with(nested: true), subject)

      {octx, Term.of(:"x-align", inner, w: ctx.sheet[:w]?, h: ctx.sheet[:h]?, to: point)}
    end
  end

  # Appears if there are more than one children. Handles `flow-row`/`flow-col`
  # prop as well as the `gap` prop.
  struct FlowBox
    include LeafBox

    def initialize(@edge : BoxEdge)
    end

    def call(ctx : UnitContext, subject : Term) : {UnitContext, Term}
      ictx, flow, gap, fractions = ctx.consume(:flow, :gap, :fractions)

      unless children = subject.as_itemsonly_d?
        return @edge.call(ictx, subject)
      end

      w = ctx.sheet[:w]?
      h = ctx.sheet[:h]?

      if children.size == 1
        octx, inner = @edge.call(ictx, children[0])

        # Assume somebody served w/h if nested; otherwise collapse only if allowed
        # and w/h are the same as child's (i.e. collapse won't break sizing).
        if ctx.nested || {inner[:w]?, inner[:h]?, ctx.collapse} == {w, h, true}
          return octx, inner
        end

        return octx, Term.of(:box, inner, w: w, h: h)
      end

      coctxs = [] of UnitContext # child octxs

      flow = Term::Dict.build do |commit|
        commit.with(:gap, gap)
        commit.with(:fractions, fractions)
        commit.with(:w, w)
        commit.with(:h, h)

        case flow
        when Term.of(:none)
          commit << Term.of(:"z-stack")
          commit.concat(children.items) do |child|
            coctx, uir = @edge.call(ictx, child)
            coctxs << coctx
            uir
          end
        when Term.of(:col)
          commit << Term.of(:"y-stack")
          commit.concat(children.items) do |child|
            coctx, uir = @edge.call(ictx.override(:h, :content), child)
            coctxs << coctx
            uir
          end
        else
          # In case of row as well as anything else (e.g. absence) we use row
          # flow (x-stack).
          commit << Term.of(:"x-stack")
          commit.concat(children.items) do |child|
            coctx, uir = @edge.call(ictx.override(:w, :content), child)
            coctxs << coctx
            uir
          end
        end
      end

      {UnitContext.octx(ictx, coctxs), Term.of(flow)}
    end
  end

  # Produces implicit text nodes from strings, normalizes non-dict terms to
  # strings and so on likewise; and recursively calls `uir` on dict terms.
  #
  # See also: `BoxEdge`.
  struct FlowEdge
    include BoxEdge

    # String and other non-dict children of units are displayed as text.
    private def text(ctx : UnitContext, caption : Term::Str)
      octx, font, weight, size, leading, color = ctx.consume(:font, :"font-weight", :"text-size", :leading, :"text-color")

      {octx, Term.of(:text, caption,
        w: ctx.sheet[:w]?,
        h: ctx.sheet[:h]?,
        font: font,
        weight: weight,
        size: size,
        leading: leading,
        color: color,
      )}
    end

    # Handles the unit-unit boundary.
    private def unit(ctx : UnitContext, unit : Term)
      octx, font, weight, color = ctx.consume(:font, :"font-weight", :"text-color")

      {octx, Microfold.uir(ctx.spec, unit, rem: ctx.rem, inherited: Term[font: font, "font-weight": weight, "text-color": color])}
    end

    def call(ctx : UnitContext, child : Term) : {UnitContext, Term}
      Term.case(child) do
        matchpi %{_dict} { unit(ctx, child) }
        matchpi %{_string} { text(ctx, child.unsafe_as_s) }
        otherwise { text(ctx, Term[child.inspect]) }
      end
    end
  end

  struct Toplevel
    include NodeBox

    def call(ctx : UnitContext, subject : Term, subbox : Hierarchy) : {UnitContext, Term}
      ictx, cursor, maxw, maxh, fr, dl, dt = ctx.consume(:cursor, :"max-w", :"max-h", :fr, :dl, :dt)

      octx, inner = subbox.call(ictx, subject)
      inner = inner.morph({:fr, fr}, {:cursor, cursor}, {:"max-w", maxw}, {:"max-h", maxh}, {:dl, dl}, {:dt, dt})

      {octx.override(:w, nil).override(:h, nil), Term.of(inner)}
    end
  end

  struct Itself
    include LeafBox

    def call(ctx : UnitContext, subject : Term) : {UnitContext, Term}
      # Consume entire sheet.
      {ctx.copy_with(sheet: Term[]), Term.of(subject | ctx.sheet)}
    end
  end

  # Represents a hierarchy of boxes.
  #
  # See also: `NodeBox`, `LeafBox`, `BoxEdge`.
  struct Hierarchy
    def initialize(@members : Slice(NodeBox), @leaf : LeafBox)
    end

    # Calls the head of the hierarchy and so on with *subject*.
    def call(ctx : UnitContext, subject : Term) : {UnitContext, Term}
      unless head = @members[0]?
        return @leaf.call(ctx, subject)
      end

      head.call(ctx, subject, Hierarchy.new(@members[1..], @leaf))
    end
  end

  HIERARCHY_NORMAL = Hierarchy.new(
    members: Slice(NodeBox).with(
      Toplevel.new,
      FloatingBox.new,
      LayerBox.new,
      MarginBox.new,
      MinHeightBox.new,
      MinWidthBox.new,
      BorderBox.new,
      BackgroundBox.new,
      PaddingBox.new,
      AlignYBox.new,
      AlignXBox.new,
    ),
    leaf: FlowBox.new(FlowEdge.new),
  )

  HIERARCHY_SELF = Hierarchy.new(
    members: Slice(NodeBox).with(
      Toplevel.new,
      FloatingBox.new,
      LayerBox.new,
      MarginBox.new,
      MinHeightBox.new,
      MinWidthBox.new,
      PaddingBox.new,
    ),
    leaf: Itself.new,
  )

  # TODO: remove this in favor of a centralized observer "file manager".
  # So that we have "hot reload" of the spec.
  SPEC = ML.terms(File.read(RESOURCES / "ufold.spec.wwml")).as_d

  private def transplant?(attr : Term) : Bool
    return true unless attr = attr.as_sym?

    !attr.to(String).prefixed_by?('.')
  end

  private def transplant(box : Term::Dict, sheet : Term::Dict, attrs : Term::Dict) : Term::Dict
    box.transaction do |commit|
      sheet.each_entry do |key, value|
        commit.with(key, value)
      end

      attrs.each_entry do |key, value|
        next unless transplant?(key)

        commit.with(key, value)
      end
    end
  end

  def uir0(spec : Term::Dict, unit : Term, rem : Term::Num, inherited : Term::Dict) : Term
    Term.case(unit) do
      matchpi %{((self node_symbol) ¦ attrs_ style⋮ "")} do
        # Read node defaults.
        nodal = Term[]
        if (defaults = spec[:defaults, node]?) && (defaults = defaults.as_s?)
          nodal = sheet(spec, attrs.unsafe_as_d, defaults.to(String), rem: rem)
        end

        sheet = sheet(spec, attrs.unsafe_as_d, style.to(String), rem: rem, base: nodal | inherited)
        ctx, box = HIERARCHY_SELF.call(UnitContext.new(spec, sheet, rem, collapse: true, nested: false), Term.of({node}))

        Term.of(transplant(box.as_d, ctx.sheet, attrs.unsafe_as_d))
      end

      matchpi %{((self node_symbol) child_ ¦ attrs_ style⋮ "")} do
        # Read node defaults.
        nodal = Term[]
        if (defaults = spec[:defaults, node]?) && (defaults = defaults.as_s?)
          nodal = sheet(spec, attrs.unsafe_as_d, defaults.to(String), rem: rem)
        end

        sheet = sheet(spec, attrs.unsafe_as_d, style.to(String), rem: rem, base: nodal | inherited)
        inner = uir(spec, child, rem: rem, inherited: inherited)
        ctx, box = HIERARCHY_SELF.call(UnitContext.new(spec, sheet, rem, collapse: attrs.empty?, nested: false), Term.of(node, inner))

        Term.of(transplant(box.as_d, ctx.sheet, attrs.unsafe_as_d))
      end

      matchpi %{(node_symbol children_+ ¦ attrs_ style⋮ "")} do
        # Read node defaults.
        nodal = Term[]
        if (defaults = spec[:defaults, node]?) && (defaults = defaults.as_s?)
          nodal = sheet(spec, attrs.unsafe_as_d, defaults.to(String), rem: rem)
        end

        sheet = sheet(spec, attrs.unsafe_as_d, style.to(String), rem: rem, base: nodal | inherited)
        ctx, box = HIERARCHY_NORMAL.call(UnitContext.new(spec, sheet, rem, collapse: attrs.empty?, nested: false), children)

        Term.of(transplant(box.as_d, ctx.sheet, attrs.unsafe_as_d))
      end

      matchpi %{((self) node_)} do
        node
      end

      otherwise do
        # TODO: pretty print inline
        Term.of(:invalid, unit.inspect)
      end
    end
  end

  UIR_CACHE = SyncCache({Term::Dict, Term, Term::Num, Term::Dict}, Term).new(2048, preallocate: true)

  # Returns the UIR tree corresponding to *unit*.
  #
  # *Units* are represented as a series of nested boxes. Each box is instantiated
  # on demand to consume certain style properties if those properties are present.
  # For example, a padding box is instantiated if padding properties are present.
  # Units can be nested.
  #
  # ```
  # node = ML.term <<-WWML
  # (p "Hello World" style: "p-3 text-neutral-200 bg-neutral-900")
  # WWML
  #
  # Microfold.uir(Microfold::SPEC, node) # => UIR...
  # ```
  def uir(spec : Term::Dict, unit : Term, *, rem = Term[16], inherited = Term[]) : Term
    UIR_CACHE.fetch({spec, unit, rem, inherited}) do
      uir0(spec, unit, rem, inherited)
    end
  end
end

# stuff = ML.term <<-WWML
# (button "Hello World")
# WWML

# puts ML.display(Microfold.uir(Microfold::SPEC, stuff))
