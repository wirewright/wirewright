module Ww::M1::Operator
  def match(behind0, op : Pass, matchee : Term, ahead0)
    Ahead.tr(behind0, ahead0)
  end

  private def compare?(a, op, b)
    case op
    when :lt  then a < b
    when :lte then a <= b
    else
      unimplemented
    end
  end

  def match(behind0, op : Num, matchee : Term, ahead0)
    unless n = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    if op.options.whole? && !n.whole?
      return Fb::Mismatch.new(behind0.env)
    end

    min = op.min
    max = op.max

    if min && !compare?(min, op.options.min_excluded? ? :lt : :lte, n)
      return Fb::Mismatch.new(behind0.env)
    end

    if max && !compare?(n, op.options.max_excluded? ? :lt : :lte, max)
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  {% for opcls, type in { Str => :string, Sym => :symbol, Boolean => :boolean, Dict => :dict } %}
    def match(behind0, op : {{opcls}}, matchee : Term, ahead0)
      unless matchee.type.{{type.id}}?
        return Fb::Mismatch.new(behind0.env)
      end

      Ahead.tr(behind0, ahead0)
    end
  {% end %}

  def match(behind0, op : Itemsonly, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.itemsonly?
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : Pairsonly, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.pairsonly?
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : SketchSubset, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.sketch_superset_of?(op.sketch)
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, matchee, ahead0)
  end

  def match(behind0, op : Bounds, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.size.in?(op.min..op.max)
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : BoundsGuard, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.size.in?(op.min..op.max)
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, matchee, ahead0)
  end

  def match(behind0, op : MaxDepth, matchee : Term, ahead0)
    # FIXME: currently we're unable to use #max of MaxDepth, since Dict#maxdepth is maximum-ever
    # depth rather than current maximum depth.
    unless (dict = matchee.as_d?) && dict.maxdepth.in?(op.min..)
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, matchee, ahead0)
  end

  def match(behind0, op : DictGuard, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    unless dict.sketch_superset_of?(op.sketch)
      return Fb::Mismatch.new(behind0.env)
    end

    unless dict.size.in?(op.bounds[0]..op.bounds[1])
      return Fb::Mismatch.new(behind0.env)
    end

    # FIXME: currently we're unable to use op.depth[1], since Dict#maxdepth is maximum-ever
    # depth rather than current maximum depth.
    unless dict.maxdepth.in?(op.depth[0]..)
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, matchee, ahead0)
  end

  def match(behind0, op : Literal, matchee : Term, ahead0)
    unless matchee == op.term
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : Capture, matchee : Term, ahead0)
    unless behind1 = behind0.propose?(op.capture, matchee)
      return Fb::Mismatch.new(behind0.env.with(op.capture, matchee))
    end

    behind1 = behind1.mount(op.capture)

    match(behind1, op.successor, matchee, ahead0)
  end

  def match(behind0, op : LiteralChoices, matchee : Term, ahead0)
    unless matchee.in?(op.choices)
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : Edge, matchee : Term, ahead0)
    case op.type
    when .any?
      valid = ML.edge?(matchee)
    when .number?, .string?, .symbol?
      valid = ML.edge?(matchee, allowed: {op.type})
    else
      raise ArgumentError.new("unexpected edge type after compilation: expected Any, Number, String, or Symbol")
    end

    unless valid
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : ItemFirst, matchee : Term, ahead0)
    unless (dict = matchee.as_itemsonly_d?) && dict.itemsize > 0
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.keypath?, Ahead.stackptr(ahead0))

    match(behind0.value(key: 0), op.successor, dict[0], ahead1)
  end

  def match(behind0, op : ItemLast, matchee : Term, ahead0)
    unless (dict = matchee.as_itemsonly_d?) && dict.itemsize > 0
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.keypath?, Ahead.stackptr(ahead0))

    match(behind0.value(key: dict.hi), op.successor, dict[dict.hi], ahead1)
  end

  def match(behind0, op : SingularSeq, matchee : Term, ahead0)
    unless (dict = matchee.as_itemsonly_d?) && dict.itemsize >= op.items.size
      return Fb::Mismatch.new(behind0.env)
    end

    if op.exhaustive && dict.itemsize != op.items.size
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.keypath?, Ahead.stackptr(ahead0))

    if op.reverse
      i, j, delta = op.items.size - 1, dict.itemsize - 1, -1i8
    else
      i, j, delta = 0, 0, +1i8
    end

    ahead2 = Ahead::ItemZip.new(op.items, dict.items, i, j, delta, Ahead.stackptr(ahead1))

    Ahead.tr(behind0.value(key: j), ahead2)
  end

  def match(behind0, op : ItemSeq, matchee : Term, ahead0)
    unless dict = matchee.as_itemsonly_d?
      return Fb::Mismatch.new(behind0.env)
    end

    Item.match(behind0, op.items, dict.items, ahead0)
  end

  def match(behind0, op : SourceChoice, matchee : Term, ahead0)
    a = match(behind0, op.a, matchee, ahead0)
    unless a.is_a?(Fb::Response)
      return a
    end

    b = match(behind0, op.b, matchee, ahead0)
    unless b.is_a?(Fb::Response)
      return b
    end

    Fb.sum(a, b)
  end

  def match(behind0, op : Both, matchee : Term, ahead0)
    ahead1 = Ahead::Match.new(op.b, matchee, Ahead.stackptr(ahead0))

    match(behind0, op.a, matchee, ahead1)
  end

  def match(behind0, op : Keypool, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    pruned = op.keys.reduce(dict) { |memo, key| memo.without(key) }
    unless pruned.empty?
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : Not, matchee : Term, ahead0)
    if matchee.in?(op.blacklist)
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : Span, matchee : Term, ahead0)
    unless a = matchee.as_s?
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, Term.of(a.charcount), ahead0)
  end

  def match(behind0, op : Tally, matchee : Term, ahead0)
    unless a = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, Term.of(a.size), ahead0)
  end

  def match(behind0, op : Type, matchee : Term, ahead0)
    match(behind0, op.successor, Term.of(matchee.type.blank), ahead0)
  end

  def match(behind0, op : Add, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, Term.of(a + op.arg), ahead0)
  end

  def match(behind0, op : Sub, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, Term.of(a - op.arg), ahead0)
  end

  def match(behind0, op : Mul, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, Term.of(a * op.arg), ahead0)
  end

  def match(behind0, op : Div, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    begin
      q = Term.of(a / op.arg)
    rescue DivisionByZeroError
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, q, ahead0)
  end

  def match(behind0, op : Idiv, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    begin
      q = Term.of(a // op.arg)
    rescue DivisionByZeroError
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, q, ahead0)
  end

  def match(behind0, op : Mod, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    begin
      m = Term.of(a % op.arg)
    rescue DivisionByZeroError
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, m, ahead0)
  end

  def match(behind0, op : Pow, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    begin
      c = Term.of(a ** op.arg)
    rescue DivisionByZeroError # e.g. 0^-2
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, c, ahead0)
  end

  def match(behind0, op : Map, matchee : Term, ahead0)
    unless v = op.arg[matchee]?
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, v, ahead0)
  end

  module Entry
    extend self

    def match(behind0, op : Required, matchee : Term, ahead0)
      unless dict = matchee.as_d?
        return Fb::Mismatch.new(behind0.env)
      end

      unless v = dict[op.key]?
        return Fb::Mismatch.new(behind0.env)
      end

      ahead1 = Ahead::Goto.new(behind0.keypath?, Ahead.stackptr(ahead0))

      Operator.match(behind0.value(key: op.key), op.value, v, ahead1)
    end

    def match(behind0, op : Optional, matchee : Term, ahead0)
      unless dict = matchee.as_d?
        return Fb::Mismatch.new(behind0.env)
      end

      ahead1 = Ahead::Goto.new(behind0.keypath?, Ahead.stackptr(ahead0))

      if value = dict[op.key]?
        behind1 = behind0.value(key: op.key)

        case fb = Operator.match(behind1, op.value, value, ahead1)
        in Fb::Match, Fb::Interrupt
          return fb
        in Fb::Mismatch
        end
      end

      behind1 = behind0.keypath(&.create_pair(op.key, value: op.default))

      Operator.match(behind1, op.value, op.default, ahead1)
    end

    def match(behind0, op : Absent | AbsentKeypath, matchee : Term, ahead0)
      unless dict = matchee.as_d?
        return Fb::Mismatch.new(behind0.env)
      end

      if op.key.in?(dict)
        return Fb::Mismatch.new(behind0.env)
      end

      case op
      in Absent
        behind1 = behind0
      in AbsentKeypath
        behind1 = behind0.mount(op.name, &.create_pair(op.key))
      end

      Ahead.tr(behind1, ahead0)
    end

    def match(behind0, op : Negative | NegativeKeypath, matchee : Term, ahead0)
      unless dict = matchee.as_d?
        return Fb::Mismatch.new(behind0.env)
      end

      if v = dict[op.key]?
        case fb = Operator.match(behind0, op.positive, v, ahead0)
        in Fb::Match # Positive example matches, nothing to do.
          return Fb::Mismatch.new(behind0.env)
        in Fb::Mismatch
        in Fb::Interrupt
          return fb
        end
      end

      case op
      in Negative
        behind1 = behind0
      in NegativeKeypath
        behind1 = behind0.mount(op.name, &.create_pair(op.key))
      end

      Ahead.tr(behind1, ahead0)
    end
  end
end
