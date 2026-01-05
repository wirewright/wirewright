module Ww::M1::Operator
  def match(behind0, op : Pass, matchee : Term, ahead0)
    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : Never, matchee : Term, ahead0)
    Fb::Mismatch.new(behind0.env)
  end

  private def compare?(a : Term::Num, op, b : Term::Num)
    case op
    when :lt  then a < b
    when :lte then a <= b
    else
      unimplemented
    end
  end

  private def compare?(a, op, b)
    raise "not implemented"
  end

  def match(behind0, op : Num, matchee : Term, ahead0)
    unless n = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    if op.spec.whole? && !n.integer?
      return Fb::Mismatch.new(behind0.env)
    end

    if (min = op.min) && !compare?(min, op.spec.min_excluded? ? :lt : :lte, n)
      return Fb::Mismatch.new(behind0.env)
    end

    if (max = op.max) && !compare?(n, op.spec.max_excluded? ? :lt : :lte, max)
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  {% for opcls, type in {Str => :string, Sym => :symbol, Boolean => :boolean, Dict => :dict} %}
    def match(behind0, op : {{opcls}}, matchee : Term, ahead0)
      unless matchee.type.{{type.id}}?
        return Fb::Mismatch.new(behind0.env)
      end

      Ahead.tr(behind0, ahead0)
    end
  {% end %}

  def match(behind0, op : SymBlank, matchee : Term, ahead0)
    unless (symbol = matchee.as_sym?) && (blank = symbol.blank?) && (name = blank.name?)
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))
    ahead2 = Ahead::Match.new(op.type, Term.of(blank.type.blank), Ahead.stackptr(ahead1))

    match(behind0.backpathless, op.name, Term.of(name), ahead2)
  end

  def match(behind0, op : SymNonblank, matchee : Term, ahead0)
    unless symbol = matchee.as_sym?
      return Fb::Mismatch.new(behind0.env)
    end

    if symbol.blank?
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

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

  def match(behind0, op : LiteralWhitelist, matchee : Term, ahead0)
    unless matchee.in?(op.whitelist)
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

  def match(behind0, op : Edge, matchee : Term, ahead0)
    case op.type
    when .any?
      valid = ML.edge?(matchee)
    else
      valid = ML.edge?(matchee, type: op.type)
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

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))

    match(behind0.value(key: 0), op.successor, dict[0], ahead1)
  end

  def match(behind0, op : ItemLast, matchee : Term, ahead0)
    unless (dict = matchee.as_itemsonly_d?) && dict.itemsize > 0
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))

    match(behind0.value(key: dict.hi), op.successor, dict[dict.hi], ahead1)
  end

  def match(behind0, op : SingularSeq, matchee : Term, ahead0)
    unless (dict = matchee.as_itemsonly_d?) && dict.itemsize >= op.items.size
      return Fb::Mismatch.new(behind0.env)
    end

    if op.exhaustive && dict.itemsize != op.items.size
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))

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

  def match(behind0, op : ChoiceSource, matchee : Term, ahead0)
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

  def match(behind0, op : Keytest, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    # Match if any key is in dict.
    op.keys.each do |key|
      if key.in?(dict)
        return Ahead.tr(behind0, ahead0)
      end
    end

    Fb::Mismatch.new(behind0.env)
  end

  {% for cfg in { {"Keypool", true}, {"NegativeKeypool", false} } %}
    {% cls, empty = cfg %}

    def match(behind0, op : {{cls.id}}, matchee : Term, ahead0)
      unless dict = matchee.as_d?
        return Fb::Mismatch.new(behind0.env)
      end

      pruned = op.keys.reduce(dict) { |memo, key| memo.without(key) }

      if pruned.empty? == {{empty}}
        Ahead.tr(behind0, ahead0)
      else
        Fb::Mismatch.new(behind0.env)
      end
    end
  {% end %}

  def match(behind0, op : LiteralBlacklist, matchee : Term, ahead0)
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

  def match(behind0, op : ParseML, matchee : Term, ahead0)
    unless string = matchee.as_s?
      return Fb::Mismatch.new(behind0.env)
    end

    begin
      result = ML.term(string.to(String))
    rescue ML::SyntaxError
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, result, ahead0)
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

  def match(behind0, op : Clamp, matchee : Term, ahead0)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(behind0.env)
    end

    a = Math.min(Math.max(a, op.min), op.max)

    match(behind0, op.successor, Term.of(a), ahead0)
  end

  def match(behind0, op : Map, matchee : Term, ahead0)
    unless v = op.arg[matchee]?
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, v, ahead0)
  end

  def match(behind0, op : Entry::Required, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    unless v = dict[op.key]?
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))

    match(behind0.value(key: op.key), op.value, v, ahead1)
  end

  def match(behind0, op : Entry::Present, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    unless v = dict[op.key]?
      return Fb::Mismatch.new(behind0.env)
    end

    unless v.type.subtype?(op.type)
      return Fb::Mismatch.new(behind0.env)
    end

    Ahead.tr(behind0, ahead0)
  end

  def match(behind0, op : Entry::Optional, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))

    if value = dict[op.key]?
      behind1 = behind0.value(key: op.key)

      # NOTE: Itemspart optional has different failure semantics vs. pairspart
      # optional. In itemspart optional, if the body fails to match the item
      # underneath, the default is tried. In pairspart optional, however,
      # we fail to match rather than trying default. In pairspart optional,
      # only absence counts toward default.
      return match(behind1, op.value, value, ahead1)
    end

    behind1 = behind0.backpath(&.create_pair(op.key, value: op.default))

    match(behind1, op.value, op.default, ahead1)
  end

  def match(behind0, op : Entry::Absent | Entry::AbsentKeypath, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    if op.key.in?(dict)
      return Fb::Mismatch.new(behind0.env)
    end

    case op
    in Entry::Absent
      behind1 = behind0
    in Entry::AbsentKeypath
      behind1 = behind0.mount(op.name, &.create_pair(op.key))
    end

    Ahead.tr(behind1, ahead0)
  end

  def match(behind0, op : Entry::Negative | Entry::NegativeKeypath, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    if v = dict[op.key]?
      case fb = match(behind0, op.barrier, v, ahead0)
      in Fb::Match # Barrier matches, nothing to do.
        return Fb::Mismatch.new(behind0.env)
      in Fb::Mismatch
      in Fb::Interrupt
        return fb
      end
    end

    case op
    in Entry::Negative
      behind1 = behind0
    in Entry::NegativeKeypath
      behind1 = behind0.mount(op.name, &.create_pair(op.key))
    end

    Ahead.tr(behind1, ahead0)
  end

  def match(behind0, op : CaptureItemsonly, matchee : Term, ahead0)
    unless dict = matchee.as_itemsonly_d?
      return Fb::Mismatch.new(behind0.env)
    end

    unless behind1 = behind0.propose?(op.capture, matchee)
      return Fb::Mismatch.new(behind0.env.with(op.capture, matchee))
    end

    behind1 = behind1.mount(op.capture, &.update_value(0).span(dict.size))

    Ahead.tr(behind1, ahead0)
  end

  def match(behind0, op : Partition, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    itemspart, pairspart = dict.partition

    ahead1 = Ahead::Match.new(op.pairside, Term.of(pairspart), Ahead.stackptr(ahead0))

    match(behind0, op.itemside, Term.of(itemspart), ahead1)
  end
end
