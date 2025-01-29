module Ww::M1::Operator
  def match(behind0, op : Pass, matchee : Term, ahead0)
    Ahead.tr(behind0, ahead0)
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
end
