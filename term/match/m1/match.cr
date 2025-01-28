module Ww::M1::Operator
  def match(behind0, op : Pass, matchee : Term, ahead0)
    ahead0.call(behind0)
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

    ahead0.call(behind0)
  end

  {% for opcls, type in { Str => :string, Sym => :symbol, Boolean => :boolean, Dict => :dict } %}
    def match(behind0, op : {{opcls}}, matchee : Term, ahead0)
      unless matchee.type.{{type.id}}?
        return Fb::Mismatch.new(behind0.env)
      end

      ahead0.call(behind0)
    end
  {% end %}

  def match(behind0, op : Itemsonly, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.itemsonly?
      return Fb::Mismatch.new(behind0.env)
    end

    ahead0.call(behind0)
  end

  def match(behind0, op : Pairsonly, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.pairsonly?
      return Fb::Mismatch.new(behind0.env)
    end

    ahead0.call(behind0)
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

    ahead0.call(behind0)
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

    ahead0.call(behind0)
  end
end
