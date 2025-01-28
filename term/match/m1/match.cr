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
end
