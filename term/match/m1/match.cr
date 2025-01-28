module Ww::M1::Operator
  def match(behind0, op : Pass, matchee : Term, ahead0)
    ahead0.call(behind0)
  end
end
