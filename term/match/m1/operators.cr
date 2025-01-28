module Ww::M1::Operator
  INSTANCE_PASS = Pass.new

  defcase Pass

  INSTANCE_NUM       = Num.new(min: nil, max: nil, options: :none)
  INSTANCE_NUM_WHOLE = Num.new(min: nil, max: nil, options: :whole)

  defcase Num, min : Term::Num?, max : Term::Num?, options : Options do
    @[Flags]
    enum Options : UInt8
      MinExcluded
      MaxExcluded
      Whole
    end

    def self.new(min, max, options : Tuple)
      new(min: min, max: max, options: Options.new(options))
    end
  end
end
