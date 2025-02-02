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

  INSTANCE_SYM = Sym.new
  INSTANCE_STR = Str.new
  INSTANCE_BOOLEAN = Boolean.new
  INSTANCE_DICT = Dict.new

  defcase Sym
  defcase Str
  defcase Boolean
  defcase Dict

  defcase Itemsonly
  defcase Pairsonly

  defcase SketchSubset, sketch : Term::Dict::Sketch, successor : Any

  defcase Bounds, min : Magnitude, max : Magnitude
  defcase BoundsGuard, min : Magnitude, max : Magnitude, successor : Any

  defcase MaxDepth, min : Magnitude, max : Magnitude, successor : Any

  defcase DictGuard,
    sketch : Term::Dict::Sketch,
    bounds : {Magnitude, Magnitude},
    depth : {Magnitude, Magnitude},
    successor : Any

  defcase Literal, term : Term
  defcase LiteralChoices, choices : Set(Term)

  defcase Capture, capture : Term, successor : Any

  defcase Edge, type : TermType

  defcase ItemSeq, items : Slice(Item::Any)
  defcase ItemFirst, successor : Any
  defcase ItemLast, successor : Any
  defcase SingularSeq, items : Slice(Any), exhaustive : Bool, reverse : Bool

  defcase SourceChoice, a : Any, b : Any

  defcase Both, a : Any, b : Any

  defcase Keypool, keys : Slice(Term)

  defcase Not, blacklist : Term::Dict
end
