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

  INSTANCE_SYM     = Sym.new
  INSTANCE_STR     = Str.new
  INSTANCE_BOOLEAN = Boolean.new
  INSTANCE_DICT    = Dict.new

  defcase Sym

  defcase SymBlank, name : Any, type : Any
  defcase SymNonblank

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

  defcase Span, successor : Any
  defcase Tally, successor : Any
  defcase Type, successor : Any
  defcase ParseML, successor : Any

  defcase Add, arg : Term::Num, successor : Any
  defcase Sub, arg : Term::Num, successor : Any
  defcase Mul, arg : Term::Num, successor : Any
  defcase Div, arg : Term::Num, successor : Any
  defcase Idiv, arg : Term::Num, successor : Any
  defcase Mod, arg : Term::Num, successor : Any
  defcase Pow, arg : Term::Num, successor : Any
  defcase Map, arg : Term::Dict, successor : Any

  defcase CaptureItemsonly, capture : Term

  defcase Partition, itemspart : Any, pairspart : Any

  module Entry
    alias Any = Required | Optional | Present | Absent | AbsentKeypath | Negative | NegativeKeypath

    # Entries are assigned an eyeballed "cost". Cheaper entries are checked
    # first by e.g. `%layer`.
    enum Cost : UInt8
      VeryCheap
      Cheap
      Moderate
      Expensive
    end

    record Required, key : Term, value : Operator::Any do
      def cost : Cost
        Cost::Moderate
      end
    end

    record Optional, key : Term, default : Term, value : Operator::Any do
      def cost : Cost
        Cost::Expensive
      end
    end

    record Present, key : Term, type : TermType do
      def cost : Cost
        Cost::Cheap
      end
    end

    record Absent, key : Term do
      def cost : Cost
        Cost::VeryCheap
      end
    end

    record AbsentKeypath, key : Term, name : Term do
      def cost : Cost
        Cost::Expensive
      end
    end

    record Negative, key : Term, positive : Operator::Any do
      def cost : Cost
        Cost::Moderate
      end
    end

    record NegativeKeypath, key : Term, positive : Operator::Any, name : Term do
      def cost : Cost
        Cost::Expensive
      end
    end
  end
end
