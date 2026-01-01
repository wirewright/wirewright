module Ww::M1::Operator
  INSTANCE_PASS = Pass.new

  defcase Pass

  INSTANCE_NEVER = Never.new

  defcase Never

  INSTANCE_NUM       = Num.new(min: Term[0], max: Term[0], spec: :none)
  INSTANCE_NUM_WHOLE = Num.new(min: Term[0], max: Term[0], spec: :whole)

  defcase Num, spec : Spec, min : Term::Num, max : Term::Num do
    @[Flags]
    enum Spec : UInt32
      MinPresent
      MaxPresent
      MinExcluded
      MaxExcluded
      Whole

      {% for member in @type.constants %}
        # Returns a copy of this spec with the `{{member}}` flag set.
        def {{member.underscore}} : Spec
          self | {{member}}
        end
      {% end %}
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
  defcase LiteralWhitelist, whitelist : Set(Term)
  defcase LiteralBlacklist, blacklist : Set(Term)

  defcase Capture, capture : Term, successor : Any

  defcase Edge, type : TermType

  # TODO: remove items!!!
  defcase ItemSeq, items : Slice(Item::Any)
  defcase ItemFirst, successor : Any
  defcase ItemLast, successor : Any
  defcase SingularSeq, items : Slice(Any), exhaustive : Bool, reverse : Bool

  defcase ChoiceSource, a : Any, b : Any

  defcase Both, a : Any, b : Any

  defcase Keytest, keys : Slice(Term)
  defcase Keypool, keys : Slice(Term)
  defcase NegativeKeypool, keys : Slice(Term)

  defcase ValueLiteral, key : Term, successor : Any

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
  defcase Clamp, min : Term::Num, max : Term::Num, successor : Any
  defcase Map, arg : Term::Dict, successor : Any

  defcase CaptureItemsonly, capture : Term

  defcase Partition, itemside : Any, pairside : Any

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

    defcase Required, key : Term, value : Operator::Any do
      def cost : Cost
        Cost::Moderate
      end
    end

    defcase Optional, key : Term, default : Term, value : Operator::Any do
      def cost : Cost
        Cost::Expensive
      end
    end

    defcase Present, key : Term, type : TermType do
      def cost : Cost
        Cost::Cheap
      end
    end

    defcase Absent, key : Term do
      def cost : Cost
        Cost::VeryCheap
      end
    end

    defcase AbsentKeypath, key : Term, name : Term do
      def cost : Cost
        Cost::Expensive
      end
    end

    defcase Negative, key : Term, barrier : Operator::Any do
      def cost : Cost
        Cost::Moderate
      end
    end

    defcase NegativeKeypath, key : Term, barrier : Operator::Any, name : Term do
      def cost : Cost
        Cost::Expensive
      end
    end
  end
end
