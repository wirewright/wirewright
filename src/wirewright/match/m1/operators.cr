module Ww::M1::Operator
  INSTANCE_PASS = Pass.new

  defcase Pass

  INSTANCE_NEVER = Never.new

  defcase Never

  INSTANCE_NUM       = Num.new(min: nil, max: nil, spec: :none)
  INSTANCE_NUM_WHOLE = Num.new(min: nil, max: nil, spec: :whole)

  defcase Num, spec : Spec, min : Arg, max : Arg do
    # FIXME: Parsing should be defined in module M1 near normal()/operator(),
    # not here!!!

    def self.subject?(term : Term)
      Term.case(term, engine: M0) do
        matchpi %{(whole %'_)} do
          Spec::Whole
        end

        matchpi %{%'_} do
          Spec::None
        end

        otherwise { }
      end
    end

    def self.arg?(term : Term)
      Term.case(term, engine: M0) do
        matchpi %{(var name_)} do
          Var.new(name)
        end

        matchpi %{_number} do
          term.as_n
        end

        otherwise { }
      end
    end

    # top
    #   (%number <subject> <cmp> <arg>)
    #   (%number <arg> <ltx> <subject> <ltx> <arg>)
    #
    # subject
    #   (whole %'_)
    #   %'_
    #
    # arg
    #   (var _)
    #   _number
    #
    # <cmp>
    #   <ltx>
    #   <gtx>
    #
    # <ltx>
    #   <
    #   <=
    #
    # <gtx>
    #   >
    #   >=
    def self.parse?(term : Term) : Num?
      Term.case(term, engine: M0) do
        matchpi %{(%number subject_ cmp_ arg_)} do
          return unless spec = subject?(subject)
          return unless r = arg?(arg)

          case cmp
          when SYM_LT
            # _ < 100
            spec = spec.max_excluded
            max = r
          when SYM_LTE
            # _ <= 100
            max = r
          when SYM_GT
            # _ > 100
            spec = spec.min_excluded
            min = r
          when SYM_GTE
            min = r
          else
            return
          end

          new(spec, min, max)
        end

        matchpi %{(%number larg_ lop_ subject_ rop_ rarg_)} do
          return unless spec = subject?(subject)
          return unless min = arg?(larg)
          return unless max = arg?(rarg)

          case lop
          when SYM_LTE
          when SYM_LT
            spec = spec.min_excluded
          else
            return
          end

          case rop
          when SYM_LTE
          when SYM_LT
            spec = spec.max_excluded
          else
            return
          end

          new(spec, min, max)
        end

        otherwise { }
      end
    end

    alias Arg = Term::Num | Var | Nil

    defrecord Var, name : Term

    @[Flags]
    enum Spec : UInt32
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
