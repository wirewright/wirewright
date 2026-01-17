module Ww::M1next
  # Short for *operator*, this module holds data structures used to represent
  # compiled operators and associated groups/categories of operators (represented
  # using aliases).
  module Op
    alias Any = Pass | Never | Num | Sym | SymBlank | SymNonblank | Boolean | Dict | Itemsonly | Pairsonly | SketchSubset | Bounds | BoundsGuard | MaxDepth | DictGuard | Literal | Capture | CaptureItemsonly | ItemSeq | ItemFirst | ItemLast | SingularSeq | Partition | Edge | LiteralWhitelist | ChoiceSource | Keypool | Span | Tally | Type | ParseML | Clamp | Bin | Both | LiteralBlacklist | Layer | ScanFirst | ScanSource | ScanAll | DfsFirst | DfsSource | DfsAll | BfsFirst | BfsAll | Value | NegativeValue | NegativeValueKeypath | EntriesFirst | EntriesSource | EntriesAll | Str | KeypathCapture | NegativeKeypool | Keytest | ValueLiteral | Filter | Pluck | Flat | Split | Adjacent | Untracked
    alias Bin = Add | Sub | Mul | Div | Idiv | Mod | Pow | Map

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
            when M1::SYM_LT
              # _ < 100
              spec = spec.max_excluded
              max = r
            when M1::SYM_LTE
              # _ <= 100
              max = r
            when M1::SYM_GT
              # _ > 100
              spec = spec.min_excluded
              min = r
            when M1::SYM_GTE
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
            when M1::SYM_LTE
            when M1::SYM_LT
              spec = spec.min_excluded
            else
              return
            end

            case rop
            when M1::SYM_LTE
            when M1::SYM_LT
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
    defcase Untracked, successor : Any

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

    defcase Partition, itemside : Any, pairside : Any, seq : Bool do
      private def self.seq?(op : Any) : Bool
        case op
        when ItemFirst,
             ItemLast,
             CaptureItemsonly,
             SingularSeq,
             ItemSeq
          true
        when BoundsGuard,
             MaxDepth,
             SketchSubset,
             DictGuard
          seq?(op.successor)
        else
          false
        end
      end

      def self.new(itemside : Any, pairside : Any)
        new(itemside, pairside, seq: seq?(itemside))
      end
    end

    defcase Layer, below : Any, side : Slice(Entry::Any)

    alias First = ScanFirst | DfsFirst | BfsFirst | EntriesFirst | SplitFirst
    alias Source = DfsSource | ScanSource | EntriesSource | SplitSource
    alias All = ScanAll | DfsAll | BfsAll | EntriesAll | SplitAll

    alias Scan = ScanFirst | ScanSource | ScanAll

    defcase ScanFirst, needle : Slice(Any) do
      def seq
        needle
      end
    end

    defcase ScanSource, needle : Slice(Any) do
      def seq
        needle
      end
    end

    defcase ScanAll, successor : Any, needle : Slice(Any), selector : Set(Term), exterior : Set(Term), min : UInt8, max : UInt8 do
      def minM
        Magnitude.new(min)
      end

      def maxM
        max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
      end

      def seq
        needle
      end
    end

    defcase Value, capture : Term, tail : Any
    defcase NegativeValue, capture : Term
    defcase NegativeValueKeypath, capture : Term, name : Term

    alias Dfs = DfsFirst | DfsSource | DfsAll

    # TODO: It would be nice to unify Dfs and Bfs search under a single SearchFirst, SearchSource,
    # SearchAll. The compiler (M1.operator) must then equip them with Tzip::Algorithm's instead of
    # Tzip itself doing that at match-time.

    defcase DfsFirst, seq : Slice(Any), part : M1::Search::Part, depth0 : Bool do
      def needle
        seq.first
      end
    end

    defcase DfsSource, seq : Slice(Any), part : M1::Search::Part, depth0 : Bool do
      def needle
        seq.first
      end
    end

    defcase DfsAll, successor : Any, seq : Slice(Any), selector : Set(Term), exterior : Set(Term), part : M1::Search::Part, min : UInt8, max : UInt8, depth0 : Bool do
      def minM
        Magnitude.new(min)
      end

      def maxM
        max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
      end

      def needle
        seq.first
      end
    end

    alias Bfs = BfsFirst | BfsAll

    defcase BfsFirst, seq : Slice(Any), part : M1::Search::Part, depth0 : Bool do
      def needle
        seq.first
      end
    end

    defcase BfsAll, successor : Any, seq : Slice(Any), selector : Set(Term), exterior : Set(Term), part : M1::Search::Part, min : UInt8, max : UInt8, depth0 : Bool do
      def minM
        Magnitude.new(min)
      end

      def maxM
        max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
      end

      def needle
        seq.first
      end
    end

    alias Entries = EntriesFirst | EntriesSource | EntriesAll

    defcase EntriesFirst, kop : Any, vop : Any do
      def needle
        [kop, vop]
      end
    end

    defcase EntriesSource, kop : Any, vop : Any do
      def needle
        [kop, vop]
      end
    end

    defcase EntriesAll, successor : Any, kop : Any, vop : Any, exterior : Set(Term), selector : Set(Term), min : UInt8, max : UInt8 do
      def minM
        Magnitude.new(min)
      end

      def maxM
        max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
      end

      def needle
        [kop, vop]
      end
    end

    defcase KeypathCapture, capture : Term

    defcase Filter, deps : Pf::Set(Term), selector : Any, successor : Any, min : Magnitude, max : Magnitude
    defcase Pluck, spec : M1next::Tzip::PluckSpec, successor : Any
    defcase Flat, spec : M1next::Tzip::FlatSpec, successor : Any

    alias Split = SplitFirst | SplitSource | SplitAll

    defcase Adjacent, members : Slice(Any)

    defcase SplitFirst, lhs : Any, focus : Slice(Any), rhs : Any
    defcase SplitSource, lhs : Any, focus : Slice(Any), rhs : Any
    defcase SplitAll, lhs : Any, focus : Slice(Any), rhs : Any, successor : Any, min : Magnitude, max : Magnitude do
      def minM
        min
      end

      def maxM
        max
      end
    end
  end

  # Compiled sequence operators such as `(⏏_⏏ ⏏(%optional 0 x_)⏏ ⏏y_⏏)`.
  module Op::Item
    alias Any = Singular | Slot | Plural | Group | GapFirst | GapSource | Optional | Many | Past

    enum ExpandStrategy : UInt8
      Auto
      Sway
      Lazy
      Greedy
    end

    # wtf is it called TAIL?!?!?!
    defcase Singular, tail : Op::Any
    defcase Slot, name : Term

    defcase Plural, contenders : Slice(Term?), min1 : Magnitude, max1 : Magnitude, type : TermType, follower : Follower, frac : UInt32, strategy : ExpandStrategy do
      def self.new(contender : Term?, min1, max1, type : TermType, follower : Follower, frac, strategy : ExpandStrategy)
        new(Slice[contender.as(Term?)], min1, max1, type, follower, frac, strategy)
      end

      def min : Magnitude
        min1 * contenders.size
      end

      def max : Magnitude
        max1 * contenders.size
      end

      enum Follower : UInt8
        {% for member in ::Ww::TermType.constants %}
        {{member}}
      {% end %}

        # Indicates that the follower is absent.
        None

        def type : TermType
          if none?
            raise ArgumentError.new("cannot query .type of a missing follower")
          end

          TermType.new(value)
        end
      end
    end

    defcase Group, successor : Op::Any, children : Slice(Any) do
      def capture
        successor.as(Op::Capture).capture
      end
    end

    alias Gap = GapFirst | GapSource

    defcase GapFirst, measurer : Op::Any, frac : UInt32, strategy : ExpandStrategy
    defcase GapSource, measurer : Op::Any, frac : UInt32, strategy : ExpandStrategy

    defcase Optional, default : Term, body : Op::Any

    defcase Many, successor : Op::Any, children : Slice(Any), interior : Set(Term), min : UInt8, max : UInt8 do
      # FIXME: Use magnitude instead of U8 in the first place!!!!!

      def capture
        successor.as(Op::Capture).capture
      end

      def minM
        Magnitude.new(min)
      end

      def maxM
        max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
      end
    end

    alias Past = PastGreedy | PastLazy

    defcase PastGreedy, children : Slice(Any), min : UInt8, max : UInt8, n : Int32 = 0 do
      # FIXME: Use magnitude instead of U8 in the first place!!!!!

      def minM
        Magnitude.new(min)
      end

      def maxM
        max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
      end
    end

    defcase PastLazy, children : Slice(Any), min : UInt8, max : UInt8, n : Int32 = 0 do
      # FIXME: Ditto!

      def minM
        Magnitude.new(min)
      end

      def maxM
        max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
      end
    end
  end

  # Compiled entry operators such as `x: ⏏(%optional 0 x_)⏏`.
  module Op::Entry
    alias Any = Required | Optional | Present | Absent | AbsentKeypath | Negative | NegativeKeypath

    # Entries are assigned an eyeballed "cost". Cheaper entries are checked
    # first by e.g. `%layer`.
    enum Cost : UInt8
      VeryCheap
      Cheap
      Moderate
      Expensive
    end

    defcase Required, key : Term, value : Op::Any do
      def cost : Cost
        Cost::Moderate
      end
    end

    defcase Optional, key : Term, default : Term, value : Op::Any do
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

    defcase Negative, key : Term, barrier : Op::Any do
      def cost : Cost
        Cost::Moderate
      end
    end

    defcase NegativeKeypath, key : Term, barrier : Op::Any, name : Term do
      def cost : Cost
        Cost::Expensive
      end
    end
  end
end
