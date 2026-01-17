# ┌──────────────────────────┬───────┬─────────┬───────┬────────────┬──────────┬───────┐
# │            P             │ Norm  │ Compile │ Match │ Optimize   │ Backmap  │ Ubase │  docs
# ├──────────────────────────┼───────┼─────────┼───────┼────────────┼──────────┼───────┤
# │ type                     │   +   │   +     │   +   │            │    ·     │       │   ~
# │ literal                  │   +   │   +     │   +   │            │    ·     │       │   ~
# │ literal dict             │       │         │   ~   │            │    ·     │       │   ~
# │ blank                    │   +   │   +     │   +   │            │    ~     │       │   ~
# │ itemsonly                │   +   │   +     │   +   │            │    ·     │       │   ~
# │ pairsonly                │   ~   │   ~     │   ~   │            │    ·     │       │   ~
# │ bounds                   │       │         │   ~   │            │    ·     │       │   ·
# │ sketch                   │       │         │   ~   │            │    ·     │       │   ·
# │ %literal                 │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %partition               │   +   │   +     │   +   │            │    ·     │       │
# │ %let                     │   +   │   +     │   +   │            │    ~     │       │   ~
# │ %edge                    │   +   │   +     │   +   │            │    ~     │       │   ~
# │ %any                     │   ~   │   ~     │   ~   │            │    ·     │       │   ~
# │ %any°                    │   ~   │   ~     │   ~   │            │    ~     │       │   ~
# │ %all                     │   ~   │   ~     │   ~   │            │    ~     │       │   ~
# │ %keypool                 │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %-keypool                │       │         │       │            │          │       │
# │ %keytest                 │       │         │       │            │          │       │
# │ %not                     │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %layer                   │   +   │   +     │   +   │            │    ~     │       │
# │ %number                  │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %nonself                 │   +   │   ·     │   ·   │     ·      │    ·     │   ·   │   ·
# │ %symbol nonblank blank   │   ~   │   ~     │   ~   │            │          │       │   ~
# │ %string                  │       │         │       │            │          │       │
# │ %string date             │       │         │       │            │          │       │
# │ %string decimal          │       │         │       │            │          │       │
# │ %string json             │       │         │       │            │          │       │
# │ %string csv              │       │         │       │            │          │       │
# │ %string uri              │       │         │       │            │          │       │
# │ %pipe: + - * / d m **    │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %pipe: span tally type   │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %pipe: map               │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %value                   │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %-value _                │   ~   │   ~     │   ~   │            │    ·     │       │
# │ %-value _ keyp           │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %singular         │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %slot             │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %plural min max   │   +   │   +     │   +   │            │    ~     │       │
# │ %dict: %plural skip mm   │   +   │   +     │   +   │            │    ·     │       │
# │ %dict: %optional (item)  │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %many             │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %many/max         │       │         │       │            │          │       │
# │ %dict: %past             │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %past/max         │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %group            │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %gap min max      │   +   │   +     │   +   │            │    ·     │       │
# │ %dict: %entry/required   │   +   │   +     │   +   │            │    ~     │       │   ~
# │ %dict: %entry k %optiona │   +   │   +     │   +   │            │    ~     │       │   ~
# │ %dict: pair %- _         │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %dict: pair %- _ keyp    │   +   │   +     │   +   │            │    ·     │       │   ~
# │ %entry                   │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %entry°                  │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %entries                 │   ~   │   ~     │   ~   │            │    ·     │       │
# │ item first               │   ~   │   ~     │   ~   │            │    ~     │       │
# │ item source              │   ~   │   ~     │   ~   │            │    ~     │       │
# │ item store               │   ~   │   ~     │   ~   │            │    ~     │       │
# │ dig first dfs            │   ~   │   ~     │   ~   │            │    ~     │       │
# │ dig source dfs           │   ~   │   ~     │   ~   │            │    ~     │       │
# │ dig & store dfs          │   ~   │   ~     │   ~   │            │    ~     │       │
# │ dig first bfs            │   ~   │   ~     │   ~   │            │    ~     │       │
# │ dig & store bfs          │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %keypath                 │   ~   │   ~     │   ~   │            │    ·     │       │
# └──────────────────────────┴───────┴─────────┴───────┴────────────┴──────────┴───────┘
# + confident
# ~ will do
# · not needed

###

# TODO: string patterns
# We should be able to treat strings as bytestrings OR unicode under the hood. Thus string patterns (%string "...")
# should have bytes as their basic unit, and even below (bits), like Erlang's bitvectors. OR unicode characters,
# more like Raku.
#
# %string should be able to do what Erlang is able to do here:
# -define(IP_VERSION, 4).
# -define(IP_MIN_HDR_LEN, 5).
#
# DgramSize = byte_size(Dgram),
# case Dgram of
#     <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16,
#       ID:16, Flgs:3, FragOff:13,
#       TTL:8, Proto:8, HdrChkSum:16,
#       SrcIP:32,
#       DestIP:32, RestDgram/binary>> when HLen>=5, 4*HLen=<DgramSize ->
#         OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
#         <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
#     ...
# end.
#
# At least we should be able to split numbers into digits and match on them.

# TODO: support something like this?
# (%all (x_dict y_dict z_dict)
#      (== (span x) (span y))
#      (== (span x) (* (span z) 2)))

require "./src/wirewright"

include Ww

module ::Ww::M1::Search
  enum Part : UInt8
    Items
    Pairs
    Entries
  end
end

# TODO: the names of operators should be nouns. Currently some of them are and others
#   are not, fix that. In fact, Operator should probably be renamed to Subject or something
#   like that. Not sure how large of a refactor that is, and how much point is there in it.
module ::Ww::M1::Operator
  alias Any = Pass | Never | Num | Sym | SymBlank | SymNonblank | Boolean | Dict | Itemsonly | Pairsonly | SketchSubset | Bounds | BoundsGuard | MaxDepth | DictGuard | Literal | Capture | CaptureItemsonly | ItemSeq | ItemFirst | ItemLast | SingularSeq | Partition | Edge | LiteralWhitelist | ChoiceSource | Keypool | Span | Tally | Type | ParseML | Clamp | Bin | Both | LiteralBlacklist | Layer | ScanFirst | ScanSource | ScanAll | DfsFirst | DfsSource | DfsAll | BfsFirst | BfsAll | Value | NegativeValue | NegativeValueKeypath | EntriesFirst | EntriesSource | EntriesAll | Str | KeypathCapture | NegativeKeypool | Keytest | ValueLiteral | Filter | Pluck | Flat | Split | Adjacent | Untracked

  alias Bin = Add | Sub | Mul | Div | Idiv | Mod | Pow | Map

  alias First = ScanFirst | DfsFirst | BfsFirst | EntriesFirst | SplitFirst
  alias Source = DfsSource | ScanSource | EntriesSource | SplitSource
  alias All = ScanAll | DfsAll | BfsAll | EntriesAll | SplitAll

  defcase Layer, below : Any, side : Slice(Entry::Any)

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

  defcase DfsFirst, seq : Slice(Any), part : Search::Part, depth0 : Bool do
    def needle
      seq.first
    end
  end

  defcase DfsSource, seq : Slice(Any), part : Search::Part, depth0 : Bool do
    def needle
      seq.first
    end
  end

  defcase DfsAll, successor : Any, seq : Slice(Any), selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool do
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

  defcase BfsFirst, seq : Slice(Any), part : Search::Part, depth0 : Bool do
    def needle
      seq.first
    end
  end

  defcase BfsAll, successor : Any, seq : Slice(Any), selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool do
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

# FIXME: bad bad BAD idea
alias Magnitude = Float32

module ::Ww::M1::Operator::Item
  alias Any = Singular | Slot | Plural | Group | GapFirst | GapSource | Optional | Many | Past

  # wtf is it called TAIL?!?!?!
  record Singular, tail : Operator::Any
  record Slot, name : Term

  record Plural, contenders : Slice(Term?), min1 : Magnitude, max1 : Magnitude, type : TermType, follower : Follower, frac : UInt32, strategy : ExpandStrategy do
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

  record Group, successor : Operator::Any, children : Slice(Any) do
    def capture
      successor.as(Operator::Capture).capture
    end
  end

  alias Gap = GapFirst | GapSource
  record GapFirst, measurer : Operator::Any, frac : UInt32, strategy : ExpandStrategy
  record GapSource, measurer : Operator::Any, frac : UInt32, strategy : ExpandStrategy
  record Optional, default : Term, body : Operator::Any
  record Many, successor : Operator::Any, children : Slice(Any), interior : Set(Term), min : UInt8, max : UInt8 do
    def capture
      successor.as(Operator::Capture).capture
    end

    def minM
      Magnitude.new(min)
    end

    def maxM
      max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
    end
  end

  alias Past = PastGreedy | PastLazy

  record PastGreedy, children : Slice(Any), min : UInt8, max : UInt8, n : Int32 = 0 do
    # FIXME: Use magnitude instead of U8 in the first place!!!!!

    def minM
      Magnitude.new(min)
    end

    def maxM
      max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
    end
  end

  record PastLazy, children : Slice(Any), min : UInt8, max : UInt8, n : Int32 = 0 do
    # FIXME: Ditto!

    def minM
      Magnitude.new(min)
    end

    def maxM
      max == 0 ? Magnitude::INFINITY : Magnitude.new(max)
    end
  end
end

module ::Ww::M1::Operator::Item
  enum ExpandStrategy : UInt8
    Auto
    Sway
    Lazy
    Greedy
  end
end

module ::Ww::M1
  SYM_LT  = Term.of(:<)
  SYM_GT  = Term.of(:>)
  SYM_LTE = Term.of(:<=)
  SYM_GTE = Term.of(:>=)
  SYM_INF = Term.of(:∞)

  # Contains methods, constants, etc. that work together to implement `M1.normal`.
  module Normal
    extend self

    SYMS_CMP = {SYM_LT, SYM_GT, SYM_LTE, SYM_GTE}
    SYMS_LTX = {SYM_LT, SYM_LTE}

    PASS = Term.of({:"%pass"})

    BLANK_DICT    = Term.of({:"%dict"})
    BLANK_NUMBER  = Term.of({:"%number", :_})
    BLANK_STRING  = Term.of({:"%string"})
    BLANK_SYMBOL  = Term.of({:"%symbol"})
    BLANK_BOOLEAN = Term.of({:"%boolean"})

    EDGE_ANY     = Term.of(:"%edge", :_)
    EDGE_SYMBOL  = Term.of(:"%edge", :_symbol)
    EDGE_STRING  = Term.of(:"%edge", :_string)
    EDGE_NUMBER  = Term.of(:"%edge", :_number)
    EDGE_DICT    = Term.of(:"%edge", :_dict)
    EDGE_BOOLEAN = Term.of(:"%edge", :_boolean)

    # :nodoc:
    INT = Term.of(
      u8: {:"%number", UInt8::MIN, :<=, {:whole, :_}, :<=, UInt8::MAX},
      u16: {:"%number", UInt16::MIN, :<=, {:whole, :_}, :<=, UInt16::MAX},
      u32: {:"%number", UInt32::MIN, :<=, {:whole, :_}, :<=, UInt32::MAX},
      u64: {:"%number", UInt64::MIN, :<=, {:whole, :_}, :<=, UInt64::MAX},
      u128: {:"%number", UInt128::MIN, :<=, {:whole, :_}, :<=, UInt128::MAX},
      i8: {:"%number", Int8::MIN, :<=, {:whole, :_}, :<=, Int8::MAX},
      "-i8": {:"%number", Int8::MIN, :<=, {:whole, :_}, :<, 0},
      "+i8": {:"%number", 0, :<=, {:whole, :_}, :<=, Int8::MAX},
      "+i8!": {:"%number", 0, :<, {:whole, :_}, :<=, Int8::MAX},
      i16: {:"%number", Int16::MIN, :<=, {:whole, :_}, :<=, Int16::MAX},
      "-i16": {:"%number", Int16::MIN, :<=, {:whole, :_}, :<, 0},
      "+i16": {:"%number", 0, :<=, {:whole, :_}, :<=, Int16::MAX},
      "+i16!": {:"%number", 0, :<, {:whole, :_}, :<=, Int16::MAX},
      i32: {:"%number", Int32::MIN, :<=, {:whole, :_}, :<=, Int32::MAX},
      "-i32": {:"%number", Int32::MIN, :<=, {:whole, :_}, :<, 0},
      "+i32": {:"%number", 0, :<=, {:whole, :_}, :<=, Int32::MAX},
      "+i32!": {:"%number", 0, :<, {:whole, :_}, :<=, Int32::MAX},
      i64: {:"%number", Int64::MIN, :<=, {:whole, :_}, :<=, Int64::MAX},
      "-i64": {:"%number", Int64::MIN, :<=, {:whole, :_}, :<, 0},
      "+i64": {:"%number", 0, :<=, {:whole, :_}, :<=, Int64::MAX},
      "+i64!": {:"%number", 0, :<, {:whole, :_}, :<=, Int64::MAX},
      i128: {:"%number", Int128::MIN, :<=, {:whole, :_}, :<=, Int128::MAX},
      "-i128": {:"%number", Int128::MIN, :<=, {:whole, :_}, :<, 0},
      "+i128": {:"%number", 0, :<=, {:whole, :_}, :<=, Int128::MAX},
      "+i128!": {:"%number", 0, :<, {:whole, :_}, :<=, Int128::MAX},
    )

    private def typesym(blank : Term::Sym::Blank) : Term::Sym
      blank.type.blank
    end

    # Context for `Normal` calls. Left for future use.
    record Context

    # Returns the normal form of an item sequence *node*.
    def item(ctx : Context, node : Term) : Term
      Term.of_case(node, engine: M0) do
        matchpi %[_symbol] do
          continue unless blank = node.blank?
          continue unless blank.plural?

          name = blank.name?

          Term.of(:"%plural", name ? {:"%capture", name} : nil, type: typesym(blank), min: blank.mult.one_or_more? ? 1 : 0, max: SYM_INF)
        end

        # Fast path to %singular for literal terms.
        matchpi %[_number], %[_string], %[_boolean] do
          {:"%singular", pattern(ctx, node)}
        end

        matchpi(
          %[(%plural ¦ opts_)],
          %[(%plural/min ¦ opts_)],
          %[(%plural/max ¦ opts_)],
          cues: {:"%plural", :"%plural/min", :"%plural/max"},
        ) do
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 0u8)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            _ = s.key(:type, value: {:_number, :_string, :_symbol, :_dict, :_}, default: :_)
            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.morph({0, node[0]})
          end
        end

        matchpi(
          %[(%plural capture_ ¦ opts_)],
          %[(%plural/min capture_ ¦ opts_)],
          %[(%plural/max capture_ ¦ opts_)],
          cues: {:"%plural", :"%plural/min", :"%plural/max"},
        ) do |opts|
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 0u8)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            _ = s.key(:type, value: {:_number, :_string, :_symbol, :_dict, :_}, default: :_)
            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.morph({0, node[0]}, {1, {:"%capture", capture}})
          end
        end

        matchpi %[(%optional _ body_)], cue: :"%optional" do
          node.morph({2, pattern(ctx, body)})
        end

        matchpi %[(%group successor_ _ _*)], cue: :"%group" do
          Term::Dict.build do |commit|
            commit << :"%group"
            commit << pattern(ctx, successor)
            commit.concat(node.items.move(2)) { |member| item(ctx, member) }
          end
        end

        matchpi %[(%many successor_ _ _* ¦ opts_)], cue: :"%many" do
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 1u8)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.transaction do |commit|
              commit << :"%many"
              commit << pattern(ctx, successor)
              commit.concat(node.items.move(2)) { |member| item(ctx, member) }
            end
          end
        end

        matchpi %[(%past _ _* ¦ opts_)], cue: :"%past" do
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 0u8)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.transaction do |commit|
              commit << :"%past"
              commit.concat(node.items.move(1)) { |member| item(ctx, member) }
              commit.with(:greedy, false)
            end
          end
        end

        matchpi %[(%past/max _ _* ¦ opts_)], cue: :"%past/max" do |opts|
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 0u8)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.transaction do |commit|
              commit << :"%past"
              commit.concat(node.items.move(1)) { |member| item(ctx, member) }
              commit.with(:greedy, true)
            end
          end
        end

        matchpi(
          %[(%gap measurer_)],
          %[(%gap/min measurer_)],
          %[(%gap/max measurer_)],
          cues: {:"%gap", :"%gap/min", :"%gap/max"}
        ) do
          node.morph({1, pattern(ctx, measurer)})
        end

        matchpi(
          %[(%gap° measurer_)],
          %[(%gap/min° measurer_)],
          %[(%gap/max° measurer_)],
          cues: {:"%gap°", :"%gap/min°", :"%gap/max°"}
        ) do
          node.morph({1, pattern(ctx, measurer)})
        end

        # NOTE: Currently we do not register %slot as a capture. And I don't think
        # there is any point in doing so.
        matchpi %[(%slot _)], cue: :"%slot" do
          node
        end

        otherwise { {:"%singular", pattern(ctx, node)} }
      end
    end

    # Returns the normal form of a pairspart *key*-*value* pair.
    def pair(ctx : Context, key : Term, value : Term) : Term
      Term.of_case(value, engine: M0) do
        matchpi %[(%optional default_ body_)], cue: :"%optional" do
          {:"%entry/optional", {:"%barrier", default}, pattern(ctx, body)}
        end

        matchpi %[(%- positive_)], cue: :"%-" do
          {:"%entry/negative", pattern(ctx, positive)}
        end

        matchpi %[(%- positive_ name_)], cue: :"%-" do
          {:"%entry/negative", pattern(ctx, positive), {:"%barrier", name}}
        end

        otherwise do
          {:"%entry/required", pattern(ctx, value)}
        end
      end
    end

    # Returns `true` if *dict* is *definitely* a literal dict.
    #
    # Checks if *dict* *probably contains* pattern matching constructs. If it
    # *probably contains* such constructs, returns `false`. Otherwise, returns
    # `true`. Something like `%qux` will disorient this algorithm into thinking
    # there is a pattern construct in there despite `%qux` not being one. This
    # should not affect semantics but may affect performance. You are thus not
    # advised to prefix arbitrary symbols with `%` unless they are related to
    # pattern matching or pattern matching is inevitable regardless.
    private def literal?(dict : Term::Dict) : Bool
      return true if dict.empty?

      dict.ee.all? do |k, v|
        if vsym = v.as_sym?
          !(vsym.blank? || M1.probably_node?(vsym) || vsym == SYM_EDGE)
        elsif vdict = v.as_d?
          literal?(vdict)
        else
          true # Strings, booleans, numbers are literals.
        end
      end
    end

    # Returns the normal form of a dictionary term *dict*.
    def dict(ctx : Context, dict : Term::Dict) : Term
      if literal?(dict)
        return Term.of(:"%literal", dict)
      end

      if dict.itemsonly?
        node = Term::Dict.build do |commit|
          commit << :"%itemseq"
          commit.concat(dict.items) { |itemnode| item(ctx, itemnode) }
        end

        return Term.of(node)
      end

      # E.g. {x: 100, y: 200} = (%layer () x: 100 y: 200)
      if dict.pairsonly?
        return pattern(ctx, Term.of(:"%layer", Term[], dict))
      end

      Term.of(:"%partition", dict(ctx, dict.itemspart), dict(ctx, dict.pairspart))
    end

    # Returns the normal form of *pattern*.
    def pattern(ctx : Context, pattern : Term) : Term
      Term.of_case(pattern, engine: M0) do
        # NOTE: this is a fast path for itemsonly dictionaries. They'd otherwise be
        # at the very bottom, which isn't exactly a good choice due to their frequency
        # in practice. We do only the simplest, almost probabilistic checks here; if they
        # fail, we will go with the longer but precise path.
        #
        # WARNING: if you want a pattern matching construct that's a dictionary and that
        # doesn't start with %, you will have to be friends with this fast path.
        matchpi %[_dict] do
          pdict = pattern.unsafe_as_d

          continue unless pdict.itemsonly?
          continue unless head = pdict.items.first?
          continue unless headsym = head.as_sym?
          continue if M1.probably_node?(headsym) || headsym == SYM_EDGE

          dict(ctx, pdict)
        end

        # Similarly, %let is very frequent (especially due to blanks such as x_)
        # compiling to e.g. (%let x _).
        matchpi %[(%'%let capture_ successor_)], cue: :"%let" do
          {:"%let", {:"%capture", capture}, pattern(ctx, successor)}
        end

        # Blanks are also very frequent; as are symbols. We avoid using matchpis
        # for type-only blanks _number, _string, etc. so that this _symbol matchpi
        # is immediately reached.
        #
        # Named blanks are transformed into %let which we then recurse upon.
        # The recursion is done to perform further reductions (since we're
        # not rewriting here we must recurse explicitly).
        matchpi %[_symbol] do
          case pattern
          when SYM_BLANK_ANY     then PASS
          when SYM_BLANK_NUMBER  then BLANK_NUMBER
          when SYM_BLANK_STRING  then BLANK_STRING
          when SYM_BLANK_SYMBOL  then BLANK_SYMBOL
          when SYM_BLANK_BOOLEAN then BLANK_BOOLEAN
          when SYM_BLANK_DICT    then BLANK_DICT
          else
            continue unless blank = pattern.unsafe_as_sym.blank?
            continue unless blank.singular?
            continue unless name = blank.name?

            pattern(ctx, Term.of(:"%let", name, typesym(blank)))
          end
        end

        # Literals are very frequent.
        matchpi %[_symbol], %[_number], %[_string], %[_boolean] do
          {:"%literal", pattern}
        end

        # Edges are somewhat frequent in Soma-land.
        #
        # (edge ...) is the only pattern matching construct not prefixed with a %.
        # It is extremely abundant in Soma/delta7 patterns, and ML emits it on @...,
        # e.g. @foo is (edge ...). We reuse @foo_ to match ((%literal edge) _).
        matchpi %[(edge arg_symbol)], cue: :edge do |arg|
          arg = arg.unsafe_as_sym
          continue unless blank = arg.blank?
          continue unless blank.singular?

          case blank.type
          in .symbol?  then edge = EDGE_SYMBOL
          in .string?  then edge = EDGE_STRING
          in .number?  then edge = EDGE_NUMBER
          in .dict?    then edge = EDGE_DICT
          in .boolean? then edge = EDGE_BOOLEAN
          in .any?     then edge = EDGE_ANY
          end

          if name = blank.name?
            edge = Term.of(:"%let", {:"%capture", name}, edge)
          end

          edge
        end

        # Partition is pretty frequent.
        matchpi %[(%'%partition itemspart_ pairspart_)], cue: :"%partition" do
          {:"%partition", pattern(ctx, itemspart), pattern(ctx, pairspart)}
        end

        matchpi %[(%'%layer below_ side_dict)], cue: :"%layer" do
          pattern.transaction do |commit|
            commit.with(1, pattern(ctx, below))

            nside = side.transaction do |nside|
              side.each_entry do |k, v|
                nside.with(k, pair(ctx, k, v))
              end
            end

            commit.with(2, nside)
          end
        end

        # (%layer _ k1: v1 k2: v2 ...) is a shorthand for (%layer _ {k1: v1 k2: v2 ...}).
        matchpi %[(%'%layer below_ ¦ pairs_)], cue: :"%layer" do
          pattern(ctx, Term.of(:"%layer", below, pairs))
        end

        # %number should be %terminal.
        matchpi %[(%number %'_)], %[(%number %'(whole _))], cue: {:"%number", :_}, cues: {nil, :whole} do
          {:"%terminal", pattern}
        end

        # Compile fixed-width %number into the corresponding bounds check. We do not
        # actually have fixed-width numbers. These kinds of patterns are often used
        # on the Crystal side to ensure we can safely e.g. to(Int32).
        matchpi %[(%number type_symbol)], cue: :"%number" do
          continue unless normal = INT[type]?

          {:"%terminal", normal}
        end

        matchpi %{(%number _*)}, cue: :"%number" do
          continue unless Operator::Num.parse?(pattern)

          {:"%terminal", pattern}
        end

        matchpi(
          %[(%pipe (+ _number) successor_)],
          %[(%pipe (- _number) successor_)],
          %[(%pipe (* _number) successor_)],
          %[(%pipe (/ _number) successor_)],
          %[(%pipe (div _number) successor_)],
          %[(%pipe (mod _number) successor_)],
          %[(%pipe (** _number) successor_)],
          %[(%pipe (clamp _number ..= _number) successor_)],
          %[(%pipe (map _dict) successor_)],
          %[(%pipe span successor_)],
          %[(%pipe tally successor_)],
          %[(%pipe type successor_)],
          %[(%pipe ml successor_)],
          %[(%pipe untracked successor_)],
          cue: :"%pipe",
          cues: {:+, :-, :*, :/, :div, :mod, :**, :clamp, :map, :span, :tally, :type, :ml, :untracked}
        ) do
          pattern.morph(
            {1, ->(term : Term) { Term.of(:"%barrier", term) }},
            {2, pattern(ctx, successor)},
          )
        end

        matchpi %[(%pipe head_ _*)], cue: :"%pipe" do
          continue if pattern.size < 4 # %pipe + head_1 + head_2 + body

          body = Term::Dict.build do |commit|
            commit << :"%pipe"

            rest = pattern.items.move(2)
            rest.each { |item| commit << item }
          end

          pattern(ctx, Term.of(:"%pipe", head, body))
        end

        matchpi %[(%all)], cue: :"%all" do
          PASS
        end

        matchpi %[(%all a_)], cue: :"%all" do
          pattern(ctx, a)
        end

        matchpi %[(%all a_ b_)], cue: :"%all" do
          {:"%all", pattern(ctx, a), pattern(ctx, b)}
        end

        matchpi %[(%all a_ b_ _ _*)], cue: :"%all" do
          rewritten = Term::Dict.build do |commit|
            commit << :"%all" << {:"%all", a, b}
            commit.concat(pattern.items.move(3))
          end

          pattern(ctx, Term.of(rewritten))
        end

        matchpi %[(%any _*)], cue: :"%any" do
          {:"%terminal", pattern.morph({0, :"%any/literal"})}
        end

        matchpi %[(%any° _*)], cue: :"%any°" do
          Term::Dict.build do |commit|
            commit << :"%any/source"

            branches = pattern.items.move(1)
            branches.each { |branch| commit << pattern(ctx, branch) }
          end
        end

        # Leave %literal as is.
        matchpi %[(%'%literal _)], cue: :"%literal" do
          pattern
        end

        # Mark %keypool and %not as %terminal so that walk doesn't walk inside them.
        matchpi(
          %[(%keypool _ _*)],
          %[(%-keypool _*)],
          %[(%keytest _*)],
          %[(%not _ _*)],
          cues: {:"%keypool", :"%-keypool", :"%keytest", :"%not"}
        ) { {:"%terminal", pattern} }

        matchpi %[(%keypath capture_)], cue: :"%keypath" do
          {:"%keypath", {:"%capture", capture}}
        end

        # %nonself is dissolved at normalization.
        matchpi %{[%nonself arg_]}, cue: :"%nonself" do
          pattern(ctx, arg)
        end

        # |@ patterns.operator.%never
        #
        # |@block
        # `%never` is a dedicated nevermatch operator.
        #
        # Its most frequent use-case is in combination with `patterns.operator.pair.%-.keypath`.
        # By writing `x: (%- (%never) x)`, what you mean is, "the positive example" --
        # which for `%-` signals "do not match" -- is a nevermatch. In other words,
        # *nothing* will be considered a positive example for `%-`; everything will be
        # considered a negative example. Thus you will be able to set `x` in both
        # possible cases: one where it exists, and one where it doesn't.
        # |@endblock
        matchpi %[(%never)], cue: :"%never" do
          pattern
        end

        matchpi %[(%value capture_ body_)], cue: :"%value" do
          {:"%value", {:"%capture", capture}, pattern(ctx, body)}
        end

        matchpi %[(%-value capture_)], cue: :"%-value" do
          {:"%-value", {:"%capture", capture}}
        end

        matchpi %[(%-value capture_ name_)], cue: :"%-value" do
          {:"%-value", {:"%capture", capture}, {:"%barrier", name}}
        end

        matchpi %[(%item _ _*)], cue: :"%item" do
          Term::Dict.build do |commit|
            commit << :"%items/first"
            commit.concat(pattern.items.move(1)) { |item| pattern(ctx, item) }
          end
        end

        matchpi %[(%item° _ _*)], cue: :"%item°" do
          Term::Dict.build do |commit|
            commit << :"%items/source"
            commit.concat(pattern.items.move(1)) { |item| pattern(ctx, item) }
          end
        end

        matchpi %[(%items successor_ _ _* ¦ opts_)], cue: :"%items" do |opts|
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 1u8)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.transaction do |commit|
              commit << :"%items/all" << pattern(ctx, successor)
              commit.concat(pattern.items.move(2)) { |item| pattern(ctx, item) }
            end
          end
        end

        matchpi %[(%entry k_ v_)], cue: :"%entry" do
          {:"%entries/first", pattern(ctx, k), pattern(ctx, v)}
        end

        matchpi %[(%entry° k_ v_)], cue: :"%entry°" do
          {:"%entries/source", pattern(ctx, k), pattern(ctx, v)}
        end

        matchpi %[(%entries successor_ k_ v_ ¦ opts_)], cue: :"%entries" do |opts|
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 1u8)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.morph(
              {0, :"%entries/all"},
              {1, pattern(ctx, successor)},
              {2, pattern(ctx, k)},
              {3, pattern(ctx, v)},
            )
          end
        end

        matchpi %{(%leaf _ _* ¦ opts_)}, %{(%leaf° _ _* ¦ opts_)}, cues: {:"%leaf", :"%leaf°"} do
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            _ = s.key(:in, value: {:items, :pairs, :entries}, default: :items)
            _ = s.key(:order, value: {:dfs, :bfs}, default: :dfs)
            _ = s.key(:self, value: {true, false}, default: false)

            opts.transaction do |commit|
              # FIXME: Do we *really* have to do this?!?!? Man...
              case pattern[0]
              when Term.of(:"%leaf")  then commit << :"%leaves/first"
              when Term.of(:"%leaf°") then commit << :"%leaves/source"
              end

              commit.concat(pattern.items.move(1)) { |item| pattern(ctx, item) }
            end
          end
        end

        matchpi %[(%leaves successor_ _ _* ¦ opts_)], cue: :"%leaves" do |opts|
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            _ = s.key(:in, value: {:items, :pairs, :entries}, default: :items)
            _ = s.key(:order, value: {:dfs, :bfs}, default: :dfs)
            min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 0)
            max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: SYM_INF)
            _ = s.key(:self, value: {true, false}, default: false)

            continue if min.is_a?(UInt8) && max.is_a?(UInt8) && min > max

            opts.transaction do |commit|
              commit << :"%leaves/all" # ditto
              commit << pattern(ctx, successor)
              commit.concat(pattern.items.move(2)) { |item| pattern(ctx, item) }
            end
          end
        end

        # Expand (%string nonempty) into (%all (%not "") _string)
        matchpi %[(%string nonempty)], cue: {:"%string", :nonempty} do
          pattern(ctx, Term.of(:"%all", {:"%not", ""}, :_string))
        end

        matchpi %[(%symbol nonblank)], cue: {:"%symbol", :nonblank} do
          {:"%terminal", pattern}
        end

        matchpi %[(%symbol blank name_ type_)], cue: {:"%symbol", :blank} do
          {:"%symbol", :blank, pattern(ctx, name), pattern(ctx, type)}
        end

        matchpi %[(%filter deps←(_*) selector_ successor_ ¦ opts_)], cue: :"%filter" do
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            _ = s.key(:min, type: UInt32, default: 1)
            _ = s.key(:max, type: UInt32, default: SYM_INF)

            opts.transaction do |commit|
              commit << :"%filter" << {:"%barrier", deps}
              commit << pattern(ctx, selector)
              commit << pattern(ctx, successor)
            end
          end
        end

        matchpi %[(%pluck spec←(_*) successor_)], cue: :"%pluck" do
          {:"%pluck", {:"%barrier", spec}, pattern(ctx, successor)}
        end

        matchpi %[(%flat spec←(_*) successor_)], cue: :"%flat" do
          {:"%flat", {:"%barrier", spec}, pattern(ctx, successor)}
        end

        # TODO: %adjacent shouldn't be a separate operator. Instead, we should compile
        # %split's that can be treated as adjacency checks into Operator::Adjacent. I don't
        # want to leak performance reasons behind %adjacent to the user. Instead, since we
        # always know better, we should pick %adjacent if possible during O2.
        #
        # Say, notice how (%split[°] _ x_ _) -> ⟨x_⟩[°]; notice also how (%split _ a_ ⟨b_⟩) ->
        # (%adjacent a_ b_), and (%split _ a_ (%split _ b_ _)) -> (%split _ a_ ⟨b_⟩) -> (%adjacent a_ b_).
        #
        # Constructions such as (%split _ a_ (%split _ b_ ⟨c_⟩)) can be rewritten to
        # (%split _ a_ (%adjacent b_ c_)) -> (%adjacent a_ b_ c_).
        matchpi %[(%adjacent _ _ _*)], cue: :"%adjacent" do
          mid = pattern.items.move(1)
          midp = Term::Dict.build do |commit|
            commit.concat(mid) { |item| pattern(ctx, item) }
          end

          {:"%adjacent", midp}
        end

        matchpi %[(%split lhs_ _ _*)], cue: :"%split" do
          mid = pattern.items.move(2).grow(-1)
          midp = Term::Dict.build do |commit|
            commit.concat(mid) { |item| pattern(ctx, item) }
          end

          rhs = pattern.items.last

          {:"%split/first", pattern(ctx, lhs), midp, pattern(ctx, rhs)}
        end

        matchpi %[(%split° lhs_ _ _*)], cue: :"%split°" do
          mid = pattern.items.move(2).grow(-1)
          midp = Term::Dict.build do |commit|
            commit.concat(mid) { |item| pattern(ctx, item) }
          end

          rhs = pattern.items.last

          {:"%split/source", pattern(ctx, lhs), midp, pattern(ctx, rhs)}
        end

        matchpi %[(%splits successor_ lhs_ _ _* ¦ opts_)], cue: :"%splits" do
          M0.schema(opts) do |s, opts|
            s.on_mismatch { continue }

            _ = s.key(:min, type: UInt32, default: 1)
            _ = s.key(:max, type: UInt32, default: SYM_INF)

            mid = pattern.items.move(3).grow(-1)
            midp = Term::Dict.build do |commit|
              commit.concat(mid) { |item| pattern(ctx, item) }
            end

            rhs = pattern.items.last

            opts.transaction do |commit|
              commit << :"%split/all"
              commit << pattern(ctx, successor)
              commit << pattern(ctx, lhs) << midp << pattern(ctx, rhs)
            end
          end
        end

        # NOTE: you should insert new matchpis here, especially if they are infrequent.
        # Below we have raw dict/literal treatment; if you put your matchpis below they
        # will probably not be reached. If your matchpi does not start with a %, make sure
        # to update the dict fast path above.

        matchpi %[_dict] { dict(ctx, pattern.unsafe_as_d) }
      end
    end
  end

  # Contains methods, constants, etc. that work together to implement `M1.bounds`.
  #
  # Bounds are represented as a pair of `Magnitude`s. The first Magnitude is the minimum
  # bound, inclusive if known; the second is the maximum Bound, inclusive if known.
  # Either or both bounds may be unknown. An unknown bound is assigned the magnitude
  # of infinity (`Magnitude::INFINITY`). This is because arithmetic with infinities is
  # close enough semantically to arithmetic with unknowns.
  module Bounds
    extend self

    # Computes the bounds of an item sequence *item*. Raises `ArgumentError` if *item*
    # is not one of the recognized item sequence items.
    def item(item : Term) : {Magnitude, Magnitude}
      Term.case(item, engine: M0) do
        matchpi %{[%singular _]}, cue: :"%singular" do
          {Magnitude.new(1.0), Magnitude.new(1.0)}
        end

        matchpi %{[%slot _]}, cue: :"%slot" do
          {Magnitude.new(0.0), Magnitude.new(0.0)}
        end

        matchpi %{[%optional _ _]}, cue: :"%optional" do
          {Magnitude.new(0.0), Magnitude.new(1.0)}
        end

        matchpi(
          %{[%gap _]},
          %{[%gap/min _]},
          %{[%gap/max _]},
          %{[%gap° _]},
          %{[%gap/min° _]},
          %{[%gap/max° _]},
          cues: {:"%gap", :"%gap/min", :"%gap/max", :"%gap°", :"%gap/min°", :"%gap/max°"}
        ) do
          {Magnitude.new(0.0), Magnitude::INFINITY}
        end

        matchpi(
          %[(%plural _* ¦ _ min: minT_ max: maxT_)],
          %[(%plural/min _* ¦ _ min: minT_ max: maxT_)],
          %[(%plural/max _* ¦ _ min: minT_ max: maxT_)],
          cues: {:"%plural", :"%plural/min", :"%plural/max"}
        ) do
          {minT.to(Magnitude), maxT == SYM_INF ? Magnitude::INFINITY : maxT.to(Magnitude)}
        end

        matchpi %[(%many _ _ _* ¦ _ min: minT_ max: maxT_)], cue: :"%many" do
          min0 = minT.to(Magnitude)
          max0 = maxT == SYM_INF ? Magnitude::INFINITY : maxT.to(Magnitude)
          # [%many successor_ ⏏item0_ _*]
          min, max = items(item.items.move(2))
          {min0 * min, max0 * max}
        end

        matchpi %[(%past _* ¦ _ min: minT_ max: maxT_)], cue: :"%past" do
          min0 = minT.to(Magnitude)
          max0 = maxT == SYM_INF ? Magnitude::INFINITY : maxT.to(Magnitude)
          min, max = items(item.items.move(1))
          # FIXME: HACK: FP math isn't what we need here!!!! FP math is correct here
          # but for us inf means unbounded. "zero times unbounded" means unbounded!!!
          if (max0.zero? && max.infinite?) || (max0.infinite? && max.zero?)
            {min0 * min, Magnitude::INFINITY}
          else
            {min0 * min, max0 * max}
          end
        end

        matchpi %{[%group _ _ _*]}, cue: :"%group" do
          # [%group successor_ ⏏_ _*]
          items(item.items.move(2))
        end

        otherwise { {0.0f32, Magnitude::INFINITY} }
      end
    end

    # Computes the bounds of a pairspart (`%layer`) entry *node*. *key* is the
    # key of the entry.
    def entry(key : Term, node : Term) : {Magnitude, Magnitude}
      Term.case(node, engine: M0) do
        matchpi %{[%entry/required _]}, cue: :"%entry/required" do
          {Magnitude.new(1.0), Magnitude.new(1.0)}
        end

        matchpi %{[%entry/optional _ _]}, cue: :"%entry/optional" do
          {Magnitude.new(0.0), Magnitude.new(1.0)}
        end

        matchpi(
          %{[%entry/negative (%pass)]},
          %{[%entry/negative (%pass) _]},
          cue: {:"%entry/negative", :"%pass"},
        ) do
          {Magnitude.new(0.0), Magnitude.new(0.0)}
        end

        otherwise { {0.0f32, Magnitude::INFINITY} }
      end
    end

    # Computes the bounds of an enumerable of item sequence item nodes (see `item`).
    def items(ie : Enumerable(Term)) : {Magnitude, Magnitude}
      min = max = Magnitude.new(0)

      ie.each do |item|
        imin, imax = item(item)
        min += imin
        max += imax
      end

      {min, max}
    end

    # Computes the bounds of an enumerable of pairspart (`%layer`) keys
    # and associated entry nodes (see `entry`).
    def entries(ee : Enumerable({Term, Term})) : {Magnitude, Magnitude}
      min = max = Magnitude.new(0)

      ee.each do |key, node|
        emin, emax = entry(key, node)
        min += emin
        max += emax
      end

      {min, max}
    end

    # Computes the bounds of a normal pattern *normp*.
    def pattern(normp : Term) : {Magnitude, Magnitude}
      Term.case(normp, engine: M0) do
        matchpi %{[%'%partition itemspart_ pairspart_]}, cue: :"%partition" do
          min0, max0 = pattern(itemspart)
          min1, max1 = pattern(pairspart)

          {min0 + min1, max0 + max1}
        end

        matchpi %{[%itemseq _*]}, cue: :"%itemseq" do
          items(normp.items.move(1))
        end

        # If the layer has an empty successor (closed layer) we're able to use
        # the max as well.
        matchpi %{[%'%layer (%'%literal ()) side_dict]}, cue: {:"%layer", :"%literal"} do
          # %layer is a trusted source here, its `side` dict only contains %entry/s,
          # and we already know how to compute bounds for those here in this method.
          entries(side.unsafe_as_d.ee)
        end

        # If the layer is open we've no choice but to drop the max.
        matchpi %{[%'%layer _ side_dict _*]}, cue: :"%layer" do
          min, _ = entries(side.unsafe_as_d.ee)

          {min, Magnitude::INFINITY}
        end

        matchpi %{[%'%literal d_dict]}, cue: :"%literal" do
          {Magnitude.new(d.size), Magnitude.new(d.size)}
        end

        matchpi(
          %{[%items/first _*]},
          %{[%items/source _*]},
          cues: {:"%items/first", :"%items/source"}
        ) do
          needle = normp.items.move(1)

          {Magnitude.new(needle.size), Magnitude::INFINITY}
        end

        matchpi %[(%items/all _* ¦ _ min: minT_number max: _)], cue: :"%items/all" do
          min = minT.to(Magnitude)
          needle = normp.items.move(2)

          {min * needle.size, Magnitude::INFINITY}
        end

        otherwise { {0.0f32, Magnitude::INFINITY} }
      end
    end
  end

  module Item
    # TODO: I think that instead of doing this crappy crap with lookahead functions
    # I barely remember, we should just do a flattening pass before proceeding to
    # itemseq and use that. This way to get our true follower (vs. e.g. %group) -- which
    # is why we do this in the first place -- we only need to increment our index. Obviously
    # something will have to advance the index but I think that's generally simpler to reason
    # about. OR maybe we can make a MONAD that hides this stuff for us and lets us simply
    # query follower as we go through the items!!!!

    alias Neighbor = {Term, NeighborFn}?
    alias NeighborFn = -> Neighbor

    def self.neighbor?(items : Term::Dict::ItemsView, outside : NeighborFn) : Neighbor
      unless head = items.first?
        return outside.call
      end

      Term.case(head, engine: M0) do
        # Dip into %group nodes. If failed, continue to the next item on the current level.
        matchpi %[(%group _ _ _*)], cue: :"%group" do
          # (%group successor_ ⏏_ _*)
          neighbor?(head.items.move(2), -> { neighbor?(items.move(1), outside).as(Neighbor) })
        end

        # Skip slots.
        matchpi %[(%slot _)], cue: :"%slot" do
          neighbor?(items.move(1), outside)
        end

        otherwise { return head, -> { neighbor?(items.move(1), outside).as(Neighbor) } }
      end
    end

    def self.sequence(items : Term::Dict::ItemsView, neighbor : NeighborFn, captures : Bag(Term)) : Array(Operator::Item::Any)
      operators = Array(Operator::Item::Any).new(items.size)

      until items.empty?
        operator, items = Item.operator(items, neighbor, captures)
        operators << operator
      end

      operators
    end

    # TODO: rename
    def self.frac(neighbor) : UInt32
      frac = 1u32 # self

      while row = neighbor.call.as?({Term, NeighborFn})
        _, neighbor = row
        frac += 1
      end

      frac
    end

    # TODO: rename
    def self.follower(neighbor) : Operator::Item::Plural::Follower
      follower = Operator::Item::Plural::Follower::None

      unless row = neighbor.call.as?({Term, NeighborFn})
        return follower
      end

      first, _ = row

      Term.case(first, engine: M0) do
        matchpi %[(%plural _* ¦ _ type: type_symbol)], cue: :"%plural" do
          follower =
            case type
            when SYM_BLANK_ANY     then Operator::Item::Plural::Follower::Any
            when SYM_BLANK_DICT    then Operator::Item::Plural::Follower::Dict
            when SYM_BLANK_SYMBOL  then Operator::Item::Plural::Follower::Symbol
            when SYM_BLANK_STRING  then Operator::Item::Plural::Follower::String
            when SYM_BLANK_NUMBER  then Operator::Item::Plural::Follower::Number
            when SYM_BLANK_BOOLEAN then Operator::Item::Plural::Follower::Boolean
            else
              unreachable
            end
        end

        otherwise { }
      end

      follower
    end

    # TODO: switch to using matchpis here and everywhere!
    def self.operator(feed : Term::Dict::ItemsView, outside : NeighborFn, captures : Bag(Term)) : {Operator::Item::Any, Term::Dict::ItemsView}
      item = feed.first

      Term.case(item, engine: M0) do
        matchpi %[(%singular child_)], cue: :"%singular" do
          {Operator::Item::Singular.new(M1.operator(child, captures)), feed.move(1)}
        end

        matchpi %{(%group successor_ _ _*)}, cue: :"%group" do
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }

          members = sequence(item.items.move(2), neighbor, captures)

          {Operator::Item::Group.new(M1.operator(successor, captures), members.to_readonly_slice(&.itself)), feed.move(1)}
        end

        matchpi %{(%many successor_ _ _* ¦ min: min0_ max: max0_)}, cue: :"%many" do
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

          outer = captures
          inner = Bag(Term).new

          # [%many successor_ ⏏_ _*]
          sequence = item.items.move(2)
          sequence.each { |node| M1.captures(node, storage: inner) }

          exterior = inner & (outer - inner)
          interior = inner - exterior

          members = sequence(item.items.move(2), -> { nil.as(Neighbor) }, captures)

          {Operator::Item::Many.new(M1.operator(successor, captures), members.to_readonly_slice(&.itself), interior.set, min, max), feed.move(1)}
        end

        match({:"%partition", {:"%past", :_, :"_*"}, {min: :min0_, max: :max0_, greedy: :greedy_boolean}}, cue: :"%past") do |min0, max0, greedy|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

          members = sequence(item.items.move(1), -> { nil.as(Neighbor) }, captures)

          if greedy.true?
            {Operator::Item::PastGreedy.new(members.to_readonly_slice(&.itself), min, max), feed.move(1)}
          else
            {Operator::Item::PastLazy.new(members.to_readonly_slice(&.itself), min, max), feed.move(1)}
          end
        end

        match({:"%optional", :default_, :body_}, cue: :"%optional") do |default, body|
          {Operator::Item::Optional.new(default, M1.operator(body, captures)), feed.move(1)}
        end

        matchpi %[(%plural (%capture _) min: min0_ max: max0_ type: type0_)] do
          min = min0.to(Magnitude)
          max = max0 == SYM_INF ? Magnitude::INFINITY : max0.to(Magnitude)
          type = type0.as_sym.blank.type

          contenders = [] of Term?

          # Note: types, min, and max values of all contenders are all equal.
          # Note: cases such as `xs_ xs_* ys_*` ARE NOT instances of contention (whereas
          # e.g. `xs_* ys_*` is). xs_* and ys_*  in such scenarios must be treated
          # as separate polyblanks!
          while (contender = feed.first?) && contender.without(1) == item.without(1)
            capture = contender[1, 1]?

            if capture.nil? || captures.tally(capture) == 1
              feed = feed.move(1)
              contenders << capture
              next
            end

            if contenders.empty?
              feed = feed.move(1)
              contenders << capture
            end

            break
          end

          neighbor = -> { Item.neighbor?(feed, outside).as(Neighbor) }
          follower, frac = follower(neighbor), frac(neighbor)

          {Operator::Item::Plural.new(contenders.to_readonly_slice, min, max, type, follower, frac, strategy: :auto), feed}
        end

        match(Term[:"%plural/min", {:"%capture", :capture_}, min: :min0_, max: :max0_, type: :type0_symbol], cue: {:"%plural/min", :"%capture"}) do |capture, min0, max0, type0|
          min = min0.to(Magnitude)
          max = max0 == SYM_INF ? Magnitude::INFINITY : max0.to(Magnitude)
          type = type0.unsafe_as_sym.blank.type

          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          follower, frac = follower(neighbor), frac(neighbor)

          {Operator::Item::Plural.new(capture, min, max, type, follower, frac, strategy: :lazy), feed.move(1)}
        end

        match(Term[:"%plural/max", {:"%capture", :capture_}, min: :min0_, max: :max0_, type: :type0_symbol], cue: {:"%plural/max", :"%capture"}) do |capture, min0, max0, type0|
          min = min0.to(Magnitude)
          max = max0 == SYM_INF ? Magnitude::INFINITY : max0.to(Magnitude)
          type = type0.unsafe_as_sym.blank.type

          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          follower, frac = follower(neighbor), frac(neighbor)

          {Operator::Item::Plural.new(capture, min, max, type, follower, frac, strategy: :greedy), feed.move(1)}
        end

        # FIXME: unify with %capture %plural case, tests
        match(Term[:"%plural", min: :min0_, max: :max0_, type: :type0_symbol], cue: :"%plural") do |min0, max0, type0|
          min = min0.to(Magnitude)
          max = max0 == SYM_INF ? Magnitude::INFINITY : max0.to(Magnitude)
          type = type0.unsafe_as_sym.blank.type

          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          follower, frac = follower(neighbor), frac(neighbor)

          {Operator::Item::Plural.new(nil, min, max, type, follower, frac, strategy: :auto), feed.move(1)}
        end

        match(Term[:"%plural/min", min: :min0_, max: :max0_, type: :type0_symbol], cue: :"%plural/min") do |min0, max0, type0|
          min = min0.to(Magnitude)
          max = max0 == SYM_INF ? Magnitude::INFINITY : max0.to(Magnitude)
          type = type0.unsafe_as_sym.blank.type

          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          follower, frac = follower(neighbor), frac(neighbor)

          {Operator::Item::Plural.new(nil, min, max, type, follower, frac, strategy: :lazy), feed.move(1)}
        end

        match(Term[:"%plural/max", min: :min0_, max: :max0_, type: :type0_symbol], cue: :"%plural/max") do |min0, max0, type0|
          min = min0.to(Magnitude)
          max = max0 == SYM_INF ? Magnitude::INFINITY : max0.to(Magnitude)
          type = type0.unsafe_as_sym.blank.type

          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          follower, frac = follower(neighbor), frac(neighbor)

          {Operator::Item::Plural.new(nil, min, max, type, follower, frac, strategy: :greedy), feed.move(1)}
        end

        match({:"%gap", :measurer_}, cue: :"%gap") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::GapFirst.new(M1.operator(measurer, captures), frac, strategy: :sway), feed.move(1)}
        end

        match({:"%gap/min", :measurer_}, cue: :"%gap/max") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::GapFirst.new(M1.operator(measurer, captures), frac, strategy: :lazy), feed.move(1)}
        end

        match({:"%gap/max", :measurer_}, cue: :"%gap/max") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::GapFirst.new(M1.operator(measurer, captures), frac, strategy: :greedy), feed.move(1)}
        end

        match({:"%gap°", :measurer_}, cue: :"%gap°") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::GapSource.new(M1.operator(measurer, captures), frac, strategy: :sway), feed.move(1)}
        end

        match({:"%gap/min°", :measurer_}, cue: :"%gap/max°") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::GapSource.new(M1.operator(measurer, captures), frac, strategy: :lazy), feed.move(1)}
        end

        match({:"%gap/max°", :measurer_}, cue: :"%gap/max°") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::GapSource.new(M1.operator(measurer, captures), frac, strategy: :greedy), feed.move(1)}
        end

        match({:"%slot", :capture_}, cue: :"%slot") do |capture|
          {Operator::Item::Slot.new(capture), feed.move(1)}
        end
      end
    end
  end

  # If a person has trouble understanding a metaphor and grasping its intended meaning,
  # they do not "crash"; they change their perspective and interpret it more literally.
  # This is a spectrum: from close-to-the-intended meaning to letter-by-letter or sound-
  # by-sound.
  #
  # Similarly, if the pattern engine cannot recognize the intended meaning of some pattern
  # term, it will simply go "one layer of meaning below" and interpret the term more literally;
  # regardless of the amounts of confusion this creates (like in the real world with metaphors).
  #
  # Unfortunately, yes, this will inevitably cause problems at some point; and even bugs.
  # This can be fixed, however, by diagnostics during normalization. We plan on adding those.
  # Any pattern, even an invalid one, has a meaning for the pattern matching engine. This is
  # a hard rule. There must be no such thing as a "pattern matching engine crash" (minus the
  # inevitable implementation errors). Diagnostics can help the programmer find potential
  # mistakes at their level of reasoning.
  def self.normal(pattern : Term, **kwargs) : Term
    Normal.pattern(Normal::Context.new(**kwargs), pattern)
  end

  def self.normal_escaped(pattern : Term, **kwargs) : Term
    Term.of_case(pattern) do
      matchpi %{_dict} do
        side = Term::Dict.build do |commit|
          pattern.each_entry do |key, value|
            commit.with(key, {:"%entry/required", normal_escaped(value)})
          end
        end

        {:"%layer", Term[], side}
      end

      otherwise { {:"%literal", pattern} }
    end
  end

  def self.bounds(normp : Term) : {Magnitude, Magnitude}
    Bounds.pattern(normp)
  end

  alias OptLevel = O0.class | O1.class | O2.class

  # No optimizations. Raw output of `M1.normal`.
  module O0
  end

  module O1
    # :nodoc:
    #
    # TODO: We should probably use O2-only here as engine; and in O2, we should use O1-only.
    module Engine
      extend self

      def match?(pattern : Term, matchee : Term, *, env = Term[]) : Term::Dict?
        return unless M1next.probably_matches?(pattern, matchee, opt: O0)

        M1next.match?(pattern, matchee, opt: O0, env: env)
      end
    end

    # Precedence of an optimization, with higher levels being closer to zero, and lower
    # levels being closer to 127. You can think of this as the order of optimizations
    # relative to each other.
    enum Precedence : Int8
      # Highest

      Sketch
      Bounds
      Depth

      # Lowest
    end

    # TODO: cache
    # TODO: move to M1.sketch like we have M1.bounds
    # TODO: we should probably use M0 here like we do in M1.bounds. No need to worsen
    # the circularity
    private def self.sketch(normp : Term) : Term::Dict::Sketch
      sketch = Term::Dict::Sketch.new(0)

      M1.walk(normp) do |node|
        # Whitelist certain nodes. All other nodes we avoid. We are defensive because sketch
        # won't work for all nodes. Thus we only calculate it for nodes where we're sure it's
        # going to work.
        Term.case(node, engine: Engine) do
          # The reason we need this matchpi right now is that term's sketch is dirty
          # and contains remains of term's past. When we are going to be able
          # to update sketch on deletion this matchpi should go away.
          matchpi %[(%'%literal term_dict)], cue: :"%literal" do
            sketch |= term.fresh_sketch

            WalkDecision::Continue
          end

          matchpi %[(%'%literal term_)], cue: :"%literal" do
            sketch = Term::Dict.mix(sketch, term)

            WalkDecision::Continue
          end

          matchpi %[(%'%sketch _ sketch0_number)], cue: :"%sketch" do
            sketch |= sketch0.to(Term::Dict::Sketch)

            # We've already computed the sketch for this part of the tree. Move on.
            WalkDecision::Skip
          end

          # These have key as their first argument and we don't want to include the key
          # in the sketch.
          matchpi(
            %{[%'%entries/first _ _*]},
            %{[%'%entries/source _ _*]},
            %{[%'%entries/all _ _ _*]},
            %{[%'%items/all _ _ _*]},
            %{[%'%leaves/all _ _ _*]},
            cues: {:"%entries/first", :"%entries/source", :"%entries/all", :"%items/all", :"%leaves/all"}
          ) do
            successors = normp.items.move(2)
            successors.each do |successor|
              sketch |= sketch(successor)
            end

            WalkDecision::Skip
          end

          matchpi(
            %{[(%any %layer
                     %singular
                     %partition
                     %itemseq
                     %items/first
                     %items/source
                     %let
                     %all
                     %entry/required)
                _*]},
            %{(%'%leaves/first _* ¦ _)},
            %{(%'%leaves/source _* ¦ _)},
            cues: {nil, :"%leaves/first", :"%leaves/source"}
          ) do
            WalkDecision::Continue
          end

          otherwise { WalkDecision::Skip }
        end
      end

      sketch
    end

    def self.sketches(normp, templates) : Nil
      keypath = [] of Term

      M1.walk(normp, keypath: keypath) do |node|
        Term.case(node, engine: Engine) do
          matchpi(
            %{[(%any %partition
                     %itemseq
                     %layer
                     %items/first
                     %items/source
                     %items/all
                     %entries/first
                     %entries/source
                     %entries/all
                     %all)
                _*]},
            %{(%'%leaves/first _* ¦ _)},
            %{(%'%leaves/source _* ¦ _)},
            %{(%'%leaves/all _* ¦ _)},
            cues: {nil, :"%leaves/first", :"%leaves/source", :"%leaves/all"}
          ) do
            sketch = sketch(node)
            next if sketch.zero?

            templates << {keypath.to_readonly_slice.dup, Term.of(:"%sketch", :_, sketch), Precedence::Sketch}
          end

          otherwise { }
        end

        WalkDecision::Continue
      end
    end

    def self.bounds(normp, templates) : Nil
      keypath = [] of Term

      M1.walk(normp, keypath: keypath) do |node|
        Term.case(node, engine: Engine) do
          matchpi %{[(%any %partition %itemseq %layer %items/first %items/source %items/all) _*]} do
            min, max = M1.bounds(node)
            min = min == Magnitude::INFINITY ? SYM_INF : min
            max = max == Magnitude::INFINITY ? SYM_INF : max

            # If `max` is unknown and `min` is unknown or 0, this amounts to not checking
            # the bounds. In such cases it is pointless to emit %bounds.
            next if min.in?(0, SYM_INF) && max == SYM_INF

            templates << {keypath.to_readonly_slice.dup, Term.of(:"%bounds", :_, min: min, max: max), Precedence::Bounds}
          end

          otherwise { }
        end

        WalkDecision::Continue
      end
    end

    def self.depths(normp, templates) : Nil
      keypath = [] of Term

      M1.walk(normp, keypath: keypath) do |node|
        Term.case(node, engine: Engine) do
          matchpi %{[(%any %itemseq %layer %items/first %items/source %items/all %leaves/first %leaves/source %leaves/all) _*]} do
            min, max = M1.depth(node)
            min = min == Magnitude::INFINITY ? SYM_INF : min
            max = max == Magnitude::INFINITY ? SYM_INF : max

            # Drop {0, ∞}, {∞, ∞}, {1, ∞} depths. The first two are clearly useless, and
            # the last one is almost always true, being equivalent to an "is dict" check
            # which we've already presumably done twice-ish with %sketch and %bounds.
            next if min.in?(0, 1, SYM_INF) && max == SYM_INF

            templates << {keypath.to_readonly_slice.dup, Term.of(:"%depth", :_, min: min, max: max), Precedence::Depth}
          end

          otherwise { }
        end

        WalkDecision::Continue
      end
    end

    def self.optimized(normp : Term) : Term
      templates = [] of {Slice(Term), Term, Precedence}

      O1.sketches(normp, templates)
      O1.bounds(normp, templates)
      O1.depths(normp, templates)

      # Modify deepest keypaths first. Since we're only going to replace at the keypath
      # and do nothing else, no further sorting (e.g. by indices) is necessary.
      #
      # Since we're wrapping, we'll sort descending on precedence. This way, highest
      # precedence gets outermost position.
      templates.unstable_sort_by! { |keypath, _, prec| {-keypath.size, -prec.value} }
      templates.each do |keypath, template|
        normp = normp.as_d.follow(keypath) do |node|
          Term.of(template.subst(Term["_": node]))
        end
      end

      Term.of(normp)
    end
  end

  # O2-level optimizations involve a rewrite loop of the normal pattern. In a series
  # of rewrites, the normal pattern is reduced to the minimum possible, most
  # concrete operators at the cost of compile time.
  module O2
    module Engine
      def self.match?(pattern : Term, matchee : Term, *, env = Term[]) : Term::Dict?
        return unless M1next.probably_matches?(pattern, matchee, opt: O1)

        M1next.match?(pattern, matchee, opt: O1, env: env)
      end
    end

    def self.optimized1(normp : Term, cycle : Int, *, recurse = true) : Term
      Term.of_case(normp, engine: Engine) do
        # (%all) should be rewritten into (%pass).
        #
        # These are internal rewrites, the user cannot reach this from the outside since during
        # normalization such %alls are eliminated.
        matchpi %[(%'%all)] do
          {:"%pass"}
        end

        # (%all X) should be rewritten into X.
        #
        # Ditto about reachability from the client-side.
        matchpi %[(%'%all successor_)] do
          optimized1(successor, cycle)
        end

        # (%all X X) should be rewritten into X.
        matchpi %[(%'%all successor_ successor_)] do
          optimized1(successor, cycle)
        end

        # (%all X Y Zs) should be rewritten into (%all (%all X Y) Zs)
        #
        # This is unreachable from the client-side, and only reachable via emission from optimized1
        # itself. This is because client-side %alls are already normalized into binary %alls.
        matchpi %[(%'%all x_ y_ zs_+)] do
          Term::Dict.build do |commit|
            commit << :"%all" << {:"%all", optimized1(x, cycle), optimized1(y, cycle)}
            commit.concat(zs.items) { |z| optimized1(z, cycle) }
          end
        end

        # Fold %sketch -> %bounds -> %depth into a single operator, %dict-guard.
        matchpi(
          %[(%sketch
              (%bounds
                (%depth successor_
                  min: min_d_
                  max: max_d_)
                min: min_b_
                max: max_b_)
              sketch_number)],
        ) do
          Term.of(:"%dict-guard", successor, sketch: sketch, bounds: {min_b, max_b}, depth: {min_d, max_d})
        end

        # Fold (_*) into an itemsonly check (which is vastly cheaper!)
        matchpi %[(%itemseq (%plural min: 0 max: ∞ type: %'_))] do
          {:"%itemsonly"}
        end

        # Rewrite (xs_*) into %let/itemsonly (which is cheaper).
        matchpi %[(%itemseq (%plural capture_ min: 0 max: ∞ type: %'_))] do
          {:"%let/itemsonly", capture}
        end

        # Rewrite bounds-checked plural such as (_+) similarly into an itemsonly check since
        # the bounds check already checks what the plural would have.
        matchpi %{[%bounds (%itemseq (%plural min: _ max: _ type: %'_))]} do
          normp.morph({1, {:"%itemsonly"}})
        end

        # Ditto but for named plurals.
        matchpi %{[%bounds (%itemseq (%plural capture_ min: _ max: _ type: %'_))]} do
          normp.morph({1, {:"%let/itemsonly", capture}})
        end

        # Rewrite (¦ _) = (%partition () _) into a pairsonly check (which is vastly cheaper!)
        matchpi %[(%'%partition (%'%literal ()) (%pass))] do
          {:"%pairsonly"}
        end

        # Rewrite (¦ xs_) = (%partition () xs_) = (%partition () (%let xs _))
        # into xs←(%partition () _).
        matchpi %[(%'%partition (%'%literal ()) (%'%let capture_ (%pass)))] do
          # (%let _ (%pass)) -> (%pass)
          normp1 = normp.morph({2, normp[2, 2]})

          {:"%let", capture, normp1}
        end

        # Rewrite (%partition (%pass) _) into (%partition (%itemsonly) _), enabling
        # further rewrites if possible.
        matchpi %[(%'%partition (%pass) _)] do
          normp.morph({1, {:"%itemsonly"}})
        end

        # Fold e. g. (x_ y_ z_) into a singular-only itemspart. This lets us render it as
        # a more efficient Operator later on.
        matchpi %[(%itemseq (%past (%singular _) min: 1))] do
          Term::Dict.build do |commit|
            commit << :"%itemseq/singular-only"

            singulars = normp.items.move(1)
            singulars.each do |(_, item)|
              commit << optimized1(item, cycle)
            end
          end
        end

        # Fold e.g. (_ _ ... _ _*) into a %prefix operator that skips matching `_*`,
        # a relatively expensive affair.
        matchpi %[(%itemseq (%past (%singular _) min: 1) (%plural min: 0 max: ∞ type: %'_))] do
          Term::Dict.build do |commit|
            commit << :"%prefix"

            prefix = normp.items.move(1).grow(-1)
            prefix.each do |(_, item)|
              commit << optimized1(item, cycle)
            end
          end
        end

        # Fold e.g. (_* _ ... _ _) into a %postfix operator that skips matching `_*`,
        # a relatively expensive affair.
        matchpi %[(%itemseq (%plural min: 0 max: ∞ type: %'_) (%past (%singular _) min: 1))] do
          Term::Dict.build do |commit|
            commit << :"%postfix"

            prefix = normp.items.move(2)
            prefix.each do |(_, item)|
              commit << optimized1(item, cycle)
            end
          end
        end

        # Fold (%bounds (_ ... _ _* _ ... _) ...) (circumfix) into an %all of a %bounds %prefix
        # and a same-%bounds %postfix.
        matchp(
          %{[%bounds
              (%itemseq (%group prefix_ (%past/max (%singular _) min: 1))
                        (%plural min: 0 max: ∞ type: %'_)
                        (%group postfix_ (%past/max (%singular _) min: 1)))]}
        ) do |prefix, postfix|
          op_prefix = Term::Dict.build do |commit|
            commit << :"%prefix"
            commit.concat(prefix.items) { |(_, item)| optimized1(item, cycle) }
          end

          op_postfix = Term::Dict.build do |commit|
            commit << :"%postfix"
            commit.concat(postfix.items) { |(_, item)| optimized1(item, cycle) }
          end

          {:"%all", normp.morph({1, op_prefix}), normp.morph({1, op_postfix})}
        end

        # When we have a %prefix or %postfix of (%pass)es, e.g. (_ _ _*), that's basically
        # a bounds check and nothing more. So if we have a bounds check around it, we can
        # replace the %prefix/%postfix with a (%pass).
        matchpi %{[%bounds ((%any %prefix %postfix) (%past (%pass) min: 1))]} do
          normp.morph({1, {:"%pass"}})
        end

        # With %itemseq/singular-only that consists entirely of %passes it works similarly,
        # although we must use %itemsonly rather than %pass to make sure the bounds are
        # talking about the right part of the dict.
        matchpi %{[%bounds (%itemseq/singular-only (%past (%pass) min: 1))]} do
          normp.morph({1, {:"%itemsonly"}})
        end

        # A %prefix inside %bounds that ends with some number of %passes should have those
        # passes omitted.
        matchpi %{[%bounds ((%group successor_ %prefix (%plural/min min: 1)) (%past (%pass) min: 1))]} do |successor|
          normp.morph({1, successor})
        end

        # A %postfix inside %bounds that ends with some number of %passes should have those
        # passes omitted.
        matchpi %{[%bounds (%postfix (%past (%pass) min: 1) (%plural/max successors min: 1))]} do |successors|
          normp.morph({1, successors.prepend(:"%postfix")})
        end

        # An bounded %itemseq/singular-only that has 50% or more of %passes should be rewritten
        # into an %all of %value fetches.
        matchpi %{[%bounds (%itemseq/singular-only args_+)]} do
          continue unless normp.probably_includes?(Term[:"%pass"])

          passes = args.items.count(Normal::PASS)

          continue unless passes/args.itemsize >= 0.5

          successor = Term::Dict.build do |commit|
            commit << :"%all"

            args.items.each_with_index do |arg, index|
              next if arg == Normal::PASS

              commit << {:"%value", {:"%literal", index}, arg}
            end
          end

          normp.morph({1, successor})
        end

        # When we have (%let _ (%dict-guard ...)), that's rather awkward since the guard
        # could have rejected and we've already had an allocation etc. In such situations
        # it is wiser to invert -- into (%dict-guard (%let _ ...)).
        matchpi %[(%'%let capture_ [%dict-guard successor_])] do
          _, _, guard = normp

          guard.morph({1, {:"%let", capture, optimized1(successor, cycle)}})
        end

        # For less lucky dictionaries/other operators that do not have %dict-guard
        # but do have a %sketch, we wait out for one cycle to see if this %sketch
        # turns into a %dict-guard. If it does not we do the same as above.
        matchpi %[(%'%let capture_ (%sketch successor_ _number))] do
          continue if cycle.zero?

          _, _, guard = normp

          guard.morph({1, {:"%let", capture, optimized1(successor, cycle)}})
        end

        # # Open %layer all entries of which are (%entry/required) should turn into
        # # an %all of (%value (%literal key) value) which we render as lookups rather
        # # than letting allocation-heavy %layer logic manage them.
        # matchp %[(%'%layer (%pass) side←(%entries required_ key_ (%entry/required value_)))] do |side, required|
        #   continue unless required.size == side.size

        #   Term::Dict.build do |commit|
        #     commit << :"%all"

        #     required.each_item_unordered do |match|
        #       commit << {:"%value", {:"%literal", match[:key]}, optimized1(match[:value], cycle)}
        #     end
        #   end
        # end

        # (%bounds min: 1 max: ∞) around a single %value has low information content.
        # Remove it.
        matchpi %[(%bounds successor←(%'%value _ _) min: 1 max: ∞)] do
          optimized1(successor, cycle)
        end

        # (%partition (%itemsonly) (%pairsonly)) -> (%dict)
        matchpi %[(%'%partition (%itemsonly) (%pairsonly))] do
          {:"%dict"}
        end

        # # (%partition (%itemsonly) (%value (%literal ...) ...)) -> (%value (%literal ...) ...)
        # #
        # # Similarly for %all of such %values. We cannot do that for %layer or generic %value
        # # etc. because that'd change what the pattern means. Note also how we match the type
        # # of the value. If one does e.g. (%partition (_*) (%value 0 x_)), unless this check is
        # # in place, one would get an assignment for x: ... which shouldn't be possible. If
        # # the key is numeric we resort to the slower path.
        # matchpi(
        #   %[(%'%partition (%itemsonly) successor←(%'%value (%'%literal (%any° _string _symbol _boolean _dict)) _))],
        #   %[(%'%partition (%itemsonly)
        #       successor←(%'%all (%past min: 1 (%'%value (%'%literal (%any° _string _symbol _boolean _dict)) _))))],
        # ) do
        #   optimized1(successor, cycle)
        # end

        # Omit inner itemspart bounds if they are the same as %partition's.
        matchpi %[(%bounds (%'%partition (%bounds successor_ min: min_ max: max_) _) min: min_ max: max_)] do
          normp.morph({1, 1, optimized1(successor, cycle)})
        end

        # Omit inner pairspart bounds if they are the same as %partition's.
        matchpi %[(%bounds (%'%partition _ (%bounds successor_ min: min_ max: max_)) min: min_ max: max_)] do
          normp.morph({1, 2, optimized1(successor, cycle)})
        end

        # These nodes are terminal nodes for `M1.walk` and for us.
        # TODO: more nodes here?
        matchpi %[(%terminal node_)] do
          optimized1(node, cycle, recurse: false)
        end

        matchpi %[(%'%literal _)], %[(%'%slot _)], %[(%capture _)], %[(%barrier _)] do
          normp
        end

        # Otherwise we recurse.
        matchpi %[_dict] do
          continue unless recurse

          normp1 = normp
          normp.each_entry do |k, v|
            normp1 = normp1.with(k, optimized1(v, cycle))
          end

          normp1
        end

        otherwise { normp }
      end
    end

    def self.optimized(normp normp0 : Term) : Term
      (0..).each do |cycle|
        normp1 = optimized1(normp0, cycle)
        if normp0 == normp1
          return normp0
        end
        normp0 = normp1
      end
    end
  end

  # Applies optimizations of *level* and lower to *normp*. Returns the optimized *normp*.
  def self.optimized(normp : Term, level : O0.class) : Term
    normp
  end

  # :ditto:
  def self.optimized(normp : Term, level : O1.class) : Term
    level.optimized(normp)
  end

  # :ditto:
  def self.optimized(normp : Term, level : O2.class) : Term
    pipe(normp, optimized(O1), level.optimized)
  end

  def self.search_part(term : Term) : Search::Part
    case term
    when Term.of(:items)   then Search::Part::Items
    when Term.of(:pairs)   then Search::Part::Pairs
    when Term.of(:entries) then Search::Part::Entries
    else
      raise ArgumentError.new
    end
  end

  module Pair
    def self.operator(key, value, captures)
      Term.case(value, engine: M0) do
        matchpi %{(%entry/required (%pass))}, cue: {:"%entry/required", :"%pass"} do
          Operator::Entry::Present.new(key, type: :any)
        end

        matchpi %{(%entry/required (%number _))}, cue: {:"%entry/required", :"%number"} do
          Operator::Entry::Present.new(key, type: :number)
        end

        matchpi %{(%entry/required (%symbol))}, cue: {:"%entry/required", :"%symbol"} do
          Operator::Entry::Present.new(key, type: :symbol)
        end

        matchpi %{(%entry/required (%string))}, cue: {:"%entry/required", :"%string"} do
          Operator::Entry::Present.new(key, type: :string)
        end

        matchpi %{(%entry/required (%boolean))}, cue: {:"%entry/required", :"%boolean"} do
          Operator::Entry::Present.new(key, type: :boolean)
        end

        matchpi %{(%entry/required (%dict))}, cue: {:"%entry/required", :"%boolean"} do
          Operator::Entry::Present.new(key, type: :dict)
        end

        match({:"%entry/required", :value_}, cue: :"%entry/required") do |value|
          Operator::Entry::Required.new(key, M1.operator(value, captures))
        end

        match({:"%entry/optional", {:"%barrier", :default_}, :value_}, cue: :"%entry/optional") do |default, value|
          Operator::Entry::Optional.new(key, default, M1.operator(value, captures))
        end

        match({:"%entry/negative", {:"%pass"}}, cue: :"%entry/negative") do
          Operator::Entry::Absent.new(key)
        end

        match({:"%entry/negative", {:"%pass"}, {:"%barrier", :name_}}, cue: :"%entry/negative") do |name|
          Operator::Entry::AbsentKeypath.new(key, name)
        end

        match({:"%entry/negative", :positive_}, cue: :"%entry/negative") do |positive|
          Operator::Entry::Negative.new(key, M1.operator(positive, captures))
        end

        match({:"%entry/negative", :positive_, {:"%barrier", :name_}}, cue: :"%entry/negative") do |positive, name|
          Operator::Entry::NegativeKeypath.new(key, M1.operator(positive, captures), name)
        end
      end
    end
  end

  def self.operator(node : Term, captures : Bag(Term)) : Operator::Any
    Term.case(node, engine: M0) do
      matchpi %[(%'%let (%capture capture_) successor_)], cue: :"%let" do
        Operator::Capture.new(capture, operator(successor, captures))
      end

      matchpi %[(%dict-guard successor_ sketch: sketch_ bounds: (min_b_ max_b_) depth: (min_d_ max_d_))], cue: :"%dict-guard" do
        Operator::DictGuard.new(
          sketch: sketch.to(Term::Dict::Sketch),
          bounds: {
            min_b == SYM_INF ? Magnitude::INFINITY : min_b.to(Magnitude),
            max_b == SYM_INF ? Magnitude::INFINITY : max_b.to(Magnitude),
          },
          depth: {
            min_d == SYM_INF ? Magnitude::INFINITY : min_d.to(Magnitude),
            max_d == SYM_INF ? Magnitude::INFINITY : max_d.to(Magnitude),
          },
          successor: operator(successor, captures)
        )
      end

      matchpi %[(%sketch successor_ sketch_number)], cue: :"%sketch" do
        Operator::SketchSubset.new(sketch.to(Term::Dict::Sketch), operator(successor, captures))
      end

      matchpi %[(%bounds (%pass) min: min_b_ max: max_b_)], cue: {:"%bounds", :"%pass"} do
        min = min_b == SYM_INF ? Magnitude::INFINITY : min_b.to(Magnitude)
        max = max_b == SYM_INF ? Magnitude::INFINITY : max_b.to(Magnitude)

        Operator::Bounds.new(min, max)
      end

      matchpi %[(%bounds successor_ min: min_b_ max: max_b_)], cue: :"%bounds" do
        min = min_b == SYM_INF ? Magnitude::INFINITY : min_b.to(Magnitude)
        max = max_b == SYM_INF ? Magnitude::INFINITY : max_b.to(Magnitude)

        Operator::BoundsGuard.new(min, max, operator(successor, captures))
      end

      matchpi %[(%depth successor_ min: min_d_ max: max_d_)], cue: :"%depth" do
        min = min_d == SYM_INF ? Magnitude::INFINITY : min_d.to(Magnitude)
        max = max_d == SYM_INF ? Magnitude::INFINITY : max_d.to(Magnitude)

        Operator::MaxDepth.new(min, max, operator(successor, captures))
      end

      matchpi %[(%itemseq/singular-only _ _*)], cue: :"%itemseq/singular-only" do
        items = node.items.move(1).to_readonly_slice { |item| operator(item, captures) }

        Operator::SingularSeq.new(items, exhaustive: true, reverse: false)
      end

      matchpi %[(%prefix successor_)], cue: :"%prefix" do
        Operator::ItemFirst.new(operator(successor, captures))
      end

      matchpi %[(%prefix _ _*)], cue: :"%prefix" do
        items = node.items.move(1).to_readonly_slice { |item| operator(item, captures) }

        Operator::SingularSeq.new(items, exhaustive: false, reverse: false)
      end

      matchpi %[(%postfix successor_)], cue: :"%postfix" do
        Operator::ItemLast.new(operator(successor, captures))
      end

      matchpi %[(%postfix _ _*)], cue: :"%postfix" do
        items = node.items.move(1).to_readonly_slice { |item| operator(item, captures) }

        Operator::SingularSeq.new(items, exhaustive: false, reverse: true)
      end

      # TODO: rename itemseq to %seq, there is no non-item seq.
      matchpi %[(%itemseq _*)], cue: :"%itemseq" do
        items = Item.sequence(node.items.move(1), -> { nil.as(Item::Neighbor) }, captures)
        slice = items.to_readonly_slice(&.itself)

        Operator::ItemSeq.new(slice)
      end

      matchpi %[(%itemsonly)], cue: :"%itemsonly" do
        Operator::Itemsonly.new
      end

      matchpi %[(%pairsonly)], cue: :"%pairsonly" do
        Operator::Pairsonly.new
      end

      matchpi %[(%let/itemsonly (%capture capture_))], cue: :"%let/itemsonly" do
        Operator::CaptureItemsonly.new(capture)
      end

      matchpi %[(%pass)], cue: :"%pass" do
        Operator::INSTANCE_PASS
      end

      matchpi %[(%'%literal term_)], cue: :"%literal" do
        Operator::Literal.new(term)
      end

      matchpi %[(%'%partition itemspart_ pairspart_)], cue: :"%partition" do
        Operator::Partition.new(
          operator(itemspart, captures),
          operator(pairspart, captures),
        )
      end

      match({:"%string"}, cue: :"%string") { Operator::INSTANCE_STR }
      match({:"%symbol"}, cue: :"%symbol") { Operator::INSTANCE_SYM }
      match({:"%boolean"}, cue: :"%boolean") { Operator::INSTANCE_BOOLEAN }
      match({:"%dict"}, cue: :"%dict") { Operator::INSTANCE_DICT }

      match({:"%keypath", {:"%capture", :capture_}}, cue: :"%keypath") do |capture|
        Operator::KeypathCapture.new(capture)
      end

      match({:"%keypool", :_, :"_*"}, cue: :"%keypool") do
        keys = node.items.move(1)

        Operator::Keypool.new(keys.to_readonly_slice(&.itself))
      end

      matchpi %[(%-keypool)], cue: :"%-keypool" do
        Operator::INSTANCE_DICT
      end

      matchpi %[(%-keypool _*)], cue: :"%-keypool" do
        keys = node.items.move(1)

        Operator::NegativeKeypool.new(keys.to_readonly_slice(&.itself))
      end

      matchpi %[(%keytest)], cue: :"%keytest" do
        Operator::INSTANCE_DICT
      end

      matchpi %[(%keytest _*)], cue: :"%keytest" do
        keys = node.items.move(1)

        Operator::Keytest.new(keys.to_readonly_slice(&.itself))
      end

      match({ {:"%literal", :"%layer"}, :below_, :side_ }, cue: :"%layer") do |below, side|
        entries = Array(Operator::Entry::Any).new(side.size)

        side.each_entry do |k, v|
          entries << Pair.operator(k, v, captures)
        end

        # Lowest cost goes first.
        entries.unstable_sort_by!(&.cost)

        Operator::Layer.new(operator(below, captures), entries.to_readonly_slice(&.itself))
      end

      matchpi %[(%value (%'%literal key_) value_)], cue: {:"%value", :"%literal"} do
        Operator::ValueLiteral.new(key, operator(value, captures))
      end

      match({:"%value", {:"%capture", :capture_}, :value_}, cue: :"%value") do |capture, value|
        Operator::Value.new(capture, operator(value, captures))
      end

      match({:"%-value", {:"%capture", :capture_}}, cue: :"%-value") do |capture|
        Operator::NegativeValue.new(capture)
      end

      match({:"%-value", {:"%capture", :capture_}, {:"%barrier", :name_}}, cue: :"%-value") do |capture, name|
        Operator::NegativeValueKeypath.new(capture, name)
      end

      matchpi %[(%pipe (%barrier (+ n_number)) successor_)], cue: {:"%pipe", :+} do
        Operator::Add.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (- n_number)) successor_)], cue: {:"%pipe", :-} do
        Operator::Sub.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (* n_number)) successor_)], cue: {:"%pipe", :*} do
        Operator::Mul.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (/ n_number)) successor_)], cue: {:"%pipe", :/} do
        Operator::Div.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (div n_number)) successor_)], cue: {:"%pipe", :div} do
        Operator::Idiv.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (mod n_number)) successor_)], cue: {:"%pipe", :mod} do
        Operator::Mod.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (** n_number)) successor_)], cue: {:"%pipe", :**} do
        Operator::Pow.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (clamp min_number ..= max_number)) successor_)], cue: {:"%pipe", :clamp, :"..="} do
        Operator::Clamp.new(min.unsafe_as_n, max.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (map arg_dict)) successor_)], cue: {:"%pipe", :map} do
        Operator::Map.new(arg.unsafe_as_d, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier span) successor_)], cue: {:"%pipe", :span} do
        Operator::Span.new(operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier tally) successor_)], cue: {:"%pipe", :tally} do
        Operator::Tally.new(operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier type) successor_)], cue: {:"%pipe", :type} do
        Operator::Type.new(operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier ml) successor_)], cue: {:"%pipe", :ml} do
        Operator::ParseML.new(operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier untracked) successor_)], cue: {:"%pipe", :untracked} do
        Operator::Untracked.new(operator(successor, captures))
      end

      match({:"%items/first", :_, :"_*"}, cue: :"%items/first") do
        sequence = node.items.move(1)

        Operator::ScanFirst.new(sequence.to_readonly_slice { |item| operator(item, captures).as(Operator::Any) })
      end

      match({:"%items/source", :_, :"_*"}, :"%items/source") do
        sequence = node.items.move(1)

        Operator::ScanSource.new(sequence.to_readonly_slice { |item| operator(item, captures).as(Operator::Any) })
      end

      matchpi %{(%items/all successor_ _ _* ¦ min: min0_ max: max0_)}, cue: :"%items/all" do
        min = min0.to(UInt8)
        max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        needle = sequence.to_readonly_slice { |item| operator(item, captures).as(Operator::Any) }

        Operator::ScanAll.new(operator(successor, captures), needle, inner.set, exterior.set, min, max)
      end

      matchpi %{(%entries/all successor_ k_ v_ ¦ min: min0_ max: max0_)}, cue: :"%entries/all" do
        min = min0.to(UInt8)
        max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        Operator::EntriesAll.new(operator(successor, captures),
          kop: operator(k, captures),
          vop: operator(v, captures),
          exterior: exterior.set,
          selector: inner.set,
          min: min,
          max: max,
        )
      end

      match({:"%entries/first", :k_, :v_}, cue: :"%entries/first") do |k, v|
        Operator::EntriesFirst.new(
          operator(k, captures),
          operator(v, captures),
        )
      end

      match({:"%entries/source", :k_, :v_}, cue: :"%entries/source") do |k, v|
        Operator::EntriesSource.new(
          operator(k, captures),
          operator(v, captures),
        )
      end

      matchpi %{(%leaves/first _ _* in: part_ order: dfs self: depth0_boolean)}, cue: {:"%leaves/first", :dfs} do
        seq = node.items.move(1).to_readonly_slice { |item| operator(item, captures).as(Operator::Any) }

        Operator::DfsFirst.new(seq, part: search_part(part), depth0: depth0.true?)
      end

      matchpi %{(%leaves/source _ _* in: part_ order: dfs self: depth0_boolean)}, cue: {:"%leaves/source", :dfs} do
        seq = node.items.move(1).to_readonly_slice { |item| operator(item, captures).as(Operator::Any) }

        Operator::DfsSource.new(seq, part: search_part(part), depth0: depth0.true?)
      end

      matchpi %{(%leaves/first _ _* in: part_ order: bfs self: depth0_boolean)}, cue: {:"%leaves/first", :bfs} do
        seq = node.items.move(1).to_readonly_slice { |item| operator(item, captures).as(Operator::Any) }

        Operator::BfsFirst.new(seq, part: search_part(part), depth0: depth0.true?)
      end

      matchpi %{(%leaves/all successor_ _ _* ¦ in: part_ min: min_ max: max_ order: dfs self: depth0_boolean)}, cue: {:"%leaves/all", :dfs} do |min, max|
        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        min = min.to(UInt8)
        max = max == SYM_INF ? 0u8 : max.to(UInt8)

        Operator::DfsAll.new(operator(successor, captures), sequence.to_readonly_slice { |item| operator(item, captures).as(Operator::Any) }, inner.set, exterior.set, search_part(part), min, max, depth0.true?)
      end

      matchpi %{(%leaves/all successor_ _ _* in: part_ min: min_ max: max_ order: bfs self: depth0_boolean)}, cue: {:"%leaves/all", :bfs} do |min, max|
        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        min = min.to(UInt8)
        max = max == SYM_INF ? 0u8 : max.to(UInt8)

        Operator::BfsAll.new(operator(successor, captures), sequence.to_readonly_slice { |item| operator(item, captures).as(Operator::Any) }, inner.set, exterior.set, search_part(part), min, max, depth0.true?)
      end

      match({:"%all", :a_, :b_}, cue: :"%all") do |a, b|
        Operator::Both.new(operator(a, captures), operator(b, captures))
      end

      matchpi %[(%any/literal _*)], cue: :"%any/literal" do
        branches = node.items.move(1).to_set

        Operator::LiteralWhitelist.new(branches)
      end

      match({:"%any/source", :a_}, cue: :"%any/source") do |a|
        operator(a, captures)
      end

      match({:"%any/source", :a_, :b_}, cue: :"%any/source") do |a, b|
        Operator::ChoiceSource.new(operator(a, captures), operator(b, captures))
      end

      match({:"%any/source", :a_, :_, :"_*"}, cue: :"%any/source") do |a|
        rest = Term::Dict.build do |commit|
          commit << :"%any/source"

          args = node.items.move(2)
          args.each { |item| commit << item }
        end

        Operator::ChoiceSource.new(operator(a, captures), operator(Term.of(rest), captures))
      end

      matchpi %{(%edge _symbol)}, cue: :"%edge" do
        case node
        when Normal::EDGE_ANY     then Operator::Edge.new(:any)
        when Normal::EDGE_SYMBOL  then Operator::Edge.new(:symbol)
        when Normal::EDGE_STRING  then Operator::Edge.new(:string)
        when Normal::EDGE_NUMBER  then Operator::Edge.new(:number)
        when Normal::EDGE_DICT    then Operator::Edge.new(:dict)
        when Normal::EDGE_BOOLEAN then Operator::Edge.new(:boolean)
        else
          continue
        end
      end

      match({:"%not", :_, :"_*"}, cue: :"%not") do
        blacklist = node.items.move(1)

        Operator::LiteralBlacklist.new(blacklist.to_set)
      end

      matchpi %{(%never)}, cue: :"%never" do
        Operator::INSTANCE_NEVER
      end

      match({:"%number", {:"%literal", :_}}, cue: :"%number") do
        Operator::INSTANCE_NUM
      end

      match({:"%number", {:"%literal", {:whole, :_}}}, cue: {:"%number", :whole}) do
        Operator::INSTANCE_NUM_WHOLE
      end

      matchpiT %{(%number _*)}, cue: :"%number" do
        Operator::Num.parse?(node) || continue
      end

      matchpi %[(%symbol nonblank)], cue: {:"%symbol", :"nonblank"} do
        Operator::SymNonblank.new
      end

      matchpi %[(%symbol blank name_ type_)], cue: {:"%symbol", :"blank"} do
        Operator::SymBlank.new(operator(name, captures), operator(type, captures))
      end

      matchpi %{(%filter (%barrier deps_dict) selector_ successor_ ¦ () min_ max_)}, cue: :"%filter" do
        minM = min.to(Magnitude)
        maxM = max == SYM_INF ? Magnitude::INFINITY : max.to(Magnitude)

        Operator::Filter.new(deps.items.to_pf_set, operator(selector, captures), operator(successor, captures), minM, maxM)
      end

      matchpi %{(%pluck (%barrier spec_dict) successor_)}, cue: :"%pluck" do
        Operator::Pluck.new(pluck_spec(spec.as_d), operator(successor, captures))
      end

      matchpi %{(%flat (%barrier spec_dict) successor_)}, cue: :"%flat" do
        Operator::Flat.new(flat_spec(spec.as_d), operator(successor, captures))
      end

      matchpi %{(%adjacent m←(_*))}, cue: :"%adjacent" do
        ops = m.items.to_readonly_slice { |x| operator(x, captures) }

        Operator::Adjacent.new(ops)
      end

      matchpi %{(%split/first lhs_ m←(_*) rhs_)}, cue: :"%split/first" do
        mops = m.items.to_readonly_slice { |mp| operator(mp, captures) }

        Operator::SplitFirst.new(operator(lhs, captures), mops, operator(rhs, captures))
      end

      matchpi %{(%split/source lhs_ m←(_*) rhs_)}, cue: :"%split/source" do
        mops = m.items.to_readonly_slice { |mp| operator(mp, captures) }

        Operator::SplitSource.new(operator(lhs, captures), mops, operator(rhs, captures))
      end

      matchpi %{(%split/all successor_ lhs_ m←(_*) rhs_ ¦ () min_ max_)}, cue: :"%split/all" do
        minM = min.to(Magnitude)
        maxM = max == SYM_INF ? Magnitude::INFINITY : max.to(Magnitude)
        mops = m.items.to_readonly_slice { |mp| operator(mp, captures) }

        Operator::SplitAll.new(operator(lhs, captures), mops, operator(rhs, captures), operator(successor, captures), minM, maxM)
      end

      # %terminal is used to mark terminal nodes for walk
      matchpi %[(%terminal subnode_)], cue: :"%terminal" do
        operator(subnode, captures)
      end

      otherwise { raise ArgumentError.new("BUG: cannot compile #{node}") }
    end
  end

  def self.flat_spec(spec : Indexable(Term)) : M1next::Tzip::FlatSpec
    spec.to_readonly_slice do |item|
      Term.case(item, engine: M0) do
        matchpi %{%'_} { M1next::Tzip::InItemsStep.new }
        matchpi %{.} { M1next::Tzip::InPairsStep.new }
        matchpi %{*} { M1next::Tzip::InEntriesStep.new }
        matchpi %{(merge _ _ _*)} do
          args = item.items.move(1)
          continue unless args.all? { |term| term.type.dict? && term.itemsonly? }

          M1next::Tzip::MergeStep.new(args.to_readonly_slice { |arg| flat_spec(arg.items) })
        end
        matchpi %{(keys _ _*)} { M1next::Tzip::KeyListStep.new(item.items.move(1).to_readonly_slice(&.itself)) }
        matchpi %{(key term_)} { M1next::Tzip::KeyStep.new(term) }
        otherwise { M1next::Tzip::KeyStep.new(item) }
      end.as(M1next::Tzip::FlatStep)
    end
  end

  def self.flat_spec(spec : Term::Dict) : M1next::Tzip::FlatSpec
    flat_spec(spec.items)
  end

  def self.pluck_spec(spec : Indexable(Term)) : M1next::Tzip::PluckSpec
    spec.to_readonly_slice do |item|
      Term.case(item, engine: M0) do
        matchpi %{%'_} { M1next::Tzip::InItemsStep.new }
        matchpi %{.} { M1next::Tzip::InPairsStep.new }
        matchpi %{*} { M1next::Tzip::InEntriesStep.new }
        matchpi %{(keys _ _*)} { M1next::Tzip::KeyListStep.new(item.items.move(1).to_readonly_slice(&.itself)) }
        matchpi %{(key term_)} { M1next::Tzip::KeyStep.new(term) }
        otherwise { M1next::Tzip::KeyStep.new(item) }
      end.as(M1next::Tzip::PluckStep)
    end
  end

  def self.pluck_spec(spec : Term::Dict) : M1next::Tzip::PluckSpec
    pluck_spec(spec.items)
  end

  enum WalkDecision : UInt8
    Continue
    Skip
    Halt
  end

  # Returns `true` if *id* is probably a pattern engine node id.
  def self.probably_node?(id : Term::Sym) : Bool
    id.prefixed_by?('%')
  end

  # Used as a constant to indicate that `walk` should walk thoroughly,
  # that is, it should include all %-nodes, including item sequence nodes
  # such as %singular.
  module WalkMode::Thorough
  end

  # Used as a constant to indicate that `walk` should continue into item sequence
  # nodes such as %singular without yielding them to the callback.
  module WalkMode::NonItemSeq
  end

  def self.walk(root : Term, mode : WalkMode::Thorough.class, callable, *, keypath = nil) : WalkDecision
    Term.case(root, engine: M0) do
      # Barrier is for nodes to prevent walk from walking into their arguments.
      matchpi %[(%barrier _)], cue: :"%barrier" do
        WalkDecision::Skip
      end

      # Captures, slots, and literals are all terminal nodes. We do not require to
      # wrap them in %terminal because this is sort of evident.
      matchpi(
        %{[%'%literal _]},
        %{[%slot _]},
        %{[%capture _]},
        cues: {:"%literal", :"%slot", :"%capture"},
      ) do
        callable.call(root)

        WalkDecision::Skip
      end

      # General-purpose marker for terminal nodes.
      matchpi %[(%terminal node_)], cue: :"%terminal" do
        callable.call(node)

        WalkDecision::Skip
      end

      # Call the callback on dicts that look like nodes.
      matchpi %{[head_symbol _*]} do
        if probably_node?(head.unsafe_as_sym)
          case callable.call(root)
          in .continue?
          in .skip? then return WalkDecision::Continue
          in .halt? then return WalkDecision::Halt
          end
        end

        continue
      end

      # Recurse into all dicts.
      matchpi %[_dict] do
        dict = root.unsafe_as_d
        dict.each_entry do |key, value|
          keypath.try &.push(key)

          case walk(value, mode, callable, keypath: keypath)
          in .continue?, .skip?
          in .halt?
            return WalkDecision::Halt
          end
        ensure
          keypath.try &.pop
        end

        WalkDecision::Continue
      end

      otherwise { WalkDecision::Skip }
    end
  end

  def self.walk(root : Term, mode : WalkMode::NonItemSeq.class, callable, *, itemseq : Bool = false, keypath = nil) : WalkDecision
    walk(root, mode: WalkMode::Thorough, keypath: keypath) do |node|
      if itemseq
        Term.case(node, engine: M0) do
          # Recurse into M1 non-item sequence children with itemseq flag off.
          matchpi(
            %{[%singular child_]},
            %{[%gap child_]},
            %{[%gap/min child_]},
            %{[%gap/max child_]},
            %{[%optional _ child_]},
            cues: {:"%singular", :"%gap", :"%gap/min", :"%gap/max", :"%optional"},
          ) do
            case walk(child, mode, callable, itemseq: false, keypath: keypath)
            in .continue?, .skip?
              WalkDecision::Skip
            in .halt?
              WalkDecision::Halt
            end
          end

          matchpi(
            %{[%many successor_ _ _*]},
            %{[%group successor_ _ _*]},
            cues: {:"%many", :"%group"}
          ) do
            case walk(successor, mode, callable, itemseq: false, keypath: keypath)
            in .continue?, .skip?
              WalkDecision::Continue
            in .halt?
              WalkDecision::Halt
            end
          end

          # Recurse into %group and %many with itemseq flag on.
          matchpi(
            %{[%past _ _*]},
            %{[%past/max _ _*]},
            cues: {:"%past", :"%past/max"},
          ) do
            WalkDecision::Continue
          end

          # Avoid all other item sequence nodes.
          otherwise { WalkDecision::Skip }
        end
      else
        Term.case(node, engine: M0) do
          matchpi %{[%itemseq _*]}, cue: :"%itemseq" do
            decision = callable.call(node)

            if decision.continue?
              node.each_item_with_index do |item, index|
                keypath.try &.push(Term.of(index))

                case walk(item, mode, callable, itemseq: true, keypath: keypath)
                in .continue?, .skip?
                in .halt?
                  decision = WalkDecision::Halt
                  break
                end
              ensure
                keypath.try &.pop
              end

              unless decision.halt?
                decision = WalkDecision::Skip
              end
            end

            decision
          end

          otherwise { callable.call(node) }
        end
      end
    end
  end

  def self.walk(root : Term, callable, *, mode = WalkMode::Thorough, **kwargs) : WalkDecision
    walk(root, mode, callable, **kwargs)
  end

  def self.walk(root : Term, **kwargs, &fn : Term -> WalkDecision) : WalkDecision
    walk(root, fn, **kwargs)
  end

  def self.captures(root : Term, *, storage = Bag(Term).new) : Bag(Term)
    walk(root) do |node|
      Term.case(node, engine: M0) do
        matchpi %{[%capture capture_]} do
          next if capture.type.symbol? && capture.unsafe_as_sym.prefixed_by?('\\')

          storage << capture
        end

        otherwise { }
      end

      WalkDecision::Continue
    end

    storage
  end

  PATTERN_CACHE = SyncCache({Term, Bool}, Operator::Any).new(16_384, preallocate: true)

  {% if flag?(:popt_0) %}
    DEFAULT_OPT_LEVEL = O0
  {% elsif flag?(:popt_1) %}
    DEFAULT_OPT_LEVEL = O1
  {% else %}
    DEFAULT_OPT_LEVEL = O2
  {% end %}

  # TODO: can we please get rid of these flags!!!! The caller can chain the calls manually
  # if they want to, we need to make that more comfortable than using this flags bullshit!!

  # TODO: cache pattern -> normal, normal -> optimal, optimal -> operator

  def self.operator0(pattern : Term, *, normalize = true, optimize = true, opt = DEFAULT_OPT_LEVEL) : Operator::Any
    normal = normalize ? normal(pattern) : pattern
    optimal = optimize ? optimized(normal, opt) : normal
    captures = captures(normal)

    operator(optimal, captures)
  end

  # TODO: overwrite in cache if higher opt level
  def self.operator(pattern : Term, *, fresh = false, normalize = true, **kwargs) : Operator::Any
    if fresh
      return operator0(pattern, **kwargs, normalize: normalize)
    end

    PATTERN_CACHE.put_if_absent({pattern, normalize}) do
      # Slow path: compile and add to cache. Sometimes multiple threads will do
      # multiple times the work; that's fine. We cannot block because that'd cause
      # a deadlock -- operator0() may in turn call operator() at some point and so on.
      operator0(pattern, **kwargs, normalize: normalize)
    end
  end
end

module ::Ww::M1
  # FIXME: The whole thing we've got going on with Magnitude, a float32, representing
  # depth, is stupid stupidity. Infinity works as "unbounded" but only up to a point,
  # because when one says min=∞ max=3 means min=unbounded max=3, one sounds
  # rather stupid; but notably, one doesn't when one says min=3 max=∞. Eventually
  # we'd want a custom type that represents the concept of "unbounded"-ness vs.
  # bounded-ness regardless of direction.

  defrecord XsectLike, ix : Indexable(Term)
  defrecord UnionLike, ix : Indexable(Term)

  def self.depth(subject : XsectLike)
    min = Magnitude.new(0)
    max = Magnitude.new(0)

    subject.ix.each do |item|
      item_min, item_max = depth(item)
      min = Math.max(min, item_min)
      max = Math.max(max, item_max)
    end

    {min, max}
  end

  def self.depth(subject : UnionLike)
    min = Magnitude::INFINITY # < see FIXME above
    max = Magnitude.new(0)

    subject.ix.each do |option|
      option_min, option_max = depth(option)
      min = Math.min(min, option_min)
      max = Math.max(max, option_max)
    end

    {min, max}
  end

  def self.depth(normp : Term) : {Magnitude, Magnitude}
    Term.case(normp, engine: M0) do
      matchpi %{[%pass]}, %{[%dict]}, cues: {:"%pass", :"%dict"} do
        {Magnitude.new(0), Magnitude::INFINITY}
      end

      matchpi %{[%'%literal x_dict]}, %{[%edge x_dict]}, cues: {:"%literal", :"%edge"} do
        maxdepth = x.fresh_maxdepth

        {Magnitude.new(maxdepth), Magnitude.new(maxdepth)}
      end

      matchpi(
        %{[%symbol]},
        %{[%string]},
        %{[%number %'_]},
        %{[%boolean]},
        %{[%'%literal _]},
        %{[%slot _]},
        %{[%entry/negative [%pass]]},
        %{[%entry/negative [%pass] _]},
        cues: {:"%symbol",
               :"%string",
               :"%number",
               :"%boolean",
               :"%literal",
               :"%slot",
               :"%entry/negative",
               :"%entry/negative"},
      ) do
        {Magnitude.new(0), Magnitude.new(0)}
      end

      matchpi(
        %{[%'%let _ successor_]},
        %{[%singular successor_]},
        %{[%entry/required successor_]},
        %{[%terminal successor_]},
        cues: {:"%let", :"%singular", :"%entry/required", :"%terminal"}
      ) do
        depth(successor)
      end

      matchpi(
        %{[%gap _]},
        %{[%gap/min _]},
        %{[%gap/max _]},
        %{[%entry/negative _]},
        %{[%entry/negative _ _]},
        cues: {:"%gap",
               :"%gap/min",
               :"%gap/max",
               :"%entry/negative",
               :"%entry/negative"},
      ) do
        {Magnitude.new(0), Magnitude::INFINITY}
      end

      # With %optional, our min is when the optional is not matched (0)
      # and our max is when the optional is matched (successor).
      #
      # Throw away minimum bound if %past, %past/max can match nothing.
      matchpi(
        %{[%optional _ successor_]},
        %{[%entry/optional _ successor_]},
        %{(%past successor_ ¦ _ min: 0)},
        %{(%past/max successor_ ¦ _ min: 0)},
        cues: {:"%optional", :"%entry/optional", :"%past", :"%past/max"},
      ) do
        _, max = depth(successor)

        {Magnitude.new(0), max}
      end

      # With %all, the idea is to take the max of both min depths and max depths. %all is
      # different from e.g. %itemseq in that it does not introduce depth itself.
      matchpi(
        %{[%all _*]},
        %{[%past _*]},
        %{[%past/max _*]},
        cues: {:"%all", :"%past", :"%past/max"}
      ) do
        depth(XsectLike.new(normp.items.move(1)))
      end

      # With %any and %any°, the idea is to take the min of min depths and max of
      # max depths.
      matchpi %{[%any/literal _ _*]}, cue: :"%any/literal" do
        min = Magnitude::INFINITY
        max = Magnitude.new(0)

        choices = normp.items.move(1)
        choices.each do |choice|
          unless dict = choice.as_d?
            min = Magnitude.new(0)
            next
          end

          maxdepth = dict.fresh_maxdepth
          min = Math.min(min, maxdepth)
          max = Math.max(max, maxdepth)
        end

        {min, max}
      end

      matchpi %{[%any/source _ _*]}, cue: :"%any/source" do
        depth(UnionLike.new(normp.items.move(1)))
      end

      matchpi %{[%'%partition itemspart_ pairspart_]}, cue: :"%partition" do
        depth(XsectLike.new({itemspart, pairspart}))
      end

      matchpi %{[%edge _]}, cue: :"%edge" do
        {Magnitude.new(1), Magnitude.new(1)}
      end

      # These operators don't know the max depth because other entries may
      # exist that are dicts of unknown depth. They can only tell the min depth,
      # which is its successor's expected min depth plus one for the dict that
      # the operator itself matches.
      matchpi(
        %{[%value _ successor_]},
        %{[%entries/first _ successor_]},
        %{[%entries/source _ successor_]},
        %{[%entries/all _ _ successor_]},
        cues: {:"%value", :"%entries/first", :"%entries/source", :"%entries/all"},
      ) do
        min, _ = depth(successor)

        {min + 1, Magnitude::INFINITY}
      end

      matchpi(
        %{(%leaves/first _* ⍊ self: depth0_boolean)},
        %{(%leaves/source _* ⍊ self: depth0_boolean)},
        cues: {:"%leaves/first", :"%leaves/source"}
      ) do
        # [%leaves/⸨first,source⸩ ⏏ needle_*]
        min, _ = depth(XsectLike.new(normp.items.move(1)))

        {depth0.true? ? min : min + 1, Magnitude::INFINITY}
      end

      matchpi %{(%leaves/all _* ⍊ self: depth0_boolean)}, cue: :"%leaves/all" do
        # [%leaves/all successor_ ⏏ needle_*]
        min, _ = depth(XsectLike.new(normp.items.move(2)))

        {depth0.true? ? min : min + 1, Magnitude::INFINITY}
      end

      matchpi %{[%-value _]}, %{[%-value _ _]}, cue: :"%-value" do
        {Magnitude.new(1), Magnitude::INFINITY}
      end

      matchpi(
        %{[%itemseq _*]},
        %{[%items/first _*]},
        %{[%items/source _*]},
        cues: {:"%itemseq", :"%items/first", :"%items/source"},
      ) do
        min, max = depth(XsectLike.new(normp.items.move(1)))

        {min + 1, max + 1} # + 1 for the dict itself
      end

      matchpi %{[%items/all _*]}, cue: :"%items/all" do
        # [%items/all successor_ ⏏ needle_*]
        min, max = depth(XsectLike.new(normp.items.move(2)))

        {min + 1, max + 1} # + 1 for the dict itself
      end

      # Throw away minimum bound if %many can match nothing.
      matchpi %{(%many _* ¦ _ min: 0)}, cue: :"%many" do
        max = Magnitude.new(0)

        items = normp.items.move(2)
        items.each do |item|
          _, max1 = depth(item)
          max = Math.max(max, max1)
        end

        {Magnitude.new(0), max}
      end

      matchpi %{[%group _*]}, %{[%many _*]}, cues: {:"%group", :"%many"} do
        depth(XsectLike.new(normp.items.move(2)))
      end

      matchpi %{[%'%layer below_ side_dict]}, cue: :"%layer" do
        min, max = depth(below)
        # Do not waste time computing side if that won't change anything.
        if {min, max} == {Magnitude::INFINITY, Magnitude::INFINITY}
          return min, max
        end

        side.each_entry do |_, v|
          min1, max1 = depth(v)
          min = Math.max(min, min1 + 1)
          max = Math.max(max, max1 + 1)
        end

        {min, max}
      end

      matchpi(
        %{(%plural _ ¦ _ type: type_)},
        %{(%plural/min _ ¦ _ type: type_)},
        %{(%plural/max _ ¦ _ type: type_)},
        %{(%plural ¦ _ type: type_)},
        %{(%plural/min ¦ _ type: type_)},
        %{(%plural/max ¦ _ type: type_)},
        cues: {:"%plural",
               :"%plural/min",
               :"%plural/max",
               :"%plural",
               :"%plural/min",
               :"%plural/max"}
      ) do
        if type.in?(SYM_BLANK_ANY, SYM_BLANK_DICT)
          {Magnitude.new(0), Magnitude::INFINITY}
        else
          {Magnitude.new(0), Magnitude.new(0)}
        end
      end

      otherwise do
        {Magnitude.new(0), Magnitude::INFINITY}
      end
    end
  end
end

module ::Ww::M1
  # A summary of measurements concerning the specificity of a pattern.
  alias Specificity = {UInt32, UInt32, UInt32, UInt32}

  # Specificity assigned to a top-level literal pattern such as `qux`, `(+ 1 2)`.
  SPECIFICITY_LITERAL = {UInt32::MAX, 0u32, 0u32, 0u32}

  # Specificity assigned to a top-level literal alternative, e.g. `(%any 0 1 2)`.
  SPECIFICITY_ANY = {UInt32::MAX - 1, 0u32, 0u32, 0u32}

  private def self.specificity0?(normp : Term)
    Term.case(normp) do
      # Recurse into top-level `%let`s and `%terminal`'s.
      matchpi %[(%terminal successor_)], %[(%'%let _ successor_)] do
        specificity0?(successor)
      end

      # If we have a literal or %any at the top level, issue max specificity
      # and exit immediately.
      matchpi %[(%'%literal _)] { SPECIFICITY_LITERAL }
      matchpi %[(%any/literal _+)] { SPECIFICITY_ANY }

      otherwise { }
    end
  end

  # Returns the specificity of a normal pattern *normp*.
  #
  # In single-way rewriting (which basically means most of rewriting we are doing
  # here in Wirewright), having a way to order patterns/rules is important, mostly
  # for the programmer (because the rewrite system itself does not care; all it cares
  # about is whether a rewrite is possible). Some patterns inherently "know more" about
  # their expected matchee and we must give way to those patterns vs. more general/
  # abstract ones. This way, the programmer may expect the most specific pattern to win.
  #
  # Thus we make some crude, recursive "level-of-detail" measurements of a pattern,
  # and summarize them under in its corresponding `Specificity` struct. Importantly
  # enough, pattern specificities are comparable.
  #
  # An alternative to single-way is multiway rewriting, where we perform all possible
  # rewrites. See, for instance, the work of Stephen Wolfram. We will support multiway
  # rewriting in the future; in the context of Wirewright, this seems at least to some
  # extent computationally possible vs. e.g. what Wolfram is (appears to me to be?)
  # showing. Regardless, his "all possible rewrites" map neatly onto an optimizing
  # pattern matching engine that we're trying to build here. Despite all this,
  # in practice, multiway rewriting is rarely *needed*, will inevitably be slower, and
  # is hard to interface with. So we focus more on single-way rewriting.
  def self.specificity(normp : Term, *, toplevel : Bool) : Specificity
    if toplevel && (specificity = specificity0?(normp))
      return specificity
    end

    captures = Set(Term).new
    repeats = literals = restrictions = choices = 0u32

    walk(normp, mode: WalkMode::NonItemSeq) do |operator|
      Term.case(operator) do
        matchpi %[(%capture _)] do
          unless captures.add?(operator)
            repeats += 1
          end

          WalkDecision::Continue
        end

        matchpi %[(%'%literal ())] do
          literals += 1

          WalkDecision::Continue
        end

        matchpi %[(%'%literal d_dict)] do
          literals += d.population.total

          WalkDecision::Continue
        end

        matchpi %[(%'%literal _)] do
          literals += 1

          WalkDecision::Continue
        end

        matchpi %[(%any/literal _+)] do
          choices += 1

          WalkDecision::Continue
        end

        # (%edge _) makes an (edge ...), see the literal, `edge`? Thus we count a literal
        # match. If edge's type is restricted we count that as a restriction. Otherwise
        # we do not.
        matchpi %[(%'%edge %'_)] do
          literals += 1

          WalkDecision::Skip
        end

        matchpi %[(%'%edge _)] do
          literals += 1
          restrictions += 1

          WalkDecision::Skip
        end

        # Count stuff such as (%number _ < 10) as two restrictions: one on the type
        # and one on the magnitude; and e.g. (%number (whole _) < 10) as three: one on
        # the type, one on the magnitude, and one on the value.
        matchpi(
          %[(%'%number %'_ _ _)],
          %[(%'%number %'(whole _))],
        ) do
          restrictions += 2

          WalkDecision::Continue
        end

        matchpi %[(%'%number %'(whole _) _ _)] do
          restrictions += 2

          WalkDecision::Continue
        end

        # Count stuff such as (%number 0 < _ < 10) as three restrictions: one on the type
        # and two on the magnitude.
        matchpi %[(%'%number _ _ %'_ _ _)] do
          restrictions += 3

          WalkDecision::Continue
        end

        # Count stuff such as (%number 0 < (whole _) < 10) as four restrictions: one on
        # the type, two on the magnitude, and one on the value.
        matchpi %[(%'%number _ _ %'(whole _) _ _)] do
          restrictions += 4

          WalkDecision::Continue
        end

        # Rather than listing all operators that make restrictions, we list those that
        # *do not*. This is because most operators make restrictions.
        matchpi(
          %[(%'%pass)],
          %[(%'%never)],
          %[(%'%let _ _)],
          %[(%'%not _+)],
        ) { WalkDecision::Continue }

        # %all sums the specificity of its offshoots.
        matchpi %[(%'%all offshoots_+)] do
          WalkDecision::Continue
        end

        # %any° takes min specificity of its branches.
        matchpi %[(%any/source branches_+)] do
          literals1, choices1, repeats1, restrictions1 = branches.items.min_of do |branch|
            specificity(branch, toplevel: false)
          end

          literals += literals1
          choices += choices1
          repeats += repeats1
          restrictions += restrictions1

          WalkDecision::Skip
        end

        matchpi %[(%'%keypool keys_+)], %[(%'%-keypool keys_*)], %[(%'%keytest keys_*)] do
          restrictions += keys.size

          WalkDecision::Continue
        end

        otherwise do
          restrictions += 1

          WalkDecision::Continue
        end
      end
    end

    {literals, choices, repeats, restrictions}
  end

  # :nodoc:
  module Head
    alias Any = Some | More | None

    record Some, term : Term
    record More
    record None

    def self.operator(candidate : Term) : Any
      Term.case(candidate) do
        matchpi %[(%terminal successor_)] { Head.operator(successor) }
        matchpi %[(%'%let _ successor_)] { Head.operator(successor) }
        matchpi %[(%'%literal term_)] { Some.new(term) }
        matchpi %[(%any° _number _symbol _string _boolean)] { Some.new(candidate) }
        otherwise { None.new }
      end
    end

    def self.singular(candidate : Term) : Any
      Term.case(candidate) do
        matchpi %[(%singular value_)] { Head.operator(value) }
        matchpi %[(%group _ items_+)] { Head.item(items) }
        otherwise { None.new }
      end
    end

    def self.item(items : Term) : Any
      Term.case(items) do
        matchpi %[((%past (%'%slot _)))] { More.new }
        matchpi %[((%past/max (%'%slot _)) successor_ successors_*)] do
          head = Head.singular(successor)
          head.is_a?(More) ? Head.item(successors) : head
        end
        otherwise { None.new }
      end
    end
  end

  # TODO: can't we implement this using walk somehow???

  # Returns the "head" of a normal pattern *normp*.
  #
  # The head of a pattern is the first literal in an expected dictionary matchee.
  # For example, in `(+ a_ b_)` that would be `+`; and in `(`a `b x←qux x_ y_)` that
  # would be `qux`. On the other hand, for `qux` or `(xs_* qux)` the head is
  # indeterminate (because we'd have to know how many `xs` there were), therefore,
  # `nil` is returned.
  def self.head?(normp : Term) : Term?
    Term.case(normp) do
      matchpi %[(%itemseq items_+)] do
        case response = Head.item(items)
        in Head::Some then response.term
        in Head::None, Head::More
        end
      end

      matchpi %[(%terminal successor_)] { head?(successor) }
      matchpi %[(%'%partition itemspart_ _)] { head?(itemspart) }
      matchpi %[(%'%let _ successor_)] { head?(successor) }
      matchpi %[(%'%literal [head_ _*])] { head }

      otherwise { }
    end
  end
end

# Represents a pattern within a `PatternSet`. Has no expected use outside of `PatternSet`.
struct Pattern
  private alias O = M1::Operator

  # Returns the index of this pattern. You are free to treat it as `PatternSet`-unique
  # identifier of this pattern.
  getter index : UInt32

  # Returns the underlying M1 operator.
  getter operator : O::Any

  # :nodoc:
  def initialize(@index : UInt32, @operator : O::Any)
  end

  # Returns the response of this pattern to *matchee* (may be positive or negative).
  def response(matchee : Term, *, env = Term[]) : Pr::Any
    fb = M1next.matches(env, @operator, matchee)

    case fb.size
    when 0 then Pr::Neg.new
    when 1 then Pr::One.new(self, fb[0])
    else
      Pr::Many.new(self, fb)
    end
  end

  def probe?(matchee : Term, *, env = Term[]) : Bool
    M1next.probe?(env, @operator, matchee)
  end

  def_equals_and_hash @index
end

# Short for "pattern response". Groups the various types of responses produced
# by `Pattern` and `PatternSet`.
module Pr
  alias Any = Pos | Neg
  alias Pos = One | Many

  # Positive response of *pattern* that resulted in one environment.
  record One, pattern : Pattern, env : Term::Dict do
    def envs
      {env}
    end
  end

  # Positive response of *pattern* that resulted in multiple environments.
  record Many, pattern : Pattern, envs : Slice(Term::Dict) do
    def ones(& : One ->)
      envs.each { |env| yield One.new(pattern, env) }
    end
  end

  # Negative response.
  record Neg
end

module ICursor
  abstract def current?
  abstract def next?

  def first?
    each do |element|
      return element
    end
  end

  def each(&)
    cursor = self

    while element = cursor.current?
      yield element

      cursor = cursor.next? || break
    end
  end

  def find(&)
    cursor = self

    while element = cursor.current?
      if result = yield element
        return cursor, result
      end

      cursor = cursor.next? || break
    end

    {cursor, nil}
  end
end

# An object capable of parsing pattern terms into `Pattern`s (a thin wrapper
# around `M1::Operator`) and organizing them for efficient response
# to matchees.
class PatternSet(T)
  alias Bucket = Slice(Pattern)

  # Includers are used to determine the indexing key; such a key must be something
  # that a pattern and all its possible matchees *necessarily share*. If the key is
  # indeterminate for a matchee, patterns requiring that key are not going to
  # be tested.
  module Key(T)
    # Extracts a key term from a pattern. Both its original (*pattern*) and
    # normal-form (*normp*) version are given. Returns `nil` if indeterminate;
    # in such case the pattern will be tested on all matchees.
    abstract def of_pattern?(pattern : Term, normp : Term) : T?

    # Extracts a key term from a matchee. If indeterminate, patterns with
    # a determinate key are all going to be skipped.
    abstract def of_matchee?(matchee : Term) : T?
  end

  # The default key implementation, uses `M1.head?`.
  module Key::Head
    extend Key(Term)

    def self.of_pattern?(pattern : Term, normp : Term) : Term?
      M1.head?(normp)
    end

    def self.of_matchee?(matchee : Term) : Term?
      matchee.as_d?.try(&.items.first?)
    end
  end

  # :nodoc:
  struct KeyedMap(T)
    def initialize(@map : Hash(T, Bucket), @key : Key(T))
    end

    def bucket?(matchee : Term) : Bucket?
      return unless key = @key.of_matchee?(matchee)

      @map[key]?
    end
  end

  # :nodoc:
  def initialize(@keyed : KeyedMap(T), @unkeyed : Bucket)
  end

  # Constructs a pattern set by extracting patterns from *base* using *selector*.
  #
  # Considers only matches of *selector* that contain a capture named `pattern`.
  # The contents of this capture are treated as a pattern and added to the pattern set.
  #
  # Yields normal `pattern` (see `M1.normal`), followed by match env of
  # *selector* for further handling by the block. Expects the block to return
  # `true` if the pattern should be handled and finally added to the set; or
  # `false`/`nil` if the pattern should be ignored.
  #
  # Yields patterns in their index order and **not** *base*-order. This means that
  # the index of the current yield will correspond to `Pattern#index` that you can
  # access from `Pr`. The index of the current iteration can thus be used as a
  # reference to the current pattern.
  #
  # ```
  # pset = PatternSet.select(ML.term(%[(rule pattern_ body_)]), base) do |normp, env|
  #   # Do something with env[:body]
  #   # ...
  #
  #   true # E.g. body is valid
  # end
  # ```
  def self.select(selector : Term, bases : Enumerable(Term), key keymod : Key(T) = Key::Head, & : Term, Term::Dict -> Bool?) : PatternSet(T) forall T
    seen = Set(Term).new

    keyed = {} of T => Array(Int32)
    headless = [] of Int32

    patterns = [] of Pattern
    specificities = [] of M1::Specificity

    bases.each do |base|
      base.each_item_unordered do |item|
        envs = M1next.matches(selector, item)
        envs.each do |env|
          next unless pattern = env[:pattern]?
          next unless seen.add?(pattern)

          index = seen.size - 1

          normp = M1.normal(pattern)

          specificity = M1.specificity(normp, toplevel: true)
          specificities << specificity

          operator = M1.operator(normp, normalize: false)

          {% if flag?(:profile) %}
            Profile.optop[operator] = pattern
          {% end %}

          pattern_object = Pattern.new(index.to_u32, operator)
          next unless yield normp, env

          patterns << pattern_object

          if key = keymod.of_pattern?(pattern, normp)
            neighbors = keyed.put_if_absent(key) { [] of Int32 }
            neighbors << index
          else
            headless << index
          end
        end
      end
    end

    # Now that we have everything neatly organized, sort keyed and headless
    # patterns by specificity, descending.
    headless.sort! { |a, b| specificities.unsafe_fetch(b) <=> specificities.unsafe_fetch(a) }
    keyed.each do |_, neighbors|
      neighbors.sort! { |a, b| specificities.unsafe_fetch(b) <=> specificities.unsafe_fetch(a) }
    end

    okeyed = keyed.transform_values do |indices|
      indices.to_readonly_slice.map(read_only: true) { |index| patterns[index] }
    end

    oheadless = headless.to_readonly_slice.map(read_only: true) { |index| patterns[index] }

    PatternSet(T).new(KeyedMap(T).new(okeyed, keymod), oheadless)
  end

  def self.select(selector : Term, *bases : Term, **kwargs, &)
    self.select(selector, bases, **kwargs) { |*args| yield *args }
  end

  # Block-less version of `select`.
  def self.select(*args, **kwargs)
    self.select(*args, **kwargs) { true }
  end

  struct Matcher
    include ::Ww::Term::Case::Matcher

    def initialize(@pset : PatternSet(Term), @table : Slice(Int32))
    end

    def self.compile(specs : Slice(Term::Case::MatchSpec)) : Matcher
      base = Term::Dict.build do |commit|
        specs.each_with_index do |spec, index|
          commit << {index, spec.pattern}
        end
      end

      table = Slice(Int32).new(base.itemsize)

      index = 0
      pset = PatternSet(Term).select(ML.term(%{(index←(%number +i32) pattern_)}), Term.of(base)) do |_, env|
        table[index] = env[:index].to(Int32)
        index += 1

        true # accept
      end

      new(pset, table)
    end

    def scan(matchee : Term, *, env : Term::Dict)
      @pset.query(matchee, env: env)
        .select(Pr::One)
        .map { |pr| {pr.env, @table[pr.pattern.index]} }
    end
  end

  macro case(matchee, **kwargs, &block)
    ::Ww::Term.case({{matchee}}, matcher: ::PatternSet::Matcher, {{kwargs.double_splat}}) {{block}}
  end

  def each_candidate(matchee : Term, & : M1::Operator::Any, UInt32 ->)
    if bucket = @keyed.bucket?(matchee)
      bucket.each { |pattern| yield pattern.operator, pattern.index }
    end

    @unkeyed.each { |pattern| yield pattern.operator, pattern.index }
  end

  struct Candidates
    include ICursor

    def initialize(@keyed : Bucket, @unkeyed : Bucket, @index = 0)
    end

    def current? : Pattern?
      if 0 <= @index < @keyed.size
        @keyed[@index]
      elsif 0 <= @keyed.size <= @index < @keyed.size + @unkeyed.size
        @unkeyed[@index - @keyed.size]
      end
    end

    def next? : Candidates?
      if @index + 1 < @keyed.size + @unkeyed.size
        Candidates.new(@keyed, @unkeyed, @index + 1)
      end
    end
  end

  struct Responses
    include ICursor

    @candidates : Candidates
    @response : Pr::Pos?

    def initialize(candidates : Candidates, @matchee : Term, @env : Term::Dict)
      @candidates, @response = candidates.find &.response(matchee, env: env).as?(Pr::Pos)
    end

    def current? : Pr::Pos?
      @response
    end

    def next? : Responses?
      # Current @candidates points to the first valid Pr::Pos (if any; otherwise
      # it points after the end of the candidates list). Thus we advance once,
      # then the initialize code does its job of finding the next Pr::Pos.
      if successor = @candidates.next?
        Responses.new(successor, @matchee, @env)
      end
    end
  end

  def query(matchee : Term, *, env : Term::Dict = Term[]) : Iterator(Pr::Pos)
    bucket = @keyed.bucket?(matchee)
    bucket ||= Bucket.empty
    bucket.each
      .chain(@unkeyed.each)
      .map(&.response(matchee, env: env))
      .select(Pr::Pos)
  end

  def candidates(matchee : Term) : Candidates
    bucket = @keyed.bucket?(matchee)
    bucket ||= Bucket.empty

    Candidates.new(bucket, @unkeyed, index: 0)
  end

  def responses(matchee : Term, *, env = Term[]) : Responses
    Responses.new(candidates(matchee), matchee, env)
  end

  def response(matchee : Term, *, env = Term[]) : Pr::Any
    responses(matchee, env: env).first? || Pr::Neg.new
  end
end

class ::Ww::Term::Dict
  # Recurses into entry values only.
  #
  # Counts itself too (smallest possible value is 1).
  @[Dncast]
  def fresh_maxdepth : Magnitude
    maxdepth = Magnitude.new(1)

    each_entry do |k, v|
      if vdict = v.as_d?
        maxdepth = Math.max(maxdepth, vdict.fresh_maxdepth + 1)
      end
    end

    maxdepth
  end

  # TODO: we should probably use Magnitude here. Float behavior with infinites may
  # work just fine here. Although I'm not sure how tolerant to imprecision we are
  # with .population, and what kinds of imprecision we'll get.
  record Population, numbers : UInt16, symbols : UInt16, strings : UInt16, booleans : UInt16 do
    def self.zero
      new(0u16, 0u16, 0u16, 0u16)
    end

    def +(other : Population)
      # TODO: overflow = max u16 = infinity
      Population.new(
        numbers + other.numbers,
        symbols + other.symbols,
        strings + other.strings,
        booleans + other.booleans,
      )
    end

    def +(other : Term)
      # TODO: overflow = max u16 = infinity
      case other.type
      in .any?
        unreachable
      in .number?
        copy_with(numbers: numbers + 1)
      in .string?
        copy_with(strings: strings + 1)
      in .symbol?
        copy_with(symbols: symbols + 1)
      in .boolean?
        copy_with(booleans: booleans + 1)
      in .dict?
        self + other.unsafe_as_d.population
      end
    end

    def total : UInt32
      numbers.to_u32 + strings.to_u32 + symbols.to_u32 + booleans.to_u32
    end
  end

  # TODO: cache on dicts
  @[Dncast]
  def population
    ee.sum(Population.zero) { |_, v| v }
  end
end

# Binarizes and simplifies nested/long `%all` *node*.
def all2(node) : Term
  Term.case(node) do
    matchpi %{(%'%all)} { M1::Normal::PASS }
    matchpi %{(%'%all a_)} { a }
    matchpi %{(%'%all a_ %'(%pass))} { a }
    matchpi %{(%'%all %'(%pass) b_)} { b }
    matchpi %{(%'%all _ _)} { Term.of(node) }
    matchpi %{(%'%all a_ b_ rest_+)} do
      a1 = Term.of(:"%all", a, b)
      b1 = rest.prepend(:"%all")

      all2(Term.of(:"%all", all2(a1), all2(b1)))
    end
  end
end

# *Pattern shapes* are a restricted, more open subset of pattern matching constructs
# that we are able to index efficiently. Any M1 pattern has an associated shape, which
# can be more or less lossy.
#
# Pattern shapes are guaranteed to consist only of the following nodes:
#
# - `(%'%value (%'%literal _) _)`
# - `(%'%any/source _+)`
# - `(%'%all a_ b_)`
# - `%'(%pass)`
# - `%'(%symbol)`
# - `%'(%string)`
# - `%'(%boolean)`
# - `%'(%dict)`
# - `%'(%number _)`
# - `(%'%literal X_)` with non-dict X or empty dict X
module ::Ww::M1::Shape
  extend self

  # Generates a sequence of *subject* itemseq calls repeated *n* times.
  private def repeated(prefix, key, subject, n, ahead0, rear) : Term
    if n.zero?
      return ahead0.call(prefix, key)
    end

    ahead1 = ->(prefix : Term::Dict, key : Term::Num) do
      repeated(prefix, key, subject, n - 1, ahead0, rear)
    end

    itemseq(prefix, key, subject, ahead1, rear)
  end

  # Returns the shape of an itemseq *item*.
  private def itemseq(prefix, key, item : Term, ahead, rear) : Term
    Term.of_case(item) do
      # Fetch successor.
      matchpi %{(%'%singular successor_)} do
        prefix = prefix.append({:"%value", {:"%literal", key}, pattern(successor)})

        ahead.call(prefix, key + 1)
      end

      # Expand small bounded %plural's into a disjunction with each possible length.
      matchpi(
        %{((%any %plural %plural/min %plural/max) (%optional untitled _) ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8)
            type_symbol)}
      ) do
        case type.blank.type
        in .any?     then unit = M1::Normal::PASS
        in .number?  then unit = M1::Normal::BLANK_NUMBER
        in .string?  then unit = M1::Normal::BLANK_STRING
        in .symbol?  then unit = M1::Normal::BLANK_SYMBOL
        in .dict?    then unit = M1::Normal::BLANK_DICT
        in .boolean? then unit = M1::Normal::BLANK_BOOLEAN
        end

        Term::Dict.build do |disj|
          disj << :"%any/source"

          (min.to(Int32)..max.to(Int32)).each do |hi|
            variant = prefix

            hi.times do |length|
              variant = variant.append({:"%value", {:"%literal", key + length}, unit})
            end

            disj << ahead.call(variant, key + hi + 1)
          end
        end
      end

      # Dive into %group's.
      matchpi %{(%'%group _ children_+)} do
        itemseq(prefix, key, children.items, ahead, rear)
      end

      # Expand small bounded %past and %many's into a disjunction with each
      # possible length.
      matchpi(
        %{(%'%past children_+ ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8)
            greedy_boolean: _)},
        %{(%'%many _ children_+ ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8))}
      ) do
        Term::Dict.build do |disj|
          disj << :"%any/source"

          (min.to(Int32)..max.to(Int32)).each do |hi|
            disj << repeated(prefix, key, children.items, hi, ahead, rear)
          end
        end
      end

      # Optionals expand into a disjunction with and without the item.
      matchpi %{(%'%optional _ body_)} do
        variant0 = ahead.call(prefix, key)
        variant1 = ahead.call(prefix.append({:"%value", {:"%literal", key}, pattern(body)}), key + 1)

        Term.of(:"%any/source", variant0, variant1)
      end

      otherwise { rear.call(prefix) }
    end
  end

  # Returns the shape of an itemseq in *feed*.
  private def itemseq(prefix, key, feed : Term::Dict::ItemsView, ahead0, rear) : Term
    unless item = feed.first?
      return ahead0.call(prefix, key)
    end

    ahead1 = ->(prefix : Term::Dict, key : Term::Num) do
      itemseq(prefix, key, feed.move(1), ahead0, rear)
    end

    itemseq(prefix, key, item, ahead1, rear)
  end

  # Returns the shape of an itemseq *seq*.
  def itemseq(seq : Term::Dict)
    rear = ->(prefix : Term::Dict) { all2(prefix) }
    ahead = ->(prefix : Term::Dict, key : Term::Num) { rear.call(prefix) }

    itemseq(Term.dict(:"%all"), Term[0], seq.items, ahead, rear)
  end

  private def entry(prefix, key, value, ahead) : Term
    Term.of_case(value) do
      matchpi %{(%'%entry/required successor_)} do
        prefix = prefix.append({:"%value", {:"%literal", key}, pattern(successor)})

        ahead.call(prefix)
      end

      matchpi %{(%'%entry/optional _ successor_)} do
        variant0 = ahead.call(prefix)
        variant1 = ahead.call(prefix.append({:"%value", {:"%literal", key}, pattern(successor)}))

        {:"%any/source", variant0, variant1}
      end

      otherwise do
        ahead.call(prefix)
      end
    end
  end

  private def entries(prefix : Term::Dict, n, entries, ahead0) : Term
    unless entry = entries.nth?(n)
      return ahead0.call(prefix)
    end

    ahead1 = ->(prefix : Term::Dict) do
      entries(prefix, n + 1, entries, ahead0)
    end

    entry(prefix, *entry, ahead1)
  end

  # Returns the shape of *entries*.
  def entries(entries : Term::Dict) : Term
    if entries.empty?
      return Term.of({:"%dict"})
    end

    ahead = ->(prefix : Term::Dict) { all2(prefix) }

    entries(Term.dict(:"%all"), 0, entries, ahead)
  end

  # Returns the shape of a normal pattern *normp*.
  def pattern(normp : Term) : Term
    Term.of_case(normp) do
      matchpi %{(%'%pass)} { normp }
      matchpi %{(%'%symbol)} { normp }
      matchpi %{(%'%string)} { normp }
      matchpi %{%'(%number _)} { normp }
      matchpi %{(%'%boolean)} { normp }
      matchpi %{(%'%dict)} { normp }

      matchpi %{(%'%literal ())} { normp }
      matchpi %{(%'%literal x_dict)} { pattern(M1.normal_escaped(x)) }
      matchpi %{(%'%literal _)} { normp }

      matchpi %{(%'%edge _)} do
        Term.of(:"%value", {:"%literal", 0}, {:"%literal", :edge})
      end

      matchpi %{(%'%let _ successor_)} do
        pattern(successor)
      end

      matchpi %{(%'%itemseq successors_+)} do
        itemseq(successors.unsafe_as_d)
      end

      matchpi %{(%'%partition itemspart_ pairspart_)} do
        all2(Term.of(:"%all", pattern(itemspart), pattern(pairspart)))
      end

      # In a pattern shape, all layers are always open. So we cannot make
      # literal belows closed. However we still account them during matching
      # for precision.
      begin
        # Do not emit useless %dict checks for below.
        matchpi %{(%'%layer (%'%literal ()) side_dict)} do
          entries(side.unsafe_as_d)
        end

        matchpi %{(%'%layer below_ side_dict)} do
          all2(Term.of(:"%all", pattern(below), entries(side.unsafe_as_d)))
        end
      end

      matchpi %{(%'%any/source successors_+)} do
        Term::Dict.build do |commit|
          commit << :"%any/source"

          successors.each_item_unordered do |item|
            commit << pattern(item)
          end
        end
      end

      matchpi %{(%'%any/literal options_+)} do
        Term::Dict.build do |commit|
          commit << :"%any/source"

          options.each_item_unordered do |item|
            commit << pattern(M1.normal_escaped(item))
          end
        end
      end

      matchpi %{(%'%all a_ b_)} do
        all2(Term.of(:"%all", pattern(a), pattern(b)))
      end

      matchpi(
        %{(%'%number %'(whole _))},
        %{(%'%number _ _ _)},
        %{(%'%number _ _ _ _ _)},
        %{(%'%pipe (%barrier (+ _number)) _)},
        %{(%'%pipe (%barrier (- _number)) _)},
        %{(%'%pipe (%barrier (* _number)) _)},
        %{(%'%pipe (%barrier (/ _number)) _)},
        %{(%'%pipe (%barrier (div _number)) _)},
        %{(%'%pipe (%barrier (mod _number)) _)},
        %{(%'%pipe (%barrier (** _number)) _)},
        %{(%'%pipe (%barrier (clamp _number ..= _number)) _)},
      ) { M1::Normal::BLANK_NUMBER }

      matchpi(
        %{(%'%pipe (map _) _)},
        %{(%'%pipe type _)},
      ) { M1::Normal::PASS }

      matchpi %{(%'%pipe span _)}, %{(%'%pipe ml _)} { M1::Normal::BLANK_STRING }
      matchpi %{(%'%pipe tally _)} { M1::Normal::BLANK_DICT }

      matchpi %{(%'%symbol nonblank)}, %{(%'%symbol blank _ _)} do
        M1::Normal::BLANK_SYMBOL
      end

      matchpi %{(%'%terminal node_)} do
        pattern(node)
      end

      otherwise do
        M1::Normal::PASS
      end
    end
  end
end

module ::Ww::M1
  def self.shape(normp : Term)
    Shape.pattern(normp)
  end

  private def self.branches(shape : Term, ahead0 : Term ->) : Nil
    Term.case(shape) do
      matchpi %{(%'%value (%'%literal _) value_)} do
        ahead1 = ->(branch : Term) do
          ahead0.call(Term.of(shape.with(2, branch)))
        end

        branches(value, ahead1)
      end

      matchpi %{(%'%all a_ b_)} do
        ahead2 = ->(branch0 : Term) do
          ahead1 = ->(branch1 : Term) do
            ahead0.call(all2(Term.of(:"%all", branch0, branch1)))
          end

          branches(b, ahead1)
        end

        branches(a, ahead2)
      end

      matchpi %{(%'%any/source children_+)} do
        children.items.each do |child|
          branches(child, ahead0)
        end
      end

      otherwise do
        ahead0.call(shape)
      end
    end
  end

  # Normalizes a pattern shape to DNF. Calls *sink* with each toplevel branch.
  #
  # As long as *shape* is a pattern shape, branches given to *sink* are guaranteed
  # to be pattern shapes without `%any/source`.
  #
  # Non-shape nodes are unexpected and will not be processed.
  def self.branches(shape : Term, &sink : Term ->) : Nil
    branches(shape, sink)
  end

  private def self.strands(prefix : Term::Dict, branch : Term, sink) : Nil
    Term.case(branch) do
      matchpi %{(%'%pass)} { sink.call(prefix) }

      matchpi %{%'(%number _)}, %{%'(%string)}, %{%'(%symbol)}, %{%'(%boolean)}, %{%'(%dict)} do
        sink.call(prefix.append(branch))
      end

      matchpi %{(%'%literal ())} do
        sink.call(prefix.append(M1::Normal::BLANK_DICT).append(branch))
      end

      matchpi %{(%'%literal _number)} do
        sink.call(prefix.append(M1::Normal::BLANK_NUMBER).append(branch))
      end

      matchpi %{(%'%literal _string)} do
        sink.call(prefix.append(M1::Normal::BLANK_STRING).append(branch))
      end

      matchpi %{(%'%literal _symbol)} do
        sink.call(prefix.append(M1::Normal::BLANK_SYMBOL).append(branch))
      end

      matchpi %{(%'%literal _boolean)} do
        sink.call(prefix.append(M1::Normal::BLANK_BOOLEAN).append(branch))
      end

      matchpi %{(%'%all a_ b_)} do
        strands(prefix, a, sink)
        strands(prefix, b, sink)
      end

      matchpi %{(%'%value (%'%literal _) successor_)} do
        prefix = prefix
          .append(M1::Normal::BLANK_DICT)
          .append(branch.without(2))

        strands(prefix, successor, sink)
      end
    end
  end

  # Calls *sink* with each strand (represented as an itemsonly dict) of *branch*.
  #
  # A strand is an exhaustive path through `%all` nodes in *branch*.
  #
  # Each strand consists of *bases*. The following list is an exhaustive list
  # of bases:
  #
  # - `(%'%value (%'%literal _))`
  # - `%'(%any)`
  # - `%'(%symbol)`
  # - `%'(%string)`
  # - `%'(%number _)`
  # - `%'(%boolean)`
  # - `%'(%dict)`
  # - `(%'%literal _)`
  def self.strands(branch : Term, &sink : Term::Dict ->)
    strands(Term.dict({:"%any"}), branch, sink)
  end
end
