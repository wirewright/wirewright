module Ww::M1
  # Short for *operator*, this module holds data structures used to represent
  # compiled operators and associated groups/categories of operators (represented
  # using aliases).
  module Op
    alias Any = Pass | Never | Num | Sym | Atom | SymBlank | SymNonblank | Boolean | Dict | Itemsonly | Pairsonly | Guard | Literal | Capture | CaptureItemsonly | Seq | ItemFirst | ItemLast | SingularSeq | Partition | Edge | LiteralWhitelist | ChoiceSource | Keypool | Span | Tally | Type | ParseML | Clamp | Bin | Both | LiteralBlacklist | Layer | ScanFirst | ScanSource | ScanAll | DfsFirst | DfsSource | DfsAll | BfsFirst | BfsSource | BfsAll | Value | NegativeValue | NegativeValueKeypath | EntriesFirst | EntriesSource | EntriesAll | Str | KeypathCapture | Keytest | ValueLiteral | Filter | Pluck | Flat | Split | Adjacent | Untracked | Matches | FrontRef | BackRef | Prepend

    # TODO: Inline, this is not used anywhere!!
    alias Bin = Add | Sub | Mul | Div | Idiv | Mod | Pow | Map

    INSTANCE_PASS = Pass.new

    defcase Pass

    INSTANCE_NEVER = Never.new

    defcase Never

    INSTANCE_NUM       = Num.new(min: nil, max: nil, spec: :none)
    INSTANCE_NUM_WHOLE = Num.new(min: nil, max: nil, spec: :whole)

    defcase Num, spec : Spec, min : Arg, max : Arg do
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

        def min_included? : Bool
          !min_excluded?
        end

        def max_included? : Bool
          !max_excluded?
        end
      end
    end

    INSTANCE_SYM     = Sym.new
    INSTANCE_STR     = Str.new
    INSTANCE_BOOLEAN = Boolean.new
    INSTANCE_DICT    = Dict.new
    INSTANCE_ATOM    = Atom.new

    defcase Sym

    defcase SymBlank, name : Any, type : Any
    defcase SymNonblank

    defcase Atom

    defcase Str
    defcase Boolean
    defcase Dict

    defcase Itemsonly

    defcase Pairsonly
    # *bounds* and *depth* ranges are inclusive.
    defcase Guard,
      sketch : Term::Dict::Sketch,
      bounds : {Magnitude, Magnitude},
      depth : {Magnitude, Magnitude},
      successor : Any

    defcase FrontRef, name : Term
    defcase BackRef, name : Term

    defcase Literal, term : Term
    defcase LiteralWhitelist, whitelist : Set(Term)
    defcase LiteralBlacklist, blacklist : Set(Term)

    defcase Capture, capture : Term, successor : Any

    defcase Edge, type : TermType

    defcase Seq, items : Item::Spatial, singulars : Array(Any)

    defcase ItemFirst, successor : Any
    defcase ItemLast, successor : Any
    defcase SingularSeq, items : Slice(Any), exhaustive : Bool, reverse : Bool

    defcase ChoiceSource, a : Any, b : Any

    defcase Both, a : Any, b : Any

    defcase Keytest, keys : Slice(Term)
    defcase Keypool, keys : Slice(Term)

    defcase ValueLiteral, key : Term, successor : Any

    defcase Span, successor : Any
    defcase Tally, successor : Any
    defcase Type, successor : Any
    defcase ParseML, successor : Any
    defcase Untracked, successor : Any
    defcase Prepend, terms : Slice(Term), successor : Any

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

    defcase Partition, itemspart : Any, pairspart : Any, seq : Bool do
      private def self.seq?(op : Any) : Bool
        case op
        when ItemFirst,
             ItemLast,
             CaptureItemsonly,
             SingularSeq,
             Seq
          true
        when Guard, Capture
          seq?(op.successor)
        else
          false
        end
      end

      def self.new(itemspart : Any, pairspart : Any)
        new(itemspart, pairspart, seq: seq?(itemspart))
      end
    end

    defcase Layer, below : Any, side : Slice(Entry::Any)

    alias First = ScanFirst | DfsFirst | BfsFirst | EntriesFirst | SplitFirst
    alias Source = DfsSource | BfsSource | ScanSource | EntriesSource | SplitSource
    alias All = ScanAll | DfsAll | BfsAll | EntriesAll | SplitAll

    alias Scan = ScanFirst | ScanSource | ScanAll

    defcase ScanFirst, seq : Slice(Any)
    defcase ScanSource, seq : Slice(Any)
    defcase ScanAll, successor : Any, seq : Slice(Any), min : Magnitude, max : Magnitude

    defcase Value, capture : Term, tail : Any
    defcase NegativeValue, capture : Term
    defcase NegativeValueKeypath, capture : Term, name : Term

    alias Dfs = DfsFirst | DfsSource | DfsAll

    defcase DfsFirst, alg : Tzip::Dfs, seq : Slice(Any)
    defcase DfsSource, alg : Tzip::Dfs, seq : Slice(Any)
    defcase DfsAll, alg : Tzip::Dfs, successor : Any, seq : Slice(Any), min : Magnitude, max : Magnitude

    alias Bfs = BfsFirst | BfsSource | BfsAll

    defcase BfsFirst, alg : Tzip::Bfs, seq : Slice(Any)
    defcase BfsSource, alg : Tzip::Bfs, seq : Slice(Any)
    defcase BfsAll, alg : Tzip::Bfs, successor : Any, seq : Slice(Any), min : Magnitude, max : Magnitude

    alias Entries = EntriesFirst | EntriesSource | EntriesAll

    defcase EntriesFirst, kop : Any, vop : Any
    defcase EntriesSource, kop : Any, vop : Any
    defcase EntriesAll, successor : Any, kop : Any, vop : Any, min : Magnitude, max : Magnitude

    alias Split = SplitFirst | SplitSource | SplitAll

    defcase SplitFirst, lhs : Any, focus : Slice(Any), rhs : Any, wide : Bool
    defcase SplitSource, lhs : Any, focus : Slice(Any), rhs : Any, wide : Bool
    defcase SplitAll, successor : Any, lhs : Any, focus : Slice(Any), rhs : Any, min : Magnitude, max : Magnitude, wide : Bool

    defcase Matches, successor : Any, subpattern : Any, min : Magnitude, max : Magnitude

    defcase Adjacent, members : Slice(Any)

    defcase KeypathCapture, capture : Term

    defcase Filter, successor : Any, deps : Pf::Set(Term), selector : Any, min : Magnitude, max : Magnitude
    defcase Pluck, spec : Tzip::PluckSpec, successor : Any
    defcase Flat, spec : Tzip::FlatSpec, successor : Any
  end

  # Compiled sequence operators such as `(⏏_⏏ ⏏(%optional 0 x_)⏏ ⏏y_⏏)`.
  module Op::Item
    extend self

    ORD_FRONT   = 0u32
    ORD_BACK    = UInt32::MAX
    ORD_INITIAL = 1u32

    defrecord Spatial, shape : Shape, refs : Array(Ref), flatcount : Int32

    alias Any = Singular | FlexSingular | Slot | Plural | Group | Gap | GapSource | Optional | Many | Past

    alias Past = PastMin | PastMax

    defcase Singular, successor : Op::Any
    defcase FlexSingular, successor : Op::Any
    defcase Slot, ord : UInt32, name : Term

    alias Plural = PluralDistrib | PluralMinMax
    alias PluralMinMax = PluralMin | PluralMax

    defcase PluralDistrib, capture : Term?, type : TermType, min : Magnitude, max : Magnitude
    defcase PluralMin, capture : Term?, type : TermType, min : Magnitude, max : Magnitude
    defcase PluralMax, capture : Term?, type : TermType, min : Magnitude, max : Magnitude

    defcase Group, ord : UInt32, successor : Op::Any, members : Slice(Any)

    alias Min = PastMin | PluralMin | GapMin
    alias Max = PastMax | PluralMax | GapMax | ManyMax

    alias Gap = GapFirstDistrib | GapMinMax
    alias GapMinMax = GapMin | GapMax
    alias GapMin = GapFirstMin | GapSourceMin
    alias GapMax = GapFirstMax | GapSourceMax
    alias GapFirstMinMax = GapFirstMin | GapFirstMax

    alias GapFirst = GapFirstDistrib | GapFirstMin | GapFirstMax

    defcase GapFirstDistrib, measurer : Op::Any
    defcase GapFirstMin, measurer : Op::Any
    defcase GapFirstMax, measurer : Op::Any

    alias GapSource = GapSourceMin | GapSourceMax

    defcase GapSourceMin, measurer : Op::Any
    defcase GapSourceMax, measurer : Op::Any

    defcase Optional, ord : UInt32, default : Term, successor : Op::Any

    alias Many = ManyMax

    defcase ManyMax, successor : Op::Any, members : Spatial, min : Magnitude, max : Magnitude

    defcase PastMin, children : Spatial, min : Magnitude, max : Magnitude
    defcase PastMax, children : Spatial, min : Magnitude, max : Magnitude

    {% begin %}
      alias Flat = Union({{ (Any.union_types - [Group, Slot]).splat }})
    {% end %}

    defrecord Ref, begin : Int32, end : Int32, ord : UInt32, item : Slot | Group

    defcase Rigid, items : Array(Op::Any)

    defcase Distrib, contenders : Array(Unit)

    alias Distrib::Unit = PluralDistrib | GapFirstDistrib

    defcase NonDistrib, items : Array(Unit)

    {% begin %}
      alias NonDistrib::Unit = Union({{ (Flat.union_types - Distrib::Unit.union_types - [Singular]).splat }})
    {% end %}

    alias Flex = Distrib | NonDistrib::Unit

    defcase FlexRegion, items : Array(Distrib | NonDistrib)

    def flatseq(items : Enumerable(Any)) : {Array(Flat), Array(Ref)}
      # Since groups and slots can appear anywhere in the structure, without
      # affecting the way it reads (they're "pure observers", so to speak),
      # we have to eliminate them. We do this by moving info about groups and
      # slots to the *refs* array.
      units = [] of Flat
      refs = [] of Ref
      flatten(items, units, refs)

      {units, refs}
    end

    private def flatten(items : Enumerable(Any), units, refs) : Nil
      items.each { |item| flatten(item, units, refs) }
    end

    private def flatten(item : Flat, units, refs) : Nil
      units << item
    end

    private def flatten(item : Slot, units, refs) : Nil
      refs << Ref.new(units.size, units.size, item.ord, item)
    end

    private def flatten(item : Group, units, refs) : Nil
      b = units.size
      flatten(item.members, units, refs)
      e = units.size

      refs << Ref.new(b, e, item.ord, item)
    end

    def spatial(items : Enumerable(Any)) : Spatial
      state, refs = flatseq(items)

      flatcount = state.size

      # Array(Flat) must now become Array(Rigid | Distrib | NonDistrib).
      state = state.map_with_index do |unit, index|
        refary = refs.select { |ref| index.in?(ref.begin, ref.end) }

        case unit
        in Singular         then Rigid.new([unit.successor])
        in Distrib::Unit    then Distrib.new([unit])
        in NonDistrib::Unit then NonDistrib.new([unit])
        end
      end

      # Now we group adjacent Rigids, Distribs, and NonDistribs together. The result
      # is an alternating sequence of them.
      state = birep(state) do |l, r|
        case {l, r}
        when {Rigid, Rigid}
          l.items.concat(r.items)
          false # pending
        when {Distrib, Distrib}
          l.contenders.concat(r.contenders)
          false # pending
        when {NonDistrib, NonDistrib}
          l.items.concat(r.items)
          false # pending
        else
          true # commit
        end
      end

      # Now both Distribs and NonDistribs become FlexRegions. The result is
      # an array of Rigids and FlexRegions.
      state = state.map do |unit|
        case unit
        in Rigid               then unit
        in Distrib, NonDistrib then FlexRegion.new([unit])
        end
      end

      # Adjacent flexes should be grouped as well.
      state = birep(state) do |l, r|
        case {l, r}
        when {Rigid, Rigid}
          l.items.concat(r.items)
          false # pending
        when {FlexRegion, FlexRegion}
          l.items.concat(r.items)
          false # pending
        else
          true # commit
        end
      end

      # At this point, state is an array of alternating Rigid and FlexRegion
      # nodes. We need to perform *recognition*, a process that identifies
      # useful shapes in the alternation based on its content and size.
      shape = recognize(state.to_readonly_slice)

      Spatial.new(shape, refs, flatcount)
    end

    private def birep(array : Array(T), &) forall T
      pending = nil
      result = [] of T

      array.each do |object|
        if pending.nil?
          pending = {object}
          next
        end

        commit = yield pending[0], object
        next unless commit

        result << pending[0]
        pending = {object}
      end

      if pending
        result << pending[0]
      end

      result
    end

    alias Shape = Rigid | FlexShape
    alias FlexShape = Empty | FlexRegion | PaddedRight | PaddedLeft | Padded | MidGap | PaddedMidGap

    defcase Empty
    defcase PaddedLeft, l : FlexShape, r : Rigid
    defcase PaddedRight, l : Rigid, r : FlexShape

    defcase Padded, l : FlexShape, m : Rigid, r : FlexShape
    defcase MidGap, l : Rigid, m : FlexShape, r : Rigid
    defcase PaddedMidGap, l : FlexShape, ml : Rigid, m : FlexShape, mr : Rigid, r : FlexShape

    # *alt* is an alternating sequence of Rigids and Flexes.
    def recognize(alt : Slice(Rigid | FlexRegion)) : Shape
      case alt.size
      when 0
        Empty.new
      when 1
        alt.first
      when .even?
        case head = alt.first
        in Rigid then return PaddedRight.new(head, recognize(alt + 1).as(FlexShape))
        in FlexRegion
        end

        case tail = alt.last
        in Rigid then return PaddedLeft.new(recognize(alt - 1).as(FlexShape), tail)
        in FlexRegion
        end

        raise ArgumentError.new
      when .odd?
        center = alt.size//2

        case mid = alt[center]
        in Rigid
          Padded.new(recognize(alt[...center]).as(FlexShape), mid, recognize(alt[center + 1..]).as(FlexShape))
        in FlexRegion
          l = recognize(alt[...center])
          r = recognize(alt[center + 1..])

          case {l, r}
          when {Rigid, Rigid}
            MidGap.new(l, mid, r)
          when {PaddedLeft, PaddedRight}
            PaddedMidGap.new(l.l, l.r, mid, r.l, r.r)
          else
            # If you want to have some fun, you can try to prove this is unreachable.
            #
            # Or maybe it *is* reachable and I'm stupid . . .
            raise ArgumentError.new
          end
        end
      else
        raise ArgumentError.new
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
