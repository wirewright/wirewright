# TODO: simplify items part "_*" to map to itemsonly?, (_+) to map to itemsonly size > 0
# TODO: simplify items part "xs_*" to map to itemsonly followed by assign, (xs_+) to itemsonly size > 0 followed by assign
# TODO: simplify items part "x_ _*" to be "match first item"
# TODO: simplify items part "_* x_" to be "match last item"
# TODO: simplify items part "_* x_ _*" to be "scan for item"
# TODO: all of the above can be done using optimizer that uses Parseout.term and
#       pattern matching itself. Just have to avoid running the optimizer on itself
#       for the first pass to avoid circularity, then can do that with compiled patterns.
# TODO: %mux for multiplex patterns, rename %either to %choice?
# TODO: (%optional) works like (%many) but matches 0/1 item.
# TODO: (%group) in ItemSequence to group multiple items, most imporantly for backmaps
# TODO: add a way to say "don't care about all other pairs but A: _, B: _, C: _ (literals)"
#       also have a way to save all other pairs e.g. `(_* ¦ opts_ width: w_number height: h_number)
#       should match w,h, and save everything else under `opts`
#

abstract class Ww::Term::Match::Parseout
  module Walkable
    abstract def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
    abstract def walk(fn : Walkable -> Bool) : Nil

    def walk(&fn : Walkable -> Bool) : Nil
      walk(fn)
    end

    def map(guidance = ->(node : Walkable) { true }, &fn : Walkable -> Walkable) : Walkable
      map(guidance, fn)
    end

    def walk(walker : Walker) : Nil
      walk { |node| walker.walk?(node) }
    end
  end

  module Walker
    abstract def walk?(node : Walkable) : Bool
  end

  include Walkable

  abstract def pattern : Pattern

  def walk(fn : Walkable -> Bool) : Nil
    fn.call(self)
  end

  def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
    return self unless guidance.call(self)

    fn.call(self)
  end

  def walk(cls : T.class, &fn : T -> Bool) : Nil forall T
    walk do |parseout|
      parseout.class <= T && fn.call(parseout.as(T))
    end
  end

  def key? : Term?
  end

  subclass Literal, term : Term do
    def pattern : Pattern
      Match::Literal[@term]
    end
  end

  subclass Blank, source : Term::Sym do
    def blank : Term::Sym::Blank
      @source.blank? || unreachable
    end

    def type : TermType
      blank.type
    end

    def name? : Term::Sym?
      blank.name?
    end

    def pattern : Pattern
      IsA[type] - Assign[name?]
    end
  end

  # subclass BlankSingleton, source : Term::Sym do
  #   def blank : Term::Sym::Blank
  #     @source.blank? || unreachable
  #   end

  #   def type : TermType
  #     blank.type
  #   end

  #   def name? : Term::Sym?
  #     blank.name?
  #   end

  #   def pattern : Pattern
  #     IsA[type] - AssignSingleton[name?]
  #   end
  # end

  subclass LiteralCall, term : Term do
    def pattern : Pattern
      Match::Literal[@term]
    end
  end

  subclass PartitionCall, items : Parseout, pairs : Parseout do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @items.walk(fn)
      @pairs.walk(fn)
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(items: @items.map(guidance, fn), pairs: @pairs.map(guidance, fn)))
    end

    def pattern : Pattern
      IsA[:dict] - Mux[
        JustItems[] - items.pattern,
        JustPairs[] - pairs.pattern,
      ]
    end

    def key? : Term?
      @items.key?
    end
  end

  subclass LetCall, name : Term, arg : Parseout do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @arg.walk(fn)
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(arg: @arg.map(guidance, fn)))
    end

    def pattern : Pattern
      Mux[@arg.pattern, Assign[@name]]
    end

    def key? : Term?
      @arg.key?
    end
  end

  subclass EdgeCall, arg : Blank do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @arg.walk(fn)
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(arg: @arg.map(guidance, fn)))
    end

    def pattern : Pattern
      IsEdge[arg.type] - Assign[arg.name?]
    end
  end

  subclass ReleaseCall, arg : Parseout do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @arg.walk(fn)
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(arg: @arg.map(guidance, fn)))
    end

    def pattern : Pattern
      Release[] - @arg.pattern
    end
  end

  subclass PairStarCall, name : Term, v : Parseout do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @v.walk(fn)
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(v: @v.map(guidance, fn)))
    end

    def pattern : Pattern
      IsA[:dict] - PairScan[@name, @v.pattern]
    end
  end

  subclass EitherCall, branches : Array(Parseout) do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @branches.each(&.walk(fn))
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(branches: @branches.map(&.map(guidance, fn).as(Parseout))))
    end

    def pattern : Pattern
      DynChoice.new(@branches.map(&.pattern.as(Pattern)))
    end
  end

  subclass KeypoolCall, whitelist : Set(Term) do
    def pattern : Pattern
      IsA[:dict] - KeyWhitelist[@whitelist]
    end
  end

  subclass ExcludeCall, blacklist : Set(Term), ward : Parseout do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @ward.walk(fn)
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(ward: @ward.map(guidance, fn)))
    end

    def pattern : Pattern
      Blacklist[@blacklist] - @ward.pattern
    end
  end

  subclass PairCall, source : Term::Sym, value : Parseout do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @value.walk(fn)
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(value: @value.map(guidance, fn)))
    end

    def pattern : Pattern
      IsA[:dict] - Pair[source, @value.pattern]
    end
  end

  subclass NatCall, min : Term::Num?, max : Term::Num?, bits : Term::Num? do
    def pattern : Pattern
      pattern = IsA[:number] - IsNat[]
      pattern -= DynMux[
        {@min.try { |min| Compare[min, :gte].as(Pattern) },
         @max.try { |max| Compare[max, :lte].as(Pattern) },
         @bits.try { |bits| Compare[Term[2]**bits, :lt].as(Pattern) },
        }.to_a.compact,
      ]
    end
  end

  subclass DepCall, name : Term do
    def pattern : Pattern
      Dep[name]
    end
  end

  subclass Dict, items : Array(Item), pairs : Array(Pair) do
    def walk(fn : Walkable -> Bool) : Nil
      return unless fn.call(self)

      @items.each(&.walk(fn))
      @pairs.each(&.walk(fn))
    end

    def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
      return self unless guidance.call(self)

      fn.call(change(items: @items.map(&.map(guidance, fn).as(Item)), pairs: @pairs.map(&.map(guidance, fn).as(Pair))))
    end

    def pattern : Pattern
      elementary = @items.select(Item::Elementary)

      items = ItemSequence[elementary.map(&.element)]
      pairs = DynMux[@pairs.map(&.pattern.as(Pattern))]

      sketch = 0u64
      walk do |it|
        case it
        when Literal
          sketch = Term::Dict.mix(sketch, it.term)
        when LiteralCall
          sketch = Term::Dict.mix(sketch, it.term)
        when EitherCall, Item::PluralSubpattern, Pair::Optional
          # Do not descend into branches/plural subpatterns (zero|more branch).
          # We don't know which symbol it's going to be (and whether it's going
          # to be any in case of plural subpattern) so we cannot "require"
          # anything them.
          next false
        end

        true
      end

      IsA[:dict] - SketchSupersetOf[sketch] - elementary.sum(&.bounds) - @pairs.sum(&.bounds) - items - pairs
    end

    def key? : Term?
      return unless first = @items.first?

      first.key?
    end

    abstract struct Item
      include Walkable

      def key? : Term?
      end

      module Elementary
        abstract def bounds : ItemsBounds
        abstract def element : ItemSequence::Element
      end

      substruct Singular, item : Parseout do
        include Elementary

        def walk(fn : Walkable -> Bool) : Nil
          return unless fn.call(self)

          @item.walk(fn)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(change(item: @item.map(guidance, fn)))
        end

        def bounds : ItemsBounds
          ItemsBounds[eq: 1]
        end

        def element : ItemSequence::Element
          ItemSequence::Element::One.new(@item.pattern)
        end

        def key? : Term?
          item = @item

          while item.is_a?(LetCall)
            item = item.arg
          end

          case item
          when Literal, LiteralCall
            item.term
          end
        end
      end

      substruct PluralLet, name : Term, body : Parseout do
        include Elementary

        def walk(fn : Walkable -> Bool) : Nil
          return unless fn.call(self)

          @body.walk(fn)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(change(body: @body.map(guidance, fn)))
        end

        def bounds : ItemsBounds
          ItemsBounds[eq: 1]
        end

        def element : ItemSequence::Element
          ItemSequence::Element::PluralLet.new(@name, @body.pattern)
        end

        def key? : Term?
          body = @body

          while body.is_a?(LetCall)
            body = body.arg
          end

          case body
          when Literal, LiteralCall
            body.term
          end
        end
      end

      substruct PluralSubpattern, name : Term, sequence : Array(Item) do
        include Elementary

        def walk(fn : Walkable -> Bool) : Nil
          return unless fn.call(self)

          @sequence.each &.walk(fn)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(change(sequence: @sequence.map(&.map(guidance, fn).as(Item))))
        end

        def bounds : ItemsBounds
          ItemsBounds[min: @sequence.size, max: Bounds::UNBOUNDED]
        end

        def element : ItemSequence::Element
          ItemSequence::Element::ManyGroup.new(name, @sequence.select(Elementary).map(&.element))
        end
      end

      substruct PluralSubseq, type : TermType, ref : Term::Sym do
        include Elementary

        def walk(fn : Walkable -> Bool) : Nil
          fn.call(self)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(self)
        end

        def bounds : ItemsBounds
          ItemsBounds[eq: Bounds::UNBOUNDED]
        end

        def element : ItemSequence::Element
          ItemSequence::Element::LitSeq.new(IsA[@type], @ref)
        end
      end

      substruct PluralSymbolic, type : TermType, contenders : Array(Term::Sym) do
        include Elementary

        def walk(fn : Walkable -> Bool) : Nil
          fn.call(self)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(self)
        end

        def mincount
          @contenders.count { |contender| (blank = contender.blank?) && blank.one? }
        end

        def bounds : ItemsBounds
          ItemsBounds[min: mincount, max: Bounds::UNBOUNDED]
        end

        def element : ItemSequence::Element
          ItemSequence::Element::ManyEvenly.new(IsA[@type], @contenders)
        end
      end

      substruct Gap, threshold : Parseout do
        include Elementary

        def walk(fn : Walkable -> Bool) : Nil
          return unless fn.call(self)

          @threshold.walk(fn)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(change(threshold: @threshold.map(guidance, fn)))
        end

        def bounds : ItemsBounds
          ItemsBounds[min: 0, max: Bounds::UNBOUNDED]
        end

        def element : ItemSequence::Element
          ItemSequence::Element::Gap.new(threshold.pattern)
        end
      end

      substruct Slot, name : Term, kind : Kind, type : TermType do
        enum Kind : UInt8
          Singular
          Plural0
          Plural1
        end

        def walk(fn : Walkable -> Bool) : Nil
          fn.call(self)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(self)
        end
      end
    end

    abstract struct Pair
      include Walkable

      substruct Required, key : Term, value : Parseout do
        def walk(fn : Walkable -> Bool) : Nil
          return unless fn.call(self)

          @value.walk(fn)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(change(value: @value.map(guidance, fn)))
        end

        def bounds
          PairsBounds[eq: 1]
        end

        def pattern : Pattern
          At[@key] - @value.pattern
        end
      end

      substruct Optional, key : Term, default : Term, value : Parseout do
        def walk(fn : Walkable -> Bool) : Nil
          return unless fn.call(self)

          @value.walk(fn)
        end

        def map(guidance : Walkable -> Bool, fn : Walkable -> Walkable) : Walkable
          return self unless guidance.call(self)

          fn.call(change(value: @value.map(guidance, fn)))
        end

        def bounds
          PairsBounds[min: 0, max: 1]
        end

        def pattern : Pattern
          AtDefault[@key, @default] - @value.pattern
        end
      end
    end
  end

  struct Context
    def initialize
      @defined = Set(Term::Sym).new
    end

    def defined?(name : Term::Sym) : Bool
      name.in?(@defined)
    end

    def define(name : Term::Sym) : Nil
      @defined << name
    end
  end

  private SPECIAL_RELEASE   = Term[:"%release"]
  private SPECIAL_LITERAL   = Term[:"%literal"]
  private SPECIAL_LET       = Term[:"%let"]
  private SPECIAL_LET_STAR  = Term[:"%let*"]
  private SPECIAL_PAIR      = Term[:"%pair"]
  private SPECIAL_PAIR_STAR = Term[:"%pair*"]
  private SPECIAL_MANY      = Term[:"%many"]
  private SPECIAL_GAP       = Term[:"%gap"]
  private SPECIAL_DEFAULT   = Term[:"%default"]
  private SPECIAL_PARTITION = Term[:"%partition"]
  private SPECIAL_EITHER    = Term[:"%either"]
  private SPECIAL_KEYPOOL   = Term[:"%keypool"]
  private SPECIAL_EXCLUDE   = Term[:"%exclude"]
  private SPECIAL_NAT       = Term[:"%nat"]
  private SPECIAL_DEP       = Term[:"%dep"]
  private SPECIAL_SLOT      = Term[:"%slot"]
  private SPECIAL_NONSELF   = Term[:"%nonself"]

  private def self.special?(ctx, pattern : Term::Dict) : Parseout?
    case {pattern.items.size, pattern.pairs.size, pattern[0]?}
    when {2, 0, SYM_EDGE}
      return unless arg = from(ctx, pattern[1]).as?(Blank)
      return unless arg.type.in?(TermType::Any, *ML::EDGE_ALLOWED_DEFAULT)

      EdgeCall.new(arg)
    when {2, 0, SPECIAL_RELEASE}
      ReleaseCall.new(arg: from(ctx, pattern[1]))
    when {2, 0, SPECIAL_LITERAL}
      LiteralCall.new(term: pattern[1])
    when {3, 0, SPECIAL_PARTITION}
      PartitionCall.new(items: from(ctx, pattern[1]), pairs: from(ctx, pattern[2]))
    when {3, 0, SPECIAL_LET}
      LetCall.new(name: pattern[1], arg: from(ctx, pattern[2]))
    when {3, 0, SPECIAL_PAIR}
      k = pattern[1]
      return unless k.type.symbol?
      k = k.unsafe_as_sym
      return if (blank = k.blank?).nil? || blank.poly? || blank.typed?
      return unless source = blank.name?
      # Even though we need k_ to be defined before we can match it, we do not
      # check this here because it is rather hard, especially given cases like
      # `(_* ¦ (%pair k_ v_)) (get k_)` which we want to support, vs. easier
      # cases like `(get k_) (_* ¦ (%pair k_ v_))` for which we could have
      # done a check at pattern compile-time.
      PairCall.new(source, value: from(ctx, pattern[2]))
    when {3, 0, SPECIAL_PAIR_STAR}
      k = pattern[1]
      v = pattern[2]

      PairStarCall.new(k, from(ctx, v))
    when {2.., 0, SPECIAL_EITHER}
      branches = pattern.items
        .move(1)
        .map { |branch| from(ctx, branch).as(Parseout) }

      EitherCall.new(branches)
    when {1.., 0, SPECIAL_KEYPOOL}
      keylist = pattern.items.move(1)

      KeypoolCall.new(keylist.to_set)
    when {3.., 0, SPECIAL_EXCLUDE}
      blacklist = pattern.items.move(1).upto(pattern.items.end - 1)

      ExcludeCall.new(blacklist.to_set, from(pattern.items.last))
    when {1, 0..3, SPECIAL_NAT}
      min, max, bits = pattern[:min]?, pattern[:max]?, pattern[:bits]?
      return unless pattern.size == {min, max, bits}.count { |opt| !opt.nil? } + 1
      return unless min.nil? || min.type.number?
      return unless max.nil? || max.type.number?
      return unless bits.nil? || bits.type.number?

      min = min.try(&.unsafe_as_n)
      max = max.try(&.unsafe_as_n)
      bits = bits.try(&.unsafe_as_n)

      NatCall.new(min, max, bits)
    when {2, 0, SPECIAL_DEP}
      DepCall.new(name: pattern[1])
    when {2, 0, SPECIAL_NONSELF}
      from(ctx, pattern[1])
    end
  end

  private def self.dictitems(ctx, items : Term::Dict::ItemsView) : Array(Dict::Item)
    pitems = Array(Dict::Item).new(items.size)

    until items.empty?
      item0 = items.first.downcast

      # Process "%many" calls. They're only available in the context of a
      # dictionary pattern.
      if item0.is_a?(Term::Dict) && item0.itemsonly?
        case {item0.size, item0[0]?}
        when {3.., SPECIAL_MANY}
          call = item0.items
          pitems << Dict::Item::PluralSubpattern.new(name: call[1], sequence: dictitems(ctx, call.move(2)))
          items = items.move(1)
          next
        when {2, SPECIAL_GAP}
          pitems << Dict::Item::Gap.new(threshold: from(item0[1]))
          items = items.move(1)
          next
        when {2, SPECIAL_SLOT}
          arg = item0[1]
          pitem = nil

          if arg.type.symbol? && (blank = arg.blank?)
            unless name = blank.name? # We're free to skip an unnamed slot, it makes no sense.
              items = items.move(1)
              next
            end

            if blank.poly? && blank.one?
              pitem = Dict::Item::Slot.new(name: name.upcast, kind: :plural1, type: blank.type)
            elsif blank.poly?
              pitem = Dict::Item::Slot.new(name: name.upcast, kind: :plural0, type: blank.type)
            else
              pitem = Dict::Item::Slot.new(name: name.upcast, kind: :singular, type: blank.type)
            end
          end

          pitem ||= Dict::Item::Slot.new(name: arg, kind: :singular, type: :any)
          pitems << pitem

          items = items.move(1)
          next
        when {3, SPECIAL_LET_STAR}
          pitems << Dict::Item::PluralLet.new(name: item0[1], body: from(ctx, item0[2]))
          items = items.move(1)
          next
        end
      end

      # Process blanks.
      if item0.is_a?(Term::Sym) && (blank0 = item0.blank?)
        name = blank0.name?

        # Parse polyblank patterns.
        if blank0.poly?
          # Polyblanks that reference defined blanks are simple, literal
          # subsequence matches. Compile them as such.
          if name && ctx.defined?(name)
            pitems << Dict::Item::PluralSubseq.new(blank0.type, name)
            items = items.move(1)
            next
          end

          items = items.move(1)
          contenders = [item0]

          while item1 = items.first?
            item1 = item1.downcast
            break unless item1.is_a?(Term::Sym) && (blank1 = item1.blank?) && blank1.poly?
            break unless {blank0.one?, blank0.type} == {blank1.one?, blank1.type}

            items = items.move(1)
            contenders << item1
          end

          pitems << Dict::Item::PluralSymbolic.new(blank0.type, contenders)
          next
        end

        if name
          # Consider the blank as defined from this point onward.
          ctx.define(name)
        end
      end

      # Process mono patterns (patterns whose span is always one item).
      pitems << Dict::Item::Singular.new(from(ctx, item0))
      items = items.move(1)
    end

    pitems
  end

  private def self.dictpairs(ctx, pairs : Term::Dict) : Array(Dict::Pair)
    ppairs = Array(Dict::Pair).new(pairs.size)

    pairs.each_entry do |key, value|
      value = value.downcast

      # Parse optional pairs (%default <default value> <pattern>)
      if value.is_a?(Term::Dict) && value.itemsonly? && value.size == 3 && value[0] == SPECIAL_DEFAULT
        ppairs << Dict::Pair::Optional.new(key, default: value[1], value: from(ctx, value[2]))
        next
      end

      ppairs << Dict::Pair::Required.new(key, from(ctx, value))
    end

    ppairs
  end

  private def self.dict?(ctx, pattern : Term::Dict) : Parseout
    Dict.new(items: dictitems(ctx, pattern.items), pairs: dictpairs(ctx, pattern.pairs))
  end

  def self.from(ctx, pattern : Term::Sym) : Parseout
    if (blank = pattern.blank?) && !blank.poly?
      return Parseout::Blank.new(pattern)
    end

    Parseout::Literal.new(pattern.upcast)
    # blank.poly? ? Parseout::BlankSingleton.new(pattern) :
  end

  def self.from(ctx, pattern : Term::Dict) : Parseout
    special?(ctx, pattern) || dict?(ctx, pattern)
  end

  def self.from(ctx, pattern : ITerm) : Parseout
    Literal.new(pattern.upcast)
  end

  def self.from(ctx, pattern : Term) : Parseout
    from(ctx, pattern.downcast)
  end

  def self.from(term : ITerm) : Parseout
    from(Context.new, term)
  end

  def self.from(term : Term) : Parseout
    from(Context.new, term)
  end

  def self.term(blank : Term::Sym::Blank, context = :toplevel) : Term
    name = blank.name?

    unless blank.poly?
      if name
        return Term.of(:blank, name, type: blank.type.symbolize)
      else
        return Term.of(:blank, type: blank.type.symbolize)
      end
    end

    if context == :toplevel
      Term.of(:"blank/singleton", name, type: blank.type.symbolize, min: blank.one? ? 1 : 0)
    else
      Term.of(:polyblank, name, type: blank.type.symbolize, min: blank.one? ? 1 : 0)
    end
  end

  def self.term(pout : Literal) : Term
    Term.of(:literal, pout.term)
  end

  def self.term(pout : Blank) : Term
    term(pout.source.blank)
  end

  # def self.term(pout : BlankSingleton) : Term
  #   term(pout.source.blank)
  # end

  def self.term(pout : LiteralCall) : Term
    Term.of(:"call/literal", pout.term)
  end

  def self.term(pout : PartitionCall) : Term
    Term.of(:"call/partition", term(pout.items), term(pout.pairs))
  end

  def self.term(pout : LetCall) : Term
    Term.of(:"call/let", pout.name, term(pout.arg))
  end

  def self.term(pout : EdgeCall) : Term
    Term.of(:"call/edge", term(pout.arg))
  end

  def self.term(pout : ReleaseCall) : Term
    Term.of(:"call/release", term(pout.arg))
  end

  def self.term(pout : PairStarCall) : Term
    Term.of(:"call/pair*", pout.name, term(pout.v))
  end

  def self.term(pout : EitherCall) : Term
    Term.of(:"call/either", term(pout.branches))
  end

  def self.term(pout : KeypoolCall) : Term
    Term.of(:"call/keypool", term(pout.whitelist))
  end

  def self.term(pout : ExcludeCall) : Term
    Term.of(:"call/exclude", term(pout.blacklist), term(pout.ward))
  end

  def self.term(pout : PairCall) : Term
    Term.of(:"call/pair", term(pout.source.blank), term(pout.value))
  end

  def self.term(pout : NatCall) : Term
    Term.of(:"call/nat", min: pout.min, max: pout.max, bits: pout.bits)
  end

  def self.term(pout : DepCall) : Term
    Term.of(:"call/dep", pout.name)
  end

  def self.term(pout : Dict) : Term
    Term.of(:dict, term(pout.items), term(pout.pairs))
  end

  def self.term(pitem : Dict::Item::Singular) : Term
    Term.of(:singular, term(pitem.item))
  end

  def self.term(pitem : Dict::Item::PluralSubpattern) : Term
    Term.of(:"plural/subpattern", pitem.name, term(pitem.sequence))
  end

  def self.term(pitem : Dict::Item::PluralSubseq) : Term
    # FIXME: is ref a blank?
    Term.of(:"plural/subseq", pitem.type.symbolize, pitem.ref)
  end

  def self.term(pitem : Dict::Item::PluralSymbolic) : Term
    Term.of(:"plural/symbolic", pitem.type.symbolize, pitem.contenders.map { |contender| term(contender.blank, context: :dict) })
  end

  def self.term(pitem : Dict::Item::Gap) : Term
    Term.of(:"plural/gap", term(pitem.threshold))
  end

  def self.term(pitem : Dict::Item::Slot) : Term
    Term.of(:"slot", pitem.name, pitem.kind.symbolize, pitem.type.symbolize)
  end

  def self.term(ppair : Dict::Pair::Required) : Term
    Term.of(:required, ppair.key, term(ppair.value))
  end

  def self.term(ppair : Dict::Pair::Optional) : Term
    Term.of(:required, ppair.key, ppair.default, term(ppair.value))
  end

  def self.term(collection : Array(T)) : Term forall T
    Term::Dict.build do |commit|
      collection.each do |element|
        commit.with(commit.size, term(element))
      end
    end.upcast
  end

  def self.term(terms : Set(T)) : Term forall T
    Term::Dict.build do |commit|
      terms.each do |term|
        commit.with(term(term), true)
      end
    end.upcast
  end

  def self.term(term : Term) : Term
    term
  end
end
