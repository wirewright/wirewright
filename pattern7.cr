# ┌──────────────────────────┬───────┬─────────┬───────┬────────────┬──────────┬───────┐
# │            P             │ Norm  │ Compile │ Match │ Optimize   │ Backmap  │ Ubase │  docs
# ├──────────────────────────┼───────┼─────────┼───────┼────────────┼──────────┼───────┤
# │ type                     │   +   │   +     │   +   │            │    ·     │       │   +
# │ literal                  │   +   │   +     │   +   │            │    ·     │       │   +
# │ literal dict             │       │         │   ~   │            │    ·     │       │   +
# │ blank                    │   +   │   +     │   +   │            │    ~     │       │   +
# │ itemsonly                │   +   │   +     │   +   │            │    ·     │       │
# │ pairsonly                │   ~   │   ~     │   ~   │            │    ·     │       │
# │ bounds                   │       │         │   ~   │            │    ·     │       │
# │ sketch                   │       │         │   ~   │            │    ·     │       │
# │ %literal                 │   +   │   +     │   +   │            │    ·     │       │   +
# │ %partition               │   +   │   +     │   +   │            │    ·     │       │   +
# │ %let                     │   +   │   +     │   +   │            │    ~     │       │
# │ %edge                    │   +   │   +     │   +   │            │    ~     │       │
# │ %any                     │   ~   │   ~     │   ~   │            │    ·     │       │
# │ %any°                    │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %all                     │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %keypool                 │   +   │   +     │   +   │            │    ·     │       │
# │ %not                     │   +   │   +     │   +   │            │    ·     │       │
# │ %layer                   │   +   │   +     │   +   │            │    ~     │       │
# │ %number                  │   +   │   +     │   +   │            │    ·     │       │
# │ %nonself                 │   +   │   ·     │   ·   │     ·      │    ·     │   ·   │
# │ %string                  │       │         │       │            │          │       │
# │ %string date             │       │         │       │            │          │       │
# │ %string decimal          │       │         │       │            │          │       │
# │ %string json             │       │         │       │            │          │       │
# │ %string csv              │       │         │       │            │          │       │
# │ %string uri              │       │         │       │            │          │       │
# │ %pipe: + - * / d m **    │   +   │   +     │   +   │            │    ·     │       │
# │ %pipe: span tally        │   +   │   +     │   +   │            │    ·     │       │
# │ %pipe: map               │   +   │   +     │   +   │            │    ·     │       │
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
# │ %dict: %pair/required    │   +   │   +     │   +   │            │    ~     │       │
# │ %dict: %pair k %optional │   +   │   +     │   +   │            │    ~     │       │
# │ %dict: pair %- _         │   +   │   +     │   +   │            │    ·     │       │
# │ %dict: pair %- _ keyp    │   +   │   +     │   +   │            │    ·     │       │
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
# │ %new                     │   ~   │   ~     │   ~   │            │    ~     │       │
# └──────────────────────────┴───────┴─────────┴───────┴────────────┴──────────┴───────┘
# + confident
# ~ will work
# · not needed

# TODO: we're pure so our hash must be deterministic, regardless of threats! Use fnv1a or something similar & fast
#       as a hashing algorithm for terms! E.g. the order of (keys (x: 1 y: 2)) must be the same across all machines&runs.

# --- After the above & tests are in place:

# NOTE NOTE NOTE::: optimizations described below we should be able to turn off for testing. Tests
#  should run with each combination of the optimizations. Since any of those, if forced, can obstruct
#  some path to a buggy patch of code in the unoptimized portion.
#
# - Two main optimizations we're going to use is memoization (where possible, determined
# at compile-time?) and backjumping to assignment owner on assignment conflict. These are
# the optimizations on the "complex patterns part".

# - At this point we should be able to implement %string stuff. It includes some advanced
#   keypath communication & caching so that must be implemented along the way.
# - At this point we should be able to detect and compile *recursively simple patterns*.
#
# Recursively simple patterns (determined at compile-time) should (?) be compiled into
# a simpler operator hierarchy (e.g. Simple or something like that) that skips feedback etc.
# Simple patterns should be optimized down to nanoseconds. An example of a simple
# pattern is (+ a_number b_number). It should match in perhaps ~30ns + 2 * dict with().
#
# Ideally Simple should be embeddable into the constraint system, perhaps via some sort of
# a "bridge" node.
#
# - Required&easy optimizations: (_*), (xs_*), (¦ _), (¦ xs_), (_* ¦ _), (_* ¦ xs_). (xs_* x_), (x_ xs_*), (fst_ _* lst_), etc.
#
# - Stuff like (_* ¦ _ x: (%optional 0 x_)) should result in a single PairRequired node. Nothing more.
#   This would be a good sign the optimizer is doing what it should.
#
# - If we detect that no pattern-matching features are used in a dictionary, that dictionary
#   should be compiled into a Literal().
#
#
# - At this point we should be able to determine the following:
#   sketch subset from pattern (dict)
#   specificity of pattern
#   max det&indet depth of pattern
#   max det&indet breadth of pattern

###

# Having these in place, we'll be able to interop with the indexing infra & what would become Nitrene.
#
# - At this point we should be able to determine the pattern's *skeleton* from its normalized version.
# - At this point we should be able to compile the skeleton to Ubases.
#
#   The pattern's skeleton consists of literals reachable from the root of the pattern. it can also
#   include types. I don't think there's a point in further overcomplicating the matter.
#
# This would connect the pattern system to pttrie5, a great feat.
#
# (Not for long though since pttrie5 is probably going to be ditched in favor of a single-set-based solution
#  since the latter offers so much)

###

# TODO: string patterns
# We should be able to treat strings as bytestrings OR unicode under the hood. Thus string patterns (%string "...")
# should have bytes as their basic unit, and even below (bits), like Erlang's bitvectors. OR unicode characters,
# more like Raku.

# TODO: support
# ;; Moves all (+ 1 2) etc. into additions field in context.
# (context (_* ⭳s) `n←(+ a_number b_number)`)
#   <=> { s: (add lhs: →a rhs: →b), (n): () }
#
# ;; ^^^ This one would require some sort of caching because it's all DfsFirst's,
# ;;     and they're expensive! We need to remember where we last stopped for all
# ;;     First's along with some identity, to be able to restore from where we've
# ;;     stopped. Otherwise all of this is going to explode in complexity (the O one).
#
# Note that we remove n, this may not be always appropriate! Instead you can mark:
#
# (context (_* ⭳s) `(+ a_number b_number)` ¦ _ -processed-adds)
#   <=>  { s: (add lhs: #a rhs: #b) } ;; Runs on loci
#   <=>. { processed-adds: true } ;; Runs on the resulting context
#
# `...` is a shorthand for (%dig (%locus ...))

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

require "./wirewright"

include Ww

struct ::Ww::Term::M0::PairSchema
  alias Rule = Required | Optional
  alias Check = MatchesAny | IntBounds

  record Required, key : Term, check : Check do
    def validated?(dict : Term::Dict) : Term::Dict?
      return unless value = dict[key]?
      return unless check.sat?(value)
      dict
    end
  end

  record Optional, key : Term, check : Check, default : Term do
    def validated?(dict : Term::Dict) : Term::Dict?
      unless value = dict[key]?
        return dict.with(key, default)
      end

      return unless check.sat?(value)

      dict
    end
  end

  record MatchesAny, options : Array(Term) do
    def sat?(value : Term) : Bool
      options.any? { |option| !!M0.match?(option, value) }
    end
  end

  record IntBounds, min : Term::Num, max : Term::Num do
    def sat?(value : Term) : Bool
      return false unless n = value.as_n?

      n.whole? && n.in?(min..max)
    end
  end

  alias Predicate = Term::Dict -> Bool

  def initialize
    @rules = [] of Rule
    @predicates = [] of Predicate
  end

  macro where(&block)
    check do |%input|
      {% for arg in block.args %}
        {{arg.id}} = %input[{{arg.symbolize}}]? || next true
      {% end %}

      {{yield}}
    end
  end

  def self.build(&) : PairSchema
    with schema = new yield

    schema
  end

  def check(range : Range)
    IntBounds.new(Term[range.begin], Term[range.end])
  end

  def check(allowed : Tuple)
    MatchesAny.new([*allowed.map { |value| Term.of(value) }])
  end

  def key(key, *, values)
    @rules << Required.new(Term.of(key), check(values))
  end

  def key(key, *, values, default)
    @rules << Optional.new(Term.of(key), check(values), Term.of(default))
  end

  def check(&fn : Predicate) : Nil
    @predicates << fn
  end

  def validated?(term : Term) : Term::Dict?
    return unless dict0 = term.as_d?
    return unless dict0.pairsonly?

    good = dict0
    bad = dict0.transaction do |commit|
      @rules.each do |rule|
        return unless good = rule.validated?(good)

        commit.without(rule.key)
      end
    end

    return unless bad.empty?
    return unless @predicates.all?(&.call(dict0))

    good
  end
end

module Search::Result
  include Enumerable(Term)

  alias Any = Item | Pair | ItemStrip

  record Item, term : Term, keypath : KeypathTip? do
    include Result

    def each(& : Term ->) : Nil
      yield term
    end

    def sequence
      ItemStrip.new(Term[{term}].items, keypath)
    end
  end

  record Pair, k : Term, v : Term, keypath : {KeypathTip, KeypathTip}? do
    include Result

    def each(& : Term ->) : Nil
      yield k
      yield v
    end
  end

  record ItemStrip, view : Term::Dict::ItemsView, keypath : KeypathTip? do
    include Result

    def each(& : Term ->) : Nil
      view.each { |item| yield item }
    end

    def +(offset : Int)
      ItemStrip.new(view + offset, keypath.try(&.forward))
    end
  end
end

module Search
  enum Part : UInt8
    ItemsOrdered
    ItemsUnordered
    Keys
    Values
    PairValues
  end

  module Spec
    record Scan, stride = 1u16
    record Dfs, part = Part::ItemsOrdered, depth0 = false, maxdepth = 0u16
    record Bfs, part = Part::ItemsOrdered, depth0 = false, maxdepth = 0u16
    record Entries

    def self.deeper(spec : Dfs | Bfs)
      spec.maxdepth.zero? ? spec : spec.copy_with(maxdepth: spec.maxdepth - 1)
    end
  end

  alias Response = Accept.class | Reject.class | Stop.class

  # Signals to the traversal logic that an item was accepted.
  module Accept
  end

  # Signals to the traversal logic that an item was rejected.
  module Reject
  end

  # Signals to the traversal logic that traversal must stop immediately.
  module Stop
  end

  def self.visit(dict, part : Part, *, keypath = nil, &)
    case part
    in .items_ordered?
      dict.items.each_with_index do |item, index|
        yield item, keypath.try &.value(index)
      end
    in .items_unordered?
      dict.each_item_with_index do |item, index|
        yield item, keypath.try &.value(index)
      end
    in .keys?
      dict.each_entry { |k, _| yield k, keypath.try &.key(k) }
    in .values?
      dict.each_entry do |key, value|
        yield value, keypath.try(&.value(key))
      end
    in .pair_values?
      dict.pairs.each_entry do |key, value|
        yield value, keypath.try(&.value(key))
      end
    end
  end

  def self.traverse(term : Term, spec : Spec::Scan, *, keypath keypath0 = nil, &fn : Result::Any -> Response) : Nil
    return unless dict = term.as_d?

    feed = dict.items
    index = 0

    while spec.stride <= feed.size
      window = feed.begin.grow(spec.stride)
      item = Result::ItemStrip.new(window, keypath: keypath0 ? keypath0.value(Term.of(index)) : nil)

      case fn.call(item)
      in Accept.class
        feed = feed.move(spec.stride)
        index += spec.stride
      in Reject.class
        feed = feed.move(1)
        index += 1
      in Stop.class
        return
      end
    end
  end

  private def self.dfs?(dict, spec, keypath0, fn) : Bool?
    visit(dict, spec.part, keypath: keypath0) do |value, keypath1|
      item = Result::Item.new(value, keypath1)

      case fn.call(item)
      in Accept.class
      in Reject.class
      in Stop.class
        return true
      end

      next if spec.maxdepth == 1
      next unless child = value.as_d?

      return true if dfs?(child, Spec.deeper(spec), keypath1, fn)
    end
  end

  def self.traverse(term : Term, spec : Spec::Dfs, *, keypath keypath0 = nil, &fn : Result::Any -> Response) : Nil
    if spec.depth0
      item = Result::Item.new(term, keypath: keypath0)

      case fn.call(item)
      in Accept.class, Reject.class
      in Stop.class
        return
      end
    end

    return unless dict = term.as_d?

    dfs?(dict, spec, keypath0, fn)
  end

  private def self.iddfs1(dict, spec, keypath0, fn, depth)
    if depth.zero?
      visit(dict, spec.part, keypath: keypath0) do |term, keypath1|
        item = Result::Item.new(term, keypath1)

        case fn.call(item)
        in Accept.class, Reject.class
        in Stop.class
          return :stop
        end
      end

      return :next
    end

    # Carry out a vote for/against reaching the bottom.
    bottom = total = 0

    visit(dict, spec.part, keypath: keypath0) do |term, keypath1|
      total += 1

      unless child = term.as_d?
        bottom += 1
        next
      end

      case iddfs1(child, spec, keypath1, fn, depth - 1)
      when :bot
        bottom += 1
      when :stop
        return :stop
      when :next
      else
        unreachable
      end
    end

    # Unanimous vote for bottom means we're at the bottom. Otherwise continue.
    bottom == total ? :bot : :next
  end

  private def self.iddfs0(dict, spec, keypath0, fn)
    maxdepth = spec.maxdepth.zero? ? nil : spec.maxdepth

    (0...maxdepth).each do |depth|
      case iddfs1(dict, spec, keypath0, fn, depth)
      when :bot, :stop
        break
      when :next
      else
        unreachable
      end
    end
  end

  def self.traverse(term : Term, spec : Spec::Bfs, *, keypath keypath0 = nil, &fn : Result::Any -> Response) : Nil
    if spec.depth0
      item = Result::Item.new(term, keypath: keypath0)

      case fn.call(item)
      in Accept.class, Reject.class
      in Stop.class
        return
      end
    end

    return unless dict = term.as_d?

    iddfs0(dict, spec, keypath0, fn)
  end

  def self.traverse(term : Term, spec : Spec::Entries, *, keypath keypath0 = nil, &fn : Result::Any -> Response) : Nil
    return unless dict = term.as_d?

    dict.each_entry do |key, value|
      item = Result::Pair.new(key, value, keypath0 ? {keypath0.key(key), keypath0.value(key)} : nil)

      case fn.call(item)
      in Accept.class, Reject.class
      in Stop.class
        return
      end
    end
  end
end

# TODO: ???: break into a u8 type field followed by some sort of variable length payload
# so that size is conserved (vs. `Any`). Avoid allocations for most-used operators/
# configs by packing. Instead of being classes, these things should be structs that
# "look" at a variably sized payload and "interpret" it somehow. Not sure how to avoid a
# match() "megamethod" though. Maybe through a macro?
#
# Additionally, we must use u32 (or even u16) array indices for operator nodes rather
# than a pointer. Thus an operator pool.
module ::Ww::Term::M1::Operator
  alias Any = Pass | Num | Sym | Boolean | Dict | SketchSubset | Literal | Capture | Itemspart | Partition | EdgeUntyped | EdgeTyped | Choices | EitherSource | Keypool | Span | Tally | Bin | Both | Not | Layer | ScanFirst | ScanSource | ScanAll | ScanAllIsolated | DfsFirst | DfsSource | DfsAllIsolated | DfsAll | BfsFirst | BfsAllIsolated | BfsAll | PairRequired | PairOptional | PairAbsent | PairAbsentKeypath | NegativePair | NegativePairKeypath | Value | NegativeValue | NegativeValueKeypath | EntriesFirst | EntriesSource | EntriesAllIsolated | EntriesAll | Str | New | Keypath

  alias Bin = Add | Sub | Mul | Div | Tdiv | Mod | Pow | Map

  alias First = ScanFirst | DfsFirst | BfsFirst | EntriesFirst
  alias Source = DfsSource | ScanSource | EntriesSource
  alias AllIsolated = ScanAllIsolated | DfsAllIsolated | BfsAllIsolated | EntriesAllIsolated
  alias All = ScanAll | DfsAll | BfsAll | EntriesAll

  defcase Pass

  # TODO: split into different objects based on the presence of min, max (options)
  defcase Num, min : Term::Num?, max : Term::Num?, options : Options do
    @[Flags]
    enum Options : UInt8
      MinExcluded
      MaxExcluded
      Whole
    end

    def self.new
      new(min: nil, max: nil, options: :none)
    end

    def self.new(min, max, options : Tuple)
      new(min: min, max: max, options: Options.new(options))
    end
  end

  defcase Sym
  defcase Str
  defcase Boolean
  defcase Dict

  defcase SketchSubset, sketch : Term::Dict::Sketch, successor : Any

  # TODO: split into Entry, Itemsonly, Pairsonly, do not use `Kind`.
  # defcase Dict, min : Magnitude, max : Magnitude, sketch : UInt64, kind : Kind do
  #   enum Kind : UInt8
  #     Entry
  #     Itemsonly
  #     Pairsonly
  #   end

  #   def self.new : Dict
  #     new(Magnitude::INFINITY, Magnitude::INFINITY, 0, :entry)
  #   end

  #   def sizes : Range(Magnitude?, Magnitude?)
  #     Range.new(
  #       min == Magnitude::INFINITY ? nil : min,
  #       max == Magnitude::INFINITY ? nil : max,
  #       exclusive: false
  #     )
  #   end
  # end
  defcase Literal, term : Term
  defcase Capture, capture : Term, successor : Any = Pass.new
  # TODO: have a variant for the extremely frequent Itemspart of Singulars. We waste 40 bytes
  # per item here for such very common arrays, which is extremely, extremely much vs. the 8 bytes
  # that it would take for a singular.
  defcase Itemspart, items : Array(Item::Any)
  defcase Partition, itemspart : Any, pairspart : Any
  defcase Choices, choices : Array(Term)
  defcase EitherSource, a : Any, b : Any
  defcase Both, a : Any, b : Any
  defcase Keypool, keys : Array(Term)

  defcase Span, successor : Any
  defcase Tally, successor : Any

  defcase Add, arg : Term::Num, successor : Any
  defcase Sub, arg : Term::Num, successor : Any
  defcase Mul, arg : Term::Num, successor : Any
  defcase Div, arg : Term::Num, successor : Any
  defcase Tdiv, arg : Term::Num, successor : Any
  defcase Mod, arg : Term::Num, successor : Any
  defcase Pow, arg : Term::Num, successor : Any
  defcase Map, arg : Term::Dict, successor : Any

  defcase Not, blacklist : Term::Dict
  defcase Layer, below : Any, side : Array({Term, Any})

  defcase EdgeUntyped
  defcase EdgeTyped, type : TermType

  alias Scan = ScanFirst | ScanSource | ScanAllIsolated | ScanAll

  defcase ScanFirst, needle : Array(Any)
  defcase ScanSource, needle : Array(Any)
  defcase ScanAllIsolated, capture : Term, needle : Array(Any), min : UInt8, max : UInt8
  defcase ScanAll, capture : Term, needle : Array(Any), selector : Set(Term), exterior : Set(Term), min : UInt8, max : UInt8

  defcase PairRequired, key : Term, value : Any
  defcase PairOptional, key : Term, default : Term, value : Any

  defcase PairAbsent, key : Term
  defcase PairAbsentKeypath, key : Term, name : Term

  defcase NegativePair, key : Term, positive : Any
  defcase NegativePairKeypath, key : Term, positive : Any, name : Term

  defcase Value, capture : Term, tail : Any
  defcase NegativeValue, capture : Term
  defcase NegativeValueKeypath, capture : Term, name : Term

  alias Dfs = DfsFirst | DfsSource | DfsAllIsolated | DfsAll

  defcase DfsFirst, needle : Any, part : Search::Part, depth0 : Bool
  defcase DfsSource, needle : Any, part : Search::Part, depth0 : Bool
  defcase DfsAllIsolated, capture : Term, needle : Any, part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool
  defcase DfsAll, capture : Term, needle : Any, selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool

  alias Bfs = BfsFirst | BfsAllIsolated | BfsAll

  defcase BfsFirst, needle : Any, part : Search::Part, depth0 : Bool
  defcase BfsAllIsolated, capture : Term, needle : Any, part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool
  defcase BfsAll, capture : Term, needle : Any, selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool

  alias Entries = EntriesFirst | EntriesSource | EntriesAllIsolated | EntriesAll

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

  defcase EntriesAllIsolated, capture : Term, kop : Any, vop : Any, min : UInt8, max : UInt8 do
    def needle
      [kop, vop]
    end
  end

  defcase EntriesAll, capture : Term, kop : Any, vop : Any, exterior : Set(Term), selector : Set(Term), min : UInt8, max : UInt8 do
    def needle
      [kop, vop]
    end
  end

  defcase New, subjects : Array(Term), pattern : Term
  defcase Keypath, capture : Term
end

alias Magnitude = Float64

# record ::Ww::Term::M1::Magnitude, v : UInt32 do
#   Infinity = new(UInt32::MAX)

#   def self.[](v : UInt32) : Magnitude
#     Magnitude[v]
#   end

#   def self.sum(a : Magnitude, b : Magnitude)
#     Infinity.in?(a, b) ? Infinity : Magnitude[a.v + b.v]
#   end

#   def self.product(a : Magnitude, b : Magnitude)
#     Infinity.in?(a, b) ? Infinity : Magnitude[a * b]
#   end

#   def self.max(a : Magnitude, b : Magnitude)
#     Math.max(a.v, b.v) # Infinity will win if it's there
#   end

#   def self.weak(a : Magnitude)

#   end
# end

module ::Ww::Term::M1::Operator::Item
  alias Any = Singular | Slot | Plural | Group | Gap | Optional | Many | Past

  record Singular, tail : Operator::Any
  record Slot, capture : Term
  record Plural, capture : Term?, min : UInt8, max : UInt8, type : TermType, strategy : ExpandStrategy

  record Group, capture : Term, children : Array(Any)
  record Gap, measurer : Operator::Any, strategy : ExpandStrategy
  record Optional, default : Term, tail : Operator::Any
  record Many, capture : Term, children : Array(Any), interior : Set(Term), min : UInt8, max : UInt8
  record Past, children : Array(Any), min : UInt8, max : UInt8, greedy : Bool
end

module ::Ww::Term::M1::Operator::Env
  alias Type = Term::Dict

  def self.append(envs : Array(Type), feedback : Fb::MatchOne)
    envs << feedback.env
  end

  def self.append(envs : Array(Type), feedback : Fb::MatchMany)
    envs.concat(feedback.envs)
  end

  def self.append(envs : Array(Type), feedback : Fb::Mismatch)
  end

  def self.feedback(envs : Indexable(Type), fallback : Type, *, more : Bool = false) : Fb::Any
    case envs.size
    when 0 then Fb::Mismatch.new(fallback)
    when 1 then Fb::MatchOne.new(envs[0], more: more)
    else
      Fb::MatchMany.new(envs.to_a)
    end
  end

  private def self.pluck?(env : Term::Dict, selector : Set(Term)) : Term::Dict?
    Term::Dict.build do |selection|
      selector.each do |key|
        selection.with(key, env[key]? || return)
      end
    end
  end

  def self.captures(envs : Array(Type), selector : Set(Term)) : Term::Dict
    Term::Dict.build do |captures|
      envs.each do |env|
        captures << (pluck?(env, selector) || next)
      end
    end
  end

  def self.domain(envs : Array(Type), capture : Term) : Term::Dict
    Term::Dict.build do |domain|
      envs.each do |env|
        next unless value = env[capture]?

        domain.with(value, true)
      end
    end
  end
end

module ::Ww::Term::M1::Operator::Fb
  alias Any = Response | Request
  alias Response = Match | Mismatch
  alias Match = MatchOne | MatchMany
  alias Request = RequestKeypath

  record MatchOne, env : Env::Type, more : Bool = false do
    def envs
      {env}
    end
  end

  record MatchMany, envs : Array(Env::Type)
  record Mismatch, env : Env::Type
  record RequestKeypath

  def self.lsum(a : Mismatch, b : Mismatch)
    a
  end

  def self.lsum(a : Match, b : Mismatch)
    a
  end

  def self.lsum(a : Mismatch, b : Match)
    b
  end

  def self.lsum(a : MatchOne, b : MatchOne)
    Fb::MatchMany.new([a.env, b.env])
  end

  def self.lsum(a : MatchOne, b : MatchMany)
    Fb::MatchMany.new([a.env].concat(b.envs))
  end

  def self.lsum(a : MatchMany, b : MatchOne)
    lsum(b, a)
  end

  def self.lsum(a : MatchMany, b : MatchMany)
    Fb::MatchMany.new(a.envs + b.envs)
  end
end

# FIXME: make sure to rename env in match to input (because it's input now)

# TODO: have an actual tip here. This object is intended for optimization!
struct KeypathTip
  def initialize(@keypath : Term::Dict)
  end

  def initialize
    initialize(Term[])
  end

  def key(key)
    KeypathTip.new(@keypath.append({:value, key}).append(:self))
  end

  def value(key)
    KeypathTip.new(@keypath.append({:value, key}))
  end

  def value?
    @keypath.size >= 1 && @keypath[@keypath.size - 1, 0] == Term.of(:value)
  end

  def residue(keys : Enumerable(Term))
    command = Term::Dict.build do |commit|
      commit << :residue
      commit.concat(keys)
    end

    KeypathTip.new(@keypath.append(command))
  end

  def forward(n = 1)
    KeypathTip.new(@keypath.morph({@keypath.size - 1, 1, @keypath[@keypath.size - 1, 1] + n}))
  end

  def up(n = 1)
    unless n < @keypath.size
      return KeypathTip.new
    end

    KeypathTip.new(@keypath.transaction do |commit|
      n.times { commit.without(commit.size - 1) }
    end)
  end

  def range(b : Term, e : Term)
    KeypathTip.new(@keypath.append({:range, b, e}))
  end

  # Converts item value tip to range tip with size *size*.
  def span(size)
    b = @keypath[@keypath.size - 1, 1]

    KeypathTip.new(@keypath.morph({@keypath.size - 1, 0, :range}, {@keypath.size - 1, 2, b + size}))
  end

  # Converts range tip to range tip with ordinal *ord*.
  def ord(ord)
    KeypathTip.new(@keypath.morph({@keypath.size - 1, 3, ord}))
  end

  # Converts item value tip to ephemeral tip.
  def ephk(value, ord)
    KeypathTip.new(@keypath.morph({@keypath.size - 1, 0, :ephemeral}, {@keypath.size - 1, 2, ord}, {@keypath.size - 1, 3, value}))
  end

  def ephv(key, default)
    KeypathTip.new(@keypath.append({:ephemeral, key, default}))
  end

  def ephv(key)
    KeypathTip.new(@keypath.append({:ephemeral, key}))
  end

  def dict
    @keypath
  end
end

module ::Ww::Term::M1::Operator
  record Behind, env : Env::Type, domains : Term::Dict, antidomains : Term::Dict, keypath : KeypathTip? do
    def []?(k : Term)
      env[k]?
    end

    def propose?(k : Term, v : Term) : Behind?
      if domain = domains[k]?
        return unless v.in?(domain)
      end

      if antidomain = antidomains[k]?
        return if v.in?(antidomain)
      end

      return unless env1 = env.unify?(k, v)

      copy_with(env: env1)
    end

    def mount(capture : Term)
      return self unless kp = keypath

      copy_with(env: env.morph({:"(keypaths)", capture, kp.dict, true}))
    end

    def mount(capture : Term, & : KeypathTip -> KeypathTip)
      return self unless kp0 = keypath

      kp1 = yield kp0

      copy_with(env: env.morph({:"(keypaths)", capture, kp1.dict, true}))
    end

    def keypath(& : KeypathTip -> KeypathTip)
      return self unless kp = keypath

      copy_with(keypath: yield kp)
    end

    # def descend(k : Term)
    #   return self unless kp = keypath

    #   copy_with(keypath: kp.append(k))
    # end

    # def forward(n = 1)
    #   return self unless kp = keypath

    #   unless last = kp.items.last?
    #     raise "BUG: attempt to move within an empty keypath"
    #   end

    #   unless last = last.as_n?
    #     raise "BUG: attempt to move within a pairspart keypath"
    #   end

    #   copy_with(keypath: kp.with(kp.size - 1, last + n))
    # end

    # def ascend
    #   return self unless kp = keypath

    #   copy_with(keypath: kp.without(kp.size - 1))
    # end

    def goto(dst : KeypathTip?)
      copy_with(keypath: dst)
    end

    # def goto(dst : Nil)
    #   if keypath
    #     raise "BUG: goto() nil keypath in keypath mode"
    #   end

    #   copy_with(keypath: dst)
    # end

    def keypathless
      copy_with(keypath: nil)
    end

    # Domain restriction: *k* must be one of *vs* (the latter is treated as a dict set).
    def one_of(k, vs) : Behind
      if domain = domains[k]?
        copy_with(domains: domains.with(k, domain.xsect(vs)))
      else
        copy_with(domains: domains.with(k, vs))
      end
    end

    # Domain restriction: *k* must **not** be one of *vs* (the latter is treated as a dict set).
    def not(k, vs) : Behind
      if antidomain = antidomains[k]?
        copy_with(antidomains: antidomains.with(k, antidomain | vs))
      else
        copy_with(antidomains: antidomains.with(k, vs))
      end
    end

    def assign(k, v)
      copy_with(env: env.with(k, v))
    end

    def partition(selector)
      lenv = env &- selector
      ldomains = domains &- selector
      lantidomains = antidomains &- selector

      renv = env.pluck(selector)
      rdomains = domains.pluck(selector)
      rantidomains = antidomains.pluck(selector)

      {copy_with(env: lenv, domains: ldomains, antidomains: lantidomains),
       copy_with(env: renv, domains: rdomains, antidomains: rantidomains)}
    end
  end

  # TODO: ahead should be a struct. We do not need to allocate procs for this.

  struct Ahead::Fn
    include Ahead

    def initialize(@fn : Behind -> Fb::Any)
    end

    def initialize(&@fn : Behind -> Fb::Any)
    end

    def call(behind0 : Behind) : Fb::Any
      @fn.call(behind0)
    end
  end

  def match(env, op : Pass, matchee : Term, ahead)
    ahead.call(env)
  end

  private def compare?(a, op, b)
    case op
    when :lt  then a < b
    when :lte then a <= b
    else
      unimplemented
    end
  end

  def match(env, op : Num, matchee : Term, ahead)
    unless n = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    if op.options.whole? && !n.whole?
      return Fb::Mismatch.new(env.env)
    end

    min = op.min
    max = op.max

    if min && !compare?(min, op.options.min_excluded? ? :lt : :lte, n)
      return Fb::Mismatch.new(env.env)
    end

    if max && !compare?(n, op.options.max_excluded? ? :lt : :lte, max)
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(env, op : Str, matchee : Term, ahead)
    unless matchee.type.string?
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(env, op : Sym, matchee : Term, ahead)
    unless matchee.type.symbol?
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(env, op : Boolean, matchee : Term, ahead)
    unless matchee.type.boolean?
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(behind0, op : Dict, matchee : Term, ahead0)
    unless matchee.type.dict?
      return Fb::Mismatch.new(behind0.env)
    end

    ahead0.call(behind0)
  end

  def match(behind0, op : SketchSubset, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && dict.sketch_superset_of?(op.sketch)
      return Fb::Mismatch.new(behind0.env)
    end

    match(behind0, op.successor, matchee, ahead0)
  end

  # def match(env, op : Dict, matchee : Term, ahead)
  #   unless dict = matchee.as_d?
  #     return Fb::Mismatch.new(env.env)
  #   end

  #   valid =
  #     case op.kind
  #     in .itemsonly? then dict.itemsonly?
  #     in .pairsonly? then dict.pairsonly?
  #     in .entry?
  #       true
  #     end

  #   valid &&= dict.size.in?(op.sizes)
  #   valid &&= dict.sketch_superset_of?(op.sketch)

  #   unless valid
  #     return Fb::Mismatch.new(env.env)
  #   end

  #   ahead.call(env)
  # end

  def match(env, op : Literal, matchee : Term, ahead)
    unless matchee == op.term
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(env, op : Capture, matchee : Term, ahead)
    unless env1 = env.propose?(op.capture, matchee)
      return Fb::Mismatch.new(env.env.with(op.capture, matchee))
    end

    env1 = env1.mount(op.capture)

    match(env1, op.successor, matchee, ahead)
  end

  def match(env, op : Itemspart, matchee : Term, ahead)
    unless (dict = matchee.as_d?) && dict.itemsonly?
      return Fb::Mismatch.new(env.env)
    end

    Item.match(env, op.items.to_readonly_slice, dict.items, ahead)
  end

  module Ahead
    macro stackptr(var)
      begin
        %slot = {{var}}.as(Ahead)
        pointerof(%slot)
      end
    end
  end

  struct Ahead::MatchOne
    include Ahead

    def call(behind0 : Behind) : Fb::Any
      Fb::MatchOne.new(behind0.env)
    end
  end

  struct Ahead::Goto
    include Ahead

    def initialize(@keypath : KeypathTip?, @ahead : Ahead*)
    end

    def call(behind0 : Behind)
      @ahead.value.call(behind0.goto(@keypath))
    end
  end

  struct Ahead::Match
    include Ahead

    def initialize(@op : Operator::Any, @matchee : Term, @ahead : Ahead*)
    end

    def call(behind0 : Behind) : Fb::Any
      Operator.match(behind0, @op, @matchee, @ahead.value)
    end
  end

  struct Ahead::ItemStep
    include Ahead

    def initialize(@ord : UInt32, @feed : Item::Feed, @ahead : Item::ItemAhead*)
    end

    def call(behind0 : Behind)
      @ahead.value.call(@ord + 1, @feed.move(1), behind0.keypath(&.forward))
    end
  end

  def match(env, op : Partition, matchee : Term, ahead)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(env.env)
    end

    items, pairs = dict.partition

    # Note: the order here doesn't *really* matter since we're a solver.
    # Even then "heuristically" speaking it would, but nevermind!
    #
    # Visually it's e.g. (+ a_ b_ ¦ x: a_ y: b_) so we're sticking to that order here.
    match(env, op.itemspart, Term.of(items.collect), Ahead::Match.new(op.pairspart, Term.of(pairs), Ahead.stackptr(ahead)))
  end

  def match(env, op : EdgeUntyped, matchee : Term, ahead)
    unless ML.edge?(matchee)
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(env, op : EdgeTyped, matchee : Term, ahead)
    unless ML.edge?(matchee, allowed: {op.type})
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(env, op : Choices, matchee : Term, ahead)
    matchee.in?(op.choices) ? ahead.call(env) : Fb::Mismatch.new(env.env)
  end

  def match(env, op : EitherSource, matchee : Term, ahead)
    a = match(env, op.a, matchee, ahead)
    unless a.is_a?(Fb::Response)
      return a
    end
    b = match(env, op.b, matchee, ahead)
    unless b.is_a?(Fb::Response)
      return b
    end

    Fb.lsum(a, b)
  end

  def match(env, op : Both, matchee : Term, ahead)
    match(env, op.a, matchee, Ahead::Match.new(op.b, matchee, Ahead.stackptr(ahead)))
  end

  def match(env, op : Keypool, matchee : Term, ahead)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(env.env)
    end

    pruned = op.keys.reduce(dict) { |memo, key| memo.without(key) }
    unless pruned.empty?
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  def match(env, op : Span, matchee : Term, ahead)
    unless a = matchee.as_s?
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, Term.of(a.charcount), ahead)
  end

  def match(env, op : Tally, matchee : Term, ahead)
    unless a = matchee.as_d?
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, Term.of(a.size), ahead)
  end

  def match(env, op : Add, matchee : Term, ahead)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, Term.of(a + op.arg), ahead)
  end

  def match(env, op : Sub, matchee : Term, ahead)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, Term.of(a - op.arg), ahead)
  end

  def match(env, op : Mul, matchee : Term, ahead)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, Term.of(a * op.arg), ahead)
  end

  def match(env, op : Div, matchee : Term, ahead)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    begin
      q = Term.of(a / op.arg)
    rescue DivisionByZeroError
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, q, ahead)
  end

  def match(env, op : Tdiv, matchee : Term, ahead)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    begin
      q = Term.of(a // op.arg)
    rescue DivisionByZeroError
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, q, ahead)
  end

  def match(env, op : Mod, matchee : Term, ahead)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    begin
      m = Term.of(a % op.arg)
    rescue DivisionByZeroError
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, m, ahead)
  end

  def match(env, op : Pow, matchee : Term, ahead)
    unless a = matchee.as_n?
      return Fb::Mismatch.new(env.env)
    end

    begin
      c = Term.of(a ** op.arg)
    rescue DivisionByZeroError # e.g. 0^-2
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, c, ahead)
  end

  def match(env, op : Map, matchee : Term, ahead)
    unless v = op.arg[matchee]?
      return Fb::Mismatch.new(env.env)
    end

    match(env, op.successor, v, ahead)
  end

  def match(env, op : Not, matchee : Term, ahead)
    if matchee.in?(op.blacklist)
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

  record Ahead::EntrySeq, matchee : Term, entries : Array({Term, Operator::Any}), cursor : UInt32, ahead : Ahead* do
    include Ahead

    def call(behind0)
      unless entry = entries[cursor]?
        return ahead.value.call(behind0)
      end

      _, successor = entry

      Operator.match(behind0, successor, matchee, copy_with(cursor: cursor + 1))
    end
  end

  def match(behind0, op : Layer, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    # Split the dictionary into a "selection" and "residue". We'll show "selection"
    # to operators on the side of the layer; and "residue" to the operator below it.
    #
    # Assume "residue" is larger than side.
    residue = dict
    selection = Term::Dict.build do |selection|
      residue = dict.transaction do |residue|
        op.side.each do |k, _|
          next unless v = dict[k]?

          selection.with(k, v)
          residue.without(k)
        end
      end
    end

    # Note: entries can depend on each other here and on residue, so instead of simply
    # iterating over the 'side'+'below' of the layer, we'll instead "string" the entries
    # after 'below', one entry after another in a chain of Aheads.
    #
    # The exact order doesn't *really* matter here since we're a solver.
    ahead1 = Ahead::EntrySeq.new(Term.of(selection), op.side, 0, Ahead.stackptr(ahead0))
    ahead2 = Ahead::Goto.new(behind0.keypath, Ahead.stackptr(ahead1))

    match(behind0.keypath(&.residue(op.side.map { |k, _| k })), op.below, Term.of(residue), ahead2)
  end

  def match(env, op : PairRequired, matchee : Term, ahead)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(env.env)
    end

    unless v = dict[op.key]?
      return Fb::Mismatch.new(env.env)
    end

    kp0 = env.keypath

    match(env.keypath(&.value(op.key)), op.value, v, Ahead::Goto.new(kp0, Ahead.stackptr(ahead)))
  end

  def match(env, op : PairOptional, matchee : Term, ahead)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(env.env)
    end

    kp0 = env.keypath

    if value = dict[op.key]?
      case fb = match(env.keypath(&.value(op.key)), op.value, value, Ahead::Goto.new(kp0, Ahead.stackptr(ahead)))
      in Fb::Match, Fb::Request
        return fb
      in Fb::Mismatch
      end
    end

    match(env.keypath(&.ephv(op.key, op.default)), op.value, op.default, Ahead::Goto.new(kp0, Ahead.stackptr(ahead)))
  end

  def match(behind0, op : PairAbsent | PairAbsentKeypath, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    if op.key.in?(dict)
      return Fb::Mismatch.new(behind0.env)
    end

    case op
    in PairAbsent
      behind1 = behind0
    in PairAbsentKeypath
      behind1 = behind0.mount(op.name, &.ephv(op.key))
    end

    ahead0.call(behind1)
  end

  def match(behind0, op : NegativePair | NegativePairKeypath, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    if v = dict[op.key]?
      case fb = match(behind0, op.positive, v, ahead0)
      in Fb::Match # Positive matches, we don't have to do anything.
        return Fb::Mismatch.new(behind0.env)
      in Fb::Mismatch
      in Fb::Request
        return fb
      end
    end

    case op
    in NegativePair
      behind1 = behind0
    in NegativePairKeypath
      behind1 = behind0.mount(op.name, &.ephv(op.key))
    end

    ahead0.call(behind1)
  end

  def search_spec(op : Scan)
    Search::Spec::Scan.new(op.needle.size.to_u16)
  end

  def search_spec(op : Dfs)
    Search::Spec::Dfs.new(op.part, depth0: op.depth0)
  end

  def search_spec(op : Bfs)
    Search::Spec::Bfs.new(op.part, depth0: op.depth0)
  end

  def search_spec(op : Entries)
    Search::Spec::Entries.new
  end

  def match(behind0, op : First, matchee : Term, ahead0)
    memo = Fb::Mismatch.new(behind0.env)

    Search.traverse(matchee, spec: search_spec(op), keypath: behind0.keypath) do |item|
      case memo = Operator.match(behind0, op.needle, item, ahead0)
      in Fb::Match, Fb::Request
        Search::Stop
      in Fb::Mismatch
        Search::Reject
      end
    end

    memo
  end

  def match(behind0, op : Source, matchee : Term, ahead0)
    envs = [] of Env::Type
    reqbox = nil

    Search.traverse(matchee, spec: search_spec(op), keypath: behind0.keypath) do |item|
      case fb = Operator.match(behind0, op.needle, item, ahead0)
      in Fb::Match
        Env.append(envs, fb)

        Search::Accept
      in Fb::Mismatch
        Search::Reject
      in Fb::Request
        reqbox = fb

        Search::Stop
      end
    end

    if request = reqbox
      return request
    end

    Env.feedback(envs, fallback: behind0.env, more: true)
  end

  def match(behind0, op : AllIsolated, matchee : Term, ahead0)
    kp0 = behind0.keypath

    keypaths = behind0.env[:"(keypaths)"]? || Term[]

    captures = Term::Dict.build do |captures|
      reqbox = nil

      Search.traverse(matchee, spec: search_spec(op), keypath: kp0) do |item|
        case fb = Operator.match(behind0, op.needle, item, Ahead::MatchOne.new)
        in Fb::Match
          fb.envs.each do |env|
            keypaths &= env[:"(keypaths)"]? || Term[]

            captures.append(env.without(:"(keypaths)"))
          end

          Search::Accept
        in Fb::Mismatch
          Search::Reject
        in Fb::Request
          reqbox = fb

          Search::Stop
        end
      end

      if request = reqbox
        return request
      end
    end

    if captures.size < op.min || captures.size > op.max > 0
      return Fb::Mismatch.new(behind0.env)
    end

    unless behind1 = behind0.propose?(op.capture, Term.of(captures))
      return Fb::Mismatch.new(behind0.env.with(op.capture, Term.of(captures)))
    end

    behind1 = behind1
      .copy_with(env: behind1.env.with(:"(keypaths)", keypaths))
      .goto(kp0)

    ahead0.call(behind1)
  end

  def match(behind0, op : All, matchee : Term, ahead0)
    kp0 = behind0.keypath

    envs = [] of Env::Type
    reqbox = nil

    keypaths = behind0.env[:"(keypaths)"]? || Term[]

    Search.traverse(matchee, spec: search_spec(op), keypath: kp0) do |item|
      case fb = Operator.match(behind0, op.needle, item, ahead0)
      in Fb::Match
        fb.envs.each do |env|
          keypaths &= env[:"(keypaths)"]? || Term[]
          envs << env
        end

        Search::Accept
      in Fb::Mismatch
        Search::Reject
      in Fb::Request
        reqbox = fb

        Search::Stop
      end
    end

    if request = reqbox
      return request
    end

    # Each feedback environment gives us candidate values for all exterior captures.
    # Thus by looking at all feedback environments at once we can determine (or reduce)
    # the domains of each exterior capture.
    behind1 = behind0

    op.exterior.each do |capture|
      domain1 = Env.domain(envs, capture)
      behind1 = behind1.one_of(capture, domain1)
    end

    captures = Env.captures(envs, selector: op.selector)

    unless behind1 = behind1.propose?(op.capture, Term.of(captures))
      return Fb::Mismatch.new(behind0.env.with(op.capture, Term.of(captures)))
    end

    behind1 = behind1.copy_with(env: behind1.env.with(:"(keypaths)", keypaths))

    fb = ahead0.call(behind1)

    case fb
    in Fb::Match
      # Prune all captures that are inconsistent with exterior assignments
      # in each env.
      consistent = fb.envs.compact_map do |exterior|
        pruned = Term::Dict.build do |commit|
          # Let's not violate captures order!
          captures.items.each do |interior|
            next unless op.exterior.all? { |capture| exterior[capture]?.in?(interior[capture], nil) }

            # Assignments of exterior captures in the interior are consistent with those
            # in the exterior.
            commit << interior
          end
        end

        next if pruned.empty? && !captures.empty?
        next if pruned.size < op.min || pruned.size > op.max > 0

        # We were under control of op.capture, noone should've overridden it
        # due to unification (even if they did, they've overridden it to the same
        # value so that wouldn't matter)
        exterior.with(op.capture, pruned)
      end

      Env.feedback(consistent, fallback: behind1.env)
    in Fb::Mismatch, Fb::Request
      fb
    end
  end

  def match(env, op : Value, matchee : Term, ahead)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(env.env)
    end

    if dict.empty?
      return Fb::Mismatch.new(env.env)
    end

    candidates = Set(Term).new

    if key = env[op.capture]?
      candidates << key
    else
      # If we do not know the value yet it might be the case that it can be learned
      # from the future. We declare its domain to be that of all keys from the matchee
      # dict, and run ahead without the value known, hoping to learn it.
      case fb = ahead.call(env.one_of(op.capture, dict))
      in Fb::MatchOne, Fb::Mismatch
        if key = fb.env[op.capture]?
          candidates << key
        end
      in Fb::MatchMany
        fb.envs.each do |fenv|
          next unless key = fenv[op.capture]?

          candidates << key
        end
      in Fb::Request
        return fb
      end
    end

    return Fb::Mismatch.new(env.env) if candidates.empty?

    envs = [] of Env::Type

    candidates.each do |key|
      next unless value = dict[key]?

      kp0 = env.keypath

      env1 = env
        .mount(op.capture, &.key(key))
        .assign(op.capture, key)
        .keypath(&.value(key))

      fb = match(env1, op.tail, value, Ahead::Goto.new(kp0, Ahead.stackptr(ahead)))
      unless fb.is_a?(Fb::Response)
        return fb
      end

      Env.append(envs, feedback: fb)
    end

    Env.feedback(envs, fallback: env.env)
  end

  def match(behind0, op : NegativeValue | NegativeValueKeypath, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    # If we know the key already, that's our fast path.
    if key = behind0[op.capture]?
      if key.in?(dict)
        return Fb::Mismatch.new(behind0.env)
      end

      case op
      in NegativeValue
        behind1 = behind0
      in NegativeValueKeypath
        behind1 = behind0.mount(op.name, &.ephv(key))
      end

      return ahead0.call(behind1)
    end

    # Restrict the future's choices of candidates for the captures. Make
    # sure they're NOT from one of dict's keys.
    behind1 = behind0.not(op.capture, dict)

    # Now ask the future for candidates.
    case fb = ahead0.call(behind1)
    in Fb::MatchOne, Fb::Mismatch
      unless key = fb.env[op.capture]?
        return Fb::Mismatch.new(behind0.env)
      end
      candidates = Set{key}
    in Fb::MatchMany
      candidates = fb.envs.to_compact_set { |env| env[op.capture]? }
    in Fb::Request
      return fb
    end

    # Now that we have a nonempty set of candidates for keys, we can pick
    # each one and pass them to the future again.
    envs = [] of Env::Type

    candidates.each do |key|
      case op
      in NegativeValue
        behind2 = behind1
      in NegativeValueKeypath
        behind2 = behind1.mount(op.name, &.ephv(key))
      end

      fb = ahead0.call(behind2)
      unless fb.is_a?(Fb::Response)
        return fb
      end

      Env.append(envs, feedback: fb)
    end

    Env.feedback(envs, fallback: behind0.env)
  end

  # TODO: cache New like we cache Term.cases
  def match(behind0, op : New, matchee : Term, ahead0)
    # If we already know all the subjects, this is the best case and
    # an immediate fast path toward instantiation.
    if op.subjects.all?(&.in?(behind0.env))
      subt = behind0.env.pluck(op.subjects)
      successor = pipe(op.pattern, M1.bsubst(subt), M1.operator)

      return match(behind0, successor, matchee, ahead0)
    end

    # If we do not, the process of learning begins. We learn partially,
    # recursively, by first trying to invoke ahead, filling in the possible
    # values for subjects, and recursively calling match(New) again with
    # less subjects. If we did not learn anything (no reduction in the number
    # of subjects) we give up with a Mismatch.
    case fb = ahead0.call(behind0)
    in Fb::Match
      learned = fb.envs
    in Fb::Mismatch
      learned = {fb.env}
    in Fb::Request
      return fb
    end

    envs = [] of Env::Type

    learned.each do |env|
      behind1 = behind0
      subjects1 = [] of Term

      subt = Term::Dict.build do |subt|
        op.subjects.each do |subject|
          unless guess = env[subject]?
            subjects1 << subject
            next
          end

          next unless behind2 = behind1.propose?(subject, guess)

          behind1 = behind2

          subt.with(subject, guess)
        end
      end

      next if op.subjects.size == subjects1.size # Did not learn anything

      instance = M1.bsubst(op.pattern, subt)

      fb = match(behind1, New.new(subjects1, instance), matchee, ahead0)
      unless fb.is_a?(Fb::Response)
        return fb
      end

      Env.append(envs, feedback: fb)
    end

    Env.feedback(envs, fallback: behind0.env)
  end

  def match(behind0, op : Keypath, matchee : Term, ahead0)
    unless keypath = behind0.keypath
      # We're not running in keypath mode. Send a back-message all the way up
      # the call stack to where the match was initiated; ask them to rematch
      # with keypath mode enabled.
      #
      # I don't think there is any optimization we can do here regarding
      # the preservation of progress. During progress keypaths are modified,
      # so we'll have to retry anyway most of the cases.
      return Fb::RequestKeypath.new
    end

    kpdict = Term.of(keypath.dict)

    unless behind1 = behind0.propose?(op.capture, kpdict)
      return Fb::Mismatch.new(behind0.env.with(op.capture, kpdict))
    end

    ahead0.call(behind1)
  end

  def feedback(env : Env::Type, op : Any, matchee : Term, *, keypaths : Bool = false) : Fb::Response
    behind0 = Behind.new(env, domains: Term[], antidomains: Term[], keypath: keypaths ? KeypathTip.new : nil)

    case fb = match(behind0, op, matchee, Ahead::MatchOne.new)
    in Fb::Response
      fb
    in Fb::RequestKeypath
      if keypaths
        return Fb::Mismatch.new(env)
      end

      feedback(env, op, matchee, keypaths: true)
    end
  end

  def match?(env : Env::Type, op : Any, matchee : Term, **kwargs) : Env::Type?
    case fb = feedback(env, op, matchee, **kwargs)
    in Fb::MatchOne  then fb.env
    in Fb::MatchMany then fb.envs[0]
    in Fb::Mismatch  then nil
    end
  end

  def matches(env : Env::Type, op : Any, matchee : Term, **kwargs) : Array(Env::Type)
    case fb = feedback(env, op, matchee, **kwargs)
    in Fb::MatchOne  then [fb.env]
    in Fb::MatchMany then fb.envs
    in Fb::Mismatch  then [] of Env::Type
    end
  end

  # TODO: this should use some kind of flag to signal to match()s that they should relax?
  # i.e. sources may emit only once etc.
  def probe?(env : Env::Type, op : Any, matchee : Term) : Bool
    feedback(env, op, matchee).is_a?(Fb::Match)
  end
end

module ::Ww::Term::M1::Operator
  extend self

  def match(behind0, op : Any, cell : Search::Result::Item, ahead)
    kp0 = behind0.keypath
    kp1 = cell.keypath
    cont = Ahead::Goto.new(kp0, Ahead.stackptr(ahead))

    match(behind0.goto(kp1), op, cell.term, cont)
  end

  def match(behind0, op : Any, cell : Search::Result::Pair, ahead)
    Fb::Mismatch.new(behind0.env)
  end

  def match(behind0, op : Any, cell : Search::Result::ItemStrip, ahead)
    Fb::Mismatch.new(behind0.env)
  end

  def match(behind0, ops : Indexable(Any), cell : Search::Result::Item, ahead)
    match(behind0, ops, cell.sequence, ahead)
  end

  def match(behind0, ops : Indexable(Any), cell : Search::Result::Pair, ahead)
    unless ops.size == 2
      return Fb::Mismatch.new(behind0.env)
    end

    kp0 = behind0.keypath

    if row = cell.keypath
      kkp, vkp = row
    else
      kkp = vkp = nil
    end

    match(behind0.goto(kkp), ops[0], cell.k) do |in1|
      match(in1.goto(vkp), ops[1], cell.v) do |in2|
        ahead.call(in2.goto(kp0))
      end
    end
  end

  # FIXME: instead of relying on Slice() implement match() that uses two indices I and J
  # over an indexable. This will allow us to not allocate arrays in several places and
  # also reuse this match() in several places, plus no .@b crap
  def match0(env, ops, matchees, ahead)
    op, matchee = ops.first?, matchees.first?

    if op.nil? && matchee.nil?
      return ahead.call(env)
    end

    unless op && matchee
      return Fb::Mismatch.new(env.env)
    end

    match(env.goto(matchees.keypath), op, matchee, Ahead::Fn.new { |candidate| match0(candidate, ops + 1, matchees + 1, ahead) })
  end

  def match(env, ops : Slice(Any), matchees : Search::Result::ItemStrip, ahead)
    kp0 = env.keypath

    match0(env, ops, matchees, Ahead::Goto.new(kp0, Ahead.stackptr(ahead)))
  end

  def match(env, ops : Array(Any), matchee, ahead)
    match(env, ops.to_readonly_slice, matchee, ahead)
  end

  def match(env, op, matchee, &ahead : Behind -> Fb::Any)
    match(env, op, matchee, Ahead::Fn.new(ahead))
  end
end

module ::Ww::Term::M1::Operator::Item
  alias Feed = Term::Dict::ItemsView

  record SuccessorsView, amount : Int32, neighbor : Item::Any?

  def self.neighbor?(item : Singular | Gap | Plural | Optional, rest) : Any?
    item
  end

  def self.neighbor?(item : Group | Many | Past, rest) : Any?
    neighbor?(item.children.to_readonly_slice)
  end

  def self.neighbor?(item : Slot, rest) : Any?
    neighbor?(rest)
  end

  def self.neighbor?(items : Slice(Any)) : Any?
    items.empty? ? nil : neighbor?(items[0], items[1..])
  end

  module ItemAhead
    macro stackptr(var)
      begin
        %slot = {{var}}.as(ItemAhead)
        pointerof(%slot)
      end
    end
  end

  struct ItemAhead::Fn
    include ItemAhead

    def initialize(@fn : UInt32, Feed, Behind -> Fb::Any)
    end

    def initialize(&@fn : UInt32, Feed, Behind -> Fb::Any)
    end

    def call(ord, feed, behind0)
      @fn.call(ord, feed, behind0)
    end
  end

  def self.match(ord, env, item : Singular, successors, feed, ahead)
    unless matchee = feed.first?
      return Fb::Mismatch.new(env.env)
    end

    Operator.match(env, item.tail, matchee, Operator::Ahead::ItemStep.new(ord, feed, ItemAhead.stackptr(ahead)))
  end

  def self.match(ord, env, item : Slot, successors, feed, ahead)
    # span: 0 is understood by the backmap engine as "pure insert", with no
    # "replace component".
    ahead.call(ord + 1, feed, env.mount(item.capture, &.span(0).ord(ord)))
  end

  enum ExpandStrategy : UInt8
    Auto
    Sway
    Lazy
    Greedy
  end

  def self.expand(feed : Feed, pivot : Int, strategy : ExpandStrategy = :sway, & : Feed, Feed ->)
    case strategy
    in .auto?
      raise ArgumentError.new
    in .sway?
      prefix, suffix = feed.begin.grow(pivot), feed.move(pivot)
      yield prefix, suffix

      # We then sway like pivot - 1, pivot + 1, pivot - 2, pivot + 2, etc...
      (1..feed.size).each do |offset|
        if pivot - offset >= 0
          prefix, suffix = feed.begin.grow(pivot - offset), feed.move(pivot - offset)
          yield prefix, suffix
        end

        if pivot + offset <= feed.size
          prefix, suffix = feed.begin.grow(pivot + offset), feed.move(pivot + offset)
          yield prefix, suffix
        end
      end
    in .lazy?
      (0..feed.size).each do |size|
        prefix, suffix = feed.begin.grow(size), feed.move(size)
        yield prefix, suffix
      end
    in .greedy?
      (0..feed.size).reverse_each do |size|
        prefix, suffix = feed.begin.grow(size), feed.move(size)
        yield prefix, suffix
      end
    end
  end

  # sway(Term[1, 2, 3, 4].items, 2) do |prefix, suffix|
  #   puts prefix
  #   puts suffix
  # end

  # Untyped `Plural`s should give way to typed successors: they should be lazy
  # relative to typed successors. This lets highly ambiguous patterns like
  # `xs_* ns_number* ys_*` match more or less intuitively on e.g.:
  #    1 2 "hello" 3 4 5 foo "world" 5 6 7
  # as: { xs: (1 2 "hello"), ns: (3 4 5), ys: (foo "hello" 5 6 7) }
  # Look ahead. How many items do we have left in the feed? Let's say it's 10.
  # How many items (itemspart patterns) do we have ahead? Let's say it's 3.
  # Be good: take 1/3 of the feed rather than consuming eagerly or lazily.
  # Doesn't work out? Take 1/3 - 1, - 2, etc. (contract), until it either works
  # out, or we cannot reduce ourselves further. In that case, start at 1/3 once
  # again, but this time, expand instead of contracting: 1/3 + 1, 1/3 + 2, etc...
  # If that doesn't work out either (we're out of items on the feed), then that's
  # a mismatch: no solution exists.

  # TODO: refactor match() to take ItemBehind which is an extension of Behind with ord, feed

  private def self.propose?(ord, env, item : Plural, prefix, suffix, ahead)
    if capture = item.capture
      candidate = env.propose?(capture, Term.of(prefix))
      return unless candidate

      candidate = candidate.mount(capture, &.span(prefix.size).ord(ord))
    else
      candidate = env
    end

    ahead.call(ord + prefix.size, suffix, candidate.keypath(&.forward(prefix.size))).as?(Fb::Match)
  end

  def self.match(ord, env, item : Plural, successors, feed, ahead)
    pivot = (feed.size / (successors.amount + 1)).ceil.to_i

    follower = successors.neighbor.as?(Plural)

    if item.strategy.auto?
      strategy = ExpandStrategy::Sway

      if follower && !item.type.any? && follower.type.any?
        strategy = ExpandStrategy::Greedy
      elsif follower && item.type.any? && !follower.type.any?
        # *Give* from the end of fraction to the follower if the follower is typed
        # and the end is a subtype of the follower's type.
        unless pivot < item.min || pivot > item.max > 0
          size0 = pivot

          while size0 > 0
            size1 = size0 - 1
            # Make sure our future size is OK with our own constraints & the follower
            # can accept it.
            break unless feed[size1].type.subtype?(follower.type)
            break if size1 < item.min || size1 > item.max > 0
            size0 = size1
          end

          # These are our candidate prefix and suffix. Try to propose them to ahead.
          # If this fails, we restart at the pivot with the usual algorithm. Which
          # will also go backwards (if sensible) but this time,  asking ahead on every
          # step rather than after subtype ends.
          prefix, suffix = feed.begin.grow(size0), feed.move(size0)
          if fb = propose?(ord, env, item, prefix, suffix, ahead)
            return fb
          end
        end
      end
    else
      strategy = item.strategy
    end

    expand(feed, pivot: pivot, strategy: strategy) do |prefix, suffix|
      next if prefix.size < item.min || prefix.size > item.max > 0
      next unless item.type.any? || prefix.all?(&.type.subtype?(item.type))
      next unless fb = propose?(ord, env, item, prefix, suffix, ahead)
      return fb
    end

    Fb::Mismatch.new(env.env)
  end

  struct ItemAhead::SequenceRest
    include ItemAhead

    def initialize(@ord : UInt32, @items : Slice(Any), @successors : SuccessorsView, @ahead : ItemAhead*)
    end

    def call(ord, outfeed, candidate)
      Item.sequence(@ord + 1, candidate, @items[1..], @successors, outfeed, @ahead.value)
    end
  end

  def self.sequence(ord, env, items : Slice(Any), successors, feed, ahead)
    unless item = items.first?
      return ahead.call(ord, feed, env)
    end

    # Continuation for the item matching methods.
    cont = ItemAhead::SequenceRest.new(ord, items, successors, ItemAhead.stackptr(ahead))

    match(ord, env, item, SuccessorsView.new(successors.amount + items.size - 1, neighbor?(items[1..])), feed, cont)
  end

  def self.match(ord, env, item : Group, successors, feed, ahead)
    start = env.keypath

    cont = ItemAhead::Fn.new do |ord, outfeed, candidate|
      group = feed.upto(outfeed)
      captures = Term.of(group)

      unless env1 = candidate.propose?(item.capture, captures)
        next Fb::Mismatch.new(env.env.with(item.capture, captures))
      end

      ahead.call(ord, outfeed, env1.mount(item.capture) { start.not_nil!.span(group.size).ord(ord) })
    end

    sequence(ord, env, item.children.to_readonly_slice, successors, feed, cont)
  end

  def self.match(ord, env, item : Gap, successors, feed, ahead)
    strategy = item.strategy.auto? ? ExpandStrategy::Sway : item.strategy

    envs = [] of Env::Type

    expand(feed, pivot: (feed.size / (successors.amount + 1)).ceil.to_i, strategy: strategy) do |prefix, suffix|
      matchee = Term.of(prefix.size)

      kp0 = env.keypath
      case fb = Operator.match(env.keypathless, item.measurer, matchee, Operator::Ahead::Fn.new { |in1| ahead.call(ord + prefix.size, suffix, in1.goto(kp0).keypath(&.forward(prefix.size))) })
      in Fb::MatchOne
        envs << fb.env
        next if fb.more
        break
      in Fb::MatchMany
        envs.concat(fb.envs)
        break
      in Fb::Mismatch
      in Fb::Request
        return fb
      end

      ord += 1
    end

    Env.feedback(envs, fallback: env.env)
  end

  def self.match(ord, env, item : Optional, successors, feed, ahead)
    kp0 = env.keypath

    if matchee = feed.first?
      fb = Operator.match(env, item.tail, matchee, Operator::Ahead::Fn.new { |in1| ahead.call(ord + 1, feed.move(1), in1.keypath(&.forward)) })
      if fb.is_a?(Fb::Match)
        return fb
      end
    end

    Operator.match(env.keypath(&.ephk(item.default, ord)), item.tail, item.default, Operator::Ahead::Fn.new { |in1| ahead.call(ord, feed, in1.goto(kp0)) })
  end

  def self.many(ord, env, item : Many, successors, feed, ahead, memo)
    unless env1 = env.propose?(item.capture, Term.of(memo))
      return Fb::Mismatch.new(env.env.with(item.capture, Term.of(memo)))
    end

    if memo.size > item.max > 0
      return Fb::Mismatch.new(env.env)
    end

    if memo.size >= item.min
      case fb = ahead.call(ord, feed, env1)
      in Fb::Match, Fb::Request
        return fb
      in Fb::Mismatch
      end
    end

    cont = ItemAhead::Fn.new do |ord, ofeed, candidate|
      if feed == ofeed
        # This continuation is run after the ahead check. This means ahead refuses to
        # consume feed. And we did not move while trying to consume feed. Thus this is
        # a hard mismatch.
        next Fb::Mismatch.new(candidate.env)
      end

      pruned, capture = candidate.partition(item.interior)

      many(ord, pruned, item, successors, ofeed, ahead, memo.append(capture.env))
    end

    sequence(ord, env, item.children.to_readonly_slice, successors, feed, cont)
  end

  def self.match(ord, env, item : Many, successors, feed, ahead)
    kp0 = env.keypath
    cont = ItemAhead::Fn.new do |ord, ofeed, in1|
      size = feed.upto(ofeed).size

      kp1 = in1.keypath
      in2 = in1
        .goto(kp0)
        .mount(item.capture, &.span(size))
        .goto(kp1)

      ahead.call(ord, ofeed, in2)
    end

    many(ord, env, item, successors, feed, cont, memo: Term[])
  end

  def self.past_lazy(ord, env, item : Past, successors, feed, ahead, memo)
    if memo > item.max > 0
      return Fb::Mismatch.new(env.env)
    end

    if memo >= item.min
      case fb = ahead.call(ord, feed, env)
      in Fb::Match, Fb::Request
        return fb
      in Fb::Mismatch
      end
    end

    cont = ItemAhead::Fn.new do |ord, ofeed, candidate|
      if feed == ofeed
        # This continuation is run after the ahead check. This means ahead refuses to
        # consume feed. And we did not move while trying to consume feed. Thus this is
        # a hard mismatch.
        next Fb::Mismatch.new(candidate.env)
      end

      past_lazy(ord, candidate, item, successors, ofeed, ahead, memo + 1)
    end

    sequence(ord, env, item.children.to_readonly_slice, successors, feed, cont)
  end

  def self.past_greedy(ord, env, item : Past, successors, feed, ahead, memo)
    if memo > item.max > 0
      return Fb::Mismatch.new(env.env)
    end

    cont = ItemAhead::Fn.new do |ord, ofeed, candidate|
      if feed == ofeed
        # Let the sequence() mismatch check do its job
        next Fb::Mismatch.new(candidate.env)
      end

      past_greedy(ord, candidate, item, successors, ofeed, ahead, memo + 1)
    end

    case fb = sequence(ord, env, item.children.to_readonly_slice, successors, feed, cont)
    in Fb::Match, Fb::Request
      fb
    in Fb::Mismatch
      if memo >= item.min
        ahead.call(ord, feed, env)
      else
        fb
      end
    end
  end

  def self.match(ord, env, item : Past, successors, feed, ahead)
    if item.greedy
      past_greedy(ord, env, item, successors, feed, ahead, memo: 0)
    else
      past_lazy(ord, env, item, successors, feed, ahead, memo: 0)
    end
  end

  struct ItemAhead::Rest
    include ItemAhead

    def initialize(@items : Slice(Any), @ahead : Operator::Ahead*)
    end

    def call(ord, outfeed, candidate)
      Item.match(ord, candidate, @items[1..], outfeed, @ahead.value)
    end
  end

  def self.match(ord, env, items : Slice(Any), feed, ahead)
    item = items.first?

    if item.nil? && feed.empty? # Matched all items.
      return ahead.call(env)
    end

    if item.nil? # Ran out of items.
      return Fb::Mismatch.new(env.env)
    end

    # Continuation for the item matching methods.
    cont = ItemAhead::Rest.new(items, Operator::Ahead.stackptr(ahead))

    match(ord, env, item, SuccessorsView.new(items.size - 1, neighbor?(items[1..])), feed, cont)
  end

  def self.match(env, items : Slice(Any), feed, ahead)
    kp0 = env.keypath

    match(0u32, env.keypath(&.value(0)), items, feed, Operator::Ahead::Goto.new(kp0, Operator::Ahead.stackptr(ahead)))
  end
end

module ::Ww::Term::M1
  SYM_LT  = Term.of(:<)
  SYM_GT  = Term.of(:>)
  SYM_LTE = Term.of(:<=)
  SYM_GTE = Term.of(:>=)
  SYM_INF = Term.of(:∞)

  # Contains methods that work together to implement `M1.normal`.
  module Normal
    extend self

    # Schemas used to validate options passed to various pattern matching constructs.
    module Schemas
    end

    Schemas::LeafUnbounded = Term::M0::PairSchema.build do
      key :in, values: {:items, :keys, :values, :"pair/values"}, default: :items
      key :order, values: {:dfs, :bfs}, default: :dfs
      key :self, values: {true, false}, default: false
    end
  
    Schemas::LeafBounded = Term::M0::PairSchema.build do
      key :in, values: {:items, :keys, :values, :"pair/values"}, default: :items
      key :order, values: {:dfs, :bfs}, default: :dfs
      key :min, values: 0..UInt8::MAX, default: 0
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      key :self, values: {true, false}, default: false
      where { |min, max| min.as_n <= max.as_n }
    end
  
    Schemas::Items = Term::M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 1
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end
  
    Schemas::Entries = Term::M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 1
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end
  
    Schemas::Plural = Term::M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 0
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      key :type, values: {:_number, :_string, :_symbol, :_dict, :_}, default: :_
      where { |min, max| min.as_n <= max.as_n }
    end
  
    Schemas::Many = Term::M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 1
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end
  
    Schemas::Past = Term::M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 0
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end

    SYM_BLANK_ANY = Term[:_]
    SYM_BLANK_DICT = Term[:_dict]
    SYM_BLANK_NUMBER = Term[:_number]
    SYM_BLANK_SYMBOL = Term[:_symbol]
    SYM_BLANK_STRING = Term[:_string]
    SYM_BLANK_BOOLEAN = Term[:_boolean]

    SYMS_CMP = {SYM_LT, SYM_GT, SYM_LTE, SYM_GTE}
    SYMS_LTX = {SYM_LT, SYM_LTE}

    NORMAL_PASS = Term.of({:"%pass"})

    NORMAL_BLANK_DICT    = Term.of({:"%dict"})
    NORMAL_BLANK_NUMBER  = Term.of({:"%number", :_})
    NORMAL_BLANK_STRING  = Term.of({:"%string"})
    NORMAL_BLANK_SYMBOL  = Term.of({:"%symbol"})
    NORMAL_BLANK_BOOLEAN = Term.of({:"%boolean"})

    # :nodoc:
    NORMAL_INT = Term.of(
      u8: {:"%number", UInt8::MIN, :<=, {:whole, :_}, :<=, UInt8::MAX},
      u16: {:"%number", UInt16::MIN, :<=, {:whole, :_}, :<=, UInt16::MAX},
      u32: {:"%number", UInt32::MIN, :<=, {:whole, :_}, :<=, UInt32::MAX},
      u64: {:"%number", UInt64::MIN, :<=, {:whole, :_}, :<=, UInt64::MAX},
      u128: {:"%number", UInt128::MIN, :<=, {:whole, :_}, :<=, UInt128::MAX},
      i8: {:"%number", Int8::MIN, :<=, {:whole, :_}, :<=, Int8::MAX},
      "-i8": {:"%number", Int8::MIN, :<=, {:whole, :_}, :<, 0},
      "+i8": {:"%number", 0, :<=, {:whole, :_}, :<=, Int8::MAX},
      i16: {:"%number", Int16::MIN, :<=, {:whole, :_}, :<=, Int16::MAX},
      "-i16": {:"%number", Int16::MIN, :<=, {:whole, :_}, :<, 0},
      "+i16": {:"%number", 0, :<=, {:whole, :_}, :<=, Int16::MAX},
      i32: {:"%number", Int32::MIN, :<=, {:whole, :_}, :<=, Int32::MAX},
      "-i32": {:"%number", Int32::MIN, :<=, {:whole, :_}, :<, 0},
      "+i32": {:"%number", 0, :<=, {:whole, :_}, :<=, Int32::MAX},
      i64: {:"%number", Int64::MIN, :<=, {:whole, :_}, :<=, Int64::MAX},
      "-i64": {:"%number", Int64::MIN, :<=, {:whole, :_}, :<, 0},
      "+i64": {:"%number", 0, :<=, {:whole, :_}, :<=, Int64::MAX},
      i128: {:"%number", Int128::MIN, :<=, {:whole, :_}, :<=, Int128::MAX},
      "-i128": {:"%number", Int128::MIN, :<=, {:whole, :_}, :<, 0},
      "+i128": {:"%number", 0, :<=, {:whole, :_}, :<=, Int128::MAX},
    )

    private def typesym(blank : Term::Sym::Blank) : Term::Sym
      case blank.type
      in .any?     then SYM_BLANK_ANY
      in .number?  then SYM_BLANK_NUMBER
      in .symbol?  then SYM_BLANK_SYMBOL
      in .string?  then SYM_BLANK_STRING
      in .boolean? then SYM_BLANK_BOOLEAN
      in .dict?    then SYM_BLANK_DICT
      end
    end

    # Returns the normal form of an itemspart *node*.
    def item(node : Term) : Term
      Term.of_case(node, engine: Term::M0) do
        matchpi %[_symbol] do
          continue unless blank = node.blank?
          continue unless blank.poly?

          name = blank.name?

          Term.of(:"%plural", name ? {:"%capture", name} : nil, type: typesym(blank), min: blank.one? ? 1 : 0, max: SYM_INF)
        end

        # Fast path to %singular for literal terms.
        matchpi %[_number], %[_string], %[_boolean] do
          {:"%singular", pattern(node)}
        end

        matchpi(
          %[(%plural ¦ opts_)],
          %[(%plural/min ¦ opts_)],
          %[(%plural/max ¦ opts_)],
          cues: {:"%plural", :"%plural/min", :"%plural/max"},
        ) do |opts|
          continue unless opts = Schemas::Plural.validated?(opts)

          opts.morph({0, node[0]})
        end

        matchpi(
          %[(%plural capture_ ¦ opts_)],
          %[(%plural/min capture_ ¦ opts_)],
          %[(%plural/max capture_ ¦ opts_)],
          cues: {:"%plural", :"%plural/min", :"%plural/max"},
        ) do |opts|
          continue unless opts = Schemas::Plural.validated?(opts)

          opts.morph({0, node[0]}, {1, {:"%capture", capture}})
        end

        matchpi %[(%optional _ body_)], cue: :"%optional" do
          node.morph({2, pattern(body)})
        end

        matchpi %[(%group capture_ _ _*)], cue: :"%group" do
          Term::Dict.build do |commit|
            commit << :"%group" << {:"%capture", capture}
            commit.concat(node.items.move(2)) { |member| item(member) }
          end
        end

        matchpi %[(%many capture_ _ _* ¦ opts_)], cue: :"%many" do |opts|
          continue unless opts = Schemas::Many.validated?(opts)

          opts.transaction do |commit|
            commit << :"%many" << {:"%capture", capture}
            commit.concat(node.items.move(2)) { |member| item(member) }
          end
        end

        matchpi %[(%past _ _* ¦ opts_)], cue: :"%past" do |opts|
          continue unless opts = Schemas::Past.validated?(opts)

          opts.transaction do |commit|
            commit << :"%past"
            commit.concat(node.items.move(1)) { |member| item(member) }
            commit.with(:greedy, false)
          end
        end

        matchpi %[(%past/max _ _* ¦ opts_)], cue: :"%past/max" do |opts|
          continue unless opts = Schemas::Past.validated?(opts)

          opts.transaction do |commit|
            commit << :"%past"
            commit.concat(node.items.move(1)) { |member| item(member) }
            commit.with(:greedy, true)
          end
        end

        matchpi(
          %[(%gap measurer_)],
          %[(%gap/min measurer_)],
          %[(%gap/max measurer_)],
          cues: {:"%gap", :"%gap/min", :"%gap/max"}
        ) do
          node.morph({1, pattern(measurer)})
        end

        # NOTE: Currently we do not register %slot as a capture. And I don't think
        # there is any point in doing so.
        matchpi %[(%slot _)], cue: :"%slot" do
          node
        end

        otherwise { {:"%singular", pattern(node)} }
      end
    end

    # Returns the normal form of a pairspart *key*-*value* pair.
    def pair(key : Term, value : Term) : Term
      Term.of_case(value, engine: Term::M0) do
        matchpi %[(%optional default_ body_)], cue: :"%optional" do
          {:"%pair/optional", key, default, pattern(body)}
        end

        matchpi %[(%- positive_)], cue: :"%-" do
          {:"%pair/negative", key, pattern(positive)}
        end

        matchpi %[(%- positive_ name_)], cue: :"%-" do
          {:"%pair/negative", key, pattern(positive), name}
        end

        otherwise do
          {:"%pair/required", key, pattern(value)}
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
    def dict(dict : Term::Dict) : Term
      if literal?(dict)
        return Term.of(:"%literal", dict)
      end

      if dict.itemsonly?
        node = Term::Dict.build do |commit|
          commit << :"%itemspart"
          commit.concat(dict.items) { |itemnode| item(itemnode) }
        end

        return Term.of(node)
      end

      # E.g. {x: 100, y: 200} = (%layer () x: 100 y: 200)
      if dict.pairsonly?
        return pattern(Term.of(:"%layer", Term[], dict))
      end

      Term.of(:"%partition", dict(dict.items.collect), dict(dict.pairs))
    end

    # Returns the normal form of *pattern*.
    def pattern(pattern : Term) : Term
      Term.of_case(pattern, engine: Term::M0) do
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

          dict(pdict)
        end

        # Similarly, %let is very frequent (especially due to blanks such as x_)
        # compiling to e.g. (%let x _).
        matchpi %[((%literal %let) capture_ successor_)], cue: :"%let" do
          {:"%let", {:"%capture", capture}, pattern(successor)}
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
          when SYM_BLANK_ANY     then NORMAL_PASS 
          when SYM_BLANK_NUMBER  then NORMAL_BLANK_NUMBER 
          when SYM_BLANK_STRING  then NORMAL_BLANK_STRING 
          when SYM_BLANK_SYMBOL  then NORMAL_BLANK_SYMBOL 
          when SYM_BLANK_BOOLEAN then NORMAL_BLANK_BOOLEAN 
          when SYM_BLANK_DICT    then NORMAL_BLANK_DICT 
          else
            continue unless blank = pattern.unsafe_as_sym.blank?
            continue unless blank.single?
            continue unless name = blank.name?

            pattern(Term.of(:"%let", name, typesym(blank)))
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
        # e.g. @foo is (edge ...). We reuse @foo_ to match roughly ((%literal edge) _).
        matchpi %[(edge arg_symbol)], cue: :edge do |arg|
          arg = arg.unsafe_as_sym
          continue unless blank = arg.blank?
          continue unless blank.single?

          case blank.type
          when .symbol? then edge = Term.of(:"%edge", :_symbol)
          when .string? then edge = Term.of(:"%edge", :_string)
          when .number? then edge = Term.of(:"%edge", :_number)
          when .any?    then edge = Term.of(:"%edge", :_)
          else
            continue
          end

          if name = blank.name?
            edge = Term.of(:"%let", {:"%capture", name}, edge)
          end

          edge
        end

        # Partition is pretty frequent.
        matchpi %[((%literal %partition) itemspart_ pairspart_)], cue: :"%partition" do
          {:"%partition", pattern(itemspart), pattern(pairspart)}
        end

        matchpi %[(%layer below_ side_dict)], cue: :"%layer" do
          pattern.transaction do |commit|
            commit.with(1, pattern(below))

            nside = side.transaction do |nside|
              side.each_entry do |k, v|
                nside.with(k, pair(k, v))
              end
            end

            commit.with(2, nside)
          end
        end

        # (%layer _ k1: v1 k2: v2 ...) is a shorthand for (%layer _ {k1: v1 k2: v2 ...}).
        matchpi %[(%layer below_ ¦ pairs_)], cue: :"%layer" do
          pattern(Term.of(:"%layer", below, pairs))
        end

        # %number should be %terminal.
        matchpi(
          %[(%number (%literal _))],
          %[(%number (%literal (whole _)))],
          cue: {:"%number", :_},
          cues: {nil, :whole}
        ) { {:"%terminal", pattern} }

        # Compile fixed-width %number into the corresponding bounds check. We do not
        # actually have fixed-width numbers. These kinds of patterns are often used
        # on the Crystal side to ensure we can safely e.g. to(Int32).
        matchpi %[(%number type_symbol)], cue: :"%number" do
          continue unless normal = NORMAL_INT[type]?

          {:"%terminal", normal}
        end

        matchpi(
          %[(%number (%literal _) op_symbol _number)],
          %[(%number (%literal (whole _)) op_symbol _number)],
          cue: {:"%number", :_},
          cues: {nil, :whole}
        ) do |op|
          continue unless op.in?(SYMS_CMP)

          {:"%terminal", pattern}
        end

        matchpi(
          %[(%number _number lop_symbol (%literal _) rop_symbol _number)],
          %[(%number _number lop_symbol (%literal (whole _)) rop_symbol _number)],
          cue: {:"%number", :_},
          cues: {nil, :whole}
        ) do |lop, rop|
          continue unless lop.in?(SYMS_LTX)
          continue unless rop.in?(SYMS_LTX)

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
          %[(%pipe (map _dict) successor_)],
          %[(%pipe span successor_)],
          %[(%pipe tally successor_)],
          cue: :"%pipe",
          cues: {:+, :-, :*, :/, :div, :mod, :**, :map, :span, :tally}
        ) do
          pattern.morph(
            {1, ->(term : Term) { Term.of(:"%barrier", term) }},
            {2, pattern(successor)},
          )
        end

        matchpi %[(%pipe head_ _*)], cue: :"%pipe" do
          continue if pattern.size < 4 # %pipe + head_1 + head_2 + body

          body = Term::Dict.build do |commit|
            commit << :"%pipe"

            rest = pattern.items.move(2)
            rest.each { |item| commit << item }
          end

          pattern(Term.of(:"%pipe", head, body))
        end

        matchpi %[(%all _*)], cue: :"%all" do
          pattern.transaction do |commit|
            offshoots = pattern.items.move(1)
            offshoots.each_with_index(offset: 1) do |offshoot, index|
              commit.with(index, pattern(offshoot))
            end
          end
        end

        matchpi %[(%any _*)], cue: :"%any" do
          {:"%terminal", pattern.morph({0, :"%any/literal"})}
        end

        matchpi %[(%any° _*)], cue: :"%any°" do
          Term::Dict.build do |commit|
            commit << :"%any/source"

            branches = pattern.items.move(1)
            branches.each { |branch| commit << pattern(branch) }
          end
        end

        # Leave %literal as is.
        matchpi %[((%literal %literal) _)], cue: :"%literal" do
          pattern
        end

        # Mark %keypool and %not as %terminal so that walk doesn't walk inside them.
        matchpi(
          %[(%keypool _ _*)],
          %[(%not _ _*)],
          cues: {:"%keypool", :"%not"}
        ) { {:"%terminal", pattern} }

        matchpi %[(%keypath capture_)], cue: :"%keypath" do
          {:"%keypath", {:"%capture", capture}}
        end

        matchpi(
          %[(%edge (%literal _symbol))],
          %[(%edge (%literal _string))],
          %[(%edge (%literal _number))],
          %[(%edge (%literal _))],
          cue: :"%edge"
        ) { pattern }

        # %nonself is dissolved at normalization.
        matchpi %[(%nonself arg_)], cue: :"%nonself" do
          pattern(arg)
        end

        matchpi %[(%value capture_ body_)], cue: :"%value" do
          {:"%value", {:"%capture", capture}, pattern(body)}
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
            commit.concat(pattern.items.move(1)) { |item| pattern(item) }
          end
        end

        matchpi %[(%item° _ _*)], cue: :"%item°" do
          Term::Dict.build do |commit|
            commit << :"%items/source"
            commit.concat(pattern.items.move(1)) { |item| pattern(item) }
          end
        end

        matchpi %[(%items capture_ _ _* ¦ opts_)], cue: :"%items" do |opts|
          continue unless opts = Schemas::Items.validated?(opts)

          opts.transaction do |commit|
            commit << :"%items/all" << {:"%capture", capture}
            commit.concat(pattern.items.move(2)) { |item| pattern(item) }
          end
        end

        matchpi %[(%entry k_ v_)], cue: :"%entry" do
          {:"%entries/first", pattern(k), pattern(v)}
        end

        matchpi %[(%entry° k_ v_)], cue: :"%entry°" do
          {:"%entries/source", pattern(k), pattern(v)}
        end

        matchpi %[(%entries capture_ k_ v_ ¦ opts_)], cue: :"%entries" do |opts|
          continue unless opts = Schemas::Entries.validated?(opts)

          opts.morph(
            {0, :"%entries/all"},
            {1, {:"%capture", capture}},
            {2, pattern(k)},
            {3, pattern(v)},
          )
        end

        matchpi %[(%leaf body_ ¦ opts_)], cue: :"%leaf" do |opts|
          continue unless opts = Schemas::LeafUnbounded.validated?(opts)

          opts.morph({0, :"%leaves/first"}, {1, pattern(body)})
        end

        matchpi %[(%leaf° body_ ¦ opts_)], cue: :"%leaf°" do |opts|
          continue unless opts = Schemas::LeafUnbounded.validated?(opts)

          opts.morph({0, :"%leaves/source"}, {1, pattern(body)})
        end

        matchpi %[(%leaves capture_ body_ ¦ opts_)], cue: :"%leaves" do |opts|
          continue unless opts = Schemas::LeafBounded.validated?(opts)

          opts.morph(
            {0, :"%leaves/all"},
            {1, {:"%capture", capture}},
            {2, pattern(body)},
          )
        end

        # Leave %new's as-is. They are normalized and compiled when their
        # dependencies are known.
        matchpi %[(%new _)], cue: :"%new" do
          {:"%terminal", pattern}
        end

        # Expand (%string nonempty) into (%all (%not "") _string)
        matchpi %[(%string nonempty)], cue: {:"%string", :nonempty} do
          pattern(Term.of(:"%all", {:"%not", ""}, :_string))
        end

        # NOTE: you should insert new matchpis here, especially if they are infrequent.
        # Below we have raw dict/literal treatment; if you put your matchpis below they
        # will probably not be reached. If your matchpi does not start with a %, make sure
        # to update the dict fast path above.

        matchpi %[_dict] { dict(pattern.unsafe_as_d) }
      end
    end
  end

  module Item
    # TODO: switch to using matchpis here and everywhere!
    def self.operator(item : Term, captures : Bag(Term)) : Operator::Item::Any
      Term.case(item, engine: Term::M0) do
        matchpi %[(%singular child_)], cue: :"%singular" do
          Operator::Item::Singular.new(M1.operator(child, captures))
        end

        match({:"%group", {:"%capture", :capture_}, :_, :"_*"}, cue: :"%group") do |capture|
          Operator::Item::Group.new(capture, item.items.move(2).map { |member| Item.operator(member, captures) })
        end

        match({:"%partition", {:"%many", {:"%capture", :capture_}, :_, :"_*"}, {min: :min0_, max: :max0_}}, cue: :"%many") do |capture, min0, max0|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

          outer = captures
          inner = Bag(Term).new

          sequence = item.items.move(2)
          sequence.each { |node| M1.captures(node, storage: inner) }

          exterior = inner & (outer - inner)
          interior = inner - exterior

          Operator::Item::Many.new(capture, item.items.move(2).map { |member| Item.operator(member, captures) }, interior.set, min, max)
        end

        match({:"%partition", {:"%past", :_, :"_*"}, {min: :min0_, max: :max0_, greedy: :greedy_boolean}}, cue: :"%past") do |min0, max0, greedy|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

          Operator::Item::Past.new(item.items.move(1).map { |member| Item.operator(member, captures) }, min, max, greedy: greedy.true?)
        end

        match({:"%optional", :default_, :body_}, cue: :"%optional") do |default, body|
          Operator::Item::Optional.new(default, M1.operator(body, captures))
        end

        match(Term[:"%plural", {:"%capture", :capture_}, min: :min0_, max: :max0_, type: :type0_symbol], cue: {:"%plural", :"%capture"}) do |capture, min0, max0, type0|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)
          type = type0.unsafe_as_sym.blank.type

          Operator::Item::Plural.new(capture, min, max, type, strategy: :auto)
        end

        match(Term[:"%plural/min", {:"%capture", :capture_}, min: :min0_, max: :max0_, type: :type0_symbol], cue: {:"%plural/min", :"%capture"}) do |capture, min0, max0, type0|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)
          type = type0.unsafe_as_sym.blank.type

          Operator::Item::Plural.new(capture, min, max, type, strategy: :lazy)
        end

        match(Term[:"%plural/max", {:"%capture", :capture_}, min: :min0_, max: :max0_, type: :type0_symbol], cue: {:"%plural/max", :"%capture"}) do |capture, min0, max0, type0|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)
          type = type0.unsafe_as_sym.blank.type

          Operator::Item::Plural.new(capture, min, max, type, strategy: :greedy)
        end

        match(Term[:"%plural", min: :min0_, max: :max0_, type: :type0_symbol], cue: :"%plural") do |min0, max0, type0|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)
          type = type0.unsafe_as_sym.blank.type

          Operator::Item::Plural.new(nil, min, max, type, strategy: :auto)
        end

        match(Term[:"%plural/min", min: :min0_, max: :max0_, type: :type0_symbol], cue: :"%plural/min") do |min0, max0, type0|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)
          type = type0.unsafe_as_sym.blank.type

          Operator::Item::Plural.new(nil, min, max, type, strategy: :lazy)
        end

        match(Term[:"%plural/max", min: :min0_, max: :max0_, type: :type0_symbol], cue: :"%plural/max") do |min0, max0, type0|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)
          type = type0.unsafe_as_sym.blank.type

          Operator::Item::Plural.new(nil, min, max, type, strategy: :greedy)
        end

        match({:"%gap", :measurer_}, cue: :"%gap") do |measurer|
          Operator::Item::Gap.new(M1.operator(measurer, captures), strategy: :sway)
        end

        match({:"%gap/min", :measurer_}, cue: :"%gap/max") do |measurer|
          Operator::Item::Gap.new(M1.operator(measurer, captures), strategy: :lazy)
        end

        match({:"%gap/max", :measurer_}, cue: :"%gap/max") do |measurer|
          Operator::Item::Gap.new(M1.operator(measurer, captures), strategy: :greedy)
        end

        match({:"%slot", :capture_}, cue: :"%slot") do |capture|
          Operator::Item::Slot.new(capture)
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
  def self.normal(pattern : Term) : Term
    Normal.pattern(pattern)
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
  
      def match?(pattern : Term, matchee : Term) : Term::Dict?
        M1.match?(pattern, matchee, opt: O0)
      end
    end

    def self.sketch(normp : Term) : Term::Dict::Sketch
      sketch = Term::Dict::Sketch.new(0)

      M1.walk(normp) do |node|
        # Whitelist certain nodes. All other nodes we avoid. We are defensive because sketch
        # won't work for all nodes. Thus we only calculate it for nodes where we're sure it's
        # going to work.
        Term.case(node, engine: Engine) do
          matchpi %[((%literal %literal) term_)] do
            sketch = Dict.mix(sketch, term)

            WalkDecision::Continue
          end

          matchpi %[((%literal %sketch) _ sketch0_number)] do
            sketch = sketch0.to(Term::Dict::Sketch)

            # We've already computed the sketch for this part of the tree. Move on.
            WalkDecision::Skip
          end

          matchpi(
            %{[(%literal %entries/first) _ successor_]},
            %{[(%literal %entries/source) _ successor_]},
            %{[(%literal %entries/all) _ _ successor_]},
            %{[(%literal %let) _ successor_]},
          ) do
            sketch |= sketch(successor)

            WalkDecision::Skip
          end

          matchpi(
            %{[(%literal %singular) _]},
            %{[(%literal %group) _*]},
            %{[(%literal %partition) _ _]},
            %{[(%literal %itemspart) _*]},
            %{[(%literal %items/first) _*]},
            %{[(%literal %items/source) _*]},
            %{[(%literal %items/all) _*]},
            %{((%literal %leaves/first) _* ¦ _ in: (%not keys))},
            %{((%literal %leaves/source) _* ¦ _ in: (%not keys))},
            %{((%literal %leaves/all) _* ¦ _ in: (%not keys))},
          ) do
            WalkDecision::Continue
          end

          otherwise { WalkDecision::Skip }
        end
      end

      sketch
    end

    def self.sketches(normp : Term) : Term
      keypath = [] of Term
      keypaths = [] of Array(Term)

      # Making `walk` able to replace in-place is just too hard and increases complexity
      # very much. Instead, we collect keypaths. This does have an unwell-ish performance/
      # memory cost but whatever.
      M1.walk(normp, keypath: keypath) do |node|
        Term.case(node, engine: Engine) do
          matchpi(%{
            [(%any %partition
                   %itemspart
                   %items/first
                   %items/source
                   %items/all
                   %entries/first
                   %entries/source
                   %entries/all)
              _*]},
            %{((%literal %leaves/first) _* ¦ _ in: (%not keys))},
            %{((%literal %leaves/source) _* ¦ _ in: (%not keys))},
            %{((%literal %leaves/all) _* ¦ _ in: (%not keys))},
          ) do
            keypaths << keypath.dup

            WalkDecision::Continue
          end

          otherwise { WalkDecision::Continue }
        end
      end

      # Modify deepest keypaths first. Since we're only going to replace at the keypath
      # and do nothing else, no further sorting (e.g. by indices) is required.
      keypaths.unstable_sort_by! { |keypath| -keypath.size }
      keypaths.each do |keypath|
        normp = normp.as_d.follow(keypath) do |node0|
          sketch = sketch(node0)
          sketch.zero? ? node0 : Term.of(:"%sketch", node0, sketch)
        end
      end

      Term.of(normp)
    end

    def self.bounds(normp : Term) : Term
      normp
    end

    def self.depth(normp : Term) : Term
      normp
    end

    def self.population(normp : Term) : Term
      normp
    end
  end

  # O2-level optimizations involve a rewrite loop of the normal pattern. In a series
  # of rewrites, the normal pattern is reduced to the minimum possible, most
  # concrete operators at the cost of longer compilation.
  module O2
  end

  # Applies optimizations of *level* and lower to *normp*. Returns the optimized *normp*.
  def self.optimized(normp : Term, level : O0.class) : Term
    normp
  end

  # :ditto:
  def self.optimized(normp : Term, level : O1.class) : Term
    pipe(normp, O1.sketches, O1.bounds, O1.depth, O1.population)
  end

  # :ditto:
  def self.optimized(normp : Term, level : O2.class) : Term
    pipe(normp, optimized(O1))
  end

  def self.search_part(term : Term) : Search::Part
    case term
    when Term.of(:items)         then Search::Part::ItemsOrdered
    when Term.of(:keys)          then Search::Part::Keys
    when Term.of(:values)        then Search::Part::Values
    when Term.of(:"pair/values") then Search::Part::PairValues
    else
      raise ArgumentError.new
    end
  end

  def self.operator(node : Term, captures : Bag(Term)) : Operator::Any
    Term.case(node, engine: Term::M0) do
      match({:"%pass"}, cue: :"%pass") { Operator::Pass.new }
      match({:"%string"}, cue: :"%string") { Operator::Str.new }
      match({:"%symbol"}, cue: :"%symbol") { Operator::Sym.new }
      match({:"%boolean"}, cue: :"%boolean") { Operator::Boolean.new }
      match({:"%dict"}, cue: :"%dict") { Operator::Dict.new }

      match({ {:"%literal", :"%literal"}, :term_ }, cue: :"%literal") do |term|
        Operator::Literal.new(term)
      end

      match({:"%let", {:"%capture", :capture_}, :successor_}, cue: :"%let") do |capture, successor|
        Operator::Capture.new(capture, operator(successor, captures))
      end

      matchpi %[(%sketch successor_ sketch_number)], cue: :"%sketch" do
        Operator::SketchSubset.new(sketch.to(Term::Dict::Sketch), operator(successor, captures))
      end

      match({:"%itemspart", :"_*"}, cue: :"%itemspart") do
        items = node.items
          .move(1)
          .map { |item| Item.operator(item, captures).as(Operator::Item::Any) }

        Operator::Itemspart.new(items)
      end

      match({:"%keypath", {:"%capture", :capture_}}, cue: :"%keypath") do |capture|
        Operator::Keypath.new(capture)
      end

      match({:"%keypool", :_, :"_*"}, cue: :"%keypool") do
        keys = node.items.move(1)

        Operator::Keypool.new(keys.to_a)
      end

      match({:"%layer", :below_, :side_}, cue: :"%layer") do |below, side|
        entries = Array({Term, Operator::Any}).new(side.size)

        side.each_entry do |k, v|
          entries << {k, operator(v, captures)}
        end

        Operator::Layer.new(operator(below, captures), entries)
      end

      match({:"%pair/required", :key_, :value_}, cue: :"%pair/required") do |key, value|
        Operator::PairRequired.new(key, operator(value, captures))
      end

      match({:"%pair/optional", :key_, :default_, :value_}, cue: :"%pair/optional") do |key, default, value|
        Operator::PairOptional.new(key, default, operator(value, captures))
      end

      match({:"%pair/negative", :key_, {:"%pass"}}, cue: :"%pair/negative") do |key|
        Operator::PairAbsent.new(key)
      end

      match({:"%pair/negative", :key_, {:"%pass"}, :name_}, cue: :"%pair/negative") do |key, name|
        Operator::PairAbsentKeypath.new(key, name)
      end

      match({:"%pair/negative", :key_, :positive_}, cue: :"%pair/negative") do |key, positive|
        Operator::NegativePair.new(key, operator(positive, captures))
      end

      match({:"%pair/negative", :key_, :positive_, :name_}, cue: :"%pair/negative") do |key, positive, name|
        Operator::NegativePairKeypath.new(key, operator(positive, captures), name)
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
        Operator::Tdiv.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (mod n_number)) successor_)], cue: {:"%pipe", :mod} do
        Operator::Mod.new(n.unsafe_as_n, operator(successor, captures))
      end

      matchpi %[(%pipe (%barrier (** n_number)) successor_)], cue: {:"%pipe", :**} do
        Operator::Pow.new(n.unsafe_as_n, operator(successor, captures))
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

      match({:"%items/first", :_, :"_*"}, cue: :"%items/first") do
        sequence = node.items.move(1)

        Operator::ScanFirst.new(sequence.map { |item| operator(item, captures).as(Operator::Any) })
      end

      match({:"%items/source", :_, :"_*"}, :"%items/source") do
        sequence = node.items.move(1)

        Operator::ScanSource.new(sequence.map { |item| operator(item, captures).as(Operator::Any) })
      end

      match(
        {:"%partition",
         {:"%items/all", {:"%capture", :capture_}, :_, :"_*"},
         {min: :min0_, max: :max0_}},
        cue: {:"%items/all", :"%capture"}
      ) do |capture, min0, max0|
        min = min0.to(UInt8)
        max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        needle = sequence.map { |item| operator(item, captures).as(Operator::Any) }

        if exterior.empty?
          Operator::ScanAllIsolated.new(capture, needle, min, max)
        else
          Operator::ScanAll.new(capture, needle, inner.set, exterior.set, min, max)
        end
      end

      match(
        {:"%partition",
         {:"%entries/all", {:"%capture", :capture_}, :k_, :v_},
         {min: :min0_, max: :max0_}},
        cue: {:"%entries/all", :"%capture"}
      ) do |capture, k, v, min0, max0|
        min = min0.to(UInt8)
        max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        if exterior.empty?
          Operator::EntriesAllIsolated.new(capture,
            kop: operator(k, captures),
            vop: operator(v, captures),
            min: min,
            max: max
          )
        else
          Operator::EntriesAll.new(capture,
            kop: operator(k, captures),
            vop: operator(v, captures),
            exterior: exterior.set,
            selector: inner.set,
            min: min,
            max: max,
          )
        end
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

      match(Term[:"%leaves/first", :body_, in: :part_, order: :dfs, self: :depth0_boolean], cue: :"%leaves/first") do |body, part, depth0|
        Operator::DfsFirst.new(operator(body, captures), part: search_part(part), depth0: depth0.true?)
      end

      match(Term[:"%leaves/first", :body_, in: :part_, order: :bfs, self: :depth0_boolean], cue: :"%leaves/first") do |body, part, depth0|
        Operator::BfsFirst.new(operator(body, captures), part: search_part(part), depth0: depth0.true?)
      end

      match(Term[:"%leaves/source", :body_, in: :part_, order: :dfs, self: :depth0_boolean], cue: :"%leaves/source") do |body, part, depth0|
        Operator::DfsSource.new(operator(body, captures), part: search_part(part), depth0: depth0.true?)
      end

      match(Term[:"%leaves/all", {:"%capture", :capture_}, :body_, in: :part_, min: :min_, max: :max_, order: :dfs, self: :depth0_boolean], cue: :"%leaves/all") do |capture, body, part, min, max, depth0|
        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        min = min.to(UInt8)
        max = max == SYM_INF ? 0u8 : max.to(UInt8)

        if exterior.empty?
          Operator::DfsAllIsolated.new(capture, operator(body, captures), search_part(part), min, max, depth0.true?)
        else
          Operator::DfsAll.new(capture, operator(body, captures), inner.set, exterior.set, search_part(part), min, max, depth0.true?)
        end
      end

      match(Term[:"%leaves/all", {:"%capture", :capture_}, :body_, in: :part_, min: :min_, max: :max_, order: :bfs, self: :depth0_boolean], cue: :"%leaves/all") do |capture, body, part, min, max, depth0|
        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        min = min.to(UInt8)
        max = max == SYM_INF ? 0u8 : max.to(UInt8)

        if exterior.empty?
          Operator::BfsAllIsolated.new(capture, operator(body, captures), search_part(part), min, max, depth0.true?)
        else
          Operator::BfsAll.new(capture, operator(body, captures), inner.set, exterior.set, search_part(part), min, max, depth0.true?)
        end
      end

      match({ {:"%literal", :"%partition"}, :itemspart_, :pairspart_ }, cue: :"%partition") do |itemspart, pairspart|
        Operator::Partition.new(operator(itemspart, captures), operator(pairspart, captures))
      end

      match({:"%all", :a_}, cue: :"%all") do |a|
        operator(a, captures)
      end

      match({:"%all", :a_, :b_}, cue: :"%all") do |a, b|
        Operator::Both.new(operator(a, captures), operator(b, captures))
      end

      match({:"%all", :a_, :_, :"_*"}, cue: :"%all") do |a|
        rest = Term::Dict.build do |commit|
          commit << :"%all"

          args = node.items.move(2)
          args.each { |item| commit << item }
        end

        Operator::Both.new(operator(a, captures), operator(Term.of(rest), captures))
      end

      matchpi %[(%any/literal _*)], cue: :"%any/literal" do
        branches = node.items.move(1).to_a
        branches.uniq!

        Operator::Choices.new(branches)
      end

      match({:"%any/source", :a_}, cue: :"%any/source") do |a|
        operator(a, captures)
      end

      match({:"%any/source", :a_, :b_}, cue: :"%any/source") do |a, b|
        Operator::EitherSource.new(operator(a, captures), operator(b, captures))
      end

      match({:"%any/source", :a_, :_, :"_*"}, cue: :"%any/source") do |a|
        rest = Term::Dict.build do |commit|
          commit << :"%any/source"

          args = node.items.move(2)
          args.each { |item| commit << item }
        end

        Operator::EitherSource.new(operator(a, captures), operator(Term.of(rest), captures))
      end

      match({:"%edge", {:"%literal", :_}}, cue: :"%edge") do
        Operator::EdgeUntyped.new
      end

      match({:"%edge", {:"%literal", :_symbol}}, cue: :"%edge") do
        Operator::EdgeTyped.new(:symbol)
      end

      match({:"%edge", {:"%literal", :_string}}, cue: :"%edge") do
        Operator::EdgeTyped.new(:string)
      end

      match({:"%edge", {:"%literal", :_number}}, cue: :"%edge") do
        Operator::EdgeTyped.new(:number)
      end

      match({:"%not", :_, :"_*"}, cue: :"%not") do
        blacklist = node.items.move(1)

        Operator::Not.new(blacklist.set)
      end

      match({:"%number", {:"%literal", :_}}, cue: :"%number") do
        Operator::Num.new
      end

      match({:"%number", {:"%literal", {:whole, :_}}}, cue: {:"%number", :whole}) do
        Operator::Num.new(min: nil, max: nil, options: :whole)
      end

      match({:"%number", :x_, :op_symbol, :b_number}, cue: :"%number") do |x, op, b|
        b = b.unsafe_as_n

        case x
        when Term.of(:_)
          options = {:none}
        when Term.of(:whole, :_)
          options = {:whole}
        else
          raise ArgumentError.new
        end

        case op
        when SYM_LT  then Operator::Num.new(min: nil, max: b, options: {*options, :max_excluded})
        when SYM_LTE then Operator::Num.new(min: nil, max: b, options: {*options, :none})
        when SYM_GT  then Operator::Num.new(min: b, max: nil, options: {*options, :min_excluded})
        when SYM_GTE then Operator::Num.new(min: b, max: nil, options: {*options, :none})
        else
          raise ArgumentError.new
        end
      end

      match({:"%number", :a_number, :lop_symbol, :x_, :rop_symbol, :b_number}, cue: :"%number") do |a, lop, x, rop, b|
        a = a.unsafe_as_n
        b = b.unsafe_as_n

        case x
        when Term.of(:_)
          options = {:none}
        when Term.of(:whole, :_)
          options = {:whole}
        else
          raise ArgumentError.new
        end

        case lop
        when SYM_LT  then options = {*options, :min_excluded}
        when SYM_LTE then options = {*options, :none} # < Because Crystal doesn't like Tuple unions
        else
          raise ArgumentError.new
        end

        case rop
        when SYM_LT  then options = {*options, :max_excluded}
        when SYM_LTE then options = {*options, :none} # < Because Crystal doesn't like Tuple unions
        else
          raise ArgumentError.new
        end

        Operator::Num.new(min: a, max: b, options: options)
      end

      match({:"%new", :pattern_}, cue: :"%new") do |pattern|
        Operator::New.new((blanks(pattern) & captures).array, pattern)
      end

      # %terminal is used to mark terminal nodes for walk
      matchpi %[(%terminal subnode_)], cue: :"%terminal" do
        operator(subnode, captures)
      end

      otherwise { raise ArgumentError.new("BUG: cannot compile #{node}") }
    end
  end

  private def self.blank_name?(term : Term) : Term?
    return unless symbol = term.as_sym?
    return unless blank = symbol.blank?
    return unless name = blank.name?

    Term.of(name)
  end

  # Returns a bag of blank names in *term*. The blanks are interpreted as "holes"
  # rather than like in patterns; so *term* can be any term whatsoever.
  def self.blanks(term : Term) : Bag(Term)
    blanks = Bag(Term).new
    blanks0(term, blanks)
    blanks
  end

  private def self.blanks0(term : Term, blanks : Bag(Term)) : Nil
    if name0 = blank_name?(term)
      blanks << name0
      return
    end

    return unless dict = term.as_d?

    dict.each_entry do |key, value|
      blanks0(key, blanks)
      blanks0(value, blanks)
    end
  end

  # Recursively substitutes blanks in *term*, by name, with values from *subt*.
  #
  # "By name" means only blank name is considered, e.g. *subt* `{x: 100}` will
  # replace blank `x_`. Polyblanks are replaced by multiple elements if
  # the replacement is a dict.
  def self.bsubst(term : Term, subt : Term::Dict) : Term
    if (symbol = term.as_sym?) && (blank = symbol.blank?) && (name = blank.name?)
      return subt[name]? || term
    end

    bsubst0(term, subt)
  end

  private def self.bsubst0(term : Term, subt : Term::Dict) : Term
    return term unless dict0 = term.as_d?

    dict1 = Term::Dict.build do |dict1|
      dict0.items.each do |item|
        unless symbol = item.as_sym?
          dict1 << bsubst0(item, subt)
          next
        end

        unless (blank = symbol.blank?) && (name = blank.name?) && (value = subt[name]?)
          dict1 << item
          next
        end

        if blank.poly? && (values = value.as_itemsonly_d?)
          dict1.concat(values.items)
        else
          dict1 << value
        end
      end

      # ?! The fact that this can override keys isn't quite making me happy
      dict0.pairs.each_entry do |key, value|
        dict1.with(bsubst(key, subt), bsubst(value, subt))
      end
    end

    Term.of(dict1)
  end

  enum WalkDecision : UInt8
    Continue
    Skip
    Halt
  end

  # Returns `true` if *id* is probably a pattern engine node id.
  def self.probably_node?(id : Term::Sym) : Bool
    id.to(String).prefixed_by?('%')
  end

  # Used as a constant to indicate that `walk` should walk thoroughly,
  # that is, it should include all %-nodes, including itemspart nodes
  # such as %singular.
  module WalkMode::Thorough
  end

  # Used as a constant to indicate that `walk` should continue into itemspart
  # nodes such as %singular without yielding them to the callback.
  module WalkMode::NonItemspart
  end

  def self.walk(root : Term, mode : WalkMode::Thorough.class, callable, *, keypath = nil) : WalkDecision
    Term.case(root, engine: Term::M0) do
      # Barrier is for higher-level nodes to protect their arguments.
      matchpi %[(%barrier _)], cue: :"%barrier" do
        WalkDecision::Skip
      end

      # Captures, slots, and literals are all terminal nodes. We do not require to
      # wrap them in %terminal because this is sort of evident.
      matchpi(
        %[((%literal %literal) _)],
        %[(%slot _)],
        %[(%capture _)],
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

  def self.walk(root : Term, mode : WalkMode::NonItemspart.class, callable, *, itemspart : Bool = false, keypath = nil) : WalkDecision
    walk(root, mode: WalkMode::Thorough, keypath: keypath) do |node|
      Term.case(node, engine: Term::M0) do
        if itemspart
          # Recurse into M1 non-itemspart children with itemspart flag off.
          matchpi(
            %[(%singular child_)],
            %[(%gap child_)],
            %[(%gap/min child_)],
            %[(%gap/max child_)],
            %[(%optional _ child_)],
            cues: {:"%singular", :"%gap", :"%gap/min", :"%gap/max", :"%optional"},
          ) do
            case walk(child, mode, callable, itemspart: false, keypath: keypath)
            in .continue?, .skip?
              WalkDecision::Skip
            in .halt?
              WalkDecision::Halt
            end
          end

          # Recurse into %group and %many with itemspart flag on.
          matchpi(
            %{[%group _ _ _*]},
            %{[%many _ _ _*]},
            %{[%past _ _*]},
            %{[%past/max _ _*]},
            cues: {:"%group", :"%many", :"%past", :"%past/max"},
          ) do
            WalkDecision::Continue
          end

          # Avoid all other itemspart nodes.
          otherwise { WalkDecision::Skip }
        else
          matchpi %[(%itemspart _*)], cue: :"%itemspart" do
            decision = callable.call(node)

            if decision.continue?
              node.each_item_with_index do |item, index|
                keypath.try &.push(Term.of(index))

                case walk(item, mode, callable, itemspart: true, keypath: keypath)
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
      Term.case(node, engine: Term::M0) do
        match({:"%capture", :capture_}) do |capture|
          storage << capture

          WalkDecision::Skip
        end

        otherwise do
          WalkDecision::Continue
        end
      end
    end

    storage
  end

  PATTERN_CACHE = Pf::Cache(Void*, Operator::Any).new

  {% if flag?(:popt_0) %}
    DEFAULT_OPT_LEVEL = O0
  {% elsif flag?(:popt_1) %}
    DEFAULT_OPT_LEVEL = O1
  {% else %}
    DEFAULT_OPT_LEVEL = O2
  {% end %}

  # TODO: overwrite in cache if higher opt level
  def self.operator(pattern : Term, *, normalize = true, fresh = false, opt = DEFAULT_OPT_LEVEL) : Operator::Any
    PATTERN_CACHE.fetch(pattern.unsafe_repr, fresh: fresh) do
      normal = normalize ? normal(pattern) : pattern
      captures = captures(normal)
      pipe(normal, optimized(opt), operator(captures))
    end
  end

  def self.matches(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], opt = DEFAULT_OPT_LEVEL, **kwargs) : Array(Term::Dict)
    Operator.matches(env, operator(pattern, opt: opt), matchee, **kwargs)
  end

  def self.match?(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], opt = DEFAULT_OPT_LEVEL, **kwargs)
    Operator.match?(env, operator(pattern, opt: opt), matchee, **kwargs)
  end
end

module ::Ww::Term::M1
end

module ::Ww::Term::M1
  struct AttachMetadata
    def initialize(@capture : Term, @body : Term?, @env : Term::Dict, @plural : Bool)
    end

    def call(node)
      node.morph(
        {:plural, @plural ? true : nil},
        {:transform, @body},
        {:env, @env},
        {:aliases, @capture, true},
      )
    end
  end

  struct AttachAlias
    def initialize(@capture : Term)
    end

    def call(node)
      node.morph({:aliases, @capture, true})
    end
  end

  def self.reflect1(ctx, meta : Term::Dict, matchee : Term)
    return ctx unless aliases = meta[:aliases]?

    aliases.ee.reduce(ctx) { |ctx, (capture, _)| ctx.with(capture, matchee) }
  end

  def self.reflect(ctx, node : Term::Dict, maxdepth : UInt32, matchee : Term)
    if (self0 = node[:endpoint]?) && (metadata = self0.as_d?)
      ctx = reflect1(ctx, metadata, matchee)
    end

    return ctx if maxdepth.zero?

    node.each_entry do |label, successor|
      Term.case(label) do
        matchpi %[(value key_)] do
          ctx = reflect(ctx, successor.as_d, maxdepth - 1, matchee[key])
        end

        matchpi %[(residue keys_*)] do
          ctx = reflect(ctx, successor.as_d, maxdepth - 1, Term.of(matchee &- keys.items))
        end

        otherwise { }
      end
    end

    ctx
  end

  struct DefaultApplier
    def apply(up0, down, my, body)
      Term.case(body) do
        matchpi %[($my capture_)] do
          my[capture]? || body
        end

        matchpi %[($up capture_)] do
          up0[capture]? || my[capture]? || body
        end

        matchpi %[($down capture_)] do
          down[capture]? || my[capture]? || body
        end

        matchpi %[_dict] do
          Term.of(body.unsafe_as_d.replace { |_, v| apply(up0, down, my, v) })
        end

        otherwise do
          body
        end
      end
    end

    # Applier must respond to `call(up0 : Term::Dict, up1 : Term::Dict, down : Term::Dict, my : Term::Dict, matchee0 : Term?, body : Term) : {up1 : Term::Dict, matchee1 : Term}`
    #
    # `matchee0` is absent (`nil`) in ephemeral pairs with no default value,
    # as in the following backmap:
    #
    # ```wwml
    # {x: (%- _ x), y: y_} <> {x: ↑y}
    # ```
    #
    # Running this backmap, `x` would be mounted as an ephemeral pair-value with
    # no default value, and thus with no corresponding matchee. The transform `↑y`'s
    # applier is then run with `nil` *matchee0*.
    def call(up0, up1, down, my, matchee0 : Term?, body)
      Term.case(body) do
        matchpi %[($tr pred_ succ_)] do
          {up1.with(pred, matchee0), apply(up0, down, my, succ)}
        end

        otherwise do
          {up1, apply(up0, down, my, body)}
        end
      end
    end
  end

  # ~2 weeks:
  #   TODO: bring back sketch, bounds range in an O1 optimization pass. depth range optimization.
  #         dict population sketch (u16 #numbers, u16 #strings, u16 #dicts, u16 #symbols -> u64 popsketch),
  #         u16::max means "infinity" or "a lot of them", if u16::max we cannot subtract anymore on without(),
  #         otherwise we can.
  #   TODO: O2 rewrite loop optimizer based on editor rules. editor is the main optimization client/benchmark!
  #   TODO: finish refactoring of ahead into structs instead of procs. Remove Ahead/ItemAhead::Fn. KeypathTip optimization.
  #   TODO: move from Operator classes to structs by introducing Operator::Ref, having an array of operators,
  #         Ref is an index into that array. at match() time resolve Ref into Operator::Any. Reduce indirections.
  #   TODO: look into optimizing backmaps. Is that possible?
  #   TODO: refactors, split into files, etc. Done for the most part, although some edge cases
  #         are inevitably not going to be handled so well. But my rule is -- no test, no pest.
  #         If (or when?) our users hit edge case bugs with a reproducible example, then we're talking.
  # --- Sometime
  # TODO: replay editor tests with multiple cursors

  # Layer-0 transform handles `self` props: applies transform and adds itself
  # to ctx1 (if requested).
  def self.transform0(ctx0, ctx1, bot, applier, node, matchee : Term?)
    return ctx1, matchee unless props = node[:endpoint]?
    return ctx1, matchee unless body = props[:transform]?

    ctx1, matchee = applier.call(ctx0, ctx1, bot, props[:env].as_d, matchee, body)

    return ctx1, matchee unless aliases = props[:aliases]?

    aliases.each_entry do |capture, _|
      ctx1 = ctx1.with(capture, matchee)
    end

    {ctx1, matchee}
  end

  # Layer-1 transform handles insertions made in transform0 into the matchee
  # (of ranges, slots, etc.)
  #
  # That is, it serves `plural: true` for values and `(range ...)` labels.
  def self.transform1(ctx0, ctx1, bot, applier, node, matchee)
    matchee0 = matchee = matchee.as_d? || return ctx1, matchee

    insertions = nil

    node.each_entry do |label, successor|
      Term.case(label) do
        matchpi %[self] { }
        matchpi %[endpoint] { }

        matchpi %[(residue keys_*)] do
          matchee = matchee.transaction do |commit|
            residue0 = matchee &- keys.items
            ctx1, residue1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, Term.of(residue0))
            residue1 &-= keys.items
            residue0.each_entry { |k, _| commit.without(k) }
            residue1.each_entry { |k, v| commit.with(k, v) }
          end
        end

        matchpi %[(ephemeral key_)] do
          ctx1, value1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, nil)
          matchee = matchee.with(key, value1)
        end

        matchpi %[(ephemeral key_ default_)] do
          ctx1, value1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, default)
          matchee = matchee.with(key, value1)
        end

        matchpi %[(value key_)] do
          if ksucc = successor[:self]?
            # One should be able to delete a key-value pair from a backmap with an empty
            # plural **key** transform:
            #
            #   ;; Removes K from dict. Note the semi-necessary alias that prevents
            #   ;; us from erasing without's k arg.
            #   (without (%value K _) K←k_) <> {(K): ()}
            #
            #   ;; With alias:
            #   (without {x: 100, y: 200} x) ;; => (without {y: 200} x)
            #   ;; Without alias:
            #   (without {x: 100, y: 200} x) ;; => (without {y: 200})
            #
            if ksucc[:endpoint, :plural]? && (transform = ksucc[:endpoint, :transform]?) && transform.empty?
              matchee = matchee.without(key)
              next
            end

            key0, value0 = key, matchee[key]
            ctx1, key1 = transform0(ctx0, ctx1, bot, applier, ksucc, key0)
            matchee = matchee.without(key0).with(key1, value0)
            key = key1
          end

          next unless successor[:endpoint, :transform]?

          plural = !!successor[:endpoint, :plural]?

          if plural && (b = key.as_n?) && b.in?(matchee.items.bounds)
            value0 = matchee[key]

            ctx1, values1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, value0)

            insertions ||= [] of {Term::Num, Term::Num, Term::Num, Term::Dict}
            index = insertions.bsearch_index { |(c_b, _, c_ord, _)| {-b, Term[0]} <= {-c_b, -c_ord} }
            index ||= insertions.size
            insertions.insert(index, {b, b + 1, Term[0], values1.as_d? || Term[{values1}]})

            next
          end

          matchee = matchee.with(key) do |value0|
            unless value0
              raise KeypathError.new
            end

            ctx1, value1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, value0)

            # One should be able to delete a key-value pair from a backmap with an empty
            # plural **value** transform:
            #
            #   {x: x_, y: y_} <> {(x): ()} ;; Removes `x` pair
            #
            plural && value1.type.dict? && value1.empty? ? nil : value1
          end
        end

        matchpi %[(range bt_number et_number)] do
          next unless successor[:endpoint, :transform]?

          b = bt.unsafe_as_n
          e = et.unsafe_as_n

          unless (b...e).subrange_of?(matchee.items.bounds)
            raise KeypathError.new
          end

          values0 = matchee.items(b, e)
          ctx1, values1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, Term.of(values0))

          insertions ||= [] of {Term::Num, Term::Num, Term::Num, Term::Dict}
          index = insertions.bsearch_index { |(c_b, _, c_ord, _)| {-b, Term[0]} <= {-c_b, -c_ord} }
          index ||= insertions.size
          insertions.insert(index, {b, e, Term[0], values1.as_d? || Term[{values1}]})
        end

        matchpi %[(range bt_number et_number ordt_number)] do
          next unless successor[:endpoint, :transform]?

          b = bt.unsafe_as_n
          e = et.unsafe_as_n
          ord = ordt.unsafe_as_n

          unless (b...e).subrange_of?(matchee.items.bounds)
            raise KeypathError.new
          end

          values0 = matchee.items(b, e)
          ctx1, values1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, Term.of(values0))

          if successor[:endpoint, :plural]?
            values1 = values1.as_d? || Term[{values1}]
          else
            values1 = Term[{values1}]
          end

          insertions ||= [] of {Term::Num, Term::Num, Term::Num, Term::Dict}
          index = insertions.bsearch_index { |(c_b, _, c_ord, _)| {-b, -ord} <= {-c_b, -c_ord} }
          index ||= insertions.size
          insertions.insert(index, {b, e, ord, values1})
        end

        matchpi %[(ephemeral bt_number ordt_number value0_)] do
          b = bt.unsafe_as_n
          ord = ordt.unsafe_as_n
          unless b.in?(matchee.items.bounds)
            raise KeypathError.new
          end

          ctx1, values1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, value0)

          if successor[:endpoint, :plural]?
            values1 = values1.as_d? || Term[{values1}]
          else
            values1 = Term[{values1}]
          end

          insertions ||= [] of {Term::Num, Term::Num, Term::Num, Term::Dict}
          index = insertions.bsearch_index { |(c_b, _, c_ord, _)| {-b, -ord} <= {-c_b, -c_ord} }
          index ||= insertions.size
          insertions.insert(index, {b, b, ord, values1})
        end

        otherwise { raise ArgumentError.new("#{label}") }
      end
    end

    # TODO: "fill in" ranges in insertions, this will allow us to apply all
    # insertions in a single transaction
    if insertions
      insertions.each do |b, e, _, values|
        matchee = matchee.replace(b...e, &.concat(values.items))
      end
    end

    {ctx1, Term.of(matchee)}
  end

  def self.transform(ctx0, ctx1, bot, applier, node0 : Term::Dict, layer : UInt32, matchee : Term)
    case layer
    when 0
      ctx1, matchee = transform0(ctx0, ctx1, bot, applier, node0, matchee)

      {ctx1, node0, matchee}
    when 1
      ctx1, matchee = transform1(ctx0, ctx1, bot, applier, node0, matchee)

      {ctx1, node0, matchee}
    else
      matchee = matchee.as_d? || return ctx1, node0, matchee

      node1 = node0
      node0.each_entry do |label0, successor0|
        Term.case(label0) do
          matchpi %[(value key0_)] do
            # Read the key and transform it using the current version of
            # the successor.
            ctx1, successor1, value1 = transform(ctx0, ctx1, bot, applier, successor0.as_d, layer - 1, matchee[key0])

            # If there is `self` defined on the successor, this means that
            # the key should be modified as well.
            if ksucc0 = successor1[:self]?
              ctx1, ksucc1, key1 = transform(ctx0, ctx1, bot, applier, ksucc0.as_d, layer - 1, key0)
              matchee = matchee.without(key0).with(key1, value1)
              node1 = node1
                .without(label0)
                .with({:value, key1}, successor1.with(:self, ksucc1))
            else
              matchee = matchee.with(key0, value1)
              node1 = node1.with(label0, successor1)
            end
          end

          matchpi %[(ephemeral _number _number value0_)] do
            ctx1, successor1, value1 = transform(ctx0, ctx1, bot, applier, successor0.as_d, layer - 1, value0)
            label1 = label0.with(3, value1)
            node1 = node1.without(label0).with(label1, successor1)
          end

          matchpi %[(residue keys_*)] do
            matchee = matchee.transaction do |commit|
              residue0 = matchee &- keys.items
              ctx1, successor1, residue1 = transform(ctx0, ctx1, bot, applier, successor0.as_d, layer - 1, Term.of(residue0))
              node1 = node1.with(label0, successor1)
              residue1 &-= keys.items
              residue0.each_entry { |k, _| commit.without(k) }
              residue1.each_entry { |k, v| commit.with(k, v) }
            end
          end

          otherwise { }
        end
      end

      {ctx1, node1, Term.of(matchee)}
    end
  end

  # Applier must respond to `call(up0 : Term::Dict, up1 : Term::Dict, down : Term::Dict, my : Term::Dict, matchee0 : Term, body : Term) : {up1 : Term::Dict, matchee1 : Term}`
  def self.backmap(envs : Enumerable(Term::Dict), backspec : Term, matchee : Term, *, applier = DefaultApplier.new) : Term
    # Collapse all keypaths into a trie. Enhance the trie with metadata. Simultaneously,
    # figure out the depth of the trie by finding the maximum keypath size.
    trie = Term[]
    depth = 0u32

    envs.each do |env|
      next unless keypaths = env[:"(keypaths)"]?

      env = env.without(:"(keypaths)")

      keypaths.each_entry do |capture, keypathset|
        keypathset.each_entry do |keypath, _|
          unless keypath = keypath.as_d?
            raise KeypathError.new
          end

          plural = false

          if body = backspec[capture]?
            action = AttachMetadata.new(capture, body, env, plural: false)
          elsif body = backspec[{capture}]?
            action = AttachMetadata.new(capture, body, env, plural: true)
          else
            # No body means it's an alias. We only must learn the alias's new value.
            # No overrides, nothing. If both have bodies AND point to the same place
            # the winner will be determined by the hash function.
            action = AttachAlias.new(capture)
          end

          trie = Term::Dict.enhance(trie, keypath.items, :endpoint, action: action)
          depth = Math.max(keypath.size.to_u32, depth)
        end
      end
    end

    # puts ML.display(trie)

    ctx0 = Term[]

    (0..depth).reverse_each do |layer|
      upper = reflect(Term[], trie, layer, matchee)
      # pp upper
      lower = ctx0.sub(upper)
      ctx0 |= upper
      ctx0, trie, matchee = transform(ctx0, ctx0, lower, applier, trie, layer, matchee)
    end

    matchee
  end

  def self.backmap?(operator : Operator::Any, backspec : Term, matchee : Term, *, env = Term[], applier = DefaultApplier.new) : Term?
    case fb = Operator.feedback(env, operator, matchee, keypaths: true)
    in Operator::Fb::Match
      backmap(fb.envs, backspec, matchee, applier: applier)
    in Operator::Fb::Mismatch
    end
  end

  def self.backmap?(pattern : Term, backspec : Term, matchee : Term, *, env = Term[], applier = DefaultApplier.new) : Term?
    backmap?(operator(pattern), backspec, matchee, env: env, applier: applier)
  end
end

class ::Ww::KeypathError < Exception
end

class ::Ww::Term::Dict
  private struct Enhance(T, U)
    def initialize(@rest : T, @action : U)
    end

    def call(endpoint)
      unless endpoint0 = endpoint.as_d?
        return endpoint
      end

      Term::Dict.enhance(endpoint0, *@rest, action: @action)
    end
  end

  private struct Enhance0(T, U)
    def initialize(@keypath : T, @index : Int32, @action : U)
    end

    def call(endpoint)
      unless endpoint0 = endpoint.as_d?
        return endpoint
      end

      Term::Dict.enhance0(endpoint0, @keypath, @index, action: @action)
    end
  end

  def self.enhance0(dict : Term::Dict, keypath : Indexable(Term), index : Int, action) : Term::Dict
    if index >= keypath.size
      return action.call(dict)
    end

    enhance(dict, keypath[index], Enhance0.new(keypath, index + 1, action))
  end

  def self.enhance(dict : Term::Dict, key, action) : Term::Dict
    dict.with(key) { |value| action.call(value || Term.of) }
  end

  def self.enhance(dict : Term::Dict, key : Indexable(Term), action) : Term::Dict
    enhance0(dict, key, 0, action)
  end

  def self.enhance(dict : Term::Dict, key, *rest, action) : Term::Dict
    enhance(dict, key, Enhance.new(rest, action))
  end
end

module ::Ww::Term::M1
  # Returns the minimum and maximum expected matchee depth for a normal pattern
  # *normp*. They participate in determining the specificity of a pattern.
  #
  # Operators such as `%leaf` make it impossible to tell the *maximum* expected depth
  # of the matchee; but leave the possibility of determining its *minimum required
  # depth*. Additionally, `%leaf` and its variants introduce one level of depth
  # themselves when their `self` option is turned off.
  def self.depth(normp : Term) : {Magnitude, Magnitude}
    mindepth = Magnitude.new(0)
    maxdepth = Magnitude.new(0)

    walk(normp) do |node|
      Term.case(node) do
        # - The two basic "boundaries" between dictionaries are %singular and %pair/required.
        #   Each "crossing" of those boundaries results in re-evaluation of min-depth.
        # - In non-self mode %leaf variants also act as such bondaries.
        matchpi(
          %[(%singular child_)],
          %[(%pair/required _ child_)],
          %[(%leaves/first child_ in: _ order: _ self: false)],
          %[(%leaves/source child_ in: _ order: _ self: false)],
          %[(%leaves/all (%capture _) child_ in: _ order: _ self: false min: _ max: _)],
        ) do
          child_min, child_max = depth(child)

          mindepth = Math.max(mindepth, child_min + Magnitude.new(1))
          maxdepth = Math.max(maxdepth, child_max + Magnitude.new(1))

          WalkDecision::Skip
        end

        # Take into account the depth of the literal dicts in the pattern.
        matchpi %[(%literal value_dict)] do
          value_depth = Magnitude.new(value.maxdepth)

          mindepth = Math.max(mindepth, value_depth)
          maxdepth = Math.max(maxdepth, value_depth)

          WalkDecision::Skip
        end

        # Take into account keypool itself and depths of the keys listed in it.
        matchpi %[(%keypool keys_+)] do
          max_key_depth = keys.items.max_of { |key| Magnitude.new(key.type.dict? ? key.maxdepth : 0) }

          mindepth = Math.max(mindepth, Magnitude.new(max_key_depth + 1))
          maxdepth = Math.max(maxdepth, Magnitude.new(max_key_depth + 1))

          WalkDecision::Skip
        end

        # These all introduce one level of nesting.
        matchpi(
          %[(%items/first _+)],
          %[(%items/source _+)],
          %[(%items/all (%capture _) _+)],
          %[(%entries/first _ _)],
          %[(%entries/source _ _)],
          %[(%entries/all (%capture _) _ _)]
        ) do
          mindepth = Math.max(mindepth, Magnitude.new(1))
          maxdepth = Math.max(maxdepth, Magnitude.new(1))

          WalkDecision::Continue
        end

        otherwise { WalkDecision::Continue }
      end
    end

    {mindepth, maxdepth}
  end

  # A summary of measurements concerning the specificity of a pattern.
  alias Specificity = {UInt32, UInt32, UInt32, UInt32}

  # Specificity assigned to a top-level literal pattern such as `qux`, `(+ 1 2)`.
  SPECIFICITY_LITERAL = {UInt32::MAX, 0u32, 0u32, 0u32}

  # Specificity assigned to a top-level literal alternative, e.g. `(%any 0 1 2)`.
  SPECIFICITY_ANY = {UInt32::MAX - 1, 0u32, 0u32, 0u32}

  private def self.specificity0?(normp : Term)
    Term.case(normp) do
      # Recurse into top-level `%let`s and `%terminal`'s.
      matchpi %[(%terminal successor_)], %[((%literal %let) _ successor_)] do
        specificity0?(successor)
      end

      # If we have a literal or %any at the top level, issue max specificity
      # and exit immediately.
      matchpi %[((%literal %literal) _)] { SPECIFICITY_LITERAL }
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

    walk(normp, mode: WalkMode::NonItemspart) do |operator|
      Term.case(operator) do
        matchpi %[(%capture _)] do
          unless captures.add?(operator)
            repeats += 1
          end

          WalkDecision::Continue
        end

        matchpi %[((%literal %literal) d_dict)] do
          literals += d.population.total

          WalkDecision::Continue
        end

        matchpi %[((%literal %literal) _)] do
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
        matchpi %[((%literal %edge) (%literal _))] do
          literals += 1

          WalkDecision::Skip
        end

        matchpi %[((%literal %edge) _)] do
          literals += 1
          restrictions += 1

          WalkDecision::Skip
        end

        # Count stuff such as (%number _ < 10) as two restrictions: one on the type
        # and one on the magnitude; and e.g. (%number (whole _) < 10) as three: one on
        # the type, one on the magnitude, and one on the value.
        matchpi(
          %[((%literal %number) (%literal _) _ _)],
          %[((%literal %number) (%literal (whole _)))],
        ) do
          restrictions += 2

          WalkDecision::Continue
        end

        matchpi %[((%literal %number) (%literal (whole _)) _ _)] do
          restrictions += 2

          WalkDecision::Continue
        end

        # Count stuff such as (%number 0 < _ < 10) as three restrictions: one on the type
        # and two on the magnitude.
        matchpi %[((%literal %number) _ _ (%literal _) _ _)] do
          restrictions += 3

          WalkDecision::Continue
        end

        # Count stuff such as (%number 0 < (whole _) < 10) as four restrictions: one on
        # the type, two on the magnitude, and one on the value.
        matchpi %[((%literal %number) _ _ (%literal (whole _)) _ _)] do
          restrictions += 4

          WalkDecision::Continue
        end

        # Rather than listing all operators that make restrictions, we list those that
        # *do not*. This is because most operators make restrictions.
        matchpi(
          %[((%literal %pass))],
          %[((%literal %let) _ _)],
          %[((%literal %not) _+)],
          %[((%literal %new) _)],
        ) { WalkDecision::Continue }

        # %all sums the specificity of its offshoots.
        matchpi %[((%literal %all) offshoots_+)] do
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

        matchpi %[((%literal %keypool) keys_+)] do
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
        matchpi %[((%literal %let) _ successor_)] { Head.operator(successor) }
        matchpi %[((%literal %literal) term_)] { Some.new(term) }
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
        matchpi %[((%past ((%literal %slot) _)))] { More.new }
        matchpi %[((%past/max ((%literal %slot) _)) successor_ successors_*)] do
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
  # For example, in `(+ a_ b_)` that would be `+`; and in `(⭳a ⭳b x←qux x_ y_)` that
  # would be `qux`. On the other hand, for `qux` or `(xs_* qux)` the head is
  # indeterminate (because we'd have to know how many `xs` there were), therefore,
  # `nil` is returned.
  def self.head?(normp : Term) : Term?
    Term.case(normp) do
      matchpi %[(%itemspart items_+)] do
        case response = Head.item(items)
        in Head::Some then response.term
        in Head::None, Head::More
        end
      end

      matchpi %[(%terminal successor_)] { head?(successor) }
      matchpi %[((%literal %partition) itemspart_ _)] { head?(itemspart) }
      matchpi %[((%literal %let) _ successor_)] { head?(successor) }
      matchpi %[((%literal %literal) [head_ _*])] { head } 

      otherwise {}
    end
  end
end

# Represents a pattern within a `PatternSet`. Has no expected use outside of `PatternSet`.
struct Pattern
  private alias O = Term::M1::Operator

  # Returns the index of this pattern. You are free to treat it as `PatternSet`-unique
  # identifier of this pattern.
  getter index : UInt32

  # Returns the underlying M1 operator.
  getter operator : O::Any

  # :nodoc:
  def initialize(@index : UInt32, @operator : O::Any)
  end

  # Returns the response of this pattern to *matchee* (may be positive or negative).
  def response(matchee : Term, *, env = Term[], keypaths = false) : Pr::Any
    case fb = O.feedback(env, @operator, matchee, keypaths: keypaths)
    in O::Fb::MatchOne  then Pr::One.new(self, fb.env)
    in O::Fb::MatchMany then Pr::Many.new(self, fb.envs)
    in O::Fb::Mismatch  then Pr::Neg.new
    end
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
  record Many, pattern : Pattern, envs : Array(Term::Dict) do
    def ones(& : One ->)
      envs.each { |env| yield One.new(pattern, env) }
    end
  end

  # Negative response.
  record Neg
end

# An object capable of parsing pattern terms into `Pattern`s (a thin wrapper
# around `Term::M1::Operator`) and organizing them for efficient response
# to matchees.
class PatternSet
  # :nodoc:
  def initialize(@headed : Hash(Term, Slice(Pattern)), @headless : Slice(Pattern))
  end

  # Constructs a pattern set by extracting patterns from *base* using *selector*.
  #
  # Considers only matches of *selector* that contain a capture named `pattern`.
  # The contents of this capture are treated as a pattern and added to the pattern set.
  #
  # Yields normal `pattern` (see `Term::M1.normal`), followed by match env of
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
  # pset = PatternSet.select(ML.parse1(%[(rule pattern_ body_)]), base) do |normp, env|
  #   # Do something with env[:body]
  #   # ...
  #
  #   true # E.g. body is valid
  # end
  # ```
  def self.select(selector : Term, base : Term, & : Term, Term::Dict -> Bool?) : PatternSet
    seen = Set(Term).new

    headed = {} of Term => Array(Int32)
    headless = [] of Int32

    patterns = [] of Pattern
    specificities = [] of Term::M1::Specificity

    base.each_item_unordered do |item|
      envs = Term.matches(selector, item)
      envs.each do |env|
        next unless pattern = env[:pattern]?
        next unless seen.add?(pattern)

        index = seen.size - 1

        normp = Term::M1.normal(pattern)

        specificity = Term::M1.specificity(normp, toplevel: true)
        specificities << specificity

        operator = Term::M1.operator(normp, normalize: false)

        pattern = Pattern.new(index.to_u32, operator)
        next unless yield normp, env

        patterns << pattern

        if head = Term::M1.head?(normp)
          neighbors = headed.put_if_absent(head) { [] of Int32 }
          neighbors << index
        else
          headless << index
        end
      end
    end

    # Now that we have everything neatly organized, sort headed and headless
    # patterns by specificity, descending.
    headless.sort! { |a, b| specificities.unsafe_fetch(b) <=> specificities.unsafe_fetch(a) }
    headed.each do |_, neighbors|
      neighbors.sort! { |a, b| specificities.unsafe_fetch(b) <=> specificities.unsafe_fetch(a) }
    end

    oheaded = headed.transform_values do |indices|
      indices.to_readonly_slice.map(read_only: true) { |index| patterns[index] }
    end

    oheadless = headless.to_readonly_slice.map(read_only: true) { |index| patterns[index] }

    new(oheaded, oheadless)
  end

  # Block-less version of `select`.
  def self.select(selector : Term, base : Term) : PatternSet
    self.select(selector, base) { true }
  end

  private def response(neighbors : Slice(Pattern), matchee : Term) : Pr::Any
    neighbors.leftmost?(&.response(matchee).as?(Pr::Pos)) || Pr::Neg.new
  end

  private def responses(neighbors : Slice(Pattern), matchee : Term) : Array(Pr::Pos)
    neighbors.compact_map(&.response(matchee).as?(Pr::Pos))
  end

  # Returns the first response of this pattern set to *matchee*. If none, returns
  # a negative response.
  def response(matchee : Term) : Pr::Any
    matchee.as_d?
      .try { |dict| dict.items.first? }
      .try { |head| @headed[head]? }
      .try { |neighbors| response(neighbors, matchee).as?(Pr::Pos) }
      .orelse { response(@headless, matchee) }
  end

  # Returns an array of all positive responses of this pattern set to *matchee*.
  def responses(matchee : Term) : Array(Pr::Pos)
    responses = [] of Pr::Pos

    if (dict = matchee.as_d?) && (head = dict.items.first?) && (neighbors = @headed[head]?)
      responses.concat(responses(neighbors, matchee))
    end

    responses.concat(responses(@headless, matchee))
  end
end

class ::Ww::Term::Dict
  # Some day this will be cached in the tree; right now, it isn't.
  #
  # Recurses into both keys and values (the former to support dict sets).
  #
  # Counts itself too (smallest possible value is 1).
  def maxdepth : UInt32
    maxdepth = 1u32

    each_entry do |k, v|
      maxdepth = {maxdepth, k.maxdepth, v.maxdepth}.max
    end

    maxdepth
  end

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
  def population
    ee.sum(Population.zero) { |_, v| v }
  end

  # Lets the block replace items in the given *range* with zero or more items
  # by appending to the commit. Returns the modified copy of `self`.
  def replace(range : Range(Term::Num, Term::Num), & : Term::Dict::Commit ->) : Term::Dict
    pairs.transaction do |commit|
      # Copy before
      (Term[0]...range.begin).each do |index|
        commit.append(self[index])
      end

      yield commit

      # Copy after
      (range.end...items.size).each do |index|
        commit.append(self[index])
      end
    end
  end

  def replace(index : Term::Num, &)
    replace(index...index + 1) { |commit| yield commit }
  end
end
