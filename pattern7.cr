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
# │ %new                     │   ~   │   ~     │   ~   │            │    ~     │       │   ~
# └──────────────────────────┴───────┴─────────┴───────┴────────────┴──────────┴───────┘
# + confident
# ~ will do
# · not needed

# TODO: we're pure so our hash must be deterministic, regardless of threats! Use fnv1a or something similar & fast
#       as a hashing algorithm for terms! E.g. the order of (keys (x: 1 y: 2)) must be the same across all machines&runs.

# --- After the above & tests are in place:

# - At this point we should be able to implement %string stuff. It includes some advanced
#   keypath communication & caching so that must be implemented along the way.
# - At this point we should be able to detect and compile *recursively simple patterns*.
#
# Recursively simple patterns (determined at compile-time) should (?) be compiled into
# a simpler operator hierarchy (e.g. Simple or something like that) that skips feedback etc.
# Simple patterns should be optimized down to nanoseconds. An example of a simple
# pattern is (+ a_number b_number). It should match in perhaps ~30ns + 2 * dict with().

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
# (context (_* ⏏s) `n←(+ a_number b_number)`)
#   <=> { s: (add lhs: →a rhs: →b), (n): ()}
#
# ;; ^^^ This one would require some sort of caching because it's all DfsFirst's,
# ;;     and they're expensive! We need to remember where we last stopped for all
# ;;     First's along with some identity, to be able to restore from where we've
# ;;     stopped. Otherwise all of this is going to explode in complexity (the O one).
#
# Note that we remove n, this may not be always appropriate! Instead you can mark:
#
# (context (_* ⏏s) `(+ a_number b_number)` ¦ _ -processed-adds)
#   <=>  { s: (add lhs: #a rhs: #b)} ;; Runs on loci
#   <=>. { processed-adds: true} ;; Runs on the resulting context
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

require "./src/wirewright"

include Ww

module Search::Result
  include Enumerable(Term)

  alias Any = Item | Pair | ItemStrip

  record Item, term : Term, backpath : Backpath::Appender? do
    include Result

    def each(& : Term ->) : Nil
      yield term
    end

    def sequence
      ItemStrip.new(Term[{term}].items, backpath)
    end
  end

  record Pair, k : Term, v : Term, backpath : {Backpath::Appender, Backpath::Appender}? do
    include Result

    def each(& : Term ->) : Nil
      yield k
      yield v
    end
  end

  record ItemStrip, view : Term::Dict::ItemsView, backpath : Backpath::Appender? do
    include Result

    def each(& : Term ->) : Nil
      view.each { |item| yield item }
    end

    def [](index : Int)
      view[index]
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

  def self.visit(dict, part : Part, *, backpath = nil, &)
    case part
    in .items_ordered?
      dict.items.each_with_index do |item, index|
        yield item, backpath.try &.update_value(index)
      end
    in .items_unordered?
      dict.each_item_with_index do |item, index|
        yield item, backpath.try &.update_value(index)
      end
    in .keys?
      dict.each_entry { |k, _| yield k, backpath.try &.update_key(k) }
    in .values?
      dict.each_entry do |key, value|
        yield value, backpath.try(&.update_value(key))
      end
    in .pair_values?
      dict.pairspart.each_entry do |key, value|
        yield value, backpath.try(&.update_value(key))
      end
    end
  end

  def self.traverse(term : Term, spec : Spec::Scan, *, backpath backpath0 = nil, &fn : Result::Any -> Response) : Nil
    return unless dict = term.as_d?

    feed = dict.items
    index = 0

    while spec.stride <= feed.size
      window = feed.begin.grow(spec.stride)
      item = Result::ItemStrip.new(window, backpath: backpath0 ? backpath0.update_value(index) : nil)

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

  private def self.dfs?(dict, spec, backpath0, fn) : Bool?
    visit(dict, spec.part, backpath: backpath0) do |value, backpath1|
      item = Result::Item.new(value, backpath1)

      case fn.call(item)
      in Accept.class
      in Reject.class
      in Stop.class
        return true
      end

      next if spec.maxdepth == 1
      next unless child = value.as_d?

      return true if dfs?(child, Spec.deeper(spec), backpath1, fn)
    end
  end

  def self.traverse(term : Term, spec : Spec::Dfs, *, backpath backpath0 = nil, &fn : Result::Any -> Response) : Nil
    if spec.depth0
      item = Result::Item.new(term, backpath: backpath0)

      case fn.call(item)
      in Accept.class, Reject.class
      in Stop.class
        return
      end
    end

    return unless dict = term.as_d?

    dfs?(dict, spec, backpath0, fn)
  end

  private def self.iddfs1(dict, spec, backpath0, fn, depth)
    if depth.zero?
      visit(dict, spec.part, backpath: backpath0) do |term, backpath1|
        item = Result::Item.new(term, backpath1)

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

    visit(dict, spec.part, backpath: backpath0) do |term, backpath1|
      total += 1

      unless child = term.as_d?
        bottom += 1
        next
      end

      case iddfs1(child, spec, backpath1, fn, depth - 1)
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

  private def self.iddfs0(dict, spec, backpath0, fn)
    maxdepth = spec.maxdepth.zero? ? nil : spec.maxdepth

    (0...maxdepth).each do |depth|
      case iddfs1(dict, spec, backpath0, fn, depth)
      when :bot, :stop
        break
      when :next
      else
        unreachable
      end
    end
  end

  def self.traverse(term : Term, spec : Spec::Bfs, *, backpath backpath0 = nil, &fn : Result::Any -> Response) : Nil
    if spec.depth0
      item = Result::Item.new(term, backpath: backpath0)

      case fn.call(item)
      in Accept.class, Reject.class
      in Stop.class
        return
      end
    end

    return unless dict = term.as_d?

    iddfs0(dict, spec, backpath0, fn)
  end

  def self.traverse(term : Term, spec : Spec::Entries, *, backpath backpath0 = nil, &fn : Result::Any -> Response) : Nil
    return unless dict = term.as_d?

    dict.each_entry do |key, value|
      item = Result::Pair.new(key, value, backpath0 ? {backpath0.update_key(key), backpath0.update_value(key)} : nil)

      case fn.call(item)
      in Accept.class, Reject.class
      in Stop.class
        return
      end
    end
  end
end

# TODO: the names of operators should be nouns. Currently some of them are and others
#   are not, fix that. In fact, Operator should probably be renamed to Subject or something
#   like that. Not sure how large of a refactor that is, and how much point is there in it.
module ::Ww::M1::Operator
  alias Any = Pass | Num | Sym | SymBlank | SymNonblank | Boolean | Dict | Itemsonly | Pairsonly | SketchSubset | Bounds | BoundsGuard | MaxDepth | DictGuard | Literal | Capture | CaptureItemsonly | ItemSeq | ItemFirst | ItemLast | SingularSeq | Partition | Edge | LiteralChoices | SourceChoice | ValueLiteral | Keypool | Span | Tally | Type | Bin | Both | Not | Layer | ScanFirst | ScanSource | ScanAll | ScanAllIsolated | DfsFirst | DfsSource | DfsAllIsolated | DfsAll | BfsFirst | BfsAllIsolated | BfsAll | Value | NegativeValue | NegativeValueKeypath | EntriesFirst | EntriesSource | EntriesAllIsolated | EntriesAll | Str | New | KeypathCapture

  alias Bin = Add | Sub | Mul | Div | Idiv | Mod | Pow | Map

  alias First = ScanFirst | DfsFirst | BfsFirst | EntriesFirst
  alias Source = DfsSource | ScanSource | EntriesSource
  alias AllIsolated = ScanAllIsolated | DfsAllIsolated | BfsAllIsolated | EntriesAllIsolated
  alias All = ScanAll | DfsAll | BfsAll | EntriesAll

  defcase ValueLiteral, key : Term, successor : Any

  defcase Layer, below : Any, side : Array(Entry::Any)

  alias Scan = ScanFirst | ScanSource | ScanAllIsolated | ScanAll

  defcase ScanFirst, needle : Slice(Any)
  defcase ScanSource, needle : Slice(Any)
  defcase ScanAllIsolated, successor : Any, needle : Slice(Any), min : UInt8, max : UInt8
  defcase ScanAll, successor : Any, needle : Slice(Any), selector : Set(Term), exterior : Set(Term), min : UInt8, max : UInt8

  defcase Value, capture : Term, tail : Any
  defcase NegativeValue, capture : Term
  defcase NegativeValueKeypath, capture : Term, name : Term

  alias Dfs = DfsFirst | DfsSource | DfsAllIsolated | DfsAll

  defcase DfsFirst, needle : Any, part : Search::Part, depth0 : Bool
  defcase DfsSource, needle : Any, part : Search::Part, depth0 : Bool
  defcase DfsAllIsolated, successor : Any, needle : Any, part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool
  defcase DfsAll, successor : Any, needle : Any, selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool

  alias Bfs = BfsFirst | BfsAllIsolated | BfsAll

  defcase BfsFirst, needle : Any, part : Search::Part, depth0 : Bool
  defcase BfsAllIsolated, successor : Any, needle : Any, part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool
  defcase BfsAll, successor : Any, needle : Any, selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool

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

  defcase EntriesAllIsolated, successor : Any, kop : Any, vop : Any, min : UInt8, max : UInt8 do
    def needle
      [kop, vop]
    end
  end

  defcase EntriesAll, successor : Any, kop : Any, vop : Any, exterior : Set(Term), selector : Set(Term), min : UInt8, max : UInt8 do
    def needle
      [kop, vop]
    end
  end

  defcase New, subjects : Array(Term), pattern : Term
  defcase KeypathCapture, capture : Term
end

alias Magnitude = Float32

module ::Ww::M1::Operator::Item
  alias Any = Singular | Slot | Plural | Group | Gap | Optional | Many | Past

  record Singular, tail : Operator::Any
  record Slot, capture : Term

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

  record Group, capture : Term, children : Array(Any)
  record Gap, measurer : Operator::Any, frac : UInt32, strategy : ExpandStrategy
  record Optional, default : Term, tail : Operator::Any
  record Many, capture : Term, children : Array(Any), interior : Set(Term), min : UInt8, max : UInt8
  record Past, children : Array(Any), min : UInt8, max : UInt8, greedy : Bool
end

module ::Ww::M1::Operator::Env
  def self.append(envs : Array(Term::Dict), feedback : Fb::MatchOne)
    envs << feedback.env
  end

  def self.append(envs : Array(Term::Dict), feedback : Fb::MatchMany)
    envs.concat(feedback.envs)
  end

  def self.append(envs : Array(Term::Dict), feedback : Fb::Mismatch)
  end

  def self.feedback(envs : Indexable(Term::Dict), fallback : Term::Dict, *, more : Bool = false) : Fb::Any
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

  def self.captures(envs : Array(Term::Dict), selector : Set(Term)) : Term::Dict
    Term::Dict.build do |captures|
      envs.each do |env|
        captures << (pluck?(env, selector) || next)
      end
    end
  end

  def self.domain(envs : Array(Term::Dict), capture : Term) : Term::Dict
    Term::Dict.build do |domain|
      envs.each do |env|
        next unless value = env[capture]?

        domain.with(value, true)
      end
    end
  end
end

module ::Ww::M1::Operator::Fb
  alias Any = Response | Interrupt
  alias Response = Match | Mismatch
  alias Match = MatchOne | MatchMany
  alias Interrupt = RequestBackpath | ProbeTrue

  record MatchOne, env : Term::Dict, more : Bool = false do
    def envs
      {env}
    end
  end

  record MatchMany, envs : Array(Term::Dict)
  record Mismatch, env : Term::Dict

  record RequestBackpath
  record ProbeTrue

  def self.sum(a : Mismatch, b : Mismatch)
    a
  end

  def self.sum(a : Match, b : Mismatch)
    a
  end

  def self.sum(a : Mismatch, b : Match)
    b
  end

  def self.sum(a : MatchOne, b : MatchOne)
    Fb::MatchMany.new([a.env, b.env])
  end

  def self.sum(a : MatchOne, b : MatchMany)
    Fb::MatchMany.new([a.env].concat(b.envs))
  end

  def self.sum(a : MatchMany, b : MatchOne)
    sum(b, a)
  end

  def self.sum(a : MatchMany, b : MatchMany)
    Fb::MatchMany.new(a.envs + b.envs)
  end
end

module ::Ww::Backpath
end

class ::Ww::Backpath::Appender
  # :nodoc:
  def initialize(@preds : Term::Dict, @tip : Word::None | Word::Terminal | Word::Nonterminal)
  end

  # :nodoc:
  EMPTY = Appender.new(preds: Term[], tip: Word::None.new)

  # Constructs an empty backpath appender.
  def self.new : Appender
    EMPTY
  end

  private def push(tip1 : Word::Some) : Appender
    Appender.new(backpath, tip1)
  end

  private def replace(tip1 : Word::Some) : Appender
    Appender.new(@preds, tip1)
  end

  # The returned appender will update the key *term* of an existing entry. Its value
  # is preserved. If the updated key collides with some existing key, the updated
  # key's value wins.
  def update_key(term) : Appender
    case @tip
    in Word::None, Word::Nonterminal
      push Word::UpdateKey.new(Term.of(term))
    in Word::Terminal
      raise KeypathError.new
    end
  end

  # The returned appender will update the value of an existing entry with the given *key*.
  def update_value(key) : Appender
    case @tip
    in Word::None, Word::Nonterminal
      push Word::UpdateValue.new(Term.of(key))
    in Word::Terminal
      raise KeypathError.new
    end
  end

  # The returned appender will modify entries left after removing keys from *ee*. Each
  # element of *ee* is converted to a Term using the block.
  def delete_keys(ee : Enumerable(T), & : T -> Term) : Appender forall T
    case @tip
    in Word::None, Word::Nonterminal
      keys = Term[].transaction do |commit|
        ee.each { |object| commit << yield object }
      end

      push Word::Delete.new(keys)
    in Word::Terminal
      raise KeypathError.new
    end
  end

  # Block-less variant of `delete_keys`.
  def delete_keys(keys : Enumerable(Term)) : Appender
    delete_keys(keys, &.itself)
  end

  # Block-less variant of `delete_keys`.
  def delete_keys(keys : Term::Dict) : Appender
    delete_keys(keys.items)
  end

  # Converts an `update` targeting one item into an update targeting a range of items.
  # The size of the range is set by *size*. The returned appender is a **terminal** one:
  # appending anything to it will fail.
  def span(size, *, ord = 0u32) : Appender
    case tip = @tip
    when Word::UpdateValue
      b = tip.key.to(Int32)
      e = b + size
      replace Word::Range.new(b, e, ord)
    else
      raise KeypathError.new
    end
  end

  # Creates a pair with the given *key*. The returned appender is a **terminal** one:
  # there is nothing to modify with it, since the value to be modified in fact does
  # not exist.
  def create_pair(key) : Appender
    case @tip
    in Word::None, Word::Nonterminal
      push Word::CreateLeaf.new(Term.of(key))
    in Word::Terminal
      raise KeypathError.new
    end
  end

  # Creates a pair with the given *key* and *value*. The returned appender will
  # be modifying *value*.
  def create_pair(key, *, value) : Appender
    case @tip
    in Word::None, Word::Nonterminal
      push Word::Create.new(Term.of(key), Term.of(value), Term.of(value))
    in Word::Terminal
      raise KeypathError.new
    end
  end

  # Converts an `update_value` of a single item into an insert of *value* before
  # that item. The returned appender will be modifying *value*.
  def insert_item(value, *, ord = 0) : Appender
    case tip = @tip
    when Word::UpdateValue
      replace Word::Insert.new(tip.key.as_n, ord, Term.of(value))
    else
      raise KeypathError.new
    end
  end

  # Shifts a numeric `update_value` *n* items forward.
  def forward(n = 1) : Appender
    case tip = @tip
    when Word::UpdateValue
      replace Word::UpdateValue.new(Term.of(tip.key + n))
    else
      raise KeypathError.new
    end
  end

  # Shifts a numeric `update_value` *n* items backward.
  def backward(n = 1) : Appender
    forward(-n)
  end

  # Returns the backpath dict that this appender constructed so far.
  def backpath : Term::Dict
    case tip = @tip
    in Word::None then @preds
    in Word::Some then @preds.transaction &.concat(Word.terms(tip))
    end
  end
end

module ::Ww::M1::Operator
  def match(behind0, op : ValueLiteral, matchee : Term, ahead0)
    unless (dict = matchee.as_d?) && (value = dict[op.key]?)
      return Fb::Mismatch.new(behind0.env)
    end

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))

    match(behind0.backpath(&.update_value(op.key)), op.successor, value, ahead1)
  end

  def match(behind0, op : Layer, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    # Split the dictionary into a "selection" and "residue". We'll show "selection"
    # to operators on the side of the layer; and "residue" to the operator below it.
    #
    # Assume "residue" is larger than side.
    #
    # Note: but in fact, entry patterns do not care about the dict that we give to them;
    # they only care about their respective key in that dict. So we do not need to make a
    # "selection" dict; we just give the matchee dict to entry patterns.
    case op.below
    when Pass, Pairsonly
      # Fast path. The first one especially is very frequently met in practice.
      residue = Term[]
    else
      residue = dict.transaction do |residue|
        op.side.each do |entry|
          next unless v = dict[entry.key]?
          residue.without(entry.key)
        end
      end
    end

    # Note: entries can depend on each other here and on residue, so instead of simply
    # iterating over the 'side'+'below' of the layer, we'll instead "string" the entries
    # after 'below', one entry after another in a chain of Aheads.
    #
    # The exact order doesn't *really* matter here since we're a solver.
    ahead1 = Ahead::EntrySeq.new(matchee, op.side, 0, Ahead.stackptr(ahead0))
    ahead2 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead1))

    match(behind0.backpath(&.delete_keys(op.side, &.key)), op.below, Term.of(residue), ahead2)
  end

  def match(behind0, op : First, matchee : Term, ahead0)
    memo = Fb::Mismatch.new(behind0.env)

    Search.traverse(matchee, spec: search_spec(op), backpath: behind0.backpath?) do |item|
      case memo = Operator.match(behind0, op.needle, item, ahead0)
      in Fb::Match, Fb::Interrupt
        Search::Stop
      in Fb::Mismatch
        Search::Reject
      end
    end

    memo
  end

  def match(behind0, op : Source, matchee : Term, ahead0)
    envs = [] of Term::Dict
    interrupt = nil

    Search.traverse(matchee, spec: search_spec(op), backpath: behind0.backpath?) do |item|
      case fb = Operator.match(behind0, op.needle, item, ahead0)
      in Fb::Match
        Env.append(envs, fb)

        Search::Accept
      in Fb::Mismatch
        Search::Reject
      in Fb::Interrupt
        interrupt = fb

        Search::Stop
      end
    end

    if interrupt_ = interrupt
      return interrupt_
    end

    Env.feedback(envs, fallback: behind0.env, more: true)
  end

  def match(behind0, op : AllIsolated, matchee : Term, ahead0)
    kp0 = behind0.backpath?

    behind1 = behind0

    captures = Term::Dict.build do |captures|
      interrupt = nil

      Search.traverse(matchee, spec: search_spec(op), backpath: kp0) do |item|
        case fb = Operator.match(behind0, op.needle, item, Ahead::MatchOne.new)
        in Fb::Match
          fb.envs.each do |env|
            behind1 = behind1.import_backpaths(env)

            captures.append(env.without(:"(backpaths)"))
          end

          Search::Accept
        in Fb::Mismatch
          Search::Reject
        in Fb::Interrupt
          interrupt = fb

          Search::Stop
        end
      end

      if interrupt_ = interrupt
        return interrupt_
      end
    end

    if captures.size < op.min || captures.size > op.max > 0
      return Fb::Mismatch.new(behind1.env)
    end

    ahead1 = Ahead::Goto.new(kp0, Ahead.stackptr(ahead0))

    match(behind1.backpathless, op.successor, Term.of(captures), ahead1)
  end

  def match(behind0, op : All, matchee : Term, ahead0)
    envs = [] of Term::Dict
    interrupt = nil

    behind1 = behind0

    Search.traverse(matchee, spec: search_spec(op), backpath: behind0.backpath?) do |item|
      case fb = Operator.match(behind0, op.needle, item, ahead0)
      in Fb::Match
        fb.envs.each do |env|
          behind1 = behind1.import_backpaths(env)
          envs << env
        end

        Search::Accept
      in Fb::Mismatch
        Search::Reject
      in Fb::Interrupt
        interrupt = fb

        Search::Stop
      end
    end

    if interrupt_ = interrupt
      return interrupt_
    end

    # Each feedback environment gives us candidate values for all exterior captures.
    # Thus by looking at all feedback environments at once we can determine (or reduce)
    # the domains of each exterior capture.
    op.exterior.each do |capture|
      domain1 = Env.domain(envs, capture)

      # Do not restrict domain if min=0, otherwise we'd have restricted it
      # to nevermatch (the empty domain).
      next if domain1.empty? && op.min.zero?

      behind1 = behind1.one_of(capture, domain1)
    end

    captures = Env.captures(envs, selector: op.selector)

    fb = Ahead.tr(behind1, ahead0)

    case fb
    in Fb::Match
      # Prune all captures that are inconsistent with exterior assignments
      # in each env.
      consistent = [] of Term::Dict

      fb.envs.each do |exterior|
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

        Env.append(consistent, feedback(exterior, op.successor, Term.of(pruned)))
      end

      Env.feedback(consistent, fallback: behind1.env)
    in Fb::Mismatch, Fb::Interrupt
      fb
    end
  end

  def match(behind0, op : Value, matchee : Term, ahead0)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(behind0.env)
    end

    if dict.empty?
      return Fb::Mismatch.new(behind0.env)
    end

    candidates = Set(Term).new

    if key = behind0[op.capture]?
      candidates << key
    else
      # If we do not know the value yet it might be the case that it can be learned
      # from the future. We declare its domain to be that of all keys from the matchee
      # dict, and run ahead without the value known, hoping to learn it.
      case fb = Ahead.tr(behind0.one_of(op.capture, dict), ahead0)
      in Fb::MatchOne, Fb::Mismatch
        if key = fb.env[op.capture]?
          candidates << key
        end
      in Fb::MatchMany
        fb.envs.each do |fenv|
          next unless key = fenv[op.capture]?

          candidates << key
        end
      in Fb::Interrupt
        return fb
      end
    end

    if candidates.empty?
      return Fb::Mismatch.new(behind0.env)
    end

    envs = [] of Term::Dict

    candidates.each do |key|
      next unless value = dict[key]?

      kp0 = behind0.backpath?

      behind1 = behind0
        .mount(op.capture, &.update_key(key))
        .assign(op.capture, key)
        .backpath(&.update_value(key))

      fb = match(behind1, op.tail, value, Ahead::Goto.new(kp0, Ahead.stackptr(ahead0)))
      unless fb.is_a?(Fb::Response)
        return fb
      end

      Env.append(envs, feedback: fb)
    end

    Env.feedback(envs, fallback: behind0.env)
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
        behind1 = behind0.mount(op.name, &.create_pair(key))
      end

      return Ahead.tr(behind1, ahead0)
    end

    # Restrict the future's choices of candidates for the captures. Make
    # sure they're NOT from one of dict's keys.
    behind1 = behind0.not(op.capture, dict)

    # Now ask the future for candidates.
    case fb = Ahead.tr(behind1, ahead0)
    in Fb::MatchOne, Fb::Mismatch
      unless key = fb.env[op.capture]?
        return Fb::Mismatch.new(behind0.env)
      end
      candidates = Set{key}
    in Fb::MatchMany
      candidates = fb.envs.to_compact_set { |env| env[op.capture]? }
    in Fb::Interrupt
      return fb
    end

    # Now that we have a nonempty set of candidates for keys, we can pick
    # each one and pass them to the future again.
    envs = [] of Term::Dict

    candidates.each do |key|
      case op
      in NegativeValue
        behind2 = behind1
      in NegativeValueKeypath
        behind2 = behind1.mount(op.name, &.create_pair(key))
      end

      fb = Ahead.tr(behind2, ahead0)
      unless fb.is_a?(Fb::Response)
        return fb
      end

      Env.append(envs, feedback: fb)
    end

    Env.feedback(envs, fallback: behind0.env)
  end

  # TODO: cache on {op.pattern, subt} level when that's going to be cheap enough
  def match(behind0, op : New, matchee : Term, ahead0)
    # If we already know all the subjects, this is the best case and an immediate
    # fast path toward instantiation.
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
    case fb = Ahead.tr(behind0, ahead0)
    in Fb::Match
      learned = fb.envs
    in Fb::Mismatch
      learned = {fb.env}
    in Fb::Interrupt
      return fb
    end

    envs = [] of Term::Dict

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

  def match(behind0, op : KeypathCapture, matchee : Term, ahead0)
    unless backpath = behind0.backpath?
      # We're not running in backpath mode. Send a back-message all the way up
      # the call stack to where the match was initiated; ask them to rematch
      # with backpath mode enabled.
      #
      # I don't think there is any optimization we can do here regarding
      # the preservation of progress. During progress backpaths are modified,
      # so we'll have to retry anyway most of the cases.
      return Fb::RequestBackpath.new
    end

    kpdict = Term.of(backpath.backpath)

    unless behind1 = behind0.propose?(op.capture, kpdict)
      return Fb::Mismatch.new(behind0.env.with(op.capture, kpdict))
    end

    Ahead.tr(behind1, ahead0)
  end
end

module ::Ww::M1::Operator
  struct Behind
    getter? probe : Bool

    def initialize(
      @captures = Term[],
      @domains = Term[],
      @antidomains = Term[],
      @backpath : Backpath::Appender? = nil,
      *,
      @probe : Bool = false,
    )
    end

    private def_change

    def env : Term::Dict
      @captures
    end

    def []?(k : Term)
      @captures[k]?
    end

    def propose?(k : Term, v1 : Term) : Behind?
      if domain = @domains[k]?
        return unless v1.in?(domain)
      end

      if antidomain = @antidomains[k]?
        return if v1.in?(antidomain)
      end

      if (v0 = @captures[k]?) && v0 != v1
        return
      end

      change(captures: @captures.with(k, v1))
    end

    def assign(k, v)
      change(captures: @captures.with(k, v))
    end

    def mount(capture : Term, backpath : Term::Dict)
      backpaths0 = @captures[:"(backpaths)"]? || Term[]
      backpaths1 = backpaths0.morph({capture, backpath, true})

      change(captures: @captures.with(:"(backpaths)", backpaths1))
    end

    def mount(capture : Term, kpb : Backpath::Appender)
      mount(capture, kpb.backpath)
    end

    def mount(capture : Term)
      @backpath.try { |kpb| mount(capture, kpb) } || self
    end

    def mount(capture : Term, & : Backpath::Appender -> Backpath::Appender)
      @backpath.try { |kpb| mount(capture, yield kpb) } || self
    end

    def import_backpaths(env : Term::Dict)
      behind1 = self

      kpsrc = env[:"(backpaths)"]? || Term[]
      kpsrc.each_entry do |capture, kpset|
        kpset.each_entry { |kp, _| behind1 = behind1.mount(capture, kp.as_d) }
      end

      behind1
    end

    def backpath? : Backpath::Appender?
      @backpath
    end

    def backpath(& : Backpath::Appender -> Backpath::Appender)
      return self unless kp = @backpath

      change(backpath: yield kp)
    end

    def goto(dst : Backpath::Appender?)
      change(backpath: dst)
    end

    def value(*, key)
      backpath &.update_value(key)
    end

    def backpathless
      change(backpath: nil)
    end

    # Domain restriction: *k* must be one of *vs* (the latter is treated as a dict set).
    def one_of(k, vs) : Behind
      if domain = @domains[k]?
        change(domains: @domains.with(k, domain.xsect(vs)))
      else
        change(domains: @domains.with(k, vs))
      end
    end

    # Domain restriction: *k* must **not** be one of *vs* (the latter is treated as a dict set).
    def not(k, vs) : Behind
      if antidomain = @antidomains[k]?
        change(antidomains: @antidomains.with(k, antidomain | vs))
      else
        change(antidomains: @antidomains.with(k, vs))
      end
    end

    def partition(selector)
      lcaptures = @captures &- selector
      ldomains = @domains &- selector
      lantidomains = @antidomains &- selector

      rcaptures = @captures.pluck(selector)
      rdomains = @domains.pluck(selector)
      rantidomains = @antidomains.pluck(selector)

      {change(captures: lcaptures, domains: ldomains, antidomains: lantidomains),
       change(captures: rcaptures, domains: rdomains, antidomains: rantidomains)}
    end
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

  enum FeedbackMode : UInt8
    Normal
    Probe
    Backpath
  end

  # :nodoc:
  def feedback0(env, op, matchee, mode : FeedbackMode) : Fb::Any
    case mode
    in .normal?
      behind0 = Behind.new(env)
    in .probe?
      behind0 = Behind.new(env, probe: true)
    in .backpath?
      behind0 = Behind.new(env, backpath: Backpath::Appender.new)
    end

    match(behind0, op, matchee, Ahead::MatchEndpoint.new)
  end

  def feedback(env : Term::Dict, op : Any, matchee : Term, *, backpaths : Bool = false) : Fb::Response
    case fb = feedback0(env, op, matchee, mode: backpaths ? FeedbackMode::Backpath : FeedbackMode::Normal)
    in Fb::Response
      fb
    in Fb::RequestBackpath
      if backpaths
        return Fb::Mismatch.new(env)
      end

      feedback(env, op, matchee, backpaths: true)
    in Fb::ProbeTrue
      raise "BUG: unexpected Fb::ProbeTrue response in normal/backpath mode"
    end
  end

  def match?(env : Term::Dict, op : Any, matchee : Term, **kwargs) : Term::Dict?
    case fb = feedback(env, op, matchee, **kwargs)
    in Fb::MatchOne  then fb.env
    in Fb::MatchMany then fb.envs[0]
    in Fb::Mismatch  then nil
    end
  end

  def matches(env : Term::Dict, op : Any, matchee : Term, **kwargs) : Array(Term::Dict)
    case fb = feedback(env, op, matchee, **kwargs)
    in Fb::MatchOne  then [fb.env]
    in Fb::MatchMany then fb.envs
    in Fb::Mismatch  then [] of Term::Dict
    end
  end

  def probe?(env : Term::Dict, op : Any, matchee : Term) : Bool
    case feedback0(env, op, matchee, mode: FeedbackMode::Probe)
    in Fb::Match, Fb::ProbeTrue
      true
    in Fb::Mismatch
      false
    in Fb::RequestBackpath
      # If the pattern requests backpath, we switch to the slower feedback() version.
      # Gains from using the probe mode are a speck compared to losses from backpath mode.
      feedback(env, op, matchee, backpaths: true).is_a?(Fb::Match)
    end
  end
end

module ::Ww::M1::Operator
  extend self

  def match(behind0, op : Any, cell : Search::Result::Item, ahead)
    kp0 = behind0.backpath?
    kp1 = cell.backpath
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

  def match(behind0, ops : Indexable(Any), cell : Search::Result::Pair, ahead0)
    unless ops.size == 2
      return Fb::Mismatch.new(behind0.env)
    end

    if row = cell.backpath
      kkp, vkp = row
    else
      kkp = vkp = nil
    end

    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))
    ahead2 = Ahead::Match.new(ops[1], cell.v, Ahead.stackptr(ahead1))
    ahead3 = Ahead::Goto.new(vkp, Ahead.stackptr(ahead2))
    ahead4 = Ahead::Match.new(ops[0], cell.k, Ahead.stackptr(ahead3))
    ahead5 = Ahead::Goto.new(kkp, Ahead.stackptr(ahead4))

    Ahead.tr(behind0, ahead5)
  end

  def match(behind0, ops : Slice(Any), matchees : Search::Result::ItemStrip, ahead0)
    ahead1 = Ahead::Goto.new(behind0.backpath?, Ahead.stackptr(ahead0))
    ahead2 = Ahead::ItemZip.new(ops, matchees.view, 0, 0, +1, Ahead.stackptr(ahead1))

    Ahead.tr(behind0.goto(matchees.backpath), ahead2)
  end

  def match(env, ops : Array(Any), matchee, ahead)
    match(env, ops.to_readonly_slice, matchee, ahead)
  end
end

module ::Ww::M1::Operator
end

module ::Ww::M1::Operator::Item
  def self.match(progress0, item : Singular, matchees, ahead0)
    unless matchee = matchees.first?
      return Fb::Mismatch.new(progress0.env)
    end

    ahead1 = Ahead::Item::Advance.new(1, Ahead::Item.stackptr(ahead0))
    ahead2 = Ahead::ItemAdapter.new(progress0.ord, matchees, Ahead::Item.stackptr(ahead1))

    Operator.match(progress0.behind, item.tail, matchee, ahead2)
  end

  def self.match(progress0, item : Slot, matchees, ahead0)
    # size: 0 is understood by the backmap engine as a "pure insert".
    progress1 = progress0.after_span(item.capture, size: 0)

    ahead0.call(progress1, matchees)
  end
end

# TODO: we probably should rename `Ahead.tr` to `Ahead.bridge` since the latter is
# **just a bit** less cryptic.
module ::Ww::M1::Operator::Ahead
  alias Any = MatchOne | MatchEndpoint | Match | Forward | ItemZip | EntrySeq | ItemAdapter | Goto

  macro stackptr(var)
    begin
      %slot = {{var}}.as(::Ww::M1::Operator::Ahead::Any)
      pointerof(%slot)
    end
  end

  record MatchOne
  record MatchEndpoint
  record Match, op : Operator::Any, matchee : Term, successor : Ahead::Any*
  record Forward, delta : Int32, successor : Ahead::Any*

  record Goto, backpath : Backpath::Appender, successor : Ahead::Any* do
    def self.new(backpath : Nil, successor : Ahead::Any*)
      successor.value
    end
  end

  record ItemZip, ops : Slice(Operator::Any), matchees : MatcheeFeed, i : Int32, j : Int32, delta : Int8, successor : Ahead::Any*

  record EntrySeq, matchee : Term, entries : Array(Entry::Any), cursor : UInt32, successor : Ahead::Any* do
    def entry?
      entries[cursor]?
    end
  end

  record ItemAdapter, ord : UInt32, matchees : MatcheeFeed, successor : Item*

  def self.tr(behind0, node0 : MatchOne)
    Fb::MatchOne.new(behind0.env)
  end

  def self.tr(behind0, node0 : MatchEndpoint)
    if behind0.probe?
      return Fb::ProbeTrue.new
    end

    Fb::MatchOne.new(behind0.env)
  end

  def self.tr(behind0, node0 : Match)
    Operator.match(behind0, node0.op, node0.matchee, node0.successor.value)
  end

  def self.tr(behind0, node0 : Forward)
    tr(behind0.backpath(&.forward(node0.delta)), node0.successor)
  end

  def self.tr(behind0, node0 : Goto)
    tr(behind0.goto(node0.backpath), node0.successor)
  end

  def self.tr(behind0, node0 : ItemZip)
    i1 = node0.i + node0.delta
    j1 = node0.j + node0.delta

    if i1.in?(0...node0.ops.size) && j1.in?(0...node0.matchees.size)
      aheadptr = Ahead.stackptr(node0.copy_with(i: i1, j: j1))
    else
      aheadptr = node0.successor
    end

    ahead = Forward.new(node0.delta, aheadptr)

    Operator.match(behind0, node0.ops[node0.i], node0.matchees[node0.j], ahead)
  end

  def self.tr(behind0, node0 : EntrySeq)
    unless entry = node0.entry?
      return tr(behind0, node0.successor)
    end

    node1 = node0.copy_with(cursor: node0.cursor + 1)

    Entry.match(behind0, entry, node0.matchee, node1)
  end

  def self.tr(behind0, node0 : ItemAdapter)
    node0.successor.value.call(Operator::Item::Progress.new(node0.ord, behind0), node0.matchees)
  end

  def self.tr(behind0, node0 : Ahead::Any*)
    tr(behind0, node0.value)
  end

  module Item
    macro stackptr(var)
      begin
        %slot = {{var}}.as(::Ww::M1::Operator::Ahead::Item)
        pointerof(%slot)
      end
    end

    struct SequenceRest
      include Item

      def initialize(@ord : UInt32, @items : Slice(Operator::Item::Any), @ahead : Item*)
      end

      def call(progress, matchees)
        Operator::Item.sequence(progress.copy_with(ord: progress.ord + 1), @items[1..], matchees, @ahead.value)
      end
    end

    struct CaptureGroup
      include Item

      # - *ord0* is the ordinal at the start of the group.
      # - *matchees* is the matchees at the start of the group.
      # - *propose* specifies whether to propose the captured group to `Behind`. Otherwise,
      #   the group is simply mounted and not proposed.
      def initialize(@ord0 : UInt32, @matchees : MatcheeFeed, @capture : Term, @ahead : Item*, *, @propose : Bool)
      end

      def call(progress, matchees)
        group = @matchees.upto(matchees)

        behind1 = progress.behind

        if @propose
          unless behind1 = behind1.propose?(@capture, proposal = Term.of(group))
            return Fb::Mismatch.new(progress.env.with(@capture, proposal))
          end
        end

        behind1 = behind1.mount(@capture, &.backward(group.size).span(group.size, ord: progress.ord))

        @ahead.value.call(progress.copy_with(behind: behind1), matchees)
      end
    end

    struct Advance
      include Item

      def initialize(@n : Int32, @ahead : Item*)
      end

      def call(progress, matchees)
        @ahead.value.call(progress.copy_with(ord: progress.ord + @n, behind: progress.behind.backpath(&.forward(@n))), matchees.move(@n))
      end
    end

    struct ManyStep
      include Item

      def initialize(@feed0 : MatcheeFeed, @item : Operator::Item::Many, @ahead : Item*, @memo : Term::Dict)
      end

      def call(progress, matchees)
        if @feed0 == matchees
          # This continuation is run after the ahead check. This means ahead refuses to
          # consume feed. And we did not move while trying to consume feed. Thus this is
          # a hard mismatch.
          return Fb::Mismatch.new(progress.env)
        end

        pruned, capture = progress.behind.partition(@item.interior)

        Operator::Item.many(progress.copy_with(behind: pruned), @item, matchees, @ahead.value, @memo.append(capture.env))
      end
    end

    struct PastStep
      include Item

      def initialize(@feed0 : MatcheeFeed, @item : Operator::Item::Past, @memo : Int32, @ahead : Item*)
      end

      def call(progress, matchees)
        if @feed0 == matchees
          return Fb::Mismatch.new(progress.env)
        end

        if @item.greedy
          Operator::Item.past_greedy(progress, @item, matchees, @ahead.value, @memo + 1)
        else
          Operator::Item.past_lazy(progress, @item, matchees, @ahead.value, @memo + 1)
        end
      end
    end

    struct Rest
      include Item

      def initialize(@items : Slice(Operator::Item::Any), @ahead : Ahead::Any*)
      end

      def call(progress, matchees)
        Operator::Item.match(progress, @items[1..], matchees, @ahead.value)
      end
    end
  end
end

module ::Ww::M1::Operator
  alias MatcheeFeed = Term::Dict::ItemsView
end

module ::Ww::M1::Operator::Item
  record Progress, ord : UInt32, behind : Behind do
    def env : Term::Dict
      @behind.env
    end

    # Returns a `Progress` object after a span of the given *size* was matched.
    # The span is mounted at *capture* if in backpath mode.
    def after_span(capture : Term, size : Int) : Progress
      copy_with(ord: @ord + 1, behind: @behind.mount(capture, &.span(size, ord: @ord)))
    end
  end

  enum ExpandStrategy : UInt8
    Auto
    Sway
    Lazy
    Greedy
  end

  def self.expand(matchees : MatcheeFeed, pivot : Int, strategy : ExpandStrategy = :sway, & : MatcheeFeed, MatcheeFeed ->)
    case strategy
    in .auto?
      raise ArgumentError.new
    in .sway?
      prefix, suffix = matchees.begin.grow(pivot), matchees.move(pivot)
      yield prefix, suffix

      # We then sway like pivot - 1, pivot + 1, pivot - 2, pivot + 2, etc...
      (1..matchees.size).each do |offset|
        if pivot - offset >= 0
          prefix, suffix = matchees.begin.grow(pivot - offset), matchees.move(pivot - offset)
          yield prefix, suffix
        end

        if pivot + offset <= matchees.size
          prefix, suffix = matchees.begin.grow(pivot + offset), matchees.move(pivot + offset)
          yield prefix, suffix
        end
      end
    in .lazy?
      (0..matchees.size).each do |size|
        prefix, suffix = matchees.begin.grow(size), matchees.move(size)
        yield prefix, suffix
      end
    in .greedy?
      (0..matchees.size).reverse_each do |size|
        prefix, suffix = matchees.begin.grow(size), matchees.move(size)
        yield prefix, suffix
      end
    end
  end

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

  private def self.propose?(progress, item : Plural, prefix, suffix, ahead)
    candidate = progress.behind

    prefix.split(item.contenders.size, empty: item.min.zero?) do |subview, index|
      if capture = item.contenders[index]
        candidate = candidate.propose?(capture, Term.of(subview))
        return unless candidate
        candidate = candidate.mount(capture, &.span(subview.size, ord: progress.ord))
      end
      candidate = candidate.backpath(&.forward(subview.size))
    end

    case fb = ahead.call(progress.copy_with(ord: progress.ord + 1, behind: candidate), suffix)
    in Fb::Match, Fb::Interrupt
      fb
    in Fb::Mismatch
    end
  end

  def self.match(progress : Progress, item : Plural, matchees, ahead)
    pivot = (matchees.size / item.frac).ceil.to_i

    if item.strategy.auto?
      strategy = ExpandStrategy::Sway

      if !item.follower.none? && !item.type.any? && item.follower.any?
        strategy = ExpandStrategy::Greedy
      elsif !item.follower.none? && item.type.any? && !item.follower.any?
        # *Give* from the end of fraction to the follower if the follower is typed
        # and the end is a subtype of the follower's type.
        unless pivot < item.min || pivot > item.max > 0
          size0 = pivot

          while size0 > 0
            size1 = size0 - 1
            # Make sure our future size is OK with our own constraints & the follower
            # can accept it.
            break unless matchees[size1].type.subtype?(item.follower.type)
            break if size1 < item.min || size1 > item.max > 0
            size0 = size1
          end

          # These are our candidate prefix and suffix. Try to propose them to ahead.
          # If this fails, we restart at the pivot with the usual algorithm. Which
          # will also go backwards (if sensible) but this time,  asking ahead on every
          # step rather than after subtype ends.
          prefix, suffix = matchees.begin.grow(size0), matchees.move(size0)
          if fb = propose?(progress, item, prefix, suffix, ahead)
            return fb
          end
        end
      end
    else
      strategy = item.strategy
    end

    expand(matchees, pivot: pivot, strategy: strategy) do |prefix, suffix|
      next if prefix.size < item.min || prefix.size > item.max > 0
      next unless item.type.any? || prefix.all?(&.type.subtype?(item.type))
      next unless fb = propose?(progress, item, prefix, suffix, ahead)
      return fb
    end

    Fb::Mismatch.new(progress.env)
  end

  def self.sequence(progress, items : Slice(Any), matchees, ahead)
    unless item = items.first?
      return ahead.call(progress, matchees)
    end

    # Continuation for the item matching methods.
    cont = Ahead::Item::SequenceRest.new(progress.ord, items, Ahead::Item.stackptr(ahead))

    match(progress, item, matchees, cont)
  end

  def self.match(progress : Progress, item : Group, matchees, ahead0)
    ahead1 = Ahead::Item::CaptureGroup.new(progress.ord, matchees, item.capture, Ahead::Item.stackptr(ahead0), propose: true)

    sequence(progress, item.children.to_readonly_slice, matchees, ahead1)
  end

  def self.match(progress : Progress, item : Gap, matchees, ahead0)
    strategy = item.strategy.auto? ? ExpandStrategy::Sway : item.strategy

    envs = [] of Term::Dict

    expand(matchees, pivot: (matchees.size / item.frac).ceil.to_i, strategy: strategy) do |prefix, suffix|
      matchee = Term.of(prefix.size)

      ahead1 = Ahead::ItemAdapter.new(progress.ord, suffix, Ahead::Item.stackptr(ahead0))
      ahead2 = Ahead::Forward.new(prefix.size, Ahead.stackptr(ahead1))
      ahead3 = Ahead::Goto.new(progress.behind.backpath?, Ahead.stackptr(ahead2))

      case fb = Operator.match(progress.behind.backpathless, item.measurer, matchee, ahead3)
      in Fb::MatchOne
        envs << fb.env
        next if fb.more
        break
      in Fb::MatchMany
        envs.concat(fb.envs)
        break
      in Fb::Mismatch
      in Fb::Interrupt
        return fb
      end

      progress = progress.copy_with(ord: progress.ord + 1)
    end

    Env.feedback(envs, fallback: progress.env)
  end

  def self.match(progress : Progress, item : Optional, matchees, ahead0)
    if matchee = matchees.first?
      ahead1 = Ahead::Item::Advance.new(1, Ahead::Item.stackptr(ahead0))
      ahead2 = Ahead::ItemAdapter.new(progress.ord, matchees, Ahead::Item.stackptr(ahead1))

      fb = Operator.match(progress.behind, item.tail, matchee, ahead2)
      if fb.is_a?(Fb::Match)
        return fb
      end
    end

    ahead1 = Ahead::ItemAdapter.new(progress.ord, matchees, Ahead::Item.stackptr(ahead0))
    ahead2 = Ahead::Goto.new(progress.behind.backpath?, Ahead.stackptr(ahead1))

    Operator.match(progress.behind.backpath(&.insert_item(item.default, ord: progress.ord)), item.tail, item.default, ahead2)
  end

  def self.many(progress : Progress, item : Many, matchees, ahead0, memo)
    unless behind1 = progress.behind.propose?(item.capture, Term.of(memo))
      return Fb::Mismatch.new(progress.env.with(item.capture, Term.of(memo)))
    end

    if memo.size > item.max > 0
      return Fb::Mismatch.new(progress.env)
    end

    if memo.size >= item.min
      case fb = ahead0.call(progress.copy_with(behind: behind1), matchees)
      in Fb::Match, Fb::Interrupt
        return fb
      in Fb::Mismatch
      end
    end

    ahead1 = Ahead::Item::ManyStep.new(matchees, item, Ahead::Item.stackptr(ahead0), memo)

    sequence(progress, item.children.to_readonly_slice, matchees, ahead1)
  end

  def self.match(progress : Progress, item : Many, matchees, ahead0)
    ahead1 = Ahead::Item::CaptureGroup.new(progress.ord, matchees, item.capture, Ahead::Item.stackptr(ahead0), propose: false)

    many(progress, item, matchees, ahead1, memo: Term[])
  end

  def self.past_lazy(progress : Progress, item : Past, matchees, ahead0, memo)
    if memo > item.max > 0
      return Fb::Mismatch.new(progress.env)
    end

    if memo >= item.min
      case fb = ahead0.call(progress, matchees)
      in Fb::Match, Fb::Interrupt
        return fb
      in Fb::Mismatch
      end
    end

    ahead1 = Ahead::Item::PastStep.new(matchees, item, memo, Ahead::Item.stackptr(ahead0))

    sequence(progress, item.children.to_readonly_slice, matchees, ahead1)
  end

  def self.past_greedy(progress : Progress, item : Past, matchees, ahead0, memo)
    if memo > item.max > 0
      return Fb::Mismatch.new(progress.env)
    end

    ahead1 = Ahead::Item::PastStep.new(matchees, item, memo, Ahead::Item.stackptr(ahead0))

    case fb = sequence(progress, item.children.to_readonly_slice, matchees, ahead1)
    in Fb::Match, Fb::Interrupt
      fb
    in Fb::Mismatch
      if memo >= item.min
        ahead0.call(progress, matchees)
      else
        fb
      end
    end
  end

  def self.match(progress : Progress, item : Past, matchees, ahead)
    if item.greedy
      past_greedy(progress, item, matchees, ahead, memo: 0)
    else
      past_lazy(progress, item, matchees, ahead, memo: 0)
    end
  end

  def self.match(progress : Progress, items : Slice(Any), matchees, ahead)
    item = items.first?

    if item.nil? && matchees.empty? # Matched all items.
      return Ahead.tr(progress.behind, ahead)
    end

    if item.nil? # Ran out of items.
      return Fb::Mismatch.new(progress.env)
    end

    # Continuation for the item matching methods.
    cont = Ahead::Item::Rest.new(items, Ahead.stackptr(ahead))

    match(progress, item, matchees, cont)
  end

  def self.match(behind0 : Behind, items : Slice(Any), matchees, ahead0)
    kp0 = behind0.backpath?

    match(Progress.new(0u32, behind0.backpath(&.update_value(0))), items, matchees, Ahead::Goto.new(kp0, Ahead.stackptr(ahead0)))
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

    # Schemas used to validate options passed to various pattern matching constructs.
    module Schemas
    end

    Schemas::LeafUnbounded = M0::PairSchema.build do
      key :in, values: {:items, :keys, :values, :"pair/values"}, default: :items
      key :order, values: {:dfs, :bfs}, default: :dfs
      key :self, values: {true, false}, default: false
    end

    Schemas::LeafBounded = M0::PairSchema.build do
      key :in, values: {:items, :keys, :values, :"pair/values"}, default: :items
      key :order, values: {:dfs, :bfs}, default: :dfs
      key :min, values: 0..UInt8::MAX, default: 0
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      key :self, values: {true, false}, default: false
      where { |min, max| min.as_n <= max.as_n }
    end

    Schemas::Items = M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 1
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end

    Schemas::Entries = M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 1
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end

    Schemas::Plural = M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 0
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      key :type, values: {:_number, :_string, :_symbol, :_dict, :_}, default: :_
      where { |min, max| min.as_n <= max.as_n }
    end

    Schemas::Many = M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 1
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end

    Schemas::Past = M0::PairSchema.build do
      key :min, values: 0..UInt8::MAX, default: 0
      key :max, values: 1..UInt8::MAX, default: SYM_INF
      where { |min, max| min.as_n <= max.as_n }
    end

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
      blank.type.blank
    end

    # Context for `Normal` calls. Left for future use.
    record Context

    # Returns the normal form of an item sequence *node*.
    def item(ctx : Context, node : Term) : Term
      Term.of_case(node, engine: M0) do
        matchpi %[_symbol] do
          continue unless blank = node.blank?
          continue unless blank.poly?

          name = blank.name?

          Term.of(:"%plural", name ? {:"%capture", name} : nil, type: typesym(blank), min: blank.one? ? 1 : 0, max: SYM_INF)
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
        ) do |opts|
          continue unless opts = Schemas::Plural.enriched?(opts)

          opts.morph({0, node[0]})
        end

        matchpi(
          %[(%plural capture_ ¦ opts_)],
          %[(%plural/min capture_ ¦ opts_)],
          %[(%plural/max capture_ ¦ opts_)],
          cues: {:"%plural", :"%plural/min", :"%plural/max"},
        ) do |opts|
          continue unless opts = Schemas::Plural.enriched?(opts)

          opts.morph({0, node[0]}, {1, {:"%capture", capture}})
        end

        matchpi %[(%optional _ body_)], cue: :"%optional" do
          node.morph({2, pattern(ctx, body)})
        end

        matchpi %[(%group capture_ _ _*)], cue: :"%group" do
          Term::Dict.build do |commit|
            commit << :"%group" << {:"%capture", capture}
            commit.concat(node.items.move(2)) { |member| item(ctx, member) }
          end
        end

        matchpi %[(%many capture_ _ _* ¦ opts_)], cue: :"%many" do |opts|
          continue unless opts = Schemas::Many.enriched?(opts)

          opts.transaction do |commit|
            commit << :"%many" << {:"%capture", capture}
            commit.concat(node.items.move(2)) { |member| item(ctx, member) }
          end
        end

        matchpi %[(%past _ _* ¦ opts_)], cue: :"%past" do |opts|
          continue unless opts = Schemas::Past.enriched?(opts)

          opts.transaction do |commit|
            commit << :"%past"
            commit.concat(node.items.move(1)) { |member| item(ctx, member) }
            commit.with(:greedy, false)
          end
        end

        matchpi %[(%past/max _ _* ¦ opts_)], cue: :"%past/max" do |opts|
          continue unless opts = Schemas::Past.enriched?(opts)

          opts.transaction do |commit|
            commit << :"%past"
            commit.concat(node.items.move(1)) { |member| item(ctx, member) }
            commit.with(:greedy, true)
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
          continue unless normal = NORMAL_INT[type]?

          {:"%terminal", normal}
        end

        matchpi(
          %[(%number %'_ op_symbol _number)],
          %[(%number %'(whole _) op_symbol _number)],
          cue: {:"%number", :_},
          cues: {nil, :whole}
        ) do |op|
          continue unless op.in?(SYMS_CMP)

          {:"%terminal", pattern}
        end

        matchpi(
          %[(%number _number lop_symbol %'_ rop_symbol _number)],
          %[(%number _number lop_symbol %'(whole _) rop_symbol _number)],
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
          %[(%pipe type successor_)],
          cue: :"%pipe",
          cues: {:+, :-, :*, :/, :div, :mod, :**, :map, :span, :tally, :type}
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
          NORMAL_PASS
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
          %[(%not _ _*)],
          cues: {:"%keypool", :"%not"}
        ) { {:"%terminal", pattern} }

        matchpi %[(%keypath capture_)], cue: :"%keypath" do
          {:"%keypath", {:"%capture", capture}}
        end

        matchpi(
          %[(%edge %'_symbol)],
          %[(%edge %'_string)],
          %[(%edge %'_number)],
          %[(%edge %'_)],
          cue: :"%edge"
        ) { pattern }

        # %nonself is dissolved at normalization.
        matchpi %[(%nonself arg_)], cue: :"%nonself" do
          pattern(ctx, arg)
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
          continue unless opts = Schemas::Items.enriched?(opts)

          opts.transaction do |commit|
            commit << :"%items/all" << pattern(ctx, successor)
            commit.concat(pattern.items.move(2)) { |item| pattern(ctx, item) }
          end
        end

        matchpi %[(%entry k_ v_)], cue: :"%entry" do
          {:"%entries/first", pattern(ctx, k), pattern(ctx, v)}
        end

        matchpi %[(%entry° k_ v_)], cue: :"%entry°" do
          {:"%entries/source", pattern(ctx, k), pattern(ctx, v)}
        end

        matchpi %[(%entries successor_ k_ v_ ¦ opts_)], cue: :"%entries" do |opts|
          continue unless opts = Schemas::Entries.enriched?(opts)

          opts.morph(
            {0, :"%entries/all"},
            {1, pattern(ctx, successor)},
            {2, pattern(ctx, k)},
            {3, pattern(ctx, v)},
          )
        end

        matchpi %[(%leaf body_ ¦ opts_)], cue: :"%leaf" do |opts|
          continue unless opts = Schemas::LeafUnbounded.enriched?(opts)

          opts.morph({0, :"%leaves/first"}, {1, pattern(ctx, body)})
        end

        matchpi %[(%leaf° body_ ¦ opts_)], cue: :"%leaf°" do |opts|
          continue unless opts = Schemas::LeafUnbounded.enriched?(opts)

          opts.morph({0, :"%leaves/source"}, {1, pattern(ctx, body)})
        end

        matchpi %[(%leaves successor_ body_ ¦ opts_)], cue: :"%leaves" do |opts|
          continue unless opts = Schemas::LeafBounded.enriched?(opts)

          opts.morph(
            {0, :"%leaves/all"},
            {1, pattern(ctx, successor)},
            {2, pattern(ctx, body)},
          )
        end

        # Leave %new's as-is. They are normalized and compiled when their
        # dependencies are known.
        matchpi %[(%new _)], cue: :"%new" do
          {:"%terminal", pattern}
        end

        # Extended %new with the exports dict.
        matchpi %[(%new (_ _*) _)], cue: :"%new" do
          {:"%terminal", pattern}
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
          cues: {:"%gap", :"%gap/min", :"%gap/max"}
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

        matchpi %[(%many _ _* ¦ _ min: minT_ max: maxT_)], cue: :"%many" do
          min0 = minT.to(Magnitude)
          max0 = maxT == SYM_INF ? Magnitude::INFINITY : maxT.to(Magnitude)
          min, max = items(item.items.move(2))
          {min0 * min, max0 * max}
        end

        matchpi %[(%past _* ¦ _ min: minT_ max: maxT_)], cue: :"%past" do
          min0 = minT.to(Magnitude)
          max0 = maxT == SYM_INF ? Magnitude::INFINITY : maxT.to(Magnitude)
          min, max = items(item.items.move(1))
          {min0 * min, max0 * max}
        end

        matchpi %{[%group _ _*]}, cue: :"%group" do
          items(item.items.move(2))
        end

        otherwise { {Magnitude::INFINITY, Magnitude::INFINITY} }
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

        otherwise { {Magnitude::INFINITY, Magnitude::INFINITY} }
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

        otherwise { {Magnitude::INFINITY, Magnitude::INFINITY} }
      end
    end
  end

  module Item
    alias Neighbor = {Term, NeighborFn}?
    alias NeighborFn = -> Neighbor

    def self.neighbor?(items : Term::Dict::ItemsView, outside : NeighborFn) : Neighbor
      unless head = items.first?
        return outside.call
      end

      Term.case(head, engine: M0) do
        # Dip into %group nodes. If failed, continue to the next item on the current level.
        matchpi %[(%group _ _*)], cue: :"%group" do
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

        match({:"%group", {:"%capture", :capture_}, :_, :"_*"}, cue: :"%group") do |capture|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }

          members = sequence(item.items.move(2), neighbor, captures)

          {Operator::Item::Group.new(capture, members), feed.move(1)}
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

          members = sequence(item.items.move(2), -> { nil.as(Neighbor) }, captures)

          {Operator::Item::Many.new(capture, members, interior.set, min, max), feed.move(1)}
        end

        match({:"%partition", {:"%past", :_, :"_*"}, {min: :min0_, max: :max0_, greedy: :greedy_boolean}}, cue: :"%past") do |min0, max0, greedy|
          min = min0.to(UInt8)
          max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

          members = sequence(item.items.move(1), -> { nil.as(Neighbor) }, captures)

          {Operator::Item::Past.new(members, min, max, greedy: greedy.true?), feed.move(1)}
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

          {Operator::Item::Gap.new(M1.operator(measurer, captures), frac, strategy: :sway), feed.move(1)}
        end

        match({:"%gap/min", :measurer_}, cue: :"%gap/max") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::Gap.new(M1.operator(measurer, captures), frac, strategy: :lazy), feed.move(1)}
        end

        match({:"%gap/max", :measurer_}, cue: :"%gap/max") do |measurer|
          neighbor = -> { Item.neighbor?(feed.move(1), outside).as(Neighbor) }
          frac = frac(neighbor)

          {Operator::Item::Gap.new(M1.operator(measurer, captures), frac, strategy: :greedy), feed.move(1)}
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
      matchpi %{_dict}  do
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
        M1.match?(pattern, matchee, opt: O0, env: env)
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
            %{[%'%entries/first _ successor_]},
            %{[%'%entries/source _ successor_]},
            %{[%'%entries/all _ _ successor_]},
            cues: {:"%entries/first", :"%entries/source", :"%entries/all"}
          ) do
            sketch |= sketch(successor)

            WalkDecision::Skip
          end

          matchpi(
            %{[(%any %layer
                     %singular
                     %group
                     %partition
                     %itemseq
                     %items/first
                     %items/source
                     %items/all
                     %let
                     %all
                     %entry/required)
                _*]},
            %{(%'%leaves/first _* ¦ _ in: (%not keys))},
            %{(%'%leaves/source _* ¦ _ in: (%not keys))},
            %{(%'%leaves/all _* ¦ _ in: (%not keys))},
            cues: {nil, :"%leaves/first", :"%leaves/source", :"%leaves/all"}
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
            %{(%'%leaves/first _* ¦ _ in: (%not keys))},
            %{(%'%leaves/source _* ¦ _ in: (%not keys))},
            %{(%'%leaves/all _* ¦ _ in: (%not keys))},
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
        M1.match?(pattern, matchee, opt: O1, env: env)
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
              (%itemseq (%group prefix (%past/max (%singular _) min: 1))
                        (%plural min: 0 max: ∞ type: %'_)
                        (%group postfix (%past/max (%singular _) min: 1)))]}
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
        matchpi %{[%bounds ((%group successor %prefix (%plural/min min: 1)) (%past (%pass) min: 1))]} do |successor|
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

          passes = args.items.count(Normal::NORMAL_PASS)

          continue unless passes/args.itemsize >= 0.5

          successor = Term::Dict.build do |commit|
            commit << :"%all"

            args.items.each_with_index do |arg, index|
              next if arg == Normal::NORMAL_PASS

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

        # Open %layer all entries of which are (%entry/required) should turn into
        # an %all of (%value (%literal key) value) which we render as lookups rather
        # than letting allocation-heavy %layer logic manage them.
        matchp %[(%'%layer (%pass) side←(%entries required key_ (%entry/required value_)))] do |side, required|
          continue unless required.size == side.size

          Term::Dict.build do |commit|
            commit << :"%all"

            required.each_item_unordered do |match|
              commit << {:"%value", {:"%literal", match[:key]}, optimized1(match[:value], cycle)}
            end
          end
        end

        # (%bounds min: 1 max: ∞) around a single %value has low information content.
        # Remove it.
        matchpi %[(%bounds successor←(%'%value _ _) min: 1 max: ∞)] do
          optimized1(successor, cycle)
        end

        # (%partition (%itemsonly) (%pairsonly)) -> (%dict)
        matchpi %[(%'%partition (%itemsonly) (%pairsonly))] do
          {:"%dict"}
        end

        # (%partition (%itemsonly) (%value (%literal ...) ...)) -> (%value (%literal ...) ...)
        #
        # Similarly for %all of such %values. We cannot do that for %layer or generic %value
        # etc. because that'd change what the pattern means. Note also how we match the type
        # of the value. If one does e.g. (%partition (_*) (%value 0 x_)), unless this check is
        # in place, one would get an assignment for x: ... which shouldn't be possible. If
        # the key is numeric we resort to the slower path.
        matchpi(
          %[(%'%partition (%itemsonly) successor←(%'%value (%'%literal (%any° _string _symbol _boolean _dict)) _))],
          %[(%'%partition (%itemsonly)
              successor←(%'%all (%past min: 1 (%'%value (%'%literal (%any° _string _symbol _boolean _dict)) _))))],
        ) do
          optimized1(successor, cycle)
        end

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
    when Term.of(:items)         then Search::Part::ItemsOrdered
    when Term.of(:keys)          then Search::Part::Keys
    when Term.of(:values)        then Search::Part::Values
    when Term.of(:"pair/values") then Search::Part::PairValues
    else
      raise ArgumentError.new
    end
  end

  module Pair
    def self.operator(key, value, captures)
      Term.case(value, engine: M0) do
        matchpi %{(%entry/required (%any))}, cue: {:"%entry/required", :"%any"} do
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
      matchpi %[(%let (%capture capture_) successor_)], cue: :"%let" do
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

      matchpi %[(%itemseq _*)], cue: :"%itemseq" do
        items = Item.sequence(node.items.move(1), -> { nil.as(Item::Neighbor) }, captures)

        Operator::ItemSeq.new(items.to_readonly_slice)
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

      match({ {:"%literal", :"%layer"}, :below_, :side_ }, cue: :"%layer") do |below, side|
        entries = Array(Operator::Entry::Any).new(side.size)

        side.each_entry do |k, v|
          entries << Pair.operator(k, v, captures)
        end

        # Lowest cost goes first.
        entries.unstable_sort_by!(&.cost)

        Operator::Layer.new(operator(below, captures), entries)
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

        if exterior.empty?
          Operator::ScanAllIsolated.new(operator(successor, captures), needle, min, max)
        else
          Operator::ScanAll.new(operator(successor, captures), needle, inner.set, exterior.set, min, max)
        end
      end

      matchpi %{(%entries/all successor_ k_ v_ ¦ min: min0_ max: max0_)}, cue: :"%entries/all" do
        min = min0.to(UInt8)
        max = max0 == SYM_INF ? 0u8 : max0.to(UInt8)

        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        if exterior.empty?
          Operator::EntriesAllIsolated.new(operator(successor, captures),
            kop: operator(k, captures),
            vop: operator(v, captures),
            min: min,
            max: max)
        else
          Operator::EntriesAll.new(operator(successor, captures),
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

      matchpi %{(%leaves/all successor_ body_ ¦ in: part_ min: min_ max: max_ order: dfs self: depth0_boolean)}, cue: {:"%leaves/all", :dfs} do |min, max|
        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        min = min.to(UInt8)
        max = max == SYM_INF ? 0u8 : max.to(UInt8)

        if exterior.empty?
          Operator::DfsAllIsolated.new(operator(successor, captures), operator(body, captures), search_part(part), min, max, depth0.true?)
        else
          Operator::DfsAll.new(operator(successor, captures), operator(body, captures), inner.set, exterior.set, search_part(part), min, max, depth0.true?)
        end
      end

      matchpi %{(%leaves/all successor_ body_ in: part_ min: min_ max: max_ order: bfs self: depth0_boolean)}, cue: {:"%leaves/all", :bfs} do |min, max|
        outer = captures
        inner = Bag(Term).new

        sequence = node.items.move(2)
        sequence.each { |node| captures(node, storage: inner) }

        exterior = inner & (outer - inner)

        min = min.to(UInt8)
        max = max == SYM_INF ? 0u8 : max.to(UInt8)

        if exterior.empty?
          Operator::BfsAllIsolated.new(operator(successor, captures), operator(body, captures), search_part(part), min, max, depth0.true?)
        else
          Operator::BfsAll.new(operator(successor, captures), operator(body, captures), inner.set, exterior.set, search_part(part), min, max, depth0.true?)
        end
      end

      match({:"%all", :a_, :b_}, cue: :"%all") do |a, b|
        Operator::Both.new(operator(a, captures), operator(b, captures))
      end

      matchpi %[(%any/literal _*)], cue: :"%any/literal" do
        branches = node.items.move(1).to_set

        Operator::LiteralChoices.new(branches)
      end

      match({:"%any/source", :a_}, cue: :"%any/source") do |a|
        operator(a, captures)
      end

      match({:"%any/source", :a_, :b_}, cue: :"%any/source") do |a, b|
        Operator::SourceChoice.new(operator(a, captures), operator(b, captures))
      end

      match({:"%any/source", :a_, :_, :"_*"}, cue: :"%any/source") do |a|
        rest = Term::Dict.build do |commit|
          commit << :"%any/source"

          args = node.items.move(2)
          args.each { |item| commit << item }
        end

        Operator::SourceChoice.new(operator(a, captures), operator(Term.of(rest), captures))
      end

      match({:"%edge", {:"%literal", :_}}, cue: :"%edge") do
        Operator::Edge.new(:any)
      end

      match({:"%edge", {:"%literal", :_symbol}}, cue: :"%edge") do
        Operator::Edge.new(:symbol)
      end

      match({:"%edge", {:"%literal", :_string}}, cue: :"%edge") do
        Operator::Edge.new(:string)
      end

      match({:"%edge", {:"%literal", :_number}}, cue: :"%edge") do
        Operator::Edge.new(:number)
      end

      match({:"%not", :_, :"_*"}, cue: :"%not") do
        blacklist = node.items.move(1)

        Operator::Not.new(blacklist.set)
      end

      match({:"%number", {:"%literal", :_}}, cue: :"%number") do
        Operator::INSTANCE_NUM
      end

      match({:"%number", {:"%literal", {:whole, :_}}}, cue: {:"%number", :whole}) do
        Operator::INSTANCE_NUM_WHOLE
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

      match({:"%new", :pattern_}, {:"%new", :_, :pattern_}, cue: :"%new") do |pattern|
        Operator::New.new((blanks(pattern) & captures).array, pattern)
      end

      matchpi %[(%symbol nonblank)], cue: {:"%symbol", :"nonblank"} do
        Operator::SymNonblank.new
      end

      matchpi %[(%symbol blank name_ type_)], cue: {:"%symbol", :"blank"} do
        Operator::SymBlank.new(operator(name, captures), operator(type, captures))
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
      dict0.pairspart.each_entry do |key, value|
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
      Term.case(node, engine: M0) do
        if itemseq
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

          # Recurse into %group and %many with itemseq flag on.
          matchpi(
            %{[%group _ _ _*]},
            %{[%many _ _ _*]},
            %{[%past _ _*]},
            %{[%past/max _ _*]},
            cues: {:"%group", :"%many", :"%past", :"%past/max"},
          ) do
            WalkDecision::Continue
          end

          # Avoid all other item sequence nodes.
          otherwise { WalkDecision::Skip }
        else
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
          storage << capture
        end

        matchpi %{[%new exports_dict _]} do
          exports.items.each do |capture|
            storage << capture
          end
        end

        otherwise { }
      end

      WalkDecision::Continue
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
  def self.operator(pattern : Term, *, normalize = true, optimize = true, fresh = false, opt = DEFAULT_OPT_LEVEL) : Operator::Any
    PATTERN_CACHE.fetch(pattern.unsafe_repr, fresh: fresh) do
      normal = normalize ? normal(pattern) : pattern
      optimal = optimize ? optimized(normal, opt) : normal
      captures = captures(normal)
      operator(optimal, captures)
    end
  end

  def self.matches(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], opt = DEFAULT_OPT_LEVEL, **kwargs) : Array(Term::Dict)
    Operator.matches(env, operator(pattern, opt: opt), matchee, **kwargs)
  end

  def self.match?(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], opt = DEFAULT_OPT_LEVEL, **kwargs)
    Operator.match?(env, operator(pattern, opt: opt), matchee, **kwargs)
  end

  def self.probe?(pattern : Term, matchee : Term, *, env : Term::Dict = Term[], opt = DEFAULT_OPT_LEVEL, **kwargs)
    Operator.probe?(env, operator(pattern, opt: opt), matchee, **kwargs)
  end
end

module ::Ww::Backpath::Word
  extend self

  alias Any = None | Some
  alias Some = Atomic | Compound

  # "Atomic" or "indivisible" words that actually exist in the trie.
  alias Atomic = Create | CreateLeaf | Delete | Insert | Range | Pair

  # "Compound words" are broken down into atomic words.
  alias Compound = UpdateKey | UpdateValue

  # Backpath words that do not have neighbors/successors in the trie (must stand at
  # the end of a "sentence").
  alias Terminal = CreateLeaf | Range

  # Words that have neighbors/successors in the trie.
  alias Nonterminal = Compound | Create | Delete | Insert

  record UpdateKey, key : Term
  record UpdateValue, key : Term

  record None
  record Create, key : Term, initial : Term, value : Term
  record CreateLeaf, key : Term
  record Insert, index : Term::Num, ord : UInt32, initial : Term
  record Pair, key : Term
  record Range, b : Int32, e : Int32, ord : UInt32
  record Delete, keys : Term::Dict

  record Key
  record Value

  # :nodoc:
  SYM_PAIR = Term.of(:pair)

  # :nodoc:
  SYM_RESIDUE = Term.of(:residue)

  # :nodoc:
  SYM_RANGE = Term.of(:range)

  # :nodoc:
  SYM_CREATE_LEAF = Term.of(:"create-leaf")

  # :nodoc:
  SYM_CREATE = Term.of(:create)

  # :nodoc:
  SYM_INSERT = Term.of(:insert)

  # Parses a subject term *subject* into an `Atomic` backpath word.
  #
  # Raises `KeypathError` if that cannot be done.
  def atomic(subject : Term) : Atomic
    raise KeypathError.new unless dict = subject.as_itemsonly_d?
    raise KeypathError.new if dict.empty?

    case {dict[0], dict.size - 1}
    when {SYM_PAIR, 1}
      _, key = dict

      Pair.new(key)
    when {SYM_RESIDUE, 1}
      _, keys = dict

      raise KeypathError.new unless keys = keys.as_d?

      Delete.new(keys)
    when {SYM_RANGE, 3}
      _, b, e, ord = dict

      raise KeypathError.new unless (b = b.as_n?) && (e = e.as_n?) && (ord = ord.as_n?)

      Range.new(b.to(Int32), e.to(Int32), ord.to(UInt32))
    when {SYM_CREATE_LEAF, 1}
      _, key = dict

      CreateLeaf.new(key)
    when {SYM_CREATE, 2}
      _, key, initial = dict

      Create.new(key, initial, value: initial)
    when {SYM_INSERT, 3}
      _, index, ord, initial = dict

      raise KeypathError.new unless (index = index.as_n?) && (ord = ord.as_n?)

      Insert.new(index, ord.to(UInt32), initial)
    else
      raise KeypathError.new
    end
  end

  # Yields substructure of *word* and/or *matchee* for the block to check out.
  def checkout(word : Create, matchee : Term, &) : Nil
    yield word.value
  end

  # :ditto:
  def checkout(word : CreateLeaf, matchee : Term, &) : Nil
  end

  # :ditto:
  def checkout(word : Insert, matchee : Term, &) : Nil
    yield word.initial
  end

  # :ditto:
  def checkout(word : Range, matchee : Term, &) : Nil
  end

  # :ditto:
  def checkout(word : Delete, matchee : Term, &) : Nil
    yield Term.of(matchee &- word.keys.items)
  end

  def terms(word : Create) : Enumerable(Term)
    {Term.of(:create, word.key, word.value)}
  end

  def terms(word : CreateLeaf) : Enumerable(Term)
    {Term.of(:"create-leaf", word.key)}
  end

  def terms(word : Insert) : Enumerable(Term)
    {Term.of(:insert, word.index, word.ord, word.initial)}
  end

  def terms(word : UpdateKey) : Enumerable(Term)
    {Term.of(:pair, word.key), Term.of(:key)}
  end

  def terms(word : UpdateValue) : Enumerable(Term)
    {Term.of(:pair, word.key), Term.of(:value)}
  end

  def terms(word : Range) : Enumerable(Term)
    {Term.of(:range, word.b, word.e, word.ord)}
  end

  def terms(word : Delete) : Enumerable(Term)
    {Term.of(:residue, word.keys)}
  end
end

module ::Ww::M1
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

    node.each_entry do |word, successor|
      Term.case(word) do
        matchpi %[(value key_)] do
          ctx = reflect(ctx, successor.as_d, maxdepth - 1, matchee[key])
        end

        matchpi %[(residue keys←(_*))] do
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
          {up1.with(pred, matchee0), Rewrite.one(apply(up0, down, my, succ))}
        end

        otherwise do
          {up1, Rewrite.one(apply(up0, down, my, body))}
        end
      end
    end
  end

  class BackmapTrie
    # NOTE: @env, @body, @captures must only exist on a "tapped" BackmapTrie nodes.
    # NOTE: @neighbors only exist on "fanout" BackmapTrie nodes.
    # NOTE: The third type of backmaptrie node unifies both of them.

    @env : Term::Dict?

    alias Word = Backpath::Word

    def initialize
      @captures = [] of Term
      @neighbors = {} of Word::Atomic => BackmapTrie | BackmapPair
    end

    def mentioned_in?(backspec)
      @captures.any? { |capture| capture.in?(backspec) || Term.dict(capture).in?(backspec) }
    end

    def mount(backpath : Term::Dict::ItemsView, capture : Term, env : Term::Dict) : Nil
      unless subject = backpath.first?
        @env = env
        @captures << capture
        return
      end

      case word = Word.atomic(subject)
      when Word::Pair
        neighbor = @neighbors.put_if_absent(word) { BackmapPair.new }.as(BackmapPair)
        neighbor.mount(backpath.move(1), capture, env)
      else
        neighbor = @neighbors.put_if_absent(word) { BackmapTrie.new }
        neighbor.mount(backpath.move(1), capture, env)
      end
    end

    def mount(backpath : Term, capture : Term, env : Term::Dict) : Nil
      mount(backpath.items, capture, env)
    end

    def reflect(layer : Int, matchee : Term)
      Term::Dict.build { |commit| reflect(layer, commit, matchee) }
    end

    def reflect(layer : Int, ctx : Term::Dict::Commit, matchee : Term)
      # Take a snapshot of what the matchee looks like at this level if we have any
      # captures at this level.
      @captures.each { |capture| ctx.with(capture, matchee) }

      return if layer.zero?

      @neighbors.each do |word, neighbor|
        case word
        when Word::Pair
          next unless value = matchee[word.key]?

          neighbor.as(BackmapPair).reflect(layer - 1, ctx, word.key, value)
        else
          Word.checkout(word, matchee) do |substructure|
            neighbor.as(BackmapTrie).reflect(layer - 1, ctx, substructure)
          end
        end
      end
    end

    def morph0(up0, up1, down, backspec, matchee, applier) : {Term::Dict, Rewrite::Any}
      plural = false
      body = nil

      @captures.each do |capture|
        if body = backspec[capture]? # Singular
          break
        elsif body = backspec[{capture}]? # Plural
          plural = true
          break
        end
      end

      rewrite = Rewrite.none

      if body
        up1, rewrite = applier.call(up0, up1, down, @env || Term[], matchee, body) # ?!

        case rewrite
        in Rewrite::One
          newvalue = rewrite.term

          # Go to plural mode if the user wishes so.
          if plural && (list = newvalue.as_itemsonly_d?)
            rewrite = Rewrite.many(list)
          end

          up1 = @captures.reduce(up1) { |up, capture| up.with(capture, newvalue) }
        in Rewrite::Many
          newvalue = Term.of(rewrite.list)

          # Snap back to singular if the user did not specify plural explicitly.
          unless plural
            rewrite = Rewrite.one(newvalue)
          end

          up1 = @captures.reduce(up1) { |up, capture| up.with(capture, newvalue) }
        end
      end

      {up1, rewrite}
    end

    def morph(layer : Int, up0, up1, down, backspec, matchee : Term, applier) : {Term::Dict, Term}
      if layer.zero?
        return up1, matchee
      end

      relabel = nil
      insertions = nil

      @neighbors.each do |word, neighbor|
        case word
        in Word::Create
          if layer == 1
            up1, rewrite = neighbor.as(BackmapTrie).morph0(up0, up1, down, backspec, word.value, applier)

            case rewrite
            in Rewrite::None then value = word.value
            in Rewrite::One  then value = rewrite.term
            in Rewrite::Many then value = rewrite.list
            end

            # If there were no changes vs. the default, we do not output the optional.
            #
            # ?! This should be done, but whether or not this should depend on changes
            #    is questionable.
            next if word.initial == value

            matchee = matchee.with(word.key, value)
          else
            up1, value = neighbor.as(BackmapTrie).morph(layer - 1, up0, up1, down, backspec, word.value, applier)

            relabel ||= [] of {Word::Atomic, Word::Atomic?}
            relabel << {word, word.copy_with(value: value)}
          end
        in Word::CreateLeaf
          next unless layer == 1

          up1, rewrite = neighbor.as(BackmapTrie).morph0(up0, up1, down, backspec, nil, applier)

          case rewrite
          in Rewrite::None
          in Rewrite::One
            matchee = matchee.with(word.key, rewrite.term)
          in Rewrite::Many
            matchee = matchee.with(word.key, rewrite.list)
          end
        in Word::Delete
          residue0 = matchee &- word.keys.items
          if layer == 1
            up1, residue1r = neighbor.as(BackmapTrie).morph0(up0, up1, down, backspec, Term.of(residue0), applier)
            residue1 = residue1r.term? || residue0
          else
            up1, residue1 = neighbor.as(BackmapTrie).morph(layer - 1, up0, up1, down, backspec, Term.of(residue0), applier)
          end
          matchee = matchee.transaction do |commit|
            residue1 &-= word.keys.items
            residue0.each_entry { |k, _| commit.without(k) }
            residue1.each_entry { |k, v| commit.with(k, v) }
          end
        in Word::Insert
          if layer == 1
            up1, rewrite = neighbor.as(BackmapTrie).morph0(up0, up1, down, backspec, word.initial, applier)

            case rewrite
            in Rewrite::None then items1 = Term[{word.initial}]
            in Rewrite::One  then items1 = Term[{rewrite.term}]
            in Rewrite::Many then items1 = rewrite.list
            end

            insertions ||= [] of {Term::Num, Term::Num, Term::Num, Term::Dict}
            insertions << {Term[word.index], Term[word.index], Term[word.ord], items1}
          else
            up1, value = neighbor.as(BackmapTrie).morph(layer - 1, up0, up1, down, backspec, word.initial, applier)

            relabel ||= [] of {Word::Atomic, Word::Atomic?}
            relabel << {word, word.copy_with(initial: value)}
          end
        in Word::Range
          next unless layer == 1

          unless (word.b...word.e).subrange_of?(matchee.items.bounds)
            raise KeypathError.new
          end

          b = Term[word.b]
          e = Term[word.e]
          ord = Term[word.ord]

          items0 = matchee.items(b, e)
          up1, rewrite = neighbor.as(BackmapTrie).morph0(up0, up1, down, backspec, Term.of(items0), applier)

          case rewrite
          in Rewrite::None
            next
          in Rewrite::One
            items1 = Term[{rewrite.term}]
          in Rewrite::Many
            items1 = rewrite.list
          end

          insertions ||= [] of {Term::Num, Term::Num, Term::Num, Term::Dict}
          insertions << {b, e, ord, items1}
        in Word::Pair
          next unless value0 = matchee[key0 = word.key]?

          up1, key1r, value1r = neighbor.as(BackmapPair).morph(layer - 1, up0, up1, down, backspec, key0, value0, applier)

          value = matchee[key0]? || raise KeypathError.new

          case key1r
          in Rewrite::None
            key1 = key0
          in Rewrite::One
            key1 = key1r.term
          in Rewrite::Many
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
            if key1r.list.empty? && layer == 1
              matchee = matchee.without(key0)
              next
            end

            key1 = Term.of(key1r.list)
          end

          unless key0 == key1
            # NOTE: may collide
            matchee = matchee.without(key0).with(key1, value)

            if layer > 1
              relabel ||= [] of {Word::Atomic, Word::Atomic?}
              relabel << {word, word.copy_with(key: key1)} # NOTE: may collide
            end
          end

          case value1r
          in Rewrite::None
          in Rewrite::One
            matchee = matchee.with(key1, value1r.term)
          in Rewrite::Many
            if inspt = matchee.index?(key1)
              insertions ||= [] of {Term::Num, Term::Num, Term::Num, Term::Dict}
              insertions << {inspt, inspt + 1, Term[0], value1r.list}
            elsif value1r.list.empty?
              # One should be able to delete a key-value pair from a backmap with an empty
              # plural **value** transform:
              #
              #   {x: x_, y: y_} <> {(x): ()} ;; Removes `x` pair
              #
              matchee = matchee.without(key1)
            else
              # Otherwise act the same as One.
              matchee = matchee.with(key1, value1r.list)
            end
          end
        end
      end

      # It is unwise to delete/insert while we're iterating over @neighbors, so
      # we have a separate "relabel" step.
      if relabel
        relabel.each do |k0, k1|
          v = @neighbors.delete(k0) || raise KeyError.new("attempt to relabel an absent label #{k0}")
          next unless k1
          @neighbors[k1] = v
        end
      end

      if insertions
        insertions.sort_by! { |b, _, ord, _| {-b, -ord} }
        insertions.each do |b, e, _, replacement|
          matchee = matchee.replace(b...e, &.concat(replacement.items))
        end
      end

      {up1, Term.of(matchee)}
    end
  end

  class BackmapPair
    @k : BackmapTrie?
    @v : BackmapTrie?

    def mount(backpath : Term::Dict::ItemsView, capture : Term, env : Term::Dict) : Nil
      case subject = backpath.first?
      when Term.of(:key)   then neighbor = @k ||= BackmapTrie.new
      when Term.of(:value) then neighbor = @v ||= BackmapTrie.new
      else
        raise KeypathError.new
      end

      neighbor.mount(backpath.move(1), capture, env)
    end

    def reflect(layer : Int, ctx : Term::Dict::Commit, key : Term, value : Term)
      @k.try &.reflect(layer, ctx, key)
      @v.try &.reflect(layer, ctx, value)
    end

    def morph(layer, up0, up1, down, backspec, key0, value0, applier)
      if layer == 0
        up1, key1r = @k.try &.morph0(up0, up1, down, backspec, key0, applier) || {up1, Rewrite.one(key0)}
        up1, value1r = @v.try &.morph0(up0, up1, down, backspec, value0, applier) || {up1, Rewrite.one(value0)}
        {up1, key1r, value1r}
      else
        up1, key1 = @k.try &.morph(layer, up0, up1, down, backspec, key0, applier) || {up1, key0}
        up1, value1 = @v.try &.morph(layer, up0, up1, down, backspec, value0, applier) || {up1, value0}
        {up1, Rewrite.one(key1), Rewrite.one(value1)}
      end
    end
  end

  def self.backmapr(envs : Enumerable(Term::Dict), backspec : Term, matchee : Term, *, applier = DefaultApplier.new) : Rewrite::Some
    trie = BackmapTrie.new
    depth = 0u32

    envs.each do |env|
      next unless backpaths = env[:"(backpaths)"]?

      backpaths.each_entry do |capture, backpathset|
        backpathset.each_entry do |backpath, _|
          trie.mount(backpath, capture, env)

          depth = Math.max(backpath.items.count { |x| !x.in?(Term.of(:key), Term.of(:value)) }.to_u32, depth)
        end
      end
    end

    ctx0 = Term[]

    # pp trie
    # pp depth

    (1..depth).reverse_each do |layer|
      # pp layer
      upper = trie.reflect(layer, matchee)
      lower = ctx0.sub(upper)
      ctx0 |= upper
      ctx0, matchee = trie.morph(layer, ctx0, ctx0, lower, backspec, matchee, applier)
      # pp matchee
    end

    upper = trie.reflect(0, matchee)
    lower = ctx0.sub(upper)
    ctx0 |= upper
    _, rewrite = trie.morph0(ctx0, ctx0, lower, backspec, matchee, applier)

    rewrite.as?(Rewrite::Some) || Rewrite.one(matchee)
  end

  def self.backmap(envs : Enumerable(Term::Dict), backspec : Term, matchee : Term, **kwargs) : Term
    rewrite = backmapr(envs, backspec, matchee, **kwargs)
    rewrite.term?
  end

  def self.backmapr(operator : Operator::Any, backspec : Term, matchee : Term, *, env = Term[], **kwargs) : Rewrite::Any
    case fb = Operator.feedback(env, operator, matchee, backpaths: true)
    in Operator::Fb::Match
      backmapr(fb.envs, backspec, matchee, **kwargs)
    in Operator::Fb::Mismatch
      Rewrite.none
    end
  end

  def self.backmapr(pattern : Term, backspec : Term, matchee : Term, **kwargs) : Rewrite::Any
    backmapr(operator(pattern), backspec, matchee, **kwargs)
  end

  def self.backmap?(pattern : Term | Operator::Any, backspec : Term, matchee : Term, **kwargs) : Term?
    rewrite = backmapr(pattern, backspec, matchee, **kwargs)
    rewrite.term?
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

module ::Ww::M1
  def self.depth(normp : Term) : {Magnitude, Magnitude}
    Term.case(normp, engine: M0) do
      matchpi %{[%pass]}, %{[%dict]}, cues: {:"%pass", :"%dict"} do
        {Magnitude.new(0), Magnitude::INFINITY}
      end

      matchpi %{[%'%literal x_dict]}, cue: :"%literal" do
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
        %{[%let _ successor_]},
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
        %{(%leaves/first _ ¦ _ in: keys)},
        %{(%leaves/source _ ¦ _ in: keys)},
        %{(%leaves/all _ _ ¦ _ in: keys)},
        %{[%new _]},
        %{[%new _ _]},
        cues: {:"%gap",
               :"%gap/min",
               :"%gap/max",
               :"%entry/negative",
               :"%entry/negative",
               :"%leaves/first",
               :"%leaves/source",
               :"%leaves/all",
               :"%new",
               :"%new"},
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
        min = Magnitude.new(0)
        max = Magnitude.new(0)

        offshoots = normp.items.move(1)
        offshoots.each do |offshoot|
          min1, max1 = depth(offshoot)
          min = Math.max(min, min1)
          max = Math.max(max, max1)
        end

        {min, max}
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
        min = Magnitude::INFINITY
        max = Magnitude.new(0)

        branches = normp.items.move(1)
        branches.each do |branch|
          min1, max1 = depth(branch)
          min = Math.min(min, min1)
          max = Math.max(max, max1)
        end

        {min, max}
      end

      matchpi %{[%'%partition itemspart_ pairspart_]}, cue: :"%partition" do
        min0, max0 = depth(itemspart)
        min1, max1 = depth(pairspart)

        {Math.max(min0, min1), Math.max(max0, max1)}
      end

      matchpi %{[%edge _]}, cue: :"%edge" do
        {Magnitude.new(1), Magnitude.new(1)}
      end

      matchpi(
        %{[%value _ successor_]},
        %{[%entries/first _ successor_]},
        %{[%entries/source _ successor_]},
        %{[%entries/all _ _ successor_]},
        %{(%leaves/first successor_ ¦ _ self: false)},
        %{(%leaves/source successor_ ¦ _ self: false)},
        %{(%leaves/all _ successor_ ¦ _ self: false)},
        cues: {:"%value",
               :"%entries/first",
               :"%entries/source",
               :"%entries/all",
               :"%leaves/first",
               :"%leaves/source",
               :"%leaves/all"},
      ) do
        min, _ = depth(successor)

        {min + 1, Magnitude::INFINITY}
      end

      matchpi(
        %{(%leaves/first successor_ ¦ _ self: true)},
        %{(%leaves/source successor_ ¦ _ self: true)},
        %{(%leaves/all _ successor_ ¦ _ self: true)},
        cues: {:"%leaves/first", :"%leaves/source", :"%leaves/all"},
      ) do
        min, _ = depth(successor)

        {min, Magnitude::INFINITY}
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
        min = Magnitude.new(1)
        max = Magnitude.new(1)

        items = normp.items.move(1)
        items.each do |item|
          min1, max1 = depth(item)
          min = Math.max(min, min1 + 1)
          max = Math.max(max, max1 + 1)
        end

        {min, max}
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
        min = Magnitude.new(0)
        max = Magnitude.new(0)

        items = normp.items.move(2)
        items.each do |item|
          min1, max1 = depth(item)
          min = Math.max(min, min1)
          max = Math.max(max, max1)
        end

        {min, max}
      end

      matchpi %{[%items/all _*]}, cue: :"%items/all" do
        min = Magnitude.new(1)
        max = Magnitude.new(1)

        items = normp.items.move(2)
        items.each do |item|
          min1, max1 = depth(item)
          min = Math.max(min, min1 + 1)
          max = Math.max(max, max1 + 1)
        end

        {min, max}
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
        {Magnitude::INFINITY, Magnitude::INFINITY}
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
          %[(%'%let _ _)],
          %[(%'%not _+)],
          %[(%'%new _)],
          %[(%'%new _ _)],
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

        matchpi %[(%'%keypool keys_+)] do
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
  # For example, in `(+ a_ b_)` that would be `+`; and in `(⏏a ⏏b x←qux x_ y_)` that
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

{% if flag?(:profile) %}
  module Profile
    class_getter rtime : Hash(M1::Operator::Any, Time::Span) do
      hash = Hash(M1::Operator::Any, Time::Span).new
      hash.compare_by_identity
      hash
    end

    class_getter optop : Hash(M1::Operator::Any, Term) do
      hash = Hash(M1::Operator::Any, Term).new
      hash.compare_by_identity
      hash
    end

    class_getter hits : Hash(M1::Operator::Any, Int32) do
      hash = Hash(M1::Operator::Any, Int32).new
      hash.compare_by_identity
      hash
    end

    at_exit do
      rtime.to_a.sort_by { |op, span| span * hits[op] }.each do |op, span|
        hitcount = hits[op]
        puts "Pattern".colorize.bold
        puts ML.display(optop[op])
        puts "Took: #{span.total_microseconds}µs × #{hitcount}".colorize.bold
      end
    end
  end
{% end %}

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
  def response(matchee : Term, *, env = Term[], backpaths = false) : Pr::Any
    fb = nil

    # TODO: remove this or somehow make this "official"
    {% if flag?(:profile) %}
      if backpaths
        fb = O.feedback(env, @operator, matchee, backpaths: backpaths)
      else
        took = Time.measure do
          fb = O.feedback(env, @operator, matchee, backpaths: backpaths)
        end
        ca = Profile.rtime[@operator]? || 0.nanoseconds
        hits = Profile.hits[@operator]? || 0

        Profile.rtime[@operator] = ca + (took - ca)/(hits + 1)
        Profile.hits[@operator] = hits + 1
      end
    {% else %}
      fb = O.feedback(env, @operator, matchee, backpaths: backpaths)
    {% end %}

    fb = fb.not_nil!

    case fb
    in O::Fb::MatchOne  then Pr::One.new(self, fb.env)
    in O::Fb::MatchMany then Pr::Many.new(self, fb.envs)
    in O::Fb::Mismatch  then Pr::Neg.new
    end
  end

  def probe?(matchee : Term, *, env = Term[]) : Bool
    O.probe?(env, @operator, matchee)
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
class PatternSet
  # :nodoc:
  def initialize(@headed : Hash(Term, Slice(Pattern)), @headless : Slice(Pattern))
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
  def self.select(selector : Term, base : Term, & : Term, Term::Dict -> Bool?) : PatternSet
    seen = Set(Term).new

    headed = {} of Term => Array(Int32)
    headless = [] of Int32

    patterns = [] of Pattern
    specificities = [] of M1::Specificity

    base.each_item_unordered do |item|
      envs = Term.matches(selector, item)
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

        pattern = Pattern.new(index.to_u32, operator)
        next unless yield normp, env

        patterns << pattern

        if head = M1.head?(normp)
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

  # Same as `select`, but parses *selector* and *base* for you.
  def self.selectp(selector : String, base : String) : PatternSet
    self.select(ML.term(selector), ML.terms(base))
  end

  struct Candidates
    include ICursor

    def initialize(@headed : Slice(Pattern), @headless : Slice(Pattern), @index = 0)
    end

    def current? : Pattern?
      if 0 <= @index < @headed.size
        @headed[@index]
      elsif 0 <= @headed.size <= @index < @headed.size + @headless.size
        @headless[@index - @headed.size]
      end
    end

    def next? : Candidates?
      if @index + 1 < @headed.size + @headless.size
        Candidates.new(@headed, @headless, @index + 1)
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

  def candidates(matchee : Term) : Candidates
    bucket = matchee.as_d?
      .try { |dict| dict.items.first? }
      .try { |head| @headed[head]? }

    bucket ||= Slice(Pattern).empty

    Candidates.new(bucket, @headless, index: 0)
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
  def population
    ee.sum(Population.zero) { |_, v| v }
  end

  # Lets the block replace items in the given *range* with zero or more items
  # by appending to the commit. Returns the modified copy of `self`.
  def replace(range : Range(Term::Num, Term::Num), & : Term::Dict::Commit ->) : Term::Dict
    pairspart.transaction do |commit|
      # Copy before
      (Term[0]...range.begin).each do |index|
        commit.append(self[index])
      end

      yield commit

      # Copy after
      (range.end...itemsize).each do |index|
        commit.append(self[index])
      end
    end
  end

  def replace(index : Term::Num, &)
    replace(index...index + 1) { |commit| yield commit }
  end
end
