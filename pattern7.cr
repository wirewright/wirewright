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
# │ %dict: %pair k %absent   │   +   │   +     │   +   │            │    ·     │       │
# │ %dict: %pair k %absent:v │   +   │   +     │   +   │            │    ·     │       │
# │ %dict: %value            │   ~   │   ~     │   ~   │            │    ~     │       │
# │ %dict: %entries          │   ~   │   ~     │   ~   │            │    ·     │       │
# │ %dict: %entries source   │   ~   │   ~     │   ~   │            │    ~     │       │
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

# At this point we should begin work on the Source-based rewriters. Later we should be able to integrate
# them into soma11 to have managed/timeout-able rewriting.

###

# Hard part in rewriting etc.: ability to support (%locus ...) and multiple envs returned.

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

# module Keypath
#   extend self

#   def append(keypath : Term::Dict, key)
#     keypath.append(key)
#   end

#   def append(keypath : Nil, key)
#   end

#   private def set(root : Term, keypath : Term::Dict::ItemsView, leaf)
#     return leaf if keypath.empty?

#     # If value is a non-dict, the keypath is invalid, so we back off without
#     # change -- the least offsensive move.
#     return root unless dict = root.as_d?

#     set(dict, keypath, leaf)
#   end

#   private def set(root : Term::Dict, keypath : Term::Dict::ItemsView, leaf)
#     return root unless step = keypath[0]?

#     root.with(step) do |value|
#       return root if value.nil?

#       set(value, keypath + 1, leaf)
#     end
#   end

#   def set(root : Term::Dict, keypath : Term::Dict, value)
#     set(root, keypath.items, value)
#   end

#   def follow?(root : Term, keypath : Term::Dict)
#     unless span = keypath[:span]?
#       return Term.of(keypath.items.reduce(root) { |dict, key| dict[key]? || return })
#     end

#     return unless span.as_n? && span.whole? && (length = span.to?(Int32))
#     return unless last = keypath.items.last?
#     return unless last.as_n? && last.whole? && (b = last.to?(Int32))

#     prior = keypath.items.grow(-1)
#     dict = prior.reduce(root) { |dict, key| dict[key]? || return }

#     Term::Dict.build do |commit|
#       (b...b + length).each do |index|
#         commit << (dict[index]? || return)
#       end
#     end
#   end
# end

# (enter <key>)
# (enter <key> span: <span>)
# (set <value>)
#
# e.g. (enter x) (enter y) (set "Hello World!")
#  on {x: {y: "Bye", z: 100}}
#  gives {x: {y: "Hello World!", z: 100}}
#
# We also must support collation of plans, e.g.
#
# (enter x) (enter a) (set qux)
# (enter x) (enter b) (set qix)
#
# is collated into
#
# (enter x) |- (enter a) - (set qux)
#           |- (enter b) - (set qix)
#
# Most importantly, this allows us to support nested %entries°.
#
# How are we going to support %layer though?
#
# %layer stands for "everything except", so we can introduce
#
# (enter/without <...keys>)
#
# This would create plans such as:
#
# (enter x) - ...
# (enter y) - ...
# (enter/without x y) - ...

# module Plan

#   def self.run(plan : Term, term : Term) : Term
#     Term.case(plan) do
#       matchpi %[(value value_)] { value }

#       # TODO: (value* value_ span: span←(%number i32))
#       matchpi %[(value* value_ span: span_number)] do
#         return term unless dict0 = term.as_d?
#         return term unless span.whole? && span.positive?

#         replace(dict0, )

#         pp span.to(Int32)
#       end

#       matchpi %[(plan ¦ steps_)] do
#         return term unless dict0 = term.as_d?

#         dict1 = dict0.transaction do |commit|
#           steps.each_entry do |step, succ|
#             Term.case(step) do
#               # Give successor the value of key. Replace with successor's value.
#               matchpi %[(enter key_)] do
#                 v0 = commit[key]? || Term.of
#                 v1 = run(succ, v0)
#                 commit.with(key, v1)
#               end

#               # Give successor a dictionary without *keys*. Replace all keys except
#               # *keys* with the dictionary returned by the successor.
#               matchpi %[(enter/without keys_*)] do
#                 residue0 = dict0 &- keys.items
#                 residue1 = run(succ, Term.of(residue0))
#                 residue1 &-= keys.items

#                 residue0.each_entry { |k, _| commit.without(k) }
#                 residue1.each_entry { |k, v| commit.with(k, v) }
#               end
#             end
#           end
#         end

#         Term.of(dict1)
#       end
#     end
#   end
# end

# plan = ML.parse1(<<-SCHEME
# (plan (enter/without x y): (value {a: 100, b: 200, x: BOO})
#       (enter x): (value qux)
#       (enter y): (value qix))
# SCHEME
# )

# trie node:
#   pairs:
#     (value k1): ...
#     (key k2): ...
#   items:
#     (index <index>): id ;; for simple item sets such as (x_ y_)
#     (range <b> <e> <ord>): id ;; for groups, slots, %many
#     (eph <index> <term>): trie node
#   residue: ... ;; "without" the rest of keys

# keypath format:
#
# To access pairs:
#   pairs <key> ...
#   pairs [residue] ...
#
# E.g. the following pattern {person: (%layer rest_ name: name_ age: age_)} :
#
# ... Produces these keypaths:
#
#   pairs (value person) pairs (value name)       = name
#   pairs (value person) pairs (value age)        = age
#   pairs (value person) residue                  = rest
#
# They in turn collate to:
#
#  {pairs: {person: {pairs: {name: name, age: age, [residue]: rest}}}}
#
# To access items:
#   items (item 0)
#   items (item 1)
#   ...
#   items (range 0 2 0)
#   items (range 3 5 1)
#   items (eph 0 {x: a}) pairs x

# append pair value
# append pair key
# append item index
# append item range
# append eph item
# append residue

# record Plug, value : Term do
#   def call(prev)
#     value
#   end
# end

# require "benchmark"

# # Problem 1: we're emitting these in a flattened form.
# # Problem 2: we don't know how to order them.
# # Problem 3: we don't know how to follow flat keypaths (e.g. for $downs).

# # Can we compile flat keypaths to nonflat ones? E.g.:
# Term.of(:virtual, 1, 0, :self)
# Term.of(:virtual, 2, 0, :self)
# Term.of(:range, 1, 1)

# # For e.g. virtuals, we need to know its ordinal. We can give the
# # ordinal in the pattern matching engine; however, the ordinals won't
# # be successive -- so we'll necessarily need a normalization step.

# root = Term.of(:a, :b)
# trie = Term[]
#   .where({:virtual, 0}, eq: {1, 0, Term[].where(:self, eq: :qix)})
#   .where({:virtual, 1}, eq: {2, 0, Term[].where(:self, eq: :qix)})
#   .where({:range, 2}, eq: {1, 1, Term[{:q, :y}]})

# pp Keypath2.apply(trie, root)

# root = Term.of(foo: Term.set(:x, Term.set(1, 2, 3), :z))

# path0 = Term[:value, :foo, :key, Term.set(1, 2, 3), :key, 1, :eq]
# path1 = Term[:value, :foo, :key, Term.set(1, 2, 3), :key, 2, :eq]
# path2 = Term[:value, :foo, :key, Term.set(1, 2, 3), :key, 3, :eq]

# trie = Term[]
#   .where(path0.items, eq: Term.of(:qux))
#   .where(path1.items, eq: Term.of(:qix))
#   .where(path2.items, eq: Term.of(:qyx))

# pp Keypath2.apply(trie, Term.of(root))

# puts ML.display(Keypath1.map(root.upcast, path0.items, Plug.new(Term.of(:qux))))
# puts ML.display(Keypath1.map(root.upcast, path1.items, Plug.new(Term.of(:qix))))
# puts ML.display(Keypath1.map(root.upcast, path2.items, Plug.new(Term.of(:qyx))))

# 1. support nested entries source (enter key)

# require "benchmark"

# term = Term.of(x: 100, y: 200, z: 300)

# pp Plan.run(plan, term)

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

  # def self.fill(form, dict : Term::Dict, spec : Spec::Scan, *, keypath bkp = nil)
  #   feed = dict.items
  #   index = 0

  #   while spec.stride <= feed.size
  #     window = feed.begin.grow(spec.stride)
  #     cell = Result::ItemStrip.new(window, keypath: bkp ? bkp.value(Term.of(index)) : nil)
  #     response = form.fill(cell)
  #     return response unless response.is_a?(Form)

  #     if form == response
  #       # Same form returned => rejected
  #       feed = feed.move(1)
  #       index += 1
  #     else
  #       # Different form returned => accepted, give more
  #       feed = feed.move(spec.stride)
  #       index += spec.stride
  #     end

  #     form = response
  #   end

  #   form.close
  # end

  # private def self.dfs(dict, spec, form, bkp)
  #   if spec.depth0
  #     cell = Result::Item.new(Term.of(dict), keypath: bkp)
  #     form = form.fill(cell)
  #     return form unless form.is_a?(Form)
  #   end

  #   visit(dict, spec.part, keypath: bkp) do |item, keypath|
  #     cell = Result::Item.new(item, keypath)
  #     form = form.fill(cell)
  #     return form unless form.is_a?(Form)

  #     next if spec.maxdepth == 1
  #     next unless child = item.as_d?

  #     if spec.maxdepth.zero? # Unlimited
  #       form = dfs(child, spec, form, keypath)
  #     else
  #       form = dfs(child, spec.copy_with(maxdepth: spec.maxdepth - 1), form, keypath)
  #     end

  #     return form unless form.is_a?(Form)
  #   end

  #   form
  # end

  # def self.fill(form, dict : Term::Dict, spec : Spec::Dfs, *, keypath bkp = nil)
  #   form = dfs(dict, spec, form, bkp)
  #   form.is_a?(Form) ? form.close : form
  # end

  # private def self.iddfs1(dict, spec, form, bkp, depth)
  #   if depth.zero?
  #     visit(dict, spec.part, keypath: bkp) do |item, keypath|
  #       cell = Result::Item.new(item, keypath)
  #       return :full, form unless form.is_a?(Form) # Crystal goes crazy if this is absent
  #       form = form.fill(cell)
  #       return :full, form unless form.is_a?(Form)
  #     end
  #     return :next, form
  #   end

  #   # Carry out a vote for/against reaching the bottom.
  #   bottom = 0
  #   total = 0

  #   visit(dict, spec.part, keypath: bkp) do |item, keypath|
  #     total += 1

  #     unless child = item.as_d?
  #       bottom += 1
  #       next
  #     end

  #     signal, form = iddfs1(child, spec, form, keypath, depth - 1)

  #     case signal
  #     when :bot
  #       bottom += 1
  #     when :full
  #       return :full, form
  #     when :next
  #     else
  #       unreachable
  #     end
  #   end

  #   # Unanimous vote for bottom means we're at the bottom. Otherwise continue.
  #   bottom == total ? {:bot, form} : {:next, form}
  # end

  # private def self.iddfs(dict, spec, form, bkp)
  #   if spec.depth0
  #     cell = Result::Item.new(Term.of(dict), keypath: bkp)
  #     form = form.fill(cell)
  #     return form unless form.is_a?(Form)
  #   end

  #   maxdepth = spec.maxdepth.zero? ? nil : spec.maxdepth

  #   (0...maxdepth).each do |depth|
  #     signal, form = iddfs1(dict, spec, form, bkp, depth)

  #     case signal
  #     when :bot, :full
  #       break
  #     when :next
  #     else
  #       unreachable
  #     end
  #   end

  #   form
  # end

  # def self.fill(form, dict : Term::Dict, spec : Spec::Bfs, *, keypath bkp = nil)
  #   form = iddfs(dict, spec, form, bkp)
  #   form.is_a?(Form) ? form.close : form
  # end

  # def self.fill(form, dict : Term::Dict, spec : Spec::Entries, *, keypath bkp = nil)
  #   dict.each_entry do |key, value|
  #     cell = Result::Pair.new(key, value, bkp ? {bkp.key(key), bkp.value(key)} : nil)
  #     form = form.fill(cell)
  #     return form unless form.is_a?(Form)
  #   end

  #   form.close
  # end
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
  alias Any = Pass | Num | Sym | Boolean | Dict | Literal | Capture | Itemspart | Partition | EdgeUntyped | EdgeTyped | Choices | EitherSource | Keypool | Span | Tally | Bin | Both | Not | Layer | ScanFirst | ScanSource | ScanAll | ScanAllIsolated | DfsFirst | DfsSource | DfsAllIsolated | DfsAll | BfsFirst | BfsAllIsolated | BfsAll | PairRequired | PairOptional | PairAbsent | PairAbsentOrBad | PairValue | EntriesSource | EntriesAllIsolated | EntriesAll | Str | New | Keypath

  alias Bin = Add | Sub | Mul | Div | Tdiv | Mod | Pow | Map

  alias First = ScanFirst | DfsFirst | BfsFirst
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

  # TODO: split into Entry, Itemsonly, Pairsonly, do not use `Kind`.
  defcase Dict, min : Magnitude, max : Magnitude, sketch : UInt64, kind : Kind do
    enum Kind : UInt8
      Entry
      Itemsonly
      Pairsonly
    end

    def self.new : Dict
      new(Magnitude::INFINITY, Magnitude::INFINITY, 0, :entry)
    end

    def sizes : Range(Magnitude?, Magnitude?)
      Range.new(
        min == Magnitude::INFINITY ? nil : min,
        max == Magnitude::INFINITY ? nil : max,
        exclusive: false
      )
    end
  end
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
  defcase PairAbsentOrBad, key : Term, good : Any
  defcase PairValue, capture : Term, tail : Any

  alias Dfs = DfsFirst | DfsSource | DfsAllIsolated | DfsAll

  defcase DfsFirst, needle : Any, part : Search::Part, depth0 : Bool
  defcase DfsSource, needle : Any, part : Search::Part, depth0 : Bool
  defcase DfsAllIsolated, capture : Term, needle : Any, part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool
  defcase DfsAll, capture : Term, needle : Any, selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool

  alias Bfs = BfsFirst | BfsAllIsolated | BfsAll

  defcase BfsFirst, needle : Any, part : Search::Part, depth0 : Bool
  defcase BfsAllIsolated, capture : Term, needle : Any, part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool
  defcase BfsAll, capture : Term, needle : Any, selector : Set(Term), exterior : Set(Term), part : Search::Part, min : UInt8, max : UInt8, depth0 : Bool

  alias Entries = EntriesSource | EntriesAllIsolated | EntriesAll

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

  def dict
    @keypath
  end
end

module ::Ww::Term::M1::Operator
  record Behind, env : Env::Type, domains : Term::Dict, keypath : KeypathTip? do
    def []?(k : Term)
      env[k]?
    end

    def propose?(k : Term, v : Term) : Behind?
      if domain = domains[k]?
        return unless v.in?(domain)
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

    def restrict(k, vs) : Behind
      if domain = domains[k]?
        copy_with(domains: domains.with(k, domain.xsect(vs)))
      else
        copy_with(domains: domains.with(k, vs))
      end
    end

    def assign(k, v)
      copy_with(env: env.with(k, v))
    end

    def partition(selector)
      lhs_dom = Term[]
      lhs_env = env.transaction do |lhs_env|
        lhs_dom = domains.transaction do |lhs_dom|
          selector.each do |key|
            lhs_env.without(key)
            lhs_dom.without(key)
          end
        end
      end

      rhs_dom = Term[]
      rhs_env = Term::Dict.build do |rhs_env|
        domains.transaction do |rhs_dom|
          selector.each do |key|
            rhs_env.with(key, env[key])
            rhs_dom.with(key, domains[key]?)
          end
        end
      end

      {copy_with(env: lhs_env, domains: lhs_dom), copy_with(env: rhs_env, domains: rhs_dom)}
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

  def match(env, op : Dict, matchee : Term, ahead)
    unless dict = matchee.as_d?
      return Fb::Mismatch.new(env.env)
    end

    valid =
      case op.kind
      in .itemsonly? then dict.itemsonly?
      in .pairsonly? then dict.pairsonly?
      in .entry?
        true
      end

    valid &&= dict.size.in?(op.sizes)
    valid &&= dict.sketch_superset_of?(op.sketch)

    unless valid
      return Fb::Mismatch.new(env.env)
    end

    ahead.call(env)
  end

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

  def match(env, op : PairAbsent, matchee : Term, ahead)
    return Fb::Mismatch.new(env.env) unless dict = matchee.as_d?
    return Fb::Mismatch.new(env.env) if op.key.in?(dict)

    ahead.call(env)
  end

  def match(env, op : PairAbsentOrBad, matchee : Term, ahead)
    return Fb::Mismatch.new(env.env) unless dict = matchee.as_d?

    unless v = dict[op.key]?
      return ahead.call(env)
    end

    
    case fb = match(env, op.good, v, ahead)
    in Fb::Match
      return Fb::Mismatch.new(env.env)
    in Fb::Mismatch
      ahead.call(env)
    in Fb::Request
      return fb
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
      behind1 = behind1.restrict(capture, domain1)
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
            next unless op.exterior.all?  { |capture| exterior[capture]?.in?(interior[capture], nil) }

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

  def match(env, op : PairValue, matchee : Term, ahead)
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
      case fb = ahead.call(env.restrict(op.capture, dict))
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
    behind0 = Behind.new(env, domains: Term[], keypath: keypaths ? KeypathTip.new : nil)

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
  module Item
    # NOTE: you should add any new normal heads here if you want them to be
    # transparent for the specificity algorithm.
    ANY_NORMAL_HEAD = ML.parse1(<<-WWML
      (%any
        %singular
        %group
        %many
        %past
        %optional
        %plural %plural/min %plural/max
        %gap %gap/min %gap/max
        %slot)
    WWML
    )

    def self.normal(item : Term) : Term
      Term.of_case(item, engine: Term::M0) do
        match(:_symbol) do
          continue unless blank = item.blank?
          continue unless blank.poly?

          name = blank.name?

          Term.of(:"%plural", name ? {:"%capture", name} : nil, type: Term::Sym.new(blank.type.blank), min: blank.one? ? 1 : 0, max: SYM_INF)
        end

        match(
          {:"%partition", {:"%plural"}, :opts_},
          {:"%partition", {:"%plural/min"}, :opts_},
          {:"%partition", {:"%plural/max"}, :opts_},
          cues: {:"%plural", :"%plural/min", :"%plural/max"}
        ) do |opts|
          continue unless opts = Schemas::Plural.validated?(opts)

          opts.morph({0, item[0]})
        end

        match(
          {:"%partition", {:"%plural", :capture_}, :opts_},
          {:"%partition", {:"%plural/min", :capture_}, :opts_},
          {:"%partition", {:"%plural/max", :capture_}, :opts_},
          cues: {:"%plural", :"%plural/min", :"%plural/max"}
        ) do |capture, opts|
          continue unless opts = Schemas::Plural.validated?(opts)

          opts.morph({0, item[0]}, {1, {:"%capture", capture}})
        end

        match({:"%optional", :_, :body_}) do |body|
          item.morph({2, M1.normal(body)})
        end

        match({:"%group", :capture_, :_, :"_*"}, cue: :"%group") do |capture|
          item.transaction do |commit|
            commit.with(1, {:"%capture", capture})
            (2...item.size).each do |index|
              commit.with(index, normal(item[index]))
            end
          end
        end

        match({:"%partition", {:"%many", :capture_, :_, :"_*"}, :opts_}, cue: :"%many") do |capture, opts|
          continue unless opts = Schemas::Many.validated?(opts)

          opts.transaction do |commit|
            commit << :"%many" << {:"%capture", capture}
            (2...item.items.size).each do |index|
              commit.with(index, normal(item[index]))
            end
          end
        end

        match({:"%partition", {:"%past", :_, :"_*"}, :opts_}, cue: :"%past") do |opts|
          continue unless opts = Schemas::Past.validated?(opts)

          opts.transaction do |commit|
            commit << :"%past"
            (1...item.items.size).each do |index|
              commit.with(index, normal(item[index]))
            end
            commit.with(:greedy, false)
          end
        end

        match({:"%partition", {:"%past/max", :_, :"_*"}, :opts_}, cue: :"%past/max") do |opts|
          continue unless opts = Schemas::Past.validated?(opts)

          opts.transaction do |commit|
            commit << :"%past"
            (1...item.items.size).each do |index|
              commit.with(index, normal(item[index]))
            end
            commit.with(:greedy, true)
          end
        end

        match(
          {:"%gap", :measurer_},
          {:"%gap/min", :measurer_},
          {:"%gap/max", :measurer_},
          cues: {:"%gap", :"%gap/min", :"%gap/max"}
        ) do |measurer|
          item.morph({1, M1.normal(measurer)})
        end

        # NOTE: Currently we do not register %slot as a capture. And I don't think
        # there is any point in doing so.
        match({:"%slot", :_}, cue: :"%slot") do
          item
        end

        otherwise do
          Term.of(:"%singular", M1.normal(item))
        end
      end
    end

    # TODO: switch to using matchpis here and everywhere!
    def self.operator(item : Term, captures : Bag(Term)) : Operator::Item::Any
      Term.case(item, engine: Term::M0) do

        matchpi %[(%singular child_)], cue: :"%singular" do
          Operator::Item::Singular.new(M1.operator(child, captures))
        end

        match({:"%group", {:"%capture", :capture_}, :_, :"_*"}, cue: :"%group") do |capture|
          Operator::Item::Group.new(capture, item.items.move(2).map { |member| Item.operator(member, captures) })
        end

        match({:"%partition", {:"%many", {:"%capture", :capture_}, :_, :"_*", }, {min: :min0_, max: :max0_}}, cue: :"%many") do |capture, min0, max0|
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

        match({:"%partition", {:"%past", :_, :"_*", }, {min: :min0_, max: :max0_, greedy: :greedy_boolean}}, cue: :"%past") do |min0, max0, greedy|
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

  #   def self.bounds(item : Term)
  #     Term.case(item, engine: Term::M0) do
  #       match({:"%singular", :_}) do
  #         {Magnitude.new(1), Magnitude.new(1)}
  #       end

  #       match(
  #         {:"%partition", {:"%plural", {:"%capture", :_}}, :_},
  #         {:"%partition", {:"%plural"}, :_},
  #       ) do
  #         min = item[:min].to(Magnitude)
  #         max = item[:max] == SYM_INF ? Magnitude::INFINITY : item[:max].to(Magnitude)

  #         {min, max}
  #       end

  #       match({:"%gap", :_}) do
  #         {Magnitude.new(0), Magnitude::INFINITY}
  #       end

  #       match({:"%group", :_, :_, :"_*"}) do
  #         children = item.items.move(2)

  #         M1.bounds(children) { |child| bounds(child) }
  #       end

  #       # FIXME: min
  #       match({:"%many", :_, :_, :"_*"}) do
  #         children = item.items.move(2)

  #         min, max = M1.bounds(children) { |child| bounds(child) }

  #         min0 = Magnitude.new(1)
  #         max0 = Magnitude::INFINITY

  #         {min * min0, max * max0}
  #       end

  #       match({:"%optional", :_, :_}) do
  #         {Magnitude.new(0), Magnitude.new(1)}
  #       end
  #     end
  #   end
  end

  # def self.bounds(items : Enumerable(Term), & : Term -> {Magnitude, Magnitude})
  #   min = Magnitude.new(0)
  #   max = Magnitude.new(0)

  #   items.each do |item|
  #     min1, max1 = yield item
  #     min += min1
  #     max += max1
  #     # We could `break` here if b0 and e0 are Infinity, but that's pretty rare and probably
  #     # isn't worth it. Most often at least b0 is going to be known which requires a scan.
  #   end

  #   {min, max}
  # end

  module Pair
    def self.normal(key : Term, value : Term) : Term
      Term.of_case(value, engine: Term::M0) do
        match({:"%optional", :default_, :body_}, cue: :"%optional") do |default, body|
          Term.of(:"%pair/optional", key, default, M1.normal(body))
        end

        match({:"%-pair"}, cue: :"%-pair") do
          Term.of(:"%pair/absent", key)
        end

        match({:"%-pair", :good_}, cue: :"%-pair") do |good|
          Term.of(:"%pair/absent", key, M1.normal(good))
        end

        otherwise do
          Term.of(:"%pair/required", key, M1.normal(value))
        end
      end
    end

    # def self.bounds(pair : Term) : {Magnitude, Magnitude}
    #   Term.case(pair, engine: Term::M0) do
    #     match({:"%pair/optional", :_, :_, :_}) do
    #       {Magnitude.new(0), Magnitude.new(1)}
    #     end

    #     match({:"%pair/absent", :_}, {:"%pair/absent", :_, :_}) do
    #       {Magnitude::INFINITY, Magnitude.new(1)}
    #     end

    #     match({:"%pair/required", :_, :_}) do
    #       {Magnitude.new(1), Magnitude.new(1)}
    #     end
    #   end
    # end
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

  SYM_LT  = Term.of(:<)
  SYM_GT  = Term.of(:>)
  SYM_LTE = Term.of(:<=)
  SYM_GTE = Term.of(:>=)

  SYMS_CMP = {SYM_LT, SYM_GT, SYM_LTE, SYM_GTE}
  SYMS_LTX = {SYM_LT, SYM_LTE}

  SYM_INF = Term.of(:∞)

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
    Term.of_case(pattern, engine: Term::M0) do
      # Convert blanks to their corresponding %-nodes.
      match({:"%literal", :_}) { Term.of({:"%pass"}) }
      match({:"%literal", :_number}) { Term.of({:"%number", :_}) }
      match({:"%literal", :_string}) { Term.of({:"%string"}) }
      match({:"%literal", :_symbol}) { Term.of({:"%symbol"}) }
      match({:"%literal", :_boolean}) { Term.of({:"%boolean"}) }
      match({:"%literal", :_dict}) { Term.of({:"%dict"}) }

      # Named blanks are transformed into %let which we then recurse upon.
      # The recursion is done to perform further reductions (since we're
      # not rewriting here we must recurse explicitly).
      match(:_symbol) do
        continue unless blank = pattern.unsafe_as_sym.blank?
        continue unless blank.single?
        continue unless name = blank.name?

        tail = Term::Sym.new(blank.type.blank)

        normal(Term.of(:"%let", name, tail))
      end

      match(:_number, :_string, :_boolean) do
        Term.of(:"%literal", pattern)
      end

      # Recurse into %let and annotate the capture.
      match({:"%let", :capture_, :successor_}, cue: :"%let") do |capture, successor|
        Term.of(:"%let", {:"%capture", capture}, normal(successor))
      end

      # Recurse into %partition.
      match({ {:"%literal", :"%partition"}, :itemspart_, :pairspart_ }, cue: :"%partition") do |itemspart, pairspart|
        Term.of(:"%partition", normal(itemspart), normal(pairspart))
      end

      # Recurse into %pipe.
      match(
        {:"%pipe", {:+, :_number}, :successor_},
        {:"%pipe", {:-, :_number}, :successor_},
        {:"%pipe", {:*, :_number}, :successor_},
        {:"%pipe", {:/, :_number}, :successor_},
        {:"%pipe", {:div, :_number}, :successor_},
        {:"%pipe", {:mod, :_number}, :successor_},
        {:"%pipe", {:**, :_number}, :successor_},
        {:"%pipe", {:map, :_dict}, :successor_},
        {:"%pipe", :span, :successor_},
        {:"%pipe", :tally, :successor_},
        cue: :"%pipe",
        cues: {:+, :-, :*, :/, :div, :mod, :**, :map, :span, :tally}
      ) do |successor|
        pattern.morph({2, normal(successor)})
      end

      # %pipe composition.
      match({:"%pipe", :head_, :"_*"}, cue: :"%pipe") do |head|
        continue if pattern.size < 4 # %pipe + head_1 + head_2 + body

        body = Term::Dict.build do |commit|
          commit << :"%pipe"

          rest = pattern.items.move(2)
          rest.each { |item| commit << item }
        end

        normal(Term.of(:"%pipe", head, body))
      end

      # Recurse into %layer entries.
      match({:"%layer", :below_, :side_dict}, cue: :"%layer") do |below, side|
        pattern.transaction do |commit|
          commit.with(1, normal(below))

          nside = side.transaction do |nside|
            side.each_entry do |k, v|
              nside.with(k, Pair.normal(k, v))
            end
          end

          commit.with(2, nside)
        end
      end

      # (%layer _ k1: v1 k2: v2 ...) is a shorthand for (%layer _ {k1: v1 k2: v2 ...}).
      match({:"%partition", {:"%layer", :below_}, :pairs_}, cue: :"%layer") do |below, pairs|
        normal(Term.of(:"%layer", below, pairs))
      end

      # Recurse into %all.
      match({:"%all", :"_*"}, cue: :"%all") do
        Term::Dict.build do |commit|
          commit << :"%all"

          offshoots = pattern.items.move(1)
          offshoots.each { |offshoot| commit << normal(offshoot) }
        end
      end

      match({:"%any", :"_*"}, cue: :"%any") do
        pattern.morph({0, :"%any/literal"})
      end

      # Recurse into %any°.
      match({:"%any°", :"_*"}, cue: :"%any°") do
        Term::Dict.build do |commit|
          commit << :"%any/source"

          branches = pattern.items.move(1)
          branches.each { |branch| commit << normal(branch) }
        end
      end

      # Normalize (edge ...)
      match({:edge, :arg_symbol}, cue: :edge) do |arg|
        arg = arg.unsafe_as_sym
        continue unless blank = arg.blank?
        continue unless blank.single?

        case blank.type
        when .symbol? then edge = {:"%edge", :_symbol}
        when .string? then edge = {:"%edge", :_string}
        when .number? then edge = {:"%edge", :_number}
        when .any?    then edge = {:"%edge", :_}
        else
          continue
        end

        if name = blank.name?
          normal(Term.of(:"%let", name, edge))
        else
          Term.of(edge)
        end
      end

      match({:"%item°", :_, :"_*"}, cue: :"%item°") do
        pattern.transaction do |commit|
          commit.with(0, :"%items/source")
          (1...pattern.size).each do |index|
            commit.with(index, normal(pattern[index]))
          end
        end
      end

      match({:"%item", :_, :"_*"}, cue: :"%item") do
        pattern.transaction do |commit|
          commit.with(0, :"%items/first")
          (1...pattern.size).each do |index|
            commit.with(index, normal(pattern[index]))
          end
        end
      end

      match({:"%partition", {:"%items", :capture_, :_, :"_*"}, :opts_}, cue: :"%items") do |capture, opts|
        continue unless opts = Schemas::Items.validated?(opts)

        opts.transaction do |commit|
          commit << :"%items/all" << {:"%capture", capture}
          commit.concat(pattern.items.move(2)) { |item| normal(item) }
        end
      end

      match({:"%entries°", :k_, :v_}, cue: :"%entries°") do |k, v|
        pattern.transaction do |commit|
          commit.with(0, :"%entries/source")
          commit.with(1, normal(k))
          commit.with(2, normal(v))
        end
      end

      match({:"%partition", {:"%entries", :capture_, :k_, :v_}, :opts_}, cue: :"%entries") do |capture, k, v, opts|
        continue unless opts = Schemas::Entries.validated?(opts)

        opts.transaction do |commit|
          commit << :"%entries/all" << {:"%capture", capture} << normal(k) << normal(v)
        end
      end

      # %literal is normal.
      # %keypool is normal.
      # %not is normal.
      match(
        { {:"%literal", :"%literal"}, :_ },
        {:"%keypool", :"_*"},
        {:"%not", :_, :"_*"},
        cues: {:"%literal", :"%keypool", :"%not"}
      ) { pattern }

      # %keypath is normal.
      match({:"%keypath", :capture_}, cue: :"%keypath") do |capture|
        Term.of(:"%keypath", {:"%capture", capture})
      end

      # Arg-less %number is normal.
      match(
        {:"%number", {:"%literal", :_}},
        {:"%number", {:"%literal", {:whole, :_}}},
        cue: :"%number"
      ) { pattern }

      # Compile fixed-width %number into the corresponding bounds check. We do not
      # actually have fixed-width numbers. These kinds of patterns are often used
      # on the Crystal side to ensure we can safely e.g. to(Int32).
      match({:"%number", :type_symbol}, cue: :"%number") do |type|
        case type
        when Term.of(:u8)   then cls = UInt8
        when Term.of(:u16)  then cls = UInt16
        when Term.of(:u32)  then cls = UInt32
        when Term.of(:u64)  then cls = UInt64
        when Term.of(:u128) then cls = UInt128
        when Term.of(:i8)   then cls = Int8
        when Term.of(:i16)  then cls = Int16
        when Term.of(:i32)  then cls = Int32
        when Term.of(:i64)  then cls = Int64
        when Term.of(:i128) then cls = Int128
        else
          continue
        end

        Term.of(:"%number", cls.min, :<=, {:whole, :_}, :<=, cls.max)
      end

      # Binary %number, binary whole %number is normal if the op is valid.
      match(
        {:"%number", {:"%literal", :_}, :op_symbol, :_number},
        {:"%number", {:"%literal", {:whole, :_}}, :op_symbol, :_number},
        cue: :"%number",
        cues: {:_, :whole}
      ) { |op| op.in?(SYMS_CMP) ? pattern : continue }

      # Ternary %number, ternary whole %number is normal if the ops are valid.
      match(
        {:"%number", :_number, :lop_symbol, {:"%literal", :_}, :rop_symbol, :_number},
        {:"%number", :_number, :lop_symbol, {:"%literal", {:whole, :_}}, :rop_symbol, :_number},
        cue: :"%number",
        cues: {:_, :whole}
      ) { |lop, rop| lop.in?(SYMS_LTX) && rop.in?(SYMS_LTX) ? pattern : continue }

      # %edge is normal.
      match(
        {:"%edge", {:"%literal", :_symbol}},
        {:"%edge", {:"%literal", :_string}},
        {:"%edge", {:"%literal", :_number}},
        {:"%edge", {:"%literal", :_}},
        cue: :"%edge"
      ) { pattern }

      # %nonself is dissolved at normalization.
      match({:"%nonself", :arg_}, cue: :"%nonself") { |arg| normal(arg) }

      # %value is normal when its body is normal and its capture is registered.
      match({:"%value", :capture_, :body_}, cue: :"%value") do |capture, body|
        Term.of(:"%value", {:"%capture", capture}, normal(body))
      end

      # %leaf is normal when its body is normal
      match({:"%partition", {:"%leaf°", :body_}, :opts_}, cue: :"%leaf°") do |body, opts|
        continue unless opts = Schemas::LeafUnbounded.validated?(opts)

        opts.morph({0, :"%leaves/source"}, {1, normal(body)})
      end

      match({:"%partition", {:"%leaf", :body_}, :opts_}, cue: :"%leaf") do |body, opts|
        continue unless opts = Schemas::LeafUnbounded.validated?(opts)

        opts.morph({0, :"%leaves/first"}, {1, normal(body)})
      end

      match({:"%partition", {:"%leaves", :capture_, :body_}, :opts_}, cue: :"%leaves") do |capture, body, opts|
        continue unless opts = Schemas::LeafBounded.validated?(opts)

        opts.morph({0, :"%leaves/all"}, {1, {:"%capture", capture}}, {2, normal(body)})
      end

      # Leave %new's as-is. They are normalized and compiled when their
      # dependencies are known.
      match({:"%new", :_}, cue: :"%new") do
        pattern
      end

      # Expand (%string nonempty) into (%all (%not "") _string)
      matchpi %[(%string nonempty)] do
        normal(Term.of(:"%all", {:"%not", ""}, :_string))
      end

      # NOTE: you should insert new matchpis here. Below we have raw dict/literal
      # treatment; if you put your matchpis below they won't be reached.

      match({:"%partition", Term[], Term[]}) do
        Term.of(:"%literal", Term[])
      end

      match({:"%partition", :_, Term[]}) do
        Term::Dict.build do |commit|
          commit << :"%itemspart"
          pattern.items.each do |item|
            commit << Item.normal(item)
          end
        end
      end

      # E.g. {x: 100, y: 200} = (%layer () x: 100 y: 200)
      match({:"%partition", Term[], :_}) do
        normal(Term.of(:"%layer", Term[], pattern))
      end

      match({:"%partition", :itemspart_, :pairspart_}) do |itemspart, pairspart|
        Term.of(:"%partition", normal(itemspart), normal(pairspart))
      end

      otherwise { Term.of(:"%literal", pattern) }
    end
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

      match({:"%itemspart", :"_*"}, cue: :"%itemspart") do
        items = node.items
          .move(1)
          .map { |item| Item.operator(item, captures).as(Operator::Item::Any) }

        Operator::Itemspart.new(items)
      end

      match({:"%keypath", {:"%capture", :capture_}}, cue: :"%keypath") do |capture|
        Operator::Keypath.new(capture)
      end

      match({:"%keypool", :"_*"}, cue: :"%keypool") do
        keys = node.items.move(1)

        Operator::Keypool.new(keys.to_a)
      end

      match(Term[:"%dict", min: :min_number, max: :max_number, sketch: :sketch_number], cue: :"%dict") do |min, max, sketch|
        Operator::Dict.new(min.to(Magnitude), max.to(Magnitude), sketch.to(UInt64), :entry)
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

      match({:"%pair/absent", :key_}, cue: :"%pair/absent") do |key|
        Operator::PairAbsent.new(key)
      end

      match({:"%pair/absent", :key_, :good_}, cue: :"%pair/absent") do |key, good|
        Operator::PairAbsentOrBad.new(key, operator(good, captures))
      end

      match({:"%value", {:"%capture", :capture_}, :value_}, cue: :"%value") do |capture, value|
        Operator::PairValue.new(capture, operator(value, captures))
      end

      match({:"%pipe", {:+, :n_number}, :successor_}, cue: {:"%pipe", :"+"}) do |n, successor|
        Operator::Add.new(n.unsafe_as_n, operator(successor, captures))
      end

      match({:"%pipe", {:-, :n_number}, :successor_}, cue: {:"%pipe", :"-"}) do |n, successor|
        Operator::Sub.new(n.unsafe_as_n, operator(successor, captures))
      end

      match({:"%pipe", {:*, :n_number}, :successor_}, cue: {:"%pipe", :"*"}) do |n, successor|
        Operator::Mul.new(n.unsafe_as_n, operator(successor, captures))
      end

      match({:"%pipe", {:/, :n_number}, :successor_}, cue: {:"%pipe", :"/"}) do |n, successor|
        Operator::Div.new(n.unsafe_as_n, operator(successor, captures))
      end

      match({:"%pipe", {:div, :n_number}, :successor_}, cue: {:"%pipe", :"div"}) do |n, successor|
        Operator::Tdiv.new(n.unsafe_as_n, operator(successor, captures))
      end

      match({:"%pipe", {:mod, :n_number}, :successor_}, cue: {:"%pipe", :"mod"}) do |n, successor|
        Operator::Mod.new(n.unsafe_as_n, operator(successor, captures))
      end

      match({:"%pipe", {:**, :n_number}, :successor_}, cue: {:"%pipe", :"**"}) do |n, successor|
        Operator::Pow.new(n.unsafe_as_n, operator(successor, captures))
      end

      match({:"%pipe", {:map, :arg_dict}, :successor_}, cue: {:"%pipe", :"map"}) do |arg, successor|
        Operator::Map.new(arg.unsafe_as_d, operator(successor, captures))
      end

      match({:"%pipe", :span, :successor_}, cue: {:"%pipe", :"span"}) do |successor|
        Operator::Span.new(operator(successor, captures))
      end

      match({:"%pipe", :tally, :successor_}, cue: {:"%pipe", :"tally"}) do |successor|
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

      match({:"%any/literal", :"_*"}, cue: :"%any/literal") do
        choices = node.items.move(1).to_a
        choices.uniq!

        Operator::Choices.new(choices)
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

  # Returns `true` if *id* is one of node ids.
  def self.node?(id : Term::Sym) : Bool
    id.to(String).prefixed_by?('%')
  end

  def self.walk(node : Term, &fn : Term -> WalkDecision) : WalkDecision
    Term.case(node, engine: Term::M0) do
      match(
        { {:"%literal", :"%literal"}, :_ },
        { {:"%new", :_} },
        cues: {:"%literal", :"%new"}
      ) do
        fn.call(node)
      end

      # FIXME: Nasty. We should visit all dictionaries and let normal prevent us
      # from doing so using (%barrier) or something like that like it already does
      # for (%literal).
      match({:"%layer", :below_, :side_dict}, cue: :"%layer") do |below, side|
        dict = node.unsafe_as_d

        case fn.call(node)
        in .continue?
        in .skip? then return WalkDecision::Continue
        in .halt? then return WalkDecision::Halt
        end

        walk(below, &fn)

        side.each_entry do |_, value|
          case walk(value, &fn)
          in .continue?, .skip?
          in .halt?
            return WalkDecision::Halt
          end
        end

        WalkDecision::Continue
      end

      match({:"%partition", {:head_symbol, :"_*"}, :_}) do |head|
        unless node?(head.unsafe_as_sym)
          return WalkDecision::Continue
        end

        case fn.call(node)
        in .continue?
        in .skip? then return WalkDecision::Continue
        in .halt? then return WalkDecision::Halt
        end

        dict = node.unsafe_as_d
        dict.each_entry do |_, value|
          case walk(value, &fn)
          in .continue?, .skip?
          in .halt?
            return WalkDecision::Halt
          end
        end

        WalkDecision::Continue
      end

      otherwise { WalkDecision::Skip }
    end
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

  def self.operator(pattern : Term, *, normalize = true, fresh = false) : Operator::Any
    PATTERN_CACHE.fetch(pattern.unsafe_repr, fresh: fresh) do
      normal = normalize ? normal(pattern) : pattern
      captures = captures(normal)
      operator(normal, captures)
    end
  end

  private def self.matches0(op : Operator::Any, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Array(Term::Dict)
    Operator.matches(env, op, matchee, **kwargs)
  end

  private def self.match0?(op : Operator::Any, matchee : Term, *, env : Term::Dict = Term[], **kwargs) : Term::Dict?
    Operator.match?(env, op, matchee, **kwargs)
  end

  def self.matches(pattern : Term, matchee : Term, **kwargs)
    matches0(operator(pattern), matchee, **kwargs)
  end

  def self.match?(pattern, matchee, **kwargs)
    match0?(operator(pattern), matchee, **kwargs)
  end
end

module ::Ww::Term::M1

end

module ::Ww::Term::M1
  struct Metadata
    def initialize(@capture : Term, @plural : Bool, @body : Term?, @env : Term::Dict)
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

    # Applier must respond to `call(up0 : Term::Dict, up1 : Term::Dict, down : Term::Dict, my : Term::Dict, matchee0 : Term, body : Term) : {up1 : Term::Dict, matchee1 : Term}`
    def call(up0, up1, down, my, matchee0, body)
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

  #      { 
  #      { 
  #  1   { 
  #      {
  #  w   
  #  e   { 
  #  e   { 
  #  k   { TODO: implement the new Env
  #      { TODO: port editor interpret, eval ruleset
  #      { TODO: editor tests infra, automated editor "motion capture" with T?UI, replay
  #      {   as regression tests. Make sure all editor rules are hit & working by using TDD
  #      {   instead of outright copy-pasting the existing soma10 rules.
  #      { TODO: replay editor tests with multiple cursors
  # --- the editor runs, all editor tests pass, all editor rules hit
  # TODO: bring back Dict, sketch in an optimization pass
  # TODO: optimizer based on editor rules. editor is the main optimization client/benchmark!
  # TODO: finish refactoring of ahead into structs instead of procs.
  # TODO: move from Operator classes to structs & variably sized operators & "arena"
  #       everything must be as flat as possible. reduce indirections extremely!
  # TODO: refactors, split into files, etc. Done for the most part, although some edge cases
  #       are inevitably not going to be handled so well. But my rule is -- no test, no pest.
  #       If (or when?) our users hit edge case bugs with a reproducible example, then we're talking.


  # Layer-0 transform handles `self` props: applies transform and adds itself
  # to ctx1 (if requested).
  def self.transform0(ctx0, ctx1, bot, applier, node, matchee)
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
            ctx1, residue1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, residue0)
            residue1 &-= keys.items
            residue0.each_entry { |k, _| commit.without(k) }
            residue1.each_entry { |k, v| commit.with(k, v) }
          end
        end

        matchpi %[(ephemeral key_ default_)] do
          ctx1, value1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, default)
          matchee = matchee.with(key, value1)
        end

        matchpi %[(value key_)] do
          if ksucc = successor[:self]?
            key0, value0 = key, matchee[key]
            ctx1, key1 = transform0(ctx0, ctx1, bot, applier, ksucc, key0)
            matchee = matchee.without(key0).with(key1, value0)
            key = key1
          end

          next unless successor[:endpoint, :transform]?

          if (b = key.as_n?) && b.in?(matchee.items.bounds) && successor[:endpoint, :plural]?
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
          ctx1, values1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, values0)

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
          ctx1, values1 = transform0(ctx0, ctx1, bot, applier, successor.as_d, values0)

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
          elsif body = backspec[{capture}]?
            plural = true
          end

          meta = Metadata.new(capture, plural, body, env)
          trie = Term::Dict.enhance(trie, keypath.items, :endpoint, action: meta)
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

# TODO: bug: walk will walk into %keypath (and other nodes taking literals such as %pipe args, %captures, etc.)
#      FIX THIS !!!
# This can be fixed trivially on the normal-form-end by making each such literal
# argument wrapped in (%hold) . This way, all walk has to do is to avoid
# walking into %capture, %literal, and %hold; that's it.

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
        matchpi %[(%keypool keys_*)] do
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
  alias Specificity = {UInt32, UInt32, UInt32}

  # Specificity assigned to a top-level literal pattern such as `qux`, `(+ 1 2)`,
  # or a top-level literal alternative, e.g. `(%any 0 1 2)`.
  LITERAL_SPECIFICITY = {UInt32::MAX, 0u32, 0u32}

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
    if toplevel
      Term.case(normp) do
        # Recurse into top-level `%let`s.
        matchpi %[((%literal %let) _ successor_)] do
          return specificity(successor, toplevel: true)
        end

        # If we have a literal or %any at the top level, issue max specificity
        # and exit immediately.
        matchpi %[((%literal %literal) _)], %[(%any/literal _+)] do
          return LITERAL_SPECIFICITY
        end

        otherwise {}
      end
    end

    captures = Set(Term).new
    repeats = literals = restrictions = 0u32

    walk(normp) do |operator|
      Term.case(operator) do
        matchpi %[(%capture _)] do
          unless captures.add?(operator)
            repeats += 1
          end

          WalkDecision::Continue
        end

        matchpi %[((%literal %literal) _)], %[(%any/literal _+)]  do
          literals += 1

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

        # None of the itemspart nodes affect specificity.
        match({ Item::ANY_NORMAL_HEAD, :"_*"}) do
          WalkDecision::Continue
        end

        # %all takes max specificity of its offshoots.
        matchpi %[((%literal %all) offshoots_+)] do
          literals1, repeats1, restrictions1 = offshoots.items.max_of do |branch|
            specificity(branch, toplevel: false)
          end

          literals += literals1
          repeats += repeats1
          restrictions += restrictions1

          WalkDecision::Skip
        end

        # %any° takes min specificity of its branches.
        matchpi %[(%any/source branches_+)] do
          literals1, repeats1, restrictions1 = branches.items.min_of do |branch|
            specificity(branch, toplevel: false)
          end

          literals += literals1
          repeats += repeats1
          restrictions += restrictions1

          WalkDecision::Skip
        end

        otherwise do
          restrictions += 1

          WalkDecision::Continue
        end
      end
    end

    {literals, repeats, restrictions}
  end

  # :nodoc:
  module Head
    alias Any = Some | More | None

    record Some, term : Term
    record More
    record None

    def self.operator(candidate : Term) : Any
      Term.case(candidate) do
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
      matchpi %[((%literal %partition) itemspart_ _)] { head?(itemspart) }
      otherwise {}
    end
  end
end 

# Represents a pattern within a `Pset`. Has no expected use outside of `Pset`.
struct Pattern
  private alias O = Term::M1::Operator

  # Returns the index of this pattern. You are free to treat it as `Pset`-unique
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
# by `Pattern` and `Pset`.
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
class Pset
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
  # ```
  # pset = Pset.select(ML.parse1(%[(rule pattern_ body_)])) do |normp, env|
  #   # Do something with env[:body]
  #   # ...
  #  
  #   true # E.g. body is valid
  # end
  # ```
  def self.select(selector : Term, base : Term, & : Term, Term::Dict -> Bool?) : Pset
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

  private def response(neighbors : Slice(Pattern), matchee : Term) : Pr::Any
    neighbors.first_of?(&.response(matchee).as?(Pr::Pos)) || Pr::Neg.new
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
end


