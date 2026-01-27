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

# TODO: REMOVE (this particular include is a big wart on the face of the project!)
include Ww

# TODO: REMOVE
module ::Ww::M1::Search
  enum Part : UInt8
    Items
    Pairs
    Entries
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

# TODO: The stuff below is horrendous and must be rewritten. Most of it is just
# architectural overhead. Yes, we need a PatternSet. No, we don't need Pattern.
# Nor Pr. Same goes for ruleset which is defined elsewhere. My gosh.

# Represents a pattern within a `PatternSet`. Has no expected use outside of `PatternSet`.
struct Pattern
  # Returns the index of this pattern. You are free to treat it as `PatternSet`-unique
  # identifier of this pattern.
  getter index : UInt32

  # Returns the underlying M1 operator.
  getter operator : M1next::Op::Any

  # :nodoc:
  def initialize(@index : UInt32, @operator : M1next::Op::Any)
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
# around `M1next::Op`) and organizing them for efficient response
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
    abstract def of_pattern?(pattern : Term, normp : M1next::Normp) : T?

    # Extracts a key term from a matchee. If indeterminate, patterns with
    # a determinate key are all going to be skipped.
    abstract def of_matchee?(matchee : Term) : T?
  end

  # The default key implementation, uses `M1next.head?`.
  module Key::Head
    extend Key(Term)

    def self.of_pattern?(pattern : Term, normp : M1next::Normp) : Term?
      M1next.head?(normp)
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
  def self.select(selector : Term, bases : Enumerable(Term), key keymod : Key(T) = Key::Head, & : M1next::Normp, Term::Dict -> Bool?) : PatternSet(T) forall T
    seen = Set(Term).new

    keyed = {} of T => Array(Int32)
    headless = [] of Int32

    patterns = [] of Pattern
    specificities = [] of M1next::Specificity

    bases.each do |base|
      base.each_item_unordered do |item|
        envs = M1next.matches(selector, item)
        envs.each do |env|
          next unless pattern = env[:pattern]?
          next unless seen.add?(pattern)

          index = seen.size - 1

          normp = M1next.normal(pattern)

          specificity = M1next.specificity(normp)
          specificities << specificity

          operator = M1next.operator(normp)

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

  def each_candidate(matchee : Term, & : M1next::Op::Any, UInt32 ->)
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

module Ww::M1
  struct ShapeIndex
    def initialize(@patterns : Slice(M1next::Op::Any))
    end

    def self.build(patterns : Enumerable(Term))
      {new(patterns.to_readonly_slice { |pattern| M1next.operator(pattern) }), (0u32...patterns.size).to_a}
    end

    def decompose(matchee : Term) : Pf::USet32
      Pf::USet32.transaction do |commit|
        @patterns.each_with_index do |pattern, index|
          next unless M1next.probably_matches?(pattern, matchee)

          commit << index.to_u32
        end
      end
    end
  end
end
