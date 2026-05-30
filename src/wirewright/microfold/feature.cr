module Ww::Microfold
  alias Feature = Utility | Cond | Present | Absent | UpCue | DnCue | Item

  # Marks the style origin of a feature, i.e., the style string from which
  # it originated.
  #
  # NOTE: Order is important. Members later in the list have higher priority,
  # and can override styles from prior entries.
  enum StyleOrigin
    Preset
    Style
  end

  defrecord Utility, origin : StyleOrigin, id : String
  defcase Cond, filter : Filter, body : Feature
  defrecord Present
  defrecord Absent
  defrecord UpCue, name : Term::Sym
  defrecord DnCue, name : Term::Sym
  defrecord Item, payload : Utility

  alias CuedFeature = CuedAtom | CueCond
  alias CuedAtom = UncuedFeature | Present | Absent | UpCue | DnCue
  alias UncuedFeature = Utility | Item

  defcase CueCond, requirements : Pf::Set(UpCue | DnCue), body : CuedAtom

  alias Filter = UncuedFilter | InCue | HasCue
  alias UncuedFilter = PairPresent | PairEq | IsFirst | IsLast | IsOnly | IsPeriod | Not

  defrecord PairPresent, keypath : Slice(Term::Sym)
  defrecord PairEq, keypath : Slice(Term::Sym), value : Term::Sym
  defrecord IsFirst
  defrecord IsLast
  defrecord IsOnly
  defrecord IsPeriod, p : Int32
  defrecord InCue, name : Term::Sym
  defrecord HasCue, name : Term::Sym
  defcase Not, arg : UncuedFilter

  # :nodoc:
  #
  # ```text
  # feature
  #   <cond>
  #   <cue>
  #   <items>
  #   <presence>
  #   <utility>
  #
  # cond
  #   <filter> ":" <feature>
  #
  # cue
  #   "is-" <symbol>
  #   "cue-" <symbol>
  #
  # item
  #   "item-" <utility>
  #
  # presence
  #   "present"
  #   "absent"
  #
  # utility
  #   .+
  #
  # filter
  #   <not filter>
  #   <entry present filter>
  #   <entry eq filter>
  #   <relative filter>
  #   <cue filter>
  #
  # not filter
  #   "-" <filter>
  #
  # entry present filter
  #   <symbol list>
  #
  # entry eq filter
  #   <symbol list> "-is-" <symbol>
  #
  # relative filter
  #   "@first"
  #   "@last"
  #   "@only"
  #   "@period-" <nat>
  #
  # cue filter
  #   "cue-" <symbol>
  #   "has-" <symbol>
  #
  # symbol list
  #   <symbol>
  #   <symbol> "." <symbol list>
  #
  # nat
  #   [0-9]
  #   [1-9] <nat>
  #
  # symbol
  #   <symbolic>
  #   <symbolic> <symbol>
  # ```
  module FeatureGrammar
    extend self

    alias Ok = Outcome::Accepted
    alias Rej = Outcome::Rejected

    def feature(origin : StyleOrigin, seln : StringView) : Ok(Feature)
      outcome = Outcome.choice!(
        cond(origin, seln),
        cue(seln),
        item(origin, seln),
        presence(seln),
        utility(origin, seln),
      )

      outcome.map(&.as(Feature))
    end

    def cond(origin : StyleOrigin, seln : StringView) : Ok(Cond) | Rej
      l, m, r = seln.partition(':')
      return Outcome.rej if m.empty? # no ':'

      Outcome.bind(filter(l), feature(origin, r)) do |filter, body|
        Outcome.ok(Cond.new(filter, body))
      end
    end

    def cue(seln : StringView) : Ok(UpCue) | Ok(DnCue) | Rej
      l, m, r = seln.partition('-')
      return Outcome.rej if m.empty? # no '-'

      case l
      when "is"  then symbol(r).map { |name| UpCue.new(name) }
      when "cue" then symbol(r).map { |name| DnCue.new(name) }
      else
        Outcome.rej
      end
    end

    def item(origin : StyleOrigin, seln : StringView) : Ok(Item) | Rej
      l, m, r = seln.partition('-')
      return Outcome.rej if m.empty? # no '-'

      unless l == "item"
        return Outcome.rej
      end

      utility(origin, r).map { |arg| Item.new(arg) }
    end

    def presence(seln : StringView) : Ok(Present) | Ok(Absent) | Rej
      case seln
      when "present"
        Outcome.ok(Present.new)
      when "absent"
        Outcome.ok(Absent.new)
      else
        Outcome.rej
      end
    end

    def utility(origin : StyleOrigin, seln : StringView) : Ok(Utility)
      Outcome.ok(Utility.new(origin, seln.to_s))
    end

    def filter(seln : StringView) : Ok(Filter) | Rej
      outcome = Outcome.choice(
        not_filter(seln),
        relative_filter(seln),
        cue_filter(seln),
        entry_eq_filter(seln),
        entry_present_filter(seln),
      )

      outcome.map(&.as(Filter))
    end

    def uncued_filter(seln : StringView) : Ok(UncuedFilter) | Rej
      outcome = Outcome.choice(
        not_filter(seln),
        relative_filter(seln),
        entry_eq_filter(seln),
        entry_present_filter(seln),
      )

      outcome.map(&.as(UncuedFilter))
    end

    def not_filter(seln : StringView) : Ok(Not) | Rej
      return Outcome.rej unless seln.starts_with?('-')

      uncued_filter(seln.lskip(1)).map do |arg|
        Not.new(arg)
      end
    end

    def entry_present_filter(seln : StringView) : Ok(PairPresent) | Rej
      symbol_list(seln).map { |keypath| PairPresent.new(keypath) }
    end

    def entry_eq_filter(seln : StringView) : Ok(PairEq) | Rej
      l, m, r = seln.partition('/')
      return Outcome.rej if m.empty? # no '/'

      symbol_list(l).bind do |keypath|
        symbol(r).map do |value|
          PairEq.new(keypath, value)
        end
      end
    end

    def relative_filter(seln : StringView) : Ok(IsFirst) | Ok(IsLast) | Ok(IsOnly) | Ok(IsPeriod) | Rej
      unless seln.starts_with?('@')
        return Outcome.rej
      end

      case seln
      when "@first" then return Outcome.ok(IsFirst.new)
      when "@last"  then return Outcome.ok(IsLast.new)
      when "@only"  then return Outcome.ok(IsOnly.new)
      end

      l, m, r = seln.partition('-')
      return Outcome.rej if m.empty? # no '-'

      unless l == "@period"
        return Outcome.rej
      end

      nat(r).map { |n| IsPeriod.new(n) }
    end

    def cue_filter(seln : StringView) : Ok(InCue) | Ok(HasCue) | Rej
      l, m, r = seln.partition('-')
      return Outcome.rej if m.empty? # no '-'

      case l
      when "has" then symbol(r).map { |name| HasCue.new(name) }
      when "in"  then symbol(r).map { |name| InCue.new(name) }
      else
        Outcome.rej
      end
    end

    def nat(seln : StringView) : Ok(Int32) | Rej
      value = 0

      seln.each_char_with_index do |chr, index|
        return Outcome.rej unless digit = chr.to_i? # ⏏abc  1⏏abc

        if index > 0 && value.zero?
          return Outcome.rej if digit.zero? # 0⏏000...
        end

        # ⏏100  1⏏00  12⏏3
        begin
          value = value * 10 + digit
        rescue OverflowError
          return Outcome.rej
        end
      end

      Outcome.ok(value)
    end

    def symbol_list(seln : StringView) : Ok(Slice(Term::Sym)) | Rej
      l, m, r = seln.partition('.')
      if m.empty? # no '.'
        return symbol(l).map { |sym| Slice[sym] }
      end

      symbol(l).bind do |head|
        symbol_list(r).map do |rest|
          rest.prepend(head)
        end
      end
    end

    def symbol(seln : StringView) : Ok(Term::Sym) | Rej
      seln.each_char do |chr|
        unless ML::Rune.new(chr).symbolic?
          return Outcome.rej
        end
      end

      Outcome.ok(Term::Sym.new(seln.to_s))
    end
  end

  private def each_feature_view(style : StringView, & : StringView ->) : Nil
    loop do
      l, m, r = style.partition(&.whitespace?)

      unless l.empty?
        yield l
      end

      break if m.empty?

      style = r
    end
  end

  # Represents a sequence of Microfold *features* along with some indexing metadata.
  # A feature is a parsed, space-delimited fragment of the style string, e.g.:
  # "... ⏏bg-neutral-500⏏ ... ⏏hover:text-neutral-500⏏".
  #
  # See also: `Feature`.
  struct FeatureSeq
    # Returns the feature content of this set.
    getter content : Slice(Feature)

    # Returns the set of node pairspart keys this feature set refers to.
    getter referred_keys : Pf::Set(Term::Sym)

    # Returns `true` if this feature set includes a feature referring to the location
    # of the node (e.g., `@first`, `@period-3`).
    getter? refers_to_location : Bool

    # Returns `true` if this feature set contains the `membrane` utility.
    getter? cue_membrane : Bool

    # Returns `true` if this feature set contains the `leaf` utility.
    getter? leaf : Bool

    # :nodoc:
    def initialize(@content, @referred_keys, @refers_to_location, @cue_membrane, @leaf)
    end

    def self.new(content : Slice(Feature), cue_membrane : Bool, leaf : Bool) : FeatureSeq
      referred_keys = Pf::Kit.stack_array(Term::Sym, 4)
      refers_to_location = false

      content.each do |feature|
        walk(feature) do |entity|
          case entity
          when PairPresent, PairEq
            referred_keys << entity.keypath.first
          when IsFirst, IsLast, IsOnly, IsPeriod
            refers_to_location = true
          end
        end
      end

      new(content, referred_keys.to_pf_set, refers_to_location, cue_membrane, leaf)
    end

    private def self.walk(feature : Utility | Present | Absent | UpCue | DnCue, &fn : Feature | Filter ->)
      fn.call(feature)
    end

    private def self.walk(feature : Cond, &fn : Feature | Filter ->)
      fn.call(feature)
      walk(feature.filter, &fn)
      walk(feature.body, &fn)
    end

    private def self.walk(filter : PairPresent | PairEq | IsFirst | IsLast | IsOnly | IsPeriod | InCue | HasCue, &fn : Feature | Filter ->)
      fn.call(filter)
    end

    private def self.walk(feature : Item, &fn : Feature | Filter ->)
      fn.call(feature)
      walk(feature.payload, &fn)
    end

    private def self.walk(filter : Not, &fn : Feature | Filter ->)
      fn.call(filter)
      walk(filter.arg, &fn)
    end

    def +(other : FeatureSeq) : FeatureSeq
      FeatureSeq.new(
        content + other.content,
        referred_keys + other.referred_keys,
        refers_to_location? || other.refers_to_location?,
        cue_membrane? || other.cue_membrane?,
        leaf? || other.leaf?,
      )
    end
  end

  private def features(origin : StyleOrigin, style : StringView) : Outcome::Accepted(FeatureSeq)
    Outcome.accumulate do |acc|
      content = Pf::Kit.stack_array(Feature)
      cue_membrane = false
      leaf = false

      each_feature_view(style) do |seln|
        case seln
        when "membrane"
          # You can't say e.g. `hover:membrane` because then you'd be able to
          # say `in-error:membrane` which would in turn make membrane-ness
          # depend on the order in which cues  arrive to the node, which
          # is implementation-defined.
          cue_membrane = true
          next
        when "leaf"
          leaf = true
          next
        end

        feature_out = FeatureGrammar.feature(origin, seln)
        feature = acc.unwrap(feature_out)
        content << feature
      end

      # Sort by specificity to make sure more specific features are applied later.
      # Since the sort is stable, user-order is preserved in runs of features with
      # the same specificity.
      content.sort_by! { |feature| specificity(feature) }

      feature_seq = FeatureSeq.new(content.to_unsafe_readonly_slice!, cue_membrane, leaf)
      Outcome.ok(feature_seq)
    end
  end

  private def specificity(filter : PairEq) : {Int32, Int32}
    {1, 0}
  end

  private def specificity(filter : Filter) : {Int32, Int32}
    {0, 1}
  end

  private def specificity(feature : Cond) : {Int32, Int32}
    eqv0, cond0 = specificity(feature.filter)
    eqv1, cond1 = specificity(feature.body)

    {eqv0 + eqv1, cond0 + cond1}
  end

  private def specificity(feature : Feature) : {Int32, Int32}
    {0, 0}
  end

  # Parses *style* and returns the corresponding set of features.
  def features(origin : StyleOrigin, style : String) : Outcome::Accepted(FeatureSeq)
    features(origin, style.view)
  end

  # :ditto:
  def features(codex : Codex, origin : StyleOrigin, style : String) : Outcome::Accepted(FeatureSeq)
    codex.feature_cache.put_if_absent({origin, style}) do
      features(origin, style)
    end
  end
end
