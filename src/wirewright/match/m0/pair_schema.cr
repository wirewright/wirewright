module Ww::M0
  # An object that helps you validate dictionary pairsparts. Acts as an M0 alternative
  # for pairspart patterns. For example, while in M1 you can simply `(... ¦ x⋮ 100)`
  # to create an optional pair, here in M0 without PairSchema the logic would have to
  # be encoded in Crystal. PairSchema helps to automate that, and is in a sense in
  # between: you still encode the constraints in Crystal, but yet, declaratively rather
  # than imperatively; once and in a reusable manner. The rest is automated away.
  #
  # TODO: remove in favor of `M0.schema` DSL
  struct PairSchema
    # :nodoc:
    alias Pair = RequiredPair | OptionalPair

    # :nodoc:
    record RequiredPair, key : Term, check : Check do
      def enriched?(dict : Term::Dict) : Term::Dict?
        return unless value = dict[key]?
        return unless check.satisfied?(value)
        dict
      end
    end

    # :nodoc:
    record OptionalPair, key : Term, check : Check, default : Term do
      def enriched?(dict : Term::Dict) : Term::Dict?
        unless value = dict[key]?
          return dict.with(key, default)
        end

        return unless check.satisfied?(value)

        dict
      end
    end

    # :nodoc:
    alias Check = MatchesAny | IntBounds

    # :nodoc:
    record MatchesAny, options : Array(Term) do
      def satisfied?(value : Term) : Bool
        options.any?(value)
      end
    end

    # :nodoc:
    record IntBounds, min : Term::Num, max : Term::Num do
      def satisfied?(value : Term) : Bool
        return false unless n = value.as_n?

        n.integer? && n.in?(min..max)
      end
    end

    def initialize
      @pairs = [] of Pair
      @predicates = [] of Term::Dict -> Bool
    end

    # Registers a predicate function: if all of *block*'s arguments are present in
    # the input term as keys, their values will be given to the block.
    macro where(&block)
      predicate do |%input|
        {% for arg in block.args %}
          {{arg.id}} = %input[{{arg.symbolize}}]? || next true
        {% end %}

        {{yield}}
      end
    end

    # A DSL-like interface to the construction of a pair schema.
    #
    # ```
    # M0::PairSchema.build do
    #   key :order, values: {:dfs, :bfs}
    #   key :min, values: 0...8, default: 0
    #   key :max, values: 1...8
    #   where { |min, max| min.as_n < max.as_n }
    # end
    # ```
    def self.build(&) : PairSchema
      with schema = new yield schema

      schema
    end

    private def check(range : Range)
      IntBounds.new(Term[range.begin], Term[range.end])
    end

    private def check(allowed : Tuple)
      MatchesAny.new([*allowed.map { |value| Term.of(value) }])
    end

    private def check(allowed : Enumerable)
      MatchesAny.new(allowed.map { |value| Term.of(value) })
    end

    # Registers a required *key* with an allowed set of *values* (an enumerable or a range).
    #
    # *key* must be a pairspart key. Itemspart keys will not work.
    def key(key, *, values) : Nil
      @pairs << RequiredPair.new(Term.of(key), check(values))
    end

    # Registers an optional *key* with an allowed set of *values* (an enumerable or a range).
    # If missing in the input term, will be assigned the value of *default*.
    #
    # *key* must be a pairspart key. Itemspart keys will not work.
    #
    # NOTE: *default* need not be one of *values*.
    def key(key, *, values, default) : Nil
      @pairs << OptionalPair.new(Term.of(key), check(values), Term.of(default))
    end

    # Registers a predicate function *fn* to run on the input term.
    #
    # See also: `where`.
    def predicate(&fn : Term::Dict -> Bool) : Nil
      @predicates << fn
    end

    # Validates *term* against the pairs in this schema and extends it with defaults
    # for the optional ones. Returns the resulting dictionary.
    def enriched?(term : Term) : Term::Dict?
      return unless dict0 = term.as_d?
      return unless dict0.pairsonly?

      good = dict0
      bad = dict0.transaction do |commit|
        @pairs.each do |pair|
          good = pair.enriched?(good) || return # Validation failed

          commit.without(pair.key)
        end
      end

      return unless bad.empty? # Extra pairs left.
      return unless @predicates.all?(&.call(dict0))

      good
    end
  end
end
