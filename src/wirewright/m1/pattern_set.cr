module Ww::M1
  # PatternSet is responsible for organizing (partitioning) M1 patterns for
  # efficient response to a matchee.
  #
  # That is, if you have many patterns and one matchee, PatternSet is expected
  # to help you match more efficiently than an O(N) scan over patterns; supposing
  # the patterns cooperate with PatternSet, meaning they're written in a way
  # that is friendly toward PatternSet (and structure discovery in general).
  #
  # Usually this means you shouldn't write patterns that are too cryptic. In other
  # words, the more "rigid" a pattern looks, the more can we do about it. Stuff like
  # `⟪x_number⟫` is the opposite of that, although we can still try searching for
  # `_number`s efficiently. The latter is expected of M1, though, not `PatternSet`.
  #
  # The exact stuff that PatternSet looks for in patterns is an implementation detail,
  # so I'm reluctant to write about it here at length. The most basic thing that
  # we do right now is look at the pattern's head (such as `+` in (`(+ a_ b_)`)) and
  # route only matchees with the matching head to the `+` bucket.
  class PatternSet(T)
    alias Bucket = Slice(Pattern)

    # :nodoc:
    defrecord Pattern, index : UInt32, op : M1::Op::Any

    # Includers are used to determine the indexing key; such a key must be something
    # that a pattern and all its possible matchees *necessarily share*. If the key is
    # indeterminate for a matchee, patterns requiring that key are not going to
    # be tested.
    module Key(T)
      # Extracts a key term from a pattern. Both its original (*pattern*) and
      # normal-form (*normp*) version are given. Returns `nil` if indeterminate;
      # in such case the pattern will be tested on all matchees.
      abstract def of_pattern?(pattern : Term, normp : M1::Normp) : T?

      # Extracts a key term from a matchee. If indeterminate, patterns with
      # a determinate key are all going to be skipped.
      abstract def of_matchee?(matchee : Term) : T?
    end

    # The default key implementation, uses `M1.head?`.
    module Key::Head
      extend Key(Term)

      def self.of_pattern?(pattern : Term, normp : M1::Normp) : Term?
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

      def empty? : Bool
        @map.empty?
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
    # *selector* for further handling by the block. The block returns `true`
    # to confirm that the pattern must be added to the set; or `false`/`nil`
    # to indicate that the pattern must be rejected.
    #
    # Yields patterns in their index order and **not** *base*-order. This means that
    # the index of the current yield will correspond to indices given later by `query`
    # and related. The index of the current iteration can thus be used as a
    # reference to the current pattern.
    #
    # ```
    # index = 0
    #
    # pset = PatternSet.select(ML.term(%[(rule pattern_ body_)]), base) do |normp, env|
    #   # Do something with env[:body]
    #   # ...
    #   # Current pattern index is *index*.
    #
    #   index += 1
    #   true # E.g. body is valid
    # end
    # ```
    def self.select(selector : Term, bases : Enumerable(Term), *, key keymod : Key(T) = Key::Head, discriminator : Term? = nil, & : M1::Normp, Term::Dict -> Bool?) : PatternSet(T) forall T
      keyed = {} of T => Array(Int32)
      headless = [] of Int32

      patterns = [] of Pattern
      specificities = [] of M1::Specificity

      bases.each do |base|
        base.each_item_unordered do |item|
          envs = M1.matches(selector, item)
          envs.each do |env|
            next unless pattern = env[:pattern]?

            if discriminator
              next unless pattern.type.dict?
              next unless pattern.itemsize == 2
              next unless discriminator == pattern[0]

              pattern = pattern[1]
            end

            index = patterns.size

            normp = M1.normal(pattern)
            specificity = M1.specificity(normp)
            specificities << specificity
            operator = M1.operator(normp)

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
      headless.sort! { |a, b| specificities[b] <=> specificities[a] }
      keyed.each do |_, bucket_indices|
        bucket_indices.sort! { |a, b| specificities[b] <=> specificities[a] }
      end

      okeyed = keyed.transform_values do |bucket_indices|
        bucket_indices.to_readonly_slice { |index| patterns[index] }
      end

      oheadless = headless.to_readonly_slice { |index| patterns[index] }

      PatternSet(T).new(KeyedMap(T).new(okeyed, keymod), oheadless)
    end

    # A version of `select` that selects from multiple bases.
    def self.select(selector : Term, *bases : Term, **kwargs, &)
      self.select(selector, bases, **kwargs) { |*args| yield *args }
    end

    # Block-less version of `select`.
    def self.select(*args, **kwargs)
      self.select(*args, **kwargs) { true }
    end

    struct Matcher
      include Term::Case::Matcher

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
        @pset.query(matchee, env: env).compact_map do |envs, index|
          next unless env_ = envs.single?

          {env_, @table[index]}
        end
      end
    end

    # Works like `Term.case`, but uses `PatternSet` as the backend.
    macro case(matchee, **kwargs, &block)
      ::Ww::Term.case({{matchee}}, matcher: ::Ww::M1::PatternSet::Matcher, {{kwargs.double_splat}}) {{block}}
    end

    # Returns `true` if there are no patterns in this set.
    def empty? : Bool
      @keyed.empty? && @unkeyed.empty?
    end

    # Returns `true` if there is at least one pattern in this set.
    def present? : Bool
      !empty?
    end

    # Yields operators and indices (see `.select`) of patterns that *probably*
    # match *matchee*.
    def each_candidate(matchee : Term, & : M1::Op::Any, UInt32 ->) : Nil
      bucket = @keyed.bucket?(matchee)
      bucket ||= Bucket.empty

      {bucket, @unkeyed}.each do |patterns|
        patterns.each do |pattern|
          next unless M1.probably_matches?(pattern.op, matchee)
          yield pattern.op, pattern.index
        end
      end
    end

    # Returns an iterator of operators and indices (see `.select`) of patterns that
    # *definitely* match *matchee*. *env* can be used to provide the initial env.
    def query(matchee : Term, *, env : Term::Dict = Term[]) : Iterator
      bucket = @keyed.bucket?(matchee)
      bucket ||= Bucket.empty

      bucket.each.chain(@unkeyed.each).compact_map do |pattern|
        next unless M1.probably_matches?(pattern.op, matchee)

        envs = M1.matches(env, pattern.op, matchee)
        next if envs.empty?

        {envs, pattern.index}
      end
    end

    # Yields operators and indices (see `.select`) of patterns that *definitely*
    # match *matchee*. *env* can be used to provide the initial env.
    def query(matchee : Term, & : Indexable(Term::Dict), UInt32 ->) : Nil
      each_candidate(matchee) do |op, index|
        envs = M1.matches(Term[], op, matchee)
        next if envs.empty?

        yield envs, index
      end
    end

    def probe?(matchee : Term) : Bool
      each_candidate(matchee) do |op, _|
        next unless M1.probe?(Term[], op, matchee)
        return true
      end

      false
    end
  end
end
