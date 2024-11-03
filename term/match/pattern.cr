# TODO: remove `-`, `.[]`, `Surrounds...`, they are probably going to break the future
# Crystal compiler; test on upstream 1.15
#
# TODO: use more "monolithic", inline patterns such as IsDict[<various properties e.g. ItemsBounds, PairsBounds, itemsonly?, pairsonly?, sketch superset: ?>]
#       for better coherency & cache friendliness. Most of them can be packed into
#       a byte or so. This goes hand-in-hand with Chain inlining: no heap, Chain
#       is always the container for things like `IsDict`. They must not appear standalone.
#
# TODO: these patterns should really be a bunch of extremely compact storage + a module
# (Pattern.self?) that has the functions on it, e.g. `match?`. no OOP here, it's not
# necessary at this level.
#
# TODO: remove simplification from here when parseout optimizer exists. It will do
# a much better job.
#
# TODO: use constraint satisfaction for ItemSequence etc. Ideally rewrite the whole
# engine as a "guided" constraint solver rather than pattern matching engine. Instead
# of analyzing all possible assignments it will only look at those that the matchee
# term can provide, which helps restrict the search; so even naive implementations
# will do since the search space is usually small.
#
# TODO: ideally (specifically in conjunctive patterns) the "pending patterns" queue
# must be global, i.e. created once per match at the very top; it must hold unresolved
# pattern progress (i.e. pattern alongside matchee) that we sift through so that
# patterns that can be solved first are solved first and others are solved later,
# helping to solve third ones etc... an "opening process" like graph exploration,
# and this queue is the "open" queue.

struct Ww::Term
  abstract class Match::Pattern
    def -(other : Pattern)
      DynChain[self, other]
    end

    def match?(term : Term) : Term::Dict?
      return unless row = match?(Term[], term)

      env, _ = row
      env
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
    end

    def match?(env : Term::Dict, term : ITerm) : {Term::Dict, Term}?
      match?(env, term.upcast)
    end

    def simplify : Pattern
      self
    end

    def simple : Pattern
      pattern0 = as(Pattern)

      while true
        pattern1 = pattern0.simplify

        if pattern0 == pattern1
          return pattern1
        end

        pattern0 = pattern1
      end
    end

    def contains?(query : Symbol) : Bool
      false
    end
  end

  module Match::Pattern::SurroundsNothing
    # Smart constructor.
    def [] : Pattern
      new
    end
  end

  module Match::Pattern::SurroundsData
    # Smart constructor.
    def [](*args, **kwargs) : Pattern
      new(*args, **kwargs)
    end
  end

  module Match::Pattern::SurroundsTerm
    # Smart constructor. Will not necessarily construct `self`.
    def [](object : Term) : Pattern
      new(object)
    end

    # :ditto:
    def [](object : ITerm) : Pattern
      self[object.upcast]
    end

    # :ditto:
    def [](object : Nil) : Pattern
      Pass[]
    end
  end

  # Noop. Omitted whenever possible.
  #
  # Passes the input term without changing it or the environment.
  class Match::Pass < Match::Pattern
    extend SurroundsNothing

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      {env, term}
    end

    def_equals_and_hash
  end

  # Literal equality.
  #
  # Passes the input term only if it is equal to *term*.
  class Match::Literal < Match::Pattern
    extend SurroundsTerm

    def initialize(@term : Term)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      @term == term ? {env, term} : nil
    end

    def_equals_and_hash @term
  end

  # Type check.
  #
  # Passes the input term only if its type is *type* or its subtype.
  class Match::IsA < Match::Pattern
    def initialize(@type : TermType)
    end

    # Smart constructor. Will not necessarily construct `IsA`.
    def self.[](object : TermType) : Pattern
      object.any? ? Pass[] : new(object)
    end

    # :ditto:
    def self.[](object : Nil) : Pattern
      Pass[]
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      term.type.subtype?(@type) ? {env, term} : nil
    end

    def_equals_and_hash @type
  end

  class Match::IsEdge < Match::Pattern
    def initialize(@type : TermType)
    end

    # Smart constructor.
    def self.[](object : TermType) : Pattern
      unless object.in?(TermType::Any, *ML::EDGE_ALLOWED_DEFAULT)
        raise ArgumentError.new
      end

      new(object)
    end

    # :ditto:
    def self.[](object : Nil) : Pattern
      Pass[]
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      ML.edge?(term, allowed: @type.any? ? ML::EDGE_ALLOWED_DEFAULT : {@type}) ? {env, term} : nil
    end

    def_equals_and_hash @type
  end

  class Match::IsNat < Match::Pattern
    extend SurroundsNothing

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      n = term.unsafe_as_n
      n.natural? ? {env, term} : nil
    end

    def_equals_and_hash
  end

  class Match::Compare < Match::Pattern
    enum Op : UInt8
      Lt
      Lte
      Gt
      Gte
    end

    def initialize(@arg : Term::Num, @op : Op)
    end

    def self.[](arg : Term::Num, op : Op) : Pattern
      new(arg, op)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      n = term.unsafe_as_n

      case @op
      in .lt?  then n < @arg ? {env, term} : nil
      in .lte? then n <= @arg ? {env, term} : nil
      in .gt?  then n > @arg ? {env, term} : nil
      in .gte? then n >= @arg ? {env, term} : nil
      end
    end

    def_equals_and_hash @arg, @op
  end

  # Sketch likeness check.
  #
  # Unsafely assumes the input term is a dictionary.
  #
  # Passes the input dictionary only if its sketch is a superset of
  # the matched sketch.
  class Match::SketchSupersetOf < Match::Pattern
    def initialize(@sketch : UInt64)
    end

    # Smart constructor. Will not necessarily construct `SketchSupersetOf`.
    def self.[](sketch : UInt64) : Pattern
      sketch.zero? ? Pass[] : new(sketch)
    end

    # :ditto:
    def self.[](object : Nil) : Pattern
      Pass[]
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      term.unsafe_as_d.sketch_superset_of?(@sketch) ? {env, term} : nil
    end

    def_equals_and_hash @sketch
  end

  # :nodoc:
  abstract class Match::Bounds < Match::Pattern
    UNBOUNDED = -1

    def initialize(@min : Int32 = UNBOUNDED, @max : Int32 = UNBOUNDED)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      measurement = measure(term.unsafe_as_d)

      return unless @min == UNBOUNDED || @min <= measurement
      return unless @max == UNBOUNDED || @max >= measurement

      {env, term}
    end

    def +(other : self)
      self.class.new(
        min: UNBOUNDED.in?(@min, other.@min) ? UNBOUNDED : @min + other.@min,
        max: UNBOUNDED.in?(@max, other.@max) ? UNBOUNDED : @max + other.@max,
      )
    end

    def_equals_and_hash @min, @max
  end

  # Dictionary items bounds check. Unsafely assumes the input is a dictionary.
  #
  # Passes the dictionary through if its items are in bounds. Otherwise fails.
  class Match::ItemsBounds < Match::Bounds
    extend SurroundsData

    def self.zero : ItemsBounds
      self[eq: 0]
    end

    def self.[](*, eq n : Int32)
      self[min: n, max: n]
    end

    def measure(dict : Term::Dict) : Int32
      dict.items.size
    end
  end

  # Dictionary pairs bounds check. Unsafely assumes the input is a dictionary.
  #
  # Passes the dictionary through if its pairs are in bounds. Otherwise fails.
  class Match::PairsBounds < Match::Bounds
    extend SurroundsData

    def self.zero : PairsBounds
      self[eq: 0]
    end

    def self.[](*, eq n : Int32)
      self[min: n, max: n]
    end

    def measure(dict : Term::Dict) : Int32
      dict.pairs.size
    end
  end

  # Dictionary items selection. Unsafely assumes the input is a dictionary.
  #
  # Passes the items of the input dictionary.
  class Match::JustItems < Match::Pattern
    extend SurroundsNothing

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      dict = term.unsafe_as_d

      {env, dict.items.collect.upcast}
    end

    def_equals_and_hash
  end

  # Dictionary pairs selection. Unsafely assumes the input is a dictionary.
  #
  # Passes the pairs of the input dictionary.
  class Match::JustPairs < Match::Pattern
    extend SurroundsNothing

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      dict = term.unsafe_as_d

      {env, dict.pairs.upcast}
    end

    def_equals_and_hash
  end

  # Strips `(hold _)` off an input term, otherwise passes the input term unchanged.
  class Match::Release < Match::Pattern
    extend SurroundsNothing

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      ML.hold?(term) ? {env, term.unsafe_as_d[1]} : {env, term}
    end

    def_equals_and_hash
  end

  # Saves the input term under *name* within the environment.
  #
  # Passes the input term unchanged if successful. Fails if *name* is already
  # assigned to a different value.
  class Match::Assign < Match::Pattern
    extend SurroundsTerm

    def initialize(@name : Term)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      return unless env1 = env.unify?(@name, term)

      {env1, term}
    end

    def_equals_and_hash @name
  end

  # Wraps the input term in a single-item itemsonly dictionary and saves it under
  # *name* within the environment.
  #
  # Passes the input term unchanged if successful. Fails if *name* is already
  # assigned to a different value.
  class Match::AssignSingleton < Match::Pattern
    extend SurroundsTerm

    def initialize(@name : Term)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      return unless state1 = env.unify?(@name, term.pack(0))

      {state1, term}
    end

    def_equals_and_hash @name
  end

  # Introduces a dependency from the environment (e.g. on another part of the pattern).
  class Match::Dep < Match::Pattern
    extend SurroundsTerm

    def initialize(@name : Term)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      return unless value = env[@name]?
      return unless term == value

      {env, value}
    end

    def_equals_and_hash @name
  end

  # Attempts to lookup *key* in the input term. Unsafely assumes the input term is
  # a dictionary.
  #
  # Passes the value of the key if successful. Fails if key is absent.
  class Match::At < Match::Pattern
    extend SurroundsTerm

    def initialize(@key : Term)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      return unless value = term.unsafe_as_d.at?(@key)

      {env, value}
    end

    def_equals_and_hash @key
  end

  # Attempts to lookup *key* in the input term. Unsafely assumes the input term
  # is a dictionary.
  #
  # Passes the value of the key if successful. Passes *default* if key is absent.
  class Match::AtDefault < Match::Pattern
    extend SurroundsData

    def initialize(@key : Term, @default : Term)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      {env, term.unsafe_as_d.at(@key, default: @default)}
    end

    def_equals_and_hash @key, @default
  end

  class Match::KeyWhitelist < Match::Pattern
    extend SurroundsData

    def initialize(@keys : Set(Term))
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      dict = term.unsafe_as_d
      return unless 0 <= dict.size <= @keys.size

      dict.each_entry { |k, _| return unless k.in?(@keys) }

      {env, term}
    end

    def_equals_and_hash @keys
  end

  class Match::Blacklist < Match::Pattern
    extend SurroundsData

    def initialize(@blacklist : Set(Term))
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      term.in?(@blacklist) ? nil : {env, term}
    end

    def_equals_and_hash @blacklist
  end

  class Match::Pair < Match::Pattern
    def initialize(@source : Term::Sym, @value : Pattern)
    end

    def self.[](source : Term::Sym, value : Pass) : Pattern
      value
    end

    def self.[](source : Term::Sym, value : Pattern) : Pattern
      new(source, value)
    end

    def simplify
      Pair.new(@source, @value.simple)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      return unless key = env[@source]?
      return unless value = term[key]?

      @value.match?(env, value)
    end

    def_equals_and_hash @source, @value
  end

  # Scanning is done out of order.
  # Unsafely assumes the incoming term is a dictionary.
  # Unsafely assumes size checks are made on items of the incoming dictionary.
  abstract class Match::ItemScanner < Match::Pattern
    abstract def subpatterns : Enumerable(Pattern)

    def contains?(query : Symbol) : Bool
      subpatterns.any? &.contains?(query)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      pending = nil

      # Enrich `env` from dictionary items
      dict = term.unsafe_as_d
      dict.each_item_with_index do |item, index|
        next unless index.in?(0...subpatterns.size) # trailing Pass[]es

        subpattern = subpatterns.unsafe_fetch(index)
        unless response = subpattern.match?(env, item)
          pending ||= Deque({Term, Int32}).new
          pending << {item, index}
          next
        end

        env, _ = response
      end

      # Retry matching items in pending until:
      #   1. No pending items
      #   2. All pending items fail to match
      failures = 0
      while true
        return env, term unless pending
        return env, term if pending.empty?
        return if failures == pending.size

        item, index = pending.shift
        subpattern = subpatterns.unsafe_fetch(index)
        unless response = subpattern.match?(env, item)
          pending << {item, index}
          failures += 1
          next
        end

        env, _ = response
        failures = 0
      end

      {env, term}
    end
  end

  class Match::ItemScan(N) < Match::ItemScanner
    def initialize(@subpatterns : StaticArray(Pattern, N)) forall N
    end

    # Constructs a `Chain` with the given *subpatterns*.
    def self.[](*subpatterns : Pattern) : Pattern
      new(subpatterns.map(&.as(Pattern)).to_static_array)
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    def simplify : Pattern
      ItemScan(N).new(@subpatterns.map(&.simple.as(Pattern)))
    end

    def_equals_and_hash @subpatterns
  end

  class Match::DynItemScan < Match::ItemScanner
    extend SurroundsData

    def initialize(@subpatterns : Array(Pattern))
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    def simplify : Pattern
      subpatterns = @subpatterns.map(&.simple.as(Pattern))

      # Remove trailing passes (if any).
      if ending = subpatterns.rindex { |pattern| !pattern.is_a?(Pass) }
        subpatterns = subpatterns[0..ending]
      else
        subpatterns.clear
      end

      {% begin %}
        case subpatterns.size
        when 0 then Pass[]
        {% for size in (1..7) %}
        when {{size}}
          ItemScan[{{ (0...size).map { |index| "subpatterns[#{index}]".id }.splat }}]
        {% end %}
        else
          DynItemScan.new(subpatterns)
        end
      {% end %}
    end

    def_equals_and_hash @subpatterns
  end

  class Match::PairScan < Match::Pattern
    extend SurroundsData

    def initialize(@name : Term, @filter : Pattern)
    end

    def simple : Pattern
      PairScan.new(@name, @filter.simple)
    end

    private def keysub(a, b)
      a1 = a
      a.each_entry do |k, v|
        next unless k.in?(b)
        a1 = a1.without(k)
      end
      a1
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      dict = term.unsafe_as_d

      selection = Term[].transaction do |commit|
        dict.each_pair do |k, v|
          next unless row = @filter.match?(env, v)
          env1, _ = row
          commit.with(k, keysub(env1, env))
        end
      end

      return if selection.empty? # One or more

      {env.with(@name, selection), term}
    end

    def_equals_and_hash @name, @filter
  end

  # FIXME: ItemSequence match results depends on order of elements but shouldn't,
  # not really, because ItemScan does not depend on order and Mux does not depend
  # on order (both are "conjunctives" or "sum operators" on envs). ItemSequence
  # is also a sum operator on envs of its subordinate matches.
  class Match::ItemSequence < Match::Pattern
    extend SurroundsData

    abstract struct Element
      substruct One, pattern : Pattern do
        def simple : One
          One.new(pattern.simple)
        end

        def contains?(query : Symbol) : Bool
          pattern.contains?(query)
        end
      end

      substruct PluralLet, name : Term, pattern : Pattern do
        def simple : PluralLet
          PluralLet.new(name, pattern.simple)
        end

        def contains?(query : Symbol) : Bool
          pattern.contains?(query)
        end
      end

      substruct LitSeq, guard : Pattern, ref : Term::Sym do
        def simple : LitSeq
          LitSeq.new(guard.simple, ref)
        end

        def contains?(query : Symbol) : Bool
          query == :lit_seq || guard.contains?(query)
        end
      end

      substruct ManyGroup, name : Term, sequence : Array(Element) do
        def simple : ManyGroup
          ManyGroup.new(@name, sequence.map(&.simple))
        end

        def contains?(query : Symbol) : Bool
          sequence.any? &.contains?(query)
        end
      end

      substruct ManyEvenly, guard : Pattern, contenders : Array(Term::Sym) do
        def can_be_empty? : Bool
          contenders.none? { |contender| (blank = contender.blank?) && blank.one? }
        end

        def unify?(state, stash)
          stash.split(contenders.size, empty: can_be_empty?) do |division, index|
            contender = contenders[index]
            next unless blank = contender.blank?
            next unless name = blank.name?
            return unless state = state.unify?(name.upcast, division)
          end

          state
        end

        def simple : ManyEvenly
          ManyEvenly.new(guard.simple, contenders)
        end

        def contains?(query : Symbol) : Bool
          guard.contains?(query)
        end
      end

      substruct Gap, threshold : Pattern do
        def simple : Gap
          Gap.new(threshold.simple)
        end

        def contains?(query : Symbol) : Bool
          threshold.contains?(query)
        end
      end
    end

    def initialize(@elements : Array(Element))
    end

    def contains?(query : Symbol) : Bool
      @elements.any? &.contains?(query)
    end

    # Performs specialization based on element types.
    #
    # - `ItemSequence(One) -> DynItemScan (-> ItemScan(N) if N <= 8)`.
    def special : Pattern
      if @elements.all?(&.is_a?(Element::One)) && @elements.none?(&.contains?(:lit_seq))
        return DynItemScan[@elements.map(&.as(Element::One).pattern)]
      end

      self
    end

    def simplify : Pattern
      ItemSequence[@elements.map(&.simple)].special
    end

    private def match?(env, pattern : Pattern, term : Term)
      return unless response = pattern.match?(env, term)
      env, _ = response
      env
    end

    private def keysub(a, b)
      a1 = a
      a.each_entry do |k, v|
        next unless k.in?(b)
        a1 = a1.without(k)
      end
      a1
    end

    # TODO: this algorithm is bad bad bad, not only because of poor expected
    # performance but also because it's DFS whereas a human user would expect
    # parallel progress on all matches, e.g. in xs_* ys_* zs_* we'd expect
    # balance and convergence on dict//3 rather than hard-coding contenders.
    # It could potentially be done by using a queue, scheduling all elements,
    # and going through the queue in steps, each element making roughly the
    # same amount of progress on each step, thus simulating parallel growth
    # of elements. When elements hit each other's boundaries they can move
    # themselves or shift? E.g.
    #
    #     1 2 3 4 5 6 7 8 9 10
    # 1   xs ys zs
    # 2   xs  ys zs
    # 3   xs  ys  zs
    # 4   xs  ys  zs
    # 5   xs   ys  zs
    # 6   xs   ys   zs
    # 7   xs   ys   zs
    # 8   xs    ys   zs
    # 9   xs    ys    zs
    # 10  xs    ys    zs
    # 11  xs    ys    zs
    # ...
    #
    # Maximize the amount of items captured for all elements. Essentially on
    # each element step we make a decision: EXTEND or KEEP. Which branch is taken
    # depends on whether we grow after EXTEND or not. Since we're maximizing for
    # everyone every element must make sure to not shrink any other one. Elements
    # can also SHIFT, moving forward all next elements. So it's more like:
    private def match?(env0, elements, items, stashed = 0)
      if elements.empty? && items.empty?
        return env0
      end

      return unless element = elements.first?

      case element
      when Element::One
        return unless item = items.first?
        return unless env1 = match?(env0, element.pattern, item)

        match?(env1, elements[1..], items.move(1))
      when Element::PluralLet
        return unless item = items.first?
        return unless env1 = match?(env0, element.pattern, item)

        env1 = env1.with(element.name, Term.of({item}))

        match?(env1, elements[1..], items.move(1))
      when Element::ManyEvenly
        item = items.first?

        if element.can_be_empty?
          # If there are no more items or the current item doesn't pass the guard,
          # unify stash and proceed to the successor.
          unless item && match?(env0, element.guard, item)
            return unless env1 = element.unify?(env0, stash: items.begin.move(-stashed))
            return match?(env1, elements[1..], items)
          end

          # - If guard is not defined, then we're lazy (ask successor first).
          # - If guard is defined, then we're eager (ask ourselves first).

          if element.guard.is_a?(Pass)
            if env1 = match?(env0, elements[1..], items)
              return element.unify?(env1, stash: items.begin.move(-stashed))
            end
            match?(env0, elements, items.move(1), stashed + 1)
          else
            items = items.move(1)
            stashed += 1

            if env1 = match?(env0, elements, items, stashed)
              return env1
            end
            if env1 = match?(env0, elements[1..], items)
              return element.unify?(env1, stash: items.begin.move(-stashed))
            end

            # Lastly try to back off for the successor.
            items = items.move(-1)
            stashed -= 1

            if env1 = match?(env0, elements[1..], items)
              return element.unify?(env1, stash: items.begin.move(-stashed))
            end
          end
        else
          return unless item && match?(env0, element.guard, item)

          items = items.move(1)
          stashed += 1

          # Let the successor match the next item. If it does so successfully try
          # to unify with my stash. If that's successful, then fine, count as a match.
          if env1 = match?(env0, elements[1..], items)
            if env1 = element.unify?(env1, stash: items.begin.move(-stashed))
              return env1
            end
          end

          # If successor didn't match or unification didn't go as planned, try to
          # apply myself to the next item. If applied successfully, count that as
          # a match.
          match?(env0, elements, items, stashed)
        end
      when Element::ManyGroup
        # one or more
        sequence = element.sequence.to_readonly_slice
        capture = nil
        fragment = items.begin

        until items.end == fragment.end
          fragment = fragment.grow(1)

          # Path 1: add another item to the group
          next unless capture = match?(env0, sequence, fragment)

          # Path 2: match another repetition of the group.
          if env1 = match?(env0, elements, items.move(fragment.size))
            succ = env1[element.name]? || Term[]
            return env1.with(element.name, cat(Term[{keysub(capture, env0)}], succ))
          end

          # Path 3: match successor.
          if env1 = match?(env0, elements[1..], items.move(fragment.size))
            return env1.with(element.name, Term[{keysub(capture, env0)}])
          end
        end
      when Element::LitSeq
        value = env0[element.ref]

        # Select only refitems that pass the element's guard. Do nothing on `Pass`
        # guard to save time.
        refdict = Dict.build do |commit|
          value.ie.each_with_index do |refitem, index|
            next unless match?(env0, element.guard, refitem)

            commit.with(commit.size, refitem)
          end
        end

        refitems = refdict.items

        while refitem = refitems.first?
          # Quit if an item matched the guard but is not equal to the
          # reference item.
          return unless refitem == items.first?

          refitems = refitems.move(1)
          items = items.move(1)
        end

        # Match successor.
        match?(env0, elements[1..], items)
      when Element::Gap
        skipped = Term[0]

        while true
          unless response = element.threshold.match?(env0, skipped.upcast)
            break if items.empty? # No more items remain.
            skipped += 1
            items = items.move(1)
            next
          end

          env1, _ = response
          unless env1 = match?(env1, elements[1..], items)
            break if items.empty? # No more items remain.
            skipped += 1
            items = items.move(1)
            next
          end

          return env1
        end
      end
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      items = term.unsafe_as_d.items
      return unless env1 = match?(env, @elements.to_readonly_slice, items)

      {env1, term}
    end

    def_equals_and_hash @elements
  end

  abstract class Match::Chooser < Match::Pattern
    abstract def subpatterns : Enumerable(Pattern)

    def contains?(query : Symbol) : Bool
      subpatterns.any? &.contains?(query)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      subpatterns.each do |subpattern|
        next unless response = subpattern.match?(env, term)
        return response
      end
    end
  end

  class Match::Choice(N) < Match::Chooser
    def initialize(@subpatterns : StaticArray(Pattern, N)) forall N
    end

    def self.[](*subpatterns : Pattern) : Pattern
      new(subpatterns.map(&.as(Pattern)).to_static_array)
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    def simplify : Pattern
      Choice(N).new(@subpatterns.map(&.simple.as(Pattern)))
    end

    def_equals_and_hash @subpatterns
  end

  class Match::DynChoice < Match::Chooser
    extend SurroundsData

    def initialize(@subpatterns : Array(Pattern))
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    def simplify : Pattern
      subpatterns1 = [] of Pattern

      # 1. A simple DynChoice is a DynChoice whose subpatterns are all simple.
      # 2. A simple DynChoice does not contain other Choosers.
      # 3. A simple DynChoice does not contain Passes.
      @subpatterns.each do |subpattern|
        case subpattern = subpattern.simple
        when Chooser
          subpattern.subpatterns.each do |subsubpattern|
            subpatterns1 << subsubpattern
          end
        when Pass
        else
          subpatterns1 << subpattern
        end
      end

      {% begin %}
        case subpatterns1.size
        when 0 then Pass[]
        when 1 then subpatterns1[0]
        {% for size in (2..7) %}
        when {{size}}
          Choice[{{ (0...size).map { |index| "subpatterns1[#{index}]".id }.splat }}]
        {% end %}
        else
          DynChoice.new(subpatterns1)
        end
      {% end %}
    end

    def_equals_and_hash @subpatterns
  end

  abstract class Match::Chainer < Match::Pattern
    abstract def subpatterns : Enumerable(Pattern)

    def contains?(query : Symbol) : Bool
      subpatterns.any? &.contains?(query)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      subpatterns.each do |subpattern|
        return unless response = subpattern.match?(env, term)
        env, term = response
      end

      {env, term}
    end
  end

  # Passes the input term through a sequence of `N` subpatterns. Each successive
  # subpattern receives the input term and the environment from the preceding
  # subpattern's match. If any subpattern in the chain fails, the whole
  # chain fails.
  class Match::Chain(N) < Match::Chainer
    def initialize(@subpatterns : StaticArray(Pattern, N)) forall N
    end

    # Constructs a `Chain` with the given *subpatterns*.
    def self.[](*subpatterns : Pattern) : Pattern
      new(subpatterns.map(&.as(Pattern)).to_static_array)
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    def simplify : Pattern
      Chain(N).new(@subpatterns.map(&.simple.as(Pattern)))
    end

    def_equals_and_hash @subpatterns
  end

  # Same as `Chain(N)` but with `N` determined at runtime.
  class Match::DynChain < Match::Chainer
    def initialize(@subpatterns : Array(Pattern))
    end

    # Constructs a `Chain` with the given *subpatterns*.
    def self.[](*subpatterns : Pattern) : Pattern
      new([*subpatterns] of Pattern)
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    def simplify : Pattern
      subpatterns1 = [] of Pattern

      # 1. A simple DynChain is a DynChain whose subpatterns are all simple.
      # 2. A simple DynChain does not contain other Chainers.
      # 3. A simple DynChain does not contain Passes.
      @subpatterns.each do |subpattern|
        case subpattern = subpattern.simple
        when Chainer
          subpattern.subpatterns.each do |subsubpattern|
            subpatterns1 << subsubpattern
          end
        when Pass
        else
          subpatterns1 << subpattern
        end
      end

      {% begin %}
        case subpatterns1.size
        when 0 then Pass[]
        when 1 then subpatterns1[0]
        {% for size in (2..7) %}
        when {{size}}
          Chain[{{ (0...size).map { |index| "subpatterns1[#{index}]".id }.splat }}]
        {% end %}
        else
          DynChain.new(subpatterns1)
        end
      {% end %}
    end

    def_equals_and_hash @subpatterns
  end

  # :nodoc:
  #
  # Assumes `@subpatterns` is defined and is enumerable.
  abstract class Match::Muxer < Match::Pattern
    abstract def subpatterns : Enumerable(Pattern)

    def contains?(query : Symbol) : Bool
      subpatterns.any? &.contains?(query)
    end

    def match?(env : Term::Dict, term : Term) : {Term::Dict, Term}?
      pending = nil

      # Enrich `env` from subpatterns
      subpatterns.each do |subpattern|
        unless response = subpattern.match?(env, term)
          pending ||= Deque(Pattern).new
          pending << subpattern
          next
        end

        env, _ = response
      end

      # Retry matching items in pending until:
      #   1. No pending items
      #   2. All pending items fail to match
      failures = 0
      while true
        return env, term unless pending
        return env, term if pending.empty?
        return if failures == pending.size

        subpattern = pending.shift
        unless response = subpattern.match?(env, term)
          pending << subpattern
          failures += 1
          next
        end

        env, _ = response
        failures = 0
      end

      {env, term}
    end
  end

  # Forwards the input term to `N` subpatterns. If any subpattern fails `Mux` also
  # fails. If all patterns succeed `Mux` succeeds, passing the input term unchanged.
  # Subpatterns are executed in sequence as they were provided. Each subpattern may
  # modify the state, these modifications are propagated to/affect successive subpatterns.
  #
  # If `N` is unknown at compile-time use `DynMux`.
  class Match::Mux(N) < Match::Muxer
    def initialize(@subpatterns : StaticArray(Pattern, N)) forall N
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    # Constructs a `Mux` multiplexing the input term to the given *subpatterns*.
    def self.[](*subpatterns : Pattern) : Mux
      new(subpatterns.map(&.as(Pattern)).to_static_array)
    end

    def simplify : Pattern
      Mux(N).new(@subpatterns.map(&.simple.as(Pattern)))
    end

    def_equals_and_hash @subpatterns
  end

  # Same as `Mux` but with `N` determined at runtime. For this purpose `DynMux`
  # accepts a Crystal `Array` of subpatterns.
  class Match::DynMux < Match::Muxer
    def initialize(@subpatterns : Array(Pattern))
    end

    def self.[](subpatterns : Array(Pattern)) : Pattern
      case subpatterns.size
      when 0 then Pass[]
      when 1 then subpatterns.first
      else
        new(subpatterns)
      end
    end

    def subpatterns : Enumerable(Pattern)
      @subpatterns
    end

    def simplify : Pattern
      subpatterns1 = [] of Pattern

      # 1. A simple DynMux is a DynMux whose subpatterns are all simple.
      # 2. A simple DynMux does not contain other Chainers.
      # 3. A simple DynMux does not contain Passes.
      @subpatterns.each do |subpattern|
        case subpattern = subpattern.simple
        when Muxer
          subpatterns1.concat(subpattern.subpatterns)
        when Pass
        else
          subpatterns1 << subpattern
        end
      end

      {% begin %}
        case subpatterns1.size
        when 0 then Pass[]
        when 1 then subpatterns1[0]
        {% for size in (2..7) %}
        when {{size}}
          Mux[{{ (0...size).map { |index| "subpatterns1[#{index}]".id }.splat }}]
        {% end %}
        else
          DynMux.new(subpatterns1)
        end
      {% end %}
    end

    def_equals_and_hash @subpatterns
  end
end
