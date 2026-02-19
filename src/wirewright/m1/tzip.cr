module Ww::M1
  # A term plus a composable explanation of how that term was selected,
  # inspected, or synthesized (see `Log`).
  struct Tzip
    # Returns the focused term.
    getter term : Term

    # Returns the underlying log. `Log::None` means logging is disabled or this
    # tzip is unreachable.
    getter log : Log::Any

    def initialize(@term, @log)
    end

    # Constructs a pair of tzips for *key* and *value* stemming out of
    # the same *log*.
    def self.entry(log : Log::Any, key : Term, value : Term) : {Tzip, Tzip}
      {key(log, key), value(log, key, value)}
    end

    # Constructs an entry key tzip stemming out of the given *log*.
    def self.key(log : Log::Any, key : Term) : Tzip
      new(key, Log.append(log, Log::ExamineKey.new(key)))
    end

    # Constructs an entry value tzip stemming out of the given *log*.
    def self.value(log : Log::Any, key : Term, value : Term) : Tzip
      new(value, Log.append(log, Log::ExamineValue.new(key)))
    end

    # Joins the logs of two tzips if their terms are equal.
    def self.join?(a : Tzip, b : Tzip) : Tzip?
      return unless a.term == b.term

      Tzip.new(a.term, Log.join(a.log, b.log))
    end

    # :nodoc:
    def self.mapping(objects : Enumerable(T), handle : Log::Sealed, & : T, Int32 -> {Term | Tzip, Tzip}) : Tzip forall T
      mapping = Pf::Kit.stack_array({Log::ExamineKey | Log::ExamineValue, Log::Some}, 8)

      dict = Term::Dict.build do |commit|
        objects.each_with_index do |object, index|
          key, value = yield object, index

          if key.is_a?(Tzip)
            if log = key.log.as?(Log::Some)
              mapping << {Log::ExamineKey.new(key.term), log}
            end

            key = key.term
          end

          commit.with(key, value.term)

          # Value may not necessarily have a log, as in the following pattern:
          #
          #   (%items xs_ (%all x_symbol (%keypath path)))
          #
          # Here, the capture *path* (and its correponding mapping when %items
          # requests it) does not have an associated log even if logging is enabled.
          # Such cases we simply skip.
          if log = value.log.as?(Log::Some)
            mapping << {Log::ExamineValue.new(key), log}
          end
        end
      end

      new(Term.of(dict), Log::Mapping.new(handle, mapping.to_readonly_slice(&.itself)))
    end

    # :nodoc:
    def self.mapping(objects : Enumerable(T), handle : Log::None, & : T, Int32 -> {Term | Tzip, Tzip}) : Tzip forall T
      dict = Term::Dict.build do |commit|
        objects.each_with_index do |object, index|
          key, value = yield object, index
          case key
          in Tzip
            commit.with(key.term, value.term)
          in Term
            commit.with(key, value.term)
          end
        end
      end

      new(Term.of(dict), Log.none)
    end

    {% if flag?(:docs) %}
      # Synthesizes a dictionary tzip with the appropriate `Log::Mapping`. Uses the block
      # to convert *objects* to key-value pairs (also supplies the block with the index
      # of the current object).
      def self.mapping(objects : Enumerable(T), handle : Log::None, & : T, Int32 -> {Term | Tzip, Tzip}) : Tzip forall T
      end
    {% end %}

    # Queries the type of this tzip's term. See also: `type`.
    delegate :number?, :string?, :symbol?, :boolean?, :dict?, to: type

    # Returns the type of this tzip's term. See also: `Term#type`.
    def type : TermType
      @term.type
    end

    # Similar to `Term::Dict#[]?`.
    def []?(key) : Tzip?
      assert dict = @term.as_d?

      key = Term.of(key)
      return unless value = dict[key]?

      Tzip.new(value, Log.append(@log, Log::ExamineValue.new(key)))
    end

    # Similar to `Term::Dict#[]`.
    def [](key) : Tzip
      self[key]? || raise KeyError.new
    end

    # Similar to `Term::Dict#each_entry`.
    def each_entry(& : Tzip, Tzip ->)
      assert dict = @term.as_d?

      dict.each_entry { |key, value| yield *Tzip.entry(@log, key, value) }
    end

    # Similar to `Term::Dict#each_entry(Part::EntriesOrd)`.
    def each_entry_ord(& : Tzip, Tzip ->)
      assert dict = @term.as_d?

      dict.each_entry(in: Term::Dict.entries_ord) do |key, value|
        yield *Tzip.entry(@log, key, value)
      end
    end

    # Similar to `Term::Dict#each_entry(PairsOrd)`.
    def each_pair_ord(& : Tzip, Tzip ->)
      assert dict = @term.as_d?

      dict.each_entry(in: Term::Dict.pairspart_ord) do |key, value|
        yield *Tzip.entry(@log, key, value)
      end
    end

    # Same as `each_pair_ord`, but only yields pair values.
    #
    # This is more efficient than using `each_pair_ord` and discarding the key,
    # as its construction actually takes some effort and memory (key-descent
    # logs are somewhat pessimized since they're rare in practice).
    def each_pair_value_ord(& : Tzip ->)
      assert dict = @term.as_d?

      dict.each_entry(in: Term::Dict.pairspart_ord) do |key, value|
        yield Tzip.value(@log, key, value)
      end
    end

    # Similar to `Term::Dict.itemspart`.
    def itemspart : Tzip
      assert dict = @term.as_d?

      Tzip.new(Term.of(dict.itemspart), Log.append(@log, Log::ExamineItemspart.new))
    end

    # Similar to `Term::Dict.pairspart`.
    def pairspart : Tzip
      assert dict = @term.as_d?

      Tzip.new(Term.of(dict.pairspart), Log.append(@log, Log::ExaminePairspart.new))
    end

    def ref(key : Term) : Log::Sealed | Log::None
      log = Log.append(@log, Log::ExamineValue.new(key))

      Log.seal(Log.simplify(log))
    end

    enum Order : UInt16
      # Skips the entirety of items or pairs.
      Skip

      # Lexicographic (we use "lexical" because it's shorter) order. For items,
      # this means index-order. Pairs are sorted by key using `Term.compare`.
      #
      # Can be slow for large dicts (maybe > 10 000 pairs) due to the requirement of
      # sorting and comparison (also may allocate memory). The cost is generally
      # one-time, however, so this is used as the default where possible to produce
      # human-comprehensible order.
      Lexical

      # Items and pairs are emitted in memory order (`Term::Dict#each_entry`). This can
      # be significantly faster and will not allocate, but the order will not be human-
      # comprehensible (it is defined by the hash function, `Term.hashcode`).
      #
      # FIXME: Right now memory-order is not guaranteed to be stable. It looks stable in
      # practice, but the order of *collisions* is currently undefined, and thus depends
      # on insertion order. It remains future work to fix this by sorting collisions --
      # the current implementation of Pf::Map does not support this. Also, Pf::Map currently
      # does not enforce structural equality and thus is generally insertion order-dependent
      # as well: a group of insertions followed by a group of deletions, when mixing within
      # the groups, may result in a different order of entries.
      Memory

      # Parses *term* and returns a pair containing the resulting items and
      # pairs order (referred to as *itemsord* and *pairsord* elsewhere).
      # Returns `nil` on invalid input.
      #
      # See also: `m1.operator.leaf.order` in the doctool.
      def self.parse?(term : Term) : {Order, Order}?
        Term.case(term, engine: M0) do
          # |@ m1.operator.leaf.order
          #
          # |@pattern
          # items
          #
          # |@block
          # Searches in items in lexical key order, i.e., left to right (0 to itemsize).
          matchpi %{items}, cue: :items do
            {Lexical, Skip}
          end

          # |@ m1.operator.leaf.order
          #
          # |@pattern
          # pairs
          #
          # |@block
          # Searches in pairs in lexical order (sorted key order, lexically smallest
          # to lexically largest).
          matchpi %{pairs}, cue: :pairs do
            {Skip, Lexical}
          end

          # |@ m1.operator.leaf.order
          #
          # |@pattern
          # entries
          #
          # |@block
          # Searches in both items and pairs in lexical order.
          matchpi %{entries}, cue: :entries do
            {Lexical, Lexical}
          end

          otherwise { }
        end
      end

      # Same as `parse?`, but raises `ArgumentError` on invalid input instead
      # of returning `nil`.
      def self.parse(term : Term) : {Order, Order}
        parse?(term) || raise ArgumentError.new
      end
    end

    # Returns *n*th value in the itemspart of this tzip. Desired value order
    # as *n* grows is determined by *order*. Returns `nil` if no such value exists.
    def nthvi?(order : Order, n : Int32) : Tzip?
      assert dict = term.as_d?
      return unless n < dict.itemsize

      case order
      in .skip?
      in .lexical?
        Tzip.value(@log, Term.of(n), dict.items[n])
      in .memory?
        Tzip.value(@log, *dict.nth(n))
      end
    end

    # Returns *n*th value in the pairspart of this tzip. Desired value order
    # as *n* grows is determined by *order*. Returns `nil` if no such value exists.
    def nthvp?(order : Order, n : Int32) : Tzip?
      assert dict = term.as_d?
      return unless n < dict.pairsize

      case order
      in .skip?
      in .lexical?
        Tzip.value(@log, *dict.ordnth(dict.itemsize + n))
      in .memory?
        Tzip.value(@log, *dict.nth(dict.itemsize + n))
      end
    end

    # Decomposes a named symbol blank tzip into its name and type. Returns `nil`
    # for any other term.
    def blank? : {Tzip, Tzip}?
      return unless symbol = term.as_sym?
      return unless blank = symbol.blank?
      return unless blank.named?

      zname = Tzip.new(
        term: Term.of(blank.name),
        log: Log.append(@log, Log::ExamineBlankName.new),
      )

      ztype = Tzip.new(
        term: Term.of(blank.type.blank),
        log: Log.append(@log, Log::ExamineBlankType.new),
      )

      {zname, ztype}
    end

    # Pretends to insert *item* before *index*. The resulting tzip is of *item*,
    # with insertion logged.
    #
    # *ord* is needed to disambiguate multiple insertions before the same index.
    # See `Log::InsertItem`.
    def insert(item : Term, *, before index : UInt32, ord : UInt32) : Tzip
      assert dict = @term.as_d?
      assert index <= dict.itemsize

      Tzip.new(item, Log.append(@log, Log::InsertItem.new(index, ord, item)))
    end

    # Pretends to insert an entry with the given *key* and *value*. The resulting
    # tzip is of *value*, with insertion logged.
    def with(key : Term, value : Term) : Tzip
      assert dict = @term.as_d?
      {% unless flag?(:release) %}
        assert !dict.includes?(key)
      {% end %}

      Tzip.new(value, Log.append(@log, Log::InsertEntry.new(key, value)))
    end

    # Removes keys from this tzip. Keys are obtained by converting *objects*
    # to terms using the block. The resulting tzip is of the dict without
    # keys, with removal of keys logged.
    def without(objects : Indexable(T), & : T -> Term) : Tzip forall T
      assert dict0 = @term.as_d?

      log = @log
      removed = Pf::Kit.stack_array(Term, 8)

      dict1 = dict0.transaction do |commit|
        objects.each do |object|
          key = yield object

          size0 = commit.size
          commit.without(key)
          size1 = commit.size
          next unless size0 > size1

          assert size0 - 1 == size1
          next if log.is_a?(Log::None)

          removed << key
        end
      end

      if log.is_a?(Log::None) || removed.empty?
        return Tzip.new(Term.of(dict1), @log)
      end

      action = Log::ExamineResidue.new(removed.to_readonly_slice(&.itself))

      Tzip.new(Term.of(dict1), Log.append(@log, action))
    end

    # Adds to this tzip's logs that it's also a key in the given *table*.
    # Returns the modified tzip.
    def also_key_in(table : Tzip) : Tzip
      assert table.dict?
      {% unless flag?(:release) %}
        assert table.term.includes?(@term)
      {% end %}

      Tzip.new(@term, Log.join(@log, Log.append(table.log, Log::ExamineKey.new(@term))))
    end

    # Returns a view of the items in this tzip's itemspart.
    def items : ItemsView
      ItemsView.new(self)
    end

    alias WalkAlgorithm = Dfs | Bfs

    alias Dfs = DfsPreorder | DfsPostorder

    # Depth-first, myself first, children later.
    defrecord DfsPreorder,
      itemsord : Order,
      pairsord : Order,
      mindepth : UInt32,
      maxdepth : UInt32

    # Depth-first, children first, myself later.
    defrecord DfsPostorder,
      itemsord : Order,
      pairsord : Order,
      mindepth : UInt32,
      maxdepth : UInt32

    # Breadth-first.
    defrecord Bfs,
      itemsord : Order,
      pairsord : Order,
      mindepth : UInt32,
      maxdepth : UInt32

    # TODO: DfsInorder
    # TODO: DfsPreorderR
    # TODO: DfsInorderR
    # TODO: DfsPostorderR

    # :nodoc:
    defrecord DfsEnter, node : Tzip, depth : Int32
    # :nodoc:
    defrecord DfsItemIter, node : Tzip, depth : Int32, n : Int32
    # :nodoc:
    defrecord DfsPairIter, node : Tzip, depth : Int32, n : Int32
    # :nodoc:
    defrecord DfsLeave, node : Tzip, depth : Int32

    # :nodoc:
    def walk(alg : DfsPreorder | DfsPostorder, & : Tzip, Int32 ->) : Nil
      itemsord, pairsord = alg.itemsord, alg.pairsord

      stack = Pf::Kit.stack_array(DfsEnter | DfsItemIter | DfsPairIter | DfsLeave, 8)
      stack << DfsEnter.new(self, depth: 0)

      loop do
        break unless s = stack.pop?

        case s
        in DfsEnter
          next if s.depth > alg.maxdepth

          if alg.is_a?(DfsPreorder) && alg.mindepth <= s.depth
            yield s.node, s.depth
          end

          if s.node.dict?
            stack << DfsItemIter.new(s.node, s.depth, n: 0)
          else
            stack << DfsLeave.new(s.node, s.depth)
          end
        in DfsItemIter
          if value = s.node.nthvi?(itemsord, s.n)
            # Entry present.
            stack << DfsItemIter.new(s.node, s.depth, s.n + 1)
            stack << DfsEnter.new(value, s.depth + 1)
            next
          end

          # Entry absent.
          stack << DfsPairIter.new(s.node, s.depth, n: 0)
        in DfsPairIter
          if value = s.node.nthvp?(pairsord, s.n)
            # Entry present.
            stack << DfsPairIter.new(s.node, s.depth, s.n + 1)
            stack << DfsEnter.new(value, s.depth + 1)
            next
          end

          # Entry absent.
          stack << DfsLeave.new(s.node, s.depth)
        in DfsLeave
          if alg.is_a?(DfsPostorder) && alg.mindepth <= s.depth
            yield s.node, s.depth
          end
        end
      end
    end

    # :nodoc:
    def walk(alg : Bfs, & : Tzip, Int32 ->) : Nil
      (alg.mindepth..alg.maxdepth).each do |frontier|
        empty = true

        level = DfsPreorder.new(
          itemsord: alg.itemsord,
          pairsord: alg.pairsord,
          mindepth: frontier,
          maxdepth: frontier,
        )

        walk(level) do |node, depth|
          yield node, depth
          empty = false
        end

        break if empty
      end
    end

    {% if flag?(:docs) %}
      # Traverses this tzip's dict. The order and restrictions on the traversal
      # are given by *alg*. Yields each node visited along with its depth.
      def walk(alg : WalkAlgorithm, & : Tzip, Int32 ->) : Nil
      end
    {% end %}

    alias FlatSpec = Slice(FlatStep)
    alias FlatStep = KeyStep | KeyListStep | InItemsStep | InPairsStep | InEntriesStep | MergeStep(FlatStep)

    defrecord KeyStep, term : Term
    defrecord KeyListStep, terms : Slice(Term)
    defrecord InItemsStep
    defrecord InPairsStep
    defrecord InEntriesStep
    defrecord MergeStep(T), branches : Slice(Slice(T))

    # :nodoc:
    def flat(sink, step : KeyStep, steps : FlatSpec) : Nil
      assert dict?
      return unless value = self[step.term]?

      value.flat(sink, steps)
    end

    # :nodoc:
    def flat(sink, step : KeyListStep, steps : FlatSpec) : Nil
      assert dict = term.as_d?
      return unless step.terms.all?(&.in?(dict))

      step.terms.each do |key|
        value = self[key]
        value.flat(sink, steps)
      end
    end

    # :nodoc:
    def flat(sink, step : InItemsStep | InPairsStep | InEntriesStep, steps : FlatSpec) : Nil
      assert dict?

      if step.is_a?(InEntriesStep) || step.is_a?(InItemsStep)
        items.each(&.flat(sink, steps))
      end

      if step.is_a?(InEntriesStep) || step.is_a?(InPairsStep)
        each_pair_value_ord(&.flat(sink, steps))
      end
    end

    # :nodoc:
    def flat(sink, step : MergeStep(FlatStep), steps : FlatSpec) : Nil
      assert dict?

      step.branches.each do |branch|
        flat(sink, steps.present? ? branch + steps : branch)
      end
    end

    # :nodoc:
    def flat(sink, spec : FlatSpec) : Nil
      unless step = spec.first?
        sink << self
        return
      end

      return unless dict?

      flat(sink, step, spec + 1)
    end

    # Walks all paths that match *spec* through the focused term, collecting every
    # terminal it reaches. The resulting tzip is a list of such terminals.
    #
    # This method serves as the implementation of the `%flat` operator.
    def flat(spec : FlatSpec) : Tzip
      sink = Pf::Kit.stack_array(Tzip, 16)
      flat(sink, spec)

      # (%flat (_ n) ns_) <> {(ns): ()} means remove all pairs with key `n`.
      case @log
      in Log::Some then handle = Log.seal(sink, &.log)
      in Log::None then handle = Log.none
      end

      Tzip.mapping(sink, handle: handle) do |item, index|
        {Term.of(index), item}
      end
    end

    alias PluckSpec = Slice(PluckStep)
    alias PluckStep = KeyStep | KeyListStep | InItemsStep | InPairsStep | InEntriesStep

    # :nodoc:
    def pluck?(step : KeyStep, steps : PluckSpec) : Tzip?
      assert dict?

      return unless value0 = self[step.term]?
      return unless value1 = value0.pluck?(steps)

      handle = Log.seal(Log.simplify(@log))

      Tzip.mapping({ {step.term, value1} }, handle, &.itself)
    end

    # :nodoc:
    def pluck?(step : KeyListStep, steps : PluckSpec) : Tzip?
      assert dict?

      entries = Pf::Kit.stack_array({Term | Tzip, Tzip}, 4)

      step.terms.each do |key|
        return unless value0 = self[key]?
        return unless value1 = value0.pluck?(steps)

        entries << {Tzip.key(@log, key), value1}
      end

      return if entries.empty?

      Tzip.mapping(entries, handle: Log.seal(Log.simplify(@log)), &.itself)
    end

    # :nodoc:
    def pluck?(step : InItemsStep | InPairsStep | InEntriesStep, steps : PluckSpec) : Tzip?
      assert dict?

      entries = Pf::Kit.stack_array({Term | Tzip, Tzip}, 8)

      if step.is_a?(InEntriesStep) || step.is_a?(InItemsStep)
        items.each do |item0|
          next unless item1 = item0.pluck?(steps)

          # NOTE: Since we append items first, we can use entries.size instead of
          # maintaining a separate counter.
          entries << {Tzip.key(@log, Term.of(entries.size)), item1}
        end
      end

      if step.is_a?(InEntriesStep) || step.is_a?(InPairsStep)
        each_pair_ord do |key, value0|
          next unless value1 = value0.pluck?(steps)

          entries << {key, value1}
        end
      end

      return if entries.empty?

      Tzip.mapping(entries, handle: Log.seal(Log.simplify(@log)), &.itself)
    end

    # :nodoc:
    def pluck?(steps : PluckSpec)
      return self unless step = steps.first?
      return unless dict? # Trying to step into a literal results in nothing.

      pluck?(step, steps + 1)
    end

    # Traverses this tzip according to *spec*, retaining only those entries
    # whose paths fully match. The result preserves dictionary structure but
    # prunes away all non-matching branches. If nothing matches, returns
    # an empty dict.
    #
    # This method serves as the implementation of the `%pluck` operator.
    #
    # TODO: Since pluck can remove items (and therefore shift indices) it's currently
    # generating Mappings for everything; and Mappings aren't the cheapest solution,
    # to put it lightly. Maybe there's a better way to do it (at least in the common case).
    def pluck(spec : PluckSpec) : Tzip
      pluck?(spec) || Tzip.new(Term.of, Log.none)
    end

    def inspect(io)
      @term.inspect(io)
      io << "//"
      @log.inspect(io)
    end
  end

  # Similar to `Term::Dict::ItemsView` but for `Tzip`-wrapped dicts.
  struct Tzip::ItemsView
    include Indexable(Tzip)

    # Returns the original tzip (of dict).
    getter tzip : Tzip

    # Returns the begin index of this view.
    getter begin : UInt32

    # Returns the end index of this view (exclusive).
    getter end : UInt32

    # :nodoc:
    def initialize(@tzip : Tzip, @begin : UInt32, @end : UInt32)
      assert @tzip.type.dict?
      assert @begin <= @end
    end

    # :nodoc:
    def initialize(@tzip : Tzip)
      assert @tzip.type.dict?

      @begin = 0u32
      @end = @tzip.term.itemsize.to_u32
    end

    # :nodoc:
    def_change

    # Returns the number of items in this view.
    def size : Int32
      (@end - @begin).to_i
    end

    def unsafe_fetch(index : Int)
      @tzip[@begin + index]
    end

    # Returns an empty view before this one's beginning.
    def before_begin : ItemsView
      change(end: @begin)
    end

    # Returns an empty view after this one's end.
    def after_end : ItemsView
      change(begin: @end)
    end

    # Returns `true` if this view starts at the beginning of the underlying
    # dict's itemspart.
    def at_begin? : Bool
      @begin.zero?
    end

    # Returns `true` if this view starts at the end of the underlying
    # dict's itemspart.
    def at_end? : Bool
      unless @begin == @tzip.term.itemsize
        return false
      end

      assert @begin == @end

      true
    end

    # Returns `true` if this view ends with an item for which the block
    # returns `true`.
    def ends_with?(& : Tzip -> Bool) : Bool
      return false unless item = last?

      yield item
    end

    # Returns a view of the first item. If none, returns an empty view.
    def head : ItemsView
      empty? ? change(end: @begin) : change(end: @begin + 1)
    end

    # Returns a view of all items following the first one. If this view
    # is empty, returns an empty view.
    def rest : ItemsView
      empty? ? change(begin: @end) : change(begin: @begin + 1)
    end

    # Returns a view of all items preceding the last one. If this view
    # is empty, returns an empty view.
    def prior : ItemsView
      empty? ? change(end: @begin) : change(end: @end - 1)
    end

    # Returns a view of the last item. If none, returns an empty view.
    def tail : ItemsView
      empty? ? change(begin: @end) : change(begin: @end - 1)
    end

    # Returns a view of all items before this view's begin in the underlying
    # dict itemspart.
    def behind : ItemsView
      change(begin: 0u32, end: @begin)
    end

    # Returns a view of all items after this view's end in the underlying
    # dict itemspart.
    def ahead : ItemsView
      change(begin: @end, end: @tzip.term.itemsize.to_u32)
    end

    # Changes the begin and end of this view to *begin1*, *end1* (in absolute coordinates).
    def reshape(begin1 : UInt32, end1 : UInt32) : ItemsView
      assert begin1 <= end1 <= @tzip.term.itemsize

      change(begin: begin1, end: end1)
    end

    # Skips at most *n* items from left.
    def lskip(n : Int32) : ItemsView
      current = self
      n.times do
        break if current.head.empty?
        current = current.rest
      end
      current
    end

    # :ditto:
    def +(n : Int32) : ItemsView
      lskip(n)
    end

    # Skips at most *n* items from right.
    def rskip(n : Int32) : ItemsView
      current = self
      n.times do
        break if current.tail.empty?
        current = current.prior
      end
      current
    end

    # :ditto:
    def -(n : Int32) : ItemsView
      rskip(n)
    end

    # Returns a view of the first *n* items of this view.
    def first(n : Int32) : ItemsView
      assert n <= size

      change(end: @begin + n)
    end

    # Returns a view of the last *n* items of this view.
    def last(n : Int32) : ItemsView
      assert n <= size

      change(begin: @end - n)
    end

    # Returns the part of this view before and excluding *pivot*.
    #
    # *pivot* is set in relative coordinates: min `0` means `begin`, max
    # is `end`.
    def before(pivot : Int) : ItemsView
      assert 0 <= pivot <= size

      change(end: @begin + pivot.to_u32)
    end

    # Returns the part of this view after and including *pivot*.
    #
    # *pivot* is set in relative coordinates: min `0` means `begin`, max
    # is `end`.
    def starting_at(pivot : Int) : ItemsView
      assert 0 <= pivot <= size

      change(begin: @begin + pivot.to_u32)
    end

    # Constructs a log corresponding to this items view. The log stems
    # from `tzip`'s log and is annotated as `Log::ExamineRange`.
    def log : Log::Any
      Log.append(@tzip.log, Log::ExamineRange.new(@begin.to_u32, @end.to_u32, ord: 0u32))
    end

    # Constructs a span reference (see `Ref::Span`) corresponding to this
    # items view.
    def ref(*, ord : UInt32) : Log::Sealed | Log::None
      log = Log.append(@tzip.log, Log::ExamineRange.new(@begin.to_u32, @end.to_u32, ord: ord))

      Log.seal(Log.simplify(log))
    end

    # Returns a tzip of the items in this view (as a dict).
    def collect : Tzip
      iv = @tzip.term.items(@begin.to_i, @end.to_i)

      # FIXME: It is unclear whether `log` is enough here or whether we should
      # construct a mapping. Assuming disciplined use, `log` *is* enough.
      Tzip.new(Term.of(iv.collect), log)
    end

    # Yields all splits of this view into two subviews, starting with
    # an empty view and consuming one item at a time until full.
    def each_partition_lazy(& : ItemsView, ItemsView ->)
      (0..size).each do |size|
        yield before(size), starting_at(size)
      end
    end

    # Yields all splits of this view into two subviews, starting with
    # this view and releasing items until empty.
    def each_partition_greedy(& : ItemsView, ItemsView ->)
      (0..size).reverse_each do |size|
        yield before(size), starting_at(size)
      end
    end

    # Yields all splits of this view into two subviews, starting at *pivot*
    # and swaying back and forth.
    def each_partition_sway(pivot : Int, & : ItemsView, ItemsView ->)
      yield before(pivot), starting_at(pivot)

      # We then sway like pivot - 1, pivot + 1, pivot - 2, pivot + 2, etc...
      (1..size).each do |offset|
        if pivot - offset >= 0
          yield before(pivot - offset), starting_at(pivot - offset)
        end

        if pivot + offset <= size
          yield before(pivot + offset), starting_at(pivot + offset)
        end
      end
    end

    # Splits this view into *n* equally-sized (if possible, otherwise left-leaning)
    # subviews. Yields each subview followed by its index.
    #
    # *empty* can be specified to enable or disable emission of empty subviews.
    def each_chunk(n : Int32, *, empty : Bool = false, & : ItemsView, Int32 ->) : Nil
      unless n.positive?
        raise ArgumentError.new
      end

      return unless empty || size >= n

      step, rem = size.divmod(n)
      from = @begin
      n.times do |i|
        to = from + step
        to += 1 if i < rem
        yield change(begin: from, end: to), i

        from = to
      end
    end

    # Yields each possible split of this view with focus (middle part) of
    # size *n*. *n* can be zero, in which case the middle part will be empty
    # (but properly positioned!). The first and last block args are the left
    # and right parts (before and after mid), correspondingly.
    #
    # The block must return a boolean indicating whether the split was *accepted*
    # (`true`; thus moving forward by *n* or by 1 if *n* is zero) or *rejected*
    # (`false`; thus moving forward by 1). This powers subsequence search, whereby
    # you find a sequence of elements and then look at elements to the left
    # and right.
    def each_split(n : Int, & : ItemsView, ItemsView, ItemsView -> Bool)
      (0...size).slide_subrange_of(n) do |subrange|
        l = before(subrange.begin)
        focus = reshape(@begin + subrange.begin, @begin + subrange.end)
        r = starting_at(subrange.end)
        yield l, focus, r
      end
    end

    def pretty_print(pp)
      pp.list("Tzip[…", self, "…]")
    end

    def inspect(io)
      io << "Tzip[…"
      join(io, ", ")
      io << "…]"
    end
  end
end
