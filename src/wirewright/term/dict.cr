require "./dict/sketch"
require "./dict/histogram"
require "./dict/cookie"
require "./dict/summary"
require "./dict/small_map"
require "./dict/utermtrie32"
require "./dict/term_trie"

module Ww
  @[Term::Assoc(TermType::Dict, :unsafe_as_d)]
  class Term::Dict
    include Equality
    include AutoUpcast
    include TypeConversion

    EMPTY = new

    def initialize
      @utrie = UTermTrie32.empty
      @ttrie = TermTrie.empty
    end

    # :nodoc:
    def initialize(@utrie : UTermTrie32::Root, @ttrie : TermTrie::Root)
    end

    # Commits allow you to compose multiple edits into one, big edit of a dict.
    # Thus you avoid having to create many useless intermediate copies.
    class Commit
      def initialize(@state : Dict)
        @cookie = Cookie.new
      end

      def result : Dict
        @state
      end

      # Runs `Dict#includes?` on the dictionary built so far.
      def includes?(object) : Bool
        @state.includes?(object)
      end

      # Runs `Dict#size` on the dictionary built so far.
      def size : Int32
        @state.size
      end

      # Runs `Dict#itemsize` on the dictionary built so far.
      def itemsize : Int32
        @state.itemsize
      end

      def pairsize : Int32
        @state.pairsize
      end

      # Runs `Dict#[]?` on the dictionary built so far.
      def []?(key) : Term?
        @state[key]?
      end

      # Runs `Dict#[]` on the dictionary built so far.
      def [](key) : Term
        @state[key]
      end

      # Adds an entry to the dictionary built so far. See also: `Dict#with`.
      def with(key, value) : self
        return without(key) if value.nil?

        @state = @state.with!(key, value, @cookie)

        self
      end

      def without(key) : self
        @state = @state.without!(key, @cookie)

        self
      end

      def without(*keys) : self
        keys.each { |key| without(key) }

        self
      end

      # Shorthand for `with(itemsize, item)`.
      def append(item) : self
        self.with(itemsize, item)
      end

      # Shorthand for `append`.
      def <<(item) : self
        append(item)
      end

      # Shorthand for a chain of `append`s for each element of the enumerable *ee*.
      # The block is given each item of *ee* and its result is appended to the transaction.
      def concat(ee : Enumerable(T), & : T -> Term) : self forall T
        ee.each { |term0| append(yield term0) }

        self
      end

      def concat(ee : Enumerable(Term)) : self
        concat(ee, &.itself)
      end
    end

    module Part
      alias Any = ItemsRange | Items | Pairs | PairsOrd | Entries | EntriesOrd

      defrecord ItemsRange, range : Range(Int32, Int32) do
        assert range.exclusive?
      end

      defrecord Items
      defrecord Pairs
      defrecord PairsOrd
      defrecord Entries
      defrecord EntriesOrd
    end

    def self.itemspart : Part::Items
      Part::Items.new
    end

    # NOTE: *range* must be exclusive.
    def self.items_range(range : Range(Int32, Int32)) : Part::ItemsRange
      Part::ItemsRange.new(range)
    end

    def self.pairspart : Part::Pairs
      Part::Pairs.new
    end

    def self.pairspart_ord : Part::PairsOrd
      Part::PairsOrd.new
    end

    def self.entries : Part::Entries
      Part::Entries.new
    end

    def self.entries_ord : Part::EntriesOrd
      Part::EntriesOrd.new
    end

    def summary : Summary
      summary = Summary.union(UTermTrie32.summary(@utrie), TermTrie.summary(@ttrie))

      Summary.assoc(summary, self)
    end

    # Yields one or more `Commit` objects so that you can build one or more
    # dictionaries without having to produce many useless intermediate copies.
    #
    # If *block* has one parameter, returns the resulting dict.
    # If *block* has more than one parameters, returns a tuple of the resulting dicts.
    #
    # See also: `#transaction`.
    macro build(&block)
      pass do
        %result = ::Ww::Term::Dict.give({{block.args.map(&.symbolize).splat}}) {{block}}
        {% if block.args.size == 1 %}
          %result[0]
        {% end %}
      end
    end

    # :nodoc:
    def self.give(arg, &)
      {build_impl { |commit| yield commit }}
    end

    # :nodoc:
    def self.give(arg, *args, &)
      dicts = nil
      dict = build_impl do |commit|
        dicts = give(*args) do |*commits|
          yield commit, *commits
        end
      end
      {dict, *dicts.not_nil!}
    end

    # :nodoc:
    def self.build_impl(& : Commit ->)
      EMPTY.transaction { |commit| yield commit }
    end

    # Compares this and *other* dictionaries.
    #
    # Comparison is performed recursively at the same position in both
    # dictionaries using `Term.compare`.
    #
    # If all entries compared equal, the sizes of both dicts are compared to
    # determine the winner (returns `0` if sizes are equal, too).
    def <=>(other : Dict) : Int32
      # NOTE: It's not super nice that we're reserving 512 bytes here, especially
      # since <=> calls are often made deeply and recursively. But to my [inexperienced]
      # eye, it seems better than going to the heap all the time on such a primitive
      # operation as comparison.
      l = Pf::Kit.stack_array({Term, Term}, 16) # 16 bytes x 16 = 256 bytes
      r = Pf::Kit.stack_array({Term, Term}, 16) #
      minsize = Math.min(size, other.size)

      each_entry do |k, v|
        l << {k, v}
        break if l.size == minsize
      end

      other.each_entry do |k, v|
        r << {k, v}
        break if r.size == minsize
      end

      # This... thing is faster than a pair of sort!'s by about 100ns on my machine
      # on one micro-benchmark. It pessimizes near-equality and equality (i.e., many
      # or all comparisons give `0`), while optimizing early inequality (early `-1`
      # or `1` comparison results in little to no wasted work).
      #
      # We also do not touch the heap in the common case (<16 entries in dict). USet32's
      # will not touch the heap until you have >64 entries (they're a pair of u64 bitmaps
      # for <64 entries).
      Pf::USet32.transaction do |lused|
        Pf::USet32.transaction do |rused|
          minsize.times do
            # Find min unused in l.
            lmin = nil
            imin = 0u32
            l.each_with_index do |pivot, i|
              next if i.to_u32.in?(lused)

              if lmin.nil? || Term.compare(pivot, lmin) < 0
                lmin = pivot
                imin = i.to_u32
              end
            end

            # Find min unused in r.
            rmin = nil
            jmin = 0u32
            r.each_with_index do |pivot, j|
              next if j.to_u32.in?(rused)

              if rmin.nil? || Term.compare(pivot, rmin) < 0
                rmin = pivot
                jmin = j.to_u32
              end
            end

            assert lmin && rmin

            cmp = Term.compare(lmin, rmin)
            if cmp == 0 # equal
              lused << imin
              rused << jmin
              next
            end

            return cmp
          end
        end
      end

      size <=> other.size
    end

    # :nodoc:
    def uitemsize : UInt32
      UTermTrie32.seqsize(@utrie)
    end

    # :nodoc:
    def upairsize : UInt32
      usize - uitemsize
    end

    # :nodoc:
    def usize : UInt32
      summary.size
    end

    @[Dncast]
    def itemsize : Int32
      uitemsize.to_i
    end

    @[Dncast]
    def pairsize : Int32
      upairsize.to_i
    end

    # Returns the number of entries in this dictionary.
    @[Dncast]
    def size : Int32
      usize.to_i
    end

    # Returns `true` if this dictionary contains no entries.
    @[Dncast]
    def empty? : Bool
      usize.zero?
    end

    # Shorthand for `!empty?`.
    @[Dncast]
    def nonempty? : Bool
      !empty?
    end

    # Returns `true` if this dictionary contains items only.
    @[Dncast]
    def itemsonly? : Bool
      upairsize.zero?
    end

    # Returns `true` if this dictionary contains pairs only.
    @[Dncast]
    def pairsonly? : Bool
      uitemsize.zero?
    end

    # Returns `true` if this dictionary contains the given *key*.
    @[Dncast]
    def includes?(key) : Bool
      !!self[key]?
    end

    # Returns `true` if *object* is the first item in this dict (checked with `==`).
    def starts_with?(object) : Bool
      return false if itemsize.zero?

      items.first == Term.of(object)
    end

    # Returns `true` if *object* is the last item in this dict (checked with `==`).
    def ends_with?(object) : Bool
      return false if itemsize.zero?

      items.last == Term.of(object)
    end

    # If *term* exists in this dict's itemspart, returns it as a `UInt32`.
    # Returns `nil` otherwise.
    #
    # NOTE: Dict itemspart does not include indices larger than `UInt32`.
    @[Dncast]
    def index32?(term) : UInt32?
      return unless index = Term[term].as?(Term::Num)
      return unless index32 = index.index32?

      index32 < itemsize ? index32 : nil
    end

    # If *term* exists in this dict's itemspart, returns it as a `Term::Num`.
    # Returns `nil` otherwise.
    #
    # NOTE: Dict itemspart does not include indices larger than `UInt32`.
    @[Dncast]
    def index?(term) : Term::Num?
      Term[index32?(term)]
    end

    # O(1) Nth entry in `each_entry`-order (items unordered, pairs unordered).
    @[Dncast]
    def nth?(index : Int32) : {Term, Term}?
      nth?(index.to_u32)
    end

    def nth?(index : UInt32) : {Term, Term}?
      if 0 <= index < UTermTrie32.summary(@utrie).size
        value = UTermTrie32.at?(@utrie, index) || raise IndexError.new
        return Term.of(index), value
      end

      index -= UTermTrie32.summary(@utrie).size

      TermTrie.nth?(@ttrie, index)
    end

    @[Dncast]
    def nth(index : Int32)
      nth?(index) || raise IndexError.new
    end

    # O(1) Nth entry in `items` followed by `Part::PairsOrd`-order.
    @[Dncast]
    def ordnth?(index : Int32) : {Term, Term}?
      if 0 <= index < itemsize
        item = self[index]? || return

        {Term.of(index), item}
      elsif itemsize <= index < size
        pairs_ord[index - itemsize]
      end
    end

    @[Dncast]
    def ordnth(index : Int32) : {Term, Term}
      ordnth?(index) || raise IndexError.new
    end

    # :nodoc:
    @[Dncast]
    def []?(key : UInt32) : Term?
      UTermTrie32.at?(@utrie, key)
    end

    # :nodoc:
    @[Dncast]
    def []?(key : Int32) : Term?
      self[key.to_u32]?
    end

    # Returns the value associated with the given *key*, or nil if *key*
    # is not associated with any value.
    @[Dncast]
    def []?(key) : Term?
      key = Term.of(key)
      if index = key.index32?
        return self[index]?
      end

      TermTrie.at?(@ttrie, key)
    end

    # Returns the value associated with the given *key*, or raises `KeyError`
    # if *key* is not associated with any value.
    @[Dncast]
    def [](key) : Term
      self[key]? || raise KeyError.new
    end

    @[Dncast]
    def []?(*keys) : Term?
      follow?(keys)
    end

    @[Dncast]
    def [](*keys) : Term
      follow(keys)
    end

    # Traverses nested dictionaries for each key in *keys*, returns the value that
    # was reached last. Returns `nil` if some key was not found during traversal.
    @[Dncast]
    def follow?(keys : Enumerable) : Term?
      Term.of(keys.reduce(self) { |dict, key| dict[key]? || return })
    end

    @[Dncast]
    def follow(keys : Enumerable) : Term
      follow?(keys) || raise KeyError.new
    end

    # Yields each entry from this dictionary. **The order of entries is
    # implementation-defined.**
    @[Dncast]
    def each_entry(& : Term, Term ->) : Nil
      UTermTrie32.each(@utrie) do |key, value|
        yield Term.of(key), Term.of(value)
      end

      TermTrie.each(@ttrie) do |key, value|
        yield key, value
      end
    end

    # Yields each itemspart entry whose key is in range, ordered 0
    # to itemsize.
    @[Dncast]
    def each_entry(*, in part : Dict::Part::ItemsRange, & : Term, Term ->)
      from = Math.min(part.range.begin, itemsize)
      to = Math.min(part.range.end, itemsize)

      if to - from > itemsize * 0.5
        # Scan
        items.each_with_index do |item, index|
          next unless from <= index < to
          yield Term.of(index), item
        end
      else
        # Fetch
        (from...to).each do |index|
          yield Term.of(index), self[index]
        end
      end
    end

    # Yields each itemspart entry, ordered 0 to itemsize.
    @[Dncast]
    def each_entry(*, in part : Dict::Part::Items, & : Term, Term ->)
      items.each_with_index do |item, index|
        yield Term.of(index), item
      end
    end

    # Yields each pairspart entry, out of order.
    @[Dncast]
    def each_entry(*, in part : Dict::Part::Pairs, & : Term, Term ->)
      pairspart.each_entry { |key, value| yield key, value }
    end

    # Yields each pairspart entry, ordered lexicographically.
    @[Dncast]
    def each_entry(*, in part : Dict::Part::PairsOrd, & : Term, Term ->)
      pairs_ord.each { |key, value| yield key, value }
    end

    # Yields each entry, out of order.
    @[Dncast]
    def each_entry(*, in part : Dict::Part::Entries, & : Term, Term ->)
      each_entry { |key, value| yield key, value }
    end

    # Yields each entry from this dictionary in lexicographical order.
    # Guarantees the order of entries to be the same across all
    # machines & runs.
    @[Dncast]
    def each_entry(*, in part : Dict::Part::EntriesOrd, & : Term, Term ->)
      unless pairsonly?
        items.each_with_index { |item, index| yield Term.of(index), item }
      end

      return if itemsonly?

      pairs_ord.each { |key, value| yield key, value }
    end

    @pairsptr : Atomic({Term, Term}*) = Atomic.new(Pointer({Term, Term}).null)

    # :nodoc:
    private def pairs_ord : Slice({Term, Term})
      if pairsptr = @pairsptr.get(:acquire) # Not null
        return Slice({Term, Term}).new(pairsptr, pairsize)
      end

      pairsptr = Pointer({Term, Term}).malloc(pairsize)
      index = 0
      each_entry(in: Dict.pairspart) do |key, value|
        pairsptr[index] = {key, value}
        index += 1
      end

      # NOTE: We're fine with unstable sort here because dict keys are never
      # equal within the same dict.
      pairs = Slice({Term, Term}).new(pairsptr, pairsize)
      pairs.unstable_sort! { |(k0, _), (k1, _)| Term.compare(k0, k1) }

      # Try to publish.
      @pairsptr.set(pairsptr, :release)

      pairs
    end

    # Yields itemspart entry values and keys (as Int32), out of order.
    @[Dncast]
    def each_item_with_index(& : Term, Int32 ->) : Nil
      hi = itemsize

      UTermTrie32.each(@utrie) do |key, value|
        next unless key < hi
        yield value, key.to_i
      end
    end

    # Yields itemspart entry values, out of order.
    @[Dncast]
    def each_item_unordered(& : Term ->) : Nil
      each_item_with_index { |item, _| yield item }
    end

    # :nodoc:
    #
    # An enumerable over dictionary entries.
    struct EntryEnumerable
      include Enumerable({Term, Term})

      def initialize(@dict : Dict, @ord : Bool)
      end

      def each(& : {Term, Term} ->)
        if @ord
          @dict.each_entry(in: Dict.entries_ord) { |k, v| yield({k, v}) }
        else
          @dict.each_entry { |k, v| yield({k, v}) }
        end
      end
    end

    # Returns an enumerable based on `each_entry` (if *ordered* is `false`) or
    # `each_entry(Part::EntriesOrd)` (if *ordered* is `true`).
    @[Dncast]
    def ee(*, ordered = false) : Enumerable({Term, Term})
      EntryEnumerable.new(self, ordered)
    end

    # Returns `true` if all entries of `self` are included in *other*.
    @[Dncast]
    def subset_of?(other : Dict) : Bool
      other.size >= size && ee.all? { |k, v| other[k]? == v }
    end

    # Returns `true` if all entries of *other* are included in `self`.
    @[Dncast]
    def superset_of?(other : Dict) : Bool
      other.subset_of?(self)
    end

    @[Dncast]
    def probably_includes?(symbol : Term::Sym) : Bool
      sketch = Sketch.symbol(symbol, hashcode: Term.hashcode(symbol))
      sketch.subset_of?(summary.symbol_sketch)
    end

    # Returns a copy of this dictionary extended with an association between
    # *key* and *value*. If *key* exists, its value is replaced with *value*.
    @[Dncast]
    def with(key, value) : Dict
      with!(key, value, cookie: Cookie.none)
    end

    # :nodoc:
    def with!(key : UInt32, value, cookie : Cookie) : Dict
      if value.nil?
        return without!(key, cookie)
      end

      value = Term.of(value)

      utrie1 = UTermTrie32.assoc(@utrie, key, value, cookie: cookie)
      if @utrie.same?(utrie1)
        return self
      end

      Dict.new(utrie1, @ttrie)
    end

    # :nodoc:
    def with!(key : Int32, value, cookie : Cookie) : Dict
      with!(key.to_u32, value, cookie)
    end

    # :nodoc:
    def with!(key, value, cookie : Cookie) : Dict
      if value.nil?
        return without!(key, cookie)
      end

      key = Term.of(key)
      if index = key.index32?
        return with!(index, value, cookie)
      end

      value = Term.of(value)

      ttrie1 = TermTrie.assoc(@ttrie, key, value, cookie: cookie)
      if @ttrie.same?(ttrie1)
        return self
      end

      Dict.new(@utrie, ttrie1)
    end

    @[Dncast]
    def without(key) : Dict
      without!(key, cookie: Cookie.none)
    end

    # :nodoc:
    def without!(key : UInt32, cookie : Cookie) : Dict
      utrie1 = UTermTrie32.dissoc(@utrie, key, cookie: cookie)
      if @utrie.same?(utrie1)
        return self
      end

      Dict.new(utrie1, @ttrie)
    end

    # :nodoc:
    def without!(key : Int32, cookie : Cookie) : Dict
      without!(key.to_u32, cookie)
    end

    # :nodoc:
    def without!(key, cookie : Cookie) : Dict
      key = Term.of(key)
      if index = key.index32?
        return without!(index, cookie)
      end

      ttrie1 = TermTrie.dissoc(@ttrie, key, cookie: cookie)
      if @ttrie.same?(ttrie1)
        return self
      end

      Dict.new(@utrie, ttrie1)
    end

    @[Dncast]
    def append(item)
      self.with(items.size, item)
    end

    # FIXME: this MUST NOT be O(n), WTF?
    @[Dncast]
    def prepend(item)
      pairspart.transaction do |commit|
        commit.append(item)
        commit.concat(items)
      end
    end

    @[Dncast]
    def prior
      if itemsize.zero?
        return self
      end

      without(itemsize - 1)
    end

    @[Dncast]
    def rest
      pairspart.transaction do |commit|
        commit.concat(1...itemsize) { |index| self[index] }
      end
    end

    @[Dncast]
    def replace(range : Range(UInt32, UInt32), rep : Term::Rep) : Dict
      assert range.exclusive?
      assert range.begin <= range.end <= uitemsize

      pairspart.transaction do |commit|
        # Copy before
        (0u32...range.begin).each do |index|
          commit << self[index]
        end

        commit.concat(rep)

        # Copy after
        (range.end...uitemsize).each do |index|
          commit << self[index]
        end
      end
    end

    @[Dncast]
    def replace(range : Range(Int32, Int32), rep : Term::Rep) : Dict
      assert range.exclusive?
      assert 0 <= range.begin <= range.end

      replace(range.begin.to_u32...range.end.to_u32, rep)
    end

    @[Dncast]
    def replace(index : Int32 | UInt32, rep : Term::Rep) : Dict
      replace(index...index + 1, rep)
    end

    # Yields a `Commit` object which allows you to mutate a copy of `self`.
    #
    # - The commit object is marked as resolved after the block. You should not
    #   retain it. If you do, all operations on the object (including readonly ones)
    #   will raise `ResolvedError`.
    # - If you pass the commit object to another fiber in the block, e.g. via a channel,
    #   and fiber yield immediately after that, the commit obviously would not be marked
    #   as resolved as the resolution code would not have been reached yet. However,
    #   if you then attempt to call mutation methods on the commit, another error,
    #   `ReadonlyError`, will be raised. In other words, the yielded commit object
    #   is readonly for any other fiber except for the fiber that it was originally
    #   yielded to.
    #
    # Returns `self` if the transaction did not touch the dictionary at all. If
    # the dictionary was changed but then the changes were reverted, this method
    # will return a new dictionary.
    @[Dncast]
    def transaction(& : Dict::Commit ->) : Dict
      commit = stack_alloc Commit.new(self)
      yield commit

      commit.result
    end

    # Returns `true` if `self` and *other* share one or more keys.
    def intersects?(other : Dict) : Bool
      sm, lg = size < other.size ? {self, other} : {other, self}
      sm.each_entry do |key, _|
        next unless key.in?(lg)
        return true # intersects
      end

      false # does not intersect
    end

    # Splits this dictionary into *items* and *pairs*. Returns an `ItemsView`
    # over the items and a `pairsonly?` `Dict` with the pairs.
    #
    # - *Entries* are associations between a key (a `Term`) and a value (also a `Term`).
    #   Dictionaries consist of such entries.
    # - *Items* are entries whose key is a natural number (including zero) that is
    #   either zero, or for which a predecessor can be found in the dict. They are
    #   treated specially for efficiency, and are considered the best andmost efficient
    #   way to represent arrays in Term-land.
    # - *Pairs* are all other entries, i.e., all entries that are not items.
    @[Dncast]
    def partition : {Dict, Dict}
      {itemspart, pairspart}
    end

    # Returns the items part of `partition` (see the latter for more info).
    #
    # This method is more efficient than using `partition` and discarding
    # the pairs part.
    @[Dncast]
    def items : Dict::ItemsView
      ItemsView.new(self, b: 0, e: itemsize)
    end

    @[Dncast]
    def items(b : Int32, e : Int32) : Dict::ItemsView
      ItemsView.new(self, b, e)
    end

    # Returns the items part of `partition` (see the latter for more info).
    @[Dncast]
    def itemspart : Dict
      ut_seqsize = UTermTrie32.seqsize(@utrie)
      ut_size = UTermTrie32.summary(@utrie).size
      tt_size = TermTrie.summary(@ttrie).size

      # We make a lot of defensive `itemspart` calls specifically when pattern-matching.
      # Thus, the main path is the one where this dict is already itemsonly.
      if ut_seqsize == ut_size && tt_size.zero?
        return self
      end

      # Another fast path is when we *do* have pairs but not in our UTermTrie32.
      # That is, we have an itemsonly UTermTrie32 without extra entries in
      # there, and we also have some pairs in TermTrie.
      if ut_seqsize == ut_size
        return Dict.new(@utrie, TermTrie.empty)
      end

      # Perform sequence partitioning. This is the slow path for `itemspart`. It is
      # only hit when we have extra entries in UTermTrie32. That is, for instance, if
      # we have gaps, such as in `{0: ~, 1: "John", 3: "Barbara"}`. Notice the missing
      # `2`, which creates a gap at which we must split with `view`.
      Dict.new(UTermTrie32.view(@utrie, 0u32, ut_seqsize), TermTrie.empty)
    end

    # Returns the pairs part of `partition` (see the latter for more info).
    @[Dncast]
    def pairspart
      ut_size = UTermTrie32.summary(@utrie).size
      if ut_size.zero?
        return self
      end

      ut_seqsize = UTermTrie32.seqsize(@utrie)
      if ut_size == ut_seqsize
        return Dict.new(UTermTrie32.empty, @ttrie)
      end

      Dict.new(UTermTrie32.view(@utrie, ut_seqsize, UTermTrie32.capacity(@utrie)), @ttrie)
    end

    # :nodoc:
    def hashcode : UInt64
      summary.hashcode
    end

    # Returns `true` if this and *other* dictionaries are equal.
    def ==(other : Dict) : Bool
      return true if same?(other)
      return false unless summary == other.summary

      unless @utrie.same?(other.@utrie)
        UTermTrie32.each(@utrie) do |key, value0|
          return false unless value1 = UTermTrie32.at?(other.@utrie, key)
          return false unless value0 == value1
        end
      end

      unless @ttrie.same?(other.@ttrie)
        TermTrie.each(@ttrie) do |key, value0|
          return false unless value1 = TermTrie.at?(other.@ttrie, key)
          return false unless value0 == value1
        end
      end

      true
    end

    def inspect(io)
      ML.compact(io, self)
    end

    def to_s(io)
      inspect(io)
    end
  end
end

require "./dict/items_view"
require "./dict/sketch"
require "./dict/histogram"
require "./dict/cookie"
require "./dict/summary"
require "./dict/small_map"
require "./dict/utermtrie32"
