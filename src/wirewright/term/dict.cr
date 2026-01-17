# SIMPLE DICTIONARIES ARE DICTIONARIES THAT DO NOT CONTAIN
# SIMPLE DICTIONARIES.

# Dictionaries should have small (8/16-element) short-term memory for which
# patterns they were NOT matched by. However this would require some integration
# on the pattern matching/PatternSet side. Doable though. PatternSet can assign
# patterns application-unique ids, and so on. In some places we have redundant
# pattern matching. This should eliminate that. Especially useful would be rejection.
# E.g. to optimize FindFirst/FindSource, which are used extensively in the editor.
# If every dictionary EntryNode remembers a tiny bit which patterns matched and which
# did not in itself, that could help. Although a huge balance game, and I'm unsure
# about how the communication would go between the two -- the pattern matching engine
# and the dict impl.

# TODO: our current dictionary implementation is very very bad. It has served
# me for over a year with minor changes, but now it is becoming more and more
# cumbersome to use and extend it. It isn't as simple as it should be, with a
# lot of overcomplication; nor as fast as it should be. When I first wrote it
# only a small portion of the system relied on Dicts. Now almost everything relies
# on dicts.
#    1. We need to have a dict implementation that is friendly towards caching. Any caching
#       idea that comes to my mind, must be trivially implementable. Right now I stumble and
#       fall every time I try to add something. I envision an event-like system where the dict
#       has a part of itself that's consuming events (specifically, pair added, pair removed,
#       item added, item removed). We can then have many such parts, each representing some kind
#       of cache, such as sketch, population, max depth, bounds sketch.
#    2. Perhaps instead, such an event-like system should be installed on item/pair nodes
#       rather than dicts. This could give us a more predictable, mergeable, diffable cache.
#    3. For the itemspart we must use finger trees.
#    4. For the pairspart we should continue using HAMTs.
#    5. The current dict impl is not aware of itself being a tree. Thus, we have an extremely
#       inefficient, linear implementation of highly-depended upon `==`, `hash` (they are used
#       by rewriters which treat them as very, very, very cheap functions to call -- but they are not)
#    6. Similarly, we have inefficient, linear or worse, implementations of dict merging and
#       diffing, which are depended on by Soma and the higher-level.
#    7. The memory layout and general design of permafrost is OK for a general-purpose HAMT,
#       but here we have a lot of packing/inlining opportunities; and we're a dict, whereas
#       Pf::Kit::Node assumes a set (roughly). We lose a lot of copy reduction opportunities
#       by not storing keys and values separately. I also believe we should somehow use variably
#       sized but fixed-bytesize nodes. I think we should fix the bytesize at 64 bytes, since this
#       is a cache line size so we'll get the stuff around-ish for free.
#    8. The keys and values should be a special kind of array, a TermArray. The TermArray should somehow
#       be able to manage the 64 bytes, trying to pack as many things in it as possible. We could have
#       byte headers followed by payload. Something like a boolean is a byte header long; it does not
#       need a payload. We can flatten small itemsonly dictionaries this way.
#    9. The whole packing thing is an undeveloped, unnecessary idea right now. What is fairly clear is that
#       again, we must have a finger tree itemspart and a HAMT pairspart with all copy reduction opportunities taken.
#   10. It is in general hard to implement proper promotion/demotion for items to pairs and vice versa. The current
#       algorithm is very bad and prevents us from saving time and space in several cases (mainly replace). Provided
#       we have a fast itemspart split and merge functions (finger trees claim to have fast splits and merges)
#       we could maintain a small, fixed-size toplevel array of "root nodes". Therefore our dict becomes a forest.
#       When we delete in the middle of an itemspart, we split at that point (we'll have to split anyway), and if
#       the rest is nonempty, we will add it to the toplevel root array. Only when the root array becomes full should
#       we merge *the smallest* itemsroot into the HAMT pairspart; or alternatively, we can split work somehow among
#       the calls to with()/without() so that the cost of many random itemspart deletes is amortized even further.
#       Finally, we should keep the min index for the itemsarrays merged into the pairspart. Whenever an item is inserted,
#       we should check if there are itemsnodes that succede the item's index, or if the pairspart contains a key that
#       succedes the item's index. If so, we should "import" the split right-hand side back into the itemspart.
#   11. ItemsView's #collect is a known performance pain point for the pattern matching engine. Polyblanks such as `xs_*`
#       `collect` excessively almost by design, but never modify the collected itemsonly dict. A DictRef variant of Dict
#       will solve this issue by being a Dict-side variant of ItemsView.
#
# A proper dictionary implementation would be very complex. Currently we are suffering a lot of performance issues
# due to having an improper dict implementation. I would really like to have a dict impl that at least reduces
# the copying overhead and has an event system for more robust caching; as well as one that has DictRef and that is
# aware of its tree nature, implementing efficient deep/shallow diff, deep/shallow merge, equality, cached hash, etc.
# This would be a good start even without finger trees and the other complexity. Trying to gradually ramp up complexity
# is better than trying to tackle an extremely complex data structure like Dict should be head-on.
#
# ALSO: Gap promotion/demotion is opaque to any caching. Thus, I think it makes sense to install any caching on a level
# lower than that of a Dict -- thus, on ItemNode and PairNode.
#
# - If we have sketches on each Pair/Item node, we'll be able to support "symbol-guided descent", and in the future
# perhaps also value-guided descent, which is especially important for the optimization of %item nodes and %leaf nodes.
# Support on the level of dictionary nodes means we'll only have to go through nodes that probably contain the symbol,
# instead of forced thorough iteration.
#
# - It appears that we can try to use linear probing for the items array on HAMT nodes instead of Sparse32.
#   If the key cannot be found, we proceed as usual into the Sparse32 children array.

module Ww
  # Represents a dictionary: an immutable, persistent collection of key-value
  # pairs supporting efficient, near-O(1) insert, delete, and lookup.
  @[Term::Assoc(TermType::Dict, :unsafe_as_d)]
  class Term::Dict
    include Equality
    include AutoUpcast
    include TypeConversion

    # :nodoc:
    alias ItemNode = Pf::Kit::Node(Item)

    # :nodoc:
    alias PairNode = Pf::Kit::Node(Pair)

    # :nodoc:
    struct Item
      getter index, value

      def initialize(@index : Int32, @value : Term)
      end
    end

    # :nodoc:
    struct Pair
      getter key, value

      def initialize(@key : Term, @value : Term)
      end
    end

    # Commits allow you to compose multiple edits into one, big edit of a dict.
    # Thus you avoid having to create many useless intermediate copies.
    class Commit
      @@id : Atomic(Pf::Kit::AuthorId) = Atomic(Pf::Kit::AuthorId).new(Pf::Kit::AUTHOR_FIRST)

      # :nodoc:
      def self.genid
        @@id.add(1)
      end

      @dict : Dict?

      protected def initialize(@parent : Dict, @fiber : UInt64)
        @id = Pf::Kit::AuthorId.new(Commit.genid)
        @resolved = false
      end

      # Runs `Dict#includes?` on the dictionary built so far.
      def includes?(object) : Bool
        (@dict || @parent).includes?(object)
      end

      # Runs `Dict#size` on the dictionary built so far.
      def size : Int32
        (@dict || @parent).size
      end

      # Runs `Dict#itemsize` on the dictionary built so far.
      def itemsize : Int32
        (@dict || @parent).itemsize
      end

      def pairsize : Int32
        (@dict || @parent).pairsize
      end

      # Runs `Dict#[]?` on the dictionary built so far.
      def []?(key) : Term?
        (@dict || @parent)[key]?
      end

      # Runs `Dict#[]` on the dictionary built so far.
      def [](key) : Term
        (@dict || @parent)[key]
      end

      # Adds an entry to the dictionary built so far. See also: `Dict#with`.
      #
      # Raises `ResolvedError` if this commit is used outside of the transaction
      # that produced it (see `Dict#transaction`).
      #
      # Raises `ReadonlyError` if called by a fiber other than the fiber that
      # initiated the transaction.
      def with(key, value) : self
        return without(key) if value.nil?

        raise Pf::ResolvedError.new if @resolved
        raise Pf::ReadonlyError.new unless @fiber == Pf.fiber_id

        dict = @dict ||= Dict.new(*@parent.state)
        dict.with!(key, value, @id)

        self
      end

      # Removes entries with the given *keys* from the dictionary built so far.
      # See also: `Dict#without`.
      #
      # Raises `ResolvedError` if this commit is used outside of the transaction
      # that produced it (see `Dict#transaction`).
      #
      # Raises `ReadonlyError` if called by a fiber other than the fiber that
      # initiated the transaction.
      def without(*keys) : self
        raise Pf::ResolvedError.new if @resolved
        raise Pf::ReadonlyError.new unless @fiber == Pf.fiber_id

        dict = @dict ||= Dict.new(*@parent.state)
        keys.each { |key| dict.without!(key, @id) }

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

      def selected(ee : Enumerable(T), cls : U.class, & : U -> _) : self forall T, U
        ee.each do |object|
          next unless object.is_a?(U)

          append(yield object)
        end

        self
      end

      def selected(ee : Enumerable(T), & : T -> Bool) : self forall T
        ee.each do |object|
          next unless yield object

          append(object)
        end

        self
      end

      def rejected(ee : Enumerable(T), & : T -> Bool) : self forall T
        ee.each do |object|
          next if yield object

          append(object)
        end

        self
      end

      # :nodoc:
      def resolve
        raise Pf::ResolvedError.new if @resolved
        raise Pf::ReadonlyError.new unless @fiber == Pf.fiber_id

        @resolved = true
        @dict || @parent
      end
    end

    # :nodoc:
    EMPTY = new

    # :nodoc:
    EMPTY_ITEM_NODE = ItemNode.new

    # :nodoc:
    EMPTY_PAIR_NODE = PairNode.new

    # Cached hash code for this dict.
    @hash = 0u64

    alias Sketch = UInt128

    # Returns the maximum-ever depth of this dictionary.
    #
    # In other words, this method **does not** return the current maximum depth;
    # it can be said to return the "maximum maximum depth", that is, the largest
    # depth seen throughout the history of this dict.
    def maxdepth : UInt32
      @maxdepth + 1
    end

    def initialize
      @items = EMPTY_ITEM_NODE
      @pairs = EMPTY_PAIR_NODE
      @sketch = Sketch.new(0)
      @maxdepth = 0u32
    end

    protected def initialize(@items, @pairs, @sketch, @maxdepth)
    end

    # Must be possible to do `initialize(*state)`. Must not include any cached
    # data: dictionaries constructed from `state` are expected be mutated without
    # notice -- and stale cache will make the dictionary dysfunctional.
    protected def state
      {@items, @pairs, @sketch, @maxdepth}
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

    # Returns `true` if this dictionary contains items only.
    @[Dncast]
    def itemsonly? : Bool
      @pairs.size.zero?
    end

    # Returns `true` if this dictionary contains pairs only.
    @[Dncast]
    def pairsonly? : Bool
      @items.size.zero?
    end

    # Returns the number of entries in this dictionary.
    @[Dncast]
    def size : Int32
      itemsize + pairsize
    end

    @[Dncast]
    @[AlwaysInline]
    def itemsize
      @items.size
    end

    @[Dncast]
    @[AlwaysInline]
    def pairsize
      @pairs.size
    end

    @[Dncast]
    @[AlwaysInline]
    def hi
      itemsize - 1
    end

    # Returns `true` if this dictionary contains no entries.
    @[Dncast]
    def empty? : Bool
      size.zero?
    end

    # Shorthand for `!empty?`.
    @[Dncast]
    def nonempty? : Bool
      !empty?
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

    @[Dncast]
    def item_at?(key : Int32) : Term?
      return unless key < @items.size
      return unless coat = @items.fetch?(Probes::FetchItem.new(key))

      entry, *_ = coat
      entry.value
    end

    # :nodoc:
    @[Dncast]
    def at?(key : Term::Num) : Term?
      return at_default?(key) unless i = index32?(key)
      return at_default?(key) unless coat = @items.fetch?(Probes::FetchItem.new(i.to_i))

      entry, *_ = coat
      entry.value
    end

    # :nodoc:
    @[Dncast]
    def at?(key : Term::Any) : Term?
      at_default?(key)
    end

    private def at_default?(key : Term::Any) : Term?
      return unless coat = @pairs.fetch?(Probes::FetchPair.new(Term.of(key)))

      entry, *_ = coat
      entry.value
    end

    # O(1) Nth entry in `each_entry`-order (items unordered, pairs unordered).
    @[Dncast]
    def nth?(index : Int32) : {Term, Term}?
      if 0 <= index < itemsize
        entry = @items.nth?(index) || return

        {Term.of(entry.index), entry.value}
      elsif itemsize <= index < size
        entry = @pairs.nth?(index - itemsize) || return

        {entry.key, entry.value}
      end
    end

    @[Dncast]
    def nth(index : Int32)
      nth?(index) || raise IndexError.new
    end

    # O(1) Nth entry in `items` followed by `each_pair_ord`-order.
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

    # Returns the value associated with the given *key*, or nil if *key*
    # is not associated with any value.
    @[Dncast]
    def at?(key) : Term?
      at?(Term[key])
    end

    # Returns the value associated with the given *key*, or raises `KeyError`
    # if *key* is not associated with any value.
    @[Dncast]
    def at(key) : Term
      at?(key) || raise KeyError.new
    end

    # Returns the value associated with the given *key*, or *default* if *key*
    # is not associated with any value.
    @[Dncast]
    def at(key, *, default) : Term
      at?(key) || Term.of(default)
    end

    # Transforms the value associated with the given *key* using the block, or
    # returns *orelse* without transforming it if *key* is not associated with
    # any value.
    @[Dncast]
    def at(key, *, orelse, &) : Term
      return Term.of(orelse) unless value = at?(key)

      Term.of(yield value)
    end

    # Alias of `at`.
    @[Dncast]
    def [](*args, **kwargs) : Term
      at(*args, **kwargs)
    end

    # Alias of `at?`.
    @[Dncast]
    def []?(*args, **kwargs) : Term?
      at?(*args, **kwargs)
    end

    # Traverses nested dictionaries for each key in *keys*, returns the value that
    # was reached last. Returns `nil` if some key was not found during traversal.
    @[Dncast]
    def dig?(*keys) : Term?
      keys.reduce(self) { |dict, key| dict.at?(key) || return }
    end

    # Same as `dig?`, but raises `KeyError` instead of returning `nil` if some key
    # was not found during traversal.
    @[Dncast]
    def dig(*keys) : Term
      dig?(*keys) || raise KeyError.new("#{keys}")
    end

    # Alias of `dig`.
    @[Dncast]
    def [](*keys) : Term
      dig(*keys)
    end

    # Alias of `dig?`.
    @[Dncast]
    def []?(*keys) : Term?
      dig?(*keys)
    end

    # Yields each entry from this dictionary. **The order of entries is
    # implementation-defined.**
    @[Dncast]
    def each_entry(& : Term, Term ->) : Nil
      @items.each { |entry| yield Term.of(entry.index), entry.value }
      @pairs.each { |entry| yield entry.key, entry.value }
    end

    @pairsptr : Atomic({Term, Term}*) = Atomic.new(Pointer({Term, Term}).null)

    # :nodoc:
    private def pairs_ord : Slice({Term, Term})
      if pairsptr = @pairsptr.get(:acquire) # Not null
        return Slice({Term, Term}).new(pairsptr, pairsize)
      end

      pairsptr = Pointer({Term, Term}).malloc(pairsize)
      index = 0
      each_pair do |key, value|
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

    # Yields each entry from this dictionary in stable order. Guarantees the order
    # of entries to be the same across all machines & runs.
    @[Dncast]
    def each_entry_ord(& : Term, Term ->) : Nil
      unless pairsonly?
        items.each_with_index { |item, index| yield Term.of(index), item }
      end

      return if itemsonly?

      pairs_ord.each { |key, value| yield key, value }
    end

    # Yields each item from this dictionary followed by its index. **Items are yielded
    # out of order**.
    @[Dncast]
    def each_item_with_index(& : Term, Int32 ->) : Nil
      @items.each { |entry| yield entry.value, entry.index }
    end

    # :nodoc:
    #
    # Ratio of itemsize to range size to begin scanning the dict instead of looking
    # up when iterating over items in range.
    SCAN_THRESHOLD = 0.6

    # TODO: We should probably remove this in favor of the built-in `ItemsView#each(within) < Indexable`
    @[Dncast]
    def each_item_with_index(*, within range : Range(Int32, Int32), & : Term, Int32 ->) : Nil
      assert range.exclusive?

      if range.empty?
        return
      end

      assert range.subrange_of?(0...itemsize)

      if itemsize / range.size >= SCAN_THRESHOLD
        # Better to iterate over dict in memory-order and check if item is in range.
        each_item_with_index do |item, index|
          next unless index.in?(range)
          yield item, index
        end
      else
        # Better to iterate over range and lookup each item.
        range.each do |index|
          assert item = item_at?(index)
          yield item, index
        end
      end
    end

    @[Dncast]
    def each_item_unordered(& : Term ->) : Nil
      @items.each { |entry| yield entry.value }
    end

    # Yields each pair from this dictionary. **Pairs are yielded out of order**.
    @[Dncast]
    def each_pair(& : Term, Term ->)
      @pairs.each { |entry| yield entry.key, entry.value }
    end

    @[Dncast]
    def each_pair_ord(& : Term, Term ->)
      pairs_ord.each { |key, value| yield key, value }
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
          @dict.each_entry_ord { |k, v| yield({k, v}) }
        else
          @dict.each_entry { |k, v| yield({k, v}) }
        end
      end
    end

    # Returns an enumerable based on `each_entry` (if *ordered* is `false`) or
    # `each_entry_ord` (if *ordered* is `true`).
    @[Dncast]
    def ee(*, ordered = false) : Enumerable({Term, Term})
      EntryEnumerable.new(self, ordered)
    end

    # :nodoc:
    #
    # An enumerable over dictionary entry values.
    struct ValueEnumberable
      include Enumerable(Term)

      def initialize(@dict : Dict)
      end

      def each(& : Term ->)
        @dict.each_entry { |_, v| yield v }
      end
    end

    # Returns an enumerable of values based on `each_entry`.
    #
    # TODO: Probably remove this.
    @[Dncast]
    def ve : Enumerable(Term)
      ValueEnumberable.new(self)
    end

    # :nodoc:
    #
    # An enumerable over dictionary items (keys 0 through n where n is the size
    # of the dictionary).
    struct ItemEnumerable
      include Enumerable(Term)

      def initialize(dict : Dict)
        @items = dict.items
      end

      def each(& : Term ->)
        @items.each { |item| yield item }
      end
    end

    # Returns an enumerable for items found in this dictionary.
    #
    # See also: `items`.
    #
    # TODO: Probably remove this in favor of `items`.
    @[Dncast]
    def ie : Enumerable(Term)
      ItemEnumerable.new(self)
    end

    # Returns an enumerable based on `each_pair`. The pairs will be emitted
    # in order if *ordered* is set to `true`.
    #
    # TODO: Probably remove this.
    @[Dncast]
    def pe(*, ordered = false) : Enumerable({Term, Term})
      pairspart.ee(ordered: ordered)
    end

    # Returns `true` if this dict appears to be a *dict set*: it is nonempty,
    # and all entry values are `true`.
    @[Dncast]
    def set? : Bool
      !empty? && ee.all? { |_, v| v.type.boolean? && v.unsafe_as_b.true? }
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

    def like?(other) : Bool
      (@sketch & other.@sketch) == other.@sketch
    end

    def sketch_superset_of?(subset : Sketch) : Bool
      (@sketch & subset) == subset
    end

    @[Dncast]
    def probably_includes?(symbol : Term::Sym) : Bool
      Dict.probably_includes?(@sketch, symbol)
    end

    def self.probably_includes?(sketch : Sketch, symbol : Term::Sym) : Bool
      bucket = Term.hashcode(symbol) % Sketch.width
      sketch.bit(bucket) == 1
    end

    def self.mix(sketch : Sketch, value : Term)
      case value.type
      when .symbol?
        bucket = Term.hashcode(value.unsafe_as_sym) % Sketch.width
        sketch | (Sketch.new(1) << bucket)
      when .dict?
        sketch | value.unsafe_as_d.@sketch
      else
        sketch
      end
    end

    def self.mixdepth(depth : UInt32, value : Term) : UInt32
      case value.type
      when .dict?
        Math.max(depth, value.unsafe_as_d.maxdepth + 1)
      else
        depth
      end
    end

    @[Dncast]
    def fresh_sketch
      sketch = Sketch.new(0)
      each_entry do |k, v|
        case v.type
        when .symbol?
          sketch = Dict.mix(sketch, v)
        when .dict?
          sketch |= v.unsafe_as_d.fresh_sketch
        end
      end
      sketch
    end

    # :nodoc:
    def with(key : Term::Num, value : Term) : Dict
      return with_default(key, value) unless key.natural?
      return with_default(key, value) unless key <= Term[@items.size]

      index = key.to(Int32)

      added, items = @items.add(Probes::AssocItemImm.new(index, value))
      unless added # Overridden or completely unchanged
        return @items.same?(items) ? self : Dict.new(items, @pairs, Dict.mix(@sketch, value), Dict.mixdepth(@maxdepth, value))
      end

      items, pairs, _, _ = Gap.promote(index + 1,
        nitems: @items.size + 1,
        npairs: @pairs.size,
        items: items,
        pairs: @pairs,
      )

      Dict.new(items, pairs, Dict.mix(@sketch, value), Dict.mixdepth(@maxdepth, value))
    end

    # :nodoc:
    def with(key : Term::Any, value : Term) : Dict
      with_default(key, value)
    end

    # Returns a copy of this dictionary extended with an association between
    # *key* and *value*. If *key* was present already its value is updated
    # in the copy.
    #
    # If *value* is `nil` acts as `without`. This is mainly useful during
    # conversion from JSON (via `Term.[]`), treating `null` as absence.
    @[Dncast]
    def with(key, value) : Dict
      if value.nil? || value.is_a?(JSON::Any) && value.raw.nil?
        return without(key)
      end

      self.with(Term[key], Term.of(value))
    end

    # TODO: have an optimized version of this
    @[Dncast]
    def with(key, &)
      self.with(key, yield self[key]?)
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

    # SAME
    # TODO: more idiomatic name
    @[Dncast]
    def lshift
      pairspart.transaction do |commit|
        commit.concat(1...itemsize) { |index| self[index] }
      end
    end

    private def with_default(key : Term::Any, value : Term) : Dict
      added, pairs = @pairs.add(Probes::AssocPairImm.new(Term.of(key), value))
      unless added # Overridden or completely unchanged
        return @pairs.same?(pairs) ? self : Dict.new(@items, pairs, Dict.mix(@sketch, value), Dict.mixdepth(@maxdepth, value))
      end

      Dict.new(@items, pairs, Dict.mix(@sketch, value), Dict.mixdepth(@maxdepth, value))
    end

    @[Dncast]
    def follow?(keys : Enumerable(Term)) : Term?
      Term.of(keys.reduce(self) { |dict, key| dict[key]? || return })
    end

    @[Dncast]
    def follow(keys : Enumerable(Term)) : Term
      follow?(keys) || raise KeyError.new
    end

    @[Dncast]
    def follow?(keys : Indexable(Term), *, __cursor = 0, &fn : Term -> Term?) : Term?
      case __cursor
      when keys.size
        fn.call(Term.of(self))
      when keys.size - 1
        key = keys[__cursor]
        return unless value0 = self[key]?
        Term.of(self.with(key, fn.call(value0)))
      else
        key = keys[__cursor]
        return unless value0 = self[key]?
        return unless value0 = value0.as_d?
        return unless value1 = value0.follow?(keys, __cursor: __cursor + 1, &fn)

        Term.of(self.with(key, value1))
      end
    end

    @[Dncast]
    def follow(keys : Indexable(Term), &fn : Term -> Term) : Term
      follow?(keys, &fn) || raise KeyError.new
    end

    @[Dncast]
    def where(key, eq fn : Term -> Term) : Dict
      return self unless v0 = self[key]?

      self.with(key, fn.call(v0))
    end

    @[Dncast]
    def where(key, eq value) : Dict
      self.with(key, value)
    end

    @[Dncast]
    def where(key, *keys, eq value) : Dict
      self.with(key, (self[key]? || Term[]).where(*keys, eq: value))
    end

    @[Dncast]
    def where(key, *keys, eq value : Nil) : Dict
      return self unless v0 = self[key]?
      v1 = v0.where(*keys, eq: nil)
      v1.empty? ? without(key) : self.with(key, v1)
    end

    @[Dncast]
    def morph(place)
      where(*place[...-1], eq: place[-1])
    end

    @[Dncast]
    def morph(place, *places)
      morph(place).morph(*places)
    end

    @[Dncast]
    def where(prefix : BiList(Term), eq value) : Dict
      case prefix
      when .empty?
        raise ArgumentError.new
      when .one?
        self.with(prefix.first, value)
      else
        key = prefix.first

        unless value0 = self[key]?
          value0 = Term[]
        end

        unless value0.type.dict?
          # Replace non-dictionary values with dictionaries if such a case ever
          # occurs. It shouldn't.
          value0 = Term[]
        end

        self.with(key, value0.where(prefix.rest, value))
      end
    end

    @[Dncast]
    def where(prefix : Dict::ItemsView | Slice(Term), eq value) : Dict
      case prefix.size
      when 0
        raise ArgumentError.new
      when 1
        self.with(prefix.first, value)
      else
        key = prefix.first

        unless value0 = self[key]?
          value0 = Term[]
        end

        unless value0.type.dict?
          # Replace non-dictionary values with dictionaries if such a case ever
          # occurs. It shouldn't.
          value0 = Term[]
        end

        self.with(key, value0.where(prefix + 1, value))
      end
    end

    # Removes the entry with the given *key* if present. Returns the modified
    # copy of this dict and the value associated with *key* (if any, else `nil`).
    @[Dncast]
    def without?(key) : {Dict, Term?}
      dict1 = without(key)

      {dict1, self[key]?}
    end

    # :nodoc:
    def without(key : Term::Num) : Dict
      return without_default(key) unless index = index32?(key)

      items, pairs, _, _ = Gap.demote(
        end_exclusive: index.to_i,
        rdrop: true, # < will remove the item
        nitems: @items.size,
        npairs: @pairs.size,
        items: @items,
        pairs: @pairs,
      )

      Dict.new(items, pairs, @sketch, @maxdepth)
    end

    # :nodoc:
    def without(key : Term::Any) : Dict
      without_default(key)
    end

    # :nodoc:
    def without(key : Term) : Dict
      without(Term[key])
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # an association with the given *key*.
    @[Dncast]
    def without(key) : Dict
      without(Term.of(key))
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # associations with any of the given *keys*.
    @[Dncast]
    def without(*keys) : Dict
      residue(keys)
    end

    def residue(keys : Enumerable)
      transaction do |commit|
        keys.each { |key| commit.without(key) }
      end
    end

    private def without_default(key : Term::Any) : Dict
      removed, pairs = @pairs.delete(Probes::DissocPairImm.new(Term.of(key)))
      removed ? Dict.new(@items, pairs, @sketch, @maxdepth) : self
    end

    protected def with!(key : Term::Num, value : Term, author) : Dict
      return with_default!(key, value, author) unless key.natural?
      return with_default!(key, value, author) unless key <= Term[@items.size]

      index = key.to(Int32)
      added, @items = @items.add(Probes::AssocItemMut.new(index, value, author: author))

      if added # Try to promote successive (index + 1) pairs to items, if any.
        @items, @pairs, _, _ = Gap.promote(
          end_exclusive: index + 1,
          author: author,
          nitems: @items.size + 1, # < new item was added
          npairs: @pairs.size,
          items: @items,
          pairs: @pairs,
        )
      end

      # If value is unchanged (e.g. with(0, :x) followed by with (0, :x)) nothing
      # will happen since the bit has already been set.
      @sketch = Dict.mix(@sketch, value)
      @maxdepth = Dict.mixdepth(@maxdepth, value)
      @pairsptr.set(Pointer({Term, Term}).null, :release)

      self
    end

    protected def with!(key : Term::Any, value : Term, author) : Dict
      with_default!(key, value, author)
    end

    protected def with_default!(key : Term::Any, value : Term, author) : Dict
      _, @pairs = @pairs.add(Probes::AssocPairMut.new(Term.of(key), value, author: author))

      @sketch = Dict.mix(@sketch, value)
      @maxdepth = Dict.mixdepth(@maxdepth, value)
      @pairsptr.set(Pointer({Term, Term}).null, :release)

      self
    end

    protected def with!(key, value, author) : Dict
      with!(Term[key], Term.of(value), author)
    end

    protected def without!(key : Term::Num, author) : Dict
      return without_default!(key, author) unless index = index32?(key)

      @items, @pairs, _, _ = Gap.demote(
        end_exclusive: index.to_i,
        author: author,
        rdrop: true, # < will remove the item
        nitems: @items.size,
        npairs: @pairs.size,
        items: @items,
        pairs: @pairs,
      )
      @pairsptr.set(Pointer({Term, Term}).null, :release)

      self
    end

    protected def without!(key : Term::Any, author) : Dict
      without_default!(key, author)
    end

    private def without_default!(key : Term::Any, author) : Dict
      _, @pairs = @pairs.delete(Probes::DissocPairMut.new(Term.of(key), hole: Pointer(Term).null, author: author))
      @pairsptr.set(Pointer({Term, Term}).null, :release)

      self
    end

    protected def without!(key : Term, author) : Dict
      without!(Term[key], author)
    end

    protected def without!(key, author) : Dict
      without!(Term[key], author)
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
      commit = Commit.new(self, Pf.fiber_id)
      yield commit
      commit.resolve
    end

    @[Dncast]
    def replace(& : Term, Term -> Term?) : Dict
      instance : Dict? = nil
      author = nil

      each_entry do |k, v|
        next unless rep = yield k, v
        # Fast path. This would have been done anyway below, but let's have it.
        if v.type.dict? && rep.type.dict?
          next if v.unsafe_as_d.same?(rep.unsafe_as_d)
        end
        author ||= Commit.genid
        instance ||= Dict.new(*state)
        instance = instance.with!(k, rep, author)
      end

      # Throw away the copy if nothing changed, but prefer the copy if it has
      # computed the hashcode.
      return self unless instance
      return instance if @hash.zero? && instance.@hash.nonzero?
      return self if state == instance.state

      instance
    end

    @[Dncast]
    def subst1(term, replacement) : Dict
      term, replacement = Term.of(term), Term.of(replacement)

      replace { |_, v| v == term ? replacement : nil }
    end

    # Recursive, depth-first substitution using the substitution table *subt*.
    @[Dncast]
    def subst(subt) : Dict
      subt = subt.as_d

      replace do |_, v0|
        if v1 = subt[v0]?
          v1
        elsif v0.type.dict?
          Term.of(v0.unsafe_as_d.subst(subt))
        end
      end
    end

    # :nodoc:
    def unify?(key : Term, vsucc : Term) : Dict?
      return self.with(key, vsucc) unless vpred = self[key]?
      return unless vpred == vsucc
      self
    end

    # Returns a copy of this dictionary where an association between *key* and
    # *vsucc* is guaranteed to exist. However, if *key* is already present
    # in this dictionary but has a different value (`==`), then this method
    # returns `nil`.
    @[Dncast]
    def unify?(key, vsucc) : Dict?
      unify?(Term.of(key), Term.of(vsucc))
    end

    private def pluck(key, commit : Commit)
      return unless value = at?(key)

      commit.with(key, value)
    end

    private def pluck(orig, renamed, commit : Commit)
      return unless value = at?(orig)

      commit.with(renamed, value)
    end

    private def pluck(key : Tuple, commit : Commit)
      pluck(*key, commit)
    end

    # Creates a dictionary that contains only entries with the given *keys*, and
    # some additional *entries*.
    #
    # ```
    # Term[x: 100, y: 200, z: 300].pluck(:x)           # => Term[x: 100]
    # Term[x: 100, y: 200, z: 300].pluck({:x, :a})     # => Term[a: 100]
    # Term[x: 100, y: 200, z: 300].pluck({:x, :a}, :y) # => Term[a: 100, y: 200]
    #
    # Term[x: 100, y: 200, z: 300].pluck({:z, :foo}, foobar: 4) # => Term[foo: 300, foobar: 4]
    # ```
    @[Dncast]
    def pluck(*keys, **rest) : Term::Dict
      Dict.build do |commit|
        keys.each { |key| pluck(key, commit) }
        rest.each { |k, v| commit.with(k, v) }
      end
    end

    # Returns a dictionary with entries whose keys are present in the enumerable *ee*.
    #
    # Keys present in *ee* but missing in `self` are skipped.
    @[Dncast]
    def pluck(ee : Enumerable(Term)) : Term::Dict
      Dict.build do |commit|
        ee.each { |key| commit.with(key, self[key]?) }
      end
    end

    # Merge-concatenate.
    @[Dncast]
    def mcat(other : Dict) : Dict
      # Fast path
      if itemsonly? && other.pairsonly?
        return Dict.new(@items, other.@pairs, @sketch | other.@sketch, Math.max(@maxdepth, other.@maxdepth))
      end

      # Slow path
      transaction do |commit|
        commit.concat(other.items)

        other.each_pair do |key, value|
          commit.with(key, value)
        end
      end
    end

    # Shallow merge.
    #
    # Merges this dictionary with a *newer* one. If two keys are equal the value
    # from *newer* is preferred.
    @[Dncast]
    def |(newer) : Dict
      newer = newer.as_d

      return newer if empty?
      return self if newer.empty?

      # Don't waste time allocating commits for singleton dicts.

      if size == 1 # Extend with missing
        k, v = ee.first
        return k.in?(newer) ? newer : newer.with(k, v)
      end

      if newer.size == 1 # Override by all from newer
        return self.with(*newer.ee.first)
      end

      # Allocate commits otherwise. Now they're supposed to save time, sometimes
      # (but actually almost always) drastically.

      if size < newer.size # Extend with missing
        newer.transaction do |commit|
          each_entry do |k, v|
            next if k.in?(newer)
            commit.with(k, v)
          end
        end
      else # Override by all from newer
        transaction do |commit|
          newer.each_entry { |k, v| commit.with(k, v) }
        end
      end
    end

    @[Dncast]
    def span(b : Num, e : Num) : Dict
      return Term[] if b == e
      return items.collect if b == Term[0] && e == Term[size]
      return Term[] unless b < e <= size

      Term::Dict.build do |commit|
        (b...e).each do |key|
          commit.append(self[key])
        end
      end
    end

    # Lets the block replace items in the given *range* with zero or more items
    # by appending to the commit. Returns the modified copy of `self`.
    @[Dncast]
    def replace(range : Range(Term::Num, Term::Num), & : Term::Dict::Commit ->) : Term::Dict
      assert range.exclusive?
      assert Term[0] <= range.begin <= Term[itemsize]

      pairspart.transaction do |commit|
        # Copy before
        (Term[0]...range.begin).each do |index|
          commit << self[index]
        end

        yield commit

        # Copy after
        (range.end...itemsize).each do |index|
          commit << self[index]
        end
      end
    end

    @[Dncast]
    def replace(index : Term::Num, &)
      replace(index...index + 1) { |commit| yield commit }
    end

    # Dict set intersection. Values are ignored; only key presence/absence is taken
    # into account. May mix keys/values from `self`/*other* for additional speedup
    # (set *mix* to `false` to disallow).
    @[Dncast]
    def xsect(other : Dict, *, mix = true) : Dict
      if empty? || other.empty?
        return Term[]
      end

      if size < other.size || !mix
        transaction do |commit|
          each_entry do |k, _|
            next if k.in?(other)
            commit.without(k)
          end
        end
      else
        other.transaction do |commit|
          other.each_entry do |k, _|
            next if k.in?(self)
            commit.without(k)
          end
        end
      end
    end

    # Dict entry mask intersection. Leaves keys common to `self` and *other*,
    # their values set to `true`.
    @[Dncast]
    def msect(other : Dict) : Dict
      Term::Dict.build do |commit|
        each_entry do |k, _|
          next unless k.in?(other)
          commit.with(k, true)
        end
        other.each_entry do |k, _|
          next unless k.in?(self)
          commit.with(k, true)
        end
      end
    end

    # Dict set subtraction. Values are ignored; only key presence/absence is taken
    # into account.
    @[Dncast]
    def sub(other : Dict) : Dict
      if size < other.size
        transaction do |commit|
          each_entry do |k, _|
            next unless k.in?(other)
            commit.without(k)
          end
        end
      else
        transaction do |commit|
          other.each_entry do |k, _|
            commit.without(k)
          end
        end
      end
    end

    # In practice `partition` and `pairs` are called very often. Therefore by
    # losing 8 bytes per existing dict, we gain a lot more -- because otherwise
    # for each new dict created by `partition` we lose 32 bytes (if without @pairspart).
    # Thus despite having more to store on every dict, we get better memory
    # performance overall.
    @itemspart : Dict?
    @pairspart : Dict?

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
      ItemsView.new(@items, b: 0, e: @items.size, sketch0: @sketch, maxdepth0: @maxdepth)
    end

    @[Dncast]
    def items(b : Int32, e : Int32) : Dict::ItemsView
      ItemsView.new(@items, b, e, sketch0: @sketch, maxdepth0: @maxdepth)
    end

    # Returns the items part of `partition` (see the latter for more info).
    @[Dncast]
    def itemspart : Dict
      if itemsonly?
        self
      else
        @itemspart ||= items.collect
      end
    end

    # Returns the pairs part of `partition` (see the latter for more info).
    @[Dncast]
    def pairspart : Dict
      if pairsonly?
        self
      else
        @pairspart ||= Dict.new(EMPTY_ITEM_NODE, @pairs, @sketch, @maxdepth)
      end
    end

    # :nodoc:
    def hashcode(& : -> UInt64) : UInt64
      if @hash.nonzero?
        return @hash
      end

      hashcode = yield

      # Take away one slot from the hash function for our own use. This
      # sadly means we have to collide all 0-hash values with all 1-
      # hash ones.
      if hashcode.zero?
        hashcode = 1u64
      end

      @hash = hashcode
    end

    # Returns `true` if this and *other* dictionaries are equal. Returns `false` otherwise.
    def ==(other : Dict) : Bool
      return true if same?(other)

      # Sketches of equal dicts have *something* in common. They may be substantially
      # different or even junky if either (or both) dictionaries have "rich histories";
      # but there must exist an intersection of bits.
      if @sketch > 0 && other.@sketch > 0 && (@sketch & other.@sketch) == 0
        return false
      end

      # TODO: use @maxdepth somehow as well

      return false unless @items.size == other.@items.size && @pairs.size == other.@pairs.size

      h0 = @hash
      h1 = other.@hash

      return false if h0.nonzero? && h1.nonzero? && h0 != h1

      each_entry do |k, v1|
        return false unless v2 = other[k]?
        return false unless v1 == v2
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

  private module Term::Dict::Gap
    # Demotes *end exclusive*-th and successive items so they become pairs. Changes
    # are authored by *author*, thereby saving a few copies.
    #
    # Returns new `Dict` state tuple from which you can initialize a `Dict`,
    # or mutate an existing one. See also: `Dict#state`.
    #
    # ```text
    #  ITEMS                     PAIRS
    # +-----------------------+
    # | 0   1   2   3   4   5 |  x   y
    # +-----------------------+
    #
    #              | Gap.demote(3)
    #              v
    #  ITEMS         PAIRS
    # +-----------+
    # | 0   1   2 |  3   4   5   x   y
    # +-----------+
    # ```
    #
    # If *rdrop* is `true` the *end exclusive*-th item is removed rather than
    # becoming a pair.
    #
    # ```text
    #  ITEMS                     PAIRS
    # +-----------------------+
    # | 0   1   2   3   4   5 |  x   y
    # +-----------------------+
    #
    #              | Gap.demote(3, rdrop: true)
    #              v
    #  ITEMS         PAIRS
    # +-----------+
    # | 0   1   2 |  4   5   x   y
    # +-----------+
    # ```
    def self.demote(end_exclusive, rdrop, items, pairs, nitems, npairs, author = nil)
      (end_exclusive...nitems).each do |index|
        hole = uninitialized Term
        author ||= Commit.genid

        removed, items = items.delete(Probes::DissocItemMut.new(index, hole: pointerof(hole), author: author))
        unless removed
          raise "BUG: Gap.demote(): invalid state: item was not removed"
        end

        nitems -= 1

        next if rdrop && index == end_exclusive

        added, pairs = pairs.add(Probes::AssocPairMut.new(Term.of(index), value: hole, author: author))
        unless added
          raise "BUG: Gap.demote(): invalid state: pair was not added"
        end

        npairs += 1
      end

      {items, pairs, nitems, npairs}
    end

    # Promotes *end exclusive*-th pair and successive pairs so they become items.
    # Changes are authored by *author*, thereby saving a few copies.
    #
    # Returns new `Dict` state tuple from which you can initialize a `Dict`,
    # or mutate an existing one.
    #
    # Inverse of `demote`:
    #
    # ```text
    #  ITEMS         PAIRS
    # +-----------+
    # | 0   1   2 |  3   4   5   x   y
    # +-----------+
    #
    #               | Gap.promote(3)
    #               v
    #  ITEMS                     PAIRS
    # +-----------------------+
    # | 0   1   2   3   4   5 |  x   y
    # +-----------------------+
    # ```
    def self.promote(end_exclusive, items, pairs, nitems, npairs, author = nil)
      return items, pairs, nitems, npairs unless npairs > 0

      while true
        hole = uninitialized Term
        author ||= Commit.genid

        removed, pairs = pairs.delete(Probes::DissocPairMut.new(Term.of(end_exclusive), hole: pointerof(hole), author: author))
        break unless removed # Next gap or no more items

        npairs -= 1

        added, items = items.add(Probes::AssocItemMut.new(key: end_exclusive, value: hole, author: author))
        unless added
          raise "BUG: Gap.promote(): item was not added"
        end

        end_exclusive += 1
        nitems += 1
      end

      {items, pairs, nitems, npairs}
    end
  end

  private module Term::Dict::Probes
    # :nodoc:
    macro compose(cls, *incls, &ext)
      struct {{cls}}
        {% for incl in incls %}
          include {{incl}}
        {% end %}

        {{yield}}
      end
    end

    # Includers are fetch probes with stored entry type `E` and key type `K`.
    module Fetch(E, K)
      include Pf::Kit::IProbeFetch(E)

      def initialize(@key : K)
      end

      abstract def keyof(stored : E) : K

      def path : UInt64
        Term.hashcode(@key)
      end

      def match?(stored : E) : Bool
        @key == keyof(stored)
      end
    end

    # Includers are add probes with stored entry type `E` and key type `K`.
    module Assoc(E, K)
      include Pf::Kit::IProbeAdd(E)

      getter path : UInt64

      @key : K
      @value : Term

      abstract def keyof(stored : E) : K

      def match?(stored : E) : Bool
        @key == keyof(stored)
      end

      def replace?(stored : E) : Bool
        !@value.same?(stored.value)
      end

      def value : E
        E.new(@key, @value)
      end
    end

    # Includers are delete probes with stored entry type `E` and key type `K`.
    module Dissoc(E, K)
      include Pf::Kit::IProbeDelete(E)

      getter path : UInt64

      @key : K

      abstract def keyof(stored : E) : K

      def match?(stored : E) : Bool
        @key == keyof(stored)
      end
    end

    # Includers do not have authorship rights, therefore they always copy
    # the underlying nodes before changing them.
    module NoAuthor
      def author : Pf::Kit::AuthorId
        Pf::Kit::AUTHOR_NONE
      end
    end

    # Includers have authorship rights, therefore they can copy the underlying
    # nodes only once before changing them, and then can change them without
    # copying forever.
    module Authored
      getter author : Pf::Kit::AuthorId
    end

    module PairStored
      def keyof(stored : Pair) : Term
        stored.key
      end
    end

    module ItemStored
      def keyof(stored : Item) : Int32
        stored.index
      end
    end

    module KVInitialize
      def initialize(@key, @value)
        @path = Term.hashcode(@key)
      end
    end

    module KInitialize
      def initialize(@key)
        @path = Term.hashcode(@key)
      end
    end

    compose FetchPair, Fetch(Pair, Term), PairStored
    compose FetchItem, Fetch(Item, Int32), ItemStored

    compose AssocPairImm, Assoc(Pair, Term), PairStored, NoAuthor, KVInitialize
    compose AssocItemImm, Assoc(Item, Int32), ItemStored, NoAuthor, KVInitialize

    compose DissocPairImm, Dissoc(Pair, Term), PairStored, NoAuthor, KInitialize
    compose DissocItemImm, Dissoc(Item, Int32), ItemStored, NoAuthor, KInitialize

    compose AssocPairMut, Assoc(Pair, Term), PairStored, Authored, KVInitialize do
      def initialize(*args, @author, **kwargs)
        super(*args, **kwargs)
      end
    end

    compose AssocItemMut, Assoc(Item, Int32), ItemStored, Authored, KVInitialize do
      def initialize(*args, @author, **kwargs)
        super(*args, **kwargs)
      end
    end

    compose DissocPairMut, Dissoc(Pair, Term), PairStored, Authored, KInitialize do
      def initialize(*args, @hole : Term*, @author, **kwargs)
        super(*args, **kwargs)
      end

      def match?(stored : Pair) : Bool
        matched = super
        if matched && !@hole.null?
          @hole.value = stored.value
        end
        matched
      end
    end

    compose DissocItemMut, Dissoc(Item, Int32), ItemStored, Authored, KInitialize do
      def initialize(*args, @hole : Term*, @author, **kwargs)
        super(*args, **kwargs)
      end

      def match?(stored : Item) : Bool
        matched = super
        if matched && !@hole.null?
          @hole.value = stored.value
        end
        matched
      end
    end
  end
end

require "./dict/items_view"
