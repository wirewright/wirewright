# TODO: our current dictionary implementation is very very bad. It has served
# me for over a year with minor changes, but now it is becoming more and more
# cumbersome to use and extend it. It isn't as simple as it should be, with a
# lot of overcomplication; nor as fast as it should be. When I first wrote it
# only a small portion of the system relied on Dicts. Now almost everything relies
# on dicts.
#
# So, a quick and dirty "blueprint of a blueprint":
#
#  - Finger trees for item-like storage (Pf::Vec)
#  - USet32 for itemspart tracking. All item keys are stored in the finger tree, but
#    we only consider as *itemspart* ones that start at 0 and continue until the first
#    gap (aka USet32#prefix).
#  - HAMT for pairspart (updated Pf::Map). Post-prefix USet32 is also pairspart.
#
# Both Pf::Map and Pf::Vec must support the same type of metadata object, which they place
# on every node and which they let clients query later on. This enables something I call
# "sketch-" or metadata-guided descent. This should speed up ⟨...⟩ which are e.g. heavily
# used in editR; preventing a full scan every time. The pattern matching engine can tell us
# a great deal about what the thing we're trying to find is "like", and the trees (finger tree
# or HAMT) can help us get there.
#
# Pf::Map should be implemented using 16-way branching instead of 32 at each node. This lets
# us do some cool optimizations/bitwise trickery. Sparse16 must be paged, meaning we don't call
# malloc() every time but instead refer to "pages" allocated ahead of time for each type of interest.
# This is a trade-off: increased GC strain versus very fast (<~20ns with locks, ~10ns without locks
# on my machine), and sometimes non-copying appends.
#
# - Pf::Map MUST be structural because we require cheap equality. That is, different order gives
#   the same structure. Collisions are stored in a separate, lexicographically sorted slice, which
#   is copied fully on each append.
# - Pf::Map nodes and Pf::Vec nodes MUST be content-addressed (see also: hash-consing). We waste
#   an enormous amount of memory constructing things that end up being the same. The same behavior
#   also generates the worst case for equality for us: two dicts that are different objects but
#   same by content. Content-addressing turns must comparisons to object address comparisons.
#
# As for per-node metadata, we must store:
#  - Key sketch (64-bit Bloom filter containing keys in the node and its subtree)
#  - Value sketch (64-bit Bloom filter containing string, number values in the node and its subtree; `true` and `false` both have a reserved bit)
#  - Symbol sketch (64-bit Bloom filter containing symbols in the node and its subtree).
#  - Population (64-bit: u16 nums, u16 strs, u16 syms, u8 trues, u8 falses); MAX of each is reserved for "infinity".
#  - Depth (32-bit: maximum depth of node and its subtree)
#  - Size (32-bit: number of items/entries in node and its subtree)
#
# This totals at 40 bytes of metadata per node. The cap is 64 bytes, so we still have some room left.
# Although the less memory it uses, the better, of course.
#
# NOTE: Pf manages the hashcode since its semantics differ. Hashcode can be read
#   off the node..
#
# Pf must update (recalculate) Metadata with each new update to the node. Each update
# to a node must trigger the recalculation of metadata on the updated node.
#
# For content-addressing, we first simulate adding to the hash or removing from it instead
# of allocating nodes etc. If the resulting hash's bucket exists in the cache and satisfies
# the change we return that instead of allocating. Otherwise we construct a new node and update
# the cache appropriately. It is important that the cache is thread-safe. A simple solution,
# such as a RW lock, could work, although we'd have to "double tap": first take the R lock and
# see if the thing exists, and then take a W lock and also check; and then, while having the W
# lock, if the thing is missing, we construct and insert and return it.
#
# It would be nice for nodes to have the size cap of 64 bytes.
#
# It would be nice for small dictionaries (e.g. size < 16) to be optimized, and be e.g.
# simply Slices under the hood. However, I'm not sure how nicely our Metadata will play
# with this. If a 2-element Term::Dict takes 56 bytes (e.g. 40 Metadata + 8 bytes hashcode + 8 bytes ptr),
# then is that *really* better than just nil-initializing some pointers and focusing instead
# on the memory consumption of USet32, Vec, and Map *individually*? As in, *they* are the ones
# doing the small-size opt, not Dict. Also, by doing small-size opt on Dict, we'll lose content-
# addressing...The metadata problem still persists if we do small-size in Dict rather than on Dict,
# but at least it is displaced somewhat manageably. In that case, Dict could simply be
# USet32 + Vec + Map. All of them are still pretty large though. If we assume 24 + 16 + 16 then
# it's still 56 bytes of control per dict (not even payload!!!) And then each node of Vec and Map
# is maybe 64 bytes but probably more like 80-ish bytes!!! This is crazy huge!

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

    alias Sketch = UInt64

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

    # Returns the maximum-ever depth of this dictionary.
    #
    # In other words, this method **does not** return the current maximum depth;
    # it can be said to return the "maximum maximum depth", that is, the largest
    # depth seen throughout the history of this dict.
    def maxdepth : UInt32
      @maxdepth + 1
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

    private def at_default?(key : Term::Any) : Term?
      # why the f is it called a COAT?
      return unless coat = @pairs.fetch?(Probes::FetchPair.new(Term.of(key)))

      entry, *_ = coat
      entry.value
    end

    # :nodoc:
    @[Dncast]
    def []?(key : Term::Num) : Term?
      return at_default?(key) unless i = index32?(key)
      return at_default?(key) unless coat = @items.fetch?(Probes::FetchItem.new(i.to_i))

      entry, *_ = coat
      entry.value
    end

    # :nodoc:
    @[Dncast]
    def []?(key : Term::Any) : Term?
      at_default?(key)
    end

    # Returns the value associated with the given *key*, or nil if *key*
    # is not associated with any value.
    @[Dncast]
    def []?(key) : Term?
      self[Term[key]]?
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
      bucket = Term.hashcode(symbol) % Sketch.bit_width

      sketch.bit(bucket) == 1
    end

    def self.mix(sketch : Sketch, value : Term)
      case value.type
      when .symbol?
        bucket = Term.hashcode(value.unsafe_as_sym) % Sketch.bit_width

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

    # Recurses into entry values only.
    #
    # Counts itself too (smallest possible value is 1).
    @[Dncast]
    def fresh_maxdepth : Magnitude
      maxdepth = Magnitude.new(1)

      each_entry do |k, v|
        if vdict = v.as_d?
          maxdepth = Math.max(maxdepth, vdict.fresh_maxdepth + 1)
        end
      end

      maxdepth
    end

    @[Dncast]
    def depth : UInt32
      maxdepth = 0u32

      each_entry do |k, v|
        next unless child = v.as_d?

        maxdepth = Math.max(maxdepth, child.depth)
      end

      1u32 + maxdepth
    end

    record Population, numbers : Magnitude, symbols : Magnitude, strings : Magnitude, booleans : Magnitude do
      def self.zero
        zero = Magnitude.new(0)
        new(zero, zero, zero, zero)
      end

      def +(other : Population)
        Population.new(
          numbers + other.numbers,
          symbols + other.symbols,
          strings + other.strings,
          booleans + other.booleans,
        )
      end

      def +(other : Term)
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

      def total : Magnitude
        numbers + strings + symbols + booleans
      end
    end

    # TODO: cache on dicts
    @[Dncast]
    def population
      ee.sum(Population.zero) { |_, v| v }
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
    def with(key, & : Term? -> _)
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

    @[Dncast]
    def without_item(index)
      replace(Term[index]) { }
    end

    @[Dncast]
    def without_item(&)
      dict = self

      each_item_with_index do |item, index|
        next unless yield item

        dict = dict.without_item(index)
      end

      dict
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
    def replace(index : Term::Num, & : Term::Dict::Commit ->)
      replace(index...index + 1) { |commit| yield commit }
    end

    private def with_default(key : Term::Any, value : Term) : Dict
      added, pairs = @pairs.add(Probes::AssocPairImm.new(Term.of(key), value))
      unless added # Overridden or completely unchanged
        return @pairs.same?(pairs) ? self : Dict.new(@items, pairs, Dict.mix(@sketch, value), Dict.mixdepth(@maxdepth, value))
      end

      Dict.new(@items, pairs, Dict.mix(@sketch, value), Dict.mixdepth(@maxdepth, value))
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

    # Returns `true` if all keys shared by `self` and *other* have equal values.
    # If no keys are shared, returns `true`.
    def agrees_with?(other : Dict) : Bool
      if size < other.size
        each_entry do |key, value0|
          next unless value1 = other[key]?
          next if value0 == value1
          return false # disagrees
        end
      else
        other.each_entry do |key, value0|
          next unless value1 = self[key]?
          next if value0 == value1
          return false # disagrees
        end
      end

      true # agrees
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
