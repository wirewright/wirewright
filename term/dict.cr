module Ww
  # Represents a dictionary: an immutable, persistent collection of key-value
  # pairs supporting efficient, near-O(1) insert, delete, and lookup.
  class Term::Dict
    include ITerm

    private alias ItemNode = Pf::Core::Node(Item)
    private alias PairNode = Pf::Core::Node(Pair)

    private struct Item
      getter index, value

      def initialize(@index : Int32, @value : Term)
      end
    end

    private struct Pair
      getter key, value

      def initialize(@key : Term, @value : Term)
      end
    end

    # An enumerable over dictionary pairs.
    private struct PairEnumerable
      include Enumerable({Term, Term})

      def initialize(@dict : Dict)
      end

      def each(& : {Term, Term} ->)
        @dict.each_entry { |k, v| yield({k, v}) }
      end
    end

    # An enumerable over dictionary items (keys 0 through n where n is the size
    # of the dictionary).
    private struct ItemEnumerable
      include Enumerable(Term)

      def initialize(dict : Dict)
        @items = dict.items
      end

      def each(& : Term ->)
        @items.each { |item| yield item }
      end
    end

    # Commits allow you to compose multiple edits into one, big edit of a dict.
    # Thus you avoid having to create many useless intermediate copies.
    class Commit
      @@id : Atomic(Pf::Core::AuthorId) = Atomic(Pf::Core::AuthorId).new(Pf::Core::AUTHOR_FIRST)
      class_getter id

      # :nodoc:
      def self.genid
        @@id.add(1)
      end

      @dict : Dict?

      protected def initialize(@parent : Dict, @fiber : UInt64)
        @id = Pf::Core::AuthorId.new(Commit.genid)
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

      # Runs `Dict#[]?` on the dictionary built so far.
      def []?(key : K) : V?
        (@dict || @parent)[key]?
      end

      # Runs `Dict#[]` on the dictionary built so far.
      def [](key : K) : V
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
    @hash : UInt64?

    def initialize
      @nitems = 0
      @npairs = 0

      @items = EMPTY_ITEM_NODE
      @pairs = EMPTY_PAIR_NODE
    end

    protected def initialize(@items, @pairs, @nitems, @npairs)
    end

    # Must be possible to do `initialize(*state)`. Must not include any cached
    # data: dictionaries constructed from `state` are expected be mutated without
    # notice -- and stale cache will make the dictionary dysfunctional.
    protected def state
      {@items, @pairs, @nitems, @npairs}
    end

    # Yields a `Commit` object so that you can build a dictionary without having
    # to produce many useless intermediate copies. See also: `#transaction`.
    def self.build(& : Commit ->)
      EMPTY.transaction { |commit| yield commit }
    end

    # Returns `true` if this dictionary contains items only.
    def itemsonly? : Bool
      @npairs.zero?
    end

    # Returns `true` if this dictionary contains pairs only.
    def pairsonly? : Bool
      @nitems.zero?
    end

    # Returns the number of entries in this dictionary.
    def size : Int32
      @nitems + @npairs
    end

    # Returns `true` if this dictionary contains no entries.
    def empty? : Bool
      size.zero?
    end

    # Returns `true` if this dictionary contains the given *key*.
    def includes?(key) : Bool
      !!self[key]?
    end

    # :nodoc:
    def at?(key : Term::Num) : Term?
      return at_default?(key) unless key.whole? && key.positive?
      return at_default?(key) unless key < Term[@nitems]
      return at_default?(key) unless coat = @items.fetch?(Probes::FetchItem.new(key.to_i))

      entry, *_ = coat
      entry.value
    end

    # :nodoc:
    def at?(key : ITerm) : Term?
      at_default?(key)
    end

    private def at_default?(key : ITerm) : Term?
      return unless coat = @pairs.fetch?(Probes::FetchPair.new(key.upcast))

      entry, *_ = coat
      entry.value
    end

    # Returns the value associated with the given *key*, or nil if *key*
    # is not associated with any value.
    def at?(key) : Term?
      at?(Term[key])
    end

    # Returns the value associated with the given *key*, or raises `KeyError`
    # if *key* is not associated with any value.
    def at(key) : Term
      at?(key) || raise KeyError.new
    end

    # Alias of `at`.
    def [](key) : Term
      at(key)
    end

    # Alias of `at?`.
    def []?(key) : Term?
      at?(key)
    end

    # Traverses nested dictionaries for each key in *keys*, returns the value that
    # was reached last. Returns `nil` if some key was not found during traversal.
    def dig?(*keys) : Term?
      keys.reduce(self) { |dict, key| dict.at?(key) || return }
    end

    # Same as `dig?`, but raises `KeyError` instead of returning `nil` if some key
    # was not found during traversal.
    def dig(*keys) : Term
      dig?(*keys) || raise KeyError.new
    end

    # Alias of `dig`.
    def [](*keys) : Term
      dig(*keys)
    end

    # Alias of `dig?`.
    def []?(*keys) : Term?
      dig?(*keys)
    end
    
    # Yields each entry from this dictionary.
    def each_entry(& : Term, Term ->) : Nil
      @items.each { |entry| yield Term.of(entry.index), entry.value }
      @pairs.each { |entry| yield entry.key, entry.value }
    end

    # Returns an enumerable based on `each_entry`.
    def ee : Enumerable({Term, Term})
      PairEnumerable.new(self)
    end

    # Returns an enumerable for items found in this dictionary.
    #
    # See also: `items`.
    def ie : Enumerable(Term)
      ItemEnumerable.new(self)
    end

    # Returns `true` if all entries of `self` are included in *other*.
    def subset_of?(other : Dict) : Bool
      other.size >= size && ee.all? { |k, v| other[k]? == v }
    end

    # Returns `true` if all entries of *other* are included in `self`.
    def superset_of?(other : Dict) : Bool
      other.subset_of?(self)
    end

    # :nodoc:
    def with(key : Term::Num, value : Term) : Dict
      return with_default(key, value) unless key.whole? && key.positive?
      return with_default(key, value) unless key <= Term[@nitems]

      index = key.to_i

      added, items = @items.add(Probes::AssocItemImm.new(index, value))
      unless added # Overridden or completely unchanged
        return @items.same?(items) ? self : Dict.new(items, @pairs, @nitems, @npairs)
      end

      state = Gap.promote(index + 1,
        nitems: @nitems + 1,
        npairs: @npairs,
        items: items,
        pairs: @pairs,
      )

      Dict.new(*state)
    end

    # :nodoc:
    def with(key : ITerm, value : Term) : Dict
      with_default(key, value)
    end

    private def with_default(key : ITerm, value : Term) : Dict
      added, pairs = @pairs.add(Probes::AssocPairImm.new(key.upcast, value))
      unless added # Overridden or completely unchanged
        return @pairs.same?(pairs) ? self : Dict.new(@items, pairs, @nitems, @npairs)
      end

      Dict.new(@items, pairs, @nitems, @npairs + 1)
    end

    # Returns a copy of this dictionary extended with an association between
    # *key* and *value*. If *key* was present already its value is updated
    # in the copy.
    #
    # If *value* is `nil` acts as `without`. This is mainly useful during
    # conversion from JSON (via `Term.[]`), treating `null` as absence. [x]
    def with(key, value) : Dict
      if value.nil? || value.is_a?(JSON::Any) && value.raw.nil?
        return without(key)
      end

      self.with(Term[key], Term.of(value))
    end

    # :nodoc:
    def without(key : Term::Num) : Dict
      return without_default(key) unless key.whole? && key.positive?
      return without_default(key) unless key < Term[@nitems]

      state = Gap.demote(
        end_exclusive: key.to_i,
        lchop: true, # < will remove the item
        nitems: @nitems,
        npairs: @npairs,
        items: @items,
        pairs: @pairs,
      )

      Dict.new(*state)
    end

    # :nodoc:
    def without(key : ITerm) : Dict
      without_default(key)
    end

    private def without_default(key : ITerm) : Dict
      removed, pairs = @pairs.delete(Probes::DissocPairImm.new(key.upcast))
      removed ? Dict.new(@items, pairs, @nitems, @npairs - 1) : self
    end

    # :nodoc:
    def without(key : Term) : Dict
      without(key.downcast)
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # an association with the given *key*.
    def without(key) : Dict
      without(Term[key])
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # associations with any of the given *keys*.
    def without(keys : Enumerable) : Dict
      transaction do |commit|
        keys.each { |key| commit.without(key) }
      end
    end

    # :ditto:
    def without(*keys) : Dict
      without(keys)
    end

    protected def with!(key : Term::Num, value : Term, author) : Dict
      return with_default!(key, value, author) unless key.whole? && key.positive?
      return with_default!(key, value, author) unless key <= Term[@nitems]

      index = key.to_i
      added, @items = @items.add(Probes::AssocItemMut.new(index, value, author: author))

      if added # Try to promote successive (index + 1) pairs to items, if any.
        @items, @pairs, @nitems, @npairs = Gap.promote(
          end_exclusive: index + 1,
          author: author,
          nitems: @nitems + 1, # < new item was added
          npairs: @npairs,
          items: @items,
          pairs: @pairs,
        )
      end

      self
    end

    protected def with!(key : ITerm, value : Term, author) : Dict
      with_default!(key, value, author)
    end

    protected def with_default!(key : ITerm, value : Term, author) : Dict
      added, @pairs = @pairs.add(Probes::AssocPairMut.new(key.upcast, value, author: author))
      if added # Added a new pair
        @npairs += 1
      end

      self
    end

    protected def with!(key, value, author) : Dict
      with!(Term[key], Term.of(value), author)
    end

    protected def without!(key : Term::Num, author) : Dict
      return without_default!(key, author) unless key.whole? && key.positive?
      return without_default!(key, author) unless key < Term[@nitems]

      @items, @pairs, @nitems, @npairs = Gap.demote(
        end_exclusive: key.to_i,
        author: author,
        lchop: true, # < will remove the item
        nitems: @nitems,
        npairs: @npairs,
        items: @items,
        pairs: @pairs,
      )

      self
    end

    protected def without!(key : ITerm, author) : Dict
      without_default!(key, author)
    end

    private def without_default!(key : ITerm, author) : Dict
      removed, @pairs = @pairs.delete(Probes::DissocPairMut.new(key.upcast, hole: Pointer(Term).null, author: author))
      if removed
        @npairs -= 1
      end

      self
    end

    protected def without!(key : Term, author) : Dict
      without!(key.downcast, author)
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
    def transaction(& : Commit ->) : Dict
      commit = Commit.new(self, Pf.fiber_id)
      yield commit
      commit.resolve
    end

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
      return instance if @hash.nil? && !instance.@hash.nil?
      return self if state == instance.state

      instance
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
    def pluck(*keys, **rest)
      Dict.build do |commit|
        keys.each { |key| pluck(key, commit) }
        rest.each { |k, v| commit.with(k, v) }
      end
    end

    # Shallow merge.
    #
    # Merges this dictionary with a *newer* one. If two keys are equal the value
    # from *newer* is preferred.
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

    # Deep merge.
    #
    # Merges this dictionary with a *newer* one. If two keys are equal and both
    # values are dictionaries, merging descends recursively. Otherwise, *newer*'s
    # value is preferred.
    def &(newer) : Dict
      newer = newer.as_d

      return newer if empty?
      return self if newer.empty?

      # Don't waste time allocating commits for singleton dicts.

      if size == 1
        k, v1 = ee.first
        return newer.with(k, v1) unless v2 = newer[k]?
        return newer unless v1.type.dict? && v2.type.dict?
        return newer.with(k, v1.unsafe_as_d & v2.unsafe_as_d)
      end

      if newer.size == 1
        k, v2 = newer.ee.first
        unless (v1 = self[k]?) && v1.type.dict? && v2.type.dict?
          return self.with(k, v2)
        end
        return self.with(k, v1.unsafe_as_d & v2.unsafe_as_d)
      end

      # Allocate commits otherwise.

      if size < newer.size
        newer.transaction do |commit|
          each_entry do |k, v1|
            unless v2 = newer[k]?
              commit.with(k, v1)
              next
            end
            next unless v1.type.dict? && v2.type.dict?
            commit.with(k, v1.unsafe_as_d & v2.unsafe_as_d)
          end
        end
      else
        transaction do |commit|
          newer.each_entry do |k, v2|
            if (v1 = self[k]?) && v1.type.dict? && v2.type.dict?
              commit.with(k, v1.unsafe_as_d & v2.unsafe_as_d)
              next
            end
            commit.with(k, v2)
          end
        end
      end
    end

    # In practice `partition` and `pairs` are called very often. Therefore by
    # losing 8 bytes per existing dict, we gain a lot more -- because otherwise
    # for each new dict created by `partition` we lose 32 bytes (if without @pairsonly).
    # Thus despite having more to store on every dict, we get better memory
    # performance overall.
    @pairsonly : Dict?

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
    def partition : {Dict::ItemsView, Dict}
      {items, pairs}
    end

    # Returns the items part of `partition` (see the latter for more info).
    #
    # This method is more efficient than using `partition` and discarding
    # the pairs part.
    def items : Dict::ItemsView
      ItemsView.new(@items, b: 0, e: @nitems, available: @nitems)
    end

    # Returns the pairs part of `partition` (see the latter for more info).
    def pairs : Dict
      @pairsonly ||= Dict.new(EMPTY_ITEM_NODE, @pairs, 0, @npairs)
    end

    def to_ir : IR
      if empty?
        children = nil
      else
        children = Array(IR).new(size*2)
        each_entry do |k, v|
          children << k.to_ir
          children << v.to_ir
        end
      end

      IR.new(IR::Type::Dict, payload: nil, children: children)
    end

    def hash(hasher)
      unless result = @hash
        result = self.class.hash

        each_entry do |key, value|
          copy = Crystal::Hasher.new
          copy = key.hash(copy)
          copy = value.hash(copy)
          result &+= copy.result
        end

        @hash = result
      end

      result.hash(hasher)
    end

    # Returns `true` if this and *other* dictionaries are equal. Returns `false` otherwise.
    def ==(other : Dict) : Bool
      return true if same?(other)
      return false unless @nitems == other.@nitems && @npairs == other.@npairs
      return false if (hx = @hash) && (hy = other.@hash) && hx != hy

      each_entry do |k, v1|
        return false unless v2 = other[k]?
        return false unless v1 == v2
      end

      true
    end

    # :nodoc:
    def ==(other) : Bool
      false
    end

    def inspect(io)
      if empty?
        io << "{}"
        return
      end

      items, entries = partition

      if entries.empty?
        io << "["
        items.join(io, ", ") { |item| io << item }
        io << "]"
      else
        io << "{"
        ee.map { |(k, v)| {k.inspect, v.inspect} }
          .sort_by! { |k, _| k }
          .join(io, ", ") { |(k, v)| io << k << ": " << v }
        io << "}"
      end
    end
  end

  private module Term::Dict::Gap
    # Demotes *end exclusive*-th and successive items to become pairs. Changes
    # are authored by *author*, thereby saving a few copies.
    #
    # Returns new `Dict` state tuple from which you can initialize a `Dict`,
    # or mutate an existing one. See also: `Dict#state`.
    #
    # ```text
    #  ITEMS                     PAIRS
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
    # If *lchop* is `true` the *end exclusive*-th item is removed rather than
    # becoming a pair.
    #
    # ```text
    #  ITEMS                     PAIRS
    # +-----------------------+
    # | 0   1   2   3   4   5 |  x   y
    # +-----------------------+
    #
    #              | Gap.demote(3, lchop: true)
    #              v
    #  ITEMS         PAIRS
    # +-----------+
    # | 0   1   2 |  4   5   x   y
    # +-----------+
    # ```
    def self.demote(end_exclusive, lchop, items, pairs, nitems, npairs, author = nil)
      (end_exclusive...nitems).each do |index|
        hole = uninitialized Term
        author ||= Commit.genid

        removed, items = items.delete(Probes::DissocItemMut.new(index, hole: pointerof(hole), author: author))
        unless removed
          raise "BUG: Gap.demote(): invalid state: item was not removed"
        end

        nitems -= 1

        next if lchop && index == end_exclusive

        added, pairs = pairs.add(Probes::AssocPairMut.new(Term.of(index), value: hole, author: author))
        unless added
          raise "BUG: Gap.demote(): invalid state: pair was not added"
        end

        npairs += 1
      end

      {items, pairs, nitems, npairs}
    end

    # Promotes *end exclusive*-th pair and successive pairs to become pairs.
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
      include Pf::Core::IProbeFetch(E)

      def initialize(@key : K)
      end

      abstract def keyof(stored : E) : K

      def path : UInt64
        Pf.hash64(@key)
      end

      def match?(stored : E) : Bool
        @key == keyof(stored)
      end
    end

    # Includers are add probes with stored entry type `E` and key type `K`.
    module Assoc(E, K)
      include Pf::Core::IProbeAdd(E)

      getter path : UInt64

      @key : K
      @value : Term

      abstract def keyof(stored : E) : K

      def match?(stored : E) : Bool
        @key == keyof(stored)
      end

      def replace?(stored : E) : Bool
        @value != stored.value
      end

      def value : E
        E.new(@key, @value)
      end
    end

    # Includers are delete probes with stored entry type `E` and key type `K`.
    module Dissoc(E, K)
      include Pf::Core::IProbeDelete(E)

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
      def author : Pf::Core::AuthorId
        Pf::Core::AUTHOR_NONE
      end
    end

    # Includers have authorship rights, therefore they can copy the underlying
    # nodes only once before changing them, and then can change them without
    # copying forever.
    module Authored
      getter author : Pf::Core::AuthorId
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
        @path = Pf.hash64(@key)
      end
    end

    module KInitialize
      def initialize(@key)
        @path = Pf.hash64(@key)
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
