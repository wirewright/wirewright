# TODO: probabilistic pattern trace (similar to sketch).
# Patterns toggle their object id % 64 in a dictionary's bitmap. This means
# (multilevel) skips become even cheaper. Especially with stuff such as
# cursor search in parsing/µsoma, we want deep searches to be cheap.

class ::Pf::Core::Node(T)
  # :nodoc:
  record CheckoutItem(T), item : T 
  # :nodoc:
  record CheckoutNode(T), node : Node(T)

  # FIXME: uses sample & shuffle! which use rand which isn't thread safe ?!
  def each_randomized(& : T ->) : Nil
    # To ensure fairness we must interleave items with nodes, which makes all
    # of this a bit more involved. We must ensure fairness in all cases, even
    # if performance is severely harmed -- the callers of this method usually
    # deal with infinities and letting an infinity win would be an error.

    # Scratch buffer that can hold up to 32 items + 32 children.
    scratch = uninitialized UInt8[64]
    commands = [CheckoutNode.new(self)] of CheckoutNode(T) | CheckoutItem(T)
    
    while command = commands.pop?
      case command
      in CheckoutItem(T)
        yield command.item
      in CheckoutNode(T)
        node = command.node

        midpoint = node.@items.size
        indices = Slice(UInt8).new(scratch.to_unsafe, midpoint + node.@children.size)

        # Shuffling will mix item and node indices so we have to mark
        # them beforehand.

        (0...midpoint).each do |index|
          indices[index] = index.to_u8 << 1
        end

        (midpoint...indices.size).each_with_index do |index, ord|
          indices[index] = (ord.to_u8 << 1) | 1u8
        end

        indices.shuffle!
        indices.each do |index|
          # We must insert the command at a random index for fairness. Otherwise
          # upper values may block lower values forever.
          target = (0..commands.size).sample

          if (index & 1u8).zero?
            commands.insert(target, CheckoutItem.new(node.@items.to_unsafe[index >> 1]))
          else
            commands.insert(target, CheckoutNode.new(node.@children.to_unsafe[index >> 1]))
          end
        end
      end
    end
  end
end

module Ww
  # Represents a dictionary: an immutable, persistent collection of key-value
  # pairs supporting efficient, near-O(1) insert, delete, and lookup.
  class Term::Dict
    include ITerm

    # :nodoc:
    alias ItemNode = Pf::Core::Node(Item)

    # :nodoc:
    alias PairNode = Pf::Core::Node(Pair)

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

    # :nodoc:
    #
    # An enumerable over dictionary pairs.
    struct EntryEnumerable
      include Enumerable({Term, Term})

      def initialize(@dict : Dict)
      end

      def each(& : {Term, Term} ->)
        @dict.each_entry { |k, v| yield({k, v}) }
      end
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

      # Runs `Dict::ItemsView#size` on the dictionary built so far.
      def itemsize : Int32
        (@dict || @parent).items.size
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
      def concat(ee : Enumerable(Term), & : Term -> Term) : self
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
    @hash : UInt64?

    def initialize
      @nitems = 0
      @npairs = 0

      @items = EMPTY_ITEM_NODE
      @pairs = EMPTY_PAIR_NODE

      @sketch = 0u64
    end

    protected def initialize(@items, @pairs, @nitems, @npairs, @sketch)
    end

    # Must be possible to do `initialize(*state)`. Must not include any cached
    # data: dictionaries constructed from `state` are expected be mutated without
    # notice -- and stale cache will make the dictionary dysfunctional.
    protected def state
      {@items, @pairs, @nitems, @npairs, @sketch}
    end

    # Yields a `Commit` object so that you can build a dictionary without having
    # to produce many useless intermediate copies. See also: `#transaction`.
    def self.build(& : Commit ->)
      EMPTY.transaction { |commit| yield commit }
    end

    macro give(&block)
      ::Ww::Term::Dict.give({{block.args.map(&.symbolize).splat}}) {{ block }}
    end

    def self.give(arg, &)
      {build { |commit| yield commit }}
    end

    def self.give(arg, *args, &)
      dicts = nil
      dict = build do |commit|
        dicts = give(*args) do |*commits|
          yield commit, *commits
        end
      end
      {dict, *dicts.not_nil!}
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

    # Shorthand for `!empty?`.
    def nonempty? : Bool
      !empty?
    end

    # Returns `true` if this dictionary contains the given *key*.
    def includes?(key) : Bool
      !!self[key]?
    end

    def index?(term) : Term::Num?
      return unless index = Term[term].as?(Term::Num)
      return unless index.in?(Term[0]...Term[items.size])
      index
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

    # Returns the value associated with the given *key*, or *default* if *key*
    # is not associated with any value.
    def at(key, *, default) : Term
      at?(key) || Term.of(default)
    end

    # Transforms the value associated with the given *key* using the block, or
    # returns *orelse* without transforming it if *key* is not associated with
    # any value.
    def at(key, *, orelse, &) : Term
      return Term.of(orelse) unless value = at?(key)

      Term.of(yield value)
    end

    # Alias of `at`.
    def [](*args, **kwargs) : Term
      at(*args, **kwargs)
    end

    # Alias of `at?`.
    def []?(*args, **kwargs) : Term?
      at?(*args, **kwargs)
    end

    # Traverses nested dictionaries for each key in *keys*, returns the value that
    # was reached last. Returns `nil` if some key was not found during traversal.
    def dig?(*keys) : Term?
      keys.reduce(self) { |dict, key| dict.at?(key) || return }
    end

    # Same as `dig?`, but raises `KeyError` instead of returning `nil` if some key
    # was not found during traversal.
    def dig(*keys) : Term
      dig?(*keys) || raise KeyError.new("#{keys}")
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

    # Fair, randomized iteration over entries of this dict.
    #
    # This has fairly specific use cases, for instance in absolute rewriting
    # (absr). In absr we would like to achieve progress even in the presence
    # of potentially infinite rewrites. That is, an infinite rewrite should
    # not block progress of other, potentially non-infinite rewrites.
    def each_entry_randomized(& : Term, Term ->) : Nil
      @items.each_randomized { |entry| yield Term.of(entry.index), entry.value }
      @pairs.each_randomized { |entry| yield entry.key, entry.value }
    end

    # Yields each item from this dictionary followed by its index. **Items are yielded
    # out of order**.
    def each_item_with_index(& : Term, Int32 ->) : Nil
      @items.each { |entry| yield entry.value, entry.index }
    end

    def each_item_unordered(& : Term ->) : Nil
      @items.each { |entry| yield entry.value }
    end

    # Yields each pair from this dictionary.
    def each_pair(& : Term, Term ->)
      @pairs.each { |entry| yield entry.key, entry.value }
    end

    # Returns an enumerable based on `each_entry`.
    def ee : Enumerable({Term, Term})
      EntryEnumerable.new(self)
    end

    # Returns an enumerable for items found in this dictionary.
    #
    # See also: `items`.
    def ie : Enumerable(Term)
      ItemEnumerable.new(self)
    end

    # Returns an enumerable based on `each_pair`.
    def pe : Enumerable({Term, Term})
      pairs.ee
    end

    # Returns `true` if all entries of `self` are included in *other*.
    def subset_of?(other : Dict) : Bool
      other.size >= size && ee.all? { |k, v| other[k]? == v }
    end

    # Returns `true` if all entries of *other* are included in `self`.
    def superset_of?(other : Dict) : Bool
      other.subset_of?(self)
    end

    def like?(other) : Bool
      (@sketch & other.@sketch) == other.@sketch
    end

    def sketch_superset_of?(subset : UInt64) : Bool
      (@sketch & subset) == subset
    end

    def probably_includes?(symbol : Term::Sym) : Bool
      Dict.probably_includes?(@sketch, symbol)
    end

    def self.probably_includes?(sketch : UInt64, symbol : Term::Sym) : Bool
      bucket = symbol.hash % 64
      sketch.bit(bucket) == 1
    end

    def self.mix(sketch : UInt64, value : Term)
      case value.type
      when .symbol?
        bucket = value.unsafe_as_sym.hash % 64
        sketch | (0b1u64 << bucket)
      when .dict?
        sketch | value.unsafe_as_d.@sketch
      else
        sketch
      end
    end

    # :nodoc:
    def with(key : Term::Num, value : Term) : Dict
      return with_default(key, value) unless key.whole? && key.positive?
      return with_default(key, value) unless key <= Term[@nitems]

      index = key.to_i

      added, items = @items.add(Probes::AssocItemImm.new(index, value))
      unless added # Overridden or completely unchanged
        return @items.same?(items) ? self : Dict.new(items, @pairs, @nitems, @npairs, Dict.mix(@sketch, value))
      end

      state = Gap.promote(index + 1,
        nitems: @nitems + 1,
        npairs: @npairs,
        items: items,
        pairs: @pairs,
      )

      Dict.new(*state, Dict.mix(@sketch, value))
    end

    # :nodoc:
    def with(key : ITerm, value : Term) : Dict
      with_default(key, value)
    end

    # Returns a copy of this dictionary extended with an association between
    # *key* and *value*. If *key* was present already its value is updated
    # in the copy.
    #
    # If *value* is `nil` acts as `without`. This is mainly useful during
    # conversion from JSON (via `Term.[]`), treating `null` as absence.
    def with(key, value) : Dict
      if value.nil? || value.is_a?(JSON::Any) && value.raw.nil?
        return without(key)
      end

      self.with(Term[key], Term.of(value))
    end

    # TODO: have an optimized version of this
    def with(key, &)
      self.with(key, yield self[key]?)
    end

    def append(item)
      self.with(items.size, item)
    end

    private def with_default(key : ITerm, value : Term) : Dict
      added, pairs = @pairs.add(Probes::AssocPairImm.new(key.upcast, value))
      unless added # Overridden or completely unchanged
        return @pairs.same?(pairs) ? self : Dict.new(@items, pairs, @nitems, @npairs, Dict.mix(@sketch, value))
      end

      Dict.new(@items, pairs, @nitems, @npairs + 1, Dict.mix(@sketch, value))
    end

    def follow?(keys : Enumerable(Term)) : Term?
      Term.of(keys.reduce(self) { |dict, key| dict[key]? || return })
    end

    def follow(keys : Enumerable(Term)) : Term
      follow?(keys) || raise KeyError.new
    end

    def where(key, eq value) : Dict
      self.with(key, value)
    end

    def where(key, *keys, eq value) : Dict
      self.with(key, (self[key]? || Term[]).where(*keys, eq: value))
    end

    def where(key, *keys, eq value : Nil) : Dict
      return self unless v0 = self[key]?
      v1 = v0.where(*keys, eq: nil)
      v1.empty? ? without(key) : self.with(key, v1)
    end

    def morph(place)
      where(*place[...-1], eq: place[-1])
    end

    def morph(place, *places)
      morph(place).morph(*places)
    end

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

    def where(prefix : ItemsView, eq value) : Dict
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

        self.with(key, value0.where(prefix.move(1), value))
      end
    end

    # :nodoc:
    def without(key : Term::Num) : Dict
      return without_default(key) unless key.whole? && key.positive?
      return without_default(key) unless key < Term[@nitems]

      state = Gap.demote(
        end_exclusive: key.to_i,
        rdrop: true, # < will remove the item
        nitems: @nitems,
        npairs: @npairs,
        items: @items,
        pairs: @pairs,
      )

      Dict.new(*state, @sketch)
    end

    # :nodoc:
    def without(key : ITerm) : Dict
      without_default(key)
    end

    # :nodoc:
    def without(key : Term) : Dict
      without(key.downcast)
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # an association with the given *key*.
    def without(key) : Dict
      without(Term.of(key))
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # associations with any of the given *keys*.
    def without(*keys) : Dict
      transaction do |commit|
        keys.each { |key| commit.without(key) }
      end
    end

    private def without_default(key : ITerm) : Dict
      removed, pairs = @pairs.delete(Probes::DissocPairImm.new(key.upcast))
      removed ? Dict.new(@items, pairs, @nitems, @npairs - 1, @sketch) : self
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

      # If value is unchanged (e.g. with(0, :x) followed by with (0, :x)) nothing
      # will happen since the bit has already been set.
      @sketch = Dict.mix(@sketch, value)

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

      # If value is unchanged (e.g. with(0, :x) followed by with (0, :x)) nothing
      # will happen since the bit has already been set.
      @sketch = Dict.mix(@sketch, value)

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
        rdrop: true, # < will remove the item
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

    def subst1(term, replacement) : Dict
      term, replacement = Term.of(term), Term.of(replacement)

      replace { |_, v| v == term ? replacement : nil }
    end

    # Recursive, depth-first substitution using the substitution table *subt*.
    def subst(subt) : Dict
      subt = subt.as_d

      replace do |_, v0|
        if v1 = subt[v0]?
          v1
        elsif v0.type.dict?
          v0.unsafe_as_d.subst(subt).upcast
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
    def pluck(*keys, **rest) : Term::Dict
      Dict.build do |commit|
        keys.each { |key| pluck(key, commit) }
        rest.each { |k, v| commit.with(k, v) }
      end
    end

    # Returns a dictionary with entries whose keys are present in the enumerable *ee*.
    #
    # Keys present in *ee* but missing in `self` are skipped.
    def pluck(ee : Enumerable(Term)) : Term::Dict
      Dict.build do |commit|
        ee.each { |key| commit.with(key, self[key]?) }
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
    def &(newer : Dict) : Dict
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

    def ^(older : Dict) : Dict
      transaction do |commit|
        older.each_entry do |k, v0|
          next unless v0 == self[k]?

          commit.without(k)
        end
      end
    end

    def &-(other : Dict) : Dict?
      if empty? || other.empty?
        return self
      end

      result = transaction do |commit|
        other.each_entry do |k, v1|
          next unless v0 = self[k]?
          if v0.type.dict? && v1.type.dict? && !(v0.empty? && v1.empty?)
            commit.with(k, v0 &- v1)
          elsif v0 == v1
            commit.without(k)
          end
        end
      end

      result.empty? ? nil : result
    end

    def &-(other : Enumerable(Term)) : Dict
      transaction do |commit|
        other.each { |key| commit.without(key) }
      end
    end
    
    def items(b : Num, e : Num) : Dict
      return Term[] if b == e
      return items.collect if b == Term[0] && e == Term[size]
      return Term[] unless b < e <= size
      
      Term::Dict.build do |commit|
        (b...e).each do |key|
          commit.append(self[key])
        end
      end
    end

    def each_prefix(prefix : BiList(Term), term : Term::Dict, &fn : BiList(Term), Term ->)
      term.each_entry do |k, v|
        each_prefix(prefix.append(k), v.downcast, &fn)
      end
    end

    def each_prefix(prefix : BiList(Term), term : ITerm, &fn : BiList(Term), Term ->)
      fn.call(prefix, term.upcast)
    end

    def each_prefix(&fn : BiList(Term), Term ->)
      each_prefix(BiList(Term)[], self, &fn)
    end

    # Dict set intersection. Values are ignored; only key presence/absence is taken
    # into account. May mix keys/values from `self`/*other* for additional speedup
    # (set *mix* to `false` to disallow).
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

    # Dict entry intersection. Leaves entries common to `self` and *other*.
    def esect(other : Dict) : Dict
      Term::Dict.build do |commit|
        if size < other.size
          each_entry do |k, v|
            next unless other[k]? == v
            commit.with(k, v)
          end
        else
          other.each_entry do |k, v|
            next unless self[k]? == v
            commit.with(k, v)
          end
        end
      end
    end

    # Dict set subtraction. Values are ignored; only key presence/absence is taken
    # into account.
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


    # FIXME: extract into Nitrene
    def dfs(&fn : Term -> Bool) : Bool
      items.each do |item|
        return true if fn.call(item)
        next unless child = item.as_d?
        return true if child.dfs(&fn)
      end

      false
    end

    enum BfsResponse : UInt8
      Present
      Absent
      Bottom
    end

    # FIXME: extract into Nitrene
    def bfs1(depth : Int, &fn : Term -> Bool) : BfsResponse
      if depth.zero?
        return items.any?(&fn) ? BfsResponse::Present : BfsResponse::Absent
      end

      bottom_votes = 0

      items.each do |item|
        unless child = item.as_d?
          bottom_votes += 1
          next
        end

        case child.bfs1(depth - 1, &fn)
        in .present?
          return BfsResponse::Present
        in .absent?
        in .bottom?
          bottom_votes += 1
        end
      end

      items.size == bottom_votes ? BfsResponse::Bottom : BfsResponse::Absent
    end

    # FIXME: extract into Nitrene
    def bfs(&fn : Term -> Bool) : Bool
      (0..).each do |depth|
        case bfs1(depth, &fn)
        in .present?
          return true
        in .absent?
        in .bottom?
          return false
        end
      end
    end

    # Shallow diff. Returns `{entries present in self but absent in other, entries absent in self but present in other}`.
    def diff1(other) : {Dict, Dict}
      added = Term::Dict.build do |commit|
        each_entry do |k, v1|
          next if (v0 = other[k]?) && v0 == v1
          commit.with(k, v1)
        end
      end

      removed = Term::Dict.build do |commit|
        other.each_entry do |k, v0|
          next if k.in?(self)
          commit.with(k, v0)
        end
      end

      {added, removed}
    end

    def diff1x(other) : {Dict, Dict, Dict}
      added = Term[]
      changed = Term[]
      removed = Term[]

      added = Term::Dict.build do |added|
        changed = Term::Dict.build do |changed|
          each_entry do |k, v1|
            next if (v0 = other[k]?) && v0 == v1

            (v0 ? changed : added).with(k, v1)
          end
        end
      end

      removed = Term::Dict.build do |commit|
        other.each_entry do |k, v0|
          next if k.in?(self)
          commit.with(k, v0)
        end
      end

      {added, changed, removed}
    end

    def self.diff(k : Term, older : Dict, newer : Dict, added0, removed0) : {Dict, Dict}
      added, removed = diff(older, newer)

      {added.empty? ? added0 : added0.with(k, added),
       removed.empty? ? removed0 : removed0.with(k, removed)}
    end

    def self.diff(k : Term, older : ITerm?, newer : ITerm, added0, removed0) : {Dict, Dict}
      older == newer ? {added0, removed0} : {added0.with(k, newer), removed0}
    end

    def self.diff(k : Term, older : ITerm, newer : Nil, added0, removed0) : {Dict, Dict}
      {added0, removed0.with(k, older)}
    end

    def self.diff(older : Dict, newer : Dict) : {Dict, Dict}
      added, removed = Term[], Term[]

      older.each_entry do |k, v0|
        v1 = newer[k]?
        added, removed = diff(k, v0.downcast, v1.try(&.downcast), added, removed)
      end

      newer.each_entry do |k, v1|
        v0 = older[k]?
        added, removed = diff(k, v0.try(&.downcast), v1.downcast, added, removed)
      end

      {added, removed}
    end

    # Deep diff. Returns `{present in self but absent in other, absent in self but present in other}`.
    def diff(older) : {Dict, Dict}
      Dict.diff(older.as_d, newer: self)

      # seen0 = {} of BiList(Term) => Term
      # seen1 = {} of BiList(Term) => Term

      # each_prefix do |prefix, value|
      #   seen0.put_if_absent(prefix) { value }
      # end

      # other.each_prefix do |prefix, value|
      #   seen1.put_if_absent(prefix) { value }
      # end

      # lhs = seen0.reduce(Term[]) do |added, (prefix, v0)|
      #   if (v1 = seen1[prefix]?) && v0 == v1
      #     added
      #   else
      #     added.where(prefix, eq: v0)
      #   end
      # end

      # rhs = seen1.reduce(Term[]) do |removed, (prefix, v1)|
      #   if seen0.has_key?(prefix)
      #     removed
      #   else
      #     removed.where(prefix, eq: v1)
      #   end
      # end

      # {lhs, rhs}
      # lhs = seen0 - seen1
      # rhs = seen1 - seen0

      # {lhs.reduce(Term[]) { |delta, (prefix, value)| delta.where(prefix, value) },
      #  rhs.reduce(Term[]) { |delta, (prefix, value)| delta.where(prefix, value) }}
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
      ItemsView.new(@items, b: 0, e: @nitems, available: @nitems, sketch0: @sketch)
    end

    # Returns the pairs part of `partition` (see the latter for more info).
    def pairs : Dict
      @pairsonly ||= Dict.new(EMPTY_ITEM_NODE, @pairs, 0, @npairs, @sketch)
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

      # Sketches of equal dicts have *something* in common. They may be substantially
      # different or even junky if either (or both) dictionaries have "rich histories";
      # but there must exist an intersection of bits.
      if @sketch > 0 && other.@sketch > 0 && (@sketch & other.@sketch) == 0
        return false
      end

      return false unless @nitems == other.@nitems && @npairs == other.@npairs
      return false if (hx = @hash) && (hy = other.@hash) && hx != hy

      each_entry do |k, v1|
        return false unless v2 = other[k]?
        return false unless v1 == v2
      end

      true
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
        !@value.same?(stored.value)
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
