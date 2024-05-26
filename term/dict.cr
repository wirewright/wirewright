module Ww
  # Represents a dictionary: an immutable, persistent collection of key-value
  # pairs supporting efficient, near-O(1) insert, delete, and lookup.
  class Term::Dict
    include ITerm

    EMPTY = new(itemsonly: true, entryonly: true)

    # An enumerable over dictionary entries.
    private struct EntryEnumerable
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
        @items, _ = dict.partition
      end

      def each(& : Term ->)
        (0...@items.size).each { |index| yield @items[index] }
      end
    end

    @hashcode : UInt64?

    def initialize(@map = Pf::Map(Term, Term).new, itemsonly = false, entryonly = false)
      @items = ItemsView.new(self) if itemsonly
      @entries = self if entryonly
    end

    def self.build(**kwargs, &)
      map = Pf::Map(Term, Term).transaction do |commit|
        yield commit
      end

      new(map, **kwargs)
    end

    # Returns the number of entries in this dictionary.
    def size : Int
      @map.size
    end

    # Returns `true` if this dictionary contains no entries.
    def empty? : Bool
      @map.empty?
    end

    # Returns `true` if this dictionary contains the given *key*.
    def includes?(key) : Bool
      @map.has_key?(Term.of(key))
    end

    # Returns `true` if all key-value pairs of `self` are included in *other*.
    def subset_of?(other : Dict) : Bool
      other.size >= size && ee.all? { |k, v| other[k]? == v }
    end

    # Returns `true` if all key-value pairs of *other* are included in `self`.
    def superset_of?(other : Dict) : Bool
      other.subset_of?(self)
    end

    # Returns `true` if this dictionary only contains items, and no entries.
    # Essentially, returns `true` if this dictionary is a list; and `false`
    # otherwise.
    def itemsonly? : Bool
      (Term[0]...Term[size]).all?(&.upcast.in?(@map))
    end

    # Returns the value associated with the given *key*, or raises `KeyError`
    # if *key* is not associated with any value.
    def at(key) : Term
      @map[Term.of(key)]
    end

    # Returns the value associated with the given *key*, or nil if *key*
    # is not associated with any value.
    def at?(key) : Term?
      @map[Term.of(key)]?
    end

    # Alias of `at`.
    def [](key) : Term
      at(key)
    end

    # Alias of `at?`.
    def []?(key) : Term?
      at?(key)
    end

    def dig(*keys) : Term
      keys.reduce(self) do |dict, key|
        raise KeyError.new unless dict = dict.is_a?(Dict)
        dict.at(key)
      end
    end

    def dig?(*keys) : Term?
      keys.reduce(self) do |dict, key|
        return unless dict.is_a?(Dict)
        return unless v = dict.at?(key)
        v
      end
    end

    # Yields each entry from this dictionary.
    def each_entry(& : Term, Term ->)
      @map.each { |k, v| yield k, v }
    end

    # Returns an enumerable based on `each_entry`.
    def ee : Enumerable({Term, Term})
      EntryEnumerable.new(self)
    end

    # Returns an enumerable of items (see `partition`).
    def ie : Enumerable(Term)
      ItemEnumerable.new(self)
    end

    # Returns a copy of this dictionary extended with an association between
    # *key* and *value*. If *key* was present already its value is updated
    # in the copy.
    #
    # If *value* is `nil` acts as `without`. This is mainly useful during
    # conversion from JSON (via `Term.[]`), treating `null` as absence.
    def with(key, value) : Dict
      return without(key) if value.nil? || value.is_a?(JSON::Any) && value.raw.nil?

      key, value = Term.of(key), Term.of(value)

      entries = @map.assoc(key, value)
      entries.same?(@map) ? self : Dict.new(entries)
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # an association with the given *key*.
    def without(key) : Dict
      Dict.new(@map.dissoc(Term.of(key)))
    end

    # Returns a copy of this dictionary that is guaranteed not to contain
    # associations with any of the given *keys*.
    def without(keys : Enumerable) : Dict
      keys.reduce(self) { |dict, key| dict.without(key) }
    end

    # Creates a dictionary with just one entry: *name* bound to the value of
    # *key* in `self`.
    #
    # ```
    # Term[x: 100, y: 200, z: 300].pluck("x")      # => Term[x: 100]
    # Term[x: 100, y: 200, z: 300].pluck("x", "a") # => Term[a: 100]
    # ```
    def pluck(key, as name = key) : Dict
      pluck({ {key, name} })
    end

    # Creates a dictionary by extracting data from this dictionary based on
    # the given *keys*. You cannot create new data, only manipulate that
    # found in `self`.
    #
    # ```
    # Term[x: 100, y: 200, z: 300].pluck({"x", {"z", "a"}}) # => Term[x: 100, a: 300]
    # ```
    def pluck(keys : Enumerable, rest = nil) : Dict
      selection = keys.reduce(Term[]) do |base, key|
        if key.is_a?(Tuple)
          key, name = key
        else
          name = key
        end
        (value = self[key]?) ? base.with(name, value) : base
      end

      return selection unless rest

      selection.with(rest, selection.ee.reduce(self) { |dict, key| dict.without(key) })
    end

    # Merges this and *other* dictionary.
    #
    # If same keys and both values are dictionaries, merging continues
    # recursively. Otherwise, *other*'s value is preferred.
    #
    # If *other* is not a dictionary, `TypeCastError` is raised for
    # consistency with other autocast methods.
    def &(other : Dict) : Dict
      if size < other.size
        ee.reduce(other) do |u, (k, my)|
          its = other[k]?
          l = my.downcast
          r = its.try(&.downcast)
          if l.is_a?(Dict) && r.is_a?(Dict)
            u.with(k, l & r)
          elsif its.nil?
            u.with(k, my)
          else
            u
          end
        end
      else
        other.ee.reduce(self) do |u, (k, its)|
          my = self[k]?
          l = my.try(&.downcast)
          r = its.downcast
          if l.is_a?(Dict) && r.is_a?(Dict)
            u.with(k, l & r)
          else
            u.with(k, its)
          end
        end
      end
    end

    def &(other : Term)
      self & other.as_d
    end

    # Splits this dictionary into a dictionary with only items in it, and another
    # dictionary with only entries in it.
    #
    # - *items* are the list part of the dictionary.
    # - *entries* are the hash map part of the dictionary.
    def partition : {Dict::ItemsView, Dict}
      if (items = @items) && (entries = @entries)
        return items, entries
      end

      # Fast path for small itemsonly dicts.
      if @map.size < 8 && itemsonly?
        @items = items = ItemsView.new(self)
        @entries = entries = Term[]
        return items, entries
      end

      # Some are numbers and some are other types of terms, or the dict has more
      # than 8 entries.
      entrymap = nil
      itemsmap = @map.transaction do |itemsmap|
        entrymap = @map.transaction do |entrymap|
          each_entry do |k, v|
            (k.downcast.is_a?(Num) ? entrymap : itemsmap).dissoc(k)
          end
        end
      end

      if (Term[0]...Term[itemsmap.size]).all?(&.upcast.in?(itemsmap))
        @items = items = ItemsView.new(Dict.new(itemsmap, itemsonly: true))
        @entries = entries = Dict.new(entrymap.not_nil!, entryonly: true)
      else
        @items = items = ItemsView.new(Term[])
        @entries = entries = self
      end

      {items, entries}
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

    # :inherit:
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

    # Returns `true` if this and *other* dictionaries are equal.
    def ==(other : Dict)
      if (h1 = @hashcode) && (h2 = other.@hashcode)
        return false unless h1 == h2
      end

      @map == other.@map
    end

    # :nodoc:
    def ==(other)
      false
    end

    def hash(hasher)
      {Dict, @map}.hash(hasher)
    end

    def hash
      @hashcode ||= super
    end

    # :nodoc:
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
end

require "./dict/items_view"
