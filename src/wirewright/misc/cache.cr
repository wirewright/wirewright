module Ww
  module ICache(K, V)
    # Returns the cached value of *key*. Returns `nil` if none.
    abstract def get?(key : K) : V?

    # Sets the cached value of *key* to *value*. Returns *value*.
    abstract def put(key : K, value : V) : V

    # Returns the cached value of *key*, if any. Otherwise, runs the block to
    # compute the value. After the value is computed, puts it in cache. Returns
    # the resulting value. The boolean returned alongside the value indicates whether
    # the value was read from cache (`true`) or computed (`false`).
    #
    # NOTE: This is simply a get followed by a put. In other words, atomicity
    # is not guaranteed; someone else may insert a value for *key* between us
    # checking it and inserting it.
    def put_if_absent?(key : K, &) : {Bool, V}
      if value = get?(key)
        return true, value
      end

      {false, put(key, value: yield)}
    end

    # Same as `put_if_absent?`, but discards the boolean returned alongside
    # the value.
    def put_if_absent(key, & : -> V) : V
      _, value = put_if_absent?(key) { yield }
      value
    end
  end

  # Disables caching.
  @[Sync::Safe]
  struct Uncached(K, V)
    include ICache(K, V)

    def initialize
      {% if V.nilable? %}
        {% V.raise "cannot use nilable V with ICache" %}
      {% end %}
    end

    def get?(key : K) : V?
    end

    def put(key : K, value : V) : V
      value
    end
  end

  # Thread-safe wrapper for `LRU`.
  @[Sync::Safe]
  class SyncLRU(K, V)
    include ICache(K, V)

    # See `LRU#initialize` for info on *args* and *kwargs*.
    def initialize(*args, **kwargs)
      @cache = LRU(K, V).new(*args, **kwargs)
      @lock = Sync::Mutex.new
    end

    def get?(key : K) : V?
      @lock.synchronize { @cache.get?(key) }
    end

    def put(key : K, value : V) : V
      @lock.synchronize { @cache.put(key, value) }
    end
  end

  # Implements a least recently used (LRU) cache.
  #
  # NOTE: You should probably use `SyncLRU` unless you know what you're doing.
  class LRU(K, V)
    include ICache(K, V)

    # :nodoc:
    defrecord Entry(K, V), key : K, value : V

    @capacity : Int32
    @entries : Dll::List(Entry(K, V))
    @table : Hash(K, Dll::Item(Entry(K, V)))

    # Constructs an LRU cache with the given *capacity*.
    #
    # *by ref* can be set to `true` to make keys hash by reference instead of value
    # (see `Hash#compare_by_identity` for more info).
    def initialize(@capacity : Int32, by_ref : Bool = false)
      {% if V.nilable? %}
        {% V.raise "cannot use nilable V with ICache" %}
      {% end %}

      @entries = Dll.empty(Entry(K, V))
      @table = Hash(K, Dll::Item(Entry(K, V))).new(initial_capacity: @capacity)
      if by_ref
        @table.compare_by_identity
      end
    end

    # Registers a cache hit.
    private def hit(item : Dll::Item) : Nil
      Dll.delete(item)
      Dll.prepend(@entries, item)
    end

    def get?(key : K) : V?
      return unless item = @table[key]?

      hit(item)

      item.m.value
    end

    def put(key : K, value : V) : V
      # Update.
      if item = @table[key]?
        Dll.update(item, Entry.new(key, value))
        hit(item)
        return value
      end

      if @capacity == @table.size
        item = Dll.pop(@entries)
        @table.delete(item.key)
      end

      # Insert.
      item = Dll.prepend(@entries, Entry.new(key, value))
      @table[key] = item

      value
    end

    # :nodoc:
    def delete(key : K) : Nil
      return unless item = @table.delete(key)

      Dll.delete(item)
    end
  end

  # :nodoc:
  #
  # Doubly linked list.
  module LRU::Dll
    extend self

    defcase Head(T), r : Item(T) | Tail(T), mutation: true
    defcase Tail(T), l : Head(T) | Item(T), mutation: true
    defcase Item(T), l : Head(T) | Item(T) | Nil, m : T, r : Tail(T) | Item(T) | Nil, mutation: true

    defrecord List(T), head : Head(T), tail : Tail(T)

    def empty(cls : T.class) : List(T) forall T
      headptr = Pointer(Void).malloc(instance_sizeof(Head(T)))
      tailptr = Pointer(Void).malloc(instance_sizeof(Tail(T)))

      head = Head(T).pre_initialize(headptr)
      tail = Tail(T).pre_initialize(tailptr)

      head.initialize(tail)
      tail.initialize(head)

      List.new(head, tail)
    end

    def prepend(list : List(T), object : T) : Item(T) forall T
      prepend(list, Item.new(nil, object, nil))
    end

    def prepend(list : List(T), item : Item(T)) : Item(T) forall T
      item.l = list.head
      item.r = list.head.r
      list.head.r.l = item
      list.head.r = item
      item
    end

    def update(item : Item(T), object : T) : Item(T) forall T
      item.m = object
      item
    end

    def delete(item : Item(T)) : T forall T
      l = item.l
      r = item.r
      raise ArgumentError.new unless l && r

      l.r = r
      r.l = l

      item.l = nil
      item.r = nil
      item.m
    end

    def pop(list : List(T)) : T forall T
      item = list.tail.l
      unless item.is_a?(Item(T))
        raise IndexError.new
      end

      delete(item)
    end
  end

  # An LRU cache that also uses weak references for `V`.
  #
  # This let's the GC collect `V`s when there's memory pressure. Simultaneously,
  # if the number of `V`s exceeds *capacity*, we start evicting as in a normal
  # LRU cache.
  class WeakLRU(K, V)
    include ICache(K, V)

    # See `LRU#initialize` for info on *args* and *kwargs*.
    def initialize(*args, **kwargs)
      {% if V.nilable? %}
        {% V.raise "cannot use nilable V with ICache" %}
      {% end %}

      @lru = LRU(K, WeakRef(V)).new(*args, **kwargs)
    end

    def get?(key : K) : V?
      return unless ref = @lru.get?(key)

      unless value = ref.value
        @lru.delete(key)
        return
      end

      value
    end

    def put(key : K, value : V) : V
      @lru.put(key, value: WeakRef.new(value))

      value
    end
  end

  # Thread-safe wrapper for `WeakLRU`.
  @[Sync::Safe]
  class SyncWeakLRU(K, V)
    include ICache(K, V)

    # See `LRU#initialize` for info on *args* and *kwargs*.
    def initialize(*args, **kwargs)
      @lru = WeakLRU(K, V).new(*args, **kwargs)
      @lock = Sync::Mutex.new
    end

    def get?(key : K) : V?
      @lock.synchronize { @lru.get?(key) }
    end

    def put(key : K, value : V) : V
      @lock.synchronize { @lru.put(key, value) }
    end
  end
end
