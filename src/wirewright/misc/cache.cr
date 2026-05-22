module Ww
  module ICache(K, V)
    # Returns the cached value of *key*. Returns `nil` if none.
    abstract def get?(key : K) : V?

    # Sets the cached value of *key* to *value*. Returns *value*.
    abstract def put(key : K, value : V) : V

    # Removes the cached value of *key*. Returns the value. Returns `nil` if none.
    abstract def delete(key : K) : V?

    # Returns the cached value of *key*, if any. Otherwise, runs the block to
    # compute the value. After the value is computed, puts it in cache. Returns
    # the resulting value. The boolean returned alongside the value indicates whether
    # the value was read from cache (`true`) or computed (`false`).
    #
    # NOTE: This is simply a get followed by a put. In other words, atomicity
    # is not guaranteed; someone else may insert a value for *key* between us
    # checking it and inserting it.
    #
    # Signature: `put_if_absent?(key : K, & : -> V) : {Bool, V}`.
    def put_if_absent?(key : K, &)
      if value = get?(key)
        return true, value
      end

      {false, put(key, value: yield)}
    end

    # Same as `put_if_absent?`, but discards the boolean returned alongside
    # the value.
    #
    # Signature: `put_if_absent(key : K, & : -> V) : V`.
    def put_if_absent(key, &)
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

    def delete(key : K) : V?
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

    def delete(key : K) : V?
      @lock.synchronize { @cache.delete(key) }
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

    def resolve?(key : K) : V?
      if item = @table[key]?
        entry = item.m
        return entry.value
      end

      if @capacity == @table.size
        entry = Dll.last(@entries)
        return entry.value
      end
    end

    def put(key : K, value : V, &) : V
      # Update.
      if item = @table[key]?
        yield item.m.value
        Dll.update(item, Entry(K, V).new(key, value))
        hit(item)
        return value
      end

      if @capacity == @table.size
        entry = Dll.pop(@entries)
        @table.delete(entry.key)
        yield entry.value
      end

      # Insert.
      item = Dll.prepend(@entries, Entry(K, V).new(key, value))
      @table[key] = item

      value
    end

    def put(key : K, value : V) : V
      put(key, value) { }
    end

    def each_least_recent(& : K, V ->)
      Dll.reverse_each(@entries) do |entry|
        yield entry.key, entry.value
      end
    end

    def delete(key : K) : V?
      return unless item = @table.delete(key)

      entry = Dll.delete(item)
      entry.value
    end

    def pretty_print(pp)
      pp.group(1, "LRU[", "]") do
        index = 0

        each_least_recent do |key, value|
          pp.comma if index > 0

          pp.group do
            key.pretty_print(pp)
            pp.text " =>"
            pp.nest do
              pp.breakable
              value.pretty_print(pp)
            end
          end

          index += 1
        end
      end
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

    def each(list : List(T), & : T ->) : Nil forall T
      current = list.head

      until current.is_a?(Tail(T))
        if current.is_a?(Item(T))
          yield current.m
        end

        break unless r = current.r

        current = r
      end
    end

    def reverse_each(list : List(T), & : T ->) : Nil forall T
      current = list.tail

      until current.is_a?(Head(T))
        if current.is_a?(Item(T))
          yield current.m
        end

        break unless l = current.l

        current = l
      end
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

    def last(list : List(T)) : T forall T
      item = list.tail.l
      unless item.is_a?(Item(T))
        raise IndexError.new
      end

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

  # An LRU cache with an additional bytesize threshold constraint.
  #
  # If the cache's total bytesize grows above the threshold, least recently used
  # items will be subject to more aggressive eviction on the basis of their bytesize
  # and presence in the GC heap (`WeakRef`) rather than the cache's capacity alone.
  #
  # A degenerate example is when the cache contains one item whose size is above
  # the threshold, but the cache's capacity is well above `1`. A future put()
  # will downgrade the large item to a `WeakRef`, which makes it susceptible
  # to the GC.
  #
  # If the large item is not strongly referenced anywhere and the GC needs memory,
  # the large item may be collected, and therefore, evicted from the cahce -- due
  # to its memory use, not cache capacity.
  #
  # Downgraded items not yet collected by the GC, if accessed using `get?`, are
  # upgraded back to strong references. This may lead to degenerate oscillatory
  # behavior *in theory*, but I haven't experienced such behavior yet. If it can
  # indeed be experienced, we'll have to guard the upgrade; but currently, as
  # we experiment, there's little point.
  #
  # EXPERIMENTAL: I'm not sure how robust this thing is at the moment.
  class ThresholdLRU(K, V)
    include ICache(K, V)

    # :nodoc:
    defrecord Tombstone
    # :nodoc:
    defrecord Strong(K, V), bytesize : UInt64, key : K, value : V
    # :nodoc:
    defrecord Weak(K, V), bytesize : UInt64, key : K, weak_ref : WeakRef(V)

    def initialize(capacity : Int32, @threshold : UInt64)
      {% if V.nilable? %}
        {% V.raise "cannot use nilable V with ICache" %}
      {% end %}

      {% unless V < ::Reference %}
        {% V.raise "V must be a reference type" %}
      {% end %}

      {% if V < ::WeakRef %}
        {% V.raise "V must not be a WeakRef" %}
      {% end %}

      {% unless V.has_method?(:bytesize) %}
        {% V.raise "V must respond to #bytesize" %}
      {% end %}

      @lru = LRU(K, Int32).new(capacity)
      @slots = [] of Strong(K, V) | Weak(K, V) | Tombstone
      @tombstones = [] of Int32
      @bytesize = 0u64
    end

    def get?(key : K) : V?
      return unless index = @lru.get?(key)

      slot = @slots[index]

      case slot
      in Tombstone
        unreachable("LRU and slots array are out of sync")
      in Strong(K, V)
        slot.value
      in Weak(K, V)
        weak_ref = slot.weak_ref
        unless object = weak_ref.value
          @lru.delete(key)
          @slots[index] = Tombstone.new
          @tombstones << index
          return
        end

        # Reinstate a strong reference since the object was recently accessed.
        #
        # NOTE: A put() in the future may cause an oscillation but I think it's
        # unlikely, esp. since put() downgrades least-recent items and this one
        # becomes the most-recent one due to the get?() above. Until thrashing
        # is actually observed in practice (if at all), I think it's better
        # to keep this unconditional.
        @slots[index] = Strong(K, V).new(slot.bytesize, key, object)
        # Weak is costless so we have to add its bytesize back.
        @bytesize += slot.bytesize

        object
      end
    end

    def put(key : K, value : V) : V
      unless index1 = @lru.resolve?(key)
        index1 = @tombstones.pop? || @slots.size
      end

      @lru.put(key, index1) do |index0|
        # This block is triggered by replacement or eviction.
        case slot0 = @slots[index0]
        in Tombstone, Weak(K, V)
          # Tombstone and Weak are costless.
        in Strong(K, V)
          @bytesize -= slot0.bytesize
        end
      end

      slot1 = Strong(K, V).new(value.bytesize.to_u64, key, value)
      @bytesize += slot1.bytesize
      if index1 == @slots.size
        @slots << slot1
      else
        @slots[index1] = slot1
      end

      if @bytesize > @threshold
        deleted = Pf::Kit.stack_array(K, 8)

        @lru.each_least_recent do |key, index|
          break if @bytesize <= @threshold

          slot = @slots[index]

          case slot
          in Tombstone
          in Weak(K, V)
            next unless slot.weak_ref.value.nil?

            deleted << key
            @slots[index] = Tombstone.new
            @tombstones << index
          in Strong(K, V)
            @slots[index] = Weak.new(slot.bytesize, key, WeakRef.new(slot.value))
            @bytesize -= slot.bytesize
          end
        end

        deleted.each do |key|
          @lru.delete(key)
        end
      end

      value
    end

    def delete(key : K) : V?
      return unless index = @lru.delete(key)

      case slot = @slots[index]
      in Tombstone
        unreachable("LRU and slots array are out of sync")
      in Weak(K, V)
        @slots[index] = Tombstone.new
        @tombstones << index

        slot.weak_ref.value # May be nil
      in Strong(K, V)
        @slots[index] = Tombstone.new
        @tombstones << index
        @bytesize -= slot.bytesize

        slot.value
      end
    end
  end

  # A cache that maintains two "generations", the *active* and *surviving*
  # generation. In Scenery, they represent the previous frame and the current,
  # in-progress frame, correspondingly.
  #
  # As the current frame is constructed, cache hits are "moved" from the previous
  # frame to the current frame. At the end of an `epoch`, we remove all unhit entries
  # from the previous frame, and swap.
  #
  # This means we only cache things that are reused across two frames, and drop
  # all other things -- not *really* all other things, though. For example, in
  # Scenery, assets can be retained for a longer time due to `HTTPService`
  # and `PathService` caches. So `GenerationalCache` can be thought of as a kind
  # of "short-term memory" cache; with asset caches etc. a "long-term memory" cache.
  class GenerationalCache(K, V)
    include ICache(K, V)

    def initialize
      @active = {} of K => V
      @surviving = {} of K => V
    end

    def get?(key : K) : V?
      if value = @surviving[key]?
        return value
      end

      return unless value = @active[key]?

      @surviving[key] = value
    end

    # Sets the cached value of *key* to *value*. Returns *value*.
    def put(key : K, value : V) : V
      @surviving[key] = value
    end

    # Removes the cached value of *key*. Returns the value. Returns `nil` if none.
    def delete(key : K) : V?
      @active.delete(key) || @surviving.delete(key)
    end

    # Clears this cache.
    def clear : Nil
      @active.clear
      @surviving.clear
    end

    def epoch(&)
      yield
    ensure
      @active.clear
      @active, @surviving = @surviving, @active
    end
  end
end
