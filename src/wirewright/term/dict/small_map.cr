class Ww::Term::Dict
  # A map of keys of type *I* in the range `0...bit_width(I)` to values
  # of type *T*. For example, `SmallMap(Int32, UInt8)` allows to map keys
  # `0..7` to an `Int32`. We say that a map like this has 8 *slots*.
  #
  # NOTE: For consistency, we only accept and respond with keys of type *I*.
  # This may require some casting on your end.
  struct SmallMap(T, I)
    # :nodoc:
    getter buffer : T*

    # :nodoc:
    getter present : Pf::BitSet(I)

    # :nodoc:
    getter capacity : Int32

    # :nodoc:
    def initialize(@buffer : T*, @present : Pf::BitSet(I), @capacity : Int32)
    end

    # Returns an empty map.
    def self.empty : SmallMap
      SmallMap.new(
        buffer: Pointer(T).null,
        present: Pf::BitSet(I).empty,
        capacity: 0,
      )
    end

    # Returns `true` if all entries of two maps *a* and *b* are equal.
    # Values from both maps are compared using the block.
    def self.equals?(a : SmallMap, b : SmallMap, &) : Bool
      return false unless a.present == b.present
      return true if a.buffer == b.buffer # pointer equality

      (0...a.size).all? do |index|
        yield a.buffer[index], b.buffer[index]
      end
    end

    # Returns `true` if this map has no entries.
    def empty? : Bool
      @present.empty?
    end

    # Returns an `Indexable` of values in this map.
    #
    # This also serves as a way to access the N-th value: just do `map.ix[0]` etc.
    def ix : Indexable(T)
      @buffer.to_slice(size)
    end

    # Returns the number of entries in this map.
    def size : Int32
      @present.size
    end

    # Returns the minimum excluded (absent) key.
    def mex : I
      @present.mex
    end

    # Returns `true` if all slots in this map are occupied.
    def full? : Bool
      @present.full?
    end

    # Returns the value of *key*, or `nil` if *key* is absent.
    def at?(key : I) : T?
      return unless key.in?(@present)

      @buffer[@present.rank(key)]
    end

    # Returns the value of *key*. Raises `KeyError` if *key* is absent.
    def at(key : I) : T
      value = at?(key)
      raise KeyError.new if value.nil?

      value
    end

    # Yields entries from this map, in ascending key order.
    def each_entry(& : I, T ->) : Nil
      @present.each do |key|
        yield key, @buffer[@present.rank(key)]
      end
    end

    # Returns a map with all keys greater than or equal to *lo*.
    def gte(lo : I) : SmallMap
      view = @present.gte(lo)
      unless first = view.select?(0)
        return SmallMap(T, I).empty
      end

      # NOTE: We must set `capacity: 0` on views of a map. This will
      # effectively disable mutation for the particular view. We don't
      # want to mutate the original map through its view and vice versa!
      SmallMap.new(@buffer + @present.rank(first), view, capacity: 0)
    end

    # Returns a map with all keys less than *hi*.
    def lt(hi : I) : SmallMap
      # The size of @buffer is already trimmed by lt(). We don't need to
      # do anything else on @buffer.
      SmallMap.new(@buffer, @present.lt(hi), capacity: 0)
    end

    # Returns a map with keys in the range defined by *lo* (lower bound) and
    # *hi* (higher bound; *exclusive*).
    def view(lo : I, hi : I) : SmallMap
      gte(lo).lt(hi)
    end

    # Same as `assoc`, but asserts that assoc changes the map. If it does not,
    # raises `ArgumentError`.
    def ensure_assoc(key : I, value : T, **kwargs) : SmallMap
      map, changed = assoc(key, value, **kwargs)
      raise ArgumentError.new unless changed

      map
    end

    # Creates or updates an association between *key* and *value*. Returns
    # the resulting map followed by a boolean indicating whether its content
    # is different from this (original) map.
    #
    # *mut* can be set to `true` to enable direct mutation of this map instead
    # (if possible). Note that you must use the returned map even if *mut* is
    # `true`, since even mutable changes can cause reallocation, or they can
    # fall back to immutable changes. You can discard the resulting map only if
    # it did not change, that is, only if the accompanying boolean is `false`.
    def assoc(key : I, value : T, *, mut : Bool) : {SmallMap(T, I), Bool}
      if mut && @capacity > 0
        return assoc!(key, value)
      end

      assoc(key, value)
    end

    private def assoc(key : I, value : T)
      assert key < 16

      offset = @present.rank(key)

      if key.in?(@present) # Update
        if @buffer[offset] == value
          return self, false # did not change
        end

        values0 = Slice.new(@buffer, @present.size)
        values1 = values0.dup
        values1.unsafe_put(offset, value)

        return SmallMap.new(values1.to_unsafe, @present, values1.size), true # changed
      end

      # Insert
      values0 = Slice.new(@buffer, @present.size)
      values1 = Slice.join({values0.trim(offset)}, {value}, {values0 + offset})

      {SmallMap.new(values1.to_unsafe, @present.add(key), values1.size), true} # changed
    end

    private def assoc!(key : I, value : T)
      assert key < 16

      offset = @present.rank(key)

      if key.in?(@present) # Update
        if @buffer[offset] == value
          return self, false # did not change
        end

        @buffer[offset] = value

        return self, true # changed
      end

      # Insert
      size = @present.size

      if size + 1 > @capacity # Does not fit
        capacity = (size * 2).clamp(8..16)
        target = Pointer(T).malloc(capacity)
        target.copy_from(@buffer, offset)
        target[offset] = value
        (target + offset + 1).copy_from(@buffer + offset, size - offset)

        return SmallMap.new(target, @present.add(key), capacity), true # changed
      end

      # Fits
      (@buffer + offset + 1).move_from(@buffer + offset, size - offset)
      @buffer[offset] = value

      {SmallMap.new(@buffer, @present.add(key), @capacity), true} # changed
    end

    # Same as `dissoc`, but asserts that dissoc changes the map. If it does not,
    # raises `ArgumentError`.
    def ensure_dissoc(key : I, **kwargs) : SmallMap
      map, removed = dissoc(key, **kwargs)
      raise ArgumentError.new unless removed

      map
    end

    # Removes an association with the given *key*. Returns the resulting map
    # followed by a boolean indicating whether its content is different from
    # this (original) map.
    #
    # *mut* can be set to `true` to enable direct mutation of this map instead
    # (if possible). Note that you must use the returned map even if *mut* is
    # `true`, since even mutable changes can cause reallocation, or they can
    # fall back to immutable changes. You can discard the resulting map only if
    # it did not change, that is, only if the accompanying boolean is `false`.
    def dissoc(key : I, *, mut : Bool) : {SmallMap(T, I), Bool}
      if mut && @capacity > 0
        return dissoc!(key)
      end

      dissoc(key)
    end

    private def dissoc(key : I)
      unless key.in?(@present)
        return self, false # did not change
      end

      size = @present.size
      offset = @present.rank(key)

      {% if T == ::Ww::Term %}
        # If we are removing a symbol or a boolean or a small integer from the end,
        # there is little point in copying. We can just subtract from the size. We
        # don't do this for strings and dictionaries and BigRationals because they
        # could take up a significant amount of memory (in theory); so we wouldn't
        # want to keep references to them hanging around. We would instead like
        # the GC to get rid of them as soon as possible.
        if offset == size - 1
          value = @buffer[offset]
          if value.inline?
            return SmallMap.new(@buffer, @present.delete(key), capacity: 0), true # changed
          end
        end
      {% end %}

      values0 = Slice.new(@buffer, size)
      values1 = values0[...offset] + values0[offset + 1..]

      {SmallMap.new(values1.to_unsafe, @present.delete(key), values1.size), true} # changed
    end

    private def dissoc!(key : I)
      unless key.in?(@present)
        return self, false # did not change
      end

      size = @present.size
      offset = @present.rank(key)

      {% if T == ::Ww::Term %}
        # Ditto
        if offset == size - 1
          value = @buffer[offset]
          if value.inline?
            return SmallMap.new(@buffer, @present.delete(key), capacity: 0), true # changed
          end
        end
      {% end %}

      (@buffer + offset).move_from(@buffer + offset + 1, size - offset - 1)

      {SmallMap.new(@buffer, @present.delete(key), @capacity), true} # changed
    end

    def pretty_print(pp)
      pp.group(1, "SmallMap@0x#{@buffer.address.to_s(base: 16)}{", "}") do
        @present.each_with_index do |key, index|
          value = @buffer[index]

          pp.comma if index > 0
          key.pretty_print(pp)
          pp.text(" =>")
          pp.nest do
            pp.breakable
            value.pretty_print(pp)
          end
        end
      end
    end
  end
end
