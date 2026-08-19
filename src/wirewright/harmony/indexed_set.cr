class Ww::Harmony
  # A versioned, mutable, indexed set of `Element`s. Elements are indexed by their
  # instance variables. The union of all possible types of ivars must be explicitly
  # provided as `Feature`.
  class IndexedSet(Element, Feature)
    defrecord FeatureId, repr : UInt64

    def initialize
      @seq = 0u64
      @version = 0u64
      @elements = Set(Element).new
      @index = {} of FeatureId => Set(Element)
      @features = {} of Feature => FeatureId
    end

    # Returns `true` if this set contains no elements.
    def empty? : Bool
      size.zero?
    end

    # Returns the number of elements in this set.
    def size : Int32
      @elements.size
    end

    # Returns `true` if this set contains *element*.
    def includes?(element : Element) : Bool
      @elements.includes?(element)
    end

    # Returns the version of this set. The version is incremented after each mutation
    # of the set.
    def version : UInt64
      @version
    end

    # Yields each element in this set.
    def each(& : Element ->) : Nil
      @elements.each { |element| yield element }
    end

    # Yields each element in this set along with its index.
    def each_with_index(& : Element, Int32 ->) : Nil
      @elements.each_with_index do |element, index|
        yield element, index
      end
    end

    # Yields each element of type T.
    def each(cls : T.class, & : T ->) : Nil forall T
      each(cls, Tuple.new) { |element| yield element }
    end

    # Yields each element of type T, whose instance variables include all
    # of *hints*.
    def each(cls : T.class, *hints : Feature, & : T ->) : Nil forall T
      each(cls, hints) { |element| yield element }
    end

    # Yields each element of type T, whose instance variables include all
    # of *hints*.
    def each(cls : T.class, hints : Tuple() | Enumerable(Feature), & : T ->) : Nil forall T
      membersets = Pf::Kit.stack_array(Set(Element), 8)

      pass do
        return unless feature_id = @features[cls]?

        membersets << @index[feature_id]
      end

      hints.each do |feature|
        return unless feature_id = @features[feature]?

        membersets << @index[feature_id]
      end

      return false if membersets.empty?

      membersets.sort_by!(&.size)

      smallest = membersets[0]
      copied = false
      membersets.each(within: 1..) do |memberset|
        unless copied # Perform immutable intersection.
          smallest &= memberset
          next
        end

        # Perform mutable intersection.
        smallest.select!(&.in?(memberset))
      end

      smallest.each { |element| yield element.as(T) }
    end

    # Returns `true` if the block is `true` for any element in this set.
    def any?(& : Element -> Bool) : Bool
      @elements.any? { |element| yield element }
    end

    # Returns `true` if the block is `true` for any element of type T in this set.
    def any?(cls : T.class, & : T -> Bool) : Bool forall T
      any?(cls) { |element| yield element }
    end

    # Returns `true` if the block is `true` for any element of type T in this set.
    # Only elements whose instance vars contain all of *hints* are considered.
    def any?(cls : T.class, *hints : Feature, & : T -> Bool) : Bool forall T
      each(cls, *hints) do |element|
        return true if yield element
      end

      false
    end

    # A shorthand for `any?` that does no further filtering beyond filtering
    # by type and possibly by hints.
    def any?(*args) : Bool
      any?(*args) { true }
    end

    # Inserts *element* into this set. Returns `true` if it was inserted, `false` if
    # it is already in this set.
    def add(element : T) : Bool forall T
      {% unless T <= Element %}
        {% T.raise "argument must be a member of Element, but #{T} is not" %}
      {% end %}

      if element.in?(@elements)
        return false
      end

      @elements << element

      features = Pf::Kit.stack_array(FeatureId, 8)

      pass do
        feature = @features.put_if_absent(element.class) do
          @seq, _ = @seq + 1, FeatureId.new(@seq)
        end
        features << feature
      end

      {% for ivar in T.instance_vars %}
        pass do
          feature = @features.put_if_absent(element.@{{ivar}}) do
            @seq, _ = @seq + 1, FeatureId.new(@seq)
          end
          features << feature
        end
      {% end %}

      features.each do |feature|
        usage = @index.put_if_absent(feature) { Set(Element).new }
        usage << element
      end

      @version += 1

      true
    end

    # Inserts an *element* into this set.
    def <<(element) : IndexedSet
      add(element)
      self
    end

    # Removes an *element* from this set. Returns `true` if it was removed, `false` if
    # it does not exist in this set.
    def delete(element : T) : Bool forall T
      {% unless T <= Element %}
        {% T.raise "expected an Element argument, not #{T}" %}
      {% end %}

      return false unless @elements.delete(element)

      @version += 1

      pass(element.class) do |feature|
        feature_id = @features[feature]

        usage = @index[feature_id]
        assert usage.delete(element)
        next unless usage.empty?

        assert @index.delete(feature_id)
        assert @features.delete(feature)
      end

      {% for ivar in T.instance_vars %}
        pass(element.@{{ivar}}) do |feature|
          feature_id = @features[feature]

          usage = @index[feature_id]
          assert usage.delete(element)
          next unless usage.empty?

          assert @index.delete(feature_id)
          assert @features.delete(feature)
        end
      {% end %}

      true
    end

    # Removes all matching elements. The arguments *args* and the block are interpreted
    # by `each`.
    def delete_all(*args, &) : Nil
      matches = Pf::Kit.stack_array(Element)

      each(*args) do |element|
        next unless yield element

        matches << element
      end

      matches.each do |element|
        delete(element)
      end
    end

    # Removes all matching elements. The arguments *args* are interpreted by `each`.
    def delete_all(*args) : Nil
      delete_all(*args) { true }
    end

    def pretty_print(pp)
      pp.list("IndexedSet{", self, "}")
    end

    def_equals_and_hash @elements
  end
end
