class Ww::Harmony
  # A versioned, mutable, indexed set of `Element`s. Elements are indexed by their
  # instance variables. The union of all possible types of ivars must be explicitly
  # provided as `Feature`.
  class IndexedSet(Element, Feature)
    defrecord FeatureId, repr : UInt64

    @changelog : Set::Changelog(Element)?

    def initialize
      @seq = 0u64
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

    def transaction(& : ->) : Set::Changelog
      # Save the old changelog on the stack.
      changelog0 = @changelog
      @changelog = Set::Changelog(Element).empty

      begin
        yield
        changelog1 = @changelog.not_nil!
        if changelog0
          changelog0 += changelog1
        end
        changelog1
      ensure
        # Restore the old changelog (extended with nested changes).
        @changelog = changelog0
      end
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
      each(cls, NamedTuple.new) { |element| yield element }
    end

    # Yields each element of type T, whose instance variables include all
    # of *hints*.
    def each(cls : T.class, **hints : **KV, & : T ->) : Nil forall T, KV
      {%
        # Validate KV keys at compile-time, otherwise we'd have a very hard time hunting
        # KeyErrors from .match?
        KV.keys.each do |key|
          unless T.instance_vars.any? { |ivar| ivar.name == key }
            KV.raise "invalid key #{key}"
          end
        end
      %}

      return unless feature_id = @features[cls]?

      membersets = Pf::Kit.stack_array(Set(Element), 8)
      membersets << @index[feature_id]

      hints.each do |_, feature|
        return unless feature_id = @features[feature]?

        membersets << @index[feature_id]
      end

      membersets.sort_by!(&.size)

      smallest = membersets[0]
      copied = false
      membersets.each(within: 1..) do |memberset|
        unless copied # Perform immutable intersection.
          smallest &= memberset
          copied = true
          next
        end

        # Perform mutable intersection.
        smallest.select!(&.in?(memberset))
      end

      smallest.each do |element|
        element = element.as(T)

        matches = true
        hints.each do |ivar, feature|
          next if IndexedSet.match?(element, ivar, feature)

          matches = false
          break
        end

        next unless matches

        yield element
      end
    end

    # :nodoc:
    def self.match?(element : Element, ivar : Symbol, feature : Feature) : Bool forall Element, Feature
      {% begin %}
        case ivar
        {% for ivar in Element.instance_vars %}
        when {{ivar.symbolize}}
          assert element.@{{ivar}}.is_a?(Feature), "type mismatch for @#{ivar}, expected #{Feature}"

          element.@{{ivar}} == feature
        {% end %}
        else
          raise KeyError.new("#{Element} has no ivar @#{ivar}")
        end
      {% end %}
    end

    # :nodoc:
    def self.each_feature(element : Element, & : Feature ->) : Nil
      {% begin %}
        case element
        {% for type in Element.union_types %}
        in {{type}}
          yield {{type}}.as(Feature)
          {% for ivar in type.instance_vars %}\
            yield element.@{{ivar.id}}.as(Feature)
          {% end %}
        {% end %}
        end
      {% end %}
    end

    # Returns `true` if the block is `true` for any element in this set.
    def any?(& : Element -> Bool) : Bool
      @elements.any? { |element| yield element }
    end

    # Returns `true` if the block is `true` for any element of type T in this set.
    # Only elements whose instance vars contain all of *hints* are considered.
    def any?(cls : T.class, **hints : Feature, & : T -> Bool) : Bool forall T
      each(cls, **hints) do |element|
        return true if yield element
      end

      false
    end

    # A shorthand for `any?` that does no further filtering beyond filtering
    # by type and possibly by hints.
    def any?(*args, **kwargs) : Bool
      any?(*args, **kwargs) { true }
    end

    def single?(*args, **kwargs)
      result = nil

      each(*args, **kwargs) do |element|
        return if result # Not single, multiple values match

        result = element
      end

      result
    end

    # Inserts *element* into this set. Returns `true` if it was inserted, `false` if
    # it is already in this set.
    def add(element : T) : Bool forall T
      {% unless T <= Element %}
        {% T.raise "argument must be a member of Element, but #{T} is not" %}
      {% end %}

      return false unless @elements.add?(element)

      feature_ids = Pf::Kit.stack_array(FeatureId, 8)

      IndexedSet(Element, Feature).each_feature(element) do |feature|
        feature_id = @features.put_if_absent(feature) do
          @seq, _ = @seq + 1, FeatureId.new(@seq)
        end
        feature_ids << feature_id
      end

      feature_ids.each do |feature_id|
        usage = @index.put_if_absent(feature_id) { Set(Element).new }
        usage << element
      end

      @changelog = @changelog.try(&.after_added(element))

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

      @changelog = @changelog.try(&.after_removed(element))

      IndexedSet(Element, Feature).each_feature(element) do |feature|
        feature_id = @features[feature]

        usage = @index[feature_id]
        assert usage.delete(element)
        next unless usage.empty?

        assert @index.delete(feature_id)
        assert @features.delete(feature)
      end

      true
    end

    # Removes all matching elements. The arguments *args* and the block are interpreted
    # by `each`.
    def delete_all(*args, **kwargs, &) : Nil
      matches = Pf::Kit.stack_array(Element)

      each(*args, **kwargs) do |element|
        next unless yield element

        matches << element
      end

      matches.each do |element|
        delete(element)
      end
    end

    # Removes all matching elements. The arguments *args* are interpreted by `each`.
    def delete_all(*args, **kwargs) : Nil
      delete_all(*args, **kwargs) { true }
    end

    def pretty_print(pp)
      pp.list("IndexedSet{", self, "}")
    end

    def inspect(io)
      io << "IndexedSet{"
      each_with_index do |element, index|
        io << ", " if index > 0
        element.inspect(io)
      end
      io << "}"
    end

    def_equals_and_hash @elements
  end
end
