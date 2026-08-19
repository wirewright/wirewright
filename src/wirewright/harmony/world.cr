class ::Ww::Harmony
  alias World = FactSet

  class FactSet
    {% begin %}
      # :nodoc:
      alias FactClass = Union({{Fact.union_types.map(&.class).splat}})
    {% end %}

    alias Feature = ServerDefn | ClientDefn | ServerId | PeerId | ClientId |
                    EndpointId | UInt64 | String | Bytes | FactClass

    # We store a fact just once on the heap, and then we manipulate a reference
    # to it.
    class FactRef
      getter fact : Fact

      def initialize(@fact)
      end

      def ==(other : Fact) : Bool
        @fact == other
      end

      def_equals_and_hash @fact
    end

    defrecord FeatureId, repr : UInt64

    def initialize
      @seq = 0u64
      @version = 0u64
      @facts = Set(FactRef).new
      @index = {} of FeatureId => Set(FactRef)
      @features = {} of Feature => FeatureId
    end

    def empty? : Bool
      size.zero?
    end

    def size : Int32
      @facts.size
    end

    def includes?(fact : Fact) : Bool
      @facts.includes?(fact)
    end

    def version : UInt64
      @version
    end

    def each(& : Fact ->) : Nil
      @facts.each { |fact_ref| yield fact_ref.fact }
    end

    def each_with_index(& : Fact, Int32 ->) : Nil
      @facts.each_with_index do |fact_ref, index|
        yield fact_ref.fact, index
      end
    end

    def each(cls : T.class, & : T ->) : Nil forall T
      each(cls, Tuple.new) { |fact| yield fact }
    end

    def each(cls : T.class, *hints : Feature, & : T ->) : Nil forall T
      each(cls, hints) { |fact| yield fact }
    end

    def each(cls : T.class, hints : Tuple() | Enumerable(Feature), & : T ->) : Nil forall T
      candidate_sets = Pf::Kit.stack_array(Set(FactRef), 8)

      pass do
        return unless feature_id = @features[cls]?

        candidate_sets << @index[feature_id]
      end

      hints.each do |feature|
        return unless feature_id = @features[feature]?

        candidate_sets << @index[feature_id]
      end

      return false if candidate_sets.empty?

      candidate_sets.sort_by!(&.size)

      smallest = candidate_sets[0].dup
      (1...candidate_sets.size).each do |index|
        candidate_set = candidate_sets[index]
        smallest.select! { |fact_ref| fact_ref.in?(candidate_set) }
      end

      smallest.each { |fact_ref| yield fact_ref.fact.as(T) }
    end

    def any?(cls : T.class, *args, & : T -> Bool) : Bool forall T
      each(cls, *args) do |fact|
        return true if yield fact
      end

      false
    end

    def any?(*args) : Bool
      any?(*args) { true }
    end

    def add(fact : T) : Bool forall T
      {% unless T <= Fact %}
        {% T.raise "argument must be a member of Fact, but #{T} is not" %}
      {% end %}

      if fact.in?(@facts)
        return false
      end

      ref = FactRef.new(fact)

      @facts << ref

      features = Pf::Kit.stack_array(FeatureId, 8)

      pass do
        feature = @features.put_if_absent(fact.class) do
          @seq, _ = @seq + 1, FeatureId.new(@seq)
        end
        features << feature
      end

      {% for ivar in T.instance_vars %}
        pass do
          feature = @features.put_if_absent(fact.@{{ivar}}) do
            @seq, _ = @seq + 1, FeatureId.new(@seq)
          end
          features << feature
        end
      {% end %}

      features.each do |feature|
        refs = @index.put_if_absent(feature) { Set(FactRef).new }
        refs << ref
      end

      @version += 1

      true
    end

    def <<(fact) : FactSet
      add(fact)
      self
    end

    def delete(fact : T) : Bool forall T
      {% unless T <= Fact %}
        {% T.raise "expected a Fact argument, not #{T}" %}
      {% end %}

      return false unless @facts.delete(fact)

      @version += 1

      pass(fact.class) do |feature|
        feature_id = @features[feature]

        refs = @index[feature_id]
        assert refs.delete(fact)
        next unless refs.empty?

        assert @index.delete(feature_id)
        assert @features.delete(feature)
      end

      {% for ivar in T.instance_vars %}
        pass(fact.@{{ivar}}) do |feature|
          feature_id = @features[feature]

          refs = @index[feature_id]
          assert refs.delete(fact)
          next unless refs.empty?

          assert @index.delete(feature_id)
          assert @features.delete(feature)
        end
      {% end %}

      true
    end

    def reject!(*args, &) : Nil
      matches = Pf::Kit.stack_array(Fact)

      each(*args) do |fact|
        next unless yield fact

        matches << fact
      end

      matches.each do |fact|
        delete(fact)
      end
    end

    def reject!(*args) : Nil
      reject!(*args) { true }
    end

    def pretty_print(pp)
      pp.list("FactSet{", self, "}")
    end

    def_equals_and_hash @facts
  end
end
