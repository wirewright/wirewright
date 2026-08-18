class Ww::Harmony
  # FIXME: We need to index by all (or most) fields rather than just one primary
  # key field. Moreover, this class is horrendously implemented. Is there a way to
  # improve this? And combine it with World?
  class KeyedFactSet
    include Enumerable(Fact)

    {% begin %}
      {%
        tables = {} of ::NoReturn => ::NoReturn

        Fact.union_types.each_with_index do |type, index|
          next unless ann = type.annotation(PrimaryKey)
          next unless field = ann[0]

          tables[type.symbolize] = {name: "table#{index}".id, key: field.var.id, key_type: field.type}
        end
      %}

      # :nodoc:
      TABLES = {{tables}}

      {% for fact_type, table in tables %}
        @{{table[:name]}} : Hash({{table[:key_type]}}, Set({{fact_type.id}}))?
      {% end %}
    {% end %}

    @unkeyed : Set(Fact)?

    def size : Int32
      size = @unkeyed.try(&.size) || 0
      {% for _, table in TABLES %}
        size += @{{table[:name]}}.try(&.size) || 0
      {% end %}
      size
    end

    def includes?(fact : Fact) : Bool
      {% for fact_type, table in TABLES %}
        if fact.is_a?({{fact_type.id}})
          return false unless buckets = @{{table[:name]}}
          return false unless bucket = buckets[fact.{{table[:key]}}]?
          return bucket.includes?(fact)
        end
      {% end %}

      @unkeyed.try(&.includes?(fact)) || false
    end

    def each(& : Fact ->) : Nil
      {% for _, table in TABLES %}
        if buckets = @{{table[:name]}}
          buckets.each do |_, bucket|
            bucket.each { |fact| yield fact.as(Fact) }
          end
        end
      {% end %}

      @unkeyed.try do |facts|
        facts.each { |fact| yield fact }
      end
    end

    def each(cls : T.class, & : T ->) : Nil forall T
      {% begin %}
        {% table = TABLES[T.symbolize] %}

        return false unless buckets = @{{table[:name]}}

        buckets.each do |_, bucket|
          bucket.each { |fact| yield fact }
        end
      {% end %}
    end

    def each(cls : T.class, key : K, & : T ->) : Nil forall T, K
      {% begin %}
        {% table = TABLES[T.symbolize] %}

        {% unless K <= table[:key_type].resolve %}
          {% K.raise "invalid key type #{K}, expected #{table[:key_type]}" %}
        {% end %}

        return unless buckets = @{{table[:name]}}
        return unless bucket = buckets[key]?

        bucket.each { |fact| yield fact }
      {% end %}
    end

    def any?(cls : T.class, & : T -> Bool) : Bool forall T
      each(cls) do |fact|
        return true if yield fact
      end

      false
    end

    def any?(cls : T.class, key : K, & : T -> Bool) : Bool forall T, K
      each(cls, key) do |fact|
        return true if yield fact
      end

      false
    end

    def reject!(cls : T.class, key : K, & : T -> Bool) : Nil forall T, K
      {% begin %}
        {% table = TABLES[T.symbolize] %}

        {% unless K <= table[:key_type].resolve %}
          {% K.raise "invalid key type #{K}, expected #{table[:key_type]}" %}
        {% end %}

        return false unless buckets = @{{table[:name]}}
        return false unless bucket = buckets[key]?

        bucket.reject! { |fact| yield fact.as(T) }
        if bucket.empty?
          buckets.delete(key)
        end
      {% end %}
    end

    def add?(fact : Fact) : Bool
      {% for fact_type, table in TABLES %}
        pass do
          next unless fact.is_a?({{fact_type.id}})
          buckets = @{{table[:name]}} ||= {} of {{table[:key_type]}} => Set({{fact_type.id}})
          bucket = buckets.put_if_absent(fact.{{table[:key]}}) { Set({{fact_type.id}}).new }
          return bucket.add?(fact)
        end
      {% end %}

      facts = @unkeyed ||= Set(Fact).new
      facts.add?(fact)
    end

    def delete(fact : Fact) : Bool
      {% for fact_type, table in TABLES %}
        pass do
          next unless fact.is_a?({{fact_type.id}})
          return false unless buckets = @{{table[:name]}}
          return false unless bucket = buckets[fact.{{table[:key]}}]?
          return false unless bucket.delete(fact)

          if bucket.empty?
            buckets.delete(fact.{{table[:key]}})
          end

          return true
        end
      {% end %}

      @unkeyed.try(&.delete(fact)) || false
    end

    def pretty_print(pp)
      pp.list("KeyedFactSet[", self, "]")
    end
  end

  class World
    include Enumerable(Fact)

    {% begin %}
      # :nodoc:
      alias FactClass = Union({{Fact.union_types.map(&.class).splat}})
    {% end %}

    def initialize
      @facts = {} of FactClass => KeyedFactSet
      @version = 0u64
    end

    def version : UInt64
      @version
    end

    def includes?(fact : Fact) : Bool
      return false unless bucket = @facts[fact.class]?
      return false unless bucket.includes?(fact)

      true
    end

    def each(& : Fact ->) : Nil
      @facts.each do |_, bucket|
        bucket.each { |fact| yield fact }
      end
    end

    def each(cls : T.class, & : T ->) : Nil forall T
      {% unless T < Fact %}
        {% T.raise "expected a Fact class, not #{T}" %}
      {% end %}

      return unless bucket = @facts[cls]?

      bucket.each do |fact|
        yield fact.as(T)
      end
    end

    def each(cls : T.class, key, & : T ->) : Nil forall T
      {% unless T < Fact %}
        {% T.raise "expected a Fact class, not #{T}" %}
      {% end %}

      return unless bucket = @facts[cls]?

      bucket.each(cls, key) do |fact|
        yield fact.as(T)
      end
    end

    def any?(cls : T.class, *args, & : T -> Bool) : Bool forall T
      {% unless T < Fact %}
        {% T.raise "expected a Fact class, not #{T}" %}
      {% end %}

      return false unless bucket = @facts[cls]?
      return false unless bucket.any?(cls, *args) { |fact| yield fact.as(T) }

      true
    end

    def add(fact : Fact) : Nil
      bucket = @facts.put_if_absent(fact.class) { KeyedFactSet.new }
      if bucket.add?(fact) # added
        @version += 1
      end
    end

    def delete(fact : Fact) : Nil
      return unless bucket = @facts[fact.class]?
      return unless bucket.delete(fact) # removed

      if bucket.size.zero? # empty
        @facts.delete(fact.class)
      end

      @version += 1
    end

    def reject!(cls : T.class, key, & : T -> Bool) : Nil forall T
      {% unless T < Fact %}
        {% T.raise "expected a Fact class, not #{T}" %}
      {% end %}

      return unless bucket = @facts[cls]?

      size0 = bucket.size
      bucket.reject!(cls, key) { |fact| yield fact }
      size1 = bucket.size

      if size1.zero? # empty
        @facts.delete(cls)
      end

      if size0 > size1 # removed
        @version += 1
      end
    end

    def pretty_print(pp)
      pp.list("World[", self, "]")
    end
  end
end
