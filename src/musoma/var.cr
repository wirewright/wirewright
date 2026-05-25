module MuSoma
  class Var(T)
    private struct Version
      def initialize
        @version = 0u64
        @seen = [] of {UInt32, UInt64}
      end

      def inc : Nil
        @version += 1
      end

      def pending?(agent : UInt32) : Bool
        @seen.each_with_index do |(candidate, version), index|
          next unless agent == candidate

          assert version <= @version

          if version == @version
            return false # "No, you're up to date."
          end

          @seen[index] = {agent, @version}

          return true # "Yes, you need to update, I'll pretend you did."
        end

        @seen << {agent, @version}

        true # "Yes, you need to update, you're not even registered here!"
      end
    end

    private module IMember
      abstract def invalidate(map) : Nil
    end

    private class Member(K, V)
      include IMember

      getter key : K

      def initialize(@key : K, @value : V)
        @version = Version.new
      end

      def pending?(agent : UInt32) : Bool
        @version.pending?(agent)
      end

      def invalidate(map) : Nil
        return unless map.responds_to?(:[]?)

        value1 = map[@key]?
        return if @value == value1

        @value = value1
        @version.inc
      end
    end

    def initialize(@value : T)
      @version = Version.new
      @members = [] of IMember
    end

    # :nodoc:
    SERIAL = [0u32]

    macro pending?(*vars, or_if = false)
      {% id = SERIAL[0] %}
      {% SERIAL[0] += 1 %}

      {%
        # Importantly, we must evaluate all of pending?()s before returning,
        # because each pending?() has the side effect of registering the agent
        # and updating its last-seen value. The logic being, if you call `pending?`
        # with many vars, we mark all of them as seen, and then check if any
        # of them changed.
      %}
      { {{or_if}},
        {% for var, index in vars %}
          {% if var.is_a?(TupleLiteral) %}
            {{var[0]}}.pending?({{id}}, {{var[1..].splat}}),
          {% else %}
            {{var}}.pending?({{id}}),
          {% end %}
        {% end %} }.any?
    end

    # Returns the value of the variable.
    def get : T
      @value
    end

    # Sets the value of the variable.
    def set(value1 : T) : Nil
      return if @value == value1

      @value = value1
      @version.inc
      @members.each(&.invalidate(@value))
    end

    # Updates the value of this variable.
    def update(&) : Nil
      set(yield @value)
    end

    # :nodoc:
    def pending?(agent : UInt32) : Bool
      @version.pending?(agent)
    end

    # :nodoc:
    def pending?(agent : UInt32, key : K) : Bool forall K
      member = @members.find { |member| member.key == key }

      if member.nil?
        member = Member.new(key, @value[key]?)
        @members << member
      end

      member.pending?(agent)
    end
  end

  class VarHash(K, V)
    def initialize
      @hash = {} of K => V
      @seen = 0u64
      @version = 0u64
    end

    def get?(key : K) : V?
      @hash[key]?
    end

    def set(key : K, value1 : V) : Nil
      value0 = @hash[key]?
      return if value0 == value1

      @hash[key] = value1
      @version += 1
    end

    # Synchronizes with a reference hash *ref*.
    def sync(ref : Hash(K, V)) : Nil
      changed = false

      # Process deletions.
      @hash.reject! do |key, _|
        if ref.has_key?(key)
          next false # do not reject
        end

        changed = true

        true # reject
      end

      # Process additions & updates.
      ref.each do |key, value1|
        value0 = @hash[key]?
        next if value0 == value1

        changed = true
        @hash[key] = value1
      end

      if changed
        @version += 1
      end
    end

    def pending? : Bool
      if @seen < @version
        @seen = @version
        true
      else
        false
      end
    end
  end
end
