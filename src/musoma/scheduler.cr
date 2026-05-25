module MuSoma
  class Scheduler
    defrecord Period, span : Time::Span
    defrecord Deadline, t : Instant

    struct Instant
      include Comparable(Instant)

      protected def initialize(@point : Time::Span)
      end

      def <=>(other : Instant)
        @point <=> other.@point
      end

      def +(delta : Time::Span) : Instant
        Instant.new(@point + delta)
      end

      def -(other : Instant) : Time::Span
        @point - other.@point
      end

      def /(period : Time::Span) : Float64
        @point / period
      end

      def repr : String
        @point.total_nanoseconds.to_i64.hash.to_s(base: 32)
      end
    end

    def initialize
      @origin = Time.instant
      @periods = [] of Time::Span
      @deadlines = [] of Instant
      @frontier = Instant.new(0.milliseconds)
    end

    def now : Instant
      Instant.new(Time.instant - @origin)
    end

    def add(object : Period) : Nil
      @periods << object.span
    end

    def add(object : Deadline) : Nil
      @deadlines << object.t
    end

    def delete(object : Period) : Nil
      @periods.delete(object.span)
    end

    def delete(object : Deadline) : Nil
      @deadlines.delete(object.t)
    end

    alias Event = Tick | Expire

    defrecord Tick, period : Time::Span, crossings : Int64
    defrecord Expire, deadline : Instant

    def tick(& : Event ->) : Nil
      now = self.now

      @periods.each do |period|
        if period.zero?
          # Zero period will make it always cross.
          crossings = 1i64
        else
          phase_frontier = (@frontier / period).floor.to_i64
          phase_now = (now / period).floor.to_i64
          crossings = phase_now - phase_frontier
        end

        next unless crossings > 0

        yield Tick.new(period, crossings)
      end

      @deadlines.select! do |deadline|
        if deadline <= now
          yield Expire.new(deadline)
          next false # reject
        end

        true # keep
      end

      @frontier = now
    end

    # Returns the amount of time the caller should wait for, from now, until
    # the nearest scheduled event.
    #
    # Returns `nil` if there are no events the caller needs to wait for.
    def timeout? : Time::Span?
      now = self.now

      nearest_period = @periods.min_of? { |period| Instant.new(period * (now / period).ceil) }
      nearest_deadline = @deadlines.min?

      if nearest_period && nearest_deadline
        nearest_event = {nearest_period, nearest_deadline}.min
      end

      nearest_event ||= nearest_period || nearest_deadline
      return unless nearest_event

      nearest_event - now
    end
  end
end
