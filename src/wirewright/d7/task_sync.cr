# An experimental way of managing asynchronous tasks for D7 clients.
#
# TODO: Write more meaningful docs!
class D7::TaskSync(Alarm, Task, Result)
  def initialize(@alarm : Alarm, &@fn : Task, Ping -> Result)
    @clock = Atomic(UInt64).new(0)
    @generation = 0u64
    @demand = {} of Task => UInt64
    @supply = Pf::Map(Task, Result).new
    @lock = Sync::Mutex.new
  end

  alias Ping = ->

  private class Canceled < Exception
    @callstack = CallStack.empty
  end

  private class OutOfTime < Exception
    @callstack = CallStack.empty
  end

  # Returns `true` if there are pending tasks. `Alarm` will be notified of
  # their completion.
  def pending? : Bool
    @lock.synchronize { @demand.present? || @supply.present? }
  end

  protected def reap : Pf::Map(Task, Result)
    @lock.synchronize do
      @supply, _ = Pf::Map(Task, Result).new, @supply
    end
  end

  protected def publish(task : Task, throttle : UInt64) : Nil
    @lock.synchronize do
      had_key = @demand.has_key?(task)

      # We update unconditionally because `#collect` depends on this to know
      # which tasks to collect.
      @demand[task] = @generation

      if had_key
        return # Deferred, already scheduled.
      end
    end

    ping = -> do
      if throttle > 0
        time = @clock.add(1, :relaxed)
        return unless time % throttle == 0
      end

      @lock.synchronize do
        unless @demand.has_key?(task)
          raise Canceled.new
        end
      end
    end

    spawn(name: "Ww::D7::TaskSync task") do
      result = @fn.call(task, ping)

      @lock.synchronize do
        next unless @demand.has_key?(task) # Just to be extra sure.

        @supply = @supply.assoc(task, result)
        @alarm.call
      end
    rescue Canceled
    end
  end

  protected def publish?(task : Task, throttle : UInt64, deadline : Time::Span) : Result?
    started_at : Time::Instant? = nil

    # NOTE: We don't handle cancellation in the fast path since it's pure
    # overhead. Assume the calling fiber *is* the fiber that cancels.
    ping = -> do
      if throttle > 0
        time = @clock.add(1, :relaxed)
        return unless time % throttle == 0
      end

      unless start = started_at
        started_at = Time.instant
        return
      end

      now = Time.instant
      duration = now - start
      if duration >= deadline
        raise OutOfTime.new
      end
    end

    begin
      return @fn.call(task, ping)
    rescue OutOfTime
    end

    publish(task, throttle)

    nil # Deferred
  end

  protected def cancel(tasks : Enumerable(Task)) : Nil
    @lock.synchronize do
      tasks.each do |task|
        @demand.delete(task)
      end
    end
  end

  protected def collect
    @lock.synchronize do
      @demand.select! { |_, generation| @generation == generation }
      @generation += 1
    end
  end

  # See `TaskSync#step`.
  struct Session(Alarm, Task, Result)
    protected def initialize(@tsync : TaskSync(Alarm, Task, Result), @map : Pf::Map(Task, Result))
    end

    # Returns the harvested result of *task*, if available.
    def result?(task : Task) : Result?
      @map[task]?
    end

    # Publishes or keeps alive the given *task*.
    #
    # - If *throttle* is nonzero, pings made by the function that's running
    #   the task are ignored except on every *throttle*th tick of a logical
    #   clock internal to `TaskSync`.
    def publish(task : Task, *, throttle : UInt64 = 0u64) : Nil
      @tsync.publish(task, throttle)
    end

    # Publishes or keeps alive the given *task*.
    #
    # - If *throttle* is nonzero, pings made by the function that's running
    #   the task are ignored except on every *throttle*th tick of a logical
    #   clock internal to `TaskSync`.
    # - *deadline*, triggers the execution of *task* immediately (inside the call
    #   to `publish`). If the call completes within *deadline*, its result is
    #   returned. Otherwise, the task is scheduled normally on a worker fiber.
    #
    # NOTE: *deadline* is cooperative; it can (and probably will!) be violated,
    # depending on how often the task execution function pings and on *throttle*,
    # among other things.
    def publish?(task : Task, *, throttle : UInt64 = 0u64, deadline : Time::Span) : Result?
      @tsync.publish?(task, throttle, deadline)
    end
  end

  # Wraps a step of a reactive reconciliation loop (the block).
  #
  # Harvests the results of all completed tasks, and yields a `Session` object
  # so that the block can read results of completed tasks, schedule tasks, or
  # keep pending tasks alive. After the block returns, collects canceled tasks
  # (i.e., tasks that the block did not at least "ping" to keep them alive).
  def step(&)
    begin
      yield Session.new(self, reap)
    ensure
      collect
    end
  end
end
