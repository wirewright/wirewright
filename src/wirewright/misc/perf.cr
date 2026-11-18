module Ww
  # Crude in-house performance profiling.
  module IPerf
    abstract def stage(caption : String, &)
  end

  # Disables performance profiling.
  class NoPerf
    include IPerf

    def stage(caption : String, &)
      yield
    end
  end

  # Enables performance profiling. Use `next` to begin a new "epoch".
  class Perf
    include IPerf

    # :nodoc:
    class Node
      property caption : String
      property duration : Time::Span
      property calls : UInt32
      property children : Hash(String, Node)

      def initialize(@caption : String)
        @duration = Time::Span.zero
        @calls = 0u32
        @children = {} of String => Node
      end

      def display(io : IO, depth : UInt32) : Nil
        io.puts "#{"  " * depth}#{@caption}: #{@duration.humanize} (#{@calls} calls, avg. #{(@duration / @calls).humanize})"
        @children.each_value(&.display(io, depth + 1))
      end
    end

    def initialize(@stat : Bool, caption : String)
      @root = Node.new(caption)
      @root.calls = 1u32 # Nobody calls root so we have to pretend something called it
      @stack = [@root]
    end

    def next(caption : String) : Nil
      assert @stack.size == 1
      assert @stack.first.same?(@root)

      if @stat
        @root.display(STDOUT, depth: 0u32)
      end

      @root = Node.new(caption)
      @root.calls = 1u32
      @stack[0] = @root
    end

    def stage(caption : String, &)
      parent = @stack.last
      node = parent.children[caption] ||= Node.new(caption)

      @stack << node

      begin
        t0 = Time.instant
        result = yield
        t1 = Time.instant
        duration = t1 - t0

        node.duration += duration
        node.calls += 1

        result
      ensure
        @stack.pop
      end
    end
  end
end
