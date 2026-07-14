module Ww
  # A uniform surface API for services that work with *extrinsics*: file
  # system readings, reports, HTTP, etc. Also sets watch handles for paths
  # where possible and manages automatic, transparent invalidation with
  # the help of `PromiseMap`.
  #
  # On the user end, using `ExtrinsicMap` is as simple as `add`ing a ref
  # and then polling it at any desired rate or reactively (see `new`).
  # None of the methods do IO; IO is done deep in the internals of `PathService`,
  # `HTTPService` and so on, on dedicated fibers. You are simply performing or
  # calling for "rendezvous" here, with `ExtrinsicMap` and various other facilities
  # "cushioning" your calls while the rendezvous is being arranged.
  class ExtrinsicMap
    Log = ::Log.for(self)

    alias Ref = ReadingRef | ReportRef | ResourceRef

    defrecord ReadingRef, path : NormalPath
    defrecord ReportRef, path : NormalPath
    defrecord ResourceRef, query : ResourceService::Query

    defrecord Reload

    # :nodoc:
    alias ReadingMap = PromiseMap(NormalPath, PathService::Reading)
    # :nodoc:
    alias ReportMap = PromiseMap(NormalPath, PathService::Report)
    # :nodoc:
    alias ResourceMap = PromiseMap(ResourceService::Query, ResourceService::Response)

    # :nodoc:
    def initialize(
      @readings : ReadingMap,
      @reports : ReportMap,
      @resources : ResourceMap,
    )
    end

    # Constructs an extrinsic map.
    #
    # - *alarm* is called on any invalidation.
    # - *msgq* must be thread-safe, and respond to `#<<(Reload)`.
    def self.new(msgq, alarm : BlockingSignal) : ExtrinsicMap
      readings = PromiseMap(NormalPath, PathService::Reading).new
      reports = PromiseMap(NormalPath, PathService::Report).new
      resources = PromiseMap(ResourceService::Query, ResourceService::Response).new

      wg = WaitGroup.new(2)
      msgloop = Msgloop.new(msgq, alarm, readings, reports, resources)

      spawn(name: "ExtrinsicMap path invalidation relay") do
        PathService.listen(wg) do |notification|
          msgloop.receive(notification)
        end
      end

      spawn(name: "ExtrinsicMap HTTP invalidation relay") do
        HTTPService.listen(wg) do |notification|
          msgloop.receive(notification)
        end
      end

      wg.wait

      new(readings, reports, resources)
    end

    private class Msgloop(Q)
      def initialize(
        @msgq : Q,
        @alarm : BlockingSignal,
        @readings : ReadingMap,
        @reports : ReportMap,
        @resources : ResourceMap,
      )
      end

      def receive(msg : PathService::Notification) : Nil
        Log.trace { msg.class }

        handle(msg)
      end

      def receive(msg : HTTPService::Notification) : Nil
        Log.trace { msg.class }

        handle(msg)
      end

      private def handle(msg : PathService::ReadingInvalid) : Nil
        return unless @resources.invalidate? || @readings.invalidate?(msg.path)

        reload_refs
      end

      private def handle(msg : PathService::ReportInvalid) : Nil
        # Invalidate parent for directory observers, when we receive a directory
        # entry event.
        #
        # Invalidate the path itself for file observers, or directory observers
        # to refresh entries.
        return unless @resources.invalidate? || @reports.invalidate?(msg.path.parent, msg.path)

        reload_refs
      end

      private def handle(msg : PathService::ReadingReady) : Nil
        return unless @resources.invalidate? || @readings.includes?(msg.path)

        # If there's a race it's just a spurious wakeup/reload.
        reload_refs
      end

      private def handle(msg : PathService::ReportReady) : Nil
        return unless @resources.invalidate? || @reports.includes?(msg.path) || @reports.includes?(msg.path.parent)

        # If there's a race it's just a spurious wakeup/reload.
        reload_refs
      end

      private def handle(msg : HTTPService::ResponseReady) : Nil
        # Only resources from the resource map can be affected by HTTP invalidation
        # at the moment.
        return unless @resources.invalidate?

        reload_refs
      end

      private def reload_refs : Nil
        Log.trace { "Reload" }

        @msgq << Reload.new
        @alarm.call
      end
    end

    def includes?(ref : ReadingRef) : Bool
      @readings.includes?(ref.path)
    end

    def includes?(ref : ReportRef) : Bool
      @reports.includes?(ref.path)
    end

    def includes?(ref : ResourceRef) : Bool
      @resources.includes?(ref.query)
    end

    def size : Int32
      @readings.size + @reports.size + @resources.size
    end

    # Returns the current value of *ref*.
    def []?(ref : ReadingRef) : PathService::Reading?
      @readings[ref.path]?
    end

    # Returns the current value of *ref*.
    def []?(ref : ReportRef) : PathService::Report?
      @reports[ref.path]?
    end

    # Returns the current value of *ref*.
    def []?(ref : ResourceRef) : ResourceService::Response?
      @resources[ref.query]?
    end

    def each_ref(& : Ref ->) : Nil
      @readings.each_key { |path| yield ReadingRef.new(path) }
      @reports.each_key { |path| yield ReportRef.new(path) }
      @resources.each_key { |query| yield ResourceRef.new(query) }
    end

    # Adds *ref* to the map. After adding *ref*, you can start polling
    # it using the corresponding `[]?` method.
    def add(ref : ReadingRef) : Nil
      PathMonitorService.add(ref.path.parent)

      @readings.add(ref.path) { PathService.read(ref.path) }
    end

    # :ditto:
    def add(ref : ReportRef) : Nil
      PathMonitorService.add(ref.path)

      @reports.add(ref.path) { PathService.report(ref.path) }
    end

    # :ditto:
    def add(ref : ResourceRef) : Nil
      @resources.add(ref.query) { ResourceService.get(ref.query) }
      @resources.touch(ref.query)
    end

    # Removes *ref* from the map.
    def delete(ref : ReadingRef) : Nil
      PathMonitorService.delete(ref.path.parent)

      @readings.delete(ref.path)
    end

    # :ditto:
    def delete(ref : ReportRef) : Nil
      PathMonitorService.delete(ref.path)

      @reports.delete(ref.path)
    end

    # :ditto:
    def delete(ref : ResourceRef) : Nil
      @resources.delete(ref.query)
    end
  end
end
