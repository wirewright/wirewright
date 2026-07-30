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
    alias Notification = PathService::Notification | HTTPService::Notification

    # :nodoc:
    def initialize(
      @readings : ReadingMap,
      @reports : ReportMap,
      @resources : ResourceMap,
      @send : Notification ->,
    )
      @relays = [] of ServiceBroadcast::QueueId
    end

    # Constructs an extrinsic map.
    #
    # - *alarm* is `#call`ed on invalidation.
    # - *msgq* must be thread-safe, and respond to `#<<(Reload)`.
    def self.new(msgq : Q?, alarm : A) : ExtrinsicMap forall Q, A
      readings = PromiseMap(NormalPath, PathService::Reading).new
      reports = PromiseMap(NormalPath, PathService::Report).new
      resources = PromiseMap(ResourceService::Query, ResourceService::Response).new

      msgloop = Msgloop(Q, A).new(msgq, alarm, readings, reports, resources)

      send = ->(notification : Notification) do
        msgloop.receive(notification)
      end

      new(readings, reports, resources, send)
    end

    def self.new(alarm) : ExtrinsicMap
      new(nil, alarm)
    end

    private class Msgloop(Q, A)
      def initialize(
        @msgq : Q?,
        @alarm : A,
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
        return unless {@resources.invalidate?, @readings.invalidate?(msg.path)}.any?

        reload_refs
      end

      private def handle(msg : PathService::ReportInvalid) : Nil
        # Invalidate parent for directory observers, when we receive a directory
        # entry event.
        #
        # Invalidate the path itself for file observers, or directory observers
        # to refresh entries.
        return unless {@resources.invalidate?, @reports.invalidate?(msg.path.parent, msg.path)}.any?

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

        if queue = @msgq
          queue << Reload.new
        end
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

    private def ensure_running! : Nil
      return if @relays.present?

      qids = BlockingQueue(ServiceBroadcast::QueueId).new

      spawn(name: "ExtrinsicMap path invalidation relay") do
        PathService.listen(qids) do |notification|
          @send.call(notification)
        end
      end

      spawn(name: "ExtrinsicMap HTTP invalidation relay") do
        HTTPService.listen(qids) do |notification|
          @send.call(notification)
        end
      end

      @relays << qids.shift
      @relays << qids.shift

      Log.trace { "ExtrinsicMap fibers are running" }
    end

    # Worker fibers are started automatically when needed. They keep running
    # until you **explicitly** call `shutdown`. They will start automatically
    # later, if necessary.
    def shutdown : Nil
      return if @relays.empty?

      wg = WaitGroup.new(@relays.size)
      @relays.each do |relay|
        PathService.broadcast(StopListening.new(relay, wg))
        HTTPService.broadcast(StopListening.new(relay, wg))
      end

      wg.wait

      Log.trace { "ExtrinsicMap fibers are stopped" }

      @relays.clear
    end

    # Adds *ref* to the map. After adding *ref*, you can start polling
    # it using the corresponding `[]?` method.
    def add(ref : ReadingRef) : Nil
      ensure_running!

      PathService.connect(ref.path.parent, ref.path, PathService::Reading)
      PathMonitorService.add(ref.path.parent).wait

      @readings.add(ref.path) { PathService.read(ref.path) }
    end

    # :ditto:
    def add(ref : ReportRef) : Nil
      ensure_running!

      PathMonitorService.add(ref.path).wait

      @reports.add(ref.path) { PathService.report(ref.path) }
    end

    # :ditto:
    def add(ref : ResourceRef) : Nil
      ensure_running!

      @resources.add(ref.query) { ResourceService.get(ref.query) }
      @resources.touch(ref.query)
    end

    # Removes *ref* from the map.
    def delete(ref : ReadingRef) : Nil
      PathService.disconnect(ref.path.parent, ref.path, PathService::Reading)
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
