{% skip_file if flag?(:noconn) %}

module Ww
  # Connections own localized regions of the termspace/termbase called *pockets*.
  # Connections control the content of their pocket with plans they execute. Those
  # plans are automatically generated from pocket state in `Tconn#submit`.
  #
  # If a connection is currently executing its plan, or is otherwise busy, its region
  # remains stable; no one can change it, this is guaranteed by the system.
  #
  # `Tconn#submit` is the main method you will use. It should only be called by one
  # fiber since it is not thread-safe. A `Tconn` must be reachable and onwed only by
  # one fiber anyway. Do not share a `Tconn` between multiple fibers; there's no point
  # in doing that.
  class Tconn
    # `Tspace`-unique connection id.
    alias Id = UInt32

    # Captures the state of essential data structures before and after plan execution.
    #
    # The phrasing "at some point" (e.g. "at some point before") is used, and it is
    # important to clarify why.
    #
    # When making a snapshot of the entire termspace/termbase, the connection is only
    # guaranteed a stable, coherent view of its localized region within the snapshot
    # (its pocket). This is because the connection is solely in control of that pocket;
    # no one else can modify it while the connection is making a snapshot. Everything
    # else, however, is unstable. Thus globally, the snapshot is made at some undefined
    # point in time; but relative to the connection's localized region, it could be
    # in past (before the connection modified its localized region and is busy doing
    # something else, e.g. making a snapshot), in present (after the connection modified
    # its localized region and is busy doing someting else, e.g. making a snapshot), etc.
    private struct Transition
      # Returns the termbase at some point before the plan was executed.
      getter tbase0 : Tbase::Snapshot

      # Returns the termspace at some point before the plan was executed.
      getter tspace0 : Tspace::Snapshot

      # Returns the registry at some point before the plan was executed.
      getter registry0 : Registry

      # Returns the predecessor form.
      getter form0 : Tform

      # Returns the termbase at some point after the plan was executed.
      getter tbase1 : Tbase::Snapshot

      # Returns the termspace at some point after the plan was executed.
      getter tspace1 : Tspace::Snapshot

      # Returns the registry at some point after the plan was executed.
      getter registry1 : Registry

      # Returns the successor form.
      getter form1 : Tform

      protected def initialize(@tbase0, @tspace0, @registry0, @form0, @tbase1, @tspace1, @registry1, @form1)
      end
    end

    # Not sure if this thing works but it sure seems to.
    #
    # The intended "strategic" goal of `MessageOptimizer` is to avoid sending
    # unnecessary EdgeRemoved events and thus bombarding clients with an unnecessary
    # palette of patches to select from in `TserverFacade`. A small appearance change
    # could cause 3 or more patches to be sent for the client to select from. This
    # defeats all savings made with smart patch/model-countermodel system.
    #
    # We thus only send messages for sensorN->appearanceM edges and appearanceN->sensorM
    # edges for maximum N->M (originating from newest source and targeted at newest destination);
    # dropping all messages from older sources or to older destinations, including inhibitory/
    # deprivation messages. Again, I'm not quite sure it works; more testing/thought is necessary.
    private struct MessageOptimizer
      def initialize
        @stoa = {} of {Id, Id, Surface::Id, Surface::Id} => Tspace::SensorToAppearanceMessage
        @atos = {} of {Id, Id, Surface::Id, Surface::Id} => Tspace::AppearanceToSensorMessage
      end

      def <<(message : Tspace::SensorToAppearanceMessage) : Nil
        key = {message.sender, message.receiver, message.src, message.dst}

        if latest = @stoa[key]?
          return unless latest.srcv <= message.srcv && latest.dstv <= message.dstv
        end

        @stoa[key] = message
      end

      def <<(message : Tspace::AppearanceToSensorMessage) : Nil
        key = {message.sender, message.receiver, message.src, message.dst}

        if latest = @atos[key]?
          return unless latest.srcv <= message.srcv && latest.dstv <= message.dstv
        end

        @atos[key] = message
      end

      def each(& : Tspace::SurfaceMessage ->) : Nil
        @stoa.each_value { |message| yield message }
        @atos.each_value { |message| yield message }
      end
    end

    def initialize(@id : Id, @tbase : Tbase, @tspace : Tspace)
    end

    # Responds to `self`'s sensor being added during plan execution.
    private def respond(messages, tr : Transition, ann : Announcement::SensorAdded) : Nil
      conn0, id0 = tr.tspace1.resolve(ann.surface)
      return unless row = tr.form1.row?(Sensor.new(id0))

      tr.tbase1.each_candidate_appearance_by_sensor(row.query, tr.registry1) do |surface|
        next unless neighbor = tr.tspace1.resolve?(surface)

        conn1, id1 = neighbor
        messages << Tspace::Message::TryExcite.new(
          sender: conn0,        # < Sender connection (owner of sensor)
          receiver: conn1,      # < Receiver connection (owner of neighbor appearance)
          src: id0,             # < Source sensor, sender-local
          srcv: ann.surface.id, # < Source sensor version, global, monotonically increasing id
          dst: id1,             # < Destination appearance, receiver-local
          dstv: surface.id,     # < Destination appearance version, global, monotonically increasing id
          token: row.token,
          query: row.query,
        )
      end
    end

    # Responds to `self`'s sensor being deleted during plan execution.
    private def respond(messages, tr : Transition, ann : Announcement::SensorDeleted) : Nil
      conn0, id0 = tr.tspace0.resolve(ann.surface)
      return unless row = tr.form0.row?(Sensor.new(id0))

      tr.tbase1.each_candidate_appearance_by_sensor(row.query, tr.registry0) do |surface|
        next unless neighbor = tr.tspace1.resolve?(surface)

        conn1, id1 = neighbor

        messages << Tspace::Message::TryInhibit.new(
          sender: conn0,        # < Sender connection (owner of sensor)
          receiver: conn1,      # < Receiver connection (owner of neighbor appearance)
          src: id0,             # < Source sensor, sender-local
          srcv: ann.surface.id, # < Source sensor version, global, monotonically increasing id
          dst: id1,             # < Destination appearance, receiver-local
          dstv: surface.id,     # < Destination appearance version, global, monotonically increasing id
        )
      end
    end

    # Responds to `self`'s appearance being added during plan execution.
    private def respond(messages, tr : Transition, ann : Announcement::AppearanceAdded) : Nil
      conn0, id0 = tr.tspace1.resolve(ann.surface)
      return unless row = tr.form1.row?(Appearance.new(id0))

      tr.tbase1.each_candidate_sensor_by_appearance(row.value, tr.registry1) do |surface|
        next unless neighbor = tr.tspace1.resolve?(surface)

        conn1, id1 = neighbor

        messages << Tspace::Message::TryPerceive.new(
          sender: conn0,        # < Sender connection (owner of appearance)
          receiver: conn1,      # < Receiver connection (owner of neighbor sensor)
          src: id0,             # < Source appearance, sender-local
          srcv: ann.surface.id, # < Source appearance version, global, monotonically increasing id
          dst: id1,             # < Destination sensor, receiver-local
          dstv: surface.id,     # < Destination sensor version, global, monotonically increasing id
          token: row.token,
          value: row.value,
        )
      end
    end

    # Responds to `self`'s appearance being deleted during plan execution.
    #
    # - If tombstone is defined for the appearance that tombstone is shown to
    #   everyone interested. Note that sensors will keep the tombstone forever,
    #   until their own demise (as if it was an image or a memory of the appearance).
    # - A deprive message is sent to all sensors that were previously perceiving
    #   the appearance.
    private def respond(messages, tr : Transition, ann : Announcement::AppearanceDeleted) : Nil
      conn0, id0 = tr.tspace0.resolve(ann.surface)
      return unless row = tr.form0.row?(Appearance.new(id0))

      tr.tbase1.each_candidate_sensor_by_appearance(row.value, tr.registry0) do |surface|
        next unless neighbor = tr.tspace1.resolve?(surface)

        conn1, id1 = neighbor

        messages << Tspace::Message::TryDeprive.new(
          sender: conn0,        # < Sender connection (owner of appearance)
          receiver: conn1,      # < Receiver connection (owner of sensor)
          src: id0,             # < Source appearance, sender-local
          srcv: ann.surface.id, # < Source appearance version, global, monotonically increasing id
          dst: id1,             # < Destination sensor, receiver-local
          dstv: surface.id,     # < Destination sensor version, global, monotonically increasing id
        )
      end

      return unless tombstone = row.tombstone

      tr.tbase1.each_candidate_sensor_by_appearance(tombstone, tr.registry1) do |surface|
        next unless neighbor = tr.tspace1.resolve?(surface)

        conn1, id1 = neighbor

        messages << Tspace::Message::TryPerceive.new(
          sender: conn0,        # < Sender connection (owner of appearance)
          receiver: conn1,      # < Receiver connection (owner of sensor)
          src: id0,             # < Source appearance, sender-local
          srcv: ann.surface.id, # < Source appearance version, global, monotonically increasing id
          dst: id1,             # < Destination sensor, receiver-local
          dstv: surface.id,     # < Destination sensor version, global, monotonically increasing id
          token: row.token,
          value: tombstone,
        )
      end
    end

    # Responds to a non-null effect. Appends replies to the given message buffer *messages*.
    private def respond(messages, tr : Transition, effect : Effect::Some) : Nil
      effect.each_announcement { |ann| respond(messages, tr, ann) }
    end

    # Returns the current `Tform` of this connection. The returned `Tform` can be
    # extended (see e.g. `Tform#query`, `Tform#value`, etc.) and then submitted as
    # the new `Tform` of this connection (see `submit`).
    def form : Tform
      @tspace.form?(@id) || form0
    end

    def form0 : Tform
      Tform.new(@id)
    end

    # Replaces the current `Tform` used by this connection with *form1*.
    #
    # May raise `TbaseAbortException` if the `Tbase` aborted while trying to
    # submit *form1*. In this case, the current `Tform` will remain unchanged;
    # no changes from *form1* are going to be committed.
    #
    # May raise `TbaseCrashException` if the `Tbase` crashed due to an implementation
    # error while trying to submit *form1*. Clients must somehow inform the management
    # infrastructure of the crash, so that the `Tbase` that crashed can be disposed
    # immediately for everyone, and a new one put as a replacement together with
    # the appropriate reconnect requests.
    def submit(form1 : Tform) : Nil
      return unless @id == form1.conn_id

      tb0 = @tbase.snapshot
      ts0 = @tspace.snapshot

      form0 = form

      # This line can raise `TbaseAbortException` (plan was aborted) or `TbaseCrashException`
      # (plan killed the termbase). However, as you can see, we didn't commit anything yet
      # on our level; thus we can safely let them bubble up, without any kind of recovery.
      effect = form0.morph(into: form1, tbase: @tbase, tspace: @tspace)

      @tspace.state(@id, form1)

      return unless effect.is_a?(Effect::Some)

      tb1 = @tbase.snapshot
      ts1 = @tspace.snapshot

      tr = Transition.new(
        tb0, ts0, effect.registry0, form0,
        tb1, ts1, effect.registry1, form1,
      )

      messages = MessageOptimizer.new

      respond(messages, tr, effect)

      messages.each { |message| @tspace.send(message) }
    end
  end
end
