require "./src/wirewright"

# Space objects serve as bridges between `Step` and a `Tconn`.
#
# - One space object corresponds to one `Tconn`.
# - `Step` points to many such space objects.
class Meridium::Space
  # Constructs a Meridium space object.
  #
  # - *set*, *chat*, and *kwargs* are passed to `Tconn`'s constructor.
  # - *alert* is called each time a public view is updated. There are no guarantees
  #   about the number of times it will be called, by whom it will be called, whether
  #   it will be called about the most up-to-date view or about a view that is already
  #   outdated, etc.; its intended use it so wake someone up if they are sleeping to
  #   have them `monitor` the newest view. If they're already awake they should simply
  #   ignore the alert. Note that *alert* may be called excessively; it is your
  #   responsibility to ignore efficiently.
  def initialize(set, chat, @alert : -> = ->{ }, **kwargs)
    # Represents the goal state that is exclusively set() by the document
    # step function, and is read in the syncloop.
    @goal = Atomic(Term::Dict?).new(nil)

    # We split views into "public" and "staging" because we only want sensors
    # in @state to be in view during `monitor`. This means we must only "commit"
    # the view after we've merged a sensor into @state.
    @view_public = Atomic(Term::Dict).new(Term[])
    @view_staging = Atomic(Term::Dict).new(Term[])

    # Represents the state that was successfully acknowledged by Tsetconn.
    # This is exclusively set() in the syncloop, and read by the document
    # thread. This acts as "feedback" to the document about the latest
    # sync'd state so that the document can label surfaces as "in sync" /
    # "out of sync".
    @state = Atomic(Term::Dict).new(Term[])

    @online = Atomic(Bool).new(false)

    conn = Tconn.new(set, chat, observer, **kwargs)

    @surfaces = Tsetconn.new(conn)
  end

  def self.local(**kwargs) : Space
    set = TspaceDigestSet.new(SyncInMemoryMultiset(Bytes).new)
    chat = SyncInMemoryChat(Activation).new
    instance = new(set, chat, **kwargs)
    instance.online
    instance
  end

  def self.remote(server : T, **kwargs) : Space forall T
    {% unless T < IChat(Activation) %}
      {% raise "server must include IChat(Activation)" %}
    {% end %}

    {% unless T < IRemoteSet(Bytes) %}
      {% raise "server must include IRemoteSet(Bytes)" %}
    {% end %}

    set = TspaceDigestSet.new(UnbufferedSet.new(server))
    chat = server
    instance = new(set, chat, **kwargs)

    server.on_connection_state_changed do |state|
      state ? instance.online : instance.offline
    end

    instance
  end

  # Commits internal view to public view.
  private def upview : Nil
    view0 = @view_public.get(:acquire)
    view1 = @view_staging.get(:acquire)
    return if view0 == view1

    # If CAS succeeds, we won and have updated the public view.
    # If CAS fails, we were outrun by someone and they updated the public view.
    @view_public.compare_and_set(view0, view1, :release, :relaxed)
  end

  # NOTE: obsever fn can be called from multiple fibers simultaneously;
  # each passing a different views. Each view has a strictly increasing
  # version number; we're guaranteed that. So we only process a view whose
  # version number is newer than the one we already know, then use a single
  # CAS to make sure we weren't outrun by an even-newer view.
  private def observer : Tconn::Observer
    # FIXME: Tsetconn::Observer instead of this ugliness !!!!
    Tconn::Observer.new do |view|
      view1 = @view_staging.get(:acquire)

      version0 = view1[:version]?.try(&.as_n?) || Term[0]
      version1 = Term[view.version]

      next unless version1 > version0

      view2 = Term::Dict.build do |commit|
        commit.with(:version, version1)

        # TODO: Tview should have a change list, which we then merge into the view;
        # Then here we'd only have to iterate over the changes.
        view.each do |_, stimuli|
          # Thanks to `Tsetconn` we know that different slots would have different
          # identities (patterns, secrets, etc.), so we don't need to merge anything.
          commit.with({stimuli.surface.pattern, stimuli.surface.secret}, stimuli.dict_multiset)
        end
      end

      # If CAS succeeds, we won and our view is the newest.
      # If CAS fails, we were outrun by someone with a newer view.
      _, _ = @view_staging.compare_and_set(view1, view2, :release, :relaxed)

      unless @goal.get(:acquire)
        upview
        @alert.call
      end
    end
  end

  private def identity?(idcls : Tsetconn::Sensor.class, surface : Term) : Tsetconn::Sensor?
    return unless surface = surface.as_d?
    return unless surface.size.in?(1, 2)

    idcls.new(pattern: surface[0], secret: surface[1]?)
  end

  private def identity?(idcls : Tsetconn::Appearance.class, surface : Term) : Tsetconn::Appearance?
    return unless surface = surface.as_d?
    return unless surface.size.in?(1, 2)

    idcls.new(value: surface[0], secret: surface[1]?)
  end

  private def tr(before, after, idcls, key)
    added, removed = after.diff(before)

    removed.each_entry do |surface, _|
      next unless identity = identity?(idcls, surface)

      Log.debug { "delete #{identity}" }

      @surfaces.delete(identity)

      # Send feedback to the current state. We're the only ones modifying it.
      # Everyone else has read-only access.
      state0 = @state.get(:acquire)
      state1 = state0.morph({key, surface, nil})
      @state.set(state1, :release)

      upview
      @alert.call
    end

    added.each_entry do |surface, _|
      next unless identity = identity?(idcls, surface)

      Log.debug { "add #{identity}" }

      @surfaces.add(identity)

      # Ditto
      state0 = @state.get(:acquire)
      state1 = state0.morph({key, surface, true})
      @state.set(state1, :release)

      upview
      @alert.call
    end
  end

  private def tr(state : Term::Dict, goal : Term::Dict) : Nil
    sensors0 = state[:sensors]?.try(&.as_d?) || Term[]
    sensors1 = goal[:sensors]?.try(&.as_d?) || Term[]

    appearances0 = state[:appearances]?.try(&.as_d?) || Term[]
    appearances1 = goal[:appearances]?.try(&.as_d?) || Term[]

    tr(before: sensors0, after: sensors1, idcls: Tsetconn::Sensor, key: Term.of(:sensors))
    tr(before: appearances0, after: appearances1, idcls: Tsetconn::Appearance, key: Term.of(:appearances))
  end

  private def sync(goal : Term::Dict) : Nil
    tr(@state.get(:acquire), goal)
  end

  private def syncloop : Nil
    goal0 = @goal.get(:acquire)

    while true
      sync(goal0) if goal0

      goal0, ok = @goal.compare_and_set(goal0, nil, :release, :acquire)
      next unless ok

      # In case we're suspended here (meaning there's no way back) but before
      # CAS; and control reaches the observer, the observer will think we're
      # still up and won't do an upview but we are actually about to go to sleep.
      # So force an upview for that case.
      upview

      @alert.call

      break
    end
  end

  protected def online : Nil
    Log.trace { "online" }

    @online.set(true, :release)
    @alert.call
  end

  protected def offline : Nil
    Log.trace { "offline" }

    @online.set(false, :release)
    @alert.call
  end

  # Returns the latest *state* and *view* of this space, followed by the momentary
  # *online* status.
  #
  # - *state* is used as feedback on `approach`. Whenever the underlying Tconn
  #   acknowledges part of an `approach`-d goal, it is incorporated by `Space`
  #   into the latest *state*.
  # - *view* contains the stimuli of sensors in *state*.
  # - *online* says whether the connection is up or down at the time of calling.
  def monitor : {Term::Dict, Term::Dict, Bool}
    {@state.get(:acquire), @view_public.get(:acquire), @online.get(:acquire)}
  end

  # Sets a new synchronization goal for this space. If no synchronization process
  # is active, this method starts one to update the termspace state to match
  # the new goal.
  #
  # `monitor` can be used to monitor the progress of synchronization.
  def approach(goal : Term::Dict) : Nil
    return if @goal.swap(goal, :release)

    Log.trace { "spawn syncloop" }

    # If previous was nil, there's no thread to sync. Spawn a thread to sync.
    spawn syncloop
  end

  def clear : Nil
    approach(Term[])
  end
end

class Meridium::Step
  Log = ::Log.for(self)

  @spaces = {} of Term => Space
  @lock = Mutex.new

  def register(name : Term, space space1 : Space) : Nil
    @lock.synchronize do
      if space0 = @spaces[name]?
        space0.clear
      end

      @spaces[name] = space1
    end
  end

  def unregister(name : Term) : Nil
    return unless space = @lock.synchronize { @spaces.delete(name) }

    space.clear
  end

  private def rendezvous(document : Term::Dict) : Term::Dict
    unless tspaces = document[Rhodium::Tspaces]?.try(&.as_d?)
      document = document.morph({Rhodium::Tspaces, Term[]})
      tspaces = Term[]
    end

    states = @lock.synchronize do
      @spaces.compact_map do |name, space|
        goal = tspaces[name]?.try(&.as_d?)
        goald = goal || Term[]

        state, view, online = space.monitor

        if online && goald.pluck(:sensors, :appearances) != state
          space.approach(goald)
        end

        goal ? {name, goald, state, view, online} : nil
      end
    end

    events = Rhodium::Q.of(document, Rhodium::Events)

    states.each do |tspace, content, syncd, view1, online|
      # Update sync status.
      sensors = content[:sensors]? || Term[]
      sensors.each_entry do |sensor, _|
        in_sync = online && !!syncd[:sensors, sensor]?
        content = content.morph({:sensors, sensor, in_sync})
      end

      appearances = content[:appearances]? || Term[]
      appearances.each_entry do |appearance, _|
        in_sync = online && !!syncd[:appearances, appearance]?
        content = content.morph({:appearances, appearance, in_sync})
      end

      # Emit stimuli events if stimuli changed.
      view0 = content[:view]?.try(&.as_d?) || Term[]
      view1.each_entry do |sensor, stimuli1|
        if (stimuli0 = view0[sensor]?) && stimuli0 == stimuli1
          next
        end
        events = events.enqueue(:stimuli, tspace, sensor, stimuli1)
      end

      content = content.morph({:view, view1})
      document = document.morph({Rhodium::Tspaces, tspace, content})
    end

    events.commit(document, Rhodium::Events)
  end

  def fn : D7::Step
    D7::Step.new do |document|
      {rendezvous(document), false}
    end
  end
end

# set = SyncInMemoryMultiset(Tspace::Atom).new
# chat = SyncInMemoryChat(Activation).new
# space = Meridium::Space.new(set, chat, alert: ->{ puts "WAKE UP!!!" })

# goal = ML.dict <<-WWML
# sensors: { (x_number): true, },
# appearances: { (100): true, }
# WWML

# space.approach(goal)
# pp space.monitor

# sleep 1.second

# pp space.monitor
# sleep 1.second

# goal = ML.dict <<-WWML
# sensors: { (x_number): true, },
# WWML

# space.approach(goal)

# sleep 1.second

# pp space.monitor
# sleep
