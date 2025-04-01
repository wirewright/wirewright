module Ww::Meridium
  extend self

  class TsetConn
    def initialize(@conn : Tconn)
      @fresh = Identity.new(0)

      # TODO: BidiHash
      @lsensors = {} of Term => Identity
      @rsensors = {} of Identity => Term

      @lappearances = {} of Term => Identity
      @rappearances = {} of Identity => Term
    end

    def close : Nil
      @conn.close
    end

    def pattern?(identity : Identity) : Term?
      @rsensors[identity]?
    end

    def value?(identity : Identity) : Term?
      @rappearances[identity]?
    end

    def add_sensor_for(pattern : Term) : Bool
      return false if @lsensors.has_key?(pattern)

      @lsensors[pattern] = @fresh
      @rsensors[@fresh] = pattern
      @conn[@fresh] = Tconn::Sensor.new(pattern)
      @fresh += 1

      true
    end

    def add_appearance_for(value : Term) : Bool
      return false if @lappearances.has_key?(value)

      @lappearances[value] = @fresh
      @rappearances[@fresh] = value
      @conn[@fresh] = Tconn::Appearance.new(value)
      @fresh += 1

      true
    end

    def delete_sensor_for(pattern : Term) : Bool
      return false unless identity = @lsensors.delete(pattern)

      @rsensors.delete(identity)
      @conn.delete(identity)

      true
    end

    def delete_appearance_for(value : Term) : Bool
      return false unless identity = @lappearances.delete(value)

      @rappearances.delete(identity)
      @conn.delete(identity)

      true
    end
  end

  record TspaceConfig,
    map : Tconn::Map,
    chat : Tconn::Chat,
    sink : Tconn::Sink,
    fresh : LabelGenerator,
    keepalive : Tconn::Keepalive?

  class TspaceRegistry
    alias Subscriber = TspaceConfig? ->
    alias Unsubscribe = ->

    def initialize
      @configs = {} of Term => TspaceConfig
      @subscribers = {} of Term => Set(Subscriber)
    end

    def []=(tsid : Term, spec : TspaceConfig)
      @configs[tsid] = spec

      return unless subscribers = @subscribers[tsid]?

      subscribers.each &.call(spec)
    end

    def delete(tsid : Term) : Nil
      return unless @configs.delete(tsid)
      return unless subscribers = @subscribers.delete(tsid)

      subscribers.each &.call(nil)
    end

    def call(tsid : Term, fn : Subscriber) : Nil
      return unless config = @configs[tsid]?

      fn.call(config)
    end

    def subscribe(tsid : Term, &fn : Subscriber) : Unsubscribe
      subscribers = @subscribers.put_if_absent(tsid) { Set(Subscriber).new }
      subscribers << fn

      -> do
        subscribers.delete(fn)
        if subscribers.empty?
          @subscribers.delete(tsid)
        end
      end
    end
  end

  class StepContext
    def initialize(@registry : TspaceRegistry)
      @state = Term[]
      @setconns = {} of Term => TsetConn
      @unsubscribe = {} of Term => TspaceRegistry::Unsubscribe
    end

    def step(document : Term::Dict) : Nil
      unless tspaces = document[Rhodium::Tspaces]?
        sync(Term[])
        return
      end

      unless tspaces = tspaces.as_d?
        sync(Term[])
        return
      end

      sync(tspaces)
    end

    def sync(tspaces : Term::Dict) : Nil
      return if @state.same?(tspaces) # Fast path

      added, changed, removed = tspaces.diff1x(@state)

      removed.each_entry do |tsid, _|
        disconnect(tsid)
      end

      changed.each_entry do |tsid, tspace|
        sync(tsid, tspace0: @state[tsid].as_d, tspace1: tspace)
      end

      added.each_entry do |tsid, tspace|
        connect(tsid)
        sync(tsid, tspace0: Term[], tspace1: tspace)
      end

      @state = tspaces
    end

    # Creates a connection to the termspace with the given *tsid*.
    #
    # The termspace is guaranteed to be ready to accept `sync`s after this method.
    #
    # May or may not block depending on the underlying map implementation.
    # See also: `StepContext.new`.
    private def connect(tsid : Term) : Nil
      if @setconns.has_key?(tsid)
        raise "BUG: attempt to double connect() to #{tsid}"
      end

      subscriber = TspaceRegistry::Subscriber.new do |config|
        unless config
          disconnect(tsid)
          next
        end

        setconn0 = @setconns[tsid]?
        conn = Tconn.new(config.map, config.chat, config.sink, config.fresh, config.keepalive)
        setconn1 = TsetConn.new(conn)

        # If there was a previous connection of some kind we'll have to migrate.
        if setconn0
          migrate(tsid, setconn0, setconn1)
        end

        @setconns[tsid] = setconn1
      end

      @unsubscribe[tsid] = @registry.subscribe(tsid, &subscriber)
      @registry.call(tsid, subscriber)
    end

    # Parses *tspace1* spec of *tsid* and syncs appropriately.
    private def sync(tsid : Term, tspace0 : Term::Dict, tspace1 : Term) : Nil
      Term.case(tspace1) do
        matchpi %[(¦ sensors⋮ {} appearances⋮ {})] do
          sync(tsid,
            sensors0: tspace0[:sensors]?.try(&.as_d) || Term[],
            sensors1: sensors.unsafe_as_d,
            appearances0: tspace0[:appearances]?.try(&.as_d) || Term[],
            appearances1: appearances.unsafe_as_d,
          )
        end

        otherwise do
          # Tspace format is invalid, disconnect.
          disconnect(tsid)
        end
      end
    end

    # Disconnects from the termspace with the given *tsid*.
    #
    # May or may not block depending on the underlying map implementation.
    # See also: `StepContext.new`.
    private def disconnect(tsid : Term) : Nil
      unless setconn = @setconns.delete(tsid)
        raise "BUG: attempt to disconnect() a connection that does not exist: #{tsid}"
      end

      unsubscribe = @unsubscribe[tsid]
      unsubscribe.call

      setconn.close
    end

    private def migrate(tsid : Term, setconn0 : TsetConn, setconn1 : TsetConn)
      setconn0.close

      sync(setconn1,
        sensors0: Term[],
        sensors1: @state[:sensors]?.try(&.as_d) || Term[],
        appearances0: Term[],
        appearances1: @state[:appearances]?.try(&.as_d) || Term[],
      )
    end

    private def sync(tsid : Term,
                     sensors0 : Term::Dict,
                     sensors1 : Term::Dict,
                     appearances0 : Term::Dict,
                     appearances1 : Term::Dict) : Nil
      unless setconn = @setconns[tsid]?
        raise "BUG: attempt to sync() a connection that does not exist: #{tsid}"
      end

      sync(setconn, sensors0, sensors1, appearances0, appearances1)
    end

    private def sync(setconn : TsetConn, sensors0, sensors1, appearances0, appearances1)
      sync_sensors(setconn, sensors0, sensors1)
      sync_appearances(setconn, appearances0, appearances1)
    end

    # Applices the changes made between two sensor states *sensors0* (before)
    # and *sensors1* (after).
    private def sync_sensors(setconn : TsetConn, sensors0 : Term::Dict, sensors1 : Term::Dict) : Nil
      added, removed = sensors1.diff1(sensors0)

      removed.each_entry do |pattern, _|
        setconn.delete_sensor_for(pattern)
      end

      added.each_entry do |pattern, _|
        # TODO: selector
        setconn.add_sensor_for(pattern)
      end
    end

    # Applices the changes made between two appearance states *appearances0*
    # (before) and *appearances1* (after).
    private def sync_appearances(setconn : TsetConn, appearances0 : Term::Dict, appearances1 : Term::Dict) : Nil
      added, removed = appearances1.diff1(appearances0)

      removed.each_entry do |value, _|
        setconn.delete_appearance_for(value)
      end

      added.each_entry do |value, _|
        # TODO: selector, tombstone
        setconn.add_appearance_for(value)
      end
    end

    def each_prompt(tsid : Term, overview : Tconn::Overview, & : Term ->)
      return unless setconn = @setconns[tsid]?

      overview.each do |identity, view|
        next unless pattern = setconn.pattern?(identity)

        yield Term.of(:event, {:stimuli, tsid, pattern, view.dict_multiset})
      end
    end
  end

  def step(me : StepContext) : D7::Step
    D7::Step.new do |document|
      me.step(document)

      {document, false}
    end
  end
end
