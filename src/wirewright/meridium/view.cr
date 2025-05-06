module Ww::Meridium
  # We distinguish *stimuli* and *percepts* in the following way.
  #
  # A *stimulus* is something *produced* and *extrinsic* from one's point of view.
  # Stimuli are produced by appearances. It is stimuli that travel over
  # the network, for example.
  #
  # A *percept*, on the other hand, is something that is the result of *perception* --
  # namely, by a sensor. Percepts are of "local origin and use" -- they are *intrinsic*;
  # percepts never escape to the network, unless converted to stimuli through an appearance.
  #
  # In Wirewright, *perception* is simply pattern matching -- breaking up a *stimulus*
  # into a list of *features* based on the sensor's pattern; each *feature* being an
  # M1 match env.
  record Percept, instant : Instant, stimulus : Term, features : Array(Term::Dict)

  # An internal object that maintains the percepts that a sensor perceives.
  #
  # See also: `Percept`.
  struct PerceptData
    # :nodoc:
    def initialize(@percepts = Pf::Map(WWID, Percept).new)
    end

    private def_change

    # Returns `true` if this object currently registers no percepts.
    def absent? : Bool
      @percepts.empty?
    end

    # :nodoc:
    def after(surface : Sensor, act : StimulusPresence) : {PerceptData, Bool}
      if percept = @percepts[act.appearance.wwid]?
        # Make sure WE are outdated, not act.
        unless percept.instant < act.appearance.instant
          Log.trace { "reject stimulus presence: instant #{percept.instant} (my) !< #{act.appearance.instant} (its)" }
          return self, false
        end

        features0 = percept.features
      end

      features1 = M1.matches(surface.pattern, act.stimulus)

      # If there is a pattern mismatch, and we've previously known the appearance,
      # this means the appearance changed its value to something we don't like
      # anymore. In that case we remove the appearance.
      if features1.empty?
        return change(percepts: @percepts.dissoc(act.appearance.wwid)), true
      end

      percept = Percept.new(act.appearance.instant, act.stimulus, features1)

      {change(percepts: @percepts.assoc(act.appearance.wwid, percept)), features0 != features1}
    end

    # :nodoc:
    def after(surface : Sensor, act : StimulusAbsence) : {PerceptData, Bool}
      unless percept = @percepts[act.appearance.wwid]?
        return self, false
      end

      # Make sure WE are outdated, not act.
      unless percept.instant <= act.appearance.instant
        Log.trace { "reject percept absence: instant #{percept.instant} (my) !< #{act.appearance.instant} (its)" }
        return self, false
      end

      {change(percepts: @percepts.dissoc(act.appearance.wwid)), true}
    end

    {% if flag?(:docs) %}
      # Registers the activation *act* targeted at the given sensor *surface*.
      # Returns the resulting copy of this object, followed by a boolean indicating
      # whether some percept changed due to *act*.
      def after(surface : Sensor, act : StimulusPresence | StimulusAbsence) : {PerceptData, Bool}
      end
    {% end %}

    # Updates this object according to an exhaustive set of *appearances*
    # perceived by the given sensor *surface*; in other words, removes all
    # perceived appearances **not** in the *appearances* set.
    def presence(surface : Sensor, appearances : Set(WWID)) : {PerceptData, Bool}
      percepts1 = @percepts.select { |id, _| id.in?(appearances) }

      {change(percepts: percepts1), @percepts != percepts1}
    end

    # Formats this object as a dict multiset of percept features.
    #
    # A *feature* is simply "local dialect" for M1 match env.
    #
    # See also: `Percept`.
    def dict_multiset : Term::Dict
      Term::Dict.build do |commit|
        @percepts.each do |_, percept|
          percept.features.each do |feature|
            commit.with(feature, (commit[feature]? || 0) + 1)
          end
        end
      end
    end
  end

  # An internal object that maintains the percept data for all of `Node`'s sensors.
  class View
    alias Version = UInt32

    # A monotonically increasing, per-view integer that can be used to detect view
    # changes easily.
    getter version : Version

    def initialize(@map = Pf::Map(Slot, PerceptData).new, @version = Version.new(0))
    end

    private def_change

    # Yields each sensor slot and percept data object to the block.
    def each(& : Slot, PerceptData ->) : Nil
      @map.each { |slot, data| yield slot, data }
    end

    # Registers a sensor *surface* at *slot*. Returns the resulting copy of
    # this view.
    #
    # This is necessary to begin perceiving stimuli.
    def register(slot : Slot, surface : Sensor) : View
      map1 = @map.assoc(slot, PerceptData.new)
      map1.same?(@map) ? self : change(map: map1, version: @version + 1)
    end

    # Unregisters the sensor *surface* at *slot*. Returns the resulting copy of
    # this view.
    #
    # This is necessary to stop perceiving stimuli.
    def unregister(slot : Slot, surface : Sensor) : View
      map1 = @map.dissoc(slot)
      map1.same?(@map) ? self : change(map: map1, version: @version + 1)
    end

    # Updates the percept data of *surface* at *slot* based on an exhaustive
    # set of *appearances* it perceives at the moment. Returns the resulting
    # copy of this view.
    def presence(slot : Slot, surface : Sensor, appearances : Set(WWID)) : View
      unless stimuli0 = @map[slot]?
        return self
      end

      stimuli1, changed = stimuli0.presence(surface, appearances)

      change(map: @map.assoc(slot, stimuli1), version: changed ? @version + 1 : @version)
    end

    # Registers the activation *act* targeted at the given sensor *surface*.
    # Returns the resulting copy of this view,
    def after(surface : Sensor, act : StimulusPresence | StimulusAbsence) : View
      slot = act.sensor.slot

      unless stimuli0 = @map[slot]?
        Log.trace { "stimuli absent for sensor #{slot}" }
        return self
      end

      stimuli1, changed = stimuli0.after(surface, act)

      change(map: @map.assoc(slot, stimuli1), version: changed ? @version + 1 : @version)
    end

    # Clears all percept data objects. Returns the modified copy of this view.
    def clear : View
      map1 = @map.map_value { PerceptData.new }
      map1.same?(@map) ? self : change(map: map1, version: @version + 1)
    end

    # Formats this view as a dict, mapping slots to multisets of percept features
    # perceived by the sensor at that slot.
    def dict_multisets : Term::Dict
      Term::Dict.build do |commit|
        each do |slot, stimuli|
          next if stimuli.absent?

          commit.with(slot, stimuli.dict_multiset)
        end
      end
    end
  end
end
