module Ww
  # Registry serves as a stable description of an ever-changing world. It keeps
  # track of which surfaces are *realized*, meaning all of their parts are present
  # and properly connected within the world. Until a surface is reflected in
  # the registry it is considered absent, even if all of its constituents are
  # present in other data structures.
  #
  # Data structures such as `Upolytrie`, `Ttrie`, etc. are updated in parallel,
  # and therefore are always unstable, and changes in them are partial from
  # a global perspective. Only clients behind the modifications (e.g. threads)
  # are aware when their modifications are completed. Once that happens clients
  # reflect their changes in the registry, in bulk, atomically; in essence, saying
  # "it exists" to all other clients and themselves.
  class Registry
    # :nodoc:
    EMPTY = new(
      sensors: Pf::Set(Sensor).new,
      appearances: Pf::Map(Appearance, {conn: Tconn::Id, nstrands: UInt32}).new,
      sensorsubs: Pf::Map(Xvertex, Pf::Set(Sensor)).new,
    )

    protected def initialize(
      @sensors : Pf::Set(Sensor),
      @appearances : Pf::Map(Appearance, {conn: Tconn::Id, nstrands: UInt32}),
      @sensorsubs : Pf::Map(Xvertex, Pf::Set(Sensor)),
    )
    end

    # Constructs an empty sensor and appearance registry.
    def self.[] : Registry
      EMPTY
    end

    private def_change

    # Returns `true` if *id* is used by a sensor or an appearance according to
    # this registry. Returns `false` otherwise.
    def taken?(id : Surface::Id) : Bool
      sensor?(id) || appearance?(id)
    end

    # Returns `true` if *id* is used by a sensor according to this registry.
    # Returns `false` otherwise.
    def sensor?(id : Surface::Id) : Bool
      @sensors.includes?(Sensor.new(id))
    end

    # Returns `true` if *id* is used by an appearance according to this registry.
    # Returns `false` otherwise.
    def appearance?(id : Surface::Id) : Bool
      @appearances.includes?(Appearance.new(id))
    end

    # Returns `true` if *appearance* is owned by connection *conn* and consists of
    # the given number of strands *nstrands*. Returns `false` otherwise.
    #
    # Serves as a "knowledge check" to allow or deny access to an appearance; if
    # you know all of *appearance*, *conn*, and *nstrands*, then you own
    # the appearance.
    def appearance?(appearance : Appearance, conn : Tconn::Id, nstrands : UInt32) : Bool
      @appearances[appearance]? == {conn: conn, nstrands: nstrands}
    end

    # Returns `true` if the given *sensor* corresponds to the Xvertex *vertex*
    # according to this registry. Returns `false` otherwise.
    def subscribed?(sensor : Sensor, vertex : Xvertex) : Bool
      return false unless subs = @sensorsubs[vertex]?

      sensor.in?(subs)
    end

    # Yields sensors corresponding to the given Xvertex *vertex* according to
    # this registry.
    def each_subscribed_sensor(vertex : Xvertex, & : Sensor ->) : Nil
      return unless subs = @sensorsubs[vertex]?

      subs.each { |sub| yield sub }
    end

    # Makes the appropriate changes to this registry according to *announcements*
    # in bulk. Returns the modified registry.
    def reflect(announcements : Array(Announcement)) : Registry
      sensors1 = @sensors.transaction do |commit|
        announcements.each do |ann|
          case ann
          when Announcement::SensorAdded
            commit.add(ann.surface)
          when Announcement::SensorDeleted
            commit.delete(ann.surface)
          end
        end
      end

      sensorsubs1 = @sensorsubs.transaction do |commit|
        announcements.each do |ann|
          case ann
          when Announcement::SensorAdded
            commit.assoc(ann.vertex, (commit[ann.vertex]? || Pf::Set(Sensor).new).add(ann.surface))
          when Announcement::SensorDeleted
            subs0 = commit[ann.vertex]
            subs1 = subs0.delete(ann.surface)
            if subs1.empty?
              commit.dissoc(ann.vertex)
            else
              commit.assoc(ann.vertex, subs1)
            end
          end
        end
      end

      appearances1 = @appearances.transaction do |commit|
        announcements.each do |ann|
          case ann
          when Announcement::AppearanceAdded
            commit.assoc(ann.surface, {conn: ann.conn, nstrands: ann.nstrands})
          when Announcement::AppearanceDeleted
            commit.dissoc(ann.surface)
          end
        end
      end

      change(sensors: sensors1, appearances: appearances1, sensorsubs: sensorsubs1)
    end
  end
end
