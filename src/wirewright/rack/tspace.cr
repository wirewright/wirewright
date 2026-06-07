# Implements termspaces, which are a way to do global, content-based communication.
#
# Termspaces are complementary to the usual way of communication in Rack: edges.
# Edges are circuit-local; edge proximity is not semantic proximity. In a sense,
# edge proximity is "proximity by construction", just like cells in a grid are
# proximal to each other "by construction"; whereas in Termspaces, entities are
# proximal by stimulus (that is, one generates a stimulus for another, thus forming
# an ephemeral "edge" or "hyperedge" if multiple entities respond to the stimulus).
# In other words, adjacency in termspaces is defined by content; whereas with edges,
# it is defined ahead-of-time, by design.
#
# Termspaces consist of *sensors* and *appearances*. Sensors see terms,
# appearances show terms.
#
# *Channel sensors* are remotely similar to channels in Go or Crystal: they wait
# for an appearance to show a matching value, and "snatch" it. If multiple
# matching appearances exist, a channel sensor gets confused, and does nothing,
# unless all the appearances have the same value (unanimous agreement); if they do,
# the sensor empties all of them.
#
# *View sensors* are pure observers: they provide a view into the termspace, showing
# all matching values currently present in the termspace. The view of view sensors
# is, conceptually, a multiset.
module Ww::Rack::Tspace
  extend self

  alias Sensor = ChanSensor | ViewSensor

  defrecord ChanSensor, id : D7::NodeId, tspace : Term, pattern : Term, template : Term
  defrecord ViewSensor, id : D7::NodeId, tspace : Term, pattern : Term, template : Term

  defrecord Appearance, id : D7::NodeId, tspace : Term, matchee : Term

  # Returns the termspace pass.
  def pass(clf : D7::Classifier) : D7::Pass
    D7::Pass.new { |circuit| step(clf, circuit) }
  end

  private def step(clf : D7::Classifier, circuit : Term) : Slice(Term)
    sensors = [] of Sensor
    appearances = [] of Appearance

    # Find candidates for an exchange.
    _ = D7.case(clf, circuit, decorator: Prepass) do
      rule(<<-WWML) do |dev|
      [sensor (tspace_ pattern_ @dst_) template_] dev
        -> (one dst) [cell @dst_] {name: dst}
      WWML
        tspace, pattern, template = D7.fetch(dev, :tspace, :pattern, :template)
        sensors << ChanSensor.new(D7.id(dev), tspace, pattern, template)

        nil # No change
      end

      rule(<<-WWML) do |dev|
      [sensor* (tspace_ pattern_ @dst_) template_] dev
        -> (one dst) [cell @dst_ _?] {name: dst}
      WWML
        tspace, pattern, template = D7.fetch(dev, :tspace, :pattern, :template)
        sensors << ViewSensor.new(D7.id(dev), tspace, pattern, template)

        nil # No change
      end

      rule(<<-WWML) do |dev, src|
      [appearance tspace_ @src_] dev
        -> (one src) [cell @src_ matchee_] {name: src}
      WWML
        tspace, matchee = D7.fetch(dev, :tspace), D7.fetch(src, :matchee)
        appearances << Appearance.new(D7.id(dev), tspace, matchee)

        nil # No change
      end
    end

    if sensors.empty? && appearances.empty?
      return Slice[circuit]
    end

    # Perform the exchange.
    stimuli = Hash(D7::NodeId, Array(Term::Rep)).new(initial_capacity: sensors.size)
    consumed = Pf::USet32[]

    sensors.each do |sensor|
      expansions = nil

      case sensor
      in ChanSensor
        counterparts = [] of {Term::Rep, Appearance}

        appearances.each_with_index do |appearance|
          next unless sensor.tspace == appearance.tspace
          next unless M1.probably_matches?(sensor.pattern, appearance.matchee)
          next unless env = M1.match?(sensor.pattern, appearance.matchee)

          expansion = Alloy2.render_rep(sensor.template, locals: env)
          unless counterparts.empty? || counterparts.last[0] == expansion
            # Multiple different values competing. Sensor chan is confused about
            # which one to pick, so it picks no one.
            counterparts.clear
            break
          end

          counterparts << {expansion, appearance}
        end

        if counterparts.present?
          expansion = counterparts.first[0]
          expansions = [expansion]

          counterparts.each do |(_, appearance)|
            consumed = consumed.add(appearance.id)
          end
        end
      in ViewSensor
        appearances.each do |appearance|
          next unless sensor.tspace == appearance.tspace
          next unless M1.probably_matches?(sensor.pattern, appearance.matchee)
          next unless env = M1.match?(sensor.pattern, appearance.matchee)

          expansion = Alloy2.render_rep(sensor.template, locals: env)
          expansions ||= [] of Term::Rep
          expansions << expansion
        end
      end

      next unless expansions

      stimuli[sensor.id] = expansions
    end

    D7.case(clf, circuit, decorator: Prepass) do
      rule(<<-WWML) do |dev, dst|
      [sensor (_ _ @dst_) _] dev
        -> (one dst) [cell @dst_] {name: dst}
      WWML
        next unless expansions = stimuli[D7.id(dev)]?
        assert expansions.size == 1

        expansion = expansions.first
        unless expansion.empty?
          instance = Term.collapse(expansion)
        end

        # instance : Term?

        D7.patch(dst, {2, instance})
      end

      rule(<<-WWML) do |dev, dst|
      [sensor* (_ _ @dst_) _] dev
        -> (one dst) [cell @dst_ _?] {name: dst}
      WWML
        unless expansions = stimuli[D7.id(dev)]?
          next D7.patch(dst, {2, Term[]})
        end

        instances = [] of Term

        expansions.each do |expansion|
          instances.concat(expansion)
        end

        instances.sort! { |a, b| Term.compare(a, b) }

        D7.patch(dst, {2, instances})
      end

      rule(<<-WWML) do |dev, src|
      [appearance tspace_ @src_] dev
        -> (one src) [cell @src_ _] {name: src}
      WWML
        next unless D7.id(dev).in?(consumed)

        D7.patch(src, {2, nil})
      end
    end
  end
end
