module Ww::Meridium
  # Tbase (short for *termbase*, whatever that is supposed to mean...) is
  # an internal object responsible for orchestrating objects that are even
  # more internal (such as `Utrie`, `Xgraph`, `Ttrie`, and so on).
  #
  # This "orchestration" results in the emergence of *sensors* and *appearances*,
  # conceptually grouped into *surfaces*; but througout the operation of Tbase
  # referred to as *subjects* as well. With the help of Tbase, you can talk about
  # them without having to remember they're just a ton of key-value pairs.
  #
  # It's like atoms and chairs -- Tbase creates the illusion of "chairs" while
  # they are just atoms; and there's something else even more internal (e.g. `Xgraph`)
  # that creates a similar illusion of atoms for `Tbase` itself, while what
  # they really are is collections of quarks and electrons etc., and so on.
  #
  # The notable thing about `Tbase` is that it only needs a single hash map (or,
  # rather, an implementor of `IMap`). It is designed this way to defer synchronicity,
  # distrubition, and connectivity to the map implementation; and therefore make
  # their presence irrelevant to the algorithm & user-configurable. Tbase simply
  # does not care. As long as the map is thread-safe, Tbase is thread-safe; as long
  # as the map is distributed, Tbase is distributed, and so on.
  struct Tbase
    alias Key = Utrie::Key | Xgraph::Key | Ttrie::Key | Etrace::Key | StrandSet::Key | AppearanceSet::Key | SensorMultimap::Key
    alias Value = Utrie::Value | Xgraph::Value | Ttrie::Value | Etrace::Value | StrandSet::Value | AppearanceSet::Value | SensorMultimap::Value

    # Data that `Tbase` needs to know about a sensor.
    record Sensor, id : Label, strands : StrandList do
      # Calls *fn* with each sensor in *pattern*.
      #
      # An arbitrary M1 *pattern* can contain branches (e.g. `%any`) so it is considered
      # to contain multiple sensors.
      def self.each(fresh : LabelGenerator, pattern : Term, &fn : Sensor ->) : Nil
        skeleton = pipe(pattern, M1.normal, M1.skeleton)

        strands = [] of Strand

        M1.branches(skeleton) do |branch|
          M1.strands(branch) do |strand|
            strands << strand.items.to_readonly_slice { |base| Term.decode(Ubase::Any, base) }
          end

          sensor = new(fresh.call, strands.to_readonly_slice(&.itself))

          fn.call(sensor)

          strands.clear
        end
      end
    end

    # Data that `Tbase` needs to know about an appearance.
    record Appearance, id : Label, value : Term

    alias Subject = Sensor | Appearance

    def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
    end

    # Constructs a `Utrie` view of this termbase's underlying map.
    def utrie : Utrie
      Utrie.new(@fresh, @map.submap(Utrie::Key, Utrie::Value))
    end

    # Constructs a `Ttrie` view of this termbase's underlying map.
    def ttrie : Ttrie
      Ttrie.new(@fresh, @map.submap(Ttrie::Key, Ttrie::Value))
    end

    # Constructs an `Etrace` view of this termbase's underlying map.
    def etrace : Etrace
      Etrace.new(@fresh, @map.submap(Etrace::Key, Etrace::Value))
    end

    # Constructs a `StrandSet` view of this termbase's underlying map.
    def strands : StrandSet
      StrandSet.new(@map.submap(StrandSet::Key, StrandSet::Value))
    end

    # Constructs an `Xgraph` view of this termbase's underlying map.
    def xgraph : Xgraph
      Xgraph.new(@fresh, @map.submap(Xgraph::Key, Xgraph::Value))
    end

    # Constructs a `SensorMultimap` view of this termbase's underlying map.
    def sensors : SensorMultimap
      SensorMultimap.new(@fresh, @map.submap(SensorMultimap::Key, SensorMultimap::Value))
    end

    # Constructs an `AppearanceSet` view of this termbase's underlying map.
    def appearances : AppearanceSet
      AppearanceSet.new(@map.submap(AppearanceSet::Key, AppearanceSet::Value))
    end

    # Inserts a sensor *subject* into this termbase. Returns *deps*.
    #
    # *deps* acts as a sink for all key-value pairs inserted by this method into
    # the underlying map. It must respond to `<<`. Its main purpose is to allow you
    # to unmount *subject* later on; to unmount *subject* you will simply have to
    # delete all key-value pairs contained in *deps* from the underlying map
    # (see `IMap#dec`).
    def mount(subject : Sensor, *, deps : D = Bag({Key, Value}).new) : D forall D
      rule = Deque(Label).new

      subject.strands.each do |strand|
        _, endpoint = utrie.mount(subject.id, strand, deps: deps)
        _ = strands.mount(subject.id, endpoint, deps: deps)

        rule << endpoint
      end

      rule.unstable_sort!

      _, conjv = xgraph.mount(subject.id, rule, deps: deps)
      _ = sensors.mount(subject.id, conjv, subject.id, deps: deps)

      deps
    end

    # Inserts an appearance *subject* into this termbase. Returns *deps*.
    #
    # *deps* acts as a sink for all key-value pairs inserted by this method into
    # the underlying map. It must respond to `<<`. Its main purpose is to allow you
    # to unmount *subject* later on; to unmount *subject* you will simply have to
    # delete all key-value pairs contained in *deps* from the underlying map
    # (see `IMap#dec`).
    def mount(subject : Appearance, *, deps : D = Bag({Key, Value}).new) : D forall D
      Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
        keypath.push(leaf)

        _, path = ttrie.mount(subject.id, keypath, subject.id, deps: deps)
        _ = etrace.mount(subject.id, path, deps: deps)

        keypath.pop

        true # continue
      end

      _ = appearances.mount(subject.id, subject.id, deps: deps)

      deps
    end

    # Calls *sink* with each appearance complement of *subject*.
    #
    # For a sensor, its appearance complements are appearances that the sensor
    # is excited by.
    def each_complement(subject : Sensor, &sink : Label ->) : Nil
      sets = [] of Set(Label)

      subject.strands.each do |strand|
        return unless endpoint = ttrie.query?(subject.id, strand)

        hits = Set(Label).new

        etrace.walk(subject.id, endpoint) do |candidate|
          next unless appearances.appearance?(subject.id, candidate)

          hits << candidate
        end

        return if hits.empty?

        sets << hits
      end

      return unless subject.strands.size == sets.size

      sets.unstable_sort_by!(&.size)
      sets[0].each do |candidate|
        # Make sure the candidate is in all sets (matches all strands of the sensor).
        next unless (1...sets.size).all? { |index| candidate.in?(sets[index]) }

        sink.call(candidate)
      end
    end

    # Calls *sink* with each sensor complement of *subject*.
    #
    # For an appearance, its sensor complements are sensors that the appearance excites.
    def each_complement(subject : Appearance, &sink : Label ->) : Nil
      hits = Deque(Label).new

      utrie.query(subject.id, subject.value) do |hit|
        next unless strands.strand?(subject.id, hit)

        hits << hit
      end

      hits.unstable_sort!

      xgraph.conjs(subject.id, hits) do |conjv|
        sensors.decode(subject.id, conjv, &sink)
      end
    end
  end
end
