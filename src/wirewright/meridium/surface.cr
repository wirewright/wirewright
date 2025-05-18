module Ww::Meridium
  # "Surface" is a group name used to refer to both sensors and appearances.
  module Surface
    # :nodoc:
    struct AtomSink
      include IAtomAppend

      def initialize(@sink : Atom ->)
      end

      def <<(atom : Atom) : self
        @sink.call(atom)

        self
      end
    end

    abstract def atoms_to(id : WWID, target : IAtomAppend) : Nil

    # Calls *sink* with atoms that constitute `self`. Atoms may repeat deliberately.
    # This can be used for e.g. refcounting (instead of using a set, you can use
    # a multiset and have much less disruptive removals later on).
    #
    # WARNING: *sink* must be thread-safe -- it will be called from multiple fibers.
    # If you don't want to be bothered with thread-safety, use `atom_set` which
    # returns a set of complements and does the thread-safety stuff for you.
    def each_atom(id : WWID, &sink : Atom ->) : Nil
      atoms_to(id, target: AtomSink.new(sink))
    end

    # Appends the atoms that constitute `self` to *target*.
    #
    # *target* must respond to `<<`.
    #
    # WARNING: *target*'s `<<` method must be thread-safe! It will be called
    # from multiple fibers.
    def atoms_to(id : WWID, target, **kwargs) : Nil
      each_atom(id, **kwargs) do |atom|
        target << atom
      end
    end

    # Returns the set of atoms that constitute `self`.
    #
    # See `each_atom` for *args* and *kwargs*.
    def atom_set(*args, **kwargs) : Set(Atom)
      atoms = Set(Atom).new
      lock = Sync::Mutex.new

      each_atom(*args, **kwargs) do |atom|
        lock.synchronize { atoms << atom }
      end

      atoms
    end

    # Calls *sink* with WWIDs of `self`'s *complements* in the termspace. WWIDs may
    # repeat without intent (so you can ignore repetitions safely).
    #
    # - For a sensor, its *complements* are appearances that the sensor is excited by.
    # - For an appearance, its *complements* are sensors that the appearance excites.
    #
    # WARNING: *sink* must be thread-safe -- it will be called from multiple fibers.
    # If you don't want to be bothered with thread-safety, use `complement_set` which
    # returns a set of complements and does the thread-safety stuff for you.
    abstract def each_complement(atoms : IAtomsPresent, &sink : WWID ->) : Nil

    # Returns the set of complements for `self` in the termspace.
    #
    # See `each_atom` for *args*, *kwargs*, and details.
    def complement_set(*args, **kwargs) : Set(WWID)
      complements = Set(WWID).new
      lock = Sync::Mutex.new

      each_complement(*args, **kwargs) do |complement|
        lock.synchronize { complements << complement }
      end

      complements
    end
  end

  # Represents a sensor surface.
  #
  # Sensor surfaces serve as passive observers of appearances in the termspace.
  # They provide a holistic view of all appearances that excite them at a
  # given moment.
  class Sensor
    include Surface

    # Returns the M1 pattern term of this sensor.
    getter pattern : Term

    # Returns the secret term of this sensor.
    getter? secret : Term?

    # Returns the relook period for this sensor.
    getter? relook : Time::Span?

    protected def initialize(
      @pattern : Term,
      @secret : Term?,
      @relook : Time::Span?,
      @branches : BranchList,
    )
      if @branches.empty?
        raise ArgumentError.new("branches list must contain at least one branch")
      end
    end

    private SK_ANY  = Term.of({:"%any"})
    private SK_SYM  = Term.of({:"%symbol"})
    private SK_STR  = Term.of({:"%string"})
    private SK_NUM  = Term.of({:"%number", :_})
    private SK_DICT = Term.of({:"%dict"})
    private SK_BOOL = Term.of({:"%boolean"})

    # Converts skeleton strand *bases* to a `Strand`.
    private def self.strand(bases : Term::Dict) : Strand
      state = :start
      strand = Strand{Ubase::Begin.new}

      bases.items.each do |base|
        case state
        when :start
          unless base == SK_ANY
            raise "BUG: unexpected base #{base}, expected (%any)"
          end
          state = :typecheck
        when :typecheck
          case base
          when SK_DICT
            strand << Ubase::IsDict.new
            state = :key
          when SK_SYM
            strand << Ubase::IsSym.new
            state = :literal
          when SK_NUM
            strand << Ubase::IsNum.new
            state = :literal
          when SK_STR
            strand << Ubase::IsStr.new
            state = :literal
          when SK_BOOL
            strand << Ubase::IsBool.new
            state = :literal
          else
            raise "BUG: unexpected base #{base}, expected typecheck"
          end
        when :key
          Term.case(base) do
            matchpi %{(%'%literal ())} do # empty dict
              strand << Ubase::Literal.new(Term.of)
              state = :after_literal
            end

            matchpi %{(%'%value (%'%literal term_))} do
              strand << Ubase::At.new(term)
              state = :typecheck
            end

            otherwise do
              raise "BUG: unexpected base #{base}, expected %value %literal"
            end
          end
        when :literal
          Term.case(base) do
            matchpi %{(%'%literal term_)} do
              strand << Ubase::Literal.new(term)
              state = :after_literal
            end

            otherwise do
              raise "BUG: unexpected base #{base}, expected %literal"
            end
          end
        when :after_literal
          raise "BUG: expected end-of-strand after literal, but found base #{base}"
        end
      end

      # Indicate abrupt end (as in e.g. `Begin - *` or `Begin - IsDict - At(0) - *`) by
      # an explicit End base. If we had literal we treat it as end-of-strand regardless.
      unless state == :after_literal
        strand << Ubase::End.new
      end

      strand
    end

    # Breaks *branch* down into its constituent strands and so on.
    private def self.strand_list(branch : Term) : StrandList
      strands = StrandList.new
      M1.strands(branch) do |bases|
        strands << strand(bases)
      end
      strands
    end

    # Breaks *skeleton* down into its constituent branches and so on.
    private def self.branch_list(skeleton : Term) : BranchList
      branches = BranchList.new
      M1.branches(skeleton) do |branch|
        branches << strand_list(branch)
      end
      branches
    end

    # Returns the skeleton of *pattern*.
    #
    # See also: `M1.skeleton`.
    private def self.skeleton(pattern : Term) : Term
      pipe(pattern, M1.normal, M1.skeleton)
    end

    # Constructs a sensor surface.
    #
    # - *pattern* is the M1 pattern matching appearances that the sensor should
    #   be excited by.
    # - *secret* acts like a "password" or "scope" to appearances; for the sensor
    #   to see an appearance, both must have the same secret.
    # - *relook* specifies the relook period for the sensor. "Relook" is an update
    #   of the sensor's view originating from that sensor itself. Normally sensors
    #   are notified about stimuli by appearances. But when the owner of an appearance
    #   e.g. crashes, there is nobody to notify the sensor about that. Thus the sensor
    #   periodically refreshes -- "relook"s -- itself to clean up appearances that
    #   exited without notice.
    def self.new(pattern : Term, secret : Term? = nil, relook : Time::Span? = nil) : Sensor
      branches = pipe(pattern, skeleton, branch_list)

      new(pattern, secret, relook, branches)
    end

    def atoms_to(id : WWID, target : IAtomAppend) : Nil
      if @branches.size == 1
        strands = @branches[0]
        endpoints = Utrie.mount(target, strands)
        apexes = {Xgraph.mount(target, endpoints)}
      else
        wg = WaitGroup.new(@branches.size)

        apexes = [] of Atom
        lock = Sync::Mutex.new

        @branches.each do |strands|
          spawn do
            endpoints = Utrie.mount(target, strands)
            apex = Xgraph.mount(target, endpoints)

            lock.synchronize { apexes << apex }
          ensure
            wg.done
          end
        end

        wg.wait
      end

      SensorRegistry.register(target, @secret, apexes, id)
    end

    def each_complement(atoms : IAtomsPresent, &sink : WWID ->) : Nil
      if @branches.size == 1
        strands = @branches[0]
        AppearanceRegistry.each_appearance(atoms, @secret, strands, &sink)
        return
      end

      wg = WaitGroup.new(@branches.size)

      @branches.each do |strands|
        spawn do
          AppearanceRegistry.each_appearance(atoms, @secret, strands, &sink)
        ensure
          wg.done
        end
      end

      wg.wait
    end

    def inspect(io)
      io << "Sensor(pattern="
      ML.compact(io, @pattern)
      io << ", secret="
      if secret = @secret
        ML.compact(io, secret)
      else
        io << "nil"
      end
      io << ", relook="
      @relook.inspect(io)
      io << ", branches="
      @branches.inspect(io)
      io << ")"
    end

    def to_s(io)
      inspect(io)
    end

    def_equals_and_hash @pattern, @secret, @relook
  end

  # Represents an appearance surface.
  #
  # Appearance surfaces serve as stimuli/excitation sources for sensors.
  class Appearance
    include Surface

    # Returns the value term of this appearance.
    getter value : Term

    # Returns the secret term of this appearance.
    getter? secret : Term?

    # Constructs an appearance surface.
    #
    # - *value* is the value of this appearance.
    # - *secret* acts like a "password" or "scope" protecting this appearance;
    #   for a sensor to see this appearance, both must have the same secret.
    def initialize(@value : Term, @secret : Term? = nil)
    end

    def atoms_to(id : WWID, target : IAtomAppend) : Nil
      AppearanceRegistry.mount(target, @secret, @value, id)
    end

    def each_complement(atoms : IAtomsPresent, &sink : WWID ->) : Nil
      hits = Utrie.endpoints(atoms, @value)
      conjvs = Xgraph.conjvs(atoms, hits)

      SensorRegistry.each_sensor(atoms, @secret, conjvs, &sink)
    end

    def inspect(io)
      io << "Appearance(value="
      ML.compact(io, @value)
      io << ", secret="
      if secret = @secret
        ML.compact(io, secret)
      else
        io << "nil"
      end
      io << ")"
    end

    def to_s(io)
      inspect(io)
    end

    def_equals_and_hash @value, @secret
  end
end
