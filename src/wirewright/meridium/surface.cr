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

    # Calls *sink* with atoms that constitute `self`. Atoms may repeat deliberately.
    # This can be used for e.g. refcounting (instead of using a set, you can use
    # a multiset and have much less disruptive removals later on).
    #
    # *mt* specifies whether to run under a multi-threaded or single-threaded
    # fiber execution context.
    #
    # WARNING: *sink* must be thread-safe -- it will be called from multiple fibers.
    # If you don't want to be bothered with thread-safety, use `atom_set` which
    # returns a set of atoms and does the thread-safety stuff for you.
    abstract def each_atom(instant : WWID, *, mt : Bool, &sink : Atom ->) : Nil

    # Appends the atoms that constitute `self` to *object*.
    #
    # *object* must respond to `<<`.
    #
    # WARNING: *object*'s `<<` method must be thread-safe! It will be called
    # from multiple fibers.
    def atoms_to(instant : WWID, object, **kwargs) : Nil
      each_atom(instant, **kwargs) do |atom|
        object << atom
      end
    end

    # Returns the set of atoms that constitute `self`.
    #
    # See `each_atom` for *args* and *kwargs*.
    def atom_set(*args, **kwargs) : Set(Atom)
      atoms = Set(Atom).new
      lock = Mutex.new

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
    # *mt* specifies whether to run under a multi-threaded or single-threaded
    # fiber execution context.
    #
    # WARNING: *sink* must be thread-safe -- it will be called from multiple fibers.
    # If you don't want to be bothered with thread-safety, use `complement_set` which
    # returns a set of complements and does the thread-safety stuff for you.
    abstract def each_complement(atoms : IAtomsPresent, *, mt : Bool, &sink : WWID ->) : Nil

    # Returns the set of complements for `self` in the termspace.
    #
    # See `each_atom` for *args*, *kwargs*, and details.
    def complement_set(*args, **kwargs) : Set(WWID)
      complements = Set(WWID).new
      lock = Mutex.new

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

    protected def initialize(@pattern : Term, @secret : Term?, @branches : BranchList)
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
      strand = Strand.new
      state = :start

      bases.items.each do |base|
        case state
        when :start
          unless base == SK_ANY
            raise "BUG: unexpected base #{base}, expected (%any)"
          end
          strand << Ubase::Begin.new
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
    def self.new(pattern : Term, secret : Term? = nil) : Sensor
      branches = pipe(pattern, skeleton, branch_list)

      new(pattern, secret, branches)
    end

    def each_atom(instant : WWID, *, mt : Bool, &sink : Atom ->) : Nil
      atoms = AtomSink.new(sink)

      if @branches.size == 1
        strands = @branches[0]
        endpoints = Utrie.mount(atoms, strands, mt: mt)
        apexes = {Xgraph.mount(atoms, endpoints)}
      else
        wg = WaitGroup.new(@branches.size)
        ctx = mt ? MT : ST

        apexes = [] of Atom
        lock = Mutex.new

        @branches.each do |strands|
          ctx.spawn do
            endpoints = Utrie.mount(atoms, strands, mt: mt)
            apex = Xgraph.mount(atoms, endpoints)

            lock.synchronize { apexes << apex }
          ensure
            wg.done
          end
        end

        wg.wait
      end

      SensorRegistry.register(atoms, @secret, apexes, instant, mt: mt)
    end

    def each_complement(atoms : IAtomsPresent, *, mt : Bool, &sink : WWID ->) : Nil
      if @branches.size == 1
        strands = @branches[0]
        AppearanceRegistry.each_appearance(atoms, @secret, strands, mt: mt, &sink)
        return
      end

      wg = WaitGroup.new(@branches.size)
      ctx = mt ? MT : ST

      @branches.each do |strands|
        ctx.spawn do
          AppearanceRegistry.each_appearance(atoms, @secret, strands, mt: mt, &sink)
        ensure
          wg.done
        end
      end

      wg.wait
    end
  end

  # Represents an appearance surface.
  #
  # Appearance surfaces serve as stimuli/excitation sources for sensors.
  class Appearance
    include Surface

    # Constructs an appearance surface.
    #
    # - *value* is the value of this appearance.
    # - *secret* acts like a "password" or "scope" protecting this appearance;
    #   for a sensor to see this appearance, both must have the same secret.
    def initialize(@value : Term, @secret : Term? = nil)
    end

    def each_atom(instant : WWID, *, mt : Bool, &sink : Atom ->) : Nil
      AppearanceRegistry.mount(AtomSink.new(sink), @secret, @value, instant, mt: mt)
    end

    def each_complement(atoms : IAtomsPresent, *, mt : Bool, &sink : WWID ->) : Nil
      hits = Utrie.endpoints(atoms, @value)
      conjvs = Xgraph.conjvs(atoms, hits, mt: mt)

      SensorRegistry.each_sensor(atoms, @secret, conjvs, mt: mt, &sink)
    end
  end
end
