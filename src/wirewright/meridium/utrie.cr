module Ww::Meridium
  # An emergent data structure for storing and navigating through strands of `Ubase`s
  # via recursive hashing.
  module Utrie
    extend self

    # We hash bases recursively. For instance, the following strand:
    #
    #    Begin - IsDict - At[0] - IsNum - Literal[100]
    #
    # Will be hashed as:
    #
    #    H0 = hash(entity utrie)
    #    H1 = hash(H0 || Begin)
    #    H2 = hash(H1 || IsDict)
    #    H3 = hash(H2 || At[0])
    #    H4 = hash(H3 || IsNum)
    #    H5 = hash(H4 || Literal[100])
    #
    # H5 is the endpoint of the strand. It is then fed to Xgraph and so on.
    private def mount1(atoms, strand : Enumerable(Ubase::Any)) : Atom
      hasher = Atom::Hasher.new

      h0 = Meridium.h(hasher, :utrie)

      strand.each do |base|
        h0 = Meridium.h(hasher, h0, base)
        atoms << h0
      end

      h0
    end

    # Mounts the atoms of strands in the strands enumerable *strands* to the Utrie
    # in *atoms*. Each strand is mounted concurrently with the others. Returns a
    # **disordered** array of endpoints corresponding to *strands*.
    def mount(atoms : IAtomAppend, strands : Enumerable(Enumerable(Ubase::Any))) : Array(Atom)
      endpoints = [] of Atom

      wg = WaitGroup.new
      lock = Mutex.new

      strands.each do |strand|
        wg.spawn do
          endpoint = mount1(atoms, strand)

          lock.synchronize { endpoints << endpoint }
        end
      end

      wg.wait

      endpoints
    end

    # :nodoc:
    @[Flags]
    enum State : UInt8
      BeforeTypecheck
      BeforeKeys
      BeforeLiteral
      BeforeEnd
      End
    end

    # :nodoc:
    #
    # An exploration "arm".
    record Arm, atom : Atom, arg : Term, state : State

    private def seed(hasher, term : Term) : Array(Arm)
      h0 = Meridium.h(hasher, :utrie)
      h0 = Meridium.h(hasher, h0, Ubase::Begin.new)

      [Arm.new(h0, term, State::BeforeTypecheck | State::BeforeEnd)]
    end

    private def sweep(atoms, gen0 : Array(Arm), gen1 : Array(Arm)) : Nil
      answer = atoms.present?(gen0, &.atom)

      gen1.clear
      gen0.each_with_index do |arm, index|
        next unless answer[index] # exists

        gen1 << arm
      end
    end

    private def advance(atoms, hasher, gen0, gen1, & : Atom ->) : Nil
      gen0.clear
      gen1.each do |arm|
        if arm.state.before_typecheck?
          case arm.arg.type
          in .any?     then unreachable
          in .symbol?  then base, state1 = Ubase::IsSym.new, State::BeforeLiteral
          in .number?  then base, state1 = Ubase::IsNum.new, State::BeforeLiteral
          in .string?  then base, state1 = Ubase::IsStr.new, State::BeforeLiteral
          in .boolean? then base, state1 = Ubase::IsBool.new, State::BeforeLiteral
          in .dict?    then base, state1 = Ubase::IsDict.new, State::BeforeKeys
          end

          atom1 = Meridium.h(hasher, arm.atom, base)

          # We can stop at e.g. IsNum - End or IsDict - End so add the BeforeEnd
          # state as well.
          gen0 << Arm.new(atom1, arm.arg, state1 | State::BeforeEnd)
        end

        if arm.state.before_end?
          atom1 = Meridium.h(hasher, arm.atom, Ubase::End.new)

          gen0 << Arm.new(atom1, arm.arg, State::End)
        end

        if arm.state.before_keys?
          #   This state is only reachable through BeforeTypecheck where we make sure
          # v it is in fact a dict.
          dict = arm.arg.as_d
          if dict.empty?
            # Empty dict is represented as strands IsDict - Literal({})
            atom1 = Meridium.h(hasher, arm.atom, Ubase::Literal.new(Term.of))

            # We do not put End after Literal. Switch to End state right away.
            gen0 << Arm.new(atom1, Term.of, State::End)
          else
            dict.each_entry do |key, value|
              atom1 = Meridium.h(hasher, arm.atom, Ubase::At.new(key))

              # We can stop at e.g. IsDict - At(0) - End so add the BeforeEnd state
              # as well.
              gen0 << Arm.new(atom1, value, State::BeforeTypecheck | State::BeforeEnd)
            end
          end
        end

        if arm.state.before_literal?
          atom1 = Meridium.h(hasher, arm.atom, Ubase::Literal.new(arm.arg))

          # We do not put End after Literal. Switch to End state right away.
          gen0 << Arm.new(atom1, arm.arg, State::End)
        end

        if arm.state.end?
          yield arm.atom
        end
      end
    end

    # Yields endpoint atoms for strands from the Utrie in *atoms* that match
    # the given *term*.
    #
    # Currently the implementation is rather sequential, and instead of multi-threading
    # relies on checking for the existence of large batches of atoms at a time. How large
    # depends on the Utrie in *atoms* and on *term*.
    def each_endpoint(atoms : IAtomsPresent, term : Term, & : Atom ->) : Nil
      hasher = Atom::Hasher.new

      gen0 = seed(hasher, term)
      gen1 = [] of Arm

      until gen0.empty?
        sweep(atoms, gen0, gen1)
        advance(atoms, hasher, gen0, gen1) { |endpoint| yield endpoint }
      end
    end

    # Collects the endpoint atoms yielded by `each_endpoint` into an array for you.
    def endpoints(atoms : IAtomsPresent, term : Term) : Array(Atom)
      endpoints = [] of Atom
      each_endpoint(atoms, term) do |endpoint|
        endpoints << endpoint
      end
      endpoints
    end
  end
end
