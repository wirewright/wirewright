module Ww::Meridium
  # An emergent graph of intersections. Materialized by "hints" about which binary
  # conjunctions exist. The querying side can then "climb" this "ladder of hints",
  # inferring higher and higher conjunctions by induction. Sensors can be looked
  # up under their top binary conjunction (called the sensor's conjunction *apex*)
  # using `SensorRegistry`.
  #
  # There is a certain trade-off in how smart the Xgraph is and how much packing
  # it provides. This is a particularly simple (one may even say crude) implementation;
  # it does not care about packing at all. We sort out of necessity, and this will
  # provide some kind of packing for very similar conjunctions; but divergence at any
  # point will derail this thing completely, and it will proceed to create newer and
  # newer nodes. Apparently I'm not smart enough to improve this :^)
  module Xgraph
    extend self

    # Returns the apex atom for *conj*, mounting "road sign" atoms along the way
    # that will lead the querying side to the apex.
    #
    # ```text
    #  a      b     c      d      e  [conj]
    #
    #     ab           cd     de
    #
    #          abcd       cdde
    #
    #              abcdcdde [apex]
    # ```
    def mount(atoms : IAtomAppend, conj : Enumerable(Atom)) : Atom
      hasher = Atom::HASHER.new

      gen0 = conj.to_a(&.itself)
      gen1 = [] of Atom

      while gen0.size > 1
        # NOTE: we're sorting by hash. The order won't be obvious. But "rulial"
        # clustering should be preserved (i.e. clustering within rule bounds).
        gen0.unstable_sort!

        cursor = 0
        while cursor < gen0.size
          u = gen0[cursor]
          unless v = gen0[cursor + 1]?
            v = u
            u = gen0[cursor - 1]
          end

          atom = Meridium.h(pointerof(hasher), :xgraph, u, v)
          gen1 << atom
          atoms << atom

          cursor += 2
        end

        gen0, gen1 = gen1, gen0
        gen1.clear
      end

      gen0.first # apex
    end

    private def explore(wg, atoms, u, vs, gen1, lock) : Nil
      hasher = Atom::HASHER.new

      # NOTE: we assume here that allocation is more expensive than hashing.
      # Whether it actually is I'm not sure; I guess it depends on how many
      # positive answers we get. If we get many then we've done twice the job
      # hashing -- bad; if we get little then we've saved some memory on all
      # the negative hashes -- good.

      answer = atoms.present?(vs) do |v|
        Meridium.h(pointerof(hasher), :xgraph, u, v)
      end

      answer.each_with_index do |exists, index|
        next unless exists

        atom = Meridium.h(pointerof(hasher), :xgraph, u, vs[index])

        lock.synchronize { gen1 << atom }
      end
    end

    # Yields conjunction vertices that exist in *hits* and are part of the Xgraph
    # in *atoms*. The yielded vertices may or may not be conjunction apexes; it
    # is your responsibility to track and check that, if necessary. Note that this
    # method may yield a lot of atoms; how much depends on the size of *hits* and
    # the exhaustiveness of the Xgraph in *atoms*.
    #
    # *mt* specifies whether to run under a multi-threaded or single-threaded
    # fiber execution context.
    def each_conjv(
      atoms : IAtomsPresent,
      hits : Enumerable(Atom), *,
      mt : Bool,
      & : Atom ->
    ) : Nil
      gen0 = hits.to_a(&.itself)
      gen1 = [] of Atom

      wg = WaitGroup.new
      ctx = mt ? MT : ST
      lock = Mutex.new

      while gen0.size > 1
        gen0.unstable_sort!
        gen0.each { |conjv| yield conjv }

        wg.add(gen0.size - 1)

        (0...gen0.size - 1).each do |index|
          u = gen0[index]
          vs = gen0.to_readonly_slice[index + 1..]

          ctx.spawn do
            explore(wg, atoms, u, vs, gen1, lock)
          ensure
            wg.done
          end
        end

        wg.wait

        gen0, gen1 = gen1, gen0
        gen1.clear
      end

      if top = gen0.first?
        yield top
      end
    end

    # Collects the conjunction vertices yielded by `each_conjv` into an array for
    # you. Returns that array.
    def conjvs(atoms : IAtomsPresent, hits : Enumerable(Atom), **kwargs) : Array(Atom)
      conjvs = [] of Atom
      each_conjv(atoms, hits, **kwargs) do |conjv|
        conjvs << conjv
      end
      conjvs
    end
  end
end
