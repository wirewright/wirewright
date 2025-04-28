module Ww::Meridium
  # Implements an emergent *bytes multimap*: effectively a general-purpose digit
  # trie that can store any number of arbitrary byte sequences into an `Atom` set;
  # and can then complete any prefix up to all of the stored sequences with
  # that prefix.
  #
  # Base 4 was chosen as the digit base; so much so that it is hard-coded into
  # the implementation. This is because it appears to be a suitable compromise
  # between very little guessing (as with base 2, you only have two choices ->
  # two questions to ask at each level) vs. atom count (with base 2 you get
  # breadth=2, and thus very deep tries that require a lot of "road signs" -- atoms).
  #
  # This implementation is *extremely* experimental, and rather slow even after
  # my efforts to optimize it. Thankfully, it is at least *somewhat* parallelizable
  # (how much depends on the underlying set). The majority of time we spend in Blake3,
  # hashing recursively or appending digits; thus multi-threading helps and is able
  # to (in some cases?) get us the <no. of cores>x speedup expected of "perfectly
  # parallelizable" problems such as this one.
  module BytesMultimap
    extend self

    private def each_b4_digit(data : Bytes, & : UInt8 ->)
      reader = BitReader.new(data)

      loop do
        # Consume one base-4 digit.
        bit0 = reader.consume? || break
        bit1 = reader.consume? || 0u8
        digit = (bit0 << 1) | bit1
        yield digit
      end
    end

    # Adds *data* to the multimap under *key* (which can be empty). We hash *key*
    # in but do not include hints for its completion, so *key* can be used as a
    # kind of "password" for *data*.
    #
    # *entity* is the "entity" to which the *key*-*data* pair belongs. It acts as
    # a "scope" for *key*-*data*.
    #
    # NOTE: You can execute multiple calls to `add` in parallel for better performance;
    # since the majority of the time is spent hashing *data*, even a crude lock-protected
    # *atoms* set would do.
    def add(atoms : IAtomAppend, entity : Entity, key : Bytes, data : Bytes) : Atom
      hasher = Blake3.new

      h0 = h(pointerof(hasher))
      h0 = h(pointerof(hasher), h0, entity.value)
      h0 = h(pointerof(hasher), h0, key)

      add(atoms, h0, data)
    end

    # :nodoc:
    def add(atoms : IAtomAppend, h0 : Atom, data : Bytes) : Atom
      append(h0, data) { |atom| atoms << atom }
    end

    # Appends the base-4 digits of *data* to *h0* through recursive hashing.
    # Returns the resulting *h0*.
    #
    # ```text
    # h0 ||= data0
    # h0 ||= data1
    # h0 ||= data2
    # ...
    # h0 ||= dataN
    #
    # ;; Where dataN is the Nth base-4 digit of *data*.
    # ```
    def append(h0 : Atom, data : Bytes, & : Atom ->) : Atom
      hasher = Blake3.new

      each_b4_digit(data) do |digit|
        h0 = h(pointerof(hasher), h0, digit)

        yield h0
      end

      h0
    end

    # :nodoc:
    def append(h0 : Atom, data : Bytes) : Atom
      append(h0, data) { }
    end

    # Each exploration fiber is running this method.
    private def explore(wg, atoms, completion, h0 : Atom, fn : Completion, Atom ->) : Nil
      hasher = Blake3.new

      loop do
        candidates = {0u8, 1u8, 2u8, 3u8}.map do |digit|
          h(pointerof(hasher), h0, digit.to_u8)
        end

        first = nil

        answer = atoms.present?(candidates, &.itself)
        answer.each_with_index do |exists, digit|
          next unless exists

          digit = digit.to_u8
          candidate = candidates[digit]

          unless first
            first = {candidate, digit}
            next
          end

          completion1 = completion.dup.append(digit)

          wg.add

          # Spawn further fibers to explore the other branches if their
          # atoms were found to exist.
          spawn explore(wg, atoms, completion1, candidate, fn)
        end

        unless first
          fn.call(completion, h0)
          wg.done
          return
        end

        # Reuse the same fiber for exploring one of the branches. This was supposed
        # to be a tail call, but since Crystal doesn't guarantee those would be
        # optimized, we write it as a loop explicitly.
        h0, digit = first
        completion = completion.append(digit)
      end
    rescue e : Exception
      # Crash gracefully (humph?!)
      wg.done
      raise e
    end

    # Calls *fn* with each possible completion for *key*-*prefix* under the given
    # *entity*. See also: `bind`.
    #
    # *mt* specifies whether to run under a multi-threaded or single-threaded
    # fiber execution context.
    #
    # WARNING: *fn* will be called from another fiber, perhaps running on another
    # thread if *mt* is `true` (it is by default). Thus make sure to either have
    # fully compartmentalized *fn*, or *fn* that talks to the outside world in a
    # thread-safe manner.
    #
    # The performance of this method in multi-threaded mode should *ideally*
    # be <no. of cpu cores>x vs. single-threaded; but this depends heavily on
    # how well `Fiber::ExecutionContext` schedules things as well as on the implementation
    # of the underlying *atoms* set. In my case, I'm getting ~3.5x speedup over
    # single-threaded mode with a very simple bucketized set (1024 buckets, each
    # with a lock). There's a lot of variables involved though, and we cannot
    # guarantee a lot (other than the underlying implementation is friendly toward
    # parallelization in general). As a general observation, it appears that the less
    # values there are in the multimap, the more comparable *mt* becomes to *st*; which
    # is actually expected, since the underlying algorithm adapts to the contents of
    # the set.
    def complete(
      atoms : IAtomsPresent,
      entity : Entity,
      key : Bytes,
      prefix : Bytes, *,
      mt : Bool,
      &fn : Completion, Atom ->
    ) : Nil
      hasher = Blake3.new

      h0 = h(pointerof(hasher))
      h0 = h(pointerof(hasher), h0, entity.value)
      h0 = h(pointerof(hasher), h0, key)

      each_b4_digit(prefix) do |digit|
        h0 = h(pointerof(hasher), h0, digit)
      end

      completion0 = Completion.new

      wg = WaitGroup.new
      wg.add

      ctx = mt ? MT : ST
      ctx.spawn { explore(wg, atoms, completion0, h0, fn) }

      wg.wait
    end

    # A group of four atoms.
    defcase Quad, a : Atom, b : Atom, c : Atom, d : Atom do
      include Enumerable(Atom)

      def each(& : Atom ->)
        {a, b, c, d}.each { |atom| yield atom }
      end
    end

    # A four-bit bitmask over the four atoms in `Quad`.
    record QuadMask, bits : UInt8 do
      def self.new(a : Bool, b : Bool, c : Bool, d : Bool) : QuadMask
        bits = 0u8
        bits |= 0b1 if a
        bits |= 0b10 if b
        bits |= 0b100 if c
        bits |= 0b1000 if d
        new(bits)
      end

      def none? : Bool
        bits.zero?
      end

      def select_with_digit(quad : Quad, & : Atom, UInt8 ->) : Nil
        yield quad.a, 0u8 if bits.bit_set?(0)
        yield quad.b, 1u8 if bits.bit_set?(1)
        yield quad.c, 2u8 if bits.bit_set?(2)
        yield quad.d, 3u8 if bits.bit_set?(3)
      end

      def &(other : QuadMask) : QuadMask
        QuadMask.new(bits & other.bits)
      end
    end

    # Constructs a quad for *atom* by continuing it with each of the base-4 digits
    # separately. This results is four atoms -- a quad.
    def quad(hasherptr, atom : Atom) : Quad
      Quad.new(
        h(hasherptr, atom, 0u8),
        h(hasherptr, atom, 1u8),
        h(hasherptr, atom, 2u8),
        h(hasherptr, atom, 3u8),
      )
    end

    # Represents a single completion-atom row. See the diagram in `expand`.
    alias Row = {Completion, Atom}

    # Represents the expansion of an atom into the quad of its possible successors
    # along with the completion that carries completion state.
    alias Expansion = {Completion, Quad}

    # Runs the expansion step on *rows* and returns the resulting expansion array.
    #
    # ```text
    #                                           expansion
    #                                          ┌───────────────────────────┐  │
    #                                          │ completion         quad   │  │
    #                                          │                           │  │
    #                                          │                    ┌── A0 │  │
    #                                     ┌──► │ 0 0 1 0 1 2 3 1 0  ├── A1 │  │
    #                                     │    │                    ├── A2 │  │
    #                                     │    │                    └── A3 │  │
    #                                     │    └───────────────────────────┘  │
    #         completion         atom ┌─┐ │                                   │
    #      │                     ┌─┐  │ │ │    ┌───────────────────────────┐  │
    #      │  0 0 1 0 1 2 3 1 0  │A│ ─│ │─┘    │ completion         quad   │  │
    #      │                     └─┘  │e│      │                           │  │
    #      │                     ┌─┐  │x│      │                    ┌── B0 │  │
    # rows │  1 0 0 1 3 2 3 1 2  │B│ ─│p│────► │ 1 0 0 1 3 2 3 1 2  ├── B1 │  │ expansions
    #      │                     └─┘  │a│      │                    ├── B2 │  │
    #      │                     ┌─┐  │n│      │                    └── B3 │  │
    #      │  3 2 1 2 1 3 3 1 0  │C│ ─│d│─┐    └───────────────────────────┘  │
    #      ▼                     └─┘  │ │ │                                   │
    #                                 └─┘ │    ┌───────────────────────────┐  │
    #                                     │    │ completion         quad   │  │
    #                                     │    │                           │  │
    #                                     │    │                    ┌── C0 │  │
    #                                     └──► │ 3 2 1 2 1 3 3 1 0  ├── C1 │  │
    #                                          │                    ├── C2 │  │
    #                                          │                    └── C3 │  │
    #                                          └───────────────────────────┘  │
    #                                                                         ▼
    # ```
    def expand(hasherptr, rows : Array(Row)) : Array(Expansion)
      rows.map { |completion, atom| {completion, quad(hasherptr, atom)} }
    end

    # Represents a quad mask-marked `Expansion`.
    alias MarkedExpansion = {Completion, Quad, QuadMask}

    # Creates a quad mask for each expansion, masking only atoms of the quad
    # that are present in *atoms*.
    #
    # ```text
    #                expansion                              marked expansion
    #
    #            │  ┌───────────────────────────┐          ┌──────────────────────────────────────┐
    #            │  │ completion         quad   │          │ completion         quad     mask     │ │
    #            │  │                           │   ┌─┐    │                                      │ │
    #            │  │                    ┌── A0 │   │ │    │                    ┌── A0   present  │ │
    #            │  │ 0 0 1 0 1 2 3 1 0  ├── A1 │ ──│ │──► │ 0 0 1 0 1 2 3 1 0  ├── A1   present  │ │
    #            │  │                    ├── A2 │   │ │    │                    ├── A2   absent   │ │
    #            │  │                    └── A3 │   │ │    │                    └── A3   present  │ │
    #            │  └───────────────────────────┘   │ │    └──────────────────────────────────────┘ │
    #            │                                  │ │                                             │
    #            │  ┌───────────────────────────┐   │ │    ┌──────────────────────────────────────┐ │
    #            │  │ completion         quad   │   │ │    │ completion         quad     mask     │ │
    #            │  │                           │   │m│    │                                      │ │
    #            │  │                    ┌── B0 │   │a│    │                    ┌── B0   absent   │ │
    # expansions │  │ 1 0 0 1 3 2 3 1 2  ├── B1 │ ──│r│──► │ 1 0 0 1 3 2 3 1 2  ├── B1   absent   │ │ marked expansions
    #            │  │                    ├── B2 │   │k│    │                    ├── B2   absent   │ │
    #            │  │                    └── B3 │   │ │    │                    └── B3   absent   │ │
    #            │  └───────────────────────────┘   │ │    └──────────────────────────────────────┘ │
    #            │                                  │ │                                             │
    #            │  ┌───────────────────────────┐   │ │    ┌──────────────────────────────────────┐ │
    #            │  │ completion         quad   │   │ │    │ completion         quad     mask     │ │
    #            │  │                           │   │ │    │                                      │ │
    #            │  │                    ┌── C0 │   │ │    │                    ┌── C0   present  │ │
    #            │  │ 3 2 1 2 1 3 3 1 0  ├── C1 │ ──│ │──► │ 3 2 1 2 1 3 3 1 0  ├── C1   absent   │ │
    #            │  │                    ├── C2 │   │ │    │                    ├── C2   abentt   │ │
    #            │  │                    └── C3 │   └─┘    │                    └── C3   absent   │ │
    #            ▼  └───────────────────────────┘          └──────────────────────────────────────┘ ▼
    # ```
    def mark(atoms : IAtomsPresent, expansions : Array(Expansion)) : Array(MarkedExpansion)
      answer = atoms.present?(expansions) do |_, quad|
        {quad.a, quad.b, quad.c, quad.d}
      end

      cursor = 0

      expansions.map do |completion, quad|
        mask = QuadMask.new(
          answer[cursor],
          answer[cursor + 1],
          answer[cursor + 2],
          answer[cursor + 3],
        )

        cursor += 4

        {completion, quad, mask}
      end
    end

    # Replaces quads in *marked* with their members which were seen in the atom set.
    # If none of a quad's members were seen in the atom set, its completion is
    # yielded to the block (i.e. there is nothing more to complete, either due
    # to set degradation or deliberately). Returns the resulting rows.
    #
    # ```text
    #                      marked expansion
    #
    #                     ┌──────────────────────────────────────┐                        completion           atom
    #                   │ │ completion         quad     mask     │   ┌─┐                                       ┌──┐ │
    #                   │ │                                      │   │ │          ┌─────► 0 0 1 0 1 2 3 1 0 0  │A0│ │
    #                   │ │                    ┌── A0   present  │ ──│ │──────────┘                         ▲  └──┘ │
    #                   │ │ 0 0 1 0 1 2 3 1 0  ├── A1   present  │ ──│ │──────────┐                            ┌──┐ │
    #                   │ │                    ├── A2   absent   │   │ │          └─────► 0 0 1 0 1 2 3 1 0 1  │A1│ │
    #                   │ │                    └── A3   present  │ ──│ │──────────┐                         ▲  └──┘ │ rows
    #                   │ └──────────────────────────────────────┘   │ │          │                            ┌──┐ │
    #                   │                                            │c│          └─────► 0 0 1 0 1 2 3 1 0 3  │A3│ │
    #                   │ ┌──────────────────────────────────────┐   │o│                                    ▲  └──┘ │
    #                   │ │ completion         quad     mask     │   │l│                                       ┌──┐ │
    #                   │ │                                      │   │l│          ┌─────► 3 2 1 2 1 3 3 1 0 0  │C0│ │
    #                   │ │                    ┌── B0   absent   │   │a│          │                         ▲  └──┘ ▼
    # marked expansions │ │ 1 0 0 1 3 2 3 1 2  ├── B1   absent   │   │p│          │
    #                   │ │ ───────┬────────── ├── B2   absent   │   │s│          │
    #                   │ │        │           └── B3   absent   │   │e│          │
    #                   │ └────────┼─────────────────────────────┘   │ │          │
    #                   │          └─────────────────────────────────│ │──┐       │
    #                   │ ┌──────────────────────────────────────┐   │ │  │       │
    #                   │ │ completion         quad     mask     │   │ │  │       │
    #                   │ │                                      │   │ │  │       │
    #                   │ │                    ┌── C0   present  │ ──│ │──┼───────┘
    #                   │ │ 3 2 1 2 1 3 3 1 0  ├── C1   absent   │   │ │  │
    #                   │ │                    ├── C2   absent   │   │ │  │
    #                   │ │                    └── C3   absent   │   └─┘  │
    #                   ▼ └──────────────────────────────────────┘        │
    #                                                                     │
    #                                                                     ▼
    #                                                         ┌───────────────────────┐
    #                                                         │         block         │
    #                                                         └───────────────────────┘
    # ```
    def collapse(marked : Array(MarkedExpansion), & : Completion ->) : Array(Row)
      rows = [] of {Completion, Atom}

      marked.each do |completion, quad, mask|
        if mask.none?
          yield completion
          next
        end

        mask.select_with_digit(quad) do |atom, digit|
          rows << {completion.dup.append(digit), atom}
        end
      end

      rows
    end

    # Same as `collapse`, but ignores the yielded completions.
    def collapse(marked : Array(MarkedExpansion)) : Array(Row)
      collapse(marked) { }
    end
  end
end
