# An internal, ephemeral trie-like data structure for mapping sensor strands to
# atoms via recursive hashing.
#
# I call it "ephemeral" because it is "materialized" only through `Atom`s left
# after `mount`; which effectively serve as hints for `each_endpoint_atom` later
# on. So fragments of the trie exist as discovered by `each_endpoint_atom` of
# querying sides; nobody "owns" or "stores" the trie. Everyone can mount and
# everyone can explore the trie as they wish, provided they know what they must
# know (e.g. the algorithm). In a restricted setting it is obviously possible
# (not here, not really; but in general) for one member to explore the entire
# trie this way. But this is rarely feasible in practice.
module Utrie
  extend self

  # :nodoc:
  enum Action : UInt8
    EmitTypecheck
    EmitKeys
    EmitLiteral
    EmitEnd
    End
  end

  # :nodoc:
  record Arm, atom : Atom, arg : Term, ok : Action

  # Breaks down the given *strand* into atoms; appends those atoms to *atoms*.
  def mount(atoms, strand : Enumerable(Ubase::Any), hasher) : Atom
    buffer = uninitialized UInt8[32]

    # Create a byte- and block-level view into the buffer.
    h0 = buffer.to_slice

    # Initialize H0 to hasher's null hash, our consensus starting point.
    hasher.reset
    hasher.final(h0)

    # We hash bases recursively. For instance, the following strand:
    #
    #    Begin - IsDict - At[0] - IsNum - Literal[100]
    #
    # Will be hashed as:
    #
    #    H0 = hash() -- null hash of hasher, e.g. BLAKE3
    #    H1 = hash(H0 x Begin)
    #    H2 = hash(H1 x IsDict)
    #    H3 = hash(H2 x At[0])
    #    H4 = hash(H3 x IsNum)
    #    H5 = hash(H4 x Literal[100])
    #
    # H5 is the endpoint of the strand. It is then fed to Xtrie and so on.
    strand.each_with_index do |base, index|
      hstep(h0, base, hasher: hasher)

      atoms << Atom.of(h0)
    end

    Atom.of(h0)
  end

  # :nodoc:
  #
  # Updates *h0* with `hash(h0 x base)`.
  def hstep(h0 : Bytes, base : Ubase::Any, hasher) : Nil
    hasher.reset
    hasher.update(h0)
    Ubase.update(hasher, base)
    hasher.final(h0)
  end

  # :nodoc:
  #
  # Clears and populates *gen* with seed `Atom`s for *query*.
  def seed(gen : Array(Arm), query : Term, hasher) : Nil
    buffer = uninitialized UInt8[32]

    h0 = buffer.to_slice

    # Initialize H0 to hasher's null hash, our consensus starting point.
    hasher.reset
    hasher.final(h0)

    # Finally, populate gen with hash(hash() x Begin).
    hstep(h0, Ubase::Begin.new, hasher)

    # NOTE: We also handle stop at the root, e.g. in `_` we have Begin - End.
    gen.clear
    gen << Utrie::Arm.new(Atom.of(h0), query, :emit_typecheck)
    gen << Utrie::Arm.new(Atom.of(h0), query, :emit_end)
  end

  # :nodoc:
  struct GenAtoms
    include Enumerable(Atom)

    def initialize(@gen : Array(Arm))
    end

    def each(& : Atom ->) : Nil
      @gen.each { |arm| yield arm.atom }
    end
  end

  # :nodoc:
  #
  # Filters *gen* to leave only arms whose atoms exist, according to *question*.
  def sieve(question, gen : Array(Arm), answers : DynamicBitArray) : Nil
    answers.clear
    question.call(GenAtoms.new(gen), answers)

    index = 0
    gen.select! do |arm|
      answers[index]
    ensure
      index += 1
    end
  end

  # :nodoc:
  #
  # Replaces arms in *gen0* with their offspring; those offspring are put in
  # *gen1* (which is cleared beforehand).
  def advance(gen0 : Array(Arm), gen1 : Array(Arm), hasher)
    buffer = uninitialized UInt8[32]

    h0 = buffer.to_slice
    blocks = h0.unsafe_slice_of(UInt64)

    gen1.clear
    gen0.each do |arm|
      # So Dwarf Fortress, huh?
      ok, arg, atom = arm.ok, arm.arg, arm.atom

      case ok
      in .emit_typecheck?
        atom.copy_blocks_to(blocks)

        hstep(h0, Ubase.from(arm.arg.type), hasher)

        if arg.type.dict?
          gen1 << Arm.new(Atom.of(h0), arg, ok: :emit_keys)
        else
          gen1 << Arm.new(Atom.of(h0), arg, ok: :emit_literal)
        end

        # NOTE: We can stop at e.g. IsNum - End.
        gen1 << Arm.new(Atom.of(h0), arg, ok: :emit_end)
      in .emit_keys?
        dict = arg.as_d
        dict.each_entry do |key, value|
          atom.copy_blocks_to(blocks)

          hstep(h0, Ubase::At.new(key), hasher)

          # NOTE: We can stop at e.g. IsDict - At(0) - . as seen in `(_)`
          gen1 << Arm.new(Atom.of(h0), value, ok: :emit_typecheck)
          gen1 << Arm.new(Atom.of(h0), value, ok: :emit_end)
        end
      in .emit_literal?
        atom.copy_blocks_to(blocks)

        hstep(h0, Ubase::Literal.new(arg), hasher)

        # NOTE: Literals are always terminal. We do not insert a terminator
        # after a literal. So we route literals directly to :end.
        gen1 << Arm.new(Atom.of(h0), arg, ok: :end)
      in .emit_end?
        atom.copy_blocks_to(blocks)

        hstep(h0, Ubase::End.new, hasher)

        gen1 << Arm.new(Atom.of(h0), arg, ok: :end)
      in .end?
        yield atom
      end
    end
  end

  # Yields endpoint atoms that *query* excites.
  #
  # - *answers* is reused in calls to *question*.
  def each_endpoint_atom(question, query : Term, answers, hasher, & : Atom ->) : Nil
    gen0 = [] of Arm
    gen1 = [] of Arm

    seed(gen0, query, hasher)

    # NOTE: This loop will terminate no matter what, since *query* is finite;
    # even if *question* lies, we're still bounded by *query*. Worst-case, we
    # yield all possible atoms for *query*.
    while true
      sieve(question, gen0, answers)
      advance(gen0, gen1, hasher) { |endpoint| yield endpoint }

      gen0, gen1 = gen1, gen0

      break if gen0.empty?
    end
  end
end

# Represents a sensor surface.
#
# Sensor surfaces serve as passive observers of appearances in the termspace.
# They provide a holistic view of all appearances that excite them at a
# given moment.
class Sensor
  # :nodoc:
  alias Strand = Array(Ubase::Any)

  # :nodoc:
  alias Conj = Array(Strand)

  # :nodoc:
  alias Disj = Array({Conj, Label})

  # This exception could occur if there is a bug in the pattern-to-strand
  # translation machinery (`M1.skeleton` and others).
  class StrandParseError < Exception
  end

  # :nodoc:
  def initialize(
    @conid : Label,
    @grpid : Label,
    @pattern : Term,
    @secret : Term?,
    @branches : Disj,
  )
  end

  private SK_ANY  = Term.of({:"%any"})
  private SK_SYM  = Term.of({:"%symbol"})
  private SK_STR  = Term.of({:"%string"})
  private SK_NUM  = Term.of({:"%number", :_})
  private SK_DICT = Term.of({:"%dict"})
  private SK_BOOL = Term.of({:"%boolean"})

  # Converts skeleton strand *bases* to a `Strand`.
  #
  # May raise `StrandParseError` if *bases* are improperly arranged.
  private def self.strand(bases : Term::Dict) : Strand
    strand = Strand.new
    state = :start

    bases.items.each do |base|
      case state
      when :start
        unless base == SK_ANY
          raise StrandParseError.new("unexpected base #{base}, expected (%any)")
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
          raise StrandParseError.new("unexpected base #{base}, expected typecheck")
        end
      when :key
        Term.case(base) do
          matchpi %{(%'%value (%'%literal term_))} do
            strand << Ubase::At.new(term)
            state = :typecheck
          end

          otherwise do
            raise StrandParseError.new("unexpected base #{base}, expected %value %literal")
          end
        end
      when :literal
        Term.case(base) do
          matchpi %{(%'%literal term_)} do
            strand << Ubase::Literal.new(term)
            state = :after_literal
          end

          otherwise do
            raise StrandParseError.new("unexpected base #{base}, expected %literal")
          end
        end
      when :after_literal
        raise StrandParseError.new("expected end-of-strand after literal, but found base #{base}")
      end
    end

    # Indicate abrupt end (as in e.g. `Begin - *` or `Begin - IsDict - At(0) - *`) by
    # an explicit End base. If we had literal we treat it as end-of-strand regardless.
    unless state == :after_literal
      strand << Ubase::End.new
    end

    strand
  end

  # Converts skeleton *branch* to the corresponding conjunction object `Conj`.
  #
  # May raise `StrandParseError` if the strands that *branch* consists of are
  # improperly arranged.
  private def self.conj(branch : Term) : Conj
    strands = [] of Strand
    M1.strands(branch) do |bases|
      strands << strand(bases)
    end
    strands
  end

  # Converts pattern *skeleton* to the corresponding disjunction object `Conj`.
  #
  # *fresh* is used to generate fresh labels for branches in *skeleton*.
  #
  # May raise `StrandParseError` if the strands that *skeleton* consists of are
  # improperly arranged.
  private def self.disj(skeleton : Term, fresh : LabelGenerator) : Disj
    branches = Disj.new
    M1.branches(skeleton) do |branch|
      branches << {conj(branch), fresh.call}
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
  # - *fresh* is the label generator to use for generating instants of sensor
  #   constituents of *pattern*: an arbitrary M1 pattern can in fact consist
  #   of multiple sensors due to branching, and *fresh* will be used to generate
  #   an id for each one of them.
  # - *conid* is the globally unique, time-sortable connection id emitted
  #   by `LabelGenerator`.
  # - *grpid* is the globally unique, time-sortable id of this particular sensor,
  #   emitted by `LabelGenerator`. As noted above, an arbitrary *pattern* (and thus,
  #   `Sensor`) may in fact consist of multiple actual sensors; we call this a
  #   *sensor group*, hence *grpid* for short.
  # - *pattern* is the M1 pattern matching appearances that the sensor should
  #   be excited by.
  # - *secret* acts like a "password" or "scope" to appearances; for the sensor
  #   to see an appearance, both must have the same secret.
  #
  # May raise `StrandParseError`, but should not if everything is implemented
  # correctly (humph).
  def self.new(
    fresh : LabelGenerator,
    conid : Label,
    grpid : Label,
    pattern : Term,
    secret : Term? = nil,
  ) : Sensor
    branches = pipe(pattern, skeleton, disj(fresh))

    new(conid, grpid, pattern, secret, branches)
  end

  # Calls *fn* with atoms that `self` consists of.
  #
  # - *hasher* is the hasher to reuse.
  def each_atom(*, hasher = DIGEST_ALG.new, &fn : Atom ->) : Nil
    hasher.reset

    conjdeq = Deque(Atom).new

    @branches.each do |strands, instant|
      strands.each do |strand|
        utriep = TaggedAtomPipe.new(:utrie, hasher, fn)
        endpoint = Utrie.mount(utriep, strand, hasher)
        conjdeq << endpoint
      end

      # Sort vertices in the conjunction as `Xgraph` demands. The order isn't
      # important, what matters is that it's the same for insertion and querying.
      conjdeq.unstable_sort!

      xgraphp = TaggedAtomPipe.new(:xgraph, hasher, fn)
      conjv = Xgraph.mount(xgraphp, conjdeq, hasher: hasher)
      conjdeq.clear

      multimapp = TaggedAtomPipe.new(:sensor_multimap, hasher, fn)
      SensorMultimap.bind(multimapp, conjv, instant)

      registryp = TaggedAtomPipe.new(:sensor_registry, hasher, fn)
      SensorRegistry.register(
        atoms: registryp,
        instant: instant,
        secret: @secret,
        info: SensorInfo.new(@conid, @grpid),
        hasher: hasher,
      )
    end
  ensure
    hasher.reset
  end

  # Yields appearance complements of this sensor to the block, according to *query*.
  #
  # Each appearance is yielded in the form of its connection id and instant.
  #
  # - *answers* is the answers bit array to reuse.
  # - *hasher* is the hasher to reuse.
  def each_complement(
    question, *,
    answers = DynamicBitArray.new,
    hasher = DIGEST_ALG.new,
    & : Label, Label ->
  ) : Nil
    answers.clear
    hasher.reset

    seensets = [] of Set(Label)
    complements = Set(Label).new

    @branches.each do |strands, instant|
      seensets.clear

      strands.each do |strand|
        seen = Set(Label).new

        fieldq = TaggedAtomQuestion.new(:appearance_field, hasher, question)
        AppearanceField.each_appearance(
          question: fieldq,
          secret: @secret,
          strand: strand,
          hasher: hasher,
          answers: answers
        ) { |appearance| seen << appearance }

        if seen.empty?
          seensets.clear
          break
        end

        seensets << seen
      end

      next unless seensets.size == strands.size

      # Make sure the candidate is in all sets (matches all strands of the sensor).
      seensets.unstable_sort_by!(&.size)
      seensets[0].each do |candidate|
        next unless (1...seensets.size).all? { |index| candidate.in?(seensets[index]) }

        complements << candidate
      end
    end

    registryq = TaggedAtomQuestion.new(:appearance_registry, hasher, question)
    AppearanceRegistry.each_info(
      question: registryq,
      instants: complements,
      secret: @secret,
      hasher: hasher,
      answers: answers,
    ) { |info, instant| yield info.conid, instant }
  ensure
    answers.clear
    hasher.reset
  end
end

# Represents an appearance surface.
#
# Appearance surfaces serve as stimuli/excitation sources for sensors.
class Appearance
  # Constructs an appearance.
  #
  # - *conid* is the globally unique, time-sortable connection id emitted
  #   by `LabelGenerator`.
  # - *instant* is the globally unique, time-sortable id of this particular
  #   appearance, emitted by `LabelGenerator`.
  # - *value* is the value of this appearance.
  # - *secret* acts like a "password" or "scope" protecting this appearance;
  #   for a sensor to see this appearance, both must have the same secret.
  def initialize(@conid : Label, @instant : Label, @value : Term, @secret : Term? = nil)
  end

  # Calls *fn* with atoms that `self` consists of.
  #
  # - *hasher* is the hasher to reuse.
  def each_atom(*, hasher = DIGEST_ALG.new, &fn : Atom ->) : Nil
    hasher.reset

    AppearanceField.mount(
      atoms: TaggedAtomPipe.new(:appearance_field, hasher, fn),
      secret: @secret,
      value: @value,
      instant: @instant,
      hasher: hasher,
    )

    AppearanceRegistry.register(
      atoms: TaggedAtomPipe.new(:appearance_registry, hasher, fn),
      instant: @instant,
      secret: @secret,
      info: AppearanceInfo.new(@conid),
      hasher: hasher,
    )
  ensure
    hasher.reset
  end

  # Yields sensor complements of this appearance to the block, according to *query*.
  #
  # Each sensor is yielded in the form of its connection id, group id, and instant.
  #
  # - *query* is the query array to reuse.
  # - *answers* is the answers bit array to reuse.
  # - *hasher* is the hasher to reuse.
  def each_complement(
    question, *,
    query = [] of Atom,
    answers = DynamicBitArray.new,
    hasher = DIGEST_ALG.new,
    & : Label, Label, Label ->
  ) : Nil
    query.clear
    answers.clear
    hasher.reset

    endpoints = Deque(Atom).new
    utrieq = TaggedAtomQuestion.new(:utrie, hasher, question, reuse: query)
    Utrie.each_endpoint_atom(utrieq, @value, answers, hasher) do |atom|
      endpoints << atom
    end

    return if endpoints.empty? # No hits

    # Sort vertices in the conjunction as `Xgraph` demands. The order isn't
    # important, what matters is that it's the same for insertion and querying.
    endpoints.unstable_sort!

    facts = [] of Atom
    xgraphq = TaggedAtomQuestion.new(:xgraph, hasher, question, reuse: query)
    Xgraph.each_fact(xgraphq, endpoints, answers: answers, hasher: hasher) do |atom|
      facts << atom
    end

    # *facts* cannot be empty because it is passthrough. If there are no
    # conjunctions we'll simply have the same content as *vertices*, which
    # as we know at this point is nonempty.

    instants = [] of Label
    multimapq = TaggedAtomQuestion.new(:sensor_multimap, hasher, question, reuse: query)
    SensorMultimap.each_sensor(multimapq, facts, hasher: hasher, answers: answers) do |instant|
      instants << instant
    end

    return if instants.empty?

    registryq = TaggedAtomQuestion.new(:sensor_registry, hasher, question, reuse: query)
    SensorRegistry.each_info(registryq, instants, @secret, hasher: hasher, answers: answers) do |info, instant|
      yield info.conid, info.grpid, instant
    end
  ensure
    query.clear
    answers.clear
    hasher.reset
  end
end

