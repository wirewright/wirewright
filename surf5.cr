require "digest"
require "log"
require "bit_array"
require "./src/wirewright"
require "./surf_common"
require "./blake3"

alias Checksum = UInt32

# TODO: refactor
module BytesMultimap(A)
  extend self

  Log = ::Log.for(self)

  module IAtom
    # Always zero for valid atoms. Nonzero sanity is invalid and is used as
    # a sanity check for the underlying set.
    abstract def sanity : UInt8
    abstract def block0 : UInt64
    abstract def block1 : UInt64
    abstract def block2 : UInt64
    abstract def block3 : UInt64

    def hash(hasher)
      block0.hash(hasher)
    end
  end

  abstract struct Atom
    getter sanity
    getter block0
    getter block1
    getter block2
    getter block3

    def initialize(@sanity : UInt8, @block0 : UInt64, @block1 : UInt64, @block2 : UInt64, @block3 : UInt64)
    end
  end

  # Initializes and populates the supplied *atoms* container with atoms for *byteslice*.
  #
  # WARNING: BytesMultimap does not insert end-of-input markers for you! When byteslice
  # B1 is a prefix of another one, B2, only B2 will be found upon query. You need to
  # designate some kind of symbol for EOI marking, so that it is a guaranteed dead-end.
  # This will help you in filtering junk off as well; since an EOI marker can only be
  # found at the end of the input, finding one in the middle is a way to early exit
  # due to corruption.
  def mount(atoms, byteslice : Bytes, *, start = 0, hasher = Blake3.new)
    # This is the fastest arrangement I was able to achieve yet. I'm getting roughly
    # 500ns per byte which is enormous, literally thousands of cycles for whatever...;
    # but still, better than what we had before.
    h0 = uninitialized UInt8[33]
    blocks = h0.to_slice.unsafe_slice_of(UInt64)

    # If start > 0, h0 will be initialized to prefix hash. If start = 0, h0 will
    # be BLAKE3's "hash of nothing".
    hasher.reset
    hasher.update(byteslice[...start])
    hasher.final(h0.to_voidptr)

    reader = BitReader.new(byteslice[start..])

    while true
      # Consume one base-4 digit.
      bit0 = reader.consume? || break
      bit1 = reader.consume? || 0u8
      digit = (bit0 << 1) | bit1

      # We hash recursively as an optimization, to avoid storing intermediate digest
      # objects (huge in case of BLAKE3, about 2KB each). Instead we only have to store
      # the 32 byte intermediate hashes.
      #
      #   H0 = hash()
      #       null hash, or if we have start > 0, also start bytes
      #   H1 = hash(H0 x 1)
      #      first digit hashed in
      #   H2 = hash(H1 x 2)
      #      second digit hashed in
      #   H3 = hash(H2 x 3)
      #      third digit hashed in
      #
      #   ... And so on. Note how we only need to know/copy the previous hash if
      #       we wanted to explore the various possibilities, instead of copying
      #       the entire digest state. E.g.:
      #
      #        H2 --> hash(H2 x <guess no. 1>)
      #               hash(H2 x <guess no. 2>)
      #               hash(H2 x <guess no. 3>)
      #               etc.
      h0.unsafe_put(32, digit)
      hasher.reset
      hasher.update(h0)
      hasher.final(h0.to_voidptr)

      block0, block1, block2, block3 = blocks

      atoms << A.new(0u8, block0, block1, block2, block3)
    end
  end

  # Keeps track of the progressive build-up of *prefix* along with a temporary
  # *tip* that collects base-4 digits.
  record Completion, prefix = [] of UInt8, tip = 0u8, cursor = 0u8 do
    def peek(& : Bytes -> T) : T forall T
      prefix << tip unless cursor.zero?

      yield prefix.to_readonly_slice
    ensure
      prefix.pop unless cursor.zero?
    end

    # Finalizes this completion and returns the resulting byteslice. The completion
    # must not be appended to after calling this method.
    def final : Bytes
      prefix << tip unless cursor.zero?
      prefix.to_readonly_slice
    end

    # Makes a full copy of this completion.
    def clone : Completion
      copy_with(prefix: prefix.dup, tip: tip, cursor: cursor)
    end

    def concat(bytes : Bytes) : Completion
      unless cursor.zero?
        raise "can only concat on byte boundary"
      end

      prefix << tip unless cursor.zero?
      prefix.concat(bytes)

      copy_with(tip: 0u8, cursor: 0u8)
    end

    # Appends a base-4 *digit* to the tip of this completion. Returns the modified
    # version of this completion. Note that the completion's prefix is mutated in-place;
    # use `clone` to create a fully detached copy, and call append on
    # it afterwards.
    def append(digit : UInt8) : Completion
      unless 0 <= digit <= 3
        raise ArgumentError.new
      end

      tip, cursor = @tip, @cursor

      tip |= digit << (6 - cursor)
      cursor += 2 # one base-4 digit

      if cursor == 8
        @prefix << tip
        tip = cursor = 0u8
      end

      copy_with(tip: tip, cursor: cursor)
    end
  end

  # A quad of atoms originating from the same parent atom, with that parent atom's
  # *completion* retained for further processing on squeeze.
  record AtomQuad(A), a : A, b : A, c : A, d : A, completion : Completion

  # :nodoc:
  record AtomQuadQuery(A), quads : Array(AtomQuad(A)), sanity : Bool = false do
    include Enumerable(A)

    def ntests : Int32
      Math.max(@quads.size * 0.25, 1).to_i
    end

    def each(& : A ->) : Nil
      @quads.each do |quad|
        yield quad.a
        yield quad.b
        yield quad.c
        yield quad.d
      end

      return unless @sanity

      ntests.times do
        yield A.new((1u8..255u8).sample, rand(UInt64), rand(UInt64), rand(UInt64), rand(UInt64))
      end
    end
  end

  # Raised when the underlying set is thought to be corrupted.
  #
  # We fight noise with noise; we periodically check whether the underlying set
  # is sane by asking it about whether nonsensical atoms exist. If the set reports
  # that such an atom exists, we consider the set "insane" and explore no further.
  class SanityCheckException < Exception
  end

  # Helps you query a `BytesMultimap`.
  class Completer(A, H)
    def initialize(
      @quads = [] of AtomQuad(A),
      @answer = DynBitArray.new,
      @survivors = [] of {A, Completion},
      @hasher : H = Blake3.new,
    )
      @clock = @trial = 0u32
      @trialstep = 1.0f32
    end

    # Fully resets this reader for subsequent reuse.
    #
    # NOTE: you will need to re-`seed` it as well.
    def reset : Nil
      @quads.clear
      @answer.clear
      @survivors.clear
      @hasher.reset
      @clock = @trial = 0u32
      @trialstep = 1.0f32
    end

    # Adds seed atoms for the given *key* to this reader's internal state.
    #
    # This method must be called before completion or any other kind of reading;
    # since otherwise the reader won't have any state to work off. It may be called
    # later on as well, in case you for some reason want to explore multiple *key*s
    # simultaneously, for example.
    #
    # WARNING: `seed` must be followed by `squeeze` for everything to work properly.
    def seed(key : Bytes = Bytes.empty) : Nil
      h0 = uninitialized UInt8[33]
      h1 = uninitialized UInt8[32]

      blocks0 = h0.to_slice.unsafe_slice_of(UInt64)
      blocks1 = h1.to_slice.unsafe_slice_of(UInt64)

      # Make H0 = hash() or H0 = hash(hash() x key)
      @hasher.reset
      @hasher.update(key)
      @hasher.final(h0.to_voidptr)

      # Loop through candidate digits and obtain quad = hash(H0 x candidate)
      quad = {0u8, 1u8, 2u8, 3u8}.map do |candidate|
        blocks1.copy_from(blocks0)
        h0.unsafe_put(32, candidate)

        @hasher.reset
        @hasher.update(h0)
        @hasher.final(h1.to_voidptr)

        block0, block1, block2, block3 = blocks1

        A.new(0u8, block0, block1, block2, block3)
      end

      @quads << AtomQuad(A).new(*quad, completion: Completion.new(key.to_a))
    end

    # Filters out or "squeezes" the set of possible atoms using *question*.
    #
    # Yields completions that were "squeezed away", meaning they have no completion.
    #
    # This method reduces the number of atoms to consider, effectively narrowing
    # down the search space.
    def squeeze(question, & : Bytes ->) : Nil
      @survivors.clear
      @answer.clear

      sanity = @clock == @trial
      query = AtomQuadQuery.new(@quads, sanity)
      question.call(Question::MemberSome(A).new(query, @answer))

      if sanity
        (@answer.size - query.ntests...@answer.size).each do |index|
          if @answer[index]
            raise SanityCheckException.new
          end
        end

        @trial = (@clock + @trialstep).to_i
        @trialstep = Math.min(@trialstep * 1.3, 8.0f32)
      end

      @clock += 1

      index = 0

      @quads.each do |quad|
        head = nil

        progress = {
          {@answer[index], quad.a},
          {@answer[index + 1], quad.b},
          {@answer[index + 2], quad.c},
          {@answer[index + 3], quad.d},
        }

        progress.each_with_index do |(survived, atom), digit|
          next unless survived

          digit = digit.to_u8

          unless head
            head = {atom, digit}
            next
          end

          @survivors << {atom, quad.completion.clone.append(digit)}
        end

        # Nobody survived.
        unless head
          yield quad.completion.final
          next
        end

        atom, digit = head

        @survivors << {atom, quad.completion.append(digit)}
      ensure
        index += 4
      end
    end

    # Expands the set of possible atoms by considering all possible next steps
    # or extensions from the current set of atoms.
    #
    # This method increases the number of atoms to consider, effectively broadening
    # the search space.
    private def expand : Nil
      @quads.clear

      h0 = uninitialized UInt8[33]
      blocks = h0.to_slice.unsafe_slice_of(UInt64)

      @survivors.each do |survivor, completion|
        # Loop through candidate digits and obtain quad = hash(H0 x candidate)
        quad = {0u8, 1u8, 2u8, 3u8}.map do |candidate|
          # Copy blocks into H0
          blocks[0] = survivor.block0
          blocks[1] = survivor.block1
          blocks[2] = survivor.block2
          blocks[3] = survivor.block3
          h0.unsafe_put(32, candidate)

          @hasher.reset
          @hasher.update(h0)
          @hasher.final(h0.to_voidptr)

          block0, block1, block2, block3 = blocks

          A.new(0u8, block0, block1, block2, block3)
        end

        @quads << AtomQuad(A).new(*quad, completion)
      end
    end

    def expect(bytes : Bytes) : Nil
      @survivors.map! do |atom, completion|
        {atom, completion.concat(bytes)}
      end
    end

    private def append(atom : A, bytes)
      h0 = uninitialized UInt8[33]
      blocks = h0.to_slice.unsafe_slice_of(UInt64)

      blocks[0] = atom.block0
      blocks[1] = atom.block1
      blocks[2] = atom.block2
      blocks[3] = atom.block3

      reader = BitReader.new(bytes)

      while true
        # Consume one base-4 digit.
        bit0 = reader.consume? || break
        bit1 = reader.consume? || 0u8
        digit = (bit0 << 1) | bit1

        h0.unsafe_put(32, digit)

        @hasher.reset
        @hasher.update(h0)
        @hasher.final(h0.to_voidptr)

        block0, block1, block2, block3 = blocks

        atom = A.new(0u8, block0, block1, block2, block3)
      end

      atom
    end

    def concat(bytes : Bytes) : Nil
      @survivors.map! do |atom, completion|
        {append(atom, bytes), completion.concat(bytes)}
      end

      expand
    end

    # Completes one base-4 digit. Yields completions that cannot be completed by
    # any base-4 digit (that terminated due to completion).
    def b4digit(question, & : Bytes ->) : Nil
      squeeze(question) { |finished| yield finished }
      expand
    end

    # Reads *n* bytes. Yields the resulting completions. Expects the block to look
    # through completions (using e.g. `Completion#peek`). Removes completions that
    # the block rejected (returned `false` for).
    #
    # May raise `SanityCheckException` in case the underlying set fails
    # a periodic sanity check.
    #
    # WARNING: the yielded completion is read-only.
    def refine(question, n : Int, & : Bytes -> Bool) : Nil
      return if n.zero?

      (n - 1).times do
        b4digit(question) { }
        b4digit(question) { }
        b4digit(question) { }
        b4digit(question) { }
      end

      b4digit(question) { }
      b4digit(question) { }
      b4digit(question) { }
      b4digit(question) { }

      @survivors.select! do |_, completion|
        completion.peek { |final| yield final }
      end
    end

    # Yields all possible completions of *key* using *question*.
    #
    # Completion byteslices are yours. You can do whatever you want with them.
    #
    # WARNING: while `BytesMultimap` has a simple sanity check built in that
    # detects set insanity, you are actually not recommended to use this method.
    # Try to use `read_bytes` instead, especially if you are reading a fixed-
    # width, known structure. This method may never terminate and consume all memory
    # if the set is insane but is able to "fool" this method into thinking there
    # are more and more completions etc. This is why you should design known-width
    # (and ideally fixed-width) data structures and read only that much bytes, and
    # then verify-verify-verify, because what you've read might be noise.
    def complete(question, key = Bytes.empty, & : Bytes ->) : Nil
      seed(key)
      squeeze(question) { |final| yield final }

      until at_end?
        b4digit(question) { |final| yield final }
      end
    rescue e : SanityCheckException
      Log.debug(exception: e) { "completion stopped: the underlying set has collisions and/or is insane" }
    end

    # Returns `true` if no more completions are possible.
    def at_end? : Bool
      @survivors.empty?
    end
  end
end

# ---------

module Utrie
  extend self

  record Atom, g0 : UInt64, g1 : UInt64, g2 : UInt64, g3 : UInt64 do
    # :nodoc:
    def self.of(h0 : Bytes)
      unless h0.size == 32
        raise ArgumentError.new("invalid h0")
      end

      blocks = h0.unsafe_slice_of(UInt64)

      Atom.new(blocks[0], blocks[1], blocks[2], blocks[3])
    end

    # :nodoc:
    def copy_groups_to(target : Slice(UInt64)) : Nil
      target[0] = g0
      target[1] = g1
      target[2] = g2
      target[3] = g3
    end
  end

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
  def mount(atoms, strand : Indexable(Ubase::Any), *, hasher = Blake3.new) : Nil
    buffer = uninitialized UInt8[32]

    # Create a byte- and block-level view into the buffer.
    h0 = buffer.to_slice

    # Initialize H0 to hasher's null hash, our consensus starting point.
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
    strand.each do |base|
      hstep(h0, base, hasher: hasher)

      atoms << Atom.of(h0)
    end
  end

  # :nodoc:
  #
  # Updates *h0* with `hash(h0 x base)`.
  def hstep(h0 : Bytes, base : Ubase::Any, *, hasher = Blake3.new) : Nil
    hasher.reset
    hasher.update(h0)
    Ubase.update(hasher, base)
    hasher.final(h0)
  end

  # :nodoc:
  #
  # Clears and populates *gen* with seed `Atom`s for *query*.
  def seed(gen : Array(Arm), query : Term, *, hasher = Blake3.new) : Nil
    buffer = uninitialized UInt8[32]

    h0 = buffer.to_slice

    # Initialize H0 to hasher's null hash, our consensus starting point.
    hasher.final(h0)

    # Finally, populate gen with hash(hash() x Begin).
    hstep(h0, Ubase::Begin.new, hasher: hasher)

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
  def sieve(question, gen : Array(Arm), answers : DynBitArray) : Nil
    answers.clear
    question.call(Question::MemberSome.new(GenAtoms.new(gen), answers))

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
  def advance(gen0 : Array(Arm), gen1 : Array(Arm), *, hasher = Blake3.new)
    buffer = StaticArray(UInt8, 32).new(0u8)

    h0 = buffer.to_slice
    g = h0.unsafe_slice_of(UInt64)

    gen1.clear
    gen0.each do |arm|
      # So Dwarf Fortress, huh?
      ok, arg, atom = arm.ok, arm.arg, arm.atom

      case ok
      in .emit_typecheck?
        atom.copy_groups_to(g)

        hstep(h0, Ubase.from(arm.arg.type), hasher: hasher)

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
          atom.copy_groups_to(g)

          hstep(h0, Ubase::At.new(key), hasher: hasher)

          # NOTE: We can stop at e.g. IsDict - At(0) - . as seen in `(_)`
          gen1 << Arm.new(Atom.of(h0), value, ok: :emit_typecheck)
          gen1 << Arm.new(Atom.of(h0), value, ok: :emit_end)
        end
      in .emit_literal?
        atom.copy_groups_to(g)

        hstep(h0, Ubase::Literal.new(arg), hasher: hasher)

        # NOTE: Literals are always terminal. We do not insert a terminator
        # after a literal. So we route literals directly to :end.
        gen1 << Arm.new(Atom.of(h0), arg, ok: :end)
      in .emit_end?
        atom.copy_groups_to(g)

        hstep(h0, Ubase::End.new, hasher: hasher)

        gen1 << Arm.new(Atom.of(h0), arg, ok: :end)
      in .end?
        yield atom
      end
    end
  end

  # Yields endpoint atoms that *query* excites.
  #
  # - *answers* is reused in calls to *question*.
  def each_endpoint_atom(question, query : Term, answers : DynBitArray, & : Atom ->) : Nil
    gen0 = [] of Arm
    gen1 = [] of Arm

    seed(gen0, query)

    # NOTE: This loop will terminate no matter what, since *query* is finite;
    # even if *question* lies, we're still bounded by *query*. Worst-case, we
    # yield all possible atoms for *query*.
    while true
      sieve(question, gen0, answers)
      advance(gen0, gen1) { |endpoint| yield endpoint }

      gen0, gen1 = gen1, gen0

      break if gen0.empty?
    end
  end
end

# xtrie

# One-to-many map for decoding a conjunction vertex fingerprint into unique sensor
# ids that are bound to it.
#
# Sensor multimap entries have the following byte format:
#
# ```text
# <conjv: fingerprint> | S R M M <sensor id : label> <crc32 checksum : 4 bytes>
# ```
module SensorMultimap
  extend self

  Log = ::Log.for(self)

  struct Atom < BytesMultimap::Atom
  end

  SIGNATURE = "SRMM".to_slice
  SIGNATURE_BYTESIZE = 4

  {% begin %}
    ENTRY_BYTESIZE = {{FINGERPRINT_BYTESIZE + SIGNATURE_BYTESIZE + LABEL_BYTESIZE + sizeof(Checksum)}}
  {% end %}

  # Appends the atoms for the multimap binding of *sensor* to the given conjunction
  # vertex fingerprint *conjv*.
  #
  # Current atom cost of one such binding is ~100 atoms. If multiple sensors are bound
  # to *conjv*, they will share the bytes for *conjv* itself, so the cost of each
  # binding will be slightly lower.
  def bind(atoms, conjv : Fingerprint, sensor : Label) : Nil
    unless conjv.size == FINGERPRINT_BYTESIZE
      raise ArgumentError.new("invalid conjunction vertex")
    end

    buffer = uninitialized UInt8[ENTRY_BYTESIZE]
    entry = cursor = buffer.to_slice

    # Append key (conjv).
    cursor.copy_from(conjv)
    cursor += conjv.size

    # Append signature.
    cursor.copy_from(SIGNATURE)
    cursor += SIGNATURE_BYTESIZE

    # Append sensor label.
    cursor = sensor.append_be(cursor)

    # Append checksum.
    checksum = Digest::CRC32.checksum(entry[...-sizeof(Checksum)])
    IO::ByteFormat::BigEndian.encode(checksum, cursor)
    cursor += sizeof(Checksum)

    # Produce atoms.
    BytesMultimap(Atom).mount(atoms, entry, start: FINGERPRINT_BYTESIZE)
  end

  # Yields sensors bound to the given conjunction vertex fingerprint *conjv*
  # according to *question*.
  def each_sensor(question, conjv : Fingerprint, *, hasher : H = Blake3.new, &sink : Label ->) : Nil forall H
    reader = BytesMultimap::Completer(Atom, H).new(hasher: hasher)
    reader.seed(conjv)

    # NOTE: The .debug conditions here are not .warns because they're more or less
    # nominal in case another thread / client etc. suddenly removes their appearance
    # while completion is in progress. Some atoms we were able to read but then were
    # suddenly cut off due to removal. Completion would terminate early therefore;
    # and one of these sanity checks would fail.

    # Read signature.
    reader.refine(question, SIGNATURE_BYTESIZE) do |entry|
      unless entry.size == FINGERPRINT_BYTESIZE + SIGNATURE_BYTESIZE
        Log.debug { "reject entry: invalid fingerprint-signature sequence bytesize" }
        next false
      end

      unless entry[-SIGNATURE_BYTESIZE...] == SIGNATURE
        Log.debug { "reject entry: invalid signature byte sequence" }
        next false
      end

      true
    end

    # Read sensor label.
    reader.refine(question, LABEL_BYTESIZE) do |entry|
      unless label = Label.from_slice_be?(entry[-LABEL_BYTESIZE..])
        Log.debug { "reject entry: invalid label byte sequence" }
        next false
      end

      unless WWID.makes_sense?(label)
        Log.debug { "reject entry: label is nonsensical (e.g. too old or is from the future)" }
        next false
      end

      true
    end

    # Read checksum.
    reader.refine(question, sizeof(Checksum)) do |entry|
      # Verify checksum (0 stands for "original")
      checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, entry[-sizeof(Checksum)..])
      checksum1 = Digest::CRC32.checksum(entry[...-sizeof(Checksum)])
      valid = checksum0 == checksum1

      unless valid
        Log.debug { "reject entry: checksum mismatch (my #{checksum1} != its #{checksum0})" }
      end

      valid
    end

    # Entries "squeezed out" at this point (ones that terminate) are valid entries.
    # Any other entries are invalid.
    reader.squeeze(question) do |entry|
      sensor = entry[FINGERPRINT_BYTESIZE + SIGNATURE_BYTESIZE...-sizeof(Checksum)]

      yield Label.from_slice_be?(sensor).not_nil!
    end
  rescue e : BytesMultimap::SanityCheckException
    Log.debug(exception: e) { "completion stopped: the underlying set has collisions and/or is insane" }
  end
end

# Appearance field entries have the following byte format:
#
# ```
# <secret: fingerprint> | A P R F <encoded bases: ...> <appearance id : label> <crc32 checksum : 4 bytes> 0
# ```
module AppearanceField
  extend self

  struct Atom < BytesMultimap::Atom
  end

  PERIOD = 0u8

  SIGNATURE_BYTESIZE = 4
  SIGNATURE = "APRF".to_slice

  # Appends the atoms for the multimap binding of *secret* and *value* to the given
  # appearance label *appearance*.
  #
  # The atom cost of such binding depends heavily on the size of *value*, and could
  # range from hundreds to hundreds of thousands of atoms (or more).
  def mount(atoms, secret : Bytes, value : Term, appearance : Label)
    strand = [] of Ubase::Any
    entry = [] of UInt8

    Term.each_keypath_and_leaf(value) do |keypath, leaf|
      strand.clear
      entry.clear

      # All entries start with the secret.
      entry.concat(secret)

      # Secret is followed by the signature on the value side.
      entry.concat(SIGNATURE)

      # Then we convert the keypath and leaf into a sequence of Ubases,
      # aka "strand".
      Ubase.strandof(keypath, leaf, to: strand)

      # We then encode the strand as a bunch of bytes.
      Ubase.encode(strand, to: entry)

      # We then append the id of the appearance.
      appearance.append_be(entry)

      # And finally, we compute the checksum of the entry so far...
      checksum = Digest::CRC32.checksum(entry.to_readonly_slice)

      # ... and append it to the entry as well.
      buffer = uninitialized UInt8[4]
      IO::ByteFormat::BigEndian.encode(checksum, buffer.to_slice)

      entry.concat(buffer)

      # Append PERIOD byte
      entry << PERIOD

      # Finally, we mount the resulting entry in the multimap; making sure to
      # skip through the entirety of the secret. This means we will not leave
      # hints about what each digit in the secret is; so while it is still possible
      # to just start exploring from an empty prefix, we'd have to explore through
      # e.g. 32 bytes (in case of a hash) without any hints whatsoever. This means
      # we'd have to explore a 2**256 search space before we get to hints. So
      # the secrets are secret enough, as long as they're longer than a few bytes.
      BytesMultimap(Atom).mount(atoms, entry.to_readonly_slice, start: secret.size)

      true # continue
    end
  end

  # Yields appearance ids at the endpoints of valid continuations of *strand*.
  def each_appearance(question, secret : Bytes, strand : Array(Ubase::Any), *, hasher : H = Blake3.new, & : Label ->) : Nil forall H
    completer = BytesMultimap::Completer(Atom, H).new(hasher: hasher)
    completer.seed(secret)

    completer.refine(question, SIGNATURE_BYTESIZE) do |entry|
      unless entry.size == secret.size + SIGNATURE_BYTESIZE
        Log.debug { "reject entry: invalid fingerprint-signature sequence bytesize" }
        next false
      end

      unless entry[-SIGNATURE_BYTESIZE...] == SIGNATURE
        Log.debug { "reject entry: invalid signature byte sequence" }
        next false
      end

      true
    end

    prefix = [] of UInt8

    unless strand.empty?
      Ubase.encode(strand, to: prefix)

      # Expect each signature to be followed by the Ubase sequence that we are
      # querying for.
      completer.concat(prefix.to_readonly_slice)
    end

    zerosize = secret.size + SIGNATURE_BYTESIZE + prefix.size

    until completer.at_end?
      completer.b4digit(question) do |entry|
        next if entry.size == zerosize

        unless entry.last == PERIOD
          Log.debug { "reject entry: does not end with PERIOD" }
          next
        end

        entry = entry[...-1]

        if entry.size < zerosize + Label.bytesize + sizeof(Checksum)
          Log.debug { "reject entry: size mismatch (#{entry.size} < #{zerosize + Label.bytesize + sizeof(Checksum)})" }
          next
        end

        # Verify checksum (0 stands for "original")
        checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, entry[-sizeof(Checksum)..])
        checksum1 = Digest::CRC32.checksum(entry[...-sizeof(Checksum)])

        unless checksum0 == checksum1
          Log.debug { "reject entry: checksum mismatch (my #{checksum1} != its #{checksum0})" }
          next
        end

        unless appearance = Label.from_slice_be?(entry[-LABEL_BYTESIZE - sizeof(Checksum)...-sizeof(Checksum)])
          Log.debug { "reject entry: invalid label byte sequence" }
          next
        end

        unless WWID.makes_sense?(appearance)
          Log.debug { "reject entry: label is nonsensical (e.g. too old or is from the future)" }
          next
        end

        yield appearance
      end
    end
  rescue e : BytesMultimap::SanityCheckException
    Log.debug(exception: e) { "completion stopped: the underlying set has collisions and/or is insane" }
  end
end

# tbase

# sensorregistry

# appearanceregistry
# TODO: we don't need to store value in AppearanceRegistry. Instead we
# can use IChat to ask the conid that owns the appearance to send the value.

# class sensor

# class appearance

class DynBitArray
  include Indexable::Mutable(Bool)

  GROWTH_FACTOR = 1.5

  def initialize(capacity0 = 32)
    @bits = BitArray.new(capacity0)
    @size = 0
  end

  def size : Int32
    @size
  end

  def unsafe_fetch(index : Int) : Bool
    @bits.unsafe_fetch(index)
  end

  def unsafe_put(index : Int, value : Bool) : Nil
    @bits.unsafe_put(index, value)
  end

  def push(value : Bool) : Nil
    # Resize
    if @size + 1 > @bits.size
      bits1 = BitArray.new((@bits.size * GROWTH_FACTOR).to_i)
      @bits.each_with_index do |bit, index|
        bits1.unsafe_put(index, bit)
      end
      @bits = bits1
    end

    unsafe_put(@size, value)

    @size += 1
  end

  def <<(value : Bool) : self
    push(value)

    self
  end

  def clear : Nil
    @size = 0
  end
end

module Question(A)
  record MemberOne(A), atom : A, answer : Bool* do
    include Question(A)
  end

  record MemberAny(A), atoms : Enumerable(A), answer : Bool* do
    include Question(A)
  end

  record MemberSome(A), atoms : Enumerable(A), answers : DynBitArray do
    include Question(A)
  end
end

module Ubase
  def self.strandof(keypath : Stack(Term), leaf : Term, *, to strand : Array(Ubase::Any)) : Nil
    keypath.each do |key|
      strand << Ubase::IsDict.new
      strand << Ubase::At.new(key)
    end

    case leaf.type
    in .symbol?
      strand << Ubase::IsSym.new
    in .string?
      strand << Ubase::IsStr.new
    in .number?
      strand << Ubase::IsNum.new
    in .boolean?
      strand << Ubase::IsBool.new
    in .dict?, .any?
      raise ArgumentError.new("invalid leaf")
    end

    strand << Ubase::Literal.new(leaf)
  end

  CODE_DICT  = 0u8
  CODE_SYM   = 1u8
  CODE_STR   = 2u8
  CODE_NUM   = 3u8
  CODE_BOOL  = 4u8
  CODE_AT_ML = 5u8
  CODE_AT_HASH = 6u8
  CODE_LITERAL_ML = 7u8
  CODE_LITERAL_HASH = 8u8

  def self.encode(strand : Array(Ubase::Any), *, to encoding : Array(UInt8))
    strand.each do |base|
      case base
      in Ubase::Begin
        raise ArgumentError.new("unexpected trunk in strand")
      in Ubase::IsDict
        encoding << CODE_DICT
      in Ubase::IsSym
        encoding << CODE_SYM
      in Ubase::IsStr
        encoding << CODE_STR
      in Ubase::IsNum
        encoding << CODE_NUM
      in Ubase::IsBool
        encoding << CODE_BOOL
      in Ubase::At, Ubase::Literal
        if ML.compact_bytesize(base.term) <= FINGERPRINT_BYTESIZE
          case base
          in Ubase::At      then encoding << CODE_AT_ML
          in Ubase::Literal then encoding << CODE_LITERAL_ML
          end

          # Stream bytes that `ML.compact` writes to io directly into
          # the encoding array.
          io = IO::ByteStream.new { |bytes| encoding.concat(bytes) }

          ML.compact(io, base.term)
        else
          case base
          in Ubase::At      then encoding << CODE_AT_HASH
          in Ubase::Literal then encoding << CODE_LITERAL_HASH
          end

          hasher = Blake3.new
          buffer = uninitialized UInt8[32]

          # Stream bytes that `ML.compact` writes into the hasher.
          io = IO::ByteStream.new { |bytes| hasher.update(bytes) }

          ML.compact(io, base.term)

          # Append hash bytes to encoding.
          hasher.final(buffer.to_voidptr)
          encoding.concat(buffer)
        end
      end
    end
  end
end

Log.setup_from_env(default_level: :trace)

set = Set(Utrie::Atom).new

ask = ->(question : Question(Utrie::Atom)) do
  case question
  when Question::MemberOne(Utrie::Atom)
    question.answer.value = question.atom.in?(set)
  when Question::MemberAny(Utrie::Atom)
    # question.answer.value = {true, false}.sample
    question.answer.value = question.atoms.any? &.in?(set)
  when Question::MemberSome(Utrie::Atom)
    question.atoms.each do |atom|
      # question.answers << {true, false}.sample
      question.answers << atom.in?(set)
    end
  else
    unreachable
  end

  nil
end

Utrie.mount(set, [Ubase::Begin.new, Ubase::IsNum.new, Ubase::Literal.new(Term.of(0))])
Utrie.mount(set, [Ubase::Begin.new, Ubase::IsNum.new, Ubase::Literal.new(Term.of(1))])
Utrie.mount(set, [Ubase::Begin.new, Ubase::IsNum.new, Ubase::Literal.new(Term.of(2))])
Utrie.mount(set, [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::End.new])
Utrie.mount(set, [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:y)), Ubase::IsNum.new, Ubase::End.new])
Utrie.mount(set, [Ubase::Begin.new, Ubase::IsDict.new, Ubase::End.new])
Utrie.mount(set, [Ubase::Begin.new, Ubase::End.new])

answer = DynBitArray.new

Utrie.each_endpoint_atom(ask, Term.of(0), answer) { |a| pp! a }
Utrie.each_endpoint_atom(ask, Term.of(1), answer) { |b| pp! b }
Utrie.each_endpoint_atom(ask, Term.of(2), answer) { |c| pp! c }
Utrie.each_endpoint_atom(ask, Term.of(3), answer) { |d| pp! d }
Utrie.each_endpoint_atom(ask, Term.of(x: 100, y: 200), answer) { |e| pp! e }
Utrie.each_endpoint_atom(ask, Term.of(x: 100), answer) { |f| pp! f }
Utrie.each_endpoint_atom(ask, Term.of(x: 200), answer) { |g| pp! g }
Utrie.each_endpoint_atom(ask, Term.of(z: 300), answer) { |h| pp! h }

{% skip_file %}

StrandSet.mount(set, Blake3.final("hello"))
StrandSet.mount(set, Blake3.final("world"))
pp set
pp StrandSet.strand?(ask, Blake3.final("hello"))
pp StrandSet.strand?(ask, Blake3.final("world"))
pp StrandSet.strand?(ask, Blake3.final("boo"))

{% skip_file %}
a = WWID.call
b = WWID.call
c = WWID.call

pp! a
pp! b
pp! c

AppearanceField.mount(set, Blake3.final(""), Term.of(:+, 1, {:-, 3, 4}, x: 100, y: 200), a)
AppearanceField.mount(set, Blake3.final(""), Term.of(:-, 2, {:-, 3, 4}, x: 100, y: 200), b)
AppearanceField.mount(set, Blake3.final("qux"), Term.of(:+, 3, {:-, "Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.", 4}, x: 100, y: 200), c)

puts "All"
AppearanceField.each_appearance(ask, Blake3.final(""), [] of Ubase::Any) do |appearance|
  pp appearance
end

puts "isDict"
AppearanceField.each_appearance(ask, Blake3.final(""), [Ubase::IsDict.new] of Ubase::Any) do |appearance|
  pp appearance
end

puts "+"
AppearanceField.each_appearance(ask, Blake3.final(""), [Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:+))] of Ubase::Any) do |appearance|
  pp appearance
end

puts "-"
AppearanceField.each_appearance(ask, Blake3.final(""), [Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:-))] of Ubase::Any) do |appearance|
  pp appearance
end

puts "+Qux"
AppearanceField.each_appearance(ask, Blake3.final("qux"), [Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:+))] of Ubase::Any) do |appearance|
  pp appearance
end

puts "-Qux"
AppearanceField.each_appearance(ask, Blake3.final("qux"), [Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:-))] of Ubase::Any) do |appearance|
  pp appearance
end

# AppearanceField.each_appearance(ask, Blake3.final("qux"), [] of Ubase::Any) do |appearance|
#   pp appearance
# end

# AppearanceField.each_appearance(ask, Blake3.final("baz"), [] of Ubase::Any) do |appearance|
#   pp appearance
# end

pp set.size

# b = WWID.call
# c = WWID.call

# pp! a
# pp! b
# pp! c

# SensorMultimap.bind(set, Blake3.final("hello world"), a)
# pp set.size
# SensorMultimap.bind(set, Blake3.final("hello world"), b)
# SensorMultimap.bind(set, Blake3.final("bye world"), c)
# SensorMultimap.bind(set, Blake3.final("qux"), a)
# SensorMultimap.each_sensor(ask, Blake3.final("hello world")) do |sensor|
#   pp sensor
# end
# SensorMultimap.each_sensor(ask, Blake3.final("bye world")) do |sensor|
#   pp sensor
# end
# SensorMultimap.each_sensor(ask, Blake3.final("qux")) do |sensor|
#   pp sensor
# end

{% skip_file %}
struct MyBitSet
  def initialize(@size : Int32)
    @bits = BitArray.new(@size)
  end

  def includes?(atom : DemoAtom)
    @bits[atom.block0 % @size]
  end

  def <<(atom : DemoAtom)
    @bits[atom.block0 % @size] = true
  end
end

set = MyBitSet.new(128)
# set = Set(DemoAtom).new
ask = ->(q : Enumerable(DemoAtom), ans : Array(Bool), early : EarlyExit) do
  q.each do |a|
    v = set.includes?(a)
    ans << v
    break if early.first_positive? && v
    # ans << {true, false}.sample
  end
end

  record DemoAtom, sanity : UInt8, block0 : UInt64, block1 : UInt64, block2 : UInt64, block3 : UInt64 do
    include BytesMultimap::Atom
  end

str = "Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.\0"

pp str.bytesize
require "benchmark"

hasher = Blake3.new
# n = 0
# digit = nil
# Benchmark.ips do |x|
#   x.report("add") do
    # BytesMultimap(DemoAtom).mount(set, str.to_slice, hasher: hasher)
    BytesMultimap(DemoAtom).mount(set, "Hello World\0".to_slice, hasher: hasher)
    # BytesMultimap(DemoAtom).mount(set, "Bye World\0".to_slice, hasher: hasher)
    # BytesMultimap(DemoAtom).mount(set, "F\0".to_slice, hasher: hasher)
  pp set
  # end

  # x.report("get") do
    # BytesMultimap(DemoAtom).digit(ask, Blake3.new) do |digest, digit0|
    #   digit = digit0
    # end

    # workspace = [] of DemoAtom
    # BytesMultimap(DemoAtom).expand(workspace, Blake3.new)
  #   end
  # end
# end
# pp digit
#   x.report("retrieve") do
#     BytesMultimap(DemoAtom).complete(ask, "".to_slice) do |comp|
#       n += 1
#     end
#   end
# end

completer = BytesMultimap::Completer(DemoAtom, Blake3).new

require "benchmark"

# n = 0
# Benchmark.ips do |x|
#   x.report("read 65 000") do
completer.complete(ask) do |comp|
  pp String.new(comp)
  # next unless comp.all?(&.chr.ascii_alphanumeric?)
end
completer.reset

#   end
# end

# completer.byte(ask) { |comp| pp comp }
# # completer.byte(ask)
# completer.each_surviving_completion do |comp|
#   pp comp
#   pp comp.u16
# end

# completer.each_surviving_row do |row|
#   pp String.new(row.prefix.to_readonly_slice)
# end

# until completer.at_end?
#   completer.b4digit(ask) do |finished|
#     pp String.new(finished)
#   end
# end

# completer.complete(ask) do |finished|
#   pp String.new(finished)
# end
# gen0 = BytesMultimap::AtomQuadArray(DemoAtom).new
# gen1 = [] of {DemoAtom, BytesMultimap::Row}
# answer = [] of Bool
# BytesMultimap(DemoAtom).seed(Bytes.empty, gen0)


require "benchmark"

n = 0
# Benchmark.ips do |x|
#   x.report("bench") do
#     gen0.clear
# BytesMultimap(DemoAtom).seed(Bytes.empty, gen0)
#  BytesMultimap(DemoAtom).complete(ask, gen0, answer, gen1, hasher: hasher) do |finished|
#   # n += 1
#   pp String.new(finished)
# end
#   end
# end
# pp n


# BytesMultimap(DemoAtom).squeeze(ask, gen0, answer, gen1) { |finished| pp finished }
# BytesMultimap(DemoAtom).expand(gen1, gen0)
# gen0, gen1 = gen1, gen0
# pp gen0
# offspring = [] of DemoAtom
# orows = [] of BytesMultimap::Row
# answer = [] of Bool
# BytesMultimap(DemoAtom).digit(ask, seeds, rows, offspring, orows, answer, hasher: hasher) do |ep|
#   pp ep
# end
# seeds, offspring = offspring, seeds
# rows, orows = orows, rows
# BytesMultimap(DemoAtom).digit(ask, seeds, rows, offspring, orows, answer, hasher: hasher) do |ep|
#   pp ep
# end
# seeds, offspring = offspring, seeds
# rows, orows = orows, rows
# BytesMultimap(DemoAtom).digit(ask, seeds, rows, offspring, orows, answer, hasher: hasher) do |ep|
#   pp ep
# end
# seeds, offspring = offspring, seeds
# rows, orows = orows, rows
# BytesMultimap(DemoAtom).digit(ask, seeds, rows, offspring, orows, answer, hasher: hasher) do |ep|
#   pp ep
# end
# seeds, offspring = offspring, seeds
# rows, orows = orows, rows

# # pp seeds
# # pp rows
# BytesMultimap(DemoAtom).digit(ask, seeds, rows, offspring, orows, answer, hasher: hasher) do |ep|
#   pp! ep
# end
# seeds, offspring = offspring, seeds
# BytesMultimap(DemoAtom).digit(ask, seeds, offspring, answer, hasher: hasher)
# seeds, offspring = offspring, seeds
# BytesMultimap(DemoAtom).digit(ask, seeds, offspring, answer, hasher: hasher)
# pp offspring
# seeds, offspring = offspring, seeds
# BytesMultimap(DemoAtom).digit(ask, seeds, offspring, answer, hasher: hasher)
# pp offspring

# # BytesMultimap(DemoAtom).expand(seeds, offspring, hasher: hasher)

# answer = [] of Bool
# seeds = BytesMultimap(DemoAtom).seeds(key: Bytes.empty)
# BytesMultimap(DemoAtom).contract(ask, seeds, answer)
# BytesMultimap(DemoAtom).expand(seeds) do |child|
#   offspring << child
# end
# # swap
# seeds, offspring = offspring, seeds
# offspring.clear
# pp seeds

# BytesMultimap(DemoAtom).contract(ask, seeds, answer)

# offspring = [] of DemoAtom
# BytesMultimap(DemoAtom).expand(seeds) do |child|
#   offspring << child
# end
# pp offspring
# pp seeds

# pp offspring
# pp set.size
# pp set.join('\n', &.progress.hexstring)

# Xtrie.mount(set, Deque{"hello".to_slice, "world".to_slice, "abc".to_slice}.sort!)
# Xtrie.mount(set, Deque{"hello".to_slice, "foo".to_slice, "abc".to_slice}.sort!)
# Xtrie.conjs(ask, Deque{"hello".to_slice, "foo".to_slice, "world".to_slice, "abc".to_slice}.sort!) do |c|
#   pp c
# end
