# require "digest"
require "log"
require "bit_array"
require "./src/wirewright"
require "./surf_common"
require "./blake3"

enum EarlyExit : UInt8
  None
  FirstPositive
end

module Xtrie
  extend self

  # Represents a connection between two "facts".
  #
  # Its fingerprint is a higher-order "fact"; that is, the combination of
  # the two basic "facts" *a* and *b*.
  record Atom, a : Fingerprint, b : Fingerprint do
    def update(digest : Digest) : Nil
      digest.update(a)
      digest.update(b)
    end

    def inspect(io)
      io << "XtrieAtom["
      Base64.strict_encode(a, io)
      io << ", "
      Base64.strict_encode(b, io)
      io << "]"
    end
  end

  # Mounts an Xtrie rule (an *xrule*). Returns the fingerprint of the xrule.
  # See also: `Xtrie`.
  #
  # NOTE: *xrule* must be pre-sorted ascending. *xrule* is mutated by this
  # method; you lose ownership of it until this method returns.
  def mount(atoms, xrule : Deque(Fingerprint)) : Fingerprint
    if xrule.empty?
      raise ArgumentError.new
    end

    digest = DIGEST_ALG.new

    while xrule.size > 1
      u = xrule.shift
      v = xrule.shift

      atoms << Atom.new(u, v)

      digest.reset
      digest.update(u)
      digest.update(v)

      xrule << digest.final
    end

    xrule[0]
  end

  # TODO: perform set sanity check to avoid infinite loops
  private def conjs(ask, digest, vertices, sink) : Nil
    query = [] of Atom
    answer = [] of Bool

    while u = vertices.shift?
      sink.call(u)

      vertices.each do |v|
        query << Atom.new(u, v)
      end

      answer.clear
      ask.call(query, answer, EarlyExit::None)
      query.clear

      answer.each_with_index do |exists, i|
        next unless exists

        digest.reset
        digest.update(u)
        digest.update(vertices[i])

        vertices << digest.final
      end
    end
  end

  # Calls *sink* with all mounted conjunction vertices whose corresponding
  # conjunctions are satisfied by *vertices*.
  #
  # NOTE: *vertices* must be pre-sorted ascending. *vertices* is mutated by
  # this method; you lose ownership of it until this method returns.
  def conjs(ask, vertices : Deque(Fingerprint), &sink : Fingerprint ->) : Nil
    digest = DIGEST_ALG.new

    conjs(ask, digest, vertices, sink)
  end
end

module BytesMultimap(A)
  extend self

  Log = ::Log.for(self)

  module Atom
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
    blocks = h0.to_voidptr.as(UInt64*)

    # If start > 0, h0 will be initialized to prefix hash. If start = 0, h0 will
    # be BLAKE3's IV.
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
      #       IV of the hash, or if we have start > 0, also start bytes
      #   H1 = hash(H0 x 1)
      #      first byte hashed in
      #   H2 = hash(H1 x 2)
      #      second byte hashed in
      #   H3 = hash(H2 x 3)
      #      third byte hashed in
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
    def peek(& : Bytes ->) : Nil
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

    # Appends a base-4 *digit* to the tip of this completion. Returns the modified
    # version of this completion. Note that the completion's prefix is mutated in-place;
    # use `clone` to create a fully detached copy, and call append on
    # it afterwards.
    def append(digit : UInt8) : Completion
      unless 0 <= digit <= 3
        raise ArgumentError.new
      end

      prefix, tip, cursor = @prefix, @tip, @cursor

      if cursor == 8
        prefix << tip
        tip = cursor = 0u8
      end

      tip |= digit << (6 - cursor)
      cursor += 2 # one base-4 digit

      copy_with(prefix: prefix, tip: tip, cursor: cursor)
    end
  end

  # A quad of atoms originating from the same parent atom, with that parent atom's
  # *completion* retained for further processing on squeeze.
  record AtomQuad(A), a : A, b : A, c : A, d : A, completion : Completion

  # :nodoc:
  record AtomQuadQuery(A), quads : Array(AtomQuad(A)), sanity : Bool = false do
    include Enumerable(A)

    delegate :clear, :<<, to: @quads

    def each(& : A ->) : Nil
      @quads.each do |quad|
        yield quad.a
        yield quad.b
        yield quad.c
        yield quad.d
      end

      if @sanity
        yield A.new((1u8..255u8).sample, rand(UInt64), rand(UInt64), rand(UInt64), rand(UInt64))
      end
    end

    def each_quad(& : AtomQuad(A) ->)
      @quads.each do |quad|
        yield quad
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
      @answer = [] of Bool,
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
      h0 = uninitialized UInt8[32]
      h1 = uninitialized UInt8[33]

      blocks0 = h0.to_slice.unsafe_slice_of(UInt64)
      blocks1 = h1.to_slice.unsafe_slice_of(UInt64)

      # Make H0 = hash() or H0 = hash(hash() x key)
      @hasher.reset
      @hasher.update(key)
      @hasher.final(h0.to_voidptr)

      # Loop through candidate digits and obtain quad = hash(H0 x candidate)
      quad = {0u8, 1u8, 2u8, 3u8}.map do |candidate|
        blocks1.copy_from(blocks0)
        h1.unsafe_put(32, candidate)

        @hasher.reset
        @hasher.update(h1)
        @hasher.final(h1.to_voidptr)

        block0, block1, block2, block3 = blocks1

        A.new(0u8, block0, block1, block2, block3)
      end

      @quads << AtomQuad(A).new(*quad, completion: Completion.new)
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
      question.call(query, @answer, EarlyExit::None)

      if sanity
        if @answer[-1]
          raise SanityCheckException.new
        end

        @trial = (@clock + @trialstep).to_i
        @trialstep = Math.min(@trialstep * 1.3, 4.0f32)
      end

      @clock += 1

      index = 0

      @quads.each do |quad|
        tail = nil

        progress = {
          {@answer[index], quad.a},
          {@answer[index + 1], quad.b},
          {@answer[index + 2], quad.c},
          {@answer[index + 3], quad.d},
        }

        progress.each_with_index do |(survived, atom), digit|
          next unless survived

          digit = digit.to_u8

          unless tail
            tail = {atom, digit}
            next
          end

          @survivors << {atom, quad.completion.clone.append(digit)}
        end

        # Nobody survived.
        unless tail
          yield quad.completion.final
          next
        end

        atom, digit = tail

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

    # Completes one base-4 digit. Yields completions that cannot be completed by
    # any base-4 digit (that terminated due to completion).
    def b4digit(question, & : Bytes ->)
      expand
      squeeze(question) { |finished| yield finished }
    end

    # Reads *n* bytes. Yields the resulting completions.
    #
    # May raise `SanityCheckException` in case the underlying set fails
    # a periodic sanity check.
    #
    # WARNING: the yielded completion is read-only.
    def read_bytes(question, n : Int, & : Completion ->) : Nil
      (n - 1).times do
        b4digit(question) { }
        b4digit(question) { }
        b4digit(question) { }
        b4digit(question) { }
      end

      b4digit(question) { }
      b4digit(question) { }
      b4digit(question) { }

      @survivors.each { |_, completion| yield completion }
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
      Log.warn(exception: e) { "completion stopped: the underlying set has collisions and/or is insane" }
    end

    # Returns `true` if no more completions are possible.
    def at_end? : Bool
      @survivors.empty?
    end
  end
end

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
