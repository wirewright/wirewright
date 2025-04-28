require "./src/wirewright"
require "log"
require "./blake3"
require "digest"
require "wait_group"

alias BitList = DynamicBitArray

# + Utrie    ;; maps bases to endpoints
# + Xgraph   ;; maps endpoint conjunctions to conjunction apex through binary conjunctions
# + BytesMultimap
#   + SensorRegistry     ;; maps conjunction apex to sensor ids
#   + AppearanceRegistry ;; maps term strands to appearance ids
#
# LATER: protect BytesMultimap, and Xgraph from lying sets by emitting two
# types of sanity check queries:
# - Since we have Entity already we can introduce NegXgraph NegSensorRegistry etc.,
#   elements of which are always absent (else the set is insane.) This tests how sanely
#   the set generates negative responses.
# - We can also do randomized lookback -- i.e. remember a random `true` query from the past
#   and emit it sometime in the future. This tests how sanely the set generates positive
#   reponses -- if it says `false` for something that was `true` before and we're working
#   on the consequences of that right now, we halt.
# These should occur on IAtomsPresent implementor level.

enum Entity : UInt8
  Utrie
  Xgraph
  SensorRegistry
  AppearanceRegistry
end

include Ww::Meridium

def h0 : Atom
  hasher = Blake3.new
  scratch = uninitialized UInt8[Atom::BYTESIZE]
  hasher.final(scratch.to_slice)
  Atom.of(scratch.to_slice)
end

NULL_HASH_ATOM = h0

def h(hasherptr) : Atom
  NULL_HASH_ATOM
end

def h(hasherptr, a : Nil) : Atom
  h(hasherptr)
end

def h(hasherptr, a : Entity) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  hasherptr.value.reset
  hasherptr.value.update(a.value)
  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def h(hasherptr, a : Term) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  hasherptr.value.reset

  io = IO::ByteStream.new { |slice| hasherptr.value.update(slice) }
  ML.compact(io, a)

  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def h(hasherptr, a : Atom, b : Bytes) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  a.copy_hash_to(scratch.to_slice)

  hasherptr.value.reset
  hasherptr.value.update(scratch.to_slice)
  hasherptr.value.update(b)
  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def h(hasherptr, a : Atom, b : Atom) : Atom
  {% begin %}
    scratch = uninitialized UInt8[{{Atom::BYTESIZE * 2}}]

    a.copy_hash_to(scratch.to_slice[0, Atom::BYTESIZE])
    b.copy_hash_to(scratch.to_slice[Atom::BYTESIZE, Atom::BYTESIZE])

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)
    hasherptr.value.final(scratch.to_slice)

    Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
  {% end %}
end

def h(hasherptr, a : Atom, b : UInt8) : Atom
  {% begin %}
    scratch = uninitialized UInt8[{{Atom::BYTESIZE + 1}}]

    a.copy_hash_to(scratch.to_slice[0, Atom::BYTESIZE])
    scratch[-1] = b

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)
    hasherptr.value.final(scratch.to_slice[0, Atom::BYTESIZE])

    Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
  {% end %}
end

def h(hasherptr, a : UInt8, b : Atom) : Atom
  {% begin %}
    scratch = uninitialized UInt8[{{Atom::BYTESIZE + 1}}]

    scratch[0] = a
    b.copy_hash_to(scratch.to_slice[1, Atom::BYTESIZE])

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)
    hasherptr.value.final(scratch.to_slice[0, Atom::BYTESIZE])

    Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
  {% end %}
end

def h(hasherptr, a : Atom, b : Ubase::Any) : Atom
  scratch = uninitialized UInt8[Atom::BYTESIZE]

  a.copy_hash_to(scratch.to_slice)

  hasherptr.value.reset
  hasherptr.value.update(scratch.to_slice)

  Ubase.update(hasherptr, b)

  hasherptr.value.final(scratch.to_slice)

  Atom.of(scratch.to_slice)
end

def h(hasherptr, a : Atom, b : Nil) : Atom
  h(hasherptr, a, h(hasherptr))
end

def h(hasherptr, entity : Entity, a, b) : Atom
  h(hasherptr, h(hasherptr, entity.value, a), b)
end

def secret_to_bytes(secret : Term) : Bytes
  io = IO::Memory.new
  io.write_byte(1)
  ML.compact(io, secret)
  io.to_slice
end

def secret_to_bytes(secret : Nil) : Bytes
  Bytes[0]
end

# :nodoc:
ST = Fiber::ExecutionContext::SingleThreaded.new("Meridium single-threaded")

# :nodoc:
MT = Fiber::ExecutionContext::MultiThreaded.new("Meridium multi-threaded", System.cpu_count.to_i)

class Completion
  # :nodoc:
  OFFSET_BEGIN = 62u8

  # :nodoc:
  OFFSET_END = 0u8

  def initialize(@blocks = Slice(UInt64).empty, @block = 0u64, @offset = OFFSET_BEGIN)
  end

  def append(digit : UInt8) : Completion
    offset = @offset

    @block |= digit.to_u64 << offset

    if offset == OFFSET_END
      @blocks = @blocks.append(@block)
      @block = 0u64
      @offset = OFFSET_BEGIN
    else
      @offset -= 2 # one base-4 digit
    end

    self
  end

  def bytesize(key : Bytes, prefix : Bytes) : Int32
    ntailbytes, ntailbits = (OFFSET_BEGIN - @offset).divmod(8)

    key.size + prefix.size + @blocks.size*8 + ntailbytes + (ntailbits.zero? ? 0 : 1)
  end

  def final_to(target : Bytes, key : Bytes, prefix : Bytes) : Bytes
    ntailbytes, ntailbits = (OFFSET_BEGIN - @offset).divmod(8)

    cursor = target

    cursor.copy_from(key)
    cursor += key.size

    cursor.copy_from(prefix)
    cursor += prefix.size

    @blocks.each do |block|
      IO::ByteFormat::BigEndian.encode(block, cursor)
      cursor += 8 # bytes
    end

    if ntailbytes + ntailbits > 0
      scratch = uninitialized UInt8[8]

      IO::ByteFormat::BigEndian.encode(@block, scratch.to_slice)

      cursor.copy_from(scratch.to_slice[0, ntailbytes])
      cursor += ntailbytes

      if ntailbits > 0
        cursor[0] = scratch[ntailbytes]
        cursor += 1 # byte
      end
    end

    target
  end

  def final(key : Bytes, prefix : Bytes) : Bytes
    final_to(Bytes.new(bytesize(key, prefix)), key, prefix)
  end

  def_equals_and_hash @blocks, @block, @offset
end

alias Checksum = UInt32

module AppearanceRegistry
  extend self

  # :nodoc:
  APPEARANCE_SET_GAP = "appearances".to_slice

  private def mount1(atoms, secret_slice : Bytes, ubases : Array(Ubase::Any), appearance : WWID) : Nil
    data = Ubase.upack(ubases)

    terminal = BytesMultimap.add(atoms, :appearance_registry, secret_slice, data: data)

    # Insert an artificial gap after the terminal atom in data. After this gap we
    # will have the appearances subscribed to the strand.
    terminal = BytesMultimap.append(terminal, APPEARANCE_SET_GAP)

    # Append the appearance id after the gap. The gap is implicit. The other side
    # will need to pass it on its own.
    scratch = uninitialized UInt8[WWID::BYTESIZE]

    appearance.to_slice_be(scratch.to_slice)

    BytesMultimap.add(atoms, terminal, scratch.to_slice)
  end

  # Subscribes *appearance* to perceptions of *value* under *secret*.
  #
  # *mt* specifies whether to run under a multi-threaded or single-threaded
  # fiber execution context.
  def mount(atoms, secret : Term?, value : Term, appearance : WWID, *, mt : Bool) : Nil
    secret_slice = secret_to_bytes(secret)

    ctx = mt ? MT : ST
    wg = WaitGroup.new

    Term.each_keypath_and_leaf(value) do |keypath, leaf|
      ubases = Ubase.strand(keypath, leaf)

      wg.add
      ctx.spawn do
        mount1(atoms, secret_slice, ubases, appearance)
      ensure
        wg.done
      end

      true # continue
    end

    wg.wait
  end

  alias Row = {Completion, Atom}

  private def bundleof(atoms, secret_slice : Bytes, strand : Array(Ubase::Any), mt : Bool) : Array(Row)
    prefix = Ubase.upack(strand)

    # The completion callback runs on different threads (that is, it may).
    # So we must synchronize somehow.
    bundle = [] of Row
    lock = Mutex.new

    BytesMultimap.complete(atoms, :appearance_registry, secret_slice, prefix, mt: mt) do |completion, atom|
      # Remember that we insert an artificial gap between the strand bytes and the set
      # of appearances subscribed to that strand (represented as a digit trie). To fill
      # this gap we have to complete 0, by an implicit mount-query consensus; there are
      # no explicit hints for us to do that in the set, so complete() terminates -- not
      # knowing what to do. We know, though -- we have to complete 0.
      row = {Completion.new, BytesMultimap.append(atom, APPEARANCE_SET_GAP)}

      lock.synchronize { bundle << row }
    end

    # At this point we know all completion fibers have terminated. We can use
    # row without a lock safely.

    bundle
  end

  def each_appearance(atoms, secret : Term?, strands, *, mt : Bool, &fn : WWID ->) : Nil
    secret_slice = secret_to_bytes(secret)

    wg = WaitGroup.new
    ctx = mt ? MT : ST

    # Convert strands to bundles concurrently.
    bundles = [] of Array(Row)
    lock = Mutex.new

    strands.each do |strand|
      wg.add

      ctx.spawn do
        bundle = bundleof(atoms, secret_slice, strand, mt)

        lock.synchronize { bundles << bundle }
      ensure
        wg.done
      end
    end

    wg.wait

    hasher = Blake3.new
    marked = [] of Array(BytesMultimap::MarkedExpansion)
    expanded = [] of Array(BytesMultimap::Expansion)
    populations = [] of Set(Completion)

    (WWID::BYTESIZE*4 + 1).times do |ord|
      return if bundles.empty?

      # Expand each bundle with possible digit completions.
      expanded.clear
      expanded.concat(bundles) { |bundle| BytesMultimap.expand(pointerof(hasher), bundle) }

      # FIXME: we need to run mark() in concurrently. Otherwise mark()
      # would block for each bundle -- nonsense!!

      # Mark each digit completion according to whether it is present in *atoms*.
      marked.clear
      marked.concat(expanded) { |bundle| BytesMultimap.mark(atoms, bundle) }

      unless ord == WWID::BYTESIZE*4
        # Prune all dead-end completions.
        bundles.clear
        bundles.concat(marked) { |bundle| BytesMultimap.collapse(bundle) }

        # Index for cheap intersection
        populations.clear
        populations.concat(bundles) do |bundle|
          bundle.to_set { |completion, _| completion }
        end

        # Select only those completions that are present in all other bundles.
        bundles.each do |bundle0|
          xsect = bundle0.select! do |completion, _|
            populations.all? { |population| completion.in?(population) }
          end

          # If any bundle ends up being empty, then all other bundles will
          # be empty and so on. No point in continuing to complete.
          return if xsect.empty?
        end

        next
      end

      marked.each do |bundle|
        # Dead-end completions at this point are valid completions. Process them.
        BytesMultimap.collapse(bundle) do |completion|
          bytesize = completion.bytesize(key: Bytes.empty, prefix: Bytes.empty)
          unless bytesize == WWID::BYTESIZE
            pp completion
            Log.debug { "reject entry: unexpected entry bytesize #{bytesize}" }
            next
          end

          scratch = uninitialized UInt8[WWID::BYTESIZE]
          entry = scratch.to_slice
          completion.final_to(entry, key: Bytes.empty, prefix: Bytes.empty)

          begin
            appearance = WWID.from_slice_be(entry)
          rescue e : WWID::ParseError
            Log.debug(exception: e) { "reject entry" }
            next
          end

          fn.call(appearance)
        end
      end

      break
    end
  end
end

class MySet
  include IAtomAppend
  include IAtomsPresent

  def initialize(@n : Int32)
    @sets = Slice(Set(Atom)).new(@n) { Set(Atom).new }
    @locks = Slice(Mutex).new(@n) { Mutex.new }
  end

  def <<(atom : Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket] << atom
    end
  end

  def present?(atom : Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket].includes?(atom)
    end
  end

  def size
    @sets.sum(&.size)
  end

  def present?(objects : Enumerable(T), & : T -> Atom | Enumerable(Atom)) : BitList forall T
    answer = BitList.new

    objects.each do |object|
      ee = yield object

      unless ee.is_a?(Enumerable(Atom))
        ee = {ee}
      end

      ee.each do |atom|
        answer << present?(atom)
      end
    end

    answer
  end
end

Log.setup_from_env(default_level: :debug)

tspace = MySet.new(4096)

# trunk = WWID.new

# puts "Generate appearances"

# appearances = (0...100).flat_map do |x|
#   (0...100).map do |y|
#     Appearance.new(Term.of(type: "pixel", x: x, y: y, color: {rand(UInt8), rand(UInt8), rand(UInt8)}))
#   end
# end.to_readonly_slice

# puts "Insert appearances into termspace"

# wg = WaitGroup.new

# wg.spawn do
#   slot = 0u32
#   piece = appearances[0...2500]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 1 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.spawn do
#   slot = 2500u32
#   piece = appearances[2500...5000]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 2 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.spawn do
#   slot = 5000u32
#   piece = appearances[5000...7500]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 3 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.spawn do
#   slot = 7500u32
#   piece = appearances[7500...10000]
#   piece.each_with_index do |appearance, index|
#     if slot % 1000 == 0
#       Log.debug { "fiber 4 #{(index/piece.size)*100}%" }
#     end
#     appearance.atoms_to(trunk.with_slot(slot), tspace)
#     slot += 1
#   end
# end

# wg.wait

# # # appearances.each_with_index do |appearance, index|
# # #   if index % 1000 == 0
# # #     puts "Insert #{index}/#{appearances.size}"
# # #   end

# # # end

# puts "Done, 100x100 set cost is #{tspace.size}"

# s = Sensor.new(Term.of(color: {0, :_, :_}))

# 100.times do
# dt, comp = Time.measured do
#   s.complement_set(tspace)
# end

# pp comp
# puts "Took #{dt.total_milliseconds}ms"
# end

# while true
#   puts "Enter sensor pattern ML"

#   q = ML.term(gets || break)

#   sensor = Sensor.new(q)

#   pp sensor

#   complements = Set(WWID).new
#   dt = Time.measure do
#     complements = sensor.complement_set(tspace)
#   end

#   puts "#{complements.size} complement(s). Done in #{dt.total_milliseconds}ms"
# end

# {% skip_file %}

trunk = WWID.new
aid0 = trunk = trunk.succ
aid1 = trunk = trunk.succ
aid2 = trunk = trunk.succ
aid3 = trunk = trunk.succ
sid0 = trunk = trunk.succ
sid1 = trunk = trunk.succ

s0 = Sensor.new(Term.of(type: "pixel", x: {:"%any", 0, 1}, y: :y_number))
s1 = Sensor.new(Term.of(type: "pixel", color: {:_, :_, 255}))

a1 = Appearance.new(Term.of(type: "pixel", x: 0, y: 100, color: {255, 0, 0}))
a2 = Appearance.new(Term.of(type: "pixel", x: 1, y: 200, color: {0, 255, 0}))
a3 = Appearance.new(Term.of(type: "pixel", x: 2, y: 300, color: {0, 0, 255}))
a4 = Appearance.new(Term.of(type: "pixel", x: 0, y: 400, color: {255, 0, 255}))

dt = Time.measure do
  s0.atoms_to(sid0, tspace, mt: true)
  s1.atoms_to(sid1, tspace, mt: true)
  a1.atoms_to(aid0, tspace, mt: true)
  a2.atoms_to(aid1, tspace, mt: true)
  a3.atoms_to(aid2, tspace, mt: true)
  a4.atoms_to(aid3, tspace, mt: true)

  scomps = s0.complement_set(tspace, mt: true)
  expect scomps == Set{aid0, aid1, aid3}

  scomps = s1.complement_set(tspace, mt: true)
  expect scomps == Set{aid2, aid3}

  acomps = a3.complement_set(tspace, mt: true)
  expect acomps == Set{sid1}

  acomps = a2.complement_set(tspace, mt: true)
  expect acomps == Set{sid0}

  acomps = a4.complement_set(tspace, mt: true)
  expect acomps == Set{sid0, sid1}
end

puts "OK in #{dt.total_milliseconds}ms"

# # acomps = a.complement_set(atoms)
# # pp acomps

# {% skip_file unless flag?(:tail) %}
# atoms = MySet(1).new
# endpoints = Utrie.mount(atoms, [
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(0))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(1))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(2))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(3))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(4))},
#   {Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(5))},
# ])
# pp endpoints
# Utrie.each_endpoint(atoms, Term.of(x: 3)) do |ep|
#   pp ep
# end

# {% if flag?(:test_xgraph) %}
#   atoms = MySet(1024).new

#   nletters = 30
#   nwords = 1000
#   maxwordlen = 20
#   nchoices = 300
#   nepochs = 1000

#   # Generate letters
#   alphabet = [] of Atom
#   (0...nletters).each do |letter|
#     alphabet << Atom.of("#{letter}")
#   end

#   # Generate rules (words)
#   words = {} of Atom => Array(Atom)

#   (0...nwords).each do
#     length = (1...maxwordlen).sample
#     word = alphabet.sample(length)
#     apex = Xgraph.mount(atoms, word)
#     unless words.put?(apex, word)
#       Log.debug { "generated duplicate word #{word}" }
#       next
#     end
#   end

#   nepochs.times do |epoch|
#     puts "Epoch #{epoch}/#{nepochs} (#{((epoch/nepochs) * 100).round(2)}%)"
#     # Pick N random words
#     choices = words.sample(nchoices)

#     expected = Set(Atom).new
#     pool = Set(Atom).new

#     choices.each do |word, letters|
#       expected << word
#       pool.concat(letters)
#     end

#     hit = Set(Atom).new
#     dt = Time.measure do
#       Xgraph.each_conjv(atoms, pool) do |conjv|
#         hit << conjv
#       end
#     end

#     expect expected.subset_of?(hit)

#     puts "OK in #{dt.total_milliseconds}ms!"
#   end
# {% end %}

# require "benchmark"

# Benchmark.ips do |x|
#   x.report("gen") do

# WWID.new
#   end
# end
# require "benchmark"


set = MySet.new(1024)
conid = WWID.new
origin = conid
aid0 = origin = origin.succ
aid1 = origin = origin.succ
aid2 = origin = origin.succ

pp! aid0
pp! aid1
pp! aid2

require "benchmark"

lock = Mutex.new

AppearanceRegistry.mount(set, nil, Term.of(:add, 1, 2, 3, 4, 5), aid0, mt: true)
AppearanceRegistry.mount(set, nil, Term.of(:sub, 1, 2), aid1, mt: true)
AppearanceRegistry.mount(set, nil, Term.of(:sub, "hello", 4), aid2, mt: true)

# _
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new] }, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# _dict
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new] }, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# (_)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0))] }, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# (_symbol)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new] }, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0, aid1, aid2}

# (add)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:add))] }, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid0}

# (sub)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, { [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))] }, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid1, aid2}

# (sub _)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, {
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1))],
}, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid1, aid2}

# (sub _number)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, {
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsNum.new],
}, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid1}

# (sub _string)
seen = Set(WWID).new
AppearanceRegistry.each_appearance(set, nil, {
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
  [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsStr.new],
}, mt: true) do |aid|
  lock.synchronize { seen << aid }
end
expect seen == Set{aid2}

puts "OK"
require "benchmark"

# Benchmark.ips do |x|
#   x.report("do it") do
# # 1000.times do
#  seen = Set(WWID).new
# AppearanceRegistry.each_appearance(set, nil, {
#   [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
#   [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsStr.new],
# }) do |aid|
#   lock.synchronize { seen << aid }
# end
# end
# end

#   end
# end
# p = Xgraph.mount(set, Deque{Atom.of("a"), Atom.of("b")})
# q = Xgraph.mount(set, Deque{Atom.of("a"), Atom.of("b"), Atom.of("c")})
# r = Xgraph.mount(set, Deque{Atom.of("b"), Atom.of("c")})
# pp! p
# pp! q
# pp! r

# Xgraph.each_conjv(set, Deque{Atom.of("a"), Atom.of("b"), Atom.of("c"), Atom.of("d")}) do |conjv|
#   pp conjv
# end

# sid0 = WWID.new
# sid1 = WWID.new
# SensorRegistry.register(set, nil, {Atom.of(Blake3.final("a")), Atom.of(Blake3.final("b")), Atom.of(Blake3.final("c"))}, sid0)
# SensorRegistry.register(set, nil, {Atom.of(Blake3.final("d"))}, sid1)
# apexes = {Atom.of(Blake3.final("a")), Atom.of(Blake3.final("b")), Atom.of(Blake3.final("c")), Atom.of(Blake3.final("d"))}
# require "benchmark"

#  n =Atomic.new(0)
# Benchmark.ips do |x|
#   x.report("speed") do
#     n.set(0)
# SensorRegistry.each_sensor(set, nil, apexes) do |s|
#   n.add(1)
# end
#   end
# end
# pp n

# pp set.@sets[0].size

# (0...10_000).each do |x|
#   BytesMultimap.add(set, :sensor_registry, "".to_slice, ('a'..'z').sample(16).to_readonly_slice(&.ord.to_u8))
# end

# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "1 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "2 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "3 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "4 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "5 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMultimap.add(set, :sensor_registry, "".to_slice, "6 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)


# require "benchmark"

# n = Atomic.new(0)
# Benchmark.ips do |x|
#   x.report("receive 10000 st") do
#     n.set(0)
#  BytesMultimap.complete(set, :sensor_registry, "".to_slice, "".to_slice, mt: false) do |comp|
#    n.add(1)
# end
#   end
#   x.report("receive 10000 mt") do
#     n.set(0)
#  BytesMultimap.complete(set, :sensor_registry, "".to_slice, "".to_slice) do |comp|
#    n.add(1)
# end
#   end
# end
# # pp atoms.size

# pp n
# # puts
# # pp atoms.size


