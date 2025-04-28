require "./src/wirewright"
require "benchmark"

alias Ubase = Meridium::Ubase
alias WWID = Meridium::WWID
# LATER: protect BytesMM, and Xgraph from lying sets by emitting two
# types of sanity check queries:
# - Since we have Entity already we can introduce NegXgraph NegSensorRegistry etc.,
#   elements of which are always absent (else the set is insane.) This tests how sanely
#   the set generates negative responses.
# - We can also do randomized lookback -- i.e. remember a random `true` query from the past
#   and emit it sometime in the future. This tests how sanely the set generates positive
#   reponses -- if it says `false` for something that was `true` before and we're working
#   on the consequences of that right now, we halt.
# These should occur on IAtomsPresent implementor level.

class MySet
  include Meridium::IAtomAppend
  include Meridium::IAtomsPresent

  def initialize(@n : Int32)
    @sets = Slice(Set(Meridium::Atom)).new(@n) { Set(Meridium::Atom).new }
    @locks = Slice(Mutex).new(@n) { Mutex.new }
  end

  def <<(atom : Meridium::Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket] << atom
    end
  end

  def present?(atom : Meridium::Atom)
    bucket = atom.@blk0 % @n
    @locks[bucket].synchronize do
      @sets[bucket].includes?(atom)
    end
  end

  def size
    @sets.sum(&.size)
  end

  def present?(objects : Enumerable(T), & : T -> Meridium::Atom | Enumerable(Meridium::Atom)) : BitList forall T
    answer = BitList.new

    objects.each do |object|
      ee = yield object

      unless ee.is_a?(Enumerable(Meridium::Atom))
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

wg = WaitGroup.new(1)
ctx = Fiber::ExecutionContext::MultiThreaded.new("main", 4)
ctx.spawn do
  tspace = MySet.new(4096)

  trunk = WWID.new
  aid0 = trunk = trunk.succ
  aid1 = trunk = trunk.succ
  aid2 = trunk = trunk.succ
  aid3 = trunk = trunk.succ
  sid0 = trunk = trunk.succ
  sid1 = trunk = trunk.succ

  s0 = Meridium::Sensor.new(Term.of(type: "pixel", x: {:"%any", 0, 1}, y: :y_number))
  s1 = Meridium::Sensor.new(Term.of(type: "pixel", color: {:_, :_, 255}))

  a1 = Meridium::Appearance.new(Term.of(type: "pixel", x: 0, y: 100, color: {255, 0, 0}))
  a2 = Meridium::Appearance.new(Term.of(type: "pixel", x: 1, y: 200, color: {0, 255, 0}))
  a3 = Meridium::Appearance.new(Term.of(type: "pixel", x: 2, y: 300, color: {0, 0, 255}))
  a4 = Meridium::Appearance.new(Term.of(type: "pixel", x: 0, y: 400, color: {255, 0, 255}))

  dt = Time.measure do
    s0.atoms_to(sid0, tspace)
    s1.atoms_to(sid1, tspace)
    a1.atoms_to(aid0, tspace)
    a2.atoms_to(aid1, tspace)
    a3.atoms_to(aid2, tspace)
    a4.atoms_to(aid3, tspace)

    scomps = s0.complement_set(tspace)
    expect scomps == Set{aid0, aid1, aid3}

    scomps = s1.complement_set(tspace)
    expect scomps == Set{aid2, aid3}

    acomps = a3.complement_set(tspace)
    expect acomps == Set{sid1}

    acomps = a2.complement_set(tspace)
    expect acomps == Set{sid0}

    acomps = a4.complement_set(tspace)
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

  lock = Mutex.new

  Meridium::AppearanceRegistry.mount(set, nil, Term.of(:add, 1, 2, 3, 4, 5), aid0)
  Meridium::AppearanceRegistry.mount(set, nil, Term.of(:sub, 1, 2), aid1)
  Meridium::AppearanceRegistry.mount(set, nil, Term.of(:sub, "hello", 4), aid2)

  # _
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [[Ubase::Begin.new]]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid0, aid1, aid2}

  # _dict
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [[Ubase::Begin.new, Ubase::IsDict.new]]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid0, aid1, aid2}

  # (_)
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [[Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0))]]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid0, aid1, aid2}

  # (_symbol)
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [[Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new]]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid0, aid1, aid2}

  # (add)
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [[Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:add))]]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid0}

  # (sub)
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [[Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))]]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid1, aid2}

  # (sub _)
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [
    [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
    [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1))],
  ]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid1, aid2}

  # (sub _number)
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [
    [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
    [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsNum.new],
  ]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid1}

  # (sub _string)
  seen = Set(WWID).new
  Meridium::AppearanceRegistry.each_appearance(set, nil, [
    [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
    [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsStr.new],
  ]) do |aid|
    lock.synchronize { seen << aid }
  end
  expect seen == Set{aid2}

  puts "OK"

  Benchmark.ips do |x|
    x.report("do it") do
      seen = Set(WWID).new
      Meridium::AppearanceRegistry.each_appearance(set, nil, [
        [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(0)), Ubase::IsSym.new, Ubase::Literal.new(Term.of(:sub))],
        [Ubase::Begin.new, Ubase::IsDict.new, Ubase::At.new(Term.of(1)), Ubase::IsStr.new],
      ]) do |aid|
        lock.synchronize { seen << aid }
      end
    end
  end
    ensure
      wg.done
    end
wg.wait
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
#   BytesMM.add(set, :sensor_registry, "".to_slice, ('a'..'z').sample(16).to_readonly_slice(&.ord.to_u8))
# end

# # BytesMM.add(set, :sensor_registry, "".to_slice, "1 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)
# # BytesMM.add(set, :sensor_registry, "".to_slice, "2 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMM.add(set, :sensor_registry, "".to_slice, "3 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)
# # BytesMM.add(set, :sensor_registry, "".to_slice, "4 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMM.add(set, :sensor_registry, "".to_slice, "5 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?".to_slice)
# # BytesMM.add(set, :sensor_registry, "".to_slice, "6 Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_slice)

# require "benchmark"

# n = Atomic.new(0)
# Benchmark.ips do |x|
#   x.report("receive 10000 st") do
#     n.set(0)
#  BytesMM.complete(set, :sensor_registry, "".to_slice, "".to_slice, mt: false) do |comp|
#    n.add(1)
# end
#   end
#   x.report("receive 10000 mt") do
#     n.set(0)
#  BytesMM.complete(set, :sensor_registry, "".to_slice, "".to_slice) do |comp|
#    n.add(1)
# end
#   end
# end
# # pp atoms.size

# pp n
# # puts
# # pp atoms.size
