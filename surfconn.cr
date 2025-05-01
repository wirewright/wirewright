require "./src/wirewright"

alias Mm = Meridium

# mset = Mm::SyncAtomMultiset.new
# sam = Mm::PresenceRedirectAtomSet.new(Mm::CheckedPresence.new(mset), mset)
# pp sam.present?({"a", "b", "c"}) { |x| Mm::Atom.of(x) }
# pp sam

# {% skip_file %}

class MyTspace
  include Mm::Tspace

  # can be called from multiple fibers
  def present?(atoms : AtomSource) : BitList
    lst = BitList.new
    atoms.each do
      # lst << true
      # lst << {true, false}.sample
      lst << false
    end
    lst
  end

  # can be called from multiple fibers
  def transaction(& : Submit, Submit ->) : Nil
    adds = [] of Mm::Atom
    dels = [] of Mm::Atom
    lock = Mutex.new

    add = Submit.new do |atom|
      lock.synchronize { adds << atom }
    end

    del = Submit.new do |atom|
      lock.synchronize { dels << atom }
    end

    yield add, del

    Log.notice { "(+) #{adds.size} (-) #{dels.size} atom(s)" }
  end

  # can be called from multiple fibers
  def connect(conid : Mm::WWID, &recv : Mm::Activation ->) : {Subscribe, Unsubscribe}
    sub = Subscribe.new do
      Log.notice { "Subscribe to #{conid}" }
    end

    unsub = Unsubscribe.new do
      Log.notice { "Unsubscribe from #{conid}" }
    end

    {sub, unsub}
  end

  # can be called from multiple fibers
  def send(conid : Mm::WWID, act : Mm::Activation) : Nil
    Log.notice { "Send #{act} to #{conid}" }
  end

  # can be called from multiple fibers
  def register(conn : Mm::Conn) : Nil
    Log.notice { "Register #{conn.conid}" }
  end

  # can be called from multiple fibers
  def unregister(conn : Mm::Conn) : Nil
    Log.notice { "Unregister #{conn.conid}" }
  end
end

tspace = MyTspace.new
conn = Mm::Conn.new(tspace, ->(conn : Mm::Conn) { puts "Wake Up!!" })
# We can use tspace quietly but register()/unregister() allows to subscribe to
# e.g. reconnects.
tspace.register(conn)
conn.summon
(0...100).each_with_index do |n, i|
  conn[i.to_u32] = Mm::Appearance.new(Term.of(n))
end
conn.clear

{% skip_file %}
MT.spawn do
  set = SyncBucketedAtomMultiset(1024).new
  chat = SyncInMemoryChat(Activation).new
  n = Atomic(Int32).new(0)
  conn = Conn.new(set, chat) do |c|
    # pp c.view.dict_multisets
    if n.add(1) % 1000 == 0
      Log.notice { "#{n}" }
    end
  end
  conn.summon

  conn[0] = Sensor.new(Term.of(:+, :a_number, :b_number), relook: nil)
  # try to trigger races bugs etc
  spawn do
    (0...100_000).each do |n|
      conn[1] = Appearance.new(Term.of(:+, n, n))
    end
  end
  spawn do
    (100_000...200_000).each do |n|
      conn[2] = Appearance.new(Term.of(:+, n, n))
    end
  end
  # conn.each do |slot, surface|
  #   puts "Conn has #{slot} #{surface}"
  # end

  # conn.clear
end
sleep
