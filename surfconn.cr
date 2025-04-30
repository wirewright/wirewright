require "./src/wirewright"

include Meridium

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
