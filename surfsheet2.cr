require "./src/wirewright"

include Meridium

struct Fooze
  include IConn

  def conid : WWID
  end

  def online : Nil
  end

  def offline : Nil
  end

  def receive(tspace : Tspace, act : Activation) : Nil
  end

  def meet(tspace : Tspace) : Nil
  end
end

# tspace = AxisTspace.new { TCPSocket.new("0.0.0.0", 9810) }
# MT.spawn { tspace.connect }

# sleep
{% skip_file %}
tspace = SyncInMemoryTspace.new
conn = Conn.new(WWID.new, tspace.book) { |c| Log.notice { "#{c.view.dict_multisets}" } }
conn.summon
conn.transaction do |txn|
  txn.put(0u32, Appearance.new(Term.of(100)))
  txn.put(1u32, Sensor.new(Term.of(:x_number)))
end

{% skip_file %}

axt = AxisTspace.new { UNIXSocket.new("/tmp/surfnet.sock") }

MT.spawn { axt.connect }

sleep 5.seconds

start, stop = Ax::Server.control { UNIXServer.new("/tmp/surfnet.sock") }
MT.spawn do
  start.call
end

sleep 5.seconds

stop.call

sleep 5.seconds

#  ->(m : Tspace::Meetable) { bookings.send(m) }
conn = Conn.new(WWID.new, axt.book) { |c| Log.notice { "#{c.view.dict_multisets}" } }
axt.subscribe(conn)
conn.transaction do |txn|
  txn.put(0u32, Appearance.new(Term.of(100)))
  txn.put(1u32, Sensor.new(Term.of(:x_number)))
end

sleep 5.seconds

start.call

sleep 5.seconds

axt.clear

sleep
{% skip_file %}

MT.spawn do
  server = Ax::Server.new(socket: UNIXServer.new("/tmp/surfsheet.sock"))
  stopped = Channel(Nil).new

  spawn do
    Log.info { "stats: #{server.stats}" }

    loop do
      select
      when stopped.receive? # nil
        break
      when timeout(16.seconds)
        Log.info { "stats: #{server.stats}" }
      end
    end
  end

  Process.on_terminate do |reason|
    if reason.interrupted?
      server.cleanup
      stopped.close
    end
    Process.exit
  end

  server.mainloop
end

sleep 1.second

socket = UNIXSocket.new("/tmp/surfsheet.sock")
socket.buffer_size = 2048

# socket = IO::Hexdump.new(socket, output: STDERR, write: true)
# sockets.send(socket)
bookings = Channel(Tspace::Meetable).new
client = Ax::Client.spawn(socket, bookings)
conn = Conn.new(WWID.new, ->(m : Tspace::Meetable) { bookings.send(m) }) { |c| Log.notice { "#{c.view.dict_multisets}" } }
conn.summon
conn.transaction do |txn|
  txn.put(2u32, Sensor.new(Term.of(:x_number))) # , relook: 2.seconds))
end
sleep 1.second
conn.transaction do |txn|
  txn.put(0u32, Appearance.new(Term.of(100)))
end
# sleep 3.seconds
# conn.transaction do |txn|
#   txn.delete(0u32)
#   txn.delete(2u32)
# end
# sleep 3.seconds
# conn.transaction do |txn|
#   txn.put(0u32, Appearance.new(Term.of(100)))
# end
# sleep 3.seconds
# conn.transaction do |txn|
#   txn.put(2u32, Sensor.new(Term.of(:x_number)))
# end

# sleep 3.seconds
# conn.dismiss
sleep 3.seconds
socket.close
# ts.response(0u32)
# sleep 3.seconds
# conn.dismiss
# messages.send({conn.conid, Activation::StimulusPresence.new(WWID.new, IWWID.new, Term.of(123))})
sleep
