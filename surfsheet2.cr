require "./src/wirewright"
require "socket"

include Meridium

module Ax
  alias AxT = Proto::Token

  class NetworkError < Exception
  end

  class ProtocolError < Exception
  end
end

struct Ax::Response
  def initialize(@r : Proto::Reader, @opcode : Proto::Token, @done : Channel(Nil))
  end

  def handle(& : Proto::Reader, Proto::Token -> T) : T forall T
    yield @r, @opcode
  ensure
    @done.send(nil)
    @done.close
  end
end

class Ax::Client
  Log = ::Log.for(self)

  DEFAULT_LISTEN_TIMEOUT = 5.seconds

  enum State : UInt8
    Online
    Offline
    Suspended
    Closed
  end

  # :nodoc:
  def initialize(@bookings : Channel(Tspace::Meetable), @w : Proto::Writer)
    @subs = {} of WWID => Conn
    @responses = Channel(Response).new

    @acts = Deque(Activation).new
    @state = State::Online

    # This lock protects mainly @acts and @state. It is also a write lock for @w.
    # @w has its own lock but it is never synchronized with here in Client;
    # since we are already synchronized with @lock.
    @lock = Mutex.new
  end

  # Constructs a new client and spawns the associated fibers on top of *io*.
  # Runs the client's mainloop in the calling fiber. Returns the client state
  # after the mainloop has been terminated.
  #
  # Meetable objects can then book a meeting with the termspace using
  # the *bookings* channel. During a meeting, the object will be given
  # an opportunity to talk to the termspace.
  def self.mainloop(io : IO, bookings : Channel(Tspace::Meetable)) : State
    w = Proto::Writer.new(io)
    r = Proto::Reader.new(io)

    instance = new(bookings, w)

    spawn { instance.recvloop(r) }

    instance.mainloop
  end

  # Returns the current state.
  #
  # NOTE: This method is thread-safe.
  def state : State
    @lock.synchronize { @state }
  end

  private def halt(e : ProtocolError) : State
    Log.debug(exception: e) { "HALT: protocol error, switch to suspended state" }

    @lock.synchronize { @state = State::Suspended }
  end

  private def halt(e : NetworkError) : State
    Log.debug(exception: e) { "HALT: network error, switch to offline state" }

    @lock.synchronize do
      # Offline is a "dangerous" state because we will attempt to reconnect;
      # thus if the current state is anything but online, we better have that
      # instead of Offline. Who knows what could have happened; we definitely
      # don't want to auto-reconnect on top of that.
      if @state.online?
        @state = State::Offline
      end

      @state
    end
  end

  # "Quiet! I want to talk to the server."
  #
  # Only one caller can talk to the server at a time.
  private def tell(&) : Bool
    @lock.synchronize do
      raise NetworkError.new unless @state.online?

      yield

      true
    rescue e : ProtocolError
      halt(e)

      false
    rescue e : NetworkError
      halt(e)

      false
    end
  end

  # `tell`s all of *objects* **without flushing**.
  private def say(*objects) : Bool
    tell { @w << objects }
  end

  # Applies default handling logic for *opcode*.
  #
  # May raise `NetworkError` or `ProtocolError`.
  def handle(r : Proto::Reader, opcode : AxT) : Nil
    case opcode
    when .stp?
      sensor, appearance, stimulus = r.wwid, r.iwwid, r.term
      r.expect(:ovr)

      @acts << StimulusPresence.new(sensor, appearance, stimulus)
    when .sta?
      sensor, appearance = r.wwid, r.iwwid
      r.expect(:ovr)

      @acts << StimulusAbsence.new(sensor, appearance)
    when .srq?
      sensor, appearance = r.iwwid, r.wwid
      r.expect(:ovr)

      @acts << StimulusRequest.new(sensor, appearance)
    when .srs?
      sensor, appearance, stimulus = r.iwwid, r.iwwid, r.term
      r.expect(:ovr)

      @acts << StimulusResponse.new(sensor, appearance, stimulus)
    else
      r.err "unexpected token #{opcode}"
    end
  end

  # Waits for *keyword* while handling all other opcodes using the default
  # handling logic. Yields the response reader after consuming *keyword*.
  #
  # May raise `NetworkError` or `ProtocolError`.
  def handle(keyword : AxT, *, timeout = DEFAULT_LISTEN_TIMEOUT, & : Proto::Reader ->) : Nil
    loop do
      select
      when response = @responses.receive?
        raise NetworkError.new unless response

        response.handle do |r, opcode|
          if opcode == keyword
            yield r
          else
            handle(r, opcode)
          end
        end
      when timeout(timeout)
        raise NetworkError.new("timed out")
      end
    end
  end

  # :nodoc:
  def mainloop : State
    Log.debug { "client mainloop is running" }

    loop do
      while act = @acts.shift?
        next unless conn = @subs[act.receiver]?

        conn.receive(Face.new(self), act)
      end

      @w.flush

      select
      when response = @responses.receive?
        raise NetworkError.new unless response

        response.handle { |r, opcode| handle(r, opcode) }
      when meetable = @bookings.receive?
        unless meetable
          @state = State::Closed
          break
        end

        meetable.meet(Face.new(self))
      end

      @w.flush
    end

    @state
  rescue e : ProtocolError
    halt(e)
  rescue e : NetworkError
    halt(e)
  end

  # :nodoc:
  def recvloop(r : Proto::Reader) : Nil
    Log.debug { "client recvloop is running" }

    loop do
      opcode = r.token
      completed = Channel(Nil).new
      @responses.send(Ax::Response.new(r, opcode, completed))
      completed.receive
    end
  rescue e : ProtocolError
    halt(e)
  rescue e : NetworkError
    halt(e)
  ensure
    @responses.close
  end

  # :nodoc:
  def present?(conn : Conn, atoms : AtomSource) : BitList
    bitsize = nil

    tell do
      bitsize = 0

      # Send query to the server.
      @w.flush do |txn|
        txn << AxT::ASK
        atoms.each do |atom|
          txn << {AxT::HAS, atom}
          bitsize += 1
        end
        txn << AxT::OVR
      end

      handle(:ans) do |r|
        bitlist = r.bitlist
        r.expect(:ovr)

        unless bitsize == bitlist.size
          r.err "bitsize mismatch between ASK and ANS"
        end

        return bitlist
      end
    end

    unless bitsize
      bitsize = 0
      atoms.each { bitsize += 1 }
    end

    Log.debug { "network or protocol error, return #{bitsize} zero(s)" }

    BitList.zeros(bitsize)
  end

  # :nodoc:
  def subscribe(conn : Conn) : Nil
    return unless @subs.put?(conn.conid, conn)

    say AxT::SUB, conn.conid, AxT::OVR
  end

  # :nodoc:
  def unsubscribe(conn : Conn) : Nil
    return unless @subs.delete(conn.conid)

    say AxT::UNS, conn.conid, AxT::OVR
  end

  # :nodoc:
  def send(conn : Conn, act : Activation::StimulusPresence) : Nil
    say AxT::STP, act.sensor, act.appearance, act.stimulus, AxT::OVR
  end

  # :nodoc:
  def send(conn : Conn, act : Activation::StimulusAbsence) : Nil
    say AxT::STA, act.sensor, act.appearance, AxT::OVR
  end

  # :nodoc:
  def send(conn : Conn, act : Activation::StimulusRequest) : Nil
    say AxT::SRQ, act.sensor, act.appearance, AxT::OVR
  end

  # :nodoc:
  def send(conn : Conn, act : Activation::StimulusResponse) : Nil
    say AxT::SRS, act.sensor, act.appearance, act.stimulus, AxT::OVR
  end

  # :nodoc:
  def transaction(conn : Conn, & : AtomSubmit, AtomSubmit ->) : Nil
    return unless state.online?

    open = false
    openlock = Mutex.new

    # WARNING: these procs are called from multiple fibers.
    add = AtomSubmit.new do |atom|
      openlock.synchronize do
        next if open

        say AxT::TXN, conn.conid
        open = true
      end

      say AxT::ADD, atom
    end

    del = AtomSubmit.new do |atom|
      openlock.synchronize do
        next if open

        say AxT::TXN, conn.conid
        open = true
      end

      say AxT::DEL, atom
    end

    yield add, del

    if open
      say AxT::OVR
    end
  end

  # :nodoc:
  struct Face
    include Tspace

    def initialize(@client : Client)
    end

    def present?(conn : Conn, atoms : AtomSource) : BitList
      @client.present?(conn, atoms)
    end

    def send(conn : Conn, act : Activation) : Nil
      @client.send(conn, act)
    end

    def transaction(conn : Conn, & : AtomSubmit, AtomSubmit ->) : Nil
      @client.transaction(conn) { |add, del| yield add, del }
    end

    def subscribe(conn : Conn) : Nil
      @client.subscribe(conn)
    end

    def unsubscribe(conn : Conn) : Nil
      @client.unsubscribe(conn)
    end
  end
end

class Ax::Session
  Log = ::Log.for(self)

  @@id = Atomic(UInt32).new(0u32)

  @id : UInt32

  def initialize(@server : Server, @io : IO)
    @id = @@id.add(1, :relaxed)

    @r = Proto::Reader.new(@io)
    @w = Proto::Writer.new(@io)

    @conids = Set(WWID).new
  end

  # Gives write access to this session's socket to the block.
  def write(& : Proto::Writer -> T) : T forall T
    yield @w
  end

  # :nodoc:
  struct AskEE
    include Enumerable(Atom)

    def initialize(@r : Ax::Proto::Reader)
    end

    def each(& : Atom ->)
      loop do
        case @r.token
        when .has?
          yield @r.atom
        when .ovr?
          break
        else
          @r.err "unexpected trailing data"
        end
      end
    end
  end

  private def handle(opcode : AxT) : Nil
    Log.trace { "#{self}: handle opcode #{opcode}" }

    case opcode
    when .sub?
      conid = @r.conid
      @r.expect(:ovr)

      unless @conids.add?(conid)
        Log.debug { "#{self}: attempt to subscribe twice for #{conid}" }
        return
      end

      @server.routes[conid] = self
    when .uns?
      conid = @r.conid
      @r.expect(:ovr)

      unless @conids.delete(conid)
        Log.debug { "#{self}: attempt to unsubscribe twice for #{conid}" }
        return
      end

      @server.routes.delete(conid)
    when .ask?
      bits = @server.atoms.present?(AskEE.new(@r))

      @w.send(AxT::ANS, bits, AxT::OVR)
    when .txn?
      conid = @r.conid

      adds = 0
      dels = 0

      loop do
        case @r.token
        when .add?
          @server.atoms.incref(conid, @r.atom)
          adds += 1
        when .del?
          @server.atoms.decref(conid, @r.atom)
          dels += 1
        when .ovr?
          break
        else
          @r.err "unexpected trailing data"
        end
      end

      Log.trace { "#{self}: #{conid}: (+) #{adds} (-) #{dels} atom ref(s)" }
    when .stp?
      sensor, appearance, payload = @r.wwid, @r.iwwid, @r.data
      @r.expect(:ovr)

      Log.trace { "#{self}: relay STP to #{sensor.conid}" }

      @server.routes.find(sensor.conid) do |session|
        session.write &.send(AxT::STP, sensor, appearance, payload, AxT::OVR)
      end
    when .sta?
      sensor, appearance = @r.wwid, @r.iwwid
      @r.expect(:ovr)

      Log.trace { "#{self}: relay STA to #{sensor.conid}" }

      @server.routes.find(sensor.conid) do |session|
        session.write &.send(AxT::STA, sensor, appearance, AxT::OVR)
      end
    when .srq?
      sensor, appearance = @r.iwwid, @r.wwid
      @r.expect(:ovr)

      Log.trace { "#{self}: relay SRQ to #{appearance.conid}" }

      @server.routes.find(appearance.conid) do |session|
        session.write &.send(AxT::SRQ, sensor, appearance, AxT::OVR)
      end
    when .srs?
      sensor, appearance, payload = @r.iwwid, @r.iwwid, @r.data
      @r.expect(:ovr)

      Log.trace { "#{self}: relay SRS to #{sensor.conid}" }

      @server.routes.find(appearance.conid) do |session|
        session.write &.send(AxT::SRS, sensor, appearance, payload, AxT::OVR)
      end
    else
      @r.err "unexpected opcode"
    end
  end

  # Closes the underlying IO, causing this session to end.
  def shutdown : Nil
    Log.trace { "#{self}: shutdown" }

    @io.close
  end

  # Starts the session listen loop.
  def listen : Nil
    Log.trace { "#{self}: listen" }

    loop do
      handle(@r.token)
    end
  ensure
    Log.trace { "#{self}: cleanup after listen" }

    @io.close
    @conids.each do |conid|
      @server.routes.delete(conid)
      swept = @server.atoms.sweep(conid)
      Log.trace { "#{self}: #{conid}: clear (-) #{swept} atom(s), 1 sub(s)" }
    end
  end

  def to_s(io)
    io << "session#" << @id
  end
end

class Ax::Server
  Log = ::Log.for(self)

  getter routes
  getter atoms

  def initialize(@socket : Socket)
    @routes = SyncHash(WWID, Session).new
    @atoms = BucketizedAtomMultiset(128).new
    @sessions = SyncHash(Session, Bool).new
  end

  def self.control(&fn : -> Socket) : {->, ->}
    socket = nil
    lock = Mutex.new

    start = -> do
      lock.synchronize do
        if socket
          Log.debug { "control: will not start several times in a row" }
          next
        end

        socket = socket_ = fn.call
        server = Server.new(socket_)
        spawn server.listen
      end
    end

    stop = -> do
      lock.synchronize do
        unless socket_ = socket
          Log.debug { "control: will not stop a server that is not running" }
          next
        end

        socket_.close
        socket = nil
      end
    end

    {start, stop}
  end

  def stats
    {routes: @routes.size, atoms: @atoms.approx_size}
  end

  def listen : Nil
    Log.info { "server is accepting clients on #{@socket}" }

    while client = @socket.accept?
      Ax.configure(client)

      session = Session.new(self, client)

      @sessions[session] = true

      spawn do
        session.listen
      rescue e : NetworkError
        Log.trace(exception: e) { "session ended" }
      rescue e : Exception
        Log.debug(exception: e) { "session crashed" }
      ensure
        @sessions.delete(session)
      end
    end
  ensure
    Log.trace { "server is not accepting clients anymore" }

    @socket.close
    @sessions.each { |session, _| session.shutdown }
    @sessions.clear
  end
end

module Ax::Proto
  MAX_DATA_BYTESIZE = 8 * 1024 # 8 KiB

  enum Token : UInt8
    ASK
    HAS
    ANS
    SUB
    UNS
    TXN
    ADD
    DEL
    STP
    STA
    SRQ
    SRS
    OVR
  end

  # Callers should expect all methods to raise either `IO::Error` in case the IO
  # is broken or reading fails; or `Proto::Error` in case there is something
  # wrong with the format of things being exchanged. No other error should be
  # expected.
  struct Reader
    def initialize(@io : IO)
    end

    def err(*args, **kwargs)
      raise ProtocolError.new(*args, **kwargs)
    end

    def u32 : UInt32
      @io.read_bytes(UInt32, IO::ByteFormat::BigEndian)
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def token : Token
      byte = @io.read_byte || raise IO::EOFError.new

      Token.from_value?(byte) || err "expected a token"
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def wwid : WWID
      scratch = uninitialized UInt8[WWID::BYTESIZE]

      @io.read_fully(scratch.to_slice)

      WWID.from_slice_be(scratch.to_slice)
    rescue e : WWID::ParseError
      err "invalid WWID", cause: e
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def iwwid : IWWID
      scratch = uninitialized UInt8[IWWID::BYTESIZE]

      @io.read_fully(scratch.to_slice)

      IWWID.from_slice_be(scratch.to_slice)
    rescue e : WWID::ParseError
      err "invalid IWWID", cause: e
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def atom : Atom
      scratch = uninitialized UInt8[Atom::BYTESIZE]

      @io.read_fully(scratch.to_slice)

      Atom.from_slice_be(scratch.to_slice)
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def data : Bytes
      bytesize = u32
      if bytesize > MAX_DATA_BYTESIZE
        err "data bytesize limit exceeded"
      end

      slice = Bytes.new(bytesize)
      @io.read_fully(slice)
      slice
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def text : String
      bytesize = u32
      if bytesize > MAX_DATA_BYTESIZE
        err "data bytesize limit exceeded"
      end

      @io.read_string(bytesize)
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def bitlist : BitList
      bitsize, buckets = u32, data

      bitlist = BitList.new(bitsize)
      reader = BitReader.new(buckets)
      while bitlist.size < bitsize && (bit = reader.consume?)
        bitlist << (bit == 1)
      end

      bitlist
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def conid : WWID
      wwid.conid
    end

    def term : Term
      ML.term(text)
    rescue e : ML::SyntaxError
      err "invalid or malformed term", cause: e
    end

    def expect(expected : Token) : Nil
      unless token == expected
        err "unexpected token, expected: #{expected}"
      end
    end
  end

  class Writer
    struct Txn
      def initialize(@io : IO)
      end

      def <<(object : Token) : self
        @io.write_byte(object.value)

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      def <<(object : UInt32) : self
        @io.write_bytes(object, IO::ByteFormat::BigEndian)

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      def <<(object : BitList) : self
        writer = MutBitWriter.new
        object.each do |bit|
          writer << (bit ? 1u8 : 0u8)
        end

        @io.write_bytes(writer.bitsize.to_u32, IO::ByteFormat::BigEndian)
        @io.write_bytes(writer.bytesize.to_u32, IO::ByteFormat::BigEndian)

        writer.each_byte { |byte| @io.write_byte(byte) }

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      def <<(object : Bytes) : self
        @io.write_bytes(object.size.to_u32, IO::ByteFormat::BigEndian)
        @io.write(object)

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      def <<(object : WWID | IWWID) : self
        object.to_slice_be { |slice| @io.write(slice) }

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      def <<(object : Term) : self
        bytesize = ML.compact_bytesize(object)

        @io.write_bytes(bytesize, IO::ByteFormat::BigEndian)

        ML.compact(@io, object)

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      def <<(object : Atom) : self
        object.to_slice_be { |slice| @io.write(slice) }

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      def <<(objects : Tuple) : self
        objects.each { |object| self << object }

        self
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end
    end

    def initialize(@io : IO)
      @lock = Mutex.new
    end

    def sync(& : Txn ->) : Nil
      @lock.synchronize { yield Txn.new(@io) }
    end

    def flush(& : Txn ->) : Nil
      sync do |txn|
        yield txn

        @io.flush
      end
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def flush
      sync { @io.flush }
    rescue e : IO::Error
      raise NetworkError.new(cause: e)
    end

    def send(*objects) : Nil
      flush do |txn|
        objects.each { |object| txn << object }
      end
    end

    def <<(object) : self
      sync { |txn| txn << object }

      self
    end
  end
end

struct SyncHash(K, V)
  @hash = {} of K => V
  @lock = Mutex.new

  def size : Int32
    @lock.synchronize { @hash.size }
  end

  def each(& : K, V ->)
    @lock.synchronize do
      @hash.each { |key, value| yield key, value }
    end
  end

  def []=(key : K, value : V) : self
    @lock.synchronize { @hash[key] = value }

    self
  end

  def delete(key : K) : self
    @lock.synchronize { @hash.delete(key) }

    self
  end

  def find(key : K, & : V -> T) : T forall T
    @lock.synchronize do
      return unless value = @hash[key]?
      yield value
    end
  end

  def clear
    @lock.synchronize { @hash.clear }
  end
end

module Ax
  struct AtomMultiset
    @atoms = {} of Atom => Hash(WWID, UInt32)
    @lock = Mutex.new

    def size : Int32
      @lock.synchronize { @atoms.size }
    end

    def present?(atom : Atom) : Bool
      @lock.synchronize { @atoms.has_key?(atom) }
    end

    def present?(atoms : Enumerable(Atom)) : BitList
      bits = BitList.new
      atoms.each do |atom|
        bits << present?(atom)
      end
      bits
    end

    def incref(referrer : WWID, atom : Atom) : Nil
      @lock.synchronize do
        refs = @atoms.put_if_absent(atom) { {} of WWID => UInt32 }
        refs[referrer] = (refs[referrer]? || 0u32) + 1
      end
    end

    def decref(referrer : WWID, atom : Atom) : Nil
      @lock.synchronize do
        return unless refs = @atoms[atom]?
        return unless refcount = refs[referrer]?

        unless refcount == 1
          refs[referrer] = refcount - 1
          return
        end

        refs.delete(referrer)
        if refs.empty?
          @atoms.delete(atom)
        end
      end
    end

    def sweep(referrer : WWID) : Int32
      @lock.synchronize do
        size0 = @atoms.size

        @atoms.reject! do |_, referrers|
          !!referrers.delete(referrer) && referrers.empty?
        end

        size0 - @atoms.size
      end
    end

    def clear : Nil
      @lock.synchronize { @atoms.clear }
    end
  end

  struct BucketizedAtomMultiset(N)
    def initialize
      @buckets = Pointer(AtomMultiset).malloc(N) { AtomMultiset.new }
    end

    private def bucket(atom : Atom) : AtomMultiset
      @buckets[atom.blk0 % N]
    end

    def approx_size : Int32
      @buckets.to_slice(N).sum(&.size)
    end

    def present?(atoms : Enumerable(Atom)) : BitList
      bits = BitList.new
      atoms.each do |atom|
        bits << bucket(atom).present?(atom)
      end
      bits
    end

    def incref(referrer : WWID, atom : Atom) : Nil
      bucket(atom).incref(referrer, atom)
    end

    def decref(referrer : WWID, atom : Atom) : Nil
      bucket(atom).decref(referrer, atom)
    end

    def sweep(referrer : WWID) : Int32
      wg = WaitGroup.new
      swept = Atomic(Int32).new(0)

      (0...N).in_subranges_of(N//System.cpu_count.to_i) do |subrange|
        Log.trace { "#{referrer}: sweep: spawn cleaner on subrange #{subrange}" }

        wg.spawn do
          subrange.each do |index|
            swept_ = @buckets[index].sweep(referrer)
            swept.add(swept_, :relaxed)
          end
        end
      end

      wg.wait

      swept.get(:relaxed)
    end

    def clear : Nil
      @buckets.to_slice(N).each(&.clear)
    end
  end
end

module Ax
  def self.configure(io : Socket)
    io.buffer_size = 2048
  end

  def self.configure(io)
  end
end

class Ax::Mediator
  Log = ::Log.for(self)

  def initialize(&@connect : -> Socket)
    @state = Atomic(Client::State).new(:offline)
    @bookings = Channel(Tspace::Meetable).new
    @subscribers = SyncHash(Conn, Bool).new
  end

  def state : Client::State
    @state.get(:relaxed)
  end

  def book : (Tspace::Meetable ->)
    ->@bookings.send(Tspace::Meetable)
  end

  def subscribe(conn : Conn) : Nil
    @subscribers[conn] = true
  end

  def unsubscribe(conn : Conn) : Nil
    @subscribers.delete(conn)
  end

  def clear : Nil
    @bookings.close
    @subscribers.clear
  end

  def connect : Nil
    reconnects = 0

    loop do
      Log.debug { "connecting to server" }

      # Connect
      begin
        socket = @connect.call
      rescue e : Socket::ConnectError
        reconnects += 1
        nap = (200 * 2**reconnects).milliseconds
        if nap > 30.seconds
          nap = 30.seconds
        end

        Log.debug(exception: e) { "connection failed, nap=#{nap.total_seconds.round(2)}s" }

        start0 = Time.monotonic

        # Catch any bookings in the interim and ignore them. We'll notify them
        # later on when we're able to connect. Assuming whomever is booking is
        # subscribed to our notifications, that is!
        loop do
          select
          when meetable = @bookings.receive?
            return unless meetable

            start1 = Time.monotonic
            nap -= (start1 - start0)
            start0 = start1

            Log.debug { "woken up by #{meetable}, ignore; nap=#{nap.total_seconds.round(2)}s" }
          when timeout(nap)
            break
          end
        end

        next
      end

      reconnects = 0

      Ax.configure(socket)

      @state.set(:online, :relaxed)

      # Notify all of our subscribers that there is a connection now. Note that
      # this almost certainly will trigger a send to @bookings. We must be careful
      # since the mainloop isn't running yet; nor are we able to handle @bookings.
      spawn do
        @subscribers.each(&.online)
      end

      begin
        # Start client mainloop
        state = Ax::Client.mainloop(socket, @bookings)

        @state.set(state, :relaxed)

        # See why it stopped.
        case state
        in .online?
          raise "BUG: online state after mainloop ended"
        in .offline?
          @subscribers.each(&.offline)

          Log.debug { "client exited with offline state, trying to reconnect" }
        in .suspended?
          Log.debug { "client exited with suspended state, terminating reconnect loop" }
          break
        in .closed?
          return
        end
      ensure
        socket.close
      end
    end
  ensure
    Log.debug { "awaiting manual reconnect in state=#{state}" }
  end
end

class SyncInMemoryTspace
  include Tspace

  def initialize
    @atoms = {} of Atom => UInt32
    @routes = {} of WWID => Conn
    @lock = Mutex.new
  end

  def book : (Tspace::Meetable ->)
    ->(meetable : Tspace::Meetable) { meetable.meet(self) }
  end

  def present?(atom : Atom) : Bool
    @lock.synchronize { @atoms.has_key?(atom) }
  end

  def present?(conn : Conn, atoms : AtomSource) : BitList
    answer = BitList.new
    atoms.each { |atom| answer << present?(atom) }
    answer
  end

  def add(atom : Atom) : Nil
    @lock.synchronize do
      @atoms[atom] = (@atoms[atom]? || 0u32) + 1
    end
  end

  def delete(atom : Atom) : Nil
    @lock.synchronize do
      return unless tally = @atoms[atom]?

      if tally == 1
        @atoms.delete(atom)
      else
        @atoms[atom] = tally - 1
      end
    end
  end

  def transaction(conn : Conn, & : AtomSubmit, AtomSubmit ->) : Nil
    yield ->add(Atom), ->delete(Atom)
  end

  def subscribe(conn : Conn) : Nil
    @lock.synchronize { @routes[conn.conid] = conn }
  end

  def unsubscribe(conn : Conn) : Nil
    @lock.synchronize { @routes.delete(conn.conid) }
  end

  def send(conn : Conn, act : Activation) : Nil
    if receiver = @lock.synchronize { @routes[act.receiver]? }
      receiver.receive(self, act)
    end
  end
end

{% skip_file %}
tspace = SyncInMemoryTspace.new
conn = Conn.new(WWID.new, tspace.book) { |c| Log.notice { "#{c.view.dict_multisets}" } }
conn.summon
conn.transaction do |txn|
  txn.put(0u32, Appearance.new(Term.of(100)))
  txn.put(1u32, Sensor.new(Term.of(:x_number)))
end

{% skip_file %}
# - Move clean(ish) stuff to src/
# - Try using nodelay TCP/IP from different processes, see how fast it performs
# - Commit
# - TODO: if secret is not nil we should use aes with secret hash as key to encrypt, use AES256

mediator = Ax::Mediator.new { UNIXSocket.new("/tmp/surfnet.sock") }

MT.spawn { mediator.connect }

sleep 5.seconds

start, stop = Ax::Server.control { UNIXServer.new("/tmp/surfnet.sock") }
MT.spawn do
  start.call
end

sleep 5.seconds

stop.call

sleep 5.seconds

#  ->(m : Tspace::Meetable) { bookings.send(m) }
conn = Conn.new(WWID.new, mediator.book) { |c| Log.notice { "#{c.view.dict_multisets}" } }
mediator.subscribe(conn)
conn.transaction do |txn|
  txn.put(0u32, Appearance.new(Term.of(100)))
  txn.put(1u32, Sensor.new(Term.of(:x_number)))
end

sleep 5.seconds

start.call

sleep 5.seconds

mediator.clear

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
