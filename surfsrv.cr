require "./src/wirewright"
require "socket"

record AtomIdentity, digest : Bytes

# A `DecayingAtomMultiset` is split into a number of buckets. Each bucket
# maintains its own part of the multiset & has an associated cleaner fiber
# which cleans up decayed atoms periodically.
class DecayingAtomMultisetBucket
  def initialize(@ord : Int32, @lifespan : Time::Span)
    @atoms = {} of AtomIdentity => {UInt32, Time}
    @atoms_lock = Mutex.new

    @cleaner = Channel(Nil).new

    spawn { cleaner }
  end

  private def cleaner : Nil
    Log.info { "cleaner loop of bucket #{@ord} will run every #{@lifespan.total_seconds}s" }

    while true
      select
      when @cleaner.receive? # nil
        Log.info { "bucket #{@ord} cleaner stopped" }
      when timeout(@lifespan)
        now = Time.utc

        Log.info { "bucket #{@ord} cleaner: it's cleaning time" }

        delta = @atoms_lock.synchronize do
          size0 = @atoms.size

          @atoms.transform_values! do |(refs, touched)|
            if now - touched >= @lifespan
              {refs - 1, touched}
            else
              {refs, touched}
            end
          end

          @atoms.reject! { |_, (refs, _)| refs.zero? }

          size1 = @atoms.size
          size0 - size1
        end

        Log.info { "bucket #{@ord} cleaner: cleaned #{delta} atom(s)" }
      end
    end
  end

  # Stops the cleaner fiber and tears down this bucket.
  def stop : Nil
    @cleaner.close
    @atoms_lock.synchronize { @atoms.clear }
  end

  # Returns `true` if *atom* is in this bucket.
  def includes?(atom : AtomIdentity) : Bool
    @atoms_lock.synchronize { @atoms.has_key?(atom) }
  end

  # Adds *atom* to this bucket.
  def add(atom : AtomIdentity) : Nil
    @atoms_lock.synchronize do
      refs, _ = @atoms[atom]? || {0u32, nil}

      @atoms[atom] = {refs + 1, Time.utc}
    end
  end

  # Removes *atom* from this bucket.
  def delete(atom : AtomIdentity) : Nil
    @atoms_lock.synchronize do
      return unless row = @atoms[atom]?

      refs, birth = row
      if refs == 1
        @atoms.delete(atom)
      else
        @atoms[atom] = {refs, birth}
      end
    end
  end
end

class DecayingAtomMultiset
  def initialize(@nbuckets : Int32, minlife : Time::Span, maxlife : Time::Span)
    @buckets = Slice(DecayingAtomMultisetBucket).new(@nbuckets) do |ord|
      # Spread out the load by randomizing the lifespan of buckets.
      lifespan = (minlife.total_milliseconds..maxlife.total_milliseconds).sample.milliseconds

      DecayingAtomMultisetBucket.new(ord, lifespan)
    end
  end

  # NOTE: Assuming *atom* is a digest already, we simply read top N bytes
  # to obtain a hash, and mod it into bucket range.
  private def bucketof(atom : AtomIdentity) : DecayingAtomMultisetBucket
    @buckets[atom.hash % @nbuckets]
  end

  # Returns `true` if this set includes *atom*.
  def includes?(atom : AtomIdentity) : Bool
    bucketof(atom).includes?(atom)
  end

  # Adds *atom* to this set; postpones its decay if already present.
  def add(atom : AtomIdentity) : Nil
    bucketof(atom).add(atom)
  end

  # Removes *atom* from this set.
  def delete(atom : AtomIdentity) : Nil
    bucketof(atom).delete(atom)
  end
end

module IRemoteSet(T)
  # Synchronous query (blocks until sent & received valid response)
  abstract def includes?(identity : T) : Bool

  # Asynchronous add (blocks until sent)
  abstract def add(identity : T) : Nil

  # Asynchronous delete (blocks until sent)
  abstract def delete(identity : T) : Nil
end

module ITransactRemoteSet(T)
  include IRemoteSet(T)

  abstract def transact(additions : Set(T), deletions : Set(T)) : Nil
end

module Protocol
  extend self

  class Error < Exception
  end

  # Reads a dict from *io* according to the protocol.
  #
  # Raises `Error` if there was an error (e.g. an I/O error).
  def getdict(io : IO) : Term::Dict
    begin
      bytesize = io.read_bytes(Int32, format: IO::ByteFormat::BigEndian)
      ml = io.read_string(bytesize)
    rescue e : IO::Error
      raise Error.new("i/o error", cause: e)
    rescue e : IO::EOFError
      raise Error.new("too few bytes", cause: e)
    end

    begin
      term = ML.term(ml)
    rescue e : ML::SyntaxError
      raise Error.new("invalid request", cause: e)
    end

    unless dict = term.as_d?
      raise Error.new("request is not a dict")
    end

    dict
  end

  # Writes a dict to *io* according to the protocol.
  #
  # Raises `Error` if there was an error (e.g. an I/O error).
  def putdict(io : IO, dict : Term::Dict, *, flush : Bool = false) : Nil
    bytesize = ML.compact_bytesize(dict)

    io.write_bytes(bytesize, format: IO::ByteFormat::BigEndian)

    ML.compact(io, dict)

    if flush
      io.flush
    end
  rescue e : IO::Error
    raise Error.new("i/o error", cause: e)
  end

  {% if flag?(:docs) %}
    # Encodes *object* into a term.
    #
    # Raises `Error` if there was an error.
    def encode(object) : Term
      unreachable
    end

    # Decodes *term* into an instance of *object*.
    #
    # Raises `Error` if there was an error.
    def decode(object : T.class, term : Term) : T forall T
      unreachable
    end
  {% end %}

  # :nodoc:
  def encode(object : Bytes) : Term
    Term.of(Base64.strict_encode(object))
  end

  # :nodoc:
  def encode(object : Label) : Term
    Term.of(:label, object.value.to_s(base: 62, precision: 22))
  end

  # :nodoc:
  def encode(object : SensorInfo) : Term
    Term.of(:sensor, encode(object.conid), encode(object.grpid), object.slot)
  end

  # :nodoc:
  def encode(object : AppearanceInfo) : Term
    Term.of(:appearance, encode(object.conid), object.slot, object.value)
  end

  # :nodoc:
  def encode(object : Activation) : Term
    case object.kind
    in .stimulus_presence?
      Term.of(:stimulus, :presence, encode(object.sensor), encode(object.instant), encode(object.appearance))
    in .stimulus_absence?
      Term.of(:stimulus, :absence, encode(object.sensor), encode(object.instant), encode(object.appearance))
    end
  end

  # :nodoc:
  def decode(object : Bytes.class, term : Term)
    unless term.type.string?
      raise Error.new("expected base64 string")
    end

    Base64.decode(term.to(String))
  rescue e : Base64::Error
    raise Error.new("base64 decode error", cause: e)
  end

  # :nodoc:
  def decode(object : AtomIdentity.class, term : Term) : AtomIdentity
    digest = decode(Bytes, term)
    unless digest.size == FINGERPRINT_BYTESIZE
      raise Error.new("invalid size of atom digest: #{digest.size}")
    end

    AtomIdentity.new(digest)
  end

  # :nodoc:
  def decode(object : Label.class, term : Term) : Label
    Term.case(term) do
      matchpi %{(label id0_string)} do
        id = id0.to(String)

        continue unless id.size == 22

        Label.new(id.to_u128(base: 62))
      end

      otherwise do
        raise Error.new("invalid label")
      end
    end
  end

  # :nodoc:
  def decode(object : SensorInfo.class, term : Term) : SensorInfo
    Term.case(term) do
      matchpi %{(sensor conid_ grpid_ slot←(%number u32))} do
        SensorInfo.new(decode(Label, conid), decode(Label, grpid), slot.to(UInt32))
      end

      otherwise do
        raise Protocol::Error.new("invalid sensor")
      end
    end
  end

  # :nodoc:
  def decode(object : AppearanceInfo.class, term : Term) : AppearanceInfo
    Term.case(term) do
      matchpi %{(appearance conid_ slot←(%number u32) value_)} do
        AppearanceInfo.new(decode(Label, conid), slot.to(UInt32), value)
      end

      otherwise do
        raise Protocol::Error.new("invalid appearance")
      end
    end
  end

  # :nodoc:
  def decode(object : Activation.class, term : Term) : Activation
    Term.case(term) do
      matchpi %{(stimulus presence sensor_ instant_ appearance_)} do
        Activation.new(:stimulus_presence, decode(SensorInfo, sensor), decode(Label, instant), decode(AppearanceInfo, appearance))
      end

      matchpi %{(stimulus absence sensor_ instant_ appearance_)} do
        Activation.new(:stimulus_absence, decode(SensorInfo, sensor), decode(Label, instant), decode(AppearanceInfo, appearance))
      end

      otherwise do
        raise Error.new("invalid activation")
      end
    end
  end
end

# Represents a client from the server's point of view.
class RemoteSurfnetClient
  Log = ::Log.for(self)

  def initialize(@set : DecayingAtomMultiset, @chat : IChat(Term), @socket : TCPSocket)
    @unsub = {} of Term => IChat::Unsubscribe
    @outlock = Mutex.new
  end

  private def response!(response : Term::Dict) : Nil
    Log.trace { "send response: #{ML.compact(response)}" }

    @outlock.synchronize do
      Protocol.putdict(@socket, response, flush: true)
    end
  end

  private def response(*args, **kwargs) : Nil
    response! Term.dict(*args, **kwargs)
  end

  # Starts the mainloop for this client. Blocks until the mainloop ends (the client
  # disconnects or is dropped by us due to an error).
  def mainloop : Nil
    Log.info { "client mainloop is running" }

    while true
      request = Protocol.getdict(@socket)

      Log.trace { "receive request: #{request}" }

      Term.case(request) do
        matchpi %{(atom? id_string)} do
          identity = Protocol.decode(AtomIdentity, id)

          response :atom?, id, @set.includes?(identity)
        end

        matchpi %{(atom/add id_string)} do
          identity = Protocol.decode(AtomIdentity, id)

          @set.add(identity)
        end

        matchpi %{(atom/delete id_string)} do
          identity = Protocol.decode(AtomIdentity, id)

          @set.delete(identity)
        end

        matchpi %{(atom/tx additions←(_*) deletions←(_*))} do
          identities_added = Array(AtomIdentity).new(additions.size)
          identities_deleted = Array(AtomIdentity).new(deletions.size)

          additions.each_item_unordered do |identity|
            identities_added << Protocol.decode(AtomIdentity, identity)
          end

          deletions.each_item_unordered do |identity|
            identities_deleted << Protocol.decode(AtomIdentity, identity)
          end

          # Everything checks out, commit.
          identities_deleted.each { |identity| @set.delete(identity) }
          identities_added.each { |identity| @set.add(identity) }

          Log.trace { "transaction +#{identities_added.size} -#{identities_deleted.size} identities" }
        end

        matchpi %{(sub/add conid_)} do
          @unsub.put_if_absent(conid) do
            # NOTE: This callback is run by everyone who wants to send something to
            # us; by their corresponding fibers.
            @chat.subscribe(Protocol.decode(Label, conid)) do |act|
              response :message, conid, act
            rescue e : Protocol::Error
              # ... thus if an error occurs on our end (in our response method), we
              # don't want to crash them. So instead we close the socket. This signals
              # to the fiber running the mainloop to terminate through an IO error.
              Log.debug(exception: e) { "error while sending message" }

              @socket.close
            end
          end
        end

        matchpi %{(sub/del conid_)} do
          next unless unsub = @unsub.delete(conid)

          unsub.call
        end

        matchpi %{(send conid_ act_)} do
          @chat.send(Protocol.decode(Label, conid), act)
        end

        otherwise do
          raise Protocol::Error.new("invalid request: #{request}")
        end
      end
    end
  rescue e : Protocol::Error
    Log.debug(exception: e) { "protocol error" }
  ensure
    # When a protocol error or any other kind of error occurs on the server-side,
    # we simply unsubscribe and close the connection.
    Log.info { "close sequence: unsubscribe" }

    @unsub.each_value(&.call)

    Log.info { "close sequence: close connection" }

    @socket.close

    Log.info { "close sequence: complete" }
  end
end

# Represents a server from the client's point of view.
class RemoteSurfnetServer
  include IChat(Activation)
  include ITransactRemoteSet(Bytes)

  Log = ::Log.for(self)

  def initialize(host : String, port : Int)
    # These ivars are accessed by the client fiber & by the reconnect fiber;
    # we need to synchronize.
    @subs = {} of Term => (Activation ->)
    @online = false
    @statecb = ->(state : Bool) { }
    @lock = Mutex.new

    @requests = Channel({Term::Dict, Channel(Bool)}).new
    @answers = Channel(Term::Dict).new
    @errors = Channel(Protocol::Error).new

    spawn reconnect(host, port)
  end

  def on_connection_state_changed(&fn : Bool ->) : Nil
    @lock.synchronize do
      @statecb = fn
      @statecb.call(@online)
    end
  end

  # The reconnect loop manages reconnect (and initial connect) with exponential
  # decay naps in between.
  private def reconnect(host, port) : Nil
    nap = nil
    reconnects = 0
    initial = true

    while true
      if nap
        Log.info { "reconnect: will try again in #{nap.total_seconds}s" }
        sleep nap
      end

      begin
        socket = TCPSocket.new(host, port)
        socket.tcp_nodelay = true

        Log.info { "established connection" }

        @lock.synchronize do
          @online = true
          @statecb.call(@online)
        end

        # Requests for just this turn.
        requests = Channel({Term::Dict, Channel(Bool)}).new

        connect(socket, requests)
        restore(requests) unless initial

        initial = false

        # Handle normal close of @requests and @errors. Wait for errors otherwise.
        # Do not waste time; relay requests through to the writer fiber.
        while true
          if requests.closed?
            error = @errors.receive?
            return unless error
            break
          else
            select
            when row = @requests.receive?
              return unless row

              request, ok0 = row

              ok1 = Channel(Bool).new

              # The only way the requests channel is closed on the writer side is
              # when an error occurs.
              begin
                requests.send({request, ok1})
              rescue Channel::ClosedError
                ok0.send(false)
                ok1.close
                next
              end

              status = ok1.receive
              if status
                nap = nil
                reconnects = 0
              end

              ok0.send(status)
            when error = @errors.receive?
              return unless error
              break
            end
          end
        end

        Log.debug(exception: error) { "reconnect due to protocol error" }

        # Make sure to close the existing socket if it's not closed already.
        # This will trigger the reader fiber if it's listening, to terminate
        # itself. The writer fiber will terminate.
        socket.close
      rescue e : Socket::ConnectError
        # Abnormal close
        Log.debug(exception: e) { "could not establish connection" }
      end

      @lock.synchronize do
        @online = false
        @statecb.call(@online)
      end

      reconnects += 1
      nap = (200 * 2**reconnects).milliseconds
      if nap > 30.seconds
        nap = 30.seconds
      end
    end
  ensure
    Log.info { "reconnect loop terminated" }
  end

  # Spawns the reader and writer fiber pair for *socket*.
  private def connect(socket, requests) : Nil
    spawn reader(socket)
    spawn writer(socket, requests)
  end

  private def reader(socket) : Nil
    Log.info { "read fiber running" }

    # Dispatch responses, blocking until the socket is closed.
    while true
      response = Protocol.getdict(socket)

      Log.trace { "receive response #{response}" }

      Term.case(response) do
        # Handle messages sent to us by other clients, relayed through
        # the server.
        matchpi %{(message conid_ act_)} do
          notify(conid, Protocol.decode(Activation, act))
        end

        otherwise do
          @answers.send(response)
        rescue Channel::ClosedError
          # Handle normal close.
          return
        end
      end
    end
  rescue e : Protocol::Error
    @errors.send(e) rescue nil
  ensure
    Log.info { "read fiber stopped" }
  end

  private def writer(socket, requests) : Nil
    Log.info { "write fiber running" }

    while request = requests.receive?
      Log.trace { "write fiber received request: #{request}" }

      dict, ok = request

      begin
        Protocol.putdict(socket, dict)
      rescue e
        requests.close

        ok.send(false)

        raise e
      else
        ok.send(true)
        ok.close
      end
    end
  rescue e : Protocol::Error
    @errors.send(e) rescue nil
  ensure
    Log.info { "write fiber stopped" }
  end

  # Called after the connection is restored.
  #
  # WARNING: do not use `@requests`; nobody is listening to it yet!
  private def restore(requests : Channel({Term::Dict, Channel(Bool)})) : Nil
    sub_requests = @lock.synchronize do
      @subs.map { |conid, _| Term[:"sub/add", conid] }
    end

    sub_requests.each do |sub_request|
      ok = Channel(Bool).new

      begin
        requests.send({sub_request, ok})
      rescue Channel::ClosedError
        # Raised when the requests fiber had some problem. It will report
        # the problem to the errors channel on its own. We just quit.
        return
      end

      # Restore requests can be redone; if ok is false then there's some
      # sort of an error on the writer fiber side, just ignore it.
      ok.receive
    end
  end

  # Notifies *conid* about activation *act*.
  private def notify(conid : Term, act : Activation) : Nil
    if sub = @lock.synchronize { @subs[conid]? }
      sub.call(act)
    end
  end

  private def answer(& : -> T) : T forall T
    while true
      begin
        answer = @answers.receive

        return yield answer
      rescue e : Protocol::Error
        @errors.send(e) rescue nil
      end
    end
  end

  # Sends a request. If the socket is unavailable, blocks until it is and
  # the request is sent.
  private def request!(request : Term::Dict, *, channel = @requests) : Nil
    ok = Channel(Bool).new

    while true
      channel.send({request, ok})

      # Block until the request is sent (i.e. the writer fiber says so).
      break if ok.receive
    end
  end

  # :ditto:
  private def request(*args, **kwargs) : Nil
    request!(Term.dict(*args, **kwargs))
  end

  # :inherit:
  #
  # Blocks until the has-atom request is sent, and is responded to in a valid way.
  def includes?(identity : Bytes) : Bool
    encoded = Protocol.encode(identity)

    request :atom?, encoded

    answer do |term|
      result = Term.case(term, env: Term[identity: encoded]) do
        matchpi %{(atom? identity_ exists_boolean)} do
          exists.true?
        end

        otherwise do
          raise Protocol::Error.new("invalid identity")
        end
      end

      result.as(Bool)
    end
  end

  # :inherit:
  #
  # Blocks until the request to add is sent.
  def add(identity : Bytes) : Nil
    request :"atom/add", Protocol.encode(identity)
  end

  # :inherit:
  #
  # Blocks until the request to delete is sent.
  def delete(identity : Bytes) : Nil
    request :"atom/delete", Protocol.encode(identity)
  end

  # :inherit:
  #
  # Blocks until the request to subscribe is sent.
  def subscribe(address : Label, &recv : Activation ->) : Unsubscribe
    address = Protocol.encode(address)

    if @lock.synchronize { @subs.put?(address, recv) }
      request :"sub/add", address
    end

    Unsubscribe.new do
      if @lock.synchronize { @subs.delete(address) }
        request :"sub/del", address
      end
    end
  end

  # :inherit:
  #
  # Blocks until sent.
  def send(to receiver : Label, message : Activation) : Nil
    request :send, Protocol.encode(receiver), Protocol.encode(message)
  end

  # :inherit:
  #
  # Blocks until sent.
  def transact(additions : Set(Bytes), deletions : Set(Bytes)) : Nil
    additions_dict = Term::Dict.build do |commit|
      commit.concat(additions) { |identity| Protocol.encode(identity) }
    end

    deletions_dict = Term::Dict.build do |commit|
      commit.concat(deletions) { |identity| Protocol.encode(identity) }
    end

    request :"atom/tx", additions_dict, deletions_dict
  end
end

struct UnbufferedSet(T)
  include ISet(T)

  struct Atom(T)
    include IAtom

    def initialize(@set : UnbufferedSet(T), @identity : T)
    end

    def burn : Nil
      @set.delete(@identity)
    end

    def reinsert : Nil
      @set.add(@identity)
    end
  end

  def initialize(@set : IRemoteSet(T))
  end

  def size? : Int32?
  end

  def includes?(identity : T) : Bool
    @set.includes?(identity)
  end

  def add(identity : T, atoms : AtomArray) : Nil
    add(identity)

    atoms << Atom.new(self, identity)
  end

  def flush : Nil
  end

  protected def add(identity : T) : Nil
    @set.add(identity)
  end

  protected def delete(identity : T) : Nil
    @set.delete(identity)
  end
end

class BufferedSet(T)
  include ISet(T)

  struct Atom(T)
    include IAtom

    def initialize(@set : BufferedSet(T), @identity : T)
    end

    def burn : Nil
      @set.delete(@identity)
      @set.flush # FIXME: ?!
    end

    def reinsert : Nil
      @set.add(@identity)
      @set.flush # FIXME: ?!
    end
  end

  def initialize(@set : ITransactRemoteSet(T))
    @additions = Set(T).new
    @deletions = Set(T).new
    @lock = Mutex.new
  end

  def size? : Int32?
  end

  def includes?(identity : T) : Bool
    @lock.synchronize { @additions.includes?(identity) } || @set.includes?(identity)
  end

  def add(identity : T, atoms : AtomArray) : Nil
    add(identity)

    atoms << Atom.new(self, identity)
  end

  def flush : Nil
    additions, deletions = @lock.synchronize do
      state = {@additions, @deletions}
      @additions = Set(T).new
      @deletions = Set(T).new
      state
    end

    @set.transact(additions, deletions)
  end

  protected def add(identity : T) : Nil
    @lock.synchronize do
      @additions << identity
    end
  end

  protected def delete(identity : T) : Nil
    @lock.synchronize do
      return if @additions.delete(identity)

      @deletions << identity
    end
  end
end

if ARGV[0]? == "serve"
  set = DecayingAtomMultiset.new(nbuckets: System.cpu_count.to_i, minlife: 30.seconds, maxlife: 1.minute)
  chat = SyncInMemoryChat(Term).new

  server = TCPServer.new("0.0.0.0", 9810)
  while socket = server.accept?
    socket.tcp_nodelay = true
    client = RemoteSurfnetClient.new(set, chat, socket)

    spawn do
      client.mainloop
    end
  end
elsif ARGV[0]? == "join-s"
  server = RemoteSurfnetServer.new("0.0.0.0", 9810)
  obs = ->(view : Tview) { pp view; nil }
  conn = Tconn.new(TspaceDigestSet.new(UnbufferedSet.new(server)), server, obs, keepalive: Keepalive::Continuous.new(30.seconds), relook: Relook::Periodic.new)
  conn.refresh_view
  setconn = Tsetconn.new(conn)
  setconn << Tsetconn.sensor(ML.term(%{(+ a_number b_number)}), relook: 1.minute)
  # setconn << Tsetconn.appearance(ML.term %{(+ 1 2)})
  sleep
elsif ARGV[0]? == "join-a"
  server = RemoteSurfnetServer.new("0.0.0.0", 9810)
  obs = ->(view : Tview) { pp view; nil }
  conn = Tconn.new(TspaceDigestSet.new(UnbufferedSet.new(server)), server, obs, keepalive: Keepalive::Continuous.new(30.seconds), relook: Relook::Periodic.new)
  setconn = Tsetconn.new(conn)
  # setconn << Tsetconn.sensor(ML.term %{(+ a_number b_number)})
  setconn << Tsetconn.appearance(ML.term %{(+ 1 2)})
  gets
  setconn.close
  sleep 10.seconds
  #   conid = WWID.call

  #   server.subscribe(conid) do |act|
  #     pp act
  #   end

  #   while (print "> "; input = gets)
  #   my_act = Activation.new(:stimulus_presence, SensorInfo.new(conid, WWID.call, 0), WWID.call, AppearanceInfo.new(conid, 1, ML.term(input)))
  #   server.send(conid, my_act)
  # end
end
