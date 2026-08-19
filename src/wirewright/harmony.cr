# Wirewright Harmony is an experimental control loop... thing for IO, most importantly
# network-related IO.
#
# Instead of manually constructing and maintaining servers, clients, etc. (by
# calling functions, that is: connect, disconnect, start, restart, etc.),
# we let Harmony do that.
#
# We only supply Harmony with a set of *goals*, where we describe the world *how we
# want to see it*: a server running at a particular address, a client at another one,
# a peer, an outbound message, etc. Symmetrically, Harmony provides us with its model
# of the world *as it is*, i.e., what is true *now*.
#
# On our end (we're providing goals), we adapt our goals to the world model reported by
# Harmony in whichever way our semantics let us. On Harmony's end, it tries to reconcile
# the two: it plans and executes actions to drive the world model closer to the intended
# world model, as described by goals.
#
# Goals constantly change, as does the world model, so Harmony is run in a loop, continuously
# firing actions, cleaning up after crashes, etc. The three important steps of the loop
# are `observe`, after which we `submit` our goals, and then `reconcile`.
class Ww::Harmony
  Log = ::Log.for(self)

  defrecord PeerId, repr : UUID
  defrecord ServerId, repr : UUID
  defrecord ClientId, repr : UUID

  alias EndpointId = PeerId | ClientId

  alias ServerDefn = TcpServerDefn | WsServerDefn | UnixServerDefn

  defrecord TcpServerDefn, host : String, port : UInt16
  defrecord WsServerDefn, host : String, port : UInt16
  defrecord UnixServerDefn, path : NormalPath

  alias ClientDefn = TcpClientDefn | WsClientDefn | UnixClientDefn

  defrecord TcpClientDefn, host : String, port : UInt16, key : Term

  defrecord WsClientDefn,
    host : String,
    port : UInt16,
    path : String,
    key : Term,
    secure : Bool

  defrecord UnixClientDefn, path : NormalPath, key : Term

  alias Fact = RunningServer | BrokenServer | RunningPeer | RunningClient |
               BrokenClient | IngoingMessage | IngoingReceiveConfirmation |
               RemoteReceiveConfirmation

  defrecord RunningServer, defn : ServerDefn, server_id : ServerId
  defrecord BrokenServer, defn : ServerDefn, detail : String

  defrecord RunningPeer, server_id : ServerId, peer_id : PeerId

  defrecord RunningClient, defn : ClientDefn, client_id : ClientId
  defrecord BrokenClient, defn : ClientDefn, detail : String

  defrecord IngoingMessage,
    endpoint_id : EndpointId,
    seq : UInt64,
    payload : Bytes

  defrecord RemoteReceiveConfirmation,
    endpoint_id : EndpointId,
    payload : Bytes

  alias Goal = ActionableGoal | KeepaliveGoal

  alias ActionableGoal = Server | Client | IngoingReceiveConfirmation | OutgoingMessage

  defrecord Server, defn : ServerDefn
  defrecord Client, defn : ClientDefn

  defrecord IngoingReceiveConfirmation, endpoint_id : EndpointId, seq : UInt64
  defrecord OutgoingMessage, endpoint_id : EndpointId, payload : Bytes

  alias KeepaliveGoal = PeerKeepalive | IngoingMessageKeepalive

  # A peer is a token representing the callers desire to keep a link
  # between a peer and a server open. Harmony does not "garbage collect"
  # peer links in any way; it is the callers responsibility to remove Peers
  # whose RunningServers or RunningPeers no longer exist.
  defrecord PeerKeepalive, peer_id : PeerId

  defrecord IngoingMessageKeepalive, endpoint_id : EndpointId, seq : UInt64

  alias Action = StartServer | StopServer | DropPeer | AcknowledgeMessage |
                 SendMessage | StartClient | StopClient | ForgetFact

  defrecord StartServer, defn : ServerDefn
  defrecord StopServer, defn : ServerDefn, server_id : ServerId
  defrecord DropPeer, peer_id : PeerId

  defrecord SendMessage, endpoint_id : EndpointId, payload : Bytes
  defrecord AcknowledgeMessage, endpoint_id : EndpointId, seq : UInt64

  defrecord StartClient, defn : ClientDefn
  defrecord StopClient, defn : ClientDefn, client_id : ClientId

  defrecord ForgetFact, fact : Fact

  alias Observation = ServerStarted | ServerStopped | ServerStartFailed |
                      ServerCrashed | MessageSent | PeerConnected | PeerDisconnected | PeerCrashed |
                      PeerReceived | MessageHandled | FactForgotten | ClientStarted |
                      ClientStopped | ClientReceived | ClientStartFailed | ClientCrashed |
                      ActionRejected

  defrecord ServerStarted, defn : ServerDefn, server_id : ServerId, queue : ServerQueue
  defrecord ServerStartFailed, defn : ServerDefn, detail : String
  defrecord ServerCrashed, defn : ServerDefn, server_id : ServerId, detail : String
  defrecord ServerStopped, defn : ServerDefn, server_id : ServerId

  defrecord ClientStarted, defn : ClientDefn, client_id : ClientId, queue : SocketQueue
  defrecord ClientStopped, defn : ClientDefn, client_id : ClientId
  defrecord ClientStartFailed, defn : ClientDefn, detail : String
  defrecord ClientCrashed, defn : ClientDefn, client_id : ClientId, detail : String
  defrecord ClientReceived, client_id : ClientId, seq : UInt64, payload : Bytes

  defrecord PeerConnected, server_id : ServerId, peer_id : PeerId, queue : SocketQueue
  defrecord PeerDisconnected, server_id : ServerId, peer_id : PeerId
  defrecord PeerCrashed, server_id : ServerId, peer_id : PeerId, detail : String
  defrecord PeerReceived, peer_id : PeerId, seq : UInt64, payload : Bytes

  defrecord MessageHandled, endpoint_id : EndpointId, seq : UInt64
  defrecord MessageSent, endpoint_id : EndpointId, payload : Bytes

  defrecord FactForgotten, fact : Fact

  defcase ActionRejected, action : Action

  alias SocketQueue = BlockingQueue(SocketCommand)
  alias SocketCommand = SocketSend | SocketAck | SocketClose

  defrecord SocketSend, payload : Bytes
  defrecord SocketAck, seq : UInt64
  defrecord SocketClose

  alias ServerQueue = BlockingQueue(ServerCommand)
  alias ServerCommand = ServerClose

  defrecord ServerClose

  defrecord Backoff,
    deadline : Time::Instant,
    attempt : UInt32,
    generation : UInt64,
    copying: true

  getter world : World
  getter goals : Set(Goal)

  def initialize(@alert : ->)
    @observations = AtomicQueue(Observation).new(@alert)
    @world = World.new
    @goals = Set(Goal).new
    @actions = Set(Action).new
    @backoff = {} of Action => Backoff
    @registry = Registry.new
    @rng = Random::PCG32.new # for backoff jitter
    @generation = 0u64
  end

  def pending? : Bool
    # Since no actions are running, no one can enqueue to @observations. So if
    # it's empty there's really no work to do.
    !(@world.empty? && @goals.empty? && @actions.empty? && @observations.empty?)
  end

  def deadline? : Time::Instant?
    @backoff.min_of? { |_, backoff| backoff.deadline }
  end

  # Replaces the current set of *goals*.
  def submit(@goals : Set(Goal)) : Nil
    Log.trace { "submit() goals: #{@goals.pretty_inspect}" }
  end

  # Runs one step of observation. This incorporates external feedback into Harmony's
  # model of the world.
  #
  # NOTE: This is expected to run *before* `reconcile`.
  def observe : Nil
    observations = @observations.swap
    return if observations.empty?

    Log.trace { "world before observe(): #{@world.pretty_inspect}" }

    observations.each do |observation|
      Log.trace { observation }
      Harmony.apply(ApplyContext.new(@world, @registry), observation)
    end

    Log.trace { "world after observe(): #{@world.pretty_inspect}" }
  end

  MIN_RETRY_DELAY = 300.milliseconds
  MAX_RETRY_DELAY = 30.seconds

  # Runs one step of *reconciliation*: based on the current world model and a `submit`ted
  # set of goals, figures out which actions to execute and executes them (most actions
  # are simply *scheduled* for execution).
  def reconcile : Nil
    now = Time.instant

    version0 = @world.version

    Harmony.plan(@world, @goals) do |action|
      next unless Harmony.admissible?(@actions, action)

      attempt = 0u32

      if entry = @backoff[action]?
        if now < entry.deadline
          @backoff[action] = entry.copy_with(generation: @generation)
          next # Do not start until we're past the deadline
        end

        attempt = entry.attempt + 1
      end

      exp = Math.min(MIN_RETRY_DELAY * 2**attempt, MAX_RETRY_DELAY)
      delay = exp * (0.5..1.0).sample(@rng) # With jitter
      @backoff[action] = Backoff.new(now + delay, attempt, @generation)
      @actions << action

      Log.trace { "execute #{action} during reconcile" }
      Harmony.execute(ExecuteContext.new(@observations, @registry), action)
    end

    @actions.reject! do |action|
      Harmony.completed?(action, @world)
    end

    # Backoff GC
    @backoff.reject! do |_, backoff|
      backoff.generation < @generation
    end

    @generation += 1

    version1 = @world.version
    return if version0 == version1

    Log.trace { "world changed after reconcile: #{@world.pretty_inspect}" }

    # Request another round of reconciliation if the world changed.
    @alert.call
  end

  # Yields actions needed to drive *world* toward a state desired by *goals*.
  def self.plan(world : World, goals : Set(Goal), & : Action ->) : Nil
    goals.each do |goal|
      next unless goal.is_a?(ActionableGoal)
      next if satisfied?(goal, world)
      yield summon(goal)
    end

    world.each do |fact|
      next if wanted?(fact, goals) && supported?(fact, world)
      yield dismiss(fact)
    end
  end

  # :nodoc:
  defrecord ExecuteContext,
    observations : AtomicQueue(Observation),
    registry : Registry

  {% if flag?(:docs) %}
    # Executes an *action*. Reports about its status / progress are made to
    # the observations queue.
    def self.execute(ctx : ExecuteContext, action : StartServer) : Nil
    end
  {% end %}

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : StartServer) : Nil
    spawn server(ctx.observations, action.defn)
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : StopServer) : Nil
    queue : ServerQueue?

    unless queue = ctx.registry[action.server_id]?
      ctx.observations << ActionRejected.new(action)
      return
    end

    queue << ServerClose.new
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : StartClient) : Nil
    spawn client(ctx.observations, action.defn)
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : StopClient) : Nil
    queue : SocketQueue?

    unless queue = ctx.registry[action.client_id]?
      ctx.observations << ActionRejected.new(action)
      return
    end

    queue << SocketClose.new
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : DropPeer) : Nil
    queue : SocketQueue?

    unless queue = ctx.registry[action.peer_id]?
      ctx.observations << ActionRejected.new(action)
      return
    end

    queue << SocketClose.new
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : AcknowledgeMessage) : Nil
    queue : SocketQueue?

    unless queue = ctx.registry[action.endpoint_id]?
      ctx.observations << ActionRejected.new(action)
      return
    end

    queue << SocketAck.new(action.seq)
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : SendMessage) : Nil
    queue : SocketQueue?

    unless queue = ctx.registry[action.endpoint_id]?
      ctx.observations << ActionRejected.new(action)
      return
    end

    queue << SocketSend.new(action.payload)
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : ForgetFact) : Nil
    ctx.observations << FactForgotten.new(action.fact)
  end

  defrecord ApplyContext, world : World, registry : Registry

  {% if flag?(:docs) %}
    # Modifies the world according to an *observation*.
    def self.apply(ctx : ApplyContext, observation : Observation) : Nil
    end
  {% end %}

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerStarted) : Nil
    ctx.world.reject!(BrokenServer, observation.defn)
    ctx.world.add(RunningServer.new(observation.defn, observation.server_id))
    ctx.registry[observation.server_id] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerStopped) : Nil
    ctx.world.delete(RunningServer.new(observation.defn, observation.server_id))
    ctx.registry.delete(observation.server_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerStartFailed) : Nil
    ctx.world.add(BrokenServer.new(observation.defn, observation.detail))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerCrashed) : Nil
    ctx.world.delete(RunningServer.new(observation.defn, observation.server_id))
    ctx.world.add(BrokenServer.new(observation.defn, observation.detail))
    ctx.registry.delete(observation.server_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : PeerConnected) : Nil
    ctx.world.add(RunningPeer.new(observation.server_id, observation.peer_id))
    ctx.registry[observation.peer_id] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : PeerDisconnected | PeerCrashed) : Nil
    ctx.world.delete(RunningPeer.new(observation.server_id, observation.peer_id))
    ctx.world.reject!(IngoingMessage, observation.peer_id)
    ctx.registry.delete(observation.peer_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : PeerReceived) : Nil
    ctx.world.add(IngoingMessage.new(observation.peer_id, observation.seq, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : MessageHandled) : Nil
    ctx.world.add(IngoingReceiveConfirmation.new(observation.endpoint_id, observation.seq))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : MessageSent) : Nil
    ctx.world.add(RemoteReceiveConfirmation.new(observation.endpoint_id, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : FactForgotten) : Nil
    ctx.world.delete(observation.fact)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStarted) : Nil
    ctx.world.reject!(BrokenClient, observation.defn)
    ctx.world.add(RunningClient.new(observation.defn, observation.client_id))
    ctx.registry[observation.client_id] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStartFailed) : Nil
    ctx.world.add(BrokenClient.new(observation.defn, observation.detail))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStopped) : Nil
    ctx.world.delete(RunningClient.new(observation.defn, observation.client_id))
    ctx.world.reject!(IngoingMessage, observation.client_id)
    ctx.registry.delete(observation.client_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientCrashed) : Nil
    ctx.world.reject!(IngoingMessage, observation.client_id)
    ctx.world.delete(RunningClient.new(observation.defn, observation.client_id))
    ctx.world.add(BrokenClient.new(observation.defn, observation.detail))
    ctx.registry.delete(observation.client_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientReceived) : Nil
    ctx.world.add(IngoingMessage.new(observation.client_id, observation.seq, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ActionRejected) : Nil
  end

  # Returns `true` if *goal* is satisfied in (by) the given *world*.
  def self.satisfied?(goal : ActionableGoal, world : World) : Bool
    case goal
    in Server
      # A server goal is satisfied by a running server. A broken server will
      # cause it to retry. This relation is asymmetric; a broken server is
      # wanted while a Server goal exists.
      world.any?(RunningServer, goal.defn)
    in Client
      # Ditto.
      world.any?(RunningClient, goal.defn)
    in IngoingReceiveConfirmation
      world.includes?(goal)
    in OutgoingMessage
      # An outgoing message is satisfied when the other side confirms it received
      # the message.
      world.any?(RemoteReceiveConfirmation, goal.endpoint_id, goal.payload)
    end
  end

  # Returns `true` if *fact* is wanted by one or more goals from the given
  # set of *goals*.
  def self.wanted?(fact : Fact, goals : Set(Goal)) : Bool
    case fact
    in RunningServer, BrokenServer
      Server.new(fact.defn).in?(goals)
    in RunningClient, BrokenClient
      Client.new(fact.defn).in?(goals)
    in RunningPeer
      # A running peer is wanted while maintaining a link with it is one
      # of the goals.
      PeerKeepalive.new(fact.peer_id).in?(goals)
    in IngoingMessage
      # An ingoing message is wanted as long as the corresponding keepalive
      # token is present in goals.
      IngoingMessageKeepalive.new(fact.endpoint_id, fact.seq).in?(goals)
    in IngoingReceiveConfirmation
      # An ingoing receive confirmation is needed while such confirmation is
      # requested by the goals.
      fact.in?(goals)
    in RemoteReceiveConfirmation
      # A send confirmation is needed while there's a matching outgoing message
      # that needs one.
      goals.any? do |goal|
        goal.is_a?(OutgoingMessage) && {fact.endpoint_id, fact.payload} == {goal.endpoint_id, goal.payload}
      end
    end
  end

  # Some facts have dependencies; they cannot exist without those dependencies
  # present. Other facts are "ground truths": they have no dependencies. This
  # function returns `true` when *fact* is either a ground truth, or all of its
  # dependencies are present in *world*.
  def self.supported?(fact : Fact, world : World) : Bool
    case fact
    in RunningServer, BrokenServer, RunningClient, BrokenClient
      true # ground truth
    in RunningPeer
      world.any?(RunningServer, fact.server_id)
    in IngoingMessage, IngoingReceiveConfirmation, RemoteReceiveConfirmation
      # These ones want their endpoint to be running.
      case ept = fact.endpoint_id
      in PeerId   then world.any?(RunningPeer, ept)
      in ClientId then world.any?(RunningClient, ept)
      end
    end
  end

  # Returns `true` if *action* can be added to the *actions* set.
  def self.admissible?(actions : Set(Action), action : Action) : Bool
    return false if action.in?(actions)
    return false if actions.any? { |other| conflicts?(action, other) }

    true
  end

  # Returns `true` if two actions are mutually exclusive (or cannot be run
  # simultaneously for other reasons).
  #
  # It is not always possible to tell if two actions conflict by looking
  # at them alone. So this is only a very loose guard. We let the runtime
  # handle these conflicts; calling code should simply avoid causing them!
  def self.conflicts?(l : Action, r : Action) : Bool
    case {l, r}
    when {StartServer, StopServer},
         {StopServer, StartServer}
      l.defn == r.defn
    when {StartClient, StopClient},
         {StopClient, StartClient}
      l.defn == r.defn
    else
      false
    end
  end

  # Returns the action needed to manifest a fact associated with *goal*.
  def self.summon(goal : ActionableGoal) : Action
    case goal
    in Server
      StartServer.new(goal.defn)
    in Client
      StartClient.new(goal.defn)
    in IngoingReceiveConfirmation
      AcknowledgeMessage.new(goal.endpoint_id, goal.seq)
    in OutgoingMessage
      SendMessage.new(goal.endpoint_id, goal.payload)
    end
  end

  # Returns the action needed to destroy *fact*. Some facts may simply be
  # discarded ("forgotten"); others must not be, and additional asynchronous
  # work must run (e.g. to close a server). That's why an entire Action is
  # sometimes necessary, and why you must wait for dismissal too (the fact
  # disappearing from the world -- it might not necessarily happen immediately).
  def self.dismiss(fact : Fact) : Action
    case fact
    in RunningServer
      StopServer.new(fact.defn, fact.server_id)
    in RunningClient
      StopClient.new(fact.defn, fact.client_id)
    in RunningPeer
      DropPeer.new(fact.peer_id)
    in BrokenServer, BrokenClient
      # No resource or fiber is associated with a BrokenServer. It is simply
      # an informational fact.
      ForgetFact.new(fact)
    in IngoingMessage,
       IngoingReceiveConfirmation,
       RemoteReceiveConfirmation
      # Just forget them...
      ForgetFact.new(fact)
    end
  end

  # Returns `true` if there are signs of *action* having been completed in *world*
  # (successfully or unsuccessfully).
  def self.completed?(action : Action, world : World) : Bool
    case action
    in StartServer
      world.any?(RunningServer, action.defn) || world.any?(BrokenServer, action.defn)
    in StopServer
      !(world.any?(RunningServer, action.defn) || world.any?(BrokenServer, action.defn))
    in StartClient
      world.any?(RunningClient, action.defn) || world.any?(BrokenClient, action.defn)
    in StopClient
      !(world.any?(RunningClient, action.defn) || world.any?(BrokenClient, action.defn))
    in DropPeer
      world.any?(RunningPeer, action.peer_id)
    in AcknowledgeMessage
      world.includes?(IngoingReceiveConfirmation.new(action.endpoint_id, action.seq))
    in SendMessage
      world.includes?(RemoteReceiveConfirmation.new(action.endpoint_id, action.payload))
    in ForgetFact
      !world.includes?(action.fact)
    end
  end

  # Runs a TCP server.
  private def self.server(observations : AtomicQueue, defn : TcpServerDefn) : Nil
    begin
      server = TCPServer.new(defn.host, defn.port)
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerStartFailed.new(defn, e.message || "internal error")
      return
    end

    id = ServerId.new(UUID.random)
    queue = ServerQueue.new
    observations << ServerStarted.new(defn, id, queue)

    spawn relay(id, observations, queue, server)

    begin
      while socket = server.accept?
        socket.tcp_nodelay = true # Disable Nagle's algorithm.
        spawn peer(observations, id, socket)
      end

      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    ensure
      # Even though the server is already stopped here (`accept?` returned), we
      # still inform the queue fiber so that it shuts itself down.
      queue << ServerClose.new
    end
  end

  # Runs a WebSocket server.
  private def self.server(observations : AtomicQueue, defn : WsServerDefn) : Nil
    id = ServerId.new(UUID.random)

    handler = HTTP::WebSocketHandler.new do |socket, ctx|
      peer(observations, id, socket)
    end

    begin
      server = HTTP::NodelayServer.new([handler])
      server.bind_tcp(defn.host, defn.port.to_i)
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerStartFailed.new(defn, e.message || "internal error")
      return
    end

    queue = ServerQueue.new
    observations << ServerStarted.new(defn, id, queue)

    spawn relay(id, observations, queue, server)

    begin
      server.listen
      # Listen returns when the server is closed.
      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    ensure
      queue << ServerClose.new
    end
  end

  # Runs a Unix server.
  private def self.server(observations : AtomicQueue, defn : UnixServerDefn) : Nil
    begin
      server = UNIXServer.new(defn.path.unwrap)
    rescue e : IO::Error
      observations << ServerStartFailed.new(defn, e.message || "internal error")
      return
    end

    id = ServerId.new(UUID.random)
    queue = ServerQueue.new
    observations << ServerStarted.new(defn, id, queue)

    spawn relay(id, observations, queue, server)

    begin
      while socket = server.accept?
        spawn peer(observations, id, socket)
      end

      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    ensure
      queue << ServerClose.new
    end
  end

  private def self.peer(observations : AtomicQueue, server_id : ServerId, socket : TCPSocket | UNIXSocket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new
    observations << PeerConnected.new(server_id, id, queue)

    spawn relay(id, observations, queue, socket)

    begin
      seq = 0u64
      while payload = NetString.decode?(socket, Bytes)
        # Turn it into a read-only slice. We know no one can change it.
        payload = Bytes.new(payload.to_unsafe, payload.bytesize, read_only: true)
        observations << PeerReceived.new(id, seq, payload)
        seq += 1
      end

      observations << PeerDisconnected.new(server_id, id)
    rescue e : IO::Error
      observations << PeerCrashed.new(server_id, id, e.message || "i/o error")
    ensure
      queue << SocketClose.new
    end
  end

  private def self.peer(observations : AtomicQueue, server_id : ServerId, socket : HTTP::WebSocket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new
    observations << PeerConnected.new(server_id, id, queue)

    spawn relay(id, observations, queue, socket)

    begin
      seq = 0u64
      while payload = socket.receive?
        observations << PeerReceived.new(id, seq, payload.to_slice)
        seq += 1
      end

      observations << PeerDisconnected.new(server_id, id)
    rescue e : IO::Error
      observations << PeerCrashed.new(server_id, id, e.message || "i/o error")
    ensure
      queue << SocketClose.new
    end
  end

  private def self.client(observations : AtomicQueue, defn : TcpClientDefn) : Nil
    begin
      socket = TCPSocket.new(defn.host, defn.port.to_i)
      socket.tcp_nodelay = true # Disable the Nagle's algorithm
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "internal error")
      return
    end

    client(observations, defn, socket)
  end

  private def self.client(observations : AtomicQueue, defn : UnixClientDefn) : Nil
    begin
      socket = UNIXSocket.new(defn.path.unwrap)
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "internal error")
      return
    end

    client(observations, defn, socket)
  end

  private def self.client(observations : AtomicQueue, defn : TcpClientDefn | UnixClientDefn, socket : TCPSocket | UNIXSocket) : Nil
    id = ClientId.new(UUID.random)
    queue = SocketQueue.new
    observations << ClientStarted.new(defn, id, queue)

    spawn relay(id, observations, queue, socket)

    begin
      seq = 0u64
      while payload = NetString.decode?(socket, Bytes)
        # Turn it into a read-only slice. We know no one can change it.
        payload = Bytes.new(payload.to_unsafe, payload.bytesize, read_only: true)
        observations << ClientReceived.new(id, seq, payload)
        seq += 1
      end
      observations << ClientStopped.new(defn, id)
    rescue e : IO::Error
      observations << ClientCrashed.new(defn, id, e.message || "i/o error")
    ensure
      queue << SocketClose.new
    end
  end

  private def self.client(observations : AtomicQueue, defn : WsClientDefn) : Nil
    begin
      socket = HTTP::WebSocket.new(defn.host, defn.path, defn.port.to_i, tls: defn.secure ? true : nil)
      socket.nagle = false
    rescue e : IO::Error | OpenSSL::Error
      observations << ClientStartFailed.new(defn, e.message || "internal error")
      return
    end

    id = ClientId.new(UUID.random)

    queue = SocketQueue.new
    observations << ClientStarted.new(defn, id, queue)

    spawn relay(id, observations, queue, socket)

    begin
      seq = 0u64
      while payload = socket.receive?
        observations << ClientReceived.new(id, seq, payload.to_slice)
        seq += 1
      end
      observations << ClientStopped.new(defn, id)
    rescue e : IO::Error | OpenSSL::Error
      observations << ClientCrashed.new(defn, id, e.message || "i/o error")
    ensure
      queue << SocketClose.new
    end
  end

  private def self.relay(id : ServerId, observations : AtomicQueue, queue : ServerQueue, server : TCPServer | HTTP::Server) : Nil
    loop do
      case command = queue.shift
      in ServerClose
        begin
          server.close
        rescue IO::Error
        end
        break
      end
    end
  end

  private def self.relay(id : ServerId, observations : AtomicQueue, queue : ServerQueue, server : UNIXServer) : Nil
    loop do
      case command = queue.shift
      in ServerClose
        begin
          server.close(delete: true)
        rescue IO::Error
        end
        break
      end
    end
  end

  private def self.relay(id : EndpointId, observations : AtomicQueue, queue : SocketQueue, socket : TCPSocket | UNIXSocket) : Nil
    loop do
      case command = queue.shift
      in SocketSend
        begin
          NetString.encode(socket, command.payload)
          observations << MessageSent.new(id, command.payload)
        rescue e : IO::Error
          socket.close
          raise e
        end
      in SocketAck
        # We don't need the remote end to receive (application-level) acknowledgement
        # with TCP/Unix.
        observations << MessageHandled.new(id, command.seq)
      in SocketClose
        begin
          socket.close
        rescue IO::Error
        end
        break
      end
    end
  end

  private def self.relay(id : EndpointId, observations : AtomicQueue, queue : SocketQueue, socket : HTTP::WebSocket) : Nil
    loop do
      case command = queue.shift
      in SocketSend
        begin
          socket.stream(&.write(command.payload))
        rescue e : IO::Error | OpenSSL::Error
          begin
            socket.close(:abnormal_closure)
          rescue IO::Error | OpenSSL::Error
          end
          Log.debug(exception: e) { "relay() i/o error" }
        end
        observations << MessageSent.new(id, command.payload)
      in SocketAck
        # We don't need the remote end to receive (application-level) acknowledgement
        # with WebSockets.
        observations << MessageHandled.new(id, command.seq)
      in SocketClose
        begin
          socket.close(:normal_closure)
        rescue IO::Error | OpenSSL::Error
        end
        break
      end
    end
  end
end

require "./harmony/world"
require "./harmony/registry"
