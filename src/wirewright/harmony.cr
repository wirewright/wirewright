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
  defrecord MsgId, repr : UInt64

  alias EndpointId = PeerId | ClientId

  alias ServerDefn = TcpServerDefn | WsServerDefn | UnixServerDefn

  defrecord TcpServerDefn,
    host : String,
    port : UInt16,
    tx : Transmission,
    brief: true

  defrecord WsServerDefn,
    host : String,
    port : UInt16,
    tx : Transmission,
    brief: true

  defrecord UnixServerDefn,
    path : NormalPath,
    tx : Transmission,
    brief: true

  alias ClientDefn = TcpClientDefn | WsClientDefn | UnixClientDefn

  defrecord TcpClientDefn,
    host : String,
    port : UInt16,
    key : Term,
    tx : Transmission,
    brief: true

  defrecord WsClientDefn,
    host : String,
    port : UInt16,
    path : String,
    key : Term,
    secure : Bool,
    tx : Transmission,
    brief: true

  defrecord UnixClientDefn,
    path : NormalPath,
    key : Term,
    tx : Transmission,
    brief: true

  alias Transmission = PortalTransmission | DirectTransmission

  defrecord PortalTransmission, brief: true
  defrecord DirectTransmission, brief: true

  alias Goal = ActionableGoal | KeepaliveGoal

  alias ActionableGoal = Server | Client | IngoingReceiveConfirmation | OutgoingMessage | MessageSlot

  defcase Server, defn : ServerDefn
  defcase Client, defn : ClientDefn

  defcase IngoingReceiveConfirmation, endpoint_id : EndpointId, msgid : MsgId
  defcase OutgoingMessage, endpoint_id : EndpointId, payload : Term::Blob

  defcase MessageSlot, endpoint_id : EndpointId

  alias KeepaliveGoal = PeerKeepalive | IngoingMessageKeepalive

  # Represents the caller's desire to keep a link between a peer and a server open.
  # Harmony does not "garbage collect" peer links in any way; it is the caller's
  # responsibility to remove PeerKeepalive goals whose RunningPeers no longer exist.
  defcase PeerKeepalive, peer_id : PeerId

  defcase IngoingMessageKeepalive, endpoint_id : EndpointId, msgid : MsgId

  {% begin %}
    # :nodoc:
    alias GoalClass = Union({{Goal.union_types.map(&.class).splat}})
  {% end %}

  alias GoalFeature = ServerDefn | ClientDefn | EndpointId | MsgId | Term::Blob | GoalClass

  alias GoalSet = IndexedSet(Goal, GoalFeature)

  alias Fact = RunningServer | PendingServer | BrokenServer | RunningPeer | RunningClient |
               PendingClient | BrokenClient | IngoingMessage | IngoingReceiveConfirmation |
               RemoteReceiveConfirmation | MessageSlotReflection | RemoteMessageSlot

  defcase RunningServer, defn : ServerDefn, server_id : ServerId

  # Retriable broken-ness. This is only used at startup, before we connect
  # to the server. After we connect, any breaks result in a `BrokenServer`.
  # Although this might seem strange, it's much easier for the circuit to
  # detect semantic breaks this way (breaks in the middle of communication).
  # If the circuit wants a reconnect, when it's ready, it can simply replace
  # the `dn` status with nothing to make it go to PendingServer again on our
  # side, with retries and backoff.
  defcase PendingServer, defn : ServerDefn, detail : String

  # Untretriable broken-ness.
  defcase BrokenServer, defn : ServerDefn, detail : String

  defcase RunningPeer, server_id : ServerId, peer_id : PeerId

  defcase RunningClient, defn : ClientDefn, client_id : ClientId
  defcase PendingClient, defn : ClientDefn, detail : String
  defcase BrokenClient, defn : ClientDefn, detail : String

  defcase IngoingMessage,
    endpoint_id : EndpointId,
    msgid : MsgId,
    payload : Term::Blob

  defcase RemoteReceiveConfirmation,
    endpoint_id : EndpointId,
    payload : Term::Blob

  # `MessageSlotReflection` is in Alice's world if she told Bob that her message slot is empty.
  defcase MessageSlotReflection, endpoint_id : EndpointId

  # `RemoteMessageSlot` is in Alice's world if Bob told her his message slot is empty.
  defcase RemoteMessageSlot, endpoint_id : EndpointId

  {% begin %}
    # :nodoc:
    alias FactClass = Union({{Fact.union_types.map(&.class).splat}})
  {% end %}

  alias FactFeature = ServerDefn | ClientDefn | ServerId | EndpointId | MsgId |
                      String | Term::Blob | FactClass

  alias FactSet = IndexedSet(Fact, FactFeature)

  alias Action = StartServer | StopServer | DropPeer | AcceptMessage |
                 SendMessage | InformReady | InformBusy |
                 StartClient | StopClient | ForgetFact

  defrecord StartServer, defn : ServerDefn, brief: true
  defrecord StopServer, defn : ServerDefn, server_id : ServerId, brief: true
  defrecord DropPeer, peer_id : PeerId, brief: true

  defrecord SendMessage, endpoint_id : EndpointId, payload : Term::Blob, brief: true
  defrecord InformReady, endpoint_id : EndpointId, brief: true
  defrecord InformBusy, endpoint_id : EndpointId, brief: true

  defrecord AcceptMessage, endpoint_id : EndpointId, msgid : MsgId, brief: true

  defrecord StartClient, defn : ClientDefn, brief: true
  defrecord StopClient, defn : ClientDefn, client_id : ClientId, brief: true

  defrecord ForgetFact, fact : Fact, brief: true

  alias Observation = ServerStarted | ServerStopped | ServerStartFailed |
                      ServerCrashed | MessageAccepted | MessageLost | PeerConnected | PeerDisconnected | PeerCrashed |
                      PeerReceived | MessageHandled | FactForgotten | ClientStarted |
                      ClientStopped | ClientReceived | ClientStartFailed | Ready | Busy |
                      InformedReady | InformedBusy | ActionRejected

  defrecord ServerStarted, defn : ServerDefn, server_id : ServerId, queue : ServerQueue
  defrecord ServerStartFailed, defn : ServerDefn, detail : String
  defrecord ServerCrashed, defn : ServerDefn, server_id : ServerId, detail : String
  defrecord ServerStopped, defn : ServerDefn, server_id : ServerId

  defrecord ClientStarted, defn : ClientDefn, client_id : ClientId, queue : SocketQueue
  defrecord ClientStopped, defn : ClientDefn, client_id : ClientId, detail : String
  defrecord ClientStartFailed, defn : ClientDefn, detail : String
  defrecord ClientReceived, client_id : ClientId, msgid : MsgId, payload : Term::Blob

  defrecord PeerConnected, server_id : ServerId, peer_id : PeerId, queue : SocketQueue
  defrecord PeerDisconnected, server_id : ServerId, peer_id : PeerId
  defrecord PeerCrashed, server_id : ServerId, peer_id : PeerId, detail : String
  defrecord PeerReceived, peer_id : PeerId, msgid : MsgId, payload : Term::Blob

  defrecord MessageHandled, endpoint_id : EndpointId, msgid : MsgId
  defrecord MessageAccepted, endpoint_id : EndpointId, payload : Term::Blob
  defrecord MessageLost, endpoint_id : EndpointId, payload : Term::Blob

  defrecord Ready, endpoint_id : EndpointId
  defrecord Busy, endpoint_id : EndpointId

  defrecord InformedReady, endpoint_id : EndpointId
  defrecord InformedBusy, endpoint_id : EndpointId

  defrecord FactForgotten, fact : Fact

  defcase ActionRejected, action : Action

  alias SocketQueue = BlockingQueue(SocketCommand)
  alias SocketCommand = SocketRxStarted | SocketRxReceived | SocketRxOver | SocketRxCrashed | SocketSend |
                        SocketAccept | SocketInformReady | SocketInformBusy | SocketClose

  defrecord SocketRxStarted
  defrecord SocketRxReceived, payload : Term::Blob
  defrecord SocketRxOver, detail : String
  defrecord SocketRxCrashed, cause : Exception
  defrecord SocketSend, payload : Term::Blob
  defrecord SocketAccept, msgid : MsgId
  defrecord SocketInformReady
  defrecord SocketInformBusy
  defrecord SocketClose

  alias ServerQueue = BlockingQueue(ServerCommand)
  alias ServerCommand = ServerClose

  defrecord ServerClose

  defrecord Backoff,
    deadline : Time::Instant,
    attempt : UInt32,
    generation : UInt64,
    copying: true

  getter world : FactSet
  getter goals : GoalSet

  def initialize(@alert : ->)
    @observations = AtomicQueue(Observation).new(@alert)
    @world = FactSet.new
    @goals = GoalSet.new
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
  def submit(@goals : GoalSet) : Nil
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
      Log.debug { observation }
      Harmony.apply(ApplyContext.new(@world, @registry, @actions), observation)
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

      Log.debug { action }

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
  def self.plan(world : FactSet, goals : GoalSet, & : Action ->) : Nil
    goals.each do |goal|
      next unless goal.is_a?(ActionableGoal)
      next if satisfied?(goal, world)
      yield summon(goal)
    end

    world.each do |fact|
      unless supported?(fact, world)
        yield dismiss(fact, :unsupported)
      end
      unless wanted?(fact, goals)
        yield dismiss(fact, :unwanted)
      end
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
  def self.execute(ctx : ExecuteContext, action : AcceptMessage | SendMessage | InformReady | InformBusy) : Nil
    queue : SocketQueue?

    unless queue = ctx.registry[action.endpoint_id]?
      ctx.observations << ActionRejected.new(action)
      return
    end

    case action
    in AcceptMessage then queue << SocketAccept.new(action.msgid)
    in SendMessage   then queue << SocketSend.new(action.payload)
    in InformReady   then queue << SocketInformReady.new
    in InformBusy    then queue << SocketInformBusy.new
    end
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : ForgetFact) : Nil
    ctx.observations << FactForgotten.new(action.fact)
  end

  defrecord ApplyContext,
    world : FactSet,
    registry : Registry,
    actions : Set(Action)

  {% if flag?(:docs) %}
    # Modifies the world according to an *observation*.
    def self.apply(ctx : ApplyContext, observation : Observation) : Nil
    end
  {% end %}

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerStarted) : Nil
    ctx.world.delete_all(BrokenServer, observation.defn)
    ctx.world.delete_all(PendingServer, observation.defn)
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
    ctx.world.add(PendingServer.new(observation.defn, observation.detail))
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
    ctx.registry.delete(observation.peer_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : PeerReceived) : Nil
    ctx.world.add(IngoingMessage.new(observation.peer_id, observation.msgid, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : MessageHandled) : Nil
    ctx.world.add(IngoingReceiveConfirmation.new(observation.endpoint_id, observation.msgid))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : MessageAccepted) : Nil
    ctx.world.add(RemoteReceiveConfirmation.new(observation.endpoint_id, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : MessageLost) : Nil
    ctx.actions.delete(SendMessage.new(observation.endpoint_id, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : InformedReady) : Nil
    ctx.world.add(MessageSlotReflection.new(observation.endpoint_id))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : InformedBusy) : Nil
    ctx.world.delete(MessageSlotReflection.new(observation.endpoint_id))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : Ready) : Nil
    ctx.world.add(RemoteMessageSlot.new(observation.endpoint_id))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : Busy) : Nil
    ctx.world.delete(RemoteMessageSlot.new(observation.endpoint_id))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : FactForgotten) : Nil
    ctx.world.delete(observation.fact)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStarted) : Nil
    ctx.world.delete_all(BrokenClient, observation.defn)
    ctx.world.delete_all(PendingClient, observation.defn)
    ctx.world.add(RunningClient.new(observation.defn, observation.client_id))
    ctx.registry[observation.client_id] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStartFailed) : Nil
    ctx.world.add(PendingClient.new(observation.defn, observation.detail))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStopped) : Nil
    ctx.world.delete(RunningClient.new(observation.defn, observation.client_id))
    ctx.world.add(BrokenClient.new(observation.defn, observation.detail))
    ctx.registry.delete(observation.client_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientReceived) : Nil
    ctx.world.add(IngoingMessage.new(observation.client_id, observation.msgid, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ActionRejected) : Nil
    ctx.actions.delete(observation.action)
  end

  # Returns `true` if *goal* is *satisfied* in (by) the given *world*. If a *goal* is satisfied,
  # no *action* is taken to approach (complete) it.
  def self.satisfied?(goal : ActionableGoal, world : FactSet) : Bool
    case goal
    in Server
      # A server goal is satisfied by a running server or a broken server. A *pending*
      # server, on the other hand, will cause retries.
      world.any?(RunningServer, goal.defn) || world.any?(BrokenServer, goal.defn)
    in Client
      # Ditto for clients.
      world.any?(RunningClient, goal.defn) || world.any?(BrokenClient, goal.defn)
    in IngoingReceiveConfirmation
      world.includes?(goal)
    in OutgoingMessage
      # An outgoing message is "satisfied" when the other side confirms it received
      # the message. Alternatively, an outgoing message is satisfied when there is
      # no slot on the other side.
      world.any?(RemoteReceiveConfirmation, goal.endpoint_id, goal.payload) ||
        !world.any?(RemoteMessageSlot, goal.endpoint_id)
    in MessageSlot
      world.includes?(MessageSlotReflection.new(goal.endpoint_id))
    end
  end

  # Returns `true` if *fact* is wanted by one or more goals from the given
  # set of *goals*.
  def self.wanted?(fact : Fact, goals : GoalSet) : Bool
    case fact
    in RunningServer, PendingServer, BrokenServer
      Server.new(fact.defn).in?(goals)
    in RunningClient, PendingClient, BrokenClient
      Client.new(fact.defn).in?(goals)
    in RunningPeer
      # A running peer is wanted while maintaining a link with it is one
      # of the goals.
      PeerKeepalive.new(fact.peer_id).in?(goals)
    in IngoingMessage
      # An ingoing message is wanted as long as the corresponding keepalive
      # token is present in goals.
      IngoingMessageKeepalive.new(fact.endpoint_id, fact.msgid).in?(goals)
    in IngoingReceiveConfirmation
      # An ingoing receive confirmation is needed while such confirmation is
      # requested by the goals.
      goals.includes?(fact)
    in RemoteReceiveConfirmation
      # A send confirmation is needed while there's a matching outgoing message
      # that needs one.
      goals.any?(OutgoingMessage, fact.endpoint_id, fact.payload)
    in MessageSlotReflection
      goals.includes?(MessageSlot.new(fact.endpoint_id))
    in RemoteMessageSlot
      true
    end
  end

  # Some facts have dependencies; they cannot exist without those dependencies
  # present. Other facts are "ground truths": they have no dependencies. This
  # function returns `true` when *fact* is either a ground truth, or all of its
  # dependencies are present in *world*.
  def self.supported?(fact : Fact, world : FactSet) : Bool
    case fact
    in RunningServer, PendingServer, BrokenServer,
       RunningClient, PendingClient, BrokenClient
      true # ground truth
    in RunningPeer
      world.any?(RunningServer, fact.server_id)
    in IngoingMessage, IngoingReceiveConfirmation, RemoteReceiveConfirmation,
       MessageSlotReflection, RemoteMessageSlot
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
      AcceptMessage.new(goal.endpoint_id, goal.msgid)
    in OutgoingMessage
      SendMessage.new(goal.endpoint_id, goal.payload)
    in MessageSlot
      InformReady.new(goal.endpoint_id)
    end
  end

  enum DismissReason
    Unwanted
    Unsupported
  end

  # Returns the action needed to destroy *fact*. Some facts may simply be
  # discarded ("forgotten"); others must not be, and additional asynchronous
  # work must run (e.g. to close a server). That's why an entire Action is
  # sometimes necessary, and why you must wait for dismissal too (the fact
  # disappearing from the world -- it might not necessarily happen immediately).
  def self.dismiss(fact : Fact, reason : DismissReason) : Action
    case fact
    in RunningServer
      StopServer.new(fact.defn, fact.server_id)
    in RunningClient
      StopClient.new(fact.defn, fact.client_id)
    in RunningPeer
      DropPeer.new(fact.peer_id)
    in PendingServer, BrokenServer, PendingClient, BrokenClient
      # No resource or fiber is associated with a BrokenServer. It is simply
      # an informational fact.
      ForgetFact.new(fact)
    in IngoingMessage,
       IngoingReceiveConfirmation,
       RemoteReceiveConfirmation,
       RemoteMessageSlot
      # Just forget them...
      ForgetFact.new(fact)
    in MessageSlotReflection
      case reason
      in .unwanted?
        InformBusy.new(fact.endpoint_id)
      in .unsupported?
        ForgetFact.new(fact)
      end
    end
  end

  # Returns `true` if there are signs of *action* having been completed in *world*
  # (successfully or unsuccessfully).
  def self.completed?(action : Action, world : FactSet) : Bool
    case action
    in StartServer
      world.any?(RunningServer, action.defn) || world.any?(PendingServer, action.defn) || world.any?(BrokenServer, action.defn)
    in StopServer
      !world.any?(RunningServer, action.defn)
    in StartClient
      world.any?(RunningClient, action.defn) || world.any?(PendingClient, action.defn) || world.any?(BrokenClient, action.defn)
    in StopClient
      !world.any?(RunningClient, action.defn)
    in DropPeer
      world.any?(RunningPeer, action.peer_id)
    in AcceptMessage
      world.includes?(IngoingReceiveConfirmation.new(action.endpoint_id, action.msgid))
    in SendMessage
      world.includes?(RemoteReceiveConfirmation.new(action.endpoint_id, action.payload))
    in ForgetFact
      !world.includes?(action.fact)
    in InformReady
      world.includes?(MessageSlotReflection.new(action.endpoint_id))
    in InformBusy
      !world.includes?(MessageSlotReflection.new(action.endpoint_id))
    end
  end

  # Runs a TCP server.
  def self.server(observations : IQueue, defn : TcpServerDefn) : Nil
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
        spawn peer(observations, id, defn.tx, socket)
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
  def self.server(observations : IQueue, defn : WsServerDefn) : Nil
    id = ServerId.new(UUID.random)

    handler = HTTP::WebSocketHandler.new do |socket, ctx|
      peer(observations, id, defn.tx, socket)
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
  def self.server(observations : IQueue, defn : UnixServerDefn) : Nil
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
        spawn peer(observations, id, defn.tx, socket)
      end

      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    ensure
      queue << ServerClose.new
    end
  end

  def self.peer(observations : IQueue, server_id : ServerId, tx : Transmission, socket : Socket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << PeerConnected.new(server_id, id, queue)

    msgloop = PeerLoop.new(observations, server_id, id, tx, queue, socket)
    msgloop.run
  end

  def self.peer(observations : IQueue, server_id : ServerId, tx : Transmission, socket : HTTP::WebSocket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << PeerConnected.new(server_id, id, queue)

    msgloop = PeerLoop.new(observations, server_id, id, tx, queue, socket)
    msgloop.run
  end

  def self.client(observations : IQueue, defn : TcpClientDefn) : Nil
    begin
      socket = TCPSocket.new(defn.host, defn.port.to_i)
      socket.tcp_nodelay = true # Disable the Nagle's algorithm
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "internal error")
      return
    end

    client(observations, defn, socket)
  end

  def self.client(observations : IQueue, defn : UnixClientDefn) : Nil
    begin
      socket = UNIXSocket.new(defn.path.unwrap)
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "internal error")
      return
    end

    client(observations, defn, socket)
  end

  def self.client(observations : IQueue, defn : WsClientDefn) : Nil
    begin
      socket = HTTP::WebSocket.new(defn.host, defn.path, defn.port.to_i, tls: defn.secure ? true : nil)
      socket.nagle = false
    rescue e : IO::Error | OpenSSL::Error
      observations << ClientStartFailed.new(defn, e.message || "internal error")
      return
    end

    client(observations, defn, socket)
  end

  private def self.client(observations : IQueue, defn : ClientDefn, socket : HTTP::WebSocket | Socket) : Nil
    id = ClientId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << ClientStarted.new(defn, id, queue)

    msgloop = ClientLoop.new(observations, defn, id, defn.tx, queue, socket)
    msgloop.run
  end

  private def self.rxloop(queue : SocketQueue, socket : Socket) : Nil
    queue << SocketRxStarted.new

    while payload = NetString.decode?(socket, Term::Blob, timeout: 3.seconds)
      queue << SocketRxReceived.new(payload)
    end

    queue << SocketRxOver.new(detail: "connection closed by the other side")
  rescue e : IO::Error | OpenSSL::Error | NetString::DecodeError
    queue << SocketRxCrashed.new(cause: e)
  end

  private def self.rxloop(queue : SocketQueue, socket : HTTP::WebSocket) : Nil
    queue << SocketRxStarted.new

    socket.on_message do |string|
      queue << SocketRxReceived.new(Term::Blob.new(string))
    end

    socket.on_binary do |payload|
      queue << SocketRxReceived.new(Term::Blob.new(payload))
    end

    socket.on_close do |code, detail|
      queue << SocketRxOver.new(detail.present? ? detail : "connection closed by the other side")
    end

    socket.run
  rescue e : IO::Error | OpenSSL::Error
    queue << SocketRxCrashed.new(cause: e)
  end

  private def self.relay(id : ServerId, observations : IQueue, queue : ServerQueue, server : TCPServer | HTTP::Server) : Nil
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

  private def self.relay(id : ServerId, observations : IQueue, queue : ServerQueue, server : UNIXServer) : Nil
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

  module Msgloop
    Log = ::Log.for(self)

    @queue : SocketQueue

    enum CloseCode
      NormalClosure
      AbnormalClosure
    end

    abstract def stream?(& : IO ->) : Bool
    abstract def close?(code : CloseCode, detail : String) : Bool

    abstract def on_receive(msgid : MsgId, payload : Term::Blob) : Nil
    abstract def on_crash(exception : Exception) : Nil
    abstract def on_disconnect(detail : String) : Nil

    abstract def on_message_accepted(payload : Term::Blob) : Nil
    abstract def on_message_lost(payload : Term::Blob) : Nil
    abstract def on_message_handled(msgid : MsgId) : Nil

    abstract def on_receive_ready : Nil
    abstract def on_receive_busy : Nil
    abstract def on_informed_ready : Nil
    abstract def on_informed_busy : Nil

    @seq = 0u64
    @pending = {} of MsgId => Term::Blob

    alias HandleFlow = HandleContinue | HandleBreak | HandleAbort

    defrecord HandleContinue
    defrecord HandleBreak
    defrecord HandleAbort, detail : String, cause : Exception? = nil

    def run : Nil
      loop do
        command = @queue.shift
        Log.debug { "msgloop:0x#{object_id.to_s(base: 16)}: #{command}" }

        case flow = handle(@tx, command)
        in HandleContinue
        in HandleBreak
          break
        in HandleAbort
          _ = close?(:abnormal_closure, flow.detail)
          if cause = flow.cause
            on_crash(cause)
          else
            on_disconnect(flow.detail)
          end
          break
        end
      end
    end

    # The socket started.
    def handle(tx : PortalTransmission, command : SocketRxStarted) : HandleFlow
      HandleContinue.new
    end

    # :ditto:
    def handle(tx : DirectTransmission, command : SocketRxStarted) : HandleFlow
      # If we don't do this here nobody would. In nonblocking mode, there is no
      # way to make the other side say, "I'm ready". Instead, we manufacture and
      # send this message locally. Basically, we're saying, "pretend the other
      # side said it's ready".
      on_receive_ready

      HandleContinue.new
    end

    # They sent us something.
    def handle(tx : PortalTransmission, command : SocketRxReceived) : HandleFlow
      begin
        frame = Portal.deserialize(command.payload.to_slice)
      rescue e : Portal::Error
        return HandleAbort.new(e.message || "portal protocol error", e)
      end

      case frame
      in Portal::Data
        on_receive(frame.msgid, frame.payload)
      in Portal::Accept
        unless payload = @pending.delete(frame.msgid)
          Log.debug { "dropping ACCEPT for a missing message" }
          return HandleContinue.new
        end

        on_message_accepted(payload)
      in Portal::Ready
        on_receive_ready
      in Portal::Busy
        on_receive_busy
      end

      HandleContinue.new
    end

    # :ditto:
    def handle(tx : DirectTransmission, command : SocketRxReceived) : HandleFlow
      msgid = MsgId.new(@seq)
      @seq += 1

      on_receive(msgid, command.payload)

      HandleContinue.new
    end

    # We want to send something.
    def handle(tx : PortalTransmission, command : SocketSend) : HandleFlow
      msgid = MsgId.new(@seq)
      @seq += 1

      frame = Portal::Data.new(msgid, command.payload)
      unless stream? { |io| Portal.serialize(io, frame) }
        # On failure, report message loss and terminate. Perhaps we'll retry some
        # other time.
        #
        # NOTE: failure to send means something is wrong with the underlying IO. There's
        # no way we'll be able to use it. So we break here instead of continuing, and in
        # general tear everything down.
        on_message_lost(command.payload)
        return HandleAbort.new("DATA not sent")
      end

      # In blocking mode, if we see the message "go toward & across the wire", that's
      # not enough; we need the other side to confirm they've received the message.
      @pending[msgid] = command.payload

      HandleContinue.new
    end

    # :nodoc:
    def handle(tx : DirectTransmission, command : SocketSend) : HandleFlow
      unless stream?(&.write(command.payload.to_slice))
        on_message_lost(command.payload)
        return HandleAbort.new("message not sent")
      end

      # In nonblocking mode, seeing it "go toward & across the wire" counts
      # as a successful send.
      on_message_accepted(command.payload)

      HandleContinue.new
    end

    # We want to confirm the receipt of their message.
    def handle(tx : PortalTransmission, command : SocketAccept) : HandleFlow
      frame = Portal::Accept.new(command.msgid)
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("ACCEPT not sent")
      end

      on_message_handled(command.msgid)

      HandleContinue.new
    end

    # :ditto:
    def handle(tx : DirectTransmission, command : SocketAccept) : HandleFlow
      on_message_handled(command.msgid)

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(tx : DirectTransmission, command : SocketInformReady) : HandleFlow
      on_informed_ready

      HandleContinue.new
    end

    # :ditto:
    def handle(tx : PortalTransmission, command : SocketInformReady) : HandleFlow
      frame = Portal::Ready.new
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("READY not sent")
      end

      on_informed_ready

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(tx : DirectTransmission, command : SocketInformBusy) : HandleFlow
      on_informed_busy

      HandleContinue.new
    end

    # :ditto:
    def handle(tx : PortalTransmission, command : SocketInformBusy) : HandleFlow
      frame = Portal::Busy.new
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("BUSY not sent")
      end

      on_informed_busy

      HandleContinue.new
    end

    # They closed the connection or crashed.
    def handle(tx : Transmission, command : SocketRxCrashed) : HandleFlow
      # The socket may not be closed since the crash is not necessarily related
      # to it. Therefore we have to use Abort which closes the socket.
      HandleAbort.new("rx error", command.cause)
    end

    # :ditto:
    def handle(tx : Transmission, command : SocketRxOver) : HandleFlow
      on_disconnect(command.detail)

      # We know the socket is closed with RxOver so we don't have to close
      # it ourselves.
      HandleBreak.new
    end

    # We want to close the connection.
    def handle(tx : Transmission, command : SocketClose) : HandleFlow
      _ = close?(:normal_closure, "")

      # SocketRxCrashed/SocketRxOver will handle actual closure. Here we only
      # command it. So we have to continue handling commands.
      HandleContinue.new
    end
  end

  module SocketLoop
    include Msgloop

    @socket : HTTP::WebSocket | Socket

    def stream?(& : IO ->) : Bool
      case socket = @socket
      in HTTP::WebSocket
        socket.stream { |io| yield io }
      in Socket
        # NOTE: We have to know how long the string is going to be, so we must use an
        # in-memory buffer first. We can use a counting IO but then the block would
        # be called twice; I'm not sure that's appropriate here. If this ever becomes
        # a problem, we can get rid of the block and use a fixed format / type hierarchy
        # for streeaming, so that the size is known in advance or at least easily derivable.
        buffer = IO::Memory.new
        yield buffer

        NetString.encode(socket, buffer.to_slice)
      end

      true # Success
    rescue e : IO::Error | OpenSSL::Error | NetString::EncodeError
      Log.debug(exception: e) { "i/o error in SocketLoop#stream?" }

      false # Failure
    end

    def close?(code : CloseCode, detail : String) : Bool
      case socket = @socket
      in HTTP::WebSocket
        case code
        in .normal_closure?   then socket.close(:normal_closure, detail)
        in .abnormal_closure? then socket.close(:abnormal_closure, detail)
        end
      in Socket
        # Ignore the code and detail.
        socket.close
      end

      true # Success
    rescue e : IO::Error | OpenSSL::Error
      Log.debug(exception: e) { "i/o error in SocketLoop#close?" }

      false # Failure
    end
  end

  class PeerLoop
    include SocketLoop

    def initialize(
      @observations : IQueue(Observation),
      @server_id : ServerId,
      @id : PeerId,
      @tx : Transmission,
      @queue : SocketQueue,
      @socket : HTTP::WebSocket | Socket,
    )
    end

    def on_receive(msgid : MsgId, payload : Term::Blob) : Nil
      @observations << PeerReceived.new(@id, msgid, payload)
    end

    def on_crash(exception : Exception) : Nil
      @observations << PeerCrashed.new(@server_id, @id, exception.message || "i/o error")
    end

    def on_disconnect(detail : String) : Nil
      @observations << PeerDisconnected.new(@server_id, @id)
    end

    def on_receive_ready : Nil
      @observations << Ready.new(@id)
    end

    def on_receive_busy : Nil
      @observations << Busy.new(@id)
    end

    def on_informed_ready : Nil
      @observations << InformedReady.new(@id)
    end

    def on_informed_busy : Nil
      @observations << InformedBusy.new(@id)
    end

    def on_message_accepted(payload : Term::Blob) : Nil
      @observations << MessageAccepted.new(@id, payload)
    end

    def on_message_lost(payload : Term::Blob) : Nil
      @observations << MessageLost.new(@id, payload)
    end

    def on_message_handled(msgid : MsgId) : Nil
      @observations << MessageHandled.new(@id, msgid)
    end
  end

  class ClientLoop
    include SocketLoop

    def initialize(
      @observations : IQueue(Observation),
      @defn : ClientDefn,
      @id : ClientId,
      @tx : Transmission,
      @queue : SocketQueue,
      @socket : HTTP::WebSocket | Socket,
    )
    end

    def on_receive(msgid : MsgId, payload : Term::Blob) : Nil
      @observations << ClientReceived.new(@id, msgid, payload)
    end

    def on_crash(exception : Exception) : Nil
      @observations << ClientStopped.new(@defn, @id, exception.message || "i/o error")
    end

    def on_disconnect(detail : String) : Nil
      @observations << ClientStopped.new(@defn, @id, detail)
    end

    def on_receive_ready : Nil
      @observations << Ready.new(@id)
    end

    def on_receive_busy : Nil
      @observations << Busy.new(@id)
    end

    def on_informed_ready : Nil
      @observations << InformedReady.new(@id)
    end

    def on_informed_busy : Nil
      @observations << InformedBusy.new(@id)
    end

    def on_message_accepted(payload : Term::Blob) : Nil
      @observations << MessageAccepted.new(@id, payload)
    end

    def on_message_lost(payload : Term::Blob) : Nil
      @observations << MessageLost.new(@id, payload)
    end

    def on_message_handled(msgid : MsgId) : Nil
      @observations << MessageHandled.new(@id, msgid)
    end
  end

  # Portal is a small, simple protocol used in `transmission: blocking`.
  module Portal
    extend self

    alias Frame = Data | Accept | Ready | Busy

    defrecord Data, msgid : MsgId, payload : Term::Blob
    defrecord Accept, msgid : MsgId
    defrecord Ready
    defrecord Busy

    def serialize(io, msgid : MsgId) : Nil
      msgid.repr.to_s(io, base: 16)
    end

    def serialize(io, frame : Data) : Nil
      io << "DATA "
      serialize(io, frame.msgid)
      io << " "
      io.write(frame.payload.to_slice)
    end

    def serialize(io, frame : Accept)
      io << "ACCEPT "
      serialize(io, frame.msgid)
    end

    def serialize(io, frame : Ready)
      io << "READY"
    end

    def serialize(io, frame : Busy)
      io << "BUSY"
    end

    struct Reader
      def initialize(@data : Bytes)
      end

      def at_end? : Bool
        @data.empty?
      end

      def skip_to_end : Bytes
        @data, _ = @data + @data.size, @data
      end

      def read?(seq : Bytes) : Bool
        unless @data.starts_with?(seq)
          return false
        end

        @data += seq.size
        true
      end

      def read?(seq : String) : Bool
        read?(seq.to_slice)
      end

      def read?(cls : MsgId.class) : MsgId?
        cursor = @data
        repr = 0u64

        16.times do |size|
          break unless byte = cursor.first?
          break unless digit = byte.chr.hexdigit?

          repr <<= 4
          repr |= digit.to_u64
          cursor += 1
        end

        @data = cursor

        MsgId.new(repr)
      end

      def transaction(&)
        backup = @data

        begin
          result = yield
        ensure
          if result.nil?
            @data = backup
          end
        end
      end

      def read?(cls : Data.class) : Data?
        transaction do
          next unless read?("DATA ")
          next unless msgid = read?(MsgId)
          next unless read?(" ")

          payload = skip_to_end

          Data.new(msgid, Term::Blob.new(payload))
        end
      end

      def read?(cls : Accept.class) : Accept?
        transaction do
          next unless read?("ACCEPT ")
          next unless msgid = read?(MsgId)
          next unless at_end?

          Accept.new(msgid)
        end
      end

      def read?(cls : Ready.class) : Ready?
        transaction do
          next unless read?("READY") && at_end?

          Ready.new
        end
      end

      def read?(cls : Busy.class) : Busy?
        transaction do
          next unless read?("BUSY") && at_end?

          Busy.new
        end
      end

      def read?(cls : Frame.class) : Frame?
        read?(Data) || read?(Accept) || read?(Ready) || read?(Busy)
      end
    end

    def deserialize?(data : Bytes) : Frame?
      r = Reader.new(data)
      r.read?(Frame)
    end

    class Error < Exception
    end

    def deserialize(data : Bytes) : Frame
      deserialize?(data) || raise Error.new("invalid portal protocol frame")
    end
  end
end

require "./harmony/indexed_set"
require "./harmony/registry"
