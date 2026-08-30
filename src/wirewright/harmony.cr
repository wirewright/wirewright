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
#
# See `Harmony.run` for Rack-independent usage example.
class Ww::Harmony
  Log = ::Log.for(self)

  defrecord ServerId, repr : UUID, brief: true

  alias EndpointId = PeerId | ClientId

  defrecord PeerId, repr : UUID, brief: true
  defrecord ClientId, repr : UUID, brief: true

  defrecord MsgId, repr : UInt64, brief: true
  defrecord HttpRequestId, repr : UUID, brief: true

  alias ServerDefn = SocketServerDefn | HttpServerDefn

  alias SocketServerDefn = TcpServerDefn | UnixServerDefn

  defrecord TcpServerDefn,
    host : String,
    port : UInt16,
    link : Link,
    brief: true

  defrecord UnixServerDefn,
    path : NormalPath,
    link : Link,
    brief: true

  defrecord HttpServerDefn,
    host : String,
    port : UInt16,
    security : TlsServerConfig?,
    brief: true

  defrecord TlsClientConfig, verify : Bool
  defrecord TlsServerConfig, cert : NormalPath, key : NormalPath

  alias ClientDefn = SocketClientDefn | HttpClientDefn

  alias SocketClientDefn = TcpClientDefn | WsClientDefn | UnixClientDefn

  defrecord TcpClientDefn,
    host : String,
    port : UInt16,
    key : Term,
    link : Link,
    renew : Bool,
    brief: true

  defrecord WsClientDefn,
    host : String,
    port : UInt16,
    path : String,
    key : Term,
    security : TlsClientConfig?,
    link : Link,
    renew : Bool,
    brief: true

  defrecord UnixClientDefn,
    path : NormalPath,
    key : Term,
    link : Link,
    renew : Bool,
    brief: true

  defrecord HttpClientDefn,
    host : String,
    port : UInt16,
    key : Term,
    security : TlsClientConfig?,
    brief: true

  alias Link = PortalLink | DirectLink

  defrecord PortalLink, brief: true
  defrecord DirectLink, brief: true

  alias Goal = ActionableGoal | KeepaliveGoal

  alias ActionableGoal = Server | WebSocketHandler | Client | IngoingReceiveConfirmation | OutgoingMessage |
                         MessageSlot | HttpServerResponse | HttpClientRequest

  defcase Server, defn : ServerDefn, brief: true
  defcase Client, defn : ClientDefn, brief: true

  defcase WebSocketHandler, server_id : ServerId, link : Link

  defcase IngoingReceiveConfirmation, endpoint_id : EndpointId, msgid : MsgId, brief: true
  defcase OutgoingMessage, endpoint_id : EndpointId, payload : Term::Blob, brief: true

  defcase MessageSlot, endpoint_id : EndpointId, brief: true

  defcase HttpServerResponse,
    server_id : ServerId,
    request_id : HttpRequestId,
    response : Term,
    brief: true

  defcase HttpClientRequest,
    client_id : ClientId,
    request : Term,
    brief: true

  alias KeepaliveGoal = PeerKeepalive | IngoingMessageKeepalive | HttpServerRequestKeepalive

  # Represents the caller's desire to keep a link between a peer and a server open.
  # Harmony does not "garbage collect" peer links in any way; it is the caller's
  # responsibility to remove PeerKeepalive goals whose RunningPeers no longer exist.
  defcase PeerKeepalive, peer_id : PeerId, brief: true

  defcase IngoingMessageKeepalive, endpoint_id : EndpointId, msgid : MsgId, brief: true

  defcase HttpServerRequestKeepalive, request_id : HttpRequestId, brief: true

  {% begin %}
    # :nodoc:
    alias GoalClass = Union({{Goal.union_types.map(&.class).splat}})
  {% end %}

  alias GoalFeature = ServerDefn | ClientDefn | ServerId | EndpointId | MsgId | HttpRequestId |
                      Term | Term::Blob | GoalClass | Link

  alias GoalSet = IndexedSet(Goal, GoalFeature)

  alias Fact = Percept | Belief

  # A percept is a manifestation of a resource, a fiber, etc. As a consequence,
  # it cannot be simply removed; you must tell the underlying resource to remove itself,
  # which will in turn cause the removal of a percept. In other words, a ground
  # fact corresponds to something real.
  #
  # A perceptual analogy could work here. When you look at a plant, the plant is
  # a percept. You cannot make the plant disappear. You must take action to
  # make it disappear. In other words, that which manifests as a percept is
  # a stimulus source.
  alias Percept = RunningServer | RunningClient

  alias Belief = DependentBelief | IndependentBelief

  # A *dependent belief* is an imaginary fact tied to a percept. In other
  # words, whereas the dependent belief itself is imaginary, the belief's
  # existence is conditioned -- and bracketed -- by the percept.
  #
  # Continuting the analogy from `Percept`, when you look at a *green* plant,
  # the qualia of greenness is not "there" with the plant itself, in the real world.
  # It is something you ascribe to the plant. In other words, greenness is not
  # a stimulus source; it is a property of a stimulus, coming from (and thus,
  # *dependent* on) a stimulus source.
  alias DependentBelief = RunningWebSocketHandler | RunningPeer | HttpServerRequest | MessageSlotReflection

  # An *independent belief* is an imaginary fact not tied to any ground or
  # dependent belief. It is entirely within the system's "subjective world",
  # and can be freely forgotten and otherwise manipulated.
  #
  # By analogy, an independent belief is a kind of "useful hallucination" --
  # an imaginary entity used for state-keeping, perhaps, in a very loose, structural
  # sense, its "thought".
  alias IndependentBelief = PendingServer | BrokenServer | PendingClient | BrokenClient | IngoingMessage |
                            IngoingReceiveConfirmation | RemoteReceiveConfirmation | RemoteMessageSlot |
                            HttpClientResponse

  defcase RunningWebSocketHandler, server_id : ServerId, brief: true

  defcase RunningServer, defn : ServerDefn, server_id : ServerId, brief: true

  # Retriable broken-ness. This is only used at startup, before we connect
  # to the server. After we connect, any breaks result in a `BrokenServer`.
  # Although this might seem strange, it's much easier for the circuit to
  # detect semantic breaks this way (breaks in the middle of communication).
  # If the circuit wants a reconnect, when it's ready, it can simply replace
  # the `dn` status with nothing to make it go to PendingServer again on our
  # side, with retries and backoff.
  defcase PendingServer, defn : ServerDefn, detail : String, brief: true

  # Untretriable broken-ness.
  defcase BrokenServer, defn : ServerDefn, detail : String, brief: true

  defcase RunningPeer, server_id : ServerId, peer_id : PeerId, brief: true

  defcase RunningClient, defn : ClientDefn, client_id : ClientId, brief: true
  defcase PendingClient, defn : ClientDefn, detail : String, brief: true
  defcase BrokenClient, defn : ClientDefn, detail : String, brief: true

  defcase IngoingMessage,
    endpoint_id : EndpointId,
    msgid : MsgId,
    payload : Term::Blob,
    brief: true

  defcase RemoteReceiveConfirmation,
    endpoint_id : EndpointId,
    payload : Term::Blob,
    brief: true

  # `MessageSlotReflection` is in Alice's world if she told Bob that her message slot is empty.
  defcase MessageSlotReflection, endpoint_id : EndpointId, brief: true

  # `RemoteMessageSlot` is in Alice's world if Bob told her his message slot is empty.
  defcase RemoteMessageSlot, endpoint_id : EndpointId, brief: true

  # Represents a request received by a server with the given *id*.
  defcase HttpServerRequest,
    server_id : ServerId,
    request_id : HttpRequestId,
    request : Term,
    brief: true

  defcase HttpClientResponse,
    client_id : ClientId,
    request : Term,
    result : HttpResponseResult,
    brief: true

  defrecord HttpResponseResult, response : Term | HttpResponseError
  defrecord HttpResponseError, detail : String

  {% begin %}
    # :nodoc:
    alias FactClass = Union({{Fact.union_types.map(&.class).splat}})
  {% end %}

  alias FactFeature = ServerDefn | ClientDefn | ServerId | EndpointId | MsgId | HttpRequestId |
                      String | Term | Term::Blob | FactClass | HttpResponseResult

  alias FactSet = IndexedSet(Fact, FactFeature)

  alias Action = StartServer | StopServer | DropPeer | AcceptMessage |
                 SendMessage | InformReady | InformBusy | StartClient |
                 StopClient | ForgetFact | RespondToHttpRequest | RejectHttpRequest |
                 SendHttpRequest | AddWebSocketHandler | RemoveWebSocketHandler

  defrecord StartServer, defn : ServerDefn, brief: true
  defrecord StopServer, defn : ServerDefn, server_id : ServerId, brief: true
  defrecord DropPeer, peer_id : PeerId, brief: true

  defrecord SendMessage, endpoint_id : EndpointId, payload : Term::Blob, brief: true
  defrecord InformReady, endpoint_id : EndpointId, brief: true
  defrecord InformBusy, endpoint_id : EndpointId, brief: true

  defrecord AddWebSocketHandler, server_id : ServerId, link : Link
  defrecord RemoveWebSocketHandler, server_id : ServerId

  defrecord AcceptMessage, endpoint_id : EndpointId, msgid : MsgId, brief: true

  defrecord StartClient, defn : ClientDefn, brief: true
  defrecord StopClient, defn : ClientDefn, client_id : ClientId, brief: true

  defrecord ForgetFact, fact : Fact, brief: true

  defrecord RespondToHttpRequest, server_id : ServerId, request_id : HttpRequestId, response : Term, brief: true
  defrecord RejectHttpRequest, server_id : ServerId, request_id : HttpRequestId, brief: true
  defrecord SendHttpRequest, client_id : ClientId, request : Term, brief: true

  alias Observation = SocketServerStarted | HttpServerStarted | ServerStopped |
                      ServerStartFailed | ServerCrashed | MessageAccepted | MessageLost |
                      PeerConnected | PeerDisconnected | PeerCrashed | PeerReceived |
                      MessageHandled | FactForgotten | SocketClientStarted | HttpClientStarted |
                      ClientStopped | ClientReceived | ClientStartFailed | Ready | Busy | InformedReady |
                      InformedBusy | ActionTransferredToQueue | ActionRejected | HttpRequestReceived | HttpRequestHandled |
                      HttpResponseReceived | WebSocketHandlerAdded | WebSocketHandlerRemoved

  defrecord SocketServerStarted, defn : SocketServerDefn, server_id : ServerId, queue : SocketServerQueue
  defrecord HttpServerStarted, defn : HttpServerDefn, server_id : ServerId, queue : HttpServerQueue

  defrecord ServerStartFailed, defn : ServerDefn, detail : String
  defrecord ServerCrashed, defn : ServerDefn, server_id : ServerId, detail : String
  defrecord ServerStopped, defn : ServerDefn, server_id : ServerId

  defrecord SocketClientStarted, defn : SocketClientDefn, client_id : ClientId, queue : SocketQueue
  defrecord HttpClientStarted, defn : HttpClientDefn, client_id : ClientId, queue : HttpClientQueue

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

  defrecord ActionTransferredToQueue, action : Action, queue_id : UInt64
  defrecord ActionRejected, action : Action

  defrecord HttpRequestReceived, server_id : ServerId, request_id : HttpRequestId, request : Term
  defrecord HttpRequestHandled, request_id : HttpRequestId
  defrecord HttpResponseReceived, client_id : ClientId, request : Term, result : HttpResponseResult

  defrecord HttpResponseError, detail : String

  defrecord WebSocketHandlerAdded, server_id : ServerId
  defrecord WebSocketHandlerRemoved, server_id : ServerId

  # A generic close command.
  defrecord Close

  defrecord Backoff,
    deadline : Time::Instant,
    attempt : UInt32,
    generation : UInt64,
    copying: true

  struct ActionSet
    include Enumerable(Action)

    def initialize
      @actions = {} of Action => UInt64?
    end

    def includes?(action : Action) : Bool
      @actions.has_key?(action)
    end

    def each(& : Action ->) : Nil
      @actions.each { |action, _| yield action }
    end

    def add(action : Action) : Nil
      @actions.put_if_absent(action, nil)
    end

    def delete(action : Action) : Nil
      @actions.delete(action)
    end

    def transfer(action : Action, queue_id : UInt64) : Nil
      @actions[action] = queue_id
    end

    def reject!(& : Action, UInt64? -> Bool) : Nil
      @actions.reject! do |action, queue_id|
        yield action, queue_id
      end
    end
  end

  # alias ActionSet = Set(Action)

  getter world : FactSet
  getter goals : GoalSet
  getter actions : ActionSet

  def initialize(@alert : ->)
    @observations = AtomicQueue(Observation).new(@alert)
    @world = FactSet.new
    @goals = GoalSet.new
    @actions = ActionSet.new
    @backoff = {} of Action => Backoff
    @exchange = Exchange.new
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
  # NOTE: `observe` should be called *before* `reconcile`.
  def observe : Set::Changelog(Fact)
    observations = @observations.swap
    if observations.empty?
      return Set::Changelog(Fact).empty
    end

    Log.trace { "world before Harmony.apply(): #{@world.pretty_inspect}" }

    changelog = @world.transaction do
      observations.each do |observation|
        Log.debug { observation }
        ctx = ApplyContext.new(@world, @exchange, @actions)
        Harmony.apply(ctx, observation)
      end
    end

    if changelog.empty?
      Log.trace { "world did not change after Harmony.apply()" }
      return Set::Changelog(Fact).empty
    end

    Log.trace { "world changelog: #{changelog.inspect}" }
    Log.trace { "world after Harmony.apply(): #{@world.pretty_inspect}" }

    changelog
  end

  MIN_RETRY_DELAY = 300.milliseconds
  MAX_RETRY_DELAY = 30.seconds

  # Runs one step of *reconciliation*: based on the current world model and a `submit`ted
  # set of goals, figures out which actions to execute and executes them (most actions
  # are simply *scheduled* for execution).
  def reconcile : Nil
    now = Time.instant

    plan = Harmony.plan(@world, @goals)

    # Cancel actions that were removed (if possible).
    @actions.each do |action|
      next if action.in?(plan)

      Harmony.cancel(ExecuteContext.new(@observations, @exchange), action)
    end

    plan.each do |action|
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
      @actions.add(action)

      Log.debug { action }

      Harmony.execute(ExecuteContext.new(@observations, @exchange), action)
    end

    @actions.reject! do |action, queue_id|
      if Harmony.completed?(action, @world)
        Log.trace { "#{action} completed" }
        next true
      end

      next false unless queue_id
      next false unless @exchange.dead?(queue_id)

      Log.trace { "#{action} died with its queue" }
      true
    end

    # Backoff GC
    @backoff.reject! do |_, backoff|
      backoff.generation < @generation
    end

    @generation += 1
    Log.trace { "actions at the end of reconcile: #{@actions}" }
  end

  # Runs a "thin" observe-submit-reconcile loop. Uses the block to produce goals
  # from the current model of the world (`#world`).
  #
  # This function is useful for testing and development of Harmony itself, and
  # all related machinery, so that Rack and others don't have to be involved.
  #
  # ```
  # Harmony.run do |world|
  #   pp! world
  #
  #   goals = Harmony::GoalSet.new
  #   goals << Harmony::Server.new(Harmony::TcpServerDefn.new("127.0.0.1", 5000u16, Harmony::DirectLink.new))
  #
  #   world.each(Harmony::RunningPeer) do |peer|
  #     goals << Harmony::PeerKeepalive.new(peer.peer_id)
  #   end
  #
  #   goals
  # end
  # ```
  def self.run(*, debug : Bool = false, & : FactSet -> GoalSet) : Nil
    alarm = BlockingSignal.new
    epoch = 0u64

    harmony = new(-> { alarm.call })

    loop do
      harmony.observe
      harmony.submit(yield harmony.world)

      if debug
        puts
        puts "World:"
        harmony.world.each do |fact|
          pp fact
          puts
        end
        puts "---------------------------------------"
        puts "Goals:"
        harmony.goals.each do |goal|
          pp goal
        end
      end

      harmony.reconcile

      if debug
        puts "Actions (after reconcile)"
        harmony.actions.each do |action|
          pp action
        end
        puts
      end

      if deadline = harmony.deadline?
        timeout = deadline - Time.instant
      end
      epoch = alarm.wait_until(epoch, timeout)
    end
  end

  # Returns the set of actions needed to drive *world* toward a state desired by *goals*.
  def self.plan(world : FactSet, goals : GoalSet) : Set(Action)
    actions = Set(Action).new

    goals.each do |goal|
      next unless goal.is_a?(ActionableGoal)
      next if satisfied?(goal, world)

      actions << summon(goal)
    end

    world.each do |fact|
      if fact.is_a?(Belief) && !supported?(fact, world)
        actions << retract(fact)
        next
      end

      next if wanted?(fact, goals)

      actions << counteract(fact)
    end

    actions
  end

  # :nodoc:
  defrecord ExecuteContext,
    observations : AtomicQueue(Observation),
    exchange : Exchange

  # :nodoc:
  def self.try_enqueue(ctx : ExecuteContext, action, commands, &)
    unless commands
      ctx.observations << ActionRejected.new(action)
      return
    end

    commands << yield

    ctx.observations << ActionTransferredToQueue.new(action, queue_id: commands.seq_id)
  end

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
    queue = ctx.exchange[action.server_id, SocketServerQueue]? ||
            ctx.exchange[action.server_id, HttpServerQueue]?

    try_enqueue(ctx, action, queue) { Close.new }
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : StartClient) : Nil
    spawn client(ctx.observations, action.defn)
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : StopClient) : Nil
    queue = ctx.exchange[action.client_id, SocketQueue]? ||
            ctx.exchange[action.client_id, HttpClientQueue]?

    try_enqueue(ctx, action, queue) { Close.new }
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : DropPeer) : Nil
    queue = ctx.exchange[action.peer_id, SocketQueue]?

    try_enqueue(ctx, action, queue) { Close.new }
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : AcceptMessage | SendMessage | InformReady | InformBusy) : Nil
    queue = ctx.exchange[action.endpoint_id, SocketQueue]?

    try_enqueue(ctx, action, queue) do
      case action
      in AcceptMessage then SocketAccept.new(action.msgid)
      in SendMessage   then SocketSend.new(action.payload)
      in InformReady   then SocketInformReady.new
      in InformBusy    then SocketInformBusy.new
      end
    end
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : ForgetFact) : Nil
    ctx.observations << FactForgotten.new(action.fact)
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : RespondToHttpRequest) : Nil
    queue = ctx.exchange[action.server_id, HttpServerQueue]?

    try_enqueue(ctx, action, queue) do
      HttpRespond.new(action.request_id, action.response)
    end
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : RejectHttpRequest) : Nil
    queue = ctx.exchange[action.server_id, HttpServerQueue]?

    try_enqueue(ctx, action, queue) do
      HttpReject.new(action.request_id)
    end
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : SendHttpRequest) : Nil
    queue = ctx.exchange[action.client_id, HttpClientQueue]?

    try_enqueue(ctx, action, queue) do
      HttpSendRequest.new(action.request)
    end
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : AddWebSocketHandler) : Nil
    queue = ctx.exchange[action.server_id, HttpServerQueue]?

    try_enqueue(ctx, action, queue) do
      HttpAddWebSocketHandler.new(action.link)
    end
  end

  # :nodoc:
  def self.execute(ctx : ExecuteContext, action : RemoveWebSocketHandler) : Nil
    queue = ctx.exchange[action.server_id, HttpServerQueue]?

    try_enqueue(ctx, action, queue) do
      HttpRemoveWebSocketHandler.new
    end
  end

  # :nodoc:
  def self.cancel(ctx : ExecuteContext, action : SendHttpRequest) : Nil
    return unless queue = ctx.exchange[action.client_id, HttpClientQueue]?

    command = HttpCancelRequest.new(action.request)
    return if queue.last? == command

    queue << command
  end

  # Attempts to cancel *action*. This may not always be possible; few actions support
  # canceling. So this function is more of an advisory one -- no guarantees are given
  # that *action* will indeed be canceled before it completes.
  #
  # NOTE: Callers may call cancel() repeatedly for the same action. We do not do any
  # complicated bookkeeping for the same reason -- most actions simply aren't cancelable
  # so we don't want to pay the price just for the few ones that are.
  def self.cancel(ctx : ExecuteContext, action : Action) : Nil
  end

  defrecord ApplyContext,
    world : FactSet,
    exchange : Exchange,
    actions : ActionSet

  {% if flag?(:docs) %}
    # Modifies the world according to an *observation*.
    def self.apply(ctx : ApplyContext, observation : Observation) : Nil
    end
  {% end %}

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : SocketServerStarted) : Nil
    ctx.world.delete_all(BrokenServer, defn: observation.defn)
    ctx.world.delete_all(PendingServer, defn: observation.defn)
    ctx.world.add(RunningServer.new(observation.defn, observation.server_id))
    ctx.exchange[observation.server_id, SocketServerQueue] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : HttpServerStarted) : Nil
    ctx.world.delete_all(BrokenServer, defn: observation.defn)
    ctx.world.delete_all(PendingServer, defn: observation.defn)
    ctx.world.add(RunningServer.new(observation.defn, observation.server_id))
    ctx.exchange[observation.server_id, HttpServerQueue] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerStopped) : Nil
    ctx.world.delete(RunningServer.new(observation.defn, observation.server_id))

    case observation.defn
    in HttpServerDefn
      ctx.exchange.delete(observation.server_id, HttpServerQueue)
    in SocketServerDefn
      ctx.exchange.delete(observation.server_id, SocketServerQueue)
    end
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerStartFailed) : Nil
    ctx.world.add(PendingServer.new(observation.defn, observation.detail))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerCrashed) : Nil
    ctx.world.delete(RunningServer.new(observation.defn, observation.server_id))
    ctx.world.add(BrokenServer.new(observation.defn, observation.detail))

    case observation.defn
    in HttpServerDefn
      ctx.exchange.delete(observation.server_id, HttpServerQueue)
    in SocketServerDefn
      ctx.exchange.delete(observation.server_id, SocketServerQueue)
    end
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : PeerConnected) : Nil
    ctx.world.add(RunningPeer.new(observation.server_id, observation.peer_id))
    ctx.exchange[observation.peer_id, SocketQueue] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : PeerDisconnected | PeerCrashed) : Nil
    ctx.world.delete(RunningPeer.new(observation.server_id, observation.peer_id))
    ctx.exchange.delete(observation.peer_id, SocketQueue)
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
  def self.apply(ctx : ApplyContext, observation : SocketClientStarted) : Nil
    ctx.world.delete_all(BrokenClient, defn: observation.defn)
    ctx.world.delete_all(PendingClient, defn: observation.defn)
    ctx.world.add(RunningClient.new(observation.defn, observation.client_id))
    ctx.exchange[observation.client_id, SocketQueue] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : HttpClientStarted) : Nil
    ctx.world.delete_all(BrokenClient, defn: observation.defn)
    ctx.world.delete_all(PendingClient, defn: observation.defn)
    ctx.world.add(RunningClient.new(observation.defn, observation.client_id))
    ctx.exchange[observation.client_id, HttpClientQueue] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStartFailed) : Nil
    ctx.world.add(PendingClient.new(observation.defn, observation.detail))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientStopped) : Nil
    ctx.world.delete(RunningClient.new(observation.defn, observation.client_id))

    case defn = observation.defn
    in HttpClientDefn
      ctx.world.add(BrokenClient.new(defn, observation.detail))
      ctx.exchange.delete(observation.client_id, HttpClientQueue)
    in SocketClientDefn
      if defn.renew
        ctx.world.add(PendingClient.new(defn, observation.detail))
      else
        ctx.world.add(BrokenClient.new(defn, observation.detail))
      end
      ctx.exchange.delete(observation.client_id, SocketQueue)
    end
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ClientReceived) : Nil
    ctx.world.add(IngoingMessage.new(observation.client_id, observation.msgid, observation.payload))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ActionTransferredToQueue) : Nil
    ctx.actions.transfer(observation.action, observation.queue_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ActionRejected) : Nil
    ctx.actions.delete(observation.action)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : HttpRequestReceived) : Nil
    ctx.world.add(HttpServerRequest.new(observation.server_id, observation.request_id, observation.request))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : HttpRequestHandled) : Nil
    ctx.world.delete_all(HttpServerRequest, request_id: observation.request_id)
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : HttpResponseReceived) : Nil
    ctx.world.add(HttpClientResponse.new(observation.client_id, observation.request, observation.result))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : WebSocketHandlerAdded) : Nil
    ctx.world.add(RunningWebSocketHandler.new(observation.server_id))
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : WebSocketHandlerRemoved) : Nil
    ctx.world.delete(RunningWebSocketHandler.new(observation.server_id))
  end

  # Returns `true` if *goal* is *satisfied* in (by) the given *world*. If a *goal* is satisfied,
  # no *action* is taken to approach (complete) it.
  def self.satisfied?(goal : ActionableGoal, world : FactSet) : Bool
    case goal
    in Server
      # A server goal is satisfied by a running server or a broken server. A *pending*
      # server, on the other hand, will cause retries.
      world.any?(RunningServer, defn: goal.defn) || world.any?(BrokenServer, defn: goal.defn)
    in WebSocketHandler
      world.any?(RunningWebSocketHandler, server_id: goal.server_id)
    in Client
      # Ditto for clients.
      world.any?(RunningClient, defn: goal.defn) || world.any?(BrokenClient, defn: goal.defn)
    in IngoingReceiveConfirmation
      world.includes?(goal)
    in OutgoingMessage
      # An outgoing message is "satisfied" when the other side confirms it received
      # the message. Alternatively, an outgoing message is satisfied when there is
      # no slot on the other side.
      world.any?(RemoteReceiveConfirmation, endpoint_id: goal.endpoint_id, payload: goal.payload) ||
        !world.any?(RemoteMessageSlot, endpoint_id: goal.endpoint_id)
    in MessageSlot
      world.includes?(MessageSlotReflection.new(goal.endpoint_id))
    in HttpServerResponse
      # This goal is satisfied when the server no longer asks for us to process such
      # a request.
      !world.any?(HttpServerRequest, request_id: goal.request_id)
    in HttpClientRequest
      # This goal is satisfied when the client tells us the response.
      world.any?(HttpClientResponse, client_id: goal.client_id, request: goal.request)
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
    in RunningWebSocketHandler
      goals.any?(WebSocketHandler, server_id: fact.server_id)
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
      goals.any?(OutgoingMessage, endpoint_id: fact.endpoint_id, payload: fact.payload)
    in MessageSlotReflection
      goals.includes?(MessageSlot.new(fact.endpoint_id))
    in RemoteMessageSlot
      true
    in HttpServerRequest
      HttpServerRequestKeepalive.new(fact.request_id).in?(goals)
    in HttpClientResponse
      goals.any?(HttpClientRequest, client_id: fact.client_id, request: fact.request)
    end
  end

  def self.supported?(fact : Belief, world : FactSet) : Bool
    case fact
    in PendingServer, BrokenServer, PendingClient, BrokenClient
      true # ground truth
    in RunningWebSocketHandler
      world.any?(RunningServer, server_id: fact.server_id)
    in RunningPeer
      unless server = world.single?(RunningServer, server_id: fact.server_id)
        return false
      end

      case server.defn
      in SocketServerDefn
        # A peer of a socket server needs nothing but the socket server to run.
        true
      in HttpServerDefn
        # A WebSocket peer needs not only the HTTP server to run but also its WebSocket
        # handler to run.
        world.any?(RunningWebSocketHandler, server_id: fact.server_id)
      end
    in IngoingMessage, IngoingReceiveConfirmation, RemoteReceiveConfirmation,
       MessageSlotReflection, RemoteMessageSlot
      # These ones want their endpoint to be running.
      case ept = fact.endpoint_id
      in PeerId   then world.any?(RunningPeer, peer_id: ept)
      in ClientId then world.any?(RunningClient, client_id: ept)
      end
    in HttpServerRequest
      world.any?(RunningServer, server_id: fact.server_id)
    in HttpClientResponse
      world.any?(RunningClient, client_id: fact.client_id)
    end
  end

  # Returns `true` if *action* can be added to the *actions* set.
  def self.admissible?(actions : ActionSet, action : Action) : Bool
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

  # Returns the action needed to manifest fact(s) associated with *goal*.
  def self.summon(goal : ActionableGoal) : Action
    case goal
    in Server
      StartServer.new(goal.defn)
    in WebSocketHandler
      AddWebSocketHandler.new(goal.server_id, goal.link)
    in Client
      StartClient.new(goal.defn)
    in IngoingReceiveConfirmation
      AcceptMessage.new(goal.endpoint_id, goal.msgid)
    in OutgoingMessage
      SendMessage.new(goal.endpoint_id, goal.payload)
    in MessageSlot
      InformReady.new(goal.endpoint_id)
    in HttpServerResponse
      RespondToHttpRequest.new(goal.server_id, goal.request_id, goal.response)
    in HttpClientRequest
      SendHttpRequest.new(goal.client_id, goal.request)
    end
  end

  # :nodoc:
  def self.retract(fact : DependentBelief) : Action
    case fact
    in RunningPeer
      DropPeer.new(fact.peer_id)
    in RunningWebSocketHandler, MessageSlotReflection, HttpServerRequest
      ForgetFact.new(fact)
    end
  end

  # :nodoc:
  def self.retract(fact : IndependentBelief) : Action
    counteract(fact)
  end

  {% if flag?(:docs) %}
    # Returns the action needed to *retract* a *belief*. Retraction occurs
    # when a belief is not `supported?` by the world.
    #
    # To retract is often simply to forget a belief. There are more complex cases,
    # though, when a belief is a mix between ground truth and a dependent belief.
    #
    # For example, RunningPeer is one such "complicated" fact -- it is both
    # a ground truth (representing the socket connected to the peer), and
    # a dependent belief (about there being a server with such a peer).
    #
    # When the belief's ground is violated (e.g. the server disappears),
    # the belief is rightfully *retracted*. But doing so involves not merely
    # forgetting the belief -- it must necessarily involve closure of the socket
    # as well.
    def self.retract(fact : Belief) : Action
    end
  {% end %}

  # :nodoc:
  def self.counteract(fact : Percept) : Action
    case fact
    in RunningServer
      StopServer.new(fact.defn, fact.server_id)
    in RunningClient
      StopClient.new(fact.defn, fact.client_id)
    end
  end

  # :nodoc:
  def self.counteract(fact : DependentBelief) : Action
    case fact
    in RunningPeer
      DropPeer.new(fact.peer_id)
    in RunningWebSocketHandler
      RemoveWebSocketHandler.new(fact.server_id)
    in MessageSlotReflection
      InformBusy.new(fact.endpoint_id)
    in HttpServerRequest
      RejectHttpRequest.new(fact.server_id, fact.request_id)
    end
  end

  # :nodoc:
  def self.counteract(fact : IndependentBelief) : Action
    ForgetFact.new(fact)
  end

  {% if flag?(:docs) %}
    # Returns an action needed to stop *fact* from manifesting -- a *counteraction*.
    # This is the inverse of `summon`.
    def self.counteract(fact : Fact) : Action
    end
  {% end %}

  # Returns `true` if there are signs of *action* having been completed in *world*
  # (successfully or unsuccessfully).
  def self.completed?(action : Action, world : FactSet) : Bool
    case action
    in StartServer
      world.any?(RunningServer, defn: action.defn) ||
        world.any?(PendingServer, defn: action.defn) ||
        world.any?(BrokenServer, defn: action.defn)
    in StopServer
      !world.any?(RunningServer, defn: action.defn)
    in StartClient
      world.any?(RunningClient, defn: action.defn) ||
        world.any?(PendingClient, defn: action.defn) ||
        world.any?(BrokenClient, defn: action.defn)
    in StopClient
      !world.any?(RunningClient, defn: action.defn)
    in DropPeer
      !world.any?(RunningPeer, peer_id: action.peer_id)
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
    in RespondToHttpRequest, RejectHttpRequest
      # These are completed if the request they are meant to respond to or
      # reject disappears.
      !world.any?(HttpServerRequest, request_id: action.request_id)
    in SendHttpRequest
      # This one is completed when the corresponding response appears.
      world.any?(HttpClientResponse, client_id: action.client_id, request: action.request)
    in AddWebSocketHandler
      world.any?(RunningWebSocketHandler, server_id: action.server_id)
    in RemoveWebSocketHandler
      !world.any?(RunningWebSocketHandler, server_id: action.server_id)
    end
  end

  alias SocketServerQueue = BlockingQueue(SocketServerCommand)
  alias SocketServerCommand = Close

  alias SocketQueue = BlockingQueue(SocketCommand)

  alias SocketCommand = SocketRxStarted | SocketRxReceived | SocketRxOver |
                        SocketRxCrashed | SocketSend | SocketAccept | SocketInformReady |
                        SocketInformBusy | Close

  defrecord SocketRxStarted
  defrecord SocketRxReceived, payload : Term::Blob
  defrecord SocketRxOver, detail : String
  defrecord SocketRxCrashed, cause : Exception
  defrecord SocketSend, payload : Term::Blob
  defrecord SocketAccept, msgid : MsgId
  defrecord SocketInformReady
  defrecord SocketInformBusy

  # Runs a TCP server.
  def self.server(observations : IQueue, defn : TcpServerDefn) : Nil
    begin
      server = TCPServer.new(defn.host, defn.port)
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerStartFailed.new(defn, e.message || "i/o error")
      return
    end

    id = ServerId.new(UUID.random)
    queue = SocketServerQueue.new

    spawn do
      observations << SocketServerStarted.new(defn, id, queue)
      while socket = server.accept?
        socket.tcp_nodelay = true # Disable Nagle's algorithm.
        spawn peer(observations, id, defn.link, socket)
      end
      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    end

    loop do
      command = queue.shift

      case command
      in Close
        server.close rescue nil
        break
      end
    end
  end

  # Runs a Unix server.
  def self.server(observations : IQueue, defn : UnixServerDefn) : Nil
    begin
      server = UNIXServer.new(defn.path.unwrap)
    rescue e : IO::Error
      observations << ServerStartFailed.new(defn, e.message || "i/o error")
      return
    end

    id = ServerId.new(UUID.random)
    queue = SocketServerQueue.new

    spawn do
      observations << SocketServerStarted.new(defn, id, queue)
      while socket = server.accept?
        spawn peer(observations, id, defn.link, socket)
      end
      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    end

    loop do
      command = queue.shift

      case command
      in Close
        server.close(delete: true) rescue nil
        break
      end
    end
  end

  def self.peer(observations : IQueue, server_id : ServerId, link : Link, socket : Socket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << PeerConnected.new(server_id, id, queue)

    msgloop = PeerLoop.new(observations, server_id, id, link, queue, socket)
    msgloop.run
  end

  def self.peer(observations : IQueue, server_id : ServerId, link : Link, socket : HTTP::WebSocket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << PeerConnected.new(server_id, id, queue)

    msgloop = PeerLoop.new(observations, server_id, id, link, queue, socket)
    msgloop.run
  end

  def self.client(observations : IQueue, defn : TcpClientDefn) : Nil
    begin
      socket = TCPSocket.new(defn.host, defn.port.to_i)
      socket.tcp_nodelay = true # Disable the Nagle's algorithm
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    client(observations, defn, socket)
  end

  def self.client(observations : IQueue, defn : UnixClientDefn) : Nil
    begin
      socket = UNIXSocket.new(defn.path.unwrap)
    rescue e : IO::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    client(observations, defn, socket)
  end

  def self.client(observations : IQueue, defn : WsClientDefn) : Nil
    begin
      if tls_config = defn.security
        tls_context = OpenSSL::SSL::Context::Client.new
        case tls_config.verify
        in true
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::PEER
        in false
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::NONE
        end

        socket = HTTP::WebSocket.new(defn.host, defn.path, defn.port.to_i, tls: tls_context)
      else
        socket = HTTP::WebSocket.new(defn.host, defn.path, defn.port.to_i, tls: nil)
      end

      # Disable the Nagle's algorithm on the underlying TCP socket.
      socket.nagle = false
    rescue e : IO::Error | OpenSSL::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    client(observations, defn, socket)
  end

  private def self.client(observations : IQueue, defn : ClientDefn, socket : HTTP::WebSocket | Socket) : Nil
    id = ClientId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << SocketClientStarted.new(defn, id, queue)

    msgloop = ClientLoop.new(observations, defn, id, defn.link, queue, socket)
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

  alias HttpServerQueue = BlockingQueue(HttpServerCommand)
  alias HttpServerCommand = HttpRxListening | HttpRxClosed | HttpRxCrashed | HttpRxRequest |
                            HttpRespond | HttpReject | HttpAddWebSocketHandler |
                            HttpRemoveWebSocketHandler | Close

  # HTTP server (receive) fiber is listening.
  defrecord HttpRxListening

  # HTTP server (receive) fiber stopped listening.
  defrecord HttpRxClosed

  # HTTP server (receive) fiber crashed with an `Exception`.
  defrecord HttpRxCrashed, cause : Exception

  # HTTP server (receive) fiber received a request and encoded it using
  # `HttpRequestLanguage`. The command fiber is expected to handle the request
  # and set *response* eventually (or fail it).
  defrecord HttpRxRequest, request_id : HttpRequestId, request : Term, response : Sync::Future(Term)

  # A response *term* was prepared for the request with the given *id*. The command
  # fiber is expected to interpret *term* using `HttpResponseLanguage` and respond
  # to the request with *request id*.
  defrecord HttpRespond, request_id : HttpRequestId, response : Term

  # A response with the given *id* must be rejected.
  defrecord HttpReject, request_id : HttpRequestId

  # Asks the HTTP server to add a WebSocket handler with Link *link*.
  defrecord HttpAddWebSocketHandler, link : Link

  # Asks the HTTP server to remove its WebSocket handler.
  defrecord HttpRemoveWebSocketHandler

  class HttpRequestRejectedException < Exception
    @callstack = CallStack.empty
  end

  class HttpClosingException < Exception
    @callstack = CallStack.empty
  end

  # :nodoc:
  #
  # WARNING: This handler MUST be the last one because it doesn't call the next handler.
  class HttpQueueDelegateHandler
    include HTTP::Handler

    def initialize(@queue : HttpServerQueue)
    end

    def call(context)
      request_id = HttpRequestId.new(UUID.random)
      request_term = HttpRequestLanguage.encode(context.request)
      response_slot = Sync::Future(Term).new

      @queue << HttpRxRequest.new(request_id, request_term, response_slot)

      begin
        response = response_slot.get
      rescue HttpRequestRejectedException
        context.response.respond_with_status(:service_unavailable, "Rejected")
      rescue HttpClosingException
        context.response.respond_with_status(:internal_server_error, "Closing")
      else
        HttpResponseLanguage.decode(response, into: context.response)
      end
    end
  end

  # :nodoc:
  class HttpWebSocketHandler < HTTP::WebSocketHandler
    property? enabled = false

    def call(context)
      unless websocket_upgrade_request?(context.request)
        return call_next(context)
      end

      unless enabled?
        context.response.status = :misdirected_request
        return
      end

      super
    end
  end

  def self.server(observations : IQueue, defn : HttpServerDefn) : Nil
    id = ServerId.new(UUID.random)
    queue = HttpServerQueue.new

    connect_default = ->(socket : HTTP::WebSocket, context : HTTP::Server::Context) do
      socket.close(:protocol_error)
    end

    connect = connect_default

    websocket_handler = HttpWebSocketHandler.new { |socket, ctx| connect.call(socket, ctx) }

    queue_delegate_handler = HttpQueueDelegateHandler.new(queue)

    begin
      server = HTTP::NodelayServer.new([
        websocket_handler,
        HTTP::ErrorHandler.new,
        HTTP::CompressHandler.new,
        queue_delegate_handler,
      ])

      if tls_config = defn.security
        # TODO: I'm sure there's a lot more configuration to it than this.
        context = OpenSSL::SSL::Context::Server.new
        context.certificate_chain = tls_config.cert.unwrap.to_s
        context.private_key = tls_config.key.unwrap.to_s
        server.bind_tls(defn.host, defn.port.to_i, context)
      else
        server.bind_tcp(defn.host, defn.port.to_i)
      end
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerStartFailed.new(defn, e.message || "i/o error")
      return
    end

    spawn do
      queue << HttpRxListening.new
      server.listen
      # Listen returns when the server is closed.
      queue << HttpRxClosed.new
    rescue e : IO::Error | OpenSSL::Error
      queue << HttpRxCrashed.new(cause: e)
    ensure
      server.close rescue nil
    end

    pending = {} of HttpRequestId => Sync::Future(Term)

    begin
      loop do
        command = queue.shift

        case command
        in HttpRxListening
          observations << HttpServerStarted.new(defn, id, queue)
        in HttpRxClosed
          break
        in HttpRxCrashed
          raise command.cause
        in HttpRxRequest
          pending[command.request_id] = command.response
          observations << HttpRequestReceived.new(id, command.request_id, command.request)
        in HttpRespond
          next unless response = pending.delete(command.request_id)

          response.set(command.response)
          observations << HttpRequestHandled.new(command.request_id)
        in HttpReject
          unless response = pending.delete(command.request_id)
            Log.debug { "ignoring an attempt to reject a nonexistent request" }
            next
          end

          response.fail(HttpRequestRejectedException.new)
          observations << HttpRequestHandled.new(command.request_id)
        in HttpAddWebSocketHandler
          begin
            next if websocket_handler.enabled?

            link = command.link
            connect = ->(socket : HTTP::WebSocket, ctx : HTTP::Server::Context) do
              peer(observations, id, link, socket)
            end

            websocket_handler.enabled = true
          ensure
            observations << WebSocketHandlerAdded.new(id)
          end
        in HttpRemoveWebSocketHandler
          begin
            websocket_handler.enabled = false
            connect = connect_default
          ensure
            observations << WebSocketHandlerRemoved.new(id)
          end
        in Close
          server.close rescue nil
          break
        end
      end

      observations << ServerStopped.new(defn, id)
    rescue e : IO::Error | OpenSSL::Error
      observations << ServerCrashed.new(defn, id, e.message || "i/o error")
    ensure
      # Make sure all waiting connections close as well.
      pending.each { |_, response| response.fail(HttpClosingException.new) }
      pending.clear
    end
  end

  alias HttpClientQueue = BlockingQueue(HttpClientCommand)
  alias HttpClientCommand = HttpSendRequest | HttpCancelRequest | Close

  defrecord HttpSendRequest, request : Term
  defrecord HttpCancelRequest, request : Term

  def self.client(observations : IQueue, defn : HttpClientDefn) : Nil
    # We'll reuse this one client for subsequent requests.
    begin
      if tls_config = defn.security
        tls_context = OpenSSL::SSL::Context::Client.new
        case tls_config.verify
        in true
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::PEER
        in false
          tls_context.verify_mode = OpenSSL::SSL::VerifyMode::NONE
        end

        client = HTTP::Client.new(defn.host, defn.port.to_i, tls: tls_context)
      else
        client = HTTP::Client.new(defn.host, defn.port.to_i, tls: nil)
      end
    rescue e : IO::Error | OpenSSL::Error
      observations << ClientStartFailed.new(defn, e.message || "i/o error")
      return
    end

    id = ClientId.new(UUID.random)
    queue = HttpClientQueue.new
    observations << HttpClientStarted.new(defn, id, queue)

    worker = BlockingQueue(HttpSendRequest | Close).new

    spawn do
      loop do
        command = worker.shift

        case command
        in HttpSendRequest
          result = HttpResponseError.new("invalid request")

          pass do
            next unless request = HttpRequestLanguage.decode?(command.request, HTTP::Request)

            response = client.exec(request)
            result = HttpResponseLanguage.encode(response)
          rescue e : IO::Error | OpenSSL::Error
            result = HttpResponseError.new(e.message || "i/o error")
          end

          observations << HttpResponseReceived.new(id, command.request, HttpResponseResult.new(result))
        in Close
          client.close
          break
        end
      end
    end

    begin
      loop do
        command = queue.shift

        case command
        in HttpSendRequest
          worker << command
        in HttpCancelRequest
          client.close
        in Close
          worker << Close.new
          client.close
          break
        end
      end
    ensure
      observations << ClientStopped.new(defn, id, "stopped")
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

        case flow = handle(@link, command)
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
    def handle(link : PortalLink, command : SocketRxStarted) : HandleFlow
      HandleContinue.new
    end

    # :ditto:
    def handle(link : DirectLink, command : SocketRxStarted) : HandleFlow
      # If we don't do this here nobody would. In nonblocking mode, there is no
      # way to make the other side say, "I'm ready". Instead, we manufacture and
      # send this message locally. Basically, we're saying, "pretend the other
      # side said it's ready".
      on_receive_ready

      HandleContinue.new
    end

    # They sent us something.
    def handle(link : PortalLink, command : SocketRxReceived) : HandleFlow
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
    def handle(link : DirectLink, command : SocketRxReceived) : HandleFlow
      msgid = MsgId.new(@seq)
      @seq += 1

      on_receive(msgid, command.payload)

      HandleContinue.new
    end

    # We want to send something.
    def handle(link : PortalLink, command : SocketSend) : HandleFlow
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
    def handle(link : DirectLink, command : SocketSend) : HandleFlow
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
    def handle(link : PortalLink, command : SocketAccept) : HandleFlow
      frame = Portal::Accept.new(command.msgid)
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("ACCEPT not sent")
      end

      on_message_handled(command.msgid)

      HandleContinue.new
    end

    # :ditto:
    def handle(link : DirectLink, command : SocketAccept) : HandleFlow
      on_message_handled(command.msgid)

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(link : DirectLink, command : SocketInformReady) : HandleFlow
      on_informed_ready

      HandleContinue.new
    end

    # :ditto:
    def handle(link : PortalLink, command : SocketInformReady) : HandleFlow
      frame = Portal::Ready.new
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("READY not sent")
      end

      on_informed_ready

      HandleContinue.new
    end

    # We are ready to accept the next message.
    def handle(link : DirectLink, command : SocketInformBusy) : HandleFlow
      on_informed_busy

      HandleContinue.new
    end

    # :ditto:
    def handle(link : PortalLink, command : SocketInformBusy) : HandleFlow
      frame = Portal::Busy.new
      unless stream? { |io| Portal.serialize(io, frame) }
        return HandleAbort.new("BUSY not sent")
      end

      on_informed_busy

      HandleContinue.new
    end

    # They closed the connection or crashed.
    def handle(link : Link, command : SocketRxCrashed) : HandleFlow
      # The socket may not be closed since the crash is not necessarily related
      # to it. Therefore we have to use Abort which closes the socket.
      HandleAbort.new("rx error", command.cause)
    end

    # :ditto:
    def handle(link : Link, command : SocketRxOver) : HandleFlow
      on_disconnect(command.detail)

      # We know the socket is closed with RxOver so we don't have to close
      # it ourselves.
      HandleBreak.new
    end

    # We want to close the connection.
    def handle(link : Link, command : Close) : HandleFlow
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
      @link : Link,
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
      @link : Link,
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

  # Portal is a small, simple protocol used in `link: blocking`.
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
require "./harmony/exchange"
