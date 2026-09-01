require "./harmony/indexed_set"
require "./harmony/exchange"
require "./harmony/observation"
require "./harmony/socket_loop"
require "./harmony/socket_client"
require "./harmony/socket_peer"
require "./harmony/socket_server"
require "./harmony/http_server"
require "./harmony/http_client"
require "./harmony/goal"
require "./harmony/fact"
require "./harmony/action"
require "./harmony/portal"

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
# the two: it plans and executes *actions* to drive the world model closer to one that
# will satisfy the goals.
#
# Goals constantly change, as does the world model, so Harmony runs in a loop.
# The loop continuously dispatches actions, cleans up after crashes, etc.
# The three important steps of the loop are `observe`; after which we `submit`
# our goals; and then `reconcile`.
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
  alias ClientDefn = SocketClientDefn | HttpClientDefn

  alias ServerPort = ExclusiveServerPort | SharedServerPort | AutoServerPort

  defrecord ExclusiveServerPort, port : UInt16
  defrecord SharedServerPort, port : UInt16
  defrecord AutoServerPort

  alias Link = PortalLink | DirectLink

  defrecord PortalLink, brief: true
  defrecord DirectLink, brief: true

  # A generic close command used by `SocketQueue`, `SocketServerQueue`, and other
  # exchange queues (see `Exchange`).
  defrecord Close

  # :nodoc:
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

    def pretty_print(pp)
      pp.list("ActionSet[", @actions, "]") do |action, queue_id|
        action.pretty_print(pp)
        if queue_id
          pp.text("@#{queue_id}")
        end
      end
    end
  end

  # A read-only view of an `ActionSet`.
  #
  # NOTE: This is a thin wrapper around `ActionSet` exposing only the methods that read,
  # to be absolutely sure you don't modify the action set accidentally (or intentionally!)
  # Only Harmony can modify the action set. You can only look at it.
  struct ReadonlyActionSet
    include Enumerable(Action)

    # :nodoc:
    def initialize(@actions : ActionSet)
    end

    def includes?(action : Action) : Bool
      @actions.includes?(action)
    end

    def each(& : Action ->) : Nil
      @actions.each { |action| yield action }
    end

    def pretty_print(pp)
      @actions.pretty_print(pp)
    end
  end

  # A read-only view of Harmony's world model (a `FactSet`).
  #
  # NOTE: This is a simple wrapper around `FactSet` exposing only the methods that read,
  # to be absolutely sure you don't modify the fact set accidentally (or intentionally!)
  # Only Harmony can modify the world model. You can only look at it.
  struct ReadonlyWorld
    # :nodoc:
    def initialize(@world : FactSet)
    end

    # See `FactSet#includes?`.
    def includes?(*args, **kwargs) : Bool
      @world.includes?(*args, **kwargs)
    end

    # See `FactSet#any?`.
    def any?(*args, **kwargs) : Bool
      @world.any?(*args, **kwargs)
    end

    # See `FactSet#single?`.
    def single?(*args, **kwargs)
      @world.single?(*args, **kwargs)
    end

    # See `FactSet#each`.
    def each(*args, **kwargs, &) : Nil
      @world.each(*args, **kwargs) { |fact| yield fact }
    end
  end

  alias ObservationQueue = AtomicQueue(Observation)

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

  # Returns `true` if there is pending activity associated with Harmony. Observation
  # and reconciliation should not stop.
  def pending? : Bool
    # Since no actions are running, no one can enqueue to @observations. So if
    # it's empty there's really no work to do.
    !(@world.empty? && @goals.empty? && @actions.empty? && @observations.empty?)
  end

  # Returns the nearest instant at which reconciliation should be retried, if any.
  #
  # The main use-case for this is exponential backoff when retrying actions. Deadline
  # lets the caller sleep until the nearest retry. If the caller doesn't want to sleep,
  # they can still spam-call `reconcile`, which would amount to a busy-wait.
  def deadline? : Time::Instant?
    @backoff.min_of? { |_, backoff| backoff.deadline }
  end

  # Returns a read-only view of Harmony's model of the world.
  def world : ReadonlyWorld
    ReadonlyWorld.new(@world)
  end

  # Returns a read-only view of the currently running actions.
  def actions : ReadonlyActionSet
    ReadonlyActionSet.new(@actions)
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

  MIN_ACTION_RETRY_DELAY = 300.milliseconds
  MAX_ACTION_RETRY_DELAY = 30.seconds

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

      exp = Math.min(MIN_ACTION_RETRY_DELAY * 2**attempt, MAX_ACTION_RETRY_DELAY)
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
  def self.run(& : ReadonlyFactSet, ReadonlyActionSet -> GoalSet) : Nil
    alarm = BlockingSignal.new
    epoch = 0u64

    harmony = new(-> { alarm.call })

    loop do
      harmony.observe
      goals = yield harmony.world, harmony.actions
      harmony.submit(goals)
      harmony.reconcile

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
    ctx.world.add(RunningServer.new(observation.defn, observation.server_id, observation.info))
    ctx.exchange[observation.server_id, SocketServerQueue] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : HttpServerStarted) : Nil
    ctx.world.delete_all(BrokenServer, defn: observation.defn)
    ctx.world.delete_all(PendingServer, defn: observation.defn)
    ctx.world.add(RunningServer.new(observation.defn, observation.server_id, observation.info))
    ctx.exchange[observation.server_id, HttpServerQueue] = observation.queue
  end

  # :nodoc:
  def self.apply(ctx : ApplyContext, observation : ServerStopped) : Nil
    ctx.world.delete_all(RunningServer, server_id: observation.server_id)

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
    ctx.world.delete_all(RunningServer, server_id: observation.server_id)
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
  def self.apply(ctx : ApplyContext, observation : SocketClientReceived) : Nil
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

  # Returns `true` if the dependencies or the given *belief* (prerequisites of
  # its existence) are satisfied in *world*. In other words, returns `true` if
  # the very configuration of *world* allows *belief* to exist (as opposed to
  # goals, which may or may not want it to exist).
  def self.supported?(belief : Belief, world : FactSet) : Bool
    case belief
    in PendingServer, BrokenServer, PendingClient, BrokenClient
      true # ground truth
    in RunningWebSocketHandler
      world.any?(RunningServer, server_id: belief.server_id)
    in RunningPeer
      unless server = world.single?(RunningServer, server_id: belief.server_id)
        return false
      end

      case server.defn
      in SocketServerDefn
        # A peer of a socket server needs nothing but the socket server to run.
        true
      in HttpServerDefn
        # A WebSocket peer needs not only the HTTP server to run but also its WebSocket
        # handler to run.
        world.any?(RunningWebSocketHandler, server_id: belief.server_id)
      end
    in IngoingMessage, IngoingReceiveConfirmation, RemoteReceiveConfirmation,
       MessageSlotReflection, RemoteMessageSlot
      # These ones want their endpoint to be running.
      case ept = belief.endpoint_id
      in PeerId   then world.any?(RunningPeer, peer_id: ept)
      in ClientId then world.any?(RunningClient, client_id: ept)
      end
    in HttpServerRequest
      world.any?(RunningServer, server_id: belief.server_id)
    in HttpClientResponse
      world.any?(RunningClient, client_id: belief.client_id)
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
  def self.retract(fact : PropertyBelief) : Action
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
    # though, when a belief is a mix between ground truth and a property belief.
    #
    # For example, RunningPeer is one such "complicated" fact -- it is both
    # a ground truth (representing the socket connected to the peer), and
    # a property belief (about there being a server with such a peer).
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
  def self.counteract(fact : PropertyBelief) : Action
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
end
