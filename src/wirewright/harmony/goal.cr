class Ww::Harmony
  alias Goal = ActionableGoal | KeepaliveGoal

  alias ActionableGoal = Server | WebSocketHandler | Client | MessageInLocalInbox |
                         OutgoingMessage | MessageCapacity | HttpServerResponse | HttpClientRequest

  # Represents the caller's desire to have a running server specified by *defn*.
  defcase Server, defn : ServerDefn, brief: true

  # Represents the caller's deisre to have a running client specified by *defn*.
  defcase Client, defn : ClientDefn, brief: true

  # Represents the caller's desire to have a web socket handler attached to an HTTP
  # server with the given *id*, using *link* as the link.
  defcase WebSocketHandler, server_id : ServerId, link : Link

  # Represents the caller's desire to keep *msgid* in *endpoint*'s mailbox.
  #
  # NOTE: This goal can also appear as a fact, which acts as a proof that all necessary
  # actions were carried out to realize the desire.
  #
  # NOTE: There is no mailbox. It's all a metaphor.
  defcase MessageInLocalInbox, endpoint_id : EndpointId, msgid : MsgId, brief: true

  # Represents the caller's desire to send *payload* to *endpoint*'s remote side.
  defcase OutgoingMessage, endpoint_id : EndpointId, payload : Term::Blob, brief: true

  # Indicates that the local side of *endpoint* is ready to receive up to
  # *capacity* messages.
  defcase MessageCapacity, endpoint_id : EndpointId, capacity : UInt32, brief: true

  # Represents the caller's desire that *server* responds to *request* with *response*
  # (expressed in the HTTP response language `HttpResponseLanguage`).
  defcase HttpServerResponse,
    server_id : ServerId,
    request_id : HttpRequestId,
    response : Term,
    brief: true

  # Represents the caller's desire that *client* sends a *request* (expressed in
  # the HTTP request language `HttpRequestLanguage`).
  defcase HttpClientRequest,
    client_id : ClientId,
    request : Term,
    brief: true

  # A keepalive goal is a goal which is used to keep some fact true. Think of it
  # as a kind of "mental effort" token of the system which keeps some "thoughts"
  # or "beliefs" up.
  alias KeepaliveGoal = PeerKeepalive | IngoingMessageKeepalive | HttpServerRequestKeepalive

  # Represents the caller's desire to preserve a link between a peer and a server from
  # garbage collection, and thus, to keep it open.
  defcase PeerKeepalive, peer_id : PeerId, brief: true

  # Represents the caller's desire to preserve an ingoing message fact from garbage
  # collection. See also: `IngoingMessage.`
  defcase IngoingMessageKeepalive, endpoint_id : EndpointId, msgid : MsgId, brief: true

  # Represents the caller's desire to preserve a *request* an HTTP server received.
  # This goal is "willed" while the request is being processed. If it is not "willed",
  # the request will be rejected by Harmony.
  defcase HttpServerRequestKeepalive, request_id : HttpRequestId, brief: true

  {% begin %}
    # :nodoc:
    alias GoalClass = Union({{Goal.union_types.map(&.class).splat}})
  {% end %}

  # :nodoc:
  alias GoalFeature = ServerDefn | ClientDefn | ServerId | EndpointId | MsgId | HttpRequestId |
                      Term | Term::Blob | GoalClass | Link | UInt32

  alias GoalSet = IndexedSet(Goal, GoalFeature)
end
