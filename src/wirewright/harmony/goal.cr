class Ww::Harmony
  alias Goal = ActionableGoal | KeepaliveGoal

  alias ActionableGoal = Server | WebSocketHandler | Client | IngoingReceiveConfirmation |
                         OutgoingMessage | MessageSlot | HttpServerResponse | HttpClientRequest

  defcase Server, defn : ServerDefn, brief: true
  defcase Client, defn : ClientDefn, brief: true

  defcase WebSocketHandler, server_id : ServerId, link : Link

  defcase IngoingReceiveConfirmation, endpoint_id : EndpointId, msgid : MsgId, brief: true
  defcase OutgoingMessage, endpoint_id : EndpointId, payload : Term::Blob, brief: true

  defcase MessageSlot, endpoint_id : EndpointId, capacity : UInt32, brief: true

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
                      Term | Term::Blob | GoalClass | Link | UInt32

  alias GoalSet = IndexedSet(Goal, GoalFeature)
end
