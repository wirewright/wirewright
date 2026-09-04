class Ww::Harmony
  alias Fact = Percept | Belief

  # A *percept* is a manifestation of a resource, an actor, etc. As a consequence,
  # it cannot be simply removed; you must tell the underlying resource or actor
  # to remove or stop itself, which will in turn cause the percept to be removed.
  alias Percept = RunningServer | RunningClient

  # *Beliefs* are all facts that are not `Percept`s.
  alias Belief = PropertyBelief | IndependentBelief

  # A *property belief* is an imaginary fact tied to a percept. In other words,
  # whereas the property belief itself is imaginary, the belief's existence is
  # conditioned -- and bracketed -- by the percept.
  alias PropertyBelief = RunningWebSocketHandler | RunningPeer | HttpServerRequest | LocalMessageDemand

  # An *independent belief* is an imaginary fact not tied to any percept or
  # property belief. It is entirely within the system's "subjective world",
  # and can be forgotten and otherwise manipulated. Independent beliefs are
  # used for state-keeping. They are the traces entities and events leave in
  # the system, which may continue to exist long after their cause is gone.
  alias IndependentBelief = PendingServer | BrokenServer | PendingClient | BrokenClient |
                            IngoingMessage | MessageInLocalInbox | MessageInRemoteInbox |
                            RemoteMessageDemand | HttpClientResponse

  # A web socket handler attached to an HTTP server with the given *id*.
  defcase RunningWebSocketHandler, server_id : ServerId, brief: true

  # Represents a server that was started successfully and is running according to *defn*.
  # *info* may contain extra information about the server, such as the port number in
  # case *defn*'s port is set to auto.
  defcase RunningServer,
    defn : ServerDefn,
    server_id : ServerId,
    info : Term::Dict,
    brief: true

  # Represents a server in a retriable broken state.
  #
  # This is only used before we succeed starting the server. After that, any breaks
  # result in a `BrokenServer`. Although this might seem strange, it's much easier
  # for the circuit to detect semantic breaks this way (breaks in the middle of
  # communication). If the circuit wants a reconnect, when it's ready, it can
  # simply replace the `dn` status with nothing to make it go to PendingServer
  # again on our side, with retries and backoff.
  defcase PendingServer, defn : ServerDefn, detail : String, brief: true

  # Represents a server in an untretriable broken state.
  defcase BrokenServer, defn : ServerDefn, detail : String, brief: true

  # Represents a peer of a server with the given *server id*.
  defcase RunningPeer, server_id : ServerId, peer_id : PeerId, brief: true

  # Represents a client that connected successfully and is running according to *defn*.
  defcase RunningClient, defn : ClientDefn, client_id : ClientId, brief: true

  # Represents a client in a retriable broken state.
  defcase PendingClient, defn : ClientDefn, detail : String, brief: true

  # Represents a client in an unretriable broken state. To retry, the client must
  # be stopped and reinserted.
  defcase BrokenClient, defn : ClientDefn, detail : String, brief: true

  # Represents an ingoing message designated for *receiver*, that is buffered
  # by (contained in) the world but not yet accepted into any mailbox. Ingoing
  # messages must be kept alive by the `IngoingMessageKeepalive` goal; otherwise,
  # they will be garbage collected.
  defcase IngoingMessage,
    receiver_id : EndpointId,
    msgid : MsgId,
    payload : Term::Blob,
    brief: true

  # Represents the local side's message *payload* residing in the remote side's mailbox.
  # Effectively, this confirms (acts as an acknowledgement of) message delivery: when Alice
  # mails to Bob and then observes her message in Bob's inbox (and not just floating about),
  # she knows her message got through and Bob will see it.
  defcase MessageInRemoteInbox,
    endpoint_id : EndpointId,
    payload : Term::Blob,
    brief: true

  # Indicates that the local side has spare capacity for one or more messages. It is
  # the reflection of the `MessageCapacity` goal on the fact side. `LocalMessageDemand`
  # is realized only when the remote side has been notified of the local side's
  # readiness to accept more messages.
  defcase LocalMessageDemand, endpoint_id : EndpointId, brief: true

  # Indicates that the remote side has spare capacity for one or more messages.
  defcase RemoteMessageDemand, endpoint_id : EndpointId, brief: true

  # Represents a request received by an HTTP server with the given *id*.
  defcase HttpServerRequest,
    server_id : ServerId,
    request_id : HttpRequestId,
    request : Term,
    brief: true

  # Represents a response received by an HTTP client with the given *id*.
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

  # :nodoc:
  alias FactFeature = ServerDefn | ClientDefn | ServerId | EndpointId | MsgId | HttpRequestId |
                      String | Term | Term::Dict | Term::Blob | FactClass | HttpResponseResult

  alias FactSet = IndexedSet(Fact, FactFeature)
end
