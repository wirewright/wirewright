class Ww::Harmony
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

  alias Belief = PropertyBelief | IndependentBelief

  # A *property belief* is an imaginary fact tied to a percept. In other
  # words, whereas the property belief itself is imaginary, the belief's
  # existence is conditioned -- and bracketed -- by the percept.
  #
  # Continuting the analogy from `Percept`, when you look at a *green* plant,
  # the qualia of greenness is not "there" with the plant itself, in the real world.
  # It is something you ascribe to the plant. In other words, greenness is not
  # a stimulus source; it is a property of a stimulus, coming from (and thus,
  # *dependent* on) a stimulus source.
  alias PropertyBelief = RunningWebSocketHandler | RunningPeer | HttpServerRequest | MessageSlotReflection

  # An *independent belief* is an imaginary fact not tied to any ground or
  # property belief. It is entirely within the system's "subjective world",
  # and can be freely forgotten and otherwise manipulated.
  #
  # By analogy, an independent belief is a kind of "useful hallucination" --
  # an imaginary entity used for state-keeping.
  alias IndependentBelief = PendingServer | BrokenServer | PendingClient | BrokenClient | IngoingMessage |
                            IngoingReceiveConfirmation | RemoteReceiveConfirmation | RemoteMessageSlot |
                            HttpClientResponse

  defcase RunningWebSocketHandler, server_id : ServerId, brief: true

  defcase RunningServer,
    defn : ServerDefn,
    server_id : ServerId,
    info : Term::Dict,
    brief: true

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
                      String | Term | Term::Dict | Term::Blob | FactClass | HttpResponseResult

  alias FactSet = IndexedSet(Fact, FactFeature)
end
