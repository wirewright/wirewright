class Ww::Harmony
  # NOTE: Observations are strictly internal. You have no access to them from outside
  # Harmony. The only things you have access to is `Fact` and `Goal`.
  alias Observation = SocketServerStarted | HttpServerStarted | ServerStopped |
                      ServerStartFailed | ServerCrashed | MessageAccepted | MessageLost |
                      PeerConnected | PeerDisconnected | PeerCrashed | PeerReceived |
                      MessageHandled | FactForgotten | SocketClientStarted | HttpClientStarted |
                      ClientStopped | SocketClientReceived | ClientStartFailed | Ready | Busy |
                      InformedReady | InformedBusy | ActionTransferredToQueue | ActionRejected |
                      HttpRequestReceived | HttpRequestHandled | HttpResponseReceived |
                      WebSocketHandlerAdded | WebSocketHandlerRemoved

  defrecord SocketServerStarted, defn : SocketServerDefn, server_id : ServerId, queue : SocketServerQueue, brief: true

  defrecord PeerConnected, server_id : ServerId, peer_id : PeerId, queue : SocketQueue, brief: true
  defrecord PeerDisconnected, server_id : ServerId, peer_id : PeerId, brief: true
  defrecord PeerCrashed, server_id : ServerId, peer_id : PeerId, detail : String, brief: true
  defrecord PeerReceived, peer_id : PeerId, msgid : MsgId, payload : Term::Blob, brief: true

  defrecord HttpServerStarted, defn : HttpServerDefn, server_id : ServerId, queue : HttpServerQueue, brief: true
  defrecord HttpRequestReceived, server_id : ServerId, request_id : HttpRequestId, request : Term, brief: true
  defrecord HttpRequestHandled, request_id : HttpRequestId, brief: true
  defrecord WebSocketHandlerAdded, server_id : ServerId, brief: true
  defrecord WebSocketHandlerRemoved, server_id : ServerId, brief: true

  defrecord ServerStartFailed, defn : ServerDefn, detail : String, brief: true
  defrecord ServerCrashed, defn : ServerDefn, server_id : ServerId, detail : String, brief: true
  defrecord ServerStopped, defn : ServerDefn, server_id : ServerId, brief: true

  defrecord SocketClientStarted, defn : SocketClientDefn, client_id : ClientId, queue : SocketQueue, brief: true
  defrecord SocketClientReceived, client_id : ClientId, msgid : MsgId, payload : Term::Blob, brief: true

  defrecord HttpClientStarted, defn : HttpClientDefn, client_id : ClientId, queue : HttpClientQueue, brief: true
  defrecord HttpResponseReceived, client_id : ClientId, request : Term, result : HttpResponseResult, brief: true

  defrecord ClientStopped, defn : ClientDefn, client_id : ClientId, detail : String, brief: true
  defrecord ClientStartFailed, defn : ClientDefn, detail : String, brief: true

  defrecord MessageHandled, endpoint_id : EndpointId, msgid : MsgId, brief: true
  defrecord MessageAccepted, endpoint_id : EndpointId, payload : Term::Blob, brief: true
  defrecord MessageLost, endpoint_id : EndpointId, payload : Term::Blob, brief: true

  defrecord Ready, endpoint_id : EndpointId, brief: true
  defrecord Busy, endpoint_id : EndpointId, brief: true

  defrecord InformedReady, endpoint_id : EndpointId, brief: true
  defrecord InformedBusy, endpoint_id : EndpointId, brief: true

  defrecord FactForgotten, fact : Fact, brief: true
  defrecord ActionTransferredToQueue, action : Action, queue_id : UInt64, brief: true
  defrecord ActionRejected, action : Action, brief: true
end
