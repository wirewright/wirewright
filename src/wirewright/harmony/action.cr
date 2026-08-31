class Ww::Harmony
  # NOTE: Actions are strictly internal. You have no access to them from outside
  # Harmony. The only things you have access to is `Fact` and `Goal`.
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
end
