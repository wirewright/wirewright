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
  defrecord InformReady, endpoint_id : EndpointId, capacity : UInt32, brief: true
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

  struct ActionSet
    include Enumerable(Action)

    defrecord Orphan, brief: true
    defrecord Owned, queue_id : UInt64, brief: true

    def initialize
      @actions = {} of Action => Orphan | Owned
    end

    def includes?(action : Action) : Bool
      @actions.has_key?(action)
    end

    def each(& : Action ->) : Nil
      @actions.each { |action, _| yield action }
    end

    def add(action : Action) : Nil
      @actions.put_if_absent(action, Orphan.new)
    end

    def delete(action : Action) : Nil
      @actions.delete(action)
    end

    def transfer(action : Action, queue_id : UInt64) : Nil
      return unless status = @actions[action]?

      case status
      in Orphan
        @actions[action] = Owned.new(queue_id)
      in Owned
        raise ArgumentError.new("cannot transfer (make owned) an action that is already owned")
      end
    end

    def reject!(& : Action, Orphan | Owned -> Bool) : Nil
      @actions.reject! do |action, status|
        yield action, status
      end
    end

    def pretty_print(pp)
      pp.list("ActionSet[", @actions, "]") do |action, status|
        action.pretty_print(pp)
        pp.text("##{status}")
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
end
