class Ww::Harmony
  {% if flag?(:docs) %}
    # Constructs a `Socket` or `HTTP::WebSocket` *peer* of a server with the given *server id*,
    # and manages it until the socket closes or crashes. Reports observations pertaining to
    # the peer to *observations* (e.g., `PeerConnected`, `PeerDisconnected`, `PeerReceived`).
    #
    # *link* is the link protocol to use. It is a kind of intermediate protocol,
    # not quite transport-level, nor application-level.
    def self.peer(observations : ObservationQueue, server_id : ServerId, link : Link, socket : Socket | HTTP::WebSocket) : Nil
    end
  {% end %}

  # :nodoc:
  def self.peer(observations : ObservationQueue, server_id : ServerId, link : Link, socket : Socket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << PeerConnected.new(server_id, id, queue)

    msgloop = PeerLoop.new(observations, server_id, id, link, queue, socket)
    msgloop.run
  end

  # :nodoc:
  def self.peer(observations : ObservationQueue, server_id : ServerId, link : Link, socket : HTTP::WebSocket) : Nil
    id = PeerId.new(UUID.random)
    queue = SocketQueue.new

    spawn rxloop(queue, socket)

    observations << PeerConnected.new(server_id, id, queue)

    msgloop = PeerLoop.new(observations, server_id, id, link, queue, socket)
    msgloop.run
  end

  # Peer message loop.
  class PeerLoop
    include SocketLoop

    def initialize(
      @observations : ObservationQueue,
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

    def on_remote_ready : Nil
      @observations << RemoteReady.new(@id)
    end

    def on_remote_busy : Nil
      @observations << RemoteBusy.new(@id)
    end

    def on_sent_ready_to_remote : Nil
      @observations << SentReadyToRemote.new(@id)
    end

    def on_sent_busy_to_remote : Nil
      @observations << SentBusyToRemote.new(@id)
    end

    def on_message_delivered_to_remote(payload : Term::Blob) : Nil
      @observations << MessageDeliveredToRemote.new(@id, payload)
    end

    def on_message_declined_by_remote(payload : Term::Blob) : Nil
      @observations << MessageDeclinedByRemote.new(@id, payload)
    end

    def on_message_from_remote_accepted(msgid : MsgId) : Nil
      @observations << MessageFromRemoteAccepted.new(@id, msgid)
    end
  end
end
