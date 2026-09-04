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

    def on_message_received_by_peer(payload : Term::Blob) : Nil
      @observations << MessageReceivedByPeer.new(@id, payload)
    end

    def on_message_not_sent(payload : Term::Blob) : Nil
      @observations << MessageNotSent.new(@id, payload)
    end

    def on_message_handled(msgid : MsgId) : Nil
      @observations << MessageHandled.new(@id, msgid)
    end
  end
end
