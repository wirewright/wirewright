struct Ww::Harmony::Exchange
  def initialize
    @socket_servers = {} of ServerId => SocketServerQueue
    @sockets = {} of EndpointId => SocketQueue
    @http_servers = {} of ServerId => HttpServerQueue
    @http_clients = {} of ClientId => HttpClientQueue
    @queue_ids = Set(UInt64).new
  end

  def dead?(queue_id : UInt64) : Bool
    !queue_id.in?(@queue_ids)
  end

  def []?(peer : EndpointId, cls : SocketQueue.class) : SocketQueue?
    @sockets[peer]?
  end

  def []?(server : ServerId, cls : SocketServerQueue.class) : SocketServerQueue?
    @socket_servers[server]?
  end

  def []?(server : ServerId, cls : HttpServerQueue.class) : HttpServerQueue?
    @http_servers[server]?
  end

  def []?(client : ClientId, cls : HttpClientQueue.class) : HttpClientQueue?
    @http_clients[client]?
  end

  def []=(peer : EndpointId, cls : SocketQueue.class, queue : SocketQueue) : SocketQueue
    @sockets[peer] = queue
    @queue_ids << queue.seq_id
    queue
  end

  def []=(server : ServerId, cls : SocketServerQueue.class, queue : SocketServerQueue) : SocketServerQueue
    @socket_servers[server] = queue
    @queue_ids << queue.seq_id
    queue
  end

  def []=(server : ServerId, cls : HttpServerQueue.class, queue : HttpServerQueue) : HttpServerQueue
    @http_servers[server] = queue
    @queue_ids << queue.seq_id
    queue
  end

  def []=(client : ClientId, cls : HttpClientQueue.class, queue : HttpClientQueue) : HttpClientQueue
    @http_clients[client] = queue
    @queue_ids << queue.seq_id
    queue
  end

  def delete(peer : EndpointId, cls : SocketQueue.class) : SocketQueue?
    return unless queue = @sockets.delete(peer)

    @queue_ids.delete(queue.seq_id)
    queue
  end

  def delete(server : ServerId, cls : SocketServerQueue.class) : SocketServerQueue?
    return unless queue = @socket_servers.delete(server)

    @queue_ids.delete(queue.seq_id)
    queue
  end

  def delete(server : ServerId, cls : HttpServerQueue.class) : HttpServerQueue?
    return unless queue = @http_servers.delete(server)

    @queue_ids.delete(queue.seq_id)
    queue
  end

  def delete(client : ClientId, cls : HttpClientQueue.class) : HttpClientQueue?
    return unless queue = @http_clients.delete(client)

    @queue_ids.delete(queue.seq_id)
    queue
  end
end
