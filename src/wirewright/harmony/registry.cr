struct Ww::Harmony::Registry
  def initialize
    @servers = {} of ServerId => ServerQueue
    @sockets = {} of EndpointId => SocketQueue
  end

  def []?(peer : EndpointId) : SocketQueue?
    @sockets[peer]?
  end

  def []=(peer : EndpointId, queue : SocketQueue) : SocketQueue
    @sockets[peer] = queue
  end

  def delete(peer : EndpointId) : SocketQueue?
    @sockets.delete(peer)
  end

  def []?(server : ServerId) : ServerQueue?
    @servers[server]?
  end

  def []=(server : ServerId, queue : ServerQueue) : ServerQueue
    @servers[server] = queue
  end

  def delete(server : ServerId) : ServerQueue?
    @servers.delete(server)
  end
end
