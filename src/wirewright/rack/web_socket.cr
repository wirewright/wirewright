module Ww::Rack::WebSocket
  extend self

  defcase State,
    serving : Set(String),
    connected_to : Set(WebSocketClientService::Conn),
    subscription : ->,
    mutation: true

  def state(epoch : Automaton::Epoch) : State
    serving = Set(String).new
    connected_to = Set(WebSocketClientService::Conn).new
    subscription = -> { epoch.call }
    State.new(serving, connected_to, subscription)
  end

  def pending?(state : State) : Bool
    state.serving.present? || state.connected_to.present?
  end

  defrecord StepContext,
    bindings : Set(String),
    conns : Set(WebSocketClientService::Conn),
    dequeue : Set(WebSocketClientService::Conn)

  def step(state : State, & : Proposer -> T) : T forall T
    seen_bindings = Set(String).new
    seen_conns = Set(WebSocketClientService::Conn).new
    dequeue = Set(WebSocketClientService::Conn).new

    ctx = StepContext.new(seen_bindings, seen_conns, dequeue)
    result = yield Proposer.new(state, ctx)

    if state.serving.empty? && !seen_bindings.empty?
      WebSocketServerService.subscribe(state.subscription)
    elsif !state.serving.empty? && seen_bindings.empty?
      WebSocketServerService.unsubscribe(state.subscription)
    end

    if state.connected_to.empty? && !seen_conns.empty?
      WebSocketClientService.subscribe(state.subscription)
    elsif !state.connected_to.empty? && seen_conns.empty?
      WebSocketClientService.unsubscribe(state.subscription)
    end

    # Handle servers started.
    seen_bindings.each do |binding|
      next if binding.in?(state.serving)

      WebSocketServerService.start(binding)
    end

    # Handle servers stopped.
    state.serving.each do |binding|
      next if binding.in?(seen_bindings)

      WebSocketServerService.stop(binding)
    end

    # Handle each client added.
    seen_conns.each do |conn|
      unless conn.in?(state.connected_to)
        WebSocketClientService.connect(conn)
        next
      end

      if conn.in?(dequeue)
        # To maintain synchronicity, we only do *reads* in `step` for `Client`.
        # If there are many `ws` nodes, all of them get the same message; which
        # we then dequeue here, once per connection.
        WebSocketClientService.dequeue(conn)
        next
      end
    end

    # Handle each client removed.
    state.connected_to.each do |conn|
      next if conn.in?(seen_conns)

      WebSocketClientService.disconnect(conn)
    end

    state.connected_to = seen_conns
    state.serving = seen_bindings

    result
  end

  struct Proposer
    def initialize(@state : State, @ctx : StepContext)
    end

    def propose(hg : D7::Hypergraph, proposals) : Nil
      WebSocket.propose(@state, @ctx, hg, proposals)
    end
  end

  defrecord Server,
    node : D7::Node,
    pool : D7::AbsEdge,
    binding : String,
    in_edge : Term,
    out_edge : Term,
    template : Term::Dict

  defrecord Client,
    node : D7::Node,
    message : D7::AbsEdge,
    conn : WebSocketClientService::Conn,
    reply : D7::AbsEdge

  private def binding?(binding : Term) : String?
    Term.case(binding) do
      # |@ rack.ws.binding
      #
      # |@pattern
      # (local port←(%number u16))
      #
      # |@block
      # Binds to a local *port* (127.0.0.1).
      matchpi %{(local port←(%number u16))} do
        "tcp://127.0.0.1:#{port}"
      end

      # |@ rack.ws.binding
      #
      # |@pattern
      # (public port←(%number u16))
      #
      # |@block
      # Binds to a public *port* (0.0.0.0).
      matchpi %{(public port←(%number u16))} do
        "tcp://0.0.0.0:#{port}"
      end

      # |@ rack.ws.binding
      #
      # |@pattern
      # _string
      #
      # |@block
      # Passes a raw binding URI down to the networking machinery. Refer to Crystal
      # [`HTTP::Server` docs](https://crystal-lang.org/api/1.21.0/HTTP/Server.html#bind%28uri%3AString%29%3ASocket%3A%3AAddress-instance-method).
      matchpi %{_string} do
        binding.to(String)
      end

      otherwise { }
    end
  end

  # :nodoc:
  def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :ws) do |node|
      Term.case(node.term) do
        matchpi %{[ws (@pool_ bindingQ_ server _? ⍊ in: (%optional @in @input_) out: (%optional @out @output_)) template_*]} do
          continue unless binding = binding?(bindingQ)

          variant = Server.new(node, hg.resolve(node.addr, pool), binding, input, output, template.as_d)
          step(state, ctx, hg, variant)
        end

        # Allow the circuit to use an errorless `dn` to disable the socket.
        matchpi %{[ws (@_ -> _ -> @_) dn]} do
        end

        matchpi %{[ws (@message_ -> connQ_ -> @reply_) _?]} do
          continue unless conn = WebSocketClientService.conn?(connQ)

          variant = Client.new(node, hg.resolve(node.addr, message), conn, hg.resolve(node.addr, reply))
          step(state, ctx, hg, variant)
        end

        otherwise { }
      end
    end
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, variant : Server) : D7::Patch?
    # If there's no associated pool, then the server's "machine" is incomplete,
    # so it cannot handle requests -- nor does it *exist*, really.
    unless pool = Rack.pool?(hg, variant.pool)
      return D7.patch(variant.node, {1, 3, {:dn, "missing pool"}})
    end

    ctx.bindings << variant.binding

    case status = WebSocketServerService.checkout?(variant.binding)
    in Nil, WebSocketServerService::Pending
      # (ws (_ _ server ⏏) _*)
      D7.patch(variant.node, {1, 3, :pending})
    in WebSocketServerService::Dn # Error
      # (ws (_ _ server ⏏) _*)
      D7.patch(variant.node, {1, 3, {:dn, status.detail}})
    in WebSocketServerService::Up
      journal = status.journal

      contents0 = pool.contents
      contents1 = pool.contents

      # Process events from the journal.
      journal.each do |event|
        case event
        in WebSocketServerService::ClientConnected
          contents1 = contents1.append(client_repr(event.id, variant))
        in WebSocketServerService::ClientDisconnected
          contents1 = fmap(contents1) do |client|
            case client
            in ConnectedClient
              client.id == event.id ? nil : client
            in DisconnectedClient
            end
          end
        in WebSocketServerService::ClientReceived
          contents1 = fmap(contents1) do |client|
            case client
            in ConnectedClient
              unless client.id == event.id
                next client
              end

              send(client, event.message)
            in DisconnectedClient
            end
          end
        end
      end

      seen = Set(UUID).new

      # Find outbound messages from clients.
      contents1 = fmap(contents1) do |client|
        case client
        in ConnectedClient
          seen << client.id
          each_outbound_message(client) do |message|
            WebSocketServerService.send(variant.binding, client.id, message)
          end

          drain(client)
        in DisconnectedClient
        end
      end

      # Find clients that disconnected / were "deformed" so much that we
      # can't see them.
      status.clients.each do |client_id|
        next if client_id.in?(seen)

        WebSocketServerService.drop(variant.binding, client_id)
      end

      # (ws (_ _ server ⏏) _*)
      # (circuit @pool ⏏)
      D7.patches(
        D7.patch(variant.node, {1, 3, :up}),
        D7.patch(pool.node, {2, contents1}),
      )
    end
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, variant : Client) : D7::Patch?
    ctx.conns << variant.conn

    case event = WebSocketClientService.checkout?(variant.conn)
    in Nil # Disconnected
      status_patch = D7.patch(variant.node, {2, nil})
    in WebSocketClientService::Up
      status_patch = D7.patch(variant.node, {2, :up})
    in WebSocketClientService::Dn
      status_patch = D7.patch(variant.node, {2, {:dn, event.detail}})
    in WebSocketClientService::Pending
      status_patch = D7.patch(variant.node, {2, :pending})
    end

    source_patch = pass do
      next unless source = Rack.cell?(hg, variant.message)
      next unless message = source.value?

      message = stringify(message)
      next unless WebSocketClientService.send?(variant.conn, message)

      D7.patch(source.node, {2, nil})
    end

    target_patch = pass do
      next unless target = Rack.cell?(hg, variant.reply)
      next unless target.value?.nil?
      next unless reply = WebSocketClientService.head?(variant.conn)

      ctx.dequeue << variant.conn

      D7.patch(target.node, {2, reply})
    end

    D7.patches(
      status_patch,
      source_patch || D7::Patch.new,
      target_patch || D7::Patch.new,
    )
  end

  defrecord ClientQueue, key : Int32, queue : Term::Dict, copying: true

  alias ClientRepr = ConnectedClient | DisconnectedClient

  defrecord ConnectedClient,
    id : UUID,
    inbound : ClientQueue?,
    outbound : ClientQueue?,
    copying: true,
    smart: true

  defrecord DisconnectedClient

  private def client_repr(id : UUID, variant : Server) : Term
    device = Term::Dict.build do |commit|
      commit << :device
      commit << {:cell, {:edge, :id}, id.to_s}
      commit << {:cell, variant.in_edge, Term[]}
      commit << {:cell, variant.out_edge, Term[]}
      commit.concat(variant.template.items)
    end

    Term.of(device)
  end

  private def client?(candidate : Term) : ClientRepr?
    Term.matchpi?(candidate, %{[device _*]}) do
      id : UUID? = nil
      inbound : ClientQueue? = nil
      outbound : ClientQueue? = nil

      children = candidate.items.move(1)
      children.each_with_index(offset: 1) do |child, key|
        Term.case(child) do
          # Recognize the id cell.
          matchpi %{[cell @id idQ_string]}, idQ: String do
            return if id # Duplicate `id`
            return unless uuid = UUID.parse?(idQ)

            id = uuid
          end

          # Recognize the inbox cell.
          matchpi %{[cell @in msgs←(_string*)]} do
            next if inbound # Duplicate `in`

            inbound = ClientQueue.new(key, msgs.as_d)
          end

          # Recognize the outbox cell.
          matchpi %{[cell @out msgs←(_string*)]} do
            next if outbound # Duplicate `out`

            outbound = ClientQueue.new(key, msgs.as_d)
          end

          otherwise { }
        end
      end

      unless id
        return DisconnectedClient.new
      end

      ConnectedClient.new(id, inbound, outbound)
    end
  end

  private def patch(original : Term, client : ClientRepr) : Term
    result = original

    if inbound = client.inbound?
      result = Term.morph(result, {inbound.key, 2, inbound.queue})
    end

    if outbound = client.outbound?
      result = Term.morph(result, {outbound.key, 2, outbound.queue})
    end

    result
  end

  private def send(client : ConnectedClient, message : String) : ClientRepr
    return client unless inbound = client.inbound?

    client.copy_with(inbound: inbound.copy_with(queue: inbound.queue.append(message)))
  end

  private def drain(client : ConnectedClient) : ClientRepr
    return client unless outbound = client.outbound?

    client.copy_with(outbound: outbound.copy_with(queue: Term[]))
  end

  private def each_outbound_message(client : ClientRepr, & : String ->) : Nil
    return unless outbound = client.outbound?

    outbound.queue.items.each do |item|
      yield stringify(item)
    end
  end

  private def fmap(contents : Term::Dict, & : ClientRepr -> ClientRepr?) : Term::Dict
    Term.flatten(contents) do |_, item|
      unless client0 = client?(item)
        next Term.rep(item)
      end

      if client1 = yield client0
        rep = Term.rep(patch(item, client1))
      else
        rep = Term.rep
      end

      rep
    end
  end

  private def stringify(term : Term) : String
    if str = term.as_s?
      return str.to(String)
    end

    ML.compact(term)
  end
end
