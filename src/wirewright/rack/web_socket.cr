module Ww::Rack::WebSocket
  extend self

  defcase State,
    epoch : Automaton::Epoch,
    running : Set(String),
    subscription : WebSocketService::Subscription?,
    mutation: true

  def state(epoch : Automaton::Epoch) : State
    State.new(epoch, running: Set(String).new, subscription: nil)
  end

  def pending?(state : State) : Bool
    state.running.present?
  end

  defrecord StepContext, bindings : Set(String)

  def step(state : State, & : Proposer -> T) : T forall T
    seen_bindings = Set(String).new

    ctx = StepContext.new(seen_bindings)
    result = yield Proposer.new(state, ctx)

    if state.running.empty? && !seen_bindings.empty?
      state.subscription = WebSocketService.subscribe(state.epoch)
    elsif !state.running.empty? && seen_bindings.empty?
      assert subscription = state.subscription
      WebSocketService.unsubscribe(subscription)
    end

    # Handle servers started.
    seen_bindings.each do |binding|
      next if binding.in?(state.running)

      WebSocketService.start(binding)
    end

    # Handle servers stopped.
    state.running.each do |binding|
      next if binding.in?(seen_bindings)

      WebSocketService.stop(binding)
    end

    state.running = seen_bindings

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
    template : Term::Dict

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
        matchpi %{[ws (@pool_ bindingQ_ server _?) template_*]} do
          continue unless binding = binding?(bindingQ)

          variant = Server.new(node, node.resolve(pool), binding, template.as_d)
          step(state, ctx, hg, variant)
        end

        otherwise { }
      end
    end
  end

  defrecord Pool, node : D7::Node, contents : Term::Dict

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, variant : Server) : D7::Patch?
    # Find the associated pool cell.
    pools = Pf::Kit.stack_array(Pool, 1)
    hg.each_node_with_head(Term.of(:pool), memberof: {variant.pool}) do |node|
      Term.matchpiT?(node.term, %{[pool @_ contents_dict]}) do
        pools << Pool.new(node, contents)
      end
    end

    # If there's no associated pool, then the server's "machine" is incomplete,
    # so it cannot handle requests -- nor does it *exist*, really.
    unless pool = pools.single?
      return D7.patch(variant.node, {1, 3, {:dn, "missing pool"}})
    end

    ctx.bindings << variant.binding

    case status = WebSocketService.checkout?(variant.binding)
    in Nil, WebSocketService::Pending
      # (ws (_ _ server ⏏) _*)
      D7.patch(variant.node, {1, 3, :pending})
    in WebSocketService::Dn # Error
      # (ws (_ _ server ⏏) _*)
      D7.patch(variant.node, {1, 3, {:dn, status.detail}})
    in WebSocketService::Up
      journal = status.journal

      contents0 = pool.contents
      contents1 = pool.contents

      # Process events from the journal.
      journal.each do |event|
        case event
        in WebSocketService::ClientConnected
          contents1 = contents1.append(client(event.id, variant.template))
        in WebSocketService::ClientDisconnected
          contents1 = fmap(contents1) do |client|
            case client
            in ConnectedClient
              client.id == event.id ? nil : client
            in DisconnectedClient
            end
          end
        in WebSocketService::ClientReceived
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
            WebSocketService.send(variant.binding, client.id, message)
          end

          drain(client)
        in DisconnectedClient
        end
      end

      # Find clients that disconnected / were "deformed" so much that we
      # can't see them.
      status.clients.each do |client_id|
        next if client_id.in?(seen)

        WebSocketService.drop(variant.binding, client_id)
      end

      # (ws (_ _ server ⏏) _*)
      # (circuit @pool ⏏)
      D7.patches(
        D7.patch(variant.node, {1, 3, :up}),
        D7.patch(pool.node, {2, contents1}),
      )
    end
  end

  defrecord ClientQueue, key : Int32, queue : Term::Dict, copying: true

  alias Client = ConnectedClient | DisconnectedClient

  defrecord ConnectedClient,
    id : UUID,
    inbound : ClientQueue?,
    outbound : ClientQueue?,
    copying: true,
    smart: true

  defrecord DisconnectedClient

  private def client(id : UUID, template : Term::Dict) : Term
    device = Term::Dict.build do |commit|
      commit << :device
      commit << {:cell, {:edge, :id}, id.to_s}
      commit.concat(template.items)
    end

    Term.of(device)
  end

  private def client?(candidate : Term) : Client?
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

  private def patch(original : Term, client : Client) : Term
    result = original

    if inbound = client.inbound?
      result = Term.morph(result, {inbound.key, 2, inbound.queue})
    end

    if outbound = client.outbound?
      result = Term.morph(result, {outbound.key, 2, outbound.queue})
    end

    result
  end

  private def send(client : ConnectedClient, message : String) : Client
    return client unless inbound = client.inbound?

    client.copy_with(inbound: inbound.copy_with(queue: inbound.queue.append(message)))
  end

  private def drain(client : ConnectedClient) : Client
    return client unless outbound = client.outbound?

    client.copy_with(outbound: outbound.copy_with(queue: Term[]))
  end

  private def each_outbound_message(client : Client, & : String ->) : Nil
    return unless outbound = client.outbound?

    outbound.queue.items.each do |item|
      if str = item.as_s?
        yield str.to(String)
        next
      end

      yield ML.compact(item)
    end
  end

  private def fmap(contents : Term::Dict, & : Client -> Client?) : Term::Dict
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
end
