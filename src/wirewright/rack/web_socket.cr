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

  defrecord Server, node : D7::Node, pool : D7::AbsEdge, binding : String, template : Term::Dict

  private def binding?(binding : Term) : String?
    Term.case(binding) do
      matchpi %{(local port←(%number u16))} do
        "tcp://127.0.0.1:#{port}"
      end

      matchpi %{(public port←(%number u16))} do
        "tcp://0.0.0.0:#{port}"
      end

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
    hg.each_node_with_head(Term.of(:cell), memberof: {variant.pool}) do |node|
      Term.case(node.term) do
        # `(circuit @pool)` appears as `(cell @pool)` initially, while it
        # holds no clients. This is an expected case.
        matchpi %{[cell @_]} do
          pools << Pool.new(node, contents: Term[])
        end

        matchpiT %{[cell @_ contents_dict]} do
          pools << Pool.new(node, contents)
        end

        otherwise { }
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

  alias Client = ConnectedClient | DisconnectedClient

  defrecord ConnectedClient,
    id : UUID,
    inbound : Term::Dict,
    outbound : Term::Dict,
    inbound_at : Int32,
    outbound_at : Int32,
    copying: true

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
      id_row : {UUID, Int32}? = nil
      in_row : {Term::Dict, Int32}? = nil
      out_row : {Term::Dict, Int32}? = nil

      children = candidate.items.move(1)
      children.each_with_index(offset: 1) do |child, key|
        Term.case(child) do
          # Recognize the id cell.
          matchpi %{[cell @id id_string]}, id: String do
            return if id_row # Duplicate `id`
            return unless uuid = UUID.parse?(id)

            id_row = {uuid, key}
          end

          # Recognize the inbox cell.
          matchpi %{[cell @in msgs_dict]} do
            return if in_row # Duplicate `in`

            in_row = {msgs.as_d, key}
          end

          # Recognize the outbox cell.
          matchpi %{[cell @out msgs_dict]} do
            return if out_row # Duplicate `out`

            out_row = {msgs.as_d, key}
          end

          otherwise { }
        end
      end

      if id_row && in_row && out_row
        return ConnectedClient.new(
          id: id_row[0],
          inbound: in_row[0],
          outbound: out_row[0],
          inbound_at: in_row[1],
          outbound_at: out_row[1],
        )
      end

      if in_row && out_row
        return DisconnectedClient.new
      end
    end
  end

  private def patch(original : Term, client : Client) : Term
    Term.morph(original,
      {client.inbound_at, 2, client.inbound},
      {client.outbound_at, 2, client.outbound},
    )
  end

  private def send(client : ConnectedClient, message : String) : Client
    client.copy_with(inbound: client.inbound.append(message))
  end

  private def drain(client : ConnectedClient) : Client
    client.copy_with(outbound: Term[])
  end

  private def each_outbound_message(client : Client, & : String ->) : Nil
    client.outbound.items.each do |item|
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
