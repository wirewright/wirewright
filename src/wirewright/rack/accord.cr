# Rack integration with `Harmony`.
module Ww::Rack::Accord
  extend self

  defcase State,
    schemas : GenerationalCache(Term, Schema::JSON),
    harmony : Harmony

  def state(epoch : Automaton::Epoch) : State
    schemas = GenerationalCache(Term, Schema::JSON).new
    harmony = Harmony.new(-> { epoch.call })
    State.new(schemas, harmony)
  end

  def pending?(state : State) : Bool
    state.harmony.pending?
  end

  def deadline?(state : State) : Time::Instant?
    state.harmony.deadline?
  end

  defrecord StepContext, world : Harmony::World, goals : Set(Harmony::Goal)

  def step(state : State, & : Propose -> T) : T forall T
    state.harmony.observe

    goals = Set(Harmony::Goal).new

    result = state.schemas.epoch do
      ctx = StepContext.new(state.harmony.world, goals)
      propose = Propose.new do |hg, proposals|
        propose(state, ctx, hg, proposals)
      end
      yield propose
    end

    state.harmony.submit(goals)
    state.harmony.reconcile

    result
  end

  private def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :server) do |node|
      Term.case(node.term) do
        matchpi %{[server (@_ _ dn) _*]} { }
        matchpi %{[server (@_ _ (dn _string)) _*]} { }

        matchpi(<<-WWML) do
        [server
          (@pool_ transportQ_ _?
            ⍊ in: (%optional @in @input_)
              out: (%optional @out @output_)
              format: (%optional none formatQ_)
              format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless defn = server_transport?(transportQ)
          continue unless format = Format.format?(state.schemas, hg, node, formatQ)
          continue unless format_policy = Format.policy?(policyQ)

          abs_pool = hg.resolve(node.addr, pool)
          machine = stack_alloc Server.new(node, defn, abs_pool, input, output, template.as_d, format, format_policy)
          step(ctx, hg, machine)
        end

        otherwise { }
      end
    end

    hg.propose(proposals, :client) do |node|
      Term.case(node.term) do
        # - Allow the circuit to use an errorless `dn` to disable the socket.
        # - Use `closed` instead of simply `dn` to avoid confusing server-side
        #   closure (`closed`) with client-side closure (`dn`).
        matchpi %{[client [@_ -> _ -> @_] (%any dn closed)]} { }
        matchpi %{[client [@_ -> _ -> @_] (dn _string)]} { }

        matchpi(<<-WWML) do
        [client
          (@outgoing_ -> transportQ_ -> @ingoing_
            ⍊ format: (%optional none formatQ_)
              format-policy: (%optional discard policyQ_))
          _?]
        WWML
          continue unless defn = client_transport?(transportQ)
          continue unless format = Format.format?(state.schemas, hg, node, formatQ)
          continue unless format_policy = Format.policy?(policyQ)

          abs_outgoing = hg.resolve(node.addr, outgoing)
          abs_ingoing = hg.resolve(node.addr, ingoing)
          machine = stack_alloc Client.new(node, defn, abs_outgoing, abs_ingoing, format, format_policy)
          step(ctx, hg, machine)
        end

        otherwise { }
      end
    end
  end

  private def server_transport?(term : Term) : Harmony::ServerDefn?
    # |@ rack.server.transport
    #
    # |@summary
    # Transports supported by the server node.
    Term.case(term) do
      # |@ rack.server.transport
      #
      # |@pattern
      # (ws local port←(%number u16))
      #
      # |@block
      # A plain WebSocket server at 127.0.0.1:*port*. If there is an existing HTTP
      # server at *port* (within the same circuit!), extends it with WebSocket support.
      matchpiT %{(ws local port←(%number u16))} do
        Harmony::WsServerDefn.new("127.0.0.1", port)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (ws public port←(%number u16))
      #
      # |@block
      # A plain WebSocket server at 0.0.0.0:*port*. If there is an existing HTTP
      # server at *port* (in the same circuit!), extends it with WebSocket support.
      matchpiT %{(ws public port←(%number u16))} do
        Harmony::WsServerDefn.new("0.0.0.0", port)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (ws host_string port←(%number u16))
      #
      # |@block
      # A plain WebSocket server at *host*:*port*. If there is an existing HTTP server
      # at *port* (in the same circuit!), extends it with WebSocket support.
      matchpiT %{(ws host_string port←(%number u16))}, host: String do
        Harmony::WsServerDefn.new(host, port)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (tcp local port←(%number u16))
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at 127.0.0.1:*port*.
      matchpiT %{(tcp local port←(%number u16))} do
        Harmony::TcpServerDefn.new("127.0.0.1", port)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (tcp public port←(%number u16))
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at 0.0.0.0:*port*.
      matchpiT %{(tcp public port←(%number u16))} do
        Harmony::TcpServerDefn.new("0.0.0.0", port)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (tcp host_string port←(%number u16))
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at 0.0.0.0:*port*.
      matchpiT %{(tcp host_string port←(%number u16))}, host: String do
        Harmony::TcpServerDefn.new(host, port)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (unix path_string)
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over a Unix socket at *path*.
      matchpiT %{(unix path_string)}, path: NormalPath do
        Harmony::UnixServerDefn.new(path)
      end

      otherwise { }
    end
  end

  private def client_transport?(term : Term) : Harmony::ClientDefn?
    # |@ rack.client.transport
    #
    # |@summary
    # Transports supported by the client node.
    Term.case(term) do
      # |@ rack.client.transport.key
      #
      # |@summary
      # The `key` pair accepted by all client transports.
      #
      # |@block
      # By default, all clients with equal transport will share the same underlying
      # connection (regardless of where they are in the circuit or how many of them
      # there are!)
      #
      # ```wwml
      # (module {}
      #   (cell @in)
      #   (cell @out)
      #   (client (@in -> (ws local 5000) -> @out)))
      #
      # (module {}
      #   (cell @in)
      #   (cell @out)
      #   (client (@in -> (ws local 5000) -> @out)))
      # ```
      #
      # This can be rewritten more explicitly as:
      #
      # ```wwml
      # (module {}
      #   (cell @in)
      #   (cell @out)
      #   (client (@in -> (ws local 5000 key: master) -> @out)))
      #
      # (module {}
      #   (cell @in)
      #   (cell @out)
      #   (client (@in -> (ws local 5000 key: master) -> @out)))
      # ```
      #
      # Notice how both clients share the same connection, named `master`.
      #
      # In order to create distinct connections for each client node, you should set
      # the *key* pair to a suitable key (e.g. a random number or a UUID).
      #
      # ```wwml
      # (module {}
      #   (cell @in)
      #   (cell @out)
      #   (client (@in -> (ws local 5000 key: "Alice's connection") -> @out)))
      #
      # (module {}
      #   (cell @in)
      #   (cell @out)
      #   (client (@in -> (ws local 5000 key: "Bob's connection") -> @out)))
      # ```

      # |@ rack.client.transport
      #
      # |@pattern
      # (ws local port←(%number u16) ⍊ key_⋮ master path⋮ "" secure⋮ false)
      #
      # |@block
      # A plain WebSocket client at 127.0.0.1:*port* on *path*. Establishes a secure
      # connection (TLS) if *secure* is `true`.
      #
      # See `rack.client.transport.key` to learn more about *key*.
      matchpiT %{(ws local port←(%number u16) ⍊ key_⋮ master path⋮ "" secure⋮ false)}, path: String do
        Harmony::WsClientDefn.new("127.0.0.1", port, path, key, secure.true?)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (ws public port←(%number u16) ⍊ key_⋮ master path⋮ "" secure⋮ false)
      #
      # |@block
      # A plain WebSocket client at 0.0.0.0:*port* on *path*. Establishes a secure
      # connection (TLS) if *secure* is `true`.
      #
      # See `rack.client.transport.key` to learn more about *key*.
      matchpiT %{(ws public port←(%number u16) ⍊ key_⋮ master path⋮ "" secure⋮ false)}, path: String do
        Harmony::WsClientDefn.new("0.0.0.0", port, path, key, secure.true?)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (ws host_string port←(%number u16) ⍊ key_⋮ master path⋮ "" secure⋮ false)
      #
      # |@block
      # A plain WebSocket client at *host*:*port* on *path*. Establishes a secure
      # connection (TLS) if *secure* is `true`.
      #
      # See `rack.client.transport.key` to learn more about *key*.
      matchpiT %{(ws host_string port←(%number u16) ⍊ key_⋮ master path⋮ "" secure⋮ false)}, host: String, path: String do
        Harmony::WsClientDefn.new(host, port, path, key, secure.true?)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (tcp local port←(%number u16) ⍊ key_⋮ master)
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at 127.0.0.1:*port*.
      #
      # See `rack.client.transport.key` to learn more about *key*.
      matchpiT %{(tcp local port←(%number u16) ⍊ key_⋮ master)} do
        Harmony::TcpClientDefn.new("127.0.0.1", port, key)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (tcp public port←(%number u16) ⍊ key_⋮ master)
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at 0.0.0.0:*port*.
      #
      # See `rack.client.transport.key` to learn more about *key*.
      matchpiT %{(tcp public port←(%number u16) ⍊ key_⋮ master)} do
        Harmony::TcpClientDefn.new("0.0.0.0", port, key)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (tcp host_string port←(%number u16) ⍊ key_⋮ master)
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at *host*:*port*.
      #
      # See `rack.client.transport.key` to learn more about *key*.
      matchpiT %{(tcp host_string port←(%number u16) ⍊ key_⋮ master)}, host: String do
        Harmony::TcpClientDefn.new(host, port, key)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (unix path_string ⍊ key_⋮ master)
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over a Unix socket at *path*.
      #
      # See `rack.client.transport.key` to learn more about *key*.
      matchpi %{(unix path_string ⍊ key_⋮ master)}, path: NormalPath do
        Harmony::UnixClientDefn.new(path, key)
      end

      otherwise { }
    end
  end

  alias Machine = Server | Client

  defcase Server,
    node : D7::Node,
    defn : Harmony::ServerDefn,
    pool : D7::AbsEdge,
    in_edge : Term,
    out_edge : Term,
    template : Term::Dict,
    format : Format::Any,
    format_policy : Format::Policy

  defcase Client,
    node : D7::Node,
    defn : Harmony::ClientDefn,
    outgoing : D7::AbsEdge,
    ingoing : D7::AbsEdge,
    format : Format::Any,
    format_policy : Format::Policy

  def step(ctx : StepContext, hg : D7::Hypergraph, server : Server) : D7::Patch?
    # If there's no associated pool, then the server's "machine" is incomplete,
    # so it cannot handle requests -- nor does it *exist*, really.
    unless pool = Rack.pool?(hg, server.pool)
      # (server (@_ _ ⏏) _*)
      return D7.patch(server.node, {1, 2, {:dn, "missing pool"}})
    end

    ctx.goals.add(Harmony::Server.new(server.defn))

    status = Term.of(:pending)
    incarnation = nil

    ctx.world.each(Harmony::RunningServer) do |fact|
      next unless fact.defn == server.defn

      status = Term.of(:up)
      incarnation = fact.server_id
      break
    end

    if incarnation.nil?
      ctx.world.each(Harmony::BrokenServer) do |fact|
        next unless fact.defn == server.defn

        status = Term.of(:pending, fact.detail)
        break
      end
    end

    # (server (@_ _ ⏏) _*)
    status_patch = D7.patch(server.node, {1, 2, status})

    pool_patch = D7::Patch.new
    pass do
      skip = Set(Harmony::PeerId).new # HACK

      devices = device_map(pool.contents) do |device|
        next unless ctx.world.any?(Harmony::RunningPeer, device.id) { true }

        update(skip, ctx, server, device)
      end

      # Process new connections.
      ctx.world.each(Harmony::RunningPeer) do |fact|
        next if fact.peer_id.in?(skip)

        peer = Harmony::PeerKeepalive.new(fact.peer_id)
        next if peer.in?(ctx.goals)

        device = ClientDevice.new(fact.peer_id, inbox: Term[], outbox: Term[])
        device = update(skip, ctx, server, device)
        devices = devices.append(render(server, device))
      end

      pool_patch = D7.patch(pool.node, {2, devices})
    end

    D7.patches(status_patch, pool_patch)
  end

  private def update(skip, ctx : StepContext, server : Server, device : ClientDevice) : ClientDevice
    id = device.id
    inbox = device.inbox?
    outbox = device.outbox?

    # If both are nil, then this device is closed. We do not want to keep the link
    # with it alive this frame.
    if inbox || outbox
      ctx.goals.add(Harmony::PeerKeepalive.new(id))
    else
      skip << id
    end

    # Process an inbound message.
    pass do
      next unless inbox
      next unless ingoing = mark?(ctx, id)

      case msgin = receive?(ctx, ingoing, server.format, server.format_policy)
      in Nil
      in Received
        device = device.copy_with(inbox: inbox.append(msgin.term))
      in Aborted
        # We get this with format-policy: abort, when a message fails to decode.
      end
    end

    # Process an outbound message.
    pass do
      next unless outbox
      next unless msgout = outbox.items.first?

      # If the message fails to encode (e.g. due to limits) we "clog" the output
      # cell so that failure is evident. Clogging is the natural semantic here:
      # the "pipe" of the socket is too "narrow" to pass a large or malformed
      # (e.g., square thing into a round hole) message.
      #
      # TODO: We should also provide a descriptive error message explaining why
      # the thing doesn't encode!
      next unless msgout = Format.encode?(server.format, msgout)

      if ctx.world.includes?(Harmony::RemoteReceiveConfirmation.new(id, msgout))
        # Consider it sent, erase the message.
        device = device.copy_with(outbox: outbox.items.move(1).collect)
        next
      end

      # Keep wanting to send the message while it's in the cell and no
      # acknowledgement exists.
      ctx.goals.add(Harmony::OutgoingMessage.new(id, msgout))
    end

    device
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, client : Client) : D7::Patch?
    ctx.goals.add(Harmony::Client.new(client.defn))

    status = Term.of(:pending)
    incarnation = nil

    ctx.world.each(Harmony::RunningClient) do |fact|
      next unless fact.defn == client.defn

      incarnation = fact.client_id
      status = Term.of(:up)
      break
    end

    if incarnation.nil?
      ctx.world.each(Harmony::BrokenClient) do |fact|
        next unless fact.defn == client.defn

        status = Term.of(:pending, fact.detail)
        break
      end
    end

    status_patch = D7.patch(client.node, {2, status})

    # Handle the source.
    source_patch = nil
    pass do
      next unless incarnation
      next unless source = Rack.cell?(hg, client.outgoing)
      next unless message = source.value?

      # If the message fails to encode (e.g. due to limits) we "clog" the message
      # cell so that failure is evident.
      #
      # TODO: We should also provide a descriptive error message explaining why
      # the thing doesn't encode!
      next unless message = Format.encode?(client.format, message)

      if ctx.world.includes?(Harmony::RemoteReceiveConfirmation.new(incarnation, message))
        # Consider it sent, erase the message.
        source_patch = D7.patch(source.node, {2, nil})
        next
      end

      # Keep wanting to send the message while it's in the cell and no
      # acknowledgement exists.
      ctx.goals.add(Harmony::OutgoingMessage.new(incarnation, message))
    end

    # Handle the target.
    target_patch = nil
    pass do
      next unless incarnation
      next unless ingoing = mark?(ctx, incarnation)
      next unless target = Rack.cell?(hg, client.ingoing)
      next unless target.value?.nil?

      case reply = receive?(ctx, ingoing, client.format, client.format_policy)
      in Nil
      in Received
        target_patch = D7.patch(target.node, {2, reply.term})
      in Aborted
        # We get this with format-policy: abort, when a message fails to decode.
        status_patch = D7.patch(client.node, {2, {:dn, "invalid message"}})
      end
    end

    D7.patches(
      status_patch,
      source_patch || D7::Patch.new,
      target_patch || D7::Patch.new,
    )
  end

  defrecord ClientDevice,
    id : Harmony::PeerId,
    inbox : Term::Dict?,
    outbox : Term::Dict?,
    smart: true,
    copying: true

  private def render(server : Server, device : ClientDevice) : Term
    device = Term::Dict.build do |commit|
      commit << :device
      commit << {:cell, {:edge, :id}, device.id.repr}
      commit << {:cell, server.in_edge, device.inbox?}
      commit << {:cell, server.out_edge, device.outbox?}
      commit.concat(server.template.items)
    end

    Term.of(device)
  end

  private def device_map(pool : Term::Dict, & : ClientDevice -> ClientDevice?) : Term::Dict
    Term.flatten(pool) do |_, candidate|
      Term.case(candidate) do
        matchpi %{[device _*]} do
          children = candidate.items.move(1)

          device_id = nil
          device_inbox = nil
          device_outbox = nil

          children.zip(1u32...candidate.itemsize) do |child, key|
            Term.case(child) do
              matchpi %{[cell @id idQ_string]}, idQ: String do
                next unless repr = UUID.parse?(idQ)

                device_id = Harmony::PeerId.new(repr)
              end

              matchpi %{[cell @in]} do
                device_inbox = {key, nil}
              end

              matchpi %{[cell @out]} do
                device_outbox = {key, nil}
              end

              matchpi %{[cell @in content←(_*)]}, content: Term::Dict do
                device_inbox = {key, content}
              end

              matchpi %{[cell @out content←(_*)]}, content: Term::Dict do
                device_outbox = {key, content}
              end

              otherwise { }
            end
          end

          # Devices without an ID get removed.
          if device_id.nil?
            next Term.rep
          end

          input = ClientDevice.new(device_id,
            device_inbox ? device_inbox[1] : Term[],
            device_outbox ? device_outbox[1] : Term[],
          )

          output = yield input
          if output.nil?
            next Term.rep
          end

          if device_inbox
            # (device ... (cell @in ⏏) ...)
            candidate = Term.morph(candidate, {device_inbox[0], 2, output.inbox?})
          end

          if device_outbox
            # (device ... (cell @out ⏏) ...)
            candidate = Term.morph(candidate, {device_outbox[0], 2, output.outbox?})
          end

          Term.rep(candidate)
        end

        # Keep everything else as-is.
        otherwise do
          Term.rep(candidate)
        end
      end
    end
  end

  # Keeps alive all incoming messages for *endpoint id*. Returns the one with
  # the smallest seq number.
  private def mark?(ctx : StepContext, endpoint_id : Harmony::EndpointId) : Harmony::IngoingMessage?
    ingoing = nil

    ctx.world.each(Harmony::IngoingMessage, endpoint_id) do |fact|
      ctx.goals << Harmony::IngoingMessageKeepalive.new(fact.endpoint_id, fact.seq)

      if ingoing.nil? || fact.seq < ingoing.seq
        ingoing = fact
      end
    end

    ingoing
  end

  defrecord Received, term : Term
  defrecord Aborted

  # Performs the receive part of the mark-and-receive cycle. Assumes all ingoing messages
  # for the endpoint of interest were `mark?`ed first, and *ingoing* is the minimum seq
  # such **marked** message.
  private def receive?(ctx : StepContext, ingoing : Harmony::IngoingMessage, format : Format::Any, format_policy : Format::Policy) : Received | Aborted | Nil
    confirmation = Harmony::IngoingReceiveConfirmation.new(ingoing.endpoint_id, ingoing.seq)

    # Initiate confirmation. If confirmation is a fact, this means it's complete.
    unless ctx.world.includes?(confirmation)
      ctx.goals << confirmation
      return
    end

    # Now that we've handled the message, we don't want to keep it alive.
    ctx.goals.delete(Harmony::IngoingMessageKeepalive.new(ingoing.endpoint_id, ingoing.seq))

    # When the world contains *confirmation*, this means we've completed reception
    # of the message.

    term = Format.decode?(format, ingoing.payload)

    case format_policy
    in .discard?
      return if term.nil?
    in .abort?
      if term.nil?
        return Aborted.new
      end
    in .wrap?
      if term
        term = Term.of(:ok, term)
      else
        term = Term.of(:err, "invalid message")
      end
    end

    Received.new(term)
  end
end
