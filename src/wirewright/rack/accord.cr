# Integrates Rack with `Harmony`.
module Ww::Rack::Accord
  extend self

  # :nodoc:
  defcase State,
    schemas : GenerationalCache(Term, Schema::JSON),
    harmony : Harmony,
    acknowledged : Set(Harmony::PeerId | Harmony::HttpRequestId)

  def state(epoch : Automaton::Epoch) : State
    schemas = GenerationalCache(Term, Schema::JSON).new
    harmony = Harmony.new(-> { epoch.call })
    acknowledged = Set(Harmony::PeerId | Harmony::HttpRequestId).new
    State.new(schemas, harmony, acknowledged)
  end

  def pending?(state : State) : Bool
    state.harmony.pending?
  end

  def deadline?(state : State) : Time::Instant?
    state.harmony.deadline?
  end

  # :nodoc:
  #
  # NOTE: *world* is a logical snapshot of the current world, which must be used in
  # a read-only way by all step() overloads. The same is true for *acknowledged*.
  # The set of *goals*, on the other hand, starts empty and is meant to be populated
  # by the step() overloads.
  defrecord StepContext,
    goals : Harmony::GoalSet,
    world : Harmony::ReadonlyWorld,
    acknowledged : Set(Harmony::PeerId | Harmony::HttpRequestId)

  def step(state : State, & : Propose -> T) : T forall T
    changelog = state.harmony.observe

    goals = Harmony::GoalSet.new

    result = state.schemas.epoch do
      propose = Propose.new do |hg, proposals|
        ctx = StepContext.new(goals, state.harmony.world, state.acknowledged)
        propose(state, ctx, hg, proposals)
      end
      yield propose
    end

    state.harmony.submit(goals)
    state.harmony.reconcile

    # NOTE: Importantly, we update the acknowledged set *after* proposal(),
    # because it should actually represent the state of affairs *before*
    # proposal(), *during* proposal().
    changelog.each do |change|
      fact = change.element
      case {change, fact}
      when {Set::Changelog::Added, Harmony::RunningPeer}
        state.acknowledged.add(fact.peer_id)
      when {Set::Changelog::Added, Harmony::HttpServerRequest}
        state.acknowledged.add(fact.request_id)
      when {Set::Changelog::Removed, Harmony::RunningPeer}
        state.acknowledged.delete(fact.peer_id)
      when {Set::Changelog::Removed, Harmony::HttpServerRequest}
        state.acknowledged.delete(fact.request_id)
      end
    end

    result
  end

  private def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :server) do |node|
      Term.case(node.term) do
        # Skip servers that are currently down.
        matchpi %{[server [@_ _ dn] _*]} { }
        matchpi %{[server [@_ _ (dn _string)] _*]} { }

        matchpi(<<-WWML) do
        [server
          (@pool_ transportQ←(ws _* ⍊ link: (%optional direct linkQ_)) _?
            ⍊ in: (%optional @in @input_)
              out: (%optional @out @output_)
              format: (%optional text formatQ_)
              format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless defn = http_server_transport?(hg, node.addr, transportQ)

          next unless link = link?(linkQ)
          next unless format = Format.format?(state.schemas, hg, node, formatQ)
          next unless format_policy = Format.policy?(policyQ)

          abs_pool = hg.resolve(node.addr, pool)
          machine = stack_alloc WebSocketServer.new(node, defn, link, abs_pool, input, output, template.as_d, format, format_policy)
          step(ctx, hg, machine)
        end

        matchpi(<<-WWML) do
        [server
          (@pool_ transportQ_ _?
            ⍊ request: (%optional @request @request_)
              response: (%optional @response @response_)
              format: (%optional text formatQ_)
              format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless defn = http_server_transport?(hg, node.addr, transportQ)

          next unless format = Format.format?(state.schemas, hg, node, formatQ)
          next unless format_policy = Format.policy?(policyQ)

          abs_pool = hg.resolve(node.addr, pool)
          machine = stack_alloc HttpServer.new(node, defn, abs_pool, request, response, template.as_d, format, format_policy)
          step(ctx, hg, machine)
        end

        matchpi(<<-WWML) do
        [server
          (@pool_ transportQ_ _?
            ⍊ in: (%optional @in @input_)
              out: (%optional @out @output_)
              format: (%optional text formatQ_)
              format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless defn = socket_server_transport?(hg, node.addr, transportQ)

          next unless format = Format.format?(state.schemas, hg, node, formatQ)
          next unless format_policy = Format.policy?(policyQ)

          abs_pool = hg.resolve(node.addr, pool)
          machine = stack_alloc SocketServer.new(node, defn, abs_pool, input, output, template.as_d, format, format_policy)
          step(ctx, hg, machine)
        end

        otherwise { }
      end
    end

    hg.propose(proposals, :client) do |node|
      Term.case(node.term) do
        # Allow the circuit to use an errorless `dn` to disable the socket. Also
        # ignore clients that are currently down for other reasons.
        matchpi %{[client [@_ -> _ -> @_] dn]} { }
        matchpi %{[client [@_ -> _ -> @_] (dn _string)]} { }

        matchpi(<<-WWML) do
        [client
          (@outgoing_ -> transportQ_ -> @ingoing_
            ⍊ format: (%optional text formatQ_)
              format-policy: (%optional discard policyQ_))
          _?]
        WWML
          defn = http_client_transport?(hg, node.addr, transportQ) ||
                 socket_client_transport?(hg, node.addr, transportQ)

          continue unless defn

          next unless format = Format.format?(state.schemas, hg, node, formatQ)
          next unless format_policy = Format.policy?(policyQ)

          abs_outgoing = hg.resolve(node.addr, outgoing)
          abs_ingoing = hg.resolve(node.addr, ingoing)

          if defn.is_a?(Harmony::HttpClientDefn)
            machine = stack_alloc HttpClient.new(node, defn, abs_outgoing, abs_ingoing, format, format_policy)
          else
            machine = stack_alloc SocketClient.new(node, defn, abs_outgoing, abs_ingoing, format, format_policy)
          end

          step(ctx, hg, machine)
        end

        otherwise { }
      end
    end
  end

  private def link?(term : Term) : Harmony::Link?
    # |@ rack.[network].link
    #
    # |@summary
    # The available modes of link.
    #
    # |@block
    # *link* determines how individual payloads are transmitted over
    # the selected transport (WebSockets, TCP, etc.)
    Term.case(term) do
      # |@ rack.[network].link
      #
      # |@pattern
      # portal
      #
      # |@block
      # Uses the internal Portal protocol to transmit the payload.
      #
      # Linking with `portal` is more reliable than with `direct`, and interacts
      # well with the semantics of Rack.
      #
      # For example, a client's outgoing message cell is not emptied until the message
      # crosses over to the other side, which provides a natural kind of backpressure;
      # nor are messages sent until the other side tells its ingoing message cell
      # is empty.
      #
      # The main drawback of `link: portal` is that it places more load on
      # the network, involving round-trips and so on.
      #
      # ### Portal
      #
      # The Portal protocol supports the following messages:
      #
      # - `DATA <msgid> <payload bytes...>`: Alice sends *payload* to Bob,
      #    with Alice's message id `<msgid>` of choice. `<msgid>` is 1-16
      #    hex digits (a 64-bit unsigned integer written in hex).
      # - `ACCEPT <msgid>`: Bob confirms that he received Alice's payload
      #    with the given *msgid*. Alice is free to remove *msgid* on her side.
      # - `READY`: Bob sends this to Alice to signal that his "mailbox" is empty;
      #   he is ready to receive the next message, if any.
      # - `BUSY`: Bob sends this to Alice to signal that his "mailbox" is full;
      #   he cannot receive any messages yet.
      #
      # Due to the way the protocol is designed and implemented right now (and I doubt
      # huge improvements to the current behavior are possible...), `READY` and `BUSY`
      # are *advisory* on the protocol level. Moreover, they can be sent by either party
      # at any point in time.
      #
      # The protocol places no demands on the order of messages, nor on the state or
      # statefulness of senders, receivers, or both.
      #
      # However, `rack.server` and `rack.client` in particular demand readiness of
      # the other party before they send and, in turn, report their own readiness.
      #
      # An important point is races. Races are definitely possible with this protocol.
      # Let's say Bob sends READY to Alice, which triggers Alice to start sending her
      # DATA to Bob; simultaneously, Bob changes his mind and sends BUSY. We observe
      # the two messages passing each other in the wire. Alice finishes sending DATA
      # and receives Bob's BUSY; Bob finishes sending BUSY and receives Alice's DATA.
      #
      # The above *advisory* label covers the case described here. DATA will be buffered
      # and processed normally as in `link: direct`; but it will be shown to Bob
      # only when he is ready, just as he sends the READY message to Alice.
      #
      # In theory, this could create a persistent backlog of one message, but I'm not sure
      # about that. Moreover, such a mode gets rid of the guarantee that "absent in my
      # outgoing cell" means "present in their ingoing cell". Importantly, however, all
      # this is only true when you explicitly write into the ingoing cell. If Portal has
      # full control over the cell, and you only look at it or clear it (e.g. by moving
      # the message it contains elsewhere, or by literally clearing it), then I'd expect
      # no races of the kind I described. In other words, as far as I understand, it is
      # possible to "break" this protocol (to an extent), but only if you actively interfere
      # with its normal functioning. One possible fix could be to use some sort of a "token",
      # a "microphone" the parties pass between each other to speak. But I'm not sure.
      matchpi %{portal} do
        Harmony::PortalLink.new
      end

      # |@ rack.[network].link
      #
      # |@pattern
      # direct
      #
      # |@block
      # Direct passthrough of the payload to the underlying transport.
      #
      # - No application-level backpressure (senders do not care about receivers).
      # - Message sends are confirmed locally (senders do not care about acknowledgement
      #   or feedback about the message they sent from receivers).
      #
      # More importantly, with direct link, there is a window of time when the message
      # is neither on the sender's side nor on the receiver's side -- it is "in the wire". If
      # anything happens to the connection while a message is travelling in the wire, the message
      # is lost. So you wouldn't want to e.g. transfer money between peers with `link: direct`.
      matchpi %{direct} do
        Harmony::DirectLink.new
      end

      otherwise { }
    end
  end

  private def host?(term : Term) : String?
    # |@ rack.[network].host
    #
    # |@summary
    # Description of a server or client host.
    Term.case(term) do
      # |@ rack.[network].host
      #
      # |@pattern
      # local
      #
      # |@block
      # Shorthand for `127.0.0.1`.
      matchpi %{local} do
        "127.0.0.1"
      end

      # |@ rack.[network].host
      #
      # |@pattern
      # public
      #
      # |@block
      # Shorthand for `0.0.0.0`.
      matchpi %{public} do
        "0.0.0.0"
      end

      # |@ rack.[network].host
      #
      # |@pattern
      # _string
      #
      # |@block
      # A generic host string, e.g., `"1.2.3.4"` or `"example.org"`.
      matchpi %{_string} do
        term.to(String)
      end

      otherwise { }
    end
  end

  private def port?(term : Term) : Harmony::ServerPort?
    # |@ rack.[network].port
    #
    # |@summary
    # Description of a server or client port.
    Term.case(term) do
      # |@ rack.[network].port
      #
      # |@pattern
      # (%number u16)
      #
      # |@block
      # A constant port.
      #
      # |@example
      # ```wwml
      # 5000
      # ```
      matchpi %{(%number u16)} do
        Harmony::ExclusiveServerPort.new(term.to(UInt16))
      end

      # |@ rack.[network].port
      #
      # |@pattern
      # (shared port←(%number u16))
      #
      # |@block
      # A shared constant port (`SO_REUSEPORT`).
      #
      # |@example
      # ```wwml
      # (shared 5000)
      # ```
      matchpiT %{(shared port←(%number u16))} do
        Harmony::SharedServerPort.new(port)
      end

      # |@ rack.[network].port
      #
      # |@pattern
      # auto
      #
      # |@block
      # Asks the operating system for an unused port. The port can be learned
      # from the server's `up`, which for servers with an `auto` port is different
      # from the normal `up`, in that it also includes the port: `(up (%number u16))`.
      matchpi %{auto} do
        Harmony::AutoServerPort.new
      end

      otherwise { }
    end
  end

  # |@ rack.[network].key
  #
  # |@summary
  # The `key` pair accepted by all client and server transports.
  #
  # |@block
  # By default, all clients and servers with the same transport (same by
  # value; equal) will share the same underlying connection, or the same
  # underlying HTTP/socket server, regardless of where they are in the circuit
  # or how many of them there are.
  #
  # This may come as a strange design choice. But the opposite choice -- to make
  # all nodes be separate connections or servers -- is also not a very good one,
  # in particular because nodes have no identity beyond content identity,
  # i.e., transport.
  #
  # This is particularly relevant for clients. If you have, say, a hundred
  # components, and each for some reason wants access to an HTTP client,
  # instead of doing complex routing to a single HTTP client, you can just
  # give each component its own HTTP client node. If they share the same key
  # (and they do share the same default `master` key if you don't change
  # it explicitly) -- if they share the same key, then the same connection
  # will be used for all of them.
  #
  # Thus you get the benefit of both decentralization and isolation (no complex
  # interaction beyond the "membrane") of each component, and centralization
  # (no needless connection duplication, running out of fds, etc.) Moreover --
  # you should thank Rack for this -- concurrent access is managed completely
  # transparently for you.
  #
  # What is said above applies to servers, too; a server's transport can also
  # have a `key: _`. But this is more of a rarity; it's not often that you put
  # a server inside each button, say (whereas it would sense to put a client in
  # each button, if e.g. the button is responsible for sending a request).
  #
  # |@example
  # Consider this circuit:
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
  # It can be rewritten more explicitly as:
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
  # Notice how both clients share the same key, `master`. Therefore, they are
  # currently "views" of the same underlying WebSocket. Whenever the WebSocket
  # receives anything, all `@out` cells of its "views" are going to be populated.
  #
  # In order to create a different connection for each client node, you should
  # set the *key* pair to a suitable key (e.g. a random number or a UUID).
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
  #
  # The key can be a `rack.edge`:
  #
  # ```wwml
  # (cell @id "Alice's connection")
  # (cell @in)
  # (cell @out)
  # (client (@in -> (ws local 5000 key: @id) -> @out))
  # ```
  #
  # This is particularly useful when you already have an id in a cell (such as in
  # client devices of a `rack.server`), or when you can extract one from a cell
  # (e.g. using the `rack.part` node).
  #
  # If there are zero or more than one cells at the edge, or if the cell is empty,
  # the entire transport is invalidated. The client (and the underlying connection)
  # are not started until the key is known.
  #
  # Changing the key dynamically will make the client "join" and "leave"
  # different connections.
  private def key?(hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Term?
    unless Term.edge?(term)
      return term
    end

    return unless cell = Rack.cell?(hg, hg.resolve(addr, term))
    return unless value = cell.value?

    value
  end

  # |@ rack.server.transport
  #
  # |@summary
  # Transports supported by the server node.

  private def socket_server_transport?(hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Harmony::SocketServerDefn?
    Term.case(term) do
      # |@ rack.server.transport
      #
      # |@pattern
      # (tcp host_ port_ ⍊ key_⋮ master link_⋮ direct)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at *host*:*port*.
      #
      # |@example
      # Here is how you can create a simple TCP echo server.
      #
      # ```wwml
      # (server (@pool (tcp local 5000))
      #   (feed (@in front) (@out back)))
      #
      # (pool @pool)
      # ```
      matchpiT %{(tcp hostQ_ portQ_ ⍊ key: (%optional master keyQ_) link: (%optional direct linkQ_))} do
        return unless host = host?(hostQ)
        return unless port = port?(portQ)
        return unless key = key?(hg, addr, keyQ)
        return unless link = link?(linkQ)

        Harmony::TcpServerDefn.new(host, port, key, link)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (unix path_string ⍊ key_⋮ master link_⋮ direct)
      #
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over a Unix socket at *path*.
      #
      # |@example
      # Here is how you can create a simple UNIX echo server. Please note that
      # the file at *path* must not exist. Otherwise, the server will refuse
      # to start until *path* is absent -- at which point it will start just fine.
      #
      # ```wwml
      # (server (@pool (unix "/tmp/example.sock"))
      #   (feed (@in front) (@out back)))
      #
      # (pool @pool)
      # ```
      matchpiT %{(unix path_string ⍊ key: (%optional master keyQ_) link: (%optional direct linkQ_))}, path: NormalPath do
        return unless key = key?(hg, addr, keyQ)
        return unless link = link?(linkQ)

        Harmony::UnixServerDefn.new(path, key, link)
      end

      otherwise { }
    end
  end

  private def socket_client_transport?(hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Harmony::SocketClientDefn?
    # |@ rack.client.transport
    #
    # |@summary
    # Transports supported by the client node.
    Term.case(term) do
      # |@ rack.client.transport.renew
      #
      # |@summary
      # The `renew` pair which all client transports accept.
      #
      # |@block
      # If `renew: true`, uses `(pending _string)` instead of `(dn _string)` when
      # a previously established connection breaks or closes.
      #
      # `renew: false` by default.
      #
      # Rack clients attempt to *connect* repeatedly, with backoff. However, they do
      # not attempt automatic *re*connects after (or in case) an *established* connection
      # breaks. You can enable reconnects in such cases by setting `renew: true`.
      #
      # We do not enable reconnects by default because doing so could create state sync bugs and
      # the like -- when the client reconnects faster than you can detect the connection was dropped,
      # a distant cousin of the ABA problem. It would be your problem, of course, not `client`'s --
      # it gives you exactly one frame where the `dn` is there, so you can detect it and suspend
      # everything quickly. But if you do not do that, we would rather give you a reliable way
      # to detect closure at your own pace and repair things, than consume `dn` silently and
      # leave you confused. A new connection is a new connection, after all, and we'd like
      # the boundary in between to be clearly recongizable.
      #
      # If you protocol or the way you use `client` allows you to, you may actually want
      # automatic reconnects. That's why `renew: true` exists, to relieve you of the need
      # to manually reset the client.
      #
      # You can still reconnect a broken client by clearing its status, either manually
      # (by literally deleting it) or through rules.

      # |@ rack.client.transport
      #
      # |@pattern
      # (ws host_ port_ ⍊ key_⋮ master ⋮link path⋮ "" renew⋮ false)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      # |@key renew rack.client.transport.renew
      #
      # |@block
      # A plain WebSocket client at *host*:*port* on *path*.
      #
      # |@example
      # Here is how you can connect to a WebSocket server running at `127.0.0.1:5000`,
      # and send it the message `"Kaixo mundua"`.
      #
      # ```wwml
      # (cell @in "Kaixo mundua")
      # (cell @out)
      # (client (@in -> (ws local 5000) -> @out))
      # ```
      #
      # If the server is a simple echo server, the above would evolve as follows:
      #
      # ```wwml
      # ;; Frame 1 (assuming the client connected successfully)
      # ;; The message is travelling over the wire.
      #
      # (cell @in)
      # (cell @out)
      # (client (@in -> (ws local 5000) -> @out) up)
      #
      # ;; Frame 2
      # ;; The echo has arrived.
      #
      # (cell @in)
      # (cell @out "Kaixo mundua")
      # (client (@in -> (ws local 5000) -> @out) up)
      # ```
      matchpiT(<<-'WWML', path: String) do
      (ws hostQ_ port←(%number u16)
        ⍊ key: (%optional master keyQ_)
          link: (%optional direct linkQ_)
          path⋮ ""
          renew⋮ false)
      WWML
        return unless host = host?(hostQ)
        return unless link = link?(linkQ)
        return unless key = key?(hg, addr, keyQ)

        security = nil
        Harmony::WsClientDefn.new(host, port, path, key, security, link, renew.true?)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (wss host_ port_ ⍊ key_⋮ master path⋮ "" ⋮link renew⋮ false)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      # |@key renew rack.client.transport.renew
      #
      # |@key verify
      # Whether to verify the certificate.
      # - `true` corresponds to `SSL_VERIFY_PEER`.
      # - `false` corresponds to `SSL_VERIFY_NONE`.
      #
      # |@block
      # A plain WebSocket client at *host*:*port* on *path*. Establishes a secure
      # connection using TLS.
      #
      # |@example
      # ```wwml
      # (cell @in "Kaixo mundua")
      # (cell @out)
      # (client (@in -> (wss local 5000) -> @out))
      # ```
      matchpiT(<<-'WWML', path: String) do
      (wss hostQ_ port←(%number u16)
        ⍊ key: (%optional master keyQ_)
          link: (%optional direct linkQ_)
          path⋮ ""
          renew⋮ false
          verify⋮ true)
      WWML
        return unless host = host?(hostQ)
        return unless link = link?(linkQ)
        return unless key = key?(hg, addr, keyQ)

        security = Harmony::TlsClientConfig.new(verify.true?)
        Harmony::WsClientDefn.new(host, port, path, key, security, link, renew.true?)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (tcp host_ port_ ⍊ key_⋮ master ⋮link renew⋮ false)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      # |@key renew rack.client.transport.renew
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at *host*:*port*.
      #
      # |@example
      # ```wwml
      # (cell @in "Kaixo mundua")
      # (cell @out)
      # (client (@in -> (tcp local 5000) -> @out))
      # ```
      matchpiT(<<-'WWML') do
      (tcp hostQ_ port←(%number u16)
        ⍊ key: (%optional master keyQ_)
          link: (%optional direct linkQ_)
          renew⋮ false)
      WWML
        return unless host = host?(hostQ)
        return unless link = link?(linkQ)
        return unless key = key?(hg, addr, keyQ)

        Harmony::TcpClientDefn.new(host, port, key, link, renew.true?)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (unix path_string ⍊ key_⋮ master ⋮link renew⋮ false)
      #
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      # |@key renew rack.client.transport.renew
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over a Unix socket at *path*.
      #
      # |@example
      # ```wwml
      # (cell @in "Kaixo mundua")
      # (cell @out)
      # (client (@in -> (unix "/tmp/example.sock") -> @out))
      # ```
      matchpi(<<-'WWML', path: NormalPath) do
      (unix path_string
        ⍊ key: (%optional master keyQ_)
          link: (%optional direct linkQ_)
          renew⋮ false)
      WWML
        return unless link = link?(linkQ)
        return unless key = key?(hg, addr, keyQ)

        Harmony::UnixClientDefn.new(path, key, link, renew.true?)
      end

      otherwise { }
    end
  end

  private def http_server_transport?(hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Harmony::HttpServerDefn?
    Term.case(term) do
      # |@ rack.server.transport
      #
      # |@pattern
      # (http host_ port_ ⍊ key_⋮ master)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      #
      # |@block
      # An HTTP server at *host*:*port*.
      #
      # HTTP requests use the HTTP request language (see `http.request`).
      # HTTP responses use the HTTP response language (see `http.response`).
      #
      # |@example
      # Here's a simple HTTP server that displays `Hello World` on the home page,
      # and `Not found` on all other pages. It'll refuse all other requests with
      # status code 400.
      #
      #
      # ```wwml
      # (server (@pool (http local 5000))
      #   (backsys
      #     {¦ -response_}
      #       <> {response: (bad-request)}
      #     {¦ request: [get _] -response_}
      #       <> {response: (not-found ⟬‸<h1 style="color: red">Not found</h1>‸⟭)}
      #     {¦ request: [get ["/"]] -response_}
      #       <> {response: (ok ⟬‸<h1>Hello World</h1>‸ ⁑ text/html⟭)}))
      #
      # (pool @pool)
      # ```
      #
      # If we send an unsupported request:
      #
      # ```wwml
      # (cell @in (post "/" "hi"))
      # (cell @out)
      # (client (@in -> (http local 5000) -> @out))
      # ```
      #
      # We get the correct response:
      #
      # ```wwml
      # (cell @in)
      # (cell @out (bad-request))
      # (client (@in -> (http local 5000) -> @out) up)
      # ```

      # |@ rack.server.transport
      #
      # |@pattern
      # (ws host_ port_ ⍊ key_⋮ master link_⋮ direct)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      #
      # |@block
      # A plain WebSocket server at *host*:*port*. If there is an existing HTTP server
      # at *port* (within the same circuit!), extends it with WebSocket support.
      #
      # |@example
      # Here's how you can create a simple WebSocket echo server.
      #
      # ```wwml
      # (server (@pool (ws local 5000))
      #   (feed (@in front) (@out back)))
      #
      # (pool @pool)
      # ```

      matchpiT(
        %{(http hostQ_ portQ_ ⍊ key: (%optional master keyQ_))},
        %{(ws hostQ_ portQ_ ⍊ key: (%optional master keyQ_))},
      ) do
        return unless host = host?(hostQ)
        return unless port = port?(portQ)
        return unless key = key?(hg, addr, keyQ)

        Harmony::HttpServerDefn.new(host, port, key, security: nil)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (https host_ port_ ⍊ key_⋮ master ssl-cert_string ssl-key_string)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      #
      # |@key ssl-cert
      # Path to the file containing the public certificate chain.
      #
      # |@key ssl-key
      # Path to the private key file.
      #
      # |@block
      # An HTTPS server at *host*:*port*. Uses TLS for security.
      #
      # HTTP requests use the HTTP request language (see `http.request`).
      # HTTP responses use the HTTP response language (see `http.response`).
      #
      # |@example
      # HTTPS support... still requires a great deal of improvement (and
      # understanding on my end), but you should be able to get a server
      # running with something along the lines of:
      #
      # ```wwml
      # (server (@pool (https local 5000 ssl-cert: "path/to/openssl.cert" ssl-key: "path/to/openssl.key"))
      #   (backsys
      #     {¦ request: [get ["/"]] -response_}
      #       <> {response: (ok "Hello")}))
      # ```

      # |@ rack.server.transport
      #
      # |@pattern
      # (wss host_ port_ ⍊ key_⋮ master link_⋮ direct ssl-cert_string ssl-key_string)
      #
      # |@key host rack.[network].host
      # |@key port rack.[network].port
      # |@key key rack.[network].key
      # |@key link rack.[network].link
      #
      # |@key ssl-cert
      # Path to the file containing the public certificate chain.
      #
      # |@key ssl-key
      # Path to the private key file.
      #
      # |@block
      # A plain WebSocket server at *host*:*port* (using TLS). If there is
      # an existing HTTPS server at *port* (within the same circuit!), extends
      # it with WebSocket support.
      #
      # |@example
      # Works similar to `https`. Here's a simple echo server:
      #
      # ```wwml
      # (server (@pool (wss local 5000 ssl-cert: "path/to/openssl.cert" ssl-key: "path/to/openssl.key"))
      #   (feed (@in front) (@out back)))
      #
      # (pool @pool)
      # ```

      matchpiT(
        %{(https hostQ_ portQ_ ⍊ key: (%optional master keyQ_) ssl-cert_string ssl-key_string)},
        %{(wss hostQ_ portQ_ ⍊ key: (%optional master keyQ_) ssl-cert_string ssl-key_string)},
        ssl_cert: NormalPath,
        ssl_key: NormalPath,
      ) do
        return unless host = host?(hostQ)
        return unless port = port?(portQ)
        return unless key = key?(hg, addr, keyQ)

        tls_config = Harmony::TlsServerConfig.new(ssl_cert, ssl_key)
        Harmony::HttpServerDefn.new(host, port, key, security: tls_config)
      end

      otherwise { }
    end
  end

  private def http_client_transport?(hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Harmony::HttpClientDefn?
    Term.case(term) do
      # |@ rack.client.transport
      #
      # |@pattern
      # (http host_ port_ ⍊ key_⋮ master)
      # (http host_ ⍊ key_⋮ master)
      #
      # |@key host rack.[network].host
      #
      # |@key port rack.[network].port
      # If omitted, uses the default HTTP port 8080.
      #
      # |@key key rack.[network].key
      #
      # |@block
      # Connects to an HTTP server at *host*:*port*.
      #
      # HTTP requests use the HTTP request language (see `http.request`). HTTP responses
      # use the HTTP response language (see `http.response`).
      #
      # |@example
      # Sending a request to a local server:
      #
      # ```wwml
      # (cell @request (get "/"))
      # (cell @response)
      # (client (@request -> (http local 5000) -> @response))
      # ```
      #
      # Sending a request to a remote server:
      #
      # ```wwml
      # (cell @request (get "/"))
      # (cell @response)
      # (client (@request -> (http "example.org") -> @response))
      # ```
      #
      # Notice how you can omit the port `8080`.
      #
      # You can use `rack.queue` instead of `cell` to queue requests, responses,
      # or both:
      #
      # ```wwml
      # (queue (@request @requests)
      #   ((get "/")
      #    (get "/posts")
      #    (get "/employees")))
      #
      # (queue (@response @responses) ())
      #
      # ;; Take a request from the front of the requests queue -- `@request`.
      # ;; Put the resulting response at the back of the responses queue -- `@responses`.
      # (client (@request -> (http "example.org" 8080) -> @responses))
      # ```

      matchpiT %{(http hostQ_ port←(%number u16) ⍊ key: (%optional master keyQ_))} do
        return unless host = host?(hostQ)
        return unless key = key?(hg, addr, keyQ)

        Harmony::HttpClientDefn.new(host, port, key, security: nil)
      end

      matchpi %{(http hostQ_ ⍊ key: (%optional master keyQ_))} do
        return unless host = host?(hostQ)
        return unless key = key?(hg, addr, keyQ)

        Harmony::HttpClientDefn.new(host, 8080, key, security: nil)
      end

      # |@ rack.client.transport
      #
      # |@pattern
      # (https host_ port_ ⍊ key_⋮ master verify⋮ true)
      # (https host_ ⍊ key_⋮ master verify⋮ true)
      #
      # |@key host rack.[network].host
      #
      # |@key port
      # If omitted, uses the default HTTPS port 443.
      #
      # |@key key rack.[network].key
      #
      # |@key verify
      # Whether to verify the certificate.
      # - `true` corresponds to `SSL_VERIFY_PEER`.
      # - `false` corresponds to `SSL_VERIFY_NONE`.
      #
      # |@block
      # Connects to an HTTP server at *host*:*port*. Establishes a secure connection
      # using TLS.
      #
      # HTTP requests use the HTTP request language (see `http.request`). HTTP responses
      # use the HTTP response language (see `http.response`).
      #
      # |@example
      # Sending a request to a remote server:
      #
      # ```wwml
      # (cell @request (get "/"))
      # (cell @response)
      # (client (@request -> (https "example.org") -> @response))
      # ```

      matchpiT %{(https hostQ_ port←(%number u16) ⍊ key: (%optional master keyQ_) verify⋮ true)} do
        return unless host = host?(hostQ)
        return unless key = key?(hg, addr, keyQ)

        tls_config = Harmony::TlsClientConfig.new(verify.true?)
        Harmony::HttpClientDefn.new(host, port, key, security: tls_config)
      end

      matchpi %{(https hostQ_ ⍊ key: (%optional master keyQ_) verify⋮ true)} do
        return unless host = host?(hostQ)
        return unless key = key?(hg, addr, keyQ)

        tls_config = Harmony::TlsClientConfig.new(verify.true?)
        Harmony::HttpClientDefn.new(host, 443, key, security: tls_config)
      end

      otherwise { }
    end
  end

  alias Machine = SocketServer | SocketClient | HttpServer | HttpClient

  defcase SocketServer,
    node : D7::Node,
    defn : Harmony::SocketServerDefn,
    pool : D7::AbsEdge,
    in_edge : Term,
    out_edge : Term,
    template : Term::Dict,
    format : Format::Any,
    format_policy : Format::Policy

  defcase SocketClient,
    node : D7::Node,
    defn : Harmony::SocketClientDefn,
    outgoing : D7::AbsEdge,
    ingoing : D7::AbsEdge,
    format : Format::Any,
    format_policy : Format::Policy

  defcase HttpServer,
    node : D7::Node,
    defn : Harmony::HttpServerDefn,
    pool : D7::AbsEdge,
    request_edge : Term,
    response_edge : Term,
    template : Term::Dict,
    format : Format::Any,
    format_policy : Format::Policy

  defcase WebSocketServer,
    node : D7::Node,
    defn : Harmony::HttpServerDefn,
    link : Harmony::Link,
    pool : D7::AbsEdge,
    in_edge : Term,
    out_edge : Term,
    template : Term::Dict,
    format : Format::Any,
    format_policy : Format::Policy

  defcase HttpClient,
    node : D7::Node,
    defn : Harmony::HttpClientDefn,
    outgoing : D7::AbsEdge,
    ingoing : D7::AbsEdge,
    format : Format::Any,
    format_policy : Format::Policy

  private def status_and_incarnation(world : Harmony::ReadonlyWorld, defn : Harmony::ServerDefn) : {Term, Harmony::ServerId?}
    world.each(Harmony::RunningServer, defn: defn) do |fact|
      if fact.info.empty?
        # E.g. `up`.
        return Term.of(:up), fact.server_id
      else
        # E.g. `(up port: 5000)`.
        return Term.of(fact.info.with(0, :up)), fact.server_id
      end
    end

    world.each(Harmony::PendingServer, defn: defn) do |fact|
      return Term.of(:pending, fact.detail), nil
    end

    world.each(Harmony::BrokenServer, defn: defn) do |fact|
      return Term.of(:dn, fact.detail), nil
    end

    {Term.of(:pending), nil}
  end

  private def status_and_incarnation(world : Harmony::ReadonlyWorld, defn : Harmony::ClientDefn) : {Term, Harmony::ClientId?}
    world.each(Harmony::RunningClient, defn: defn) do |fact|
      return Term.of(:up), fact.client_id
    end

    world.each(Harmony::PendingClient, defn: defn) do |fact|
      return Term.of(:pending, fact.detail), nil
    end

    world.each(Harmony::BrokenClient, defn: defn) do |fact|
      return Term.of(:dn, fact.detail), nil
    end

    {Term.of(:pending), nil}
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, server : SocketServer | HttpServer) : D7::Patch?
    # If there's no associated pool, then the server's "machine" is structurally
    # incomplete, so it cannot work -- nor does it *exist*, really.
    unless pool = Rack.pool?(hg, server.pool)
      # (server (@_ _ ⏏) _*)
      return D7.patch(server.node, {1, 2, {:dn, "missing pool"}})
    end

    ctx.goals.add(Harmony::Server.new(server.defn))

    status, incarnation = status_and_incarnation(ctx.world, server.defn)

    # When there's no incarnation, this means the server has disappeared for some reason. Along
    # with it, assume all its clients have disappeared. Clear the pool and update the status
    # to inform the circuit.
    if incarnation.nil?
      return D7.patches(
        # (server (@_ _ ⏏) _*)
        D7.patch(server.node, {1, 2, status}),
        # (pool @_ ⏏)
        D7.patch(pool.node, {2, Term[]}),
      )
    end

    step(ctx, hg, server, pool, status, incarnation)
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, server : WebSocketServer) : D7::Patch?
    # If there's no associated pool, then the server's "machine" is structurally
    # incomplete, so it cannot work -- nor does it *exist*, really.
    unless pool = Rack.pool?(hg, server.pool)
      # (server (@_ _ ⏏) _*)
      return D7.patch(server.node, {1, 2, {:dn, "missing pool"}})
    end

    ctx.goals.add(Harmony::Server.new(server.defn))

    status, incarnation = status_and_incarnation(ctx.world, server.defn)

    # Ditto as above: the server disappeared, assume its clients disappeared too.
    # Clear the pool and update the status to inform the circuit.
    if incarnation.nil?
      return D7.patches(
        # (server (@_ _ ⏏) _*)
        D7.patch(server.node, {1, 2, status}),
        # (pool @_ ⏏)
        D7.patch(pool.node, {2, Term[]}),
      )
    end

    handler = ctx.goals.single?(Harmony::WebSocketHandler, server_id: incarnation)
    handler ||= Harmony::WebSocketHandler.new(incarnation, server.link)
    ctx.goals.add(handler)

    unless handler.link == server.link
      return D7.patch(server.node, {1, 2, {:dn, "conflicting `link:`s for the same host and port"}})
    end

    step(ctx, hg, server, pool, status, incarnation)
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, client : SocketClient | HttpClient) : D7::Patch?
    ctx.goals.add(Harmony::Client.new(client.defn))

    status, incarnation = status_and_incarnation(ctx.world, client.defn)
    if incarnation.nil?
      return D7.patch(client.node, {2, status})
    end

    step(ctx, hg, client, status, incarnation)
  end

  defrecord DeviceIn, key : UInt32
  defrecord DeviceOut, key : UInt32, msg : Term?, smart: true
  defrecord DeviceCell, key : UInt32, value : Term

  # :nodoc:
  EDGE_ID = Term.of(:edge, :id)

  # Returns the peer id associated with *device*.
  private def extract?(device : D7::CircuitNode, cls : Harmony::PeerId.class | Harmony::HttpRequestId.class)
    # NOTE: the id cell is the first cell because we generate it this way. So
    # O(N) here is effectively O(1).
    return unless cell = extract?(device, DeviceCell, EDGE_ID)
    return unless reprQ = cell.value.as_s?
    return unless repr = UUID.parse?(reprQ.to(String))

    cls.new(repr)
  end

  private def extract?(device : D7::CircuitNode, cls : DeviceIn.class, edge : Term) : DeviceIn?
    return unless cell = extract?(device, DeviceCell, edge)
    return unless _ = cell.value.as_itemsonly_d?

    DeviceIn.new(cell.key)
  end

  private def extract?(device : D7::CircuitNode, cls : DeviceOut.class, edge : Term) : DeviceOut?
    return unless cell = extract?(device, DeviceCell, edge)
    return unless msgs = cell.value.as_itemsonly_d?

    DeviceOut.new(cell.key, msgs.items.first?)
  end

  private def extract?(device : D7::CircuitNode, cls : DeviceCell.class, edge needle : Term) : DeviceCell?
    candidates = Pf::Kit.stack_array(DeviceCell, 1)

    device.children.each_with_index(offset: device.feature.range.begin) do |child, index|
      next unless child.is_a?(D7::GndLeaf)

      node = child.feature.node
      next unless node = node.as_d?
      next unless node.itemsize == 3

      head, edge, value = node
      next unless head == Term.of(:cell)
      next unless edge == needle

      candidates << DeviceCell.new(index.to_u32, value)
    end

    candidates.single?
  end

  private def each_device(devices : D7::CircuitNode, & : D7::CircuitNode, UInt32 ->) : Nil
    devices.children.zip(0u32...devices.children.size) do |device, device_key|
      next unless device.is_a?(D7::CircuitNode)

      node : Term::Dict = device.feature.node
      next unless node.itemsize >= 1
      next unless node.items.first == Term.of(:device)

      yield device, device_key
    end
  end

  alias DeviceChange = DeviceAdded | DeviceRemoved | DeviceModified
  alias DeviceModified = DeviceReceivedMessage | DeviceReceivedBatch | DeviceSentMessage

  defrecord DeviceAdded, device : Term, brief: true
  defrecord DeviceRemoved, device_key : UInt32, brief: true
  defrecord DeviceReceivedMessage, device_key : UInt32, mailbox_key : UInt32, msg : Term, brief: true
  defrecord DeviceReceivedBatch, device_key : UInt32, mailbox_key : UInt32, batch : Slice(Term), brief: true
  defrecord DeviceSentMessage, device_key : UInt32, mailbox_key : UInt32, brief: true

  struct DeviceChangeList
    def initialize
      @added = [] of DeviceAdded
      @removed = Set(DeviceRemoved).new
      @modified = [] of DeviceModified
    end

    def includes?(cls : DeviceRemoved.class) : Bool
      @removed.present?
    end

    def includes?(change : DeviceRemoved) : Bool
      @removed.includes?(change)
    end

    def each_added(& : DeviceAdded ->) : Nil
      @added.each { |change| yield change }
    end

    def each_modified(& : DeviceModified ->) : Nil
      @modified.each { |change| yield change }
    end

    def <<(change : DeviceAdded) : Nil
      @added << change
    end

    def <<(change : DeviceRemoved) : Nil
      @removed << change
    end

    def <<(change : DeviceModified) : Nil
      @modified << change
    end
  end

  private def apply(pool : Term::Dict, changes : DeviceChangeList) : Term::Dict
    pool = pool.transaction do |commit|
      changes.each_modified do |change|
        # We read the updated device, because different changes can target
        # the same device.
        device0 = commit[change.device_key]

        mailbox0 = device0[change.mailbox_key, 2]
        case change
        in DeviceReceivedMessage
          mailbox1 = mailbox0.append(change.msg)
        in DeviceReceivedBatch
          mailbox1 = mailbox0.transaction(&.concat(change.batch))
        in DeviceSentMessage
          mailbox1 = mailbox0.replace(0...1, Term.rep)
        end

        device1 = Term.morph(device0, {change.mailbox_key, 2, mailbox1})

        commit.with(change.device_key, device1)
      end

      changes.each_added do |change|
        commit << change.device
      end
    end

    unless DeviceRemoved.in?(changes)
      return pool
    end

    pool.pairspart.transaction do |commit|
      pool.items.each_with_index do |item, index|
        change = DeviceRemoved.new(index.to_u32)
        next if change.in?(changes)

        commit << item
      end
    end
  end

  class FormatAborted < Exception
    @callstack = CallStack.empty
  end

  defrecord HttpRequest, term : Term
  defrecord HttpResponse, term : Term

  private def encode?(format : Format::Any, policy : Format::Policy, msg : Term) : Term::Blob?
    payload = Format.encode?(format, msg)

    case policy
    in .discard?
      payload
    in .abort?, .wrap? # ?!
      payload || raise FormatAborted.new
    end
  end

  private def decode?(format : Format::Any, policy : Format::Policy, payload : Term::Blob) : Term?
    term = Format.decode?(format, payload)

    case policy
    in .discard?
      term
    in .abort?
      term || raise FormatAborted.new
    in .wrap?
      if term
        Term.of(:ok, term)
      else
        Term.of(:err, "format decode error")
      end
    end
  end

  private def encode?(format : Format::Any, policy : Format::Policy, request : HttpRequest) : Term?
    Term.case(request.term) do
      matchpi %{[_symbol _ body_]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(request.term, {2, blob})
      end

      # Keep all other requests as-is.
      otherwise do
        request.term
      end
    end
  end

  private def decode?(format : Format::Any, policy : Format::Policy, payload : HttpRequest) : Term?
    Term.case(payload.term) do
      matchpiT %{[_symbol _ bodyQ_blob]} do
        return unless body = Format.decode?(format, bodyQ)

        Term.morph(payload.term, {2, body})
      end

      # Keep all other requests as-is.
      otherwise do
        payload.term
      end
    end
  end

  private def encode?(format : Format::Any, policy : Format::Policy, response : HttpResponse) : Term?
    Term.case(response.term) do
      matchpi %{[_ [attachment body_]]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(response.term, {1, 1, blob})
      end

      matchpi %{[_ [file _string body_]]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(response.term, {1, 2, blob})
      end

      matchpi %{[_ body_]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(response.term, {1, blob})
      end

      # Keep all other requests as-is.
      otherwise do
        response.term
      end
    end
  end

  private def decode?(format : Format::Any, policy : Format::Policy, payload : HttpResponse) : Term?
    Term.case(payload.term) do
      matchpiT %{[_ [attachment bodyQ_blob]]} do
        return unless body = Format.decode?(format, bodyQ)

        Term.morph(payload.term, {1, 1, body})
      end

      matchpiT %{[_ [file _string bodyQ_blob]]} do
        return unless body = Format.decode?(format, bodyQ)

        Term.morph(payload.term, {1, 2, body})
      end

      matchpiT %{[_ bodyQ_blob]} do
        return unless body = Format.decode?(format, bodyQ)

        Term.morph(payload.term, {1, body})
      end

      # Keep all other responses as-is.
      otherwise do
        payload.term
      end
    end
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, server : SocketServer | WebSocketServer, pool : Pool, status : Term, incarnation : Harmony::ServerId) : D7::Patch?
    _, device_tree = D7.follow(hg.@tree, pool.node.addr)
    return unless device_tree.is_a?(D7::CircuitNode)

    changes = DeviceChangeList.new

    each_device(device_tree) do |device, device_key|
      next unless peer_id = extract?(device, Harmony::PeerId)

      # Detect device disconnects.
      unless ctx.world.any?(Harmony::RunningPeer, peer_id: peer_id)
        changes << DeviceRemoved.new(device_key)
        next
      end

      alive = false

      # Process ingoing messages.
      pass do
        next unless inbox = extract?(device, DeviceIn, server.in_edge)

        alive = true

        # Indicate to the other side that *peer* has spare space for messages.
        ctx.goals.add(Harmony::MessageSlot.new(peer_id))

        rows = Pf::Kit.stack_array({Harmony::MsgId, Term}, 1)

        ctx.world.each(Harmony::IngoingMessage, endpoint_id: peer_id) do |fact|
          confirmation = Harmony::IngoingReceiveConfirmation.new(fact.endpoint_id, fact.msgid)

          # Initiate confirmation. If confirmation is a fact, this means it's complete.
          unless ctx.world.includes?(confirmation)
            ctx.goals << Harmony::IngoingMessageKeepalive.new(fact.endpoint_id, fact.msgid)
            ctx.goals << confirmation
            next
          end

          next unless msg = decode?(server.format, server.format_policy, fact.payload)

          rows << {fact.msgid, msg}
        end

        # Most often there are no messages.
        next if rows.empty?

        # Sometimes there's just one message.
        if row = rows.single?
          _, msg = row
          changes << DeviceReceivedMessage.new(device_key, inbox.key, msg)
          next
        end

        # Very rarely there are several messages.
        rows.unstable_sort_by! { |msgid, _| msgid.repr }
        batch = rows.to_readonly_slice { |(_, msg)| msg }
        changes << DeviceReceivedBatch.new(device_key, inbox.key, batch)
      end

      # Process outgoing messages.
      pass do
        next unless outbox = extract?(device, DeviceOut, server.out_edge)

        alive = true
        next unless msg = outbox.msg?
        next unless payload = encode?(server.format, server.format_policy, msg)

        if ctx.world.includes?(Harmony::RemoteReceiveConfirmation.new(peer_id, payload))
          changes << DeviceSentMessage.new(device_key, outbox.key)
          next
        end

        ctx.goals << Harmony::OutgoingMessage.new(peer_id, payload)
      end

      next unless alive

      ctx.goals << Harmony::PeerKeepalive.new(peer_id)
    rescue FormatAborted
    end

    ctx.world.each(Harmony::RunningPeer, server_id: incarnation) do |fact|
      next if fact.peer_id.in?(ctx.acknowledged)

      # Keep the peer and all messages designated for it alive. We will process
      # the messages on the next tick.
      ctx.goals.add(Harmony::PeerKeepalive.new(fact.peer_id))
      ctx.world.each(Harmony::IngoingMessage, endpoint_id: fact.peer_id) do |msg_fact|
        ctx.goals.add(Harmony::IngoingMessageKeepalive.new(fact.peer_id, msg_fact.msgid))
      end

      instance = Term::Dict.build do |commit|
        commit << :device
        commit << {:cell, {:edge, :id}, fact.peer_id.repr}
        commit << {:cell, server.in_edge, Term[]}
        commit << {:cell, server.out_edge, Term[]}
        commit.concat(server.template.items)
      end

      changes << DeviceAdded.new(Term.of(instance))
    end

    D7.patches(
      # (server (@_ _ ⏏) _*)
      D7.patch(server.node, {1, 2, status}),
      # (pool @_ ⏏)
      D7.patch(pool.node, {2, apply(pool.contents, changes)}),
    )
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, server : HttpServer, pool : Pool, status : Term, incarnation : Harmony::ServerId) : D7::Patch?
    _, device_tree = D7.follow(hg.@tree, pool.node.addr)
    return unless device_tree.is_a?(D7::CircuitNode)

    changes = DeviceChangeList.new

    each_device(device_tree) do |device, device_key|
      next unless request_id = extract?(device, Harmony::HttpRequestId)

      # If this request id doesn't have a corresponding request, this means the request was
      # handled already and we can remove this device.
      unless ctx.world.any?(Harmony::HttpServerRequest, server_id: incarnation, request_id: request_id)
        changes << DeviceRemoved.new(device_key)
        next
      end

      # Wait until a response is available.
      unless response = extract?(device, DeviceCell, server.response_edge)
        ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(request_id))
        next
      end

      # Discard response if its encoding is invalid. Since we no longer keep the corresponding
      # request alive, and we're edge-triggered, the request will disappear eventually along
      # with the device.
      begin
        next unless payload = encode?(server.format, server.format_policy, HttpResponse.new(response.value))
      rescue FormatAborted
        next
      end

      # If the response does not specify the content type explictily, and Format
      # suggests one, use the suggested content type.
      pass do
        next unless suggested_content_type = Format.content_type?(server.format)
        next if payload.includes?(:"content-type")

        payload = Term.morph(payload, {:"content-type", suggested_content_type})
      end

      ctx.goals.add(Harmony::HttpServerResponse.new(incarnation, request_id, payload))
    end

    # Add new connections.
    ctx.world.each(Harmony::HttpServerRequest, server_id: incarnation) do |fact|
      # Do not reintroduce requests we've already handled.
      next if fact.request_id.in?(ctx.acknowledged)

      begin
        next unless request = decode?(server.format, server.format_policy, HttpRequest.new(fact.request))
      rescue FormatAborted
        # NOTE: Since HTTP is stateless and works one request at a time -- at least
        # conceptually -- discard (drop request) and abort (drop connection) are
        # the same thing.
        next
      end

      ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(fact.request_id))

      instance = Term::Dict.build do |commit|
        commit << :device
        commit << {:cell, {:edge, :id}, fact.request_id.repr}
        commit << {:cell, server.request_edge, request}
        commit << {:cell, server.response_edge}
        commit.concat(server.template.items)
      end

      changes << DeviceAdded.new(Term.of(instance))
    end

    D7.patches(
      # (server (@_ _ ⏏) _*)
      D7.patch(server.node, {1, 2, status}),
      # (pool @_ ⏏)
      D7.patch(pool.node, {2, apply(pool.contents, changes)}),
    )
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, client : SocketClient, status : Term, incarnation : Harmony::ClientId) : D7::Patch?
    status_patch = D7.patch(client.node, {2, status})

    # Handle the source.
    source_patch = nil
    pass do
      next unless source = Rack.cell?(hg, client.outgoing)
      next unless message = source.value?

      # If the message fails to encode (e.g. due to limits) we "clog" the message
      # cell so that failure is evident.
      #
      # TODO: We should also provide a descriptive error message explaining why
      # the thing doesn't encode!
      begin
        next unless message = encode?(client.format, client.format_policy, message)
      rescue e : FormatAborted
        status_patch = D7.patch(client.node, {2, {:dn, e.message || "format encode error"}})
        next
      end

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
      next unless target = Rack.cell?(hg, client.ingoing)

      ingoing = nil

      # Keep all pending messages alive, but save only the one with min(msgid).
      ctx.world.each(Harmony::IngoingMessage, endpoint_id: incarnation) do |fact|
        ctx.goals << Harmony::IngoingMessageKeepalive.new(fact.endpoint_id, fact.msgid)

        if ingoing.nil? || fact.msgid.repr < ingoing.msgid.repr
          ingoing = fact
        end
      end

      next unless target.empty?

      if ingoing.nil?
        ctx.goals.add(Harmony::MessageSlot.new(incarnation))
        next
      end

      confirmation = Harmony::IngoingReceiveConfirmation.new(ingoing.endpoint_id, ingoing.msgid)

      # Initiate confirmation. If confirmation is a fact, this means it's complete.
      unless ctx.world.includes?(confirmation)
        ctx.goals << confirmation
        next
      end

      ctx.goals.delete(Harmony::IngoingMessageKeepalive.new(ingoing.endpoint_id, ingoing.msgid))

      begin
        next unless msg = decode?(client.format, client.format_policy, ingoing.payload)

        target_patch = D7.patch(target.node, {2, msg})
      rescue e : FormatAborted
        status_patch = D7.patch(client.node, {2, {:dn, e.message || "format decode error"}})
      end
    end

    D7.patches(
      status_patch,
      source_patch || D7::Patch.new,
      target_patch || D7::Patch.new,
    )
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, client : HttpClient, status : Term, incarnation : Harmony::ClientId) : D7::Patch?
    patch = D7.patch(client.node, {2, status})

    # If there's no current request, then there's no response to wait or be waiting for.
    pass do
      next unless source = Rack.cell?(hg, client.outgoing)
      next unless request = source.value?

      next unless target = Rack.cell?(hg, client.ingoing)
      next unless target.empty?

      begin
        next unless request = encode?(client.format, client.format_policy, HttpRequest.new(request))
      rescue e : FormatAborted
        patch = D7.patch(client.node, {2, {:dn, e.message || "format encode error"}})
        next
      end

      unless fact = ctx.world.single?(Harmony::HttpClientResponse, client_id: incarnation, request: request)
        ctx.goals.add(Harmony::HttpClientRequest.new(incarnation, request))
        next
      end

      payload = fact.result.response

      case payload
      in Harmony::HttpResponseError
        patch = D7.patch(client.node, {2, {:dn, payload.detail}})
        next
      in Term
      end

      begin
        next unless response = decode?(client.format, client.format_policy, HttpResponse.new(payload))
      rescue e : FormatAborted
        patch = D7.patch(client.node, {2, {:dn, e.message || "format decode error"}})
        next
      end

      patch = D7.patches(
        D7.patch(client.node, {2, status}),
        D7.patch(source.node, {2, nil}),
        D7.patch(target.node, {2, response}),
      )
    end

    patch
  end
end
