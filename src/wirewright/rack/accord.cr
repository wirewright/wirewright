# Rack integration with `Harmony`.
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

  # NOTE: *world* is a logical snapshot of the current world which must be used in
  # a read-only way by step() overloads. The same is true for *acknowledged*. The set
  # of *goals*, on the other hand, starts empty and is meant to be populated by them.
  defrecord StepContext,
    world : Harmony::FactSet,
    goals : Harmony::GoalSet,
    acknowledged : Set(Harmony::PeerId | Harmony::HttpRequestId)

  def step(state : State, & : Propose -> T) : T forall T
    changelog = state.harmony.observe

    goals = Harmony::GoalSet.new

    result = state.schemas.epoch do
      propose = Propose.new do |hg, proposals|
        ctx = StepContext.new(state.harmony.world, goals, state.acknowledged)
        propose(state, ctx, hg, proposals)
      end
      yield propose
    end

    state.harmony.submit(goals)
    state.harmony.reconcile

    # NOTE: Importantly, we update the acknowledged set *after* proposal because it
    # should actually represent the state of affairs *prior* to proposal
    # *during* proposal.
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
        matchpi %{[server (@_ _ dn) _*]} { }
        matchpi %{[server (@_ _ (dn _string)) _*]} { }

        matchpi(<<-WWML) do
        [server
          (@pool_ transportQ←(ws _* ⍊ link: (%optional direct linkQ_)) _?
            ⍊ in: (%optional @in @input_)
              out: (%optional @out @output_)
              format: (%optional binary formatQ_)
              format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless defn = http_server_transport?(transportQ)
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
              format: (%optional binary formatQ_)
              format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless defn = http_server_transport?(transportQ)

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
              format: (%optional binary formatQ_)
              format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless defn = socket_server_transport?(transportQ)
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
        # Allow the circuit to use an errorless `dn` to disable the socket.
        matchpi %{[client [@_ -> _ -> @_] dn]} { }
        matchpi %{[client [@_ -> _ -> @_] (dn _string)]} { }

        matchpi(<<-WWML) do
        [client
          (@outgoing_ -> transportQ_ -> @ingoing_
            ⍊ format: (%optional binary formatQ_)
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
    # Lets you describe a server or client host.
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

  private def socket_server_transport?(term : Term) : Harmony::SocketServerDefn?
    Term.case(term) do
      # |@ rack.server.transport
      #
      # |@pattern
      # (tcp host_ port←(%number u16) ⍊ link_⋮ direct)
      #
      # |@key host rack.[network].host
      # |@key link rack.[network].link
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at *host*:*port*.
      matchpiT %{(tcp hostQ_ port←(%number u16) ⍊ link_⋮ direct)} do
        return unless host = host?(hostQ)

        Harmony::TcpServerDefn.new(host, port, link?(link) || return)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (unix path_string ⍊ link_⋮ direct)
      #
      # |@key link rack.[network].link
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over a Unix socket at *path*.
      matchpiT %{(unix path_string ⍊ link_⋮ direct)}, path: NormalPath do
        Harmony::UnixServerDefn.new(path, link?(link) || return)
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
      # |@ rack.client.key
      #
      # |@summary
      # The `key` pair whic all client transports accept.
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
      # If there is no cell at the edge, or if the cell is empty, the entire transport
      # is invalidated.

      # |@ rack.client.transport.renew
      #
      # |@summary
      # The `renew` pair which all client transports accept.
      #
      # |@block
      # If `renew: true`, uses `(pending _string)` instead of `(dn _string)` when
      # the connection breaks or closes. `renew: false` by default.
      #
      # Rack clients attempt to *connect* repeatedly by default, with backoff; but they do
      # not do automatic *re*connects after (or in case) an *established* connection breaks
      # for some reason. You can explicitly enable reconnects by setting `renew: true`.
      #
      # We do not enable reconnects by default because doing so could create state sync bugs and
      # the like -- when the client reconnects faster than you can detect the connection was dropped.
      # That would be your problem, of course, not `client`'s -- it gives you exactly one frame where
      # the `dn` is there, so you can detect it and pause everything quickly. But if you do not do
      # that, we would rather give you a reliable way to detect closure at your pace and repair
      # things, than consume `dn` silently and leave you confused. A new connection is a new connection,
      # after all, and we'd like the boundary between to be clearly recongizable.
      #
      # If you protocol or the way you use `client` is stateless, or there's no complex
      # sync logic, you may actually want reconnects. That's why `renew: true` exists,
      # to relieve you of the need to manually reset the client (or via rules).

      # |@ rack.client.transport
      #
      # |@pattern
      # (ws host_ port←(%number u16) ⍊ key_⋮ master ⋮link path⋮ "" renew⋮ false)
      #
      # |@key host rack.[network].host
      # |@key key rack.client.key
      # |@key renew rack.client.transport.renew
      # |@key link rack.[network].link
      #
      # |@block
      # A plain WebSocket client at *host*:*port* on *path*.
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
      # (wss host_ port←(%number u16) ⍊ key_⋮ master path⋮ "" ⋮link renew⋮ false)
      #
      # |@key host rack.[network].host
      # |@key key rack.client.key
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
      # (tcp host_ port←(%number u16) ⍊ key_⋮ master ⋮link renew⋮ false)
      #
      # |@key host rack.[network].host
      # |@key key rack.client.key
      # |@key link rack.[network].link
      # |@key renew rack.client.transport.renew
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over TCP at *host*:*port*.
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
      # |@key key rack.client.key
      # |@key link rack.[network].link
      # |@key renew rack.client.transport.renew
      #
      # |@block
      # [NetStrings](https://cr.yp.to/proto/netstrings.txt) over a Unix socket at *path*.
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

  private def http_server_transport?(term : Term) : Harmony::HttpServerDefn?
    Term.case(term) do
      # |@ rack.server.transport
      #
      # |@pattern
      # (http host_ port←(%number u16))
      #
      # |@key host rack.[network].host
      #
      # |@block
      # An HTTP server at *host*:*port*.
      #
      # HTTP requests are represented using the HTTP request language.
      # See `http.request`.
      #
      # HTTP responses are represented using the HTTP response language.
      # See `http.response`

      # |@ rack.server.transport
      #
      # |@pattern
      # (ws host_ port←(%number u16) ⍊ link_⋮ direct)
      #
      # |@key host rack.[network].host
      # |@key link rack.[network].link
      #
      # |@block
      # A plain WebSocket server at *host*:*port*. If there is an existing HTTP server
      # at *port* (within the same circuit!), extends it with WebSocket support.

      matchpiT %{[http hostQ_ port←(%number u16)]}, %{[ws hostQ_ port←(%number u16)]} do
        return unless host = host?(hostQ)

        Harmony::HttpServerDefn.new(host, port, security: nil)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (https host_ port←(%number u16) ⍊ cert_string key_string)
      #
      # |@key host rack.[network].host
      #
      # |@key cert
      # Path to the file containing the public certificate chain.
      #
      # |@key key
      # Path to the private key file.
      #
      # |@block
      # An HTTPS server at *host*:*port*.
      #
      # HTTP requests are represented using the HTTP request language.
      # See `http.request`.
      #
      # HTTP responses are represented using the HTTP response language.
      # See `http.response`

      # |@ rack.server.transport
      #
      # |@pattern
      # (wss host_ port←(%number u16) ⍊ link_⋮ direct cert_string key_string)
      #
      # |@key host rack.[network].host
      # |@key link rack.[network].link
      #
      # |@key cert
      # Path to the file containing the public certificate chain.
      #
      # |@key key
      # Path to the private key file.
      #
      # |@block
      # A plain WebSocket server at *host*:*port* (using TLS). If there is
      # an existing HTTPS server at *port* (within the same circuit!), extends
      # it with WebSocket support.

      matchpiT(
        %{(https hostQ_ port←(%number u16) ⍊ cert_string key_string)},
        %{(wss hostQ_ port←(%number u16) ⍊ cert_string key_string)},
        cert: NormalPath, key: NormalPath,
      ) do
        return unless host = host?(hostQ)

        tls_config = Harmony::TlsServerConfig.new(cert, key)
        Harmony::HttpServerDefn.new(host, port, security: tls_config)
      end

      otherwise { }
    end
  end

  private def http_client_transport?(hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Harmony::HttpClientDefn?
    Term.case(term) do
      # |@ rack.client.transport
      #
      # |@pattern
      # (http host_ port←(%number u16) ⍊ key_⋮ master)
      # (http host_ ⍊ key_⋮ master)
      #
      # |@key host rack.[network].host
      #
      # |@key port
      # The port number. If omitted, uses the default HTTP port 8080.
      #
      # |@key key rack.client.key
      #
      # |@block
      # Connects to an HTTP server at *host*:*port*.
      #
      # HTTP requests are represented using the HTTP request language.
      # See `http.request`.
      #
      # HTTP responses are represented using the HTTP response language.
      # See `http.response`

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
      # (https host_ port←(%number u16) ⍊ key_⋮ master verify⋮ true)
      # (https host_ ⍊ key_⋮ master verify⋮ true)
      #
      # |@key host rack.[network].host
      #
      # |@key port
      # The port number. If omitted, uses the default HTTPS port 443.
      #
      # |@key key rack.client.key
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
      # HTTP requests are represented using the HTTP request language.
      # See `http.request`.
      #
      # HTTP responses are represented using the HTTP response language.
      # See `http.response`

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

  def step(ctx : StepContext, hg : D7::Hypergraph, server : SocketServer | HttpServer) : D7::Patch?
    # If there's no associated pool, then the server's "machine" is structurally
    # incomplete, so it cannot work -- nor does it *exist*, really.
    unless pool = Rack.pool?(hg, server.pool)
      # (server (@_ _ ⏏) _*)
      return D7.patch(server.node, {1, 2, {:dn, "missing pool"}})
    end

    ctx.goals.add(Harmony::Server.new(server.defn))

    status = Term.of(:pending)
    incarnation = nil

    pass do
      ctx.world.each(Harmony::RunningServer, defn: server.defn) do |fact|
        status = Term.of(:up)
        incarnation = fact.server_id
        break
      end

      next if incarnation

      ctx.world.each(Harmony::PendingServer, defn: server.defn) do |fact|
        status = Term.of(:pending, fact.detail)
        break
      end

      next if incarnation

      ctx.world.each(Harmony::BrokenServer, defn: server.defn) do |fact|
        status = Term.of(:dn, fact.detail)
        break
      end
    end

    step(ctx, hg, server, pool, status, incarnation)
  end

  def step(ctx : StepContext, hg : D7::Hypergraph, server : WebSocketServer) : D7::Patch?
    # If there's no associated pool, then the server's "machine" is structurally
    # incomplete, so it cannot work -- nor does it *exist*, really.
    unless pool = Rack.pool?(hg, server.pool)
      # (server (@_ _ ⏏) _*)
      return D7.patch(server.node, {1, 2, {:dn, "missing pool"}})
    end

    ctx.goals.add(Harmony::Server.new(server.defn))

    status = Term.of(:pending)
    incarnation = nil

    pass do
      ctx.world.each(Harmony::RunningServer, defn: server.defn) do |fact|
        status = Term.of(:up)
        incarnation = fact.server_id
        break
      end

      next if incarnation

      ctx.world.each(Harmony::PendingServer, defn: server.defn) do |fact|
        status = Term.of(:pending, fact.detail)
        break
      end

      next if incarnation

      ctx.world.each(Harmony::BrokenServer, defn: server.defn) do |fact|
        status = Term.of(:dn, fact.detail)
        break
      end
    end

    unless incarnation
      return D7.patch(server.node, {1, 2, status})
    end

    if handler = ctx.goals.single?(Harmony::WebSocketHandler, server_id: incarnation)
      unless handler.link == server.link
        return D7.patch(server.node, {1, 2, {:dn, "link conflict"}})
      end
    else
      ctx.goals.add(Harmony::WebSocketHandler.new(incarnation, server.link))
    end

    step(ctx, hg, server, pool, status, incarnation)
  end

  def step(ctx : StepContext, hg : D7::Hypergraph, server : SocketServer | WebSocketServer, pool : Pool, status : Term, incarnation : Harmony::ServerId) : D7::Patch?
    _, device_tree = D7.follow(hg.@tree, pool.node.addr)
    return unless device_tree.is_a?(D7::CircuitNode)

    status_patch = D7.patch(server.node, {1, 2, status})

    devices = pool.contents

    # Iterate in reverse because we're going to replace() things
    (0...device_tree.children.size).reverse_each do |device_index|
      device = device_tree.children[device_index]
      next unless device.is_a?(D7::CircuitNode)

      # NOTE: *device_index* is a valid key for *devices* because *device_tree* (and its children)
      # is the parsed version of *devices*.

      Term.matchpi?(device.feature.node, %{[device _*]}) do
        # Determine the id of this device. Note that the id cell is the first cell
        # predominantly (basically always) -- because we generate it this way and
        # the user has very little ways to change this, so O(N) here is effectively O(1).
        peer_id = device.children.leftmost? do |child|
          next unless child.is_a?(D7::GndLeaf)

          Term.matchpi?(child.feature.node, %{[cell @id reprQ_string]}) do
            next unless repr = UUID.parse?(reprQ.to(String))

            Harmony::PeerId.new(repr)
          end
        end

        next unless peer_id

        unless ctx.world.any?(Harmony::RunningPeer, peer_id: peer_id)
          devices = devices.replace(device_index...device_index + 1, Term.rep)
          next
        end

        has_inbox = false
        has_outbox = false

        # Rendezvous world with message boxes.
        device.children.each_with_index(offset: device.feature.range.begin) do |child, child_key|
          next unless child.is_a?(D7::GndLeaf)

          Term.matchpi?(child.feature.node, %{[cell @edge_ msgs←(_*)]}) do
            case edge
            when server.in_edge
              has_inbox = true

              batch = [] of {Harmony::MsgId, Term}

              ctx.world.each(Harmony::IngoingMessage, endpoint_id: peer_id) do |fact|
                confirmation = Harmony::IngoingReceiveConfirmation.new(fact.endpoint_id, fact.msgid)

                # Initiate confirmation. If confirmation is a fact, this means it's complete.
                unless ctx.world.includes?(confirmation)
                  ctx.goals << Harmony::IngoingMessageKeepalive.new(fact.endpoint_id, fact.msgid)
                  ctx.goals << confirmation
                  next
                end

                term = Format.decode?(server.format, fact.payload)

                case server.format_policy
                in .discard?
                  next if term.nil?
                in .abort?
                  if term.nil?
                    return status_patch
                  end
                in .wrap?
                  if term
                    term = Term.of(:ok, term)
                  else
                    term = Term.of(:err, "invalid message")
                  end
                end

                batch << {fact.msgid, term}
              end

              batch.unstable_sort_by! { |msgid, _| msgid.repr }

              msgs1 = msgs.transaction do |commit|
                commit.concat(batch) { |_, payload| payload }
              end

              devices = Term.morph(devices, {device_index, child_key, 2, msgs1})
            when server.out_edge
              has_outbox = true

              next unless msgQ = msgs.items.first?
              next unless msg = Format.encode?(server.format, msgQ)

              if ctx.world.includes?(Harmony::RemoteReceiveConfirmation.new(peer_id, msg))
                msgs1 = msgs.items.move(1)
                devices = Term.morph(devices, {device_index, child_key, 2, msgs1})
                next
              end

              ctx.goals << Harmony::OutgoingMessage.new(peer_id, msg)
            end
          end
        end

        next unless has_inbox || has_outbox

        # Indicate to the other side that *peer* has spare space for messages.
        if has_inbox
          ctx.goals.add(Harmony::MessageSlot.new(peer_id))
        end

        ctx.goals << Harmony::PeerKeepalive.new(peer_id)
      end
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
      devices = devices.append(instance)
    end

    D7.patches(
      # (server (@_ _ ⏏) _*)
      status_patch,
      # (pool @_ ⏏)
      D7.patch(pool.node, {2, devices}),
    )
  end

  def step(ctx : StepContext, hg : D7::Hypergraph, server : HttpServer, pool : Pool, status : Term, incarnation : Harmony::ServerId) : D7::Patch?
    _, device_tree = D7.follow(hg.@tree, pool.node.addr)
    return unless device_tree.is_a?(D7::CircuitNode)

    devices = pool.contents

    # Update old connections.
    # Remove dropped connections.
    #
    # Iterate in reverse because we're going to replace() things
    (0...device_tree.children.size).reverse_each do |device_index|
      device_node = device_tree.children[device_index]
      next unless device_node.is_a?(D7::CircuitNode)

      Term.matchpi?(device_node.feature.node, %{[device _*]}) do
        request_id = nil
        response = nil

        device_node.children.each do |child|
          next unless child.is_a?(D7::GndLeaf)

          Term.case(child.feature.node) do
            matchpi %{[cell @id reprQ_string]}, reprQ: String do
              next unless repr = UUID.parse?(reprQ)

              request_id = Harmony::HttpRequestId.new(repr)
            end

            matchpi %{[cell @edge_ responseQ_]} do
              next unless edge == server.response_edge
              next unless response = http_encode_response?(server.format, responseQ)

              # If they do not specify the content type explictily and Format suggests
              # a content type, use the suggested content type.
              pass do
                next unless suggested_content_type = Format.content_type?(server.format)
                next if responseQ.includes?(:"content-type")

                response = Term.morph(response, {:"content-type", suggested_content_type})
              end
            end

            otherwise { }
          end
        end

        next unless request_id

        # If this request id doesn't have a corresponding request, this means the request was
        # handled already and we can remove this device.
        unless ctx.world.any?(Harmony::HttpServerRequest, server_id: incarnation, request_id: request_id)
          devices = devices.replace(device_index...device_index + 1, Term.rep)
          next
        end

        if response
          ctx.goals.add(Harmony::HttpServerResponse.new(incarnation, request_id, response))
          next
        end

        ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(request_id))
      end
    end

    # Add new connections.
    ctx.world.each(Harmony::HttpServerRequest, server_id: incarnation) do |fact|
      # Do not reintroduce requests we've already handled.
      next if fact.request_id.in?(ctx.acknowledged)

      request = http_decode?(server.format, fact.request)

      case server.format_policy
      in .discard?, .abort?
        # NOTE: Since HTTP is stateless and works one request at a time -- at least
        # conceptually -- discard (drop request) and abort (drop connection) are
        # the same thing.
        next unless request
      in .wrap?
        if request
          request = Term.of(:ok, request)
        else
          request = Term.of(:err, "invalid message")
        end
      end

      ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(fact.request_id))

      instance = Term::Dict.build do |commit|
        commit << :device
        commit << {:cell, {:edge, :id}, fact.request_id.repr}
        commit << {:cell, server.request_edge, request}
        commit << {:cell, server.response_edge}
        commit.concat(server.template.items)
      end

      devices = devices.append(instance)
    end

    D7.patches(
      # (server (@_ _ ⏏) _*)
      D7.patch(server.node, {1, 2, status}),
      # (pool @_ ⏏)
      D7.patch(pool.node, {2, devices}),
    )
  end

  # When there's no incarnation, this means the server have disappeared for some reason. Along
  # with it, assume all its clients have disappeared. Clear the pool, and update the status
  # to inform the user.
  def step(ctx : StepContext, hg : D7::Hypergraph, server : SocketServer | HttpServer, pool : Pool, status : Term, incarnation : Nil) : D7::Patch?
    D7.patches(
      # (server (@_ _ ⏏) _*)
      D7.patch(server.node, {1, 2, status}),
      # (pool @_ ⏏)
      D7.patch(pool.node, {2, Term[]}),
    )
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, client : SocketClient | HttpClient) : D7::Patch?
    ctx.goals.add(Harmony::Client.new(client.defn))

    status = Term.of(:pending)
    incarnation = nil

    pass do
      ctx.world.each(Harmony::RunningClient, defn: client.defn) do |fact|
        incarnation = fact.client_id
        status = Term.of(:up)
        break
      end

      next if incarnation

      ctx.world.each(Harmony::PendingClient, defn: client.defn) do |fact|
        status = Term.of(:pending, fact.detail)
        break
      end

      next if incarnation

      ctx.world.each(Harmony::BrokenClient, defn: client.defn) do |fact|
        status = Term.of(:dn, fact.detail)
        break
      end
    end

    step(ctx, hg, client, status, incarnation)
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
      next unless target = Rack.cell?(hg, client.ingoing)

      ingoing = nil

      # Keep all pending messages alive, but save only the one with min(msgid).
      ctx.world.each(Harmony::IngoingMessage, endpoint_id: incarnation) do |fact|
        ctx.goals << Harmony::IngoingMessageKeepalive.new(fact.endpoint_id, fact.msgid)

        if ingoing.nil? || fact.msgid.repr < ingoing.msgid.repr
          ingoing = fact
        end
      end

      next unless target.value?.nil?

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

      term = Format.decode?(client.format, ingoing.payload)

      case client.format_policy
      in .discard?
        next if term.nil?
      in .abort?
        if term.nil?
          return D7.patch(client.node, {2, {:dn, "invalid message"}})
        end
      in .wrap?
        if term
          term = Term.of(:ok, term)
        else
          term = Term.of(:err, "invalid message")
        end
      end

      target_patch = D7.patch(target.node, {2, term})
    end

    D7.patches(
      status_patch,
      source_patch || D7::Patch.new,
      target_patch || D7::Patch.new,
    )
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, client : SocketClient | HttpClient, status : Term, incarnation : Nil) : D7::Patch?
    D7.patch(client.node, {2, status})
  end

  private def http_encode_request?(format : Format::Any, request : Term) : Term?
    Term.case(request) do
      matchpi %{[_symbol _ body_]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(request, {2, blob})
      end

      otherwise { request }
    end
  end

  private def http_encode_response?(format : Format::Any, response : Term) : Term?
    Term.case(response) do
      matchpi %{[_ [attachment body_]]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(response, {1, 1, blob})
      end

      matchpi %{[_ [file _string body_]]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(response, {1, 2, blob})
      end

      matchpi %{[_ body_]} do
        return unless blob = Format.encode?(format, body)

        Term.morph(response, {1, blob})
      end

      otherwise { response }
    end
  end

  private def http_decode?(format : Format::Any, term : Term) : Term?
    Term.case(term) do
      matchpi %{[_* bodyQ_blob]} do
        return unless body = Format.decode?(format, bodyQ.as_blob)

        Term.morph(term, {term.itemsize - 1, body})
      end

      otherwise { term }
    end
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, client : HttpClient, status : Term, incarnation : Harmony::ClientId) : D7::Patch?
    status_patch = D7.patch(client.node, {2, status})

    # If there's no current request, then there's no response to wait or be waiting for.
    return status_patch unless source = Rack.cell?(hg, client.outgoing)
    return status_patch unless request = source.value?

    return status_patch unless target = Rack.cell?(hg, client.ingoing)
    return status_patch unless target.value?.nil?

    return status_patch unless request = http_encode_request?(client.format, request)

    ctx.world.each(Harmony::HttpClientResponse, client_id: incarnation, request: request) do |fact|
      response = fact.result.response

      case response
      in Term
        response = http_decode?(client.format, response)

        case client.format_policy
        in .discard?
        in .abort?
          if response.nil?
            return D7.patch(client.node, {2, {:dn, "malformed response"}})
          end
        in .wrap?
          if response
            response = Term.of(:ok, response)
          else
            response = Term.of(:err, "malformed response")
          end
        end

        return D7.patches(
          D7.patch(client.node, {2, status}),
          D7.patch(source.node, {2, nil}),
          D7.patch(target.node, {2, response}),
        )
      in Harmony::HttpResponseError
        return D7.patch(client.node, {2, {:dn, response.detail}})
      end
    end

    ctx.goals.add(Harmony::HttpClientRequest.new(incarnation, request))

    status_patch
  end
end
