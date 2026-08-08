module Ww::Rack::WebSocket
  extend self

  defcase State,
    serving : Set(String),
    connected_to : Set(ClientConn),
    subscription : ->,
    schemas : GenerationalCache(Term, Schema::JSON),
    mutation: true

  def state(epoch : Automaton::Epoch) : State
    serving = Set(String).new
    connected_to = Set(ClientConn).new
    subscription = -> { epoch.call }
    schemas = GenerationalCache(Term, Schema::JSON).new
    State.new(serving, connected_to, subscription, schemas)
  end

  def pending?(state : State) : Bool
    state.serving.present? || state.connected_to.present?
  end

  defrecord StepContext,
    bindings : Set(String),
    conns : Set(ClientConn),
    dequeue : Set(ClientConn)

  def step(state : State, & : Proposer -> T) : T forall T
    seen_bindings = Set(String).new
    seen_conns = Set(ClientConn).new
    dequeue = Set(ClientConn).new

    result = state.schemas.epoch do
      ctx = StepContext.new(seen_bindings, seen_conns, dequeue)
      yield Proposer.new(state, ctx)
    end

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

  defcase Server,
    node : D7::Node,
    pool : D7::AbsEdge,
    binding : String,
    in_edge : Term,
    out_edge : Term,
    template : Term::Dict,
    format : Format::Any,
    format_policy : FormatPolicy

  defcase Client,
    node : D7::Node,
    message : D7::AbsEdge,
    conn : ClientConn,
    reply : D7::AbsEdge,
    format : Format::Any,
    format_policy : FormatPolicy

  private def binding?(binding : Term) : String?
    # |@ rack.ws.binding
    #
    # |@summary
    # Describes where to bind a WebSocket server.
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

  # See `rack.ws.format` to learn about the supported formats.
  #
  # TODO: limits, limits, limits!!
  module Format
    alias Any = None | TermJSON | TermJSONSchema | TermML | TermPrettyML

    defrecord None
    defrecord TermJSON
    defrecord TermJSONSchema, schema : Schema::JSON, top : Term
    defrecord TermML
    defrecord TermPrettyML
  end

  private def format?(state : State, hg : D7::Hypergraph, node : D7::Node, term : Term) : Format::Any?
    # |@ rack.ws.format
    #
    # |@summary
    # Describes how to (de)serialize terms.
    Term.case(term) do
      # |@ rack.ws.format
      #
      # |@pattern
      # none
      #
      # |@block
      # Allows to communicate using UTF-8 encoded messages (string terms) and
      # arbitrary byte payloads (blob terms).
      matchpi %{none} do
        Format::None.new
      end

      # |@ rack.ws.format
      #
      # |@pattern
      # json
      #
      # |@block
      # (De)serializes terms into JSON. This uses a *very* loose mapping of terms to
      # JSON. This is because terms do not map to JSON exactly, nor the other way.
      # Without you providing hints during deserialization, the terms you get out of
      # `format: json` can look very ugly.
      #
      # For example, the term `(+ 1 2 x: 100 y: 200)` is serialized by `format: json`
      # into `{"0": "+", "1": 1, "2": 2, "x": 100, "y": 200}`, which deserializes
      # into the term `{"0": "+", "1": 1, "2": 2, "x": 100, "y": 200}`.
      #
      # Use this only as a last resort. The better options are `jsonp` (JSON protocol,
      # where we serialize terms into JSON objects properly tagged with types etc.)
      # or `(json @_ _)`, where you can specify a schema to drive the decoding. The latter
      # is the recommended approach since it reduces the attack surface by forcing
      # you to explicitly specify the kinds of JSON that are accepted.
      matchpi %{json} do
        Format::TermJSON.new
      end

      # |@ rack.ws.format
      #
      # |@pattern
      # ml
      #
      # |@block
      # Uses a compact, restricted subset of WwML to (de)serialize terms
      # transparently for you.
      #
      # Compact ML includes the following parts of the WwML grammar:
      # - Number literals such as `100`, `1.23`, `≈100`, `1/2`.
      # - String literals such as `"hello"`.
      # - Symbol literals such as `xyz`, `⸍qux⸝`.
      # - Boolean literals `true` and `false`.
      # - Dictionary terms of the general form:  `()`, `(+ 1 2)`, `(+ 1 2 x: 100 y: 200)`,
      #   etc. Even pairsonly dictionaries are represented this way: `(x: 100 y: 200)`.
      #
      # NOTE: Right now, this still uses the full-blown WwML parser; which means
      # `ml` is vulnerable to all sorts of things; in the future, we plan to use
      # a smaller, faster, better fortified parser for this, since we expect `format: ml`
      # to be Internet-facing in some scenarios.
      matchpi %{ml} do
        Format::TermML.new
      end

      # |@ rack.ws.format
      #
      # |@pattern
      # prettyml
      #
      # |@block
      # Uses WwML to encode (pretty print) the term, with all the shorthands and
      # associated slowness. May produce multiline output. For example, `±x`
      # is serialized as `(%let x _number)` when using `ml`, but with `prettyml`,
      # it is serialized as `±x`.
      #
      # Use this for debugging / visualization only. `prettyml` is not guaranteed
      # to be fast -- not to parse, nor to pretty print.
      matchpi %{prettyml} do
        Format::TermPrettyML.new
      end

      # |@ rack.ws.format
      #
      # |@pattern
      # (json @edge_ top_)
      #
      # |@key edge rack.edge
      # Tells where to look for the JSON schema node (see `rack.schema`).
      #
      # |@key top
      # Selects a toplevel rule from the JSON schema.
      #
      # |@block
      # Uses a JSON schema to (de)serialize terms. See `rack.schema` for
      # more info.
      matchpi %{(json @edge_ top_)} do
        schemas = Pf::Kit.stack_array(Schema::JSON, 1)

        hg.each_member(hg.resolve(node.addr, edge), heads: {Term.of(:schema)}) do |candidate|
          Term.matchpi?(candidate.term, %{[schema (@_ json) schemaQ_*]}) do
            schema = state.schemas.put_if_absent(schemaQ) do
              Schema::JSON.new(schemaQ)
            end
            schemas << schema
          end
        end

        continue unless schema = schemas.single?

        Format::TermJSONSchema.new(schema, top)
      end

      otherwise { }
    end
  end

  enum FormatPolicy
    Discard
    Abort
    Wrap
  end

  def format_policy?(term : Term) : FormatPolicy?
    # |@ rack.ws.format-policy
    #
    # |@summary
    # Determines how adherence to a format is maintained.
    Term.case(term) do
      # |@ rack.ws.format-policy
      #
      # |@pattern
      # discard
      #
      # |@block
      # Messages that were decoded successfully are passed as-is. Messages that
      # were not are discarded without notifying the offending client.
      matchpi %{discard} do
        FormatPolicy::Discard
      end

      # |@ rack.ws.format-policy
      #
      # |@pattern
      # abort
      #
      # |@block
      # Messages that were decoded successfully are passed as-is. Messages that
      # were not trigger connection closure.
      matchpi %{abort} do
        FormatPolicy::Abort
      end

      # |@ rack.ws.format-policy
      #
      # |@pattern
      # wrap
      #
      # |@block
      # Messages are wrapped in a result type: `(ok msg_)` if decoded successfully,
      # where *msg* is the decoded message; or `(err detail_string)`, explaining
      # why decoding failed.
      #
      # The client device is free to interpret this however it may; e.g., by
      # notifying the corresponding client of the error, or by somehow handling
      # it entirely on the server-side.
      matchpi %{wrap} do
        FormatPolicy::Wrap
      end

      otherwise { }
    end
  end

  alias ClientConn = WebSocketClientService::Conn

  CLIENT_DEFAULT_MAX_RETRIES = 10u32

  # NOTE: Can only be a string for compatibility with the circuit-side (URIs
  # can only do strings, we don't and probably shouldn't parse them any further).
  CLIENT_DEFAULT_KEY = Term.of("master")

  def client_conn?(term : Term) : ClientConn?
    Term.case(term) do
      matchpiT %{(local port←(%number u16))} do
        ClientConn.new("127.0.0.1", port, "", CLIENT_DEFAULT_KEY, false, CLIENT_DEFAULT_MAX_RETRIES)
      end

      matchpiT %{(public port←(%number u16))} do
        ClientConn.new("0.0.0.0", port, "", CLIENT_DEFAULT_KEY, false, CLIENT_DEFAULT_MAX_RETRIES)
      end

      matchpi %{_string} do
        return unless uri = URI.parse(term.to(String))
        return unless host = uri.host
        return unless port = uri.port
        return unless UInt16::MIN <= port <= UInt16::MAX

        case uri.scheme
        when "ws"  then secure = false
        when "wss" then secure = true
        else
          return
        end

        key = Term.of(uri.query_params["key"]?) || CLIENT_DEFAULT_KEY
        max_retries = uri.query_params["max-retries"]?.try(&.to_u32?) || CLIENT_DEFAULT_MAX_RETRIES

        ClientConn.new(host, port.to_u16, uri.path, key, secure, max_retries)
      end

      otherwise { }
    end
  end

  private def encode?(format : Format::None, term : Term) : String?
    if str = term.as_s?
      return str.to(String)
    end

    ML.compact(term)
  end

  # TODO: TermJSONSchema should probably do the same checks on terms, handling
  # the emit side as well.
  private def encode?(format : Format::TermJSON | Format::TermJSONSchema, term : Term) : String?
    JSON.build do |json|
      encode(format, json, term)
    end
  end

  private def encode?(format : Format::TermML, term : Term) : String?
    ML.compact(term)
  end

  private def encode?(format : Format::TermPrettyML, term : Term) : String?
    ML.display(term, maxwidth: 80)
  end

  private def encode(format, json : JSON::Builder, term : Term) : Nil
    encode(format, json, Term[term])
  end

  private def encode(format, json : JSON::Builder, term : Term::Num) : Nil
    case repr = term.repr
    in Int64, Float32
      json.number(repr)
    in BigRational
      # if format.fractions
      #   string
      # else
      #   to_f64
      json.number(repr.to_f64)
    end
  end

  private def encode(format, json : JSON::Builder, term : Term::Str | Term::Sym) : Nil
    json.string(term.to(String))
  end

  private def encode(format, json : JSON::Builder, term : Term::Boolean) : Nil
    json.bool(term.true?)
  end

  private def encode(format, json : JSON::Builder, term : Term::Dict) : Nil
    if term.itemsonly?
      json.array do
        term.items.each do |item|
          encode(format, json, item)
        end
      end

      return
    end

    json.object do
      # Generic dictionary.
      term.each_entry(in: Term::Dict.entries_ord) do |key, value|
        if key.type.string?
          json.string(key.to(String))
        else
          # x: 100 becomes "x": 100
          # {x: 100, y: 200}: 300 becomes "{x: 100, y: 200}": 300
          #
          # When we decode, we can only recover the key if the user explicitly tells
          # us to (e.g. through a schema).
          json.string(ML.compact(key))
        end

        encode(format, json, value)
      end
    end
  end

  private def encode(format, json : JSON::Builder, term : Term::Blob) : Nil
    json.string do |io|
      Base64.strict_encode(term, io)
    end
  end

  private def decode?(format : Format::None, message : String) : Term?
    Term.of(message)
  end

  private def decode?(format : Format::TermJSON, message : String) : Term?
    begin
      Schema::JSON.read(message)
    rescue e : JSON::ParseException
      Log.debug(exception: e) { "error while decoding JSON message" }
    end
  end

  private def decode?(format : Format::TermJSONSchema, message : String) : Term?
    begin
      Schema::JSON.read(format.schema, format.top, message)
    rescue e : JSON::ParseException
      Log.debug(exception: e) { "error while decoding JSON message" }
    end
  end

  private def decode?(format : Format::TermML | Format::TermPrettyML, message : String) : Term?
    begin
      ML.term(message)
    rescue e : ML::SyntaxError
      Log.debug(exception: e) { "error while decoding WwML message" }
    end
  end

  # :nodoc:
  def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :ws) do |node|
      Term.case(node.term) do
        matchpi(<<-WWML) do
        [ws (server @pool_ bindingQ_ _?
              ⍊ in: (%optional @in @input_)
                out: (%optional @out @output_)
                format: (%optional none formatQ_)
                format-policy: (%optional discard policyQ_))
          template_*]
        WWML
          continue unless binding = binding?(bindingQ)
          continue unless format = format?(state, hg, node, formatQ)
          continue unless format_policy = format_policy?(policyQ)

          abs_pool = hg.resolve(node.addr, pool)
          variant = stack_alloc Server.new(node, abs_pool, binding, input, output, template.as_d, format, format_policy)
          step(state, ctx, hg, variant)
        end

        # - Allow the circuit to use an errorless `dn` to disable the socket.
        # - Use `closed` instead of simply `dn` for both to avoid confusing server-side
        #   closure (`closed`) and client-side closure (`dn`).
        matchpi %{[ws [client (@_ -> _ -> @_)] (%any dn closed)]} do
        end

        matchpi(<<-WWML) do
        [ws
          (client (@message_ -> connQ_ -> @reply_)
            ⍊ format: (%optional none formatQ_)
              format-policy: (%optional discard policyQ_))
          _?]
        WWML
          continue unless conn = client_conn?(connQ)
          continue unless format = format?(state, hg, node, formatQ)
          continue unless format_policy = format_policy?(policyQ)

          abs_message = hg.resolve(node.addr, message)
          abs_reply = hg.resolve(node.addr, reply)
          variant = stack_alloc Client.new(node, abs_message, conn, abs_reply, format, format_policy)
          step(state, ctx, hg, variant)
        end

        otherwise { }
      end
    end
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, server : Server) : D7::Patch?
    # If there's no associated pool, then the server's "machine" is incomplete,
    # so it cannot handle requests -- nor does it *exist*, really.
    unless pool = Rack.pool?(hg, server.pool)
      # (ws (_ _ server ⏏) _*)
      return D7.patch(server.node, {1, 3, {:dn, "missing pool"}})
    end

    ctx.bindings << server.binding

    case status = WebSocketServerService.checkout?(server.binding)
    in Nil, WebSocketServerService::Pending
      # (ws (_ _ server ⏏) _*)
      D7.patch(server.node, {1, 3, :pending})
    in WebSocketServerService::Dn # Error
      # (ws (_ _ server ⏏) _*)
      D7.patch(server.node, {1, 3, {:dn, status.detail}})
    in WebSocketServerService::Up
      journal = status.journal

      contents0 = pool.contents
      contents1 = pool.contents

      # Process events from the journal.
      journal.each do |event|
        case event
        in WebSocketServerService::ClientConnected
          # O(1)
          contents1 = contents1.append(client_repr(event.id, server))
        in WebSocketServerService::ClientDisconnected
          # O(N) ?!?!?!?
          contents1 = fmap(contents1) do |client|
            case client
            in ConnectedClient
              client.id == event.id ? nil : client
            in DisconnectedClient
            end
          end
        in WebSocketServerService::ClientReceived
          message = decode?(server.format, event.message)
          next if message.nil? && server.format_policy.discard?

          # O(N) ?!?!?!?
          contents1 = fmap(contents1) do |client|
            case client
            in ConnectedClient
              next client unless client.id == event.id
              next client unless inbound = client.inbound?

              # Kick client if message fails to decode.
              next if message.nil? && server.format_policy.abort?

              if server.format_policy.wrap?
                message = message ? Term.of(:ok, message) : Term.of(:err, "invalid message")
              end

              assert message

              client.copy_with(inbound: inbound.copy_with(queue: inbound.queue.append(message)))
            in DisconnectedClient
            end
          end
        end
      end

      seen = Set(UUID).new

      # Find outbound messages from clients.
      #
      # O(N) ?!?!?!?
      contents1 = fmap(contents1) do |client|
        case client
        in ConnectedClient
          seen << client.id

          if outbound = client.outbound?
            outq = outbound.queue.items
            while term = outq.first?
              # If the message fails to encode, instead of suppressing it, which would
              # just be a source of hard-to-find bugs, we "clog" the outbound queue.
              #
              # TODO: We should also provide a descriptive error message! Encode doesn't
              # fail by itself, only when *format* limits are exceeded by *term*.
              break unless message = encode?(server.format, term)

              WebSocketServerService.send(server.binding, client.id, message)

              outq = outq.move(1)
            end

            client = client.copy_with(outbound: outbound.copy_with(queue: outq.collect))
          end

          client
        in DisconnectedClient
        end
      end

      # Find clients that disconnected / were "deformed" so much that we
      # can't see them.
      status.clients.each do |client_id|
        next if client_id.in?(seen)

        WebSocketServerService.drop(server.binding, client_id)
      end

      # (ws (_ _ server ⏏) _*)
      # (circuit @pool ⏏)
      D7.patches(
        D7.patch(server.node, {1, 3, :up}),
        D7.patch(pool.node, {2, contents1}),
      )
    end
  end

  private def step(state : State, ctx : StepContext, hg : D7::Hypergraph, client : Client) : D7::Patch?
    case status = WebSocketClientService.checkout?(client.conn)
    in Nil # Not connected
      if client.conn.in?(state.connected_to)
        # Client was connected and now isn't.
        status_patch = D7.patch(client.node, {2, :closed})
      else
        # Client was not connected (the client just joined the circuit and
        # wants to connect).
        ctx.conns << client.conn
        status_patch = D7.patch(client.node, {2, nil})
      end
    in WebSocketClientService::Up # Connected
      ctx.conns << client.conn
      status_patch = D7.patch(client.node, {2, :up})
    in WebSocketClientService::Dn # Tried to connect, but ended up with an error
      ctx.conns << client.conn
      status_patch = D7.patch(client.node, {2, {:dn, status.detail}})
    in WebSocketClientService::Pending # Connecting...
      ctx.conns << client.conn
      status_patch = D7.patch(client.node, {2, :pending})
    end

    source_patch = pass do
      next unless source = Rack.cell?(hg, client.message)
      next unless message = source.value?

      # If the message fails to encode (e.g. due to limits) we "clog" the message
      # cell so that failure is evident.
      #
      # TODO: We should also provide a descriptive error message explaining why
      # the thing doesn't encode!
      next unless message = encode?(client.format, message)
      next unless WebSocketClientService.send?(client.conn, message)

      D7.patch(source.node, {2, nil})
    end

    target_patch = pass do
      next unless target = Rack.cell?(hg, client.reply)
      next unless target.value?.nil?
      next unless reply = WebSocketClientService.head?(client.conn)

      ctx.dequeue << client.conn

      reply = decode?(client.format, reply)

      case client.format_policy
      in .discard?
        next if reply.nil?
      in .abort?
        if reply.nil?
          return D7.patch(client.node, {2, {:dn, "invalid message"}})
        end
      in .wrap?
        if reply
          reply = Term.of(:ok, reply)
        else
          reply = Term.of(:err, "invalid message")
        end
      end

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

  private def client_repr(id : UUID, server : Server) : Term
    device = Term::Dict.build do |commit|
      commit << :device
      commit << {:cell, {:edge, :id}, id.to_s}
      commit << {:cell, server.in_edge, Term[]}
      commit << {:cell, server.out_edge, Term[]}
      commit.concat(server.template.items)
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
          matchpi %{[cell @in msgs←(_*)]} do
            next if inbound # Duplicate `in`

            inbound = ClientQueue.new(key, msgs.as_d)
          end

          # Recognize the outbox cell.
          matchpi %{[cell @out msgs←(_*)]} do
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
end
