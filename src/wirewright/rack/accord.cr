# |@ rack.[network]
#
# |@summary
# Groups articles related to the `rack.client` and `rack.server` nodes.

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
        matchpi %{[server (@_ _ dn) _*]} { }
        matchpi %{[server (@_ _ (dn _string)) _*]} { }

        matchpi %{[server (@pool_ configQ_ _?) template_*]} do
          next unless config = server_config?(state, hg, node.addr, configQ)

          abs_pool = hg.resolve(node.addr, pool)

          case config
          in HttpServerConfig
            machine = stack_alloc HttpServer.new(node, config.transport, abs_pool, config.encoder, config.decoder, template.as_d)
          in SocketServerConfig
            case transport = config.transport
            in Harmony::SocketServerDefn
              machine = stack_alloc SocketServer.new(node, transport, abs_pool, config.encoder, config.decoder, config.encoder_capacity, config.decoder_capacity, template.as_d)
            in WebSocketServerDefn
              machine = stack_alloc WebSocketServer.new(node, transport.defn, transport.link, abs_pool, config.encoder, config.decoder, config.encoder_capacity, config.decoder_capacity, template.as_d)
            end
          end

          step(ctx, hg, machine)
        end

        otherwise { }
      end
    end

    hg.propose(proposals, :client) do |node|
      Term.case(node.term) do
        # Allow the circuit to use an errorless `dn` to disable the socket. Also
        # ignore clients that are currently down for other reasons.
        matchpi %{[client [_ -> _ -> _] dn]} { }
        matchpi %{[client [_ -> _ -> _] (dn _string)]} { }

        matchpi(
          %{[client (encoderQ_ -> transportQ_ -> decoderQ_)]},
          %{[client (encoderQ_ -> transportQ_ -> decoderQ_) _]},
        ) do
          defn = http_client_transport?(hg, node.addr, transportQ) ||
                 socket_client_transport?(hg, node.addr, transportQ)

          continue unless defn

          next unless encoder = encoder?(state, hg, node.addr, encoderQ)
          next unless decoder = decoder?(state, hg, node.addr, decoderQ)

          if defn.is_a?(Harmony::HttpClientDefn)
            machine = stack_alloc HttpClient.new(node, defn, encoder, decoder)
          else
            machine = stack_alloc SocketClient.new(node, defn, encoder, decoder)
          end

          step(ctx, hg, machine)
        end

        otherwise { }
      end
    end
  end

  # :nodoc:
  EDGE_IN = Term.of(:edge, :in)
  # :nodoc:
  EDGE_OUT = Term.of(:edge, :out)

  alias ServerConfig = HttpServerConfig | SocketServerConfig

  defrecord HttpServerConfig,
    encoder : Encoder,
    decoder : Decoder,
    transport : Harmony::HttpServerDefn

  defrecord SocketServerConfig,
    encoder : Encoder,
    decoder : Decoder,
    encoder_capacity : UInt32,
    decoder_capacity : UInt32,
    transport : Harmony::SocketServerDefn | WebSocketServerDefn

  private def server_config?(state : State, hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : ServerConfig?
    # |@ rack.server.config
    #
    # |@summary
    # Describes the configuration of a server.
    Term.case(term) do
      # |@ rack.server.config
      #
      # |@pattern
      # (decoder_ -> transport_ -> encoder_)
      #
      # |@key decoder rack.[network].decoder
      # The decoder that all client devices will use to parse ingoing bytes
      # into terms.
      #
      # |@key transport rack.[network].transport
      # The transport to use.
      #
      # |@key encoder rack.[network].encoder
      # The encoder that all client devices will use to convert terms to
      # outgoing bytes.
      #
      # |@block
      # The full form of the config.
      #
      # ### Socket servers
      #
      # For socket servers, the full form of the *encoder* `(_ @_)`, *decoder*, or both
      # can be extended with the `capacity` pair: `capacity: (%number u32)`.
      #
      # `capacity` caps the number of messages in the ingoing queue, the outgoing queue,
      # or both. If a capped queue contains *capacity* or more messages, the client
      # device will refuse to accept new messages.
      #
      # `capacity` only works with transports whose `link` implements backpressure
      # (such as `link: handoff`; see `rack.[network].link`).
      #
      # |@example
      # ```wwml
      # (server (@pool ((text @request) -> (http local 5000) -> (text @response)))
      #   (cell @response (ok "Hello World")))
      # (pool @pool)
      # ```
      #
      # Limiting the capacity of the ingoing message queue means the client device will
      # refuse to accept more messages until the ingoing message queue is exhausted:
      #
      # ```wwml
      # ;; Frame 0 (seed)
      #
      # (server (@pool ((text @in capacity: 1) -> (ws local 5000 link: handoff) -> (text @out)))
      #   (feed (@in front) (@back out)))
      # (pool @pool)
      #
      # (queue (@name @names) ("Alice" "Bob" "Charlie"))
      # (client (@name -> (ws local 5000 link: handoff) -> @replies))
      # ```
      #
      # The client above is broken: it doesn't have anywhere to put replies. Thanks to
      # the combination of `link: handoff` and `capacity: 1`, the client will only send
      # `"Alice"` to the client device; which is promptly moved into the outgoing queue,
      # and stalls there since it has nowhere to go. To illustrate:
      #
      # ```wwml
      # ;; Frame N (omitting server)
      #
      # (pool @pool
      #   (device
      #     (cell @id "unique client device id")
      #     (cell @in ())
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # (queue (@name @names) ("Alice" "Bob" "Charlie"))
      # (client (@name -> (ws local 5000 link: handoff) -> @replies)
      #   up)
      #
      # ;; Frame N+1
      #
      # (pool @pool
      #   (device
      #     (cell @id "unique client device id")
      #     (cell @in ("Alice"))
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # (queue (@name @names) ("Bob" "Charlie"))
      # (client (@name -> (ws local 5000 link: handoff) -> @replies)
      #   up)
      #
      # ;; Frame N+2
      #
      # (pool @pool
      #   (device
      #     (cell @id "unique client device id")
      #     (cell @in ())
      #     (cell @out ("Alice"))
      #     (feed (@in front) (@out back))))
      #
      # (queue (@name @names) ("Bob" "Charlie"))
      # (client (@name -> (ws local 5000 link: handoff) -> @replies)
      #   up)
      # ```
      #
      # `"Alice"` will sit in the client device's out queue until the connection is
      # dropped or until `client` finally fixes itself and adds a replies cell or queue:
      #
      # ```wwml
      # ;; Frame N+M
      #
      # (pool @pool
      #   (device
      #     (cell @id "unique client device id")
      #     (cell @in ())
      #     (cell @out ("Alice"))
      #     (feed (@in front) (@out back))))
      #
      # (queue (@name @names) ("Bob" "Charlie"))
      # (queue (@reply @replies) ())
      # (client (@name -> (ws local 5000 link: handoff) -> @replies)
      #   up)
      #
      # ;; Frame N+M+1
      #
      # (pool @pool
      #   (device
      #     (cell @id "unique client device id")
      #     (cell @in ())
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # (queue (@name @names) ("Bob" "Charlie"))
      # (queue (@reply @replies) ("Alice"))
      # (client (@name -> (ws local 5000 link: handoff) -> @replies)
      #   up)
      # ```
      #
      # After this, *names* proceed to be sent properly.
      #
      # You can similarly limit the out queue:
      #
      # ```wwml
      # (server (@pool ((text @in capacity: 1) -> (ws local 5000 link: handoff) -> (text @out capacity: 1)))
      #   (feed (@in front) (@back out)))
      # (pool @pool)
      # ```
      #
      # In the above, I limited *both* of them. This means only one message gets to enter
      # the client device's in queue. Then if it is moved away for processing, more messages
      # can enter the client device. When a reply is put in the client device's out queue,
      # no more messages are accepted until the other side confirms it received it.
      matchpi %{(decoderQ_dict -> transportQ_ -> encoderQ_dict)} do
        return unless decoder = decoder?(state, hg, addr, decoderQ)
        return unless transport = server_transport?(hg, addr, transportQ)
        return unless encoder = encoder?(state, hg, addr, encoderQ)

        case transport
        in Harmony::HttpServerDefn
          HttpServerConfig.new(encoder, decoder, transport)
        in Harmony::SocketServerDefn, WebSocketServerDefn
          decoder_capacity = UInt32::MAX
          Term.matchpi?(decoderQ, %{(_ @_ ⍊ capacity_: (%number u32))}) do
            decoder_capacity = capacity.to(UInt32)
          end

          encoder_capacity = UInt32::MAX
          Term.matchpi?(encoderQ, %{(_ @_ ⍊ capacity_: (%number u32))}) do
            encoder_capacity = capacity.to(UInt32)
          end

          SocketServerConfig.new(encoder, decoder, encoder_capacity, decoder_capacity, transport)
        end
      end

      # |@ rack.server.config
      #
      # |@pattern
      # transport_
      #
      # |@key transport rack.[network].transport
      # The transport to use.
      #
      # |@block
      # The short form of the config. It is a shorthand for
      # `(text @in) -> <your transport> -> (text @out)`.
      #
      # |@example
      # ```wwml
      # (server (@pool (ws local 5000))
      #   (feed (@in front) (@out back)))
      # (pool @pool)
      # ```
      otherwise do
        return unless transport = server_transport?(hg, addr, term)

        encoder = Encoder.new(EDGE_OUT, Format::Plaintext.new, policy: :clog)
        decoder = Decoder.new(EDGE_IN, Format::Plaintext.new, policy: :discard)

        encoder_capacity = UInt32::MAX
        decoder_capacity = UInt32::MAX

        case transport
        in Harmony::HttpServerDefn
          HttpServerConfig.new(encoder, decoder, transport)
        in Harmony::SocketServerDefn, WebSocketServerDefn
          SocketServerConfig.new(encoder, decoder, encoder_capacity, decoder_capacity, transport)
        end
      end
    end
  end

  alias ServerTransport = Harmony::ServerDefn | WebSocketServerDefn

  defrecord WebSocketServerDefn,
    defn : Harmony::HttpServerDefn,
    link : Harmony::Link

  private def server_transport?(hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : ServerTransport?
    Term.case(term) do
      # WebSockets are special in that they require an HTTP(S) server but they're
      # not an HTTP(S) server on its own. In a sense they're both an HTTP(S) server
      # and a socket server. So we have to handle them in this ugly way.
      matchpi(
        %{(ws _* ⍊ link: (%optional stream linkQ_))},
        %{(wss _* ⍊ link: (%optional stream linkQ_))},
      ) do
        return unless transport = http_server_transport?(hg, addr, term)
        return unless link = link?(linkQ)

        WebSocketServerDefn.new(transport, link)
      end

      otherwise do
        http_server_transport?(hg, addr, term) || socket_server_transport?(hg, addr, term)
      end
    end
  end

  enum EncoderPolicy
    Clog
    Discard
  end

  private def encoder_policy?(term : Term) : EncoderPolicy?
    # |@ rack.[network].encoder.policy
    #
    # |@summary
    # The ways encoding can fail.
    Term.case(term) do
      # |@ rack.[network].encoder.policy
      #
      # |@pattern
      # clog
      #
      # |@block
      # Messages that were encoded successfully proceed to the remote endpoint.
      # Messages that failed to encode stay in place without further progress.
      matchpi %{clog} do
        EncoderPolicy::Clog
      end

      # |@ rack.[network].encoder.policy
      #
      # |@pattern
      # discard
      #
      # |@block
      # Messages that were encoded successfully proceed to the remote endpoint.
      # Messages that were not are dropped.
      matchpi %{discard} do
        EncoderPolicy::Discard
      end

      otherwise { }
    end
  end

  enum DecoderPolicy
    Discard
    Wrap
  end

  private def decoder_policy?(term : Term) : DecoderPolicy?
    # |@ rack.[network].decoder.policy
    #
    # |@summary
    # The ways decoding can fail.
    Term.case(term) do
      # |@ rack.[network].decoder.policy
      #
      # |@pattern
      # discard
      #
      # |@block
      # Messages that were decoded successfully are passed through. Messages
      # that were not are dropped.
      matchpi %{discard} do
        DecoderPolicy::Discard
      end

      # |@ rack.[network].decoder.policy
      #
      # |@pattern
      # wrap
      #
      # |@block
      # Messages are wrapped in a result type: `(ok msg_)` if decoded successfully,
      # where *msg* is the decoded message; or `(err detail_string)`, where *detail*
      # should explain why decoding failed.
      matchpi %{wrap} do
        DecoderPolicy::Wrap
      end

      otherwise { }
    end
  end

  defrecord Encoder, edge : Term, format : Format::Any, policy : EncoderPolicy

  private def encoder?(state : State, hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Encoder?
    # |@ rack.[network].encoder
    #
    # |@summary
    # Description of an encoder.
    #
    # |@block
    # An encoder uses a *format* to convert a Wirewright term to an arbitrary byte
    # payload (blob).
    Term.case(term) do
      # |@ rack.[network].encoder
      #
      # |@pattern
      # @edge_
      #
      # |@block
      # Shorthand for `(@edge_ text)`. E.g., instead of writing `(@x text)`, you
      # can simply write `@x`.
      matchpi %{@_} do
        format = Format::Plaintext.new
        Encoder.new(term, format, policy: :clog)
      end

      # |@ rack.[network].encoder
      #
      # |@pattern
      # (format_ @edge_ ⍊ policy_⋮ clog)
      #
      # |@key format rack.[network].format
      # The format to use. E.g., `ml`, `text`.
      #
      # |@key policy rack.[network].encoder.policy
      # How encode failure should mainifest.
      #
      # |@block
      # Constructs an *encoder* given its format and its failure mode.
      matchpi %{(formatQ_ @edge_ ⍊ policy: (%optional clog policyQ_))} do
        return unless format = format?(state.schemas, hg, addr, formatQ)
        return unless policy = encoder_policy?(policyQ)

        Encoder.new(edge, format, policy)
      end

      otherwise { }
    end
  end

  # :nodoc:
  #
  # NOTE: Even though the format can contain an edge (e.g. schema), the server
  # or client node actually doesn't participate in that edge, so we don't have
  # to yield it here.
  def each_encoder_edge(term : Term, & : Term ->) : Nil
    Term.case(term) do
      matchpi %{@_} { yield term }
      matchpi %{[_ @edge_]} { yield edge }
      otherwise { }
    end
  end

  defrecord Decoder, edge : Term, format : Format::Any, policy : DecoderPolicy

  private def decoder?(state : State, hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Decoder?
    # |@ rack.[network].decoder
    #
    # |@summary
    # Description of a decoder.
    #
    # |@block
    # A decoder uses a *format* to convert an arbitrary byte payload (blob) to
    # a Wirewright term.
    Term.case(term) do
      # |@ rack.[network].decoder
      #
      # |@pattern
      # @edge_
      #
      # |@block
      # Shorthand for `(@edge_ text)`. E.g., instead of writing `(@x text)`, you
      # can simply write `@x`.
      matchpi %{@_} do
        format = Format::Plaintext.new
        Decoder.new(term, format, policy: :discard)
      end

      # |@ rack.[network].decoder
      #
      # |@pattern
      # (format_ @edge_ ⍊ policy_⋮ discard)
      #
      # |@key format rack.[network].format
      # The format to use. E.g., `ml`, `text`.
      #
      # |@key policy rack.[network].decoder.policy
      # How decode failure should mainifest.
      #
      # |@block
      # Constructs a *decoder* given the desired format and its failure mode.
      matchpi %{(formatQ_ @edge_ ⍊ policy: (%optional discard policyQ_))} do
        return unless format = format?(state.schemas, hg, addr, formatQ)
        return unless policy = decoder_policy?(policyQ)

        Decoder.new(edge, format, policy)
      end

      otherwise { }
    end
  end

  # :nodoc:
  #
  # NOTE: Even though the format can contain an edge (e.g. schema), the server
  # or client node actually doesn't participate in that edge, so we don't have
  # to yield it here.
  def each_decoder_edge(term : Term, & : Term ->) : Nil
    Term.case(term) do
      matchpi %{@_} { yield term }
      matchpi %{[_ @edge_]} { yield edge }
      otherwise { }
    end
  end

  private def format?(schemas : ICache, hg : D7::Hypergraph, addr : D7::NodeAddr, term : Term) : Format::Any?
    # |@ rack.[network].format
    #
    # |@summary
    # Description of how to (de)serialize terms.
    Term.case(term) do
      # |@ rack.[network].format
      #
      # |@pattern
      # binary
      #
      # |@block
      # Arbitrary binary payloads (blob terms), e.g. `⟬de ad be ef⟭`.
      #
      # It is possible to *send* (but not receive) strings with `format: binary`. In
      # that case, the string's UTF-8 byte representation is sent. If you want to send
      # *and* receive strings, consider using `text`.
      #
      # NOTE: For transports that do not support content-type (TCP, UNIX, WebSockets etc.),
      # the blob's media type will be stripped before the blob is sent.
      matchpi %{binary} do
        Format::Binary.new
      end

      # |@ rack.[network].format
      #
      # |@pattern
      # text
      #
      # |@block
      # Send and receive string terms, transmitted over the wire as UTF-8.
      # E.g. `"hello world"`.
      matchpi %{text} do
        Format::Plaintext.new
      end

      # |@ rack.[network].format
      #
      # |@pattern
      # json
      #
      # |@block
      # JSON payloads. (De)serializes terms into JSON. This uses a *very* loose
      # mapping of terms to JSON. This is because terms do not map to JSON exactly.
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
      # you to explicitly specify the kinds of JSON to accept.
      matchpi %{json} do
        Format::TermJSON.new
      end

      # |@ rack.[network].format
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
        targets = Pf::Kit.stack_array(Schema::JSON, 1)

        hg.each_member(hg.resolve(addr, edge), heads: {Term.of(:schema)}) do |candidate|
          Term.matchpi?(candidate.term, %{[schema (@_ json) schemaQ_*]}) do
            targets << schemas.put_if_absent(schemaQ) { Schema::JSON.new(schemaQ) }
          end
        end

        continue unless schema = targets.single?

        Format::TermJSONSchema.new(schema, top)
      end

      # |@ rack.[network].format
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

      # |@ rack.[network].format
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

      otherwise { }
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
      # handoff
      #
      # |@block
      # Uses the handoff protocol to transmit the payload.
      #
      # Linking with `handoff` is more reliable than with `stream`, and interacts
      # well with the semantics of Rack. You can imagine `stream` as a "firehose"
      # for messages, and `handoff` as a careful message exchange where each party
      # respects the other's capacity.
      #
      # For example, a client's outgoing message cell is not emptied until the message
      # crosses over to the other side *and the other side confirms that*. This provides
      # a natural kind of backpressure. Nor are messages *sent* until the other side
      # tells us its ingoing message cell is empty.
      #
      # The main drawback of `link: handoff` is that it places more load on the network,
      # may involve round-trips and may even send the payload just for it to be declined
      # by the other side (although the last point should be rare in practice).
      #
      # ### The handoff protocol
      #
      # TODO: Documentation
      matchpi %{handoff} do
        Harmony::HandoffLink.new
      end

      # |@ rack.[network].link
      #
      # |@pattern
      # stream
      #
      # |@block
      # Direct passthrough of the payload to the underlying transport.
      #
      # - No application-level backpressure (senders do not care about receivers).
      # - Message sends are confirmed locally (senders do not care about acknowledgement
      #   or feedback about the message they sent from receivers).
      #
      # More importantly, with `link: stream`, there is a window of time when the message
      # is neither on the sender's side nor on the receiver's side -- it is "in the wire". If
      # anything happens to the connection while a message is travelling in the wire, the message
      # is lost. So you wouldn't want to e.g. transfer money between peers with `link: stream`.
      matchpi %{stream} do
        Harmony::StreamLink.new
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
      # Shorthand for `"127.0.0.1"`.
      matchpi %{local} do
        "127.0.0.1"
      end

      # |@ rack.[network].host
      #
      # |@pattern
      # public
      #
      # |@block
      # Shorthand for `"0.0.0.0"`.
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
      # from the server's `up`. The `up` is different for servers with an `auto`
      # port, in that it also tells the port: `(up port: (%number u16))`.
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
  # What is said above applies to servers, too; a server's transport can also
  # have a `key: _`. But this is more of a rarity; it's not often that you put
  # a server inside each button, say (whereas it would make sense to put a client in
  # each button, if e.g. the buttons are responsible for sending requests).
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
      # (tcp host_ port_ ⍊ key_⋮ master link_⋮ stream)
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
      # (pool @pool)
      # ```
      matchpiT %{(tcp hostQ_ portQ_ ⍊ key: (%optional master keyQ_) link: (%optional stream linkQ_))} do
        return unless host = host?(hostQ)
        return unless port = port?(portQ)
        return unless key = key?(hg, addr, keyQ)
        return unless link = link?(linkQ)

        Harmony::TcpServerDefn.new(host, port, key, link)
      end

      # |@ rack.server.transport
      #
      # |@pattern
      # (unix path_string ⍊ key_⋮ master link_⋮ stream)
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
      # (pool @pool)
      # ```
      matchpiT %{(unix path_string ⍊ key: (%optional master keyQ_) link: (%optional stream linkQ_))}, path: NormalPath do
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
      # If your protocol or the way you use `client` allows you to, you may actually want
      # automatic reconnects. That's why `renew: true` exists, to relieve you of the need
      # to manually reset the client.
      #
      # You can still reconnect a broken client by clearing its status, either manually
      # (by literally deleting the status) or through rules.

      # |@ rack.client.transport
      #
      # |@pattern
      # (ws host_ port_ ⍊ key_⋮ master link_⋮ stream path⋮ "" renew⋮ false)
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
          link: (%optional stream linkQ_)
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
      # (wss host_ port_ ⍊ key_⋮ master path⋮ "" link_⋮ stream renew⋮ false)
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
          link: (%optional stream linkQ_)
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
      # (tcp host_ port_ ⍊ key_⋮ master link_⋮ stream renew⋮ false)
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
          link: (%optional stream linkQ_)
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
      # (unix path_string ⍊ key_⋮ master link_⋮ stream renew⋮ false)
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
          link: (%optional stream linkQ_)
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
      # (ws host_ port_ ⍊ key_⋮ master link_⋮ stream)
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
      # (pool @pool)
      # ```

      # |@ rack.server.transport
      #
      # |@pattern
      # (wss host_ port_ ⍊ key_⋮ master link_⋮ stream ssl-cert_string ssl-key_string)
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
    encoder : Encoder,
    decoder : Decoder,
    encoder_capacity : UInt32,
    decoder_capacity : UInt32,
    template : Term::Dict

  defcase SocketClient,
    node : D7::Node,
    defn : Harmony::SocketClientDefn,
    encoder : Encoder,
    decoder : Decoder

  defcase HttpServer,
    node : D7::Node,
    defn : Harmony::HttpServerDefn,
    pool : D7::AbsEdge,
    encoder : Encoder,
    decoder : Decoder,
    template : Term::Dict

  defcase WebSocketServer,
    node : D7::Node,
    defn : Harmony::HttpServerDefn,
    link : Harmony::Link,
    pool : D7::AbsEdge,
    encoder : Encoder,
    decoder : Decoder,
    encoder_capacity : UInt32,
    decoder_capacity : UInt32,
    template : Term::Dict

  defcase HttpClient,
    node : D7::Node,
    defn : Harmony::HttpClientDefn,
    encoder : Encoder,
    decoder : Decoder

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

    # When there's no incarnation, this means the server has disappeared for some reason. Assume
    # all its clients have disappeared, too. Clear the pool and update the status to inform
    # the circuit.
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
    unless pool = Rack.pool?(hg, server.pool)
      # (server (@_ _ ⏏) _*)
      return D7.patch(server.node, {1, 2, {:dn, "missing pool"}})
    end

    ctx.goals.add(Harmony::Server.new(server.defn))

    status, incarnation = status_and_incarnation(ctx.world, server.defn)
    if incarnation.nil?
      return D7.patches(
        # (server (@_ _ ⏏) _*)
        D7.patch(server.node, {1, 2, status}),
        # (pool @_ ⏏)
        D7.patch(pool.node, {2, Term[]}),
      )
    end

    # A web socket server is an HTTP server extended with a WebSocket handler.
    # Make the handler, and ensure no conflicts arise.
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

  defrecord ClientDevice, tree : D7::CircuitNode, key : UInt32

  defrecord DeviceIn, key : UInt32, size : UInt32
  defrecord DeviceOut, key : UInt32, size : UInt32, msg : Term?, smart: true
  defrecord DeviceCell, key : UInt32, value : Term

  # :nodoc:
  EDGE_ID = Term.of(:edge, :id)

  # Returns the peer id associated with *device*.
  private def extract?(device : ClientDevice, cls : Harmony::PeerId.class | Harmony::HttpRequestId.class)
    # NOTE: the id cell is the first cell because we generate it this way. So
    # O(N) here is effectively O(1).
    return unless cell = extract?(device, DeviceCell, EDGE_ID)
    return unless reprQ = cell.value.as_s?
    return unless repr = UUID.parse?(reprQ.to(String))

    cls.new(repr)
  end

  # Returns the ingoing message queue associated with *device*.
  private def extract?(device : ClientDevice, cls : DeviceIn.class, edge : Term) : DeviceIn?
    return unless cell = extract?(device, DeviceCell, edge)
    return unless msgs = cell.value.as_itemsonly_d?

    DeviceIn.new(cell.key, msgs.uitemsize)
  end

  # Returns the outgoing message queue associated with *device*.
  private def extract?(device : ClientDevice, cls : DeviceOut.class, edge : Term) : DeviceOut?
    return unless cell = extract?(device, DeviceCell, edge)
    return unless msgs = cell.value.as_itemsonly_d?

    DeviceOut.new(cell.key, msgs.uitemsize, msgs.items.first?)
  end

  # Searches for a nonempty cell at *edge* in *device*.
  private def extract?(device : ClientDevice, cls : DeviceCell.class, edge needle : Term) : DeviceCell?
    tree = device.tree
    candidates = Pf::Kit.stack_array(DeviceCell, 1)

    tree.children.each_with_index(offset: tree.feature.range.begin) do |child, index|
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

  # Yields each client device along with its key in the underlying term in the circuit.
  private def each_client_device(device_tree : D7::CircuitNode, & : ClientDevice ->) : Nil
    device_tree.children.zip(0u32...device_tree.children.size) do |device, device_key|
      next unless device.is_a?(D7::CircuitNode)

      node : Term::Dict = device.feature.node
      next unless node.itemsize >= 1
      next unless node.items.first == Term.of(:device)

      yield ClientDevice.new(device, device_key)
    end
  end

  alias DeviceChange = DeviceAdded | DeviceRemoved | DeviceModified
  alias DeviceModified = DeviceEnqueueOne | DeviceEnqueueMany |
                         DeviceDequeue | DeviceClearCell

  defrecord DeviceAdded, device : Term, brief: true
  defrecord DeviceRemoved, device_key : UInt32, brief: true
  defrecord DeviceEnqueueOne, device_key : UInt32, mailbox_key : UInt32, msg : Term, brief: true
  defrecord DeviceEnqueueMany, device_key : UInt32, mailbox_key : UInt32, batch : Slice(Term), brief: true
  defrecord DeviceDequeue, device_key : UInt32, mailbox_key : UInt32, brief: true
  defrecord DeviceClearCell, device_key : UInt32, cell_key : UInt32, brief: true

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

        case change
        in DeviceEnqueueOne
          mailbox0 = device0[change.mailbox_key, 2]
          mailbox1 = mailbox0.append(change.msg)
          device1 = Term.morph(device0, {change.mailbox_key, 2, mailbox1})
        in DeviceEnqueueMany
          mailbox0 = device0[change.mailbox_key, 2]
          mailbox1 = mailbox0.transaction(&.concat(change.batch))
          device1 = Term.morph(device0, {change.mailbox_key, 2, mailbox1})
        in DeviceDequeue
          mailbox0 = device0[change.mailbox_key, 2]
          mailbox1 = mailbox0.replace(0...1, Term.rep)
          device1 = Term.morph(device0, {change.mailbox_key, 2, mailbox1})
        in DeviceClearCell
          device1 = Term.morph(device0, {change.cell_key, 2, nil})
        end

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

  defrecord Clog
  defrecord Discard

  alias EncodeFailedAction = Clog | Discard
  alias DecodeFailedAction = Discard

  defrecord HttpRequest, term : Term
  defrecord HttpResponse, term : Term

  private def encode(encoder : Encoder, message : Term) : Term::Blob | EncodeFailedAction
    result = Format.encode?(encoder.format, message)

    case encoder.policy
    in .clog?    then result || Clog.new
    in .discard? then result || Discard.new
    end
  end

  private def decode(decoder : Decoder, payload : Term::Blob) : Term | DecodeFailedAction
    result = Format.decode?(decoder.format, payload)

    case decoder.policy
    in .discard? then result || Discard.new
    in .wrap?    then result ? Term.of(:ok, result) : Term.of(:err, "message decode error")
    end
  end

  private def encode(encoder : Encoder, request : HttpRequest) : Term | EncodeFailedAction
    result : Term? = nil

    Term.case(request.term) do
      matchpi %{[_symbol _ body_]} do
        if blob = Format.encode?(encoder.format, body)
          result = Term.morph(request.term, {2, blob})
        end
      end

      # Keep all other requests as-is.
      otherwise do
        result = request.term
      end
    end

    case encoder.policy
    in .clog?    then result || Clog.new
    in .discard? then result || Discard.new
    end
  end

  private def decode(decoder : Decoder, payload : HttpRequest) : Term | DecodeFailedAction
    result : Term? = nil

    Term.case(payload.term) do
      matchpiT %{[_symbol _ bodyQ_blob]} do
        if body = Format.decode?(decoder.format, bodyQ)
          result = Term.morph(payload.term, {2, body})
        end
      end

      # Keep all other requests as-is.
      otherwise do
        result = payload.term
      end
    end

    case decoder.policy
    in .discard? then result || Discard.new
    in .wrap?    then result ? Term.of(:ok, result) : Term.of(:err, "request decode error")
    end
  end

  private def encode(encoder : Encoder, response : HttpResponse) : Term | EncodeFailedAction
    result : Term? = nil

    Term.case(response.term) do
      matchpi %{[_ [attachment body_]]} do
        if blob = Format.encode?(encoder.format, body)
          result = Term.morph(response.term, {1, 1, blob})
        end
      end

      matchpi %{[_ [file _string body_]]} do
        if blob = Format.encode?(encoder.format, body)
          result = Term.morph(response.term, {1, 2, blob})
        end
      end

      matchpi %{[_ body_]} do
        if blob = Format.encode?(encoder.format, body)
          result = Term.morph(response.term, {1, blob})
        end
      end

      # Keep all other responses as-is.
      otherwise do
        result = response.term
      end
    end

    case encoder.policy
    in .clog?    then result || Clog.new
    in .discard? then result || Discard.new
    end
  end

  private def decode(decoder : Decoder, payload : HttpResponse) : Term | DecodeFailedAction
    result : Term? = nil

    Term.case(payload.term) do
      matchpiT %{[_ [attachment bodyQ_blob]]} do
        if body = Format.decode?(decoder.format, bodyQ)
          result = Term.morph(payload.term, {1, 1, body})
        end
      end

      matchpiT %{[_ [file _string bodyQ_blob]]} do
        if body = Format.decode?(decoder.format, bodyQ)
          result = Term.morph(payload.term, {1, 2, body})
        end
      end

      matchpiT %{[_ bodyQ_blob]} do
        if body = Format.decode?(decoder.format, bodyQ)
          result = Term.morph(payload.term, {1, body})
        end
      end

      # Keep all other responses as-is.
      otherwise do
        result = payload.term
      end
    end

    case decoder.policy
    in .discard? then result || Discard.new
    in .wrap?    then result ? Term.of(:ok, result) : Term.of(:err, "request decode error")
    end
  end

  private def sync_ingoing(ctx, server, device, id : Harmony::PeerId, inbox, changes) : Nil
    received = Pf::Kit.stack_array({Harmony::MsgId, Term}, 1)

    ctx.world.each(Harmony::IngoingMessage, endpoint_id: id) do |fact|
      confirmation = Harmony::IngoingReceiveConfirmation.new(fact.endpoint_id, fact.msgid)

      # Initiate confirmation. If confirmation is a fact, this means it's complete.
      unless ctx.world.includes?(confirmation)
        ctx.goals << Harmony::IngoingMessageKeepalive.new(fact.endpoint_id, fact.msgid)
        ctx.goals << confirmation
        next
      end

      case decode_out = decode(server.decoder, fact.payload)
      in Term
        received << {fact.msgid, decode_out}
      in Discard
      end
    end

    # Most often there are no messages.
    return if received.empty?

    # Sometimes there's just one message.
    if row = received.single?
      _, msg = row
      changes << DeviceEnqueueOne.new(device.key, inbox.key, msg)
      return
    end

    # Very rarely there are several messages.
    received.unstable_sort_by! { |msgid, _| msgid.repr }
    batch = received.to_readonly_slice { |(_, msg)| msg }
    changes << DeviceEnqueueMany.new(device.key, inbox.key, batch)
  end

  private def sync_outgoing(ctx, server, device, id : Harmony::PeerId, outbox, changes) : Nil
    return unless msg = outbox.msg?

    case encode_out = encode(server.encoder, msg)
    in Term::Blob
      # They've received it, we can safely dequeue.
      if ctx.world.includes?(Harmony::RemoteReceiveConfirmation.new(id, encode_out))
        changes << DeviceDequeue.new(device.key, outbox.key)
        return
      end

      # Keep willing to send it.
      ctx.goals << Harmony::OutgoingMessage.new(id, encode_out)
    in Clog
    in Discard
      changes << DeviceDequeue.new(device.key, outbox.key)
    end
  end

  private def sync_connected(ctx, server, id : Harmony::ServerId, changes) : Nil
    ctx.world.each(Harmony::RunningPeer, server_id: id) do |fact|
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
        commit << {:cell, server.decoder.edge, Term[]}
        commit << {:cell, server.encoder.edge, Term[]}
        commit.concat(server.template.items)
      end

      changes << DeviceAdded.new(Term.of(instance))
    end
  end

  private def step(ctx : StepContext, hg : D7::Hypergraph, server : SocketServer | WebSocketServer, pool : Pool, status : Term, incarnation : Harmony::ServerId) : D7::Patch?
    _, device_tree = D7.follow(hg.@tree, pool.node.addr)
    return unless device_tree.is_a?(D7::CircuitNode)

    changes = DeviceChangeList.new

    each_client_device(device_tree) do |device|
      next unless peer_id = extract?(device, Harmony::PeerId)

      # Detect device disconnects.
      unless ctx.world.any?(Harmony::RunningPeer, peer_id: peer_id)
        changes << DeviceRemoved.new(device.key)
        next
      end

      inbox = extract?(device, DeviceIn, server.decoder.edge)
      outbox = extract?(device, DeviceOut, server.encoder.edge)

      # The device is closed, it is no longer accepting or sending messages.
      next if inbox.nil? && outbox.nil?

      if inbox
        sync_ingoing(ctx, server, device, peer_id, inbox, changes)
      end

      if outbox
        sync_outgoing(ctx, server, device, peer_id, outbox, changes)
      end

      # If our thresholds allow it, indicate to the other side that the device
      # has spare space for messages.
      pass do
        next if inbox && inbox.size >= server.decoder_capacity
        next if outbox && outbox.size >= server.encoder_capacity

        ctx.goals.add(Harmony::MessageSlot.new(peer_id, server.decoder_capacity))
      end

      ctx.goals << Harmony::PeerKeepalive.new(peer_id)
    end

    sync_connected(ctx, server, incarnation, changes)

    D7.patches(
      # (server (@_ _ ⏏) _*)
      D7.patch(server.node, {1, 2, status}),
      # (pool @_ ⏏)
      D7.patch(pool.node, {2, apply(pool.contents, changes)}),
    )
  end

  # TODO: refactors
  private def step(ctx : StepContext, hg : D7::Hypergraph, server : HttpServer, pool : Pool, status : Term, incarnation : Harmony::ServerId) : D7::Patch?
    _, device_tree = D7.follow(hg.@tree, pool.node.addr)
    return unless device_tree.is_a?(D7::CircuitNode)

    changes = DeviceChangeList.new

    each_client_device(device_tree) do |device|
      next unless request_id = extract?(device, Harmony::HttpRequestId)

      # If this request id doesn't have a corresponding request, this means the request was
      # handled already and we can remove this device.
      unless ctx.world.any?(Harmony::HttpServerRequest, server_id: incarnation, request_id: request_id)
        changes << DeviceRemoved.new(device.key)
        next
      end

      # Wait until a response is available.
      unless response = extract?(device, DeviceCell, server.encoder.edge)
        ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(request_id))
        next
      end

      # Discard response if its encoding is invalid. Since we no longer keep the corresponding
      # request alive, and we're edge-triggered, the request will disappear eventually along
      # with the device.
      case encode_out = encode(server.encoder, HttpResponse.new(response.value))
      in Term
        payload = encode_out
      in Clog
        ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(request_id))
        next
      in Discard
        ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(request_id))
        changes << DeviceClearCell.new(device.key, response.key)
        next
      end

      # If the response does not specify the content type explictily, and Format
      # suggests one, use the suggested content type.
      pass do
        next unless suggested_content_type = Format.content_type?(server.encoder.format)
        next if payload.includes?(:"content-type")

        payload = Term.morph(payload, {:"content-type", suggested_content_type})
      end

      ctx.goals.add(Harmony::HttpServerResponse.new(incarnation, request_id, payload))
    end

    # Add new connections.
    ctx.world.each(Harmony::HttpServerRequest, server_id: incarnation) do |fact|
      # Do not reintroduce requests we've already handled.
      next if fact.request_id.in?(ctx.acknowledged)

      case decode_out = decode(server.decoder, HttpRequest.new(fact.request))
      in Term
        request = decode_out
      in Discard
        next
      end

      ctx.goals.add(Harmony::HttpServerRequestKeepalive.new(fact.request_id))

      instance = Term::Dict.build do |commit|
        commit << :device
        commit << {:cell, {:edge, :id}, fact.request_id.repr}
        commit << {:cell, server.decoder.edge, request}
        commit << {:cell, server.encoder.edge}
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

  # TODO: refactors
  private def step(ctx : StepContext, hg : D7::Hypergraph, client : SocketClient, status : Term, incarnation : Harmony::ClientId) : D7::Patch?
    status_patch = D7.patch(client.node, {2, status})

    # Handle the source.
    source_patch = nil
    pass do
      abs_encoder_edge = hg.resolve(client.node.addr, client.encoder.edge)
      next unless source = Rack.cell?(hg, abs_encoder_edge)
      next unless message = source.value?

      case encode_out = encode(client.encoder, message)
      in Term::Blob
        message = encode_out
      in Clog
        next
      in Discard
        source_patch = D7.patch(source.node, {2, nil})
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
      abs_decoder_edge = hg.resolve(client.node.addr, client.decoder.edge)
      next unless target = Rack.cell?(hg, abs_decoder_edge)

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
        ctx.goals.add(Harmony::MessageSlot.new(incarnation, capacity: 1u32))
        next
      end

      confirmation = Harmony::IngoingReceiveConfirmation.new(ingoing.endpoint_id, ingoing.msgid)

      # Initiate confirmation. If confirmation is a fact, this means it's complete.
      unless ctx.world.includes?(confirmation)
        ctx.goals << confirmation
        next
      end

      ctx.goals.delete(Harmony::IngoingMessageKeepalive.new(ingoing.endpoint_id, ingoing.msgid))

      case decode_out = decode(client.decoder, ingoing.payload)
      in Term
        target_patch = D7.patch(target.node, {2, decode_out})
      in Discard
      end
    end

    D7.patches(
      status_patch,
      source_patch || D7::Patch.new,
      target_patch || D7::Patch.new,
    )
  end

  # TODO: refactors
  private def step(ctx : StepContext, hg : D7::Hypergraph, client : HttpClient, status : Term, incarnation : Harmony::ClientId) : D7::Patch?
    patch = D7.patch(client.node, {2, status})

    pass do
      # If there's no current request, then there's no response to wait or be waiting for.
      abs_encoder_edge = hg.resolve(client.node.addr, client.encoder.edge)
      next unless source = Rack.cell?(hg, abs_encoder_edge)
      next unless request = source.value?

      # If the response cell is occupied, wait until it is not. There is an opportunity
      # for an in-flight request to be aborted if the response cell *becomes* full.
      abs_decoder_edge = hg.resolve(client.node.addr, client.decoder.edge)
      next unless target = Rack.cell?(hg, abs_decoder_edge)
      next unless target.empty?

      case encode_out = encode(client.encoder, HttpRequest.new(request))
      in Term
        request = encode_out
      in Clog
        next
      in Discard
        patch = D7.patches(patch, D7.patch(source.node, {2, nil}))
        next
      end

      unless fact = ctx.world.single?(Harmony::HttpClientResponse, client_id: incarnation, request: request)
        ctx.goals.add(Harmony::HttpClientRequest.new(incarnation, request))
        next
      end

      payload = fact.result.response
      if payload.is_a?(Harmony::HttpResponseError)
        patch = D7.patches(patch, D7.patch(client.node, {2, {:dn, payload.detail}}))
        next
      end

      # payload : Term

      case decode_out = decode(client.decoder, HttpResponse.new(payload))
      in Term
        patch = D7.patches(patch,
          D7.patch(source.node, {2, nil}),
          D7.patch(target.node, {2, decode_out}),
        )
      in Discard
      end
    end

    patch
  end
end
