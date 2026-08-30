# See `rack.[network].format` to learn about the supported formats.
module Ww::Rack::Format
  extend self

  alias Any = Binary | Text
  alias Text = Plaintext | TermJSON | TermJSONSchema | TermML | TermPrettyML

  # TODO: limits, limits, limits!!

  defrecord Binary
  defrecord Plaintext
  defrecord TermJSON
  defrecord TermJSONSchema, schema : Schema::JSON, top : Term
  defrecord TermML
  defrecord TermPrettyML

  # |@ rack.[network]
  #
  # |@summary
  # Groups articles related to the `rack.client` and `rack.server` nodes.

  def format?(schemas : ICache, hg : D7::Hypergraph, node : D7::Node, term : Term) : Any?
    # |@ rack.[network].format
    #
    # |@summary
    # Describes how to (de)serialize terms.
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
      # that case, the string's UTF-8 byte representation is sent (and received).
      # If you want to send *and* receive UTF-8 payloads, consider using `text`.
      #
      # NOTE: For protocols that do not support content-type (TCP, UNIX, WebSockets etc.),
      # the blob's media type will be stripped before the blob is sent.
      matchpi %{binary} do
        Binary.new
      end

      # |@ rack.[network].format
      #
      # |@pattern
      # text
      #
      # |@block
      # UTF-8-encoded string payloads, e.g. `"hello world"`.
      matchpi %{text} do
        Plaintext.new
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
        TermJSON.new
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

        hg.each_member(hg.resolve(node.addr, edge), heads: {Term.of(:schema)}) do |candidate|
          Term.matchpi?(candidate.term, %{[schema (@_ json) schemaQ_*]}) do
            targets << schemas.put_if_absent(schemaQ) { Schema::JSON.new(schemaQ) }
          end
        end

        continue unless schema = targets.single?

        TermJSONSchema.new(schema, top)
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
        TermML.new
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
        TermPrettyML.new
      end

      otherwise { }
    end
  end

  enum Policy
    Discard
    Abort
    Wrap
  end

  def policy?(term : Term) : Policy?
    # |@ rack.[network].format-policy
    #
    # |@summary
    # Determines how adherence to a format is maintained.
    Term.case(term) do
      # |@ rack.[network].format-policy
      #
      # |@pattern
      # discard
      #
      # |@block
      # Messages that were decoded successfully are passed as-is. Messages that
      # were not are discarded without notifying the offending client.
      matchpi %{discard} do
        Policy::Discard
      end

      # |@ rack.[network].format-policy
      #
      # |@pattern
      # abort
      #
      # |@block
      # Messages that were decoded successfully are passed as-is. Messages that
      # were not trigger connection closure.
      matchpi %{abort} do
        Policy::Abort
      end

      # |@ rack.[network].format-policy
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
        Policy::Wrap
      end

      otherwise { }
    end
  end

  def content_type?(format : Binary) : String?
  end

  def content_type?(format : Plaintext) : String?
    "text/plain;charset=UTF-8"
  end

  def content_type?(format : TermJSON | TermJSONSchema) : String?
    "application/json"
  end

  def content_type?(format : TermML | TermPrettyML) : String?
    "application/x-wwml"
  end

  def encode?(format : Binary, term : Term) : Term::Blob?
    return unless term = term.as_blob? || term.as_s?

    case term
    in Term::Blob then term
    in Term::Str  then Term::Blob.new(term.to(String), Term::Blob::Classif.plaintext)
    end
  end

  def encode?(format : Plaintext, term : Term) : Term::Blob?
    return unless str = term.as_s?

    Term::Blob.new(term.to(String))
  end

  # TODO: TermJSONSchema should probably validate input terms as well.
  def encode?(format : TermJSON | TermJSONSchema, term : Term) : Term::Blob?
    Term::Blob.build do |io|
      JSON.build(io) { |json| encode(format, json, term) }
    end
  end

  def encode?(format : TermML, term : Term) : Term::Blob?
    Term::Blob.build { |io| ML.compact(io, term) }
  end

  def encode?(format : TermPrettyML, term : Term) : Term::Blob?
    Term::Blob.build do |io|
      ML.display(io, term, maxwidth: 80)
    end
  end

  private def encode(format, json : JSON::Builder, term : Term) : Nil
    encode(format, json, Term[term])
  end

  private def encode(format, json : JSON::Builder, term : Term::Num) : Nil
    case repr = term.repr
    in Int64, Float32 then json.number(repr)
    in BigRational
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

  def decode?(format : Binary, message : Term::Blob) : Term?
    Term.of(message)
  end

  def decode?(format : Text, message : Term::Blob) : Term?
    return unless message.utf8?

    # FIXME: Converting it to_string here seems fairly expensive. It's a perfectly
    # avoidable allocation.
    decode?(format, message.to_string)
  end

  private def decode?(format : Plaintext, message : String) : Term?
    Term.of(message)
  end

  private def decode?(format : TermJSON, message : String) : Term?
    begin
      Schema::JSON.read(message)
    rescue e : JSON::ParseException
      Log.debug(exception: e) { "error while decoding JSON message" }
    end
  end

  private def decode?(format : TermJSONSchema, message : String) : Term?
    begin
      Schema::JSON.read(format.schema, format.top, message)
    rescue e : JSON::ParseException
      Log.debug(exception: e) { "error while decoding JSON message" }
    end
  end

  private def decode?(format : TermML | TermPrettyML, message : String) : Term?
    begin
      ML.term(message)
    rescue e : ML::SyntaxError
      Log.debug(exception: e) { "error while decoding WwML message" }
    end
  end
end
