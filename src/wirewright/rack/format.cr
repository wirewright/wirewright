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
