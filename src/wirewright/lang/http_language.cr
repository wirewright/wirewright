# |@ http
#
# |@summary
# A collection of languages related to HTTP.

# Utilities and encode/decode functions shared by `HttpRequestLanguage` and `HttpResponseLanguage`.
module Ww::HttpLanguage
  extend self

  # Convenience overload to avoid having to `#try` and so on on nilable types
  # all the time.
  def encode(object : Nil) : Nil
  end

  # :ditto:
  def decode?(term : Nil, cls) : Nil
  end

  def decode?(term : Term, cls : Bool.class) : Bool?
    term.as_b?.try(&.true?)
  end

  def decode?(term : Term, cls : String.class) : String?
    term.as_s?.try(&.to(String))
  end

  # |@ http.headers
  #
  # |@pattern
  # _dict
  #
  # |@block
  # The headers dictionary consists of symbol or string keys. When Wirewright
  # describes an ingoing HTTP request or response, it uses exclusively symbol
  # keys. You can use string keys when describing HTTP requests and responses
  # inside Wirewright.
  #
  # We specify headers in lowercase. For example, `Content-Type` becomes
  # `content-type`. Wirewright translates headers into their conventional
  # case automatically when constructing an HTTP request or response from
  # its description. For example:
  #
  # ```text
  # content-type     -> Content-Type
  # content-id       -> Content-ID
  # x-websocket-key  -> X-WebSocket-Key
  # sec-ch-dpr       -> Sec-CH-DPR
  # ```
  #
  # A header's value is expected to be a string. A header can have zero or more
  # values. When a header has zero values, it is omitted. The value of a header
  # with multiple values is a comma-separated list.
  #
  # |@example
  # ```wwml
  # {content-type: "text/html",
  #  ;; Multiple values:
  #  x-user-likes: ("apples" "bananas"),
  #  ;; Zero values, this header will be omitted:
  #  x-user-dislikes: ()}
  # ```
  #
  # Note that the above example presents headers as a distinct dictionary. Most
  # often, however, you'd see them in the pairspart of requests (`http.request`)
  # and responses (`http.response`):
  #
  # ```wwml
  # ;; Example request with headers:
  # (post "/"
  #    "<p>Hello World</p>"
  #   content-type: "text/html"
  #   x-user-dislikes: ()
  #   x-user-likes: ("apples" "bananas"))
  #
  # ;; Example response with headers:
  # (ok content-type: "text/html"
  #     x-user-dislikes: ()
  #     x-user-likes: ("apples" "bananas")
  #   "<p>Hello World</p>")
  # ```

  def encode(headers : HTTP::Headers) : Term
    result = Term::Dict.build do |commit|
      headers.each do |name, values|
        name = HttpHeader.normalize(name)
        next if HttpHeader.omit?(name)

        key = Term::Sym.new(name.@repr)
        commit.with(key, values.single? || values)
      end
    end

    Term.of(result)
  end

  def decode(dict : Term::Dict, cls : HTTP::Headers.class) : HTTP::Headers
    headers = HTTP::Headers.new

    dict.each_entry do |key, value|
      next unless key = key.as_sym? || key.as_s?

      name = HttpHeader.normalize(key.to(String))

      Term.case(value) do
        matchpi %{_string} do
          headers[HttpHeader.present(name)] = value.to(String)
        end

        matchpi %{(_string*)} do
          headers[HttpHeader.present(name)] = value.items.map(&.to(String))
        end

        otherwise { }
      end
    end

    headers
  end
end

# HTTP header utilities for `HttpRequestLanguage` and `HttpResponseLanguage`.
module Ww::HttpLanguage::HttpHeader
  extend self

  # Represents a normalized HTTP header name.
  #
  # `@repr : String` is the underlying header name.
  #
  # We show *normalized* HTTP headers to users.
  #
  # For example, `Content-Type` normalizes to `content-type` (our normalization
  # function is currently `String#downcase`). HTTP headers are case-insensitive,
  # and this is our way to "simulate" that.
  #
  # The inverse of normalization is presentation (see `present`)>
  struct Normal
    def initialize(@repr : String)
    end
  end

  def normalize(header : String) : Normal
    Normal.new(header.downcase)
  end

  PRESENTATION = {
    # Acronyms
    "id":   "ID",
    "ip":   "IP",
    "ua":   "UA",
    "api":  "API",
    "url":  "URL",
    "uri":  "URI",
    "md5":  "MD5",
    "sha":  "SHA",
    "tls":  "TLS",
    "http": "HTTP",
    # Mixed case
    "etag":      "ETag",
    "websocket": "WebSocket",
    "ipsec":     "IPsec",
    "oauth":     "OAuth",
    "openapi":   "OpenAPI",
    "graphql":   "GraphQL",
    # Punctuation
    "utf8":   "UTF-8",
    "utf16":  "UTF-16",
    "sha1":   "SHA-1",
    "sha256": "SHA-256",
    "h2":     "H2",
    "h3":     "H3",
    "x509":   "X.509",
  }

  # Pretty-prints a normalized *header* using the "traditional" HTTP header case.
  #
  # For example, `content-type` becomes `Content-Type`.
  #
  # `PRESENTATION` is a database of capitalizations / exceptions to the general
  # capitalization rule.
  def present(header : Normal) : String
    String.build do |io|
      seln = Pf::StringSeln.new(header.@repr)
      loop do
        segment, sep, seln = seln.partition('-')
        break if segment.empty? && sep.empty?

        if presentation = PRESENTATION[segment.to_s]? # ?!
          io << presentation << sep
          next
        end

        first, rest = segment.first_and_rest
        if chr = first.first_char?
          io << chr.upcase
        end
        io << rest << sep
      end
    end
  end

  OMITTED_HEADER_SET = Set{
    # Cookies are handled as `[cookie _ _]`.
    "cookie",
    "set-cookie",
    # These are confined to the runtime internals, users are not expected to care
    # about these headers.
    "connection",
    "content-length",
    "accept-encoding",
    "transfer-encoding",
    # Content type can be determined from the body of the request or response,
    # or from the form part (via e.g. blob's mime type or content-type-dependent
    # conversions).
    "content-type",
  }

  # Returns `true` if *header* should be omitted from requests and responses.
  #
  # See `OMITTED_HEADER_SET`.
  def omit?(header : Normal) : Bool
    header.@repr.in?(OMITTED_HEADER_SET)
  end
end
