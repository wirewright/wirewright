# Implements the HTTP response language. See `http.response`.
module Ww::HttpResponseLanguage
  extend self
  include HttpLanguage

  defrecord Status, code : Int32, description : String?, smart: true

  # 2xx

  # :nodoc:
  SYM_OK = Term.of(:ok)
  # :nodoc:
  SYM_CREATED = Term.of(:created)
  # :nodoc:
  SYM_ACCEPTED = Term.of(:accepted)

  # 3xx

  # :nodoc:
  SYM_MOVED = Term.of(:moved)
  # :nodoc:
  SYM_REDIRECT = Term.of(:redirect)

  # 4xx

  # :nodoc:
  SYM_BAD_REQUEST = Term.of(:"bad-request")
  # :nodoc:
  SYM_FORBIDDEN = Term.of(:forbidden)
  # :nodoc:
  SYM_NOT_FOUND = Term.of(:"not-found")

  # 5xx

  # :nodoc:
  SYM_ERR = Term.of(:err)
  # :nodoc:
  SYM_UNAVAILABLE = Term.of(:unavailable)

  def status_code?(codename : Term) : Int32?
    case codename
    when SYM_OK          then 200
    when SYM_CREATED     then 201
    when SYM_ACCEPTED    then 202
    when SYM_MOVED       then 301
    when SYM_REDIRECT    then 302
    when SYM_BAD_REQUEST then 400
    when SYM_FORBIDDEN   then 403
    when SYM_NOT_FOUND   then 404
    when SYM_ERR         then 500
    when SYM_UNAVAILABLE then 503
    end
  end

  def status_codename?(code : Int32) : Term?
    case code
    when 200 then SYM_OK
    when 201 then SYM_CREATED
    when 202 then SYM_ACCEPTED
    when 301 then SYM_MOVED
    when 302 then SYM_REDIRECT
    when 400 then SYM_BAD_REQUEST
    when 403 then SYM_FORBIDDEN
    when 404 then SYM_NOT_FOUND
    when 500 then SYM_ERR
    when 503 then SYM_UNAVAILABLE
    end
  end

  def encode(status : Status) : Term
    code = status_codename?(status.code) || Term.of(status.code)

    unless description = status.description?
      # ok  201
      return code
    end

    # (ok "Success")  (201 "Success")
    Term.of(code, description)
  end

  def decode?(term : Term, cls : Status.class) : Status?
    # |@ http.response.status
    #
    # |@summary
    # HTTP response status code.
    Term.case(term) do
      # |@ http.response.status
      #
      # |@pattern
      # _symbol
      #
      # |@block
      # A symbolic representation for the most common HTTP status codes.
      #
      # | Symbol        | HTTP status code |
      # | ------------- | ---------------- |
      # | `ok`          | 200              |
      # | `created`     | 201              |
      # | `accepted`    | 202              |
      # | `moved`       | 301              |
      # | `redirect`    | 302              |
      # | `bad-request` | 400              |
      # | `forbidden`   | 403              |
      # | `not-found`   | 404              |
      # | `err`         | 500              |
      # | `unavailable` | 503              |
      #
      # |@example
      # ```wwml
      # not-found
      # ```
      matchpi %{_symbol} do
        return unless code = status_code?(term)

        # NOTE: We have to go through HTTP::Status to get the description, no point
        # duplicating descriptions in Ww code when they're already in the stdlib.
        status = HTTP::Status.new(code)
        Status.new(status.code, status.description)
      end

      # |@ http.response.status
      #
      # |@pattern
      # (%number 100 <= (whole _) <= 999)
      #
      # |@block
      # Numeric representation of HTTP status codes.
      #
      # |@example
      # ```wwml
      # 404
      # ```
      matchpi %{(%number 100 <= (whole _) <= 999)} do
        status = HTTP::Status.new(term.to(Int32))
        Status.new(status.code, status.description)
      end

      # |@ http.response.status
      #
      # |@pattern
      # (code_symbol description_string)
      #
      # |@block
      # Extends a status code with a custom *description*.
      #
      # |@example
      # ```wwml
      # (ok "Fetched an employee")
      # (not-found "Employee does not exist")
      # ```
      matchpi %{(codeQ_symbol description_string)}, description: String do
        return unless code = status_code?(codeQ)

        Status.new(code, description)
      end

      # |@ http.response.status
      #
      # |@pattern
      # (code←(%number 100 <= (whole _) <= 999) description_string)
      #
      # |@block
      # Extends a status code with a custom *description*.
      #
      # |@example
      # ```wwml
      # (200 "Fetched an employee")
      # ```
      matchpi %{(code←(%number 100 <= (whole _) <= 999) description_string)}, code: Int32, description: String do
        Status.new(code, description)
      end

      otherwise { }
    end
  end

  enum SameSite
    None
    Strict
    Lax
  end

  # :nodoc:
  SYM_NONE = Term.of(:none)
  # :nodoc:
  SYM_STRICT = Term.of(:strict)
  # :nodoc:
  SYM_LAX = Term.of(:lax)

  def encode(same_site : SameSite)
    case same_site
    in .none?   then SYM_NONE
    in .strict? then SYM_STRICT
    in .lax?    then SYM_LAX
    end
  end

  def decode?(term : Term, cls : SameSite.class) : SameSite?
    case term
    when SYM_NONE   then SameSite::None
    when SYM_STRICT then SameSite::Strict
    when SYM_LAX    then SameSite::Lax
    end
  end

  defrecord Cookie,
    name : String,
    value : String,
    path : String?,
    domain : String?,
    secure : Bool?,
    http_only : Bool,
    same_site : SameSite?,
    extension : String?,
    max_age : Time::Span?,
    smart: true

  def encode(cookie : Cookie) : Term
    Term.of(:cookie, cookie.name, cookie.value,
      path: cookie.path?,
      domain: cookie.domain?,
      secure: cookie.secure?,
      "http-only": cookie.http_only || nil,
      extension: cookie.extension?,
      max_age: encode(cookie.max_age?),
      "same-site": encode(cookie.same_site?),
    )
  end

  def encode(cookies : Array(Cookie)) : Term
    result = Term::Dict.build do |commit|
      cookies.each do |cookie|
        commit << encode(cookie)
      end
    end

    Term.of(result)
  end

  def decode?(term : Term, cls : Cookie.class) : Cookie?
    # |@ http.response.cookie
    #
    # |@pattern
    # (cookie name_ value_string
    #   ⍊ ⋮path ⋮domain ⋮secure ⋮http-only ⋮same-site ⋮extension ⋮max-age)
    #
    # |@key name
    # The name of the cookie. It can be a symbol or a string.
    #
    # |@key value
    # The value of the cookie. It must be a string; otherwise, the cookie will
    # be ignored.
    #
    # |@key path
    # An optional *string* which indicates the path that must exist in the requested
    # URL for the browser to send the cookie.
    #
    # If omitted, defaults to the path component of the request URL.
    #
    # |@key domain
    # Defines the host to which the cookie will be sent.
    #
    # Only the current domain can be set as the value, or a domain of a higher
    # order, unless it is a public suffix. Setting the domain will make the cookie
    # available to it, as well as to all its subdomains.
    #
    # If omitted, the cookie is returned only to the host that sent them (i.e.,
    # it becomes a "host-only cookie").
    #
    # |@key secure
    # An optional *boolean*. If `true`, indicates that the cookie is sent [in subsequent
    # requests] to the server only when a request is made with the `https:` scheme (except
    # on localhost).
    #
    # |@key http-only
    # An optional *boolean*. If `true`, forbids JavaScript from accessing the cookie.
    #
    # If omitted, defaults to `false`.
    #
    # |@key same-site
    # Controls whether or not a cookie is sent with [subsequent] cross-site requests:
    # that is, requests originating from a different site, including the scheme, from
    # the site that set the cookie.
    #
    # The following values are supported:
    #
    # | Type     | Meaning                                                                                                                                                                                                                                                                                                    |
    # | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
    # | `none`   | Send the cookie with both cross-site and same-site requests. The Secure attribute must also be set when using this value.                                                                                                                                                                                  |
    # | `lax`    | Send the cookie only for requests originating from the same site that set the cookie, and for cross-site requests that meet both of the following criteria:<br>- The request is a top-level navigation.<br>-The request uses a [safe](https://developer.mozilla.org/en-US/docs/Glossary/Safe/HTTP) method. |
    # | `strict` | Send the cookie only for requests originating from the same site that set the cookie.                                                                                                                                                                                                                      |
    #
    # |@key extension
    # An optional *string* containing unparsed attributes. E.g. `Partitioned`, `Priority=High`.
    #
    # |@key max-age http.duration
    # The span of time to wait for until the cookie expires. It is expressed in the duration
    # sublanguage. A zero or negative duration will expire the cookie immediately.
    #
    # |@block
    # Describes a response cookie.
    #
    # This documentation assumes the other side is a browser, but it may not necessarily be
    # a browser. The treatment of pairs above would then depend on the other side.
    #
    # Documentation adapted from: https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Set-Cookie
    #
    # |@example
    # ```wwml
    # (cookie session-id "deadbeef"
    #   secure: true
    #   http-only: true
    #   same-site: strict)
    # ```
    Term.matchpi?(term, %{[cookie nameQ_ value_string]}) do
      return unless name = nameQ.as_sym? || nameQ.as_s?

      Cookie.new(name.to(String), value.to(String),
        path: decode?(term[:path]?, String),
        domain: decode?(term[:domain]?, String),
        secure: decode?(term[:secure]?, Bool),
        http_only: decode?(term[:"http-only"]?, Bool) || false,
        same_site: decode?(term[:"same-site"]?, SameSite),
        extension: decode?(term[:extension]?, String),
        max_age: decode?(term[:"max-age"]?, Time::Span),
      )
    end
  end

  defrecord InlineBody, content : Term::Blob

  # |@ http.response.body
  #
  # |@pattern
  # _string
  # _blob
  #
  # |@block
  # Describes a simple (Content-Disposition: inline) string or blob body
  # of an HTTP response.
  #
  # A string is converted into a blob with the media type `text/plain;charset=UTF-8`.
  # This media type can be overridden by setting an explicit content-type
  # in `http.response`.

  def decode?(term : Term, cls : InlineBody.class) : InlineBody?
    return unless content = term.as_s? || term.as_blob?

    case content
    in Term::Blob then InlineBody.new(content)
    in Term::Str  then InlineBody.new(Term::Blob.new(content))
    end
  end

  defrecord AttachmentBody, filename : String?, content : Term::Blob, smart: true

  def decode?(term : Term, cls : AttachmentBody.class) : AttachmentBody?
    Term.case(term) do
      # |@ http.response.body
      #
      # |@pattern
      # [file filename_string body_]
      #
      # |@key filename
      # For file attachments, specifies the name of the file.
      #
      # |@key body
      # A string or a blob. A string is converted to a blob with the media type
      # `text/plain;charset=UTF-8`.
      #
      # The media type of the blob is preserved, and is used in the response as the value
      # of the Content-Type header.
      #
      # |@block
      # Describes a file attachment body of an HTTP response. If there's a browser
      # on the other side, it will usually prompt the user to download the file,
      # with *filename* set as the default filename.
      #
      # |@example
      # ```wwml
      # (file "avatar.png"
      #   ⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
      #    00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
      #    2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc⟭)
      # ```
      matchpi %{[file filename_string bodyQ_]}, filename: String do
        return unless body = decode?(bodyQ, InlineBody)

        AttachmentBody.new(filename, body.content)
      end

      # |@ http.response.body
      #
      # |@pattern
      # [attachment body_]
      #
      # |@key body
      # A string or a blob. A string is converted to a blob with the media type
      # `text/plain;charset=UTF-8`.
      #
      # The media type of the blob is preserved, and is used in the response as the value
      # of the Content-Type header.
      #
      # |@block
      # Describes a nameless attachment. This is basically the same as a `file`
      # body but without a filename. If there's a browser on the other side,
      # it will usually prompt the user to download the attachment.
      #
      # |@example
      # ```wwml
      # (attachment
      #   ⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
      #    00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
      #    2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc⟭)
      # ```
      matchpi %{[attachment bodyQ_]} do
        return unless body = decode?(bodyQ, InlineBody)

        filename = nil
        AttachmentBody.new(filename, body.content)
      end

      otherwise { }
    end
  end

  alias Body = InlineBody | AttachmentBody

  def decode?(term : Term, cls : Body.class) : Body?
    decode?(term, InlineBody) || decode?(term, AttachmentBody)
  end

  private def write_filename_ascii(io, filename : String, *, fallback : Char) : Nil
    assert fallback.ascii?

    filename.each_char do |chr|
      io << (chr.ascii? ? chr : fallback)
    end
  end

  # Reference: https://www.rfc-editor.org/info/rfc2231/
  #
  # ```text
  # extended-initial-value := [charset] "'" [language] "'" extended-other-values
  # extended-other-values := *(ext-octet / attribute-char)
  # ext-octet := "%" 2(DIGIT / "A" / "B" / "C" / "D" / "E" / "F")
  # attribute-char := <any (US-ASCII) CHAR except SPACE, CTLs, "*", "'", "%", or tspecials>
  # ```
  private def write_filename_rfc2231(io, filename : String) : Nil
    filename.each_char do |chr|
      if chr.ascii? && !(chr.in?(' ', '*', '\'', '%') || chr.control? || chr.tspecial?)
        io << chr
      else
        chr.each_byte do |byte|
          io << '%'
          byte.to_s(io, base: 16, precision: 2, upcase: true)
        end
      end
    end
  end

  # Reference: https://github.com/crystal-lang/crystal/blob/57cf7da5094db6c5d3c058c6d054a757b5ced19e/src/mime/media_type.cr#L447
  private def parse_filename_rfc2231?(seln : Pf::StringSeln) : String?
    charset, quote, seln = seln.partition('\'')
    return if quote.empty?

    language, quote, seln = seln.partition('\'')
    return if quote.empty?

    io = IO::Memory.new
    io.set_encoding(charset.to_s.downcase)

    while chr = seln.first_char?
      # q⏏ux %⏏
      seln = seln.rest

      unless chr == '%'
        # q⏏ux
        io << chr
        next
      end

      # %⏏

      return if seln.empty?

      digit0, seln = seln.first_and_rest
      digit0 = digit0.chr.hexdigit?
      return if digit0.nil?

      digit1, seln = seln.first_and_rest
      digit1 = digit1.chr.hexdigit?
      return if digit1.nil?

      # %20⏏
      octet = (digit0.to_u8 << 4) | digit1.to_u8
      io.write_byte(octet)
    end

    io.rewind
    io.gets_to_end
  end

  # Input is e.g. UTF-8''hello%20world
  private def parse_filename_rfc2231?(source : String) : String?
    parse_filename_rfc2231?(Pf::StringSeln.new(source))
  end

  # Reference: https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Content-Disposition
  private def content_disposition(attachment : AttachmentBody) : String
    String.build do |io|
      io << "attachment"
      next unless filename = attachment.filename?

      # filename="..." is the older property, it requires ASCII.
      io << "; filename=\""
      write_filename_ascii(io, filename, fallback: '?')
      io << '"'

      # filename *= [charset] "'" [language] "'" extended-other-values
      io << "; filename*=UTF-8''"
      write_filename_rfc2231(io, filename)
    end
  end

  defrecord ContentDisposition, filename : String?, smart: true

  # Reference: https://github.com/crystal-lang/crystal/blob/57cf7da5094db6c5d3c058c6d054a757b5ced19e/src/http/formdata.cr#L133
  #
  # Note that response content disposition is different from form content disposition.
  private def parse_response_content_disposition?(disposition : String) : ContentDisposition?
    parts = disposition.split(';')

    return unless type = parts.shift?
    return unless type.strip.in?("inline", "attachment")

    filename_ascii = nil
    filename_rfc2231 = nil

    parts.each do |part|
      key, _, value = part.partition('=')

      key = key.strip
      value = value.strip
      if value[0] == '"'
        value = HTTP.dequote_string(value[1...-1])
      end

      case key
      when "filename"
        filename_ascii = value
      when "filename*"
        filename_rfc2231 = parse_filename_rfc2231?(value)
      end
    end

    filename = filename_rfc2231 || filename_ascii
    ContentDisposition.new(filename)
  end

  # Translates an existing client *response* into the response language.
  def encode(response : HTTP::Client::Response) : Term
    # If the message is some kind of custom message, set it as status_description,
    # otherwise, omit it because it is (arguably) redundant.
    unless response.status.description == response.status_message
      status_description = response.status_message
    end

    status = encode(Status.new(response.status_code, status_description))

    headers = encode(response.headers).as_d

    cookies = response.cookies.map do |cookie|
      case cookie.samesite
      in Nil
      in .none?   then same_site = SameSite::None
      in .strict? then same_site = SameSite::Strict
      in .lax?    then same_site = SameSite::Lax
      end

      Cookie.new(
        cookie.name, cookie.value, cookie.path, cookie.domain,
        cookie.secure, cookie.http_only, same_site, cookie.extension,
        cookie.max_age,
      )
    end

    body = nil # Missing
    pass do
      classif = Term::Blob::Classif.of(pp! response.mime_type)

      if src = response.body_io?
        body = Term::Blob.build(classif: classif) { |dst| IO.copy(src, dst) }
      elsif src = response.body?
        body = Term::Blob.new(src, classif)
      else
        next # Missing body
      end

      # Another way to have a missing body.
      if body.empty? && classif.nil?
        body = nil
        next
      end

      # Process Content-Disposition.
      pass do
        next unless content_disposition_header = response.headers["Content-Disposition"]?
        next unless content_disposition = parse_response_content_disposition?(content_disposition_header)

        # If we can handle Content-Disposition, omit it from headers. Otherwise, leave
        # it in headers to let the user handle it instead.
        headers = headers.without(:"content-disposition")

        if filename = content_disposition.filename?
          body = Term.of(:file, filename, body)
        else
          body = Term.of(:attachment, body)
        end
      end
    end

    result = headers.transaction do |commit|
      commit << status << body
      if cookies.present?
        commit.with(:cookies, encode(cookies))
      end
    end

    Term.of(result)
  end

  # Configures a server *response* according to the given response language *term*.
  def decode(term : Term, *, into response : HTTP::Server::Response) : Nil
    # |@ http.response
    #
    # |@pattern
    # (status_ ¦ headers_ cookies⋮ ())
    # (status_ body_ ¦ headers_ cookies⋮ ())
    #
    # |@key status http.response.status
    # The status code of the response.
    #
    # |@key body http.response.body
    # Optionally, the body of the response.
    #
    # |@key headers http.headers
    # Custom headers or header overrides sent with the response.
    #
    # |@key cookies http.response.cookies
    # A *list* of cookies sent with the response. Only the itemspart is taken
    # into account.
    #
    # |@block
    # Describes an HTTP response.
    #
    # |@example
    # ```wwml
    # (ok "<p>Hello World!</p>"
    #   content-type: "text/html"
    #   cookies:
    #     ((cookie name "Alice")
    #      (cookie session-id "1ea962e07b" max-age: (1 m))))
    # ```
    Term.matchpi?(term, <<-WWML) do
    (statusQ_ _? ¦ headersQ_ cookies: (%optional () cookiesQ_dict))
    WWML
      return unless status = decode?(statusQ, Status)

      headers = decode(headersQ.as_d, HTTP::Headers)

      if bodyQ = term[1]?
        body = decode?(bodyQ, Body)
      end

      cookies = cookiesQ.items.compact_map do |cookie|
        decode?(cookie, Cookie)
      end

      # COMMIT!

      # Crystal requires us to set status and headers (including cookies) before
      # writing the body.

      response.status_code = status.code
      if description = status.description?
        response.status_message = description
      end

      response.headers.merge!(headers)

      cookies.each do |cookie|
        case cookie.same_site?
        in Nil
        in .none?   then same_site = HTTP::Cookie::SameSite::None
        in .strict? then same_site = HTTP::Cookie::SameSite::Strict
        in .lax?    then same_site = HTTP::Cookie::SameSite::Lax
        end

        begin
          # We just ignore Expires and set Max-Age. Maintaining one property instead
          # of two is better. See e.g.: https://mrcoles.com/blog/cookies-max-age-vs-expires/,
          # this recommends to just use Max-Age. Browsers prefer Max-Age as well. Max-Age
          # is relative, too, so it's more well-behaved wrt. timezones and other kinds
          # of time complexity.
          expires = nil

          http_cookie = HTTP::Cookie.new(
            cookie.name,
            cookie.value,
            cookie.path?,
            expires,
            cookie.domain?,
            cookie.secure?,
            cookie.http_only,
            same_site,
            cookie.extension?,
            cookie.max_age?,
          )
          response.cookies << http_cookie
        rescue e : ArgumentError | IO::Error
          Log.error(exception: e) { "error while creating a cookie" }
        end
      end

      return unless body

      pass do
        # In e.g. `(get "hello world" content-type: "text/html")`, we want to override
        # the content type of the body (`text/plain;charset=UTF-8`) with `text/html`.
        # Therefore, only use the body's type if there is no existing type (from headers).
        next unless response.content_type.nil?
        next unless classif = body.content.classif?
        response.content_type = classif.to_s
      end

      case body
      in InlineBody
      in AttachmentBody
        response.headers["Content-Disposition"] = content_disposition(body)
      end

      # Finally, write the body.
      IO.copy(src: body.content.to_io, dst: response)
    end
  end
end
