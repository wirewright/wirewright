# Implements the HTTP request language. See `http.request`.
module Ww::HttpRequestLanguage
  extend self
  include HttpLanguage

  defrecord Resource, path : String, params : URI::Params

  def encode(resource : Resource) : Term
    result = Term::Dict.build do |commit|
      commit << resource.path

      resource.params.each do |key, value1|
        key = Term::Sym.new(key)

        # ?x=100           x: "100"
        # ?x=100&x=200     x: ("100" "200")
        if value0 = commit[key]?
          if list = value0.as_d?
            commit.with(key, list.append(value1))
          else
            commit.with(key, Term[value0, value1])
          end
          next
        end

        commit.with(key, value1)
      end
    end

    Term.of(result)
  end

  def decode?(term : Term, cls : Resource.class) : Resource?
    # |@ http.request.resource
    #
    # |@summary
    # Description of an HTTP resource.
    Term.case(term) do
      # |@ http.request.resource
      #
      # |@pattern
      # _string
      #
      # |@block
      # A shorthand for a resource without params.
      #
      # NOTE: When Wirewright describes an ingoing HTTP requests, it will *never* use
      # the shorthand form. It will always spell it out. For example, if you hit `/`,
      # it will spell it out as `("/")`. This is done for consistency, so that you
      # don't have to special-case parameter-less resources and potentially
      # duplicate logic.
      #
      # |@example
      # In the following example, `"/"` is the resource.
      #
      # ```wwml
      # (get "/")
      # ```
      #
      # Here is the fully spelled out version for comparison:
      #
      # ```wwml
      # (get ("/"))
      # ```
      matchpi %{_string} do
        path = term.to(String)
        Resource.new(path, URI::Params.new)
      end

      # |@ http.request.resource
      #
      # |@pattern
      # (path_string ¦ params_)
      #
      # |@key path
      # The path part of the resource, e.g. `"/"` or `/posts/new`.
      #
      # |@key params
      # The parameters of the resource. These correspond to URI parameters.
      #
      # |@block
      # A fully spelled out resource description. It includes the URI *path* and *params*.
      #
      # |@example
      # ```wwml
      # (get ("/posts" query: "Article 1"))
      # ```
      #
      # Wirewright translates the resource description `(get ("/posts" query: "Article 1"))`
      # into `/posts?query=Article%201`.
      #
      # A parameter can have zero or more values. Parameters with zero values are omitted
      # from the URI.
      #
      # ```wwml
      # (get
      #   ("/posts"
      #    type: "article"
      #    tag: ("computing" "programming" "fun")
      #    category: ()))
      # ```
      #
      # The resource above translates into the following URI:
      #
      # ```text
      # /posts?type=article&tag=computing&tag=programming&tag=fun
      # ```
      #
      # Notice that the `category` parameter was omitted.
      matchpi %{(path_string ¦ paramsQ_)}, path: String do
        params = URI::Params.new

        paramsQ.each_entry do |key, value|
          next unless key.type.symbol? || key.type.string?

          name = key.to(String)

          Term.case(value) do
            matchpi %{_string} do
              params.add(name, value.to(String))
            end

            matchpi %{(_string*)} do
              value.items.each do |item|
                params.add(name, item.to(String))
              end
            end

            otherwise { }
          end
        end

        Resource.new(path, params)
      end

      otherwise { }
    end
  end

  # |@ http.request.cookies
  #
  # |@pattern
  # _dict
  #
  # |@summary
  # The request cookie dictionary.
  #
  # |@block
  # The request cookie dictionary contains cookies supplied with the request.
  #
  # Keys can be strings or symbols. Note that only symbols are used for *ingoing*
  # requests; that is, when Wirewright parses an HTTP request, it converts cookie
  # names to symbols. But you can use string cookie names in outgoing requests if
  # you want to (e.g. to concatenate things).
  #
  # |@example
  # ```wwml
  # (get "/" cookies: {"USERNAME": "Alice"})
  # ```
  #
  # If you were to receive this on the server-side, you'd get:
  #
  # ```wwml
  # (get "/" cookies: {USERNAME: "Alice"})
  # ```
  #
  # Note how the string `"USERNAME"` turned into the symbol `USERNAME`.

  # In HTTP requests, a cookie is simply a key-value pair. For some reason, Crystal represents
  # both request and response cookies with the same HTTP::Cookie type, thus, mixing Cookie
  # and Set-Cookie. We don't do this.
  #
  # Reference: https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Cookie
  def encode(cookies : HTTP::Cookies) : Term
    result = Term::Dict.build do |commit|
      cookies.each do |cookie|
        commit.with(Term::Sym.new(cookie.name), cookie.value)
      end
    end

    Term.of(result)
  end

  def decode(dict : Term::Dict, cls : HTTP::Cookies.class) : HTTP::Cookies
    cookies = HTTP::Cookies.new

    dict.each_entry do |key, value|
      next unless key = key.as_sym? || key.as_s?
      next unless value = value.as_s?

      cookies[key.to(String)] = value.to(String)
    end

    cookies
  end

  defrecord URLEncodedForm, fields : Hash(String, Array(String))
  defrecord MultipartForm, fields : Hash(String, Array(MultipartField))

  alias MultipartField = MultipartData | MultipartFile

  defrecord MultipartData, data : Term::Blob, headers : HTTP::Headers
  defrecord MultipartFile, data : Term::Blob, headers : HTTP::Headers, filename : String

  # |@ http.request.form
  #
  # |@summary
  # Description of an HTTP form.

  # |@ http.request.form
  #
  # |@pattern
  # _dict
  #
  # |@block
  # A URL-encoded form is represented by a dictionary with string values.
  # Additionally, you can use the following types inside Wirewright.
  #
  # | Type       | Meaning                                                           |
  # | ---------- | ----------------------------------------------------------------- |
  # | `_number`  | Converts the number to a string. E.g. `42` is sent as `"42"`.     |
  # | `_boolean` | `true` is converted to the string `"on"`, and `false` to `"off"`. |
  #
  # These types are never produced when parsing an HTTP form, but you are still
  # allowed to use them as shorthands for the corresponding stringification.
  #
  # Symbol and string keys in the form dict are allowed. Entries with an invalid key,
  # value, or both are ignored. Note that when describing an ingoing HTTP request,
  # Wirewright uses exclusively symbols for keys (like in `http.request.cookies`).
  #
  # A key in the dict corresponds to a *name* in an HTTP form.
  #
  # Note that Wirewright will switch from a URL-encoded form to a multipart form
  # automatically if it notices a `[file _*]` or `[field _*]` value.
  #
  # |@example
  # In the example below, the request's body will be represented as an HTTP URL-
  # encoded form.
  #
  # ```wwml
  # ((post form) "/"
  #   {name: "Jane", age: 25})
  # ```
  #
  # Keys can have zero or more values. Keys with zero values are omitted.
  #
  # ```wwml
  # ((post form) "/"
  #   {name: "Jane",
  #    age: 25,
  #    ;; `likes` has multiple values:
  #    likes: ("Cheese" "Apples" "Carrots"),
  #    ;; `dislikes` will be omitted:
  #    dislikes: ()})
  # ```

  def encode(form : URLEncodedForm) : Term
    result = Term::Dict.build do |commit|
      form.fields.each do |name, bucket|
        commit.with(Term::Sym.new(name), bucket.single? || bucket)
      end
    end

    Term.of(result)
  end

  private def fieldify?(term : Term) : String?
    Term.case(term) do
      matchpi %{_string} do
        term.to(String)
      end

      matchpi %{_number} do
        ML.compact(term)
      end

      matchpi %{true} do
        "on"
      end

      matchpi %{false} do
        "off"
      end

      otherwise { }
    end
  end

  def decode?(term : Term, cls : URLEncodedForm.class) : URLEncodedForm?
    return unless dict = term.as_d?

    fields = {} of String => Array(String)

    dict.each_entry do |key, value|
      next unless key = key.as_sym?

      name = key.to(String)

      Term.case(value) do
        matchpi %{[file _*]}, %{[field _*]} do
          # This is a multipart form.
          return
        end

        matchpi %{(_*)} do
          fields[name] = value.items.compact_map { |item| fieldify?(item) }
        end

        otherwise do
          next unless field = fieldify?(value)

          fields[name] = [field]
        end
      end
    end

    URLEncodedForm.new(fields)
  end

  # |@ http.request.form
  #
  # |@pattern
  # _dict
  #
  # |@block
  # *Multipart forms* are more complex forms.
  #
  # Multipart forms are marked with `multipart: true` when Wirewright describes
  # an ingoing HTTP request. Although you are not required to mark forms with
  # `multipart: true` when sending requests from inside Wirewright, you are advised
  # to do so because otherwise there's an ambiguity between the type of form
  # Wirewright will use.
  #
  # |@example
  # ```wwml
  # ((post form) "/employees/create"
  #   {name: (field "John Doe"),
  #    age: (field 25),
  #    avatar:
  #      (file "avatar.png"
  #        ⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
  #         00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
  #         2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc⟭)})
  # ```
  #
  # As in URL-encoded forms, an entry can have zero or more values:
  #
  # ```wwml
  # ((post form) "/employees/create"
  #   {name: (field "John Doe"),
  #    likes:
  #      ((field "Susan Doe")
  #       (field "Jane Doe"))})
  # ```
  #
  # `field` and `file` accept headers. See `http.headers` for more info.
  #
  # ```wwml
  # ((post form) "/articles/create"
  #   {name: (field "Lorem ipsum"),
  #    body:
  #      (field content-type: "text/html"
  #        "<p>Lorem ipsum dolor sit amet</p>"),
  #    tags:
  #      ((field "programming")
  #       (field "computers")),
  #    avatar:
  #      (file "avatar.png"
  #        ⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
  #         00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
  #         2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc⟭
  #       content-type: "image/png")})
  # ```
  #
  # `field` and `file` expect blobs for content. The blob's media type is used
  # as the content-type unless it is overridden by the corresponding headers.
  #
  # As a shorthand, you can use a string instead of a blob, like we do in
  # examples above. Strings are converted to blobs under the hood, with the media
  # type (and thus, content-type) of `text/plain;charset=UTF-8`.
  #
  # It is also possible to use a string, but override the media type with a custom
  # one, like we do above in `body: _`. The string initially proposes
  # `text/plain;charset=UTF-8`, but it is later overridden by the explicit
  # `content-type: "text/html"`.
  #
  # Numbers and booleans can be used instead of strings in `field`, exactly like
  # in URL-encoded forms.
  #
  # If a `field` does not have headers, it can be omitted and its content can be
  # used directly. For example, the above can be rewritten as:
  #
  # ```wwml
  # ((post form) "/articles/create"
  #   {name: "Lorem ipsum",
  #    body:
  #      (field content-type: "text/html"
  #        "<p>Lorem ipsum dolor sit amet</p>"),
  #    tags: ("programming" "computers"),
  #    avatar:
  #      (file "avatar.png"
  #        ⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
  #         00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
  #         2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc⟭
  #       content-type: "image/png")})
  # ```
  #
  # Notice how non-informative `field`s have disappeared.
  #
  # NOTE: You should not upload big files through this, at least not right now.
  # Blobs are stored entirely in RAM, so if someone uploads a 10GB file, you'd
  # need 10GB of RAM (but probably more due to control overhead). We are working
  # on providing a better way to upload large files. The recommendation right now
  # is to either chunk it, or use something like S3 and pass a reference (URL) to
  # the file instead. The latter would arguably be the option you end up with anyway
  # in practice, so large file uploads are currently not a priority for Wirewright.

  def encode(form : MultipartForm) : Term
    result = Term::Dict.build do |commit|
      commit.with(:multipart, true)

      form.fields.each do |name, bucket|
        key = Term::Sym.new(name)

        if field = bucket.single?
          commit.with(key, encode(field))
          next
        end

        commit.with(key, bucket.map { |field| encode(field) })
      end
    end

    Term.of(result)
  end

  def decode?(term : Term, cls : MultipartForm.class) : MultipartForm?
    return unless dict = term.as_d?

    fields = {} of String => Array(MultipartField)

    dict.each_entry do |key, value|
      next unless key = key.as_sym?

      name = key.to(String)

      Term.case(value) do
        matchpi %{[file _*]} do
          next unless field = decode?(value, MultipartFile)

          fields[name] = [field] of MultipartField
        end

        matchpi %{_dict} do
          # Go to MultipartData if it's something like (field "Hello").
          head = value.items.first?
          continue if head && head.type.symbol?

          fields[name] = value.items.compact_map do |item|
            decode?(item, MultipartData) || decode?(item, MultipartFile)
          end
        end

        otherwise do
          next unless field = decode?(value, MultipartData)

          fields[name] = [field] of MultipartField
        end
      end
    end

    MultipartForm.new(fields)
  end

  def encode(field : MultipartData)
    headers = encode(field.headers).as_d

    # We are the handling of Content-Disposition.
    headers = headers.without(:"content-disposition")

    result = headers.transaction do |commit|
      commit << :field << field.data
    end

    Term.of(result)
  end

  def decode?(term : Term, cls : MultipartData.class) : MultipartData?
    Term.case(term) do
      matchpi %{_blob} do
        MultipartData.new(term.as_blob, HTTP::Headers.new)
      end

      matchpiT %{(field content_blob ¦ headersQ_)} do
        headers = decode(headersQ.as_d, HTTP::Headers)
        MultipartData.new(content, headers)
      end

      matchpi %{(field contentQ_ ¦ headersQ_)} do
        next unless content = fieldify?(contentQ)

        classif = Term::Blob::Classif.plaintext
        blob = Term::Blob.new(content, classif)
        headers = decode(headersQ.as_d, HTTP::Headers)
        headers["Content-Type"] ||= classif.to_s

        MultipartData.new(blob, headers)
      end

      otherwise do
        next unless content = fieldify?(term)

        classif = Term::Blob::Classif.plaintext
        blob = Term::Blob.new(content, classif)
        headers = HTTP::Headers{"Content-Type" => classif.to_s}

        MultipartData.new(blob, headers)
      end
    end
  end

  def encode(field : MultipartFile)
    headers = encode(field.headers).as_d

    # We are the handling of Content-Disposition.
    headers = headers.without(:"content-disposition")

    result = headers.transaction do |commit|
      commit << :file << field.filename << field.data
    end

    Term.of(result)
  end

  def decode?(term : Term, cls : MultipartFile.class) : MultipartFile?
    Term.matchpi?(term, %{(file filename_string content←(%any° _blob _string) ¦ headersQ_)}) do |content|
      headers = decode(headersQ.as_d, HTTP::Headers)
      content = content.as_blob? || Term::Blob.new(content.as_s)
      if classif = content.classif?
        headers["Content-Type"] ||= classif.to_s
      end

      MultipartFile.new(content, headers, filename.to(String))
    end
  end

  def encode(request : HTTP::Request) : Term
    method = Term::Sym.new(request.method.downcase)
    resource = Resource.new(request.path, request.query_params)
    headers = encode(request.headers).as_d

    content = nil

    pass do
      content_type = request.headers["Content-Type"]?

      # Handle Content-Type: application/x-www-form-urlencoded.
      if form_params = request.form_params?
        fields = {} of String => Array(String)

        form_params.each do |name, value|
          bucket = fields.put_if_absent(name) { [] of String }
          bucket << value
        end

        content = URLEncodedForm.new(fields)
        next
      end

      # Handle Content-Type: multipart/form-data.
      if boundary = content_type.try { |header| MIME::Multipart.parse_boundary(header) }
        body = request.body || raise HTTP::FormData::Error.new("Cannot extract form-data from HTTP request: body is empty")

        fields = {} of String => Array(MultipartField)

        HTTP::FormData.parse(body, boundary) do |part|
          classif = pass do
            next unless content_type = part.headers["Content-Type"]?
            next unless content_type = MIME::MediaType.parse?(content_type)

            Term::Blob::Classif.of(content_type)
          end

          data = Term::Blob.build(classif: classif) do |io|
            IO.copy(src: part.body, dst: io)
          end

          if filename = part.filename
            value = MultipartFile.new(data, part.headers, filename)
          else
            value = MultipartData.new(data, part.headers)
          end

          bucket = fields.put_if_absent(part.name) { [] of MultipartField }
          bucket << value
        end

        content = MultipartForm.new(fields)
        next
      end

      next unless body = request.body

      # Handle other types of bodies.

      classif = pass do
        next unless content_type
        next unless media_type = MIME::MediaType.parse?(content_type)

        Term::Blob::Classif.of(media_type)
      end

      content = Term::Blob.build(classif: classif) do |io|
        IO.copy(src: body, dst: io)
      end
    end

    result = headers.transaction do |commit|
      case content
      in Nil, Term::Str, Term::Blob
        commit << method
      in URLEncodedForm, MultipartForm
        commit << {method, :form}
      end

      commit << encode(resource)

      case content
      in Nil
      in Term::Str, Term::Blob
        commit << content
      in URLEncodedForm, MultipartForm
        commit << encode(content)
      end

      cookies = encode(request.cookies)
      unless cookies.empty?
        commit.with(:cookies, cookies)
      end
    end

    Term.of(result)
  end

  def decode?(request : Term, cls : HTTP::Request.class) : HTTP::Request?
    Term.case(request) do
      # |@ http.request
      #
      # |@pattern
      # (method_symbol resource_ (%plural body min: 0 max: 1) ¦ headers_ cookies⋮ {})
      #
      # |@key method
      # Sets the HTTP method to use. We spell methods in lowercase, e.g., `get`.
      # Wirewright converts them to the traditional GET etc. under the hood.
      #
      # |@key resource http.request.resource
      # Specifies the relevant parts of the URI.
      #
      # |@key body
      # Optionally, specifies the request body. The request body can be a string
      # or a blob. Strings are a shorthand for a blob with the media type
      # `text/plain;charset=UTF-8`.
      #
      # |@key cookies http.request.cookies
      # Optionally, provides the cookies to send with the request.
      #
      # |@key headers http.headers
      # Optionally, provides the headers to send with the request.
      #
      # |@block
      # A request with an optional string or blob body. See the other overload if
      # you want to send a form body instead.
      #
      # |@example
      # ```wwml
      # ;; GET the resource `/`:
      # (get "/")
      #
      # ;; GET the resource `/people?q=Alice`:
      # (get ("/" q: "Alice"))
      #
      # ;; PUT the contents of a post:
      # (put "/posts/post-1"
      #    "<p>Lorem ipsum dolor sit amet!</p>"
      #   content-type: "text/html")
      #
      # ;; DELETE a resource:
      # (delete "/posts/post-1")
      #
      # ;; Provide cookies:
      # (get "/" cookies: {SESSION_ID: "deadbeef"})
      # ```
      matchpi(<<-WWML) do
      (method_symbol resourceQ_ _? ¦ headersQ_ cookies: (%optional {} cookiesQ_dict))
      WWML
        return unless resource = decode?(resourceQ, Resource)

        headers = decode(headersQ.as_d, HTTP::Headers)

        cookies = decode(cookiesQ.as_d, HTTP::Cookies)
        cookies.add_request_headers(headers)

        # Since this is not a form request, its body can only be a string
        # or a blob. Or it could be missing.
        if body = request[2]?
          return unless body = body.as_blob? || body.as_s? # other bodies are invalid

          case body
          in Term::Str
            headers["Content-Type"] ||= "text/plain;charset=UTF-8"
            body = body.to(String).to_slice
          in Term::Blob
            if classif = body.classif?
              headers["Content-Type"] ||= classif.to_s
            end
            body = body.to_slice
          end
        end

        headers["User-Agent"] ||= "Wirewright"

        uri = URI.new(path: resource.path, query: resource.params)
        HTTP::Request.new(method.to(String).upcase, uri.to_s, headers, body)
      end

      # |@ http.request
      #
      # |@pattern
      # ((method_symbol form) resource_ form_ ¦ headers_ cookies_dict)
      #
      # |@key method
      # Sets the HTTP method to use. We spell methods in lowercase, e.g., `get`.
      # Wirewright converts them to the traditional GET etc. under the hood.
      #
      # |@key resource http.request.resource
      # Specifies the relevant parts of the URI.
      #
      # |@key form http.request.form
      # The form body.
      #
      # |@key cookies http.request.cookies
      # Optionally, provides the cookies to send with the request.
      #
      # |@key headers http.headers
      # Optionally, provides the headers to send with the request.
      #
      # |@block
      # A request with a form body: either a URL-encoded form, or a multipart form.
      # See the other overload if you want to send a string or blob body instead.
      #
      # |@example
      # ```wwml
      # ((post form) "/employees/new"
      #   {name: "John Doe",
      #    age: 25})
      # ```
      #
      # See `http.request.form` for more examples.
      matchpi(<<-WWML) do
      ((method_symbol form) resourceQ_ formQ_dict ¦ headersQ_ cookies: (%optional {} cookiesQ_dict))
      WWML
        return unless resource = decode?(resourceQ, Resource)

        headers = decode(headersQ.as_d, HTTP::Headers)

        cookies = decode(cookiesQ.as_d, HTTP::Cookies)
        cookies.add_request_headers(headers)

        if formQ[:multipart]? == Term.of(true)
          return unless form = decode?(formQ, MultipartForm)
        else
          return unless form = decode?(formQ, URLEncodedForm) || decode?(formQ, MultipartForm)
        end

        body = Term::Blob.build do |io|
          case form
          in MultipartForm
            HTTP::FormData.build(io) do |builder|
              headers["Content-Type"] = builder.content_type

              form.fields.each do |name, bucket|
                bucket.each do |field|
                  src = field.data.to_io

                  case field
                  in MultipartData
                    builder.field(name, src, field.headers)
                  in MultipartFile
                    metadata = HTTP::FormData::FileMetadata.new(filename: field.filename)
                    builder.file(name, src, metadata, field.headers)
                  end
                end
              end
            end
          in URLEncodedForm
            HTTP::Params.build(io) do |params|
              headers["Content-Type"] = "application/x-www-form-urlencoded"

              form.fields.each do |name, value|
                params.add(name, value)
              end
            end
          end
        end

        headers["User-Agent"] ||= "Wirewright"

        uri = URI.new(path: resource.path, query: resource.params)
        HTTP::Request.new(method.to(String).upcase, uri.to_s, headers, body.to_io)
      end

      otherwise { }
    end
  end
end
