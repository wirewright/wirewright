module Ww
  # The main idea behind Wirewright's ResourceServer is that it should provide
  # a dirt cheap polling API for fetching resources (described by `Query`).
  #
  # Consider `ResourceServer.read_string(ResourceServer.file("/tmp/foo.wwml"))`.
  # ResourceServer lets you invoke this as many times as you want -- say, every
  # microsecond. ResourceServer acts as a "cushion" for such polls; so clients
  # simply poll, whereas under the hood, we are using inotify, caching, exponential
  # backoff, and so on to maintain an up-to-date model of the external resource,
  # be it a file or an HTTP endpoint.
  #
  # ResourceServer is a unifying *get* (fetch) abstraction over multiple underlying
  # servers, which follow a similar architecture & goals: `PathServer`, `URIServer`.
  # Everything else differs too much for unification to make sense. So refer to
  # `PathServer`, `URIServer` and so on for e.g. *writing* or *deletion*.
  module ResourceServer
    extend self

    alias Query = FileQuery | CodexQuery | RuntimeQuery | FontQuery | CodepointsQuery | URIQuery

    defrecord RuntimeQuery, path : Path
    defrecord CodexQuery, name : String
    defrecord FileQuery, path : Path
    defrecord FontQuery, family : String, weight : Int32, italic : Bool
    defrecord CodepointsQuery, family : String
    defrecord URIQuery, uri : URI

    def runtime(path : Path) : RuntimeQuery
      RuntimeQuery.new(path)
    end

    def codex(name : String) : CodexQuery
      CodexQuery.new(name)
    end

    def file(path : Path) : FileQuery
      FileQuery.new(path)
    end

    def font(family : String, weight : Int32, italic : Bool) : FontQuery
      FontQuery.new(family, weight, italic)
    end

    def codepoints(family : String) : CodepointsQuery
      CodepointsQuery.new(family)
    end

    def uri(uri : URI) : URIQuery
      URIQuery.new(uri)
    end

    alias Response = Present | Absent | Wait

    defrecord Present, content : Term::Blob
    defrecord Absent, detail : String = ""
    defrecord Wait

    private def get_impl(query : CodexQuery) : Response
      get_impl(RuntimeQuery.new(Path["codices"] / (query.name + ".codex.wwml")))
    end

    private def get_impl(query : RuntimeQuery) : Response
      get_impl(FileQuery.new(Ww.roots.runtime / query.path))
    end

    private def get_impl(query : FileQuery) : Response
      case response = PathServer.view(query.path)
      in PathServer::Wait
        Wait.new
      in PathServer::Absent
        Absent.new(detail: "path does not exist")
      in PathServer::DirListing
        Absent.new(detail: "path is a directory")
      in PathServer::LargeFileListing
        Absent.new(detail: "file at path is too large to load into memory")
      in PathServer::FileListing
        Present.new(response.content)
      end
    end

    # Lists the supported font weights.
    enum FontWeight : Int32
      Thin
      ExtraLight
      Light
      Regular
      Text
      Medium
      SemiBold
      Bold
      ExtraBold
      Black

      def self.closest(n : Int32)
        weights = {100, 200, 300, 400, 450, 500, 600, 700, 800, 900}
        weight = weights.min_by { |weight| (weight - n).abs }
        new(weights.index!(weight))
      end

      def self.find(haystack : String)
        {% for weight in FontWeight.constants %}
        if haystack.includes?("{{weight.downcase}}")
          return FontWeight::{{weight}}
        end
      {% end %}

        FontWeight::Regular
      end
    end

    # :nodoc:
    defrecord FontEntry, path : Path, weight : FontWeight, italic : Bool

    # The supported font extensions.
    FONT_EXTENSIONS = {".otf", ".ttf"}

    # :nodoc:
    FONT_LISTING_CACHE = SyncLRU(PathServer::DirListing, Slice(FontEntry)).new(capacity: 32)

    private def font_listing(view : PathServer::DirListing) : Slice(FontEntry)
      FONT_LISTING_CACHE.put_if_absent(view) do
        entries = Pf::Kit.stack_array(FontEntry, 8)

        view.entries.each do |entry|
          next unless entry.is_a?(PathServer::FileEntry)
          next unless entry.path.extension.in?(FONT_EXTENSIONS)

          stem = entry.path.stem.downcase

          entries << FontEntry.new(entry.path,
            weight: FontWeight.find(stem),
            italic: stem.includes?("italic"),
          )
        end

        entries.to_unsafe_readonly_slice!
      end
    end

    private def get_impl(query : FontQuery) : Response
      case view = PathServer.view(Ww.roots.runtime / "fonts" / query.family)
      in PathServer::Wait
        Wait.new
      in PathServer::Absent
        Absent.new(detail: "font family not found in font database")
      in PathServer::FileListing, PathServer::LargeFileListing
        Absent.new(detail: "font family is not a directory")
      in PathServer::DirListing
        weight = FontWeight.closest(query.weight)
        italic = query.italic

        # Find the best matching font.
        #
        # The distance between two fonts is the distance between their italics
        # followed by the distance between weights. This way, we prefer italic
        # over non-italic if we don't have a matching weight and the user
        # requested italic. We only use non-italic for italic if there is no
        # italic variant whatsoever.
        best = font_listing(view).min_by? do |candidate|
          {((italic ? 1 : 0) - (candidate.italic ? 1 : 0)).abs,
           (weight.value - candidate.weight.value).abs}
        end

        unless best
          return Absent.new(detail: "could not find matching font in family")
        end

        get_impl(FileQuery.new(best.path))
      end
    end

    private def get_impl(query : CodepointsQuery) : Response
      case view = PathServer.view(Ww.roots.runtime / "fonts" / query.family)
      in PathServer::Wait
        Wait.new
      in PathServer::Absent
        Absent.new(detail: "font family not found in font database")
      in PathServer::FileListing, PathServer::LargeFileListing
        Absent.new(detail: "font family is not a directory")
      in PathServer::DirListing
        view.entries.each do |entry|
          next unless entry.path.extension?(".codepoints")
          return get_impl(FileQuery.new(entry.path))
        end

        Absent.new(detail: "font codepoints not found")
      end
    end

    private def get_impl(query : URIQuery) : Response
      case status = URIServer.get(query.uri)
      in URIServer::Wait
        Wait.new
      in URIServer::Absent
        Absent.new(status.detail)
      in URIServer::Present
        Present.new(status.content)
      end
    end

    @@running = Atomic(Bool).new(false)
    @@changed = BlockingSignal.new

    private def ensure_server_running!
      return if @@running.swap(true)

      spawn(name: "ResourceServer path wait") do
        epoch = 0u64

        loop do
          epoch = PathServer.wait(epoch)

          @@changed.call
        end
      end

      spawn(name: "ResourceServer URI wait") do
        epoch = 0u64

        loop do
          epoch = URIServer.wait(epoch)

          @@changed.call
        end
      end
    end

    # Responds to *query*. The returned response is based on a snapshot of
    # the resource associated with *query* at some unspecified point in time.
    # The response is *eventually consistent*: it may not reflect the instantaneous
    # state of the resource.
    def get(query : Query) : Response
      ensure_server_running!

      get_impl(query)
    end

    class Error < Exception
    end

    def wait(epoch : UInt64) : UInt64
      @@changed.wait(epoch)
    end

    def wait(query : Query) : Present | Absent
      epoch = 0u64

      loop do
        case response = get(query)
        in Wait
          epoch = wait(epoch)
        in Present, Absent
          return response
        end
      end
    end

    def read_blob(query : Query) : Term::Blob
      case response = wait(query)
      in Present
        response.content
      in Absent
        raise Error.new(response.detail)
      end
    end

    def read_string(query : Query) : String
      blob = read_blob(query)

      String.new(blob.bytes)
    end

    # Parses *term* into a query.
    def query?(term : Term) : Query?
      Term.case(term) do
        matchpi %{(file path_string)}, path: Path do
          FileQuery.new(path)
        end

        matchpi %{(font family_string ¦ weight_: (%number +i32) italic⋮ false)}, family: String, weight: Int32, italic: Bool do
          FontQuery.new(family, weight, italic)
        end

        matchpi %{(codepoints family_string)}, family: String do
          CodepointsQuery.new(family)
        end

        matchpi %{(uri uri_string)}, uri: String do
          URIQuery.new(URI.parse(uri))
        end

        otherwise { }
      end
    end
  end
end
