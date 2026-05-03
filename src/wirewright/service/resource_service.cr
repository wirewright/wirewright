module Ww
  module ResourceService
    extend self

    alias Query = FileQuery |
                  CodexQuery |
                  RuntimeQuery |
                  FontQuery |
                  CodepointsQuery |
                  HTTPQuery |
                  IdQuery

    defrecord RuntimeQuery, member : String
    defrecord CodexQuery, name : String
    defrecord FileQuery, path : NormalPath
    defrecord FontQuery, family : String, weight : Int32, italic : Bool
    defrecord CodepointsQuery, family : String
    defrecord HTTPQuery, uri : URI
    defrecord IdQuery, content : Term::Blob

    # Shorthand for constructing `RuntimeQuery`.
    def runtime(member : String) : RuntimeQuery
      RuntimeQuery.new(member)
    end

    # Shorthand for constructing `CodexQuery`.
    def codex(name : String) : CodexQuery
      CodexQuery.new(name)
    end

    # Shorthand for constructing `FileQuery`.
    def file(path : NormalPath) : FileQuery
      FileQuery.new(path)
    end

    # :ditto:
    def file(path : String) : FileQuery
      file(NormalPath[path])
    end

    # Shorthand for constructing `FontQuery`.
    def font(family : String, weight : Int32, italic : Bool) : FontQuery
      FontQuery.new(family, weight, italic)
    end

    # Shorthand for constructing `CodepointsQuery`.
    def codepoints(family : String) : CodepointsQuery
      CodepointsQuery.new(family)
    end

    # Shorthand for constructing `HTTPQuery`.
    def http(uri : URI) : HTTPQuery
      HTTPQuery.new(uri)
    end

    # :ditto:
    def http(uri : String) : HTTPQuery
      http(URI.parse(uri))
    end

    # Shorthand for constructing `IdQuery`.
    def id(content : Term::Blob) : IdQuery
      IdQuery.new(content)
    end

    alias Response = Present | Absent

    # Signals that a response is present and was loaded into memory successfully
    # as *content*.
    defrecord Present, query : ResolvedQuery, content : Term::Blob

    # Signals that a response is absent and/or was not loaded into memory. *detail*
    # may provide further explanation.
    defrecord Absent, detail : String

    # Lists the supported font extensions.
    FONT_EXTENSIONS = {".otf", ".ttf"}

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
    defrecord FontEntry, path : NormalPath, weight : FontWeight, italic : Bool

    # :nodoc:
    FONT_ENTRIES_CACHE = SyncLRU(PathService::DirListing, Slice(FontEntry)).new(capacity: 32)

    private def font_entries(listing : PathService::DirListing) : Slice(FontEntry)
      FONT_ENTRIES_CACHE.put_if_absent(listing) do
        entries = Pf::Kit.stack_array(FontEntry, 8)

        listing.entries.each do |entry|
          next unless entry.is_a?(PathService::FileEntry)
          next unless entry.path.extension.in?(FONT_EXTENSIONS)

          stem = entry.path.stem.downcase

          entries << FontEntry.new(entry.path,
            weight: FontWeight.find(stem),
            italic: stem.includes?("italic"),
          )
        end

        entries.sort_by!(&.weight)
        entries.to_unsafe_readonly_slice!
      end
    end

    # :nodoc:
    alias Resn = ResolvedQuery | Absent
    # :nodoc:
    alias ResolvedQuery = FileQuery | HTTPQuery | IdQuery

    private def resolve(query : CodexQuery) : Promise(Resn)
      member = "codices/#{query.name}.codex.wwml"

      resolve(RuntimeQuery.new(member))
    end

    private def resolve(query : RuntimeQuery) : Promise(Resn)
      unless runtime = Ww.roots.runtime
        return Promise(Resn).resolved(Absent.new("Wirewright runtime directory does not exist"))
      end

      resolve(FileQuery.new(NormalPath[runtime / query.member]))
    end

    private def resolve(query : ResolvedQuery) : Promise(Resn)
      Promise(Resn).resolved(query)
    end

    private def resolve(query : FontQuery) : Promise(Resn)
      unless runtime = Ww.roots.runtime
        return Promise(Resn).resolved(Absent.new("Wirewright runtime directory does not exist"))
      end

      PathService.listing(NormalPath[runtime / "fonts" / query.family]).map do |listing|
        case listing
        in PathService::Absent
          Promise(Resn).accepted(Absent.new("font family not found in font database"))
        in PathService::FileListing
          Promise(Resn).accepted(Absent.new("font family is not a directory"))
        in PathService::DirListing
          weight = FontWeight.closest(query.weight)
          italic = query.italic

          # Find the best matching font.
          #
          # The distance between two fonts is the distance between their italics
          # followed by the distance between weights. This way, we prefer italic
          # over non-italic if we don't have a matching weight and the user
          # requested italic. We only use non-italic for italic if there is no
          # italic variant whatsoever.
          best = font_entries(listing).min_by? do |candidate|
            {((italic ? 1 : 0) - (candidate.italic ? 1 : 0)).abs,
             (weight.value - candidate.weight.value).abs}
          end

          if best
            Promise(Resn).accepted(FileQuery.new(best.path))
          else
            Promise(Resn).accepted(Absent.new("could not find matching font in family"))
          end
        end
      end
    end

    private def resolve(query : CodepointsQuery) : Promise(Resn)
      unless runtime = Ww.roots.runtime
        return Promise(Resn).resolved(Absent.new("Wirewright runtime directory does not exist"))
      end

      PathService.listing(NormalPath[runtime / "fonts" / query.family]).map do |listing|
        case listing
        in PathService::Absent
          Promise(Resn).accepted(Absent.new("font family not found in font database"))
        in PathService::FileListing
          Promise(Resn).accepted(Absent.new("font family is not a directory"))
        in PathService::DirListing
          entry = listing.entries.find(&.path.extension?(".codepoints"))
          if entry
            Promise(Resn).accepted(FileQuery.new(entry.path))
          else
            Promise(Resn).accepted(Absent.new("font codepoints not found"))
          end
        end
      end
    end

    private def get!(query : FileQuery) : Promise(Response)
      PathService.read(query.path).map do |reading|
        case reading
        in PathService::ContentReading
          response = Present.new(query, reading.blob)
        in PathService::DigestReading
          response = Absent.new("resource too large to load into memory")
        in PathService::Absent
          response = Absent.new(reading.detail)
        end

        Promise(Response).accepted(response)
      end
    end

    private def get!(query : HTTPQuery) : Promise(Response)
      HTTPService.get(query.uri).map do |response|
        case response
        in HTTPService::Present
          Promise(Response).accepted(Present.new(query, response.body))
        in HTTPService::Absent, HTTPService::Aborted
          Promise(Response).accepted(Absent.new(response.detail))
        end
      end
    end

    private def get!(query : IdQuery) : Promise(Response)
      Promise(Response).resolved(Present.new(query, query.content))
    end

    # Returns the response to *query*. The returned response is based on a snapshot
    # of the resource associated with *query* at some unspecified point in the past.
    # The response is *eventually consistent*: it may not reflect the instantaneous
    # state of the resource.
    #
    # For maintaining resources up-to-date, consult `PathService` and `HTTPService`
    # individually, and also `invalidate`.
    def get(query : Query) : Promise(Response)
      resolve(query).bind do |resn|
        case resn
        in ResolvedQuery
          get!(resn)
        in Absent
          Promise(Response).resolved(resn)
        end
      end
    end

    # Raised in case of an error in higher-level functions such as `read_blob`.
    class Error < Exception
    end

    # Blocks until *query* is resolved and loaded into memory. Raises `Error`
    # in case of an error.
    def read_blob(query : Query) : Term::Blob
      state = get(query).wait

      case state
      in Promise::Accepted(Response)
      in Promise::Rejected
        raise Error.new(state.detail)
      end

      case response = state.object
      in Present
        response.content
      in Absent
        raise Error.new(response.detail)
      end
    end

    # Blocks until *query* is resolved and loaded into memory, and converts
    # the resulting `Term::Blob` to a Crystal `String`. Raises `Error` in
    # case of an error.
    def read_string(query : Query) : String
      blob = read_blob(query)
      blob.to_string
    end

    private def invalidate!(query : FileQuery) : Nil
      PathService.invalidate(query.path)
    end

    private def invalidate!(query : HTTPQuery) : Nil
      HTTPService.invalidate(query.uri)
    end

    private def invalidate!(query : IdQuery) : Nil
      # IdQueries are not cached.
    end

    # Invalidates `PathService` and/or  `HTTPService` caches associated with *query*.
    def invalidate(query : Query) : Nil
      resn = resolve(query).wait
      return unless resn.is_a?(Promise::Accepted(Resn))             # ?!
      return unless resolved_query = resn.object.as?(ResolvedQuery) # ?!

      invalidate!(resolved_query)
    end

    # Parses *term* into a query.
    def query?(term : Term) : Query?
      Term.case(term) do
        matchpi %{(codex name_string)}, name: String do
          codex(name)
        end

        matchpi %{(runtime member_string)}, member: String do
          runtime(member)
        end

        matchpi %{(file path_string)}, path: NormalPath do
          file(path)
        end

        matchpi %{(font family_string ¦ weight_: (%optional 450 (%number +i32)) italic⋮ false)}, family: String, weight: Int32, italic: Bool do
          font(family, weight, italic)
        end

        matchpi %{(codepoints family_string)}, family: String do
          codepoints(family)
        end

        matchpi %{(uri uri_string)}, uri: String do
          http(uri) # ?!
        end

        matchpi %{(literal content_string)}, content: String do
          id(Term::Blob.new(content))
        end

        matchpi %{(literal content_blob)}, content: Term::Blob do
          id(content)
        end

        otherwise { }
      end
    end
  end
end
