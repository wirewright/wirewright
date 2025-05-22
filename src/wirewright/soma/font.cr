module Ww::Soma
  # Lists the supported font weights.
  enum FontWeight : UInt8
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

    def self.parse(n : Int32)
      weights = {100, 200, 300, 400, 450, 500, 600, 700, 800, 900}
      weight = weights.min_by { |weight| (weight - n).abs }
      new(weights.index!(weight).to_u8)
    end

    def self.size : Int32
      {{ @type.constants.size }}
    end

    def self.sway(pivot : FontWeight, & : FontWeight ->)
      yield pivot

      (1...size).each do |offset|
        yield pivot - offset if p = pivot.value >= offset
        yield pivot + offset if q = pivot.value + offset < size
        break unless p || q
      end
    end
  end

  # Specifies where to start searching for fonts.
  FONTS_FOLDER = RESOURCES / "fonts"

  # Font entry parser can parse candidate font or font-related paths into
  # `FontEntry` objects.
  module FontEntryParser
    extend self

    Log = ::Log.for("FontEntryParser")

    record FontEntry, kind : Kind, path : Path, family : String, weight : FontWeight, italic : Bool do
      enum Kind : UInt8
        Font
        Codepoints
      end
    end

    # :nodoc:
    CAMEL_BOUND_REGEX = /(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])/

    # :nodoc:
    def spaced(camel : String) : String
      camel.gsub(CAMEL_BOUND_REGEX, ' ')
    end

    # :nodoc:
    FONT_FILE_REGEX = /(?<family>\w+)\-(?<weightid>\w+?)??(?<variant>Italic)?$/

    # Lists the supported font extensions.
    FONT_EXTENSIONS = {".otf", ".ttf"}

    # Attempts to parse *path* into a font entry. Returns `nil` if parsing failed.
    def font_entry?(path : Path) : FontEntry?
      case path.extension
      when .in?(FONT_EXTENSIONS)
        kind = FontEntry::Kind::Font
      when ".codepoints"
        kind = FontEntry::Kind::Codepoints
      else
        return
      end

      unless match = path.stem.match(FONT_FILE_REGEX, options: :anchored)
        Log.warn { "path with font-like extension did not match font regex: #{path}, skip" }
        return
      end

      family, weightid, variant = match["family"], match["weightid"]?, match["variant"]?

      weightid ||= "Regular"

      unless weight = FontWeight.parse?(weightid)
        Log.warn { "weight did not match one of known weights for font: #{path}, skip" }
        return
      end

      italic = false

      case variant = match["variant"]?
      when .nil?
      when "Italic"
        italic = true
      else
        Log.warn { "unknown font variant: #{path}, skip" }
        return
      end

      FontEntry.new(kind, path, spaced(family), weight, italic)
    end
  end

  # Font index locates a font on disk based on its name (e.g. "IBM Plex Sans"),
  # weight (e.g. `FontWeight::Bold`), and whether it's italic or not. The result
  # is an absolute path to the font file (TTF, OTF, etc.)
  #
  # Font index also allows to map of human-readable codepoint names to the actual
  # numeric codepoints; assuming the font has a corresponding `.codepoints` file.
  #
  # See: `path_to?`, `codepoint?`.
  module FontIndex
    extend self

    Log = ::Log.for("FontIndex")

    # :nodoc:
    record CodepointMap, codepoints = {} of String => Char do
      delegate :has_key?, :[]?, :[]=, to: @codepoints

      def inspect(io)
        io << "{codepoint map with " << codepoints.size << " codepoint(s)}"
      end

      def to_s(io)
        inspect(io)
      end
    end

    # :nodoc:
    record FontQuery, family : String, weight : FontWeight, italic : Bool

    # :nodoc:
    record FontResponse, path : Path, codepoints : CodepointMap

    # :nodoc:
    alias FontIndex = Hash(FontQuery, FontResponse)

    # :nodoc:
    #
    # Constructs a font index based on fonts on the disk.
    def index(root : Path) : FontIndex
      leaves = Dir[root / "**/*.*", match: :none]
      entries = leaves.compact_map { |file| FontEntryParser.font_entry?(Path[file]) }
      index = FontIndex.new(initial_capacity: entries.size)

      entries.each do |entry|
        next unless entry.kind.font?

        query = FontQuery.new(entry.family, entry.weight, entry.italic)

        if twin = index[query]?
          Log.warn { "twin font files: #{twin.path}, skip #{entry.path}" }
          next
        end

        index[query] = FontResponse.new(entry.path, codepoints: CodepointMap.new)
      end

      entries.each do |entry|
        next unless entry.kind.codepoints?

        query = FontQuery.new(entry.family, entry.weight, entry.italic)
        unless font = index[query]?
          Log.warn { "found a .codepoints file but not the corresponding font file: #{entry.path}" }
          next
        end

        File.open(entry.path) do |io|
          io.each_line do |line|
            name, codepoint_hex = line.split(' ', limit: 2)

            if font.codepoints.has_key?(name)
              Log.warn { "#{entry.path}: duplicate name for codepoint: #{name}, skip" }
              next
            end

            unless codepoint = codepoint_hex.to_i?(base: 16)
              Log.warn { "#{entry.path}: cannot parse codepoint hex: #{codepoint_hex}, skip" }
              next
            end

            font.codepoints[name] = codepoint.chr
          end
        end
      end

      index
    end

    @@lock = Sync::RWLock.new
    @@index : FontIndex = @@lock.write { index(FONTS_FOLDER) }

    private def each_possible_query_with_italic(family, weight pivot, italic, &) : Nil
      FontWeight.sway(pivot) do |weight|
        yield FontQuery.new(family, weight, italic)
      end
    end

    # :nodoc:
    def each_possible_query(family, weight, italic, &) : Nil
      each_possible_query_with_italic(family, weight, italic) { |query| yield query }
      each_possible_query_with_italic(family, weight, !italic) { |query| yield query }
    end

    # Returns the path to *font* with the given *weight*, or optionally to its *italic* variant.
    #
    # - If *weight* does not exist for *font* tries to sway *weight* to find an existing
    #   font. For example, for Regular it will sway Text-Medium, Light-Bold, Thin-Black.
    # - If still nothing, tries to flip your *italic* choice. For non italic queries tries
    #   to find an italic variant that satisfies *family* and *weight*, if possible; for
    #   non-italic queries, similarly tries to find an italic variant.
    # - If still nothing, returns `nil`.
    def path_to?(family : String, weight : FontWeight = FontWeight::Regular, *, italic : Bool = false) : Path?
      each_possible_query(family, weight, italic) do |query|
        next unless response = @@lock.read { @@index[query]? }
        return response.path
      end
    end

    # Retrieves the codepoint for *name* based on the `.codepoints` file for
    # the given font variant.
    #
    # The algorithm for looking up the font and the corresponding `.codepoints`
    # file is the same as in `path_to?`.
    def codepoint?(name : String, family : String, weight : FontWeight = FontWeight::Regular, *, italic : Bool = false) : Char?
      each_possible_query(family, weight, italic) do |query|
        @@lock.read do
          next unless response = @@index[query]?
          next unless codepoint = response.codepoints[name]?
          return codepoint
        end
      end
    end
  end
end
