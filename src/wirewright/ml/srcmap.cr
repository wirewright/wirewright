module Ww::ML
  # Represents a source map: a hash mapping termpaths to views of the original
  # source string that they come from (or are related to, as it is not always
  # possible to link things back properly).
  alias SrcMapHash = Hash(Tpath, StringView)

  # A thin wrapper around `SrcMapHash` that simplifies querying.
  struct SrcMap
    def initialize(@hash : SrcMapHash)
    end

    # Returns the view associated with *path*, if any. *path* is converted
    # to a `Tpath` using `Tpath.[]`.
    #
    # If the view of *path* itself is unavailable, the view of its prior is
    # tried, and so on, until the path is empty or something is found. This
    # way, if the exact *path* is unavailable, at least the source of its parent
    # (or grandparent, etc.) is returned.
    def []?(path) : StringView?
      tpath = Tpath[path]

      loop do
        if text = @hash[tpath]?
          return text
        end

        return if tpath.empty?

        tpath = tpath[...-1]
      end
    end

    # Same as `[]?`, but raises `KeyError` if no view can be found.
    def [](path) : StringView
      self[path]? || raise KeyError.new
    end

    # Works like the `cd` command-line utility: leaves only paths that start
    # with *step*, dropping *step* itself.
    def cd(step : Tpath::Step) : SrcMap
      result = SrcMapHash.new

      @hash.each do |path, text|
        next unless path.first? == step

        result[path[1..]] = text
      end

      SrcMap.new(result)
    end

    # Shorthand for `cd(Tpath.value(Term.of(key)))`.
    def cd(key) : SrcMap
      cd(Tpath.value(Term.of(key)))
    end
  end
end
