module Ww
  # Represents a path normalized with respect to Wirewright's root path
  # set `roots`.
  #
  # A normal path is always an absolute, lexically normalized (see `Path#normalize`) path.
  struct NormalPath
    protected def initialize(*, __path @path : Path)
    end

    # The normalization process is pure & purely lexical, so we can cache it.
    # Ww.roots are computed once and stay the same for the lifetime of
    # the process.
    @@cache = SyncLRU(Path, NormalPath).new(128)

    def self.new(path : Path)
      @@cache.put_if_absent(path) do
        unless path.normal?
          path = path.normalize
        end

        unless path.absolute?
          path = path.expand(base: Ww.roots.cwd, home: Ww.roots.home, expand_base: false)
        end

        new(__path: path)
      end
    end

    def self.[](*parts : String)
      new(Path[*parts])
    end

    def self.[](path : Path)
      new(path)
    end

    def <=>(other : NormalPath)
      unwrap <=> other.unwrap
    end

    def unwrap : Path
      @path
    end

    def parent : NormalPath
      # If @path is normal, then @path.parent is normal.
      NormalPath.new(__path: @path.parent)
    end

    def basename : String
      @path.basename
    end

    def stem : String
      @path.stem
    end

    def extension : String
      @path.extension
    end

    def extension?(*extensions : String) : Bool
      @path.extension?(*extensions)
    end

    def absolute? : Bool
      true # Normal paths are absolute
    end

    def /(member : String | Path) : Path
      @path / member
    end

    def to_s(io)
      io << @path
    end

    def inspect(io)
      io << "normal(" << @path << ")"
    end
  end
end
