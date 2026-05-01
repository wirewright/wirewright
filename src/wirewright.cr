require "log"
require "big"
require "uuid"
require "json"
require "sync/*"
require "http/client"
require "http/status"
require "mime/media_type"
require "digest"
require "colorize"
require "permafrost"
require "wait_group"
require "compress/gzip"
require "semantic_version"

require "../util"
require "../rtk"

require "./wirewright/libs/*"
require "./wirewright/misc/*"
require "./wirewright/unicode"
require "./wirewright/pantomime"
require "./wirewright/term"
require "./wirewright/m0"
require "./wirewright/tpath"
require "./wirewright/outcome"
require "./wirewright/service/*"

require "./wirewright/path_monitor"
require "./wirewright/path_server"
require "./wirewright/uri_server"
require "./wirewright/resource_server"

require "./wirewright/issue"
require "./wirewright/ml"
require "./wirewright/lr"

require "./wirewright/pigment"
require "./wirewright/nitrene"
require "./wirewright/scenery"
require "./wirewright/microfold_2"

require "./wirewright/dwuir"
require "./wirewright/soma"
require "./wirewright/microfold"

module Ww
end

# TODO: REMOVE (this particular include is a big wart on the face of the project!)
include Ww

require "./wirewright/m1"
require "./wirewright/rho"
require "../baz5"
require "./wirewright/d7"
require "../primitives"
require "../libtermbox2"

require "./wirewright/alloy"
require "./wirewright/rack"

Log.setup_from_env(default_level: :warn, backend: Log::IOBackend.new(STDERR))

module Ww
  # TODO: Come up with a better name. Magnitude doesn't quite fit.
  alias Magnitude = Float32

  # Attempts to interpret *term* as a `Magnitude`.
  #
  # - `∞` gives `Magnitude::INFINITY`.
  # - Otherwise uses `Term#to`, which will raise `TypeCastError` on failure.
  def self.magn(term : Term) : Magnitude
    term == Term.of(:∞) ? Magnitude::INFINITY : term.to(Magnitude)
  end

  VERSION = "0.0.0-iota"

  # The default execution context used by Wirewright.
  MT = Fiber::ExecutionContext::Parallel.new("Wirewright", maximum: Fiber::ExecutionContext.default_workers_count)

  # Represents the root path set of Wirewright.
  #
  # - *cwd* points to the current working directory. It defaults to `Dir.current`,
  #   but can be overridden using the `WW_CWD` environment variable.
  #
  # - *home* points to the home directory (in case `~` is used in paths). It
  #   defaults to `Path.home`, but can be overridden using the `WW_HOME`
  #   environment variable.
  #
  # - *runtime* points to the runtime directory. The runtime directory contains
  #   assets used by Wirewright itself, such as codices and fonts. It defaults
  #   `runtime/` sibling of the executable path, or `runtime/` in CWD in case
  #   that doesn't work. The runtime directory can be overridden using
  #   the `WW_RUNTIME` environment variable. If the runtime directory cannot
  #   be determined, it is set to `nil`. Note that due to races and TOCTOU type
  #   of stuff, the fact that *runtime* is not nil does not mean it exists at
  #   the current moment; it only means that it existed at the time of check.
  defrecord RootSet, cwd : Path, home : Path, runtime : Path?

  # Returns the root path set of Wirewright.
  #
  # See `RootSet` for more info.
  class_getter roots : RootSet do
    cwd = pass do
      if setting = ENV["WW_CWD"]?
        next Path[setting]
      end

      Path[Dir.current]
    end

    home = pass do
      if setting = ENV["WW_HOME"]?
        next Path[setting]
      end

      Path.home
    end

    runtime = pass do
      if setting = ENV["WW_RUNTIME"]?
        next Path[setting]
      end

      {Process.executable_path, cwd}.leftmost? do |origin|
        next unless origin

        path = Path[origin] / "runtime"
        next unless Dir.exists?(path)

        path
      end
    end

    RootSet.new(cwd, home, runtime)
  end

  # Normalizes the given *path* with respect to Wirewright's root path set `roots`.
  def self.normalize(path : Path) : Path
    unless path.normal?
      path = path.normalize
    end

    unless path.absolute?
      path = path.expand(base: roots.cwd, home: roots.home, expand_base: false)
    end

    path
  end
end
