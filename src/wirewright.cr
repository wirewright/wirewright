require "db"
require "log"
require "big"
require "uuid"
require "json"
require "sync/*"
require "bit_array"
require "http"
require "mime/media_type"
require "digest"
require "sqlite3"
require "colorize"
require "permafrost"
require "wait_group"
require "compress/gzip"
require "semantic_version"

require "../util"

require "./wirewright/libs/*"
require "./wirewright/misc/*"
require "./wirewright/unicode"
require "./wirewright/pantomime"
require "./wirewright/term"
require "./wirewright/m0"
require "./wirewright/tpath"
require "./wirewright/outcome"
require "./wirewright/service"
require "./wirewright/extrinsic_map"

require "./wirewright/path_monitor"
require "./wirewright/path_server"
require "./wirewright/uri_server"
require "./wirewright/resource_server"

require "./wirewright/ml"
require "./wirewright/lr"

require "./wirewright/lang"
require "./wirewright/nitrene"
require "./wirewright/scenery"
require "./wirewright/microfold"

require "./wirewright/console"

require "./wirewright/dwuir"
require "./wirewright/soma"

module Ww
end

# TODO: REMOVE (this particular include is a big wart on the face of the project!)
include Ww

require "./wirewright/m1"
require "./wirewright/rho"
require "./wirewright/scan_kit"
require "./wirewright/parse_kit"
require "../baz5"
require "./wirewright/d7"
require "../libtermbox2"

require "./wirewright/alloy"
require "./wirewright/harmony"
require "./wirewright/rack"

Log.setup_from_env(
  default_level: :warn,
  backend: Log::IOBackend.new(STDERR),
  default_sources: ENV["LOG_SOURCES"]? || "*",
)

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

  # The string used for the User-Agent header in requests. This follows the [Wikimedia
  # User-Agent policy](https://foundation.wikimedia.org/wiki/Policy:Wikimedia_Foundation_User-Agent_Policy).
  USER_AGENT = "Wirewright/#{VERSION} (https://github.com/wirewright/wirewright)"

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
  #   to the `runtime/` sibling of the executable path, or `runtime/` in CWD in case
  #   that doesn't work. The runtime directory can be overridden using
  #   the `WW_RUNTIME` environment variable. If the runtime directory cannot
  #   be determined, it is set to `nil`. Note that due to races and TOCTOU type
  #   of stuff, the fact that *runtime* is not nil does not mean that it exists;
  #   it only means that it existed at the time of the check.
  defrecord RootPathSet, cwd : Path, home : Path, runtime : Path?

  # Returns the root path set of Wirewright.
  #
  # See `RootPathSet` for more info.
  class_getter roots : RootPathSet do
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

    RootPathSet.new(cwd, home, runtime)
  end
end
