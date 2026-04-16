require "log"
require "big"
require "zstd"
require "uuid"
require "json"
require "sync"
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
require "./wirewright/term"
require "./wirewright/m0"
require "./wirewright/tpath"
require "./wirewright/outcome"
require "./wirewright/path_monitor"
require "./wirewright/path_server"
require "./wirewright/uri_server"
require "./wirewright/resource_server"
require "./wirewright/issue"
require "./wirewright/ml"
require "./wirewright/pigment"
require "./wirewright/dwuir"
require "./wirewright/microfold"
require "./wirewright/soma"
require "./wirewright/lr"

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
  # - *cwd* points to the current working directory.
  # - *home* points to the home directory (in case `~` is used in paths).
  # - *runtime* points to the runtime directory. The runtime directory contains
  #   assets used by Wirewright itself, such as codices and fonts.
  #
  # Note that you aren't the intended user of `RootSet` (nor `normalize`);
  # `ResourceServer`, `PathServer`, and others are. You should use them instead
  # of reading files or directories with Crystal's `File` or `Dir` API.
  defrecord RootSet, cwd : Path, home : Path, runtime : Path

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

    unless runtime
      abort "Wirewright runtime directory not found"
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
