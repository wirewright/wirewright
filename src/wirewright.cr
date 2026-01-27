require "log"
require "big"
require "zstd"
require "uuid"
require "json"
require "sync"
require "digest"
require "blake3"
require "colorize"
require "permafrost"
require "wait_group"
require "compress/gzip"
require "semantic_version"

require "../util"
require "../rtk"

require "./wirewright/misc/*"
require "./wirewright/unicode"
require "./wirewright/term"
require "./wirewright/m0"
require "./wirewright/tpath"
require "./wirewright/file_server"
require "./wirewright/issue"
require "./wirewright/ml"
require "./wirewright/meridium"
require "./wirewright/pigment"
require "./wirewright/dwuir"
require "./wirewright/microfold"
require "./wirewright/soma"
require "./wirewright/lr"

require "../pattern7"
require "./wirewright/m1"
require "../baz5"
require "./wirewright/d7"
require "../primitives"
require "../libtermbox2"
require "../inputr"

require "./wirewright/alloy"

Log.setup_from_env(default_level: :warn, backend: Log::IOBackend.new(STDERR))

module Ww
  VERSION = "0.0.0-kappa"

  # TODO: move resources into runtime

  RESOURCES = Path[ENV["SOMA_RESOURCES_DIR"]? || Dir.current]

  # TODO: This is lame!! We must have much more control over when all these
  # checks happen.
  RUNTIME_PATH = begin
    candidates = {
      ENV["WW_RUNTIME"]?.try { |string| Path[string] },
      Process.executable_path.try { |string| Path[string].parent / "runtime" },
      Path[Dir.current] / "runtime",
    }

    rtpath = candidates.find { |candidate| candidate && Dir.exists?(candidate) }
    rtpath || abort "Wirewright runtime directory not found"
  end

  MT = Fiber::ExecutionContext::Parallel.new("Wirewright", Fiber::ExecutionContext.default_workers_count)

  alias Magnitude = Float32

  # FIXME: WTF is this? Seriously . . .
  module Approx
    extend self

    EPS = 0.001

    {% for type in %w(Float32 Float64) %}
      def equals?(a : {{type.id}}, b : {{type.id}}) : Bool
        (a - b).abs < EPS
      end
    {% end %}
  end
end

require "socket"
require "openssl"
