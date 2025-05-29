require "log"
require "big"
require "uuid"
require "json"
require "sync"
require "digest"
require "blake3"
require "colorize"
require "permafrost"
require "wait_group"

require "../oklch"
require "../util"

require "./wirewright/term"
require "./wirewright/ml"
require "./wirewright/meridium"
require "./wirewright/soma"

require "../pattern7"
require "../baz5"
require "../delta7_proto2"
require "../primitives"
require "../templ"

{% if flag?(:release) %}
  Log.setup_from_env(default_level: :warn)
{% else %}
  Log.setup_from_env(default_level: :debug)
{% end %}

module Ww
  RESOURCES = Path[ENV["SOMA_RESOURCES_DIR"]? || Dir.current]

  MT = Fiber::ExecutionContext::MultiThreaded.new("Wirewright", System.cpu_count.to_i)

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
