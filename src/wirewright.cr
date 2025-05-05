require "log"
require "big"
require "json"
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
end

require "socket"
