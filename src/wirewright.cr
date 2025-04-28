require "log"
require "big"
require "json"
require "digest"
require "colorize"
require "permafrost"
require "wait_group"
require "blake3"

require "../oklch"
require "../util"
require "./wirewright/term"
require "./wirewright/ml"
{% if flag?(:surf5) %}
  require "./wirewright/meridium"
{% end %}
require "../pattern7"
require "../baz5"
require "../delta7_proto2"
require "../primitives"
{% unless flag?(:surf5) %}
  require "../surf4"
{% end %}
require "../templ"

RESOURCES = Path[ENV["SOMA_RESOURCES_DIR"]? || Dir.current]
