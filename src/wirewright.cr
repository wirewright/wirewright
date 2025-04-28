require "colorize"
require "json"
require "big"
require "permafrost"

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
