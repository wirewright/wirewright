require "colorize"
require "json"
require "big"
require "permafrost"

require "../oklch"
require "../util"
require "./wirewright/term"
require "./wirewright/ml"
require "../pattern7"
require "../baz5"
require "../delta7_proto2"
require "../primitives"
require "../surf2"
require "../templ"

RESOURCES = Path[ENV["SOMA_RESOURCES_DIR"]? || Dir.current]
