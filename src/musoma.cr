require "db"
require "sqlite3"
require "./wirewright"

# MuSoma is the newest, most advanced graphical front-end for Wirewright.
module MuSoma
  extend self
  include Ww
end

require "./musoma/var"
require "./musoma/translate"
require "./musoma/scheduler"
require "./musoma/editor"
require "./musoma/classifier"
require "./musoma/perturb"
require "./musoma/codex"
require "./musoma/input"
require "./musoma/agents"
require "./musoma/run"

{% if flag?(:musoma) %}
  ctx = Fiber::ExecutionContext::Isolated.new("Wirewright MuSoma", spawn_context: Ww::MT) { MuSoma.run }
  ctx.wait
{% end %}
