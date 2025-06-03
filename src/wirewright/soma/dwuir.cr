module Ww::Soma::DwUIR
  # TODO: move!!!
  alias TextSelectionRange = Range(Int32, Int32)?
end

require "./dwuir/color"
require "./dwuir/font"

{% if flag?(:newsoma) %}
  require "./dwuir/tf"
  require "./dwuir/magn"
  require "./dwuir/pencil"
  require "./dwuir/point"
  require "./dwuir/segment"
  require "./dwuir/rect"
  require "./dwuir/quad"
  require "./dwuir/paint"
  require "./dwuir/draw_command"
  require "./dwuir/layer"
  require "./dwuir/pixel"

  require "./dwuir/wrap_token"
  require "./dwuir/text_element"
  require "./dwuir/text_command"
  require "./dwuir/text_drawable"
  require "./dwuir/walk"
{% end %}
