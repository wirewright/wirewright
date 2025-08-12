module Ww::Soma::DwUIR
  extend self

  Log = ::Log.for(self)
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
  require "./dwuir/picture"
  require "./dwuir/walk"
  require "./dwuir/render"
  require "./dwuir/hit"

  require "./dwuir/pixel_rect"
  require "./dwuir/compositor"
  require "./dwuir/platform"
  require "./dwuir/viewer"
  require "./dwuir/replier"

  require "./dwuir/pvg"

  require "./dwuir/window"
{% end %}
