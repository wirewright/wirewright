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
  require "./dwuir/snap"

  require "./dwuir/pvg"

  require "./dwuir/window"

  module Ww::Soma::DwUIR
    # Groups configuration for the `show` shorthand function.
    #
    # - *width* is the width of the resulting image, in pixels.
    # - *height* is the height of the resulting image, in pixels.
    # - *backdrop* is the clear color of the resulting image.
    # - *content* is the DwUIR to make a snapshot of.
    record ShowConf,
      width : Int32,
      height : Int32,
      backdrop : Color,
      content : Term

    # A shorthand function to perform a single, one-off draw of *content*.
    # Returns the resulting pixel rectangle `PixelRect`.
    def show(ctx : Viewer::Context, conf : ShowConf)
      screen = PixelRect.new(0, 0, conf.width, conf.height)
      viewer = Viewer.new(screen, ctx)
      viewer.show(conf.content, bg: conf.backdrop)

      screen
    end
  end
{% end %}
