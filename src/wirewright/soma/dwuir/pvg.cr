module Ww::Soma::DwUIR
  # An implementation of `Platform` using PlutoVG.
  #
  # Reference: https://github.com/sammycage/plutovg
  struct PvgPlatform
    include Platform

    def initialize(files : FileServer, vwh : Point)
      @fonts = PvgFontFaceStore.new
      @images = PvgImageServer.new(files, vwh)
    end

    def pencils : PencilServer
      @fonts.pencils
    end

    def images : ImageServer
      @images
    end

    def layer_for(key : DrawKey) : Layer
      PvgPainter.layer_for(@fonts, @images, key)
    end
  end
end

require "./pvg/libplutovg"
require "./pvg/libplutosvg"
require "./pvg/pencil"
require "./pvg/painter"
require "./pvg/image_server"
