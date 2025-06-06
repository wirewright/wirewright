module Ww::Soma::DwUIR
  # An implementation of `Platform` using PlutoVG.
  #
  # Reference: https://github.com/sammycage/plutovg
  struct PvgPlatform
    include Platform

    def initialize
      @fonts = PvgFontFaceStore.new
    end

    def pencils : (PencilRequest -> IPencil)
      @fonts.pencils
    end

    def layer_for(resources : ResourceLoader, key : DrawKey) : Layer
      PvgPainter.layer_for(@fonts, resources, key)
    end
  end
end

require "./pvg/libplutovg"
require "./pvg/libplutosvg"
require "./pvg/pencil"
require "./pvg/painter"
