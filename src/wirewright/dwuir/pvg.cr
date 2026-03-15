module Ww::DwUIR
  # Reference: https://github.com/sammycage/plutovg
  struct PvgPlatform
    def initialize
      @fonts = PvgFontFaceStore.new
    end

    def pencils : PencilServer
      @fonts.pencils
    end

    def layer_for(key : DrawKey) : Layer
      PvgPainter.layer_for(@fonts, key)
    end
  end
end

require "./pvg/pencil"
require "./pvg/painter"
