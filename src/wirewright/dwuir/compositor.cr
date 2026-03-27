module Ww::DwUIR
  # A compositor manages `Layer`s produced by a painter, and finally blends them
  # back-to-front over the destination pixel rect.
  class Compositor
    Log = ::Log.for(self)

    def initialize
      @curr = {} of DrawKey => Layer
      @succ = {} of DrawKey => Layer
    end

    # :nodoc:
    def layer_for(key : DrawKey, & : -> Layer) : Layer
      @succ[key] = @curr[key]? || yield
    end

    # :nodoc:
    def submit : Nil
      @curr, @succ = @succ, @curr
      @succ.clear

      Log.debug { "raster cache size is #{@curr.sum { |_, v| v.bytesize }.humanize_bytes} (#{@curr.sum { |_, v| v.fullsize }.humanize_bytes} unpacked)" }
    end

    # Composites *picture* over *dst*, thereby modifying *dst*.
    #
    # *clip* gives the width and height of the clip rect; its origin is implicitly
    # at absolute 0; 0. Callers would normally provide *dst* as their screen or
    # viewport pixel rect, and *clip* as their screen or viewport width & height.
    #
    # *dmg* should give the damage rect in absolute coordinates. Only commands that
    # intersect with the damage rect are going to be composited.
    def composite(dst : PixelRect, clip : {Int32, Int32}, picture : Picture, dmg : Rect) : Nil
      picture.each_command do |member|
        composite(dst, clip, member, dmg)
      end
    end

    # A version of `composite` for callers that do not compute a damage rect.
    #
    # This method simply sets the damage rect equal to *clip* rect (normally
    # that would be the entire screen or viewport).
    def composite(dst : PixelRect, clip : {Int32, Int32}, picture : Picture) : Nil
      composite(dst, clip, picture, Rect[0, 0, *clip])
    end

    # TODO: concentrate all blending function in the Compositor. Currently we have
    # Pixel, composite(), and PixelRect all doing blending in one way or another.
    # This f**ks up any optimization efforts due to spread.

    private def composite(dst, clip, command : DrawShape, dmg : Rect)
      return if Rect.xsect(command.dmgbounds, dmg).empty? # not damaged

      x, y, _, _ = command.tfbounds.round.ixywh

      # TODO: this is extremely slow & barely useful beyond a certain point.
      #
      # We need Layer to give us PixelGroup-s for parallelism (beyond
      # a certain layer size threshold). We need PixelGroup-s to consist
      # of PixelRow's for SIMD (e.g. as in https://github.com/WojciechMula/toys/blob/1863bfd2b139ecbf4f1c9ae3fc018767c143ed8a/blend_32bpp/blend_32bpp.c#L184)
      # Each core should receive a pixel group; and blend the pixel rows it consists
      # of with SIMD.
      layer = @curr[command.key]
      layer.each_pixel_with_coords do |pixel, i, j|
        next unless 0 <= x + i < clip.x
        next unless 0 <= y + j < clip.y
        next unless command.views.all?(&.includes?(Point[x + i, y + j]))

        dst.blend(x + i, y + j, pixel)
      end
    end

    private def composite(dst, clip, command : DrawComposite, dmg : Rect)
      return if Rect.xsect(command.dmgbounds, dmg).empty? # not damaged

      dst1 = PixelRect.new(*command.tfbounds.round.ixywh)

      composite(dst1, clip, command.picture, dmg)

      dst1.blend_over(dst, command.opacity)
    end
  end
end
