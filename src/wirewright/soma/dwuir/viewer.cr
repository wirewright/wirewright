module Ww::Soma::DwUIR
  # A viewer object stores all the state necessary to "convert" or write a DwUIR
  # to a `PixelRect` called the *screen*. *You* provide the pixel rect along with
  # the other auxiliary objects. So you're free to do whatever you want with it,
  # before or after `show`ing your DwUIR.
  class Viewer
    @picture : Picture?

    def initialize(
      @screen : PixelRect,
      @compositor : Compositor,
      @platform : Platform,
      @resources : ResourceLoader,
    )
    end

    # :nodoc:
    def paint(key : DrawKey) : Nil
      @compositor.layer_for(key) { @platform.layer_for(@resources, key) }
    end

    # :nodoc:
    def paint(command : DrawShape) : Nil
      paint(command.key)
    end

    # :nodoc:
    def paint(command : DrawComposite) : Nil
      paint(command.picture)
    end

    # :nodoc:
    def paint(picture : Picture)
      picture.each_command { |command| paint(command) }
    end

    # Modifies the screen pixel rect to match *dwuir*.
    #
    # Returns the master damage rect (bounding box of all damage rectangles).
    def show(dwuir : Term, bg : Color, *, dmgdbg = false) : Array(Rect)
      picture1 = DwUIR.picture(dwuir, @platform.pencils)

      dmgbounds_and_dmgrects(picture1) do |dmgbounds, dmgrects|
        dmgcov = dmgbounds.size.compare(@screen.bounds.size)

        # If damage covers >= 70% of the screen, just do a full clear/redraw.
        if dmgdbg || (dmgcov.x >= 0.7 && dmgcov.y >= 0.7)
          @screen.fill(bg)
        else
          dmgrects.each do |rect|
            @screen.region(rect).fill(bg)
          end
        end

        paint(picture1)

        @compositor.submit
        @compositor.composite(@screen, {@screen.width, @screen.height}, picture1, dmgbounds)

        if dmgdbg || dmgrects.empty?
          return [@screen.bounds]
        end

        return dmgrects
      ensure
        @picture = picture1
      end

      [] of Rect
    end

    private def dmgbounds_and_dmgrects(picture1, &)
      unless picture0 = @picture
        yield @screen.bounds, [] of Rect
        return
      end

      rects = [] of Rect
      dmgbounds = nil

      picture1.damage(picture0) do |dmgrect|
        rects << dmgrect
        dmgbounds = dmgbounds ? dmgbounds.max(dmgrect) : dmgrect
      end

      # Nothing damaged, meaning there is no change between the pictures.
      return unless dmgbounds && !dmgbounds.empty?

      yield dmgbounds, rects
    end
  end
end
