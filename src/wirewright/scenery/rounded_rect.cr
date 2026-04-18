module Ww::Scenery
  # Represents a rounded rectangle.
  struct RoundedRect
    alias Radii = {tl: Magnitude, tr: Magnitude, bl: Magnitude, br: Magnitude}

    # Returns the bounding rect.
    getter bounds : Rect

    # Returns the radii.
    getter radii : Radii

    def initialize(@bounds : Rect, @radii : Radii)
    end

    def self.new(bounds : Rect, radius : Magnitude) : RoundedRect
      max_radius = Math.min(bounds.size.x, bounds.size.y)/2
      radius = radius.clamp(Magnitude.new(0)..max_radius)
      radii = {tl: radius, tr: radius, bl: radius, br: radius}

      new(bounds, radii)
    end

    def self.new(bounds : Rect, radii : RectRadii) : RoundedRect
      max_radius = Math.min(bounds.size.x, bounds.size.y)/2
      radii = {tl: radii.tl.resolve(max_radius).clamp(Magnitude.new(0)..max_radius),
               tr: radii.tr.resolve(max_radius).clamp(Magnitude.new(0)..max_radius),
               bl: radii.bl.resolve(max_radius).clamp(Magnitude.new(0)..max_radius),
               br: radii.br.resolve(max_radius).clamp(Magnitude.new(0)..max_radius)}

      new(bounds, radii)
    end

    # Snaps this rounded rect's bounds to pixel coordinates.
    def snap : RoundedRect
      RoundedRect.new(@bounds.snap, @radii)
    end

    def tl : Magnitude
      radii[:tl]
    end

    def tr : Magnitude
      radii[:tr]
    end

    def bl : Magnitude
      radii[:bl]
    end

    def br : Magnitude
      radii[:br]
    end

    # Returns the top-left corner radius as a point.
    def tl2 : Point
      Point[tl, tl]
    end

    # Returns the top-right corner radius as a point.
    def tr2 : Point
      Point[tr, tr]
    end

    # Returns the bottom-left corner radius as a point.
    def bl2 : Point
      Point[bl, bl]
    end

    # Returns the bottom-right corner radius as a point.
    def br2 : Point
      Point[br, br]
    end

    # Calculates the A point for this rounded rectangle.
    #
    # ```text
    #         A                                 H
    #
    #         │                                 │
    #     ┌───┼─────────────────────────────────┼────┐
    #     │   │                                 │    │
    #     │                                          │
    # B ──┼──                                      ──┼── G
    #     │                                          │
    #     │                                          │
    #     │               rounded rect               │
    #     │                                          │
    #     │    note: the points will shift slightly  │
    #     │              based on radii              │
    #     │                                          │
    #     │                                          │
    # C ──┼──                                      ──┼── F
    #     │                                          │
    #     │   │                                 │    │
    #     └───┼─────────────────────────────────┼────┘
    #         │                                 │
    #
    #         D                                 E
    # ```
    def a : Point
      bounds.tl + Point[tl, 0]
    end

    # Calculates the B point for this rounded rectangle. See `a`.
    def b : Point
      bounds.tl + Point[0, tl]
    end

    # Calculates the C point for this rounded rectangle. See `a`.
    def c : Point
      bounds.bl - Point[0, bl]
    end

    # Calculates the D point for this rounded rectangle. See `a`.
    def d : Point
      bounds.bl + Point[bl, 0]
    end

    # Calculates the E point for this rounded rectangle. See `a`.
    def e : Point
      bounds.br - Point[br, 0]
    end

    # Calculates the F point for this rounded rectangle. See `a`.
    def f : Point
      bounds.br - Point[0, br]
    end

    # Calculates the G point for this rounded rectangle. See `a`.
    def g : Point
      bounds.tr + Point[0, tr]
    end

    # Calculates the H point for this rounded rectangle. See `a`.
    def h : Point
      bounds.tr - Point[tr, 0]
    end
  end
end
