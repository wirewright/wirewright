require "crsfml"

require "./uiRb"
require "./sfml_util"

module UIR::Platform::SFML::FontLoader
  WEIGHTS = {
    {100, "Thin"},
    {200, "ExtraLight"},
    {300, "Light"},
    {400, "Regular"},
    {450, "Text"},
    {500, "Medium"},
    {600, "SemiBold"},
    {700, "Bold"},
    {800, "ExtraBold"},
    {900, "Black"},
  }

  # :nodoc:
  def self.refs(font : String, postfix : String) : Indexable(Path)
    {Path["fonts"] / "#{font.delete(' ')}-#{postfix}.ttf",
     Path["fonts"] / "#{font.delete(' ')}-#{postfix}.otf"}
  end

  # :nodoc:
  def self.path0?(font : String, weight pivot : Int32) : Path?
    WEIGHTS.reverse_each do |weight, postfix|
      next unless weight <= pivot

      refs(font, postfix).each do |ref|
        next unless File.exists?(ref)
        return ref
      end
    end

    WEIGHTS.each do |weight, postfix|
      break if weight <= pivot

      refs(font, postfix).each do |ref|
        next unless File.exists?(ref)
        return ref
      end
    end
  end

  @@paths = {} of {String, Int32} => Path?
  @@paths_lock = Mutex.new

  # Returns the path to *font* with the given *weight*.
  #
  # - If *weight* does not exist for *font* tries to fall back to lower values
  #   of *weight*.
  # - If still nothing, tries to fall back on higher values of *weight*.
  # - If still nothing, returns `nil`.
  def self.path?(font : String, weight : Int32) : Path?
    @@paths_lock.synchronize do
      @@paths.put_if_absent({font, weight}) { path0?(font, weight) }
    end
  end

  @@cache = {} of Path => {SF::Font, Mutex}
  @@cache_lock = Mutex.new

  def self.font_at(path : Path, &)
    font, lock = @@cache_lock.synchronize do
      @@cache.put_if_absent(path) do
        {SF::Font.from_file(path.to_s), Mutex.new}
      end
    end

    lock.synchronize { yield font }
  end
end

record UIR::Platform::SFML::TextMeasurer, font : SF::Font, size : Int32, leading : Float32, tracking : Float32 do
  def self.new(font, size)
    new(font, size, leading: 1.0, tracking: 1.0)
  end

  def glyph(char : Char)
    @font.get_glyph(char.ord, @size, bold: false)
  end

  def kerning(c1 : Char, c2 : Char)
    @font.get_kerning(c1.ord, c2.ord, @size, bold: false)
  end

  def line_spacing : Float32
    @font.get_line_spacing(@size)
  end

  def texture
    @font.get_texture(@size)
  end

  def wswidth0
    glyph(' ').advance
  end

  def wswidth
    wswidth0 + letter_spacing
  end

  def letter_spacing
    (wswidth0 / 3) * (tracking - 1)
  end

  def line_height
    (size * leading).ceil.to_i
  end

  def measure(string : String, *, window = 0...string.size)
    reader = Char::Reader.new(string, pos: string.char_index_to_byte_index(window.begin) || raise IndexError.new)

    width = 0
    state = '\0'

    window.each do
      current = reader.current_char

      width += kerning(state, current)
      state = current

      case current
      when ' ', '\n'
        width += wswidth
      when '\t'
        width += 4 * wswidth
      else
        glyph = glyph(current)
        width += glyph.advance + letter_spacing
      end

      break unless reader.has_next?

      reader.next_char
    end

    width.ceil.to_i
  end

  def_equals_and_hash @sf, @size, @leading, @tracking
end

record UIR::Platform::SFML::TextInfo, font : SF::Font, size : Int32, leading : Float32 do
  def self.from(term : Term, &)
    Term.case(term) do
      matchpi %[{¦ font_string weight_: (%any 100 200 300 400 450 500 600 700 800 900) size_: (%number u8) leading_number}] do
        next unless path = FontLoader.path?(font.to(String), weight.to(Int32))

        FontLoader.font_at(path) do |font|
          yield new(font, size.to(Int32), leading.to(Float32))
        end
      end

      otherwise { }
    end
  end
end

module UIR::Platform::SFML
  extend self
  extend IPlatform

  def measure(content : String, font : String, weight : Int32, size : Int32, leading : Float32) : {Int32, Int32}
    unless path = FontLoader.path?(font, weight)
      return 0, 0
    end

    FontLoader.font_at(path) do |font|
      measurer = TextMeasurer.new(font, size, leading, tracking: 1.0f32)

      if content.empty?
        return 0, measurer.line_height
      end

      width = 0
      height = 0

      content.each_line(chomp: true) do |line|
        width = Math.max(width, measurer.measure(line))
        height += measurer.line_height
      end

      {width, height}
    end
  end

  # :nodoc:
  HAND = SF::Cursor.from_system(SF::Cursor::Type::Hand)
  # :nodoc:
  ARROW = SF::Cursor.from_system(SF::Cursor::Type::Arrow)

  private def color?(term : Term) : SF::Color?
    Term.case(term) do
      matchpi %{(r←(%number u8) g←(%number u8) b←(%number u8))} do
        SF::Color.new(r.to(UInt8), g.to(UInt8), b.to(UInt8))
      end

      otherwise { }
    end
  end

  # Manages and allows to `collapse` layers, each represented by SFML render textures.
  struct LayerManager
    # :nodoc:
    record LayerData, x : Int32, y : Int32, z : Int32, target : SF::RenderTexture

    @layers = [] of LayerData

    # If absent, allocates an absolutely positioned (*x*, *y*), *z*-th layer of width *w* and
    # height *h*. If the layer already exists, does nothing.
    #
    # Layers with higher *z* are drawn on top of those with a lower *z*.
    def create(z : Int32, *, x : Int32, y : Int32, w : Int32, h : Int32, bg = SF::Color::Transparent, clip : SF::FloatRect? = nil) : Nil
      index = @layers.bsearch_index { |other| other.z >= z }

      if index && (other = @layers[index]?)
        return if z == other.z # Already exists
      end

      index ||= @layers.size

      target = SF::RenderTexture.new(w, h)

      if clip
        target.view = SF::View.new(clip)
      end

      target.clear(bg)

      @layers.insert(index, LayerData.new(x, y, z, target))
    end

    # Draws an SFML drawable *sf* on *z*-th layer.
    #
    # Raises `ArgumentError` if *z*-th layer was not `create`d.
    def draw(z : Int32, sf : SF::Drawable) : Nil
      layer = @layers.bsearch { |layer| layer.z >= z }

      unless layer && layer.z == z
        raise ArgumentError.new("layer #{z} was not create()'d")
      end

      layer.target.draw(sf)
    end

    # Draws all layers on the *primary* layer and returns the resulting texture.
    def collapse(primary : Int32) : SF::Texture
      base = @layers[primary].target

      @layers.each do |layer|
        next if layer.z == primary

        layer.target.display

        sprite = SF::Sprite.new(layer.target.texture)
        sprite.position = SF.vector2i(layer.x, layer.y)
        base.draw(sprite)
      end

      base.display
      base.texture
    end
  end

  def render(vote_cursor, layers, layer, frame : Term, dl, dt)
    Term.case(frame) do
      # Any node can specify the cursor.
      matchpi %[{¦ cursor: pointer}] do
        vote_cursor.call(HAND)

        continue
      end

      matchpi %[(text caption_string ¦ rest_ color_ l_: (%number i32) t_: (%number i32))] do
        TextInfo.from(rest) do |info|
          sf = SF::Text.new(caption.to(String), info.font, info.size)
          sf.line_spacing = info.leading
          sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)
          sf.color = color?(color) || SF::Color::Black
          sf.letter_spacing = 1

          layers.draw(layer, sf)
        end
      end

      matchpi(
        %[(rect ¦ _ bg_
                    l_: (%number i32)
                    t_: (%number i32)
                    final-w: w←(%number +i32)
                    final-h: h←(%number +i32)
                    border-radius: (%optional 0 border_radius←(%number (whole _) >= 0))
                    ring-l: (%optional 0 ring-l←(%number (whole _) >= 0))
                    ring-r: (%optional 0 ring-r←(%number (whole _) >= 0))
                    ring-t: (%optional 0 ring-t←(%number (whole _) >= 0))
                    ring-b: (%optional 0 ring-b←(%number (whole _) >= 0)))]
      ) do |bg|
        if border_radius.unsafe_as_n > 0
          sf = SF::RoundedRectangleShape.new
          sf.border_radius = border_radius.to(Float64)
          sf = sf.as(SF::Rectangular)
        else
          sf = SF::RectangleShape.new.as(SF::Rectangular)
        end

        sf.fill_color = color?(bg) || SF::Color::Transparent
        sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)
        sf.size = SF.vector2i(w.to(Int32), h.to(Int32))

        # Ring works like padding but it's intrinsic to the rect, and not accounted
        # during sizing.
        sf.position -= SF.vector2i(ring_l.to(Int32), ring_t.to(Int32))
        sf.size = sf.size.to_i + SF.vector2i(ring_l.to(Int32) + ring_r.to(Int32), ring_t.to(Int32) + ring_b.to(Int32))

        sf.update

        layers.draw(layer, sf)
      end

      matchpi(
        %[(rect/outline ¦ _ bg_
                            l_: (%number i32)
                            t_: (%number i32)
                            final-w: w←(%number +i32)
                            final-h: h←(%number +i32)
                            border-width: thickness←(%number +i32)
                            border-radius: (%optional 0 border_radius←(%number (whole _) >= 0)))]
      ) do |thickness|
        thickness = thickness.to(Int32)

        if border_radius.unsafe_as_n > 0
          sf = SF::RoundedRectangleShape.new
          sf.border_radius = border_radius.to(Float64)
          sf = sf.as(SF::Rectangular)
        else
          sf = SF::RectangleShape.new.as(SF::Rectangular)
        end

        sf.outline_thickness = thickness
        sf.outline_color = color?(bg) || SF::Color::Transparent
        sf.fill_color = SF::Color::Transparent

        # We're border-box by default! And only border-box!
        sf.position = SF.vector2i(l.to(Int32) + dl + thickness, t.to(Int32) + dt + thickness)
        sf.size = SF.vector2i(w.to(Int32) - thickness*2, h.to(Int32) - thickness*2)

        sf.update

        layers.draw(layer, sf)
      end

      matchpi %[(circle ¦ _ bg_ l_: (%number i32) t_: (%number i32) radius_: (%number +i32))] do |bg|
        sf = SF::CircleShape.new(radius.to(Int32))
        sf.fill_color = color?(bg) || SF::Color::Transparent
        sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)

        layers.draw(layer, sf)
      end

      matchpi(
        %[(triangle ¦ _ bg_
                        l_: (%number i32)
                        t_: (%number i32)
                        final-w: w←(%number +i32)
                        final-h: h←(%number +i32)
                        pointing: left)]
      ) do |bg|
        sf = SF::ConvexShape.new
        sf.point_count = 3
        sf[0] = SF.vector2i(0, h.to(Int32) // 2)
        sf[1] = SF.vector2i(w.to(Int32), 0)
        sf[2] = SF.vector2i(w.to(Int32), h.to(Int32))
        sf.fill_color = color?(bg) || SF::Color::Transparent
        sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)

        layers.draw(layer, sf)
      end

      matchpi(
        %{(viewport child←{¦ final-w: full-w←(%number +i32)
                             final-h: full-h←(%number +i32)}
           ¦ _ bg_
               l_: (%number +i32)
               t_: (%number +i32)
               final-w: w←(%number +i32)
               final-h: h←(%number +i32)
               x_: (%number i32)
               y_: (%number i32))}
      ) do
        manager = LayerManager.new
        manager.create(0,
          x: l.to(Int32) + dl,
          y: t.to(Int32) + dt,
          w: Math.max(full_w.to(Int32), w.to(Int32)),
          h: Math.max(full_h.to(Int32), h.to(Int32)),
          bg: color?(bg) || SF::Color::White,
          clip: SF.float_rect(x.to(Int32), y.to(Int32), w.to(Int32), h.to(Int32)),
        )

        render(vote_cursor, manager, 0, child, -l.to(Int32), -t.to(Int32))

        texture = manager.collapse(0)

        sf = SF::Sprite.new(texture)
        sf.position = SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt)

        layers.draw(layer, sf)
      end

      matchpi(
        %{(layer child_ ¦ _ l_: (%number +i32)
                            t_: (%number +i32)
                            final-w: w←(%number +i32)
                            final-h: h←(%number +i32)
                            z-index: n←(%number +i32))}
      ) do
        return if w.zero? || h.zero?

        layers.create(n.to(Int32), x: l.to(Int32) + dl, y: t.to(Int32) + dt, w: w.to(Int32), h: h.to(Int32))

        render(vote_cursor, layers, n.to(Int32), child, dl: -l.to(Int32), dt: -t.to(Int32))
      end

      matchpi %[_dict] do
        frame.items.each { |child| render(vote_cursor, layers, layer, child, dl, dt) }
      end

      otherwise { }
    end
  end

  def render?(tree : Term) : {SF::Cursor, SF::Texture}?
    Term.case(tree) do
      matchpi %{(window child_ ¦ _ bg_ final-w: w←(%number +i32) final-h: h←(%number +i32))} do
        layers = LayerManager.new
        layers.create(0, x: 0, y: 0, w: w.to(Int32), h: h.to(Int32), bg: color?(bg) || SF::Color::White)

        cursor = ARROW
        vote_cursor = ->(proposal : SF::Cursor) do
          return if cursor != ARROW && proposal == ARROW

          cursor = proposal
        end

        render(vote_cursor, layers, 0, child, 0, 0)

        {cursor, layers.collapse(0)}
      end

      otherwise { }
    end
  end

  def show(reducer : Reducer) : Nil
    drawable = reducer.call(Term.of, Term.of(:open))

    Term.case(drawable) do
      matchpi %[(window _ ¦ _ title⋮ "Untitled" final-w: w←(%number +i32) final-h: h←(%number +i32))] do
        window = SF::RenderWindow.new(SF::VideoMode.new(w.to(Int32), h.to(Int32)), title: title.to(String), settings: SF::ContextSettings.new(depth: 24, antialiasing: 8))
        window.framerate_limit = 60

        while window.open?
          while event = window.poll_event
            transcribed = nil

            case event
            when SF::Event::Closed
              window.close

              transcribed = Term.of(:exit)
            when SF::Event::Resized
              window.view = SF::View.new(SF.float_rect(0, 0, event.width, event.height))

              transcribed = Term.of(:size, event.width, event.height)
            when SF::Event::TextEntered
              chr = event.unicode.chr
              next unless chr.printable?

              transcribed = Term.of(:input, chr)
            when SF::Event::MouseButtonPressed
              transcribed = Term.of(:"mouse-press", event.x, event.y)
            when SF::Event::MouseButtonReleased
              transcribed = Term.of(:"mouse-release", event.x, event.y)
            when SF::Event::MouseMoved
              transcribed = Term.of(:motion, event.x, event.y)
            when SF::Event::KeyPressed
              keyname = nil
              case event.code
              when .escape?    then keyname = "escape"
              when .tab?       then keyname = "tab"
              when .home?      then keyname = "home"
              when .end?       then keyname = "end"
              when .enter?     then keyname = "enter"
              when .delete?    then keyname = "delete"
              when .left?      then keyname = "left"
              when .right?     then keyname = "right"
              when .up?        then keyname = "up"
              when .down?      then keyname = "down"
              when .backspace? then keyname = "backspace"
              when .numpad2?   then keyname = "np2"
              when .numpad4?   then keyname = "np4"
              when .numpad5?   then keyname = "np5"
              when .numpad6?   then keyname = "np6"
              when .numpad8?   then keyname = "np8"
              end

              if event.control
                case event.code
                when .c? then keyname = "c"
                when .v? then keyname = "v"
                end
              end

              next unless keyname

              keyname = "S-#{keyname}" if event.shift
              keyname = "C-#{keyname}" if event.control
              key = Term::Sym.new(keyname)

              transcribed = Term.of(:key, key)
            end

            if transcribed
              drawable = reducer.call(drawable, transcribed)
            end
          end

          drawable = reducer.call(drawable, Term.of(:cycle))

          window.clear(SF::Color::White)

          if render = render?(drawable)
            cursor, texture = render
            window.mouse_cursor = cursor
            sprite = SF::Sprite.new(texture)
            window.draw(sprite)
          end

          window.display
        end
      end
    end
  end
end
