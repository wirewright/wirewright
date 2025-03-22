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

class UIR::Platform::SFML::TextData
  getter size

  def initialize(@font : SF::Font, @size : Int32, @leading = 1.0f32, @tracking = 1.0f32)
  end

  def glyph(chr : Char)
    @font.get_glyph(chr.ord, @size, bold: false)
  end

  def advance(chr : Char)
    glyph(chr).advance
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

  private def wswidth0
    glyph(' ').advance
  end

  def wswidth : Int32
    (wswidth0 + letter_spacing).to_i
  end

  def letter_spacing
    (wswidth0 / 3) * (@tracking - 1)
  end

  def line_height
    (@size * @leading).ceil.to_i
  end

  def wsstep : SF::Vector2i
    SF.vector2i(wswidth, 0)
  end

  def tabstep : SF::Vector2i
    wsstep * 4
  end

  def nlstep
    SF.vector2i(0, line_height)
  end

  def glyphstep(state : Char, chr : Char)
    SF.vector2i((kerning(state, chr) + advance(chr) + letter_spacing).to_i, 0)
  end
end

# Allows to specify bounds for text wrapping.
record UIR::Platform::SFML::WrapBounds, position : SF::Vector2i, width : Int32?, height : Int32? do
  def includes?(rect : SF::IntRect) : Bool
    right = @width.try { |width| @position.x + width }

    return false unless rect.left.in?(@position.x...right)
    return false unless rect.right.in?(@position.x...right)

    bottom = @height.try { |height| @position.y + height }

    return false unless rect.top.in?(@position.y...bottom)
    return false unless rect.bottom.in?(@position.y...bottom)

    true
  end
end

# An immutable "cursor" appears to me a great analogy when implementing text
# wrapping. It greatly simplifies the code.
struct UIR::Platform::SFML::TextCursor
  # Returns the position of the top-left corner of this cursor.
  getter position : SF::Vector2i

  def initialize(@text : TextData, @left : Int32, @position : SF::Vector2i, @state : Char)
  end

  record WrapContext, bounds : WrapBounds, ellipsis : String

  private def self.commit?(ctx : WrapContext, cursor : self) : self?
    ctx.bounds.includes?(cursor.after(ctx.ellipsis).bounds) ? cursor : nil
  end

  private def self.commit?(ctx : WrapContext, cursor cursor0 : self, *args : String | Char) : self?
    commit?(ctx, args.reduce(cursor0) { |cursor, arg| cursor.after(arg) })
  end

  private def self.wrap?(ctx : WrapContext, io, cursor0 : self, word : String, ws : String) : self?
    # Path 1: Type the entire word.
    if cursor1 = commit?(ctx, cursor0, ws, word)
      io << ws << word
      return cursor1
    end

    # Path 2: The entire word cannot be committed. See if soft break can
    # be committed followed by the entire word (i.e. on its own line).
    if (cursor0.position.x > 0 || cursor0.position.y > 0) && (cursor1 = commit?(ctx, cursor0, '\n', word))
      io << '\n' << word
      return cursor1
    end

    # Path 3: Try typing the word letter-by-letter.
    word.each_char_with_index do |letter, index|
      # Path 3.1: the letter can be committed to the current line.
      if cursor1 = commit?(ctx, cursor0, index.zero? ? ws : "", letter)
        io << ws if index.zero?
        io << letter
        cursor0 = cursor1
        next
      end

      # Path 3.2: the letter can be committed after a soft break.
      if cursor1 = commit?(ctx, cursor0, '\n', letter)
        io << '\n' << letter
        cursor0 = cursor1
        next
      end

      # Path 3.3: the letter cannot be committed. Type ellipsis.
      io << ctx.ellipsis

      return # Returning nil signals that we're done.
    end

    cursor0
  end

  # Wraps *string* to fit in *bounds* according to text data *text*. Returns
  # wrapped *string* (i.e. with appropriately placed newlines).
  def self.wrap(text : TextData, string : String, bounds : WrapBounds, ellipsis = "...") : String
    String.build do |io|
      ctx = WrapContext.new(bounds, ellipsis)
      index = 0
      cursor = new(text, bounds.position.x, bounds.position, state: '\0')

      string.split(' ', remove_empty: true) do |word|
        ws = index > 0 && cursor.position.x > 0 ? " " : ""
        cursor = wrap?(ctx, io, cursor, word, ws)
        index += 1

        break unless cursor
      end
    end
  end

  # Returns the bounding box of this cursor.
  def bounds : SF::IntRect
    SF.int_rect(@position.x, @position.y, 1, @text.line_height)
  end

  private def_change

  # Returns a copy of this cursor after typing *chr*.
  def after(chr : Char) : self
    case chr
    when '\r'
      self
    when ' '
      change(position: @position + @text.wsstep, state: chr)
    when '\t'
      change(position: @position + @text.wsstep*4, state: chr)
    when '\n'
      change(position: SF.vector2i(@left, @position.y) + @text.nlstep, state: chr)
    else
      change(position: @position + @text.glyphstep(@state, chr), state: chr)
    end
  end

  # Returns a copy of this cursor after typing *string*.
  def after(string : String) : self
    cursor = self
    string.each_char do |chr|
      cursor = cursor.after(chr)
    end
    cursor
  end
end

module UIR::Platform::SFML
  extend self
  extend IPlatform

  def wrap(content : String, font : String, weight : Int32, size : Int32, leading : Float32, *, w : Int32? = nil, h : Int32? = nil) : String
    unless path = FontLoader.path?(font, weight)
      return ""
    end

    FontLoader.font_at(path) do |font|
      text = TextData.new(font, size, leading, tracking: 1.0f32)

      TextCursor.wrap(text, content, WrapBounds.new(SF.vector2i(0, 0), w, h))
    end
  end

  def measure(text : TextData, string : String, *, window = 0...string.size) : Int32
    reader = Char::Reader.new(string, pos: string.char_index_to_byte_index(window.begin) || raise IndexError.new)

    width = 0
    state = '\0'

    window.each do
      current = reader.current_char

      case current
      when '\r'
      when ' ', '\n'
        width += text.wsstep.x
      when '\t'
        width += text.tabstep.x
      else
        width += text.glyphstep(state, current).x
      end

      state = current

      break unless reader.has_next?

      reader.next_char
    end

    width.ceil.to_i
  end

  def measure(content : String, font : String, weight : Int32, size : Int32, leading : Float32) : {Int32, Int32}
    unless path = FontLoader.path?(font, weight)
      return 0, 0
    end

    FontLoader.font_at(path) do |font|
      text = TextData.new(font, size, leading, tracking: 1.0f32)

      if content.empty?
        return 0, text.line_height
      end

      width = 0
      height = 0

      content.each_line(chomp: true) do |line|
        width = Math.max(width, measure(text, line))
        height += text.line_height
      end

      {width, height}
    end
  end

  # :nodoc:
  HAND = SF::Cursor.from_system(SF::Cursor::Type::Hand)
  # :nodoc:
  ARROW = SF::Cursor.from_system(SF::Cursor::Type::Arrow)
  # :nodoc:
  SIZEALL = SF::Cursor.from_system(SF::Cursor::Type::SizeAll)

  private def color?(term : Term, alpha = Term[255]) : SF::Color?
    Term.case({term, alpha}) do
      givenpi %{(r←(%number u8) g←(%number u8) b←(%number u8)) a←(%number u8)} do
        SF::Color.new(r.to(UInt8), g.to(UInt8), b.to(UInt8), a.to(UInt8))
      end

      otherwise { }
    end
  end

  # Manages and allows to `collapse` layers, each represented by an SFML
  # render texture.
  struct LayerManager
    # :nodoc:
    record LayerData, x : Int32, y : Int32, z : Int32, target : SF::RenderTexture

    @layers = [] of LayerData

    # If absent, allocates an absolutely positioned (*x*, *y*), *z*-th layer of width *w* and
    # height *h*. If the layer already exists, does nothing.
    #
    # Layers with higher *z* are drawn on top of those with a lower *z*.
    def create(z : Int32, *, x : Int32, y : Int32, w : Int32, h : Int32, bg = SF::Color::Transparent) : Nil
      index = @layers.bsearch_index { |other| other.z >= z }

      if index && (other = @layers[index]?)
        return if z == other.z # Already exists
      end

      index ||= @layers.size

      target = SF::RenderTexture.new(w, h)
      target.clear(bg)

      @layers.insert(index, LayerData.new(x, y, z, target))
    end

    # Draws an SFML drawable *sf* on *z*-th layer.
    #
    # Raises `ArgumentError` if *z*-th layer was not `create`d.
    def draw(z : Int32, sf : SF::Drawable, *, states = nil) : Nil
      layer = @layers.bsearch { |layer| layer.z >= z }

      unless layer && layer.z == z
        raise ArgumentError.new("layer #{z} was not create()'d")
      end

      if states
        layer.target.draw(sf, states)
      else
        layer.target.draw(sf)
      end
    end

    def collapse(primary : Int32, clip : SF::IntRect? = nil) : SF::Texture
      l = r = t = b = 0

      @layers.each do |layer|
        w = layer.target.size.x
        h = layer.target.size.y

        l = Math.min(l, layer.x)
        r = Math.max(r, layer.x + w)
        t = Math.min(t, layer.y)
        b = Math.max(b, layer.y + h)
      end

      base = SF::RenderTexture.new(r - l, b - t)

      @layers.each do |layer|
        layer.target.display

        sprite = SF::Sprite.new(layer.target.texture)
        sprite.position = SF.vector2i(layer.x, layer.y)
        base.draw(sprite)
      end

      base.display

      clip ||= SF.int_rect(SF.vector2i(0, 0), @layers[primary].target.size)

      clipped = SF::RenderTexture.new(clip.width, clip.height)
      sprite = SF::Sprite.new(base.texture)
      sprite.position = -clip.position
      clipped.draw(sprite)
      clipped.display
      clipped.texture
    end
  end

  def render_text(text : TextData, string : String, target : SF::VertexArray, offset offset0 = SF.vector2i(0, 0), color = SF::Color::Black)
    state = '\0'

    offset = offset0
    voffset = SF.vector2i(0, text.size)

    string.each_char_with_index do |chr|
      glyph = text.glyph(chr)

      case chr
      when '\r'
        next
      when ' '
        offset += text.wsstep
      when '\t'
        offset += text.wsstep*4
      when '\n'
        offset = SF.vector2i(offset0.x, offset.y) + text.nlstep
      else
        offset += SF.vector2(text.kerning(state, chr), 0).to_i

        target.append SF::Vertex.new(offset + voffset + glyph.bounds.top_left, color, glyph.texture_rect.top_left)
        target.append SF::Vertex.new(offset + voffset + glyph.bounds.top_right, color, glyph.texture_rect.top_right)
        target.append SF::Vertex.new(offset + voffset + glyph.bounds.bottom_right, color, glyph.texture_rect.bottom_right)
        target.append SF::Vertex.new(offset + voffset + glyph.bounds.bottom_left, color, glyph.texture_rect.bottom_left)

        offset += SF.vector2f(glyph.advance + text.letter_spacing, 0).to_i
      end
    ensure
      state = chr
    end
  end

  def render_text(text : TextData, string : String, **kwargs)
    va = SF::VertexArray.new(SF::Quads)
    render_text(text, string, va, **kwargs)
    va
  end

  def render(vote_cursor, layers, layer, frame : Term, dl, dt)
    Term.case(frame) do
      # Any node can specify the cursor.
      matchpi %[{¦ cursor_symbol}] do
        vote_cursor.call(cursor.unsafe_as_sym)

        continue
      end

      matchpi(
        %[(text caption_string ¦ rest_ color_ font_string leading_number
                                 weight_: (%any 100 200 300 400 450 500 600 700 800 900)
                                 size_: (%number u8)
                                 final-w: w←(%number +i32)
                                 final-h: h←(%number i32)
                                 l_: (%number i32)
                                 t_: (%number i32))]
      ) do
        continue unless path = FontLoader.path?(font.to(String), weight.to(Int32))

        FontLoader.font_at(path) do |font|
          text = TextData.new(font, size.to(Int32), leading.to(Float32), tracking: 1.0f32)

          sf = render_text(text, caption.to(String), offset: SF.vector2i(l.to(Int32) + dl, t.to(Int32) + dt), color: color?(color) || SF::Color::Black)
          layers.draw(layer, sf, states: SF::RenderStates.new(text.texture))
        end
      end

      matchpi(
        %[(rect ¦ _ bg_
                    alpha: (%optional 255 alpha←(%number u8))
                    l_: (%number i32)
                    t_: (%number i32)
                    final-w: w←(%number +i32)
                    final-h: h←(%number +i32)
                    border-radius: (%optional 0 border-radius←(%number (whole _) >= 0))
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

        sf.fill_color = color?(bg, alpha) || SF::Color::Transparent
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
                            border-radius: (%optional 0 border-radius←(%number (whole _) >= 0)))]
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
          x: 0,
          y: 0,
          w: Math.max(full_w.to(Int32), w.to(Int32)),
          h: Math.max(full_h.to(Int32), h.to(Int32)),
          bg: color?(bg) || SF::Color::White,
        )

        render(vote_cursor, manager, 0, child, -l.to(Int32), -t.to(Int32))

        texture = manager.collapse(0, clip: SF.int_rect(x.to(Int32), y.to(Int32), w.to(Int32), h.to(Int32)))

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
      matchpi %{(window child_ ¦ _ bg_ final-w: w←(%number +i32) final-h: h←(%number +i32) cursor⋮ arrow)} do
        layers = LayerManager.new
        layers.create(0, x: 0, y: 0, w: w.to(Int32), h: h.to(Int32), bg: color?(bg) || SF::Color::White)

        selected_cursor = ARROW
        vote_cursor = ->(proposal : Term::Sym) do
          return unless selected_cursor == ARROW

          case proposal
          when Term[:pointer]
            selected_cursor = HAND
          when Term[:grabbing]
            selected_cursor = SIZEALL
          end
        end
        vote_cursor.call(cursor.unsafe_as_sym)

        render(vote_cursor, layers, 0, child, 0, 0)

        {selected_cursor, layers.collapse(0)}
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
            transcribed = [] of Term

            case event
            when SF::Event::Closed
              window.close

              transcribed << Term.of(:exit)
            when SF::Event::Resized
              window.view = SF::View.new(SF.float_rect(0, 0, event.width, event.height))

              transcribed << Term.of(:size, event.width, event.height)
            when SF::Event::TextEntered
              chr = event.unicode.chr
              next unless chr.printable?

              transcribed << Term.of(:input, chr)
            when SF::Event::MouseButtonPressed
              transcribed << Term.of(:mouse, :motion, event.x, event.y) << Term.of(:mouse, :press)
            when SF::Event::MouseButtonReleased
              transcribed << Term.of(:mouse, :motion, event.x, event.y) << Term.of(:mouse, :release)
            when SF::Event::MouseMoved
              transcribed << Term.of(:mouse, :motion, event.x, event.y)
            when SF::Event::KeyPressed
              keyname = nil
              case event.code
              when .f1?        then keyname = "f1"
              when .f2?        then keyname = "f2"
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

              transcribed << Term.of(:key, key)
            end

            transcribed.each do |term|
              drawable = reducer.call(drawable, term)
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
