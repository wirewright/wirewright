require "wait_group"

record Point, x : Int32, y : Int32
record Rect, origin : Point, extent : Point

record Ring, l : UInt8, r : UInt8, t : UInt8, b : UInt8 do
  def x : UInt16
    l.to_u16 + r.to_u16
  end

  def y : UInt16
    t.to_u16 + b.to_u16
  end
end

record RGB, r : UInt8, g : UInt8, b : UInt8 do
  def self.parse(term : Term) : RGB
    Term.case(term) do
      matchpi(
        %{(r←(%number u8) g←(%number u8) b←(%number u8))},
        %{(rgb r←(%number u8) g←(%number u8) b←(%number u8))},
      ) do
        new(r.to(UInt8), g.to(UInt8), b.to(UInt8))
      end

      matchpi %{(oklch l←(%number 0 <= _ <= 1) c←(%number 0 <= _ <= 1) h←(%number 0 <= _ <= 360))} do
        new(*oklch(l.to(Float64), c.to(Float64), h.to(Float64)))
      end

      otherwise do
        new(0u8, 0u8, 0u8)
      end
    end
  end
end

enum Heading : UInt8
  Left
  Right
  Up
  Down

  def self.parse(term : Term)
    Term.case(term) do
      matchpi %{left} { Left }
      matchpi %{right} { Right }
      matchpi %{up} { Up }
      matchpi %{down} { Down }

      otherwise { Up }
    end
  end
end

enum FontWeight : UInt8
  Thin
  ExtraLight
  Light
  Regular
  Text
  Medium
  SemiBold
  Bold
  ExtraBold
  Black

  def self.parse(n : Int)
    weights = {100, 200, 300, 400, 450, 500, 600, 700, 800, 900}
    weight = weights.min_by { |weight| (weight - n).abs }
    new(weights.index!(weight).to_u8)
  end

  def self.each_with_name(& : FontWeight, String ->)
    values.each { |member| yield member, member.to_s }
  end

  def self.reverse_each_with_name(& : FontWeight, String ->)
    values.reverse_each { |member| yield member, member.to_s }
  end
end

abstract class DrawCommand
end

# Draws a string of text.
#
# - *z* is the z-index of the text.
# - *caption* specifies the string value for the text.
# - *font*, *size*, *weight*, *leading*, and *tracking* are font properties.
# - *color* sets the color of the text.
# - *origin* sets the position where the text should be drawn.
defcase FillText < DrawCommand, z : Int32,
  origin : Point,
  caption : String,
  font : String,
  size : UInt16,
  weight : FontWeight,
  leading : Float32,
  tracking : Float32,
  color : RGB

# Draws a filled rectangle.
#
# - *z* is the z-index of the rectangle.
# - *box* specifies its origin and extent.
# - *color* specifies its fill color.
# - *alpha* specifies the opacity (0 - transparent, 255 - opaque) of the fill.
# - *radius* specifies its corner radius (0 - square corners).
# - *ring* specifies its ring, which in CSS terms could be described as a fully
#   opaque box shadow. Setting to all-0 disables the ring.
defcase FillRect < DrawCommand,
  z : Int32,
  box : Rect,
  color : RGB,
  alpha : UInt8,
  radius : UInt16,
  ring : Ring

# Draws a rectangle outline.
#
# - *z* is the z-index of the outline.
# - *box* specifies its origin and extent.
# - *color* specifies its color.
# - *thickness* specifies how thick the outline is.
# - *radius* specifies corner radius (0 - square corners).
defcase OutlineRect < DrawCommand,
  z : Int32,
  box : Rect,
  color : RGB,
  thickness : UInt8,
  radius : UInt16

# Draws a filled circle.
#
# - *z* is the z-index of the circle.
# - *origin* specifies the location of its top-left corner.
# - *radius* is its radius.
# - *color* is the fill color.
defcase FillCircle < DrawCommand,
  z : Int32,
  origin : Point,
  radius : UInt16,
  color : RGB

# Draws a filled triangle.
#
# - *z* is the z-index of the triangle.
# - *box* specifies its bounding box.
# - *heading* specifies where the triangle points (left, right, etc.)
# - *color* specifies the fill color of the triangle.
defcase FillTriangle < DrawCommand,
  z : Int32,
  box : Rect,
  heading : Heading,
  color : RGB

# A limited view into the product of children draw commands *children*.
#
# - *z* is the z-index of the view itself. Its children may have different z-indices.
# - *box* is the bounding box of the view.
# - *color* is the clear-color of the view.
defcase View < DrawCommand,
  z : Int32,
  children : Array(DrawCommand),
  box : Rect,
  color : RGB

enum Cursor : UInt8
  Arrow
  Pointer
  Grabbing

  def self.parse(term : Term)
    Term.case(term) do
      matchpi %{arrow} { Arrow }
      matchpi %{pointer} { Pointer }
      matchpi %{grabbing} { Grabbing }

      otherwise { Arrow }
    end
  end
end

defcase Window,
  cursor : Cursor,
  size : Point,
  color : RGB,
  children : Array(DrawCommand)

# :nodoc:
record DrawContext, commands : Array(DrawCommand), setcursor : (Cursor ->)

# :nodoc:
#
# Appends draw commands associated with *node* to *ctx*.
#
# - *z* is the z-index of *node*.
def draw(ctx : DrawContext, node : Term, x : Int32, y : Int32, z : Int32) : Nil
  Term.case(node) do
    # Any node can specify the cursor.
    matchpi %[{¦ cursor_symbol}] do
      ctx.setcursor.call(Cursor.parse(cursor))

      continue
    end

    matchpi(
      %[(text caption_string ¦ _ color_ font_string leading_number
                               weight_: (%any 100 200 300 400 450 500 600 700 800 900)
                               size_: (%number u16)
                               dl_: (%number i32)
                               dt_: (%number i32))]
    ) do
      ctx.commands << FillText.new(z,
        origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
        caption: caption.to(String),
        font: font.to(String),
        size: size.to(UInt16),
        weight: FontWeight.parse(weight.to(Int32)),
        leading: leading.to(Float32),
        tracking: 1.0f32,
        color: RGB.parse(color),
      )
    end

    matchpi(
      %[(rect ¦ _ bg_
                  alpha: (%optional 255 alpha←(%number u8))
                  dl_: (%number i32)
                  dt_: (%number i32)
                  final-w: w←(%number +i32)
                  final-h: h←(%number +i32)
                  border-radius: (%optional 0 radius←(%number u16))
                  ring-l: (%optional 0 rl←(%number u8))
                  ring-r: (%optional 0 rr←(%number u8))
                  ring-t: (%optional 0 rt←(%number u8))
                  ring-b: (%optional 0 rb←(%number u8)))]
    ) do
      ctx.commands << FillRect.new(z,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
          extent: Point.new(w.to(Int32), h.to(Int32)),
        ),
        color: RGB.parse(bg),
        alpha: alpha.to(UInt8),
        radius: radius.to(UInt16),
        ring: Ring.new(rl.to(UInt8), rr.to(UInt8), rt.to(UInt8), rb.to(UInt8)),
      )
    end

    matchpi(
      %[(rect/outline ¦ _ bg_
                          dl_: (%number i32)
                          dt_: (%number i32)
                          final-w: w←(%number +i32)
                          final-h: h←(%number +i32)
                          border-width: thickness←(%number u8)
                          border-radius: (%optional 0 radius←(%number u16)))]
    ) do
      inset = thickness.to(Int32)

      ctx.commands << OutlineRect.new(z,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32) + inset, y + dt.to(Int32) + inset),
          extent: Point.new(w.to(Int32) - inset*2, h.to(Int32) - inset*2),
        ),
        color: RGB.parse(bg),
        thickness: inset.to_u8,
        radius: radius.to(UInt16),
      )
    end

    matchpi(
      %[(circle ¦ _ bg_
                    dl_: (%number i32)
                    dt_: (%number i32)
                    radius_: (%number u16))]
    ) do
      ctx.commands << FillCircle.new(z,
        origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
        radius: radius.to(UInt16),
        color: RGB.parse(bg),
      )
    end

    matchpi(
      %[(triangle ¦ _ bg_
                      dl_: (%number i32)
                      dt_: (%number i32)
                      final-w: w←(%number +i32)
                      final-h: h←(%number +i32)
                      pointing_: (%any left right up down))]
    ) do
      ctx.commands << FillTriangle.new(z,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
          extent: Point.new(w.to(Int32), h.to(Int32)),
        ),
        heading: Heading.parse(pointing),
        color: RGB.parse(bg),
      )
    end

    matchpi(
      %[(viewport child←{¦ final-w: cw←(%number +i32) final-h: ch←(%number +i32)}
         ¦ _ bg_
             dl_: (%number +i32)
             dt_: (%number +i32)
             final-w: w←(%number +i32)
             final-h: h←(%number +i32)
             pan-x_: (%number i32)
             pan-y_: (%number i32))]
    ) do
      children = [] of DrawCommand

      draw(ctx.copy_with(commands: children), child, pan_x.to(Int32), pan_y.to(Int32), z)

      # Sort children by layer (z-index) now that we know they're complete.
      #
      # Smaller z-index will be drawn on top of, so the default order (ASC) is fine.
      children.sort_by!(&.z)

      ctx.commands << View.new(z,
        children: children,
        box: Rect.new(
          origin: Point.new(x + dl.to(Int32), y + dt.to(Int32)),
          extent: Point.new(w.to(Int32), h.to(Int32)),
        ),
        color: RGB.parse(bg),
      )
    end

    matchpi(
      %[(layer child_ ¦ _ dl_: (%number i32)
                          dt_: (%number i32)
                          z-index: n←(%number +i32))]
    ) do
      draw(ctx, child, x + dl.to(Int32), y + dt.to(Int32), z: n.to(Int32))
    end

    matchpi %[{¦ dl_: (%number i32) dt_: (%number i32)}] do
      node.items.each { |child| draw(ctx, child, x + dl.to(Int32), y + dt.to(Int32), z) }
    end

    otherwise { }
  end
end

# Converts uiR *markup* into a `Window` object. This object, among other
# things, contains an array of draw commands to be executed by a Painter
# to actually paint the window on the screen.
def draw(markup : Term) : Window
  Term.case(markup) do
    matchpi %{(window child_ ¦ _ bg_ final-w: w←(%number +i32) final-h: h←(%number +i32) cursor⋮ arrow)} do
      children = [] of DrawCommand
      cursor0 = Cursor.parse(cursor)

      setcursor = ->(proposal : Cursor) do
        # Only allow changing Arrow to any cursor.
        case {cursor0, proposal}
        when {Cursor::Arrow, _}
          cursor0 = proposal
        end
      end

      draw(DrawContext.new(children, setcursor), child, x: 0, y: 0, z: 0)

      # Sort children by layer (z-index) now that we know they're complete.
      #
      # Smaller z-index will be drawn on top of, so the default order (ASC) is fine.
      children.sort_by!(&.z)

      Window.new(cursor0, Point.new(w.to(Int32), h.to(Int32)), RGB.parse(bg), children)
    end

    # TODO: if markup is invalid, display an error window.
  end
end

# ----

require "crsfml"
require "./sfml_util"

module UIR::Platform::SFML
  extend IPlatform
  extend self

  MAX_ANTIALIASING = Lock.synchronize { SF::RenderTexture.maximum_antialiasing_level }

  # FIXME: how to support nested viewports?
  class Scratchpad
    getter surface : SF::RenderTexture

    def initialize(*, w = 1024, h = 1024)
      @surface = SF::RenderTexture.new(w, h, SF::ContextSettings.new(depth: 24, antialiasing: MAX_ANTIALIASING))
      @locked = false
    end

    def fit(w : Int32, h : Int32, & : SF::RenderTexture ->) : Nil
      if @locked
        raise "nested viewport not supported: the surface is locked!"
      end

      if @surface.size.x < w || @surface.size.y < h
        @surface = SF::RenderTexture.new(w, h)
      end

      @locked = true

      begin
        yield @surface
      ensure
        @locked = false
      end
    end
  end

  # Font finder can locate a font on disk based on its name (e.g. "IBM Plex Sans")
  # and weight (e.g. `FontWeight::Bold`). The result of font finder's work is an
  # absolute path to the font file (TTF or OTF).
  module FontFinder
    extend self

    # :nodoc:
    def refs(font : String, postfix : String) : Indexable(Path)
      {RESOURCES / "fonts" / "#{font.delete(' ')}-#{postfix}.ttf",
       RESOURCES / "fonts" / "#{font.delete(' ')}-#{postfix}.otf"}
    end

    # :nodoc:
    def path0?(font : String, weight pivot : FontWeight) : Path?
      FontWeight.reverse_each_with_name do |weight, postfix|
        next unless weight <= pivot

        refs(font, postfix).each do |ref|
          next unless File.exists?(ref)
          return ref
        end
      end

      FontWeight.each_with_name do |weight, postfix|
        refs(font, postfix).each do |ref|
          next unless File.exists?(ref)
          return ref
        end
      end
    end

    @@cache = {} of {String, FontWeight} => Path?

    # Returns the path to *font* with the given *weight*.
    #
    # - If *weight* does not exist for *font* tries to fall back to lower values
    #   of *weight*.
    # - If still nothing, tries to fall back on higher values of *weight*.
    # - If still nothing, returns `nil`.
    def path?(font : String, weight : FontWeight) : Path?
      @@cache.put_if_absent({font, weight}) { path0?(font, weight) }
    end
  end

  record FontData, font : SF::Font, size : Int32

  # Font keeper keeps a global `{path, size}` to `FontData` cache.
  module FontKeeper
    extend self

    @@cache = {} of {Path, Int32} => FontData

    # Returns the `FontData` object associated with the font at *path*, with point
    # size of *size*.
    def font_data(path : Path, size : Int32) : FontData
      @@cache.put_if_absent({path, size}) do
        FontData.new(SF::Font.from_file(path.to_s), size)
      end
    end
  end

  record TextData,
    font : SF::Font,
    size : Int32,
    leading : Float32,
    tracking : Float32

  struct TextData
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
  record WrapBounds, position : SF::Vector2i, width : Int32?, height : Int32? do
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
  struct TextCursor
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

  private def sf(object : RGB) : SF::Color
    SF::Color.new(object.r, object.g, object.b)
  end

  private def sf(object : RGB, a : UInt8) : SF::Color
    SF::Color.new(object.r, object.g, object.b, a)
  end

  private def sf(object : Point) : SF::Vector2i
    SF.vector2i(object.x, object.y)
  end

  private def sf(object : Rect) : SF::IntRect
    SF.int_rect(sf(object.origin), sf(object.extent))
  end

  # Returns the vertex array corresponding to *caption*.
  private def write(data : TextData, caption : String, position : SF::Vector2i, color : SF::Color) : SF::VertexArray
    vertices = SF::VertexArray.new(SF::Quads)

    state = '\0'

    offset = position
    voffset = SF.vector2i(0, data.size)

    caption.each_char_with_index do |chr|
      glyph = data.glyph(chr)

      case chr
      when '\r'
        next
      when ' '
        offset += data.wsstep
      when '\t'
        offset += data.wsstep*4
      when '\n'
        offset = SF.vector2i(position.x, offset.y) + data.nlstep
      else
        offset += SF.vector2(data.kerning(state, chr), 0).to_i

        vertices.append SF::Vertex.new(offset + voffset + glyph.bounds.top_left, color, glyph.texture_rect.top_left)
        vertices.append SF::Vertex.new(offset + voffset + glyph.bounds.top_right, color, glyph.texture_rect.top_right)
        vertices.append SF::Vertex.new(offset + voffset + glyph.bounds.bottom_right, color, glyph.texture_rect.bottom_right)
        vertices.append SF::Vertex.new(offset + voffset + glyph.bounds.bottom_left, color, glyph.texture_rect.bottom_left)

        offset += SF.vector2f(glyph.advance + data.letter_spacing, 0).to_i
      end
    ensure
      state = chr
    end

    vertices
  end

  private def paint(target : SF::RenderTarget, command : FillText)
    return unless path = FontFinder.path?(command.font, command.weight)

    data = FontKeeper.font_data(path, command.size)
    text = TextData.new(data.font, data.size, command.leading, command.tracking)

    vertices = write(text, command.caption, sf(command.origin), sf(command.color))

    target.draw(vertices, SF::RenderStates.new(text.texture))
  end

  private def paint(target : SF::RenderTarget, command : FillRect)
    if command.radius > 0
      sf = SF::RoundedRectangleShape.new
      sf.border_radius = command.radius
      sf = sf.as(SF::Rectangular)
    else
      sf = SF::RectangleShape.new.as(SF::Rectangular)
    end

    sf.fill_color = sf(command.color, command.alpha)
    sf.position = sf(command.box.origin)
    sf.size = sf(command.box.extent)

    sf.position -= SF.vector2i(command.ring.l, command.ring.t)
    sf.size = sf.size.to_i + SF.vector2i(command.ring.x, command.ring.y)

    sf.update

    target.draw(sf)
  end

  private def paint(target : SF::RenderTarget, command : OutlineRect)
    if command.radius > 0
      sf = SF::RoundedRectangleShape.new
      sf.border_radius = command.radius
      sf = sf.as(SF::Rectangular)
    else
      sf = SF::RectangleShape.new.as(SF::Rectangular)
    end

    sf.outline_thickness = command.thickness
    sf.outline_color = sf(command.color)
    sf.fill_color = SF::Color::Transparent

    sf.position = sf(command.box.origin)
    sf.size = sf(command.box.extent)

    sf.update

    target.draw(sf)
  end

  private def paint(target : SF::RenderTarget, command : FillCircle)
    raise "not implemented: fill circle"
  end

  private def paint(target : SF::RenderTarget, command : FillTriangle)
    raise "not implemented: fill triangle"
  end

  @@scratch : Scratchpad = Lock.synchronize { Scratchpad.new }

  private def paint(target : SF::RenderTarget, command : View)
    @@scratch.fit(command.box.extent.x, command.box.extent.y) do |surface|
      surface.clear(sf(command.color))

      paint(surface, command.children)

      surface.display

      sf = SF::Sprite.new(surface.texture)
      sf.position = sf(command.box.origin)
      sf.texture_rect = SF.int_rect(0, 0, command.box.extent.x, command.box.extent.y)

      target.draw(sf)
    end
  end

  private def paint(target : SF::RenderTarget, commands : Array(DrawCommand))
    commands.each { |command| paint(target, command) }
  end

  private def paint(sf : SF::RenderWindow, window : Window) : Nil
    case window.cursor
    in .arrow?
      sf.mouse_cursor = SF_ARROW
    in .pointer?
      sf.mouse_cursor = SF_HAND
    in .grabbing?
      sf.mouse_cursor = SF_SIZEALL
    end

    sf.clear(sf(window.color))

    paint(sf, window.children)

    sf.display
  end

  # Global SFML lock. Any SFML call must be synchronized using this lock:
  # SFML is not thread-safe.
  module Lock
    @@lock = Mutex.new(:reentrant)

    def self.lock
      @@lock.lock
    end

    def self.unlock
      @@lock.unlock
    end

    def self.synchronize(&)
      @@lock.synchronize { yield }
    end
  end

  # :nodoc:
  SF_HAND = Lock.synchronize { SF::Cursor.from_system(SF::Cursor::Type::Hand) }

  # :nodoc:
  SF_ARROW = Lock.synchronize { SF::Cursor.from_system(SF::Cursor::Type::Arrow) }

  # :nodoc:
  SF_SIZEALL = Lock.synchronize { SF::Cursor.from_system(SF::Cursor::Type::SizeAll) }

  def wrap(content : String, font : String, weight : FontWeight, size : Int32, leading : Float32, w : Int32?, h : Int32?) : String
    Lock.synchronize do
      return "" unless path = FontFinder.path?(font, weight)

      data = FontKeeper.font_data(path, size)
      text = TextData.new(data.font, size, leading, tracking: 1.0f32)

      TextCursor.wrap(text, content, WrapBounds.new(SF.vector2i(0, 0), w, h))
    end
  end

  private def measure_line(text : TextData, string : String, *, window = 0...string.size) : Int32
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

  def measure(content : String, font : String, weight : FontWeight, size : Int32, leading : Float32) : {Int32, Int32}
    Lock.synchronize do
      unless path = FontFinder.path?(font, weight)
        return 0, 0
      end

      data = FontKeeper.font_data(path, size)
      text = TextData.new(data.font, size, leading, tracking: 1.0f32)

      if content.empty?
        return 0, text.line_height
      end

      width = 0
      height = 0

      content.each_line(chomp: true) do |line|
        width = Math.max(width, measure_line(text, line))
        height += text.line_height
      end

      {width, height}
    end
  end

  private def transcribe(window : SF::RenderWindow, event : SF::Event::Closed)
    window.close

    [Term.of(:exit)]
  end

  private def transcribe(window : SF::RenderWindow, event : SF::Event::Resized)
    window.view = SF::View.new(SF.float_rect(0, 0, event.width, event.height))

    [Term.of(:size, event.width, event.height)]
  end

  private def transcribe(window : SF::RenderWindow, event : SF::Event::TextEntered)
    chr = event.unicode.chr
    chr.printable? ? [Term.of(:input, chr)] : [] of Term
  end

  private def transcribe(window : SF::RenderWindow, event : SF::Event::MouseButtonPressed)
    [Term.of(:mouse, :motion, event.x, event.y), Term.of(:mouse, :press)]
  end

  private def transcribe(window : SF::RenderWindow, event : SF::Event::MouseButtonReleased)
    [Term.of(:mouse, :motion, event.x, event.y), Term.of(:mouse, :release)]
  end

  private def transcribe(window : SF::RenderWindow, event : SF::Event::MouseMoved)
    [Term.of(:mouse, :motion, event.x, event.y)]
  end

  private def transcribe(window : SF::RenderWindow, event : SF::Event::KeyPressed)
    keyname = nil
    case event.code
    when .f1?        then keyname = "f1"
    when .f2?        then keyname = "f2"
    when .f3?        then keyname = "f3"
    when .f4?        then keyname = "f4"
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
    when .page_up?   then keyname = "pgup"
    when .page_down? then keyname = "pgdn"
    end

    if event.control
      case event.code
      when .a? then keyname = "a"
      when .c? then keyname = "c"
      when .v? then keyname = "v"
      end
    end

    return [] of Term unless keyname

    keyname = "S-#{keyname}" if event.shift
    keyname = "C-#{keyname}" if event.control
    key = Term::Sym.new(keyname)

    [Term.of(:key, key)]
  end

  private def transcribe(window : SF::RenderWindow, event : _)
    [] of Term
  end

  def show0(reducer : Reducer) : Nil
    drawable = reducer.call(Term.of, Term.of(:open))

    Term.case(drawable) do
      matchpi %[(window _ ¦ _ title⋮ "Untitled" icon⋮ "" final-w: w←(%number +i32) final-h: h←(%number +i32))] do
        Lock.lock

        window = SF::RenderWindow.new(SF::VideoMode.new(w.to(Int32), h.to(Int32)), title: title.to(String), settings: SF::ContextSettings.new(depth: 24, antialiasing: MAX_ANTIALIASING))
        window.framerate_limit = 60

        iconfile = icon.to(String)
        unless iconfile.empty?
          iconpath = RESOURCES / iconfile

          if File.exists?(iconpath)
            iconimg = SF::Image.from_file(iconpath.to_s)

            window.set_icon(256, 256, iconimg.pixels_ptr) # Assume 32-bit RGBA
          end
        end

        Lock.unlock

        while Lock.synchronize { window.open? }
          while event = Lock.synchronize { window.poll_event }
            transcribed = Lock.synchronize { transcribe(window, event) }
            transcribed.each { |term| drawable = reducer.call(drawable, term) }
          end

          drawable = reducer.call(drawable, Term.of(:cycle))
          commands = draw(drawable)

          Lock.synchronize do
            paint(window, commands)
          end
        end
      end
    end
  end

  def show(reducer : Reducer) : Nil
    wg = WaitGroup.new(1)

    ExecutionContext::Isolated.new("SFML") do
      show0(reducer)
    ensure
      wg.done
    end

    wg.wait
  end
end

