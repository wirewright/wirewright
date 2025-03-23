module MapCoord
  def map(x, y)
    position + SF.vector2(x, y) * size
  end

  def align(*, my, its, it : MapCoord)
    self.position = (it.map(*its) - (map(*my) - position)).to_i
  end
end

struct SF::Rect(T)
  def position
    SF::Vector2(T).new(left, top)
  end

  def size
    SF::Vector2(T).new(width, height)
  end
end

class SF::Sprite
  include MapCoord

  def size
    texture.not_nil!.size
  end
end

class SF::CircleShape
  def size
    SF.vector2f(radius, radius)
  end
end

class SF::View::Reference
  include MapCoord

  def position
    SF.vector2f(0, 0)
  end
end

class SF::Transformable
  include MapCoord
end

class SF::Text
  # Returns the width of this text.
  def width : Int
    (global_bounds.width + local_bounds.left).to_i
  end

  # Returns the height of this text.
  def height : Int
    Math.max(global_bounds.height + local_bounds.top, character_size).to_i
  end

  # Returns the full width and height of this text.
  def size
    SF.vector2i(width, height)
  end

  def center
    position + size*0.5
  end

  def center=(vec)
    self.position = (vec - size*0.5).to_i
  end
end

struct Cloak(T)
  def initialize(@object : T)
  end

  def unwrap
    @object
  end
end

# Defines a `change` method which functions like `#copy_with` for records.
macro def_change
  {% verbatim do %}
    def change(**kwargs) : self
      {% begin %}
        {{@type}}.new(
          {% for var in @type.instance_vars %}
            {{var.id}}: (kwargs[{{var.symbolize}}]? || @{{var.id}}),
          {% end %}
        )
      {% end %}
    end
  {% end %}
end

struct NamedTuple
  # Yields each `{key, value}` pair, uses the block's return value
  # as the new value in the returned named tuple.
  def mapv(&)
    {% begin %}
      {% if T.size == 0 %}
        self
      {% else %}
          { {% for k in T %}
              {{k.id}}: (yield {{k.symbolize}}, self[{{k.symbolize}}]),
            {% end %} }
      {% end %}
    {% end %}
  end
end

struct Number
  def xy
    SF.vector2(self, self)
  end

  def x0
    SF.vector2(self, 0)
  end

  def y0
    SF.vector2(0, self)
  end
end

struct SF::Rect(T)
  def to_f
    SF.float_rect(left, top, width, height)
  end

  def position
    SF.vector2(left, top)
  end

  def size
    SF.vector2(width, height)
  end

  def right
    left + width
  end

  def bottom
    top + height
  end

  def top_left
    position
  end

  def top_right
    position + size.ox
  end

  def bottom_left
    position + size.oy
  end

  def bottom_right
    position + size
  end

  def center
    position + size*0.5
  end

  def margin(px = 0, py = 0)
    SF::Rect(T).new(left - px, top - py, width + px*2, height + py*2)
  end
end

struct SF::Vector2(T)
  def length
    Math.hypot(x, y)
  end

  def norm
    l = length
    SF.vector2f(x/l, y/l)
  end

  def distance(other : Vector2)
    Math.hypot(x - other.x, y - other.y)
  end

  def *(other : Vector2(T))
    Vector2(T).new(x * other.x, y * other.y)
  end

  def dot(other : Vector2)
    x * other.x + y * other.y
  end

  def perpendicular
    Vector2(T).new(-y, x)
  end

  def angle(other : SF::Vector2)
    Math.atan2(other.y - y, other.x - x)*(180/Math::PI)
  end

  def rotate(angle)
    SF.vector2f(x*Math.cos(angle) - y*Math.sin(angle), x*Math.sin(angle) + y*Math.cos(angle))
  end

  def min(other : Vector2(T))
    Vector2(T).new(Math.min(x, other.x), Math.min(y, other.y))
  end

  def max(other : Vector2(T))
    Vector2(T).new(Math.max(x, other.x), Math.max(y, other.y))
  end

  def to_i
    SF.vector2i(x.to_i, y.to_i)
  end

  def to_f
    SF.vector2f(x.to_f, y.to_f)
  end

  def ox
    SF::Vector2(T).new(@x, 0)
  end

  def oy
    SF::Vector2(T).new(0, @y)
  end
end

module SF
  def self.int_rect(position : SF::Vector2i, size : SF::Vector2i)
    int_rect(position.x, position.y, size.x, size.y)
  end

  def self.int_rect(position : SF::Vector2f, size : SF::Vector2f)
    int_rect(position.to_i, size.to_i)
  end
end

module SF::Keyboard
  def self.shift? : Bool
    key_pressed?(Scan::Scancode::LShift) || key_pressed?(Scan::Scancode::RShift)
  end
end

module SF::Rectangular
end

class SF::RectangleShape
  include Rectangular
end

class SF::RoundedRectangleShape < SF::Shape
  include Rectangular

  getter size : SF::Vector2i = SF.vector2i(0, 0)
  @border_radius : Float64 = 0.0f64

  def initialize
    super()

    @corner_point_count = 64
  end

  def size=(@size : Vector2i)
  end

  def border_radius=(@border_radius : Float64)
  end

  def point_count : Int32
    @corner_point_count * 4
  end

  def get_point(index : Int) : SF::Vector2f
    if index >= point_count
      return SF.vector2f(0, 0)
    end

    border_radius = @border_radius.clamp(0.0..@size.x/2).to_f

    delta_angle = 90 / (@corner_point_count - 1)
    center_index = index//@corner_point_count
    center = SF.vector2f(0, 0)

    case center_index
    when 0
      center = SF.vector2f(@size.x - border_radius, border_radius)
    when 1
      center = SF.vector2f(border_radius, border_radius)
    when 2
      center = SF.vector2f(border_radius, @size.y - border_radius)
    when 3
      center = SF.vector2f(@size.x - border_radius, @size.y - border_radius)
    end

    SF.vector2f(
      border_radius*Math.cos(delta_angle*(index - center_index)*Math::PI/180) + center.x,
      -border_radius*Math.sin(delta_angle*(index - center_index)*Math::PI/180) + center.y)
  end
end
