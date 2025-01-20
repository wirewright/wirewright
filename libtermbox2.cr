@[Link(ldflags: "#{__DIR__}/libtermbox2.a")]
lib LibTermbox2
  TB_ERR_NO_EVENT = -6
  TB_ERR_POLL     = -14

  enum EventType : UInt8
    Key    = 1u8
    Resize
    Mouse
  end

  @[Flags]
  enum InputMode : Int32
    Current
    Esc
    Alt
    Mouse
  end

  enum OutputMode : Int32
    Current
    Normal
    Color256
    Color216
    Grayscale
    Truecolor
  end

  # Mouse buttons for mouse events.
  enum MouseButton : UInt8
    Left
    Right
    Middle
    Release
    WheelUp
    WheelDown
  end

  # Key constants.
  enum Key : UInt16
    CTRL_TILDE       = 0x00
    CTRL_2           = 0x00
    CTRL_A           = 0x01
    CTRL_B           = 0x02
    CTRL_C           = 0x03
    CTRL_D           = 0x04
    CTRL_E           = 0x05
    CTRL_F           = 0x06
    CTRL_G           = 0x07
    BACKSPACE        = 0x08
    CTRL_H           = 0x08
    TAB              = 0x09
    CTRL_I           = 0x09
    CTRL_J           = 0x0a
    CTRL_K           = 0x0b
    CTRL_L           = 0x0c
    ENTER            = 0x0d
    CTRL_M           = 0x0d
    CTRL_N           = 0x0e
    CTRL_O           = 0x0f
    CTRL_P           = 0x10
    CTRL_Q           = 0x11
    CTRL_R           = 0x12
    CTRL_S           = 0x13
    CTRL_T           = 0x14
    CTRL_U           = 0x15
    CTRL_V           = 0x16
    CTRL_W           = 0x17
    CTRL_X           = 0x18
    CTRL_Y           = 0x19
    CTRL_Z           = 0x1a
    ESC              = 0x1b
    CTRL_LSQ_BRACKET = 0x1b
    CTRL_3           = 0x1b
    CTRL_4           = 0x1c
    CTRL_BACKSLASH   = 0x1c
    CTRL_5           = 0x1d
    CTRL_RSQ_BRACKET = 0x1d
    CTRL_6           = 0x1e
    CTRL_7           = 0x1f
    CTRL_SLASH       = 0x1f
    CTRL_UNDERSCORE  = 0x1f
    SPACE            = 0x20
    BACKSPACE2       = 0x7f
    CTRL_8           = 0x7f

    # Terminal-dependent key constants.

    F1               = (0xffff - 0)
    F2               = (0xffff - 1)
    F3               = (0xffff - 2)
    F4               = (0xffff - 3)
    F5               = (0xffff - 4)
    F6               = (0xffff - 5)
    F7               = (0xffff - 6)
    F8               = (0xffff - 7)
    F9               = (0xffff - 8)
    F10              = (0xffff - 9)
    F11              = (0xffff - 10)
    F12              = (0xffff - 11)
    INSERT           = (0xffff - 12)
    DELETE           = (0xffff - 13)
    HOME             = (0xffff - 14)
    END              = (0xffff - 15)
    PGUP             = (0xffff - 16)
    PGDN             = (0xffff - 17)
    ARROW_UP         = (0xffff - 18)
    ARROW_DOWN       = (0xffff - 19)
    ARROW_LEFT       = (0xffff - 20)
    ARROW_RIGHT      = (0xffff - 21)
    BACK_TAB         = (0xffff - 22)
    MOUSE_LEFT       = (0xffff - 23)
    MOUSE_RIGHT      = (0xffff - 24)
    MOUSE_MIDDLE     = (0xffff - 25)
    MOUSE_RELEASE    = (0xffff - 26)
    MOUSE_WHEEL_UP   = (0xffff - 27)
    MOUSE_WHEEL_DOWN = (0xffff - 28)
  end

  # Key modifiers (bitwise).
  @[Flags]
  enum KeyModifier : UInt8
    Alt
    Ctrl
    Shift
    Motion
  end

  struct Event
    type : EventType
    mod : KeyModifier
    key : Key
    ch : UInt32
    resize_w : Int32
    resize_h : Int32
    mouse_x : Int32
    mouse_y : Int32
  end

  fun tb_init : Int32
  fun tb_shutdown : Int32

  fun tb_width : Int32
  fun tb_height : Int32

  fun tb_clear : Int32
  fun tb_set_clear_attrs(fg : UInt64, bg : UInt64) : Int32

  fun tb_present : Int32
  fun tb_invalidate : Int32

  fun tb_set_cursor(cx : Int32, cy : Int32)
  fun tb_hide_cursor : Int32

  fun tb_set_cell(x : Int32, y : Int32, ch : UInt32, fg : UInt64, bg : UInt64) : Int32

  fun tb_set_input_mode(mode : InputMode) : Int32
  fun tb_set_output_mode(mode : OutputMode) : Int32

  fun tb_peek_event(event : Event*, timeout_ms : Int32) : Int32
  fun tb_poll_event(event : Event*) : Int32

  fun tb_strerror(err : Int32) : UInt8*
  fun tb_last_errno : Errno
end

module Termbox
  extend self

  alias Event = LibTermbox2::Event
  alias InputMode = LibTermbox2::InputMode
  alias OutputMode = LibTermbox2::OutputMode
  alias MouseButton = LibTermbox2::MouseButton
  alias Key = LibTermbox2::Key
  alias KeyModifier = LibTermbox2::KeyModifier

  # :nodoc:
  #
  # HSL 8 colors palette.
  PALETTE8 = {
    {0, 0, 0},
    {0, 100, 25},
    {120, 100, 25},
    {60, 100, 25},
    {240, 100, 25},
    {300, 100, 25},
    {180, 100, 25},
    {0, 0, 75},
  }

  # :nodoc:
  #
  # HSL 216 colors palette.
  PALETTE216 = {
    {0, 0, 0},
    {240, 100, 18},
    {240, 100, 26},
    {240, 100, 34},
    {240, 100, 42},
    {240, 100, 50},
    {120, 100, 18},
    {180, 100, 18},
    {97, 100, 26},
    {7, 100, 34},
    {13, 100, 42},
    {17, 100, 50},
    {120, 100, 26},
    {62, 100, 26},
    {180, 100, 26},
    {93, 100, 34},
    {2, 100, 42},
    {8, 100, 50},
    {120, 100, 34},
    {52, 100, 34},
    {66, 100, 34},
    {180, 100, 34},
    {91, 100, 42},
    {98, 100, 50},
    {120, 100, 42},
    {46, 100, 42},
    {57, 100, 42},
    {68, 100, 42},
    {180, 100, 42},
    {89, 100, 50},
    {120, 100, 50},
    {42, 100, 50},
    {51, 100, 50},
    {61, 100, 50},
    {70, 100, 50},
    {180, 100, 50},
    {0, 100, 18},
    {300, 100, 18},
    {82, 100, 26},
    {72, 100, 34},
    {66, 100, 42},
    {62, 100, 50},
    {60, 100, 18},
    {0, 0, 37},
    {240, 17, 45},
    {240, 33, 52},
    {240, 60, 60},
    {240, 100, 68},
    {7, 100, 26},
    {120, 17, 45},
    {180, 17, 45},
    {210, 33, 52},
    {220, 60, 60},
    {225, 100, 68},
    {7, 100, 34},
    {120, 33, 52},
    {150, 33, 52},
    {180, 33, 52},
    {200, 60, 60},
    {210, 100, 68},
    {3, 100, 42},
    {120, 60, 60},
    {140, 60, 60},
    {160, 60, 60},
    {180, 60, 60},
    {195, 100, 68},
    {7, 100, 50},
    {120, 100, 68},
    {135, 100, 68},
    {150, 100, 68},
    {165, 100, 68},
    {180, 100, 68},
    {0, 100, 26},
    {17, 100, 26},
    {300, 100, 26},
    {86, 100, 34},
    {77, 100, 42},
    {71, 100, 50},
    {2, 100, 26},
    {0, 17, 45},
    {300, 17, 45},
    {270, 33, 52},
    {260, 60, 60},
    {255, 100, 68},
    {60, 100, 26},
    {60, 17, 45},
    {0, 0, 52},
    {240, 20, 60},
    {240, 50, 68},
    {240, 100, 76},
    {3, 100, 34},
    {90, 33, 52},
    {120, 20, 60},
    {180, 20, 60},
    {210, 50, 68},
    {220, 100, 76},
    {2, 100, 42},
    {100, 60, 60},
    {120, 50, 68},
    {150, 50, 68},
    {180, 50, 68},
    {200, 100, 76},
    {8, 100, 50},
    {105, 100, 68},
    {120, 100, 76},
    {140, 100, 76},
    {160, 100, 76},
    {180, 100, 76},
    {0, 100, 34},
    {27, 100, 34},
    {13, 100, 34},
    {300, 100, 34},
    {88, 100, 42},
    {81, 100, 50},
    {2, 100, 34},
    {0, 33, 52},
    {330, 33, 52},
    {300, 33, 52},
    {280, 60, 60},
    {270, 100, 68},
    {6, 100, 34},
    {30, 33, 52},
    {0, 20, 60},
    {300, 20, 60},
    {270, 50, 68},
    {260, 100, 76},
    {60, 100, 34},
    {60, 33, 52},
    {60, 20, 60},
    {0, 0, 68},
    {240, 33, 76},
    {240, 100, 84},
    {1, 100, 42},
    {80, 60, 60},
    {90, 50, 68},
    {120, 33, 76},
    {180, 33, 76},
    {210, 100, 84},
    {8, 100, 50},
    {90, 100, 68},
    {100, 100, 76},
    {120, 100, 84},
    {150, 100, 84},
    {180, 100, 84},
    {0, 100, 42},
    {33, 100, 42},
    {22, 100, 42},
    {11, 100, 42},
    {300, 100, 42},
    {90, 100, 50},
    {6, 100, 42},
    {0, 60, 60},
    {340, 60, 60},
    {320, 60, 60},
    {300, 60, 60},
    {285, 100, 68},
    {7, 100, 42},
    {20, 60, 60},
    {0, 50, 68},
    {330, 50, 68},
    {300, 50, 68},
    {280, 100, 76},
    {8, 100, 42},
    {40, 60, 60},
    {30, 50, 68},
    {0, 33, 76},
    {300, 33, 76},
    {270, 100, 84},
    {60, 100, 42},
    {60, 60, 60},
    {60, 50, 68},
    {60, 33, 76},
    {0, 0, 84},
    {240, 100, 92},
    {9, 100, 50},
    {75, 100, 68},
    {80, 100, 76},
    {90, 100, 84},
    {120, 100, 92},
    {180, 100, 92},
    {0, 100, 50},
    {37, 100, 50},
    {28, 100, 50},
    {18, 100, 50},
    {9, 100, 50},
    {300, 100, 50},
    {2, 100, 50},
    {0, 100, 68},
    {345, 100, 68},
    {330, 100, 68},
    {315, 100, 68},
    {300, 100, 68},
    {1, 100, 50},
    {15, 100, 68},
    {0, 100, 76},
    {340, 100, 76},
    {320, 100, 76},
    {300, 100, 76},
    {1, 100, 50},
    {30, 100, 68},
    {20, 100, 76},
    {0, 100, 84},
    {330, 100, 84},
    {300, 100, 84},
    {0, 100, 50},
    {45, 100, 68},
    {40, 100, 76},
    {30, 100, 84},
    {0, 100, 92},
    {300, 100, 92},
    {60, 100, 50},
    {60, 100, 68},
    {60, 100, 76},
    {60, 100, 84},
    {60, 100, 92},
    {0, 0, 100},
  }

  # :nodoc:
  #
  # HSL grayscale palette.
  PALETTE_GRAYSCALE = {
    {0, 0, 3},
    {0, 0, 7},
    {0, 0, 10},
    {0, 0, 14},
    {0, 0, 18},
    {0, 0, 22},
    {0, 0, 26},
    {0, 0, 30},
    {0, 0, 34},
    {0, 0, 37},
    {0, 0, 40},
    {0, 0, 46},
    {0, 0, 50},
    {0, 0, 54},
    {0, 0, 58},
    {0, 0, 61},
    {0, 0, 65},
    {0, 0, 69},
    {0, 0, 73},
    {0, 0, 77},
    {0, 0, 81},
    {0, 0, 85},
    {0, 0, 89},
    {0, 0, 93},
  }

  # :nodoc:
  #
  # HSL 256 colors palette.
  PALETTE256 = {
    *PALETTE8,
    {0, 0, 50},
    {0, 100, 50},
    {120, 100, 50},
    {60, 100, 50},
    {240, 100, 50},
    {300, 100, 50},
    {180, 100, 50},
    {0, 0, 100},
    *PALETTE216,
    *PALETTE_GRAYSCALE,
  }

  @[Flags]
  enum Color : UInt64
    Red     = 0xff0000
    Green   = 0x00ff00
    Blue    = 0x0000ff
    Yellow  = 0xffff00
    Magenta = 0xff00ff
    Cyan    = 0x00ffff
    White   = 0xffffff

    Bold       = 0x01000000
    Underline
    Reverse
    Italic
    Blink
    Black
    Bright
    Dim
    Strikeout
    Underline2
    Overline
    Invisible

    def default? : Bool
      (value & 0xffffff) == 0
    end

    private def closest(palette, needle)
      best_i = 0
      best_dsq = Float64::MAX

      palette.each_with_index do |(h, s, l), i|
        dx = h - needle[0]
        dy = s - needle[1]
        dz = l - needle[2]
        dsq = dx ** 2 + dy ** 2 + dz ** 2
        next unless dsq < best_dsq
        best_i = i
        best_dsq = dsq
      end

      best_i
    end

    def convert(mode : OutputMode) : UInt64
      return value if default? || mode.truecolor?

      h, s, l = hsl

      case mode
      in .normal?
        Color.new(closest(PALETTE8, {h, s, l}).to_u64 + 1).value # todo: add attributes
      in .color256?
        Color.new(closest(PALETTE256, {h, s, l}).to_u64 + 1).value # todo: add attributes remove bright attr
      in .color216?
        Color.new(closest(PALETTE216, {h, s, l}).to_u64 + 1).value # todo: add attributes remove bright attr
      in .grayscale?
        Color.new(closest(PALETTE_GRAYSCALE, {h, s, l}).to_u64 + 1).value # todo: add attributes remove bright attr
      in .current?, .truecolor?
        raise "unreachable"
      end
    end

    def hsl : {Int32, Int32, Int32}
      r, g, b = self.r / 255.0, self.g / 255.0, self.b / 255.0

      max = {r, g, b}.max
      min = {r, g, b}.min

      h = s = l = (max + min) / 2

      if max == min
        h = s = 0
      else
        d = max - min
        s = (l > 0.5) ? d / (2 - max - min) : d / (max + min)

        if max == r
          h = (g - b) / d + (g < b ? 6 : 0)
        elsif max == g
          h = (b - r) / d + 2
        elsif max == b
          h = (r - g) / d + 4
        end

        h /= 6
      end

      {(h * 360).to_i, (s * 100).to_i, (l * 100).to_i}
    end

    def r : UInt8
      ((value & 0xff0000) >> 16).to_u8
    end

    def g : UInt8
      ((value & 0x00ff00) >> 8).to_u8
    end

    def b : UInt8
      (value & 0x0000ff).to_u8
    end

    def self.rgb(r : UInt8, g : UInt8, b : UInt8)
      if r == 0 && g == 0 && b == 0
        return Black
      end

      new((r.to_u64 << 16) | (g.to_u64 << 8) | b.to_u64)
    end
  end

  private macro assert!(call)
    %v = {{call}}
    %v.negative? ? raise "termbox error: #{String.new(LibTermbox2.tb_strerror(%v))}" : %v
  end

  def init : Nil
    assert! LibTermbox2.tb_init
  end

  def shutdown : Nil
    assert! LibTermbox2.tb_shutdown
  end

  def init(&)
    init
    yield
  ensure
    shutdown
  end

  def input_mode : InputMode
    InputMode.new(assert! LibTermbox2.tb_set_input_mode(InputMode::Current))
  end

  def input_mode=(mode : InputMode)
    assert! LibTermbox2.tb_set_input_mode(mode)
  end

  def output_mode : OutputMode
    OutputMode.new(assert! LibTermbox2.tb_set_output_mode(OutputMode::Current))
  end

  def output_mode=(mode : OutputMode)
    assert! LibTermbox2.tb_set_output_mode(mode)
    assert! LibTermbox2.tb_invalidate
  end

  def width : Int32
    assert! LibTermbox2.tb_width
  end

  def height : Int32
    assert! LibTermbox2.tb_height
  end

  def clear : Nil
    assert! LibTermbox2.tb_clear
  end

  def clear(*, fg : Color, bg : Color) : Nil
    assert! LibTermbox2.tb_set_clear_attrs(fg.value, bg.value)
    assert! LibTermbox2.tb_clear
  end

  def present : Nil
    assert! LibTermbox2.tb_present
  end

  def set(char : Char, *, x : Int32, y : Int32, fg : Color, bg : Color)
    assert! LibTermbox2.tb_set_cell(x, y, char.ord, fg.convert(output_mode), bg.convert(output_mode))
  end

  def write(object, *, x ox : Int32, y oy : Int32, **kwargs)
    x = ox
    y = oy
    object.to_s.each_char do |char|
      if char == '\n'
        y += 1
        x = ox
        next
      else
        set(char, **kwargs, x: x, y: y)
        x += 1
      end
    end
  end

  def peek?(timeout = 0.milliseconds) : Event?
    while true
      result = LibTermbox2.tb_peek_event(out event, timeout.total_milliseconds.to_i)
      if result == LibTermbox2::TB_ERR_NO_EVENT
        return
      end
      unless result == LibTermbox2::TB_ERR_POLL
        return event
      end
      unless LibTermbox2.tb_last_errno.eintr?
        raise "termbox error: poll error (errno != EINTR)"
      end
    end
  end

  def poll : Event
    while true
      result = LibTermbox2.tb_peek_event(out event, -1)
      unless result == LibTermbox2::TB_ERR_POLL
        return event
      end
      unless LibTermbox2.tb_last_errno.eintr?
        raise "termbox error: poll error (errno != EINTR)"
      end
    end
  end
end

