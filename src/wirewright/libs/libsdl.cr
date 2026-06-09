{% skip_file unless flag?(:sdl3) %}

require "sdl-crystal-bindings/sdl3-crystal-bindings"

module Ww
  # Internal SDL3 wrapper.
  #
  # Uses [Hadeweka/SDL-Crystal-Bindings](https://github.com/Hadeweka/SDL-Crystal-Bindings).
  module SDL
    extend self

    Log = ::Log.for(self)

    FALSE = 0u8
    TRUE  = 1u8

    private macro assert_true(call)
      assert({{call}} == {{@type}}::TRUE)
    end

    @@trigger : UInt32 = 0

    def init : Nil
      assert_true LibSDL.set_hint(LibSDL::HINT_NO_SIGNAL_HANDLERS, "1")
      assert_true LibSDL.set_hint(LibSDL::HINT_QUIT_ON_LAST_WINDOW_CLOSE, "0")
      assert_true LibSDL.init(LibSDL::InitFlags::VIDEO)

      first_event_code = LibSDL.register_events(1)
      assert first_event_code > 0, "LibSDL.register_events(1)"

      @@trigger = first_event_code
    end

    # WARNING: Callers are responsible for destroying cursors using `destroy`.
    alias Cursor = LibSDL::Cursor*
    alias SystemCursor = LibSDL::SystemCursor

    def make(cls : Cursor.class, id : SystemCursor) : Cursor
      cursor = LibSDL.create_system_cursor(id)
      assert cursor, "LibSDL.create_system_cursor(#{id})"

      cursor
    end

    # WARNING: Callers are responsible for destroying windows using `destroy`.
    alias Window = LibSDL::Window*
    # WARNING: Callers are responsible for destroying renderers using `destroy`.
    alias Renderer = LibSDL::Renderer*
    alias WindowFlags = LibSDL::WindowFlags

    def make(cls : {Window.class, Renderer.class}, title : String, width : Int32, height : Int32, flags = WindowFlags::None) : {Window, Renderer}
      assert_true LibSDL.create_window_and_renderer(title, width, height, flags, out window, out renderer)

      if LibSDL.set_render_vsync(renderer, LibSDL::RENDERER_VSYNC_ADAPTIVE) == FALSE
        Log.notice { "setting Vsync to ADAPTIVE failed, continuing with SDL defaults..." }
      end

      {window, renderer}
    end

    # WARNING: Callers are responsible for destroying textures using `destroy`.
    alias Texture = LibSDL::Texture*
    alias PixelFormat = LibSDL::PixelFormat
    alias TextureAccess = LibSDL::TextureAccess

    def make(cls : Texture.class, renderer : Renderer, width : Int32, height : Int32, format : PixelFormat, access : TextureAccess) : Texture
      texture = LibSDL.create_texture(renderer, format, access, width, height)
      assert texture, "LibSDL.create_texture(renderer, #{format}, #{access}, #{width}, #{height})"

      texture
    end

    def destroy(entity : Window) : Nil
      LibSDL.destroy_window(entity)
    end

    def destroy(entity : Renderer) : Nil
      LibSDL.destroy_renderer(entity)
    end

    def destroy(entity : Texture) : Nil
      LibSDL.destroy_texture(entity)
    end

    def destroy(entity : Cursor) : Nil
      LibSDL.destroy_cursor(entity)
    end

    alias WindowId = LibSDL::WindowID

    def id(window : Window) : WindowId
      id = LibSDL.get_window_id(window)
      assert id > 0, "LibSDL.get_window_id"

      id
    end

    def set(window : Window, *, resizable : Bool) : Nil
      assert_true LibSDL.set_window_resizable(window, resizable ? TRUE : FALSE)
    end

    def set(window : Window, *, title : String) : Nil
      assert_true LibSDL.set_window_title(window, title)
    end

    def set(window : Window, *, input : Bool) : Nil
      if input
        assert_true LibSDL.start_text_input(window)
      else
        assert_true LibSDL.stop_text_input(window)
      end
    end

    def resize(window : Window, width : Int32, height : Int32) : Nil
      assert_true LibSDL.set_window_size(window, width, height)
    end

    def lock(texture : Texture, & : Void*, Int32 ->)
      assert_true LibSDL.lock_texture(texture, nil, out pixelsptr, out pitch)

      begin
        yield pixelsptr, pitch
      ensure
        LibSDL.unlock_texture(texture)
      end
    end

    def copy(src texture : Texture, dst renderer : Renderer, width : Int32, height : Int32)
      rect = LibSDL::FRect.new(x: 0, y: 0, w: width, h: height)

      assert_true LibSDL.render_texture(renderer, texture,
        srcrect: pointerof(rect),
        dstrect: pointerof(rect),
      )
    end

    def present(renderer : Renderer) : Nil
      assert_true LibSDL.render_present(renderer)
    end

    def show(window : Window) : Nil
      assert_true LibSDL.show_window(window)
    end

    def cursor=(cursor : Bool) : Bool
      if cursor
        assert_true LibSDL.show_cursor
      else
        assert_true LibSDL.hide_cursor
      end
      cursor
    end

    def cursor=(cursor : Cursor) : Cursor
      assert_true LibSDL.set_cursor(cursor)

      cursor
    end

    def hide(window : Window) : Nil
      assert_true LibSDL.hide_window(window)
    end

    def push(event : Trigger)
      assert @@trigger > 0

      raw_event = LibSDL::Event.new(type: @@trigger)
      assert_true LibSDL.push_event(pointerof(raw_event))
    end

    def pump : Nil
      LibSDL.pump_events
    end

    def wait : Event
      assert_true LibSDL.wait_event(out event)

      transcribe(event)
    end

    def modifiers : Slice(KeyModifier)
      modifiers = Pf::Kit.stack_array(KeyModifier, 4)
      transcribe(modifiers, Keymod.new(LibSDL.get_mod_state.value))

      modifiers.to_unsafe_readonly_slice!
    end

    alias EventType = LibSDL::EventType

    alias MouseId = LibSDL::MouseID
    alias KeyboardId = LibSDL::KeyboardID
    alias Scancode = LibSDL::Scancode
    alias Keycode = LibSDL::Keycode

    enum MouseButton : UInt8
      Left   = 1
      Middle = 2
      Right  = 3
      X1     = 4
      X2     = 5
    end

    alias Event = Trigger | WindowEvent | MouseEvent | KeyboardEvent | TextEvent | UnknownEvent

    defrecord Trigger

    alias WindowEvent = WindowMouseFocusGained | WindowMouseFocusLost | WindowExposed |
                        WindowResized | WindowClosed | WindowFocusGained | WindowFocusLost |
                        WindowMinimized | WindowRestored

    defrecord WindowMouseFocusGained, window_id : WindowId
    defrecord WindowMouseFocusLost, window_id : WindowId
    defrecord WindowExposed, window_id : WindowId
    defrecord WindowResized, window_id : WindowId, width : Int32, height : Int32
    defrecord WindowClosed, window_id : WindowId
    defrecord WindowFocusGained, window_id : WindowId
    defrecord WindowFocusLost, window_id : WindowId
    defrecord WindowMinimized, window_id : WindowId
    defrecord WindowRestored, window_id : WindowId

    alias MouseEvent = MouseMoved | MouseButtonEvent | MouseWheelScrolled
    alias MouseButtonEvent = MouseButtonUp | MouseButtonDown

    defrecord MouseMoved, window_id : WindowId, mouse_id : MouseId, x : Float32, y : Float32, dx : Float32, dy : Float32
    defrecord MouseButtonUp, window_id : WindowId, mouse_id : MouseId, button : MouseButton, clicks : UInt8
    defrecord MouseButtonDown, window_id : WindowId, mouse_id : MouseId, button : MouseButton, clicks : UInt8
    defrecord MouseWheelScrolled, window_id : WindowId, mouse_id : MouseId, dx : Float32, dy : Float32

    alias KeyboardEvent = KeyboardKeyEvent
    alias KeyboardKeyEvent = KeyboardKeyDown | KeyboardKeyUp
    alias TextEvent = TextEntered

    defrecord KeyboardKeyDown,
      window_id : WindowId,
      keyboard_id : KeyboardId,
      scancode : Scancode,
      keycode : Keycode,
      modifiers : Slice(KeyModifier),
      repeat : Bool

    defrecord KeyboardKeyUp,
      window_id : WindowId,
      keyboard_id : KeyboardId,
      scancode : Scancode,
      keycode : Keycode,
      modifiers : Slice(KeyModifier)

    enum KeyModifier
      LShift
      RShift
      # ???
      Level5
      LCtrl
      RCtrl
      LAlt
      RAlt
      # LGUI
      LWin
      # RGUI
      RWin
      # Num
      NumLock
      # Caps
      CapsLock
      # Mode
      AltGr
      # Scroll
      ScrollLock
    end

    defrecord TextEntered, window_id : WindowId, rune : String

    defrecord UnknownEvent

    private def transcribe(event : LibSDL::Event) : Event
      if event.type == @@trigger
        return Trigger.new
      end

      case type = LibSDL::EventType.new(event.type.to_i)
      when .window_mouse_enter?
        WindowMouseFocusGained.new(event.window.window_id)
      when .window_mouse_leave?
        WindowMouseFocusLost.new(event.window.window_id)
      when .window_exposed?
        WindowExposed.new(event.window.window_id)
      when .window_resized?
        WindowResized.new(event.window.window_id,
          width: event.window.data1,
          height: event.window.data2,
        )
      when .window_close_requested?
        WindowClosed.new(event.window.window_id)
      when .window_focus_gained?
        WindowFocusGained.new(event.window.window_id)
      when .window_focus_lost?
        WindowFocusLost.new(event.window.window_id)
      when .window_minimized?
        WindowMinimized.new(event.window.window_id)
      when .window_restored?
        WindowRestored.new(event.window.window_id)
      when .mouse_motion?
        MouseMoved.new(event.motion.window_id,
          mouse_id: event.motion.which,
          x: event.motion.x,
          y: event.motion.y,
          dx: event.motion.xrel,
          dy: event.motion.yrel,
        )
      when .mouse_wheel?
        # https://wiki.libsdl.org/SDL3/SDL_MouseWheelEvent
        case event.wheel.direction
        in .normal?  then factor = 1.0f32
        in .flipped? then factor = -1.0f32
        end

        MouseWheelScrolled.new(event.wheel.window_id,
          mouse_id: event.wheel.which,
          dx: event.wheel.x * factor,
          dy: event.wheel.y * factor,
        )
      when .mouse_button_up?
        MouseButtonUp.new(event.button.window_id,
          mouse_id: event.button.which,
          button: MouseButton.new(event.button.button),
          clicks: event.button.clicks,
        )
      when .mouse_button_down?
        MouseButtonDown.new(event.button.window_id,
          mouse_id: event.button.which,
          button: MouseButton.new(event.button.button),
          clicks: event.button.clicks,
        )
      when .text_input?
        TextEntered.new(event.text.window_id, String.new(event.text.text))
      when .key_up?
        modifiers = Pf::Kit.stack_array(KeyModifier, 4)
        transcribe(modifiers, Keymod.new(event.key.mod.value))

        KeyboardKeyUp.new(event.key.window_id,
          keyboard_id: event.key.which,
          scancode: Scancode.new(event.key.scancode),
          keycode: Keycode.new(event.key.key),
          modifiers: modifiers.to_unsafe_readonly_slice!,
        )
      when .key_down?
        modifiers = Pf::Kit.stack_array(KeyModifier, 4)
        transcribe(modifiers, Keymod.new(event.key.mod.value))

        KeyboardKeyDown.new(event.key.window_id,
          keyboard_id: event.key.which,
          scancode: Scancode.new(event.key.scancode),
          keycode: Keycode.new(event.key.key),
          modifiers: modifiers.to_unsafe_readonly_slice!,
          repeat: event.key.repeat == TRUE,
        )
      else
        UnknownEvent.new
      end
    end

    # :nodoc:
    @[Flags]
    enum Keymod : UInt16
      NONE   = 0x0000
      LSHIFT = 0x0001
      RSHIFT = 0x0002
      LEVEL5 = 0x0004
      LCTRL  = 0x0040
      RCTRL  = 0x0080
      LALT   = 0x0100
      RALT   = 0x0200
      LGUI   = 0x0400
      RGUI   = 0x0800
      NUM    = 0x1000
      CAPS   = 0x2000
      MODE   = 0x4000
      SCROLL = 0x8000
    end

    private def transcribe(modifiers, keymod : Keymod) : Nil
      modifiers << :l_shift if keymod.lshift?
      modifiers << :r_shift if keymod.rshift?
      modifiers << :level5 if keymod.level5?
      modifiers << :l_ctrl if keymod.lctrl?
      modifiers << :r_ctrl if keymod.rctrl?
      modifiers << :l_alt if keymod.lalt?
      modifiers << :r_alt if keymod.ralt?
      modifiers << :l_win if keymod.lgui?
      modifiers << :r_win if keymod.rgui?
      modifiers << :num_lock if keymod.num?
      modifiers << :caps_lock if keymod.caps?
      modifiers << :alt_gr if keymod.mode?
      modifiers << :scroll_lock if keymod.scroll?
    end
  end
end
