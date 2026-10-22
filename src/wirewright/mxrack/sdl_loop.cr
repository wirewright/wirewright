module Ww::MxRack
  # Mediates the interaction between MxRack and SDL.
  class SdlLoop
    # A dynamic texture (analogical to a dynamic array).
    class DynTexture
      @texture : SDL::Texture?

      def initialize
        @width = 0u32
        @height = 0u32
      end

      def get(& : SDL::Texture ->) : Nil
        return unless texture = @texture

        yield texture
      end

      def lock(&) : Nil
        return unless texture = @texture

        SDL.lock(texture) do |pixels, pitch|
          yield pixels, pitch
        end
      end

      MAX_WIDTH  = 16384
      MAX_HEIGHT = 16384

      # Reserves the space required by *desired width* and *height*. The reserved space
      # will be greater or equal than *desired*.
      #
      # The allocated texture is associated with *renderer*.
      def reserve(renderer : SDL::Renderer, desired_width : UInt32, desired_height : UInt32) : Nil
        assert desired_width <= MAX_WIDTH && desired_height <= MAX_HEIGHT, "reserve() exceeds max size of #{MAX_WIDTH}x#{MAX_HEIGHT}"

        target_width = Math.pw2ceil(desired_width)
        target_height = Math.pw2ceil(desired_height)
        return if {target_width, target_height} == {@width, @height}

        @width = target_width
        @height = target_height

        if texture = @texture
          SDL.destroy(texture)
        end

        @texture = SDL.make(SDL::Texture, renderer, @width.to_i, @height.to_i,
          format: SDL::PixelFormat::ARGB8888,
          access: SDL::TextureAccess::STREAMING,
        )
      end

      def destroy : Nil
        return unless texture = @texture

        SDL.destroy(texture)
        @texture = nil
        @width = 0u32
        @height = 0u32
      end
    end

    defcase WindowHandle,
      window : SDL::Window,
      renderer : SDL::Renderer,
      texture : DynTexture,
      desired_backdrop : Pigment::RGBA,
      actual_backdrop : Pigment::RGBA,
      window_width : UInt32,
      window_height : UInt32,
      desired_width : UInt32,
      actual_width : UInt32,
      desired_height : UInt32,
      actual_height : UInt32,
      cursor : MouseCursor,
      command : Scenery::DrawCommand?,
      dirty_rects : Array(Scenery::Rect),
      mutation: true

    def initialize(@eventq : EventQueue)
      @handles = {} of UInt64 => WindowHandle

      # Load cursors.
      @cursors = {} of Symbol => SDL::Cursor
      {% for cursor in %w[default text wait crosshair progress nwse_resize nesw_resize ew_resize ns_resize move not_allowed pointer nw_resize n_resize ne_resize e_resize se_resize s_resize sw_resize w_resize] %}
        @cursors[{{cursor.id.symbolize}}] = SDL.make(SDL::Cursor, SDL::SystemCursor::{{cursor.id.upcase}})
      {% end %}
    end

    def self.start(wg : WaitGroup, alert : BlockingSignal, eventq : EventQueue) : MediaProgramQueue
      programs = MediaProgramQueue.new

      sdl_wg = WaitGroup.new(1)
      trigger_buffer = Deque(MediaProgram).new
      trigger_buffer_lock = Sync::Mutex.new

      Fiber::ExecutionContext::Isolated.new("[MxRack] SDL thread", spawn_context: Fiber::ExecutionContext.current) do
        SDL.init

        sdl_loop = SdlLoop.new(eventq)
        sdl_wg.done

        # Unfortunately, for whatever reason, there is this terrible flicker when you resize
        # the window vertically. Links and related:
        #   - https://discourse.libsdl.org/t/window-resizing-linux/55791
        #   - https://github.com/libsdl-org/SDL/issues/14763
        #   - https://github.com/libsdl-org/SDL/issues/11618
        #   - https://discourse.libsdl.org/t/how-to-stop-stretching-effect-when-border-drag-window-resizing/65365
        #   - https://github.com/lairworks/nas2d-core/issues/1216
        #
        # Without VSync it is rarer but appears too. (It does so after a while rather than
        # immediately: if you spam resize, it works fine, then there's a brief stutter,
        # and afterwards it seemingly goes "out of sync" and starts flickering.)
        #
        # I don't know how to fix this, nor do the pages above suggest anything that works.
        # This seems to be an X11 problem though.
        loop do
          event = SDL.wait
          next if event.is_a?(SDL::UnknownEvent)

          if event.is_a?(SDL::Trigger)
            event = trigger_buffer_lock.synchronize { trigger_buffer.shift }
          end

          sdl_loop.receive(event)
          sdl_loop.present
        end
      end

      # Wait until SDL is initialized.
      sdl_wg.wait

      spawn(name: "[MxRack] SDL thread event relay") do
        loop do
          program = programs.shift
          trigger_buffer_lock.synchronize do
            trigger_buffer << program
          end
          SDL.push(SDL::Trigger.new)
        end
      end

      # Signal completion to the caller.
      wg.done

      programs
    end

    # Changes the global mouse cursor to *goal* on behalf of *handle*.
    private def set_cursor(handle : WindowHandle, goal : MouseCursor) : Nil
      if handle.cursor.none? && !goal.none?
        SDL.cursor = true # Show cursor
      elsif !handle.cursor.none? && goal.none?
        SDL.cursor = false # Hide cursor
      end

      handle.cursor = goal

      case goal
      in .none?         then return
      in .arrow?        then key = :default
      in .text?         then key = :text
      in .blocked?      then key = :wait
      in .crosshair?    then key = :crosshair
      in .progress?     then key = :progress
      in .resize_tl_br? then key = :nwse_resize
      in .resize_bl_tr? then key = :nesw_resize
      in .resize_x?     then key = :ew_resize
      in .resize_y?     then key = :ns_resize
      in .resize?       then key = :move
      in .not_allowed?  then key = :not_allowed
      in .pointer?      then key = :pointer
      in .resize_tl?    then key = :nw_resize
      in .resize_t?     then key = :n_resize
      in .resize_tr?    then key = :ne_resize
      in .resize_r?     then key = :e_resize
      in .resize_br?    then key = :se_resize
      in .resize_b?     then key = :s_resize
      in .resize_bl?    then key = :sw_resize
      in .resize_l?     then key = :w_resize
      end

      return unless cursor = @cursors[key]?

      SDL.cursor = cursor
    end

    private def key_name?(code : SDL::Scancode) : Term?
      {% begin %}
        case code
        {% for n in 0..9 %}
        when .scancode_{{n}}? then Term.of({{n.stringify}})
        when .kp_{{n}}?  then Term.of(:np{{n}})
        {% end %}
        {% for n in 1..12 %}
        when .f{{n}}? then Term.of(:fn, {{n}})
        {% end %}
        {% for key in "abcdefghijklmnopqrstuvwxyz".chars %}
        when .{{key.id}}? then Term.of({{key.id.stringify}})
        {% end %}
        when .leftbracket?  then Term.of("[")
        when .rightbracket? then Term.of("]")
        when .semicolon?    then Term.of(";")
        when .apostrophe?   then Term.of("'")
        when .comma?        then Term.of(",")
        when .period?       then Term.of(".")
        when .slash?        then Term.of("/")
        when .minus?        then Term.of("-")
        when .equals?       then Term.of("=")
        when .backslash?    then Term.of("\\")
        when .space?        then Term.of(" ")
        when .grave?        then Term.of("`")
        when .up?           then Term.of(:up)
        when .down?         then Term.of(:dn)
        when .left?         then Term.of(:left)
        when .right?        then Term.of(:right)
        when .tab?          then Term.of(:tab)
        when .escape?       then Term.of(:escape)
        when .return?       then Term.of(:enter)
        when .insert?       then Term.of(:insert)
        when .delete?       then Term.of(:delete)
        when .backspace?    then Term.of(:backspace)
        when .home?         then Term.of(:home)
        when .end?          then Term.of(:end)
        when .pageup?       then Term.of(:pgup)
        when .pagedown?     then Term.of(:pgdn)
        end
      {% end %}
    end

    private def mod_name?(code : SDL::Scancode) : Term?
      case code
      when .lctrl?  then Term.of(:ctrl, :left)
      when .rctrl?  then Term.of(:ctrl, :right)
      when .lshift? then Term.of(:shift, :left)
      when .rshift? then Term.of(:shift, :right)
      when .lalt?   then Term.of(:alt, :left)
      when .ralt?   then Term.of(:alt, :right)
      end
    end

    # This overload is here primarily to support mapping CapsLock to Escape,
    # something that is my muscle memory. I rely on it very much. Notice how
    # we don't handle the CapsLock scancode above. So if CapsLock=Escape,
    # scancode is left unhandled, but we pick up the keycode.
    private def key_name?(code : SDL::Keycode) : Term?
      case code
      when .escape? then Term.of(:escape)
      end
    end

    private def execute(instr : WindowInstr::Create) : Nil
      if @handles.has_key?(instr.window_id)
        raise KeyError.new("Create() of window with an existing window id")
      end

      window, renderer = SDL.make({SDL::Window, SDL::Renderer}, instr.title, instr.width.to_i, instr.height.to_i, flags: SDL::WindowFlags::HIDDEN)
      texture = DynTexture.new

      command = nil
      dirty_rects = [] of Scenery::Rect
      handle = WindowHandle.new(window, renderer, texture,
        instr.backdrop, Pigment.black, # desired, actual backdrop (clear color)
        instr.width, instr.height,     # window width and height
        instr.width, 0,                # desired, actual texture width
        instr.height, 0,               # desired, actual texture height
        MouseCursor::Arrow,
        command,
        dirty_rects,
      )
      @handles[instr.window_id] = handle
    end

    private def execute(instr : WindowInstr::Delete) : Nil
      assert handle = @handles.delete(instr.window_id)

      SDL.hide(handle.window)
      handle.texture.destroy
      SDL.destroy(handle.window)

      # https://wiki.libsdl.org/SDL3/SDL_DestroyWindow
      #
      # > Note that on some platforms, the visible window may not actually be
      # > removed from the screen until the SDL event loop is pumped again,
      # > even though the SDL_Window is no longer valid after this call.
      SDL.pump
    end

    private def execute(instr : WindowInstr::Show) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.show(handle.window)
    end

    private def execute(instr : WindowInstr::Hide) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.hide(handle.window)
    end

    private def execute(instr : WindowInstr::Minimize) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.minimize(handle.window)
    end

    private def execute(instr : WindowInstr::Maximize) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.maximize(handle.window)
    end

    private def execute(instr : WindowInstr::Restore) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.restore(handle.window)
    end

    private def execute(instr : WindowInstr::SetTitle) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.set(handle.window, title: instr.title)
    end

    private def execute(instr : WindowInstr::SetResizable) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.set(handle.window, resizable: instr.resizable)
    end

    private def execute(instr : WindowInstr::SetBackdrop) : Nil
      return unless handle = @handles[instr.window_id]?

      handle.desired_backdrop = instr.backdrop
    end

    private def execute(instr : WindowInstr::SetSize) : Nil
      return unless handle = @handles[instr.window_id]?
      return if {handle.window_width, handle.window_height} == {instr.width, instr.height}

      # Resize the window. This will cause SDL::WindowResized eventually, at which
      # point we will resize the texture and so on.
      SDL.resize(handle.window, instr.width.to_i, instr.height.to_i)
      handle.window_width = instr.width
      handle.window_height = instr.height
    end

    private def execute(instr : WindowInstr::SetCursor) : Nil
      return unless handle = @handles[instr.window_id]?

      set_cursor(handle, instr.cursor)
    end

    private def execute(instr : WindowInstr::BeginInput) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.set(handle.window, input: true)
    end

    private def execute(instr : WindowInstr::EndInput) : Nil
      return unless handle = @handles[instr.window_id]?

      SDL.set(handle.window, input: false)
    end

    private def execute(instr : WindowInstr::Draw) : Nil
      return unless handle = @handles[instr.window_id]?

      handle.command = instr.command
      handle.dirty_rects.concat(instr.dirty_rects)
    end

    private def handle(event program : WindowProgram)
      program.each_instr { |instr| execute(instr) }
    end

    private def handle(event : SDL::MouseAdded) : Nil
      @eventq << MousePlugged.new(event.mouse_id)
    end

    private def handle(event : SDL::MouseRemoved) : Nil
      @eventq << MouseUnplugged.new(event.mouse_id)
    end

    private def handle(event : SDL::KnownEvent) : Nil
      @handles.each do |window_id, handle|
        next unless SDL.id(handle.window) == event.window_id

        handle(window_id, handle, event)
        return
      end
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowResized) : Nil
      handle.desired_width = event.width.to_u32
      handle.desired_height = event.height.to_u32

      @eventq << WindowReceived.new(window_id, WindowSizeChanged.new(event.width.to_u32, event.height.to_u32))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowMouseFocusGained) : Nil
      # Since SDL cursors are global (for whatever reason), we have to handle window-local
      # cursors ourselves. When the mouse enters the window we need to change the cursor
      # to that window's cursor.
      set_cursor(handle, handle.cursor)

      @eventq << WindowReceived.new(window_id, WindowHoverChanged.new(true))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowMouseFocusLost) : Nil
      # I don't think we have to "revert" our setting of the cursor.

      @eventq << WindowReceived.new(window_id, WindowHoverChanged.new(false))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowFocusGained) : Nil
      @eventq << WindowReceived.new(window_id, WindowFocusChanged.new(true))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowFocusLost) : Nil
      @eventq << WindowReceived.new(window_id, WindowFocusChanged.new(false))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowClosed) : Nil
      # Hide the window because the user expects that to happen when they press WindowClosed.
      # The rest of handling is up to the circuit.
      execute(WindowInstr::Hide.new(window_id))

      @eventq << WindowReceived.new(window_id, WindowFormChanged.new(:closed))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowMaximized) : Nil
      @eventq << WindowReceived.new(window_id, WindowFormChanged.new(:maximized))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowMinimized) : Nil
      @eventq << WindowReceived.new(window_id, WindowFormChanged.new(:minimized))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowRestored) : Nil
      @eventq << WindowReceived.new(window_id, WindowFormChanged.new(:floating))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::WindowExposed) : Nil
      handle.dirty_rects << Scenery::Rect.inf
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::MouseMoved) : Nil
      @eventq << WindowReceived.new(window_id, MouseMoved.new(event.mouse_id, event.x, event.y))
    end

    private def mouse_button(button : SDL::MouseButton) : Term
      case button
      in .left?   then Term.of(:left)
      in .middle? then Term.of(:middle)
      in .right?  then Term.of(:right)
      in .x1?     then Term.of(:back)
      in .x2?     then Term.of(:forward)
      end
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::MouseButtonUp) : Nil
      button = mouse_button(event.button)
      @eventq << WindowReceived.new(window_id, MouseButtonReleased.new(event.mouse_id, button))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::MouseButtonDown) : Nil
      button = mouse_button(event.button)
      @eventq << WindowReceived.new(window_id, MouseButtonPressed.new(event.mouse_id, button, event.clicks.to_u32))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::MouseWheelScrolled) : Nil
      pressed = Pf::Kit.stack_array(MouseButtonPressed, 2)

      # https://wiki.libsdl.org/SDL3/SDL_MouseWheelEvent
      #
      # > The amount scrolled vertically, positive away from the user and negative toward the user.
      if event.dy < 0
        pressed << MouseButtonPressed.new(event.mouse_id, Term.of(:"wheel-dn"), (-event.dy).to_u32)
      elsif event.dy > 0
        pressed << MouseButtonPressed.new(event.mouse_id, Term.of(:"wheel-up"), event.dy.to_u32)
      end

      # https://wiki.libsdl.org/SDL3/SDL_MouseWheelEvent
      #
      # > The amount scrolled horizontally, positive to the right and negative to the left.
      if event.dx < 0
        pressed << MouseButtonPressed.new(event.mouse_id, Term.of(:"wheel-left"), (-event.dx).to_u32)
      elsif event.dx > 0
        pressed << MouseButtonPressed.new(event.mouse_id, Term.of(:"wheel-right"), event.dx.to_u32)
      end

      pressed.each do |message|
        @eventq << WindowReceived.new(window_id, message)
      end
      pressed.each do |message|
        @eventq << WindowReceived.new(window_id, MouseButtonReleased.new(event.mouse_id, message.button))
      end
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::KeyboardKeyDown) : Nil
      name = key_name?(event.scancode) || key_name?(event.keycode)

      pass do
        next if name.nil?

        repr = Term::Dict.build do |commit|
          commit << :key << name

          event.modifiers.each do |modifier|
            case modifier
            when .l_shift?, .r_shift? then commit.with(:shift, true)
            when .l_ctrl?, .r_ctrl?   then commit.with(:ctrl, true)
            when .l_alt?, .r_alt?     then commit.with(:alt, true)
            end
          end
        end
        repr = Term.of(repr)

        if event.repeat
          @eventq << WindowReceived.new(window_id, RuneReleased.new(repr))
          @eventq << WindowReceived.new(window_id, RunePressed.new(repr))
        else
          @eventq << WindowReceived.new(window_id, RunePressed.new(repr))
        end
      end

      pass do
        next if event.repeat

        # Modifier keys are eligible for KeyPressed but they do not have a `key_name?`.
        # We have to handle them differently.
        name ||= mod_name?(event.scancode)
        next if name.nil?

        key = Term.of(:key, name)
        @eventq << WindowReceived.new(window_id, KeyPressed.new(event.keyboard_id, key))
      end
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::KeyboardKeyUp) : Nil
      name = key_name?(event.scancode) || key_name?(event.keycode)

      if name
        repr = Term.of(:key, name)
        @eventq << WindowReceived.new(window_id, RuneReleased.new(repr))
        @eventq << WindowReceived.new(window_id, KeyReleased.new(event.keyboard_id, repr))
        return
      end

      # If `key_name?` doesn't work, it could still be a modifier, which is eligible
      # for KeyReleased.
      name ||= mod_name?(event.scancode)
      return if name.nil?

      repr = Term.of(:key, name)
      @eventq << WindowReceived.new(window_id, KeyReleased.new(event.keyboard_id, repr))
    end

    private def handle(window_id : UInt64, handle : WindowHandle, event : SDL::TextEntered) : Nil
      rune = Term.of(:rune, event.rune)
      @eventq << WindowReceived.new(window_id, RunePressed.new(rune))
      @eventq << WindowReceived.new(window_id, RuneReleased.new(rune))
    end

    def receive(event)
      handle(event)
    end

    def present(handle : WindowHandle) : Nil
      unless {handle.desired_width, handle.desired_height} == {handle.actual_width, handle.actual_height}
        handle.texture.reserve(handle.renderer, handle.desired_width, handle.desired_height)
        handle.actual_width = handle.desired_width
        handle.actual_height = handle.desired_height
        handle.dirty_rects << Scenery::Rect.inf
      end

      unless handle.desired_backdrop == handle.actual_backdrop
        handle.actual_backdrop = handle.desired_backdrop
        handle.dirty_rects << Scenery::Rect.inf
        # We only need to set it once here, after every change, because nothing
        # else ever modifies this.
        SDL.set(handle.renderer, color: {handle.actual_backdrop.r8, handle.actual_backdrop.g8, handle.actual_backdrop.b8, 255u8})
      end

      if (command = handle.command) && handle.dirty_rects.present?
        handle.texture.lock do |pixels, pitch|
          screen = Scenery::PixelRect.new(pixels.as(UInt8*), handle.actual_width.to_i, handle.actual_height.to_i, pitch)
          # Although calling to_readonly_slice on an Array is generally unsafe
          # (since the array may reallocate), here it is perfectly safe: rasterize()
          # will not modify it (it has no access to the array; and the slice
          # is readonly), nor will we or anything else, so no way it can reallocate
          # and so on.
          Scenery::Safe.rasterize(screen, command, handle.actual_backdrop, handle.dirty_rects.to_readonly_slice)
        end

        handle.dirty_rects.clear
      end

      SDL.clear(handle.renderer)
      handle.texture.get do |texture|
        SDL.copy(texture, handle.renderer, handle.actual_width.to_i, handle.actual_height.to_i)
      end
      SDL.present(handle.renderer)
    end

    def present
      @handles.each_value do |handle|
        present(handle)
      end
    end
  end
end
