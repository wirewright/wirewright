module Ww::Soma::DwUIR
  module Window
    Log = ::Log.for(self)

    # :nodoc:
    #
    # Represents the "invalidatable" part of the window. Size and backdrop
    # changes cause this object to be recreated from scratch.
    class Frame
      def initialize(
        renderer : SDL::Renderer,
        compositor : Compositor,
        platform : Platform,
        width : Int32,
        height : Int32,
        @backdrop : Color,
      )
        @buffer = SDL::Texture.new(renderer, width, height, LibSDL::PixelFormatEnum::ARGB8888)
        @screen = PixelRect.new(0, 0, width, height)
        @viewer = Viewer.new(@screen, compositor, platform)
      end

      def show(content : Term)
        damage = @viewer.show(content, bg: @backdrop)

        @buffer.lock do |pixels, pitch|
          damage.each do |rect|
            region = @screen.region(rect)
            region.each_pixel_with_coords do |pixel, i, j|
              pixels[@screen.width * j + i] = pixel.argb
            end
          end
        end
      end

      def present(renderer)
        renderer.draw_color = SDL::Color[255, 255, 255, 255]
        renderer.clear
        renderer.copy(@buffer, SDL::Rect[0, 0, @buffer.width, @buffer.height], SDL::Rect[0, 0, @buffer.width, @buffer.height])
        renderer.present
      end

      def inspect(io)
        io << "frame(" << @screen.width << "x" << @screen.height << ")"
      end
    end

    alias Any = None | Some

    # Represents a null or uninitialized window safely. Participates in transitions
    # from a closed window to an open one (`Some`); and vice versa, from an open
    # one to a closed one (i.e., the transition `Some` -> `None` represents
    # window closure).
    record None

    # Represents an open window.
    record Some, sys : SDL::Window, renderer : SDL::Renderer, frame : Frame, conf : Conf do
      # Open windows are compared and hashed by their SDL (`sys`) window id.
      def_equals_and_hash sys.id
    end

    # Holds the objects shared between windows and window transitions.
    defcase Context,
      platform : Platform,
      compositor : Compositor,
      cursors : CursorStore

    # Stores SDL system cursor instances.
    class CursorStore
      alias Cursor = LibSDL::Cursor*

      @cursors = {} of Symbol => Cursor

      def finalize
        @cursors.each { |_, cursor| LibSDL.free_cursor(cursor) }

        # Since the finalizer was called, @cursors is now unreachable too, so
        # there is no point clearing it.
      end

      {% for cursor in %w[ibeam wait sizeall arrow hand wait crosshair sizenwse sizenesw sizewe sizens sizeall no] %}
      # Returns the *{{cursor.id}}* SDL system cursor.
      def {{cursor.id}} : Cursor
        @cursors.put_if_absent({{cursor.id.symbolize}}) do
          LibSDL.create_system_cursor(LibSDL::SystemCursor::{{cursor.id.upcase}})
        end
      end
    {% end %}
    end

    # Constructs a window context object.
    #
    # *platform* and *compositor* will be reused by all windows & between
    # window transitions.
    def context(platform : Platform, compositor : Compositor) : Context
      Context.new(platform, compositor, cursors: CursorStore.new)
    end

    # Performs a window state transition: transitions from an existing state
    # *window* to the next state defined by a DwUIR window *spec*.
    #
    # See also: `soma.dwuir.window` in the doctool.
    def next(context ctx : Context, window : Any, spec : Term) : Any
      unless conf1 = conf?(spec)
        case window
        in None
        in Some
          # Drop the window it. Let the GC do cleanup. On our end we simply
          # hide it from the user.
          hide(window)
        end

        return None.new
      end

      case window
      in None
        # Window did not exist and only now assumed valid form. Construct.
        sys = SDL::Window.new(conf1.title, conf1.width, conf1.height)
        sys.resizable = conf1.resizable

        change_cursor(ctx, sys, :arrow, conf1.cursor)

        renderer = SDL::Renderer.new(sys, SDL::Renderer::Flags::ACCELERATED)

        frame = Frame.new(renderer, ctx.compositor, ctx.platform, conf1.width, conf1.height, conf1.backdrop)
        frame.show(conf1.content)

        Some.new(sys, renderer, frame, conf1)
      in Some
        conf0 = window.conf

        # Window existed before and is still in valid form. See what changed,
        # if anything.
        redraw = false

        # Is it window title that changed? Sync.
        unless conf0.title == conf1.title
          window.sys.title = conf1.title
        end

        # Is it resizability that changed? Sync.
        unless conf0.resizable == conf1.resizable
          window.sys.resizable = conf1.resizable
        end

        unless conf0.cursor == conf1.cursor
          change_cursor(ctx, window.sys, conf0.cursor, conf1.cursor)
        end

        # Change of certain properties invalidates the frame.
        unless {conf0.width, conf0.height, conf0.backdrop} == {conf1.width, conf1.height, conf1.backdrop}
          frame = Frame.new(window.renderer, ctx.compositor, ctx.platform, conf1.width, conf1.height, conf1.backdrop)
          window = window.copy_with(frame: frame)
          window.sys.size = {conf1.width, conf1.height}
          redraw = true
        end

        # Is it window content that changed?
        redraw ||= conf0.content != conf1.content

        if redraw
          window.frame.show(conf1.content)
        end

        window.copy_with(conf: conf1)
      end
    end

    private def some(window : Some, &)
      yield window
    end

    private def some(window : None, &)
    end

    # Syncs the SDL window content and frame content of *window*.
    def present(window : Any) : Nil
      some(window) { |it| it.frame.present(it.renderer) }
    end

    # Syncs the SDL window content and frame content of all of *windows*.
    def present(windows : Enumerable(Any)) : Nil
      windows.each { |window| present(window) }
    end

    # Shows *window* if it is hidden.
    def show(window : Any) : Nil
      some(window, &.sys.show)
    end

    # Hides *window* if it is shown.
    def hide(window : Any) : Nil
      some(window, &.sys.hide)
    end

    # Polls and yields events from each open window of *windows*. Since this method
    # also handles window closure, a set of live (open) windows is returned for
    # the caller to sync with.
    def poll(windows : Enumerable(Any), & : Some, Term ->) : Set(Some)
      live = Set(Some).new

      windows.each do |window|
        next unless window.is_a?(Some)

        live << window
      end

      loop do
        break unless live.present?
        break unless event = SDL::Event.poll

        case event
        when SDL::Event::Quit
          windows.each { |window| hide(window) }
          live.clear
        when SDL::Event::MouseMotion
          next unless target = target?(windows, event)

          Event.term(Event::MouseMotion.new(event.which, event.x, event.y)) do |term|
            yield target, term
          end
        when SDL::Event::MouseButton
          next unless target = target?(windows, event)

          case event.button
          when LibSDL::BUTTON_LEFT
            button = Event::MouseButton::Left
          when LibSDL::BUTTON_MIDDLE
            button = Event::MouseButton::Middle
          when LibSDL::BUTTON_RIGHT
            button = Event::MouseButton::Right
          when LibSDL::BUTTON_X1
            button = Event::MouseButton::Backward
          when LibSDL::BUTTON_X2
            button = Event::MouseButton::Forward
          else
            Log.debug { "unhandled SDL mouse button id #{event.button}" }
            next
          end

          case event
          when .pressed?
            Event.term(Event::MouseDn.new(event.which, button, event.x, event.y, event.clicks)) do |term|
              yield target, term
            end
          when .released?
            Event.term(Event::MouseUp.new(event.which, button, event.x, event.y, event.clicks)) do |term|
              yield target, term
            end
          end
        when SDL::Event::Keyboard
          next unless target = target?(windows, event)

          key = nil

          {% begin %}
            case scancode = event.keysym.scancode
            {% for key in "0123456789".chars %}
            when .key_{{key.id}}?
              key = Event::Key::Digit{{key.id}}
            when .kp_{{key.id}}?
              key = Event::Key::Np{{key.id}}
            {% end %}
            {% for key in %w[f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12] %}
            when .{{key.id}}?
              key = Event::Key::{{key.id.upcase}}
            {% end %}
            {% for key in "abcdefghijklmnopqrstuvwxyz".chars %}
            when .{{key.id}}?
              key = Event::Key::{{key.id.upcase}}
            {% end %}
            when .grave?     then key = Event::Key::Backquote
            when .up?        then key = Event::Key::Up
            when .down?      then key = Event::Key::Dn
            when .left?      then key = Event::Key::Left
            when .right?     then key = Event::Key::Right
            when .tab?       then key = Event::Key::Tab
            when .return?    then key = Event::Key::Enter
            when .insert?    then key = Event::Key::Insert
            when .delete?    then key = Event::Key::Delete
            when .backspace? then key = Event::Key::Backspace
            when .home?      then key = Event::Key::Home
            when .end?       then key = Event::Key::End
            when .pageup?    then key = Event::Key::PgUp
            when .pagedown?  then key = Event::Key::PgDn
            when .lctrl?     then key = Event::Key::Cl
            when .rctrl?     then key = Event::Key::Cr
            when .lshift?    then key = Event::Key::Sl
            when .rshift?    then key = Event::Key::Sr
            when .lalt?      then key = Event::Key::Al
            when .ralt?      then key = Event::Key::Ar
            else
              Log.debug { "unhandled SDL scancode #{scancode}" }
              next
            end
          {% end %}

          ctrl = event.keysym.mod.lctrl? || event.keysym.mod.rctrl?
          shift = event.keysym.mod.lshift? || event.keysym.mod.rshift?
          alt = event.keysym.mod.lalt? || event.keysym.mod.ralt?

          case event.type
          when .keyup?
            Event.term(Event::KeyUp.new(key, ctrl, shift, alt)) do |term|
              yield target, term
            end
          when .keydown?
            Event.term(Event::KeyDn.new(key, ctrl, shift, alt)) do |term|
              yield target, term
            end
          end
        when SDL::Event::TextInput
          next unless target = target?(windows, event)

          Event.term(Event::KeyInput.new(String.new(event.text.to_slice, truncate_at_null: true))) do |term|
            yield target, term
          end
        when SDL::Event::Window
          next unless target = target?(windows, event)

          case SDL::Window::Event.new(event.event)
          when .resized?
            Event.term(Event::WindowResized.new(event.data1, event.data2)) do |term|
              yield target, term
            end
          when .close?
            hide(target)
            live.delete(target.sys.id)
          end
        end
      end

      live
    end

    private def target?(window : Some, id : UInt32) : Some?
      window.sys.id == id ? window : nil
    end

    private def target?(window : None, id : UInt32) : Some?
    end

    private def target?(windows : Enumerable(Any), event) : Some?
      windows.leftmost? { |window| target?(window, event.window_id) }
    end

    private def change_cursor(ctx, sys : SDL::Window, cursor0 : Cursor, cursor1 : Cursor)
      return if cursor0 == cursor1

      if !cursor0.none? && cursor1.none?
        # Hide cursor.
        LibSDL.show_cursor(0)
      elsif cursor0.none? && !cursor1.none?
        # Show cursor.
        LibSDL.show_cursor(1)
      end

      case cursor1
      in .none?
      in .text?         then LibSDL.set_cursor(ctx.cursors.ibeam)
      in .wait?         then LibSDL.set_cursor(ctx.cursors.wait)
      in .grab?         then LibSDL.set_cursor(ctx.cursors.sizeall)
      in .arrow?        then LibSDL.set_cursor(ctx.cursors.arrow)
      in .pointer?      then LibSDL.set_cursor(ctx.cursors.hand)
      in .progress?     then LibSDL.set_cursor(ctx.cursors.wait)
      in .crosshair?    then LibSDL.set_cursor(ctx.cursors.crosshair)
      in .resize_tl_br? then LibSDL.set_cursor(ctx.cursors.sizenwse)
      in .resize_bl_tr? then LibSDL.set_cursor(ctx.cursors.sizenesw)
      in .resize_x?     then LibSDL.set_cursor(ctx.cursors.sizewe)
      in .resize_y?     then LibSDL.set_cursor(ctx.cursors.sizens)
      in .resize?       then LibSDL.set_cursor(ctx.cursors.sizeall)
      in .not_allowed?  then LibSDL.set_cursor(ctx.cursors.no)
      end
    end
  end
end
