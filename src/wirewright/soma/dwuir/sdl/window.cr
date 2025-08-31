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
        viewer_context : Viewer::Context,
        width : Int32,
        height : Int32,
        @backdrop : Color,
      )
        @buffer = SDL::Texture.new(renderer, width, height, LibSDL::PixelFormatEnum::ARGB8888)
        @screen = PixelRect.new(0, 0, width, height)
        @viewer = Viewer.new(@screen, viewer_context)
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

    defcase State, sys : SDL::Window::WithDestroy, renderer : SDL::Renderer::WithDestroy, frame : Frame, conf : Conf do
      include Some

      @valid = true

      def sys : SDL::Window
        unless @valid
          raise "BUG: access to expended state"
        end

        @sys
      end

      def renderer : SDL::Renderer
        unless @valid
          raise "BUG: access to expended state"
        end

        @renderer
      end

      def close : Nil
        return unless @valid

        @renderer.destroy
        @sys.destroy

        @valid = false
      end

      def_equals_and_hash sys.id, conf
    end

    # :nodoc:
    record Context, viewer : Viewer::Context, cursors : CursorStore

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
    def context(viewer_context : Viewer::Context) : Context
      Context.new(viewer_context, cursors: CursorStore.new)
    end

    # Performs a window state transition: transitions from an existing state
    # *window* to the next state defined by a DwUIR window *spec*.
    #
    # See also: `soma.dwuir.window` in the doctool.
    def next(context ctx : Context, window : Any, spec : Term) : Any
      unless conf1 = conf?(spec)
        case window
        in None
        in Some then close(window)
        end

        return None.new
      end

      case window
      in None
        # Window did not exist and only now assumed valid form. Construct.
        sys = SDL::Window::WithDestroy.new(conf1.title, conf1.width, conf1.height)
        sys.resizable = conf1.resizable

        change_cursor(ctx, sys, :arrow, conf1.cursor)

        renderer = SDL::Renderer::WithDestroy.new(sys, SDL::Renderer::Flags::ACCELERATED)

        frame = Frame.new(renderer, ctx.viewer, conf1.width, conf1.height, conf1.backdrop)
        frame.show(conf1.content)

        State.new(sys, renderer, frame, conf1)
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
          frame = Frame.new(window.renderer, ctx.viewer, conf1.width, conf1.height, conf1.backdrop)
          window = window.copy_with(frame: frame)

          # Sync window geometry *but avoid noop overwrites* -- in my case XFCE
          # doesn't seem happy about them and drops some resize events.
          unless window.sys.size == {conf1.width, conf1.height}
            window.sys.size = {conf1.width, conf1.height}
          end

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

    def close(window : Any) : Nil
      some(window, &.close)
    end

    private def dispatch(live : Set(Some), event : SDL::Event::MouseMotion, fn)
      return unless target = target?(live, event)

      Event.term(Event::MouseMotion.new(event.which, event.x, event.y)) do |term|
        fn.call(target, term)
      end
    end

    private def dispatch(live : Set(Some), event : SDL::Event::MouseButton, fn)
      return unless target = target?(live, event)

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
        return
      end

      case event
      when .pressed?
        Event.term(Event::MouseDn.new(event.which, button, event.x, event.y, event.clicks)) do |term|
          fn.call(target, term)
        end
      when .released?
        Event.term(Event::MouseUp.new(event.which, button, event.x, event.y, event.clicks)) do |term|
          fn.call(target, term)
        end
      end
    end

    private def dispatch(live : Set(Some), event : SDL::Event::MouseWheel, fn)
      return unless target = target?(live, event)

      Event.term(Event::MouseWheel.new(event.which, event.x, event.y*-1)) do |term|
        fn.call(target, term)
      end
    end

    private def dispatch(live : Set(Some), event : SDL::Event::Keyboard, fn)
      return unless target = target?(live, event)

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
        when .space?     then key = Event::Key::Space
        when .grave?     then key = Event::Key::Backquote
        when .up?        then key = Event::Key::Up
        when .down?      then key = Event::Key::Dn
        when .left?      then key = Event::Key::Left
        when .right?     then key = Event::Key::Right
        when .tab?       then key = Event::Key::Tab
        when .escape?    then key = Event::Key::Esc
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
          return
        end
      {% end %}

      ctrl = event.keysym.mod.lctrl? || event.keysym.mod.rctrl?
      shift = event.keysym.mod.lshift? || event.keysym.mod.rshift?
      alt = event.keysym.mod.lalt? || event.keysym.mod.ralt?

      case event.type
      when .keyup?
        Event.term(Event::KeyUp.new(key, ctrl, shift, alt)) do |term|
          fn.call(target, term)
        end
      when .keydown?
        Event.term(Event::KeyDn.new(key, ctrl, shift, alt)) do |term|
          fn.call(target, term)
        end
      end
    end

    private def dispatch(live : Set(Some), event : SDL::Event::TextInput, fn)
      return unless target = target?(live, event)

      Event.term(Event::KeyInput.new(String.new(event.text.to_slice, truncate_at_null: true))) do |term|
        fn.call(target, term)
      end
    end

    private def dispatch(live : Set(Some), event : SDL::Event::Window, fn)
      return unless target = target?(live, event)

      case SDL::Window::Event.new(event.event)
      when .resized?
        # NOTE: resized is only triggered on user resize, programmatic
        # resize doesn't trigger it which is actually what we want here!
        Event.term(Event::WindowResized.new(event.data1, event.data2)) do |term|
          fn.call(target, term)
        end
      when .close?
        live.delete(target)
      when .exposed?
        present(target)
      end
    end

    private def dispatch(live : Set(Some), event, fn)
    end

    # Polls and yields events from each open window of *windows*. Since this method
    # also handles window closure, a set of live (open) windows is returned for
    # the caller to sync with.
    def poll(windows : Enumerable(Any), &fn : Some, Term ->) : Set(Some)
      live = Set(Some).new

      windows.each do |window|
        next unless window.is_a?(Some)

        live << window
      end

      loop do
        break unless live.present?
        break unless event = SDL::Event.poll

        dispatch(live, event, fn)
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
