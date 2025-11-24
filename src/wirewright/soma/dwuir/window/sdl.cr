require "sdl"

# Patches to SDL

lib LibSDL
  fun set_window_resizable = SDL_SetWindowResizable(window : Window*, resizable : Bool) : Bool
  fun get_window_id = SDL_GetWindowID(window : Window*) : UInt32
end

class SDL::Window
  alias Event = LibSDL::WindowEventID

  def id : UInt32
    id = LibSDL.get_window_id(@window)
    if id.zero?
      raise SDL::Error.new("SDL_GetWindowID")
    end
    id
  end

  def resizable=(value : Bool) : Bool
    LibSDL.set_window_resizable(@window, value)

    value
  end
end

class SDL::Window::WithDestroy < SDL::Window
  @destroyed = false

  def destroy
    return if @destroyed

    @destroyed = true

    LibSDL.destroy_window(self)
  end

  def finalize
    destroy
  end
end

class SDL::Renderer::WithDestroy < SDL::Renderer
  @destroyed = false

  def destroy
    return if @destroyed

    @destroyed = true

    LibSDL.destroy_renderer(self)
  end

  def finalize
    destroy
  end
end

module Ww::Soma::DwUIR
  # Window implementation to display DwUIR in an OS window.
  #
  # NOTE: You must explicitly initialize SDL first, by wrapping your code
  # in `setup`. You will normally do this somewhere near the top-level, or
  # at the top-level.
  module Window::SDL
    extend self

    Log = ::Log.for(self)

    alias Any = None | Some

    # Represents a null or uninitialized window safely. Participates in transitions
    # from a closed window to an open one (`Some`); and vice versa, from an open
    # one to a closed one (i.e., the transition `Some` -> `None` represents
    # window closure).
    record None

    # :nodoc:
    #
    # Represents the "invalidatable" part of the window. Size and backdrop
    # changes cause this object to be recreated from scratch.
    class Frame
      def initialize(
        renderer : ::SDL::Renderer::WithDestroy,
        @requests : Channel(Protocol::Request),
        width : Int32,
        height : Int32,
        @backdrop : Color,
      )
        @screen = PixelRect.new(0, 0, width, height)
        @buffer = ::SDL::Texture.new(renderer, width, height, LibSDL::PixelFormatEnum::ARGB8888)
      end

      def show(content : Term)
        response = Sync::Future(Protocol::FrameResponse).new
        request = Protocol::FrameRequest.new(content, @screen, @backdrop, response)
        @requests.send(request)

        @buffer.lock do |pixels, pitch|
          response.get.damage.each do |rect|
            region = @screen.region(rect)
            region.each_pixel_with_coords do |pixel, i, j|
              pixels[@screen.width * j + i] = pixel.argb
            end
          end
        end
      end

      def present(renderer)
        renderer.draw_color = ::SDL::Color.new(*@backdrop.rgb, 255)
        renderer.clear
        renderer.copy(@buffer, ::SDL::Rect.new(0, 0, @buffer.width, @buffer.height), ::SDL::Rect.new(0, 0, @buffer.width, @buffer.height))
        renderer.present
      end

      def inspect(io)
        io << "frame(" << @buffer.width << "x" << @buffer.height << ")"
      end
    end

    # Represents the pieces of window configuration relevant to graphical
    # Soma/DwUIR.
    #
    # *backdrop* is the background color (clear color) of the window. You
    # are advised to use it instead of a large background rectangle; the current
    # compositor/rerender machinery yields degenerate performance and damage
    # contagion on large rects at the moment.
    defcase Conf,
      title : String,
      width : Int32,
      height : Int32,
      resizable : Bool,
      cursor : Cursor,
      backdrop : Color,
      content : Term

    # Parses a window spec *spec* and returns the corresponding `Conf`, or `nil`
    # if parsing failed.
    #
    # See also: `soma.dwuir.window.os` in doctool.
    private def conf?(spec : Term) : Conf?
      # |@ soma.dwuir.window.os
      #
      # |@block
      # Defines the properties of a system window to display *content*.
      # |@endblock
      #
      # |@key title -- Sets the title of the window.
      #
      # |@key width -- Sets the width of the window (in pixels).
      #
      # |@key height -- Sets the height of the window (in pixels).
      #
      # |@key resizable -- Determines whether window resizing should be allowed.
      #
      # |@key cursor soma.dwuir.cursor -- Sets the current mouse cursor.
      #
      # |@key backdrop soma.dwuir.color -- Sets the background color (clear color)
      # of the window.
      Term.matchpi?(spec,
        <<-WWML
          (window content_*
            ⍊ width_: (%number +i16)
              height_: (%number +i16)
              title⋮ "Untitled"
              resizable⋮ true
              cursor⋮ arrow
              backdrop_⋮ white)
        WWML
      ) do
        Conf.new(
          title: title.to(String),
          width: width.to(Int32),
          height: height.to(Int32),
          resizable: resizable.to(Bool),
          cursor: Cursor.parse(cursor),
          backdrop: Color.term(backdrop, fallback: Color.named("white")),
          content: content,
        )
      end
    end

    private def conf?(spec : Nil) : Conf?
    end

    # Represents an open window.
    defcase Some,
      sys : ::SDL::Window::WithDestroy,
      renderer : ::SDL::Renderer::WithDestroy,
      frame : Frame,
      conf : Conf

    class Some
      @valid = true

      def sys : ::SDL::Window
        unless @valid
          raise "BUG: access to expended state"
        end

        @sys
      end

      def renderer : ::SDL::Renderer
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

    # An object issued by `setup`. It is a "capability"; the fact that you
    # have an instance of it acts as a proof that you called `setup`, so that
    # functions that rely on that can rely on that with a bit more confidence.
    class SetupProof
      # :nodoc:
      property? valid : Bool = true
    end

    private def check(proof : SetupProof) : Nil
      assert proof.valid?
    end

    # Initializes SDL. Gives you a proof object which you will have to pass
    # to functions that depend on initialized SDL. The proof object must not
    # outlive the block.
    def setup(& : SetupProof ->)
      ::SDL.set_hint("SDL_NO_SIGNAL_HANDLERS", "1")
      ::SDL.set_hint("SDL_QUIT_ON_LAST_WINDOW_CLOSE", "0")
      ::SDL.init(::SDL::Init::VIDEO)

      proof = SetupProof.new

      begin
        yield proof
      ensure
        proof.valid = false

        ::SDL.quit
      end
    end

    # :nodoc:
    record Context, requests : Channel(Protocol::Request), cursors : CursorStore

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

    private def change_cursor(ctx, sys : ::SDL::Window, cursor0 : Cursor, cursor1 : Cursor)
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

    # Constructs a window context object.
    def context(proof : SetupProof, requests : Channel(Protocol::Request)) : Context
      check(proof)

      Context.new(requests, cursors: CursorStore.new)
    end

    # Performs a window state transition: transitions from an existing state
    # *window* to the next state defined by a DwUIR window *spec*.
    #
    # See also: `soma.dwuir.window` in the doctool.
    def next(proof : SetupProof, context ctx : Context, window : Any, spec : Term?) : Any
      check(proof)

      unless conf1 = conf?(spec)
        case window
        in None
        in Some
          change_cursor(ctx, window.sys, window.conf.cursor, :arrow)

          window.close
        end

        GC.collect

        return None.new
      end

      case window
      in None
        # Window did not exist and only now assumed valid form. Construct.
        sys = ::SDL::Window::WithDestroy.new(conf1.title, conf1.width, conf1.height)
        sys.resizable = conf1.resizable

        change_cursor(ctx, sys, :arrow, conf1.cursor)

        renderer = ::SDL::Renderer::WithDestroy.new(sys, ::SDL::Renderer::Flags::ACCELERATED)

        frame = Frame.new(renderer, ctx.requests, conf1.width, conf1.height, conf1.backdrop)
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
          frame = Frame.new(window.renderer, ctx.requests, conf1.width, conf1.height, conf1.backdrop)
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

    # :nodoc:
    def present(proof : SetupProof, window : Some) : Nil
      check(proof)

      window.frame.present(window.renderer)
    end

    # :nodoc:
    def present(proof : SetupProof, window : None) : Nil
      check(proof)
    end

    {% if flag?(:docs) %}
      # Syncs the OS window content and frame content of *window*.
      def present(proof : SetupProof, window : Any) : Nil
      end
    {% end %}

    # Syncs the OS window content and frame content of all of *windows*.
    def present(proof : SetupProof, windows : Enumerable(Any)) : Nil
      windows.each { |window| present(proof, window) }
    end

    private def dispatch(window_id, event, sink)
      Event.term(event) do |term|
        sink.call(Term.of(:event, window_id, term))
      end
    end

    private def dispatch(event : ::SDL::Event::MouseMotion, sink)
      dispatch(event.window_id, Event::MouseMotion.new(event.which, event.x, event.y), sink)
    end

    private def dispatch(event : ::SDL::Event::MouseButton, sink)
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
        tr = Event::MouseDn.new(event.which, button, event.x, event.y, event.clicks.to_i)
      when .released?
        tr = Event::MouseUp.new(event.which, button, event.x, event.y, event.clicks.to_i)
      else
        return
      end

      dispatch(event.window_id, tr, sink)
    end

    private def dispatch(event : ::SDL::Event::MouseWheel, sink)
      dispatch(event.window_id, Event::MouseWheel.new(event.which, event.x, event.y*-1), sink)
    end

    private def dispatch(event : ::SDL::Event::Keyboard, sink)
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
        when .leftbracket?  then key = Event::Key::Lsqb
        when .rightbracket? then key = Event::Key::Rsqb
        when .semicolon?    then key = Event::Key::Semicolon
        when .apostrophe?   then key = Event::Key::Quote
        when .comma?        then key = Event::Key::Comma
        when .period?       then key = Event::Key::Period
        when .slash?        then key = Event::Key::Slash
        when .minus?        then key = Event::Key::Minus
        when .equals?       then key = Event::Key::Equals
        when .backslash?    then key = Event::Key::Backslash
        when .space?        then key = Event::Key::Space
        when .grave?        then key = Event::Key::Backquote
        when .up?           then key = Event::Key::Up
        when .down?         then key = Event::Key::Dn
        when .left?         then key = Event::Key::Left
        when .right?        then key = Event::Key::Right
        when .tab?          then key = Event::Key::Tab
        when .escape?       then key = Event::Key::Escape
        when .return?       then key = Event::Key::Enter
        when .insert?       then key = Event::Key::Insert
        when .delete?       then key = Event::Key::Delete
        when .backspace?    then key = Event::Key::Backspace
        when .home?         then key = Event::Key::Home
        when .end?          then key = Event::Key::End
        when .pageup?       then key = Event::Key::PgUp
        when .pagedown?     then key = Event::Key::PgDn
        when .lctrl?        then key = Event::Key::Cl
        when .rctrl?        then key = Event::Key::Cr
        when .lshift?       then key = Event::Key::Sl
        when .rshift?       then key = Event::Key::Sr
        when .lalt?         then key = Event::Key::Al
        when .ralt?         then key = Event::Key::Ar
        else
          Log.debug { "unhandled SDL scancode #{scancode}" }
          return
        end
      {% end %}

      ctrl = event.keysym.mod.lctrl? || event.keysym.mod.rctrl?
      shift = event.keysym.mod.lshift? || event.keysym.mod.rshift?
      alt = event.keysym.mod.lalt? || event.keysym.mod.ralt?

      # Disable modifiers on modifier keys themselves since that would make
      # little sense, at least it makes little sense to me.
      if key.cl? || key.cr? || key.sl? || key.sr? || key.al? || key.ar?
        ctrl = shift = alt = false
      end

      up = Event::KeyUp.new(key, ctrl, shift, alt)
      dn = Event::KeyDn.new(key, ctrl, shift, alt)

      case event.type
      when .keyup?
        dispatch(event.window_id, up, sink)
      when .keydown?
        if event.repeat > 0
          # Repeats are mapped to up-dn for consistency.
          dispatch(event.window_id, up, sink)
          dispatch(event.window_id, dn, sink)
        else
          dispatch(event.window_id, dn, sink)
        end
      end
    end

    private def dispatch(event : ::SDL::Event::TextInput, sink)
      rune = String.new(event.text.to_slice, truncate_at_null: true)

      dispatch(event.window_id, Event::KeyInput.new(rune), sink)
    end

    private def dispatch(event : ::SDL::Event::Window, sink)
      # NOTE: resized is only triggered on user resize, programmatic
      # resize doesn't trigger it which is actually what we want here!
      case ::SDL::Window::Event.new(event.event.to_i)
      when .resized? then tr = Event::WindowResized.new(event.data1, event.data2)
      when .close?   then tr = Event::WindowClosed.new
      when .exposed? then tr = Event::WindowExposed.new
      when .enter?   then tr = Event::MouseIn.new
      when .leave?   then tr = Event::MouseOut.new
      else
        return
      end

      dispatch(event.window_id, tr, sink)
    end

    private def dispatch(event, sink)
    end

    # Starts an SDL event loop in an isolated fiber context. Calls *sink*
    # with for each event received from SDL, and for each post received
    # from *posts*.
    #
    # Send `shutdown` to *posts* to terminate the event loop. We treat closure
    # of *posts* as an error.
    def evloop(proof : SetupProof, posts : Channel(Term), &sink : Term ->) : Nil
      # FIXME: this will leak if evloop is called multiple times. Which doesn't
      # happen in practice, but still...
      deq = LibSDL::EventType.register(2)

      postq = Deque(Term).new
      postq_lock = Mutex.new

      # This fiber will wait for posts on the posts channel, and add those to
      # the posts queue postq. It will then possibly wake the mainloop
      # using `SDL_PushEvent`.
      #
      # We use a separate queue instead of directly passing terms through PushEvent
      # because I'm afraid of situations where the term will be lost to GC under strain,
      # if the only remaining pointer is now given to C to go wherever. Instead, we keep
      # terms entirely on the Crystal side, so that the GC can see them; and only
      # use SDL's event queue for DEQ (aka "please dequeue") notifications.
      spawn do
        while post = posts.receive
          postq_lock.synchronize { postq << post }

          event = ::SDL::Event::User.new(code: deq)
          if LibSDL.push_event(event) < 0
            raise ::SDL::Error.new("PushEvent")
          end

          break if post == Term.of(:shutdown)
        end
      end

      loop do
        event = ::SDL::Event.wait

        case event
        when ::SDL::Event::User
          case event.code
          when deq.value
            post = postq_lock.synchronize { postq.shift }
            return if post == Term.of(:shutdown)

            sink.call(post)
          end
        else
          # Any other event.
          dispatch(event, sink)
        end
      end
    end

    # Provides window management on top of `evloop`.
    #
    # - Send `(wm open key_ spec_)` to *posts* to bind a window spec to *key*.
    # - Send `(wm update key_ spec_)` to *posts* to update the spec of *key*. Note
    #   that you must open it first using `(wm open ...)`. Invalid specs cause the window
    #   to hide without ruining state. You can continue to update; once the spec is
    #   valid the window will show again.
    # - Send `(wm close key_)` to *posts* to close the window bound to *key* if it
    #   is open. Window updates to *key* will be ignored until you `(wm open ...)`.
    # - See also: `evloop`.
    #
    # Events are given to *sink* like so: `(event key_ term_)`, where *term* is
    # an event term.
    def wmloop(proof : SetupProof, posts : Channel(Term), context : Context, &sink : Term ->)
      windows = {} of Term => Any
      sysid2key = {} of UInt32 => Term

      begin
        evloop(proof, posts) do |post|
          Term.case(post) do
            matchpi %{(wm open key_ spec_)} do
              next if windows.has_key?(key)

              window0 = None.new
              window1 = self.next(proof, context, window0, spec)
              present(proof, window1)

              windows[key] = window1

              next unless window1.is_a?(Some)

              sysid2key[window1.sys.id] = key

              sink.call(Term.of(:wm, :event, key, {:window, :updated, spec}))
            end

            matchpi %{(wm update key_ spec_)} do
              next unless window0 = windows[key]?

              if window0.is_a?(Some)
                sysid = window0.sys.id
              end

              window1 = self.next(proof, context, window0, spec)
              present(proof, window1)

              case window1
              in None
                sysid2key.delete(sysid) if sysid
              in Some
                windows[key] = window1

                assert sysid.in?(nil, window1.sys.id)
                if sysid.nil?
                  sysid2key[window1.sys.id] = key
                end
              end

              sink.call(Term.of(:wm, :event, key, {:window, :updated, spec}))
            end

            # Closure request on our side.
            matchpi %{(wm close key_)} do
              next unless window = windows.delete(key)
              next unless window.is_a?(Some)

              sysid2key.delete(window.sys.id)

              _ = self.next(proof, context, window, spec: nil)

              sink.call(Term.of(:wm, :event, key, {:window, :closed}))
            end

            # These global events seem to be sent by SDL to give us consistent state
            # even before any windows are created, e.g. if modifier keys are held
            # before the program starts. On the other hand, when opening windows,
            # we appear to receive the proper sequence of events anyway, so we appear
            # to not need to handle these global ones.
            matchpi %{(event 0 _)} do
            end

            # User-side closure request (the user clicked "X")
            matchpi %{(event sysid←(%number u32) term←(window closed))} do
              unless key = sysid2key.delete(sysid.to(UInt32))
                Log.warn { "BUG: nonexistent window with sysid `#{sysid}` was closed" }
                next
              end

              unless window = windows.delete(key)
                Log.warn { "BUG: nonexistent window with sysid `#{sysid}`, key `#{key}` was closed" }
                next
              end

              _ = self.next(proof, context, window, spec: nil)

              sink.call(Term.of(:wm, :event, key, term))
            end

            matchpi %{(event sysid←(%number u32) (window exposed))} do
              unless key = sysid2key[sysid.to(UInt32)]?
                Log.warn { "BUG: nonexistent window with sysid `#{sysid}` was exposed" }
                next
              end

              unless window = windows[key]?
                Log.warn { "BUG: nonexistent window with sysid `#{sysid}`, key `#{key}` was exposed" }
                next
              end

              present(proof, window)
            end

            matchpi %{(event sysid←(%number u32) term_)} do
              unless key = sysid2key[sysid.to(UInt32)]?
                Log.warn { "BUG: nonexistent window with sysid `#{sysid}` received event: #{ML.compact(term)}" }
                next
              end

              sink.call(Term.of(:wm, :event, key, term))
            end

            otherwise do
              sink.call(post)
            end
          end
        end
      ensure
        # Close all windows to free all associated SDL objects.
        windows.each_value do |window|
          _ = self.next(proof, context, window, spec: nil)
        end

        windows.clear
        sysid2key.clear
      end
    end
  end
end
