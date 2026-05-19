{% skip_file unless flag?(:sdl3) %}

module Ww
  # The MediaService currently only handles window management / display using SDL3.
  # We plan to handle video, audio and other forms of media here as well, using SDL3
  # (among, perhaps, other things); hence the more general name.
  #
  # References:
  # - [SDL](https://libsdl.org/)
  # - [SDL-Crystal-Bindings](https://github.com/Hadeweka/SDL-Crystal-Bindings)
  #
  # Window example:
  #
  # ```
  # # We need to use an isolated context to make sure all spawns go to MT;
  # # Wirewright requires a parallel context -- which isn't yet ExecutionContext.default.
  # ctx = Fiber::ExecutionContext::Isolated.new("App", spawn_context: MT) do
  #   spec = ML.term(<<-WWML)
  #   (window
  #     (align x: 0.5 y: 0.5
  #       (text caption: "Kaixo, mundua!" size: 48 color: white)))
  #   WWML
  #
  #   # You can call it from anywhere at all to update the app's window "remotely".
  #   # Not that there is a direct way to do that.
  #   #
  #   # You can `.wait` on the returned WaitGroup to wait until MediaService
  #   # opens the window and shows it to the user. Unlike `withdraw`, where
  #   # we recommend waiting, with `publish`, it's completely up to you and
  #   # your logic.
  #   MediaService.publish(Term.of(:app), MediaService.window_spec(spec)).wait
  #
  #   # In practice, if you need to do what's below, you should do:
  #   #
  #   #   MediaService.wait_until_all_closed(Set{Term.of(:app)})
  #   #
  #   # ... but let's implement it ourselves to see how it actually works.
  #
  #   # The idea is to sleep while the window we're interested in is open.
  #   MediaService.listen do |notification|
  #     next unless notification.is_a?(MediaService::WindowDescriptionChanged)
  #     next unless notification.session_key == Term.of(:app)
  #
  #     if (window_description = notification.description?) && window_description.state.open?
  #       next
  #     end
  #
  #     # window_description.is_a?(Nil) on programmatic closure which is initiated
  #     # by `MediaService.withdraw`.
  #     #
  #     # window_description.state.open? flips to false when the user clicks
  #     # the "X" button. MediaService closes the window but you still may want to
  #     # handle that like we do here.
  #
  #     break # Window was closed.
  #   end
  #
  #   # NOTE: MediaService is still aware of the window, regardless of whether
  #   # you use the above listen() block or wait_until_all_closed()! We must
  #   # withdraw the window to clear all associated resources.
  #   #
  #   # Ditto about wait. You can proceed immediately without waiting, but more
  #   # often than not you should prefer to wait, especially for withdraw(),
  #   # especially during exit.
  #   MediaService.withdraw(Term.of(:app), MediaService::WindowSpec).wait
  # end
  #
  # ctx.wait
  # ```
  module MediaService
    extend self

    Log = ::Log.for(self)

    # Mouse cursors supported by `MediaService`.
    enum Cursor
      None
      Arrow
      Text
      Blocked
      Crosshair
      Progress
      ResizeTlBr
      ResizeBlTr
      ResizeX
      ResizeY
      Resize
      NotAllowed
      Pointer
      ResizeTl
      ResizeT
      ResizeTr
      ResizeR
      ResizeBr
      ResizeB
      ResizeBl
      ResizeL

      # The inverse of `MediaService.cursor`.
      def to_term : Term
        case self
        in None       then Term.of(:none)
        in Arrow      then Term.of(:arrow)
        in Text       then Term.of(:text)
        in Blocked    then Term.of(:blocked)
        in Crosshair  then Term.of(:crosshair)
        in Progress   then Term.of(:progress)
        in ResizeTlBr then Term.of(:"resize-tl-br")
        in ResizeBlTr then Term.of(:"resize-bl-tr")
        in ResizeX    then Term.of(:"resize-x")
        in ResizeY    then Term.of(:"resize-y")
        in Resize     then Term.of(:resize)
        in NotAllowed then Term.of(:"not-allowed")
        in Pointer    then Term.of(:pointer)
        in ResizeTl   then Term.of(:"resize-tl")
        in ResizeT    then Term.of(:"resize-t")
        in ResizeTr   then Term.of(:"resize-tr")
        in ResizeR    then Term.of(:"resize-r")
        in ResizeBr   then Term.of(:"resize-br")
        in ResizeB    then Term.of(:"resize-b")
        in ResizeBl   then Term.of(:"resize-bl")
        in ResizeL    then Term.of(:"resize-l")
        end
      end
    end

    # Parses a cursor's symbol term representation *term* into the corresponding
    # `Cursor` value. E.g. `pointer` becomes `Pointer`.
    #
    # If no `Cursor` corresponds to *term*, returns the given *fallback* cursor.
    def cursor(term : Term, fallback : Cursor = :arrow) : Cursor
      # |@ window-service.cursor
      #
      # |@block
      # The following cursors are available:
      #
      # - `none` hides the cursor.
      # - `arrow` shows the default arrow pointer.
      # - `text` shows an I-beam for text selection.
      # - `blocked` shows a busy indicator (the app is unresponsive; e.g. hourglass or spinning circle).
      # - `crosshair` shows a crosshair for precision selection.
      # - `progress` shows a busy indicator with an arrow (the app is responsive).
      # - `resize-tl-br` shows a diagonal resize cursor from top-left to bottom-right.
      # - `resize-bl-tr` shows a diagonal resize cursor from bottom-left to top-right.
      # - `resize` shows a four-way resize cursor.
      # - `resize-x` shows a horizontal resize cursor.
      # - `resize-y` shows a vertical resize cursor.
      # - `not-allowed` shows a "no" symbol for unavailable actions.
      # - `pointer` shows a hand pointer for clickable elements, links.
      # - `resize-tl` is resize top-left.
      # - `resize-t` is resize top.
      # - `resize-tr` is resize top-right.
      # - `resize-r` is resize right.
      # - `resize-br` is resize bottom-right.
      # - `resize-b` is resize bottom.
      # - `resize-bl` is resize bottom-left.
      # - `resize-l` is resize left.
      case term
      when Term.of(:none)           then Cursor::None
      when Term.of(:arrow)          then Cursor::Arrow
      when Term.of(:text)           then Cursor::Text
      when Term.of(:blocked)        then Cursor::Blocked
      when Term.of(:crosshair)      then Cursor::Crosshair
      when Term.of(:progress)       then Cursor::Progress
      when Term.of(:"resize-tl-br") then Cursor::ResizeTlBr
      when Term.of(:"resize-bl-tr") then Cursor::ResizeBlTr
      when Term.of(:"resize-x")     then Cursor::ResizeX
      when Term.of(:"resize-y")     then Cursor::ResizeY
      when Term.of(:resize)         then Cursor::Resize
      when Term.of(:"not-allowed")  then Cursor::NotAllowed
      when Term.of(:pointer)        then Cursor::Pointer
      when Term.of(:"resize-tl")    then Cursor::ResizeTl
      when Term.of(:"resize-t")     then Cursor::ResizeT
      when Term.of(:"resize-tr")    then Cursor::ResizeTr
      when Term.of(:"resize-r")     then Cursor::ResizeR
      when Term.of(:"resize-br")    then Cursor::ResizeBr
      when Term.of(:"resize-b")     then Cursor::ResizeB
      when Term.of(:"resize-bl")    then Cursor::ResizeBl
      when Term.of(:"resize-l")     then Cursor::ResizeL
      else
        fallback
      end
    end

    defrecord WindowSpec,
      content : Term,
      title : String,
      width : Magnitude,
      height : Magnitude,
      resizable : Bool,
      cursor : Cursor,
      backdrop : Pigment::RGBA,
      input : Bool

    # Tries to parse *term* as a `WindowSpec`. Returns `nil` if impossible.
    #
    # See `window-service.window`.
    def window_spec?(term : Term) : WindowSpec?
      # |@ window-service.window
      #
      # |@pattern
      # (window content_* ⍊
      #   title⋮ "Untitled"
      #   width⋮ 500
      #   height⋮ 400
      #   resizable⋮ true
      #   cursor⋮ arrow
      #   backdrop_⋮ (oklch 0.2 0 0)
      #   input⋮ false)
      #
      # |@key content scenery
      # A z-stack of Scenery nodes that the window shows.
      #
      # |@key width
      # The width of the window (in pixels).
      #
      # |@key height
      # The height of the window (in pixels).
      #
      # |@key resizable
      # Whether the user should be able to resize the window away from its desired
      # *width* and *height* (including by maximizing it).
      #
      # |@key cursor window-service.cursor
      # The currently active cursor. This is the attribute that is eventually set
      # by something like Microfold when you e.g. do `hover:cursor-pointer` on
      # a button.
      #
      # |@key backdrop pigment
      # The clear color of the window.
      #
      # |@key input
      # Whether this window should accept text input.
      #
      # |@block
      # Tells the window server how an OS window should look and behave like, and
      # also its content.
      #
      # ```wwml
      # (window
      #   (text caption: "Hello World"))
      # ```
      Term.matchpi?(term, <<-WWML) do
      (window content_* ⍊
        title⋮ "Untitled"
        width⋮ 500
        height⋮ 400
        resizable⋮ true
        cursor⋮ arrow
        backdrop_⋮ (oklch 0.2 0 0)
        input⋮ false)
      WWML
        WindowSpec.new(content,
          title: title.to(String),
          width: width.to(Magnitude),
          height: height.to(Magnitude),
          resizable: resizable.to(Bool),
          cursor: cursor(cursor),
          backdrop: Pigment.rgba(backdrop, fallback: Pigment.white),
          input: input.to(Bool),
        )
      end
    end

    # Same as `window_spec?`, but raises `ArgumentError` instead of returning `nil`.
    def window_spec(term : Term) : WindowSpec
      window_spec?(term) || raise ArgumentError.new
    end

    # :nodoc:
    alias Msg = WindowOpened | WindowUpdated | WindowClosed | PathService::Notification | HTTPService::Notification

    # :nodoc:
    defrecord WindowOpened, session_key : Term, spec : WindowSpec, wg : WaitGroup
    # :nodoc:
    defrecord WindowUpdated, session_key : Term, spec : WindowSpec, wg : WaitGroup
    # :nodoc:
    defrecord WindowClosed, session_key : Term, wg : WaitGroup

    # :nodoc:
    alias SpecState = PendingSpec | ReadySpec | WithdrawnSpec

    # :nodoc:
    defrecord PendingSpec, spec : WindowSpec, wg : WaitGroup
    # :nodoc:
    defrecord ReadySpec, spec : WindowSpec
    # :nodoc:
    defrecord WithdrawnSpec, spec : WindowSpec, wg : WaitGroup

    @@lock = Sync::Mutex.new

    @@workspace = {} of Term => SpecState
    @@workspace_dirty = false
    @@workspace_signal = BlockingSignal.new

    @@msgs = BlockingQueue(Msg).new
    @@buffer = BlockingQueue(Msg).new

    # WARNING: Assumes `@@lock` is taken.
    private def ensure_running!
      return if @@running

      @@running = true

      wg = WaitGroup.new(1)

      # FIXME: SDL wants to be on its own thread. In fact, it wants to be on the main thread.
      # However, I'm not sure how to do that in Crystal: if we block the main thread with
      # SDL_WaitEvent, then we can't run fibers.
      Fiber::ExecutionContext::Isolated.new("MediaService event loop", spawn_context: Fiber::ExecutionContext.current) do
        Log.trace { "initialize SDL" }

        SDL.init

        Log.trace { "load system cursors" }

        # Load cursors.
        cursors = {} of Symbol => SDL::Cursor
        {% for cursor in %w[default text wait crosshair progress nwse_resize nesw_resize ew_resize ns_resize move not_allowed pointer nw_resize n_resize ne_resize e_resize se_resize s_resize sw_resize w_resize] %}
          cursors[{{cursor.id.symbolize}}] = SDL.make(SDL::Cursor, SDL::SystemCursor::{{cursor.id.upcase}})
        {% end %}

        Log.trace { "SDL initialization complete" }

        wg.done

        Log.trace { "starting event loop" }

        evloop = Evloop.new(cursors, @@buffer)
        loop do
          event = SDL.wait
          evloop.receive(event)
        end
      end

      # Make sure SDL is initialized before we proceed.
      #
      # Fibers may want to enqueue in the meanwhile. The first caller interested in SDL
      # will force a wait until SDL is initialized, by holding all other fibers with @@lock.
      wg.wait

      spawn(name: "MediaService-SDL relay") do
        loop do
          msg = @@msgs.shift
          Log.trace { "relay #{msg.class}" }
          @@buffer << msg
          SDL.push(SDL::Trigger.new)
        end
      end

      spawn(name: "MediaService workspace reaper") do
        epoch = 0u64

        opened = Set(Term).new
        ready = [] of {Term, ReadySpec}
        closed = [] of Term
        waiting = [] of WaitGroup

        loop do
          epoch = @@workspace_signal.wait(epoch)

          Log.trace { "workspace reap" }

          begin
            @@lock.synchronize do
              @@workspace.each do |key, state|
                case state
                in PendingSpec
                  if opened.add?(key)
                    @@msgs << WindowOpened.new(key, state.spec, state.wg)
                  else
                    @@msgs << WindowUpdated.new(key, state.spec, state.wg)
                  end

                  ready << {key, ReadySpec.new(state.spec)}
                  waiting << state.wg
                in ReadySpec # Handled
                in WithdrawnSpec
                  next unless opened.delete(key)

                  @@msgs << WindowClosed.new(key, state.wg)
                  closed << key
                  waiting << state.wg
                end
              end

              ready.each do |key, spec|
                @@workspace[key] = spec
              end

              closed.each do |key|
                @@workspace.delete(key)
              end

              @@workspace_dirty = false
            end

            # The reaper must ultimately be bound by SDL, otherwise, we'd overwhelm SDL,
            # because Crystal runtime lets us get here 100k+ times per second, which is
            # good for us & of the Crystal runtime, but bad for SDL. Anyway, while we're
            # waiting here, @@workspace will absorb further changes, and set the dirty flag
            # if necessary.
            Log.trace { "reap: waiting for #{waiting.size} WaitGroup(s)" }
            waiting.each &.wait

            Log.trace { "reap: #{ready.size} spec(s) ->ready, #{closed.size} spec(s) withdrawn" }
          ensure
            ready.clear
            closed.clear
            waiting.clear
          end
        end
      end

      spawn(name: "MediaService PathService relay") do
        PathService.listen do |notification|
          @@msgs << notification
        end
      end

      spawn(name: "MediaService HTTPService relay") do
        HTTPService.listen do |notification|
          @@msgs << notification
        end
      end
    end

    defcase Session,
      window : SDL::Window,
      renderer : SDL::Renderer,
      texture : SDL::Texture,
      cache : Scenery::CacheSet,
      spec : WindowSpec,
      width_pref : Int32,
      height_pref : Int32,
      width_real : Int32,
      height_real : Int32,
      backdrop_real : Pigment::RGBA,
      state : WindowState,
      command : Scenery::DrawCommand?,
      scenesrc : Scenery::SceneSource?,
      description : WindowDescription?,
      input : Pf::Set(Term),
      keyboard : Pf::Set(Term),
      mice : Slice(Mouse),
      mutation: true

    @[Flags]
    enum WindowState
      # Set if the window is open (minimized, maximized, or floating).
      Open

      # Set if the window has mouse focus.
      Hover

      # Set if the window has keyboard focus.
      Active

      # Set if the window is not minimized.
      Visible
    end

    struct Mouse
      @[Flags]
      enum State
        Left
        Right
        Middle
        Forward
        Backward
      end

      getter id : UInt32
      getter position : Scenery::HitQuery
      getter state : State
      getter scroll_x : Magnitude
      getter scroll_y : Magnitude

      # :nodoc:
      def initialize(@id, @position, @state, @scroll_x, @scroll_y)
      end

      # :nodoc:
      def self.morph(mice : Slice(Mouse), id : UInt32, &) : Slice(Mouse)
        copied = false

        mouse_index = mice.index { |mouse| mouse.id == id }
        if mouse_index.nil?
          initial = Mouse.new(id,
            position: Scenery::HitQuery.zero,
            state: State::None,
            scroll_x: Magnitude.new(0),
            scroll_y: Magnitude.new(0),
          )
          mouse_index = mice.size
          mice = mice.append(initial)
          copied = true
        end

        unless copied
          mice = mice.dup
        end

        mice[mouse_index] = yield mice[mouse_index]
        mice
      end

      # :nodoc:
      def_copy_with

      def down(delta : State, clicks : Int) : Mouse
        case clicks
        when 0 # ?!
          copy_with(state: @state | delta, position: @position.point)
        when 1
          copy_with(state: @state | delta, position: @position.single)
        when 2
          copy_with(state: @state | delta, position: @position.double)
        else
          copy_with(state: @state | delta, position: @position.triple)
        end
      end

      def up(delta : State) : Mouse
        copy_with(state: @state ^ delta, position: @position.point)
      end

      def move(x : Number, y : Number) : Mouse
        copy_with(position: @position.move(Scenery::Point[x, y]))
      end

      def scroll(dx : Number, dy : Number) : Mouse
        copy_with(scroll_x: @scroll_x + dx, scroll_y: @scroll_y + dy)
      end
    end

    # The difference between *input* and *keyboard* is that *input* may contain
    # `rune`s and other virtual keys, and responds to key repeats. *keyboard* only
    # contains `key`s, and does not respond to key repeats; so while you hold
    # a key, it is present in *keyboard*, and when you release it, it is removed.
    defrecord WindowDescription,
      vantages : Slice(Term),
      mice : Slice(Mouse),
      input : Pf::Set(Term),
      keyboard : Pf::Set(Term),
      width : Int32,
      height : Int32,
      state : WindowState

    private class Evloop
      def initialize(@cursors : Hash(Symbol, SDL::Cursor), @buffer : BlockingQueue(Msg))
        @cursor = Cursor::Arrow
        @sessions = {} of Term => Session
      end

      def receive(msg) : Nil
        Log.trace { msg.class }

        begin
          handle(msg)
        rescue e : Exception
          Log.error(exception: e) { "handle() call crashed" }
        end
      end

      def handle(event : SDL::Trigger) : Nil
        msg = @buffer.shift
        receive(msg)
      end

      private def session(event, &) : Nil
        return unless row = @sessions.find { |_, session| SDL.id(session.window) == event.window_id }

        session_key, session = row
        yield session_key, session
      end

      private def before_tick_handle(event, &) : Nil
        session(event) do |session_key, session|
          yield session_key, session
          tick(session_key, session)
        end
      end

      # When we enter window X, we change the current cursor (global) with X's cursor.
      def handle(event : SDL::WindowMouseFocusGained) : Nil
        before_tick_handle(event) do |_, session|
          session.state |= WindowState::Hover
          set_cursor(session.spec.cursor)
        end
      end

      # When we leave window X, we swap the global cursor with arrow, i.e.,
      # the default, "resting" cursor.
      def handle(event : SDL::WindowMouseFocusLost) : Nil
        before_tick_handle(event) do |_, session|
          session.state ^= WindowState::Hover
          session.mice = Slice(Mouse).empty
          set_cursor(:arrow)
        end
      end

      def handle(event : SDL::WindowExposed) : Nil
        session(event) do |session_key, session|
          tick(session_key, session, force_redraw: true)
        end
      end

      # When we resize a window, we update its size preference and tick() it.
      def handle(event : SDL::WindowResized) : Nil
        before_tick_handle(event) do |_, session|
          width, height = Scenery::PixelRect.clamp(event.width.to_f32, event.height.to_f32)
          session.width_pref = width
          session.height_pref = height
        end
      end

      def handle(event : SDL::WindowClosed) : Nil
        before_tick_handle(event) do |_, session|
          SDL.hide(session.window)
          session.state = WindowState::None
          session.mice = Slice(Mouse).empty
          set_cursor(:arrow)
        end
      end

      def handle(event : SDL::WindowFocusGained) : Nil
        before_tick_handle(event) do |_, session|
          session.state |= WindowState::Active
        end
      end

      def handle(event : SDL::WindowFocusLost) : Nil
        before_tick_handle(event) do |_, session|
          session.state ^= WindowState::Active
        end
      end

      def handle(event : SDL::WindowMinimized) : Nil
        before_tick_handle(event) do |_, session|
          session.state ^= WindowState::Visible
        end
      end

      def handle(event : SDL::WindowRestored) : Nil
        before_tick_handle(event) do |_, session|
          session.state |= WindowState::Visible
        end
      end

      def handle(event : SDL::MouseMoved) : Nil
        before_tick_handle(event) do |_, session|
          session.mice = Mouse.morph(session.mice, event.mouse_id) do |mouse|
            mouse.move(event.x, event.y)
          end
        end
      end

      def handle(event : SDL::MouseButtonEvent) : Nil
        delta = Mouse::State::None
        case event.button
        when .left?   then delta = Mouse::State::Left
        when .middle? then delta = Mouse::State::Middle
        when .right?  then delta = Mouse::State::Right
        when .x1?     then delta = Mouse::State::Backward
        when .x2?     then delta = Mouse::State::Forward
        end

        handle(event, delta)
      end

      def handle(event : SDL::MouseButtonDown, delta : Mouse::State) : Nil
        before_tick_handle(event) do |_, session|
          session.mice = Mouse.morph(session.mice, id: event.mouse_id) do |mouse|
            mouse.down(delta, event.clicks)
          end
        end
      end

      def handle(event : SDL::MouseButtonUp, delta : Mouse::State) : Nil
        before_tick_handle(event) do |_, session|
          session.mice = Mouse.morph(session.mice, event.mouse_id) do |mouse|
            mouse.up(delta)
          end
        end
      end

      def handle(event : SDL::MouseWheelScrolled) : Nil
        before_tick_handle(event) do |_, session|
          session.mice = Mouse.morph(session.mice, event.mouse_id) do |mouse|
            mouse.scroll(event.dx, -event.dy)
          end
        end
      end

      def handle(event : SDL::KeyboardKeyEvent) : Nil
        unless name = key_name?(event.scancode) || key_name?(event.keycode)
          Log.debug { "unhandled SDL scancode #{event.scancode} (#{event.keycode})" }
          return
        end

        entity = Term.of(:key, name)
        handle(event, entity)
      end

      def handle(event : SDL::KeyboardKeyUp, entity : Term) : Nil
        before_tick_handle(event) do |_, session|
          session.input = session.input.delete(entity)
          session.keyboard = session.keyboard.delete(entity)
        end
      end

      def handle(event : SDL::KeyboardKeyDown, entity : Term) : Nil
        session(event) do |session_key, session|
          session.keyboard = session.keyboard.add(entity)

          if event.repeat
            # Release
            session.input = session.input.delete(entity)
            tick(session_key, session)

            # Press
            session.input = session.input.add(entity)
            tick(session_key, session)
          else
            # Press
            session.input = session.input.add(entity)
            tick(session_key, session)
          end
        end
      end

      def handle(event : SDL::TextEntered) : Nil
        session(event) do |session_key, session|
          entity = Term.of(:rune, event.rune)

          # Press
          session.input = session.input.add(entity)
          tick(session_key, session)

          # Release
          session.input = session.input.delete(entity)
          tick(session_key, session)
        end
      end

      def handle(event : SDL::Event) : Nil
        Log.debug { "unhandled SDL event: #{event}" }
      end

      def handle(msg : WindowOpened) : Nil
        Log.debug { "opening window #{msg.session_key}" }

        width, height = Scenery::PixelRect.clamp(msg.spec.width, msg.spec.height)

        window, renderer = SDL.make({SDL::Window, SDL::Renderer}, msg.spec.title, width, height)
        SDL.set(window, resizable: msg.spec.resizable)
        SDL.set(window, input: msg.spec.input)

        texture = SDL.make(SDL::Texture, renderer, width, height,
          format: SDL::PixelFormat::ARGB8888,
          access: SDL::TextureAccess::STREAMING,
        )

        cache = Scenery::Safe.cache

        session = Session.new(window, renderer, texture, cache,
          spec: msg.spec,
          width_pref: width,
          height_pref: height,
          width_real: width,
          height_real: height,
          backdrop_real: msg.spec.backdrop,
          state: WindowState::Open | WindowState::Visible,
          command: nil,
          scenesrc: nil,
          description: nil,
          input: Pf::Set(Term).new,
          keyboard: Pf::Set(Term).new,
          mice: Slice(Mouse).empty,
        )

        assert @sessions.put?(msg.session_key, session)

        tick(msg.session_key, session)
      ensure
        msg.wg.done
      end

      def handle(msg : WindowUpdated) : Nil
        Log.debug { "updating window #{msg.session_key}" }

        session = @sessions[msg.session_key]

        spec0 = session.spec
        spec1 = msg.spec

        session.spec = spec1

        unless session.state.open?
          SDL.show(session.window)
          session.state |= WindowState::Open
        end

        unless spec0.title == spec1.title
          SDL.set(session.window, title: spec1.title)
        end

        unless spec0.resizable == spec1.resizable
          SDL.set(session.window, resizable: spec1.resizable)
        end

        unless spec0.input == spec1.input
          SDL.set(session.window, input: spec1.input)
        end

        unless {spec0.width, spec0.height} == {spec1.width, spec1.height}
          # Size preference is modified only when its spec counterpart changes. If you
          # do not modify the window size explicitly, we won't "snap" it back, on resize,
          # to the size  declared in the spec.
          session.width_pref, session.height_pref = Scenery::PixelRect.clamp(spec1.width, spec1.height)
        end

        unless spec0.cursor == spec1.cursor
          set_cursor(spec1.cursor)
        end

        tick(msg.session_key, session)
      ensure
        msg.wg.done
      end

      def handle(msg : WindowClosed) : Nil
        return unless session = @sessions.delete(msg.session_key)

        Log.debug { "close window #{msg.session_key}" }

        SDL.hide(session.window)
        SDL.destroy(session.texture)
        SDL.destroy(session.window)
        session = nil
        # https://wiki.libsdl.org/SDL3/SDL_DestroyWindow
        #
        # > Note that on some platforms, the visible window may not actually be
        # > removed from the screen until the SDL event loop is pumped again,
        # > even though the SDL_Window is no longer valid after this call.
        SDL.pump

        MediaService.broadcast(msg.session_key, description: nil)
      ensure
        msg.wg.done
      end

      def handle(msg : PathService::Notification | HTTPService::Notification) : Nil
        tick
      end

      def tick(**kwargs) : Nil
        @sessions.each { |session_key, session| tick(session_key, session, **kwargs) }
      end

      def tick(session_key : Term, session : Session, **kwargs) : Nil
        scene = redraw(session, **kwargs)
        redescribe(session_key, session, scene)
      end

      # Returns the scene that was drawn.
      def redraw(session : Session, *, force_redraw : Bool = false) : Scenery::Scene
        width = session.width_real
        height = session.height_real

        # Resize the texture and the window according to the size preference.
        size_changed = {session.width_pref, session.height_pref} != {width, height}
        if size_changed
          width, height = session.width_pref, session.height_pref

          SDL.resize(session.window, width, height)

          SDL.destroy(session.texture)
          session.texture = SDL.make(SDL::Texture, session.renderer, width, height,
            format: SDL::PixelFormat::ARGB8888,
            access: SDL::TextureAccess::STREAMING,
          )

          session.width_real = width
          session.height_real = height
        end

        backdrop_changed = session.backdrop_real != session.spec.backdrop
        if backdrop_changed
          session.backdrop_real = session.spec.backdrop
          # We'll trigger a full rasterize() below, no need to do anything here...
        end

        # Make a scene source or use the latest one.
        scenesrc0 = session.scenesrc
        scenesrc1 = Scenery::Safe.scenesrc(session.cache, session.spec.content, Magnitude.new(width), Magnitude.new(height))
        unless scenesrc0 == scenesrc1
          session.scenesrc = scenesrc1
        end

        # Poll the source for the latest version of the scene. It changes as more
        # images and fonts load and so on.
        scene = Scenery::Safe.poll(session.cache, scenesrc1).unwrap

        invalidated = size_changed || backdrop_changed

        command1 = Scenery::Safe.depict(session.cache, scene)
        if (command0 = session.command) && !invalidated
          dirty_rects = Scenery::Safe.diff(command0, command1)
        else
          # Trigger full rasterize() if the texture was invalidated (which happens at the very
          # beginning, on resize, on backdrop change, etc., see above).
          dirty_rects = Slice[Scenery::Rect[0, 0, width, height]]
        end
        session.command = command1

        if dirty_rects.present?
          # Transfer pixel data to SDL.
          SDL.lock(session.texture) do |pixels, pitch|
            screen = Scenery::PixelRect.new(pixels.as(UInt8*), width, height, pitch)
            Scenery::Safe.rasterize(screen, command1, session.spec.backdrop, dirty_rects)
          end
        end

        if dirty_rects.present? || force_redraw
          SDL.copy(session.texture, session.renderer, width, height)
          SDL.present(session.renderer)
        end

        scene
      end

      def redescribe(session_key : Term, session : Session, scene : Scenery::Scene) : Nil
        queries = session.mice.to_readonly_slice(&.position)
        vantages = Scenery::Safe.describe(session.cache, scene, queries)

        description = WindowDescription.new(vantages,
          session.mice,
          session.input,
          session.keyboard,
          session.width_real,
          session.height_real,
          session.state,
        )

        return if session.description == description

        session.description = description

        MediaService.broadcast(session_key, description)
      end

      def set_cursor(target : Cursor) : Nil
        if @cursor == target
          return target
        end

        Log.debug { "swap cursor #{@cursor} -> #{target}" }

        if !@cursor.none? && target.none?
          # Hide cursor.
          SDL.cursor = false
        elsif @cursor.none? && !target.none?
          # Show cursor.
          SDL.cursor = true
        end

        case target
        in .none?
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

        if cursor = @cursors[key]?
          SDL.cursor = cursor
        end

        @cursor = target
      end

      def key_name?(code : SDL::Scancode) : Term?
        {% begin %}
          case code
          {% for n in 0..9 %}
          when .scancode_{{n}}? then Term.of(:digit, {{n}})
          when .kp_{{n}}?  then Term.of(:np, {{n}})
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
          when .lctrl?        then Term.of(:ctrl, :left)
          when .rctrl?        then Term.of(:ctrl, :right)
          when .lshift?       then Term.of(:shift, :left)
          when .rshift?       then Term.of(:shift, :right)
          when .lalt?         then Term.of(:alt, :left)
          when .ralt?         then Term.of(:alt, :right)
          end
        {% end %}
      end

      # This overload is here primarily to support CapsLock->Escape mapping,
      # which is my muscle memory; I rely on it very much. Notice how we don't
      # handle the CapsLock scancode. So if CapsLock=Escape, scancode is left
      # unhandled, but we pick up the keycode.
      def key_name?(code : SDL::Keycode) : Term?
        case code
        when .escape? then Term.of(:escape)
        end
      end
    end

    # WARNING: Assumes `@@lock` is taken!
    private def publish_spec(key : Term, spec : SpecState) : Nil
      @@workspace[key] = spec
      return if @@workspace_dirty

      @@workspace_dirty = true
      @@workspace_signal.call
    end

    # Creates or updates an association between *key* and a window *spec*. Returns
    # a wait group so that callers can wait for completion (but this is not necessary).
    #
    # This function is poll-friendly: you can call it massively, and on modern hardware,
    # it is expected to run in 100k+ calls per second or more.
    def publish(session_key : Term, spec : WindowSpec) : WaitGroup
      @@lock.synchronize do
        ensure_running!

        state = @@workspace[session_key]?

        case state
        in Nil
          wg = WaitGroup.new(1)
          publish_spec(session_key, PendingSpec.new(spec, wg))
          wg
        in PendingSpec
          if state.spec == spec
            return state.wg
          end

          # Notify PendingSpec clients of completion. We're overwriting their spec
          # but pretend it was handled.
          state.wg.done

          wg = WaitGroup.new(1)
          publish_spec(session_key, PendingSpec.new(spec, wg))
          wg
        in ReadySpec
          wg = WaitGroup.new(1)

          if state.spec == spec
            wg.done
            return wg
          end

          publish_spec(session_key, PendingSpec.new(spec, wg))

          wg
        in WithdrawnSpec
          # Notify clients who requested withdraw() that their withdraw is "finished".
          state.wg.done

          wg = WaitGroup.new(1)

          # Demote back into ReadySpec.
          if state.spec == spec
            publish_spec(session_key, ReadySpec.new(spec))
            wg.done
            return wg
          end

          publish_spec(session_key, PendingSpec.new(spec, wg))

          wg
        end
      end
    end

    # Withdraws the window spec associated with *session_key*. Returns a wait
    # group so that callers can wait for completion (but this is not necessary).
    #
    # This function is poll-friendly: you can call it massively, and on modern hardware,
    # it is expected to run in 100k+ calls per second or more.
    def withdraw(session_key : Term, cls : WindowSpec.class) : WaitGroup
      @@lock.synchronize do
        state = @@workspace[session_key]?

        case state
        in Nil
          wg = WaitGroup.new(1)
          wg.done
          return wg
        in PendingSpec
          # Notify PendingSpec clients of completion. We're withdrawing their spec
          # but pretend it was handled.
          state.wg.done

          wg = WaitGroup.new(1)
          publish_spec(session_key, WithdrawnSpec.new(state.spec, wg))
          wg
        in ReadySpec
          wg = WaitGroup.new(1)
          publish_spec(session_key, WithdrawnSpec.new(state.spec, wg))
          wg
        in WithdrawnSpec
          state.wg
        end
      end
    end

    alias Notification = WindowDescriptionChanged

    # Signals that the `WindowDescription` of *session_key* changed to *description*.
    defrecord WindowDescriptionChanged,
      session_key : Term,
      description : WindowDescription?,
      smart: true

    # :nodoc:
    def broadcast(session_key : Term, description : WindowDescription?)
      broadcast(WindowDescriptionChanged.new(session_key, description))
    end

    include ServiceBroadcast(Notification)

    # Blocks the calling fiber until *all* windows referred to by *session keys*
    # are closed.
    #
    # *args* are forwarded to `listen`.
    def wait_until_all_closed(session_keys : Set(Term), *args) : Nil
      open = session_keys.dup

      listen(*args) do |notification|
        next unless notification.is_a?(MediaService::WindowDescriptionChanged)
        next unless notification.session_key.in?(session_keys)

        if (window_description = notification.description?) && window_description.state.open?
          open << notification.session_key
          next
        end

        open.delete(notification.session_key)
        break if open.empty?
      end
    end
  end
end
