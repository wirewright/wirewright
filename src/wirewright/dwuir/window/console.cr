module Ww::DwUIR
  # Window implementation to display textual DwUIR in a terminal.
  #
  # NOTE: You must explicitly initialize `Console` first, by wrapping your
  # code in `setup`. You will normally do this somewhere near the top-level,
  # or at the top-level.
  module Window::Console
    extend self

    alias Any = None | Some

    record None
    record Some, screen : Textual::Screen, conf : Conf

    # :nodoc:
    defcase Conf, backdrop : Pigment::RGBA, content : Term

    private def conf?(spec : Term) : Conf?
      # |@ soma.dwuir.window.console
      #
      # |@block
      # Defines the properties of the terminal that will display *content*.
      # |@endblock
      #
      # |@key backdrop soma.dwuir.color -- Sets the background color (clear color)
      # of the terminal.
      Term.matchpi?(spec, %{(window content_* ⍊ backdrop_⋮ black)}) do
        Conf.new(
          backdrop: Pigment.rgba(backdrop, fallback: Pigment.named("black")),
          content: content,
        )
      end
    end

    private def conf?(spec : Nil) : Conf?
      spec
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

    # Performs a window state transition: transitions from an existing state
    # *window* to the next state defined by a DwUIR terminal window *spec*.
    def next(window : Any, spec : Term?) : Any
      unless conf1 = conf?(spec)
        return None.new
      end

      case window
      in None
        screen = pipe(conf1.content, Textual.picture, Textual.screen)

        Some.new(screen, conf1)
      in Some
        conf0 = window.conf

        # Redraw if window content changed.
        unless conf0.content == conf1.content
          screen = pipe(conf1.content, Textual.picture, Textual.screen)
          window = window.copy_with(screen: screen)
        end

        window.copy_with(conf: conf1)
      end
    end

    # :nodoc:
    def present(proof : SetupProof, window : Some) : Nil
      check(proof)

      maxx = Termbox.width
      maxy = Termbox.height

      Termbox.clear(fg: Termbox::Color::White, bg: Termbox::Color.rgb(*window.conf.backdrop.rgb8))

      screen = window.screen

      if (beam = screen.beam?) && beam[0].in?(0...maxx) && beam[1].in?(0...maxy)
        LibTermbox2.tb_set_cursor(*beam)
      else
        LibTermbox2.tb_hide_cursor
      end

      screen.cells.each do |(x, y), cell|
        next unless x.in?(0...maxx) && y.in?(0...maxy)

        if cell.is_a?(Pigment::RGBA)
          Termbox.set(' ', x: x.to_i, y: y.to_i, fg: Termbox::Color::White, bg: Termbox::Color.rgb(*cell.rgb8))
          next
        end

        case cell
        in Textual::Rune
          rune, bg = cell, window.conf.backdrop
        in Tuple(Textual::Rune, Pigment::RGBA)
          rune, bg = cell
        end

        fg = Termbox::Color.rgb(*rune.fg.rgb8)
        bg = Termbox::Color.rgb(*bg.rgb8)

        if rune.decoration.bold?
          fg |= Termbox::Color::Bold
        end

        if rune.decoration.italic?
          fg |= Termbox::Color::Italic
        end

        if rune.decoration.underline?
          fg |= Termbox::Color::Underline
        end

        Termbox.set(rune.chr, x: x.to_i, y: y.to_i, fg: fg, bg: bg)
      end

      Termbox.present
    end

    # :nodoc:
    def present(proof : SetupProof, window : None) : Nil
      check(proof)
    end

    {% if flag?(:docs) %}
      # Syncs the Termbox buffer and content of *window*.
      #
      # NOTE: If *window* is `None`, this method is a noop.
      def present(proof : SetupProof, window : Any) : Nil
      end
    {% end %}

    # Waits for an event from the terminal, and calls *fn* with it.
    def wait(proof : SetupProof, &fn : Term ->) : Nil
      check(proof)
      dispatch(event: Termbox.poll, &fn)
    end

    # Calls *fn* with latest events from the terminal, if any. Does not block
    # if no events were received.
    def poll(proof : SetupProof, &fn : Term ->) : Nil
      check(proof)

      loop do
        break unless event = Termbox.peek?

        dispatch(event, &fn)
      end
    end

    # Welcome to hell!
    #
    # What you will see below is utterly deranged on so many levels!! Makes me
    # appreciate what SDL is doing.

    # Sets up exit and interrupt handlers to shutdown Termbox properly.
    def setup(& : SetupProof ->) : Nil
      proof = SetupProof.new

      shutdown = -> do
        proof.valid = false
        print "\e[2 q" # Set cursor to steady block
        Termbox.shutdown
      end

      Process.on_terminate do |reason|
        shutdown.call
        Process.exit
      end

      Termbox.init
      Termbox.input_mode = Termbox::InputMode::Alt | Termbox::InputMode::Mouse
      Termbox.output_mode = :truecolor

      print "\e[6 q" # Set cursor to vertical bar

      begin
        yield proof, Termbox.width, Termbox.height
      ensure
        shutdown.call
      end
    end

    # Whether the Shift key was pressed in the previous event.
    @@shift = false

    # Which mouse buttons are currently pressed.
    @@buttons = Set(Event::MouseButton).new

    private def synthetic(fn : Term ->) : (Term ->)
      ->(event : Term) do
        fn.call(event.as_d(&.with(:synthetic, true)))
      end
    end

    private def ambiguous(fn : Term ->) : (Term ->)
      ->(event : Term) do
        fn.call(event.as_d(&.with(:ambiguous, true)))
      end
    end

    private def dispatch(event, *, syn : Bool = false, &fn : Term ->)
      if syn
        fn = synthetic(fn)
      end

      Event.term(event, &fn)
    end

    private def dispatch(key : Event::Key, *, ctrl = false, shift = false, alt = false, syn : Bool = false, amb : Bool = false, &fn : Term ->)
      if amb
        fn = ambiguous(fn)
      end

      dispatch(Event::KeyDn.new(key, ctrl, shift, alt), &fn)
      dispatch(Event::KeyUp.new(key, ctrl, shift, alt), syn: true, &fn)
    end

    # TODO: we should be able to parse double, triple etc. clicks here with
    # some effort using Termbox.peek with a short timeout.
    private def dispatch_mouse(event : Termbox::Event, &fn : Term ->)
      case event.key
      when .mouse_left?
        if @@buttons.any?(&.left?)
          dispatch(Event::MouseMotion.new(0u32, x: event.mouse_x, y: event.mouse_y), &fn)
        else
          dispatch(Event::MouseDn.new(0u32, :left, x: event.mouse_x, y: event.mouse_y, n: 1), &fn)
        end

        @@buttons << :left
      when .mouse_middle?
        if @@buttons.any?(&.middle?)
          dispatch(Event::MouseMotion.new(0u32, event.mouse_x, event.mouse_y), &fn)
        else
          dispatch(Event::MouseDn.new(0u32, :middle, x: event.mouse_x, y: event.mouse_y, n: 1), &fn)
        end

        @@buttons << :middle
      when .mouse_right?
        if @@buttons.any?(&.right?)
          dispatch(Event::MouseMotion.new(0u32, x: event.mouse_x, y: event.mouse_y), &fn)
        else
          dispatch(Event::MouseDn.new(0u32, :right, x: event.mouse_x, y: event.mouse_y, n: 1), &fn)
        end

        @@buttons << :right
      when .mouse_release?
        @@buttons.each do |button|
          dispatch(Event::MouseUp.new(0u32, button, x: event.mouse_x, y: event.mouse_y, n: 1), &fn)
        end
        @@buttons.clear
      when .mouse_wheel_up?
        dispatch(Event::MouseWheel.new(0u32, dx: 0, dy: -1), &fn)
      when .mouse_wheel_down?
        dispatch(Event::MouseWheel.new(0u32, dx: 0, dy: +1), &fn)
      end
    end

    private def dispatch_key(event : Termbox::Event, &fn : Term ->)
      typeof(fn) # Crystal issue #15940: https://github.com/crystal-lang/crystal/issues/15940

      {% begin %}
        if @@shift && !event.mod.shift?
          dispatch(Event::KeyUp.new(Event::Key::Sl, ctrl: false, shift: false, alt: false), syn: true, &ambiguous(fn))
          dispatch(Event::KeyUp.new(Event::Key::Sr, ctrl: false, shift: false, alt: false), syn: true, &ambiguous(fn))
          @@shift = false
        elsif !@@shift && event.mod.shift?
          dispatch(Event::KeyDn.new(Event::Key::Sl, ctrl: false, shift: false, alt: false), syn: true, &ambiguous(fn))
          dispatch(Event::KeyDn.new(Event::Key::Sr, ctrl: false, shift: false, alt: false), syn: true, &ambiguous(fn))
          @@shift = true
        end

        chr = event.ch.chr
        if chr.printable?
          case chr
          when .ascii_number?
            case chr
            {% for digit in "0123456789".chars %}
            when {{digit}}
              dispatch(Event::Key::Digit{{digit.id}}, syn: true, &fn)
              dispatch(Event::Key::Np{{digit.id}}, syn: true, &fn)
            {% end %}
            end
          when '['  then dispatch(Event::Key::Lsqb, syn: true, &fn)
          when ']'  then dispatch(Event::Key::Rsqb, syn: true, &fn)
          when ';'  then dispatch(Event::Key::Semicolon, syn: true, &fn)
          when '\'' then dispatch(Event::Key::Quote, syn: true, &fn)
          when ','  then dispatch(Event::Key::Comma, syn: true, &fn)
          when '.'  then dispatch(Event::Key::Period, syn: true, &fn)
          when '/'  then dispatch(Event::Key::Slash, syn: true, &fn)
          when '\\' then dispatch(Event::Key::Backslash, syn: true, &fn)
          when '-'  then dispatch(Event::Key::Minus, syn: true, &fn)
          when '='  then dispatch(Event::Key::Equals, syn: true, &fn)
          when '`'  then dispatch(Event::Key::Backquote, syn: true, &fn)
          end

          dispatch(Event::KeyInput.new(chr.to_s), &fn)
          return
        end

        case event.key
        # Unambiguous Ctrl keys, possibly with an Alt modifier.
        {% for key in "abcdefgjklnopqrstuvwxyz".chars %}
        when .ctrl_{{key.id}}?
          dispatch(Event::Key::{{key.id.upcase}}, ctrl: true, alt: event.mod.alt?, &fn)
        {% end %}
        when .ctrl_6?
          dispatch(Event::Key::Digit6, ctrl: true, alt: event.mod.alt?, &fn)
        # Unambiguous general keys with modifiers.
        when .space?
          dispatch(Event::KeyInput.new(" "), syn: true, &fn)
          dispatch(Event::Key::Space, alt: event.mod.alt?, &fn)
        {% for n in 1..12 %}
        when .f{{n.id}}?
          dispatch(Event::Key::F{{n.id}}, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        {% end %}
        {% for key in %w[insert delete home end] %}
        when .{{key.id}}?
          dispatch(Event::Key::{{key.capitalize.id}}, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        {% end %}
        when .pgup?
          dispatch(Event::Key::PgUp, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        when .pgdn?
          dispatch(Event::Key::PgDn, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        when .arrow_up?
          dispatch(Event::Key::Up, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        when .arrow_down?
          dispatch(Event::Key::Dn, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        when .arrow_left?
          dispatch(Event::Key::Left, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        when .arrow_right?
          dispatch(Event::Key::Right, ctrl: event.mod.ctrl?, shift: event.mod.shift?, alt: event.mod.alt?, &fn)
        when .back_tab?
          dispatch(Event::Key::Tab, ctrl: event.mod.ctrl?, shift: true, alt: event.mod.alt?, &fn)
        # Ambiguous
        when .ctrl_tilde? # .ctrl_2?
          dispatch(Event::Key::Backquote, ctrl: true, shift: true, alt: event.mod.alt?, amb: true, &fn)
          dispatch(Event::Key::Digit2, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
        when .backspace? # .ctrl_h?
          dispatch(Event::Key::Backspace, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
          dispatch(Event::Key::H, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
        when .tab? # .ctrl_i?
          dispatch(Event::Key::Tab, amb: true, &fn)
          dispatch(Event::Key::I, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
        when .enter? # .ctrl_m?
          dispatch(Event::Key::Enter, alt: event.mod.alt?, amb: true, &fn)
          dispatch(Event::Key::M, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
        when .esc? # .ctrl_lsq_bracket?, .ctrl_3?
          dispatch(Event::Key::Escape, amb: true, &fn)
          dispatch(Event::Key::Lsqb, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
          dispatch(Event::Key::Digit3, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
        when .ctrl_4? # .ctrl_backslash?
          dispatch(Event::Key::Backslash, ctrl: true, amb: true, &fn)
          dispatch(Event::Key::Digit4, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
        when .ctrl_5? # .ctrl_rsq_bracket?
          dispatch(Event::Key::Digit5, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
          dispatch(Event::Key::Rsqb, ctrl: true, amb: true, &fn)
        when .ctrl_7? # .ctrl_slash?, .ctrl_underscore?
          dispatch(Event::Key::Digit7, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
          dispatch(Event::Key::Slash, ctrl: true, amb: true, &fn)
          dispatch(Event::Key::Minus, ctrl: true, shift: true, amb: true, &fn)
        when .backspace2? # .ctrl_8?
          dispatch(Event::Key::Backspace, alt: event.mod.alt?, amb: true, &fn)
          dispatch(Event::Key::Digit8, ctrl: true, alt: event.mod.alt?, amb: true, &fn)
        end
      {% end %}
    end

    private def dispatch(event : Termbox::Event, &fn : Term ->)
      case event.type
      when .resize?
        dispatch(Event::WindowResized.new(event.resize_w, event.resize_h), &fn)
      when .mouse?
        dispatch_mouse(event, &fn)
      when .key?
        dispatch_key(event, &fn)
      end
    end
  end
end
