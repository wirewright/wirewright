module Ww::Soma::DwUIR
  # DwUIR window management module.
  #
  # This module defines methods for reuse by specific implementations of
  # window management.
  module Window
    extend self

    # Mouse cursors supported by Soma/DwUIR.
    enum Cursor
      None
      Text
      Wait
      Grab
      Arrow
      Pointer
      Progress
      Crosshair
      ResizeTlBr
      ResizeBlTr
      ResizeX
      ResizeY
      Resize
      NotAllowed

      # Parses a cursor's symbol term representation *term* into the corresponding
      # `Cursor` value. E.g. `pointer` becomes `Pointer`.
      #
      # If no `Cursor` corresponds to *term*, returns the given *fallback* cursor.
      def self.parse(term : Term, fallback : Cursor = :arrow) : Cursor
        # |@ soma.dwuir.cursor
        #
        # |@block
        # The following cursors are available:
        #
        # - `none` hides the cursor.
        # - `text` shows an I-beam for text selection.
        # - `wait` shows a busy indicator (e.g. hourglass or spinning circle).
        # - `grab` shows an open hand for draggable elements.
        # - `arrow` shows the default arrow pointer.
        # - `pointer` shows a hand pointer for clickable elements.
        # - `progress` shows a busy indicator with an arrow.
        # - `crosshair` shows a crosshair for precision selection.
        # - `resize-tl-br` shows a diagonal resize cursor from top-left to bottom-right.
        # - `resize-bl-tr` shows a diagonal resize cursor from bottom-left to top-right.
        # - `resize-x` shows a horizontal resize cursor.
        # - `resize-y` shows a vertical resize cursor.
        # - `resize` shows a four-way resize cursor.
        # - `not-allowed` shows a "no" symbol for unavailable actions.
        # |@endblock

        case term
        when Term.of(:none)           then None
        when Term.of(:text)           then Text
        when Term.of(:wait)           then Wait
        when Term.of(:grab)           then Grab
        when Term.of(:arrow)          then Arrow
        when Term.of(:pointer)        then Pointer
        when Term.of(:progress)       then Progress
        when Term.of(:crosshair)      then Crosshair
        when Term.of(:"resize-tl-br") then ResizeTlBr
        when Term.of(:"resize-bl-tr") then ResizeBlTr
        when Term.of(:"resize-x")     then ResizeX
        when Term.of(:"resize-y")     then ResizeY
        when Term.of(:resize)         then Resize
        when Term.of(:"not-allowed")  then NotAllowed
        else
          fallback
        end
      end

      # Converts this cursor to its symbol term representation. E.g. `Arrow`
      # becomes `arrow`.
      def term : Term
        case term
        in .none?         then Term.of(:none)
        in .text?         then Term.of(:text)
        in .wait?         then Term.of(:wait)
        in .grab?         then Term.of(:grab)
        in .arrow?        then Term.of(:arrow)
        in .pointer?      then Term.of(:pointer)
        in .progress?     then Term.of(:progress)
        in .crosshair?    then Term.of(:crosshair)
        in .resize_tl_br? then Term.of(:"resize-tl-br")
        in .resize_bl_tr? then Term.of(:"resize-bl-tr")
        in .resize_x?     then Term.of(:"resize-x")
        in .resize_y?     then Term.of(:"resize-y")
        in .resize?       then Term.of(:resize)
        in .not_allowed?  then Term.of(:"not-allowed")
        end
      end
    end

    # An internal, intermediate event representation; stands between a particular
    # window management implementation (e.g. SDL) and the term representation
    # of events. You can convert the structures defined here to their term
    # representation using the `term` function, which yields one or more term
    # representations for a given structure.
    module Event
      extend self

      enum MouseButton
        Left
        Middle
        Right
        Forward
        Backward
      end

      enum Key
        # Digits

        {% for n in 0..9 %}
          Digit{{n.id}}
        {% end %}

        # Numpad digits

        {% for n in 0..9 %}
          Np{{n.id}}
        {% end %}

        # Function keys

        {% for n in 1..12 %}
          F{{n.id}}
        {% end %}

        # Letters

        {% for key in "abcdefghijklmnopqrstuvwxyz".chars %}
          {{key.id.upcase}}
        {% end %}

        # Misc

        Lsqb
        Rsqb
        Semicolon
        Quote
        Comma
        Period
        Slash
        Minus
        Equals
        Backslash
        Space
        Backquote
        Up
        Dn
        Left
        Right
        Tab
        Enter
        Esc
        Insert
        Delete
        Backspace
        Home
        End
        PgUp
        PgDn

        # Left control.
        Cl
        # Right control.
        Cr
        # Left shift.
        Sl
        # Right shift.
        Sr
        # Left alt.
        Al
        # Right alt.
        Ar
      end

      record MouseMotion, device : UInt32, x : Int32, y : Int32
      record MouseDn, device : UInt32, button : MouseButton, x : Int32, y : Int32, n : Int32
      record MouseUp, device : UInt32, button : MouseButton, x : Int32, y : Int32, n : Int32
      record MouseWheel, device : UInt32, dx : Int32, dy : Int32
      record WindowResized, w : Int32, h : Int32
      record KeyInput, rune : String
      record KeyUp, key : Key, ctrl : Bool, shift : Bool, alt : Bool
      record KeyDn, key : Key, ctrl : Bool, shift : Bool, alt : Bool

      def term(button : MouseButton, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.mouse.button.name
        #
        # |@block
        # The following mouse buttons are available:
        #
        # - `left`
        # - `middle`
        # - `right`
        # - `forward`
        # - `backward`
        # |@endblock

        term =
          case button
          in .left?     then Term.of(:left)
          in .middle?   then Term.of(:middle)
          in .right?    then Term.of(:right)
          in .forward?  then Term.of(:forward)
          in .backward? then Term.of(:backward)
          end

        fn.call(term)
      end

      def term(key : Key, &fn : Term ->) : Nil
        typeof(fn) # Crystal issue #15940: https://github.com/crystal-lang/crystal/issues/15940

        # |@ soma.dwuir.window.event.keyboard.key
        #
        # |@block
        # Both left and right variants of Ctrl, Shift, and Alt keys (modifier keys)
        # count toward `ctrl`/`shift`/`alt` when used in combination with another key.
        #
        # Modifier keys emit their own up/dn events: `(alt ⸨left,right⸩)` for Alt;
        # similarly, (ctrl _) for Ctrl; and `(shift _)` for Shift. This way, you can
        # either ignore the key's side using `(shift _)`, or match it, e.g. `(shift left)`.
        # |@endblock

        {% begin %}
          case key
          {% for n in 0..9 %}
          in .digit{{n.id}}?
            fn.call(Term.of(:"digit/{{n.id}}"))
            fn.call(Term.of(:digit, {{n}}))
          in .np{{n.id}}?
            fn.call(Term.of(:"np/{{n.id}}"))
            fn.call(Term.of(:digit, {{n}}))
          {% end %}
          {% for n in 1..12 %}
          in .f{{n.id}}?
            fn.call(Term.of(:"f{{n.id}}"))
          {% end %}
          {% for key in "abcdefghijklmnopqrstuvwxyz".chars %}
          in .{{key.id}}?
            fn.call(Term.of({{key.id.symbolize}}))
          {% end %}
          in .lsqb?      then fn.call(Term.of(:lsqb))
          in .rsqb?      then fn.call(Term.of(:rsqb))
          in .semicolon? then fn.call(Term.of(:semicolon))
          in .quote?     then fn.call(Term.of(:quote))
          in .comma?     then fn.call(Term.of(:comma))
          in .period?    then fn.call(Term.of(:period))
          in .slash?     then fn.call(Term.of(:slash))
          in .backslash? then fn.call(Term.of(:backslash))
          in .minus?     then fn.call(Term.of(:minus))
          in .equals?    then fn.call(Term.of(:equals))
          in .backquote? then fn.call(Term.of(:backquote))
          in .up?        then fn.call(Term.of(:up))
          in .dn?        then fn.call(Term.of(:dn))
          in .left?      then fn.call(Term.of(:left))
          in .right?     then fn.call(Term.of(:right))
          in .tab?       then fn.call(Term.of(:tab))
          in .enter?     then fn.call(Term.of(:enter))
          in .esc?       then fn.call(Term.of(:esc))
          in .insert?    then fn.call(Term.of(:insert))
          in .delete?    then fn.call(Term.of(:delete))
          in .space?     then fn.call(Term.of(:space))
          in .backspace? then fn.call(Term.of(:backspace))
          in .home?      then fn.call(Term.of(:home))
          in .end?       then fn.call(Term.of(:end))
          in .pg_up?     then fn.call(Term.of(:pgup))
          in .pg_dn?     then fn.call(Term.of(:pgdn))
          in .cl?        then fn.call(Term.of(:ctrl, :left))
          in .cr?        then fn.call(Term.of(:ctrl, :right))
          in .sl?        then fn.call(Term.of(:shift, :left))
          in .sr?        then fn.call(Term.of(:shift, :right))
          in .al?        then fn.call(Term.of(:alt, :left))
          in .ar?        then fn.call(Term.of(:alt, :right))
          end
        {% end %}
      end

      def term(e : MouseMotion, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.mouse.motion
        #
        # |@block
        # Mouse motion events are of the form `(mouse device_ motion x: _number y: _number)`.
        #
        # - *x* is the X-position of the mouse at the time of the event.
        # - *y* is the Y-position of the mouse at the time of the event.
        # |@endblock
        fn.call(Term.of(:mouse, e.device, :motion, x: e.x, y: e.y))
      end

      def term(e : MouseDn, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.mouse.button.dn
        #
        # |@block
        # Mouse button pressed events are of the form `(mouse device_ btn_ button dn x: _number y: _number n: _number)`.
        #
        # - *device* identifies the mouse device (if there are multiple of them).
        # - *btn* is the mouse button name (see `soma.dwuir.window.event.mouse.button.name`).
        # - *x* is the X-position of the mouse at the time of the event.
        # - *y* is the Y-position of the mouse at the time of the event.
        # - *n* is the number of clicks (`1` for single click, `2` for double click etc.)
        #
        # For example, an event could be: `(mouse 0 left button dn x: 100 y: 100 n: 1)`.
        # |@endblock
        term(e.button) do |button|
          fn.call(Term.of(:mouse, e.device, button, :button, :dn, x: e.x, y: e.y, n: e.n))
        end
      end

      def term(e : MouseUp, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.mouse.button.up
        #
        # |@block
        # Mouse button released events are of the form `(mouse device_ btn_ button up x: _number y: _number n: _number)`.
        #
        # - *device* identifies the mouse device (if there are multiple of them).
        # - *btn* is the mouse button name (see `soma.dwuir.window.event.mouse.button.name`).
        # - *x* is the X-position of the mouse at the time of the event.
        # - *y* is the Y-position of the mouse at the time of the event.
        # - *n* is the number of clicks (`1` for single click, `2` for double click etc.)
        #
        # For example, an event could be: `(mouse 0 left button up x: 100 y: 100 n: 1)`.
        # |@endblock
        term(e.button) do |button|
          fn.call(Term.of(:mouse, e.device, button, :button, :up, x: e.x, y: e.y, n: e.n))
        end
      end

      def term(e : MouseWheel, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.mouse.wheel
        #
        # |@block
        # Mouse wheel events are of the form `(mouse device_ dx: _number dy: _number)`.
        #
        # - *device* identifies the mouse device (if there are multiple of them).
        # - *dx* is the amount of horizontal scroll; negative if scrolling left, positive if scrolling right.
        # - *dy* is the amount of vertical scroll; negative if scrolling up, positive if scrolling down.
        # |@endblock
        fn.call(Term.of(:mouse, e.device, :wheel, dx: e.dx, dy: e.dy))
      end

      def term(e : WindowResized, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.window.resized
        #
        # |@block
        # Window resize events are of the form `(window resized w: _number h: _number)`.
        # |@endblock
        fn.call(Term.of(:window, :resized, w: e.w, h: e.h))
      end

      def term(e : KeyInput, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.keyboard.input
        #
        # |@block
        # Keyboard input events are of the form `(keyboard input _string)`,
        # where *rune* is the input text such as `"a"` or `"ä"` etc. (i.e. possibly
        # long and possibly Unicode).
        # |@endblock
        fn.call(Term.of(:keyboard, :input, e.rune))
      end

      def term(e : KeyUp, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.keyboard.key.up
        #
        # |@block
        # Key release events are of the form `(keyboard key code_ up ctrl: _boolean shift: _boolean alt: _boolean)`.
        #
        # NOTE: *ctrl*, *shift*, and *alt* are going to be defined only if `true`.
        # In other words, they are set pairs.
        #
        # For example, `(keyboard key left up ctrl: true)`
        # |@endblock
        term(e.key) do |key|
          fn.call(Term.of(:keyboard, :key, key, :up, ctrl: e.ctrl || nil, shift: e.shift || nil, alt: e.alt || nil))
        end
      end

      def term(e : KeyDn, &fn : Term ->) : Nil
        # |@ soma.dwuir.window.event.keyboard.key.dn
        #
        # |@block
        # Key press events are of the form `(keyboard key code_ dn ctrl: _boolean shift: _boolean alt: _boolean)`.
        #
        # NOTE: *ctrl*, *shift*, and *alt* are going to be defined only if `true`.
        # In other words, they are set pairs.
        #
        # For example, `(keyboard key home dn)`
        # |@endblock
        term(e.key) do |key|
          fn.call(Term.of(:keyboard, :key, key, :dn, ctrl: e.ctrl || nil, shift: e.shift || nil, alt: e.alt || nil))
        end
      end
    end
  end
end

require "./window/sdl"
require "./window/console"
