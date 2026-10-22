module Ww::MxRack
  # Returns the standard MxRack classifier.
  def clf : D7::Classifier
    ->(node : Term) do
      Term.case(node) do
        # See mxrack/window_spec.cr
        matchpi %{[window _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        # |@ mxrack.box
        #
        # |@pattern
        # [box children_*]
        #
        # |@key children mxrack
        # Zero or more child nodes.
        #
        # |@summary
        # A group node with `flow-none`.
        #
        # |@block
        # The same as `rack.group`, but uses `flow-none` instead of `flow-col`.
        #
        # |@example
        # ```wwml
        # (box
        #   (p "Hello World")
        #   (p "Bye World" style: "dt-100px"))
        # ```
        matchpi %{[box _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        # |@ mxrack.section
        #
        # |@pattern
        # [section title_string children_*]
        #
        # |@key title
        # The name of the section. In MuSoma, it will be displayed in the circuit pane.
        #
        # |@key children mxrack
        # Zero or more child nodes.
        #
        # |@summary
        # A group with a name. It can be folded or unfolded in MuSoma.
        #
        # |@example
        # ```wwml
        # (section "Example"
        #   (cell @x 0)
        #   (cell @y)
        #   (feed @x @y))
        # ```
        matchpi %{[section _string _*]} do
          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        # |@ mxrack.visual
        #
        # |@pattern
        # [visual template_*]
        #
        # |@key template alloy
        # The template to expand.
        #
        # |@summary
        # A visual Alloy template which can refer to cells.
        #
        # |@block
        # Lets you embed an Alloy template that renders visually, and allows you
        # to refer to cells at edges in Nitrene expressions.
        #
        # |@example
        # ```wwml
        # (cell @people ("Jane" "John" "Samantha"))
        #
        # (window style: "flow-col gap-3"
        #   (visual
        #     ;; This is an Alloy template that is rendered purely visually. You can refer to
        #     ;; values in cells using `@` like we do below with `@people`, which refers to
        #     ;; the current value in the cell.
        #     (^each (@people as person_)
        #       (box style: "flow-row item-center-y gap-1 text-ui-content border-l-md \
        #                    border-ui-muted px-1"
        #         (icon "person")
        #         (p ^person style: "text-lg/6")))))
        # ```
        matchpi %{[visual _*]} do
          D7.gnd(node)
        end

        # |@ mxrack.p
        #
        # |@summary
        # A visual text node with a couple of shorthands.
        #
        # |@block
        # `p` had historically meant *paragraph* until the meaning changed, but
        # the name didn't. So, perhaps more appropriately, it could be seen as
        # an abbreviation for *print*.

        # |@ mxrack.p
        #
        # |@pattern
        # [p @edge_]
        #
        # |@key edge rack.[edge]
        # Refers to the cell whose value term should be displayed by the text.
        # The value is converted to a string if it is not a string already.
        #
        # |@block
        # A shorthand for constructing a visual text node that should display
        # the value of a cell directly.
        #
        # |@example
        # ```wwml
        # (window style: "center"
        #   (cell @count 123)
        #   (p @count style: "text-ui-content text-3xl/8 font-light"))
        # ```

        # |@ mxrack.p
        #
        # |@pattern
        # [p (^ expr_)]
        #
        # |@key expr nitrene
        # A Nitrene expression whose value term should be displayed by the text.
        # The value is converted to a string if it is not a string already.
        #
        # |@block
        # A shorthand for constructing a visual text node that should display the result
        # of a Nitrene expression. You can refer to edges in the expression using `@`,
        # just like in the `mxrack.visual` node.
        #
        # |@example
        # ```wwml
        # (window style: "center"
        #   (cell @count 123)
        #   (p ^(+ @count 1) style: "text-ui-content text-3xl/8 font-light"))
        # ```

        matchpi %{[p @_]}, %{[p ^_]} do
          D7.gnd(node)
        end

        # |@ mxrack.mouse
        #
        # |@pattern
        # (mouse filter←* ⍊ position_⋮ false)
        # (mouse filter←(%number u64) ⍊ position_⋮ false)
        #
        # |@key filter
        # The id of the mouse to track. If `*`, tracks all mice. (The latter is
        # usually what you want because there's almost always just one mouse.)
        #
        # |@key position
        # Determines whether, and how, the mouse should track its position.
        # The possible values are:
        # - `false`: position should not be tracked.
        # - `always`: position should be tracked at all times.
        # - `on-press`: position should be tracked only while any mouse button is down.
        #
        # The position is exposed on the mouse node as `l: _number` and `t: _number`,
        # both expressed in window coordinates (`l: 0 t: 0` means top-left corner of
        # the window). The position can be negative or greater than the window's size.
        #
        # |@summary
        # Represents a physical mouse.
        #
        # |@block
        # Represents and tracks a physical mouse device globally -- unlike e.g.
        # `mxrack.mousearea`, which only tracks mice that are over it.
        #
        # Note that `mxrack.mouse`, `mxrack.keyboard`, `mxrack.input` and similar are
        # always associated with a `window`, and so must be placed inside that window.
        #
        # |@example
        # ```wwml
        # (window
        #   ;; Try changing to position: on-press and observe what happens.
        #   (frag @mouse (mouse * position: always))
        #
        #   (limit max-w: 50 max-h: 50
        #     (frag @circle
        #       (transform dl: 0 dt: 0 origin-l: 0.5 origin-t: 0.5
        #         (rect style: "fill-green-500 opacity-70% radius-full"))))
        #
        #   (backsys
        #     {¦ mouse: {¦ ±l ±t} circle: {¦ ±dl ±dt}}
        #       <> {dl: ^l, dt: ^t}))
        # ```
        matchpi %{[mouse *]}, %{[mouse (%number u64)]} do
          D7.gnd(node, merge_policy: D7::MergeDiff.new(1u32))
        end

        # |@ mxrack.input
        #
        # |@pattern
        # (input keys_* ⍊ focus_⋮ false)
        #
        # |@key keys
        # Zero or more keys that are pressed now, or pressed as part of a key repeat.
        #
        # |@key focus mxrack.[input focus]
        # The input's focus state. You can normally use Tab and Shift-Tab to
        # activate `focus: false` inputs that, correspondingly, follow or precede
        # `focus: true` ones.
        #
        # |@summary
        # High-level input device.
        #
        # |@block
        # `input` is for handling high-level input, primarily text input.
        #
        # - For text input it receives *runes* (e.g. `(rune "H")`, `(rune "П")`, or `(rune "見")`).
        # - It receives string keys (e.g. `(key "h")`).
        # - It receives symbol keys (e.g. `(key backspace)`).
        # - It does *not* receive standalone modifier keys (e.g. `(key (ctrl left))`);
        # - Instead, it bundles modifiers with `key`s: `(key backspace ctrl: true)`.
        # - It supports key repeats (when you hold a key for some time and it starts to
        #   repeat released-pressed very quickly).
        #
        # A useful analogy for `input` is an on-screen, virtual keyboard; whereas for
        # `mxrack.keyboard` it is the physical keyboard device. An on-screen keyboard can
        # contain an arbitrary amount of *runes* (e.g. the entire Unicode character set);
        # it is also subject to keyboard layout changes.
        #
        # Note that `mxrack.mouse`, `mxrack.keyboard`, `mxrack.input` and similar are
        # always associated with a `window`, and so must be placed inside that window.
        #
        # IMPORTANT: `input` nodes will only work if the window is marked as accepting
        # input (`input: true`; see `mxrack.window` for more info). Normally you'd enable
        # `input` through Microfold, by writing `input` in the style of whatever visual
        # component you have that the input belongs to/drives; or alternatively by writing
        # `input` in the style of the window itself, rather than manually setting `input: true`.
        #
        # To understand why `input: true` is needed, imagine an on-screen keyboard.
        # By default, it is hidden, so `mxrack.input` nodes cannot receive anything.
        # When you set `input: true` on a window, the window asks the OS to open
        # an on-screen keyboard; subsequently inputs will be able to receive keys.
        #
        # |@example
        # ```wwml
        # (window style: "input"
        #   (frag @input (input focus: always))
        #
        #   ;; Track how the input node evolves.
        #   (frag (@records (log _ records_*))
        #     (log (@input)))
        #
        #   ;; Render that visually.
        #   (group style: "flow-col text-ui-content"
        #     (p "Try pressing keys" style: "text-3xl font-light")
        #     (visual
        #       (^each (@records as record_)
        #         (p ^\record style: "text-xs/4 font-mono")))))
        # ```
        matchpi %{[input _*]} do
          D7.gnd(node, merge_policy: D7::MergeDiff.new(1u32))
        end

        # |@ mxrack.keyboard
        #
        # |@pattern
        # (keyboard keys_* ⍊ focus_⋮ false)
        #
        # |@key keys
        # Zero or more currently *held* keys. Note that if a keyboard node appears in
        # a circuit, it *will not* pick up keys that were held at that time (by design).
        # Metaphorically, a `keyboard` must "hear" a key being pressed to present it
        # in *keys*; and before it exists, it cannot hear anything. I don't know whether
        # this is the behavior we want, but currently it doesn't seem to cause any problems.
        #
        # |@key focus mxrack.[input focus]
        # The keyboard's focus state. You can normally use Tab and Shift-Tab to
        # activate `focus: false` inputs that, correspondingly, follow or precede
        # `focus: true` ones.
        #
        # |@summary
        # Represents the physical keyboard.
        #
        # |@block
        # `keyboard` is for handling low-level, keyboard layout-independent input.
        # Notably, unlike `mxrack.input`, `keyboard`s treat modifier keys as separate
        # keys (e.g. `(key (shift left))`).
        #
        # Note that `mxrack.mouse`, `mxrack.keyboard`, `mxrack.input` and similar are
        # always associated with a `window`, and so must be placed inside that window.
        #
        # |@example
        # ```wwml
        # (window
        #   (frag @kb (keyboard focus: always))
        #
        #   ;; Track how the keyboard node evolves.
        #   (frag (@records (log _ records_*))
        #     (log (@kb)))
        #
        #   ;; Render that visually.
        #   (group style: "flow-col text-ui-content"
        #     (p "Try pressing and holding keys" style: "text-3xl font-light")
        #     (visual
        #       (^each (@records as record_)
        #         (p ^\record style: "text-xs/4 font-mono")))))
        # ```
        matchpi %{[keyboard _*]} do
          D7.gnd(node, merge_policy: D7::MergeDiff.new(1u32))
        end

        # |@ mxrack.mousearea
        #
        # |@pattern
        # (mousearea mice_* ⍊ position_⋮ false)
        #
        # |@key mice
        # Zero or more mice being tracked by the mousearea. Each mouse looks (and works)
        # similar to `mxrack.mouse`: `[mouse id←(%number u64)]`. The *id* is system defined
        # and differentiates mice. See `mxrack.mouse` for info on mouse presses and
        # handling multiple (double-, triple-, etc.) clicks.
        #
        # |@key position
        # Determines whether, and how, the mousearea should track positions of the mice
        # it administers. The possible values are:
        # - `false`: mouse positions should not be tracked. The mousearea will also *not*
        #   capture dragged mice. (That is, if you press a button while the mouse is inside
        #   the mousearea, then move it out, the mouse will just leave the mousearea.)
        # - `always`: position should be tracked at all times.
        # - `on-press`: a mouse's position should be tracked only while one of its
        #   buttons is down.
        #
        # With `position: always` and `position: on-press`, the mousearea will "capture"
        # (will keep showing) mice that are dragged out of it. That is, if you press
        # a button while the mouse is inside the mousearea, then move the mouse out,
        # the mouse will still be in the mousearea, and it will not enter any other mousearea.
        # The logic behind this is that you almost always want them together: tracking
        # position and this drag capturing behavior. If that doesn't work for you, you
        # can use `mxrack.mouse` together with `?l`, `?t`, `?w`, `?h` visual questions
        # `mxrack.[visual question]`.
        #
        # |@summary
        # A node that "harbors" mice while they are hovering over its layout-imposed bounds.
        #
        # |@block
        # A mousearea is a node that "harbors" mice while they are hovering over its bounds.
        # It gives you a representation of each such mouse, its position within the bounds
        # of the mousearea, its button state, and so on.
        #
        # |@example
        # ```wwml
        # (window style: "flow-col p-5"
        #   (p "Try hovering over the box, pressing buttons, etc." style: "text-ui-content")
        #   (box style: "fr-1 content-clip border-sm border-ui-muted rounded"
        #     (frag @mousearea
        #       ;; Try removing position: always, setting a different position: _
        #       (mousearea position: always))
        #     (frag (@records (log _ records_*))
        #       (log (@mousearea)))
        #     (group style: "flow-col text-xs/4 font-mono text-ui-content"
        #       (visual
        #         (^each (@records as record_)
        #           (p ^\record))))))
        # ```
        matchpi %{[mousearea _*]} do
          D7.gnd(node, merge_policy: D7::MergeDiff.new(1u32))
        end

        # |@ mxrack.site
        #
        # |@pattern
        # [site]
        matchpi %{[site]} do
          D7.gnd(node)
        end

        # |@ mxrack.ensemble
        #
        # |@pattern
        # [ensemble children_*]
        #
        # |@key children mxrack
        matchpi %{[ensemble _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        matchpi %{[head_ _*]} do
          continue unless Scenery::KnowledgeBase.parent_head?(head)

          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        otherwise do
          Rack.classify!(node)
        end
      end
    end
  end
end
