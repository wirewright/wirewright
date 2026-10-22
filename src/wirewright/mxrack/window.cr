module Ww::MxRack
  # |@ mxrack.window.cursor
  #
  # |@summary
  # The available window cursors.
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
  enum MouseCursor
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
  end

  # :nodoc:
  CURSOR_MAP = {
    Term.of(:none)           => MouseCursor::None,
    Term.of(:arrow)          => MouseCursor::Arrow,
    Term.of(:text)           => MouseCursor::Text,
    Term.of(:blocked)        => MouseCursor::Blocked,
    Term.of(:crosshair)      => MouseCursor::Crosshair,
    Term.of(:progress)       => MouseCursor::Progress,
    Term.of(:"resize-tl-br") => MouseCursor::ResizeTlBr,
    Term.of(:"resize-bl-tr") => MouseCursor::ResizeBlTr,
    Term.of(:"resize-x")     => MouseCursor::ResizeX,
    Term.of(:"resize-y")     => MouseCursor::ResizeY,
    Term.of(:resize)         => MouseCursor::Resize,
    Term.of(:"not-allowed")  => MouseCursor::NotAllowed,
    Term.of(:pointer)        => MouseCursor::Pointer,
    Term.of(:"resize-tl")    => MouseCursor::ResizeTl,
    Term.of(:"resize-t")     => MouseCursor::ResizeT,
    Term.of(:"resize-tr")    => MouseCursor::ResizeTr,
    Term.of(:"resize-r")     => MouseCursor::ResizeR,
    Term.of(:"resize-br")    => MouseCursor::ResizeBr,
    Term.of(:"resize-b")     => MouseCursor::ResizeB,
    Term.of(:"resize-bl")    => MouseCursor::ResizeBl,
    Term.of(:"resize-l")     => MouseCursor::ResizeL,
  }

  # Recognizes a window cursor *term* if possible.
  def window_cursor?(term : Term) : MouseCursor?
    CURSOR_MAP[term]?
  end

  enum WindowForm
    Closed
    Floating
    Minimized
    Maximized
  end

  # Recognizes a window form *term* if possible.
  def window_form?(term : Term) : WindowForm?
    # |@ mxrack.window.form
    #
    # |@summary
    # Whether a window is open, minimized, maximized, etc.

    case term
    when Term.of(:floating)
      # |@ mxrack.window.form
      #
      # |@pattern
      # floating
      #
      # |@summary
      # The window is (should be) floating. This is basically the normal, open state.
      WindowForm::Floating
    when Term.of(:minimized)
      # |@ mxrack.window.form
      #
      # |@pattern
      # minimized
      #
      # |@summary
      # The window is (should be) minimized.
      WindowForm::Minimized
    when Term.of(:maximized)
      # |@ mxrack.window.form
      #
      # |@pattern
      # maximized
      #
      # |@summary
      # The window is (should be) maximized.
      WindowForm::Maximized
    when Term.of(:closed)
      # |@ mxrack.window.form
      #
      # |@pattern
      # closed
      #
      # |@summary
      # The window is (should be) closed. You can reopen it by setting
      # e.g. `form: floating`.
      WindowForm::Closed
    end
  end

  @[::Flags]
  enum WindowFlags
    Input
    Resizable
    WantsFocus
    WantsHover
    WantsSize
    WantsForm
    ControlsResizable
    ControlsSize
    ControlsForm
  end

  defrecord WindowSpec,
    title : String,
    width : Magnitude,
    height : Magnitude,
    cursor : MouseCursor,
    backdrop : Pigment::RGBA,
    form : WindowForm,
    flags : WindowFlags,
    copying: true

  # Recognizes a window spec *term*. Returns `nil` if *term* is not a window
  # spec term.
  def window_spec?(term : Term) : WindowSpec?
    # |@ mxrack.window
    #
    # |@pattern
    # (window children_* ⍊
    #   title⋮ "Untitled"
    #   width⋮ 500
    #   height⋮ 400
    #   cursor_⋮ arrow
    #   backdrop_⋮ (oklch 0.2 0 0)
    #   input⋮ false
    #   resizable⋮ true
    #   form⋮ floating)
    #
    # |@key title
    # The *title* of the window. You can change this value to change the title
    # of the window dynamically.
    #
    # |@key width
    # The *width* of the window, in pixels. You can change this value to change
    # the width of the window dynamically. This value will change as a result of
    # window resizing.
    #
    # |@key height
    # The *height* of the window, in pixels. You can change this value to change
    # the height of the window dynamically. This value will change as a result of
    # window resizing.
    #
    # |@key cursor mxrack.window.cursor
    # The mouse cursor to use while the mouse is over the window. In practical code
    # you set this via Microfold. For example, `(window ... style: "cursor-pointer")`
    # eventually becomes `(window ... cursor: pointer)`. Or when your button does,
    # say, `hover:cursor-pointer`, what happens is Microfold "bubbles" `cursor-pointer`
    # up to the nearest enclosing window, which then too results in `cursor: pointer`
    # set on it.
    #
    # |@key backdrop pigment
    # The background color of the window itself (its clear color). In Microfold `style: _`s,
    # you should prefer `backdrop` to `bg`. The difference is pretty much technical
    # rather than visual; it suffices to say that `bg` creates a z-stack with
    # a rectangle of that color and your content, whereas `backdrop` literally
    # assigns the window that color.
    #
    # |@key input
    # Whether this window should start accepting text input (`mxrack.input`). This
    # can be changed dynamically. Usually you do this through Microfold: `style: "input"`,
    # or perhaps `style: "focus:input"`. Like *cursor*, the `input` utility "bubbles up"
    # to the nearest enclosing window and sets `input: true` on it.
    #
    # |@key resizable
    # Whether the user should be able to resize the window away from its *width*
    # and *height* (including by maximizing it). This can be changed dynamically.
    #
    # |@key form mxrack.window.form
    # Whether the window is closed, floating, maximized, etc. This can be changed
    # dynamically. For example, the program can reopen a closed window (`form: closed`
    # to `form: floating`) or maximize a floating one (`form: floating` -> `form: maximized`).
    # User actions are reflected by this value as well. The user maximizing the window
    # will be reflected in *form* changing to `maximized`.
    #
    # |@key children mxrack
    # Child nodes. Can contain nested windows (think a dialog box or something along
    # those lines). The window will display *visual nodes* (see `mxrack.[visual]`),
    # except for nested windows and their nodes, which will be displayed in separate
    # windows if needed.
    #
    # |@summary
    # An operating system window.
    #
    # |@block
    # Maintains an operating system window displaying the visual projection
    # of *children*.
    #
    # |@example
    # ```wwml
    # (window title: "My App" style: "center"
    #   (p "Kaixo mundua"))
    # ```
    Term.matchpi?(term, <<-WWML) do
    (window _* ⍊
      title⋮ "Untitled"
      width⋮ 500
      height⋮ 400
      cursor: (%optional arrow cursorQ_)
      backdrop_⋮ (oklch 0.2 0 0)
      input⋮ false
      resizable⋮ true
      form: (%optional floating formQ_))
    WWML
      continue unless cursor = window_cursor?(cursorQ)
      continue unless form = window_form?(formQ)

      flags = WindowFlags::None

      if input.true?
        flags |= WindowFlags::Input
      end
      if resizable.true?
        flags |= WindowFlags::Resizable
      end

      if term.includes?(:hover)
        flags |= WindowFlags::WantsHover
      end
      if term.includes?(:focus)
        flags |= WindowFlags::WantsFocus
      end
      if term.includes?(:width) && term.includes?(:height)
        flags |= WindowFlags::WantsSize
        flags |= WindowFlags::ControlsSize
      end
      if term.includes?(:resizable)
        flags |= WindowFlags::ControlsResizable
      end
      if term.includes?(:form)
        flags |= WindowFlags::WantsForm
        flags |= WindowFlags::ControlsForm
      end

      WindowSpec.new(
        title: title.to(String),
        width: width.to(Magnitude),
        height: height.to(Magnitude),
        cursor: cursor,
        backdrop: Pigment.rgba(backdrop, fallback: Pigment.white),
        form: form,
        flags: flags,
      )
    end
  end

  # Represents a window projection of a circuit or its part. Use `projections` to
  # obtain from a circuit.
  defcase Window,
    key : Term,
    addr : D7::NodeAddr,
    scenery : Term,
    inputs : Slice(Input),
    mice : Slice(Mouse),
    mouseareas : Slice(MouseArea),
    questions : Array(Question),
    spec : WindowSpec

  defrecord WindowNode,
    addr : D7::NodeAddr,
    tree : D7::GroupNode

  # :nodoc:
  SYM_WINDOW = Term.of(:window)

  # Recognizes `mxrack.window` nodes in *tree* and returns them.
  def window_nodes(tree : D7::ParseTree) : Slice(WindowNode)
    case tree
    in D7::InertLeaf, D7::GndLeaf
      Slice(WindowNode).empty
    in D7::MixtureNode, D7::ScopeNode
      window_nodes(tree.child)
    in D7::CircuitNode
      window_nodes(tree.to_group)
    in D7::GroupNode
      unless tree.summary.has_head?(SYM_WINDOW)
        return Slice(WindowNode).empty
      end

      results = Pf::Kit.stack_array(WindowNode, 4)

      # We descend further into the window because it could contain nested
      # windows (think dialog boxes of some sort).
      tree.children.zip(tree.feature.range) do |child, key|
        addr = D7::NodeAddr[key]

        window_nodes(child).each do |window_node|
          results << WindowNode.new(D7::NodeAddr.concat(addr, window_node.addr), window_node.tree)
        end
      end

      pass do
        # [window _*]
        node = tree.feature.node
        next unless head = node.items.first?
        next unless head == SYM_WINDOW

        results << WindowNode.new(D7::NodeAddr.empty, tree)
      end

      results.to_unsafe_readonly_slice!
    end
  end

  module WindowInstr
    alias Any = Show | Hide | Minimize | Maximize | Restore | SetTitle | SetResizable |
                SetBackdrop | SetSize | SetCursor | BeginInput | EndInput | Draw |
                Create | Delete

    defrecord Show, window_id : UInt64
    defrecord Hide, window_id : UInt64
    defrecord Minimize, window_id : UInt64
    defrecord Maximize, window_id : UInt64
    defrecord Restore, window_id : UInt64
    defrecord SetTitle, window_id : UInt64, title : String
    defrecord SetResizable, window_id : UInt64, resizable : Bool
    defrecord SetBackdrop, window_id : UInt64, backdrop : Pigment::RGBA
    defrecord SetSize, window_id : UInt64, width : UInt32, height : UInt32
    defrecord SetCursor, window_id : UInt64, cursor : MouseCursor
    defrecord BeginInput, window_id : UInt64
    defrecord EndInput, window_id : UInt64
    defrecord Draw, window_id : UInt64, command : Scenery::DrawCommand, dirty_rects : Slice(Scenery::Rect)
    defrecord Create, window_id : UInt64, title : String, width : UInt32, height : UInt32, backdrop : Pigment::RGBA
    defrecord Delete, window_id : UInt64
  end

  # Describes the state after a window transition (*spec*, *scenesrc*, *scene*,
  # *command*), and the *program* needed to carry it out.
  defrecord WindowPlan,
    program : WindowProgram,
    spec : WindowSpec,
    scenesrc : Scenery::SceneSource,
    scene : Scenery::Scene,
    command : Scenery::DrawCommand

  # Instead of manipulating windows directly, MxRack constructs *window programs*.
  #
  # A window program is a kind of *media program*. Media programs are sent to a *media
  # program queue*. On the other end of such a queue is, most likely, SDL -- more precisely,
  # the `SdlLoop`. The `SdlLoop` is tasked with executing media programs, including
  # window programs. It does so by talking with SDL.
  #
  # A window program is a sequence of `WindowInstr`s (window instructions). Window
  # programs can be synthesized from a window state transition. See `WindowProgram.reconcile`.
  struct WindowProgram
    # :nodoc:
    def initialize(@instrs : Array(WindowInstr::Any))
    end

    # Derives a window program and the associated retained state, bundled as
    # `WindowPlan`, for *window id*, based on a *desired* window specification
    # and scenery.
    def self.reconcile(
      window_id : UInt64,
      cache : Scenery::CacheSet,
      desired : {WindowSpec, Term},
    ) : WindowPlan
      spec1, scenery_src = desired

      instrs = [] of WindowInstr::Any

      width, height = Scenery::PixelRect.clamp(spec1.width, spec1.height)
      instrs << WindowInstr::Create.new(window_id, spec1.title, width.to_u32, height.to_u32, spec1.backdrop)
      instrs << WindowInstr::SetResizable.new(window_id, spec1.flags.resizable?)
      instrs << WindowInstr::SetCursor.new(window_id, spec1.cursor)

      if spec1.flags.input?
        instrs << WindowInstr::BeginInput.new(window_id)
      end

      case spec1.form
      in .floating?
        instrs << WindowInstr::Show.new(window_id)
      in .minimized?
        instrs << WindowInstr::Show.new(window_id)
        instrs << WindowInstr::Minimize.new(window_id)
      in .maximized?
        instrs << WindowInstr::Show.new(window_id)
        instrs << WindowInstr::Maximize.new(window_id)
      in .closed?
        instrs << WindowInstr::Hide.new(window_id)
      end

      scenesrc0 = nil
      scenesrc1 = Scenery::Safe.scenesrc(cache, scenesrc0, scenery_src)
      scene = Scenery::Safe.poll(cache, scenesrc1, Magnitude.new(width), Magnitude.new(height)).unwrap
      command = Scenery::Safe.depict(cache, scene)
      dirty_rects = Slice[Scenery::Rect[0, 0, width, height]]
      instrs << WindowInstr::Draw.new(window_id, command, dirty_rects)

      WindowPlan.new(new(instrs), spec1, scenesrc1, scene, command)
    end

    # Derives a window program and the associated retained state, bundled as
    # `WindowPlan`, for *window id*, based on the *current* state, and a *desired*
    # window specification and scenery.
    def self.reconcile(
      window_id : UInt64,
      cache : Scenery::CacheSet,
      current : {WindowSpec, Scenery::SceneSource, Scenery::DrawCommand},
      desired : {WindowSpec, Term},
    ) : WindowPlan
      spec0, scenesrc0, command0 = current
      spec1, scenery_src = desired

      instrs = [] of WindowInstr::Any

      unless spec0.title == spec1.title
        instrs << WindowInstr::SetTitle.new(window_id, spec1.title)
      end

      pass do
        next unless spec1.flags.controls_size?

        size0 = Scenery::PixelRect.clamp(spec0.width, spec0.height)
        size1 = Scenery::PixelRect.clamp(spec1.width, spec1.height)
        next if size0 == size1

        width, height = size1
        instrs << WindowInstr::SetSize.new(window_id, width.to_u32, height.to_u32)
      end

      pass do
        next unless spec1.flags.controls_resizable?
        next if spec0.flags.resizable? == spec1.flags.resizable?

        instrs << WindowInstr::SetResizable.new(window_id, spec1.flags.resizable?)
      end

      unless spec0.backdrop == spec1.backdrop
        instrs << WindowInstr::SetBackdrop.new(window_id, spec1.backdrop)
      end

      unless spec0.cursor == spec1.cursor
        instrs << WindowInstr::SetCursor.new(window_id, spec1.cursor)
      end

      case {spec0.flags.input?, spec1.flags.input?}
      in {false, false}, {true, true}
      in {false, true}
        instrs << WindowInstr::BeginInput.new(window_id)
      in {true, false}
        instrs << WindowInstr::EndInput.new(window_id)
      end

      pass do
        next unless spec1.flags.controls_form?
        next if spec0.form == spec1.form

        # This is very ugly, I know, but it's very easy to see what's going on here
        # and why. I don't think compressing this is a good idea...
        case {spec0.form, spec1.form}
        when {.floating?, .minimized?}
          instrs << WindowInstr::Minimize.new(window_id)
        when {.floating?, .maximized?}
          instrs << WindowInstr::Maximize.new(window_id)
        when {.floating?, .closed?}
          instrs << WindowInstr::Hide.new(window_id)
        when {.minimized?, .floating?}
          instrs << WindowInstr::Restore.new(window_id)
        when {.minimized?, .maximized?}
          instrs << WindowInstr::Maximize.new(window_id)
        when {.minimized?, .closed?}
          instrs << WindowInstr::Hide.new(window_id)
        when {.maximized?, .floating?}
          instrs << WindowInstr::Restore.new(window_id)
        when {.maximized?, .minimized?}
          instrs << WindowInstr::Minimize.new(window_id)
        when {.maximized?, .closed?}
          instrs << WindowInstr::Hide.new(window_id)
        when {.closed?, .floating?}
          instrs << WindowInstr::Show.new(window_id)
          instrs << WindowInstr::Restore.new(window_id)
        when {.closed?, .maximized?}
          instrs << WindowInstr::Show.new(window_id)
          instrs << WindowInstr::Maximize.new(window_id)
        when {.closed?, .minimized?}
          instrs << WindowInstr::Show.new(window_id)
          instrs << WindowInstr::Minimize.new(window_id)
        end
      end

      scenesrc1 = Scenery::Safe.scenesrc(cache, scenesrc0, scenery_src)
      unless spec1.flags.controls_size?
        # We know the true window size and the spec doesn't.
        spec1 = spec1.copy_with(width: spec0.width, height: spec0.height)
      end
      scene = Scenery::Safe.poll(cache, scenesrc1, spec1.width, spec1.height).unwrap
      command1 = Scenery::Safe.depict(cache, scene)

      full_redraw = new(instrs).needs_full_redraw?
      if full_redraw
        # Don't waste time computing dirty rects if everything will be
        # redrawn anyway.
        dirty_rects = Slice[Scenery::Rect[0, 0, spec1.width, spec1.height]]
      else
        dirty_rects = Scenery::Safe.diff(command0, command1)
      end

      # dirty_rects can be empty if Scenery::Safe.diff() finds no difference
      # between the two draw commands.
      if dirty_rects.present?
        instrs << WindowInstr::Draw.new(window_id, command1, dirty_rects)
      end

      WindowPlan.new(new(instrs), spec1, scenesrc1, scene, command1)
    end

    # Returns `true` if there are no instructions in this program.
    def empty? : Bool
      @instrs.empty?
    end

    # Returns `true` if instructions in this program would require a full redraw.
    def needs_full_redraw? : Bool
      @instrs.any? do |instr|
        case instr
        when WindowInstr::SetBackdrop, WindowInstr::SetSize
          true
        else
          false
        end
      end
    end

    # Yields each window instruction comprising this program, inorder.
    def each_instr(& : WindowInstr::Any ->) : Nil
      @instrs.each { |instr| yield instr }
    end
  end

  class WindowSession
    # :nodoc:
    defcase State,
      spec : WindowSpec,
      scenesrc : Scenery::SceneSource,
      scene : Scenery::Scene,
      command : Scenery::DrawCommand,
      mutation: true

    # Returns the id assigned to this window session.
    #
    # It is a process-unique, monotonic id assigned by `WindowManager`.
    # It is used in `WindowProgram`s emitted by this session.
    # It is assumed that events directed at this window session through
    # `accept` target that id.
    getter session_id : UInt64

    @state : State?

    def initialize(@session_id : UInt64, @programs : MediaProgramQueue)
      @cache = Scenery::Safe.cache
      @mouse_models = {} of UInt64 => Term
      @mouse_positions = {} of UInt64 => Scenery::Point
    end

    # Returns the scene that is being displayed in the OS window.
    def scene? : Scenery::Scene?
      @state.try(&.scene)
    end

    # Returns `true` if any mouse button of the mouse with the given *id*
    # is pressed.
    def mouse_pressed?(mouse_id : UInt64) : Bool
      return false unless mouse_model = @mouse_models[mouse_id]?
      return false if mouse_model.itemsonly?

      mouse_model.includes?(:left) ||
        mouse_model.includes?(:middle) ||
        mouse_model.includes?(:right) ||
        mouse_model.includes?(:back) ||
        mouse_model.includes?(:forward)
    end

    # Returns the mouse model associated with *mouse id*. Raises `KeyError` if
    # not found.
    def mouse_model(mouse_id : UInt64) : Term
      @mouse_models[mouse_id]
    end

    # Yields most up-to-date positions of each mouse seen so far.
    def each_mouse_position(& : UInt64, Scenery::Point ->) : Nil
      @mouse_positions.each do |mouse_id, position|
        yield mouse_id, position
      end
    end

    # :nodoc:
    #
    # E.g. a font file changed or appeared, an image was loaded, etc...
    def accept(message : ResourceChanged) : Nil
      # Redraw autonomously.
      redraw
    end

    # :nodoc:
    def accept(message : MousePlugged) : Nil
      @mouse_models[message.mouse_id] = Term.of(:mouse, message.mouse_id)
    end

    # :nodoc:
    def accept(message : MouseUnplugged) : Nil
      @mouse_models.delete(message.mouse_id)
    end

    # :nodoc:
    def accept(message : MouseMessage) : Nil
      # The mouse that we start with does not trigger a MousePlugged event.
      # We have to create the entry for it on the first message.
      @mouse_models.put_if_absent(message.mouse_id) { Term.of(:mouse, message.mouse_id) }

      # Update the model.
      case message
      in MouseMoved
        @mouse_positions[message.mouse_id] = Scenery::Point[message.x, message.y]
      in MouseButtonPressed
        @mouse_models.update(message.mouse_id) do |model|
          Term.morph(model, {message.button, message.clicks})
        end
      in MouseButtonReleased
        @mouse_models.update(message.mouse_id) do |model|
          Term.morph(model, {message.button, nil})
        end
      end
    end

    # :nodoc:
    def accept(message : KeyboardMessage) : Nil
    end

    # :nodoc:
    #
    # We need to modify the wm's spec according to *message*; otherwise, the system
    # will think *it's* the one initiating the resize, minimize, etc.; which would
    # lead to duplicate commands and other weirdness.
    def accept(message : WindowSizeChanged) : Nil
      return unless state = @state

      state.spec = state.spec.copy_with(
        width: Magnitude.new(message.width),
        height: Magnitude.new(message.height)
      )

      # Redraw the content autonomously until the circuit reacts, but mainly because
      # it may choose not to.
      #
      # Note that the lower level (e.g. SdlLoop) is assumed to redraw autonomously, too,
      # but it only does the most basic kind of redraw, which is basically to put
      # the old (un-resized) window content in the top-left corner and clear any
      # extra space with the backdrop color, or, if the resulting size is smaller,
      # just crop the un-resized content. What we do here though, on our level of
      # autonomy, is relayout and only then redraw, which is a different thing.
      redraw
    end

    # :nodoc:
    def accept(message : WindowHoverChanged) : Nil
      return if message.hover

      @mouse_positions.clear
      @mouse_models.transform_values! { |mouse_model| Term.of(mouse_model.itemspart) }
    end

    # :nodoc:
    def accept(message : WindowFocusChanged) : Nil
    end

    # :nodoc:
    def accept(message : WindowFormChanged) : Nil
      return unless state = @state

      state.spec = state.spec.copy_with(form: message.form)
    end

    {% if flag?(:docs) %}
      # A window's `WindowSession`, metaphorically, functions like the "autonomous nervous
      # system" of a window; so it, too, needs to process some events and messages. You
      # *always* have to feed them to it, and you *always* have to do that before engaging
      # the "central nervous system" -- the circuit.
      def accept(message : ResourceChanged | MousePlugged | MouseUnplugged | MouseMessage | KeyboardMessage | WindowMessage) : Nil
      end
    {% end %}

    private def redraw : Nil
      return unless state = @state # We have no state to redraw from...

      instrs = [] of WindowInstr::Any

      # FIXME: Redraw existing AssetNode here instead of creating a new scene!!!
      # FIXME: This shouldn't be here, maybe move to WindowProgram?
      scene = Scenery::Safe.poll(@cache, state.scenesrc, Magnitude.new(state.spec.width), Magnitude.new(state.spec.height)).unwrap
      command = Scenery::Safe.depict(@cache, scene)
      dirty_rects = Slice[Scenery::Rect[0, 0, state.spec.width, state.spec.height]]
      instrs << WindowInstr::Draw.new(@session_id, command, dirty_rects)
      @programs << WindowProgram.new(instrs)

      state.scene = scene
      state.command = command
    end

    def submit(spec : WindowSpec, scenery_src : Term) : Nil
      if state = @state
        current = {state.spec, state.scenesrc, state.command}
        desired = {spec, scenery_src}
        plan = WindowProgram.reconcile(@session_id, @cache, current, desired)

        # Update retained state.
        state.spec = plan.spec
        state.scenesrc = plan.scenesrc
        state.scene = plan.scene
        state.command = plan.command
      else
        desired = {spec, scenery_src}
        plan = WindowProgram.reconcile(@session_id, @cache, desired)

        # Update retained state.
        @state = State.new(plan.spec, plan.scenesrc, plan.scene, plan.command)
      end

      return if plan.program.empty?

      @programs << plan.program
    end

    # Communicates teardown to the associated window. You must not reuse
    # `WindowSession` afterwards. (The behavior in that case is unspecified;
    # it will probably work but you shouldn't even have to try in the first place.)
    def terminate : Nil
      @programs << WindowProgram.new([WindowInstr::Delete.new(@session_id)] of WindowInstr::Any)
    end
  end

  class WindowManager
    @@ids = Atomic(UInt64).new(0u64)

    def initialize(@programs : MediaProgramQueue)
      @epoch = 0u64
      @sessions = {} of Term => WindowSession
      @last_connected_at = {} of Term => UInt64
    end

    # Returns `true` if no window sessions are active.
    def empty? : Bool
      @sessions.empty?
    end

    # Relays *event* to all active window sessions (`WindowSession#accept`).
    def broadcast(event : Event) : Nil
      @sessions.each { |_, session| session.accept(event) }
    end

    # Returns the current key term for the given window *session id*.
    def key?(session_id : UInt64) : Term?
      @sessions.each do |key, session|
        next unless session.session_id == session_id
        return key
      end
    end

    # Accesses a window session by *key* without prolonging it.
    def join?(key : Term) : WindowSession?
      @sessions[key]?
    end

    # Accesses a window session by *key* and prolongs its lifetime by one
    # epoch (`gc`).
    def connect(key : Term) : WindowSession
      unless session = @sessions[key]?
        return open(key)
      end

      @last_connected_at[key] = @epoch

      session
    end

    private def open(key : Term) : WindowSession
      assert !@sessions.has_key?(key)
      assert !@last_connected_at.has_key?(key)

      session = WindowSession.new(@@ids.add(1, :relaxed), @programs)
      @sessions[key] = session
      @last_connected_at[key] = @epoch

      session
    end

    # Garbage collects all windows that were not referenced during the current
    # window manager epoch. Marks the start of a new epoch.
    def gc : Nil
      @sessions.select! do |key, session|
        key_epoch = @last_connected_at[key]
        assert key_epoch <= @epoch

        if key_epoch == @epoch
          next true # Keep
        end

        @last_connected_at.delete(key)
        session.terminate

        false # reject
      end

      @epoch += 1
    end
  end
end
