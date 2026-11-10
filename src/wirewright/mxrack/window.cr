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
    key : Term,
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
    #   key_⋮ main
    #   title⋮ "Untitled"
    #   width⋮ 500
    #   height⋮ 400
    #   cursor_⋮ arrow
    #   backdrop_⋮ (oklch 0.2 0 0)
    #   input⋮ false
    #   resizable⋮ true
    #   form⋮ floating)
    #
    # |@key key
    # The unique key identifying the window. If you have multiple windows you are
    # recommended to assign each one a unique key. Otherwise, Wirewright will use
    # the window node's address, which may change for the same window throughout
    # the evolution of the circuit, causing the window to be destroyed and re-created
    # unnecessarily (because Wirewright would think it's a different window).
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
      key_⋮ main
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

      WindowSpec.new(key,
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
    addr : D7::NodeAddr,
    scenery : Term,
    inputs : Slice(Input),
    mice : Slice(Mouse),
    mouseareas : Slice(MouseArea),
    questions : Array(Question),
    spec : WindowSpec

  class Window
    def key : Term
      @spec.key
    end
  end

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
      in MouseDragged
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

  # Returns `true` if *node* is a *visual* node, that is, a node that must be
  # retained for visualization purposes.
  #
  # This function requires a Microfold codex because whether or not a *node* is
  # visual may depend on whether or not it is a Microfold preset node.
  #
  # See `mxrack.[visual]` in the doctool for more info on how visual nodes are found.
  def visual?(microfold : Microfold::Codex, node : Term::Dict) : Bool
    return false unless head = node.items.first?

    # |@ mxrack.[visual]
    #
    # |@summary
    # "Traits" of nodes that make them *visual* for MxRack.

    # |@ mxrack.[visual]
    #
    # |@block
    # A node with a `style` pair is unconditionally a visual node.
    #
    # |@example
    # Leaf:
    # ```wwml
    # (foo style: "bg-red-500")
    # ```
    #
    # Parent:
    # ```wwml
    # (group style: "bg-red-500"
    #   (cell @x 0)
    #   (p @x))
    #
    # (module {} style: "bg-red-500"
    #   (cell @x 0)
    #   (p @x))
    # ```
    if node.includes?(:style)
      return true
    end

    # |@ mxrack.[visual]
    #
    # |@block
    # Style-less groups are **not** visual nodes because they are not necessarily
    # visual -- most often theylre not.
    #
    # |@example
    # The nested group is **not** visual because it does not define a `style: _` (even
    # an empty style would do); so paragraphs render in a row:
    #
    # ```wwml
    # (group style: "flow-row"
    #   (group
    #     (p "A")
    #     (p "B")))
    # ```
    #
    # The moment we add a `style` pair to the nested group it renders the paragraphs
    # in a column:
    #
    # ```wwml
    # (group style: "flow-row"
    #   (group style: "" ;; Added empty `style`
    #     (p "A")
    #     (p "B")))
    # ```
    #
    # This is because the nested group is now treated as a visual node. According
    # to the standard Microfold codex, a `group` is `flow-col` by default, so what
    # we have here, visually, is a row with one element in it, that element being
    # a column of two paragraphs A and B.
    if head == Term.of(:group)
      return false
    end

    # |@ mxrack.[visual]
    #
    # |@block
    # A node with a preset head is a visual node. Whether or not the head of
    # a node is a preset head is determined based on the Microfold codex passed
    # to MxRack. Most often this is the standard Microfold codex, which defines
    # style *presets* for heads `p`, `box`, `h1`, etc.
    #
    # |@example
    # ```wwml
    # (h1 "Hello World")
    # (p "Count is 0")
    # ```
    #
    # In this example, `h1` and `p` are both preset heads (according to the standard
    # Microfold codex), so they are both treated as visual nodes.
    if microfold.preset?(head)
      return true
    end

    # |@ mxrack.[visual]
    #
    # |@block
    # A node with a recognizable Scenery leaf or parent head is a visual node.
    # See `scenery` for a list of Scenery nodes.
    #
    # |@example
    # A Scenery leaf head such as `img` (`scenery.img`) turns the node into
    # a visual node:
    #
    # ```wwml
    # (img src: (uri "https://picsum.photos/300/300"))
    # ```
    #
    # Similarly, a Scenery parent head such as `x-stack` (`scenery.stack`) turns
    # it into a visual node:
    #
    # ```wwml
    #  (x-stack
    #    (cell @x "A") ;; < These are non-visual so they are dropped when visualizing the circuit.
    #    (cell @y "B") ;; <
    #    (cell @z "C") ;; <
    #    (p @x)  ;; <
    #    (p @y)  ;; <
    #    (p @z)) ;; < Whereas these are visual so they are retained.
    # ```
    if Scenery::KnowledgeBase.leaf_head?(head) || Scenery::KnowledgeBase.parent_head?(head)
      return true
    end

    false
  end

  # :ditto:
  def visual?(microfold : Microfold::Codex, node : Term) : Bool
    return false unless dict = node.as_d?

    visual?(microfold, dict)
  end

  class ViAddrSeq(Node)
    getter addr : D7::NodeAddr
    getter successor : Node

    def initialize(@addr : D7::NodeAddr, @successor : Node)
      while node = @successor.as?(ViAddrSeq(Node))
        @addr = D7::NodeAddr.concat(@addr, node.addr)
        @successor = node.successor
      end
    end

    def self.new(key : UInt32, successor : Node)
      new(D7::NodeAddr[key], successor)
    end
  end

  @[::Flags]
  enum ViSummary
    HasQuestion
    HasWindow
    HasEnsemble
    HasSite
    HasInstantiable

    def self.of(node : ViRecognized::Node) : ViSummary
      summary = None

      case node
      in ViRecognized::Leaf
        summary |= HasQuestion if node.questions.present?
      in ViRecognized::Gnd
        summary |= HasQuestion if node.questions.present?
        summary |= HasInstantiable if node.pairs.present?
      in ViRecognized::Site
        summary |= HasSite
      in ViRecognized::Hitarea
      in ViRecognized::Visual
        summary |= HasInstantiable
      in ViRecognized::Guard
        summary |= HasInstantiable
        summary |= ViSummary.of(node.child)
      in ViRecognized::Group
        summary |= node.summary
      in ViRecognized::Ensemble
        summary |= HasEnsemble
        summary |= ViSummary.of(node.children)
      in ViAddrSeq(ViRecognized::Node)
        summary |= ViSummary.of(node.successor)
      end

      summary
    end

    def self.of(node : ViInstantiated::Node) : ViSummary
      summary = None

      case node
      in ViInstantiated::Leaf
        summary |= HasQuestion if node.questions.present?
      in ViInstantiated::Site
        summary |= HasSite
      in ViInstantiated::Hitarea
      in ViInstantiated::Group
        summary |= node.summary
      in ViInstantiated::Ensemble
        summary |= HasEnsemble
        summary |= ViSummary.of(node.children)
      in ViInstantiated::Thunk
        summary |= ViSummary.of(node.successor)
      in ViAddrSeq(ViInstantiated::Node)
        summary |= ViSummary.of(node.successor)
      end

      summary
    end

    def self.of(node : ViSituated::Node) : ViSummary
      summary = None

      case node
      in ViSituated::Leaf
        summary |= HasQuestion if node.questions.present?
      in ViSituated::Hitarea
      in ViSituated::Group
        summary |= node.summary
      in ViSituated::Thunk
        summary |= ViSummary.of(node.successor)
      in ViAddrSeq(ViSituated::Node)
        summary |= ViSummary.of(node.successor)
      end

      summary
    end

    def self.of(objects : Slice) : ViSummary
      objects.reduce(None) { |summary, object| summary | of(object) }
    end

    def self.of(head : Term, children : Slice(ViRecognized::Node), questions : Slice(ViRecognized::QuestionId), pairs : Slice(ViRecognized::Pair)) : ViSummary
      summary = of(children)
      summary |= HasWindow if head == Term.of(:window)
      summary |= HasInstantiable if pairs.present?
      summary |= HasQuestion if questions.present?
      summary
    end

    def self.of(head : Term, children : Slice, questions : Slice) : ViSummary
      summary = of(children)
      summary |= HasWindow if head == Term.of(:window)
      summary |= HasQuestion if questions.present?
      summary
    end
  end

  alias ViComponentList = Slice({Term, Term})

  module ViPairFormat
    extend self

    alias Any = None | Compact

    defrecord None
    defrecord Compact

    def render(format : None, term : Term) : Term
      term
    end

    def render(format : Compact, term : Term) : Term
      term.type.string? ? term : Term.of(ML.compact(term))
    end
  end

  module ViRecognized
    extend self

    alias Node = Leaf | Gnd | Site | Hitarea | Visual | Guard | Group | Ensemble | ViAddrSeq(Node)

    defrecord Leaf, node : Term, questions : Slice(QuestionId)
    defrecord Gnd, node : Term::Dict, questions : Slice(QuestionId), pairs : Slice(Pair)
    defrecord Site
    defrecord Hitarea
    defrecord Visual, template : Alloy::CompiledTemplate

    defcase Guard, predicate : D7::PassablePredicate, child : Node

    defrecord Group,
      head : Term,
      children : Slice(Node),
      questions : Slice(QuestionId),
      pairs : Slice(Pair),
      pairspart : Term::Dict,
      summary : ViSummary

    struct Group
      def self.new(head : Term, children : Slice(Node), questions : Slice(QuestionId), pairs : Slice(Pair), pairspart : Term::Dict) : Group
        summary = ViSummary.of(head, children, questions, pairs)
        new(head, children, questions, pairs, pairspart, summary)
      end
    end

    defrecord QuestionId, name : Term::Sym

    defrecord Pair, key : Term, value : Edge | Expr, format : ViPairFormat::Any
    defrecord Edge, edge : Term
    defrecord Expr, expr : Term

    defrecord Ensemble, children : Slice(Node)

    # :nodoc:
    def questions_and_pairs(dict : Term::Dict) : {Slice(QuestionId), Slice(Pair)}
      questions = Pf::Kit.stack_array(QuestionId, 4)
      pairs = Pf::Kit.stack_array(Pair, 4)

      dict.each_entry(in: Term::Dict.pairspart) do |key, value|
        pass do
          next unless keysym = key.as_sym?
          next unless keysym.prefixed_by?('?')

          questions << QuestionId.new(keysym)
        end

        # This is rather hot so we can't afford M1 here. The pattern is roughly:
        #   key: (%any° @expr_ (^ expr_))
        pass do
          next unless candidate = value.as_itemsonly_d?
          next unless candidate.itemsize == 2

          head, _ = candidate

          case head
          when SYM_EDGE # @expr_
            pairs << Pair.new(key, value: Edge.new(Term.of(value)), format: ViPairFormat::None.new)
          when SYM_CARET # (^ expr_)
            _, expr = candidate
            pairs << Pair.new(key, value: Expr.new(expr), format: ViPairFormat::None.new)
          end
        end
      end

      {questions.to_unsafe_readonly_slice!, pairs.to_unsafe_readonly_slice!}
    end

    def recognize(node : Term) : Leaf | Gnd
      unless dict = node.as_d?
        return Leaf.new(node, Slice(QuestionId).empty)
      end

      questions, pairs = questions_and_pairs(dict)
      pairs.empty? ? Leaf.new(node, questions) : Gnd.new(dict, questions, pairs)
    end

    alias RecognizeCache = DependentCache({UInt64, ViComponentList, Term}, Slice(Node))

    def recognize(setup : WindowingSetup, components : ViComponentList, tree : D7::InertLeaf) : Slice(Node)
      node = tree.feature.node
      unless MxRack.visual?(setup.microfold, node)
        return Slice(Node).empty
      end

      Slice(Node).of(recognize(node))
    end

    def recognize!(setup : WindowingSetup, components : ViComponentList, tree : D7::InertLeaf | D7::GndLeaf) : Slice(Node)
      node = tree.feature.node

      Term.case(node) do
        matchpi %{[visual _*]} do
          template = Term.morph(Term.of(node.itemspart), {0, :"^splice"})
          comp = Alloy.compile(Alloy.recognize(template, components))
          Slice(Node).of(Visual.new(comp))
        end

        matchpi %{[p @edge_]} do
          questions, pairs = questions_and_pairs(node.as_d)
          pair = Pair.new(key: Term.of(1), value: Edge.new(edge), format: ViPairFormat::Compact.new)
          pairs = pairs.append(pair)
          Slice(Node).of(Gnd.new(node.as_d, questions, pairs))
        end

        matchpi %{[p (^ expr_)]} do
          questions, pairs = questions_and_pairs(node.as_d)
          pair = Pair.new(key: Term.of(1), value: Expr.new(expr), format: ViPairFormat::Compact.new)
          pairs = pairs.append(pair)
          Slice(Node).of(Gnd.new(node.as_d, questions, pairs))
        end

        matchpi %{[site]} do
          Slice(Node).of(Site.new)
        end

        matchpi %{[mousearea _*]} do
          Slice(Node).of(Hitarea.new)
        end

        otherwise do
          unless MxRack.visual?(setup.microfold, node)
            return Slice(Node).empty
          end

          Slice(Node).of(recognize(node))
        end
      end
    end

    def recognize(setup : WindowingSetup, components : ViComponentList, tree : D7::InertLeaf | D7::GndLeaf) : Slice(Node)
      setup.cache.put_if_absent({setup.epoch, components, tree.feature.node}) do
        recognize!(setup, components, tree)
      end
    end

    def recognize(setup : WindowingSetup, components : ViComponentList, tree : D7::MixtureNode | D7::ScopeNode) : Slice(Node)
      recognize(setup, components, tree.child)
    end

    def recognize(setup : WindowingSetup, components : ViComponentList, tree : D7::CircuitNode) : Slice(Node)
      recognize(setup, components, tree.to_group)
    end

    def recognize!(setup : WindowingSetup, components : ViComponentList, tree : D7::GroupNode) : Slice(Node)
      node = tree.feature.node
      predicate = tree.feature.passable

      children = Pf::Kit.stack_array(Node)
      tree.children.zip(tree.feature.range) do |child, key|
        recognize(setup, components, child).each do |successor|
          children << ViAddrSeq(Node).new(key, successor)
        end
      end

      visual = false

      Term.case(node) do
        # Force (window) and (automaton) nodes to be visual. Whereas for window that
        # makes sense, for automaton things are a little more nuanced. We actually omit
        # automaton from the resulting markup later on. But we need to know where
        # automata are in the tree to orient ourselves & make correct decisions in
        # later passes. So that's why we mark automaton as visual.
        matchpi %{[window _*]}, %{[automaton _*]} do
          visual = true
        end

        # Emit Ensemble. Again, all further processing is deferred at this point.
        matchpi %{[ensemble _*]} do
          return Slice(Node).of(Ensemble.new(children.to_unsafe_readonly_slice!))
        end

        otherwise { }
      end

      head = tree.feature.signature.try(&.head)
      if head.nil? || !(visual || MxRack.visual?(setup.microfold, node))
        case predicate
        in D7::Passable.class
        in D7::PassablePredicate
          children.map! { |child| Guard.new(predicate, child) }
        end
        return children.to_unsafe_readonly_slice!
      end

      questions, pairs = questions_and_pairs(node)
      group = Group.new(head, children.to_unsafe_readonly_slice!, questions, pairs, node.pairspart)
      case predicate
      in D7::Passable.class
        Slice(Node).of(group)
      in D7::PassablePredicate
        Slice(Node).of(Guard.new(predicate, group))
      end
    end

    def recognize(setup : WindowingSetup, components : ViComponentList, tree : D7::GroupNode) : Slice(Node)
      setup.cache.put_if_absent({setup.epoch, components, Term.of(tree.feature.node)}) do
        recognize!(setup, components, tree)
      end
    end

    # :nodoc:
    def components(tree : D7::InertLeaf) : Slice({Term, Term})
      Slice({Term, Term}).empty
    end

    # :nodoc:
    def components(tree : D7::GndLeaf) : Slice({Term, Term})
      Term.case(tree.feature.node) do
        matchpi %{[rule pattern_ body_]} do
          Slice[{pattern, body}]
        end

        otherwise do
          Slice({Term, Term}).empty
        end
      end
    end

    # :nodoc:
    def components(tree : D7::MixtureNode | D7::ScopeNode) : Slice({Term, Term})
      components(tree.child)
    end

    # :nodoc:
    def components(tree : D7::CircuitNode) : Slice({Term, Term})
      components(tree.to_group)
    end

    # :nodoc:
    def components(tree : D7::GroupNode) : Slice({Term, Term})
      unless tree.summary.has_signature?(D7::NodeSignature.new(Term.of(:rule), 3))
        return Slice({Term, Term}).empty
      end

      rules = Pf::Kit.stack_array({Term, Term}, 8)
      tree.children.each do |child|
        rules.concat(components(child))
      end

      rules.to_unsafe_readonly_slice!
    end

    {% if flag?(:docs) %}
      # Returns visual component rules in *circuit*.
      def components(circuit : D7::ParseTree) : Slice({Term, Term})
      end
    {% end %}
  end

  module ViInstantiated
    extend self

    alias Prev = ViRecognized

    alias Node = Leaf | Site | Hitarea | Group | Ensemble | Thunk | ViAddrSeq(Node)

    alias Leaf = Prev::Leaf
    alias Site = Prev::Site
    alias Hitarea = Prev::Hitarea
    alias QuestionId = Prev::QuestionId

    defrecord Group,
      head : Term,
      children : Slice(Node),
      questions : Slice(QuestionId),
      pairspart : Term::Dict,
      summary : ViSummary

    struct Group
      def self.new(head : Term, children : Slice(Node), questions : Slice(QuestionId), pairspart : Term::Dict)
        summary = ViSummary.of(head, children, questions)
        new(head, children, questions, pairspart, summary)
      end
    end

    defrecord Ensemble, children : Slice(Node)

    # Instead of eagerly converting a potentially large input tree to a ViInstantiated,
    # we create thunks.
    defrecord Thunk, successor : Prev::Node

    # FIXME: Do not use exceptions for control flow!
    private class EvalAborted < Exception
      @callstack = CallStack.empty
    end

    # The eval function we pass to Nitrene which it then calls whenever it wants
    # us to evaluate a term. More specifically, this is a *composite* function,
    # although there is no difference at this level.
    #
    # So when Nitrene asks us to evaluate a term, and that term is an edge, we
    # use the hypergraph to determine the value at the edge, and return that
    # to Nitrene. If the edge does not exist we blow up with `EvalAborted`.
    private def evalf(hg : D7::Hypergraph, addr : D7::NodeAddr) : Nitrene::Eval
      Nitrene::Eval.new do |it, vars, expr|
        # (edge _)  (edge? _)
        next Nitrene.inert unless dict = expr.as_d?
        next Nitrene.inert unless dict.itemsonly?
        next Nitrene.inert unless dict.itemsize == 2
        next Nitrene.inert unless dict.probably_includes_any?(SYM_EDGE, SYM_EDGEP)

        head, _ = dict

        case head
        when SYM_EDGE # (edge _), more freq than EDGEP
          value = Rack.cell?(hg, hg.resolve(addr, expr)).try(&.value?)
          value || raise EvalAborted.new
        when SYM_EDGEP # (edge? _)
          _, name = expr
          value = Rack.cell?(hg, hg.resolve(addr, Term.of(:edge, name))).try(&.value?)
          value ? Term.of(:some, value) : Term.of(:none)
        else
          Nitrene.inert
        end
      end
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, node : Term::Dict, pairs : Slice(Prev::Pair)) : Term::Dict
      if pairs.empty?
        return node
      end

      node.transaction do |commit|
        it = Nitrene::Interpreter.new(
          Nitrene.primitive,
          Nitrene.either(evalf(hg, addr), Nitrene.composite),
        )

        pairs.each do |pair|
          case value = pair.value
          in Prev::Edge
            # x: @mode
            expr = value.edge
          in Prev::Expr
            # x: ^(= @mode insert)
            expr = value.expr
          end

          begin
            vars = Term[]
            result = Nitrene.eval(it, vars, expr)

            # For example:
            #   (cell @name "Bob")
            #   (cell @hover true)
            #   (p "Hello" ⏏hover: ^@hover⏏)            -> (p "Hello" ⏏hover: true⏏)
            #   (p "Hello" ⏏hover: ^(= @name "Alice")⏏) -> (p "Hello" ⏏hover: false⏏)
            commit.with(pair.key, ViPairFormat.render(pair.format, result))
          rescue EvalAborted
            # Omit if any edge is undefined. For example:
            #   (module {}
            #     (p "Hello" ⏏hover: ^@hover⏏)) -> (p "Hello")
            commit.without(pair.key)
          end
        end
      end
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, input : Prev::Leaf | Prev::Site | Prev::Hitarea) : Slice(Node)
      Slice(Node).of(input)
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, input : Prev::Gnd) : Slice(Node)
      instance = instantiate(hg, addr, input.node, input.pairs)
      leaf = Leaf.new(Term.of(instance), input.questions)
      Slice(Node).of(leaf)
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, input : Prev::Visual) : Slice(Node)
      rep = Alloy.render_rep(input.template, composite: evalf(hg, addr))
      rep.to_readonly_slice do |instance|
        Leaf.new(instance, Slice(QuestionId).empty).as(Node)
      end
    rescue EvalAborted
      Slice(Node).empty
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, input : Prev::Guard) : Slice(Node)
      # Impassable GroupNodes must not make it into the visual tree. For instance:
      #
      #   (cell @x 0)
      #   (guard (@x 0) (p "x is 0"))
      #   (guard (@x 1) (p "x is 1"))
      #
      # Only the first `p` should be visible.
      unless input.predicate.call(hg, addr)
        return Slice(Node).empty
      end

      instantiate(hg, addr, input.child)
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, input : Prev::Group) : Slice(Node)
      unless input.summary.has_instantiable?
        return Slice(Node).of(Thunk.new(input))
      end

      children = Pf::Kit.stack_array(Node, 8)
      input.children.each do |child|
        children.concat(instantiate(hg, addr, child))
      end

      pairspart = instantiate(hg, addr, input.pairspart, input.pairs)
      group = Group.new(input.head, children.to_unsafe_readonly_slice!, input.questions, pairspart)
      Slice(Node).of(group)
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, input : Prev::Ensemble) : Slice(Node)
      children = Pf::Kit.stack_array(Node, 8)
      input.children.each do |child|
        children.concat(instantiate(hg, addr, child))
      end

      Slice(Node).of(Ensemble.new(children.to_unsafe_readonly_slice!))
    end

    # :nodoc:
    def instantiate(hg : D7::Hypergraph, addr : D7::NodeAddr, input : ViAddrSeq(Prev::Node)) : Slice(Node)
      instantiate(hg, D7::NodeAddr.concat(addr, input.addr), input.successor).to_readonly_slice do |successor|
        ViAddrSeq(Node).new(input.addr, successor).as(Node)
      end
    end

    # *Instantiates* zero or more *roots*.
    #
    # Instantiation turns recognized nodes `Prev::Node` into instantiated nodes
    # `ViInstantiated::Node`. Instantiation subtracts many types via evaluation.
    #
    # The instantiation pass is inherently adverse to memoization because it requires
    # knowledge of the full hypergraph *hg*, and knowledge of the global node address
    # of each node of interest. Instead, to skip work, we rely on summaries computed
    # during the recognition pass to skip work where possible (through `Thunk` thunks).
    def instantiate(hg : D7::Hypergraph, roots : Slice(Prev::Node)) : Slice(Node)
      results = Pf::Kit.stack_array(Node, 4)
      roots.each do |root|
        results.concat(instantiate(hg, D7::NodeAddr.empty, root))
      end
      results.to_unsafe_readonly_slice!
    end

    # :nodoc:
    def unwrap(node : Prev::Leaf | Prev::Site | Prev::Hitarea) : Node
      node
    end

    # :nodoc:
    def unwrap(node : Prev::Gnd | Prev::Visual | Prev::Guard) : Node
      raise TypeCastError.new
    end

    # :nodoc:
    def unwrap(node : Prev::Group) : Node
      children = node.children.to_readonly_slice { |child| Thunk.new(child).as(Node) }
      Group.new(node.head, children, node.questions, node.pairspart)
    end

    # :nodoc:
    def unwrap(node : Prev::Ensemble) : Node
      children = node.children.to_readonly_slice { |child| Thunk.new(child).as(Node) }
      Ensemble.new(children)
    end

    # :nodoc:
    def unwrap(node : ViAddrSeq(Prev::Node)) : Node
      ViAddrSeq(Node).new(node.addr, unwrap(node.successor))
    end

    def unwrap(cast : Thunk) : Node
      unwrap(cast.successor)
    end
  end

  # situate?
  module ViSituated
    extend self

    alias Prev = ViInstantiated

    alias Node = Leaf | Hitarea | Group | Thunk

    defrecord Leaf, addr : D7::NodeAddr, node : Term, questions : Slice(Question)
    defrecord Hitarea, addr : D7::NodeAddr

    defrecord Group,
      addr : D7::NodeAddr,
      head : Term,
      children : Slice(Node),
      questions : Slice(Question),
      pairspart : Term::Dict,
      summary : ViSummary

    struct Group
      def self.new(addr : D7::NodeAddr, head : Term, children : Slice(Node), questions : Slice(Question), pairspart : Term::Dict)
        summary = ViSummary.of(head, children, questions)
        new(addr, head, children, questions, pairspart, summary)
      end
    end

    # Instead of eagerly converting a potentially large instance tree to a ViSituated
    # node we create thunks.
    defrecord Thunk, addr : D7::NodeAddr, successor : Prev::Node

    # :nodoc:
    def situate(input : Prev::Leaf, addr : D7::NodeAddr, argq : Deque(Node)) : Slice(Node)
      questions = input.questions.to_readonly_slice { |question| Question.new(addr, Term.of(question.name)) }
      Slice(Node).of(Leaf.new(addr, input.node, questions))
    end

    # :nodoc:
    def situate(input : Prev::Hitarea, addr : D7::NodeAddr, argq : Deque(Node)) : Slice(Node)
      Slice(Node).of(Hitarea.new(addr))
    end

    # :nodoc:
    def situate(input : Prev::Site, addr : D7::NodeAddr, argq : Deque(Node)) : Slice(Node)
      arg = argq.shift?
      arg ? Slice(Node).of(arg) : Slice(Node).empty
    end

    # :nodoc:
    def situate(input : Prev::Group, addr : D7::NodeAddr, argq : Deque(Node)) : Slice(Node)
      unless input.summary.has_ensemble? || (argq.present? && input.summary.has_site?)
        return Slice(Node).of(Thunk.new(addr, input))
      end

      children = Pf::Kit.stack_array(Node, 8)
      input.children.each do |child|
        children.concat(situate(child, addr, argq))
      end

      questions = input.questions.to_readonly_slice { |question| Question.new(addr, Term.of(question.name)) }
      group = Group.new(addr, input.head, children.to_unsafe_readonly_slice!, questions, input.pairspart)
      Slice(Node).of(group)
    end

    # :nodoc:
    def situate(input : Prev::Ensemble, addr : D7::NodeAddr, argq : Deque(Node)) : Slice(Node)
      unless body = input.children.first?
        return Slice(Node).empty
      end

      subargq = Deque(Node).new
      args = input.children.rest
      args.each { |arg| subargq.concat(situate(arg, addr, argq)) }
      situate(body, addr, subargq)
    end

    # :nodoc:
    def situate(input : Prev::Thunk, addr : D7::NodeAddr, argq : Deque(Node)) : Slice(Node)
      summary = ViSummary.of(input.successor)
      unless summary.has_ensemble? || summary.has_site?
        return Slice(Node).of(Thunk.new(addr, Prev.unwrap(input)))
      end

      situate(Prev.unwrap(input), addr, argq)
    end

    # :nodoc:
    def situate(input : ViAddrSeq(Prev::Node), addr : D7::NodeAddr, argq : Deque(Node)) : Slice(Node)
      situate(input.successor, D7::NodeAddr.concat(addr, input.addr), argq)
    end

    # The situate pass subtracts Ensembles and Sites.
    def situate(roots : Slice(Prev::Node)) : Slice(Node)
      results = Pf::Kit.stack_array(Node, 4)
      roots.each do |root|
        argq = Deque(Node).new
        results.concat(situate(root, D7::NodeAddr.empty, argq))
      end
      results.to_unsafe_readonly_slice!
    end

    # :nodoc:
    def window_markup(input : Leaf) : {Term::Rep, Slice(Question)}
      # Fast path
      if input.questions.empty?
        return Term.rep(input.node), Slice(Question).empty
      end

      # (p "Hello World" ?w: _)
      assert dict = input.node.as_d?

      # Replace question pair values with node addr so that we can reference back
      # from Scenery::Scene to the MxRack node.
      dict = dict.transaction do |commit|
        input.questions.each do |question|
          commit.with(question.key, input.addr)
        end
      end

      {Term.rep_of(dict), input.questions}
    end

    # :nodoc:
    def window_markup(input : Hitarea) : {Term::Rep, Slice(Question)}
      questions = Slice(Question).empty
      {Term.rep(Term.of(:hitarea, input.addr)), questions}
    end

    # :nodoc:
    def window_markup(input : Group) : {Term::Rep, Slice(Question)}
      case input.head
      when Term.of(:window)
        # Ignore nested windows.
        return Term.rep, Slice(Question).empty
      when Term.of(:automaton)
        # All windows below automaton are virtual, meaning their questions belong
        # to the system window. However their markup should be ignored!
        return Term.rep, questions(input)
      end

      # Accumulate questions.
      questions = input.questions

      markup = input.pairspart.transaction do |commit|
        # Replace question pair values with node addr so that we can reference back
        # from Scenery::Scene to the MxRack node.
        questions.each do |question|
          commit.with(question.key, input.addr)
        end

        commit << input.head
        input.children.each do |child|
          child_rep, child_questions = window_markup(child)
          commit.concat(child_rep)
          questions += child_questions
        end
      end

      {Term.rep_of(markup), questions}
    end

    # :nodoc:
    def window_markup(input : Thunk) : {Term::Rep, Slice(Question)}
      window_markup(unwrap(input))
    end

    def window_root_markup(input : Group) : {Term, Slice(Question)}
      assert input.head == Term.of(:window)

      # Accumulate questions.
      questions = Slice(Question).empty

      markup = input.pairspart.transaction do |commit|
        commit << input.head
        input.children.each do |child|
          child_rep, child_questions = window_markup(child)
          commit.concat(child_rep)
          questions += child_questions
        end
      end

      {Term.of(markup), questions}
    end

    # :nodoc:
    def each_window_group(input : Leaf | Hitarea, sink) : Nil
    end

    # :nodoc:
    def each_window_group(input : Group, sink) : Nil
      return unless input.summary.has_window?

      if input.head == Term.of(:window)
        sink.call(input)
      end

      input.children.each do |child|
        each_window_group(child, sink)
      end
    end

    # :nodoc:
    def each_window_group(input : Prev::Thunk | Thunk, sink) : Nil
      each_window_group(unwrap(input), sink)
    end

    def each_window_group(roots : Slice(Node), &fn : Group ->) : Nil
      roots.each { |root| each_window_group(root, fn) }
    end

    # :nodoc:
    def questions(sink, node : Leaf) : Nil
      sink.concat(node.questions)
    end

    # :nodoc:
    def questions(sink, node : Hitarea) : Nil
    end

    # :nodoc:
    def questions(sink, node : Group) : Nil
      return unless node.summary.has_question?

      sink.concat(node.questions)

      node.children.each do |child|
        questions(sink, child)
      end
    end

    # :nodoc:
    def questions(sink, node : Prev::Thunk | Thunk) : Nil
      questions(sink, unwrap(node))
    end

    def questions(node : Node) : Slice(Question)
      sink = Pf::Kit.stack_array(Question)
      questions(sink, node)
      sink.to_unsafe_readonly_slice!
    end

    # :nodoc:
    def unwrap(addr : D7::NodeAddr, node : Prev::Leaf) : Node
      questions = node.questions.to_readonly_slice { |question| Question.new(addr, Term.of(question.name)) }
      Leaf.new(addr, node.node, questions)
    end

    # :nodoc:
    def unwrap(addr : D7::NodeAddr, node : Prev::Hitarea) : Node
      Hitarea.new(addr)
    end

    # :nodoc:
    def unwrap(addr : D7::NodeAddr, node : Prev::Ensemble | Prev::Site) : Node
      raise TypeCastError.new
    end

    # :nodoc:
    def unwrap(addr : D7::NodeAddr, node : Prev::Thunk) : Node
      unwrap(addr, Prev.unwrap(node))
    end

    # :nodoc:
    def unwrap(addr : D7::NodeAddr, node : Prev::Group) : Node
      children = node.children.to_readonly_slice { |child| Thunk.new(addr, child).as(Node) }
      questions = node.questions.to_readonly_slice { |question| Question.new(addr, Term.of(question.name)) }
      Group.new(addr, node.head, children, questions, node.pairspart)
    end

    # :nodoc:
    def unwrap(addr : D7::NodeAddr, node : ViAddrSeq(Prev::Node)) : Node
      unwrap(D7::NodeAddr.concat(addr, node.addr), node.successor)
    end

    def unwrap(node : Thunk) : Node
      unwrap(node.addr, node.successor)
    end
  end

  defrecord StagingWindow,
    addr : D7::NodeAddr,
    spec : WindowSpec,
    scenery : Term,
    questions : Slice(Question),
    copying: true

  # *epoch* is used for cache invalidation. More specifically, it must be incremented
  # on every change to *microfold*.
  defrecord WindowingSetup,
    cache : ViRecognized::RecognizeCache,
    microfold : Microfold::Codex,
    epoch : UInt64

  def windows(setup : WindowingSetup, circuit : D7::ParseTree, hg : D7::Hypergraph, interactive : Bool) : Array(Window)
    components = ViRecognized.components(circuit)
    recognized = ViRecognized.recognize(setup, components, circuit)
    instance = ViInstantiated.instantiate(hg, recognized)
    situated = ViSituated.situate(instance)
    staging = Pf::Kit.stack_array(StagingWindow)

    setup.microfold.epoch do
      ViSituated.each_window_group(situated) do |window_group|
        visuals, questions = ViSituated.window_root_markup(window_group)
        scenery = Microfold.render(setup.microfold, visuals).unwrap
        next unless window_spec = window_spec?(scenery)

        staging << StagingWindow.new(window_group.addr, window_spec, scenery, questions)
      end
    end

    # Key collisions, as in:
    #
    #   (window key: a)
    #   (window key: a)
    #
    # ... are resolved by using the node address. Though not ideal, it does
    # cover many "tricky" changes such as creation of deeper windows and so on,
    # without losing identity. Only if you move the window itself or any of its
    # parents would the identity change, which is arguably as good as we can get
    # without assigning windows random unique ids. (TODO: We should probably do that
    # as an opt-in, though, e.g. `key: random` or something like that which should
    # rewrite to key: "UUID" or something like that before we even have a chance to
    # look at the window).
    if staging.size > 1
      keys = staging.to_bag(&.spec.key)

      staging.map! do |window|
        if keys.count(window.spec.key) == 1
          next window
        end

        spec1 = window.spec.copy_with(key: Term.of(window.addr))
        window.copy_with(spec: spec1)
      end
    end

    staging.map do |window|
      _, window_tree = hg.follow(window.addr)
      assert window_tree.is_a?(D7::GroupNode)
      window_node = WindowNode.new(window.addr, window_tree)

      Window.new(window.addr,
        window.scenery,
        interactive ? inputs(window_node) : Slice(Input).empty,
        interactive ? mice(window_node) : Slice(Mouse).empty,
        interactive ? mouseareas(window_node) : Slice(MouseArea).empty,
        window.questions.to_a, # ?!
        window.spec,
      )
    end
  end

  def windows(setup : WindowingSetup, circuit : D7::ParseTree, *, interactive : Bool = true) : Array(Window)
    windows = [] of Window

    hg = D7::Hypergraph.new(circuit)
    Rack::Prepass.call(hg) do |hg| # shadow
      windows = windows(setup, circuit, hg, interactive)
    end

    windows
  end

  defrecord Question, addr : D7::NodeAddr, key : Term
  defrecord Answer, addr : D7::NodeAddr, key : Term, value : Term

  private def reply?(node : Scenery::ZStack, box : Scenery::Box, vbox : Scenery::VBox, question : Term) : Term?
    # |@ mxrack.[visual question]
    #
    # |@summary
    # Appearance questions supported by visual nodes (of the type, e.g., "What is your width?").
    #
    # |@block
    # Visual nodes can respond to a variety of so-called *visual questions*. You
    # ask these questions in the pairspart of the visual node of interest, and
    # the node keeps answering them, live, as the application evolves.
    #
    # The keys of all visual questions start with `?`. The corresponding values
    # can be anything at all; they will be overwritten by the answer on
    # the next frame.
    #
    # By convention, we seed questions with `_` (the underscore symbol); although
    # if you for some reason do not like that, you can use any other symbol or term.
    # On the next tick, if possible, the seed gets replaced by the node's answer,
    # which is going to be updated by the node from that point onwards.
    #
    # |@example
    # ```wwml
    # ;; Frame 0 (seed)
    #
    # (window width: 500 height: 400
    #   (p "Hello World" ?w: _ ?h: _))
    #
    # ;; Frame 1
    #
    # (window width: 500 height: 400
    #   (p "Hello World" ?w: ≈500 ?h: ≈400))
    # ```
    #
    # In this example, we are asking the `p` about its width (`?w: _`) and height
    # (`?h: _`). In the second frame the `p` answers with `500x400`, which is
    # the same as the size of the window (which the `p` "floods").
    case question
    when Term.of(:"?w")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?w: _}
      #
      # |@block
      # Replaces the value of `?w` with the *logical* width of the visual node.
      # *Logical* sizes are not affected by overflow and transforms.
      Term.of(box.bounds.w)
    when Term.of(:"?h")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?h: _}
      #
      # |@block
      # Replaces the value of `?h` with the *logical* height of the visual node.
      # *Logical* sizes are not affected by overflow and transforms.
      Term.of(box.bounds.h)
    when Term.of(:"?l")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?l: _}
      #
      # |@block
      # Replaces the value of `?l` with the absolute (window-space) left position
      # of the visual node.
      Term.of(box.bounds.x)
    when Term.of(:"?r")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?r: _}
      #
      # |@block
      # Replaces the value of `?r` with the absolute (window-space) right position
      # of the visual node.
      Term.of(box.bounds.x + box.bounds.w)
    when Term.of(:"?t")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?t: _}
      #
      # |@block
      # Replaces the value of `?t` with the absolute (window-space) top position
      # of the visual node.
      Term.of(box.bounds.y)
    when Term.of(:"?b")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?b: _}
      #
      # |@block
      # Replaces the value of `?b` with the absolute (window-space) bottom position
      # of the visual node.
      Term.of(box.bounds.y + box.bounds.h)
    when Term.of(:"?visual-w")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?visual-w: _}
      #
      # |@block
      # Replaces the value of `?visual-w` with the *visual* width of the node. Visual
      # sizes are affected by overflow and transforms.
      Term.of(vbox.bounds.w)
    when Term.of(:"?visual-h")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?visual-h: _}
      #
      # |@block
      # Replaces the value of `?visual-h` with the *visual* height of the node. Visual
      # sizes are affected by overflow and transforms.
      Term.of(vbox.bounds.h)
    when Term.of(:"?aim-x")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))
      return unless rect = clip.aim_rect

      Term.of(rect.x)
    when Term.of(:"?aim-y")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))
      return unless rect = clip.aim_rect

      Term.of(rect.y)
    when Term.of(:"?aim-offset-x")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))

      Term.of(clip.aim.x)
    when Term.of(:"?aim-offset-y")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))

      Term.of(clip.aim.y)
    when Term.of(:"?navs")
      Term.of(Textoscope.navs(node, box))
    when Term.of(:"?nav")
      navs = Textoscope.navs(node, box)
      navs.single?
    end
  end

  # Leaves
  private def answers(
    node : Scenery::Inert | Scenery::RectShape | Scenery::HitArea | Scenery::Pending | Scenery::Img | Scenery::Svg | Scenery::IconGlyph | Scenery::ShapedText,
    box : Scenery::Box,
    vbox : Scenery::VBox,
  ) : Slice(Answer)
    Slice(Answer).empty
  end

  private def answers(node : Scenery::ZStack, box : Scenery::Box, vbox : Scenery::VBox) : Slice(Answer)
    unless info = node.info
      return answers(node.children, box, vbox)
    end

    if info.pairs.empty? # Fast path
      return answers(node.children, box, vbox)
    end

    answers = Pf::Kit.stack_array(Answer)

    info.pairs.each_entry do |key, value|
      next unless keysym = key.as_sym?
      next unless keysym.prefixed_by?('?')

      # Remember that we store the address in values of ?k: v_ pairs. Basically for
      # any visual node that made it all the way here, that still has a ?-key, we
      # know the value is an address; an analysis of `visualize` would show that.
      # (Or... umm... I hope it would.)
      next unless value = value.as_itemsonly_d?
      next unless addr = D7::NodeAddr.new?(value)

      answer = reply?(node, box, vbox, key) || Term.of(:_)
      answers << Answer.new(addr, key, answer)
    end

    # Inline answers(node.children, box) to reuse the existing `answers` stack array.
    node.children.zip(box.children, vbox.children) do |child, child_box, child_vbox|
      child_box = child_box.translate(box.bounds.tl)
      child_vbox = Scenery::VBox.new(child_vbox.bounds.translate(vbox.bounds.tl), child_vbox.children)
      answers.concat(answers(child, child_box, child_vbox))
    end

    answers.to_unsafe_readonly_slice!
  end

  # Parents
  private def answers(
    node : Scenery::Vantage | Scenery::Padding | Scenery::Align | Scenery::XYStack | Scenery::Composite | Scenery::TransformMatrix | Scenery::Clip | Scenery::Gate,
    box : Scenery::Box,
    vbox : Scenery::VBox,
  ) : Slice(Answer)
    answers(node.children, box, vbox)
  end

  private def answers(nodes : Slice(Scenery::AimedNode), box : Scenery::Box, vbox : Scenery::VBox) : Slice(Answer)
    answers = Pf::Kit.stack_array(Answer)

    nodes.zip(box.children, vbox.children) do |node, node_box, node_vbox|
      node_box = node_box.translate(box.bounds.tl)
      node_vbox = Scenery::VBox.new(node_vbox.bounds.translate(vbox.bounds.tl), node_vbox.children)
      answers.concat(answers(node, node_box, node_vbox))
    end

    answers.to_unsafe_readonly_slice!
  end

  alias AnswerMap = Hash({D7::NodeAddr, Term}, Term)

  def answer_map(scene : Scenery::Scene) : AnswerMap
    # Convert OriginBox -> Box.
    box = scene.box.translate(Scenery::Point[0, 0])
    answers = answers(scene.root.node, box, scene.vbox)
    answers.to_h { |answer| { {answer.addr, answer.key}, answer.value } }
  end
end
