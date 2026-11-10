module Ww::MxRack
  defcase Automaton,
    addr : D7::NodeAddr,
    key : Term,
    clf : D7::Classifier,
    workspace : Term::Dict,
    tree : D7::GroupNode,
    draft : Term

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::InertLeaf | D7::GndLeaf) : Nil
  end

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::MixtureNode | D7::ScopeNode) : Nil
    automata(sink, addr, scope, tree.child)
  end

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::CircuitNode) : Nil
    automata(sink, addr, scope, tree.to_group)
  end

  # :nodoc:
  SYM_AUTOMATON = Term.of(:automaton)

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::GroupNode) : Nil
    return unless tree.summary.has_head?(SYM_AUTOMATON)

    Term.case(tree.feature.node) do
      matchpiT %{[automaton ((clfQ_ key_) workspace_dict) draft_*]} do
        case clfQ
        when Term.of(:rack)
          clf = Rack.clf
        when Term.of(:mxrack)
          clf = MxRack.clf
        when Term.of(:musoma)
          # clf = MuSoma.clf
          raise ""
        else
          continue
        end

        # Automata are scoped to make sure you can run the same circuit recursively
        # in an automaton. Otherwise we'd get key collisions which would mess
        # everything up.
        subscope = scope.append(key)

        group = D7::GroupNode.new(D7.parent(draft.as_d), tree.children)
        sink << Automaton.new(addr, Term.of(subscope), clf, workspace, group, draft)

        tree.children.zip(tree.feature.range) do |child, key|
          automata(sink, addr.append(key), subscope, child)
        end
      end

      otherwise do
        tree.children.zip(tree.feature.range) do |child, key|
          automata(sink, addr.append(key), scope, child)
        end
      end
    end
  end

  # Returns automata projections of the given circuit *tree*. Automata are ordered
  # by depth: *deepest ones go first*.
  def automata(tree : D7::ParseTree) : Slice(Automaton)
    sink = Pf::Kit.stack_array(Automaton, 8)
    addr = D7::NodeAddr.empty
    scope = Slice(Term).empty
    automata(sink, addr, scope, tree)

    sink.sort_by! { |automaton| -automaton.addr.size }
    sink.to_unsafe_readonly_slice!
  end

  defrecord ThemeMixin, mixin : Term::Dict

  # Recognizes a theme mixin definition *defn*.
  def theme_mixin(defn : Term) : ThemeMixin
    mixin = Term[]

    unless defn = defn.as_d?
      return ThemeMixin.new(mixin)
    end

    vars = Term::Dict.build do |commit|
      defn.each_entry(in: Term::Dict.pairspart) do |key, value|
        Term.matchpi?(key, %{(var name_)}) do
          commit.with(name, Nitrene.eval(Term[], value))
        end
      end
    end

    defn.each_entry(in: Term::Dict.pairspart) do |section, extension|
      Term.matchpi?(section, %{(table _)}) do
        next unless contribution = Alloy.render(extension, locals: vars).as_d?

        contribution.each_entry do |key, value|
          mixin = Term.morph(mixin, {section, key, value})
        end
      end
    end

    ThemeMixin.new(mixin)
  end

  def theme_mix(mixins : Enumerable(Term::Dict)) : Term::Dict
    mix = Term[]

    mixins.each do |mixin|
      mixin.each_entry do |section, contribution|
        contribution.each_entry do |key, value|
          mix = Term.morph(mix, {section, key, value})
        end
      end
    end

    mix
  end

  # :nodoc:
  def theme_mixins(tree : D7::InertLeaf) : Slice(ThemeMixin)
    Slice(ThemeMixin).empty
  end

  # :nodoc:
  def theme_mixins(tree : D7::GndLeaf) : Slice(ThemeMixin)
    node = tree.feature.node

    Term.case(node) do
      matchpi %{[theme]} do
        Slice[theme_mixin(node)]
      end

      otherwise do
        Slice(ThemeMixin).empty
      end
    end
  end

  # :nodoc:
  def theme_mixins(tree : D7::MixtureNode | D7::ScopeNode) : Slice(ThemeMixin)
    theme_mixins(tree.child)
  end

  # :nodoc:
  def theme_mixins(tree : D7::CircuitNode) : Slice(ThemeMixin)
    theme_mixins(tree.to_group)
  end

  # :nodoc:
  SYM_THEME = Term.of(:theme)

  # :nodoc:
  def theme_mixins(tree : D7::GroupNode) : Slice(ThemeMixin)
    unless tree.summary.has_head?(SYM_THEME)
      return Slice(ThemeMixin).empty
    end

    mixins = Pf::Kit.stack_array(ThemeMixin, 4)

    tree.children.zip(tree.feature.range) do |child, key|
      mixins.concat(theme_mixins(child))
    end

    mixins.to_unsafe_readonly_slice!
  end

  {% if flag?(:docs) %}
    # Returns the Microfold theme mixins in the given circuit's *tree*.
    def theme_mixins(tree : D7::ParseTree) : Slice(Automaton)
    end
  {% end %}

  private def visual_component_rules(tree : D7::ParseTree) : Slice({Term, Term})
    case tree
    in D7::InertLeaf
      Slice({Term, Term}).empty
    in D7::GndLeaf
      Term.case(tree.feature.node) do
        matchpi %{[rule pattern_ body_]} do
          Slice[{pattern, body}]
        end

        otherwise do
          Slice({Term, Term}).empty
        end
      end
    in D7::MixtureNode, D7::ScopeNode
      visual_component_rules(tree.child)
    in D7::ParentNode
      unless tree.summary.has_signature?(D7::NodeSignature.new(Term.of(:rule), 3))
        return Slice({Term, Term}).empty
      end

      rules = Pf::Kit.stack_array({Term, Term}, 8)
      tree.children.each do |child|
        rules.concat(visual_component_rules(child))
      end

      rules.to_unsafe_readonly_slice!
    end
  end

  def windows_with_automata(windows : Array(Window), circuit)
    automata = automata(circuit)

    au_windows = {} of Automaton => Array(Window)
    au_windows.compare_by_identity

    windows = windows.select do |projection|
      automaton = automata.find do |automaton|
        next unless projection.addr.size > automaton.addr.size
        next unless projection.addr.starts_with?(automaton.addr)

        true
      end

      if automaton.nil?
        next true # keep, doesn't belong to any automaton
      end

      bucket = au_windows.put_if_absent(automaton) { [] of Window }
      bucket << projection

      false # reject
    end

    {windows, automata, au_windows}
  end

  def repr(tree : D7::ParseTree) : Term
    case tree
    in D7::InertLeaf
      Term.of(:inert, tree.feature.node)
    in D7::GndLeaf
      Term.of(:gnd, Term[], tree.feature.node)
    in D7::MixtureNode
      Term.of(:inert, tree.feature.node)
    in D7::ScopeNode
      repr(tree.child)
    in D7::ParentNode
      parent = tree.feature

      repr = parent.node.pairspart.transaction do |commit|
        commit << :parent << Term[]

        parent.node.items.each_with_index do |item, key|
          key = key.to_u32

          unless key.in?(parent.range)
            commit << {:inert, item}
            next
          end

          index = key - parent.range.begin
          child = tree.children[index]
          child_repr = repr(child)

          # Prepend key to child addrs on unwind.
          Term.case(child_repr) do
            matchpi %{[gnd addr←(_*) _]}, %{[parent addr←(_*) _*]} do
              commit << Term.morph(child_repr, {1, addr.prepend(key)})
            end

            matchpi %{[inert _]} do
              commit << child_repr
            end
          end
        end
      end

      Term.of(repr)
    end
  end

  class Circuit
    # Contains the circuit to evolve in `step` to produce snapshots.
    getter draft : Term
    getter version : UInt64
    getter parser

    def initialize(@clf : D7::Classifier, @alert : BlockingSignal)
      @draft = Term.of
      @version = 0u64
      @parser = D7::Parser.new(@clf)
      @automaton = Rack::Automaton.new(@parser, alert: @alert, display_mask: :subframe)
    end

    def draft=(draft : Term) : Term
      if @draft == draft
        return draft # Unchanged
      end

      @version += 1
      @draft = draft
    end

    # Returns `true` if there are more snapshots to read.
    def pending? : Bool
      @automaton.pending?
    end

    # Returns the nearest instant at which `step` should be called. Returns `nil`
    # if no such instant is set.
    def deadline? : Time::Instant?
      @automaton.deadline?
    end

    def step(prepass, library) : D7::ParseTree
      subframes = Pf::Kit.stack_array(Term)
      frames = Pf::Kit.stack_array(Term)
      @automaton.step(subframes, frames, @draft, prepass, library)

      subframes.each do |subframe|
        next if @draft == subframe

        self.draft = subframe
      end

      @parser.parse(@draft)
    end
  end

  private def focus_exchange(windows : Array(Window)) : {Array(Window), Slice(Adjustment)}
    adjustments = Pf::Kit.stack_array(Adjustment)
    assignments = Pf::Kit.stack_array({Int32, Int32, InputFocus}, 4)

    windows.each_with_index do |window, window_index|
      wanted = false
      seized = false
      window.inputs.each do |input|
        if input.focus.seized?
          seized = true
          break
        end
        if input.focus.wanted?
          wanted = true
        end
      end

      if seized
        window.inputs.each_with_index do |input, input_index|
          next unless input.focus.active?

          adjustments << AdjustInputBlur.new(input.addr)
          assignments << {window_index, input_index, InputFocus::Inactive}
        end
      end

      next unless wanted

      window.inputs.each_with_index do |input, input_index|
        if input.focus.active?
          adjustments << AdjustInputBlur.new(input.addr)
          assignments << {window_index, input_index, InputFocus::Inactive}
          next
        end

        if input.focus.wanted?
          adjustments << AdjustInputFocus.new(input.addr)
          assignments << {window_index, input_index, InputFocus::Active}
          next
        end
      end
    end

    if assignments.empty?
      return windows, adjustments.to_unsafe_readonly_slice! # Unchanged
    end

    windows = windows.map_with_index do |window, window_index|
      unless assignments.any? { |(candidate_window_index, _, _)| candidate_window_index == window_index }
        next window
      end

      inputs = window.inputs.map_with_index do |input, input_index|
        assignments.each do |(candidate_window_index, candidate_input_index, focus)|
          next unless candidate_window_index == window_index
          next unless candidate_input_index == input_index

          input = input.copy_with(focus: focus)
        end

        input
      end

      window.copy_with(inputs: inputs)
    end

    {windows, adjustments.to_unsafe_readonly_slice!}
  end

  defrecord HitMouseArea, mousearea : MouseArea, point : Scenery::Point

  enum HitOpacity
    Transparent
    Opaque
  end

  private def hit_mouseareas(mouseareas : Slice(MouseArea), hit_node : Scenery::HitNode) : {Slice(HitMouseArea), HitOpacity}
    case hit_node
    in Scenery::HitEmpty
      {Slice(HitMouseArea).empty, HitOpacity::Transparent}
    in Scenery::HitTextLeaf, Scenery::HitLeaf
      {Slice(HitMouseArea).empty, HitOpacity::Opaque}
    in Scenery::HitAreaLeaf
      unless addrQ = hit_node.payload.as_d?
        return Slice(HitMouseArea).empty, HitOpacity::Transparent
      end

      unless addr = D7::NodeAddr.new?(addrQ)
        return Slice(HitMouseArea).empty, HitOpacity::Transparent
      end

      unless mousearea = mouseareas.find { |candidate| candidate.addr == addr }
        return Slice(HitMouseArea).empty, HitOpacity::Transparent
      end

      opacity = mousearea.flags.opaque? ? HitOpacity::Opaque : HitOpacity::Transparent
      {Slice[HitMouseArea.new(mousearea, hit_node.part.tl)], opacity}
    in Scenery::HitGroup
      staging = Pf::Kit.stack_array(HitMouseArea)

      hit_node.children.reverse_each do |child|
        hits, opacity = hit_mouseareas(mouseareas, child)
        staging.concat(hits)

        case opacity
        in .transparent?
        in .opaque?
          return staging.to_unsafe_readonly_slice!, HitOpacity::Opaque
        end
      end

      {staging.to_unsafe_readonly_slice!, HitOpacity::Transparent}
    end
  end

  private def hit(window : Window, session : WindowSession) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    # If there is no scene this means the user cannot see anything yet, so
    # calculating hits makes no sense.
    unless scene = session.scene?
      return adjustments.to_unsafe_readonly_slice!
    end

    session.each_mouse_position do |mouse_id, mouse_point|
      # NOTE: We use the single-click query to get all nodes that were hit. Using
      # point query here would only get the topmost one. But we want to handle
      # occlusion ourselves to support `opacity: _` in mouseareas!
      hit_query = Scenery::HitQuery.new(mouse_point, mouse_point, :single)
      hit_tree = Scenery.hit(scene, hit_query)

      text_aims = Textoscope.text_aims(scene.root.node, scene.box.translate(Scenery::Point[0, 0]), hit_tree)
      text_aims.each do |text_aim|
        next unless text_aim.is_a?(Textoscope::QualifiedTextAim)

        adjustments << AdjustQuestion.new(text_aim.addr, Term.of(:"?aim"), text_aim.description)
      end

      hit_mouseareas, _ = hit_mouseareas(window.mouseareas, hit_tree)

      mouse_captured = false

      if session.mouse_pressed?(mouse_id)
        window.mouseareas.each do |mousearea|
          next unless mousearea.flags.wants_position_always? || (mousearea.flags.wants_position_drag? && session.mouse_pressed?(mouse_id))
          next unless mouse = mousearea.mice.find { |candidate| candidate.id == mouse_id }

          # Back-to-front.
          hit_row = hit_mouseareas.last?
          if hit_row && hit_row.mousearea.addr == mousearea.addr
            hit_point = hit_row.point
          end

          hit_abs = Scenery::Point[0, 0]
          if row = Scenery.hitarea?(scene.root.node, scene.box.translate(Scenery::Point[0, 0]), Term.of(mousearea.addr))
            _, hit_box = row
            hit_abs = mouse_point - hit_box.bounds.tl
          end

          adjustments << AdjustMouseareaPosition.new(mousearea.addr, mouse.key, hit_abs, hit_point)

          mouse_captured = true
        end
      end

      next if mouse_captured

      # The mouse must join mouseareas it hit.
      hit_mouseareas.each do |hit|
        mouse = hit.mousearea.mice.find { |candidate| candidate.id == mouse_id }
        if mouse.nil?
          # A new mouse entered the mousearea.
          mouse = session.mouse_model(mouse_id)
          adjustments << AdjustMouseareaShowMouse.new(hit.mousearea.addr, mouse)
          next
        end

        next unless hit.mousearea.flags.wants_position_always? || (hit.mousearea.flags.wants_position_drag? && session.mouse_pressed?(mouse_id))

        adjustments << AdjustMouseareaDelta.new(hit.mousearea.addr, mouse.key, hit.point)
      end

      # The mouse must leave all mouseareas it did not hit.
      window.mouseareas.each do |mousearea|
        next if hit_mouseareas.any? { |hit| mousearea.addr == hit.mousearea.addr }
        next unless mouse = mousearea.mice.find { |candidate| candidate.id == mouse_id }

        adjustments << AdjustMouseareaHideMouse.new(mousearea.addr, mouse.key)
      end
    end

    adjustments.to_unsafe_readonly_slice!
  end

  private def process(window : Window, message : WindowSizeChanged) : Slice(Adjustment)
    unless window.spec.flags.wants_size?
      return Slice(Adjustment).empty
    end

    Slice(Adjustment).of(AdjustWindowSize.new(window.addr, message.width, message.height))
  end

  private def process(window : Window, message : WindowFocusChanged) : Slice(Adjustment)
    unless window.spec.flags.wants_focus?
      return Slice(Adjustment).empty
    end

    Slice(Adjustment).of(AdjustWindowFocus.new(window.addr, message.focus))
  end

  private def process(window : Window, message : WindowHoverChanged) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    if window.spec.flags.wants_hover?
      adjustments << AdjustWindowHover.new(window.addr, message.hover)
    end

    if message.hover
      return adjustments.to_unsafe_readonly_slice!
    end

    # Window was UNhovered

    window.mouseareas.each do |mousearea|
      next if mousearea.mice.empty?

      # When the OS tells us that the mouse has left the window, this necessarily
      # means there are no mice over the window, nor are there any mice "captured"
      # by the window, so there's no drag either. Therefore it is safe for us to
      # clear all mouseareas.
      adjustments << AdjustMouseareaClear.new(mousearea.addr)
    end

    adjustments.to_unsafe_readonly_slice!
  end

  private def process(window : Window, message : WindowFormChanged) : Slice(Adjustment)
    unless window.spec.flags.wants_form?
      return Slice(Adjustment).empty
    end

    Slice(Adjustment).of(AdjustWindowForm.new(window.addr, message.form))
  end

  private def process(window : Window, message : MouseMoved) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment, 4)

    window.mice.each do |mouse|
      next unless mouse.id?.in?(nil, message.mouse_id)
      next unless mouse.flags.wants_position_always?

      adjustments << AdjustMousePosition.new(mouse.addr, Scenery::Point[message.x, message.y])
    end

    adjustments.to_unsafe_readonly_slice!
  end

  private def process(window : Window, message : MouseDragged) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    window.mice.each do |mouse|
      next unless mouse.id?.in?(nil, message.mouse_id)
      next unless mouse.flags.wants_position_drag?

      adjustments << AdjustMousePosition.new(mouse.addr, Scenery::Point[message.x, message.y])
    end

    adjustments.to_unsafe_readonly_slice!
  end

  private def process(window : Window, message : MouseButtonPressed) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    window.mice.each do |mouse|
      next unless mouse.id?.in?(nil, message.mouse_id)

      adjustments << AdjustMousePress.new(mouse.addr, message.button, message.clicks)
    end

    window.mouseareas.each do |mousearea|
      next unless mouse = mousearea.mice.find { |candidate| candidate.id == message.mouse_id }

      adjustments << AdjustMouseareaPress.new(mousearea.addr, mouse.key, message.button, message.clicks)
    end

    adjustments.to_unsafe_readonly_slice!
  end

  private def process(window : Window, message : MouseButtonReleased) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    window.mice.each do |mouse|
      next unless mouse.id?.in?(nil, message.mouse_id)

      adjustments << AdjustMouseRelease.new(mouse.addr, message.button)
    end

    window.mouseareas.each do |mousearea|
      next unless mouse = mousearea.mice.find { |candidate| candidate.id == message.mouse_id }

      reset_delta = mousearea.flags.wants_position_drag?
      adjustments << AdjustMouseareaRelease.new(mousearea.addr, mouse.key, message.button, reset_delta)
    end

    adjustments.to_unsafe_readonly_slice!
  end

  private def process(window : Window, message : KeyboardMessage) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    # When an input is unfocused (donor input), it does not make sense to
    # send it Tab or S-Tab or other focus control key; nor does it make
    # sense to send the key to the focus recipient. So we remember both focus
    # donors and recipients, and skip them when we unify keys.
    inputs_focused_or_unfocused = Set(D7::NodeAddr).new

    pass do
      next unless message.is_a?(RunePressed)

      case message.rune
      when Term.of(:key, :tab)
        next if window.inputs.any?(&.focus.seized?)

        if window.inputs.none?(&.focus.active?)
          # Focus first inactive input.
          window.inputs.each do |input|
            next unless input.focus.inactive?

            adjustments << AdjustInputFocus.new(input.addr)
            inputs_focused_or_unfocused << input.addr
            break
          end
        else
          # Focus next input.
          focusable = window.inputs.select { |input| input.focus.active? || input.focus.inactive? }
          focusable.each_with_index do |input, input_index|
            if input.focus.active?
              # On Tab, active inputs make themselves inactive and clear themselves
              # (becoming focus donors).
              adjustments << AdjustInputBlur.new(input.addr)
              inputs_focused_or_unfocused << input.addr
            elsif input.focus.inactive?
              prev_input = focusable[(input_index - 1) % focusable.size]
              next unless prev_input.focus.active?

              # On Tab, inactive inputs preceded by (with wraparound) active inputs
              # make themselves active (becoming focus recipients).
              adjustments << AdjustInputFocus.new(input.addr)
              inputs_focused_or_unfocused << input.addr
            end
          end
        end
      when Term.of(:key, :tab, shift: true)
        next if window.inputs.any?(&.focus.seized?)

        if window.inputs.none?(&.focus.active?)
          # Focus last inactive input.
          (0...window.inputs.size).reverse_each do |input_index|
            input = window.inputs[input_index]
            next unless input.focus.inactive?

            adjustments << AdjustInputFocus.new(input.addr)
            inputs_focused_or_unfocused << input.addr
            break
          end
        else
          # Focus previous input.
          focusable = window.inputs.select { |input| input.focus.active? || input.focus.inactive? }
          focusable.each_with_index do |input, input_index|
            if input.focus.active?
              # On S-Tab, active inputs make themselves inactive and clear themselves
              # (becoming focus donors).
              adjustments << AdjustInputBlur.new(input.addr)
              inputs_focused_or_unfocused << input.addr
            elsif input.focus.inactive?
              next_input = focusable[(input_index + 1) % focusable.size]
              next unless next_input.focus.active?

              # On S-Tab, inactive inputs followed by (with wraparound) active inputs
              # make themselves active (becoming focus recipients).
              adjustments << AdjustInputFocus.new(input.addr)
              inputs_focused_or_unfocused << input.addr
            end
          end
        end
      when Term.of(:key, :escape)
        next unless window.inputs.none?(&.focus.seized?)

        window.inputs.each_with_index do |input, input_index|
          next unless input.focus.active?

          adjustments << AdjustInputBlur.new(input.addr)
          inputs_focused_or_unfocused << input.addr
        end
      end
    end

    # Unify message against active inputs.
    window.inputs.each do |input|
      next unless input.focus.should_receive_keys?
      next if input.addr.in?(inputs_focused_or_unfocused)

      case {input, message}
      when {KeyboardInput, KeyPressed}
        adjustments << AdjustInputPress.new(input.addr, message.key)
      when {KeyboardInput, KeyReleased}
        next unless input_key = input.keys.find { |candidate_key| candidate_key.repr == message.key }

        adjustments << AdjustInputRelease.new(input.addr, input_key.index)
      when {RuneInput, RunePressed}
        adjustments << AdjustInputPress.new(input.addr, message.rune)
      when {RuneInput, RuneReleased}
        next unless input_key = input.keys.find { |candidate_key| Term.of(candidate_key.repr.itemspart) == message.rune }

        adjustments << AdjustInputRelease.new(input.addr, input_key.index)
      end
    end

    adjustments.to_unsafe_readonly_slice!
  end

  def transcribe(window : Window) : Term
    Term.morph(window.scenery,
      {:key, window.key},
      {:title, window.spec.title},
      {:width, window.spec.width},
      {:height, window.spec.height},
      {:cursor, transcribe(window.spec.cursor)},
      {:backdrop, window.spec.backdrop},
      {:form, transcribe(window.spec.form)},
    )
  end

  def transcribe(form : WindowForm) : Term
    case form
    in .closed?    then Term.of(:closed)
    in .floating?  then Term.of(:floating)
    in .minimized? then Term.of(:minimized)
    in .maximized? then Term.of(:maximized)
    end
  end

  def transcribe(cursor : MouseCursor) : Term
    # If there are multiple aliases we'd just use whichever happens to be
    # the first one.
    term, _ = CURSOR_MAP.find! { |_, candidate| candidate == cursor }
    term
  end

  private def run(alert : BlockingSignal, eventq : EventQueue) : Nil
    filename = ARGV[0]
    quiet = "--quiet".in?(ARGV)

    wg = WaitGroup.new(3) # SDL and two adapters

    sdl = SdlLoop.start(wg, alert, eventq)

    spawn(name: "[MxRack] PathService adapter") do
      PathService.listen(wg) { eventq << ResourceChanged.new }
    end

    spawn(name: "[MxRack] HTTPService adapter") do
      HTTPService.listen(wg) { eventq << ResourceChanged.new }
    end

    # Wait until everything initializes.
    wg.wait

    wm = WindowManager.new(sdl)

    # Load codex and associated things.
    mxrack = ML.document(ResourceService.read_string(ResourceService.codex("mxrack")))
    library = Rack::Assembler.library(mxrack[:library])

    microfold_base_theme = ML.document(ResourceService.read_string(ResourceService.codex("ufold"))).as_d
    microfold_mxrack_theme = theme_mixin(mxrack[:theme]).mixin

    seed = ML.document(File.read(filename))

    microfold = nil
    microfold_mixed_theme = Term.of

    parsers = {} of D7::Classifier => D7::Parser
    circuits = {} of Term => Circuit

    # Proceed to the mainloop.
    circuit = Circuit.new(clf, alert)
    circuit.draft = seed

    prev_displayed = Term.of

    windowing_setup = nil
    windowing_cache = ViRecognized::RecognizeCache.new

    epoch = 0u64
    UInt64::MAX.times do |frameno|
      t0 = Time.instant
      version0 = circuit.version

      # Maintain the Microfold codex up-to-date as a composition of various "ingredients":
      # some of them coming from the base theme in codices/, some from the MxRack codex,
      # and others yet, live, from the circuit itself.
      microfold_circuit_theme = theme_mix(theme_mixins(circuit.parser.parse(circuit.draft)).map(&.mixin))

      microfold_mixed_theme_candidate = {
        microfold_base_theme,
        microfold_mxrack_theme,
        microfold_circuit_theme,
      }
      if microfold.nil? || microfold_mixed_theme != microfold_mixed_theme_candidate
        microfold = Microfold.codex(theme_mix(microfold_mixed_theme_candidate)).unwrap
        microfold_mixed_theme = microfold_mixed_theme_candidate
        if windowing_setup
          windowing_setup = WindowingSetup.new(windowing_cache, microfold.@codex, windowing_setup.epoch + 1)
        end
      end

      windowing_setup ||= WindowingSetup.new(windowing_cache, microfold.@codex, 0u64)

      windows = windows(windowing_setup, circuit.parser.parse(circuit.draft))

      # Handle events.
      case event = eventq.shift?
      in Nil
      in ResourceChanged, MousePlugged, MouseUnplugged
        wm.broadcast(event)
      in WindowReceived
        pass do
          next unless key = wm.key?(event.session_id)
          next unless window = windows.find { |window| window.key == key }
          next unless window_session = wm.join?(key)

          window_session.accept(event.message)

          adjustments = process(window, event.message)
          if adjustments.present?
            circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
          end
        end
      end

      # Hit test.
      pass do
        adjustments = Slice(Adjustment).empty

        windows.each do |window|
          next unless session = wm.join?(window.key)
          next unless scene = session.scene?

          # Update questions ?xyz: _.
          if window.questions.present?
            answers = answer_map(scene)

            window.questions.each do |question|
              answer = answers[{question.addr, question.key}]?
              adjustments = adjustments.append(AdjustQuestion.new(question.addr, question.key, answer))
            end
          end

          # Perform hit testing.
          #
          # NOTE: This must go after answer_map() because answer_map() will reset ?aim,
          # which we later update in hit() for all hit texts. This way, all *un*hit
          # texts are reset, and all hit texts are updated!
          adjustments += hit(window, session)
        end

        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end
      end

      # Step the circuit.
      frame = pass do
        # Run physics (in our case of course symbolic physics!)
        step_out = circuit.step(Rack::Prepass, library)

        # Get all projections of the circuit.
        # windows, automata, au_windows = windows_with_automata(alloy_cache, microfold, step_out)
        windows, automata, au_windows = windows_with_automata(windows(windowing_setup, step_out), step_out)

        adjustments = Pf::Kit.stack_array(Adjustment, 8)

        automata.reverse_each do |automaton|
          guest = circuits.put_if_absent(automaton.key) { Circuit.new(automaton.clf, alert) }

          pass do
            next if automaton.workspace.includes?(:turnover) # The circuit didn't handle turnover yet, don't overwhelm it

            guest.draft = automaton.draft

            # TODO: We want to submit subframes, not frames.
            _ = guest.step(Rack::Prepass, library)

            next if automaton.draft == guest.draft

            adjustments << AdjustAutomatonUpdateDraft.new(automaton.addr, guest.draft)
            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:turnover), Term.of(true))
          end

          if present = automaton.workspace[:present]?
            present_tree = guest.parser.parse(present)
            window_transcripts = windows(windowing_setup, present_tree, interactive: false).map { |window| transcribe(window) }

            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:repr), repr(present_tree))
            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:windows), Term.of(window_transcripts))
          else
            window_transcripts = (au_windows[automaton]? || [] of Window).map { |window| transcribe(window) }

            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:repr), repr(automaton.tree))
            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:windows), Term.of(window_transcripts))
          end
        end

        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end

        # Stepping automata can alter projections, so we have to recalculate them afterward.
        # windows, _, _ = windows_with_automata(alloy_cache, microfold, circuit.parser.parse(circuit.draft))
        draft_tree = circuit.parser.parse(circuit.draft)
        windows, _, _ = windows_with_automata(windows(windowing_setup, draft_tree), draft_tree)

        # Perform focus exchange if possible -- so that `focus: wanted` becomes `focus: true`,
        # and previous foci are released.
        windows, adjustments = focus_exchange(windows)

        # Apply adjustments, if any.
        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end

        circuit.draft
      end

      # Adjustments below must not change node addresses or rewrite nodes in any way.
      # They may only annotate or update existing nodes.

      # Present windows.
      windows.each do |window|
        session = wm.connect(window.key)
        session.submit(window.spec, window.scenery)
      end

      # DEBUG
      pass do
        next if quiet

        to_display = frame
        next if prev_displayed == to_display

        puts ML.display(to_display)
        prev_displayed = to_display
      end

      # This is a spot for garbage collection and similar kinds of cleanup.
      wm.gc
      windowing_cache.sweep

      t1 = Time.instant
      puts "Frame #{frameno}  #{(t1 - t0).total_milliseconds}ms"

      # Then there is this final section of the loop, which decides how and whether
      # to wait, just as in a classical game loop.

      next unless version0 == circuit.version
      # Circuit not changed.

      next unless eventq.empty?
      # No pending events. NOTE: This doesn't appear to be racey. Is it?

      break if wm.empty? && !pending?(circuit, circuits)
      # Some windows or pending work.

      # Wait.
      if deadline = deadline?(circuit, circuits)
        epoch = alert.wait_until(epoch, timeout: deadline - Time.instant)
      else
        epoch = alert.wait(epoch)
      end
    end

    # TODO: deinit
  end

  private def pending?(circuit : Circuit, automata : Hash(Term, Circuit)) : Bool
    circuit.pending? || automata.any? { |_, automaton| automaton.pending? }
  end

  # FIXME: ... if automaton not paused (turnover must be absent in workspace) ...
  private def deadline?(circuit : Circuit, automata : Hash(Term, Circuit)) : Time::Instant?
    left = circuit.deadline?
    automata.each do |_, automaton|
      right = automaton.deadline?
      left = left && right ? Math.min(left, right) : left || right
    end
    left
  end

  def run : Nil
    ctx = Fiber::ExecutionContext::Isolated.new("MxRack main thread", spawn_context: MT) do
      alert = BlockingSignal.new
      events = EventQueue.new(alert)
      run(alert, events)
    end
    ctx.wait
  end
end
