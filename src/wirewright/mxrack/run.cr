module Ww::MxRack
  # ... | AudioGraph | ... (?)
  alias Projection = Window | Automaton | ThemeMixin

  defcase Automaton,
    addr : D7::NodeAddr,
    key : Term,
    clf : D7::Classifier,
    workspace : Term::Dict,
    tree : D7::GroupNode,
    draft : Term

  # :nodoc:
  def automata(tree : D7::InertLeaf | D7::GndLeaf) : Slice(Automaton)
    Slice(Automaton).empty
  end

  # :nodoc:
  def automata(tree : D7::MixtureNode | D7::ScopeNode) : Slice(Automaton)
    automata(tree.child)
  end

  # :nodoc:
  def automata(tree : D7::CircuitNode) : Slice(Automaton)
    automata(tree.to_group)
  end

  # :nodoc:
  SYM_AUTOMATON = Term.of(:automaton)

  # :nodoc:
  def automata(tree : D7::GroupNode) : Slice(Automaton)
    automata = Pf::Kit.stack_array(Automaton, 4)

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

        group = D7::GroupNode.new(D7.parent(draft.as_d), tree.children)
        automata << Automaton.new(D7::NodeAddr.empty, key, clf, workspace, group, draft)
      end

      otherwise { }
    end

    if tree.summary.has_head?(SYM_AUTOMATON)
      tree.children.zip(tree.feature.range) do |child, key|
        addr = D7::NodeAddr[key]

        automata(child).each do |automaton|
          automata << automaton.copy_with(addr: D7::NodeAddr.concat(addr, automaton.addr))
        end
      end
    end

    automata.to_unsafe_readonly_slice!
  end

  {% if flag?(:docs) %}
    # Returns automata projections of the given circuit *tree*.
    def automata(tree : D7::ParseTree) : Slice(Automaton)
    end
  {% end %}

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

  # Returns multimedia and interactivity nodes in *circuit*.
  def projections(microfold : Microfold::SyncCodex, circuit : D7::ParseTree, *, interactive : Bool = true) : Array(Projection)
    projections = [] of Projection

    hg = D7::Hypergraph.new(circuit)
    Rack::Prepass.call(hg) do |hg| # shadow
      projections = projections(microfold, circuit, hg, interactive)
    end

    projections
  end

  private def projections(microfold : Microfold::SyncCodex, circuit : D7::ParseTree, hg : D7::Hypergraph, interactive : Bool) : Array(Projection)
    components = visual_component_rules(circuit)

    # Walk the circuit and find all [window _*]s. We need to know their parse trees,
    # addresses, and keys.
    window_trees_and_keys = {} of D7::NodeAddr => {D7::GroupNode, Term}

    window_nodes = window_nodes(circuit)
    window_nodes.each do |window_node|
      window_key = window_node.tree.feature.node[:key]? || Term.of(:main)
      assert window_trees_and_keys.put?(window_node.addr, {window_node.tree, window_key})
    end

    # Colliding keys must form a blacklist.
    window_keys = Set(Term).new
    window_key_blacklist = Set(Term).new

    window_trees_and_keys.each_value do |(_, window_key)|
      next if window_key.in?(window_key_blacklist)
      next if window_keys.add?(window_key)

      window_keys.delete(window_key)
      window_key_blacklist << window_key
    end

    projections = [] of Projection

    window_trees_and_keys.each do |window_addr, (window_tree, window_key)|
      window_node = WindowNode.new(window_addr, window_tree)

      if window_key.in?(window_key_blacklist)
        # Key collisions, as in:
        #
        #   (window key: a)
        #   (window key: a)
        #
        # ... are resolved by using the node address. Although not ideal, it does
        # cover many "tricky" changes such as creation of deeper windows and so on,
        # without losing identity. Only if you move the window itself or any of its
        # parents would the identity change, which is arguably as good as we can get
        # without assigning windows random unique ids. (TODO: We should probably do that
        # as an opt-in, though, e.g. `key: random` or something like that which should
        # rewrite to key: "UUID" or something like that before we even have a chance to
        # look at the window).
        window_key = Term.of(window_node.addr)
      end

      visuals, questions = visualize(microfold, components, hg, window_node, interactive)

      # It always returns Outcome::Accepted though so no worries about the unwrap.
      # By unwrapping we just discard the diagnostics here (for now).
      scenery = Microfold.render(microfold, visuals).unwrap

      next unless window_spec = window_spec?(scenery)

      window = Window.new(window_key, window_node.addr,
        scenery,
        interactive ? inputs(window_node) : Slice(Input).empty,
        interactive ? mice(window_node) : Slice(Mouse).empty,
        interactive ? mouseareas(window_node) : Slice(MouseArea).empty,
        questions,
        window_spec,
      )

      projections << window
    end

    projections.concat(automata(circuit))
    projections.concat(theme_mixins(circuit))

    projections
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

  private def focus_exchange(projections : Array(Projection)) : {Array(Projection), Slice(Adjustment)}
    adjustments = Pf::Kit.stack_array(Adjustment)
    assignments = Pf::Kit.stack_array({Int32, Int32, InputFocus}, 4)

    projections.each_with_index do |projection, projection_index|
      next unless projection.is_a?(Window)

      wanted = false
      seized = false
      projection.inputs.each do |input|
        if input.focus.seized?
          seized = true
          break
        end
        if input.focus.wanted?
          wanted = true
        end
      end

      if seized
        projection.inputs.each_with_index do |input, input_index|
          next unless input.focus.active?

          adjustments << AdjustInputBlur.new(input.addr)
          assignments << {projection_index, input_index, InputFocus::Inactive}
        end
      end

      next unless wanted

      projection.inputs.each_with_index do |input, input_index|
        if input.focus.active?
          adjustments << AdjustInputBlur.new(input.addr)
          assignments << {projection_index, input_index, InputFocus::Inactive}
          next
        end

        if input.focus.wanted?
          adjustments << AdjustInputFocus.new(input.addr)
          assignments << {projection_index, input_index, InputFocus::Active}
          next
        end
      end
    end

    if assignments.empty?
      return projections, adjustments.to_unsafe_readonly_slice! # Unchanged
    end

    projections = projections.map_with_index do |projection, projection_index|
      unless projection.is_a?(Window)
        next projection
      end

      unless assignments.any? { |(candidate_projection_index, _, _)| candidate_projection_index == projection_index }
        next projection
      end

      inputs = projection.inputs.map_with_index do |input, input_index|
        assignments.each do |(candidate_projection_index, candidate_input_index, focus)|
          next unless candidate_projection_index == projection_index
          next unless candidate_input_index == input_index

          input = input.copy_with(focus: focus)
        end

        input
      end

      projection.copy_with(inputs: inputs)
    end

    {projections, adjustments.to_unsafe_readonly_slice!}
  end

  private def hitarea_leaf?(hit_node : Scenery::HitNode) : Scenery::HitAreaLeaf?
    case hit_node
    in Scenery::HitEmpty
    in Scenery::HitTextLeaf
    in Scenery::HitAreaLeaf
      hit_node
    in Scenery::HitLeaf
    in Scenery::HitGroup
      # Front to back.
      hit_node.children.reverse_each do |child|
        return hitarea_leaf?(child) || next
      end
    end
  end

  # Returns the MouseArea of *mouseareas* hit in *hit tree*, if any. Returns
  # `nil` otherwise.
  private def hit_mousearea?(mouseareas : Slice(MouseArea), hit_tree : Scenery::HitNode) : {MouseArea, Scenery::Point}?
    return unless leaf = hitarea_leaf?(hit_tree)
    return unless addrQ = leaf.payload.as_d?
    return unless addr = D7::NodeAddr.new?(addrQ)
    return unless mousearea = mouseareas.find { |candidate| candidate.addr == addr }

    {mousearea, leaf.part.tl}
  end

  private def hit(window : Window, session : WindowSession) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    # If there is no scene this means the user cannot see anything yet, so
    # calculating hits makes no sense.
    unless scene = session.scene?
      return adjustments.to_unsafe_readonly_slice!
    end

    session.each_mouse_position do |mouse_id, mouse_point|
      hit_query = Scenery::HitQuery.new(mouse_point, mouse_point, :single)
      hit_tree = Scenery.hit(scene, hit_query)

      text_aims = Textoscope.text_aims(scene.root.node, scene.box.translate(Scenery::Point[0, 0]), hit_tree)
      text_aims.each do |text_aim|
        next unless text_aim.is_a?(Textoscope::QualifiedTextAim)

        adjustments << AdjustQuestion.new(text_aim.addr, Term.of(:"?aim"), text_aim.description)
      end

      hit_row = hit_mousearea?(window.mouseareas, hit_tree)

      mouse_captured = false

      if session.mouse_pressed?(mouse_id)
        window.mouseareas.each do |mousearea|
          next unless mousearea.flags.wants_position_always? || (mousearea.flags.wants_position_drag? && session.mouse_pressed?(mouse_id))
          next unless mouse = mousearea.mice.find { |candidate| candidate.id == mouse_id }

          if hit_row && hit_row[0].addr == mousearea.addr
            hit_point = hit_row[1]
          end

          hit_abs = Scenery::Point[0, 0]
          if row = Scenery.hitarea?(scene.root.node, scene.box.translate(Scenery::Point[0, 0]), Term.of(mousearea.addr))
            _, hitbox = row
            hit_abs = mouse_point - hitbox.bounds.tl
          end

          adjustments << AdjustMouseareaPosition.new(mousearea.addr, mouse.key, hit_abs, hit_point)

          mouse_captured = true
        end
      end

      next if mouse_captured

      # The mouse must join the mousearea it hit.
      pass do
        next if hit_row.nil?

        hit_mousearea, hit_point = hit_row

        mouse = hit_mousearea.mice.find { |candidate| candidate.id == mouse_id }
        if mouse.nil?
          # A new mouse entered the mousearea.
          mouse = session.mouse_model(mouse_id)
          adjustments << AdjustMouseareaShowMouse.new(hit_mousearea.addr, mouse)
          next
        end

        next unless hit_mousearea.flags.wants_position_always? || (hit_mousearea.flags.wants_position_drag? && session.mouse_pressed?(mouse_id))

        adjustments << AdjustMouseareaDelta.new(hit_mousearea.addr, mouse.key, hit_point)
      end

      # The mouse must leave all mouse areas it did not hit.
      window.mouseareas.each do |mousearea|
        next if hit_row && mousearea.addr == hit_row[0].addr
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

  def transcribe(projection : Window) : Term
    Term.morph(projection.scenery,
      {:title, projection.spec.title},
      {:width, projection.spec.width},
      {:height, projection.spec.height},
      {:cursor, transcribe(projection.spec.cursor)},
      {:backdrop, projection.spec.backdrop},
      {:form, transcribe(projection.spec.form)},
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

  ROOT_AUTOMATON_KEY = Term.of(:root)

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
    projections = [] of Projection

    prev_displayed = Term.of

    epoch = 0u64
    loop do
      version0 = circuit.version

      # Maintain the Microfold codex up-to-date as a composition of various "ingredients":
      # some of them coming from the base theme in codices/, some from the MxRack codex,
      # and others yet, live, from the circuit itself.
      microfold_circuit_theme = theme_mix(projections.each.select(ThemeMixin).map(&.mixin))

      microfold_mixed_theme_candidate = {
        microfold_base_theme,
        microfold_mxrack_theme,
        microfold_circuit_theme,
      }
      if microfold.nil? || microfold_mixed_theme != microfold_mixed_theme_candidate
        microfold = Microfold.codex(theme_mix(microfold_mixed_theme_candidate)).unwrap
        microfold_mixed_theme = microfold_mixed_theme_candidate
      end

      # Handle events.
      case event = eventq.shift?
      in Nil
      in ResourceChanged, MousePlugged, MouseUnplugged
        wm.broadcast(event)
      in WindowReceived
        pass do
          next unless key = wm.key?(event.session_id)

          window = projections.leftmost? do |projection|
            next unless projection.is_a?(Window)
            next unless projection.key == key

            projection
          end

          next if window.nil?
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

        projections.each do |projection|
          next unless projection.is_a?(Window)
          next unless session = wm.join?(projection.key)
          next unless scene = session.scene?

          # Update questions ?xyz: _.
          if projection.questions.present?
            answers = answer_map(scene)

            projection.questions.each do |question|
              answer = answers[{question.addr, question.key}]?
              adjustments = adjustments.append(AdjustQuestion.new(question.addr, question.key, answer))
            end
          end

          # Perform hit testing.
          #
          # NOTE: This must go after answer_map() because answer_map() will reset ?aim,
          # which we later update in hit() for all hit texts. This way, all *un*hit
          # texts are reset, and all hit texts are updated!
          adjustments += hit(projection, session)
        end

        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end
      end

      # Step the circuit and sub-automata.
      # frame, projections = step(D7::NodeAddr.empty, circuit, library, microfold, parsers, circuits, alert, paused: false)

      # Step the circuit.
      frame = pass do
        # Run physics (in our case of course symbolic physics!)
        step_out = circuit.step(Rack::Prepass, library)

        # Get all projections of the circuit.
        projections = projections(microfold, step_out)

        # Make sure deepest automata go last (I mean, the order doesn't matter, but
        # the fact everything's sorted does).
        automata = projections.select(Automaton).sort_by!(&.addr.size)

        au_projections = {} of Automaton => Array(Projection)
        au_projections.compare_by_identity

        projections.select! do |projection|
          unless projection.is_a?(Window)
            next true # keep
          end

          automaton = automata.reverse_each.find do |automaton|
            next unless projection.addr.size > automaton.addr.size
            next unless projection.addr.starts_with?(automaton.addr)

            true
          end

          if automaton.nil?
            next true # keep
          end

          bucket = au_projections.put_if_absent(automaton) { [] of Projection }
          bucket << projection

          false # reject
        end

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

            # FIXME: it should be preview() or something like that, no hitareas and such
            window_transcripts = projections(microfold, present_tree, interactive: false)
              .select(Window)
              .map { |window| transcribe(window) }

            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:repr), repr(present_tree))
            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:windows), Term.of(window_transcripts))
          else
            window_transcripts = (au_projections[automaton]? || [] of Projection).each
              .select(Window)
              .map { |window| transcribe(window) }

            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:repr), repr(automaton.tree))
            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:windows), Term.of(window_transcripts))
          end
        end

        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end

        # Perform focus exchange if possible -- so that `focus: wanted` becomes `focus: true`,
        # and previous foci are released.
        projections, adjustments = focus_exchange(projections)

        # Apply adjustments, if any.
        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end

        circuit.draft
      end

      # Adjustments below must not change node addresses or rewrite nodes in any way.
      # They may only annotate or update existing nodes.

      # Present window projections.
      projections.each do |projection|
        next unless projection.is_a?(Window)

        session = wm.connect(projection.key)
        session.submit(projection.spec, projection.scenery)
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

      # Then there is this final section of the loop, which decides how and whether
      # to wait, just as in a classical game loop.

      next unless version0 == circuit.version
      # Circuit not changed.

      next unless eventq.empty?
      # No pending events. NOTE: This doesn't appear to be racey. Is it?

      break if wm.empty? && !circuit.pending?
      # Some windows or pending work.

      # Wait.
      if deadline = circuit.deadline?
        epoch = alert.wait_until(epoch, timeout: deadline - Time.instant)
      else
        epoch = alert.wait(epoch)
      end
    end

    # TODO: deinit
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
