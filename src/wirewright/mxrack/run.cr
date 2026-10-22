module Ww::MxRack
  alias Projection = Window # | AudioGraph | ... (?)

  # Returns multimedia and interactivity nodes in *circuit*.
  def projections(microfold : Microfold::SyncCodex, circuit : D7::ParseTree) : Array(Projection)
    projections = [] of Projection

    hg = D7::Hypergraph.new(circuit)
    Rack::Prepass.call(hg) do |hg| # shadow
      projections = projections(microfold, circuit, hg)
    end

    projections
  end

  private def projections(microfold : Microfold::SyncCodex, circuit : D7::ParseTree, hg : D7::Hypergraph) : Array(Projection)
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

    windows = [] of Window

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
        window_key = Term.of(window_addr)
      end

      visuals, questions = visualize(microfold, hg, window_node)
      # It always returns Outcome::Accepted though so no worries about the unwrap.
      # By unwrapping we just discard the diagnostics here (for now).
      scenery = Microfold.render(microfold, visuals).unwrap

      next unless window_spec = window_spec?(scenery)

      window = Window.new(window_key, window_addr,
        scenery,
        inputs(window_node),
        mice(window_node),
        mouseareas(window_node),
        questions,
        window_spec,
      )

      windows << window
    end

    windows
  end

  private def present(ctx : PresentContext, projection : Window) : Nil
    session = ctx.wm.connect(projection.key)
    session.submit(projection.spec, projection.scenery)
  end

  defrecord PresentContext, wm : WindowManager

  def present(ctx : PresentContext, projections : Array(Projection)) : Nil
    projections.each do |projection1|
      present(ctx, projection1)
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

  private def process(window : Window, session : WindowSession, message : WindowSizeChanged) : Slice(Adjustment)
    unless window.spec.flags.wants_size?
      return Slice(Adjustment).empty
    end

    Slice(Adjustment).of(AdjustWindowSize.new(window.addr, message.width, message.height))
  end

  private def process(window : Window, session : WindowSession, message : WindowFocusChanged) : Slice(Adjustment)
    unless window.spec.flags.wants_focus?
      return Slice(Adjustment).empty
    end

    Slice(Adjustment).of(AdjustWindowFocus.new(window.addr, message.focus))
  end

  private def process(window : Window, session : WindowSession, message : WindowHoverChanged) : Slice(Adjustment)
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

  private def process(window : Window, session : WindowSession, message : WindowFormChanged) : Slice(Adjustment)
    unless window.spec.flags.wants_form?
      return Slice(Adjustment).empty
    end

    Slice(Adjustment).of(AdjustWindowForm.new(window.addr, message.form))
  end

  private def process(window : Window, session : WindowSession, message : MouseMoved) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    window.mice.each do |mouse|
      next unless mouse.id?.in?(nil, message.mouse_id)
      next unless mouse.flags.wants_position_always? || (mouse.flags.wants_position_on_press? && session.mouse_pressed?(message.mouse_id))

      adjustments << AdjustMousePosition.new(mouse.addr, Scenery::Point[message.x, message.y])
    end

    adjustments.to_unsafe_readonly_slice!
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
          next unless mousearea.flags.wants_position_always? || (mousearea.flags.wants_position_on_press? && session.mouse_pressed?(mouse_id))
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

        next unless hit_mousearea.flags.wants_position_always? || (hit_mousearea.flags.wants_position_on_press? && session.mouse_pressed?(mouse_id))

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

  private def process(window : Window, session : WindowSession, message : MouseButtonPressed) : Slice(Adjustment)
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

  private def process(window : Window, session : WindowSession, message : MouseButtonReleased) : Slice(Adjustment)
    adjustments = Pf::Kit.stack_array(Adjustment)

    window.mice.each do |mouse|
      next unless mouse.id?.in?(nil, message.mouse_id)

      adjustments << AdjustMouseRelease.new(mouse.addr, message.button)
    end

    window.mouseareas.each do |mousearea|
      next unless mouse = mousearea.mice.find { |candidate| candidate.id == message.mouse_id }

      reset_delta = mousearea.flags.wants_position_on_press?
      adjustments << AdjustMouseareaRelease.new(mousearea.addr, mouse.key, message.button, reset_delta)
    end

    adjustments.to_unsafe_readonly_slice!
  end

  private def process(window : Window, session : WindowSession, message : KeyboardMessage) : Slice(Adjustment)
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

    vars = Term[]
    mxrack[:theme].each_entry do |key, value|
      Term.matchpi?(key, %{(var name_)}) do
        vars = vars.with(name, Nitrene.eval(Term[], value))
      end
    end

    microfold_doc = ML.document(ResourceService.read_string(ResourceService.codex("ufold")))

    mxrack[:theme].each_entry do |section, extension|
      Term.matchpi?(section, %{(table _)}) do
        next unless mixin = Alloy.render(extension, locals: vars).as_d?

        mixin.each_entry do |key, value|
          microfold_doc = Term.morph(microfold_doc, {section, key, value})
        end
      end
    end

    # TODO: if seed is a document with section --- default, then:
    #   --- theme
    #   Can contain extensions to the theme

    microfold = Microfold.codex(microfold_doc).unwrap

    # Proceed to the mainloop.
    circuit = Circuit.new(clf, alert)
    circuit.draft = ML.document(File.read(filename))
    projections = [] of Projection

    prev_displayed = Term.of

    epoch = 0u64
    loop do
      version0 = circuit.version

      # Handle events.
      case event = eventq.shift?
      in Nil
      in ResourceChanged, MousePlugged, MouseUnplugged
        wm.broadcast(event)
      in WindowReceived
        pass do
          next unless key = wm.key?(event.session_id)
          next unless window = projections.find { |projection| projection.is_a?(Window) && projection.key == key }
          next unless window_session = wm.join?(key)

          window_session.accept(event.message)

          adjustments = process(window, window_session, event.message)
          if adjustments.present?
            circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
          end
        end
      end

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

      # Run physics (in our case of course symbolic physics!)
      frame = circuit.step(Rack::Prepass, library)

      # Derive and submit projections.
      pass do
        # Get all projections of the circuit.
        projections = projections(microfold, frame)

        # Perform focus exchange if possible, so that `focus: wanted` becomes `focus: true`
        # and previous foci are released. This must not change node addresses!
        projections, adjustments = focus_exchange(projections)
        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end

        # *Present* the projections. For graphical projections this means draw them. In the future,
        # for audio projections, it'd perhaps mean submit them to the DSP engine and so on.
        present(PresentContext.new(wm), projections)
      end

      # DEBUG
      pass do
        next if quiet

        to_display = frame.feature.node
        next if prev_displayed == to_display

        puts ML.display(to_display)
        prev_displayed = to_display
      end

      # This is a spot for garbage collection and similar kinds of cleanup.
      wm.gc

      # Finally there is the last section of the loop, which decides how and whether
      # to wait, just as in a classical game loop.
      next unless version0 == circuit.version
      next unless eventq.empty? # NOTE: This doesn't appear to be racey. Is it?

      break if wm.empty? && !circuit.pending?

      # Wait
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
