module Ww::MxRack
  class Circuit
    # Contains the circuit to evolve in `step` to produce snapshots.
    getter draft : Term
    getter version : UInt64
    getter parser
    getter repr_parser

    def initialize(@clf : D7::Classifier, @reprclf : D7::Classifier, @alert : BlockingSignal)
      @draft = Term.of
      @version = 0u64
      @parser = D7::Parser.new(@clf)
      @repr_parser = D7::Parser.new(@reprclf)
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

    def step(prepass, library, perf : IPerf) : Nil
      subframes = Pf::Kit.stack_array(Term)
      frames = Pf::Kit.stack_array(Term)
      @automaton.step(subframes, frames, @draft, prepass, library, perf)

      subframes.each do |subframe|
        next if @draft == subframe

        self.draft = subframe
      end
    end

    def sweep : Nil
      @parser.sweep
      @repr_parser.sweep
    end
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

  # Represents an `mxrack.argv` node.
  defrecord ArgvNode, addr : D7::NodeAddr

  # :nodoc:
  SYM_ARGV = Term.of(:argv)

  private def args_nodes(sink, tree : D7::ParseTree, addr : D7::NodeAddr) : Nil
    case tree
    in D7::InertLeaf
    in D7::GndLeaf
      Term.matchpi?(tree.feature.node, %{[argv]}) do
        sink << ArgvNode.new(addr)
      end
    in D7::ScopeNode, D7::MixtureNode
      args_nodes(sink, tree.child, addr)
    in D7::CircuitNode
      args_nodes(sink, tree.to_group, addr)
    in D7::GroupNode
      return unless tree.summary.has_head?(SYM_ARGV)

      tree.children.zip(tree.feature.range) do |child, key|
        args_nodes(sink, child, addr.append(key))
      end
    end
  end

  def args_nodes(tree : D7::ParseTree) : Slice(ArgvNode)
    sink = Pf::Kit.stack_array(ArgvNode, 8)
    addr = D7::NodeAddr.empty
    args_nodes(sink, tree, addr)

    sink.to_unsafe_readonly_slice!
  end

  # :nodoc:
  HELP = <<-'HELP'
  USAGE
    mxrack [OPTIONS] path/to/seed.wwml
    mxrack [OPTIONS] path/to/seed.wwml -- [CIRCUIT ARGS]

  SYNOPSIS
    Media Extensions for Rack. MxRack lets you build interactive applications
    with Wirewright.

  OPTIONS
    --quiet
      Suppress circuit printout before each frame.
    --stat
      Print statistics for each frame.
    --live
      EXPERIMENTAL: Watch for changes to the seed source and attempt to merge them.
      Failed merges prompt full reload.

  CIRCUIT ARGS
    Circuit arguments are passed to the circuit. They can be accessed through
    the `mxrack.argv` node.

  EXAMPLE
    $ echo '(window (p "Kaixo mundua!"))' > /tmp/seed.wwml
    $ mxrack /tmp/seed.wwml

  HELP

  private def run(iout : IO, alert : BlockingSignal, eventq : EventQueue, args : Array(String)) : Nil
    # Take args up to `--` as our args; then the args that follow are the circuit's
    # args which we'll expose through `mxrack.argv`.
    if argsep = args.index { |arg| arg == "--" }
      mxrack_args = args[...argsep]
      circuit_argv = Term.of(args[argsep + 1..])
    else
      mxrack_args = args.dup
      circuit_argv = Term.of
    end

    if args.empty?
      iout << HELP
      return
    end

    Console.display(iout, Console::MxBanner.new)

    seed_path = NormalPath[mxrack_args[0]]
    quiet = "--quiet".in?(mxrack_args)
    stat = "--stat".in?(mxrack_args)
    live = "--live".in?(mxrack_args)

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

    Console.display(iout, Console::InfoLog.new("Initialized adapters & SDL"))

    # Watch codices.
    pass do
      PathMonitorService.add(NormalPath[Path["runtime"] / "codices"])
      Console.display(iout, Console::InfoLog.new("Watching runtime/codices for changes"))
    end

    # Watch seed.
    pass do
      next unless live

      PathMonitorService.add(seed_path.parent)
      Console.display(iout, Console::InfoLog.new("Watching #{seed_path.parent} for changes to #{seed_path.basename}"))
    end

    perf = Perf.new("Frame 0 (seed)")

    # Load MxRack codex.
    mxrack_blob = ResourceService.read_blob(ResourceService.codex("mxrack"))
    mxrack_blob_digest = mxrack_blob.digest
    mxrack = ML.document(mxrack_blob.to_string)
    mxrack_theme = theme_mixin(mxrack[:theme])
    mxrack_library = Rack::Assembler.library(mxrack[:library])
    Console.display(iout, Console::InfoLog.new("Loaded MxRack codex"))

    # Load Microfold codex.
    microfold_base_blob = ResourceService.read_blob(ResourceService.codex("ufold"))
    microfold_base_digest = microfold_base_blob.digest
    microfold_base_theme = ML.document(microfold_base_blob.to_string).as_d
    Console.display(iout, Console::InfoLog.new("Loaded Microfold codex"))

    # Load seed.
    seed_blob = PathService.read(seed_path).wait.unwrap.as(PathService::ContentReading).blob
    seed_digest = seed_blob.digest
    seed = ML.document(seed_blob.to_string)
    prev_seed = seed

    Console.display(iout, Console::InfoLog.new("Loaded seed"))

    microfold : Microfold::Codex? = nil
    microfold_mixed_theme = Term.of

    circuits = {} of Term => Circuit

    # Proceed to the mainloop.
    circuit = Circuit.new(clf, nopclf, alert)
    circuit.draft = seed

    prev_displayed = Term.of

    windowing_setup = nil
    windowing_cache = ViRecognized::RecognizeCache.new

    Console.display(iout, Console::InfoLog.new("Ready"))

    epoch = 0u64
    UInt64::MAX.times do |frameno|
      version0 = circuit.version

      # Live reload MxRack codex.
      perf.stage("Checkout MxRack codex") do
        mxrack_blob = ResourceService.read_blob(ResourceService.codex("mxrack"))
        next if mxrack_blob_digest == mxrack_blob.digest
        mxrack_blob_digest = mxrack_blob.digest

        begin
          mxrack = ML.document(mxrack_blob.to_string)
          mxrack_theme = theme_mixin(mxrack[:theme])
          mxrack_library = Rack::Assembler.library(mxrack[:library])
          Console.display(iout, Console::NoteLog.new("Reloaded MxRack codex"))
        rescue e : ML::SyntaxError
          Console.display(iout, Console::ErrLog.new("Syntax error in MxRack codex: #{e.message}"))
        end
      end

      # Live reload base Microfold codex.
      perf.stage("Checkout Microfold codex") do
        microfold_base_blob = ResourceService.read_blob(ResourceService.codex("ufold"))
        next if microfold_base_digest == microfold_base_blob.digest
        microfold_base_digest = microfold_base_blob.digest

        begin
          microfold_base_theme = ML.document(microfold_base_blob.to_string).as_d
          Console.display(iout, Console::NoteLog.new("Reloaded Microfold codex"))
        rescue e : ML::SyntaxError
          Console.display(iout, Console::ErrLog.new("Syntax error in Microfold codex: #{e.message}"))
        end
      end

      # Maintain the Microfold codex up-to-date as a composition of various "ingredients":
      # some of them coming from the base theme in codices/, some from the MxRack codex,
      # and others yet, live, from the circuit itself.
      microfold = perf.stage("Mix Microfold codex") do
        microfold_circuit_theme = theme_mixins(circuit.parser.parse(circuit.draft))

        microfold_mixed_theme_candidate = {
          microfold_base_theme,
          mxrack_theme,
          microfold_circuit_theme,
        }
        if microfold.nil? || microfold_mixed_theme != microfold_mixed_theme_candidate
          microfold = mix(Microfold::Codex, microfold_base_theme, Slice[mxrack_theme] + microfold_circuit_theme)
          microfold_mixed_theme = microfold_mixed_theme_candidate
          if windowing_setup
            windowing_setup = WindowingSetup.new(windowing_cache, microfold, windowing_setup.epoch + 1, perf)
          end
        end

        microfold
      end

      windowing_setup ||= WindowingSetup.new(windowing_cache, microfold, 0u64, perf)

      # Live reload seed.
      perf.stage("Checkout seed") do
        next unless live
        next unless seed_blob = PathService.read(seed_path).wait.unwrap?
        next unless seed_blob = seed_blob.as?(PathService::ContentReading).try(&.blob)
        next if seed_blob.digest == seed_digest

        seed = ML.document(seed_blob.to_string)
        seed_digest = seed_blob.digest

        reference = prev_seed
        prev_seed = seed

        if merged = merge3?(reference, circuit.draft, seed)
          circuit.draft = merged
          Console.display(iout, Console::NoteLog.new("Reloaded seed"))
        else
          Console.display(iout, Console::NoteLog.new("Cannot merge changes automatically, do you wish to do a full reload? [y/]"))
          next unless gets == "y"

          Console.display(iout, Console::NoteLog.new("Fully reloaded seed"))
          circuit.draft = seed
        end
      end

      windows = perf.stage("Derive windows") do
        windows(windowing_setup, Slice(Term).empty, circuit.parser.parse(circuit.draft), interactive: true)
      end

      perf.stage("Handle event") do
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
      end

      perf.stage("Hit test") do
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
          # which we later update (set) in hit() for each hit text. This way, all *un*hit
          # texts are reset, and all hit texts are updated!
          adjustments += hit(window, session)
        end

        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end
      end

      perf.stage("Fill `mxrack.argv` nodes") do
        draft_tree = circuit.parser.parse(circuit.draft)
        args_nodes = args_nodes(draft_tree)

        adjustments = Slice(Adjustment).empty
        args_nodes.each do |args_node|
          adjustments = adjustments.append(AdjustArgv.new(args_node.addr, circuit_argv))
        end

        if adjustments.present?
          circuit.draft = adjust(draft_tree, adjustments)
        end
      end

      frame = perf.stage("Step circuit") do
        perf.stage("Symbolic physics") do
          circuit.step(Rack::Prepass, mxrack_library, perf)
        end

        # Get all projections of the circuit.
        step_out = circuit.parser.parse(circuit.draft)
        windows, automata, au_windows = windows_with_automata(windows(windowing_setup, Slice(Term).empty, step_out, interactive: true), step_out)

        adjustments = Pf::Kit.stack_array(Adjustment, 8)

        # FIXME: gc unused circuits
        automata.reverse_each do |automaton|
          au_token = Term[Term.hashcode(automaton.key)]
          guest = circuits.put_if_absent(automaton.key) { Circuit.new(auclf(au_token), reprclf(au_token), alert) }
          guest.draft = automaton.draft

          token = Term[Term.hashcode(automaton.key)]

          # Maintain automaton token up-to-date.
          unless automaton.workspace[:token]? == token
            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:token), Term.of(token))
          end

          pass do
            # The circuit didn't handle turns yet, or wants to manipulate the current
            # draft or present; don't disturb it with a physics pass
            next if automaton.workspace.includes?(:turn) || automaton.workspace.includes?(:program)

            # TODO: We want to submit subframes, not frames.
            perf.stage("Symbolic physics") do
              guest.step(Rack::Prepass, mxrack_library, perf)
            end

            next if automaton.draft == guest.draft

            adjustments << AdjustAutomatonUpdateDraft.new(automaton.addr, guest.draft)
          end

          # Turn management
          pass do
            next unless automaton.workspace[:turn]?.in?(nil, Term.of(:automaton))

            current = automaton.workspace[:present]? || guest.draft
            current_version = Term.of(Term.hashcode(current).to_s(base: 16))
            next if automaton.workspace[:showing]? == current_version

            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:turn), Term.of(:inspect))
            adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:showing), current_version)
          end

          if program = automaton.workspace[:program]?
            Term.matchpiT?(program, %{(addrQ_dict backsysQ_*)}) do
              next unless addr = D7::NodeAddr.new?(addrQ)

              backsys = backsysQ.items.to_compact_readonly_slice do |item|
                Term.matchpi?(item, %{[backmap pattern_ backspec_]}) do
                  {pattern, backspec}
                end
              end

              if input = automaton.workspace[:present]?
                output = manipulate(input, addr, 0u32, backsys)
                adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:present), output)
              else
                output = manipulate(automaton.draft, addr, 0u32, backsys)
                adjustments << AdjustAutomatonUpdateDraft.new(automaton.addr, output)
              end

              adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:program), nil)
            end

            next
          end

          perf.stage("Produce present and repr for automata") do
            if present = automaton.workspace[:present]?
              present_tree = guest.parser.parse(present)
              repr_tree = guest.repr_parser.parse(present)
              window_transcripts = windows(windowing_setup, automaton.scope, present_tree, interactive: false).map { |window| transcribe(window) }

              adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:repr), repr(windowing_setup, automaton.addr, repr_tree, present_tree, token))
              adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:windows), Term.of(window_transcripts))
            else
              window_transcripts = (au_windows[automaton]? || [] of Window).map { |window| transcribe(window) }
              # FIXME: this is wrong in all sorts of ways. Why do we have to reparse stuff?
              # Why do we use the MxRack classifier to parse everything and then have to reparse
              # draft and so on with automaton's classifier auclf()?
              draft_tree = guest.parser.parse(guest.draft)
              repr_tree = guest.repr_parser.parse(guest.draft)

              adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:repr), repr(windowing_setup, automaton.addr, repr_tree, draft_tree, token))
              adjustments << AdjustAutomatonUpdate.new(automaton.addr, Term.of(:windows), Term.of(window_transcripts))
            end
          end
        end

        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end

        # Stepping automata can alter projections, so we have to recalculate them afterward.
        draft_tree = circuit.parser.parse(circuit.draft)
        windows, _, _ = windows_with_automata(windows(windowing_setup, Slice(Term).empty, draft_tree, interactive: true), draft_tree)

        # Perform focus exchange if possible -- so that `focus: wanted` becomes `focus: true`,
        # and previous foci are released.
        windows, adjustments = focus_exchange(windows)

        # Apply adjustments, if any.
        if adjustments.present?
          circuit.draft = adjust(circuit.parser.parse(circuit.draft), adjustments)
        end

        circuit.draft
      end

      perf.stage("Present windows") do
        windows.each do |window|
          session = wm.connect(window.key)
          session.submit(window.spec, window.scenery)
        end
      end

      # DEBUG
      pass do
        next if quiet

        to_display = frame
        next if prev_displayed == to_display

        ML.display(iout, to_display)
        prev_displayed = to_display
      end

      # This is a spot for garbage collection and similar kinds of cleanup.
      perf.stage("GC") do
        wm.gc
        windowing_cache.sweep if windowing_cache.vertexcnt > 512
        microfold.sweep
        circuit.sweep
        circuits.each_value(&.sweep)
      end

      if stat
        perf_root = perf.sample
        perf_root.display(iout, depth: 0u32)
      end

      perf.next("Frame #{frameno + 1}")

      # Then there is this final section of the loop, which decides how and whether
      # to wait, just as in a classical game loop.

      version1 = circuit.version
      next unless version0 == version1
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

    Console.display(iout, Console::InfoLog.new("Reached quiescence"))

    # TODO: deinit
  end

  def run : Nil
    ctx = Fiber::ExecutionContext::Isolated.new("MxRack main thread", spawn_context: MT) do
      alert = BlockingSignal.new
      events = EventQueue.new(alert)
      run(STDOUT, alert, events, ARGV)
    end
    ctx.wait
  end
end
