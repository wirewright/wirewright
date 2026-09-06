module MuSoma
  defcase Workspace,
    console : Channel(Console::Widget),
    extrinsics : ExtrinsicMap,
    codex : Var(Codex),
    state : Var(Term::Dict),
    library : Var(Rack::Assembler::RuleLibrary),
    mu_codex_doc : Term,
    mu_codex : Var(Microfold::SyncCodex),
    parser : D7::Parser,
    msgq : BlockingQueue(Msg),
    alarm : BlockingSignal,
    scheduler : Scheduler

  # Constructs a workspace.
  def workspace(
    console : Channel(Console::Widget),
    input_ref : ExtrinsicMap::ReadingRef,
    codex_ref : ExtrinsicMap::ReadingRef,
    microfold_ref : ExtrinsicMap::ReadingRef,
  ) : Workspace
    # Runloop.
    msgq = BlockingQueue(Msg).new
    alarm = BlockingSignal.new

    # Codex (required for the whole thing to run, so we have to load it
    # ahead-of-time).
    codex_string = PathService.read_string(codex_ref.path)
    codex_doc = ML.document(codex_string)
    codex = codex(codex_doc)
    codex_var = Var.new(codex)

    # State.
    state = codex.initial_state
    state_var = Var.new(state)

    # Library.
    library_var = Var.new(Rack::Assembler.library)

    # Microfold codex.
    mu_codex_doc = ML.document(PathService.read_string(microfold_ref.path))
    mu_themed_doc = Term.merge(mu_codex_doc, Term.of(codex.theme))
    mu_rem = state[:rem].as_n
    mu_codex = Microfold.codex(mu_themed_doc, rem: mu_rem).unwrap
    mu_codex_var = Var.new(mu_codex)

    # Extrinsics.
    extrinsics = ExtrinsicMap.new(msgq, alarm)
    extrinsics.add(codex_ref)
    extrinsics.add(microfold_ref)

    # Misc.
    parser = D7::Parser.new(MuSoma.clf)
    scheduler = Scheduler.new

    Workspace.new(
      console, extrinsics, codex_var, state_var, library_var, mu_codex_doc,
      mu_codex_var, parser, msgq, alarm, scheduler,
    )
  end

  class AgentPopulation
    def initialize(
      @app : AppAgent,
      @codex : CodexAgent,
      @editor : EditorAgent,
      @pretty : PrettyAgent,
      @distill : DistillAgent,
      @rack : RackAgent,
      @mouse : MouseAgent,
      @keyboard : KeyboardAgent,
      @scheduler : SchedulerAgent,
    )
    end

    def boot(workspace : Workspace)
      # In no particular order.
      @editor.boot(workspace)
      @rack.boot(workspace)
    end

    def sync(workspace : Workspace) : Nil
      # Everything depends on the codex so it must be synced first.
      @codex.sync(workspace)

      # In no particular order.
      @editor.sync(workspace)
      @rack.sync(workspace)
    end

    def present(workspace : Workspace) : Nil
      @pretty.present(workspace)
      @distill.present(workspace)
      # App must be called after the above because it needs access to the results
      # of the above to draw.
      @app.present(workspace)
    end

    def observe(workspace : Workspace, hg : D7::Hypergraph, plan : Plan) : Nil
      # In no particular order.
      @keyboard.observe(workspace, hg, plan)
      @scheduler.observe(workspace, hg, plan)
    end

    def receive(workspace : Workspace, request : AppRequest) : Nil
      # In no particular order.
      @app.receive(workspace, request)
      @rack.receive(workspace, request)
    end

    def receive(workspace : Workspace, plan : Plan, msg : Msg) : Nil
      # In no particular order.
      @app.receive(workspace, plan, msg)
      @rack.receive(workspace, plan, msg)
      @mouse.receive(workspace, plan, msg)
      @keyboard.receive(workspace, plan, msg)
    end

    def step(ws : Workspace) : Nil
      # Not sure how important order is here, but it seems important...
      # Assembler should definitely come before Rack, and Rack should
      # probably come last. The other ones are "eyeballed".
      @pretty.step(ws)
      @editor.step(ws)
      @rack.step(ws)
    end
  end

  def sync(ws : Workspace, agents : AgentPopulation)
    agents.sync(ws)

    # Sync Microfold (theme).
    if Var.pending?({ws.state, :rem}, ws.codex)
      mu_themed_doc = Term.merge(ws.mu_codex_doc, Term.of(ws.codex.get.theme))
      ws.mu_codex.set(Microfold.codex(mu_themed_doc, rem: ws.state.get[:rem].as_n).unwrap)
    end
  end

  def present(ws : Workspace, agents : AgentPopulation)
    agents.present(ws)
  end

  def entangle(ws : Workspace, agents : AgentPopulation)
    plan = Plan.new

    # Plan: observe ("soak in" changes from the circuit)
    #
    # NOTE: Pausing applies to entangle read but NOT entangle write. We cannot "pause"
    # the real world & its perturbations (we can, in terms of computation, but
    # that won't make much sense); so the latest circuit absorbs changes even
    # if it is paused. However while paused we prevent the circuit from affecting
    # the real world.
    Term.matchpi?(ws.state.get, %{{¦ timeline: (_ I _ (%any . ...) draft_)}}) do
      draft_tree = ws.parser.parse(draft)
      draft_hg = D7::Hypergraph.new(draft_tree)
      agents.observe(ws, draft_hg, plan)
    end

    # Plan: scheduler
    ws.scheduler.tick do |event|
      agents.receive(ws, plan, event)
    end

    # Plan: msgq
    if msg = ws.msgq.shift?
      agents.receive(ws, plan, msg)
    end

    # Rendezvous (execute plan)
    if plan.present?
      ws.state.update do |state|
        Term.matchpi?(state, %{{¦ timeline: (_ I _ _ draft_)}}) do
          draft_tree = ws.parser.parse(draft)
          draft1 = MuSoma.perturb(draft_tree, plan)
          state = Term.morph(state, {:timeline, 4, draft1})
        end

        state
      end
    end
  end

  def step(ws : Workspace, agents : AgentPopulation)
    # step stateR
    if Var.pending?(ws.state, ws.codex)
      requests = Pf::Kit.stack_array(AppRequest, 8)

      ws.state.update do |state|
        codex = ws.codex.get

        state = Rho.rewrite(codex.stateR, Term.of(state)).as_d

        # Clear cookies after a rewrite cycle. Rules use cookies to implement e.g.
        # mutual exclusion during a rewrite cycle.
        state = state.with(:cookies, Term[])

        # Move requests over the boundary. We can't execute them right now because
        # we're inside update().
        state[:requests].items.each do |request|
          requests << AppRequest.new(request)
        end
        state = state.with(:requests, Term[])

        state
      end

      # Handle requests.
      requests.each do |request|
        agents.receive(ws, request)
      end
    end

    agents.step(ws)
  end

  def run(args : Array(String) = ARGV) : Nil
    # Initialize console.
    console = Channel(Console::Widget).new

    # Owns STDOUT. In the future we'd want to pipe Crystal's Log here, somehow, or
    # make our own, so that the style is consistent.
    spawn(name: "MuSoma console") do
      while widget = console.receive?
        Console.display(STDOUT, widget)
      end
    end

    console.send(Console::MuBanner.new)

    # Parse arguments.
    unless input_arg = args.first?
      console.send(Console::CriticalLog.new("Expected a file argument (try `examples/calculator.musoma.wwml` your MuSoma download has an `examples` directory)"))
      Fiber.yield
      return
    end

    unless runtime = Ww.roots.runtime
      console.send(Console::CriticalLog.new("Could not find Wirewright runtime (you likely need to set WW_RUNTIME)"))
      Fiber.yield
      return
    end

    console.send(Console::InfoLog.new("Initializing refs"))

    input_ref = ExtrinsicMap::ReadingRef.new(NormalPath[input_arg])
    # TODO: Use ResourceRef instead of manually resolving runtime!
    library_ref = ExtrinsicMap::ReadingRef.new(NormalPath[runtime / "soma.lib.wwml"])
    codex_ref = ExtrinsicMap::ReadingRef.new(NormalPath[runtime / "codices/musoma.codex.wwml"])
    editR_ref = ExtrinsicMap::ReadingRef.new(NormalPath[runtime / "codices/editR.codex.wwml"])
    microfold_ref = ExtrinsicMap::ReadingRef.new(NormalPath[runtime / "codices/ufold.codex.wwml"])

    console.send(Console::InfoLog.new("Initializing workspace"))

    ws = MuSoma.workspace(console, input_ref, codex_ref, microfold_ref)

    # Spawn runloop-related fibers.
    wg = WaitGroup.new(1)

    spawn(name: "MuSoma media listener") do
      MediaService.listen(wg) do |notification|
        next unless notification.is_a?(MediaService::WindowDescriptionChanged)

        ws.msgq << notification
        ws.alarm.call
      end
    end

    wg.wait

    console.send(Console::InfoLog.new("Initializing and booting agents"))

    agents = AgentPopulation.new(
      app: AppAgent.new,
      codex: CodexAgent.new(codex_ref),
      editor: EditorAgent.new(editR_ref),
      pretty: PrettyAgent.new,
      distill: DistillAgent.new,
      rack: RackAgent.new(library_ref, input_ref),
      mouse: MouseAgent.new,
      keyboard: KeyboardAgent.new,
      scheduler: SchedulerAgent.new,
    )

    agents.boot(ws)

    console.send(Console::InfoLog.new("Running"))

    epoch = 0u64

    loop do
      sync(ws, agents)
      present(ws, agents)
      entangle(ws, agents)
      step(ws, agents)

      # WAIT
      next if Var.pending?(ws.state, ws.codex, ws.mu_codex, ws.library)
      next if ws.msgq.present?

      epoch = ws.alarm.wait_until(epoch, ws.scheduler.timeout?)
    end
  end
end
