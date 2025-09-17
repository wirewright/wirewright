# Wirewright Rack command-line interface.
#
# TODO: implement as a Rack. We almost have all the tools. I think we're only missing
# some form of IPC in racks.
module Ww::Frontend::Rack
  private alias Textual = Soma::DwUIR::Textual
  private alias Console = Soma::DwUIR::Window::Console

  # Rack server frontend.
  module Server
    extend self

    UIR_PATH        = Path["uiR-succ8.soma.wwml"]
    RACK_BASIS_PATH = RUNTIME_PATH / "basis.rack.wwml"
    EDIT_PATH       = RUNTIME_PATH / "input.soma.wwml"

    # Starts a Rack server.
    def run(*, files = Disk) : Nil
      server = Fiber::ExecutionContext::Isolated.new("Rack Server", spawn_context: MT) do
        ::Ww::Rack::Server.hub(STDIN, STDOUT) do |hub|
          conf = ::Ww::Rack::Server.conf(hub, files,
            basis_path: RACK_BASIS_PATH,
            uir_path: UIR_PATH,
            edit_path: EDIT_PATH,
          )

          ::Ww::Rack::Server.serve(conf, hub)
        end
      end

      server.wait
    end
  end

  # Rack client frontend.
  module Client
    extend self

    APP_PATH   = Path["ww5.wwml"]
    UIR_PATH   = Path["uiR-succ8.soma.wwml"]
    THEME_PATH = Path["theme.ufold.wwml"]
    INPUT_PATH = RUNTIME_PATH / Path["input.soma.wwml"]

    # :nodoc:
    record Requests, channel : Channel(Term), seq : Int32

    # :nodoc:
    defcase State,
      files : FileServer,
      requests : Requests,
      window : Term,
      components : Ruleset,
      component_cache : Alloy::ExpansionCache,
      theme : Soma::Microfold::Theme,
      uiR : Rewriter,
      insetfixR : Rewriter,
      input : Input::Context,
      console : Console::Any,
      env0 : Term::Dict?,
      env1 : Term::Dict,
      viewport : Term::Dict?

    private def boot(files : FileServer, requests : Requests, env : Term::Dict, console : Console::Any) : State
      appspec = ML.document(files.read_string(APP_PATH))

      env = appspec[:model] | env

      components, rest = Ruleset.ruleset_and_rest(ML.term(%{[rule pattern_ template_]}), appspec[:view])
      component_cache = SyncCache(Term, Alloy::Expansion).new(1024, preallocate: true)

      window = rest.items.find do |item|
        Term.matchpi?(item, %{[window _*]}) do
          true
        end
      end

      unless window
        raise "could not find a term matching `[window _*]` in section `app` of the app template"
      end

      # When setting up Microfold, remember we're in the terminal; so use `1`
      # for spacing and 1 for `rem`.
      themedoc = ML.document(files.read_string(THEME_PATH))
      themedoc = themedoc.morph({:globals, :spacing, 1})
      theme = Soma::Microfold.theme(themedoc, rem: Term[1])

      uiR = Soma.uiR(
        replier: ->(term : Term) { Textual.reply(term) },
        rulebase: ML.document(files.read_string(UIR_PATH)),
      )

      insetfixR = Textual.insetfixR

      inputbase = ML.document(files.read_string(INPUT_PATH))
      inputctx = Input.context(inputbase[:rules])

      State.new(files, requests, window, components, component_cache, theme, uiR, insetfixR, inputctx, console, env0: nil, env1: env, viewport: nil)
    end

    # Renders and presents the next frame.
    private def next(state : State) : State
      if state.env0 == state.env1
        return state
      end

      dwuir = pipe(state.window,
        Alloy.render(state.env1),
        Alloy.render(state.components, cache: state.component_cache),
        Soma::Microfold.render(state.theme),
        rewrite(state.insetfixR),
        rewrite(state.uiR),
      )

      viewport0 = state.viewport

      Term.matchpi?(dwuir, %{(%leaf {¦ final-w: ±width, final-h: ±height, id: viewport})}) do
        state = state.copy_with(viewport: Term[width: width, height: height])
      end

      console1 = Console.next(state.console, dwuir)
      Console.present(console1)

      unless viewport0 == state.viewport
        state.requests.channel.send(Term.of(:console, :viewport, state.viewport))
      end

      state.copy_with(env0: state.env1, console: console1)
    end

    # Handles a console event.
    private def next(state : State, *, event : Term) : State
      Term.case(event) do
        matchpi %{(window resized ⍊ w: ±width h: ±height)} do
          state.copy_with(
            env1: state.env1.morph(
              {:terminal, :width, width},
              {:terminal, :height, height},
            )
          )
        end

        matchpi %{(keyboard key c dn ⍊ ctrl)} do
          state.copy_with(console: Console::None.new)
        end

        matchpi %{(keyboard key r dn ⍊ ctrl)} do
          boot(state.files, state.requests, state.env1, state.console)
        end

        matchpi %{[keyboard key escape dn]} do
          state.copy_with(env1: state.env1.morph({:"screen-index", 0}))
        end

        matchpi %{(keyboard key p dn ⍊ ctrl)} do
          state.copy_with(env1: state.env1.morph({:"screen-index", (state.env1[:"screen-index"] - 1) % state.env1[:"screens"].size}))
        end

        matchpi %{(keyboard key n dn ⍊ ctrl)} do
          state.copy_with(env1: state.env1.morph({:"screen-index", (state.env1[:"screen-index"] + 1) % state.env1[:"screens"].size}))
        end

        if state.env1[:"screen-index"] == Term.of(0) # rack
          matchpi %{(keyboard key up dn)} do
            case state.env1[:focus, :targets, state.env1[:focus, :index]]
            when Term.of(:"control-pane")
              state.copy_with(env1: Term.morph(state.env1, :states, :"control-pane", :scroll) { |value| value - 1 })
            when Term.of(:"events-pane")
              state.copy_with(env1: Term.morph(state.env1, :states, :"events-pane", :scroll) { |value| value - 1 })
            else
              continue
            end
          end

          matchpi %{(keyboard key dn dn)} do
            case state.env1[:focus, :targets, state.env1[:focus, :index]]
            when Term.of(:"control-pane")
              state.copy_with(env1: Term.morph(state.env1, :states, :"control-pane", :scroll) { |value| value + 1 })
            when Term.of(:"events-pane")
              state.copy_with(env1: Term.morph(state.env1, :states, :"events-pane", :scroll) { |value| value + 1 })
            else
              continue
            end
          end

          matchpi %{[keyboard key pgdn dn]} do
            case state.env1[:focus, :targets, state.env1[:focus, :index]]
            when Term.of(:"control-pane")
              state.copy_with(env1: state.env1.morph({:states, :"control-pane", :scroll, 0}))
            when Term.of(:"events-pane")
              state.copy_with(env1: state.env1.morph({:states, :"events-pane", :scroll, 0}))
            else
              continue
            end
          end

          matchpi %{[keyboard key backspace dn]} do
            case state.env1[:focus, :targets, state.env1[:focus, :index]]
            when Term.of(:"control-pane")
              state.copy_with(
                env1: state.env1.morph(
                  {:states, :"control-pane", :log, Term[]},
                  {:states, :"control-pane", :scroll, 0},
                )
              )
            when Term.of(:"events-pane")
              state.copy_with(
                env1: state.env1.morph(
                  {:states, :"events-pane", :log, Term[]},
                  {:states, :"events-pane", :scroll, 0},
                )
              )
            else
              continue
            end
          end

          matchpi %{(keyboard key tab dn ⍊ shift)} do
            state.copy_with(env1: Term.morph(state.env1, :focus, :index) { |index| (index - 1) % state.env1[:focus, :targets].itemsize })
          end

          matchpi %{[keyboard key tab dn]} do
            state.copy_with(env1: Term.morph(state.env1, :focus, :index) { |index| (index + 1) % state.env1[:focus, :targets].itemsize })
          end

          otherwise do
            return state unless state.env1[:states, :"command-input", :state] == Term.of(:unlocked)
            return state unless state.env1[:focus, :targets, state.env1[:focus, :index]] == Term.of(:input)

            input0 = state.env1[:states, :"command-input", :model]
            input1 = Input.send(state.input, input0, event) do |submission|
              if state1 = submit?(state, submission)
                state = state1
                true # accepted
              else
                false # rejected
              end
            end

            state.copy_with(env1: state.env1.morph({:states, :"command-input", :model, input1}))
          end
        else # screen-index > 0
          otherwise do
            if screen = state.env1[:screens, state.env1[:"screen-index"]]?
              state.requests.channel.send(Term.of(:console, :event, screen[:id], event))
            end
            state
          end
        end
      end
    end

    # Handles a response from the Rack server.
    private def next(state : State, *, response : Term) : State
      Term.case(response) do
        matchpi %{(done id←(%number +i32))} do
          seq = state.requests.seq
          unless seq == id.to(Int32)
            return state
          end

          state.copy_with(
            env1: state.env1.morph({:states, :"command-input", :state, :unlocked}),
            requests: state.requests.copy_with(seq: seq + 1),
          )
        end

        matchpi %{(reply entry_)} do
          log(state, entry)
        end

        matchpi %{(console update id←(%number +i32) window_)} do
          # Find or create screen width id, update its content.
          screens0 = screens1 = state.env1[:screens]
          screens0.each_entry do |key, screen|
            next unless screen.type.dict?
            next unless screen[:id] == id

            screens1 = screens1.with(key, screen.with(:window, window))
          end

          if screens0 == screens1
            screens1 = screens1.append(Term[id: id, window: window])
          end

          state.copy_with(env1: state.env1.morph({:screens, screens1}))
        end

        matchpi %{(console close id←(%number +i32))} do
          screens0 = state.env1[:screens]
          screens1 = Term::Dict.build do |commit|
            screens0.items.each do |screen|
              unless screen.type.dict?
                commit << screen
                next
              end
              unless screen[:id] == id
                commit << screen
                next
              end
              # skip
            end
          end

          state.copy_with(env1: state.env1.morph({:screens, screens1}))
        end

        matchpi %{(event (narration (rack deactivated path_string)))} do
          if state.env1[:states, :header, :rackpath]? == path
            state = state.copy_with(env1: state.env1.morph({:states, :header, Term[]}))
          end
          continue
        end

        matchpi %{(event (narration (rack activated path_string)))} do
          # When a rack is activated, we send console viewport size to it in case
          # it needs it to continue.
          if viewport = state.viewport
            state.requests.channel.send(Term.of(:console, :viewport, viewport))
          end

          state = state.copy_with(env1: state.env1.morph({:states, :header, :rackpath, path}))
          continue
        end

        matchpi %{(event (narration (window open addr_)))} do
          state = state.copy_with(env1: state.env1.morph({:states, :header, :agents, :windows, addr, :open}))
          continue
        end

        matchpi %{(event (narration (window closed addr_)))} do
          state = state.copy_with(env1: state.env1.morph({:states, :header, :agents, :windows, addr, :closed}))
          continue
        end

        matchpi %{(event (narration (file loaded addr_ _)))} do
          state = state.copy_with(env1: state.env1.morph({:states, :header, :agents, :files, addr, :loaded}))
          continue
        end

        matchpi %{(event (narration (file unloaded addr_ _)))} do
          state = state.copy_with(env1: state.env1.morph({:states, :header, :agents, :files, addr, :unloaded}))
          continue
        end

        matchpi %{(event msg_)} do
          state.copy_with(env1: Term.morph(state.env1, :states, :"events-pane", :log, &.append(msg)))
        end

        otherwise { state }
      end
    end

    # Handles a submission from the user. Returns `nil` if it must be rejected
    # on the input level.
    private def submit?(state : State, submission : String) : State?
      begin
        command = ML.terms(submission)
      rescue e : ML::SyntaxError
        return log(state,
          Term.of(:thread, submission),
          Term.of(:error, title: "Invalid command", detail: "Syntax error in command: #{e.detail}")
        )
      end

      Term.case(command) do
        givenpi %{help} do
          log(state,
            Term.of(:thread, submission),
            Term.of(:help, state.env1[:help]),
          )
        end

        givenpi %{rack} do
          request(state, Term.of(submission), Term.of({:help}))
        end

        givenpi %{rack subcommand_+} do
          request(state, Term.of(submission), subcommand)
        end

        otherwise do
          log(state,
            Term.of(:thread, submission),
            Term.of(:error, title: "invalid command", detail: "Invalid command `#{submission}`, use `help` to learn about available commands."),
          )
        end
      end
    end

    private def log(state : State, *nodes : Term) : State
      nodes.reduce(state) { |memo, node| log(memo, node) }
    end

    private def log(state : State, node : Term) : State
      state.copy_with(env1: Term.morph(state.env1, :states, :"control-pane", :log, &.append(node)))
    end

    private def request(state : State, submission : Term, payload : Term)
      state = log(state, Term.of(:thread, submission))
      state = state.copy_with(env1: state.env1.morph({:states, :"command-input", :state, :locked}))
      state.requests.channel.send(Term.of(:txn, state.requests.seq, payload))
      state
    end

    # Blocking send that doesn't explode if the channel is closed while waiting.
    private def try_send(channel, object)
      channel.send(object)
    rescue Channel::ClosedError
    end

    # Rack client mainloop.
    private def run(files : FileServer, server : Process, initial : Path?, width : Int32, height : Int32) : Nil
      evtick = Channel(Bool).new
      requests = Channel(Term).new
      responses = Channel(Term).new
      terminate = Channel(Exception?).new

      # This fiber will listen for terminal events.
      spawn do
        loop do
          evtick.send(true)
          sleep 30.milliseconds
        end
      rescue e
        try_send(terminate, e)
      end

      # This fiber will encode and send requests to the server process.
      spawn do
        while request = requests.receive
          server.input.puts ML.compact(request)
          server.input.flush
        end
      rescue e
        try_send(terminate, e)
      end

      # This fiber will receive and decode responses from the server process.
      spawn do
        while input = server.output.gets
          responses.send(ML.term(input))
        end
      rescue e
        try_send(terminate, e)
      else
        try_send(terminate, nil)
      end

      state = boot(files,
        requests: Requests.new(requests, seq: 0),
        env: Term[terminal: {width: width, height: height}],
        console: Console::None.new,
      )

      begin
        if initial
          state = request(state, Term.of(initial), Term.of(:load, initial))
        end

        loop do
          state = self.next(state)

          select
          when evtick.receive
            # Let's not block for too long inside the poll loop.
            events = Deque(Term).new
            Console.poll do |event|
              events << event
            end

            while event = events.shift?
              state = self.next(state, event: event)
            end
          when response = responses.receive
            state = self.next(state, response: response)
          when reason = terminate.receive
            break unless reason

            raise reason
          end

          break if state.console.is_a?(Console::None)
        end
      ensure
        terminate.close
        evtick.close
        requests.close
        responses.close
      end
    end

    # Starts a Rack client.
    def run(server : Process, initial : Path?, *, files : FileServer = Disk) : Nil
      client = Fiber::ExecutionContext::Isolated.new("Rack Client", spawn_context: MT) do
        Console.setup do |width, height|
          run(files, server, initial, width, height)
        end
      end

      client.wait
    end
  end
end
