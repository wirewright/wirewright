module Ww::Rack
  # Coordinates the lifecycle of a rack instance and the communication
  # protocol around it.
  #
  # A rack server instantiates rack environments, provides narration events
  # for state transitions, and drives the control loop that mediates between
  # a client and a rack environment.
  #
  # NOTE: Rack servers assume one-to-one relationship with their client: each
  # server has one and only one associated client, and vice versa. There is no
  # support for multiplexing; that is entirely the caller's concern.
  #
  # ```
  # include Ww
  #
  # Rack::Server.hub(STDIN, STDOUT) do |hub|
  #   conf = Rack::Server.conf(hub, Disk,
  #     basis_path: Path["runtime", "basis.rack.wwml"],
  #     uir_path: Path["runtime", "uiR.soma.wwml"],
  #     edit_path: Path["runtime", "input.soma.wwml"],
  #   )
  #
  #   Rack::Server.serve(conf, hub)
  # end
  # ```
  module Server
    extend self

    Log = ::Log.for(self)

    # :nodoc:
    #
    # Helpers for pushing a message to the client.
    module Push
      extend self

      def comment(sink : Channel(Term), detail : String)
        notify(sink, Term.of(:comment, detail))
      end

      def warn(sink : Channel(Term), detail : String)
        notify(sink, Term.of(:warning, detail))
      end

      def note(sink : Channel(Term), detail : String)
        notify(sink, Term.of(:note, detail))
      end

      def err(sink : Channel(Term), detail : String)
        notify(sink, Term.of(:error, detail))
      end

      def narrate(sink : Channel(Term), term : Term)
        notify(sink, Term.of(:narration, term))
      end

      def notify(sink : Channel(Term), term : Term)
        sink.send(Term.of(:event, term))
      rescue e : Channel::ClosedError
        Log.debug(exception: e) { "channel closed while running notify" }
      end
    end

    # :nodoc:
    #
    # Helpers for replying to client requests.
    module Reply
      extend self

      def err(sink : Channel(Term), title : String, detail : String?)
        reply(sink, Term.of(:error, title: title, detail: detail))
      end

      def reply(sink : Channel(Term), item : Term)
        sink.send(Term.of(:reply, item))
      end
    end

    # :nodoc:
    module Narrator
      extend self

      record Context, sink : Channel(Term), env : Env, device_addr : DeviceAddr, device : Term

      private def narrate(ctx : Context, state0 : State::Window::NotOpen, state1 : State::Window::Open)
        Push.narrate(ctx.sink, Term.of(:window, :open, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Window::Open, state1 : State::Window::NotOpen)
        Push.narrate(ctx.sink, Term.of(:window, :closed, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Console::NotOpen, state1 : State::Console::Open)
        Push.narrate(ctx.sink, Term.of(:console, :open, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Console::Open, state1 : State::Console::NotOpen)
        Push.narrate(ctx.sink, Term.of(:console, :closed, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Source::None, state1 : State::Source::FilePending)
        Push.narrate(ctx.sink, Term.of(:file, :pending, ctx.device_addr, state1.path))
      end

      private def narrate(ctx : Context, state0 : State::Source::FilePending, state1 : State::Source::FileLoaded)
        Push.narrate(ctx.sink, Term.of(:file, :loaded, ctx.device_addr, state1.path))
      end

      private def narrate(ctx : Context, state0 : State::Source::FileLoaded, state1 : State::Source::FileLoaded)
        Push.narrate(ctx.sink, Term.of(:file, :reloaded, ctx.device_addr, state1.path))
      end

      private def narrate(ctx : Context, state0 : State::Source::FileLoaded | State::Source::FilePending, state1 : State::Source::None)
        Push.narrate(ctx.sink, Term.of(:file, :unloaded, ctx.device_addr, state0.path))
      end

      private def narrate(ctx : Context, state0 : State::Ticker::NotRunning, state1 : State::Ticker::Running)
        Push.narrate(ctx.sink, Term.of(:ticker, :running, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Ticker::Running, state1 : State::Ticker::NotRunning)
        Push.narrate(ctx.sink, Term.of(:ticker, :"not-running", ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Image::None | State::Image::File, state1 : State::Image::File)
        Push.narrate(ctx.sink, Term.of(:image, :drawn, ctx.device_addr, state1.path))
      end

      # :nodoc:
      QUERY_FILE_OF_ML = ML.dict(<<-WWML)
      [src @specs_ @_] specs [const @specs_ (file path_string)]
      WWML

      # :nodoc:
      QUERY_FILE_OF_MICROFOLD_NODES = ML.dict(<<-WWML)
      [ml @srcs_ @_] srcs [src @specs_ @_] specs [const @specs_ (file path_string)]
      WWML

      private def narrate(ctx : Context, state0 : State::ML::Err, state1 : State::ML::Ok)
        exception = state0.exception
        excerpt, line, column = ML::SyntaxError.lookaround(exception.text)

        # Try to figure out the path of failure based on statically known path.
        # First, this already covers about 90% or 99% of expected use cases. Second,
        # if something is touching the source, even if we are smart and query the rack,
        # we still don't know what we're pointing into anymore; is it just statically
        # unknown path or that plus some modifications, in which case our pointing
        # into the file would be misleading at best?
        path = Term.matchpi?(ctx.device, %{[ml @srcs_ @_]}) do
          next unless env1 = Rack.query?(ctx.env.index, origin: srcs, qpath: QUERY_FILE_OF_ML)

          env1[:path]
        end

        Push.notify(ctx.sink,
          Term.of(:error, :ml,
            addr: ctx.device_addr,
            detail: exception.detail,
            code: excerpt,
            path: path,
            line: line,
            column: column))
      end

      # TODO: const path if available
      # TODO: backtrace rendering
      private def narrate(ctx : Context, state0 : State::Alloy::ViewIssues, state1 : State::Alloy::Ok)
        state0.issues.each do |issue|
          Push.notify(ctx.sink,
            Term.of(:error, :alloy,
              addr: ctx.device_addr,
              detail: issue.detail,
              severity: issue.severity))
        end
      end

      # TODO: const path if available
      # TODO: backtrace
      private def narrate(ctx : Context, state0 : State::Alloy::TemplateIssues, state1 : State::Alloy::Ok)
        state0.issues.each do |issue|
          Push.notify(ctx.sink,
            Term.of(:error, :alloy,
              addr: ctx.device_addr,
              detail: issue.detail,
              severity: issue.severity))
        end
      end

      # TODO: backtrace rendering
      #   related: go through Microfold and make sure to have instructions to switch
      #            to theme in backtrace, so that we can point into the theme.
      private def narrate(ctx : Context, state0 : State::MuRender::Issues, state1 : State::MuRender::Ok)
        # Ditto as above: try to figure out the path statically; if we cannot,
        # the user knows better than we do.
        path = Term.matchpi?(ctx.device, %{[microfold (@_ @nodes_) @_]}) do
          next unless env1 = Rack.query?(ctx.env.index, origin: nodes, qpath: QUERY_FILE_OF_MICROFOLD_NODES)

          env1[:path]
        end

        state0.issues.each do |issue|
          Push.notify(ctx.sink,
            Term.of(:error, :microfold,
              addr: ctx.device_addr,
              path: path,
              detail: issue.detail,
              severity: issue.severity))
        end
      end

      private def narrate(ctx : Context, state0 : State::Source::BadQuery, state1 : State::Source::None)
        Push.notify(ctx.sink,
          Term.of(:error, :source,
            addr: ctx.device_addr,
            detail: "bad source query",
            code: ML.compact(state0.query)))
      end

      private def narrate(ctx : Context, state0 : State::Ticker::BadPeriodSpec, state1 : State::Ticker::None)
        Push.notify(ctx.sink,
          Term.of(:error, :ticker,
            addr: ctx.device_addr,
            detail: "bad period specification",
            code: ML.compact(state0.spec)))
      end

      private def narrate(ctx : Context, state0 : State::Image::BadSpec, state1 : State::Image::None)
        Push.notify(ctx.sink,
          Term.of(:error, :image,
            addr: ctx.device_addr,
            detail: "bad image specification",
            code: ML.compact(state0.spec)))
      end

      private def narrate(ctx : Context, state0 : State::Image::BadTarget, state1 : State::Image::None)
        Push.notify(ctx.sink,
          Term.of(:error, :image,
            addr: ctx.device_addr,
            detail: "bad image target specification",
            code: ML.compact(state0.target)))
      end

      private def narrate(ctx : Context, state0 : State::Log::Any, state1 : State::Log::Message)
        case state1.style
        in .comment? then Push.comment(ctx.sink, ML.compact(state1.term))
        in .note?    then Push.note(ctx.sink, ML.compact(state1.term))
        in .warning? then Push.warn(ctx.sink, ML.compact(state1.term))
        in .error?   then Push.err(ctx.sink, ML.compact(state1.term))
        end
      end

      # Ignore all other state transitions.
      private def narrate(ctx : Context, state0, state1)
      end

      # Constructs a server narrator agent that outputs narrations, events, logs,
      # etc. into *sink*.
      def agent(sink : Channel(Term)) : Agent::Narrator
        Agent::Narrator.new do |env, device_addr, device, state0, state1|
          narrate(Context.new(sink, env, device_addr, device), state0, state1)
        end
      end
    end

    private alias Window = DwUIR::Window::SDL

    # Represents the configuration of a server. Prefer to obtain this object from
    # `conf` instead of constructing it by hand.
    defcase Conf,
      basis : Term::Dict,
      mki : Path, Term -> Instance::Some,
      files : FileServer,
      window_context : Window::Context,
      fstime : Time::Span

    # Extended constructor for a server configuration.
    #
    # - *fstime* defines ideal file system polling period.
    def conf(
      hub : Hub,
      basis : Term::Dict,
      uir_base : Term,
      edit_base : Term,
      compositor : DwUIR::Compositor,
      platform : DwUIR::Platform, *,
      fstime : Time::Span = 500.milliseconds,
    ) : Conf
      viewer_context = DwUIR::Viewer::Context.new(compositor, platform)
      window_context = Window.context(hub.setup_proof, viewer_context)

      mki = ->(path : Path, rack : Term) do
        editR = Input.inputR(edit_base)

        text_metricsR = callR { |term| Rewrite.one(DwUIR::Textual.reply(term)) }
        graphics_metricsR = callR { |term| Rewrite.one(DwUIR.reply(platform, term)) }

        text_uiR = Soma.uiR(text_metricsR, uir_base)
        graphics_uiR = Soma.uiR(graphics_metricsR, uir_base)

        agents = [
          Narrator.agent(hub.responses),
          Rack.rewriter(:insetfixR, DwUIR::Textual.insetfixR),
          Rack.rewriter(:editR, editR),
          Rack.rewriter(:text_uiR, text_uiR),
          Rack.rewriter(:graphics_uiR, graphics_uiR),
          Rack::Image.file_snapper(viewer_context),
          Rack.scheduler { |period, workspace| tick(period, workspace, hub.posts) },
          wmsync(hub.posts),
          tsync(hub.responses),
          fixpoint(hub.posts),
        ]

        env, unload = Rack.env(rack, basis, agents)

        Instance::Some.new(path, env, unload)
      end

      Conf.new(basis, mki, platform.files, window_context, fstime)
    end

    # WMsync narrator posts window content and state updates to *sink*.
    #
    # - `(window update id_ spec_)`
    # - `(window close id_)`
    private def wmsync(sink : Channel(Term)) : Agent::Narrator
      Agent::Narrator.new do |_, _, _, state0, state1|
        case {state0, state1}
        when {State::Window::Open, State::Window::NotOpen}
          sink.send(Term.of(:window, :close, state0.id))
        when {State::Window::Any, State::Window::Open}
          sink.send(Term.of(:window, :update, state1.id, state1.spec))
        end
      end
    end

    # Watch step for file-backed `src` devices against *files*. Sends appropriate
    # queries to the environment when a file dependency is created, removed,
    # or modified.
    private def fsmon(files : FileServer, env : Env) : Nil
      alert = Set(Int32).new

      states0 = states1 = env.states
      states0.each do |device_addr, state0|
        case state0
        when State::Source::FilePending
        when State::Source::FileLoaded
          t0 = state0.instant
        else
          next
        end

        path = state0.path

        loop do
          t1 = files.modification_time?(path)

          case {t0, t1}
          in {nil, nil}
            # Did not and does not exist.
            state1 = state0
            break
          in {_, nil}
            # Removed.
            state1 = State::Source::FilePending.new(path, state0.dst)
            states1 = states1.assoc(device_addr, state1)
            alert << device_addr
            break
          in {nil, _}
            # Created.
          in {_, _}
            # Exists.
            break if t0 == t1
          end

          # Modified.
          begin
            content = files.read_string(path)
          rescue FileServerError
            t1 = nil
            next
          end

          state1 = State::Source::FileLoaded.new(path, state0.dst, content, t1)
          states1 = states1.assoc(device_addr, state1)
          alert << device_addr
          break
        end
      end

      env.submit(states1)

      return if alert.empty?

      workspace = Term::Dict.build do |commit|
        alert.each do |device_addr|
          state0, state1 = states0[device_addr], states1[device_addr]

          case {state0, state1}
          when {State::Source::FileLoaded, State::Source::FilePending}
            # Removed
            commit.with(state1.dst, :"?")
          when {State::Source::FileLoaded, State::Source::FileLoaded}, # Modified
               {State::Source::FilePending, State::Source::FileLoaded} # Created
            commit.with(state1.dst, {:currently, state1.content})
          end
        end
      end

      env.send(workspace)
    end

    # Tsync narrator posts console content and existence updates to *sink*,
    # to be reflected in the *t*erminal by the client.
    #
    # - `(console update id_ spec_)`
    # - `(console close id_)`
    private def tsync(sink : Channel(Term)) : Agent::Narrator
      Agent::Narrator.new do |_, _, _, state0, state1|
        case {state0, state1}
        when {State::Console::Open, State::Console::NotOpen}
          sink.send(Term.of(:console, :close, state0.props.id))
        when {State::Console::Any, State::Console::Open}
          sink.send(Term.of(:console, :update, state1.props.id, state1.spec))
        end
      end
    end

    # Fixpoint narrator posts rewrite requests for `fixpoint` nodes.
    private def fixpoint(sink : Channel(Term)) : Agent::Narrator
      Agent::Narrator.new do |_, device_addr, _, state0, state1|
        case {state0, state1}
        when {State::Fixpoint::Any, State::Fixpoint::Pending}
          sink.send(Term.of(:rewrite, device_addr, state1.seq, state1.term))
        end
      end
    end

    # Shorthand constructor for a server configuration.
    def conf(hub : Hub, files : FileServer, basis_path : Path, uir_path : Path, edit_path : Path, **kwargs) : Conf
      basis = ML.document(files.read_string(basis_path))
      uir_base = ML.document(files.read_string(uir_path))
      edit_base = ML.document(files.read_string(edit_path))[:rules]

      conf(hub, basis.as_d, uir_base, edit_base, DwUIR::Compositor.new, DwUIR::PvgPlatform.new(files), **kwargs)
    end

    # Represents a rack instance: a group of handles associated with an initialized
    # rack environment.
    module Instance
      alias Any = Some | Nil

      record Some, path : Path, env : Env, unload : (->)
    end

    private def load(conf : Conf, instance : Instance::Any, path : Path, responses : Channel(Term)) : Instance::Any
      begin
        source = conf.files.read_string(path)
      rescue e : FileServerError
        Reply.err(responses, "file server error", e.message)
        return instance
      end

      begin
        rack = ML.document(source)
      rescue e : ML::SyntaxError
        # TODO: output a full blown syntax error!
        Reply.err(responses, "syntax error in rack spec", e.message)
        return instance
      end

      if instance
        Push.narrate(responses, Term.of(:rack, :deactivated, instance.path))

        instance.unload.call
      end

      Push.narrate(responses, Term.of(:rack, :activated, path))

      conf.mki.call(path, rack)
    end

    # :nodoc:
    HELP = ML.document(<<-WWML)
    (section "General"
      (table
        ("exit" "Politely asks the rack process to terminate itself")
        ("load path_string" "Unloads the current rack file and loads a new one")
        ("unload" "Unloads the current rack file")
        ("reload" "Reloads the current rack file to incorporate your newest changes")))
    (section "Devices"
      (table
        ("device list" "Prints interactive devices in the rack")
        ("device list all" "Prints all devices in the rack")
        ("device info addr←(%number +i32)" "Prints the device at <addr>")))
    (section "Windows"
      (table
        ("window open addr←(%number +i32)" "Opens a closed window at device <addr>")
        ("window close addr←(%number +i32)" "Closes an open window at device <addr>")))
    (section "Workspace"
      (table
        ("workspace edges" "Prints workspace edges in the rack")
        ("workspace trigger @edge_" "Triggers the rack at computed <edge>")
        ("workspace trigger @edge_ value_" "Sets the value of computed <edge> to <value>, triggers the rack")))
    WWML

    # Handles a single request.
    private def handle(conf : Conf, instance : Instance::Any, request : Term, responses : Channel(Term)) : Instance::Any
      Term.case(request) do
        matchpi %{(load path_string)} do
          load(conf, instance, Path[path.to(String)], responses)
        end

        matchpi %{(unload)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          instance.unload.call

          Push.narrate(responses, Term.of(:rack, :deactivated, instance.path))

          nil
        end

        # The utility here is that we remember the path; otherwise, it's the same as `load`.
        matchpi %{(reload)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          load(conf, instance, instance.path, responses)
        end

        matchpi %{(device list)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          response = Term::Dict.build do |commit|
            commit << :list

            env = instance.env
            env.states.each do |device_addr, state|
              device = env.index.device(device_addr)

              case state
              when State::Source::FilePending
                commit << Term.of(:device, :file, addr: device_addr, status: :progress)
              when State::Source::FileLoaded
                commit << Term.of(:device, :file, addr: device_addr, status: :active)
              when State::Window::Open
                commit << Term.of(:device, :window, addr: device_addr, status: :active)
              when State::Window::Closed
                commit << Term.of(:device, :window, addr: device_addr, status: :passive)
              when State::Console::Open
                commit << Term.of(:device, :console, addr: device_addr, status: :active)
              when State::Console::Closed
                commit << Term.of(:device, :console, addr: device_addr, status: :passive)
              when State::Image::Any
                commit << Term.of(:device, :image, addr: device_addr, status: :none)
              when State::Ticker::Any
                commit << Term.of(:device, :ticker, addr: device_addr, status: :none)
              end
            end
          end

          Reply.reply(responses, Term.of(response))

          instance
        end

        matchpi %{(device list all)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          response = Term::Dict.build do |commit|
            commit << :list

            env = instance.env
            env.index.each_device_with_addr do |device, device_addr|
              commit << Term.of(:"device-at", device_addr, device)
            end
          end

          Reply.reply(responses, Term.of(response))

          instance
        end

        matchpi %{(device info addr←(%number +i32))} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          env = instance.env

          unless device = env.index.device?(addr.to(Int32))
            Reply.err(responses, "control error", "invalid device address `#{addr}`. Use `device list all` to verify.")
            return instance
          end

          Reply.reply(responses, Term.of(:"device-at", addr, device))

          instance
        end

        matchpi %{(window open addr←(%number +i32))} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          env = instance.env
          env.map do |device_addr, state|
            next unless device_addr == addr.to(Int32)

            case state
            when State::Window::Closed
              State::Window::Open.new(state.id, state.spec, state.events)
            when State::Console::Closed
              State::Console::Open.new(state.props, state.spec)
            end
          end

          instance
        end

        matchpi %{(window close addr←(%number +i32))} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          env = instance.env
          env.map do |device_addr, state|
            next unless device_addr == addr.to(Int32)

            case state
            when State::Window::Open
              State::Window::Closed.new(state.id, state.spec, state.events)
            when State::Console::Open
              State::Console::Closed.new(state.props, state.spec)
            end
          end

          instance
        end

        matchpi %{(workspace edges)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          response = Term::Dict.build do |commit|
            commit << :list

            env = instance.env
            env.index.each_edge { |edge| commit << edge }
          end

          Reply.reply(responses, Term.of(response))

          instance
        end

        matchpi %{(workspace trigger @edge_)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          env = instance.env
          env.send(Term.entries({edge, :"?"}))

          instance
        end

        matchpi %{(workspace trigger @edge_ value_)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          env = instance.env
          env.send(Term.entries({edge, {:currently, value}}))

          instance
        end

        matchpi %{(help)} do
          Reply.reply(responses, Term.of(:help, HELP))

          instance
        end

        matchpi %{(exit)} do
          raise Exit.new
        end

        matchpi %{(console event key_ event_)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          instance.env.each(State::Console::Open) do |_, state|
            next unless key == Term.of(state.props.id)

            workspace = Term.entries({state.props.events, {:currently, event}})
            instance.env.send(workspace)
            break
          end

          instance
        end

        matchpi %{(console viewport size←(¦ () width: (%number +i32) height: (%number +i32)))} do
          unless instance
            # We do not necessarily want to err if this is the case. The client is
            # allowed to report console viewport sizes too early. They are expected
            # to report console viewport size
            return instance
          end

          workspace = Term::Dict.build do |commit|
            instance.env.each(State::Console::Any) do |_, state|
              commit.with(state.props.sizes, {:currently, size})
            end
          end

          instance.env.send(workspace)
          instance
        end

        otherwise do
          Reply.err(responses, "invalid request", "invalid request `#{ML.compact(request)}`")

          instance # On error, we keep the instance unchanged.
        end
      end
    end

    # Spawns the tick fiber containing the tick loop. The tick loop
    # will post *workspace* to *posts* for every *period*.
    private def tick(period : Time::Span, workspace : Term::Dict, posts : Channel(Term)) : (->)
      cancel = Channel(Bool).new

      spawn do
        id = Fiber.current.object_id

        begin
          Log.trace { "tick##{id.to_s(62)}: running" }

          loop do
            select
            when cancel.receive?
              break
            when timeout(period)
              select
              when posts.send(Term.of(:workspace, workspace))
              when cancel.receive?
                break
              end
            end
          end
        rescue Channel::ClosedError
          Log.trace { "tick##{id.to_s(62)}: queries channel was closed" }
        ensure
          Log.trace { "tick##{id.to_s(62)}: stopped" }

          cancel.close
        end
      end

      -> do
        begin
          cancel.send(true)
        rescue Channel::ClosedError
          Log.debug { "BUG? tick fiber not running on cancel" }

          # Since it's closed there is no tick fiber, so we kind of "succeeded"
          # in canceling it.
        end
      end
    end

    # Groups input/output channels that the server mainloop works with.
    record Hub, setup_proof : Window::SetupProof, posts = Channel(Term).new, responses = Channel(Term).new

    struct Hub
      def close : Nil
        posts.close
        responses.close
      end
    end

    # :nodoc:
    #
    # Raised on a polite exit request.
    class Exit < Exception
    end

    # Starts the Rack server event loop. Blocks until the event loop terminates.
    # Even though the loop is "infinite", it may terminate due to an error or if
    # the client asks it to, through `(exit)`.
    #
    # - *conf* is a server configuration constructed using `conf`.
    # - *hub* is a communication hub constructed using `hub`.
    def serve(conf : Conf, hub : Hub) : Nil
      instance = nil

      # Sends periodic notifications to the main thread to check file dependencies.
      spawn do
        Log.trace { "fstick: running" }

        loop do
          hub.posts.send(Term.of(:fstick))

          # We probably don't want to do anything smart or adaptive here, do we?
          sleep conf.fstime
        end
      rescue e : Channel::ClosedError
        Log.trace { "fstick: channel was closed" }
      ensure
        Log.trace { "fstick: stopped" }
      end

      Window.wmloop(hub.setup_proof, hub.posts, conf.window_context) do |post|
        Term.case(post) do
          # If window was closed (as in, the X button was clicked), we'd want
          # to reflect that in the state map.
          matchpi %{(event key_ (window closed))} do
            next unless instance_ = instance.as?(Instance::Some)

            instance_.env.map(State::Window::Open) do |_, state|
              next unless key == Term.of(state.id)

              State::Window::Closed.new(state.id, state.spec, state.events)
            end

            continue
          end

          # A window event.
          matchpi %{(event key_ term_)} do
            next unless instance_ = instance.as?(Instance::Some)

            instance_.env.each(State::Window::Some) do |_, state|
              next unless key == Term.of(state.id)

              workspace = Term.entries({state.events, {:currently, term}})
              instance_.env.send(workspace)
            end
          end

          # Transaction request from the client.
          matchpi %{(request (txn id_ seq_*))} do
            seq.items.each do |item|
              instance = handle(conf, instance, item, hub.responses)
            end

            hub.responses.send(Term.of(:done, id))
          end

          # Request from the client.
          matchpi %{(request request_)} do
            instance = handle(conf, instance, request, hub.responses)
          end

          # Workspace (most likely from a ticker).
          matchpi %{(workspace workspace_dict)} do
            next unless instance_ = instance.as?(Instance::Some)

            instance_.env.send(workspace.as_d)
          end

          # File system check notification.
          matchpi %{fstick} do
            next unless instance_ = instance.as?(Instance::Some)

            fsmon(conf.files, instance_.env)
          end

          matchpi %{(rewrite device-addr←(%number +i32) seq←(%number +i32) term_)} do
            next unless instance_ = instance.as?(Instance::Some)

            states0 = instance_.env.states
            next unless state0 = states0[device_addr.to(Int32)]?

            case state0
            when State::Fixpoint::Pending
              # Proceed only if it wasn't invalidated. We'll presumably receive a new
              # `rewrite` post in the future.
              next unless state0.seq == seq.to(Int32)

              state1 = State::Fixpoint::Cycle.new(state0.seq, state0.term)
              states1 = states0.assoc(device_addr.to(Int32), state1)
              instance_.env.submit(states1)

              workspace = Term.entries({state0.source, {:currently, term}})
              instance_.env.send(workspace)
            end
          end

          otherwise do
            Log.warn { "ignoring invalid post: #{ML.compact(post)}" }
          end
        end
      end
    rescue Exit
      Log.info { "received an exit request, stopped the server mainloop" }
    end

    # Constructs and yields a `Hub`, spawning a pair of fibers to bridge between
    # external IOs *source* and *sink* and the internal channel system. *source*
    # is used for listening for requests, and *sink* is used for sending asynchronous
    # responses and notifications.
    #
    # We use the newline character `\n` to delimit requests and responses in
    # *source* and *sink*.
    def hub(source : IO, sink : IO, & : Hub ->) : Nil
      Window.setup do |proof|
        hub = Hub.new(proof)

        # This fiber will listen on *responses*, convert terms it receives to WwML
        # using ML.compact, and finally output them to *sink* (which is most
        # likely STDOUT).
        spawn do
          Log.trace { "response handler: running" }

          while response = hub.responses.receive?
            sink.puts ML.compact(response)
            sink.flush
          end
        ensure
          Log.trace { "response handler: stopped, closing hub" }

          hub.close
        end

        # This fiber will listen on *source* (most likely STDIN), parse ML it
        # receives into terms, and forward to *posts*.
        spawn do
          Log.trace { "source handler: running" }

          while line = source.gets
            begin
              command = ML.term(line)
            rescue e : ML::SyntaxError
              hub.responses.send(Term.of(:log, Term.of(:error, title: "BUG: protocol error", detail: e.detail)))
              next
            end

            hub.posts.send(Term.of(:request, command))
          end
        ensure
          Log.trace { "source handler: stopped, closing hub" }

          hub.close
        end

        begin
          yield hub
        ensure
          hub.close
        end
      end
    end
  end
end
