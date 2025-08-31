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

      def comment(console : Channel(Term), detail : String)
        notify(console, Term.of(:comment, detail))
      end

      def warn(console : Channel(Term), detail : String)
        notify(console, Term.of(:warning, detail))
      end

      def note(console : Channel(Term), detail : String)
        notify(console, Term.of(:note, detail))
      end

      def err(console : Channel(Term), detail : String)
        notify(console, Term.of(:error, detail))
      end

      def narrate(console : Channel(Term), term : Term)
        notify(console, Term.of(:narration, term))
      end

      def notify(console : Channel(Term), term : Term)
        console.send(Term.of(:event, term))
      end
    end

    # :nodoc:
    #
    # Helpers for replying to client requests.
    module Reply
      extend self

      def err(console : Channel(Term), title : String, detail : String?)
        reply(console, Term.of(:error, title: title, detail: detail))
      end

      def reply(console : Channel(Term), item : Term)
        console.send(Term.of(:reply, item))
      end
    end

    # :nodoc:
    module Narrator
      extend self

      record Context, console : Channel(Term), env : Env, device_addr : DeviceAddr, device : Term

      private def narrate(ctx : Context, state0 : State::Window::NotOpen, state1 : State::Window::Open)
        Push.narrate(ctx.console, Term.of(:window, :open, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Window::Open, state1 : State::Window::NotOpen)
        Push.narrate(ctx.console, Term.of(:window, :closed, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Source::None, state1 : State::Source::FilePending)
        Push.narrate(ctx.console, Term.of(:file, :pending, ctx.device_addr, state1.path))
      end

      private def narrate(ctx : Context, state0 : State::Source::FilePending, state1 : State::Source::FileLoaded)
        Push.narrate(ctx.console, Term.of(:file, :loaded, ctx.device_addr, state1.path))
      end

      private def narrate(ctx : Context, state0 : State::Source::FileLoaded, state1 : State::Source::FileLoaded)
        Push.narrate(ctx.console, Term.of(:file, :reloaded, ctx.device_addr, state1.path))
      end

      private def narrate(ctx : Context, state0 : State::Source::FileLoaded | State::Source::FilePending, state1 : State::Source::None)
        Push.narrate(ctx.console, Term.of(:file, :unloaded, ctx.device_addr, state0.path))
      end

      private def narrate(ctx : Context, state0 : State::Ticker::NotRunning, state1 : State::Ticker::Running)
        Push.narrate(ctx.console, Term.of(:ticker, :running, ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Ticker::Running, state1 : State::Ticker::NotRunning)
        Push.narrate(ctx.console, Term.of(:ticker, :"not-running", ctx.device_addr))
      end

      private def narrate(ctx : Context, state0 : State::Image::None | State::Image::File, state1 : State::Image::File)
        Push.narrate(ctx.console, Term.of(:image, :drawn, ctx.device_addr, state1.path))
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

        Push.notify(ctx.console,
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
          Push.notify(ctx.console,
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
          Push.notify(ctx.console,
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
          Push.notify(ctx.console,
            Term.of(:error, :microfold,
              addr: ctx.device_addr,
              path: path,
              detail: issue.detail,
              severity: issue.severity))
        end
      end

      private def narrate(ctx : Context, state0 : State::Source::BadQuery, state1 : State::Source::None)
        Push.notify(ctx.console,
          Term.of(:error, :source,
            addr: ctx.device_addr,
            detail: "bad source query",
            code: ML.compact(state0.query)))
      end

      private def narrate(ctx : Context, state0 : State::Ticker::BadPeriodSpec, state1 : State::Ticker::None)
        Push.notify(ctx.console,
          Term.of(:error, :ticker,
            addr: ctx.device_addr,
            detail: "bad period specification",
            code: ML.compact(state0.spec)))
      end

      private def narrate(ctx : Context, state0 : State::Image::BadSpec, state1 : State::Image::None)
        Push.notify(ctx.console,
          Term.of(:error, :image,
            addr: ctx.device_addr,
            detail: "bad image specification",
            code: ML.compact(state0.spec)))
      end

      private def narrate(ctx : Context, state0 : State::Image::BadTarget, state1 : State::Image::None)
        Push.notify(ctx.console,
          Term.of(:error, :image,
            addr: ctx.device_addr,
            detail: "bad image target specification",
            code: ML.compact(state0.target)))
      end

      private def narrate(ctx : Context, state0 : State::Log::Any, state1 : State::Log::Message)
        case state1.style
        in .comment? then Push.comment(ctx.console, ML.compact(state1.term))
        in .note?    then Push.note(ctx.console, ML.compact(state1.term))
        in .warning? then Push.warn(ctx.console, ML.compact(state1.term))
        in .error?   then Push.err(ctx.console, ML.compact(state1.term))
        end
      end

      # Ignore all other state transitions.
      private def narrate(ctx : Context, state0, state1)
      end

      # Constructs a server narrator agent that outputs narrations, events, logs,
      # etc. into *console*.
      def agent(console : Channel(Term)) : Agent::Narrator
        Agent::Narrator.new do |env, device_addr, device, state0, state1|
          narrate(Context.new(console, env, device_addr, device), state0, state1)
        end
      end
    end

    # Represents the configuration of a server. Prefer to obtain this object from
    # `conf` instead of constructing it by hand.
    defcase Conf,
      basis : Term::Dict,
      mki : Path, Term -> Instance::Some,
      files : FileServer,
      frametime : Time::Span,
      fstime : Time::Span

    # Extended constructor for a server configuration.
    #
    # - *frametime* defines ideal duration of a single frame.
    # - *fstime* defines ideal file system polling period.
    def conf(
      hub : Hub,
      basis : Term::Dict,
      uir_base : Term,
      compositor : D::Compositor,
      platform : D::Platform,
      *,
      frametime : Time::Span = 1.second / 60,
      fstime : Time::Span = 500.milliseconds,
    ) : Conf
      files = platform.files

      viewer_context = D::Viewer::Context.new(compositor, platform)
      window_context = D::Window.context(viewer_context)

      mki = ->(path : Path, rack : Term) do
        wm = Rack::WM.new

        wmon = wm.poll
        fsmon = Rack::FS.monitor(files)

        agents = [
          Narrator.agent(hub.responses),
          Rack.uir(platform, uir_base),
          Rack::Image.file_snapper(viewer_context),
          Rack.scheduler { |period, query| tick(period, query, hub.workspaces) },
          wm.sync(window_context),
        ]

        env, unload = Rack.env(rack, basis, agents)

        Instance::Some.new(path, wmon, fsmon, env, unload)
      end

      Conf.new(basis, mki, files, frametime, fstime)
    end

    # Shorthand constructor for a server configuration.
    def conf(hub : Hub, files : FileServer, basis_path : Path, uir_path : Path, **kwargs) : Conf
      basis = ML.document(files.read_string(basis_path))
      uir_base = ML.document(files.read_string(uir_path))

      conf(hub, basis.as_d, uir_base, D::Compositor.new, D::PvgPlatform.new(files), **kwargs)
    end

    # Represents a rack instance: the set of handles associated with an initialized
    # rack environment.
    module Instance
      alias Any = Locked | Unlocked
      alias Unlocked = Some | Nil

      record Some, path : Path, wmon : Client, fsmon : Client, env : Env, unload : (->)
      record Locked, unlocked : Unlocked
    end

    private def load(conf : Conf, instance : Instance::Unlocked, path : Path, responses : Channel(Term)) : Instance::Unlocked
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
        ("workspace query @edge_" "Queries the rack about the value of computed <edge>")
        ("workspace assign @edge_ value_" "Updates the value of computed <edge> to <value>, prints the resulting workspace")))
    WWML

    # Handles a single request.
    private def handle(conf : Conf, instance : Instance::Unlocked, request : Term, responses : Channel(Term)) : Instance::Any
      Term.case(request) do
        # The idea is for the client to send `lock` before sending anything else.
        # The server responds with `locked`. The client responds with whatever it
        # wanted to send in the first place. This way, the server can postpone
        # expensive operations to remain reachable; and the client will know whether
        # the server is busy by assuming the time after `lock` and until `locked`
        # as busy.
        matchpi %{lock} do
          responses.send(Term.of(:locked))

          Instance::Locked.new(instance)
        end

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
            next unless state.is_a?(State::Window::Closed)
            next unless device_addr == addr.to(Int32)

            State::Window::Open.new(state.id, state.spec, state.events)
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
            next unless state.is_a?(Rack::State::Window::Open)
            next unless device_addr == addr.to(Int32)

            State::Window::Closed.new(state.id, state.spec, state.events)
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

        matchpi %{(workspace query @edge_)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          env = instance.env
          workspace = env.send(Term.entries({edge, :"?"}))

          Reply.reply(responses, Term.of(:term, workspace[edge]))

          instance
        end

        matchpi %{(workspace assign @edge_ value_)} do
          unless instance
            Reply.err(responses, "control error", "rack not loaded")
            return instance
          end

          env = instance.env
          workspace = env.send(Term.entries({edge, {:currently, value}}))

          Reply.reply(responses, Term.of(:workspace, workspace))

          instance
        end

        matchpi %{(help)} do
          Reply.reply(responses, Term.of(:help, HELP))

          instance
        end

        matchpi %{(exit)} do
          raise Exit.new
        end

        otherwise do
          Reply.err(responses, "invalid request", "invalid request `#{ML.compact(request)}`")

          instance # On error, we keep the instance unchanged.
        end
      end
    end

    # Unlock a locked instance on request. The idea with locking an instance is that
    # we block all processes that want to touch it until a request arrives. Now is
    # that time, so unlock. Note that the other processes will still have to wait
    # until we handle *request* before they have a chance at touching whatever
    # instance comes out in the end.
    private def handle(conf : Conf, instance : Instance::Locked, request : Term, responses : Channel(Term)) : Instance::Any
      handle(conf, instance.unlocked, request, responses)
    end

    # Spawns the tick fiber containing the tick loop. The tick loop
    # will send *query* to *queries* for every *period*.
    private def tick(period : Time::Span, query : Term::Dict, queries : Channel(Term::Dict)) : (->)
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
              when queries.send(query)
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
    record Hub,
      requests = Channel(Term).new,
      responses = Channel(Term).new,
      workspaces = Channel(Term::Dict).new

    struct Hub
      def close : Nil
        requests.close
        responses.close
        workspaces.close
      end
    end

    # :nodoc:
    #
    # Raised on a polite exit request.
    class Exit < Exception
    end

    # Runs the server mainloop, handling requests coming from *requests* and
    # sending responding to or notifying the client through *responses*.
    # Additionally, accepts workspaces/workspace queries on *workspaces*,
    # which acts as a sink for e.g. tickers.
    def serve(conf : Conf, hub : Hub) : Nil
      instance = nil

      fstick = Channel(Bool).new
      wmtick = Channel(Bool).new

      spawn do
        Log.trace { "wmtick: running" }

        loop do
          wmtick.send(true)

          dt = Time.measure { wmtick.receive }
          nap = conf.frametime - dt
          if nap.positive?
            sleep nap
          end
        end
      rescue e : Channel::ClosedError
        Log.trace { "wmtick: channel was closed" }
      ensure
        Log.trace { "wmtick: stopped" }

        wmtick.close
      end

      spawn do
        Log.trace { "fstick: running" }

        loop do
          fstick.send(true)
          fstick.receive

          # We probably don't want to do anything smart or adaptive here, do we?
          sleep(conf.fstime)
        end
      rescue e : Channel::ClosedError
        Log.trace { "fstick: channel was closed" }
      ensure
        Log.trace { "fstick: stopped" }

        fstick.close
      end

      begin
        loop do
          select
          when wmtick.receive
            begin
              next unless instance.is_a?(Instance::Some)

              instance.wmon.call(instance.env)
            ensure
              wmtick.send(true)
            end
          when fstick.receive
            begin
              next unless instance.is_a?(Instance::Some)

              instance.fsmon.call(instance.env)
            ensure
              fstick.send(true)
            end
          when request = hub.requests.receive?
            break unless request

            instance = handle(conf, instance, request, hub.responses)
          when workspace = hub.workspaces.receive
            next unless instance.is_a?(Instance::Some)

            env = instance.env
            env.send(workspace)
          end
        end
      rescue Exit
        Log.info { "received an exit request, stopped the server mainloop" }
      ensure
        fstick.close
        wmtick.close
      end
    ensure
      hub.close
    end

    # Constructs and yields a `Hub`, spawning a pair of fibers to bridge between
    # external IOs *source* and *sink* and the internal channel system. *source*
    # is used for listening for requests, and *sink* is used for sending asynchronous
    # responses and notifications.
    #
    # We use the newline character `\n` to delimit requests and responses in
    # *source* and *sink*.
    def hub(source : IO, sink : IO, & : Hub ->) : Nil
      hub = Hub.new

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
      # receives into terms, and forward to *requests*.
      spawn do
        Log.trace { "source handler: running" }

        while line = source.gets
          begin
            command = ML.term(line)
          rescue e : ML::SyntaxError
            hub.responses.send(Term.of(:log, Term.of(:error, title: "BUG: protocol error", detail: e.detail)))
            next
          end

          hub.requests.send(command)
        end
      ensure
        Log.trace { "source handler: stopped, closing hub" }

        hub.close
      end

      yield hub
    end
  end
end
