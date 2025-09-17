module Ww::Rack
  # Rack imaging agency.
  #
  # `dwuir/image` devices depend on this agency's presence. They are
  # not going to function otherwise.
  module Image
    extend self

    # Constructs a narrator agent that draws DwUIR images from observed specs,
    # capturing and storing them on disk using a selected format (e.g. PPM, PNG, JPEG).
    #
    # *ctx* is a viewer context that should be used in the process of
    # drawing an image (see for example `Soma::DwUIR.snap`).
    def file_snapper(ctx : DwUIR::Viewer::Context) : Agent::Narrator
      Agent::Narrator.new do |_, _, _, state0, state1|
        case {state0, state1}
        when {_, State::Image::File}
          begin
            DwUIR.snap(ctx, state1.conf, state1.path)
          rescue e : DwUIR::SnapError
            Log.error(exception: e) { e.message }
          end
        end
      end
    end

    # Constructs a narrator agent that draws DwUIR image(s) with the given *id*
    # from observed specs, calling *fn* with their resulting pixel rect(s).
    def slot(ctx : DwUIR::Viewer::Context, id : Term, &fn : DwUIR::PixelRect ->) : Agent::Narrator
      Agent::Narrator.new do |_, _, _, state0, state1|
        case {state0, state1}
        when {_, State::Image::InMemory}
          next unless state1.id == id

          image = DwUIR.show(ctx, state1.conf)
          fn.call(image)
        end
      end
    end
  end

  # Constructs a narrator agent that schedules periodic ticking using *fn*.
  #
  # - The first argument of *fn* is period (e.g. every `100.milliseconds`).
  # - The second argument is a query dict that should be sent (`Env#send`)
  #   to the environment to execute a tick.
  #
  # `ticker` devices depend on this agent's presence (otherwise, they are
  # going to be ignored).
  def scheduler(&fn : Time::Span, Term::Dict -> (->)) : Agent::Narrator
    cancels = {} of Int32 => (->)

    Agent::Narrator.new do |_, _, _, state0, state1|
      case {state0, state1}
      when {State::Ticker::Running, State::Ticker::NotRunning}
        next unless cancel = cancels.delete(state0.id)

        cancel.call
      when {State::Ticker::NotRunning, State::Ticker::Running}
        cancels[state1.id] = fn.call(state1.period, state1.query)
      when {State::Ticker::Running, State::Ticker::Running}
        # TODO: maybe we should tell the thing to change period instead of
        # throwing it away?
        if cancel = cancels.delete(state0.id)
          cancel.call
        end

        cancels[state1.id] = fn.call(state1.period, state1.query)
      end
    end
  end

  # Constructs an agent that finds and handles requests for UIR rewriting using
  # the uiR rewriter, with metrics set to `graphics`.
  #
  # TODO: this is a hack. uiR is no different from any other *rewriter circuit*,
  # but we do not have them implemented at the moment.
  def uir_graphics(platform : DwUIR::Platform, rulebase : Term) : Agent::Peer
    uiR = Soma.uiR(
      replier: ->(term : Term) { DwUIR.reply(platform, term) },
      rulebase: rulebase,
    )

    uir(uiR, :graphics)
  end

  def uir_text(rulebase : Term) : Agent::Peer
    uiR = Soma.uiR(
      replier: ->(term : Term) { DwUIR::Textual.reply(term) },
      rulebase: rulebase,
    )

    uir(uiR, :text)
  end

  private def uir(uiR : Rewriter, metrics : State::UIR::Metrics)
    Agent::Peer.new do |states0, _|
      proposals = [] of Term

      states1 = states0.map(State::UIR::Pending) do |_, state0|
        next unless state0.metrics == metrics

        dwuir = rewrite(state0.uir, uiR)
        proposals << Term.of(:proposal, state0.dst, {:currently, dwuir})

        State::UIR::None.new
      end

      {states1, proposals}
    end
  end

  # Constructs an agent that finds and handles requests for process arguments
  # and environment.
  #
  # `args` and `env` devices depend on this agent's presence (otherwise they
  # are going to be ignored)
  def process(*, args : Array(String), env : Hash(String, String)) : Agent::Provider
    Agent::Provider.new do |states|
      states.map do |state0|
        case state0
        when State::ProcessArgs::None
          State::ProcessArgs::Some.new(args)
        when State::ProcessEnv::None
          State::ProcessEnv::Some.new(env)
        end
      end
    end
  end

  # Rack SDL window management agency.
  #
  # `dwuir/window` devices depend on this agency's presence. Otherwise they
  # are going to function partially (if parts of this agency are active) or
  # not function at all (if this agency is missing entirely).
  struct SDLWM
    private alias Window = DwUIR::Window::SDL

    def initialize
      @windows = {} of Int32 => Window::Any
    end

    # Constructs an environment client that performs periodic event polling;
    # perturbing the environment to handle some events, and handling others
    # completely by itself. Namely, window closure events are handled by this
    # client, triggering the transition from `State::Window::Open` to
    # `State::Window::Closed`.
    def poll : Client
      Client.new do |env|
        open_windows = @windows
          .each_value
          .select(Window::Some)
          .to_set

        # Handle input events.
        survived_windows = Window.poll(open_windows) do |target, event|
          env.each(State::Window::Open) do |_, state|
            next unless events = state.events
            next unless window = @windows[state.id]?
            next unless window == target

            query = Term.entries({events, {:currently, event}})
            env.send(query)
          end
        end

        # Handle window closure.
        env.map(State::Window::Open) do |device_addr, state|
          window = @windows[state.id]

          if window.in?(open_windows) && !window.in?(survived_windows)
            State::Window::Closed.new(state.id, state.spec, state.events)
          end
        end
      end
    end

    # Constructs an agent that performs window synchronization of internal window
    # specs with actual OS windows. This is the agent that opens and closes windows,
    # presents their content on spec change, etc.
    def sync(ctx : Window::Context) : Agent::Narrator
      Agent::Narrator.new do |_, _, _, state0, state1|
        case {state0, state1}
        when {State::Window::Open, State::Window::NotOpen}
          next unless window0 = @windows.delete(state0.id)

          window1 = Window.next(ctx, window0, spec: nil)
          assert window1.is_a?(Window::None)
        when {State::Window::Any, State::Window::Open}
          window0 = @windows[state1.id]? || Window::None.new
          window1 = Window.next(ctx, window0, state1.spec)
          Window.present(window1)

          @windows[state1.id] = window1
        end
      end
    end
  end

  # Terminal window management agency.
  #
  # We distinguish *terminals* from *consoles* from *windows*. A *window* is
  # the content of a console. A *terminal* can show multiple *consoles* through
  # multiplexing, or if it does not support multiplexing, it will show only
  # one console.
  #
  # `dwuir/console` devices depend on this agency's presence. Otherwise they
  # are going to function partially (if parts of this agency are active) or
  # not function at all (if this agency is missing entirely).
  struct TWM
    def initialize
      @events = {} of Int32 => Term
    end

    # Sends *event* to an events edge owned by a console with the given *id*.
    def send(env : Env, id : Int32, event : Term)
      return unless edge = @events[id]?

      env.send(Term.entries({edge, {:currently, event}}))
    end

    # Constructs an agent that performs window synchronization of internal window
    # specs with the terminal.
    #
    # *specs* is used to send console id, window spec pairs to the terminal. Window
    # spec is nil if the window should be closed.
    def sync(&specs : Int32, Term? ->) : Agent::Narrator
      Agent::Narrator.new do |_, _, _, state0, state1|
        case {state0, state1}
        when {State::Console::Open, State::Console::NotOpen}
          next unless @events.delete(state0.props.id)

          specs.call(state0.props.id, nil)
        when {State::Console::Any, State::Console::Open}
          if edge = state1.props.events
            @events[state1.props.id] = edge
          end

          specs.call(state1.props.id, state1.spec)
        end
      end
    end
  end

  # File system agents.
  module FS
    extend self

    # Constructs an agent that finds pending files in the state map, and proposes
    # their content to the workspace (caching it in the state map for future reference).
    def server(files : FileServer) : Agent::Peer
      Agent::Peer.new do |states0, index|
        proposals = [] of Term

        states1 = states0.map(State::Source::FilePending) do |device_addr, state0|
          device = index.device(device_addr)

          begin
            content = files.read_string(state0.path)
          rescue e : FileServerError
            Log.debug(exception: e) { "could not read file #{state0.path}" }
            next
          end

          proposals << Term.of(:proposal, state0.dst, {:currently, content})

          State::Source::FileLoaded.new(state0.path, state0.dst, content, instant: Time.local)
        end

        {states1, proposals}
      end
    end

    # Constructs an environment client that performs a watch step for file-
    # backed `src` devices against *files*. This client will send appropriate
    # queries to the environment when a file dependency is created, removed,
    # or modified.
    def monitor(files : FileServer) : Client
      Client.new do |env|
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

        next if alert.empty?

        query = Term[]

        alert.each do |device_addr|
          state0, state1 = states0[device_addr], states1[device_addr]

          case {state0, state1}
          when {State::Source::FileLoaded, State::Source::FilePending}
            # Removed
            query = query.with(state1.dst, :"?")
          when {State::Source::FileLoaded, State::Source::FileLoaded}, # Modified
               {State::Source::FilePending, State::Source::FileLoaded} # Created
            query = query.with(state1.dst, {:currently, state1.content})
          end
        end

        env.send(query)
      end
    end
  end
end
