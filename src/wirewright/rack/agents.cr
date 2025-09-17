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

  # An agent that manages a rewriter of the given *kind*.
  def rewriter(kind : State::Rewriter::Kind, rewriter : Rewriter)
    Agent::Peer.new do |states0, _|
      proposals = [] of Term
      states1 = states0.map(State::Rewriter::Pending) do |_, state0|
        next unless state0.kind == kind

        output = rewrite(state0.input, rewriter)
        proposals << Term.of(:proposal, state0.dst, {:currently, output})

        State::Rewriter::None.new(state0.kind)
      end

      {states1, proposals}
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

  # File system agency.
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
  end
end
