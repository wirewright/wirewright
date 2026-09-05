module MuSoma
  # Crystal-side representation of the MuSoma codex, usually located at
  # `runtime/codices/musoma.codex.wwml`.
  class Codex
    getter theme : Term::Dict
    getter initial_state : Term::Dict
    getter pretty : Pretty
    getter stateR : Rho::Rewriter
    getter fbR : Rho::Rewriter
    getter app : App
    getter history_limit : UInt32 # TODO: Codex::Config#history_limit

    # :nodoc:
    def initialize(@theme, @initial_state, @pretty, @stateR, @fbR, @app, @history_limit)
    end

    def self.new(codex : Term::Dict) : Codex
      theme = theme(codex)
      initial_state = codex[:"initial-state"]?.as_d? || Term[]
      pretty = Pretty.new(codex)
      stateR = Rho.rewriter(Term.of(codex), section: Term.of(:stateR))
      fbR = Rho.rewriter(Term.of(codex), section: Term.of(:fbR))
      app = App.new(codex)

      history_limit = 128u32
      pass do
        next unless setting = codex[:settings, :"history-limit"]?
        next unless setting = setting.as_n?
        next unless setting = setting.to?(UInt32)

        history_limit = setting
      end

      new(theme, initial_state, pretty, stateR, fbR, app, history_limit)
    end

    private def self.theme(codex : Term::Dict) : Term::Dict
      config = codex[:theme]?.as_d? || Term[]
      config.items.reduce(config.pairspart) do |memo, stmt|
        Term.case(stmt) do
          matchpi %{(include section_)} do
            vars = Term[]

            body = codex[section]?.as_d? || Term[]
            body.each_entry do |key, value|
              Term.matchpi?(key, %{(var var_)}) do
                vars = vars.with(var, value)
                body = body.without(key)
              end
            end

            patch = Alloy.render(vars, Term.of(body))

            Term.merge(memo, patch.as_d)
          end

          otherwise do
            memo
          end
        end
      end
    end
  end

  class Codex::Pretty
    # :nodoc:
    def initialize(@sema : Alloy::Rewriter, @prettyR : Rho::Rewriter)
      @render_cache = GenerationalCache(Alloy::RenderKey, Term::Rep).new
      @rho_cache = GenerationalCache({Rho::RewriterId, Term}, Term::Rep).new
    end

    def self.new(codex : Term::Dict) : Pretty
      sema = Alloy.rewriter(codex[:"pretty/sema"]? || Term.of)
      prettyR = Rho.rewriter(Term.of(codex), section: Term.of(:prettyR))

      new(sema, prettyR)
    end

    # Converts a circuit *repr* (see `MuSoma.repr`) to Microfold.
    def rewrite(repr : Term) : Term
      sema_out = @render_cache.epoch do
        Alloy.rewrite(@sema, repr, cache: @render_cache)
      end

      Rho.rewrite(@prettyR, sema_out, cache: @rho_cache)
    end
  end

  class Codex::App
    # :nodoc:
    def initialize(@template : Alloy::CompiledTemplate)
      @cache = GenerationalCache(Alloy::RenderKey, Term::Rep).new
    end

    def self.new(codex : Term::Dict) : App
      template = Alloy.compile(Alloy.sheet(codex[:app]? || Term.of))

      new(template)
    end

    # Computes the UI of the app (Microfold) from the current *state*.
    def render(state : Term::Dict) : Term
      render = @cache.epoch do
        Alloy.render(@template, globals: state, cache: @cache)
      end

      unless render = render.as_d?
        return Term.of
      end

      render.items.last? || Term.of
    end
  end

  def codex(document : Term) : Codex
    codex(document.as_d? || Term[])
  end

  def codex(document : Term::Dict) : Codex
    Codex.new(document)
  end
end
