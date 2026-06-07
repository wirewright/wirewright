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
    getter history_limit : Int32 # TODO: Codex::Config#history_limit

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

      history_limit = 128
      pass do
        next unless setting = codex[:settings, :"history-limit"]?
        next unless setting = setting.as_n?
        next unless setting = setting.to?(Int32)

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
    def initialize(@sema_old : Ruleset, @sema : Alloy2::Rewriter, @prettyR : Rho::Rewriter)
      @render_cache = GenerationalCache(Alloy2::RenderKey, Term::Rep).new
      @rho_cache = GenerationalCache({Rho::RewriterId, Term}, Term::Rep).new
    end

    def self.new(codex : Term::Dict) : Pretty
      sema = Alloy2.rewriter(codex[:"pretty/sema"]? || Term.of)
      sema_old = Ruleset.select(Ruleset::DEFAULT_SELECTOR, codex[:"pretty/sema"]? || Term.of)
      prettyR = Rho.rewriter(Term.of(codex), section: Term.of(:prettyR))

      new(sema_old, sema, prettyR)
    end

    # Converts a circuit *repr* (see `MuSoma.repr`) to Microfold.
    def rewrite(repr : Term) : Term
      sema_out_new = @render_cache.epoch do
        Alloy2.rewrite(@sema, repr, cache: @render_cache)
      end

      # sema_out_old = Alloy.compose(@sema_old, Term[], Alloy.component(repr))

      # unless sema_out_old == sema_out_new
      #   puts ML.display(repr)
      #   puts ML.display(sema_out_old)
      #   puts ML.display(sema_out_new)
      #   raise ""
      # end

      Rho.rewrite(@prettyR, sema_out_new, cache: @rho_cache)
    end
  end

  class Codex::App
    # :nodoc:
    def initialize(@ruleset : Ruleset, @template_old : Term, @template : Alloy2::CompiledTemplate)
      @cache = GenerationalCache(Alloy2::RenderKey, Term::Rep).new
    end

    def self.new(codex : Term::Dict) : App
      ruleset, rest = Ruleset.ruleset_and_rest(Ruleset::DEFAULT_SELECTOR, codex[:app]? || Term.of)
      template_old = rest.items.first? || Term.of
      template = Alloy2.compile(Alloy2.sheet(codex[:app]? || Term.of))

      new(ruleset, template_old, template)
    end

    # Computes the UI of the app (Microfold) from the current *state*.
    def render(state : Term::Dict) : Term
      # render_old = Alloy.compose(@ruleset, state, Alloy.template(Term[], @template_old))
      render_new = @cache.epoch do
        Alloy2.render(@template, globals: state, cache: @cache)
      end

      render_new = render_new.items.last # ?!

      # unless render_old == render_new
      #   puts ML.display(render_old)
      #   puts ML.display(render_new)
      #   raise ""
      # end

      render_new
    end
  end

  def codex(document : Term) : Codex
    codex(document.as_d? || Term[])
  end

  def codex(document : Term::Dict) : Codex
    Codex.new(document)
  end
end
