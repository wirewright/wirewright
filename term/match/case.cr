# TODO: missing feature: explicit cue, cues support

struct Ww::Term
  struct CaseContext(Engine)
    # See `continue`.
    module Continue
    end

    # :nodoc:
    def initialize(@matchee : Term)
    end

    # :nodoc:
    # 
    # WARNING: only access at macro time! Otherwise the world will explode! ¯\_(ツ)_/¯
    PATTERN_TERM_ID = [0u32]

    # :nodoc:
    PATTERN_TERM_CACHE = Pf::Cache(UInt32, Term).new

    # :nodoc:
    def match?(pid : UInt32, pattern : -> Term, cue cues = Tuple.new, default = nil, &)
      term = PATTERN_TERM_CACHE.fetch(pid, &pattern)
      if (dict = term.as_d?) && !cues.all? { |cue| dict.probably_includes?(Term[cue]) }
        return default
      end
      unless env = Engine.match?(term, @matchee)
        return default
      end
      yield env
    end

    # Expands to `next` with a special sentinel module, `Continue`, that indicates
    # to the control flow logic that review of the current case must be stopped
    # immediately and it should switch to the next case (fallthrough).
    #
    # You can return `Continue` manually if you for some reason dislike the `next`.
    #
    # ```
    # Term.case(term) do
    #   givenpi %[x_ y_] do
    #     continue unless good?(x)
    #     continue unless good?(y)
    #     puts({x, y})
    #   end
    # end
    # ```
    macro continue
      next ::Ww::Term::CaseContext::Continue
    end

    # :nodoc:
    macro match!(pattern, *, cue = nil, icaps = [] of ::NoReturn, location = nil, &block)
      {% location ||= "#{block.filename.id}:#{block.line_number}:#{block.column_number}" %}
      {% pid = ::Ww::Term::CaseContext::PATTERN_TERM_ID[0] %}
      {% ::Ww::Term::CaseContext::PATTERN_TERM_ID[0] += 1 %}

      %result = match?({{pid}}, {{pattern}}, cue: { {{cue}} }.flatten.compact, default: ::Ww::Term::CaseContext::Continue) do |%env|
        {% for icap in icaps %}
          {% unless block.args.any? { |arg| arg.id == icap.id } %}
            {{icap.id}} = (%env[{{icap.id.symbolize}}]? || raise "case: #{ {{location}} }: missing capture '{{icap.id}}'")
          {% end %}
        {% end %}

        pass(
          {% for capture in block.args %}
            (%env[{{capture.id.symbolize}}]? || raise "case: #{ {{location}} }: missing capture '{{capture.id}}'"),
          {% end %}
        ) {{block}}
      end

      unless %result.is_a?(::Ww::Term::CaseContext::Continue.class)
        # This macro is expanded with a .case, as in:
        # 
        #   X.case(...) do
        #     ...
        #     ⏏ match(...) { ... }
        #     ...
        #   end
        #
        # We will thus get:
        #
        #   X.case(...) do
        #     ...
        #     break %result ⏏
        #     ...
        #   end
        break %result
      end
    end

    macro match(pattern, **kwargs, &block)
      match!(->{ Term.of({{pattern}}) }, {{kwargs.double_splat}}) {{block}}
    end

    # Same as `match` but constructs terms by parsing string *ml* using `Ww::ML`,
    # instead of constructing terms using Crystal (which is rather inconvenient
    # but sometimes necessary due to e.g. circularity issues).
    macro matchp(ml, **kwargs, &block)
      {% location = "#{ml.filename.id}:#{ml.line_number}:#{ml.column_number}" %}

      match!(->{ ::Ww::ML.parse1({{ml}}) }, location: {{location}}, {{kwargs.double_splat}}) {{block}}
    end

    RE_CAPTURES = /([a-zA-Z_]\w*?)(?:_(?:any|number|symbol|string|boolean|dict)?[+*⋮]?|←|⋮)|\((?:%let)\s+([a-zA-Z]\w*)/

    # `matchp` that can infer basic captures (such as `x_`) from *ml* source
    # at compile-time.
    #
    # The supported kinds of captures are described by `RE_CAPTURES`.
    macro matchpi(ml, **kwargs, &block)
      {% captures = ml.scan(::Ww::Term::CaseContext::RE_CAPTURES).map { |match| (match[1] || match[2]).id }.uniq %}
      {% location = "#{ml.filename.id}:#{ml.line_number}:#{ml.column_number}" %}

      match!(-> { ::Ww::ML.parse1({{ml}}) }, icaps: [{{captures.splat}}] of ::NoReturn, location: {{location}}, {{kwargs.double_splat}}) {{block}}
    end

    # Same as `match`, but guarantees to constructs a dictionary with the given
    # *items* and *pairs*.
    macro given(*items, **pairs, &block)
      {% location = "#{block.filename.id}:#{block.line_number}:#{block.column_number}" %}

      match!(-> { Term.dict({{items.splat}}, {{pairs.double_splat}}) }, location: {{location}}, {{kwargs.double_splat}}) {{block}}
    end

    # Same as `matchp`, but *ml* is treated like a top-level dict (without parens).
    macro givenp(ml, &block)
      {% location = "#{ml.filename.id}:#{ml.line_number}:#{ml.column_number}" %}

      match!(-> { ::Ww::ML.parse({{ml}}) }, location: {{location}}) {{block}}
    end

    # `givenp` that can infer basic captures (such as `x_`) from *ml* source
    # at compile-time.
    #
    # See `matchpi` for more info.
    macro givenpi(ml, &block)
      {% captures = ml.scan(::Ww::Term::CaseContext::RE_CAPTURES).map { |match| (match[1] || match[2]).id }.uniq %}
      {% location = "#{ml.filename.id}:#{ml.line_number}:#{ml.column_number}" %}

      match!(-> { ML.parse({{ml}}) }, icaps: [{{captures.splat}}] of ::NoReturn, location: {{location}}) {{block}}
    end

    {% for name in %w(match matchp matchpi given givenp givenpi) %}
      # Expands to multiple `{{name.id}}`es with different arguments but the same *block*.
      macro {{name.id}}(*args, cue = nil, cues = nil, **kwargs, &block)
        \{% for arg, index in args %}
          {{name.id}}(\{{arg}}, cue: { \{{cue}}, \{{cues ? cues[index] : nil}} }, \{{kwargs.double_splat}}) \{{block}}
        \{% end %}
      end
    {% end %}

    # Catch-all case.
    macro otherwise(&)
      # This macro is expanded with a .case, as in:
      # 
      #   X.case(...) do
      #     ...
      #     ⏏ otherwise { ... }
      #     ...
      #   end
      #
      # We will thus get:
      #
      #   X.case(...) do
      #     ...
      #     break begin ... end ⏏
      #     ...
      #   end
      break {{yield}}
    end
  end
end

