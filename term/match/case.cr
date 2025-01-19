struct Ww::Term
  struct CaseContext(Engine)
    # See `continue`.
    module Continue
    end

    # :nodoc:
    def initialize(@matchee : Term, @stats : CaseStatistics? = nil)
    end

    # :nodoc:
    # 
    # WARNING: only access at macro time! Otherwise the world will explode! ¯\_(ツ)_/¯
    PATTERN_TERM_ID = [0u32]

    # :nodoc:
    PATTERN_TERM_CACHE = Pf::Cache(UInt32, Term).new

    # :nodoc:
    def match?(pid : UInt32, pattern : -> Term, cue cues = Tuple.new, default = nil, &)
      if (mdict = @matchee.as_d?) && !cues.all? { |cue| mdict.probably_includes?(Term[cue]) }
        @stats.try &.rejected_by_cue

        return default
      end

      pterm = PATTERN_TERM_CACHE.fetch(pid, &pattern)

      unless env = Engine.match?(pterm, @matchee)
        @stats.try &.rejected(pterm)

        return default
      end

      @stats.try &.accepted(pterm)

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
    macro givenp(ml, **kwargs, &block)
      {% location = "#{ml.filename.id}:#{ml.line_number}:#{ml.column_number}" %}

      match!(-> { ::Ww::ML.parse({{ml}}) }, location: {{location}}, {{kwargs.double_splat}}) {{block}}
    end

    # `givenp` that can infer basic captures (such as `x_`) from *ml* source
    # at compile-time.
    #
    # See `matchpi` for more info.
    macro givenpi(ml, **kwargs, &block)
      {% captures = ml.scan(::Ww::Term::CaseContext::RE_CAPTURES).map { |match| (match[1] || match[2]).id }.uniq %}
      {% location = "#{ml.filename.id}:#{ml.line_number}:#{ml.column_number}" %}

      match!(-> { ML.parse({{ml}}) }, icaps: [{{captures.splat}}] of ::NoReturn, location: {{location}}, {{kwargs.double_splat}}) {{block}}
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

  # A lightweight object used to store statistics for Term.case calls. Very internal.
  # Used to determine the best order of cases based on the accesses in practice for
  # better performance (especially with M0 where rejections aren't as cheap as in M1).
  class CaseStatistics
    def initialize
      @acceptions = {} of Term => UInt32
      @rejections = {} of Term => UInt32
      @rejections_by_cue = 0u32
      @lock = Mutex.new
    end

    def accepted(pattern : Term) : Nil
      @lock.synchronize do
        @acceptions.update(pattern, 0) { |tally| tally + 1 }
      end
    end

    def rejected(pattern : Term) : Nil
      @lock.synchronize do
        @rejections.update(pattern, 0) { |tally| tally + 1 }
      end
    end

    def rejected_by_cue : Nil
      @lock.synchronize do
        @rejections_by_cue += 1
      end
    end

    def show(io)
      @lock.synchronize do
        pos = @acceptions.to_a.sort_by! { |_, tally| -tally.to_i64 }
        neg = @rejections.to_a.sort_by! { |_, tally| -tally.to_i64 }

        # I'm bad at math. Is it?
        io.puts "[OPTIMAL ORDERING]"

        ratios = {} of Term => Float32

        @acceptions.each do |pattern, p|
          unless q = @rejections[pattern]?
            ratios[pattern] = Float32::INFINITY
            next
          end
          ratios[pattern] = p.to_f32 / (p + q)
        end

        @rejections.each do |pattern, _|
          next if ratios.has_key?(pattern)
          ratios[pattern] = -Float32::INFINITY
        end

        ratios_sorted = ratios.to_a.sort_by! { |_, ratio| -ratio }
        ratios_sorted.each do |pattern, ratio|
          io.puts "#{ratio} - #{pattern}"
        end

        io.puts
        io.puts "[ACCEPTIONS, DESCENDING]"

        pos.each do |pattern, tally|
          tally.format(io)
          io << " - " << pattern
          io.puts
        end

        io.puts "[REJECTIONS, DESCENDING]"

        neg.each do |pattern, tally|
          tally.format(io)
          io << " - " << pattern
          io.puts
        end

        io.puts "[REJECTIONS BY CUE]"

        @rejections_by_cue.format(io)
        io.puts

        io.puts "[TOTAL]"

        total_pos = pos.sum { |_, tally| tally }
        total_neg = neg.sum { |_, tally| tally } + @rejections_by_cue

        io.puts "    Matches: #{total_pos + total_neg}"
        io.puts " Acceptions: #{total_pos}"
        io.puts " Rejections: #{total_neg}"
        io.puts "Cue success: #{((@rejections_by_cue/total_neg) * 100).round(2)}%"

        io.puts
      end
    end
  end
end

