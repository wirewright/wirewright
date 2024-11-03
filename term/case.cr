module Ww
  abstract struct Term
    class PatternCache
      # :nodoc:
      class SlotMap
        EMPTY = new(map: Pf::Map(Int32, Term::Match::Pattern).new)

        protected def initialize(@map : Pf::Map(Int32, Term::Match::Pattern))
        end

        def self.[] : SlotMap
          EMPTY
        end

        private def_change

        def present? : Bool
          !same?(EMPTY)
        end

        def [](index : Int32) : Term::Match::Pattern
          @map[index]
        end

        def fill(&) : SlotMap
          if present?
            raise ArgumentError.new
          end

          change(map: @map.transaction { |commit| yield commit })
        end
      end

      def initialize
        @slots = Atomic(SlotMap).new(SlotMap[])
      end

      # :nodoc:
      #
      # Every thread proposes its rendition of the cache. As an "axiom" all those renditions
      # are the same for the same instance of `PatternCache`. However if some other thread
      # has already proposed a rendition in the meantime, we quitely accept that other
      # thread's rendition BUT reuse our own cache on the current run. On the later runs
      # we'll reuse the public version.
      def propose(key : Int32, & : Pf::Map::Commit(Int32, Term::Match::Pattern) ->) : SlotMap
        slots0 = @slots.get(:acquire)
        if slots0.present?
          return slots0
        end
        slots1 = slots0.fill { |commit| yield commit }
        _, _ok = @slots.compare_and_set(slots0, slots1, :release, :relaxed)
        slots1
      end
    end

    # :nodoc:
    struct PatternCache::OneTime
      def propose(key : Int32, &) : SlotMap
        SlotMap[].fill { |commit| yield commit }
      end
    end

    class PatternCache::Global
      def initialize
        @caches = Atomic(Pf::MapBox(Int32, PatternCache)).new(Pf::MapBox(Int32, PatternCache).new)
      end

      def propose(key : Int32, & : Pf::Map::Commit(Int32, Term::Match::Pattern) ->) : SlotMap
        caches0 = @caches.get(:acquire)

        cache = caches0[key]? || Term::PatternCache.new
        changed = false

        slots = cache.propose(key) do |commit|
          changed = true
          yield commit
        end

        return slots unless changed

        caches0 = @caches.get(:acquire)
        while true
          caches1 = caches0.assoc(key, cache)
          caches0, ok = @caches.compare_and_set(caches0, caches1, :release, :acquire)
          break if ok
        end

        slots
      end
    end

    PatternCache::GLOBAL = PatternCache::Global.new

    module Case
      struct Continue
      end

      RE_CAPTURES = /([a-zA-Z_]\w*?)(?:_(?:any|number|symbol|string|boolean|dict)?[+*⋮]?|←|⥆|⋮)|\((?:%let\*?|%pair\*|%many)\s+([a-zA-Z]\w*)/

      # :nodoc:
      macro match(pattern, matchee, result, env, ml = nil, &block)
        {% if block.args.empty? && ml %}
          {% captures = ml.scan(RE_CAPTURES).map { |match| (match[1] || match[2]).id }.uniq %}
        {% else %}
          {% captures = block.args %}
        {% end %}

        if %response = {{pattern}}.match?({{env}}, {{matchee}})
          %subt, _ = %response

          {{result}} = pass(
            {% for capture in captures %}
              {% if optional = capture.ends_with?("?") %}
                {% capture = capture[...-1] %}
              {% end %}

              {% if optional %}
                %subt[{{capture.symbolize}}]?,
              {% else %}
                {% if flag?(:release) %}
                  {% error = "substitution for \"#{capture.id}\" not found. Run in debug mode for more info" %}
                {% else %}
                  {% error = <<-ERROR
                  Term.case: substitution for "#{capture.id}" not found in pattern for block at: #{block.filename.id}:#{block.line_number}:#{block.column_number}
                  ERROR
                  %}
                {% end %}

                %subt[{{capture.symbolize}}]? || (raise {{error}}),
              {% end %}
            {% end %}
          ) {% if !block.args.empty? || captures.empty? %} {{block}} {% else %} do |{{captures.splat}}|
            {{block.body}}
          end
          {% end %}
        end
      end

      # :nodoc:
      macro otherwise(matchee, result, &block)
        {{result}} = begin
          {{yield}}
        end
      end

      # :nodoc:
      macro passoc(group, exp, proposal, index)
      end

      # :nodoc:
      macro caseg(groups, matchee, cache, env, group_by, key, &block)
        %group_by = {{group_by}}
        %result = continue = ::Ww::Term::Case::Continue
        %matchee = ::Ww::Term.of({{matchee}})
        %env = {{env}}

        %patterns = {{cache}}.propose({{key}}) do |%proposal|
          {% for row, index in groups %}
            {% exp = row[:exp] %}

            {% if exp.name == :givenp || exp.name == :givenpi %}
              %pattern = ::Ww::ML.parse({{exp.args.splat}})
              %proposal.assoc({{index}}, ::Ww::Term::Match.compile(%pattern))
            {% elsif exp.name == :matchp || exp.name == :matchpi %}
              %pattern = ::Ww::ML.parse1({{exp.args.splat}})
              %proposal.assoc({{index}}, ::Ww::Term::Match.compile(%pattern))
            {% elsif exp.name == :given %}
              %pattern = ::Ww::Term::Dict.build do |commit|
                {% for arg, argno in exp.args %}
                  commit.with({{argno}}, {{arg}})
                {% end %}
                {% if exp.named_args %}
                  {% for named_arg in exp.named_args %}
                    commit.with({{named_arg.name.symbolize}}, {{named_arg.value}})
                  {% end %}
                {% end %}
              end
              %proposal.assoc({{index}}, ::Ww::Term::Match.compile(%pattern))
            {% elsif exp.name == :match %}
              %pattern = {{(exp.args + (exp.named_args || [] of ::NoReturn)).splat}}
              %proposal.assoc({{index}}, ::Ww::Term::Match.compile(%pattern))
            {% end %}
          {% end %}
        end

        {% group_chunks = [] of ::NoReturn %}
        {% group_chunk = [] of ::NoReturn %}
        {% current_group = nil %}

        {% for row, index in groups %}
          {% group, exp = row[:group], row[:exp] %}
          {% if group == current_group %}
            {% group_chunk << exp %}
          {% else %}
            {% unless group_chunk.empty? %}
              {% group_chunks << {current_group, group_chunk} %}
            {% end %}
            {% current_group = group %}
            {% group_chunk = [exp] %}
          {% end %}
        {% end %}

        {% unless group_chunk.empty? %}
          {% group_chunks << {current_group, group_chunk} %}
        {% end %}

        pass do
          {% index = 0 %}
          {% for row in group_chunks %}
            {% group, chunk = row %}

            {% if group_by && group %}
              if {{group_by}}.{{group.id}}
            {% end %}

            {% for exp in chunk %}
              {% if exp.name == :given || exp.name == :givenp %}
                ::Ww::Term::Case.match(%patterns[{{index}}], %matchee, %result, %env) {{exp.block}}
              {% elsif exp.name == :givenpi %}
                ::Ww::Term::Case.match(%patterns[{{index}}], %matchee, %result, %env, ml: {{exp.args[0]}}) {{exp.block}}
              {% elsif exp.name == :match || exp.name == :matchp %}
                ::Ww::Term::Case.match(%patterns[{{index}}], %matchee, %result, %env) {{exp.block}}
              {% elsif exp.name == :matchpi %}
                ::Ww::Term::Case.match(%patterns[{{index}}], %matchee, %result, %env, ml: {{exp.args[0]}}) {{exp.block}}
              {% elsif exp.name == :otherwise %}
                ::Ww::Term::Case.otherwise(%matchee, %result) {{exp.block}}
              {% end %}

              unless %result == ::Ww::Term::Case::Continue
                break
              end

              {% index += 1 %}
            {% end %}

            {% if group_by && group %}
              end
            {% end %}
          {% end %}
        end

        if %result.is_a?(::Ww::Term::Case::Continue.class)
          raise ArgumentError.new
        end

        %result
      end

      # :nodoc:
      #
      # Counter that we increment from within the `case` macro to obtain unique
      # case ids for each expansion of Term.case, to use as a key in the global
      # pattern cache.
      CASE_ID = [0]

      # :nodoc:
      macro case(matchee, *, cache = ::Ww::Term::PatternCache::GLOBAL, env = ::Ww::Term[], group_by = nil, &block)
        {% key = ::Ww::Term::Case::CASE_ID[0] %}
        {% ::Ww::Term::Case::CASE_ID[0] += 1 %}

        {% if block.body.is_a?(Expressions) %}
          {% exps = block.body.expressions %}
        {% else %}
          {% exps = [block.body] %}
        {% end %}

        {% groups = [] of ::NoReturn %}

        {% for exp in exps %}
          {% unless exp.is_a?(Call) %}
            {% exp.raise "expected a call, got: #{exp}" %}
          {% end %}

          {% unless {:group, :given, :givenp, :givenpi, :match, :matchp, :matchpi, :otherwise}.includes?(exp.name.symbolize) && exp.block %}
            {% exp.raise <<-END
            unexpected call to "#{exp.name.id}". Supported calls:
              - group(call, &)
              - given(*args, **kwargs, &)
              - givenp(*wwml : String, &) "parse"
              - givenpi(*wwml : String, &) "parse autoimport"
              - match(*args, **kwargs, &)
              - matchp(*wwml : String, &) "parse"
              - matchpi(*wwml : String, &) "parse autoimport"
              - otherwise(&)
            END
            %}
          {% end %}

          {% if exp.name == :group %}
            {% unless arg = exp.args.first %}
              {% exp.raise "expected an argument to group: group(call, &)" %}
            {% end %}

            {% body = exp.block.body %}
            {% for subexp in (body.is_a?(Expressions) ? body.expressions : [body]) %}
              {% groups << {group: arg, exp: subexp} %}
            {% end %}
          {% elsif (exp.name == :givenp || exp.name == :matchp || exp.name == :givenpi || exp.name == :matchpi) && exp.args.size > 1 %}
            {% for arg in exp.args %}
              {% groups << {group: nil, exp: "#{exp.name}(#{arg}) #{exp.block}".id} %}
            {% end %}
          {% else %}
            {% groups << {group: nil, exp: exp} %}
          {% end %}
        {% end %}

        ::Ww::Term::Case.caseg({{groups}}, {{matchee}}, {{cache}}, {{env}}, {{group_by}}, {{key}}) {{block}}
      end
    end

    # Types of blanks supported by autodeclare:
    #
    # - Untyped ("any") blanks: `<id>_`.
    # - Typed blanks: `<id>_number`, `<id>_boolean`, etc.
    # - Untyped polyblanks: `<id>_*`, `<id>_+`.
    # - Typed polyblanks: `<id>_number*`, `<id>_boolean+`.
    # - Let and let shorthand: `%let <id>`, `<id>←`.
    # - `Let*` and `let*` shorthand: `%let* <id>`, `<id>⥆`.
    # - `(%pair*)`: `%pair* <id>`.
    # - `(%many)`: `%many <id>`.
    # - Type inference `%default` shorthand syntax: `<id>⋮ <value>`.
    #
    # Blanks/<id> can contain underscores, letters, digits; all blanks specifically
    # must end with `_`, `_number`,`_string`,`_boolean`,`_symbol`,`_dict`,`_any`; everything
    # before is an optional name, including other underscores. Nothing must come after.
    # Polyblanks are required to have trailing `*` or `+`, e.g. `_dict*` or `_symbol+`.
    macro case(*args, **kwargs, &block)
      ::Ww::Term::Case.case({{args.splat}}, {{kwargs.double_splat}}) {{block}}
    end
  end
end
