module Ww::Sparse::TermAST
  private def instance(term : Term, state : Term, seen : Set(Term)) : Term
    instance?(term, state, seen) || term
  end

  private def instance?(term : Term, state : Term, seen : Set(Term)) : Term?
    instance?(term, state, seen) { true }
  end

  private def instance(term : Term, state : Term, seen : Set(Term), &approve : Term -> Bool) : Term
    instance?(term, state, seen, approve) || term
  end

  private def instance?(term : Term, state : Term, seen : Set(Term), &approve : Term -> Bool) : Term?
    instance?(term, state, seen, approve)
  end

  private def instance?(term : Term, state : Term, seen : Set(Term), approve : Term -> Bool) : Term?
    {% begin %}
      Term.case(term) do
        given :"/?", :factor_ do |factor|
          term = term
            .with(1, instance(factor, state, seen, &.type.number?))
            .upcast

          approve.call(term) ? term : nil
        end

        given :pair, :"path_+", :value_ do |path, value|
          term = term
            .replace do |i, v|
              if i == Term[term.size - 1]
                # Value always comes last and can be replaced by anything.
                instance(v, state, seen)
              else
                # Part of path cannot be replaced by a dictionary.
                instance(v, state, seen) { |replacement| !replacement.type.dict? }
              end
            end
            .upcast

          approve.call(term) ? term : nil
        end

        # Range permutations: begin, begin exclusive; end, end exclusive,
        # omission support is indicated by `nil`.
        {% for spec in [{:b, nil}, {nil, :e},
                        {:bx, nil}, {nil, :ex},
                        {:b, :e}, {:bx, :ex},
                        {:b, :ex}, {:bx, :e}] %}
          {% b, e = spec %}

          given(:range,
            {% if b %} {{b.id}}: :b_, {% end %}
            {% if e %} {{e.id}}: :e_, {% end %}
          ) do |b?, e?|
            # We're checking presence of b/e at the macro level, hence the need
            # for not_nil at runtime
            term = term
            {% if b %}
              .with({{b}}, instance?(b?.not_nil!, state, seen, &.type.number?) || b?)
            {% end %}
            {% if e %}
              .with({{e}}, instance?(e?.not_nil!, state, seen, &.type.number?) || e?)
            {% end %}
              .upcast

            approve.call(term) ? term : nil
          end
        {% end %}

        # Math functions that take a single numeric argument, n.
        {% for pattern in [{:+, :n_}, {:-, :n_}, {:*, :n_}, {:/, :n_}, {:**, :n_}] %}
          given :"->>", {{pattern}}, :"deps_+" do |n, deps|
            n = instance(n, state, seen, &.type.number?)
            term = term
              .replace do |i, v|
                if i == Term[1]
                  # Header always comes second (after ->>), replace it with rewritten n.
                  Term.of({ {{pattern[0]}}, n })
                else
                  # Otherwise we're in deps or at ->> and we can put anything there.
                  instance(v, state, seen)
                end
              end
              .upcast

            approve.call(term) ? term : nil
          end
        {% end %}

        given :hole, escaped: :escaped_boolean do |escaped|
          if escaped.true?
            result = escape(state)
          elsif seen.add?(term)
            result = instance?(state, state, seen, approve)
            seen.delete(term)
          end

          return unless result # Found replacement
          return unless approve.call(result) # Replacement approved by parent

          result
        end

        given :hole, :"path_+", escaped: :escaped_boolean do |path, escaped|
          # Instantiate parts of the path.
          term = term
            .replace do |_, part|
              instance(part, state, seen) { |replacement| !replacement.type.dict? }
            end
            .upcast

          # Try to find the value. Disallow dictionary-typed keys because they
          # are impossible to obtain normally, and signify an unsuccessful
          # instantiation.
          value = (1...1 + path.size).reduce(state) do |dict, i|
            part = term[i]
            break if part.type.dict?
            break unless v = dict[part]?
            v
          end

          if value
            if escaped.true?
              result = escape(value)
            elsif seen.add?(term)
              result = instance?(value, state, seen, approve)
              seen.delete(term)
            end
          end

          return unless result # Found replacement
          return unless approve.call(result) # Replacement approved by parent

          result
        end

        given :hole, escaped: :escaped_boolean, default: :default_ do |escaped, default|
          if escaped.true?
            result = escape(state)
          elsif seen.add?(term)
            result = instance?(state, state, seen, approve)
            seen.delete(term)
          end

          if result && approve.call(result)
            return result
          end

          if seen.add?(term)
            result = instance?(default, state, seen)
            seen.delete(term)
          end

          return unless result
          return unless approve.call(result)

          result
        end

        given :hole, :"path_+", escaped: :escaped_boolean, default: :default_ do |path, escaped, default|
          term = term
            .replace do |_, part|
              instance(part, state, seen) { |replacement| !replacement.type.dict? }
            end
            .upcast

          value = (1...1 + path.size).reduce(state) do |dict, i|
            part = term[i]
            break if part.type.dict?
            break unless v = dict[part]?
            v
          end

          if value
            if escaped.true?
              result = escape(value)
            elsif seen.add?(term)
              result = instance?(value, state, seen, approve)
              seen.delete(term)
            end
          end

          if result && approve.call(result)
            return result
          end

          if seen.add?(term)
            result = instance?(default, state, seen)
            seen.delete(term)
          end

          return unless result
          return unless approve.call(result)

          result
        end

        given :rescue, :"branches_+" do |branches|
          branches.ie.each do |branch|
            next unless branch = instance?(branch, state, seen, approve)
            next unless translatable?(term: branch)
            return branch
          end
        end

        # Leave literals as is. Instantiate values of unhandled dictionaries.
        otherwise do
          return term unless term.type.dict?

          term.replace { |_, v| instance(v, state, seen) }.upcast
        end
      end
    {% end %}
  end

  # Returns an instance of *query* within the given *state*.
  #
  # During instantiation, holes in *query* are replaced with corresponding values
  # or subqueries from *state*. Additionally, rescues are resolved.
  #
  # If some hole cannot be replaced or some rescue resolved, `instance` will leave
  # them as is. Therefore, if you want to translate the instance later (and you most
  # likely do), you will have to run `translatable?` on the returned instance before
  # handing it over to `translate`. Otherwise be prepared to catch `CannotTranslateException`.
  # Holes nor rescues are translatable. This is particularly important if queries
  # are provided by the user.
  def instance(query : ITerm | Term, state : ITerm | Term) : Term
    instance(query.upcast, state.upcast)
  end

  # :nodoc:
  def instance(query : Term, state : Term) : Term
    return query unless query.itemsonly?

    items = query.items
    return query unless items.size >= 2
    return query unless items[0] == Term.of(:sparse)

    seen = Set(Term).new
    query.replace { |k, v| instance(v, state, seen) }.upcast
  end
end
