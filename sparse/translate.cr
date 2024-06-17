module Ww::Sparse::TermAST
  private TRANSLATABLE_PATTERN_CACHE = [] of Term

  # Returns `true` if an inner *term* (one inside `(sparse ...)`) is translatable.
  # Otherwise, returns `false`.
  private def translatable?(*, term : Term) : Bool
    {% begin %}
      Term.case(term, cache: TRANSLATABLE_PATTERN_CACHE) do
        given :any? { true }
        given :number? { true }
        given :string? { true }
        given :symbol? { true }
        given :dict? { true }
        given :boolean? { true }
        given :"/?", :_number { true }

        given :range, b: :_number, e: :_number { true }
        given :range, b: :_number { true }
        given :range, e: :_number { true }

        given :range, bx: :_number, e: :_number { true }
        given :range, bx: :_number { true }

        given :range, b: :_number, ex: :_number { true }
        given :range, ex: :_number { true }

        given :range, bx: :_number, ex: :_number { true }

        given :&, :"conjuncts+" do |conjuncts|
          conjuncts.ie.all? { |conjunct| translatable?(term: conjunct) }
        end

        given :pair, :"path+", :value_ do |path, value|
          path.ie.none?(&.type.dict?) && translatable?(term: value)
        end

        {% for pattern in [{:+, :_number}, {:-, :_number}, {:*, :_number},
                           {:/, :_number}, {:**, :_number}, {:upcase},
                           {:downcase}, {:size}, {:sin}, {:cos}, {:tan}] %}
          given :"->>", {{pattern}}, :"deps+" do |deps|
            deps.ie.all? { |dep| translatable?(term: dep) }
          end
        {% end %}

        # Return true for literals such as `12`, `true`, `"hello world"`, and
        # false otherwise.
        otherwise { !term.type.dict? }
      end
    {% end %}
  end

  # Returns `true` if an entire Sparse *query* represented using TermAST is
  # translatable. Returns `false` otherwise.
  def translatable?(*, query : Term::Dict) : Bool
    return false unless query.itemsonly?

    items = query.items
    return false unless items.size >= 2
    return false unless items[0] == Term.of(:sparse)

    (1...items.size).all? { |index| translatable?(term: items[index]) }
  end

  # :ditto:
  def translatable?(*, query : ITerm)
    false
  end

  # :ditto:
  def translatable?(*, query : Term)
    translatable?(query: query.downcast)
  end

  # :nodoc:
  def escape(term : Term::Dict) : Term
    escaped = Term::Dict.build do |commit|
      commit.with(commit.size, :&)
      term.each_entry do |k, v|
        commit.with(commit.size, {:pair, escape(k), escape(v)})
      end
    end

    escaped.upcast
  end

  # Escapes *term* to be valid Sparse TermAST.
  #
  # Generates TermAST that corresponds to matching *term* literally. If *term*
  # is a dictionary, matches literally all of its key-value pairs (recursing
  # on values).
  def escape(term : Term) : Term
    escape(term.downcast)
  end

  # :ditto:
  def escape(term : ITerm) : Term
    term.upcast
  end

  private UNSAFE_TRANSLATE_PATTERN_CACHE = [] of Term

  # Raises `CannotTranslateException` if impossible to translate *pattern* or one
  # of its subterms.
  private def translate(pattern : Term, builder : Unit::Builder, prefix : Unit::Builder ->) : Nil
    {% begin %}
      Term.case(pattern, cache: UNSAFE_TRANSLATE_PATTERN_CACHE) do
        {% for match, base in {:any?     => Unit::IsAny,
                               :number?  => Unit::IsNum,
                               :string?  => Unit::IsStr,
                               :symbol?  => Unit::IsSym,
                               :dict?    => Unit::IsDict,
                               :boolean? => Unit::IsBoolean} %}
          given {{match}} do
            prefix.call(builder)

            builder << {{base}}.new
          end
        {% end %}

        given :pair, :"path+", :value_ do |path, value|
          return unless path.ie.none?(&.type.dict?)

          translate(value, builder) do |builder|
            path.ie.each do |key|
              prefix.call(builder)
              builder << Unit::IsDict.new << Unit::WhereKey.new(key)
            end
          end
        end

        given :&, :"conjuncts+" do |conjuncts|
          builder.join(conjuncts.ie) do |conjunct|
            translate(conjunct, builder, prefix)
          end
        end

        given :"/?", :factor_number do |factor|
          prefix.call(builder)

          builder << Unit::IsNum.new << Unit::DivBy.new(factor.as_n)
        end

        # Range permutations: begin, begin exclusive; end, end exclusive,
        # omission support is indicated by `nil`.
        {% for spec in [{:b, nil}, {nil, :e}, {:bx, nil}, {nil, :ex},
                        {:b, :e}, {:bx, :ex}, {:b, :ex}, {:bx, :e}] %}
          {% b, e = spec %}

          given(:range,
            {% if b %} {{b.id}}: :b_number, {% end %}
            {% if e %} {{e.id}}: :e_number, {% end %}
          ) do |b?, e?|
            prefix.call(builder)

            builder << Unit::IsNum.new
            builder << Unit::InRange.new(
              b: b?.try(&.as_n),
              e: e?.try(&.as_n),
              bx: {{b == :bx}},
              ex: {{e == :ex}},
            )
          end
        {% end %}

        # Fold repetition of function definitions somewhat.
        #
        # Could have been a macro but there's really no need to make it even more
        # complicated.
        {% for spec in [{ {:+, :n_number}, {:n}, Unit::IsNum, "Offset.new(n.as_n)" },
                        { {:-, :n_number}, {:n}, Unit::IsNum, "Offset.new(-n)" },
                        { {:*, :n_number}, {:n}, Unit::IsNum, "Scale.new(n.as_n)" },
                        { {:/, :n_number}, {:n}, Unit::IsNum, "Scale.new(n.reciprocal)" },
                        { {:**, :n_number}, {:n}, Unit::IsNum, "Pow.new(n.as_n)" },
                        { {:upcase}, nil, Unit::IsStr, "Upcase.new" },
                        { {:downcase}, nil, Unit::IsStr, "Downcase.new" },
                        { {:size}, nil, Unit::IsAny, "Size.new" },
                        { {:sin}, nil, Unit::IsNum, "Sin.new" },
                        { {:cos}, nil, Unit::IsNum, "Cos.new" },
                        { {:tan}, nil, Unit::IsNum, "Tan.new" }] %}
          {% signature, args, inpcheck, constructor = spec %}

          given :"->>", {{signature}}, :"deps+" do |{% if args %} {{args.map(&.id).splat}}, {% end %} deps|
            deps.ie.each do |dep|
              translate(dep, builder) do
                prefix.call(builder)

                builder << {{inpcheck}}.new << Unit::Tf::{{constructor.id}}
              end
            end
          end
        {% end %}

        match :_dict { raise CannotTranslateException.new }

        otherwise do
          prefix.call(builder)

          builder << pattern.type.check << Unit::Match.new(pattern)
        end
      end
    {% end %}
  end

  private def translate(pattern : Term, builder : Unit::Builder, &prefix : Unit::Builder ->) : Nil
    translate(pattern, builder, prefix)
  end

  private def translate(pattern : Term, builder : Unit::Builder) : Nil
    translate(pattern, builder) { }
  end

  # Translates *pattern* without checking whether it is `translatable?`.
  #
  # Will raise `CannotTranslateException` if *term* or one of its subterms
  # cannot be translated.
  def translate(*, pattern : Term) : Unit::Member
    translate(pattern, builder = Unit::Builder.new)

    builder.unit
  end

  # Translates *query* without checking whether it is `translatable?`. Returns
  # the resulting `Unit::Query`.
  #
  # Will raise `CannotTranslateException` if *term* or one of its subterms
  # cannot be translated.
  def translate(query : Term) : Unit::Query
    raise CannotTranslateException.new unless query.itemsonly?

    items = query.items
    raise CannotTranslateException.new unless items.size >= 2
    raise CannotTranslateException.new unless items[0] == Term.of(:sparse)

    pattern = translate(pattern: items[1])
    counterpatterns = (2...items.size).map { |index| translate(pattern: items[index]) }

    Unit::Query.new(pattern, counterpatterns)
  end
end
