PRIMITIVES = ProcRuleset.build do
  rulepi1 %[(+ args_number+)] { args.items.reduce { |a, b| a + b } }
  rulepi1 %[(- args_number+)] { args.items.reduce { |a, b| a - b } }
  rulepi1 %[(* args_number+)] { args.items.reduce { |a, b| a * b } }

  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
  rulepi1 %[(// a_number (%all b_number (%not 0)))] { a // b }

  rulepi1 %[(~ args_string+)] { args.items.reduce { |a, b| a.stitch(b) } }

  rulepi1 %[(string term_)] { ML.display(term, endl: false) }

  rulepi1 %[(ml ml_string ¦ () shadow⋮ true)] do
    term = ML.term(ml.to(String))
    if shadow.false? && (symbol = term.as_sym?) && Rhodium.shadow?(symbol)
      # TODO: line col message
      {:"ml/err"}
    else
      {:"ml/ok", term}
    end
  rescue ML::SyntaxError
    # TODO: line col message
    {:"ml/err"}
  end

  rulepi1 %[(< a_number b_number)] do
    a.unsafe_as_n < b.unsafe_as_n
  end

  rulepi1 %[(> a_number b_number)] do
    a.unsafe_as_n > b.unsafe_as_n
  end

  rulepi1 %[(= a_ bs_+)] do
    bs.unsafe_as_d.items.all? { |b| a == b }
  end

  # FIXME: remove this flag
  {% if flag?(:soma6) %}
    # Converts an arbitrary term into its D7VR (Microfold unit) -> UIR (thus D7UIR)
    # code-only representation.
    #
    # D7VR is what you convert a term into, to then feed that to Microfold, then UIR,
    # then draw it, then paint the resulting draw commands using some kind of painting
    # backend (e.g. sfpaint).
    rulepi1 %[(d7uir term_ ¦ () rem_: (%number +i32) code-only: true)] do
      pipe(term, D7VR.term_unit, D7VR.uir(rem: rem.unsafe_as_n))
    end
  {% end %}

  # E.g. (join {x: 1, y: 2} (entry z 3))
  rulepi1 %[(entry k_ v_)] do
    Term[].with(k, v)
  end

  rulepi1 %[(cat xs_dict+)] do
    Term::Dict.build do |commit|
      xs.items.each do |x|
        x = x.unsafe_as_d

        commit.concat(x.items)

        x.each_pair do |k, v|
          commit.with(k, v)
        end
      end
    end
  end

  rulepi1 %[(join xs_dict ys_dict)] do
    xs | ys
  end

  rulepi1 %[(merge xs_dict ys_dict)] do
    xs & ys
  end

  rulepi1 %[(value xs_dict key_)] do
    xs[key]? || Term.of(:value, xs, key)
  end

  rulepi1 %[(hashcode term_)] do
    Term.hashcode(term)
  end

  rulepi1 %[(entries xs_dict)] do
    Term[xs.ee(ordered: true)]
  end

  # TODO: remove!!!!!! this leaks the fact that we don't actually have proper entry order!!
  rulepi1 %[(nth xs_dict n←(%number +i32))] do
    if response = xs.nth?(n.to(Int32))
      key, value = response
      Term.of(:some, {key, value})
    else
      Term.of({:none})
    end
  end

  rulepi1 %[(itemspart xs_dict)] do
    xs.itemspart
  end

  rulepi1 %[(pairspart xs_dict)] do
    xs.pairspart
  end

  # Flattens itemspart of *xs*, its items and so on, recursively.
  rulepi1 %[(flatten xs_)] do
    Term::Dict.build do |commit|
      Term.each_keypath_and_item(xs) do |_, leaf|
        commit << leaf

        true # Continue
      end
    end
  end

  rulepi1 %[(tally xs_dict)] do
    xs.unsafe_as_d.size
  end

  rulepi1 %[(charcount xs_string)] do
    xs.unsafe_as_s.charcount
  end

  rulepi1 %[(sum ())] { 0 }
  rulepi1 %[(sum (args_number+))] { args.items.reduce { |a, b| a.unsafe_as_n + b.unsafe_as_n } }

  rulepi1 %[(min args_number+)] { args.items.min_by(&.unsafe_as_n) }
  rulepi1 %[(min (args_number+))] { args.items.min_by(&.unsafe_as_n) }

  rulepi1 %[(max args_number+)] { args.items.max_by(&.unsafe_as_n) }
  rulepi1 %[(max (args_number+))] { args.items.max_by(&.unsafe_as_n) }

  rulepi1 %[(ceil arg_number)] { arg.ceil }

  rulepi1 %[(runes s_string b←(%number i32) to e←(%number i32))] do
    Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(rune s_string b←e←(%number i32))] do
    Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(words s_string b←(%number i32) to e←(%number i32))] do
    Term::Str::Substring.words(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(word s_string b←e←(%number i32))] do
    Term::Str::Substring.words(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(lines s_string b←(%number i32) to e←(%number i32))] do
    Term::Str::Substring.lines(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(line s_string b←e←(%number i32))] do
    Term::Str::Substring.lines(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  # S₁S₂S₃S₄
  # M₁M₂
  # == S₁S₂
  #
  # S₁S₂S₃S₄
  # M₁M₂M₃M₄M₅M₆
  # == S₁S₂S₃S₄
  rulepi1 %[(prefix s_string m_string)] do
    Term::Str::Substring.runes(s.unsafe_as_s, 0, m.charcount - 1)
  end
  rulepi1 %[(prefix s_string "")] do
    ""
  end

  rulepi1 %[(suffix s_string m_string)] do
    Term::Str::Substring.runes(s.unsafe_as_s, m.charcount, -1)
  end
end
