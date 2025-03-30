PRIMITIVES = ProcRuleset.build do
  rulepi1 %[(+ args_number+)] { args.items.reduce { |a, b| a + b } }
  rulepi1 %[(- args_number+)] { args.items.reduce { |a, b| a - b } }
  rulepi1 %[(* args_number+)] { args.items.reduce { |a, b| a * b } }

  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
  rulepi1 %[(// a_number (%all b_number (%not 0)))] { a // b }

  rulepi1 %[(~ args_string+)] { args.items.reduce { |a, b| a.stitch(b) } }

  rulepi1 %[(string term_)] { ML.display(term, endl: false) }

  rulepi1 %[(ml ml_string)] do
    begin
      {:"ml/ok", ML.term(ml.to(String))}
    rescue ML::SyntaxError
      # TODO: line col message
      {:"ml/err"}
    end
  end

  rulepi1 %[(< a_number b_number)] do
    a.unsafe_as_n < b.unsafe_as_n
  end

  rulepi1 %[(> a_number b_number)] do
    a.unsafe_as_n > b.unsafe_as_n
  end

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
