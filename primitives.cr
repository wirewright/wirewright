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

  rulepi1 %[(> a_number b_number)] do
    a.unsafe_as_n > b.unsafe_as_n
  end

  # E.g. (join {x: 1, y: 2} (entry z 3))
  rulepi1 %[(entry k_ v_)] do
    Term[].with(k, v)
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

  rulepi1 %[(sum ())] { 0 }
  rulepi1 %[(sum (args_number+))] { args.items.reduce { |a, b| a.unsafe_as_n + b.unsafe_as_n } }

  rulepi1 %[(min args_number+)] { args.items.min_by(&.unsafe_as_n) }
  rulepi1 %[(min (args_number+))] { args.items.min_by(&.unsafe_as_n) }

  rulepi1 %[(max args_number+)] { args.items.max_by(&.unsafe_as_n) }
  rulepi1 %[(max (args_number+))] { args.items.max_by(&.unsafe_as_n) }

  # TODO: support mixed substring?
  rulepi1 %[(substring s_string (rune b←(%number i32)) (rune e←(%number i32)))] do
    Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(substring s_string (word b←(%number i32)) (word e←(%number i32)))] do
    Term::Str::Substring.words(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  # TODO: take substring by lines.
end
