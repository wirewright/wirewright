module Input
  extend self

  private def backmapR(ruleset : Ruleset)
    onceR = callR(PRIMITIVES)

    # First rewrite entries, then rewrite self.
    set, exhevalR = recR
    set.call chainR(entriesR(exhevalR), onceR)

    evalR = dfsR(
      switchR(
        { %[($ rewritee_)], exhevalR },
        { %[($once rewritee_)], onceR },
      )
    )

    set_backmapr, rec_backmapr = recR

    refR = dfsR(
      switchR(
        { %[($my rewritee←($ _))], chainR(rec_backmapr, envR(Term.of(:"$my"))) },
        { %[($my rewritee_)], envR(Term.of(:"$my")) },
        { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
        { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
      )
    )

    backmapR = set_backmapr.call chainR(refR, evalR)

    selector = ML.term(%{[backmap pattern_ backspec_]})

    rulesetR(ruleset, noR, backmapR, noR)
  end

  private def rulesets(document : Term, *features : Term)
    bases = features.map { |feature| document[feature] }

    instants = Ruleset.select(ML.term(%{[backmap (instant pattern_*) backspec_]}), *bases)
    events = Ruleset.select(ML.term(%{[backmap (event pattern_*) backspec_]}), *bases)

    {backmapR(instants), backmapR(events)}
  end

  private def rewrite(base : Rewriter, input : Term, events : Term)
    return input, events unless input.type.dict?
    return input, events unless events.type.dict?

    Term.case(input) do
      matchpi %{(suspended _)} do
        return input, events
      end

      otherwise { }
    end

    events1 = events

    case r = rewrite0(Term.of(input, events), base)
    in Rewrite::None
      input1 = input.transaction do |commit|
        input.each_item_with_index do |item, index|
          item1, events1 = rewrite(base, item, events)
          next if item == item1 && events == events1
          commit.with(index, item1)
          break
        end
      end
      {Term.of(input1), events1}
    in Rewrite::One
      Term.case(r.term) do
        matchpi %{(input1_ events1_)} do
          {input1, events1}
        end

        otherwise do
          raise "unsupported"
        end
      end
    in Rewrite::Many
      raise "unsupported"
    end
  end

  private def step(instantR, eventR, input0 : Term, events0 : Term)
    input1, events1 = rewrite(instantR, input0, events0)
    unless {input0, events0} == {input1, events1}
      return input1, events1
    end

    case r = rewrite0(events0, eventR)
    in Rewrite::None then {input0, events0}
    in Rewrite::One  then {input0, r.term}
    in Rewrite::Many
      unreachable("not implemented")
    end
  end

  private def process(instantR, eventR, input0 : Term, events0 : Term)
    loop do
      input1, events1 = step(instantR, eventR, input0, events0)
      break if {input0, events0} == {input1, events1}

      input0 = input1
      events0 = events1
    end

    input0
  end

  def input(rulebase : Term, *features : Term)
    instantR, eventR = rulesets(rulebase, *features)

    ->(input : Term, events : Term) do
      process(instantR, eventR, input, events)
    end
  end
end
