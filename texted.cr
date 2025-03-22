require "./src/wirewright"

base = File.read("input.soma.wwml")

refR = dfsR(
  switchR(
    { %[($my rewritee_)], envR(Term.of(:"$my")) },
    { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
    { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
  )
)

primitives = ProcRuleset.build do
end

onceR = chainR(callR(primitives), callR(PRIMITIVES))

evalR = dfsR(
  switchR(
    { %[($ rewritee_)], exhR(dfsR(onceR)) },
    { %[($once rewritee_)], onceR },
  )
)

backmapR = chainR(refR, evalR)

selector = ML.term(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])

set, rec = recR

step = exhR(
  set.call choiceR(
    rulesetR(Ruleset.select(selector, ML.terms(base)), noR, backmapR, noR),
    itemsR(rec),
  )
)

model = Term.of("", :|, File.read("editor-suggestions.soma.wwml"), Term[])

def input(model : Term)
  Term.case(model) do
    matchpi %{[l_string | r_string (_*)]} do
      Term::Dict.build do |commit|
        commit << :group
        commit.with(:style, "content flow-col gap-2")

        (l.to(String).lines(chomp: false)[...-1]? || [] of String).each do |line|
          commit << Term.of(:p, line, style: "text-neutral-300")
        end

        commit << Term.of(:group,
          Term.of(:p, l.to(String).lines(chomp: false)[-1]? || "", style: "text-neutral-300"),
          Term.of({:self, :rect}, style: "bg-blue-500 w-px h-max"),
          Term.of(:p, r.to(String).lines(chomp: false)[0]? || "", style: "text-neutral-300"),
          style: "content flow-row"
        )

        (r.to(String).lines(chomp: false)[1..]? || [] of String).each do |line|
          commit << Term.of(:p, line, style: "text-neutral-300")
        end
      end
    end

    otherwise { }
  end
end

require "./uiRb"
require "./sfuiR"

module UIR::Platform
  alias Current = SFML
end

frame0 = ML.term <<-WWML
((self window) style: "bg-neutral-900 max origin" max-w: 1000 max-h: 800)
WWML

frame0 = frame0.morph({1, input(model)})

ui = UIR::Reducers.microfold(Term.of(frame0)) do |frame, drawable, event|
  if event == Term.of(:cycle)
    next frame
  end

  Term.case(model) do
    matchpi %{[_ _ _ events←(_*)]} do
      model = Term.of(model.morph({3, events.append(event)}))
      model = rewrite(model, step)
    end

    otherwise { }
  end

  frame = frame.morph({1, input(model)})

  Term.of(frame)
end

UIR::Platform::Current.show(ui)

