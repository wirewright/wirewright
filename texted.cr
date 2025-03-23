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

# src = <<-SRC
# Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi
# Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit
# nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud
# officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia
# dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris
# ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim.
# Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.
# SRC

model = Term.of(Term[], "", :|, "", Term[], Term[])

def input(model : Term)
  Term.of(:p, ML.display(model), style: "text-neutral-300 font-mono text-xs")

  # Term.of(:group, l, Term.of({:self, :rect}, style: "w-px h-max"), r, style: "content text-neutral-300 flow-block")
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
    matchpi %{[_ _ _ _ _ events←(_*)]} do
      model = Term.of(model.morph({5, events.append(event)}))
      model = rewrite(model, step)
    end

    otherwise { }
  end

  frame = frame.morph({1, input(model)})

  Term.of(frame)
end

UIR::Platform::Current.show(ui)

