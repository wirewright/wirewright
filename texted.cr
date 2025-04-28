# missing features:
#  - small stuff (missing keybindings, cannot switch sides in Shift, etc.)
#  - single-line appearance
# <----> here we will be able to start using the thing for command palette
#  - multiline appearance with scrolling (up/down, left/right, mouse wheel, Shift + mouse wheel)
#  - multiline with wrapping
#  - point and click

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

src = <<-SRC
Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi
Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit
nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud
officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia
dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris
ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim.
Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.
SRC

def text_to_input(text)
  lines = text.lines(chomp: true)
  if lines.empty?
    return Term.of(Term[], "", :|, "", Term[], Term[])
  end

  Term.of(Term[lines.to_readonly_slice[...-1]], lines[-1], :|, "", Term[], Term[])
end

model = text_to_input(src)

# FIXME: deduplicate (use Alloy template?)
def input(model : Term, style = "font-mono text-neutral-300")
  Term.case(model) do
    matchpi %{[(above_string*) l_string | r_string (below_string*) (_*)]} do
      Term::Dict.build do |commit|
        commit << :group
        commit.with(:style, "content flow-col")

        above.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end

        commit << Term.of(:group,
          Term.of(:p, l, style: style),
          Term.of({:self, :rect}, style: "w-0 h-max bg-blue-500 ring-r ring-blue-500"),
          Term.of(:p, r, style: style),
          style: "content flow-row")

        below.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end
      end
    end

    matchpi %{[(above_string*) l_string (<| selected_string) r_string (below_string*) (_*)]} do
      Term::Dict.build do |commit|
        commit << :group
        commit.with(:style, "content flow-col")

        above.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end

        commit << Term.of(:group,
          Term.of(:p, l, style: style),
          Term.of({:self, :rect}, style: "w-0 h-max bg-yellow-500 ring-l ring-yellow-500"),
          Term.of(:p, selected, style: "#{style} text-white bg-blue-500"),
          Term.of(:p, r, style: style),
          style: "content flow-row")

        below.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end
      end
    end

    matchpi %{[(above_string*) l_string (|> selected_string) r_string (below_string*) (_*)]} do
      Term::Dict.build do |commit|
        commit << :group
        commit.with(:style, "content flow-col")

        above.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end

        commit << Term.of(:group,
          Term.of(:p, l, style: style),
          Term.of(:p, selected, style: "#{style} text-white bg-blue-500"),
          Term.of({:self, :rect}, style: "w-0 h-max bg-yellow-500 ring-r ring-yellow-500"),
          Term.of(:p, r, style: style),
          style: "content flow-row")

        below.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end
      end
    end

    matchpi %{[(above_string*) l_string (<| front_string mid_string* back_string) r_string (below_string*) (_*)]} do
      Term::Dict.build do |commit|
        commit << :group
        commit.with(:style, "content flow-col")

        above.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end

        commit << Term.of(:group,
          Term.of(:p, l, style: style),
          Term.of({:self, :rect}, style: "w-0 h-max bg-yellow-500 ring-l ring-yellow-500"),
          Term.of(:p, front, style: "#{style} text-white bg-blue-500"),
          style: "content flow-row")

        mid.items.each do |line|
          commit << Term.of(:p, line, style: "#{style} text-white bg-blue-500")
        end

        commit << Term.of(:group,
          Term.of(:p, back, style: "#{style} text-white bg-blue-500"),
          Term.of(:p, r, style: style),
          style: "content flow-row")

        below.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end
      end
    end

    matchpi %{[(above_string*) l_string (|> front_string mid_string* back_string) r_string (below_string*) (_*)]} do
      Term::Dict.build do |commit|
        commit << :group
        commit.with(:style, "content flow-col")

        above.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end

        commit << Term.of(:group,
          Term.of(:p, l, style: style),
          Term.of(:p, front, style: "#{style} text-white bg-blue-500"),
          style: "content flow-row")

        mid.items.each do |line|
          commit << Term.of(:p, line, style: "#{style} text-white bg-blue-500")
        end

        commit << Term.of(:group,
          Term.of(:p, back, style: "#{style} text-white bg-blue-500"),
          Term.of({:self, :rect}, style: "w-0 h-max bg-yellow-500 ring-r ring-yellow-500"),
          Term.of(:p, r, style: style),
          style: "content flow-row")

        below.items.each do |line|
          commit << Term.of(:p, line, style: style)
        end
      end
    end
  end
end

require "./uiRb"
require "./sfpaint"

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
