require "./src/wirewright"
require "./uiRb"
require "./sfpaint"

alias UIR::Platform::Current = SFML

source = Atomic(String).new("")
changed = Atomic(Bool).new(false)
watch(Path["ui-pg.wwml"]) do |updated|
  source.set(updated.gets_to_end)
  changed.set(true)
end

fallback = ML.term <<-WWML
((self window) style: "bg-red-950 max origin" max-w: 1000 max-h: 800
  (group style: "px-5 py-5 flow-col gap-5 text-red-100"
    (icon "error" style: "text-7xl py-5")
    (h1 "Oops. Something went wrong!!1")
    (p "It appears there's an error in the playground file." style: "text-red-200")))
WWML

#   (group style: "flow-col gap-1"
#     (^let x: (1 2 3)
#       (p ^\\x))))
# ;;    (h1 "Completed")
# ;;    ((self hr rect) style: "w-max")
# ;;    (^each (todos as {¦ title_string completed: true})
# ;;      (p ^title style: "text-green-500"))
# ;;    (h1 "Not completed")
# ;;    ((self hr rect) style: "w-max")
# ;;    (^each (todos as {¦ title_string completed: false})
# ;;      (p ^title style: "text-orange-500"))))
# WWML

# A simple goal would be to create: an input box and a list of items below it.
# I can type to search. I can hover over an item. I can click on an item to
# select it. There is a text above items that shows which item is currently selected.

# model = ML.term <<-WWML
# (frame
#   (input "" | "")
#   (list
#     (item "Apple")
#     (item "Banana")
#     (item "Cherry")
#     (item "Date")
#     (item "Elderberry")
#     (item "Fig")
#     (item "Grape")
#     (item "Honeydew")
#     (item "Kiwi")
#     (item "Lemon")))
# WWML

# env = Term[
#   todos: {
#     {
#       title:     "Lorem ipsum",
#       completed: false,
#     },
#     {
#       title:     "Lorem ipsum 2",
#       completed: true,
#     },
#     {
#       title:     "Lorem ipsum 3",
#       completed: false,
#     },
#   },
# ]
# frame = Alloy.render(env, template)
frame = fallback

vw = frame[:"max-w"]
vh = frame[:"max-h"]

ui = UIR::Reducers.microfold(frame) do |_, drawable, event|
  if changed.swap(false)
    begin
      updated = ML.term(source.get)
    rescue e : ML::SyntaxError
      Log.error(exception: e)
      frame = fallback
    else
      frame = updated
    end
  end

  Term.case(event) do
    matchpi %{(size w_number h_number)} do
      vw = w
      vh = h
    end

    matchpi %{(key enter)} do
      puts ML.display(drawable)
    end

    otherwise { }
  end

  frame = Term.of(frame.morph({:"max-w", vw}, {:"max-h", vh}))

  Term.of(frame)
end

UIR::Platform::Current.show(ui)
