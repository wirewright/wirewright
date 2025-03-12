require "./src/wirewright"
require "./uiRb"
require "./sfuiR"

module UIR::Platform
  alias Current = SFML
end

state0 = ML.term <<-WWML
((self window) style: "bg-neutral-900 max origin" max-w: 1000 max-h: 800
  (g style: "max flow-col gap-3 p-3"
    (g style: "bg-neutral-800 w-max h-content px-2 py-1 rounded-sm"
      (p "Wirewright µsoma" style: "text-neutral-400 text-xs"))
    (g style: "w-max h-content border border-neutral-700 rounded flow-col"
      (p "Buttons" style: "text-lg text-neutral-300 d-center w-max h-content py-2")
      ((self rect) style: "w-max h-px bg-neutral-700")
      (g style: "w-max h-content p-2 gap-2"
        (button style: "bg-neutral-700 hover:bg-blue-500 hover:cursor-pointer rounded-sm px-4 py-2 text-sm text-neutral-50 content font-sans font-medium" id: btn1
          "Button 1")
        (button style: "hover:bg-blue-500 hover:cursor-pointer px-2 py-1 text-sm rounded-xs text-neutral-50 border border-neutral-500 hover:border-blue-500 content font-mono font-medium" id: btn2
          "Button 2")))))
WWML

ui = UIR::Reducers.microfold(state0) do |state, drawable, event|
  Term.case(event) do
    matchpi %{(motion x_number y_number)} do
      if prev = state[:hovered]?
        Keypath.each_item(state) do |keypath, item|
          next unless item = item.as_d?
          next unless prev == item[:id]?

          state = Term.of(state.as_d.follow(keypath) { item.morph({:hover, nil}).upcast })

          false # break
        end
      end

      state = Term.of(state.morph({:hovered, nil}))

      UIR.hit(drawable, x.unsafe_as_n, y.unsafe_as_n) do |kp|
        node = drawable.follow(kp)
        next unless id = node[:id]?

        state = Term.of(state.morph({:hovered, id}))

        Keypath.each_item(state) do |keypath, item|
          next unless item = item.as_d?
          next unless id == item[:id]?

          state = Term.of(state.as_d.follow(keypath) { item.morph({:hover, true}).upcast })

          false # break
        end
      end

      # puts ML.display(state)
    end

    otherwise { }
  end

  state
end

UIR::Platform::Current.show(ui)

