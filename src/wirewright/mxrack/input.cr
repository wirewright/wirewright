module Ww::MxRack
  alias Input = RuneInput | KeyboardInput

  # Represents the relevant parts of an `[input _*]` (`mxrack.input`) node
  # in the circuit.
  defrecord RuneInput,
    addr : D7::NodeAddr,
    keys : Slice(InputKey),
    focus : InputFocus,
    copying: true

  # Represents the relevant parts of a `[keyboard _*]` (`mxrack.keyboard`)
  # node in the circuit.
  defrecord KeyboardInput,
    addr : D7::NodeAddr,
    keys : Slice(InputKey),
    focus : InputFocus,
    copying: true

  # Represents an input key, `repr←[key _]` for `mxrack.keyboard`, and `repr←[key _]`
  # or `repr←[rune _string]` for `mxrack.input`.
  defrecord InputKey, index : UInt32, repr : Term

  # |@ mxrack.input.focus
  #
  # |@summary
  # The possible values for the `focus: _` pair, which controls whether and how
  # an `mxrack.input` node receives keys and runes.
  enum InputFocus
    # |@ mxrack.[input focus]
    #
    # |@pattern
    # false
    #
    # |@block
    # The input is not linked to the keyboard, can be a recipient of focus, can be
    # a donor of focus.
    Inactive

    # |@ mxrack.[input focus]
    #
    # |@pattern
    # true
    #
    # |@block
    # The input is linked to the keyboard, can be a recipient of focus, can be
    # a donor of focus.
    Active

    # |@ mxrack.[input focus]
    #
    # |@pattern
    # wanted
    #
    # |@block
    # The input is not linked to the keyboard. It is politely asking all focused
    # inputs to donate their focus to itself.
    Wanted

    # |@ mxrack.[input focus]
    #
    # |@pattern
    # always
    #
    # |@block
    # The input is linked to the keyboard. It cannot be a recipient or donor
    # of focus. Even if another input seizes focus, this input will continue
    # receiving keys.
    AlwaysActive

    # |@ mxrack.[input focus]
    #
    # |@pattern
    # disabled
    #
    # |@block
    # The input is not linked to the keyboard. It cannot be a recipient or donor
    # of focus.
    AlwaysInactive

    # |@ mxrack.[input focus]
    #
    # |@pattern
    # seized
    #
    # |@block
    # The input is linked to the keyboard. It cannot be a recipient or donor
    # of focus. It will refuse all `wanted` requests and will blur all focused
    # (`focus: true`) inputs while active. `focus: seized` does not affect
    # `focus: always` inputs.
    Seized

    def should_receive_keys? : Bool
      active? || always_active? || seized?
    end
  end

  # Returns the `InputFocus` that *term* stands for, if any.
  def input_focus?(term : Term) : InputFocus?
    case term
    when Term.of(false)     then InputFocus::Inactive
    when Term.of(true)      then InputFocus::Active
    when Term.of(:wanted)   then InputFocus::Wanted
    when Term.of(:always)   then InputFocus::AlwaysActive
    when Term.of(:disabled) then InputFocus::AlwaysInactive
    when Term.of(:seized)   then InputFocus::Seized
    end
  end

  private def inputs(tree : D7::InertLeaf) : Slice(Input)
    Slice(Input).empty
  end

  private def inputs(tree : D7::GndLeaf) : Slice(Input)
    node = tree.feature.node

    Term.case(node) do
      matchpi %{(input _* ⍊ focus: (%optional false focusQ_))} do
        continue unless focus = input_focus?(focusQ)

        subterms = node.items.move(1)
        keys = subterms.to_compact_readonly_slice do |subterm, index|
          Term.matchpi?(subterm, %{[rune _]}, %{[key _]}) do
            # +1 for `input`, all keys come after it.
            InputKey.new(1u32 + index.to_u32, subterm)
          end
        end

        input = RuneInput.new(D7::NodeAddr.empty, keys, focus)
        Slice(Input).of(input)
      end

      matchpi %{(keyboard _* ⍊ focus: (%optional false focusQ_))} do
        continue unless focus = input_focus?(focusQ)

        subterms = node.items.move(1)
        keys = subterms.to_compact_readonly_slice do |subterm, index|
          Term.matchpi?(subterm, %{[key _]}) do
            # +1 for `keyboard`, all keys come after it.
            InputKey.new(1u32 + index.to_u32, subterm)
          end
        end

        input = KeyboardInput.new(D7::NodeAddr.empty, keys, focus)
        Slice(Input).of(input)
      end

      otherwise do
        Slice(Input).empty
      end
    end
  end

  private def inputs(tree : D7::ScopeNode | D7::MixtureNode) : Slice(Input)
    inputs(tree.child)
  end

  private def inputs(tree : D7::CircuitNode) : Slice(Input)
    inputs(tree.to_group)
  end

  # :nodoc:
  SYM_INPUT = Term.of(:input)

  # :nodoc:
  SYM_KEYBOARD = Term.of(:keyboard)

  private def inputs(tree : D7::GroupNode) : Slice(Input)
    summary = tree.summary
    unless summary.has_head?(SYM_INPUT) || summary.has_head?(SYM_KEYBOARD)
      return Slice(Input).empty
    end

    inputs = Pf::Kit.stack_array(Input, 4)

    tree.children.zip(tree.feature.range) do |child, key|
      addr = D7::NodeAddr[key]
      inputs(child).each do |input|
        inputs << input.copy_with(addr: D7::NodeAddr.concat(addr, input.addr))
      end
    end

    inputs.to_unsafe_readonly_slice!
  end

  # Returns a read-only slice of inputs that belong to the given window
  # node, *window*.
  def inputs(window : WindowNode) : Slice(Input)
    inputs(window.tree).to_readonly_slice do |input|
      input.copy_with(addr: D7::NodeAddr.concat(window.addr, input.addr))
    end
  end
end
