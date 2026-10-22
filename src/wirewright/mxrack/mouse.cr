module Ww::MxRack
  @[::Flags]
  enum MouseFlags
    WantsPositionAlways
    WantsPositionOnPress
  end

  # Represents the relevant parts of a `mouse` node (`mxrack.mouse`).
  defrecord Mouse,
    addr : D7::NodeAddr,
    id : UInt64?,
    flags : MouseFlags,
    copying: true,
    smart: true

  defrecord MouseArea,
    addr : D7::NodeAddr,
    mice : Slice(MouseAreaMouse),
    flags : MouseFlags,
    copying: true

  defrecord MouseAreaMouse, key : UInt32, id : UInt64

  private def mouseareas(tree : D7::InertLeaf) : Slice(MouseArea)
    Slice(MouseArea).empty
  end

  private def mouseareas(tree : D7::GndLeaf) : Slice(MouseArea)
    node = tree.feature.node

    Term.case(node) do
      matchpi %{(mousearea _* ⍊ position_⋮ false)} do
        subterms = node.items.move(1)
        mice = subterms.to_compact_readonly_slice do |subterm, index|
          Term.matchpi?(subterm, %{[mouse id←(%number u64)]}) do
            # +1 for `mousearea`.
            MouseAreaMouse.new(1u32 + index, id.to(UInt64))
          end
        end

        flags = MouseFlags::None
        case position
        when Term.of(:always)
          flags |= MouseFlags::WantsPositionAlways
        when Term.of(:"on-press")
          flags |= MouseFlags::WantsPositionOnPress
        end

        mousearea = MouseArea.new(D7::NodeAddr.empty, mice, flags)
        Slice(MouseArea).of(mousearea)
      end

      otherwise do
        Slice(MouseArea).empty
      end
    end
  end

  private def mouseareas(tree : D7::ScopeNode | D7::MixtureNode) : Slice(MouseArea)
    mouseareas(tree.child)
  end

  private def mouseareas(tree : D7::CircuitNode) : Slice(MouseArea)
    mouseareas(tree.to_group)
  end

  # :nodoc:
  SYM_MOUSEAREA = Term.of(:mousearea)

  private def mouseareas(tree : D7::GroupNode) : Slice(MouseArea)
    unless tree.summary.has_head?(SYM_MOUSEAREA)
      return Slice(MouseArea).empty
    end

    mouseareas = Pf::Kit.stack_array(MouseArea, 4)

    tree.children.zip(tree.feature.range) do |child, key|
      addr = D7::NodeAddr[key]
      mouseareas(child).each do |mousearea|
        mouseareas << mousearea.copy_with(addr: D7::NodeAddr.concat(addr, mousearea.addr))
      end
    end

    mouseareas.to_unsafe_readonly_slice!
  end

  # Returns a read-only slice of mouse areas (`mxrack.mousearea` nodes) that
  # belong to the given window node, *window*.
  def mouseareas(window : WindowNode) : Slice(MouseArea)
    mouseareas(window.tree).to_readonly_slice do |mousearea|
      mousearea.copy_with(addr: D7::NodeAddr.concat(window.addr, mousearea.addr))
    end
  end

  private def mice(tree : D7::InertLeaf) : Slice(Mouse)
    Slice(Mouse).empty
  end

  private def mice(tree : D7::GndLeaf) : Slice(Mouse)
    node = tree.feature.node

    Term.case(node) do
      matchpi(
        %{(mouse idQ←*  ⍊ position_⋮ false)},
        %{(mouse idQ←(%number u64) ⍊ position_⋮ false)},
      ) do
        id = idQ == Term.of(:*) ? nil : idQ.to(UInt64)

        flags = MouseFlags::None
        case position
        when Term.of(:always)
          flags |= MouseFlags::WantsPositionAlways
        when Term.of(:"on-press")
          flags |= MouseFlags::WantsPositionOnPress
        end

        mousearea = Mouse.new(D7::NodeAddr.empty, id, flags)
        Slice(Mouse).of(mousearea)
      end

      otherwise do
        Slice(Mouse).empty
      end
    end
  end

  private def mice(tree : D7::ScopeNode | D7::MixtureNode) : Slice(Mouse)
    mice(tree.child)
  end

  private def mice(tree : D7::CircuitNode) : Slice(Mouse)
    mice(tree.to_group)
  end

  # :nodoc:
  SYM_MOUSE = Term.of(:mouse)

  private def mice(tree : D7::GroupNode) : Slice(Mouse)
    unless tree.summary.has_head?(SYM_MOUSE)
      return Slice(Mouse).empty
    end

    mice = Pf::Kit.stack_array(Mouse, 4)

    tree.children.zip(tree.feature.range) do |child, key|
      addr = D7::NodeAddr[key]
      mice(child).each do |mouse|
        mice << mouse.copy_with(addr: D7::NodeAddr.concat(addr, mouse.addr))
      end
    end

    mice.to_unsafe_readonly_slice!
  end

  # Returns a read-only slice of mice (`mxrack.mouse` nodes) that belong to
  # the given window node, *window*.
  def mice(window : WindowNode) : Slice(Mouse)
    mice(window.tree).to_readonly_slice do |mouse|
      mouse.copy_with(addr: D7::NodeAddr.concat(window.addr, mouse.addr))
    end
  end
end
