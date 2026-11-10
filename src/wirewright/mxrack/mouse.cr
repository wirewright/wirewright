module Ww::MxRack
  @[::Flags]
  enum MouseFlags
    WantsPositionAlways
    WantsPositionDrag
  end

  # Represents the relevant parts of a `mouse` node (`mxrack.mouse`).
  defrecord Mouse,
    addr : D7::NodeAddr,
    id : UInt64?,
    flags : MouseFlags,
    copying: true,
    smart: true

  @[::Flags]
  enum MouseareaFlags
    WantsPositionAlways
    WantsPositionDrag
    Opaque
  end

  defrecord MouseArea,
    addr : D7::NodeAddr,
    mice : Slice(MouseAreaMouse),
    flags : MouseareaFlags,
    copying: true

  defrecord MouseAreaMouse, key : UInt32, id : UInt64

  private def mouseareas(sink, addr : D7::NodeAddr, tree : D7::InertLeaf) : Nil
  end

  private def mouseareas(sink, addr : D7::NodeAddr, tree : D7::GndLeaf) : Nil
    node = tree.feature.node

    Term.matchpi?(node, <<-'WWML') do
    (mousearea _*
      ⍊ position_: (%optional never (%any never always drag))
        opacity_: (%optional opaque (%any transparent opaque)))
    WWML
      miceQ = node.items.move(1)
      mice = miceQ.to_compact_readonly_slice do |mouseQ, index|
        Term.matchpi?(mouseQ, %{[mouse id←(%number u64)]}) do
          # +1 for `mousearea`.
          MouseAreaMouse.new(1u32 + index, id.to(UInt64))
        end
      end

      flags = MouseareaFlags::None
      case position
      when Term.of(:always)
        flags |= MouseareaFlags::WantsPositionAlways
      when Term.of(:drag)
        flags |= MouseareaFlags::WantsPositionDrag
      end
      case opacity
      when Term.of(:opaque)
        flags |= MouseareaFlags::Opaque
      end

      sink << MouseArea.new(addr, mice, flags)
    end
  end

  private def mouseareas(sink, addr : D7::NodeAddr, tree : D7::ScopeNode | D7::MixtureNode) : Nil
    mouseareas(sink, addr, tree.child)
  end

  private def mouseareas(sink, addr : D7::NodeAddr, tree : D7::CircuitNode) : Nil
    mouseareas(sink, addr, tree.to_group)
  end

  # :nodoc:
  SYM_MOUSEAREA = Term.of(:mousearea)

  private def mouseareas(sink, addr : D7::NodeAddr, tree : D7::GroupNode) : Nil
    return unless tree.summary.has_head?(SYM_MOUSEAREA)

    tree.children.zip(tree.feature.range) do |child, key|
      mouseareas(sink, addr.append(key), child)
    end
  end

  # Returns a read-only slice of mouseareas (`mxrack.mousearea` nodes) that
  # belong to the given window node *window*.
  def mouseareas(window : WindowNode) : Slice(MouseArea)
    sink = Pf::Kit.stack_array(MouseArea, 64)
    mouseareas(sink, window.addr, window.tree)
    sink.to_unsafe_readonly_slice!
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
        when Term.of(:drag)
          flags |= MouseFlags::WantsPositionDrag
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
