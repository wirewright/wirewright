module Ww::MxRack
  # See `adjust` to learn what adjustments are.
  alias Adjustment = AdjustWindow | AdjustMouse | AdjustMousearea | AdjustInput | AdjustTextoscope | AdjustQuestion

  alias AdjustWindow = AdjustWindowSize | AdjustWindowFocus | AdjustWindowForm |
                       AdjustWindowHover

  defrecord AdjustWindowSize, addr : D7::NodeAddr, width : UInt32, height : UInt32
  defrecord AdjustWindowFocus, addr : D7::NodeAddr, focus : Bool
  defrecord AdjustWindowForm, addr : D7::NodeAddr, form : WindowForm
  defrecord AdjustWindowHover, addr : D7::NodeAddr, hover : Bool

  alias AdjustMouse = AdjustMousePosition | AdjustMousePress | AdjustMouseRelease

  defrecord AdjustMousePosition, addr : D7::NodeAddr, abs : Scenery::Point
  defrecord AdjustMousePress, addr : D7::NodeAddr, button : Term, clicks : UInt32
  defrecord AdjustMouseRelease, addr : D7::NodeAddr, button : Term

  alias AdjustMousearea = AdjustMouseareaPosition | AdjustMouseareaDelta | AdjustMouseareaPress |
                          AdjustMouseareaRelease | AdjustMouseareaClear | AdjustMouseareaShowMouse |
                          AdjustMouseareaHideMouse

  defrecord AdjustMouseareaPosition, addr : D7::NodeAddr, mouse_key : UInt32, abs : Scenery::Point, delta : Scenery::Point?, smart: true
  defrecord AdjustMouseareaDelta, addr : D7::NodeAddr, mouse_key : UInt32, delta : Scenery::Point
  defrecord AdjustMouseareaPress, addr : D7::NodeAddr, mouse_key : UInt32, button : Term, clicks : UInt32
  defrecord AdjustMouseareaRelease, addr : D7::NodeAddr, mouse_key : UInt32, button : Term, reset_delta : Bool
  defrecord AdjustMouseareaClear, addr : D7::NodeAddr
  defrecord AdjustMouseareaShowMouse, addr : D7::NodeAddr, mouse : Term
  defrecord AdjustMouseareaHideMouse, addr : D7::NodeAddr, mouse_key : UInt32

  alias AdjustInput = AdjustInputFocus | AdjustInputBlur | AdjustInputPress |
                      AdjustInputRelease

  defrecord AdjustInputFocus, addr : D7::NodeAddr
  defrecord AdjustInputBlur, addr : D7::NodeAddr
  defrecord AdjustInputPress, addr : D7::NodeAddr, key : Term
  defrecord AdjustInputRelease, addr : D7::NodeAddr, key_index : UInt32

  alias AdjustTextoscope = AdjustTextoscopeView

  defrecord AdjustTextoscopeView, addr : D7::NodeAddr, view : Term

  defrecord AdjustQuestion, addr : D7::NodeAddr, key : Term, answer : Term?

  # E.g. (window _* ⏏width: 500⏏ ⏏height: 400⏏) -> (window _* ⏏width: 800⏏ ⏏height: 600⏏)
  private def adjust(target : Term, adjustment : AdjustWindowSize) : Term
    Term.morph(target, {:width, adjustment.width}, {:height, adjustment.height})
  end

  # E.g. (window _* ⏏focus: true⏏) -> (window _* ⏏focus: false⏏)
  private def adjust(target : Term, adjustment : AdjustWindowFocus) : Term
    Term.morph(target, {:focus, adjustment.focus})
  end

  # E.g. (window _* ⏏hover: true⏏) -> (window _* ⏏hover: false⏏)
  private def adjust(target : Term, adjustment : AdjustWindowHover) : Term
    Term.morph(target, {:hover, adjustment.hover})
  end

  # E.g. (window _* ⏏form: closed⏏) -> (window _* ⏏form: floating⏏)
  private def adjust(target : Term, adjustment : AdjustWindowForm) : Term
    case adjustment.form
    in .closed?    then Term.morph(target, {:form, :closed})
    in .floating?  then Term.morph(target, {:form, :floating})
    in .maximized? then Term.morph(target, {:form, :maximized})
    in .minimized? then Term.morph(target, {:form, :minimized})
    end
  end

  # E.g. (mouse * ⏏l: 0⏏ ⏏t: 0⏏) -> (mouse * ⏏l: 100⏏ ⏏t: 100⏏)
  private def adjust(target : Term, adjustment : AdjustMousePosition) : Term
    Term.morph(target,
      {:l, adjustment.abs.x},
      {:t, adjustment.abs.y},
    )
  end

  # E.g. (mouse *) -> (mouse * ⏏left: 1⏏)
  private def adjust(target : Term, adjustment : AdjustMousePress) : Term
    Term.morph(target, {adjustment.button, adjustment.clicks})
  end

  # E.g. (mouse * ⏏left: 1⏏) -> (mouse *)
  private def adjust(target : Term, adjustment : AdjustMouseRelease) : Term
    Term.morph(target, {adjustment.button, nil})
  end

  # E.g. (mousearea (mouse 0 ⏏l: 0⏏ ⏏t: 0⏏)) -> (mousearea (mouse 0 ⏏l: 100⏏ ⏏t: 100⏏))
  private def adjust(target : Term, adjustment : AdjustMouseareaPosition) : Term
    # Initialize the `dl`, `dt`, or both of the mouse in case it lacks them. This can
    # happen with mousearea position: on-press. Before press the mousearea is oblivious
    # to position and therefore, to `dl` and `dt`. After press the mouse is captured,
    # meaning absolute mouse positions stream directly, and deltas do not, because
    # that would make no sense. The whole point of streaming absolute mouse positions
    # is to be able to continue dragging outside of the mousearea, and outside the mousearea
    # deltas are poorly defined.
    #
    # All this leaves no chance for dl and dt to be initialized in such a configuration.
    # So we try to initialize them here to the first drag point. The main assumption
    # here is that the first drag point lands inside the mousearea. (This is why delta
    # is nilable.) It's not that big of an assumption though. Try to imagine it broken,
    # and your brains will turn into a black hole.
    pass do
      next unless mouse = target[adjustment.mouse_key]?
      next unless delta = adjustment.delta?
      next if mouse.includes?(:l) || mouse.includes?(:t)

      target = Term.morph(target,
        {adjustment.mouse_key, :dl, delta.x},
        {adjustment.mouse_key, :dt, delta.y},
      )
    end

    Term.morph(target,
      {adjustment.mouse_key, :l, adjustment.abs.x},
      {adjustment.mouse_key, :t, adjustment.abs.y},
    )
  end

  # E.g. (mousearea (mouse 0 ⏏dl: 0⏏ ⏏dt: 0⏏)) -> (mousearea (mouse 0 ⏏dl: 100⏏ ⏏dt: 100⏏))
  private def adjust(target : Term, adjustment : AdjustMouseareaDelta) : Term
    Term.morph(target,
      {adjustment.mouse_key, :dl, adjustment.delta.x},
      {adjustment.mouse_key, :dt, adjustment.delta.y},
    )
  end

  # E.g. (mousearea) -> (mousearea ⏏(mouse 0)⏏)
  private def adjust(target : Term, adjustment : AdjustMouseareaShowMouse) : Term
    # TODO: insert sorted by id?
    Term.of(target.append(adjustment.mouse))
  end

  # E.g. (mousearea ⏏(mouse 0)⏏) -> (mousearea)
  private def adjust(target : Term, adjustment : AdjustMouseareaHideMouse) : Term
    Term.of(target.replace(adjustment.mouse_key...adjustment.mouse_key + 1, Term.rep))
  end

  # E.g. (mousearea _ _*) -> (mousearea)
  private def adjust(target : Term, adjustment : AdjustMouseareaClear) : Term
    Term.of(target.replace(1...target.itemsize, Term.rep))
  end

  # E.g. (mousearea (mouse 0)) -> (mousearea (mouse 0 ⏏left: 1⏏))
  private def adjust(target : Term, adjustment : AdjustMouseareaPress) : Term
    Term.morph(target, {adjustment.mouse_key, adjustment.button, adjustment.clicks})
  end

  # E.g. (mousearea (mouse 0 ⏏left: 1⏏)) -> (mousearea (mouse 0))
  private def adjust(target : Term, adjustment : AdjustMouseareaRelease) : Term
    if adjustment.reset_delta
      target = Term.morph(target,
        {adjustment.mouse_key, :dl, nil},
        {adjustment.mouse_key, :dt, nil},
      )
    end

    # Reset drag state (l, t) as well.
    Term.morph(target,
      {adjustment.mouse_key, adjustment.button, nil},
      {adjustment.mouse_key, :l, nil},
      {adjustment.mouse_key, :t, nil},
    )
  end

  # E.g. (keyboard) -> (keyboard ⏏(key backspace)⏏)
  #
  # We insert in sorted order to retain determinism (e.g. for matching chords exactly)
  private def adjust(target : Term, adjustment : AdjustInputPress) : Term
    keys = target.items.move(1)

    index = keys.bsearch_index { |key| Term.compare(key, adjustment.key) >= 0 }
    if index.nil?
      return Term.of(target.append(adjustment.key))
    end

    if keys[index] == adjustment.key
      return target
    end

    index += 1 # Adjust for move(1) above.
    Term.of(target.replace(index...index, Term.rep(adjustment.key)))
  end

  # E.g. (keyboard ⏏(key backspace)⏏) -> (keyboard)
  #      (keyboard ⏏(key backspace)⏏ (key up)) -> (keyboard (key up))
  private def adjust(target : Term, adjustment : AdjustInputRelease) : Term
    Term.of(target.replace(adjustment.key_index...adjustment.key_index + 1, Term.rep))
  end

  # E.g. (keyboard) -> (keyboard ⏏focus: true⏏)
  private def adjust(target : Term, adjustment : AdjustInputFocus) : Term
    Term.of(target.with(:focus, true))
  end

  # E.g. (keyboard ⏏focus: true⏏) -> (keyboard)
  #      (keyboard ⏏(key up) (key backspace) focus: true⏏) -> (keyboard)
  private def adjust(target : Term, adjustment : AdjustInputBlur) : Term
    Term.of(target.with(:focus, nil).replace(1...target.itemsize, Term.rep))
  end

  # (textoscope ⏏(...)⏏ ...)
  private def adjust(target : Term, adjustment : AdjustTextoscopeView) : Term
    Term.of(target.with(1, adjustment.view))
  end

  # (p "Hello World" ⏏?w: _⏏)
  private def adjust(target : Term, adjustment : AdjustQuestion) : Term
    Term.of(target.with(adjustment.key, adjustment.answer || Term.of(:_)))
  end

  private def adjust(tree : D7::InertLeaf, router : D7::NodeAddrRouter) : Term
    raise ArgumentError.new
  end

  private def adjust(tree : D7::GndLeaf, router : D7::NodeAddrRouter) : Term
    assert adjustments = router.value?
    adjustments.reduce(tree.feature.node) do |node, adjustment|
      adjust(node, adjustment)
    end
  end

  private def adjust(tree : D7::ScopeNode, router : D7::NodeAddrRouter) : Term
    adjust(tree.child, router)
  end

  private def adjust(tree : D7::MixtureNode, router : D7::NodeAddrRouter) : Term
    tree.feature.mix.call(adjust(tree.child, router))
  end

  private def adjust(tree : D7::CircuitNode, router : D7::NodeAddrRouter) : Term
    adjust(tree.to_group, router)
  end

  private def adjust(tree : D7::GroupNode, router : D7::NodeAddrRouter) : Term
    if router.has_children?
      # Apply child adjustments.
      result = tree.feature.node.transaction do |commit|
        tree.children.zip(tree.feature.range) do |child, key|
          next unless ahead = router.cd?(key)

          commit.with(key, adjust(child, ahead))
        end
      end
      result = Term.of(result)
    else
      result = Term.of(tree.feature.node)
    end

    # Apply adjustments to the parent itself.
    if bucket = router.value?
      return bucket.reduce(result) { |memo, action| adjust(memo, action) }
    end

    result
  end

  # Applies zero or more *adjustments* to node(s) in the given *tree*. Returns
  # the resulting possibly modified circuit.
  #
  # NOTE: There is no fast path for empty *adjustments*. Consider doing the check
  # on the calling side.
  #
  # Instead of modifying the circuit "online" as we process events and other
  # kinds of perturbations, we collect so-called *adjustments*, of the `Adjustment`
  # type. The resulting list of adjustments is then applied to the circuit in bulk,
  # in one transaction, to ground and parent nodes in *tree*.
  #
  # The adjustments are applied in a bottom-up way: first, the children are adjusted,
  # and then their parents, and so on up the tree.
  #
  # A *conflict* occurs when two or more adjustments of the same ground or parent
  # node write to the same entry of the node or interfere with each other in some
  # other way. There is no special handling of conflicts in `adjust`: conflicting
  # adjustments will run in their `Slice` order, so the later one in the given
  # `Slice` will stomp (or stumble) over changes in the former one.
  #
  # So it is safer to say that `adjust` does not expect conflicting adjustments,
  # even if it can handle most of them just fine.
  #
  # Adjustments currently do not validate their target node. For example,
  # `AdjustWindowForm` will work on any node and not just on `window` nodes,
  # although it should probably do the latter eventually. Right now this is not
  # necessary since *adjustments* are expected to come from a trusted source.
  def adjust(tree : D7::ParseTree, adjustments : Slice(Adjustment)) : Term
    # Bucketize adjustments while preserving their original order in-bucket.
    buckets = {} of D7::NodeAddr => Array(Adjustment)
    adjustments.each do |adjustment|
      bucket = buckets.put_if_absent(adjustment.addr) { [] of Adjustment }
      bucket << adjustment
    end

    # Convert bucket map to an addr router of buckets.
    routerB = D7::NodeAddrRouter::Builder(Array(Adjustment)).new
    buckets.each do |addr, bucket|
      routerB[addr] = bucket
    end

    adjust(tree, routerB.router)
  end
end
