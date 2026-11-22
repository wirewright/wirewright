module Ww::MxRack
  defcase Automaton,
    addr : D7::NodeAddr,
    scope : Slice(Term),
    key : Term,
    workspace : Term::Dict,
    draft : Term

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::InertLeaf | D7::GndLeaf) : Nil
  end

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::MixtureNode | D7::ScopeNode) : Nil
    automata(sink, addr, scope, tree.child)
  end

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::CircuitNode) : Nil
    automata(sink, addr, scope, tree.to_group)
  end

  # :nodoc:
  SYM_AUTOMATON = Term.of(:automaton)

  # :nodoc:
  def automata(sink, addr : D7::NodeAddr, scope : Slice(Term), tree : D7::GroupNode) : Nil
    return unless tree.summary.has_head?(SYM_AUTOMATON)

    Term.case(tree.feature.node) do
      matchpiT %{[automaton (key_ workspace_dict) draft_*]} do
        # Automata are scoped to make sure you can run the same circuit recursively
        # in an automaton. Otherwise we'd get key collisions which would mess
        # everything up.
        subscope = scope.append(key)

        sink << Automaton.new(addr, subscope, Term.of(subscope), workspace, draft)

        tree.children.zip(tree.feature.range) do |child, key|
          automata(sink, addr.append(key), subscope, child)
        end
      end

      otherwise do
        tree.children.zip(tree.feature.range) do |child, key|
          automata(sink, addr.append(key), scope, child)
        end
      end
    end
  end

  # Returns automata projections of the given *circuit*. Automata are ordered
  # by depth: *deepest ones go first*.
  def automata(circuit : D7::ParseTree) : Slice(Automaton)
    sink = Pf::Kit.stack_array(Automaton, 8)
    addr = D7::NodeAddr.empty
    scope = Slice(Term).empty
    automata(sink, addr, scope, circuit)

    sink.sort_by! { |automaton| -automaton.addr.size }
    sink.to_unsafe_readonly_slice!
  end

  def windows_with_automata(windows : Array(Window), circuit : D7::ParseTree)
    automata = automata(circuit)

    au_windows = {} of Automaton => Array(Window)
    au_windows.compare_by_identity

    windows = windows.select do |projection|
      automaton = automata.find do |automaton|
        next unless projection.addr.size > automaton.addr.size
        next unless projection.addr.starts_with?(automaton.addr)

        true
      end

      if automaton.nil?
        next true # keep, doesn't belong to any automaton
      end

      bucket = au_windows.put_if_absent(automaton) { [] of Window }
      bucket << projection

      false # reject
    end

    {windows, automata, au_windows}
  end

  # :nodoc:
  SYM_WIDGET = Term[:widget]

  # :nodoc:
  defcase ReprContext,
    setup : WindowingSetup,
    components : ViComponentList,
    circuit : D7::ParseTree,
    hg : D7::Hypergraph,
    prefix : D7::NodeAddr,
    token : Term::Num

  private def repr(ctx : ReprContext, tree : D7::InertLeaf, addr : D7::NodeAddr) : Term
    Term.of(:inert, tree.feature.node)
  end

  private def repr(ctx : ReprContext, tree : D7::GndLeaf, addr : D7::NodeAddr) : Term
    Term.of(:gnd, tree.feature.node, addr: addr)
  end

  private def repr(ctx : ReprContext, tree : D7::MixtureNode, addr : D7::NodeAddr) : Term
    Term.of(:inert, tree.feature.node)
  end

  private def repr(ctx : ReprContext, tree : D7::ScopeNode, addr : D7::NodeAddr) : Term
    repr(ctx, tree.child, addr)
  end

  private def repr(ctx : ReprContext, tree : D7::CircuitNode, addr : D7::NodeAddr) : Term
    repr(ctx, tree.to_group, addr)
  end

  private def repr(ctx : ReprContext, tree : D7::GroupNode, addr : D7::NodeAddr) : Term
    parent = tree.feature

    # If there is a passable path to this `widget` node, we visualize it. If there is
    # no such path (e.g. widget is inside a supervisor template), we simply render
    # the `widget` as a parent.
    pass do
      next unless parent.node.probably_includes?(SYM_WIDGET)
      next unless parent.signature.try(&.head) == SYM_WIDGET
      next unless row = D7.follow?(ctx.circuit, addr)

      _, widget_tree = row
      next unless widget_tree.is_a?(D7::GroupNode)
      next unless widget_tree.feature.node.same?(parent.node)
      next if editing?(Term.of(parent.node), ctx.token)

      recognized = ViRecognized.recognize(ctx.setup, ctx.components, widget_tree)

      # Prepend addr to roots.
      recognized = recognized.to_readonly_slice do |root|
        offset = 0u32
        ViAddrSeq(ViRecognized::Node).new(addr, offset, root).as(ViRecognized::Node)
      end

      instance = ViInstantiated.instantiate(ctx.hg, recognized)

      # Turn circuit-relative addresses into global ones by prepending prefix. E.g. mousearea
      # addresses must be global, we can't leave them circuit-local.
      instance = instance.to_readonly_slice do |root|
        offset = 2u32
        ViAddrSeq(ViInstantiated::Node).new(ctx.prefix, offset, root).as(ViInstantiated::Node)
      end

      situated = ViSituated.situate(instance)
      visuals = ViSituated.distill(situated)
      return Term.of(:widget, visuals, addr: addr)
    end

    repr = Term::Dict.build do |commit|
      commit << :parent
      commit.with(:addr, addr)
      commit.with(:reference, parent.node)

      # Mark parents that contain the editor with editing: true.
      if editing?(Term.of(parent.node), ctx.token)
        commit.with(:editing, true)
      end

      parent.node.items.each_with_index do |item, key|
        key = key.to_u32

        unless key.in?(parent.range)
          commit << {:inert, item}
          next
        end

        index = key - parent.range.begin
        child = tree.children[index]
        commit << repr(ctx, child, addr.append(key))
      end
    end

    Term.of(repr)
  end

  def repr(setup : WindowingSetup, prefix : D7::NodeAddr, repr_tree : D7::ParseTree, circuit_tree : D7::ParseTree, token : Term::Num) : Term
    components = ViRecognized.components(circuit_tree)
    hg = D7::Hypergraph.new(circuit_tree)
    ctx = stack_alloc ReprContext.new(setup, components, circuit_tree, hg, prefix, token)
    repr(ctx, repr_tree, D7::NodeAddr.empty)
  end

  def reprclf(token : Term::Num) : D7::Classifier
    successor = auclf(token)

    ->(node : Term) do
      feature = successor.call(node)

      Term.case(node) do
        matchpi %{(slot _ _? ⍊ -open)} do
          continue if feature.is_a?(D7::Inert)
          continue if editing?(node, token)

          D7.gnd(node)
        end

        matchpi %{[slot _ _?]} do
          continue if feature.is_a?(D7::Inert)

          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        matchpi %{[node _ _]} do
          continue if feature.is_a?(D7::Inert)

          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        matchpi %{(section _string _* ⍊ open)} do
          continue if feature.is_a?(D7::Inert)

          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        matchpi %{(section _string _* ⍊ -open)} do
          continue if feature.is_a?(D7::Inert)
          continue if editing?(node, token)

          D7.gnd(node)
        end

        matchpi %{(rule _ _ ⍊ open)} do
          D7.parent(node.as_d, 2u32...3u32)
        end

        matchpi %{(component _dict _*)} do
          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        matchpi %{(rule _ _ ⍊ -open)} do
          continue if feature.is_a?(D7::Inert)

          D7.gnd(node)
        end

        matchpi %{[rewriter _*]} do
          continue if feature.is_a?(D7::Inert)

          D7.gnd(node)
        end

        matchpi %{(device _* ⍊ -open)} do
          continue if feature.is_a?(D7::Inert)
          continue if editing?(node, token)

          D7.gnd(node)
        end

        # It's unconditionally a parent node for MuSoma. For Rack, on the other
        # hand, it's a ground node.
        matchpi %{[server [@_ _ _?] _*]} do
          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        matchpi %{(supervisor (@_ @_ _ - @_) _* ⍊ -open)} do
          continue if feature.is_a?(D7::Inert)
          continue if editing?(node, token)

          D7.gnd(node)
        end

        matchpi %{[supervisor (@_ @_ _ - @_) _*]} do
          continue if feature.is_a?(D7::Inert)

          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        otherwise do
          feature
        end
      end
    end
  end

  def nopclf : D7::Classifier
    ->(node : Term) do
      D7.inert(node).as(D7::Feature)
    end
  end

  def transcribe(window : Window) : Term
    Term.morph(window.scenery,
      {:key, window.key},
      {:title, window.spec.title},
      {:width, window.spec.width},
      {:height, window.spec.height},
      {:cursor, transcribe(window.spec.cursor)},
      {:backdrop, window.spec.backdrop},
      {:form, transcribe(window.spec.form)},
    )
  end

  def transcribe(form : WindowForm) : Term
    case form
    in .closed?    then Term.of(:closed)
    in .floating?  then Term.of(:floating)
    in .minimized? then Term.of(:minimized)
    in .maximized? then Term.of(:maximized)
    end
  end

  def transcribe(cursor : MouseCursor) : Term
    # If there are multiple aliases we'd just use whichever happens to be
    # the first one.
    term, _ = CURSOR_MAP.find! { |_, candidate| candidate == cursor }
    term
  end

  def manipulate(matchee : Term, addr : D7::NodeAddr, index : UInt32, backsys : Slice({Term, Term})) : Term
    if index == addr.size
      return M1.backmap(backsys, matchee)
    end

    unless dict = matchee.as_d?
      return matchee
    end

    key = addr[index]
    unless value0 = dict[key]?
      return matchee
    end

    value1 = manipulate(value0, addr, index + 1, backsys)
    Term.of(dict.with(key, value1))
  end
end
