module Ww::MxRack
  # Returns `true` if *node* is a *visual* node, that is, a node that must be
  # retained for visualization purposes.
  #
  # This function requires a Microfold codex because whether or not a *node* is
  # visual may depend on whether or not it is a Microfold preset node.
  #
  # See `mxrack.[visual]` in the doctool for more info on how visual nodes are found.
  def visual?(microfold : Microfold::SyncCodex, node : Term::Dict) : Bool
    return false unless head = node.items.first?

    # |@ mxrack.[visual]
    #
    # |@summary
    # "Traits" of nodes that make them *visual* for MxRack.

    # |@ mxrack.[visual]
    #
    # |@block
    # A node with a `style` pair is unconditionally a visual node.
    #
    # |@example
    # Leaf:
    # ```wwml
    # (foo style: "bg-red-500")
    # ```
    #
    # Parent:
    # ```wwml
    # (group style: "bg-red-500"
    #   (cell @x 0)
    #   (p @x))
    #
    # (module {} style: "bg-red-500"
    #   (cell @x 0)
    #   (p @x))
    # ```
    if node.includes?(:style)
      return true
    end

    # |@ mxrack.[visual]
    #
    # |@block
    # Style-less groups are **not** visual nodes because they are not necessarily
    # visual -- most often theylre not.
    #
    # |@example
    # The nested group is **not** visual because it does not define a `style: _` (even
    # an empty style would do); so paragraphs render in a row:
    #
    # ```wwml
    # (group style: "flow-row"
    #   (group
    #     (p "A")
    #     (p "B")))
    # ```
    #
    # The moment we add a `style` pair to the nested group it renders the paragraphs
    # in a column:
    #
    # ```wwml
    # (group style: "flow-row"
    #   (group style: "" ;; Added empty `style`
    #     (p "A")
    #     (p "B")))
    # ```
    #
    # This is because the nested group is now treated as a visual node. According
    # to the standard Microfold codex, a `group` is `flow-col` by default, so what
    # we have here, visually, is a row with one element in it, that element being
    # a column of two paragraphs A and B.
    if head == Term.of(:group)
      return false
    end

    # |@ mxrack.[visual]
    #
    # |@block
    # A node with a preset head is a visual node. Whether or not the head of
    # a node is a preset head is determined based on the Microfold codex passed
    # to MxRack. Most often this is the standard Microfold codex, which defines
    # style *presets* for heads `p`, `box`, `h1`, etc.
    #
    # |@example
    # ```wwml
    # (h1 "Hello World")
    # (p "Count is 0")
    # ```
    #
    # In this example, `h1` and `p` are both preset heads (according to the standard
    # Microfold codex), so they are both treated as visual nodes.
    if microfold.preset?(head)
      return true
    end

    # |@ mxrack.[visual]
    #
    # |@block
    # A node with a recognizable Scenery leaf or parent head is a visual node.
    # See `scenery` for a list of Scenery nodes.
    #
    # |@example
    # A Scenery leaf head such as `img` (`scenery.img`) turns the node into
    # a visual node:
    #
    # ```wwml
    # (img src: (uri "https://picsum.photos/300/300"))
    # ```
    #
    # Similarly, a Scenery parent head such as `x-stack` (`scenery.stack`) turns
    # it into a visual node:
    #
    # ```wwml
    #  (x-stack
    #    (cell @x "A") ;; < These are non-visual so they are dropped when visualizing the circuit.
    #    (cell @y "B") ;; <
    #    (cell @z "C") ;; <
    #    (p @x)  ;; <
    #    (p @y)  ;; <
    #    (p @z)) ;; < Whereas these are visual so they are retained.
    # ```
    if Scenery::KnowledgeBase.leaf_head?(head) || Scenery::KnowledgeBase.parent_head?(head)
      return true
    end

    false
  end

  # :ditto:
  def visual?(microfold : Microfold::SyncCodex, node : Term) : Bool
    return false unless dict = node.as_d?

    visual?(microfold, dict)
  end

  # :nodoc:
  defrecord VisualizeContext,
    microfold : Microfold::SyncCodex,
    hg : D7::Hypergraph,
    questions : Array(Question)

  # :nodoc:
  #
  # Represents an enclosing `ensemble` node containing a number of consumable *args*.
  # `site` nodes consume the args.
  defrecord Ensemble, args : Deque(Term)

  # FIXME: Do not use exceptions for control flow!
  private class VisualizeEdgeUndefined < Exception
    @callstack = CallStack.empty
  end

  # The eval function we pass to Nitrene which it then calls whenever it wants
  # us to evaluate a term. More specifically, this is a *composite* function,
  # although there is no difference at this level.
  #
  # So when Nitrene asks us to evaluate a term, and that term is an edge, we
  # use the hypergraph to determine the value at the edge, and return that
  # to Nitrene. If the edge does not exist we blow up with `VisualizeEdgeUndefined`.
  private def visualize_edge_eval(ctx : VisualizeContext, addr : D7::NodeAddr) : Nitrene::Eval
    Nitrene::Eval.new do |it, vars, expr|
      unless Term.edge?(expr)
        next Nitrene.inert
      end

      unless value = Rack.cell?(ctx.hg, ctx.hg.resolve(addr, expr)).try(&.value?)
        raise VisualizeEdgeUndefined.new
      end

      value
    end
  end

  # :nodoc:
  SYM_EDGE = Term.of(:edge)
  # :nodoc:
  SYM_CARET = Term.of(:^)

  # Here we replace pairs like `foo: ^100` or `show: ^(> @count 0)` or `@:hover` with
  # their values, if any. If a pair has no value we omit it.
  private def visualize_postprocess(ctx : VisualizeContext, addr : D7::NodeAddr, node : Term) : Term
    return node unless dict0 = node.as_d?
    return node if dict0.itemsonly?

    it = Nitrene::Interpreter.new(
      Nitrene.primitive,
      Nitrene.either(visualize_edge_eval(ctx, addr), Nitrene.composite),
    )

    dict1 = dict0.transaction do |commit|
      dict0.each_entry(in: Term::Dict.pairspart) do |key, value|
        pass do
          next unless keysym = key.as_sym?
          next unless keysym.prefixed_by?('?')

          ctx.questions << Question.new(addr, key)

          # ?w: _ or ?w: 100 becomes ?w: (...node addr...) so we can trace the query
          # back to the node when we're going to answer it.
          commit.with(key, addr)
        end

        # This is rather hot so we can't afford M1 here. The pattern is roughly:
        #   key: (%any° @expr_ (^ expr_))

        next unless value_dict = value.as_itemsonly_d?
        next unless value_dict.itemsize == 2

        case value_dict[0]
        when SYM_EDGE # @expr_
          expr = value
        when SYM_CARET # (^ expr_)
          _, expr = value_dict
        else
          next
        end

        begin
          vars = Term[]
          result = Nitrene.eval(it, vars, expr)

          # For example:
          #   (cell @name "Bob")
          #   (cell @hover true)
          #   (p "Hello" ⏏hover: ^@hover⏏)            -> (p "Hello" ⏏hover: true⏏)
          #   (p "Hello" ⏏hover: ^(= @name "Alice")⏏) -> (p "Hello" ⏏hover: false⏏)
          commit.with(key, result)
        rescue VisualizeEdgeUndefined
          # Omit if any edge is undefined. For example:
          #   (module {}
          #     (p "Hello" ⏏hover: ^@hover⏏)) -> (p "Hello")
          commit.without(key)
        end
      end
    end

    Term.of(dict1)
  end

  private def visualize(ctx : VisualizeContext, ensemble : Ensemble, addr : D7::NodeAddr, tree : D7::InertLeaf) : Term::Rep
    node = tree.feature.node
    unless visual?(ctx.microfold, node)
      return Term.rep
    end

    Term.rep(visualize_postprocess(ctx, addr, node))
  end

  private def visualize(ctx : VisualizeContext, ensemble : Ensemble, addr : D7::NodeAddr, tree : D7::GndLeaf) : Term::Rep
    node = tree.feature.node

    Term.case(node) do
      matchpi %{[visual template_*]} do
        begin
          instance = Alloy.render(template, composite: visualize_edge_eval(ctx, addr))
        rescue VisualizeEdgeUndefined
          # Omit if not defined.
          #   (module {}
          #     (visual (p ^@x)))
          return Term.rep
        end

        Term.rep(instance.items)
      end

      matchpi %{[p @expr_]}, %{[p (^ expr_)]} do
        it = Nitrene::Interpreter.new(
          Nitrene.primitive,
          Nitrene.either(visualize_edge_eval(ctx, addr), Nitrene.composite),
        )

        begin
          vars = Term[]
          value = Nitrene.eval(it, vars, expr)
        rescue VisualizeEdgeUndefined
          # Omit if not defined.
          #   (module {}
          #     (p @x))
          return Term.rep
        end

        unless value.type.string?
          # Make sure the value is a string.
          #
          #   (cell @x 100)
          #   (p ^(+ @x 1))
          #
          # We expect this to be effectively (p "101").
          value = Term.of(ML.compact(value))
        end

        Term.rep(visualize_postprocess(ctx, addr, Term.morph(node, {1, value})))
      end

      matchpi %{[site]} do
        unless arg = ensemble.args.shift?
          # If the enclosing ensemble provides no more arguments (or there's
          # no enclosing ensemble...), simply omit the site. I'd expect this
          # to be a very rare occurrence, though.
          return Term.rep
        end

        Term.rep(arg)
      end

      matchpi %{[mousearea _*]} do
        Term.rep(Term.of(:hitarea, addr))
      end

      otherwise do
        unless visual?(ctx.microfold, node)
          return Term.rep
        end

        Term.rep(visualize_postprocess(ctx, addr, node))
      end
    end
  end

  private def visualize(ctx : VisualizeContext, ensemble : Ensemble, addr : D7::NodeAddr, tree : D7::MixtureNode | D7::ScopeNode) : Term::Rep
    visualize(ctx, ensemble, addr, tree.child)
  end

  private def visualize(ctx : VisualizeContext, ensemble : Ensemble, addr : D7::NodeAddr, tree : D7::CircuitNode) : Term::Rep
    visualize(ctx, ensemble, addr, tree.to_group)
  end

  private def visualize(ctx : VisualizeContext, ensemble : Ensemble, addr : D7::NodeAddr, tree : D7::GroupNode) : Term::Rep
    # Impassable GroupNodes must not make it into the visual tree. For instance:
    #
    #   (cell @x 0)
    #   (guard (@x 0) (p "x is 0"))
    #   (guard (@x 1) (p "x is 1"))
    #
    # Only the first `p` should be visible.
    if tree.is_a?(D7::GroupNode)
      predicate = tree.feature.passable
      unless predicate.call(ctx.hg, addr)
        return Term.rep
      end
    end

    node = tree.feature.node

    Term.case(node) do
      matchpi %{[ensemble _ _*]} do
        body = tree.children.first
        args = tree.children.rest
        arg_range = tree.feature.range.begin + 1...tree.feature.range.end

        argq = Deque(Term).new
        args.zip(arg_range) do |child, arg_key|
          child_rep = visualize(ctx, ensemble, addr.append(arg_key), child)
          next if child_rep.empty?

          unless arg = child_rep.single?
            arg = Term::Dict.build do |commit|
              commit << :box
              commit.concat(child_rep)
            end
            arg = Term.of(arg)
          end

          argq << arg
        end

        visualize(ctx, Ensemble.new(argq), addr.append(1), body)
      end

      # Importantly we want to avoid entering nested windows.
      matchpi %{[window _*]} do
        Term.rep
      end

      # matchpi %{[textoscope _ _*]} do
      #   rep = Term.flatten(tree.children) do |child, index|
      #     key = tree.feature.range.begin + index
      #     visualize(ctx, ensemble, addr.append(key), child)
      #   end

      #   result = Term::Dict.build do |commit|
      #     commit << :vantage # textoscope
      #     commit.with(:id, addr)
      #     commit.concat(rep)
      #   end

      #   Term.rep(visualize_postprocess(ctx, addr, Term.of(result)))
      # end

      matchpi %{[head_ _*]} do
        if visual?(ctx.microfold, node)
          # (group
          #   (cell @x 100)
          #   (cell @y 200)
          #   (p @x)
          #   (p @y))
          #
          # should become:
          #
          #   (group
          #     (p "100")
          #     (p "200"))
          #
          # Notice how `group` was preserved.
          Term.rep(visualize(ctx, ensemble, addr, tree.feature, tree.children))
        else
          # (device
          #   (p "Item 1")
          #   (p "Item 2"))
          #
          # becomes multiple nodes:
          #
          #   (p "Item 1")
          #   (p "Item 2")
          #
          # Notice how `device` was dissolved.
          Term.flatten(tree.children) do |child, index|
            key = tree.feature.range.begin + index
            visualize(ctx, ensemble, addr.append(key), child)
          end
        end
      end

      otherwise do
        Term.rep
      end
    end
  end

  private def visualize(ctx : VisualizeContext, ensemble : Ensemble, addr : D7::NodeAddr, parent : D7::Parent, children : Enumerable(D7::ParseTree)) : Term
    assert parent.node.itemsize > 0

    rep = Term.flatten(children) do |child, index|
      key = parent.range.begin + index
      visualize(ctx, ensemble, addr.append(key), child)
    end

    result = parent.node.pairspart.transaction do |commit|
      head = parent.node[0] # E.g. (⏏group⏏ (p "A") (p "B") (p "C"))
      commit << head
      commit.concat(rep)
    end

    visualize_postprocess(ctx, addr, Term.of(result))
  end

  # Returns the visual projection of *window*, embedded in the complete hypergraph *hg*.
  # The visual projection is expressed using `Microfold`.
  #
  # TODO: Split into two phases: the first one does as much cheap, bottom-up, cacheable, useful work
  # as possible, leaving traces, building indices, etc.; and the second phase does Hypergraph/Nitrene eval/Alloy
  # stuff which is hardly cacheable, evaluates @edges, ^(@edge expressions), `visual`s, etc. If none
  # are present in a subtree it must be an immediate skip/cache hit and nothing more. Right now
  # we're doing an unavoidable full pass *on every frame* which is very expensive for large UIs,
  # most components in which aren't even visible! Maybe we can have three passes, one is a cacheable
  # "compile" pass, the second gets hypergraph edges and so on to prepare an "environment" for
  # the third pass, and lastly the third pass does cacheable Nitrene.eval, Alloy, etc. on the "environment".
  def visualize(microfold : Microfold::SyncCodex, hg : D7::Hypergraph, window : WindowNode) : {Term, Array(Question)}
    questions = [] of Question
    ctx = VisualizeContext.new(microfold, hg, questions)
    ensemble = Ensemble.new(args: Deque(Term).new)
    visuals = visualize(ctx, ensemble, window.addr, window.tree.feature, window.tree.children)
    {visuals, questions}
  end

  defrecord Question, addr : D7::NodeAddr, key : Term
  defrecord Answer, addr : D7::NodeAddr, key : Term, value : Term

  private def reply?(node : Scenery::ZStack, box : Scenery::Box, vbox : Scenery::VBox, question : Term) : Term?
    # |@ mxrack.[visual question]
    #
    # |@summary
    # Appearance questions supported by visual nodes (of the type, e.g., "What is your width?").
    #
    # |@block
    # Visual nodes can respond to a variety of so-called *visual questions*. You
    # ask these questions in the pairspart of the visual node of interest, and
    # the node keeps answering them, live, as the application evolves.
    #
    # The keys of all visual questions start with `?`. The corresponding values
    # can be anything at all; they will be overwritten by the answer on
    # the next frame.
    #
    # By convention, we seed questions with `_` (the underscore symbol); although
    # if you for some reason do not like that, you can use any other symbol or term.
    # On the next tick, if possible, the seed gets replaced by the node's answer,
    # which is going to be updated by the node from that point onwards.
    #
    # |@example
    # ```wwml
    # ;; Frame 0 (seed)
    #
    # (window width: 500 height: 400
    #   (p "Hello World" ?w: _ ?h: _))
    #
    # ;; Frame 1
    #
    # (window width: 500 height: 400
    #   (p "Hello World" ?w: ≈500 ?h: ≈400))
    # ```
    #
    # In this example, we are asking the `p` about its width (`?w: _`) and height
    # (`?h: _`). In the second frame the `p` answers with `500x400`, which is
    # the same as the size of the window (which the `p` "floods").
    case question
    when Term.of(:"?w")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?w: _}
      #
      # |@block
      # Replaces the value of `?w` with the *logical* width of the visual node.
      # *Logical* sizes are not affected by overflow and transforms.
      Term.of(box.bounds.w)
    when Term.of(:"?h")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?h: _}
      #
      # |@block
      # Replaces the value of `?h` with the *logical* height of the visual node.
      # *Logical* sizes are not affected by overflow and transforms.
      Term.of(box.bounds.h)
    when Term.of(:"?l")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?l: _}
      #
      # |@block
      # Replaces the value of `?l` with the absolute (window-space) left position
      # of the visual node.
      Term.of(box.bounds.x)
    when Term.of(:"?r")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?r: _}
      #
      # |@block
      # Replaces the value of `?r` with the absolute (window-space) right position
      # of the visual node.
      Term.of(box.bounds.x + box.bounds.w)
    when Term.of(:"?t")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?t: _}
      #
      # |@block
      # Replaces the value of `?t` with the absolute (window-space) top position
      # of the visual node.
      Term.of(box.bounds.y)
    when Term.of(:"?b")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?b: _}
      #
      # |@block
      # Replaces the value of `?b` with the absolute (window-space) bottom position
      # of the visual node.
      Term.of(box.bounds.y + box.bounds.h)
    when Term.of(:"?visual-w")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?visual-w: _}
      #
      # |@block
      # Replaces the value of `?visual-w` with the *visual* width of the node. Visual
      # sizes are affected by overflow and transforms.
      Term.of(vbox.bounds.w)
    when Term.of(:"?visual-h")
      # |@ mxrack.[visual question]
      #
      # |@pattern
      # {¦ ?visual-h: _}
      #
      # |@block
      # Replaces the value of `?visual-h` with the *visual* height of the node. Visual
      # sizes are affected by overflow and transforms.
      Term.of(vbox.bounds.h)
    when Term.of(:"?aim-x")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))
      return unless rect = clip.aim_rect

      Term.of(rect.x)
    when Term.of(:"?aim-y")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))
      return unless rect = clip.aim_rect

      Term.of(rect.y)
    when Term.of(:"?aim-offset-x")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))

      Term.of(clip.aim.x)
    when Term.of(:"?aim-offset-y")
      return unless clip = Scenery.first?(node, box, Scenery::Clip(Scenery::AimedNode))

      Term.of(clip.aim.y)
    when Term.of(:"?navs")
      Term.of(Textoscope.navs(node, box))
    when Term.of(:"?nav")
      navs = Textoscope.navs(node, box)
      navs.single?
    end
  end

  # Leaves
  private def answers(
    node : Scenery::Inert | Scenery::RectShape | Scenery::HitArea | Scenery::Pending | Scenery::Img | Scenery::Svg | Scenery::IconGlyph | Scenery::ShapedText,
    box : Scenery::Box,
    vbox : Scenery::VBox,
  ) : Slice(Answer)
    Slice(Answer).empty
  end

  private def answers(node : Scenery::ZStack, box : Scenery::Box, vbox : Scenery::VBox) : Slice(Answer)
    unless info = node.info
      return answers(node.children, box, vbox)
    end

    if info.pairs.empty? # Fast path
      return answers(node.children, box, vbox)
    end

    answers = Pf::Kit.stack_array(Answer)

    info.pairs.each_entry do |key, value|
      next unless keysym = key.as_sym?
      next unless keysym.prefixed_by?('?')

      # Remember that we store the address in values of ?k: v_ pairs. Basically for
      # any visual node that made it all the way here, that still has a ?-key, we
      # know the value is an address; an analysis of `visualize` would show that.
      # (Or... umm... I hope it would.)
      next unless value = value.as_itemsonly_d?
      next unless addr = D7::NodeAddr.new?(value)

      answer = reply?(node, box, vbox, key) || Term.of(:_)
      answers << Answer.new(addr, key, answer)
    end

    # Inline answers(node.children, box) to reuse the existing `answers` stack array.
    node.children.zip(box.children, vbox.children) do |child, child_box, child_vbox|
      child_box = child_box.translate(box.bounds.tl)
      child_vbox = Scenery::VBox.new(child_vbox.bounds.translate(vbox.bounds.tl), child_vbox.children)
      answers.concat(answers(child, child_box, child_vbox))
    end

    answers.to_unsafe_readonly_slice!
  end

  # Parents
  private def answers(
    node : Scenery::Vantage | Scenery::Padding | Scenery::Align | Scenery::XYStack | Scenery::Composite | Scenery::TransformMatrix | Scenery::Clip | Scenery::Gate,
    box : Scenery::Box,
    vbox : Scenery::VBox,
  ) : Slice(Answer)
    answers(node.children, box, vbox)
  end

  private def answers(nodes : Slice(Scenery::AimedNode), box : Scenery::Box, vbox : Scenery::VBox) : Slice(Answer)
    answers = Pf::Kit.stack_array(Answer)

    nodes.zip(box.children, vbox.children) do |node, node_box, node_vbox|
      node_box = node_box.translate(box.bounds.tl)
      node_vbox = Scenery::VBox.new(node_vbox.bounds.translate(vbox.bounds.tl), node_vbox.children)
      answers.concat(answers(node, node_box, node_vbox))
    end

    answers.to_unsafe_readonly_slice!
  end

  alias AnswerMap = Hash({D7::NodeAddr, Term}, Term)

  def answer_map(scene : Scenery::Scene) : AnswerMap
    # Convert OriginBox -> Box.
    box = scene.box.translate(Scenery::Point[0, 0])
    answers = answers(scene.root.node, box, scene.vbox)
    answers.to_h { |answer| { {answer.addr, answer.key}, answer.value } }
  end
end
