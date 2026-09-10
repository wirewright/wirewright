# Implements miscellaneous nodes such as `discard`, `delay`, `transfer`.
module Ww::Rack::Misc
  extend self

  def step(& : Propose -> T) : T forall T
    propose = Propose.new do |hg, proposals|
      propose(hg, proposals)
    end
    yield propose
  end

  private def propose(hg : D7::Hypergraph, proposals)
    hg.propose(proposals, :discard, :latest, :feed, :transfer, :delay, :view, :journal) do |candidate|
      step(hg, candidate)
    end
  end

  private def step(hg : D7::Hypergraph, node : D7::Node) : D7::Patch?
    Term.case(node.term, block_type: {:proc, hg : D7::Hypergraph, node : D7::Node}) do
      # Discard can target zero or more nodes.
      matchpi %{[discard @edge_]} do
        targets = Pf::Kit.stack_array(D7::Node, 4)

        Rack.each_cell(hg, hg.resolve(node.addr, edge)) do |target|
          next if target.empty?

          targets << target.node
        end

        targets.present? ? D7.patches(targets, {2, nil}) : nil
      end

      # Discard with a pattern can target zero or more nodes.
      matchpi %{[discard @edge_ pattern_]} do
        targets = Pf::Kit.stack_array(D7::Node, 4)

        Rack.each_cell(hg, hg.resolve(node.addr, edge)) do |target|
          next unless value = target.value?
          next unless M1.probe?(pattern, value)

          targets << target.node
        end

        targets.present? ? D7.patches(targets, {2, nil}) : nil
      end

      matchpi %{[latest @edge_]}, %{[latest @edge_ _]} do
        return unless cell = Rack.cell?(hg, hg.resolve(node.addr, edge))

        D7.patch(node, {2, cell.value?})
      end

      matchpi %{[feed _*]} do
        return unless spec = Feed.spec?(node.term)

        Feed.step(hg, node, spec)
      end

      matchpi %{[delay 1 successor_]} do
        D7.replace(node, successor)
      end

      matchpiT %{[delay n←(%number +i32!) _]} do
        D7.patch(node, {1, n - 1})
      end

      # This one is used in some mixture()s wher ewe require the circuit to evolve
      # despite the possibility of no other changes.
      matchpiT %{[delay n←(%number +i32!)]} do
        D7.patch(node, {1, n - 1})
      end

      matchpi %{[transfer (@src_ pattern_ @dst_) template_]} do
        transfer(hg, node, src, pattern, dst, template)
      end

      matchpi %{[transfer (srcs←((%past @_ min: 1)) pattern_ @dst_) template_]} do
        transfer(hg, node, srcs.items, pattern, dst, template)
      end

      matchpi %{[transfer (not←(not (%past @_ min: 0)) srcs←((%past @_ min: 1)) pattern_ @dst_) template_]} do
        inhibitorsQ = not.items.move(1)
        active = inhibitorsQ.all? do |inhibitorQ|
          inhibitor_cell = Rack.cell?(hg, hg.resolve(node.addr, inhibitorQ))
          inhibitor_cell.nil? || inhibitor_cell.empty?
        end

        next unless active

        transfer(hg, node, srcs.items, pattern, dst, template)
      end

      # TODO: cache
      matchpi %{[view (@src_ pattern_ @dst_) template_]} do
        return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, dst))

        src_cell = Rack.cell?(hg, hg.resolve(node.addr, src))

        # If the source cell is missing or empty, clear the destination cell.
        if src_cell.nil? || (matchee = src_cell.value?).nil?
          return D7.patch(dst_cell.node, {2, nil})
        end

        # Clear the destination cell on pattern mismatch.
        unless env = M1.match?(pattern, matchee)
          return D7.patch(dst_cell.node, {2, nil})
        end

        expansion = Alloy.render_rep(template, locals: env)
        unless expansion.empty?
          output = Term.collapse(expansion)
        end

        D7.patch(dst_cell.node, {2, output})
      end

      # TODO: cache
      matchpi %{[view (srcs←((%past @_ min: 1)) pattern_ @dst_) template_]} do
        return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, dst))

        row = Term::Dict.build do |commit|
          srcs.items.each do |src|
            src_cell = Rack.cell?(hg, hg.resolve(node.addr, src))

            # If one of the source cells is missing or empty, clear the destination cell.
            if src_cell.nil? || (matchee = src_cell.value?).nil?
              return D7.patch(dst_cell.node, {2, nil})
            end

            commit << matchee
          end
        end

        # Clear the destination cell on pattern mismatch.
        unless env = M1.match?(pattern, Term.of(row))
          return D7.patch(dst_cell.node, {2, nil})
        end

        expansion = Alloy.render_rep(template, locals: env)
        unless expansion.empty?
          output = Term.collapse(expansion)
        end

        D7.patch(dst_cell.node, {2, output})
      end

      matchpi %{[journal (@edge_) _*]} do
        return unless target = Rack.cell?(hg, hg.resolve(node.addr, edge))
        return unless goal = target.value?

        event = Term.of(:appeared, goal)
        D7.patch(node, {1, 1, goal}, {node.term.itemsize, event})
      end

      matchpi %{[journal (@edge_ state_) _*]} do
        return unless target = Rack.cell?(hg, hg.resolve(node.addr, edge))

        # State present, goal absent, we need to remove state.
        unless goal = target.value?
          event = Term.of(:disappeared, state)
          return D7.patch(node, {1, 1, nil}, {node.term.itemsize, event})
        end

        return if state == goal

        # State present, goal changed.
        event = Term.of(:changed, goal)
        D7.patch(node, {1, 1, nil}, {node.term.itemsize, event})
      end

      otherwise { }
    end
  end

  # TODO: cache
  private def transfer(hg : D7::Hypergraph, node : D7::Node, src : Term, pattern : Term, dst : Term, template : Term) : D7::Patch?
    return unless src_cell = Rack.cell?(hg, hg.resolve(node.addr, src))
    return unless matchee = src_cell.value?
    return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, dst))
    return unless dst_cell.empty?
    return unless env = M1.match?(pattern, matchee)

    expansion = Alloy.render_rep(template, locals: env)
    unless expansion.empty?
      output = Term.collapse(expansion)
    end

    D7.patches(
      D7.patch(src_cell.node, {2, nil}),
      D7.patch(dst_cell.node, {2, output}),
    )
  end

  # TODO: cache
  private def transfer(hg : D7::Hypergraph, node : D7::Node, srcs : Enumerable(Term), pattern : Term, dst : Term, template : Term) : D7::Patch?
    return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, dst))
    return unless dst_cell.empty?

    src_cell_nodes = Pf::Kit.stack_array(D7::Node, 8)

    row = Term::Dict.build do |commit|
      srcs.each do |src|
        return unless src_cell = Rack.cell?(hg, hg.resolve(node.addr, src))
        return unless src_value = src_cell.value?

        commit << src_value
        src_cell_nodes << src_cell.node
      end
    end

    return unless env = M1.match?(pattern, Term.of(row))

    expansion = Alloy.render_rep(template, locals: env)
    unless expansion.empty?
      output = Term.collapse(expansion)
    end

    D7.patches(
      D7.patches(src_cell_nodes) { |src_cell_node| D7.patch(src_cell_node, {2, nil}) },
      D7.patch(dst_cell.node, {2, output}),
    )
  end
end

module Ww::Rack
  module Prepass
    extend self

    def call(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
      Part.prepass(hg, &fn)
    end
  end

  def manipulate(parser : D7::Parser, circuit : Term, prepass) : Slice(Term)
    D7.step(parser, circuit, required_heads: {Term.of(:manipulable)}) do |hg|
      prepass.call(hg) do |hg|
        D7.merge(hg, proposals: manipulate(hg))
      end
    end
  end

  def manipulate(hg : D7::Hypergraph) : Indexable(D7::Patch)
    hg.propose(:manipulable) do |node|
      Term.case(node.term) do
        matchpi %{[manipulable header←(input←(%'edge capture_) -> _*) payload_]} do
          selector = Term.of(:"%let", capture, :_)
          patterns = header.items.move(1)
          manipulate(hg, node, hg.resolve(node.addr, input), selector, patterns, payload)
        end

        matchpi %{[manipulable header←(@input_ selector_ -> _*) payload_]} do
          patterns = header.items.move(2)
          manipulate(hg, node, hg.resolve(node.addr, input), selector, patterns, payload)
        end

        otherwise { }
      end
    end
  end

  def manipulate(hg : D7::Hypergraph, node : D7::Node, input : D7::AbsEdge, selector : Term, patterns : Indexable(Term), payload : Term) : D7::Patch?
    return unless source = Rack.cell?(hg, input)

    # If value is present and matches, synthesize a backspec that plugs stuff in.
    backspec = pass do
      next unless value = source.value?
      next unless env = M1.match?(selector, value)

      Term::Dict.build do |commit|
        env.each_entry do |key, value|
          commit.with(key, {:"^verbatim", value})
        end
      end
    end

    return unless backspec

    result = patterns.leftmost? do |pattern|
      M1.backmap?(pattern, Term.of(backspec), payload)
    end

    return unless result

    D7.patch(node, {2, result})
  end
end
