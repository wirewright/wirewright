module Ww::Rack::ControlSpace
  extend self

  # :nodoc:
  SYM_SURFACE = Term.of(:surface)

  # :nodoc:
  SYM_CONTROL = Term.of(:control)

  def probably_exists_in?(hg : D7::Hypergraph) : Bool
    hg.has_head?(SYM_SURFACE) && hg.has_head?(SYM_CONTROL)
  end

  defrecord Surface, term : Term, node : D7::Node
  defrecord Control, dst : Term, pattern : Term, node : D7::Node

  def spaces(hg : D7::Hypergraph)
    spaces = {} of D7::NodeScope => {Array(Surface), Array(Control)}

    # Discover surfaces and controls.
    hg.each_node_with_head(Term.of(:surface)) do |node|
      Term.matchpi?(node.term, %{[surface surface_]}) do
        scope = hg.scope(node.id)
        surfaces, _ = spaces.put_if_absent(scope) { {[] of Surface, [] of Control} }
        surfaces << Surface.new(surface, node)
      end
    end

    hg.each_node_with_head(Term.of(:control)) do |node|
      Term.matchpi?(node.term, %{[control @dst_ pattern_]}) do
        scope = hg.scope(node.id)
        _, controls = spaces.put_if_absent(scope) { {[] of Surface, [] of Control} }
        controls << Control.new(dst, pattern, node)
      end
    end

    spaces
  end

  def prepass(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
    unless probably_exists_in?(hg)
      return fn.call(hg)
    end

    spaces = spaces(hg)
    if spaces.empty?
      return fn.call(hg)
    end

    secret = sync_rand(UInt32)

    used = Pf::USet32.new
    parts = {} of D7::NodeId => {Term, Term}

    # Pair surfaces with controls.
    spaces.each do |_, (surfaces, controls)|
      controls.each do |control|
        partner = nil

        surfaces.each do |surface|
          next unless M1.probably_matches?(control.pattern, surface.term)
          next unless M1.probe?(control.pattern, surface.term)

          if partner.nil?
            partner = surface
            next
          end

          # The control is confused because there are many possible partners in
          # the space.
          partner = nil
          break
        end

        # Confused (many partners) or no partner exists.
        next unless partner

        used = used.add(partner.node.id)

        src = Term.of(:edge, {secret, partner.node.id})
        assert parts.put?(control.node.id, {src, Term.of(:part, {src, control.dst}, control.pattern)})
      end
    end

    cells = {} of D7::NodeId => {Surface, Term, Term}

    spaces.each do |_, (surfaces, _)|
      surfaces.each do |surface|
        next unless surface.node.id.in?(used)

        edge = Term.of(:edge, {secret, surface.node.id})
        assert cells.put?(surface.node.id, {surface, edge, Term.of(:cell, edge, surface.term)})
      end
    end

    # Replace surface nodes with `(cell @...)`.
    #
    # Initially, surface nodes have no edges. So we simply keep-all
    # with `true`. We need to add cell's @... edge though.
    cells.each do |node_id, (_, edge, cell)|
      hg.replace!(node_id, cell)
      hg.join!(node_id, hg.abs_edge(edge, wrt: node_id))
    end

    # Replace control nodes with (part ...).
    #
    # The control node is already connected to the dst edge. We only need
    # to connect it to the src edge (its partner cell)
    parts.each do |node_id, (src, part)|
      hg.replace!(node_id, part)
      hg.join!(node_id, hg.abs_edge(src, wrt: node_id))
    end

    patch = fn.call(hg)
    if patch.empty?
      return patch
    end

    patch.each do |node_id, rep|
      # Assert no managed part's in patch.
      assert !parts.has_key?(node_id)
      next unless node_id.in?(used)

      # Replace managed cell's in patch with updated (surface ...).
      surface, _, _ = cells[node_id]
      Term.case(rep) do
        matchpi %{(cell @_ value_)} do
          patch = patch.assoc(node_id, Term.of(surface.node.term.morph({1, value})))
        end

        matchpi %{(cell @_)} do
          patch = patch.assoc(node_id, Term.of(surface.node.term.morph({1, nil})))
        end
      end
    end

    patch
  end
end
