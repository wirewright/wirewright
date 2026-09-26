module MuSoma
  # MuSoma classifier.
  #
  # This is the central place where MuSoma-specific nodes are defined and documented
  # (e.g. `window`, `figure`).
  #
  # The MuSoma classifier eventually calls the Rack classifier, `Rack.clf`.
  def clf : D7::Classifier
    successor = baseclf

    ->(node : Term) do
      feature = successor.call(node)

      if incomplete?(clf, feature)
        return D7.inert(node, annotations: {:incomplete})
      end

      feature
    end
  end

  # TODO: docs
  private def baseclf : D7::Classifier
    successor = ->Rack.classify!(Term)

    ->(node : Term) do
      Term.case(node) do
        matchpi %{[window _*]}, %{[box _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        matchpi %{[section _string _*]} do
          D7.parent(node.as_d, 2u32...node.uitemsize)
        end

        matchpi %{[reflection {¦ @desc_ node: @view_} _?]} do
          body = node[2]?

          defn = Term.of(:group,
            {:reflection, desc},
            {:frag, view, body})

          D7.mixture(node, defn) do |(_, _, node_out)|
            Term.case(node_out) do
              matchpi %{(frag _ body-out_)} { Term.morph(node, {2, body_out}) }
              matchpi %{(frag _)} { Term.morph(node, {2, nil}) }
            end
          end
        end

        matchpi %{[reflection @edge_ _+]} do
          defn = Term.morph(node, {0, :group}, {1, Term.of(:reflection, edge)})

          D7.mixture(node, defn) do |mix|
            Term.morph(mix, {0, :reflection}, {1, edge})
          end
        end

        matchpi %{[reflection @edge_]} do
          D7.gnd(node, edge)
        end

        matchpi %{[mouse _*]}, %{[input _*]}, %{[keyboard _*]} do
          # The default `exclusive` merge policy is too restrictive! We must be able to
          # have different patches add/remove keys/buttons, and set pairs, so use
          # the `surface` policy.
          D7.gnd(node, merge_policy: D7::MergeDiff.new(1u32))
        end

        # |@ musoma.node.site
        #
        # |@pattern
        # [site]
        matchpi %{[site]} do
          D7.gnd(node)
        end

        # |@ musoma.node.ensemble
        #
        # |@pattern
        # [ensemble body_ members_*]
        #
        # |@key body musoma.node
        #
        # |@key members musoma.node
        # NOTE: Sites occurring within an ensemble's *members* belong to the nearest
        # enclosing ensemble, not the ensemble being constructed.
        matchpi %{[ensemble _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        matchpi %{[figure _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        matchpi %{[p @edge_]} do
          D7.gnd(node, edge)
        end

        matchpi %{(comment _string)} do
          D7.gnd(node)
        end

        matchpi %{[head_ _*]} do
          continue unless Scenery::KnowledgeBase.parent_head?(head)

          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        otherwise do
          successor.call(node)
        end
      end
    end
  end

  struct ReflectionPrepass
    def initialize(@vantages : VarHash(D7::GlobalNodeAddr, Term))
    end

    def call(hg : D7::Hypergraph, proposals : Array(D7::Patch), &fn : D7::Hypergraph, Array(D7::Patch) ->) : Nil
      Rack::Prepass.call(hg, proposals) do |hg, proposals| # shadow
        process(hg, proposals, &fn)
      end
    end

    private def process(hg : D7::Hypergraph, proposals : Array(D7::Patch), &fn : D7::Hypergraph, Array(D7::Patch) ->) : Nil
      replacements = {} of D7::NodeAddr => D7::Gnd

      figure_node_ids = Pf::USet32.transaction do |txn|
        hg.each_node_with_head(Term.of(:reflection)) do |node|
          Term.matchpi?(node.term, %{[reflection @edge_]}) do
            global_addr = hg.to_global(node.addr)
            observation = @vantages.get?(global_addr)
            replacements[node.addr] = D7.gnd(Term.of(:cell, edge, observation), edge)
            txn << node.id
          end
        end
      end

      if replacements.empty?
        fn.call(hg, proposals)
        return
      end

      fn.call(hg.gnd_map(replacements), proposals)

      # Discard all patches to reflection nodes, which we've replaced. Such patches make
      # no sense. This covers circuits like:
      #
      #   (reflection @f (p "Hello"))
      #   (discard @f)
      #
      # Which, as I've said, make no sense, because there isn't *really* anything to
      # discard. For Rack, reflection acts as a kind of "infinite source", replenished
      # immediately regardless of what Rack does to it.
      proposals.map! &.dissoc(figure_node_ids)
    end
  end

  # A post-processing step on distilled markup to replacespairs that have
  # an edge value with that value (or to remove them if there is no value):
  #
  # ```wwml
  # (cell @visible true)
  # (p "Kaixo!" style: "absent visible:present" @:visible)
  # ```
  #
  # ... distills to:
  #
  # ```wwml
  # (p "Kaixo!" style: "absent visible:present" visible: false)
  # ```
  private def resolve_pairs(hg : D7::Hypergraph, addr : D7::NodeAddr, rep : Term::Rep) : Term::Rep
    return rep unless node0 = rep.single?
    return rep unless node0 = node0.as_d?

    node1 = node0.transaction do |commit|
      node0.each_entry(in: Term::Dict.pairspart) do |key, value|
        next unless Term.edge?(value)
        next unless cell = Rack.cell?(hg, hg.resolve(addr, value))

        # Compare:
        #
        #   (cell @visible)
        #   (p "Kaixo!" style: "absent visible:present" @:visible)
        #
        # ... with:
        #
        #   (cell @visible false)
        #   (p "Kaixo!" style: "absent visible:present" @:visible)
        #
        # Generally, absence should map to absence. It is up to Microfold how to
        # interpret `false` vs. absence (in this case they are interpreted the same).
        commit.with(key, cell.value?)
      end
    end

    Term.rep_of(node1)
  end

  # :nodoc:
  def distill(µ, hg, addr, tree : D7::InertLeaf, sites, site_zero) : {Term::Rep, UInt32}
    node = tree.feature.node

    Term.case(node) do
      matchpi %{{¦ style}} do
        {resolve_pairs(hg, addr, curate(node)), site_zero}
      end

      matchpi %{[head_]} do
        continue unless Scenery::KnowledgeBase.leaf_head?(head)

        {curate(node), site_zero}
      end

      matchpi %{[head_ _*]} do
        continue unless µ.preset?(head)

        {curate(node), site_zero}
      end

      otherwise do
        {Term.rep, site_zero} # omit
      end
    end
  end

  # :nodoc:
  def distill(µ, hg, addr, tree : D7::GndLeaf, sites, site_zero) : {Term::Rep, UInt32}
    node = tree.feature.node

    Term.case(node) do
      matchpi %{[site]} do
        {Term.rep(sites[site_zero]? || Slice(Term).empty), site_zero} # present or omit
      end

      matchpi %{[slot _]} do
        {Term.rep(Term.of({:loading})), site_zero}
      end

      matchpi %{[p @edge_]} do
        unless value = Rack.cell?(hg, hg.resolve(addr, edge)).try(&.value?)
          next Term.rep, site_zero # omit
        end

        unless value.type.string?
          value = Term.of(ML.compact(value))
        end

        {Term.rep(Term.morph(node, {1, value})), site_zero}
      end

      matchpi %{{¦ style}} do
        {resolve_pairs(hg, addr, curate(node)), site_zero}
      end

      otherwise do
        {Term.rep, site_zero} # omit
      end
    end
  end

  # :nodoc:
  def distill(µ, hg, addr, tree : D7::ParentNode, sites, site_zero) : {Term::Rep, UInt32}
    # Impassable GroupNodes must not make it into the distilled markup. For
    # example, in:
    #
    #   (cell @x 0)
    #   (guard (@x 0) (p "x is 0"))
    #   (guard (@x 1) (p "x is 1"))
    #
    # Only the first `p` should be visible.
    if tree.is_a?(D7::GroupNode)
      predicate = tree.feature.passable
      unless predicate.call(hg, addr)
        return Term.rep, site_zero
      end
    end

    node = tree.feature.node

    Term.case(node) do
      matchpi %{[group [reflection @_] _*]} do
        children, site_zero = distill(µ, hg, addr, tree.children, tree.feature.range, sites, site_zero)

        vantage_id = Term.of(:reflection, addr.append(1))

        if child = children.single?
          distilled = Term.morph(child, {:"µ-vantage", vantage_id})
        else
          distilled = Term::Dict.build do |commit|
            commit << :box
            commit.with(:"µ-vantage", vantage_id)
            commit.concat(children)
          end
        end

        {Term.rep_of(distilled), site_zero}
      end

      matchpi %{[ensemble _ _*]} do
        body = tree.children.first
        sites = tree.children.rest.to_readonly_slice do |child, index|
          key = tree.feature.range.begin + index + 1 # skip body
          child, site_zero = distill(µ, hg, addr.append(key), child, sites, site_zero)
          child
        end

        distill(µ, hg, addr.append(1), body, sites, site_zero)
      end

      matchpi %{[window _*]}, %{{¦ style}} do
        children, site_zero = distill(µ, hg, addr, tree.children, tree.feature.range, sites, site_zero)
        {resolve_pairs(hg, addr, curate(tree, children)), site_zero}
      end

      matchpi %{[head_ _*]} do
        continue unless Scenery::KnowledgeBase.parent_head?(head)

        children, site_zero = distill(µ, hg, addr, tree.children, tree.feature.range, sites, site_zero)
        {curate(tree, children), site_zero}
      end

      otherwise do
        distill(µ, hg, addr, tree.children, tree.feature.range, sites, site_zero)
      end
    end
  end

  def distill(µ, hg, addr, nodes : Slice(D7::ParseTree), range : Range(UInt32, UInt32), sites, site_zero) : {Term::Rep, UInt32}
    result = Term.flatten(nodes) do |child, index|
      key = range.begin + index
      rep, site_zero = distill(µ, hg, addr.append(key), child, sites, site_zero)
      rep
    end

    {result, site_zero}
  end

  # :nodoc:
  def distill(µ, hg, addr, tree : D7::ScopeNode | D7::MixtureNode, sites, site_zero) : {Term::Rep, UInt32}
    distill(µ, hg, addr, tree.child, sites, site_zero)
  end

  def distill(codex : Microfold::SyncCodex, tree : D7::ParseTree) : Term
    markup = Term.rep # ?!

    hg = D7::Hypergraph.new(tree)
    Rack::Prepass.call(hg) do |hg|
      markup, _ = distill(codex, hg, D7::NodeAddr.empty, tree, sites: Slice(Term).empty, site_zero: 0u32)
    end

    Term.of(markup)
  end

  private def curate(tree : D7::ParentNode, children : Enumerable(Term)) : Term::Rep
    node = tree.feature.node
    range = tree.feature.range

    curated = node.pairspart.transaction do |commit|
      # Curate impassable items in front.
      (0...range.begin).each do |key|
        commit.concat(curate(node[key]))
      end

      # Replace passable items.
      commit.concat(children)

      # Curate impassable items at the back.
      (range.end...node.itemsize).each do |key|
        commit.concat(curate(node[key]))
      end

      # Curate pairs.
      node.each_entry(in: Term::Dict.pairspart) do |key, value|
        rep = curate(value)
        if rep.empty?
          commit.without(key)
          next
        end

        commit.with(key, Term.collapse(rep))
      end
    end

    Term.rep_of(curated)
  end

  defrecord WindowInfo, id : Term, defn : Term, open : Bool

  # Returns a list of `WindowInfo` objects describing windows (and their content)
  # found in *tree*.
  def window_infos(codex : Microfold::SyncCodex, tree : D7::ParseTree) : Slice(WindowInfo)
    roots = distill(codex, tree)
    unless roots = roots.as_d?
      return Slice(WindowInfo).empty
    end

    seen = Set(Term).new
    seqid = 0

    roots.items.to_compact_readonly_slice do |root|
      Term.matchpi?(root, %{[window _*]}) do
        # Do not trust the user: make sure the key is not claimed before using
        # it for a window, fall back to seqid if claimed.
        id = root[:key]?
        if id.nil? || !seen.add?(id)
          id = Term.of(seqid)
          seqid += 1
        end

        # Assume it's open by default, only if open: false is it closed.
        open = root[:open]? != Term.of(false)

        WindowInfo.new(id, root, open)
      end
    end
  end
end
