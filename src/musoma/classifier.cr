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
        matchpi %{[window _*]} do
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
          D7.gnd(node)
        end

        # Setting duration to zero makes the sequencer step on every cycle.
        matchpi %{[sequencer _ _+]} do
          D7.gnd(node)
        end

        # Setting duration to zero makes the ticker step on every cycle.
        matchpi %{[ticker _ _number]} do
          D7.gnd(node)
        end

        matchpi %{[trunk]} do
          D7.gnd(node)
        end

        # adjunct is a special node which defines a point-of-view for adjacency
        # queries in distill, in particular, trunk queries. I.e., when you do `(trunk)`,
        # the question is, trunk with respect to what? The answer is, with respect to
        # to the enclosing `adjunct`.
        matchpi %{[adjunct _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
        end

        matchpi %{[figure _*]} do
          D7.parent(node.as_d, 1u32...node.uitemsize)
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

  struct ReflectionPrepass(Prepass)
    def initialize(@vantages : VarHash(D7::NodeAddr, Term), @successor : Prepass)
    end

    def call(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
      replacements = {} of D7::NodeAddr => D7::Gnd

      figure_node_ids = Pf::USet32.transaction do |txn|
        hg.each_node_with_head(Term.of(:reflection)) do |node|
          Term.matchpi?(node.term, %{[reflection @edge_]}) do
            observation = @vantages.get?(node.addr)
            replacements[node.addr] = D7.gnd(Term.of(:cell, edge, observation), edge)
            txn << node.id
          end
        end
      end

      if replacements.empty?
        return @successor.call(hg, &fn)
      end

      patch = @successor.call(hg.gnd_map(replacements), &fn)

      # Discard all patches to reflection nodes ,which we've replaced. Such patches make
      # no sense. This covers circuits like:
      #
      #   (reflection @f (p "Hello"))
      #   (discard @f)
      #
      # Which, as I've said, make no sense, because there isn't *really* anything to
      # discard. For Rack, reflection acts as a kind of "infinite source", replenished
      # immediately regardless of what Rack does to it.
      if figure_node_ids.size < patch.size
        patch = patch.transaction do |txn|
          figure_node_ids.each do |node_id|
            txn.dissoc(node_id)
          end
        end
      else
        patch = patch.transaction do |txn|
          patch.each do |node_id, value|
            next unless node_id.in?(figure_node_ids)

            txn.dissoc(node_id)
          end
        end
      end

      patch
    end
  end

  defrecord Trunk, active : Term::Rep?

  # :nodoc:
  def distill(codex, tree : D7::InertLeaf, addr, trunk) : Term::Rep
    node = tree.feature.node

    Term.case(node) do
      matchpi %{{¦ style}} do
        curate(node)
      end

      matchpi %{[head_]} do
        continue unless Scenery::KnowledgeBase.leaf_head?(head)

        curate(node)
      end

      matchpi %{[head_ _*]} do
        continue unless codex.preset?(head)

        curate(node)
      end

      otherwise do
        Term.rep # omit
      end
    end
  end

  # :nodoc:
  def distill(codex, tree : D7::GndLeaf, addr, trunk) : Term::Rep
    node = tree.feature.node

    Term.case(node) do
      matchpi %{[trunk]} do
        Term.rep(trunk.active || Slice(Term).empty) # active or omit
      end

      matchpi %{[slot _]} do
        Term.rep(Term.of({:loading}))
      end

      matchpi %{{¦ style}} do
        curate(node)
      end

      otherwise do
        Term.rep # omit
      end
    end
  end

  private def adjunct?(tree : D7::ParseTree) : Bool
    return false unless tree.is_a?(D7::ParentNode)

    Term.case(tree.feature.node) do
      matchpi %{[adjunct _*]} { true }
      otherwise { false }
    end
  end

  # :nodoc:
  def distill(codex, tree : D7::ParentNode, addr, trunk) : Term::Rep
    pred = nil
    buffer = Pf::Kit.stack_array(Term::Rep, 8)

    tree.children.zip(tree.feature.range) do |child, key|
      child_adjunct = adjunct?(child)

      if child_adjunct
        child_trunk = Trunk.new(pred)
      else
        child_trunk = trunk
      end

      rep = distill(codex, child, addr.append(key), child_trunk)
      if !child_adjunct && pred
        buffer << pred
      end

      pred = rep
    end

    # Flush the last one.
    if pred
      buffer << pred
    end

    children = Term.flatten(buffer, &.itself)

    Term.case(tree.feature.node) do
      matchpi %{[group [reflection @_] _*]} do
        distilled = Term::Dict.build do |commit|
          commit << :vantage
          commit.with(:id, {:reflection, addr.append(1)})
          commit.concat(children)
        end

        Term.rep_of(distilled)
      end

      matchpi %{[window _*]}, %{{¦ style}} do
        curate(tree, children)
      end

      matchpi %{[head_ _*]} do
        continue unless Scenery::KnowledgeBase.parent_head?(head)

        curate(tree, children)
      end

      otherwise do
        children
      end
    end
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

  # :nodoc:
  def distill(codex, tree : D7::MixtureNode | D7::ScopeNode, addr, trunk) : Term::Rep
    distill(codex, tree.child, addr, trunk)
  end

  # Finds Microfold and Scenery nodes in *tree* and returns a list of roots
  # for trees built this way.
  def distill(codex : Microfold::SyncCodex, tree : D7::ParseTree) : Term
    addr = D7::NodeAddr.empty
    trunk = Trunk.new(active: nil)
    Term.of(distill(codex, tree, addr, trunk))
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
