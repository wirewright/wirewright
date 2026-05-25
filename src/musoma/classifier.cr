module MuSoma
  class CircuitParser
    def initialize
      @cache = GenerationalCache(Term, D7::ParseTree).new
    end

    def step(circuit : Term, prepass) : Term
      @cache.epoch do
        Rack.step(MuSoma.clf, circuit, prepass, cache: @cache).last
      end
    end

    def parse(circuit : Term) : D7::ParseTree
      @cache.epoch do
        D7.parse(MuSoma.clf, circuit, reply: D7::ParseTree, cache: @cache)
      end
    end
  end

  # MuSoma classifier.
  #
  # This is the central place where MuSoma-specific nodes are defined and documented
  # (e.g. `window`, `path-report`).
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

        matchpi %{[figure {¦ @observable_ node: @view_} body_]} do
          defn = Term.of(:group,
            {:figure, observable},
            {:node, view, body})

          D7.mixture(node, defn) do |(_, _, node_out)|
            Term.case(node_out) do
              matchpi %{(node _ body-out_)} { Term.morph(node, {2, body_out}) }
              matchpi %{(node _)} { Term.morph(node, {2, nil}) }
            end
          end
        end

        matchpi %{[figure @edge_ _+]} do
          defn = Term.morph(node, {0, :group}, {1, Term.of(:figure, edge)})

          D7.mixture(node, defn) do |mix|
            Term.morph(mix, {0, :figure}, {1, edge})
          end
        end

        matchpi %{[figure @edge_]} do
          D7.gnd(node, edge)
        end

        matchpi %{[mouse _*]}, %{[input _*]}, %{[keyboard _*]} do
          D7.gnd(node)
        end

        matchpi %{[path-report _string _?]} do
          D7.gnd(node)
        end

        matchpi %{[path-reading _string _?]} do
          D7.gnd(node)
        end

        matchpi %{[resource _ _?]} do
          D7.gnd(node)
        end

        matchpi(
          %{[file-sink _string _blob]},
          %{[file-sink _string _string]},
        ) do
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

  struct FigurePrepass(Prepass)
    def initialize(@vantages : VarHash(D7::NodeAddr, Term), @successor : Prepass)
    end

    def call(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
      figure_node_ids = Pf::USet32.transaction do |txn|
        hg.each_node_with_head(Term.of(:figure)) do |node|
          observation = @vantages.get?(node.addr)

          Term.matchpi?(node.term, %{[figure @edge_]}) do
            hg.replace!(node.id, Term.of(:cell, edge, observation))
            txn << node.id
          end
        end
      end

      patch = @successor.call(hg, &fn)

      if figure_node_ids.empty?
        return patch
      end

      # Discard all patches to figure nodes ,which we've replaced. Such patches make
      # no sense. This covers circuits like:
      #
      #   (figure @f (p "Hello"))
      #   (discard @f)
      #
      # Which, as I've said, make no sense, because there isn't *really* anything to
      # discard. For Rack, figure acts as a kind of "infinite source", replenished
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

  private def repr(tree : D7::InertLeaf, addr : D7::NodeAddr) : Term
    repr = Term::Dict.build do |commit|
      commit << :inert << tree.feature.node

      tree.feature.annotations.each do |ann|
        case ann
        when .incomplete? then commit.with(:incomplete, true)
        end
      end
    end

    Term.of(repr)
  end

  private def repr(tree : D7::GndLeaf, addr) : Term
    node = tree.feature.node

    Term.case(node) do
      matchpi %{(device (@_ surface_) _* ⍊ -open)} do
        Term.of(:"closed-device-widget", addr, surface)
      end

      matchpi %{(backsys header_dict _* ⍊ -open)} do
        Term.of(:"closed-backsys-widget", addr, header)
      end

      matchpi %{(backsys _* ⍊ open)} do
        Term.of(:"open-backsys-widget", addr, node)
      end

      matchpi %{(slot call_ _ ⍊ -open)} do
        Term.of(:"closed-slot-widget", addr, call)
      end

      matchpi %{[path-reading path_string _]} do
        Term.of(:"path-reading-widget", addr, node)
      end

      matchpi %{[path-report path_string _]} do
        Term.of(:"path-report-widget", addr, node)
      end

      matchpi %{(section title_string _* ⍊ -open)} do
        Term.of(:"closed-section-widget", addr, title)
      end

      otherwise do
        Term.of(:gnd, node)
      end
    end
  end

  private def repr(tree : D7::UnaugmentedParentNode, addr) : Term
    parent = tree.feature

    repr = parent.node.pairspart.transaction do |commit|
      commit << :parent

      parent.node.items.each_with_index do |item, key|
        key = key.to_u32

        unless key.in?(parent.range)
          commit << Term.of(:inert, item)
          next
        end

        index = key - parent.range.begin
        child = tree.children[index]

        commit << repr(child, addr.append(key))
      end
    end

    Term.case(parent.node) do
      matchpi %{[slot _ _]} do
        Term.of(:"open-slot-widget", addr, repr)
      end

      matchpi %{[device _ _*]} do
        Term.of(:"open-device-widget", addr, repr)
      end

      matchpi %{[section title_string _*]} do
        Term.of(:"open-section-widget", addr, title, repr)
      end

      otherwise do
        Term.of(repr)
      end
    end
  end

  # Returns the representation tree for *tree*. This tree is ready for
  # pretty-printing.
  def repr(tree : D7::UnaugmentedParseTree) : Term
    repr = repr(tree, D7::NodeAddr.empty)

    # Mark the topmost parent as root for styling in prettyR.
    Term.matchpi(repr, %{[parent _*]}) do
      Term.morph(repr, {0, :root})
    end
  end

  private def distill(codex, tree : D7::InertLeaf, addr) : Term::Rep
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

  private def distill(codex, tree : D7::GndLeaf, addr) : Term::Rep
    node = tree.feature.node

    Term.case(node) do
      matchpi %{{¦ style}} do
        curate(node)
      end

      otherwise do
        Term.rep # omit
      end
    end
  end

  private def distill(codex, tree : D7::ParentNode, addr) : Term::Rep
    children = Term.flatten(tree.children) do |child, index|
      key = tree.feature.range.begin + index
      distill(codex, child, addr.append(key))
    end

    Term.case(tree.feature.node) do
      matchpi %{[group [figure @_] _*]} do
        distilled = Term::Dict.build do |commit|
          commit << :vantage
          commit.with(:id, {:figure, addr.append(1)})
          commit.concat(children)
        end

        Term.rep_of(distilled)
      end

      matchpi %{[window _*]}, %{{¦ style}} do
        distill(tree, children)
      end

      matchpi %{[head_ _*]} do
        continue unless Scenery::KnowledgeBase.parent_head?(head)

        distill(tree, children)
      end

      otherwise do
        children
      end
    end
  end

  private def distill(tree : D7::ParentNode, children : Term::Rep) : Term::Rep
    node = tree.feature.node
    range = tree.feature.range

    distilled = node.pairspart.transaction do |commit|
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

    Term.rep_of(distilled)
  end

  private def distill(codex, tree : D7::MixtureNode | D7::ScopeNode, addr) : Term::Rep
    distill(codex, tree.child, addr)
  end

  # Finds Microfold and Scenery nodes in *tree* and returns a list of roots
  # for trees built this way.
  def distill(codex : Microfold2::SyncCodex, tree : D7::ParseTree) : Term
    Term.of(distill(codex, tree, D7::NodeAddr.empty))
  end

  defrecord WindowInfo, id : Term, defn : Term, open : Bool

  # Returns a list of `WindowInfo` objects describing windows (and their content)
  # found in *circuit*.
  def window_infos(codex : Microfold2::SyncCodex, tree : D7::ParseTree) : Slice(WindowInfo)
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
