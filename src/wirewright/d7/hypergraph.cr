module Ww::D7
  # An integer used to identify a node in a hypergraph. Node ids are usually
  # a (hypergraph lifetime)-bounded rendition of the (circuit frame lifetime)-
  # bounded `NodeAddr`.
  alias NodeId = UInt32

  struct Node
    getter id : NodeId
    getter addr : NodeAddr
    getter head : Term
    getter term : Term

    # :nodoc:
    def initialize(@id, @addr, @head, @term)
    end
  end

  # Represents an absolute edge.
  #
  # We differentiate between *absolute* and *relative* edges. When you see
  # `edge : Term` and the like, that's a *relative* edge. Such edges must
  # be resolved against the node they're taken from before doing most serious
  # things (such as comparing two edges to check if e.g. two nodes are neighbors).
  #
  # The result of *resolution* of a relative edge *with respect to* a node is called
  # an *absolute edge*.
  #
  # The main thing calculated during resolution is *module*, the address of
  # the module that owns the edge; and its true *term* (as in `@x`, `@foo`)
  # *under that module* (and NOT e.g. a nested module).
  defrecord AbsEdge, module : NodeAddr, term : Term

  # A hypergraph is a graph whose edges can include any number of nodes; each edge
  # is a subset of the set of nodes in the graph. It's easier to think of a hypergraph
  # as a society of nodes, where each node can participate in zero or more groups,
  # and each group can consist of one or more nodes.
  #
  # The hypergraph representation used in D7 is *ephemeral*: the hypergraph is never
  # materialized. Instead, `Hypergraph` and related offer a way to query a `ParseTree`
  # *as if* it was a hypergraph. `ParseTree`, in turn, contains some recursive
  # measurements, statistics, and indices to help `Hypergraph` find things.
  #
  # NOTE: A hypergraph can contain annotations. I'm not a big fan of them, because they
  # make Hypergraph stateful/mutable; but anything else doesn't seem nice either.
  class Hypergraph
    # Includers can be used to annotate a `Hypergraph`.
    module Annotation
    end

    # Returns the target level of this hypergraph.
    getter level : UInt32

    # :nodoc:
    def initialize(@tree : ParseTree, @level : UInt32)
      @lock = Sync::Mutex.new
      @annotations = Set(Annotation).new
    end

    # :nodoc:
    alias Guide = GroupNode -> Bool

    # :nodoc:
    defrecord WalkContext,
      hg : Hypergraph,
      guide : Guide,
      ids : Range(NodeId, NodeId),
      sink : NodeId, NodeAddr, Gnd -> WalkFlow

    # :nodoc:
    enum WalkFlow
      Continue
      Break
    end

    # :nodoc:
    def self.walk(hg : Hypergraph, guide : Guide, fn : Node, Set(Term) -> WalkFlow, ids : Range(NodeId, NodeId) = NodeId::MIN...NodeId::MAX) : Nil
      assert ids.exclusive?
      assert ids.begin <= ids.end

      sink = ->(id : NodeId, addr : NodeAddr, feature : Gnd) do
        assert id.in?(ids)

        # Use Gnd#defn (the node's definition) rather than #node here.
        # The hypergraph should only ever see the defn.
        node = Node.new(id, addr, feature.head, feature.defn)
        fn.call(node, feature.edges)
      end

      ctx = WalkContext.new(hg, guide, ids, sink)
      walk(ctx, NodeAddr.empty, hg.@tree, hg.@level, 0u32)
    end

    private def self.walk(ctx, addr, tree : InertLeaf, level, id_zero) : WalkFlow
      WalkFlow::Continue
    end

    private def self.walk(ctx, addr, tree : GndLeaf, level, id_zero) : WalkFlow
      unless level.zero?
        return WalkFlow::Continue
      end

      ctx.sink.call(id_zero, addr, tree.feature)
    end

    private def self.walk(ctx, addr, tree : ScopeNode | MixtureNode, level, id_zero) : WalkFlow
      walk(ctx, addr, tree.child, level, id_zero)
    end

    private def self.walk(ctx, addr, tree : CircuitNode, level, id_zero) : WalkFlow
      if level.zero?
        return walk(ctx, addr, tree.leaf, level, id_zero)
      end

      # This branch cannot possibly contain circuits at the target level.
      if D7.maxlevel(tree) < level
        return WalkFlow::Continue
      end

      treatment = GroupNode.new(D7.parent(tree.feature.node, tree.feature.range), tree.children)
      walk(ctx, addr, treatment, level - 1, id_zero)
    end

    private def self.walk(ctx, addr, tree : GroupNode, level, id_zero) : WalkFlow
      # This branch cannot possibly contain circuits at the target level.
      if D7.maxlevel(tree) < level
        return WalkFlow::Continue
      end

      # The guide must only apply to the level we're searching for, because
      # all important metrics are level-local and will likely block our descent
      # down to *level* incorrectly.
      if level.zero? && !ctx.guide.call(tree)
        return WalkFlow::Continue
      end

      predicate = tree.feature.passable
      unless predicate.call(ctx.hg, addr)
        return WalkFlow::Continue
      end

      assert ctx.ids.exclusive?

      tree.children.each_with_index(offset: tree.feature.range.begin) do |child, key|
        id_width = D7.population(child)
        break if ctx.ids.end <= id_zero

        if ctx.ids.begin < id_zero + id_width
          case walk(ctx, addr.append(key), child, level, id_zero)
          in .continue?
          in .break?
            return WalkFlow::Break
          end
        end

        id_zero += id_width
      end

      WalkFlow::Continue
    end

    # :nodoc:
    defrecord ResolveContext, hg : Hypergraph, guide : Guide, fn : Node ->

    # :nodoc:
    def self.resolve(hg : Hypergraph, root : ParseTree, addr : NodeAddr, guide : Guide, membership : Slice(Term), id_zero, fn : Node ->) : Nil
      ctx = ResolveContext.new(hg, guide, fn)
      resolve_root(ctx, addr, root, membership, id_zero)
    end

    private def self.resolve_root(ctx, addr, tree : InertLeaf, membership, id_zero) : Nil
    end

    private def self.resolve_root(ctx, addr, tree : GndLeaf, membership, id_zero) : Nil
      resolve_inner(ctx, addr, tree, membership, id_zero)
    end

    private def self.resolve_root(ctx, addr, tree : ScopeNode | MixtureNode, membership, id_zero) : Nil
      resolve_root(ctx, addr, tree.child, membership, id_zero)
    end

    private def self.resolve_root(ctx, addr, tree : CircuitNode, membership, id_zero) : Nil
      stat = tree.summary.child_level? || LevelSummary.new
      return unless membership.all?(&.in?(stat.edges))

      tree.children.each_with_index(offset: tree.feature.range.begin) do |child, key|
        resolve_inner(ctx, addr.append(key), child, membership, id_zero)
        id_zero += D7.population(child)
      end
    end

    private def self.resolve_root(ctx, addr, tree : GroupNode, membership, id_zero) : Nil
      resolve_inner(ctx, addr, tree, membership, id_zero)
    end

    private def self.resolve_inner(ctx, addr, tree : InertLeaf, membership, id_zero) : Nil
    end

    private def self.resolve_inner(ctx, addr, tree : GndLeaf, membership, id_zero) : Nil
      return if membership.empty?
      return unless membership.all?(&.in?(tree.feature.edges))

      # Use Gnd#defn (the node's definition) rather than #node here.
      # The hypergraph should only ever see the defn.
      node = Node.new(id_zero, addr, tree.feature.head, tree.feature.defn)
      ctx.fn.call(node)
    end

    private def self.resolve_inner(ctx, addr, tree : ScopeNode, membership, id_zero) : Nil
      return if membership.empty?

      scope_step = tree.feature.scope

      case scope_step
      in ScopeOpenExcept
        # If `locals` blocks any one edge from *membership*, prune. Only if
        # `locals` lets all edges from *membership* pass through, do we descend.
        return unless membership.all? { |edge| !edge.in?(scope_step.edges) }

        resolve_inner(ctx, addr, tree.child, membership, id_zero)
      in ScopeClosedExcept
        translated = Pf::Kit.stack_array(Term, 8)

        membership.each do |member|
          scope_step.bindings.each_entry do |interior, exterior|
            next unless exterior == member
            translated << interior
          end
        end

        translated_slice = translated.to_unsafe_readonly_buffer_or_spill_slice!
        resolve_inner(ctx, addr, tree.child, translated_slice, id_zero)
      end
    end

    private def self.resolve_inner(ctx, addr, tree : MixtureNode, membership, id_zero) : Nil
      return if membership.empty?

      resolve_inner(ctx, addr, tree.child, membership, id_zero)
    end

    private def self.resolve_inner(ctx, addr, tree : CircuitNode, membership, id_zero) : Nil
      return if membership.empty?

      resolve_inner(ctx, addr, tree.leaf, membership, id_zero)
    end

    private def self.resolve_inner(ctx, addr, tree : GroupNode, membership, id_zero) : Nil
      return if membership.empty?

      stat = tree.summary.current_level
      return unless membership.all?(&.in?(stat.edges))
      return unless ctx.guide.call(tree)

      predicate = tree.feature.passable
      return unless predicate.call(ctx.hg, addr)

      tree.children.each_with_index(offset: tree.feature.range.begin) do |child, key|
        resolve_inner(ctx, addr.append(key), child, membership, id_zero)
        id_zero += D7.population(child)
      end
    end

    # Returns `true` if *head* is present in the hypergraph at *any* level.
    def has_head_anywhere?(head : Term) : Bool
      D7.summary(@tree).head?(head)
    end

    # Returns `true` if there are no nodes at the current level and below
    # the current level.
    def bottom? : Bool
      maxlevel = D7.maxlevel(@tree)

      if @level < maxlevel
        return false
      end

      if @level > maxlevel
        return true
      end

      # When exactly at maxlevel, we have to actually iterate to see if
      # there's anything.

      guide = Guide.new { true }

      empty = true
      sink = ->(node : Node, edges : Set(Term)) do
        empty = false
        WalkFlow::Break
      end

      Hypergraph.walk(self, guide, sink)

      empty
    end

    # Traverses nodes in the hypergraph at the target level.
    def each_node(&fn : Node ->) : Nil
      guide = Guide.new { true }

      sink = ->(node : Node, edges : Set(Term)) do
        fn.call(node)
        WalkFlow::Continue
      end

      Hypergraph.walk(self, guide, sink)
    end

    def gnd_map(replacements : Hash(NodeAddr, Gnd)) : Hypergraph
      Hypergraph.new(D7.gnd_map(@tree, replacements), @level)
    end

    # Traverses only nodes with the given *head* in the hypergraph at the target
    # level. Node ids are compatible with `each_node`.
    #
    # This is generally faster than a linear scan with `each_node` because
    # the underlying structure indexes heads, and this method makes sure to
    # skip as much work as possible if the head is definitely absent in
    # a subtree.
    def each_node_with_head(head : Term, &fn : Node ->) : Nil
      guide = Guide.new { |group| D7.head?(group, head) }

      # Guide can give false positives! We need to catch them here.
      sink = ->(node : Node, edges : Set(Term)) do
        if node.head == head
          fn.call(node)
        end

        WalkFlow::Continue
      end

      Hypergraph.walk(self, guide, sink)
    end

    def each_node_with_head(head : Term, *, memberof : Tuple(AbsEdge), &fn : Node ->) : Nil
      # TODO: The general algorithm (for Indexable(AbsEdge)) would probably look like this:
      #
      # roots = membership.map do |edge|
      #   descend?(edge.module).not_nil!
      # end
      #
      # guide = Guide.new { ... head? ... }
      #
      # nodes = stack_array
      #
      # roots.each do |root|
      #   resolve(root, guide, membership) do |node|
      #     nodes << node
      #   end
      # end

      edge = memberof[0]

      # The first step is to descend down to the module which the caller claims to
      # be the origin of the edge.
      return unless row = D7.follow?(@tree, edge.module)

      addr = edge.module
      id_zero, origin = row

      guide = Guide.new { |group| D7.head?(group, head) }

      # Guide can give false positives! We need to catch them here.
      sink = ->(node : Node) do
        return unless node.head == head

        fn.call(node)
      end

      needle = edge.term
      Hypergraph.resolve(self, origin, addr, guide, pointerof(needle).to_slice(1), id_zero, sink)
    end

    private def single(node_id : NodeId) : {Node, Set(Term)}
      guide = Guide.new { true }

      buffer = Pf::Kit.stack_array({Node, Set(Term)}, 1)
      sink = ->(node : Node, edges : Set(Term)) do
        buffer << {node, edges}
        WalkFlow::Break
      end

      Hypergraph.walk(self, guide, sink, ids: node_id...node_id + 1)

      buffer.single
    end

    def each_edge(node_id : NodeId, & : AbsEdge ->) : Nil
      node, edges = single(node_id)
      edges.each { |edge| yield resolve(node.addr, edge) }
    end

    def each_member(edge : AbsEdge, *, heads : Indexable(Term) = Slice(Term).empty, &fn : Node ->) : Nil
      # The first step is to descend down to the module which the caller claims to
      # be the origin of the edge.
      return unless row = D7.follow?(@tree, edge.module)

      # Then we conduct a search in the subtree reached this way.
      addr = edge.module
      id_zero, origin = row

      guide = Guide.new do |group|
        heads.all? { |head| D7.head?(group, head) }
      end

      needle = edge.term
      Hypergraph.resolve(self, origin, addr, guide, pointerof(needle).to_slice(1), id_zero, fn)
    end

    def each_neighbor(of node_id : NodeId, on edge : AbsEdge, **kwargs, &fn : Node ->) : Nil
      seen = Pf::Kit.stack_array(NodeId, 8)
      seen << node_id
      seen_set : Set(NodeId)? = nil # allocate on demand

      each_edge(node_id) do |candidate_edge|
        next unless edge == candidate_edge

        each_member(candidate_edge, **kwargs) do |neighbor|
          next if neighbor.id.in?(seen)

          if set = seen_set
            next unless set.add?(neighbor.id)
          elsif seen.size == 8
            seen_set = Set{neighbor.id}
          else
            seen << neighbor.id
          end

          fn.call(neighbor)
        end
      end
    end

    def each_neighbor(node_id : NodeId, &fn : Node ->) : Nil
      each_edge(node_id) do |candidate_edge|
        each_member(candidate_edge) do |neighbor|
          next if neighbor.id == node_id # Skip self

          fn.call(neighbor)
        end
      end
    end

    def [](node_id : NodeId) : Node
      node, _ = single(node_id)
      node
    end

    def propose(*heads : Symbol, &fn : Node -> Patch?) : Indexable(D7::Patch)
      # FIXME: stack_array miscompiles for some reason... We *really* need
      # to rewrite Pf::Map, its representation is too hard for Crystal
      # to compile...
      proposals = [] of D7::Patch
      propose(proposals, *heads, &fn)
      proposals
    end

    def propose(proposals, *heads : Symbol, &fn : Node -> Patch?) : Nil
      heads.each do |head|
        each_node_with_head(Term.of(head)) do |node|
          proposal = fn.call(node)
          next if proposal.nil?

          proposals << proposal
        end
      end
    end

    def annotated_with?(ann : Annotation) : Bool
      @lock.synchronize { ann.in?(@annotations) }
    end

    def annotate(ann : Annotation, &)
      @lock.synchronize { @annotations << ann }

      begin
        yield
      ensure
        @lock.synchronize { @annotations.delete(ann) }
      end
    end

    # Resolves *edge* with respect to the node at *addr*.
    #
    # This is necessary in cases where you read an edge from a node (e.g.
    # using pattern matching). You can't use the edge as-is because the actual
    # cell (or node) it refers to can be different due to modules in-between.
    # You must first pass the edge through `resolve` so that it finds the correct
    # edge with respect to the node that you've read it from.
    #
    # See also: `AbsEdge`.
    def resolve(addr : NodeAddr, edge : Term) : AbsEdge
      node = @tree
      scopes = Pf::Kit.stack_array({Int32, NodeScope}, 8)

      addr.each_with_index do |key, key_index|
        loop do
          case node
          in InertLeaf, GndLeaf
            raise KeyError.new
          in MixtureNode
            node = node.child
            next
          in ScopeNode
            scopes << {key_index, node.feature.scope}
            node = node.child
            next
          in ParentNode
            # NOTE: Circuits must surround themselves with scopes to seal themselves off
            # from the outside world completely. Otherwise, two circuits with the same
            # level would be able to communicate, and that would go against our semantics.
            #
            #   ;; Must NOT work!
            #   (circuit @0 (cell @x 100))
            #   (circuit @1 (cell @y))
            #   (circuit @2 (feed @x @y))
            #
            if node.is_a?(CircuitNode)
              scopes << {key_index, ScopeClosedExcept.new(Term[])}
            end

            index = key - node.feature.range.begin
            unless 0 <= index < node.children.size
              raise KeyError.new
            end

            node = node.children.unsafe_fetch(index)
          end

          break
        end
      end

      # NOTE: Below, we use `trim(key_index)` instead of `trim(key_index + 1)`
      # because ScopeNodes themselves do not have an address -- they are "virtual"
      # nodes. So if we use `key_index + 1` for the "module" of the edge, that'd
      # mean the ScopeNode's *child* is the module -- which is incorrect! Instead,
      # we use simply `key_index`, which refers to the parent of the scope node.

      scopes.reverse_each do |key_index, scope|
        case scope
        in ScopeOpenExcept
          if edge.in?(scope.edges)
            # In Rack:
            #
            #  (local (⏏@dst⏏) ;; @dst found, so it's a local!
            #    (feed @src ⏏@dst⏏)
            return AbsEdge.new(addr.trim(key_index), edge)
          end
          # Continue climbing. This edge falls into "open", thus outer-scoped,
          # not "except" and thus inner-scoped.
          #
          # In Rack:
          #  (local (@dst) ;; <<- @src NOT found, so it's outerly-scoped.
          #    (feed ⏏@src⏏ @dst)
        in ScopeClosedExcept
          unless exterior = scope.bindings[edge]?
            # In Rack:
            #  (module {@x: @y} ;; <<- no @a, so it's a local!
            #    (feed ⏏@a⏏ @x)
            return AbsEdge.new(addr.trim(key_index), edge)
          end

          # In Rack:
          #  (module {⏏@x⏏: @y} ;; @x found, its *exterior* is the outerly-scoped @y.
          #    (feed @a ⏏@x⏏)
          edge = exterior
        end
      end

      AbsEdge.new(NodeAddr.empty, edge)
    end
  end
end
