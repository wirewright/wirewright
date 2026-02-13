module Ww::D7
  # An integer used to identify a node in a hypergraph. Node ids are
  # usually hypergraph-bounded rendition of circuit-bounded `NodeAddr`.
  alias NodeId = UInt32

  defrecord Node, id : NodeId, term : Term

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
  # each group consisting of other nodes in the community.
  class Hypergraph
    alias EdgeId = UInt32

    # :nodoc:
    def initialize
      @node_terms = [] of Term
      @head_index = {} of Term => Pf::USet32
      @node_addrs = [] of NodeAddr
      @node_scopes = [] of NodeScope
      # TODO: I think we can actually try storing this in the good old
      # array of arrays or something along those lines (aka edge array;
      # for graphs it's a set of pairs and for hypergraphs it's a set
      # of sets; the representation for either is a matter of artistry,
      # so to speak). Probabilistic indexing can help us search without
      # the overhead of hashing and so on, and ALSO without the overhead
      # of linear scans. We'd also have high density/linear access pattern.
      # Representing hypergraphs extremely efficiently is actually a very
      # interesting problem, you see, and that's exactly NOT what we're
      # doing here right now.
      @node_edges = {} of NodeId => Array(AbsEdge)
      @edge_nodes = {} of AbsEdge => Array(NodeId)
    end

    def add(addr : NodeAddr, scope : NodeScope, node : Term, edges : Enumerable(T), & : T -> AbsEdge) : Nil forall T
      node_id = @node_terms.size.to_u32
      node_head = node[0]
      @head_index[node_head] = (@head_index[node_head]? || Pf::USet32[]).add(node_id)
      @node_terms << node
      @node_addrs << addr
      @node_scopes << scope

      edges.each_with_index do |object, edge_index|
        edge = yield object

        # FIXME: WTF... we're running out of names aren't we?
        memberships = @node_edges.put_if_absent(node_id) { [] of AbsEdge }
        memberships << edge

        member_ids = @edge_nodes.put_if_absent(edge) { [] of NodeId }
        member_ids << node_id
      end
    end

    def replace(node_id : NodeId, term1 : Term, & : AbsEdge -> Bool) : Nil
      term0 = @node_terms[node_id]
      @node_terms[node_id] = term1

      head0 = term0[0]
      head1 = term1[0]
      unless head0 == head1
        @head_index[head0] = @head_index[head0].delete(node_id)
        @head_index[head1] = (@head_index[head1]? || Pf::USet32[])
      end

      return unless edges = @node_edges[node_id]?

      edges.select! do |edge|
        if yield edge
          next true # Accepted
        end

        # Rejected
        nodes = @edge_nodes[edge]
        nodes.delete(node_id)
        false
      end
    end

    def empty? : Bool
      @node_terms.empty?
    end

    # Returns the node with the given *id*.
    def [](id : NodeId) : Term
      @node_terms[id]
    end

    # Returns the `fold`-address of the node with the given *id*.
    def addr(id : NodeId) : NodeAddr
      @node_addrs[id]
    end

    # Resolves the given *edge* term with respect to a node *wrt*.
    def abs_edge(edge : Term, *, wrt : NodeId) : AbsEdge
      scope = @node_scopes[wrt]
      AbsEdge.new(*scope[edge])
    end

    # Yields nodes of this hypergraph.
    def each_node(& : Node ->) : Nil
      @node_terms.each_with_index do |node, id|
        yield Node.new(NodeId.new(id), node)
      end
    end

    def has_head?(head : Term) : Bool
      @head_index.has_key?(head)
    end

    def each_node_with_head(& : Node ->) : Nil
      @head_index.each do |head, bucket|
        bucket.each do |node_id|
          yield Node.new(node_id, @node_terms[node_id]), head
        end
      end
    end

    def each_node_with_head(head : Term, & : Node ->) : Nil
      return unless bucket = @head_index[head]?

      bucket.each do |node_id|
        yield Node.new(node_id, @node_terms[node_id]), head
      end
    end

    # Yields absolute edges associated with a node with the given *id*.
    def each_edge(id : NodeId, & : AbsEdge ->) : Nil
      return unless memberships = @node_edges[id]?

      memberships.each { |edge| yield edge }
    end

    # Yields nodes that are members of the given *edge*.
    def each_member(edge : AbsEdge, & : Node ->) : Nil
      return unless member_ids = @edge_nodes[edge]?

      member_ids.each do |member_id|
        yield Node.new(member_id, @node_terms[member_id])
      end
    end

    def each_neighbor(*, of id : NodeId, on edge : AbsEdge, & : Node ->) : Nil
      each_edge(id) do |candidate_edge|
        next unless edge == candidate_edge

        each_member(candidate_edge) do |neighbor|
          next if neighbor.id == id # Skip self
          yield neighbor
        end
      end
    end
  end
end
