module Ww::D7
  # An integer used to identify a node in a hypergraph. Node ids are usually
  # a (hypergraph lifetime)-bounded rendition of the (circuit frame lifetime)-
  # bounded `NodeAddr`.
  alias NodeId = UInt32

  struct Node
    getter id : NodeId
    getter addr : NodeAddr
    getter scope : NodeScope
    getter head : Term
    getter term : Term

    # :nodoc:
    def initialize(@id, @addr, @scope, @head, @term)
    end

    # Resolves *edge* with respect to this node.
    #
    # This is necessary in cases where you read an edge from a node (e.g.
    # using pattern matching) inside a regime. You can't use the edge as-is
    # because the actual cell (or node) it refers to can be different due
    # to modules in-between. You must first pass the edge through `resolve`
    # so that it finds the correct edge with respect to the node that you've
    # read it from.
    #
    # See also: `AbsEdge`.
    def resolve(edge : Term) : AbsEdge
      AbsEdge.new(*@scope[edge])
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
  # each group consisting of other nodes in the community.
  class Hypergraph
    alias EdgeId = UInt32

    # :nodoc:
    def initialize
      @head_index = {} of Term => Pf::USet32
      @nodes = [] of Node

      # TODO: I think we can actually try storing this in the good old
      # array of arrays or something along these lines (aka edge array;
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

    # Mutates this hypergraph to add a node with the given *addr*, *scope*,
    # and *term*. Returns the resulting `NodeId`.
    def add!(addr : NodeAddr, scope : NodeScope, term : Term) : NodeId
      unless head = term[0]?
        # Abort
        raise ArgumentError.new("could not determine node head")
      end

      # Commit
      id = @nodes.size.to_u32
      @head_index[head] = (@head_index[head]? || Pf::USet32[]).add(id)
      @nodes << Node.new(id, addr, scope, head, term)

      id
    end

    # Mutates this hypergraph to replace the term associated with *node id*.
    # The node's scope and edges are left intact.
    def replace!(node_id : NodeId, term : Term) : Nil
      unless head1 = term[0]?
        # Abort
        raise ArgumentError.new("could not determine node head")
      end

      # Commit
      node0 = @nodes[node_id]
      head0 = node0.head

      unless head0 == head1
        @head_index[head0] = @head_index[head0].delete(node_id)
        @head_index[head1] = (@head_index[head1]? || Pf::USet32[]).add(node_id)
      end

      node1 = Node.new(node0.id, node0.addr, node0.scope, head1, term)
      @nodes[node_id] = node1
    end

    # Mutates this hypergraph to subscribe a node with the given *node id*
    # to *edge*.
    def join!(node_id : NodeId, edge : AbsEdge) : Nil
      edges = @node_edges.put_if_absent(node_id) { [] of AbsEdge }
      edges << edge

      members = @edge_nodes.put_if_absent(edge) { [] of NodeId }
      members << node_id
    end

    # Mutates this hypergraph to unsubscribe a node with the given *node id*
    # from *edge*.
    def leave!(node_id : NodeId, edge : AbsEdge) : Nil
      pass do
        next unless edges = @node_edges[node_id]?

        edges.delete(edge)
        next unless edges.empty?

        @node_edges.delete(node_id)
      end

      pass do
        next unless members = @edge_nodes[edge]?

        members.delete(node_id)
        next unless members.empty?

        @edge_nodes.delete(edge)
      end
    end

    # Returns the node with the given *node id*.
    def [](node_id : NodeId) : Node
      @nodes[node_id]
    end

    # Yields nodes of this hypergraph.
    def each_node(& : Node ->) : Nil
      @nodes.each { |node| yield node }
    end

    # Returns `true` if this hypergraph has a node with the given *head*.
    def has_head?(head : Term) : Bool
      @head_index.has_key?(head)
    end

    # Yields nodes and their heads.
    def each_node_with_head(& : Node, Term ->) : Nil
      @head_index.each do |head, bucket|
        bucket.each do |node_id|
          yield @nodes[node_id], head
        end
      end
    end

    # Yields only nodes with the given *head* (if any).
    def each_node_with_head(head : Term, & : Node ->) : Nil
      return unless bucket = @head_index[head]?

      bucket.each do |node_id|
        yield @nodes[node_id]
      end
    end

    # Yields absolute edges associated with the node with the given *id*.
    def each_edge(node_id : NodeId, & : AbsEdge ->) : Nil
      return unless edges = @node_edges[node_id]?

      edges.each { |edge| yield edge }
    end

    # Yields nodes that are members of the given *edge*.
    def each_member(edge : AbsEdge, & : Node ->) : Nil
      return unless member_ids = @edge_nodes[edge]?

      member_ids.each do |member_id|
        yield @nodes[member_id]
      end
    end

    # Yields neighbors of *node id* on the given *edge*, if any.
    def each_neighbor(*, of node_id : NodeId, on edge : AbsEdge, & : Node ->) : Nil
      each_edge(node_id) do |candidate_edge|
        next unless edge == candidate_edge

        each_member(candidate_edge) do |neighbor|
          next if neighbor.id == node_id # Skip self
          yield neighbor
        end
      end
    end
  end
end
