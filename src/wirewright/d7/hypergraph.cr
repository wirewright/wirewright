module Ww::D7
  # An integer used to identify a node in a hypergraph. Node ids are
  # usually hypergraph-bounded rendition of circuit-bounded `NodeAddr`.
  alias NodeId = UInt32

  # The address of a node in a circuit.
  alias NodeAddr = Slice(Int32)

  # Records the scopes that the traversal process passes through. The addr
  # is that of the scope (e.g. `module`, but in general, see `Scope`), and
  # the dict is its bindings dict.
  alias NodeScope = Slice({NodeAddr, Term::Dict})

  defrecord Node, id : NodeId, term : Term

  # Represents a hyperedge.
  #
  # *scope* is the address of the module that defines the scope of the edge *term*.
  defrecord Hyperedge, scope : NodeAddr, term : Term

  # A hypergraph is a graph whose edges can include any number of nodes; each edge
  # is a subset of the set of nodes in that graph. It's easier to think of a hypergraph
  # as a community of nodes. Each node can participate in zero or more groups, each
  # group formed from other nodes in the community.
  class Hypergraph
    alias EdgeId = UInt32

    # - *nodemap* maps nodes to node ids (implicit, array index).
    # - *edgemap* maps node ids (implicit, array index) to hyperedges that node
    #   is participating in.
    # - *trmap* maps node ids (implicit, array index) to node addresses (from `fold`).
    def initialize(
      @node_terms : Array(Term),
      @node_addrs : Array(NodeAddr),
      @node_edges : Hash({NodeId, Int32}, Hyperedge),
      @edge_nodes : Hash({Hyperedge, Int32}, NodeId),
    )
    end

    struct Builder
      def initialize
        @node_terms = [] of Term
        @node_addrs = [] of NodeAddr
        @node_edges = {} of {NodeId, Int32} => Hyperedge
        @edge_members = {} of {Hyperedge, Int32} => NodeId
      end

      def submit(addr : NodeAddr, node : Term, edges : Enumerable(T), & : T -> Hyperedge) : Nil forall T
        node_id = @node_terms.size.to_u32

        @node_terms << node
        @node_addrs << addr

        edges.each_with_index do |object, edge_index|
          edge = yield object

          cardinality = 0
          while @edge_members.has_key?({edge, cardinality})
            cardinality += 1
          end

          @node_edges[{node_id, edge_index}] = edge
          @edge_members[{edge, cardinality}] = node_id
        end
      end

      def hypergraph
        Hypergraph.new(@node_terms, @node_addrs, @node_edges, @edge_members)
      end
    end

    def self.build(& : Builder ->)
      builder = Builder.new
      yield builder

      builder.hypergraph
    end

    def empty? : Bool
      order.zero?
    end

    # Returns the number of nodes in this hypergraph.
    def order
      @node_terms.size
    end

    # Returns the node with the given *id*.
    def [](id : NodeId) : Term
      @node_terms[id]
    end

    # Returns the `fold`-address of the node with the given *id*.
    def addr(id : NodeId) : NodeAddr
      @node_addrs[id]
    end

    # Yields nodes of this hypergraph.
    def each_node(& : Node ->) : Nil
      @node_terms.each_with_index do |node, id|
        yield Node.new(NodeId.new(id), node)
      end
    end

    # Yields hyperedges of a node with the given node *id* (as yielded
    # by `each_node`).
    def each_edge(id : NodeId, & : Hyperedge ->) : Nil
      index = 0
      while edge = @node_edges[{id, index}]?
        yield edge
        index += 1
      end
    end

    def each_member(edge : Hyperedge, & : Node ->) : Nil
      index = 0
      while member_id = @edge_nodes[{edge, index}]?
        yield Node.new(member_id, @node_terms[member_id])
        index += 1
      end
    end

    def each_edge_with_member(& : Hyperedge, NodeId ->)
      @edge_nodes.each do |(edge, _), member|
        yield edge, member
      end
    end

    # Returns `true` if *id* participates in the given hyperedge *edge*. Returns
    # `false` otherwise.
    def member?(id : NodeId, edge needle : Hyperedge) : Bool
      each_edge(id) do |edge|
        next unless edge == needle
        return true
      end

      false
    end

    # Converts `self` to an unordered graph through clique expansion: nodes that
    # share a hyperedge are connected; each unordered edge is represented with
    # a pair of edges going in opposite directions.
    def graph : Slice(Pf::USet32)
      groups = {} of Hyperedge => Pf::USet32

      each_node_with_id do |node, id|
        each_edge(id) do |edge|
          groups[edge] = (groups[edge]? || Pf::USet32.new).add(id)
        end
      end

      graph = Slice(Pf::USet32).new(order) { Pf::USet32.new }

      groups.each do |_, members|
        members.each do |u|
          members.each do |v|
            next if u == v

            vs = graph[u]
            next if v.in?(vs)

            graph[u] = vs.add(v)
          end
        end
      end

      graph.read_only
    end
  end
end
