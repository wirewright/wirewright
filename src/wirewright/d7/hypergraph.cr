module Ww::D7
  # Represents a hyperedge.
  #
  # *mod* is the address of the module that defines the scope of the edge *term*.
  defrecord Hyperedge, mod : NodeAddr, term : Term

  # A hypergraph is a graph whose edges can include any number of nodes; each edge
  # is a subset of the set of nodes in that graph. It's easier to think of a hypergraph
  # as a community of nodes. Each node can participate in zero or more groups, each
  # group formed from other nodes in the community.
  struct Hypergraph
    # - *nodemap* maps nodes to node ids (implicit, array index).
    # - *edgemap* maps node ids (implicit, array index) to hyperedges that node
    #   is participating in.
    # - *trmap* maps node ids (implicit, array index) to node addresses (from `fold`).
    def initialize(@nodemap : Array(Term), @edgemap : Array(Slice(Hyperedge)), @trmap : Array(NodeAddr))
    end

    # Returns the number of nodes in this graph.
    def order
      @nodemap.size
    end

    # Returns the node associated with the given *id*.
    def [](id : NodeId) : Term
      @nodemap[id]
    end

    # Returns the `fold`-address of the node associated with the given *id*.
    def addr(id : NodeId) : NodeAddr
      @trmap[id]
    end

    # Yields nodes of this hypergraph.
    def each_node_with_id(&) : Nil
      @nodemap.each_with_index do |node, id|
        yield node, NodeId.new(id)
      end
    end

    # Yields hyperedges of a node with the given node *id* (as yielded
    # by `each_node`).
    def each_edge(id : NodeId, & : Hyperedge ->) : Nil
      @edgemap[id].each { |edge| yield edge }
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

      graph.readonly
    end
  end
end
