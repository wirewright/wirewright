module Ww::Sparse
  # A data structure used for pattern matching unordered intersections.
  #
  # The simplest way to describe the problem that this data structure is trying to
  # solve efficiently is: output all words that can be built using the given alphabet
  # from a dictionary of words; supporting efficient addition and deletion of words
  # to/from the dictionary, if possible, sublinear relative to the amount of words
  # in the dictionary.
  class Xgraph
    # Holds the number of unique ground vertices present in this graph.
    protected property nground = 0

    def initialize
      @relations = Set(Vertex).new
    end

    # Creates a bidirectional link between two vertices *a* and *b*.
    private def connect!(a : Vertex, b : Vertex)
      b.relate(a, label: a.relate(b, label: nil))
    end

    # Connects all vertices in the given *vertices* array.
    private def connect!(vertices : Array(Vertex))
      case vertices.size
      when 0 then raise "empty relation"
      when 1 then return vertices[0]
      when 2 then return connect!(vertices[0], vertices[1])
      end

      layer = vertices
      above = Array(Vertex).new(layer.size // 2)

      until layer.size == 1
        index = 0
        while index < layer.size
          a = layer[index]
          b = layer[index + 1]? || layer[index - 1]
          above << connect!(a, b)
          index += 2
        end
        layer.clear
        layer, above = above, layer
      end

      layer.first
    end

    # Returns `true` if this graph contains no relations.
    def empty? : Bool
      @relations.empty?
    end

    # Returns `true` if *vertex* is a relation in this graph.
    def relation?(vertex : Vertex)
      vertex.in?(@relations)
    end

    # Creates and returns a new ground `Vertex` for use in relations within
    # this graph.
    #
    # *cleanup* is an optional function that will be called when the vertex
    # is removed from the graph.
    def vertex(&cleanup : ->) : Vertex
      Vertex.new(self, &cleanup)
    end

    # :ditto:
    def vertex
      Vertex.new(self)
    end

    # Assembles all *vertices* into a single "group" called a *relation*, whose
    # corresponding vertex is subsequently returned. Note that for equal sets of
    # *vertices*, the same relation vertex is returned.
    #
    # ```
    # xg = Xgraph.new
    #
    # x0 = xg.vertex
    # x1 = xg.vertex
    # x2 = xg.vertex
    # x3 = xg.vertex
    #
    # r0 = xg.relate({x0, x1})
    # r1 = xg.relate({x1, x2, x3})
    # r2 = xg.relate({x0, x3})
    #
    # xg.eval(Set{x0, x1, x3}) do |x|
    #   p! x
    #   # x => r0
    #   # x => r2
    # end
    # ```
    def relate(vertices : Set(Vertex))
      unless vertices.all?(&.belongs_to?(self))
        raise "to relate vertices, all of them must belong to the same graph"
      end

      # Sort by [transiently] unique object id to get the same order of connections.
      array = vertices.to_a.unstable_sort_by!(&.object_id)

      relation = connect!(array)
      @relations << relation
      relation
    end

    # See `relate`; converts *vertices* into a set for you.
    def relate(vertices : Enumerable(Vertex))
      relate(vertices.to_set)
    end

    # Dissolves (removes) a relation made using `relate` given its relation vertex.
    def dissolve(relation : Vertex)
      return unless @relations.delete(relation)

      relation.try_dissolve
    end

    private def evaliter(vertices, &)
      @relations.each do |relation|
        next unless relation.present?(vertices)
        yield relation
      end
    end

    # Evaluates a set of *vertices*, yielding relation vertices of relations whose
    # members were all found in it.
    #
    # Mutates the set of *vertices*. If undesired, use `eval`. For an example,
    # see `relate`.
    def eval!(vertices : Set(Vertex), & : Vertex ->)
      # If the vertices set contains more than 50% of the total number of ground
      # vertices in this graph then it will be cheaper to dumbly iterate over all
      # relations. The smart approach is going to be *vastly* more expensive due
      # to the massive traffic between layers. This achieves trade-off between
      # massive queries vs. "small island" queries.
      if vertices.size > @nground * 0.5
        evaliter(vertices) { |ceil| yield ceil }
        return
      end

      above = Set(Vertex).new

      until vertices.empty?
        vertices.each &.elevate(self, vertices, above) { |ceil| yield ceil }
        vertices.clear
        vertices, above = above, vertices
      end
    end

    # Evaluates a set of *vertices*, yielding relation vertices of relations whose
    # members were all found in it.
    #
    # For an example, see `relate`.
    def eval(vertices : Set(Vertex), & : Vertex ->)
      eval!(vertices.dup) { |relation| yield relation }
    end

    # Same as `eval` but collects the yielded relation vertices into a set for you.
    # This set is subsequently returned.
    def eval(vertices : Set(Vertex)) : Set(Vertex)
      relations = Set(Vertex).new
      eval(vertices) { |relation| relations << relation }
      relations
    end

    # Appends a DOT/Graphviz representation of this graph to *io*.
    def to_dot(io)
      seen = Set(Vertex).new

      io.puts "strict graph {"
      @relations.each do |relation|
        relation.to_dot(io, self, seen)
      end
      io.puts "}"
    end
  end

  # Represents a vertex bound to an `Xgraph`. `Vertex` instances act as vertices
  # and edge labels simultaneously.
  #
  # Thus a multi-layered structure is formed where edges in the current layer are
  # nodes in the layer above. If one allows cross-layer edges, one will observe
  # better packing but (sometimes *much*) worse performance. But in Wirewright we
  # work under the assumption of small clusters ("islands") so the "better packing"
  # part isn't really reachable most of the times. Therefore a split-layer architecture
  # is employed.
  class Xgraph::Vertex
    private NOP = ->{}

    private on_demand up : Hash(Vertex, Vertex)

    protected def initialize(@graph : Xgraph, @down : {Vertex, Vertex}? = nil, &@cleanup : ->)
      @graph.nground += 1 unless @down
    end

    protected def initialize(*args, **kwargs)
      initialize(*args, **kwargs, &NOP)
    end

    # Returns `true` if this vertex belongs to the given *graph*.
    def belongs_to?(graph : Xgraph) : Bool
      @graph.same?(graph)
    end

    # Returns `true` if this vertex is a top-level (public) relation in its graph.
    def relation? : Bool
      @graph.relation?(self)
    end

    # Creates a *unidirectional* link between `self` and *other*. Takes an optional
    # *label* argument for the label vertex, otherwise creates a new label vertex.
    #
    # Returns the resulting label vertex (i.e. *label* if it was provided, otherwise
    # the new label vertex).
    protected def relate(other : Vertex, label : Vertex?) : Vertex
      up.put_if_absent(other) { label || Vertex.new(@graph, {self, other}) }
    end

    # Dissolves a *unidirectional* link between `self` and *other*.
    protected def dissolve(other : Vertex) : Nil
      return unless has_up?

      up.delete(other)
    end

    # Tries to dissolve this vertex.
    #
    # - If this vertex is public (exported as a relation by *graph*), dissolution
    #   is cancelled.
    # - If this vertex is relied upon by a vertex in the layer above dissolution
    #   is cancelled.
    # - Otherwise, dissolution succeeds on this vertex, and proceeds recursively in
    #   case this vertex is a label vertex.
    #
    # For each ground vertex reached (vertices that are not label vertices are
    # considered ground vertices), *ground* is called with that vertex.
    #
    # Cycles won't ever dissolve. However, it should be generally impossible to
    # obtain a cycle.
    protected def try_dissolve : Nil
      return if relation?
      return if has_up?

      unless down = @down
        @graph.nground -= 1
        @cleanup.call
        return
      end

      from, to = down

      # Dissolve the edge that `self` is a representation of.
      from.dissolve(to)
      to.dissolve(from)

      # Recurse.
      from.try_dissolve
      to.try_dissolve
    end

    # Bottom-up motion: ground vertices to groups.
    protected def elevate(graph, vertices : Set(Vertex), above : Set(Vertex), & : Vertex ->)
      yield self if graph.relation?(self)

      return unless has_up?

      if vertices.size < up.size
        vertices.each do |vertex|
          next unless edge = up[vertex]?
          above << edge
        end
      else
        up.each do |req, edge|
          next unless req.in?(vertices)
          above << edge
        end
      end
    end

    # Top-down motion: groups to ground vertices.
    protected def present?(vertices : Set(Vertex))
      return in?(vertices) unless down = @down

      from, to = down
      from.present?(vertices) && to.present?(vertices)
    end

    # :nodoc:
    def to_dot(io, seen = Set(Vertex).new)
      return unless seen.add?(self)

      if relation?
        io << '"' << self << "\" [peripheries=2]"
        io.puts
      end

      return unless down = @down

      from, to = down
      from.to_dot(io, seen)
      to.to_dot(io, seen)
      io << '"' << from << "\" -- \"" << to << "\" [label=\"" << self << "\"]"
    end

    def inspect(io)
      io << "⸢"
      object_id.to_s(io, base: 31)
      if relation?
        io << "*"
      end
      io << "⸥"
    end

    def to_s(io)
      inspect(io)
    end
  end
end
