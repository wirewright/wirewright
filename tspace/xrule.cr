module Ww
  # Represents an Xgraph rule, which is a set of Uvertices (see `Uvertex`). A "column"
  # of Xgraph rules, in their normal form (see `normal`), is, in effect, "collapsed"
  # to form an `Xgraph`.
  #
  # ```text
  # Xrule        Xrule#normal    Xgraph
  # ---------    -------------   --------
  #
  # 4 1 2        1 2 4                    +-----> 4
  #                                       |
  # 2 6 1        1 2 6                    +-----> 6
  #                                       v
  # 1 8 2        1 2 8            1 <-->  2 <---> 7 <--+
  #                                       ^            |
  # 2 7 1 8      1 2 7 8                  +-----> 8 <--+
  #                                       |
  # 10 1 9 2     1 2 9 10                 +-----> 9 <--+
  #                                       |            |
  #                                       +-----> 10 <-+
  # ```
  #
  # An Xgraph rule is said to be satisfied in a query set when all its member Uvertices
  # are present in that query set.
  struct Xrule
    # :nodoc:
    EMPTY = new(vertices: Pf::Set(Uvertex).new, hash: 1u64)

    protected def initialize(@vertices : Pf::Set(Uvertex), @hash : UInt64)
    end

    protected def self.hashplus(h1 : UInt64, h2 : UInt64) : UInt64
      h1 &+ h2
    end

    # Constructs an empty Xrule.
    def self.[] : Xrule
      EMPTY
    end

    # Constructs an Xrule from an enumerable of Uvertices *vertices*.
    def self.[](vertices : Enumerable(Uvertex)) : Xrule
      new(vertices.to_pf_set, vertices.reduce(1u64) { |hash, vertex| hashplus(hash, vertex.hash) })
    end

    # Constructs an Xrule with the given *vertices* as members.
    def self.[](*vertices : Uvertex) : Xrule
      self[vertices]
    end

    # Returns the number of members of this rule.
    def size : Int32
      @vertices.size
    end

    # Returns `true` if this rule is a singleton rule (contains only one member vertex).
    def singleton? : Bool
      size == 1
    end

    # Yields each member Uvertex of this rule.
    def each_vertex(& : Uvertex ->) : Nil
      @vertices.each { |vertex| yield vertex }
    end

    # Renders this rule in normal form, as an array of Uvertices.
    def normal : Array(Uvertex)
      normal = @vertices.to_a
      normal.unstable_sort!
    end

    # Returns `true` if this rule is satisfied in the given *set* of vertices; meaning
    # all member vertices of this rule are present in *set*.
    def satisfied?(set : Set(Uvertex)) : Bool
      return false if size > set.size

      each_vertex do |vertex|
        next if vertex.in?(set)
        return false
      end

      true
    end

    # Adds *vertex* to the list of member Uvertices of this rule. Returns a modified
    # copy of this rule.
    def with(vertex : Uvertex) : Xrule
      Xrule.new(@vertices.add(vertex), Xrule.hashplus(@hash, vertex.hash))
    end

    def inspect(io)
      io << "Xrule("
      @vertices.join(io, " && ")
      io << ")"
    end

    def_hash @hash
    def_equals @vertices
  end
end
