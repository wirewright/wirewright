module Ww
  # A graph-backed data structure used for storing unordered `Xrule`s and querying
  # for Xrules present in a query set efficiently.
  #
  # If the amount of Xrules present in some query set Q is estimated to be small
  # enough, the performance of Xgraph will depend on the size of Q. For instance,
  # if the Xgraph contains a hundred thousand rules but only few of those are
  # estimated to be selected by Q, the number of steps needed to perform the search
  # will depend on the size of Q, smaller (sometimes much) than a full scan of
  # the Xrules.
  #
  # Computers are really fast and good at scanning vast amounts of data. Especially
  # since there are virtually no allocations made during a scan, only fetches that
  # are moderately sequential (32 rules in sequence -> pointer -> 32 rules in sequence -> ...).
  # As a con, there is the dependence on the number of Xrules (see `scan`). Comparing
  # this to graph exploration, we have heavy allocation and fetch memory traffic
  # (see `explore`). But in exchange we get dependence on the size of Q. Thus for
  # smaller Qs we estimate a small number of matching rules and use graph exploration;
  # and for larger Qs we estimate a large number of matching rules and use scanning.
  #
  # TODO: Consider storing @rules in a trie. We sort of do it already it @graph,
  # but with @graph we need undirected connections -- a trie can't give us those.
  class Xgraph
    # :nodoc:
    EMPTY = new(
      fresh: Xvertex::ZERO,
      graph: Pf::Map(Uvertex, Pf::Set(Uvertex)).new,
      rules: Pf::Map(Xrule, Xvertex).new,
      refcount: RefTracker(Xvertex)[],
      urefcount: Pf::Map(Uvertex, UInt32).new,
    )

    # :nodoc:
    NEIGHBORS_NONE = Pf::Set(Uvertex).new

    protected def initialize(
      @fresh : Xvertex,
      @graph : Pf::Map(Uvertex, Pf::Set(Uvertex)),
      @rules : Pf::Map(Xrule, Xvertex),
      @refcount : RefTracker(Xvertex),
      @urefcount : Pf::Map(Uvertex, UInt32),
    )
    end

    # Constructs an empty intersection graph (Xgraph).
    def self.[] : Xgraph
      EMPTY
    end

    private def_change

    # Returns `true` if *conn* has acquired one or more references to Xvertex *xv*.
    # Returns `false` otherwise.
    def acquired?(conn : Tconn::Id, xv : Xvertex) : Bool
      @refcount.nonzero?(xv, conn)
    end

    # Returns the Xvertex corresponding to *rule*. Returns `Xvertex::NONE` if *rule*
    # is absent from this Xgraph.
    def vertexof(rule : Xrule) : Xvertex
      @rules[rule]? || Xvertex::NONE
    end

    # If you happen to know the Xvertex *xv* corresponding to *rule*, you can call
    # this method for slightly better performance compared to `acquire(conn, rule)`.
    #
    # See also: `acquire(conn, rule)`.
    #
    # WARNING: The behavior of this method is not specified in case *rule* does not
    # correspond to *xv*.
    def acquire(conn : Tconn::Id, rule : Xrule, xv : Xvertex) : Xgraph
      # If rule already defined, return its vertex, do not forget to increment its
      # reference count.
      unless xv == Xvertex::NONE
        return change(refcount: @refcount.inc(xv, conn))
      end

      norm = rule.normal

      # Increment reference count of member Uvertices.
      urefcount = @urefcount.transaction do |commit|
        rule.each_vertex do |uvertex|
          commit.assoc(uvertex, (commit[uvertex]? || 0u32) + 1)
        end
      end

      # Link consecutive Uvertices with undirected edges.
      graph = @graph.transaction do |graph|
        norm.each_cons_pair do |u, v|
          graph
            .assoc(u, (graph[u]? || NEIGHBORS_NONE).add(v))
            .assoc(v, (graph[v]? || NEIGHBORS_NONE).add(u))
        end
      end

      change(
        fresh: @fresh + 1,
        graph: graph,
        rules: @rules.assoc(rule, @fresh),
        refcount: @refcount.inc(@fresh, conn),
        urefcount: urefcount,
      )
    end

    # Increments the reference count of *rule* and its corresponding Xvertex
    # for *conn*. Returns the modified copy of this Xgraph.
    def acquire(conn : Tconn::Id, rule : Xrule) : Xgraph
      acquire(conn, rule, xv: vertexof(rule))
    end

    # If you happen to know the Xvertex *xv* corresponding to *rule*, you can call
    # this method for slightly better performance compared to `release(conn, rule)`.
    #
    # See also: `release(conn, rule)`.
    #
    # WARNING: The behavior of this method is not specified in case *rule* does not
    # correspond to *xv*.
    def release(conn : Tconn::Id, rule : Xrule, xv : Xvertex) : Xgraph
      return self if xv == Xvertex::NONE
      return self unless decresp = @refcount.dec?(xv, conn)

      refcount, deleted = decresp

      unless deleted
        return change(refcount: refcount)
      end

      # All references to rule lost.

      # Remove the rule.
      rules = @rules.dissoc(rule)

      # Decrement reference count for member vertices of the rule. Populate
      # the deletion array with vertices whose reference count reached zero.
      deletion = nil
      urefcount = @urefcount.transaction do |commit|
        rule.each_vertex do |uvertex|
          nrefs = commit[uvertex]

          if nrefs == 1
            unless rule.singleton? # Singleton rules are not in the graph.
              queue = deletion ||= [] of Uvertex
              queue << uvertex
            end

            commit.dissoc(uvertex)
          else
            commit.assoc(uvertex, nrefs - 1)
          end
        end
      end

      if deletion.nil?
        return change(rules: rules, refcount: refcount, urefcount: urefcount)
      end

      graph = @graph.transaction do |commit|
        # Notify neighbors of deleted uvertecies.
        deletion.each do |uvertex|
          neighbors = commit[uvertex]
          neighbors.each do |neighbor|
            commit.assoc(neighbor, commit[neighbor].delete(uvertex))
          end
        end

        # Delete uvertices.
        deletion.each do |uvertex|
          commit.dissoc(uvertex)
        end
      end

      change(graph: graph, rules: rules, refcount: refcount, urefcount: urefcount)
    end

    # Decrements the reference count of *rule* and its corresponding Xvertex
    # for *conn*. Returns the modified copy of this Xgraph.
    def release(conn : Tconn::Id, rule : Xrule) : Xgraph
      release(conn, rule, xv: vertexof(rule))
    end

    private def explore(uvs, uv, rule0, seen, &fn : Xvertex ->) : Nil
      return unless seen.add?(rule0)

      if rulev = @rules[rule0]?
        fn.call(rulev)
      end

      return unless neighbors = @graph[uv]?

      if uvs.size < neighbors.size
        uvs.each do |other|
          next if uv == other
          next unless other.in?(neighbors)
          rule1 = rule0.with(other)
          next if rule0 == rule1
          explore(uvs, other, rule1, seen, &fn)
        end
      else
        neighbors.each do |neighbor|
          next unless neighbor.in?(uvs)
          rule1 = rule0.with(neighbor)
          next if rule0 == rule1
          explore(uvs, neighbor, rule1, seen, &fn)
        end
      end
    end

    # Same as `rules`, but forces a graph explore. Performance depends on the size
    # of the set of Uvertices *uvs* (more or less).
    def explore(in uvs : Set(Uvertex), &fn : Xvertex ->) : Nil
      seen = Set(Xrule).new

      uvs.each do |uvertex|
        explore(uvs, uvertex, Xrule[uvertex], seen, &fn)
      end
    end

    # Same as `rules`, but forces a scan of the set of Uvertices *uvs*. Performance
    # depends on the amount of rules in the Xgraph and on their length.
    def scan(uvs : Set(Uvertex), &fn : Xvertex ->) : Nil
      @rules.each do |rule, rulev|
        next unless rule.satisfied?(uvs)

        fn.call(rulev)
      end
    end

    # Calls *fn* with Xvertices corresponding to Xrules that match the set of
    # Uvertices *uvs*.
    def rules(in uvs : Set(Uvertex), &fn : Xvertex ->) : Nil
      # max = @refcount.size - @rules.size

      # Computers are really really good at scanning vs. graph exploration. The latter
      # besides other things features heavy memory traffic and the like; while
      # scanning doesn't allocate anything. So the threshold for scanning is
      # set rather low.
      uvs.size < 128 ? explore(uvs, &fn) : scan(uvs, &fn)
    end
  end
end
