module Ww
  # A trie that stores strings of `Ubase`s called *unit (or ubase) strands*. Trie
  # nodes can be bound to a `Uvertex`.
  class Utrie
    # :nodoc:
    EMPTY = new(vertex: Uvertex::NONE, fanout: Fanout[])

    # Returns the fanout part of this unit trie.
    getter fanout : Fanout

    protected def initialize(@vertex : Uvertex, @fanout : Fanout)
    end

    # Constructs an empty unit trie.
    def self.[] : Utrie
      EMPTY
    end

    private def_change

    # Returns `true` if this unit trie node contains no unit bases and is not bound to
    # a `Uvertex`, and thus is completely useless -- empty. Returns `false` otherwise.
    def empty? : Bool
      @vertex == Uvertex::NONE && @fanout.empty?
    end

    # If a `Uvertex` is assigned to this unit trie, returns that `Uvertex`;
    # otherwise, returns `nil`.
    def vertex? : Uvertex?
      @vertex == Uvertex::NONE ? nil : @vertex
    end

    private def at(base : Ubase::Nullary, & : Utrie -> Utrie) : Utrie
      if base.det.is_any?
        return yield self
      end

      store1 = @fanout.nullary(base) { |trie| yield trie }
      store1.same?(@fanout) ? self : change(fanout: store1)
    end

    private def at(base : Ubase::Unary, & : Utrie -> Utrie) : Utrie
      store1 = @fanout.unary(base) { |trie| yield trie }
      store1.same?(@fanout) ? self : change(fanout: store1)
    end

    private def at(base : Ubase::Match, & : Utrie -> Utrie) : Utrie
      store1 = @fanout.match(base.term) { |trie| yield trie }
      store1.same?(@fanout) ? self : change(fanout: store1)
    end

    private def at(base : Ubase::Segment, & : Utrie -> Utrie) : Utrie
      store1 = @fanout.segment(base.b, base.bx, base.e, base.ex) { |trie| yield trie }
      store1.same?(@fanout) ? self : change(fanout: store1)
    end

    private def at(base : Ubase::DivBy, & : Utrie -> Utrie) : Utrie
      store1 = @fanout.divisor(base.divisor) { |trie| yield trie }
      store1.same?(@fanout) ? self : change(fanout: store1)
    end

    private def at(base : Ubase::WhereKey, & : Utrie -> Utrie) : Utrie
      store1 = @fanout.key(base.key) { |trie| yield trie }
      store1.same?(@fanout) ? self : change(fanout: store1)
    end

    protected def at(path : Slice(Ubase), fn : Utrie -> Utrie) : Utrie
      if path.empty?
        return fn.call(self)
      end

      at(path[0], &.at(path[1..], fn))
    end

    # Follows *path* and substitutes the trie node thus reached with its transformed
    # version obtained by calling *fn*. Returns the modified copy of `self`.
    #
    # Call with *path* arrays that you and only you own. A *path* array must be
    # inaccessible while `at` is working.
    def at(path : Array(Ubase), &fn : Utrie -> Utrie) : Utrie
      at(path.to_readonly_slice, fn)
    end

    private def via?(base : Ubase::Nullary) : Utrie?
      @fanout.via_nullary?(base)
    end

    private def via?(base : Ubase::Unary) : Utrie?
      @fanout.via_unary?(base)
    end

    private def via?(base : Ubase::Match) : Utrie?
      @fanout.via_match?(base.term)
    end

    private def via?(base : Ubase::Segment) : Utrie?
      @fanout.via_segment?(base.b, base.bx, base.e, base.ex)
    end

    private def via?(base : Ubase::DivBy) : Utrie?
      @fanout.via_divisor?(base.divisor)
    end

    private def via?(base : Ubase::WhereKey) : Utrie?
      @fanout.via_key?(base.key)
    end

    protected def vertexof(path : Slice(Ubase)) : Uvertex
      if path.empty?
        return @vertex
      end

      unless neighbor = via?(path[0])
        return Uvertex::NONE
      end

      neighbor.vertexof(path[1..])
    end

    # Follows *path* and returns the `Uvertex` that the trie node thus reached is bound
    # to (see `bind`). If none, or *path* is absent in `self`, returns `Uvertex::NONE`.
    #
    # Call with *path* arrays that you and only you own. A *path* array must be
    # inaccessible while `vertexof` is working.
    def vertexof(path : Array(Ubase)) : Uvertex
      vertexof(path.to_readonly_slice)
    end

    # Unconditionally binds `self` to the given Uvertex *uv*.
    def bind(uv : Uvertex) : Utrie
      change(vertex: uv)
    end

    # Unconditionally removes the binding of `self` to a `Uvertex` (if present).
    def unbind : Utrie
      change(vertex: Uvertex::NONE)
    end
  end

  # Represents the fanout part of a unit trie `Utrie`. Essentially an object
  # containing edges of the owner `Utrie` to neighboring `Utrie`s, grouped by
  # edge label (e.g. nullary edge for `Ubase::Nullary` bases, divisor edges for
  # `Ubase::DivBy` bases etc.)
  class Utrie::Fanout
    # :nodoc:
    abstract struct SegPoint
      include Comparable(SegPoint)

      def <=>(other : SegPoint)
        {n, exclusive.to_unsafe} <=> {other.n, other.exclusive.to_unsafe}
      end

      record Begin < self, n : Term::Num, exclusive : Bool, children : RBTree(SegPoint)
      record End < self, n : Term::Num, exclusive : Bool, trie : Utrie
      record Probe < self, n : Term::Num, exclusive : Bool
    end

    # :nodoc:
    EMPTY = new(
      nullary: Pf::Map(Ubase::Nullary, Utrie).new,
      unary: Pf::Map(Ubase::Unary, Utrie).new,
      matches: Pf::Map(Term, Utrie).new,
      segments: RBTree(SegPoint)[],
      divisors: Pf::Map(Term::Num, Utrie).new,
      keys: Pf::Map(Term, Utrie).new,
    )

    protected def initialize(
      @nullary : Pf::Map(Ubase::Nullary, Utrie),
      @unary : Pf::Map(Ubase::Unary, Utrie),
      @matches : Pf::Map(Term, Utrie),
      @segments : RBTree(SegPoint),
      @divisors : Pf::Map(Term::Num, Utrie),
      @keys : Pf::Map(Term, Utrie),
    )
    end

    # Constructs an empty `Fanout`.
    def self.[] : Fanout
      EMPTY
    end

    private def_change

    # Returns `true` if this fanout object points to no tries, and is therefore
    # completely useless -- empty.
    def empty? : Bool
      same?(EMPTY) || {@nullary, @unary, @matches, @segments, @divisors, @keys}.all?(&.empty?)
    end

    # Changes the trie reached via a nullary *k* using the block, returns a modified
    # copy of `self`.
    def nullary(k : Ubase::Nullary, & : Utrie -> Utrie) : Fanout
      v0 = @nullary[k]? || Utrie[]
      v1 = yield v0
      v1.same?(v0) ? self : change(nullary: v1.empty? ? @nullary.dissoc(k) : @nullary.assoc(k, v1))
    end

    # Returns the trie reached via a nullary *k*, or `nil` if no such trie.
    def via_nullary?(k : Ubase::Nullary) : Utrie?
      @nullary[k]?
    end

    # Yields tries reached via nullary edges.
    def via_nullary(& : Ubase::Nullary, Utrie ->) : Nil
      @nullary.each { |k, trie| yield k, trie }
    end

    # Returns the number of nullary edges between this fanout object and
    # neighboring tries.
    def nnullaries : Int32
      @nullary.size
    end

    # Changes the trie reached via a unary *k* using the block, returns a modified
    # copy of `self`.
    def unary(k : Ubase::Unary, & : Utrie -> Utrie) : Fanout
      v0 = @unary[k]? || Utrie[]
      v1 = yield v0
      v1.same?(v0) ? self : change(unary: v1.empty? ? @unary.dissoc(k) : @unary.assoc(k, v1))
    end

    # Returns the trie reached via a unary *k*, or `nil` if no such trie.
    def via_unary?(k : Ubase::Unary) : Utrie?
      @unary[k]?
    end

    # Yields tries reached via unary edges.
    def via_unary(& : Ubase::Unary, Utrie ->) : Nil
      @unary.each { |k, trie| yield k, trie }
    end

    # Returns the number of unary edges between this fanout object and
    # neighboring tries.
    def nunaries : Int32
      @unary.size
    end

    # Changes the trie reached via a literally matched term *k* using the block,
    # returns a modified copy of `self`.
    def match(k : Term, & : Utrie -> Utrie) : Fanout
      v0 = @matches[k]? || Utrie[]
      v1 = yield v0
      v1.same?(v0) ? self : change(matches: v1.empty? ? @matches.dissoc(k) : @matches.assoc(k, v1))
    end

    # Returns the trie reached via a literally matched term *k*, or `nil` if no
    # such trie.
    def via_match?(k : Term) : Utrie?
      @matches[k]?
    end

    # Returns the number of match edges between this fanout object and
    # neighboring tries.
    def nmatches : Int32
      @matches.size
    end

    # Changes the trie reached via divisor *k* using the block, returns a modified
    # copy of `self`.
    def divisor(k : Term::Num, & : Utrie -> Utrie) : Fanout
      v0 = @divisors[k]? || Utrie[]
      v1 = yield v0
      v1.same?(v0) ? self : change(divisors: v1.empty? ? @divisors.dissoc(k) : @divisors.assoc(k, v1))
    end

    # Returns the trie reached via a divisor *k*, or `nil` if no such trie.
    def via_divisor?(k : Term::Num) : Utrie?
      @divisors[k]?
    end

    # Yields tries reached via divisor edges.
    def via_divisor(& : Term::Num, Utrie ->) : Nil
      @divisors.each { |k, trie| yield k, trie }
    end

    # Returns the number of divisor edges between this fanout object and
    # neighboring tries.
    def ndivisors : Int32
      @divisors.size
    end

    # Changes the trie reached via entry key term *k* using the block, returns
    # a modified copy of `self`.
    def key(k : Term, & : Utrie -> Utrie) : Fanout
      v0 = @keys[k]? || Utrie[]
      v1 = yield v0
      v1.same?(v0) ? self : change(keys: v1.empty? ? @keys.dissoc(k) : @keys.assoc(k, v1))
    end

    # Returns the trie reached via an entry key term *k*, or `nil` if no such trie.
    def via_key?(k : Term) : Utrie?
      @keys[k]?
    end

    # Yields tries reached via entry key edges.
    def via_key(& : Term, Utrie ->) : Nil
      @keys.each { |k, trie| yield k, trie }
    end

    # Returns the number of entry key edges between this fanout object and
    # neighboring tries.
    def nkeys : Int32
      @keys.size
    end

    # Changes the trie reached via a segment (range with bounded begin *b* and end *e*)
    # using the block, returns the modified copy of `self`.
    #
    # - *bx* excludes *b* from the range,
    # - *ex* excludes *e* from the range.
    def segment(b : Term::Num, bx : Bool, e : Term::Num, ex : Bool, & : Utrie -> Utrie) : Fanout
      # Gosh this is scary!

      @segments.query(SegPoint::Probe.new(b, bx)) do |begin0|
        begin0 = begin0.as(SegPoint::Begin)
        begin0.children.query(SegPoint::Probe.new(e, ex)) do |end0|
          end0 = end0.as(SegPoint::End)

          trie0 = end0.trie
          trie1 = yield trie0
          return self if trie0.same?(trie1)

          end1 = trie1.empty? ? nil : end0.copy_with(trie: trie1)
          begin1 = begin0.copy_with(children: end1.nil? ? begin0.children.without(end0) : begin0.children.with(end1))
          ranges1 = begin1.children.empty? ? @segments.without(begin0) : @segments.with(begin1)
          return change(segments: ranges1)
        end

        trie0 = Utrie[]
        trie1 = yield trie0
        return self if trie0.same?(trie1) || trie1.empty?

        end1 = SegPoint::End.new(e, ex, trie1)
        begin1 = begin0.copy_with(children: begin0.children.with(end1))
        ranges1 = @segments.with(begin1)

        return change(segments: ranges1)
      end

      trie0 = Utrie[]
      trie1 = yield trie0
      return self if trie0.same?(trie1) || trie1.empty?

      end1 = SegPoint::End.new(e, ex, trie1)
      begin1 = SegPoint::Begin.new(b, bx, RBTree(SegPoint)[end1])
      ranges1 = @segments.with(begin1)

      change(segments: ranges1)
    end

    # Returns the trie reached via the given segment (see `segment`), or `nil` if
    # no such trie.
    def via_segment?(b : Term::Num, bx : Bool, e : Term::Num, ex : Bool) : Utrie?
      @segments.query(SegPoint::Probe.new(b, bx)) do |begin0|
        begin0 = begin0.as(SegPoint::Begin)
        begin0.children.query(SegPoint::Probe.new(e, ex)) do |end0|
          end0 = end0.as(SegPoint::End)
          return end0.trie
        end
      end
    end

    # Yields tries reached via segments containing the given number *n*.
    def via_segment_containing(n : Term::Num, &fn : Utrie ->) : Nil
      @segments.lt(SegPoint::Probe.new(n, exclusive: false), eq: true) do |begin0|
        begin0 = begin0.as(SegPoint::Begin)
        next if begin0.exclusive && begin0.n == n

        begin0.children.gt(SegPoint::Probe.new(n, exclusive: false), eq: true) do |end0|
          end0 = end0.as(SegPoint::End)
          next if end0.exclusive && end0.n == n

          fn.call(end0.trie)
        end
      end
    end
  end
end
