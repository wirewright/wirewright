class Ww::Term::Dict
  # The design of `UTermTrie32` is very similar in spirit to `Pf::USet32`'s.
  # Both are heavily inspired by Rich Hickey's persistent vector design.
  #
  # Some notes:
  #
  # I was planning to use a finger tree initially, but finger trees turned out to
  # be slow, at least the way I implemented them; I had several attempts to optimize
  # the implementation, but they all more or less failed.
  #
  # First, finger trees are inherently deeper than this. Here we're 16-way so
  # lookup is log16 while finger trees are 2/3-way-ish and lookup is log2. Even
  # though it barely matters in practice for us it does. 500k word list finger
  # tree looks up 250_000th number in ~45ns on my machine, that's the best I was
  # able to make it. This thing looks it up in maybe 10-15ns; the prior impl
  # looks up in 20ns or so. It *is* a micro-benchmark, yes. But we simply can't
  # afford the new impl to be slow at *anything at all*, even at micro-benchmarks.
  # Whatever we do must beat the prior impl. Otherwise, what's the point?
  #
  # A similar story for assocs and dissocs. It is also much easier to add support for
  # cookies in UTermTrie32, and cookies allocate way less, and we're generally
  # on the faster side doing memcpy and memmove and the like, optimized by people
  # far smarter than I am, instead of doing anything ad-hoc.
  #
  # That said, we do have significant losses by not using a finger tree. Splits
  # are still expensive. Joins are still expensive. That's very sad, but when you
  # look at it from a practical standpoint, we rarely, if ever, split or join large
  # dicts. The worst offender here is perhaps the `queue` node in Rack, or editR's
  # mailbox. That is, we routinely prepend to them. Backmaps also like to insert in
  # random spots from time to time. However, these operations are done, in average,
  # on on very small dicts, and by using a finger tree in such small cases, we'd
  # probably be facing a lot of overhead anyway. I can't say for sure though!
  #
  # UTermTrie32 is used in dicts to store all keys that fall into UInt32 domain.
  # This means that now, compared to the prior impl, we don't have to split anything
  # or move entries between the itemspart and the pairspart, *provided we have
  # an efficient way to separate entries whose key is before mex, and entries
  # whose key is after, in the trie*. Thanks to *presence* and *full* bitmaps,
  # and in opposition to finger trees, we indeed can do this with UTermTrie32
  # very quickly. For instance, running `seqpart` on tries of hundreds of thousands
  # of entries completes in less than a microsecond on my machine.
  #
  # An important fast path when retrieving the itemspart of a dict is supported.
  # Namely, if the UTermTrie of the dict is a sequence, meaning there are no
  # entries past its mex, the entire trie is the itemspart and no work is needed.
  # The question -- about whether there are entries past the mex of the trie -- is
  # encoded in the comparison `seqsize == size` (see `seq_only?`). With UTermTrie32,
  # it is effectively two fetches. That is, we cache enough to make this comparison have
  # negligible cost, both on assoc- and dissoc-side, and on the comparison side;
  # and thus, the fast path has a fast guard. The guard fires almost always. Very
  # rarely in practice do we have entries with keys past the mex.
  module UTermTrie32
    extend self

    alias Trie = Leaf | Node

    defcase Leaf, cookie : Cookie, summary : Summary, children : TermMap16, mutation: true

    alias Node = Node0 | Node1 | Node2 | Node3 | Node4 | Node5 | Node6

    defcase Node0, seqsize : UInt32, cookie : Cookie, summary : Summary, children : NodeMap16(Leaf), mutation: true
    defcase Node1, seqsize : UInt32, cookie : Cookie, summary : Summary, children : NodeMap16(Node0), mutation: true
    defcase Node2, seqsize : UInt32, cookie : Cookie, summary : Summary, children : NodeMap16(Node1), mutation: true
    defcase Node3, seqsize : UInt32, cookie : Cookie, summary : Summary, children : NodeMap16(Node2), mutation: true
    defcase Node4, seqsize : UInt32, cookie : Cookie, summary : Summary, children : NodeMap16(Node3), mutation: true
    defcase Node5, seqsize : UInt32, cookie : Cookie, summary : Summary, children : NodeMap16(Node4), mutation: true
    defcase Node6, seqsize : UInt32, cookie : Cookie, summary : Summary, children : NodeMap16(Node5), mutation: true

    # :nodoc:
    EMPTY_LEAF = Leaf.new(Cookie.none, Summary.zero, TermMap16.empty)

    # :nodoc:
    EMPTY_NODE0 = Node0.new(0u32, Cookie.none, Summary.zero, NodeMap16(Leaf).empty)

    # :nodoc:
    EMPTY_NODE1 = Node1.new(0u32, Cookie.none, Summary.zero, NodeMap16(Node0).empty)

    # :nodoc:
    EMPTY_NODE2 = Node2.new(0u32, Cookie.none, Summary.zero, NodeMap16(Node1).empty)

    # :nodoc:
    EMPTY_NODE3 = Node3.new(0u32, Cookie.none, Summary.zero, NodeMap16(Node2).empty)

    # :nodoc:
    EMPTY_NODE4 = Node4.new(0u32, Cookie.none, Summary.zero, NodeMap16(Node3).empty)

    # :nodoc:
    EMPTY_NODE5 = Node5.new(0u32, Cookie.none, Summary.zero, NodeMap16(Node4).empty)

    # :nodoc:
    EMPTY_NODE6 = Node6.new(0u32, Cookie.none, Summary.zero, NodeMap16(Node5).empty)

    private def summarize(children, &) : Summary
      summary = Summary.zero
      Map16.each_value(children) do |child|
        summary = Summary.union(summary, yield child)
      end
      summary
    end

    private def summarize(children : TermMap16) : Summary
      summarize(children) { |child| Summary.of(child) }
    end

    private def summarize(children : NodeMap16) : Summary
      summarize(children, &.summary)
    end

    def empty
      EMPTY_LEAF
    end

    def leaf(cookie : Cookie, summary, children)
      Leaf.new(cookie, summary, children)
    end

    def leaf(cookie : Cookie, summary, children, *, prototype : Leaf)
      if prototype.cookie.allows_mutation_by?(cookie)
        prototype.summary = summary
        prototype.children = children
        return prototype
      end

      Leaf.new(cookie, summary, children)
    end

    def leaf(cookie : Cookie, children, **kwargs)
      leaf(cookie, summarize(children), children, **kwargs)
    end

    {% for level in 0..6 %}
      def node{{level}}(cookie : Cookie, summary, children)
        seqsize = Map16.full?(children) ? summary.size : seqsize(children)

        Node{{level}}.new(seqsize, cookie, summary, children)
      end

      def node{{level}}(cookie : Cookie, summary, children, *, prototype : Node{{level}})
        seqsize = Map16.full?(children) ? summary.size : seqsize(children)

        if prototype.cookie.allows_mutation_by?(cookie)
          prototype.seqsize = seqsize
          prototype.summary = summary
          prototype.children = children
          return prototype
        end

        Node{{level}}.new(seqsize, cookie, summary, children)
      end

      def node{{level}}(cookie : Cookie, children, **kwargs)
        node{{level}}(cookie, summarize(children), children, **kwargs)
      end
    {% end %}

    defrecord LeafKey, value : UInt8

    alias NodeKey = NodeKey0 | NodeKey1 | NodeKey2 | NodeKey3 | NodeKey4 | NodeKey5 | NodeKey6

    defrecord NodeKey0, value : UInt8, successor : LeafKey
    defrecord NodeKey1, value : UInt8, successor : NodeKey0
    defrecord NodeKey2, value : UInt8, successor : NodeKey1
    defrecord NodeKey3, value : UInt8, successor : NodeKey2
    defrecord NodeKey4, value : UInt8, successor : NodeKey3
    defrecord NodeKey5, value : UInt8, successor : NodeKey4
    defrecord NodeKey6, value : UInt8, successor : NodeKey5

    def decompose(key : UInt32)
      component = key & 0xfu32
      result = LeafKey.new(component.to_u8)
      key >>= 4
      return result if key.zero?

      {% for level in 0..5 %}
        component = key & 0xfu32
        result = NodeKey{{level}}.new(component.to_u8, result)
        key >>= 4
        return result if key.zero?
      {% end %}

      NodeKey6.new(key.to_u8, result)
    end

    def height(arg)
      case arg
      in Leaf, LeafKey   then 0
      in Node0, NodeKey0 then 1
      in Node1, NodeKey1 then 2
      in Node2, NodeKey2 then 3
      in Node3, NodeKey3 then 4
      in Node4, NodeKey4 then 5
      in Node5, NodeKey5 then 6
      in Node6, NodeKey6 then 7
      end
    end

    # NOTE: Wrap needs *cookie* because we'd like the nodes it creates (if any)
    # to be owned by *cookie*. This way, later, if we mutate using *cookie*,
    # we'll mutate right away, without having to path-copy the nodes created
    # by `wrap`.

    def wrap(cookie : Cookie, node : Leaf)
      if Map16.empty?(node.children)
        return EMPTY_NODE0
      end

      children1, changed = Map16.assoc(NodeMap16(Leaf).empty, 0u32, node)
      assert changed

      node0(cookie, node.summary, children1)
    end

    {% for level in 0..5 %}
      def wrap(cookie : Cookie, node : Node{{level}})
        if Map16.empty?(node.children)
          return EMPTY_NODE{{level + 1}}
        end

        children1, changed = Map16.assoc(NodeMap16(Node{{level}}).empty, 0u32, node)
        assert changed

        node{{level + 1}}(cookie, node.summary, children1)
      end
    {% end %}

    def wrap(cookie : Cookie, node : Node6)
      node
    end

    def wrap(key : LeafKey)
      NodeKey0.new(0u8, key)
    end

    {% for level in 0..5 %}
      def wrap(key : NodeKey{{level}})
        NodeKey{{level + 1}}.new(0u8, key)
      end
    {% end %}

    def wrap(key : NodeKey6)
      key
    end

    private def eqcast(cookie : Cookie, node, key, &)
      node_height = height(node)
      key_height = height(key)

      if node_height < key_height
        until node_height == key_height
          node = wrap(cookie, node)
          node_height += 1
        end
      elsif node_height > key_height
        until node_height == key_height
          key = wrap(key)
          key_height += 1
        end
      end

      yield node, key
    end

    private def assoc(cookie : Cookie, node : Leaf, key : LeafKey, value : Term) : {Leaf, Bool}
      mut = node.cookie.allows_mutation_by?(cookie)

      children0 = node.children
      children1, changed = Map16.assoc(children0, key.value, value, mut: mut)
      unless changed
        return node, false
      end

      # If we're inserting, we don't have to recalculate the summary. We
      # can just union the inserted child into the existing summary.
      if Map16.size(children0) < Map16.size(children1)
        summary = Summary.union(node.summary, Summary.of(value))

        return leaf(cookie, summary, children1, prototype: node), true
      end

      # Recalculate the summary from scratch.
      {leaf(cookie, children1, prototype: node), true}
    end

    {% for level in 0..6 %}
      private def assoc(cookie : Cookie, node : Node{{level}}, key : NodeKey{{level}}, value : Term) : {Node{{level}}, Bool}
        mut = node.cookie.allows_mutation_by?(cookie)

        unless child0 = Map16.at?(node.children, key.value)
          {% if level.zero? %}
            child0 = EMPTY_LEAF
          {% else %}
            child0 = EMPTY_NODE{{level - 1}}
          {% end %}
          child1, changed = assoc(cookie, child0, key.successor, value)
          assert changed

          # If we're inserting, we don't have to recalculate the summary. We
          # can just union the inserted child into the existing summary.
          summary1 = Summary.union(node.summary, child1.summary)

          children1, _ = Map16.assoc(node.children, key.value, child1, mut: mut)
          return node{{level}}(cookie, summary1, children1, prototype: node), true
        end

        # If we're updating, we have to recalculate the summary from scratch.
        # Use smart constructors for this.
        child1, changed = assoc(cookie, child0, key.successor, value)
        unless changed
          return node, false
        end

        children1, _ = Map16.assoc(node.children, key.value, child1, mut: mut)
        {node{{level}}(cookie, children1, prototype: node), true}
      end
    {% end %}

    private def assoc(cookie : Cookie, node : Leaf | Node, key : LeafKey | NodeKey, value : Term)
      eqcast(cookie, node, key) do |eq_node, eq_key|
        # At this point, eq node class = eq key class, and thus, since we guarantee
        # that equal class overloads are present, we will never hit infinite
        # recursion here.
        assoc(cookie, eq_node, eq_key, value)
      end
    end

    def assoc(root : Leaf | Node, key : UInt32, value : Term, *, cookie : Cookie = Cookie.none)
      root1, _ = assoc(cookie, root, decompose(key), value)
      root1
    end

    private def dissoc(cookie : Cookie, node : Leaf, key : LeafKey) : {Leaf, Bool}
      mut = node.cookie.allows_mutation_by?(cookie)

      children0 = node.children
      children1, removed = Map16.dissoc(children0, key.value, mut: mut)
      unless removed
        return node, false
      end

      # Recalculate the summary from scratch.
      {leaf(cookie, children1, prototype: node), true}
    end

    {% for level in 0..6 %}
      private def dissoc(cookie : Cookie, node : Node{{level}}, key : NodeKey{{level}}) : {Node{{level}}, Bool}
        unless child0 = Map16.at?(node.children, key.value)
          return node, false
        end

        child1, removed = dissoc(cookie, child0, key.successor)
        unless removed
          return node, false
        end

        mut = node.cookie.allows_mutation_by?(cookie)

        if summary(child1).size.zero?
          children1, removed = Map16.dissoc(node.children, key.value, mut: mut)
          assert removed

          return node{{level}}(cookie, children1, prototype: node), true
        end

        children1, _ = Map16.assoc(node.children, key.value, child1, mut: mut)

        {node{{level}}(cookie, children1, prototype: node), true}
      end
    {% end %}

    private def dissoc(cookie : Cookie, node : Leaf | Node, key : LeafKey | NodeKey) : {Leaf | Node, Bool}
      eqcast(cookie, node, key) do |eq_node, eq_key|
        # At this point, eq node class = eq key class, and thus, since we guarantee
        # that equal class overloads are present, we will never hit infinite
        # recursion here.
        dissoc(cookie, eq_node, eq_key)
      end
    end

    def dissoc(root : Leaf | Node, key : UInt32, *, cookie : Cookie = Cookie.none)
      root1, _ = dissoc(cookie, root, decompose(key))
      root1
    end

    def summary(node : Leaf | Node) : Summary
      # Since all nodes cache the summary we don't actually have to do anything here.
      node.summary
    end

    private def at?(node : Leaf, key : LeafKey) : Term?
      Map16.at?(node.children, key.value)
    end

    {% for level in 0..6 %}
      private def at?(node : Node{{level}}, key : NodeKey{{level}}) : Term?
        return unless child = Map16.at?(node.children, key.value)

        at?(child, key.successor)
      end
    {% end %}

    private def at?(node : Leaf | Node, key : LeafKey | NodeKey)
      node_height = height(node)
      key_height = height(key)
      return if key_height > node_height

      until node_height == key_height
        key = wrap(key)
        key_height += 1
      end

      # At this point, node height = key height, and thus, since we guarantee
      # that equal height overloads are present, we will never hit infinite
      # recursion here.
      at?(node, key)
    end

    def at?(node : Leaf | Node, key : UInt32) : Term?
      at?(node, decompose(key))
    end

    private def each(prefix : UInt32, node : Leaf, & : UInt32, Term ->) : Nil
      Map16.each_entry(node.children) do |key, value|
        yield (prefix << 4) | key, value
      end
    end

    {% for level in 0..6 %}
      private def each(prefix : UInt32, node : Node{{level}}, & : UInt32, Term ->)
        Map16.each_entry(node.children) do |key, child|
          each((prefix << 4) | key, child) do |full_key, value|
            yield full_key, value
          end
        end
      end
    {% end %}

    # Order: 0 to ∞.
    def each(node : Leaf | Node, & : UInt32, Term ->)
      each(0u32, node) { |key, value| yield key, value }
    end

    # We call *the sequence* a view of *node* that is empty, or starts
    # at 0 and ends at the mex (minimum excluded value) of the trie.
    #
    # In other words, *sequence size* is the mex of the trie.
    def seqsize(node : Leaf) : UInt32
      if Map16.full?(node.children)
        return node.summary.size
      end

      Map16.seqsize(node.children)
    end

    # :ditto:
    def seqsize(node : Node) : UInt32
      node.seqsize
    end

    private def seqsize(children : NodeMap16) : UInt32
      seqsize = 0u32

      Map16.each_in_seq(children) do |child|
        seqsize += seqsize(child)
      end

      seqsize
    end

    private def seqpart(cookie : Cookie, node : Leaf) : Leaf
      if Map16.seq_only?(node.children)
        return node
      end

      leaf(cookie, Map16.seqpart(node.children))
    end

    private def seqpart(cookie : Cookie, node : Node)
      if Map16.full?(node.children)
        return node
      end

      seqpart0(cookie, node)
    end

    # seqpart0 is the inner implementation of seqpart. It requires its
    # *node* argument to be non-full.

    private def seqpart0(cookie : Cookie, node : Leaf)
      seqpart(cookie, node)
    end

    {% for level in 0..6 %}
      private def seqpart0(cookie : Cookie, node : Node{{level}})
        size = 0u32
        tail = nil

        Map16.each_in_seq(node.children) do |child|
          if Map16.full?(child.children)
            size += 1
            next
          end

          tail = seqpart0(cookie, child)
        end

        if size.zero? && tail.nil?
          return EMPTY_NODE{{level}}
        end

        children1 = Map16.trim(node.children, size)

        # If tail is nil, this means all nodes in seq were full. We're
        # just at the boundary. E.g., seqsize 16, 32, 64 and so on.
        #
        # Although it isn't expected that the tail is empty, it could be.
        # So handle that similarly.
        if tail.nil? || Map16.empty?(tail.children)
          return node{{level}}(cookie, children1)
        end

        mut = node.cookie.allows_mutation_by?(cookie)

        # If tail contains something, we must append it to children.
        children1, changed = Map16.assoc(children1, size, tail, mut: mut)
        assert changed

        node{{level}}(cookie, children1)
      end
    {% end %}

    # NOTE: `seqpart` is stupid; it will faithfully path-copy (and worse!) even
    # if the entirety of *node* is one giant sequence. One remedy is to guard calls
    # to `seqpart` with `size == seqsize` checks (aka `seq_only?`); such checks are
    # basically two fetches and an integer compare. Most importantly, in practice, such
    # a check  will almost always be `true`.
    def seqpart(node : Leaf | Node, *, cookie : Cookie = Cookie.none)
      seqpart(cookie, node)
    end

    def seq_only?(node : Leaf | Node) : Bool
      seqsize(node) == summary(node).size
    end

    def equals?(a : Term, b : Term)
      a == b
    end

    {% for cls in %w[Leaf Node0 Node1 Node2 Node3 Node4 Node5 Node6] %}
      def equals?(a : {{cls.id}}, b : {{cls.id}}) : Bool
        return true if a.same?(b)
        return false unless a.summary == b.summary

        Map16.equals?(a.children, b.children) do |child0, child1|
          equals?(child0, child1)
        end
      end
    {% end %}

    def equals?(a, b) : Bool
      false
    end
  end
end
