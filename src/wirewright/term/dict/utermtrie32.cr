class Ww::Term::Dict
  # UTermTrie32 is used in dicts to store all `UInt32` keys.
  #
  # The design of `UTermTrie32` is very similar in spirit to `Pf::USet32`'s.
  # Both were inspired by Rich Hickey's persistent vector; I'm not sure how
  # much I'm diverging from it here, though.
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
  # on very small dicts, and by using a finger tree in such small cases, we'd
  # probably be facing a lot of overhead. I can't say for sure though!
  #
  # UTermTrie32 is used in dicts to store all keys that fall into UInt32 domain.
  # This means that now, compared to the prior impl, we don't have to split anything
  # or move entries between the itemspart and the pairspart, *provided we have
  # an efficient way to separate entries whose key is before mex, and entries
  # whose key is after, in the trie*. Thanks to some bitmaps, and opposed to finger
  # trees, we indeed can do this very quickly with UTermTrie32. For instance, running
  # `seqpart` on tries with hundreds of thousands of entries completes in less
  # than a microsecond on my machine -- not that this tells much, of course...
  #
  # An important fast path when retrieving the itemspart of a dict is supported.
  # Namely, if the UTermTrie of the dict is a sequence, meaning there are no
  # entries past its mex, the entire trie is the itemspart and no work is needed.
  # The question -- about whether there are entries past the mex of the trie -- is
  # answered by the comparison `seqsize == size` (see `seq_only?`). With UTermTrie32,
  # it is effectively two fetches and a compare. That is, we cache enough to make the cost
  # of this comparison negligible, on assoc- and dissoc-side, and during comparison itself;
  # and thus, the fast path has a fast guard, which is nice. The guard fires almost
  # always: very rarely in practice do we have entries with keys past the mex.
  #
  # Public API:
  #
  # - `empty : R`
  # - `summary(root : R) : Summary`
  # - `at?(root : R, key : UInt32) : Term?``
  # - `nth?(root : R, n : UInt32) : {Term, Term}?``
  # - `each(root : R, & : UInt32, Term ->) : Nil`
  # - `guided_each(root : R, guide : Summary, & : UInt32, Term ->) : Nil`
  # - `seqsize(root : R) : UInt32`
  # - `assoc(root : R, key : UInt32, value : Term, *, cookie : Cookie = Cookie.none) : R`
  # - `dissoc(root : R, key : UInt32, *, cookie : Cookie = Cookie.none) : R`
  # - (absent) `gte(root : R, lo : UInt32, *, cookie : Cookie = Cookie.none) : R`
  # - (absent) `lt(root : R, hi : UInt32, *, cookie : Cookie = Cookie.none) : R`
  module UTermTrie32
    extend self

    alias ItemMap16 = SmallMap(Item, UInt16)

    # We want a hashcode that depends on order (kind of). The easiest way is to include
    # the key when summarizing stuff. So this structure stores a key along the term so
    # that summarize() calls know the global key when recalculating summaries.
    defrecord Item, key : UInt32, term : Term

    alias Root = Leaf | Node

    defcase Leaf, summary : Summary, cookie : Cookie, children : ItemMap16, mutation: true

    alias Node = Node0 | Node1 | Node2 | Node3 | Node4 | Node5 | Node6

    defcase Node0, summary : Summary, seqsize : UInt32, cookie : Cookie, children : SmallMap(Leaf, UInt16), mutation: true
    defcase Node1, summary : Summary, seqsize : UInt32, cookie : Cookie, children : SmallMap(Node0, UInt16), mutation: true
    defcase Node2, summary : Summary, seqsize : UInt32, cookie : Cookie, children : SmallMap(Node1, UInt16), mutation: true
    defcase Node3, summary : Summary, seqsize : UInt32, cookie : Cookie, children : SmallMap(Node2, UInt16), mutation: true
    defcase Node4, summary : Summary, seqsize : UInt32, cookie : Cookie, children : SmallMap(Node3, UInt16), mutation: true
    defcase Node5, summary : Summary, seqsize : UInt32, cookie : Cookie, children : SmallMap(Node4, UInt16), mutation: true
    defcase Node6, summary : Summary, seqsize : UInt32, cookie : Cookie, children : SmallMap(Node5, UInt16), mutation: true

    EMPTY_LEAF  = Leaf.new(Summary.zero, Cookie.none, ItemMap16.empty)
    EMPTY_NODE0 = Node0.new(Summary.zero, 0u32, Cookie.none, SmallMap(Leaf, UInt16).empty)
    EMPTY_NODE1 = Node1.new(Summary.zero, 0u32, Cookie.none, SmallMap(Node0, UInt16).empty)
    EMPTY_NODE2 = Node2.new(Summary.zero, 0u32, Cookie.none, SmallMap(Node1, UInt16).empty)
    EMPTY_NODE3 = Node3.new(Summary.zero, 0u32, Cookie.none, SmallMap(Node2, UInt16).empty)
    EMPTY_NODE4 = Node4.new(Summary.zero, 0u32, Cookie.none, SmallMap(Node3, UInt16).empty)
    EMPTY_NODE5 = Node5.new(Summary.zero, 0u32, Cookie.none, SmallMap(Node4, UInt16).empty)
    EMPTY_NODE6 = Node6.new(Summary.zero, 0u32, Cookie.none, SmallMap(Node5, UInt16).empty)

    def capacity(node : Leaf | Node | Leaf.class | Node.class)
      case node
      in Leaf, Leaf.class   then 16u32**1
      in Node0, Node0.class then 16u32**2
      in Node1, Node1.class then 16u32**3
      in Node2, Node2.class then 16u32**4
      in Node3, Node3.class then 16u32**5
      in Node4, Node4.class then 16u32**6
      in Node5, Node5.class then 16u32**7
      in Node6, Node6.class then 16u32**8
      end
    end

    private def summarize(children : ItemMap16) : Summary
      Summary.union(children.ix) { |item| Summary.of(item.key, item.term) }
    end

    private def summarize(children : SmallMap) : Summary
      Summary.union(children.ix, &.summary)
    end

    def empty
      EMPTY_LEAF
    end

    def leaf(cookie : Cookie, summary, children)
      Leaf.new(summary, cookie, children)
    end

    def leaf(cookie : Cookie, summary, children, *, prototype : Leaf)
      if prototype.cookie.allows_mutation_by?(cookie)
        prototype.summary = summary
        prototype.children = children
        return prototype
      end

      Leaf.new(summary, cookie, children)
    end

    {% for level in 0..6 %}
      def node{{level}}(cookie : Cookie, summary, children)
        if summary.size == capacity(Node{{level}})
          seqsize = summary.size
        else
          seqsize = seqsize(children)
        end

        Node{{level}}.new(summary, seqsize, cookie, children)
      end

      def node{{level}}(cookie : Cookie, summary, children, *, prototype : Node{{level}})
        if summary.size == capacity(Node{{level}})
          seqsize = summary.size
        else
          seqsize = seqsize(children)
        end

        if prototype.cookie.allows_mutation_by?(cookie)
          prototype.seqsize = seqsize
          prototype.summary = summary
          prototype.children = children
          return prototype
        end

        Node{{level}}.new(summary, seqsize, cookie, children)
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
    # to be owned by *cookie*. This way, if we mutate them using *cookie* later,
    # we'll mutate them right away, without having to copy.

    def wrap(cookie : Cookie, node : Leaf)
      if node.children.empty?
        return EMPTY_NODE0
      end

      children0 = SmallMap(Leaf, UInt16).empty
      children1, _ = children0.assoc(0u32, node, mut: false)

      node0(cookie, node.summary, children1)
    end

    {% for level in 0..5 %}
      def wrap(cookie : Cookie, node : Node{{level}})
        if node.children.empty?
          return EMPTY_NODE{{level + 1}}
        end

        children0 = SmallMap(Node{{level}}, UInt16).empty
        children1, _ = children0.assoc(0u32, node, mut: false)

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

    def unwrap(node : Leaf | Node)
      loop do
        return node unless node.is_a?(Node)
        return node unless node.children.size == 1

        node.children.each_entry do |key, child|
          unless key.zero?
            return node
          end

          node = child
          break
        end
      end
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

    private def assoc(cookie : Cookie, node : Leaf, key : LeafKey, item item1 : Item) : {Leaf, Bool}
      mut = node.cookie.allows_mutation_by?(cookie)

      size0 = node.children.size

      children1, changed = node.children.assoc(key.value.to_u16, item1, mut: mut)
      unless changed
        return node, false
      end

      # When we're inserting, we don't have to recalculate the summary. We
      # can just union the inserted child into the existing summary.
      if size0 < children1.size
        summary = Summary.union(node.summary, Summary.of(item1.key, item1.term))

        return leaf(cookie, summary, children1, prototype: node), true
      end

      # Recalculate.
      {leaf(cookie, summarize(children1), children1, prototype: node), true}
    end

    {% for level in 0..6 %}
      private def assoc(cookie : Cookie, node : Node{{level}}, key : NodeKey{{level}}, item : Item) : {Node{{level}}, Bool}
        mut = node.cookie.allows_mutation_by?(cookie)

        unless child0 = node.children.at?(key.value.to_u16)
          {% if level.zero? %}
            child0 = EMPTY_LEAF
          {% else %}
            child0 = EMPTY_NODE{{level - 1}}
          {% end %}

          child1, changed = assoc(cookie, child0, key.successor, item)
          assert changed

          # If we're inserting, we don't have to recalculate the summary. We
          # can just union the inserted child into the existing summary.
          summary1 = Summary.union(node.summary, child1.summary)
          children1, _ = node.children.assoc(key.value.to_u16, child1, mut: mut)

          return node{{level}}(cookie, summary1, children1, prototype: node), true
        end

        child0_summary = child0.summary

        # If we're updating, we have to recalculate the summary from scratch.
        # Use smart constructors for this.
        child1, changed = assoc(cookie, child0, key.successor, item)
        unless changed
          return node, false
        end

        child1_summary = child1.summary

        children1, _ = node.children.assoc(key.value.to_u16, child1, mut: mut)

        if Summary.compatible?(child0_summary, child1_summary)
          summary = Summary.update(node.summary, child0_summary, child1_summary)

          return node{{level}}(cookie, summary, children1, prototype: node), true
        end

        {node{{level}}(cookie, summarize(children1), children1, prototype: node), true}
      end
    {% end %}

    private def assoc(cookie : Cookie, node : Leaf | Node, key : LeafKey | NodeKey, item : Item)
      eqcast(cookie, node, key) do |eq_node, eq_key|
        # At this point, eq node class = eq key class, and thus, since we guarantee
        # that equal class overloads are present, we will never hit infinite
        # recursion here.
        assoc(cookie, eq_node, eq_key, item)
      end
    end

    def assoc(root : Leaf | Node, key : UInt32, value : Term, *, cookie : Cookie = Cookie.none)
      root1, _ = assoc(cookie, root, decompose(key), Item.new(key, value))
      root1
    end

    private def dissoc(cookie : Cookie, node : Leaf, key : LeafKey) : {Leaf, Bool}
      mut = node.cookie.allows_mutation_by?(cookie)

      children1, removed = node.children.dissoc(key.value.to_u16, mut: mut)
      unless removed
        return node, false
      end

      # Recalculate the summary from scratch.
      {leaf(cookie, summarize(children1), children1, prototype: node), true}
    end

    {% for level in 0..6 %}
      private def dissoc(cookie : Cookie, node : Node{{level}}, key : NodeKey{{level}}) : {Node{{level}}, Bool}
        unless child0 = node.children.at?(key.value.to_u16)
          return node, false
        end

        child1, removed = dissoc(cookie, child0, key.successor)
        unless removed
          return node, false
        end

        mut = node.cookie.allows_mutation_by?(cookie)

        if summary(child1).size.zero?
          children1, _ = node.children.dissoc(key.value.to_u16, mut: mut)

          return node{{level}}(cookie, summarize(children1), children1, prototype: node), true
        end

        children1, _ = node.children.assoc(key.value.to_u16, child1, mut: mut)

        {node{{level}}(cookie, summarize(children1), children1, prototype: node), true}
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
      return unless item = node.children.at?(key.value.to_u16)

      item.term
    end

    {% for level in 0..6 %}
      private def at?(node : Node{{level}}, key : NodeKey{{level}}) : Term?
        return unless child = node.children.at?(key.value.to_u16)

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

    def nth?(node : Leaf, n : UInt32) : {UInt32, Term}?
      return unless item = node.children.ix[n]?

      {item.key, item.term}
    end

    def nth?(node : Node, n : UInt32) : {UInt32, Term}?
      node.children.each_entry do |_, child|
        size = child.summary.size
        if n < size
          return nth?(child, n)
        end

        n -= size
      end
    end

    private def each(prefix : UInt32, node : Leaf, & : UInt32, Term ->) : Nil
      node.children.each_entry do |key, item|
        yield (prefix << 4) | key, item.term
      end
    end

    {% for level in 0..6 %}
      private def each(prefix : UInt32, node : Node{{level}}, & : UInt32, Term ->)
        node.children.each_entry do |key, child|
          each((prefix << 4) | key, child) do |full_key, value|
            yield full_key, value
          end
        end
      end
    {% end %}

    # Order: ascending by key.
    def each(node : Leaf | Node, & : UInt32, Term ->)
      each(0u32, node) { |key, value| yield key, value }
    end

    private def guided_each(prefix : UInt32, node : Leaf, guideptr : Summary*, & : UInt32, Term ->) : Nil
      return unless guideptr.value.subset_of?(node.summary)

      node.children.each_entry do |key, item|
        yield (prefix << 4) | key, item.term
      end
    end

    {% for level in 0..6 %}
      private def guided_each(prefix : UInt32, node : Node{{level}}, guideptr : Summary*, & : UInt32, Term ->)
        return unless guideptr.value.subset_of?(node.summary)

        node.children.each_entry do |key, child|
          guided_each((prefix << 4) | key, child, guideptr) do |full_key, value|
            yield full_key, value
          end
        end
      end
    {% end %}

    # Order: ascending by key.
    def guided_each(node : Leaf | Node, guide : Summary, & : UInt32, Term ->)
      guideptr = pointerof(guide)

      guided_each(0u32, node, guideptr) { |key, value| yield key, value }
    end

    # We call *the sequence* a view of *node* that is empty, or starts
    # at 0 and ends at the mex (minimum excluded value) of the trie.
    #
    # In other words, *sequence size* is the mex of the trie.
    def seqsize(node : Leaf) : UInt32
      node.children.mex.to_u32
    end

    # :ditto:
    def seqsize(node : Node) : UInt32
      node.seqsize
    end

    private def seqsize(children : SmallMap) : UInt32
      seqsize = 0u32

      children.mex.times do |index|
        child = children.ix.unsafe_fetch(index)
        seqsize += seqsize(child)
        break unless summary(child).size == capacity(child) # full
      end

      seqsize
    end

    # *to* is exclusive.
    #
    # NOTE: This function does not modify *node*. The only reason you may
    # want to pass *cookie* is for the nodes created by this function to
    # have *cookie*; this way, you can avoid copies on subsequent modifications.
    def view(node : Leaf | Node, from : UInt32, to : UInt32, *, cookie : Cookie = Cookie.none) : Leaf | Node
      unless from <= to <= capacity(node)
        return UTermTrie32.empty
      end

      unwrap(view0(cookie, node, from, to))
    end

    # The invariant for `view0` is that both *from* and *to* are in bounds
    # of the given *node*.
    private def view0(cookie : Cookie, node : Leaf, from, to)
      children = node.children.view(from.to_u16, to.to_u16)

      leaf(cookie, summarize(children), children)
    end

    {% for level in 0..6 %}
      private def view0(cookie : Cookie, node : Node{{level}}, from, to)
        {% if level == 0 %}
          map = SmallMap(Leaf, UInt16).empty
        {% else %}
          map = SmallMap(Node{{level - 1}}, UInt16).empty
        {% end %}

        state = :before_first

        node.children.each_entry do |key, child|
          capacity = capacity(child)

          case state
          when :before_first
            if from > capacity
              from -= capacity
              to -= capacity
              next
            end

            if to <= capacity
              subview = view0(cookie, child, from, to)
              unless subview.children.empty?
                map, _ = map.assoc(key.to_u16, subview, mut: true)
              end
              break
            end

            to -= capacity

            subview = view0(cookie, child, from, capacity)
            unless subview.children.empty?
              map, _ = map.assoc(key.to_u16, subview, mut: true)
            end

            state = :after_first
          when :after_first
            if to > capacity
              to -= capacity
              map, _ = map.assoc(key.to_u16, child, mut: true)
              next
            end

            # Last
            subview = view0(cookie, child, 0, to)
            unless subview.children.empty?
              map, _ = map.assoc(key.to_u16, subview, mut: true)
            end
            break
          end
        end

        node{{level}}(cookie, summarize(map), map)
      end
    {% end %}
  end
end
