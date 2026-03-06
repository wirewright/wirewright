class Ww::Term::Dict
  # Public API:
  #
  # - `empty : R`
  # - `summary(root : R) : Summary`
  # - `at?(root : R, key : Term) : Term?``
  # - `nth?(root : R, n : UInt32) : {Term, Term}?``
  # - `each(root : R, & : Term, Term ->) : Nil`
  # - `assoc(root : R, key : Term, value : Term, *, cookie : Cookie = Cookie.none) : R`
  # - `dissoc(root : R, key : Term, *, cookie : Cookie = Cookie.none) : R`
  module TermTrie
    extend self

    alias Root = Node

    alias Unit = Node | Entry | Bucket
    alias UnitMap16 = SmallMap(Unit, UInt16)

    defrecord Key, hashcode : UInt64, term : Term

    defcase Node, summary : Summary, cookie : Cookie, children : UnitMap16, mutation: true
    defcase Entry, key : Key, value : Term, cookie : Cookie, mutation: true
    defcase Bucket, entries : Slice(Entry), mutation: true

    EMPTY_NODE = Node.new(summary: Summary.zero, cookie: Cookie.none, children: UnitMap16.empty)

    def empty : Node
      EMPTY_NODE
    end

    private def summarize(unit : Node) : Summary
      unit.summary
    end

    private def summarize(unit : Entry) : Summary
      Summary.of({term: unit.key.term, hashcode: unit.key.hashcode}, unit.value)
    end

    private def summarize(unit : Bucket) : Summary
      Summary.union(unit.entries) { |entry| summarize(entry) }
    end

    private def summarize(units : UnitMap16)
      Summary.union(units.ix) { |unit| summarize(unit) }
    end

    def summary(unit : Unit) : Summary
      summarize(unit)
    end

    def entry(cookie : Cookie, key : Key, value : Term) : Entry
      Entry.new(key, value, cookie)
    end

    def bucket(cookie : Cookie, entries : Slice(Entry)) : Bucket
      Bucket.new(entries)
    end

    def bucket(cookie : Cookie, *entries : {Key, Term}) : Bucket
      buffer = Pointer(Entry).malloc(entries.size)
      entries.each_with_index do |(key, value), index|
        buffer[index] = entry(cookie, key, value)
      end

      bucket(cookie, Slice.new(buffer, entries.size))
    end

    private def indexof(hashcode : UInt64, depth : UInt64) : UInt16
      ((hashcode >> (depth * 4)) & 0xfu64).to_u16
    end

    private def at?(unit : Node, key : Key, depth : UInt64) : Term?
      index = indexof(key.hashcode, depth)
      return unless child = unit.children.at?(index)

      at?(child, key, depth + 1)
    end

    private def at?(unit : Entry, key : Key, depth : UInt64) : Term?
      return unless unit.key == key

      unit.value
    end

    private def at?(unit : Bucket, key : Key, depth : UInt64) : Term?
      # We keep it O(N) since in practice, comparisons are much more expensive
      # for Terms than equality checks.
      unit.entries.each do |entry|
        next unless entry.key == key
        return entry.value
      end
    end

    def at?(unit : Unit, key : Term) : Term?
      at?(unit, key(key), depth: 0u64)
    end

    def nth?(root : Node, n : UInt32) : {Term, Term}?
      return unless n < root.summary.size

      root.children.ix.each do |child|
        case child
        in Node
          if entry = nth?(child, n)
            return entry
          end

          n -= child.summary.size
        in Entry
          if n.zero?
            return child.key.term, child.value
          end

          n -= 1
        in Bucket
          if entry = child.entries[n]?
            return entry.key.term, entry.value
          end

          n -= child.entries.size
        end
      end
    end

    def each(root : Node, & : Term, Term ->) : Nil
      stack = Pf::Kit.stack_array({Node, UInt32})
      stack << {root, 0u32}

      loop do
        break unless frame = stack.pop?

        node, n = frame
        next unless child = node.children.ix[n]?

        stack << {node, n + 1}

        case child
        in Node
          stack << {child, 0u32}
          next
        in Entry
          entries = {child}
        in Bucket
          entries = child.entries
        end

        entries.each do |entry|
          yield entry.key.term, entry.value
        end
      end
    end

    private def key(key : Term)
      Key.new(Term.hashcode(key), key)
    end

    defcase AssocContext, cookie : Cookie, key : Key, value : Term, delta : Summary do
      def delta
        if @delta.size.zero? # not initialized
          @delta = Summary.of({term: key.term, hashcode: key.hashcode}, value)
        end

        @delta
      end
    end

    record Inserted, unit : Unit
    record Updated, unit : Unit
    record Unchanged, unit : Unit
    record Removed, unit : Node

    private def assoc(ctx : AssocContext, unit : Node, depth : UInt64)
      mut = unit.cookie.allows_mutation_by?(ctx.cookie)
      index = indexof(ctx.key.hashcode, depth)

      # Insert.
      unless child0 = unit.children.at?(index)
        entry = entry(ctx.cookie, ctx.key, ctx.value)
        summary1 = Summary.union(unit.summary, ctx.delta)
        children1 = unit.children.ensure_assoc(index, entry, mut: mut)

        if mut
          unit.summary = summary1
          unit.children = children1
          return Inserted.new(unit)
        end

        node1 = Node.new(summary1, ctx.cookie, children1)
        return Inserted.new(node1)
      end

      # Update.
      response = assoc(ctx, child0, depth + 1)

      if response.is_a?(Unchanged)
        return response.copy_with(unit: unit)
      end

      child1 = response.unit
      children1, _ = unit.children.assoc(index, child1, mut: mut)

      if response.is_a?(Inserted)
        summary1 = Summary.union(unit.summary, ctx.delta)
      else
        summary1 = summarize(children1)
      end

      if mut
        unit.summary = summary1
        unit.children = children1
        return response.copy_with(unit: unit)
      end

      node1 = Node.new(summary1, ctx.cookie, children1)

      response.copy_with(unit: node1)
    end

    private def assoc(ctx : AssocContext, unit : Entry, depth : UInt64)
      if ctx.key.hashcode == unit.key.hashcode
        unless ctx.key.term == unit.key.term
          # Collision
          return Inserted.new(bucket(ctx.cookie, {unit.key, unit.value}, {ctx.key, ctx.value}))
        end

        if ctx.value == unit.value
          return Unchanged.new(unit)
        end

        if unit.cookie.allows_mutation_by?(ctx.cookie)
          unit.value = ctx.value

          return Updated.new(unit)
        end

        return Updated.new(entry(ctx.cookie, ctx.key, ctx.value))
      end

      subctx = stack_alloc AssocContext.new(ctx.cookie, unit.key, unit.value, delta: Summary.zero)

      response = assoc(subctx, EMPTY_NODE, depth)
      assoc(ctx, response.unit, depth)
    end

    private def assoc(ctx : AssocContext, unit : Bucket, depth : UInt64)
      pivot = unit.entries.bsearch_index do |entry|
        Term.compare(entry.key.term, ctx.key.term) >= 0
      end

      pass do
        next unless pivot

        entry = unit.entries[pivot]
        next unless entry.key == ctx.key

        if entry.value == ctx.value
          return Unchanged.new(unit)
        end

        # Update.
        copy = unit.entries.dup
        copy.unsafe_put(pivot, entry(ctx.cookie, entry.key, ctx.value))

        return Updated.new(bucket(ctx.cookie, copy))
      end

      # Insert.
      pivot ||= unit.entries.size

      copy = Slice.join({
        unit.entries.trim(pivot),
        Slice[entry(ctx.cookie, ctx.key, ctx.value)],
        unit.entries + pivot,
      })

      Inserted.new(bucket(ctx.cookie, copy))
    end

    def assoc(root : Node, key : Term, value : Term, *, cookie : Cookie = Cookie.none) : Node
      ctx = stack_alloc AssocContext.new(cookie, key(key), value, delta: Summary.zero)

      response = assoc(ctx, root, depth: 0u64)
      response.unit.as(Node)
    end

    def dissoc(cookie : Cookie, unit : Node, key : Key, depth : UInt64)
      index = indexof(key.hashcode, depth)
      unless child0 = unit.children.at?(index)
        return Unchanged.new(unit)
      end

      mut = unit.cookie.allows_mutation_by?(cookie)

      case child0
      in Node
        response = dissoc(cookie, child0, key, depth + 1)
        if response.is_a?(Unchanged)
          return Unchanged.new(unit)
        end

        child1 = response.unit
        assert child1.is_a?(Node)

        if child1.children.empty?
          child1 = nil
        end
      in Entry
        unless child0.key == key
          return Unchanged.new(unit)
        end

        child1 = nil
      in Bucket
        unless pos = child0.entries.index { |entry| entry.key == key }
          return Unchanged.new(unit)
        end

        entries = child0.entries[...pos] + child0.entries[pos + 1..]
        if entries.size == 1
          child1 = entries[0]
        else
          child1 = Bucket.new(entries)
        end
      end

      if child1
        children1, _ = unit.children.assoc(index, child1, mut: mut)
      else
        children1, _ = unit.children.dissoc(index, mut: mut)
      end

      summary1 = summarize(children1)

      if mut
        unit.summary = summary1
        unit.children = children1
        return Removed.new(unit)
      end

      Removed.new(Node.new(summary1, cookie, children1))
    end

    def dissoc(root : Node, key : Term, *, cookie : Cookie = Cookie.none) : Node
      response = dissoc(cookie, root, key(key), depth: 0u64)
      response.unit.as(Node)
    end
  end
end
