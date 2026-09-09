# The implementation of the `part` node. See also: `rack.part`.
module Ww::Rack::Part
  extend self

  # :nodoc:
  defcase PartNode,
    node : D7::Node,
    abs_whole : D7::AbsEdge,
    rel_part_edge : Term,
    abs_part_edge : D7::AbsEdge,
    pattern : M1::Op::Any

  # :nodoc:
  #
  # For example:
  #
  #   (cell @xs (1 (2) 3))       < part source
  #
  #   (part (@xs @a) (_ a_ _))   < IntermediateRWPart
  #   (part (@xs @xt) (_* `xt))  < WPart
  #
  #   (part (@a @val) (val_))    < IntermediateRWPart -> RWPart
  #   (part (@a @yt) (_* `yt))   < IntermediateRWPart -> WPart
  alias PartChain = IntermediateRWPart | RWPart | WPart

  # :nodoc:
  defcase IntermediateRWPart,
    part : PartNode,
    log : M1::Log::SealedOne,
    matchee : Term,
    successor : PartChain

  # :nodoc:
  defcase RWPart,
    part : PartNode,
    log : M1::Log::SealedOne,
    matchee : Term

  # :nodoc:
  defcase WPart,
    part : PartNode,
    log : M1::Log::SealedOne

  private def each(chain : PartChain, & : PartChain ->) : Nil
    while chain.is_a?(IntermediateRWPart)
      yield chain
      chain = chain.successor
    end

    yield chain
  end

  defrecord PartTree,
    source_node : D7::Node,
    source_value : Term,
    chains : Array(PartChain)

  def prepass(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
    # Fast path.
    unless graph = graph?(hg)
      return fn.call(hg)
    end

    rgraph = reverse_graph(graph)

    forest = {} of D7::AbsEdge => PartTree
    graph.each do |whole, children|
      next unless source_cell = Rack.cell?(hg, whole)
      next unless source_value = source_cell.value?

      chains = [] of PartChain
      each_chain(graph, rgraph, whole, source_value) do |chain|
        chains << chain
      end

      forest[whole] = PartTree.new(source_cell.node, source_value, chains)
    end

    replacements = {} of D7::NodeAddr => D7::Gnd
    replaced = Set(D7::NodeId).new

    forest.each do |_, tree|
      tree.chains.each do |chain|
        each(chain) do |element|
          part = element.part

          replacements.put_if_absent(part.node.addr) do
            replaced << part.node.id

            case element
            in WPart
              D7.gnd(Term.of(:cell, part.rel_part_edge), part.rel_part_edge)
            in RWPart, IntermediateRWPart
              D7.gnd(Term.of(:cell, part.rel_part_edge, element.matchee), part.rel_part_edge)
            end
          end
        end
      end
    end

    patch = fn.call(hg.gnd_map(replacements))
    updates = D7::Patch.new

    forest.each do |whole, tree|
      next if patch.has_key?(whole)

      proposals = {} of M1::Log::SealedOne => Array({Term, Term::Dict})
      changed = Pf::USet32.new

      tree.chains.each do |chain|
        # Go from root-most to leaf-most part. If a part changes, then all parts
        # below it must be ignored -- their changes, if any, were overwritten:
        #
        #   (cell @xs (1 (2) 3))
        #   (part (@xs @x) (_ x_ _))
        #   (part (@x @y) (y_))
        #   (discard @x)
        #   (backsys @y ±n <> {n: ^(+ n 1)})
        #
        # Here, no matter what happens to @y (which comes later in the chain), we should
        # just remove @x and move on.
        each(chain) do |element|
          part = element.part
          next unless replacement = patch[part.node.id]?

          unless element.is_a?(WPart)
            break if changed.includes?(part.node.id)
          end

          _, capture = part.rel_part_edge
          backspec = nil

          Term.case(replacement) do
            matchpi %{[cell @_ value_]} do
              if value.type.dict? || value.type.symbol?
                backspec = Term[].with(capture, {:"^verbatim", value})
              else
                # Do not waste time doing ^verbatim stuff on terms that cannot cause
                # us trouble: booleans, numbers, etc.
                backspec = Term[].with(capture, value)
              end
            end

            matchpi %{[cell @_ ]} do
              backspec = Term[].with({capture}, Term[])
            end

            otherwise { }
          end

          next if backspec.nil?

          bucket = proposals.put_if_absent(element.log) { [] of {Term, Term::Dict} }
          bucket << {capture, backspec}
          changed = changed.add(part.node.id)
          break
        end
      end

      staging = [] of {M1::Log::SealedOne, Term, Term::Dict}

      # Filter conflicting proposals or "horizontal conflicts" out.
      #
      #   (cell @xs (1 2 3))
      #   (part (@xs @x) (x_ _ _))
      #   (part (@xs @y) (x_ _ _))
      #   ;; modify @x
      #   ;; modify @y
      #   ;; => bucket contains two changes for the same spot!
      #   ;; => both eliminated
      proposals.each do |log, bucket|
        next unless row = bucket.single?

        capture, backspec = row
        staging << {log, capture, backspec}
      end

      # Here we do something very important.
      #
      # Backmaps will consider it a conflict when a nested mutation is stomped over
      # by an outer mutation from a different backmap agent. That is, e.g., if one
      # agent wants to mutate a list item, and another one wants to destroy or completely
      # rewrite the list, that's a conflict to the backmap engine. It will use the standard
      # conflict resolution procedure of kicking out agents one after another in
      # the order we write them in the backsystem. The closer an agent is to
      # the beginning of the backsystem, the earlier will it be kicked out.
      #
      # What `part`s should do is not blindly block things, but instead allow
      # specifically the *parent* mutation to succeed -- parts should allow
      # the parent `part` to overwrite a child `part`'s mutation:
      #
      #   (cell @xs (1 (2) 3))
      #   (part (@xs @x) (_ x_ _))
      #   (part (@x @y) (y_))
      #   ;; modify @x
      #   ;; modify @y
      #   ;; => @y modification should be stomped over by @x
      #
      # Whether this is a sane default is a different question; but the other simple
      # way to resolve something like this -- toggling what's from the user's point of
      # view a random part off -- is *definitely* worse. The third, and possibly correct
      # solution, is to kick out all conflicting parts like we do above for "horizontal"
      # conflicts. The problem with this is that the circuit *already run* under
      # the assumption that some values were removed etc. We have to preserve that
      # at least logically. Allowing parents to win is the best choice, I think.
      #
      # Not that any of this is commonly met in practice!
      staging.unstable_sort_by! do |log, capture, _|
        {-M1::Log.size(log.seq), capture}
      end

      backsys = [] of {M1::EnvLogList, Term::Dict}
      staging.each do |log, capture, backspec|
        log_list = Slice[{capture, log}]
        env_log_list = Slice[{Term[], log_list}]
        backsys << {env_log_list, backspec}
      end

      update = M1.backmapR(backsys, tree.source_value)
      if update.empty?
        # (cell @x (1 2 3))
        # (part (@x @y) y_)
        # (discard @y)
        updates = D7.patches(updates, D7.patch(tree.source_node, {2, nil}))
      else
        updates = D7.patches(updates, D7.patch(tree.source_node, {2, Term.collapse(update)}))
      end
    end

    # Remove `part` -> `cell` replacements from the resulting patch.
    patch = patch.transaction do |txn|
      if replaced.size <= patch.size
        replaced.each { |id| txn.dissoc(id) }
      else
        patch.each do |id, _|
          next unless id.in?(replaced)

          txn.dissoc(id)
        end
      end
    end

    D7.merge(hg, {patch, updates})
  end

  # Finds all `part` nodes and builds a graph of them.
  #
  # For example:
  #
  #   (part (@xs @x))
  #   (part (@xs @y))
  #   (part (@y @z))
  #   (part (@ys @x))
  #
  # ...produces:
  #
  #   @xs: @x @y
  #   @ys: @x
  #   @y: @z
  private def graph?(hg : D7::Hypergraph) : Hash(D7::AbsEdge, Array(PartNode))?
    graph = nil

    hg.each_node_with_head(Term.of(:part)) do |candidate|
      Term.matchpi?(candidate.term, %{[part (@whole_ @part_) pattern_]}) do
        abs_whole = hg.resolve(candidate.addr, whole)
        abs_part = hg.resolve(candidate.addr, part)
        node = PartNode.new(candidate, abs_whole, part, abs_part, M1.operator(pattern))

        graph ||= {} of D7::AbsEdge => Array(PartNode)
        children = graph.not_nil!.put_if_absent(abs_whole) { [] of PartNode }
        children << node
      end
    end

    graph
  end

  # Builds a graph associating *parts* with *wholes*. Effectively, this is the reverse
  # of the part `graph?` (which associates *wholes* with their *parts*).
  #
  # For example:
  #
  #   (part (@xs @x))
  #   (part (@xs @y))
  #   (part (@y @z))
  #   (part (@ys @x))
  #
  # ...produces:
  #
  #   @x: @xs @ys
  #   @y: @xs
  #   @z: @y
  private def reverse_graph(graph : Hash(D7::AbsEdge, Array(PartNode))) : Hash(D7::AbsEdge, Array(D7::AbsEdge))
    rgraph = {} of D7::AbsEdge => Array(D7::AbsEdge)

    graph.each do |whole, parts|
      parts.each do |part|
        wholes = rgraph.put_if_absent(part.abs_part_edge) { [] of D7::AbsEdge }
        wholes << whole
      end
    end

    rgraph
  end

  # (cell @xs (1 2 3))          < root
  #   (part (@xs @a) (a_ _ _))  }
  #   (part (@xs @b) (_ b_ _))  } children
  #   (part (@xs @c) (_ _ c_))  }
  private def each_chain(graph, rgraph, root : D7::AbsEdge, matchee : Term, &fn : PartChain ->) : Nil
    return unless children = graph[root]?

    children.each do |part|
      each_chain(graph, rgraph, part, matchee, M1::Log.root, fn)
    end
  end

  private def each_chain(graph, rgraph, part : PartNode, matchee : Term, prefix : M1::Log::SeqOne, fn) : Nil
    #                                          This part is invalid and should be ignored, along with
    #                                          all of its children (if any).
    #                                          ------------
    #   (cell @cell0 ...) - (part @cell0 @x) - (part @x @y) - ...
    #   (cell @cell1 ...) - (part @cell1 @x) /
    sources = rgraph[part.abs_whole]?
    return unless sources.nil? || sources.size == 1

    return unless env_log_lists = M1.matches_and_logs(Term[], part.pattern, matchee)
    return if env_log_lists.empty?

    # (part (@xs @⏏a⏏) (a_ _ _))
    _, capture = part.rel_part_edge

    continuations = continue(prefix, env_log_lists, capture)
    continuation0 = continuations.next
    return unless continuation0.is_a?(Continuation)
    continuation1 = continuations.next

    # If matches has size 1, the we can put it in a cell:
    #
    #   (cell @whole (1 2 3))
    #   (part (@whole @n) ⟨±n⟩)
    #
    # ... should turn into:
    #
    #   (cell @whole (1 2 3))
    #   (cell @n 1)
    #
    # However if there are many matches:
    #
    #   (cell @whole (1 2 3))
    #   (part (@whole @n) ⟨±n⟩°)
    #
    # ... it makes no sense to have multiple cells; nor can we have just one cell,
    # because it is not clear what to put there. Instead, we create an empty cell:
    #
    #   (cell @whole (1 2 3))
    #   (cell @n)
    #
    # ... and allow *replacements* to occur. So if I write `100` to @n:
    #
    #   (cell @whole (1 2 3))
    #   (cell @n 100)
    #
    # ... we would need to "unpack" this like so:
    #
    #   (cell @whole (100 100 100))
    #   (part (@whole @n) ⟨±n⟩°)
    #
    # Notice how this treatment (part_value : Nil) is similar to the treatment
    # of %slots:
    #
    #   (cell @whole (1 2 3))
    #   (part (@whole @a) (_* `a))
    if continuation1.is_a?(Continuation)
      fn.call(WPart.new(part, M1::Log.seal(continuation0.log)))
      fn.call(WPart.new(part, M1::Log.seal(continuation1.log)))
      continuations.each do |continuation|
        fn.call(WPart.new(part, M1::Log.seal(continuation.log)))
      end
      return
    end

    # There is only one continuation, *continuation0*. That is, between:
    #
    #   (cell @whole (1 2 3))
    #   (part (@whole @n) ⟨±n⟩°)
    #
    # ... and:
    #
    #   (cell @whole (1 2 3))
    #   (part (@whole @n) (n_ _ _))
    #
    # ... we are in the *latter*.

    log = continuation0.log
    sealed_log = M1::Log.seal(log)

    # Write-only part. Such parts cannot have children (even if they do
    # in the circuit):
    #
    #   (cell @whole ())
    #   (part (@whole @tail) (_* `tail))
    unless submatchee = continuation0.env[capture]?
      fn.call(WPart.new(part, sealed_log))
      return
    end

    # Readable and writable part.
    fn.call(RWPart.new(part, sealed_log, submatchee))

    return unless children = graph[part.abs_part_edge]?

    children.each do |child|
      sink = ->(chain : PartChain) do
        fn.call(IntermediateRWPart.new(part, sealed_log, submatchee, successor: chain))
      end

      each_chain(graph, rgraph, child, submatchee, log, sink)
    end
  end

  # :nodoc:
  defrecord Continuation, env : Term::Dict, log : M1::Log::SeqOne

  private def continue(prefix : M1::Log::SeqOne, env_log_list : M1::EnvLogList, capture : Term) : Iterator(Continuation)
    env_log_list.each.compact_map do |row|
      # Find the log corresponding to *capture*.
      env, log_list = row
      next unless row = log_list.find { |name, _| name == capture }

      _, suffix = row

      # The log for *capture* is a *suffix*. We need to append it to *prefix* to
      # obtain the full log.
      log = prefix
      suffix_seq = M1::Log::SeqSlice.new(suffix.seq)
      suffix_seq.each do |action|
        log = M1::Log.append(log, action)
      end

      # If the log reaches into something that can't be reached, it'd be None,
      # so we reject it. Think (%pipe size ±n). A reference to `n` would be None,
      # because it's "virtual" -- it doesn't make sense to independently change `n`.
      next if log.is_a?(M1::Log::None)

      Continuation.new(env, log)
    end
  end
end
