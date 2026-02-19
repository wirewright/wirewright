# The guts of the `part` node.
#
# The `part` node is a bit like the backmap engine `M1::Backmap`, except it's
# much simpler (it only handles keypaths; no refs, no optionals, no conflict
# resolution; no concept of agents, etc.; nothing of that sort).
#
# It's also important to say that it's not the part node that is like the backmap
# engine, but the entire tree of part nodes rooted at a `cell`. The entire tree --
# all `part` nodes collectively -- form something resembling a backmap, allowing
# one to modify the root cell's value.
#
# There can obviously be overlap. `part`s may be unaware of each other, and that's
# the whole point! Think `part`s corresponding to different *parts* of a device's
# surface (imagine displays, knobs; except they're symbolic).
#
# We say there is a *conflict* if two keypaths corresponding to different `part`
# nodes propose different values.
#
# Conflict is resolved by ignoring changes from both conflicting parties. This is
# the strategy for `part`; the backmap engine uses a different strategy (backtracking
# search until a non-conflicting config is found, taking into account user-
# assigned priority).
#
# If the root cell itself was patched, its `part` node subtree refuses to touch it.
#
# `part` nodes are special in that they do not require intermediate cells for
# storing state. The literally modify the innards of a term after each tick. This
# leads to a very "baroque" implementation that decorates the main `pass` instead
# of being a neat rule in it, like e.g. `Feed`.
#
# `part` relies on trickery to convince the main pass that the `part`s are
# actually just `cell`s (extraction), and then after the main pass is complete,
# the implementation of `part` works hard to hide the fact there were any tricks
# at all, as it merges stuff back into the root cell (absorption).
module Ww::Rack::Part
  extend self

  defcase TreeNode,
    entries : Hash(Term, TreeNode)? = nil,
    proposals : Set(Term?)? = nil,
    mutation: true

  def follow(node : TreeNode, key : Term) : TreeNode
    entries = node.entries ||= {} of Term => TreeNode
    entries.put_if_absent(key) { TreeNode.new }
  end

  # NOTE: `nil` proposals signify removal. Think `(cell @x 100)` -> `(cell @x)`.
  def propose(node : TreeNode, proposal : Term?) : Nil
    proposals = node.proposals ||= Set(Term?).new
    proposals << proposal
  end

  # NOTE: `nil` signifies removal. Think `(cell @x 100)` -> `(cell @x)`;
  # if you focus on `100` you get *term* `100` -> `nil`.
  def merge(node : TreeNode, term : Term) : Term?
    # If neither entries nor proposals exist, then simply return term.
    if node.entries.nil? && node.proposals.nil?
      return term
    end

    # If proposals exist, accept them. If there is more than proposal for
    # the same spot, this is a conflict. If there is just one proposal
    # for this spot, we accept it.
    #
    # NOTE: We'd actually like to conflict if there are nested proposals
    # (in node.entries). However, this creates a nasty problem of "dependent"
    # proposals (namely removals). That is, say, we use a feed on the parent
    # and move it somewhere. This emits a `nil` proposal on the parent.
    # Simultaneously, we modify a child, say, increment it. Originally, we'd
    # have a conflict: the parent wants to be modified in its entirety, and
    # the child wants to be modified, too, assuming the parent remains intact.
    # We'd like to reject both to keep everything in order. However, this is
    # undesirable, since the parent's modification already affected the circuit!
    # Thus, the rest of the circuit assumes the parent was modified.
    #
    # What we do here is thus prefer modifications of the parent. This is done
    # under the assumption that modifying the parent "burns" info about the children.
    # Thus, no one would be able to see that we've actually NOT applied the changes
    # to children. To those children to which this recursively applies, we can say
    # something similar. The updated parent can look visually identical to the previous
    # one. However, we still assume information was lost. This seems to be the most
    # coherent solution yet.
    if proposals = node.proposals
      if proposals.size > 1
        return term
      end

      assert proposals.size == 1
      return proposals.first
    end

    assert entries = node.entries

    # If entries exist, then merge recursively each one of those entries,
    # assuming respective keys exist.
    assert dict0 = term.as_d?

    removals = Pf::Kit.stack_array(Int32)

    dict1 = dict0.transaction do |commit|
      dict0.each_item_with_index do |item, index|
        key = Term.of(index)
        next unless successor = entries[key]?

        unless result = merge(successor, item)
          removals << index
          next
        end

        commit.with(key, result)
      end

      dict0.each_entry(in: Term::Dict.pairspart) do |key, value|
        next unless successor = entries[key]?

        commit.with(key, merge(successor, value))
      end
    end

    removals.unstable_sort!
    removals.reverse_each do |index|
      dict1 = dict1.without_item(index)
    end

    Term.of(dict1)
  end

  # :nodoc:
  SYM_CELL = Term.of(:cell)

  # :nodoc:
  SYM_PART = Term.of(:part)

  def probably_exists_in?(hg : D7::Hypergraph) : Bool
    hg.has_head?(SYM_CELL) && hg.has_head?(SYM_PART)
  end

  def each_root_cell(hg : D7::Hypergraph, & : D7::Node, D7::AbsEdge, Term ->) : Nil
    buckets = {} of D7::AbsEdge => Array({D7::Node, Term})

    hg.each_node_with_head(SYM_CELL) do |node|
      Term.matchpi?(node.term, %{[cell @edge_ whole_]}) do
        abs_edge = hg.abs_edge(edge, wrt: node.id)

        bucket = buckets.put_if_absent(abs_edge) { [] of {D7::Node, Term} }
        bucket << {node, whole}
      end
    end

    buckets.each do |abs_edge, bucket|
      next unless bucket.size == 1

      node, whole = bucket.first
      yield node, abs_edge, whole
    end
  end

  defrecord PartStep,
    node : D7::Node,
    pattern : Term,
    capture : Term,
    from : D7::AbsEdge,
    to : D7::AbsEdge

  # Since we're mutating a lot, almost everything is context...
  defcase CompleteContext,
    hg : D7::Hypergraph,
    seen : Pf::USet32::Commit,
    preds : Pf::Kit::HybridArray(D7::AbsEdge, 8),
    path : Pf::Kit::HybridArray(PartStep, 8),
    sink : Pf::Kit::HybridArray(PartStep, 8) ->

  def complete(ctx : CompleteContext, root : D7::Node) : Nil
    return if root.id.in?(ctx.seen)

    ctx.seen << root.id

    pred = ctx.preds.last
    recursed = false

    ctx.hg.each_neighbor(of: root.id, on: pred) do |candidate|
      Term.case(candidate.term) do
        matchpi %{[part (@from_ to←(%'edge capture_)) pattern_]} do
          from_abs = ctx.hg.abs_edge(from, wrt: candidate.id)
          next unless from_abs == pred

          to_abs = ctx.hg.abs_edge(to, wrt: candidate.id)

          # Don't follow edges we've already followed.
          #
          # TODO: this seems necessary and makes some tests pass, but why, exactly?
          next if to_abs.in?(ctx.preds)

          step = PartStep.new(candidate, pattern, capture, from_abs, to_abs)
          recursed = true

          ctx.path << step
          ctx.preds << to_abs
          begin
            complete(ctx, candidate)
          ensure
            ctx.path.pop
            ctx.preds.pop
          end
        end

        otherwise { }
      end
    end

    return if recursed
    return if ctx.path.empty?

    ctx.sink.call(ctx.path)
  end

  # WARNING: *sink* is called with a stack-allocated array, which must not
  # be modified (it is owned by this function!) or retained in a way so that
  # it outlives the call to this function.
  def each_path_thru_parts(hg, root : D7::Node, edge : D7::AbsEdge, &sink : Pf::Kit::HybridArray(PartStep, 8) ->) : Nil
    path = Pf::Kit.stack_array(PartStep, 8)
    preds = Pf::Kit.stack_array(D7::AbsEdge, 8)
    preds << edge

    _ = Pf::USet32.transaction do |seen|
      ctx = stack_alloc CompleteContext.new(hg, seen, preds, path, sink)
      complete(ctx, root)
    end
  end

  def follow?(part : PartStep, matchee : Term) : {Term, M1::Log::SeqSlice}?
    return unless M1.probably_matches?(part.pattern, matchee)

    envlogs = M1.matches_and_logs(Term[], M1.operator(part.pattern), matchee)
    return unless envlog = envlogs.first?

    env, logs = envlog
    return unless value = env[part.capture]?
    return unless needle = logs.find { |name, _| name == part.capture }

    _, suffix = needle

    # Removes things like <examine itemspart>—<examine key 0> which lets
    # us support patterns like [x_].
    suffix_norm = M1::Log.normalize(suffix.seq)
    return if suffix_norm.is_a?(M1::Log::None)

    {value, M1::Log::SeqSlice.new(suffix_norm)}
  end

  defrecord Tree, root : D7::Node, node : TreeNode

  defrecord Endpoint, leaf_part : PartStep, value : Term

  # NOTE: We also keep track of leaves so that we don't have to descend
  # the tree to propose something.
  defrecord Forest,
    trees : Array(Tree),
    leaves : Hash(D7::NodeId, TreeNode),
    endpoints : Array(Endpoint)

  def forest(hg : D7::Hypergraph) : Forest
    trees = [] of Tree
    leaves = {} of D7::NodeId => TreeNode
    endpoints = [] of Endpoint

    each_root_cell(hg) do |node, src_abs, whole|
      root = nil

      each_path_thru_parts(hg, node, src_abs) do |path|
        trace = nil
        matchee = whole

        path.each do |part|
          break unless response = follow?(part, matchee)

          successor, actions = response

          # Sometimes, as in the following:
          #
          #   (part (@root @⏏a⏏) (%symbol blank ⏏a⏏_ _))
          #
          # ... we can get action sequences that we do not support. Here,
          # the sequence for `a` is an unsupported one -- we can't `part`
          # into a symbol name. We ignore parts with unsupported sequences
          # by pretending that they did not match.
          break unless actions.all?(M1::Log::ExamineValue)

          # Initialize root. We do it so late to avoid allocating roots
          # for parts that do not match.
          if root.nil?
            root = TreeNode.new
            trees << Tree.new(node, root.not_nil!)
          end

          # Initialize trace to root if not initialized.
          trace ||= root.not_nil!

          actions.each do |action|
            assert action.is_a?(M1::Log::ExamineValue)

            trace = follow(trace, action.key)
          end

          leaf_node = leaves.put_if_absent(part.node.id, trace)
          assert leaf_node.same?(trace)

          endpoints << Endpoint.new(part, successor)
          matchee = successor
        end
      end
    end

    Forest.new(trees, leaves, endpoints)
  end

  private def absorb(patch : D7::Patch, forest : Forest, &) : D7::Patch
    if patch.size < forest.leaves.size
      patch.select do |node_id, rep|
        unless endpoint = forest.leaves[node_id]?
          next true # keep
        end

        yield node_id, rep, endpoint

        false # remove
      end
    else
      forest.leaves.each do |node_id, endpoint|
        next unless rep = patch[node_id]?

        yield node_id, rep, endpoint

        patch = patch.dissoc(node_id)
      end

      patch
    end
  end

  def absorb(patch : D7::Patch, forest : Forest) : D7::Patch
    absorb(patch, forest) do |node_id, rep, endpoint|
      Term.case(rep) do
        # Propose update.
        matchpi %{[cell @_ value_]} { propose(endpoint, proposal: value) }
        # Propose removal.
        matchpi %{[cell @_]} { propose(endpoint, proposal: nil) }
        otherwise { }
      end
    end
  end

  def merge(patch : D7::Patch, forest : Forest) : D7::Patch
    forest.trees.each do |tree|
      next if tree.root.id.in?(patch)

      cell = tree.root.term
      Term.matchpi(cell, %{[cell @_ whole0_]}) do
        whole1 = Part.merge(tree.node, whole0)
        next if whole0 == whole1

        patch = patch.assoc(tree.root.id, Term.morph(cell, {2, whole1}))
      end
    end

    patch
  end

  def prepass(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
    unless Part.probably_exists_in?(hg)
      return fn.call(hg)
    end

    # Generate a "forest" containing trees whose root is some [cell ...], whose
    # nodes are `part`s, and whose leaves are "leaf parts", which are, together
    # with the value on their output edge, known as "endpoints".
    forest = Part.forest(hg)

    # Replace endpoint [part (@from @to) ...] with (cell @to <endpoint value>).
    forest.endpoints.each do |endpoint|
      leaf = endpoint.leaf_part

      hg.replace!(leaf.node.id, Term.of(:cell, leaf.to.term, endpoint.value))
      hg.leave!(leaf.node.id, leaf.from)
      hg.join!(leaf.node.id, leaf.to)
    end

    patch = fn.call(hg)

    pipe(patch, Part.absorb(forest), Part.merge(forest))
  end
end
