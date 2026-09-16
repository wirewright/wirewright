module Ww::D7
  # An immutable map of node ids to replacement terms.
  struct Patch
    include Enumerable({NodeId, Term})

    # :nodoc:
    def initialize(@keys : Pf::USet32, @valuesptr : Term*)
    end

    # :nodoc:
    def self.new(keys : Pf::USet32, values : Slice(Term)) : Patch
      assert keys.size == values.size

      new(keys, values.to_unsafe)
    end

    # Constructs an empty patch.
    def self.new : Patch
      new(keys: Pf::USet32.new, valuesptr: Pointer(Term).null)
    end

    # Constructs a patch replacing one *id* with *rep*.
    def self.assoc(id : NodeId, rep : Term) : Patch
      new.assoc(id, rep)
    end

    # Returns `true` if *a* and *b* are *definitely* compatible. Returns `false` if
    # they are *probably* incompatible.
    def self.compatible?(a : Patch, b : Patch) : Bool
      !a.@keys.intersects?(b.@keys)
    end

    # Returns `true` if all of *patches* are *definitely* compatible, and thus can
    # be safely merged. Returns `false` if they are *probably* incompatible.
    def self.compatible?(patches : Indexable(Patch)) : Bool
      patches.each_with_index do |patch0, i|
        patches.each_with_index do |patch1, j|
          next if i == j
          next if compatible?(patch0, patch1)
          return false # probably incompatible
        end
      end

      true # compatible
    end

    protected def values : Slice(Term)
      @valuesptr.to_slice(@keys.size)
    end

    # Returns `true` if this patch includes a replacement for a node with the given *id*.
    def includes?(id : NodeId) : Bool
      @keys.includes?(id)
    end

    def size : Int32
      @keys.size
    end

    # Returns the replacement term associated with the given *id*.
    def []?(id : NodeId) : Term?
      return unless id.in?(@keys)

      @valuesptr[@keys.rank(id)]
    end

    # Yields each node id and replacement term in this patch.
    def each(& : {NodeId, Term} ->) : Nil
      @keys.each_with_index do |id, index|
        yield({id, @valuesptr[index]})
      end
    end

    # Adds or updates this patch with a new replacement term *rep* for *id*.
    # Returns the resulting patch.
    def assoc(id : NodeId, rep : Term) : Patch
      index = @keys.rank(id)

      if id.in?(@keys) # update at index
        return Patch.new(@keys, Slice.join({values.trim(index)}, {rep}, {values + index + 1}))
      end

      # insert at index
      Patch.new(@keys.add(id), Slice.join({values.trim(index)}, {rep}, {values + index}))
    end

    # Removes the replacement for *id* from this patch, if any. Returns
    # the resulting patch.
    def dissoc(id : NodeId) : Patch
      unless id.in?(@keys)
        return self
      end

      index = @keys.rank(id)
      assert index < @keys.size

      Patch.new(@keys.delete(id), Slice.join({values.trim(index)}, Tuple.new, {values + index + 1}))
    end

    def pretty_print(pp)
      pp.list("D7::Patch[", self, "]") do |id, rep|
        pp.group do
          id.pretty_print(pp)
          pp.text " =>"
          pp.nest do
            pp.breakable
            rep.pretty_print(pp)
          end
        end
      end
    end

    def_equals_and_hash @keys, values
  end

  # Constructs a patch that replaces all nodes in *object* with *term*.
  def replace(object : Node, term : Term) : Patch
    Patch.assoc(object.id, term)
  end

  # Constructs a patch that morphs node terms in *object* according
  # to *morphseq*.
  #
  # See also `Term.morph`.
  def patch(object : Node, *morphseq) : Patch
    Patch.assoc(object.id, Term.morph(object.term, *morphseq))
  end

  # Constructs patches for each object in *objects* using the block;
  # merges the resulting patches into one final patch.
  #
  # Conflicting changes from later patches (*objects*-wise, with greater index)
  # win over former ones.
  def patches(objects : Enumerable(T), & : T, Int32 -> Patch?) : Patch forall T
    patch = Patch.new

    objects.each_with_index do |object, index|
      contrib = yield object, index
      next if contrib.nil?

      contrib.each do |key, value|
        patch = patch.assoc(key, value)
      end
    end

    patch
  end

  def patches(objects : Enumerable(Node), *morphseq) : Patch
    patches(objects) { |object| patch(object, *morphseq) }
  end

  # Shorthand for `patches` when all objects are patches already.
  def patches(objects : Enumerable(Patch)) : Patch
    patches(objects, &.itself)
  end

  # Shorthand that lets you list & merge multiple patches from the arguments.
  def patches(*objects : Patch) : Patch
    patches(objects)
  end

  # :nodoc:
  defrecord MergeNode,
    reference : Term,
    policy : MergePolicy,
    successors : Array(MergeNodeSuccessor)

  # :nodoc:
  defrecord MergeNodeSuccessor,
    term : Term,
    proposal_index : UInt32

  alias MergePolicy = MergeDiff | MergeSeq

  defrecord MergeDiff, depth_limit : UInt32
  defrecord MergeSeq

  # :nodoc:
  #
  # Implementation of the merge algorithm used by D7.
  #
  # Returns the merged Patch and a boolean indicating whether one or more *proposals*
  # were rejected.
  def merge(proposals : Indexable(Patch), & : NodeId -> {Term, MergePolicy}) : {Patch, Bool}
    # Fast path for when there are no proposals whatsoever.
    if proposals.empty?
      return Patch.new, false # nothing rejected
    end

    # Fast path for when there is just one proposal.
    if patch = proposals.single?
      return patch, false # nothing rejected
    end

    if Patch.compatible?(proposals)
      patch = Patch.new

      proposals.each do |proposal|
        proposal.each do |node_id, rep|
          patch = patch.assoc(node_id, rep)
        end
      end

      return patch, false # nothing rejected
    end

    nodetab = {} of NodeId => MergeNode

    proposals.each_with_index do |proposal, proposal_index|
      proposal.each do |node_id, rep|
        node = nodetab.put_if_absent(node_id) do
          node_reference, node_policy = yield node_id
          node_successors = [] of MergeNodeSuccessor
          MergeNode.new(node_reference, node_policy, node_successors)
        end

        # Filter out proposals that are exactly the same as the original node. Sloppy
        # callers can give us those, and much of this code [logically] relies on
        # the fact the proposals are actually different.
        next if node.reference == rep

        node.successors << MergeNodeSuccessor.new(rep, proposal_index.to_u32)
      end
    end

    proposals_rejected = Pf::USet32.new

    nodetab.each do |node_id, merge_node|
      next unless merge_node.policy.is_a?(MergeSeq)
      next unless merge_node.successors.size >= 2

      winner = merge_node.successors.min_by(&.term) # lexicographical minimum

      # Reject all proposals but the winning one.
      merge_node.successors.each do |successor|
        next if successor == winner

        proposals_rejected = proposals_rejected.add(successor.proposal_index)
      end
    end

    # Do a round of cleaning rejected proposals.
    if proposals_rejected.present?
      nodetab.each do |_, merge_node|
        merge_node.successors.reject!(&.proposal_index.in?(proposals_rejected))
      end
    end

    difftab = {} of NodeId => Array({Array(Term::Diff::Action), UInt32})

    nodetab.each do |node_id, merge_node|
      next unless policy = merge_node.policy.as?(MergeDiff)
      next unless merge_node.successors.size >= 2

      diffs = [] of {Array(Term::Diff::Action), UInt32}

      merge_node.successors.each do |successor|
        next if successor.proposal_index.in?(proposals_rejected) # Rejected by ourselves on past iterations.

        unless actions = Term.diff?(merge_node.reference, successor.term, depth_limit: policy.depth_limit)
          proposals_rejected = proposals_rejected.add(successor.proposal_index)
          next
        end

        compatible = true

        diffs.each do |(accepted_actions, accepted_proposal_index)|
          next if Term::Diff.compatible?(accepted_actions, actions)

          # Reject both of them if they conflict. Go on to reject more if we
          # conflict with more. No two conflicting proposals must make it into
          # the difftab.
          proposals_rejected = proposals_rejected.add(accepted_proposal_index).add(successor.proposal_index)
          compatible = false
        end

        next unless compatible

        diffs << {actions, successor.proposal_index}
      end

      difftab[node_id] = diffs
    end

    # Do a round of cleanup to get rid of rejected proposals in nodetab and difftab.
    if proposals_rejected.present?
      nodetab.each do |_, merge_node|
        merge_node.successors.reject!(&.proposal_index.in?(proposals_rejected))
      end

      difftab.each do |_, diffs|
        diffs.reject! do |_, proposal_index|
          proposal_index.in?(proposals_rejected)
        end
      end
    end

    patch = Patch.new

    # Computing the rank of a proposal is rather expensive, so we cache it.
    proposal_rank_cache = {} of UInt32 => Slice(Term)

    nodetab.each do |node_id, merge_node|
      # Changes to this node were all eliminated.
      next if merge_node.successors.empty?

      # We managed to narrow down on one successor without applying the diff.
      if successor = merge_node.successors.single?
        patch = patch.assoc(node_id, successor.term)
        next
      end

      # NOTE: Assume multi-successor nodes are *all* handled by difftab.
      diffs = difftab[node_id]

      # Sort by proposal rank for deterministic insertion.
      diffs.sort_by! do |_, proposal_index|
        proposal_rank_cache.put_if_absent(proposal_index) do
          proposal_rank(proposals[proposal_index])
        end
      end

      mutation = nil
      diffs.each do |actions, proposal_index|
        # Actions are flattened in the same way the diff algorithm tells us to
        # execute them. The greater order is that of *proposal ranks*, though.
        actions.each do |action|
          mutation ||= Term::Diff::Mutation.new
          mutation << action
        end
      end

      next unless mutation

      patch = patch.assoc(node_id, Term.of(mutation.apply(merge_node.reference.as_d))) # ?!
    end

    {patch, proposals_rejected.present?}
  end

  private def proposal_rank(proposal : Patch) : Slice(Term)
    rank = Pf::Kit.stack_array(Term, 8)
    proposal.each { |(_, rep)| rank << rep }
    rank.sort! # lexicographically
    rank.to_unsafe_readonly_slice!
  end

  def merge(hg : Hypergraph, proposals : Indexable(Patch)) : Patch
    patch, _ = merge(proposals) do |node_id|
      node = hg[node_id]
      {node.term, node.merge_policy}
    end
    patch
  end

  # Performs *frame fusion*.
  #
  # *Frame fusion* is a fancy way of saying "If the next frame has all
  # changes of the current one, then we don't need to show the current one
  # to the user; they'll see the changes in the next frame anyway". In other
  # words, if the next frame *subsumes* the current one, the current one is skipped.
  #
  # Calls *fn* with frames to show to the user.
  #
  # This method may yield duplicate consecutive frames, and it is the caller's
  # responsibility to filter them out.
  #
  # *pred* is the last frame seen by the user. Usually this would be the last
  # frame produced by this method. Otherwise it would be the very first circuit,
  # which the caller itself should show to the user as the first frame.
  def fuse(parser : Parser, pred : Term, frames : Indexable(Term), &fn : Term ->) : Nil
    if frames.empty?
      fn.call(pred)
      return
    end

    if frame = frames.single?
      fn.call(frame)
      return
    end

    assert frames.size >= 2

    index = 0
    frame = frames[index]

    behind = fuse_map(parser, pred)
    current = fuse_map(parser, frame)

    loop do
      unless succ = frames[index + 1]?
        fn.call(frame)
        break
      end

      ahead = fuse_map(parser, succ)
      behind_vs_current = fuse_changeset(behind, current)
      behind_vs_ahead = fuse_changeset(behind, ahead)

      begin
        # Skip current frame if all its changes are also present in the next frame.
        next if behind_vs_current.subset_of?(behind_vs_ahead)

        fn.call(frame)
      ensure
        frame = succ
        index += 1

        behind = current
        current = ahead
      end
    end
  end

  # :nodoc:
  alias FuseChange = FuseUpdated | FuseRemoved

  # :nodoc:
  defrecord FuseUpdated, addr : NodeAddr, node : Term
  # :nodoc:
  defrecord FuseRemoved, addr : NodeAddr

  # TODO: If we use ParseTrees instead of Hash(NodeAddr, Term)s, we'll be able
  # to skip a lot of work!
  private def fuse_changeset(pred : Hash(NodeAddr, Term), succ : Hash(NodeAddr, Term)) : Set(FuseChange)
    changes = Set(FuseChange).new

    pred.each do |addr, node|
      next if succ.has_key?(addr)

      changes << FuseRemoved.new(addr)
    end

    succ.each do |addr, node|
      ancestor_node = pred[addr]?
      next if ancestor_node == node

      changes << FuseUpdated.new(addr, node)
    end

    changes
  end

  private def fuse_map(parser : Parser, circuit : Term)
    nodes = {} of NodeAddr => Term

    feature_tree = parser.parse(circuit)
    D7.each_flat_feature_with_addr(feature_tree) do |feature, addr|
      nodes[addr] = feature.node
    end

    nodes
  end
end
