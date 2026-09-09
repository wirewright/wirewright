module Ww::D7
  # An immutable map of node ids to replacement terms.
  #
  # Disjoint changes to the same node are supported and will be properly merged.
  alias Patch = Pf::Map(NodeId, Term)

  # Constructs a patch that replaces all nodes in *object* with *term*.
  def replace(object : Node, term : Term) : Patch
    Pf::Map.assoc(object.id, term)
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
      patch = patch.merge(contrib)
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

  private def compatible?(a : Tpath, b : Tpath) : Bool
    if a.size == b.size
      return a != b # if different, then they're compatible
    end

    sm, lg = a.size < b.size ? {a, b} : {b, a}
    !lg.starts_with?(sm) # E.g. Tpath[2] and Tpath[2—1] are incompatible.
  end

  private def compatible?(ref : Term, successor : Term, &predicate : Tpath -> Bool) : Bool
    compatible?(ref, successor, Tpath[], predicate)
  end

  private def compatible?(ref : Term, successor : Term, path, predicate) : Bool
    if ref == successor
      return true # compatible
    end

    unless (ref = ref.as_d?) && (successor = successor.as_d?)
      return predicate.call(path)
    end

    if ref.itemsize == successor.itemsize
      # Visit items recursively.
      successor.items.each_with_index do |item1, index|
        subpath = path.append(Tpath.value(index))
        item0 = ref[index]
        return false unless compatible?(item0, item1, subpath, predicate)
      end
    else
      # Itemsize change (adding or removing an item) is treated holistically:
      # only one participant is allowed to modify it. To achieve this we mark
      # all items as having been modified.
      target = {ref, successor}.max_by(&.itemsize)
      target.items.each_with_index do |target, index|
        subpath = path.append(Tpath.value(index))
        return false unless predicate.call(subpath)
      end
    end

    ref.each_entry(in: Term::Dict.pairspart) do |key, value0|
      subpath = path.append(Tpath.value(key))
      unless value1 = successor[key]?
        # Successor removed *key*.
        return false unless predicate.call(subpath)
        next
      end

      # Successor possibly modified *key*.
      unless compatible?(value0, value1, subpath, predicate)
        return false
      end
    end

    successor.each_entry(in: Term::Dict.pairspart) do |key, _|
      next if key.in?(ref)

      # Successor added *key*.
      subpath = path.append(Tpath.value(key))
      return false unless predicate.call(subpath)
    end

    true # compatible
  end

  private def compatible?(orig : Term, rep0 : Term, rep1 : Term) : Bool
    affected0 = Pf::Kit.stack_array(Tpath)

    _ = compatible?(orig, rep0) do |path|
      affected0 << path

      true # continue
    end

    compatible?(orig, rep1) do |path1|
      affected0.all? { |path0| compatible?(path0, path1) }
    end
  end

  # Accumulates changes made by *successor* into *acc*. Changes are found
  # by comparing *ref* and *successor*.
  #
  # WARNING: This method assumes implicitly that the changes of all *successors*
  # accumulated into *acc* are disjoint. If they conflict, this method will
  # break. You are expected to guard calls to this method with a disjointedness check.
  private def overlay(acc : Term, ref : Term, successor : Term) : Term
    unless (acc_dict = acc.as_d?) && (successor_dict = successor.as_d?)
      return successor
    end

    assert ref_dict = ref.as_d?

    result = acc_dict.transaction do |commit|
      ref_dict.each_entry do |key, ref_value|
        unless successor_value = successor_dict[key]?
          # Successor removed *key*.
          commit.without(key)
          next
        end

        # We'd like to keep acc's values unless the successor modifies
        # the entry.
        next if successor_value == ref_value

        # Successor modified *key*.
        commit.with(key, overlay(acc_dict[key], ref_value, successor_value))
      end

      successor.each_entry do |key, successor_value|
        next if key.in?(ref_dict)

        # Successor created *key*.
        commit.with(key, successor_value)
      end
    end

    Term.of(result)
  end

  def merge(hg : Hypergraph, proposals : Indexable(Patch)) : Patch
    if proposals.empty?
      return Patch.new
    end

    # A table from node id to replacement proposals for that node along
    # with proposal index (used for ranking).
    #
    # NOTE: Proposals are sorted by soln, and iteration is inorder, thus
    # the arrays here are sorted as well by proposal index, asc, and
    # therefore by soln, asc.
    patchtab = {} of NodeId => Array({Term, UInt32})

    probably_conflicts = false

    proposals.each_with_index do |proposal, proposal_index|
      proposal.each do |node_id, rep|
        reps = patchtab.put_if_absent(node_id) { [] of {Term, UInt32} }
        reps << {rep, proposal_index.to_u32}

        if reps.size > 1
          probably_conflicts = true
        end
      end
    end

    proposals_declined = Pf::USet32[]
    if probably_conflicts
      proposals_declined = decline_set(hg, patchtab)
    end

    Patch.transaction do |patch|
      patchtab.each do |node_id, reps|
        if entry = reps.single? # Fast path
          rep, proposal_index = entry
          next if proposal_index.in?(proposals_declined)

          patch.assoc(node_id, rep)
          next
        end

        ref = hg[node_id].term
        acc = ref

        reps.each do |rep, proposal_index|
          next if proposal_index.in?(proposals_declined)

          acc = overlay(acc, ref, rep)
        end

        patch.assoc(node_id, acc)
      end
    end
  end

  # Computes the proposal decline set for *patchtab*: declines proposals
  # that conflict.
  def decline_set(hg : Hypergraph, patchtab : Hash(NodeId, Array({Term, UInt32}))) : Pf::USet32
    Pf::USet32.transaction do |declined|
      patchtab.each do |node_id, reps|
        orig = hg[node_id].term

        reps.each_with_index do |(rep0, proposal_index0), i|
          next if proposal_index0.in?(declined)

          abstains = false

          reps.each_with_index do |(rep1, proposal_index1), j|
            next if i == j
            next if proposal_index1.in?(declined)

            # If our (rep0's) proposal index is smaller, then we are more preferred,
            # and thus we won't disable ourselves in case of conflict with rep1. This
            # means there is little point in checking for conflict in the first place.
            # rep1, who is less preferred, will do that instead.
            next if proposal_index0 < proposal_index1

            # We shouldn't have the same rule propose two versions for the same node.
            # This can't happen because all rules return a Patch, which is a hash table;
            # its keys cannot repeat.
            assert proposal_index0 != proposal_index1

            next if compatible?(orig, rep0, rep1)

            # We (rep0) are less preferred than rep1 and are also incompatible with
            # it. We are in conflict with rep1. We must abstain in favor of rep1
            # because we are less preferred.
            abstains = true
            break
          end

          next unless abstains

          declined << proposal_index0
        end
      end
    end
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
