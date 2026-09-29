module Ww::Rack::Changes
  extend self

  # :nodoc:
  SYM_CHANGES = Term.of(:changes)

  def prepass(hg : D7::Hypergraph, &fn : D7::Hypergraph ->) : Nil
    # The changes prepass only makes sense if we (and *fn*) are allowed to modify
    # the hypergraph. If we are not then no changes are possible, so there are
    # no changes we can detect, so there is nothing to do for us overall.
    fn.call(hg)
  end

  defrecord ChangesNode,
    id : D7::NodeId,
    maxdepth : UInt32,
    limit : UInt32,
    dict : Term::Dict,
    deny_patches : Bool

  defrecord TrackerInfo,
    tracked_by : Array(ChangesNode),
    value : Term?,
    smart: true

  def prepass(hg : D7::Hypergraph, proposals : Array(D7::Patch), &fn : D7::Hypergraph, Array(D7::Patch) ->) : Nil
    # Fast path.
    unless hg.has_head?(SYM_CHANGES)
      fn.call(hg, proposals)
      return
    end

    tracker = {} of D7::NodeId => TrackerInfo
    target_cell_ids = Pf::USet32.new

    hg.each_node_with_head(SYM_CHANGES) do |node|
      Term.matchpi?(node.term, <<-'WWML') do |limit, maxdepth|
      [changes (behavior←(%any sync async) @edge_
                 ⍊ limit_: (%optional ∞ (%any° ∞ (%number u32)))
                   maxdepth_: (%optional ∞ (%any° ∞ (%number u32))))
        _*]
      WWML
        abs_edge = hg.resolve(node.addr, edge)
        next unless cell = Rack.cell?(hg, abs_edge)

        limit = limit == Term.of(:∞) ? UInt32::MAX : limit.to(UInt32)
        maxdepth = maxdepth == Term.of(:∞) ? UInt32::MAX : maxdepth.to(UInt32)
        node_dict = node.term.as_d
        deny_patches = behavior == Term.of(:sync) && node_dict.itemsize > 2 # [changes _ op_ _*]
        changes_node = ChangesNode.new(node.id, maxdepth, limit, node_dict, deny_patches)

        info = tracker.put_if_absent(cell.node.id) do
          TrackerInfo.new(tracked_by: [] of ChangesNode, value: cell.value?)
        end
        info.tracked_by << changes_node

        target_cell_ids = target_cell_ids.add(cell.node.id)
      end
    end

    buffer = [] of D7::Patch
    fn.call(hg, buffer)

    buffer.each do |proposal|
      denied = false

      proposal.each(in: target_cell_ids) do |cell_id, rep|
        info = tracker[cell_id]
        reference = info.value?

        info.tracked_by.each do |changes_node|
          break if denied

          Term.case(rep) do
            # [cell _ _]  [outlet _ _]  . . .
            matchpi %{[_ _ value_]} do
              next if reference == value # Unchanged

              if changes_node.deny_patches
                denied = true
                next
              end

              if reference
                diff = Term.diff?(reference, value, depth_limit: changes_node.maxdepth)
              end

              operations = [] of Term

              if reference.nil?
                assert value # reference != value was proven above
                operations << Term.of(:put, value)
              elsif diff.nil?
                operations << Term.of(:set, value)
              else
                insertions = {} of {Slice(Term), UInt32} => Array(Term)
                deletions = [] of Term::Diff::DeleteItem
                updates = [] of Term::Diff::WithItem | Term::Diff::PairAction

                diff.each do |action|
                  case action
                  in Term::Diff::InsertItem
                    bucket = insertions.put_if_absent({action.prefix, action.index}) { [] of Term }
                    bucket << action.item # (i.e., preserving diff-order in buckets)
                  in Term::Diff::DeleteItem
                    deletions << action
                  in Term::Diff::WithItem, Term::Diff::PairAction
                    updates << action
                  end
                end

                insertions.each do |(prefix, index), bucket|
                  operations << insert_op(reference, prefix, index, bucket)
                end

                deletions.each do |deletion|
                  operations << delete_op(reference, deletion.prefix, deletion.index, deletion.old_item)
                end

                updates.each do |update|
                  case update
                  in Term::Diff::WithItem
                    operations << assoc_op(reference, update.prefix, Term.of(update.index), update.item)
                  in Term::Diff::WithPair
                    operations << assoc_op(reference, update.prefix, update.key, update.value)
                  in Term::Diff::WithoutPair
                    operations << dissoc_op(reference, update.prefix, update.key, update.old_value)
                  end
                end
              end

              proposal = propose(changes_node, proposal, operations)
            end

            # [cell _]  [outlet _]  . . .
            matchpi %{[_ _]} do
              next if reference.nil? # Unchanged

              if changes_node.deny_patches
                denied = true
                next
              end

              proposal = propose(changes_node, proposal, [Term.of(:drop, reference)])
            end

            otherwise { }
          end
        end
      end

      next if denied

      proposals << proposal
    end
  end

  private def insert_op(reference : Term, prefix : Slice(Term), index : UInt32, seq : Array(Term)) : Term
    Term.of(:insert, Term.of(prefix).append(index), seq)
  end

  private def delete_op(reference : Term, prefix : Slice(Term), index : UInt32, old_value : Term) : Term
    Term.of(:delete, Term.of(prefix).append(index), old_value)
  end

  private def assoc_op(reference : Term, prefix : Slice(Term), key : Term, value : Term) : Term
    Term.of(:assoc, Term.of(prefix).append(key), value)
  end

  private def dissoc_op(reference : Term, prefix : Slice(Term), key : Term, old_value : Term) : Term
    Term.of(:dissoc, Term.of(prefix).append(key), old_value)
  end

  private def propose(changes_node : ChangesNode, proposal : D7::Patch, operations : Array(Term)) : D7::Patch
    # Append the extra operations:
    #
    #   (changes (@x limit: 3)
    #     A B C)
    #            v
    #   (changes (@x limit: 3)
    #     A B C ⏏D E⏏)
    rep = changes_node.dict.transaction do |commit|
      if op = operations.single?
        commit << op
      else
        # Sort operations to achieve deterministic ordering in the transaction.
        # The system that will interpret / execute operations will, presumably,
        # do its own sorting to properly modify the reference.
        commit << Term.of(operations.sort).prepend(:txn)
      end
    end

    if rep.itemsize - 2 > changes_node.limit
      # Expire operations past limit:
      #
      #   (changes (@x limit: 3)
      #     ⏏A B⏏ C D E)
      #           v
      #   (changes (@x limit: 3)
      #     C D E)
      rep = rep.replace(2u32...rep.itemsize.to_u32 - changes_node.limit, Term.rep)
    end

    # TODO: Handle this when (if?) this is reached.
    #
    # Supposedly nothing can modify the `changes` node directly (other than us here, of
    # course). But perhaps something can?
    assert !proposal.includes?(changes_node.id)
    proposal.assoc(changes_node.id, Term.of(rep))
  end
end
