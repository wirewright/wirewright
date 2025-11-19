module Ww::D7
  alias FoldRep0 = FoldContext, Feature, FoldRep0, FoldDefault -> Reaction
  alias FoldRep = FoldRep0
  alias FoldDefault = -> Reaction

  # :nodoc:
  record FoldContext,
    clf : Classifier,
    addr : NodeAddr,
    scope : NodeScope,
    event : Term?

  # Constructs the initial fold context.
  def fold_context(clf : Classifier, *, event : Term? = nil) : FoldContext
    FoldContext.new(clf, NodeAddr.empty, NodeScope.empty, event)
  end

  def fold(ctx : FoldContext, node, &frep : FoldRep) : Reaction
    fold(ctx, node, frep)
  end

  def fold(ctx : FoldContext, node : Term, frep : FoldRep) : Reaction
    fold(ctx, ctx.clf.call(node), frep)
  end

  def fold(ctx : FoldContext, feature : Inert, frep : FoldRep) : Reaction
    rxn(feature.node)
  end

  def fold(ctx : FoldContext, feature : Gnd, frep : FoldRep) : Reaction
    frep.call(ctx, feature, frep, -> { rxn(feature.node) })
  end

  def fold(ctx : FoldContext, feature : Parent, frep : FoldRep) : Reaction
    default = -> do
      node1 = nil

      queue = Term::Dict.build do |commit|
        node1 = flattenT(feature.node, range: feature.range) do |child, index|
          rxn = fold(ctx.copy_with(addr: ctx.addr.append(index)), child, frep)

          # NOTE: Events from children are concatenated in indeterminate order.
          # This should probably be changed to something deterministic (e.g. sort
          # events added during the same tick lexicographically).
          commit.concat(rxn.emission.items)

          rxn.node
        end
      end

      assert node1

      rxn(node1, queue)
    end

    frep.call(ctx, feature, frep, default)
  end

  def fold(ctx : FoldContext, feature : Scope, frep : FoldRep) : Reaction
    default = -> do
      subscope = ctx.scope.append({ctx.addr, feature.bindings})

      fold(ctx.copy_with(scope: subscope), feature.cont, frep)
    end

    frep.call(ctx, feature, frep, default)
  end

  def fold(ctx : FoldContext, feature : Mixture, frep : FoldRep) : Reaction
    default = -> do
      rxn = fold(ctx, feature.defn, frep)
      rxn(feature.mix.call(feature.node, rxn.node), rxn.emission)
    end

    frep.call(ctx, feature, frep, default)
  end

  def fold(ctx : FoldContext, feature : Circuit, frep : FoldRep) : Reaction
    default = -> do
      fold(ctx, feature.cont.call(Term.of(feature.node)), frep)
    end

    frep.call(ctx, feature, frep, default)
  end

  def fold(ctx : FoldContext, feature : Chat, frep : FoldRep) : Reaction
    default = -> do
      queue0 = feature.queue.items
      event0 = queue0.first?

      upflow = Term[]
      dnflow = Term[]

      if (event = ctx.event) && M1.probe?(feature.desc, event)
        dnflow = dnflow.append(event)

        # Remember to `ack` pulse events.
        Term.matchpi?(event, %{(pulse @_ _)}) do
          upflow = upflow.append(:ack)
        end
      end

      rxn = fold(ctx.copy_with(event: event0), feature.cont, frep)

      # Sort events appended during one tick lexicographically, so that we have
      # a deterministic order.
      events = rxn.emission.items.to_a.sort_by! do |event|
        # FIXME: this is really inefficient!!! We should have Term.compare!!!!
        ML.compact(event)
      end

      # `ack` events provide a mechanism for backpressure. The queue will
      # refuse to dequeue a `pulse` if it's not ack'd.
      #
      # NOTE: we disallow sending non-dict events on the user side to prevent
      # users from forging `ack`s.
      ackd = true
      if event0
        Term.matchpi?(event0, %{(pulse @_ _)}) do
          ackd = events.any? { |event1| Term.matchpi?(event1, %{ack}) { true } }
        end
      end

      queue1 = Term::Dict.build do |commit|
        unless queue0.empty?
          commit.concat(queue0.move(ackd ? 1 : 0))
        end

        commit.concat(dnflow.items)

        rxn.emission.items.each do |event|
          next unless event.type.dict? # < Ignores e.g. `ack`

          if M1.probe?(feature.asc, event)
            upflow = upflow.append(event)
            next
          end

          commit << event
        end
      end

      rxn(feature.submit.call(rxn.node, queue1), upflow)
    end

    frep.call(ctx, feature, frep, default)
  end

  private def flattenT(dict : Term::Dict, range : Range(Int32, Int32), & : Term, Int32 -> Term) : Term
    Term.of(flatten(dict, range) { |item, index| yield item, index })
  end

  private def flatten(dict : Term::Dict, range : Range(Int32, Int32), & : Term, Int32 -> Term) : Term::Dict
    dict.transaction do |commit|
      dict.each_item_with_index(within: range) do |item0, index|
        commit.with(index, (yield item0, index))
      end
    end
  end
end
