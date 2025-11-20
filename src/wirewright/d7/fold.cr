module Ww::D7
  alias NodeAddr = Slice(Int32)
  alias NodeScope = Slice({NodeAddr, Term::Dict})

  alias FoldRep0 = FoldContext, Feature, FoldRep0, FoldDefault -> Reaction
  alias FoldRep = FoldRep0
  alias FoldDefault = -> Reaction

  record NodeChat, msg : Term, enq : Bool do
    def self.cycle
      new(Term.of(:cycle), enq: true)
    end
  end

  # :nodoc:
  record FoldContext,
    clf : Classifier,
    addr : NodeAddr,
    scope : NodeScope,
    chat : NodeChat

  # Constructs the initial fold context.
  def fold_context(clf : Classifier, *, chat : NodeChat = NodeChat.cycle) : FoldContext
    FoldContext.new(clf, NodeAddr.empty, NodeScope.empty, chat)
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

          # NOTE: Messages from children during the smae tick are concatenated in
          # indeterminate order. The receiving chat will order them as it wishes.
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
      msg0 = queue0.first?

      upflow = Term[]
      dnflow = Term[]

      if M1.probe?(feature.desc, ctx.chat.msg)
        dnflow = dnflow.append(ctx.chat.msg)

        # Remember to `ack` pulse messages.
        Term.matchpi?(ctx.chat.msg, %{(pulse @_ _)}) do
          upflow = upflow.append(:ack)
        end
      end

      rxn = fold(ctx.copy_with(chat: NodeChat.new(msg0 || Term.of(:cycle), enq: feature.enq)), feature.cont, frep)

      # Sort messages appended during one tick lexicographically, so that we have
      # a deterministic order.
      msgs = rxn.emission.items.to_a.sort_by! do |msg|
        # FIXME: this is really inefficient!!! We should have Term.compare!!!!
        ML.compact(msg)
      end

      # `ack` messages provide a mechanism for backpressure. The queue will
      # refuse to dequeue a `pulse` if it's not ack'd.
      #
      # NOTE: we disallow sending non-dict messages on the user side to prevent
      # users from forging `ack`s.
      ackd = true
      if msg0
        Term.matchpi?(msg0, %{(pulse @_ _)}) do
          ackd = msgs.any? { |msg1| Term.matchpi?(msg1, %{ack}) { true } }
        end
      end

      queue1 = Term::Dict.build do |commit|
        if ackd
          commit.concat(queue0.move(1))
        else
          commit.concat(queue0)
        end

        commit.concat(dnflow.items)

        msgs.each do |msg|
          next unless msg.type.dict? # < Ignores e.g. `ack`

          if M1.probe?(feature.asc, msg)
            upflow = upflow.append(msg)
            next
          end

          Term.case(msg) do
            matchpi %{(group children_*)} { commit.concat(children.items) }
            otherwise { commit << msg }
          end
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
