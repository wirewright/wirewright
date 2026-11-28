require "./src/wirewright"
require "benchmark"

module NeoRack
  extend self

  alias Patch = PatchTable | PatchCancel
  alias PatchTable = D7::Patch

  defrecord PatchCancel

  alias NodeAddr = D7::NodeAddr
  alias NodeAddrRouter = D7::NodeAddrRouter

  alias MergePolicy = D7::MergePolicy
  alias MergeDiff = D7::MergeDiff
  alias MergeSeq = D7::MergeSeq

  alias NodeId = UInt32

  alias Node = IInert | IGnd | Mixture | Scope | ICircuit | IGroup | IAutonomy | Guard

  # Represents an inert (data) node. Inert nodes are nodes not recognized by
  # Rack or its extensions.
  module IInert
    # Returns the term that this node corresponds to / is a parsed
    # representation of.
    abstract def original : Term
  end

  # Represents an inert node, such as `(p "Hello World")` or `100`.
  defrecord Inert, original : Term, includes: {IInert}

  @[::Flags]
  enum NodeSignature : UInt64
    Group
    CellEmpty
    CellOccupied
    FeedTransfer
    FeedBroadcast
    FeedAggregate
    FeedDistribute
    FeedParallelTransfer
    DiscardUnconditional
    DiscardConditional
    Reading
    TransferOne
    TransferMany
    ViewOne
    ViewOneOver
    ViewMany
    ViewManyOver
    Delay
    PartBackmap
    PartBacksys
    BacksysSingle
    BacksysTemplate
    Changes
    FormLeaf
    OutletEmpty
    OutletOccupied
    FieldEmpty
    FieldOccupied
    Manipulable
    Supervisor
    ParserTransfer
    ParserView
    RewriterTransfer
    RewriterView
    RewriterVersionedView
    RewriterAsymmetricVersionedView
    RewriterRegimeTarget
    RewriterRegimeTemplate
    Fs
    Sequencer
    Ticker
    Timesource
    Lfo
    PendingTimer
    WaitingTimer
    PendingCountdown
    WaitingCountdown
    Clock

    def self.of(cls) : NodeSignature
      case cls
      in NeoRack::Group.class                             then Group
      in NeoRack::Cell.class                              then CellEmpty | CellOccupied
      in NeoRack::Cell::Empty.class                       then CellEmpty
      in NeoRack::Cell::Occupied.class                    then CellOccupied
      in NeoRack::Feed::Transfer.class                    then FeedTransfer
      in NeoRack::Feed::Broadcast.class                   then FeedBroadcast
      in NeoRack::Feed::Aggregate.class                   then FeedAggregate
      in NeoRack::Feed::Distribute.class                  then FeedDistribute
      in NeoRack::Feed::ParallelTransfer.class            then FeedParallelTransfer
      in NeoRack::Discard::Unconditional.class            then DiscardUnconditional
      in NeoRack::Discard::Conditional.class              then DiscardConditional
      in NeoRack::Reading.class                           then Reading
      in NeoRack::Transfer::One.class                     then TransferOne
      in NeoRack::Transfer::Many.class                    then TransferMany
      in NeoRack::View::One.class                         then ViewOne
      in NeoRack::View::Many.class                        then ViewMany
      in NeoRack::View::OneOver.class                     then ViewOneOver
      in NeoRack::View::ManyOver.class                    then ViewManyOver
      in NeoRack::Delay.class                             then Delay
      in NeoRack::Part.class                              then PartBackmap | PartBacksys
      in NeoRack::Part::Backmap.class                     then PartBackmap
      in NeoRack::Part::Backsys.class                     then PartBacksys
      in NeoRack::Backsys::Single.class                   then BacksysSingle
      in NeoRack::Backsys::Template.class                 then BacksysTemplate
      in NeoRack::Changes.class                           then Changes
      in NeoRack::FormLeaf.class                          then FormLeaf
      in NeoRack::Outlet.class                            then OutletEmpty | OutletOccupied
      in NeoRack::Outlet::Empty.class                     then OutletEmpty
      in NeoRack::Outlet::Occupied.class                  then OutletOccupied
      in NeoRack::Field.class                             then FieldEmpty | FieldOccupied
      in NeoRack::Field::Empty.class                      then FieldEmpty
      in NeoRack::Field::Occupied.class                   then FieldOccupied
      in NeoRack::Manipulable.class                       then Manipulable
      in NeoRack::Supervisor.class                        then Supervisor
      in NeoRack::Parser::Transfer.class                  then ParserTransfer
      in NeoRack::Parser::View.class                      then ParserView
      in NeoRack::Rewriter::Transfer.class                then RewriterTransfer
      in NeoRack::Rewriter::View.class                    then RewriterView
      in NeoRack::Rewriter::VersionedView.class           then RewriterVersionedView
      in NeoRack::Rewriter::AsymmetricVersionedView.class then RewriterAsymmetricVersionedView
      in NeoRack::Rewriter::RegimeTarget.class            then RewriterRegimeTarget
      in NeoRack::Rewriter::RegimeTemplate.class          then RewriterRegimeTemplate
      in NeoRack::Fs.class                                then Fs
      in NeoRack::Timing::Sequencer.class                 then Sequencer
      in NeoRack::Timing::Ticker.class                    then Ticker
      in NeoRack::Timing::Timesource.class                then Timesource
      in NeoRack::Timing::Lfo.class                       then Lfo
      in NeoRack::Timing::PendingTimer.class              then PendingTimer
      in NeoRack::Timing::WaitingTimer.class              then WaitingTimer
      in NeoRack::Timing::PendingCountdown.class          then PendingCountdown
      in NeoRack::Timing::WaitingCountdown.class          then WaitingCountdown
      in NeoRack::Timing::Clock.class                     then Clock
      end
    end
  end

  # Ground nodes are nodes recognized by Rack or its extensions.
  module IGnd
    # Returns the term that this node corresponds to / is a parsed
    # representation of.
    abstract def original : Term

    # Returns this node's merge policy of choice.
    abstract def merge_policy : MergePolicy

    # Returns the edges that this ground node participates in.
    abstract def each_edge(& : Term ->) : Nil
  end

  # A decomposition of *original* into a definition node *successor*, with a *fold*
  # function to compose possibly modified *defn* back into a possibly modified *original*.
  defcase Mixture, original : Term, successor : Node, fold : Term -> Term

  alias NodeScope = ScopeOpenExcept | ScopeClosedExcept

  defrecord ScopeOpenExcept, edges : Slice(Term)
  defrecord ScopeClosedExcept, bindings : Term::Dict

  # Wraps a *successor* node in a lexical scope qualified by *scope*.
  defcase Scope, original : Term, scope : NodeScope, successor : IGroup

  # A node with zero or more children.
  module IParent
    # Returns the term that this node corresponds to / is a parsed
    # representation of.
    def original : Term
      Term.of(original_dict)
    end

    # Returns the dictionary that this node corresponds to / is a parsed
    # representation of. Notably, `children` and `range` must align with
    # this dictionary.
    abstract def original_dict : Term::Dict

    abstract def children : Slice(Node)
    abstract def children_summary : Summary
    abstract def range : Range(UInt32, UInt32)
    abstract def map(& : Node -> Node) : self
  end

  # A circuit or a subcircuit.
  module ICircuit
    include IParent

    abstract def leaf : Node
  end

  # A group.
  module IGroup
    include IParent
  end

  # The implementation of `rack.group`.
  defcase Group,
    children_summary : Summary,
    original_dict : Term::Dict,
    children : Slice(Node),
    range : Range(UInt32, UInt32),
    includes: {IGroup}

  class Group
    def self.new(original_dict : Term::Dict, children : Slice(Node), range : Range(UInt32, UInt32)) : Group
      children_summary = NeoRack.summarize(children)
      new(children_summary, original_dict, children, range)
    end

    def map(& : Node -> Node) : Group
      Group.new(@original_dict, @children.to_readonly_slice { |child| (yield child).as(Node) }, @range)
    end
  end

  module IAutonomy
    abstract def original : Term
  end

  module Guard
    abstract def original : Term
    abstract def successor : Node
    abstract def passable?(hg : Hypergraph, addr : NodeAddr) : Bool

    defcase Empty, original : Term, edge : Term, successor : Node, includes: {Guard} do
      @lock = Sync::Mutex.new
      @answers = {} of {Hypergraph, NodeAddr} => Bool

      def passable?(hg : Hypergraph, addr : NodeAddr) : Bool
        @lock.synchronize do
          answer = @answers[{hg, addr}]?
          unless answer.nil?
            return answer # Return "memoized" answer to break out of recursion.
          end

          @answers[{hg, addr}] = false
        end

        begin
          dep = hg.single?(Cell::Occupied, connected_to: hg.resolve(addr, edge))
          dep.nil?
        ensure
          @lock.synchronize do
            @answers.delete({hg, addr})
          end
        end
      end
    end

    defcase Occupied, original : Term, edge : Term, pattern : M1::Op::Any, successor : Node, includes: {Guard} do
      @lock = Sync::Mutex.new
      @answers = {} of {Hypergraph, NodeAddr} => Bool

      def passable?(hg : Hypergraph, addr : NodeAddr) : Bool
        @lock.synchronize do
          answer = @answers[{hg, addr}]?
          unless answer.nil?
            return answer # Return "memoized" answer to break out of recursion.
          end

          @answers[{hg, addr}] = false
        end

        begin
          return false unless dep = hg.single?(Cell::Occupied, connected_to: hg.resolve(addr, edge))
          return false unless M1.probe?(Term[], pattern, dep.node.value)

          true
        ensure
          @lock.synchronize do
            @answers.delete({hg, addr})
          end
        end
      end
    end
  end

  # The implementation of `rack.cell`.
  module Cell
    include IGnd

    defcase Empty, original : Term, merge_policy : MergePolicy, edge : Term, includes: {Cell}
    defcase Occupied, original : Term, merge_policy : MergePolicy, edge : Term, value : Term, includes: {Cell}

    def each_edge(& : Term ->) : Nil
      yield edge
    end
  end

  # The implementation of `rack.feed`.
  module Feed
    include IGnd

    alias Src = RefCopy | Ref
    alias Dst = RefOver | Ref

    defrecord RefCopy, ref : Ref
    defrecord RefOver, ref : Ref

    alias Ref = Edge | DictFront | DictBack

    defrecord Edge, edge : Term
    defrecord DictFront, edge : Term
    defrecord DictBack, edge : Term

    def self.src?(term : Term) : Src?
      ref_copy?(term) || ref?(term)
    end

    def self.dst?(term : Term) : Dst?
      ref_over?(term) || ref?(term)
    end

    {% for side in %w[src dst] %}
      def self.{{side.id}}_list?(term : Term) : Slice({{side.id.capitalize}})?
        return unless dict = term.as_itemsonly_d?
        return unless dict.itemsize >= 1

        members = Pf::Kit.stack_array({{side.id.capitalize}}, 4)

        dict.items.each do |item|
          return unless member = {{side.id}}?(item)
          members << member
        end

        members.to_unsafe_readonly_slice!
      end
    {% end %}

    def self.ref_copy?(term : Term) : RefCopy?
      Term.matchpi?(term, %{(copy refQ_)}) do
        return unless ref = ref?(refQ)

        RefCopy.new(ref)
      end
    end

    def self.ref_over?(term : Term) : RefOver?
      Term.matchpi?(term, %{(over refQ_)}) do
        return unless ref = ref?(refQ)

        RefOver.new(ref)
      end
    end

    def self.ref?(term : Term) : Ref?
      Term.case(term) do
        matchpi %{@_} do
          Edge.new(term)
        end

        matchpi %{(@edge_ front)} do
          DictFront.new(edge)
        end

        matchpi %{(@edge_ back)} do
          DictBack.new(edge)
        end

        otherwise { }
      end
    end

    alias Inhibitor = Edge

    def self.inhibitor?(term : Term) : Inhibitor?
      return unless Term.edge?(term)

      Edge.new(term)
    end

    def self.inhibitor_list?(terms : Enumerable(Term)) : Slice(Inhibitor)?
      inhibitors = Pf::Kit.stack_array(Inhibitor, 4)

      terms.each do |term|
        return unless inhibitor = inhibitor?(term)
        inhibitors << inhibitor
      end

      inhibitors.to_unsafe_readonly_slice!
    end

    defrecord TakeResult, value : Term, patch : (-> Patch)
    defrecord PutResult, patch : Patch

    # :nodoc:
    def self.take?(hg : Hypergraph, addr : NodeAddr, src : Edge) : TakeResult?
      return unless place = hg.occupied_place?(addr, src.edge)

      TakeResult.new(place.value, -> { place.write(Term.rep) })
    end

    # :nodoc:
    def self.take?(hg : Hypergraph, addr : NodeAddr, src : DictFront) : TakeResult?
      return unless place = hg.occupied_place?(addr, src.edge)
      return unless dict = place.value.as_d?
      return unless head = dict.items.first?

      TakeResult.new(head, -> { place.write(Term.rep_of(dict.rest)) })
    end

    # :nodoc:
    def self.take?(hg : Hypergraph, addr : NodeAddr, src : DictBack) : TakeResult?
      return unless place = hg.occupied_place?(addr, src.edge)
      return unless dict = place.value.as_d?
      return unless tail = dict.items.last?

      TakeResult.new(tail, -> { place.write(Term.rep_of(dict.prior)) })
    end

    # :nodoc:
    def self.take?(hg : Hypergraph, addr : NodeAddr, src : RefCopy) : TakeResult?
      return unless result = take?(hg, addr, src.ref)

      TakeResult.new(result.value, -> { NeoRack.patch })
    end

    {% if flag?(:docs) %}
      # Reads *value* at *dst*.
      def self.take?(hg : Hypergraph, addr : NodeAddr, src : Src) : TakeResult?
      end
    {% end %}

    # :nodoc:
    def self.put?(hg : Hypergraph, addr : NodeAddr, dst : Edge, value : Term) : PutResult?
      return unless place = hg.empty_place?(addr, dst.edge)

      PutResult.new(place.write(Term.rep(value)))
    end

    # :nodoc:
    def self.put?(hg : Hypergraph, addr : NodeAddr, dst : DictFront, value : Term) : PutResult?
      return unless place = hg.occupied_place?(addr, dst.edge)
      return unless dict = place.value.as_d?

      PutResult.new(place.write(Term.rep_of(dict.prepend(value))))
    end

    # :nodoc:
    def self.put?(hg : Hypergraph, addr : NodeAddr, dst : DictBack, value : Term) : PutResult?
      return unless place = hg.occupied_place?(addr, dst.edge)
      return unless dict = place.value.as_d?

      PutResult.new(place.write(Term.rep_of(dict.append(value))))
    end

    # :nodoc:
    def self.put?(hg : Hypergraph, addr : NodeAddr, dst : RefOver, value : Term) : PutResult?
      over?(hg, addr, dst.ref, value)
    end

    {% if flag?(:docs) %}
      # Writes *value* at *dst*.
      def self.put?(hg : Hypergraph, addr : NodeAddr, dst : Dst, value : Term) : PutResult?
      end
    {% end %}

    private def self.over?(hg : Hypergraph, addr : NodeAddr, dst : Edge, value : Term) : PutResult?
      return unless place = hg.place?(addr, dst.edge)

      PutResult.new(place.write(Term.rep(value)))
    end

    private def self.over?(hg : Hypergraph, addr : NodeAddr, dst : DictFront, value : Term) : PutResult?
      return unless place = hg.occupied_place?(addr, dst.edge)
      return unless dict = place.value.as_d?
      return unless dict.uitemsize > 0

      PutResult.new(place.write(Term.rep_of(dict.with(0u32, value))))
    end

    private def self.over?(hg : Hypergraph, addr : NodeAddr, dst : DictBack, value : Term) : PutResult?
      return unless place = hg.occupied_place?(addr, dst.edge)
      return unless dict = place.value.as_d?
      return unless dict.uitemsize > 0

      PutResult.new(place.write(Term.rep_of(dict.with(dict.uitemsize - 1, value))))
    end

    def merge_policy : MergePolicy
      MergeDiff.new(0u32) # exclusive
    end

    defcase Transfer, original : Term, inhibitors : Slice(Inhibitor), src : Src, dst : Dst, includes: {Feed} do
      def each_edge(& : Term ->) : Nil
        inhibitors.each { |inhibitor| yield Feed.edge(inhibitor) }
        yield Feed.edge(src)
        yield Feed.edge(dst)
      end
    end

    defcase Broadcast, original : Term, inhibitors : Slice(Inhibitor), src : Src, dsts : Slice(Dst), includes: {Feed} do
      def each_edge(& : Term ->) : Nil
        inhibitors.each { |inhibitor| yield Feed.edge(inhibitor) }
        yield Feed.edge(src)
        dsts.each { |dst| yield Feed.edge(dst) }
      end
    end

    defcase Aggregate, original : Term, inhibitors : Slice(Inhibitor), srcs : Slice(Src), dst : Dst, includes: {Feed} do
      def each_edge(& : Term ->) : Nil
        inhibitors.each { |inhibitor| yield Feed.edge(inhibitor) }
        srcs.each { |src| yield Feed.edge(src) }
        yield Feed.edge(dst)
      end
    end

    defcase Distribute, original : Term, inhibitors : Slice(Inhibitor), src : Src, dsts : Slice(Dst), includes: {Feed} do
      def each_edge(& : Term ->) : Nil
        inhibitors.each { |inhibitor| yield Feed.edge(inhibitor) }
        yield Feed.edge(src)
        dsts.each { |dst| yield Feed.edge(dst) }
      end
    end

    defcase ParallelTransfer, original : Term, inhibitors : Slice(Inhibitor), srcs : Slice(Src), dsts : Slice(Dst), includes: {Feed} do
      def each_edge(& : Term ->) : Nil
        inhibitors.each { |inhibitor| yield Feed.edge(inhibitor) }
        srcs.each { |src| yield Feed.edge(src) }
        dsts.each { |dst| yield Feed.edge(dst) }
      end
    end
  end

  module Discard
    include IGnd

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
      yield edge
    end

    defcase Unconditional, original : Term, edge : Term, includes: {Discard}
    defcase Conditional, original : Term, edge : Term, pattern : M1::Op::Any, includes: {Discard}
  end

  # The implementation of the `rack.reading` node.
  defcase Reading, original : Term, edge : Term, includes: {IGnd} do
    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
      yield edge
    end
  end

  # The implementation of the `rack.transfer` node.
  module Transfer
    include IGnd

    defcase One,
      original : Term,
      src : Term,
      dst : Term,
      pattern : M1::Op::Any,
      template : Alloy::CompiledTemplate,
      includes: {Transfer}

    class One
      def each_edge(& : Term ->) : Nil
        yield src
        yield dst
      end
    end

    defcase Many,
      original : Term,
      inhibitors : Slice(Term),
      srcs : Slice(Term),
      dst : Term,
      pattern : M1::Op::Any,
      template : Alloy::CompiledTemplate,
      includes: {Transfer}

    class Many
      def each_edge(& : Term ->) : Nil
        inhibitors.each { |inhibitor| yield inhibitor }
        srcs.each { |src| yield src }
        yield dst
      end
    end

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end
  end

  # The implementation of the `rack.view` node.
  module View
    include IGnd

    defcase One,
      original : Term,
      src : Term,
      dst : Term,
      pattern : M1::Op::Any,
      template : Alloy::CompiledTemplate,
      includes: {View}

    class One
      def each_edge(& : Term ->) : Nil
        yield src
        yield dst
      end
    end

    defcase OneOver,
      original : Term,
      src : Term,
      dst : Term,
      pattern : M1::Op::Any,
      template : Alloy::CompiledTemplate,
      includes: {View}

    class OneOver
      def each_edge(& : Term ->) : Nil
        yield src
        yield dst
      end
    end

    defcase Many,
      original : Term,
      srcs : Slice(Term),
      dst : Term,
      pattern : M1::Op::Any,
      template : Alloy::CompiledTemplate,
      includes: {Transfer}

    class Many
      def each_edge(& : Term ->) : Nil
        srcs.each { |src| yield src }
        yield dst
      end
    end

    defcase ManyOver,
      original : Term,
      srcs : Slice(Term),
      dst : Term,
      pattern : M1::Op::Any,
      template : Alloy::CompiledTemplate,
      includes: {Transfer}

    class ManyOver
      def each_edge(& : Term ->) : Nil
        srcs.each { |src| yield src }
        yield dst
      end
    end

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end
  end

  # Implementation of the `rack.delay` node.
  defcase Delay,
    original : Term,
    countdown : UInt32,
    successor : Term,
    includes: {IGnd}

  class Delay
    def each_edge(& : Term ->) : Nil
    end

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end
  end

  defcase Circuit,
    children_summary : Summary,
    original_dict : Term::Dict,
    children : Slice(Node),
    range : Range(UInt32, UInt32),
    leaf : Node,
    includes: {ICircuit}

  class Circuit
    def self.new(original_dict : Term::Dict, children : Slice(Node), range : Range(UInt32, UInt32), leaf : Node) : Node
      children_summary = Summary.union(NeoRack.summarize(children), NeoRack.summarize(leaf))
      new(children_summary, original_dict, children, range, leaf)
    end

    def map(& : Node -> Node) : Circuit
      Circuit.new(@original_dict, @children.to_readonly_slice { |child| (yield child).as(Node) }, @range, (yield @leaf).as(Node))
    end
  end

  defcase Device,
    children_summary : Summary,
    original_dict : Term::Dict,
    children : Slice(Node),
    range : Range(UInt32, UInt32),
    includes: {ICircuit}

  class Device
    def self.new(original_dict : Term::Dict, children : Slice(Node), range : Range(UInt32, UInt32)) : Device
      children_summary = NeoRack.summarize(children)
      new(children_summary, original_dict, children, range)
    end

    def leaf : Node
      Inert.new(original)
    end

    def map(& : Node -> Node) : Device
      Device.new(@original_dict, @children.to_readonly_slice { |child| (yield child).as(Node) }, @range)
    end
  end

  module Part
    include IGnd

    defcase Backmap,
      original : Term,
      src : Term,
      dst : Term,
      capture : Term,
      pattern : M1::Op::Any,
      includes: {Part}

    defcase Backsys,
      original : Term,
      src : Term,
      dst : Term,
      whole : Term,
      capture : Term,
      backsys : Slice({M1::Op::Any, Term::Dict}),
      includes: {Part}

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
      yield src
      yield dst
    end
  end

  module Backsys
    include IGnd

    defcase Single,
      original : Term,
      edge : Term,
      backsys : Slice({M1::Op::Any, Term::Dict}),
      includes: {Backsys}

    class Single
      def each_edge(& : Term ->) : Nil
        yield edge
      end
    end

    defcase Template,
      original : Term,
      template : EdgeTemplate,
      template_dict : Term::Dict,
      backsys : Slice({M1::Op::Any, Term::Dict}),
      includes: {Backsys}

    class Template
      def each_edge(& : Term ->) : Nil
        template.each { |member| yield member.edge }
      end
    end

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end
  end

  defcase Changes,
    original : Term,
    edge : Term,
    maxdepth : UInt32,
    writable : Bool,
    includes: {IGnd}

  class Changes
    def merge_policy : MergePolicy
      # Different parties contributing different things to the same cell with a relaxed
      # merge policy -- a cell this changes node is watching -- may require diff-based
      # merging on the `changes` node itself. Hence the need for the surface merge policy
      # on `changes`.
      MergeDiff.new(1u32)
    end

    def each_edge(& : Term ->) : Nil
      yield edge
    end

    def append(ops : Enumerable(Term)) : Term
      if op = ops.single?
        return Term.of(original.append(op))
      end

      # Sort operations to achieve deterministic ordering in the transaction.
      # The system that will interpret / execute operations will, presumably,
      # do its own sorting to properly modify the reference.
      Term.of(original.append(Term[ops.to_a.sort!].prepend(:txn)))
    end

    def self.log(changes : NodeRef(Changes), before : Nil, after : Nil) : Patch
      NeoRack.patch # no change
    end

    def self.log(changes : NodeRef(Changes), before : Term, after : Nil) : Patch
      unless changes.node.writable
        return PatchCancel.new
      end

      op = Term.of(:drop, before)
      NeoRack.replace(changes, changes.node.append({op}))
    end

    def self.log(changes : NodeRef(Changes), before : Nil, after : Term) : Patch
      unless changes.node.writable
        return PatchCancel.new
      end

      op = Term.of(:put, after)
      NeoRack.replace(changes, changes.node.append({op}))
    end

    def self.log(changes : NodeRef(Changes), before : Term, after : Term) : Patch
      unless changes.node.writable
        return PatchCancel.new
      end

      diff = Term.diff?(before, after, depth_limit: changes.node.maxdepth)

      if diff.nil?
        op = Term.of(:set, after)
        return NeoRack.replace(changes, changes.node.append({op}))
      end

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

      ops = Pf::Kit.stack_array(Term)

      insertions.each do |(prefix, index), bucket|
        ops << insert_op(before, prefix, index, bucket)
      end

      deletions.each do |deletion|
        ops << delete_op(before, deletion.prefix, deletion.index, deletion.old_item)
      end

      updates.each do |update|
        case update
        in Term::Diff::WithItem
          ops << assoc_op(before, update.prefix, Term.of(update.index), update.item)
        in Term::Diff::WithPair
          ops << assoc_op(before, update.prefix, update.key, update.value)
        in Term::Diff::WithoutPair
          ops << dissoc_op(before, update.prefix, update.key, update.old_value)
        end
      end

      NeoRack.replace(changes, changes.node.append(ops))
    end

    private def self.insert_op(reference : Term, prefix : Slice(Term), index : UInt32, seq : Array(Term)) : Term
      Term.of(:insert, Term.of(prefix).append(index), seq)
    end

    private def self.delete_op(reference : Term, prefix : Slice(Term), index : UInt32, old_value : Term) : Term
      Term.of(:delete, Term.of(prefix).append(index), old_value)
    end

    private def self.assoc_op(reference : Term, prefix : Slice(Term), key : Term, value : Term) : Term
      Term.of(:assoc, Term.of(prefix).append(key), value)
    end

    private def self.dissoc_op(reference : Term, prefix : Slice(Term), key : Term, old_value : Term) : Term
      Term.of(:dissoc, Term.of(prefix).append(key), old_value)
    end
  end

  defrecord FormLeaf, form : Form, includes: {IGnd}

  struct FormLeaf
    def original : Term
      form.original
    end

    def merge_policy : MergePolicy
      form.merge_policy
    end

    def each_edge(& : Term ->) : Nil
      yield form.edge
    end
  end

  defcase Form,
    children_summary : Summary,
    original_dict : Term::Dict,
    merge_policy : MergePolicy,
    edge : Term,
    children : Slice(Node),
    range : Range(UInt32, UInt32),
    uniform : Bool,
    includes: {ICircuit}

  class Form
    def self.new(original_dict : Term::Dict, merge_policy : MergePolicy, edge : Term, children : Slice(Node), range : Range(UInt32, UInt32), uniform : Bool) : Form
      children_summary = NeoRack.summarize(children)
      new(children_summary, original_dict, merge_policy, edge, children, range, uniform)
    end

    def map(& : Node -> Node) : Form
      Form.new(@original_dict, @merge_policy, @edge, @children.to_readonly_slice { |child| yield child }, @range, @uniform)
    end

    def leaf : FormLeaf
      FormLeaf.new(self)
    end

    alias Entry = OutletEntry | FieldEntry

    defrecord OutletEntry, addr : NodeAddr, original : Term, key : Term, value : Term?, readonly : Bool
    defrecord FieldEntry, addr : NodeAddr, original : Term, key : Term, value : Term?, readonly : Bool

    private def self.scan(hg : Hypergraph, addr : NodeAddr, node : IInert | IAutonomy, ordptr : UInt32*, fn)
    end

    private def self.scan(hg : Hypergraph, addr : NodeAddr, node : IGnd, ordptr : UInt32*, fn)
      case node
      when Outlet::Empty
        key = Term.of(ordptr.value)
        ordptr.value += 1
        fn.call(OutletEntry.new(addr, node.original, key, nil, node.readonly?))
      when Outlet::Occupied
        key = Term.of(ordptr.value)
        ordptr.value += 1
        fn.call(OutletEntry.new(addr, node.original, key, node.value, node.readonly?))
      when Field::Empty
        fn.call(FieldEntry.new(addr, node.original, node.key, nil, node.readonly?))
      when Field::Occupied
        fn.call(FieldEntry.new(addr, node.original, node.key, node.value, node.readonly?))
      end
    end

    private def self.scan(hg : Hypergraph, addr : NodeAddr, node : Mixture | Scope, ordptr : UInt32*, fn)
      scan(hg, addr, node.successor, ordptr, fn)
    end

    private def self.scan(hg : Hypergraph, addr : NodeAddr, node : ICircuit, ordptr : UInt32*, fn)
      return if node.is_a?(Form) # Do not descend into nested forms.

      node.children.zip(node.range) do |child, key|
        scan(hg, addr.append(key), child, ordptr, fn)
      end
    end

    private def self.scan(hg : Hypergraph, addr : NodeAddr, node : IGroup, ordptr : UInt32*, fn)
      node.children.zip(node.range) do |child, key|
        scan(hg, addr.append(key), child, ordptr, fn)
      end
    end

    private def self.scan(hg : Hypergraph, addr : NodeAddr, node : Guard, ordptr : UInt32*, fn)
      return unless node.passable?(hg, addr)

      scan(hg, addr, node.successor, ordptr, fn)
    end

    def self.scan(form : Form, &fn : Entry ->)
      summary = NeoRack.summarize(form)
      return unless summary.includes?(Outlet) || summary.includes?(Field)

      # We need this hypergraph to evaluate guards. Since forms are circuits they
      # are self-enclosed, so we're fine rooting the hypergraph at the form.
      hg = Hypergraph.new(form)

      ord = 0u32

      # Skip the form itself because we'd immediately hit the "do not descend
      # into nested forms" branch above, thinking it's a nested form.
      form.children.zip(form.range) do |child, key|
        scan(hg, NodeAddr[key], child, pointerof(ord), fn)
      end
    end
  end

  module Outlet
    include IGnd

    def merge_policy : MergePolicy
      spec.merge_policy
    end

    def readonly? : Bool
      spec.readonly
    end

    def each_edge(& : Term ->) : Nil
      yield spec.edge
    end

    defrecord Spec,
      merge_policy : MergePolicy,
      edge : Term,
      readonly : Bool,
      copying: true

    defcase Empty, original : Term, spec : Spec, includes: {Outlet}
    defcase Occupied, original : Term, spec : Spec, value : Term, includes: {Outlet}

    def self.spec?(term : Term) : Spec?
      Term.case(term) do
        matchpi %{(readonly baseQ_)} do
          continue unless base = spec_base?(baseQ)

          base.copy_with(readonly: true)
        end

        otherwise { spec_base?(term) }
      end
    end

    private def self.spec_base?(term : Term) : Spec?
      Term.case(term) do
        matchpi %{(%'edge key_)} do
          Spec.new(MergeDiff.new(0u32), term, readonly: false)
        end

        matchpiT %{(policyQ_symbol edge←(%'edge key_))} do
          return unless policy = NeoRack.merge_policy?(policyQ)

          Spec.new(policy, edge, readonly: false)
        end

        otherwise { }
      end
    end
  end

  module Field
    include IGnd

    def merge_policy : MergePolicy
      spec.merge_policy
    end

    def key : Term
      spec.key
    end

    def readonly? : Bool
      spec.readonly
    end

    def each_edge(& : Term ->) : Nil
      yield spec.edge
    end

    defrecord Spec,
      merge_policy : MergePolicy,
      edge : Term,
      key : Term,
      readonly : Bool,
      copying: true

    defcase Empty, original : Term, spec : Spec, includes: {Field}
    defcase Occupied, original : Term, spec : Spec, value : Term, includes: {Field}

    def self.spec?(term : Term) : Spec?
      Term.case(term) do
        matchpi %{(readonly baseQ_)} do
          continue unless base = spec_base?(baseQ)

          base.copy_with(readonly: true)
        end

        matchpi %{(readonly baseQ_ name_)} do
          continue unless base = spec_base?(baseQ)

          base.copy_with(readonly: true, key: name)
        end

        matchpi %{(baseQ_ name_)} do
          continue unless base = spec_base?(baseQ)

          base.copy_with(key: name)
        end

        otherwise { spec_base?(term) }
      end
    end

    private def self.spec_base?(term : Term) : Spec?
      Term.case(term) do
        matchpi %{(%'edge key_)} do
          Spec.new(MergeDiff.new(0u32), term, key, readonly: false)
        end

        matchpiT %{(policyQ_symbol edge←(%'edge key_))} do
          return unless policy = NeoRack.merge_policy?(policyQ)

          Spec.new(policy, edge, key, readonly: false)
        end

        otherwise { }
      end
    end
  end

  defcase Manipulable,
    original : Term,
    edge : Term,
    selector : M1::Op::Any,
    env_trace_lists : M1::EnvTraceList,
    operand : Term,
    includes: {IGnd}

  class Manipulable
    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
      yield edge
    end
  end

  defcase Supervisor,
    original : Term,
    tasks : Term,
    reference_edge : Term,
    task_edge : Term,
    keyp : M1::Op::Any,
    pool : Pool,
    template : Term::Dict,
    includes: {IGnd}

  class Supervisor
    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
      yield tasks
      yield pool
    end

    alias Pool = DevicePool | ModulePool

    defrecord DevicePool, edge : Term
    defrecord ModulePool, edge : Term, bindings : Term::Dict

    defrecord Fixture, original : Term

    defrecord Task, key : Term, body : Term, copying: true

    defrecord Worker,
      original : Term,
      key : Term,
      reference : Term,
      current : Term?,
      reference_index : UInt32,
      current_index : UInt32,
      copying: true

    # Instantiates a worker for *task*.
    def self.worker(supervisor : Supervisor, task : Task) : Worker
      original = Term::Dict.build do |commit|
        case pool = supervisor.pool
        in DevicePool
          commit << :device
        in ModulePool
          commit << :module
          commit << pool.bindings
        end

        commit << {:cell, {:edge, :reference}, task.body}
        commit << {:cell, supervisor.task_edge, task.body}
        commit.concat(supervisor.template.items)
      end

      reference = current = task.body
      case supervisor.pool
      in DevicePool
        reference_index = 1u32 # < refer to the dict we've just constructed
        current_index = 2u32   # <
      in ModulePool
        reference_index = 2u32
        current_index = 3u32
      end
      Worker.new(Term.of(original), task.key, reference, current, reference_index, current_index)
    end

    # Returns the value of a cell at *edge* in a *worker*.
    def self.worker_get?(ahead : Term::Dict::ItemsView, edge : Term) : {Term?, UInt32}?
      ahead.zip(ahead.b...ahead.e) do |child, index|
        Term.case(child) do
          matchpi %{[cell @candidate_ value_]} do
            next unless candidate == edge
            return value, index.to_u32
          end

          matchpi %{[cell @candidate_]} do
            return nil, index.to_u32
          end

          otherwise { }
        end
      end
    end

    # Parses a task dict into a sequence of `Task`s and `Fixture`s (unrecognized terms
    # which `supervisor` leaves as-is).
    def self.task_seq(supervisor : Supervisor, tasks : Term::Dict) : Array(Task | Fixture)
      seq = [] of Task | Fixture

      tasks.items.each do |item|
        added = false

        pass do
          next unless env = M1.match?(Term[], supervisor.keyp, item)
          next unless entry = env.ee.first?
          _, key = entry

          seq << Task.new(key, item)
          added = true
        end

        next if added

        seq << Fixture.new(item)
      end

      seq
    end

    # :nodoc:
    SYM_DEVICE = Term.of(:device)
    # :nodoc:
    SYM_MODULE = Term.of(:module)

    # Parses a worker pool dict into a sequence of `Worker`s and `Fixture`s (unrecognized
    # terms which `supervisor` leaves as-is).
    def self.worker_seq(supervisor : Supervisor, workers : Term::Dict) : Array(Worker | Fixture)
      seq = [] of Worker | Fixture

      workers.items.each do |item|
        added = false

        pass do
          next unless worker = item.as_d?
          next unless worker.itemsize >= 2

          ahead = worker.items

          case pool = supervisor.pool
          in Supervisor::DevicePool
            next unless ahead.starts_with?({SYM_DEVICE})
            ahead = ahead.move(1)
          in Supervisor::ModulePool
            next unless ahead.starts_with?({SYM_MODULE, pool.bindings})
            ahead = ahead.move(2)
          end

          next unless row = worker_get?(ahead, supervisor.reference_edge)

          reference, reference_index = row
          next if reference.nil?
          next unless env = M1.match?(Term[], supervisor.keyp, reference)
          next unless entry = env.ee.first?
          _, key = entry

          next unless row = worker_get?(ahead, supervisor.task_edge)
          current, current_index = row

          seq << Worker.new(item, key, reference, current, reference_index, current_index)
          added = true
        end

        next if added

        seq << Fixture.new(item)
      end

      seq
    end

    alias TaskMap = Hash(Term, Array(Task))
    alias WorkerMap = Hash(Term, Array(Worker))

    # Indexes a sequence of workers and fixtures *seq*.
    def self.worker_map(seq : Array(Worker | Fixture)) : WorkerMap
      map = WorkerMap.new

      seq.each do |item|
        case item
        in Fixture
        in Worker
          bucket = map.put_if_absent(item.key) { [] of Worker }
          bucket << item
        end
      end

      map
    end

    # Creates a task order map, which maps a task key to the index of
    # the corresponding task.
    def self.task_order(seq : Array(Task | Fixture)) : Hash(Term, UInt32)
      seq.each
        .select(Task)
        .with_index
        .to_h { |task, index| {task.key, index.to_u32} }
    end

    # Indexes a sequence of tasks and fixtures *seq*.
    def self.task_map(seq : Array(Task | Fixture)) : TaskMap
      map = TaskMap.new

      seq.each do |item|
        case item
        in Fixture
        in Task
          bucket = map.put_if_absent(item.key) { [] of Task }
          bucket << item
        end
      end

      map
    end

    # *delete* is a set of keys. Tasks with these keys must be removed.
    # *update* maps a key to the corresponding updated task.
    defrecord TaskPlan,
      delete = Set(Term).new,
      update = {} of Term => Term

    struct TaskPlan
      def empty? : Bool
        delete.empty? && update.empty?
      end
    end

    # *create* is a list of tasks for which workers should be inserted.
    # *update* maps a key to a task. Workers must update their reference and
    # current cell to the task.
    # *delete* is a set of keys. Workers with these keys must be removed.
    defrecord WorkerPlan,
      create = Array(Task).new,
      update = {} of Term => Term,
      delete = Set(Term).new

    struct WorkerPlan
      def empty? : Bool
        create.empty? && update.empty? && delete.empty?
      end
    end

    def self.plan(task_map : TaskMap, worker_map : WorkerMap) : {TaskPlan, WorkerPlan}
      task_plan = TaskPlan.new
      worker_plan = WorkerPlan.new

      task_map.each do |key, bucket|
        # Supervisors really don't like colliding tasks so they completely prune them
        # and the corresponding devices (if any).
        unless task = bucket.single?
          task_plan.delete << key
          worker_plan.delete << key
          next
        end

        unless associated_workers = worker_map[key]?
          worker_plan.create << task
          next
        end

        # If there are two or more workers on the same key, delete them.
        # They collide.
        unless worker = associated_workers.single?
          worker_plan.delete << key # this will delete all of them
          next
        end

        # Process changes of tasks by workers.
        current = worker.current

        if current.nil?
          # The worker wants to remove itself and the associated task. Perhaps
          # it is "complete".
          task_plan.delete << key
          worker_plan.delete << key
          next
        end

        if worker.reference == current
          next if worker.reference == task.body

          # The worker did not change its copy of task, but the task in tasks
          # changed. Update the worker's copy.
          worker_plan.update[key] = task.body
          next
        end

        # The worker updated its copy of the task. Update the task and the worker's
        # own reference copy.
        task_plan.update[key] = current
        worker_plan.update[key] = current
      end

      # Prune workers without associated tasks.
      worker_map.each do |key, bucket|
        next if task_map.has_key?(key)

        worker_plan.delete << key
      end

      {task_plan, worker_plan}
    end

    def self.execute(supervisor : Supervisor, seq : Array(Task | Fixture), plan : TaskPlan) : Array(Task | Fixture)
      seq.compact_map do |item|
        case item
        in Fixture
          item # Keep it as-is.
        in Task
          next if item.key.in?(plan.delete) # Skip it if they want to delete it.

          updated = plan.update[item.key]?
          updated ? item.copy_with(body: updated) : item
        end
      end
    end

    def self.execute(supervisor : Supervisor, seq : Array(Worker | Fixture), plan : WorkerPlan) : Array(Worker | Fixture)
      seq = seq.compact_map do |item|
        case item
        in Fixture
          item
        in Worker
          next if item.key.in?(plan.delete) # Skip it if they want to delete it.

          updated = plan.update[item.key]?
          updated ? item.copy_with(reference: updated, current: updated) : item
        end
      end

      plan.create.each do |task|
        seq << worker(supervisor, task)
      end

      seq
    end

    def self.render(seq : Array(Task | Fixture), tasks : Term::Dict) : Term::Dict
      Term::Dict.build do |commit|
        seq.each do |item|
          case item
          in Fixture then commit << item.original
          in Task    then commit << item.body
          end
        end
      end
    end

    def self.render(seq : Array(Worker | Fixture), order : Hash(Term, UInt32), workers : Term::Dict) : Term::Dict
      # Sort workers so that they have the same order as their corresponding tasks.
      workerq = seq.select(Worker)
      workerq.sort_by! { |worker| order[worker.key] }

      # Render the resulting pool.
      Term::Dict.build do |commit|
        seq.each do |item|
          case item
          in Fixture
            commit << item.original
          in Worker
            item = workerq.shift # shadow

            # 2 refers to the cell's value:
            #   (cell @reference ⏏)
            #   (cell @current ⏏)
            commit << Term.morph(item.original,
              {item.reference_index, 2, item.reference},
              {item.current_index, 2, item.current},
            )
          end
        end
      end
    end

    def self.ordered?(seq : Array(Worker | Fixture), order : Hash(Term, UInt32)) : Bool
      seq.each
        .select(Worker)
        .map { |worker| order[worker.key] }
        .cons_pair
        .all? { |l, r| l < r }
    end

    @@nops = SyncLRU(Term::Str, Bool).new(capacity: 128)

    def self.repair(hg : Hypergraph, patches : Array(Patch), supervisor : NodeRef(Supervisor)) : Nil
      # Fetch stuff from the hypergraph.
      if tasks_cell = hg.occupied_place?(supervisor.addr, supervisor.node.tasks)
        tasks0 = tasks_cell.value.as_d?
      end
      tasks0 ||= Term[]

      return unless pool_cell = hg.place?(supervisor.addr, supervisor.node.pool.edge)

      workers0 = pool_cell.as?(Place::Occupied).try(&.value.as_d?) || Term[]

      digest = Term.digest(supervisor.node.original, workers0, tasks0)
      return if !!@@nops.get?(digest) # Fast path if we've recently learned it's a nop.

      # Parse tasks.
      task_seq = Supervisor.task_seq(supervisor.node, tasks0)
      task_order = Supervisor.task_order(task_seq)
      task_map = Supervisor.task_map(task_seq)

      # Parse workers.
      worker_seq = Supervisor.worker_seq(supervisor.node, workers0)
      worker_map = Supervisor.worker_map(worker_seq)

      # Build and execute plans.
      task_plan, worker_plan = Supervisor.plan(task_map, worker_map)

      unless task_plan.empty?
        task_seq = Supervisor.execute(supervisor.node, task_seq, task_plan)
      end
      unless worker_plan.empty?
        worker_seq = Supervisor.execute(supervisor.node, worker_seq, worker_plan)
      end

      # Render the new task and worker dicts.

      # Task render() only depends on the task plan.
      unless task_plan.empty?
        tasks1 = Supervisor.render(task_seq, tasks0)
      end

      # Worker render() depends on worker plan and also on proper order, since it
      # may want to order workers properly.
      unless worker_plan.empty? && Supervisor.ordered?(worker_seq, task_order)
        workers1 = Supervisor.render(worker_seq, task_order, workers0)
      end

      # Produce patches.

      if workers1 && workers0 != workers1
        pool_patch = pool_cell.write(Term.rep_of(workers1))
      end

      if tasks1 && tasks_cell && tasks0 != tasks1
        tasks_patch = tasks_cell.write(Term.rep_of(tasks1))
      end

      if pool_patch.nil? && tasks_patch.nil?
        @@nops.put(digest, true) # Remember it's a nop to avoid wasting time in the future.
        return
      end

      patches << NeoRack.txn(tasks_patch || NeoRack.patch, pool_patch || NeoRack.patch)
    end
  end

  module Parser
    include IGnd

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
      yield src
      yield dst
    end

    defcase Transfer,
      original : Term,
      src : Term,
      dst : Term,
      error : Term?,
      grammar : ParseKit::GrammarF,
      grammar_digest : Term::Str,
      top : Term::Sym,
      cache : SyncLRU(Term::Str, Result),
      includes: {Parser}

    defcase View,
      original : Term,
      src : Term,
      dst : Term,
      error : Term?,
      grammar : ParseKit::GrammarF,
      grammar_digest : Term::Str,
      top : Term::Sym,
      cache : SyncLRU(Term::Str, Result),
      includes: {Parser}

    defrecord Task,
      text : Term::Str,
      grammar : ParseKit::GrammarF,
      grammar_digest : Term::Str,
      top : Term::Sym

    struct Task
      def_equals_and_hash text, grammar_digest, top
    end

    alias Result = Term | ParseKit::Err

    def self.execute(task : Task, ping) : Result
      seln = task.text.to(Pf::StringSeln)
      ctx = ParseKit.context(task.grammar, ping)
      ParseKit.resolve(ParseKit.parse(ctx, task.top, seln))
    end
  end

  module Rewriter
    include IGnd

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    defcase Transfer,
      original : Term,
      src : Term,
      dst : Term,
      rho : Rho::Rewriter,
      cache : Rho::IRewriteCache,
      includes: {Rewriter}

    class Transfer
      def each_edge(& : Term ->) : Nil
        yield src
        yield dst
      end
    end

    defcase View,
      original : Term,
      src : Term,
      dst : Term,
      rho : Rho::Rewriter,
      cache : Rho::IRewriteCache,
      includes: {Rewriter}

    class View
      def each_edge(& : Term ->) : Nil
        yield src
        yield dst
      end
    end

    defcase VersionedView,
      original : Term,
      src : Term,
      version : Term,
      dst : Term,
      rho : Rho::Rewriter,
      cache : Rho::IRewriteCache,
      includes: {Rewriter}

    class VersionedView
      def each_edge(& : Term ->) : Nil
        yield src
        yield version
        yield dst
      end
    end

    defcase AsymmetricVersionedView,
      original : Term,
      src : Term,
      version : Term,
      dst : Term,
      rho : Rho::Rewriter,
      cache : Rho::IRewriteCache,
      includes: {Rewriter}

    class AsymmetricVersionedView
      def each_edge(& : Term ->) : Nil
        yield src
        yield version
        yield dst
      end
    end

    defcase RegimeTarget,
      original : Term,
      target : Term,
      rho : Rho::Rewriter,
      cache : Rho::IRewriteCache,
      nop : SyncLRU(Term::Str, Bool),
      includes: {Rewriter}

    class RegimeTarget
      def each_edge(& : Term ->) : Nil
        yield target
      end
    end

    defcase RegimeTemplate,
      original : Term,
      template : EdgeTemplate,
      template_dict : Term::Dict,
      rho : Rho::Rewriter,
      cache : Rho::IRewriteCache,
      nop : SyncLRU(Term::Str, Bool),
      includes: {Rewriter}

    class RegimeTemplate
      def each_edge(& : Term ->) : Nil
        template.each { |edge| yield edge }
      end
    end

    defrecord Task,
      rewriter : Rho::Rewriter,
      cache : Rho::IRewriteCache,
      input : Term

    alias Result = Term

    def self.execute(task : Task, ping) : Result
      Rho.rewrite(task.rewriter, task.input, cache: task.cache, ping: ping)
    end

    def self.rewriter_and_cache(spec : Term, data : Term) : {Rho::Rewriter, Rho::IRewriteCache}
      Term.case(spec) do
        matchpiT %{(cached rwspec_ ⍊ capacity: (%optional 32 capacity←(%number +i32)))} do
          rewriter = Rho.rewriter!(rwspec, data)
          cache = SyncLRU({Rho::RewriterId, Term}, Term::Rep).new(capacity)
          {rewriter, cache}
        end

        otherwise do
          rewriter = Rho.rewriter!(spec, data)
          cache = Uncached({Rho::RewriterId, Term}, Term::Rep).new
          {rewriter, cache}
        end
      end
    end
  end

  defcase Fs,
    original : Term,
    query : Term,
    reply : Term,
    includes: {IGnd}

  class Fs
    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
      yield query
      yield reply
    end

    alias Request = CreateFile | CreateDir | CreateDirIfMissing | DeleteFile | DeleteFileIfExists |
                    DeleteDir | DeleteDirIfExists | Move | CreateTmpFile | OverwriteFile | AppendFile |
                    ReadTextFile | ReadBinaryFile

    defrecord CreateFile, path : NormalPath
    defrecord CreateDir, path : NormalPath
    defrecord CreateDirIfMissing, path : NormalPath
    defrecord DeleteFile, path : NormalPath
    defrecord DeleteFileIfExists, path : NormalPath
    defrecord DeleteDir, path : NormalPath, recursive : Bool
    defrecord DeleteDirIfExists, path : NormalPath, recursive : Bool
    defrecord Move, src : NormalPath, dst : NormalPath
    defrecord CreateTmpFile, parent : NormalPath
    defrecord OverwriteFile, path : NormalPath, content : Term::Str | Term::Blob
    defrecord AppendFile, path : NormalPath, content : Term::Str | Term::Blob
    defrecord ReadTextFile, path : NormalPath
    defrecord ReadBinaryFile, path : NormalPath

    def self.request?(term : Term) : Request?
      # |@ rack.fs.request
      #
      # |@summary
      # Requests that you can make to the file system.
      Term.case(term) do
        # |@ rack.fs.request
        #
        # |@pattern
        # [create file path_string]
        #
        # |@block
        # Creates a file at *path* if missing. This works like the shell
        # command `touch`.
        matchpi %{[create file path_string]}, path: NormalPath do
          CreateFile.new(path)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [create dir path_string]
        #
        # |@block
        # Creates a directory at *path*. Fails if *path* exists. This works
        # like the shell command `mkdir`.
        matchpi %{[create dir path_string]}, path: NormalPath do
          CreateDir.new(path)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [create dir if missing path_string]
        #
        # |@block
        # Creates a directory at *path* if it is missing.
        matchpi %{[create dir if missing path_string]}, path: NormalPath do
          CreateDirIfMissing.new(path)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [delete file path_string]
        #
        # |@block
        # Removes the file at *path*. Fails if *path* does not exist or is
        # a directory.
        matchpi %{[delete file path_string]}, path: NormalPath do
          DeleteFile.new(path)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [delete file if exists path_string]
        #
        # |@block
        # Removes the file at *path* if present. Fails if *path* is a directory.
        matchpi %{[delete file if exists path_string]}, path: NormalPath do
          DeleteFileIfExists.new(path)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # (delete dir path_string ⍊ recursive⋮ false)
        #
        # |@key recursive
        # Whether to delete recursively (e.g. `rm` vs. `rm -r`). If `false`, fails
        # if *path* is a nonempty directory.
        #
        # |@block
        # Removes the directory at *path*. Fails if *path* does not exist or is a file.
        matchpi %{(delete dir path_string ⍊ recursive⋮ false)}, path: NormalPath do
          DeleteDir.new(path, recursive.true?)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # (delete dir if exists path_string ⍊ recursive⋮ false)
        #
        # |@key recursive
        # Whether to delete recursively (e.g. `rm` vs. `rm -r`). If `false`, fails
        # if *path* is a nonempty directory.
        #
        # |@block
        # Removes the directory at *path* if present. Fails if *path* is a file.
        matchpi %{(delete dir if exists path_string ⍊ recursive⋮ false)}, path: NormalPath do
          DeleteDirIfExists.new(path, recursive.true?)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [move src_string to dst_string]
        #
        # |@block
        # Moves a file system entry from a source path *src* to a destination path
        # *dst*. If *dst* is on another device, *src* is copied to *dst* and removed.
        matchpi %{[move src_string to dst_string]}, src: NormalPath, dst: NormalPath do
          Move.new(src, dst)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [create temporary file in parent_string]
        #
        # |@key parent
        # Path to the directory where the temporary file should be created.
        #
        # |@block
        # Creates a temporary file in the given *parent* directory. Its name can
        # be retrieved from the `rack.fs.response` (`present`). You are responsible
        # for eventually removing it.
        matchpi %{[create temporary file in parent_string]}, parent: NormalPath do
          CreateTmpFile.new(parent)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [overwrite file path_string content_blob]
        # [overwrite file path_string content_string]
        #
        # |@block
        # Overwrites the contents of the file at *path* with *content*. The file
        # will be created if missing.
        matchpi %{[overwrite file path_string content_]}, path: NormalPath do |content|
          continue unless content = content.as_s? || content.as_blob?

          OverwriteFile.new(path, content)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [append to file path_string content_blob]
        # [append to file path_string content_string]
        #
        # |@block
        # Appends *content* to the file at *path*. The file will be created
        # if missing.
        matchpi %{[append to file path_string content_]}, path: NormalPath do |content|
          continue unless content = content.as_s? || content.as_blob?

          AppendFile.new(path, content)
        end

        # |@ rack.fs.request
        #
        # |@pattern
        # [read text file path_string]
        # [read binary file path_string]
        #
        # |@block
        # Reads the content of the file at *path* if it exists. The result is a string
        # for `text` files (encoded using UTF-8; an error otherwise), and a blob
        # for `binary` files.

        matchpi %{[read text file path_string]}, path: NormalPath do
          ReadTextFile.new(path)
        end

        matchpi %{[read binary file path_string]}, path: NormalPath do
          ReadBinaryFile.new(path)
        end

        otherwise { }
      end
    end

    alias Response = Present | Absent | Moved | Read | Wrote | Err

    defrecord Present, path : NormalPath
    defrecord Absent, path : NormalPath
    defrecord Moved, src : NormalPath, dst : NormalPath
    defrecord Read, path : NormalPath, content : Term::Str | Term::Blob
    defrecord Wrote, path : NormalPath
    defrecord Err, detail : String

    def self.transcribe(response : Response) : Term
      # |@ rack.fs.response
      #
      # |@summary
      # The possible responses to a request.
      case response
      in Present
        # |@ rack.fs.response
        #
        # |@pattern
        # (ok (present path_string))
        #
        # |@block
        # Confirms that an action was carried out after which *path* was
        # observed present.
        Term.of(:ok, {:present, response.path})
      in Absent
        # |@ rack.fs.response
        #
        # |@pattern
        # (ok (absent path_string))
        #
        # |@block
        # Confirms that an action was carried out after which *path* was
        # observed absent.
        Term.of(:ok, {:absent, response.path})
      in Read
        # |@ rack.fs.response
        #
        # |@pattern
        # (ok (read path_string content_string))
        # (ok (read path_string content_blob))
        #
        # |@block
        # Shows the content of the file at *path* at some indeterminate point
        # in the past.
        Term.of(:ok, {:read, response.path, response.content})
      in Wrote
        # |@ rack.fs.response
        #
        # |@pattern
        # (ok (wrote path_string))
        #
        # |@block
        # Confirms that an action was carried out after which the content
        # of the file at *path* changed.
        Term.of(:ok, {:wrote, response.path})
      in Moved
        # |@ rack.fs.response
        #
        # |@pattern
        # (ok (moved src_string dst_string))
        #
        # |@block
        # Confirms that the file system entry at *src* was moved to *dst*.
        Term.of(:ok, {:moved, response.src, response.dst})
      in Err
        # |@ rack.fs.response
        #
        # |@pattern
        # (err detail_string)
        #
        # |@block
        # Signals that an error occurred while performing a request. *detail*
        # explains the reason.
        Term.of(:err, response.detail)
      end
    end

    def self.execute(task : CreateFile, ping : D7::TaskBoard::Ping) : Response
      FileUtils.touch(task.path.unwrap)
      Present.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : CreateDir, ping : D7::TaskBoard::Ping) : Response
      FileUtils.mkdir(task.path.unwrap)
      Present.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : CreateDirIfMissing, ping : D7::TaskBoard::Ping) : Response
      FileUtils.mkdir(task.path.unwrap)
      Present.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : DeleteFile, ping : D7::TaskBoard::Ping) : Response
      FileUtils.rm(task.path.unwrap)
      Absent.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : DeleteFileIfExists, ping : D7::TaskBoard::Ping) : Response
      FileUtils.rm_f(task.path.unwrap)
      Absent.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : DeleteDir, ping : D7::TaskBoard::Ping) : Response
      if task.recursive
        FileUtils.rm_r(task.path.unwrap)
      else
        FileUtils.rmdir(task.path.unwrap)
      end

      Absent.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : DeleteDirIfExists, ping : D7::TaskBoard::Ping) : Response
      begin
        if task.recursive
          FileUtils.rm_r(task.path.unwrap)
        else
          FileUtils.rmdir(task.path.unwrap)
        end
      rescue File::NotFoundError
      end

      Absent.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : Move, ping : D7::TaskBoard::Ping) : Response
      FileUtils.mv(task.src.unwrap, task.dst.unwrap)
      Moved.new(task.src, task.dst)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : CreateTmpFile, ping : D7::TaskBoard::Ping) : Response
      name = File.tempname(prefix: "ww", suffix: nil, dir: task.parent.unwrap.to_s)
      File.touch(name)
      Present.new(NormalPath[task.parent / name])
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : OverwriteFile, ping : D7::TaskBoard::Ping) : Response
      case content = task.content
      in Term::Str # Assume UTF-8 / text
        File.write(task.path.unwrap, content.to(String), mode: "w")
      in Term::Blob # Assume binary data
        File.open(task.path.unwrap, mode: "wb", &.write(content.to_slice))
      end

      Wrote.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : AppendFile, ping : D7::TaskBoard::Ping) : Response
      case content = task.content
      in Term::Str # Assume UTF-8 / text
        File.write(task.path.unwrap, content.to(String), mode: "a")
      in Term::Blob # Assume binary data
        File.open(task.path.unwrap, mode: "ab", &.write(content.to_slice))
      end

      Wrote.new(task.path)
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    # :nodoc:
    READ_BLOCK_SIZE = 4096

    def self.execute(task : ReadTextFile, ping : D7::TaskBoard::Ping) : Response
      File.open(task.path.unwrap, "r") do |src|
        content = String.build do |dst|
          # Read in blocks to have an opportunity to call ping.
          loop do
            ping.call

            finished = false

            READ_BLOCK_SIZE.times do
              unless chr = src.read_char
                finished = true
                break
              end

              dst << chr
            end

            break if finished
          end
        end

        Read.new(task.path, Term[content])
      end
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end

    def self.execute(task : ReadBinaryFile, ping : D7::TaskBoard::Ping) : Response
      File.open(task.path.unwrap, "r") do |src|
        content = Term::Blob.build do |dst|
          # Read in blocks to have an opportunity to call ping.
          loop do
            ping.call

            bytes_read = IO.copy(src, dst, limit: READ_BLOCK_SIZE)
            break if bytes_read.zero?
          end
        end

        Read.new(task.path, Term[content])
      end
    rescue e : File::Error | IO::Error
      Err.new(e.message || "internal error")
    end
  end

  module Timing
    include IGnd

    UNIVERSE = Random::Secure.rand(UInt64)

    def merge_policy : MergePolicy
      MergeDiff.new(0u32)
    end

    def each_edge(& : Term ->) : Nil
    end

    defcase Sequencer,
      original : Term,
      period : Time::Span,
      includes: {Timing}

    defcase Ticker,
      original : Term,
      period : Time::Span,
      step : Term::Num,
      state : Term::Num,
      includes: {Timing}

    defcase Timesource,
      original : Term,
      period : Time::Span,
      target : Term,
      message : Term,
      includes: {Timing}

    class Timesource
      def each_edge(& : Term ->) : Nil
        yield target
      end
    end

    defcase Lfo,
      original : Term,
      period : Time::Span,
      waveform : Waveform,
      includes: {Timing}

    defcase PendingTimer,
      original : Term,
      delay : Time::Span,
      includes: {Timing}

    defcase WaitingTimer,
      original : Term,
      delay : Time::Span,
      universe : UInt64,
      deadline : Time::Span,
      includes: {Timing}

    defcase PendingCountdown,
      original : Term,
      delay : Time::Span,
      rate : Time::Span,
      includes: {Timing}

    defcase WaitingCountdown,
      original : Term,
      delay : Time::Span,
      rate : Time::Span,
      universe : UInt64,
      deadline : Time::Span,
      includes: {Timing}

    defcase Clock,
      original : Term,
      rate : Time::Span,
      format : Time::Format,
      location : Time::Location,
      includes: {Timing}

    def self.crossingcnt(checkpoint : Time::Span, now : Time::Span, period : Time::Span) : Int64
      if period.zero?
        # Zero period should make it always cross.
        1i64
      else
        previous = (checkpoint / period).floor.to_i64
        current = (now / period).floor.to_i64
        current - previous
      end
    end

    def self.freq?(term : Term) : Float64?
      Term.case(term) do
        # |@ rack.lfo.freq
        #
        # |@pattern
        # (±freq hz)
        # (±freq Hz)
        #
        # |@block
        # Specifies a frequency in hertz.
        #
        # |@example
        # ```wwml
        # (440 Hz)
        # ```
        matchpi %{(±freq hz)}, %{(±freq Hz)} do
          freq.to(Float64)
        end

        # |@ rack.lfo.freq
        #
        # |@pattern
        # (±freq khz)
        # (±freq kHz)
        #
        # |@block
        # Specifies a frequency in kilohertz.
        #
        # |@example
        # ```wwml
        # (10 kHz)
        # ```
        matchpi %{(±freq khz)}, %{(±freq kHz)} do
          freq.to(Float64) * 1000
        end

        otherwise { }
      end
    end

    alias Waveform = SinWave | SawUpWave | SawDnWave | TriWave | PulseWave

    defrecord SinWave, freq : Float64, offset : Float64, amplitude : Float64
    defrecord SawUpWave, freq : Float64
    defrecord SawDnWave, freq : Float64
    defrecord TriWave, freq : Float64
    defrecord PulseWave, freq : Float64, duty : Float64

    def self.waveform?(waveform : Term) : Waveform?
      Term.case(waveform) do
        # |@ rack.lfo.waveform
        #
        # |@pattern
        # (sin freq_ ⍊ offset⋮ 1/2 amplitude⋮ 1/2)
        #
        # |@key freq rack.lfo.freq
        # The frequency of the wave.
        #
        # |@key offset
        # The offset of the wave.
        #
        # |@key amplitude
        # The amplitude of the wave.
        #
        # |@block
        # Defines a sine waveform.
        #
        # |@example
        # ```wwml
        # (sin (440 Hz))
        # (sin (1 Hz) offset: 0 amplitude: 1)
        # ```
        matchpi(
          %{(sin freqQ_ ⍊ offset⋮ 1/2 amplitude⋮ 1/2)},
          offset: Float64,
          amplitude: Float64,
        ) do
          next unless freq = freq?(freqQ)

          SinWave.new(freq, offset, amplitude)
        end

        # |@ rack.lfo.waveform
        #
        # |@pattern
        # (saw freq_ ⍊ direction_: (%optional up (%any up dn)))
        #
        # |@key freq rack.freq
        # The frequency of the wave.
        #
        # |@key direction
        # Whether the saw should be rising (`up`) or falling (`dn`).
        #
        # |@block
        # Defines a sawtooth waveform.
        #
        # |@example
        # ```wwml
        # (saw (10 Hz))
        # (saw (10 Hz) direction: dn)
        # ```

        matchpi %{(saw freqQ_ ⍊ direction: (%optional up up))} do
          next unless freq = freq?(freqQ)

          SawUpWave.new(freq)
        end

        matchpi %{(saw freqQ_ ⍊ direction: dn)} do
          next unless freq = freq?(freqQ)

          SawDnWave.new(freq)
        end

        # |@ rack.lfo.waveform
        #
        # |@pattern
        # [tri freq_]
        #
        # |@key freq rack.freq
        # The frequency of the wave.
        #
        # |@block
        # Defines a triangle waveform.
        #
        # |@example
        # ```wwml
        # (tri (10 Hz))
        # ```
        matchpi %{[tri freqQ_]} do
          next unless freq = freq?(freqQ)

          TriWave.new(freq)
        end

        # |@ rack.lfo.waveform
        #
        # |@pattern
        # (pulse freq_ ⍊ duty⋮ 1/2)
        #
        # |@key freq rack.freq
        # The frequency of the wave.
        #
        # |@key duty
        # The fraction of each cycle spent in the high state.
        #
        # |@block
        # Defines a pulse waveform.
        #
        # |@example
        # ```wwml
        # (pulse (1 Hz))
        # (pulse (10 Hz) duty: 1/5)
        # ```
        matchpi %{(pulse freqQ_ ⍊ duty⋮ 1/2)}, duty: Float64 do
          next unless freq = freq?(freqQ)

          PulseWave.new(freq, duty)
        end

        otherwise { }
      end
    end

    def self.sample(waveform : SinWave, now : Time::Span) : Float64?
      # Calculate how many cycles have passed.
      cycles = now.total_seconds * waveform.freq
      # Take the fractional part, e.g. 1.⏏23⏏, telling us how far we are
      # into the current cycle.
      phase = cycles - cycles.floor
      waveform.offset + waveform.amplitude*Math.sin(Math::TAU * phase)
    end

    def self.sample(waveform : SawUpWave, now : Time::Span) : Float64?
      cycles = now.total_seconds * waveform.freq
      phase = cycles - cycles.floor
      phase # Ramp is just phase
    end

    def self.sample(waveform : SawDnWave, now : Time::Span) : Float64?
      cycles = now.total_seconds * waveform.freq
      phase = cycles - cycles.floor
      1.0f64 - phase
    end

    def self.sample(waveform : TriWave, now : Time::Span) : Float64?
      cycles = now.total_seconds * waveform.freq
      phase = cycles - cycles.floor
      1.0f64 - (2*phase - 1.0f64).abs
    end

    def self.sample(waveform : PulseWave, now : Time::Span) : Float64?
      cycles = now.total_seconds * waveform.freq
      phase = cycles - cycles.floor
      phase < waveform.duty ? 0.0f64 : 1.0f64
    end

    private def freq?(term : Term) : Float64?
      Term.case(term) do
        # |@ rack.lfo.freq
        #
        # |@pattern
        # (±freq hz)
        # (±freq Hz)
        #
        # |@block
        # Specifies a frequency in hertz.
        #
        # |@example
        # ```wwml
        # (440 Hz)
        # ```
        matchpi %{(±freq hz)}, %{(±freq Hz)} do
          freq.to(Float64)
        end

        # |@ rack.lfo.freq
        #
        # |@pattern
        # (±freq khz)
        # (±freq kHz)
        #
        # |@block
        # Specifies a frequency in kilohertz.
        #
        # |@example
        # ```wwml
        # (10 kHz)
        # ```
        matchpi %{(±freq khz)}, %{(±freq kHz)} do
          freq.to(Float64) * 1000
        end

        otherwise { }
      end
    end

    def self.location?(term : Term) : Time::Location?
      Term.case(term) do
        matchpi %{local} do
          Time::Location.local
        end

        matchpi %{utc} do
          Time::Location::UTC
        end

        matchpi %{_string} do
          Time::Location.load?(term.to(String))
        end

        otherwise { }
      end
    rescue Time::Error
      # TODO: Log it or something...
    end
  end

  # Represents the toplevel circuit.
  defcase Root,
    original_dict : Term::Dict,
    children : Slice(Node),
    children_summary : Summary,
    includes: {ICircuit}

  class Root
    def self.new(original_dict : Term::Dict, children : Slice(Node)) : Root
      children_summary = NeoRack.summarize(children)
      new(original_dict, children, children_summary)
    end

    def range : Range(UInt32, UInt32)
      0u32...original_dict.uitemsize
    end

    def leaf : Node
      Inert.new(original)
    end

    def map(& : Node -> Node) : Root
      Root.new(@original_dict, @children.to_readonly_slice { |child| (yield child).as(Node) })
    end
  end

  # Represents the summary of a parent node. We use such summaries to
  # speed up search and traversal. Summaries allow us to take all sorts
  # of shortcuts.
  defrecord Summary,
    maxlevel : UInt32,
    gnd_count : UInt32,
    automaton_count : UInt32,
    content : NodeSignature,
    copying: true

  struct Summary
    def self.new : Summary
      new(maxlevel: 0u32, gnd_count: 0u32, automaton_count: 0u32, content: NodeSignature::None)
    end

    def self.with(**kwargs) : Summary
      new.copy_with(**kwargs)
    end

    def self.union(a : Summary, b : Summary) : Summary
      maxlevel = Math.max(a.maxlevel, b.maxlevel)
      gnd_count = a.gnd_count + b.gnd_count
      automaton_count = a.automaton_count + b.automaton_count
      content = a.content | b.content
      new(maxlevel, gnd_count, automaton_count, content)
    end

    def includes?(cls : T.class) forall T
      !(@content & NodeSignature.of(T)).none?
    end
  end

  def summarize(node : IInert) : Summary
    Summary.new
  end

  def summarize(node : IGnd) : Summary
    Summary.with(gnd_count: 1u32, content: NodeSignature.of(node.class))
  end

  def summarize(node : Mixture | Scope | Guard) : Summary
    summarize(node.successor)
  end

  def summarize(node : ICircuit) : Summary
    summary = node.children_summary
    summary.copy_with(maxlevel: summary.maxlevel + 1)
  end

  def summarize(node : IAutonomy) : Summary
    Summary.with(automaton_count: 1u32)
  end

  def summarize(node : IGroup) : Summary
    summary = node.children_summary
    summary.copy_with(content: summary.content | NodeSignature.of(node.class))
  end

  def summarize(nodes : Slice(Node)) : Summary
    nodes.reduce(Summary.new) do |summary, node|
      Summary.union(summary, summarize(node))
    end
  end

  alias RecognizeCache = DependentCache(Term, Node)
  alias RecognizeProc = RecognizeCache, Term, RecognizeProc -> Node

  def recognize(recognize_cache : RecognizeCache, original : Term) : Root | Inert
    unless document = original.as_d?
      return Inert.new(original)
    end

    rec = ->(recognize_cache : RecognizeCache, member : Term, this : RecognizeProc) do
      recognize(recognize_cache, member, this).as(Node)
    end

    children = document.items.to_readonly_slice do |node|
      recognize(recognize_cache, node, rec).as(Node)
    end

    Root.new(document, children)
  end

  # :nodoc:
  SYM_SEQ = Term[:seq]
  # :nodoc:
  SYM_EXCLUSIVE = Term[:exclusive]
  # :nodoc:
  SYM_SURFACE = Term[:surface]
  # :nodoc:
  SYM_ARENA = Term[:arena]
  # :nodoc:
  SYM_SPACE = Term[:space]

  def merge_policy?(name : Term::Sym) : MergePolicy?
    # |@ rack.[merge policy]
    #
    # |@summary
    # Ways to handle conflicting patches to `rack.cell`s and some other nodes.
    #
    # |@block
    # A node's *merge policy* is its preferred way of handling many simultaneous
    # patches to itself (i.e., when two or more patches are made to the same node
    # within the same tick).
    #
    # The simplest merge policy (and the default one for most nodes) is `exclusive`,
    # which basically forbids making many simultaneous patches to a node.
    #
    # The smarest merge strategies involve the term diff algorithm. Do note that
    # *conflicting inserts* in particular (inserts at the same spot made by different
    # parties) can be problematic: batch order is retained; but batches are ordered
    # among themselves lexicographically.

    case name
    when SYM_SEQ
      # |@ rack.[merge policy]
      #
      # |@pattern
      # seq
      #
      # |@block
      # Accepts the patch that contains the *minimum* candidate term among
      # competing patches. Minimum is not necessarily lexicographical minimum;
      # the kind of minimum we use varies with term type. For numbers, it'd be
      # the arithmetic minimum. For strings, it'd be the lexicographical minimum.
      # And so on. See `ww.[term order]` to learn more.
      MergeSeq.new
    when SYM_EXCLUSIVE
      # |@ rack.[merge policy]
      #
      # |@pattern
      # exclusive
      #
      # |@block
      # Accepts a single patch. If there are two or more competing patches, rejects
      # all of them.
      #
      # NOTE: This policy is the default policy for the vast majority of nodes.
      MergeDiff.new(depth_limit: 0u32)
    when SYM_SURFACE
      # |@ rack.[merge policy]
      #
      # |@pattern
      # surface
      #
      # |@block
      # Smart (diff-based) merge of competing patches to the node itself (e.g. `(foo 1 2)` ->
      # `(foo 1 2 ⏏3⏏)`; its "surface"). Deeper dictionaries are treated atomically.
      MergeDiff.new(depth_limit: 1u32)
    when SYM_ARENA
      # |@ rack.[merge policy]
      #
      # |@pattern
      # arena
      #
      # |@block
      # Smart (diff-based) merge of competing patches to the node itself (e.g. `(foo 1 2)` ->
      # `(foo 1 2 ⏏3⏏)`; its "surface"), or to its immediate dictionary children (e.g. `(foo (1 2))` ->
      # `(foo (1 2 ⏏3⏏))`). Deeper dictionaries are treated atomically.
      MergeDiff.new(depth_limit: 2u32)
    when SYM_SPACE
      # |@ rack.[merge policy]
      #
      # |@pattern
      # space
      #
      # |@block
      # Smart (diff-based) effectively unlimited deep merge of patches.
      MergeDiff.new(depth_limit: UInt32::MAX)
    end
  end

  alias Replacer = ZeroReplacer | OneReplacer | ManyReplacer

  defrecord ZeroReplacer
  defrecord OneReplacer, update : Term? -> Term, part : Term?, smart: true
  defrecord ManyReplacer, update : Term? -> Term

  private def replacer(pattern : Term, capture : Term, whole : Term, *, splice : Bool = false) : Replacer
    env_trace_lists = M1.matches_and_traces(Term[], M1.operator(pattern), whole)
    if env_trace_lists.empty?
      return ZeroReplacer.new
    end

    if env_trace_list = env_trace_lists.single?
      env, log_list = env_trace_list
      part0 = env[capture]?
    end

    update = ->(part1 : Term?) do
      case part1
      in Nil
        backspec = Term[].with({capture}, Term[]) # {(capture): ()}
      in Term
        key = capture
        if splice
          key = Term.of({capture})
        end

        if part1.type.dict? || part1.type.symbol?
          backspec = Term[].with(key, {:"^verbatim", part1})
        else
          # Do not waste time doing ^verbatim stuff on terms that cannot cause
          # us trouble: booleans, numbers, etc.
          backspec = Term[].with(key, part1)
        end
      end

      backmap = {env_trace_lists, backspec}
      rep = M1.backmapR({backmap}, whole)
      Term.collapse(rep)
    end

    if env_trace_list.nil?
      assert part0.nil?
      return ManyReplacer.new(update)
    end

    OneReplacer.new(update, part0)
  end

  private def keyset(rules : Enumerable(Term)) : Set(Term)
    keys = Set(Term).new

    rules.each do |rule|
      Term.case(rule) do
        matchpi %{[(%any rule backmap) (%'%layer _ side_dict) _]} do
          side.each_entry(in: Term::Dict.pairspart) do |key, _|
            next unless keysym = key.as_sym?
            next if keysym.prefixed_by?('#')

            keys << key
          end
        end

        matchpi %{[(%any rule backmap) pattern_dict _]} do
          pattern.each_entry(in: Term::Dict.pairspart) do |key, _|
            next unless keysym = key.as_sym?
            next if keysym.prefixed_by?('#')

            keys << key
          end
        end

        otherwise { }
      end
    end

    keys
  end

  def recognize(recognize_cache : RecognizeCache, original : Term, rec : RecognizeProc) : Node
    recognize_cache.put_if_absent(original) { recognize!(recognize_cache, original, rec) }
  end

  private def recognize!(recognize_cache : RecognizeCache, original : Term, rec : RecognizeProc) : Node
    Term.case(original) do
      matchpi %{[cell @edge_]} do
        policy = MergeDiff.new(0u32)
        Cell::Empty.new(original, policy, edge)
      end

      matchpi %{[cell @edge_ term_]} do
        policy = MergeDiff.new(0u32)
        Cell::Occupied.new(original, policy, edge, term)
      end

      matchpiT %{[cell (policyQ_symbol @edge_)]} do
        continue unless policy = merge_policy?(policyQ)

        Cell::Empty.new(original, policy, edge)
      end

      matchpiT %{[cell (policyQ_symbol @edge_) term_]} do
        continue unless policy = merge_policy?(policyQ)

        Cell::Occupied.new(original, policy, edge, term)
      end

      matchpi %{[cell (edge←(%'edge capture_) pattern_) whole0_]} do
        case replacer = replacer(pattern, capture, whole0)
        in ZeroReplacer
          continue
        in OneReplacer
          part0 = replacer.part?
        in ManyReplacer
        end

        defn = Term.of(:cell, edge, part0)

        fold = ->(mix : Term) do
          Term.case(mix) do
            matchpi %{(cell @_)} do
              if part0
                Term.morph(original, {2, replacer.update.call(nil)})
              else
                original # Was absent, is absent.
              end
            end

            matchpi %{(cell @_ part1_)} do
              if part0 == part1
                original # Was present, is present, is equal.
              else
                Term.morph(original, {2, replacer.update.call(part1)})
              end
            end
          end
        end

        Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
      end

      matchpi %{[feed (%group edges_ (%past @_ min: 3))]} do
        defn = Term::Dict.build do |commit|
          commit << :group
          edges.items.each_cons_pair do |a, b|
            commit << Term.of(:feed, a, b)
          end
        end

        Mixture.new(original, rec.call(recognize_cache, Term.of(defn), rec), ->(mix : Term) { original })
      end

      matchpi(
        %{[feed (srcQ_ items) dstQ_]},
        %{[feed (not _*) (srcQ_ items) dstQ_]},
      ) do
        continue unless src = Feed.src?(srcQ)
        continue unless dst = Feed.dst_list?(dstQ)

        inhibitors = Slice(Feed::Inhibitor).empty
        if original.itemsize == 4 # Second pattern matched.
          _, not = original
          inhibitorsQ = not.items.move(1)
          continue unless inhibitors = Feed.inhibitor_list?(inhibitorsQ)
        end

        Feed::Distribute.new(original, inhibitors, src, dst)
      end

      matchpi(
        %{[feed srcQ_ dstQ_]},
        %{[feed (not _*) srcQ_ dstQ_]},
      ) do
        inhibitors = Slice(Feed::Inhibitor).empty
        if original.itemsize == 4 # Second pattern matched.
          _, not = original
          inhibitorsQ = not.items.move(1)
          continue unless inhibitors = Feed.inhibitor_list?(inhibitorsQ)
        end

        src = Feed.src?(srcQ)
        dst = Feed.dst?(dstQ)

        # (feed @u @v)
        if src && dst
          return Feed::Transfer.new(original, inhibitors, src, dst)
        end

        # (feed @u (@v @w))
        if src && (dst_list = Feed.dst_list?(dstQ))
          return Feed::Broadcast.new(original, inhibitors, src, dst_list)
        end

        # (feed (@u @v) @w)
        if dst && (src_list = Feed.src_list?(srcQ))
          return Feed::Aggregate.new(original, inhibitors, src_list, dst)
        end

        # (feed (@x @y) (@a @b))
        pass do
          next unless src_list = Feed.src_list?(srcQ)
          next unless dst_list = Feed.dst_list?(dstQ)
          next unless src_list.size == dst_list.size
          return Feed::ParallelTransfer.new(original, inhibitors, src_list, dst_list)
        end

        continue
      end

      matchpi %{[discard @edge_]} do
        Discard::Unconditional.new(original, edge)
      end

      matchpi %{[discard @edge_ patternQ_]} do
        pattern = M1.operator(patternQ)
        Discard::Conditional.new(original, edge, pattern)
      end

      matchpi %{[reading @edge_]}, %{[reading @edge_ _]} do
        Reading.new(original, edge)
      end

      matchpi %{[group _*]} do
        childrenQ = original.items.move(1)
        children = childrenQ.to_readonly_slice { |childQ| rec.call(recognize_cache, childQ, rec) }
        Group.new(original.as_d, children, 1u32...original.uitemsize)
      end

      matchpiT %{[module bindings_dict _*]} do
        scope = ScopeClosedExcept.new(bindings)
        childrenQ = original.items.move(2)
        children = childrenQ.to_readonly_slice { |childQ| rec.call(recognize_cache, childQ, rec) }
        Scope.new(original, scope, Group.new(original.as_d, children, 2u32...original.uitemsize))
      end

      matchpi %{[locals localsQ←((%past @_ min: 0)) _*]} do
        locals = localsQ.items.to_readonly_slice(&.itself)
        scope = ScopeOpenExcept.new(locals)
        childrenQ = original.items.move(2)
        children = childrenQ.to_readonly_slice { |childQ| rec.call(recognize_cache, childQ, rec) }
        Scope.new(original, scope, Group.new(original.as_d, children, 2u32...original.uitemsize))
      end

      matchpi %{[transfer (@src_ patternQ_ @dst_) templateQ_]} do
        pattern = M1.operator(patternQ)
        template = Alloy.compile(Alloy.recognize(templateQ))
        Transfer::One.new(original, src, dst, pattern, template)
      end

      matchpi %{[transfer (srcsQ←((%past @_ min: 1)) patternQ_ @dst_) templateQ_]} do
        inhibitors = Slice(Term).empty
        srcs = srcsQ.items.to_readonly_slice(&.itself)
        pattern = M1.operator(patternQ)
        template = Alloy.compile(Alloy.recognize(templateQ))
        Transfer::Many.new(original, inhibitors, srcs, dst, pattern, template)
      end

      matchpi %{[transfer (not←(not (%past @_ min: 0)) srcsQ←((%past @_ min: 1)) patternQ_ @dst_) templateQ_]} do
        inhibitorsQ = not.items.move(1)
        inhibitors = inhibitorsQ.to_readonly_slice(&.itself)
        srcs = srcsQ.items.to_readonly_slice(&.itself)
        pattern = M1.operator(patternQ)
        template = Alloy.compile(Alloy.recognize(templateQ))
        Transfer::Many.new(original, inhibitors, srcs, dst, pattern, template)
      end

      matchpi %{[view (@src_ patternQ_ @dst_) templateQ_]} do
        pattern = M1.operator(patternQ)
        template = Alloy.compile(Alloy.recognize(templateQ))
        View::One.new(original, src, dst, pattern, template)
      end

      matchpi %{[view (srcsQ←((%past @_ min: 1)) patternQ_ @dst_) templateQ_]} do
        srcs = srcsQ.items.to_readonly_slice(&.itself)
        pattern = M1.operator(patternQ)
        template = Alloy.compile(Alloy.recognize(templateQ))
        View::Many.new(original, srcs, dst, pattern, template)
      end

      matchpi %{[view (@src_ patternQ_ (over @dst_)) templateQ_]} do
        pattern = M1.operator(patternQ)
        template = Alloy.compile(Alloy.recognize(templateQ))
        View::OneOver.new(original, src, dst, pattern, template)
      end

      matchpi %{[view (srcsQ←((%past @_ min: 1)) patternQ_ (over @dst_)) templateQ_]} do
        srcs = srcsQ.items.to_readonly_slice(&.itself)
        pattern = M1.operator(patternQ)
        template = Alloy.compile(Alloy.recognize(templateQ))
        View::ManyOver.new(original, srcs, dst, pattern, template)
      end

      matchpiT %{[delay countdown←(%number u32) successor_]} do
        Delay.new(original, countdown, successor)
      end

      matchpi %{[device _*]} do
        childrenQ = original.items.move(1)
        children = childrenQ.to_readonly_slice { |childQ| rec.call(recognize_cache, childQ, rec) }
        Device.new(original.as_d, children, 1u32...original.uitemsize)
      end

      matchpi(
        %{[circuit @header_ children0_*]},
        %{[circuit header←(_symbol @_) children0_*]},
      ) do
        if children0.empty?
          defn = Term.of(:cell, header)
        else
          defn = Term.of(:cell, header, children0)
        end

        fold = ->(mix : Term) do
          Term.of_case(mix) do
            matchpi %{(cell _ children1←[_*])} do
              original.replace(2...original.itemsize, Term.rep(children1.items))
            end

            otherwise do
              original.replace(2...original.itemsize, Term.rep)
            end
          end
        end

        children = children0.items.to_readonly_slice { |child| rec.call(recognize_cache, child, rec) }
        range = 2u32...original.uitemsize
        leaf = Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
        Circuit.new(original.as_d, children, range, leaf)
      end

      matchpi %{[circuit (edge←(%'edge capture_) pattern_) children0_*]} do
        replacer = replacer(pattern, capture, children0)
        continue if replacer.is_a?(ZeroReplacer)

        case replacer
        in OneReplacer
          defn = Term.of(:cell, edge, replacer.part?)
        in ManyReplacer
          defn = Term.of(:cell, edge)
        end

        fold = ->(mix : Term) do
          children1 = Term.case(mix) do
            matchpi %{(cell @_)} { replacer.update.call(nil) }
            matchpi %{(cell @_ part1_)} { replacer.update.call(part1) }
          end

          Term.of(original.replace(2...original.itemsize, Term.rep(children1.items)))
        end

        children = children0.items.to_readonly_slice { |child| rec.call(recognize_cache, child, rec) }
        range = 2u32...original.uitemsize
        leaf = Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
        Circuit.new(original.as_d, children, range, leaf)
      end

      matchpi %{[node @header_]}, %{[node header←(_symbol @_)]} do
        defn = Term.of(:cell, header)

        fold = ->(mix : Term) do
          Term.of_case(mix) do
            matchpi %{(cell _)} { original }
            matchpi %{(cell _ value1_)} { Term.morph(original, {2, value1}) }
          end
        end

        Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
      end

      matchpi(
        %{[node @header_ child0_]},
        %{[node header←(_symbol @_) child0_]},
      ) do
        defn = Term.of(:group, {:cell, header, child0}, {:group, child0})

        fold = ->(mix : Term) do
          Term.case(mix) do
            # New child arrived. Higher priority.
            matchpi %{(group (cell _ child1_) _)} do
              continue if child0 == child1

              Term.morph(original, {2, child1})
            end

            # Cell was erased. Higher priority.
            matchpi %{(group (cell _) _)} do
              Term.morph(original, {2, nil})
            end

            # If cell did not change, update from group content.
            matchpi %{(group _ (group child1_))} do
              Term.morph(original, {2, child1})
            end
          end
        end

        Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
      end

      matchpi(
        %{[node (spec←edge←(%'edge capture_) pattern_) child0_]},
        %{[node (spec←(edge←(%'edge capture_)) pattern_) child0_]},
      ) do
        case replacer = replacer(pattern, capture, child0, splice: !spec.same?(edge))
        in ZeroReplacer
          defn = child0
        in OneReplacer
          defn = Term.of(:group, {:cell, edge, replacer.part?}, child0)
        in ManyReplacer
          defn = Term.of(:group, {:cell, edge}, child0)
        end

        fold = ->(mix : Term) do
          if replacer.is_a?(ZeroReplacer)
            return Term.morph(original, {2, mix})
          end

          Term.case(mix) do
            matchpi %{(group (cell _) child1_)} do
              if replacer.is_a?(OneReplacer) && replacer.part?.nil?
                Term.morph(original, {2, child1})
              else
                Term.morph(original, {2, replacer.update.call(nil)})
              end
            end

            matchpi %{(group (cell _ part1_) child1_)} do
              if replacer.is_a?(OneReplacer) && replacer.part? == part1
                Term.morph(original, {2, child1})
              else
                Term.morph(original, {2, replacer.update.call(part1)})
              end
            end
          end
        end

        Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
      end

      matchpi(
        %{[nodes @header_ children0_*]},
        %{[nodes header←(_symbol @_) children0_*]},
      ) do
        if children0.empty?
          defn = Term.morph(original, {0, :group}, {1, {:cell, header}})
        else
          defn = Term.morph(original, {0, :group}, {1, {:cell, header, children0}})
        end

        fold = ->(mix : Term) do
          Term.of_case(mix) do
            matchpi %{[group (cell _ children1←(_*)) _*]} do
              continue if children0 == children1

              original.replace(2...original.itemsize, Term.rep(children1.items))
            end

            matchpi %{[group _ children1_*]} do
              original.replace(2...original.itemsize, Term.rep(children1.items))
            end

            matchpi %{[group (cell _) _*]} do
              original.replace(2...original.itemsize, Term.rep)
            end
          end
        end

        Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
      end

      matchpi(<<-'WWML', min: Int32) do
      [queue (@front_ @back_
               ⍊ min_: (%optional 1 (%number +i32!))
                 max_: (%optional ∞ (%any° (%number +i32!) ∞)))
        buffer0_dict]
      WWML
        defn = Term::Dict.build do |commit|
          commit << :group

          # Check if we can dequeue.
          if 0 < min <= buffer0.itemsize
            commit << Term.of(:cell, {:seq, front}, buffer0.items.first, front: true)
          end

          # Check if we can enqueue.
          if max == Term.of(:∞) || buffer0.itemsize < max.to(Int32)
            commit << Term.of(:cell, {:seq, back}, back: true)
          end
        end

        fold = ->(mix : Term) do
          buffer1 = buffer0

          Term.case(mix) do
            # Dequeue.
            matchpi %{⟨(cell _ ⍊ front)⟩} do
              buffer1 = buffer1.rest
              continue
            end

            # Sync.
            matchpi %{⟨(cell _ x_ ⍊ front)⟩} do
              buffer1 = Term.morph(buffer1, {0, x})
              continue
            end

            # Enqueue.
            matchpi %{⟨(cell _ x_ ⍊ back)⟩} do
              buffer1 = buffer1.append(x)
              continue
            end

            otherwise { }
          end

          Term.morph(original, {2, buffer1})
        end

        Mixture.new(original, rec.call(recognize_cache, Term.of(defn), rec), fold)
      end

      matchpi %{[guard (@edge_) _*]} do
        childrenQ = original.items.move(2)
        children = childrenQ.to_readonly_slice { |child| rec.call(recognize_cache, child, rec) }
        range = 2u32...original.uitemsize
        successor = Group.new(original.as_d, children, range)
        Guard::Empty.new(original, edge, successor)
      end

      matchpi %{[guard (@edge_ patternQ_) _*]} do
        pattern = M1.operator(patternQ)
        childrenQ = original.items.move(2)
        children = childrenQ.to_readonly_slice { |child| rec.call(recognize_cache, child, rec) }
        range = 2u32...original.uitemsize
        successor = Group.new(original.as_d, children, range)
        Guard::Occupied.new(original, edge, pattern, successor)
      end

      matchpi %{[part (@src_ dst←(%'edge capture_)) patternQ_]} do
        pattern = M1.operator(patternQ)
        Part::Backmap.new(original, src, dst, capture, pattern)
      end

      matchpi %{[part (src←(%'edge whole_) dst←(%'edge capture_)) [backmap _ _] _*]} do
        backsysQ = original.items.move(2)
        backsys = backsysQ.to_compact_readonly_slice do |item|
          Term.matchpiT?(item, %{[backmap pattern_ backspec_dict]}) do
            {M1.operator(pattern), backspec}
          end
        end
        Part::Backsys.new(original, src, dst, whole, capture, backsys)
      end

      matchpi %{[backsys @target_ _*]} do
        backsysQ = original.items.move(2)
        backsys = backsysQ.to_compact_readonly_slice do |item|
          Term.matchpiT?(item, %{[backmap pattern_ backspec_dict]}) do
            {M1.operator(pattern), backspec}
          end
        end

        Backsys::Single.new(original, target, backsys)
      end

      matchpi %{[backsys [backmap _ _] _*]} do
        keys = keyset(rules: original.items.move(1))

        # Synthesize template based on the keys we've found.
        templateQ = Term::Dict.build do |commit|
          keys.each do |key|
            commit.with(key, {:edge, key})
          end
        end
        template = EdgeTemplate.recognize(templateQ)

        backsysQ = original.items.move(1)
        backsys = backsysQ.to_compact_readonly_slice do |item|
          Term.matchpiT?(item, %{[backmap pattern_ backspec_dict]}) do
            {M1.operator(pattern), backspec}
          end
        end

        Backsys::Template.new(original, template, templateQ, backsys)
      end

      matchpiT %{[backsys templateQ_dict _*]} do
        template = EdgeTemplate.recognize(templateQ)
        backsysQ = original.items.move(2)
        backsys = backsysQ.to_compact_readonly_slice do |item|
          Term.matchpiT?(item, %{[backmap pattern_ backspec_dict]}) do
            {M1.operator(pattern), backspec}
          end
        end

        Backsys::Template.new(original, template, templateQ, backsys)
      end

      matchpi %{[log (@edge_ ⍊ limit_: (%optional 10 (%number +i32))) _*]}, limit: Int32 do
        history = original.items.move(2)

        defn = Term.of(:reading, edge)

        fold = ->(mix : Term) do
          Term.case(mix) do
            matchpi %{(reading @_ value_)} do
              continue if history.last? == value

              if limit.zero?
                original
              elsif history.size + 1 > limit
                # (log (@x limit: 2) 100 200)    300
                # (log (@x limit: 2) ⏏100⏏ 200)  300
                # (log (@x limit: 2) ⏏⏏ 200)     300
                # (log (@x limit: 2) 200 300)  <-/
                Term.of(original.replace(2, Term.rep).append(value))
              else
                Term.of(original.append(value))
              end
            end

            otherwise do
              original # Leave the original as-is if @edge is absent or unchanged.
            end
          end
        end

        Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
      end

      # NOTE: The changes node cannot detect changes made upstream of *edge*. It
      # can only detect changes made at *edge* or downstream of *edge*.
      matchpi(<<-'WWML') do
      [changes (behavior←(%any sync async) @edge_
         ⍊ limit: (%optional ∞ limitQ←(%any° ∞ (%number u32)))
           maxdepth: (%optional ∞ maxdepthQ←(%any° ∞ (%number u32))))
        ops_*]
      WWML
        case behavior
        when Term.of(:async)
          writable = true
        when Term.of(:sync)
          writable = ops.empty?
        else
          unreachable
        end

        limit = limitQ == Term.of(:∞) ? UInt32::MAX : limitQ.to(UInt32)
        maxdepth = maxdepthQ == Term.of(:∞) ? UInt32::MAX : maxdepthQ.to(UInt32)
        defn = Changes.new(original, edge, maxdepth, writable)

        # We cannot enforce changes `limit` deeper in the implementation, so we have
        # to do it here.
        fold = ->(mix : Term) do
          # *mix* is (changes _ ops_*) -- of course, assuming the Changes implementation is
          # bug-free and well-behaved.
          result = mix.as_d

          # (changes _ ⏏ops_*⏏)
          if result.uitemsize - 2 >= limit
            # (changes _ ⏏_⏏ ops_*) -> (changes _ ops_*)
            result = result.replace(2u32...result.uitemsize - limit, Term.rep)
          end

          Term.of(result)
        end

        Mixture.new(original, defn, fold)
      end

      matchpi %{[form fst←(%any° @edge_ (@edge_)) _*]} do
        policy = MergeDiff.new(0u32)
        childrenQ = original.items.move(2)
        children = childrenQ.to_readonly_slice { |child| rec.call(recognize_cache, child, rec) }
        range = 2u32...original.uitemsize
        uniform = fst != edge # @edge is not uniform, (@edge) is uniform
        Form.new(original.as_d, policy, edge, children, range, uniform)
      end

      matchpiT %{[form (policyQ_symbol fst←(%any° @edge_ (@edge_))) _*]} do
        continue unless policy = merge_policy?(policyQ)

        childrenQ = original.items.move(2)
        children = childrenQ.to_readonly_slice { |child| rec.call(recognize_cache, child, rec) }
        range = 2u32...original.uitemsize
        uniform = fst != edge # @edge is not uniform, (@edge) is uniform
        Form.new(original.as_d, policy, edge, children, range, uniform)
      end

      matchpi %{[outlet specQ_]} do
        continue unless spec = Outlet.spec?(specQ)

        Outlet::Empty.new(original, spec)
      end

      matchpi %{[outlet specQ_ value_]} do
        continue unless spec = Outlet.spec?(specQ)

        Outlet::Occupied.new(original, spec, value)
      end

      matchpi %{[field specQ_]} do
        continue unless spec = Field.spec?(specQ)

        Field::Empty.new(original, spec)
      end

      matchpi %{[field specQ_ value_]} do
        continue unless spec = Field.spec?(specQ)

        Field::Occupied.new(original, spec, value)
      end

      matchpi(
        %{[manipulator (@edge_ -> _*) child0_]},
        %{[manipulator (@edge_ _ -> _*) child0_]},
      ) do
        defn = Term.of(:group, Term.morph(original, {0, :manipulable}), child0)

        fold = ->(mix : Term) do
          Term.case(mix) do
            # New value arrived. Higher priority.
            matchpi %{(group [_ _ child1_] _)} do
              continue if child0 == child1

              Term.morph(original, {2, child1})
            end

            # New value computed.
            matchpi %{(group _ child1_)} do
              Term.morph(original, {2, child1})
            end
          end
        end

        Mixture.new(original, rec.call(recognize_cache, defn, rec), fold)
      end

      matchpi %{[manipulable (@edge_ -> partsQ_*) child_]} do
        selector = M1::Op::INSTANCE_PASS

        parts = partsQ.items.to_readonly_slice { |pattern| M1.operator(pattern) }
        winning_env_trace_lists = parts.leftmost? do |part|
          env_trace_lists = M1.matches_and_traces(Term[], part, child)
          env_trace_lists.present? ? env_trace_lists : nil
        end
        continue if winning_env_trace_lists.nil?

        Manipulable.new(original, edge, selector, winning_env_trace_lists, child)
      end

      matchpi %{[manipulable (@edge_ selectorQ_ -> partsQ_*) child_]} do
        selector = M1.operator(selectorQ)

        parts = partsQ.items.to_readonly_slice { |pattern| M1.operator(pattern) }
        winning_env_trace_lists = parts.leftmost? do |part|
          env_trace_lists = M1.matches_and_traces(Term[], part, child)
          env_trace_lists.present? ? env_trace_lists : nil
        end
        continue if winning_env_trace_lists.nil?

        Manipulable.new(original, edge, selector, winning_env_trace_lists, child)
      end

      matchpi %{[supervisor (@tasks_ @task_ keypQ_ - @pool_ ⍊ reference_: (%optional @reference @_)) template_*]} do
        keyp = M1.operator(keypQ)
        Supervisor.new(original, tasks, reference, task, keyp, Supervisor::DevicePool.new(pool), template.as_d)
      end

      matchpi %{[supervisor (@tasks_ @task_ keypQ_ - @pool_ bindings_dict ⍊ reference_: (%optional @reference @_)) template_*]} do
        keyp = M1.operator(keypQ)
        Supervisor.new(original, tasks, reference, task, keyp, Supervisor::ModulePool.new(pool, bindings.as_d), template.as_d)
      end

      matchpiT %{[parser (@src_ -> top_symbol -> @dst_) grammarQ_*]} do
        error = nil
        grammar = ParseKit.flatten(ParseKit.grammar(grammarQ))
        grammar_digest = Term.digest(grammarQ)
        cache = SyncLRU(Term::Str, Parser::Result).new(capacity: 8)
        Parser::Transfer.new(original, src, dst, error, grammar, grammar_digest, top, cache)
      end

      matchpiT %{[parser (@src_ -> top_symbol -> @dst_ / @error_) grammarQ_*]} do
        grammar = ParseKit.flatten(ParseKit.grammar(grammarQ))
        grammar_digest = Term.digest(grammarQ)
        cache = SyncLRU(Term::Str, Parser::Result).new(capacity: 8)
        Parser::Transfer.new(original, src, dst, error, grammar, grammar_digest, top, cache)
      end

      matchpiT %{[parser (@src_ - top_symbol - @dst_) grammarQ_*]} do
        error = nil
        grammar = ParseKit.flatten(ParseKit.grammar(grammarQ))
        grammar_digest = Term.digest(grammarQ)
        cache = SyncLRU(Term::Str, Parser::Result).new(capacity: 8)
        Parser::View.new(original, src, dst, error, grammar, grammar_digest, top, cache)
      end

      matchpiT %{[parser (@src_ - top_symbol - @dst_ / @error_) grammarQ_*]} do
        grammar = ParseKit.flatten(ParseKit.grammar(grammarQ))
        grammar_digest = Term.digest(grammarQ)
        cache = SyncLRU(Term::Str, Parser::Result).new(capacity: 8)
        Parser::View.new(original, src, dst, error, grammar, grammar_digest, top, cache)
      end

      matchpiT %{[rewriter (@src_ -> specQ_ -> @dst_) data_*]} do
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        Rewriter::Transfer.new(original, src, dst, rewriter, cache)
      end

      matchpiT %{[rewriter (@src_ - specQ_ - @dst_) data_*]} do
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        Rewriter::View.new(original, src, dst, rewriter, cache)
      end

      matchpiT %{[rewriter ((@src_ @version_) - specQ_ - @dst_) data_*]} do
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        Rewriter::VersionedView.new(original, src, version, dst, rewriter, cache)
      end

      matchpiT %{[rewriter ((@src_ @version_) -> specQ_ - @dst_) data_*]} do
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        Rewriter::AsymmetricVersionedView.new(original, src, version, dst, rewriter, cache)
      end

      matchpiT %{[rewriter (specQ_ - @target_) data_*]} do
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        nop = SyncLRU(Term::Str, Bool).new(capacity: 8)
        Rewriter::RegimeTarget.new(original, target, rewriter, cache, nop)
      end

      matchpiT %{[rewriter (specQ_ - templateQ_dict) data_*]} do
        template = EdgeTemplate.recognize(templateQ)
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        nop = SyncLRU(Term::Str, Bool).new(capacity: 8)
        Rewriter::RegimeTemplate.new(original, template, templateQ, rewriter, cache, nop)
      end

      matchpi %{[rewriter specQ_ (%group data_ [(%any rule backmap) _*] _*)]} do
        keys = keyset(rules: original.items.move(2))

        templateQ = Term::Dict.build do |commit|
          keys.each do |key|
            commit.with(key, {:edge, key})
          end
        end

        template = EdgeTemplate.recognize(templateQ)
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        nop = SyncLRU(Term::Str, Bool).new(capacity: 8)
        Rewriter::RegimeTemplate.new(original, template, templateQ, rewriter, cache, nop)
      end

      matchpi %{[rewriter specQ_ (%group data_ [stage _*] _*)]} do
        keys = Set(Term).new
        stages = original.items.move(2)
        stages.each do |stage|
          keys.concat(keyset(stage.items.move(1)))
        end

        templateQ = Term::Dict.build do |commit|
          keys.each do |key|
            commit.with(key, {:edge, key})
          end
        end

        template = EdgeTemplate.recognize(templateQ)
        rewriter, cache = Rewriter.rewriter_and_cache(specQ, data)
        nop = SyncLRU(Term::Str, Bool).new(capacity: 8)
        Rewriter::RegimeTemplate.new(original, template, templateQ, rewriter, cache, nop)
      end

      matchpi %{[fs @query_ @reply_]} do
        Fs.new(original, query, reply)
      end

      matchpi %{[sequencer periodQ_ _*]} do
        continue unless period = DurationLanguage.decode?(periodQ)

        Timing::Sequencer.new(original, period)
      end

      matchpi %{[ticker (%layer periodQ_ {| step⋮ 1}) ±state]} do
        continue unless period = DurationLanguage.decode?(periodQ)

        Timing::Ticker.new(original, period, step.as_n, state.as_n)
      end

      matchpi %{[timesource (periodQ_ @edge_) message_]} do
        continue unless period = DurationLanguage.decode?(periodQ)

        Timing::Timesource.new(original, period, edge, message)
      end

      matchpi %{[lfo (periodQ_ waveformQ_) _?]} do
        continue unless period = DurationLanguage.decode?(periodQ)
        continue unless waveform = Timing.waveform?(waveformQ)

        Timing::Lfo.new(original, period, waveform)
      end

      matchpi %{[timer delayQ_ start]} do
        continue unless delay = DurationLanguage.decode?(delayQ)

        Timing::PendingTimer.new(original, delay)
      end

      matchpi %{[timer delayQ_ (wait instant_string)]}, instant: String do
        continue unless delay = DurationLanguage.decode?(delayQ)

        parts = instant.split(':', limit: 2)
        continue unless parts.size == 2

        universe, deadline_ns = parts
        continue unless universe = universe.to_u64?(base: 62)
        continue unless deadline_ns = deadline_ns.to_u64?(base: 62)

        deadline = deadline_ns.nanoseconds

        Timing::WaitingTimer.new(original, delay, universe, deadline)
      end

      # |@example
      # ```wwml
      # (node @countdown
      #   (countdown (10 s rate: (33 ms)) start))
      #
      # (cell @display)
      #
      # (backsys
      #   {¦ countdown: (countdown _ ready) display_}
      #     <> {(display): ()}
      #
      #   {¦ countdown: (countdown [±t s] (wait ±progress _)) ⋮display}
      #     <> {display: ^(let* ((remaining-ms (* (* t 1000) (- 1 (exact progress))))
      #                          (centiseconds (floor (/ remaining-ms 10)))
      #                          (minutes (floor (/ centiseconds 6000)))
      #                          (seconds (mod (floor (/ centiseconds 100)) 60))
      #                          (milliseconds (mod (floor remaining-ms) 1000)))
      #                     "⸢(zeropad minutes 2)⸣:⸢(zeropad seconds 2)⸣.⸢(zeropad milliseconds 3)⸣")})
      # ```

      matchpi %{[countdown (%layer delayQ_ {| rate: rateQ_}) start]} do
        continue unless delay = DurationLanguage.decode?(delayQ)
        continue unless rate = DurationLanguage.decode?(rateQ)

        Timing::PendingCountdown.new(original, delay, rate)
      end

      matchpi %{[countdown (%layer delayQ_ {| rate: rateQ_}) (wait _number instant_string)]}, instant: String do
        continue unless delay = DurationLanguage.decode?(delayQ)
        continue unless rate = DurationLanguage.decode?(rateQ)

        parts = instant.split(':', limit: 2)
        continue unless parts.size == 2

        universe, deadline_ns = parts
        continue unless universe = universe.to_u64?(base: 62)
        continue unless deadline_ns = deadline_ns.to_u64?(base: 62)

        deadline = deadline_ns.nanoseconds
        Timing::WaitingCountdown.new(original, delay, rate, universe, deadline)
      end

      matchpi %{[clock (formatQ_string rate: rateQ_ location: locationQ_) _?]}, formatQ: String do
        continue unless rate = DurationLanguage.decode?(rateQ)
        continue unless location = Timing.location?(locationQ)

        format = Time::Format.new(formatQ, location)
        Timing::Clock.new(original, rate, format, location)
      end
      otherwise do
        Inert.new(original)
      end
    end
  end

  struct EdgeTemplate
    alias Member = Optional | Required | TriState

    defrecord TriState, key : Term, edge : Term
    defrecord Optional, key : Term, edge : Term
    defrecord Required, key : Term, edge : Term

    include Indexable(Member)

    # :nodoc:
    def initialize(@members : Slice(Member))
    end

    def size : Int32
      @members.size
    end

    def unsafe_fetch(index : Int) : Member
      @members.unsafe_fetch(index)
    end

    def self.recognize(template : Term::Dict) : EdgeTemplate
      members = Pf::Kit.stack_array(Member, 8)

      template.items.each_with_index do |item, index|
        next unless Term.edge?(item)

        members << Required.new(Term.of(index), item)
      end

      template.each_entry(in: Term::Dict.pairspart) do |key, value|
        Term.case(value) do
          matchpi %{@_} do
            members << Optional.new(key, value)
          end

          matchpi %{(optional @edge_)} do
            members << TriState.new(key, edge)
          end

          otherwise { }
        end
      end

      new(members.to_unsafe_readonly_slice!)
    end

    alias SituatedMember = RWMember | MissingMember

    defrecord RWMember, key : Term, initial : Term?, place : Place::Any
    defrecord MissingMember, key : Term

    def self.situate?(hg : Hypergraph, addr : NodeAddr, template : EdgeTemplate) : Slice(SituatedMember)?
      situated = Pf::Kit.stack_array(SituatedMember, 8)

      template.each do |member|
        case member
        in Required
          break unless place = hg.occupied_place?(addr, member.edge)

          situated << RWMember.new(member.key, place.value, place)
        in Optional
          place = hg.place?(addr, member.edge)

          case place
          in Nil
            situated << MissingMember.new(member.key)
          in Place::Empty
            situated << RWMember.new(member.key, nil, place)
          in Place::Occupied
            situated << RWMember.new(member.key, place.value, place)
          end
        in TriState
          place = hg.place?(addr, member.edge)

          case place
          in Nil
            situated << MissingMember.new(member.key)
          in Place::Empty
            situated << RWMember.new(member.key, Term.of(:none), place)
          in Place::Occupied
            situated << RWMember.new(member.key, Term.of(:some, place.value), place)
          end
        end
      end

      # Missing some required places.
      return unless situated.size == template.size

      situated.to_unsafe_readonly_slice!
    end

    def self.render(template : Term::Dict, situated : Slice(SituatedMember)) : Term::Dict
      template.transaction do |commit|
        situated.each do |member|
          case member
          in RWMember
            commit.with(member.key, member.initial)
          in MissingMember
            commit.without(member.key)
          end
        end
      end
    end

    def self.writeback(hg : Hypergraph, situated : Slice(SituatedMember), reply : Term::Dict) : Patch
      NeoRack.txn(situated) do |member|
        case member
        in MissingMember
        in RWMember
          unless value = reply[member.key]?
            next member.place.write(Term.rep)
          end

          # Don't emit patches for things that did not change.
          next if value == member.initial

          member.place.write(Term.rep(value))
        end
      end
    end
  end

  MAX_STEP_DEPTH = 512u32

  private def gather!(level, hg : Hypergraph, node : IInert | IGnd | IAutonomy, addr : NodeAddr, depth : UInt32) : Nil
  end

  private def gather!(level, hg : Hypergraph, node : Mixture | Scope, addr : NodeAddr, depth : UInt32) : Nil
    gather!(level, hg, node.successor, addr, depth)
  end

  private def gather!(level, hg : Hypergraph, node : Guard, addr : NodeAddr, depth : UInt32) : Nil
    return unless node.passable?(hg, addr)

    gather!(level, hg, node.successor, addr, depth)
  end

  private def gather!(level, hg : Hypergraph, node : ICircuit, addr : NodeAddr, depth : UInt32) : Nil
    if depth.zero?
      level << node
      return
    end

    node.children.zip(node.range) do |child, key|
      gather!(level, hg, child, addr.append(key), depth - 1)
    end
  end

  private def gather!(level, hg : Hypergraph, node : IGroup, addr : NodeAddr, depth : UInt32) : Nil
    node.children.zip(node.range) do |child, key|
      gather!(level, hg, child, addr.append(key), depth)
    end
  end

  private def gather!(level, root : ICircuit, depth : UInt32) : Nil
    hg = Hypergraph.new(root)
    addr = NodeAddr.empty
    gather!(level, hg, root, addr, depth)
  end

  defrecord AbsEdge, module : NodeAddr, id : Term

  # A *place* is an abstraction we invent here in Rack internals over cells. A place
  # is a cell-like thing. It can be empty or it can be occupied, just like a cell.
  # An occupied places has a value, `Occupied#value`. An empty place does not. Both
  # kinds of places allow you to `write` to them.
  #
  # Through this place abstraction we implement nodes such as `rack.part`, `rack.form`,
  # and `rack.changes`.
  module Place
    alias Any = Empty | Occupied
    alias Empty = EmptyDependency | EmptyCell | EmptyOutlet
    alias Occupied = OccupiedDependency | OccupiedCell | OccupiedOutlet | OccupiedForm

    module WritableCell
      abstract def value? : Term?

      def write(rep : Term::Rep) : Patch
        value0 = value?
        unless rep.empty?
          value1 = Term.collapse(rep)
        end
        # value1 : Term?

        NeoRack.txn(
          NeoRack.txn(@changes) { |node| Changes.log(node, before: value0, after: value1) },
          PatchTable.assoc(@target_id, Term.morph(@original, {2, value1})),
        )
      end
    end

    module WritableDependency
      abstract def value? : Term?

      def write(rep : Term::Rep) : Patch
        value0 = value?
        # NOTE: We're interested in the value **before** decorate().
        unless rep.empty?
          value1 = Term.collapse(rep)
        end
        # value1 : Term?

        rep = @decorate.call(rep)

        NeoRack.txn(
          NeoRack.txn(@changes) { |node| Changes.log(node, before: value0, after: value1) },
          @upstream.write(rep)
        )
      end
    end

    module WritableOutlet
      abstract def value? : Term?

      def write(rep : Term::Rep) : Patch
        if @outlet.readonly
          return PatchCancel.new
        end

        value0 = value?
        unless rep.empty?
          value1 = Term.collapse(rep)
        end
        # value1 : Term?

        schedule = NodeAddrRouter::Builder(Term?).new
        # `2` refers to (outlet @edge ⏏)
        schedule[@outlet.addr] = Term.morph(@outlet.original, {2, value1})

        NeoRack.txn(
          NeoRack.txn(@changes) { |node| Changes.log(node, before: value0, after: value1) },
          PatchTable.assoc(@target_id, NeoRack.apply(@form, schedule.router)),
        )
      end
    end

    struct EmptyCell
      include WritableCell

      # :nodoc:
      def initialize(@target_id : NodeId, @original : Term, @changes : Slice(NodeRef(Changes)))
      end

      def value? : Term?
      end
    end

    struct OccupiedCell
      include WritableCell

      getter value : Term

      # :nodoc:
      def initialize(@target_id : NodeId, @original : Term, @value, @changes : Slice(NodeRef(Changes)))
      end

      def value? : Term?
        value
      end
    end

    class EmptyDependency
      include WritableDependency

      # :nodoc:
      def initialize(@upstream : Any, @changes : Slice(NodeRef(Changes)), @decorate : Term::Rep -> Term::Rep)
      end

      def value? : Term?
      end
    end

    class OccupiedDependency
      include WritableDependency

      getter value : Term

      # :nodoc:
      def initialize(@upstream : Any, @value, @changes : Slice(NodeRef(Changes)), @decorate : Term::Rep -> Term::Rep)
      end

      def value? : Term?
        value
      end
    end

    struct EmptyOutlet
      include WritableOutlet

      # :nodoc:
      def initialize(
        @target_id : NodeId,
        @outlet : Form::OutletEntry,
        @form : Form,
        @changes : Slice(NodeRef(Changes)),
      )
      end

      def value? : Term?
      end
    end

    struct OccupiedOutlet
      include WritableOutlet

      getter value : Term

      # :nodoc:
      def initialize(
        @target_id : NodeId,
        @outlet : Form::OutletEntry,
        @value : Term,
        @form : Form,
        @changes : Slice(NodeRef(Changes)),
      )
      end

      def value? : Term?
        value
      end
    end

    struct OccupiedForm
      getter value : Term

      # :nodoc:
      def initialize(
        @target_id : NodeId,
        @entries : Slice(Form::Entry),
        @form : Form,
        @changes : Slice(NodeRef(Changes)),
      )
        # Turn the entries into a dict.
        row = Term::Dict.build do |commit|
          @entries.each do |entry|
            case entry
            in Form::OutletEntry
              commit << entry.value # can be nil, in which case nothing is appended
            in Form::FieldEntry
              commit.with(entry.key, entry.value) # value can be nil, in which case nothing is added
            end
          end
        end

        @value = Term.of(row)
      end

      def value? : Term?
        value
      end

      def write(rep : Term::Rep) : Patch
        # Figure out what changed.
        unless changes = Term.diff?(@value, Term.collapse(rep), depth_limit: 1u32)
          return PatchCancel.new
        end

        # Build a schedule.
        schedule = NodeAddrRouter::Builder(Term?).new
        accepted = true

        changes.each do |action|
          assert action.prefix.empty?

          next unless entry_index = @entries.index { |entry| entry.key == Term.of(action.key) }
          entry = @entries[entry_index]

          pass do
            next unless action.is_a?(Term::Diff::InsertItem)
            next unless entry.is_a?(Form::OutletEntry) && entry.value

            needle = nil

            (0...entry_index).reverse_each do |candidate_index|
              candidate = @entries[candidate_index]
              break unless candidate.is_a?(Form::OutletEntry)
              break unless candidate.value.nil? # we want an empty outlet
              next if candidate.readonly        # we want a writable one

              needle = candidate
              break
            end

            if needle.nil?
              accepted = false
              next
            end

            entry = needle
          end
          break unless accepted

          if entry.readonly
            accepted = false
            break
          end

          case action
          in Term::Diff::WithItem, Term::Diff::InsertItem
            schedule[entry.addr] = Term.morph(entry.original, {2, action.item})
          in Term::Diff::WithPair
            schedule[entry.addr] = Term.morph(entry.original, {2, action.value})
          in Term::Diff::DeleteItem, Term::Diff::WithoutPair
            schedule[entry.addr] = Term.morph(entry.original, {2, nil})
          end
        end

        unless accepted
          return PatchCancel.new
        end

        value0 = value?
        value1 = rep.empty? ? nil : Term.collapse(rep)

        # Deliver changes to entries of the form. The result is a term -- the new value
        # for the form, which we propose in the patch.
        NeoRack.txn(
          NeoRack.txn(@changes) { |node| Changes.log(node, before: value0, after: value1) },
          PatchTable.assoc(@target_id, NeoRack.apply(@form, schedule.router)),
        )
      end
    end
  end

  defrecord NodeRef(T), addr : NodeAddr, id : NodeId, node : T do
    {% unless T < IGnd %}
      {% T.raise "expected an IGnd includer" %}
    {% end %}
  end

  class Hypergraph
    getter root : ICircuit

    def initialize(@root : ICircuit)
    end

    def fetch(id : NodeId) : FetchResponse
      Hypergraph.fetch?(NodeAddr.empty, id, @root) || raise IndexError.new
    end

    def resolve(addr : NodeAddr, edge : Term) : AbsEdge
      node = @root
      scopes = Pf::Kit.stack_array({Int32, NodeScope}, 8)

      addr.each_with_index do |key, key_index|
        loop do
          case node
          in IInert, IGnd, IAutonomy
            raise KeyError.new
          in Mixture
            node = node.successor
            next
          in Scope
            scopes << {key_index, node.scope}
            node = node.successor
            next
          in Guard
            node = node.successor
          in IParent
            # NOTE: Circuits must surround themselves with scopes to seal themselves off
            # from the outside world completely. Otherwise, two circuits with the same
            # level would be able to communicate, and that would go against our semantics.
            #
            #   ;; Must NOT work!
            #   (circuit @0 (cell @x 100))
            #   (circuit @1 (cell @y))
            #   (circuit @2 (feed @x @y))
            #
            if node.is_a?(ICircuit)
              scopes << {key_index, ScopeClosedExcept.new(Term[])}
            end

            assert key >= node.range.begin

            index = key - node.range.begin
            unless 0 <= index < node.children.size
              raise KeyError.new
            end

            node = node.children.unsafe_fetch(index)
          end

          break
        end
      end

      # NOTE: Below, we use `trim(key_index)` instead of `trim(key_index + 1)`
      # because ScopeNodes themselves do not have an address -- they are "virtual"
      # nodes. So if we use `key_index + 1` for the "module" of the edge, that'd
      # mean the ScopeNode's *child* is the module -- which is incorrect! Instead,
      # we use simply `key_index`, which refers to the parent of the scope node.

      scopes.reverse_each do |key_index, scope|
        case scope
        in ScopeOpenExcept
          if edge.in?(scope.edges)
            # In Rack:
            #
            #  (locals (⏏@dst⏏) ;; @dst found, so it's a local!
            #    (feed @src ⏏@dst⏏)
            return AbsEdge.new(addr.trim(key_index), edge)
          end
          # Continue climbing. This edge falls into "open", thus outer-scoped,
          # not "except" and thus inner-scoped.
          #
          # In Rack:
          #  (locals (@dst) ;; <<- @src NOT found, so it's outerly-scoped.
          #    (feed ⏏@src⏏ @dst)
        in ScopeClosedExcept
          unless exterior = scope.bindings[edge]?
            # In Rack:
            #  (module {@x: @y} ;; <<- no @a, so it's a local!
            #    (feed ⏏@a⏏ @x)
            return AbsEdge.new(addr.trim(key_index), edge)
          end

          # In Rack:
          #  (module {⏏@x⏏: @y} ;; @x found, its *exterior* is the outerly-scoped @y.
          #    (feed @a ⏏@x⏏)
          edge = exterior
        end
      end

      AbsEdge.new(NodeAddr.empty, edge)
    end

    # TODO: early exit
    def present?(cls : T.class, connected_to edge : AbsEdge) : Bool forall T
      present = false
      each(cls, edge) do
        present = true
      end
      present
    end

    def single?(cls : T.class, connected_to edge : AbsEdge) : NodeRef(T)? forall T
      refs = Pf::Kit.stack_array(NodeRef(T), 4)
      each(cls, edge) do |ref|
        refs << ref
      end
      refs.single?
    end

    private def places(addr : NodeAddr, edge : Term, seenptr : Pf::USet32*) : Slice(Place::Any)
      abs_edge = resolve(addr, edge)

      pool = Pf::Kit.stack_array(NodeRef(Cell) | NodeRef(Outlet) | NodeRef(Field) | NodeRef(Part), 4)

      each(Cell, connected_to: abs_edge) do |candidate|
        # Cells have just one edge so no worries here.
        pool << candidate
      end

      each(Outlet, connected_to: abs_edge) do |candidate|
        # Outlets have just one edge so no worries here.
        pool << candidate
      end

      each(Field, connected_to: abs_edge) do |candidate|
        # Fields have just one edge so no worries here.
        pool << candidate
      end

      each(Part, connected_to: abs_edge) do |candidate|
        abs_dst = resolve(candidate.addr, candidate.node.dst)
        # Parts have two edges:
        #
        #   (part (@src @dst) ...)
        #
        # ... so we must verify which one we've actually hit. It must be @dst.
        next unless abs_edge == abs_dst

        pool << candidate
      end

      # Collect changes nodes so that the resulting Place can refer to them.
      changes = Pf::Kit.stack_array(NodeRef(Changes), 4)
      each(Changes, connected_to: abs_edge) do |node|
        changes << node
      end

      places = Pf::Kit.stack_array(Place::Any, 4)

      # Process forms on the edge (if any).
      each(FormLeaf, connected_to: abs_edge) do |leaf|
        form = leaf.node.form

        # Collect entries of the form, such as `rack.field` and `rack.outline`.
        entries = Pf::Kit.stack_array(Form::Entry, 8)
        Form.scan(form) do |entry|
          entries << entry
        end

        added = false
        pass do
          next if form.uniform
          next unless outlet = entries.single?.as?(Form::OutletEntry)

          case value = outlet.value
          in Nil
            places << Place::EmptyOutlet.new(leaf.id, outlet, form, changes.to_unsafe_readonly_slice!)
          in Term
            places << Place::OccupiedOutlet.new(leaf.id, outlet, value, form, changes.to_unsafe_readonly_slice!)
          end
          added = true
        end

        next if added

        places << Place::OccupiedForm.new(leaf.id, entries.to_unsafe_readonly_slice!, form, changes.to_unsafe_readonly_slice!)
      end

      pool.each do |member|
        case node = member.node
        when Cell::Empty
          places << Place::EmptyCell.new(member.id, node.original, changes.to_unsafe_readonly_slice!)
        when Cell::Occupied, Outlet::Occupied, Field::Occupied
          places << Place::OccupiedCell.new(member.id, node.original, node.value, changes.to_unsafe_readonly_slice!)
        when Part
          if seenptr.value.includes?(member.id) # cycle
            return Slice(Place::Any).empty
          end

          seenptr.value = seenptr.value.add(member.id)

          unless upstream = place?(member.addr, node.src, seenptr)
            return Slice(Place::Any).empty
          end

          unless upstream.is_a?(Place::Occupied)
            return Slice(Place::Any).empty
          end

          next unless place = part_place?(node, upstream, changes)

          places << place
        end
      end

      places.to_unsafe_readonly_slice!
    end

    private def part_place?(node : Part::Backmap, upstream : Place::Occupied, changes : Pf::Kit::HybridArray)
      env_trace_lists = M1.matches_and_traces(Term[], node.pattern, upstream.value)
      return if env_trace_lists.empty?

      value = pass do
        # If there's more than one match, we can make a write-only cell. We can't
        # make a readable cell because we're unsure about what they would like
        # to read. For example:
        #
        #   (cell @root (1 2 3 4 5))
        #   (part (@root @n) ⟨±n⟩°)
        #
        # Here, @n is write-only and will set all numbers.
        next unless env_trace_list = env_trace_lists.single?

        env, _ = env_trace_list
        env[node.capture]?
      end

      decorate = decoratef(node, upstream, value, env_trace_lists)

      # Note how absence of value isn't as great of a deal. Consider
      #
      #   (cell @D {})
      #   (cell @x 100)
      #   (part (@D @y) {¦ -y_})
      #   (feed @x @y)
      #
      # Here, even though there's no capture named `y`, we should still have a place
      # for it, so that the circuit can evolve like so:
      #
      #   (cell @D {y: 100})
      #   (cell @x)
      #   (part (@D @y) {¦ -y_})
      #   (feed @x @y)
      if value.nil?
        return Place::EmptyDependency.new(upstream, changes.to_unsafe_readonly_slice!, decorate)
      end

      Place::OccupiedDependency.new(upstream, value, changes.to_unsafe_readonly_slice!, decorate)
    end

    private def part_place?(node : Part::Backsys, upstream : Place::Occupied, changes : Pf::Kit::HybridArray)
      query = Term.of(Term[].with(node.whole, upstream.value))
      reply = M1.backmap(node.backsys, query)
      return unless reply = reply.as_d?

      value = reply[node.capture]?
      decorate = decoratef(node, upstream, value)

      if value.nil?
        return Place::EmptyDependency.new(upstream, changes.to_unsafe_readonly_slice!, decorate)
      end

      Place::OccupiedDependency.new(upstream, value, changes.to_unsafe_readonly_slice!, decorate)
    end

    private def decoratef(node : Part::Backmap, upstream : Place::Occupied, input : Term?, env_trace_lists : M1::EnvTraceList)
      ->(rep : Term::Rep) do
        if input == rep.single? # Unchanged
          return Term.rep(upstream.value)
        end

        backspec = pass do
          # Having an empty rep should remove `1`:
          #
          #   (cell @root (+ 1 2))
          #   (part (@root @a) (+ a_ _))
          #   (discard @a)
          #
          # like so:
          #
          #   (cell @root (+ 2))
          unless goal = rep.single?
            next Term[].with({node.capture}, rep)
          end

          if goal.type.dict? || goal.type.symbol?
            Term[].with(node.capture, {:"^verbatim", goal})
          else
            # Do not waste time doing ^verbatim stuff on terms that cannot cause
            # us trouble: booleans, numbers, etc.
            Term[].with(node.capture, goal)
          end
        end

        backmap = {env_trace_lists, backspec}
        M1.backmapR({backmap}, upstream.value)
      end
    end

    private def decoratef(node : Part::Backsys, upstream : Place::Occupied, input : Term?)
      ->(rep : Term::Rep) do
        if input == rep.single? # Unchanged
          return Term.rep(upstream.value)
        end

        query = Term.of(
          Term[]
            .with(node.whole, upstream.value)
            .with(node.capture, Term.collapse(rep))
        )
        reply = M1.backmap(node.backsys, query)

        rep = Term.rep(upstream.value)
        pass do
          next unless reply = reply.as_d?
          next unless successor = reply[node.whole]?

          rep = Term.rep(successor)
        end

        rep
      end
    end

    private def place?(addr : NodeAddr, edge : Term, seenptr : Pf::USet32*) : Place::Any?
      places(addr, edge, seenptr).single?
    end

    def place?(addr : NodeAddr, edge : Term) : Place::Any?
      seen = Pf::USet32.new
      place?(addr, edge, pointerof(seen))
    end

    def occupied_place?(addr : NodeAddr, edge : Term) : Place::Occupied?
      place?(addr, edge).as?(Place::Occupied)
    end

    def empty_place?(addr : NodeAddr, edge : Term) : Place::Empty?
      place?(addr, edge).as?(Place::Empty)
    end

    def occupied_places(addr : NodeAddr, edge : Term) : Slice(Place::Occupied)
      seen = Pf::USet32.new
      places = places(addr, edge, pointerof(seen))
      places.to_compact_readonly_slice { |place| place.as?(Place::Occupied) }
    end

    def each(cls : T.class, &fn : NodeRef(T) ->) forall T
      scan(NodeAddr.empty, 0u32, @root, fn)
    end

    def each(cls : T.class, connected_to edge : AbsEdge, &fn : NodeRef(T) ->) : Nil forall T
      id_zero, ceiling = Hypergraph.descend(@root, edge.module)
      assert ceiling.is_a?(Scope) || ceiling.is_a?(ICircuit)

      case ceiling
      in ICircuit
        scan_scope(edge.module, id_zero, edge.id, ceiling, fn)
      in Scope
        # Conduct search starting from the edge module's successor. We must start
        # from the successor; otherwise, our lookups that are meant for the interior
        # of the module, will hit the module's very boundary, possibly being "deflected"
        # by the module.
        scan_scope(edge.module, id_zero, edge.id, ceiling.successor, fn)
      end
    end

    # :nodoc:
    def scan(addr : NodeAddr, id : NodeId, node : IInert | IAutonomy, fn) : Nil
    end

    # :nodoc:
    def scan(addr : NodeAddr, id : NodeId, node : IGnd, fn : NodeRef(T) ->) : Nil forall T
      return unless node.is_a?(T)

      fn.call(NodeRef(T).new(addr, id, node))
    end

    # :nodoc:
    def scan(addr : NodeAddr, id : NodeId, node : Mixture | Scope, fn) : Nil
      scan(addr, id, node.successor, fn)
    end

    # :nodoc:
    def scan(addr : NodeAddr, id : NodeId, node : Guard, fn) : Nil
      return unless node.passable?(self, addr)

      scan(addr, id, node.successor, fn)
    end

    # :nodoc:
    def scan(addr : NodeAddr, id : NodeId, node : IParent, fn : NodeRef(T) ->) : Nil forall T
      summary = NeoRack.summarize(node)
      return unless summary.includes?(T)

      node.children.zip(node.range) do |child, key|
        scan(addr.append(key), id, child, fn)
        id += NeoRack.summarize(child).gnd_count
      end
    end

    # :nodoc:
    def scan_scope(addr : NodeAddr, id : NodeId, edge : Term, node : IInert | IAutonomy, fn) : Nil
    end

    # :nodoc:
    def scan_scope(addr : NodeAddr, id : NodeId, edge : Term, node : IGnd, fn : NodeRef(T) ->) : Nil forall T
      return unless node.is_a?(T)

      found = false
      node.each_edge do |candidate|
        next unless candidate == edge
        found = true
        break
      end

      return unless found

      fn.call(NodeRef(T).new(addr, id, node))
    end

    # :nodoc:
    def scan_scope(addr : NodeAddr, id : NodeId, edge : Term, node : Mixture, fn) : Nil
      scan_scope(addr, id, edge, node.successor, fn)
    end

    # :nodoc:
    def scan_scope(addr : NodeAddr, id : NodeId, edge : Term, node : Scope, fn) : Nil
      case scope = node.scope
      in ScopeOpenExcept
        return if edge.in?(scope.edges)

        scan_scope(addr, id, edge, node.successor, fn)
      in ScopeClosedExcept
        translation = scope.bindings.ee.find { |interior, exterior| edge == exterior }
        return unless translation

        interior, _ = translation
        scan_scope(addr, id, interior, node.successor, fn)
      end
    end

    # :nodoc:
    def scan_scope(addr : NodeAddr, id : NodeId, edge : Term, node : IParent, fn : NodeRef(T) ->) : Nil forall T
      summary = NeoRack.summarize(node)
      return unless summary.includes?(T)

      node.children.zip(node.range) do |child, key|
        scan_scope(addr.append(key), id, edge, child, fn)
        id += NeoRack.summarize(child).gnd_count
      end
    end

    # :nodoc:
    def scan_scope(addr : NodeAddr, id : NodeId, edge : Term, node : Guard, fn) : Nil
      return unless node.passable?(self, addr)

      scan_scope(addr, id, edge, node.successor, fn)
    end
  end

  class Hypergraph
    def self.descend?(node : Node, addr : NodeAddr) : {NodeId, Node}?
      id = NodeId.new(0)

      addr.each do |index|
        loop do
          case node
          in IInert, IGnd, IAutonomy
            return
          in Mixture, Scope, Guard
            node = node.successor
          in IParent
            if node.is_a?(ICircuit)
              id += NeoRack.summarize(node.leaf).gnd_count # Leave an id range reserved for the leaf
            end

            offset = node.range.begin
            assert index >= offset

            # Do not forget to shift the id of the child by all prior ids.
            prior = node.children.trim(index - offset)
            prior.each do |child|
              id += NeoRack.summarize(child).gnd_count
            end

            node = node.children[index - offset]
            break
          end
        end
      end

      {id, node}
    end

    def self.descend(node : Node, addr : NodeAddr) : {NodeId, Node}
      descend?(node, addr) || raise KeyError.new
    end

    defrecord FetchResponse, addr : NodeAddr, node : IGnd

    def self.fetch?(addr : NodeAddr, id : NodeId, node : IInert | IAutonomy) : FetchResponse?
    end

    def self.fetch?(addr : NodeAddr, id : NodeId, node : IGnd) : FetchResponse?
      id.zero? ? FetchResponse.new(addr, node) : nil
    end

    def self.fetch?(addr : NodeAddr, id : NodeId, node : Mixture | Scope | Guard) : FetchResponse?
      fetch?(addr, id, node.successor)
    end

    def self.fetch?(addr : NodeAddr, id : NodeId, node : IParent) : FetchResponse?
      node.children.zip(node.range) do |child, key|
        stride = NeoRack.summarize(child).gnd_count
        if id < stride
          return fetch?(addr.append(key), id, child)
        end

        id -= stride
      end
    end
  end

  def apply(node : IInert | IGnd | IAutonomy, router : NodeAddrRouter) : Term
    router.value? || node.original
  end

  def apply(node : Mixture, router : NodeAddrRouter) : Term
    result = apply(node.successor, router)

    # Save on a call to the fold function.
    if result == node.original
      return node.original
    end

    node.fold.call(result)
  end

  def apply(node : Scope | Guard, router : NodeAddrRouter) : Term
    apply(node.successor, router)
  end

  def apply(node : ICircuit, router : NodeAddrRouter) : Term
    if router.has_value?
      apply(node.leaf, router)
    else
      apply_parent(node, router)
    end
  end

  def apply(node : IParent, router : NodeAddrRouter) : Term
    apply_parent(node, router)
  end

  private def apply_parent(node : IParent, router : NodeAddrRouter) : Term
    result = node.original_dict.transaction do |commit|
      node.children.zip(node.range) do |child, key|
        next unless successor = router.cd?(key)

        commit.with(key, apply(child, successor))
      end
    end

    Term.of(result)
  end

  def broadcast(changes : Deque, node : IInert | IGnd | IAutonomy, depth : UInt32) : Term
    node.original
  end

  def broadcast(changes : Deque, node : Mixture, depth : UInt32) : Term
    result = broadcast(changes, node.successor, depth)

    # Save on a call to the fold function.
    if node.original == result
      return node.original
    end

    node.fold.call(result)
  end

  def broadcast(changes : Deque, node : Scope | Guard, depth : UInt32) : Term
    broadcast(changes, node.successor, depth)
  end

  def broadcast(changes : Deque, node : ICircuit, depth : UInt32) : Term
    # If there are no changes left, we can safely unwind and ignore the rest of nodes.
    if changes.empty?
      return node.original
    end

    if depth > 0
      return broadcast_parent(changes, node, depth - 1)
    end
    # depth = 0

    receiver, desired = changes.first

    unless node.same?(receiver)
      return node.original
    end

    _ = changes.shift
    desired
  end

  def broadcast(changes : Deque, node : IParent, depth : UInt32) : Term
    broadcast_parent(changes, node, depth)
  end

  private def broadcast_parent(changes : Deque, node : IParent, depth : UInt32) : Term
    # If there are no changes left, we can safely unwind and ignore the rest of nodes.
    if changes.empty?
      return node.original
    end

    summary = NeoRack.summarize(node)

    # Maxlevel is never 0. It is at least one. More than one if there are subcircuits.
    # We need to simulate subcircuits only if there are subcircuits!
    if summary.maxlevel < depth
      return node.original
    end

    result = node.original_dict.transaction do |commit|
      node.children.zip(node.range) do |child, key|
        commit.with(key, broadcast(changes, child, depth))
      end
    end

    Term.of(result)
  end

  private def nested_seal(node : Node) : Node
    case node
    in IInert, IGnd, IAutonomy
      node
    in Mixture, Scope, Guard
      node.copy_with(successor: nested_seal(node.successor))
    in ICircuit
      node.leaf
    in IGroup
      summary = summarize(node)
      summary.maxlevel > 0 ? node.map { |child| nested_seal(child) }.as(IGroup) : node
    end
  end

  # Replaces subcircuits of *circuit* with their corresponding leaf nodes (`ICircuit#leaf`).
  def seal(circuit : ICircuit) : ICircuit
    summary = summarize(circuit)
    # Since *circuit* is an ICircuit, the resulting maxlevel is at least 1.
    summary.maxlevel > 1 ? circuit.map { |child| nested_seal(child) }.as(ICircuit) : circuit
  end

  # Maintains relations across spots in the circuit.
  def repair(hg : Hypergraph, patches : Array(Patch)) : Nil
    hg.each(Manipulable) do |manipulable|
      next unless target = hg.occupied_place?(manipulable.addr, manipulable.node.edge)
      next unless env = M1.match?(Term[], manipulable.node.selector, target.value)
      next unless entry = env.ee.first?
      key, value = entry

      backspec = Term[].with(key, {:"^verbatim", value})
      backmap = {manipulable.node.env_trace_lists, backspec}
      rep = M1.backmapR({backmap}, manipulable.node.operand)
      patches << patch(manipulable, {2, Term.collapse(rep)})
    end

    hg.each(Supervisor) do |supervisor|
      Supervisor.repair(hg, patches, supervisor)
    end
  end

  alias ParserTaskBoard = D7::TaskBoard(BlockingSignal, Parser::Task, Parser::Result)
  alias ParserRdv = D7::TaskBoard::Rdv(BlockingSignal, Parser::Task, Parser::Result)

  alias RewriterTaskBoard = D7::TaskBoard(BlockingSignal, Rewriter::Task, Rewriter::Result)
  alias RewriterRdv = D7::TaskBoard::Rdv(BlockingSignal, Rewriter::Task, Rewriter::Result)

  alias FsTaskBoard = D7::TaskBoard(BlockingSignal, Fs::Request, Fs::Response)
  alias FsRdv = D7::TaskBoard::Rdv(BlockingSignal, Fs::Request, Fs::Response)

  class Context
    def initialize(@signal : BlockingSignal)
      @fs_tasks = FsTaskBoard.new(@signal) { |task, ping| Fs.execute(task, ping) }
      @parser_tasks = ParserTaskBoard.new(@signal) { |task, ping| Parser.execute(task, ping) }
      @rewriter_tasks = RewriterTaskBoard.new(@signal) { |task, ping| Rewriter.execute(task, ping) }

      @time_origin = Time.instant
      @time_checkpoint = Time::Span.zero
      @time_periods = Set(Time::Span).new
      @time_deadlines = Set(Time::Span).new
    end

    # Returns `true` if there are pending background or future tasks, or any
    # other sort of business. Quiescence cannot be reached while the context
    # is pending.
    def pending? : Bool
      {@fs_tasks, @parser_tasks, @rewriter_tasks}.any?(&.pending?)
    end

    def deadline? : Time::Instant?
      now = Time.instant
      elapsed = now - @time_origin

      period_t = @time_periods.min_of? do |period|
        if period.zero?
          now
        else
          n = (elapsed / period).floor.to_i64 + 1
          @time_origin + period * n
        end
      end

      deadline_t = @time_deadlines.min_of? do |deadline|
        @time_origin + deadline
      end

      if period_t && deadline_t
        {period_t, deadline_t}.min
      else
        period_t || deadline_t
      end
    end

    # :nodoc:
    def epoch(&)
      @fs_tasks.rdv do |fs_rdv|
        @parser_tasks.rdv do |parser_rdv|
          @rewriter_tasks.rdv do |rewriter_rdv|
            @time_periods.clear
            @time_deadlines.clear

            time_now = Time.instant - @time_origin
            begin
              yield ContextEpoch.new(fs_rdv, parser_rdv, rewriter_rdv, @time_checkpoint, time_now, @time_periods, @time_deadlines)
            ensure
              @time_checkpoint = time_now
            end
          end
        end
      end
    end
  end

  defrecord ContextEpoch,
    fs_rdv : FsRdv,
    parser_rdv : ParserRdv,
    rewriter_rdv : RewriterRdv,
    time_checkpoint : Time::Span,
    time_now : Time::Span,
    time_periods : Set(Time::Span),
    time_deadlines : Set(Time::Span)

  # Runs the main symbolic physics pass.
  def advance(epoch : ContextEpoch, hg : Hypergraph, patches : Array(Patch)) : Nil
    hg.each(Feed::Transfer) do |feed|
      next if feed.node.inhibitors.any? { |inhibitor| !!hg.occupied_place?(feed.addr, inhibitor.edge) }
      next unless took = Feed.take?(hg, feed.addr, feed.node.src)
      next unless placed = Feed.put?(hg, feed.addr, feed.node.dst, took.value)

      patches << txn(took.patch.call, placed.patch)
    end

    hg.each(Feed::Broadcast) do |feed|
      next if feed.node.inhibitors.any? { |inhibitor| !!hg.occupied_place?(feed.addr, inhibitor.edge) }
      next unless took = Feed.take?(hg, feed.addr, feed.node.src)

      placed = Pf::Kit.stack_array(Feed::PutResult, 4)
      feed.node.dsts.each do |dst|
        break unless result = Feed.put?(hg, feed.addr, dst, took.value)
        placed << result
      end
      next unless placed.size == feed.node.dsts.size # some destinations failed

      patches << txn(took.patch.call, txn(placed, &.patch))
    end

    hg.each(Feed::Distribute) do |feed|
      next if feed.node.inhibitors.any? { |inhibitor| !!hg.occupied_place?(feed.addr, inhibitor.edge) }
      next unless took = Feed.take?(hg, feed.addr, feed.node.src)
      next unless dict = took.value.as_d?
      next unless dict.itemsize == feed.node.dsts.size

      placed = Pf::Kit.stack_array(Feed::PutResult, 4)
      dict.items.zip(feed.node.dsts) do |item, dst|
        break unless result = Feed.put?(hg, feed.addr, dst, item)
        placed << result
      end
      next unless placed.size == feed.node.dsts.size # some destinations failed

      patches << txn(took.patch.call, txn(placed, &.patch))
    end

    hg.each(Feed::Aggregate) do |feed|
      next if feed.node.inhibitors.any? { |inhibitor| !!hg.occupied_place?(feed.addr, inhibitor.edge) }

      took = Pf::Kit.stack_array(Feed::TakeResult, 4)
      feed.node.srcs.each do |src|
        break unless result = Feed.take?(hg, feed.addr, src)
        took << result
      end
      next unless took.size == feed.node.srcs.size # some sources missing

      row = Term::Dict.build do |commit|
        commit.concat(took, &.value)
      end

      next unless placed = Feed.put?(hg, feed.addr, feed.node.dst, Term.of(row))

      patches << txn(txn(took, &.patch.call), placed.patch)
    end

    hg.each(Feed::ParallelTransfer) do |feed|
      assert feed.node.srcs.size == feed.node.dsts.size

      next if feed.node.inhibitors.any? { |inhibitor| !!hg.occupied_place?(feed.addr, inhibitor.edge) }

      took = Pf::Kit.stack_array(Feed::TakeResult, 4)
      feed.node.srcs.each do |src|
        break unless result = Feed.take?(hg, feed.addr, src)
        took << result
      end
      next unless took.size == feed.node.srcs.size # some sources missing

      placed = Pf::Kit.stack_array(Feed::PutResult, 4)
      took.zip(feed.node.dsts) do |src, dst|
        break unless result = Feed.put?(hg, feed.addr, dst, src.value)
        placed << result
      end
      next unless placed.size == feed.node.dsts.size # some destinations failed

      patches << txn(
        txn(took, &.patch.call),
        txn(placed, &.patch),
      )
    end

    hg.each(Discard::Unconditional) do |discard|
      targets = hg.occupied_places(discard.addr, discard.node.edge)
      next if targets.empty?

      patches << txn(targets, &.write(Term.rep))
    end

    hg.each(Discard::Conditional) do |discard|
      targets = hg.occupied_places(discard.addr, discard.node.edge)
      next if targets.empty?

      patches << txn(targets) do |target|
        next unless M1.probe?(Term[], discard.node.pattern, target.value)

        target.write(Term.rep)
      end
    end

    hg.each(Reading) do |reading|
      next unless target = hg.place?(reading.addr, reading.node.edge)

      case target
      when Place::Empty
        patches << patch(reading, {2, nil})
      when Place::Occupied
        patches << patch(reading, {2, target.value})
      end
    end

    hg.each(Transfer::One) do |transfer|
      next unless src = hg.occupied_place?(transfer.addr, transfer.node.src)
      next unless env = M1.match?(Term[], transfer.node.pattern, src.value)
      next unless dst = hg.empty_place?(transfer.addr, transfer.node.dst)

      instance_rep = Alloy.render_rep(transfer.node.template, locals: env)
      patches << txn(src.write(Term.rep), dst.write(instance_rep))
    end

    hg.each(Transfer::Many) do |transfer|
      next if transfer.node.inhibitors.any? { |inhibitor| !!hg.occupied_place?(transfer.addr, inhibitor) }

      srcs = Pf::Kit.stack_array(Place::Occupied, 8)

      transfer.node.srcs.each do |src_edge|
        break unless src = hg.occupied_place?(transfer.addr, src_edge)

        srcs << src
      end

      # Not all source cells were found, abort.
      next unless srcs.size == transfer.node.srcs.size

      row = Term::Dict.build do |commit|
        commit.concat(srcs, &.value)
      end

      next unless env = M1.match?(Term[], transfer.node.pattern, Term.of(row))
      next unless dst = hg.empty_place?(transfer.addr, transfer.node.dst)

      instance_rep = Alloy.render_rep(transfer.node.template, locals: env)
      patches << txn(txn(srcs, &.write(Term.rep)), dst.write(instance_rep))
    end

    hg.each(View::One) do |view|
      next unless dst = hg.place?(view.addr, view.node.dst)

      # If the source cell is missing or empty, clear the destination cell.
      unless src = hg.occupied_place?(view.addr, view.node.src)
        patches << dst.write(Term.rep)
        next
      end

      # Clear the destination cell on pattern mismatch.
      unless env = M1.match?(Term[], view.node.pattern, src.value)
        patches << dst.write(Term.rep)
        next
      end

      instance_rep = Alloy.render_rep(view.node.template, locals: env)
      patches << dst.write(instance_rep)
    end

    hg.each(View::Many) do |view|
      next unless dst = hg.place?(view.addr, view.node.dst)

      row = Term::Dict.build do |commit|
        view.node.srcs.each do |src_edge|
          break unless src = hg.occupied_place?(view.addr, src_edge)

          commit << src.value
        end
      end

      # Not all source cells were found, abort. Remember to clear the destination cell.
      unless row.size == view.node.srcs.size
        patches << dst.write(Term.rep)
        next
      end

      # Clear the destination cell on pattern mismatch.
      unless env = M1.match?(Term[], view.node.pattern, Term.of(row))
        patches << dst.write(Term.rep)
        next
      end

      instance_rep = Alloy.render_rep(view.node.template, locals: env)
      patches << dst.write(instance_rep)
    end

    hg.each(View::OneOver) do |view|
      next unless src = hg.occupied_place?(view.addr, view.node.src)
      next unless env = M1.match?(Term[], view.node.pattern, src.value)
      next unless dst = hg.place?(view.addr, view.node.dst)

      instance_rep = Alloy.render_rep(view.node.template, locals: env)
      patches << dst.write(instance_rep)
    end

    hg.each(View::ManyOver) do |view|
      row = Term::Dict.build do |commit|
        view.node.srcs.each do |src_edge|
          break unless src = hg.occupied_place?(view.addr, src_edge)

          commit << src.value
        end
      end

      next unless row.size == view.node.srcs.size
      next unless env = M1.match?(Term[], view.node.pattern, Term.of(row))
      next unless dst = hg.place?(view.addr, view.node.dst)

      instance_rep = Alloy.render_rep(view.node.template, locals: env)
      patches << dst.write(instance_rep)
    end

    hg.each(Delay) do |delay|
      if delay.node.countdown <= 1
        patches << replace(delay, delay.node.successor)
        next
      end

      patches << patch(delay, {1, delay.node.countdown - 1})
    end

    hg.each(Backsys::Single) do |backsys|
      next unless target = hg.occupied_place?(backsys.addr, backsys.node.edge)
      next unless rep = M1.backmapR?(backsys.node.backsys, target.value)
      next if rep == Term.rep(target.value) # Unchanged, get out early to avoid hitting e.g. merge()

      patches << target.write(rep)
    end

    hg.each(Backsys::Template) do |backsys|
      next unless situated = EdgeTemplate.situate?(hg, backsys.addr, backsys.node.template)

      query = EdgeTemplate.render(backsys.node.template_dict, situated)
      next unless reply = M1.backmap?(backsys.node.backsys, Term.of(query))
      next unless reply = reply.as_d?
      next if query == reply # unchanged

      patches << EdgeTemplate.writeback(hg, situated, reply)
    end

    hg.each(Parser::Transfer) do |parser|
      next unless src = hg.occupied_place?(parser.addr, parser.node.src)
      next unless text = src.value.as_s?
      next unless dst = hg.empty_place?(parser.addr, parser.node.dst)
      if error_edge = parser.node.error
        next unless error = hg.empty_place?(parser.addr, error_edge)
      end

      unless result = parser.node.cache.get?(text)
        task = Parser::Task.new(text, parser.node.grammar, parser.node.grammar_digest, parser.node.top)

        result = epoch.parser_rdv.result?(task)
        # Try computing inline (returns Result) or schedule (returns Nil).
        result ||= epoch.parser_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)

        if result
          parser.node.cache.put(text, result)
        end
      end

      case result
      in Nil
        # Result not available
      in Term
        patches << txn(src.write(Term.rep), dst.write(Term.rep(result)))
      in ParseKit::Err
        # Parse error has nowhere to go, so we clog the input.
        next if error.nil?

        repr = Term.of(:err, result.detail)
        patches << txn(src.write(Term.rep), error.write(Term.rep(repr)))
      end
    end

    hg.each(Parser::View) do |parser|
      next unless dst = hg.place?(parser.addr, parser.node.dst)

      src = hg.place?(parser.addr, parser.node.src)
      if error_edge = parser.node.error
        error = hg.place?(parser.addr, error_edge)
      end

      txn = [] of Patch

      pass do
        # If the source cell is absent or empty, clear target cell(s).
        unless src.is_a?(Place::Occupied) && (text = src.value.as_s?)
          txn << dst.write(Term.rep)
          txn << error.write(Term.rep) if error
          next
        end

        unless result = parser.node.cache.get?(text)
          task = Parser::Task.new(text, parser.node.grammar, parser.node.grammar_digest, parser.node.top)

          result = epoch.parser_rdv.result?(task)
          result ||= epoch.parser_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)

          if result
            parser.node.cache.put(text, result)
          end
        end

        case result
        in Nil
          # Result not available
          txn << dst.write(Term.rep)
          txn << error.write(Term.rep) if error
        in Term
          txn << dst.write(Term.rep(result))
          txn << error.write(Term.rep) if error
        in ParseKit::Err
          repr = Term.of(:err, result.detail)
          txn << dst.write(Term.rep)
          txn << error.write(Term.rep(repr)) if error
        end
      end

      patches << txn(txn)
    end

    hg.each(Rewriter::Transfer) do |rewriter|
      next unless src = hg.occupied_place?(rewriter.addr, rewriter.node.src)
      next unless dst = hg.empty_place?(rewriter.addr, rewriter.node.dst)

      task = Rewriter::Task.new(rewriter.node.rho, rewriter.node.cache, src.value)

      result = epoch.rewriter_rdv.result?(task)
      result ||= epoch.rewriter_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)
      next unless result # Result not available yet.

      patches << txn(src.write(Term.rep), dst.write(Term.rep(result)))
    end

    hg.each(Rewriter::View) do |rewriter|
      next unless dst = hg.place?(rewriter.addr, rewriter.node.dst)

      src = hg.place?(rewriter.addr, rewriter.node.src)
      unless src.is_a?(Place::Occupied)
        patches << dst.write(Term.rep)
        next
      end

      task = Rewriter::Task.new(rewriter.node.rho, rewriter.node.cache, src.value)

      result = epoch.rewriter_rdv.result?(task)
      result ||= epoch.rewriter_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)
      next unless result # Result not available yet.

      patches << dst.write(Term.rep(result))
    end

    hg.each(Rewriter::VersionedView) do |rewriter|
      next unless version = hg.place?(rewriter.addr, rewriter.node.version)

      unless dst = hg.place?(rewriter.addr, rewriter.node.dst)
        # Clear version (missing output cell).
        patches << version.write(Term.rep)
        next
      end

      # Find a single nonempty input cell.
      unless src = hg.occupied_place?(rewriter.addr, rewriter.node.src)
        # Clear output and version (missing input cell or input).
        patches << txn(version.write(Term.rep), dst.write(Term.rep))
        next
      end

      # Include rewriter node digest to ensure rule and spec changes trigger
      # recomputation of the result.
      digest = Term.digest(rewriter.node.original, src.value)
      next if version.value? == digest # Unchanged

      task = Rewriter::Task.new(rewriter.node.rho, rewriter.node.cache, src.value)

      result = epoch.rewriter_rdv.result?(task)
      result ||= epoch.rewriter_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)
      next unless result # Result not available yet.

      patches << txn(version.write(Term.rep_of(digest)), dst.write(Term.rep(result)))
    end

    hg.each(Rewriter::AsymmetricVersionedView) do |rewriter|
      next unless version = hg.place?(rewriter.addr, rewriter.node.version)

      unless dst = hg.place?(rewriter.addr, rewriter.node.dst)
        # Clear version (missing output cell).
        patches << version.write(Term.rep)
        next
      end

      # Find a single nonempty input cell.
      unless src = hg.occupied_place?(rewriter.addr, rewriter.node.src)
        # Clear output and version (missing input cell or input).
        patches << txn(version.write(Term.rep), dst.write(Term.rep))
        next
      end

      # Include rewriter node digest to ensure rule and spec changes trigger
      # recomputation of the result.
      digest = Term.digest(rewriter.node.original, src.value)

      if version.value? == digest # Unchanged
        # Input is the same as the one we've already seen, simply consume it.
        patches << src.write(Term.rep)
        next
      end

      task = Rewriter::Task.new(rewriter.node.rho, rewriter.node.cache, src.value)

      result = epoch.rewriter_rdv.result?(task)
      result ||= epoch.rewriter_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)
      next unless result # Result not available yet.

      patches << txn(
        src.write(Term.rep),
        version.write(Term.rep_of(digest)),
        dst.write(Term.rep(result)),
      )
    end

    hg.each(Rewriter::RegimeTarget) do |rewriter|
      next unless target = hg.occupied_place?(rewriter.addr, rewriter.node.target)

      target_digest = Term.digest(target.value)
      next if rewriter.node.nop.get?(target_digest) # unchanged

      task = Rewriter::Task.new(rewriter.node.rho, rewriter.node.cache, target.value)

      result = epoch.rewriter_rdv.result?(task)
      result ||= epoch.rewriter_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)
      next unless result # Result not available yet.

      if target == result # damaged or unchanged
        rewriter.node.nop.put(target_digest, true)
        next
      end

      patches << target.write(Term.rep(result))
    end

    hg.each(Rewriter::RegimeTemplate) do |rewriter|
      next unless situated = EdgeTemplate.situate?(hg, rewriter.addr, rewriter.node.template)

      query : Term::Dict
      query = EdgeTemplate.render(rewriter.node.template_dict, situated)
      query_digest = Term.digest(query)
      next if rewriter.node.nop.get?(query_digest) # unchanged

      task = Rewriter::Task.new(rewriter.node.rho, rewriter.node.cache, Term.of(query))

      reply = epoch.rewriter_rdv.result?(task)
      reply ||= epoch.rewriter_rdv.publish?(task, throttle: 64u64, deadline: 100.microseconds)
      next unless reply # Result not available yet.

      reply = reply.as_d?
      if reply.nil? || query == reply # damaged or unchanged
        rewriter.node.nop.put(query_digest, true)
        next
      end

      patches << EdgeTemplate.writeback(hg, situated, reply)
    end

    hg.each(Fs) do |fs|
      next unless query = hg.occupied_place?(fs.addr, fs.node.query)
      next unless reply = hg.empty_place?(fs.addr, fs.node.reply)
      next unless request = Fs.request?(query.value)

      if response = epoch.fs_rdv.result?(request)
        responseQ = Fs.transcribe(response)
        patches << txn(query.write(Term.rep), reply.write(Term.rep(responseQ)))
        next
      end

      # File system operations are always executed on a different fiber
      # to avoid blocking evolution.
      epoch.fs_rdv.publish(request)
    end

    hg.each(Timing::Sequencer) do |sequencer|
      # Maintain a set of active periods. Note that it *is* possible to get these from
      # the circuit instead of having to mutate this set. However, getting them from
      # the circuit is probably more expensive, but more importantly, it will also
      # bypass `guard`s. So if you have a sequencer inside an inactive guard, it'll
      # still be seen and it'll cause a wakeup. We clearly don't want that. By far,
      # the easiest way is to just maintain this set here of "live" (reachable) periods.
      epoch.time_periods << sequencer.node.period

      sequencerQ = sequencer.node.original

      # The (> _) term is called the *needle*. A sequencer can have zero or more
      # needles. A needle is moved forward, with wraparound, for each crossing.

      crossings = Timing.crossingcnt(epoch.time_checkpoint, epoch.time_now, sequencer.node.period)
      crossings.times do
        successor = sequencerQ.transaction do |commit|
          steps = sequencerQ.items.move(2)
          steps.each_with_index do |step, index|
            pred = steps[(index - 1) % steps.size]

            # If this step was active, then disable it.
            Term.matchpi?(step, %{(> term_)}) do
              step = term
            end

            # If this step had an active predecessor, then enable it.
            Term.matchpi?(pred, %{(> _)}) do
              step = Term.of(:>, step)
            end

            key = 2 + index
            commit.with(key, step)
          end
        end

        sequencerQ = Term.of(successor)
      end

      patches << replace(sequencer, sequencerQ)
    end

    hg.each(Timing::Ticker) do |ticker|
      epoch.time_periods << ticker.node.period

      crossings = Timing.crossingcnt(epoch.time_checkpoint, epoch.time_now, ticker.node.period)
      next if crossings.zero? # Fast path

      # (ticker _ ⏏)
      patches << patch(ticker, {2, ticker.node.state + ticker.node.step*Term[crossings]})
    end

    hg.each(Timing::Timesource) do |timesource|
      next unless target = hg.empty_place?(timesource.addr, timesource.node.target)

      epoch.time_periods << timesource.node.period

      crossings = Timing.crossingcnt(epoch.time_checkpoint, epoch.time_now, timesource.node.period)
      next if crossings.zero?

      patches << target.write(Term.rep(timesource.node.message))
    end

    hg.each(Timing::Lfo) do |lfo|
      epoch.time_periods << lfo.node.period

      crossings = Timing.crossingcnt(epoch.time_checkpoint, epoch.time_now, lfo.node.period)
      next if crossings.zero? # Fast path out

      next unless readout = Timing.sample(lfo.node.waveform, epoch.time_now)

      # (lfo _ ⏏)
      patches << patch(lfo, {2, readout})
    end

    hg.each(Timing::PendingTimer) do |timer|
      deadline = (epoch.time_now + timer.node.delay).total_nanoseconds.to_u64

      instant = String.build do |io|
        Timing::UNIVERSE.to_s(io, base: 62)
        io << ':'
        deadline.to_s(io, base: 62)
      end

      # (timer _ ⏏)
      patches << patch(timer, {2, {:wait, instant}})
    end

    hg.each(Timing::WaitingTimer) do |timer|
      # If universe does not match, we restart the timer node.
      unless timer.node.universe == Timing::UNIVERSE
        # (timer _ ⏏)
        patches << patch(timer, {2, :start})
        next
      end

      if epoch.time_now < timer.node.deadline
        # Keep waiting
        epoch.time_deadlines << timer.node.deadline
        next
      end

      # We're past the deadline.
      # (timer _ ⏏)
      patches << patch(timer, {2, :ready})
    end

    hg.each(Timing::PendingCountdown) do |countdown|
      deadline = (epoch.time_now + countdown.node.delay).total_nanoseconds.to_u64

      instant = String.build do |io|
        Timing::UNIVERSE.to_s(io, base: 62)
        io << ':'
        deadline.to_s(io, base: 62)
      end

      # (countdown _ ⏏)
      patches << patch(countdown, {2, {:wait, 0, instant}})
    end

    hg.each(Timing::WaitingCountdown) do |timer|
      # If universe does not match, we restart the timer node.
      unless timer.node.universe == Timing::UNIVERSE
        # (countdown _ ⏏)
        patches << patch(timer, {2, :start})
        next
      end

      if epoch.time_now < timer.node.deadline
        # Keep waiting
        epoch.time_periods << timer.node.rate
        epoch.time_deadlines << timer.node.deadline

        crossings = Timing.crossingcnt(epoch.time_checkpoint, epoch.time_now, timer.node.rate)
        next if crossings.zero?

        progress = (epoch.time_now/timer.node.deadline).clamp(0.0..1.0)

        # (countdown _ (wait ⏏ _))
        patches << patch(timer, {2, 1, progress})
        next
      end

      # We're past the deadline.
      # (countdown _ ⏏)
      patches << patch(timer, {2, :ready})
    end

    hg.each(Timing::Clock) do |clock|
      epoch.time_periods << clock.node.rate

      crossings = Timing.crossingcnt(epoch.time_checkpoint, epoch.time_now, clock.node.rate)
      next if crossings.zero?

      readout = clock.node.format.format(Time.local(clock.node.location))

      # (clock _ ⏏)
      patches << patch(clock, {2, readout})
    end
  end

  def step(recognize_cache : RecognizeCache, world : Term, & : Hypergraph, Array(Patch) ->) : {Slice(Term), Term}
    root = recognize(recognize_cache, world)
    if root.is_a?(Inert)
      return Slice(Term).empty, world
    end

    history = Pf::Kit.stack_array(Term, 8)

    level = [] of ICircuit
    changes = Deque({ICircuit, Term}).new
    patches = [] of Patch

    MAX_STEP_DEPTH.times do |depth|
      assert level.empty?
      assert changes.empty?
      gather!(level, root, depth)
      break if level.empty?

      # TODO: in my sweetest dreams this is a parallel each. Currently though fn()
      # is not guaranteed to be thread-safe, nor is the choice of whether to go
      # parallel so easy. We'd need heuristics since it's not always cheap.
      while circuit = level.shift?
        hg = Hypergraph.new(seal(circuit))
        yield hg, patches
        next if patches.empty?

        # Merge patches proposed by the block so that we have one final patch
        # that we can apply to the circuit.
        patch, _ = D7.merge(patches.reject(PatchCancel)) do |node_id|
          response = hg.fetch(node_id)
          {response.node.original, response.node.merge_policy}
        end
        patches.clear

        next if patch.empty?

        router = NodeAddrRouter.build(patch) do |(node_id, rep)|
          response = hg.fetch(node_id)
          {response.addr, rep}
        end

        changes << {circuit, apply(hg.root, router)}
      end

      next if changes.empty?

      # Deliver desired changes to target circuits. Target circuits will replace
      # themselves with their successors
      history << world
      world = broadcast(changes, root, depth)
      assert changes.empty?, "broadcast() should have cleared changes"

      # If the patch changed something, we have to reparse to obtain the new tree.
      # This heavily relies on recognition cache for performance.
      root = recognize(recognize_cache, world)
      break if root.is_a?(Inert)
    end

    {history.to_unsafe_readonly_slice!, world}
  end

  def step(recognize_cache : RecognizeCache, node : Inert, & : Hypergraph, Array(Patch) ->) : {Slice(Term), Term}
    {Slice(Term).empty, node.original}
  end

  def step(ctx : Context, recognize_cache : RecognizeCache, world : Term) : Slice(Term)
    ctx.epoch do |epoch|
      _, world = step(recognize_cache, world) { |hg, patches| repair(hg, patches) }
      history, world = step(recognize_cache, world) { |hg, patches| advance(epoch, hg, patches) }
      _, world = step(recognize_cache, world) { |hg, patches| repair(hg, patches) }

      history.append(world)
    end
  end

  def replace(object : NodeRef, term : Term) : Patch
    PatchTable.assoc(object.id, term)
  end

  # Constructs an empty patch.
  def patch : Patch
    PatchTable.new.as(Patch)
  end

  # Constructs a patch that morphs node terms in *object* according
  # to *morphseq*.
  #
  # See also `Term.morph`.
  def patch(object : NodeRef, *morphseq) : Patch
    PatchTable.assoc(object.id, Term.morph(object.node.original, *morphseq)).as(Patch)
  end

  # Constructs patches for each object in *objects* using the block;
  # merges the resulting patches into one final patch.
  #
  # Conflicting changes from later patches (*objects*-wise, with greater index)
  # win over former ones.
  def txn(objects : Enumerable(T), & : T, Int32 -> Patch?) : Patch forall T
    patch = PatchTable.new

    objects.each_with_index do |object, index|
      contrib = yield object, index
      next if contrib.nil?

      if contrib.is_a?(PatchCancel)
        return PatchCancel.new
      end

      contrib.each do |key, value|
        patch = patch.assoc(key, value)
      end
    end

    patch
  end

  def txn(objects : Enumerable(Node), *morphseq) : Patch
    txn(objects) { |object| patch(object, *morphseq) }
  end

  # Shorthand for `txn` when all objects are patches already.
  def txn(objects : Enumerable(Patch)) : Patch
    txn(objects, &.itself)
  end

  # Shorthand that lets you list & merge multiple patches from the arguments.
  def txn(*objects : Patch) : Patch
    txn(objects)
  end
end

# TODO: add test:
#   (cell (space @data) {x: 100, y: 200})
#   (part (@data @x) {¦ ±x})
#   (backsys @data {¦ ±y} <> {y: ^(* y 2)})
#   (backsys @x ±n <> {n: ^(+ n 1)})
#   (changes (async @data limit: 3))
#   (changes (async @x limit: 3))
#
# TODO: add test:
#   (part (@workspace @people) {¦ people_})
#   (part (@people @jane) ⟨jane←{¦ name: "Jane"}⟩)
#   (part (@jane @age) {¦ ±age})
#   (backsys @age ±n <> {n: ^(+ n 1)})
#   (backsys @jane {¦ busy_} <> {(busy): ()})
#
#   (part (@people @busy) ⟨{¦ -busy_}⟩°)
#   (backsys {@:busy}
#     {¦ -busy_} <> {busy: true})
#
#   (cell (space @workspace)
#     {people:
#       ({name: "Jane", age: 25}
#        {name: "John", age: 30}
#        {name: "Barbara", age: 35})})
#
#   (changes (async @age))
#   (changes (async @jane))
#   (changes (async @people))
#   (changes (async @workspace))
#
# TODO: add test:
#   (form @xf
#     (outlet @x 100)
#     (outlet @y 200)
#     (outlet @z 300)
#     (field @foo 100)
#     (field @bar 200))
#
#   (reading @xf)
#   (backsys @xf
#     [±n _ _] <> {n: ^(+ n 1)}
#     [_ m_ _] <> {(m): ()}
#     [_ `m _] <> {m: 200}
#     {¦ ±foo} <> {foo: ^(+ foo 1)}
#     {¦ bar_} <> {(bar): ()}
#     (±n _* ⍊ -bar_) <> {bar: ^n})
#
# TODO: add test:
#   (form @xf
#     (outlet @x)
#     (outlet @y)
#     (outlet @z))
#
#   (cell @xs (1 2 3))
#   (feed (@xs back) (@xf front)) ;; clogs, vs. @xf back, which must work
#
# TODO: add test:
#  (form @xf
#    (group
#      (outlet @x)
#      (outlet (readonly (space @y))))
#    (outlet @z))
#
#  (cell @xs (1 2 3))
#  (feed (@xs front) (@xf back))
#  ;; Right now this clogs at the readonly outlet. I don't know how this should
#  ;; behave. Clogging seems fine.
#
# TODO: add test:
#   (form (space @xf)
#     (field @x 100)
#     (field @y 200))
#
#   (part (@xf @x) {¦ ±x})
#   (part (@xf @y) {¦ ±y})
#   (discard @x)
#   (discard @y)
#
# TODO: add to tests:
#   (cell @tasks (1 x y 2 3 z))
#   (cell @amount 1)
#   (supervisor (@tasks @task ±n - @pool {:@amount})
#     (backsys
#       {¦ ±task ±amount} <> {task: ^(+ task amount)}))
#   (nodes @pool
#     a
#     b
#     c)
#
# TODO: add to tests:
#   (cell @x 100)
#   (feed @x @xf)
#   (form @xf
#     (outlet @x))
#   (reading @xf)
#
# TODO: add to tests
#   (form @xf
#     (field @x 100)
#     (field @y 200))
#   (changes (sync @xf limit: 3))
#   (backsys @xf
#     {¦ ±x} <> {x: ^(+ x 1)}
#     {¦ ±y} <> {y: ^(- y 1)})
#
# TODO: add to tests:
#   (form @xf
#     (field @x 100)
#     (field @y 200))
#   (changes (sync @xf limit: 3))
#   (backsys @xf
#     {¦ ±x} <> {x: ^(+ x 1)}
#     {¦ ±y} <> {y: ^(- y 1)})
#
# TODO: add to tests:
#  (form @xf
#    (field @x 100)
#    (field @y 200))
#  (node (@change (changes _ change_ _*))
#    (changes (sync @xf limit: 3)))
#  (backsys @xf
#    {¦ ±x} <> {x: ^(+ x 1)}
#    {¦ ±y} <> {y: ^(- y 1)})
#  (discard @change)

circuit = ML.document(<<-'WWML')
(timer (2 s) start)
WWML

# - (hard) assembler: slot, rule
#   - prevent infinite recursion when instantiating call template, depth limit
#   - refactors. how to do less in Summary, esp. in terms of memory allocation?
#
# - (med) include bloom filter of edges in summary (128 bits?), use during traversal
# - (med) include bloom filter of {head, edge} pairs in summary (256 or 512 bits?), use during traversal
#
# --- Move to src/wirewright/future/rack (?)
#
# - Try running all the rack tests we can
#
# --- Try to implement MxRack on top of NeoRack. I'm especially interested in Automaton
#     since it's the buggiest part of the current MxRack impl. See if there's a way to
#     improve anything. How to implement/step() nested automata? How to deliver events
#     to them? How to consume them during Automaton step? How to put the circuit in the loop? ---
#
# - (med) tspace: sensor
# - (med) tspace: sensor many
# - (med) tspace: sensor queue
# - (med) tspace: sensor view
# - (med) tspace: sensor journal
# - (med) tspace: appearance
#
# - (hard) harmony: resource
# - (hard) harmony: path...reading, path...report
# - (hard) harmony: db
# - (hard) harmony: server
# - (hard) harmony: client
#
# --- all of Rack complete, all examples and tests should run ---
#
# - (med) use finger tree with Summary instead of children : Slice(Node)

signal = BlockingSignal.new
ctx = NeoRack::Context.new(signal)
recognize_cache = NeoRack::RecognizeCache.new

# Benchmark.ips do |x|
#   x.report("step") do
#     subframes = NeoRack.step(ctx, recognize_cache, circuit)
#     circuit = subframes.last
#     recognize_cache.sweep
#   end
# end

printed = nil
epoch = 0u64
loop do
  pred = circuit

  unless printed == circuit
    puts ML.display(circuit)
    printed = circuit
  end

  subframes = NeoRack.step(ctx, recognize_cache, circuit)
  circuit = subframes.last
  recognize_cache.sweep

  next unless pred == circuit

  if deadline = ctx.deadline?
    epoch = signal.wait_until(epoch, timeout: deadline - Time.instant)
    next
  end

  break unless ctx.pending?

  epoch = signal.wait(epoch)
end
