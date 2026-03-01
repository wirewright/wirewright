module Ww::M1
  # Implementation of the backmap engine.
  #
  # NOTE: You most likely want one of the `M1.backmap` overloads.
  #
  # The algorithm is, very generally, as follows:
  #
  # - Logs that M1's matching half gives us are clues about a structure -- which we,
  #   with our god's eye view -- know is the matchee. The backmap engine doesn't quite
  #   know this, though.
  # - Instead, it uses logs to materialize a term-like tree (see `Node`). Each log
  #   is a path through that tree (although not verbatim). In a sense, the backmap
  #   engine operates on a model of the matchee, built based on match logs. It does refer
  #   to the matchee afterwards, however, as it merges its response back where appropriate.
  # - Logs must be normalized before giving them to the engine, see `Log.normalize`.
  # - Mutations are routed toward their corresponding *endpoint* on backmap tree nodes
  #   (both logs and mutations are associated with names; we use those to do the routing).
  #   We use the term *mutation* (and derived) to refer to components of a backspec, as in
  #   `{x: ⏏^y⏏, y: ⏏^x⏏}`. Mutations do not have a name; they are *associated* with
  #   a name. Among other metadata, they carry e.g. multiplicity, which is written
  #   as `{⏏(x)⏏: ^y}`. This could be confusing because multiplicity is very close to
  #   the name while not actually being part of the mutation.
  # - We partition the tree into levels (each level populated/defined/delimited
  #   by `LevelNode`s), similar to breadth-first search.
  # - The levels are traversed bottom-up.
  # - When a level receives "attention" of the algorithm, each node in it computes
  #   its replacement proposal and attaches it to itself under `proposal`. The computation
  #   is strictly non-recursive: parents look at their children's attached `proposal`s
  #   to figure out their own one.
  # - The loop that goes bottom to top, giving each layer attention, we refer to as
  #   *backpropagation*, because it superficially resembles neural nets.
  # - `up` and `dn` lookups work similarly by traversing the tree -- top-down, bottom-up,
  #   etc. Their implementations can be somewhat confusing, because for performance, we do
  #   lookups on the same tree that we're currently working on -- both lookup and backprop
  #   may change the tree. However, their changes are in a sense "disjoint", in that each
  #   affects parts of the tree not yet reached by the other (`up`, operating above
  #   the current level); or is a pure observer of work already complete (`dn`, operating
  #   below the current level).
  module Backmap
    extend self

    # `Ref` is a scoped log name (as returned by e.g. `matches_and_logs`). You can
    # imagine `Ref`s as log names with a subscript. Consider, for instance,
    # the following backmap:
    #
    # ```wwml
    # (a_ b_) <> {a: ^b, b: ^a}
    # ```
    #
    # Here, *a* gets the reference *a₀* and *b* gets the reference *b₀*. This may
    # seem to make little sense, but then, consider the following backsystem:
    #
    # ```wwml
    # (a_ _ b_ _) <> {a: ^b, b: ^a}
    # (_ a_ _ b_) <> {a: ^b, b: ^a}
    # ```
    #
    # Blindly using *a* and *b* for both would cause a name clash even though
    # the underlying logs are not in conflict. We prevent such name clashes by
    # referring to the first backmap's *a*, *b* as *a₀*, *b₀*; and similarly,
    # *a₁*, *b₁* for the second backmap's *a*, *b*.
    #
    # That is, the first discriminatory component of a ref is the id of the agent
    # in the backsystem. We currently use the agent's index in the backsystem as
    # its id.
    #
    # Refs also include env ids, which let us distinguish different match envs
    # coming from the same agent. Currently this happens only for source patterns,
    # as in `⟨x_⟩° <> {...}` -- the agent is the same but each env is distinct.
    # In a sense, we pretend as if distinct agents were making those changes.
    defrecord Ref, name : Term, agent_id : UInt32, env_id : UInt32

    struct Ref
      # Constructs a reference into the same agent and env id as *proto*, but
      # with a different name. This is used to refer to another capture within
      # the same agent and env.
      def self.sibling(proto : Ref, name : Term) : Ref
        Ref.new(name, proto.agent_id, proto.env_id)
      end
    end

    # An endpoint is a mutable container for zero or more names and zero or one `Mut`s.
    struct Endpoint
      # :nodoc:
      def initialize(@refs : List(Ref)?, @muts : Mut | Array(Mut)?)
      end

      # Constructs an empty endpoint.
      def self.new : Endpoint
        new(refs: nil, muts: nil)
      end

      # :nodoc:
      def self.append(endpoint : Endpoint, mut : Mut) : Endpoint
        prev = endpoint.@muts

        case prev
        in Nil
          muts = mut
        in Mut
          muts = [prev, mut]
        in Array(Mut)
          prev << mut
          muts = prev
        end

        new(endpoint.@refs, muts)
      end

      # :nodoc:
      def self.append(endpoint : Endpoint, ref : Ref, arena : Arena(List(Ref), _)) : Endpoint
        new(List.append(arena, endpoint.@refs, ref), endpoint.@muts)
      end

      # Returns `true` if this endpoint has an associated mutation.
      def mutates? : Bool
        !@muts.nil?
      end

      # Returns `true` if this endpoint has *ref* as one of its refs.
      def includes?(ref : Ref) : Bool
        if refs = @refs
          if refs.includes?(ref)
            return true
          end
        end

        each_mut do |mut|
          if mut.ref == ref
            return true
          end
        end

        false
      end

      # Yields refs associated with this endpoint.
      #
      # The order of refs is implementation-defined.
      def each_ref(& : Ref ->) : Nil
        if refs = @refs
          refs.reverse_each { |ref| yield ref }
        end

        each_mut do |mut|
          yield mut.ref
        end
      end

      # Yields mutations associated with this endpoint.
      #
      # Their order is implementation-defined.
      def each_mut(& : Mut ->) : Nil
        case muts = @muts
        in Nil
        in Mut        then yield muts
        in Array(Mut) then muts.each { |mut| yield mut }
        end
      end

      def inspect(io)
        io << "Endpoint["

        if muts = @muts
          muts.inspect(io)
        end

        io << ":"

        if refs = @refs
          refs.join(io, ",") do |ref|
            io << ref.name << ref.agent_id.subscript << "₋" << ref.env_id.subscript
          end
        end

        io << "]"
      end
    end

    # Short for *mutation*, `Mut` represents a rewrite intent attached to an `Endpoint`.
    # Endpoints, in turn, are attached to nodes (namely `LevelNode`s).
    defcase Mut, adjs : Adj, mult : Mult, ref : Ref, env : Term::Dict, template : Term do
      @[Flags]
      enum Adj
        Post
        Local
      end

      enum Mult
        One
        Many
      end

      def inspect(io)
        adjs.each do |adj|
          io << adj
          io << " "
        end

        io << "mut"
        case mult
        in .one?
        in .many? then io << "*"
        end

        io << "@" << ref << "(⸤"
        io << template
        io << "⸥ <- "
        io << env
        io << ")"
      end
    end

    alias Node = Slot | Leaf

    defcase Slot,
      initial : Term,
      form : Dict | Blank | Nil,
      endpoint : Endpoint,
      depth : UInt32,
      proposal : Term::Rep? = nil,
      mutates : Bool = false,
      agents : Pf::USet32? = nil,
      mutation: true

    defcase Leaf,
      endpoint : Endpoint,
      depth : UInt32,
      proposal : Term::Rep? = nil,
      mutation: true

    # :nodoc:
    alias ImpliesDict = DictStep | DictInteriorStep

    # :nodoc:
    #
    # Steps that `Dict` understands.
    alias DictStep = Log::ExamineItemspart |
                     Log::ExaminePairspart

    defcase Dict,
      interior : DictInterior,
      itemspart : Endpoint,
      pairspart : Endpoint,
      mutation: true

    # :nodoc:
    #
    # Steps that `DictInterior` understands.
    alias DictInteriorStep = Log::ExamineKey |
                             Log::ExamineValue |
                             DictInteriorFanoutStep

    # :nodoc:
    #
    # Steps that `DictInterior` stores in its fanout. Equal steps from different
    # rewrite agents will proceed to the same successor node; distinct steps may
    # or may not trigger a conflict (i.e., we don't know whether they'll overlap
    # at mount-time; we only know they're distinct, and so they proceed to
    # different successor nodes).
    alias DictInteriorFanoutStep = Log::ExamineRange |
                                   Log::ExamineResidue |
                                   Log::InsertEntry |
                                   Log::InsertItem

    defcase DictInterior,
      initial : Term::Dict,
      depth : UInt32,
      entries = ListMap(Term, DictEntry).new,
      fanout = ListMap(DictInteriorFanoutStep, Node).new,
      proposal : Term::Dict? = nil,
      mutation: true

    # :nodoc:
    alias ImpliesBlank = Log::ExamineBlankName |
                         Log::ExamineBlankType

    defcase Blank, name : Node, type : Node

    defcase DictEntry, key : Node, value : Node

    # Nodes listed here form levels. The backmap engine proceeds bottom-up
    # through such levels.
    alias LevelNode = Node | DictInterior

    # Represents a conflict. The word is used in a very general way: it may not
    # necessarily be a conflict between *agent_ids*; the set may very well contain
    # just one agent, in which case we say the agent is in conflict with the backmap
    # engine itself.
    #
    # We assume a high-level control agent will handle conflicts. The goal of the agent
    # would be to withdraw one or more of conflicting *agents* such that no conflict
    # occurs; maximizing the number of active agents; reflecting user-expected order
    # in its decisions for withdrawal (i.e., withdraw lower-ranked first). The control
    # agent is usually implemented as backtracking search on top of a full backmap pass
    # (see `backmap`).
    #
    # NOTE: Since *agent_ids* is represented as a `USet32`, it is "automatically" sorted
    # by agent id.
    defrecord Conflict, agent_ids : Pf::USet32 do
      assert agent_ids.present?
    end

    def conflict(*ids : UInt32) : Conflict
      Conflict.new(Pf::USet32[*ids])
    end

    def conflict(*nodes) : Conflict
      Conflict.new(Backmap.agents(*nodes))
    end

    # :nodoc:
    def mutates?(node : Leaf) : Bool
      node.endpoint.mutates?
    end

    # :nodoc:
    def mutates?(node : Slot) : Bool
      node.mutates
    end

    {% if flag?(:docs) %}
      # Returns `true` if *node* or its subtree contains one or more
      # mutations (`Mut`s).
      def mutates?(node : Node) : Bool
      end
    {% end %}

    # Mount actions are executed on an endpoint when it is reached by `mount`.
    alias MountAction = MountMut | MountRef

    # Adds a `Mut` to the endpoint.
    struct MountMut
      def initialize(@mut : Mut)
      end

      def call(ctx : Context, endpoint : Endpoint) : Endpoint
        ctx.append(endpoint, @mut)
      end
    end

    # Adds a `Ref` to the endpoint.
    struct MountRef
      def initialize(@ref : Ref)
      end

      def call(ctx : Context, endpoint : Endpoint) : Endpoint
        ctx.append(endpoint, @ref)
      end
    end

    defrecord Mounted, mutates : Bool

    alias MountOut = Mounted

    # Populates the backmap tree starting at *root*. Constructs nodes according
    # to *log*.
    #
    # - *depth* determines the initial depth (you probably want `0`).
    # - *action* is the action to perform on the endpoint thus reached.
    #
    # Detects conflicts over the same endpoint in the tree.
    def mount(
      ctx : Context,
      root : Node,
      log : Log::SeqOne,
      depth : UInt32,
      action : MountAction,
    ) : MountOut
      mount(ctx, root, Log::SeqSlice.new(log), depth, action)
    end

    private def mount(ctx, node, steps : Log::SeqSlice, depth, action) : MountOut
      unless step = steps.first?
        return mount(ctx, node, action)
      end

      mount(ctx, node, step, steps + 1, depth, action)
    end

    private def mount(ctx, node : Slot, action) : MountOut
      node.endpoint = action.call(ctx, node.endpoint)
      node.mutates ||= node.endpoint.mutates?

      Mounted.new(mutates: node.mutates)
    end

    private def mount(ctx, node : Leaf, action) : MountOut
      node.endpoint = action.call(ctx, node.endpoint)

      Mounted.new(mutates: node.endpoint.mutates?)
    end

    private def mount(ctx, node : Leaf, step, steps, depth, action) : MountOut
      raise ArgumentError.new
    end

    private def mount(ctx, node : Slot, step : ImpliesDict, steps, depth, action) : MountOut
      unless form = node.form
        assert initial = node.initial.as_d?

        node.form = form = ctx.dict(initial, depth)
      end

      response = mount(ctx, form, step, steps, depth, action)
      node.mutates ||= response.mutates || node.endpoint.mutates?
      response
    end

    private def mount(ctx, node : Slot, step : ImpliesBlank, steps, depth, action) : MountOut
      unless form = node.form
        assert initial = node.initial.as_sym?
        assert blank = initial.blank?

        node.form = form = ctx.blank(Term.of(blank.name), Term.of(blank.type.blank), depth)
      end

      response = mount(ctx, form, step, steps, depth, action)
      node.mutates ||= response.mutates || node.endpoint.mutates?
      response
    end

    private def mount(ctx, form : Dict, step : Log::ExamineItemspart | Log::ExaminePairspart, steps, depth, action) : MountOut
      assert steps.empty?

      case step
      in Log::ExamineItemspart then form.itemspart = action.call(ctx, form.itemspart)
      in Log::ExaminePairspart then form.pairspart = action.call(ctx, form.pairspart)
      end

      Mounted.new(mutates: form.itemspart.mutates? || form.pairspart.mutates?)
    end

    # Steps other than ExamineItemspart, ExaminePairspart are routed towards
    # the interior.
    private def mount(ctx, form : Dict, step, steps, depth, action) : MountOut
      assert step.is_a?(DictInteriorStep)

      # NOTE: Remember that dict interior is one level deeper than Dict (and its
      # parent Slot).
      mount(ctx, form.interior, step, steps, depth + 1, action)
    end

    # NOTE: Remember than entries are one level deeper than DictInterior.
    private def mount(ctx, form : DictInterior, step : Log::ExamineKey | Log::ExamineValue, steps, depth, action) : MountOut
      unless entry = form.entries[step.key]?
        knode = ctx.slot(initial: step.key, form: nil, depth: depth + 1, endpoint: Endpoint.new)

        if value = form.initial[step.key]?
          # Value exists. This is the case for most backmaps, e.g.:
          #
          #   {¦ x_ y_} <> {x: 1, y: 2}
          #
          vnode = ctx.slot(initial: value, form: nil, depth: depth + 1, endpoint: Endpoint.new)
        else
          # Value does not exist. This is the case for backmaps such as:
          #
          #   {¦ -x_} <> {x: 100}
          #
          # There's no *x* in the matchee: that's what the pattern matches for! We
          # can't do anything beautiful about it: the value simply doesn't exist,
          # so we create an imaginary Leaf node which explodes if we try to do
          # anything that implies "reality"; but otherwise Leaf works just fine.
          vnode = ctx.leaf(Endpoint.new, depth: depth + 1)
        end

        entry = ctx.dict_entry(knode, vnode)
        form.entries = ctx.assoc(form.entries, step.key, entry)
      end

      case step
      in Log::ExamineKey   then mount(ctx, entry.key, steps, depth + 1, action)
      in Log::ExamineValue then mount(ctx, entry.value, steps, depth + 1, action)
      end
    end

    # NOTE: Remember that successors in fanout are one level deeper than DictInterior.

    private def mount(ctx, form : DictInterior, step : Log::ExamineRange, steps, depth, action) : MountOut
      unless successor = form.fanout[step]?
        range = form.initial.items(step.begin.to_i, step.end.to_i)
        successor = ctx.slot(Term.of(range), form: nil, depth: depth + 1, endpoint: Endpoint.new)
        form.fanout = ctx.assoc(form.fanout, step, successor)
      end

      mount(ctx, successor, steps, depth + 1, action)
    end

    private def mount(ctx, form : DictInterior, step : Log::ExamineResidue, steps, depth, action) : MountOut
      unless successor = form.fanout[step]?
        residue = Term.exclude(form.initial, step.removed)
        successor = ctx.slot(Term.of(residue), form: nil, depth: depth + 1, endpoint: Endpoint.new)
        form.fanout = ctx.assoc(form.fanout, step, successor)
      end

      mount(ctx, successor, steps, depth + 1, action)
    end

    private def mount(ctx, form : DictInterior, step : Log::InsertEntry | Log::InsertItem, steps, depth, action) : MountOut
      unless successor = form.fanout[step]?
        successor = ctx.slot(step.value, form: nil, depth: depth + 1, endpoint: Endpoint.new)
        form.fanout = ctx.assoc(form.fanout, step, successor)
      end

      mount(ctx, successor, steps, depth + 1, action)
    end

    # NOTE: Remember that the name and type of a blank is one level deeper than
    # Blank (and its parent Slot).

    private def mount(ctx, form : Blank, step : Log::ExamineBlankName, steps, depth, action) : MountOut
      mount(ctx, form.name, steps, depth + 1, action)
    end

    private def mount(ctx, form : Blank, step : Log::ExamineBlankType, steps, depth, action) : MountOut
      mount(ctx, form.type, steps, depth + 1, action)
    end

    private def mount(ctx, form : Blank, step, steps, depth, action) : MountOut
      raise ArgumentError.new
    end

    # Returns the set of ids of agents that mutate *nodes*.
    def agents(*nodes) : Pf::USet32
      Pf::USet32.transaction do |sink|
        nodes.each { |node| agents0(node, sink) }
      end
    end

    # :nodoc:
    def agents(node : Slot) : Pf::USet32
      if agents = node.agents
        return agents
      end

      # Cache agents on Slot because Slot needs agents in a hot path.
      agents = node.agents = Pf::USet32.transaction { |sink| agents0(node, sink) }
      agents
    end

    private def agents0(node : Endpoint, sink) : Nil
      node.each_mut do |mut|
        sink << mut.ref.agent_id
      end
    end

    private def agents0(node : Slot, sink) : Nil
      if agents = node.agents # cached
        agents.each { |id| sink << id }
        return
      end

      if form = node.form
        agents0(form, sink)
      end
      agents0(node.endpoint, sink)
    end

    private def agents0(node : Leaf, sink) : Nil
      agents0(node.endpoint, sink)
    end

    private def agents0(node : Dict, sink) : Nil
      agents0(node.interior, sink)
      agents0(node.itemspart, sink)
      agents0(node.pairspart, sink)
    end

    private def agents0(node : DictInterior, sink) : Nil
      node.entries.each { |_, entry| agents0(entry, sink) }
      node.fanout.each { |_, successor| agents0(successor, sink) }
    end

    private def agents0(node : DictEntry, sink)
      agents0(node.key, sink)
      agents0(node.value, sink)
    end

    private def agents0(node : Blank, sink)
      agents0(node.name, sink)
      agents0(node.type, sink)
    end

    # Represents a dict patch.
    #
    # Instead of directly modifying dicts during mutation, the backmap engine first
    # records *patches*. This lets the engine detect conflicts, and thus refuse
    # to commit.
    alias Patch = Assoc | Dissoc | Replace | ReplaceRange

    # Creates an entry. The entry must not exist.
    defrecord Assoc, key : Term, value : Term

    # Removes an entry. The entry must exist.
    defrecord Dissoc, key : Term

    # Replaces an entry. The entry must exist.
    defrecord Replace, key : Term, value : Term

    # Replaces a range of zero or more items with zero or more other items.
    # *end* is exclusive. *ord* defines ordering among ranges with the same
    # *begin* and *end*.
    defrecord ReplaceRange, begin : UInt32, end : UInt32, ord : UInt32, rep : Term::Rep do
      assert @begin <= @end
    end

    # :nodoc:
    def keys(patch : Assoc, &)
      yield patch.key
    end

    # :nodoc:
    def keys(patch : Dissoc, &)
      yield patch.key
    end

    # :nodoc:
    def keys(patch : Replace, &)
      yield patch.key
    end

    # :nodoc:
    def keys(patch : ReplaceRange, &)
      (patch.begin...patch.end).each { |index| yield Term.of(index) }
    end

    {% if flag?(:docs) %}
      # Yields keys affected by *patch*.
      def keys(patch : Patch, & : Term ->)
      end
    {% end %}

    # :nodoc:
    def conflicts?(p : Assoc | Replace, q : Assoc | Replace) : Bool
      p.key == q.key && p.value != q.value
    end

    private def conflicts?(p : ReplaceRange, key : Term) : Bool
      return false unless n = key.as_n?
      return false unless index = n.index32?

      (p.begin...p.end).includes?(index)
    end

    # :nodoc:
    def conflicts?(p : ReplaceRange, q : Assoc | Dissoc | Replace) : Bool
      conflicts?(p, q.key)
    end

    # :nodoc:
    def conflicts?(p : Assoc | Dissoc | Replace, q : ReplaceRange) : Bool
      conflicts?(q, p)
    end

    # :nodoc:
    def conflicts?(p : ReplaceRange, q : ReplaceRange) : Bool
      if {p.begin, p.end, p.rep} == {q.begin, q.end, q.rep}
        return false
      end

      (p.begin...p.end).intersects?(q.begin...q.end)
    end

    # :nodoc:
    def conflicts?(p, q) : Bool
      # NOTE: There is usually a very small amount of keys (< 10), so a quadratic
      # intersection check is OK.
      keys(p) do |k0|
        keys(q) do |k1|
          return true if k0 == k1
        end
      end

      false
    end

    {% if flag?(:docs) %}
      # Returns `true` if two patches *p* and *q* are expected to modify the same keys.
      def conflicts?(p : Patch, q : Patch) : Bool
      end
    {% end %}

    struct Consensus(T)
      # :nodoc:
      defrecord Ambiguous
      # :nodoc:
      defrecord Settled(T), object : T

      # :nodoc:
      def initialize(@state : Ambiguous | Settled(T))
      end

      def self.[](object : T)
        new(Settled.new(object))
      end

      def value? : T?
        return unless state = @state.as?(Settled(T))

        state.object
      end

      def add(object : T) : Consensus(T)
        return self unless value = value?

        if value == object
          return self # They're equal, consensus is maintained.
        end

        # They're different, consensus is broken.
        Consensus(T).new(Ambiguous.new)
      end
    end

    # Represents a *mutation context*, often simply abbreviated as *µ*.
    #
    # Where you see mutation context-or-nil, this means that function or method
    # supports disabling mutation (i.e., ignoring user mutations and working based
    # on the proposals currently in the tree alone).
    class MutContext
      getter maxdepth : UInt32

      def initialize(@ctx : Context, @maxdepth)
        # @depth sections the tree in half horizontally into an upper (`@ctx.hi`) and
        # lower (`@ctx.lo`) halves. We usually think of `@depth` as belonging to
        # the upper half.
        @depth = @maxdepth
        @hiset = false
      end

      private def each_node(*, depth : Int, & : LevelNode ->)
        @ctx.nodes.each(within: @ctx.offsets[depth]...@ctx.offsets[depth + 1]?) do |node|
          yield node
        end
      end

      # Looks up *ref* using the *up-to-date*, alternatively top-down strategy.
      #
      # This method first searches for up-to-date *ref* in the lower half of
      # the tree (as per `@depth`, excluding `@depth`).
      #
      # If not found, this method continues to search in the upper part of
      # the tree, including the level at `@depth`, by disabling mutations and
      # synthesizing values higher in the tree based on the ones lower down.
      #
      # Finally, if the upper half does not contain *ref*, too, this method
      # indicates absence by returning `nil`.
      #
      # This method works only if one underlying log corresponds to *ref*, or if
      # they all agree on the value. Otherwise, this method returns `nil`. In other
      # words, this method is blind to "pluralities" corresponding to *ref*.
      def up?(ref : Ref) : Term?
        if cons = @ctx.lo[ref]?
          return cons.value?
        end

        hi = @ctx.hi

        unless @hiset
          assert hi.empty?

          proposals = Pf::Kit.stack_array(Term::Rep | Term::Dict?, 8)

          (0..@depth).reverse_each do |depth|
            each_node(depth: depth) do |node|
              # Remember the proposal at active depth. It may be useful for levels
              # above so we can't stomp over it.
              if depth == @depth
                proposals << node.proposal
              end

              Backmap.propose(µ: nil, node: node)

              next unless node.is_a?(Node)
              next unless proposal = node.proposal

              value = Term.collapse(proposal)

              node.endpoint.each_ref do |ref|
                hi[ref] = hi[ref]?.try(&.add(value)) || Consensus[value]
              end
            end
          end

          # Restore proposals of nodes at the active depth after we're done.
          index = 0
          each_node(depth: @depth) do |node|
            if node.is_a?(DictInterior)
              node.proposal = proposals[index].as(Term::Dict?)
            else
              node.proposal = proposals[index].as(Term::Rep?)
            end
            index += 1
          end

          @hiset = true
        end

        return unless cons = hi[ref]?

        cons.value?
      end

      # Looks up *ref* using the *up-to-date*, bottom-up strategy. The name `dn?`
      # is used for symmetry with `up?` and for brevity.
      #
      # This method searches for *ref* starting at the bottom of the tree and going
      # up by level until but excluding the level at `@depth`.
      def dn?(ref : Ref) : Term?
        return if @depth == @maxdepth # Can't look down at the bottom.

        assert @depth < @maxdepth

        (@depth + 1..@maxdepth).reverse_each do |depth|
          needle = nil

          proposals = Pf::Kit.stack_array(Term::Rep | Term::Dict?, 8)

          each_node(depth: depth) do |node|
            next unless node.is_a?(Node)
            next unless ref.in?(node.endpoint)

            # Remember and restore the proposal after we're done, upper levels (including
            # those *we*'re about to look at) may still rely on its validity.
            #
            # NOTE: We need to propose() because *node*'s current proposal accounts for muts,
            # and `dn` doesn't want muts in the level it's looking at at (but is fine with
            # muts below that, that's why we reset).
            proposal0 = node.proposal
            begin
              Backmap.propose(µ: nil, node: node)
              next unless proposal = node.proposal
            ensure
              node.proposal = proposal0
            end

            # Give up if there are multiple proposals with the same name.
            return if needle

            needle = Term.collapse(proposal)
          end

          return needle if needle
        end
      end

      # Performs a full backpropagation pass over the tree.
      #
      # NOTE: we provide *tree* for debugging purposes only: it is sometimes useful
      # to print it here to see how backpropagation proceeds. Traversal of the tree
      # occurs through `each_node`, which iterates over an array of nodes prepared
      # ahead-of-time for proper ordering (bottom up breadth-first search).
      def backprop(__tree) : Proposed | Conflict
        assert @depth == @maxdepth
        assert @ctx.lo.empty?

        loop do
          assert @ctx.hi.empty? && !@hiset

          # pp __tree
          # gets

          each_node(depth: @depth) do |node|
            case result = Backmap.propose(self, node)
            in Conflict then return result
            in Proposed
            end
          end

          break if @depth.zero?

          # NOTE: the two passes are necessary for nodes at the same depth to not
          # see the modified `lo`, which they refer to during propose().
          each_node(depth: @depth) do |node|
            next unless node.is_a?(Node)
            next unless proposal = node.proposal

            value = Term.collapse(proposal)

            node.endpoint.each_ref do |ref|
              @ctx.lo[ref] = @ctx.lo[ref]?.try(&.add(value)) || Consensus[value]
            end
          end

          @depth -= 1
          @ctx.hi.clear
          @hiset = false
        end

        # pp __tree
        # gets

        Proposed.new
      end
    end

    private def rep(µ : MutContext, mut : Mut, this : Term?) : Term::Rep
      eval = Alloy::Eval.new do |expr, default, _, issues|
        Term.case(expr) do
          matchpi %{(up capture_)} do
            unless value = µ.up?(Ref.sibling(mut.ref, capture)) || mut.env[capture]?
              issues.major { "unrecognized capture #{capture} (top-down lookup)" }
              value = Term.of(:literal, expr)
            end

            value
          end

          matchpi %{(dn capture_)} do
            unless value = µ.dn?(Ref.sibling(mut.ref, capture)) || mut.env[capture]?
              issues.major { "unrecognized capture #{capture} (bottom-up lookup)" }
              value = Term.of(:literal, expr)
            end

            value
          end

          otherwise { default.call(issues) }
        end
      end

      env = mut.env
      if mut.adjs.local?
        env = env.with(:it, this)
      end

      # FIXME: backmaps must support Issue::Sink I suppose. We can't just throw
      # issues out like this.
      expansion, _ = Alloy.render0(env, mut.template, eval: eval, severity: :quiet)

      unless term = expansion.single?
        # (x_ _ _) <> {x: (^splice a b c)}, (100 200 300) -> (a b c 200 300)
        return expansion
      end

      if mut.mult.one?
        # (x_ _ _) <> {x: a}, (100 200 300) -> (a 200 300)
        return Term.rep(term)
      end

      assert mut.mult.many?

      unless dict = term.as_d?
        # (x_ _ _) <> {(x): a}, (100 200 300) -> (a 200 300)
        return Term.rep(term)
      end

      # (x_ _ _) <> {(x): (a b c)}, (100 200 300) -> (a b c 200 300)
      Term.rep(dict.items)
    end

    # Holds the representations for the results of rendering, as well as auxiliary
    # functions to operate on them. Rendering has three possible results: `None`,
    # `Some`, and `Conflict` (used here in the sense of "rendering possible, but
    # conflicts among participant agents must be resolved first").
    module Render
      extend self

      # Represents the absence of a render. This usually means there's no mutation
      # attached to the rendered node (subtree), so it should be left unchanged.
      # Thus, render may return `None`.
      defrecord None

      # Represents a successful render.
      #
      # NOTE: Rendering isn't tied to a specific *T*, but usually it is a `Rep`.
      defrecord Some(T), object : T

      def none : None
        None.new
      end

      def some(object) : Some
        Some.new(object)
      end

      def one(object) : Some
        some(Term.rep(Term.of(object)))
      end
    end

    def render(µ : MutContext?, endpoint : Endpoint, this : Term?, & : Term::Rep -> _)
      return Render.none unless µ

      memo = nil
      endpoint.each_mut do |mut|
        unless object = yield rep(µ, mut, this)
          return conflict(mut.ref.agent_id)
        end

        if memo.nil?
          memo = {mut, object}
          next
        end

        prev_mut, prev_object = memo
        unless prev_object == object
          return conflict(prev_mut.ref.agent_id, mut.ref.agent_id)
        end
      end

      return Render.none unless memo

      _, object = memo
      Render.some(object)
    end

    def render(µ : MutContext?, endpoint : Endpoint, this : Term?)
      render(µ, endpoint, this, &.itself)
    end

    alias RenderOut = Render::None | Render::Some(Term::Rep) | Conflict

    # TODO: Right now we erase multiplicity, neither can we match on it; we must
    # not erase multiplicity and instead allow a three-argument form of (%symbol blank _ _ _),
    # where the third argument matches the multiplicity (`_` represents one, `_*` represents
    # zero or more, `_+` represents one or more). This is niche enough not to be of immediate
    # concern, but for completeness, we should support this eventually.
    def render(µ : MutContext?, form : Blank) : RenderOut
      return Render.none unless namep = form.name.proposal
      return Render.none unless typep = form.type.proposal

      case namep.size
      when 0
        # {(name): ()} means remove name (make blank nameless).
        name = Term::Sym.empty
      when 1
        name = namep.first.as_sym?
      end

      return conflict(form.name) unless name

      # name : Term::Sym

      case typep.size
      when 0
        # {(type): ()} means remove type (turning blank into nonblank).
        return Render.one(name)
      when 1
        if typesym = typep.first.as_sym?
          type = TermType.parse?(typesym)
        end
      end

      return conflict(form.type) unless type

      # name : Term::Sym
      # type : TermType
      Render.one(Term::Sym.blank(name.to(String), type, mult: :one))
    end

    def render(µ : MutContext?, form : Dict) : RenderOut
      return Render.none unless interior = form.interior.proposal

      # interior : Term::Dict

      dict0 = form.interior.initial

      itemspart = render(µ, form.itemspart, this: Term.of(dict0.itemspart)) do |proposal|
        term = Term.collapse(proposal)
        next unless dict = term.as_d?
        next unless dict.itemsonly?

        dict
      end

      pairspart = render(µ, form.pairspart, this: Term.of(dict0.pairspart)) do |proposal|
        term = Term.collapse(proposal)
        next unless dict = term.as_d?
        next unless dict.pairsonly?

        dict
      end

      case {itemspart, pairspart}
      in {Conflict, _}
        itemspart
      in {_, Conflict}
        pairspart
      in {Conflict, Conflict}
        conflict(itemspart.agents + pairspart.agents)
      in {Render::None, Render::None}
        # Itemspart and pairspart are not mutated in any way. This is true for 99%
        # of real-world usage (if not more!) So we don't want to waste time union'ing
        # and so on.
        Render.one(interior)
      in {Render::None, Render::Some}
        # (%partition _ pairs_) <> {pairs: {x: 100, y: 200}}
        Render.one(Term.union(interior.itemspart, pairspart.object))
      in {Render::Some, Render::None}
        # (%partition items_ _) <> {items: (1 2 3)}
        Render.one(Term.union(itemspart.object, interior.pairspart))
      in {Render::Some, Render::Some}
        # (%partition items_ pairs_) <> {items: (1 2 3), pairs: {x: 100, y: 200}}
        Render.one(Term.union(itemspart.object, pairspart.object))
      end
    end

    # Confirms that the `proposal` attribute of a node has been set.
    defrecord Proposed

    alias ProposeOut = Proposed | Conflict

    # NOTE: propose methods must clear the proposal before running! Otherwise
    # early exits mess everything up! These kinds of hacks are solved by value
    # semantics, in theory; but in practice, here, at the very bottom of Wirewright,
    # we really want to be performant!

    private def post?(endpoint : Endpoint) : Bool
      endpoint.each_mut do |mut|
        return false unless mut.adjs.post?
      end
      true
    end

    def propose(µ : MutContext?, node : Slot) : ProposeOut
      node.proposal = nil

      result = render(µ, node.endpoint, this: node.initial)
      if (form = node.form) && (result.is_a?(Render::None) || !post?(node.endpoint))
        form_result = render(µ, form)
      else
        form_result = Render.none
      end

      if result.is_a?(Render::Some) && form_result.is_a?(Render::Some) && result.object != form_result.object
        agents = agents(node)
        unless agents.size == 1
          return Conflict.new(agents)
        end
      end

      if result.is_a?(Render::None)
        result = form_result
      end

      case result
      in Conflict     then return result
      in Render::None then node.proposal = Term.rep(node.initial)
      in Render::Some then node.proposal = result.object
      end

      Proposed.new
    end

    def propose(µ : MutContext?, node : Leaf) : ProposeOut
      node.proposal = nil

      result = render(µ, node.endpoint, this: nil)

      case result
      in Conflict then return result
      in Render::None
        # Consider the following backmap:
        #
        #   {¦ -x_} <> {x: 10}
        #
        # Here, the entry node for `x` has a `Leaf` value. Its endpoint will hold a Mut
        # corresponding to `x: 10`. This will eventually lead to the Leaf node proposing
        # `10`. On the other hand, in the following backmap:
        #
        #   {¦ -x_} <> {}
        #
        # ... the leaf endpoint will be empty. An empty endpoint means render(_, Endpoint)
        # doesn't have anything to do, which in turn means Leaf's proposal is `nil`, which
        # in turn means the levels above must handle `nil`s somehow, to account for
        # situations like this one. The best thing to do is to ignore nil-proposals.
        # In this particular case, we can ignore the entry `x: <nil proposal node>`
        # by simply not inserting it into the dictionary.
      in Render::Some
        node.proposal = result.object
      end

      Proposed.new
    end

    alias PatchArray = Pf::Kit::HybridArray({Patch, Node | DictEntry}, 16)

    defrecord ProposeContext, patches : PatchArray, dict : Term::Dict, subtree : Node

    def propose(µ : MutContext?, node : DictInterior) : ProposeOut
      node.proposal = nil

      dict = node.initial
      patches : PatchArray = Pf::Kit.stack_array({Patch, Node | DictEntry}, 16)

      node.fanout.each do |step, successor|
        # If successor is not mutated, we need not devote any attion to it.
        next unless mutates?(successor)
        next unless proposal = successor.proposal

        unless propose?(ProposeContext.new(patches, dict, successor), step, proposal)
          return conflict(successor)
        end
      end

      node.entries.each do |key0, entry|
        # If neither key nor value is mutated, accounting for them could cause
        # spurious conflicts, so we skip them.
        next unless mutates?(entry.key) || mutates?(entry.value)

        keyp = entry.key.proposal
        valuep = entry.value.proposal

        if keyp && Term.changes?(key0, after: keyp)
          # valuep : Nil  (%entry k_string _) <> {k: (seen ^k)}
          # valuep : Rep  (%entry k_string v_) <> {k: (seen ^k), v: (seen ^v)}
          valuep ||= Term.rep(dict[key0])
          if valuep.empty?
            # Removal wins: (%entry k_string v_) <> {k: (seen ^k), (v): ()}
            patches << {Dissoc.new(key0), entry.value}
            next
          end

          value = Term.collapse(valuep)

          # Key changed.
          patches << {Dissoc.new(key0), entry.key}

          # Implement broadcast value behavior, as demonstrated by:
          #
          #   (%entry k_ foo) <> {(k): (a b c)}
          #
          # Running this on {x: foo, y: bar}, you get: {a: foo, b: foo, c: foo, y: bar}.
          keyp.each do |dst|
            patches << {Assoc.new(dst, value), entry.key}
          end

          # If value also changed, then the Assocs we've emitted above are enough.
          next
        end

        # Key is the same.

        next unless valuep

        if index = dict.index32?(key0)
          if term = valuep.single?
            # (x_ _) <> {x: 100}
            patches << {Replace.new(key0, term), entry.value}
            next
          end
          # (x_ _) <> {(x): (a b c)}
          # (x_ _) <> {(x): ()}
          # etc.
          patches << {ReplaceRange.new(index, index + 1, ord: index, rep: valuep), entry.value}
          next
        end

        if valuep.empty?
          # {¦ x_} <> {(x): ()}
          patches << {Dissoc.new(key0), entry.value}
          next
        end

        # {¦ x_} <> {x: 100}  {¦ x_} <> {(x): (a b c)}
        patches << {Replace.new(key0, Term.collapse(valuep)), entry.value}
      end

      patches.each_with_index do |(p, srcnode0), i|
        patches.each_with_index do |(q, srcnode1), j|
          next if i == j
          next unless conflicts?(p, q)
          next unless agents(srcnode0) == agents(srcnode1)

          # For any rule there's an exception! Of course! . . .

          case {p, q}
          when {ReplaceRange, ReplaceRange}
            # If we have two ranges, they are *really* in conflict only if different agents
            # are involved. The same (group of) agents can't conflict with itself on
            # ReplaceRange, assuming the ords are different.
            next unless p.ord == q.ord
          when {ReplaceRange, Replace}, {Replace, ReplaceRange}
            # These are never in conflict for a single agent because patch application
            # is staged, so ReplaceRanges win over Replaces.
            next
          end

          return conflict(srcnode0, srcnode1)
        end

        if p.is_a?(Assoc) && dict.includes?(p.key)
          return conflict(srcnode0)
        end
      end

      replacements = Pf::Kit.stack_array(ReplaceRange, 4)

      dict = dict.transaction do |commit|
        patches.each do |patch, _|
          case patch
          in Assoc, Replace
            commit.with(patch.key, patch.value)
          in Dissoc
            commit.without(patch.key)
          in ReplaceRange
            replacements << patch
          end
        end
      end

      if replacements.present?
        replacements.unstable_sort_by! { |r| {r.begin, r.end, r.ord} }
        replacements.reverse_each do |r|
          dict = dict.replace(r.begin...r.end, r.rep)
        end
      end

      node.proposal = dict

      Proposed.new
    end

    def propose?(ctx, step : Log::ExamineRange, proposal : Term::Rep) : Bool
      ctx.patches << {ReplaceRange.new(step.begin, step.end, step.ord, proposal), ctx.subtree}

      true # ok
    end

    def propose?(ctx, step : Log::ExamineResidue, proposal : Term::Rep) : Bool
      residue = Term.collapse(proposal)
      unless residue = residue.as_d?
        return false # error
      end

      dict = ctx.dict
      case step.part
      in .any? # undifferentiated
      in .itemspart?
        dict = dict.itemspart
      in .pairspart?
        dict = dict.pairspart
      end

      dict.each_entry do |key, _|
        next if key.in?(step.removed) || key.in?(residue)

        ctx.patches << {Dissoc.new(key), ctx.subtree}
      end

      residue.each_entry do |key, value|
        unless key.in?(dict)
          ctx.patches << {Assoc.new(key, value), ctx.subtree}
          next
        end

        if key.in?(step.removed)
          return false # error
        end

        ctx.patches << {Replace.new(key, value), ctx.subtree}
      end

      true # ok
    end

    def propose?(ctx, step : Log::InsertEntry, proposal : Term::Rep) : Bool
      # The subtree of *step* must have at least one mutation, otherwise, the entry
      # is not inserted. See, for example:
      #
      #   {¦ x⋮ 100} <> {x: 100}
      #
      # Here, despite the same value as in the optional, *x* does have an associated
      # mut, therefore, we interpret this as the user intending to create *x*. On the other
      # hand, here:
      #
      #   {¦ x⋮ 100} <> {}
      #
      # There is no mut associated with *x*, or its subtree. We interpret this as
      # the user not wanting to create *x*. Similarly, here:
      #
      #   {¦ x: (%optional (1 2) (a_ b_))} <> {b: 10}
      #
      # ... there is a mut for *b*, so we create *x*. On the other hand, here:
      #
      #   {¦ x: (%optional (1 2) (a_ b_))} <> {}
      #
      # ... the subtree of *x* has no muts, and therefore, we do not create it.
      unless mutates?(ctx.subtree)
        return true # ok
      end

      # Quite rare but sometimes useful: {¦ x⋮ 100} <> {(x): ()}, it must not
      # create the entry.
      if proposal.empty?
        return true # ok
      end

      ctx.patches << {Assoc.new(step.key, Term.collapse(proposal)), ctx.subtree}

      true # ok
    end

    def propose?(ctx, step : Log::InsertItem, proposal : Term::Rep) : Bool
      # Ditto as the above:
      #
      #   ((%optional 0 x_)) <> {x: 0}
      #   ((%optional (1 2) (a_ b_))) <> {a: 0}
      #
      # ... inserts the item, whereas:
      #
      #   ((%optional 0 x_)) <> {}
      #   ((%optional 0 x_)) <> {(x): ()}
      #   ((%optional (1 2) (a_ b_))) <> {}
      #
      # ... does not.
      unless mutates?(ctx.subtree)
        return true # ok
      end

      # The following must not insert the item:
      #
      #   ((%optional 0 x_)) <> {(x): ()}
      #   ((%optional (1 2) x←(a_ b_))) <> {(x): ()}
      #   ... etc.
      if proposal.empty?
        return true # ok
      end

      ctx.patches << {ReplaceRange.new(begin: step.index, end: step.index, ord: step.ord, rep: proposal), ctx.subtree}

      true # ok
    end

    # Context is mainly involved in managing memory for the backmap engine. It
    # holds objects reused throughout the algorithm (e.g. the backmap tree itself).
    class Context
      private alias MutArena = Arena(Mut, 8)
      private alias SlotArena = Arena(Slot, 16)
      private alias LeafArena = Arena(Leaf, 8)
      private alias DictArena = Arena(Dict, 8)
      private alias DictInteriorArena = Arena(DictInterior, 8)
      private alias DictEntryArena = Arena(DictEntry, 8)
      private alias DictEntryMapArena = Arena(List({Term, DictEntry}), 16)
      private alias DictFanoutMapArena = Arena(List({DictInteriorFanoutStep, Node}), 8)
      private alias RefListArena = Arena(List(Ref), 8)
      private alias LevelNodeArray = Pf::Kit::HybridArray(LevelNode, 16)
      private alias OffsetArray = Pf::Kit::HybridArray(Int32, 16)
      private alias ConsensusRefMap = HybridMap(Ref, Consensus(Term))

      # :nodoc:
      #
      # WARNING: Access valid only during `µ`.
      getter nodes : LevelNodeArray

      # :nodoc:
      #
      # Jump table. Index is level. Value is the index of the first node in
      # that level in `nodes`.
      #
      # WARNING: Access valid only during `µ`.
      getter offsets : OffsetArray

      # :nodoc:
      #
      # Reused by `µ`.
      getter hi : ConsensusRefMap

      # :nodoc:
      #
      # Reused by `µ`.
      getter lo : ConsensusRefMap

      # :nodoc:
      def initialize(
        @muts : MutArena,
        @slots : SlotArena,
        @leaves : LeafArena,
        @dicts : DictArena,
        @interiors : DictInteriorArena,
        @entries : DictEntryArena,
        @refs : RefListArena,
        @entry_maps : DictEntryMapArena,
        @fanout_maps : DictFanoutMapArena,
        @hi, @lo, @nodes, @offsets,
      )
        @maxdepth = 0u32
      end

      # Constructs an instance of `Context` valid for the lifetime of the block.
      # Returns the block's result.
      #
      # WARNING: No checks are made with respect to the lifetime of `Context`.
      # Using `Context` past its lifetime will result in undefined behavior.
      def self.scope(& : Context ->)
        nested_scopes(
          MutArena, SlotArena, LeafArena, DictArena, DictInteriorArena,
          DictEntryArena, RefListArena, DictEntryMapArena, DictFanoutMapArena,
          ConsensusRefMap, ConsensusRefMap,
        ) do |*args|
          nodes = Pf::Kit.stack_array(LevelNode, 16)
          offsets = Pf::Kit.stack_array(Int32, 16)
          ctx = stack_alloc self.new(*args, nodes, offsets)
          yield ctx
        end
      end

      # Constructs an object using memory managed by this context. The returned object
      # is valid for the lifetime of this context.
      def mut(*args, **kwargs) : Mut
        @muts.construct(*args, **kwargs)
      end

      # :ditto:
      def slot(*args, **kwargs) : Slot
        slot = @slots.construct(*args, **kwargs)
        @nodes << slot
        @maxdepth = Math.max(@maxdepth, slot.depth)
        slot
      end

      # :ditto:
      def leaf(*args, **kwargs) : Leaf
        leaf = @leaves.construct(*args, **kwargs)
        @nodes << leaf
        @maxdepth = Math.max(@maxdepth, leaf.depth)
        leaf
      end

      # :ditto:
      def dict_interior(*args, **kwargs) : DictInterior
        interior = @interiors.construct(*args, **kwargs)
        @nodes << interior
        @maxdepth = Math.max(@maxdepth, interior.depth)
        interior
      end

      # :ditto:
      def dict(*args, **kwargs) : Dict
        @dicts.construct(*args, **kwargs)
      end

      # :ditto:
      def dict(initial : Term::Dict, depth : Int) : Dict
        dict(
          interior: dict_interior(initial, depth + 1),
          itemspart: Endpoint.new,
          pairspart: Endpoint.new,
        )
      end

      # :ditto:
      def dict_entry(*args, **kwargs) : DictEntry
        @entries.construct(*args, **kwargs)
      end

      # :ditto:
      def blank(*args, **kwargs) : Blank
        # Blanks are exceedingly rare in practice so we don't bother wasting stack space
        # on an arena for them.
        Blank.new(*args, **kwargs)
      end

      # :ditto:
      def blank(name : Term, type : Term, depth : Int)
        blank(
          name: slot(initial: name, form: nil, depth: depth + 1, endpoint: Endpoint.new),
          type: slot(initial: type, form: nil, depth: depth + 1, endpoint: Endpoint.new),
        )
      end

      # Constructs an instance of `MutContext` valid for the lifetime of the block.
      # Returns the block's result.
      def µ(& : MutContext ->)
        assert @nodes.present?

        if @offsets.empty?
          @nodes.unstable_sort_by!(&.depth)
          @offsets << 0

          @nodes.each_with_index do |node, index|
            next if @offsets.size - 1 == node.depth
            assert @offsets.size == node.depth
            @offsets << index
          end
        end

        @hi.clear
        @lo.clear

        µ = stack_alloc MutContext.new(self, @maxdepth)
        yield µ
      end

      # Returns a copy of *endpoint* with *mut* set as the associated mutation.
      #
      # This endpoint must not have a mutation
      def append(endpoint : Endpoint, mut : Mut) : Endpoint
        Endpoint.append(endpoint, mut)
      end

      # Returns a copy of *endpoint* with *ref* added as one of the associated refs.
      def append(endpoint : Endpoint, ref : Ref) : Endpoint
        Endpoint.append(endpoint, ref, @refs)
      end

      # Returns a copy of *map* containing an association of *key* with *value*.
      def assoc(map : ListMap(Term, DictEntry), key : Term, value : DictEntry)
        ListMap.assoc(@entry_maps, map, key, value)
      end

      # :ditto:
      def assoc(map : ListMap(DictInteriorFanoutStep, Node), key : DictInteriorFanoutStep, value : Node)
        ListMap.assoc(@fanout_maps, map, key, value)
      end
    end

    # Represents a rewrite agent, which is a very general way of saying "backmap
    # inside a backsystem".
    #
    # *T* must be `Enumerable({Term::Dict, LogList})`.
    defrecord Agent(T), matches : T, backspec : Term::Dict

    private def muti(expr : Term, &assign : Term, Mut::Adj, Mut::Mult ->)
      Term.case(expr) do
        matchpi %{(post arg_)} do
          muti(arg) do |name, adj, mult|
            assign.call(name, adj | Mut::Adj::Post, mult)
          end
        end

        matchpi %{(local arg_)} do
          muti(arg) do |name, adj, mult|
            assign.call(name, adj | Mut::Adj::Local, mult)
          end
        end

        matchpi %{(forall args_*)} do
          args.items.each do |arg|
            muti(arg, &assign)
          end
        end

        matchpi %{(literal name_)} do
          assign.call(name, Mut::Adj::None, Mut::Mult::One)
        end

        matchpi %{(name_)} do
          assign.call(name, Mut::Adj::None, Mut::Mult::Many)
        end

        otherwise do
          assign.call(expr, Mut::Adj::None, Mut::Mult::One)
        end
      end
    end

    # Precompute Mut data in bulk from an agent's *backspec* for later use. This is
    # no-alloc and cheaper compared to lookup, esp. for things like `(x)`.
    private def muttab(backspec : Term::Dict, &)
      HybridMap(Term, {Term, Mut::Adj, Mut::Mult}).scope do |muts|
        backspec.each_entry do |key, template|
          unless keydict = key.as_d? # Fast path
            # {x: (1 2 3)}  {(x y z): (1 2 3)}
            muts[key] = {template, Mut::Adj::None, Mut::Mult::One}
            next
          end

          # NOTE: In e.g. {(x): (1 2 3), x: qux} we should always prefer `x`, no
          # matter the order (in fact there is none!)

          if keydict.itemsize == 1 # Fast path
            name = keydict[0]
            next if name.in?(backspec)
            # {(x): (1 2 3)}
            muts[name] = {template, Mut::Adj::None, Mut::Mult::Many}
            next
          end

          muti(key) do |name, adj, mult|
            # See the note above.
            next if name.in?(backspec)

            muts[name] = {template, adj, mult}
          end
        end

        yield muts
      end
    end

    # Runs the backmap engine on *agents* and *matchee*, ignoring *disabled* agents
    # (listed by their id).
    #
    # The id of an agent is its index in *agents*. Thus, if you plan on calling
    # this function multiple times, *agents* must stay unchanged.
    #
    # Returns a replacement or a conflict (conflicts store ids of agents involved).
    def backmap(agents : Indexable(Agent), disabled : Pf::USet32, matchee : Term) : Term::Rep | Conflict
      if agents.empty? || agents.size == disabled.size
        return Term.rep(matchee)
      end

      Context.scope do |ctx|
        tree = ctx.slot(initial: matchee, form: nil, depth: 0u32, endpoint: Endpoint.new)

        agents.each_with_index do |agent, index|
          agent_id = index.to_u32
          next if agent_id.in?(disabled)

          muttab(agent.backspec) do |muts|
            agent.matches.each_with_index do |(env, logs), env_id|
              logs.each do |name, log|
                log = Log.normalize(log.seq)
                next if log.is_a?(Log::None)

                ref = Ref.new(name, agent_id, env_id.to_u32)

                unless row = muts[name]?
                  result = mount(ctx, tree, log, 0, MountRef.new(ref))
                  assert result.is_a?(Mounted)
                  next
                end

                template, post, mult = row
                mut = ctx.mut(post, mult, ref, env, template)

                result = mount(ctx, tree, log, 0, MountMut.new(mut))
                assert result.is_a?(Mounted)
              end
            end
          end
        end

        # WARNING: new nodes must not be inserted into the tree beyond this point [in
        # control flow], as the tree contains some important caches that we don't know
        # how to invalidate!

        case result = ctx.µ(&.backprop(tree))
        in Conflict then result
        in Proposed then tree.proposal || Term.rep(matchee)
        end
      end
    end

    # Backtracking search over *disabled* sets to make the backsystem of *agents*
    # conflict-free.
    #
    # Please note that in 99% (if not more!) of the cases there's no search at all;
    # clients are rarely if ever expected to provide conflicting backmaps, since
    # they're kind of obvious and there are many ways to make them conflict-free
    # by hand (e.g. by staging). The search here is really just a gesture of
    # last resort: us really wanting the client to get *some* result even if there
    # are conflicts.
    private def backmap?(agents : Indexable(Agent), disabled : Pf::USet32, matchee : Term) : Term::Rep?
      result = backmap(agents, disabled, matchee)
      if result.is_a?(Term::Rep)
        return result # ok
      end

      # Already sorted by id (thus by index in agents).
      result.agent_ids.each do |agent_id|
        next unless result = backmap?(agents, disabled.add(agent_id), matchee)
        return result # ok
      end

      # Disabled set not found in this branch.
    end

    # **Internal entry point to the backmap engine.**
    #
    # This is the function all clients end up calling to work with the backmap
    # engine (most likely the caller is one of the functions defined under `M1`).
    def backmap(agents : Indexable(Agent), matchee : Term) : Term::Rep
      backmap?(agents, disabled: Pf::USet32[], matchee: matchee) || Term.rep(matchee)
    end
  end
end
