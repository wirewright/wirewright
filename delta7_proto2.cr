require "./src/wirewright"
require "./baz5"
require "./baz5_editor"
require "./suggestion_synthesis"

# TODO: "arbitrary keypath" Stack(Int32) must be called "docpath" (as in "path into a document")
# TODO: nodepath Stack(Int32) is emitted by successor? and is "path into a document that is proven to point to a node"
module Rhodium
  extend self

  # Symbol used to identify the initialize queue of a document.
  Initialize = Term[:"#initialize"]

  # Symbol used to identify the event queue of a document.
  Events = Term[:"#events"]

  # :nodoc:
  Shadow = Term.of(:"#shadow")

  # :nodoc:
  Cells = Term.of(:"#cells")

  # :nodoc:
  JobsPending = Term.of(:"#jobs/pending")

  # :nodoc:
  JobsCompleted = Term.of(:"#jobs/completed")

  # :nodoc:
  Population = Term.of(:"#population")

  # :nodoc:
  Cycle = Term.of(:cycle)

  # :nodoc:
  PostCycle = Term.of(:"#post-cycle")

  Tspaces = Term.of(:"#tspaces")

  # Follows an arbitrary *keypath* into *document*. Returns the pointed-to
  # node or `nil` if the keypath is invalid. This does not take into account
  # the passability of nodes (use `passable?` to check if the keypath is
  # passable beforehand if you need that).
  def follow?(document : Term::Dict, keypath : Stack(Int32)) : Term?
    node = Term.of(document)

    keypath.each do |step|
      return unless node = node.as_d?
      return unless node = node[step]?
    end

    node
  end

  # Same as `follow?`, but raises `KeypathError` if *keypath* is invalid.
  def follow(document : Term::Dict, keypath : Stack(Int32)) : Term
    follow?(document, keypath) || raise KeypathError.new
  end

  # Returns the passable range (children range) for *node*. Returns `nil`
  # if *node* is impassable.
  def passable_range?(node : Term) : Range(Int32, Int32)?
    Term.case(node) do
      matchpi %{[group _*]}, %{[row _*]}, %{[col _*]} { 1...node.itemsize }
      matchpi %{[unit _ _*]} { 2...node.itemsize }
      matchpi %{[edit-cage for @_ _*]} { 3...node.itemsize }
      matchpi %{[decay (%number +i32) _*]} { 2...node.itemsize }
      matchpi %{[lookaround @_ @_ _*]} { 3...node.itemsize }
      matchpi %{[frag _ @_]} { 1...2 }
      matchpi %{[mutator @_ _]} { 2...3 }
      matchpi %{[cover _ _+]} { 2...node.itemsize }
      otherwise { }
    end
  end

  # Returns `true` if *node* is passable, i.e. has children that are also nodes.
  # Returns `false` otherwise.
  def passable_node?(node : Term) : Bool
    !!passable_range?(node)
  end

  # Mutates *keypath* into its successor in *document*, taking into account
  # the (im-)passability of certain nodes.
  #
  # Returns `true` if *keypath* was mutated to the successor.
  # Returns `false` if *keypath* has no successor.
  #
  # The successor of *keypath* is guaranteed to be something called *nodepath*,
  # that is, a keypath to a valid nodes. The terms are used somewhat interchangeably
  # throughout the code, but in theory there is a distinction. A *keypath* is
  # an arbitrary keypath whereas a *nodepath* points to a node exclusively.
  def successor?(document : Term::Dict, keypath : Stack(Int32), *, descend : Bool = true) : Bool
    return false unless focus = follow?(document, keypath)

    # We're at the root.
    if document.same?(focus)
      if document.itemsize.zero?
        return false
      end

      keypath << document.itemsize - 1

      return true
    end

    # Check if we can descend. Descend if that is the case.
    if descend
      passable_range = passable_range?(focus)
      if passable_range && !passable_range.empty?
        unless passable_range.exclusive?
          raise "BUG: expected .possible_range? to return an exclusive range"
        end

        keypath << passable_range.end - 1

        return true
      end
    end

    index = keypath.pop

    while true
      # Find the parent's passable range.
      if keypath.empty?
        parent_passable_range = 0...document.itemsize
      else
        parent = follow(document, keypath)
        parent_passable_range = passable_range?(parent)

        # We somehow ended up here so the parent must have been passable. Otherwise
        # we were provided a bogus keypath so raise.
        unless parent_passable_range && (parent = parent.as_d?)
          raise KeypathError.new
        end
      end

      # If we've visited the beginning of the passable range, pop it.
      if index - 1 < parent_passable_range.begin
        # We're at the beginning of root.
        unless index = keypath.pop?
          return false
        end
        next
      end

      # Otherwise, go back one item.
      keypath << index - 1

      return true
    end
  end

  # Returns *true* is *keypath* is passable within *document*.
  #
  # NOTE: this does not mean that the pointed-to node is a valid node! Check
  # that yourself if you care.
  def passable?(document : Term::Dict, keypath : Stack(Int32)) : Bool
    node = document

    keypath.each do |step|
      if document.same?(node)
        passable_range = 0...document.itemsize
      else
        passable_range = passable_range?(node)
      end

      return false unless passable_range
      return false unless passable_range.includes?(step)
      return false unless node = node.as_d?
      return false unless node = node[step]?
    end

    true
  end

  # Returns `true` if the node pointed to by *nodepath* has an observer parent.
  def observed?(document : Term::Dict, nodepath : Stack(Int32)) : Bool
    !!enclosing?(document, nodepath) { |parent| observer?(Term.of(parent)) }
  end

  # Rewrites the term at an arbitrary *keypath* into *document* using the block.
  # Returns the rewritten copy of *document*, or `nil` if *keypath* is invalid.
  def rewrite?(document : Term::Dict, keypath : Stack(Int32), & : Term -> Rewrite::Any) : Term::Dict?
    stack = Stack(Term::Dict).new
    tip = Term.of(document)

    keypath.each do |step|
      return unless node0 = tip.as_d?
      return unless node1 = node0[step]?

      stack << node0
      tip = node1
    end

    case rewrite = yield tip
    in Rewrite::None
      document
    in Rewrite::One
      tip = rewrite.term

      keypath.reverse_each do |step|
        parent = stack.pop
        tip = parent.with(step, tip)
      end

      expect stack.empty?

      tip.as_d? || raise ArgumentError.new("toplevel rewrite must produce a dict")
    in Rewrite::Many
      unless parent = stack.pop?
        return rewrite.list
      end

      tip = parent.replace(Term[keypath.last], &.concat(rewrite.list.items))

      (0...keypath.size - 1).reverse_each do |index|
        step = keypath[index]
        parent = stack.pop
        tip = parent.with(step, tip)
      end

      expect stack.empty?

      tip.as_d? || raise ArgumentError.new("toplevel rewrite must produce a dict")
    end
  end

  # Same as `rewrite?`, but raises `KeypathError` if *keypath* is invalid.
  def rewrite(document : Term::Dict, keypath : Stack(Int32), & : Term -> Rewrite::Any) : Term::Dict
    rewrite?(document, keypath) { |term| yield term } || raise KeypathError.new
  end

  # Shorthand for `rewrite` that does not depend on the term being rewritten.
  def rewrite(document : Term::Dict, keypath : Stack(Int32), rewrite : Rewrite::Any) : Term::Dict
    rewrite(document, keypath) { rewrite }
  end

  # Shorthand for `rewrite` with `Rewrite::One` of *term*.
  def assign(document : Term::Dict, keypath : Stack(Int32), term : Term) : Term::Dict
    rewrite(document, keypath, Rewrite.one(term))
  end

  # Returns the nearest enclosing parent of the term pointed to by *keypath*
  # for which the block is truthy. Returns `nil` if *keypath* is invalid.
  def enclosing?(document : Term::Dict, keypath : Stack(Int32), & : Term::Dict -> Bool) : Term::Dict?
    stack = Stack(Term::Dict).new
    tip = Term.of(document)

    keypath.each do |step|
      return unless node0 = tip.as_d?
      return unless node1 = node0[step]?

      stack << node0
      tip = node1
    end

    # NOTE: we do not care about the tip here, since it's... *enclosing*.
    # Enclosing the tip, that is.
    stack.reverse_each do |parent|
      next unless yield parent
      return parent
    end
  end

  # Same as `enclosing?`, but raises `KeypathError` if *keypath* is invalid.
  def enclosing(document : Term::Dict, keypath : Stack(Int32), & : Term::Dict -> Bool) : Term::Dict
    enclosing?(document, keypath) { |parent| yield parent } || raise KeypathError.new
  end

  @[Flags]
  enum CursordepthConfig : UInt8
    # Visit the itemspart of dictionaries.
    Items

    # Visit the shadow pairspart of dictionaries.
    PairsShadow

    # Visit the nonshadow pairspart of dictionaries.
    PairsNonshadow

    # When visiting deep itemspart, consider the items there as those of the node
    # on which cursordepth() was called. If a deep item is a cursor, this
    # will result in `1` being returned as if the cursor was a direct item of
    # the original node.
    AdoptInDeepItems

    # When visiting deep pairspart, consider the pairs there as those of the node
    # on which cursordepth() was called. If a deep pair is a cursor, this
    # will result in `1` being returned as if the cursor was a direct pair of
    # the original node.
    AdoptInDeepPairs

    def each(dict : Term::Dict, & : Term ->) : Nil
      if items?
        dict.each_item_unordered { |item| yield item }
      end

      if pairs_shadow? && pairs_nonshadow?
        dict.each_entry { |_, value| yield value }
      elsif pairs_shadow?
        dict.each_entry do |key, value|
          next unless Rhodium.shadow?(key)
          yield value
        end
      elsif pairs_nonshadow?
        dict.each_entry do |key, value|
          next if Rhodium.shadow?(key)
          yield value
        end
      end
    end
  end

  private def cursordepth0(dict : Term::Dict, depth : Int32, config : CursordepthConfig) : Int32
    if M1::Operator.probe?(Term[], CURSORP, Term.of(dict))
      return depth
    end

    unless dict.probably_includes?(Term[:|])
      return Int32::MAX
    end

    mindepth = Int32::MAX

    config.each(dict) do |child|
      next unless child = child.as_d?

      subdepth = cursordepth0(child, depth + 1, config)
      mindepth = subdepth if subdepth < mindepth
    end

    mindepth
  end

  # Returns the depth at which the cursor is found in *term*.
  #
  # - If *term* is the cursor returns `0`.
  # - If the cursor is contained in one of *term*'s entries returns `1`,
  #   if in one of *term*'s entry entries, `2`, and so on.
  # - If there are no cursors in *term* returns `-1`.
  #
  # See also: `CursordepthConfig`.
  #
  # If there are multiple cursors in *node*, returns the *minimum* depth of one
  # in the itemspart; or if there is no cursor there, then the minimum depth of
  # one in the pairspart.
  def cursordepth(term : Term, *, config : CursordepthConfig = {:items}) : Int32
    return -1 unless dict = term.as_d?

    itemsdepth = cursordepth0(dict.itemspart, 0, config)
    if itemsdepth < Int32::MAX
      return config.adopt_in_deep_items? && itemsdepth > 1 ? 1 : itemsdepth
    end

    pairsdepth = cursordepth0(dict.pairspart, 0, config)
    if pairsdepth < Int32::MAX
      return config.adopt_in_deep_pairs? && pairsdepth > 1 ? 1 : pairsdepth
    end

    -1
  end

  # Shorthand for calling `cursordepth` with config preferred by Rhodium nodes.
  def cursordepth_in_node(node : Term) : Int32
    cursordepth(node, config: CursordepthConfig.new({:items, :pairs_nonshadow, :adopt_in_deep_pairs}))
  end

  # Converts *source* indexable of terms to a keypath into *document*. Returns
  # `nil` if the resulting keypath is invalid. Guarantees to return a valid
  # keypath into *document*.
  def keypath?(document : Term::Dict, steps : Indexable(Term)) : Stack(Int32)?
    keypath = Stack(Int32).new(steps.size)

    current = document

    steps.each do |step|
      return unless index = step.to?(Int32)
      return unless current = current[index]?

      keypath.push(index)
    end

    keypath
  end

  # Same as `keypath?`, but raises `KeypathError` if the resulting keypath
  # is invalid.
  def keypath(document : Term::Dict, steps : Indexable(Term)) : Stack(Int32)
    keypath?(document, steps) || raise KeypathError.new
  end

  # A thin queue-like wrapper around an itemsonly carrier dict, used
  # to access the document event queue.
  struct Q
    def initialize(@carrier : Term::Dict)
      unless @carrier.itemsonly?
        raise ArgumentError.new("expected an itemsonly carrier dict for Q")
      end
    end

    # Constructs with the given *carrier* term. If *carrier* is not
    # a dict, initializes to an empty queue.
    def self.of(container : Term::Dict, key : Term::Sym)
      return new(Term[]) unless carrier = container[key]?
      return new(Term[]) unless carrier = carrier.as_itemsonly_d?

      new(carrier)
    end

    # Returns `true` if this queue is empty.
    def empty? : Bool
      @carrier.empty?
    end

    # Returns the first element in this queue. Returns `nil` if this
    # queue is empty.
    def first? : Term?
      @carrier[0]?
    end

    # Constructs an event from *args* and *kwargs* using `Term.of` and
    # inserts it at the back of this queue. Returns the modified copy
    # of this queue.
    def enqueue(*args, **kwargs) : Q
      Q.new(@carrier.append(Term.of(*args, **kwargs)))
    end

    # Constructs an event from *args* and *kwargs* using `Term.of` and
    # inserts it at the front of this queue. Returns the modified copy
    # of this queue.
    def interject(*args, **kwargs) : Q
      Q.new(@carrier.prepend(Term.of(*args, **kwargs)))
    end

    # Returns a copy of this queue without the first element. If there
    # is none, the returned copy is the same as this queue.
    def dequeue : Q
      Q.new(@carrier.lshift)
    end

    # Synchronizes the event queue of *container* with `self`'s contents.
    # Returns the modified copy of *container*.
    #
    # It is assumed that this queue was constructed using `Q.of` with
    # the same *container*. Violating this assumption is not an error,
    # but you should think hard before doing that.
    def commit(container : Term::Dict, key : Term::Sym) : Term::Dict
      @carrier.empty? ? container.without(key) : container.with(key, @carrier)
    end
  end

  # Returns `true` if *key* is a shadow attribute key. Returns `false` otherwise.
  #
  # We consider `#`-prefixed symbol keys *in a node pairspart* to be its *shadow
  # attribute keys*. The user is not supposed to see or create them (for debugging
  # or exploration purposes, they may be allowed to see them; but creating them is
  # not an expected use case).
  def shadow?(key : Term::Sym) : Bool
    key.to(String).prefixed_by?('#')
  end

  # :ditto:
  def shadow?(key : Term) : Bool
    return false unless symbol = key.as_sym?

    shadow?(symbol)
  end

  class EffectBuilder
    getter events = [] of Term
    getter rewrite : Rewrite::Any = Rewrite.none
    getter cells = [] of {Term, Term}
    getter jobs = [] of Term
    getter? disappear = false

    def initialize(@node : Term)
    end

    def disappear : Nil
      @disappear = true
    end

    def cell(k, v)
      cells << {Term.of(k), Term.of(v)}
    end

    def schedule(job)
      jobs << job
    end

    def event(*args, **kwargs)
      events << Term.of(*args, **kwargs)
    end

    def backmap(pattern : Term, backspec : Term)
      @rewrite = M1.backmapr(pattern, backspec, @node)
    end

    def backmap(pattern : String, backspec : String)
      backmap(ML.term(pattern), ML.term(backspec))
    end

    def backmap(pattern : String, **kwargs)
      backmap(ML.term(pattern), Term.of(**kwargs))
    end

    def change(**kwargs)
      unless @rewrite.is_a?(Rewrite::None)
        raise "cannot use multiple different rewrite methods, please use only one"
      end

      @rewrite = Rewrite.one(@node.itemspart | @node.pairspart | Term.of(**kwargs))
    end

    def clear(*keys)
      unless @rewrite.is_a?(Rewrite::None)
        raise "cannot use multiple different rewrite methods, please use only one"
      end

      @rewrite = Rewrite.one(@node.without(*keys))
    end
  end

  # TODO: rename keypath to nodepath in Rhodium

  def effect(document1 : Term::Dict, nodepath : Stack(Int32), node : Term, &) : {Term::Dict, Bool}
    builder = EffectBuilder.new(node)

    transition_vote = with builder yield builder

    document0 = document1

    builder.events.each do |event|
      document1 = Q.of(document1, Events).enqueue(event).commit(document1, Events)
    end

    builder.cells.each do |k, v|
      document1 = document1.morph({Cells, k, v})
    end

    # Disappear allows the node to remove itself as if it didn't exist. This
    # lets us prevent expulsion.
    if builder.disappear?
      each_identity(node) do |identity|
        document1 = document1.morph({Population, identity, false})
      end
    end

    # Replace node with its new version.
    if rewrite = builder.rewrite.as?(Rewrite::Some)
      document1 = rewrite(document1, nodepath, rewrite)
    end

    builder.jobs.each do |job|
      document1 = document1.morph({JobsPending, job, true})
    end

    {document1, transition_vote}
  end

  COMPLETION_MANAGER = begin
    suggestions = File.read(RESOURCES / "suggestions.soma.wwml")
    spec = ML.terms(suggestions).as_d

    NodeCompletion::CompletionManager.new(spec)
  end

  # TODO: this thing is MADNESS! In an ideal world these would be backmaps,
  #   in a less ideal one, something PatternSet-based. All optimization to
  #   pattern-based lookup (like we do here) will be done in PatternSet.
  #   Stupid O(N), even if rejection-fast, is not a good approach. We also
  #   allocate a 3-dict every time, even for rejections. Crazy. Ideally we'd
  #   somehow partition into nested cases with chained env or smth like that.
  #   And I'm not talking about all the parse-backmap calls, that's the least stupid
  #   thing here. Small backmaps should be pretty efficient, a few microseconds perhaps.
  def handle(document0 : Term::Dict, document1 : Term::Dict, nodepath : Stack(Int32), event : Term) : {Term::Dict, Bool}
    node0 = follow(document0, nodepath)

    Term.case({node0, event, cursordepth_in_node(node0)}) do
      # TODO: these are very similar and should be refactored into
      # something single, with variations, like transform.

      givenpi %{[cell v_ @cout_] (initialize _) -1} do
        effect(document1, nodepath, node0) do
          event :"cell/created", cout, v
          cell cout, v

          true
        end
      end

      givenpi %{[cell v_ @cout_ for pattern_] (initialize _) -1} do
        effect(document1, nodepath, node0) do
          if created = M1.probe?(pattern, v)
            event :"cell/created", cout, v
            cell cout, v
          else
            backmap ML.term(%{[cell v_ _ for _]}), Term.of(Term[].with({:v}, Term[]))
          end

          created
        end
      end

      givenpi(
        %{[cell v_ @cout_] (assign @cout_ v_) -1},
        %{[cell v_ @cout_ for _] (assign @cout_ v_) -1},
      ) do
        {document1, false}
      end

      givenpi %{[cell v0_ @cout_] (assign @cout_ v1_) -1} do
        effect(document1, nodepath, node0) do
          event :"cell/updated", cout, v0, v1
          cell cout, v1
          backmap %{[cell v_ @_]}, v: v1

          # The identity of the cell did not change, do not trigger transition.
          false
        end
      end

      givenpi(
        %{[cell @cout_] (assign @cout_ v0_) -1},
        %{[cell @cout_] (cell/created @cout_ v0_) -1},
      ) do
        effect(document1, nodepath, node0) do
          backmap %[[cell ⏏v @_]], v: v0

          true
        end
      end

      givenpi(
        %{[cell @cout_ for _] (assign @cout_ v0_) -1},
        %{[cell @cout_ for _] (cell/created @cout_ v0_) -1},
      ) do
        effect(document1, nodepath, node0) do
          backmap %{[cell ⏏v @_ for _]}, v: v0

          true
        end
      end

      givenpi %{[cell v0_ @cout_ for pattern_] (assign @cout_ v1_) -1} do
        if M1.probe?(pattern, v1)
          effect(document1, nodepath, node0) do
            event :"cell/updated", cout, v0, v1
            cell cout, v1
            backmap %{[cell v_ @_ _*]}, v: v1

            # The identity of the cell did not change, do not trigger transition.
            false
          end
        else
          {document1, false}
        end
      end

      # TODO: instead of (%number (whole _) > 0) we should have (%number 0 < i32). All +-variants must
      # allow the exclusion of zero this way.

      # TODO: this should support for ... guard as well!!

      givenpi %{[cell vs0←(_*) @cout_] (assign/log @cout_ v_ limit←(%number (whole _) > 0)) -1} do
        vs1 = vs0.rightmost(limit.to(Int32) - 1).append(v)

        effect(document1, nodepath, node0) do
          event :"cell/updated", cout, vs0, vs1
          cell cout, vs1
          backmap %{[cell v_ @_]}, v: vs1

          # The identity of the cell did not change, do not trigger transition.
          false
        end
      end

      # frag
      begin
        givenpi %{[frag v_ @cout_] (initialize (frag @_)) _} do
          effect(document1, nodepath, node0) do
            event :"cell/created", cout, v
            cell cout, v

            true
          end
        end

        givenpi %{[frag v_ @cout_] (assign @cout_ v_) _} do
          {document1, false}
        end

        givenpi %{[frag v_ @cout_] (pulse @cout_ clear) _} do
          effect(document1, nodepath, node0) do
            backmap %[[_ v_ @_]], %[{(v): ()}]

            true
          end
        end

        givenpi(
          %{[frag @cout_] (pulse @cout_ destroy) _},
          %{[frag _ @cout_] (pulse @cout_ destroy) _},
        ) do
          # Vote for transition to cleanup etc.
          {rewrite(document1, nodepath, Rewrite.many(Term[])), true}
        end

        # This will trigger the next rule due to fragment's secondary identity
        # changing (being removed) after the assignment.
        givenpi %{[frag v0_ @cout_] (assign @cout_ v1_) _} do
          effect(document1, nodepath, node0) do
            backmap %{[frag v_ @_]}, v: v1

            # We need this to trigger the next rule.
            true
          end
        end

        givenpi %{[frag v1_ @cout_] (frag/removed @cout_ v0_) _} do
          effect(document1, nodepath, node0) do
            event :"cell/updated", cout, v0, v1
            cell cout, v1
            backmap %{[frag v_ @_]}, v: v1

            # Fragment may have been assigned anything, including something that
            # needs a transition right now.
            true
          end
        end

        givenpi %{[frag @cout_] (assign @cout_ v0_) _} do
          effect(document1, nodepath, node0) do
            backmap %{[frag ⏏v @_]}, v: v0

            true
          end
        end
      end

      # changes/view
      begin
        givenpi(
          %{[changes/view @cin_] (assign @cin_ view_) -1},
          %{[changes/view @cin_] (cell/created @cin_ view_) -1},
        ) do
          effect(document1, nodepath, node0) do
            backmap %{[changes/view ⏏view @_]}, view: view

            # changes/view doesn't have an identity like cells or frags do; it
            # only listens to (assign)s.
            false
          end
        end

        givenpi(
          %{[changes/view _ @cin_] (assign @cin_ view_) -1},
          %{[changes/view _ @cin_] (cell/created @cin_ view_) -1},
          %{[changes/view _ @cin_] (cell/changed @cin_ _ view_) -1},
        ) do
          effect(document1, nodepath, node0) do
            backmap %{[changes/view view_ @_]}, view: view

            # ditto
            false
          end
        end

        # changes/view also understands clear and destroy feedback
        givenpi(
          %{[changes/view _ @cin_] (pulse @cin_ clear) -1},
          %{[changes/view _ @cin_] (cell/removed @cin_) -1},
        ) do
          effect(document1, nodepath, node0) do
            backmap %{[changes/view view_ @_]}, %[{(view): ()}]

            # ditto
            false
          end
        end

        givenpi(
          %{[changes/view @cin_] (pulse @cin_ destroy) -1},
          %{[changes/view _ @cin_] (pulse @cin_ destroy) -1},
        ) do
          # ditto on transition
          {rewrite(document1, nodepath, Rewrite.many(Term[])), false}
        end
      end

      givenpi %{(alloy (@pin_ to @pout_) template_ ¦ _ strict⋮ false) (pulse @pin_ vars_) -1} do |vars, template|
        vars = vars.as_d? || Term[]

        if ML.edge?(template)
          unless template = document0[Cells, template]?
            return document1, false
          end
        end

        instance, complaints = Alloy.render_with_complaints(vars, template)

        effect(document1, nodepath, node0) do
          if (complout = node0[:complaints]?) && ML.edge?(complout)
            complaints.each do |complaint|
              event :pulse, complout, complaint
            end
          end

          # TODO: have the node "tell" about errors in the template when strict: false!!!
          if strict.false? || complaints.empty?
            event :pulse, pout, instance
          end

          false
        end
      end

      # Spawner
      begin
        givenpi(
          %{(spawner @pin_ ¦ _ -dir) (pulse @pin_ offspring_) -1},
          %{(spawner @pin_ ¦ _ dir: up) (pulse @pin_ offspring_) -1},
        ) do
          {rewrite(document1, nodepath, Rewrite.many(Term[offspring, node0])), true}
        end

        givenpi %{(spawner @pin_ ¦ _ dir: down) (pulse @pin_ offspring_) -1} do
          {rewrite(document1, nodepath, Rewrite.many(Term[node0, offspring])), true}
        end
      end

      # Sensors and appearances
      begin
        givenpi %{[sensor pattern_ in tspace_symbol to @_] (initialize _) -1} do
          selector = node0[:selector]?

          {document1.morph({Tspaces, tspace, :sensors, {pattern, selector}, true}), true}
        end

        givenpi(
          %{[sensor pattern_ in tspace_ to @pout_] (stimuli tspace_ (pattern_) multiset_) -1},
          %{(sensor pattern_ in tspace_ to @pout_ ¦ _ selector_) (stimuli tspace_ (pattern_ selector_) multiset_) -1},
        ) do
          effect(document1, nodepath, node0) do
            event :pulse, pout, multiset

            false
          end
        end

        givenpi %{[appearance value_ in tspace_symbol] (initialize _) -1} do
          selector = node0[:selector]?

          {document1.morph({Tspaces, tspace, :appearances, {value, selector}, true}), true}
        end
      end

      # Button
      begin
        # Activate
        givenpi(
          %{(button _ to @_ (_*) ¦ _ hover: true) (mouse press) _},
          %{(button _ as _ to @_ (_*) ¦ _ hover: true) (mouse press) _},
          %{(button _ to @_ waiting @_ (_*) ¦ _ hover: true) (mouse press) _},
          %{(button _ as _ to @_ waiting @_ (_*) ¦ _ hover: true) (mouse press) _},
        ) do
          effect(document1, nodepath, node0) do
            backmap %[{¦ -active_}], %[{active: true}]

            false
          end
        end

        # Deactivate & press
        givenpi(
          %{(button _ to @_ (_*) ¦ _ active: true) (mouse release) _},
          %{(button _ as _ to @_ (_*) ¦ _ active: true) (mouse release) _},
          %{(button _ to @_ waiting @_ (_*) ¦ _ active: true) (mouse release) _},
          %{(button _ as _ to @_ waiting @_ (_*) ¦ _ active: true) (mouse release) _},
        ) do
          effect(document1, nodepath, node0) do
            backmap %[(_* (_* ⏏M) ¦ _ active_)], %[{(active): (), M: (press)}]

            false
          end
        end

        # 1 phase button

        givenpi %{[button _ as msg_ to @pout_ ((press) _*)] cycle _} do |msg|
          effect(document1, nodepath, node0) do
            if ML.edge?(msg.not_nil!)
              msg = document0[Cells, msg]?
            end
            if msg
              event :pulse, pout, msg
              backmap %{[button _ as _ to @_ (state_ _*)]}, %[{(state): ()}]
            end
            false
          end
        end

        givenpi %{[button msg_ to @pout_ ((press) _*)] cycle _} do |msg|
          effect(document1, nodepath, node0) do
            if ML.edge?(msg.not_nil!)
              msg = document0[Cells, msg]?
            end
            if msg
              event :pulse, pout, msg
              backmap %{[button _ to _ (action_ _*)]}, %[{(action): ()}]
            end
            false
          end
        end

        # 2 phase button

        # Down
        givenpi %{[button _ as msg_ to @pout_ waiting @_ ((press) _*)] cycle _} do |msg|
          effect(document1, nodepath, node0) do
            if ML.edge?(msg.not_nil!)
              msg = document0[Cells, msg]?
            end
            if msg
              event :pulse, pout, msg
              backmap %{[button _ as _ to @_ waiting _ ((state_) _*)]}, %[{state: pressed}]
            end
            false
          end
        end

        # Up
        givenpi %{[button _ as _ to @_ waiting @acks_ ((pressed) _*)] (pulse @acks_ _) _} do
          effect(document1, nodepath, node0) do
            backmap %{[button _ as _ to @_ waiting _ (state_ _*)]}, %[{(state): ()}]

            false
          end
        end

        # Down
        givenpi %{[button msg_ to @pout_ waiting @_ ((press) _*)] cycle _} do |msg|
          effect(document1, nodepath, node0) do
            if ML.edge?(msg.not_nil!)
              msg = document0[Cells, msg]?
            end
            if msg
              event :pulse, pout, msg
              backmap %{[button _ to _ waiting _ ((state_) _*)]}, %[{state: pressed}]
            end

            false
          end
        end

        # Up
        givenpi %{[button _ to @_ waiting @acks_ ((pressed) _*)] (pulse @acks_ _) _} do
          effect(document1, nodepath, node0) do
            backmap %{[button _ to @_  waiting _ (state_ _*)]}, %[{(state): ()}]

            false
          end
        end
      end

      givenpi %{[combine pins←⟨@pin_⟩ in storage_dict for @pout_] (pulse @pin_ value_) -1} do
        storage1 = storage

        pins.items.each_with_index do |current, index|
          next unless current == pin

          storage1 = storage1.with(index, value)
        end

        effect(document1, nodepath, node0) do
          if storage1.size == pins.size
            event :pulse, pout, storage1
            storage1 = Term[]
          end

          backmap %{[combine _ in storage_ for @_]}, storage: storage1

          false
        end
      end

      givenpi %{[sampler @cin_ on @pin_ to @pout_] (pulse @pin_ _) -1} do
        effect(document1, nodepath, node0) do
          if value = document0[Cells, cin]?
            event :pulse, pout, value
          end

          false
        end
      end

      givenpi %{[sampler cins←⟨@_⟩ on @pin_ to @pout_] (pulse @pin_ _) -1} do
        effect(document1, nodepath, node0) do
          row = Term::Dict.build do |commit|
            cins.items.each do |cin|
              unless value = document0[Cells, cin]?
                return document1, false
              end

              commit << value
            end
          end

          event :pulse, pout, row

          false
        end
      end

      givenpi %{(log @pin_ in @cout_ ¦ _ limit⋮ 10) (pulse @pin_ term_) -1} do
        effect(document1, nodepath, node0) do
          event :"assign/log", cout, term, limit

          false
        end
      end

      givenpi %{(log @pin_ in (entries_*) ¦ _ limit: (%optional 10 limit←(%number (whole _) > 0))) (pulse @pin_ term_) _} do
        effect(document1, nodepath, node0) do
          backmap ML.term(%{[log _ in (entries_*)]}), Term.of(Term[].with({:entries}, entries.rightmost(limit.to(Int32) - 1).append(term)))

          false
        end
      end

      givenpi %{[latest @pin_ @cout_] (pulse @pin_ v_) -1} do
        effect(document1, nodepath, node0) do
          event :assign, cout, v

          false
        end
      end

      givenpi %{[latest (@pin_ pattern_) (@cout_ form_)] (pulse @pin_ v_) -1} do
        if env = M1.match?(pattern, v)
          effect(document1, nodepath, node0) do
            event :assign, cout, M1.bsubst(form, env)

            false
          end
        else
          {document1, false}
        end
      end

      givenpi(
        %{[changes @cin_ to @pout_] (cell/created @cin_ v_) -1},
        %{[changes @cin_ to @pout_] (cell/updated @cin_ _ v_) -1},
        %{[changes @cin_ to @pout_ as v_] (cell/created @cin_ _) -1},
        %{[changes @cin_ to @pout_ as v_] (cell/updated @cin_ _ _) -1},
      ) do
        effect(document1, nodepath, node0) do
          event :pulse, pout, v

          false
        end
      end

      givenpi(
        %{[initial @cin_ to @pout_] (cell/created @cin_ v_) -1},
        %{[initial @cin_ to @pout_ as v_] (cell/created @cin_ _) -1},
      ) do
        effect(document1, nodepath, node0) do
          event :pulse, pout, v

          false
        end
      end

      # `blast`: inorder emission of items from lists received on `pin`.
      givenpi %{[blast @pin_ to @pout_] (pulse @pin_ list_dict) -1} do
        effect(document1, nodepath, node0) do
          list.items.each do |item|
            event :pulse, pout, item
          end

          false
        end
      end

      begin
        givenpi(
          %{[bridge @pin_ to @pout_] (pulse @pin_ value_) -1},
          %{[bridge @pin_ as value_ to @pout_] (pulse @pin_ _) -1}
        ) do
          effect(document1, nodepath, node0) do
            event :pulse, pout, value

            false
          end
        end

        givenpi %{[bridge (@pin_ pattern_) to @pout_] (pulse @pin_ matchee_) -1} do
          unless M1.probe?(pattern, matchee)
            return document1, false
          end

          effect(document1, nodepath, node0) do
            event :pulse, pout, matchee

            false
          end
        end

        givenpi %{[bridge (@pin_ pattern_) to (@pout_ key_)] (pulse @pin_ matchee_) -1} do
          unless envs = M1.matches(pattern, matchee)
            return document1, false
          end

          effect(document1, nodepath, node0) do
            envs.each do |env|
              next unless value = env[key]?

              event :pulse, pout, value
            end

            false
          end
        end
      end

      givenpi %{[echo @pin_] (pulse @pin_ e_) -1} do
        effect(document1, nodepath, node0) do
          event e

          false
        end
      end

      givenpi %{[event e_] cycle -1} do
        effect(document1, nodepath, node0) do
          event e
          backmap %[N_], %[{(N): ()}]

          # Event can be anything, including something that needs a transition.
          # Thus force a transition.
          true
        end
      end

      givenpi %{[queue @pin_ to @_ in (_*) waiting @_] (pulse @pin_ value_) -1} do
        effect(document1, nodepath, node0) do
          backmap %{[_ _ to _ in (_* ⏏back) waiting _]}, back: {:new, value}

          false
        end
      end

      givenpi %{[queue @_ to @pout_ in ((new value_) _*) waiting @_] cycle -1} do
        effect(document1, nodepath, node0) do
          event :pulse, pout, value
          backmap %{[_ _ to _ in ((state_ _) _*) waiting _]}, state: :pending

          false
        end
      end

      givenpi %{[queue @_ to @_ in ((pending _) _*) waiting @acks_] (pulse @acks_ _) -1} do
        effect(document1, nodepath, node0) do
          backmap %{[_ _ to _ in (state_ _*) waiting _]}, %[{(state): ()}]

          false
        end
      end

      # Transform logic
      begin
        # Schedule job.
        givenpi %{(transform _* ¦ _ #shadow: _ #spec: spec←{¦ in: @pin_, body_}) (pulse @pin_ input_) -1} do
          env0 = Term[]

          if (state_edge = spec[:state]?) && ML.edge?(state_edge)
            unless state = document0[Cells, state_edge]?
              return document1, false
            end

            # If state is a symbolic edge e.g. @qux, use qux to refer to its value.
            # Otherwise, use the generic `state`.
            unless state_id = state_edge[1].as_sym?
              state_id = Term[:state]
            end

            env0 = env0.with(state_id, state)
          end

          # If a filter pattern is defined, make sure it matches.
          if filter = spec[:filter]?
            unless env1 = M1.match?(filter, input, env: env0)
              return document1, false
            end
          end

          env1 ||= env0
          env1 = env1.with(:_, input)

          effect(document1, nodepath, node0) do
            change "#job": {program: body, env: env1}

            true
          end
        end

        # Send feedback busy. Schedule job.
        givenpi %{(transform _* ¦ _ #shadow: _ #spec: {¦ in: @pin_} #job: job_) (initialize _) -1} do
          effect(document1, nodepath, node0) do
            schedule job

            false
          end
        end

        # Wait for the job to complete.
        givenpi %{(transform _* ¦ _ #shadow: _ #spec: {¦ in: @pin_, out: @pout_} #job: job_) (job/completed job_ result_) -1} do
          effect(document1, nodepath, node0) do
            event :pulse, pout, result
            clear :"#job"
            disappear

            true
          end
        end
      end

      # Stateful transform
      givenpi %{[transform (@pin_ to @pout_ with state_) body_] (initialize _) -1} do
        effect(document1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, state: state, body: body}

          true
        end
      end

      # Stateless transform
      givenpi %{[transform (@pin_ to @pout_) body_] (initialize _) -1} do
        effect(document1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, body: body}

          true
        end
      end

      # Stateless filter transform
      givenpi %{[transform (@pin_ pattern_ to @pout_) body_] (initialize _) -1} do
        effect(document1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, filter: pattern, body: body}

          true
        end
      end

      # Stateful filter transform
      givenpi %{[transform (@pin_ pattern_ to @pout_ with state_) body_] (initialize _) -1} do
        effect(document1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, filter: pattern, state: state, body: body}

          true
        end
      end

      # Absence node
      begin
        # Initialize `absence` to newborn state.
        givenpi %{[absence @_ as _ to @_] (initialize _) -1} do
          effect(document1, nodepath, node0) do
            change "#shadow": {:"%literal", node0.itemspart}, "#state": :newborn

            true
          end
        end

        # Whenever we're in newborn state, on cycle, look around to see if the cell's
        # identity is in the population.
        givenpi %{(absence @cin_ as msg_ to @pout_ ¦ _ #shadow: _ #state: newborn) cycle -1} do
          effect(document1, nodepath, node0) do
            if document0[Cells, cin]?
              change "#state": :paired
            else
              event :pulse, pout, msg
              change "#state": :unpaired
            end

            false
          end
        end

        givenpi %{(absence @cin_ as msg_ to @pout_ ¦ _ #shadow: _ #state: paired) (cell/removed @cin_) -1} do
          effect(document1, nodepath, node0) do
            event :pulse, pout, msg
            change "#state": :unpaired

            false
          end
        end

        givenpi %{(absence @cin_ as _ to @_ ¦ _ #shadow: _ #state: unpaired) (cell/created @cin_ _) -1} do
          effect(document1, nodepath, node0) do
            change "#state": :paired

            false
          end
        end
      end

      givenpi %{[delay 0 children_*] cycle _} do
        {rewrite(document1, nodepath, Rewrite.many(children.unsafe_as_d)), true}
      end

      givenpi %{[delay n←(%number +i32) _*] cycle _} do
        effect(document1, nodepath, node0) do
          backmap %{[delay n_ _*]}, n: n - 1

          false
        end
      end

      givenpi %{[decay 0 _*] cycle _} do
        {rewrite(document1, nodepath, Rewrite.many(Term[])), true}
      end

      givenpi %{[decay n←(%number +i32) _*] cycle _} do
        effect(document1, nodepath, node0) do
          backmap %{[decay n_ _*]}, n: n - 1

          false
        end
      end

      # `edit-cast`: converts pulse signal to root-centric edit broadcast.
      givenpi %{[edit-cast @pin_ to @bout_] (pulse @pin_ motion_) -1} do
        effect(document1, nodepath, node0) do
          event :edit, bout, motion

          # Transition will be done at edit-time.
          false
        end
      end

      # `edit-cage`: converts pulse signal to children-centric non-broadcast (private) edit.
      givenpi %{[edit-cage for @pin_ children_*] (pulse @pin_ motion_) _} do
        effect(document1, nodepath, node0) do
          backmap ML.term(%{[_ _ _ children_*]}), Term[].with({:children}, edit(children, motion, edge: pin, smart: true)).upcast

          true
        end
      end

      givenpi %{[match (@pin_ to @pout_) (%group conds (%past (_ _) min: 1))] (pulse @pin_ input_) -1} do |conds|
        conds.items.each do |(pattern, template)|
          next unless env = M1.match?(pattern, input)

          output = M1.bsubst(template, env)

          return effect(document1, nodepath, node0) do
            event :pulse, pout, output

            false
          end
        end

        {document1, false}
      end

      # Lookaround can look behind and ahead on demand. It can also contain children.
      # On the children part, there is no isolation; it works just like `group`.
      givenpi %{[lookaround @views_ @capture_ children_*] (pulse @capture_ _) _} do
        expect nodepath.size > 0

        pivot = nodepath.last
        parent = nodepath.pop { follow(document0, nodepath) }.as_d

        if parent.same?(document0)
          range = 0...parent.itemsize
        else
          range = passable_range?(Term.of(parent))

          # We've arrived here somehow. Assume we did that by `successor?` -- which
          # guarantees adherence to passability.
          expect range
        end

        behind = parent.items(Term[range.begin], Term[pivot])
        ahead = parent.items(Term[pivot + 1], Term[range.end])

        effect(document1, nodepath, node0) do
          event :pulse, views, {behind, ahead}

          false
        end
      end

      givenpi %{[mutator @pin_ value0_] (pulse @pin_ (pattern_ backspec_)) _} do
        value1 = M1.backmap?(pattern, backspec, value0) || value0
        node1 = Term.of(node0.morph({2, value1}))

        # Always trigger transition because we don't know what was edited. Maybe
        # it needs a transition and maybe not!
        {assign(document1, nodepath, node1), true}
      end

      # FIXME: this node will lead to cryptic bugs in user land. it should be removed.
      # if users want periodicity they can construct feedback circuits. those are user-
      # content centric rather than related to D7 internals.
      givenpi %{[periodic e_] cycle -1} do
        effect(document1, nodepath, node0) do
          event e

          false
        end
      end

      # FIXME: if node handles cycle it won't hit this. It should be able to!
      # Any node can define mail: @pout_ attribute. If that's the case this rule
      # activates, helping the node consume events from its inbox.
      givenpi %[{¦ inbox: (msg_ _*) mail: @pout_} cycle -1] do
        effect(document1, nodepath, node0) do
          event :pulse, pout, msg
          backmap %[{¦ inbox: (M_ _*)}], %[{(M): ()}]

          false
        end
      end

      # Any node that wants to receive hover sets its hover: false.
      # Any node that wants to receive active sets its active: false.
      # Here we handle hover: true + mouse press = active: false -> active: true,
      # and the reverse. We also enqueue mail about hover and press.
      begin
        # Activate
        givenpi %[{¦ hover: true active: false} (mouse press) -1] do
          effect(document1, nodepath, node0) do
            backmap %[{¦ active_}], %[{active: true}]

            false
          end
        end

        # Deactivate & press
        givenpi %[{¦ active: true inbox_dict} (mouse release) -1] do
          effect(document1, nodepath, node0) do
            backmap %[{¦ active_ inbox: [_* ⏏M]}], %[{active: false, M: (press)}]

            false
          end
        end
      end

      # Suggestions
      #
      # When we see a cursor in a suitable position, we populate it with a list
      # of suggestions. What the cursor/UI does with them is not of our interest.
      givenpi %{_ cycle _} do
        if node1 = COMPLETION_MANAGER.complete?(node0)
          {assign(document1, nodepath, node1), false}
        else
          {document1, false}
        end
      end

      otherwise do
        {document1, false}
      end
    end
  end

  # Returns `true` if *node* is an observer node.
  def observer?(node : Term) : Bool
    Term.case(node) do
      matchpi %{(frag _ @_)} { true }
      otherwise { false }
    end
  end

  # Yields identities of *node*, if any.
  #
  # Nodes with identity are interested in receiving initialize events.
  def each_identity(node : Term, & : Term ->) : Nil
    Term.case({node, cursordepth_in_node(node)}) do
      givenpi(
        %{(cell _ @cout_) -1},
        %{(cell _ @cout_ for _) -1},
      ) do
        yield Term.of(:cell, cout)
      end

      # Fragments include the value in their identity so that we have observability.
      givenpi %{(frag value_ @cout_) _} do
        yield Term.of(:frag, cout)
        yield Term.of(:frag, value, cout)
      end

      givenpi %{(transform _* ¦ #shadow: _ #spec: {¦ in: @pin_} #job: job_) -1} do
        yield Term.of(:transform, pin, job)
      end

      givenpi(
        %{(transform (@_ to @_) _) -1},
        %{(transform (@_ to @_ with _) _) -1},
        %{(transform (@_ _ to @_) _) -1},
        %{(transform (@_ _ to @_ with _) _) -1},
        %{(absence @_ as _ to @_) -1},
      ) { yield node }

      givenpi %{(sensor pattern_ in tspace_symbol to @_ ¦ (%keypool selector)) -1} do
        yield Term.of(:sensor, tspace, pattern, node[:selector]?)
      end

      givenpi %{(appearance value_ in tspace_symbol ¦ (%keypool selector)) -1} do
        yield Term.of(:appearance, tspace, value, node[:selector]?)
      end

      otherwise { }
    end
  end

  # Handles the expellation of a node *identity* from *document0*.
  #
  # - Node identity is present in *document0*.
  # - Node identity is absent in *document1*.
  #
  # The goal of this method is to accommodate this transition into *document1*.
  #
  # NOTE: *document1* is passed for modification (writing) only. Do not read it. This will
  # violate semantics, introducing order-dependence of reads (since *document1* is possibly
  # only partially modified whenever you get hold of it). *document0* is read-only and
  # *document1* is write-only.
  def expel(document0 : Term::Dict, document1 : Term::Dict, identity : Term) : Term::Dict
    Term.case(identity) do
      matchpi %{(cell @cout_)}, %{(frag @cout_)} do
        document1 = document1.morph({Cells, cout, nil})

        Q.of(document1, Events).enqueue(:"cell/removed", cout).commit(document1, Events)
      end

      # If there is a successor fragment to @cout, it will catch this event
      # and perform a transition.
      matchpi %{(frag value_ @cout_)} do
        Q.of(document1, Events).enqueue(:"frag/removed", cout, value).commit(document1, Events)
      end

      matchpi %{(transform @pin_ job_)} do
        document1.morph({JobsPending, job, nil})
      end

      matchpi %{(sensor tspace_ pattern_)} do
        document1.morph({Tspaces, tspace, :sensors, {pattern}, nil})
      end

      matchpi %{(sensor tspace_ pattern_ selector_)} do
        document1.morph({Tspaces, tspace, :sensors, {pattern, selector}, nil})
      end

      matchpi %{(appearance tspace_ value_)} do
        document1.morph({Tspaces, tspace, :appearances, {value}, nil})
      end

      matchpi %{(appearance tspace_ value_ selector_)} do
        document1.morph({Tspaces, tspace, :appearances, {value, selector}, nil})
      end

      otherwise { document1 }
    end
  end

  # Steps forward in time the document *document0*, assuming the occurrence of *event*.
  # Returns the resulting document *document1* (the *successor* of *document0*).
  #
  # NOTE: *document1* is passed for modification (writing) only. Do not read it. This will
  # violate semantics, introducing order-dependence of reads (since *document1* is possibly
  # only partially modified whenever you get hold of it). *document0* is read-only and
  # *document1* is write-only.
  def step(document0 : Term::Dict, document1 : Term::Dict, event : Term) : {Term::Dict, Bool}
    nodepath = Stack(Int32).new

    # All nodes must unanimously vote `false` for us to vote `false` on transition.
    transition_vote = false

    while successor?(document0, nodepath)
      document1, node_transition_vote = handle(document0, document1, nodepath, event)

      # - If the node voted yes (do transition) we vote yes (do transition).
      # - If the node is observed (meaning it is e.g. inside of a fragment) we must
      #   vote yes even if the node says otherwise; since e.g. fragment or any other
      #   kind of observer will rely on transitions with an overwhelmingly
      #   high probability.
      if node_transition_vote || observed?(document0, nodepath)
        transition_vote = true
      end
    end

    {document1, transition_vote}
  end

  # Steps forward in time the document *document0*. Returns the resulting
  # document *document1* (the *successor* of *document0*).
  def step(document0 : Term::Dict) : {Term::Dict, Bool}
    # First we have to exhaust all initialize events.
    initialize_queue = Q.of(document0, Initialize)

    while head = initialize_queue.first?
      initialize_queue = initialize_queue.dequeue

      Term.case(head) do
        matchpi %{((steps_number+) identity_)} do
          next unless keypath = keypath?(document0, steps.items)

          document1 = initialize_queue.commit(document0, Initialize)

          return handle(document0, document1, keypath, Term.of(:initialize, identity))
        end

        otherwise { }
      end
    end

    # There may have been some bogus initialize events; commit the queue that
    # is empty of them. If there were no initialize events, this will be
    # a noop.
    document0 = initialize_queue.commit(document0, Initialize)

    queue = Q.of(document0, Events)

    unless event = queue.first?
      document1 = queue.enqueue(Cycle).commit(document0, Events)

      return document1, false
    end

    document1 = queue.dequeue.commit(document0, Events)

    # Any event other than cycle => remove #post-cycle. It's post-that
    # event now.
    document1 = document1.morph({PostCycle, nil})

    Term.case(event) do
      matchpi %{(edit @edge_ motion_)} do
        edited = edit(Term.of(document1), motion, edge, smart: true)

        # Edit is potentially destructive and not under our control; therefore it
        # will always force a transition.
        {edited.as_d? || raise("toplevel edit must produce a dict"), true}
      end

      matchpi %{cycle} do
        document1 = document1.morph({PostCycle, true})

        continue
      end

      otherwise do
        step(document0, document1, event)
      end
    end
  end

  # Returns `true` if *document* is marked as post-cycle. Returns `false` otherwise.
  def post_cycle?(document : Term::Dict) : Bool
    document.includes?(PostCycle)
  end

  # Returns `true` if *document* is in a "settled" state: it needs external
  # events to be "unsettled"; and will otherwise remain stable forever.
  def settled?(document : Term::Dict) : Bool
    return false unless Q.of(document, Events).empty?
    return false unless Q.of(document, Initialize).empty?
    return false unless post_cycle?(document)

    true
  end

  # Performs the transition from *document0* to its successor *document1*.
  #
  # "Temporal" aspects have their origin here, such as "cell/created".
  def transition(document0 : Term::Dict, document1 : Term::Dict) : Term::Dict
    # Common nodepath stack we will reuse.
    nodepath = Stack(Int32).new

    # "Fix" nodes whose #shadow is not the same as itemspart -- by removing
    # all #-pairs in their pairspart.
    while successor?(document1, nodepath)
      # Assume there is little repetition.
      document1 = rewrite(document1, nodepath) do |node|
        next Rewrite.none unless node0 = node.as_d?
        next Rewrite.none unless shadow = node0[Shadow]?
        next Rewrite.none if M1.probe?(shadow, Term.of(node0.itemspart))

        node1 = node0.transaction do |commit|
          node0.each_pair do |key, _|
            next unless shadow?(key)

            commit.without(key)
          end
        end

        Rewrite.one(node1)
      end
    end

    population0 = document0[Population]? || Term[]
    population1 = Term[]

    document2 = document1

    while successor?(document1, nodepath)
      node = follow(document1, nodepath)

      # Ask each node for its identity. If it has one, we do this population thing.
      # It it does not, we ignore the node and move on.
      each_identity(node) do |identity|
        if identity.in?(population0)
          # Prolong
          population0 = population0.without(identity)
          population1 = population1.with(identity, true)
        elsif !identity.in?(population1)
          # Initialize
          document2 = Q.of(document2, Initialize).enqueue({nodepath, identity}).commit(document2, Initialize)
          population1 = population1.with(identity, true)
        end
      end
    end

    population0.each_entry do |identity, _|
      # Handle controlled expulsion, when the node wants itself to disappear
      # without anyone knowing. The node removes itself and marks its identity
      # as `false` in population1. We see that its identity is indeed absent
      # from document1 at this point (if there are duplicates we'll never reach
      # this check since the duplicates will override with their existence);
      # and also see the `false` mark, meaning the node's disappearance
      # was controlled.
      next if document1[Population, identity]? == Term[false]

      # Expel. Note that this cannot be an event, because in fact, there's no node
      # to send it to; only to that node's identity, its "ghost"; the document's
      # "memory" of the node.
      document2 = expel(document0, document2, identity)
    end

    document2.morph({Population, population1})
  end

  # Returns the transition function for `Rhodium`.
  def transition : D7::Transition
    D7::Transition.new do |document0, document1, log|
      log.append { Term.of(:input, :rhodium, :transition, document0, document1) }
      document1 = transition(document0, document1)
      log.append { Term.of(:output, :rhodium, :transition, document1) }
      document1
    end
  end

  # Returns the step function for `Rhodium`.
  def step : D7::Step
    D7::Step.new do |document0, log|
      log.append { Term.of(:input, :rhodium, :step, document0) }
      document1, transition_vote = step(document0)
      log.append { Term.of(:output, :rhodium, :step, document1) }
      {document1, transition_vote}
    end
  end
end

# Nitrene serves ephemeral rewrite jobs asynchronously for Rhodium.
#
# `transform` nodes publish their jobs at the root document and Nitrene
# picks them up. When the job is complete, it publishes an appropriate
# event -- which the `transform` that started the job (or a transform with
# the same pending job) picks up.
module Nitrene
  extend self

  class StepContext
    @mt : Fiber::ExecutionContext

    # WARNING: *alarm* will be called from another thread. Make sure whatever
    # you do there is thread-safe.
    def initialize(&@alarm : ->)
      @mt = Fiber::ExecutionContext::MultiThreaded.new("Nitrene", 4)
      @active = Atomic(UInt32).new(0u32)
      @running = Atomic(Term::Dict).new(Term[])
      @completed = Atomic(Term::Dict).new(Term[])
    end

    class JobInterrupted < Exception
    end

    private def spawn(job : Term, program : Term, env : Term::Dict) : Nil
      @mt.spawn do
        @active.add(1, :release)
        # sleep 3.seconds

        tick = -> do
          running = @running.get(:acquire)
          unless job.in?(running)
            raise JobInterrupted.new
          end
        end

        result = Nitrene.run(program, env, observer: tick)

        completed0 = @completed.get(:acquire)
        while true
          completed1 = completed0.with(job, result)
          completed0, ok = @completed.compare_and_set(completed0, completed1, :release, :acquire)
          break if ok
        end

        @active.sub(1, :release)
        @alarm.call
      rescue JobInterrupted
      end
    end

    # Takes off and returns all completed jobs.
    def completed : Term::Dict
      @completed.swap(Term[], :release)
    end

    # Synchronizes running jobs with *jobs*: spawns new ones and cancels ones
    # not in *jobs*.
    def sync(jobs running1 : Term::Dict) : Nil
      running0 = @running.swap(running1, :release)
      running1.each_entry do |job, _|
        next if job.in?(running0)

        Term.case(job) do
          matchpi %{(¦ () program_ env_dict)} { spawn(job, program, env.unsafe_as_d) }
          otherwise { }
        end
      end
    end

    # WARNING: the caller guarantees that `sync` will never be called and is not
    # being called right now while this method runs.
    def jobless? : Bool
      running = @running.get(:acquire)
      completed = @completed.get(:acquire)
      active = @active.get(:acquire)

      running.empty? && completed.empty? && active.zero?
    end
  end

  # TODO: this isn't it!!!!
  JOB_REWRITER = chainR(using(plug(:env), dfsR(envR)), callR(PRIMITIVES))

  # Runs a Nitrene *program* within the given *env*.
  #
  # See `rewrite` to learn about *observer*.
  def run(program : Term, env : Term::Dict, *, observer = nil) : Term
    if observer
      rewrite(program, JOB_REWRITER, observer, env: env)
    else
      rewrite(program, JOB_REWRITER, env: env)
    end
  end

  # :nodoc:
  #
  # Asynchronous step implementation.
  def step(nictx : StepContext, document document0 : Term::Dict) : Term::Dict
    document1 = document0

    jobs_pending = document0[Rhodium::JobsPending]?.try(&.as_d?) || Term[]
    jobs_pending = jobs_pending.transaction do |commit|
      jobs_completed = nictx.completed
      jobs_completed.each_entry do |job, result|
        # Skip completed jobs that are not in the pending list.
        next unless job.in?(jobs_pending)

        commit.without(job)

        document1 = Rhodium::Q.of(document1, Rhodium::Events)
          .enqueue(:"job/completed", job, result)
          .commit(document1, Rhodium::Events)
      end
    end

    # Swap running jobs with pending jobs.
    nictx.sync(jobs_pending)

    document1.with(Rhodium::JobsPending, jobs_pending)
  end

  # :nodoc:
  #
  # Synchronous step implementation.
  def step(document document0 : Term::Dict) : Term::Dict
    document1 = document0

    jobs_pending = document0[Rhodium::JobsPending]?.try(&.as_d?) || Term[]
    jobs_pending.each_entry do |job, _|
      Term.matchpi?(job, %{(¦ () program_ env_dict)}) do
        result = run(program, env.unsafe_as_d)

        document1 = Rhodium::Q.of(document1, Rhodium::Events)
          .enqueue(:"job/completed", job, result)
          .commit(document1, Rhodium::Events)
      end
    end

    document1.with(Rhodium::JobsPending, nil)
  end

  # Constructs an **asynchronous** step function for `Nitrene`.
  #
  # *nictx* is the job context. Nitrene will serve jobs asynchronously; the
  # context will keep info about the currently running jobs etc. between
  # steps (along with `ExecutionContext` and so on).
  #
  # NOTE: you will have to restart the run loop if it terminates before some
  # jobs complete. See also: `StepContext.new`, `Goal.jobless`.
  def step(nictx : StepContext) : D7::Step
    D7::Step.new do |document0, log|
      log.append { Term.of(:input, :nitrene, :step, document0) }
      document1 = step(nictx, document0)
      log.append { Term.of(:output, :nitrene, :step, document1) }

      # Nitrene will never trigger a transition since it does not modify
      # the document; it only emits events and takes pending jobs off.
      {document1, false}
    end
  end

  # Constructs a **synchronous** step function for `Nitrene`.
  #
  # All Nitrene jobs are guaranteed to complete at the end of each step.
  #
  # Can be used for localizing bugs to asynchronous `step`. Otherwise prefer
  # asynchronous `step(nictx : StepContext)` since that's what Nitrene is about.
  def step : D7::Step
    D7::Step.new do |document0, log|
      log.append { Term.of(:input, :nitrene, :step, document0) }
      document1 = step(document0)
      log.append { Term.of(:output, :nitrene, :step, document1) }

      # Nitrene will never trigger a transition since it does not modify
      # the document; it only emit events and takes pending jobs off.
      {document1, false}
    end
  end
end

# Short for *short-term memory*. Implements a rolling set: a fixed-
# size circular array with an O(N) set semantics.
class STM(T, N)
  def initialize
    @ring = StaticArray(T?, N).new { }
    @cursor = 0
  end

  def add?(element : T) : Bool
    if @cursor >= @ring.size
      @cursor = 0
    end
    return false if @ring.any?(element)
    @ring[@cursor] = element
    @cursor += 1
    true
  end
end

module D7
  extend self

  alias Step = Term::Dict, Log -> {Term::Dict, Bool}
  alias Transition = Term::Dict, Term::Dict, Log -> Term::Dict

  # Optional logging of document rewrites.
  #
  # The following rewrite event protocol is adhered to:
  #
  # - `(original document_)`: emitted by `run` to show the document which it
  #   was called with.
  # - `(input subsystem_symbol step document_)`: to show what document
  #   a subsystem's step function received.
  # - `(input subsystem_symbol transition document0_ document1_)`: to show
  #   what documents a subsystem's transition function received.
  # - `(output subsystem_symbol step document_)`: to show what document
  #   a subsystem's step function produced.
  # - `(output subsystem_symbol transition document_)`: to show what document
  #   a subsystem's transition function produced.
  #
  # `subsystem` could be `rhodium` (for `Rhodium`), `nitrene` (for `Nitrene`), etc.
  abstract struct Log
  end

  # Observe by a proc.
  struct Log::Fn < Log
    def initialize(@observer : Term ->)
    end

    def append(& : -> Term) : Nil
      @observer.call(yield)
    end
  end

  # Do not observe.
  struct Log::None < Log
    def append(& : -> Term) : Nil
    end
  end

  # Chains steps *a* and *b*.
  def steps(a : Step, b : Step) : Step
    Step.new do |document, log|
      document, transition_vote0 = a.call(document, log)
      document, transition_vote1 = b.call(document, log)

      {document, transition_vote0 || transition_vote1}
    end
  end

  # Chains steps *a*, *b* and so on.
  def steps(a : Step, b : Step, *cs : Step) : Step
    steps(steps(a, b), *cs)
  end

  # Chains transitions *a* and *b*.
  def transitions(a : Transition, b : Transition) : Transition
    Transition.new do |document0, document1, log|
      document2 = a.call(document0, document1, log)
      document3 = b.call(document1, document2, log)
      document3
    end
  end

  # Chains transitions *a*, *b*, and so on.
  def transitions(a : Transition, b : Transition, *cs : Transition) : Transition
    transitions(transitions(a, b), *cs)
  end

  # D7 mainloop. It's like a game loop except it's not.
  #
  # - *log* lets you observe document rewrites -- if you want to, at the expense
  #   of performance (observing is slower than not). See `Log` for the protocol.
  # - *transition* lets you observe and contribute to transitions between successive
  #   documents, say, D0 (current document) and D1 (document at the next time step).
  # - *step* lets you advance the current document D0 by one step to obtain D1.
  # - *goal* defines the goal of rewriting: it is usually `equal`, `match`, or `none`.
  # - *initial* enables or disables the initial transition from an empty document
  #   to *document1*. This triggers such things as cell creation events, for example.
  def run(document document1 : Term::Dict, log : Log, transition : Transition, step : Step, goal : Goal::Fn, *, initial : Bool) : Term::Dict
    if initial
      log.append { Term.of(:original, document1) }

      document0 = Term[]
    else
      document0 = document1
    end

    transition_vote = !document0.same?(document1)

    while true
      if transition_vote
        document1 = transition.call(document0, document1, log)
      end
      document2, transition_vote = step.call(document1, log)
      if goal.call(document2)
        return document2
      end

      document0 = document1
      document1 = document2
    end
  end

  # The simplest way to run a document. May block forever if *document* has
  # e.g. a feedback loop or something that is cyclic. Uses synchronous Nitrene.
  # Does not handle surfaces.
  def run(document : Term::Dict) : Term::Dict
    run(document,
      log: Log::None.new,
      transition: Rhodium.transition,
      step: steps(Rhodium.step, Nitrene.step),
      goal: Goal.none,
      initial: true,
    )
  end

  # Spec for an *equality* termination condition for document rewriting.
  #
  # The rewriting process continues until the document becomes equal to *target*.
  #
  # - *target* specifies the expected final state of the document.
  # - If *nonshadow* is set to `true`, ignores shadow attributes on nodes
  #   and the document itself when checking for equality.
  # - *limit* sets an optional limit on the number of rewriting steps before
  #   forced termination.
  #
  # See also: `run?`, `Goal.equal`, `Goal.nonshadow`, `Goal.limited`.
  record Equal, target : Term::Dict, nonshadow : Bool = true, limit : Int32? = nil do
    # :nodoc:
    def goal : Goal::Fn
      goal = Goal.equal(target)
      goal = Goal.nonshadow(goal) if nonshadow
      if limit_ = limit
        goal = Goal.limited(goal, limit: limit_)
      end

      goal
    end
  end

  # Spec for a *pattern match* termination condition for document rewriting.
  #
  # The rewriting process continues until the document matches *pattern*.
  #
  # - *pattern* is the pattern that must be matched for rewriting to stop.
  # - If *nonshadow* is set to `true`, ignores shadow attributes on nodes
  #   and the document itself when checking for equality.
  # - *limit* sets an optional limit on the number of rewriting steps before
  #   forced termination.
  #
  # See also: `run?`, `Goal.matches`, `Goal.nonshadow`, `Goal.limited`.
  record Matches, pattern : Term, nonshadow : Bool = true, limit : Int32? = nil do
    # :nodoc:
    def goal : Goal::Fn
      goal = Goal.matches(pattern)
      goal = Goal.nonshadow(goal) if nonshadow
      if limit_ = limit
        goal = Goal.limited(goal, limit: limit_)
      end

      goal
    end
  end

  # Executes a rewriting process for *document* which is terminated according
  # to the given termination condition *cond*. Returns `{true, final document}`
  # if rewriting completed successfully (according to *cond*); returns
  # `{false, latest document}` otherwise.
  #
  # See `run` for info on other arguments.
  def run?(document : Term::Dict, cond : Equal | Matches, *, log : Log = Log::None.new) : {Bool, Term::Dict}
    # We don't need an alarm because we're using Goal.jobless.
    nictx = Nitrene::StepContext.new { }

    goal = Goal.jobless(cond.goal, nictx)
    transition = Rhodium.transition
    step = steps(Rhodium.step, Nitrene.step)

    begin
      {true, run(document, log, transition, step, goal, initial: true)}
    rescue e : Goal::Interrupted
      {false, e.latest}
    end
  end

  # Strips shadow pairs off of *node*.
  def nonshadow1(node : Term) : Term
    return node unless dict = node.as_d?

    Term.of(nonshadow1(dict))
  end

  # :ditto:
  def nonshadow1(node : Term::Dict) : Term::Dict
    node.transaction do |commit|
      # NOTE: Assume shadow? is never true for keys from the itemspart.
      node.each_pair do |key, value|
        next unless Rhodium.shadow?(key)
        commit.without(key)
      end
    end
  end

  # Strips shadow pairs off of *node* and its itemspart children and
  # so on recursively.
  def nonshadow(node : Term) : Term
    return node unless dict = node.as_d?

    Term.of(nonshadow(dict))
  end

  # :ditto:
  def nonshadow(node : Term::Dict) : Term::Dict
    node.transaction do |commit|
      node.each_item_with_index do |item, index|
        commit.with(index, nonshadow(item))
      end

      # NOTE: Assume shadow? is never true for keys from the itemspart.
      node.each_pair do |key, value|
        next unless Rhodium.shadow?(key)
        commit.without(key)
      end
    end
  end
end

# Provides a set of functions that define stopping conditions for
# document rewriting.
module D7::Goal
  extend self

  alias Fn = Term::Dict -> Bool

  # Restricts *goal* to the nonshadow part of the document.
  #
  # Allows you to hide shadow attributes on nodes and the document itself to *goal*.
  #
  # See also: `D7.nonshadow`.
  def nonshadow(goal : Fn) : Fn
    Fn.new { |document| goal.call(D7.nonshadow(document)) }
  end

  # Constructs a `Goal::Fn` that waits until the document being rewritten
  # is equal to *expected*.
  def equal(expected : Term::Dict) : Fn
    Fn.new { |document| document == expected }
  end

  # Constructs a `Goal::Fn` that waits until the document being rewritten
  # matches *pattern*.
  def matches(pattern : Term) : Fn
    Fn.new { |document| M1.probe?(pattern, Term.of(document)) }
  end

  # Restricts *goal* to documents that have no running jobs in Nitrene
  # context *nictx*.
  def jobless(goal : Fn, nictx : Nitrene::StepContext) : Fn
    Fn.new { |document| nictx.jobless? ? goal.call(document) : false }
  end

  # Constructs a `Goal::Fn` that communicates to the rewriting engine
  # that there is no goal, and the document may be rewritten forever.
  #
  # Short term memory is provided by default, allowing detection and early
  # termination for short cycles instead of spinning. How short its term is
  # is defined by the *lookback* argument. By default, the goal function sees
  # 8 documents back.
  #
  # To opt out of short memory (thus spinning when no change), set *lookback*
  # to `nil`.
  #
  # NOTE: *lookback* will be closed over and mutated. Try not to pass anything
  # you would want control over.
  #
  # NOTE: the only requirement is that *lookback* responds to `add?`. You are
  # recommended to use `STM` for efficiency if N is small.
  def none(*, lookback = STM(Term::Dict, 8).new) : Fn
    if lookback
      Fn.new { |document| Rhodium.settled?(document) && !lookback.add?(document) }
    else
      Fn.new { false }
    end
  end

  # Raised by `limited` to indicate that the goal limit was exceeded.
  class Interrupted < Exception
    # Returns the last document for which the goal function was called
    # and failed.
    getter latest

    def initialize(@latest : Term::Dict)
      @message = "goal limit exceeded"
    end
  end

  # Wraps around another *goal* function; raises `Interrupted` if the goal
  # cannot be reached within *limit* steps.
  def limited(goal : Fn, *, limit budget : Int32 = 128) : Fn
    Fn.new do |document|
      fulfilled = goal.call(document)

      unless fulfilled
        budget -= 1
        if budget.zero?
          raise Interrupted.new(document)
        end
      end

      fulfilled
    end
  end
end

last_doc = nil

observe = ->(entry : Term) do
  Term.case(entry) do
    matchpi %{(original doc_)}, %{(output _ _ doc_)} do
      next if doc == last_doc

      puts ML.display(doc, maxwidth: 80)
      sleep 1.second

      last_doc = doc
    end

    otherwise { }
  end
end

# doc0 = ML.terms <<-WWML
# (cell @count for _number)
#    (changes @count to @log)
#    (absence @count as "Missing" to @log)
#    (log @log in ())
#    (event (assign @count 100))
# WWML

# nctx = Nitrene::StepContext.new
# initial = true

# while true
#   doc0 = D7.run(doc0.as_d,
#     log: D7::Log::Fn.new(observe),
#     transition: Rhodium.transition,
#     step: D7.steps(Rhodium.step, Nitrene.step(nctx)),
#     goal: D7::Goal.none,
#     initial: initial,
#   )
#   initial = false

#   break unless nctx.wait?
# end

# puts D7.run_until_equal?(doc0.as_d, doc1.as_d, log: D7::Log::Fn.new(observe), limit: 128)

# D7.run(doc0.as_d, D7::Log::Fn.new(observe), Rhodium.transition, D7.steps(Rhodium.step, Nitrene.step), D7::Goal.limited(D7::Goal.find(doc1.as_d, hidden: false), limit: 128))

# doc1 = doc1.as_d

# puts "Initial"
# puts ML.display(doc1)
# gets

# doc1 = Rhodium.transition(doc0, doc1)

# puts "After transition"
# puts ML.display(doc1)
# gets

# doc0 = doc1

# while true
#   puts "Step"
#   puts ML.display(doc0)
#   gets

#   doc1 = Rhodium.step(doc0)

#   puts "After step"
#   puts ML.display(doc1)
#   gets

#   doc1 = D7.step(doc1)

#   puts "Transition"
#   puts ML.display(doc0)
#   puts " -> "
#   puts ML.display(doc1)
#   gets

#   doc0 = Rhodium.transition(doc0, doc1)

#   puts "After transition"
#   puts ML.display(doc0)
#   gets
# end

# pp Rhodium.passable?(doc, Stack{0, 1})

# n = 0
# Benchmark.ips do |x|
#   x.report("time") do
#  keypath = Stack(Int32).new
# while Rhodium.successor?(doc, keypath)
#   # pp keypath
# end
# n += keypath.nil? ? 1 : 0
#   end
# end
