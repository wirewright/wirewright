require "./wirewright"
require "./baz5"
require "./baz5_editor"
require "./suggestion_synthesis"
require "execution_context"

# TODO: "arbitrary keypath" Stack(Int32) must be called "docpath" (as in "path into a document")
# TODO: nodepath Stack(Int32) is emitted by successor? and is "path into a document that is proven to point to a node"
module Rhodium
  extend self

  Initialize = Term[:"#initialize"]
  Events      = Term[:"#events"]

  Shadow      = Term.of(:"#shadow")
  Cells       = Term.of(:"#cells")
  JobsPending = Term.of(:"#jobs/pending")
  JobsCompleted = Term.of(:"#jobs/completed")
  Population  = Term.of(:"#population")

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
      matchpi %[(group _*)], %{[row _*]}, %{[col _*]} { 1...node.itemsize }
      matchpi %[(edit-cage for @_ _*)] { 3...node.itemsize }
      matchpi %[(decay (%number +i32) _*)] { 2...node.itemsize }
      matchpi %[(lookaround @_ @_ @_ _*)] { 4...node.itemsize }
      matchpi %[(fragment _ @_)] { 1...2 }
      otherwise {}
    end
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
  def successor?(document : Term::Dict, keypath : Stack(Int32)) : Bool
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
    passable_range = passable_range?(focus)
    if passable_range && !passable_range.empty?
      unless passable_range.exclusive?
        raise "BUG: expected .possible_range? to return an exclusive range"
      end

      keypath << passable_range.end - 1

      return true
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

    (0...keypath.size).each do |index|
      step = keypath[index]

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

  private def cursordepth0(node : Term, depth : Int32) : Int32
    if M1::Operator.probe?(Term[], CURSORP, node)
      return depth
    end

    return Int32::MAX unless dict = node.as_d?
    return Int32::MAX if dict.empty?
    return Int32::MAX unless dict.probably_includes?(Term[:|])

    dict.ee.min_of do |k, v|
      Math.min(cursordepth0(k, depth + 1), cursordepth0(v, depth + 1))
    end
  end

  # Returns the depth at which the cursor is found in *node*.
  #
  # - If *node* is the cursor returns `0`.
  # - If the cursor is contained in one of *node*'s entries returns `1`,
  #   if in one of *node*'s entry entries, `2`, and so on.
  # - If there are no cursors in *node* returns `-1`.
  #
  # If there are multiple cursors in *node* returns the depth of the closest
  # cursor (i.e. minimum depth).
  def cursordepth(node : Term) : Int32
    depth = cursordepth0(node, 0)
    depth == Int32::MAX ? -1 : depth
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

  # Returns `true` if *key* is an internal key. Returns `false` otherwise.
  #
  # We consider `#`-prefixed symbol keys *in a node pairspart* to be *internal keys*.
  # The user is not supposed to see or create them (for debugging or exploration
  # purposes, they may be allowed to see them; but creating them is not an expected
  # use case).
  def internal_key?(key : Term::Sym) : Bool
    key.to(String).prefixed_by?('#')
  end

  # :ditto:
  def internal_key?(key : Term) : Bool
    return false unless symbol = key.as_sym?

    internal_key?(symbol)
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
    if builder.disappear? && (identity = identity?(node))
      document1 = document1.morph({Population, identity, false})
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
    suggestions = File.read("#{__DIR__}/suggestions.soma.wwml")
    spec = ML.terms(suggestions).as_d

    NodeCompletion::CompletionManager.new(spec)
  end

  # TODO: rename root0 -> document0
  # TODO: rename root1 -> document1
  # TODO: this thing is MADNESS! In an ideal world these would be backmaps,
  #   in a less ideal one, something PatternSet-based. All optimization to
  #   pattern-based lookup (like we do here) will be done in PatternSet.
  #   Stupid O(N), even if rejection-fast, is not a good approach. We also
  #   allocate a 3-dict every time, even for rejections. Crazy. Ideally we'd
  #   somehow partition into nested cases with chained env or smth like that.
  #   And I'm not talking about all the parse-backmap calls, that's the least stupid
  #   thing here. Small backmaps should be pretty efficient, a few microseconds perhaps.
  def handle(root0 : Term::Dict, root1 : Term::Dict, nodepath : Stack(Int32), event : Term) : {Term::Dict, Bool}
    node0 = follow(root0, nodepath)

    Term.case({node0, event, cursordepth(node0)}) do
      # TODO: these are very similar and should be refactored into
      # something single, with variations.

      givenpi %[(cell v_ @cout_) initialize -1] do
        effect(root1, nodepath, node0) do
          event :"cell/created", cout, v
          cell cout, v

          true
        end
      end

      givenpi %[(cell v_ @cout_ for pattern_) initialize -1] do
        effect(root1, nodepath, node0) do
          if created = M1.probe?(pattern, v)
            event :"cell/created", cout, v
            cell cout, v
          else
            backmap ML.term(%[(cell v_ _ for _)]), Term.of(Term[].with({:v}, Term[]))
          end

          created
        end
      end

      # ??!!?1 What's that _* doing? It doesn't matter since there is no change anyway
      # but still, super sloppy!!
      givenpi %[(cell v_ @cout_ _*) (assign @cout_ v_) -1] do
        {root1, false}
      end

      givenpi %[(cell v0_ @cout_) (assign @cout_ v1_) -1] do
        effect(root1, nodepath, node0) do
          event :"cell/updated", cout, v0, v1
          cell cout, v1
          backmap %[(cell v_ @_)], v: v1

          # The identity of the cell did not change, do not trigger transition.
          false
        end
      end

      givenpi %[(cell @cout_) (assign @cout_ v0_) -1] do
        effect(root1, nodepath, node0) do
          backmap %[(cell ⏏v @_)], v: v0

          true
        end
      end

      givenpi %[(cell @cout_ for _) (assign @cout_ v0_) -1] do
        effect(root1, nodepath, node0) do
          backmap %[(cell ⏏v @_ for _)], v: v0

          true
        end
      end

      givenpi %[(cell v0_ @cout_ for pattern_) (assign @cout_ v1_) -1] do
        if M1.probe?(pattern, v1)
          effect(root1, nodepath, node0) do
            event :"cell/updated", cout, v0, v1
            cell cout, v1
            backmap %[(cell v_ @_ _*)], v: v1

            # The identity of the cell did not change, do not trigger transition.
            false
          end
        else
          {root1, false}
        end
      end

      # TODO: instead of (%number (whole _) > 0) we should have (%number i32 > 0). All +-variants must
      # allow the exclusion of zero this way.

      # TODO: this should support for ... guard as well!!

      givenpi %[(cell vs0←(_*) @cout_) (assign/log @cout_ v_ limit←(%number (whole _) > 0)) -1] do
        vs1 = vs0.rightmost(limit.to(Int32) - 1).append(v)

        effect(root1, nodepath, node0) do
          event :"cell/updated", cout, vs0, vs1
          cell cout, vs1
          backmap %[(cell v_ @_)], v: vs1

          # The identity of the cell did not change, do not trigger transition.
          false
        end
      end

      # Fragment
      #
      # Fragments behave like cells except they're passable. And don't have
      # a guard variant.
      begin
        givenpi %[(fragment v_ @cout_) initialize _] do
          effect(root1, nodepath, node0) do
            event :"cell/created", cout, v
            cell cout, v

            true
          end
        end

        givenpi %[(fragment v_ @cout_) (assign @cout_ v_) _] do
          {root1, false}
        end

        givenpi %[(fragment v0_ @cout_) (assign @cout_ v1_) _] do
          effect(root1, nodepath, node0) do
            event :"cell/updated", cout, v0, v1
            cell cout, v1
            backmap %[(fragment v_ @_)], v: v1

            # The identity of the fragment did not change, do not trigger transition.
            false
          end
        end

        givenpi %[(fragment @cout_) (assign @cout_ v0_) _] do
          effect(root1, nodepath, node0) do
            backmap %[(fragment ⏏v @_)], v: v0

            true
          end
        end
      end

      # Button
      begin
        givenpi %[(button _ as @cin_ to @pout_ ((press) _*)) cycle _] do
          effect(root1, nodepath, node0) do
            if msg = root0[Cells, cin]?
              event :pulse, pout, msg
            end
            backmap %[(button _ as _ to @_ (action_ _*))], %[{(action): ()}]

            false
          end
        end

        givenpi %[(button _ as msg_ to @pout_ ((press) _*)) cycle _] do
          effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            backmap %[(button _ as _ to @_ (action_ _*))], %[{(action): ()}]

            false
          end
        end

        givenpi %[(button @cin_ to @pout_ ((press) _*)) cycle _] do
          effect(root1, nodepath, node0) do
            if msg = root0[Cells, cin]?
              event :pulse, pout, msg
            end
            backmap %[(button _ to _ (action_ _*))], %[{(action): ()}]

            false
          end
        end

        givenpi %[(button msg_ to @pout_ ((press) _*)) cycle _] do
          effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            backmap %[(button _ to _ (action_ _*))], %[{(action): ()}]

            false
          end
        end

        # The different kinds of buttons we have all support feedback. The patterns
        # are a bit loose but I guess it's fine. It's too much of a button to
        # not work.

        givenpi %[(button caption_ to @pout_ (_*)) (feedback busy @pout_) _] do
          effect(root1, nodepath, node0) do
            backmap %[{_ #shadow: (%- _ shadow) #waiting: (%- _ waiting)}],
              shadow: Term.of(:button, {:"%literal", caption}, :to, {:"%literal", pout}, {:"_*"}),
              waiting: 1

            false
          end
        end

        givenpi %[(button caption_ as msg_ to @pout_ (_*)) (feedback busy @pout_) _] do
          effect(root1, nodepath, node0) do
            backmap %[{_ #shadow: (%- _ shadow) #waiting: (%- _ waiting)}],
              shadow: Term.of(:button, {:"%literal", caption}, :as, {:"%literal", msg}, :to, {:"%literal", pout}, {:"_*"}),
              waiting: 1

            false
          end
        end

        # ?!?!?!?! _* must not be there!!
        givenpi %[(button _* to @pout_ (_*) ¦ #shadow: _ #waiting: waiting←(%number (whole _) > 0)) (feedback busy @pout_) _] do
          effect(root1, nodepath, node0) do
            change "#waiting": waiting + 1

            false
          end
        end

        # ?!?!?!?! _* must not be there!!
        givenpi %[(button _* to @pout_ (_*) ¦ #shadow: _ #waiting: 1) (feedback (%any done cancelled) @pout_) _] do
          effect(root1, nodepath, node0) do
            clear :"#waiting", :"#shadow"

            false
          end
        end

        # ?!?!?!?! _* must not be there!!
        givenpi %[(button _* to @pout_ (_*) ¦ #shadow: _ #waiting: waiting←(%number (whole _) > 0)) (feedback (%any done cancelled) @pout_) _] do
          effect(root1, nodepath, node0) do
            change "#waiting": waiting - 1

            false
          end
        end
      end

      givenpi %[(log @pin_ in @cout_ ¦ limit⋮ 10) (pulse @pin_ term_) -1] do
        effect(root1, nodepath, node0) do
          event :"assign/log", cout, term, limit

          false
        end
      end

      givenpi %[(log @pin_ in (entries_*) ¦ limit: (%optional 10 limit←(%number (whole _) > 0))) (pulse @pin_ term_) (%not 1 2)] do
        effect(root1, nodepath, node0) do
          backmap ML.term(%[(log _ in (entries_*) ¦ _)]), Term.of(Term[].with({:entries}, entries.rightmost(limit.to(Int32) - 1).append(term)))

          false
        end
      end

      givenpi %[(latest @pin_ @cout_) (pulse @pin_ v_) -1] do
        effect(root1, nodepath, node0) do
          event :assign, cout, v

          false
        end
      end

      givenpi %[(latest (@pin_ pattern_) (@cout_ form_)) (pulse @pin_ v_) -1] do
        if env = M1.match?(pattern, v)
          effect(root1, nodepath, node0) do
            event :assign, cout, M1.bsubst(form, env)

            false
          end
        else
          {root1, false}
        end
      end

      givenpi(
        %[(changes @cin_ to @pout_) (cell/created @cin_ v_) -1],
        %[(changes @cin_ to @pout_) (cell/updated @cin_ _ v_) -1],
        %[(changes @cin_ to @pout_ as v_) (cell/created @cin_ _) -1],
        %[(changes @cin_ to @pout_ as v_) (cell/updated @cin_ _ _) -1],
      ) do
        effect(root1, nodepath, node0) do
          event :pulse, pout, v

          false
        end
      end

      # `blast`: inorder emission of items from lists received on `pin`.
      givenpi %[(blast @pin_ to @pout_) (pulse @pin_ list_dict) -1] do
        effect(root1, nodepath, node0) do
          list.items.each do |item|
            event :pulse, pout, item
          end

          false
        end
      end

      givenpi %[(echo @pin_) (pulse @pin_ e_) -1] do
        effect(root1, nodepath, node0) do
          event e

          false
        end
      end

      givenpi %[(event e_) cycle -1] do
        effect(root1, nodepath, node0) do
          event e
          backmap %[N_], %[{(N): ()}]

          true # FIXME: Not sure I understand. Why does event need a transition?
        end
      end

      givenpi %[(queue @pin_ to @_ in (_*)) (pulse @pin_ value_) -1] do
        effect(root1, nodepath, node0) do
          backmap %[(_ _ to _ in (_* ⏏head))], head: value

          false
        end
      end

      givenpi %[(queue @pin_ to @pout_ in (head_ _*)) (pull @pout_) -1] do
        effect(root1, nodepath, node0) do
          event :pulse, pout, head

          false
        end
      end

      # Dequeue
      givenpi %[(queue @pin_ to @pout_ in (head_ _*)) (feedback completed @pout_ head_) -1] do
        effect(root1, nodepath, node0) do
          backmap %[(_ _ to _ in (head_ _*))], %[{(head): ()}]

          false
        end
      end

      givenpi %[(pull @pout_ from @pin_) (pull @pout_) -1] do
        effect(root1, nodepath, node0) do
          event :pull, pin

          false
        end
      end

      givenpi %[(pull @pout_ from @pin_) (pulse @pin_ value_) -1] do
        effect(root1, nodepath, node0) do
          change pending: value

          false
        end
      end

      givenpi(
        %[(pull @pout_ from @pin_ pending: value_) (pull @pout_) -1],
      ) do
        effect(root1, nodepath, node0) do
          event :pulse, pout, value
          change state: :busy

          false
        end
      end

      givenpi %[(pull @pout_ from @pin_ pending: value_ state: busy) (feedback (%any done cancelled) @pout_) -1] do
        effect(root1, nodepath, node0) do
          event :feedback, :completed, pin, value
          clear :state, :pending

          false
        end
      end

      # Transform logic
      begin
        # For stateful transforms, signal it's ready to take a job if the state
        # is a hard-coded const or a cell with a known value.
        #
        # For stateless transforms, signal that we're ready unconditionally.
        givenpi %[(transform _* ¦ #shadow: _ #spec: spec←{_ in: @pin_}) cycle -1] do
          ready = true

          if (state = spec[:state]?) && ML.edge?(state)
            ready = !!root0[Cells, state]?
          end

          if ready
            effect(root1, nodepath, node0) do
              event :pull, pin

              false
            end
          else
            {root1, false}
          end
        end

        # Schedule job.
        givenpi %[(transform _* ¦ #shadow: _ #spec: spec←{_ in: @pin_, body_}) (pulse @pin_ input_) -1] do
          env0 = Term[]

          if (state_edge = spec[:state]?) && ML.edge?(state_edge)
            unless state = root0[Cells, state_edge]?
              return root1, false
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
              return root1, false
            end
          end

          env1 ||= env0
          env1 = env1.with(:_, input)

          effect(root1, nodepath, node0) do
            change "#job": {program: body, env: env1}

            true
          end
        end

        # Send feedback busy. Schedule job.
        givenpi %[(transform _* ¦ #shadow: _ #spec: {_ in: @pin_} #job: job_) initialize -1] do
          effect(root1, nodepath, node0) do
            event :feedback, :busy, pin
            schedule job

            false
          end
        end

        # Wait for the job to complete.
        givenpi %[(transform _* ¦ #shadow: _ #spec: {_ in: @pin_, out: @pout_} #job: job_) (job/completed job_ result_) -1] do
          effect(root1, nodepath, node0) do
            event :feedback, :done, pin
            event :pulse, pout, result
            clear :"#job"
            disappear

            true
          end
        end
      end

      # Initialize stateful transform
      givenpi %[(transform @pin_ to @pout_ with state_ body_) initialize -1] do
        effect(root1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, state: state, body: body}

          true
        end
      end

      # Stateless transform
      givenpi %[(transform @pin_ to @pout_ body_) initialize -1] do
        effect(root1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, body: body}

          true
        end
      end

      # Stateless filter transform
      givenpi %[(transform (@pin_ pattern_) to @pout_ body_) initialize -1] do
        effect(root1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, filter: pattern, body: body}

          true
        end
      end

      # Stateful filter transform
      givenpi %[(transform (@pin_ pattern_) to @pout_ with state_ body_) initialize -1] do
        effect(root1, nodepath, node0) do
          change "#shadow": {:"%literal", node0.itemspart}, "#spec": {in: pin, out: pout, filter: pattern, state: state, body: body}

          true
        end
      end

      # Absence node
      begin
        # Initialize `absence` to newborn state.
        givenpi %{(absence @_ as _ to @_) initialize -1} do
          effect(root1, nodepath, node0) do
            change "#shadow": {:"%literal", node0.itemspart}, "#state": :newborn

            true
          end
        end

        # Whenever we're in newborn state, on cycle, look around to see if the cell's
        # identity is in the population.
        givenpi %{(absence @cin_ as msg_ to @pout_ ¦ #shadow: _ #state: newborn) cycle -1} do
          effect(root1, nodepath, node0) do

            if root0[Cells, cin]?
              change "#state": :paired
            else
              event :pulse, pout, msg
              change "#state": :unpaired
            end

            false
          end
        end

        givenpi %{(absence @cin_ as msg_ to @pout_ ¦ #shadow: _ #state: paired) (cell/removed @cin_) -1} do
          effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            change "#state": :unpaired

            false
          end
        end

        givenpi %{(absence @cin_ as _ to @_ ¦ #shadow: _ #state: unpaired) (cell/created @cin_ _) -1} do
          effect(root1, nodepath, node0) do
            change "#state": :paired

            false
          end
        end
      end

      givenpi %[(delay 0 children_*) cycle _] do
        {rewrite(root1, nodepath, Rewrite.many(children.unsafe_as_d)), true}
      end

      givenpi %[(delay n←(%number +i32) _*) cycle _] do
        effect(root1, nodepath, node0) do
          backmap %[(delay n_ _*)], n: n - 1

          false
        end
      end

      givenpi %[(decay 0 _*) cycle _] do
        {rewrite(root1, nodepath, Rewrite.many(Term[])), true}
      end

      givenpi %[(decay n←(%number +i32) _*) cycle _] do
        effect(root1, nodepath, node0) do
          backmap %[(decay n_ _*)], n: n - 1

          false
        end
      end

      # `edit-cast`: converts pulse signal to root-centric edit broadcast.
      givenpi %[(edit-cast @pin_ to @bout_) (pulse @pin_ motion_) -1] do
        effect(root1, nodepath, node0) do
          event :edit, bout, motion

          # Transition will be done at edit-time.
          false
        end
      end

      # `edit-cage`: converts pulse signal to children-centric non-broadcast (private) edit.
      givenpi %[(edit-cage for @pin_ children_*) (pulse @pin_ motion_) _] do
        effect(root1, nodepath, node0) do
          backmap ML.term(%[(_ _ _ children_*)]), Term[].with({:children}, edit(children, motion, edge: pin, smart: true)).upcast

          true
        end
      end

      givenpi %[(map @pin_ to @pout_ (%group conds (%past (_ _) min: 1))) (pulse @pin_ input_) -1] do |conds|
        conds.items.each do |(pattern, template)|
          next unless env = M1.match?(pattern, input)

          output = M1.bsubst(template, env)

          return effect(root1, nodepath, node0) do
            event :pulse, pout, output

            false
          end
        end

        {root1, false}
      end

      # Lookaround can look behind and ahead on demand. It can also contain children.
      # On the children part, there is no isolation; it works just like `group`.
      givenpi %[(lookaround @behind-out_ @ahead-out_ @capture_ children_*) (pulse @capture_ _) _] do
        expect nodepath.size > 0

        pivot = nodepath.last
        parent = nodepath.pop { follow(root0, nodepath) }.as_d

        if parent.same?(root0)
          range = 0...parent.itemsize
        else
          range = passable_range?(Term.of(parent))

          # We've arrived here somehow. Assume we did that by `successor?` -- which
          # guarantees adherence to passability.
          expect range
        end

        behind = parent.items(Term[range.begin], Term[pivot])
        ahead = parent.items(Term[pivot + 1], Term[range.end])

        effect(root1, nodepath, node0) do
          event :pulse, behind_out, behind
          event :pulse, ahead_out, ahead

          false
        end
      end

      givenpi %[(periodic e_) cycle -1] do
        effect(root1, nodepath, node0) do
          event e

          false
        end
      end

      # Suggestions
      #
      # When we see a cursor in a suitable position, we populate it with a list
      # of suggestions. What the cursor/UI does with them is not of our interest.
      givenpi %{_ cycle _} do
        if node1 = COMPLETION_MANAGER.complete?(node0)
          {assign(root1, nodepath, node1), false}
        else
          {root1, false}
        end
      end

      otherwise do
        {root1, false}
      end
    end
  end

  # Returns the identity of *node*. Returns `nil` if *node* has no identity.
  def identity?(node : Term) : Term?
    Term.case({node, cursordepth(node)}) do
      # ?!?!!?!  FIXME: What is this _* doing here?!
      givenpi %{(cell _ @cout_ _*) -1}, %{(fragment _ @cout_) _} do
        Term.of(:cell, cout)
      end

      givenpi %{(transform _* ¦ #shadow: _ #spec: {_ in: @pin_} #job: job_) -1} do
        Term.of(:transform, pin, job)
      end

      # Nodes interested in receiving initialize events.
      givenpi(
        %{(transform @_ to @_ with _ _) -1},
        %{(transform @_ to @_ _) -1},
        %{(transform (@_ _) to @_ _) -1},
        %{(transform (@_ _) to @_ with _ _) -1},
        %{(absence @_ as _ to @_) -1},
      ) { node }

      otherwise {}
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
      matchpi %{(cell @cout_)}, %{(fragment @cout_)} do
        document1 = document1.morph({Cells, cout, nil})

        Q.of(document1, Events).enqueue(:"cell/removed", cout).commit(document1, Events)
      end

      matchpi %{(transform @pin_ job_)} do
        document1 = document1.morph({JobsPending, job, nil})

        Q.of(document1, Events).enqueue(:feedback, :cancelled, pin).commit(document1, Events)
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

      # If any node voted yes we vote yes.
      if node_transition_vote
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
        matchpi %{(steps_number+)} do
          next unless keypath = keypath?(document0, steps.items)

          document1 = initialize_queue.commit(document0, Initialize)

          return handle(document0, document1, keypath, Term.of(:initialize))
        end

        otherwise { }
      end
    end

    # There may have been some bogus initialize events; commit the queue that
    # is empty of them. If there were no initialize events, this will be
    # a noop.
    document0 = initialize_queue.commit(document0, Initialize)

    queue = Q.of(document0, Events)
    if event = queue.first?
      document1 = queue.dequeue.commit(document0, Events)
    else
      event = Term.of(:cycle)
      document1 = document0
    end

    Term.case(event) do
      matchpi %{(edit @edge_ motion_)} do
        edited = edit(Term.of(document1), motion, edge, smart: true)

        # Edit is potentially destructive and not under our control; therefore it
        # will always force a transition.
        {edited.as_d? || raise("toplevel edit must produce a dict"), true}
      end

      otherwise do
        step(document0, document1, event)
      end
    end
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
            next unless internal_key?(key)

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
      next unless identity = identity?(node)

      if identity.in?(population0)
        # Prolong
        population0 = population0.without(identity)
        population1 = population1.with(identity, true)
      elsif !identity.in?(population1)
        # Initialize
        document2 = Q.of(document2, Initialize).enqueue(nodepath).commit(document2, Initialize)
        population1 = population1.with(identity, true)
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

  JOB_REWRITER = chainR(using(plug(:env), dfsR(envR)), callR(PRIMITIVES))

  class JobContext
    getter alarm : Channel(Bool)

    def initialize
      @mt = ExecutionContext::MultiThreaded.new("Nitrene", 4)
      @running = Atomic(Term::Dict).new(Term[])
      @completed = Atomic(Term::Dict).new(Term[])
      @alarm = Channel(Bool).new
    end

    class JobInterrupted < Exception
    end

    private def spawn(job : Term, program : Term, env : Term::Dict) : Nil
      @mt.spawn do
        # sleep 3.seconds

        result = rewrite(program, JOB_REWRITER, env: env) do
          running = @running.get(:acquire)
          unless job.in?(running)
            raise JobInterrupted.new
          end
        end

        completed0 = @completed.get(:acquire)
        while true
          completed1 = completed0.with(job, result)
          completed0, ok = @completed.compare_and_set(completed0, completed1, :release, :acquire)
          break if ok
        end

        select
        when @alarm.send(true)
        else
        end
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
          otherwise {}
        end
      end
    end

    def wait? : Bool
      running = @running.get(:acquire)
      if running.empty?
        return false
      end

      @alarm.receive

      true
    end
  end

  def step(ctx : JobContext, document document0 : Term::Dict) : Term::Dict
    document1 = document0

    jobs_pending = document0[Rhodium::JobsPending]?.try(&.as_d?) || Term[]
    jobs_pending = jobs_pending.transaction do |commit|
      jobs_completed = ctx.completed
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
    ctx.sync(jobs_pending)

    document1.with(Rhodium::JobsPending, jobs_pending)
  end

  # Returns the step function for `Nitrene`.
  #
  # - *ctx* provides the job context. Nitrene is serving the jobs asynchronously
  #   and this context keeps info about the currently running jobs etc, along with
  #   `ExecutionContext` and so on.
  def step(ctx = JobContext.new) : D7::Step
    D7::Step.new do |document0, log|
      log.append { Term.of(:input, :nitrene, :step, document0) }
      document1 = step(ctx, document0)
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
    @cursor &+= 1
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
  def steps(a : Step, b : Step)
    Step.new do |document, log|
      document, transition_vote0 = a.call(document, log)
      document, transition_vote1 = b.call(document, log)

      {document, transition_vote0 || transition_vote1}
    end
  end

  # Chains steps *a* and *b* and so on.
  def steps(a : Step, b : Step, *cs)
    steps(steps(a, b), *cs)
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

  def run(document : Term::Dict, *, log : Log = Log::None.new, goal : Goal::Fn = Goal.none, initial : Bool = true) : Term::Dict
    run(document, log, Rhodium.transition, steps(Rhodium.step, Nitrene.step), goal, initial: initial)
  end

  def run?(document : Term::Dict, goal : Goal::Fn, *, log : Log = Log::None.new, limit = nil) : {Bool, Term::Dict}
    if limit
      goal = Goal.limited(goal, limit: limit)
    end

    {true, run(document, log: log, goal: goal)}
  rescue e : Goal::Interrupted
    {false, e.latest}
  end

  def run_until_equal?(document : Term::Dict, target : Term::Dict, *, only_visible : Bool = true, **kwargs) : {Bool, Term::Dict}
    run?(document, Goal.visible(Goal.equal(target), enabled: only_visible), **kwargs)
  end

  def run_until_matches?(document : Term::Dict, pattern : Term, *, only_visible : Bool = true, **kwargs) : {Bool, Term::Dict}
    run?(document, Goal.visible(Goal.matches(pattern), enabled: only_visible), **kwargs)
  end

  # Strips internal pairs off of *document* and the nodes in it.
  #
  # See also: `Rhodium#internal_key?`.
  def visible(document document0 : Term::Dict, *, except = Tuple.new) : Term::Dict
    nodepath = Stack(Int32).new

    # Strip document.
    document1 = document0.transaction do |commit|
      document0.each_pair do |key, _|
        next if key.in?(except)
        next unless Rhodium.internal_key?(key)

        commit.without(key)
      end
    end

    # Strip nodes.
    while Rhodium.successor?(document0, nodepath)
      document1 = Rhodium.rewrite(document1, nodepath) do |node0|
        next Rewrite.none unless dict0 = node0.as_d?
        next Rewrite.none unless dict0.pairsize > 0 # Saves an allocation in the common case

        dict1 = dict0.transaction do |commit|
          dict0.each_pair do |key, _|
            next if key.in?(except)
            next unless Rhodium.internal_key?(key)

            commit.without(key)
          end
        end

        Rewrite.one(dict1)
      end
    end

    document1
  end
end

module D7::Goal
  extend self

  alias Fn = Term::Dict -> Bool

  # Restricts *goal* to the visible part of the document if *enabled*.
  #
  # Allows you to show (`true`) or hide (`false`) internal pairs on nodes and
  # the document itself to *goal*. Hiding them is an opt in but recommended
  # since the pairs are internal for a reason; and may burden the user visually
  # if seen.
  #
  # See also: `D7.visible`.
  def visible(goal : Fn, *, enabled : Bool) : Fn
    Fn.new { |document| goal.call(enabled ? D7.visible(document) : document) }
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
  # recommended ot use `STM` for efficienty if N is small; but you also may
  # use `Set` for infinite lookback (leaking memory but able to detect very
  # large cycles); since the latter also responds to `add?`.
  def none(*, lookback = STM(Term::Dict, 8).new) : Fn
    if lookback
      Fn.new { |document| !lookback.add?(document) }
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
  def limited(goal : Fn, *, limit budget = 128) : Fn
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

    otherwise {}
  end
end

# doc0 = ML.terms <<-WWML
# (cell @count for _number)
#    (changes @count to @log)
#    (absence @count as "Missing" to @log)
#    (log @log in ())
#    (event (assign @count 100))
# WWML

# nctx = Nitrene::JobContext.new
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
