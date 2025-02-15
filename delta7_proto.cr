# this thing is a prototype. yes it is f*cking slow. yes it is stupid
# yes it is underdesigned. so be it. i don't know how to design it well.
# yes i had 20 tries. yes most of them succeeded. yes all of them were shitty.
# experience leads to better design. brick by brick one builds a house
#
# regardless, this code is a big big pile of poo !
#
# hopefully it turns into something nicer one day.

require "./wirewright"
# require "execution_context"
require "./baz5"

module D
  extend self

  Events        = Term[:"#events"]
  Cells         = Term[:"#cells"]
  Population    = Term[:"#population"]
  JobsPending   = Term[:"#jobs/pending"]
  JobsCompleted = Term[:"#jobs/completed"]

  # TODO: Delete in favor of more granularity
  DocumentMarker = Term[:"#document"]

  struct Q
    def initialize(@data : Term::Dict)
      unless @data.itemsonly?
        raise ArgumentError.new("expected an itemsonly data dict for Q")
      end
    end

    # TODO: remove in favor of (root, path) : Q (aka more granularity,
    # queue does not necessarily belong to a document)
    def self.of(document : Term::Dict)
      new(document[Events]?.try(&.as_itemsonly_d?) || Term[])
    end

    def empty?
      @data.empty?
    end

    def first? : Term?
      @data[0]?
    end

    def enqueue(*args, **kwargs) : Q
      Q.new(@data.append(Term.of(*args, **kwargs)))
    end

    def interject(*args, **kwargs) : Q
      Q.new(@data.prepend(Term.of(*args, **kwargs)))
    end

    def dequeue : Q
      Q.new(@data.lshift)
    end

    def commit(document : Term::Dict) : Term::Dict
      if @data.empty?
        document.without(Events)
      else
        document.with(Events, @data)
      end
    end
  end

  def passable_range?(node : Term) : Range(Int32, Int32)?
    Term.case(node) do
      matchpi %[(group _*)], %{[row _*]}, %{[col _*]} { 1...node.itemsize }
      matchpi %[(edit-cage for @_ _*)] { 3...node.itemsize }
      matchpi %[(decay (%number +i32) _*)] { 2...node.itemsize }
      matchpi %[(lookaround @_ @_ @_ _*)] { 4...node.itemsize }

      otherwise {}
    end
  end

  # :nodoc:
  def follow?(root : Term, rangepath : Term::Dict::ItemsView)
    return root unless range = rangepath.first?
    return unless root_dict = root.as_d?

    Term.case(range) do
      matchpi %[((%number +i32) e←(%number (whole _) > 0))] do
        if item = root_dict[e - 1]?
          follow?(item, rangepath.move(1))
        end
      end

      otherwise {}
    end
  end

  # Follows a *rangepath* into *root*.
  #
  # A *rangepath* is an itemspart path (itemspath) where each step is an exclusive
  # range of the form `(<begin> <end exclusive>)`, i.e. `(1 10)`. The last element
  # of the range is interpreted as the current key (thus for `(1 10)` the key would
  # be `9`).
  def follow?(root : Term, rangepath : Term::Dict)
    follow?(root, rangepath.items)
  end

  def follow(root, rangepath)
    follow?(root, rangepath) || raise KeypathError.new
  end

  # :nodoc:
  #
  # Rewrites layer 1.
  def rewrite1(dict : Term::Dict, index : Term::Num, rewrite : Rewrite::One) : Term::Dict
    dict.with(index, rewrite.term)
  end

  # :nodoc:
  #
  # Rewrites layer 1.
  def rewrite1(dict : Term::Dict, index : Term::Num, rewrite : Rewrite::Many) : Term::Dict
    dict.replace(index, &.concat(rewrite.list.items))
  end

  def rewrite?(root : Term, rangepath : Term::Dict::ItemsView, rewrite : Rewrite::Some)
    if rangepath.empty?
      return rewrite.term?
    end

    return unless dict = root.as_d?

    Term.case(rangepath.first) do
      matchpi %[((%number +i32) e←(%number (whole _) > 0))] do
        return unless item = dict[e - 1]?

        if rangepath.size == 1
          # Even though we do not use `item` defined above, we'd still need to check
          # whether e - 1 exists; it works both ways.
          return Term.of(rewrite1(dict, e - 1, rewrite))
        end

        Term.of(dict.with(e - 1, rewrite?(item, rangepath.move(1), rewrite)))
      end

      otherwise {}
    end
  end

  def rewrite?(root : Term, rangepath : Term::Dict, rewrite : Rewrite::Some)
    rewrite?(root, rangepath.items, rewrite)
  end

  def rewrite(root, rangepath, rewrite)
    rewrite?(root, rangepath, rewrite) || raise KeypathError.new
  end

  def assign?(root : Term, rangepath : Term::Dict, value) : Term?
    rewrite?(root, rangepath, Rewrite.one(value))
  end

  def assign(root, rangepath, value) : Term
    assign?(root, rangepath, value) || raise KeypathError.new
  end

  # Returns rangepath successor to *rangepath* in *root*.
  def successor?(root : Term, rangepath : Term::Dict) : Term::Dict?
    return unless focus = follow?(root, rangepath)

    # We're doing a DFS here so first check if there is an opportunity to descend.
    if (root_dict = root.as_d?) && root_dict.itemsize > 0
      range = root.same?(focus) ? (0...root_dict.itemsize) : passable_range?(focus)

      if range
        unless range.exclusive?
          raise ArgumentError.new("expected an exclusive range from #passable_range?")
        end

        return rangepath.append({range.begin, range.end})
      end
    end

    # If we cannot descend then check if we're at the root, if we are, then there's
    # nothing we can do.
    until rangepath.items.empty?
      hi = rangepath.hi
      b, e = rangepath[hi]

      # If we're not in front yet, move back.
      if e - b > 1
        return rangepath.with(hi, {b, e - 1})
      end

      # When we transition from nonempty rangepath to empty rangepath, that's where
      # traversal ends.
      return if hi.zero?

      # If we can't move back (at zero) we should pop and recurse.
      rangepath = rangepath.without(rangepath.hi)
    end
  end

  private def enclosing0?(node, rangepath, predicate) : Term::Dict?
    return if rangepath.empty?
    return unless neighbor = follow?(node, rangepath.begin.grow(1))

    tail = rangepath.move(1)

    unless result = enclosing0?(neighbor, tail, predicate)
      if predicate.call(neighbor)
        result = rangepath.expand.upto(tail.begin).collect
      end
    end

    result
  end

  # Removes one range at a time from the end of *rangepath* until *predicate*
  # is `true` for the targeted node. Returns the resulting rangepath.
  #
  # In other words, returns the nearest node enclosing the node *rangepath* points
  # to, for which the predicate returns `true`.
  def enclosing?(root : Term, rangepath : Term::Dict::ItemsView, &predicate : Term -> Bool) : Term::Dict?
    unless needle = enclosing0?(root, rangepath, predicate)
      if predicate.call(root)
        needle = Term[]
      end
    end

    needle
  end

  # :ditto:
  def enclosing?(root, rangepath : Term::Dict, &predicate : Term -> Bool) : Term::Dict?
    enclosing?(root, rangepath.items, &predicate)
  end

  # Same as `enclosing?`, but raises `KeypathError` if *predicate* returns `false`
  # for all nodes targeted by *rangepath*.
  def enclosing(root, rangepath, &predicate : Term -> Bool) : Term::Dict?
    enclosing?(root, rangepath, &predicate) || raise KeypathError.new
  end

  # Returns a path to the nearest enclosing document.
  # TODO: remove in favor of more granularity.
  def docpath(root : Term, nodepath : Term::Dict) : Term::Dict
    enclosing(root, nodepath.items.grow(-1)) do |step|
      next false unless dict = step.as_d?
      next false unless dict.includes?(DocumentMarker)
      true
    end
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

  def effect(root : Term, nodepath : Term::Dict, node : Term, &) : Term
    builder = EffectBuilder.new(node)

    with builder yield builder

    docpath = docpath(root, nodepath)

    document0 = follow(root, docpath).as_d
    document1 = document0

    builder.events.each do |event|
      document1 = Q.of(document1).enqueue(event).commit(document1)
    end

    builder.cells.each do |k, v|
      document1 = document1.morph({Cells, k, v})
    end

    # Disappear allows the node to remove itself as if it didn't exist. This
    # lets us prevent expulsion.
    if builder.disappear? && (identity = identity?(node))
      document1 = document1.morph({Population, identity, nil})
    end

    # Replace document with its new version.
    unless document0.same?(document1)
      root = rewrite(root, docpath, Rewrite.one(document1))
    end

    # Replace node with its new version.
    if rewrite = builder.rewrite.as?(Rewrite::Some)
      root = rewrite(root, nodepath, rewrite)
    end

    builder.jobs.each do |job|
      root = root.morph({JobsPending, job, true})
    end

    Term.of(root)
  end

  # TODO: this does not belong here
  private def rightmost(xs, n)
        if 0 < n < xs.itemsize
          prefix = Term::Dict.build do |commit|
            (xs.itemsize - n...xs.itemsize).each do |index|
              commit << xs[index]
            end
          end
        else
          prefix = xs
        end
  end

  private def cursordepth0(node : Term, depth : Int32) : Int32
    Term.case(node) do
      matchp %[(_string | _string (_*) ≡@_)] { depth }
      matchp %(_dict) do
        dict = node.unsafe_as_d
        return Int32::MAX unless dict.probably_includes?(Term[:|])
        return Int32::MAX if dict.empty?

        dict.ee.min_of do |k, v|
          Math.min(cursordepth0(k, depth + 1), cursordepth0(v, depth + 1))
        end
      end

      otherwise { Int32::MAX }
    end
  end

  # Returns the depth at which the cursor is found in *node*. If *node* is
  # the cursor returns `0`. If the cursor is contained in one of *node*'s
  # entries returns `1` and so on. If there are multiple cursors in *node*
  # returns the depth of the closest cursor (i.e. minimum depth). If there
  # are no cursors in *node* returns `-1`.
  def cursordepth(node : Term) : Int32
    depth = cursordepth0(node, 0)
    depth == Int32::MAX ? -1 : depth
  end

  INITIAL_SUGGESTIONS = Term.of(
    {"absence", "Senses the absence of a cell"},
    {"blast", "Outputs the items of lists received at @pin, in order, at @pout"},
    {"button", "An element bridging UI and logic. Clicks will trigger a pulse on @pout"},
    {"cell", "Stores a term inside itself. Acts as a source of const signal"},
    {"changes", "Converts a const signal into a pulse whenever the former changes"},
    {"col", "Arranges its children in a vertical stack, with an optional gap"},
    {"decay", "Counts down until destroying itself and its children"},
    {"delay", "Counts down until replacing itself with its children"},
    {"echo", "Emits as event whatever pulse it received on @pin"},
    {"edit-cage", "Makes sure a cursor cannot escape"},
    {"edit-cast", "Converts a pulse to an edit command to the cursor"},
    {"event", "Emits an event and destroys itself"},
    {"group", "Groups nodes together without any logical/UI effect"},
    {"latest", "Converts incoming pulse signals to const"},
    {"log", "Shows last N pulses it received on @pin"},
    {"lookaround", "Can get a snapshot of the document behind and ahead of itself while hosting some children"},
    {"map", "Converts an incoming pulse to one of templated outputs using pattern-matching"},
    {"periodic", "Emits an event on every cycle"},
    {"pull", "Asks for more on @pin for every cycle until it gets something. Sends to @pout and waits while @pout completes"},
    {"queue", "Holds an unbounded number of terms. Guarantees dequeue only after the front was handled by the other side (successfully or not)"},
    {"row", "Arranges its children in a horizontal stack, with an optional gap"},
    {"transform", "Transforms incoming pulse into outgoing pulse using Nitrene"},
  )

  # periodic lookaround map edit-cage edit-cast decay delay absence transform pull queue event echo blast changes latest log button cell group row col
  def nodestep(root0 : Term, root1 : Term, nodepath, event)
    node0 = follow(root1, nodepath)

    Term.case({node0, event, cursordepth(node0)}) do
      givenpi %[(cell v_ @cout_) invited -1] do
        root1 = effect(root1, nodepath, node0) do
          event :"cell/created", cout, v
          cell cout, v
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell v_ @cout_ for pattern_) invited -1] do
        root1 = effect(root1, nodepath, node0) do
          if M1.probe?(pattern, v)
            event :"cell/created", cout, v
            cell cout, v
          else
            backmap ML.term(%[(cell v_ _ for _)]), Term.of(Term[].with({:v}, Term[]))
          end
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell v_ @cout_ _*) (assign @cout_ v_) -1] do
        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell v0_ @cout_) (assign @cout_ v1_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event :"cell/updated", cout, v0, v1
          cell cout, v1
          backmap %[(cell v_ @_)], v: v1
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell @cout_) (assign @cout_ v0_) -1] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(cell ⏏v @_)], v: v0
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell @cout_ for _) (assign @cout_ v0_) -1] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(cell ⏏v @_ for _)], v: v0
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell v0_ @cout_ for pattern_) (assign @cout_ v1_) -1] do
        if M1.probe?(pattern, v1)
          root1 = effect(root1, nodepath, node0) do
            event :"cell/updated", cout, v0, v1
            cell cout, v1
            backmap %[(cell v_ @_ _*)], v: v1
          end
        end

        {root1, successor?(root1, nodepath)}
      end

      # TODO: instead of (%number (whole _) > 0) we should have (%number i32 > 0). All +-variants must
      # allow the exclusion of zero this way.

      givenpi %[(cell vs0←(_*) @cout_) (assign/log @cout_ v_ limit←(%number (whole _) > 0)) -1] do
        vs1 = rightmost(vs0, limit.to(Int32) - 1).append(v)

        root1 = effect(root1, nodepath, node0) do
          event :"cell/updated", cout, vs0, vs1
          cell cout, vs1
          backmap %[(cell v_ @_)], v: vs1
        end

        {root1, successor?(root1, nodepath)}
      end

      # Button
      begin
        givenpi %[(button _ as @cin_ to @pout_ ((press) _*)) cycle _] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          root1 = effect(root1, nodepath, node0) do
            if msg = document[Cells, cin]?
              event :pulse, pout, msg
            end
            backmap %[(button _ as _ to @_ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button _ as msg_ to @pout_ ((press) _*)) cycle _] do
          root1 = effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            backmap %[(button _ as _ to @_ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button @cin_ to @pout_ ((press) _*)) cycle _] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          root1 = effect(root1, nodepath, node0) do
            if msg = document[Cells, cin]?
              event :pulse, pout, msg
            end
            backmap %[(button _ to _ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button msg_ to @pout_ ((press) _*)) cycle _] do
          root1 = effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            backmap %[(button _ to _ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        # The different kinds of buttons we have all support feedback. The patterns
        # are a bit loose but I guess it's fine. It's too much of a button to
        # not work.

        givenpi %[(button _* to @pout_ (_*) ¦ waiting⋮ 0) (feedback busy @pout_) _] do
          root1 = effect(root1, nodepath, node0) do
            change waiting: waiting + 1
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button _* to @pout_ (_*) ¦ waiting: 1) (feedback (%any done cancelled) @pout_) _] do
          root1 = effect(root1, nodepath, node0) do
            clear :waiting
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button _* to @pout_ (_*) ¦ waiting_: (%number (whole _) > 0)) (feedback (%any done cancelled) @pout_) _] do
          root1 = effect(root1, nodepath, node0) do
            change waiting: waiting - 1
          end

          {root1, successor?(root1, nodepath)}
        end
      end

      givenpi %[(log @pin_ in @cout_ ¦ limit⋮ 10) (pulse @pin_ term_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event :"assign/log", cout, term, limit
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(log @pin_ in (entries_*) ¦ limit: (%optional 10 limit←(%number (whole _) > 0))) (pulse @pin_ term_) (%not 1 2)] do
        root1 = effect(root1, nodepath, node0) do
          backmap ML.term(%[(log _ in (entries_*) ¦ _)]), Term.of(Term[].with({:entries}, rightmost(entries, limit.to(Int32) - 1).append(term)))
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(latest @pin_ @cout_) (pulse @pin_ v_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event :assign, cout, v
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(latest (@pin_ pattern_) (@cout_ form_)) (pulse @pin_ v_) -1] do
        if env = M1.match?(pattern, v)
          root1 = effect(root1, nodepath, node0) do
            event :assign, cout, M1.bsubst(form, env)
          end
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi(
        %[(changes @cin_ to @pout_) (cell/created @cin_ v_) -1],
        %[(changes @cin_ to @pout_) (cell/updated @cin_ _ v_) -1],
        %[(changes @cin_ to @pout_ as v_) (cell/created @cin_ _) -1],
        %[(changes @cin_ to @pout_ as v_) (cell/updated @cin_ _ _) -1],
      ) do
        root1 = effect(root1, nodepath, node0) do
          event :pulse, pout, v
        end

        {root1, successor?(root1, nodepath)}
      end

      # `blast`: inorder emission of items from lists received on `pin`.
      givenpi %[(blast @pin_ to @pout_) (pulse @pin_ list_dict) -1] do
        root1 = effect(root1, nodepath, node0) do
          list.items.each do |item|
            event :pulse, pout, item
          end
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(echo @pin_) (pulse @pin_ e_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event e
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(event e_) cycle -1] do
        root1 = effect(root1, nodepath, node0) do
          event e
          backmap %[N_], %[{(N): ()}]
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(queue @pin_ to @_ in (_*)) (pulse @pin_ value_) -1] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(_ _ to _ in (_* ⏏head))], head: value
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(queue @pin_ to @pout_ in (head_ _*)) (pull @pout_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event :pulse, pout, head
        end

        {root1, successor?(root1, nodepath)}
      end

      # Dequeue
      givenpi %[(queue @pin_ to @pout_ in (head_ _*)) (feedback completed @pout_ head_) -1] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(_ _ to _ in (head_ _*))], %[{(head): ()}]
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(pull @pout_ from @pin_) (pull @pout_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event :pull, pin
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(pull @pout_ from @pin_) (pulse @pin_ value_) -1] do
        root1 = effect(root1, nodepath, node0) do
          change pending: value
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi(
        %[(pull @pout_ from @pin_ pending: value_) (pull @pout_) -1],
      ) do
        root1 = effect(root1, nodepath, node0) do
          event :pulse, pout, value
          change state: :busy
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(pull @pout_ from @pin_ pending: value_ state: busy) (feedback (%any done cancelled) @pout_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event :feedback, :completed, pin, value
          clear :state, :pending
        end

        {root1, successor?(root1, nodepath)}
      end

      # Transform logic
      begin
        # For stateful transforms, signal it's ready to take a job if the state
        # is a hard-coded const or a cell with a known value.
        #
        # For stateless transforms, signal that we're ready unconditionally.
        givenpi %[(transform _* ¦ #spec: spec←{_ in: @pin_}) cycle -1] do
          ready = true

          if (state = spec[:state]?) && ML.edge?(state)
            # FIXME: how to get rid of this
            docpath = docpath(root0, nodepath)
            document = follow(root0, docpath)
            ready = !!document[Cells, state]?
          end

          if ready
            root1 = effect(root1, nodepath, node0) do
              event :pull, pin
            end
          end

          {root1, successor?(root1, nodepath)}
        end

        # Schedule job.
        givenpi %[(transform _* ¦ #spec: spec←{_ in: @pin_, body_}) (pulse @pin_ input_) -1] do
          if (state = spec[:state]?) && ML.edge?(state)
            docpath = docpath(root0, nodepath)
            document = follow(root0, docpath)
            unless state = document[Cells, state]?
              return root1, successor?(root1, nodepath)
            end
          end

          env0 = Term[state: state]

          # If a filter pattern is defined, make sure it matches.
          if filter = spec[:filter]?
            unless env1 = M1.match?(filter, input, env: env0)
              return root1, successor?(root1, nodepath)
            end
          end

          env1 ||= env0
          env1 = env1.with(:_, input)

          root1 = effect(root1, nodepath, node0) do
            change "#job": {program: body, env: env1}
          end

          {root1, successor?(root1, nodepath)}
        end

        # Send feedback busy. Schedule job.
        givenpi %[(transform _* ¦ #spec: {_ in: @pin_} #job: job_) invited -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :busy, pin
            schedule job
          end

          {root1, successor?(root1, nodepath)}
        end

        # Wait for the job to complete.
        givenpi %[(transform _* ¦ #spec: {_ in: @pin_, out: @pout_} #job: job_) (job/completed job_ result_) -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :done, pin
            event :pulse, pout, result
            clear :"#job"
            disappear
          end

          {root1, successor?(root1, nodepath)}
        end
      end

      # Stateful transform
      givenpi %[(transform @pin_ to @pout_ with state_ body_) initialize -1] do
        root1 = effect(root1, nodepath, node0) do
          change "#spec": {in: pin, out: pout, state: state, body: body}
        end

        {root1, successor?(root1, nodepath)}
      end

      # Stateless transform
      givenpi %[(transform @pin_ to @pout_ body_) initialize -1] do
        root1 = effect(root1, nodepath, node0) do
          change "#spec": {in: pin, out: pout, body: body}
        end

        {root1, successor?(root1, nodepath)}
      end

      # Stateless filter transform
      givenpi %[(transform (@pin_ pattern_) to @pout_ body_) initialize -1] do
        root1 = effect(root1, nodepath, node0) do
          change "#spec": {in: pin, out: pout, filter: pattern, body: body}
        end

        {root1, successor?(root1, nodepath)}
      end

      # Stateful filter transform
      givenpi %[(transform (@pin_ pattern_) to @pout_ with state_ body_) initialize -1] do
        root1 = effect(root1, nodepath, node0) do
          change "#spec": {in: pin, out: pout, filter: pattern, state: state, body: body}
        end

        {root1, successor?(root1, nodepath)}
      end

      # Absence node
      begin
        # Suggestions
        # TODO: it would be nice if we are able to auto-generate these !! Me
        # copy pasting this way won't scale!
        begin
          givenpi %[(absence (_string | _string () @user ¦ _ suggestions: (%- _))) cycle _] do
            suggestion = Term.of("(absence *@cin_* as msg_ to @pout_)", <<-SUGG
            *cin* - const whose absence should be detected
            msg - message to send
            pout - sink for the message
            SUGG
            )

            node1 = node0.morph({1, :suggestions, {suggestion}})
            root1 = assign(root1, nodepath, node1)

            {root1, successor?(root1, nodepath)}
          end

          givenpi %[(absence @_ (_string | _string () @user ¦ _ suggestions: (%- _))) cycle _] do
            suggestion = Term.of("(absence @cin_ *as* msg_ to @pout_)", <<-SUGG
            cin - const whose absence should be detected
            msg - message to send
            pout - sink for the message
            SUGG
            )
            node1 = node0.morph({2, :suggestions, {suggestion}})
            root1 = assign(root1, nodepath, node1)

            {root1, successor?(root1, nodepath)}
          end

          givenpi %[(absence @_ as (_string | _string () @user ¦ _ suggestions: (%- _))) cycle _] do
            suggestion = Term.of("(absence @cin_ as *msg_* to @pout_)", <<-SUGG
            cin - const whose absence should be detected
            *msg* - message to send
            pout - sink for the message
            SUGG
            )
            node1 = node0.morph({3, :suggestions, {suggestion}})
            root1 = assign(root1, nodepath, node1)

            {root1, successor?(root1, nodepath)}
          end

          givenpi %[(absence @_ as _ (_string | _string () @user ¦ _ suggestions: (%- _))) cycle _] do
            suggestion = Term.of("(absence @cin_ as msg_ *to* @pout_)", <<-SUGG
            cin - const whose absence should be detected
            msg - message to send
            pout - sink for the message
            SUGG
            )

            node1 = node0.morph({4, :suggestions, {suggestion}})
            root1 = assign(root1, nodepath, node1)

            {root1, successor?(root1, nodepath)}
          end

          givenpi %[(absence @_ as _ to (_string | _string () @user ¦ _ suggestions: (%- _))) cycle _] do
            suggestion = Term.of("(absence @cin_ as msg_ to *@pout_*)", <<-SUGG
            cin - const whose absence should be detected
            msg - message to send
            *pout* - sink for the message
            SUGG
            )

            node1 = node0.morph({5, :suggestions, {suggestion}})
            root1 = assign(root1, nodepath, node1)

            {root1, successor?(root1, nodepath)}
          end
        end

        # Initialize `absence` to newborn state.
        givenpi %[(absence @_ as _ to @_) cycle -1] do
          root1 = effect(root1, nodepath, node0) do
            change "#state": :newborn
          end

          {root1, successor?(root1, nodepath)}
        end

        # Whenever we're in newborn state, on cycle, look around to see if the cell's
        # identity is in the population.
        givenpi %[(absence @cin_ as msg_ to @pout_ #state: newborn) cycle -1] do
          root1 = effect(root1, nodepath, node0) do
            partner = Term.of(:cell, cin)

            # FIXME: how to get rid of this
            docpath = docpath(root1, nodepath)
            document = follow(root1, docpath).as_d

            if partner.in?(document[Population]? || Term[])
              change "#state": :paired
            else
              event :pulse, pout, msg
              change "#state": :unpaired
            end
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(absence @cin_ as msg_ to @pout_ #state: paired) (cell/removed @cin_) -1] do
          root1 = effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            change "#state": :unpaired
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(absence @cin_ as _ to @_ #state: unpaired) (cell/created @cin_ _) -1] do
          root1 = effect(root1, nodepath, node0) do
            change "#state": :paired
          end

          {root1, successor?(root1, nodepath)}
        end
      end

      givenpi %[(delay 0 children_*) cycle _] do
        root1 = rewrite(root1, nodepath, Rewrite.many(children.unsafe_as_d))

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(delay n←(%number +i32) _*) cycle _] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(delay n_ _*)], n: n - 1
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(decay 0 _*) cycle _] do
        root1 = rewrite(root1, nodepath, Rewrite.many(Term[]))

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(decay n←(%number +i32) _*) cycle _] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(decay n_ _*)], n: n - 1
        end

        {root1, successor?(root1, nodepath)}
      end

      # `edit-cast`: converts pulse signal to root-centric edit broadcast.
      givenpi %[(edit-cast @pin_ to @bout_) (pulse @pin_ motion_) -1] do
        root1 = effect(root1, nodepath, node0) do
          event :edit, bout, motion
        end

        {root1, successor?(root1, nodepath)}
      end

      # `edit-cage`: converts pulse signal to children-centric non-broadcast (private) edit.
      givenpi %[(edit-cage for @pin_ children_*) (pulse @pin_ motion_) _] do
        root1 = effect(root1, nodepath, node0) do
          backmap ML.term(%[(_ _ _ children_*)]), Term[].with({:children}, edit(children, motion, edge: pin, smart: true)).upcast
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(map @pin_ to @pout_ (%group conds (%past (_ _) min: 1))) (pulse @pin_ input_) -1] do |conds|
        conds.items.each do |(pattern, template)|
          next unless env = M1.match?(pattern, input)

          output = M1.bsubst(template, env)

          root1 = effect(root1, nodepath, node0) do
            event :pulse, pout, output
          end

          break
        end

        {root1, successor?(root1, nodepath)}
      end

      # Lookaround can look behind and ahead on demand. It can also contain children.
      # On the children part, there is no isolation; it works just like `group`.
      #
      # FIXME: I don't think the latter is OK behavior but will do for now.
      givenpi %[(lookaround @behind_out_ @ahead_out_ @pin_ children_*) (pulse @pin_ _) _] do
        range = nodepath.items.last

        cursor_b, cursor_e = range
        behind_b = cursor_b.as_n
        behind_e = cursor_e.as_n - 1

        parent = follow(root1, nodepath.items.grow(-1)).as_d

        if nodepath.size == 1
          predrange = 0...root1.itemsize
        else
          predrange = passable_range?(Term.of(parent)) || raise ""
        end

        ahead_b = cursor_e.as_n
        ahead_e = Term[predrange.end]

        behind = parent.items(behind_b, behind_e)
        ahead = parent.items(ahead_b, ahead_e)

        root1 = effect(root1, nodepath, node0) do
          event :pulse, behind_out, behind
          event :pulse, ahead_out, ahead
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(periodic e_) cycle -1] do
        root1 = effect(root1, nodepath, node0) do
          event e
        end

        {root1, successor?(root1, nodepath)}
      end

      # Suggestions
      #
      # When we see a cursor in a suitable position, we populate it with a list
      # of suggestions. What the cursor/UI does with them is not of our interest.
      begin
        givenpi %{((_string | _string () @user ¦ _ suggestions: (%- _))) cycle _} do
          node1 = node0.morph({0, :suggestions, INITIAL_SUGGESTIONS})
          root1 = assign(root1, nodepath, node1)
          {root1, successor?(root1, nodepath)}
        end
      end

      otherwise do
        {root1, successor?(root1, nodepath)}
      end
    end
  end

  def expel(root0, root1, docpath, identity : Term)
    Term.case(identity) do
      matchpi %[(cell @cout_)] do
        document = follow(root1, docpath).as_d
        document = document.morph({Cells, cout, nil})

        assign(root1, docpath, Q.of(document).enqueue(:"cell/removed", cout).commit(document))
      end

      matchpi %[(transform @pin_ _)] do
        document = follow(root1, docpath).as_d

        assign(root1, docpath, Q.of(document).enqueue(:feedback, :cancelled, pin).commit(document))
      end

      otherwise { root1 }
    end
  end

  def advance(root0, root1, docpath, event) : {Term, Term::Dict?}
    nodepath = successor?(root1, docpath)

    while nodepath && nodepath.size > docpath.size
      root1, nodepath = nodestep(root0, root1, nodepath, event)
    end

    {root1, nodepath}
  end

  def advance(root0 : Term, root1 : Term, docpath : Term::Dict, *, initialized = false)
    return root1 unless document0 = follow?(root1, docpath)
    return root1 unless document0 = document0.as_d?

    unless initialized
      root1, _ = advance(root0, root1, docpath, Term.of(:initialize))
      root1 = transition(root1, docpath)
      return advance(root1, root1, docpath, initialized: true)
    end

    queue = Q.of(document0)
    if event = queue.first?
      document1 = queue.dequeue.commit(document0)
    else
      event = Term.of(:cycle)
      document1 = document0
    end

    root1 = assign(root1, docpath, document1)

    if docpath.empty? # Root
      Term.case(event) do
        # Edit-events are potentially destructive so we start from scratch after
        # them, using recursion in this case.
        matchpi %[(edit @edge_ motion_)] do
          return advance(edit(root1, motion, edge, smart: true), Term[])
        end

        otherwise { }
      end
    end

    root1, _ = advance(root0, root1, docpath, event)

    transition(root1, docpath)
  end

  def advance(root0 : Term, docpath : Term::Dict)
    advance(root0, root0, docpath)
  end

  def identity?(node : Term)
    Term.case({node, cursordepth(node)}) do
      givenpi %{(cell _ @cout_ _*) -1} do
        Term.of(:cell, cout)
      end

      givenpi %{(transform _* ¦ #spec: {_ in: @pin_} #job: job_) -1} do
        Term.of(:transform, pin, job)
      end

      otherwise {}
    end
  end

  def transition(root0 : Term, docpath : Term::Dict)
    document0 = follow(root0, docpath)

    population0 = document0[Population]? || Term[]
    population1 = Term[]

    nodepath = successor?(root0, docpath)

    # v To be modified by invited nodes or expulsion.
    root1 = root0

    while nodepath
      node = follow(root0, nodepath)

      # Ask each node for its identity. If it has one, we do the population thing.
      # It it does not, we ignore the node and move on.
      if identity = identity?(node)
        if identity.in?(population0)
          # Prolong
          population0 = population0.without(identity)
          population1 = population1.with(identity, true)
        elsif !identity.in?(population1)
          # Invite
          root1, nodepath = nodestep(root0, root1, nodepath, Term.of(:invited))
          population1 = population1.with(identity, true)
          next
        end
      end

      nodepath = successor?(root0, nodepath)
    end

    population0.each_entry do |identity, _|
      # Expel. Note that this cannot be an event, because in fact, there's no node
      # to send it to; only to that node's identity, its "ghost", the document's
      # "memory" of the node.
      root1 = expel(root0, root1, docpath, identity)
    end

    document1 = follow(root1, docpath).morph({Population, population1})

    assign(root1, docpath, document1)
  end

  def publish(root : Term)
    root = root.as_d
    if results = root[JobsCompleted]?
      results.each_entry do |job, value|
        root = Q.of(root).enqueue(:"job/completed", job, value).commit(root)
      end
      root = root.without(JobsCompleted)
    end

    Term.of(root)
  end

  def next(root0 : Term) : Term
    # TODO: remove DocumentMarker in favor of more granularity
    pipe(root0.morph({DocumentMarker, true}).upcast, transition(docpath: Term[]), publish, advance(docpath: Term[]))
  end
end

module D7
  def self.hidden?(symbol : Term::Sym)
    symbol.to(String).prefixed_by?('#')
  end

  # Strips hidden pairs from *root*.
  #
  # NOTE: in D7, we rely on the convention that all hidden pairs have a key that
  # is prefixed with '#'.
  def self.visible(root : Term) : Term
    # TODO: we should probably use D.successor? here
    root = root.as_d? || return root

    root = root.transaction do |commit|
      root.each_item_with_index do |item, index|
        commit.with(index, visible(item))
      end

      root.each_pair do |key, value|
        next unless symbol = key.as_sym?
        next unless hidden?(symbol)

        commit.without(key)
      end
    end

    Term.of(root)
  end

  def self.handle(root0 : Term) : Term
    root1 = root0

    if jobs_pending = root0[D::JobsPending]?
      execute = ->(program : Term, env : Term::Dict) do
        rewrite(program, Nitrene.rewriter, env: env)
      end

      jobs_pending.each_entry do |job, _|
        Term.case(job) do
          matchpi %[(¦ () program_ env_dict)] do
            root1 = root1.morph({D::JobsCompleted, job, execute.call(program, env.unsafe_as_d)})
          end
        end
      end

      root1 = root1.without(D::JobsPending)
    end

    Term.of(root1)
  end

  def self.next(root0 : Term) : Term
    pipe(root0, D.next, handle)
  end

  class Interrupted < Exception
    getter initial : Term
    getter last : Term

    def initialize(@initial, @last)
      @message = "could not reach goal state"
    end
  end

  # Advances *root* through a fixed or unlimited number of cycles, set by *max_cycles*.
  # Yields intermediate roots and expects the block to return `true` whenever the yielded
  # root is a terminal root. Returns the terminal root. Raises `Interrupted` when *max_cycles*
  # is exceeded.
  def self.run(initial : Term, *, max_cycles : Int32? = nil, & : Term -> Bool) : Term
    root0 = initial

    while max_cycles.nil? || max_cycles > 0
      root1 = D7.next(root0)
      if yield root1
        return root1
      end

      # At this point we assume equality is cheap, even on large dicts. Thus have
      # a cheap way to bail out without exceeding the limit through pointless
      # computation.
      break if root0 == root1

      root0 = root1

      max_cycles -= 1 if max_cycles
    end

    raise Interrupted.new(initial, root0)
  end
end

module Nitrene
  REWRITER = chainR(using(plug(:env), dfsR(envR)), callR(PRIMITIVES))

  def self.rewriter : Rewriter
    REWRITER
  end
end

# Proud to announce the World's Best Debugger!!!
# doc = <<-WWML
# (cell 10 @count for (%number (whole _) > 0))
# (changes @count to @deltas as -1)
# (transform @deltas to @counts with @count (+ state _))
# (latest @counts @count)
# (changes @count to @counts~)
# (log @counts~ in () limit: 5)
# WWML

# D7.run(ML.terms(doc)) do |im|
#   puts ML.display(im)
#   gets
#   false
# end

# running0 = Set(Term).new
# results = Deque({Term, Term}).new
# lock = Mutex.new

# class JobInterrupted < Exception
# end

# ctx = ExecutionContext::MultiThreaded.new("mt", 4)
# primitives = ProcRuleset.build do
#   rulepi1 %[(+ a_number b_number)] { a + b }
# end

# while true
#   root = D.run(root) do |im|
#     puts ML.display(im, maxwidth: 80)
#     sleep 100.milliseconds

#     # Schedule/un-schedule jobs

#     running1 = Set(Term).new

#     (im[JobsPending]? || Term[]).each_entry do |job, _|
#       running1 << job
#     end

#     im = im.without(JobsPending)

#     lock.synchronize do
#       (running1 - running0).each do |job|
#         ctx.spawn do
#           # # Artificial delay
#           # chan = Channel(Nil).new
#           # ctx.spawn do
#           #   sleep 3.seconds
#           #   chan.send(nil)
#           # end
#           # chan.receive
#           result = rewrite(job[:program], chainR(using(job[:env].as_d, dfsR(envR)), callR(primitives))) do
#             unless lock.synchronize { job.in?(running0) }
#               raise JobInterrupted.new
#             end
#           end

#           lock.synchronize do
#             results << {job, result}
#           end
#         rescue JobInterrupted
#           puts "Job interrupted"
#         end
#       end

#       running0.concat(running1)

#       # Import job results
#       results.each do |job, value|
#         im = im.morph({:results, job, value})
#         running0.delete(job)
#       end
#       results.clear
#     end

#     im
#   end

#   break if lock.synchronize { running0.empty? }
# end

# puts ML.display(root, maxwidth: 120)
# kp = Term[]
# while succ = D.successor?(root, kp)
#   pp kp
#   kp = succ
# end
# pp kp
