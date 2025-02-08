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
      matchpi %[(group _*)] { 1...node.itemsize }
      matchpi %[(edit-cage for @_ _*)] { 3...node.itemsize }
      matchpi %[(decay (%number +i32) _*)] { 2...node.itemsize }

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

  # Returns a path to the nearest enclosing document.
  # TODO: remove in favor of more granularity.
  def docpath(root : Term, nodepath : Term::Dict) : Term::Dict
    path = nodepath.items.grow(-1)

    until path.empty?
      candidate = follow(root, path)
      if candidate[:"(document)"]?
        return path.collect
      end
      path = path.grow(-1)
    end

    Term[]
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

  def nodestep(root0 : Term, root1 : Term, nodepath, event)
    node0 = follow(root1, nodepath)

    Term.case({node0, event, cursordepth(node0)}) do
      givenpi %[(cell v_ @cout_ _*) invited -1] do
        root1 = effect(root1, nodepath, node0) do
          event :"cell/created", cout, v
          cell cout, v
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

      givenpi %[(log @pin_ in (entries_*) ¦ limit: (%optional 10 limit←(%number (whole _) > 0))) (pulse @pin_ term_) -1] do
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

      # TODO: note how these transforms are almost exact copies of each other. There must
      # be a way to "protocolize" the "behavior" of a transform while allowing for individual
      # variations to reduce redundancy.

      # Stateful transform
      begin
        # Signal that we're ready for a job
        givenpi %[(transform @pin_ to @pout_ with @cin_ _) cycle -1] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          if document[Cells, cin]?
            root1 = effect(root1, nodepath, node0) do
              event :pull, pin
            end
          end

          {root1, successor?(root1, nodepath)}
        end

        # Schedule job
        givenpi %[(transform @pin_ to @pout_ with @cin_ body_) (pulse @pin_ input_) -1] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          if state = document[Cells, cin]?
            root1 = effect(root1, nodepath, node0) do
              change job: {program: body, env: {"_": input, state: state}}
            end
          end

          {root1, successor?(root1, nodepath)}
        end

        # Send feedback busy
        givenpi %[(transform @pin_ to @_ with @_ _ ¦ () job_) invited -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :busy, pin
            schedule job
          end

          {root1, successor?(root1, nodepath)}
        end

        # Wait for the job to complete
        givenpi %[(transform @pin_ to @pout_ with @cin_ body_ job: job_) (job/completed job_ v_) -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :done, pin
            event :pulse, pout, v
            clear :job
            disappear
          end

          {root1, successor?(root1, nodepath)}
        end
      end

      # Stateless transform
      begin
        # Signal that we're ready for a job
        givenpi %[(transform @pin_ to @pout_ body_) cycle -1] do
          root1 = effect(root1, nodepath, node0) do
            event :pull, pin
          end

          {root1, successor?(root1, nodepath)}
        end

        # Schedule job
        givenpi %[(transform @pin_ to @pout_ body_) (pulse @pin_ input_) -1] do
          root1 = effect(root1, nodepath, node0) do
            change job: {program: body, env: {"_": input}}
          end

          {root1, successor?(root1, nodepath)}
        end

        # Send feedback busy
        givenpi %[(transform @pin_ to @_ _ ¦ () job_) invited -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :busy, pin
            schedule job
          end

          {root1, successor?(root1, nodepath)}
        end

        # Wait for the job to complete
        givenpi %[(transform @pin_ to @pout_ body_ job: job_) (job/completed job_ v_) -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :done, pin
            event :pulse, pout, v
            clear :job
            disappear
          end

          {root1, successor?(root1, nodepath)}
        end
      end

      # Stateless filter transform
      begin
        # Signal that we're ready for a job
        givenpi %[(transform (@pin_ pattern_) to @pout_ body_) cycle -1] do
          root1 = effect(root1, nodepath, node0) do
            event :pull, pin
          end

          {root1, successor?(root1, nodepath)}
        end

        # Schedule job
        givenpi %[(transform (@pin_ pattern_) to @pout_ body_) (pulse @pin_ input_) -1] do
          if env = M1.match?(pattern, input)
            root1 = effect(root1, nodepath, node0) do
              change job: {program: body, env: env}
            end
          end

          {root1, successor?(root1, nodepath)}
        end

        # Send feedback busy
        givenpi %[(transform (@pin_ _) to @_ _ ¦ () job_) invited -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :busy, pin
            schedule job
          end

          {root1, successor?(root1, nodepath)}
        end

        # Wait for the job to complete
        givenpi %[(transform (@pin_ _) to @pout_ body_ job: job_) (job/completed job_ v_) -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :done, pin
            event :pulse, pout, v
            clear :job
            disappear
          end

          {root1, successor?(root1, nodepath)}
        end
      end

      # Stateful filter transform
      begin
        # Signal that we're ready for a job
        givenpi %[(transform (@pin_ pattern_) to @pout_ with @cin_ body_) cycle -1] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          if document[Cells, cin]?
            root1 = effect(root1, nodepath, node0) do
              event :pull, pin
            end
          end

          {root1, successor?(root1, nodepath)}
        end

        # Schedule job
        givenpi %[(transform (@pin_ pattern_) to @pout_ with @cin_ body_) (pulse @pin_ input_) -1] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          if state = document[Cells, cin]?
            if env = M1.match?(pattern, input)
              root1 = effect(root1, nodepath, node0) do
                change job: {program: body, env: env.with(:state, state)}
              end
            end
          end

          {root1, successor?(root1, nodepath)}
        end

        # Send feedback busy
        givenpi %[(transform (@pin_ _) to @_ with @_ _ ¦ () job_) invited -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :busy, pin
            schedule job
          end

          {root1, successor?(root1, nodepath)}
        end

        # Wait for the job to complete
        givenpi %[(transform (@pin_ _) to @pout_ with @_ body_ job: job_) (job/completed job_ v_) -1] do
          root1 = effect(root1, nodepath, node0) do
            event :feedback, :done, pin
            event :pulse, pout, v
            clear :job
            disappear
          end

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
          backmap ML.term(%[(_ _ _ children_*)]), Term[].with({:children}, edit(children, motion, edge: pin)).upcast
        end

        {root1, successor?(root1, nodepath)}
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

  def advance(root0 : Term, root1 : Term, docpath : Term::Dict)
    return root1 unless document0 = follow?(root1, docpath)
    return root1 unless document0 = document0.as_d?

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
          return advance(edit(root1, motion, edge), Term[])
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

      givenpi(
        %{(transform @pin_ to @_ with @_ _ ¦ () job_) -1},
        %{(transform @pin_ to @_ _ ¦ () job_) -1},
        %{(transform (@pin_ _) to @_ _ ¦ () job_) -1},
        %{(transform (@pin_ _) to @_ with @_ _ ¦ () job_) -1},
      ) do
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

  def run1(root0 : Term) : Term
    pipe(root0, transition(docpath: Term[]), publish, advance(docpath: Term[]))
  end

  def run(root0 : Term, & : Term::Dict -> Term::Dict)
    # FIXME: this is a hack. Maybe there are more efficient termination conditions
    # than this. This will leak memory forever if nonperiodic or infinitely nesting.
    # We don't have any problem with nonperiodic or infinitely nesting -- we can rewrite
    # forever, that's fine. But the fact that this leaks memory is not.
    history = Set(Term).new

    root0 = root0.as_d

    while true
      root0 = yield root0
      root1 = run1(Term.of(root0))
      unless history.add?(root1)
        return root1
      end

      root0 = root1.as_d
    end
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

  def self.run1(root0 : Term) : Term
    pipe(root0, D.run1, handle)
  end

  def self.run(root0 : Term, & : Term::Dict ->) : Term
    D.run(root0) { |root1| doc = handle(Term.of(root1)).as_d; yield doc; doc }
  end

  def self.run(root0 : Term)
    run(root0) { }
  end
end

module Nitrene
  PRIMITIVES = ProcRuleset.build do
    rulepi1 %[(+ a_number b_number)] { a + b }
    rulepi1 %[(- a_number b_number)] { a - b }
    rulepi1 %[(* a_number b_number)] { a * b }
  end

  REWRITER = chainR(using(plug(:env), dfsR(envR)), callR(PRIMITIVES))

  def self.rewriter : Rewriter
    REWRITER
  end
end

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
