# this thing is a prototype. yes it is f*cking slow. yes it is stupid
# yes it is underdesigned. so be it. i don't know how to design it well.
# yes i had 20 tries. yes most of them succeeded. yes all of them were shitty.
# experience leads to better design. brick by brick one builds a house
#
# regardless, this code is a big big pile of poo !
#
# hopefully it turns into something nicer one day.

require "./wirewright"
require "execution_context"
require "./baz5"

# Functions related to the µsoma document queue.
# TODO: maybe we should get rid of this, for simplicity? It's just a prepend()/append()
# And we're going to optimize the former some day in the future...
module Q
  extend self

  # The empty queue.
  ZERO = Term[:queue, Term[], lo: -1, hi: -1]

  # Returns `true` if *dict* appears to be a queue.
  def queue?(dict : Term::Dict) : Bool
    Term.case(dict) do
      givenp %(queue _dict lo: _number hi: _number) { true }
      otherwise { false }
    end
  end

  # Returns `true` if *queue* is empty.
  def empty?(queue : Term::Dict) : Bool
    first?(queue).nil?
  end

  # Returns `true` if *queue* consists of a single item and that single item is *term*.
  def singleton?(queue : Term::Dict, term : Term) : Bool
    Term.case(queue) do
      givenpi %(queue q_dict lo: lo_number hi: _number) { q.size == 1 && q[lo] == term }
    end
  end

  # Returns the amount of items in *queue*.
  def size(queue : Term::Dict) : Int32
    Term.case(queue) do
      givenpi %(queue q_dict lo: _number hi: _number) { q.size }
    end
  end

  # Returns the first item in the queue. Returns `nil` if there are no items in
  # the queue. Raises `ArgumentError` if the queue is malformed.
  def first?(queue : Term::Dict) : Term?
    Term.case(queue) do
      givenpi %(queue _dict lo: lo_number hi: _number) { queue[1, lo]? }
    end
  end

  # Rewrites *queue0* according to the given *command*. Returns the rewritten queue.
  # Raises `ArgumentError` if *queue0* or *command* are malformed.
  #
  # Supported *command*s:
  #
  # - `(dequeue)`: remove the first item from the queue. See `first?` if you want
  #   to retrieve it first.
  # - `(enqueue t_)`: add `t_` to the back of the queue.
  # - `(interject t_)`: add `t_` to the front of the queue.
  def next(queue0 : Term::Dict, command : Term) : Term::Dict
    Term.case({queue0, command}) do
      givenpi %[(queue () lo: _number hi: _number) (dequeue)] do
        queue0
      end

      givenpi %[(queue q_dict lo: lo_number hi: _number) (dequeue)] do
        queue0.morph({1, q.without(lo)}, {:lo, lo + 1})
      end

      givenpi %[(queue _dict lo: _number hi: hi_number) (enqueue t_)] do
        queue0.morph({1, hi, t}, {:hi, hi + 1})
      end

      givenpi %[(queue _dict lo: lo_number hi: _number) (interject t_)] do
        queue0.morph({1, lo - 1, t}, {:lo, lo - 1})
      end
    end
  end
end

module D
  extend self

  Queue = Term.of(:"(queue)")

  # Returns *document*'s queue (`Q`).
  def queue(document : Term::Dict) : Term::Dict
    return Q::ZERO unless queue = document[Queue]?
    return Q::ZERO unless queue = queue.as_d?
    return Q::ZERO unless Q.queue?(queue)

    queue
  end

  # Sets the *document*'s queue to *queue*.
  def set(document : Term::Dict, *, queue : Term::Dict)
    document.with(Queue, queue)
  end

  # Constructs an "interject" command for the control queue.
  def interject(*args, **kwargs) : Term
    Term.of(:interject, Term.of(*args, **kwargs))
  end

  # Constructs an "enqueue" command for the control queue.
  def enqueue(*args, **kwargs) : Term
    Term.of(:enqueue, Term.of(*args, **kwargs))
  end

  def exeq(document, command : Term)
    queue0 = queue(document)
    queue1 = Q.next(queue0, command)

    set(document, queue: queue1)
  end

  def passable_range?(node : Term) : Range(Int32, Int32)?
    Term.case(node) do
      matchpi %[(group _*)] { 1...node.itemsize }
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
    if root_dict = root.as_d?
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
    getter locals = [] of {Term, Term, Term}
    getter jobs = [] of Term
    getter? disappear = false

    def initialize(@node : Term)
    end

    def disappear : Nil
      @disappear = true
    end

    def local(scope, k, v)
      locals << {Term.of(scope), Term.of(k), Term.of(v)}
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

    def clear(key)
      unless @rewrite.is_a?(Rewrite::None)
        raise "cannot use multiple different rewrite methods, please use only one"
      end

      @rewrite = Rewrite.one(@node.without(key))
    end
  end

  def effect(root : Term, nodepath : Term::Dict, node : Term, &) : Term
    builder = EffectBuilder.new(node)

    with builder yield builder

    docpath = docpath(root, nodepath)

    document0 = follow(root, docpath).as_d
    document1 = document0

    builder.events.each do |event|
      document1 = exeq(document1, enqueue(event))
    end

    builder.locals.each do |scope, k, v|
      document1 = document1.morph({:"(locals)", scope, k, v})
    end

    # Disappear allows the node to remove itself as if it didn't exist. This
    # lets us prevent expulsion.
    if builder.disappear? && (identity = identity?(node))
      document1 = document1.morph({:"(population)", identity, nil})
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
      root = root.morph({:"(jobs)", job, true})
    end

    Term.of(root)
  end

  # - log to cell limit
  # - tests (p6_test) support, move examples to tests

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

  def nodestep(root0 : Term, root1 : Term, nodepath, event)
    node0 = follow(root1, nodepath)

    Term.case({node0, event}) do
      givenpi %[(cell v_ @cout_ _*) invited] do
        root1 = effect(root1, nodepath, node0) do
          event :"cell/created", cout, v
          local :cells, cout, v
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell v_ @cout_ _*) (assign @cout_ v_)] do
        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell v0_ @cout_) (assign @cout_ v1_)] do
        root1 = effect(root1, nodepath, node0) do
          event :"cell/updated", cout, v0, v1
          local :cells, cout, v1
          backmap %[(cell v_ @_)], v: v1
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell v0_ @cout_ for pattern_) (assign @cout_ v1_)] do
        if M1.probe?(pattern, v1)
          root1 = effect(root1, nodepath, node0) do
            event :"cell/updated", cout, v0, v1
            local :cells, cout, v1
            backmap %[(cell v_ @_ _*)], v: v1
          end
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(cell vs0←(_*) @cout_) (assign/log @cout_ v_)] do
        vs1 = vs0.append(v)

        root1 = effect(root1, nodepath, node0) do
          event :"cell/updated", cout, vs0, vs1
          local :cells, cout, vs1
          backmap %[(cell v_ @_)], v: vs1
        end

        {root1, successor?(root1, nodepath)}
      end

      # Button
      begin
        givenpi %[(button _ as @cin_ to @pout_ ((press) _*)) cycle] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          root1 = effect(root1, nodepath, node0) do
            if msg = document[:"(locals)", :cells, cin]?
              event :pulse, pout, msg
            end
            backmap %[(button _ as _ to @_ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button _ as msg_ to @pout_ ((press) _*)) cycle] do
          root1 = effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            backmap %[(button _ as _ to @_ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button @cin_ to @pout_ ((press) _*)) cycle] do
          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath)

          root1 = effect(root1, nodepath, node0) do
            if msg = document[:"(locals)", :cells, cin]?
              event :pulse, pout, msg
            end
            backmap %[(button _ to _ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button msg_ to @pout_ ((press) _*)) cycle] do
          root1 = effect(root1, nodepath, node0) do
            event :pulse, pout, msg
            backmap %[(button _ to _ (action_ _*))], %[{(action): ()}]
          end

          {root1, successor?(root1, nodepath)}
        end

        # The different kinds of buttons we have all support feedback. The patterns
        # are a bit loose but I guess it's fine. It's too much of a button to
        # not work.

        givenpi %[(button _* to @pout_ (_*) ¦ waiting⋮ 0) (feedback wait @pout_)] do
          root1 = effect(root1, nodepath, node0) do
            change waiting: waiting + 1
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button _* to @pout_ (_*) ¦ waiting: 1) (feedback (%any ready cancelled) @pout_)] do
          root1 = effect(root1, nodepath, node0) do
            clear :waiting
          end

          {root1, successor?(root1, nodepath)}
        end

        givenpi %[(button _* to @pout_ (_*) ¦ waiting_: (%number (whole _) > 0)) (feedback (%any ready cancelled) @pout_)] do
          root1 = effect(root1, nodepath, node0) do
            change waiting: waiting - 1
          end

          {root1, successor?(root1, nodepath)}
        end
      end

      givenpi %[(log @pin_ in @cout_ ¦ limit⋮ 10) (pulse @pin_ term_)] do
        root1 = effect(root1, nodepath, node0) do
          event :"assign/log", cout, term, limit
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(log @pin_ in (entries_*) ¦ limit: (%optional 10 limit←(%number (whole _) > 0))) (pulse @pin_ term_)] do
        root1 = effect(root1, nodepath, node0) do
          backmap ML.term(%[(log _ in (entries_*) ¦ _)]), Term.of(Term[].with({:entries}, rightmost(entries, limit.to(Int32) - 1).append(term)))
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(latest @pin_ @cout_) (pulse @pin_ v_)] do
        root1 = effect(root1, nodepath, node0) do
          event :assign, cout, v
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi(
        %[(changes @cin_ to @pout_) (cell/created @cin_ v_)],
        %[(changes @cin_ to @pout_) (cell/updated @cin_ _ v_)],
        %[(changes @cin_ to @pout_ as v_) (cell/created @cin_ _)],
        %[(changes @cin_ to @pout_ as v_) (cell/updated @cin_ _ _)],
      ) do
        root1 = effect(root1, nodepath, node0) do
          event :pulse, pout, v
        end

        {root1, successor?(root1, nodepath)}
      end

      # Schedule job
      givenpi %[(transform @pin_ to @pout_ with @cin_ body_) (pulse @pin_ input_)] do
        # FIXME: how to get rid of this
        docpath = docpath(root1, nodepath)
        document = follow(root1, docpath)

        if state = document[:"(locals)", :cells, cin]?
          root1 = effect(root1, nodepath, node0) do
            change job: {program: body, env: {"_": input, state: state}}
          end
        end

        {root1, successor?(root1, nodepath)}
      end

      # Send feedback wait
      givenpi %[(transform @pin_ to @_ with @_ _ ¦ () job_) invited] do
        root1 = effect(root1, nodepath, node0) do
          event :feedback, :wait, pin
          schedule job
        end

        {root1, successor?(root1, nodepath)}
      end

      # Wait for the job to complete
      givenpi %[(transform @pin_ to @pout_ with @cin_ body_ job: job_) (job/completed job_ v_)] do
        root1 = effect(root1, nodepath, node0) do
          event :feedback, :ready, pin
          event :pulse, pout, v
          clear :job
          disappear
        end

        {root1, successor?(root1, nodepath)}
      end

      # Initialize `absence` to newborn state.
      givenpi %[(absence @_ as _ to @_) cycle] do
        root1 = effect(root1, nodepath, node0) do
          change state: :newborn
        end

        {root1, successor?(root1, nodepath)}
      end

      # Whenever we're in newborn state, on cycle, look around to see if the cell's
      # identity is in the population.
      givenpi %[(absence @cin_ as msg_ to @pout_ state: newborn) cycle] do
        root1 = effect(root1, nodepath, node0) do
          partner = Term.of(:cell, cin)

          # FIXME: how to get rid of this
          docpath = docpath(root1, nodepath)
          document = follow(root1, docpath).as_d

          if partner.in?(document[:"(population)"]? || Term[])
            change state: :paired
          else
            event :pulse, pout, msg
            change state: :unpaired
          end
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(absence @cin_ as msg_ to @pout_ state: paired) (cell/removed @cin_)] do
        root1 = effect(root1, nodepath, node0) do
          event :pulse, pout, msg
          change state: :unpaired
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(absence @cin_ as _ to @_ state: unpaired) (cell/created @cin_ _)] do
        root1 = effect(root1, nodepath, node0) do
          change state: :paired
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(delay 0 children_*) cycle] do
        root1 = rewrite(root1, nodepath, Rewrite.many(children.unsafe_as_d))

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(delay n←(%number +i32) _*) cycle] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(delay n_ _*)], n: n - 1
        end

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(decay 0 _*) cycle] do
        root1 = rewrite(root1, nodepath, Rewrite.many(Term[]))

        {root1, successor?(root1, nodepath)}
      end

      givenpi %[(decay n←(%number +i32) _*) cycle] do
        root1 = effect(root1, nodepath, node0) do
          backmap %[(decay n_ _*)], n: n - 1
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
        document = document.morph({:"(locals)", :cells, cout, nil})

        assign(root1, docpath, exeq(document, enqueue(:"cell/removed", cout)))
      end

      matchpi %[(transform @pin_ _)] do
        document = follow(root1, docpath).as_d

        assign(root1, docpath, exeq(document, enqueue(:feedback, :cancelled, pin)))
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

    queue0 = queue(document0)
    if event = Q.first?(queue0)
      queue1 = Q.next(queue0, Term.of({:dequeue}))
      document1 = set(document0, queue: queue1)
    else
      event = Term.of(:cycle)
      document1 = document0
    end

    root1 = assign(root1, docpath, document1)
    root1, _ = advance(root0, root1, docpath, event)

    transition(root1, docpath)
  end

  def identity?(node : Term)
    Term.case(node) do
      matchpi %{(cell _ @cout_ _*)} { Term.of(:cell, cout) }
      matchpi %{(transform @pin_ to @_ with _ _ ¦ () job_)} { Term.of(:transform, pin, job) }

      otherwise {}
    end
  end

  def transition(root0 : Term, docpath : Term::Dict)
    document0 = follow(root0, docpath)

    population0 = document0[:"(population)"]? || Term[]
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

    document1 = follow(root1, docpath).morph({:"(population)", population1})

    assign(root1, docpath, document1)
  end

  def publish(root : Term::Dict)
    if results = root[:"(results)"]?
      results.each_entry do |job, value|
        root = exeq(root, enqueue(:"job/completed", job, value))
      end
      root = root.without(:"(results)")
    end

    root
  end

  def unchanged?(root0 : Term::Dict, root1 : Term::Dict) : Bool
    return false unless pipe(root0, queue, Q.empty?)
    return false unless pipe(root1, queue, Q.empty?)

    # We remove Queue because two empty Queues may contain different hi/lo,
    # messing up the raw equality check that we're doing here.
    root0.without(Queue) == root1.without(Queue)
  end

  def run(root0 : Term::Dict, & : Term::Dict -> Term::Dict)
    root0 = transition(Term.of(root0), docpath: Term[]).as_d

    while true
      root0 = yield root0
      root0 = publish(root0)
      root1 = advance(Term.of(root0), Term.of(root0), docpath: Term[]).as_d

      if unchanged?(root0, root1)
        return root1
      end

      root0 = root1
    end
  end
end

# test cell invited/expelled, changes node, absence node
root = ML.terms(<<-WWML
(decay 10 (cell 0 @count))
(delay 15 (cell "Hello World" @count))
(changes @count to @counts)
(absence @count as "Missing" to @counts)
(log @counts in ())
WWML
).as_d

# Test button long form raw value
root = ML.terms(<<-WWML
(button "Increment" as +1 to @deltas ((press) (press) (press)))
(log @deltas in ())
WWML
).as_d

# Test button short form raw value
root = ML.terms(<<-WWML
(button "Increment" to @deltas ((press) (press) (press)))
(log @deltas in ())
WWML
).as_d

# Test button long form edge value
root = ML.terms(<<-WWML
(cell "Hello World" @msg)
(button "Do it" as @msg to @deltas ((press) (press) (press)))
(log @deltas in ())
WWML
).as_d

# Test button short form edge value
root = ML.terms(<<-WWML
(cell "Hello World" @msg)
(button @msg to @deltas ((press) (press) (press)))
(log @deltas in ())
WWML
).as_d

# Test latest
root = ML.terms(<<-WWML
(cell "Hello World" @msg)
(button "Change msg" as "Changed!" to @msgs ((press)))
(latest @msgs @msg)
(changes @msg to @msg-changes)
(log @msg-changes in ())
WWML
).as_d

# Test log to cell

root = ML.terms(<<-WWML
(cell () @log)
(button "Hello World" to @msgs ((press) (press) (press)))
(log @msgs in @log)
WWML
).as_d

# Basic stateful transform

root = ML.terms(<<-WWML
(cell 0 @count)
(button "Increment" as +1 to @deltas ((press)))
(transform @deltas to @counts with @count (+ 1 2))
(latest @counts @count)
WWML
).as_d

# Counter single step

root = ML.terms(<<-WWML
(cell 5 @count)
(button "Increment" as +1 to @deltas ((press)))
(button "Decrement" as -1 to @deltas ())
(transform @deltas to @counts with @count (+ state _))
(latest @counts @count)
WWML
).as_d

root = ML.terms(<<-WWML
(cell 5 @count)
(button "Increment" as +1 to @deltas ())
(button "Decrement" as -1 to @deltas ((press)))
(transform @deltas to @counts with @count (+ state _))
(latest @counts @count)
WWML
).as_d

# Counter multi step, testing how button reacts to transform feedback

root = ML.terms(<<-WWML
(cell 5 @count)
(button "Increment" as +1 to @deltas ((press) (press) (press)))
(button "Decrement" as -1 to @deltas ())
(transform @deltas to @counts with @count (+ state _))
(latest @counts @count)
WWML
).as_d

# Test cancellation (NOTE: this depends on how fast we handle jobs, modify for predictability!)
root = ML.terms(<<-WWML
(cell 5 @count)
(button "Increment" as +1 to @deltas ((press) (press) (press) (press) (press)))
(decay 1 (transform @deltas to @counts with @count (+ state _)))
(latest @counts @count)
WWML
).as_d

# Button listens to multiple feedbacks
root = ML.terms(<<-WWML
(cell 0 @count)

(button "Increment" as +1 to @deltas ((press) (press) (press) (press) (press)))

;; This one should cancel
(decay 1 (transform @deltas to @counts with @count (+ state _)))

;; This one should go on, a "Backup" of sorts
(decay 5 (transform @deltas to @counts with @count (- state _)))

(latest @counts @count)
WWML
).as_d

# Test transforms not eating each other if cancelled, same tasks
root = ML.terms(<<-WWML
(cell 0 @count)

(button "Increment" as +1 to @deltas ((press) (press) (press) (press) (press)))

;; This one should cancel
(decay 1 (transform @deltas to @counts with @count (+ state _)))

;; This one should go on, a "Backup" of sorts
(decay 5 (transform @deltas to @counts with @count (+ state _)))

(latest @counts @count)
WWML
).as_d

# Log limit
root = ML.terms(<<-WWML
(cell 10 @count for (%number (whole _) > 0))

;; This is a feedback loop
(changes @count to @deltas as -1)
(transform @deltas to @counts with @count (+ state _))
(latest @counts @count)

;; Observe (scoping is an issue right now)
(changes @count to @counts/1)
(log @counts/1 in () limit: 5)
WWML
).as_d


running0 = Set(Term).new
results = Deque({Term, Term}).new
lock = Mutex.new

class JobInterrupted < Exception
end

ctx = ExecutionContext::MultiThreaded.new("mt", 4)
primitives = ProcRuleset.build do
  rulepi1 %[(+ a_number b_number)] { a + b }
end

while true
  root = D.run(root) do |im|
    puts ML.display(im, maxwidth: 80)
    sleep 100.milliseconds

    # Schedule/un-schedule jobs

    running1 = Set(Term).new

    (im[:"(jobs)"]? || Term[]).each_entry do |job, _|
      running1 << job
    end

    im = im.without(:"(jobs)")

    lock.synchronize do
      (running1 - running0).each do |job|
        ctx.spawn do
          # # Artificial delay
          # chan = Channel(Nil).new
          # ctx.spawn do
          #   sleep 3.seconds
          #   chan.send(nil)
          # end
          # chan.receive
          result = rewrite(job[:program], chainR(using(job[:env].as_d, dfsR(envR)), callR(primitives))) do
            unless lock.synchronize { job.in?(running0) }
              raise JobInterrupted.new
            end
          end

          lock.synchronize do
            results << {job, result}
          end
        rescue JobInterrupted
          puts "Job interrupted"
        end
      end

      running0.concat(running1)

      # Import job results
      results.each do |job, value|
        im = im.morph({:"(results)", job, value})
        running0.delete(job)
      end
      results.clear
    end

    im
  end

  break if lock.synchronize { running0.empty? }
end

puts ML.display(root, maxwidth: 120)
# kp = Term[]
# while succ = D.successor?(root, kp)
#   pp kp
#   kp = succ
# end
# pp kp
