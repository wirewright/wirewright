# Implements the `supervisor` node.
module Ww::Rack::Supervisor
  extend self

  def step(parser : D7::Parser, circuit : Term, prepass) : Slice(Term)
    D7.step(parser, circuit) do |hg, proposals0|
      prepass.call(hg, proposals0) do |hg, proposals1|
        step(&.call(hg, proposals1))
      end
    end
  end

  def step(& : Propose -> T) : T forall T
    propose = Propose.new do |hg, proposals|
      propose(hg, proposals)
    end
    yield propose
  end

  alias Variant = DeviceVariant | ModuleVariant

  defcase DeviceVariant,
    abs_tasks : D7::AbsEdge,
    ref_edge : Term,
    task_edge : Term,
    pattern : Term,
    abs_pool : D7::AbsEdge,
    template : Term

  defcase ModuleVariant,
    abs_tasks : D7::AbsEdge,
    ref_edge : Term,
    task_edge : Term,
    pattern : Term,
    abs_pool : D7::AbsEdge,
    bindings : Term::Dict,
    template : Term

  private def propose(hg : D7::Hypergraph, proposals)
    hg.propose(proposals, :supervisor) do |node|
      Term.case(node.term) do
        matchpi %{[supervisor (@tasks_ @task_ pattern_ - @pool_) template_*]} do
          abs_tasks = hg.resolve(node.addr, tasks)
          abs_pool = hg.resolve(node.addr, pool)
          ref_edge = Term.of(:edge, :reference)
          variant = stack_alloc DeviceVariant.new(abs_tasks, ref_edge, task, pattern, abs_pool, template)
          step(hg, node, variant)
        end

        matchpiT %{[supervisor (@tasks_ @task_ pattern_ - @pool_ bindings_dict) template_*]} do
          abs_tasks = hg.resolve(node.addr, tasks)
          abs_pool = hg.resolve(node.addr, pool)
          ref_edge = Term.of(:edge, :reference)
          variant = stack_alloc ModuleVariant.new(abs_tasks, ref_edge, task, pattern, abs_pool, bindings, template)
          step(hg, node, variant)
        end

        otherwise { }
      end
    end
  end

  alias Correction = AddWorker | DeleteWorker | UpdateWorker | UpdateTask | DeleteTask

  defrecord AddWorker, key : Term, task : Term
  defrecord DeleteWorker, index : Int32
  defrecord UpdateWorker, index : Int32, task : Term
  defrecord UpdateTask, index : Int32, task : Term
  defrecord DeleteTask, index : Int32

  defrecord ManagedTask, index : Int32, term : Term

  defrecord ManagedTaskList,
    cell : Cell,
    map : Hash(Term, ManagedTask),
    conflicting_keys : Set(Term),
    steer : Array(Correction) -> D7::Patch

  private def task_list?(hg : D7::Hypergraph, edge : D7::AbsEdge, variant : Variant) : ManagedTaskList?
    return unless cell = Rack.cell?(hg, edge)
    return unless list0 = cell.value?.as_d?

    map = {} of Term => ManagedTask
    conflicting_keys = Set(Term).new

    list0.items.each_with_index do |item, index|
      next unless key = key?(variant.pattern, item)
      next if key.in?(conflicting_keys)

      if map.delete(key)
        conflicting_keys << key
        next
      end

      map[key] = ManagedTask.new(index, item)
    end

    steer = ->(corrections : Array(Correction)) do
      updated = {} of Int32 => Term
      removed = Set(Int32).new

      corrections.each do |correction|
        case correction
        when UpdateTask then updated[correction.index] = correction.task
        when DeleteTask then removed << correction.index
        end
      end

      list1 = list0.pairspart.transaction do |commit|
        list0.items.each_with_index do |item, index|
          next if index.in?(removed)

          commit << (updated[index]? || item)
        end
      end

      D7.patch(cell.node, {2, list1})
    end

    ManagedTaskList.new(cell, map, conflicting_keys, steer)
  end

  defrecord Worker,
    key : Term,
    index : Int32,
    reference : Term,
    task : Term?,
    update : Term -> Term,
    smart: true

  defrecord WorkerPool,
    pool : Pool,
    map : Hash(Term, Worker),
    conflicting_keys : Set(Term),
    steer : Array(Correction) -> D7::Patch

  # TODO: the code here is pretty bad, maybe there's a way to refactor it?
  private def managed_workers(pool : Pool, variant : Variant, tasks : ManagedTaskList) : WorkerPool
    map = {} of Term => Worker
    conflicting_keys = tasks.conflicting_keys.dup

    pool.contents.items.each_with_index do |item, index|
      next unless worker = worker?(variant, item, index)
      next if worker.key.in?(conflicting_keys)

      if map.delete(worker.key)
        conflicting_keys << worker.key
        next
      end

      map[worker.key] = worker
    end

    steer = ->(corrections : Array(Correction)) do
      added = [] of AddWorker
      removed = Set(Int32).new
      updated = {} of Int32 => Term

      corrections.each do |correction|
        case correction
        when AddWorker    then added << correction
        when DeleteWorker then removed << correction.index
        when UpdateWorker then updated[correction.index] = correction.task
        end
      end

      workers = [] of {Term, Term}

      # Handle worker deletions and updates.
      pool.contents.items.each_with_index do |item, index|
        # Skip workers that were removed.
        next if index.in?(removed)

        # Skip items that do not look like workers.
        next unless worker = worker?(variant, item, index)
        next if worker.key.in?(conflicting_keys)

        # Skip workers that were not updated.
        unless task = updated[index]?
          workers << {item, worker.key}
          next
        end

        # Perform the replacement and move on.
        workers << {worker.update.call(task), worker.key}
      end

      # Handle worker adds.
      added.each do |correction|
        worker = Term::Dict.build do |commit|
          case variant
          in DeviceVariant
            commit << :device
          in ModuleVariant
            commit << :module << variant.bindings
          end
          commit << {:cell, variant.ref_edge, correction.task}
          commit << {:cell, variant.task_edge, correction.task}
          commit.concat(variant.template.items)
        end
        workers << {Term.of(worker), correction.key}
      end

      # Sort by index in tasks so that the order of workers matches that of tasks.
      workers.sort_by! do |(worker, key)|
        tasks.map[key].index
      end

      contents1 = Term::Dict.build do |commit|
        # Handle worker deletions and updates.
        pool.contents.items.each_with_index do |item, index|
          # Skip workers that were removed.
          next if index.in?(removed)

          # Skip items that do not look like workers.
          unless worker = worker?(variant, item, index)
            commit << item
            next
          end

          # Delete workers with duplicate keys.
          next if worker.key.in?(conflicting_keys)

          replacement, _ = workers.shift
          commit << replacement
        end

        next if workers.empty?

        commit.concat(workers) do |(worker, _)|
          worker
        end
      end

      D7.patch(pool.node, {2, contents1})
    end

    WorkerPool.new(pool, map, conflicting_keys, steer)
  end

  # TODO: there's a lot of O(N) stuff here (where N is either @tasks size or @pool size),
  # maybe there are ways to fix that? Through caching, perhaps?
  private def step(hg : D7::Hypergraph, node : D7::Node, variant : Variant) : D7::Patch?
    return unless pool = Rack.pool?(hg, variant.abs_pool)

    # Empty the pool if the task cell is missing or malformed in any way.
    unless tasks = task_list?(hg, variant.abs_tasks, variant)
      return D7.patch(pool.node, {2, Term[]})
    end

    workers = managed_workers(pool, variant, tasks)

    corrections = [] of Correction

    # Emit corrections based on worker state.
    workers.map.each do |key, worker|
      next if key.in?(tasks.conflicting_keys)

      unless task = tasks.map[key]?
        corrections << DeleteWorker.new(worker.index)
        # Task is already absent so we don't have to remove it.
        next
      end

      # If the worker is missing or has an empty task cell (the latter is vastly more likely),
      # this means it wants to remove itself and the task.
      unless worker_task = worker.task?
        corrections << DeleteWorker.new(worker.index)
        corrections << DeleteTask.new(task.index)
        next
      end

      # If this worker's task did not change, but the @tasks' did, update
      # the worker to the @tasks' task.
      if worker.reference == worker_task
        unless worker.reference == task.term
          corrections << UpdateWorker.new(worker.index, task.term)
        end
        next
      end

      # If the task in @tasks did not change, prefer the worker's @task if
      # @task changed with respect to @reference.
      if worker.reference == task.term
        unless worker.reference == worker_task
          # We still have to update the worker's @reference so we have to UpdateWorker.
          corrections << UpdateWorker.new(worker.index, worker_task)
          corrections << UpdateTask.new(task.index, worker_task)
        end
        next
      end

      # If *both* the task in @tasks and the worker's own @task changed,
      # merge their changes with respect to @reference.

      patches = {
        D7::Patch.assoc(0u32, task.term),
        D7::Patch.assoc(0u32, worker_task),
      }

      merged_patch, _ = D7.merge(patches) do |id|
        assert id == 0u32

        {worker.reference, D7::MergeDiff.new(UInt32::MAX)}
      end

      # If there is an irresolvable conflict, prefer the task from @tasks,
      # since workers are ultimately dependent on @tasks.
      unless successor_task = merged_patch[0]?
        corrections << UpdateWorker.new(worker.index, task.term)
        next
      end

      corrections << UpdateWorker.new(worker.index, successor_task)
      corrections << UpdateTask.new(task.index, successor_task)
    end

    # Emit corrections based on task state.
    tasks.map.each do |key, task|
      next if workers.map.has_key?(key)

      corrections << AddWorker.new(key, task.term)
    end

    # Apply corrections to workers and tasks.
    D7.patches(
      tasks.steer.call(corrections),
      workers.steer.call(corrections)
    )
  end

  private def key?(pattern : Term, term : Term) : Term?
    return unless env = M1.match?(pattern, term)
    return unless env.size == 1

    _, key = env.ee.first
    key
  end

  private def cell?(edge : Term, child : Term) : Term?
    Term.matchpi?(child, %{[cell @candidate_ value_]}) do
      return unless candidate == edge
      return value
    end
  end

  private def worker?(variant : DeviceVariant, item : Term, index : Int32) : Worker?
    Term.matchpi?(item, %{[device _*]}) do
      children = item.items.move(1)
      next unless reference_row = children.leftmost_with_index? { |child| cell?(variant.ref_edge, child) }

      reference, reference_index = reference_row
      next unless key = key?(variant.pattern, reference)

      # The task cell can be missing, most often when it is empty. This is
      # used by workers to signify removal of their corresponding tasks, and
      # themselves from the pool.
      if task_row = children.leftmost_with_index? { |child| cell?(variant.task_edge, child) }
        task, task_index = task_row
      end

      update = ->(value1 : Term) do
        reference_key = reference_index + 1

        if task_index.nil?
          return Term.morph(item, {reference_key, 2, value1})
        end

        task_key = task_index + 1
        Term.morph(item, {reference_key, 2, value1}, {task_key, 2, value1})
      end

      Worker.new(key, index, reference, task, update)
    end
  end

  private def worker?(variant : ModuleVariant, item : Term, index : Int32) : Worker?
    Term.matchpi?(item, %{[module _dict _*]}) do
      children = item.items.move(2)
      next unless reference_row = children.leftmost_with_index? { |child| cell?(variant.ref_edge, child) }

      reference, reference_index = reference_row
      next unless key = key?(variant.pattern, reference)

      # The task cell can be missing, most often when it is empty. This is
      # used by workers to signify removal of their corresponding tasks, and
      # themselves from the pool.
      if task_row = children.leftmost_with_index? { |child| cell?(variant.task_edge, child) }
        task, task_index = task_row
      end

      update = ->(value1 : Term) do
        reference_key = reference_index + 2

        if task_index.nil?
          return Term.morph(item, {reference_key, 2, value1})
        end

        task_key = task_index + 2
        Term.morph(item, {reference_key, 2, value1}, {task_key, 2, value1})
      end

      Worker.new(key, index, reference, task, update)
    end
  end
end
