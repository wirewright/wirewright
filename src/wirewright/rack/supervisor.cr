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

  defcase StandardVariant,
    abs_tasks : D7::AbsEdge,
    ref_edge : Term,
    task_edge : Term,
    pattern : Term,
    abs_pool : D7::AbsEdge,
    template : Term

  private def propose(hg : D7::Hypergraph, proposals)
    hg.propose(proposals, :supervisor) do |node|
      Term.matchpi?(node.term, %{[supervisor (@tasks_ @task_ pattern_ - @pool_) template_*]}) do
        abs_tasks = hg.resolve(node.addr, tasks)
        abs_pool = hg.resolve(node.addr, pool)
        ref_edge = Term.of(:edge, :reference)
        variant = stack_alloc StandardVariant.new(abs_tasks, ref_edge, task, pattern, abs_pool, template)
        step(hg, node, variant)
      end
    end
  end

  alias Correction = AddDevice | DeleteDevice | UpdateDevice | UpdateTask | DeleteTask

  defrecord AddDevice, key : Term, task : Term
  defrecord DeleteDevice, index : Int32
  defrecord UpdateDevice, index : Int32, task : Term
  defrecord UpdateTask, index : Int32, task : Term
  defrecord DeleteTask, index : Int32

  defrecord ManagedTask, index : Int32, term : Term

  defrecord ManagedTaskList,
    cell : Cell,
    map : Hash(Term, ManagedTask),
    conflicting_keys : Set(Term),
    steer : Array(Correction) -> D7::Patch

  private def task_list?(hg : D7::Hypergraph, edge : D7::AbsEdge, variant : StandardVariant) : ManagedTaskList?
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

  defrecord ManagedDevice,
    key : Term,
    index : Int32,
    reference : Term,
    task : Term?,
    update : Term -> Term,
    smart: true

  defrecord ManagedDevicePool,
    pool : Pool,
    map : Hash(Term, ManagedDevice),
    conflicting_keys : Set(Term),
    steer : Array(Correction) -> D7::Patch

  # TODO: the code here is pretty bad, maybe there's a way to refactor it?
  private def managed_devices(pool : Pool, variant : StandardVariant, tasks : ManagedTaskList) : ManagedDevicePool
    map = {} of Term => ManagedDevice
    conflicting_keys = tasks.conflicting_keys.dup

    pool.contents.items.each_with_index do |item, index|
      next unless device = device?(variant, item, index)
      next if device.key.in?(conflicting_keys)

      if map.delete(device.key)
        conflicting_keys << device.key
        next
      end

      map[device.key] = device
    end

    steer = ->(corrections : Array(Correction)) do
      added = [] of AddDevice
      removed = Set(Int32).new
      updated = {} of Int32 => Term

      corrections.each do |correction|
        case correction
        when AddDevice    then added << correction
        when DeleteDevice then removed << correction.index
        when UpdateDevice then updated[correction.index] = correction.task
        end
      end

      devices = [] of {Term, Term}

      # Handle device deletions and updates.
      pool.contents.items.each_with_index do |item, index|
        # Skip devices that were removed.
        next if index.in?(removed)

        # Skip items that do not look like devices.
        next unless device = device?(variant, item, index)
        next if device.key.in?(conflicting_keys)

        # Skip devices that were not updated.
        unless task = updated[index]?
          devices << {item, device.key}
          next
        end

        # Perform the replacement and move on.
        devices << {device.update.call(task), device.key}
      end

      # Handle device adds.
      added.each do |correction|
        device = Term::Dict.build do |commit|
          commit << :device
          commit << {:cell, variant.ref_edge, correction.task}
          commit << {:cell, variant.task_edge, correction.task}
          commit.concat(variant.template.items)
        end
        devices << {Term.of(device), correction.key}
      end

      # Sort by index in tasks so that the order of devices matches that of tasks.
      devices.sort_by! do |(device, key)|
        tasks.map[key].index
      end

      contents1 = Term::Dict.build do |commit|
        # Handle device deletions and updates.
        pool.contents.items.each_with_index do |item, index|
          # Skip devices that were removed.
          next if index.in?(removed)

          # Skip items that do not look like devices.
          unless device = device?(variant, item, index)
            commit << item
            next
          end

          # Delete devices with duplicate keys.
          next if device.key.in?(conflicting_keys)

          replacement, _ = devices.shift
          commit << replacement
        end

        next if devices.empty?

        commit.concat(devices) do |(device, _)|
          device
        end
      end

      D7.patch(pool.node, {2, contents1})
    end

    ManagedDevicePool.new(pool, map, conflicting_keys, steer)
  end

  # TODO: there's a lot of O(N) stuff here (where N is either @tasks size or @pool size),
  # maybe there are ways to fix that? Through caching, perhaps?
  private def step(hg : D7::Hypergraph, node : D7::Node, variant : StandardVariant) : D7::Patch?
    return unless pool = Rack.pool?(hg, variant.abs_pool)

    # Empty the pool if the task cell is missing or malformed in any way.
    unless tasks = task_list?(hg, variant.abs_tasks, variant)
      return D7.patch(pool.node, {2, Term[]})
    end

    devices = managed_devices(pool, variant, tasks)

    corrections = [] of Correction

    # Emit corrections based on device state.
    devices.map.each do |key, device|
      next if key.in?(tasks.conflicting_keys)

      unless task = tasks.map[key]?
        corrections << DeleteDevice.new(device.index)
        # Task is already absent so we don't have to remove it.
        next
      end

      # If the device is missing or has an empty task cell (the latter is vastly more likely),
      # this means it wants to remove itself and the task.
      unless device_task = device.task?
        corrections << DeleteDevice.new(device.index)
        corrections << DeleteTask.new(task.index)
        next
      end

      # If this device's task did not change, but the @tasks' did, update
      # the device to the @tasks' task.
      if device.reference == device_task
        unless device.reference == task.term
          corrections << UpdateDevice.new(device.index, task.term)
        end
        next
      end

      # If the task in @tasks did not change, prefer the device's @task if
      # @task changed with respect to @reference.
      if device.reference == task.term
        unless device.reference == device_task
          # We still have to update the device's @reference so we have to UpdateDevice.
          corrections << UpdateDevice.new(device.index, device_task)
          corrections << UpdateTask.new(task.index, device_task)
        end
        next
      end

      # If *both* the task in @tasks and the device's own @task changed,
      # merge their changes with respect to @reference.

      patches = {
        D7::Patch.assoc(0u32, task.term),
        D7::Patch.assoc(0u32, device_task),
      }

      merged_patch, _ = D7.merge(patches) do |id|
        assert id == 0u32

        {device.reference, D7::MergeDiff.new(UInt32::MAX)}
      end

      # If there is an irresolvable conflict, prefer the task from @tasks,
      # since devices are ultimately dependent on @tasks.
      unless successor_task = merged_patch[0]?
        corrections << UpdateDevice.new(device.index, task.term)
        next
      end

      corrections << UpdateDevice.new(device.index, successor_task)
      corrections << UpdateTask.new(task.index, successor_task)
    end

    # Emit corrections based on task state.
    tasks.map.each do |key, task|
      next if devices.map.has_key?(key)

      corrections << AddDevice.new(key, task.term)
    end

    # Apply corrections to devices and tasks.
    D7.patches(
      tasks.steer.call(corrections),
      devices.steer.call(corrections)
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

  private def device?(variant : StandardVariant, item : Term, index : Int32) : ManagedDevice?
    Term.matchpi?(item, %{[device _*]}) do
      children = item.items.move(1)
      next unless reference_row = children.leftmost_with_index? { |child| cell?(variant.ref_edge, child) }

      reference, reference_index = reference_row
      next unless key = key?(variant.pattern, reference)

      # The task cell can be missing, most often when it is empty. This is
      # used by devices to signify removal of their corresponding tasks, and
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

      ManagedDevice.new(key, index, reference, task, update)
    end
  end
end
