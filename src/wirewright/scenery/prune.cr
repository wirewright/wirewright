module Ww::Scenery
  private def prune?(command : DrawImage | DrawSvg | DrawGlyph | DrawRect | DrawRoundedRect | DrawRoundedRectFrame, tf : Tf, view : Rect)
    return unless tf.map(command.bounds).intersects?(view)

    command
  end

  private def prune?(command : DrawSeq, tf : Tf, view : Rect) : DrawSeq?
    return unless tf.map(command.bounds).intersects?(view)

    removed_indices = Pf::USet32.new
    changed_indices = Pf::USet32.new
    changed_commands = Pf::Kit.stack_array(DrawCommand)

    command.children.each_with_index do |child, index|
      uindex = index.to_u32

      unless pruned_child = prune?(child, tf, view)
        removed_indices = removed_indices.add(uindex)
        next
      end

      next if child.same?(pruned_child)

      changed_indices = changed_indices.add(uindex)
      changed_commands << pruned_child
    end

    # Nothing changed. The entire subtree is visible.
    if changed_indices.empty? && removed_indices.empty?
      return command
    end

    # Everything was removed. The entire subtree is invisible.
    return if command.children.size == removed_indices.size

    bounds = Rect.empty
    commands = Pointer(DrawCommand).malloc(command.children.size - removed_indices.size)
    commands_size = 0
    changed_cursor = 0

    command.children.each_with_index do |child, index|
      uindex = index.to_u32
      next if uindex.in?(removed_indices)

      if uindex.in?(changed_indices)
        changed_command = changed_commands[changed_cursor]
        commands[commands_size] = changed_command
        changed_cursor += 1
        bounds = Rect.union(bounds, changed_command.bounds)
      else
        commands[commands_size] = child
        bounds = Rect.union(bounds, child.bounds)
      end

      commands_size += 1
    end

    DrawSeq.new(Slice(DrawCommand).new(commands, commands_size, read_only: true), bounds)
  end

  private def prune?(command, &) : DrawCommand?
    return unless pruned_child = yield command.child

    if command.child.same?(pruned_child)
      return command
    end

    command.copy_with(child: pruned_child)
  end

  private def prune?(command : DrawTransform, tf : Tf, view : Rect) : DrawTransform?
    prune?(command) { prune?(command.child, tf.append(command.tf), view) }
  end

  private def prune?(command : DrawOpacity, tf : Tf, view : Rect) : DrawOpacity?
    prune?(command) { prune?(command.child, tf, view) }
  end

  private def prune?(command : DrawClip, tf : Tf, view : Rect) : DrawClip?
    prune?(command) do
      # Map local bounds to global bounds (*view* is always in global coords,
      # and *tf* tells us how to go from local coords to global ones).
      subview = tf.map(command.visible.bounds)

      # Make sure that if view is smaller than subview, we pick that.
      subview = Rect.intersection(view, subview)

      prune?(command.child, tf, subview)
    end
  end

  # Removes draw commands whose output will be invisible according to *tf* and *view*.
  #
  # *view* is the toplevel view rect (e.g. window). It is possible to provide
  # an infinite rect.
  #
  # TODO: We're not a windowing system (i.e., it isn't as relevant to us here),
  # but at some point, we must properly handle occlusion.
  def prune(command : DrawCommand, view : Rect, tf : Tf = Tf.new) : DrawCommand
    prune?(command, tf, view) || DrawSeq.empty
  end
end
