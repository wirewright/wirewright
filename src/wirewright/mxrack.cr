{% skip_file unless flag?(:mxrack) %}

# |@ mxrack
#
# |@summary
# Media extensions for Rack.
#
# |@block
# MxRack is short for **M**edia e**x**tensions for **Rack**. MxRack is a collection
# of nodes that seamlessly extend `rack` with the ability to manage windows, do interactive
# graphics using `microfold` and `scenery`, and so on. In the future, MxRack will also
# contain nodes for working with and synthesizing audio; and perhaps some other nodes.
#
# MxRack uses SDL3, and so is a rather heavy binary to distribute. We use AppImages
# to relieve you of most work of preparing your system for Wirewright. Rack, on
# the other hand, through `irack`, can be distributed as a static binary.
#
# I am telling you this to highlight the notable difference between the two, `mxrack` and
# `irack`. MxRack is relatively heavy and depends on the availability of SDL3, graphics,
# interactivity, and so on; and `irack` is lightweight, and -- at least in theory -- can
# be run on low-performance machines and servers, among other things. (I wouldn't
# recommend doing so right now, though, because even `irack` is probably too inefficient
# for small devices, especially in terms of memory usage. But the intended in-theory
# usage for both is as described.)
#
# The nodes listed here are specific to MxRack. Any Rack node is also an MxRack
# node. Go to `rack` to see docs for Rack nodes, which apply to MxRack too.

# Media extensions for Rack. See `mxrack` in the doctool.
module Ww::MxRack
  extend self

  alias MediaProgramQueue = BlockingQueue(MediaProgram)
  alias MediaProgram = WindowProgram

  FORCED_REMOTE_HEADS = Set{Term.of(:slot), Term.of(:rule), Term.of(:backmap)}

  # An implementation of a very simple three-way merge with MxRack-specific heuristics.
  #
  # EXPERIMENTAL: This is by no means a general three-way merge or diff, nor is it
  # expected to handle all edge cases properly. Use with care. The algorithm can be
  # considered "fuzzy" in the sense that its heuristic may misinterpret things.
  def merge3?(reference : Term, local : Term, remote : Term) : Term?
    # Fast paths.
    return remote if local == reference
    return local if remote == reference
    return local if local == remote

    # Heuristic: If both are dicts and their heads are different symbols,
    # prefer remote. E.g.:
    #
    #   L: (slot (X) 100)
    #   R: (group (slot (X)) (slot (X)))
    #
    # Merging conventionally makes very little sense here, and would produce:
    #
    #   M: (group (slot (X)) 100 (slot (X)))
    #
    # Instead, we prefer remote:
    #
    #   M: (group (slot (X)) (slot (X)))
    #
    # We still prefer a conventional merge if both local and remote agree on
    # the itemsize with the reference.
    #
    # For changes that look like slot changes, we prefer *remote*.
    pass do
      next unless {reference, local, remote}.all?(&.type.dict?)
      next unless head0 = local.items.first?
      next unless head1 = remote.items.first?
      next unless head0.type.symbol? && head1.type.symbol?

      if head0 == head1
        return remote if head0.in?(FORCED_REMOTE_HEADS)
        next
      end

      next if reference.itemsize == local.itemsize && reference.itemsize == remote.itemsize
      return remote
    end

    # Produce diffs.
    local_diff = Term.diff?(reference, local, depth_limit: 1u32)
    remote_diff = Term.diff?(reference, remote, depth_limit: 1u32)
    return unless local_diff && remote_diff

    table = {} of Term => Term | {Term, Term}

    local_diff = local_diff.compact_map do |action|
      assert action.prefix.empty?

      case action
      when Term::Diff::WithItem
        key = Term.of(action.index)
        value = action.item
      when Term::Diff::WithPair
        key = action.key
        value = action.value
      else
        next action
      end

      case current = table[key]?
      in Nil
        table[key] = value
      in Term
        table[key] = {current, value}
      in Tuple(Term, Term)
        raise KeyError.new
      end

      nil # omit
    end

    remote_diff = remote_diff.compact_map do |action|
      assert action.prefix.empty?

      case action
      when Term::Diff::WithItem
        key = Term.of(action.index)
        value = action.item
      when Term::Diff::WithPair
        key = action.key
        value = action.value
      else
        next action
      end

      case current = table[key]?
      in Nil
        table[key] = value
      in Term
        table[key] = {current, value}
      in Tuple(Term, Term)
        raise KeyError.new
      end

      nil # omit
    end

    reference1 = reference

    table.each do |key, value|
      if value.is_a?(Term)
        reference1 = reference1.with(key, value)
        next
      end

      # Merge recursively.
      l, r = value
      return unless merged = merge3?(reference[key], l, r)

      reference1 = reference1.with(key, merged)
    end

    # Apply the rest of mutations.
    mutation = Term::Diff::Mutation.new
    local_diff.each { |action| mutation << action }
    remote_diff.each { |action| mutation << action }

    Term.of(mutation.apply(reference1.as_d))
  end
end

require "./mxrack/adjust"
require "./mxrack/classifier"
require "./mxrack/input"
require "./mxrack/mouse"
require "./mxrack/window"
require "./mxrack/sdl_loop"
require "./mxrack/textoscope"
require "./mxrack/event"
require "./mxrack/theme"
require "./mxrack/automaton"
require "./mxrack/run"
