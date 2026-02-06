# Guts of the `feed` node.
module Ww::Rack::Feed
  extend self

  alias Spec = WithoutInhibitors | WithInhibitors

  # Crystal-side representation of the Inhibitor modifier that all variants
  # of the feed node are allowed to have. For eaxmple `(feed (not @x @y) @u @v)`
  # for Transfer, or, say, `(feed (not @x @y) @u (@v @w))` for Broadcast.
  # These are known as "_ with inhibitors" variants: here, it is Transfer with
  # inhibitors, and Broadcast with inhibitors, correspondingly.
  defrecord WithInhibitors, node : Term, variant : Variant, inhibitors : Slice(Edge)
  defrecord WithoutInhibitors, node : Term, variant : Variant

  alias Variant = Transfer | Aggregate | Broadcast | ParallelTransfer

  # Crystal-side representation of the Transfer variant of the feed node,
  # for example `(feed @u @v)`.
  defrecord Transfer, from : SrcEdge, to : DstEdge

  # Crystal-side representation of the Aggregate variant of the feed node,
  # for example `(feed (@u @v) @w)`.
  defrecord Aggregate, from : Slice(SrcEdge), to : DstEdge

  # Crystal-side representation of the Broadcast variant of the feed node,
  # for example `(feed @u (@v @w))`.
  defrecord Broadcast, from : SrcEdge, to : Slice(DstEdge)

  # Crystal-side representation of the Parallel Transfer variant of
  # the feed node, for example `(feed (@x @y) (@a @b))`.
  defrecord ParallelTransfer, from : Slice(SrcEdge), to : Slice(DstEdge)

  # Crystal-side representation of the source edge, for example `(feed ⏏@x⏏ @y)`,
  # `(feed ⏏(copy (@x front))⏏ @y)` and so on.
  alias SrcEdge = EdgeObject | Copy

  # Represents `(copy _)` as in `(feed ⏏(copy @x)⏏ @y)`
  defrecord Copy, object : EdgeObject

  alias DstEdge = EdgeObject | Atop

  # Represents `(atop _)` as in `(feed @x ⏏(atop @y)⏏)`
  defrecord Atop, object : EdgeObject

  alias EdgeObject = Edge | Front | Back

  # Represesents `@_` as in `(feed ⏏@x⏏ (atop ⏏@y⏏))`.
  defrecord Edge, term : Term do
    {% unless flag?(:release) %}
      assert Term.edge?(term)
    {% end %}
  end

  # Represents `(@_ front)` as in `(feed (copy ⏏(@x front)⏏) @y)`.
  defrecord Front, list_edge : Edge

  # Represents `(@_ back)` as in `(feed (copy ⏏(@x back)⏏) @y)`.
  defrecord Back, list_edge : Edge

  # <edge object>
  #   @_
  #   (@_ front)
  #   (@_ back)
  private def edge_object?(term : Term) : EdgeObject?
    Term.case(term) do
      matchpi %{(@u_ front)} { Front.new(Edge.new(u)) }
      matchpi %{(@u_ back)} { Back.new(Edge.new(u)) }
      matchpi %{@u_} { Edge.new(u) }
      otherwise { }
    end
  end

  # <src edge>
  #   <edge object>
  #   (copy <src edge object>)
  private def src_edge?(term : Term) : SrcEdge?
    Term.case(term) do
      matchpi %{(copy arg_)} do
        return unless edge_object = edge_object?(arg)

        Copy.new(edge_object)
      end

      otherwise do
        edge_object?(term)
      end
    end
  end

  # <src edge list>
  #   (<src edge>+)
  private def src_edge_list?(term : Term) : Slice(SrcEdge)?
    return unless dict = term.as_itemsonly_d?
    return if dict.empty?

    edges = Pf::Kit.stack_array(SrcEdge)
    dict.items.each do |item|
      return unless src_edge = src_edge?(item)

      edges << src_edge
    end

    edges.to_readonly_slice(&.itself)
  end

  # <dst edge>
  #   <edge object>
  #   (atop <edge object>)
  private def dst_edge?(term : Term) : DstEdge?
    Term.case(term) do
      matchpi %{(atop arg_)} do
        return unless edge_object = edge_object?(arg)

        Atop.new(edge_object)
      end

      otherwise do
        edge_object?(term)
      end
    end
  end

  # <src edge list>
  #   (<dst edge>+)
  private def dst_edge_list?(term : Term) : Slice(DstEdge)?
    return unless dict = term.as_itemsonly_d?
    return if dict.empty?

    edges = Pf::Kit.stack_array(DstEdge)
    dict.items.each do |item|
      return unless dst_edge = dst_edge?(item)

      edges << dst_edge
    end

    edges.to_readonly_slice(&.itself)
  end

  # <variant>
  #   [feed <src> <dst>]
  #
  # <srC>
  #   <src edge>
  #   <src edge list>
  #
  # <dst>
  #   <dst edge>
  #   <dst edge list>
  private def variant?(term : Term) : Variant?
    Term.matchpi?(term, %{[feed src_ dst_]}) do
      src_edge = src_edge?(src)
      dst_edge = dst_edge?(dst)

      if src_edge && dst_edge
        # (feed @u @v)
        return Transfer.new(src_edge, dst_edge)
      end

      src_edge_list = src_edge_list?(src)

      if src_edge_list && dst_edge
        # (feed (@u @v) @w)
        return Aggregate.new(src_edge_list, dst_edge)
      end

      dst_edge_list = dst_edge_list?(dst)

      if src_edge && dst_edge_list
        # (feed @u (@v @w))
        return Broadcast.new(src_edge, dst_edge_list)
      end

      if src_edge_list && dst_edge_list
        # (feed (@x @y) (@a @b))
        return ParallelTransfer.new(src_edge_list, dst_edge_list)
      end
    end
  end

  # Tries to parse *term* as a feed node `Spec`. Returns the spec if successful.
  def spec?(term : Term) : Spec?
    Term.case(term) do
      matchpi %{[feed (not (%group inhibitors_ (%past @_ min: 1))) rest_*]} do
        thunk = Term.of(rest.prepend(:feed))
        return unless variant = variant?(thunk)

        WithInhibitors.new(thunk, variant,
          inhibitors: inhibitors.items.to_readonly_slice { |edge| Edge.new(edge) },
        )
      end

      matchpi %{[feed _*]} do
        return unless variant = variant?(term)

        WithoutInhibitors.new(term, variant)
      end

      otherwise { }
    end
  end

  {% if flag?(:docs) %}
    # Yields edges found in the given *spec*.
    def each_edge(spec : Spec, & : Edge ->) : Nil
    end
  {% end %}

  # :nodoc:
  def each_edge(spec : WithoutInhibitors, & : Edge ->) : Nil
    each_edge(spec.variant) { |edge| yield edge }
  end

  # :nodoc:
  def each_edge(spec : WithInhibitors, & : Edge ->) : Nil
    each_edge(spec.variant) { |edge| yield edge }

    spec.inhibitors.each do |inhibitor|
      yield inhibitor
    end
  end

  # :nodoc:
  def each_edge(spec : Variant, & : Edge ->) : Nil
    each_edge(spec.from) { |edge| yield edge }
    each_edge(spec.to) { |edge| yield edge }
  end

  # :nodoc:
  def each_edge(spec : Slice(SrcEdge) | Slice(DstEdge), & : Edge ->) : Nil
    spec.each do |item|
      each_edge(item) { |edge| yield edge }
    end
  end

  # :nodoc:
  def each_edge(spec : Copy | Atop, & : Edge ->) : Nil
    each_edge(spec.object) { |edge| yield edge }
  end

  # :nodoc:
  def each_edge(spec : Front | Back, & : Edge ->) : Nil
    yield spec.list_edge
  end

  # :nodoc:
  def each_edge(spec : Edge, & : Edge ->) : Nil
    yield spec
  end

  # Returns a readonly slice of edge terms found in the given *spec*.
  def edges(spec) : Slice(Term)
    edges = Pf::Kit.stack_array(Term)
    each_edge(spec) do |edge|
      edges << edge.term
    end
    edges.to_readonly_slice(&.itself)
  end

  # Returns the first edge term in *spec*.
  def edge(spec) : Term
    each_edge(spec) { |edge| return edge.term }
  end

  {% if flag?(:docs) %}
    # We *render* to represent different kinds of feed nodes in a single
    # way, so that the rewrite regime can reason about them without being
    # aware of all the diversity.
    #
    # The renderout looks like this:
    #
    # ```text
    # (feed
    #   (<list of inhibitor edges>)
    #   (<list of input edges>)
    #   (<list of output edges>)
    #  <original node>)
    # ```
    #
    # Notice how it presents the edges in a flat way, which is friendly
    # toward the rewrite regime.
    #
    # The representation above is how the rewrite regime sees `feed` nodes.
    # When the feed rule fires, we recover *spec* by parsing `<original node>`
    # again, and pass it to `get?`, `put?`, or the high-level `patch?` along
    # with matched nodes for variant-specific interpretation.
    def render(spec : Spec) : Term
    end
  {% end %}

  # :nodoc:
  def render(spec : WithInhibitors) : Term
    render(spec.node, spec.variant, spec.inhibitors)
  end

  # :nodoc:
  def render(spec : WithoutInhibitors) : Term
    render(spec.node, spec.variant, inhibitors: Slice(Edge).empty)
  end

  # :nodoc:
  def render(node : Term, spec : Variant, inhibitors : Slice(Edge)) : Term
    inputs = Pf::Kit.stack_array(Term)
    outputs = Pf::Kit.stack_array(Term)

    each_edge(spec.from) { |input| inputs << input.term }
    each_edge(spec.to) { |output| outputs << output.term }

    Term.of(:feed, inhibitors.map(&.term), inputs, outputs, node)
  end

  {% if flag?(:docs) %}
    # Removes a value from a source cell *src*. Returns the resulting
    # patch to *src*, and the value removed. Returns `nil` if *src* is
    # not a cell, or if it the cell is empty.
    def take?(spec : EdgeObject, src : D7::Node) : {D7::Patch, Term}?
    end
  {% end %}

  # :nodoc:
  def take?(spec : Edge, src : D7::Node) : {D7::Patch, Term}?
    Term.matchpi?(src.term, %{[cell @_ value_]}) do
      {D7.patch(src, {2, nil}), value}
    end
  end

  # :nodoc:
  def take?(spec : Front, src : D7::Node) : {D7::Patch, Term}?
    Term.matchpi?(src.term, %{[cell @_ values←[value_ _*]]}) do
      {D7.patch(src, {2, values.without_item(0)}), value}
    end
  end

  # :nodoc:
  def take?(spec : Back, src : D7::Node) : {D7::Patch, Term}?
    Term.matchpi?(src.term, %{[cell @_ values←[_* value_]]}) do
      {D7.patch(src, {2, values.without(values.itemsize - 1)}), value}
    end
  end

  {% if flag?(:docs) %}
    # Copies a value from a source cell *src*. Returns the resulting
    # patch to *src*, for consistency; it is guaranteed to be empty.
    # Returns also the value copied. Returns `nil` if *src* is not a cell,
    # or if it the cell is empty.
    def copy?(spec : EdgeObject, src : D7::Node) : {D7::Patch, Term}?
    end
  {% end %}

  # :nodoc:
  def copy?(spec : Edge, src : D7::Node) : {D7::Patch, Term}?
    Term.matchpi?(src.term, %{[cell @_ value_]}) do
      {D7::Patch.new, value}
    end
  end

  # :nodoc:
  def copy?(spec : Front, src : D7::Node) : {D7::Patch, Term}?
    Term.matchpi?(src.term, %{[cell @_ values←[value_ _*]]}) do
      {D7::Patch.new, value}
    end
  end

  # :nodoc:
  def copy?(spec : Back, src : D7::Node) : {D7::Patch, Term}?
    Term.matchpi?(src.term, %{[cell @_ values←[_* value_]]}) do
      {D7::Patch.new, value}
    end
  end

  {% if flag?(:docs) %}
    # Inserts *value* into an empty edge object. Notably, list front
    # and list back objects are considered empty, and result in the placement
    # before the frontmost or backmost item (if any; correspondingly).
    def place?(spec : EdgeObject, dst : D7::Node, value : Term) : D7::Patch?
    end
  {% end %}

  # :nodoc:
  def place?(spec : Edge, dst : D7::Node, value : Term) : D7::Patch?
    Term.matchpi?(dst.term, %{[cell @_]}) do
      D7.patch(dst, {2, value})
    end
  end

  # :nodoc:
  def place?(spec : Front, dst : D7::Node, value : Term) : D7::Patch?
    Term.matchpi?(dst.term, %{[cell @_ values←[_*]]}) do
      D7.patch(dst, {2, values.prepend(value)})
    end
  end

  # :nodoc:
  def place?(spec : Back, dst : D7::Node, value : Term) : D7::Patch?
    Term.matchpi?(dst.term, %{[cell @_ values←[_*]]}) do
      D7.patch(dst, {2, values.append(value)})
    end
  end

  {% if flag?(:docs) %}
    # Replaces the value specified by *dst* and *spec* with *value*. List
    # front and list back refer to the first and last elements of a list; if
    # absent, or if the cell that is supposed to hold the list is empty,
    # no action is taken.
    #
    # Returns the resulting patch to *dst* if successful.
    def blend?(spec : EdgeObject, dst : D7::Node, value : Term) : D7::Patch?
    end
  {% end %}

  # :nodoc:
  def blend?(spec : Edge, dst : D7::Node, value : Term) : D7::Patch?
    Term.matchpi?(dst.term, %{[cell @_ _?]}) do
      D7.patch(dst, {2, value})
    end
  end

  # :nodoc:
  def blend?(spec : Front, dst : D7::Node, value : Term) : D7::Patch?
    Term.matchpi?(dst.term, %{[cell @_ values←[_ _*]]}) do
      D7.patch(dst, {2, 0, value})
    end
  end

  # :nodoc:
  def blend?(spec : Back, dst : D7::Node, value : Term) : D7::Patch?
    Term.matchpi?(dst.term, %{[cell @_ values←[_* _]]}) do
      D7.patch(dst, {2, values.itemsize - 1, value})
    end
  end

  {% if flag?(:docs) %}
    # Takes or copies the value defined by *spec* and *src*. If successful,
    # returns the patch to *src* and the value retrieved.
    def get?(spec : SrcEdge, src : D7::Node) : {D7::Patch, Term}?
    end
  {% end %}

  # :nodoc:
  def get?(spec : Copy, src : D7::Node)
    copy?(spec.object, src)
  end

  # :nodoc:
  def get?(spec : EdgeObject, src : D7::Node)
    take?(spec, src)
  end

  {% if flag?(:docs) %}
    # Places or blends *value* into the spot defined by *spec* and *dst*.
    # If successful, returns the patch to *dst*.
    def put?(spec : DstEdge, dst : D7::Node, value : Term) : {D7::Patch, Term}?
    end
  {% end %}

  # :nodoc:
  def put?(spec : Atop, src : D7::Node, value : Term)
    blend?(spec.object, src, value)
  end

  # :nodoc:
  def put?(spec : EdgeObject, src : D7::Node, value : Term)
    place?(spec, src, value)
  end

  {% if flag?(:docs) %}
    # *src* must match cells whose edge is captured under `src`.
    # *dst* must match cells whose edge is captured under `dst`.
    def patch?(spec : Variant, src : D7::MatchGroup, dst : D7::MatchGroup) : D7::Patch?
    end
  {% end %}

  # :nodoc:
  def patch?(spec : Transfer, src : D7::MatchGroup, dst : D7::MatchGroup) : D7::Patch?
    assert src.size == 1
    assert dst.size == 1
    return unless get_response = get?(spec.from, src.first.node)

    src_patch, value = get_response
    return unless dst_patch = put?(spec.to, dst.first.node, value)

    D7.patches(src_patch, dst_patch)
  end

  # :nodoc:
  def patch?(spec : Aggregate, src : D7::MatchGroup, dst : D7::MatchGroup) : D7::Patch?
    return unless spec.from.size == src.size

    assert dst.size == 1

    patches = Pf::Kit.stack_array(D7::Patch, 4)
    values = Pf::Kit.stack_array(Term, 4)

    permutation = D7.permutation(src, :src, goal: edges(spec.from))
    permutation.each do |index|
      src_spec = spec.from[index]
      src_match = src[index]
      next unless get_response = get?(src_spec, src_match.node)

      src_patch, src_value = get_response
      patches << src_patch
      values << src_value
    end

    return unless patches.size == src.size # get?() must succeed for all nodes

    assert patches.size == values.size

    return unless dst_patch = put?(spec.to, dst.first.node, Term.of(values))

    patches << dst_patch

    D7.patches(patches)
  end

  # :nodoc:
  def patch?(spec : Broadcast, src : D7::MatchGroup, dst : D7::MatchGroup) : D7::Patch?
    return unless spec.to.size == dst.size

    assert src.size == 1

    return unless get_response = get?(spec.from, src.first.node)

    src_patch, value = get_response
    return unless dict = value.as_d?
    return unless dict.itemsize == spec.to.size

    patches = Pf::Kit.stack_array(D7::Patch, 4)

    permutation = D7.permutation(dst, :dst, goal: edges(spec.to))
    permutation.each do |index|
      dst_spec = spec.to[index]
      dst_match = dst[index]
      dst_item = dict[index]
      next unless dst_patch = put?(dst_spec, dst_match.node, dst_item)

      patches << dst_patch
    end

    return unless patches.size == spec.to.size # put?() must succeed for all nodes

    patches << src_patch

    D7.patches(patches)
  end

  # :nodoc:
  def patch?(spec : ParallelTransfer, src : D7::MatchGroup, dst : D7::MatchGroup) : D7::Patch?
    return unless spec.from.size == src.size
    return unless spec.to.size == dst.size

    patches = Pf::Kit.stack_array(D7::Patch, 8)
    values = Pf::Kit.stack_array(Term, 8)

    src_permutation = D7.permutation(src, :src, goal: edges(spec.from))
    src_permutation.each do |index|
      src_spec = spec.from[index]
      src_match = src[index]
      next unless get_response = get?(src_spec, src_match.node)

      patch, value = get_response
      patches << patch
      values << value
    end

    return unless patches.size == src.size # get?() must succeed for all nodes

    dst_permutation = D7.permutation(dst, :dst, goal: edges(spec.to))
    dst_permutation.each do |index|
      dst_spec = spec.to[index]
      dst_match = dst[index]
      dst_value = values[index]
      next unless patch = put?(dst_spec, dst_match.node, dst_value)

      patches << patch
    end

    return unless patches.size == src.size + dst.size # put?() must succeed for all nodes

    D7.patches(patches)
  end
end
