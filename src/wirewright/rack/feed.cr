# Implementation of the feed node.
module Ww::Rack::Feed
  extend self

  alias Spec = WithoutInhibitors | WithInhibitors

  defrecord WithInhibitors, node : Term, variant : Variant, inhibitors : Slice(Place)
  defrecord WithoutInhibitors, node : Term, variant : Variant

  alias Variant = Transfer | Aggregate | Distribute | ParallelTransfer | Broadcast

  defrecord Transfer, from : Source, to : Destination
  defrecord Aggregate, from : Slice(Source), to : Destination
  defrecord Distribute, from : Source, to : Slice(Destination)

  # *from* and to are guaranteed *to* be of equal size.
  defrecord ParallelTransfer, from : Slice(Source), to : Slice(Destination) do
    assert from.size == to.size
  end

  defrecord Broadcast, from : Source, to : Slice(Destination)

  alias Source = Place | Copy

  # Represents `(copy _)` as in `(feed ⏏(copy @x)⏏ @y)`
  defrecord Copy, place : Place

  alias Destination = Place | Over

  # Represents `(over _)` as in `(feed @x ⏏(over @y)⏏)`
  defrecord Over, place : Place

  alias Place = Edge | Front | Back

  # Represesents `@_` as in `(feed ⏏@x⏏ (over ⏏@y⏏))`.
  defrecord Edge, term : Term do
    {% unless flag?(:release) %}
      assert Term.edge?(term)
    {% end %}
  end

  # Represents `(@_ front)` as in `(feed (copy ⏏(@x front)⏏) @y)`.
  defrecord Front, list_edge : Edge

  # Represents `(@_ back)` as in `(feed (copy ⏏(@x back)⏏) @y)`.
  defrecord Back, list_edge : Edge

  def edge(x : Edge | Front | Back | Copy | Over) : Term
    case x
    in Edge       then x.term
    in Front      then x.list_edge.term
    in Back       then x.list_edge.term
    in Copy, Over then edge(x.place)
    end
  end

  # <place>
  #   @_
  #   (@_ front)
  #   (@_ back)
  private def place?(term : Term) : Place?
    Term.case(term) do
      matchpi %{(@u_ front)} { Front.new(Edge.new(u)) }
      matchpi %{(@u_ back)} { Back.new(Edge.new(u)) }
      matchpi %{@u_} { Edge.new(u) }
      otherwise { }
    end
  end

  # <src edge>
  #   <place>
  #   (copy <place>)
  private def src_edge?(term : Term) : Source?
    Term.case(term) do
      matchpi %{(copy arg_)} do
        return unless place = place?(arg)

        Copy.new(place)
      end

      otherwise do
        place?(term)
      end
    end
  end

  # <src edge list>
  #   (<src edge>+)
  private def src_edge_list?(term : Term) : Slice(Source)?
    return unless dict = term.as_itemsonly_d?
    return if dict.empty?

    edges = Pf::Kit.stack_array(Source)
    dict.items.each do |item|
      return unless src_edge = src_edge?(item)

      edges << src_edge
    end

    edges.to_readonly_slice(&.itself)
  end

  # <dst edge>
  #   <place>
  #   (over <place>)
  private def dst_edge?(term : Term) : Destination?
    Term.case(term) do
      matchpi %{(over arg_)} do
        return unless place = place?(arg)

        Over.new(place)
      end

      otherwise do
        place?(term)
      end
    end
  end

  # <src edge list>
  #   (<dst edge>+)
  private def dst_edge_list?(term : Term) : Slice(Destination)?
    return unless dict = term.as_itemsonly_d?
    return if dict.empty?

    edges = Pf::Kit.stack_array(Destination)
    dict.items.each do |item|
      return unless dst_edge = dst_edge?(item)

      edges << dst_edge
    end

    edges.to_readonly_slice(&.itself)
  end

  # <variant>
  #   [feed <src> <dst>]
  #
  # <src>
  #   <src edge>
  #   <src edge list>
  #
  # <dst>
  #   <dst edge>
  #   <dst edge list>
  private def variant?(term : Term) : Variant?
    Term.case(term) do
      matchpi %{[feed (src_ items) dst_]} do
        return unless src_edge = src_edge?(src)
        return unless dst_edge_list = dst_edge_list?(dst)

        Distribute.new(src_edge, dst_edge_list)
      end

      matchpi %{[feed src_ dst_]} do
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

        if src_edge_list && dst_edge_list && src_edge_list.size == dst_edge_list.size
          # (feed (@x @y) (@a @b))
          return ParallelTransfer.new(src_edge_list, dst_edge_list)
        end
      end

      otherwise { }
    end
  end

  # Tries to parse *term* as a feed node `Spec`. Returns the spec if successful.
  def spec?(term : Term) : Spec?
    Term.case(term) do
      matchpi %{[feed not←(not _+) rest_*]} do
        thunk = Term.of(rest.prepend(:feed))
        return unless variant = variant?(thunk)

        inhibitors = Pf::Kit.stack_array(Place, 4)

        inhibitorsQ = not.items.move(1)
        inhibitorsQ.each do |inhibitorQ|
          return unless inhibitor = place?(inhibitorQ)

          inhibitors << inhibitor
        end

        WithInhibitors.new(thunk, variant, inhibitors.to_readonly_slice)
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
      each_edge(inhibitor) { |edge| yield edge }
    end
  end

  # :nodoc:
  def each_edge(spec : Variant, & : Edge ->) : Nil
    each_edge(spec.from) { |edge| yield edge }
    each_edge(spec.to) { |edge| yield edge }
  end

  # :nodoc:
  def each_edge(spec : Slice(Source) | Slice(Destination), & : Edge ->) : Nil
    spec.each do |item|
      each_edge(item) { |edge| yield edge }
    end
  end

  # :nodoc:
  def each_edge(spec : Copy | Over, & : Edge ->) : Nil
    each_edge(spec.place) { |edge| yield edge }
  end

  # :nodoc:
  def each_edge(spec : Front | Back, & : Edge ->) : Nil
    yield spec.list_edge
  end

  # :nodoc:
  def each_edge(spec : Edge, & : Edge ->) : Nil
    yield spec
  end

  # Returns a set of edges found in *spec*.
  def edges(spec) : Set(Term)
    edges = Pf::Kit.stack_array(Term)
    each_edge(spec) do |edge|
      edges << edge.term
    end
    edges.to_set
  end

  {% if flag?(:docs) %}
    # Removes a value from a source cell *src*. Returns the resulting
    # patch to *src*, and the value removed. Returns `nil` if *src* is
    # not a cell, or if it the cell is empty.
    def take?(spec : Place, cell : Cell) : {D7::Patch, Term}?
    end
  {% end %}

  # :nodoc:
  def take?(spec : Edge, cell : Cell) : {D7::Patch, Term}?
    return unless value = cell.value?

    {D7.patch(cell.node, {2, nil}), value}
  end

  # :nodoc:
  def take?(spec : Front, cell : Cell) : {D7::Patch, Term}?
    return unless dict = cell.value?.as_d?
    return unless first = dict.items.first?

    {D7.patch(cell.node, {2, dict.rest}), first}
  end

  # :nodoc:
  def take?(spec : Back, cell : Cell) : {D7::Patch, Term}?
    return unless dict = cell.value?.as_d?
    return unless last = dict.items.last?

    {D7.patch(cell.node, {2, dict.prior}), last}
  end

  {% if flag?(:docs) %}
    # Copies a value from a source cell *src*. Returns the resulting
    # patch to *src*, for consistency; it is guaranteed to be empty.
    # Returns also the value copied. Returns `nil` if *src* is not a cell,
    # or if it the cell is empty.
    def copy?(spec : Place, cell : Cell) : {D7::Patch, Term}?
    end
  {% end %}

  # :nodoc:
  def copy?(spec : Edge, cell : Cell) : {D7::Patch, Term}?
    return unless value = cell.value?

    {D7::Patch.new, value}
  end

  # :nodoc:
  def copy?(spec : Front, cell : Cell) : {D7::Patch, Term}?
    return unless dict = cell.value?.as_d?
    return unless first = dict.items.first?

    {D7::Patch.new, first}
  end

  # :nodoc:
  def copy?(spec : Back, cell : Cell) : {D7::Patch, Term}?
    return unless dict = cell.value?.as_d?
    return unless last = dict.items.last?

    {D7::Patch.new, last}
  end

  {% if flag?(:docs) %}
    # Inserts *value* into *cell* according to a place *spec*. Returns
    # the resulting patch to *cell*. Returns `nil` if *value* cannot be placed
    # (e.g. *cell* is already full).
    def insert?(spec : Place, cell : Cell, value : Term) : D7::Patch?
    end
  {% end %}

  # :nodoc:
  def insert?(spec : Edge, cell : Cell, value : Term) : D7::Patch?
    return unless cell.empty?

    D7.patch(cell.node, {2, value})
  end

  # :nodoc:
  def insert?(spec : Front, cell : Cell, value : Term) : D7::Patch?
    return unless dict = cell.value?.as_d?

    D7.patch(cell.node, {2, dict.prepend(value)})
  end

  # :nodoc:
  def insert?(spec : Back, cell : Cell, value : Term) : D7::Patch?
    return unless dict = cell.value?.as_d?

    D7.patch(cell.node, {2, dict.append(value)})
  end

  {% if flag?(:docs) %}
    # Replaces the value at *cell* with *value* according to *spec*.
    # Returns the resulting patch to *cell* if successful.
    def replace?(spec : Place, cell : Cell, value : Term) : D7::Patch?
    end
  {% end %}

  # :nodoc:
  def replace?(spec : Edge, cell : Cell, value : Term) : D7::Patch?
    D7.patch(cell.node, {2, value})
  end

  # :nodoc:
  def replace?(spec : Front, cell : Cell, value : Term) : D7::Patch?
    return unless dict = cell.value?.as_d?
    return unless dict.itemsize > 0

    D7.patch(cell.node, {2, 0, value})
  end

  # :nodoc:
  def replace?(spec : Back, cell : Cell, value : Term) : D7::Patch?
    return unless dict = cell.value?.as_d?
    return unless dict.itemsize > 0

    D7.patch(cell.node, {2, dict.itemsize - 1, value})
  end

  {% if flag?(:docs) %}
    # Takes or copies the value at *cell* according to *spec*. If successful,
    # returns the patch to *cell* along with the value.
    def get?(spec : Source, cell : Cell) : {D7::Patch, Term}?
    end
  {% end %}

  # :nodoc:
  def get?(spec : Copy, cell : Cell) : {D7::Patch, Term}?
    copy?(spec.place, cell)
  end

  # :nodoc:
  def get?(spec : Place, cell : Cell) : {D7::Patch, Term}?
    take?(spec, cell)
  end

  {% if flag?(:docs) %}
    # Inserts or replaces *value* at *cell* according to *spec*. If successful,
    # returns the patch to *cell*.
    def put?(spec : Destination, cell : Cell, value : Term) : D7::Patch?
    end
  {% end %}

  # :nodoc:
  def put?(spec : Over, cell : Cell, value : Term) : D7::Patch?
    replace?(spec.place, cell, value)
  end

  # :nodoc:
  def put?(spec : Place, cell : Cell, value : Term) : D7::Patch?
    insert?(spec, cell, value)
  end

  # (feed @x @y)  (feed (@xs front) @y)
  def step(hg : D7::Hypergraph, node : D7::Node, feed : Transfer) : D7::Patch?
    return unless src_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(feed.from)))
    return unless src_row = get?(feed.from, src_cell)

    src_patch, src_value = src_row
    return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(feed.to)))
    return unless dst_patch = put?(feed.to, dst_cell, src_value)

    D7.patches(src_patch, dst_patch)
  end

  # (feed (@x @y) @z)
  def step(hg : D7::Hypergraph, node : D7::Node, feed : Aggregate) : D7::Patch?
    return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(feed.to)))

    patches = Pf::Kit.stack_array(D7::Patch, 8)

    row = Term::Dict.build do |commit|
      feed.from.each do |src|
        return unless src_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(src)))
        return unless src_row = get?(src, src_cell)

        src_patch, src_value = src_row
        patches << src_patch
        commit << src_value
      end
    end

    return unless dst_patch = put?(feed.to, dst_cell, Term.of(row))

    patches << dst_patch

    D7.patches(patches)
  end

  # (feed (@x items) (@y @z))
  def step(hg : D7::Hypergraph, node : D7::Node, feed : Distribute) : D7::Patch?
    return unless src_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(feed.from)))
    return unless src_row = get?(feed.from, src_cell)

    src_patch, src_value = src_row
    return unless row = src_value.as_d?
    return unless row.itemsize == feed.to.size

    patches = Pf::Kit.stack_array(D7::Patch, 8)
    patches << src_patch

    row.items.zip(feed.to) do |value, dst|
      return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(dst)))
      return unless dst_patch = put?(dst, dst_cell, value)

      patches << dst_patch
    end

    D7.patches(patches)
  end

  # (feed (@x @y) (@a @b))
  def step(hg : D7::Hypergraph, node : D7::Node, feed : ParallelTransfer) : D7::Patch?
    patches = Pf::Kit.stack_array(D7::Patch, 8)

    feed.from.zip(feed.to) do |src, dst|
      return unless src_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(src)))
      return unless src_row = get?(src, src_cell)

      src_patch, src_value = src_row
      return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(dst)))
      return unless dst_patch = put?(dst, dst_cell, src_value)

      patches << src_patch
      patches << dst_patch
    end

    D7.patches(patches)
  end

  # (feed @x (@y @z))
  def step(hg : D7::Hypergraph, node : D7::Node, feed : Broadcast) : D7::Patch?
    return unless src_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(feed.from)))
    return unless src_row = get?(feed.from, src_cell)

    src_patch, src_value = src_row

    patches = Pf::Kit.stack_array(D7::Patch, 8)
    patches << src_patch

    feed.to.each do |dst|
      return unless dst_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(dst)))
      return unless dst_patch = put?(dst, dst_cell, src_value)

      patches << dst_patch
    end

    D7.patches(patches)
  end

  def step(hg : D7::Hypergraph, node : D7::Node, feed : WithoutInhibitors) : D7::Patch?
    step(hg, node, feed.variant)
  end

  def step(hg : D7::Hypergraph, node : D7::Node, feed : WithInhibitors) : D7::Patch?
    # If the inhibitor cell does not exist, we're fine. If we succeed in `get?`ting it,
    # then consider the feed inhibited.
    active = feed.inhibitors.all? do |inhibitor|
      inhibitor_cell = Rack.cell?(hg, hg.resolve(node.addr, edge(inhibitor)))
      inhibitor_cell.nil? || get?(inhibitor, inhibitor_cell).nil?
    end

    return unless active

    # Otherwise, proceed to the variant.
    step(hg, node, feed.variant)
  end
end
