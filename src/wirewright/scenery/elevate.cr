module Ww::Scenery
  # :nodoc:
  defrecord ElevateResponse,
    node : ElevatedNode,
    box : OriginBox,
    upbound : Slice(ElevateEntry)

  # :nodoc:
  #
  # Represents an upbound node (e.g. overlay).
  defrecord ElevateEntry, node : ElevatedNode, box : Box

  # Leaf nodes remain unchanged.
  private def elevate!(cache, node : Inert | RectShape | Pending | Img | Svg | IconGlyph | ShapedText, box : OriginBox) : ElevateResponse
    ElevateResponse.new(node, box, upbound: Slice(ElevateEntry).empty)
  end

  # Nodes with a subtree elevate() their subtree recursively.
  private def elevate!(cache, node : Padding | Align | XYStack | ZStack | Composite | TransformMatrix | Viewport | Aim | Vantage | Gate, box : OriginBox) : ElevateResponse
    elevated_children, elevated_boxes, upbound = elevate(cache, node.children, box.children)
    elevated_node = node.copy_with(children: elevated_children)
    elevated_box = OriginBox.new(box.bounds.size, children: elevated_boxes)

    ElevateResponse.new(elevated_node, elevated_box, upbound)
  end

  # `Page` nodes turn into a `ZStack`, gather upbound `ElevateEntry`s, and append
  # them to the z-stack.
  private def elevate!(cache, node : Page, box : OriginBox) : ElevateResponse
    elevated_children, elevated_boxes, upbound = elevate(cache, node.children, box.children)
    elevated_node = ZStack.anon(elevated_children + upbound.map(&.node.as(ElevatedNode)))
    elevated_box = OriginBox.new(box.bounds.size, elevated_boxes + upbound.map(&.box))

    ElevateResponse.new(elevated_node, elevated_box, upbound: Slice(ElevateEntry).empty)
  end

  # `Overlay`s first elevate their subtree recursively, then turn the result
  # into an `ElevateEntry`. The overlay itself is replaced by `Inert`, which
  # acts as a kind of tombstone. The box subtree remains unchanged since there's
  # little point in changing it; `Inert` will make it unreachable anyway.
  private def elevate!(cache, node : Overlay, box : OriginBox) : ElevateResponse
    elevated_children, elevated_boxes, upbound = elevate(cache, node.children, box.children)

    upnode = ZStack.anon(elevated_children)
    upbox = Box.new(box.bounds, elevated_boxes)
    upbound = upbound.append(ElevateEntry.new(upnode, upbox))

    ElevateResponse.new(Inert.new, box, upbound)
  end

  private def elevate(cache, node : SizedNode, box : Box) : {ElevatedNode, Box, Slice(ElevateEntry)}
    response = elevate(cache, node, OriginBox.new(box.bounds.size, box.children))
    if box.at_origin?
      return response.node, response.box.translate(Point[0, 0]), response.upbound
    end

    translated_box = response.box.translate(box.bounds.tl)
    translated_upbound = response.upbound.map do |entry|
      ElevateEntry.new(entry.node, box: entry.box.translate(box.bounds.tl))
    end

    {response.node, translated_box, translated_upbound}
  end

  private def elevate(cache, nodes : Slice(SizedNode), boxes : Slice(Box)) : {Slice(ElevatedNode), Slice(Box), Slice(ElevateEntry)}
    elevated_nodes = Pf::Kit.stack_array(ElevatedNode, 8)
    elevated_boxes = Pf::Kit.stack_array(Box, 8)
    upbound = Pf::Kit.stack_array(ElevateEntry, 8)

    nodes.zip(boxes) do |node, box|
      elevated_node, elevated_box, node_upbound = elevate(cache, node, box)
      elevated_nodes << elevated_node
      elevated_boxes << elevated_box
      upbound.concat(node_upbound)
    end

    {elevated_nodes.to_unsafe_readonly_slice!,
     elevated_boxes.to_unsafe_readonly_slice!,
     upbound.to_unsafe_readonly_slice!}
  end

  private def elevate(cache, node : SizedNode, box : OriginBox) : ElevateResponse
    cache.put_if_absent({node, box}) { elevate!(cache, node, box) }
  end

  # Performs the *elevation* pass on *root* and its corresponding *box*.
  # Returns the resulting elevated root and box. See `ElevatedNode` for more
  # info on the elevation pass.
  #
  # NOTE: You must use the returned box along with the returned elevated node;
  # **not** *box*.
  def elevate(cache : CacheSet, root : Root(SizedNode), box : OriginBox) : {Root(ElevatedNode), OriginBox}
    cache.elevate.epoch do
      response = elevate(cache.elevate, root.node, box)

      {Root(ElevatedNode).new(response.node), response.box}
    end
  end
end
