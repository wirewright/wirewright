module Ww::Scenery
  # :nodoc:
  defrecord VBox, bounds : Rect, children : Slice(VBox)

  private def vbox!(cache, node : Inert | RectShape | Pending | Img | Svg | IconGlyph | ShapedText, box : OriginBox) : VBox
    VBox.new(box.bounds, children: Slice(VBox).empty)
  end

  private def vbox!(cache, node : TransformMatrix, box : OriginBox) : VBox
    vbox = vbox(cache, node.children, box.children)

    bounds = Rect.empty
    vbox.children.each do |child|
      bounds = Rect.union(bounds, node.tf.map(child.bounds))
    end

    VBox.new(bounds, vbox.children)
  end

  private def vbox!(cache, node : Clip, box : OriginBox) : VBox
    vbox = vbox(cache, node.children, box.children)

    VBox.new(box.bounds, vbox.children)
  end

  private def vbox!(cache, node : Padding | Align | XYStack | ZStack | Composite | Observer | Observable | Gate, box : OriginBox) : VBox
    vbox(cache, node.children, box.children)
  end

  private def vbox(cache, node : AimedNode, box : OriginBox) : VBox
    cache.put_if_absent({node, box}) { vbox!(cache, node, box) }
  end

  private def vbox(cache, node : AimedNode, box : Box) : VBox
    vbox = vbox(cache, node, OriginBox.new(box.bounds.size, box.children))

    VBox.new(vbox.bounds.translate(box.bounds.tl), vbox.children)
  end

  private def vbox(cache, nodes : Slice(AimedNode), boxes : Slice(Box)) : VBox
    vboxes = Pf::Kit.stack_array(VBox, 8)
    bounds = Rect.empty

    nodes.zip(boxes) do |node, box|
      vbox = vbox(cache, node, box)
      vboxes << vbox
      bounds = Rect.union(bounds, vbox.bounds)
    end

    VBox.new(bounds, vboxes.to_unsafe_readonly_slice!)
  end

  # Returns the *visual box tree* for *root* and its corresponding *box* tree.
  #
  # The visual box tree is different from the [layout] box tree in that it accounts
  # for transforms (`TransformMatrix`, aka `Transform`, aka `scenery.transform`).
  # The visual box tree is sued for hit testing and visibility checking.
  def vbox(cache : CacheSet, root : Root(AimedNode), box : OriginBox) : VBox
    cache.vbox.epoch { vbox(cache.vbox, root.node, box) }
  end
end
