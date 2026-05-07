module Ww::Scenery
  # Represents the layout box of a `SizedNode`. The boxes of *children* are
  # positioned using the coordinate system of their parent box.
  #
  # See also: `Size`.
  struct Box
    # Returns the bounding rect.
    getter bounds : Rect

    # Returns the children boxes.
    getter children : Slice(Box)

    # :nodoc
    def initialize(@bounds, @children, @children_hash : UInt64)
    end

    def initialize(@bounds, @children)
      @children_hash = @children.hash
    end

    def_copy_with

    # Returns `true` if this box is located at the origin (0; 0).
    def at_origin? : Bool
      bounds.tl.zero?
    end

    # Translates this box by *delta*.
    def translate(delta : Point) : Box
      Box.new(@bounds.translate(delta), @children)
    end

    def hash(hasher)
      @bounds.hash(hasher)
      @children_hash.hash(hasher)
    end
  end

  # Wraps a `Box` with the guarantee that it's located at the origin (0; 0).
  struct OriginBox
    def initialize(@box : Box)
      assert @box.at_origin?
      assert !@box.bounds.inf?
    end

    delegate :bounds, :children, to: @box

    def self.new(size : Point, children : Slice(Box)) : OriginBox
      bounds = Rect.new(tl: Point[0, 0], size: size)

      new(Box.new(bounds, children))
    end

    # Translates the wrapped box by *delta*, which gives you a `Box`.
    def translate(delta : Point) : Box
      @box.translate(delta)
    end
  end

  private def box!(cache, node : Inert | RectShape | Pending | Img | Svg | IconGlyph | ShapedText, size : Size) : OriginBox
    OriginBox.new(size.outer, children: Slice(Box).empty)
  end

  private def box!(cache, node : XYStack, size : Size) : OriginBox
    offset = Point[0, 0]
    child_boxes = Pf::Kit.stack_array(Box, 8)

    node.children.zip(size.children) do |child_node, child_size|
      child_origin_box = box(cache, child_node, child_size)
      child_box = child_origin_box.translate(offset)

      offset += node.axis.select(child_box.bounds.size)
      offset += node.axis.put(node.gap)

      child_boxes << child_box
    end

    OriginBox.new(size.outer, children: child_boxes.to_unsafe_readonly_slice!)
  end

  private def box!(cache, node : Padding, size : Size) : OriginBox
    child_boxes = Pf::Kit.stack_array(Box, 8)

    node.children.zip(size.children) do |child_node, child_size|
      child_origin_box = box(cache, child_node, child_size)
      child_box = child_origin_box.translate(Point[node.pl, node.pt])
      child_boxes << child_box
    end

    OriginBox.new(size.outer, children: child_boxes.to_unsafe_readonly_slice!)
  end

  private def box!(cache, node : Align, size : Size) : OriginBox
    offset = size.outer * node.pivot - size.inner * node.pivot

    child_boxes = Pf::Kit.stack_array(Box, 8)

    node.children.zip(size.children) do |child_node, child_size|
      child_origin_box = box(cache, child_node, child_size)
      child_box = child_origin_box.translate(offset)
      child_boxes << child_box
    end

    OriginBox.new(size.outer, child_boxes.to_unsafe_readonly_slice!)
  end

  private def box!(cache, node : ZStack | Composite | TransformMatrix | Viewport | Aim | Page | Overlay | Observer | Observable | Gate, size : Size) : OriginBox
    child_boxes = Pf::Kit.stack_array(Box, 8)

    node.children.zip(size.children) do |child_node, child_size|
      child_origin_box = box(cache, child_node, child_size)
      child_box = child_origin_box.translate(Point[0, 0])
      child_boxes << child_box
    end

    OriginBox.new(size.outer, children: child_boxes.to_unsafe_readonly_slice!)
  end

  private def box(cache, node : SizedNode, size : Size) : OriginBox
    cache.put_if_absent({node, size}) { box!(cache, node, size) }
  end

  # Computes the box tree for *root* and its size tree *size*.
  def box(cache : CacheSet, root : Root(SizedNode), size : Size) : OriginBox
    cache.boxes.epoch { box(cache.boxes, root.node, size) }
  end
end
