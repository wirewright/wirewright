module Ww::Scenery
  # A compact, immutable description of a user interaction in screen-space,
  # interpreted as either a single point, or a selection region.
  struct HitQuery
    enum Mode
      Point
      Single
      Double
      Triple
    end

    # Returns the *anchor* of this query, which is the point that stays static
    # as you move a rectangular selection. In a point query, *anchor* and
    # *focus* are the same.
    getter anchor : Point

    # Returns the *focus* of this query, which is the movable point of
    # the selection. In a point query, *anchor* and *focus* are the same.
    getter focus : Point

    # Returns the *mode* of this selection. This corresponds to clicking, double-
    # clicking, triple-clicking and so on.
    getter mode : Mode

    def initialize(@anchor, @focus, @mode)
    end

    def self.zero : HitQuery
      new(Point[0, 0], Point[0, 0], mode: :point)
    end

    def self.parse(term : Term) : HitQuery
      Term.case(term) do
        matchpi %{[point ±x ±y]}, x: Magnitude, y: Magnitude do
          zero.move(Point[x, y])
        end

        matchpi(
          %{[single [anchor ±ax ±ay] [focus ±fx ±fy]]},
          ax: Magnitude,
          ay: Magnitude,
          fx: Magnitude,
          fy: Magnitude,
        ) do
          zero.move(Point[ax, ay]).single.move(Point[fx, fy])
        end
      end
    end

    # Returns a copy of this query which is set to be a `Mode::Point` query.
    def point : HitQuery
      HitQuery.new(@focus, @focus, mode: :point)
    end

    # Returns a copy of this query which is set to be a `Mode::Single` query.
    def single : HitQuery
      HitQuery.new(@focus, @focus, mode: :single)
    end

    # Returns a copy of this query which is set to be a `Mode::Double` query.
    def double : HitQuery
      HitQuery.new(@focus, @focus, mode: :double)
    end

    # Returns a copy of this query which is set to be a `Mode::Triple` query.
    def triple : HitQuery
      HitQuery.new(@focus, @focus, mode: :triple)
    end

    # Returns a copy of this query moved to a particular *point* (e.g., on
    # mouse move).
    def move(point : Point) : HitQuery
      if @mode.point?
        return HitQuery.new(point, point, @mode)
      end

      HitQuery.new(@anchor, point, @mode)
    end

    # Converts this query to a rect.
    def rect : Rect
      Rect.new(tl: Point.min(@anchor, @focus), br: Point.max(@anchor, @focus))
    end
  end

  # The hit tree is like the `Box` tree or the `Size` tree or the `VBox` tree
  # etc.; except it's about *hits*, as in, mouse hits, as in, things currently
  # hovered over with the mouse. We generalize quite a bit from this picture with
  # `HitQuery`, but the idea stays the same: we define some region of interest in
  # screen-space coordinates; and we want a tree representation of which nodes
  # are in this region of interest, isomorphic to the original `AimedNode` tree;
  # and of their "reaction" to being there.
  alias HitNode = HitEmpty | HitLeaf | HitTextLeaf | HitGroup

  # The current node and its subtree (if any) were not hit by the hit query.
  defrecord HitEmpty

  # A leaf was hit by the hit query (e.g., a rectangle).
  #
  # *part* is the part of the leaf's visual bounds that was hit.
  defrecord HitLeaf, part : Rect, copying: true

  # A text node was hit by the hit query.
  #
  # *part* is the part of the text's visual bounds that was hit.
  defrecord HitTextLeaf,
    anchor : Int32,
    focus : Int32,
    seln : Pf::GraphemeSeln,
    part : Rect,
    copying: true

  # Multiple children of the current node were hit by the hit query.
  #
  # *part* is the part of the current node's visual bounds that was hit.
  defrecord HitGroup, children : Slice(HitNode), part : Rect, copying: true

  # Returns a tree of hit nodes which represents the union of two hit nodes.
  # The hit nodes must come from the same node tree.
  #
  # NOTE: Currently, we do not support unioning two `HitTextLeaf` nodes.
  # The one from *a* will be preferred.
  def union(a : HitNode, b : HitNode) : HitNode
    case {a, b}
    in {HitEmpty, _}              then b
    in {_, HitEmpty}              then a
    in {HitTextLeaf, HitTextLeaf} then a # ?!
    in {_, HitTextLeaf}           then b # ?!
    in {HitTextLeaf, _}           then a # ?!
    in {HitLeaf, HitLeaf}         then HitLeaf.new(Rect.union(a.part, b.part))
    in {HitLeaf, HitGroup}        then b.copy_with(part: Rect.union(a.part, b.part))
    in {HitGroup, HitLeaf}        then a.copy_with(part: Rect.union(a.part, b.part))
    in {HitGroup, HitGroup}
      assert a.children.size == b.children.size

      children = Pf::Kit.stack_array(HitNode)
      a.children.zip(b.children) do |a_child, b_child|
        children << union(a_child, b_child)
      end

      HitGroup.new(children.to_unsafe_readonly_slice!, part: Rect.union(a.part, b.part))
    end
  end

  private def hit(node : Inert | RectShape | Pending | Img | Svg | IconGlyph, box : Box, vbox : VBox, tf : Tf, query : HitQuery) : HitNode
    part = Rect.intersection(vbox.bounds, tf.inverse.map(query.rect))
    part.negative? ? HitEmpty.new : HitLeaf.new(part)
  end

  private def hit_index(node : ShapedText, box : Box, point : Point) : Int32
    if point.y <= 0
      return 0 # before begin
    end

    x = Magnitude.new(0)
    y = Magnitude.new(0)
    cursor = 0

    line_wrap(node, at: box.bounds.w) do |line|
      if point.y > y + node.line_height
        line.items.each do |item|
          case item
          in IBeam
          in Endl, ShapedStyledGlyph
            cursor = item.grapheme_index
          end
        end

        y += node.line_height
        next
      end

      line.items.each do |item|
        case item
        in IBeam
        in Endl
          cursor = item.grapheme_index
        in ShapedStyledGlyph
          cursor = item.grapheme_index

          mid = x + (item.advance.x / 2)
          if point.x >= mid
            x += item.advance.x
            cursor += 1
            next
          end

          break
        end
      end

      return cursor
    end

    node.caption.size # after end
  end

  private def hit(node : ShapedText, box : Box, vbox : VBox, tf : Tf, query : HitQuery) : HitNode
    itf = tf.inverse

    part = Rect.intersection(vbox.bounds, itf.map(query.rect))
    if part.negative?
      return HitEmpty.new
    end

    anchor_index = hit_index(node, box, itf.map(query.anchor)).clamp(0..node.caption.size)
    focus_index = hit_index(node, box, itf.map(query.focus)).clamp(0..node.caption.size)

    from = Math.min(anchor_index, focus_index)
    to = Math.max(anchor_index, focus_index)

    seln = node.caption.select(from, to)

    HitTextLeaf.new(anchor_index, focus_index, seln, part)
  end

  private def hit(node : TransformMatrix, box : Box, vbox : VBox, tf : Tf, query : HitQuery) : HitNode
    box_hit(node, box, vbox, tf, Tf[tf, node.tf], query)
  end

  # TODO: Implement proper rounded rect-rect intersection?
  private def hit(node : Clip, box : Box, vbox : VBox, tf : Tf, query : HitQuery) : HitNode
    box_hit(node, box, vbox, tf, Tf[tf, Tf.translate(-node.offset)], query)
  end

  private def hit(node : Padding | Align | XYStack | ZStack | Composite | Observer | Observable | Gate, box : Box, vbox : VBox, tf : Tf, query : HitQuery) : HitNode
    box_hit(node, box, vbox, tf, tf, query)
  end

  # NOTE: We're using *box* for its width only; so we won't bother doing coordinate
  # system translation here.
  private def box_hit(node : AimedNode, box : Box, vbox : VBox, tf : Tf, child_tf : Tf, query : HitQuery) : HitNode
    part = Rect.intersection(vbox.bounds, tf.inverse.map(query.rect))
    if part.negative?
      return HitEmpty.new
    end

    hits = Pf::Kit.stack_array(HitNode)
    present = false

    # Iterate in reverse for occlusion.
    (0...node.children.size).reverse_each do |child_index|
      if present && query.mode.point?
        hits << HitEmpty.new
        next
      end

      child_node = node.children[child_index]
      child_box = box.children[child_index]
      child_vbox = vbox.children[child_index]

      translated_child_tf = Tf[child_tf, Tf.translate(child_vbox.bounds.tl)]
      child_vbox = VBox.new(Rect.new(tl: Point[0, 0], size: child_vbox.bounds.size), child_vbox.children)

      child_hit = hit(child_node, child_box, child_vbox, translated_child_tf, query)
      present ||= !child_hit.is_a?(HitEmpty)
      hits << child_hit
    end

    unless present
      return HitEmpty.new
    end

    hits.reverse!

    HitGroup.new(hits.to_unsafe_readonly_slice!, part)
  end

  # Constructs a hit tree for *root*, *box*, and *vbox* according to *query*.
  def hit(root : Root(AimedNode), box : OriginBox, vbox : VBox, query : HitQuery) : HitNode
    hit(root.node, box.translate(Point[0, 0]), vbox, Tf.new, query)
  end

  # Constructs a hit tree for *scene* according to *query*.
  def hit(scene : Scene, query : HitQuery) : HitNode
    hit(scene.root, scene.box, scene.vbox, query)
  end
end
