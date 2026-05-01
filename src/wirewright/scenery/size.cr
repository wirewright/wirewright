module Ww::Scenery
  # Represents a _c_on_s_train_t_.
  defrecord Cst, min_w : Magnitude, max_w : Magnitude, min_h : Magnitude, max_h : Magnitude, copying: true do
    assert !min_w.infinite?
    assert !min_h.infinite?
    assert min_w <= max_w
    assert min_h <= max_h
  end

  struct Cst
    # Constrains to content size.
    def self.content : Cst
      new(0, Magnitude::INFINITY, 0, Magnitude::INFINITY)
    end

    # Constrains to a fixed *size*.
    def self.size(size : Point) : Cst
      new(0, size.x, 0, size.y)
    end
  end

  # Represents the size of a `SizedNode`. Sizes form a tree with the same shape
  # as the `SizedTree`. This lets us "zip" over both trees at the same time to
  # access both the sized node fields and its sizing info.
  #
  # This approach of "zipping" over trees is similar in spirit to AoS vs. SoA,
  # where it's more aligned with SoA (but here, it is a "struct" of *trees*,
  # not arrays).
  #
  # This approach is used in several places in `Scenery`, and is a clean, type-
  # safe alternative for nilable fields in nodes themselves, or the duplication
  # of the node hierarchy for each phase (sizing, boxes, visual boxes, etc.); which,
  # considering even the modest amount of nodes in Scenery, would be a very
  # tedious job indeed.
  defcase Size,
    inner : Point,
    outer : Point,
    children : Slice(Size),
    copying: true,
    caches_hash: true

  class Size
    @@zero : Size?

    # Shorthand for `new(Point[0, 0])`.
    def self.zero : Size
      @@zero ||= new(Point[0, 0])
    end

    # Constructs a leaf size node whose inner and outer size are the same
    # constant *size*.
    def self.new(size : Point) : Size
      new(size, size, children: Slice(Size).empty)
    end

    # Constructs a size node whose inner size is *inner*, with zero or more
    # *children*, whose outer size expands to fill all available space according
    # to the constraint *cst*.
    def self.expand(inner : Point, cst : Cst, children : Slice(Size) = Slice(Size).empty) : Size
      outer = inner

      if cst.max_w.finite?
        outer = Point[cst.max_w, outer.y]
      end

      if cst.max_h.finite?
        outer = Point[outer.x, cst.max_h]
      end

      outer = Point.max(outer, Point[cst.min_w, cst.min_h])

      new(inner, outer, children)
    end
  end

  # :nodoc:
  defrecord LineEstimate, text : Pf::GraphemeSeln, advance : Magnitude

  # :nodoc:
  SIZE_TO_CHAR_WIDTH_ESTIMATE = Magnitude.new(0.55)

  # :nodoc:
  #
  # Performs line-wrapping based on a very crude estimate of character width.
  #
  # We use this to format placeholder rects for text nodes while the font(s)
  # are loading.
  #
  # *limit* is allowed to be infinite, in which case no wrapping is performed.
  def line_wrap(node : Text, at limit : Magnitude, & : LineEstimate ->) : Nil
    bol = node.caption
    cursor = bol
    state = nil
    advance = Magnitude.new(0)

    while cursor.present?
      item = cursor.first

      if state && advance > limit
        line, line_advance, next_bol = state
        yield LineEstimate.new(line, advance)

        state = nil
        bol = next_bol
        cursor = bol
        advance = Magnitude.new(0)
        next
      end

      case node.unibreaks[item.byte_start]
      in .no_break?
      in .must_break?
        yield LineEstimate.new(bol.before_start.through(item.before_start), advance)

        state = nil
        cursor = cursor.rest
        bol = cursor
        advance = Magnitude.new(0)
        next
      in .allow_break?
        if item == " "
          state = {bol.before_start.through(item.before_start), advance, cursor.rest}
        else
          state = {bol.before_start.through(item), advance, cursor.rest}
        end
      in .unfinished?, .indeterminate?
      end

      advance += node.size * SIZE_TO_CHAR_WIDTH_ESTIMATE
      cursor = cursor.rest
    end

    yield LineEstimate.new(bol, advance)
  end

  # :nodoc:
  #
  # Performs line-wrapping based on precise character width info from a shaped
  # text *node*.
  #
  # *limit* is allowed to be infinite, in which case no wrapping is performed.
  def line_wrap(node : ShapedText, at limit : Magnitude, & : ShapedLine ->) : Nil
    bol = node.seq
    cursor = bol
    state = nil
    advance = Magnitude.new(0)

    while item = cursor.first?
      case item
      in IBeam
        cursor += 1
      in Endl
        yield ShapedLine.new(bol.upto(cursor + 1), advance)

        state = nil
        cursor += 1
        bol = cursor
        advance = Magnitude.new(0)
      in ShapedStyledGlyph
        if state && advance > limit
          line, line_advance, next_bol = state
          yield ShapedLine.new(line, line_advance)

          state = nil
          bol = next_bol
          cursor = bol
          advance = Magnitude.new(0)
          next
        end

        case item.break_policy
        in .no_break?
        in .preserve?
          state = {bol.upto(cursor + 1), advance, cursor + 1}
        in .collapse?
          state = {bol.upto(cursor), advance, cursor + 1}
        end

        advance += item.advance.x
        cursor += 1
      end
    end

    yield ShapedLine.new(bol, advance)
  end

  # :nodoc:
  #
  # Yields words (as `ShapedLine`s) in *node*.
  #
  # See the other overload for *kwargs*.
  def each_word(node : ShapedText, **kwargs, &) : Nil
    each_word(node.seq, **kwargs) { |word| yield word }
  end

  # :ditto:
  def each_word(node : ShapedLine, **kwargs, &) : Nil
    each_word(node.items, **kwargs) { |word| yield word }
  end

  # :nodoc:
  #
  # Yields words (as `ShapedLine`s) in a sequence of shaped items *seq*.
  #
  # If *collapse* is `true`, whitespace between the words is collapsed; otherwise,
  # it "sticks" to the word before it.
  def each_word(seq : Slice(ShapedItem), *, collapse : Bool = true, &) : Nil
    word = seq
    cursor = word
    advance = Magnitude.new(0)

    while item = cursor.first?
      if item.is_a?(IBeam)
        cursor += 1
        next
      end

      case item
      in Endl
        yield ShapedLine.new(word.upto(cursor), advance)
      in ShapedStyledGlyph
        case item.break_policy
        in .no_break?
          cursor += 1
          advance += item.advance.x
          next
        in .preserve?
          yield ShapedLine.new(word.upto(cursor + 1), advance)
        in .collapse?
          yield ShapedLine.new(collapse ? word.upto(cursor) : word.upto(cursor + 1), advance)
        end
      end

      cursor += 1
      word = cursor
      advance = Magnitude.new(0)
    end

    yield ShapedLine.new(word, advance)
  end

  private def min_size!(cache, node : Inert) : Point
    Point[0, 0]
  end

  private def min_size!(cache, node : RectShape) : Point
    Point[1, 1]
  end

  private def min_size!(cache, node : IconGlyph) : Point
    measurement = node.font.measure(node.glyph_index, node.size)

    Point[measurement.advance, node.size]
  end

  private def min_size!(cache, node : ShapedText) : Point
    min_width = Magnitude.new(0)
    min_height = Magnitude.new(0)

    each_word(node) do |word|
      min_width = Math.max(min_width, word.advance)
    end

    line_wrap(node, at: min_width) do
      min_height += node.line_height
    end

    Point[min_width, min_height]
  end

  private def min_size!(cache, node : Svg | Img) : Point
    Point[8, 8]
  end

  private def min_size!(cache, node : Pending) : Point
    case blame = node.blame
    when Text
      Point[8, blame.leading.resolve(blame.size) + Math.max(0, TEXT_PLACEHOLDER_LINE_GAP * (blame.size - 1))]
    else
      Point[8, 8]
    end
  end

  # FIXME: Variant's min size isn't really 0x0, is it? We should somehow compute
  # the proper min size *while avoiding circularity*.
  private def min_size!(cache, node : Floating | Overlay | Viewport | Variant) : Point
    Point[0, 0]
  end

  private def min_size!(cache, node : Limit) : Point
    Point.max(min_size(cache, node.children), Point[node.min_w.resolve(0), node.min_h.resolve(0)])
  end

  private def min_size!(cache, node : ZStack | Content | Align | Composite | Transform | Aim | Page | Observer | Observable | Gate) : Point
    min_size(cache, node.children)
  end

  private def min_size!(cache, node : Padding) : Point
    min_size(cache, node.children) + Point[node.pl + node.pr, node.pt + node.pb]
  end

  private def min_size!(cache, node : XYStack) : Point
    size = Point[0, 0]
    count = 0

    node.children.each do |child|
      child_size = min_size(cache, child)
      next if child_size.zero?

      size = Point.max(
        node.axis.select(size) + node.axis.select(child_size),
        node.axis.cross.select(size),
        node.axis.cross.select(child_size),
      )

      count += 1
    end

    if count > 0
      gaps = node.axis.put(node.gap * (count - 1))
      size += gaps
    end

    size
  end

  private def min_size!(cache, node : XYWrap) : Point
    main = XYStack.new(
      axis: node.axis.cross, # < as if we're wrapping at all opportunities
      children: node.children,
      shares: node.line_shares.call(node.children.size),
      gap: node.axis.get(node.gap),
    )

    min_size(cache, main)
  end

  private def min_size(cache, nodes : Slice(ShapedNode)) : Point
    assert nodes.present?

    nodes.reduce(Point[0, 0]) do |memo, node|
      Point.max(memo, min_size(cache, node))
    end
  end

  private def min_size(cache, node : ShapedNode) : Point
    cache.put_if_absent(node) { min_size!(cache, node) }
  end

  private def size!(cache, node : Inert, cst : Cst) : {SizedNode, Size}
    {node, Size.zero}
  end

  # NOTE: If the parent doesn't know the size and the rect doesn't know the size
  # either, we'll use some hard-coded default size to show *something* to
  # the user. But this isn't a good situation to be in, generally.
  private def size!(cache, node : RectShape, cst : Cst) : {SizedNode, Size}
    {node, Size.expand(Point[16, 16], cst)}
  end

  # :nodoc:
  #
  # A small gap we add between text placeholder lines (pixels).
  TEXT_PLACEHOLDER_LINE_GAP = 3

  private def size!(cache, node : Pending, cst : Cst) : {SizedNode, Size}
    unless text = node.blame.as?(Text)
      return node, Size.expand(min_size(cache.min_size, node), cst)
    end

    width = Magnitude.new(0)
    height = Magnitude.new(0)

    line_wrap(text, at: cst.max_w) do |line|
      if height > 0
        # Add a tiny gap between the lines, otherwise on default leading: _
        # values we'd see no gap and that'd look bad.
        height += TEXT_PLACEHOLDER_LINE_GAP
      end

      width = Math.max(width, line.advance)
      height += text.leading.resolve(text.size) # A crude estimate of line height
    end

    {node, Size.expand(Point[width, height], cst)}
  end

  private def size!(cache, node : IconGlyph, cst : Cst) : {SizedNode, Size}
    inner_size = min_size(cache.min_size, node)

    {node, Size.expand(inner_size, cst)}
  end

  private def size!(cache, node : ShapedText, cst : Cst) : {SizedNode, Size}
    width = Magnitude.new(0)
    height = Magnitude.new(0)

    line_wrap(node, at: cst.max_w) do |line|
      width = Math.max(width, line.advance)
      height += node.line_height
    end

    {node, Size.expand(Point[width, height], cst)}
  end

  private def size!(cache, node : Svg | Img, cst : Cst) : {SizedNode, Size}
    # Ask the image about its size and use that as the inner size. Images will
    # scale to their box also, of course. This affects particularly the
    # `transform` node: how deltas are applied, what the origin point means, etc.
    inner_size = node.src.size

    {node, Size.expand(inner_size, cst)}
  end

  private def size!(cache, node : Content, cst : Cst) : {SizedNode, Size}
    if node.x
      cst = Cst.new(0, Magnitude::INFINITY, cst.min_h, cst.max_h)
    end

    if node.y
      cst = Cst.new(cst.min_w, cst.max_w, 0, Magnitude::INFINITY)
    end

    box_size(cache, node, cst)
  end

  private def size!(cache, node : Floating, cst : Cst) : {SizedNode, Size}
    node, size = box_size(cache, node, cst)

    if node.x
      size = size.copy_with(outer: Point[0, size.outer.y])
    end

    if node.y
      size = size.copy_with(outer: Point[size.outer.x, 0])
    end

    {node, size}
  end

  private def size!(cache, node : Overlay, cst : Cst) : {SizedNode, Size}
    node, size = box_size(cache, node, Cst.content)

    {node, size.copy_with(outer: Point[0, 0])}
  end

  private def size!(cache, node : Transform, cst : Cst) : {SizedNode, Size}
    # NOTE: We can't use box_size and give Transform to it here because Transform
    # isn't a member of the SizedNode union. So box_size wouldn't know how to
    # rewrite it into one. Instead, we convert the Transform temporarily into
    # a ZStack.
    sized_z_stack, size = size!(cache, ZStack.new(node.children), cst)

    bounds = Rect.new(tl: Point[0, 0], size: size.inner)

    origin = bounds.map(node.translation.origin)
    pivot = bounds.map(node.rotation.origin)
    scale = node.scale.factor

    total_w = cst.max_w.infinite? ? bounds.w : Math.max(bounds.w, cst.max_w)
    total_h = cst.max_h.infinite? ? bounds.h : Math.max(bounds.h, cst.max_h)

    offset = Point[
      node.translation.dl.resolve(total_w - bounds.w),
      node.translation.dt.resolve(total_h - bounds.h),
    ]

    origin -= offset

    tf = Tf[
      Tf.translate(-origin),
      Tf.translate(pivot),
      Tf.rotate(node.rotation.angle),
      Tf.scale(scale),
      Tf.translate(-pivot),
    ]

    {TransformMatrix(SizedNode).new(sized_z_stack.children, tf), size}
  end

  private def size!(cache, node : ZStack | Composite | Viewport | Aim | Page | Observer | Observable | Gate, cst : Cst) : {SizedNode, Size}
    box_size(cache, node, cst)
  end

  private def size!(cache, node : Limit, cst : Cst) : {SizedNode, Size}
    min_w = node.min_w.resolve(cst.max_w)
    if min_w.infinite?
      min_w = cst.min_w
    end

    min_h = node.min_h.resolve(cst.max_h)
    if min_h.infinite?
      min_h = cst.min_h
    end

    max_w = node.max_w.try(&.resolve(cst.max_w)) || Magnitude::INFINITY
    max_h = node.max_h.try(&.resolve(cst.max_h)) || Magnitude::INFINITY

    child_max_w = Math.min(max_w, cst.max_w)
    child_max_h = Math.min(max_h, cst.max_h)

    child_min_w = Math.min(child_max_w, Math.max(min_w, cst.min_w))
    child_min_h = Math.min(child_max_h, Math.max(min_h, cst.min_h))

    child_cst = Cst.new(child_min_w, child_max_w, child_min_h, child_max_h)

    box_size(cache, node, child_cst)
  end

  private def size!(cache, node : Padding, cst : Cst) : {SizedNode, Size}
    delta = Point[node.pl + node.pr, node.pt + node.pb]

    child_cst = Cst.new(
      min_w: Math.max(Magnitude.new(0), cst.min_w - node.pl - node.pr),
      max_w: Math.max(Magnitude.new(0), cst.max_w - node.pl - node.pr),
      min_h: Math.max(Magnitude.new(0), cst.min_h - node.pt - node.pb),
      max_h: Math.max(Magnitude.new(0), cst.max_h - node.pt - node.pb),
    )

    sized_node, size = box_size(cache, node, child_cst)

    {sized_node, size.copy_with(outer: size.outer + delta)}
  end

  private def size!(cache, node : Align, cst : Cst) : {SizedNode, Size}
    child_cst = Cst.content

    # Keep X constraint if only Y changed.
    if node.pivot.x.zero?
      child_cst = Cst.new(cst.min_w, cst.max_w, child_cst.min_h, child_cst.max_h)
    end

    # Keep Y constraint if only X changed.
    if node.pivot.y.zero?
      child_cst = Cst.new(child_cst.min_w, child_cst.max_w, cst.min_h, cst.max_h)
    end

    # ... so if you do x: 0 y: 0, that's a noop, not content x: true y: true!

    sized_node, size = box_size(cache, node, child_cst)

    {sized_node, Size.expand(size.inner, cst, size.children)}
  end

  # :nodoc:
  defrecord FlexShare, num : Magnitude, min : Magnitude
  # :nodoc:
  defrecord FixedShare, span : Magnitude

  # NOTE: This could probably be refactored, but I doubt there'd be a whole lot
  # of point in that. It's not that this alg is meant to change or be read/
  # maintained very often...
  private def size!(cache, node : XYStack, cst : Cst) : {SizedNode, Size}
    sized_children_count = 0

    content_sized_node, child_content_sizes = subsizes(cache, node, node.axis.cross.select(cst)) do |_, child_size|
      next if node.axis.select(child_size.outer).zero?

      sized_children_count += 1
    end

    if sized_children_count.zero?
      return content_sized_node, Size.expand(Point[0, 0], cst, child_content_sizes)
    end

    size = node.axis.put(node.gap * (sized_children_count - 1))

    # If axis is content-sized, arrange content-sized children without respecting
    # fr and so on. Notice how we skip zero-sized children.
    if node.axis.inf?(cst)
      child_content_sizes.each do |child_content_size|
        next if node.axis.select(child_content_size.outer).zero?

        size = Point.max(
          node.axis.select(size) + node.axis.select(child_content_size.outer),
          node.axis.cross.select(size),
          node.axis.cross.select(child_content_size.outer),
        )
      end

      # We need to remeasure because child_content_size is pure intrinsic size of
      # each child without dependence on siblings or the parent; whereas we want
      # the cross axis to be the min(cst cross axis, max(sibling cross axes)).
      #
      # cst.min_w/h is removed because it only makes sense for the parent, it cannot be
      # applied easily onto children.
      case node.axis
      in .x? then child_cst = Cst.new(0, size.x, cst.min_h, cst.max_h)
      in .y? then child_cst = Cst.new(cst.min_w, cst.max_w, 0, size.y)
      end

      node, subsizes = subsizes(cache, node, child_cst)

      return node, Size.expand(size, cst, subsizes)
    end

    # If axis size is restricted, distribute available space between children.
    avail = node.axis.max(cst) - (sized_children_count - 1)*node.gap
    den = Magnitude.new(0)

    shares = child_content_sizes.to_readonly_slice do |child_content_size, index|
      next if node.axis.select(child_content_size.outer).zero?

      case share = node.shares[index]
      in FrShare
        # Ask the node about its absolutely smallest size. We'll reserve this
        # size for the node.
        min_size = min_size(cache.min_size, node.children[index])

        den += share.num
        span = node.axis.get(min_size)
        avail -= span

        FlexShare.new(share.num, span)
      in ContentShare
        span = node.axis.get(child_content_size.outer)
        avail -= span

        FixedShare.new(span)
      end
    end

    avail = Math.max(avail, Magnitude.new(0))

    node, child_sizes = subsizes(node) do |sized_children, child_sizes|
      node.children.zip(shares) do |child, share|
        case share
        in Nil
          span = Magnitude::INFINITY
        in FlexShare
          if den.zero?
            span = share.min
          else
            span = ((share.num/den) * avail + share.min).floor
          end
        in FixedShare
          span = share.span
        end

        case node.axis
        in .x? then child_cst = Cst.new(Magnitude.new(0), span, cst.min_h, cst.max_h)
        in .y? then child_cst = Cst.new(cst.min_w, cst.max_w, Magnitude.new(0), span)
        end

        sized_child, child_size = size(cache, child, child_cst)

        size = Point.max(
          node.axis.select(size) + node.axis.select(child_size.outer),
          node.axis.cross.select(size),
          node.axis.cross.select(child_size.outer),
        )

        sized_children << sized_child
        child_sizes << child_size
      end
    end

    {node, Size.expand(size, cst, child_sizes)}
  end

  private def size!(cache, node : XYWrap, cst : Cst) : {SizedNode, Size}
    if node.axis.inf?(cst)
      main = XYStack.new(
        axis: node.axis,
        children: node.children,
        shares: node.item_shares,
        gap: node.axis.get(node.gap),
      )

      return size(cache, main, cst)
    end

    origin = node.children
    start = origin
    feed = origin
    span = Magnitude.new(0)
    lines = Pf::Kit.stack_array(ShapedNode, 8)

    submit = -> do
      line = start.upto(feed)
      main = XYStack.new(
        axis: node.axis,
        children: line,
        shares: node.item_shares[start.ptr - origin.ptr, line.size],
        gap: node.axis.get(node.gap),
      )
      lines << main
      start = feed
      span = Magnitude.new(0)
    end

    while child = feed.first?
      if start.ptr < feed.ptr
        span += node.axis.get(node.gap)
      end

      _, child_size = size(cache, child, Cst.content)
      advance = node.axis.get(child_size.outer)

      if start.ptr < feed.ptr && span + advance > node.axis.max(cst)
        submit.call
      end

      feed += 1
      span += advance
    end

    submit.call

    cross = XYStack.new(
      axis: node.axis.cross,
      children: lines.to_unsafe_readonly_slice!,
      shares: node.line_shares.call(lines.size),
      gap: node.axis.cross.get(node.gap),
    )

    size(cache, cross, cst)
  end

  private def size!(cache, node : Variant, cst : Cst) : {SizedNode, Size}
    vars = Term[
      "min-w": cst.min_w.infinite? ? :∞ : cst.min_w,
      "max-w": cst.max_w.infinite? ? :∞ : cst.max_w,
      "min-h": cst.min_h.infinite? ? :∞ : cst.min_h,
      "max-h": cst.max_h.infinite? ? :∞ : cst.max_h,
    ]

    interior = ZStack.new(node.children)
    _, content_size = size(cache, interior, Cst.content)
    width = content_size.outer.x
    height = content_size.outer.y

    vars = vars
      .with(:w, width.infinite? ? :∞ : width)
      .with(:h, height.infinite? ? :∞ : height)

    outcome = Nitrene.eval(vars, node.cond)
    if outcome.unwrap == Term.of(false)
      return Inert.new, Size.new(Point[0, 0])
    end

    size(cache, interior, cst)
  end

  private def box_size(cache, node : ShapedNode, cst : Cst) : {SizedNode, Size}
    w = cst.max_w
    h = cst.max_h

    if w.infinite?
      w = Magnitude.new(0)

      node.children.each do |child|
        _, child_size = size(cache, child, Cst.content)
        w = Math.max(w, child_size.outer.x)
      end
    end

    if h.infinite?
      h = Magnitude.new(0)

      node.children.each do |child|
        _, child_size = size(cache, child, Cst.new(0, w, 0, Magnitude::INFINITY))
        h = Math.max(h, child_size.outer.y)
      end
    end

    child_cst = Cst.new(cst.min_w, Math.max(w, cst.min_w), cst.min_h, Math.max(h, cst.min_h))
    node, child_sizes = subsizes(cache, node, child_cst)

    inner = Point[0, 0]
    child_sizes.each do |child_size|
      inner = Point.max(inner, child_size.outer)
    end

    {node, Size.expand(inner, cst, child_sizes)}
  end

  private def size(cache : CacheSet, node : ShapedNode, cst : Cst) : {SizedNode, Size}
    cache.measurement.put_if_absent({node, cst}) do
      size!(cache, node, cst)
    end
  end

  private def subsizes(node, &) : {SizedNode, Slice(Size)}
    children = Pf::Kit.stack_array(SizedNode, 8)
    subsizes = Pf::Kit.stack_array(Size, 8)
    yield children, subsizes

    sized_node = node.copy_with(children: children.to_unsafe_readonly_slice!)
    {sized_node, subsizes.to_unsafe_readonly_slice!}
  end

  private def subsizes(cache, node, cst : Cst, &) : {SizedNode, Slice(Size)}
    subsizes(node) do |children, child_sizes|
      node.children.each do |child0|
        child1, child_size = size(cache, child0, cst)
        children << child1
        child_sizes << child_size

        yield child1, child_size
      end
    end
  end

  private def subsizes(cache, node, cst : Cst) : {SizedNode, Slice(Size)}
    subsizes(cache, node, cst) { }
  end

  # Performs the sizing pass on a shaped *root*. *cst* is the size constraint.
  #
  # The algorithm is inspired by (among many other things) [Flutter constraints](https://docs.flutter.dev/ui/layout/constraints).
  def size(cache : CacheSet, root : Root(ShapedNode), cst : Cst) : {Root(SizedNode), Size}
    cache.measurement.epoch do
      cache.min_size.epoch do
        sized_node, size = size(cache, root.node, cst)

        {Root(SizedNode).new(sized_node), size}
      end
    end
  end
end
