module Ww::Scenery
  # :nodoc:
  defrecord AimResponse, node : AimedNode, foci : Slice(Rect)

  private def aim!(cache, node : Inert | RectShape | Pending | Img | Svg | IconGlyph, box : OriginBox) : AimResponse
    AimResponse.new(node, foci: Slice(Rect).empty)
  end

  private def aim!(cache, node : ShapedText, box : OriginBox) : AimResponse
    x = box.bounds.x
    y = box.bounds.y

    foci = Pf::Kit.stack_array(Rect, 4)

    seen = Set(Decoration).new
    seen.compare_by_identity

    line_wrap(node, at: box.bounds.w) do |line|
      line.items.each do |item|
        case item
        in IBeam
          ibeam = item.selection
          next unless ibeam.aim

          foci << Rect[x - ibeam.clearance, y, ibeam.clearance*2 + ibeam.thickness, node.line_height]
        in Endl, ShapedStyledGlyph
          item.decorations.each do |decoration|
            next unless spec = decoration.spec.as?(Selection)
            next unless spec.aim

            # Skip glyphs in the middle of a selection.
            next unless decoration.anchor_to_left || decoration.anchor_to_right

            case item
            in ShapedStyledGlyph
              advance = item.advance.x
            in Endl
              advance = spec.endl_width
            end

            # Mark the glyph rect as a focus, plus clearance.
            foci << Rect[x - spec.clearance, y, advance + spec.clearance*2, node.line_height]
          end

          # No point incrementing on Endl because the loop will immediately break after
          # it anyway.
          if item.is_a?(ShapedStyledGlyph)
            x += item.advance.x
          end
        end
      end

      x = box.bounds.x
      y += node.line_height
    end

    AimResponse.new(node, foci.to_unsafe_readonly_slice!)
  end

  private def aim!(cache, node : Padding | Align | XYStack | ZStack | Composite | Vantage | Gate, box : OriginBox) : AimResponse
    aimed_children, foci = aim(cache, node.children, box.children)
    aimed_node = node.copy_with(children: aimed_children)

    AimResponse.new(aimed_node, foci)
  end

  private def aim!(cache, node : TransformMatrix, box : OriginBox) : AimResponse
    aimed_children, foci = aim(cache, node.children, box.children)
    aimed_node = node.copy_with(children: aimed_children)

    AimResponse.new(aimed_node, foci: foci.map { |focus| node.tf.map(focus) })
  end

  # Scrolls *page* so that *segment* is visible (1D). Returns *page*'s scrolled
  # start offset.
  private def scroll(page_start : Magnitude, page_size : Magnitude, segment_start : Magnitude, segment_size : Magnitude) : Magnitude
    page_end = page_start + page_size
    segment_end = segment_start + segment_size

    if segment_start < page_start
      return segment_start
    end

    if segment_end > page_end
      if segment_size > page_size # overflow
        return segment_start
      end

      return segment_end - page_size
    end

    page_start
  end

  private def aim!(cache, node : Viewport, box : OriginBox) : AimResponse
    aimed_children, foci = aim(cache, node.children, box.children)

    # Union through aim rects of children to get to the aim rect for us.
    aim_rect = nil

    case node.aim
    in .off?, .off_through?
    in .on?, .on_through?
      foci.each do |focus|
        aim_rect = aim_rect.nil? ? focus : Rect.union(aim_rect, focus)
      end
    end

    # Union through child bounds to determine the content rect.
    content_rect = box.children.reduce(Rect.empty) do |memo, child|
      Rect.union(memo, child.bounds)
    end

    # Resolve offsets.
    total_size = Point.max(box.bounds.size, content_rect.size)
    remaining_size = total_size - box.bounds.size

    page_offset = Point[
      node.page_x.resolve(remaining_size.x),
      node.page_y.resolve(remaining_size.y),
    ]

    offset = Point[
      node.offset_x.resolve(remaining_size.x),
      node.offset_y.resolve(remaining_size.y),
    ]

    # Finalize aim rect. If we don't have an aim rect, make it the page.
    # Offset applies unconditionally from wherever we thus land.
    aim_rect ||= Rect.new(tl: page_offset, size: box.bounds.size)
    aim_rect = aim_rect.translate(offset)

    # Compute the view rect.
    x = scroll(page_offset.x, box.bounds.w, aim_rect.x, aim_rect.w)
    y = scroll(page_offset.y, box.bounds.h, aim_rect.y, aim_rect.h)
    view_offset = Point[x, y].round

    aimed_node = Clip.new(aimed_children, view_offset, node.radii, node.goal)

    case node.aim
    in .on?, .off?
      foci = Slice(Rect).empty
    in .on_through?, .off_through?
      foci = foci.to_readonly_slice(&.translate(-view_offset))
    end

    AimResponse.new(aimed_node, foci)
  end

  private def aim!(cache, node : Aim, box : OriginBox) : AimResponse
    aimed_children, foci = aim(cache, node.children, box.children)

    AimResponse.new(ZStack.anon(aimed_children), foci: foci.append(box.bounds))
  end

  private def aim(cache, node : ElevatedNode, box : OriginBox) : AimResponse
    cache.put_if_absent({node, box}) { aim!(cache, node, box) }
  end

  private def aim(cache, node : ElevatedNode, box : Box) : AimResponse
    response = aim(cache, node, OriginBox.new(box.bounds.size, box.children))
    if box.at_origin?
      return response
    end

    AimResponse.new(response.node, foci: response.foci.map(&.translate(box.bounds.tl)))
  end

  private def aim(cache, nodes : Slice(ElevatedNode), boxes : Slice(Box)) : {Slice(AimedNode), Slice(Rect)}
    aimed_nodes = Pf::Kit.stack_array(AimedNode, 8)
    foci = Pf::Kit.stack_array(Rect, 8)

    nodes.zip(boxes) do |node, box|
      response = aim(cache, node, box)
      aimed_nodes << response.node
      foci.concat(response.foci)
    end

    {aimed_nodes.to_unsafe_readonly_slice!,
     foci.to_unsafe_readonly_slice!}
  end

  # Performs the *aiming* pass on *root*. Returns the resulting aimed root.
  # See `AimedNode` for more info on the aiming pass.
  def aim(cache : CacheSet, root : Root(ElevatedNode), box : OriginBox) : Root(AimedNode)
    cache.aim.epoch do
      response = aim(cache.aim, root.node, box)

      Root(AimedNode).new(response.node)
    end
  end
end
