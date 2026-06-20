module Ww::Scenery
  # See `Safe.describe`.
  def describe(cache : CacheSet, scene : Scene, hit : HitNode) : Slice(Term)
    descriptions = describe(cache, scene.root, scene.box, hit)

    Description.vantages(descriptions)
  end

  private def describe(cache : CacheSet, root : Root(AimedNode), box : OriginBox, hit : HitNode)
    cache.describe.epoch do
      describe(cache.describe, root.node, box, hit)
    end
  end

  class Description
    def initialize(@describe : (-> Slice(Term)), @vantages : (-> Slice(Term)))
    end

    # Asks the callee to describe its subtree (including itself).
    def describe : Slice(Term)
      @describe_result ||= @describe.call
    end

    # Asks the callee for vantages in its subtree (including itself).
    def vantages : Slice(Term)
      @vantages_result ||= @vantages.call
    end
  end

  def Description.vantages(descriptions : Indexable(Description), tail : Tuple() | Indexable(Term) = Tuple.new) : Slice(Term)
    sink = Pf::Kit.stack_array(Term, 8)

    descriptions.each do |description|
      sink.concat(description.vantages)
    end

    tail.each do |term|
      sink << term
    end

    sink.to_unsafe_readonly_slice!
  end

  private def describe(cache, node : AimedNode, box : OriginBox, hit : HitNode)
    cache.put_if_absent({node, box, hit}) do
      describe!(cache, node, box, hit)
    end
  end

  private def describe!(cache, node : Inert, box : OriginBox, hit : HitNode)
    describe = -> { Slice(Term).empty }
    vantages = -> { Slice(Term).empty }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : RectShape, box : OriginBox, hit : HitNode)
    describe = -> do
      result = Term::Dict.build do |commit|
        commit << :rect
        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Slice(Term).empty }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : Pending, box : OriginBox, hit : HitNode)
    describe = -> do
      result = Term::Dict.build do |commit|
        commit << :pending
        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Slice(Term).empty }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : Img, box : OriginBox, hit : HitNode)
    describe = -> do
      result = Term::Dict.build do |commit|
        commit << :img
        commit.with(:src, node.src.digest)

        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Slice(Term).empty }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : Svg, box : OriginBox, hit : HitNode)
    describe = -> do
      result = Term::Dict.build do |commit|
        commit << :svg
        commit.with(:src, node.src.digest)

        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Slice(Term).empty }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : IconGlyph, box : OriginBox, hit : HitNode)
    describe = -> do
      result = Term::Dict.build do |commit|
        commit << :icon
        commit.with(:font, node.font.digest)
        commit.with(:codepoint, node.codepoint)
        commit.with(:glyph, node.glyph_index)
        commit.with(:size, node.size.value)

        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Slice(Term).empty }

    Slice[Description.new(describe, vantages)]
  end

  private def rel_annotate(parent, whole : Pf::GraphemeSeln, part : Pf::GraphemeSeln)
    if parent.anchor <= parent.focus
      # Left-leaning, counts from seln.begin, span is positive.
      {part.begin - whole.begin, part.size}
    else
      # Right-leaning, counts from seln.end, span is negative.
      {part.end - whole.begin, -part.size}
    end
  end

  # TODO: Refactor, this thing is *massive*.
  private def describe!(cache, node : ShapedText, box : OriginBox, hit : HitNode)
    describe = -> do
      result = Term::Dict.build do |commit|
        commit << :text
        commit.with(:caption, node.caption.to_s)
        commit.with(:"line-h", describe(node.line_height))

        if node.selections.present?
          commit.with(:selection, true)
        end

        if selection = node.selections.first?
          seln = node.caption.select(selection.range)
          selection_anchor, selection_span = rel_annotate(selection, node.caption, seln)
          commit.with(:"selection-anchor", selection_anchor)
          commit.with(:"selection-span", selection_span)
        end

        x = Magnitude.new(0)
        y = Magnitude.new(0)

        line_infos = [] of Term
        word_infos = [] of Term

        line_wrap(node, at: box.bounds.w) do |line|
          line_info = Term::Dict.build do |info|
            range = line.grapheme_range? || (0...0)
            assert range.exclusive?

            info << :line
            info.with(:dl, describe(x)) unless x.approx?(0)
            info.with(:dt, describe(y)) unless y.approx?(0)
            info.with(:w, describe(line.advance))
            info.with(:anchor, range.begin)
            info.with(:span, range.size)

            text = node.caption.select(range)

            node.selections.each do |selection|
              seln = node.caption.select(selection.range)
              next unless part = Pf::GraphemeSeln.intersection?(text, seln)

              selection_anchor, selection_span = rel_annotate(selection, text, part)
              info.with(:"selection-anchor", selection_anchor)
              info.with(:"selection-span", selection_span)
              break
            end

            next unless hit.is_a?(HitTextLeaf)
            next unless part = Pf::GraphemeSeln.intersection?(text, hit.seln)

            hit_anchor, hit_span = rel_annotate(hit, text, part)
            info.with(:"hit-anchor", hit_anchor)
            info.with(:"hit-span", hit_span)
          end

          line_infos << Term.of(line_info)

          each_word(line, collapse: false) do |word|
            next unless range = word.grapheme_range?
            assert range.exclusive?

            word_info = Term::Dict.build do |info|
              text = node.caption.select(range)

              info << :word
              info.with(:dl, describe(x)) unless x.approx?(0)
              info.with(:dt, describe(y)) unless y.approx?(0)
              info.with(:w, describe(word.advance))
              info.with(:anchor, range.begin)
              info.with(:span, range.size)

              node.selections.each do |selection|
                seln = node.caption.select(selection.range)
                next unless part = Pf::GraphemeSeln.intersection?(text, seln)

                selection_anchor, selection_span = rel_annotate(selection, text, part)
                info.with(:"selection-anchor", selection_anchor)
                info.with(:"selection-span", selection_span)
                break
              end

              next unless hit.is_a?(HitTextLeaf)
              next unless part = Pf::GraphemeSeln.intersection?(text, hit.seln)

              hit_anchor, hit_span = rel_annotate(hit, text, part)
              info.with(:"hit-anchor", hit_anchor)
              info.with(:"hit-span", hit_span)
            end

            word_infos << Term.of(word_info)

            x += word.advance
          end

          x = Magnitude.new(0)
          y += node.line_height
        end

        commit.concat(line_infos)
        commit.concat(word_infos)

        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Slice(Term).empty }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : Padding | Align | Composite | TransformMatrix, box : OriginBox, hit : HitNode)
    describe(cache, node.children, box.children, hit)
  end

  private def describe!(cache, node : XYStack, box : OriginBox, hit : HitNode)
    descriptions = describe(cache, node.children, box.children, hit)

    describe = -> do
      result = Term::Dict.build do |commit|
        case node.axis
        in .x? then commit << :"x-stack"
        in .y? then commit << :"y-stack"
        end

        descriptions.each do |description|
          commit.concat(description.describe)
        end

        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Description.vantages(descriptions) }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : ZStack, box : OriginBox, hit : HitNode)
    descriptions = describe(cache, node.children, box.children, hit)

    describe = -> do
      info = node.info || ZInfo.new(Term.of(:"z-stack"), pairs: Term[])

      result = Term::Dict.build do |commit|
        commit << info.name
        descriptions.each do |description|
          commit.concat(description.describe)
        end

        annotate(commit, box)
        annotate(commit, hit)
        # Prefer client's pairs if annotate() happens to conflict.
        info.pairs.each_entry do |key, value|
          commit.with(key, value)
        end
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Description.vantages(descriptions) }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : Clip, box : OriginBox, hit : HitNode)
    descriptions = describe(cache, node.children, box.children, hit)

    describe = -> do
      content_rect = box.children.reduce(Rect.empty) do |memo, child|
        Rect.union(memo, child.bounds)
      end

      result = Term::Dict.build do |commit|
        commit << :clip
        commit.with(:"offset-x", describe(node.offset.x))
        commit.with(:"offset-y", describe(node.offset.y))

        unless node.aim_visible
          commit.with(:"aim-x", describe(node.aim.x))
          commit.with(:"aim-y", describe(node.aim.y))
        end

        commit.with(:"content-w", describe(content_rect.w))
        commit.with(:"content-h", describe(content_rect.h))

        max_offset = Point.max(Point[0, 0], content_rect.size - box.bounds.size)
        commit.with(:"max-offset-x", describe(max_offset.x))
        commit.with(:"max-offset-y", describe(max_offset.y))

        if goal = node.goal
          # Clamp goal offset.
          goal = Point.min(Point.max(Point[0, 0], goal), max_offset)

          # Calculate thumb values.
          # FIXME: division by zero
          thumb_dl = (goal.x / content_rect.w) * box.bounds.w
          thumb_dt = (goal.y / content_rect.h) * box.bounds.h
          thumb_w = (box.bounds.w / content_rect.w) * box.bounds.w
          thumb_h = (box.bounds.h / content_rect.h) * box.bounds.h

          commit.with(:"offset-x'", describe(goal.x))
          commit.with(:"offset-y'", describe(goal.y))
          commit.with(:"thumb-dl'", describe(thumb_dl))
          commit.with(:"thumb-dt'", describe(thumb_dt))
          commit.with(:"thumb-w'", describe(thumb_w))
          commit.with(:"thumb-h'", describe(thumb_h))
        end

        descriptions.each do |description|
          commit.concat(description.describe)
        end

        annotate(commit, box)
        annotate(commit, hit)
      end

      Slice[Term.of(result)]
    end

    vantages = -> { Description.vantages(descriptions) }

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : Vantage, box : OriginBox, hit : HitNode)
    descriptions = describe(cache, node.children, box.children, hit)

    # Vantages shadow their subtree's description.
    describe = -> do
      Slice[Term.of(:vantage, id: node.id)]
    end

    case {node.status, hit}
    in {.inactive?, _}
      vantages = -> { Description.vantages(descriptions) }

      return Slice[Description.new(describe, vantages)]
    in {.active_if_hit?, HitEmpty}
      vantages = -> { Description.vantages(descriptions, tail: {Term.of(:vantage, id: node.id)}) }

      return Slice[Description.new(describe, vantages)]
    in {.active?, _}, {.active_if_hit?, _}
    end

    vantages = -> do
      result = Term::Dict.build do |commit|
        commit << :vantage
        commit.with(:id, node.id)
        descriptions.each do |description|
          commit.concat(description.describe)
        end
      end

      # Add our own vantage to the list of vantages in the subtree.
      Description.vantages(descriptions, tail: {Term.of(result)})
    end

    Slice[Description.new(describe, vantages)]
  end

  private def describe!(cache, node : Gate, box : OriginBox, hit : HitNode)
    descriptions = describe(cache, node.children, box.children, hit)

    describe = -> { Slice(Term).empty }
    vantages = -> { Description.vantages(descriptions) }

    Slice[Description.new(describe, vantages)]
  end

  private def describe(cache, node : AimedNode, box : Box, hit : HitNode)
    describe(cache, node, OriginBox.new(box.bounds.size, box.children), hit)
  end

  private def describe(cache, nodes : Slice(AimedNode), boxes : Slice(Box), hit_parent : HitNode)
    descriptions = Pf::Kit.stack_array(Description, 8)

    nodes.zip(boxes, 0...nodes.size) do |node, box, index|
      case hit_parent
      in HitEmpty, HitLeaf
        # HitLeaf means the parent's own bounds were hit but none of its children
        # were, so we make each child's hit Empty.
        hit = HitEmpty.new
      in HitTextLeaf
        raise IndexError.new
      in HitGroup
        hit = hit_parent.children[index]
      end

      description = describe(cache, node, box, hit)
      descriptions.concat(description)
    end

    descriptions.to_unsafe_readonly_slice!
  end

  # Since we never promised to return precise values, let's round to be less
  # noisy. Ultimately, we can't be precise anyway, it's floats we're
  # talking about.
  def describe(m : Magnitude) : Term
    Term.of(m.round)
  end

  private def annotate(commit : Term::Dict::Commit, box : OriginBox) : Nil
    commit.with(:w, describe(box.bounds.w))
    commit.with(:h, describe(box.bounds.h))
  end

  private def annotate(commit : Term::Dict::Commit, hit : HitEmpty) : Nil
  end

  private def annotate(commit : Term::Dict::Commit, hit : HitLeaf | HitGroup) : Nil
    if hit.hover
      commit.with(:"hit-hover", true)
    end

    focus = hit.part.tl
    anchor = hit.part.br

    commit.with(:"hit-focus-l", describe(focus.x))
    commit.with(:"hit-focus-t", describe(focus.y))
    commit.with(:"hit-anchor-l", describe(anchor.x))
    commit.with(:"hit-anchor-t", describe(anchor.y))
  end

  private def annotate(commit : Term::Dict::Commit, hit : HitTextLeaf) : Nil
    if hit.hover
      commit.with(:"hit-hover", true)
    end

    focus = hit.part.tl
    anchor = hit.part.br

    commit.with(:"hit-focus-l", describe(focus.x))
    commit.with(:"hit-focus-t", describe(focus.y))
    commit.with(:"hit-anchor-l", describe(anchor.x))
    commit.with(:"hit-anchor-t", describe(anchor.y))

    hit_anchor, hit_span = rel_annotate(hit, hit.seln.expand, hit.seln)
    commit.with(:"hit-anchor", hit_anchor)
    commit.with(:"hit-focus", hit_anchor + hit_span)
    commit.with(:"hit-span", hit_span)
  end
end
