module Ww::Scenery
  # See `Safe.describe`.
  def describe(scene : Scene, hit : HitNode) : {Term, Slice(Term)}
    upflow = describe(scene.root, scene.box, hit)

    node_desc = Term::Dict.build do |commit|
      commit << :scene
      commit.with(:width, scene.width)
      commit.with(:height, scene.height)
      commit.concat(upflow.nodes)
    end

    {Term.of(node_desc), upflow.observers}
  end

  # :nodoc:
  struct DescribeUpflow
    getter nodes : Slice(Term)
    getter observers : Slice(Term)
    getter observing : Slice(Term)

    def initialize(@nodes, @observers, @observing)
    end

    def self.empty : DescribeUpflow
      new(nodes: Slice(Term).empty, observers: Slice(Term).empty, observing: Slice(Term).empty)
    end

    def self.node(term : Term) : DescribeUpflow
      new(nodes: Slice[term], observers: Slice(Term).empty, observing: Slice(Term).empty)
    end
  end

  private def describe(root : Root(AimedNode), box : OriginBox, hit : HitNode)
    describe(root.node, box, Tf.new, hit)
  end

  private def describe(node : Inert, box : OriginBox, tf : Tf, hit : HitNode)
    DescribeUpflow.empty
  end

  private def describe(node : RectShape, box : OriginBox, tf : Tf, hit : HitNode)
    desc = Term::Dict.build do |commit|
      commit << :rect

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.node(Term.of(desc))
  end

  private def describe(node : Pending, box : OriginBox, tf : Tf, hit : HitNode)
    desc = Term::Dict.build do |commit|
      commit << :pending

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.node(Term.of(desc))
  end

  private def describe(node : Img, box : OriginBox, tf : Tf, hit : HitNode)
    desc = Term::Dict.build do |commit|
      commit << :img
      commit.with(:src, node.src.digest)

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.node(Term.of(desc))
  end

  private def describe(node : Svg, box : OriginBox, tf : Tf, hit : HitNode)
    desc = Term::Dict.build do |commit|
      commit << :svg
      commit.with(:src, node.src.digest)

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.node(Term.of(desc))
  end

  private def describe(node : IconGlyph, box : OriginBox, tf : Tf, hit : HitNode)
    desc = Term::Dict.build do |commit|
      commit << :icon
      commit.with(:font, node.font.digest)
      commit.with(:codepoint, node.codepoint)
      commit.with(:glyph, node.glyph_index)
      commit.with(:size, node.size)

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.node(Term.of(desc))
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

  private def describe(node : ShapedText, box : OriginBox, tf : Tf, hit : HitNode)
    desc = Term::Dict.build do |commit|
      commit << :text
      commit.with(:caption, node.caption.to_s)
      commit.with(:"line-h", describe(node.line_height))

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

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.node(Term.of(desc))
  end

  private def describe(node : TransformMatrix, box : OriginBox, tf : Tf, hit : HitNode)
    describe(node.children, box.children, Tf[tf, node.tf], hit)
  end

  private def describe(node : Floating | Limit | Content | Padding | Align | Composite | TransformMatrix, box : OriginBox, tf : Tf, hit : HitNode)
    describe(node.children, box.children, tf, hit)
  end

  private def describe(node : XYStack, box : OriginBox, tf : Tf, hit : HitNode)
    upflow = describe(node.children, box.children, tf, hit)

    desc = Term::Dict.build do |commit|
      case node.axis
      in .x? then commit << :"x-stack"
      in .y? then commit << :"y-stack"
      end

      commit.concat(upflow.nodes)

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.new(nodes: Slice[Term.of(desc)], observers: upflow.observers, observing: upflow.observing)
  end

  private def describe(node : ZStack, box : OriginBox, tf : Tf, hit : HitNode)
    upflow = describe(node.children, box.children, tf, hit)

    info = node.info || ZInfo.new(Term.of(:"z-stack"), pairs: Term[])

    desc = Term::Dict.build do |commit|
      commit << info.name
      commit.concat(upflow.nodes)

      annotate(commit, box, tf)
      annotate(commit, hit)

      # Prefer client's pairs if annotate() happens to conflict.
      info.pairs.each_entry do |key, value|
        commit.with(key, value)
      end
    end

    DescribeUpflow.new(nodes: Slice[Term.of(desc)], observers: upflow.observers, observing: upflow.observing)
  end

  private def describe(node : Clip, box : OriginBox, tf : Tf, hit : HitNode)
    upflow = describe(node.children, box.children, tf, hit)

    content_box = box.children.reduce(Rect.empty) do |memo, child|
      Rect.union(memo, child.bounds)
    end

    total = Point.max(box.bounds.size, content_box.size)

    desc = Term::Dict.build do |commit|
      commit << :clip

      x_start = node.offset.x / total.x
      x_end = (node.offset.x + box.bounds.w) / total.x

      y_start = node.offset.y / total.y
      y_end = (node.offset.y + box.bounds.h) / total.y

      commit.with(:"x-thumb-start", x_start.clamp(0.0..1.0))
      commit.with(:"x-thumb-end", x_end.clamp(0.0..1.0))

      commit.with(:"y-thumb-start", y_start.clamp(0.0..1.0))
      commit.with(:"y-thumb-end", y_end.clamp(0.0..1.0))

      commit.concat(upflow.nodes)

      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.new(nodes: Slice[Term.of(desc)], observers: upflow.observers, observing: upflow.observing)
  end

  private def describe(node : Observer, box : OriginBox, tf : Tf, hit : HitNode)
    upflow = describe(node.children, box.children, tf, hit)

    observer = Term::Dict.build do |commit|
      commit << :observer
      commit.with(:id, node.id)
      commit.concat(upflow.observing)
    end

    desc = Term.of(:observer, id: node.id)

    DescribeUpflow.new(nodes: upflow.nodes, observers: upflow.observers.append(Term.of(observer)), observing: Slice[Term.of(desc)])
  end

  private def describe(node : Observable, box : OriginBox, tf : Tf, hit : HitNode)
    upflow = describe(node.children, box.children, tf, hit)

    desc = Term::Dict.build do |commit|
      commit << :observable
      annotate(commit, box, tf)
      annotate(commit, hit)
    end

    DescribeUpflow.new(nodes: Slice[Term.of(desc)], observers: upflow.observers, observing: upflow.nodes)
  end

  private def describe(node : Gate, box : OriginBox, tf : Tf, hit : HitNode)
    upflow = describe(node.children, box.children, tf, hit)

    DescribeUpflow.new(Slice(Term).empty, upflow.observers, observing: Slice(Term).empty)
  end

  private def describe(node : AimedNode, box : Box, tf : Tf, hit : HitNode)
    describe(node, OriginBox.new(box.bounds.size, box.children), Tf[tf, Tf.translate(box.bounds.tl)], hit)
  end

  private def describe(nodes : Slice(AimedNode), boxes : Slice(Box), tf : Tf, hit_parent : HitNode)
    offspring_nodes = Pf::Kit.stack_array(Term, 8)
    offspring_observers = Pf::Kit.stack_array(Term, 8)
    offspring_observing = Pf::Kit.stack_array(Term, 8)

    (0...nodes.size).each do |index|
      node = nodes[index]
      box = boxes[index]

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

      upflow = describe(node, box, tf, hit)

      offspring_nodes.concat(upflow.nodes)
      offspring_observers.concat(upflow.observers)
      offspring_observing.concat(upflow.observing)
    end

    DescribeUpflow.new(
      offspring_nodes.to_unsafe_readonly_slice!,
      offspring_observers.to_unsafe_readonly_slice!,
      offspring_observing.to_unsafe_readonly_slice!,
    )
  end

  # Since we never promised to return precise values, let's round to be less
  # noisy. Ultimately, we can't be precise anyway, it's floats we're
  # talking about.
  private def describe(m : Magnitude) : Term
    Term.of(m.round)
  end

  private def annotate(commit : Term::Dict::Commit, box : OriginBox, tf : Tf) : Nil
    screen_bounds = tf.map(box.bounds)

    commit.with(:"screen-l", describe(screen_bounds.x))
    commit.with(:"screen-t", describe(screen_bounds.y))
    commit.with(:"screen-w", describe(screen_bounds.w))
    commit.with(:"screen-h", describe(screen_bounds.h))
    commit.with(:"layout-w", describe(box.bounds.w))
    commit.with(:"layout-h", describe(box.bounds.h))
  end

  private def annotate(commit : Term::Dict::Commit, hit : HitEmpty) : Nil
  end

  private def annotate(commit : Term::Dict::Commit, hit : HitLeaf | HitGroup) : Nil
    commit.with(:"hit-dl", describe(hit.part.x))
    commit.with(:"hit-dt", describe(hit.part.y))
    commit.with(:"hit-w", describe(hit.part.w))
    commit.with(:"hit-h", describe(hit.part.h))
  end

  private def annotate(commit : Term::Dict::Commit, hit : HitTextLeaf) : Nil
    commit.with(:"hit-dl", describe(hit.part.x))
    commit.with(:"hit-dt", describe(hit.part.y))
    commit.with(:"hit-w", describe(hit.part.w))
    commit.with(:"hit-h", describe(hit.part.h))

    hit_anchor, hit_span = rel_annotate(hit, hit.seln.expand, hit.seln)
    commit.with(:"hit-anchor", hit_anchor)
    commit.with(:"hit-span", hit_span)
  end
end
