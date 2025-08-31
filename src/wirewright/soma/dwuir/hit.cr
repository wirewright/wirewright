module Ww::Soma::DwUIR
  # :nodoc:
  #
  # Exceptions for control flow! Yay!!
  class FoundWord < Exception
    @callstack = CallStack.empty

    getter word

    def initialize(@word : TextDrawable::InlineString)
    end
  end

  # Variants of the positive response of `.hit?`.
  module Hit
    alias Any = Node | Text

    # A node was hit.
    #
    # - *keypath* specifies its keypath.
    # - *node* specifies the node itself, to enable convenient inspection
    #   without having to follow the keypath on the caller's end.
    record Node, keypath : Term::Dict, node : Term

    # A text node was hit.
    #
    # - *keypath* and *node* are the same as in `Node`.
    # - *char* is a string view pointing to the character under the hit point.
    #   It is guaranteed to point into the full caption of the text node. It is
    #   provided to enable access to positional information (e.g. byte index).
    # - *normpt* gives the normalized coordinates of where the hit point landed
    #   inside the target character's (logical) bounding box. For example,
    #   `0.5 < normpt.x <= 1` would mean the hit point landed on the right half of
    #   the character's bounding box. Coordinate range is guaranteed to be `0-1`.
    record Text, keypath : Term::Dict, node : Term, char : StringView, normpt : Point
  end

  # :nodoc:
  defcase Target,
    node : Term,
    tf : Tf,
    view : View,
    layer : LayerRank,
    origin : Point,
    keypath : Term::Dict

  private def target?(root : Term, point : Point) : Target?
    result = nil

    walk(root, keypath: Term[], viewport: Rect.inf) do |context, node|
      Term.case(node) do
        matchpi %[(_ ⍊ final-w final-h)] do
          tfbounds = context.tf.map(context.bounds)
          unless tfbounds.intersects?(context.view)
            next WalkFlow::Recurse
          end

          tfquad = context.tf.quad(context.bounds)
          unless tfquad.includes?(point)
            next WalkFlow::Recurse
          end

          target1 = Target.new(
            node: node,
            tf: context.tf,
            view: context.view,
            layer: context.layer,
            origin: context.bounds.tl,
            keypath: context.keypath || unreachable,
          )

          target0 = result # Copy into local
          if target0.nil? || target0.layer < target1.layer
            result = target1
          end

          WalkFlow::Next
        end

        otherwise do
          # We must recurse here in any case because DwUIR's tree is not necessarily
          # reflecting the true bounds of things. I.e., you can have a child
          # which is shifted by a big negative delta into view, whereas its parent
          # is out of view. While the parent cannot get hit by the point, the child
          # can, and we must be able to reach it.
          WalkFlow::Recurse
        end
      end
    end

    result
  end

  private def hit?(view : View, tf : Tf, bounds : Rect, point : Point) : Bool
    return false unless tf.map(bounds).intersects?(view)
    return false unless tf.quad(bounds).includes?(point)

    true
  end

  # Determines the sized leaf of *root* that lies under the given *point*. Returns
  # a `Hit::Any` object detailing the hit. Returns `nil` if no such leaf exists.
  #
  # *pencils* is required to serve pencils that are used to calculate the properties
  # associated with `Hit::Text`.
  #
  # Z-indices are taken into account. Only visible nodes are going to be considered.
  #
  # A *sized leaf* here means something like a rectangle or text node with known
  # width and height (`final-w`, `final-h`).
  #
  # Only sized leaves are inspected and can be hit. The resulting hit object includes
  # a keypath which you can use to get nodes along the path to the leaf (hit parents).
  def hit?(root : Term, pencils : PencilServer, point : Point) : Hit::Any?
    return unless target = target?(root, point)

    Term.case(target.node) do
      matchpi %[(text ⍊ final-w: ±w final-h: ±h)] do
        extent = Point[w.to(Float32), h.to(Float32)]

        continue unless spec = text_spec?(target.node, extent)
        continue unless font = spec.font?

        pencil = pencils.call(PencilRequest.new(font, spec.size, spec.leading, spec.tracking))

        begin
          spec.each_text_drawable(pencil, origin: target.origin) do |dw|
            next unless dw.is_a?(TextDrawable::InlineString)
            next unless hit?(target.view, target.tf, dw.bounds, point)

            raise FoundWord.new(dw)
          end
        rescue e : FoundWord
          word = e.word
          word.segments.each do |segment|
            case segment
            in TextDrawable::InlineSegment
              segment.view.each_char_with_abs_byte_index do |chr, byte_index|
                pencil, bounds = pencil.after_writing_with_bounds(chr)
                bounds = bounds.translate(word.bounds.tl)
                next unless hit?(target.view, target.tf, bounds, point)

                char = StringView.new(segment.view.string, byte_index, byte_index + 1, chr.single_byte?)
                worldpt = target.tf.inverse.map(point)
                normpt = bounds.normalize(worldpt)
                return Hit::Text.new(target.keypath, target.node, char, normpt)
              end
            in TextDrawable::VirtualSegment
              pencil = pencil.after_writing(segment.string)
            end
          end

          return Hit::Node.new(target.keypath, target.node)
        end
      end

      otherwise do
        Hit::Node.new(target.keypath, target.node)
      end
    end
  end
end
