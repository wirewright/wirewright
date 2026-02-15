module Ww::Rack
  extend self

  # Returns a tree of nodes that look like Microfold nodes in *circuit*. When
  # rendered using Microfold, then uiR, followed by DwUIR, this lets you literally
  # *see* the circuit, as an image. Conversely, the circuit can define its image
  # appearance this way, by embedding nodes like `p`, `h1`, `button`, by annotating
  # `group`s with `style:`, etc.
  def visualize(circuit : Term, *, blacklist : Set(Term) = Set{Term.of(:rule), Term.of(:backmap)}, addrs : Bool = false) : Term
    D7.image(clf.call(circuit), circuit) do |addr, feature|
      case feature
      in D7::ParentImage
        Term.case(feature.parent.node) do
          matchpi %{{¦ style -addr}} do
            node = feature.node
            if addrs
              node = Term.morph(node, {:addr, addr})
            end

            Slice[Term.of(node)]
          end

          otherwise { feature.children }
        end
      in D7::Inert
        Term.case(feature.node) do
          matchpi %{(head_ _* ⍊ -addr)} do
            continue if head.in?(blacklist)

            node = feature.node
            if addrs
              node = Term.morph(node, {:addr, addr})
            end

            Slice[node]
          end

          otherwise do
            Slice(Term).empty
          end
        end
      in D7::Gnd
        Term.case(feature.node) do
          matchpi %{{¦ style -addr}} do
            node = feature.node
            if addrs
              node = Term.morph(node, {:addr, addr})
            end

            Slice[node]
          end

          otherwise { Slice(Term).empty }
        end
      end
    end
  end
end

require "./rack/classifier"
require "./rack/feed"
require "./rack/part"
require "./rack/tspace"
require "./rack/control_space"
require "./rack/pass"
