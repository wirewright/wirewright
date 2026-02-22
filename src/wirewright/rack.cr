module Ww::Rack
  extend self

  # Returns a tree of nodes that look like Microfold nodes in *circuit*.
  #
  # When rendered using Microfold, then uiR, followed by DwUIR, this lets you,
  # a human, literally *see* the circuit as an image -- as rectangles, text,
  # and so on.
  #
  # The circuit can define its appearance this way, by embedding nodes like
  # `p`, `h1`, `button`, arbitrarily; by annotating `group`s with `style:`s, etc.
  #
  # The circuit also has access to its visual reflection, via the `picture` node.
  # The picture node lets entities in the circuit "look" at the appearance of their
  # parts, track mouse movement, and so on -- effectively, they can reason about
  # the latest symbolic picture of themselves, as if looking into a mirror that shows
  # their visual appearancae; and how the world interacts with it. When you move
  # the mouse over a button, its symbolic picture updates; the button watches
  # for such updates, and modifies its "view" (the thing that, in turn, produces
  # the next picture).
  #
  # Pictures report read-only DwUIR extended with live high-level state such as mouse
  # position and so on (high-level because we're already well past, say, hit testing).
  # DwUIR is read-only because there is no point in making it writable. The feedback
  # loop is already closed: you can modify Microfold in response to changes in the symbolic
  # picture, which in turn changes the picture, and so on.
  def visualize(circuit : Term, *, blacklist : Set(Term) = Set{Term.of(:rule), Term.of(:backmap)}, strip : Set(Term) = Set(Term).new, addrs : Bool = false) : Term
    D7.image(clf.call(circuit), circuit) do |addr, feature|
      case feature
      in D7::ParentImage
        Term.case(feature.parent.node) do
          matchpi %{[picture @_ {¦ -addr}]} do
            assert child = feature.children.single?

            Slice[Term.morph(child, {:addr, addr})]
          end

          matchpi %{(window _* ⍊ -addr)} do
            Slice[Term.morph(feature.node, {:addr, addr})]
          end

          matchpi %{{¦ style}} do
            node = feature.node
            strip.each do |key|
              node = Term.morph(node, {key, nil})
            end

            Slice[Term.of(node)]
          end

          otherwise { feature.children }
        end
      in D7::Inert
        Term.case(feature.node) do
          matchpi %{[head_ _*]} do
            continue if head.in?(blacklist)

            node = feature.node
            strip.each do |key|
              node = Term.morph(node, {key, nil})
            end

            Slice[node]
          end

          otherwise do
            Slice(Term).empty
          end
        end
      in D7::Gnd
        Term.case(feature.node) do
          matchpi %{{¦ style}} do
            node = feature.node
            strip.each do |key|
              node = Term.morph(node, {key, nil})
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
