module Ww::Rack
  extend self

  defrecord Cell, node : D7::Node, value : Term?, smart: true

  def cell?(hg : D7::Hypergraph, input : D7::AbsEdge) : Cell?
    candidates = Pf::Kit.stack_array(Cell, 1)
    hg.each_node_with_head(Term.of(:cell), memberof: {input}) do |node|
      # Since cell has only one edge, `memberof:` above already covers
      # the edge check.
      Term.case(node.term) do
        matchpi %{[cell @_ value_]} do
          candidates << Cell.new(node, value)
        end

        matchpi %{[cell @_]} do
          candidates << Cell.new(node, value: nil)
        end
      end
    end

    candidates.single?
  end
end

require "./rack/classifier"
require "./rack/feed"
require "./rack/part"
require "./rack/tspace"
require "./rack/assembler"
require "./rack/pass"
require "./rack/parser"
require "./rack/extrinsics"
require "./rack/database"
require "./rack/web_socket"
require "./rack/automaton"
