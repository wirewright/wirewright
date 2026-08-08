# |@ rack
#
# |@summary
# A rewrite regime.
#
# |@block
# Wirewright Rack is a particular instance of symbolic physics. In other words,
# it is a *rewrite regime*, implemented using Wirewright D7. The latter is a kind
# of symbolic physics construction toolkit; a "lego" for constructing rewrite regimes.
# Rack is one such regime.
#
# A symbolic world in Rack is called a *circuit*. A *circuit* consists of nodes.
# Nodes can be *leaves* (such as `rack.cell`), parents (e.g. `rack.group`),
# or both (`rack.circuit`). The reason the name *circuit* is used is purely
# historical. I am actually considering renaming it to *world*. But that's quite
# a huge rename.
#
# Instead of evaluating circuits, Rack & D7 *simulate* their *evolution*, like
# you can simulate the evolution of a cellular automaton such as [Conway's
# Game of Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life).
#
# Rack circuits can contain *subcircuits* (`rack.circuit`, `rack.node`, `rack.device`, etc.)
# Evolution (time) always runs *top-down*: host circuits are always *time-stepped*
# (evolved into the future by one step) **before** subcircuits. The reasoning for
# this is the same as in simulation. We consider subcircuits to be *embedded* in, and
# *simulated* by, the host circuit. As the host circuit must run in order for
# the subcircuit to run, the order of evolution is naturally top-down.
#
# For a start, consider looking at simpler, less "philosophically heavy" nodes such
# as: `rack.cell`, `rack.feed`, `rack.parser`, `rack.path`.
#
# |@example
# The following is the "Hello World" of Rack -- an oscillator.
#
# ```wwml
# (cell @x 0)
# (cell @y)
# (feed @x @y @x)
# ```
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

  defrecord Pool, node : D7::Node, contents : Term::Dict

  def pool?(hg : D7::Hypergraph, edge : D7::AbsEdge) : Pool?
    pools = Pf::Kit.stack_array(Pool, 1)
    hg.each_node_with_head(Term.of(:pool), memberof: {edge}) do |node|
      Term.matchpiT?(node.term, %{[pool @_ contents_dict]}) do
        pools << Pool.new(node, contents)
      end
    end

    pools.single?
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
require "./rack/schema"
require "./rack/database"
require "./rack/web_socket"
require "./rack/supervisor"
require "./rack/automaton"
