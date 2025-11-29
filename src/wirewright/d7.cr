# Wirewright Delta7 (D7 for short) is a *symbolic physics engine*. In a sense,
# it is just like a *physics engine* (think Box2D), but instead of working with
# bodies, it works with *symbols*. Rather than solving equations, D7 searches for
# relationships between symbols according to constraints. Instead of manipulating
# velocity and position, D7 *rewrites*.
#
# At its core, D7 is an attempt to model *autopoiesis* as described by Maturana,
# Varela, and others.
#
# See, for instance, *Autopoiesis: the organization of living systems, its
# characterization and a model* by Varela, Maturana & Uribe (1974). D7 is trying
# to check all the boxes in section 9, "Key".
#
# I think autopoiesis can be modeled in any "physics simulator". The only problem
# is that in practice, the physics simulators we build are too unstructured. It's
# "perceptually hard" to extract useful info from it, both for us as observers and
# for the entities within the simulation. Imagine how much intrinsic structure
# a particle simulator would require to start recognizing or matching on its own
# configuration or its parts? With D7, it's as simple as a pattern match on a fragment.
#
# Imagine a game. How hard would it be to make a car drive itself in that game, given
# only the game's visuals as output and keyboard press states as input -- that is,
# "from an outside agent's point of view"?
#
# We know the answer: very hard. That's why people resort to black box (ish) methods
# like neural networks. In the worst case, you'd need a human -- an intricate
# apparatus indeed.
#
# If only we had a *symbolic* physics simulator, with the same or similar kinds
# of behaviors, but with structures easy to pattern match and construct
# programmatically, "from an outside agent's point of view"...
#
# In fact, what D7 does, at its core, is it attempts to internalize observation and
# intervention. Normally, that's what humans do: they observe how their programs behave,
# intervene, and sometimes change the behavior. The program itself cannot do that, not really.
# D7, on the other hand, lets the program observe and intervene, too. At least in terms of
# "ways of influence", D7 places the program and the programmer on equal footing; what one
# can do the other can, and vice versa. The above is, really, an implementation detail
# necessary to support this "embedding": symbolic worlds and pattern matching for
# interpretability, rewriting, etc.
#
# With D7, a *symbol* is an identity or a composition thereof. Such symbols are
# represented meaningfully with `Term`s.
#
# D7 programs -- called *circuits* -- form a hypergraph. A hypergraph is a graph
# whose edges -- hyperedges -- are *sets*. You can imagine a hyperedge as a group.
# Each node in a hypergraph participates in zero or more such groups.
#
# In D7, there is no difference between *running* a circuit and *building* it. There
# is no "runtime", nor is there "compile-time". D7 is more like a game, which you
# can pause, save, and return to in the future. Since D7 circuits are persistent
# and immutable (they are `Term`s), you get time travel for free, too, which is
# very useful for debugging. Branching and other features come for free, too
# (think Git or rather, something crude and Git-like).
#
# A *D7 engine* to a D7 circuit is like a browser to a web page.
#
# D7 introduces the notion of *entanglement*. Entanglement is how D7 circuits interact
# with the outside world. The circuit may include symbolic objects recognized
# by the engine. Those objects are synced by the engine to their "outside-world"
# counterparts. Such objects are called *percepts* (internal, inbound representations
# of outside-world entities) and *effects* (internal, outbound representations of
# outside-world actions or transformations).
#
# With entanglement, D7 lets you access files, communicate with processes,
# build server, graphical, and terminal apps and so on.
#
# Alongside edges, D7 also has *surfaces*: *sensors* and *appearances*.
# A sensor senses zero or more appearances. An appearance excites zero or more
# sensors. Surfaces live in a *termspace*. D7 circuits can include zero or more
# termspaces. A termspace can be local or global. A local termspace is bounded by
# the circuit. A global termspace is either circuit-global or remote. A remote
# termspace is like a multiplayer game, where each sensor and appearance is a tiny
# "player" and the termspace itself is like a world (think Minecraft).
#
# Surfaces complement hyperedges in that hyperedges are hard-coded connectivity
# (even if dynamically generated, especially with the help of D7 modules); whereas
# for surfaces, whether they are "connected" is highly dynamic and depends
# on the content itself.
#
# D7 circuits are graphs whose edges are *sets*; D7 termspaces are graphs whose
# edges are *functions*, or more specifically, *predicates*.
module Ww::D7
  extend self
end

require "./d7/hypergraph"
require "./d7/feature"
require "./d7/regime"
require "./d7/step"
