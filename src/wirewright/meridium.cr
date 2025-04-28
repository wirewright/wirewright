# Meridium is a subsystem of Wirewright responsible for interacting with
# the outside world via the network. It is the general abstraction Wirewright
# employs for networking.
#
# Clients and servers of the termspace, termbase, internal objects such as
# `Xgraph`, surfaces (sensors & appearances), the underlying maps, and everything
# else network-related is the domain of Meridium.
module Ww::Meridium
  # Implementations can capture atoms coming from various sources such as `Utrie`,
  # `Xgraph`, etc.
  module IAtomAppend
    abstract def <<(atom : Atom)
  end

  # Implementations can be queried for the presence of certain `Atom`s.
  module IAtomsPresent
    # Returns a BitList indicating whether each atom, rendered from *objects*
    # via the block, exists.
    #
    # Guarantees the resulting bit list matches the size of *objects*.
    abstract def present?(objects : Enumerable(T), & : T -> Atom | Enumerable(Atom)) : BitList forall T
  end
end

require "./meridium/atom"
require "./meridium/bytes_multimap"
require "./meridium/xgraph"
require "./meridium/sensor_registry"
require "./meridium/surface"
