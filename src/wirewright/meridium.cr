# Meridium is a subsystem of Wirewright responsible for interacting with
# the outside world via the network. It is the general abstraction Wirewright
# employs for networking.
#
# Clients and servers of the termspace, internal emergent entities such as
# `Xgraph`, surfaces (`Sensor`s & `Appearance`s), and lots of other network-
# related things are within the domain of Meridium.
module Ww::Meridium
  # Implementations can capture atoms coming from various sources such as `Utrie`,
  # `Xgraph`, etc.
  module IAtomAppend
    abstract def <<(atom : Atom)
  end

  # Implementations can be queried about the presence of `Atom`s.
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
