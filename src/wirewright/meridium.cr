# Meridium is a subsystem of Wirewright responsible for interacting with
# the outside world via the network. It is the general abstraction Wirewright
# employs for networking.
#
# Clients and servers of the termspace, termbase, internal objects such as
# `Xgraph`, surfaces (sensors & appearances), the underlying maps, and everything
# else network-related is the domain of Meridium.
module Ww::Meridium
end

require "./meridium/map"
require "./meridium/ubase"
require "./meridium/utrie"
require "./meridium/xgraph"
require "./meridium/tbase"
require "./meridium/tsetconn"
