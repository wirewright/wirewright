{% skip_file unless flag?(:mxrack) %}

# |@ mxrack
#
# |@summary
# Media extensions for Rack.
#
# |@block
# MxRack is short for **M**edia e**x**tensions for **Rack**. MxRack is a collection
# of nodes that seamlessly extend `rack` with the ability to manage windows, do interactive
# graphics using `microfold` and `scenery`, and so on. In the future, MxRack will also
# contain nodes for working with and synthesizing audio; and perhaps some other nodes.
#
# MxRack uses SDL3, and so is a rather heavy binary to distribute. We use AppImages
# to relieve you of most work of preparing your system for Wirewright. Rack, on
# the other hand, through `irack`, can be distributed as a static binary.
#
# I am telling you this to highlight the notable difference between the two, `mxrack` and
# `irack`. MxRack is relatively heavy and depends on the availability of SDL3, graphics,
# interactivity, and so on; and `irack` is lightweight, and -- at least in theory -- can
# be run on low-performance machines and servers, among other things. (I wouldn't
# recommend doing so right now, though, because even `irack` is probably too inefficient
# for small devices, especially in terms of memory usage. But the intended in-theory
# usage for both is as described.)
#
# The nodes listed here are specific to MxRack. Any Rack node is also an MxRack
# node. Go to `rack` to see docs for Rack nodes, which apply to MxRack too.

# Media extensions for Rack. See `mxrack` in the doctool.
module Ww::MxRack
  extend self

  alias MediaProgramQueue = BlockingQueue(MediaProgram)
  alias MediaProgram = WindowProgram
end

require "./mxrack/classifier"
require "./mxrack/input"
require "./mxrack/mouse"
require "./mxrack/visualize"
require "./mxrack/adjust"
require "./mxrack/window"
require "./mxrack/sdl_loop"
require "./mxrack/textoscope"
require "./mxrack/event"
require "./mxrack/run"
