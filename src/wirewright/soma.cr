# Wirewright µsoma is like a web browser, but for Wirewright.
#
# If you want a more precise definition, here it is; the idea with µsoma is that
# it is a big black box that takes event `Term`s as input, and produces bitmaps
# (huge pixel arrays) as output. Or, well, it manipulates *the* huge pixel
# array that you give it -- for performance.
#
# "The user", then, completes the feedback loop between the bitmaps and the input
# events. We treat the user here as a system capable of transforming bitmaps
# to input events.
#
# This module hosts everything related to µsoma, from GUI primitives to the µsoma
# application itself.
#
# Note how with bitmaps, we try to stay platform-independent here; in the sense
# that with Soma, we do not yet know what will do the window management and finally
# render us (in the sense of SFML vs. SDL vs. GLFW + OpenGL etc. or even PNG or JPEG!)
module Ww::Soma
end

require "./soma/dwuir"
require "./soma/microfold"
