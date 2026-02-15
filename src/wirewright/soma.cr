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
  extend self

  # Constructs a new instance of a UIR rewriter, along with cache.
  #
  # NOTE: this function allocates a whole lot of cache; otherwise, UIR would be
  # incredibly slow. The cache is thread-unsafe, too -- so you must create an
  # instance of uiR per thread.
  #
  # TODO: Soma is a temporary home for the uiR rewriter. Since it is a general-
  # purpose layout engine, it is reused both by GUI and TUI stuff. However, uiR
  # by itself is no different from any other rewriter; when the rewriter circuit
  # machinery arrives, we'll have to think really hard about how to organize
  # everything while keeping uiR exposed to the internals of the system. In a sense,
  # uiR is a rewriter that is needed inside the system but at the same time, it is
  # implemented using that same system.
  def uiR(metricsR : Rewriter, ruleset : Ruleset)
    cache = SyncCache(Term, Rewrite::Any).new(capacity: 2**16, preallocate: true)

    set_main, rec_main = recR

    dictR = rejR(%{(guard _* ⍊ allow: {¦ -uir})}, itemsR(rec_main))

    mainR = memoR(cache,
      exhR(
        choiceR(
          dictR,
          chainR(
            alloy_rulesetR(ruleset),
            metricsR,
          ),
        )
      ),
      &.type.dict?
    )

    # recursive exhR
    set_main.call(mainR)
  end

  def uiR(metricsR : Rewriter, rulebase : Term)
    selector = ML.term(%{[backmap pattern_ backspec_]})
    ruleset = Ruleset.select(selector, rulebase)

    uiR(metricsR, ruleset)
  end

  def uiR(dw, rulebase : Term)
    ruleset = Ruleset.select(Ruleset::DEFAULT_SELECTOR, rulebase)

    metricsR = callR do |term|
      reply = Sync::Future(Term).new
      dw << DwUIR::GraphicsReplyRequest.new(term, reply)
      Rewrite.one(reply.get)
    end

    uiR(metricsR, rulebase)
  end

  def editR(ruleset : Ruleset)
    exhR(absR(alloy_rulesetR(ruleset)))
  end

  def editR(rulebase : Term)
    editR(Ruleset.select(Ruleset::DEFAULT_SELECTOR, rulebase))
  end

  def dispatch(state : Term, msg : Term)
    Term.each_keypath_and_node(state) do |keypath, node|
      Term.case(node) do
        matchpi %{[I _*]} do
          state = Term.assign(state, keypath, to: node.append(msg))

          false # no descend
        end

        otherwise do
          true # descend
        end
      end
    end

    state
  end
end
