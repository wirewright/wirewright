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
  def self.uiR(replier : Term -> Term, rulebase : Term)
    cache = Cache(Term, Rewrite::Any).new(capacity: 2**16, preallocate: true)
    onceR = callR(PRIMITIVES)

    # First rewrite entries, then rewrite self.
    set, exhevalR = recR
    set.call chainR(entriesR(exhevalR), onceR)

    evalR = dfsR(
      switchR(
        { %[($ rewritee_)], exhevalR },
        { %[($once rewritee_)], onceR },
      )
    )

    set_backmapr, rec_backmapr = recR

    refR = dfsR(
      switchR(
        { %[($my rewritee←($ _))], chainR(rec_backmapr, envR(Term.of(:"$my"))) },
        { %[($my rewritee_)], envR(Term.of(:"$my")) },
        { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
        { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
      )
    )

    backmapR = set_backmapr.call chainR(refR, evalR)

    selector = ML.term(%[(%any° [rule pattern_ template_] [backmap pattern_ backspec_])])

    replierR = callR do |term|
      Rewrite.one(replier.call(term))
    end

    # recursive exhR
    set_main, rec_main = recR
    set_main.call(memoR(cache, exhR(choiceR(
      itemsR(rec_main),
      chainR(
        rulesetR(Ruleset.select(selector, rulebase), noR, backmapR, noR),
        replierR,
      ),
    ))))
  end
end

require "./soma/dwuir"
require "./soma/microfold"
