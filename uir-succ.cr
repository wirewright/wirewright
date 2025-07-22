module Ww::Soma::UIR
  extend self

  @@cache = SyncCache(Term, Rewrite::Any).new(capacity: 2**16, preallocate: true)

  # Returns the UIR rewriter.
  #
  # TODO: move to `uiR.soma.wwml` once the rewriter DSL is available.
  def self.rewriter(platform : DwUIR::Platform) : Rewriter
    base_main = File.read(RESOURCES / (ENV["RSET"]? || "uiR-succ8.soma.wwml"))

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

    dwuirR = callR do |term|
      Rewrite.one(DwUIR.reply(platform, term))
    end

    # recursive exhR
    set_main, rec_main = recR
    set_main.call(memoR(@@cache, exhR(choiceR(
      itemsR(rec_main),
      chainR(
        rulesetR(Ruleset.select(selector, ML.terms(base_main)), noR, backmapR, noR),
        dwuirR,
      ),
    ))))
  end
end
