module Ww::ML::Grammar
  # Boolean literal rule constructors.
  module Boolean
    extend self

    # Parses a boolean literal.
    #
    # *symbolic* is used to test whether the boolean is actually a symbol
    # prefix or not (as in `⏏true⏏s` or `⏏false⏏s`).
    def boolean(symbolic : P::Pi(_)) : P::Pi
      P.choice(
        self.true(symbolic),
        self.false(symbolic),
      )
    end

    # Parses boolean `true`.
    #
    # *symbolic* is used to test whether the boolean is actually a symbol
    # prefix or not (as in `⏏true⏏s` or `⏏false⏏s`).
    def true(symbolic : P::Pi(_)) : P::Pi
      core = P.dseq(P.chrseq("true"), P.ahead(P.not(symbolic)))

      P.map(core) { Term[true] }
    end

    # Parses boolean `false`.
    #
    # *symbolic* is used to test whether the boolean is actually a symbol
    # prefix or not (as in `⏏true⏏s` or `⏏false⏏s`).
    def false(symbolic : P::Pi(_)) : P::Pi
      core = P.dseq(P.chrseq("false"), P.ahead(P.not(symbolic)))

      P.map(core) { Term[false] }
    end
  end
end
