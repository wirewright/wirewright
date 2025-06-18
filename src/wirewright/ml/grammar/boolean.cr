module Ww::ML::Grammar
  module Boolean
    extend self

    private alias G = Grammar

    # Parses a boolean literal.
    G.rule(boolean, Term::Boolean) do
      P.choice(self.true, self.false)
    end

    # Parses boolean `true`.
    G.rule(true, Term::Boolean) do
      core = P.dseq(P.chrseq("true"), P.ahead(Symbol.nonsymbolic))

      P.map(core) { Term[true] }
    end

    # Parses boolean `false`.
    G.rule(false, Term::Boolean) do
      core = P.dseq(P.chrseq("false"), P.ahead(Symbol.nonsymbolic))

      P.map(core) { Term[false] }
    end
  end
end
