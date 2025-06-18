module Ww::ML::Grammar
  # Boolean literal rules.
  module Boolean
    extend self

    private alias G = Grammar

    G.rule(boolean, Term::Boolean, comment: "Parses a boolean literal.") do
      P.choice(self.true, self.false)
    end

    G.rule(true, Term::Boolean, comment: "Parses boolean `true`.") do
      core = P.dseq(P.chrseq("true"), P.ahead(Symbol.nonsymbolic))

      P.map(core) { Term[true] }
    end

    G.rule(false, Term::Boolean, comment: "Parses boolean `false`.") do
      core = P.dseq(P.chrseq("false"), P.ahead(Symbol.nonsymbolic))

      P.map(core) { Term[false] }
    end
  end
end
