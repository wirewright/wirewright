module Ww
  # NOTE: `pattern` is provided for info, you will usually not need to match it;
  # the whole point of `Ruleset` and `M1::PatternSet` is that it matches it for you.
  module Rule
    extend self

    alias Any = Template | Backmap

    record Template, pattern : Term, body : Term
    record Backmap, pattern : Term, backspec : Term
  end

  class Ruleset
    # :nodoc:
    def initialize(@pset : M1::PatternSet(Term), @rules : Slice(Rule::Any))
    end

    # - Capture `template` in *selector* forms a template rule.
    # - Capture `backspec` in *selector* forms a backmap rule.
    def self.select(selector, *bases, **kwargs)
      rules = [] of Rule::Any

      pset = M1::PatternSet(Term).select(selector, *bases, **kwargs) do |normp, env|
        template = env[:template]?
        backspec = env[:backspec]?
        next if template && backspec # confused

        if template
          rules << Rule::Template.new(env[:pattern], template)
          next true # ok
        end

        if backspec
          rules << Rule::Backmap.new(env[:pattern], backspec)
          next true # ok
        end

        # skip
      end

      new(pset, rules.to_readonly_slice(&.itself))
    end

    # FIXME: It would make more sense for this to return `{Ruleset, Term}` instead, so
    # that we can pass through *base* if it's not a dict.
    def self.ruleset_and_rest(selector, base, **kwargs) : {Ruleset, Term::Dict}
      ruleset = self.select(selector, base)

      unless base.type.dict?
        return ruleset, Term[]
      end

      rest = base.pairspart.transaction do |commit|
        base.items.each do |item|
          next if M1.probe?(selector, item) # it is a rule

          commit << item
        end
      end

      {ruleset, rest}
    end

    DEFAULT_SELECTOR = ML.term("(%any° [rule pattern_ template_] [backmap pattern_ backspec_])")

    def self.select(base : Term)
      self.select(DEFAULT_SELECTOR, base)
    end

    def each_candidate(matchee : Term, & : M1::Op::Any, Rule::Any ->)
      @pset.each_candidate(matchee) do |op, index|
        yield op, @rules[index]
      end
    end

    def each_candidate_with_id(matchee : Term, & : {M1::Op::Any, Rule::Any}, UInt32 ->)
      @pset.each_candidate(matchee) do |op, index|
        yield({op, @rules[index]}, index)
      end
    end

    def query(matchee : Term, & : Indexable(Term::Dict), Rule::Any ->)
      @pset.query(matchee) do |envs, index|
        yield envs, @rules[index]
      end
    end

    def to_s(io)
      io << "Ruleset(<" << @rules.size << " rule(s)>)"
    end
  end
end
