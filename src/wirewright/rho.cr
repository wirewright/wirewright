module Ww
  # Wirewright Rho is a set of composable rewriters. Think parser combinators,
  # but for term rewriting.
  module Rho
    extend self

    struct Rewriter
      alias Fn = Term -> Term::Rep

      # Returns `true` if this rewriter is *finite*.
      #
      # A finite rewriter is one which, at rewriter compile-time, can be proven
      # to terminate. We simply associate each rewriter kind (such as `constR`,
      # `ascR` and so on) with finite-ness; for example, `exhR` is unconditionally
      # infinite. Nothing smart is done here.
      getter? finite : Bool

      # :nodoc:
      def initialize(@fn : Fn, @finite)
      end

      def flat_map(& : Fn -> Rewriter) : Rewriter
        element = yield @fn

        Rewriter.new(element.@fn, @finite && element.@finite)
      end

      def call(input : Term) : Term::Rep
        @fn.call(input)
      end
    end

    private def finite(fn)
      Rewriter.new(fn, finite: true)
    end

    private def infinite(fn)
      Rewriter.new(fn, finite: false)
    end

    def noR : Rewriter
      finite(->(input : Term) { Term.rep(input) })
    end

    def constR(rep : Term::Rep) : Rewriter
      finite(->(input : Term) { rep })
    end

    # - Outside of *exh*austive mode, applies the most specific matching rule.
    # - In *exh*austive mode, every rule is given an opportunity to match and
    #   transform the workspace. Rules are sorted by specificity; the most specific
    #   rule is preferred. Rules are *expended*, meaning a rule can only run once.
    #   This means that exhaustive rulesetR is guaranteed to terminate, since at
    #   some point we'll exhaust the entire ruleset.
    def rulesetR(ruleset : Ruleset, exh : Bool = false) : Rewriter
      if exh
        finite(exh_rulesetR(ruleset))
      else
        finite(first_rulesetR(ruleset))
      end
    end

    private def first_rulesetR(ruleset : Ruleset)
      ->(input : Term) do
        ruleset.each_candidate(input) do |pattern, rule|
          case rule
          in Rule::Template
            next unless env = M1.match?(Term[], pattern, input)

            rep, _ = Alloy.render0(env, rule.body, severity: :quiet)
          in Rule::Backmap
            next unless rep = M1.backmapR?(pattern, rule.backspec, input)
          end

          if Term.changes?(input, after: rep)
            return rep
          end
        end

        Term.rep(input)
      end
    end

    private def exh_rulesetR(ruleset : Ruleset) : Rewriter::Fn
      ->(input : Term) do
        seen = Pf::USet32.new
        workspace = Term.rep(input)

        running = true
        loop do
          running = false

          workspace = Term.flatten(workspace) do |offspring|
            result = nil

            ruleset.each_candidate_with_id(offspring) do |(pattern, rule), id|
              next if seen.includes?(id)

              case rule
              in Rule::Template
                next unless env = M1.match?(Term[], pattern, offspring)

                rep, _ = Alloy.render0(env, rule.body, severity: :quiet)
              in Rule::Backmap
                next unless rep = M1.backmapR?(pattern, rule.backspec, offspring)
              end

              next unless Term.changes?(offspring, after: rep)

              result = rep
              running = true
              seen = seen.add(id)
              break
            end

            result || Term.rep(offspring)
          end

          break unless running
        end

        workspace
      end
    end

    # *Descending rewrite*: pre-order depth-first rewrite of a dictionary *part*.
    def descR(
      successor : Rewriter,
      part : Term::Dict::Part::Any = Term::Dict.itemspart,
      passable : M1::PatternSet(_)? = nil,
      impassable : M1::PatternSet(_)? = nil,
    ) : Rewriter
      finite(->(input : Term) { descR(successor, part, passable, impassable, input) })
    end

    private def descR(successor, part, passable, impassable, input : Term)
      unless dict = input.as_d?
        return successor.call(input)
      end

      return Term.rep(input) if passable && !passable.probe?(input)
      return Term.rep(input) if impassable && impassable.probe?(input)

      rep = successor.call(input)
      if Term.changes?(input, after: rep)
        return rep
      end

      output = Term.flatten(dict, part: part) do |_, value|
        descR(successor, part, passable, impassable, value)
      end

      Term.rep(Term.of(output))
    end

    # *Ascending rewrite*: post-order depth-first rewrite of a dictionary *part*.
    def ascR(
      successor : Rewriter,
      part : Term::Dict::Part::Any = Term::Dict.itemspart,
      passable : M1::PatternSet(_)? = nil,
      impassable : M1::PatternSet(_)? = nil,
    ) : Rewriter
      finite(->(input : Term) { ascR(successor, part, passable, impassable, input) })
    end

    private def ascR(successor, part, passable, impassable, input : Term)
      return successor.call(input) unless input.type.dict?

      return Term.rep(input) if passable && !passable.probe?(input)
      return Term.rep(input) if impassable && impassable.probe?(input)

      output = Term.flatten(input, part: part) do |_, value|
        ascR(successor, part, passable, impassable, value)
      end

      successor.call(output)
    end

    # *Bidirectional rewrite*: first, attempts a pre-order rewrite with
    # *successor*; if no change, then applies recursively to *part*, if
    # possible; if the rewritten version changed, attempts to apply
    # *successor* again.
    def bidiR(
      successor : Rewriter,
      part : Term::Dict::Part::Any = Term::Dict.itemspart,
      passable : M1::PatternSet(_)? = nil,
      impassable : M1::PatternSet(_)? = nil,
    ) : Rewriter
      finite(->(input : Term) { bidiR(successor, part, passable, impassable, input) })
    end

    private def bidiR(successor, part, passable, impassable, input : Term)
      unless input.type.dict?
        return successor.call(input)
      end

      return Term.rep(input) if passable && !passable.probe?(input)
      return Term.rep(input) if impassable && impassable.probe?(input)

      rep = successor.call(input)
      if Term.changes?(input, after: rep)
        return rep
      end

      output = Term.flatten(input, part: part) do |_, value|
        bidiR(successor, part, passable, impassable, value)
      end

      if input == output
        return Term.rep(input)
      end

      successor.call(output)
    end

    def exhR(successor : Rewriter) : Rewriter
      infinite(->(input : Term) { exhR(input, successor) })
    end

    private def exhR(input : Term, successor)
      loop do
        rep = successor.call(input)
        if rep.empty?
          return rep
        end

        if offspring = rep.single?
          if input == offspring
            return rep # Done
          end

          input = offspring # tail call
          next
        end

        return Term.flatten(rep) { |offspring| exhR(offspring, successor) }
      end
    end

    # :nodoc:
    SHORTHAND_ASCR = Term.of(:ascR, Term.of(:rulesetR, exh: true))
    # :nodoc:
    SHORTHAND_DESCR = Term.of(:descR, Term.of(:rulesetR, exh: true))
    # :nodoc:
    SHORTHAND_BIDIR = Term.of(:bidiR, Term.of(:rulesetR, exh: true))

    def rewriter!(spec : Term, data : Term) : Rewriter
      Term.case(spec) do
        matchpi %{noR} do
          noR
        end

        matchpi %{ascR} do
          rewriter(SHORTHAND_ASCR, data)
        end

        matchpi %{descR} do
          rewriter(SHORTHAND_DESCR, data)
        end

        matchpi %{bidiR} do
          rewriter(SHORTHAND_BIDIR, data)
        end

        matchpi %{[constR offspring_*]} do
          constR(Term.rep(offspring.items))
        end

        matchpi %{(rulesetR ⍊ exh⋮ false)} do
          rewriter(Term.morph(spec, {1, Ruleset::DEFAULT_SELECTOR}), data)
        end

        matchpi %{(rulesetR selector_ ⍊ exh⋮ false)} do
          ruleset = Ruleset.select(selector, data)

          rulesetR(ruleset, exh: exh.true?)
        end

        matchpi %{[exhR successor_]} do
          exhR(rewriter(successor, data))
        end

        matchpi %{[ascR successor_]} do
          ascR(rewriter(successor, data))
        end

        matchpi %{[descR successor_]} do
          descR(rewriter(successor, data))
        end

        matchpi %{[bidiR successor_]} do
          bidiR(rewriter(successor, data))
        end

        otherwise do
          noR
        end
      end
    end

    REWRITER_CACHE = SyncCache({Term, Term}, Rewriter).new(capacity: 32, preallocate: true)

    def rewriter(spec : Term, data : Term) : Rewriter
      REWRITER_CACHE.put_if_absent({spec, data}) do
        rewriter!(spec, data)
      end
    end
  end
end
