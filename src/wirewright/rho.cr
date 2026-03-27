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

    private def leaf?(passable, impassable, input : Term) : Bool
      return true unless input.type.dict?

      return true if passable && passable.present? && !passable.probe?(input)
      return true if impassable && impassable.present? && impassable.probe?(input)

      false
    end

    # *Descending rewriter*: pre-order depth-first rewrite of a dictionary *part*.
    def descR(
      successor : Rewriter,
      part : Term::Dict::Part::Any = Term::Dict.itemspart,
      passable : M1::PatternSet(_)? = nil,
      impassable : M1::PatternSet(_)? = nil,
    ) : Rewriter
      finite(->(input : Term) { descR(successor, part, passable, impassable, input) })
    end

    private def descR(successor, part, passable, impassable, input : Term)
      if leaf?(passable, impassable, input)
        return successor.call(input)
      end

      rep = successor.call(input)
      if Term.changes?(input, after: rep)
        return rep
      end

      output = Term.flatten(input, part: part) do |_, value|
        descR(successor, part, passable, impassable, value)
      end

      Term.rep(output)
    end

    # *Ascending rewriter*: post-order depth-first rewrite of a dictionary *part*.
    def ascR(
      successor : Rewriter,
      part : Term::Dict::Part::Any = Term::Dict.itemspart,
      passable : M1::PatternSet(_)? = nil,
      impassable : M1::PatternSet(_)? = nil,
    ) : Rewriter
      finite(->(input : Term) { ascR(successor, part, passable, impassable, input) })
    end

    private def ascR(successor, part, passable, impassable, input : Term)
      if leaf?(passable, impassable, input)
        return successor.call(input)
      end

      output = Term.flatten(input, part: part) do |_, value|
        ascR(successor, part, passable, impassable, value)
      end

      successor.call(output)
    end

    # *Bidirectional rewriter*: first, attempts a pre-order rewrite with
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
      if leaf?(passable, impassable, input)
        return successor.call(input)
      end

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

    # *Exhaustive rewriter*: rewrite until no change. May not terminate.
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

    # *Chain rewriter*: rewrites with *a* and the result of that with *b*.
    def chainR(a : Rewriter, b : Rewriter) : Rewriter
      fn = ->(input : Term) do
        rep = a.call(input)

        Term.flatten(rep) do |offspring|
          b.call(offspring)
        end
      end

      Rewriter.new(fn, finite: a.finite? && b.finite?)
    end

    # :nodoc:
    SHORTHAND_ASCR = ML.term(<<-WWML)
    (ascR (rulesetR exh: true)
      passable: [passable pattern_]
      impassable: [impassable pattern_])
    WWML

    # :nodoc:
    SHORTHAND_DESCR = ML.term(<<-WWML)
    (descR (rulesetR exh: true)
      passable: [passable pattern_]
      impassable: [impassable pattern_])
    WWML

    # :nodoc:
    SHORTHAND_BIDIR = ML.term(<<-WWML)
    (bidiR (rulesetR exh: true)
      passable: [passable pattern_]
      impassable: [impassable pattern_])
    WWML

    private def dirR(spec : Term, data : Term, &) : Rewriter
      Term.matchpi(spec, %{[_ successor_]}) do
        if passable = spec[:passable]?
          passable_set = M1::PatternSet(Term).select(passable, data)
        end

        if impassable = spec[:impassable]?
          impassable_set = M1::PatternSet(Term).select(impassable, data)
        end

        yield rewriter(successor, data), passable_set, impassable_set
      end
    end

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

        # |@ rho.rulesetR
        #
        # |@pattern
        # (rulesetR ⍊ ⋮selector ⋮discriminator ⋮section exh⋮ false)
        #
        # |@key selector m1.operator
        # The selector pattern. The captures this pattern makes determine the kind
        # of rule. Namely:
        #
        # - If the pattern captures `pattern` and `template`, the ruleset creates
        #   a *template rule*. The ruleset interprets the captured `pattern` as
        #   an `m1.operator`, and the `template` as `alloy.template`.
        #
        # - If the pattern captures `pattern` and `backspec`, the ruleset creates
        #   a *backmap rule* (or simply a *backmap*). The ruleset interprets *pattern*
        #   as an `m1.operator`, and *backspec* as an `m1.backspec`.
        #
        # Other captures are ignored. If you try to confuse the ruleset by capturing
        # all of *pattern*, *template*, and *backspec*, the ruleset will refuse to
        # create the rule.
        #
        # |@key discriminator
        # The selector is wrapped in a dict pattern with *discriminator* as its head.
        #
        # For example, `(rulesetR selector: (rule pattern_ template_) discriminator: main)`
        # is the same as writing `(rulesetR selector: (rule [main pattern_] template_))`.
        #
        # *discriminator* is most useful when you want to leave the selector out, that is,
        # use the default selector. The default selector is rather lengthy, so it's
        # nice to be able to avoid typing it out if you only want to wrap it.
        #
        # *discriminator* is used to store multiple distinct rulesets in the same rulebase
        # term. This is most useful in Rack's `rack.rewriter` node, which only allows you
        # to specify one rulebase.
        #
        # |@key section
        # Defines the section of the rulebase where the ruleset must search for rules.
        # Sections are most useful with standalone rewriters that are defined as parts
        # of a document. Consider, for instance, the following ML document:
        #
        # ```
        # --- main
        # a => 100
        # b => 200
        #
        # --- calculate
        # (±a ±b) => ^(+ a b)
        #
        # --- rewriter
        # (chainR
        #   (rulesetR section: main)
        #   (rulesetR section: calculate))
        # ```
        #
        # Notice how we use *section* to refer to sections of the same document
        # the rewriter is in. By convention, Rho front-ends search for the `rewriter`
        # section in any dict they receive. When found, a front-end passes the entire
        # document to the rewriter. Thus, for instance, a rewriter may access itself.
        # Writing `(rulesetR section: rewriter)` in the above makes perfect sense;
        # there is no need to separate the rules from the rewriter provided the overall
        # scheme makes sense:
        #
        # ```
        # --- rewriter
        # a => 100
        # b => 200
        # (±a ±b) => ^(+ a b)
        #
        # (ascR (rulesetR section: rewriter))
        # ```
        #
        # IMPORTANT: The rewriter spec must be located at the very end of the `rewriter`
        # section for the above to work.
        #
        # |@key exh
        # Enables or disables *exhaustive rule application semantics*. That is,
        # normally, at rewrite-time, the ruleset receives a term. It finds the most
        # specific rule that matches the term, and rewrites the term using that rule.
        # If the rule changed nothing, the ruleset resumes search until it had
        # seen all rules. If the rule changed the term, the ruleset stops. That's
        # with *exh* set to `false`. If *exh* is set to `true`, the ruleset will
        # restart its search after a change. Importantly, the ruleset will ignore
        # all rules it has matched before. Thus, even though the rewrite is exhaustive,
        # it is guaranteed to terminate (versus, say, `rho.exhR`). That is, at
        # some point you will run out of rules.
        #
        # |@block
        # Constructs a *ruleset rewriter*: a rewriter that finds and applies rules
        # from a *rulebase* to input terms.
        matchpi %{(rulesetR ⍊ exh⋮ false)} do
          selector = spec[:selector]? || Ruleset::DEFAULT_SELECTOR

          discriminator = spec[:discriminator]?

          if section = spec[:section]?
            continue unless ruledoc = data.as_d?
            continue unless rulebase = ruledoc[section]?
          else
            rulebase = data
          end

          rulesetR(Ruleset.select(selector, rulebase, discriminator: discriminator), exh: exh.true?)
        end

        matchpi %{[exhR successor_]} do
          exhR(rewriter(successor, data))
        end

        matchpi %{[ascR _]} do
          dirR(spec, data) do |successorR, passable_set, impassable_set|
            ascR(successorR, passable: passable_set, impassable: impassable_set)
          end
        end

        matchpi %{[descR _]} do
          dirR(spec, data) do |successorR, passable_set, impassable_set|
            descR(successorR, passable: passable_set, impassable: impassable_set)
          end
        end

        matchpi %{[bidiR _]} do
          dirR(spec, data) do |successorR, passable_set, impassable_set|
            bidiR(successorR, passable: passable_set, impassable: impassable_set)
          end
        end

        matchpi %{[chainR head_ successors_*]} do
          successors.items.reduce(rewriter(head, data)) do |memoR, successor|
            chainR(memoR, rewriter(successor, data))
          end
        end

        otherwise do
          noR
        end
      end
    end

    REWRITER_CACHE = SyncLRU({Term, Term}, Rewriter).new(capacity: 32)

    def rewriter(spec : Term, data : Term) : Rewriter
      REWRITER_CACHE.put_if_absent({spec, data}) do
        rewriter!(spec, data)
      end
    end

    # TODO: add support for rules / rewriter definitions.
    def rewriter(document : Term) : Rewriter
      return noR unless dict = document.as_d?
      return noR unless section = dict[:rewriter]?
      return noR unless spec = section.items.last?

      rewriter(spec, document)
    end
  end
end
