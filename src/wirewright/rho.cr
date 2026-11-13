module Ww
  # |@ rho
  #
  # |@summary
  # A collection of composable rewriters.
  #
  # |@block
  # Wirewright Rho is a collection of composable rewriters. They are like parser
  # combinators, but for term rewriting.
  #
  # |@example
  # ```wwml
  # (chainR
  #   (adjR (rulesetR phase: move))
  #   (adjR (rulesetR phase: look exh: true)))
  # ```
  module Rho
    extend self

    alias RewriterId = UInt64
    alias IRewriteCache = ICache({RewriterId, Term}, Term::Rep)

    defrecord RewriteAttachments,
      id : RewriterId,
      cache : IRewriteCache,
      ping : ->

    # Represents a Rho rewriter.
    struct Rewriter
      # :nodoc:
      alias Fn = RewriteAttachments, Term -> Term::Rep

      # Returns `true` if this rewriter is *finite*.
      #
      # A finite rewriter is one which, at rewriter compile-time, can be proven
      # to terminate. We simply associate each rewriter kind (such as `constR`,
      # `ascR` and so on) with finite-ness; for example, `exhR` is unconditionally
      # infinite. Nothing smart is done here.
      getter? finite : Bool

      @@ids = Atomic(UInt64).new(0u64)

      # Returns the process-unique id of this rewriter.
      getter id : RewriterId

      # :nodoc:
      def initialize(@fn : Fn, @finite)
        @id = @@ids.add(1, :relaxed)
      end

      # Rewrites the given input term using this rewriter.
      #
      # Some rewriters support caching. This is why *cache* must be passed.
      #
      # NOTE: This is mostly for internal use, or if you want a `Term::Rep`; prefer
      # `Rho.rewrite` otherwise.
      def call(input : Term, cache : IRewriteCache, ping : ->) : Term::Rep
        @fn.call(RewriteAttachments.new(@id, cache, ping), input)
      end
    end

    private def finite(&fn : Rewriter::Fn)
      finite(fn)
    end

    private def finite(fn : Rewriter::Fn)
      Rewriter.new(fn, finite: true)
    end

    private def infinite(&fn : Rewriter::Fn)
      infinite(fn)
    end

    private def infinite(fn : Rewriter::Fn)
      Rewriter.new(fn, finite: false)
    end

    # *Noop rewriter*. See `rho.noR`.
    def noR : Rewriter
      finite do |attachments, input|
        attachments.ping.call
        Term.rep(input)
      end
    end

    # *Constant rewriter*. See `rho.constR`.
    def constR(rep : Term::Rep) : Rewriter
      finite do |attachments, _|
        attachments.ping.call
        rep
      end
    end

    # *Ruleset rewriter*. See `rho.rulesetR`.
    def rulesetR(ruleset : Ruleset, exh : Bool = false) : Rewriter
      if exh
        finite { |attachments, input| exh_rulesetR(attachments, ruleset, input) }
      else
        finite { |attachments, input| first_rulesetR(attachments, ruleset, input) }
      end
    end

    private def first_rulesetR(attachments, ruleset : Ruleset, input : Term) : Term::Rep
      ruleset.each_candidate(input) do |pattern, rule|
        attachments.ping.call

        case rule
        in Rule::Template
          next unless env = M1.match?(Term[], pattern, input)

          rep = Alloy.render_rep(rule.body, locals: env)
        in Rule::Backmap
          next unless rep = M1.backmapR?(pattern, rule.backspec, input)
        end

        if Term.changes?(input, after: rep)
          return rep
        end
      end

      Term.rep(input)
    end

    private def exh_rulesetR(attachments, ruleset : Ruleset, input : Term) : Term::Rep
      seen = Pf::USet32.new
      workspace = Term.rep(input)

      running = true
      loop do
        running = false

        workspace = Term.flatten(workspace) do |offspring|
          result = nil

          ruleset.each_candidate_with_id(offspring) do |(pattern, rule), id|
            attachments.ping.call

            next if seen.includes?(id)

            case rule
            in Rule::Template
              next unless env = M1.match?(Term[], pattern, offspring)

              rep = Alloy.render_rep(rule.body, locals: env)
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

    # See `rho.guardR`.
    def guardR(pattern : M1::Op::Any, successor : Rewriter) : Rewriter
      finite do |attachments, input|
        guardR(pattern, successor, attachments, input)
      end
    end

    private def guardR(pattern, successor, attachments, input : Term) : Term::Rep
      unless M1.probably_matches?(pattern, input)
        return Term.rep(input)
      end

      unless M1.probe?(Term[], pattern, input)
        return Term.rep(input)
      end

      successor.call(input, attachments.cache, attachments.ping)
    end

    private def leaf?(passable, impassable, input : Term) : Bool
      return true unless input.type.dict?

      return true if passable && passable.present? && !passable.probe?(input)
      return true if impassable && impassable.present? && impassable.probe?(input)

      false
    end

    # :nodoc:
    defrecord LeafPredicate(T),
      guide : Term::Dict::Summary,
      passable : M1::PatternSet(T)?,
      impassable : M1::PatternSet(T)?

    # *Descending rewriter*. See `rho.descR`.
    def descR(successor : Rewriter, part : Part, leafp : LeafPredicate) : Rewriter
      finite do |attachments, input|
        descR(successor, part, leafp, attachments, input)
      end
    end

    private def descR(successor, part, leafp, attachments, input : Term) : Term::Rep
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      if leaf?(leafp.passable, leafp.impassable, input)
        return successor.call(input, attachments.cache, attachments.ping)
      end

      rep = successor.call(input, attachments.cache, attachments.ping)
      if Term.changes?(input, after: rep)
        return rep
      end

      assert dict0 = input.as_d?

      dict1 = flatten(dict0, part, leafp.guide) do |value|
        descR(successor, part, leafp, attachments, value)
      end

      output = Term.rep_of(dict1)

      attachments.cache.put({attachments.id, input}, output)

      output
    end

    # *Scan rewriter*. See `rho.scanR`.
    def scanR(successor : Rewriter, part : Part, leafp : LeafPredicate) : Rewriter
      finite do |attachments, input|
        scanR(successor, part, leafp, attachments, input)
      end
    end

    private def scanR(successor, part, leafp, attachments, input : Term) : Term::Rep
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      if leaf?(leafp.passable, leafp.impassable, input)
        return successor.call(input, attachments.cache, attachments.ping)
      end

      assert dict0 = input.as_d?

      dict1 = flatten(dict0, part, leafp.guide) do |value|
        successor.call(value, attachments.cache, attachments.ping)
      end

      rep = Term.rep_of(dict1)

      if Term.changes?(input, after: rep)
        attachments.cache.put({attachments.id, input}, rep)
      end

      rep
    end

    # *Ascending rewriter*. See `rho.ascR`.
    def ascR(successor : Rewriter, part : Part, leafp : LeafPredicate) : Rewriter
      finite do |attachments, input|
        ascR(successor, part, leafp, attachments, input)
      end
    end

    private def ascR(successor, part, leafp, attachments, input : Term) : Term::Rep
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      if leaf?(leafp.passable, leafp.impassable, input)
        return successor.call(input, attachments.cache, attachments.ping)
      end

      assert dict0 = input.as_d?

      dict1 = flatten(dict0, part, leafp.guide) do |value|
        ascR(successor, part, leafp, attachments, value)
      end

      output = Term.of(dict1)

      rep = successor.call(output, attachments.cache, attachments.ping)

      if Term.changes?(input, after: rep)
        attachments.cache.put({attachments.id, input}, rep)
      end

      rep
    end

    # *Bidirectional rewriter*. See `rho.bidiR`.
    def bidiR(successor : Rewriter, part : Part, leafp : LeafPredicate) : Rewriter
      finite do |attachments, input|
        bidiR(successor, part, leafp, attachments, input)
      end
    end

    private def bidiR(successor, part, leafp, attachments, input : Term) : Term::Rep
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      rep = successor.call(input, attachments.cache, attachments.ping)

      if leaf?(leafp.passable, leafp.impassable, input)
        return rep
      end

      if Term.changes?(input, after: rep)
        attachments.cache.put({attachments.id, input}, rep)
        return rep
      end

      assert dict0 = input.as_d?

      dict1 = flatten(dict0, part, leafp.guide) do |value|
        bidiR(successor, part, leafp, attachments, value)
      end

      output = Term.of(dict1)

      if input == output
        return Term.rep(input)
      end

      rep = successor.call(output, attachments.cache, attachments.ping)
      attachments.cache.put({attachments.id, input}, rep)

      rep
    end

    # *Wave rewriter*. See `rho.waveR`.
    def waveR(successor : Rewriter, leafp : LeafPredicate) : Rewriter
      finite do |attachments, input|
        waveR(successor, leafp, attachments, input)
      end
    end

    private def waveR(successor, leafp, attachments, input : Term) : Term::Rep
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      if leaf?(leafp.passable, leafp.impassable, input)
        return successor.call(input, attachments.cache, attachments.ping)
      end

      assert dict0 = input.as_d?

      rep0 = successor.call(input, attachments.cache, attachments.ping)
      rep1 = Term.flatten(rep0) do |offspring|
        unless dict1 = offspring.as_d?
          next Term.rep(offspring)
        end

        if Term.item_changed?(dict0, dict1)
          next Term.rep(offspring)
        end

        output = Term.flatten(dict1, leafp.guide, part: Term::Dict.itemspart) do |_, value|
          waveR(successor, leafp, attachments, value)
        end

        Term.rep_of(output)
      end

      if Term.changes?(input, after: rep1)
        attachments.cache.put({attachments.id, input}, rep1)
      end

      rep1
    end

    # *Adjacent rewriter*: rewrites items of a dictionary, taking their neighbors
    # into account.
    def adjR(successor : Rewriter) : Rewriter
      finite do |attachments, input|
        adjR(successor, attachments, input)
      end
    end

    private def adjR(successor, attachments, input : Term) : Term::Rep
      unless dict0 = input.as_d?
        return Term.rep(input)
      end

      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      dict1 = Term.flatten(dict0, part: Term::Dict.itemspart) do |key, item|
        assert index = key.index32?

        query = Term::Dict.build do |commit|
          if index > 0
            commit.with(:l, dict0[index - 1])
          end

          commit.with(:m, item)

          if index + 1 < dict0.itemsize
            commit.with(:r, dict0[index + 1])
          end
        end

        responses0 = successor.call(Term.of(query), attachments.cache, attachments.ping)
        responses1 = responses0.to_compact_readonly_slice do |response|
          response.as_d?.try { |r| r[:m]? }
        end

        Term.rep(responses1)
      end

      rep = Term.rep_of(dict1)

      if Term.changes?(input, after: rep)
        attachments.cache.put({attachments.id, input}, rep)
      end

      rep
    end

    # *Value rewriter*. Uses *successor* to rewrite the value of an entry
    # with the given *key*.
    def valueR(key : Term, successor : Rewriter) : Rewriter
      finite do |attachments, input|
        valueR(key, successor, attachments, input)
      end
    end

    private def valueR(key, successor, attachments, input : Term) : Term::Rep
      Term.subst(input, {key}) do |value0|
        successor.call(value0, attachments.cache, attachments.ping)
      end
    end

    # *Exhaustive rewriter*. See `rho.exhR`.
    def exhR(successor : Rewriter, *, limit : UInt32 = UInt32::MAX) : Rewriter
      if limit == UInt32::MAX # ?!
        infinite do |attachments, input|
          exhR(successor, attachments, input, limit)
        end
      else
        finite do |attachments, input|
          exhR(successor, attachments, input, limit)
        end
      end
    end

    private def exhR(successor, attachments, input : Term, limit : UInt32) : Term::Rep
      limit.times do |epoch|
        attachments.ping.call

        rep = successor.call(input, attachments.cache, attachments.ping)
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

        output = Term.flatten(rep) do |offspring|
          exhR(successor, attachments, offspring, limit - epoch)
        end

        return output
      end

      Term.rep(input) # E.g. if limit: 0 due to `limit - epoch` above.
    end

    # *Chain rewriter*: see `rho.chainR`.
    def chainR(a : Rewriter, b : Rewriter) : Rewriter
      fn = Rewriter::Fn.new do |attachments, input|
        rep = a.call(input, attachments.cache, attachments.ping)

        Term.flatten(rep) do |offspring|
          b.call(offspring, attachments.cache, attachments.ping)
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

    # :nodoc:
    SHORTHAND_WAVER = ML.term(<<-WWML)
    (waveR (rulesetR exh: true)
      passable: [passable pattern_]
      impassable: [impassable pattern_])
    WWML

    defrecord Part, dict : Term::Dict::Part::Any, axis : Term?, key : Term?

    private def flatten(dict : Term::Dict, part : Part, guide : Term::Dict::Summary, & : Term -> Term::Rep) : Term::Dict
      if axis = part.axis
        return dict unless target0 = dict[axis]?.as_d?

        if key = part.key
          return dict unless value0 = target0[key]?

          input = Term.of(dict.with(axis, value0))
          output = Term.collapse(yield input)
          return dict unless output = output.as_d?

          target1 = Term.morph(target0, {key, output[axis]?})
          return output.with(axis, target1)
        else
          target1 = Term.flatten(target0, guide, part: part.dict) do |_, value0|
            input = Term.of(dict.with(axis, value0))
            output = Term.collapse(yield input)
            next Term.rep unless output = output.as_d?

            dict = output
            Term.rep_of(dict[axis]? || Slice(Term).empty)
          end
          return dict.with(axis, target1)
        end
      end

      Term.flatten(dict, guide, part: part.dict) do |_, value|
        yield value
      end
    end

    private def dirR(spec : Term, data : Term, &) : Rewriter
      Term.matchpi(spec, %{[_ successor_]}) do
        if passable = spec[:passable]?
          passable_set = M1::PatternSet(Term).select(passable, data)
        end

        if impassable = spec[:impassable]?
          impassable_set = M1::PatternSet(Term).select(impassable, data)
        end

        axis = spec[:axis]?
        key = spec[:key]?

        case spec[:part]?
        when Term.of(:items)
          part = Part.new(Term::Dict.itemspart, axis, key)
        when Term.of(:pairs)
          part = Part.new(Term::Dict.pairspart, axis, key)
        when Term.of(:entries)
          part = Part.new(Term::Dict.entries, axis, key)
        else
          part = Part.new(Term::Dict.itemspart, axis, key) # default
        end

        guide = Term::Dict::Summary.zero

        pass do
          next unless cues = spec[:cues]?
          next unless cue_list = cues.as_d?

          cue_list.items.each do |item|
            guide = Term::Dict::Summary.union(guide, Term::Dict::Summary.of(item))
          end
        end

        leafp = LeafPredicate.new(guide, passable_set, impassable_set)

        yield rewriter(successor, data), part, leafp
      end
    end

    # See `rho.rulesysR`.
    def rulesysR(ruleset : Ruleset, depth_limit : UInt32) : Rewriter
      finite do |attachments, input|
        rulesysR(ruleset, depth_limit, attachments, input)
      end
    end

    private def rulesysR(ruleset : Ruleset, depth_limit : UInt32, attachments, input : Term) : Term::Rep
      proposals = Pf::Kit.stack_array(Term, 8)

      ruleset.each_candidate(input) do |op, rule|
        case rule
        in Rule::Template
          next unless env = M1.match?(Term[], op, input)

          rep = Alloy.render_rep(rule.body, locals: env)
        in Rule::Backmap
          matches_and_traces = M1.matches_and_traces(Term[], op, input)
          backmap = {matches_and_traces, rule.backspec.as_d}
          backsys = {backmap}
          rep = M1.backmapR(backsys, input)
        end

        proposals << Term.collapse(rep)
      end

      if proposals.empty?
        return Term.rep(input)
      end

      programs = Pf::Kit.stack_array(Array(Term::Diff::Action) | Term, 8)
      proposals.each do |proposal|
        program = Term.diff?(input, proposal, depth_limit: depth_limit) || proposal
        programs << program
      end

      if program = programs.single?
        if program.is_a?(Term)
          # E.g. 100 -> 200 replacement.
          return Term.rep(program)
        end

        # Accept immediately if there is just one program.
        accepted = program
      else
        accepted = Pf::Kit.stack_array(Term::Diff::Action)
        rejected = Pf::USet32.new

        programs.zip(0u32...programs.size) do |program0, i|
          next if i.in?(rejected)

          if program0.is_a?(Term)
            return Term.rep(input) # General replacement conflicts with everything else.
          end

          # Decline if any other program conflicts with this one.
          accept = true
          (i + 1...programs.size).each do |j|
            program1 = programs[j]
            if program1.is_a?(Term)
              return Term.rep(input) # General replacement conflicts with everything else.
            end

            unless Term::Diff.compatible?(program0, program1)
              rejected = rejected.add(i).add(j)
              accept = false
              break
            end
          end

          next unless accept

          accepted.concat(program0)
        end
      end

      mutation = Term::Diff::Mutation.new
      accepted.each do |action|
        mutation << action
      end

      output = mutation.apply(input.as_d)
      Term.rep_of(output)
    end

    # Same as `rewriter`, but avoids hitting the rewriter cache.
    def rewriter!(spec : Term, data : Term) : Rewriter
      Term.case(spec) do
        # |@ rho.noR
        #
        # |@pattern
        # noR
        #
        # |@summary
        # Noop (null, identity) rewriter.
        #
        # |@block
        # Keeps the input term as-is.
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2))
        # (cell @out)
        # (rewriter (@in -> noR -> @out))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (+ 1 2))
        # (rewriter (@in -> noR -> @out))
        # ```
        matchpi %{noR} do
          noR
        end

        # |@ rho.constR
        #
        # |@pattern
        # [constR offspring_*]
        #
        # |@summary
        # Constant rewriter.
        #
        # |@block
        # Replaces the input term with zero or more constant *offspring* terms.
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2))
        # (cell @out)
        # (rewriter (@in -> (constR 100) -> @out))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out 100)
        # (rewriter (@in -> (constR 100) -> @out))
        # ```
        #
        # Or, for example, with `rho.scanR`:
        #
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2))
        # (cell @out)
        # (rewriter (@in -> (scanR (constR 100)) -> @out))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (100 100 100)) ;; All items replaced with 100.
        # (rewriter (@in -> (scanR (constR 100)) -> @out))
        # ```
        matchpi %{[constR offspring_*]} do
          constR(Term.rep(offspring.items))
        end

        # |@ rho.rulesetR
        #
        # |@summary
        # Ruleset rewriter.

        # |@ rho.rulesetR
        #
        # |@pattern
        # rulesetR
        #
        # |@block
        # A basic ruleset rewriter. Only one rule is given the opportunity to rewrite
        # the input term. The most specific rule wins (according to M1 pattern specificty).
        #
        # The data term at the invokation spot is parsed as the ruleset.
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2))
        # (cell @out)
        # (rewriter (@in -> (scanR rulesetR) -> @out)
        #   ±n => ^(+ n 1))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (+ 2 3)) ;; Number items incremented
        # ;; (rewriter (@in -> (scanR rulesetR) -> @out) ...)
        # ```
        matchpi %{rulesetR} do
          ruleset = Ruleset.select(data)
          rulesetR(ruleset, exh: false)
        end

        # |@ rho.rulesetR
        #
        # |@pattern
        # (rulesetR ⍊ ⋮selector ⋮phase exh⋮ false)
        #
        # |@key selector m1.operator
        # The selector pattern. The captures this pattern makes determine the kind
        # of rule. Namely:
        #
        # - If the pattern captures `pattern` and `template`, the ruleset creates
        #   a *template rule*. The ruleset interprets the captured `pattern` as
        #   an `m1.operator`, and the `template` as `alloy`.
        #
        # - If the pattern captures `pattern` and `backspec`, the ruleset creates
        #   a *backmap rule* (or simply a *backmap*). The ruleset interprets *pattern*
        #   as an `m1.operator`, and *backspec* as an `m1.backspec`.
        #
        # Other captures are ignored. If you try to confuse the ruleset by capturing
        # all of *pattern*, *template*, and *backspec*, the ruleset will refuse to
        # create the rule.
        #
        # |@key phase
        # The selector is wrapped in a dict pattern with *phase* as its head.
        #
        # For example, `(rulesetR selector: (rule pattern_ template_) phase: main)`
        # is the same as writing `(rulesetR selector: (rule [main pattern_] template_))`.
        #
        # *phase* is most useful when you want to leave the selector out, that is,
        # use the default selector. The default selector is rather lengthy, so it's
        # nice to be able to avoid typing it out if you only want to wrap it.
        #
        # *phase* is used to store multiple distinct rulesets in the same rulebase
        # term. This is most useful in Rack's `rack.rewriter` node, which only allows you
        # to specify one rulebase.
        #
        # |@key exh
        # Outside of exhaustive mode (`false`), applies the most specific matching rule.
        #
        # In exhaustive mode (`true`), every rule is given an opportunity to match and
        # transform the input term. Rules are sorted by specificity; the most specific
        # rule is preferred. A rule is *expended* if it changes the input term, meaning
        # a rule can only change the input term once. Thus, even though the rewrite is
        # exhaustive, it is guaranteed to terminate (versus, say, `rho.exhR`). That is,
        # at some point you will necessarily run out of rules, since the number of rules
        # is finite.
        #
        # |@block
        # Constructs a *ruleset rewriter*: a rewriter that finds and applies rules
        # from a *rulebase* to input terms.
        matchpi %{(rulesetR ⍊ exh⋮ false)} do
          continue unless rulebase = data.as_d?

          phase = spec[:phase]?

          unless selector = spec[:selector]?
            selector = Ruleset::DEFAULT_SELECTOR

            # Preprocess the rulebase to support multi-pattern rules and backmaps. Currently
            # we support them in just this corner of the system. TODO: We should add support
            # for them to Ruleset, though, where it belongs.
            (0u32...rulebase.uitemsize).reverse_each do |index|
              member = rulebase[index]

              # [rule (%plural min: 3)]
              # [backmap (%plural min: 3)]
              next unless member = member.as_d?
              next unless member.itemsize >= 4
              head = member.items.first
              next unless head.in?(Term.of(:rule), Term.of(:backmap))

              # Duplicate body for each pattern.
              patterns = member.items.move(1).grow(-1)
              body = member.items.last
              reps = patterns.to_readonly_slice do |pattern|
                Term.of(head, pattern, body)
              end

              # Splice.
              rulebase = rulebase.replace(index, Term.rep(reps))
            end
          end

          rulesetR(Ruleset.select(selector, Term.of(rulebase), phase: phase), exh: exh.true?)
        end

        # |@ rho.rulesysR
        #
        # |@pattern
        # rulesysR
        # (rulesysR depth-limit: ∞)
        # (rulesysR depth-limit_: (%number +i32!))
        #
        # |@key depth-limit
        # Specifies the depth limit for the diff algorithm; dictionaries below this depth
        # are treated atomically (as indivisible units). Unless the dictionaries you rewrite
        # are typically very shallow, please set this, most often you'll just want `1` or
        # `2` or rarely `3`. Using the default `∞` with deep dictionaries currently has
        # really, *really* poor performance, so avoid it at all costs.
        #
        # `0` works, and will treat the top-level dictionary atomically, but that has
        # very little practical point.
        #
        # |@summary
        # Rule system rewriter.
        #
        # |@block
        # An experimental rewriter that uses the term diff algorithm to merge changes
        # from two or more rules.
        #
        # All rules are shown the same input term. They can then rewrite it. Rewrites
        # are merged into the input term by taking a diff between the input term and
        # rewritten term(s).
        #
        # This rewriter is strict. All conflicting changes are rejected (i.e. there
        # is no preference for one change or another, no precedence, etc.).

        matchpi %{rulesysR}, %{(rulesetR ⍊ depth-limit: ∞)} do
          ruleset = Ruleset.select(data)
          limit = UInt32::MAX
          rulesysR(ruleset, limit)
        end

        matchpi %{(rulesysR ⍊ depth-limit_: (%number +i32!))}, depth_limit: UInt32 do
          ruleset = Ruleset.select(data)
          rulesysR(ruleset, depth_limit)
        end

        # |@ rho.exhR
        #
        # |@pattern
        # (exhR successor_ ⍊ ⋮limit)
        #
        # |@key successor rho
        #
        # |@key limit
        # Sets the maximum number of cycles. If set, exhR becomes a finite rewriter.
        # Minimum: 1. Maximum: 32 (inclusive).
        #
        # |@summary
        # Exhaustive rewriter.
        #
        # |@block
        # Rewrites using *successor* until quiescence or until *limit* is exceeded.
        # Effectively, unless *limit* is set, converts a finite or infinite *successor*
        # to an infinite one.
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in 100)
        # (cell @out)
        # (rewriter (@in -> (exhR rulesetR) -> @out)
        #   n←(%number _ < 110) => ^(+ n 1))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out 110) ;; The rewriter incremented the number in a loop until it hit 110.
        # ;; (rewriter (@in -> (exhR rulesetR) -> @out) ...)
        # ```
        matchpi %{[exhR successor_]} do
          limit = pass do
            next unless candidate = spec[:limit]?
            next unless candidate = candidate.index32?
            next unless 1 <= candidate <= 32

            candidate
          end

          exhR(rewriter(successor, data), limit: limit || UInt32::MAX)
        end

        # |@ rho.scanR
        #
        # |@pattern
        # (scanR successor_ ⍊ pairs_)
        #
        # |@key successor rho
        #
        # |@key pairs
        # See `rho.ascR` to learn more about the pairs accepted by this rewriter.
        #
        # |@summary
        # Scan rewriter.
        #
        # |@block
        # Rewrites the input dictionary's items, pairs, or both (depending on
        # `part: _` in *pairs*).
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (1 2 3 x: 100 y: 200))
        # (cell @out)
        # (rewriter (@in -> (scanR rulesetR) -> @out)
        #   ±n => ^(+ n 1))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (2 3 4 x: 100 y: 200)) ;; All items were incremented, pairs intact.
        # ;; (rewriter (@in -> (scanR rulesetR) -> @out) ...)
        # ```
        #
        # The same circuit with `part: pairs`:
        #
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (1 2 3 x: 100 y: 200))
        # (cell @out)
        # (rewriter (@in -> (scanR rulesetR part: pairs) -> @out)
        #   ±n => ^(+ n 1))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (1 2 3 x: 101 y: 201)) ;; All pairs were incremented, items intact.
        # ;; (rewriter (@in -> (scanR rulesetR part: pairs) -> @out) ...)
        # ```
        #
        # Similarly, if you set `part: entries`, both items and pairs will
        # be incremented.
        matchpi %{[scanR successor_]} do
          dirR(spec, data) do |successorR, part, leafp|
            scanR(successorR, part, leafp)
          end
        end

        # |@ rho.ascR
        #
        # |@summary
        # Ascending rewriter.

        # |@ rho.ascR
        #
        # |@pattern
        # ascR
        #
        # |@block
        # The shorthand form of `ascR`. It is equivalent to writing:
        #
        # ```wwml
        # (ascR (rulesetR exh: true)
        #   passable: [passable pattern_]
        #   impassable: [impassable pattern_])
        # ```
        matchpi %{ascR} do
          rewriter(SHORTHAND_ASCR, data)
        end

        # |@ rho.ascR
        #
        # |@pattern
        # (ascR successor_ ⍊ ⋮axis part⋮ items ⋮passable ⋮impassable ⋮cues)
        #
        # |@key successor rho
        #
        # |@key axis
        # If present, sets the key whose value should be the term-to-be-rewritten.
        # For example, consider the dict `{expr: (+ x y), vars: {x: 100, y: 200}}`.
        # You might want to rewrite so that `{expr: +, vars: {x: 100, y: 200}}`,
        # `{expr: x, vars: {...}}`, `{expr: y, vars: {...}}`, `{expr: (+ x y), vars: {...}}`.
        #
        # |@key part
        # Can be `items` (default, fallback; descend only into itemsparts),
        # `pairs` (only into pairsparts), or `entries` (descend into both).
        #
        # |@key passable m1.operator
        # |@key impassable m1.operator
        #
        # |@key cues
        # A list of cue terms. This can be used to optimize rewriters that guarantee
        # the presence of some "structural token". For example, in editR, we use this
        # property to only descend into terms which probably have `I` in them, since
        # `I` (picked for its resemblance to the I-beam) is the definitive sign that
        # editR should be interested in the term.
        #
        # |@block
        # Post-order depth-first rewrite of a dictionary *part*.
        matchpi %{[ascR _]} do
          dirR(spec, data) do |successorR, part, leafp|
            ascR(successorR, part, leafp)
          end
        end

        # |@ rho.descR
        #
        # |@summary
        # Descending rewriter.

        # |@ rho.descR
        #
        # |@pattern
        # descR
        #
        # |@block
        # The shorthand form of `descR`. It is equivalent to writing:
        #
        # ```wwml
        # (descR (rulesetR exh: true)
        #   passable: [passable pattern_]
        #   impassable: [impassable pattern_])
        # ```
        matchpi %{descR} do
          rewriter(SHORTHAND_DESCR, data)
        end

        # |@ rho.descR
        #
        # |@pattern
        # (descR successor_ ⍊ ⋮axis part⋮ items ⋮passable ⋮impassable ⋮cues)
        #
        # |@key successor rho
        #
        # |@key axis
        # See `rho.ascR`.
        #
        # |@key part
        # See `rho.ascR`.
        #
        # |@key passable m1.operator
        # |@key impassable m1.operator
        #
        # |@key cues
        # See `rho.ascR`.
        #
        # |@block
        # Pre-order depth-first rewrite of a dictionary *part*.
        matchpi %{[descR _]} do
          dirR(spec, data) do |successorR, part, leafp|
            descR(successorR, part, leafp)
          end
        end

        # |@ rho.bidiR
        #
        # |@summary
        # Bidirectional rewriter.

        # |@ rho.bidiR
        #
        # |@pattern
        # bidiR
        #
        # |@block
        # The shorthand form of `bidiR`. It is equivalent to writing:
        #
        # ```wwml
        # (bidiR (rulesetR exh: true)
        #   passable: [passable pattern_]
        #   impassable: [impassable pattern_])
        # ```
        matchpi %{bidiR} do
          rewriter(SHORTHAND_BIDIR, data)
        end

        # |@ rho.bidiR
        #
        # |@pattern
        # (bidiR successor_ ⍊ ⋮axis part⋮ items ⋮passable ⋮impassable ⋮cues)
        #
        # |@key successor rho
        #
        # |@key axis
        # See `rho.ascR`.
        #
        # |@key part
        # See `rho.ascR`.
        #
        # |@key passable m1.operator
        # |@key impassable m1.operator
        #
        # |@key cues
        # See `rho.ascR`.
        #
        # |@block
        # First, attempts a pre-order rewrite with *successor*; if no change,
        # then applies recursively to *part*, if possible; if the rewritten
        # version changed, attempts to apply *successor* again.
        matchpi %{[bidiR _]} do
          dirR(spec, data) do |successorR, part, leafp|
            bidiR(successorR, part, leafp)
          end
        end

        # |@ rho.waveR
        #
        # |@summary
        # Wave rewriter.

        # |@ rho.waveR
        #
        # |@pattern
        # waveR
        #
        # |@block
        # The shorthand form of `waveR`. It is equivalent to writing:
        #
        # ```wwml
        # (waveR (rulesetR exh: true)
        #   passable: [passable pattern_]
        #   impassable: [impassable pattern_])
        # ```
        matchpi %{waveR} do
          rewriter(SHORTHAND_WAVER, data)
        end

        # |@ rho.waveR
        #
        # |@pattern
        # (waveR successor_ ⍊ ⋮passable ⋮impassable ⋮cues)
        #
        # |@key successor rho
        #
        # |@key passable m1.operator
        # |@key impassable m1.operator
        #
        # |@key cues
        # See `rho.ascR`.
        #
        # |@block
        # Lets you recurse down the itemspart tree, proceeding if you modify
        # the pairspart of the current node, or do not modify anything; but
        # stopping if you modify its itemspart.
        #
        # In other words, you must not modify anything "in front of" the rewriter,
        # anything the rewriter will soon visit.
        #
        # Imagine a train laying tracks in front of itself. If there's an infinite
        # amount of tracks onboard, this process will never terminate as long as
        # there's enough space to lay tracks. Imagine yourself throwing data off
        # the side of the train as it moves; notice how this doesn't affect
        # the train's course, nor whether it'll stop.
        #
        # In this analogy, the rewriter is the train, and paths through itemsparts
        # are tracks. To keep `waveR` finite, it will simply refuse to go along a
        # track if you modify anything in front (even if you *remove* tracks ahead).
        # On the other hand, you are allowed to modify data on the "sides" --
        # the pairsparts of nodes the rewriter recurses through are orthogonal to
        # tracks, and so, waveR is happy with you changing them.
        #
        # Thus, `waveR` is invincible from infinite regress in degenerate cases
        # such as `(x_) => ((^x))`, whereas something like `(exhR descR)` isn't.
        # This is the reason `waveR` is guaranteed to terminate, as its depth is
        # always bounded by the deepest unchanged item.
        matchpi %{[waveR _]} do
          dirR(spec, data) do |successorR, _, leafp|
            waveR(successorR, leafp)
          end
        end

        # |@ rho.chainR
        #
        # |@summary
        # Chain rewriter.

        # |@ rho.chainR
        #
        # |@pattern
        # [chainR successors_+]
        #
        # |@key successors rho
        #
        # |@block
        # Rewrites the input term with each *successor* in turn.
        #
        # The data term is broadcast to all *successors*.
        #
        # If one successor doesn't terminate the rest of them won't be reached.
        # Use limited `exhR` if you want to guarantee termination.
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2))
        # (cell @out)
        # (rewriter (@in -> (chainR (scanR rulesetR) rulesetR) -> @out)
        #   ;; Increment numbers. Since our toplevel term is a dict, this is only
        #   ;; reached by (scanR rulesetR).
        #   ±n => ^(+ n 1)
        #   ;; Evaluate sum. This will only apply to our toplevel term.
        #   (+ ±a ±b) => ^(+ a b))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out 5) ;; (+ 1 2) -> (+ 2 3) -> 5
        # ;; (rewriter (@in -> (chainR (scanR rulesetR) rulesetR) -> @out) ...)
        # ```
        matchpi %{[chainR head_ successors_*]} do
          successors.items.reduce(rewriter(head, data)) do |memoR, successor|
            chainR(memoR, rewriter(successor, data))
          end
        end

        # |@ rho.chainR
        #
        # |@pattern
        # chainR
        #
        # |@block
        # This variant works similarly to `rho.stageR` but allows each individual
        # `step` to specify its own rewriter, unlike `rho.stageR` which uses
        # the same rewriter for all of its `stage`s.
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2))
        # (cell @out)
        # (rewriter (@in -> chainR -> @out)
        #   ;; Zero or more steps, each takes a Rho rewriter followed by zero or
        #   ;; more items for the data term that should be passed to the rewriter.
        #   (step (scanR rulesetR)
        #     ±n => ^(+ n 1))
        #   (step rulesetR
        #     (+ ±a ±b) => ^(+ a b)))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out 5) ;; (+ 1 2) -> (+ 2 3) -> 5
        # ;; (rewriter (@in -> chainR -> @out) ...)
        # ```
        matchpi %{chainR} do
          continue unless document = data.as_d?

          steps = Pf::Kit.stack_array(Rewriter, 4)

          document.items.each do |item|
            Term.matchpi?(item, %{[step rwspec_ stepdata_*]}) do
              steps << rewriter(rwspec, stepdata)
            end
          end

          if steps.empty?
            return noR
          end

          steps.reduce { |predecessor, successor| chainR(predecessor, successor) }
        end

        # |@ rho.guardR
        #
        # |@pattern
        # [guardR pattern_ successor_]
        #
        # |@key pattern m1.operator
        # The input term must match this operator to be rewritten with *successor*.
        #
        # |@key successor rho
        # The guarded rewriter.
        #
        # |@summary
        # Guard rewriter.
        #
        # |@block
        # Guards *successor* with an M1 *pattern*. An input term is only passed to
        # *successor* if it matches *pattern*. Otherwise, it escapes from this
        # rewriter unchanged (in that case this rewriter works like `rho.noR`).
        #
        # |@example
        # Here the guardR's pattern says that the only allowed input term is
        # an itemsonly dictionary:
        #
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2))
        # (cell @out)
        # (rewriter (@in -> (guardR (_*) (scanR rulesetR)) -> @out)
        #   ±n => ^(+ n 1))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (+ 2 3))
        # ;; (rewriter (@in -> (guardR (_*) (scanR rulesetR))) ...)
        # ```
        #
        # So if we add some pairs, the rewriter will not change the input:
        #
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (+ 1 2 x: 100 y: 200))
        # (cell @out)
        # (rewriter (@in -> (guardR (_*) (scanR rulesetR)) -> @out)
        #   ±n => ^(+ n 1))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (+ 1 2 x: 100 y: 200)) ;; Nothing changed
        # ;; (rewriter (@in -> (guardR (_*) (scanR rulesetR))) ...)
        # ```
        matchpi %{[guardR pattern_ successor_]} do
          guardR(M1.operator(pattern), rewriter(successor, data))
        end

        # |@ rho.adjR
        #
        # |@pattern
        # [adjR successor_]
        #
        # |@key successor rho
        #
        # |@summary
        # Adjacent rewriter.
        #
        # |@block
        # Rewrites each item in an input dictionary using *successor*. Each item is
        # also paired with its left and right neighbors. The resulting dictionary
        # `{| l m r}` is rewritten using *successor*, which must return `{¦ m}`.
        matchpi %{[adjR successor_]} do
          adjR(rewriter(successor, data))
        end

        # |@ rho.valueR
        #
        # |@pattern
        # [valueR key_ successor_]
        #
        # |@key successor rho
        #
        # |@summary
        # Value rewriter.
        #
        # |@block
        # Uses *successor* to rewrite the value of an entry with the given *key*.
        matchpi %{[valueR key_ successor_]} do
          valueR(key, rewriter(successor, data))
        end

        # |@ rho.stageR
        #
        # |@pattern
        # [stageR successor_]
        #
        # |@key successor rho
        #
        # |@summary
        # Stage rewriter.
        #
        # |@block
        # Lets you split the rulebase into zero or more sections, called *stages*.
        # *successor* is instantiated for each stage, the stage being the instance's
        # rulebase, and the resulting rewriters are "stitched" together into a sequence
        # using `rho.chainR`.
        #
        # |@example
        # ```wwml
        # ;; Frame 0 (seed)
        #
        # (cell @in (a b a))
        # (cell @out)
        # (rewriter (@in -> (stageR (scanR rulesetR)) -> @out)
        #  (stage
        #    a => b
        #    b => a)
        #  (stage
        #    a => 100
        #    b => 200))
        #
        # ;; Frame 1
        #
        # (cell @in)
        # (cell @out (200 100 200))
        # ;; (rewriter (@in -> (stageR (scanR rulesetR)) -> @out) ...)
        # ```
        #
        # In the above, rewriting proceeded in two stages:
        #
        # ```text
        #   Input: (a b a)
        # Stage 1: (b a b)
        # Stage 2: (200 100 200)
        # ```
        matchpi %{[stageR successor_]} do
          continue unless document = data.as_d?

          stages = Pf::Kit.stack_array(Rewriter, 4)

          document.items.each do |item|
            Term.matchpi?(item, %{[stage stagedata_*]}) do
              stages << rewriter(successor, stagedata)
            end
          end

          if stages.empty?
            return noR
          end

          stages.reduce { |predecessor, successor| chainR(predecessor, successor) }
        end

        # |@ rho.section
        #
        # |@pattern
        # [section name_ successor_]
        #
        # |@key name
        # Name of the section to use.
        #
        # |@key successor rho
        #
        # |@summary
        # Selects a section of the data term for its successor rewriter.
        #
        # |@block
        # Defines the section of the rulebase where rulesets in *successor* must
        # search for rules.
        #
        # Sections are most useful for standalone rewriters (or passes) that are
        # defined as parts of a single document. Consider, for instance, the
        # following document:
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
        #   (section main rulesetR)
        #   (section calculate rulesetR))
        # ```
        #
        # Notice how we use `section` to refer to sections of the same document
        # the rewriter is in. By convention, Rho front-ends search for the `rewriter`
        # section in any dict they receive. When found, a front-end passes the entire
        # document to the rewriter. Thus, for instance, a rewriter may access itself.
        # Writing `(section rewriter rulesetR)` in the above makes perfect sense;
        # there is no need to separate the rules from the rewriter provided the general
        # arrangement makes sense:
        #
        # ```
        # --- rewriter
        # a => 100
        # b => 200
        # (±a ±b) => ^(+ a b)
        #
        # (ascR (section rewriter rulesetR))
        # ```
        #
        # NOTE: The rewriter spec must be located at the very end of the `rewriter`
        # section for the above to work.
        matchpi %{[section name_ successor_]} do
          continue unless ruledoc = data.as_d?
          continue unless rulebase = ruledoc[name]?

          rewriter(successor, rulebase)
        end

        otherwise do
          noR
        end
      end
    end

    # :nodoc:
    REWRITER_CACHE = SyncLRU({Term, Term}, Rewriter).new(capacity: 32)

    def rewriter(spec : Term, data : Term) : Rewriter
      REWRITER_CACHE.put_if_absent({spec, data}) do
        rewriter!(spec, data)
      end
    end

    # TODO: add support for rules / rewriter definitions (to define recursive
    # rewriters etc.)
    def rewriter(document : Term, *, section : Term = Term.of(:rewriter)) : Rewriter
      return noR unless dict = document.as_d?
      return noR unless section = dict[section]?
      return noR unless spec = section.items.last?

      rewriter(spec, document)
    end

    # Rewrites the *input* term using *rewriter*.
    #
    # Some rewriters support caching. You can pass a *cache* object for them to use.
    # By default, `Uncached` is used, which means that for such rewriters, caching
    # will be disabled.
    #
    # *ping* is called periodically to allow cooperative scheduling, yields,
    # timeouts, aborts, etc.
    def rewrite(
      rewriter : Rewriter,
      input : Term, *,
      cache : IRewriteCache = Uncached({RewriterId, Term}, Term::Rep).new,
      ping = -> { },
    ) : Term
      rep = rewriter.call(input, cache, ping)

      Term.collapse(rep)
    end
  end
end
