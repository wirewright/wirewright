module Ww
  # Wirewright Rho is a set of composable rewriters. Think parser combinators,
  # but for term rewriting.
  module Rho
    extend self

    alias RewriterId = UInt64
    alias IRewriteCache = ICache({RewriterId, Term}, Term::Rep)

    defrecord RewriteAttachments, id : RewriterId, cache : IRewriteCache

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
      def call(input : Term, cache : IRewriteCache) : Term::Rep
        @fn.call(RewriteAttachments.new(@id, cache), input)
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
      finite { |_, input| Term.rep(input) }
    end

    # *Constant rewriter*. See `rho.constR`.
    def constR(rep : Term::Rep) : Rewriter
      finite { rep }
    end

    # *Ruleset rewriter*. See `rho.rulesetR`.
    def rulesetR(ruleset : Ruleset, exh : Bool = false) : Rewriter
      if exh
        finite { |_, input| exh_rulesetR(ruleset, input) }
      else
        finite { |_, input| first_rulesetR(ruleset, input) }
      end
    end

    private def first_rulesetR(ruleset : Ruleset, input : Term) : Term::Rep
      ruleset.each_candidate(input) do |pattern, rule|
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

    private def exh_rulesetR(ruleset : Ruleset, input : Term) : Term::Rep
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

    alias Part = Term::Dict::Part::Any

    # *Descending rewriter*. See `rho.descR`.
    def descR(successor : Rewriter, part : Part, leafp : LeafPredicate) : Rewriter
      finite do |attachments, input|
        descR(successor, part, leafp, attachments, input)
      end
    end

    private def descR(successor, part, leafp, attachments, input : Term)
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      if leaf?(leafp.passable, leafp.impassable, input)
        return successor.call(input, attachments.cache)
      end

      rep = successor.call(input, attachments.cache)
      if Term.changes?(input, after: rep)
        return rep
      end

      assert dict0 = input.as_d?

      dict1 = Term.flatten(dict0, leafp.guide, part: part) do |_, value|
        descR(successor, part, leafp, attachments, value)
      end

      output = Term.rep_of(dict1)

      attachments.cache.put({attachments.id, input}, output)

      output
    end

    # *Ascending rewriter*. See `rho.ascR`.
    def ascR(successor : Rewriter, part : Part, leafp : LeafPredicate) : Rewriter
      finite do |attachments, input|
        ascR(successor, part, leafp, attachments, input)
      end
    end

    private def ascR(successor, part, leafp, attachments, input : Term)
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      if leaf?(leafp.passable, leafp.impassable, input)
        return successor.call(input, attachments.cache)
      end

      assert dict0 = input.as_d?

      dict1 = Term.flatten(dict0, leafp.guide, part: part) do |_, value|
        ascR(successor, part, leafp, attachments, value)
      end

      output = Term.of(dict1)

      rep = successor.call(output, attachments.cache)

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

    private def bidiR(successor, part, leafp, attachments, input : Term)
      if rep = attachments.cache.get?({attachments.id, input})
        return rep
      end

      rep = successor.call(input, attachments.cache)

      if leaf?(leafp.passable, leafp.impassable, input)
        return rep
      end

      if Term.changes?(input, after: rep)
        attachments.cache.put({attachments.id, input}, rep)
        return rep
      end

      assert dict0 = input.as_d?

      dict1 = Term.flatten(dict0, leafp.guide, part: part) do |_, value|
        bidiR(successor, part, leafp, attachments, value)
      end

      output = Term.of(dict1)

      if input == output
        return Term.rep(input)
      end

      rep = successor.call(output, attachments.cache)
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
        return successor.call(input, attachments.cache)
      end

      assert dict0 = input.as_d?

      rep0 = successor.call(input, attachments.cache)
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

        responses0 = successor.call(Term.of(query), attachments.cache)
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

    private def exhR(successor, attachments, input : Term, limit : UInt32)
      limit.times do |epoch|
        rep = successor.call(input, attachments.cache)
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
        rep = a.call(input, attachments.cache)

        Term.flatten(rep) do |offspring|
          b.call(offspring, attachments.cache)
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

    private def dirR(spec : Term, data : Term, &) : Rewriter
      Term.matchpi(spec, %{[_ successor_]}) do
        if passable = spec[:passable]?
          passable_set = M1::PatternSet(Term).select(passable, data)
        end

        if impassable = spec[:impassable]?
          impassable_set = M1::PatternSet(Term).select(impassable, data)
        end

        case spec[:part]?
        when Term.of(:items)
          part = Term::Dict.itemspart
        when Term.of(:pairs)
          part = Term::Dict.pairspart
        when Term.of(:entries)
          part = Term::Dict.entries
        else
          part = Term::Dict.itemspart # default
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

    private def rewriter!(spec : Term, data : Term) : Rewriter
      Term.case(spec) do
        # |@ rho.noR
        #
        # |@pattern
        # noR
        #
        # |@block
        # Noop (identity) rewriter: keeps the input term as-is.
        matchpi %{noR} do
          noR
        end

        # |@ rho.constR
        #
        # |@pattern
        # [constR offspring_*]
        #
        # |@block
        # Replaces the input term with zero or more constant *offspring* terms.
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
          selector = spec[:selector]? || Ruleset::DEFAULT_SELECTOR

          discriminator = spec[:discriminator]?
          rulebase = data

          rulesetR(Ruleset.select(selector, rulebase, discriminator: discriminator), exh: exh.true?)
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
        # |@block
        # Rewrites using *successor* until fixed point or until *limit* is exceeded.
        # Effectively, unless *limit* is set, converts a finite or infinite rewriter
        # to an infinite rewriter.
        matchpi %{[exhR successor_]} do
          limit = pass do
            next unless candidate = spec[:limit]?
            next unless candidate = candidate.index32?
            next unless 1 <= candidate <= 32

            candidate
          end

          exhR(rewriter(successor, data), limit: limit || UInt32::MAX)
        end

        # |@ rho.ascR
        #
        # |@pattern
        # ascR
        matchpi %{ascR} do
          rewriter(SHORTHAND_ASCR, data)
        end

        # |@ rho.ascR
        #
        # |@pattern
        # (ascR successor_ ⍊ part⋮ items ⋮passable ⋮impassable ⋮cues)
        #
        # |@key successor rho
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
        # *Ascending rewriter*: post-order depth-first rewrite of a dictionary *part*.
        matchpi %{[ascR _]} do
          dirR(spec, data) do |successorR, part, leafp|
            ascR(successorR, part, leafp)
          end
        end

        # |@ rho.descR
        #
        # |@pattern
        # descR
        matchpi %{descR} do
          rewriter(SHORTHAND_DESCR, data)
        end

        # |@ rho.descR
        #
        # |@pattern
        # (descR successor_ ⍊ part⋮ items ⋮passable ⋮impassable ⋮cues)
        #
        # |@key successor rho
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
        # *Descending rewriter*: pre-order depth-first rewrite of a dictionary *part*.
        matchpi %{[descR _]} do
          dirR(spec, data) do |successorR, part, leafp|
            descR(successorR, part, leafp)
          end
        end

        # |@ rho.bidiR
        #
        # |@pattern
        # bidiR
        matchpi %{bidiR} do
          rewriter(SHORTHAND_BIDIR, data)
        end

        # |@ rho.bidiR
        #
        # |@pattern
        # (bidiR successor_ ⍊ part⋮ items ⋮passable ⋮impassable ⋮cues)
        #
        # |@key successor rho
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
        # *Bidirectional rewriter*: first, attempts a pre-order rewrite with
        # *successor*; if no change, then applies recursively to *part*, if
        # possible; if the rewritten version changed, attempts to apply
        # *successor* again.
        matchpi %{[bidiR _]} do
          dirR(spec, data) do |successorR, part, leafp|
            bidiR(successorR, part, leafp)
          end
        end

        # |@ rho.waveR
        #
        # |@pattern
        # waveR
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
        # The *wave rewriter* lets you recurse down the itemspart tree, proceeding
        # if you modify the pairspart of the current node, or do not modify anything;
        # but stopping if you modify its itemspart.
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
        # |@pattern
        # [chainR successors_+]
        #
        # |@key successors rho
        #
        # |@block
        # Rewrites the input term with each *successor* in turn. If one successor
        # doesn't terminate the rest of them won't be reached. Use limited `exhR`
        # if you want to guarantee termination.
        matchpi %{[chainR head_ successors_*]} do
          successors.items.reduce(rewriter(head, data)) do |memoR, successor|
            chainR(memoR, rewriter(successor, data))
          end
        end

        # |@ rho.adjR
        #
        # |@pattern
        # [adjR successor_]
        #
        # |@key successor rho
        #
        # |@block
        # The *adjacent rewriter* rewrites each item in an input dictionary
        # using *successor*. Each item is also paired with its left and right
        # neighbors. The resulting dictionary `{| l m r}` is rewritten using
        # *successor*, which must return `{¦ m}`.
        matchpi %{[adjR successor_]} do
          adjR(rewriter(successor, data))
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
        #   (section main (rulesetR))
        #   (section calculate (rulesetR)))
        # ```
        #
        # Notice how we use `section` to refer to sections of the same document
        # the rewriter is in. By convention, Rho front-ends search for the `rewriter`
        # section in any dict they receive. When found, a front-end passes the entire
        # document to the rewriter. Thus, for instance, a rewriter may access itself.
        # Writing `(section rewriter (rulesetR))` in the above makes perfect sense;
        # there is no need to separate the rules from the rewriter provided the general
        # arrangement makes sense:
        #
        # ```
        # --- rewriter
        # a => 100
        # b => 200
        # (±a ±b) => ^(+ a b)
        #
        # (ascR (section rewriter (rulesetR)))
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
    # ```
    # ```
    def rewrite(rewriter : Rewriter, input : Term, *, cache : IRewriteCache = Uncached({RewriterId, Term}, Term::Rep).new) : Term
      rep = rewriter.call(input, cache)

      Term.collapse(rep)
    end
  end
end
