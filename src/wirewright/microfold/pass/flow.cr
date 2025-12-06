module Ww::Microfold::Pass
  # Hosts the flow passes.
  module Flow
    extend self

    # :nodoc:
    FLOWR_SELECTOR = ML.term(%[(%any° [rule pattern_ template_] [backmap pattern_ backspec_])])

    # :nodoc:
    def flowR(rulebase : Term) : Rewriter
      # TODO: this should be a rewriter circuit stored in the theme!

      ruleset = Ruleset.select(FLOWR_SELECTOR, rulebase)

      # Clear cache so that if the theme is reloaded, we don't have stale
      # cache entries.
      cache = {} of Term => Rewrite::Any

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

      set, backmapR = recR

      refR = dfsR(
        switchR(
          { %[($my rewritee←($ _))], chainR(backmapR, envR(Term.of(:"$my"))) },
          { %[($my rewritee_)], envR(Term.of(:"$my")) },
          { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
          { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
        )
      )

      set.call chainR(refR, evalR)

      # Optimization: Do not enter recursive exhR unless the node is tagged
      # with µ-flow.
      guardR = Rewriter.new do |ctx, staging|
        staging.reduce do |term|
          next Rewrite.none unless dict = term.as_d?
          next Rewrite.none unless dict.itemsize > 0 && dict[:"µ-flow"]?

          Rewrite.one(dict)
        end
      end

      set, flowR = recR
      set.call(
        memoR(cache,
          allR(
            guardR,
            exhR(
              choiceR(itemsR(flowR), rulesetR(ruleset, noR, backmapR, noR)),
              # Cap the number of exhR revolutions. This makes sure we will terminate, at
              # last in theory; note how this is recursive exhR, so each recursive step will
              # have 32 exhR revolutions at its disposal (for each such revolution, each child
              # will have 32 exhR revolutions of its own etc.)
              limit: 32
            )
          )
        )
      )
    end

    # Performs the flow tracing pass on *node*.
    #
    # During flow tracing, Microfold annotates nodes that have anything to do
    # with cue flow with the `µ-flow` property; including nodes that contain
    # children with `µ-flow`, recursively.
    #
    # Most nodes do not use cues/flow features and hence do not require flow rewriting.
    # We do not want all nodes to pay the price of fixpoint flow which they do not
    # even use. Only nodes that require fixpoint flow should be inspected.
    def trace(node : Term) : Term
      traced, _ = trace0(node)
      traced
    end

    private def trace0(node : Term) : {Term, Bool}
      has_cues = false

      node = node.as_d do |nodedict|
        nodedict.pairspart.transaction do |commit|
          nodedict.items.each_with_index do |item, index|
            traced, item_has_cues = trace0(item)
            has_cues ||= item_has_cues
            commit << traced
          end
        end
      end

      unless has_cues
        Term.case(node) do
          matchpi(
            %[{¦ µ-preset: ⟨((%any on-cue-up on-cue-dn cue-up cue-dn) _*)⟩}],
            %[{¦ µ-style: ⟨((%any on-cue-up on-cue-dn cue-up cue-dn) _*)⟩}],
          ) { has_cues = true }

          otherwise { }
        end
      end

      unless has_cues
        return node, false
      end

      {node.as_d(&.morph({:"µ-flow", true})), true}
    end

    # Performs the flow evaluation pass -- the main flow pass on *root*.
    #
    # During the flow evaluation pass, Microfold propagates cue information until
    # fixpoint using the `flowR` rewriter circuit equipped with the flow ruleset
    # from the theme. Cue conditionals such as `in-*:` and `has-*:` are also
    # resolved during flow evaluation.
    def eval(root : Term, theme : Theme) : Term
      rewrite(root, theme.flowR)
    end

    # Runs the flow cleanup pass on *root*.
    #
    # During the flow cleanup pass, remains of flow tracing and evaluation are
    # removed from the tree.
    def prune(node : Term, theme : Theme, *, __smart : Bool = true)
      return node unless nodedict = node.as_d?
      return node if __smart && !nodedict[:"µ-flow"]?

      # µ-flow parents will affect their non-flow children so we have
      # to clear them as well, unless they themselves are µ-flow.
      nodedict = nodedict.transaction do |commit|
        commit.without(:"µ-flow")

        theme.each_flow_key_to_prune do |key|
          commit.without(key)
        end

        nodedict.items.each_with_index do |item, index|
          if item[:"µ-flow"]?
            commit.with(index, prune(item, theme))
          else
            commit.with(index, prune(item, theme, __smart: false))
          end
        end
      end

      Term.of(nodedict)
    end
  end

  # Runs the flow passes on *root* in proper sequence.
  def flow(root : Term, theme : Theme) : Term
    pipe(root, Flow.trace, Flow.eval(theme), Flow.prune(theme))
  end
end
