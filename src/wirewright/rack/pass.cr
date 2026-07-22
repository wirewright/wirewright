module Ww::Rack
  module Prepass
    extend self

    def call(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
      Part.prepass(hg, &fn)
    end
  end

  # TODO: In theory, we can actually avoid D7.case and this weird DSL,
  # and write all rules manually (i.e., explore the hypergraph manually).
  # Provided we have lots of helpers this should turn out to be much more
  # performant, readable, and also a huge lot more flexible; we can still
  # use hub-spokes (which D7.case forces) but occasionally conduct arbitrary
  # searches. This would be very useful to simplify the implementation of `part`.
  # It would be nice if there were no prepasses and all of this other weird
  # stuff. We should keep compatibility with Classifier though because everything
  # else relies on it, including MuSoma (i.e., it survived very well so it doesn't
  # deserve being thrown out); and also its caching is extremely useful. When we
  # get to the hypergraph, though, here in Rack, we're free do to whatever we want,
  # though. Instead of the hypergraph we should probably just use some sort of
  # Array(Node), where Node is a pre-parsed node. We won't need pattern matching
  # here, without it, everything should be pretty very fast, assuming we cache Term -> Node.
  # The Array(Node) can have some indexing attached as well.
  def propose(hg : D7::Hypergraph, proposals) : Nil
    D7.case(hg, proposals) do
      rule(<<-WWML) do |tgt|
      [discard @u_] dev
        -> (one u) [cell @u_ _] {name: tgt, max: ∞}
      WWML
        D7.patch(tgt, {2, nil})
      end

      rule(<<-WWML) do |dev, tgt|
      [discard @u_ pattern_] dev
        -> (one u) [cell @u_ value_] {name: tgt, max: ∞}
      WWML
        pattern = D7.fetch(dev, :pattern)

        patches = Pf::Kit.stack_array(D7::Patch, 4)

        tgt.each do |match|
          value = D7.fetch(match, :value)
          next unless M1.probe?(pattern, value)

          patches << D7.patch(match, {2, nil})
        end

        D7.patches(patches)
      end

      rule(<<-WWML) do |dev, src, dst|
      [feed inhibitors←((%past @_)) srcs←((%past @_ min: 1)) dsts←((%past @_ min: 1)) node_] dev
        -> (many inhibitors inhibitor) [cell @inhibitor_ _] {name: inhibitor, min: 0, max: 0}
        -> (many srcs src) cell←[cell @src_ _?] {name: src}
        -> (many dsts dst) cell←[cell @dst_ _?] {name: dst}
      WWML
        node = D7.fetch(dev, :node)
        assert spec = Feed.spec?(node)
        assert spec.is_a?(Feed::WithoutInhibitors)

        Feed.patch?(spec.variant, dev.single, src, dst)
      end

      rule(<<-WWML) do |dev, src, dst|
      [transfer inhibitors←((%past @_)) srcs←((%past @_ min: 1)) pattern_ @dst_ template_] dev
        -> (many inhibitors inhibitor) [cell @inhibitor_ _] {name: inhibitor, min: 0, max: 0}
        -> (many srcs src) [cell @src_ value_] {name: src}
        -> (one dst) cell←[cell @dst_] {name: dst}
      WWML
        dev_srcs, pattern, template = D7.fetch(dev, :srcs, :pattern, :template)

        permutation = D7.permutation(dev.single, src, :src, arranged_like_in: dev_srcs.items)

        matchee = Term::Dict.build do |commit|
          permutation.each do |index|
            src_match = src[index]
            commit << D7.fetch(src_match, :value)
          end
        end

        next unless env = M1.match?(pattern, Term.of(matchee))

        expansion = Alloy.render_rep(template, locals: env)
        unless expansion.empty?
          instance = Term.collapse(expansion)
        end

        # instance : Term?

        D7.patches(
          D7.patch(src, {2, nil}),
          D7.patch(dst, {2, instance}),
        )
      end

      rule(<<-WWML) do |dev, src, res|
      [backsys srcs←((%past @_ min: 0)) resources←((%past @_ min: 0)) restab_dict rules_dict] dev
        -> (many srcs src) [cell @src_ value_] {name: src}
        -> (many resources resource) [cell @resource_ _?] {name: res, min: 0}
      WWML
        dev_srcs, restab, rules = D7.fetch(dev, :srcs, :restab, :rules)

        permutation = D7.permutation(dev.single, src, :src, arranged_like_in: dev_srcs.items)

        res_env = {} of D7::AbsEdge => Term

        res.each do |match|
          Term.matchpi?(match.node.term, %{[cell @edge_ value_]}) do
            res_env[D7.resolve(edge, wrt: match)] = value
          end
        end

        matchee = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << D7.fetch(src[index], :value)
          end

          restab.each_entry do |key, value_local|
            value = D7.resolve(value_local, wrt: dev.single)
            commit.with(key, res_env[value]?)
          end
        end

        backsys = rules.items.compact_map do |rule|
          Term.matchpi?(rule, %{[backmap pattern_ backspec_]}) do
            {pattern, backspec}
          end
        end

        next unless rep = M1.backmap?(backsys, Term.of(matchee))
        next unless rep.type.dict? && rep.itemsize == matchee.itemsize

        patches = Pf::Kit.stack_array(D7::Patch, 8)

        # Generate patches for the itemspart.
        dev_srcs.items.zip(rep.items) do |edge_local, item|
          edge = D7.resolve(edge_local, wrt: dev.single)
          dst = D7.find(src, where: :src, eq: edge)
          patches << D7.patch(dst, {2, item})
        end

        # Generate patches for the pairspart.
        #
        # The pairspart corresponds to resources. Adding a resource is adding
        # a pair is filling its cell. Removing a resource is removing its pair
        # its emptying its cell. Modifying a pair works like in the itemspart:
        # the cell's content is modified.
        restab.each_entry do |key, edge_local|
          edge = D7.resolve(edge_local, wrt: dev.single)
          value0 = res_env[edge]?
          value1 = rep[key]?

          case {value0, value1}
          in {Nil, Nil}
            # No change
          in {Term, Nil}
            # It may happen that the resource cell does not actually exist, as in
            # the example below:
            #
            #   (cell @x 100)
            #   (backsys {@:x @:y}
            #     {¦ x_ -y_} <> {y: ^x})
            #
            # Note how @y is absent, -y_ succeeds and sets y: 100 which we read with
            # rep[key] above. However, we don't actually have anywhere to write! That's
            # why we use a non-raising find here.
            next unless dst = D7.find?(res, where: :resource, eq: edge)

            # Clear cell
            patches << D7.patch(dst, {2, nil})
          in {Nil, Term}, {Term, Term}
            next if value0 == value1 # No change

            # Ditto: the resource cell may not actually exist and we must handle
            # that gracefully.
            next unless dst = D7.find?(res, where: :resource, eq: edge)

            # Fill/modify cell
            patches << D7.patch(dst, {2, value1})
          end
        end

        D7.patches(patches)
      end

      rule(<<-WWML) do |dev, target|
      [rewriter spec_ @target_ rules_dict] dev
        -> (one target) [cell @target_ value_] {name: target}
      WWML
        spec, rules = D7.fetch(dev, :spec, :rules)
        input = D7.fetch(target, :value)

        rewriter = Rho.rewriter(spec, rules)
        next unless rewriter.finite?

        # Rewrite the input term.
        output = Rho.rewrite(rewriter, Term.of(input))

        D7.patch(target, {2, output})
      end

      rule(<<-WWML) do |dev, spec, itemcell, paircell|
      [rewriter @spec_ itemsrcs←((%past @_ min: 0)) pairsrcs←((%past @_ min: 0)) pairtab_dict rules_dict] dev
        -> (one spec) [cell @spec_ term_] {name: spec}
        -> (many itemsrcs itemsrc) [cell @itemsrc_ value_] {name: itemcell}
        -> (many pairsrcs pairsrc) [cell @pairsrc_ _?] {name: paircell, min: 0}
      WWML
        itemsrcs, pairtab, rules = D7.fetch(dev, :itemsrcs, :pairtab, :rules)
        spec_term = D7.fetch(spec, :term)

        permutation = D7.permutation(dev.single, itemcell, :itemsrc, arranged_like_in: itemsrcs.items)
        rewriter = Rho.rewriter(spec_term, rules)
        next unless rewriter.finite?

        # Record which cells from pairsrc were found.
        pair_values = {} of D7::AbsEdge => Term

        paircell.each do |match|
          Term.matchpi?(match.node.term, %{[cell @pairsrc_ value_]}) do
            pair_values[D7.resolve(pairsrc, wrt: match)] = value
          end
        end

        # Prepare the input term.
        input = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << D7.fetch(itemcell[index], :value)
          end

          # For example, in pairtab entry `x: @foo`, `x` is the input key we want
          # to define, and `@foo` is the edge whose value we should use.
          pairtab.each_entry do |key, local_pairsrc|
            pairsrc = D7.resolve(local_pairsrc, wrt: dev.single)
            commit.with(key, pair_values[pairsrc]?)
          end
        end

        # Rewrite the input term.
        output = Rho.rewrite(rewriter, Term.of(input))

        # Returning a non-dict or removing one of items is something we simply can't
        # handle in any sensible way, it's quite late but we still can bail out.
        next unless output = output.as_d?
        next unless output.itemsize == input.itemsize

        # Unpack the output term and produce the appropriate patches to itemsrc
        # and paircell(s).

        patches = Pf::Kit.stack_array(D7::Patch, 8)

        # Prepare patches for the itemspart.
        itemsrcs.items.zip(output.items) do |itemsrc_local, item|
          itemsrc = D7.resolve(itemsrc_local, wrt: dev.single)
          target = D7.find(itemcell, where: :itemsrc, eq: itemsrc)
          patches << D7.patch(target, {2, item})
        end

        # Prepare patches for the pairspart.
        pairtab.each_entry do |key, local_pairsrc|
          pairsrc = D7.resolve(local_pairsrc, wrt: dev.single)
          value0 = pair_values[pairsrc]?
          value1 = output[key]?

          case {value0, value1}
          in {Nil, Nil}
            # Absent in both, no change.
          in {Term, Nil}
            # It may happen that the pair cell does not actually exist, as in
            # the example below:
            #
            #   (cell @x 100)
            #   (backsys {@:x @:y}
            #     {¦ x_ -y_} <> {y: ^x})
            #
            # Note how @y is absent, -y_ succeeds and sets y: 100 which we read with
            # rep[key] above. However, we don't actually have anywhere to write! We
            # skip such writes.
            target = D7.find?(paircell, where: :pairsrc, eq: pairsrc)
          in {Nil, Term}, {Term, Term}
            next if value0 == value1 # No change

            # Ditto: the pair cell may not actually exist and we must handle
            # that gracefully.
            target = D7.find?(paircell, where: :pairsrc, eq: pairsrc)
          end

          next unless target

          patches << D7.patch(target, {2, value1})
        end

        D7.patches(patches)
      end

      rule %{[delay 1 successor_] dev} do |dev|
        successor = D7.fetch(dev, :successor)
        D7.replace(dev, successor)
      end

      rule %{[delay n←(%number +i32!) _] dev} do |dev|
        n = D7.fetch(dev, :n)
        D7.patch(dev, {1, n - 1})
      end

      rule %{[delay n←(%number +i32!)] dev} do |dev|
        n = D7.fetch(dev, :n)
        D7.patch(dev, {1, n - 1})
      end

      rule(<<-WWML) do |dev, src, dst|
      [view (srcs←((%past @_ min: 1)) pattern_ @dst_) template_] dev
        -> (many srcs src) [cell @src_ term_] {name: src, min: 0}
        -> (one dst) [cell @dst_ _?] {name: dst}
      WWML
        dev_srcs, pattern, template = D7.fetch(dev, :srcs, :pattern, :template)
        if src.size < dev_srcs.size
          # Less edges than we require. This means the view is invalid now
          # since some source cells have disappeared. So we empty the dst cell.
          next D7.patch(dst, {2, nil})
        end

        assert src.size == dev_srcs.size

        # Fetch src values.
        permutation = D7.permutation(dev.single, src, :src, arranged_like_in: dev_srcs.items)

        src_terms = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << D7.fetch(src[index], :term)
          end
        end

        matchee = Term.of(src_terms)
        unless env = M1.match?(pattern, matchee)
          # Pattern mismatch. Clear the dst cell: the view is invalid.
          next D7.patch(dst, {2, nil})
        end

        expansion = Alloy.render_rep(template, locals: env)
        unless expansion.empty?
          instance = Term.collapse(expansion)
        end

        D7.patch(dst, {2, instance})
      end

      # Extension without dst pattern. It requires an existing dst cell. The dst
      # cell may be empty.
      rule(<<-WWML) do |dev, src, dst|
      [extension (srcs←((%past @_ min: 1)) pattern_ @dst_) template_] dev
        -> (many srcs src) [cell @src_ term_] {name: src, min: 0}
        -> (one dst) [cell @dst_ _?] {name: dst}
      WWML
        dev_srcs, pattern, template = D7.fetch(dev, :srcs, :pattern, :template)
        if src.size < dev_srcs.size
          # Less edges than we require. This means the extension is invalid now
          # now since some source cells have disappeared. So we empty the dst cell.
          next D7.patch(dst, {2, nil})
        end

        assert src.size == dev_srcs.size

        # Fetch src values.
        permutation = D7.permutation(dev.single, src, :src, arranged_like_in: dev_srcs.items)

        src_terms = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << D7.fetch(src[index], :term)
          end
        end

        matchee = Term.of(src_terms)
        unless env = M1.match?(pattern, matchee)
          # Pattern mismatch. Clear the dst cell: the extension is invalid.
          next D7.patch(dst, {2, nil})
        end

        expansion = Alloy.render_rep(template, locals: env)
        unless expansion.empty?
          instance = Term.collapse(expansion)
        end

        # instance : Term?
        dst_term = D7.part?(dst, 2)

        next if instance && dst_term && Term.extension?(dst_term, of: instance)

        D7.patch(dst, {2, instance})
      end

      # Extension with dst pattern. It requires an existing nonempty dst cell.
      rule(<<-WWML) do |dev, src, dst|
      [extension (srcs←((%past @_ min: 1)) src-pattern_ @dst_ dst-pattern_) template_] dev
        -> (many srcs src) [cell @src_ term_] {name: src, min: 0}
        -> (one dst) [cell @dst_ term_] {name: dst}
      WWML
        dev_srcs, src_pattern, dst_pattern, template = D7.fetch(dev, :srcs, :"src-pattern", :"dst-pattern", :template)

        # NOTE: As opposed to the dst pattern-less extension variant above, this one simply
        # abstains if the pattern does not match or if too few edges. This seems reasonable
        # to me: an extension with a dst pattern looks like an "observer", and only commits
        # if it's absolutely sure. The way it looks to me, it does not necessarily "own"
        # the extension all the time. You can consider the pattern as a kind of "password"
        # that takes into account both the inputs and the output of the node. Only if
        # all checks out is the extension free to modify the dst cell arbitrarily.

        next if src.size < dev_srcs.size

        assert src.size == dev_srcs.size

        # Fetch src values.
        permutation = D7.permutation(dev.single, src, :src, arranged_like_in: dev_srcs.items)

        src_terms = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << D7.fetch(src[index], :term)
          end
        end

        # Fetch dst term.
        dst_term = D7.fetch(dst, :term)

        # Combine them using %all. This lets src_pattern constrain/learn from
        # dst_pattern and vice versa.
        pattern = Term.of(src_pattern, dst_pattern)
        matchee = Term.of(src_terms, dst_term)
        next unless env = M1.match?(pattern, Term.of(matchee))

        expansion = Alloy.render_rep(template, locals: env)
        unless expansion.empty?
          instance = Term.collapse(expansion)
        end

        # instance : Term?
        next if instance && Term.extension?(dst_term, of: instance)

        D7.patch(dst, {2, instance})
      end

      # TODO: Right now we do not support infinite rewriters since we
      # don't know whether they'll terminate. We should schedule a task
      # of some sort here; i.e., the rewrite may or may not complete
      # in a single tick. We only guarantee completion in one tick for
      # finite rewriters.
      rule(<<-WWML) do |dev, src, spec, dst|
      [rewriter (@input_ -> @spec_ -> @output_) rules_*] dev
        -> (one input) [cell @input_ term_] {name: src}
        -> (one spec) [cell @spec_ term_] {name: spec}
        -> (one output) [cell @output_] {name: dst}
      WWML
        rules = D7.fetch(dev, :rules)
        spec_term = D7.fetch(spec, :term)
        src_term = D7.fetch(src, :term)

        rewriter = Rho.rewriter(spec_term, rules)
        next unless rewriter.finite?

        out_term = Rho.rewrite(rewriter, src_term)

        D7.patches(
          D7.patch(src, {2, nil}),
          D7.patch(dst, {2, out_term}),
        )
      end

      rule(<<-WWML) do |dev, src, spec, dst|
      [rewriter (@input_ - @spec_ -  @output_) rules_*] dev
        -> (one input) [cell @input_ _?] {name: src, min: 0}
        -> (one spec) [cell @spec_ _?] {name: spec, min: 0}
        -> (one output) [cell @output_ _?] {name: dst}
      WWML
        rules = D7.fetch(dev, :rules)

        src_term = src.present? ? D7.part?(src, 2) : nil
        spec_term = spec.present? ? D7.part?(spec, 2) : nil

        if src_term.nil? || spec_term.nil?
          next D7.patch(dst, {2, nil})
        end

        rewriter = Rho.rewriter(spec_term, rules)
        next unless rewriter.finite?

        out_term = Rho.rewrite(rewriter, src_term)

        dst_term = D7.part?(dst, 2)
        next if dst_term && Term.extension?(dst_term, of: out_term)

        D7.patch(dst, {2, out_term})
      end
    end
  end

  def backref_step(parser : D7::Parser, circuit : Term, prepass) : Slice(Term)
    D7.step(parser, circuit) do |hg|
      prepass.call(hg) do |hg|
        D7::Regime.merge(hg, proposals: backref_step(hg))
      end
    end
  end

  def backref_step(hg : D7::Hypergraph) : Indexable(D7::Patch)
    hg.propose(:backref) do |node|
      Term.case(node.term) do
        matchpi %{[backref header←(input←(%'edge name_) _*) payload_]} do
          patterns = header.items.move(1)
          backref_step(hg, node, node.resolve(input), name, patterns, payload)
        end

        otherwise { }
      end
    end
  end

  def backref_step(hg : D7::Hypergraph, node : D7::Node, input : D7::AbsEdge, name : Term, patterns : Indexable(Term), payload : Term) : D7::Patch?
    return unless source = Rack.cell?(hg, input)

    if value = source.value?
      backspec = Term.of(Term[].with(name, value))
    else
      backspec = Term.of(Term[].with({name}, Term[]))
    end

    result = patterns.leftmost? do |pattern|
      M1.backmap?(pattern, backspec, payload)
    end

    return unless result

    D7.patch(node, {2, result})
  end
end
