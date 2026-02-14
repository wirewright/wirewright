module Ww::Rack
  # Returns the main Rack pass.
  def pass : D7::Pass
    D7::Pass.new { |clf, circuit| step(clf.call(circuit), circuit) }
  end

  # :nodoc:
  #
  # *Prepass* is Rack's decorator over the general hypergraph solving process.
  # The most important thing it does is it walks the `part` subtree of root
  # cells and constructs child `cell`s with appropriate values, if possible; and
  # then after *fn* runs, it absorbs & merges child cells back into the root.
  def prepass(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
    ControlSpace.prepass(hg) do |hg|
      Part.prepass(hg, &fn)
    end
  end

  private def step(clf : D7::Classifier, circuit : Term) : Slice(Term)
    D7.case(clf, circuit, decorator: prepass) do
      rule(<<-WWML) do |tgt|
      [discard @u_] dev
        -> (one u) [cell @u_ value_] {name: tgt, max: ∞}
      WWML
        D7.patch(tgt, {2, nil})
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

        Feed.patch?(spec.variant, src, dst)
      end

      rule(<<-WWML) do |dev, src, dst|
      [transfer inhibitors←((%past @_)) srcs←((%past @_ min: 1)) pattern_ @dst_ template_] dev
        -> (many inhibitors inhibitor) [cell @inhibitor_ _] {name: inhibitor, min: 0, max: 0}
        -> (many srcs src) [cell @src_ value_] {name: src}
        -> (one dst) cell←[cell @dst_] {name: dst}
      WWML
        srcs, pattern, template = D7.fetch(dev, :srcs, :pattern, :template)

        permutation = D7.permutation(src, :src, goal: srcs.items)

        matchee = Term::Dict.build do |commit|
          permutation.each do |index|
            src_match = src[index]
            commit << src_match.env[:value]
          end
        end

        next unless env = M1.match?(pattern, Term.of(matchee))

        expansion, _ = Alloy.render0(env, template, severity: :quiet)
        next if expansion.is_a?(Alloy::Err)

        unless expansion.is_a?(Alloy::Splice) && expansion.offspring.empty?
          instance = Alloy.collapse(expansion)
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
        src_edges, restab, rules = D7.fetch(dev, :srcs, :restab, :rules)

        permutation = D7.permutation(src, :src, goal: src_edges.items)

        matchee = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << src[index].env[:value]
          end
        end

        res_env = {} of Term => Term

        res.each do |match|
          Term.case(match.node.term) do
            matchpi %{[cell @dst_ value_]} do
              res_env[dst] = value
            end

            matchpi %{[cell @dst_]} { }
          end
        end

        restab.each_entry do |key, value|
          matchee = matchee.with(key, res_env[value]?)
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
        src_edges.items.zip(rep.items) do |edge, item|
          dst = D7.find(src, where: :src, eq: edge)
          patches << D7.patch(dst, {2, item})
        end

        # Generate patches for the pairspart.
        #
        # The pairspart corresponds to resources. Adding a resource is adding
        # a pair is filling its cell. Removing a resource is removing its pair
        # its emptying its cell. Modifying a pair works like in the itemspart:
        # the cell's content is modified.
        restab.each_entry do |key, edge|
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

      rule %{[delay 1 successor_] dev} do |dev|
        successor = D7.fetch(dev, :successor)

        D7.replace(dev, successor)
      end

      rule %{[delay n←(%number +i32!) _] dev} do |dev|
        n = D7.fetch(dev, :n)

        D7.patch(dev, {1, n - 1})
      end

      # View without dst pattern. It requires an existing dst cell. The dst
      # cell may be empty.
      rule(<<-WWML) do |dev, src, dst|
      [view (srcs←((%past @_ min: 1)) pattern_ @dst_) template_] dev
        -> (many srcs src) [cell @src_ value_] {name: src, min: 0}
        -> (one dst) [cell @dst_ _?] {name: dst}
      WWML
        src_edges, pattern, template = D7.fetch(dev, :srcs, :pattern, :template)
        if src.size < src_edges.size
          # Less edges than we require. This means the view is invalid now
          # now since some source cells have disappeared. So we empty the dst cell.
          next D7.patch(dst, {2, nil})
        end

        assert src.size == src_edges.size

        # Fetch src values.
        permutation = D7.permutation(src, :src, goal: src_edges.items)

        src_values = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << src[index].env[:value]
          end
        end

        matchee = Term.of(src_values)
        unless env = M1.match?(pattern, matchee)
          # Pattern mismatch. Clear the dst cell: the view is invalid.
          next D7.patch(dst, {2, nil})
        end

        expansion, _ = Alloy.render0(env, template, severity: :quiet)
        if expansion.is_a?(Alloy::Err)
          # Alloy error. Clear the dst cell: the view is invalid.
          next D7.patch(dst, {2, nil})
        end

        unless expansion.is_a?(Alloy::Splice) && expansion.offspring.empty?
          instance = Alloy.collapse(expansion)
        end

        # instance : Term?

        D7.patch(dst, {2, instance})
      end

      # View with dst pattern. It requires an existing nonempty dst cell.
      rule(<<-WWML) do |dev, src, dst|
      [view (srcs←((%past @_ min: 1)) src-pattern_ @dst_ dst-pattern_) template_] dev
        -> (many srcs src) [cell @src_ value_] {name: src, min: 0}
        -> (one dst) [cell @dst_ value_] {name: dst}
      WWML
        src_edges, src_pattern, dst_pattern, template = D7.fetch(dev, :srcs, :"src-pattern", :"dst-pattern", :template)

        # NOTE: As opposed to the dst pattern-less view variant above, this one simply
        # abstains if the pattern does not match or if too few edges. This seems reasonable
        # to me: a view with a dst pattern looks like an "observer", and only commits
        # if it's absolutely sure. The way it looks to me, it does not necessarily "own"
        # the view all the time. You can consider the pattern as a kind of "password"
        # that takes into account both the inputs and the output of the node. Only if
        # all checks out is the view free to modify the dst cell arbitrarily.

        next if src.size < src_edges.size

        assert src.size == src_edges.size

        # Fetch src values.
        permutation = D7.permutation(src, :src, goal: src_edges.items)

        src_values = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << src[index].env[:value]
          end
        end

        # Fetch dst value.
        dst_value = D7.fetch(dst, :value)

        # Combine them using %all. This lets src_pattern constrain/learn from
        # dst_pattern and vice versa.
        pattern = Term.of(src_pattern, dst_pattern)
        matchee = Term.of(src_values, dst_value)
        next unless env = M1.match?(pattern, Term.of(matchee))

        expansion, _ = Alloy.render0(env, template, severity: :quiet)
        if expansion.is_a?(Alloy::Err)
          next D7.patch(dst, {2, nil})
        end

        unless expansion.is_a?(Alloy::Splice) && expansion.offspring.empty?
          instance = Alloy.collapse(expansion)
        end

        # instance : Term?

        D7.patch(dst, {2, instance})
      end

      rule(<<-WWML) do |dev, src, ref, dst|
      [view (srcs←((%past @_ min: 1)) pattern_ (@ref_ @dst_)) template_] dev
        -> (many srcs src) [cell @src_ value_] {name: src, min: 0}
        -> (one ref) [cell @ref_ _?] {name: ref}
        -> (one dst) [cell @dst_ _?] {name: dst}
      WWML
        src_edges, pattern, template = D7.fetch(dev, :srcs, :pattern, :template)
        if src.size < src_edges.size
          # Less edges than we require. This means the view is invalid now
          # now since some source cells have disappeared. So we empty the dst cell.
          next D7.patches(D7.patch(ref, {2, nil}), D7.patch(dst, {2, nil}))
        end

        assert src.size == src_edges.size

        # Fetch src values.
        permutation = D7.permutation(src, :src, goal: src_edges.items)

        src_values = Term::Dict.build do |commit|
          permutation.each do |index|
            commit << src[index].env[:value]
          end
        end

        matchee = Term.of(src_values)
        unless env = M1.match?(pattern, matchee)
          # Pattern mismatch. Clear the dst cell: the view is invalid.
          next D7.patches(D7.patch(ref, {2, nil}), D7.patch(dst, {2, nil}))
        end

        expansion, _ = Alloy.render0(env, template, severity: :quiet)
        if expansion.is_a?(Alloy::Err)
          # Alloy error. Clear the dst cell: the view is invalid.
          next D7.patches(D7.patch(ref, {2, nil}), D7.patch(dst, {2, nil}))
        end

        unless expansion.is_a?(Alloy::Splice) && expansion.offspring.empty?
          instance = Alloy.collapse(expansion)
        end

        next if instance == D7.node(ref).term[2]?

        # instance : Term?

        D7.patches(D7.patch(ref, {2, instance}), D7.patch(dst, {2, instance}))
      end
    end
  end
end
