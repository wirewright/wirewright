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

      rule(<<-WWML) do |dev, src|
      [journal header←(@edge_ _?) events_*] dev
        -> (one edge) [cell @edge_ _?] {name: src, min: 0, max: 1}
      WWML
        header, events = D7.fetch(dev, :header, :events)

        if header.itemsize == 2
          ref = header.items.last
        end

        goal = src.present? ? D7.part?(src, 2) : nil
        next if ref.nil? && goal.nil?

        if goal.nil?
          assert ref
          desc = Term.of(:disappeared, ref)
        elsif ref.nil?
          assert goal
          desc = Term.of(:appeared, goal)
        elsif ref != goal # ref : Term, goal : Term
          desc = Term.of(:changed, goal)
        end

        D7.patch(dev, {1, 1, goal}, {2 + events.itemsize, desc})
      end
    end
  end

  def rig_step(parser : D7::Parser, circuit : Term, prepass) : Slice(Term)
    D7.step(parser, circuit, required_heads: {Term.of(:rig)}) do |hg|
      prepass.call(hg) do |hg|
        D7::Regime.merge(hg, proposals: rig_step(hg))
      end
    end
  end

  def rig_step(hg : D7::Hypergraph) : Indexable(D7::Patch)
    hg.propose(:rig) do |node|
      Term.case(node.term) do
        matchpi %{[rig header←(@input_ selector_ -> _*) payload_]} do
          patterns = header.items.move(2)
          rig_step(hg, node, hg.resolve(node.addr, input), selector, patterns, payload)
        end

        otherwise { }
      end
    end
  end

  def rig_step(hg : D7::Hypergraph, node : D7::Node, input : D7::AbsEdge, selector : Term, patterns : Indexable(Term), payload : Term) : D7::Patch?
    return unless source = Rack.cell?(hg, input)

    # If value is present and matches, synthesize a backspec that plugs stuff in.
    backspec = pass do
      next unless value = source.value?
      next unless env = M1.match?(selector, value)

      Term::Dict.build do |commit|
        env.each_entry do |key, value|
          commit.with(key, {:"^verbatim", value})
        end
      end
    end

    if backspec
      result = patterns.leftmost? do |pattern|
        M1.backmap?(pattern, Term.of(backspec), payload)
      end

      return unless result
      return D7.patch(node, {2, result})
    end

    # If value is absent or doesn't match, we have to clear stuff. But we
    # don't know the names of things because *selector* doesn't match. So
    # instead we match each *pattern* in turn to figure out the captures
    # to clear.
    env_log_list = patterns.leftmost? do |pattern|
      matches = M1.matches_and_logs(Term[], M1.operator(pattern), payload)
      matches.present? ? matches : nil
    end

    return unless env_log_list

    backspec = Term::Dict.build do |commit|
      env_log_list.each do |env, _|
        env.each_entry do |capture, _|
          commit.with({capture}, Term[])
        end
      end
    end

    rep = M1.backmapR(Slice[{env_log_list, backspec}], payload)
    result = Term.collapse(rep)
    D7.patch(node, {2, result})
  end
end
