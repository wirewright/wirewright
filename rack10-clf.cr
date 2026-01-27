module ::Ww::D7::Kit
  extend self

  def collate(grp : Ngrp, keys : Indexable(Term), selector : Term, value : Term) : Term
    assert grp.size == keys.size

    argmt = Term::Dict.build do |commit|
      keys.each do |key|
        grp.each do |_, node|
          next unless node.env[selector] == key

          commit << node.env[value]
        end
      end
    end

    Term.of(argmt)
  end

  alias Ncap = Regime::NodeCapture
  alias Ngrp = Regime::NodeCaptureGroup

  def first(grp : Ngrp) : Ncap
    grp.first[1]
  end

  def id(grp : Ngrp) : NodeId
    grp.first[0]
  end

  def addr(grp : Ngrp) : NodeAddr
    first(grp).addr
  end

  def first(grp : Ngrp, key)
    grp.leftmost? { |_, capture| capture.env[key] } || raise Enumerable::EmptyError.new
  end

  def first(grp : Ngrp, *keys)
    keys.map { |key| first(grp, key) }
  end

  module Sink::Bag
  end

  def collect(grp : Ngrp, key, sink : Sink::Bag.class, &)
    Term::Dict.build do |commit|
      grp.each do |_, capture|
        orig = capture.env[key]
        next unless value = yield orig

        commit.with(value, (commit[value]? || 0) + 1)
      end
    end
  end

  def collect(grp, key, sink)
    collect(grp, key, sink, &.itself)
  end

  def patch(grp : Ngrp, &)
    grp.to_h do |id, capture|
      {id, Term.of(yield capture.node)}
    end
  end

  def patch(grp : Ngrp, *morphseq : Tuple)
    patch(grp, &.morph(*morphseq))
  end

  def patches(*patches)
    memo = {} of NodeId => Term

    patches.each do |patch|
      memo.merge!(patch) do |id, rep0, rep1|
        raise ArgumentError.new("patches conflict over node #{id}: #{rep0} vs. #{rep1}")
      end
    end

    memo
  end

  def patches(objects : Enumerable, &)
    memo = {} of NodeId => Term

    objects.each_with_index do |object, index|
      id, rep = yield object, index
      unless memo.put?(id, Term.of(rep))
        raise ArgumentError.new("patches conflict over node #{id}")
      end
    end

    memo
  end

  def join(objects : Enumerable, *, offset = 0, &)
    memo = {} of NodeId => Term

    objects.each_with_index(offset: offset) do |object, index|
      next unless patch = yield object, index

      memo.merge!(patch) do |id, rep0, rep1|
        unless rep0 == rep1
          raise ArgumentError.new("patches conflict over node #{id}: #{rep0} vs. #{rep1}")
        end
        rep0
      end
    end

    memo
  end
end

module ::Ww::Rack
  extend self
  include D7::Kit

  @@clf : D7::Classifier?
  @@cache = SyncCache(Term, D7::Feature).new(512, preallocate: true)

  def clf : D7::Classifier
    @@clf ||= ->(node : Term) do
      @@cache.put_if_absent(node) { classify(node) }
    end
  end

  # Returns `true` if *node* contains one or more cursor kernel.
  def editing?(node : Term) : Bool
    Term.case(node) do
      matchpi %{(I _* ⍊ structural user)} do
        true
      end

      matchpi %{_dict} do
        return false unless node.probably_includes?(Term[:I])

        node.ee.any? { |_, value| editing?(value) }
      end

      otherwise { false }
    end
  end

  def classify(node : Term) : D7::Feature
    clf = classify0(node)
    unless clf.is_a?(D7::Gnd) || clf.is_a?(D7::Mixture)
      return clf
    end

    if editing?(clf.node)
      return D7.inert(node)
    end

    clf
  end

  # :nodoc:
  def classify0(node : Term) : D7::Feature
    PatternSet.case(node) do
      matchpi %{[circuit (@edge_ pattern_) _]} do
        D7.circuit(node.as_d, 2...3) do |node|
          _, _, value0 = node
          _, capture = edge
          next D7.inert(node) unless M1.probably_matches?(pattern, value0)
          next D7.inert(node) unless env = M1.match?(pattern, value0)

          unless view0 = env[capture]?
            next D7.inert(node)
          end

          mix0 = Term.of(:cell, edge, view0)

          D7.mixture(node, mix0) do |node0, mix1|
            backspec = Term[]

            Term.case(mix1) do
              matchpi %{(cell @_)} { backspec = Term.entries({ { {capture}, Term[] } }) }
              matchpi %{(cell @_ view1_)} { backspec = Term.entries({ { {capture}, view1 } }) }
            end

            value1 = M1.backmap(pattern, Term.of(backspec), value0)

            Term.of(node0.morph({2, value1}))
          end
        end
      end

      matchpi %{[circuit @edge_ _]} do
        D7.circuit(node.as_d, 2...3) do |node1|
          _, _, value0 = node

          mix0 = Term.of(:cell, edge, value0)

          D7.mixture(node, mix0) do |node0, mix1|
            Term.of_case(mix1) do
              matchpi %{(cell @_)} { node0.morph({2, nil}) }
              matchpi %{(cell @_ value1_)} { node0.morph({2, value1}) }
            end
          end
        end
      end

      matchpi %{[circuit @edge_]}, %{[frag @edge_]} do
        D7.mixture(node, Term.of(:cell, edge)) do |node0, view|
          Term.of_case(view) do
            matchpi %{(cell @_)} { node0 }
            matchpi %{(cell @_ value1_)} { node0.morph({2, value1}) }
          end
        end
      end

      matchpi %{[frag @edge_ value_]} do
        mix0 = Term.of(:group,
          Term.of(:cell, edge),
          Term.of(:group, value))

        D7.mixture(node, mix0) do |node0, mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_ value1_)⟩} { node0.morph({2, value1}) }
            matchpi %{⟨(group)⟩} { node0.morph({2, nil}) }
            matchpi %{⟨(group value1_)⟩} { node0.morph({2, value1}) }
          end
        end
      end

      matchpi %{[cell @edge_ _?]} do
        D7.gnd(node, edge)
      end

      matchpi %{[feed @u_ @v_]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[feed (%group edges_ (%past @_ min: 3))]} do
        defn = Term::Dict.build do |commit|
          commit << :group
          edges.items.each_cons_pair do |a, b|
            commit << Term.of(:feed, a, b)
          end
        end

        D7.mixture(node, defn) { |orig, _| orig }
      end

      matchpi %{[feed @u_ (over @v_)]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[feed (copy @u_) @v_]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[feed (copy @u_) (over @v_)]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[discard @edge_]} do
        D7.gnd(node, edge)
      end

      matchpi %{[group _*]} do
        D7.parent(node.as_d, 1...node.itemsize)
      end

      matchpi %{[unit _*]} do
        D7.circuit(node.as_d, 1...node.itemsize) { |node1| D7.inert(node1) }
      end

      matchpi %{[delay (%number +i32) _]} do
        D7.gnd(node)
      end

      matchpi %{[module bindings_dict _*]} do
        D7.scope(bindings.as_d, D7.parent(node.as_d, 2...node.itemsize))
      end

      matchpi %{[transfer (@u_ _ @v_) _]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[view (@u_ _ @v_) _]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[view (us←((%past @_ min: 1)) _ @v_) _]} do
        edges = [] of Term
        us.items.each_with_index do |u, i|
          edges << u
        end
        edges << v

        D7.gnd(node, edges)
      end

      matchpi %{[view (us←((%past @_ min: 1)) _ (@v_ _)) _]} do
        edges = [] of Term
        us.items.each_with_index do |u, i|
          edges << u
        end
        edges << v

        D7.gnd(node, edges)
      end

      matchpi %{[view (@u_ _ (@v_ _)) _]} do
        D7.gnd(node, u, v)
      end

      matchpi(
        %{[view (ml @u_) (term @v_)]},
        %{[view (ml @u_) (terms @v_)]},
        %{[view (ml @u_) (document @v_)]},
      ) do
        D7.gnd(node, u, v)
      end

      matchpi %{[view [alloy @base_ @globals_ @vars_ @view_] (composite @comp_)]} do
        D7.gnd(node, base, globals, vars, view, comp)
      end

      matchpi %{[view [alloy @vars_ @template_] (instance @instance_)]} do
        D7.gnd(node, vars, template, instance)
      end

      matchpi %{[log (@u_ _ @v_) _]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[fb (@edge_ _) _]} do
        D7.gnd(node, edge)
      end

      matchpi %{[fb (us←((%past @_ min: 1)) _) _]} do
        D7.gnd(node, edges: us.items.map_with_index { |u, i| u })
      end

      matchpi %{[queue (@front_ @back_ ⍊ min_: (%optional 1 (%number +i32!)) max_: (%optional ∞ (%any° (%number +i32!) ∞))) queue_dict]}, min: Int32 do
        defn = Term::Dict.build do |commit|
          commit << :group

          # Check if we are allowed to dequeue.
          if 0 < min <= queue.itemsize
            commit << Term.of(:cell, front, queue.items.first, front: true)
          end

          # Check if we are allowed to enqueue.
          if max == Term.of(:∞) || queue.itemsize < max.to(Int32)
            commit << Term.of(:cell, back, back: true)
          end
        end

        D7.mixture(node, Term.of(defn)) do |orig, view|
          rest = queue

          Term.case(view) do
            # Dequeue.
            matchpi %{⟨(cell @_ ⍊ front)⟩} do
              rest = rest.items.move(1).collect
              continue
            end

            # Sync.
            matchpi %{⟨(cell @_ x_ ⍊ front)⟩} do
              rest = rest.morph({0, x})
              continue
            end

            # Enqueue.
            matchpi %{⟨(cell @_ x_ ⍊ back)⟩} do
              rest = rest.append(x)
            end

            otherwise { }
          end

          Term.of(orig.morph({2, rest}))
        end
      end

      # Chan sensor
      matchpi %{[sensor (_ _ @u_) _]} do
        D7.gnd(node, u)
      end

      # View sensor
      matchpi %{[sensor* (_ _ @u_) _]} do
        D7.gnd(node, u)
      end

      # Fused chan sensor-cell
      matchpi %{[(sensor cell) (tspace_ pattern_ @edge_) _?]} do
        value0 = node[2]?

        mix0 = Term.of(:group,
          Term.of(:cell, edge, value0),
          Term.of(:sensor, {tspace, pattern, edge}, {:^, edge[1]}),
        )

        D7.mixture(node, mix0) do |orig, mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_)⟩} { Term.of(orig.morph({2, nil})) }
            matchpi %{⟨(cell @_ value1_)⟩} { Term.of(orig.morph({2, value1})) }
          end
        end
      end

      # Fused view sensor-cell
      matchpi %{[(sensor* cell) (tspace_ pattern_ @edge_) _?]} do
        value0 = node[2]?

        mix0 = Term.of(:group,
          Term.of(:cell, edge, value0),
          Term.of(:"sensor*", {tspace, pattern, edge}, {:^, edge[1]}),
        )

        D7.mixture(node, mix0) do |orig, mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_)⟩} { Term.of(orig.morph({2, nil})) }
            matchpi %{⟨(cell @_ value1_)⟩} { Term.of(orig.morph({2, value1})) }
          end
        end
      end

      # Appearance
      matchpi %{[appearance _ @u_]} do
        D7.gnd(node, u)
      end

      # Fused appearance-cell
      matchpi %{[(appearance cell) (tspace_ @edge_) _?]} do
        value0 = node[2]?

        mix0 = Term.of(:group,
          Term.of(:cell, edge, value0),
          Term.of(:appearance, tspace, edge),
        )

        D7.mixture(node, mix0) do |orig, mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_)⟩} { Term.of(orig.morph({2, nil})) }
            matchpi %{⟨(cell @_ value1_)⟩} { Term.of(orig.morph({2, value1})) }
          end
        end
      end

      matchpi %{[path @u_ _]} do
        D7.gnd(node, u)
      end

      matchpi %{[path (@u_ path_) _?]} do
        value0 = node[2]?

        mix0 = Term.of(:group,
          Term.of(:cell, u, value0),
          Term.of(:path, u, path),
        )

        D7.mixture(node, mix0) do |orig, mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_)⟩} { Term.of(orig.morph({2, nil})) }
            matchpi %{⟨(cell @_ value1_)⟩} { Term.of(orig.morph({2, value1})) }
          end
        end
      end

      matchpi %{[window (@u_ _) _*]} do
        D7.gnd(node, u)
      end

      matchpi %{[image @u_ @v_]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[image (@u_ _) @v_]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[image @u_ (@v_ _)]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[image (@u_ _) (@v_ _)]} do
        D7.gnd(node, u, v)
      end

      matchpi %{[(%any h1 h2 h3 h4 h5 h6 p code) caption_string]} do
        D7.gnd(node)
      end

      matchpi %{[(%any h1 h2 h3 h4 h5 h6 p code span) @u_]} do
        D7.gnd(node, u)
      end

      matchpi %{[rulesys us←((%past @_ min: 1)) _*]} do
        D7.gnd(node, us.items)
      end

      matchpi %{[rewriter [@ruleset_ _] @src_ @dst_]} do
        D7.gnd(node, ruleset, src, dst)
      end

      matchpi %{[microfold [@themedoc_] @src_ @dst_]} do
        D7.gnd(node, themedoc, src, dst)
      end

      otherwise { D7.inert(node) }
    end
  end
end

module ::Ww::Rack
  # TODO: instead of using Sink::Bag emit Term.compare-ordered dicts.
  def master(clf : D7::Classifier, circuit : Term)
    D7.case(clf, circuit) do
      rule %{(one dev [log (@src_ pattern_ @log_) template_]) (one src [cell @src_ input_]) (one log [cell @log_ entries_dict])} do
        pattern, template, input, entries = first(dev, :pattern), first(dev, :template), first(src, :input), first(log, :entries)
        next unless vars = M1.match?(pattern, input)

        entry = Alloy.render(vars, template)
        next if entry == entries.items.last?

        patch(log, &.morph({2, entries.append(entry)}))
      end

      # TODO: Each one of these should have their own server. Because they can be
      # very long-running.

      rule %{(one dev [view cfg←(alloy @base_ @globals_ @vars_ @view_ ¦ {% selector}) (composite @comp_)]) (one basesrc [cell @base_ term_]) (one globalsrc [cell @globals_ term_dict]) (one varsrc [cell @vars_ term_dict]) (one viewsrc [cell @view_ term_]) (one dst [cell @comp_ _?])} do
        base, globals, vars, view = first(basesrc, :term), first(globalsrc, :term), first(varsrc, :term), first(viewsrc, :term)
        selector = first(dev, :cfg)[:selector]? || Ruleset::DEFAULT_SELECTOR
        ruleset = Ruleset.select(selector, base)

        comp, issues = Alloy.compose_with_issues(ruleset, globals.as_d, Alloy.template(vars.as_d, view))
        # FIXME: store issues in msg: (...)

        patch(dst, &.morph({2, comp}))
      end

      rule %{(one dev [view (alloy @vars_ @template_) (instance @instance_)]) (one varsrc [cell @vars_ term_dict]) (one templatesrc [cell @template_ term_]) (one dst [cell @instance_ _?])} do
        vars, template = first(varsrc, :term), first(templatesrc, :term)
        instance, issues = Alloy.render_with_issues(vars.as_d, template)
        # FIXME: issues must be sent as events

        patch(dst, &.morph({2, instance}))
      end

      rule %{(one dev [view (ml @src_) (term @dst_)]) (one src [cell @src_ ml_string]) (one dst [cell @dst_ _?])} do
        ml = first(src, :ml)

        begin
          term = ML.term(ml.to(String))
          patch(dst, &.morph({2, term}))
        rescue e : ML::SyntaxError
          # TODO: store err in msg: (err ...)
          patch(dst, &.morph({2, nil}))
        end
      end

      rule %{(one dev [view (ml @src_) (terms @dst_)]) (one src [cell @src_ ml_string]) (one dst [cell @dst_ _?])} do
        ml = first(src, :ml)

        begin
          terms = ML.terms(ml.to(String))
          patch(dst, &.morph({2, terms}))
        rescue e : ML::SyntaxError
          # TODO: store err in msg: (err ...)
          patch(dst, &.morph({2, nil}))
        end
      end

      rule %{(one dev [view (ml @src_) (document @dst_)]) (one src [cell @src_ ml_string]) (one dst [cell @dst_ _?])} do
        ml = first(src, :ml)

        begin
          document = ML.document(ml.to(String))
          patch(dst, &.morph({2, document}))
        rescue e : ML::SyntaxError
          # TODO: store err in msg: (err ...)
          patch(dst, &.morph({2, nil}))
        end
      end

      # This rule handles the following situations:
      #
      #   Unicast (move to dst):
      #     (cell @src <value>)
      #     (feed @src @dst)
      #     (cell @dst)
      #
      #   Multicast:
      #     (cell @src <value>)
      #     (feed @src @dst)
      #     (cell @dst)
      #     (cell @dst)
      #     (cell @dst)
      #     ;; ...
      #
      #   Aggregate:
      #     (cell @src <value0>)
      #     (cell @src <value1>)
      #     (cell @src <value2>)
      #     ;; ...
      #     (feed @src @dst)
      #     (cell @dst)
      rule(
        %{(one dev [feed @src_ @dst_]) (many src [cell @src_ x_]) (many dst [cell @dst_])},
        %{(one dev [feed @src_ (over @dst_)]) (many src [cell @src_ x_]) (many dst [cell @dst_ _?])},
      ) do
        case {src.size, dst.size}
        when {1, _} # distribute
          x = first(src, :x)

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, x})),
          )
        when {_, 1} # aggregate
          x = collect(src, :x, Sink::Bag)

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, x})),
          )
        end

        # TODO: What to do on M:M? Move srcs to dsts? What if there are less dsts
        # that srcs? Who wins? What's the order?
      end

      # This rule handles the following situation:
      #
      #   (cell @src0 <value0>)
      #   (cell @src1 <value1>)
      #   (cell @src2 <value2>)
      #   ;; ...
      #   (feed @src0 @dst)
      #   (feed @src1 @dst)
      #   (feed @src2 @dst)
      #   ;; ...
      #   (cell @dst)
      #
      # This is nondeterministic in principle. We resolve the conflict deterministically from
      # dst's point of view by picking an src edge that is the lexicographic minimum among all
      # available src edges.
      rule %{(one dst [cell @dst_]) (link (many mid [feed @src_ @dst_] min: 2) (many src [cell @src_ x_]))} do
        id, capture = src.min { |(_, capture0), (_, capture1)| Term.compare(capture0.env[:src], capture1.env[:src]) }
        x = capture.env[:x]

        patches(
          {id => Term.of(capture.node.morph({2, nil}))},
          patch(dst, &.morph({2, x})),
        )
      end

      rule(
        %{(one dev [feed (copy @src_) @dst_]) (many src [cell @src_ x_]) (many dst [cell @dst_])},
        %{(one dev [feed (copy @src_) (over @dst_)]) (many src [cell @src_ x_]) (many dst [cell @dst_ _?])},
      ) do
        case {src.size, dst.size}
        when {1, _} # distribute
          x = first(src, :x)

          patch(dst, &.morph({2, x}))
        when {_, 1} # aggregate
          x = collect(src, :x, Sink::Bag)

          patch(dst, &.morph({2, x}))
        end
      end

      rule %{(one dev [discard @tgt_]) (many tgt [cell @tgt_ _])} do
        patch(tgt, &.morph({2, nil}))
      end

      rule %{(one dev [transfer (@src_ pattern_ @dst_) template_]) (one src [cell @src_ x_]) (one dst [cell @dst_])} do
        pattern, template = first(dev, :pattern, :template)

        case {src.size, dst.size}
        when {1, _} # distribute
          x = first(src, :x)
          next unless vars = M1.match?(pattern, x)

          instance = Alloy.render(vars, template)

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, instance})),
          )
        when {_, 1} # aggregate
          instances = collect(src, :x, Sink::Bag) do |x|
            next unless vars = M1.match?(pattern, x)

            Alloy.render(vars, template)
          end

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, instances})),
          )
        end
      end

      rule %{(one dev [view (@src_ pattern_ @dst_) template_]) (one src [cell @src_ x_]) (one dst [cell @dst_ _?])} do
        pattern, template, dst_edge, x = {*first(dev, :pattern, :template, :dst), first(src, :x)}

        # Dst disappears on pattern mismatch.
        unless vars = M1.match?(pattern, x)
          next patch(dst, &.morph({2, nil}))
        end

        instance = Alloy.render(vars, template)

        # TODO: errors as events
        patches(
          patch(dev, &.morph({1, 2, {dst_edge, Term.hashcode256(x)}})),
          patch(dst, &.morph({2, instance})),
        )
      end

      rule %{(one dev [view (@src_ pattern_ (@dst_ state_)) template_]) (one src [cell @src_ x_]) (one dst [cell @dst_ _?])} do
        pattern, template, dst_edge, state, x = {*first(dev, :pattern, :template, :dst, :state), first(src, :x)}
        hashcode = Term.of(Term.hashcode256(x))
        next if state == hashcode

        # Dst disappears on pattern mismatch.
        unless vars = M1.match?(pattern, x)
          next patches(
            patch(dev, &.morph({1, 2, dst_edge})),
            patch(dst, &.morph({2, nil})),
          )
        end

        instance = Alloy.render(vars, template)

        # TODO: errors
        patches(
          patch(dev, &.morph({1, 2, {dst_edge, hashcode}})),
          patch(dst, &.morph({2, instance})),
        )
      end

      rule %{(one dev [view (@src_ _ (%any° @dst_ (@dst_ _))) _]) (one src [cell @src_]) (one dst [cell @dst_ _])} do
        dst_edge = first(dev, :dst)

        patches(
          patch(dev, &.morph({1, 2, dst_edge})),
          patch(dst, &.morph({2, nil})),
        )
      end

      rule %{(one dst [cell @dst_ _?]) (link (one dev [view (src←((%past @_ min: 1)) pattern_ output←(%any° @dst_ (@dst_ _))) template_]) (many src [cell @src_ term_]))} do
        pattern, template, src_edges, dst_edge, output = first(dev, :pattern, :template, :src, :dst, :output)

        unless src_edges.size == src.size # Some cells missing or duplicated
          next patches(
            patch(dev, &.morph({1, 2, dst_edge})),
            patch(dst, &.morph({2, nil})),
          )
        end

        matchee = collate(src, src_edges.items, Term.of(:src), Term.of(:term))
        hashcode = Term.hashcode256(matchee)

        cont = -> do
          unless vars = M1.match?(pattern, matchee)
            return patches(
              patch(dev, &.morph({1, 2, dst_edge})),
              patch(dst, &.morph({2, nil})),
            )
          end

          expansion, _ = Alloy.render0(vars, template, severity: :quiet)
          if expansion.is_a?(Alloy::Err) || (expansion.is_a?(Alloy::Splice) && expansion.offspring.empty?)
            instance = nil
          else
            instance = Alloy.collapse(expansion)
          end
          # TODO: errors

          patches(
            patch(dev, &.morph({1, 2, {dst_edge, hashcode}})),
            patch(dst, &.morph({2, instance})),
          )
        end

        Term.case(output) do
          matchpi %{(@_ state_)} do
            next if state == hashcode

            cont.call
          end

          matchpi %{@_} do
            cont.call
          end
        end
      end

      # TODO: handle src-itself-disappears

      rule %{(one dev [fb (@edge_ pattern_) backspec_]) (one tgt [cell @edge_ value0_])} do
        pattern, backspec = first(dev, :pattern, :backspec)
        value0 = first(tgt, :value0)
        next unless value1 = M1.backmap?(pattern, backspec, value0)

        patch(tgt, &.morph({2, value1}))
      end

      rule %{(one dev [fb (tgt←((%past @_ min: 1)) pattern_) backspec_]) (many tgt [cell @tgt_ term_])} do
        pattern, backspec, tgt_edges = first(dev, :pattern, :backspec, :tgt)
        unless tgt_edges.size == tgt.size
          next # Too many or too few associated cells
        end

        matchee = collate(tgt, tgt_edges.items, Term.of(:tgt), Term.of(:term))

        next unless results = M1.backmap?(pattern, backspec, Term.of(matchee))

        unless results.size == tgt_edges.size
          next # The backmap mutilated our original matchee.
        end

        patches(tgt_edges.items) do |tgt_edge, index|
          id, capture = tgt.find! { |_, node| node.env[:tgt] == tgt_edge }
          {id, capture.node.morph({2, results[index]})}
        end
      end

      # FIXME: This implementation passes the current tests but is WRONG. It does not
      # support insertion or deletion. For that we need backmaps to cooperate with us
      # and give us raw patches. This adds priority to the backmap rewrite.
      rule %{(one dev [rulesys tgt←((%past @_ min: 1)) rules_*]) (many tgt [cell @tgt_ term_])} do
        # rules, tgt_edges = first(dev, :rules, :tgt)
        # next unless tgt_edges.size == tgt.size # Confused: duplicate cells on @tgt?

        # matchee = collate(tgt, tgt_edges.items, Term.of(:tgt), Term.of(:term))

        # rules.items.each do |rule|
        #   Term.case(rule) do
        #     matchpi %{[backmap pattern_ backspec_]} do
        #       next unless result = M1.backmap?(pattern, backspec, matchee, applier: Alloy::Applier.new)
        #       next unless result.type.dict?
        #       next unless result.itemsize == matchee.itemsize

        #       matchee = result
        #     end

        #     matchpi %{[one-of options_*]} do
        #       options.items.each do |option|
        #         result = Term.matchpi?(option, %{[backmap pattern_ backspec_]}) do
        #           next unless candidate = M1.backmap?(pattern, backspec, matchee, applier: Alloy::Applier.new)
        #           next unless candidate.type.dict?
        #           next unless candidate.itemsize == matchee.itemsize

        #           candidate
        #         end

        #         next unless result

        #         matchee = result
        #         break
        #       end
        #     end

        #     otherwise { }
        #   end
        # end

        # patches(tgt_edges.items) do |tgt_edge, index|
        #   id, capture = tgt.find! { |_, node| node.env[:tgt] == tgt_edge }

        #   {id, capture.node.morph({2, matchee[index]})}
        # end

        rules, tgt_edges = first(dev, :rules, :tgt)
        next unless tgt_edges.size == tgt.size # Confused: duplicate cells on @tgt?

        matchee = collate(tgt, tgt_edges.items, Term.of(:tgt), Term.of(:term))
        contents = matchee.items.map { |item| Term.content(item) }

        results = [] of Term
        buckets = Slice.new(matchee.itemsize) { [] of {Slice(Term), Term} }

        rules.items.each do |rule|
          Term.matchpi?(rule, %{[backmap pattern_ backspec_]}) do
            next unless result = M1.backmap?(pattern, backspec, matchee)
            next unless result.type.dict?
            next unless result.itemsize == matchee.itemsize

            result.items.each_with_index do |item, index|
              delta = Term.content(item, not_in: contents[index])
              delta.each do |entry|
                buckets[index] << entry
              end
            end
          end
        end

        # Descending, longest keypath first.
        buckets.each do |bucket|
          bucket.sort_by! { |keypath| -keypath.size }
        end

        matchee.each_item_with_index do |item, index|
          bucket = buckets[index]
          bucket.each do |keypath, leaf|
            if keypath.empty?
              matchee = matchee.with(index, leaf)
            else
              matchee = matchee.with(index, matchee[index].where(keypath, eq: leaf))
            end
          end
        end

        patches(tgt_edges.items) do |tgt_edge, index|
          id, capture = tgt.find! { |_, node| node.env[:tgt] == tgt_edge }

          {id, capture.node.morph({2, matchee[index]})}
        end
      end

      rule %{(one dev [delay 1 child_])} do
        child = first(dev, :child)

        patch(dev) { child }
      end

      rule %{(one dev [delay n←(%number +i32) _])} do
        n = first(dev, :n).to(Int32)

        patch(dev, &.morph({1, n - 1}))
      end

      # rule %{(one dev [rewriter (@ruleset_ name_ ⍊ selector_: (%optional (%any° [rule pattern_ template_] [backmap pattern_ backspec_]) _)) @src_ @dst_]) (one rs [cell @ruleset_ base_]) (one src [cell @src_ term_]) (one dst [cell @dst_])} do
      #   name, selector, base, term = {*first(dev, :name, :selector), first(rs, :base), first(src, :term)}
      #   ruleset = RULESETS.put_if_absent(base) { Ruleset.select(selector, base) }

      #   case name
      #   when Term.of(:uiR, :text)
      #     build = -> do
      #       metricsR = callR do |term|
      #         reply = Sync::Future(Term).new
      #         dw.send(DwUIR::TextReplyRequest.new(term, reply))
      #         Rewrite.one(reply.get)
      #       end
      #       chainR(DwUIR::Textual.insetfixR, Soma.uiR(metricsR, ruleset))
      #     end
      #   when Term.of(:uiR, :graphics)
      #     build = -> do
      #       metricsR = callR do |term|
      #         reply = Sync::Future(Term).new
      #         dw.send(DwUIR::GraphicsReplyRequest.new(term, reply))
      #         Rewrite.one(reply.get)
      #       end
      #       Soma.uiR(metricsR, ruleset)
      #     end
      #   when Term.of(:editR)
      #     build = -> { Input.inputR(ruleset) }
      #   else
      #     next
      #   end

      #   rewriter = REWRITERS.put_if_absent({name, ruleset}, &build)
      #   output = rewrite(term, rewriter)

      #   patches(
      #     patch(src, {2, nil}),
      #     patch(dst, {2, output}),
      #   )
      # end

      rule %{(one dev [microfold (@themedoc_ ⍊ rem⋮ 16) @src_ @dst_]) (one themesrc [cell @themedoc_ theme_]) (one src [cell @src_ term_]) (one dst [cell @dst_])} do
        rem, themedoc, term = {first(dev, :rem), first(themesrc, :theme), first(src, :term)}
        theme = THEMES.put_if_absent({themedoc, rem}) do
          Microfold.theme(themedoc, rem.as_n)
        end

        # TODO: issues
        output, _ = Microfold.render(theme, term, severity: :quiet)
        patches(
          patch(src, {2, nil}),
          patch(dst, {2, output}),
        )
      end
    end
  end

  RULESETS  = SyncHash(Term, Ruleset).new
  REWRITERS = SyncHash({Term, Ruleset}, Rewriter).new
  THEMES    = SyncHash({Term, Term}, Microfold::Theme).new

  alias Sensor = ChanSensor | ViewSensor

  defrecord ChanSensor, addr : D7::NodeAddr, tspace : Term, pattern : Term
  defrecord ViewSensor, addr : D7::NodeAddr, tspace : Term, pattern : Term
  defrecord Appearance, addr : D7::NodeAddr, tspace : Term, matchee : Term

  def tspace
    D7::Pass.new { |clf, circuit| tspace(clf, circuit) }
  end

  def master
    D7::Pass.new { |clf, circuit| master(clf, circuit) }
  end

  def tspace(clf : D7::Classifier, circuit : Term) : Term
    sensors = [] of Sensor
    appearances = [] of Appearance

    # Find candidates for an exchange.
    _ = D7.case(clf, circuit) do
      rule %{(one dev [sensor (tspace_ pattern_ @edge_) _]) (one dst [cell @edge_])} do
        tspace, pattern = first(dev, :tspace, :pattern)
        sensors << ChanSensor.new(addr(dev), tspace, pattern)

        nil # No change
      end

      rule %{(one dev [sensor* (tspace_ pattern_ @edge_) _]) (one dst [cell @edge_ _?])} do
        tspace, pattern = first(dev, :tspace, :pattern)
        sensors << ViewSensor.new(addr(dev), tspace, pattern)

        nil # No change
      end

      rule %{(one dev [appearance tspace_ @edge_]) (one src [cell @edge_ matchee_])} do
        tspace, matchee = first(dev, :tspace), first(src, :matchee)
        appearances << Appearance.new(addr(dev), tspace, matchee)

        nil # No change
      end
    end

    if sensors.empty? && appearances.empty?
      return circuit
    end

    # Perform the exchange.
    perceptions = Hash(D7::NodeAddr, Array(Term::Dict)).new(initial_capacity: sensors.size)
    expended = Set(D7::NodeAddr).new

    sensors.each do |sensor|
      envs = nil

      case sensor
      in ChanSensor
        counterparts = [] of {Term::Dict, Appearance}

        appearances.each_with_index do |appearance|
          next unless sensor.tspace == appearance.tspace
          next unless M1.probably_matches?(sensor.pattern, appearance.matchee)
          next unless env = M1.match?(sensor.pattern, appearance.matchee)

          unless counterparts.empty? || counterparts.last[0] == env
            # Multiple different values competing. Sensor chan is confused about
            # which one to pick, so it picks none.
            counterparts.clear
            break
          end

          counterparts << {env, appearance}
        end

        if counterparts.present?
          env = counterparts.first[0]
          envs = [env]

          counterparts.each do |(_, appearance)|
            expended << appearance.addr
          end
        end
      in ViewSensor
        appearances.each do |appearance|
          next unless sensor.tspace == appearance.tspace
          next unless M1.probably_matches?(sensor.pattern, appearance.matchee)
          next unless env = M1.match?(sensor.pattern, appearance.matchee)

          envs ||= [] of Term::Dict
          envs << env
        end
      end

      next unless envs

      perceptions[sensor.addr] = envs
    end

    D7.case(clf, circuit) do
      rule %{(one dev [sensor (_ _ @edge_) template_]) (one dst [cell @edge_])} do
        next unless envs = perceptions[addr(dev)]?
        assert envs.size == 1

        template = first(dev, :template)
        instance = Alloy.render(envs[0], template)

        patch(dst, &.morph({2, instance}))
      end

      rule %{(one dev [sensor* (tspace_ _ @edge_) template_]) (one dst [cell @edge_ _?])} do
        unless envs = perceptions[addr(dev)]?
          next patch(dst, &.morph({2, Term[]}))
        end

        template = first(dev, :template)
        instances = envs.map { |env| Alloy.render(env, template) }
        instances.sort! { |a, b| Term.compare(a, b) }

        patch(dst, &.morph({2, instances}))
      end

      rule %{(one dev [appearance tspace_ @edge_]) (one src [cell @edge_ matchee_])} do
        next unless addr(dev).in?(expended)

        patch(src, &.morph({2, nil}))
      end
    end
  end

  # def tspace : D7::Pass
  #   D7::Pass.new { |clf, circuit| tspace(clf, circuit) }
  # end

  # def master : D7::Pass
  #   D7::Pass.new { |clf, circuit| master(clf, circuit) }
  # end
end
