module Ww::Rack
  # The guts of the `part` node.
  #
  # The `part` node is a bit like the backmap engine `M1::Backmap`, except it's
  # much simpler (it only handles keypaths; no refs, no optionals, no conflict
  # resolution; no concept of agents, etc.; nothing of that sort).
  #
  # It's also important to say that it's not the part node that is like the backmap
  # engine, but the entire tree of part nodes rooted at a `cell`. The entire tree --
  # all `part` nodes collectively -- form something resembling a backmap, allowing
  # one to modify the root cell's value.
  #
  # There can obviously be overlap. `part`s may be unaware of each other, and that's
  # the whole point! Think `part`s corresponding to different *parts* of a device's
  # surface (imagine displays, knobs; except they're symbolic).
  #
  # We say there is a *conflict* if two keypaths corresponding to different `part`
  # nodes propose different values. We also consider as conflict cases where different
  # `part` nodes target keypaths such that one is a prefix of the other.
  #
  # Conflict is resolved by ignoring changes from both conflicting parties. This is
  # the strategy for `part`; the backmap engine uses a different strategy (backtracking
  # search until a non-conflicting config is found, taking into account user-
  # assigned priority).
  #
  # If the root cell itself was patched, its `part` node subtree refuses to touch it.
  #
  # `part` nodes are special in that they do not require intermediate cells for
  # storing state. The literally modify the innards of a term after each tick. This
  # leads to a very "baroque" implementation that decorates the main `pass` instead
  # of being a neat rule in it, like e.g. `Feed`.
  #
  # `part` relies on trickery to convince the main pass that the `part`s are
  # actually just `cell`s (extraction), and then after the main pass is complete,
  # the implementation of `part` works hard to hide the fact there were any tricks
  # at all, as it merges stuff back into the root cell (absorption).
  module Part
    extend self

    defcase Tree,
      entries : Hash(Term, Tree)? = nil,
      proposals : Set(Term?)? = nil,
      mutation: true

    def follow(tree : Tree, key : Term) : Tree
      entries = tree.entries ||= {} of Term => Tree
      entries.put_if_absent(key) { Tree.new }
    end

    # NOTE: `nil` proposals signify removal. Think `(cell @x 100)` -> `(cell @x)`.
    def propose(tree : Tree, proposal : Term?) : Nil
      proposals = tree.proposals ||= Set(Term?).new
      proposals << proposal
    end

    # NOTE: `nil` signifies removal. Think `(cell @x 100)` -> `(cell @x)`;
    # if you focus on `100` you get *term* `100` -> `nil`.
    def merge(tree : Tree, term : Term) : Term?
      # If entries exist, then assume deeper proposals exist. If deeper proposals
      # exist, then we are in conflict if we have proposals of our own. Thus, we
      # refuse to merge this branch & deeper.
      #
      # If neither entries nor proposals exist, then simply return term.
      if (tree.entries && tree.proposals) || (tree.entries.nil? && tree.proposals.nil?)
        return term
      end

      # If proposals exist, then entries do not exist. If there is more than
      # proposal for the same spot, we decline all of them. If there is just
      # one proposal for this spot, we accept it.
      if proposals = tree.proposals
        if proposals.size > 1
          return term
        end

        assert proposals.size == 1
        return proposals.first
      end

      assert entries = tree.entries

      # If entries exist, then merge recursively each one of those entries,
      # assuming respective keys exist.
      assert dict0 = term.as_d?

      removals = Pf::Kit.stack_array(Int32)

      dict1 = dict0.transaction do |commit|
        dict0.each_item_with_index do |item, index|
          key = Term.of(index)
          next unless successor = entries[key]?

          unless result = merge(successor, item)
            removals << index
            next
          end

          commit.with(key, result)
        end

        dict0.each_pair do |key, value|
          next unless successor = entries[key]?

          commit.with(key, merge(successor, value))
        end
      end

      removals.unstable_sort!
      removals.reverse_each do |index|
        dict1 = dict1.without_item(index)
      end

      Term.of(dict1)
    end
  end

  # Returns the main Rack pass.
  def pass : D7::Pass
    D7::Pass.new { |clf, circuit| step(clf.call(circuit), circuit) }
  end

  def complete(hg : D7::Hypergraph, seen, root : D7::Node, edge_path, path, completer, sink) forall T
    completed = false

    src = edge_path.last

    hg.each_neighbor(of: root.id, on: src) do |candidate|
      next if candidate.id.in?(seen)

      seen << candidate.id

      # Hey fn, is this a valid completion?
      next unless response = completer.call(src, candidate)

      # Yes, fn says it is. Proceed completing recursively.
      dst, object = response

      # Don't follow edges we already followed [TODO: why?]
      next if dst.in?(edge_path)

      completed = true

      complete(hg, seen, candidate, edge_path.append(dst), path.append(object), completer, sink)
    end

    return if completed

    sink.call(path)
  end

  def complete(hg, root : D7::Node, edge : D7::AbsEdge, &completer : D7::AbsEdge, D7::Node -> {D7::AbsEdge, T}) forall T
    paths = [] of Slice(T)

    _ = Pf::USet32.transaction do |seen|
      sink = ->(path : Slice(T)) do
        return if path.empty?

        paths << path
      end

      complete(hg, seen, root, Slice[edge], Slice(T).empty, completer, sink)
    end

    paths
  end

  # :nodoc:
  #
  # *Prepass* is Rack's decorator over the general hypergraph solving process.
  # The most important thing it does is it walks the `part` subtree of root
  # cells and constructs child `cell`s with appropriate values, if any; and
  # then after the block runs, it merges those child cells back into the root.
  def prepass(clf : D7::Classifier, hg : D7::Hypergraph, & : D7::Hypergraph -> D7::Patch) : D7::Patch
    roots = {} of D7::NodeId => {Term, (Term? -> Term), Part::Tree}
    endpoints = {} of D7::NodeId => Part::Tree
    replacements = [] of ->

    # TODO: skip duplicate cells
    # TODO: skip duplicate parts
    # IN GENERAL: Do heavy prefiltering here, we're mutating the hypergraph after all!!!
    hg.each_node do |node|
      Term.matchpi?(node.term, %{[cell @src_ whole_]}) do
        paths = complete(hg, node, hg.abs_edge(src, wrt: node.id)) do |from_abs, completion|
          Term.matchpi?(completion.term, %{[part (@from_ to←(%'edge capture_)) pattern_]}) do
            next unless from_abs == hg.abs_edge(from, wrt: completion.id)

            to_abs = hg.abs_edge(to, wrt: completion.id)
            {to_abs, {pattern, completion.id, to_abs, capture}}
          end
        end

        next if paths.empty?

        # pp paths

        root = Part::Tree.new
        roots[node.id] = {whole, ->(whole1 : Term?) { Term.of(node.term.morph({2, whole1})) }, root}

        paths.each do |path|
          matchee = whole
          trace = root

          path.each do |pattern, part_id, target, capture|
            break unless M1.probably_matches?(pattern, matchee)

            envlogs = M1.matches_and_logs(Term[], M1.operator(pattern), matchee)
            break unless envlog = envlogs.first?

            env, logs = envlog
            break unless successor = env[capture]?
            break unless found = logs.find { |name, _| capture }

            _, suffix = found

            # Removes things like <examine itemspart>—<examine key 0> which lets
            # us support patterns like [x_].
            normal_suffix = M1::Log.normalize(suffix.seq)
            break if normal_suffix.is_a?(M1::Log::None)

            actions = M1::Log::SeqSlice.new(normal_suffix)
            break unless actions.all?(M1::Log::ExamineValue)

            actions.each do |action|
              assert action.is_a?(M1::Log::ExamineValue)

              trace = Part.follow(trace, action.key)
            end

            endpoints[part_id] = trace

            # Replace [part (@src @dst) ...] with (cell @dst <value>). Notice how
            # we keep only @dst as the edge!
            #
            # NOTE: We delay replacements since they mutate the hypergraph, and we're
            # iterating over it right now!
            replacements << -> do
              hg.replace(part_id, Term.of(:cell, target.term, successor)) do |edge|
                edge == target
              end
            end

            matchee = successor
          end
        end
      end
    end

    replacements.each &.call

    patch = yield hg

    patch = patch.select do |node_id, rep|
      unless endpoint = endpoints[node_id]?
        next true
      end

      Term.case(rep) do
        matchpi %{[cell @_ value_]} { Part.propose(endpoint, value) }
        matchpi %{[cell @_]} { Part.propose(endpoint, nil) }
        otherwise { }
      end

      false
    end

    roots.each do |root_id, (whole0, submit, tree)|
      next if root_id.in?(patch)

      whole1 = Part.merge(tree, whole0)
      next if whole0 == whole1

      patch = patch.assoc(root_id, submit.call(whole1))
    end

    patch
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
    end
  end
end
