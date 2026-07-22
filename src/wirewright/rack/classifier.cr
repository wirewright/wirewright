module Ww::Rack
  # :nodoc:
  def classify!(node : Term) : D7::Feature
    M1::PatternSet.case(node) do
      matchpi %{[cell @u_ _?]} do
        D7.gnd(node, u)
      end

      matchpi %{[feed _*]} do
        continue unless spec = Feed.spec?(node)

        D7.gnd(node, Feed.edges(spec), defn: Feed.render(spec))
      end

      matchpi %{[feed (%group edges_ (%past @_ min: 3))]} do
        defn = Term::Dict.build do |commit|
          commit << :group
          edges.items.each_cons_pair do |a, b|
            commit << Term.of(:feed, a, b)
          end
        end

        D7.mixture(node, defn) { node }
      end

      matchpi %{[discard @u_]} do
        D7.gnd(node, u)
      end

      matchpi %{[discard @u_ _]} do
        D7.gnd(node, u)
      end

      matchpi %{[part (@src_ @dst_) _]} do
        D7.gnd(node, src, dst)
      end

      matchpi %{[group _*]} do
        D7.parent(node.as_d, 1u32...node.uitemsize)
      end

      matchpi %{[module bindings_dict _*]} do
        D7.scope(D7.parent(node.as_d, 2u32...node.uitemsize), bindings: bindings.as_d)
      end

      matchpi %{[locals locals←((%past @_ min: 0)) _*]} do
        D7.scope(D7.parent(node.as_d, 2u32...node.uitemsize), locals: locals.items)
      end

      matchpi %{[device _*]} do
        D7.circuit(node.as_d, 1u32...node.uitemsize, D7.inert(node))
      end

      matchpi %{[node @edge_ _?]} do
        if child = node[2]?
          mix0 = Term.of(:cell, edge, child)
        else
          mix0 = Term.of(:cell, edge)
        end

        leaf = D7.mixture(node, mix0) do |mix1|
          Term.of_case(mix1) do
            matchpi %{(cell @_ child1_)} { Term.morph(node, {2, child1}) }
            otherwise { Term.morph(node, {2, nil}) }
          end
        end

        D7.circuit(node.as_d, 2u32...node.uitemsize, leaf)
      end

      matchpi %{[node (edge←(%'edge capture_) pattern_) child0_]} do
        leaf = pass do
          next D7.inert(node) unless env = M1.match?(pattern, child0)

          if view0 = env[capture]?
            mix0 = Term.of(:cell, edge, view0)
          else
            mix0 = Term.of(:cell, edge)
          end

          D7.mixture(node, mix0) do |mix1|
            backspec = Term[]

            Term.case(mix1) do
              matchpi %{(cell @_)} do
                backspec = Term.entries({ { {capture}, Term[] } })
              end

              matchpi %{(cell @_ value_)} do
                backspec = Term.entries({ {capture, Term.of(:"^verbatim", value)} })
              end
            end

            child1 = M1.backmap(pattern, Term.of(backspec), child0)
            Term.morph(node, {2, child1})
          end
        end

        D7.circuit(node.as_d, 2u32...node.uitemsize, leaf)
      end

      # Backrefs are like `node`s with a pattern, except there can be zero or more
      # alternative patterns, and also backrefs aren't cells. The value at edge
      # is backmapped into payload during evaluation of backrefs.
      matchpi %{[backref (@edge_ _*) _]} do
        leaf = D7.gnd(node, edge)
        D7.circuit(node.as_d, 2u32...3u32, leaf)
      end

      matchpi %{[circuit @edge_ children0_*]} do
        if children0.empty?
          mix0 = Term.of(:cell, edge)
        else
          mix0 = Term.of(:cell, edge, children0)
        end

        leaf = D7.mixture(node, mix0) do |mix1|
          Term.of_case(mix1) do
            matchpi %{(cell @_ children1←[_*])} do
              node.replace(2...node.itemsize, Term.rep(children1.items))
            end

            otherwise do
              node.replace(2...node.itemsize, Term.rep)
            end
          end
        end

        D7.circuit(node.as_d, 2u32...node.uitemsize, leaf)
      end

      matchpi %{[circuit (pool @edge_) children0_*]} do
        mix0 = Term.of(:pool, edge, children0)

        leaf = D7.mixture(node, mix0) do |mix1|
          Term.matchpi(mix1, %{(pool @_ children1←[_*])}) do
            Term.of(node.replace(2...node.itemsize, Term.rep(children1.items)))
          end
        end

        D7.circuit(node.as_d, 2u32...node.uitemsize, leaf)
      end

      matchpi %{[pool @edge_ _]} do
        D7.gnd(node, edge)
      end

      matchpi %{[circuit (edge←(%'edge capture_) pattern_) children0_*]} do
        leaf = pass do
          next D7.inert(node) unless M1.probably_matches?(pattern, children0)
          next D7.inert(node) unless env = M1.match?(pattern, children0)

          if view0 = env[capture]?
            mix0 = Term.of(:cell, edge, view0)
          else
            mix0 = Term.of(:cell, edge)
          end

          D7.mixture(node, mix0) do |mix1|
            backspec = Term[]

            Term.case(mix1) do
              matchpi %{(cell @_)} do
                backspec = Term.entries({ { {capture}, Term[] } })
              end

              matchpi %{(cell @_ value_)} do
                backspec = Term.entries({ {capture, Term.of(:"^verbatim", value)} })
              end
            end

            children1 = M1.backmap(pattern, Term.of(backspec), children0)

            Term.of(node.replace(2...node.itemsize, Term.rep(children1.items)))
          end
        end

        D7.circuit(node.as_d, 2u32...node.uitemsize, leaf)
      end

      # NOTE: I am unsure about the difference between frag and `node`/`circuit` now
      # that we define frag as both readable and writable. The only difference I can
      # see is that when I write to the frag's cell, the execution of that is carried over
      # to the next tick, whereas in `circuit`, when I write to the cell it creates on the current
      # level,  the execution continues within the same tick as the evaluator descends down
      # to the next level, breadth-first. There's also obviously the isolation; subcircuits
      # are completely sealed from the outside in base Rack (termspaces can be used to
      # connect them, however). Frags, on the other hand, are simply a reference to
      # a part or "pocket" of the current running circuit. Since we use synchronous rewriting,
      # anything that reads the frag or stuff within it (e.g. cells defined in the frag)
      # sees only the previous frame; so there's nothing unexpected on that end.
      #
      # NOTE: The empty case is handled below.
      matchpi %{[frag @edge_ value0_]} do
        mix0 = Term.of(:group, {:cell, edge, value0}, {:group, value0})

        D7.mixture(node, mix0) do |mix1|
          Term.case(mix1) do
            # New value arrived. Higher priority.
            matchpi %{(group (cell @_ value1_) _)} do
              continue if value0 == value1

              Term.morph(node, {2, value1})
            end

            # New value computed.
            matchpi %{(group _ (group value1_))} do
              Term.morph(node, {2, value1})
            end
          end
        end
      end

      matchpi %{[cell (edge←(%'edge capture_) pattern_) whole0_]} do
        next D7.inert(node) unless M1.probably_matches?(pattern, whole0)
        next D7.inert(node) unless env = M1.match?(pattern, whole0)
        next D7.inert(node) unless part0 = env[capture]?

        D7.mixture(node, Term.of(:cell, edge, part0)) do |mix1|
          backspec = Term[]

          Term.case(mix1) do
            matchpi %{(cell @_)} do
              backspec = Term.entries({ { {capture}, Term[] } })
            end

            matchpi %{(cell @_ part1_)} do
              backspec = Term.entries({ {capture, Term.of(:"^verbatim", part1)} })
            end
          end

          whole1 = M1.backmap(pattern, Term.of(backspec), whole0)

          Term.morph(node, {2, whole1})
        end
      end

      matchpi %{[frag @edge_]}, %{[cell (@edge_ _)]} do
        D7.mixture(node, Term.of(:cell, edge)) do |view|
          Term.of_case(view) do
            matchpi %{(cell @_)} { node }
            matchpi %{(cell @_ value1_)} { Term.morph(node, {2, value1}) }
          end
        end
      end

      matchpi %{[delay (%number +i32!)]}, %{[delay (%number +i32) _]} do
        D7.gnd(node)
      end

      matchpi %{[transfer (@src_ pattern_ @dst_) template_]} do
        D7.mixture(node, Term.of(:transfer, { {:not}, {src}, {pattern}, dst }, template)) { node }
      end

      matchpi %{[transfer (srcs←((%past @_ min: 1)) pattern_ @dst_) template_]} do
        D7.mixture(node, Term.of(:transfer, { {:not}, srcs, pattern, dst }, template)) { node }
      end

      matchpi %{[transfer ((not (%group inhibitors_ (%past @_))) srcs←((%past @_ min: 1)) pattern_ @dst_) template_]} do
        edges = [] of Term
        edges.concat(inhibitors.items)
        edges.concat(srcs.items)
        edges << dst

        D7.gnd(node, edges, defn: Term.of(:transfer, inhibitors, srcs, pattern, dst, template))
      end

      matchpi %{[backsys @src_ backmaps_*]} do
        offspring = Term::Dict.build do |commit|
          commit << :backsys << {src}

          backmaps.items.each do |backmap|
            Term.matchpi?(backmap, %{[backmap pattern_ backspec_]}) do
              commit << Term.of(:backmap, {pattern}, backspec)
            end
          end
        end

        D7.mixture(node, offspring) { node }
      end

      matchpi %{[backsys ((%group srcs_ (%past @_ min: 0)) ¦ res_) backmaps_*]} do
        edges = [] of Term
        edges.concat(srcs.items)

        res_edges, restab = Term::Dict.build do |res_edges, restab|
          res.each_entry do |key, value|
            # Ignore numbers to avoid tricky cases where the resources dict is like
            # (@a @b @c), which would invalidate the disjointedness of
            # <src values dict> | <res dict>.
            next if key.type.number?
            next unless Term.edge?(value)

            edges << value
            res_edges << value
            restab.with(key, value)
          end
        end

        D7.gnd(node, edges, defn: Term.of(:backsys, srcs, res_edges, restab, backmaps))
      end

      matchpi %{[queue (@front_ @back_ ⍊ min_: (%optional 1 (%number +i32!)) max_: (%optional ∞ (%any° (%number +i32!) ∞))) buffer0_dict]}, min: Int32 do
        defn = Term::Dict.build do |commit|
          commit << :group

          # Check if we can dequeue.
          if 0 < min <= buffer0.itemsize
            commit << Term.of(:cell, front, buffer0.items.first, front: true)
          end

          # Check if we can enqueue.
          if max == Term.of(:∞) || buffer0.itemsize < max.to(Int32)
            commit << Term.of(:cell, back, back: true)
          end
        end

        D7.mixture(node, Term.of(defn)) do |view|
          buffer1 = buffer0

          Term.case(view) do
            # Dequeue.
            matchpi %{⟨(cell @_ ⍊ front)⟩} do
              buffer1 = buffer1.rest
              continue
            end

            # Sync.
            matchpi %{⟨(cell @_ x_ ⍊ front)⟩} do
              buffer1 = Term.morph(buffer1, {0, x})
              continue
            end

            # Enqueue.
            matchpi %{⟨(cell @_ x_ ⍊ back)⟩} do
              buffer1 = buffer1.append(x)
              continue
            end

            otherwise { }
          end

          Term.morph(node, {2, buffer1})
        end
      end

      matchpi %{[view (@src_ src-pattern_ @dst_) template_]} do
        D7.gnd(node, src, dst, defn: Term.of(:view, { {src}, {src_pattern}, dst }, template))
      end

      matchpi %{[view (srcs←((%past @_ min: 1)) _ @dst_) _]} do
        edges = [] of Term
        edges.concat(srcs.items)
        edges << dst

        D7.gnd(node, edges)
      end

      matchpi %{[extension (@src_ src-pattern_ @dst_) template_]} do
        D7.gnd(node, src, dst, defn: Term.of(:extension, { {src}, {src_pattern}, dst }, template))
      end

      matchpi %{[extension (@src_ src-pattern_ @dst_ dst-pattern_) template_]} do
        D7.gnd(node, src, dst, defn: Term.of(:extension, { {src}, {src_pattern}, dst, dst_pattern }, template))
      end

      matchpi(
        %{[extension (srcs←((%past @_ min: 1)) _ @dst_ _) _]},
        %{[extension (srcs←((%past @_ min: 1)) _ @dst_) _]},
      ) do
        edges = [] of Term
        edges.concat(srcs.items)
        edges << dst

        D7.gnd(node, edges)
      end

      matchpi %{[sensor (_ _)]} do
        D7.gnd(node)
      end

      matchpi %{[surface storage←[cell @_] [sensor (_ _) _]]} do
        mix0 = storage
        D7.mixture(node, mix0) do |mix1|
          Term.morph(node, {1, mix1})
        end
      end

      matchpi %{[sensor (many _ _)]} do
        D7.gnd(node)
      end

      matchpi %{[surface storage←[cell @_] [sensor (many _ _) _]]} do
        mix0 = storage
        D7.mixture(node, mix0) do |mix1|
          Term.morph(node, {1, mix1})
        end
      end

      matchpi %{[sensor (view _ _) _*]} do
        D7.gnd(node)
      end

      matchpi %{[surface storage←[cell @_] [sensor (view _ _) _]]} do
        mix0 = storage
        D7.mixture(node, mix0) do |mix1|
          Term.morph(node, {1, mix1})
        end
      end

      matchpi %{[sensor (journal _ _) _*]} do
        D7.gnd(node)
      end

      matchpi %{[appearance _ _]} do
        D7.gnd(node)
      end

      matchpi %{[surface [cell @_ _] _]} do
        D7.parent(node.as_d, 1u32...2u32)
      end

      matchpi %{[rewriter (@input_ -> @spec_ -> @output_) _*]} do
        D7.gnd(node, input, spec, output)
      end

      matchpi %{[rewriter (@input_ - @spec_ - @output_) _*]} do
        D7.gnd(node, input, spec, output)
      end

      matchpi(
        %{[rewriter (@input_ -> spec_ -> @output_) _*]},
        %{[rewriter (@input_ - spec_ - @output_) _*]},
      ) do
        bindings = Term[].with({:edge, :in}, input).with({:edge, :out}, output)
        defn = Term.of(:module, bindings,
          {:cell, {:edge, :spec}, spec},
          Term.morph(node,
            {1, 0, {:edge, :in}},
            {1, 2, {:edge, :spec}},
            {1, 4, {:edge, :out}},
          ),
        )

        D7.mixture(node, defn) { node }
      end

      matchpi %{[rewriter (spec_ <-> @edge_) body_*]} do
        edges = [edge]
        D7.gnd(node, edges, defn: Term.of(:rewriter, spec, edge, body))
      end

      matchpi %{[rewriter (@spec_ <-> ((%group itemsrcs_ (%past @_ min: 0)) ¦ pairsrcs_)) body_*]} do
        edges = [spec]
        edges.concat(itemsrcs.items)

        res_edges, restab = Term::Dict.build do |res_edges, restab|
          pairsrcs.each_entry do |key, value|
            # Ignore numbers to avoid tricky cases where the resources dict is like
            # (@a @b @c), which would invalidate the disjointedness of
            # <src values dict> | <res dict>.
            next if key.type.number?
            next unless Term.edge?(value)

            edges << value

            res_edges << value
            restab.with(key, value)
          end
        end

        D7.gnd(node, edges, defn: Term.of(:rewriter, spec, itemsrcs, res_edges, restab, body))
      end

      matchpi %{[rewriter (spec_ <-> srcs_dict) _*]} do
        # For example, the following rewriter:
        #
        #   (rewriter ((rulesetR) <-> {@:n @:m})
        #     {¦ ±n} <> {n: ^(+ n 1)}
        #     {¦ ±m} <> {n: ^(+ n 1)})
        #
        # ... should expand to:
        #
        #   (module {@n: @(local n), @m: @(local m)}
        #     (cell @spec (rulesetR))
        #     (rewriter (@spec <-> {n: @(local n), m: @(local m)})
        #       {¦ ±n} <> {n: ^(+ n 1)}
        #       {¦ ±m} <> {n: ^(+ n 1)}))
        #

        bindings, local_srcs = Term::Dict.build do |bindings_commit, local_srcs_commit|
          srcs.items.each_with_index do |value, key|
            next unless Term.edge?(value)

            local = Term.of(:edge, {:local, key})
            bindings_commit.with(local, value)
            local_srcs_commit.with(key, local)
          end

          srcs.each_entry(in: Term::Dict.pairspart) do |key, value|
            next if key.type.number?

            local = Term.of(:edge, {:local, key})
            bindings_commit.with(local, value)
            local_srcs_commit.with(key, local)
          end
        end

        defn = Term.of(:module, bindings,
          {:cell, {:edge, :spec}, spec},
          Term.morph(node,
            {1, 0, {:edge, :spec}},
            {1, 2, local_srcs},
          ),
        )

        D7.mixture(node, defn) { node }
      end

      matchpi %{[slot _]} do
        D7.gnd(node)
      end

      matchpi %{[slot _ _]} do
        D7.parent(node.as_d, 2u32...3u32)
      end

      matchpi %{[log (@all_ @last_ ⍊ limit_: (%optional 10 (%number +i32)) edges⋮ false) log0_dict]}, limit: Int32, log0: Term::Dict do
        tail = log0.items.tail(limit)
        tip0 = tail.last?

        # HACK: We use `(delay 1)` to force a change on Rack pass. This way,
        # the unmix function runs regardless of whether anything changed in
        # *mix0* due to evaluation, **but only during Rack pass** -- since only
        # Rack pass evaluates delays.
        mix0 = Term.of(:group,
          Term.of(:cell, all, tail),
          Term.of(:cell, last, tip0),
          Term.of(:delay, 1)
        )

        D7.mixture(node, mix0) do |mix1|
          Term.matchpi(mix1, %{(group _ (cell _ tip1_) _)}) do
            if edges.true? && tip0 == tip1  # Unchanged
              if log0.itemsize == tail.size # Did not truncate
                next node
              end

              # Truncated
              next Term.morph(node, {2, Term.merge(Term[tail], log0.pairspart)})
            end

            log1 = log0.pairspart.transaction do |commit|
              prefix = tail
              if prefix.size + 1 > limit
                prefix = prefix.move(1)
              end

              commit.concat(prefix)
              commit << tip1
            end

            # Edge or forced
            Term.morph(node, {2, log1})
          end
        end
      end

      matchpi %{[parser (@input_ -> _symbol -> @output_) _*]} do
        D7.gnd(node, input, output)
      end

      matchpi %{[parser (@input_ -> _symbol -> @output_ / @error_) _*]} do
        D7.gnd(node, input, output, error)
      end

      matchpi %{[parser (@input_ - _symbol - @output_) _*]} do
        D7.gnd(node, input, output)
      end

      matchpi %{[parser (@input_ - _symbol - @output_ / @error_) _*]} do
        D7.gnd(node, input, output, error)
      end

      matchpi %{[path (_string reading)]}, %{[path (_string reading) _]} do
        D7.gnd(node)
      end

      matchpi %{[path (_string report)]}, %{[path (_string report) _]} do
        D7.gnd(node)
      end

      matchpi %{[path (_string sink) _]} do
        D7.gnd(node)
      end

      matchpi %{[resource _]}, %{[resource _ _]} do
        D7.gnd(node)
      end

      matchpi(
        %{[db (@stmt_ -> _string -> @response_)]},
        %{[db (@stmt_ -> _string -> @response_) _]},
      ) do
        D7.gnd(node, stmt, response)
      end

      matchpi %{[ws (@pool_ _ server _?) _*]} do
        D7.gnd(node, pool)
      end

      otherwise do
        D7.inert(node)
      end
    end
  end

  @@cache = SyncLRU(Term, D7::Feature).new(capacity: 512)

  # :nodoc:
  def classify(node : Term) : D7::Feature
    @@cache.put_if_absent(node) { classify!(node).as(D7::Feature) }
  end

  # Returns the Rack classifier.
  def clf : D7::Classifier
    ->classify(Term)
  end
end
