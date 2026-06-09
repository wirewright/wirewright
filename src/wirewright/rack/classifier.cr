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

      # A device is a unit (an edge-less circuit) with an "appearance", which is
      # called ts "surface". During printing, we prefer to hide the children of
      # `device` in favor of its surface, although the surface is styled differently
      # to avoid confusion (a bit like a real device, where you see the box and the knobs
      # but you also know there is something *inside* the box that's actually doing the work).
      # Think of it this way: the surface of a device is its "control panel", an opaque "box"
      # hiding the machinery inside (*children*). Both the outside and the inside
      # have access to the surface; and both can modify it (think display readouts
      # and knobs).
      matchpi %{[device (@edge_ _?) children0_*]} do
        surface0 = node[1, 1]?

        unit = Term::Dict.build do |commit|
          commit << :unit
          commit << {:cell, edge, surface0}
          commit.concat(children0.items)
        end

        defn = Term.of(:group, {:surface, surface0}, unit)

        D7.mixture(node, defn) do |mix|
          Term.case(mix) do
            matchpi %{(group (surface surface1_) _)} do
              # If we're evaluating the deeper (unit ...) right now, we will see
              # surface0 == surface1. This means we should try to update from
              # the inner cell instead.
              continue if surface0 == surface1

              Term.morph(node, {1, 1, surface1})
            end

            matchpi %{(group (surface) _)} do
              # Ditto.
              continue if surface0.nil?

              Term.morph(node, {1, 1, nil})
            end

            matchpi %{(group _ (unit (cell @_ surface1_) children1_*))} do
              result = node.pairspart.transaction do |commit|
                commit << :device << {edge, surface1}
                commit.concat(children1.items)
              end

              Term.of(result)
            end

            matchpi %{(group _ (unit (cell @_) children1_*))} do
              result = node.pairspart.transaction do |commit|
                commit << :device << {edge}
                commit.concat(children1.items)
              end

              Term.of(result)
            end
          end
        end
      end

      matchpi %{[surface _]} do
        D7.gnd(node)
      end

      matchpi %{[control @edge_ _]} do
        D7.gnd(node, edge)
      end

      matchpi %{[unit _*]} do
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

      matchpi %{[frag @edge_ value_]} do
        mix0 = Term.of(:group, {:cell, edge}, {:group, value})

        D7.mixture(node, mix0) do |mix1|
          Term.of_case(mix1) do
            # New value arrived. Higher priority.
            matchpi %{(_ (cell @_ value1_) _)} { Term.morph(node, {2, value1}) }
            # New value computed.
            matchpi %{(_ _ (group value1_))} { Term.morph(node, {2, value1}) }
          end
        end
      end

      matchpi %{[frag (r @edge_) value_]} do
        mix = Term.of(:group, {:cell, edge, value}, value)

        D7.mixture(node, mix) do |(_, _, value1)|
          Term.morph(node, {2, value1})
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

      matchpi %{[delay (%number +i32) _]} do
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
          {:cell, edge, value0},
          {:sensor, {tspace, pattern, edge}, {:^, edge[1]}},
        )

        D7.mixture(node, mix0) do |mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_)⟩} { Term.morph(node, {2, nil}) }
            matchpi %{⟨(cell @_ value1_)⟩} { Term.morph(node, {2, value1}) }
          end
        end
      end

      # Fused view sensor-cell
      matchpi %{[(sensor* cell) (tspace_ pattern_ @edge_) _?]} do
        value0 = node[2]?

        mix0 = Term.of(:group,
          {:cell, edge, value0},
          {:"sensor*", {tspace, pattern, edge}, {:^, edge[1]}},
        )

        D7.mixture(node, mix0) do |mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_)⟩} { Term.morph(node, {2, nil}) }
            matchpi %{⟨(cell @_ value1_)⟩} { Term.morph(node, {2, value1}) }
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
          {:cell, edge, value0},
          {:appearance, tspace, edge},
        )

        D7.mixture(node, mix0) do |mix1|
          Term.of_case(mix1) do
            matchpi %{⟨(cell @_)⟩} { Term.morph(node, {2, nil}) }
            matchpi %{⟨(cell @_ value1_)⟩} { Term.morph(node, {2, value1}) }
          end
        end
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
