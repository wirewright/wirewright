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

      matchpi %{[part (@src_ @dst_) _]} do
        D7.gnd(node, src, dst)
      end

      matchpi %{[group _*]}, %{[window _*]} do
        D7.parent(node.as_d, 1...node.itemsize)
      end

      matchpi %{[picture @_ _]} do
        D7.parent(node.as_d, 2...node.itemsize)
      end

      matchpi %{[keyboard _*]} do
        D7.gnd(node)
      end

      matchpi %{[module bindings_dict _*]} do
        D7.scope(D7.parent(node.as_d, 2...node.itemsize), bindings: bindings.as_d)
      end

      matchpi %{[locals locals←((%past @_ min: 0)) _*]} do
        D7.scope(D7.parent(node.as_d, 2...node.itemsize), locals: locals.items)
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
        D7.circuit(node.as_d, 1...node.itemsize) { D7.inert(node) }
      end

      matchpi %{[node @edge_ _?]} do
        D7.circuit(node.as_d, 2...node.itemsize) do
          if child = node[2]?
            mix0 = Term.of(:cell, edge, child)
          else
            mix0 = Term.of(:cell, edge)
          end

          D7.mixture(node, mix0) do |mix1|
            Term.of_case(mix1) do
              matchpi %{(cell @_ child1_)} { Term.morph(node, {2, child1}) }
              otherwise { Term.morph(node, {2, nil}) }
            end
          end
        end
      end

      matchpi %{[circuit @edge_ children0_*]} do
        D7.circuit(node.as_d, 2...node.itemsize) do
          if children0.empty?
            mix0 = Term.of(:cell, edge)
          else
            mix0 = Term.of(:cell, edge, children0)
          end

          D7.mixture(node, mix0) do |mix1|
            Term.of_case(mix1) do
              matchpi %{(cell @_ children1←[_*])} do
                node.replace(2...node.itemsize, Term.rep(children1.items))
              end

              otherwise do
                node.replace(2...node.itemsize, Term.rep)
              end
            end
          end
        end
      end

      matchpi %{[circuit (edge←(%'edge capture_) pattern_) children0_*]} do
        D7.circuit(node.as_d, 2...node.itemsize) do
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

      matchpi %{[view (@src_ src-pattern_ @dst_ dst-pattern_) template_]} do
        D7.gnd(node, src, dst, defn: Term.of(:view, { {src}, {src_pattern}, dst, dst_pattern }, template))
      end

      matchpi(
        %{[view (srcs←((%past @_ min: 1)) _ @dst_ _) _]},
        %{[view (srcs←((%past @_ min: 1)) _ @dst_) _]},
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

      otherwise do
        D7.inert(node)
      end
    end
  end

  @@cache = SyncCache(Term, D7::Feature).new(512, preallocate: true)

  # :nodoc:
  def classify(node : Term) : D7::Feature
    @@cache.put_if_absent(node) { classify!(node) }
  end

  alias Component = DeviceComponent | TemplateComponent

  defrecord DeviceComponent,
    id : UInt32,
    pattern : M1::Op::Any,
    specificity : M1::Specificity,
    recipe : Term

  defrecord TemplateComponent,
    id : UInt32,
    pattern : M1::Op::Any,
    specificity : M1::Specificity,
    template : Term

  # :nodoc:
  def make(components : Indexable(Component), trace, node : Term)
    components.each do |component| # Sorted by specificity, most specific first
      next if component.id.in?(trace)
      next unless M1.probably_matches?(component.pattern, node)
      next unless env = M1.match?(Term[], component.pattern, node)

      return make(components, trace.add(component.id), component, env)
    end

    node # No component matches this node
  end

  # :nodoc:
  def make(components, trace, component : TemplateComponent, env : Term::Dict)
    render_out = Alloy.render(env, component.template)

    # Since instantiate() instantiates a circuit, and rules such as x => 100 only
    # give us a node, here it's 100, we have to wrap it in a list to get a circuit,
    # (100), and subsequently unwrap it.
    instance = instantiate(components, trace, Term.of({render_out}))
    instance.items.first
  end

  # :nodoc:
  def make(components, trace, component : DeviceComponent, env : Term::Dict, *, ignore_surfaces : Bool = false)
    surfaces = [] of DeviceSurface

    rewritten = D7.map(clf, component.recipe) do |_, feature|
      if feature.is_a?(D7::Parent)
        next Term.of(feature.node)
      end

      # feature : D7::Inert | D7::Gnd
      ingredient = device_ingredient(feature.node)

      case ingredient
      in DeviceSurface
        if ignore_surfaces
          next Term.of(feature.node)
        end

        surfaces << ingredient

        # Generate parts, if any.
        parts = ingredient.captures.map do |name|
          Term.of(:part, {ingredient.edge, {:edge, name}}, ingredient.pattern)
        end

        if parts.size == 1
          # (surface (@surface (counter ±value))
          #   (counter 0))
          parts.first
        else
          # (surface (@surface (button action_ actions←(_*)))
          #   (button inc ()))
          part_group = Term::Dict.build do |commit|
            commit << :group
            commit.concat(parts)
          end

          Term.of(part_group)
        end
      in DeviceTemplate
        render_out = Alloy.render(env, Term.of(ingredient.template))

        # Since the input is a dict, the output must also be a dict. Alloy can't
        # destroy enclosing dicts. It also can't create pairs.
        instance = instantiate(components, trace, render_out)
        assert instance.itemsize > 0

        if instance.itemsize == 1
          # (template
          #   (cell @x ^foo))
          instance.items.first
        else
          # (template
          #   (cell @x ^foo)
          #   (cell @y ^bar))
          Term.of(instance.prepend(:group))
        end
      in DeviceNode
        unless feature.is_a?(D7::Inert)
          next feature.node # unchanged
        end

        make(components, trace, feature.node)
      end
    end

    if surfaces.size > 1
      # It is an error to have more than one surface. Since we have already
      # committed in map(), we have to "revert" by doing everything again.
      # Since this is an error branch, we don't care about its performance.
      return make(components, trace, component, env, ignore_surfaces: true)
    end

    unless surface_recipe = surfaces.single?
      # Render components without a surface recipe, such as:
      #
      #   (component
      #     (cell @a 0)
      #     (cell @b 0)
      #     (template
      #       (cell @c ^n)))
      #
      # This should be rendered as (e.g., with n=0):
      #
      #   (module {}
      #     (cell @a 0)
      #     (cell @b 0)
      #     (cell @c 0))
      result = Term::Dict.build do |commit|
        commit << :module << Term[]
        commit.concat(rewritten.items)
      end

      return Term.of(result)
    end

    # Render components with a surface recipe, as in:
    #
    #   (component
    #     (surface (@surface (button action_ actions←(_*)))
    #       (button ^action ()))
    #     (backsys
    #       ...))
    #
    # These components construct a `device`.

    surface = Alloy.render(env, surface_recipe.template)

    result = Term::Dict.build do |commit|
      commit << :device << {surface_recipe.edge, surface}
      commit.concat(rewritten.items)
    end

    Term.of(result)
  end

  alias DeviceRecipe = Array(DeviceIngredient)
  alias DeviceIngredient = DeviceSurface | DeviceTemplate | DeviceNode

  defrecord DeviceSurface, edge : Term, pattern : Term, captures : Set(Term), template : Term
  defrecord DeviceTemplate, template : Term::Dict
  defrecord DeviceNode, node : Term

  def device_ingredient(child : Term) : DeviceIngredient
    Term.case(child) do
      matchpi %{[surface (@edge_ pattern_) template_]} do
        normp = M1.normal(pattern)
        captures = M1.capture_names(normp)

        DeviceSurface.new(edge, pattern, captures, template)
      end

      matchpi %{[template template_*]} do
        DeviceTemplate.new(template.as_d)
      end

      otherwise do
        DeviceNode.new(child)
      end
    end
  end

  # Returns the components defined in *circuit*.
  def components(libraries : Enumerable(Term)) : Array(Component)
    # TODO: Store by head where possible!!
    components = [] of Component

    seq = 0u32

    libraries.each do |term|
      next unless library = term.as_d?

      library.items.each do |item|
        Term.case(item) do
          matchpi %{[rule pattern_ [component recipe_*]]} do
            normp = M1.normal(pattern)
            specificity = M1.specificity(normp)
            op = M1.operator(normp)

            component = DeviceComponent.new(seq, op, specificity, recipe)
            components << component
            seq += 1
          end

          matchpi %{[rule pattern_ template_]} do
            normp = M1.normal(pattern)
            specificity = M1.specificity(normp)
            op = M1.operator(normp)

            component = TemplateComponent.new(seq, op, specificity, template)
            components << component
            seq += 1
          end

          otherwise { }
        end
      end
    end

    # Sort most specific first.
    components.sort! { |a, b| b.specificity <=> a.specificity }
    components
  end

  # :nodoc:
  def instantiate(components, trace, circuit : Term) : Term
    D7.map(clf, circuit) do |_, feature|
      if feature.is_a?(D7::Inert)
        make(components, trace, feature.node)
      else
        Term.of(feature.node)
      end
    end
  end

  # Instantiates the components in *circuit*, recursively.
  #
  # *Instantiation* is simply the replacement of components with their bodies,
  # using Alloy, recursively -- essentially, a component is a macro, but with
  # slightly more processing.
  #
  # We do not have "subroutines" or "functions" in Rack, we have *components*,
  # which you can imagine as a library of pre-built circuits, exposed through
  # this macro-like interface. In fact, "a library of components" is exactly
  # the metaphor we're aiming at, even with planned GUIs. I have tried many
  # other approaches and all of them Rack resists fiercely. This one seems
  # to be the best one -- and the simplest one -- so far.
  #
  # Callers are expected to treat instantiation as a one-off event; the place where
  # it makes most sense is before the first rewrite step. In the future, we plan
  # to allow the user to insert components, live, from a menu of some sort, or
  # maybe from some kind of prompt where you can enter components, right at the cursor.
  # Right now, however, the only way to instantiate components is through this
  # `instantiate ` function.
  #
  # The circuit itself cannot instantiate components. Components can be instantiated
  # inside other components, but we explicitly forbid recursion: the same component
  # cannot instantiate itself within itself or within another, nested component.
  #
  # To achieve the latter, we trace our path through components throughout expansion,
  # making sure to not visit the same component twice. Note that we count *rules* as
  # components here, meaning e.g. `(C _) => (C 0), (C 0) => A` on `(C x)` expands to
  # `A` just fine. The apparent "name" of a component, `C`, is purely a human construct
  # here. What matters for the instantiator is the pattern as a whole. Ones that matched
  # are expended for that particular path.
  #
  # In this respect, a non-bug is the ability to do e.g. `(C _) => (C 0), (C _) => (C 1)`
  # (notice the same pattern repeated twice for different rules). On `(C x)`, this would
  # expand to `(C 0)` -> `(C 1)` (we preserve user rule order in case the patterns are
  # the same). It goes without saying that you shouldn't do this.
  def instantiate(circuit : Term, libraries : Enumerable(Term) = Slice(Term).empty) : Term
    components = components([circuit].concat(libraries))

    instantiate(components, Pf::USet32.new, circuit)
  end

  # Returns the Rack classifier.
  def clf : D7::Classifier
    ->classify(Term)
  end
end
