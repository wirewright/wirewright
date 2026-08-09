module Ww::Rack
  # Prevents `rack.guard` cycles.
  defrecord GuardAnnotation, addr : D7::NodeAddr, includes: {D7::Hypergraph::Annotation}

  # :nodoc:
  def classify!(node : Term) : D7::Feature
    M1::PatternSet.case(node, block_type: :proc) do
      # |@ rack.cell
      #
      # |@summary
      # Designates a place in the circuit where a term can be stored.

      # |@ rack.cell
      #
      # |@pattern
      # [cell @edge_]
      # [cell @edge_ value_]
      #
      # |@key edge rack.edge
      # The hyperedge ("group") the cell should be a member of.
      #
      # |@key value term
      # The term stored in the cell. If absent, the cell is *empty* and does not
      # participate in *edge*.
      #
      # |@block
      # Cells are one of the most fundamental nodes in Rack. They "fence off"
      # a part of a circuit and use it to store a term. The term does not
      # participate in rewriting; it is treated as a pure literal.
      #
      # |@example
      # Cells by themselves are inert:
      #
      # ```wwml
      # (cell @x 100)
      # (cell @y)
      # ```
      #
      # The `rack.feed` node can be used to move values between cells:
      #
      # ```wwml
      # (cell @x 100)
      # (cell @y)
      # (feed @x @y)
      # ```
      #
      # This evolves to:
      #
      # ```wwml
      # (cell @x)
      # (cell @y 100)
      # (feed @x @y)
      # ```
      matchpi %{[cell @edge_ _?]} do
        D7.gnd(node, edge)
      end

      # |@ rack.cell
      #
      # |@pattern
      # [cell (@edge_ pattern_) whole_]
      #
      # |@key edge rack.edge
      # The hyperedge ("group") the cell should be a member of. The name of this
      # edge (e.g. `foo` in `@foo`) is also used to retrieve the value of the cell
      # from the match env of *pattern* applied to *whole*.
      #
      # |@key pattern m1.operator
      # The pattern to match the value of *whole*. It should make a capture with
      # the same name as *edge* (e.g. `foo` in `@foo`).
      #
      # |@key whole term
      # The term stored in the cell and subject to scrutiny by the *pattern*.
      #
      # |@block
      # A variant of the cell node which exposes a part of *whole* at *edge*,
      # according to *pattern*.
      #
      # |@example
      # Consider the following example:
      #
      # ```wwml
      # (cell (@age {¦ ±age})
      #   {name: "John", age: 35})
      #
      # (backsys @age
      #   ±n <> {n: ^(+ n 1)})
      # ```
      #
      # In this example, the backsystem increments `age` -- a part of the value
      # stored in the `cell` -- forever:
      #
      # ```wwml
      # (cell (@age {¦ ±age})
      #   {name: "John", age: 36}) ;; 37, 38, ... in successive evolutions
      #
      # (backsys @age
      #   ±n <> {n: ^(+ n 1)})
      # ```
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

      # |@ rack.feed
      #
      # |@summary
      # Moves terms between places.

      # |@ rack.feed
      #
      # |@pattern
      # [feed source_ destination_]
      # [feed (not inhibitors_*) source_ destination_]
      #
      # |@key source rack.feed.source
      # One or more sources of terms.
      #
      # |@key destination rack.feed.destination
      # One or more corresponding destinations.
      #
      # |@key inhibitors rack.edge
      # One or more inhibitor edges.
      #
      # |@block
      # Feeds allow you to move terms from one place (usually designated by
      # `rack.cell`) to another. There are several variants of the feed node:
      #
      # - *Transfer*: move a term from one place to another (1:1).
      # - *Aggregate*: move terms from many places to one (M:1).
      # - *Broadcast*: move terms from one place to many (1:M).
      # - *Parallel transfer*: move terms from many places to many places (M:M)
      #
      # Feeds can have *inhibitors*. The presence of a nonempty cell at one of
      # inhibitor edges deactivates the feed.
      #
      # |@example
      #
      # ### Transfer
      #
      # ```wwml
      # (cell @x 100)
      # (cell @y)
      # (feed @x @y)
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (cell @x)
      # (cell @y 100)
      # (feed @x @y)
      # ```
      #
      # ### Aggregate
      #
      # ```wwml
      # (cell @x 100)
      # (cell @y 200)
      # (feed (@x @y) @z)
      # (cell @z)
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (cell @x)
      # (cell @y)
      # (feed (@x @y) @z)
      # (cell @z (100 200))
      # ```
      #
      # ### Broadcast
      #
      # ```wwml
      # (cell @x (100 200))
      # (feed @x (@y @z))
      # (cell @y)
      # (cell @z)
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (cell @x)
      # (feed @x (@y @z))
      # (cell @y 100)
      # (cell @z 200)
      # ```
      #
      # ### Parallel transfer
      #
      # ```wwml
      # (cell @x 100)
      # (cell @y 200)
      # (feed (@x @y) (@b @a)) ;; NOTICE how we flip the order: @b @a
      #
      # (cell @a)
      # (cell @b)
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (cell @x)
      # (cell @y)
      # (feed (@x @y) (@b @a))
      #
      # ;; We've flipped the order above:
      # (cell @a 200)
      # (cell @b 100)
      # ```
      matchpi %{[feed _*]} do
        continue unless spec = Feed.spec?(node)

        D7.gnd(node, Feed.edges(spec), defn: Feed.render(spec))
      end

      # |@ rack.feed
      #
      # |@pattern
      # [feed (%group edges_ (%past @_ min: 3))]
      #
      # |@key edges rack.edge
      # A sequence of three or more edges.
      #
      # |@block
      # Connects three or more edges in a chain.
      #
      # In other words, `(feed @a @b @c)` is the same as:
      #
      # ```wwml
      # (feed @a @b)
      # (feed @b @c)
      # ```
      #
      # |@example
      # ```wwml
      # ;; Frame 0 (seed)
      # (cell @a 0)
      # (cell @b)
      # (cell @c)
      # (feed @a @b @c @a)
      #
      # ;; Frame 1
      # (cell @a)
      # (cell @b 0)
      # (cell @c)
      # (feed @a @b @c @a)
      #
      # ;; Frame 2
      # (cell @a)
      # (cell @b)
      # (cell @c 0)
      # (feed @a @b @c @a)
      #
      # ;; Frame 3
      # (cell @a 0)
      # (cell @b)
      # (cell @c)
      # (feed @a @b @c @a)
      #
      # ;; ...
      # ```
      matchpi %{[feed (%group edges_ (%past @_ min: 3))]} do
        defn = Term::Dict.build do |commit|
          commit << :group
          edges.items.each_cons_pair do |a, b|
            commit << Term.of(:feed, a, b)
          end
        end

        D7.mixture(node, defn) { node }
      end

      # |@ rack.discard
      #
      # |@summary
      # Clears (empties) cell(s).

      # |@ rack.discard
      #
      # |@pattern
      # [discard @edge_]
      #
      # |@key edge rack.edge
      # The edge where the node should search for cells to clear.
      #
      # |@block
      # Empties zero or more cells at *edge*.
      #
      # |@example
      #
      # ### Basic
      #
      # ```wwml
      # (cell @a 100)
      # (discard @a)
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (cell @a)
      # (discard @a)
      # ```
      #
      # ### Timing
      #
      # ```wwml
      # (cell @a 100)
      # (discard @a)
      # (feed (copy @a) @b)
      # (cell @b)
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (cell @a)
      # (discard @a)
      # (feed (copy @a) @b)
      # (cell @b 100)
      # ```
      #
      # The observed behavior is the combination of `discard` and `feed`:
      # - `discard` empties the cell `@a` on the next tick.
      # - `feed...copy` does not touch `@a`, but copies its value into `@b` on
      #   the next tick.
      matchpi %{[discard @edge_]} do
        D7.gnd(node, edge)
      end

      # |@ rack.discard
      #
      # |@pattern
      # [discard @edge_ pattern_]
      #
      # |@key edge rack.edge
      # The edge where the node should search for cells to clear.
      #
      # |@key pattern m1.operator
      # Only cells containing values matching the pattern are emptied.
      #
      # |@block
      # Empties zero or more cells at *edge*, but only if their value matches
      # the given *pattern*.
      #
      # |@example
      #
      # ```wwml
      # (cell @info "John Doe")
      # (cell @info 25)
      # (discard @info _number)
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (cell @info "John Doe")
      # (cell @info)
      # (discard @info _number)
      # ```
      #
      # Notice how the number disappeared while the string remains in place.
      matchpi %{[discard @edge_ _]} do
        D7.gnd(node, edge)
      end

      # |@ rack.part
      #
      # |@pattern
      # [part (@src_ @dst_) pattern_]
      matchpi %{[part (@src_ @dst_) _]} do
        D7.gnd(node, src, dst)
      end

      # |@ rack.group
      #
      # |@pattern
      # [group children_*]
      #
      # |@key children rack
      # Zero or more child nodes.
      #
      # |@summary
      # Groups zero or more nodes without creating a scope.
      #
      # |@block
      # The group node can be used to group zero or more nodes together. Note that
      # `group` **does not** introduce any kind of scoping to edges or anything else
      # in the group.
      #
      # |@example
      # ```wwml
      # (group
      #   (cell @x 100)
      #   (cell @y)
      #   (feed @x @y))
      # ```
      matchpi %{[group _*]} do
        D7.parent(node.as_d, 1u32...node.uitemsize)
      end

      # |@ rack.module
      #
      # |@pattern
      # [module bindings_dict children_*]
      #
      # |@key bindings
      # A dictionary mapping edges in the *interior* of the module to edges in
      # its *exterior*. For example, if the bindings dict is `{@a: @foo, @b: @bar}`,
      # this means that `@a` inside the module will stand for `@foo` outside it,
      # and `@b` will stand for `@bar`. In a sense, `@foo` is "imported" from the
      # outside under the name `@a`. Often, the interior and exterior name is
      # the same, that is, e.g., `{@a: @a}`. In such cases ML offers the shorthand
      # `:@edge`. So `{@a: @a}` can be written as `{:@a}`. Similarly, if there
      # are multiple such bindings, as in `{@a: @a, @b: @b}`, you can write them
      # as `{:@a, :@b}`.
      #
      # |@key children rack
      # Zero or more child nodes.
      #
      # |@summary
      # Groups zero or more nodes and introduces a new scope, including edges
      # from the outside if needed.
      #
      # |@block
      # The module node introduces an scope into which edges can be imported
      # through the use of *bindings*.
      #
      # |@example
      # ```wwml
      # (cell @x 100)
      # (cell @y)
      # (feed @x @y)
      #
      # (module {}
      #   (cell @x 200)
      #   (cell @y)
      #   (feed @x @y))
      # ```
      #
      # The above evolves into:
      #
      # ```wwml
      # (cell @x)
      # (cell @y 100)
      # (feed @x @y)
      #
      # (module {}
      #   (cell @x)
      #   (cell @y 200)
      #   (feed @x @y))
      # ```
      #
      # Since the contents of the module are "sealed" from the outside due
      # to the empty bindings dict, there is no confusion between `feed`s.
      #
      # Edges can be "imported":
      #
      # ```wwml
      # (cell @x 100)
      # (cell @y)
      #
      # (module {:@x}
      #   (cell @y)
      #   (feed @x @y))
      # ```
      #
      # The above evolves to:
      #
      # ```wwml
      # (cell @x)
      # (cell @y)
      #
      # (module {:@x}
      #   (cell @y 100)
      #   (feed @x @y))
      # ```
      #
      # The `feed` refers to `@y` in the module because `@y` does not escape;
      # whereas `@x` does escape, so the cell outside the module participates
      # in the rewrite.
      matchpi %{[module bindings_dict _*]} do
        D7.scope(D7.parent(node.as_d, 2u32...node.uitemsize), bindings: bindings.as_d)
      end

      # |@ rack.locals
      #
      # |@pattern
      # [locals locals←((%past @_ min: 0)) _*]
      matchpi %{[locals locals←((%past @_ min: 0)) _*]} do
        D7.scope(D7.parent(node.as_d, 2u32...node.uitemsize), locals: locals.items)
      end

      # |@ rack.guard
      #
      # |@pattern
      # [guard (@edge_ pattern_) children_*]
      #
      # |@key edge rack.edge
      # The edge to search for cells at. The guard node will only attempt a match
      # if there is exactly one cell at *edge*. The node is "confused" by many cells
      # (even if some or all of them match).
      #
      # |@key pattern m1.operator
      # The pattern to match. All captures are discarded so there's no reason to
      # make them.
      #
      # |@key children rack
      # Child nodes to activate or deactivate. They are *not* isolated in any way.
      # If they are active, it's as if the guard does not exist.
      #
      # |@summary
      # Conditional activation of children based on a cell.
      #
      # |@block
      # Activates or deactivates *children* based on whether the cell at *edge*
      # matches *pattern*.
      #
      # |@example
      # ```wwml
      # ;; Frame 0 (seed)
      # (cell @turn alice)
      # (backsys @turn
      #   t←alice <> {t: bob}
      #   t←bob <> {t: alice})
      #
      # (guard (@turn alice)
      #   (queue (@x @xs) (1 2 3)))
      #
      # (guard (@turn bob)
      #   (queue (@x @xs) (100 200 300)))
      #
      # (feed @x @ys)
      # (queue (@y @ys) ())
      #
      # ;; Frame 1 (omitting backsys because it stays the same)
      # (cell @turn bob)
      # (guard (@turn alice)
      #   (queue (@x @xs) (2 3)))
      # (guard (@turn bob)
      #   (queue (@x @xs) (100 200 300)))
      # (feed @x @ys)
      # (queue (@y @ys) (1))
      #
      # ;; Frame 2
      # (cell @turn alice)
      # (guard (@turn alice)
      #   (queue (@x @xs) (2 3)))
      # (guard (@turn bob)
      #   (queue (@x @xs) (200 300)))
      # (feed @x @ys)
      # (queue (@y @ys) (1 100))
      #
      # ;; Frame 3
      # (cell @turn bob)
      # (guard (@turn alice)
      #   (queue (@x @xs) (3)))
      # (guard (@turn bob)
      #   (queue (@x @xs) (200 300)))
      # (feed @x @ys)
      # (queue (@y @ys) (1 100 2))
      #
      # ;; Frame 4
      # (cell @turn alice)
      # (guard (@turn alice)
      #   (queue (@x @xs) (3)))
      # (guard (@turn bob)
      #   (queue (@x @xs) (300)))
      # (feed @x @ys)
      # (queue (@y @ys) (1 100 2 200))
      #
      # ;; ... and so on until both queues are empty; at which point @turn
      # ;; just continues to cycle indefinitely.
      # ```
      matchpi %{[guard (@edge_ pattern_) _*]} do
        D7.parent(node.as_d, 2u32...node.uitemsize) do |hg, addr|
          ann = GuardAnnotation.new(addr)
          next false if hg.annotated_with?(ann) # cycle

          hg.annotate(ann) do
            next false unless dep = Rack.cell?(hg, hg.resolve(addr, edge))
            next false unless value = dep.value?
            next false unless M1.probe?(pattern, value)

            true # passable
          end
        end
      end

      # |@ rack.device
      #
      # |@pattern
      # [device children_*]
      matchpi %{[device _*]} do
        D7.circuit(node.as_d, 1u32...node.uitemsize, D7.inert(node))
      end

      # |@ rack.node
      #
      # |@pattern
      # [node @edge_]
      # [node @edge_ child_]
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

      # |@ rack.node
      #
      # |@pattern
      # [node (@edge_ pattern_) child_]
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

      # |@ rack.circuit
      #
      # |@pattern
      # [circuit @edge_ children_*]
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

      # |@ rack.circuit
      #
      # |@pattern
      # [circuit (pool @edge_) children_*]
      matchpi %{[circuit (pool @edge_) children0_*]} do
        mix0 = Term.of(:pool, edge, children0)

        leaf = D7.mixture(node, mix0) do |mix1|
          Term.matchpi(mix1, %{(pool @_ children1←[_*])}) do
            Term.of(node.replace(2...node.itemsize, Term.rep(children1.items)))
          end
        end

        D7.circuit(node.as_d, 2u32...node.uitemsize, leaf)
      end

      # Internal
      matchpi %{[pool @edge_ _]} do
        D7.gnd(node, edge)
      end

      # |@ rack.circuit
      #
      # |@pattern
      # [circuit (@edge_ pattern_) children_*]
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

      # |@ rack.frag
      #
      # |@pattern
      # [frag @edge_]
      # [frag @edge_ node_]
      #
      # |@block
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
      matchpi %{[frag @edge_ value0_]} do
        # NOTE: The empty case is handled below.
        mix0 = Term.of(:group, {:cell, edge, value0}, {:group, value0})

        D7.mixture(node, mix0) do |mix1|
          Term.case(mix1) do
            # New value arrived. Higher priority.
            matchpi %{(group (cell @_ value1_) _)} do
              continue if value0 == value1

              Term.morph(node, {2, value1})
            end

            # Value was erased. Higher priority.
            matchpi %{(group (cell @_) _)} do
              Term.morph(node, {2, nil})
            end

            # If cell did not change, use the updated group content as value.
            matchpi %{(group _ (group value1_))} do
              Term.morph(node, {2, value1})
            end
          end
        end
      end

      matchpi %{[frag @edge_]} do
        D7.mixture(node, Term.of(:cell, edge)) do |view|
          Term.of_case(view) do
            matchpi %{(cell @_)} { node }
            matchpi %{(cell @_ value1_)} { Term.morph(node, {2, value1}) }
          end
        end
      end

      # |@ rack.frag
      #
      # |@pattern
      # [frag (@edge_ selector_ -> patterns_*) child_]
      #
      # |@key edge rack.edge
      # The edge where the fragment should search for a source cell.
      #
      # |@key selector m1.operator
      # The pattern to use to extract part(s) of the value at *edge*.
      #
      # |@key patterns m1.operator
      # A list of alternative patterns to find part(s) of *child* with. The first
      # matching pattern is used. The fragment node assigns the captures made by
      # *selector* to the same-named places identified by the matching pattern.
      #
      # |@block
      # Binds parts of the value at *edge* (determined by *selector*) to part(s)
      # of a *child* node (determined by the first matching *pattern*) unidirectionally
      # (*edge* controls *child* but not the other way).
      #
      # |@example
      # ```wwml
      # ;; Frame 0 (seed)
      # (cell @xs (1 2 3))
      #
      # ;; Increment the middle number in @xs.
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      #
      # ;; Bind the value of the middle number to {count: _} in @ys using
      # ;; the fragment node.
      # (frag (@xs (_ ±n _) -> (cell @ys {¦ -count: n}) (cell @ys {¦ count: n_}))
      #   (cell @ys {}))
      #
      # ;; Frame 1
      # (cell @xs (1 3 3))
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      # (frag (@xs (_ ±n _) -> (cell @ys {¦ -count: n}) (cell @ys {¦ count: n_}))
      #   (cell @ys {count: 3}))
      #
      # ;; Frame 2
      # (cell @xs (1 4 3))
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      # (frag (@xs (_ ±n _) -> (cell @ys {¦ -count: n}) (cell @ys {¦ count: n_}))
      #   (cell @ys {count: 4}))
      #
      # ;; Frame 3
      # (cell @xs (1 5 3))
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      # (frag (@xs (_ ±n _) -> (cell @ys {¦ -count: n}) (cell @ys {¦ count: n_}))
      #   (cell @ys {count: 5}))
      #
      # ;; ...etc.
      # ```
      matchpi %{[frag (@edge_ _ -> _*) child0_]} do
        mix0 = Term.of(:group, Term.morph(node, {0, :rig}), child0)

        D7.mixture(node, mix0) do |mix1|
          Term.case(mix1) do
            # New value arrived. Higher priority.
            matchpi %{(group [_ _ child1_] _)} do
              continue if child0 == child1

              Term.morph(node, {2, child1})
            end

            # New value computed.
            matchpi %{(group _ child1_)} do
              Term.morph(node, {2, child1})
            end
          end
        end
      end

      # I'm not sure there's a point in exposing this node...
      matchpi %{[rig (@edge_ _ -> _*) _]} do
        D7.gnd(node, edge)
      end

      # |@ rack.delay
      #
      # |@pattern
      # [delay countdown←(%number +i32) node_]
      #
      # |@key countdown
      # A number counting down to zero (inclusive) with each rewrite tick.
      #
      # |@key node rack
      # The delay node is replaced by this node when the countdown reaches zero.
      #
      # |@summary
      # Delays the evolution of a child node by a number of rewrite ticks.
      #
      # |@block
      # Counts down to zero before allowing the evolution of a child node.
      #
      # |@example
      # Consider the following sequence of frames:
      #
      # ```wwml
      # ;; Frame 0 (seed)
      # (delay 3 (cell @x 0))
      # (feed @x @y)
      # (cell @y)
      #
      # ;; Frame 1
      # (delay 2 (cell @x 0))
      # (feed @x @y)
      # (cell @y)
      #
      # ;; Frame 2
      # (delay 1 (cell @x 0))
      # (feed @x @y)
      # (cell @y)
      #
      # ;; Frame 3
      # (delay 0 (cell @x 0))
      # (feed @x @y)
      # (cell @y)
      #
      # ;; Frame 4
      # (cell @x 0)
      # (feed @x @y)
      # (cell @y)
      #
      # ;; Frame 5
      # (cell @x)
      # (feed @x @y)
      # (cell @y 0)
      # ```
      matchpi %{[delay (%number +i32!)]}, %{[delay (%number +i32) _]} do
        D7.gnd(node)
      end

      # |@ rack.transfer
      #
      # |@pattern
      # [transfer (@src_ pattern_ @dst_) template_]
      #
      # |@key src rack.edge
      # The edge used to find the cell from which to take a value.
      #
      # |@key pattern m1.operator
      # The pattern used to gate transfer. The captures it makes are made available in
      # *template* as variables.
      #
      # |@key dst rack.edge
      # The edge used to find the cell where to place the value.
      #
      # |@key template alloy
      # The template to use to transform the value in passing. Captures made in
      # *pattern* are available in the template as variables.
      matchpi %{[transfer (@src_ pattern_ @dst_) template_]} do
        D7.mixture(node, Term.of(:transfer, { {:not}, {src}, {pattern}, dst }, template)) { node }
      end

      # |@ rack.transfer
      #
      # |@pattern
      # [transfer (srcs←((%past @_ min: 1)) pattern_ @dst_) template_]
      matchpi %{[transfer (srcs←((%past @_ min: 1)) pattern_ @dst_) template_]} do
        D7.mixture(node, Term.of(:transfer, { {:not}, srcs, pattern, dst }, template)) { node }
      end

      # |@ rack.transfer
      #
      # |@pattern
      # [transfer ((not (%group inhibitors_ (%past @_))) srcs←((%past @_ min: 1)) pattern_ @dst_) template_]
      matchpi %{[transfer ((not (%group inhibitors_ (%past @_))) srcs←((%past @_ min: 1)) pattern_ @dst_) template_]} do
        edges = [] of Term
        edges.concat(inhibitors.items)
        edges.concat(srcs.items)
        edges << dst

        D7.gnd(node, edges, defn: Term.of(:transfer, inhibitors, srcs, pattern, dst, template))
      end

      # |@ rack.backsys
      #
      # |@pattern
      # [backsys [backmap pattern_ _] backmaps_*]
      #
      # |@key pattern m1.operator
      # This key applies to all patterns in backmaps; I'm just highlighting
      # the first one. The following patterns are supported:
      #
      #  - `(%'%layer _ side_dict)`: takes pair keys from the *side* dict.
      #  - `_dict`: takes pair keys from the dict.
      #  - Other patterns are ignored.
      #
      # |@key backmaps
      # The other backmaps should have patterns like those described in *pattern*
      # to be able to participate in edge inference.
      #
      # |@block
      # Defines a backsystem over edges whose names are *inferred* from *backmaps*
      # based on their *pattern*.
      #
      # Instead of listing edges manually, you can let the `backsys` node infer
      # edges based on keys featured in *pattern*s. This helps avoid explicit
      # edge lists getting out of sync with what is actually needed by backmaps
      # in the backsystem -- which happens much more often than you'd expect
      # due to the need for manual maintenance!
      #
      # |@example
      # ```wwml
      # ;; Frame 0 (seed)
      # (backsys
      #   {¦ ±x} <> {x: ^(+ x 1)}
      #   {¦ ±y} <> {y: ^(- y 1)})
      #
      # (cell @x 0)
      # (cell @y 0)
      #
      # ;; Frame 1 (omitting backsys because it does not change)
      # (cell @x 1)
      # (cell @y -1)
      #
      # ;; Frame 2 (omitting backsys because it does not change)
      # (cell @x 2)
      # (cell @y -2)
      #
      # ;; etc...
      # ```
      #
      # The `backsys` above is the same as explicitly writing:
      #
      # ```wwml
      # (backsys {@:x, @:y}
      #   {¦ ±x} <> {x: ^(+ x 1)}
      #   {¦ ±y} <> {y: ^(- y 1)})
      # ```
      matchpi %{[backsys [backmap _ _] _*]} do
        keys = Set(Term).new

        backmaps = node.items.move(1)
        backmaps.each do |(_, pattern, _)|
          Term.case(pattern) do
            matchpi %{(%'%layer _ side_dict)} do
              side.each_entry(in: Term::Dict.pairspart) do |key, _|
                keys << key
              end
            end

            matchpi %{_dict} do
              pattern.each_entry(in: Term::Dict.pairspart) do |key, _|
                keys << key
              end
            end

            otherwise { }
          end
        end

        edges = Set(Term).new

        pairsrcs = Term::Dict.build do |commit|
          keys.each do |key|
            edge = Term.of(:edge, key)
            edges << edge
            commit.with(key, edge)
          end
        end

        D7.gnd(node, edges, defn: Term.of(:backsys, Term[], edges, pairsrcs, backmaps))
      end

      # |@ rack.backsys
      #
      # |@pattern
      # [backsys @src_ backmaps_*]
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

      # |@ rack.backsys
      #
      # |@pattern
      # [backsys ((%group itemsrcs_ (%past @_ min: 0)) ¦ pairsrcs_) backmaps_*]
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

      # |@ rack.queue
      #
      # |@pattern
      # [queue
      #   (@front_ @back_ ⍊
      #     min_: (%optional 1 (%number +i32!))
      #     max_: (%optional ∞ (%any° (%number +i32!) ∞)))
      #   buffer_dict]
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

      # |@ rack.view
      #
      # |@pattern
      # [view (@src_ pattern_ @dst_) template_]
      matchpi %{[view (@src_ pattern_ @dst_) template_]} do
        D7.gnd(node, src, dst, defn: Term.of(:view, { {src}, {pattern}, dst }, template))
      end

      # |@ rack.view
      #
      # |@pattern
      # [view (srcs←((%past @_ min: 1)) pattern_ @dst_) template_]
      matchpi %{[view (srcs←((%past @_ min: 1)) _ @dst_) _]} do
        edges = [] of Term
        edges.concat(srcs.items)
        edges << dst

        D7.gnd(node, edges)
      end

      # |@ rack.extension
      #
      # |@pattern
      # [extension (@src_ pattern_ @dst_) template_]
      matchpi %{[extension (@src_ pattern_ @dst_) template_]} do
        D7.gnd(node, src, dst, defn: Term.of(:extension, { {src}, {pattern}, dst }, template))
      end

      # |@ rack.extension
      #
      # |@pattern
      # [extension (@src_ src-pattern_ @dst_ dst-pattern_) template_]
      matchpi %{[extension (@src_ src-pattern_ @dst_ dst-pattern_) template_]} do
        D7.gnd(node, src, dst, defn: Term.of(:extension, { {src}, {src_pattern}, dst, dst_pattern }, template))
      end

      # |@ rack.extension
      #
      # |@pattern
      # [extension (srcs←((%past @_ min: 1)) src-pattern_ @dst_) template_]
      # [extension (srcs←((%past @_ min: 1)) src-pattern_ @dst_ dst-pattern_) template_]
      matchpi(
        %{[extension (srcs←((%past @_ min: 1)) _ @dst_ _) _]},
        %{[extension (srcs←((%past @_ min: 1)) _ @dst_) _]},
      ) do
        edges = [] of Term
        edges.concat(srcs.items)
        edges << dst

        D7.gnd(node, edges)
      end

      # |@ rack.sensor
      #
      # |@pattern
      # [sensor (tspace_ pattern_)]
      # [sensor (tspace_ pattern_) percept_]
      matchpi %{[sensor (_ _)]} do
        D7.gnd(node)
      end

      # |@ rack.sensor
      #
      # |@pattern
      # [sensor (many tspace_ pattern_) percepts_*]
      matchpi %{[sensor (many _ _)]} do
        D7.gnd(node)
      end

      # |@ rack.sensor
      #
      # |@pattern
      # [sensor (queue tspace_ pattern_) percepts_*]
      matchpi %{[sensor (queue _ _) _*]} do
        D7.gnd(node)
      end

      # |@ rack.sensor
      #
      # |@pattern
      # [sensor (view tspace_ pattern_) percepts_*]
      matchpi %{[sensor (view _ _) _*]} do
        D7.gnd(node)
      end

      # |@ rack.sensor
      #
      # |@pattern
      # [sensor (journal tspace_ pattern_) events_*]
      matchpi %{[sensor (journal _ _) _*]} do
        D7.gnd(node)
      end

      # |@ rack.appearance
      #
      # |@pattern
      # [appearance tspace_]
      # [appearance tspace_ value_]
      matchpi %{[appearance _ _]} do
        D7.gnd(node)
      end

      # |@ rack.surface
      matchpi %{[surface storage←[cell @_] [sensor (_ _) _]]} do
        mix0 = storage
        D7.mixture(node, mix0) do |mix1|
          Term.morph(node, {1, mix1})
        end
      end

      # |@ rack.surface
      matchpi %{[surface storage←[cell @_] [sensor (many _ _) _]]} do
        mix0 = storage
        D7.mixture(node, mix0) do |mix1|
          Term.morph(node, {1, mix1})
        end
      end

      # |@ rack.surface
      matchpi %{[surface storage←[cell @_] [sensor (view _ _) _]]} do
        mix0 = storage
        D7.mixture(node, mix0) do |mix1|
          Term.morph(node, {1, mix1})
        end
      end

      # |@ rack.surface
      matchpi %{[surface [cell @_ _] _]} do
        D7.parent(node.as_d, 1u32...2u32)
      end

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (@input_ -> @spec_ -> @output_) data_*]
      matchpi %{[rewriter (@input_ -> @spec_ -> @output_) _*]} do
        D7.gnd(node, input, spec, output)
      end

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (@input_ - @spec_ - @output_) data_*]
      matchpi %{[rewriter (@input_ - @spec_ - @output_) _*]} do
        D7.gnd(node, input, spec, output)
      end

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (@input_ - spec_ - @output_) data_*]
      # [rewriter (@input_ -> spec_ -> @output_) data_*]
      #
      # |@key spec rho
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

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (spec_ - @edge_) grammar_*]
      #
      # |@key spec rho
      matchpi %{[rewriter (spec_ - @edge_) body_*]} do
        edges = [edge]
        D7.gnd(node, edges, defn: Term.of(:rewriter, spec, edge, body))
      end

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (@spec_ - ((%group itemsrcs_ (%past @_ min: 0)) ¦ pairsrcs_)) data_*]
      matchpi %{[rewriter (@spec_ - ((%group itemsrcs_ (%past @_ min: 0)) ¦ pairsrcs_)) body_*]} do
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

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (spec_ - srcs_dict) data_*]
      #
      # |@key spec rho
      matchpi %{[rewriter (spec_ - srcs_dict) _*]} do
        # For example, the following rewriter:
        #
        #   (rewriter ((rulesetR) - {@:n @:m})
        #     {¦ ±n} <> {n: ^(+ n 1)}
        #     {¦ ±m} <> {n: ^(+ n 1)})
        #
        # ... should expand to:
        #
        #   (module {@n: @(local n), @m: @(local m)}
        #     (cell @spec (rulesetR))
        #     (rewriter (@spec - {n: @(local n), m: @(local m)})
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

      # |@ rack.slot
      #
      # |@pattern
      # [slot seed_]
      # [slot seed_ instance_]
      matchpi %{[slot _]} do
        D7.gnd(node)
      end

      matchpi %{[slot _ _]} do
        D7.parent(node.as_d, 2u32...3u32)
      end

      # |@ rack.journal
      #
      # |@pattern
      # [journal (@edge_ _?) _*]
      matchpi %{[journal (@edge_ _?) _*]} do
        # TODO: This node should supersede `log`, and be the swiss army knife of
        # temporal / evolution tracking in Rack.
        D7.gnd(node, edge)
      end

      # |@ rack.log
      #
      # |@pattern
      # [log
      #   (@all_ @last_ ⍊
      #     limit_: (%optional 10 (%number +i32))
      #     edges⋮ false)
      #   log_dict]
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

      # |@ rack.parser
      #
      # |@pattern
      # [parser (@input_ -> top_symbol -> @output_) grammar_*]
      matchpi %{[parser (@input_ -> _symbol -> @output_) _*]} do
        D7.gnd(node, input, output)
      end

      # |@ rack.parser
      #
      # |@pattern
      # [parser (@input_ -> top_symbol -> @output_ / @error_) grammar_*]
      matchpi %{[parser (@input_ -> _symbol -> @output_ / @error_) _*]} do
        D7.gnd(node, input, output, error)
      end

      # |@ rack.parser
      #
      # |@pattern
      # [parser (@input_ - top_symbol - @output_) grammar_*]
      matchpi %{[parser (@input_ - _symbol - @output_) _*]} do
        D7.gnd(node, input, output)
      end

      # |@ rack.parser
      #
      # |@pattern
      # [parser (@input_ - top_symbol - @output_ / @error_) grammar_*]
      matchpi %{[parser (@input_ - _symbol - @output_ / @error_) _*]} do
        D7.gnd(node, input, output, error)
      end

      # |@ rack.path
      #
      # |@summary
      # Live symbolic observation of a file system path.

      # |@ rack.path
      #
      # |@pattern
      # [path (path_string reading)]
      # [path (path_string reading) reading_]
      #
      # |@key path
      # Specifies the path to a file, for example, `/tmp/test.txt`.
      #
      # |@key reading rack.path.reading
      # The *reading* of the file, if available.
      #
      # |@block
      # A symbolic file viewer. Maintains a live reading of the file at the given
      # *path*. The reading will change as the file changes.
      #
      # |@example
      # Let's say you write `Hello` to `/tmp/test.txt`. Then:
      #
      # ```wwml
      # (path ("/tmp/test.txt" reading))
      # ```
      #
      # ... evolves to:
      #
      # ```wwml
      # (path ("/tmp/test.txt" reading)
      #   (present "Hello"))
      # ```
      #
      # If you then change the content of `/tmp/test.txt` to `Bye`:
      #
      # ```wwml
      # (path ("/tmp/test.txt" reading)
      #   (present "Bye"))
      # ```
      #
      # If you then remove the file:
      #
      # ```wwml
      # (path ("/tmp/test.txt" reading)
      #   (absent "file does not exist")) ;; error messages may depend on the OS!
      # ```
      #
      # If you want to operate on the content of the file, you're supposed
      # to observe the node's evolution:
      #
      # ```wwml
      # (node (@content (path _ (present content_string)))
      #   (path ("/tmp/test.txt" reading)))
      #
      # (parser (@content - top - @result)
      #   (top ws x←expr ws) => ^x
      #   (expr a←expr "+" b←nat) => ^(+ a b)
      #   (expr a←expr "-" b←nat) => ^(- a b)
      #   (expr nat)
      #   (nat (form "[0-9]+" nat))
      #   ;; Zero or more vertical or horizontal space characters.
      #   (ws "[%s]*"))
      #
      # (cell @result)
      # ```
      #
      # This program will watch the content of `/tmp/test.txt`, parse it,
      # and display the result, live, in `@result`. The cell will be cleared
      # if there is a parse error or if the file does not exist. For example,
      # if you write `2+2` to the file, the result cell will contain `4`. If
      # you change the expression, the result will be recalculated automatically.
      matchpi %{[path (_string reading)]}, %{[path (_string reading) _]} do
        D7.gnd(node)
      end

      # |@ rack.path
      #
      # |@pattern
      # [path (path_string report)]
      # [path (path_string report) report_]
      #
      # |@key path
      # Specifies the path to a file system entry, for example, `/tmp/test.txt`
      # or `/tmp/dir`.
      #
      # |@key report rack.path.report
      # The *report* about the file system entry, if available.
      #
      # |@block
      # A symbolic directory / file system explorer. Maintains a live report
      # about the file system entry at the given *path*. The report will
      # change as the entry changes (e.g., for a dictionary, as entries get
      # added or removed).
      #
      # |@example
      # I'm going to show the evolution of `path...report` as I create and
      # populate `/tmp/dir`.
      #
      # ```wwml
      # ;; Frame 0 (seed)
      # (path ("/tmp/dir" report))
      #
      # ;; Frame 1
      # (path ("/tmp/dir" report)
      #   (absent "path does not exist"))
      #
      # ;; $ mkdir /tmp/dir
      #
      # ;; Frame 2
      # (path ("/tmp/dir" report)
      #   (dir timestamp: "2026-07-28 18:01:43 UTC")) ;; Your timestamp will differ, of course!
      #
      # ;; $ mkdir /tmp/dir/a
      #
      # ;; Frame 3
      # (path ("/tmp/dir" report)
      #   (dir timestamp: "2026-07-28 18:01:48 UTC" ;; timestamp changed!
      #     (dir "a")))
      #
      # ;; $ touch /tmp/dir/b.txt
      #
      # ;; Frame 4
      # (path ("/tmp/dir" report)
      #   (dir timestamp: "2026-07-28 18:01:51 UTC" ;; timestamp changed!
      #     (dir "a")
      #     (file "b.txt")))
      #
      # ;; $ touch /tmp/dir/c.txt
      #
      # ;; Frame 5
      # (path ("/tmp/dir" report)
      #   (dir timestamp: "2026-07-28 18:01:62 UTC" ;; timestamp changed!
      #     (dir "a")
      #     (file "b.txt")
      #     (file "c.txt")))
      # ```
      #
      # ### Recursive watching
      # It is possible to watch things recursively:
      #
      # ```wwml
      # (circuit @reports
      #   (path ("/tmp/dir" report) root: true))
      #
      # (rewriter ((scanR (rulesetR)) - @reports)
      #  ;; Remove reports that are absent, except the root report.
      #  R←(path (_ report) [absent _] ⍊ -root)
      #    <> {(R): ()}
      #  ;; Spawn child reports.
      #  R←(path (path_string report) (dir members_* ⍊ -valid_) ⍊ offspring⋮ {})
      #    <> {(R):
      #          (^(up R)
      #           (^when ((members offspring) (⟨[_ member_string]⟩° (%-value member)))
      #             (path (^"⸢path⸣/⸢member⸣" report))))
      #        valid: true,
      #        offspring: ^(set members [_ member_string])})
      # ```
      #
      # Your first impression might be that this is a very baroque way of
      # doing things. However, this example actually demonstrates how Wirewright
      # wants you to think (even if the example itself is not particularly elegant
      # in writing.)
      #
      # In the above, the `@reports` circuit is effectively a nested symbolic world.
      # The ruleset in the rewriter defines some "laws" for the world. The rewriter
      # itself, `(scanR (rulesetR))`, tells the rewriter *how* to apply the laws. In
      # particular, `rho.scanR` goes through the items of `@reports`, applying
      # `rho.rulesetR` (and therefore, our laws) to each item in turn.
      #
      # There are two laws. The first one says that absent path reports except
      # the root report (which we mark manually with `root: true`) must cease
      # to exist. The second law says that updates to a path report must spawn
      # offspring path reports.
      #
      # To understand the laws more intuitively, let's illustrate them in human terms.
      # Remember path reports are basically symbolic file explorers? Imagine a standard
      # OS file explorer. It gives you a live view of a directory (or a file). You
      # can see the members of a directory; you can see them get added and
      # removed, live.
      #
      # If you wanted to do recursive watching with this setup, by hand, what would
      # you do? Open new file explorer windows for each member. Then more windows
      # for subdirectories. Then open windows for their members in turn. In the end,
      # you have a messy desktop filled with windows.
      #
      # The moment any window changes, you look at it to see what was *added* or
      # *removed*. For things that were added, you spawn subwindows. That's
      # the second law.
      #
      # The first law is, when you see a window telling "this path no longer exists",
      # maybe grayed out, you close the window. That's it. Unless it's root, of course;
      # if you close the root you won't have anything to watch, just a blank screen,
      # and that's not good.
      #
      # Now, if you imagine each window *itself* subdivides into sub-windows, and each
      # grayed out window closes itself automatically, rather than requiring a human
      # to do it, then the picture you get is basically how the example above works --
      # except instead of visual, OS file explorer windows, we have symbolic ones, residing
      # inside `@reports`. The rewriter laws simply automate what a human would do:
      # windows open sub-windows as directories appear, and dead windows close themselves
      # automatically.
      #
      # The implementation of the second law is so intricate because we want to avoid
      # spawing duplicate path reports. So when we spawn offspring reports (by "we",
      # I mean the second law), we mark the directory as `valid`. When the environment
      # updates our directory description, it takes down our `valid` flag (the fresh
      # description simply doesn't have it; there is no deeper intent). The absence of
      # the `valid` flag triggers us to check the report out. We only want to spawn
      # offspring for members that were *added* to avoid duplication. To keep track
      # of that we keep the previous set of members under *offspring*. The `alloy.when`
      # then "diffs" and only spawns reports for new members. Again, we only take care
      # of the new members. The old members we've already spawned; they are already
      # "living freely" in `@reports`, recursively applying the same laws, and being
      # updated by the environment.
      #
      # You are then supposed to *observe* the evolution of `@reports`, filter for
      # reports you're interested in, etc.
      #
      # Alternatively, it is possible to use `rack.supervisor` to associate arbitrary
      # `rack.device`s with each report. For example, the program below will record paths
      # appearing and disappearing:
      #
      # ```wwml
      # (circuit @reports
      #   (path ("/tmp/wirewright-path1" report) root: true))
      #
      # (rewriter ((scanR (rulesetR)) - @reports)
      #   ;; Remove reports that are absent, except the root report.
      #   R←(path (_ report) [absent _] ⍊ -root)
      #     <> {(R): ()}
      #   ;; Spawn child reports.
      #   R←(path (path_string report) (dir members_* ⍊ -valid_) ⍊ offspring⋮ {})
      #     <> {(R):
      #           (^(up R)
      #            (^when ((members offspring) (⟨[_ member_string]⟩° (%-value member)))
      #              (path (^"⸢path⸣/⸢member⸣" report))))
      #         valid: true,
      #         offspring: ^(set members [_ member_string])})
      #
      # (supervisor (@reports @report [path (path_string report) _] - @pool)
      #   (frag (@report [path (path_string report) _]
      #                    -> (appearance paths `path)
      #                       (appearance paths path_))
      #     (appearance paths)))
      #
      # (circuit (pool @pool))
      #
      # (frag @journal
      #   (sensor (journal paths _string)))
      # ```
      #
      # The first half is basically the same as in the previous example, but in
      # the second half we have a `rack.supervisor` node maintaining a pool of
      # devices associated with each path. The only thing each device does in
      # this example is maintain an appearance containing the corresponding
      # report's path. The appearance shows this path to the `paths` termspace.
      #
      # Finally, there's the journal sensor which tracks appearances joining
      # and leaving the `paths` termspace.
      matchpi %{[path (_string report)]}, %{[path (_string report) _]} do
        D7.gnd(node)
      end

      # |@ rack.fs
      #
      # |@pattern
      # [fs @request_ @response_]
      #
      # |@key request rack.edge
      # The edge of a cell containing a file system request. See `rack.fs.request`.
      #
      # |@key response rack.edge
      # The edge of a cell where a file system response should be placed in
      # response to a *request*. See `rack.fs.response`.
      #
      # |@summary
      # A node for creating, deleting, writing files and directories.
      #
      # |@block
      # A _f_ile _s_ystem manipulation machine.
      #
      # Real-world file systems are very hard to manipulate declaratively (due to
      # the abundance of races). Instead of fighting with reality, Rack provides `fs`,
      # a node that you can use to talk to the file system as if it was a "server":
      # you send *requests*, and the file system replies with *responses*.
      #
      # |@example
      # Creating a file:
      #
      # ```wwml
      # ;; $ ls /tmp/a
      # ;; ls: ... no such file or directory ...
      #
      # ;; Frame 0 (seed)
      #
      # (cell @in (create file "/tmp/a"))
      # (fs @in @out)
      # (cell @out)
      #
      # ;; Frame 1
      #
      # (cell @in)
      # (fs @in @out)
      # (cell @out (ok (present "/tmp/a")))
      #
      # ;; $ ls /tmp/a
      # ;; /tmp/a
      # ```
      #
      # Removing a file:
      #
      # ```wwml
      # ;; $ ls /tmp/a
      # ;; /tmp/a
      #
      # ;; Frame 0 (seed)
      #
      # (cell @in (delete file if exists "/tmp/a"))
      # (fs @in @out)
      # (cell @out)
      #
      # ;; Frame 1
      #
      # (cell @in)
      # (fs @in @out)
      # (cell @out (ok (absent "/tmp/a")))
      #
      # ;; $ ls /tmp/a
      # ;; ls: ... no such file or directory ...
      # ```
      #
      # Writing to a file:
      #
      # ```wwml
      # ;; Frame 0 (seed)
      #
      # (cell @in (overwrite file "/tmp/a" "Kaixo mundua!"))
      # (fs @in @out)
      # (cell @out)
      #
      # ;; Frame 1
      #
      # (cell @in)
      # (fs @in @out)
      # (cell @out (ok (wrote "/tmp/a")))
      #
      # ;; $ cat /tmp/a
      # ;; Kaixo mundua!
      # ```
      #
      # Executing a sequence of requests:
      #
      # ```wwml
      # ;; Frame 0 (seed)
      #
      # (queue (@request @requests)
      #   ((create dir if missing "/tmp/dir")
      #    (overwrite "/tmp/dir/a" "Hello from file A")
      #    (overwrite "/tmp/dir/b" "Hello from file B")))
      # (fs @request @response)
      # (cell @response)
      # (discard @response (ok _))
      #
      # ;; Frame 1
      #
      # (queue (@request @requests)
      #   ((overwrite "/tmp/dir/a" "Hello from file A")
      #    (overwrite "/tmp/dir/b" "Hello from file B")))
      # (fs @request @response)
      # (cell @response (ok (present "/tmp/dir")))
      # (discard @response (ok _))
      #
      # ;; Frame 2
      # ;; NOTE: The fs node (as well as many other nodes) requires its output
      # ;; cell (*response*) to be empty before it picks up a request. In this
      # ;; frame, `discard` clears the response cell. `fs` does nothing.
      #
      # (queue (@request @requests)
      #   ((overwrite "/tmp/dir/a" "Hello from file A")
      #    (overwrite "/tmp/dir/b" "Hello from file B")))
      # (fs @request @response)
      # (cell @response)
      # (discard @response (ok _))
      #
      # ;; Frame 3
      # ;; Now that the response cell is clear, `fs` can pick up another request.
      #
      # (queue (@request @requests)
      #   ((overwrite "/tmp/dir/b" "Hello from file B")))
      # (fs @request @response)
      # (cell @response (ok (wrote "/tmp/dir/a")))
      # (discard @response (ok _))
      #
      # ;; Frame 4
      #
      # (queue (@request @requests)
      #   ((overwrite "/tmp/dir/b" "Hello from file B")))
      # (fs @request @response)
      # (cell @response)
      # (discard @response (ok _))
      #
      # ;; Frame 5
      #
      # (queue (@request @requests) ())
      # (fs @request @response)
      # (cell @response (ok (wrote "/tmp/b")))
      # (discard @response (ok _))
      #
      # ;; Frame 6
      #
      # (queue (@request @requests) ())
      # (fs @request @response)
      # (cell @response)
      # (discard @response (ok _))
      #
      # ;; $ ls /tmp/dir
      # ;; a b
      # ;; $ cat /tmp/a
      # ;; Hello from file A
      # ;; $ cat /tmp/b
      # ;; Hello from file B
      # ```
      matchpi %{[fs @request_ @response_]} do
        D7.gnd(node, request, response)
      end

      # |@ rack.resource
      #
      # |@pattern
      # [resource query_]
      # [resource query_ response_]
      matchpi %{[resource _]}, %{[resource _ _]} do
        D7.gnd(node)
      end

      # |@ rack.db
      #
      # |@pattern
      # [db (@stmt_ -> uri_string -> @response_)]
      # [db (@stmt_ -> uri_string -> @response_) status_]
      #
      # |@key stmt rack.edge
      # The edge used to find the cell containing the SQL statement to execute.
      # See also: `rack.db.stmt`.
      #
      # |@key uri
      # The URI to connect to the database. Currently, only the following databases
      # are supported:
      # - SQLite3: for example, `sqlite:/tmp/people.db`.
      #
      # |@key response rack.edge
      # The edge used to find the cell to store the database response. The response
      # can be of several forms:
      # - `(ok ±rows-affected)` for successful `exec` statements.
      # - `(ok rows_dict*)` for successful `query` statements.
      # - `(err detail_string)` for database errors.
      #
      # |@key status
      # The status of the connection. This value is maintained by the `db` node
      # and is meant for observation (e.g. drawing a "green" connected circle
      # or something along those lines).
      #
      # - `up` means the connection is active.
      # - `pending` means there is an ongoing transition.
      # - `(dn detail_string)` means the connection is inactive, with *detail
      #   providing more details as to why.
      matchpi(
        %{[db (@stmt_ -> _string -> @response_)]},
        %{[db (@stmt_ -> _string -> @response_) _]},
      ) do
        D7.gnd(node, stmt, response)
      end

      # |@ rack.ws
      #
      # |@summary
      # WebSocket server and client nodes.

      # |@ rack.ws
      #
      # |@pattern
      # [ws (server @pool_ binding_ (%plural status min: 0 max: 1)
      #       ⍊ format_⋮ none
      #         format-policy_⋮ discard
      #         in_⋮ @in
      #         out_⋮ @out)
      #   template_*]
      #
      # |@key pool rack.edge
      # Tells where to look for the device circuit pool. For each active client
      # connection, a device is created and maintained in the pool. This is very
      # much like how `rack.supervisor` works, except instead of lists as the backing
      # collection we have WebSocket connections.
      #
      # |@key binding rack.ws.binding
      # Specifies where to bind the server.
      #
      # |@key status
      # The status of the server. Some applications display a green circle to indicate
      # they are connected, then perhaps a blue one to indicate pending, and a red one
      # to indicate failure to connect. `rack.ws` nodes do something similar, except
      # instead of a circle we use a symbol. Below is an exhaustive list of the possible
      # values for *status*. If *status* is missing, then for that frame it is indeterminate.
      #
      # - `pending`: the server is starting up.
      # - `up`: the server is running.
      # - `(dn detail_string)`: the server is not running, with *detail* explaining why.
      #
      # |@key format rack.ws.format
      # Specifies the message format to use, for example, `none`, `json`, `ml`.
      #
      # |@key format-policy rack.ws.format-policy
      # Specifies how to ensure adherence to the format.
      #
      # |@key in rack.edge
      # Optionally, renames the incoming message queue.
      #
      # |@key out rack.edge
      # Optionally, renames the outgoing message queue.
      #
      # |@key template rack
      # Rack nodes to include in each instantiated device (one per client). Each
      # client device is generated at least with these cells:
      #
      # - `(cell @id _string)`: stores the UUID of the client.
      # - `(cell @in (_*))`: the incoming message queue. It stores messages *after* they
      #   are decoded by *format*. The edge can be renamed using *in*.
      # - `(cell @out (_*))`: the outgoing message queue. It stores messages *before* they
      #   are encoded by *format*. The edge can be renamed using *out*.
      #
      # |@block
      # Runs a WebSocket server at *binding*. This node works similar to `rack.supervisor`:
      # it maintains a pool of devices, one per active client connection; and as clients come
      # and go, their corresponding devices are added and removed. Devices can be extended
      # by providing a *template*, which is the same as the template in `rack.supervisor`.
      #
      # When using `rack.ws` server, we distinguish the following things:
      # - A *server* is a node maintaining a population of *client devices*.
      # - A *client* is a node which is separated from the server by the network.
      # - A *client device* is a `rack.device` representative of a client
      #   *on the server side*. Think of it as an "avatar" or an "ambassador"
      #   of the client, or a kind of "robotic arm" the client controls remotely.
      #
      # *template* equips each client device with the means to carry out whatever
      # the corresponding client wants. Since the server controls what a client
      # device can do, it also controls the kinds of behaviors a client can perform
      # remotely. Clients can't just do arbitrary code execution (unless you
      # explicitly allow it, and there are ways to do it safely in Wirewright,
      # although it won't be "arbitrary" anymore).
      #
      # ### Notes
      #
      # - `ws` ignores non-`device` nodes in *pool*.
      # - To close the connection on the server-side, the corresponding client device
      #   must clear its id cell. `ws` garbage-collects nodes with an empty id cell.
      # - Client devices do not structurally require the ingoing and outgoing message
      #   queues. If the ingoing message queue cell is absent or empty, the client
      #   device won't be able to receive messages from the client (all messages will
      #   be discarded). Similarly, if the outgoing queue is disabled, the client device
      #   will not be able to send messages back to the client. This behavior could be
      #   if you want to "mute" clients.
      #
      # |@example
      #
      # ### Essentials
      #
      # Let's start a noop server at 127.0.0.1:5000:
      #
      # ```wwml
      # (ws (server @pool (local 5000)))
      # (circuit (pool @pool))
      # ```
      #
      # This evolves through the following:
      #
      # ```wwml
      # ;; Frame 1
      # (ws (server @pool (local 5000) pending)) ;; The server is starting...
      # (circuit (pool @pool))
      #
      # ;; Frame 2
      # (ws (server @pool (local 5000) up)) ;; The server started successfully!
      # (circuit (pool @pool))
      # ```
      #
      # Now let me connect to it:
      #
      # ```wwml
      # ;; Frame 3
      # (ws (server @pool (local 5000) up))
      # (circuit (pool @pool)
      #   (device
      #     ;; Your id will differ, ids are UUIDv4.
      #     (cell @id "84d8e6e9-5b10-4743-93f5-c925ace9c138")
      #     (cell @in ())
      #     (cell @out ())))
      # ```
      #
      # Now I will send a message, let's say, `hello`.
      #
      # ```wwml
      # ;; Frame 4
      # (ws (server @pool (local 5000) up))
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "84d8e6e9-5b10-4743-93f5-c925ace9c138")
      #     (cell @in ("hello\n"))
      #     (cell @out ())))
      # ```
      #
      # Notice how it is directed to the incoming message queue. Let's introduce
      # another client:
      #
      # ```wwml
      # ;; Frame 5
      # (ws (server @pool (local 5000) up))
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "84d8e6e9-5b10-4743-93f5-c925ace9c138")
      #     (cell @in ("hello\n"))
      #     (cell @out ()))
      #   (device
      #     (cell @id "77d394f2-9a54-4f58-a5d3-ead50973d4fc")
      #     (cell @in ())
      #     (cell @out ())))
      # ```
      #
      # Now if I disconnect the first client (the one I sent `hello` from):
      #
      # ```wwml
      # ;; Frame 6
      # (ws (server @pool (local 5000) up))
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "77d394f2-9a54-4f58-a5d3-ead50973d4fc")
      #     (cell @in ())
      #     (cell @out ())))
      # ```
      #
      # Notice how `ws` removed the corresponding client device.
      #
      # ### Echo
      #
      # Let's start an echo server at 127.0.0.1:5000:
      #
      # ```wwml
      # (ws (server @pool (local 5000))
      #   (feed (@in front) (@out back)))
      #
      # (circuit (pool @pool))
      # ```
      #
      # Let's trace its evolution as I connect to it and write `hello`:
      #
      # ```wwml
      # ;; Frame 1
      # (ws (server @pool (local 5000) pending)
      #   (feed (@in front) (@out back)))
      #
      # (circuit (pool @pool))
      #
      # ;; Frame 2
      # (ws (server @pool (local 5000) up)
      #   (feed (@in front) (@out back)))
      #
      # (circuit (pool @pool))
      #
      # ;; Frame 3
      # ;; I connect to the server. From now on I'll omit `ws` because it does
      # ;; not change.
      #
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "fcc51789-929e-4dd2-a209-204a38cc80d3")
      #     (cell @in ())
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # ;; Frame 4.1
      # ;; I write `hello`.
      #
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "fcc51789-929e-4dd2-a209-204a38cc80d3")
      #     (cell @in ("hello\n"))
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # ;; Frame 4.2
      # ;; The feed node moves my message to the outgoing message queue.
      #
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "fcc51789-929e-4dd2-a209-204a38cc80d3")
      #     (cell @in ())
      #     (cell @out ("hello\n"))
      #     (feed (@in front) (@out back))))
      #
      # ;; Frame 5
      # ;; The message is picked up by the runtime and sent as a reply.
      #
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "fcc51789-929e-4dd2-a209-204a38cc80d3")
      #     (cell @in ())
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # ;; I see the server reply: `hello`.
      # ```
      #
      # ### Format
      #
      # Instead of taking with clients using plain text (or bytes), which is
      # what the default `format: none` does, we can talk using terms. There
      # are many ways to do that, see `rack.ws.format`. For example, we can
      # use `ml`:
      #
      # ```wwml
      # (ws (server @pool (local 5000) format: ml)
      #   (feed (@in front) (@out back)))
      #
      # (circuit (pool @pool))
      # ```
      #
      # Let's see what happens after I connect and send `(+ 1 2)`:
      #
      # ```wwml
      # ;; Initialization and the `ws` node itself are omitted for brevity.
      #
      # ;; Frame N
      # ;; I connected
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "c9d6b3c3-60bf-49e2-a620-69c44a041d37")
      #     (cell @in ())
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # ;; Frame N+1
      # ;; I sent `(+ 1 2)`. Notice how it has arrived as a term.
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "c9d6b3c3-60bf-49e2-a620-69c44a041d37")
      #     (cell @in ((+ 1 2)))
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # ;; Frame N+2
      # ;; The feed node moves the term to the outgoing message queue. The outgoing
      # ;; message queue, too, accepts terms now that we're using format: ml.
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "c9d6b3c3-60bf-49e2-a620-69c44a041d37")
      #     (cell @in ())
      #     (cell @out ((+ 1 2)))
      #     (feed (@in front) (@out back))))
      #
      # ;; Frame N+3
      # ;; The runtime consumed the term, encoded it, and sent it over the network.
      # (circuit (pool @pool)
      #   (device
      #     (cell @id "c9d6b3c3-60bf-49e2-a620-69c44a041d37")
      #     (cell @in ())
      #     (cell @out ())
      #     (feed (@in front) (@out back))))
      #
      # ;; I see the server reply: `(+ 1 2)`.
      # ```
      #
      # Again, see `rack.ws.format` to learn more about the available formats.
      # For JSON, you might find `rack.schema` useful.
      #
      # By default, messages that fail to *decode* (client sends malformed stuff)
      # are *discarded*. This behavior is controlled by `format-policy`. See
      # `rack.ws.format-policy` to learn about other policies (e.g. closing connection,
      # wrapping messages in a result type, etc.)
      #
      # ### More complex examples
      #
      # See the examples directory for more complex examples. WebSocket examples
      # are prefixed with `websocket-`.
      matchpi %{[ws [server @pool_ _ _?] _*]} do
        D7.gnd(node, pool)
      end

      # If a websocket is down (e.g. cannot connect to the server), don't waste
      # time interacting with the WebSocket machinery.
      matchpi %{[ws [client (@_ -> _ -> @_)] [dn _string]]} do
        D7.inert(node)
      end

      # |@ rack.ws
      #
      # |@pattern
      # [ws (client (@message_ -> conn_ -> @reply_) ⍊ ⋮format ⋮format-policy)]
      # [ws (client (@message_ -> conn_ -> @reply_) ⍊ ⋮format ⋮format-policy) status_]
      #
      # |@key message rack.edge
      # Tells where to look for a message cell. Note that this expects a cell containing
      # *just one message*, not a queue of messages. You can queue messages using the queue
      # node `rack.queue`, or by other means (e.g. through a backsystem `rack.backsys` that
      # eventually writes to the cell at *message*; or using `rack.part` instead of a cell
      # for *message*).
      #
      # The client clears *message* when it is filed. Note that since we're using
      # plain WebSockets, we can't actually guarantee that a cleared *message* cell
      # implies message delivered; the connection may close with the message
      # in flight etc. So in "serious" scenarios, treat *message* as a "volatile"
      # kind of cell; perhaps require the server to confirm receipt before clearing
      # your own, logical message cell.
      #
      # |@key conn rack.ws.conn
      # Specifies where to find and how to connect to the WebSocket server
      # of interest.
      #
      # |@key reply rack.edge
      # Tells where to look for a reply cell. Just as with *message*, this expects
      # a cell for *just one message*. If you want to queue messages (and most likely,
      # you do!), use e.g. the `rack.queue` node.
      #
      # There is no built-in backpressure with plain WebSockets. If the *reply* cell
      # is occupied, the runtime will buffer messages until *reply* is cleared, which
      # could, in degenerate cases, lead to memory leaks. So you are advised to
      # queue *replies* yourself to avoid or at least have control over said leaks.
      #
      # |@key format rack.ws.format
      # Specifies the message format to use, for example, `none`, `json`, `ml`.
      #
      # |@key format-policy rack.ws.format-policy
      # Specifies how to ensure adherence to the format.
      #
      # |@key status
      # Connection status. Some applications display a green circle to indicate they
      # are connected, then perhaps a blue one to indicate pending, and a red one
      # to indicate failure to connect. `rack.ws` nodes do something similar, except
      # instead of a circle we use a symbol. Below is an exhaustive list of the possible
      # values for *status*. If *status* is missing, then for that frame it is indeterminate.
      #
      # - `pending`: the client is connecting to the server.
      # - `up`: the connection is active.
      # - `dn`: the connection was closed on the client-side (this is mainly used to
      #   "disable" or "turn off" web socket clients without erasing them from
      #   the circuit.)
      # - `(dn detail_string)`: the connection failed and no reconnect attempts
      #   will be made; *detail* explains the reason. To reconnect manually (aka "try again"),
      #   you should replace this status by `pending` or remove it.
      # - `closed`: the connection was closed nominally by the server. This is
      #   different from `dn`, which represents client-initiated closure. To
      #   reconnect, when necessary, you can replace `closed` by `pending` or
      #   simply remove it.
      #
      # |@block
      # Maintains a connection to the WebSocket server at *conn*.
      #
      # |@example
      #
      # ### Essentials
      #
      # No format. We'll use the `format: none` echo server from the examples for
      # the server overload.
      #
      # ```wwml
      # (cell @message "Hello World")
      # (cell @reply)
      # (ws (client (@message -> (local 5000) -> @reply)))
      # ```
      #
      # The client node has to connect first:
      #
      # ```wwml
      # ;; Frame 1
      #
      # (cell @message "Hello World")
      # (cell @reply)
      # (ws (client (@message -> (local 5000) -> @reply))
      #   pending) ;; Connecting...
      #
      # ;; Frame 2
      #
      # (cell @message "Hello World")
      # (cell @reply)
      # (ws (client (@message -> (local 5000) -> @reply))
      #   up) ;; Connected successfully!
      #
      # ;; Frame 3
      #
      # (cell @message) ;; < message was sent
      # (cell @reply)
      # (ws (client (@message -> (local 5000) -> @reply))
      #   up)
      #
      # ;; Frame 4
      #
      # (cell @message)
      # (cell @reply "Hello World") ;; we've received the response!
      # (ws (client (@message -> (local 5000) -> @reply))
      #   up)
      # ```
      #
      # Using queues instead of `cell`s:
      #
      # ```wwml
      # (queue (@message @messages)
      #   ("First message"
      #    "Second message"
      #    "Third message"))
      # (queue (@reply @replies) ())
      # (ws (client (@message -> (local 5000) -> @reply)))
      # ```
      #
      # After the connection is initialized, you'll see messages from the message
      # queue being filed to the server, and responses coming back. The way I'm
      # going to number frames is just one way out of many. In this case, a lot
      # of alternative arrangements are possible depending on network latency etc.
      # That is, some messages can arrive in batches rather than each taking
      # a separate frame. We can send multiple messages before a reply arrives, too.
      # But generally, the main thing we're guaranteed is *order*.
      #
      # ```wwml
      # ;; Frame N
      # ;; The first message is filed.
      #
      # (queue (@message @messages)
      #   ("Second message"
      #    "Third message"))
      # (queue (@reply @replies) ())
      # (ws (client (@message -> (local 5000) -> @replies))
      #   up)
      #
      # ;; Frame N+1
      # ;; The second message is filed, the reply for the first one arrives.
      #
      # (queue (@message @messages)
      #   ("Third message"))
      # (queue (@reply @replies)
      #   ("First message"))
      # (ws (client (@message -> (local 5000) -> @replies))
      #   up)
      #
      # ;; Frame N+2
      # ;; The third message is filed, the reply for the second one arrives.
      #
      # (queue (@message @messages) ())
      # (queue (@reply @replies)
      #   ("First message"
      #    "Second message"))
      # (ws (client (@message -> (local 5000) -> @replies))
      #   up)
      #
      # ;; Frame N+3
      # ;; The reply for the third message arrives.
      #
      # (queue (@message @messages) ())
      # (queue (@reply @replies)
      #   ("First message"
      #    "Second message"
      #    "Third message"))
      # (ws (client (@message -> (local 5000) -> @replies))
      #   up)
      # ```
      #
      # ### Format
      #
      # Things work exactly the same as in the server overload. You can specify
      # a format other than `format: none`, e.g., `format: ml` or `format: json`,
      # and the client node will (de)serialize terms appropriately behind the scenes.
      #
      # ```wwml
      # (queue (@message @messages) (foo 100 ≈1.23 false (+ 1 2 x: 100 y: 200)))
      # (queue (@reply @replies) ())
      # (ws (client (@message -> (local 5000) -> @replies) format: ml))
      # ```
      #
      # This circuit evolves as follows (the same caveats about order/arrangement aplly
      # as in the examples above):
      #
      # ```wwml
      # ;; Frame N
      #
      # (queue (@message @messages) (foo 100 ≈1.23 false (+ 1 2 x: 100 y: 200)))
      # (queue (@reply @replies) ())
      # (ws (client (@message -> (local 5000) -> @replies) format: ml)
      #   up)
      #
      # ;; Frame N+1
      #
      # (queue (@message @messages) (100 ≈1.23 false (+ 1 2 x: 100 y: 200)))
      # (queue (@reply @replies) ())
      # (ws (client (@message -> (local 5000) -> @replies) format: ml)
      #   up)
      #
      # ;; Frame N+2
      #
      # (queue (@message @messages) (≈1.23 false (+ 1 2 x: 100 y: 200)))
      # (queue (@reply @replies) (foo))
      # (ws (client (@message -> (local 5000) -> @replies) format: ml)
      #   up)
      #
      # ;; Frame N+3
      #
      # (queue (@message @messages) (false (+ 1 2 x: 100 y: 200)))
      # (queue (@reply @replies) (foo 100))
      # (ws (client (@message -> (local 5000) -> @replies) format: ml)
      #   up)
      #
      # ;; Frame N+3
      #
      # (queue (@message @messages) ((+ 1 2 x: 100 y: 200)))
      # (queue (@reply @replies) (foo 100 ≈1.23))
      # (ws (client (@message -> (local 5000) -> @replies) format: ml)
      #   up)
      #
      # ;; Frame N+4
      #
      # (queue (@message @messages) ())
      # (queue (@reply @replies) (foo 100 ≈1.23 false))
      # (ws (client (@message -> (local 5000) -> @replies) format: ml)
      #   up)
      #
      # ;; Frame N+5
      #
      # (queue (@message @messages) ())
      # (queue (@reply @replies) (foo 100 ≈1.23 false (+ 1 2 x: 100 y: 200)))
      # (ws (client (@message -> (local 5000) -> @replies) format: ml)
      #   up)
      # ```
      matchpi %{[ws [client (@message_ -> _ -> @reply_)] _?]} do
        D7.gnd(node, message, reply)
      end

      # |@ rack.schema
      #
      # |@pattern
      # [schema (@edge_ json) schema_*]
      #
      # |@block
      # EXPERIMENTAL
      matchpi %{[schema (@edge_ json) _*]} do
        D7.gnd(node, edge)
      end

      # |@ rack.supervisor
      #
      # |@pattern
      # [supervisor (@values_ @value_ pattern_ - @pool_) children_*]
      #
      # |@key values rack.edge
      # The edge where the node should search for the cell with a list of values.
      #
      # |@key value rack.edge
      # The edge of the cell containing the item assigned by the supervisor. Each
      # member device is instantiated with such a cell. For example, if the list
      # `(1 2 3)` is at the *values* edge, and *value* is `@x`, the *pool* will
      # contain the following devices:
      #
      # ```wwml
      # (device {}
      #   (cell @x 1)
      #   ...)
      # (device {}
      #   (cell @x 2)
      #   ...)
      # (device {}
      #   (cell @x 3)
      #   ...)
      # ```
      #
      # In the above, `...` contains all of *children*.
      #
      # |@key pattern m1.operator
      # The pattern used to extract the *key* for each item in the *values* list.
      # Items not matching this pattern are skipped. The value associated with
      # the first capture is used as the key; its name is irrelevant.
      #
      # The set of keys determines the population of the pool. For each unique key,
      # a member device is created; when the key disappears, the corresponding device
      # is removed. Keys must uniquely identify items. Otherwise, the item(s) and
      # device(s) are ignored -- the supervisor node is "confused" by them.
      #
      # |@key pool rack.edge
      # The edge of the pool where member devices are maintained.
      #
      # |@key children rack
      # Supplies nodes for member device. It must not contain cells with the edge
      # `@key` and *value*; otherwise you risk a name clash.
      #
      # |@summary
      # Maintains a dynamic population of devices based on a list at a cell.
      #
      # |@block
      # Maintains a population of *member devices* for each keyed item in
      # the list at *values*.
      #
      # "Recruits" a device for each new key and its corresponding item, and
      # places it in the *pool*.
      #
      # When a key is removed, the corresponding device is removed from the *pool*.
      #
      # The key of an item should be a stable part of it. A device's state
      # (i.e., the device itself) is preserved while its key is stable. If
      # keys are unstable, you'll see a lot of "device churn". If keys collide
      # within the same generation, the corresponding device(s) are removed
      # and the keys ignored. If keys collide across generations, the device
      # from the previous generation is reused (along with its state).
      #
      # Devices have *read-only* access to items in the *values* list: they
      # cannot write back. As the item changes, the *value* cell in
      # the corresponding device will be updated.
      #
      # |@example
      # The following circuit:
      #
      # ```wwml
      # (cell @xs ((a 1) (b 2) (c 3)))
      # (supervisor (@xs @x (k_ _) - @pool)
      #   (p "Hello World"))
      # (circuit (pool @pool))
      # ```
      #
      # Populates the pool as follows:
      #
      # ```wwml
      # (cell @xs ((a 1) (b 2) (c 3)))
      # (supervisor (@xs @x (k_ _) - @pool))
      # (circuit (pool @pool)
      #   (device
      #     (cell @x (a 1))
      #     (p "Hello World"))
      #   (device
      #     (cell @x (b 2))
      #     (p "Hello World"))
      #   (device
      #     (cell @x (c 3))
      #     (p "Hello World")))
      # ```
      matchpi %{[supervisor (@values_ @_ _ - @pool_) _*]} do
        # NOTE: the other edge, @values_ ⏏@value_⏏, is an interior edge, it
        # is not exposed to the outside world.
        D7.gnd(node, values, pool)
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
