module Ww::Rack
  # :nodoc:
  def classify!(node : Term) : D7::Feature
    M1::PatternSet.case(node) do
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
      # ;; Step 0
      # (cell @a 0)
      # (cell @b)
      # (cell @c)
      # (feed @a @b @c @a)
      #
      # ;; Step 1
      # (cell @a)
      # (cell @b 0)
      # (cell @c)
      # (feed @a @b @c @a)
      #
      # ;; Step 2
      # (cell @a)
      # (cell @b)
      # (cell @c 0)
      # (feed @a @b @c @a)
      #
      # ;; Step 3
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

      # |@ rack.backref
      #
      # |@pattern
      # [backref (@edge_ patterns_*) child_]
      #
      # |@key edge rack.edge
      # The edge which which the backreference should watch.
      #
      # |@key patterns m1.operator
      # A list of alternative patterns. The first pattern that matches *child* is
      # used in a backmap. The backmap plugs the value of *edge* where the same-
      # named capture tells it to.
      #
      # |@block
      # Backrefs are like `rack.node`s with a pattern, except there can be zero or more
      # alternative *patterns*, and also backrefs aren't cells. The value at *edge* is
      # backmapped into *child* during evaluation.
      #
      # |@example
      # ```wwml
      # (cell @xs (1 2 3))
      #
      # ;; Increment the middle number in @xs.
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      #
      # ;; Bind the value of @x to {count: _} in @ys using the backref node.
      # (part (@xs @mid) (_ mid_ _))
      # (backref (@mid (cell @ys {| -count: mid}) (cell @ys {count: mid_}))
      #   (cell @ys))
      # ```
      #
      # The circuit above evolves as follows:
      #
      # ```wwml
      # ;; Step 1
      # (cell @xs (1 3 3))
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      # (part (@xs @mid) (_ mid_ _))
      # (backref (@mid (cell @ys {| -count: mid}) (cell @ys {count: mid_}))
      #   (cell @ys {count: 3}))
      #
      # ;; Step 2
      # (cell @xs (1 4 3))
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      # (part (@xs @mid) (_ mid_ _))
      # (backref (@mid (cell @ys {| -count: mid}) (cell @ys {count: mid_}))
      #   (cell @ys {count: 4}))
      #
      # ;; Step 3
      # (cell @xs (1 5 3))
      # (backsys @xs
      #   (_ ±n _) <> {n: ^(+ n 1)})
      # (part (@xs @mid) (_ mid_ _))
      # (backref (@mid (cell @ys {| -count: mid}) (cell @ys {count: mid_}))
      #   (cell @ys {count: 5}))
      #
      # ;; ...etc.
      # ```
      matchpi %{[backref (@edge_ _*) _]} do
        leaf = D7.gnd(node, edge)
        D7.circuit(node.as_d, 2u32...3u32, leaf)
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

      # |@ rack.pool
      #
      # |@pattern
      # [pool @edge_ value_]
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

      matchpi %{[frag @edge_]} do
        D7.mixture(node, Term.of(:cell, edge)) do |view|
          Term.of_case(view) do
            matchpi %{(cell @_)} { node }
            matchpi %{(cell @_ value1_)} { Term.morph(node, {2, value1}) }
          end
        end
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
      # ;; Frame 0
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
      # [rewriter (spec_ <-> @edge_) grammar_*]
      #
      # |@key spec rho
      matchpi %{[rewriter (spec_ <-> @edge_) body_*]} do
        edges = [edge]
        D7.gnd(node, edges, defn: Term.of(:rewriter, spec, edge, body))
      end

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (@spec_ <-> ((%group itemsrcs_ (%past @_ min: 0)) ¦ pairsrcs_)) data_*]
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

      # |@ rack.rewriter
      #
      # |@pattern
      # [rewriter (spec_ <-> srcs_dict) data_*]
      #
      # |@key spec rho
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
      # Lets you read, write, and observe file system entries, live.

      # |@ rack.path
      #
      # |@pattern
      # [path (path_string reading)]
      # [path (path_string reading) reading_]
      #
      # |@key path
      # Specifies the path to a file, for example, `/tmp/test.txt`.
      #
      # |@key reading path.reading
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
      #   (absent "file does not exist")) ;; or something like that
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
      # |@key report path.report
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
      # ;; Frame 0
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
      # ;; Frame 4
      # (path ("/tmp/dir" report)
      #   (dir timestamp: "2026-07-28 18:01:62 UTC" ;; timestamp changed!
      #     (dir "a")
      #     (file "b.txt")
      #     (file "c.txt")))
      # ```
      #
      # ### Recursive watching
      # Currently, it is possible to watch things recursively, but perhaps in
      # a somewhat baroque way. This way also illustrates the philosophy behind
      # Wirewright. Basically, below, we create a *rewrite environment*, a kind
      # of "bubble" with the following "laws of physics":
      #
      # - Absent path reports disapper.
      # - Present path reports mark themselves as handled and spawn child
      #   reports. The "laws" are then applied to the child reports, and so on,
      #   which achieves the "recursion".
      #
      # ```wwml
      # (circuit @reports
      #   (path ("/tmp/dir" report) root: true))
      #
      # (rewriter ((scanR (rulesetR)) <-> @reports)
      #   ;; Remove reports that are absent, except the root report.
      #   R←(path (_ report) [absent _] ⍊ -root)
      #     <> {(R): ()}
      #   ;; Spawn child reports.
      #   R←(path (path_string report) [dir children_*] ⍊ -handled_)
      #     <> {(R):
      #           (^(up R) ;; < report with handled: true
      #            (^each (children as [_ member_string])
      #              (path (^"⸢path⸣/⸢member⸣" report)))),
      #         handled: true})
      # ```
      #
      # You are supposed to then observe the evolution of `reports`, find reports
      # for paths you're interested in, etc. Alternatively, it is possible to spawn
      # not reports but modules containing internal logic, perhaps using `rack.sensor`s
      # and `rack.appearance`s to communicate in a distributed manner. The `rack.ensemble`
      # node is useful for such cases. For example, the program below will show paths
      # that appear and disappear:
      #
      # ```wwml
      # (circuit @reports
      #   (path ("/tmp/dir" report) root: true))
      #
      # (rewriter ((scanR (rulesetR)) <-> @reports)
      #   ;; Remove reports that are absent, except the root report.
      #   R←(path (_ report) [absent _] ⍊ -root)
      #     <> {(R): ()}
      #   ;; Spawn child reports.
      #   R←[path (path_string report) (dir children_* ⍊ -handled_)]
      #     <> {(R):
      #           (^(up R) ;; < report with handled: true
      #            (^each (children as [_ member_string])
      #              (path (^"⸢path⸣/⸢member⸣" report)))),
      #         handled: true})
      #
      # (ensemble (@reports @report [path (path_string report) _] - @pool)
      #   (backref (@key (appearance paths `key) (appearance paths key_))
      #     (appearance paths)))
      #
      # (circuit (pool @pool))
      #
      # (sensor (journal paths _string))
      # ```
      matchpi %{[path (_string report)]}, %{[path (_string report) _]} do
        D7.gnd(node)
      end

      # |@ rack.path
      #
      # |@pattern
      # [path (path_string sink) goal←(present content_string)]
      # [path (path_string sink) goal←(present content_blob)]
      # [path (path_string sink) goal←absent]
      # [path (path_string sink) [err detail_string]]
      #
      # |@key path
      # Specifies the path to a file, for example, `/tmp/test.txt`.
      #
      # |@key goal
      # The desired goal state of the file, either `present` or `absent`.
      #
      # |@key content
      # For `present` goals, specifies the desired content of the file.
      # It can be either a string (writes UTF-8) or a blob (writes an opaque
      # stream of bytes).
      #
      # |@key detail
      # In case of an error (for example, file does not exist during removal
      # or write was denied), the goal is replaced by `[err detail_string]`,
      # where *detail* should tell the reason for failure.
      #
      # |@block
      # The `path...sink` node allows you to (over)write files and remove files.
      #
      # |@example
      # Assuming `/tmp/test.txt` does not exist, we can create it by:
      #
      # ```wwml
      # (path ("/tmp/test.txt" sink) (present "Kaixo mundua"))
      # ```
      #
      # The above evolves into:
      #
      # ```wwml
      # (path ("/tmp/test.txt" sink))
      # ```
      #
      # That the `path` "consumed" the goal means it completed successfully:
      #
      # ```text
      # $ cat /tmp/test.txt
      # Kaixo mundua
      # ```
      matchpi %{[path (_string sink) _]} do
        D7.gnd(node)
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
      # |@pattern
      # [ws (@pool_ binding_ server _?) _*]
      #
      # |@key binding rack.ws.binding
      # Specifies where to bind the server.
      matchpi %{[ws (@pool_ _ server _?) _*]} do
        D7.gnd(node, pool)
      end

      # |@ rack.ensemble
      #
      # |@pattern
      # [ensemble (@values_ @value_ pattern_ - @pool_) children_*]
      #
      # |@key values rack.edge
      # The edge where the node should search for a list of values.
      #
      # |@key value rack.edge
      # Each member module of the ensemble contains a cell with this edge. The cell's
      # value is the module's item. For example, if `(1 2 3)` is at the *values* edge
      # `@xs`, and *value* is at `@x`, the *pool* will contain the following modules:
      #
      # ```wwml
      # (module {}
      #   (cell @x 1)
      #   ...)
      # (module {}
      #   (cell @x 2)
      #   ...)
      # (module {}
      #   (cell @x 3)
      #   ...)
      # ```
      #
      # In the above, `...` contains all of *children*.
      #
      # |@key pattern m1.operator
      # The pattern used to extract the *key* from each item in the *values* list.
      # The value associated with the first capture is used. Its name is irrelevant.
      # The set of keys determines the population of the pool. For each unique key,
      # a member module is created; when a key disappears, the corresponding module
      # is removed. For tracking purposes each `module` contains an additional
      # `(cell @key _?)`. Keys must uniquely identify items. Otherwise, the item(s)
      # and key(s) are ignored -- the ensemble node is "confused".
      #
      # |@key pool rack.edge
      # The edge of the pool where active member modules are stored.
      #
      # |@key children rack
      # Supplies nodes for member module. It must not contain cells with the edge
      # `@key` and *value*; otherwise you risk a name clash.
      #
      # |@summary
      # Maps items of a list to modules.
      #
      # |@block
      # Maintains a population of *member modules* for each keyed item in
      # the list referred to by *values*. "Recruits" a module for each new
      # key and its corresponding item and places it in the *pool*. When
      # the key is removed, the corresponding module is removed from the *pool*.
      # The key of an item can be a stable part of it. In that case the module's
      # state is preserved while the key is stable. Importantly, modules currently
      # have *read-only* access to items in the *values* list: they cannot write
      # back. As the item changes, the *value* cell in the corresponding module
      # will be updated. *Write* access to allow bidirectionality is a TODO. It
      # is difficult to implement because it is very conflict-prone.
      #
      # |@example
      # The following circuit:
      #
      # ```wwml
      # (cell @xs ((a 1) (b 2) (c 3)))
      # (ensemble (@xs @x (k_ _) - @pool)
      #   (p "Hello World"))
      # (circuit (pool @pool))
      # ```
      #
      # Populates the pool as follows:
      #
      # ```wwml
      # (cell @xs ((a 1) (b 2) (c 3)))
      # (ensemble (@xs @x (k_ _) - @pool))
      # (circuit (pool @pool)
      #   (module {}
      #     (cell @key a)
      #     (cell @x (a 1))
      #     (p "Hello World"))
      #   (module {}
      #     (cell @key b)
      #     (cell @x (b 2))
      #     (p "Hello World"))
      #   (module {}
      #     (cell @key c)
      #     (cell @x (c 3))
      #     (p "Hello World")))
      # ```
      #
      # Notice how the `@key` cell was created automatically.
      matchpi %{[ensemble (@values_ @_ _ - @pool_) _*]} do
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
