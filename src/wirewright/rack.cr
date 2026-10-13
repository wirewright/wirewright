# |@ rack
#
# |@summary
# A rewrite regime.
#
# |@block
# Wirewright Rack is a particular instance of symbolic physics. In other words,
# it is a *rewrite regime*, implemented using Wirewright D7. The latter is a kind
# of symbolic physics construction toolkit; a "lego" for constructing rewrite regimes.
# Rack is one such regime.
#
# A symbolic world in Rack is called a *circuit*. A *circuit* consists of nodes.
# Nodes can be *leaves* (such as `rack.cell`), parents (e.g. `rack.group`),
# or both (`rack.circuit`). The reason the name *circuit* is used is purely
# historical. I am actually considering renaming it to *world*. But that's quite
# a huge rename.
#
# Instead of evaluating circuits, Rack & D7 *simulate* their *evolution*, like
# you can simulate the evolution of a cellular automaton such as [Conway's
# Game of Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life).
#
# Rack circuits can contain *subcircuits* (`rack.circuit`, `rack.node`, `rack.device`, etc.)
# Evolution (time) always runs *top-down*: host circuits are always *time-stepped*
# (evolved into the future by one step) **before** subcircuits (guest circuits).
# The reasoning for this is the same as in simulation. We consider guest circuits to
# be *simulated* by their host circuit. As the host circuit must run in order for
# a subcircuit to run, the order of evolution is naturally top-down: the simulated
# time-step of a guest circuit requires the host's time-step.
#
# For a start, consider looking at simpler, less "philosophically heavy" nodes such
# as: `rack.cell`, `rack.feed`, `rack.parser`, `rack.path`.
#
# |@example
# The following is the "Hello World" of Rack -- an oscillator.
#
# ```wwml
# (cell @x 0)
# (cell @y)
# (feed @x @y @x)
# ```
module Ww::Rack
  extend self

  defrecord Cell, node : D7::Node, value : Term?, smart: true

  struct Cell
    def empty? : Bool
      @value.nil?
    end
  end

  # :nodoc:
  SYM_CELL = Term.of(:cell)
  # :nodoc:
  SYM_OUTLET = Term.of(:outlet)
  # :nodoc:
  SYM_FIELD = Term.of(:field)

  def each_cell(hg : D7::Hypergraph, input : D7::AbsEdge, & : Cell ->) : Nil
    candidates = Pf::Kit.stack_array(Cell, 4)

    hg.each_node_with_head(SYM_CELL, SYM_OUTLET, SYM_FIELD, memberof: {input}) do |node|
      dict = node.term.as_d

      # Since `cell?` is called extremely frequently, we cannot afford M1 here, so
      # we hand-write the matching code.

      # At this point we know that [cell _*], and *input* is somewhere in it.
      #
      # We also know that *node* is a Gnd node, which means it was accepted by
      # the classifier.
      #
      # Since cell has only one edge, `memberof:` above already covers the edge check.

      if dict.itemsize == 2
        # [cell fst_]
        # [outlet fst_]
        # [field fst_]
        #
        # *input* is somewhere in *fst* and the classifier accepted it, so we're
        # fine with recognizing the whole thing as an empty cell.
        candidates << Cell.new(node, value: nil)
        next
      end

      if dict.itemsize == 3
        # [cell fst_ snd_]
        # [outlet fst_ snd_]
        # [field fst_ snd_]
        #
        # *input* can be inside *fst* or *snd*, but we know that the classifier is
        # happy, so it must be in *fst*. We're fine with recognizing the whole thing
        # as a nonempty cell, *snd* being its value.
        candidates << Cell.new(node, value: dict[2])
        next
      end
    end

    candidates.each do |candidate|
      yield candidate
    end
  end

  def cell?(hg : D7::Hypergraph, input : D7::AbsEdge) : Cell?
    candidates = Pf::Kit.stack_array(Cell, 1)
    each_cell(hg, input) do |cell|
      candidates << cell
    end
    candidates.single?
  end

  defrecord Pool, node : D7::Node, contents : Term::Dict

  def pool?(hg : D7::Hypergraph, edge : D7::AbsEdge) : Pool?
    return unless cell = Rack.cell?(hg, edge)

    if value = cell.value?
      return unless contents = value.as_d?
    end
    Pool.new(cell.node, contents || Term[])
  end

  # |@ rack.[edge template]
  #
  # |@pattern
  # _dict
  #
  # |@summary
  # A dictionary with zero or more edge references that are subsequently
  # replaced by the values at the corresponding cells.
  #
  # |@block
  # Edge templates are primarily used in `rack.rewriter` and `rack.backsys`.
  # A rewriter or backsys node *instantiates* its template: replaces edges
  # in it with the corresponding values. The instance is then modified or
  # rewritten by the node. Lastly, the node inspects the rewritten instance
  # for updates to cells at the edges, and carries out those updates.
  #
  # Instantiation does not start until all edges in the *itemspart* have
  # a value. In the *pairspart*, edges without a value cause the corresponding
  # entry to be omitted.
  #
  # For example, the template `(@x @y @:a @:b)` will not be instantiated until
  # both `@x` and `@y` have a value. `@a` and `@b` may be empty: in that case
  # their entries are simply not included in the instance.
  #
  # Literals are left as-is. For example, in `(+ @a @b)`, `@a` and `@b` will be
  # replaced with their values when available, and `+` will be kept as is. If we
  # take `(cell @a 1)` and `(cell @b 2)`, then `(+ 1 2)`.
  #
  # In the pairspart, simple pairs confuse the absence of a cell at an edge with
  # its emptiness: both cases "look" the same for a pair such as `@:x` (short for `x: @x`).
  # That is, for `@:x`:
  #
  # - If the cell `@x` is missing, `-x` (x is not included in the instance).
  # - If the cell `@x` is empty, `-x`.
  # - If the cell `@x` has a value, `x: value_`, where *value* is that value.
  #
  # You can use `optional` to distinguish the three states. For example, `x: (optional @x)`
  # distinguishes the three possible states as follows:
  #
  # - If the cell `@x` is missing, `x: none`.
  # - If the cell `@x` is empty, `-x` (x is not included in the instance).
  # - If the cell `@x` has a value, `x: (some value_)`, where *value* is that value.
  #
  # |@example
  # Swap, edges in itemspart:
  #
  # ```wwml
  # (cell @x 100)
  # (cell @y 200)
  # (backsys (@x @y)
  #   (x_ y_) <> {x: ^y, y: ^x})
  # ```
  #
  # Swap, edges in pairspart:
  #
  # ```wwml
  # (cell @x 100)
  # (cell @y 200)
  # (backsys {@:x, @:y}
  #   {¦ x_ y_} <> {x: ^y, y: ^x})
  # ```
  #
  # Swap, edges in both:
  #
  # ```wwml
  # (cell @x 100)
  # (cell @y 200)
  # (backsys (@x @:y)
  #   (x_ ⍊ y_) <> {x: ^y, y: ^x})
  # ```
  #
  # Optional:
  #
  # ```wwml
  # ;; Frame 0 (seed)
  #
  # ;; Activate the @x cell when @y becomes nonempty.
  # (guard (@y _)
  #   (cell @x))
  # (cell @y)
  # (backsys {x: (optional @x), @:y}
  #   ;; @x is missing.
  #   {¦ x: none -y_} <> {y: "Y is nonempty now"}
  #   ;; @x is empty.
  #   {¦ -x_} <> {x: (some 100)}
  #   ;; @x is nonempty, increment the number it stores.
  #   {¦ x: (some n_)} <> {n: ^(+ n 1)})
  #
  # ;; Frame 1
  #
  # (guard (@y _)
  #   (cell @x))
  # (cell @y "Y is nonempty now")
  # ;; (backsys {@:x, @:y} ...)
  #
  # ;; Frame 2
  #
  # (guard (@y _)
  #   (cell @x 100))
  # (cell @y "Y is nonempty now")
  # ;; (backsys {@:x, @:y} ...)
  #
  # ;; Frame 3
  #
  # (guard (@y _)
  #   (cell @x 101))
  # (cell @y "Y is nonempty now")
  # ;; (backsys {@:x, @:y} ...)
  #
  # ;; Frame 4
  #
  # (guard (@y _)
  #   (cell @x 102))
  # (cell @y "Y is nonempty now")
  # ;; (backsys {@:x, @:y} ...)
  #
  # ;; ... and so on.
  # ```
  #
  # Note that you cannot change the itemsize of a template instance during
  # rewriting. Instances with itemsize different from what it was during
  # instantiation are discarded wholesale:
  #
  # ```wwml
  # ;; Frame 0 (seed)
  #
  # (cell @x 1)
  # (cell @y 2)
  # (cell @z 3)
  # (backsys (@x @y @z)
  #   ;; Incrementing *x* is fine, but removing *y* is not!
  #   (x_ y_ _) <> {x: ^(+ x 1), (y): ()})
  #
  # ;; This circuit is quiescent because the backsystem's rewritten instance
  # ;; is discarded.
  # ```
  #
  # You can remove and add pairs. Removals clear the corresponding cell,
  # and additions fill a cell if it was empty.
  #
  # ```wwml
  # ;; Frame 0 (seed)
  #
  # (cell @x)
  # (backsys {@:x}
  #   ;; If x is empty, fill it with `100`.
  #   {¦ -x_} <> {x: 100}
  #   ;; If x is nonempty, clear it.
  #   {¦ x_} <> {(x): ()})
  #
  # ;; Frame 1
  #
  # (cell @x 100)
  # ;; (backsys {@:x} ...)
  #
  # ;; Frame 1
  #
  # (cell @x)
  # ;; (backsys {@:x} ...)
  #
  # ;; Frame 2
  #
  # (cell @x 100)
  # ;; (backsys {@:x} ...)
  #
  # ;; ... and so on
  # ```

  module EdgeTemplateMember
    alias Any = Bare | Optional

    struct Bare
      getter key : Term

      # :nodoc:
      def initialize(@node : D7::Node, @key : Term, @value0 : Term?)
      end

      def patch(value1 : Term?) : D7::Patch
        # (cell @_ value0_) -> (cell @_ value1_)  [value0 : Term, value1 : Term]
        # (cell @_) -> (cell @_ value1_)          [value0 : Nil, value1 : Term]
        # (cell @_ value0_) -> (cell @_)          [value0 : Term, value1 : Nil]
        @value0 == value1 ? D7.patch : D7.patch(@node, {2, value1})
      end
    end

    struct Optional
      getter key : Term

      # :nodoc:
      def initialize(@node : D7::Node, @key : Term, @value0 : Term?)
      end

      def patch(value1 : Term?) : D7::Patch
        if @value0 == value1
          return D7.patch
        end

        if value1.nil? # => value0 != nil
          return D7.patch(@node, {2, nil})
        end

        Term.case(value1) do
          matchpi %{(some rep_)} { D7.patch(@node, {2, rep}) }
          otherwise { D7.patch }
        end
      end
    end
  end

  # NOTE: This function does not implement rollback or transactionality; if it returns
  # `nil`, all block yields prior must be reverted *by the caller*.
  def fill_edge_template?(hg : D7::Hypergraph, addr : D7::NodeAddr, edge_template : Term::Dict, & : EdgeTemplateMember::Any ->) : Term::Dict?
    edge_template.transaction do |commit|
      edge_template.items.each_with_index do |item, index|
        next unless Term.edge?(item)

        abs_target = hg.resolve(addr, item)

        # We can't leave holes in the itemspart, so back off if any cell from
        # there is missing.
        return unless target_cell = Rack.cell?(hg, abs_target)
        return unless target_value = target_cell.value?

        yield EdgeTemplateMember::Bare.new(target_cell.node, Term.of(index), target_value)

        commit.with(index, target_value)
      end

      edge_template.each_entry(in: Term::Dict.pairspart) do |key, value|
        # Fast / typical path.
        if Term.edge?(value)
          abs_target = hg.resolve(addr, value)
          unless target_cell = Rack.cell?(hg, abs_target)
            commit.without(key) # Skip if cell missing
            next
          end

          target_value = target_cell.value?
          # Omit (target_value : Nil) if cell value is missing, but still count as
          # a target cell so that the rewriter can write there if necessary.
          commit.with(key, target_value)

          yield EdgeTemplateMember::Bare.new(target_cell.node, key, target_value)
          next
        end

        Term.case(value) do
          # Cell absence is represented by `key: none`.
          # Cell presence is represented by `key: (some value_)`.
          # Cell emptiness is represented by `-key`.
          matchpi %{(optional @edge_)} do
            abs_target = hg.resolve(addr, edge)
            unless target_cell = Rack.cell?(hg, abs_target)
              commit.with(key, :none)
              next
            end

            if target_value = target_cell.value?
              commit.with(key, {:some, target_value})
            else
              commit.without(key)
            end

            yield EdgeTemplateMember::Optional.new(target_cell.node, key, target_value)
          end

          otherwise do
            # Keep key: value as is. For example:
            #   {⏏foo: 100⏏, @:x}
            # ... with `(cell @x 100)` should give:
            #   {foo: 100, x: 100}
            # ... where `foo` is a completely passive, inert entry.
          end
        end
      end
    end
  end
end

require "./rack/classifier"
require "./rack/feed"
require "./rack/part"
require "./rack/form"
require "./rack/changes"
require "./rack/tspace"
require "./rack/assembler"
require "./rack/backsys"
require "./rack/rewriter"
require "./rack/parser"
require "./rack/extrinsics"
require "./rack/fs"
require "./rack/schema"
require "./rack/format"
require "./rack/database"
require "./rack/accord"
require "./rack/supervisor"
require "./rack/timekeeper"
require "./rack/misc"
require "./rack/automaton"
