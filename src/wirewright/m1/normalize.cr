module Ww::M1
  # :nodoc:
  NORMAL_PASS = Term.of_dict(:"%pass")

  # :nodoc:
  NORMAL_BLANK_DICT = Term.of_dict(:"%dict")
  # :nodoc:
  NORMAL_BLANK_NUMBER = Term.of_dict(:"%number", :_, depth: 0)
  # :nodoc:
  NORMAL_BLANK_STRING = Term.of_dict(:"%string", depth: 0)
  # :nodoc:
  NORMAL_BLANK_SYMBOL = Term.of(:"%symbol", depth: 0)
  # :nodoc:
  NORMAL_BLANK_BOOLEAN = Term.of(:"%boolean", depth: 0)

  # :nodoc:
  NORMAL_EDGE_ANY = Term.of(:"%edge", :_, depth: {:+, :envelope, 1})
  # :nodoc:
  NORMAL_EDGE_SYMBOL = Term.of(:"%edge", :_symbol, depth: 1)
  # :nodoc:
  NORMAL_EDGE_STRING = Term.of(:"%edge", :_string, depth: 1)
  # :nodoc:
  NORMAL_EDGE_NUMBER = Term.of(:"%edge", :_number, depth: 1)
  # :nodoc:
  NORMAL_EDGE_DICT = Term.of(:"%edge", :_dict, depth: {:+, :envelope, 1})
  # :nodoc:
  NORMAL_EDGE_BOOLEAN = Term.of(:"%edge", :_boolean, depth: 1)

  # :nodoc:
  STRING_NONEMPTY = Term.of(:"%all", {:"%not", ""}, :_string)

  # :nodoc:
  #
  # Auxiliary functions for `normalize`. Since Crystal doesn't support nested defs,
  # we have to do this nonsense. These are conceptually scoped under `normalize`.
  module Normalize
    extend self

    # Returns `true` if *term* could possibly be an operator name.
    def probably_operator?(term : Term) : Bool
      return false unless sym = term.as_sym?

      probably_operator?(sym)
    end

    # :ditto:
    def probably_operator?(sym : Term::Sym) : Bool
      sym.prefixed_by?('%') || sym == SYM_EDGE
    end

    # Returns `true` if *term* is a literal term (either a non-dict literal term
    # such as `100` or a dict literal such as `(+ 1 2)`).
    #
    # This function may return false negatives (says it's not a literal but it
    # actually is); but never false positives (says it's a literal but it's not).
    def literal?(term : Term) : Bool
      if dict = term.as_d?
        return literal?(dict)
      end

      if sym = term.as_sym?
        return !(sym.blank? || probably_operator?(sym))
      end

      true # number, string, etc.
    end

    # :ditto:
    def literal?(dict : Term::Dict) : Bool
      dict.ee.all? { |_, value| literal?(value) }
    end

    # Marks a normal *op* as terminal.
    def terminal(op : Term, **kwargs) : Term
      op.as_d do |dict|
        dict.transaction do |commit|
          {terminal: true}.merge(kwargs).each do |key, value|
            commit.with(key, value)
          end
        end
      end
    end

    # Marks a normal *op* as sealed.
    def sealed(op : Term) : Term
      op.as_d(&.with(:sealed, true))
    end

    # Normalizes *prod* and marks it as sealed.
    def sealed(prod) : Term
      sealed(M1.normalize(prod))
    end

    # Constructs a literal operator for *term*.
    def literal(term : Term) : Term
      if dict = term.as_d?
        return terminal(Term.of(:"%literal", term,
          depth: dict.depth,
          bounds: dict.size,
        ))
      end

      terminal(Term.of(:"%literal", term, depth: 0, bounds: 0))
    end

    def singular(successor : Term) : Term
      Term.of(:"%singular", successor,
        seq: true,
        depth: :envelope,
        bounds: 1,
      )
    end
  end

  # :nodoc:
  def normalize(prod : Π::Item) : Term
    item = prod.item

    Term.case(item, engine: M0) do
      # |@ m1.operator.seq.polyblank
      #
      # |@pattern
      # %'_*
      # %'_number*
      # %'_string*
      # %'_symbol*
      # %'_boolean*
      # %'_dict*
      # %'_+
      # %'_number+
      # %'_string+
      # %'_symbol+
      # %'_boolean+
      # %'_dict+
      # _symbol
      #
      # |@block
      # A polyblank provides a shorthand syntax for matching *zero or more* (`_*`) or
      # *one or more* (`_+`) items.
      #
      # - `_*` polyblanks are a shorthand for `(%plural <name, if any> min: 0 type: <type, if any>)`
      # - `_+` polyblanks are a shorthand for `(%plural <name, if any> min: 1 type: <type, if any>)`
      #
      # Refer to `m1.operator.blank` for general info.
      # Refer to `m1.operator.plural` for semantics and examples.
      matchpi %{_symbol} do
        sym = item.as_sym
        continue unless blank = sym.blank?
        continue unless blank.plural?

        # _* -- name absent
        # xs_* -- name present
        expansion = Term.of(:"%plural", blank.name?,
          type: blank.type.blank,
          min: blank.mult.one_or_more? ? 1 : 0,
        )

        normalize(Π.item(expansion))
      end

      # |@ m1.operator.seq.singular
      #
      # |@pattern
      # _
      #
      # |@block
      # Anything that is not an item sequence operator is treated as a *singular*.
      # Singulars are interpreted as operators (`m1.operator`).
      matchpi %{_symbol}, %{_number}, %{_string}, %{_boolean} do
        Normalize.singular(normalize(Π.pattern(item)))
      end

      # |@ m1.operator.seq.slot
      #
      # |@pattern
      # (%'%slot name_)
      #
      # |@block
      # Slots are used to mark insertion points for the backmap engine. They are
      # skipped during matching.
      #
      # ```
      # (queue (_* `dst) src←(pending x_)) <> {(src): (), dst: ^x}
      #
      # (queue () (pending 100)) ;; => (queue (100))
      # (queue (100 200 300) (pending 400)) ;; => (queue (100 200 300 400))
      # ```
      #
      # NOTE: In WwML, writing `` `foo `` is the same as writing `(%slot foo)`.
      matchpi %{(%'%slot name_)}, cue: :"%slot" do
        Term.of(:"%slot", {:"%ref", name},
          seq: true,
          depth: 0,
          bounds: 0,
          passable: true,
        )
      end

      # |@ m1.operator.seq.plural
      #
      # |@pattern
      # (%'%plural ¦ min⋮ 0 max_⋮ ∞ type⋮ _) (%'%plural name_ ¦ min⋮ 0 max_⋮ ∞ type⋮ _)
      #
      # |@key name
      # Capture name (if any).
      #
      # |@key type
      # Typed, unnamed blank representing the type (see `m1.operator.blank` for general
      # info). Use `_` to allow any type.
      #
      # |@key max
      # Infinity is shown for reference only. In practice, `max` only
      # accepts numbers.
      #
      # |@block
      # Smart plural: consumes at least *min* items, at most *max* items
      # (inclusive) of the given *type*; adapting to the surrounding context
      # as appropriate.
      #
      # ### Polyblanks
      #
      # Polyblanks in item sequences are shorthands for `%plural`: `xs_*` is the
      # same as `(%plural xs)`, `xs_+` is the same as `(%plural xs min: 1)`;
      # something like `xs_number+` is the same as `(%plural xs min: 1 type:
      # _number)`. For nameless polyblanks, the name is omitted, e.g. `_number*`
      # becomes `(%plural type: _number)`.
      #
      # ### Essentials
      #
      # Smart plurals are the main components of *shapes*.
      #
      # Each shape consists of *rigid* and *flexible* parts. Singulars
      # (`m1.operator.seq.singular`, e.g. `+`, `100`, `a_`) are rigid.
      # Everything else is flex.
      #
      # Rigids stick together to form longer rigids. For example, in
      # `(+ a_ b_)`, what starts as three rigids ends up being just one rigid,
      # consisting, in order, of the units `+`, `a_`, and `b_`.
      #
      # Smart plurals and smart gaps `(%gap ⏏)` are considered *distributive*:
      # when adjacent to each other, they share space evenly among each other.
      # Adjacent distributives group, and become, collectively, one flex unit.
      # For example, the three distributives in `(xs_* ys_* zs_*)` form a group,
      # and become one flex unit -- one flex.
      #
      # All other sequence operators are *non-distributive*: e.g., `%gap/min`,
      # `%past/max`, `%plural/min`, etc. They do not group with distributives,
      # but they are still flexes. Distributive groups, as I said above, are
      # also flexes.
      #
      # Flexes stick together to form longer flexes.
      #
      # For example, in `(xs_* ys_* zs_* (%past/max _))`, what starts as three
      # distributives `xs_*`, `ys_*`, and `zs_*`, ends up being just one
      # distributive group, consisting, in order, of the units `xs_*`, `ys_*`,
      # and `zs_*`. The distributive group and `%past/max`, both flexes, stick
      # together to form one long flex.
      #
      # Non-distributive and distributive flexes will compete over space.
      # Competition here means, basically, backtracking search. Distributive
      # flexes will divide it evenly, without any kind of search. Notably, when
      # competing with non-distributive flexes, distributive flexes are
      # *greedy*: i.e., the group of distributed flexes behaves like `%past/max`
      # or `%plural/max`, in that it will try to consume as many items as
      # possible, to then divide them evenly among distributive units in the
      # group. Thus, patterns like `(xs_* ys_* (%plural/max zs type: _number))`
      # are underconstrained: the distributive group `xs_* ys_*` will take all
      # items for itself and leave none to `(%plural/max zs type: _number)`, so
      # *zs* will always be empty `()`. Adding constraints (e.g. `min`
      # constraint) helps. You can also use `%gap/min`, `%gap/max` wrapped in
      # e.g. `(%group zs_ ⏏)` to provide more complex numeric constraints on the
      # size of *zs*; or specify the constraints on the group's successor itself
      # (as in, e.g., `(%group zs←(%pipe tally 4) (%plural/max type:
      # _number))`).
      #
      # ### Shapes
      #
      # So, after all reductions, sequence patterns become an alternating
      # sequence of flexes and rigids. There are several kinds of shapes M1
      # recognizes:
      #
      # - `rigid-flex`: also known as *prefix* (we're rigid-centric), and
      #   internally as *padded-right*. Consider, for instance, `(x_ xs_*)`,
      #   `(+ x_ xs_*)` etc.
      # - `flex-rigid`: also known as *suffix*, and internally as *padded-left*.
      #   Consider, for instance, `(xs_* x_)`, `(xs_* x_ y_)` etc.
      # - `rigid-flex-rigid`: also known as *midsection*, and internally as
      #   *midgap*. Consider, for instance, `(a_ mid_* b_)`.
      # - `flex-rigid-flex`: also known as *circumfix*, and internally as *padded*.
      #   Consider, for instance, `(l_* m_ r_*)`.
      #
      # These can compose: for example, midsection composes with suffix on the
      # left and prefix on the right, i.e., `flex-rigid-flex-rigid-flex`.
      #
      # Consider, for instance, the pattern `(l_* m0_ gap_* m1_ r_*)`. This form
      # is a special case and is known as *padded midgap* internally.
      #
      # Or, say, `(+ l_* a_ gap_* b_)`, which reads as
      # `rigid-flex-rigid-flex-rigid`: the first part, `+ l_*`, is a prefix,
      # `gap_* b_` is a suffix, and `a_` is the middle part of a *circumfix*.
      #
      # ### Distribution of space
      #
      # The left-hand side of a *prefix* is rigid. Thus, its size is always
      # known in advance. That number of items is taken from the beginning of
      # the list of available items. The rest of items are passed, recursively,
      # to the prefix's right-hand side flex.
      #
      # Symmetrically for *suffix*.
      #
      # *Midsection* knows the size of its left and right parts, and gives that
      # number of items to each from left and right, correspondingly. The
      # remaining middle items are distributed recursively by the midsection's
      # middle flex.
      #
      # *Circumfix* knows the size of its middle rigid. It passes the center N
      # items, where N is the size of the middle rigid, to the rigid. The
      # remaining items on the left- and right-hand sides are passed to the
      # flexes on both sides.
      #
      # *Padded midgap* divides the items into three parts. The two rigids are
      # taken off of the middle (so conceptually, in `l_* a_ m_* b_ r_*`, `a_
      # m_* b_` group together, as in, `a_` is subtracted from `m`'s left, and
      # `b_` from its right). Spare items are distributed evenly between `l`,
      # `m`, and `r`, leaning left (so `l` gets one item first, then `m`, then
      # `r`, then `l` again and so on).
      #
      # ### What all this means in practice
      #
      # Smart plurals, smart gaps, and this whole arrangement with shapes, exists
      # fundamentally to allow you to match *spatially* rather than *structurally*.
      # The notions of left half, right half, center, and so on all matter in spatial,
      # not structural matching (although to be precise, you can say spatial matching
      # is a subset of structural matching!)
      #
      # Consider, for instance, a task where you want to divide a list into two
      # halves: left and right. You can do so simply with `(l_* r_*)`. Three halves?
      # `(l_* m_* r_*)`. Three halves, at least one value in the middle half? Simply
      # `(l_* m_+ r_*)`. Or, say, you want to take the middle element: `(_* m_ _*)`.
      #
      # This works for odd-sized lists, but for even-sized lists, it'll take the second
      # middle (the right one). Thus, in `l_* m_ r_*`, `l` will have more items than `r`.
      # Let's say you want to reject instead: `((%gap n_) m_ (%gap n_))` does the job. In
      # case you want to capture the left and right halves, you can surround them with `%group`:
      # `(%group l_ (%gap n_)) m_ (%group r_ (%gap n_)))`. This *is* more verbose, yes; but
      # what this pattern means is not in the gamut of "often needed", and so, trade-offs
      # are made against it.

      # |@ m1.operator.seq.plural/min
      #
      # |@pattern
      # (%'%plural/min ¦ min⋮ 0 max_⋮ ∞ type⋮ _)
      # (%'%plural/min name_ ¦ min⋮ 0 max_⋮ ∞ type⋮ _)
      #
      # |@key name
      # Capture name (if any).
      #
      # |@key type
      # Typed, unnamed blank representing the type (see `m1.operator.blank`
      # for general info). Use `_` to allow any type.
      #
      # |@key max
      # Infinity is shown for reference only. In practice, `max` only accepts numbers.
      #
      # |@block
      # Lazy plural: consumes at least *min* items, at most *max* items (inclusive)
      # of the given *type*; consuming as few items as possible.
      #
      # ```
      # ((%plural/min xs min: 1) ys_*) => (^xs ^ys)
      #
      # (a b c d) ;; => ((a) (b c d))
      # ```

      # |@ m1.operator.seq.plural/max
      #
      # |@pattern
      # (%'%plural/max ¦ min⋮ 0 max_⋮ ∞ type⋮ _)
      # (%'%plural/max name_ ¦ min⋮ 0 max_⋮ ∞ type⋮ _)
      #
      # |@key name
      # Capture name (if any).
      #
      # |@key type
      # Typed, unnamed blank representing the type (see `m1.operator.blank`
      # for general info). Use `_` to allow any type.
      #
      # |@key max
      # Infinity is shown for reference only. In practice, `max` only accepts numbers.
      #
      # |@block
      # Greedy (eager) plural: consumes at least *min* items, at most *max* items
      # (inclusive) of the given *type*; consuming as many items as possible.
      #
      # ```
      # ((%plural/max xs min: 1) ys_*) => (^xs ^ys)
      #
      # (a b c d) ;; => ((a b c d) ())
      #
      # ;; Or, say, if we add a min constraint on *ys*:
      # ((%plural/max xs min: 1) (%plural ys min: 1)) => (^xs ^ys)
      #
      # (a b c d) ;; => ((a b c) (d))
      # ```

      matchpi(
        %{(head←%'%plural ¦ opts_)},
        %{(head←%'%plural/min ¦ opts_)},
        %{(head←%'%plural/max ¦ opts_)},
        %{(head←%'%plural _ ¦ opts_)},
        %{(head←%'%plural/min _ ¦ opts_)},
        %{(head←%'%plural/max _ ¦ opts_)},
        cues: {:"%plural", :"%plural/min", :"%plural/max",
               :"%plural", :"%plural/min", :"%plural/max"},
      ) do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 0u32)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          type = s.key(:type, value: {:_, :_number, :_string, :_symbol, :_dict}, default: Term.of(:_))
          continue if max.is_a?(UInt32) && min > max

          normal = opts.transaction do |commit|
            commit << head
            if name = item[1]?
              commit << {:"%capture", name}
            end

            commit.with(:seq, true)
            unless type.in?(Term.of(:_), Term.of(:_dict))
              commit.with(:depth, 0)
            end
            commit.with(:bounds, {min, :"..=", max})
          end

          Term.of(normal)
        end
      end

      # |@ m1.operator.seq.gap
      #
      # |@pattern
      # (%'%gap measurer_)
      # (%'%gap/min measurer_)
      # (%'%gap/max measurer_)
      #
      # |@key measurer m1.operator
      # An operator that is excited by matching lengths.
      #
      # |@block
      # Expands using a chosen strategy until the length excites *measurer*.
      #
      # - `%gap` is smart/structural (as `m1.operator.seq.plural`).
      # - `%gap/min` expands using the lazy strategy (as `m1.operator.seq.plural/min`).
      # - `%gap/max` expands using the greedy strategy (as `m1.operator.seq.plural/max`).

      # |@ m1.operator.seq.gap°
      #
      # |@pattern
      # (%'%gap/min° measurer_)
      # (%'%gap/max° measurer_)
      #
      # |@key measurer m1.operator
      # An operator that is excited by matching lengths.
      #
      # |@block
      # Expands using a chosen strategy until the length excites *measurer* . Forms
      # a source by continuing to expand until all lengths are exhausted.
      #
      # - `%gap/min°` expands using the lazy strategy (as `m1.operator.seq.plural/min`).
      # - `%gap/max°` expands using the greedy strategy (as `m1.operator.seq.plural/max`).

      matchpi(
        %{(head←%'%gap measurer_)},
        %{(head←%'%gap/min measurer_)},
        %{(head←%'%gap/max measurer_)},
        %{(head←%'%gap/min° measurer_)},
        %{(head←%'%gap/max° measurer_)},
        cues: {:"%gap", :"%gap/min", :"%gap/max",
               :"%gap/min°", :"%gap/max°"}
      ) do
        Term.of(head, Normalize.sealed(Π.pattern(measurer)), seq: true)
      end

      # |@ m1.operator.seq.optional
      #
      # |@pattern
      # (%'%optional default_ successor_)
      #
      # |@key default
      # A literal to pass to *successor* if the item is missing.
      #
      # |@block
      # Expects an item that excites *successor*. If none, uses *default* to
      # excite *successor*.
      #
      # ```
      # (parse (%optional "John Doe" name_string) age_number) => (^name ^age)
      #
      # (parse 24) ;; => ("John Doe" 24)
      # (parse "Samantha Doe" 30) ;; => ("Samantha Doe" 30)
      # ```
      matchpi %{(%'%optional default_ successor_)}, cue: :"%optional" do
        Term.of(:"%optional", {:"%payload", default}, Normalize.sealed(Π.pattern(successor)),
          seq: true,
          bounds: {0, :"..=", 1},
          depth: {:max, {:members, 0, :"..<", 1}, :max},
        )
      end

      # |@ m1.operator.seq.group
      #
      # |@pattern
      # (%'%group successor_ members_+)
      #
      # |@key successor m1.operator
      # An operator that should receive the resulting item sequence.
      #
      # |@key members m1.operator.seq
      # One or more member item sequence operators.
      #
      # |@block
      # Allows you to match on the sequence of items covered by *members*.
      #
      # ```
      # (match (%group (first_ _* last_) l_* r_*)) => (^first ^last ^l ^r)
      #
      # (match a b c d e f)
      # ;; => (a f (a b c) (d e f))
      # ```
      matchpi %{(%'%group successor_ _ _*)}, cue: :"%group" do
        members = item.items.move(2)

        normal = Term::Dict.build do |commit|
          commit << :"%group" << Normalize.sealed(Π.pattern(successor))
          commit.concat(members) { |member| normalize(Π.item(member)) }

          commit.with(:seq, true)
          commit.with(:depth, :envelope)
          commit.with(:bounds, :sum)
          commit.with(:passable, true)
        end

        Term.of(normal)
      end

      # |@ m1.operator.seq.many
      #
      # |@pattern
      # (%many successor_ members_+ ¦ min⋮ 1 max_⋮ ∞)
      #
      # |@key successor m1.operator
      # An operator that should receive the list of match envs after matching *members*.
      #
      # |@key members m1.operator.seq
      # One or more member item sequence operators. They are matched in a shared,
      # *isolated* match envs (so e.g. they can't make constraints with the rest
      # of the pattern but can make constraints among themselves).
      #
      # |@key max
      # Infinity is shown for reference only. `max` only accepts numbers.
      #
      # |@block
      # Allows you to match at least *min*, at most *max* (inclusive) instances of item
      # sequence operators in isolated *members*, matching as many instances as
      # possible (eager). The resulting match envs (one for each instance) are
      # passed to *successor*.
      matchpi %{(%'%many successor_ _ _* ¦ opts_)}, cue: :"%many" do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 1u32)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          continue if max.is_a?(UInt32) && min > max

          members = item.items.move(2)

          normal = opts.transaction do |commit|
            commit << :"%many/max" << Normalize.sealed(Π.pattern(successor))

            commit.with(:seq, true)
            if min > 0
              commit.concat(members) { |member| normalize(Π.item(member)) }
              commit.with(:depth, :envelope)
              commit.with(:bounds, {:*, :sum, {min, :"..=", max}})
              commit.with(:passable, true)
            else # min == 0
              commit.concat(members) { |member| Normalize.sealed(Π.item(member)) }
              # 0 is *successor*, we want to skip it; hence `1`. members do not include
              # successor, hence `1 + _`.
              commit.with(:depth, {:max, {:members, 1, :"..<", 1 + members.size}, :max})
              commit.with(:bounds, {:*, {:sum, {:members, 1, :"..<", 1 + members.size}, :max}, {0, :"..=", max}})
            end
          end

          Term.of(normal)
        end
      end

      # |@ m1.operator.seq.past
      #
      # |@pattern
      # (%'%past members_+ ¦ min⋮ 0 max_⋮ ∞)
      # (%'%past/max members_+ ¦ min⋮ 0 max_⋮ ∞)
      #
      # |@key members m1.operator.seq
      #
      # |@key max
      # Infinity is shown for reference only. `max` only accepts numbers.
      #
      # |@block
      # Repeats *members* at least *min*, at most *max* times (inclusive) using
      # a chosen strategy.
      #
      # - `%past` is lazy (see also: `m1.operator.seq.plural/min`).
      # - `%past/max` is greedy (see also: `m1.operator.seq.plural/max`).
      #
      # All members are matched in the current env, so they will constrain each other:
      #
      # ```
      # (parse ((%past x_))) => (^x)
      #
      # (parse ())      ;; => () [i.e., there's x]
      # (parse (1))     ;; => (1)
      # (parse (1 2 3)) ;; mismatch
      # (parse (1 1 1)) ;; => (1)
      # ```
      #
      # One useful idiom is `(%past _ min: 0 max: 1)`, for which WwML has the shorthand `_?`.
      matchpi(
        %{(head←%'%past _ _* ¦ opts_)},
        %{(head←%'%past/max _ _* ¦ opts_)},
        cues: {:"%past", :"%past/max"},
      ) do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 0u32)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          continue if max.is_a?(UInt32) && min > max

          members = item.items.move(1)

          normal = opts.transaction do |commit|
            case head
            when Term.of(:"%past")     then commit << Term.of(:"%past/min")
            when Term.of(:"%past/max") then commit << Term.of(:"%past/max")
            else
              unreachable
            end

            commit.with(:seq, true)
            if min > 0
              commit.concat(members) { |member| normalize(Π.item(member)) }
              commit.with(:depth, :envelope)
              commit.with(:bounds, {:*, :sum, {min, :"..=", max}})
              commit.with(:passable, true)
            else # min == 0
              commit.concat(members) { |member| Normalize.sealed(Π.item(member)) }
              commit.with(:depth, {:max, {:members, 0, :"..<", members.size}, :max})
              commit.with(:bounds, {:*, {:sum, {:members, 0, :"..<", members.size}, :max}, {0, :"..=", max}})
            end
          end

          Term.of(normal)
        end
      end

      matchpi %{(%'%flex successor_)}, cue: :"%flex" do
        Term.of(:"%flex", normalize(Π.pattern(successor)),
          seq: true,
          depth: :envelope,
          bounds: 1,
        )
      end

      matchpi %{_dict} do
        Normalize.singular(normalize(Π.pattern(item)))
      end
    end
  end

  # :nodoc:
  def normalize(prod : Π::EntryKV) : Term
    key, value = prod.key, prod.value

    Term.case(value, engine: M0) do
      # |@ m1.operator.pair.optional
      #
      # |@pattern
      # (%'%optional default_ successor_)
      #
      # |@key default
      # A literal to pass to *successor* if the value is missing.
      #
      # |@key successor m1.operator
      #
      # |@block
      # Expects an entry whose value excites *successor*, or no entry. If no entry,
      # passes *default* to *successor*.
      #
      # - If the entry's value does not excite *successor*, this counts as a mismatch.
      # - If *default* does not excite *successor*, this counts as a mismatch.
      #
      # Entry optional has several shorthands in WwML (see the WwML doc, section
      # *Selectors*). The most important one is the triple colon with default, `⋮`:
      #
      # ```
      # (¦ name⋮ "John Doe", age⋮ 34) => (^name ^age)
      #
      # {}                            ;; => ("John Doe" 34)
      # {name: "Susan Doe"}           ;; => ("Susan Doe" 34)
      # {name: qux}                   ;; mismatch
      # {name: "Susan Doe", age: 45}  ;; => ("Susan Doe" 45)
      # {name: "Susan Doe", age: qux} ;; mismatck
      # ```
      matchpi %{(%'%optional default_ successor_)}, cue: :"%optional" do
        Term.of(:"%entry/optional", {:"%key", key}, {:"%payload", default}, Normalize.sealed(Π.pattern(successor)),
          entry: true,
          bounds: {0, :"..=", 1},
          depth: {:+, {:max, {:members, 0, :"..<", 1}, :max}, 1},
        )
      end

      # |@ m1.operator.pair.negative
      #
      # |@pattern
      # (%'%- barrier_)
      #
      # |@key barrier m1.operator
      # Matches in an isolated environment.
      #
      # |@block
      # Expects an entry whose value **does not** excite *barrier*, or no entry.
      # If the entry's value excites *barrier*, this counts as a mismatch.
      #
      # The most useful form of this operator is `x: (%- _)`, which has a WwML selector
      # shorthand `-x`. This basically means "not any", thus matching the absence of `x`.
      #
      # ```
      # {¦ -x y_} <> {y: ^(+ y 1)}
      #
      # {y: 1}         ;; => {y: 2}
      # {x: qux, y: 1} ;; mismatch
      # ```
      matchpi %{(%'%- barrier_)}, cue: :"%-" do
        normal = Term::Dict.build do |commit|
          commit << :"%entry/negative" << {:"%key", key} << Normalize.sealed(Π.pattern(barrier))

          commit.with(:entry, true)
          if barrier == Term[:_] # matches absence
            commit.with(:bounds, 0)
            commit.with(:depth, 1)
          else
            # May match presence, treat as optional.
            commit.with(:bounds, {0, :"..=", 1})
            commit.with(:depth, {:min, 1})
          end
        end

        Term.of(normal)
      end

      # |@ m1.operator.pair.negative
      #
      # |@pattern
      # (%'%- barrier_ name_)
      #
      # |@key barrier m1.operator
      # Matches in an isolated environment.
      #
      # |@key name
      # Any term to refer to the missing value in backmaps.
      #
      # |@block
      # Expects an entry whose value **does not** excite *barrier*, or no entry.
      # If the entry's value excites *barrier*, this counts as a mismatch.
      #
      # The most useful form of this operator is `x: (%- _ x)` in backmaps, which
      # has a WwML selector shorthand `-x_`. This basically means "not any", thus
      # matching the absence of `x`, and capturing the "hole" under `x`, possibly
      # to be filled by the backmap.
      #
      # This operator is often used to mark structures as "seen" by particular rules:
      #
      # ```
      # {¦ name_string -seen_} <> {name: ^(~ "Hello, " name), seen: true}
      #
      # {name: "John Doe"}                    ;; => {name: "Hello, John Doe", seen: true}
      # {name: "Hello, John Doe", seen: true} ;; => mismatch
      # ```
      #
      # Especially if you want multiple rules to be able to sign a structure as "seen" --
      # without [easy] collisions -- you can use the rule id symbol `◇` provided by
      # WwML as the capture name. `◇` generates a rule id based on the hash of
      # the current rule.
      #
      # ```
      # {¦ name_string -◇_} <> {name: ^(~ "Hello, " name), ◇: true}
      # {¦ age_number -◇_} <> {age: ^(+ age 10), ◇: true}
      #
      # {name: "John Doe"}
      # ;; => {name: "Hello, John Doe", ckewdBbMdzbJduby: true}
      #
      # {name: "Susan Doe", age: 25}
      # ;; => {name: "Hello, Susan Doe",
      # ;;     age: 35,
      # ;;     ckewdBbMdzbJduby: true,
      # ;;     eydFbWdTcuePendi: true}
      # ```
      #
      # Similarly, to identify a block of rules (one or more empty lines act as block
      # delimiters), you can use the rule block id shorthand, `▢`:
      #
      # ```
      # {¦ name_string -▢_}
      #   <> {name: ^(~ "Hello, " name), ▢: true}
      # ;; NOTE: This rule is more specific than the one above.
      # {¦ name_string age_number -▢_}
      #   <> {name: ^(~ "Hi, " name), age: ^(+ age 10), ▢: true}
      #
      # {name: "John Doe"}
      # ;; => {name: "Hello, John Doe", ccefcVyZbYdWcA: true}
      #
      # {name: "Susan Doe", age: 25}
      # ;; => {name: "Hi, Susan Doe", age: 35, ccefcVyZbYdWcA: true}
      # ```
      #
      # Rule block ids are useful for mutually exclusive rules, although specificity
      # must be managed carefully; it will be the most specific rule that signs the underlying
      # structure using the rule block id, thus blocking all other [less or equally specific]
      # rules in the same block.
      matchpi %{(%'%- barrier_ name_)}, cue: :"%-" do
        normal = Term::Dict.build do |commit|
          commit << :"%entry/negative" << {:"%key", key} << Normalize.sealed(Π.pattern(barrier))
          commit << {:"%ref", name}

          commit.with(:entry, true)
          if barrier == Term[:_] # matches absence
            commit.with(:bounds, 0)
            commit.with(:depth, 1)
          else
            # May match presence, treat as optional.
            commit.with(:bounds, {0, :"..=", 1})
            commit.with(:depth, {:min, 1})
          end
        end

        Term.of(normal)
      end

      # |@ m1.operator.pair.required
      #
      # |@pattern
      # operator_
      #
      # |@key operator m1.operator
      #
      # |@block
      # Expects an entry whose value matches *operator*.
      #
      # ```
      # {¦ name_} => ^name
      # ;; Shorthand for {¦ name: name_}
      # ;; Shorthand for (%layer _ {name: name_})
      #
      # {name: "John Doe"} ;; => "John Doe"
      # ```
      otherwise do
        Term.of(:"%entry/required", {:"%key", key}, normalize(Π.pattern(value)),
          entry: true,
          depth: {:+, :envelope, 1},
          bounds: 1,
        )
      end
    end
  end

  # :nodoc:
  def normalize(prod : Π::Dict(Term::Dict)) : Term
    dict = prod.dict
    if Normalize.literal?(dict)
      # (+ a b)
      return Normalize.literal(Term.of(dict))
    end

    if dict.itemsonly?
      # (+ a_ b_)
      normal = Term::Dict.build do |commit|
        commit << :"%seq"
        commit.concat(dict.items) { |item| normalize(Π.item(item)) }

        commit.with(:guarded, true)
        commit.with(:depth, {:+, :envelope, 1})
        commit.with(:bounds, :sum)
      end

      return Term.of(normal)
    end

    if dict.pairsonly?
      # {name: "John", age: age←(%number (whole _) >= 24)}
      return normalize(Π.pattern(Term.of(:"%layer", Term.of, dict)))
    end

    # (+ a_ b_ age: (%number _ > 30))
    normalize(Π.pattern(Term.of(:"%partition", dict.itemspart, dict.pairspart)))
  end

  # :nodoc:
  def normalize(prod : Π::Pattern(Term)) : Term
    pattern = prod.pattern

    Term.case(pattern, engine: M0) do
      matchpi %{_dict} do
        # NOTE: this is a fast path for itemsonly dictionaries. They'd otherwise be
        # at the very bottom, which wouldn't be good because they're very frequent
        # in practice. We do only the simplest, almost probabilistic checks here; if they
        # fail, we will go with the longer but precise path.
        #
        # WARNING: if you want a pattern matching construct that's a dictionary and that
        # doesn't start with %, you will have to be friends with this fast path
        # (see `probably_operator?`)
        dict = pattern.as_d
        if (head = dict.items.first?) && Normalize.probably_operator?(head)
          continue
        end

        normalize(Π.dict(dict))
      end

      # |@ m1.operator.let
      #
      # |@pattern
      # (%'%let name_ successor_)
      #
      # |@key name
      # Any term works: a number, a symbol, a string, a dictionary etc. For example,
      # matching `(foo 1←_ 0←_)` (which is the same as `(foo (%let 1 _) (%let 0 _))`)
      # against `(foo a b)` results in the match env `(b a)`.
      #
      # |@block
      # `%let` lets you *capture* its matchee before proceeding to match *successor*.
      #
      # Captures offer a way to save the matchee in a *match environment* (or simply
      # *match env*, or *env*).
      #
      # There are several reasons why you might want to capture.
      #
      # - To access and work with the captured term(s) after matching.
      #   ```
      #   (swap a←(_ _) b←(_ _)) => (swap ^b ^a)
      #   ;; Left-hand side is the same as writing `(swap (%let a (_ _)) (%let b (_ _)))`.
      #
      #   (swap (1 2) (3 4)) ;; => (swap (3 4) (1 2))
      #   ```
      #
      # - To refer to the captured term within the pattern itself, but in other
      #   places. This way, you can *constrain* those other places based on
      #   the captured term.
      #   ```
      #   (equal? x_ x_) => true
      #   (equal? _ _) => false
      #
      #   (equal? 100 100) ;; => true
      #   (equal? 100 200) ;; => false
      #
      #   (value (%value k v_) k_) => (ok ^v)
      #   (value _ _) => (err "not found")
      #
      #   (value {x: 100, y: 200} x) ;; => (ok 100)
      #   (value {x: 100, y: 200} y) ;; => (ok 200)
      #   (value {x: 100, y: 200} z) ;; => (err "not found")
      #
      #   (first-common ⟨x_⟩ ⟨x_⟩) => (ok ^x)
      #   (first-common _ _) => (err "no common elements")
      #
      #   (first-common (1 2 3) (a b 1 c 2)) ;; => (ok 1)
      #   (first-common (1 2 3) (4 5 6))     ;; => (err "no common elements")
      #   ```
      #
      # - To use backmaps: backmaps let you modify captures after the matching
      #   process. In a sense, captures are "getters" and backmaps can turn them
      #   into "setters" automatically, while preserving the rest of structure.
      #   ```
      #   (swap a_ b_) <> {b: ^a, a: ^b}
      #   ;; Left-hand side is the same as writing `(swap (%let a _) (%let b _))`.
      #
      #   (swap 100 200) ;; => (swap 200 100)
      #   ```
      matchpi %{(%'%let name_ successor_)}, cue: :"%let" do
        Term.of(:"%let", {:"%capture", name}, normalize(Π.pattern(successor)),
          depth: :envelope,
          # %let has only one successor, so despite the wording, "sum" here really
          # means "passthrough".
          bounds: :sum,
        )
      end

      # |@ m1.operator.blank
      #
      # |@pattern
      # %'_
      # %'_dict
      # %'_number
      # %'_string
      # %'_symbol
      # %'_boolean
      # (%symbol blank _ _)
      #
      # |@block
      # A blank matches a term of a chosen type; or any term if there is no chosen type.
      #
      # | One             | Named | Typed | Zero or more                                           | One or more                                            |
      # | --------------- | ----- | ----- | ------------------------------------------------------ | ------------------------------------------------------ |
      # | `_`             | -     | -     | `_*`                                                   | `_+`                                                   |
      # | `_number`       | -     | +     | `_number*`                                             | `_number+`                                             |
      # | `_string`       | -     | +     | `_string*`                                             | `_string+`                                             |
      # | `_symbol`       | -     | +     | `_symbol*`                                             | `_symbol+`                                             |
      # | `_boolean`      | -     | +     | `_boolean*`                                            | `_boolean+`                                            |
      # | `_dict`         | -     | +     | `_dict*`                                               | `_dict+`                                               |
      # | `author_`       | +     | -     | `author_*` (but usually you'd use plural: `authors_*`) | `author_+` (but usually you'd use plural: `authors_+`) |
      # | `author_string` | +     | +     | `author_string*` (ditto, `authors_string*`)            | `author_string+` (ditto, `author_string+`)             |
      #
      # Singular blanks are generally shorthands for `%let`. For example, `x_number` is
      # a shorthand for `(%let x _number)` which is in turn a shorthand for `(%let x (%number _))`.
      # Similarly, `name_string` is a shorthand for `(%let name _string)`.
      #
      # ```
      # (lamp on)      ;; => (ok "Turned the lamp on")
      # (lamp off)     ;; => (ok "Turned the lamp off")
      # (lamp _symbol) ;; => (err "Humph?")
      # ```
      #
      # The `_` blank is also called *pass* when used as an operator. A pretty common
      # use-case for `_` is to represent "everything else" in rules:
      #
      # ```
      # (square a_number) => (ok (* a a))
      # (square _) => (err "Oops. Cannot square it")
      #
      # (square 4)     ;; => (ok 16)
      # (square "Qux") ;; => (err "Oops. Cannot square it")
      # ```
      matchpi %{_symbol} do
        sym = pattern.as_sym

        # Blanks are very frequent; as are symbols. We avoid using matchpis for type-
        # only blanks _number, _string, etc. so that this _symbol matchpi is
        # immediately reached.
        #
        # Named blanks are transformed into %let which we recurse upon. The recursion
        # is done to perform further normalization (since we're not rewriting here we
        # must recurse manually).
        case sym
        when SYM_BLANK_ANY     then NORMAL_PASS
        when SYM_BLANK_NUMBER  then NORMAL_BLANK_NUMBER
        when SYM_BLANK_STRING  then NORMAL_BLANK_STRING
        when SYM_BLANK_SYMBOL  then NORMAL_BLANK_SYMBOL
        when SYM_BLANK_BOOLEAN then NORMAL_BLANK_BOOLEAN
        when SYM_BLANK_DICT    then NORMAL_BLANK_DICT
        else
          continue unless blank = sym.blank?
          continue unless blank.singular?
          continue unless name = blank.name?

          normalize(Π.pattern(Term.of(:"%let", name, blank.type.blank)))
        end
      end

      # |@ m1.operator.literal
      #
      # |@pattern
      # _symbol
      # _number
      # _string
      # _boolean
      #
      # |@block
      # Numbers such as `100`, strings (e.g. `"hello world"`), booleans (`true`, `false`),
      # non-blank symbols (e.g. `qux`), and polyblanks at top-level (e.g. `xs_*`) are treated
      # as shorthands for `(%literal ⏏)`: `(%literal 100)`, `(%literal "hello world")`,
      # `(%literal true)`, and so on, correspondingly.
      #
      # Just as with `%literal`, they require an exact match.
      #
      # ```
      # (xor false false) => false
      # (xor false true) => true
      # (xor true false) => true
      # (xor true true) => false
      #
      # (xor true false) ;; => true
      # ```
      matchpi %{_symbol}, %{_number}, %{_string}, %{_boolean} do
        Normalize.literal(pattern)
      end

      # |@ m1.operator.partition
      #
      # |@pattern
      # (%'%partition itemspart_ pairspart_)
      #
      # |@key itemspart m1.operator
      # |@key pairspart m1.operator
      #
      # |@block
      # Forwards a dictionary's itemspart to *itemspart*, and pairspart
      # to *pairspart*.
      #
      # `%partition` with an item sequence *itemspart* has a shorthand in
      # WwML: `(¦ pairspart_)` is the same as `(%partition () pairspart_)`;
      # similarly, e.g. `(+ a_ b_ ¦ opts_)` is the same as `(%partition (+ a_ b_) opts_)`.
      #
      # ```
      # (%partition items_ pairs_) => (^items ^pairs)
      #
      # ()                  ;; => (() ())
      # (+ 1 2)             ;; => ((+ 1 2) ())
      # {a: 10, b: 20}      ;; => (() {a: 10, b: 20})
      # (+ 1 2 a: 10)       ;; => ((+ 1 2) {a: 10})
      # (+ 1 2 a: 10 b: 20) ;; => ((+ 1 2) {a: 10, b: 20})
      # ```
      matchpi %{(%'%partition itemspart_ pairspart_)}, cue: :"%partition" do
        Term.of(:"%partition", normalize(Π.pattern(itemspart)), normalize(Π.pattern(pairspart)),
          guarded: true,
          depth: :envelope,
          bounds: :sum,
        )
      end

      # |@ m1.operator.edge
      #
      # |@pattern
      # (%'edge _symbol)
      #
      # |@block
      # Edges are frequent in Delta7, Rack, and generally in µsoma. Therefore, in patterns,
      # `(edge <typed blank>)` is treated specially for brevity and historical reasons.
      # It matches an edge "inside-out", treating an edge as an indivisible unit despite
      # the way the pattern is spelled and the fact that it's a dict just like any other
      # after all.
      #
      # It is an unfortunate but very useful exception from the strict rule of `%`-prefixing
      # terms whose meaning is not literal for M1.
      #
      # `(edge x_)` matches as `x←(edge _)`.
      #
      # `(edge  ...)` has a shorthand syntax of `@...` in WwML: `@x` is the same as `(edge x)`
      # and `@x_` is the same as `(edge x_)` (which matches `x←(edge _)`).
      #
      # `@x_number` is a shorthand for `(edge x_number)`, which, as expected, receives
      # special treatment, and matches as `x←(edge _number)`.
      #
      # It is possible to use a dictionary literal as an edge name. This is sometimes useful
      # to "scope" edges. For example, `@(a b c)` could represent what one would scope under
      # `a.b.c` in a traditional language.
      #
      # ```wwml
      # (appender (_* `target) @edge_) <> {target: ^edge, (edge): ()}
      #
      # (appender () @foo)     ;; => (appender (@foo))
      # (appender (@foo) @bar) ;; => (appender (@foo @bar))
      # ```
      #
      # If you want to treat edges as "divisible" (that is, if you want them to receive
      # normal, dictionary treatment), then you can either enclose `edge` in a `%literal`,
      # like so: `((%literal edge) x_)`; or use `%nonself` on the argument: `(edge (%nonself x_))`.
      # `%nonself` has the shorthand prefix `≡` so the latter may be rewritten as `(edge ≡x_)`.
      #
      # ```wwml
      # (appender (_* `target) outer←(edge ≡edge_))
      #   <> {target: ^edge, (outer): ()}
      #
      # (appender () @foo)    ;; => (appender (foo))
      # (appender (foo) @bar) ;; => (appender (foo bar))
      # ```
      matchpi %{(edge arg_symbol)}, cue: :edge do
        continue unless blank = arg.blank?
        continue unless blank.singular?

        if name = blank.name?
          # (edge x_symbol) -> x←(edge _symbol)
          return normalize(Π.pattern(Term.of(:"%let", name, {:edge, blank.type.blank})))
        end

        case blank.type
        in .symbol?  then NORMAL_EDGE_SYMBOL
        in .string?  then NORMAL_EDGE_STRING
        in .number?  then NORMAL_EDGE_NUMBER
        in .dict?    then NORMAL_EDGE_DICT
        in .boolean? then NORMAL_EDGE_BOOLEAN
        in .any?     then NORMAL_EDGE_ANY
        end
      end

      # |@ m1.operator.layer
      #
      # |@pattern
      # (%'%layer below_ side_dict)
      # (%'%layer below_ ¦ side_)
      #
      # |@key below m1.operator
      #
      # |@key side
      # Entries that the layer recognizes. Each entry's value can be one of
      # `m1.operator.entry`. It can be that for any entry, but for `%layer`,
      # this is true in particular.
      #
      # |@block
      # `%layer` subtracts entries with keys in *side* from the matchee dict.
      # The result is two dicts: one with the subtracted entries, and the other,
      # called *residue*, containing the rest of items. The residue is passed
      # to *below*.
      #
      # `%layer` is often used to match some entries while ignoring the rest.
      # In such cases the residue is allowed to be anything, so `_` is used
      # for *below*.
      #
      # For example, take a look at the pattern `(%layer _ {name: name_string, age: age_number})`.
      # When given, say, the matchee `{name: "John", age: 25, occupation: "Gardener"}`, `%layer`
      # "peels off" *name* and *age*, producing `{name: "John", age: 25}` and passing it to
      # the side -- namely, `{name: name_string, age: age_number}`. *occupation* is then in
      # the residue, `{occupation: "Gardener"}`. The residue is passed to *below*, `_`, and
      # thus, discarded.
      #
      # Matching the side, we obtain a match env, `{name: "John", age: 25}`.
      # The residue contributes nothing to the match env.
      #
      # This form of `%layer` -- with `_` as the residue -- is very common, especially
      # in rules, since they often want to describe only parts of structure, not
      # the entirety of it. For this reason, it has a shorthand: `{¦ ...}`. When
      # written using this shorthand, the example pattern above starts to look like
      # `{¦ name: name_string, age: age_number}`. Further rewrites are possible,
      # using other, unrelated shorthands: redundant `name: name_string` and
      # `age: age_number` collapse to `{¦ name_string age_number}`. `age_number` can
      # then be further collapsed, producing `{¦ name_string ±age}`.
      #
      # You can use `%layer` in an opposite way when it comes to the residue: instead
      # of `_`, you can pass `()`, the empty dict. This means that the layer must
      # now make sure the residue is empty; that is, the matchee must not contain
      # any extra entries other than those in the side.
      #
      # Consider, for example, the pattern `(%layer () {name: name_string, age: age_string})`.
      # What it says is that there must be no entries in the matchee besides `name: name_string`
      # and `age: age_string`. That is, after subtracting *name* and *age*, the residue
      # must be empty -- `()`.
      #
      # The residue can be matched arbitrarily. You can capture it, as in
      # `(%layer rest_ {name: name_, age: age_})`. This pattern, when given
      # the matchee `{name: "Susan", age: 35, children: 2, occupation: "Manager"}`,
      # produces `{name: "Susan", age: 35, rest: {children: 2, occupation: "Manager"}}`.
      #
      # You can nest `%layer`s, or use `%keypool` to allow zero or more of
      # the allowed set of extra keys:
      # `(%layer (%keypool occupation children) {name: name_string, age: age_number})`
      # allows only *occupation* and *children* (both optional) beside *name* and *age*.
      #
      # A general shorthand exists for when you want to write `%layer` in
      # the pairspart of `%partition`, while also having an item sequence pattern
      # in the itemspart.
      #
      # Consider, for instance, the pattern
      # `(%partition (/ a_ b_) (%layer () {precision: precision_number})`.
      # You can rewrite it to `(/ a_ b_ ¦ () precision: precision_number)`. When
      # using this shorthand, you can omit `()`, provided the pattern makes sense
      # afterwards. That is, you can write `(/ a_ b_ ¦ precision: precision_number)`;
      # but then, if you want to collapse it further by writing `(/ a_ b_ ¦ precision_number)`,
      # the pattern stops making sense, because what it says is
      # `(%partition (/ a_ b_) (%layer precision_number {}))`. The residue of a dict
      # is a dict -- not a number -- so this will never match! So in this particular case
      # you have to choose: either `(/ a_ b_ ¦ precision: precision_number)`, or
      # `(/ a_ b_ ¦ () precision_number)`. The second is preferred.
      #
      # There also exists a shorthand for `¦ _`, `⍊`; so you can rewrite `(/ a_ b_ ¦ _ precision_number)`
      # to `(/ a_ b_ ⍊ precision_number)` which further reduces to `(/ a_ b_ ⍊ ±precision)`.
      matchpi(
        %{(%'%layer below_ side_dict)},
        %{(%'%layer below_ ¦ side_)},
        cue: :"%layer",
      ) do
        normal = Term::Dict.build do |commit|
          commit << :"%layer" << normalize(Π.pattern(below))
          commit.concat(side.ee) do |(key, value)|
            normalize(Π.entry(key, value))
          end

          commit.with(:guarded, true)
          commit.with(:depth, :envelope)
          commit.with(:bounds, :sum)
        end

        Term.of(normal)
      end

      # |@ m1.operator.number
      #
      # |@pattern
      # (%'%number type_symbol)
      #
      # |@key type
      # One of the available fixed-width types.
      #
      # |@block
      # A shorthand for matching a number bounded by the number of bits. Expands
      # to `(%number <min bound for type> <= (whole _) <= <max bound for type>)`.
      #
      # The following bit widths are available: `8`, `16`, `32`, `64`, `128`.
      #
      # - Prefix with `u` to check within unsigned bounds (zero or positive): e.g., `u8`, `u32`.
      # - Prefix with `i` to check within signed bounds: e.g., `i8`, `i32`.
      # - Prefix with `+i` to check within signed bounds, allowing only zero or
      #   positive: e.g., `+i8`, `+i32`. Add `!` at the end to exclude zero, e.g.
      #   `+i8!` means "in signed 8-bit bounds, positive, nonzero".
      # - Prefix with `-i` to check within signed bounds, allowing only negative:
      #   e.g., `-i8`, `-i32`.
      #
      # Note that there is no fixed-width number type in Wirewright; numbers can
      # be arbitrarily long. However, bit width is a very convenient way to
      # constrain numbers. It is also useful under the hood: we use it to make
      # sure we can safely cast to e.g. Crystal `Int32`.
      #
      # ```
      # (side (%number -i8)) => left
      # (side (%number +i8)) => right
      #
      # (side -100) ;; => left
      # (side 0)    ;; => right
      # (side 10)   ;; => right
      #
      # (rgb? ((%number u8) (%number u8) (%number u8))) => true
      # (rgb? _) => false
      #
      # (rgb? (0x33 0xfa 0xfa)) => ;; true
      # (rgb? (1000 0xfa 0xfa)) => ;; false
      # ```
      matchpi %{(%'%number type_symbol)}, cue: :"%number" do
        continue unless spec = NumberSpec::INT[type]?

        Normalize.terminal(spec, depth: 0)
      end

      # TODO: mark var's as %ref.
      matchpi %{(%'%number _*)}, cue: :"%number" do
        continue unless _ = NumberSpec.op?(pattern)

        Normalize.terminal(pattern, depth: 0)
      end

      # |@ m1.operator.pipe
      #
      # |@pattern
      # (%'%pipe fn_ successor_)
      #
      # |@key fn
      # The function that should be used to transform the matchee. Refer to more
      # specific doc items for info.
      #
      # |@key successor m1.operator
      #
      # |@block
      # Allows you to "pipe" the matchee through a chain of transformations. Each
      # transformation is a function, *fn*. Refer to more specific doc items to
      # learn about them.
      #
      # ```wwml
      # (parity (%pipe (mod 2) (map (even odd)) p_)) => ^p
      #
      # (parity 0)  ;; => even
      # (parity 2)  ;; => even
      # (parity 1)  ;; => odd
      # (parity -5) ;; => odd
      # ```
      matchpi(
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (+ n_number) _)
        #
        # |@block
        # Adds *n* to a number matchee: e.g. `(%pipe (+ 100) x_)`.
        %{(%'%pipe fn←(+ _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (- n_number) _)
        #
        # |@block
        # Subtracts *n* from a number matchee: e.g. `(%pipe (- 100) x_)`.
        %{(%'%pipe fn←(- _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (* n_number) _)
        #
        # |@block
        # Multiplies a number matchee by *n*: e.g. `(%pipe (* 2) double_)`.
        %{(%'%pipe fn←(* _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (/ n_number) _)
        #
        # |@block
        # Divides a number matchee by *n*: e.g. `(%pipe (/ 2) n_)`. Turns into
        # a nevermatch if *n* is zero: `(%pipe (/ 0) n_)` will never match.
        %{(%'%pipe fn←(/ _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (div n_number) _)
        #
        # |@block
        # Integer division of a number matchee by *n*: e.g. `(%pipe (div 10) x_)` will
        # match `42` with `{x: 40}`. Like `/`, it is a nevermatch if *n* is zero.
        %{(%'%pipe fn←(div _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (mod n_number) _)
        #
        # |@block
        # Remainder after integer division of a number matchee by *n*: e.g.
        # `(%pipe (mod 10) x_)` will match `42` with `{x: 2}`. Like `/`, it is
        # a nevermatch *n* is zero.
        %{(%'%pipe fn←(mod _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (** n_number) _)
        #
        # |@block
        # Raises a number matchee to the power *n*: e.g. `(%pipe (** 2) n_)` will
        # match `4` with `{x: 16}`. It is a nevermatch if the matchee is zero and
        # *n* is negative.
        %{(%'%pipe fn←(** _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (clamp min_number ..= max_number) _)
        #
        # |@block
        # Clamps a number matchee so that it lies within the given bounds.
        #
        # If the matchee is less than *min*, it is replaced with *min*; if greater
        # than *max*, it is replaced with *max*. Otherwise, the value passes through
        # unchanged.
        #
        # ```
        # (%pipe (clamp 0 ..= 10) x_) => ^x
        #
        # -5 ;; => 0
        # 5  ;; => 5
        # 20 ;; => 10
        # ```
        %{(%'%pipe fn←(clamp _number ..= _number) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe (map table_dict) _)
        #
        # |@block
        # Using the matchee as a key, retrieves the associated value (if any)
        # in *table*. If the value is absent, triggers a mismatch.
        #
        # ```
        # (%pipe (map {x: 10, y: 20}) mapout_) => ^mapout
        #
        # a ;; mismatch
        # x ;; => 10
        # y ;; => 20
        # z ;; mismatch
        # ```
        %{(%'%pipe fn←(map _dict) successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe span _)
        #
        # |@block
        # Transforms a string matchee into the number of characters in it: e.g.,
        # `(%pipe span 1)` matches strings that contain exactly one character
        # (Unicode codepoint).
        %{(%'%pipe fn←span successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe tally _)
        #
        # |@block
        # Transforms a dictionary matchee into the number of entries in it: e.g.,
        # `(%pipe tally 3)` matches dictionaries which have exactly three entries
        # (items, e.g. `(+ 1 2)`, pairs, e.g. `{x: 10, y: 20, z: 30}`, or both, e.g.
        # `(point x: 10 y: 20)`).
        %{(%'%pipe fn←tally successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe type _)
        #
        # |@block
        # Transforms any term into an unnamed, typed blank: e.g. `(%pipe type t_)`
        # will match `42` with `{t: _number}`, `"hello"` with `{t: _string}`, and
        # so on.
        %{(%'%pipe fn←type successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe ml _)
        #
        # |@block
        # Interprets a string matchee as a term represented using WwML.
        #
        # In other words, `ml` parses strings as WwML. If parsing fails, this counts
        # as a mismatch. If parsing succeeds, the term is passed to the pipe successor
        # so that pattern matching can continue on the parsed term.
        #
        # ```
        # (%pipe ml (mod 2) (map (even odd)) x_) => ^x
        #
        # "100" ;; => even
        # "101" ;; => odd
        # "abc" ;; mismatch triggered by mod
        # "("   ;; mismatch triggered by ml
        # ```
        %{(%'%pipe fn←ml successor_)},
        # |@ m1.operator.pipe
        #
        # |@pattern
        # (%'%pipe untracked _)
        #
        # |@block
        # A utility function for backmaps which prevents the backmap engine
        # from being able to manipulate downstream captures.
        #
        # All pipe functions do this, so in terms of what this function does,
        # it is an identity function.
        #
        # ```
        # ((%pipe untracked x_) x_) <> {x: ^(seen x)}
        #
        # (100 100) ;; => (100 (seen 100))
        # (100 200) ;; mismatch
        # ```
        %{(%'%pipe fn←untracked successor_)},
        cue: :"%pipe",
        cues: {:+, :-, :*, :/, :div, :mod, :**, :clamp, :map, :span, :tally,
               :type, :ml, :untracked}
      ) do
        Normalize.sealed(Term.of(:"%pipe", {:"%payload", fn}, normalize(Π.pattern(successor))))
      end

      # |@ m1.operator.pipe
      #
      # |@pattern
      # (%'%pipe _ _ _+)
      #
      # |@block
      # Shorthand for chaining several `%pipe` transformations.
      #
      # For example, `(%pipe (+ 1) (** 3) (mod 2) (map (10 20)) x_)` matches `42`
      # with `{x: 20}` because `(42 + 1)**3 mod 2 = 1`, which, according to the mapper
      # dict `{0: 10, 1: 20}`, is `20`, which is subsequently captured by `x_`.
      matchpi %{(%'%pipe head_ _*)}, cue: :"%pipe" do
        continue if pattern.size < 4 # at least %pipe + head₁ + head₂ + body

        cont = Term::Dict.build do |commit|
          commit << :"%pipe"
          commit.concat(pattern.items.move(2))
        end

        normalize(Π.pattern(Term.of(:"%pipe", head, cont)))
      end

      # |@ m1.operator.all
      #
      # |@pattern
      # (%'%all arms_*)
      #
      # |@key arms m1.operator
      #
      # |@block
      # An AND over *arms*: lets you match multiple patterns against the same
      # underlying matchee. The arms are allowed to constrain each other.
      #
      # ```
      # (%all ⟨(entry k_ v_)⟩ (k_ _*)) => ^v
      #
      # (y (entry x 100) (entry y 200) (entry z 300))
      # ;; => 200
      #
      # ;; NOTE: The recommended way to write the above is:
      # ;;
      # ;;  (k_ (%group ⟨(entry k_ v_)⟩ _*)) => ^v
      # ;;
      # ```
      matchpi %{(%'%all _*)}, cue: :"%all" do
        arms = pattern.items.move(1)

        normal = Term::Dict.build do |commit|
          commit << :"%all"
          commit.concat(arms) { |arm| normalize(Π.pattern(arm)) }

          commit.with(:guarded, true)
          commit.with(:depth, :envelope)
        end

        Term.of(normal)
      end

      # |@ m1.operator.any
      #
      # |@pattern
      # (%'%any options_*)
      #
      # |@key options
      # Lists the allowed terms (treated literally).
      #
      # |@block
      # An OR over zero or more literal *options*: the matchee must be one of
      # the alternatives.
      #
      # ```wwml
      # (available? (%any Monday Wednesday Friday)) => true
      # (available? _) => false
      #
      # (available? Monday) ;; => true
      # (available? Friday) ;; => true
      # (available? Sunday) ;; => false
      # ```
      #
      # For example, `(%any a b c)` is functionally equivalent to the pattern
      # `(%any° (%literal a) (%literal b) (%literal c))` (i.e., forcing literal
      # treatment; although it is not necessary in this particular example).
      matchpi %{(%'%any _*)}, cue: :"%any" do
        options = pattern.items.move(1)

        mindepth = options.min_of? { |option| (dict = option.as_d?) ? dict.depth : 0 } || 0
        maxdepth = options.max_of? { |option| (dict = option.as_d?) ? dict.depth : 0 } || 0

        Normalize.terminal(pattern, depth: {mindepth, :"..=", maxdepth})
      end

      # |@ m1.operator.not
      #
      # |@pattern
      # (%'%not terms_*)
      #
      # |@key terms
      # Lists the forbidden terms (treated literally).
      #
      # |@block
      # Prevents select terms from matching: the matchee must **not** be one of *terms*.
      #
      # ```wwml
      # (allow? _) => true
      # (allow? (%not admin owner)) => false
      #
      # (allow? admin)    ;; => true
      # (allow? owner)    ;; => true
      # (allow? john-doe) ;; => false
      # ```
      matchpi %{(%'%not _*)}, cue: :"%not" do
        Normalize.terminal(pattern)
      end

      matchpi %{(%'%any° _*)}, cue: :"%any°" do
        arms = pattern.items.move(1)

        normal = Term::Dict.build do |commit|
          commit << :"%any°"
          commit.concat(arms) { |arm| Normalize.sealed(Π.pattern(arm)) }

          commit.with(:disjunction, true)
          commit.with(:depth, {:∩, {:min, {:members, 0, :"..<", arms.size}, :min}, {:max, {:members, 0, :"..<", arms.size}, :max}})
        end

        Term.of(normal)
      end

      matchpi %{(%'%literal term_)}, cue: :"%literal" do
        Normalize.literal(term)
      end

      matchpi %{(%'%keypool _*)}, cue: :"%keypool" do
        Normalize.terminal(pattern)
      end

      matchpi %{(%'%-keypool _*)}, cue: :"%-keypool" do
        Normalize.terminal(pattern)
      end

      matchpi %{(%'%keytest _*)}, cue: :"%keytest" do
        Normalize.terminal(pattern)
      end

      matchpi %{(%'%keypath capture_)}, cue: :"%keypath" do
        Term.of(:"%keypath", {:"%capture", capture})
      end

      matchpi %{[%'%nonself arg_]}, cue: :"%nonself" do
        normalize(Π.pattern(arg))
      end

      matchpi %{(%'%never)}, cue: :"%never" do
        pattern
      end

      matchpi %{(%'%value capture_ body_)}, cue: :"%value" do
        Term.of(:"%value", {:"%capture", capture}, normalize(Π.pattern(body)),
          depth: {:+, {:max, :member, :min}, 1},
        )
      end

      matchpi %{(%'%-value capture_)}, cue: :"%-value" do
        Term.of(:"%-value", {:"%capture", capture},
          depth: {:min, 1},
        )
      end

      matchpi %{(%'%-value capture_ name_)}, cue: :"%-value" do
        Term.of(:"%-value", {:"%capture", capture}, {:"%ref", name},
          depth: {:min, 1},
        )
      end

      matchpi %{(%'%string nonempty)}, cue: {:"%string", :nonempty} do
        normalize(Π.pattern(STRING_NONEMPTY))
      end

      matchpi %{(%'%symbol nonblank)}, cue: {:"%symbol", :nonblank} do
        Term.of(:"%symbol", :nonblank, depth: 0)
      end

      matchpi %{(%'%symbol blank name_ type_)}, cue: {:"%symbol", :blank} do
        Term.of(:"%symbol", :blank, Normalize.sealed(Π.pattern(name)), Normalize.sealed(Π.pattern(type)), depth: 0)
      end

      matchpi %{(%'%pluck spec←(_*) successor_)}, cue: :"%pluck" do
        # NOTE: Since spec can't be invalid, we don't bother checking it here.
        # Any spec works; whether it makes sense or not is another question...
        Term.of(:"%pluck", {:"%payload", spec}, normalize(Π.pattern(successor)),
          guarded: true,
          depth: {:max, :member, :min},
        )
      end

      matchpi %{(%'%flat spec←(_*) successor_)}, cue: :"%flat" do
        # Depth step is count leading _/./* (they signal "deeper", and so increase
        # depth unconditionally).
        nsteps = 0
        spec.items.each do |step|
          break unless step.in?(Term[:_], Term[:"."], Term[:"*"])
          nsteps += 1
        end

        Term.of(:"%flat", {:"%payload", spec}, normalize(Π.pattern(successor)),
          guarded: true,
          depth: {:+, {:max, :member, :min}, nsteps},
        )
      end

      matchpi %{(%'%filter deps←(_*) selector_ successor_ ¦ opts_)}, cue: :"%filter" do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 1u32)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          continue if max.is_a?(UInt32) && min > max

          normal = opts.transaction do |commit|
            commit << :"%filter" << {:"%payload", deps}
            commit << normalize(Π.pattern(selector))
            commit << Normalize.sealed(Π.pattern(successor))

            commit.with(:guarded, true)
            if min > 0
              commit.with(:depth, {:+, {:max, :member, :min}, 1})
            end
          end

          Term.of(normal)
        end
      end

      matchpi %{(head←%'%item _ _*)}, %{(head←%'%item° _ _*)}, cues: {:"%item", :"%item°"} do
        members = pattern.items.move(1)

        normal = Term::Dict.build do |commit|
          commit << head
          commit.concat(members) { |member| normalize(Π.pattern(member)) }

          commit.with(:guarded, true)
          commit.with(:depth, {:+, {:max, :member, :min}, 1})
          commit.with(:bounds, {members.size, :"..=", :"∞"})
        end

        Term.of(normal)
      end

      matchpi %{(%'%items successor_ _ _* ¦ opts_)}, cue: :"%items" do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 1u32)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          continue if max.is_a?(UInt32) && min > max

          members = pattern.items.move(2)

          normal = opts.transaction do |commit|
            commit << :"%items" << Normalize.sealed(Π.pattern(successor))
            commit.concat(members) { |member| normalize(Π.pattern(member)) }

            commit.with(:guarded, true)
            if min > 0
              commit.with(:depth, {:+, {:max, :member, :min}, 1})
              commit.with(:bounds, {members.size, :"..=", :"∞"})
            end
          end

          Term.of(normal)
        end
      end

      matchpi %{(head←%'%entry key_ value_)}, %{(head←%'%entry° key_ value_)}, cues: {:"%entry", :"%entry°"} do
        Term.of(head, Normalize.sealed(Π.pattern(key)), normalize(Π.pattern(value)), depth: {:+, {:max, :member, :min}, 1})
      end

      matchpi %{(%'%entries successor_ k_ v_ ¦ opts_)}, cue: :"%entries" do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 1u32)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          continue if max.is_a?(UInt32) && min > max

          normal = opts.transaction do |commit|
            commit << :"%entries"
            commit << Normalize.sealed(Π.pattern(successor))
            commit << Normalize.sealed(Π.pattern(k))
            commit << normalize(Π.pattern(v))

            commit.with(:depth, {:+, {:max, :member, :min}, 1})
          end

          Term.of(normal)
        end
      end

      matchpi(
        %{(head←%'%leaf _ _* ¦ opts_)},
        %{(head←%'%leaf° _ _* ¦ opts_)},
        cues: {:"%leaf", :"%leaf°"},
      ) do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          part = s.key(:in, value: Term, default: Term.of(:items))
          _ = s.key(:order, value: {:dfs, :bfs}, default: :dfs)
          depth0 = s.key(:self, value: {true, false}, default: false)

          continue unless _ = Tzip::Order.parse?(part)

          members = pattern.items.move(1)

          normal = opts.transaction do |commit|
            commit << head
            commit.concat(members) { |member| normalize(Π.pattern(member)) }

            commit.with(:guarded, true)
            if depth0
              commit.with(:depth, {:max, :member, :min})
            else
              commit.with(:depth, {:+, {:max, :member, :min}, 1})
            end
          end

          Term.of(normal)
        end
      end

      matchpi %{(%'%leaves successor_ _ _* ¦ opts_)}, cue: :"%leaves" do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          part = s.key(:in, value: Term, default: Term.of(:items))
          _ = s.key(:order, value: {:dfs, :bfs}, default: :dfs)
          depth0 = s.key(:self, value: {true, false}, default: false)

          min = s.key(:min, type: UInt32, default: 1u32)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))

          continue unless _ = Tzip::Order.parse?(part)
          continue if max.is_a?(UInt32) && min > max

          members = pattern.items.move(2)

          normal = opts.transaction do |commit|
            commit << :"%leaves" << Normalize.sealed(Π.pattern(successor))
            commit.concat(members) { |member| normalize(Π.pattern(member)) }

            commit.with(:guarded, true)
            if min > 0
              if depth0
                commit.with(:depth, {:max, :member, :min})
              else
                commit.with(:depth, {:+, {:max, :member, :min}, 1})
              end
            end
          end

          Term.of(normal)
        end
      end

      matchpi %{(head←%'%split _ _ _*)}, %{(head←%'%split° _ _ _*)}, cues: {:"%split", :"%split°"} do
        members = pattern.items.move(1)

        normal = Term::Dict.build do |commit|
          commit << head
          commit.concat(members) { |member| normalize(Π.pattern(member)) }

          commit.with(:guarded, true)
          commit.with(:bounds, {members.size - 2, :"..=", :∞})
          # Split left-hand side and right-hand side are synthetic. Thus we can't
          # have a depth guard for them, only for the middle part(s).
          commit.with(:depth, {:+, {:max, {:members, 1, :"..<", members.size - 1}, :min}, 1})
        end

        Term.of(normal)
      end

      matchpi %{(%'%splits successor_ _ _ _* ¦ opts_)}, cue: :"%splits" do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 1)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          continue if max.is_a?(UInt32) && min > max

          members = pattern.items.move(2)

          normal = opts.transaction do |commit|
            commit << :"%splits" << Normalize.sealed(Π.pattern(successor))
            commit.concat(members) { |member| normalize(Π.pattern(member)) }

            commit.with(:guarded, true)
            commit.with(:depth, {:+, {:max, :member, :min}, 1})
          end

          Term.of(normal)
        end
      end

      matchpi %{(%'%matches successor_ subpattern_ ¦ opts_)}, cue: :"%matches" do
        M0.schema(opts) do |s, opts|
          s.on_mismatch { continue }

          min = s.key(:min, type: UInt32, default: 0)
          max = s.key(:max, type: UInt32, default: Term.of(:∞))
          continue if max.is_a?(UInt32) && min > max

          members = pattern.items.move(2)

          normal = opts.transaction do |commit|
            commit << :"%matches" << Normalize.sealed(Π.pattern(successor))
            commit << normalize(Π.pattern(subpattern))

            commit.with(:guarded, true)
            commit.with(:depth, :envelope)
          end

          Term.of(normal)
        end
      end

      # NOTE: Insert new matchpis here, especially if they are infrequent. Below we
      # have raw dict/literal treatment. If your matchpi does not start with a %,
      # make sure to update the dict fast path above.

      matchpi %{_dict} do
        normalize(Π.dict(pattern.as_d))
      end
    end
  end
end
