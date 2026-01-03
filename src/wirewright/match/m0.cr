# `M0` is a small pattern matching engine. It is used to implement `M1`, which
# is Wirewright's main pattern matching engine.
#
# A pattern matching engine in the realm of Wirewright is a bit like a sensory
# organ. A sensory organ sometimes needs its own eyes and ears to do what it
# does -- to sense. So think of M0 as the eyes and ears -- the sensory organs --
# of M1, which is itself a (vastly more intricate) sensory organ for the entirety
# of Wirewright. Instead of perceiving images or sounds, M0 and M1 perceive
# symbols and symbolic structure. They do that in different ways: M0 in a simple
# way, and M1 in an (arguably) more complex way. But the underlying goal stays
# the same: perception of symbolic structure.
#
# M0 implements a subset of constructs from M1. *All* patterns that M0 can match,
# M1 will match; *some* patterns that M1 can match, M0 will match.
#
# The following constructs are supported in M0:
#
# - Literals: `100 "hello" xyzzy true ...`.
# - Blanks (named, unnamed, typed, untyped): `_ _number x_number`.
# - Partition: split a dictionary into its items and pairs partition, for
#   example `(%partition itemspart_ pairspart_)`.
# - Match p1-N on the first N items of an itemspart, correspondingly, possibly
#   with a pairspart; for example `(a b c _*)`, `(+ a_ b_ _* precision: precision_)`.
# - `(%layer _ {...})`: open dictionary entries match. Nothing fancy like optional
#   pairs and the like; use `M0.schema` for more complex pairspart descriptions.
# - `(%literal _)`: match literally, mainly for escaping the above and itself.
# - Recursive application of all of the above and itself on dictionary items
#   and pairs, for example: `(+ a_number 100 x: x_string)`.
#
# As usual, the returned match env contains blank names mapped to the term that
# they have captured. E.g. `(+ a_ b_)` on `(+ 1 2)` will give the match env
# `{a: 1, b: 2}`.
#
# The semantics of using the same-named blank across the pattern to make equality
# constraints is preserved. So for instance matching the pattern `(+ a_ a_)` against
# `(+ x y)` will fail; whereas running the same pattern on `(+ x x)` will succeed
# with the match env `{a: x}`.
#
# M0 exists to simplify the implementation of the compiler for M1, since the latter
# needs to do lots of `Term` pattern matching itself.
#
# As opposed to M1, which does backtracking search at its core, M0 patterns are
# compiled to sequences of instructions that are executed on a tiny ("degenerate"!)
# stack VM. The VM itself uses stack space (as in, vs. heap) for as long as it can,
# spilling to heap only when it absolutely can't fit something on the stack. M0
# mismatches rarely touch the heap even if intermediate captures were made.
#
# Expected runtime of `match?` after compilation is in low hundreds of ns, more
# generally sub-microsecond. Similar runtime is expected of `compile` itself,
# but you'd only need to run it once per pattern anyway, so its performance
# doesn't matter as much.
module Ww::M0
  extend self

  # Matches a term, converts to *type* (see `Term::TypeConversion`).
  def restrict?(term : Term, restriction : Term.class, type)
    term.to?(type)
  end

  # Matches `Term::Any` of type *T*.
  def restrict?(term : Term, restriction : T.class, type : Term.class) forall T
    Term[term].as?(T)
  end

  # Matches `Term::Any` of type *T*, converts to *type* (see `Term::TypeConversion`).
  def restrict?(term : Term, restriction : T.class, type) forall T
    Term[term].as?(T).try(&.to?(type))
  end

  # Matches an integer of the given *type* (via `Term::TypeConversion`), making sure
  # it fits in *range* (of the same integer type, preferably).
  def restrict?(term : Term, restriction : Range, type : Int.class)
    return unless n = term.as_n?
    return unless n.integer?
    return unless int = n.to?(type)

    int.in?(restriction) ? int : nil
  end

  # Matches any term in *restriction* literally, converts to *type*
  # (see `Term::TypeConversion`).
  def restrict?(term : Term, restriction : Indexable, type)
    return unless restriction.any? { |option| Term.of(option) == term }

    term.to?(type)
  end

  # `M0.schema` is a DSL for matching and validating dict pairsparts.
  #
  # *block* requires one parameter, used to refer to the schema. It optionally
  # accepts a second parameter, used to refer to *matchee* extended with defaults.
  #
  # Define expected keys with `var = schema.key(name, value: ..., type?: ..., default?: ...)`
  # anywhere at the block's top-level:
  # - *schema* is the first parameter to *block*.
  # - *name* is the name of the key in *matchee*.
  # - *value* and *type* set the key's value restriction and its target type (see `restrict?`).
  #   *value* is required. *type* is optional, its default value is `Term`.
  # - *default* provides the default value. It is optional. If absent, absence of *key* will
  #   result in mismatch. *default* does not have to fulfill the restriction set by *value*;
  #   it does not have to be of *type*.
  # - *var* will store the resulting object. Its type is the union of *type* and *default*'s type.
  #
  # Use `_` instead of *var* to discard the value.
  #
  # Define a custom mismatch handler with `s.on_mismatch { ... }` anywhere at the block's
  # top-level. The mismatch handler is executed in the block surrounding `M0.schema`, so
  # any `next` or `break` is interpreted by the block surrounding `M0.schema` (i.e., `M0.schema`
  # and `on_mismatch` are "expanded out of the way").
  #
  # The default mismatch handler is `on_mismatch { next }`.
  #
  # ```
  # M0.schema(opts) do |s, ext|
  #   s.on_mismatch do
  #     raise "Mismatch!"
  #   end
  #
  #   _ = s.key(:in, value: {:items, :values, :pairs}, default: :items)
  #   _ = s.key(:order, value: {:dfs, :bfs}, default: :dfs)
  #   min = s.key(:min, type: UInt8, value: 0u8..UInt8::MAX, default: 0)
  #   max = s.key(:max, type: UInt8, value: 1u8..UInt8::MAX, default: :infinity)
  #   _ = s.key(:self, value: {true, false}, default: false)
  #
  #   pp typeof(min) # UInt8
  #   pp typeof(max) # UInt8 | Symbol
  #
  #   pp ext # opts extended with defaults
  # end
  # ```
  #
  # *open* allows pairs other than those specified in the schema.)
  macro schema(matchee, *, open = false, &block)
    {%
      unless block && {1, 2}.includes?(block.args.size)
        raise "expected a block with one or two argument(s)"
      end

      keys = [] of ::NoReturn
      others = [] of ::NoReturn
      mismatch = nil

      ref = block.args[0]
      extended = block.args[1]

      nodes = block.body.is_a?(Expressions) ? block.body.expressions : [block.body]
      nodes.each do |node|
        if node.is_a?(Assign) && node.value.is_a?(Call) && node.value.receiver.id == ref.id
          call = node.value
          if call.name == :key
            # const
            unrecognized = "unrecognized call, expected: key(name : MacroId, *, value : ASTNode = Term, type : ASTNode = Term, default : ASTNode? = nil)"

            unless call.args.size == 1
              call.raise unrecognized
            end

            name = call.args[0]

            kw_value = call.named_args.find { |kwarg| kwarg.name == :value }
            kw_type = call.named_args.find { |kwarg| kwarg.name == :type }
            kw_default = call.named_args.find { |kwarg| kwarg.name == :default }

            arity = 0
            arity += 1 if kw_value
            arity += 1 if kw_type
            arity += 1 if kw_default

            unless call.named_args.size == arity
              call.raise unrecognized
            end

            keys << {
              key:         name.id,
              var:         node.target.is_a?(Underscore) ? nil : node.target.id,
              restriction: kw_value ? kw_value.value : Term,
              type:        kw_type ? kw_type.value : Term,
              default:     kw_default ? {kw_default.value} : nil,
            }
          else
            call.raise "unrecognized schema assign-call name: #{call.name}"
          end
        elsif node.is_a?(Call) && node.receiver && node.receiver.id == ref.id
          if node.name == :on_mismatch
            unless node.block
              node.raise "expected a block"
            end

            if mismatch
              node.raise "duplicate mismatch"
            end

            mismatch = node.block.body
          else
            node.raise "unrecognized schema call name: #{node.name}"
          end
        else
          # Unrecognized node in the block.
          others << node
        end
      end
    %}

    unless %matchee = ({{matchee}}).as_d?
      {% if mismatch %}\
        {{mismatch}}
        unreachable("M0.schema's on_mismatch must be NoReturn")
      {% else %}\
        next
      {% end %}\
    end

    %arity = 0

    {% for key, i in keys %}\
      if %value{i} = %matchee[{{key[:key].symbolize}}]?
        unless %value{i} = {{@type}}.restrict?(%value{i}, {{key[:restriction]}}, {{key[:type]}})
          {% if mismatch %}\
            {{mismatch}}
            unreachable("M0.schema's on_mismatch must be NoReturn")
          {% else %}\
            next
          {% end %}\
        end
        %value{i} = { %value{i} }
        %arity += 1
      else
        {% if default = key[:default] %}\
          %value{i} = {{default}}
          {% if extended %}\
            {{extended}} = {{extended}}.with({{key[:key].symbolize}}, *%value{i})
          {% end %}\
        {% end %}\
      end

      unless %value{i}
        {% if mismatch %}\
          {{mismatch}}
          unreachable("M0.schema's on_mismatch must be NoReturn")
        {% else %}\
          next
        {% end %}\
      end

      {% if key[:var] %}\
        {{key[:var]}}, *_ = %value{i}
      {% end %}\
    {% end %}\

    {% unless open %}\
      unless %arity == %matchee.pairsize
        {% if mismatch %}\
          {{mismatch}}
          unreachable("M0.schema's on_mismatch must be NoReturn")
        {% else %}\
          next
        {% end %}\
      end
    {% end %}\

    {% for node in others %}\
      {{node}}
    {% end %}\
  end

  # :nodoc:
  alias Insn = AssertEqual |
               Partition |
               Fetch |
               Drop |
               AssertSubtype |
               Assign |
               AssertSize |
               AssertItemsizeAtLeast |
               AssertPairsizeAtLeast |
               AssertPairsize

  # :nodoc:
  #
  # Pop term, assert dict, push itemspart followed by pairspart.
  defrecord Partition

  # :nodoc:
  #
  # Keep term, assert dict, assert has *key*, push value of *key*.
  defrecord Fetch, key : Term

  # :nodoc:
  #
  # Pop term.
  defrecord Drop

  # :nodoc:
  #
  # Pop term, assert env *capture* absent or equal to term, add capture to env.
  defrecord Assign, capture : Term::Sym

  # :nodoc:
  #
  # Pop term, assert equal to *term*.
  defrecord AssertEqual, term : Term

  # :nodoc:
  #
  # Keep term, assert its type is subtype of *type*.
  defrecord AssertSubtype, type : TermType

  # :nodoc:
  #
  # Keep term, assert dict, assert size equal to *size*.
  defrecord AssertSize, size : Int32

  # :nodoc:
  #
  # Keep term, assert dict, assert itemsize greater than or equal to *itemsize*.
  defrecord AssertItemsizeAtLeast, itemsize : Int32

  # :nodoc:
  #
  # Keep term, assert dict, assert pairsize greater than or equal to *itemsize*.
  defrecord AssertPairsizeAtLeast, pairsize : Int32

  # :nodoc:
  #
  # Keep term, assert dict, assert pairsize equal to *pairsize*.
  defrecord AssertPairsize, pairsize : Int32

  # :nodoc:
  SYM_LITERAL = Term[:"%literal"]

  # :nodoc:
  SYM_PARTITION = Term[:"%partition"]

  # :nodoc:
  SYM_LAYER = Term[:"%layer"]

  # :nodoc:
  SYM_UNDERSCORE = Term[:_]

  # :nodoc:
  SYM_UNDERSCORE_STAR = Term[:"_*"]

  # Returns `true` if *term* is the pass blank `_`.
  private def pass?(term : Term) : Bool
    return false unless sym = term.as_sym?
    return false unless blank = sym.blank?
    return false unless blank.singular?
    return false if blank.typed? || blank.named?

    true
  end

  # x_ x_string qux
  private def compile(insns, pattern : Term::Sym) : Nil
    if (blank = pattern.blank?) && blank.singular?
      # <matchee> ⏏
      if blank.typed?
        insns << AssertSubtype.new(blank.type)
        # <matchee> ⏏
      end
      if name = blank.name?
        insns << Assign.new(name)
      else
        # Pass (`_`)
        insns << Drop.new
      end
      # ⏏
      return
    end

    # <matchee> ⏏
    insns << AssertEqual.new(Term.of(pattern))
    # ⏏
  end

  # (+ a_ b_) (%literal 100)
  private def compile(insns, pattern : Term::Dict) : Nil
    if pattern.itemsize > 0
      case {pattern[0], pattern.itemsize - 1, pattern.pairsize}
      when {SYM_LITERAL, 1, 0} # (%literal 123)
        # <matchee> ⏏
        insns << AssertEqual.new(pattern[1])
        # ⏏
        return
      when {SYM_PARTITION, 2, 0} # (%partition itemspart_ pairspart_)
        # <matchee> ⏏
        insns << Partition.new
        # <itemspart> <pairspart> ⏏
        compile(insns, pattern[2])
        # <itemspart> ⏏
        compile(insns, pattern[1])
        # ⏏
        return
      when {SYM_LAYER, 2, 0}
        # <matchee> ⏏
        if (pattern[1] == SYM_UNDERSCORE) && (selector = pattern[2].as_d?)
          # <matchee> ⏏
          insns << AssertPairsizeAtLeast.new(selector.size)
          # <matchee> ⏏

          # (%layer _ {a: a_, b: 200})
          selector.each_entry do |key, value|
            # <matchee> ⏏
            insns << Fetch.new(key)
            # <matchee> <value of key> ⏏
            compile(insns, value)
            # <matchee> ⏏
          end

          # <matchee> ⏏
          insns << Drop.new
          # ⏏
          return
        end
      end

      if pattern.ends_with?(SYM_UNDERSCORE_STAR)
        # (+ 1 2 _*)
        # (+ 1 2 _* x: 100 y: y_)
        prior = pattern.items.grow(-1)

        # <matchee> ⏏
        insns << AssertItemsizeAtLeast.new(prior.size)
        # <matchee> ⏏

        prior.each_with_index do |item, index|
          next if pass?(item) # Don't waste resources on `_`

          # <matchee> ⏏
          insns << Fetch.new(Term.of(index))
          # <matchee> <value at index> ⏏
          compile(insns, item)
          # <matchee> ⏏
        end

        if pattern.pairsize > 0
          insns << AssertPairsize.new(pattern.pairsize)

          pattern.each_pair do |key, value|
            next if pass?(value) # Don't waste resources on `_`

            # <matchee> ⏏
            insns << Fetch.new(key)
            # <matchee> <value of key> ⏏
            compile(insns, value)
            # <matchee> ⏏
          end
        end

        # <matchee> ⏏
        insns << Drop.new
        # ⏏
        return
      end
    end

    # <matchee> ⏏
    insns << AssertSize.new(pattern.size)
    # <matchee> ⏏

    pattern.each_entry do |key, value|
      next if pass?(value) # Don't waste resources on `_`

      # <matchee> ⏏
      insns << Fetch.new(key)
      # <matchee> <value of key> ⏏
      compile(insns, value)
      # <matchee> ⏏
    end

    # <matchee>
    insns << Drop.new
    # ⏏
  end

  # 100 "hello" true
  private def compile(insns, pattern : Term::Any) : Nil
    # <matchee> ⏏
    insns << AssertEqual.new(Term.of(pattern))
    # ⏏
  end

  private def compile(insns, pattern : Term) : Nil
    compile(insns, Term[pattern])
  end

  # :nodoc:
  CACHE = SyncCache(Term, Slice(Insn)).new(512, preallocate: true)

  # Compiles an M0 pattern into a sequence of M0 instructions.
  def compile(pattern : Term) : Slice(Insn)
    CACHE.put_if_absent(pattern) do
      # There usually aren't a lot of instructions. So we can use stack space.
      # This lets us know, later on, the exact amount of memory to allocate,
      # which is neat.
      insns = Pf::Kit.stack_array(Insn, 64)
      compile(insns, pattern)

      insns.to_readonly_slice(&.itself)
    end
  end

  # Matches *matchee* against a sequence of M0 instructions *insns* and
  # a match *env*.
  def match?(env : Term::Dict, insns : Slice(Insn), matchee matchee0 : Term) : Term::Dict?
    stack = Pf::Kit.stack_array(Term, 32)
    stack << matchee0

    # NOTE: In practice, the amount of captures in a pattern is *tiny*. I mean it: 99%
    # of the time it's <16, most of them well below 16, like, 2, 4, up to 8 if you're
    # lucky. Only generated patterns could have more than 16, or very large hand-written
    # ones whose performance will dwarf the overhead of heap alloc or GC.
    captures = Pf::Kit.stack_array({Term::Sym, Term}, 16)

    # Bloom filter for capture names.
    filter = 0u64

    insns.each do |insn|
      case insn
      in AssertEqual
        matchee = stack.pop
        return unless matchee == insn.term
      in AssertSubtype
        return unless stack.last.type.subtype?(insn.type)
      in AssertSize
        return unless dict = stack.last.as_d?
        return unless dict.size == insn.size
      in AssertItemsizeAtLeast
        return unless dict = stack.last.as_d?
        return unless dict.itemsize >= insn.itemsize
      in AssertPairsizeAtLeast
        return unless dict = stack.last.as_d?
        return unless dict.pairsize >= insn.pairsize
      in AssertPairsize
        return unless dict = stack.last.as_d?
        return unless dict.pairsize == insn.pairsize
      in Assign
        matchee = stack.pop

        slot = Term.hashcode(insn.capture) % 64
        mask = 1u64 << slot

        if filter & mask == 0u64 # Definitely not in captures.
          filter |= mask
          captures << {insn.capture, matchee}
          next
        end

        # Possibly in captures.
        found = false
        captures.reverse_each do |capture, value|
          next unless capture == insn.capture

          unless value == matchee
            # As in `(+ a_ a_)`, `(+ 1 2)`.
            return
          end

          found = true
          break
        end

        next if found

        captures << {insn.capture, matchee}
      in Partition
        matchee = stack.pop
        return unless dict = matchee.as_d?

        stack << Term.of(dict.itemspart)
        stack << Term.of(dict.pairspart)
      in Fetch
        matchee = stack.last
        return unless dict = matchee.as_d?
        return unless value = dict[insn.key]?

        stack << value
      in Drop
        _ = stack.pop
      end
    end

    Term.entries(captures)
  end

  # Matches *pattern* against *matchee*. Returns the resulting match env on
  # match. Returns `nil` on mismatch.
  def match?(pattern : Term, matchee : Term, *, env = Term[]) : Term::Dict?
    match?(env, compile(pattern), matchee)
  end

  # Wraps the output of `match?` in an array.
  #
  # M0 cannot give you more than one match env like M1 can, it can only give you zero
  # or one. This function is simply a shim for API compatibility should one call
  # `matches` instead of `match?`.
  def matches(pattern : Term, matchee : Term, *, env = Term[]) : Array(Term::Dict)
    env = match?(pattern, matchee, env: env)
    env ? [env] : [] of Term::Dict
  end
end
