module Ww::ML
  # Contains the many overloads of `render`, which turn nodes from `Tree`
  # into `Tsrc`s.
  #
  # A `Tsrc` is short for *term-source map*, that is, a pair where the first element
  # is a term, and the second one is a source map mapping termpaths (`Tpath`s) into
  # that term to views of the original source code. In practice, `Tsrc` is either
  # `UntrackedTsrc` or `TrackedTsrc`.
  module Renderer(Tsrc)
    extend self

    # :nodoc:
    defcase RenderContext(Tsrc),
      location : StringView?,
      rule_id : Term::Sym?,
      rule_block_id : Term::Sym?,
      ids : Hash(Tree::PlaceholderContainer, Term::Sym),
      cache : Hash(Tree::ExprNode, Tsrc)

    # Raised when rendering fails.
    class RenderError < Exception
      getter detail : String
      getter? text : StringView?

      def initialize(@detail, @text)
      end
    end

    private def raise(detail : String, blame : StringView)
      raise RenderError.new(detail, text: blame)
    end

    private def raise(detail : String, blame : Tree::Location)
      raise RenderError.new(detail, text: blame.text)
    end

    private def raise(detail : String, blame)
      raise RenderError.new(detail, text: nil)
    end

    # :nodoc:
    #
    # This is certainly not an exception used for control flow!
    class RequiresRuleId < Exception
      getter? blame : StringView?

      def initialize(@blame)
        @callstack = CallStack.empty
      end
    end

    # :nodoc:
    #
    # This is certainly not an exception used for control flow!
    class RequiresRuleBlockId < Exception
      getter? blame : StringView?

      def initialize(@blame)
        @callstack = CallStack.empty
      end
    end

    # :nodoc:
    defrecord DictNop
    # :nodoc:
    defrecord DictAppend(Tsrc), item : Tsrc
    # :nodoc:
    defrecord DictAssign(Tsrc), blame : Tree::Expr | StringView, key : Tsrc, value : Tsrc

    private def nop
      DictNop.new
    end

    private def append(item : Tsrc)
      DictAppend.new(item)
    end

    private def assign(adjoin, key : Tsrc, value : Tsrc)
      DictAssign.new(adjoin, key, value)
    end

    private def dict(commit, action : DictNop)
    end

    private def dict(commit, action : DictAppend)
      commit << action.item
    end

    private def dict(commit, action : DictAssign)
      if action.key.in?(commit)
        raise "duplicate key `#{ML.compact(action.key.term)}` in dict", action.blame
      end

      commit.with(action.key, action.value)
    end

    private def dict(commit, objects : Enumerable, &) : Nil
      objects.each do |object|
        dict(commit, (yield object))
      end
    end

    private def dict(ctx : RenderContext, objects : Enumerable, &) : Tsrc
      tsrc(ctx) do |commit|
        dict(commit, objects) { |object| yield object }
      end
    end

    private def dict(ctx : RenderContext, *enumerables : Enumerable, &) : Tsrc
      tsrc(ctx) do |commit|
        enumerables.each do |objects|
          dict(commit, objects) { |object| yield object }
        end
      end
    end

    # Do not use exceptions for control flow!
    class RequiresRuleId < Exception
      getter? blame : StringView?

      def initialize(@blame)
        @callstack = CallStack.empty
      end
    end

    # Do not use exceptions for control flow!
    class RequiresRuleBlockId < Exception
      getter? blame : StringView?

      def initialize(@blame)
        @callstack = CallStack.empty
      end
    end

    private def id(node : Tree::DictRule) : Term::Sym
      hash = Tree.hashcode(node)

      Term::Sym.new(Alpha48.encode(hash.blk0))
    end

    private def id(node : Tree::DictEntryBlock) : Term::Sym
      hashes = [] of Term::H256

      node.entries.each do |tg|
        # Whether an entry is disabled or enabled does not matter, so that hashes
        # are stable e.g. while debugging with focus on some rule.
        entry = tg.entry

        # Filter in on DictRules so that entries and items don't matter; only
        # rules should.
        next unless entry = Tree.topmost?(entry, as: Tree::DictRule)

        hashes << Tree.hashcode(entry)
      end

      # Sort so that rule order doesn't matter.
      hashes.sort!

      hash = Term::H256.combine(hashes)

      Term::Sym.new(Alpha48.encode(hash.blk0))
    end

    private def id(ctx : RenderContext, node : Tree::PlaceholderContainer)
      ctx.ids.put_if_absent(node) { id(node) }
    end

    private def id?(ctx : RenderContext, node : Tree::PlaceholderContainer)
      ctx.ids[node]?
    end

    private def tsrc(ctx : RenderContext, object, *, src = nil)
      Tsrc.of(ctx.location, object, src: src.as?(Tree::Location).try(&.text))
    end

    private def tsrc(ctx : RenderContext, &)
      Tsrc.build(ctx.location) { |commit| yield commit }
    end

    private def render0(ctx : RenderContext, node : Tree::Leaf) : Tsrc
      tsrc(ctx, node.term)
    end

    private def render0(ctx : RenderContext, node : Tree::Stitch) : Tsrc
      case node.segments.size
      when 0 # ""
        tsrc(ctx, "")
      when 1 # "foo" "⸢x⸣"
        segment, *_ = node.segments

        term = render(ctx, segment)
        if term.type.string?
          # "foo"
          return term
        end

        # "⸢x⸣"
        tsrc(ctx, {:~, term})
      else
        # "foo ⸢bar⸣ baz"
        tsrc(ctx) do |commit|
          commit << tsrc(ctx, :~)
          commit.concat(node.segments) { |segment| render(ctx, segment) }
        end
      end
    end

    private def render0(ctx : RenderContext, node : Tree::Itemspattern) : Tsrc
      itemspart = tsrc(ctx) do |commit|
        commit.concat(node.items) { |item| render(ctx, item) }
      end

      tsrc(ctx, {:"%partition", itemspart, :_})
    end

    private def render0(ctx : RenderContext, node : Tree::SameValueSet) : Tsrc
      dict(ctx, node.items) do |item|
        assign(item, render(ctx, item), tsrc(ctx, node.value))
      end
    end

    private def render0(ctx : RenderContext, node : Tree::Multiset) : Tsrc
      # TODO: multisets can have duplicate keys (like `1` in `{# 1 1 2 3}`).
      # Moreover, it is unclear where their values -- the tallies -- come from,
      # conceptually, and what they're associated with (`{# a a b c}` ->
      # `{a: 2, b: 1, c: 1}`). So the location business is unclar.
      tsrc(ctx) do |commit|
        node.entries.each do |entry|
          case entry
          in Tree::MultisetEntryOne
            delta = Term[1]
            key = render(ctx, entry.item)
          in Tree::MultisetEntryN
            delta = render(ctx, entry.count)
            unless delta = delta.term.as_n?
              raise "expected a number for count", entry.count
            end

            key = render(ctx, entry.item)
          end

          tally = commit[key]? || Term[0]

          commit.with(key, tally + delta)
        end
      end
    end

    private def render0(ctx : RenderContext, node : Tree::Keypool) : Tsrc
      tsrc(ctx) do |commit|
        commit << tsrc(ctx, :"%keypool")
        commit.concat(node.items) { |item| render(ctx, item) }
      end
    end

    private def render0(ctx : RenderContext, node : Tree::AllItem) : Tsrc
      if node.items.empty?
        raise "empty all-item makes no sense", ctx.location
      end

      pairside = render(ctx, node.pairside)

      head = node.source ? Term.of(:"%item°") : Term.of(:"%item")
      args = [] of Tsrc

      node.items.each do |item|
        args << tsrc(ctx, {tsrc(ctx, head), render(ctx, item)})
      end

      if pairside
        # ⟨& qux ¦ x_⟩ -> (%all (%item qux ...) (%partition _ x_))
        args << tsrc(ctx, {:"%partition", :_, pairside})
      end

      # ⟨& x⟩
      if args.size == 1
        return args[0]
      end

      tsrc(ctx) do |commit|
        commit << tsrc(ctx, :"%all")
        commit.concat(args)
      end
    end

    private def render0(ctx : RenderContext, node : Tree::AllLeaf) : Tsrc
      if node.items.empty?
        raise "empty all-leaf makes no sense", ctx.location
      end

      pairside = render(ctx, node.pairside)

      head = node.source ? Term.of(:"%leaf°") : Term.of(:"%leaf")
      args = [] of Tsrc

      node.items.each do |item|
        args << tsrc(ctx) do |commit|
          commit << tsrc(ctx, head)
          commit << render(ctx, item)
          commit.with(:self, true)
          unless node.source
            commit.with(:order, :bfs)
          end
        end
      end

      if pairside
        # ⟪qux ¦ x_⟫ -> (%all (%leaf qux ...) (%partition _ x_))
        args << tsrc(ctx, {:"%partition", :_, pairside})
      end

      # ⟪x⟫
      if args.size == 1
        return args[0]
      end

      tsrc(ctx) do |commit|
        commit << tsrc(ctx, :"%all")
        commit.concat(args)
      end
    end

    private def render0(ctx : RenderContext, node : Tree::PairDict) : Tsrc
      dict(ctx, node.pairs) { |pair| render(ctx, pair) }
    end

    private def render0(ctx : RenderContext, node : Tree::Split) : Tsrc
      assert node.parts.present?

      unless node.parts.size == 1
        raise "%split shorthand family not available yet", node
      end

      node.parts.each do |part|
        assert child = Tree.topmost(part, as: Tree::SplitPartNode)
        next if child.items.present?

        raise "expected at least one item in split part", part
      end

      if pairside = node.pairside
        pairspart = render(ctx, pairside)

        # ⟨a b c ⍊ x: 100⟩
        decorate = ->(itemspart : Tsrc) do
          tsrc(ctx, {:"%partition", itemspart, pairspart})
        end
      else
        # ⟨a b c⟩
        decorate = ->(itemspart : Tsrc) { itemspart }
      end

      part = render(ctx, node.parts[0])

      interior = tsrc(ctx) do |commit|
        commit << tsrc(ctx, node.source ? :"%item°" : :"%item")
        commit.concat(part)
      end

      decorate.call(interior)
    end

    private def render0(ctx : RenderContext, node : Tree::SplitPartNode) : Array(Tsrc)
      render(ctx, node.items)
    end

    private def render0(ctx : RenderContext, node : Tree::Edge) : Tsrc
      tsrc(ctx, {:edge, render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::Up) : Tsrc
      tsrc(ctx, {:"$up", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::Dn) : Tsrc
      tsrc(ctx, {:"$down", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::My) : Tsrc
      tsrc(ctx, {:"$my", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::AlloyExpr) : Tsrc
      tsrc(ctx, {:^, render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::AlloySpliceExpr) : Tsrc
      tsrc(ctx, {:"^*", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::Literal) : Tsrc
      tsrc(ctx, {:literal, render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::Dollar) : Tsrc
      tsrc(ctx, {:"$", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::DollarOnce) : Tsrc
      tsrc(ctx, {:"$once", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::Positive) : Tsrc
      arg = render(ctx, node.arg)
      if arg.type.number?
        return arg
      end

      tsrc(ctx, {:+, arg})
    end

    private def render0(ctx : RenderContext, node : Tree::Negative) : Tsrc
      arg = render(ctx, node.arg)
      if n = arg.term.as_n?
        return tsrc(ctx, -n)
      end

      tsrc(ctx, {:-, arg})
    end

    private def render0(ctx : RenderContext, node : Tree::PatternLet) : Tsrc
      tsrc(ctx, {:"%let", render(ctx, node.left), render(ctx, node.right)})
    end

    private def render0(ctx : RenderContext, node : Tree::PatternLiteral) : Tsrc
      tsrc(ctx, {:"%literal", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::PatternNonself) : Tsrc
      tsrc(ctx, {:"%nonself", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::PatternSlot) : Tsrc
      tsrc(ctx, {:"%slot", render(ctx, node.arg)})
    end

    private def render0(ctx : RenderContext, node : Tree::PatternNumber) : Tsrc
      tsrc(ctx, {:"%let", render(ctx, node.arg), :_number})
    end

    defrecord NonBlank
    defrecord TypedBlank, name : Term::Sym, type : TermType

    private def possible_blank(blame, key : Term)
      return NonBlank.new unless keysym = key.as_sym?
      return NonBlank.new unless blank = keysym.blank?

      unless name = blank.name?
        raise "expected a named blank", blame
      end

      unless blank.singular?
        raise "expected a singular blank (but what you've got here is a polyblank)", blame
      end

      TypedBlank.new(name, blank.type)
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorKey)
      key = render(ctx, node.key)

      case blank = possible_blank(node.key, key.term)
      in NonBlank
        # {¦ xyz}
        #
        # xyz -> xyz: _
        assign(node.key, key, tsrc(ctx, :_))
      in TypedBlank
        # {¦ xyz_number}
        #
        # xyz_<type> -> xyz: xyz_<type>
        assign(node.key, tsrc(ctx, blank.name), key)
      end
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorMaybeKey)
      key = render(ctx, node.key)

      # {¦ ⋮x} -> xyz: (%- (%never) xyz)
      assign(node.key, key, tsrc(ctx, {:"%-", {:"%never"}, key}))
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorLetTrue)
      key = render(ctx, node.key)

      # {¦ x⁺}
      #
      # x⁺ -> x: x←true
      assign(node.key, key, tsrc(ctx, {:"%let", key, true}))
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorLetFalse)
      key = render(ctx, node.key)

      # {¦ x⁻}
      #
      # x⁻ -> x: x←false
      assign(node.key, key, tsrc(ctx, {:"%let", key, false}))
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorPairDefault)
      key = render(ctx, node.key)
      value = render(ctx, node.value)

      case blank = possible_blank(node.key, key.term)
      in NonBlank
        # x⋮ y -> x: (%optional y x←_<type of y>)
        if keysym = key.term.as_sym?
          capture = tsrc(ctx, Term::Sym.blank(keysym, value.type), src: node.key)
        else
          capture = tsrc(ctx, {:"%let", key, value.type.blank})
        end

        assign(node.key, key, tsrc(ctx, {:"%optional", value, capture}))
      in TypedBlank
        if blank.type.any?
          # x_⋮ y -> x: (%optional y x_)
          return assign(node.key,
            tsrc(ctx, blank.name, src: node.key),
            tsrc(ctx, {:"%optional", value, key}))
        end

        # x_<type>⋮ y -> x: (%optional <default for type> y)
        assign(node.key,
          tsrc(ctx, blank.name, src: node.key),
          tsrc(ctx, {:"%optional", ML.initial(blank.type), value}))
      end
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorPairRequired)
      key = render(ctx, node.key)
      value = render(ctx, node.value)

      case blank = possible_blank(node.key, key.term)
      in NonBlank
        # x: y
        assign(node.key, key, value)
      in TypedBlank
        name = tsrc(ctx, blank.name, src: node.key)

        Term.case(value.term, engine: M0) do
          # Rewrite x_: (%optional · ·) to x: (%optional · x←·) to avoid confusion.
          match({:"%optional", :_, :_}, cue: :"%optional") do
            optional, default, pattern = value

            if blank.type.any?
              # x_: (%optional 0 (%number _ < 128))
              #   -> x: (%optional 0 (%let x (%number _ < 128)))
              body = pattern
            else
              # x_number: (%optional 0 (%number _ < 128))
              #   -> x: (%optional 0 (%let x (%all _number (%number _ < 128))))
              body = tsrc(ctx, {:"%all", tsrc(ctx, blank.type.blank, src: node.key), pattern})
            end

            return assign(node.key, name, tsrc(ctx, {optional, default, {:"%let", name, body}}))
          end

          otherwise { }
        end

        if blank.type.any?
          # x_: (%number _ > 100) -> x: (%let x (%number _ > 100))
          assign(node.key, name, tsrc(ctx, {:"%let", name, value}))
        else
          # x_number: (%number _ > 100) -> x: (%all x_number (%number _ > 100))
          assign(node.key, name, tsrc(ctx, {:"%all", key, value}))
        end
      end
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorNegative)
      key = render(ctx, node.key)
      capture = render(ctx, node.capture)

      case blank = possible_blank(node.key, key.term)
      in NonBlank
        if capture
          # -a: foo -> a: (%- _ foo)
          assign(node.key, key, tsrc(ctx, {:"%-", :_, capture}))
        else
          # -a -> a: (%- _)
          assign(node.key, key, tsrc(ctx, {:"%-", :_}))
        end
      in TypedBlank
        name = tsrc(ctx, blank.name, src: node.key)

        # -a_number -> a: (%- _number a)
        # -a_number: qux -> a: (%- _number qux)
        assign(node.key, name, tsrc(ctx, {:"%-", tsrc(ctx, blank.type.blank, src: node.key), capture || name}))
      end
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorNumber)
      key = render(ctx, node.key)
      rename = render(ctx, node.rename)

      # ±x -> x: x←_number
      # ±x: foo -> x: foo←_number
      assign(node.key, key, tsrc(ctx, {:"%let", rename || key, :_number}))
    end

    private def render0(ctx : RenderContext, node : Tree::SelectorEdge)
      key = render(ctx, node.key)

      case blank = possible_blank(node.key, key.term)
      in NonBlank
        # @x -> @x: _
        assign(node.key, tsrc(ctx, {:edge, key}), tsrc(ctx, :_))
      in TypedBlank
        # @x_ -> x: @x_
        # @x_number -> x: @x_number
        assign(node.key, tsrc(ctx, blank.name, src: node.key), tsrc(ctx, {:edge, key}))
      end
    end

    private def render0(ctx : RenderContext, node : Tree::Pairspattern) : Tsrc
      # {¦} -> _dict
      if node.selection.empty?
        return tsrc(ctx, :_dict)
      end

      selection = dict(ctx, node.selection) do |selector|
        render(ctx, selector)
      end

      if itemsname = render(ctx, node.itemsname)
        # {M¦ a_ b_}
        return tsrc(ctx, {:"%partition", {:"%let", itemsname, :_}, {:"%layer", :_, selection}})
      end

      # {¦ a_ b_}
      tsrc(ctx, {:"%layer", :_, selection})
    end

    private def render0(ctx : RenderContext, node : Tree::EntriesPattern) : Tsrc
      # {|} -> {}
      if node.selection.empty?
        return tsrc(ctx, Term[])
      end

      # {| a_ b_}
      dict(ctx, node.selection) { |selector| render(ctx, selector) }
    end

    # NOTE: `Layer` always appears in (%partition _ ⏏) or similarly in other pairside
    # contexts; it is not a free-floating node. This is enforced by the type grammar.
    private def render0(ctx : RenderContext, node : Tree::Layer)
      residue = render(ctx, node.residue)
      selection = dict(ctx, node.selection) { |selector| render(ctx, selector) }

      # (¦) -> (%partition () ()) -> ()
      if residue.nil? && selection.term.empty?
        return tsrc(ctx, Term.of)
      end

      # {¦} -> _dict
      if residue.try(&.term) == Term.of(:_) && selection.term.empty?
        return tsrc(ctx, :_dict)
      end

      # (¦ a: 1 b: 2) -> {a: 1, b: 2}
      # (¦ () a: 1 b: 2) -> {a: 1, b: 2}
      if residue.nil? || residue.term == Term.of
        return selection
      end

      # (¦ x_) -> x_
      #
      # NOTE: remember we're always (%partition · ⏏) or in similar contexts, so
      # we can be sure it's a dict. Although this particular reduction still
      # looks sketchy AF... It's used in many places especially with M0, so
      # unfortunately it must be kept.
      if selection.term.empty?
        return residue
      end

      tsrc(ctx, {:"%layer", residue, selection})
    end

    private def render0(ctx : RenderContext, node : Tree::LayerIgnoreResidue)
      tsrc(ctx, {:"%layer", :_, render(ctx, node.child)})
    end

    private def render0(ctx : RenderContext, node : Tree::Dict)
      itemside = render(ctx, node.itemside)
      pairside = render(ctx, node.pairside)

      if itemside.term.empty? && (pairside.nil? || pairside.type.dict? && pairside.term.empty?)
        # () (¦) -> ()
        tsrc(ctx, Term.of)
      elsif itemside.term.itemsonly? && pairside
        # (circle x_ y_ ⍊ ±radius)
        tsrc(ctx, {:"%partition", itemside, pairside})
      elsif pairside
        # %all+%layer is somewhat nastier on the pattern matching side
        # (might sometimes process more entries); but it is a good fallback
        # if the client gives us something that has pairs *and* a pairspattern,
        # as in: `(point x: 0 ¦ _ -y)`, which could be useful if the client wants
        # to avoid pairspattern treatment of certain keys or values.
        tsrc(ctx, {:"%all", itemside, pairside})
      else
        # (point 1 2)
        # (point x: 100 y: 200)
        itemside
      end
    end

    private def render0(ctx : RenderContext, node : Tree::DictExtend)
      itemside = render(ctx, node.itemside)
      extra = render(ctx, node.extra)

      # (point ^x ^y ^… rest) -> (^extend (point ^x ^y) rest)
      tsrc(ctx, {:"^extend", itemside, extra})
    end

    # :nodoc:
    @[Flags]
    enum DictCategory
      Item
      Pair
      Rule
    end

    private def category(node : Tree::Location)
      category(node.child)
    end

    private def category(node : Tree::DictItem)
      DictCategory::Item
    end

    private def category(node : Tree::DictPair)
      DictCategory::Pair
    end

    private def category(node : Tree::DictRule)
      DictCategory::Rule
    end

    private def render0(ctx : RenderContext, node : Tree::DictSection) : Tsrc
      rules = false
      focused = DictCategory::None

      node.blocks.each do |block|
        block.entries.each do |tg|
          rules ||= !!Tree.topmost?(tg.entry, as: Tree::DictRule)
          next unless tg.is_a?(Tree::FocusedEntry)

          focused |= category(tg.entry)
        end
      end

      tsrc(ctx) do |commit|
        node.blocks.each do |block|
          block.entries.each do |tg|
            case tg
            in Tree::DisabledEntry
              next
            in Tree::EnabledEntry
              next if category(tg.entry).in?(focused)
            in Tree::FocusedEntry
            end

            if rules
              # NOTE: it is of paramount importance that we set rule_block_id in all
              # cases, to support nesting (the closest enclosing id wins).
              begin
                action = render(ctx.copy_with(rule_block_id: id?(ctx, block)), tg.entry)
              rescue RequiresRuleBlockId
                id = id(ctx, block)
                action = render(ctx.copy_with(rule_block_id: id), tg.entry)
              end
            else
              action = render(ctx, tg.entry)
            end

            dict(commit, action)
          end
        end
      end
    end

    private def render0(ctx : RenderContext, entry : Tree::DictItem)
      item = render(ctx, entry.item)

      DictAppend.new(item)
    end

    private def render0(ctx : RenderContext, entry : Tree::DictKVPair)
      key = render(ctx, entry.key)
      value = render(ctx, entry.value)

      DictAssign.new(entry.key, key, value)
    end

    private def render0(ctx : RenderContext, entry : Tree::DictAlloyPair)
      key = render(ctx, entry.key)

      if key.type.symbol?
        # ^:x -> x: ^x
        id = String.build do |io|
          io << "^"
          ML.compact(io, key.term)
        end
        value = tsrc(ctx, Term::Sym.new(id))
      else
        # ^:(+ 1 2) -> (+ 1 2): ^(+ 1 2)
        value = tsrc(ctx, {:"^", key})
      end

      DictAssign.new(entry.key, key, value)
    end

    private def render0(ctx : RenderContext, entry : Tree::DictEdgePair)
      key = render(ctx, entry.key)

      # :@x -> x: @x
      DictAssign.new(entry.key, key, tsrc(ctx, {:edge, key}))
    end

    private def render0(ctx : RenderContext, entry : Tree::TemplateRule)
      rule(ctx, Term.of(:rule), entry, &.template)
    end

    private def render0(ctx : RenderContext, entry : Tree::BackmapRule)
      rule(ctx, Term.of(:backmap), entry, &.backspec)
    end

    private def rule(ctx : RenderContext, head : Term, entry, &)
      id = id?(ctx, entry)
      body_node = yield entry

      # NOTE: it is of paramount importance that we set rule_block_id in all
      # cases, to support nesting (the closest enclosing id wins).

      begin
        pattern = render(ctx.copy_with(rule_id: id), entry.pattern)
      rescue RequiresRuleId
        id ||= id(ctx, entry)

        # Re-render with an id. Most of the stuff will be cached in ctx, including any
        # nested ids, so don't worry about performance [much]!
        pattern = render(ctx.copy_with(rule_id: id), entry.pattern)
      end

      begin
        body = render(ctx.copy_with(rule_id: id), body_node)
      rescue RequiresRuleId
        id ||= id(ctx, entry)

        # Same point about performance: almost all progress we've made up to this point is
        # most likely in the cache.
        body = render(ctx.copy_with(rule_id: id), body_node)
      end

      rule = tsrc(ctx) do |commit|
        commit << tsrc(ctx, head) << pattern << body

        if doc = doc?(ctx, entry.comments)
          commit.with(Term.of(:doc), doc)
        end
      end

      DictAppend.new(rule)
    end

    private def doc?(ctx : RenderContext, comments : Array(StringView)) : Tsrc?
      return if comments.empty?

      Tsrc.build do |commit|
        comments.each do |comment|
          # Each comment is of the form ";;<content>".
          content = comment.lskip(2)

          # Chop one space off from the left of the comment since it's canonical
          # to put it there:
          #
          #   ;;·Hello World
          #   ;;· indented
          #
          # Strip trailing whitespace similarly.
          content = content.lchop(' ').rstrip(" \t\r")

          commit << Tsrc.of(content, Term.of(content))
        end
      end
    end

    private def render0(ctx : RenderContext, node : Tree::DocumentDict)
      default = render(ctx, node.default)

      rest = dict(ctx, node.sections) { |section| render(ctx, section) }
      if rest.term.empty?
        return default
      end

      unless document = rest.add?(tsrc(ctx, :default), default)
        raise "document's default section collides with a section named `default`", node.default
      end

      document
    end

    private def render0(ctx : RenderContext, node : Tree::DocumentSection)
      name = render(ctx, node.name)
      body = render(ctx, node.body)

      assign(node.text, name, body)
    end

    private def render0(ctx : RenderContext, node : Tree::RuleId)
      if id = ctx.rule_id
        return tsrc(ctx, id)
      end

      # NOTE: there is no valid way to continue if something needs an Id and
      # the caller can't produce one. So we aren't using issues here.
      raise RequiresRuleId.new(ctx.location)
    end

    private def render0(ctx : RenderContext, node : Tree::RuleIdBlank)
      if id = ctx.rule_id
        return tsrc(ctx, Term::Sym.blank(id, :any))
      end

      # Ditto
      raise RequiresRuleId.new(ctx.location)
    end

    private def render0(ctx : RenderContext, node : Tree::RuleBlockId)
      if id = ctx.rule_block_id
        return tsrc(ctx, id)
      end

      # Ditto
      raise RequiresRuleBlockId.new(ctx.location)
    end

    private def render0(ctx : RenderContext, node : Tree::RuleBlockIdBlank)
      if id = ctx.rule_block_id
        return tsrc(ctx, Term::Sym.blank(id, :any))
      end

      # Ditto
      raise RequiresRuleBlockId.new(ctx.location)
    end

    private def render0(ctx : RenderContext, node : Tree::Tuck)
      pad = ->(arg : Tsrc, offset : Int32) do
        tsrc(ctx) do |commit|
          offset.times do
            commit << tsrc(ctx, :_)
          end

          commit << arg
          commit << tsrc(ctx, :"_*")
        end
      end

      memo = render(ctx, node.arg)

      node.offsets.reverse_each do |offset|
        memo = pad.call(memo, offset)
      end

      memo
    end

    private def render0(ctx : RenderContext, node : Tree::Location(_))
      render(ctx.copy_with(location: node.text), node.child)
    end

    private def render0(ctx : RenderContext, nodes : Array)
      nodes.map { |node| render(ctx, node) }
    end

    private def render0(ctx : RenderContext, node : Nil)
    end

    private def render(ctx : RenderContext, node)
      render0(ctx, node)
    end

    private def render(ctx : RenderContext, node : Tree::ExprNode) : Tsrc
      ctx.cache.put_if_absent(node) { render0(ctx, node) }
    end

    # Renders *node* into a term-source `Tsrc`.
    #
    # *node* is one of `Tree` nodes, probably something like `Tree::DocumentDict` or
    # `Tree::DocumentSection`, or perhaps one of `Tree::Expr`.
    #
    # Raises `RenderError` on error.
    def render(node) : Tsrc
      ctx = RenderContext.new(
        location: nil,
        rule_id: nil,
        rule_block_id: nil,
        ids: {} of Tree::PlaceholderContainer => Term::Sym,
        cache: {} of Tree::ExprNode => Tsrc,
      )

      render(ctx, node)
    rescue e : RequiresRuleId
      raise RenderError.new("cannot use `◇` outside of a rule", e.blame?)
    rescue e : RequiresRuleBlockId
      raise RenderError.new("cannot use `▢` outside of a rule", e.blame?)
    end
  end
end
