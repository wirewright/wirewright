module Ww::ML
  # Machinery to convert slices of atomic lexemes (`Lexeme::Atom`) to
  # WwML trees (`Tree::Expr`).
  struct Reader
    private alias Λ = Lexeme::Atom

    # Represents the ways parsing can fail.
    alias Err = Refusal | Failure

    # Represents a failing but choice-recoverable parse with an associated
    # *detail* and *text* (the latter selects the offending fragment
    # of source code).
    record Refusal, detail : String, text : StringView do
      # :nodoc:
      def ord
        {-1, text.byte_start}
      end
    end

    # Represents an unrecoverable parse with an associated *detail* and *text*
    # (the latter selects the offending fragment of source code).
    #
    # `Failure`s interrupt the parsing process. They are essentially
    # lightweight exceptions.
    record Failure, detail : String, text : StringView do
      # :nodoc:
      def ord
        {+1, text.byte_start}
      end
    end

    def initialize(lexemes : Slice(Λ), @addons : Addons)
      @cursor = Cursor.new(lexemes)
    end

    private delegate :behind, :ahead, :forward, to: @cursor

    # Executes the block inside a cursor transaction. If the block returns
    # a non-nil , any cursor changes it made are committed, and the non-nil
    # is returned. If the block returns `nil`, all changes it made to
    # the cursor are reverted, and `nil` is returned.
    private def try?(&)
      state = @cursor.state

      begin
        result = yield
      ensure
        if result.nil?
          @cursor.restore(state)
        end
      end

      result
    end

    # Executes the block inside a cursor transaction. If the block returns
    # `Err`, the transaction is reverted, and the `Err` is returned. Otherwise,
    # the transaction is committed, and the block's result is returned.
    private def txn(&)
      state = @cursor.state
      restore = true

      begin
        result = yield
        restore = result.is_a?(Err)
      ensure
        if restore
          @cursor.restore(state)
        end
      end

      result
    end

    # Skips lexemes until the block returns `true`.
    #
    # Returns `true` if more lexemes remain ahead. Returns `false` if EOI
    # is ahead.
    private def skip(& : Λ -> Bool) : Bool
      loop do
        unless yield ahead
          return true
        end

        unless forward
          return false
        end
      end
    end

    # Attempts to move past a lexical token of the given *type*. Returns
    # the token if successful; `nil` otherwise.
    private def past?(type : Lexeme::Token::Type) : Lexeme::Token?
      try? do
        λ = ahead

        return unless λ.is_a?(Lexeme::Token)
        return unless λ.type == type

        forward

        λ
      end
    end

    # Transaction: Attempts to move past a sequence of lexical tokens of
    # the given *types*. Returns `true` if successful; `false` otherwise.
    private def past?(*types : Lexeme::Token::Type) : Bool
      try? do
        types.each do |type|
          return false unless past?(type)
        end

        true
      end
    end

    # Attempts to move past a lexical datum accepted by the block. Returns
    # the datum if successful; `nil` otherwise.
    private def past_datum?(& : Term -> Bool) : Lexeme::Datum?
      try? do
        return unless λ = ahead.as?(Lexeme::Datum)
        return unless yield λ.term

        forward

        λ
      end
    end

    # Attempts to move past a lexical datum. Returns the datum if successful;
    # `nil` otherwise.
    private def past_datum? : Lexeme::Datum?
      past_datum? { true }
    end

    # Returns `true` if a sequence of lexical tokens of the given *types*
    # is found after the cursor.
    private def ahead?(*types : Lexeme::Token::Type) : Bool
      try? do
        return past?(*types) ? true : false
      end
    end

    private def refusal(detail : String, text : StringView, **kwargs)
      Refusal.new(detail, text, **kwargs)
    end

    private def failure(detail : String, text : StringView, **kwargs)
      Failure.new(detail, text, **kwargs)
    end

    private def bind(result, &)
      yield result
    end

    private def bind(result : Err, &)
      result
    end

    # Transaction: chooses the first successful parseout-returning branch
    # among *branches*.
    private macro choice(*branches)
      pass do
        %err = refusal("unexpected input", ahead.text)

        {% for branch in branches %}
          case %π = txn { {{branch}} }
          when Failure
            next %π
          when Refusal
            # Prefer π over any previous refusal unless the previous refusal made more
            # progress into the source string.
            %err = %π.ord < %err.ord ? %err : %π
          else
            next %π
          end
        {% end %}

        %err
      end
    end

    # Attempts to perform a *parse*.
    #
    # If the parse succeeds, returns the resulting tree.
    #
    # If the parse fails, **exits the enclosing function**.
    #
    # - If the parse failed with refusal and *expect* is `true`, converts
    #   the refusal to failure, and exits.
    # - If the parse failed with refusal and *expect* is a string, converts
    #   the refusal to failure with that string set as detail (if appropriate);
    #   and exits.
    # - If the parse failed with failure, exits with failure.
    #
    # Wraps *parse* in a location node (`track`).
    private macro value!(parse, *, expect = false)
      track do
        case %π = {{parse}}
        when Refusal
          {% if expect.is_a?(StringLiteral) %}
            return { %π, failure({{expect}}, %π.text) }.max_by(&.ord)
          {% elsif expect %}
            return failure(%π.detail, %π.text)
          {% else %}
            return %π
          {% end %}
        when Failure
          return %π
        else
          %π
        end
      end
    end

    # Returns a pair of procs. The first proc marks "start recording here".
    # The second proc closes the current span and hands you whatever text lies
    # between the latest start point and the current cursor position, then
    # immediately opens a new span from that point onward.
    #
    # Calling the "start" proc again without calling the "end" proc resets
    # the start point to wherever you currently are, effectively discarding
    # any unclosed span.
    private def recorder : {->, ->}
      cursor = @cursor.state
      start = ahead

      fbegin = -> do
        cursor = @cursor.state
        start = ahead
      end

      fend = -> do
        if cursor == @cursor.state
          # Cursor did not move, then emit an empty span.
          span = start.text.before_begin
        else
          span = start.text.span(behind.text)
        end

        cursor = @cursor.state
        start = ahead

        span
      end

      {fbegin, fend}
    end

    # Rerturns a pair of the block's result and the fragment of source code
    # it advanced through. If the block returns `Err`, returns that `Err` instead.
    private def span(&)
      _, finish = recorder

      π = yield
      π.is_a?(Err) ? π : {π, finish.call}
    end

    # Wraps the node returned by the block in a `Location` node, assuming it
    # is one of the `Tree::HasLocation` nodes. Otherwise, returns the node as-is.
    private def track(&)
      case row = span { yield }
      when Err
        return row
      end

      tracked(*row)
    end

    private def tracked(π, span)
      if π.is_a?(Tree::HasLocation) && @addons.location?
        return Tree.location(π, span)
      end

      π
    end

    # Datums are "baked" lexemes -- lexemes that were read into a term
    # during lexical analysis.
    private def datum
      unless datum = past_datum?
        return refusal("expected a boolean, string, or number", ahead.text.before_begin)
      end

      Tree::Leaf.new(datum.term)
    end

    # Reads a WwML symbol; standard such as `px-10`, or arising from templating
    # such as in `px⫽y-10`.
    private def symbol
      # Treat standalone `+`, `-`, etc. as a symbol in symbol context: in `(⏏+⏏ 1 2)`,
      # `+` is a symbol.
      case
      when past?(:plus)       then return Tree::Leaf.new(Term.of(:+))
      when past?(:minus)      then return Tree::Leaf.new(Term.of(:-))
      when past?(:dollar)     then return Tree::Leaf.new(Term.of(:"$"))
      when past?(:caret)      then return Tree::Leaf.new(Term.of(:^))
      when past?(:caret_star) then return Tree::Leaf.new(Term.of(:"^*"))
      when ahead?(:plus_left, :symbol),
           ahead?(:minus_left, :symbol),
           ahead?(:dollar_left, :symbol),
           ahead?(:caret_left, :symbol),
           ahead?(:caret_star_left, :symbol)
        prefix = ahead.text
        forward
        # $⏏foo
        root = ahead.text
        forward
        # $foo⏏
      when ahead?(:symbol)
        prefix = ahead.text.before_begin
        root = ahead.text
        forward
      else
        return refusal("expected a symbol", ahead.text.before_begin)
      end

      name = String.build do |io|
        io << prefix << root

        while suffix = past?(:symbol_suffix)
          io << suffix.text
        end
      end

      Tree::Leaf.new(Term.of(Term::Sym.new(name)))
    end

    # ⏏"hello"  ⏏"hello ⸢interpolated⸣ string"
    private def stringdq
      unless past?(:dquote_left)
        return refusal("expected a string", ahead.text.before_begin)
      end

      segments = [] of Tree::Expr

      until past?(:dquote_right)
        if segment = past_datum?(&.type.string?)
          segments << Tree::Leaf.new(segment.term)
          next
        end

        assert past?(:tl_half_bracket)

        segments << value!(slot, expect: true)

        unless past?(:tr_half_bracket)
          return failure("expected `⸣` to end string interpolation", ahead.text.before_begin)
        end
      end

      Tree::Stitch.new(segments)
    end

    # ⏏[a b c]
    private def itemspattern
      unless past?(:lbracket)
        return refusal("expected `[` to begin an itemspattern", ahead.text.before_begin)
      end

      items = [] of Tree::Expr

      until past?(:rbracket)
        if ahead?(:colon_left) || ahead?(:colon_right)
          return failure("cannot use pairs in an itemspattern", ahead.text)
        end

        items << value!(slot, expect: "expected an item, or `]` to end the itemspattern")
      end

      Tree::Itemspattern.new(items)
    end

    # ⏏{¦ a b c}
    private def pairspattern
      case
      when past?(:lcurly_broken_bar)
      when ahead?(:lcurly)
        π = txn do
          # {⏏
          forward

          # Try match {⏏xyz
          atom
        end

        case π
        when Failure
          return π
        when Refusal
          # {⏏
        else
          itemsname = π
          # {xyz⏏
        end

        unless past?(:broken_bar)
          return refusal("expected `¦`", ahead.text.before_begin)
        end

        # {¦⏏  {xyz¦⏏
      else
        return refusal("expected a pairspattern", ahead.text.before_begin)
      end

      selection = value!(selectors(:rcurly), expect: true)

      unless past?(:rcurly)
        return failure("expected `}` to close the pairspattern", ahead.text.before_begin)
      end

      # {¦ a b c}⏏
      Tree::Pairspattern.new(itemsname, selection)
    end

    # ⏏{| a b c}
    private def entries_pattern
      unless past?(:lcurly_bar)
        return refusal("expected `{| ` to start an entries-pattern", ahead.text.before_begin)
      end

      # {| ⏏x y z}
      selection = value!(selectors(:rcurly))

      # {| x y z⏏}
      unless past?(:rcurly)
        return failure("expected `}` to close the entries-pattern", ahead.text.before_begin)
      end

      # {| x y z}⏏
      Tree::EntriesPattern.new(selection)
    end

    # ⏏{x: 100, y: 200}
    private def pairs
      unless past?(:lcurly)
        return refusal("expected `{`", ahead.text.before_begin)
      end

      pairs = [] of Tree::DictPair

      until past?(:rcurly)
        case π = short_pair
        when Failure
          return π
        when Refusal
        else
          pairs << π
          next
        end

        key = value!(slot, expect: "expected a key, or `}` to end the dict")

        unless past?(:colon_right)
          return failure("expected `:` after key", behind.text.after_end)
        end

        value = value!(slot, expect: "expected a value, or `}` to end the dict")
        pairs << Tree::DictKVPair.new(key, value)
      end

      Tree::PairDict.new(pairs)
    end

    # {+ ...}
    # {- ...}
    # {+¦ ...}
    # {-¦ ...}
    private def set
      items = [] of Tree::Expr

      case
      when past?(:lcurly_plus)
        construct = -> { Tree::SameValueSet.new(items, value: Term.of(true)) }
      when past?(:lcurly_minus)
        construct = -> { Tree::SameValueSet.new(items, value: Term.of(false)) }
      when past?(:lcurly_plus_broken_bar)
        construct = -> { Tree::LayerIgnoreResidue.new(Tree::SameValueSet.new(items, value: Term.of(true))) }
      when past?(:lcurly_minus_broken_bar)
        construct = -> { Tree::LayerIgnoreResidue.new(Tree::SameValueSet.new(items, value: Term.of(false))) }
      else
        return refusal("expected a dict set", ahead.text.before_begin)
      end

      until past?(:rcurly)
        if ahead?(:colon_left) || ahead?(:colon_right)
          return failure("cannot use pairs in dict set", ahead.text)
        end

        items << value!(slot, expect: "expected a key, or `}` to end the dict set")
      end

      construct.call
    end

    # {# a b a a}
    # {# a 5×b a a}
    # {#¦ a b a a}
    # {#¦ a 5×b a a}
    private def mset
      entries = [] of Tree::MultisetEntry

      case
      when past?(:lcurly_hash)
        construct = -> { Tree::Multiset.new(entries) }
      when past?(:lcurly_hash_broken_bar)
        construct = -> { Tree::LayerIgnoreResidue.new(Tree::Multiset.new(entries)) }
      else
        return refusal("expected `{#` or `{#¦`", ahead.text.before_begin)
      end

      until past?(:rcurly)
        if ahead?(:colon_left) || ahead?(:colon_right)
          return failure("cannot use pairs in dict multiset", ahead.text)
        end

        left = value!(slot, expect: "expected an item, or `}` to end the multiset")

        if past?(:times)
          right = value!(slot, expect: "expected an item")
          entries << Tree::MultisetEntryN.new(left, right)
          next
        end

        entries << Tree::MultisetEntryOne.new(left)
      end

      construct.call
    end

    # {% x y z}
    private def keypool
      unless past?(:lcurly_percent)
        return refusal("expected `{%` to begin keypool", ahead.text.before_begin)
      end

      items = [] of Tree::Expr

      until past?(:rcurly)
        if ahead?(:colon_left) || ahead?(:colon_right)
          return failure("cannot use pairs in keypool", ahead.text)
        end

        items << value!(slot, expect: "expected a key, or `}` to end the keypool")
      end

      Tree::Keypool.new(items)
    end

    # ⏏⟨a b c⟩
    private def split
      unless past?(:langle)
        return refusal("expected `⟨`", ahead.text.before_begin)
      end

      parts = [] of Tree::SplitPart
      part = [] of Tree::Expr
      span_begin, span_end = recorder

      loop do
        # ⟨a b c⏏⟩
        if ahead?(:rangle)
          parts << tracked(Tree::SplitPartNode.new(part), span_end.call)
          forward
          return split(parts, pairside: nil, source: false)
        end

        # ⟨a b c⏏⟩°
        if ahead?(:rangle_source)
          parts << tracked(Tree::SplitPartNode.new(part), span_end.call)
          forward
          return split(parts, pairside: nil, source: true)
        end

        case π = interfix(:rangle, :rangle_source)
        when Refusal
        when Err
          return π
        else
          # ⟨a b c ¦ _⏏⟩
          if ahead?(:rangle)
            parts << tracked(Tree::SplitPartNode.new(part), span_end.call)
            forward
            return split(parts, pairside: π, source: false)
          end

          # ⟨a b c ¦ _⏏⟩°
          if ahead?(:rangle_source)
            parts << tracked(Tree::SplitPartNode.new(part), span_end.call)
            forward
            return split(parts, pairside: π, source: true)
          end

          return failure("expected `⟩` to end the split", ahead.text.before_begin)
        end

        # ⟨a b c ⏏… d e f⟩
        if ahead?(:ellipsis)
          parts << tracked(Tree::SplitPartNode.new(part), span_end.call)
          forward
          part = [] of Tree::Expr
          span_begin.call
          next
        end

        # ⟨a ⏏b c⟩
        part << value!(slot, expect: "expected an item, `…`, an interfix, or `⟩` to end the split")
      end
    end

    # Smart constructor for Split or FirstItem.
    private def split(parts : Array(Tree::SplitPart), *, pairside, source : Bool)
      if parts.size == 1
        return Tree::ItemFirst.new(parts.first, pairside, source)
      end

      Tree::Split.new(parts, pairside, source)
    end

    # ⏏⟨& a b c⟩
    private def all_item
      unless past?(:langle_ampersand)
        return refusal("expected `⟨&`", ahead.text.before_begin)
      end

      items = [] of Tree::Expr

      loop do
        if ahead?(:colon_left) || ahead?(:colon_right)
          return failure("cannot use pairs in an all-items expression", ahead.text)
        end

        # ⟨& a b c⏏⟩
        if past?(:rangle)
          return Tree::AllItem.new(items, pairside: nil, source: false)
        end

        # ⟨& a b c⏏⟩°
        if past?(:rangle_source)
          return Tree::AllItem.new(items, pairside: nil, source: true)
        end

        case π = interfix(:rangle, :rangle_source)
        when Refusal
        when Err
          return π
        else
          # ⟨& a b c ⍊ qux⏏⟩
          if past?(:rangle)
            return Tree::AllItem.new(items, pairside: π, source: false)
          end

          # ⟨& a b c ⍊ qux⏏⟩°
          if past?(:rangle_source)
            return Tree::AllItem.new(items, pairside: π, source: true)
          end
        end

        # ⟨& a ⏏b c⟩
        items << value!(slot, expect: "expected an item, an interfix, or `⟩` to end the all-items expression")
      end
    end

    # ⏏⟪a b c⟫
    private def all_leaf
      unless past?(:double_langle)
        return refusal("expected `⟪`", ahead.text.before_begin)
      end

      items = [] of Tree::Expr

      loop do
        # ⟪a b c⏏⟫
        if past?(:double_rangle)
          return Tree::AllLeaf.new(items, pairside: nil, source: false)
        end

        # ⟪a b c⏏⟫°
        if past?(:double_rangle_source)
          return Tree::AllLeaf.new(items, pairside: nil, source: true)
        end

        case π = interfix(:double_rangle, :double_rangle_source)
        when Refusal
        when Err
          return π
        else
          # ⟪a b c ⍊ qux⏏⟫
          if past?(:double_rangle)
            return Tree::AllLeaf.new(items, pairside: π, source: false)
          end

          # ⟪a b c ⍊ qux⏏⟫°
          if past?(:double_rangle_source)
            return Tree::AllLeaf.new(items, pairside: π, source: true)
          end
        end

        # ⟪a ⏏b c⟫
        items << value!(slot, expect: "expected an item, an interfix, or `⟫` to end the all-leaves expression")
      end
    end

    UNDERSCORE_QUESTION = Tree::Leaf.new(Term.of(:"%past", :_, min: 0, max: 1))

    # ⏏_?
    #
    # TODO: move to lexeme reader
    private def shorthand
      case
      when past?(:underscore_question)
        UNDERSCORE_QUESTION
      else
        refusal("expected a shorthand", ahead.text.before_begin)
      end
    end

    # ⏏(a b c)
    private def dict
      unless past?(:lparen)
        return refusal("expected `(` to start a dict", ahead.text.before_begin)
      end

      case π = section(allow_empty: true)
      when Failure
        return π
      when Refusal
        itemside = Tree::DictSection.new([] of Tree::DictEntryBlock)
      else
        itemside = π
      end

      case π = interfix(:rparen)
      when Failure
        return π
      when Refusal
        case
        when past?(:caret_ellipsis)
          # (a b ⏏^… c)
          extra = value!(atom, expect: true)
          make = -> { Tree::DictExtend.new(itemside, extra) }
        else
          make = -> { Tree::Dict.new(itemside, pairside: nil) }
        end
      else
        make = -> { Tree::Dict.new(itemside, pairside: π) }
      end

      unless past?(:rparen)
        return failure("expected `)` to end the dict", ahead.text.before_begin)
      end

      make.call
    end

    # {¦ ⏏:xyz}  {¦ ⏏⋮x}  etc.
    private def prefix_selector
      case
      when past?(:colon_left)
        # {¦ :⏏xyz}
        key = value!(atom)

        Tree::SelectorPairRequired.new(key, key)
      when past?(:triple_colon_left)
        # {¦ ⋮⏏x}
        key = value!(atom)

        Tree::SelectorMaybeKey.new(key)
      when past?(:minus), past?(:minus_left)
        # -⏏a
        # -⏏a: foo
        # -⏏a_number
        key = value!(atom)
        if past?(:colon_right)
          name = value!(atom)
        end

        Tree::SelectorNegative.new(key, name)
      when past?(:plus_minus)
        # ±⏏x
        key = value!(atom)

        Tree::SelectorNumber.new(key)
      when past?(:at_sign)
        selector = try? do
          # @⏏x_
          term = value!(atom)
          next if ahead?(:colon_right) # @x⏏: 100

          Tree::SelectorEdge.new(term)
        end

        selector || refusal("not a prefix selector", ahead.text)
      else
        refusal("not a prefix selector", ahead.text)
      end
    end

    # {¦ ⏏xyz: 100}  {¦ ⏏x⋮ 100}  {¦ ⏏x⁺}  etc.
    private def infix_selector
      key = value!(atom)

      case
      when past?(:sup_plus_right)
        # {¦ x⁺⏏}
        Tree::SelectorLetTrue.new(key)
      when past?(:sup_minus_right)
        # {¦ x⁻⏏}
        Tree::SelectorLetFalse.new(key)
      when past?(:colon_right)
        # {¦ x:⏏ y}
        value = value!(slot, expect: true)

        Tree::SelectorPairRequired.new(key, value)
      when past?(:triple_colon_right)
        # {¦ x⋮⏏ y}
        value = value!(slot, expect: true)

        Tree::SelectorPairDefault.new(key, value)
      else
        Tree::SelectorKey.new(key)
      end
    end

    private def selector : Tree::Selector | Err
      track do
        choice(
          prefix_selector,
          infix_selector,
          refusal("expected a selector", ahead.text.before_begin)
        )
      end
    end

    private def selectors(*delimiters : Lexeme::Token::Type) : Array(Tree::Selector) | Err
      selection = [] of Tree::Selector

      until delimiters.any? { |delimiter| ahead?(delimiter) }
        case π = selector
        when Refusal
          return selection
        when Failure
          return π
        else
          selection << π
        end
      end

      selection
    end

    private def layer(*delimiters : Lexeme::Token::Type)
      # (¦⏏)
      # (x y z ¦⏏)
      if delimiters.any? { |delimiter| ahead?(delimiter) }
        return Tree::Layer.new(nil, [] of Tree::Selector)
      end

      # (x y z ¦ ⏏ _ a b c)

      # If `(¦ a⏏ b c)`, then residue = a, pairspattern = (b c)
      # If `(¦ a⏏ :b c)`, then residue = (a), pairspattern = (b: b c)
      # If `(¦ a⏏: b c)`, then residue = (), pairspattern = (a: b c)
      head = residue = nil

      π = txn do
        bind(selector) do |tree|
          # HACK: hard-code `(¦ x⏏←_)` and the like to count as residue.
          if ahead?(:arrow_left)
            next refusal("invalid selector", ahead.text.before_begin)
          end

          tree
        end
      end

      case π
      when Refusal
        residue = value!(slot, expect: "expected a residue expression")
      when Failure
        return π
      else
        # In ⏏_⏏ x: 100, `_` is residue, not selector.
        # In ⏏qux_⏏ x: 100, `qux_` is residue, not selector.
        if π1 = Tree.topmost?(π, as: Tree::SelectorKey)
          residue = π1.key
        else
          head = π
        end
      end

      selection = value!(selectors(*delimiters))
      selection.unshift(head) if head

      Tree::Layer.new(residue, selection)
    end

    # Pairspart interfix
    #
    # (x y z ⏏¦ _ a b c)
    # (x y z ⏏⍊ a b c)
    private def interfix(*delimiters : Lexeme::Token::Type)
      case
      when past?(:broken_bar)
        # (x y z ¦⏏ _ a b c)
        value!(layer(*delimiters))
      when past?(:bar_underscore)
        # (x y z ⍊⏏ a b c)
        selection = value!(selectors(*delimiters))

        Tree::Layer.new(residue: Tree::Leaf.new(Term.of(:_)), selection: selection)
      else
        refusal("expected pairspart interfix", ahead.text.before_begin)
      end
    end

    private def term : Tree::Expr | Err
      choice(
        dict,
        datum,
        symbol,
        stringdq,
        itemspattern,
        pairspattern,
        entries_pattern,
        pairs,
        set,
        mset,
        keypool,
        split,
        all_item,
        all_leaf,
        shorthand,
      )
    end

    # ⏏@qux  ⏏%'(+ a_ b_)  etc.
    private def sigil
      case
      when past?(:at_sign)
        Tree::Edge.new(value!(atom, expect: true))
      when past?(:arrow_up)
        Tree::Up.new(value!(atom, expect: true))
      when past?(:arrow_dn)
        Tree::Dn.new(value!(atom, expect: true))
      when past?(:arrow_right)
        Tree::My.new(value!(atom, expect: true))
      when past?(:caret_left)
        Tree::AlloyExpr.new(value!(atom, expect: true))
      when past?(:caret_star_left)
        Tree::AlloySpliceExpr.new(value!(atom, expect: true))
      when past?(:quote)
        Tree::Literal.new(value!(atom, expect: true))
      when past?(:dollar_left)
        Tree::Dollar.new(value!(atom, expect: true))
      when past?(:dollar_quote)
        Tree::DollarOnce.new(value!(atom, expect: true))
      when past?(:percent_quote)
        Tree::PatternLiteral.new(value!(atom, expect: true))
      when past?(:triple_equals)
        Tree::PatternNonself.new(value!(atom, expect: true))
      when past?(:backquote)
        Tree::PatternSlot.new(value!(atom, expect: true))
      when past?(:plus_minus)
        Tree::PatternNumber.new(value!(atom, expect: true))
      when past?(:plus_left)
        Tree::Positive.new(value!(atom, expect: true))
      when past?(:minus_left)
        Tree::Negative.new(value!(atom, expect: true))
      else
        refusal("expected a sigil", ahead.text.before_begin)
      end
    end

    # ⏏⸤qux⸥
    private def grouping
      unless past?(:bl_half_bracket)
        return refusal("expected `⸤`", ahead.text.before_begin)
      end

      value = value!(slot, expect: true)

      unless past?(:br_half_bracket)
        return failure("expected `⸥` to close the `⸤`", ahead.text.before_begin)
      end

      value
    end

    # ⏏◇  ⏏▢   etc.
    private def placeholder
      case
      when past?(:diamond)            then Tree::RuleId.new
      when past?(:diamond_underscore) then Tree::RuleIdBlank.new
      when past?(:rrect)              then Tree::RuleBlockId.new
      when past?(:rrect_underscore)   then Tree::RuleBlockIdBlank.new
      else
        refusal("expected a placeholder", ahead.text.before_begin)
      end
    end

    # ⏏⁰x  ⏏¹⁻⁵x  etc.
    private def tuck
      unless initial = past?(:sup_digits_left)
        return refusal("tuck operator requires at least one superscript left digit", ahead.text.before_begin)
      end

      offsets = [Kit.sup2i?(initial.text) || unreachable("nondigit in sub_digits_left")]

      while past?(:sup_minus_left)
        unless digits = past?(:sup_digits_left)
          return failure("expected one or more superscript digits after `⁻`", ahead.text.before_begin)
        end

        offsets << (Kit.sup2i?(digits.text) || unreachable("nondigit in sub_digits_left"))
      end

      arg = value!(atom, expect: true)

      Tree::Tuck.new(offsets, arg)
    end

    private def group_split
      unless past?(:ellipsis_before_langle)
        return refusal("expected `…⟨` to start group split", ahead.text.before_begin)
      end

      interior = value!(choice(split, all_item), expect: "expected `⟨` or `⟨&`")

      unless past?(:ellipsis_after_rangle)
        return refusal("expected `⟩…` to end group split", ahead.text.before_begin)
      end

      Tree::GroupSplit.new(interior)
    end

    private def atom : Tree::Expr | Err
      choice(
        term,
        sigil,
        grouping,
        placeholder,
        tuck,
        group_split,
        refusal("expected a term", ahead.text.before_begin),
      )
    end

    # Reads a slot, which is roughly defined as the WwML's equivalent of
    # binary operators (e.g. `←`) and everything below (including "atoms" such
    # as `⏏@⏏x`, `⏏x⏏`, dict `⏏(+ 1 2)⏏`, etc.)
    def slot : Tree::Expr | Err
      left = value!(atom)

      case
      when past?(:arrow_left)
        # x←⏏y
        right = value!(slot, expect: true)

        Tree::PatternLet.new(left, right)
      else
        left
      end
    end

    # ⏏:qux  ⏏^:x  etc.
    private def short_pair : Tree::DictPair | Err
      case
      when past?(:colon_left)
        # :⏏qux
        term = value!(slot, expect: "expected a term to follow `:`")

        Tree::DictKVPair.new(term, term)
      when past?(:caret_colon)
        # ^:⏏qux
        term = value!(slot, expect: "expected a term to follow `^:`")

        Tree::DictAlloyPair.new(term)
      when past?(:at_sign_colon)
        # @:⏏qux
        term = value!(slot, expect: "expected a term to follow `@:`")

        Tree::DictEdgePair.new(term)
      else
        refusal("expected a pair prefix", ahead.text.before_begin)
      end
    end

    private def entry_itself(comments : Array(StringView))
      π = short_pair
      unless π.is_a?(Refusal)
        return π
      end

      # Read the head part of the entry.
      left = value!(slot)

      # x⏏: y
      if past?(:colon_right)
        right = value!(slot, expect: "expected a value to follow `:`")

        return Tree::DictKVPair.new(left, right)
      end

      case
      when past?(:bidi_arrow)
        # x <> ⏏y
        body = value!(slot, expect: "expected backmap body")
        rulecls = Tree::BackmapRule
      when past?(:fat_arrow_right)
        # x => ⏏y
        body = value!(slot, expect: "expected rule body")
        rulecls = Tree::TemplateRule
      else
        # x⏏
        return Tree::DictItem.new(left)
      end

      rulecls.new(left, body, comments)
    end

    # Reads a single item, pair, or rule. Rules may consume comments
    # above them.
    private def entry
      comments = [] of StringView

      if @addons.doc_comment?
        # Collect comments that we'll possibly attach to the entry.
        @cursor.reverse_each_previous_ignored do |λ|
          break unless λ.is_a?(Lexeme::Token)
          break unless λ.type.line_comment?

          comments.unshift(λ.text)
        end
      end

      # Read entry state.
      case
      when past?(:semicolon)
        # ;⏏(+ 1 2)
        entry = value!(entry_itself(comments), expect: "expected an entry")

        Tree::DisabledEntry.new(entry)
      when past?(:semicolon_comma)
        # ;,⏏(+ 1 2)
        entry = value!(entry_itself(comments), expect: "expected an entry")

        Tree::FocusedEntry.new(entry)
      else
        # ⏏(+ 1 2)
        entry = value!(entry_itself(comments))

        Tree::EnabledEntry.new(entry)
      end
    end

    private def block_delimiter?(λ : Lexeme::Token) : Bool
      λ.type.blank_line? || λ.type.double_blank_line? || λ.type.double_pipe?
    end

    private def block_delimiter?(λ) : Bool
      false
    end

    # Entry blocks are groups of entries usually separated by a blank line
    # (similar in parsing spirit to something like a Markdown paragraph).
    private def entry_block : Tree::DictEntryBlock | Err
      entries = [] of Tree::ToggleableEntry

      loop do
        unless entries.empty?
          break if @cursor.ignored? { |λ| block_delimiter?(λ) }
        end

        case π = entry
        when Refusal then break
        when Failure then return π
        end

        entries << π
      end

      if entries.empty?
        return refusal("expected a nonempty entry block", ahead.text.before_begin)
      end

      Tree::DictEntryBlock.new(entries)
    end

    # Reads a document section.
    #
    # This is one of the top-level reader methods. It is used by the user-level
    # `ML.terms` and derived.
    #
    # *allow_empty*, if `false`, causes this method to refuse on empty sections.
    def section(*, allow_empty : Bool = false) : Tree::DictSection | Err
      blocks = [] of Tree::DictEntryBlock

      loop do
        case π = entry_block
        when Refusal then break
        when Failure then return π
        end

        blocks << π
      end

      if blocks.empty? && !allow_empty
        return refusal("expected a nonempty section", ahead.text.before_begin)
      end

      Tree::DictSection.new(blocks)
    end

    # Reads a document.
    #
    # This is one of the top-level reader methods. It is used by the user-level
    # `ML.document` and derived.
    def document : Tree::DocumentDict | Err
      default = value!(section(allow_empty: true))

      sections = [] of Tree::DocumentSection
      text_begin, text_end = recorder

      loop do
        text_begin.call

        case
        when past?(:vspace_triple_dash)
          # --- ⏏qux
          # (+ 1 2)
          name = value!(atom, expect: true)
        when ahead?(:vspace_triple_dash_vspace)
          # ⏏---
          # (+ 1 2)
          name = Tree::Leaf.new(Term.of(:aux))
          forward
          # ---
          # ⏏(+ 1 2)
        else
          break
        end

        text = text_end.call

        # --- qux
        # ⏏(+ 1 2)
        body = value!(section(allow_empty: true))

        sections << Tree::DocumentSection.new(name, body, text)
      end

      Tree::DocumentDict.new(default, sections)
    end

    # Toplevel parse: yields `self` to the block and waits for it to read
    # the input, then makes sure no more input follows. Trailing input causes
    # a failure.
    #
    # If you want to read more data after a WwML term, feel free to not use
    # this method.
    def top(&)
      π = yield self
      if π.is_a?(Err)
        return π
      end

      unless ahead?(:eoi)
        return failure("unexpected input", ahead.text.before_begin)
      end

      π
    end
  end
end

require "./reader/cursor"
