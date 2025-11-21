module Ww::ML
  # The WwML term reader offers machinery to convert an array of atomic
  # lexemes -- read by `Lexeme::Reader` -- into a `Term`.
  #
  # NOTE: Use `ML.term` and friends to read terms.
  #
  # ```
  # begin
  #   atoms = ML::Lexeme.atoms(source)
  # rescue e : SyntaxError
  #   # Lexical error
  # end
  #
  # reader = ML::Reader.new(atoms)
  #
  # # This is the point where actual parsing happens.
  # parseout = reader.document
  #
  # # You MUST run post-validation for everything to be appropriate!
  # case π = reader.toplevel(source, parseout)
  # in Reader::Parseout::Ok
  #   # ...
  # in Reader::Parseout::Err
  #   # ... (Maybe convert to ML::SyntaxError?)
  # end
  # ```
  struct Reader
    private alias Π = Parseout::Any
    private alias Λ = Lexeme::Atom

    # Represents the intermediate and final results of parsing.
    module Parseout
      alias Any = Ok | Err
      alias Err = Refusal | Failure

      # Represents a successful parse that resulted in *term*.
      record Ok, term : Term

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
    end

    def initialize(lexemes : Slice(Λ), @addons : Addons)
      @cursor = Cursor.new(lexemes)
    end

    private delegate :behind, :ahead, :forward, to: @cursor

    # Executes the block inside a cursor transaction. If the block returns
    # a non-nil object, any cursor changes it made are committed. If the block
    # returns `nil`, all changes it made to the cursor are reverted.
    private def try?(& : -> T?) : T forall T
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

    # Same as `try?`, except the block now must return a `Parseout`. The transaction
    # succeeds if it is `Parseout::Ok`; and is reverted otherwise.
    private def txn(& : -> Π) : Π
      state = @cursor.state

      begin
        result = yield

        case result
        in Parseout::Ok
        in Parseout::Err
          @cursor.restore(state)
        end
      ensure
        if result.nil?
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
    # is found after the cursor. Returns `false` otherwise.
    private def ahead?(*types : Lexeme::Token::Type) : Bool
      try? do
        return past?(*types) ? true : false
      end
    end

    private def ok(object) : Π
      Parseout::Ok.new(Term.of(object))
    end

    private def refusal(detail : String, text : StringView, **kwargs) : Π
      Parseout::Refusal.new(detail, text, **kwargs)
    end

    private def failure(detail : String, text : StringView, **kwargs) : Π
      Parseout::Failure.new(detail, text, **kwargs)
    end

    # Transaction: chooses the first successful parseout-returning branch
    # among *branches*.
    private macro choice(*branches)
      pass do
        %err = refusal("unexpected input", ahead.text)

        {% for branch in branches %}
          case %π = txn { {{branch}} }
          in Parseout::Ok, Parseout::Failure
            next %π
          in Parseout::Refusal
            # Prefer π over any previous refusal unless the previous refusal made more
            # progress into the source string.
            %err = %π.ord < %err.ord ? %err : %π
          end
        {% end %}

        %err
      end
    end

    # Attempts to perform a *parse*.
    #
    # If the parse succeeds, returns the resulting term and the fragment
    # of source code from which it was produced.
    #
    # If the parse fails, **exits the enclosing function**.
    #
    # - If the parse failed with refusal and *expect* is `true`, converts
    #   the refusal to failure, and exits.
    # - If the parse failed with refusal and *expect* is a string, converts
    #   the refusal to failure with that string set as detail (if appropriate);
    #   and exits.
    # - If the parse failed with failure, exits with failure.
    private macro capture!(parse, *, expect = false)
      pass do
        %begin = ahead
        %cursor = @cursor.state

        case %π = {{parse}}
        in Parseout::Ok
          if %cursor == @cursor.state
            # Cursor did not move, then emit an empty span.
            { %π.term, %begin.text.before_begin }
          else

            { %π.term, %begin.text.span(behind.text) }
          end
        in Parseout::Refusal
          {% if expect.is_a?(StringLiteral) %}
            return { %π, failure({{expect}}, %begin.text) }.max_by(&.ord)
          {% elsif expect %}
            return failure(%π.detail, %π.text)
          {% else %}
            return %π
          {% end %}
        in Parseout::Failure
          return %π
        end
      end
    end

    private alias AsKeyEntry = {Term, Term}

    @as_key_cache : StaticArray(AsKeyEntry?, 4) = StaticArray[
      nil.as(AsKeyEntry?),
      nil.as(AsKeyEntry?),
      nil.as(AsKeyEntry?),
      nil.as(AsKeyEntry?),
    ]

    # We treat terms as AST nodes here in the reader. This has the unintended
    # consequence of altering the term's identity with possible metadata that
    # would have otherwise belonged to the AST. This is OK since we expect
    # pruning from the client -- after all, the client explicitly asks for
    # metadata, so we assume they know what they're doing.
    #
    # The main problem arises when we use the term as a key -- in other words,
    # in contexts where its identity matters.
    #
    # It is of paramount importance that this method is called on all key terms
    # to remove AST metadata from them; thus, fixing their identity to that
    # intended by the client.
    #
    # In other words, we do not provide metadata in keys because that would change
    # their identity. This method erases the metadata.
    #
    # This method is fairly cheap most of the time. We only store metadata on
    # dicts and key dicts are extremely rare in practice. Thus, this method fast
    # path-s almost all the time.
    private def as_key(key : Term | Term::Any) : Term
      key = Term.of(key)
      return key unless @addons.location?
      return key unless key.type.dict?

      # Search in cache.
      @as_key_cache.each do |entry|
        next unless entry
        from, to = entry
        next unless key.same?(from)
        return to
      end

      to = Term.patch(key) do |node|
        if nodedict = node.as_d?
          Term::Patch::ReplaceDescend.new(
            Term.of(
              nodedict
                .without(Term::Sym.byte_start)
                .without(Term::Sym.byte_end))
          )
        else
          Term::Patch::Skip.new
        end
      end

      # Prepend to cache.
      @as_key_cache[3] = @as_key_cache[2]
      @as_key_cache[2] = @as_key_cache[1]
      @as_key_cache[1] = @as_key_cache[0]
      @as_key_cache[0] = {key, to}

      to
    end

    # Datums are "baked" lexemes -- lexemes that were parsed into a term
    # during lexical analysis.
    private def datum : Π
      unless datum = past_datum?
        return refusal("expected a boolean, string, or number", ahead.text.before_begin)
      end

      ok(datum.term)
    end

    # Parses a WwML symbol; standard such as `px-10`, or arising from templating
    # such as in `px⫽y-10`.
    private def symbol : Π
      # Treat standalone `+`, `-`, etc. as a symbol in symbol context: in `(⏏+⏏ 1 2)`,
      # `+` is a symbol.
      case
      when past?(:plus)
        return ok(:+)
      when past?(:minus)
        return ok(:-)
      when past?(:dollar)
        return ok(:"$")
      when past?(:caret)
        return ok(:^)
      when past?(:caret_star)
        return ok(:"^*")
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

      ok(Term.of(Term::Sym.new(name)))
    end

    # Parses a double-quoted string.
    private def stringdq : Π
      unless past?(:dquote_left)
        return refusal("expected a string", ahead.text.before_begin)
      end

      segment0 = segments = nil

      until past?(:dquote_right)
        case
        when segment = past_datum?(&.type.string?)
          if segment0
            segments ||= [] of Term
            segments << segment.term
          else
            segment0 = segment.term
          end
        when past?(:tl_half_bracket)
          segment, _ = capture!(slot)

          if segment0
            segments ||= [] of Term
            segments << segment
          else
            segment0 = segment
          end

          unless past?(:tr_half_bracket)
            return failure("expected `⸣` to end string interpolation", ahead.text.before_begin)
          end
        else
          unreachable("unexpected lexeme after dquote_left")
        end
      end

      case {segment0, segments}
      when {.nil?, .nil?}
        # ""
        ok("")
      when {Term, .nil?}
        if segment0.type.string?
          # "foo"
          ok(segment0)
        else
          # "⸢x⸣"
          ok(Term.of(:~, segment0))
        end
      when {Term, Array(Term)}
        # "foo ⸢bar⸣ baz"
        stitch = Term::Dict.build do |commit|
          commit << :~ << segment0
          commit.concat(segments)
        end

        ok(stitch)
      else
        unreachable
      end
    end

    private macro blank!(key, keysrc, &block)
      pass({{key}}, {{keysrc}}) do |%key, %keysrc|
        next unless %keysym = %key.as_sym?
        next unless %blank = %keysym.blank?

        unless %name = %blank.name?
          return failure("expected a named blank", %keysrc)
        end

        unless %blank.single?
          return failure("expected a singular blank (but what you've got here is a polyblank)", %keysrc)
        end

        %type = %blank.type

        pass(%name, %type) {{block}}
      end
    end

    # Parses a prefix selector.
    private def s_prefix : Π
      case
      when past?(:colon_left)
        # {¦ :⏏xyz} -> xyz: xyz
        term, _ = capture!(atom)

        ok(Term[].with(as_key(term), term))
      when past?(:triple_colon_left)
        # {¦ ⋮⏏x} -> xyz: (%- (%never) xyz)
        term, _ = capture!(atom)

        ok(Term[].with(as_key(term), {:"%-", {:"%never"}, term}))
      when past?(:minus), past?(:minus_left)
        # -⏏a -> a: (%- _)
        # -⏏a: foo -> a: (%- _ foo)
        # -⏏a_number -> a: (%- _number a)
        key, keysrc = capture!(atom)
        if past?(:colon_right)
          name, _ = capture!(atom)
        end

        blank!(key, keysrc) do |name, type|
          return ok(Term[].with(as_key(name), {:"%-", type.blank, name || name}))
        end

        if name
          return ok(Term[].with(as_key(key), {:"%-", :_, name}))
        end

        ok(Term[].with(as_key(key), {:"%-", :_}))
      when past?(:plus_minus)
        # ±⏏x -> x: x←_number
        # ±⏏x: foo -> x: foo←_number
        key, _ = capture!(atom)

        if past?(:colon_right)
          name, _ = capture!(atom)
        end

        ok(Term[].with(as_key(key), {:"%let", name || key, :_number}))
      else
        refusal("not a prefix selector", ahead.text)
      end
    end

    # Parses an infix selector.
    private def s_infix : Π
      key, keysrc = capture!(atom)

      case
      when past?(:sup_plus_right)
        # As in {¦ x⁺⏏}
        #
        # x⁺ -> x: x←true
        ok(Term[].with(as_key(key), {:"%let", key, true}))
      when past?(:sup_minus_right)
        # As in {¦ x⁻⏏}
        #
        # x⁻ -> x: x←false
        ok(Term[].with(as_key(key), {:"%let", key, false}))
      when past?(:colon_right)
        # As in {¦ x:⏏ y}
        #
        # x: y
        # x_: y -> x: x←y
        # x_<type>: y -> x: x←(%all y _<type>)
        value, _ = capture!(slot, expect: true)

        blank!(key, keysrc) do |name, type|
          Term.case(value, engine: M0) do
            # Fix x_: (%optional <> <>) -> x: (%optional <> x←<>) to avoid confusion.
            match({:"%optional", :default_, :pattern_}, cue: :"%optional") do |default, pattern|
              if type.any?
                return ok(Term[].with(as_key(name), {:"%optional", default, {:"%let", name, pattern}}))
              end

              return ok(Term[].with(as_key(name), {:"%optional", default, {:"%let", name, {:"%all", type.blank, pattern}}}))
            end

            otherwise do
              if type.any?
                return ok(Term[].with(as_key(name), {:"%let", name, value}))
              end

              return ok(Term[].with(as_key(name), {:"%all", key, value}))
            end
          end
        end

        ok(Term[].with(as_key(key), value))
      when past?(:triple_colon_right)
        # As in {¦ x⋮⏏ y}
        #
        # x⋮ y -> x: (%optional y x←_<type of y>)
        # x_⋮ y -> x: (%optional y x_)
        # x_<type>⋮ y -> x: (%optional <default for type> y)
        value, _ = capture!(slot, expect: true)

        blank!(key, keysrc) do |name, type|
          if type.any?
            return ok(Term[].with(as_key(name), {:"%optional", value, key}))
          end

          return ok(Term[].with(as_key(name), {:"%optional", ML.initial(type), value}))
        end

        capture = Term::Sym.new(String.build { |io| io << key << value.type.blank })

        ok(Term[].with(as_key(key), {:"%optional", value, capture}))
      else
        # As in {¦ xyz⏏}
        #
        # xyz        -> xyz: _
        # xyz_<type> -> xyz: xyz_<type>

        blank!(key, keysrc) do |name, _|
          return ok(Term[].with(as_key(name), key))
        end

        ok(Term[].with(as_key(key), :_))
      end
    end

    private def selector : Π
      choice(
        s_prefix,
        s_infix,
        refusal("expected a selector", ahead.text.before_begin)
      )
    end

    # Parses selectors until the block returns `true` (e.g. terminating token ahead).
    private def selectors(& : -> Bool) : Π
      selection = Term::Dict.build do |commit|
        until yield
          selected, selectedsrc = capture!(selector)
          selected.each_entry do |key, value|
            if commit[key]?
              return failure("duplicate key `#{ML.compact(key)}` in selection", selectedsrc)
            end

            commit.with(key, value)
          end
        end
      end

      ok(selection)
    end

    # Parses a dict layer, as in `(¦ ⏏ _ a b c)`. Selectors are parsed
    # until the block returns `true`.
    private def layer(& : -> Bool) : Π
      residue = Term[]

      txn do
        case π = slot
        in Parseout::Ok
          # If `(¦ a⏏ b c)`, then residue = a, pairspattern = (b c)
          # If `(¦ a⏏ :b c)`, then residue = (a), pairspattern = (b: b c)
          # If `(¦ a⏏: b c)`, then residue = (), pairspattern = (a: b c)
          if ahead?(:colon_right)
            next refusal("expected a residual term", ahead.text.before_begin)
          end

          residue = π.term
        in Parseout::Err
        end

        π
      end

      selection, _ = capture!(selectors { yield })

      # (¦) -> (%partition () ()) -> ()
      if residue.empty? && selection.empty?
        return ok(Term[])
      end

      # {¦} -> _dict
      if residue == Term[:_] && selection.empty?
        return ok(:_)
      end

      # (¦ a: 1 b: 2) -> {a: 1, b: 2}
      if residue.empty?
        return ok(selection)
      end

      # (¦ x_) -> x_
      if selection.empty?
        return ok(residue)
      end

      ok({:"%layer", residue, selection})
    end

    private def dict : Π
      unless lparen = past?(:lparen)
        return refusal("expected `(` to start a dict", ahead.text.before_begin)
      end

      case π = section
      in Parseout::Ok
        inner = π.term
      in Parseout::Refusal
        inner = Term.of
      in Parseout::Failure
        return π
      end

      interfix = ->(term : Term) { term }

      # Parse interfix
      case
      when ahead?(:broken_bar),
           ahead?(:bar_underscore),
           ahead?(:double_asterisk)
        if past?(:broken_bar)
          pairspattern, _ = capture!(layer { ahead?(:rparen) }, expect: true)
        elsif past?(:bar_underscore)
          # ⍊ -> ¦ _
          selection, _ = capture!(selectors { ahead?(:rparen) }, expect: true)
          pairspattern = Term.of(:"%layer", :_, selection)
        elsif ahead?(:double_asterisk)
          # ⁑ -> _* ¦ _
          inner = Term.of(inner.append(:"_*"))
          forward
          selection, _ = capture!(selectors { ahead?(:rparen) }, expect: true)
          pairspattern = Term.of(:"%layer", :_, selection)
        else
          unreachable
        end

        interfix = ->(term : Term) do
          unless dict = term.as_d?
            return term
          end

          if dict.empty? && pairspattern.empty?
            # (¦) -> ()
            Term.of
          elsif dict.itemsonly?
            Term.of(:"%partition", term, pairspattern)
          else
            # %all+%layer is somewhat nastier on the pattern matching side
            # (might sometimes process more entries); but it is a good fallback
            # if the client gives us something that has pairs *and* a pairspattern,
            # as in: `(point x: 0 ¦ _ -y)`, which could be useful if the client wants
            # to avoid pairspattern treatment of certain keys or values.
            Term.of(:"%all", term, pairspattern)
          end
        end
      when past?(:caret_ellipsis)
        # Alloy ^extend interfix
        extra, _ = capture!(atom, expect: true)
        interfix = ->(term : Term) { Term.of(:"^extend", term, extra) }
      end

      unless rparen = past?(:rparen)
        return failure("expected `)` to end the dict", ahead.text.before_begin)
      end

      outer = interfix.call(inner)

      if @addons.location?
        outer = outer.morph(
          {Term::Sym.byte_start, lparen.text.byte_start},
          {Term::Sym.byte_end, rparen.text.byte_end},
        )
      end

      ok(outer)
    end

    # ⏏[a b c]
    private def itemspattern : Π
      unless past?(:lbracket)
        return refusal("expected `[` to begin an itemspattern", ahead.text.before_begin)
      end

      items = Term::Dict.build do |commit|
        until past?(:rbracket)
          if ahead?(:colon_left) || ahead?(:colon_right)
            return failure("cannot use pairs in an itemspattern", ahead.text)
          end

          item, _ = capture!(
            choice(
              slot,
              failure("expected an item, or `]` to end the itemspattern", ahead.text.before_begin)
            )
          )

          commit << item
        end
      end

      ok({:"%partition", items, :_})
    end

    # ⏏{¦ a b c}
    private def pairspattern : Π
      case
      when past?(:lcurly_broken_bar)
      when ahead?(:lcurly)
        itemsname, _ = capture!(txn do
          # {⏏
          forward

          case π1 = atom
          in Parseout::Ok
            unless past?(:broken_bar)
              next refusal("expected `¦`", ahead.text.before_begin)
            end
            # {xyz¦⏏
          in Parseout::Err
          end

          π1
        end)
      else
        return refusal("expected a pairspattern", ahead.text.before_begin)
      end

      selection, _ = capture!(selectors { ahead?(:rcurly) }, expect: true)

      # {¦ a b c⏏}
      forward

      # {¦} -> _dict
      if selection.empty?
        return ok(:_dict)
      end

      if itemsname
        ok({:"%partition", {:"%let", itemsname, :_}, {:"%layer", :_, selection}})
      else
        ok({:"%layer", :_, selection})
      end
    end

    private def keypool0 : Π
      unless past?(:lcurly_percent)
        return refusal("expected `{%` to begin keypool", ahead.text.before_begin)
      end

      keypool = Term::Dict.build do |commit|
        commit << :"%keypool"

        until past?(:rcurly)
          if ahead?(:colon_left) || ahead?(:colon_right)
            return failure("cannot use pairs in keypool", ahead.text)
          end

          item, _ = capture!(
            choice(
              slot,
              failure("expected a key, or `}` to end the keypool", ahead.text.before_begin)
            )
          )

          commit << item
        end
      end

      ok(keypool)
    end

    # ⏏{% a b c}
    private def keypool : Π
      pattern, patternsrc = capture!(keypool0)

      Term.case(pattern, engine: M0) do
        match({:"%keypool"}, cue: :"%keypool") do
          # (%keypool)
          return failure("empty %keypool makes no sense, did you mean `{}`?", patternsrc)
        end

        otherwise do
          ok(pattern)
        end
      end
    end

    # {+ ...}
    # {- ...}
    # {+¦ ...}
    # {-¦ ...}
    private def set : Π
      case
      when past?(:lcurly_plus)
        insert = ->(commit : Term::Dict::Commit, item : Term) { commit.with(item, true) }
        surround = ->(set : Term::Dict) { set }
      when past?(:lcurly_minus)
        insert = ->(commit : Term::Dict::Commit, item : Term) { commit.with(item, false) }
        surround = ->(set : Term::Dict) { set }
      when past?(:lcurly_plus_broken_bar)
        insert = ->(commit : Term::Dict::Commit, item : Term) { commit.with(item, true) }
        surround = ->(set : Term::Dict) { Term.of(:"%layer", :_, set) }
      when past?(:lcurly_minus_broken_bar)
        insert = ->(commit : Term::Dict::Commit, item : Term) { commit.with(item, false) }
        surround = ->(set : Term::Dict) { Term.of(:"%layer", :_, set) }
      else
        return refusal("expected a dict set", ahead.text.before_begin)
      end

      set = Term::Dict.build do |commit|
        until past?(:rcurly)
          if ahead?(:colon_left) || ahead?(:colon_right)
            return failure("cannot use pairs in dict set", ahead.text)
          end

          item, itemsrc = capture!(
            choice(
              slot,
              failure("expected a key, or `}` to end the dict set", ahead.text.before_begin),
            )
          )

          key = as_key(item)

          if commit[key]?
            return failure("duplicate item in dict set: `#{ML.compact(item)}`", itemsrc)
          end

          insert.call(commit, key)
        end
      end

      ok(surround.call(set))
    end

    # {# a b a a}
    # {# a 5×b a a}
    # {#¦ a b a a}
    # {#¦ a 5×b a a}
    private def mset : Π
      case
      when past?(:lcurly_hash)
        open = false
      when past?(:lcurly_hash_broken_bar)
        open = true
      else
        return refusal("expected `{#` or `{#¦`", ahead.text.before_begin)
      end

      mset = Term::Dict.build do |commit|
        until past?(:rcurly)
          if ahead?(:colon_left) || ahead?(:colon_right)
            return failure("cannot use pairs in dict multiset", ahead.text)
          end

          item, termsrc0 = capture!(slot)
          step = Term[1]

          if past?(:times)
            unless step = item.as_n?
              return failure("expected a number for count", termsrc0)
            end

            item, _ = capture!(slot)
          end

          key = as_key(item)
          tally = commit[key]? || Term[0]

          commit.with(key, tally + step)
        end
      end

      ok(open ? {:"%layer", :_, mset} : mset)
    end

    private def pairs : Π
      unless past?(:lcurly)
        return refusal("expected `{`", ahead.text.before_begin)
      end

      pairs = Term::Dict.build do |commit|
        until past?(:rcurly)
          # ⏏:qux
          if past?(:colon_left)
            key, keysrc = capture!(
              choice(
                slot,
                failure("expected a key to follow `:`, for example: `:foo`; did you forget a space, as in: `: foo`?", ahead.text.before_begin),
              )
            )

            if commit[as_key(key)]?
              return failure("duplicate key `#{ML.compact(key)}` in dict", keysrc)
            end

            commit.with(as_key(key), key)

            next
          end

          key, keysrc = capture!(
            choice(
              slot,
              failure("expected a key, or `}` to end the dict", ahead.text.before_begin),
            )
          )

          unless past?(:colon_right)
            return failure("expected `:` after key", keysrc.after_end)
          end

          value, _ = capture!(
            choice(
              slot,
              failure("expected a value, or `}` to end the dict", ahead.text.before_begin),
            )
          )

          if commit[as_key(key)]?
            return failure("duplicate key `#{ML.compact(key)}` in dict", keysrc)
          end

          commit.with(as_key(key), value)
        end
      end

      ok(pairs)
    end

    # ⏏⟨⟩
    # ⏏⟨⟩°
    private def split : Π
      opening = ahead

      unless past?(:langle)
        return refusal("expected `⟨`", ahead.text.before_begin)
      end

      decorate = ->(term : Term) { term }
      source = false

      parts = [] of Array(Term)
      part = [] of Term

      postpart = false

      loop do
        # Detect part boundary.
        case
        when ahead?(:rangle),
             ahead?(:rangle_source),
             ahead?(:ellipsis),
             ahead?(:broken_bar),
             ahead?(:bar_underscore)
          if part.empty?
            unless postpart
              return failure("expected at least one item in split part", ahead.text.before_begin)
            end
          else
            # Commit nonempty part.
            parts << part
            part = [] of Term
          end

          # Now actually distinguish the lexeme ahead.
          case
          when past?(:rangle)
          when past?(:rangle_source)
            source = true
          when past?(:ellipsis)
            next
          when past?(:broken_bar)
            layer, _ = capture!(layer { ahead?(:rangle) || ahead?(:rangle_source) }, expect: true)
            decorate = ->(kernel : Term) { Term.of(:"%partition", kernel, layer) }

            # ⏏⟩
            # ⏏⟩°
            postpart = true
            next
          when past?(:bar_underscore)
            # ⍊ -> ¦ _
            selection, _ = capture!(selectors { ahead?(:rangle) || ahead?(:rangle_source) }, expect: true)
            decorate = ->(kernel : Term) { Term.of(:"%partition", kernel, Term.of(:"%layer", :_, selection)) }
            postpart = true
            next
          else
            unreachable
          end

          break
        end

        item, _ = capture!(
          choice(
            slot,
            failure("expected an item, `…`, `¦`, or `⟩` to end the split", ahead.text.before_begin)
          )
        )

        part << item
      end

      # opening points to the opening `⟨`
      # closing points to the closing `⟩` or `⟩°`
      closing = behind

      split(parts, source, decorate, span: opening.text.span(closing.text))
    end

    # :nodoc:
    private def split(parts, source : Bool, decorate, span) : Π
      case parts.size
      when 1
        conf = {Term.of(:_), parts[0], Term.of(:_), [] of Term, Term.of(:_)}
      when 2
        conf = {Term.of(:_), parts[0], Term.of(:_), parts[1], Term.of(:_)}
      when 3
        unless parts[0].size == 1
          return failure("expected the left part to contain exactly one item", span)
        end

        unless parts[2].size == 1
          return failure("expected the right part to contain exactly one item", span)
        end

        conf = {parts[0][0], parts[1], Term.of(:_), [] of Term, parts[2][0]}
      when 4
        unless parts[0].size == 1
          return failure("expected the left part to contain exactly one item", span)
        end

        unless parts[3].size == 1
          return failure("expected the right part to contain exactly one item", span)
        end

        conf = {parts[0][0], parts[1], Term.of(:_), parts[2], parts[3][0]}
      when 5
        unless parts[0].size == 1
          return failure("expected the left part to contain exactly one item", span)
        end

        unless parts[2].size == 1
          return failure("expected the middle part to contain exactly one item", span)
        end

        unless parts[4].size == 1
          return failure("expected the right part to contain exactly one item", span)
        end

        conf = {parts[0][0], parts[1], parts[2][0], parts[3], parts[4][0]}
      else
        return failure("expected 1-5 split parts, not #{parts.size}", span)
      end

      interior = Term.of(split5(*conf, source))

      ok(decorate.call(interior))
    end

    # :nodoc:
    #
    # Renders the base split ⟨l … xs … m … ys … r⟩[°] with potential simplifications
    # based on the presence of l/xs/m/ys/r.
    #
    # TODO: since %split is currently unimplemented, we don't have any way to test
    # how well the generated splits work and whether they work at all.
    private def split5(l : Term, xs : Indexable(Term), m : Term, ys : Indexable(Term), r : Term, source : Bool) : Term::Dict
      # Use %item[°] for cases such as ⟨a b c⟩.
      if l == Term[:_] && !xs.empty? && m == Term[:_] && ys.empty? && r == Term[:_]
        operator = Term::Dict.build do |commit|
          commit << (source ? :"%item°" : :"%item")
          commit.concat(xs)
        end

        return operator
      end

      op = source ? Term[:"%split°"] : Term[:"%split"]

      if xs.empty?
        xs0 = Term.of(:_)
        xsr = Term[]
      else
        xs0 = xs[0]
        xsr = Term::Dict.build do |commit|
          (1...xs.size).each { |index| commit << xs[index] }
        end
      end

      if ys.empty?
        if m == Term[:_]
          return Term[op, l, xs0, xsr.empty? ? r : xsr.append(r == Term[:_] ? :"_*" : {:"%group", r, :"_*"})]
        end

        ys0 = Term.of(:_)
        ysr = Term[]
      else
        ys0 = ys[0]
        ysr = Term::Dict.build do |commit|
          (1...ys.size).each { |index| commit << ys[index] }
        end
      end

      Term[op, l, xs0,
        {op,
         xsr.empty? ? m : xsr.append(m == Term[:_] ? :"_*" : {:"%group", m, :"_*"}),
         ys0,
         ysr.empty? ? r : ysr.append(r == Term[:_] ? :"_*" : {:"%group", r, :"_*"})}]
    end

    UNDERSCORE_QUESTION = Term.of(:"%past", :_, min: 0, max: 1)

    # Pure shorthands like `_?`.
    private def shorthand : Π
      case
      when past?(:underscore_question)
        ok(UNDERSCORE_QUESTION)
      else
        refusal("expected a shorthand", ahead.text.before_begin)
      end
    end

    private def term : Π
      choice(
        dict,
        datum,
        symbol,
        stringdq,
        itemspattern,
        pairspattern,
        pairs,
        set,
        mset,
        keypool,
        split,
        shorthand,
      )
    end

    private def sigil : Π
      case
      when past?(:at_sign)
        wrap = ->(arg : Term) { Term.of(:edge, arg) }
      when past?(:arrow_up)
        wrap = ->(arg : Term) { Term.of(:"$up", arg) }
      when past?(:arrow_dn)
        wrap = ->(arg : Term) { Term.of(:"$down", arg) }
      when past?(:arrow_right)
        wrap = ->(arg : Term) { Term.of(:"$my", arg) }
      when past?(:caret_left)
        wrap = ->(arg : Term) { Term.of(:^, arg) }
      when past?(:caret_star_left)
        wrap = ->(arg : Term) { Term.of(:"^*", arg) }
      when past?(:quote)
        wrap = ->(arg : Term) { Term.of(:literal, arg) }
      when past?(:dollar_left)
        wrap = ->(arg : Term) { Term.of(:"$", arg) }
      when past?(:dollar_quote)
        wrap = ->(arg : Term) { Term.of(:"$once", arg) }
      when past?(:percent_quote)
        wrap = ->(arg : Term) { Term.of(:"%literal", arg) }
      when past?(:triple_equals)
        wrap = ->(arg : Term) { Term.of(:"%nonself", arg) }
      when past?(:backquote)
        wrap = ->(arg : Term) { Term.of(:"%slot", arg) }
      when past?(:plus_minus)
        wrap = ->(arg : Term) { Term.of(:"%let", arg, :_number) }
      when past?(:plus_left)
        wrap = ->(arg : Term) { arg.type.number? ? arg : Term.of(:+, arg) }
      when past?(:minus_left)
        wrap = ->(arg : Term) { arg.type.number? ? Term.of(-arg) : Term.of(:-, arg) }
      else
        return refusal("expected a sigil", ahead.text.before_begin)
      end

      arg, _ = capture!(atom, expect: true)

      ok(wrap.call(arg))
    end

    # Parses the tuck operator: `⏏⁰⏏x`, `⏏¹⁻⁵⏏x` etc.
    private def tuck : Π
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

      arg, _ = capture!(atom, expect: true)

      offsets.reverse_each do |offset|
        arg = tuck_pad(arg, offset)
      end

      ok(arg)
    end

    private def tuck_pad(arg : Term, offset : Int32) : Term
      padded = Term::Dict.build do |commit|
        offset.times do
          commit << :_
        end

        commit << arg
        commit << :"_*"
      end

      Term.of(padded)
    end

    private def item_parens : Π
      unless past?(:bl_half_bracket)
        return refusal("expected `⸤`", ahead.text.before_begin)
      end

      value, _ = capture!(item)

      unless past?(:br_half_bracket)
        return failure("expected `⸥` to close the `⸤`", ahead.text.before_begin)
      end

      ok(value)
    end

    private def placeholder : Π
      anchor = ahead.text.byte_start

      case
      when past?(:diamond)
        ok(Term::Sym.rule_id(anchor, blank: false))
      when past?(:diamond_underscore)
        ok(Term::Sym.rule_id(anchor, blank: true))
      when past?(:rrect)
        ok(Term::Sym.rule_block_id(anchor, blank: false))
      when past?(:rrect_underscore)
        ok(Term::Sym.rule_block_id(anchor, blank: true))
      else
        return refusal("expected a placeholder", ahead.text.before_begin)
      end
    end

    private def atom : Π
      choice(
        term,
        sigil,
        tuck,
        item_parens,
        placeholder,
        refusal("expected a term", ahead.text.before_begin),
      )
    end

    # (atom ↣)* atom (↢ atom)*
    private def pend : Π
      lhs = [] of Term
      mid = nil
      midsrc = nil
      rhs = [] of Term

      arg, argsrc = capture!(atom)

      loop do
        case
        when past?(:arrow_right_tail)
          # x↣y
          if mid
            return failure("cannot use prepend in append block", behind.text)
          end

          lhs << arg
        when past?(:arrow_left_tail)
          # x↢y
          if mid
            rhs << arg
          else
            mid = arg
            midsrc = argsrc
          end
        else
          if mid
            rhs << arg
          else
            mid = arg
            midsrc = argsrc
          end
          break
        end

        arg, argsrc = capture!(atom)
      end

      unless mid && midsrc
        unreachable("loop(&) block not called")
      end

      if lhs.empty? && rhs.empty?
        return ok(mid)
      end

      unless dict = mid.as_d?
        return failure("expected a read-time dict", midsrc)
      end

      unless lhs.empty?
        lhs.reverse_each do |item|
          dict = dict.prepend(item)
        end
      end

      unless rhs.empty?
        rhs.each do |item|
          dict = dict.append(item)
        end
      end

      ok(dict)
    end

    private def slot : Π
      l, _ = capture!(pend)

      case
      when past?(:arrow_left)
        # x←y -> (%let x y)
        wrap = ->(x : Term, y : Term) { ok({:"%let", x, y}) }
      else
        return ok(l)
      end

      r, _ = capture!(slot, expect: true)

      wrap.call(l, r)
    end

    private def entry : DictEntry | Parseout::Err
      comments = [] of StringView

      # Collect comments that we'll possibly attach to the entry.
      if @addons.doc_comment?
        @cursor.reverse_each_previous_ignored do |λ|
          break unless λ.is_a?(Lexeme::Token)
          break unless λ.type.line_comment?

          comments.unshift(λ.text)
        end
      end

      # Parse entry state.
      case
      when past?(:semicolon)
        state = :disabled
      when past?(:semicolon_comma)
        state = :focused
      else
        state = :enabled
      end

      # ⏏:qux
      if past?(:colon_left)
        key, keysrc = capture!(
          choice(
            slot,
            failure("expected a key to follow `:`, for example: `:foo`; did you forget a space, as in: `: foo`?", ahead.text.before_begin),
          )
        )

        return DictPair.new(as_key(key), keysrc, key, keysrc, state)
      end

      # Parse the head part of the entry.
      head, headsrc = capture!(slot)

      opsrc = ahead.text

      case
      when past?(:colon_right)
        # x: ⏏y
        body, bodysrc = capture!(slot, expect: "expected a value to follow `:`")
        return DictPair.new(as_key(head), headsrc, body, bodysrc, state)
      when past?(:bidi_arrow)
        # x <> ⏏y
        body, _ = capture!(slot, expect: "expected backmap body")
        rule_term = Term.of(:backmap, head, body)
      when past?(:fat_arrow_right)
        # x => ⏏y
        body, _ = capture!(slot, expect: "expected rule body")
        rule_term = Term.of(:rule, head, body)
      else
        # x⏏
        return DictItem.new(head, headsrc, state)
      end

      unless comments.empty?
        doc = Term::Dict.build do |commit|
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

            commit << content
          end
        end
      end

      rule = DictRule.new(rule_term, opsrc, Term.of(doc), state)
      rule.patched
    end

    private def block_delimiter?(lexeme : Lexeme::Token) : Bool
      lexeme.type.blank_line? || lexeme.type.double_blank_line? || lexeme.type.white_rectangle?
    end

    private def block_delimiter?(lexeme) : Bool
      false
    end

    private def entry_block : DictEntryBlock | Parseout::Err
      entries = [] of DictEntry

      loop do
        delimiter = false

        unless entries.empty?
          @cursor.each_previous_ignored do |lexeme|
            next unless block_delimiter?(lexeme)
            delimiter = true
            break
          end
        end

        break if delimiter

        case π = entry
        in DictEntry
          entries << π
        in Parseout::Refusal
          break
        in Parseout::Failure
          return π
        end
      end

      if entries.empty?
        return refusal("expected a nonempty entry block", ahead.text.before_begin)
      end

      DictEntryBlock.new(entries)
    end

    # Parses a WwML dictionary item.
    #
    # NOTE: rules are valid dictionary items.
    #
    # NOTE: you **must** run `Reader.validate` if you're running this as the main
    # reading method.
    def item : Π
      case π = entry
      in DictItem, DictRule
        ok(π.term)
      in DictPair
        refusal("expected an item, not pair", π.keysrc)
      in Parseout::Err
        return π
      end
    end

    # Parses a WwML section.
    #
    # *allow_empty* enables or disable failure if the section is empty.
    #
    # NOTE: you **must** run `Reader.validate` if you're running this as the main
    # reading method.
    def section(*, allow_empty : Bool = false) : Π
      blocks = [] of DictEntryBlock

      loop do
        case π = entry_block
        in DictEntryBlock
          blocks << π
        in Parseout::Refusal
          break
        in Parseout::Failure
          return π
        end
      end

      if blocks.empty?
        if allow_empty
          return ok(Term.of)
        else
          return refusal("expected a nonempty section", ahead.text.before_begin)
        end
      end

      # Instantiate the blocks by replacing ▢ and ▢_ in rules with the block id.
      entries = [] of DictEntry

      has_focused_items = false
      has_focused_pairs = false
      has_focused_rules = false

      blocks.each do |block|
        block.patched.entries.each do |entry|
          entries << entry

          next unless entry.state == :focused

          case entry
          in DictItem then has_focused_items = true
          in DictPair then has_focused_pairs = true
          in DictRule then has_focused_rules = true
          end
        end
      end

      section = Term::Dict.build do |commit|
        entries.each do |entry|
          next if entry.state == :disabled

          case entry
          in DictItem
            next if has_focused_items && entry.state != :focused

            commit << entry.term
          in DictPair
            next if has_focused_pairs && entry.state != :focused

            if commit[entry.key]?
              return failure("duplicate key `#{ML.compact(entry.key)}` in dict", entry.keysrc)
            end

            commit.with(entry.key, entry.value)
          in DictRule
            next if has_focused_rules && entry.state != :focused

            commit << entry.term.morph({:doc, entry.doc})
          end
        end
      end

      ok(section)
    end

    # Parses a WwML document dict.
    #
    # NOTE: you **must** run `Reader.validate` if you're running this as the main
    # reading method.
    def document : Π
      document = Term::Dict.build do |commit|
        default, _ = capture!(section(allow_empty: true))

        commit.with(:default, default)

        loop do
          case
          when past?(:vspace_triple_dash)
            name, namesrc = capture!(atom)
          when ahead?(:vspace_triple_dash_vspace)
            name = Term.of(:aux)
            namesrc = ahead.text
            # ⏏---
            forward
          else
            break
          end

          if commit[name]?
            return failure("duplicate section name: `#{ML.compact(name)}`", namesrc)
          end

          body, _ = capture!(section(allow_empty: true))
          commit.with(as_key(name), body)
        end
      end

      # {default: {...}}
      if document.size == 1 && (default = document[:default]?)
        return ok(default)
      end

      ok(document)
    end

    # :nodoc:
    def toplevel(source : String, parseout π : Parseout::Ok) : Π
      unless ahead?(:eoi)
        return failure("unexpected input", ahead.text.before_begin)
      end

      Term.each_leaf_thorough(π.term) do |leaf|
        next unless symbol = leaf.as_sym?

        case
        when id = symbol.rule_id_sentinel? || symbol.rule_id_blank_sentinel?
          container = "rule"
        when id = symbol.rule_block_id_sentinel? || symbol.rule_block_id_blank_sentinel?
          container = "rule block"
        else
          next
        end

        view = source.view(id.byte_start, byte_size: id.name.bytesize)

        return failure("cannot use `#{id.name}` outside of a #{container}", view)
      end

      π
    end

    # :nodoc:
    def toplevel(source : String, parseout π : Parseout::Err) : Π
      π
    end

    {% if flag?(:docs) %}
      # Performs post-validation of the given *parseout*.
      #
      # - Catches invalid instances of `◇` and `▢`.
      def toplevel(source : String, parseout : Π) : Π
      end
    {% end %}
  end
end

require "./reader/cursor"
require "./reader/dict_entry"
