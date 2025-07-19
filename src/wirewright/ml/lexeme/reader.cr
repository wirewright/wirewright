module Ww::ML::Lexeme
  # The WwML lexeme reader is an object capable of producing a stream of lexemes
  # from a UTF-8-encoded string of WwML source code.
  #
  # The lexeme reader is a fairly important part of reading WwML, because it resolves
  # many of the nasty ambiguities that WwML has evolved to have -- all while staying
  # on the character level. Things like left- and right-colon, caret-as-prefix and caret-
  # as-symbol make lexing rather nontrivial; they are clearly warts, but I am keeping
  # them -- regularity must mix with irregularity for good balance. What is important
  # is that the term reader is relieved of this work. It becomes a trivial, mostly
  # prefix-led recursive descent.
  #
  # Note that you are recommended to use the higher-level method `Lexeme.lexemes`,
  # unless you seek better allocation performance (as in stream-of-lexemes vs. array-
  # of-lexemes).
  #
  # ```
  # rr = ML::Lexeme::Reader.new("(+ 1 2)")
  # rr.next # => Lexeme::Token(:boi)
  # rr.next # => Lexeme::Token(:lparen)
  # rr.next # => Lexeme::Token(:symbol)
  # rr.next # => Lexeme::Datum(:number, 1)
  # rr.next # => Lexeme::Datum(:number, 2)
  # rr.next # => Lexeme::Token(:rparen)
  # rr.next # => Lexeme::Token(:eoi)
  #
  # # Once exhausted, the reader will return EOI forever:
  # rr.next # => Lexeme::Token(:eoi)
  # rr.next # => Lexeme::Token(:eoi)
  # rr.next # => Lexeme::Token(:eoi)
  # ```
  class Reader
    # :nodoc:
    def initialize(@source : String, @runes : Slice(Rune))
      @boi = true
      @rune_index = 0
      @byte_index = 0
    end

    # Constructs a lexeme reader for the given source string.
    #
    # Set *check valid* to `false` to opt out of an initial UTF-8 validity check.
    # You are not advised to do that since the entirety of downstream machinery
    # pretty much assumes valid UTF-8; it's better to raise early and with a helpful
    # error message; than late and with an unhelpful one.
    #
    # Raises `SyntaxError` on invalid input.
    def self.new(source : String, *, check_valid : Bool = true) : Reader
      if check_valid && !source.valid_encoding?
        raise SyntaxError.new("source must be valid UTF-8", source.view.before_begin)
      end

      # NOTE: simdutf saves a millisecond or two on the corpus here, and some
      # allocations; but I don't think they're worth the dependency on stdc++
      runes = [] of Rune

      source.each_char_with_index do |chr, index|
        rune = Rune.new(chr)
        runes << rune
      end

      new(source, runes.to_readonly_slice)
    end

    private def raise(detail : String, text : StringView = ahead1)
      raise SyntaxError.new(detail, text)
    end

    # Returns a view of the beginning-of-input.
    private def boi : StringView
      @source.view(byte_start: 0, byte_size: 0)
    end

    # Returns a view of the end-of-input.
    private def eoi : StringView
      @source.view(byte_start: @source.bytesize, byte_size: 0)
    end

    # Returns `true` if the cursor is positioned immediately after the beginning-
    # of-input.
    private def boi? : Bool
      @boi
    end

    # Returns `true` if the cursor is positioned immediately before the end-
    # of-input.
    private def eoi? : Bool
      @rune_index == @runes.size
    end

    # Returns a view of the character ahead of the cursor (of EOI if none).
    private def ahead1 : StringView
      eoi? ? eoi : @source.view(byte_start: @byte_index, byte_size: ahead.byte_size)
    end

    # Returns a view of the character behind the cursor (of BOI if none).
    private def behind1 : StringView
      boi? ? boi : @source.view(byte_start: @byte_index - behind.byte_size, byte_size: behind.byte_size)
    end

    private def unsafe_behind : Rune
      @runes.unsafe_fetch(@rune_index &- 1)
    end

    private def unsafe_ahead : Rune
      @runes.unsafe_fetch(@rune_index)
    end

    # Returns the rune behind of the cursor (BOI if none).
    private def behind : Rune
      @rune_index.zero? ? Rune.new(:boi, '\0') : unsafe_behind
    end

    # Returns the rune ahead of the cursor (EOI if none).
    private def ahead : Rune
      @rune_index >= @runes.size ? Rune.new(:eoi, '\0') : unsafe_ahead
    end

    # Moves the cursor past the rune ahead. Returns `true` if moved, `false`
    # otherwise (i.e. EOI ahead).
    private def forward : Bool
      if eoi?
        return false
      end

      @byte_index += unsafe_ahead.byte_size
      @rune_index += 1

      true
    end

    # Moves the cursor past zero or more runes accepted by the block. Returns
    # `true` if the cursor moved, `false` otherwise (EOI ahead).
    private def skip(& : Rune -> Bool) : Bool
      while yield ahead
        return false unless forward
      end

      true
    end

    # Moves the cursor past zero or more runes accepted by the block. Returns
    # `true` if skipped at least one rune, `false` otherwise.
    private def skip?(& : Rune -> Bool) : Bool
      result = false

      while yield ahead
        result = true

        break unless forward
      end

      result
    end

    private def past?(&)
      if yield ahead
        forward

        true
      else
        false
      end
    end

    private def past?(object) : Bool
      past? { |rune| rune == object }
    end

    # Moves the cursor past a sequence of runes that match *objects*.
    #
    # `Rune#==` is used to check for equality.
    private def past?(*objects) : Bool
      try? do
        objects.each do |object|
          return false unless past?(object)
        end

        true # commit
      end
    end

    # Returns a `StringView` spanning the portion of the source string
    # the cursor traversed while executing the block. The view may be
    # empty if the cursor did not move during the block.
    private def view(&) : StringView
      byte_start = @byte_index
      yield
      byte_end = @byte_index

      @source.view(byte_start, byte_end: byte_end)
    end

    # Returns the block's result. Additionally, if the result is `nil`, rolls
    # the reader back to the state before the block was called.
    #
    # Used for backtracking in optional or speculative analyses.
    private def try?(& : -> T?) : T? forall T
      state = {@byte_index, @rune_index}

      begin
        result = yield
      ensure
        if result.nil?
          # failed
          @byte_index, @rune_index = state
        end
      end
    end

    # Saves the reader's state for the duration of the block. Restores it
    # after the block returns.
    private def save(& : -> T) : T forall T
      try? { return yield }
    end

    private alias TxnResponse = TxnRevert | TxnCommit
    private alias TxnCommit = TxnReady | TxnToken

    private record TxnRevert
    private record TxnReady, lexeme : Lexeme::Any
    private record TxnToken, type : Lexeme::Token::Type

    private def revert
      TxnRevert.new
    end

    private def ready(lexeme : Lexeme::Any)
      TxnReady.new(lexeme)
    end

    private def token(type : Lexeme::Token::Type)
      TxnToken.new(type)
    end

    # Wraps the block in transaction and view capturing machinery. In this sense,
    # it is a combination of `try?` and `view`.
    #
    # The block must return one of `TxnResponse` types, effectively communicating
    # with this method (i.e., its context): should this method revert, construct
    # a lexical token using the captured view, or simply pass the block's `ready`
    # lexeme to the caller?
    #
    # Returns the resulting lexeme; or `nil` if reverted.
    private def txn?(& : -> TxnResponse) : Lexeme::Any?
      state = {@byte_index, @rune_index}

      begin
        response = yield

        text = @source.view(state[0], byte_end: @byte_index)

        case response
        in TxnRevert
        in TxnReady
          result = response.lexeme
        in TxnToken
          result = Lexeme::Token.new(response.type, text)
        end

        result
      ensure
        if result.nil? # Revert
          @byte_index, @rune_index = state
        end
      end
    end

    # Wraps each branch in a transaction and chooses the first one to succeed.
    # Branches must return `TxnResponse`.
    private macro choice?(*branches)
      pass do
        {% for branch in branches %}
          if %result = txn? { {{branch}} }
            next %result
          end
        {% end %}
      end
    end

    # Skips horizontal whitespace characters and comma `,`. Returns `true` if
    # skipped some, `false` otherwise.
    private def hspaces : Bool
      skip? { |ahead| ahead.hspace? || ahead == ',' }
    end

    private def nows(text : String, *, prefix : Bool = true, &)
      if ahead.space?
        if prefix
          raise "whitespace after prefix `#{text}` not allowed"
        else
          raise "whitespace after `#{text}` not allowed"
        end
      end

      yield
    end

    private def unspaced : TxnResponse
      if ahead.space? # Fast path
        return revert
      end

      case
      when past?('←')
        return nows("←", prefix: false) { token(:arrow_left) }
      when past?('↢')
        return nows("↢", prefix: false) { token(:arrow_left_tail) }
      when past?('↣')
        return nows("↣", prefix: false) { token(:arrow_right_tail) }
      when past?('×')
        return nows("×", prefix: false) { token(:times) }
      when past?(':')
        # :⏏

        if past?('₊') # :₊⏏
          if ahead.space?
            # a:₊⏏ b
            return token(:colon_sub_plus_right)
          else
            # :₊⏏foo
            return token(:colon_sub_plus_left)
          end
        end

        # :⏏
        if ahead.space?
          # a:⏏ b
          return token(:colon_right)
        end
      when past?('⋮')
        # ⋮⏏

        if past?('₊')
          # ⋮₊⏏
          if ahead.space?
            # a⋮₊⏏ b
            return token(:triple_colon_sub_plus_right)
          else
            # ⋮₊⏏foo
            return token(:triple_colon_sub_plus_left)
          end
        end

        # ⋮⏏
        if ahead.space?
          # a⋮⏏ b
          return token(:triple_colon_right)
        end
      when past?('₊')
        # ₊⏏
        if past?(':')
          # ₊:⏏
          if ahead.space?
            # a₊:⏏ b
            return token(:sub_plus_colon_right)
          end
        end

        # ₊:⏏foo
        return token(:sub_plus_colon_left)
      end

      revert
    end

    private def lcurly : TxnResponse
      # We have to go through this hell to distinguish e.g.
      #   {-foo: 100} vs. {- foo}
      response = try? do
        candidate =
          case
          when past?('¦')
            token(:lcurly_broken_bar)
          when past?('…')
            token(:lcurly_ellipsis)
          when past?('+')
            if past?('¦')
              token(:lcurly_plus_broken_bar)
            else
              token(:lcurly_plus)
            end
          when past?('-')
            if past?('¦')
              token(:lcurly_minus_broken_bar)
            else
              token(:lcurly_minus)
            end
          when past?('#')
            if past?('¦')
              token(:lcurly_hash_broken_bar)
            else
              token(:lcurly_hash)
            end
          when past?('%')
            token(:lcurly_percent)
          end

        if ahead.delimiter? && candidate
          # {-⏏ x y z}
          candidate
        end
      end

      response || token(:lcurly)
    end

    private def comment
      skip { |rune| !rune.vspace? }

      token(:line_comment)
    end

    private def vspace : TxnResponse
      unless ahead.vspace?
        return revert
      end

      whitespace = view { skip(&.space?) }

      case whitespace.ee.count('\n')
      when 1
        ready(Lexeme::Empty.new)
      when 2
        token(:blank_line)
      else
        # 3..
        token(:double_blank_line)
      end
    end

    private def superscript
      unless ahead.superscript?
        return revert
      end

      right = save do
        skip(&.superscript?)

        # abc¹²³⏏ xyz
        ahead.delimiter?
      end

      if right && behind.space?
        raise "detached superscript not allowed"
      end

      if right
        case
        when past?('⁺')          then return token(:sup_plus_right)
        when past?('⁻')          then return token(:sup_minus_right)
        when skip?(&.sup_digit?) then return token(:sup_digits_right)
        end
      else
        # abc ¹²³⏏xyz
        case
        when past?('⁺')          then return token(:sup_plus_left)
        when past?('⁻')          then return token(:sup_minus_left)
        when skip?(&.sup_digit?) then return token(:sup_digits_left)
        end
      end

      unreachable("superscript must be called with at least one superscript rune")
    end

    private def subscript : TxnResponse
      unless ahead.subscript?
        return revert
      end

      right = save do
        skip(&.subscript?)

        # abc₁₂₃⏏ xyz
        ahead.delimiter?
      end

      if right && behind.space?
        raise "detached subscript not allowed"
      end

      if right
        case
        when past?('₊')         then return token(:sub_plus_right)
        when past?('₋')         then return token(:sub_minus_right)
        when skip(&.sub_digit?) then return token(:sub_digits_right)
        end
      else
        # abc ₁₂₃⏏xyz
        case
        when past?('₊')         then return token(:sub_plus_left)
        when past?('₋')         then return token(:sub_minus_left)
        when skip(&.sub_digit?) then return token(:sub_digits_left)
        end
      end

      unreachable("subscript must be called with at least one subscript rune")
    end

    private def raw_string : TxnResponse
      unless past?('⎡')
        return revert
      end

      text = view do
        nesting = 1

        loop do
          unless skip { |rune| !rune.in?('⎡', '⎤') }
            raise "improperly terminated raw string literal"
          end

          case
          when past?('⎡')
            nesting += 1
          when ahead == '⎤'
            nesting -= 1
            break if nesting.zero?
            forward
          else
            unreachable
          end
        end
      end

      unless past?('⎤')
        unreachable
      end

      ready(Lexeme::Datum.new(:raw_string, Term.of(text), text))
    end

    private def string : TxnResponse
      unless ahead == '"'
        return revert
      end

      lexemes = [] of Lexeme::Any
      lexemes << Lexeme::Token.new(:dquote_left, view { forward })

      loop do
        prefix = view do
          loop do
            unless skip { |rune| !rune.in?('"', '\\', '⸢') }
              raise "expected `\"` to end the string literal, `\\` to begin an escape sequence, or `⸢` to begin interpolation"
            end

            if past?('\\')
              unless forward
                raise "interpolation ended abruptly"
              end
              next
            end

            break
          end
        end

        unless prefix.empty?
          lexemes << Lexeme::Datum.new(:substring, Term.of(Kit.unescape(prefix)), prefix)
        end

        case
        when ahead == '⸢'
          lexemes << Lexeme::Token.new(:tl_half_bracket, view { forward })

          if ahead.space?
            raise "whitespace after `⸢` not allowed"
          end

          while lexeme = try? { top?.as(Lexeme::Any?) }
            lexemes << lexeme
          end

          # ⸢...⏏
          unless ahead == '⸣'
            raise "expected `⸣` to end string interpolation"
          end

          # ⸢...⏏⸣
          lexemes << Lexeme::Token.new(:tr_half_bracket, view { forward })
        when ahead == '"'
          lexemes << Lexeme::Token.new(:dquote_right, view { forward })
          break
        else
          unreachable
        end
      end

      ready(Lexeme::Many.new(lexemes))
    end

    private def template_stop? : Bool
      return true unless ahead.symbolic?

      try? do
        forward

        if ahead == '⫽'
          # Return triggers a transaction revert here. True means we stop
          # before the character prior to ⫽, as in: x⏏y⫽
          return true
        end

        nil # triggers a revert
      end

      false
    end

    private def template : TxnResponse
      segments = [] of Lexeme::Any

      loop do
        prefix = view do
          until template_stop?
            forward
          end
        end

        unless prefix.empty?
          if segments.empty?
            segments << Lexeme::Token.new(:symbol, prefix)
          else
            segments << Lexeme::Token.new(:symbol_suffix, prefix)
          end
        end

        case ahead
        when '⸨'
          options = [] of StringView
          delay = 0

          text = view do
            forward # ⸨⏏

            loop do
              option = view { skip(&.symbolic?) }
              break if option.empty?

              options << option
              break unless past?(',')
            end

            unless past?('⸩')
              raise "expected `⸩` to close the `⸨` in template"
            end
          end
        when '⟦'
          options = [] of StringView
          delay = 1

          text = view do
            forward # ⟦⏏

            loop do
              option = view { skip(&.symbolic?) }
              break if option.empty?

              options << option
              break unless past?(',')
            end

            unless past?('⟧')
              raise "expected `⟧` to close the `⟦` in template"
            end
          end
        when .symbolic?
          options = [] of StringView
          delay = 0

          text = view do
            options << view { past?(&.symbolic?) }

            while past?('⫽')
              options << view do
                unless past?(&.symbolic?)
                  raise "unexpected character after `⫽`"
                end
              end
            end
          end
        else
          return ready(Lexeme::Many.new(segments))
        end

        if segments.empty?
          segments << Lexeme::Choice.new(:symbol, options, text, delay)
        else
          segments << Lexeme::Choice.new(:symbol_suffix, options, text, delay)
        end
      end
    end

    private def radix(text : StringView) : TxnResponse
      unless ahead.sub_digit?
        return revert
      end

      radix = view { skip(&.sub_digit?) }
      unless ahead.delimiter?
        return revert
      end

      # deadbeef₁₆
      value = Kit.radix(text, radix)

      ready(Lexeme::Datum.new(:number, value, text &+ radix))
    end

    private def symbolic : TxnResponse
      case ahead
      when .symbolic_strong_digit?
        decimal = true
      when .symbolic?
        decimal = false
      else
        return revert
      end

      text = view do
        all_symbolic = try? do
          skip(&.symbolic?)

          ahead.in?('⫽', '⟦', '⸨') ? nil : true
        end

        # Reverted. We've found a template in side the symbolic. Switch to parsing
        # a template then!
        unless all_symbolic
          return template
        end
      end

      # Parse radix notation.
      if ahead.sub_digit?
        if lexeme = txn? { radix(text) }
          return ready(lexeme)
        end
      end

      # Parse decimal form.
      if decimal
        value = Kit.decimal(text)

        return ready(Lexeme::Datum.new(:number, Term.of(value), text))
      end

      # Parse symbol.
      case text
      when "true"
        return ready(Lexeme::Datum.new(:boolean, Term.of(true), text))
      when "false"
        return ready(Lexeme::Datum.new(:boolean, Term.of(false), text))
      end

      ready(Lexeme::Token.new(:symbol, text))
    end

    private def colon_ambiguous?(rune : Rune) : Bool
      rune.symbolic? || rune.paired_right? || rune == '"'
    end

    private def spaced : TxnResponse
      pred = behind

      case
      when past?('(') then return token(:lparen)
      when past?(')') then return token(:rparen)
      when ahead == ':'
        if colon_ambiguous?(pred)
          # foo:⏏bar
          raise "ambiguous use of `:`, add whitespace before or after `:` to clarify"
        end

        forward

        return nows(":") { token(:colon_left) }
      when past?('¦')  then return token(:broken_bar)
      when past?('⍊')  then return token(:bar_underscore)
      when past?('⁑')  then return token(:double_asterisk)
      when past?('{')  then return lcurly
      when past?('}')  then return token(:rcurly)
      when past?('[')  then return token(:lbracket)
      when past?(']')  then return token(:rbracket)
      when past?('…')  then return token(:ellipsis)
      when past?('@')  then return nows("@") { token(:at_sign) }
      when past?('±')  then return nows("±") { token(:plus_minus) }
      when past?('→')  then return nows("→") { token(:arrow_right) }
      when past?('↑')  then return nows("↑") { token(:arrow_up) }
      when past?('↓')  then return nows("↓") { token(:arrow_dn) }
      when past?('\'') then return nows("'") { token(:quote) }
      when past?('`')  then return nows("`") { token(:backquote) }
      when past?('≡')  then return nows("≡") { token(:triple_equals) }
      when past?('⸤')  then return nows("⸤") { token(:bl_half_bracket) }
      when past?('⸥')  then return nows("⸥") { token(:br_half_bracket) }
      when ahead == '⋮'
        if colon_ambiguous?(pred)
          # foo⋮⏏bar
          raise "ambiguous use of `⋮`, add whitespace before or after `⋮` to clarify"
        end

        forward

        return nows("⋮") { token(:triple_colon_left) }
      when past?('⟨')
        return token(:langle)
      when past?('⟩')
        if past?('°')
          return token(:rangle_source)
        else
          return token(:rangle)
        end
      when past?('◇')
        if past?('_')
          return token(:diamond_underscore)
        else
          return token(:diamond)
        end
      when past?('▢')
        if past?('_')
          return token(:rrect_underscore)
        else
          return token(:rrect)
        end
      when past?('+')
        if ahead.content?
          return token(:plus_left)
        else
          return token(:plus)
        end
      when past?('-')
        if past?('-', '-')
          skip(&.hspace?)

          # ---⏏
          if ahead.vspace?
            return token(:vspace_triple_dash_vspace)
          end

          # --- ⏏qux
          return token(:vspace_triple_dash)
        end

        if ahead.content?
          return token(:minus_left)
        else
          return token(:minus)
        end
      when past?('▪')
        return token(:white_rectangle)
      when past?('%')
        if past?('\'')
          return nows("%'") { token(:percent_quote) }
        end
      when past?('$')
        if past?('\'')
          return nows("$'") { token(:dollar_quote) }
        end

        if ahead.content?
          return token(:dollar_left)
        else
          return token(:dollar)
        end
      when past?('^')
        if past?('…')
          return token(:caret_ellipsis)
        end

        if ahead.content?
          return token(:caret_left)
        else
          return token(:caret)
        end
      when past?('<')
        if past?('>') && !ahead.symbolic?
          return token(:bidi_arrow)
        end
      when past?('=')
        if past?('>') && !ahead.symbolic?
          return token(:fat_arrow_right)
        end
      when past?(';')
        case
        when past?(',') then return token(:semicolon_comma)
        when past?(';') then return comment
        else
          return token(:semicolon)
        end
      when ahead == '⟦', ahead == '⸨'
        return template
      when eoi?
        return token(:eoi)
      end

      revert
    end

    private def top? : Lexeme::Any?
      unless ahead.space?
        if lexeme = txn? { unspaced }
          return lexeme
        end
      end

      # Make sure horizontal whitespace is not included in text.
      hspaces

      if ahead.symbolic_strong? && (lexeme = txn? { symbolic })
        return lexeme
      end

      if lexeme = txn? { spaced }
        return lexeme
      end

      if lexeme = choice?(symbolic, vspace, string, raw_string, superscript, subscript)
        return lexeme
      end
    end

    private def top : Lexeme::Any
      if lexeme = top?
        return lexeme
      end

      # Match some presumably common errors to give useful error messages
      # instead of the generic "unexpected character".
      case ahead
      when '←', '↢', '↣', '×'
        raise "whitespace before `#{ahead.chr}` not allowed", behind1
      end

      raise "unexpected character"
    end

    # Returns an *extremely* crude prediction of the number of resulting
    # lexemes based on the number of runes in the source.
    def predicted_lexeme_count : Int32
      (@runes.size * 0.252).to_i
    end

    # Returns the lexeme(s) ahead and moves this reader's cursor past them.
    def next : Lexeme::Any
      if @boi
        @boi = false

        return Lexeme::Token.new(:boi, boi)
      end

      top
    end
  end
end
