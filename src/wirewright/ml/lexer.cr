module Ww::ML
  # The WwML lexeme reader (*lexer*) is an object capable of producing a stream
  # of `Lexeme`s from a UTF-8-encoded string of WwML source code.
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
  # rr = ML::Lexer.new("(+ 1 2)")
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
  class Lexer
    # :nodoc:
    def initialize(@source : String, @runes : Slice(Rune))
      @boi = true
      @rune_index = 0
      @byte_index = 0
    end

    # Constructs a lexeme reader for the given source string.
    #
    # Raises `SyntaxError` on invalid input.
    def self.new(source : String) : Lexer
      unless source.valid_encoding?
        raise SyntaxError.new("source must be valid UTF-8", "".view) # ?!
      end

      # NOTE: simdutf saves a millisecond or two on the corpus here, and some
      # allocations; but I don't think that's worth the dependency on stdc++
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

    private def subview(byte_start : Int, byte_end : Int) : StringView
      # Since we verify above that `@source.valid_encoding?`, we can create
      # StringViews from it using the unsafe constructor.
      StringView.new(@source, byte_start.to_u32, byte_end.to_u32)
    end

    # Returns a view of the beginning-of-input.
    private def boi : StringView
      subview(byte_start: 0, byte_end: @source.bytesize)
    end

    # Returns a view of the end-of-input.
    private def eoi : StringView
      bytesize = @source.bytesize
      subview(byte_start: bytesize, byte_end: bytesize)
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
      eoi? ? eoi : subview(byte_start: @byte_index, byte_end: @byte_index + ahead.bytesize)
    end

    # Returns a view of the character behind the cursor (of BOI if none).
    private def behind1 : StringView
      boi? ? boi : subview(byte_start: @byte_index - behind.bytesize, byte_end: @byte_index)
    end

    private def prior : StringView
      subview(byte_start: 0, byte_end: @byte_index - behind.bytesize)
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

      @byte_index += unsafe_ahead.bytesize
      @rune_index += 1

      true
    end

    # Moves the cursor past zero or more runes accepted by the block. Returns
    # `true` if the cursor moved, `false` if EOI ahead.
    #
    # TODO: Can we refactor this away in favor of `skip?`? Why do we have two
    # skip functions?
    private def skip(& : Rune -> Bool) : Bool
      while yield ahead
        return false unless forward
      end

      true
    end

    # Moves the cursor past zero or more runes accepted by the block. Returns
    # `true` if skipped at least one rune, `false` otherwise.
    private def skip?(*, limit = Int32::MAX, & : Rune -> Bool) : Bool
      result = false

      while limit > 0 && (yield ahead)
        result = true
        limit -= 1

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

      subview(byte_start, byte_end)
    end

    private def view_and_object(& : -> T) : {StringView, T} forall T
      object = nil
      view = view { object = {yield} }
      {view, *(object || unreachable)}
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

        text = subview(byte_start: state[0], byte_end: @byte_index)

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
      elsif ahead.eoi?
        if prefix
          raise "unexpected end-of-input after prefix `#{text}`"
        else
          raise "unexpected end-of-input after `#{text}`"
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
          when past?('|')
            token(:lcurly_bar)
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

        next unless candidate
        # Reject on things like {-⏏x: 100}
        next unless ahead.visual_boundary?

        # Accept on things like:
        # {-⏏ x y z}
        # {-⏏(x) (y) (z)}
        # {-⏏}
        # {-⏏
        candidate
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

      case whitespace.count('\n')
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
        !ahead.content?
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
        !ahead.content?
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

    private def raw(lparen : Char, rparen : Char, *, caption : String, & : StringView -> TxnResponse) : TxnResponse
      unless past?(lparen)
        return revert
      end

      text = view do
        nesting = 1

        loop do
          unless skip { |rune| !rune.in?(lparen, rparen) }
            raise "improperly terminated #{caption}"
          end

          case
          when past?(lparen)
            nesting += 1
          when ahead == rparen
            nesting -= 1
            break if nesting.zero?
            forward
          else
            unreachable
          end
        end
      end

      assert past?(rparen)

      yield text
    end

    private def raw_string : TxnResponse
      raw('⎡', '⎤', caption: "raw string literal") do |text|
        ready(Lexeme::Datum.new(:raw_string, Term.of(text), text))
      end
    end

    private def raw_symbol : TxnResponse
      raw('⸍', '⸝', caption: "raw symbol literal") do |text|
        ready(Lexeme::Datum.new(:raw_symbol, Term.of(Term::Sym.new(text.to_s)), text))
      end
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

          while lexeme = try? { top_non_eoi? }
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

    private alias BlobMediaType = Term::Blob::Classif | MediaTypeAuto | Nil

    private struct MediaTypeAuto
    end

    # The block should return `true` to terminate. The rune the block returned `true`
    # for is not consumed.
    private def blob_media_type(& : Rune -> Bool) : BlobMediaType
      skip(&.hspace?)

      case
      when past?('?')
        # ⟬dead beef ⁑ ?⏏⟭
        # ⟬dead beef ⁑ ?⏏   ⟭
        skip(&.hspace?)
        # ⟬dead beef ⁑ ?⏏⟭
        # ⟬dead beef ⁑ ?   ⏏⟭

        MediaTypeAuto.new
      else
        media_type_view = view do
          skip?(limit: 64) { |rune| yield rune }
        end
        # ⟬dead beef ⁑ text/html⏏⟭

        # TODO: the errors provided by `MediaType.parse` are rather poorly structured,
        # we'll need to write our own parser with a proper union return type. E.g.:
        #
        #   invalid media type: Invalid '/' at 13 (scratch:1:15)
        #
        # is raised for:
        #
        #   application//
        #
        # Which is quite a strange error message, isn't it?
        media_type = MIME::MediaType.parse(media_type_view.to_s) do |message|
          raise "invalid media type: #{message}", media_type_view
        end

        Term::Blob::Classif.of(media_type)
      end
    end

    # ⟬de ⏏‸Hello World‸ ad be ef⟭
    private def blob_utf8_fragment(io) : Nil
      unless past?('‸')
        raise "expected `‸` to begin a UTF-8 fragment in blob", ahead1.before_begin
      end

      _, prefix = prior.rskip_to("\n")
      x = prefix.size + 1

      first_line_view = view do
        skip { |rune| !(rune == '‸' || rune.vspace?) }
      end

      io << first_line_view

      while past?(&.vspace?)
        io.puts

        # Lookahead
        line_view = save do
          view do
            skip { |rune| !rune.vspace? }
          end
        end

        # Allow blank lines to have any indentation.
        if line_view.blank?
          skip { |rune| !rune.vspace? } # Commit
          next
        end

        x.times do
          past?(' ') || raise "expected indentation to match the `‸` above (including whitespace under `‸` itself)"
        end

        line_view = view do
          skip { |rune| !(rune == '‸' || rune.vspace?) }
        end

        io << line_view
      end

      # ⟬de ‸Hello World⏏‸ ad be ef⟭
      unless past?('‸')
        raise "expected `‸` to end the UTF-8 fragment in blob", ahead1.before_begin
      end
    end

    # ⟬⏏dead beef⟭
    private def blob_interior(io) : BlobMediaType
      loop do
        case
        when past?('⟭')
          # ⟬dead beef⟭⏏
          break
        when past?(&.space?)
          # ⟬dead ⏏beef⟭, i.e., whitespace *between* bytes.
        when digit0 = ahead.hexdigit?
          # ⟬⏏dead beef⟭
          forward
          # ⟬d⏏ead beef⟭

          skip(&.space?)

          unless digit1 = ahead.hexdigit?
            raise "a byte must consist of two hexdigits; use 0 to pad (e.g. `⟬ab c⏏⟭` -> `⟬ab c0⟭`)", ahead1.before_begin
          end
          forward

          # ⟬de⏏ad beef⟭  ⟬dead⏏ beef⟭  . . .
          byte = (digit0.to_u8 << 4) | digit1.to_u8
          io.write_byte(byte)
        when ahead == '‸'
          # ⟬de ⏏‸Hello World‸ ad be ef⟭
          blob_utf8_fragment(io)
          # ⟬de ‸Hello World‸⏏ ad be ef⟭
        when past?('\\')
          # ⟬de \⏏n ad be ef⟭
          case ahead
          when 'a' then io << '\a'
          when 'b' then io << '\b'
          when 'e' then io << '\e'
          when 't' then io << '\t'
          when 'n' then io << '\n'
          when 'f' then io << '\f'
          when 'r' then io << '\r'
          else
            raise "invalid UTF-8 escape sequence in blob, expected one of `abetnfr`"
          end

          forward
          # ⟬de \n⏏ ad be ef⟭
        when past?('⁑')
          # ⟬dead beef ⁑⏏ text/html⟭
          skip(&.space?)
          # ⟬dead beef ⁑ ⏏text/html⟭

          media_type = blob_media_type { |rune| rune != '⟭' }

          unless past?('⟭')
            raise "expected `⟭` to end the blob (note: maximum media type length is 64 characters)", ahead1.before_begin
          end

          # ⟬dead beef ⁑ text/html⟭⏏
          # ⟬dead beef ⁑ ?⟭⏏
          return media_type
        else
          raise "expected hex digit(s), `⁑`, or `⟭` to end the blob"
        end
      end
    end

    private def blob_diagram : Term::Blob
      _, prefix = prior.rskip_to("\n")
      x = prefix.size

      # ╭⏏┤ text/html
      # ┌⏏┤ text/html
      # ┌⏏┤
      unless past?('┤')
        raise "expected a `┤` character after the blob diagram's top corner"
      end

      # ╭┤⏏ text/html
      # ┌┤⏏ text/html
      # ┌┤⏏

      media_type = nil
      unless ahead.vspace?
        media_type = blob_media_type { |rune| !rune.vspace? }
      end

      # ╭┤ text/html⏏
      # ┌┤ text/html⏏
      # ┌┤⏏
      unless past?(&.vspace?)
        raise "expected a newline"
      end

      result = Term::Blob.build do |io|
        loop do
          x.times do
            past?(' ') || raise "expected a whitespace"
          end

          break if past?('└') || past?('╰')

          unless past?('│')
            raise "expected a vertical bar `│`"
          end

          case
          when past?(' ')
            line = view do
              skip { |rune| !rune.vspace? }
            end

            unless past?(&.vspace?)
              raise "expected a newline"
            end

            io.puts(line)
          when past?(&.vspace?)
            io.puts
          else
            raise "expected a newline or one whitespace followed by content after `│`"
          end
        end

        # ╰⏏┤
        unless past?('┤')
          raise "expected `┤` to end the blob diagram"
        end
        # ╰┤⏏
      end

      case media_type
      in Nil
        # No media type specified
      in Term::Blob::Classif
        # Explicit media type
        result = Term::Blob.refine(result, media_type)
      in MediaTypeAuto
        # Guess media type
        result = Term::Blob.classify(result)
      end

      result
    end

    private def blob_literal : Term::Blob
      media_type = nil
      result = Term::Blob.build do |io|
        media_type = blob_interior(io)
      end

      case media_type
      in Nil
        # No media type specified
      in Term::Blob::Classif
        # Explicit media type
        result = Term::Blob.refine(result, media_type)
      in MediaTypeAuto
        # Guess media type
        result = Term::Blob.classify(result)
      end

      result
    end

    private def blob : TxnResponse
      text, blob = view_and_object do
        case
        when past?('┌') || past?('╭')
          blob_diagram
        when past?('⟬')
          blob_literal
        else
          return revert
        end
      end

      ready(Lexeme::Datum.new(:blob, Term.of(blob), text))
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
      if ahead.content?
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

    private def number_approx : TxnResponse
      text, n = view_and_object do
        unless past?('≈')
          return revert
        end

        positive = true

        if past?('+')
        elsif past?('-')
          positive = false
        end

        suffix = view { skip(&.symbolic?) }
        if suffix.empty?
          raise "expected a decimal number, `NaN`, or `Infinity` after `≈`", suffix
        end

        case suffix
        when "NaN"
          magn = Term.of(Term::Num.nan)
        when "Infinity"
          magn = Term.of(Term::Num.infinity)
        else
          magn = Kit.decimal(suffix, exact: false)
        end

        if magn.type.number?
          Term.of(Term::Num.approx(positive ? magn.as_n : Term[-1] * magn.as_n))
        else
          Term.of(positive ? magn : Term.of(:-, magn))
        end
      end

      ready(Lexeme::Datum.new(:number, n, text))
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
      when past?('{')  then return lcurly
      when past?('}')  then return token(:rcurly)
      when past?('[')  then return token(:lbracket)
      when past?(']')  then return token(:rbracket)
      when past?('⸤')  then return token(:bl_half_bracket)
      when past?('⸥')  then return token(:br_half_bracket)
      when past?('…')  then return token(:ellipsis)
      when past?('±')  then return nows("±") { token(:plus_minus) }
      when past?('→')  then return nows("→") { token(:arrow_right) }
      when past?('↑')  then return nows("↑") { token(:arrow_up) }
      when past?('↓')  then return nows("↓") { token(:arrow_dn) }
      when past?('\'') then return nows("'") { token(:quote) }
      when past?('`')  then return nows("`") { token(:backquote) }
      when past?('≡')  then return nows("≡") { token(:triple_equals) }
      when past?('@')
        if past?(':')
          # @:⏏foo
          return nows("@:") { token(:at_sign_colon) }
        end

        return nows("@") { token(:at_sign) }
      when ahead == '⋮'
        if colon_ambiguous?(pred)
          # foo⋮⏏bar
          raise "ambiguous use of `⋮`, add whitespace before or after `⋮` to clarify"
        end

        forward

        return nows("⋮") { token(:triple_colon_left) }
      when past?('⟨')
        if try? { past?('&') && ahead.visual_boundary? }
          # ⟨& ⏏
          return token(:langle_ampersand)
        else
          # ⟨⏏
          # ⟨ ⏏&
          # ⟨⏏&x
          return token(:langle)
        end
      when past?('⟩')
        if past?('°')
          return token(:rangle_source)
        else
          return token(:rangle)
        end
      when past?('⟪')
        return token(:double_langle)
      when past?('⟫')
        if past?('°')
          return token(:double_rangle_source)
        else
          return token(:double_rangle)
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
      when past?('∥')
        return token(:double_pipe)
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
        case
        when past?(':')
          # ^:⏏foo
          return nows("^:") { token(:caret_colon) }
        when past?('…')
          return token(:caret_ellipsis)
        when past?('*')
          if ahead.content?
            return token(:caret_star_left)
          else
            return token(:caret_star)
          end
        else
          if ahead.content?
            return token(:caret_left)
          else
            return token(:caret)
          end
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
        when past?(';')
          return comment
        when past?(',')
          return nows(";,") { token(:semicolon_comma) }
        else
          return nows(";") { token(:semicolon) }
        end
      when past?('_')
        if past?('?') && !ahead.symbolic?
          return token(:underscore_question)
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

      if ahead.symbolic_strong_letter? && (lexeme = txn? { symbolic })
        return lexeme
      end

      if lexeme = txn? { spaced }
        return lexeme
      end

      if lexeme = choice?(symbolic, vspace, string, raw_string, raw_symbol, blob, number_approx, superscript, subscript)
        return lexeme
      end
    end

    private def top_non_eoi? : Lexeme::Any?
      return unless lexeme = top?

      unless lexeme.is_a?(Lexeme::Token)
        return lexeme
      end

      return if lexeme.type.eoi?

      lexeme
    end

    private def top : Lexeme::Any
      if lexeme = top?
        return lexeme
      end

      # Match some presumably common errors to give useful error messages
      # instead of the generic "unexpected character".
      case ahead
      when '←', '×'
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

    def inspect(io)
      @source.to_s.insert(@rune_index, "⏏").to_s(io)
    end
  end
end
