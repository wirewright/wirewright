module Ww::ML
  # A lexeme is, in the most general sense, an abstract lexical substructure
  # identified in a string of WwML. Most likely it is, or corresponds to, a group
  # of `Rune`s -- Unicode codepoints with lightweight WwML-specific tagging.
  module Lexeme
    extend self

    alias Any = Empty | One | Many

    # Represents a lexical unit that corresponds to a single atom after instantiation.
    alias One = Atom | Choice

    # Represents an indivisible lexical unit -- a *lexical atom*.
    alias Atom = Token | Datum

    # Represents a "null" or "ignore me" lexeme, emitted sometimes to avoid
    # reentrant lexeme reads.
    record Empty

    # Represents a *lexical token*.
    #
    # - *type* determines the type of the token.
    # - *text* points to the fragment of the source string that generated
    #   this datum.
    record Token, type : Type, text : StringView do
      # Lists the available types of tokens.
      enum Type : UInt8
        # Indicates beginning-of-input.
        BOI
        # Indicates end-of-input.
        EOI
        LineComment
        BrokenBar
        Lparen
        Rparen
        Langle
        Rangle
        RangleSource
        Lbracket
        Rbracket
        Lcurly
        LcurlyBrokenBar
        LcurlyEllipsis
        LcurlyPlus
        LcurlyPlusBrokenBar
        LcurlyMinus
        LcurlyMinusBrokenBar
        LcurlyHash
        LcurlyHashBrokenBar
        LcurlyPercent
        Rcurly
        BLHalfBracket
        BRHalfBracket
        Backquote
        ColonLeft
        ColonRight
        TripleColonLeft
        TripleColonSubPlusLeft
        TripleColonSubPlusRight
        TripleColonRight
        Ellipsis
        TripleEquals
        Semicolon
        SemicolonComma
        PlusLeft
        Plus
        MinusLeft
        Minus
        CaretLeft
        Caret
        DollarLeft
        Dollar
        Symbol
        SymbolSuffix
        DquoteLeft
        DquoteRight
        TLHalfBracket
        TRHalfBracket
        RawString
        ArrowLeft
        ArrowLeftTail
        ArrowRight
        ArrowRightTail
        ArrowUp
        ArrowDn
        DoubleAsterisk
        BarUnderscore
        SupLeft
        SupPlusLeft
        SupMinusLeft
        SupDigitsLeft
        SupRight
        SupPlusRight
        SupMinusRight
        SupDigitsRight
        SubLeft
        SubPlusLeft
        SubMinusLeft
        SubDigitsLeft
        SubRight
        SubPlusRight
        SubMinusRight
        SubDigitsRight
        AtSign
        Quote
        PercentQuote
        DollarQuote
        CaretEllipsis
        SubPlusColonLeft
        SubPlusColonRight
        ColonSubPlusLeft
        ColonSubPlusRight
        PlusMinus
        VspaceTripleDash
        VspaceTripleDashVspace
        WhiteRectangle
        BlankLine
        DoubleBlankLine
        FatArrowRight
        BidiArrow
        Rrect
        RrectUnderscore
        Diamond
        DiamondUnderscore
        Times
      end
    end

    # Represents a lexical unit that was parsed into a `Term` during lexical analysis
    # (whereas normally this is done at term read-time). Numbers and raw strings are
    # examples of Data: they are simply too close to the characters to be parsed at
    # term-read time; it is clearly the job of lexical analysis to do that instead.
    #
    # - *label* specifies the lexical origin of the datum. In other words, it is
    #   the "historical token type" of the datum.
    # - *text* points to the fragment of the source string that generated
    #   this datum.
    record Datum, label : Label, term : Term, text : StringView do
      enum Label : UInt8
        Other
        Number
        Boolean
        RawString
        Substring
      end
    end

    # Represents a *lexical choice*.
    record Choice, type : Token::Type, options : Array(StringView), text : StringView, delay : Int32

    # Represents a nested group of lexemes. Such groups can be produced e.g. by
    # string interpolation, which performs lexical analysis recursively but is
    # still required to return a single lexeme.
    record Many, lexemes : Array(Any)

    private def push(lexemes : Array(One), lexeme : Empty)
    end

    private def push(lexemes : Array(One), lexeme : One)
      lexemes << lexeme
    end

    private def push(lexemes : Array(One), lexeme : Many)
      lexeme.lexemes.each { |child| push(lexemes, child) }
    end

    # Converts a source *string* into a read-only slice of lexemes, flattening
    # `Many`s and ignoring `None`s for your convenience.
    #
    # Raises `SyntaxError` on invalid input.
    #
    # See also: `One`.
    def lexemes(string : String) : Slice(One)
      reader = Reader.new(string)
      lexemes = Array(One).new(reader.predicted_lexeme_count)

      loop do
        lexeme = reader.next
        push(lexemes, lexeme)

        break if lexeme.is_a?(Token) && lexeme.type.eoi?
      end

      lexemes.to_readonly_slice
    end

    enum BlockBoundaryResponse
      # Lexeme is not a block boundary
      No
      # Lexeme is a block boundary. It must be excluded from the delimited
      # block and from the next block -- thus, forming a block of its own.
      Exclusive
      # Lexeme is a block boundary. It must be put at the start of the next block.
      Front
      # Lexeme is a block boundary. It must be put at the end of the delimited block.
      Rear
    end

    # :nodoc:
    def block_boundary?(lexeme : Lexeme::Token) : BlockBoundaryResponse
      case lexeme.type
      when .blank_line?,
           .double_blank_line?,
           .boi?, .eoi?
        BlockBoundaryResponse::Exclusive
      when .vspace_triple_dash?,
           .vspace_triple_dash_vspace?,
           .white_rectangle?
        BlockBoundaryResponse::Front
      else
        BlockBoundaryResponse::No
      end
    end

    # Returns whether *lexeme* is a block boundary and what kind of block
    # boundary it is.
    def block_boundary?(lexeme) : BlockBoundaryResponse
      BlockBoundaryResponse::No
    end

    # Yields each lexical block in *lexemes*. A lexical block is usually delimited
    # by a blank line, a double blank line, etc. A lexical block is the unit of
    # choice instantiation for lexical `Choice`s found in it.
    #
    # Delimiter lexemes are yielded as a separate block unless they are *inclusive*
    # (e.g. `***`), in which case they attach to the end of the block they delimit.
    #
    # NOTE: May emit empty blocks.
    def each_block(lexemes : Slice(One), & : Slice(One) ->) : Nil
      origin = lexemes
      size = 0

      lexemes.each do |lexeme|
        case block_boundary?(lexeme)
        in .no?
          size += 1
        in .exclusive?
          yield origin[0, size] # excluding delimiter
          yield origin[size, 1] # delimiter
          origin += size + 1
          size = 0
        in .front?
          yield origin[0, size]
          origin += size
          size = 1
        in .rear?
          yield origin[0, size + 1]
          origin += size + 1
          size = 0
        end
      end

      return if size.zero?

      yield origin[0, size]

      origin += size
      size = 0
    end

    # Determines the maximum *block arity*.
    #
    # See also: `arity`.
    MAX_BLOCK_ARITY = 8

    # Returns the current delay and block arity of *block*.
    #
    # Block arity is the number of block instances generated from the lexical choice
    # operators `⸨⸩⟦⟧⫽`. In other words, block arity is the common *choice arity* --
    # the number of branches *all* choices in the block have.
    #
    # If different choice arities are found in the same block, raises `SyntaxError`.
    def delay_and_arity?(block : Slice(One)) : {Int32, Int32}?
      # Find minimum delay.
      delay = nil

      block.each do |lexeme|
        next unless lexeme.is_a?(Lexeme::Choice)
        next unless delay.nil? || lexeme.delay < delay

        delay = lexeme.delay
      end

      return unless delay

      arity0 = 0

      block.each do |lexeme|
        next unless lexeme.is_a?(Lexeme::Choice)
        next unless lexeme.delay == delay

        arity1 = lexeme.options.size
        if arity1 > MAX_BLOCK_ARITY
          raise SyntaxError.new("choice arity #{arity1} exceeds the maximum allowed block arity #{MAX_BLOCK_ARITY}", lexeme.text)
        end

        unless arity0.zero? || arity0 == arity1
          raise SyntaxError.new("choice arity mismatch: expected #{arity0} options, but got #{arity1}", lexeme.text)
        end

        arity0 = arity1
      end

      if arity0.zero?
        unreachable("block has choice delay but its arity is zero")
      end

      {delay, arity0}
    end

    # See `arity(Slice(One))`.
    def delay_and_arity?(block : Array(One)) : {Int32, Int32}?
      delay_and_arity?(block.to_readonly_slice)
    end

    # Instantiates *block* and yields lexeme `Atom`s in a flat stream.
    #
    # *__front* and *__back* specify the arrays to reuse.
    def each_instance_in_block(block : Slice(One), *, __front = [] of One, __back = [] of One, & : Atom ->)
      front = block
      back = __back

      (0..).each do |epoch|
        break unless response = delay_and_arity?(front)

        complete = true
        delay, arity = response

        (0...arity).each do |pivot|
          front.each do |lexeme|
            case lexeme
            in Atom
              back << lexeme
            in Choice
              unless lexeme.delay == delay
                complete = false
                back << lexeme.copy_with(delay: lexeme.delay - 1)
                next
              end

              back << Token.new(lexeme.type, lexeme.options[pivot])
            end
          end
        end

        if epoch.zero?
          front = back
          back = __front
        else
          front, back = back, front.as(Array(One))
          back.clear
        end

        break if complete
      end

      front.each do |lexeme|
        yield lexeme.as(Atom)
      end
    ensure
      __front.clear
      __back.clear
    end

    # Instantiates *lexemes*: removes `Choice`s through instantiation, thereby
    # converting a slice of uninstantiated `Lexeme::One`s to one containing
    # lexical atoms only -- thus ,ready for term reading.
    #
    # The returned slice is read-only.
    #
    # May raise `SyntaxError` on invalid input (e.g. choice arity mismatch).
    def atoms(lexemes : Slice(One)) : Slice(Atom)
      # NOTE: Assume 33% growth after instantiation. I'm not sure if it's the right
      # number in the general case, though; needs much more refinement.
      atoms = Array(Atom).new((1.33 * lexemes.size).to_i)

      front = [] of One
      back = [] of One

      each_block(lexemes) do |block|
        next if block.empty?

        each_instance_in_block(block, __front: front, __back: back) do |lexeme|
          atoms << lexeme
        end
      end

      atoms.to_readonly_slice
    end

    # The front-end of WwML lexical analysis: converts a source *string* to
    # a read-only slice of lexical atoms.
    #
    # See also: `Atom`.
    def atoms(string : String) : Slice(Atom)
      pipe(string, lexemes, atoms)
    end
  end
end

require "./lexeme/reader"
