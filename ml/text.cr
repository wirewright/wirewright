module Ww::ML::Text
  # An exclusive range, specifies the begin and end of the source substring that
  # a `Token` occupies.
  record Span, b : Int32, e : Int32

  # Token object used throughout `Lexer` and `Parser`.
  struct Token
    # Returns the type of this token.
    getter type : Symbol

    def initialize(@type : Symbol, @payload : Span | Term, @pos : Int32)
    end

    def self.new(type : Symbol, b : Int32, e : Int32)
      new(type, Span.new(b, e), b)
    end

    # Interprets this term's payload as `Term`.
    def term : Term
      @payload.as(Term)
    end
  end

  # WwML lexer implementation.
  struct Lexer
    def initialize(@source : String)
      @reader = Char::Reader.new(@source)
      @queue = Deque(Token).new
    end

    # Returns the current character's **byte** index.
    private def pos : Int32
      @reader.pos
    end

    # Returns the current character.
    private def chr : Char
      @reader.current_char
    end

    # Advances to the next character.
    private def advance : Nil
      @reader.next_char
    end

    # Returns the next character without advancing to it.
    private def peek : Char
      @reader.has_next? ? @reader.peek_next_char : '\0'
    end

    # Skips a string token assuming `‸"<rest of the input>`.
    private def string : Token
      advance # Consume "

      content = String.build do |io|
        while true
          case chr
          when '\\'
            advance
            case chr
            when '0'  then io << '\0'
            when '"'  then io << '"'
            when '\\' then io << '\\'
            when '/'  then io << '/'
            when 'b'  then io << '\b'
            when 'f'  then io << '\f'
            when 'n'  then io << '\n'
            when 'r'  then io << '\r'
            when 't'  then io << '\t'
            when 'u'
              u1 = advance
              u2 = advance
              u3 = advance
              u4 = advance
              u5 = advance
              u6 = advance
              io << {u1, u2, u3, u4, u5, u6}.join.to_i(16).chr
            when '\0'
              raise "unexpected end-of-input while reading an escape sequence"
            else
              io << chr
            end
          when '"'
            advance
            break
          when '\0'
            raise "unexpected end-of-input while reading a string"
          else
            io << chr
            advance
          end
        end
      end

      Token.new(:term, Term.of(content), pos - content.bytesize - 1)
    end

    # Skips a comment token assuming `‸;<rest of the input>`.
    #
    # Consecutive comments such as `;...\n;...` are joined `\n;` acting as "glue".
    # Inserting even whitespace between `\n` and `;` will result in two separate
    # comments being produced.
    private def comment : Token
      b = pos

      while true
        case chr
        when '\n'
          advance
          break unless peek == ';'
        when '\0'
          break
        else
          advance
        end
      end

      Token.new(:comment, b, pos)
    end

    # Skips a symbol token assuming `‸<valid first char><rest of the input>`.
    private def symbol : Token
      content = String.build do |io|
        while true
          case chr
          when 'a'..'z', 'A'..'Z', '0'..'9',
               '!', '$', '%', '&', '*', '+',
               '-', '.', '/', '<', '=', '>',
               '?', '@', '_', '~', 'λ'
            io << chr
            advance
          else
            break
          end
        end
      end

      case content
      when "true"
        v = Term::Boolean.new(true)
      when "false"
        v = Term::Boolean.new(false)
      else
        v = Term::Sym.new(content)
      end

      Token.new(:term, Term.of(v), pos - content.bytesize)
    end

    # Skips a number token assuming `‸<rest of the input>`.
    private def number : Token
      b = pos

      # sign * sig * 10^(exp + bias_sign * bias)
      sig = 0.to_big_i
      sig_sign = 0i8 # 0 undetermined, -1 negative, +1 positive
      exp = 0i16
      bias = 0i16
      bias_sign = 0i8 # 0 undetermined, -1 negative, +1 positive

      state = :initial

      # Read the part before '/' (assuming there is a '/' of course)
      while true
        case state
        when :initial
          case chr
          when '+' # Set sign to +
            sig_sign = 1i8
            advance
          when '-' # Set sign to -
            sig_sign = -1i8
            advance
          when '0'
            state = :sig_zero
            advance
          when '1'..'9'
            state = :sig_nonzero
          else
            advance
            return Token.new(:invalid, b, pos)
          end
        when :sig_zero
          case chr
          when '.' # Switch to floating-point
            state = :exp_dot
            advance
          when 'e', 'E' # Set bias
            state = :bias_sign
            advance
          else
            break
          end
        when :sig_nonzero
          case chr
          when '_'
            advance
          when '0'..'9' # Add digit
            LibGMP.mul_ui(sig, sig, 10)
            LibGMP.add_ui(sig, sig, chr.to_i)
            advance
          when '.' # Switch to floating-point
            state = :exp_dot
            advance
          when 'e', 'E' # Set bias
            state = :bias_sign
            advance
          else
            break
          end
        when :exp_dot
          case chr
          when '_' # Disallow underscore immediately after '.'
            break
          else
            state = :exp
            next
          end
        when :exp
          case chr
          when '_'
            advance
          when '0'..'9' # Add digit, decrease exponent
            LibGMP.mul_ui(sig, sig, 10)
            LibGMP.add_ui(sig, sig, chr.to_i)
            exp -= 1
            advance
          when 'e', 'E' # Set bias
            state = :bias_sign
            advance
          else
            break
          end
        when :bias_sign
          case chr
          when '+'
            state = :bias_pos
            bias_sign = +1i8
            advance
          when '-'
            state = :bias_neg
            bias_sign = -1i8
            advance
          when '0'..'9'
            state = :bias_pos
            bias_sign = 1i8
          else
            advance
            return Token.new(:invalid, b, pos)
          end
        when :bias_pos, :bias_neg
          case chr
          when '0'..'9'
            bias = bias * 10 + chr.to_i
            advance
          else
            break
          end
        end
      end

      # Compute the value before '/'
      v = (sig_sign.zero? ? 1 : sig_sign) * sig * 10.to_big_d**(exp + bias * bias_sign)

      # If the value is an integer attempt to read a natural number after '/'.
      # Similar to Scheme (see for instance R5RS, 7.1.1) we don't accept a sign
      # after '/'.
      if v.integer? && chr == '/' && peek.in?('1'..'9')
        advance # Skip /

        num = v.to_big_i
        den = 0.to_big_i
        while chr.in?('0'..'9')
          LibGMP.mul_ui(den, den, 10)
          LibGMP.add_ui(den, den, chr.to_i)
          advance
        end

        # den cannot be zero at this point
        term = Term.of(BigRational.new(num, den))
      elsif v.integer? && Int32::MIN <= v <= Int32::MAX
        term = Term.of(v.to_i)
      else
        term = Term.of(v)
      end

      Token.new(:term, term, b)
    end

    # Skips a Sparse literal token assuming `‸#<rest of the input>`.
    private def sparse : Token
      b = pos

      advance # Consume '#'

      case chr
      when '('  # Up to closing ')'
        advance # Consume '('
        nesting = 0u8
        content = String.build do |io|
          while true
            case chr
            when '('
              nesting += 1
            when ')'
              if nesting.zero?
                advance
                break
              end
              nesting -= 1
            end
            io << chr
            advance
          end
        end
      when '['  # Up to closing ']'
        advance # Consume '['
        nesting = 0u8
        content = String.build do |io|
          io << '['
          while true
            case chr
            when '['
              nesting += 1
            when ']'
              if nesting.zero?
                advance
                break
              end
              nesting -= 1
            when '\0'
              break
            end
            io << chr
            advance
          end
          io << ']'
        end
      when '{'  # Up to closing '}'
        advance # Consume '{'
        nesting = 0u8
        content = String.build do |io|
          io << '{'
          while true
            case chr
            when '{'
              nesting += 1
            when '}'
              if nesting.zero?
                advance
                break
              end
              nesting -= 1
            when '\0'
              break
            end
            io << chr
            advance
          end
          io << '}'
        end
      else # Up to first whitespace outside of ""s
        content = String.build do |io|
          state = :normal

          while true
            case state
            when :normal
              case chr
              when '"'
                state = :string
              when .whitespace?, '\0'
                break
              end
              io << chr
              advance
            when :string
              case chr
              when '"'
                state = :normal
              when '\\'
                io << chr
                advance
              when '\0'
                break
              end
              io << chr
              advance
            end
          end
        end
      end

      query = Sparse::TermAST.query(content)

      Token.new(:term, query, b)
    end

    private def thru! : Token?
      while true
        case chr
        when .whitespace?, ','
          advance
        when 'a'..'z', 'A'..'Z'
          return symbol
        when '('
          advance
          return Token.new(:"(", pos - 1, pos)
        when ')'
          advance
          return Token.new(:")", pos - 1, pos)
        when '+'
          return peek.in?('0'..'9') ? number : symbol
        when '-'
          case peek
          when '>'
            advance
            advance
            return Token.new(:"->", pos - 2, pos)
          when '0'..'9'
            return number
          else
            return symbol
          end
        when '='
          case peek
          when '>'
            advance
            advance
            return Token.new(:"=>", pos - 2, pos)
          when '`'
            advance
            advance
            return Token.new(:"=`", pos - 2, pos)
          else
            return symbol
          end
        when ';'
          comment # Skip comment
        when '1'..'9'
          return number
        when '"'
          return string
        when '^'
          case peek
          when ':'
            advance
            advance
            return Token.new(:"^:", pos - 2, pos)
          else
            advance
            return Token.new(:"^", pos - 1, pos)
          end
        when '\\'
          case peek
          when ':'
            advance
            advance
            return Token.new(:"\\:", pos - 2, pos)
          else
            advance
            return Token.new(:"\\", pos - 1, pos)
          end
        when '#'
          return sparse
        when '0'
          case peek
          when '.', 'e', 'E'
            return number
          else
            advance
            return Token.new(:term, Term.of(0), pos - 1)
          end
        when '['
          advance
          return Token.new(:"[", pos - 1, pos)
        when ']'
          advance
          return Token.new(:"]", pos - 1, pos)
        when '{'
          advance
          return Token.new(:"{", pos - 1, pos)
        when '}'
          advance
          return Token.new(:"}", pos - 1, pos)
        when '!', '$', '%', '&', '*', '.', '/', '<', '>', '?', '@', '_', '~', 'λ'
          return symbol
        when '`'
          advance
          return Token.new(:"`", pos - 1, pos)
        when ':'
          advance
          return Token.new(:":", pos - 3, pos)
        when '→'
          advance
          return Token.new(:"->", pos - 3, pos)
        when '⇒'
          advance
          return Token.new(:"=>", pos - 3, pos)
        when '\''
          advance
          return Token.new(:"'", pos - 1, pos)
        when '\0'
          return
        else
          advance
          return Token.new(:invalid, pos - 1, pos)
        end
      end
    rescue e : Exception
      unless e.class == Exception # Exception only!
        raise e
      end
      raise SyntaxError.new(e.message)
    end

    # Advances through the current token and returns it, or `nil` if positioned at
    # the end-of-input.
    def thru? : Token?
      @queue.shift? || thru!
    end

    # Returns *n*-th token ahead without advancing, or `nil` if hit end-of-input.
    def ahead?(n : UInt32 = 1) : Token?
      if token = @queue[n - 1]?
        return token
      end

      n.times do
        break unless token = thru!
        @queue << token
      end

      token
    end
  end

  # WwML parser implementation that sits on top of a `Lexer`.
  struct Parser
    def initialize(@lexer : Lexer)
    end

    # Override `raise` so we don't have to type `SyntaxError` all the time.
    private def raise(message : String) : NoReturn
      raise SyntaxError.new(message)
    end

    # Parses a parenthesized list assuming `(‸<...>`.
    private def plist : Term
      Term::Dict.build do |commit|
        len = 0
        while token = @lexer.thru?
          break if token.type == :")"
          len = expression(commit, len, token)
        end
      end.upcast
    end

    # Parses a linked list shorthand constructor assuming `[‸<...>`.
    private def llist : Term
      items = [] of Term
      while token = @lexer.thru?
        break if token.type == :"]"
        items << slot(token)
      end

      tail = Term.of({:list})
      items.reverse_each do |item|
        tail = Term.of(:list, item, tail)
      end
      tail
    end

    # Parses a dictionary assuming `{‸<...>`.
    private def dict : Term
      Term::Dict.build do |commit|
        len = 0
        while token = @lexer.thru?
          break if token.type == :"}"
          len = expression(commit, len, token)
        end
      end.upcast
    end

    private def slot(token : Token) : Term
      case token.type
      when :term
        token.term
      when :"("   then plist
      when :"["   then llist
      when :"{"   then dict
      when :"'"   then Term.of(:hold, slot)
      when :"`"   then Term.of(:embed, slot)
      when :"=`"  then Term.of(:eval, {:embed, slot})
      when :"^"   then Term.of(:place, {:leaf, slot})
      when :"^:"  then Term.of(:paste, {:leaf, slot})
      when :"\\"  then Term.of(:place, slot)
      when :"\\:" then Term.of(:paste, {:"to-dict", slot})
      else
        raise "unexpected token: `#{token.type}`"
      end
    end

    private def slot : Term
      unless token = @lexer.thru?
        raise "unexpected end-of-input"
      end
      slot(token)
    end

    private def expression(commit, len, token) : Int32
      lhs = slot(token)
      unless operator = @lexer.ahead?
        commit.with(len, lhs)
        return len + 1
      end

      case operator.type
      when :"->" # "lhs_ -> rhs_" -> (macro lhs rhs)
        @lexer.thru?
        commit.with(len, {:macro, lhs, slot})
        len + 1
      when :"=>" # "lhs_ => rhs_" -> (rule lhs rhs)
        @lexer.thru?
        commit.with(len, {:rule, lhs, slot})
        len + 1
      when :":"
        @lexer.thru?
        commit.with(lhs, slot)
        len
      else
        commit.with(len, lhs)
        len + 1
      end
    end

    # Parses and returns a single top-level expression term.
    def expression : Term
      slot
    end

    # Parses and returns multiple top-level expressions wrapping them in
    # an itemsonly dictionary.
    def expressions : Term
      Term::Dict.build do |commit|
        len = 0
        while token = @lexer.thru?
          len = expression(commit, len, token)
        end
      end.upcast
    end
  end
end
