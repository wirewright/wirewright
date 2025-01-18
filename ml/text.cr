# TODO: shorthand syntax for repetitive key-value pair stores in partition: e.g. `leading: leading_number`,
# `leading: leading←(%nat)`.
#   maybe x:_... syntax would work for blanks, e.g. x:_number, and for lets, similarly
#   x:←(...) [no whitespace allowed]
# TODO: shorthand for number patterns such as (%nat) after there are more of them,
# allow combining etc. Should be easy to restrict numbers, otherwise everyone
# including me will use _number which is bad bad bad especially in Crystal land because
# it could cause overflows & exceptions!
#   -- study/explore syntactic modularity for this. something like HTML/CSS classes could work,
#      maybe define a bunch of "class-like aliases"/have them be user-defined, aka get them from
#      the environment. then one would be able to invoke them.
#   -- maybe use something like masks e.g. +___ for positive numbers, -___ for negative
#      numbers, ±___ for either, no. of `_`s stands for the amount of digits, you can
#      specify an arbitrary base ±b16___ i.e. max value will be fff. Should be possible
#      to say whether zero is allowed: ±___!0. Maybe use some unicode character for the
#      not part. Or e.g. ___⁰ for +/- number, ___⁺⁰ for positive zero allowed, ___¯⁰
#      for negative zero allowed, ___⁺ positive zero disallowed and so on. Alternatively
#      we can use range notation `{|0-255|}` with shorthands for number types e.g.
#      `⎡u8`, `⎡u8`, `⎡u8!0` to exclude 0. So e.g. `⎡0<N:x<100` would stand for natural
#       `x` between `1` and `100`, `⎡N:x` for natural x, and so on.
# TODO: short syntax for nested "ignore everything else" patterns in partition, i.e.:
#   wrap: (¦ _ collapse: (¦ _ newlines: true)) ==> wrap.collapse.newlines: true
# TODO: maybe use something else (:? perhaps, or ⦂) instead of ⋮ for defaults in partition
# because it is too similar to ¦.
# TODO: in general, partition should hold the majority of these "syntaxes". most of
#   them should not leak out into the outer language. If you don't use partition syntax,
#   you won't have these shorthands around, and that's exactly what we want!

# TODO: general: I'm more and more in favor of throwing this parser away and actually
# creating a parsing engine (like we have pattern matching engine etc.) that can accept
# grammars as Terms. This would be a huge awesome unifying move for the project since
# this way we'll be able to interface the outside world with the system 1) via network
# 2) via UI 3) via syntax (user-defined). And one of those grammars can be the WwML grammar itself!
#
# This would cause a considerable performance hit though, but for performance sensitive
# stuff (e.g. talking over the network) we'll have WwMR anyway (the machine-readable
# representation of terms)
#
#     -state_ is a shorthand for state: (%pair/absent state) which should be a variant of (%pair/absent) that mounts
#     the keypath to the absent pair in keypath mode.

module Ww::ML::Text
  # An exclusive range, specifies the begin and end of the source substring that
  # a `Token` occupies.
  record Span, b : Int32, e : Int32

  # todo: use u7 for type, have 1 bit for span | term, store @payload as pointer(void),
  # if span bit on then @payload is nitemsgth, remove Span (b = @pos, e = b + @payload.address)
  # this will make sizeof(Token) = 16

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

  # TODO: streaming (IO) support

  # WwML lexer implementation.
  struct Lexer
    def initialize(@source : String)
      @reader = Char::Reader.new(@source)
      @queue = Deque(Token).new
    end

    # Returns the current character's **byte** index.
    def pos : Int32
      @reader.pos
    end

    # Returns the current character.
    def chr : Char
      @reader.current_char
    end

    # Advances to the next character.
    private def advance : Nil
      @reader.next_char
    end

    # Returns the next character without advancing to it.
    private def ahead : Char
      @reader.has_next? ? @reader.peek_next_char : '\0'
    end

    # Returns the previous character without moving back to it. Returns `nil`
    # if there is nothing behind.
    private def behind? : Char?
      return unless @reader.has_previous?

      @reader.previous_char.tap { @reader.next_char }
    end

    private def raise(message : String)
      raise SyntaxError.new(message, pos - 1)
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
              u6 = chr
              io << {u1, u2, u3, u4, u5, u6}.join.to_i(16).chr # FIXME: to_i raise
            when 'x'
              b1 = advance
              b2 = chr
              io.write_byte({b1, b2}.join.to_i(16).to_u8) # FIXME: to_i raise
            when '\0'
              raise "unexpected end-of-input while reading an escape sequence"
            else
              io << chr
            end
            advance
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
          break unless ahead == ';'
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
               '?', '@', '_', '~', 'λ', '|',
               '∞', '⏏', '°', '∈', '⊆', '⊂'
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
      if v.integer? && chr == '/' && ahead.in?('1'..'9')
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
          return ahead.in?('0'..'9') ? number : symbol
        when '-'
          case ahead
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
          case ahead
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
        when '<'
          case ahead
          when '>'
            advance
            advance
            return Token.new(:"<>", pos - 2, pos)
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
          case ahead
          when ':'
            advance
            advance
            return Token.new(:"^:", pos - 2, pos)
          else
            advance
            return Token.new(:"^", pos - 1, pos)
          end
        when '\\'
          case ahead
          when ':'
            advance
            advance
            return Token.new(:"\\:", pos - 2, pos)
          else
            advance
            return Token.new(:"\\", pos - 1, pos)
          end
        when '0'
          case ahead
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
        when '!', '$', '%', '&', '*', '.', '/', '>', '?', '_', '~', 'λ', '|', '∞', '⏏', '°', '∈', '⊆', '⊂'
          return symbol
        when '`'
          advance
          return Token.new(:"`", pos - 1, pos)
        when ':'
          advance
          return Token.new(:":", pos - 3, pos)
        when '→'
          advance
          return Token.new(:"→", pos - 3, pos)
        when '↑'
          advance
          return Token.new(:"↑", pos - 3, pos)
        when '↓'
          advance
          return Token.new(:"↓", pos - 3, pos)
        when '⇒'
          advance
          return Token.new(:"=>", pos - 3, pos)
        when '⟷'
          advance
          return Token.new(:"<->", pos - 3, pos)
        when '\''
          advance
          return Token.new(:"'", pos - 1, pos)
        when '@'
          advance
          return Token.new(:"@", pos - 1, pos)
        when '¦'
          advance
          return Token.new(:"¦", pos - 2, pos)
        when '⋮'
          advance
          return Token.new(:"⋮", pos - 3, pos)
        when '⤳'
          advance
          return Token.new(:"⤳", pos - 3, pos)
        when '⭳'
          advance
          return Token.new(:"⭳", pos - 3, pos)
        when '←'
          if behind?.try(&.whitespace?)
            raise "unexpected whitespace near `←`"
          end
          advance
          if chr.whitespace?
            raise "unexpected whitespace near `←`"
          end
          return Token.new(:"←", pos - 3, pos)
        when '⥆'
          if behind?.try(&.whitespace?)
            raise "unexpected whitespace near `⥆`"
          end
          advance
          if chr.whitespace?
            raise "unexpected whitespace near `⥆`"
          end
          return Token.new(:"⥆", pos - 3, pos)
        when '⟅'
          advance
          return Token.new(:"⟅", pos - 3, pos)
        when '⟆'
          advance
          return Token.new(:"⟆", pos - 3, pos)
        when '⧼'
          advance
          return Token.new(:"⧼", pos - 3, pos)
        when '⧽'
          advance
          return Token.new(:"⧽", pos - 3, pos)
        when '≡'
          advance
          return Token.new(:"≡", pos - 3, pos)
        when '\0'
          return
        else
          advance
          return Token.new(:invalid, pos - 1, pos)
        end
      end
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
      raise SyntaxError.new(message, @lexer.pos - 1)
    end

    # Parses a parenthesized list assuming `(‸<...>`.
    private def plist : Term
      state = :open
      reg = nil
      list = Term[]

      while true
        case state0 = state
        when :open
          list = list.transaction do |commit|
            while token = @lexer.thru?
              case token.type
              when :")"
                state = :closed
                break
              when :"¦"
                state = :partition
                break
              end
              expression(commit, token)
            end
          end
        when :partition
          pairs = Term[]

          while token = @lexer.thru?
            case token.type
            when :")"
              state = :closed
              break
            end

            key = slot(token)

            unless op = @lexer.ahead?
              raise "unexpected end-of-input after partition"
            end

            case op.type
            when :")"
              @lexer.thru?

              unless pairs.empty?
                raise "pairs partition override cannot be combined with pairs"
              end
              pairs = key
              state = :closed
              break
            when :":"
              @lexer.thru?

              pairs = pairs.with(key, slot)
            when :"⋮"
              @lexer.thru?

              value = slot

              raise "I refuse to do anything circular"
              # Yay we're using the parser inside itself!!
              # Term.case(key) do
              #   givenpi %((%literal %let) name_ body_) do
              #     pairs = pairs.with(name, {:"%default", value, key})
              #   end

              #   matchp %(_symbol) do
              #     key0 = key.unsafe_as_sym

              #     # The client wants to specify the type themselves.
              #     if blank = key0.blank?
              #       name = blank.name? || raise "unnamed blank with a default makes no sense"
              #       pairs = pairs.with(name, {:"%default", value, key0})
              #       next
              #     end

              #     # Infer the type from value and synthesize a symbol.
              #     key1 = String.build do |io|
              #       io << key0
              #       value.type.blank(io)
              #     end

              #     pairs = pairs.with(key0, {:"%default", value, Term::Sym.new(key1)})
              #   end

              #   otherwise do
              #     pairs = pairs.with(key, {:"%default", value, {:"%let", key, Term::Sym.new(value.type.blank)}})
              #   end
              # end
            else
              if pairs.empty? && key.type.symbol? && (blank = key.blank?) && blank.poly?
                # ¦ pairs_*
                state = :"%value*"
                reg = blank.name?
                break
              elsif pairs.empty?
                # ¦ pairs_ ...
                # ¦ (x y z) ...
                state = :"%layer"
                reg = key
                break
              end

              raise "unexpected token, expected ':' or '⋮'"
            end
          end

          if state == :closed
            list = Term[:"%partition", list, pairs]
          end
        when :"%value*"
          name = reg.as(Term::Sym?)

          # TODO: support multiple %value* pairs with %mux
          body = Term[{:"%value*"}].transaction do |commit|
            unless token = @lexer.thru?
              raise "unexpected end-of-input before %value* key"
            end

            key = slot(token)

            unless (op = @lexer.thru?) && op.type == :":"
              raise "unexpected end-of-input after %value* key, expected ':'"
            end

            commit.append(key)
            commit.append(slot)

            unless (token = @lexer.thru?) && token.type == :")"
              raise "unexpected character after %value* value, expected ')'"
            end

            state = :closed
          end

          if state == :closed
            list = Term[:"%partition", list, name ? {:"%let", name, body} : body]
          end
        when :"%layer"
          extra = reg.as(Term)

          body = Term[:"%layer", extra].transaction do |commit|
            unless token = @lexer.thru?
              raise "unexpected end-of-input"
            end

            while true
              key = slot(token)

              unless (op = @lexer.thru?) && op.type == :":"
                raise "unexpected end-of-input after %layer key, expected ':'"
              end

              commit.with(key, slot)

              unless token = @lexer.thru?
                raise "unexpected end-of-input after %layer value, expected another key-value pair or ')'"
              end

              if token.type == :")"
                state = :closed
                break
              end
            end
          end

          if state == :closed
            list = Term[:"%partition", list, body]
          end
        when :closed
          return list.upcast
        end

        if state0 == state # Expect the state to change.
          raise "expected closing ')'"
        end
      end
    end

    # Parses an itemspart shorthand assuming `[‸<...>`.
    private def litemspart : Term
      itemspart = Term[].transaction do |commit|
        while token = @lexer.thru?
          case token.type
          when :"]"
            break
          end

          expression(commit, token)
        end
      end

      Term.of(:"%partition", itemspart, :"_")
    end

    # Parses a dictionary assuming `{‸<...>`.
    private def dict : Term
      Term::Dict.build do |commit|
        closed = false
        while token = @lexer.thru?
          if token.type == :"}"
            closed = true
            break
          end
          expression(commit, token)
        end
        unless closed
          raise "expected closing '}'"
        end
      end.upcast
    end

    private def term?(cls : Term::Dict.class) : Term?
      return unless token = @lexer.ahead?
      return unless token.type == :"{"

      @lexer.thru?

      dict
    end

    private def term?(cls : T.class) : Term? forall T
      return unless token = @lexer.ahead?
      return unless token.type == :term

      term = token.term.downcast
      return unless term.is_a?(T)

      @lexer.thru?

      token.term
    end

    # Slot-level operators.
    private def operator1?(lhs, operator) : Term?
      case operator.type
      when :"←" # "lhs_ ← rhs_" -> (%let lhs rhs)
        # E.g. we write x←y (without the spaces), it's actually an error to put
        # spaces around '←'. This is so that there is less confusion when seen
        # as part of a larger pattern. This shorthand is specifically for larger
        # patterns where let's are the least of concerns. In more explanatory/
        # simpler contexts it's better to just use %let, there isn't a lot of
        # character difference. It's when you have multiple, even nested, let's,
        # that '←' becomes useful.
        @lexer.thru?
        Term.of(:"%let", lhs, slot)
      when :"⥆" # "lhs_ ⥆ rhs_" -> (%let* lhs rhs)
        @lexer.thru?
        Term.of(:"%let*", lhs, slot)
      end
    end

    # "Find somewhere" pattern shorthand. ⟅...⟆ = (_* ... _* ¦ _) -> (%partition (_* ... _*) _)
    private def psomewhere : Term
      list = Term[].transaction do |commit|
        commit.append(:"_*")

        while token = @lexer.thru?
          case token.type
          when :"⟆"
            break
          end

          expression(commit, token)
        end

        commit.append(:"_*")
      end

      Term.of(:"%partition", list, :_)
    end

    private def pitem : Term
      list = Term[].transaction do |commit|
        commit.append(:"%item")

        while token = @lexer.thru?
          case token.type
          when :"⧽"
            break
          end

          expression(commit, token)
        end
      end

      Term.of(list)
    end

    private def slot(token : Token) : Term
      term =
        case token.type
        when :term  then token.term
        when :"⟅"   then psomewhere
        when :"⧼"   then pitem
        when :"("   then plist
        when :"["   then litemspart
        when :"{"   then dict
        when :"→"   then Term.of(:"$my", slot)
        when :"↑"   then Term.of(:"$up", slot)
        when :"↓"   then Term.of(:"$down", slot)
        when :"'"   then Term.of(:hold, slot)
        when :"`"   then Term.of(:embed, slot)
        when :"=`"  then Term.of(:eval, {:embed, slot})
        when :"^"   then Term.of(:place, {:leaf, slot})
        when :"^:"  then Term.of(:paste, {:leaf, slot})
        when :"\\"  then Term.of(:place, slot)
        when :"\\:" then Term.of(:paste, {:"to-dict", slot})
        when :"⤳"   then Term.of(:"%dep", slot)
        when :"⭳"   then Term.of(:"%slot", slot)
        when :"≡"   then Term.of(:"%nonself", slot)
        when :"@"
          unless name = term?(Term::Sym) || term?(Term::Str) || term?(Term::Num)
            raise "expected symbol, string, or number as edge name"
          end

          Term.of(:edge, name)
        else
          raise "unexpected token: '#{token.type}'"
        end

      return term unless operator = @lexer.ahead?

      operator1?(term, operator) || term
    end

    private def slot : Term
      unless token = @lexer.thru?
        raise "unexpected end-of-input"
      end
      slot(token)
    end

    private def expression(commit, token)
      lhs = slot(token)
      unless operator = @lexer.ahead?
        commit.append(lhs)
        return
      end

      case operator.type
      when :"->" # "lhs_ -> rhs_" -> (macro lhs rhs)
        @lexer.thru?
        commit.append({:macro, lhs, slot})
      when :"=>" # "lhs_ => rhs_" -> (rule lhs rhs)
        @lexer.thru?
        commit.append({:rule, lhs, slot})
      when :"<>" # "lhs_ <> rhs_" -> (backmap lhs rhs)
        @lexer.thru?
        commit.append({:backmap, lhs, slot})
      when :":"
        @lexer.thru?
        commit.with(lhs, slot)
      else
        commit.append(lhs)
      end
    end

    # Parses and returns a single top-level expression term.
    def expression : Term
      term = slot
      if @lexer.ahead?
        raise "expected end-of-input, found something else instead"
      end

      term
    end

    # Parses and returns multiple top-level expressions wrapping them in
    # an itemsonly dictionary.
    def expressions : Term
      Term::Dict.build do |commit|
        while token = @lexer.thru?
          expression(commit, token)
        end
      end.upcast
    end
  end
end
