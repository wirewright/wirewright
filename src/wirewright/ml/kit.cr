module Ww::ML
  # Utilities associated with WwML. The main user of these is WwML itself
  # (i.e., the lexeme reader and the term reader).
  module Kit
    extend self

    # Returns the number term corresponding to *chr* in *radix*, or `nil` if invalid.
    #
    # - Accepts radix in [1, 62]. Raises `ArgumentError` if *radix* is out of this range.
    # - Allows uppercase and lowercase letters to be used interchangeably in radices 11-36.
    def chr2nat?(chr : Char, radix : Term::Num) : Term::Num?
      unless Term[1] <= radix <= Term[62]
        raise ArgumentError.new
      end

      if Term[11] <= radix <= Term[36]
        # Normalize
        chr = chr.upcase
      end

      if chr.in?('0'..'9')
        digit = chr - '0'
      elsif chr.in?('A'..'Z')
        digit = 10 + (chr - 'A')
      elsif chr.in?('a'..'z')
        digit = 36 + (chr - 'a')
      else
        return
      end

      digit < radix ? Term[digit] : nil
    end

    # Parses a string of Unicode base-10 subscript digits (₀-₉) into a number term.
    # Returns `nil` on invalid input.
    def sub2nat?(digitsrc : StringView) : Term::Num?
      n = Term[0]

      digitsrc.each_char do |digit|
        return unless digit.in?('₀'..'₉')

        n = n.append(Term[digit - '₀'], radix: Term[10])
      end

      n
    end

    # Parses a string of Unicode superscript digits into a Crystal `Int32`.
    # Returns `nil` on invalid input.
    def sup2i?(digitsrc : StringView) : Int32?
      n = 0

      digitsrc.each_char do |digit|
        case digit
        when '⁰' then n = n * 10 + 0
        when '¹' then n = n * 10 + 1
        when '²' then n = n * 10 + 2
        when '³' then n = n * 10 + 3
        when '⁴'..'⁹'
          n = n * 10 + 4 + (digit - '⁴')
        else
          return
        end
      end

      n
    end

    # Parses a number written using the decimal form.
    #
    # *exact* toggles between exact and approximate arithmetic.
    #
    # Raises `SyntaxError` on invalid input.
    #
    # ```text
    # decimal
    #   nat "/" nat
    #   nat frpart? expn?
    #
    # nat
    #   digit
    #   digit "_" nat
    #   digit nat
    #
    # frpart
    #   "." nat
    #
    # expn
    #   ("e" | "E") ("+" | "-" | "") nat
    # ```
    def decimal(decsrc : StringView, *, exact : Bool = true) : Term | Term::Num
      decsrc.reader do |r|
        if Rtk.at_end?(r)
          raise ArgumentError.new
        end

        digits = Rtk.view(r) { Rtk.skip_to(r, "/eE.") }

        n, _ = digits_to_number(digits, exact: exact)

        if Rtk.past?(r, '/')
          densrc = Rtk.rest(r)

          if densrc.empty?
            raise SyntaxError.new("expected at least one digit for the denominator", densrc)
          end

          den, _ = digits_to_number(densrc, exact: exact)

          if den.zero?
            raise SyntaxError.new("division by zero", densrc)
          end

          return n/den
        end

        if Rtk.past?(r, '.')
          frsrc = Rtk.view(r) { Rtk.skip_to(r, "eE") }

          if frsrc.empty?
            raise SyntaxError.new("expected at least one fractional part digit after `.`; did you mean `#{ML.compact(n)}.0`?", frsrc)
          end

          frpart, frlen = digits_to_number(frsrc, exact: exact)

          # 123_456.789⏏
          n += frpart * Term[1]/(Term[10] ** frlen)
        end

        if Rtk.past?(r, "eE")
          # 1e⏏
          sign = Term[1]

          if Rtk.past?(r, '+')
            # 1e+⏏
          elsif Rtk.past?(r, '-')
            # 1e-⏏
            sign = Term[-1]
          end

          mantissasrc = Rtk.rest(r)
          if mantissasrc.empty?
            raise SyntaxError.new("expected at least one digit for the exponent", mantissasrc)
          end

          mantissa, _ = digits_to_number(mantissasrc, exact: exact)

          n = Term.of(:sci, n, sign * mantissa)
        end

        n
      end
    end

    # Parses decimal *digitsrc*, possibly separated by underscores. Returns
    # the resulting natural number and the amount of digits in it (due to
    # underscores this isn't simply `digitsrc.size`).
    #
    # Raises `SyntaxError` on invalid input.
    def digits_to_number(digitsrc : StringView, *, exact : Bool) : {Term::Num, Term::Num}
      # Validate
      digitsrc.each_split do |l, m, r|
        case m
        when '0'..'9'
        when '_'
          if l.empty?
            raise SyntaxError.new("leading underscores not allowed in number", m)
          end

          if r.empty?
            raise SyntaxError.new("trailing underscores not allowed in number", m)
          end

          if r.starts_with?('_')
            raise SyntaxError.new("multiple consecutive underscores not allowed in number", m)
          end
        else
          raise SyntaxError.new("extra symbolic characters found in number", m &+ r)
        end
      end

      n = exact ? Term::Num.exact(0) : Term::Num.approx(0.0f64)
      len = Term[0]

      digitsrc.each_char do |chr|
        next if chr == '_'

        n = n * Term[10] + (chr2nat?(chr, radix: Term[10]) || unreachable)
        len += Term[1]
      end

      {n, len}
    end

    # Parses a number written using the radix form.
    #
    # *digitsrc* and *radixsrc* are the two components of the radix form: the digits
    # block and the subscript natural number describing the radix in decimal,
    # correspondingly. For example, a valid pair of arguments to this method
    # would be `digits: "123" radix: "₁₀"`.
    #
    # Raises `SyntaxError` on invalid input.
    def radix(digitsrc : StringView, radixsrc : StringView) : Term
      unless radix = sub2nat?(radixsrc)
        raise SyntaxError.new("invalid radix", radixsrc)
      end

      unless 1 <= radix <= 62
        raise SyntaxError.new("expected radix 1-62, not `#{radix}`", radixsrc)
      end

      digitsrc.each_split do |l, m, r|
        case m
        when '0'..'9', 'a'..'z', 'A'..'Z'
        when '_'
          if l.empty?
            raise SyntaxError.new("leading underscores not allowed in number", m)
          end

          if r.empty?
            raise SyntaxError.new("trailing underscores not allowed in number", m)
          end

          if r.starts_with?('_')
            raise SyntaxError.new("multiple consecutive underscores not allowed in number", m)
          end
        else
          raise SyntaxError.new("extra symbolic characters found in number", m &+ r)
        end
      end

      digitsrc.reader do |r|
        digits = Term::Dict.build do |commit|
          commit << :digits
          commit.with(:radix, radix)

          until Rtk.at_end?(r)
            next if Rtk.past?(r, '_')

            unless digit = chr2nat?(Rtk.chr(r), radix)
              raise SyntaxError.new("invalid digit `#{Rtk.chr(r)}` for radix `#{radix}`", Rtk.ahead1(r))
            end

            commit << digit

            Rtk.forward(r)
          end
        end

        Term.of(digits)
      end
    end

    # Appends an escaped representation of *chr* to *io*.
    def escape(io, ch : Char) : Nil
      case {ch, ch.bytesize}
      when {'"', _}
        io << "\\\""
      when {'\\', _}
        io << "\\\\"
      when {'\n', _}
        io << "\\n"
      when {'\t', _}
        io << "\\t"
      when {'\r', _}
        io << "\\r"
      when {'⸢', _}
        io << "\\⸢"
      when {.printable?, _}
        io << ch
      when {_, 1}
        io << "\\x"
        ch.ord.to_s(io, base: 16, precision: 2, upcase: true)
      when {_, 2}
        io << "\\u"
        ch.ord.to_s(io, base: 16, precision: 4, upcase: true)
      else
        io << "\\u{"
        ch.ord.to_s(io, base: 16, upcase: true)
        io << "}"
      end
    end

    # Evaluates the escape sequences in *stringsrc*.
    #
    # Raises `SyntaxError` on invalid escape sequences.
    def unescape(stringsrc : StringView) : Term::Str
      stringsrc.reader do |r|
        prefix = Rtk.view(r) { Rtk.skip_to(r, '\\') }

        # "hello world"⏏
        if Rtk.at_end?(r)
          return Term[stringsrc]
        end

        interior = String.build do |io|
          io << prefix

          unescape(r, io)
        end

        Term[interior]
      end
    end

    private def unescape(r, io) : Nil
      until Rtk.at_end?(r)
        if Rtk.past?(r, '\\')
          unescape1(r, io)
          next
        end

        io << Rtk.view(r) { Rtk.forward(r) }
      end
    end

    private def unescape1(r, io) : Nil
      case
      when Rtk.past?(r, '"')  then io << '"'
      when Rtk.past?(r, '\\') then io << '\\'
      when Rtk.past?(r, 'n')  then io << '\n'
      when Rtk.past?(r, 't')  then io << '\t'
      when Rtk.past?(r, 'r')  then io << '\r'
      when Rtk.past?(r, '⸢')  then io << '⸢'
      when Rtk.past?(r, '\n')
        Rtk.skip(r, " \t")
      when Rtk.past?(r, 'x')
        d0 = Rtk.hexdigit?(r)
        d1 = d0 && Rtk.hexdigit?(r)

        unless d0 && d1
          raise SyntaxError.new("expected exactly two hex digits after `\\x`", Rtk.ahead1(r))
        end

        io << (d0 << 4 | d1).chr
      when Rtk.past?(r, 'u')
        unescape1u(r, io)
      else
        raise SyntaxError.new("invalid escape character, expected one of: `\"⸢ntrxu\\` or newline", Rtk.ahead1(r))
      end
    end

    # Characters that can be used in a Unicode codepoint name.
    U_CODEPOINT_NAME_CHARSET = "a-zA-Z0-9() \\-"

    # Characters that can be used in an emoji name.
    U_EMOJI_NAME_CHARSET = "a-zA-Z0-9_"

    private def unescape1u(r, io) : Nil
      case
      when Rtk.past?(r, '{')
        # \u{⏏
        codepoint = 0
        digits = Rtk.view(r) do
          6.times do
            break unless digit = Rtk.hexdigit?(r)
            codepoint = (codepoint << 4) | digit
          end
        end

        if digits.empty?
          raise SyntaxError.new("expected 1-6 hex digits in `\\u{}`", digits)
        end

        unless Rtk.past?(r, '}')
          raise SyntaxError.new("expected `}` to close `\\u{`", Rtk.ahead1(r))
        end

        begin
          io << codepoint.chr
        rescue ArgumentError
          raise SyntaxError.new("unsupported codepoint `#{codepoint.to_s(base: 16)}`", digits)
        end
      when Rtk.pastsequ?(r, "[:")
        # \u[:⏏
        name = Rtk.view(r) { Rtk.skip(r, U_EMOJI_NAME_CHARSET) }

        if name.empty?
          raise SyntaxError.new("expected emoji name", name)
        end

        unless Rtk.pastsequ?(r, ":]")
          raise SyntaxError.new("expected `:]` to close `\\u[:`", Rtk.ahead1(r))
        end

        begin
          emoji(io, name.to_s)
        rescue e : KeyError
          raise SyntaxError.new(e.message.not_nil!("BUG: missing error message"), name)
        end
      when Rtk.past?(r, '[')
        # \u[⏏
        name = Rtk.view(r) { Rtk.skip(r, U_CODEPOINT_NAME_CHARSET) }

        if name.empty?
          raise SyntaxError.new("expected Greek letter, codepoint, or emoji name (e.g. `\\u[delta]`, or \\u[greek capital letter delta]`, or `\\u[:poop:]`)", name)
        end

        unless Rtk.past?(r, ']')
          raise SyntaxError.new("expected `]` to close `\\u[`", Rtk.ahead1(r))
        end

        begin
          codepoint(io, name.to_s)
        rescue e : KeyError
          raise SyntaxError.new(e.message.not_nil!("BUG: missing error message"), name)
        end
      when d0 = Rtk.hexdigit?(r)
        # \u_⏏
        d1 = d0 && Rtk.hexdigit?(r)
        d2 = d1 && Rtk.hexdigit?(r)
        d3 = d2 && Rtk.hexdigit?(r)

        unless d1 && d2 && d3
          raise SyntaxError.new("expected exactly four hex digits after `\\u`", Rtk.ahead1(r))
        end

        io << ((d0 << 12) | (d1 << 8) | (d2 << 4) | d3).chr
      else
        raise SyntaxError.new("expected one of `\\u{...}`, `\\u[...]`, `\\u____`", Rtk.ahead1(r))
      end
    end

    # Appends a codepoint with the given *name* to *io*.
    #
    # *name* is the `\u[...]` name of the codepoint, meaning either:
    #
    # - A Greek letter, such as `pi` for `π` and `Pi` for `Π`
    # - A Unicode codepoint name, such as `greek capital letter delta`.
    #
    # Raises `KeyError` if codepoint is not found.
    def codepoint(io, name : String) : Nil
      if chr = ::Ww::Unicode::Index.greek?(name)
        io << chr
        return
      end

      # Normalize before looking up the codepoint.
      normname = name.upcase.squeeze(" ")

      if codepoint = ::Ww::Unicode::Index.codepoint?(normname)
        begin
          io << codepoint.chr
          return
        rescue ArgumentError
          raise KeyError.new("unsupported codepoint `#{codepoint.to_s(base: 16)}`")
        end
      end

      raise KeyError.new("`#{name}` is not a Greek letter name nor a Unicode codepoint name")
    end

    # Constructs a string containing a codepoint with the given *name*.
    #
    # See `codepoint`.
    def codepoint(name : String) : String
      String.build { codepoint(io, name) }
    end

    # Appends an emoji with the given *name* to *io*.
    #
    # Raises `KeyError` such an emoji does not exist.
    def emoji(io, name : String) : Nil
      unless emoji = ::Ww::Unicode::Index.emoji?(name)
        raise KeyError.new("`#{name}` is not an emoji")
      end

      io << emoji
    end

    # Constructs a string containing an emoji with the given *name*.
    #
    # See `emoji(io, name)`.
    def emoji(name : String) : String
      String.build { emoji(io, name) }
    end
  end
end
