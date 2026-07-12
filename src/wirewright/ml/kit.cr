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

      # Special case for unary, digit can't be zero because (most? all?)
      # math would break. Use digit `1` instead.
      if radix == Term[1]
        return unless chr == '1'
        return Term[1]
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
    def decimal(src : StringView, *, exact : Bool = true) : Term | Term::Num
      if src.empty?
        raise ArgumentError.new
      end

      digits, src = src.skip_to("/eE.")
      num, _ = nat(digits, exact: exact)

      if src.starts_with?('/')
        src = src.rest

        # 2/⏏3⏏
        densrc = src
        if densrc.empty?
          raise SyntaxError.new("expected at least one digit for the denominator", densrc.before_begin)
        end

        den, _ = nat(densrc, exact: exact)
        if den.zero?
          raise SyntaxError.new("division by zero", densrc)
        end

        return num/den
      end

      if src.starts_with?('.')
        src = src.rest

        # 123_456.⏏789⏏
        # 123_456.⏏789⏏e+14
        frsrc, src = src.skip_to("eE")
        if frsrc.empty?
          raise SyntaxError.new("expected at least one fractional part digit after `.`; did you mean `#{ML.compact(num)}.0`?", frsrc.before_begin)
        end

        frpart, frlen = nat(frsrc, exact: exact)
        num += frpart * Term[1]/(Term[10] ** frlen)
      end

      if src.starts_with?('e') || src.starts_with?('E')
        src = src.rest

        # 1e⏏
        sign = Term[1]

        if src.starts_with?('+')
          src = src.rest
          # 1e+⏏
        elsif src.starts_with?('-')
          src = src.rest
          # 1e-⏏
          sign = Term[-1]
        end

        mantissasrc = src
        if mantissasrc.empty?
          raise SyntaxError.new("expected at least one digit for the exponent", mantissasrc.before_begin)
        end

        mantissa, _ = nat(mantissasrc, exact: exact)
        num = Term.of(:sci, num, sign * mantissa)
      end

      num
    end

    # Parses decimal *digitsrc*, possibly separated by underscores. Returns
    # the resulting natural number and the amount of digits in it (due to
    # underscores this isn't simply `digitsrc.size`).
    #
    # Raises `SyntaxError` on invalid input.
    def nat(digitsrc : StringView, *, exact : Bool) : {Term::Num, Term::Num}
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

      digits = Term::Dict.build do |commit|
        commit << :digits
        commit.with(:radix, radix)

        digitsrc.each_char_seln do |chrview|
          next if chrview == '_'

          unless digit = chr2nat?(chrview.first_char, radix)
            raise SyntaxError.new("invalid digit `#{chrview}` for radix `#{radix}`", chrview)
          end

          commit << digit
        end
      end

      Term.of(digits)
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

    # Evaluates escape sequences in *src*. Raises `SyntaxError` on invalid
    # escape sequences.
    def unescape(src : StringView) : Term::Str
      prefix, src = src.skip_to("\\")
      if src.empty?
        # "⏏hello world" -> "hello world⏏", no escape sequences.
        return Term[prefix]
      end

      interior = String.build do |io|
        io << prefix
        unescape(io, src)
      end

      Term[interior]
    end

    private def unescape(io : IO, src : StringView) : Nil
      until src.empty?
        rune, src = unescape1(src)
        io << rune
      end
    end

    private def unescape1(src : StringView) : {Char | String, StringView}
      if src.empty?
        raise ArgumentError.new
      end

      # h⏏ello -> he⏏llo
      unless src.starts_with?('\\')
        return src.first_char, src.rest
      end
      src = src.rest
      # \⏏

      if src.starts_with?('"')
        src = src.rest
        # \"⏏
        return '"', src
      end

      if src.starts_with?('\\')
        src = src.rest
        # \\⏏
        return '\\', src
      end

      if src.starts_with?('b')
        src = src.rest
        # \b⏏
        return '\b', src
      end

      if src.starts_with?('f')
        src = src.rest
        # \f⏏
        return '\f', src
      end

      if src.starts_with?('n')
        src = src.rest
        # \n⏏
        return '\n', src
      end

      if src.starts_with?('t')
        src = src.rest
        # \t⏏
        return '\t', src
      end

      if src.starts_with?('r')
        src = src.rest
        # \r⏏
        return '\r', src
      end

      if src.starts_with?('⸢')
        src = src.rest
        # \⸢⏏
        return '⸢', src
      end

      if src.starts_with?('\n')
        src = src.rest
        # hello world \
        # ⏏  then line wrapped with indent

        _, src = src.skip_thru(" \t")
        # hello world \
        #   ⏏then line wrapped with indent

        return "", src
      end

      if src.starts_with?('x')
        src = src.rest
        # \x⏏

        digitsrc, digits, src = src.skip_thru_seq(limit: 2u32, &.hexdigit?)
        unless digits.size == 2
          raise SyntaxError.new("expected exactly two hex digits after `\\x`", digitsrc)
        end
        # \x0a⏏

        chr = (digits[0] << 4 | digits[1]).chr
        return chr, src
      end

      if src.starts_with?('u')
        src = src.rest
        # \u⏏
        return unescape1u(src)
      end

      raise SyntaxError.new("invalid escape character, expected one of: `\"⸢bfntrxu\\` or newline", src.first)
    end

    # Characters that can be used in a Unicode codepoint name.
    U_CODEPOINT_NAME_CHARSET = "a-zA-Z0-9() \\-"

    # Characters that can be used in an emoji name.
    U_EMOJI_NAME_CHARSET = "a-zA-Z0-9_"

    private def unescape1u(src : StringView) : {Char | String, StringView}
      if src.starts_with?('{')
        src = src.rest
        # \u{⏏

        digitsrc, digits, src = src.skip_thru_seq(limit: 6, &.hexdigit?)
        if digits.empty?
          raise SyntaxError.new("expected 1-6 hex digits in `\\u{}`", digitsrc)
        end

        unless src.starts_with?('}')
          raise SyntaxError.new("expected `}` to close `\\u{`", src.before_begin)
        end
        src = src.rest
        # \u{61}⏏

        codepoint = 0
        digits.each do |digit|
          codepoint = (codepoint << 4) | digit
        end

        begin
          return codepoint.chr, src
        rescue ArgumentError
          raise SyntaxError.new("unsupported codepoint `#{codepoint.to_s(base: 16)}`", digitsrc)
        end
      end

      if src.starts_with?("[:")
        src = src.rest.rest
        # \u[:⏏

        name, src = src.skip_thru(U_EMOJI_NAME_CHARSET)
        if name.empty?
          raise SyntaxError.new("expected emoji name", name)
        end
        # \u[:poop⏏

        unless src.starts_with?(":]")
          raise SyntaxError.new("expected `:]` to close `\\u[:`", src.before_begin)
        end

        src = src.rest.rest
        # \u[:poop:]⏏

        begin
          return emoji(name.to_s), src
        rescue e : KeyError
          raise SyntaxError.new(e.message.not_nil!("BUG: missing error message"), name)
        end
      end

      if src.starts_with?('[')
        src = src.rest
        # \u[⏏

        name, src = src.skip_thru(U_CODEPOINT_NAME_CHARSET)
        if name.empty?
          raise SyntaxError.new("expected Greek letter, codepoint, or emoji name (e.g. `\\u[delta]`, or \\u[greek capital letter delta]`, or `\\u[:poop:]`)", name)
        end

        unless src.starts_with?(']')
          raise SyntaxError.new("expected `]` to close `\\u[`", src.before_begin)
        end
        src = src.rest
        # \u[delta]⏏

        begin
          return codepoint(name.to_s), src
        rescue e : KeyError
          raise SyntaxError.new(e.message.not_nil!("BUG: missing error message"), name)
        end
      end

      digitsrc, digits, src = src.skip_thru_seq(limit: 4, &.hexdigit?)
      if digits.present?
        unless digits.size == 4
          raise SyntaxError.new("expected exactly four hex digits after `\\u`", digitsrc)
        end

        # \u⏏beef -> \ubeef⏏
        chr = ((digits[0] << 12) | (digits[1] << 8) | (digits[2] << 4) | digits[3]).chr
        return chr, src
      end

      raise SyntaxError.new("expected one of `\\u{...}`, `\\u[...]`, `\\u____`", src.before_begin)
    end

    # Returns the `Char` with the codepoint *name* refers to.
    #
    # *name* is the `\u[...]` name of the codepoint, meaning either:
    #
    # - A Greek letter, such as `pi` for `π` and `Pi` for `Π`
    # - A Unicode codepoint name, such as `greek capital letter delta`.
    #
    # Raises `KeyError` if codepoint is not found.
    def codepoint(name : String) : Char
      if chr = ::Ww::Unicode::Index.greek?(name)
        return chr
      end

      # Normalize before looking up the codepoint.
      normname = name.upcase.squeeze(" ")

      if codepoint = ::Ww::Unicode::Index.codepoint?(normname)
        begin
          return codepoint.chr
        rescue ArgumentError
          raise KeyError.new("unsupported codepoint `#{codepoint.to_s(base: 16)}`")
        end
      end

      raise KeyError.new("`#{name}` is not a Greek letter name nor a Unicode codepoint name")
    end

    # Returns the emoji *name* refers to. Raises `KeyError` if such an emoji
    # does not exist.
    def emoji(name : String) : String
      ::Ww::Unicode::Index.emoji?(name) || raise KeyError.new("`#{name}` is not an emoji")
    end
  end
end
