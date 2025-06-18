module Ww::ML::Grammar
  module Number
    extend self

    private alias G = Grammar

    # Parses a number literal.
    #
    # NOTE: there are several different kinds of number literals in WwML, and
    # some of them emit dictionaries rather than numbers to preserve the notation.
    # Thus the return type is not just a number but also a dict. Make sure to
    # filter appropriately.
    G.rule(number, Term::Num | Term::Dict) do
      signed(mag) { |n| n.type.number? ? n.unsafe_as_n * Term[-1] : Term[:-, n] }
    end

    # :nodoc:
    G.rule(mag, Term::Num | Term::Dict) do
      P.postfixed(
        P.choice(radixmag, fracmag, decmag),
        P.strict(P.ahead(Symbol.nonsymbolic), detail: "trailing symbolic characters found after number"),
      )
    end

    # :nodoc:
    G.rule(fracmag, Term::Num) do
      core = P.locrange(
        P.infixed(
          nat,
          P.chrseq("/", detail: "expected `/` (without whitespace)"),
          P.strict(nat, detail: "expected at least one digit for the denominator"),
        ),
      )

      P.select(core) do |s, (loc, (p, q))|
        q.zero? ? P.failure(loc, "division by zero") : p/q
      end
    end

    # :nodoc:
    G.rule(decmag, Term::Num | Term::Dict) do
      core = P.seq(decfrac, P.optional(expn))

      P.map(core) do |mantissa, exponent|
        exponent ? Term[:sci, mantissa, exponent] : mantissa
      end
    end

    # :nodoc:
    G.rule(decfrac, Term::Num) do
      core = P.seq(nat, P.optional(decfracpart, default: Term[0]))

      P.map(core) { |int, fracpart| int + fracpart }
    end

    # :nodoc:
    G.rule(decfracpart, Term::Num) do
      core = P.prefixed(
        P.chrseq("."),
        P.strict(natdigits, detail: "expected at least one fractional part digit after `.`"),
      )

      P.map(core) { |value, size| value / Term[10]**size }
    end

    # :nodoc:
    G.rule(expn, Term::Num) do
      P.prefixed(
        P.chr("eE"),
        P.strict(int, detail: "expected at least one digit for the exponent"),
      )
    end

    # Parses a natural number with a sign.
    G.rule(int, Term::Num) do
      signed(nat) { |n| Term[-1] * n }
    end

    # Parses a natural number (zero or positive).
    G.rule(nat, Term::Num) do
      P.map(natdigits) { |value, _| value }
    end

    # :nodoc:
    #
    # Enables an optional prefix sign `+` or `-` for *x*. Calls *neg* with the return
    # value of *x* to negate if necessary.
    def signed(x : P::Pi(State -> {T, State} | P::Err), &neg : T -> U) forall T, U
      P.choice(
        P.prefixed(P.chrseq("+"), P.choice(x, P.refuse("expected digits after the `+` sign"))),
        P.map(P.prefixed(P.chrseq("-"), P.choice(x, P.refuse("expected digits after the `-` sign"))), &neg),
        x,
      )
    end

    # :nodoc:
    def natdigits(**kwargs)
      digits = P.pastchr("0-9_", min: 1)
      core = P.locrange(P.view(digits))

      P.select(core) { |_, args| natf(*args, **kwargs) }
    end

    # :nodoc:
    def natf(startrng, digits : StringView)
      if digits.starts_with?('_')
        return P.refusal(startrng.first(1), "leading underscores forbidden in numbers")
      end

      if digits.ends_with?('_')
        return P.failure(startrng.last(1), "trailing underscores forbidden in numbers")
      end

      size = Term[0]
      value = Term[0]
      state = :digit

      digits.each_char_with_rel_byte_index do |digit, offset|
        case {state, digit}
        when {:underscore, '_'}
          return P.failure(startrng.at(offset).halo(1), "multiple consecutive underscores forbidden in numbers")
        when {:underscore, _}
          state = :digit
        when {:digit, '_'}
          state = :underscore
          next
        when {:digit, _}
        else
          unreachable("unknown state in natf: #{state}")
        end

        value = value.append(Term[digit - '0'], base: Term[10])
        size += 1
      end

      {value, size}
    end

    # :nodoc:
    G.rule(radixmag, Term::Dict) do
      letters = P.pastchr("a-zA-Z0-9_", min: 1, mindetail: "expected at least one digit before radix subscript")
      core = P.seq(P.locrange(P.view(letters)), P.locrange(subnat))

      P.select(core) do |_, ((startrng, digits), (baseloc, base))|
        radixf(startrng, digits, baseloc, base)
      end
    end

    # :nodoc:
    def radixf(startloc, digits : StringView, baseloc, base : Int32)
      unless 1 <= base <= 62
        return P.failure(baseloc, "expected radix 1-62, not `#{base}`")
      end

      if digits.starts_with?('_')
        # We fail here as opposed to e.g. natf, because we *already* found a valid
        # looking subscript. Thus the client probably meant to have a radix number
        # here and we're responsible for that.
        return P.failure(startloc.first(1), "leading underscores forbidden in numbers")
      end

      if digits.ends_with?('_')
        return P.failure(startloc.last(1), "trailing underscores forbidden in numbers")
      end

      Term::Dict.build do |commit|
        commit << :digits
        commit.with(:radix, base)

        state = :digit

        digits.each_char_with_rel_byte_index do |digit, offset|
          case {state, digit}
          when {:digit, '_'}
            state = :underscore
            next
          when {:digit, _}
          when {:underscore, '_'}
            return P.failure(startloc.at(offset).halo(1), "multiple consecutive underscores forbidden in numbers")
          when {:underscore, _}
            state = :digit
          else
            unreachable("unknown state in radixf: #{state}")
          end

          # Radices less than or equal to 36 are case-insensitive.
          normdigit = 11 <= base <= 36 ? digit.upcase : digit

          unless value = digit?(normdigit, base)
            return P.failure(startloc.at(offset).first(1), "invalid digit `#{digit}` for base `#{base}`")
          end

          commit << Term[value]
        end
      end
    end

    private def digit?(ch : Char, base : Int32) : Int32?
      return unless 1 <= base <= 62

      if ch.in?('0'..'9')
        digit = ch - '0'
      elsif ch.in?('A'..'Z')
        digit = 10 + (ch - 'A')
      elsif ch.in?('a'..'z')
        digit = 36 + (ch - 'a')
      else
        return
      end

      digit < base ? digit : nil
    end

    # Parses a subscript natural number, e.g. `₁₀`, represented as an Int32.
    G.rule(subnat, Int32) do
      digits = P.pastchr("₀-₉", min: 1, mindetail: "expected at least one subscript digit")
      core = P.locrange(P.view(digits))

      P.select(core) { |_, args| subnatf(*args) }
    end

    # :nodoc:
    def subnatf(startloc, digits : StringView)
      value = 0
      digits.each_char do |digit|
        value = value * 10 + (digit - '₀')
      end

      value
    end
  end
end
