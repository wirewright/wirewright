module ::Ww::ML::Grammar
  # Symbol literal rule constructors.
  module Symbol
    extend self

    # Symbolic nonletter character set. These characters, along with Unicode letter
    # characters (category L), are considered *symbolic characters*.
    SYMBOLIC_NONLETTER = %{0-9_!$%&*+\\-\\^./#<=>?@~λ|∞°∈⊆⊂\\\\}

    # A subset of symbolic characters that can be treated as symbol unambiguously
    # (if also followed by `SYMBOLIC_QUICK_DELIM`).
    SYMBOLIC_QUICK = %{a-zA-Z_\\-}

    # After we see `SYMBOLIC_QUICK` followed by this we are **certain**
    # it's a symbol, no going back.
    SYMBOLIC_QUICK_DELIM = %| \n)}]⟩|

    # Parses one symbolic character.
    def symbolic : P::Pi
      P.choice(
        P.chr(&.letter?),
        P.chr(SYMBOLIC_NONLETTER, cls: Charset32),
      )
    end

    # Fast path for symbol literals. The parseout type is `Term::Sym`.
    #
    # Symbols are by far the most frequent character-consumer in WwML. The vast
    # majority of symbols are purely alphabetic sequences followed by whitespace
    # or one of the closing brackets. This is the path we're optimizing for here,
    # at the cost of a backtrack.
    def quick : P::Pi
      chars = P.pastchr(SYMBOLIC_QUICK, min: 1)
      delim = P.ahead(P.chr(SYMBOLIC_QUICK_DELIM))
      core = P.postfixed(P.view(chars), delim)

      P.select(core) do |s, caption|
        case caption
        when "true", "false"
          P.refusal(s, "not a symbol")
        else
          Term::Sym.new(caption.to_s)
        end
      end
    end

    # Parses a symbol literal. The parseout type is `Term::Sym`.
    #
    # Slow (and thorough) path for symbol literals.
    #
    # This rule is usually "activated" as a last resort, after everything
    # else has been tried; and nothing would "fit".
    def symbol : P::Pi
      chars = P.pastchr(SYMBOLIC_NONLETTER, cls: Charset32, min: 1, &.letter?)

      # HACK: ^ is a valid symbol character whereas `…` is not; we have to manually
      # hit the brake pedal here, or else `^` will be consumed & become a symbol and `…`
      # will be left as-is; BUT `^…` is a token of its own so we cannot have that. This
      # is the types of problems a lexer solves I suppose, but we don't have a lexer
      # so let's not think about it!1
      core = P.prefixed(P.not(P.chrseq("^…")), P.locrange(P.view(chars)))

      P.select(core) do |_, (loc, caption)|
        symbolf(loc, caption)
      end
    end

    # :nodoc:
    def symbolf(loc, caption : StringView)
      if caption.first_char.ascii_number?
        return P.refusal(loc, "symbol cannot start with a digit")
      end

      Term::Sym.new(caption.to_s)
    end
  end
end
