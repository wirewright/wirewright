module Ww::ML::Grammar
  # String escape sequence rules.
  module Escape
    extend self

    # Appends an escaped representation of *chr* to *io*.
    #
    # NOTE: This method is located here because escape sequences are obviously
    # tighly coupled to parsing thereof. It's easier to keep things in sync when
    # they're as close to each other as they are now.
    def display(io, ch : Char) : Nil
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

    # Parses a string escape sequence such as `⏏\n`. Parseout type is `String | Char`.
    def escape : P::Pi
      P.prefixed(P.chr("\\\\"), P.strict(escaped))
    end

    # :nodoc:
    def escaped : P::Pi # String | Char
      P.choice(
        control,
        P.prefixed(P.chrseq("x"), P.strict(byte)),
        P.prefixed(P.chrseq("u"), P.strict(codepoint)),
        P.refuse("invalid or abruptly terminated escape sequence, expected one of: `\"⸢ntrxu\\` or newline"),
      )
    end

    # :nodoc:
    def control : P::Pi # String | Char
      P.choice(
        P.map(P.chr("\"")) { '"' },
        P.map(P.chr("\\\\")) { '\\' },
        P.map(P.chr("n")) { '\n' },
        P.map(P.chr("t")) { '\t' },
        P.map(P.chr("r")) { '\r' },
        P.map(P.chr("⸢")) { '⸢' },
        P.seq(P.chr("\n"), P.pastchr(" \t", min: 0)) { "" },
      )
    end

    # :nodoc:
    def byte : P::Pi # Char
      P.refuse_with("expected exactly two hex digits after `\\x`",
        P.seq(hexdigit, hexdigit) { |d0, d1| ((d0 << 4) | d1).chr }
      )
    end

    # :nodoc:
    def codepoint : P::Pi #  String | Char
      P.choice(
        ucurly,
        ubracket,
        uni4,
        P.refuse("expected one of `\\u{...}`, `\\u[...]`, `\\u____`")
      )
    end

    # :nodoc:
    def ucurly : P::Pi # Char
      P.surrounded(
        P.chrseq("{", detail: "expected `{` to begin `\\u{`"),
        uni6,
        P.chrseq("}", detail: "expected `}` to close `\\u{`"),
      )
    end

    # NOTE: we have to go through this BracketedEntity hell to get error messages
    # that make more sense specifically for `\u[]`. We're implementing lazy
    # resolution here, in the sense that `\u[:qux:` fails with "expected `]`"
    # rather than "`qux` not found". For some reason this feels more natural.
    # Versus e.g. ucurly where eager handling feels more natural.

    # :nodoc:
    enum Bracketed
      Emoji
      Codename
    end

    # :nodoc:
    alias BracketedEntity = {Bracketed, P::LocationRange, StringView}

    # :nodoc:
    def ubracket : P::Pi # String | Char
      core = P.surrounded(
        P.chrseq("[", detail: "expected `[` to begin `\\u[`"),
        byname,
        P.chrseq("]", detail: "expected `]` to close `\\u[`"),
      )

      P.select(core) do |_, (kind, loc, caption)|
        case kind
        in .emoji?    then emojif(loc, caption)
        in .codename? then codenamef(loc, caption)
        end
      end
    end

    # :nodoc:
    def emojif(loc, chars : StringView)
      caption = chars.to_s

      unless emoji = Unicode::Index.emoji?(caption)
        return P.refusal(loc, "`#{caption}` is not an emoji")
      end

      emoji
    end

    # :nodoc:
    def codenamef(loc, chars : StringView)
      caption = chars.to_s

      if chr = Unicode::Index.greek?(caption)
        return chr
      end

      # Normalize before looking up the codepoint.
      normcaption = caption.upcase.squeeze(" ")

      if codepoint = Unicode::Index.codepoint?(normcaption)
        begin
          return codepoint.chr
        rescue ArgumentError
          return P.refusal(loc, "unsupported codepoint `#{codepoint.to_s(base: 16)}`")
        end
      end

      P.refusal(loc, "`#{caption}` is not a Greek letter name nor a Unicode codepoint name")
    end

    # :nodoc:
    def uni4 : P::Pi # Char
      core = P.refuse_with("expected exactly four hex digits after `\\u`",
        P.reduce(
          initial: 0,
          successor: hexdigit,
          min: 4,
          max: 4,
        ) { |codepoint, digit| (codepoint << 4) | digit }
      )

      P.map(core, &.chr)
    end

    # :nodoc:
    def uni6 : P::Pi # Char
      core = P.refuse_with("expected 1-6 hex digits in `\\u{}`",
        P.locrange(
          P.reduce(
            initial: 0,
            successor: hexdigit,
            min: 1,
            max: 6,
          ) { |codepoint, digit| (codepoint << 4) | digit },
        )
      )

      P.select(core) do |_, (loc, codepoint)|
        codepoint.chr
      rescue ArgumentError
        P.refusal(loc, "unsupported codepoint `#{codepoint.to_s(base: 16)}`")
      end
    end

    # :nodoc:
    def byname : P::Pi # {Bracketed, P::LocationRange, StringView}
      P.choice(
        emoji,
        chrname,
        P.refuse("expected Greek letter, codepoint, or emoji name (e.g. `\\u[delta]`, or \\u[greek capital letter delta]`, or `\\u[:poop:]`)"),
      )
    end

    # Defines the characters allowed in emoji names.
    EMOJI_NAME_CHARSET = "a-zA-Z0-9_"

    # :nodoc:
    def emoji : P::Pi # BracketedEntity
      chars = P.pastchr(EMOJI_NAME_CHARSET, min: 1, mindetail: "expected emoji name")

      P.surrounded(
        P.chrseq(":", detail: "expected `:` before emoji name"),
        P.locrange(P.view(chars)),
        P.chrseq(":", detail: "expected `:` after emoji name"),
      ) { |loc, codepoint| {Bracketed::Emoji, loc, codepoint} }
    end

    # Defines the characters allowed in codepoint names.
    CODEPOINT_NAME_CHARSET = "a-zA-Z0-9() \\-"

    # :nodoc:
    def chrname : P::Pi # BracketedEntity
      chars = P.pastchr(CODEPOINT_NAME_CHARSET, cls: Charset16, min: 1, mindetail: "expected codepoint name")
      core = P.locrange(P.view(chars))

      P.map(core) { |loc, codepoint| {Bracketed::Codename, loc, codepoint} }
    end

    # :nodoc:
    def hexdigit : P::Pi # Int32
      core = P.chr("a-fA-F0-9")

      P.capture(core, &.first_char.to_i(base: 16))
    end
  end
end
