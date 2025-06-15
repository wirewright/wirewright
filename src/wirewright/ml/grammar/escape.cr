class Ww::ML::Grammar
  # String escape sequence rules.
  class Escape
    def initialize(@g : Grammar)
    end

    # Appends an escaped representation of *chr* to *io*.
    #
    # NOTE: This method is located here because escape sequences are obviously
    # tighly coupled to parsing thereof. It's easier to keep things in sync if
    # they're as close to each other as they are now.
    def self.display(io, ch : Char) : Nil
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

    # Parses a string escape sequence such as `⏏\n`.
    Grammar.rule(escape, String | Char) do
      P.prefixed(P.chr("\\\\"), P.strict(escaped))
    end

    # :nodoc:
    Grammar.rule(escaped, String | Char) do
      P.choice(
        control,
        P.prefixed(P.chrseq("x"), byte),
        P.prefixed(P.chrseq("u"), codepoint),
        P.refuse("invalid escape sequence"),
      )
    end

    # :nodoc:
    Grammar.rule(control, Char) do
      P.choice(
        P.map(P.chr("\"")) { '"' },
        P.map(P.chr("\\\\")) { '\\' },
        P.map(P.chr("n")) { '\n' },
        P.map(P.chr("t")) { '\t' },
        P.map(P.chr("r")) { '\r' },
        P.map(P.chr("⸢")) { '⸢' },
      )
    end

    # :nodoc:
    Grammar.rule(byte, Char) do
      P.choice(
        P.map(P.seq(@g.hexdigit, @g.hexdigit)) { |d0, d1| ((d0 << 4) | d1).chr },
        P.refuse("expected exactly two hex digits after `\\x`"),
      )
    end

    # :nodoc:
    Grammar.rule(codepoint, String | Char) do
      P.choice(ucurly, ubracket, uni4)
    end

    # :nodoc:
    Grammar.rule(ucurly, Char) do
      P.surrounded(
        P.chrseq("{", detail: "expected `{` to begin `\\u{`"),
        uni6,
        P.chrseq("}", detail: "expected `}` to close `\\u{`")
      )
    end

    # :nodoc:
    Grammar.rule(ubracket, String | Char) do
      P.surrounded(
        P.chrseq("[", detail: "expected `[` to begin `\\u[`"),
        byname,
        P.chrseq("]", detail: "expected `]` to close `\\u[`"),
      )
    end

    # :nodoc:
    Grammar.rule(uni4, Char) do
      core = P.reduce(
        initial: 0,
        successor: @g.hexdigit,
        min: 4,
        max: 4,
        mindetail: "expected exactly four hex digits after `\\u`"
      ) { |codepoint, digit| (codepoint << 4) | digit }

      P.map(core, &.chr)
    end

    # :nodoc:
    Grammar.rule(uni6, Char) do
      core = P.seq(
        P.loc,
        P.reduce(
          initial: 0,
          successor: @g.hexdigit,
          min: 1,
          max: 6,
          mindetail: "expected 1-6 hex digits in `\\u{}`"
        ) { |codepoint, digit| (codepoint << 4) | digit },
      )

      P.select(core) do |_, (loc, codepoint)|
        codepoint.chr
      rescue ArgumentError
        P.refusal(loc, "unsupported codepoint `#{codepoint.to_s(base: 16)}`")
      end
    end

    # :nodoc:
    Grammar.rule(byname, String | Char) do
      P.choice(
        emoji,
        chrname,
        P.refuse("expected Greek letter, codepoint, or emoji name (e.g. `\\u[delta]`, or \\u[greek capital letter delta]`, or `\\u[:poop:]`)"),
      )
    end

    # Defines the characters allowed in emoji names.
    EMOJI_NAME_CHARSET = "a-zA-Z0-9_"

    # :nodoc:
    Grammar.rule(emoji, String) do
      chars = P.pastchr(EMOJI_NAME_CHARSET, min: 1)
      core = P.surrounded(
        P.chrseq(":", detail: "expected `:` before emoji name"),
        P.seq(P.loc, P.view(chars)),
        P.chrseq(":", detail: "expected `:` after emoji name"),
      )

      P.select(core) { |_, args| emojif(*args) }
    end

    # :nodoc:
    def emojif(loc, chars : StringView)
      caption = chars.to_s

      unless emoji = Unicode::Index.emoji?(caption)
        return P.refusal(loc, "emoji `#{caption}` not found")
      end

      emoji
    end

    # Defines the characters allowed in codepoint names.
    CODEPOINT_NAME_CHARSET = "a-zA-Z0-9() \\-"

    # :nodoc:
    Grammar.rule(chrname, Char) do
      chars = P.pastchr(CODEPOINT_NAME_CHARSET, cls: Charset16, min: 1)
      core = P.seq(P.loc, P.view(chars))

      P.select(core) { |_, args| chrnamef(*args) }
    end

    # :nodoc:
    def chrnamef(loc, chars : StringView)
      caption = chars.to_s

      if chr = Unicode::Index.greek?(caption)
        return chr
      end

      # Normalize before looking up the codepoint.
      caption = caption.upcase.squeeze(" ")

      if codepoint = Unicode::Index.codepoint?(caption)
        begin
          return codepoint.chr
        rescue ArgumentError
          return P.refusal(loc, "unsupported codepoint `#{codepoint.to_s(base: 16)}`")
        end
      end

      P.refusal(loc, "`#{caption}` is not a Greek letter name nor a Unicode codepoint name")
    end
  end
end
