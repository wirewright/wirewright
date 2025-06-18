module Ww::ML::Grammar
  # String literal rules.
  #
  # - Standard string literal rules (`std`), as in `⏏"hello world"`.
  # - Raw string literal rules (`raw`), as in `⏏⎡hello world⎤`.
  module Str
    extend self

    alias Frag = Char | String | StringView | Term

    # The set of characters that are allowed in strings.
    CHARSET = %[\n\t\r\u0020-\u{10FFFF}]

    # Parses a string literal.
    #
    # See: `std`, `raw`.
    def string(escape : P::Pi, interpolation : P::Pi) : P::Pi
      P.choice(
        std(escape, interpolation),
        raw,
      )
    end

    # Parses a standard string literal, as in `⏏"hello world"`.
    #
    # Parseout type is a `Term` due to interpolation (as in `"⏏⸢qux⸣⏏", which
    # is the same as writing `qux` -- i.e., a symbol).
    #
    # *escape* is used to parse escape sequences, as in `⏏\n`.
    #
    # *term* is used to parse terms inside interpolation, as in `⸢⏏⸣`.
    def std(escape : P::Pi, interpolation : P::Pi) : P::Pi
      P.prefixed(
        P.chrseq("\""),
        P.reduce(
          initial: [] of Frag,
          successor: P.choice(
            escape,
            interpolation,
            char,
            P.refuse("expected `\"` to end the string")
          ),
          min: 0,
          delimiter: P.chrseq("\""),
        ),
        &->join(Array(Frag))
      )
    end

    # :nodoc:
    def join(frags : Array(Frag)) : Term
      if frags.empty? # ""
        return Term.of("")
      end

      cursor = 0
      captions = [] of Term

      while cursor < frags.size
        frag = frags[cursor]

        case frag
        in Char, String, StringView
          caption = String.build do |io|
            io << frag
            cursor += 1

            while cursor < frags.size
              other = frags[cursor]

              case other
              in Char, String, StringView
                io << other
                cursor += 1
              in Term
                # The cursor should remain on the term so that the outer loop
                # can handle it.
                break
              end
            end
          end

          next if caption.empty?

          captions << Term.of(caption)
        in Term
          captions << frag
          cursor += 1
        end
      end

      expect captions.size > 0

      if captions.size == 1
        # "⸢qux⸣"
        # "\u[delta]: hello\n world"
        return captions[0]
      end

      stitch = Term::Dict.build do |commit|
        commit << :~
        commit.concat(captions)
      end

      Term.of(stitch)
    end

    # Parses string interpolation, as in `⸢qux⸣`, using *interior* to parse
    # the interior term.
    def interpolation(interior : P::Pi, desc : String = "interpolation interior") : P::Pi
      P.surrounded(
        P.chrseq("⸢"),
        P.choice(interior, P.refuse("expected #{desc} (without whitespace after `⸢`)")),
        P.chrseq("⸣", detail: "expected `⸣` to close `⸢` (without whitespace before `⸣`)"),
        strict: true,
      )
    end

    # :nodoc:
    def char : P::Pi # StringView
      P.view(P.chr(CHARSET))
    end

    # Parses a raw string literal, as in `⏏⎡hello world⎤`. Parseout type
    # is `Term::Str`.
    def raw : P::Pi
      interior = P.pastchr(CHARSET, nest: "⎡", unnest: "⎤")

      P.surrounded(
        P.chrseq("⎡"),
        P.capture(interior) { |caption| Term[caption] },
        P.chrseq("⎤", detail: "expected `⎤` to close `⎡`"),
        strict: true,
      )
    end
  end
end
