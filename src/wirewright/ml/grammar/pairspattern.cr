module Ww::ML::Grammar
  # Rules for parsing dictionary pairspattern, as in `{¦⏏ residue_ x_number y⁺ z⋮ 50}`.
  #
  # NOTE: As it happens sometimes, the name *pairspattern* stuck to something
  # that isn't quite about "pairs"; but rather, about "entries", because you
  # can match dict items with them too (although it would be considered strange
  # if you did that). Anything else sounds even clumsier, though, so I'm
  # keeping the name.
  module Pairspattern
    extend self

    # Parses a dictionary pairspattern. The parseout type is `Term`.
    #
    # - *symbol* is used to parse symbols, as in `-⏏x⏏: foo`.
    # - *spacing* is used to parse spacing between `:` and value and in
    #   other places where that is appropriate. **It must match exactly one
    #   instance of spacing.** This and related rules will manage repeated
    #   spacing themselves.
    # - *term* is used to parse terms.
    # - *pair* must match one key-value pair (whatever the caller wants that to be).
    #   The importantant part is its parseout must be of the following type:
    #   `{ {keyloc : LocationRange, key : Term}, value : Term }`.
    # - *delim* can be set for the sequence of entries. **It will be consumed.**
    # - *delimdetail* can be set as the refusal message when this rule suspects
    #   the delimiter is missing.
    def pairspattern(
      symbol : P::Pi,
      spacing : P::Pi,
      term : P::Pi,
      pair : P::Pi,
      delim : P::Pi? = nil,
      delimdetail : String? = nil,
    ) : P::Pi
      residue = residue(spacing, term)
      entries = entries(symbol, spacing, term, pair, delim, delimdetail)

      P.seq(P.optional(residue), entries) do |residue_, entries_|
        if entries_.empty?
          Term.of(residue_ || Term[])
        elsif residue_.nil? || residue_ == Term[]
          Term.of(entries_)
        else
          Term.of(:"%layer", residue_, entries_)
        end
      end
    end

    # Parses pairspattern residue. The parseout type is `Term`.
    def residue(spacing : P::Pi, term : P::Pi) : P::Pi
      P.postfixed(
        P.surrounded(
          P.reduce(spacing, min: 0),
          term,
          P.reduce(spacing, min: 0),
        ),
        P.not(P.chr(%{:⋮})),
      )
    end

    # Parses zero or more pairspattern entries. The parseout type is `Term::Dict`.
    #
    # See `pairspattern` for parameter overview.
    def entries(
      symbol : P::Pi,
      spacing : P::Pi,
      term : P::Pi,
      pair : P::Pi,
      delim : P::Pi?,
      delimdetail : String? = nil,
    ) : P::Pi
      entry = P.surrounded(
        P.reduce(spacing, min: 0),
        entry(symbol, spacing, term, pair),
        P.reduce(spacing, min: 0),
      )

      P.cat(
        builder: DictFactory.new,
        successor: delimdetail ? P.choice(entry, P.refuse(delimdetail)) : entry,
        min: 0,
        delimiter: delim,
      )
    end

    # Parses one pairspattern entry. The parseout type is `{Term, Term}`.
    #
    # See `pairspattern` for parameter overview.
    def entry(symbol : P::Pi, spacing : P::Pi, term : P::Pi, pair : P::Pi) : P::Pi
      P.choice(
        negation(symbol, spacing, term),
        superscript(term),
        itemlike(symbol),
        optional(symbol, spacing, term),
        P.select(pair) { |_, ((loc, k), v)| pairf(loc, k, v) },
      )
    end

    # :nodoc:
    def pairf(keyloc, key : Term, value : Term)
      unless (keysym = key.as_sym?) && (keyblank = keysym.blank?)
        return key, value
      end

      verify(keyloc, keyblank) do |name|
        type = keyblank.type

        Term.case(value, engine: M0) do
          match({:"%optional", :_, :_}, cue: :"%optional") do
            unless type.any?
              return P.failure(keyloc, "restricting the type has no meaning here")
            end

            # x_: (%optional 0 <>) => x: (%optional 0 (%let x <>))
            {Term.of(name), Term.of(value.morph({2, Term.of(:"%let", name, value[2])}))}
          end

          otherwise do
            if type.any?
              {Term.of(name), Term.of(:"%let", name, value)}
            else
              {Term.of(name), Term.of(:"%let", name, {:"%all", type.blank, value})}
            end
          end
        end
      end
    end

    # Parses the negation shorthand family. The parseout type is `{Term, Term}`.
    #
    # - a negated key such as `⏏-x`
    # - a negated key alias as in `⏏-x: foo`
    # - a negated blank as in `⏏-x_`
    def negation(symbol : P::Pi, spacing : P::Pi, term : P::Pi) : P::Pi
      interior = P.prefixed(
        P.chrseq("-"),
        P.seq(
          P.locrange(symbol),
          P.optional(
            P.seq(
              P.chrseq(":"),
              P.reduce(spacing, min: 1, mindetail: "expected at least one space after `:`"),
              term,
            ) { |_, _, v| v }
          )
        )
      )

      P.select(interior) { |_, (k, v)| negationf(*k, v) }
    end

    # :nodoc:
    def negationf(keyloc, key : Term::Sym, name : Term?)
      unless blank = key.blank?
        return Term.of(key), Term.of(:"%-", :_, name)
      end

      if name
        return P.failure(keyloc, "blank here has no meaning, name is already specified after `:`")
      end

      verify(keyloc, blank) do |name|
        {Term.of(name), Term.of(:"%-", blank.type.blank, name)}
      end
    end

    # Parses the superscript shorthand family. The parseout type is `{Term, Term}`.
    #
    # - `⏏x⁺`
    # - `⏏x⁻`
    def superscript(term : P::Pi) : P::Pi
      P.seq(term, P.view(P.chr("⁺⁻"))) do |key, sup|
        case sup
        when "⁺" then {key, Term.of(:"%let", key, true)}
        when "⁻" then {key, Term.of(:"%let", key, false)}
        else
          unreachable("unknown superscript")
        end
      end
    end

    # Parses the itemlike family of shorthands:
    #
    # - `⏏<name>_<type>`
    # - `⏏<name>`
    #
    # The parseout type is `{Term, Term}`.
    def itemlike(symbol : P::Pi) : P::Pi
      interior = P.postfixed(P.locrange(symbol), P.ahead(P.chr("^⋮:")))

      P.select(interior) { |_, args| itemlikef(*args) }
    end

    # :nodoc:
    def itemlikef(loc, key : Term::Sym)
      unless blank = key.blank?
        return Term.of(key), Term.of(:_)
      end

      verify(loc, blank) do |name|
        {Term.of(name), Term.of(key)}
      end
    end

    private def verify(loc, blank : Term::Sym::Blank, &)
      unless name = blank.name?
        return P.failure(loc, "expected a named blank")
      end

      if blank.poly?
        return P.failure(loc, "unexpected polyblank")
      end

      yield name
    end

    # Parses the optional family of shorthands:
    #
    # - `⏏⋮qux`
    # - `⏏x⋮ 123`
    #
    # The parseout type is `{Term, Term}`.
    def optional(symbol : P::Pi, spacing : P::Pi, term : P::Pi)
      P.choice(
        optional_prefix(symbol),
        optional_infix(symbol, spacing, term),
      )
    end

    # :nodoc:
    NEVER_DICT = Term.dict(:"%never")

    # :nodoc:
    def optional_prefix(symbol : P::Pi) : P::Pi
      P.prefixed(P.chrseq("⋮"), symbol) do |key|
        {Term.of(key), Term.of(:"%-", NEVER_DICT, key)}
      end
    end

    # :nodoc:
    def optional_infix(symbol : P::Pi, spacing : P::Pi, term : P::Pi) : P::Pi
      interior = P.infixed(
        P.locrange(symbol),
        P.chrseq("⋮"),
        P.prefixed(spacing, term),
      )

      P.select(interior) { |_, (k, v)| optional_infixf(*k, v) }
    end

    # :nodoc:
    def optional_infixf(keyloc, key : Term::Sym, value : Term)
      unless blank = key.blank?
        # FIXME: Is this seriously the best we can do?! Maybe have a Sym
        # constructor that does this? This must not, **must not** cost
        # an allocation!
        capture = Term::Sym.new(String.build { |io| io << key << value.type.blank })

        return Term.of(key), Term.of(:"%optional", value, capture)
      end

      verify(keyloc, blank) do |name|
        {Term.of(name), Term.of(:"%optional", value, key)}
      end
    end
  end
end
