# A tiny string parsing language. Used mainly by `Rack::Parser`, which
# implements the `rack.parser` node.
#
# This is an implementation of what I can loosely identify as a Packrat
# parser, but with some extensions. Mainly, it is extended with support
# for direct and indirect left recursion; additionally, I added a longest-
# match choice operator, which, I suppose, turns this into something that
# is not a PEG after all.
#
# See `parsekit.parselet` for docs on parselets. See `parsekit.grammar` for docs on
# the way we define grammars.
#
# References:
# - ["Packrat parsers can support left recursion" (Warth et al.)](https://doi.org/10.1145/1328408.1328424).
module Ww::ParseKit
  extend self

  # |@ parsekit.parselet
  #
  # |@block
  # We call parser operators / combinators *parselets* for simplicity; although
  # the word doesn't mean exactly what we use it for here. It seems generic enough
  # to work, though.

  alias Parselet = Reject | Stringp | RuleRef | OrdChoice | MaxChoice |
                   Seq | Many | ManySep | Capture | Location | Find | Form

  defrecord Reject
  defrecord Stringp, pattern : ScanKit::Pattern, observed : Bool
  defrecord RuleRef, name : Term::Sym
  defrecord OrdChoice, members : Slice(Parselet)
  defrecord MaxChoice, members : Slice(Parselet)
  defrecord Seq, members : Slice(Parselet), observed : Bool
  defcase Many, min : UInt32, max : UInt32, member : Parselet, observed : Bool
  defcase ManySep, min : UInt32, max : UInt32, member : Parselet, sep : Parselet, trailing : Bool, observed : Bool
  defcase Capture, name : Term, member : Parselet
  defcase Location, name : Term, member : Parselet
  defcase Find, member : Parselet
  defcase Form, member : Parselet, spec : FormSpec

  def parselet(term : Term, observed : Bool) : Parselet
    Term.case(term) do
      # |@ parsekit.parselet.stringp
      #
      # |@pattern
      # _string
      #
      # |@block
      # Matches a string pattern.
      #
      # The underlying engine is called ScanKit. The syntax of patterns is inspired
      # by Lua's string patterns, but only in a very minor way.
      #
      # Reference: https://www.lua.org/pil/20.2.html.
      #
      # ## Anchoring
      #
      # In `parsekit.parselet.stringp`, you cannot modify the anchoring of the pattern.
      # In other contexts, however, ScanKit lets you modify the anchoring:
      #
      # - Prefixing the pattern with `…` detaches it from the beginning of the input string.
      # - Adding `…` at the end of the pattern detaches it from the end of the input string.
      #
      # By default, the pattern is attached to both ends of the string, meaning it must
      # describe the entire string. By modifying the anchoring you allow parts of the string
      # before the match (or after, or both) to be skipped. For example:
      # `…date←(day←%|dd|/month←%|dd|/year←%|dddd|)…` will match `"Deadline is 30/05/1999, firm"`
      # successfully.
      #
      # ## Categories
      #
      # Categories are prefixed by `%`.
      #
      # | Syntax | Meaning                                                                               |
      # | ------ | ------------------------------------------------------------------------------------- |
      # | `%_`   | Matches any character.                                                                |
      # | `%a`   | Matches an alphabetic character (uppercase or lowercase letter according to Unicode). |
      # | `%b`   | Matches a binary digit (shorthand for `[01]`).                                        |
      # | `%c`   | Matches a control character according to Unicode.                                     |
      # | `%d`   | Matches a decimal digit (shorthand for `[0-9]`).                                      |
      # | `%e`   | Matches an emoji.                                                                     |
      # | `%g`   | Matches a grapheme.                                                                   |
      # | `%h`   | Matches a horizontal space character according to Unicode.                            |
      # | `%L`   | Matches an uppercase letter according to Unicode.                                     |
      # | `%l`   | Matches a lowercase letter according to Unicode.                                      |
      # | `%n`   | Matches a number character according to Unicode.                                      |
      # | `%o`   | Matches an octal digit (shorthand for `[0-7]`).                                       |
      # | `%p`   | Matches a punctuation character according to Unicode.                                 |
      # | `%s`   | Matches a horizontal or vertical space character (shorthand for `[%h%v]`.             |
      # | `%v`   | Matches a vertical space character according to Unicode.                              |
      # | `%w`   | Matches a word character (shorthand for `[a-zA-Z0-9_]`).                              |
      # | `%x`   | Matches a hex digit (shorthand for `[0-9a-fA-F]`).                                    |
      #
      # To match the character `%` itself, use the character set syntax: `[%]`.
      #
      # ## Category sequences
      #
      # Instead of writing e.g. `%d%d%d%d` you can use the category sequence
      # notation: `%|dddd|`. Characters that are not associated with a category
      # are matched literally. So you can write, for example: `%|dd-dd-dddd|`.
      # This matches strings such as `25-01-2026`. `-` is matched literally
      # because it does not correspond to any category.
      #
      # ## Character sets
      #
      # Character sets are delimited by the square brackets `[]`. Things inside
      # a character set are called *character units*. There can be zero or more
      # character units inside a character set. An empty character set will never
      # match. Each character unit is allowed to contribute any number of characters
      # to the set. This means, for instance, that overlapping character units are
      # OK (e.g., `[a-cb-f]`)
      #
      # | Syntax    | Meaning                                                                                                     |
      # | --------- | ----------------------------------------------------------------------------------------------------------- |
      # | *c*       | Contributes one character *c*. For example: `[x]`, `[ax]`.                                                  |
      # | *c₀*-*c₁* | Contributes characters in the range *c₀* to *c₁* according to Unicode. For example, `[a-z]`, `[a-zA-Z0-9]`. |
      # | `%...`    | Contributes characters from a category. For example, `[%d]`, `[%L%l%d]`.                                    |
      #
      # You can escape `%` inside a character set by writing it at the end. For
      # example, `[a-z%]`. In fact, the recommended way to escape `%` by itself,
      # `[%]`, is a special case of this.
      #
      # Character sets have an optional "negative" part which you can use to subtract some
      # characters from the set. The negative part starts with `^`. For example, the pattern
      # `[a-z^%x]` matches a character that is in range `a-z` but is not a hex character
      # `[a-fA-F0-9]`.
      #
      # A negative-only character set starts with an empty positive part: `[^`. An empty
      # positive part matches any character. The negative part then tells which characters to
      # reject. For example, the pattern `[^a-z]` matches any character that is not in
      # the set `a-z`.
      #
      # As a curiosity, the above means `[]` will never match (the positive part is empty
      # and the negative part is absent), but `[^]` is equivalent to `%_` (the positive part
      # is empty but the negative part is present, therefore, the base case is to match any
      # character, and then the negative part doesn't subtract anything from that).
      #
      # A *quantifier* can be attached to a character set. For example, `[a-z]{0,3}`, `[0-9]*`.
      #
      # | Quantifier | Meaning                                                              |
      # | ---------- | -------------------------------------------------------------------- |
      # | `*`        | Zero or more times (shorthand for `{0,}`).                           |
      # | `+`        | One or more times (shorthand for `{1,}`).                            |
      # | `{n,}`     | *n* or more times. *n* is restricted to 1-4 digits (i.e., `0-9999`). |
      # | `{,n}`     | 0 up to *n* times. *n* is restricted to 1-4 digits.                  |
      # | `{m,n}`    | *m* up to *n* times. Both *m* and *n* are restricted to 1-4 digits.  |
      #
      # Whitespace around the comma `,` in `{}` quantifiers is ignored.
      #
      # > [!NOTE]
      # > You cannot use quantifiers with categories: the pattern`%d*` does not mean
      # > what you probably think it does, and will instead match `%d` (a digit) followed
      # > by literal `*` (asterisk). Instead, you have to remember that only character
      # > sets accept quantifiers; so put the `%d` in a character set like so: `[%d]`;
      # > and then attach the qualifier: `[%d]*`. To reiterate, something like `%d*%l*`
      # > matches digit-asterisk-letter-asterisk, whereas `[%d]*[%l]*` matches zero
      # > or more digits followed by zero or more asterisks. And something like `%d*%d`
      # > matches digit-asterisk-digit.
      #
      # ## Captures
      #
      # You can make named captures using `←`. If the capture is named using exactly
      # one letter, it can be written simply as `x←...`. For example: `x←%l`. For
      # captures with one or more letters in the name, the name must be wrapped
      # in `()`. For example, `(letter)←%l`.
      #
      # Captures bind to the next *atom*. An *atom* is a category (e.g., `x←%l`),
      # a category sequence (e.g., `(year)←%|dddd|`), or a character set
      # (e.g., `(name)←[a-zA-Z]`). In order to bind to a *sequence* of atoms,
      # use `()`. For example, `Deadline is date←(day←%|dd|/month←%|dd|/year←%|dddd|), firm`.
      #
      # ## Literal matching
      #
      # The whitespace character ` ` receives special treatment outside of character
      # sets. It is a shorthand for `[%s]+`. So for example, `%|dd| %|dd| %|dddd|`
      # "desugars" into the vastly less readable `%d%d[%s]+%d%d[%s]+%d%d%d%d`. To
      # match a literal whitespace character, wrap it in a character set: `[ ]`.
      #
      # Characters with no special treatment are handled literally. Therefore, for example,
      # the pattern `abc` matches the sequence of characters `a`, `b`, `c` literally.
      #
      # ### Escaping special characters
      #
      # Most special characters can be escaped using the character set notation `[...]`.
      # For example, to escape `%`, you can use `[%]`. Then e.g. `[%]x` will match
      # the sequence of characters `%`, `x` literally.
      #
      # To escape parts of the charcater set notation itself, namely `[`, `]`, and `^`,
      # use `%` as detailed in the table below.
      #
      # | Character | How to escape |
      # | --------- | ------------- |
      # | `[`       | `%[`          |
      # | `]`       | `%]`          |
      # | `^`       | `%^`          |
      #
      # For example, here is how the string `[a-z^%x]` can be escaped: `%[a-z^[%]x%]`.
      # Here, `[`, `]` were replaced by `%[` and `%]`, correspondingly; and other special
      # characters, including `%` itself, were replaced by character sets, e.g. `[%]`.
      #
      # Note that `^` doesn't have to be escaped outside of character sets. The only
      # reason you might want to use `%^` is in character sets themselves, if you want
      # to include `^` as one possible choice for the character. For example, `[a-z%^]`
      # allows characters in the range `a-z` or the caret `^`. So, for example, `^qux` will
      # match. On the other hand, `[\x20-\u{10FFFF}^%x%^]` matches all printable characters
      # *excluding* hex characters `[a-fA-F0-9]` and the caret `^`.
      matchpi %{_string} do
        pattern = ScanKit.recognize(term.to(String), anchor_l: true, anchor_r: false)
        Stringp.new(pattern, observed)
      end

      # |@ parsekit.parselet.ruleref
      #
      # |@pattern
      # (%symbol nonblank)
      #
      # |@block
      # Refers to a rule defined in the grammar.
      matchpi %{(%symbol nonblank)} do
        RuleRef.new(term.as_sym)
      end

      # |@ parsekit.parselet.any
      #
      # |@pattern
      # [any members_*]
      #
      # |@key members parsekit.parselet
      #
      # |@block
      # Sequential or: tries to match each of *members* in turn.
      matchpi %{[any  _*]} do
        subterms = term.items.move(1)
        members = subterms.to_readonly_slice { |subterm| parselet(subterm, observed).as(Parselet) }
        OrdChoice.new(members)
      end

      # |@ parsekit.parselet.max
      #
      # |@pattern
      # [max members_*]
      #
      # |@key members parsekit.parselet
      #
      # |@block
      # Parallel or: tries all of *members* simultaneously, and picks the member
      # whose match is the longest.
      matchpi %{[max _*]} do
        subterms = term.items.move(1)
        members = subterms.to_readonly_slice { |subterm| parselet(subterm, observed).as(Parselet) }
        MaxChoice.new(members)
      end

      # |@ parsekit.parselet.seq
      #
      # |@pattern
      # [seq members_*]
      #
      # |@key members parsekit.parselet
      #
      # |@block
      # Matches a sequence of *members*.
      matchpi %{[seq _*]} do
        subterms = term.items.move(1)
        parselets = subterms.to_readonly_slice { |subterm| parselet(subterm, observed).as(Parselet) }
        Seq.new(parselets, observed)
      end

      # |@ parsekit.parselet.many
      #
      # |@pattern
      # (many member_ ⍊
      #   min_: (%optional 0 (%number u32))
      #   max_: (%optional 4096 (%number u32)))
      #
      # |@key member parsekit.parselet
      #
      # |@key min
      # The minimum number of instances of *member*.
      #
      # |@key max
      # The maximum number of instances of *member*.
      #
      # |@block
      # Matches *min* to *max* instances of *member*.
      matchpi %{(many subterm_ ⍊ min_: (%optional 0 (%number u32)) max_: (%optional 4096 (%number u32)))} do
        Many.new(min.to(UInt32), max.to(UInt32), parselet(subterm, observed), observed)
      end

      # |@ parsekit.parselet.many
      #
      # |@pattern
      # (many member_ sep_ ⍊
      #   min_: (%optional 0 (%number u32))
      #   max_: (%optional 4096 (%number u32))
      #   trailing⋮ false)
      #
      # |@key member parsekit.parselet
      #
      # |@key sep parsekit.parselet
      # If the separator emits no captures, it is suppressed from the output of
      # `many` (accessible e.g. by capturing `r←(many _*)`) Otherwise it is included.
      #
      # |@key min
      # The minimum number of instances of *member*.
      #
      # |@key max
      # The maximum number of instances of *member*.
      #
      # |@key trailing
      # Whether to allow a trailing instance of *sep*.
      #
      # |@block
      # Matches *min* to *max* instances of *member* separated by *sep*, with an optional
      # trailing separator if *trailing* is `true`.
      matchpi(<<-WWML) do
      (many subterm_ sep_ ⍊
        min_: (%optional 0 (%number u32))
        max_: (%optional 4096 (%number u32))
        trailing⋮ false)
      WWML
        ManySep.new(min.to(UInt32), max.to(UInt32),
          parselet(subterm, observed),
          parselet(sep, observed),
          trailing.to(Bool),
          observed,
        )
      end

      # |@ parsekit.parselet.maybe
      #
      # |@pattern
      # [maybe member_]
      #
      # |@key member parsekit.parselet
      #
      # |@block
      # Matches zero or one instances of *member*. Shorthand for `(many _ min: 0 max: 1)`.
      matchpi %{[maybe subterm_]} do
        Many.new(0u32, 1u32, parselet(subterm, observed), observed)
      end

      # |@ parsekit.parselet.capture
      #
      # |@pattern
      # [%'%let name_ member_]
      #
      # |@key name
      # The name of the capture.
      #
      # |@key member parsekit.parselet
      #
      # |@block
      # Captures the result of matching *member*. We use `%let` to "inherit"
      # the syntax `_←_`.
      matchpi %{[%'%let name_ subterm_]} do
        Capture.new(name, parselet(subterm, observed: true))
      end

      # |@ parsekit.parselet.loc
      #
      # |@pattern
      # [loc name_ member_]
      #
      # |@key name
      # The name of the capture.
      #
      # |@key member parsekit.parselet
      #
      # |@block
      # Captures the location info for *member* under name. The location info is
      # a dict containing the following entries.
      #
      # | Key            | Description                                                             |
      # | -------------- | ----------------------------------------------------------------------- |
      # | `line-begin`   | (TODO) The line where the match for *member* starts (zero-based).       |
      # | `line-end`     | (TODO) The line where the match for *member* ends (zero-based).         |
      # | `column-begin` | (TODO) The column where the match for *member* starts (zero-based).     |
      # | `column-end`   | (TODO) The column where the match for *member* ends (zero-based).       |
      # | `rune-begin`   | The index of the rune where the match for *member* starts (zero-based). |
      # | `rune-end`     | The index of the rune where the match for *member* ends (zero-based).   |
      # | `byte-begin`   | The index of the byte where the match for *member* starts (zero-based). |
      # | `byte-end`     | The index of the byte where the match for *member* ends (zero-based).   |
      matchpi %{[loc name_ subterm_]} do
        Location.new(name, parselet(subterm, observed))
      end

      # |@ parsekit.parselet.find
      #
      # |@pattern
      # [find member_]
      #
      # |@key member parsekit.parselet
      #
      # |@block
      # Searches for *member* in the remainder of the string. That is, the remainder
      # of the string should not necessarily start with *member*; some characters can
      # be skipped.
      matchpi %{[find subterm_]} do
        Find.new(parselet(subterm, observed))
      end

      # |@ parsekit.parselet.form
      #
      # |@pattern
      # [form member_ spec_]
      #
      # |@key member parsekit.parselet
      # The parselet to use to capture some characters for *spec*. This can be as
      # loose or strict as necessary; with `form`, it is *spec* deciding whether input
      # is valid, not *member*.
      #
      # |@key spec parsekit.formspec
      # The spec for `form` to use.
      #
      # |@block
      # Interprets the text matched by *member* according to *spec*. The result of
      # this parselet is the result of the interpretation. Captures made in *member*
      # are passed through.
      #
      # NOTE: *spec* failing to process the text matched by *member* is a hard error,
      # meaning the entire parse is aborted with an error pointing to the appropriate
      # location in the text. Use `parsekit.formspec.default` to return a fallback
      # term instead.
      matchpi %{[form subterm_ specQ_]} do
        continue unless spec = form_spec?(specQ)

        Form.new(parselet(subterm, observed: false), spec)
      end

      otherwise do
        Reject.new
      end
    end
  end

  private def form_spec?(term : Term) : FormSpec?
    Term.case(term) do
      # |@ parsekit.formspec.nat
      #
      # |@pattern
      # nat
      #
      # |@block
      # A shorthand for `(nat radix: 10)`.
      matchpi %{nat} do
        NatForm.new(radix: 10u32)
      end

      # |@ parsekit.formspec.nat
      #
      # |@pattern
      # (nat ¦ radix_: (%number 1 <= (whole _) <= 62))
      #
      # |@key radix
      # The radix to use.
      #
      # |@block
      # Accepts a natural number expressed in the given *radix*. The result is
      # a number term.
      #
      # - The number can consist of digits of the given *radix* (see *radix* for info).
      # - The number can contain underscores.
      # - The number must not start with an underscore.
      # - The number must not end with an underscore.
      matchpi %{(nat ¦ radix_: (%number 1 <= (whole _) <= 62))} do
        NatForm.new(radix.to(UInt32))
      end

      # |@ parsekit.formspec.default
      #
      # |@pattern
      # (default member_ fallback_)
      #
      # |@key member parsekit.formspec
      # The spec whose errors should be intercepted.
      #
      # |@key fallback
      # The fallback term.
      #
      # |@block
      # Intercepts errors from *member* and replaces them with the given *fallback*
      # term. This prevents parsing from aborting the parse if *member* is unsatisfied
      # with the underlying text.
      matchpi %{[default subterm_ fallback_]} do
        continue unless member = form_spec?(subterm)

        DefaultForm.new(member, fallback)
      end

      otherwise { }
    end
  end

  alias Production = RuleProduction | AliasProduction

  defcase RuleProduction, parselet : Parselet, template : Alloy::CompiledTemplate
  defcase AliasProduction, parselet : Parselet

  private def nullable?(productions, path, parselet : Reject) : Bool
    false
  end

  private def nullable?(productions, path, parselet : Stringp) : Bool
    ScanKit.nullable?(parselet.pattern)
  end

  private def nullable?(productions, path, parselet : RuleRef) : Bool
    unless overloads = productions[parselet.name]?
      return false # Production not found is a Reject, which is not nullable.
    end

    nullable?(productions, path, overloads)
  end

  private def nullable?(productions, path, parselet : OrdChoice | MaxChoice) : Bool
    parselet.members.any? { |member| nullable?(productions, path, member) }
  end

  private def nullable?(productions, path, parselet : Seq) : Bool
    # For example, `(seq "" "x")` as a whole is not nullable even
    # though the first member is. On the other hand `(seq "" "[%d]?")`
    # is nullable because all its members are nullable.
    parselet.members.all? { |member| nullable?(productions, path, member) }
  end

  private def nullable?(productions, path, parselet : Many) : Bool
    parselet.min.zero? || nullable?(productions, path, parselet.member)
  end

  private def nullable?(productions, path, parselet : ManySep) : Bool
    parselet.min.zero? || (nullable?(productions, path, parselet.member) && nullable?(productions, path, parselet.sep))
  end

  private def nullable?(productions, path, parselet : Capture | Location | Find | Form) : Bool
    nullable?(productions, path, parselet.member)
  end

  private def nullable?(productions, path, production : Production) : Bool
    unless path.add?(production)
      return false # Do not follow cycles. Assume cycles are not nullable.
    end

    begin
      nullable?(productions, path, production.parselet)
    ensure
      path.delete(production)
    end
  end

  private def nullable?(productions, path, overloads : Array(Production)) : Bool
    overloads.any? { |overload| nullable?(productions, path, overload) }
  end

  # Returns `true` if *object* can match the empty string.
  private def nullable?(productions, object : Parselet | Production | Array(Production)) : Bool
    path = Set(Production).new
    path.compare_by_identity
    nullable?(productions, path, object)
  end

  private def left_recursive?(productions, path, pivot : Term::Sym, parselet : Reject) : Bool
    false
  end

  private def left_recursive?(productions, path, pivot : Term::Sym, parselet : Stringp) : Bool
    false
  end

  private def left_recursive?(productions, path, pivot : Term::Sym, parselet : RuleRef) : Bool
    if pivot == parselet.name
      return true
    end

    unless overloads = productions[parselet.name]?
      return false # Production not found is a Reject, which is not left-recursive.
    end

    left_recursive?(productions, path, pivot, overloads)
  end

  private def left_recursive?(productions, path, pivot : Term::Sym, parselet : OrdChoice | MaxChoice) : Bool
    parselet.members.any? do |member|
      left_recursive?(productions, path, pivot, member)
    end
  end

  private def left_recursive?(productions, path, pivot : Term::Sym, parselet : Seq) : Bool
    return false unless head = parselet.members.first?

    parselet.members.each do |member|
      if left_recursive?(productions, path, pivot, member)
        return true
      end

      # If a non-nullable member is in the way, the rule is not left recursive.
      #
      # For example:
      #   (x "-" x)
      #
      # This rule is not left-recursive because `"-"` is not left-recursive, and
      # it is also not nullable. On the other hand, consider:
      #
      #   (x "[-]?" x)
      #
      # Here, `"[-]?"` is nullable; therefore, even though it is not left-recursive,
      # there is a chance it might be skipped; therefore, we must continue searching.
      # Then we find `x` and decide the rule is left-recursive, which is indeed
      # the case here.
      break unless nullable?(productions, path, member)
    end

    false
  end

  # For Many and ManySep, we're conservative: even if there's a min: 0 in the way,
  # we'll still consider the rule left-recursive.
  private def left_recursive?(productions, path, pivot : Term::Sym, parselet : Many | ManySep | Capture | Location | Find | Form) : Bool
    left_recursive?(productions, path, pivot, parselet.member)
  end

  private def left_recursive?(productions, path, pivot : Term::Sym, production : Production) : Bool
    unless path.add?(production)
      return false # Do not follow cycles.
    end

    begin
      left_recursive?(productions, path, pivot, production.parselet)
    ensure
      path.delete(production)
    end
  end

  private def left_recursive?(productions, path, pivot : Term::Sym, overloads : Array(Production)) : Bool
    overloads.any? do |overload|
      left_recursive?(productions, path, pivot, overload)
    end
  end

  # Returns `true` if any *overload* of *pivot* is left-recursive.
  private def left_recursive?(productions, pivot : Term::Sym, overloads : Array(Production)) : Bool
    path = Set(Production).new
    path.compare_by_identity
    left_recursive?(productions, path, pivot, overloads)
  end

  # TODO: Intern so that it becomes
  #   productions : Slice(Production)
  #   left_recursive : Pf::USet32
  defrecord Grammar,
    productions : Hash(Term::Sym, Array(Production)),
    left_recursive : Set(Term::Sym)

  # flatten overload buckets to obtain productions slice
  # construct ranges hash (sym => range)
  # productions = productions.map |overload|
  #   intern(ranges hash, overload)
  # productions

  def grammar(ruleset : Term) : Grammar
    unless ruleset = ruleset.as_d?
      return Grammar.new(({} of Term::Sym => Array(Production)), Set(Term::Sym).new)
    end

    productions = {} of Term::Sym => Array(Production)

    ruleset.items.each do |item|
      Term.case(item) do
        # |@ parsekit.grammar.rule
        #
        # |@pattern
        # [rule (name_symbol parselet_) template_]
        #
        # |@key name
        # The name of the rule.
        #
        # |@key parselet parsekit.parselet
        # The parselet to match. Captures made by this parselet will be available
        # in *template*.
        #
        # |@key template alloy
        # The template to expand. Captures made by *parselet* are available.
        #
        # |@block
        # Defines a grammar rule. Captures made by *parselet* are made available to
        # *template*. The result of template its the result of the rule references
        # to the rule.
        matchpiT %{[rule (name_symbol parseletQ_) templateQ_]} do
          production = RuleProduction.new(parselet(parseletQ, observed: false), template: Alloy.compile(templateQ))
          overloads = productions.put_if_absent(name) { [] of Production }
          overloads << production
        end

        # |@ parsekit.grammar.rule
        #
        # |@pattern
        # [rule (name_symbol (%plural parselets min: 2)) template_]
        #
        # |@key name
        # The name of the rule.
        #
        # |@key parselets parsekit.parselet
        # A sequence of parselets.
        #
        # |@key template alloy
        # The template to expand. Captures made by *parselets* are available.
        #
        # |@block
        # A shorthand for defining a rule with a sequence of *parselets*. That is,
        # `(A x y z) => t` is the same as `(A (seq x y z)) => t`.
        matchpiT %{[rule pattern←(name_symbol _ _ _*) templateQ_]} do
          subterms = pattern.items.move(1)
          members = subterms.to_readonly_slice { |parseletQ| parselet(parseletQ, observed: false) }
          parselet = Seq.new(members, observed: false)
          production = RuleProduction.new(parselet, template: Alloy.compile(templateQ))
          overloads = productions.put_if_absent(name) { [] of Production }
          overloads << production
        end

        # |@ parsekit.grammar.alias
        #
        # |@pattern
        # [name_symbol parselet_]
        #
        # |@key name
        # The name of the rule.
        #
        # |@key parselet parsekit.parselet
        #
        # |@block
        # Gives a name to a parselet without providing a template.
        matchpiT %{[name_symbol parseletQ_]} do
          production = AliasProduction.new(parselet(parseletQ, observed: false))
          overloads = productions.put_if_absent(name) { [] of Production }
          overloads << production
        end

        # |@ parsekit.grammar.alias
        #
        # |@pattern
        # [name_symbol (%plural parselets min: 2)]
        #
        # |@key name
        # The name of the rule.
        #
        # |@key parselets parsekit.parselet
        # The sequence of parselets.
        #
        # |@block
        # Gives a name to a sequence of *parselets* without providing a template.
        matchpiT %{[name_symbol _ _ _*]} do
          subterms = item.items.move(1)
          members = subterms.to_readonly_slice { |parseletQ| parselet(parseletQ, observed: false) }
          parselet = Seq.new(members, observed: false)
          production = AliasProduction.new(parselet)
          overloads = productions.put_if_absent(name) { [] of Production }
          overloads << production
        end

        otherwise { }
      end
    end

    left_recursive = Set(Term::Sym).new

    productions.each do |name, production|
      next unless left_recursive?(productions, name, production)

      left_recursive << name
    end

    Grammar.new(productions, left_recursive)
  end

  defrecord Err, detail : String, text : Pf::StringSeln
  defrecord Refusal

  alias FormSpec = NatForm | DefaultForm

  defrecord NatForm, radix : UInt32 do
    assert 1 <= radix <= 62
  end

  defcase DefaultForm, member : FormSpec, fallback : Term

  def digit?(spec : NatForm, text : Pf::StringSeln) : Term::Num?
    return unless text.size == 1

    ML::Kit.chr2nat?(text.chr, Term[spec.radix])
  end

  def eval(spec : NatForm, text : Pf::StringSeln) : Term | Err
    n = Term[0]

    # Validate
    text.each_split do |l, m, r|
      if d = digit?(spec, m)
        # Append digit.
        n = n * Term[spec.radix] + d
        next
      end

      if m == '_'
        if l.empty?
          return Err.new("leading underscores not allowed in number", m)
        end

        if r.empty?
          return Err.new("trailing underscores not allowed in number", m)
        end

        if r.starts_with?('_')
          return Err.new("multiple consecutive underscores not allowed in number", m)
        end

        # Skip underscores.
        next
      end

      return Err.new("unexpected characters found in number", m &+ r)
    end

    Term.of(n)
  end

  def eval(spec : DefaultForm, text : Pf::StringSeln) : Term
    case result = eval(spec.member, text)
    in Term
      result
    in Err
      spec.fallback
    end
  end

  struct MemoKey
    # :nodoc:
    def initialize(@ref : UInt64, @byte : UInt64)
    end

    def self.new(head : Term::Sym, text : Pf::StringSeln)
      new(head.@bits, text.byte_start.to_u64)
    end

    def hash(hasher)
      raise "MemoKey#hash(hasher) must not be called"
    end

    def hash : UInt64
      Int.mix(@ref, @byte)
    end

    def inspect(io)
      io << "("
      @ref.to_s(io, base: 32)
      io << "|"
      io << @byte
      io << ")"
    end
  end

  # :nodoc:
  defcase Context,
    grammar : Grammar,
    memo : Pf::Map(MemoKey, Parseout),
    checkpoint : (UInt64 ->),
    clock : UInt64

  class Context
    setter memo

    def tick : Nil
      @checkpoint.call(@clock)
      @clock += 1
    end
  end

  def context(grammar : Grammar, checkpoint : UInt64 ->)
    memo = Pf::Map(MemoKey, Parseout).new
    Context.new(grammar, memo, checkpoint, clock: 0u64)
  end

  def context(grammar : Grammar)
    context(grammar, ->(clock : UInt64) { })
  end

  alias Parseout = Ok | Err | Refusal

  defrecord Ok,
    match : Pf::StringSeln,
    captures : CaptureLog,
    result : Result,
    ahead : Pf::StringSeln

  # Instead of using a `Pf::Map` or some other map for capture entries we simply
  # use a log, which is fast enough for the common case. Deduplication occurs
  # on replay.
  #
  # Note also that since a log can only support adding and updating entries
  # (but not removing them), checks such as `CaptureLog#empty?` remain valid
  # and well-defined.
  alias CaptureLog = Slice(CaptureEntry)

  defrecord CaptureEntry, name : Term, value : Result

  alias Result = Term | Thunk | CaptureLog | MatchList

  defrecord Thunk, template : Alloy::CompiledTemplate, captures : CaptureLog
  defrecord MatchList, items : Slice(MatchListItem)

  alias MatchListItem = Result

  def resolve(object : Term) : Term
    object
  end

  def resolve(object : MatchList) : Term
    result = Term::Dict.build do |commit|
      object.items.each do |item|
        commit << resolve(item)
      end
    end

    Term.of(result)
  end

  def resolve(object : CaptureLog) : Term
    Term.of(resolve_log(object))
  end

  def resolve_log(log : CaptureLog) : Term::Dict
    Term::Dict.build do |commit|
      log.each do |entry|
        commit.with(entry.name, resolve(entry.value))
      end
    end
  end

  def resolve(object : Thunk) : Term
    Alloy.render(object.template, locals: resolve_log(object.captures))
  end

  def resolve(object : Ok) : Term | Err | Refusal
    unless object.ahead.empty?
      return Err.new("expected end-of-input", object.ahead)
    end

    resolve(object.result)
  end

  def resolve(object : Err | Refusal) : Term | Err | Refusal
    object
  end

  def parse(ctx : Context, parselet : Reject, text : Pf::StringSeln) : Parseout
    Refusal.new
  end

  def parse(ctx : Context, parselet : Stringp, text : Pf::StringSeln) : Parseout
    unless row = ScanKit.match?(parselet.pattern, text)
      return Refusal.new
    end

    captures, ahead = row
    match = text.upto(ahead)

    if parselet.observed
      result = Term.of(match)
    else
      result = Term.of("")
    end

    capture_list = captures.ee.to_readonly_slice do |(name, value)|
      CaptureEntry.new(name, value)
    end

    Ok.new(match, capture_list, result, ahead)
  end

  def parse(ctx : Context, parselet : RuleRef, text : Pf::StringSeln) : Parseout
    parse(ctx, parselet.name, text)
  end

  def parse(ctx : Context, parselet : OrdChoice, text : Pf::StringSeln) : Parseout
    parselet.members.each do |branch|
      π = parse(ctx, branch, text)
      next if π.is_a?(Refusal)
      return π
    end

    Refusal.new
  end

  def parse(ctx : Context, parselet : MaxChoice, text : Pf::StringSeln) : Parseout
    candidates = Pf::Kit.stack_array(Ok, 4)

    parselet.members.each do |branch|
      case π = parse(ctx, branch, text)
      in Ok
        candidates << π
      in Err
        return π
      in Refusal
      end
    end

    candidates.max_by?(&.match.bytesize) || Refusal.new
  end

  def parse(ctx : Context, parselet : Seq, text : Pf::StringSeln) : Parseout
    start = text

    captures = Pf::Kit.stack_array(CaptureEntry, 8)
    items = Pf::Kit.stack_array(MatchListItem, 8)

    parselet.members.each do |member|
      π = parse(ctx, member, text)
      unless π.is_a?(Ok)
        return π
      end

      if parselet.observed
        if π.captures.empty?
          items << π.result
        else
          items << π.captures
        end
      end

      captures.concat(π.captures)
      text = π.ahead
    end

    Ok.new(start.upto(text), captures.to_unsafe_readonly_slice!, MatchList.new(items.to_unsafe_readonly_slice!), text)
  end

  def parse(ctx : Context, parselet : Many, text : Pf::StringSeln) : Parseout
    assert parselet.min <= parselet.max

    start = text
    items = Pf::Kit.stack_array(MatchListItem, 8)

    # Match the required part.
    parselet.min.times do
      π = parse(ctx, parselet.member, text)
      unless π.is_a?(Ok)
        return π
      end

      if parselet.observed
        if π.captures.empty?
          items << π.result
        else
          items << π.captures
        end
      end

      text = π.ahead
    end

    # Match the optional part.
    (parselet.max - parselet.min).times do
      case π = parse(ctx, parselet.member, text)
      in Ok
        if parselet.observed
          if π.captures.empty?
            items << π.result
          else
            items << π.captures
          end
        end

        text = π.ahead
      in Err
        return π
      in Refusal
        break
      end
    end

    Ok.new(start.upto(text), CaptureLog.empty, MatchList.new(items.to_unsafe_readonly_slice!), text)
  end

  def parse(ctx : Context, parselet : ManySep, text : Pf::StringSeln) : Parseout
    assert parselet.min <= parselet.max

    start = text
    items = Pf::Kit.stack_array(MatchListItem, 8)

    # Match the required part.
    parselet.min.times do |index|
      if index > 0 # Match separator
        π = parse(ctx, parselet.sep, text)
        unless π.is_a?(Ok)
          return π
        end

        if parselet.observed && !π.captures.empty?
          items << π.captures
        end

        text = π.ahead
      end

      π = parse(ctx, parselet.member, text)
      unless π.is_a?(Ok)
        return π
      end

      if parselet.observed
        if π.captures.empty?
          items << π.result
        else
          items << π.captures
        end
      end

      text = π.ahead
    end

    # Match the optional part.
    (parselet.max - parselet.min).times do |index|
      # Match separator.
      if parselet.min > 0 || index > 0
        π = parse(ctx, parselet.sep, text)
        case π
        in Ok
          if parselet.observed && !π.captures.empty?
            items << π.captures
          end
          text = π.ahead
        in Err
          return π
        in Refusal
          break
        end
      end

      case π = parse(ctx, parselet.member, text)
      in Ok
        if parselet.observed
          if π.captures.empty?
            items << π.result
          else
            items << π.captures
          end
        end

        text = π.ahead
      in Err
        return π
      in Refusal
        break
      end
    end

    # Match optional trailing separator.
    if items.present? && parselet.trailing
      π = parse(ctx, parselet.sep, text)
      case π
      in Ok
        if parselet.observed && !π.captures.empty?
          items << π.captures
        end
        text = π.ahead
      in Err
        return π
      in Refusal
      end
    end

    Ok.new(start.upto(text), CaptureLog.empty, MatchList.new(items.to_unsafe_readonly_slice!), text)
  end

  def parse(ctx : Context, parselet : Capture, text : Pf::StringSeln) : Parseout
    π = parse(ctx, parselet.member, text)
    unless π.is_a?(Ok)
      return π
    end

    capture = CaptureEntry.new(parselet.name, π.result)
    captures = π.captures.append(capture)
    Ok.new(π.match, captures, π.result, π.ahead)
  end

  def parse(ctx : Context, parselet : Location, text : Pf::StringSeln) : Parseout
    π = parse(ctx, parselet.member, text)
    unless π.is_a?(Ok)
      return π
    end

    match = π.match

    report = Term::Dict.build do |commit|
      # commit.with(:"line-begin", _)
      # commit.with(:"line-end", _)

      # commit.with(:"column-begin", _)
      # commit.with(:"column-end", _)

      commit.with(:"rune-begin", match.char_start)
      commit.with(:"rune-end", match.char_end)

      commit.with(:"byte-begin", match.byte_start)
      commit.with(:"byte-end", match.byte_end)
    end

    capture = CaptureEntry.new(parselet.name, Term.of(report))
    Ok.new(match, π.captures.append(capture), π.result, π.ahead)
  end

  def parse(ctx : Context, parselet : Find, text : Pf::StringSeln) : Parseout
    text.each_before_and_after do |_, after|
      case π = parse(ctx, parselet.member, after)
      in Ok, Err then return π
      in Refusal
      end
    end

    Refusal.new
  end

  def parse(ctx : Context, parselet : Form, text : Pf::StringSeln) : Parseout
    case π = parse(ctx, parselet.member, text)
    in Err, Refusal
      return π
    in Ok
    end

    case result = eval(parselet.spec, π.match)
    in Term
      Ok.new(π.match, π.captures, result, π.ahead)
    in Err
      result
    end
  end

  def parse(ctx : Context, production : RuleProduction, text : Pf::StringSeln) : Parseout
    π = parse(ctx, production.parselet, text)
    unless π.is_a?(Ok)
      return π
    end

    result = Thunk.new(production.template, π.captures)
    Ok.new(π.match, CaptureLog.empty, result, π.ahead)
  end

  def parse(ctx : Context, production : AliasProduction, text : Pf::StringSeln) : Parseout
    π = parse(ctx, production.parselet, text)
    unless π.is_a?(Ok)
      return π
    end

    if π.captures.empty?
      Ok.new(π.match, CaptureLog.empty, π.result, π.ahead)
    else
      Ok.new(π.match, CaptureLog.empty, π.captures, π.ahead)
    end
  end

  def parse(ctx : Context, overloads : Array(Production), text : Pf::StringSeln) : Parseout
    overloads.each do |overload|
      π = parse(ctx, overload, text)

      case π
      in Ok, Err then return π
      in Refusal
      end
    end

    Refusal.new
  end

  def parse(ctx : Context, ref : Term::Sym, text : Pf::StringSeln) : Parseout
    key = MemoKey.new(ref, text)
    if π = ctx.memo[key]?
      return π
    end

    unless overloads = ctx.grammar.productions[ref]?
      return Refusal.new
    end

    ctx.tick

    # Fast path for rules that are not left-recursive.
    unless ref.in?(ctx.grammar.left_recursive)
      return parse(ctx, overloads, text)
    end

    zero_memo = ctx.memo.assoc(key, Refusal.new)
    best_memo = zero_memo
    best_out : Ok? = nil

    loop do
      ctx.memo = zero_memo

      π = parse(ctx, overloads, text)
      case π
      in Ok
        break if best_out && best_out.ahead.byte_start >= π.ahead.byte_start
      in Refusal, Err
        break # Longest match or failure
      end

      best_out = π
      best_memo = ctx.memo.assoc(key, best_out)
      zero_memo = zero_memo.assoc(key, best_out)
    end

    ctx.memo = best_memo

    best_out || Refusal.new
  end
end
