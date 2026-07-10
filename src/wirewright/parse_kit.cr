# A tiny string parsing language. Used mainly by `Rack::Parser`.
#
# As far as I understand, what I implemented here is basically PEG.
#
# NOTE: It's quite hard to deal with left-recursion in PEGs so I'm just
# returning `Refusal` on left recursion. Maybe there's a way to fix this,
# because left recursion is sometimes useful.
#
# See `parsekit.parselet` for docs on parselets. See `parsekit.grammar` for docs on
# the way we define grammars.
module Ww::ParseKit
  extend self

  # |@ parsekit.parselet
  #
  # |@block
  # We call parser operators / combinators *parselets* for simplicity; although
  # the word doesn't mean exactly what we use it for here. It seems generic enough
  # to work, though.

  alias Parselet = Reject | Stringp | RuleRef | OrdChoice | MaxChoice |
                   Seq | Many | Capture | Location | Find | Form

  defrecord Reject
  defrecord Stringp, pattern : ScanKit::Pattern, observed : Bool
  defrecord RuleRef, name : Term::Sym
  defrecord OrdChoice, members : Slice(Parselet)
  defrecord MaxChoice, members : Slice(Parselet)
  defrecord Seq, members : Slice(Parselet), observed : Bool
  defcase Many, min : UInt32, max : UInt32, member : Parselet, observed : Bool
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
      # You can negate a character set by writing `^` immediately after the opening
      # bracket. For example, `[^a-zA-Z]` matches all characters that are *not* in `[a-zA-Z]`.
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
      # Whitespace around the comma in `,` `{}` quantifiers is ignored.
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
      # ## Matching multiple atoms simultaneously
      #
      # Use `~` to match multiple atoms simultaneously (i.e., at the same position):
      # `[0-9]~[^1-3]` means "match digits excluding 1-3", whereas `[0-9]~[a-z]` is
      # the same as `[0-9a-z]`.
      #
      # More `~`s can be chained: `[0-9]~[a-z]~[^a-f]` is the same as `[0-9a-z]~[^a-f]`,
      # meaning "match digits and ASCII lowercase, but exclude letters a-f".
      #
      # For `~` members, all start at the same position in the text, but only
      # the longest wins.
      #
      # ## Literal matching
      #
      # The whitespace character ` ` receives special treatment outside of character
      # sets. It is a shorthand for `[%s]+`. So for example, `%|dd| %|dd| %|dddd|`
      # "desugars" into the vastly less readable `%d%d[%s]+%d%d[%s]+%d%d%d%d`. To
      # match a literal whitespace character, wrap it in a character set: `[ ]`.
      #
      # Characters that receive no special treatment are handled literally. Therefore,
      # a pattern such as `abc` matches the characters `a`, `b`, `c` literally.
      #
      # ### Escaping special characters
      #
      # Most special characters can be escaped using the character set notation `[...]`.
      # Here is how you can escape special characters.
      #
      # | Character                      | How to escape                                                                                                                                                                                                                                                     |
      # | ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
      # | `[`                            | `%[` (we have to have this as a special case because character sets cannot include bare brackets `[]`).                                                                                                                                                           |
      # | `]`                            | `%]` (we have to have this as a special case because character sets cannot include bare brackets `[]`).                                                                                                                                                           |
      # | `\|`                           | `[\|]` (where necessary, normally you can just use `\|`).                                                                                                                                                                                                         |
      # | `(`, `)`, `%`, `←`, and others | `[(]`, `[)]`, `[%]`, `[←]`, etc.                                                                                                                                                                                                                                  |
      # | ` ` (whitespace)               | `[ ]`                                                                                                                                                                                                                                                             |
      # | `^` in character sets          | If you have a nonempty character set, put `^` in a nonfirst position (e.g., `[a-z^]`, or `[a-z^%]` here also escaping `%`). If you want to match simply `^`, `[^]` won't work (it's an empty negated set, whose meaning is the same as `%_`); just use bare  `^`. |
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
      # (any members_*)
      #
      # |@key members parsekit.parselet
      #
      # |@block
      # Sequential or: tries to match each of *members* in turn.
      matchpi %{(any  _*)} do
        subterms = term.items.move(1)
        members = subterms.to_readonly_slice { |subterm| parselet(subterm, observed).as(Parselet) }
        OrdChoice.new(members)
      end

      # |@ parsekit.parselet.max
      #
      # |@pattern
      # (max members_*)
      #
      # |@key members parsekit.parselet
      #
      # |@block
      # Parallel or: tries all of *members* simultaneously, and picks the member
      # whose match is the longest.
      matchpi %{(max _*)} do
        subterms = term.items.move(1)
        members = subterms.to_readonly_slice { |subterm| parselet(subterm, observed).as(Parselet) }
        MaxChoice.new(members)
      end

      # |@ parsekit.parselet.seq
      #
      # |@pattern
      # (seq members_*)
      #
      # |@key members parsekit.parselet
      #
      # |@block
      # Matches a sequence of *members*.
      matchpi %{(seq _*)} do
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

      # |@ parsekit.parselet.maybe
      #
      # |@pattern
      # (maybe member_)
      #
      # |@key member parsekit.parselet
      #
      # |@block
      # Matches zero or one instances of *member*. Shorthand for `(many _ min: 0 max: 1)`.
      matchpi %{(maybe subterm_)} do
        Many.new(0u32, 1u32, parselet(subterm, observed), observed)
      end

      # |@ parsekit.parselet.capture
      #
      # |@pattern
      # (%'%let name_ member_)
      #
      # |@key name
      # The name of the capture.
      #
      # |@key member parsekit.parselet
      #
      # |@block
      # Captures the result of matching *member*. We use `%let` to "inherit"
      # the syntax `_←_`.
      matchpi %{(%'%let name_ subterm_)} do
        Capture.new(name, parselet(subterm, observed: true))
      end

      # |@ parsekit.parselet.loc
      #
      # |@pattern
      # (loc name_ member_)
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
      matchpi %{(loc name_ subterm_)} do
        Location.new(name, parselet(subterm, observed))
      end

      # |@ parsekit.parselet.find
      #
      # |@pattern
      # (find member_)
      #
      # |@key member parsekit.parselet
      #
      # |@block
      # Searches for *member* in the remainder of the string. That is, the remainder
      # of the string should not necessarily start with *member*; some characters can
      # be skipped.
      matchpi %{(find subterm_)} do
        Find.new(parselet(subterm, observed))
      end

      # |@ parsekit.parselet.form
      #
      # |@pattern
      # (form member_ spec_)
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
      matchpi %{(form subterm_ specQ_)} do
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
      # Accepts a natural number. The result is a number term.
      #
      # - The number can consist of digits `[0-9_]`.
      # - The number must not start with an underscore.
      # - The number must not end with an underscore.
      matchpi %{nat} do
        NatForm.new
      end

      matchpi %{(default subterm_ fallback_)} do
        continue unless member = form_spec?(subterm)

        DefaultForm.new(member, fallback)
      end

      otherwise { }
    end
  end

  alias Production = RuleProduction | AliasProduction

  defcase RuleProduction, parselet : Parselet, template : Alloy::CompiledTemplate
  defcase AliasProduction, parselet : Parselet

  defrecord Grammar, productions : Hash(Term::Sym, Array(Production))

  def grammar(ruleset : Term) : Grammar
    unless ruleset = ruleset.as_d?
      return Grammar.new({} of Term::Sym => Array(Production))
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
          bucket = productions.put_if_absent(name) { [] of Production }
          bucket << production
        end

        # |@ parsekit.grammar.rule
        #
        # |@pattern
        # [rule (name_symbol parselets_+) template_]
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
          bucket = productions.put_if_absent(name) { [] of Production }
          bucket << production
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
          bucket = productions.put_if_absent(name) { [] of Production }
          bucket << production
        end

        otherwise { }
      end
    end

    Grammar.new(productions)
  end

  defrecord Err, detail : String, text : Pf::StringSeln
  defrecord Refusal

  alias FormSpec = NatForm | DefaultForm

  defrecord NatForm
  defcase DefaultForm, member : FormSpec, fallback : Term

  def eval(spec : NatForm, text : Pf::StringSeln) : Term | Err
    value, _ = ML::Kit.nat(text, exact: true)
    Term.of(value)
  rescue e : ML::SyntaxError
    Err.new(e.detail, e.text)
  end

  def eval(spec : DefaultForm, text : Pf::StringSeln) : Term
    case result = eval(spec.member, text)
    in Term
      result
    in Err
      spec.fallback
    end
  end

  alias Memo = Hash(MemoKey, SkimOut)

  struct MemoKey
    # :nodoc:
    def initialize(@ref : UInt64, @byte : UInt64)
    end

    def self.new(production : Production, text : Pf::StringSeln)
      new(production.object_id, text.byte_start.to_u64)
    end

    def hash(hasher)
      raise "MemoKey#hash(hasher) must not be called"
    end

    def hash : UInt64
      Int.mix(@ref, @byte)
    end
  end

  # :nodoc:
  defcase Context,
    grammar : Grammar,
    oracle : Memo,
    path : Set(MemoKey),
    checkpoint : (UInt64 ->),
    clock : UInt64

  class Context
    def tick : Nil
      @checkpoint.call(@clock)
      @clock += 1
    end
  end

  def context(grammar : Grammar, checkpoint : UInt64 ->)
    oracle = {} of MemoKey => SkimOut
    path = Set(MemoKey).new
    Context.new(grammar, oracle, path, checkpoint, clock: 0u64)
  end

  def context(grammar : Grammar)
    context(grammar, ->(clock : UInt64) { })
  end

  alias SkimOut = Pf::StringSeln | Err | Refusal

  def skim(ctx : Context, parselet : Reject, text : Pf::StringSeln) : SkimOut
    Refusal.new
  end

  def skim(ctx : Context, parselet : Stringp, text : Pf::StringSeln) : SkimOut
    ScanKit.test?(parselet.pattern, text) || Refusal.new
  end

  def skim(ctx : Context, parselet : RuleRef, text : Pf::StringSeln) : SkimOut
    skim(ctx, parselet.name, text)
  end

  def skim(ctx : Context, parselet : OrdChoice, text : Pf::StringSeln) : SkimOut
    parselet.members.each do |branch|
      π = skim(ctx, branch, text)
      next if π.is_a?(Refusal)
      return π
    end

    Refusal.new
  end

  def skim(ctx : Context, parselet : MaxChoice, text : Pf::StringSeln) : SkimOut
    candidates = Pf::Kit.stack_array(Pf::StringSeln, 4)

    parselet.members.each do |branch|
      case π = skim(ctx, branch, text)
      in Pf::StringSeln
        candidates << π
      in Err
        return π
      in Refusal
      end
    end

    candidates.max_by?(&.bytesize) || Refusal.new
  end

  def skim(ctx : Context, parselet : Seq, text : Pf::StringSeln) : SkimOut
    parselet.members.each do |member|
      π = skim(ctx, member, text)
      unless π.is_a?(Pf::StringSeln)
        return π
      end

      text = π
    end

    text
  end

  def skim(ctx : Context, parselet : Many, text : Pf::StringSeln) : SkimOut
    assert parselet.min <= parselet.max

    # Match the required part.
    parselet.min.times do
      π = skim(ctx, parselet.member, text)
      unless π.is_a?(Pf::StringSeln)
        return π
      end

      text = π
    end

    # Match the optional part.
    (parselet.max - parselet.min).times do
      case π = skim(ctx, parselet.member, text)
      in Pf::StringSeln
        text = π
      in Err
        return π
      in Refusal
        break
      end
    end

    text
  end

  def skim(ctx : Context, parselet : Capture | Location | Form, text : Pf::StringSeln) : SkimOut
    skim(ctx, parselet.member, text)
  end

  def skim(ctx : Context, parselet : Find, text : Pf::StringSeln) : SkimOut
    text.each_before_and_after do |_, after|
      case π = skim(ctx, parselet.member, after)
      in Pf::StringSeln, Err then return π
      in Refusal
      end
    end

    Refusal.new
  end

  def skim(ctx : Context, production : RuleProduction | AliasProduction, text : Pf::StringSeln) : SkimOut
    key = MemoKey.new(production, text)
    if π = ctx.oracle[key]?
      return π
    end

    unless ctx.path.add?(key)
      # PEG parsers cannot easily handle left-recursion. Here we simply
      # refuse to parse.
      return Refusal.new
    end

    ctx.tick

    begin
      π = skim(ctx, production.parselet, text)
      ctx.oracle[key] = π
    ensure
      ctx.path.delete(key)
    end

    π
  end

  def skim(ctx : Context, bucket : Array(Production), text : Pf::StringSeln) : SkimOut
    bucket.each_with_index do |production, rank|
      π = skim(ctx, production, text)
      unless π.is_a?(Refusal)
        return π
      end
    end

    Refusal.new
  end

  def skim(ctx : Context, ref : Term::Sym, text : Pf::StringSeln) : SkimOut
    unless bucket = ctx.grammar.productions[ref]?
      return Refusal.new
    end

    skim(ctx, bucket, text)
  end

  alias Parseout = Ok | Err | Refusal

  defrecord Ok,
    match : Pf::StringSeln,
    captures : Term::Dict,
    result : Term,
    ahead : Pf::StringSeln

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

    Ok.new(match, captures, result, ahead)
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

    captures = Term[]
    match = Term::Dict.build do |commit|
      parselet.members.each do |member|
        π = parse(ctx, member, text)
        unless π.is_a?(Ok)
          return π
        end

        if parselet.observed
          if π.captures.empty?
            commit << π.result
          else
            commit << π.captures
          end
        end

        captures = Term.union(captures, π.captures)
        text = π.ahead
      end
    end

    Ok.new(start.upto(text), captures, Term.of(match), text)
  end

  def parse(ctx : Context, parselet : Many, text : Pf::StringSeln) : Parseout
    assert parselet.min <= parselet.max

    start = text

    match = Term::Dict.build do |commit|
      # Match the required part.
      parselet.min.times do
        π = parse(ctx, parselet.member, text)
        unless π.is_a?(Ok)
          return π
        end

        if parselet.observed
          if π.captures.empty?
            commit << π.result
          else
            commit << π.captures
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
              commit << π.result
            else
              commit << π.captures
            end
          end

          text = π.ahead
        in Err
          return π
        in Refusal
          break
        end
      end
    end

    Ok.new(start.upto(text), Term[], Term.of(match), text)
  end

  def parse(ctx : Context, parselet : Capture, text : Pf::StringSeln) : Parseout
    π = parse(ctx, parselet.member, text)
    unless π.is_a?(Ok)
      return π
    end

    captures = π.captures.with(parselet.name, π.result)
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

    Ok.new(match, π.captures.with(parselet.name, report), π.result, π.ahead)
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

  def parse(ctx : Context, key : MemoKey, production : RuleProduction, text : Pf::StringSeln) : Parseout
    unless ctx.path.add?(key)
      return Refusal.new
    end

    ctx.tick

    begin
      case π = parse(ctx, production.parselet, text)
      in Ok
        result = Alloy.render(production.template, locals: π.captures)
        Ok.new(π.match, Term[], result, π.ahead)
      in Err, Refusal
        π
      end
    ensure
      ctx.path.delete(key)
    end
  end

  def parse(ctx : Context, key : MemoKey, production : AliasProduction, text : Pf::StringSeln) : Parseout
    # It is possible to do weird stuff such as `(x x)` (alias `x` is `x`)
    # so we have to protect aliases as well.
    unless ctx.path.add?(key)
      return Refusal.new
    end

    begin
      parse(ctx, production.parselet, text)
    ensure
      ctx.path.delete(key)
    end
  end

  def parse(ctx : Context, bucket : Array(Production), text : Pf::StringSeln) : Parseout
    bucket.each_with_index do |production, rank|
      key = MemoKey.new(production, text)

      case cached = ctx.oracle[key]?
      in Pf::StringSeln, Nil # Known match or not visited
        case π = parse(ctx, key, production, text)
        in Ok, Err
          return π
        in Refusal
        end
      in Err # Known mismatch
        return cached
      in Refusal
      end
    end

    Refusal.new
  end

  def parse(ctx : Context, ref : Term::Sym, text : Pf::StringSeln) : Parseout
    if bucket = ctx.grammar.productions[ref]?
      parse(ctx, bucket, text)
    else
      Refusal.new
    end
  end
end
