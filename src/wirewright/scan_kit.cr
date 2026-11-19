# A tiny string pattern matching language. A companion of `ParseKit`.
module Ww::ScanKit
  extend self

  # |@ scankit
  #
  # |@summary
  # A small string pattern matching language designed as a companion to `parsekit`.
  #
  # |@block
  # The syntax of patterns is inspired by Lua string patterns, but only in a very minor way.
  # Reference: https://www.lua.org/pil/20.2.html.
  #
  # | Syntax                | Meaning                                   | Example       |
  # | --------------------- | ----------------------------------------- | ------------- |
  # | `a`                   | Literal character                         | `hello`       |
  # | `%x`                  | Character category                        | `%d`          |
  # | `%`                   | Literal percent sign                      | `%`           |
  # | `%\|…\|`              | Category/literal sequence                 | `%\|dd-dd\|`  |
  # | `[ ... ]`             | Character set                             | `[abc]`       |
  # | `[a-z]`               | Character range                           | `[a-z]`       |
  # | `[^ ... ]`            | Exclusion set                             | `[^0-9]`      |
  # | `[positive^negative]` | Inclusion with exclusions                 | `[a-z^aeiou]` |
  # | `[ ... ]?`            | Zero or one repetition                    | `[a-z]?`      |
  # | `[ ... ]+`            | One or more repetitions                   | `[a-z]+`      |
  # | `[ ... ]*`            | Zero or more repetitions                  | `[a-z]*`      |
  # | `[ ... ]{m,n}`        | Bounded repetition                        | `[0-9]{2,4}`  |
  # | `[ ... ]{m,}`         | Minimum repetition                        | `[0-9]{2,}`   |
  # | `(name←...)`          | Named capture                             | `(age←[%d]+)` |
  # | `(name)`              | Capture one arbitrary character           | `(char)`      |
  # | `(name*)`             | Capture zero or more arbitrary characters | `(rest*)`     |
  # | `…pattern`            | Unanchored left boundary                  | `…hello`      |
  # | `pattern…`            | Unanchored right boundary                 | `hello…`      |
  # | `…pattern…`           | Unanchored both boundaries                | `…hello…`     |
  # | *Whitespace*          | One or more whitespace characters         | `hello world` |
  #
  # |@example
  # ```
  # …(date←(day←%|dd|)/(month←%|dd|)/(year←%|dddd|))…
  # ```
  #
  # ... matches `"Deadline is 30/05/1999, firm"`.

  # |@ scankit.category
  #
  # |@summary
  # Character category.
  #
  # |@block
  # Character categories are introduced using `%` followed by a category qualifier.
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
  # An important distinction is that `%s` includes both horizontal and vertical
  # whitespace, whereas `%h` and `%v` match their respective subsets.
  #
  # |@example
  # ```
  # %x%x%x%x
  # ```

  # |@ scankit.category-sequence
  #
  # |@summary
  # Category sequence.
  #
  # |@block
  # Instead of writing e.g. `%d%d%d%d` you can use the category sequence notation:
  # `%|dddd|`. Characters that are not associated with a category are matched literally.
  #
  # |@example
  # ```
  # %|dd-dd-dddd|
  # ```
  #
  # ... represents a sequence equivalent to:
  #
  # ```
  # %d%d-%d%d-%d%d%d%d
  # ```
  #
  # This matches strings such as `25-01-2026`. `-` is matched literally because it
  # does not correspond to any category.

  # |@ scankit.charset
  #
  # |@summary
  # Character set.
  #
  # |@block
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
  # Character sets have an optional "negative" part which you can use to subtract some
  # characters from the set. The negative part starts with `^`. For example, the pattern
  # `[a-z^aeiou]` matches a character that is in range `a-z` that is not a vowel.
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
  # See also: `scankit.quantifier`
  #
  # |@example
  # ```
  # [abc]
  # [a-z]
  # [A-Z]
  # [a-zA-Z]
  # [0-9]
  # [%d]
  # [^0-9]
  # ```

  # |@ scankit.quantifier
  #
  # |@summary
  # Character set quantifier.
  #
  # |@block
  # Quantifiers apply to character sets and determine how many consecutive
  # matching characters are consumed.
  #
  # | Quantifier | Meaning                                                              |
  # | ---------- | -------------------------------------------------------------------- |
  # | `*`        | Zero or more times (shorthand for `{0,}`).                           |
  # | `+`        | One or more times (shorthand for `{1,}`).                            |
  # | `{n,}`     | *n* or more times. *n* is restricted to 1-4 digits (i.e., `0-9999`). |
  # | `{,n}`     | 0 up to *n* times. *n* is restricted to 1-4 digits.                  |
  # | `{m,n}`    | *m* up to *n* times. Both *m* and *n* are restricted to 1-4 digits.  |
  #
  # **Matching is greedy.** ScanKit consumes as many consecutive matching
  # characters as possible, up to the maximum, but no less than the minimum.
  #
  # Whitespace around the comma `,` in `{}` quantifiers is ignored.
  #
  # > [!NOTE]
  # > You cannot use quantifiers with categories: the pattern`%d*` does not mean
  # > what you probably think it does, and will instead match `%d` (a digit) followed
  # > by literal `*` (asterisk). Instead, you have to remember that only character
  # > sets accept quantifiers; so put the `%d` in a character set like so: `[%d]`;
  # > and then attach the quantifier: `[%d]*`. To reiterate, something like `%d*%l*`
  # > matches digit-asterisk-letter-asterisk, whereas `[%d]*[%l]*` matches zero
  # > or more digits followed by zero or more letters. And something like `%d*%d`
  # > matches digit-asterisk-digit.
  #
  # |@example
  # ```
  # [%d]+
  # ```
  #
  # ... matches one or more decimal digits.
  #
  # ```
  # [%d]{2,4}
  # ```
  #
  # ... matches between two and four decimal digits.
  #
  # ```
  # [%a]{3,}
  # ```
  #
  # ... matches at least three alphabetic characters.

  # |@ scankit.capture
  #
  # |@summary
  # Named capture.
  #
  # |@block
  # Named captures associate a matching substring with a symbolic name.
  #
  # There are several forms.
  #
  # | Syntax           | Description                                                               |
  # | ---------------- | ------------------------------------------------------------------------- |
  # | `(name←S0S1…Sn)` | Captures the substring matched by one or more scanners *S0* through *Sn*. |
  # | `(name*)`        | Captures zero or more arbitrary characters. The same as `(name←[%_]*)`    |
  # | `(name)`         | Captures exactly one arbitrary character. The same as `(name←%_)`         |
  #
  # Named captures also play the role of groups. Most often you need groups
  # for capturing, so ScanKit does not implement groups as a separate construct
  # (unlike regex where it's `(...)`).
  #
  # |@example
  # ```
  # (age←[%d]+)
  # ```
  #
  # Grouping:
  #
  # ```
  # (triple←[%d]+-[%d]+-[%d]+)
  # ```
  #
  # Here, under *triple*, we capture a *group*  (a concatenation) of three
  # scanners `[%d]+`, separated by a literal dash `-`. The equivalent regex
  # could be `(?<triple>\d+-\d+-\d+)`.

  # |@ scankit.anchor
  #
  # |@summary
  # Pattern anchoring.
  #
  # |@block
  # Patterns are anchored to both boundaries by default.
  #
  # For example, the pattern `hello` requires the entire input to match hello.
  #
  # The ellipsis character `…` disables anchoring at the corresponding boundary.
  #
  # | Pattern   | Left anchored | Right anchored |
  # | --------- | ------------- | -------------- |
  # | `hello`   | Yes           | Yes            |
  # | `…hello`  | No            | Yes            |
  # | `hello…`  | Yes           | No             |
  # | `…hello…` | No            | No             |
  #
  # When left anchoring is disabled, ScanKit searches successive input positions
  # until it finds a successful match.
  #
  # When right anchoring is disabled, a successful prefix match is sufficient,
  # even if additional input remains.
  #
  # When both boundaries are unanchored, the pattern searches for the first
  # successful occurrence.

  # |@ scankit.whitespace
  #
  # |@summary
  # Literal space in patterns.
  #
  # |@block
  # For convenience, a literal space in a pattern is not interpreted as a literal
  # space character. Instead, it is equivalent to `[%s]+`. Consequently,
  # a pattern such as `hello world` matches `hello` followed by one or more
  # whitespace characters, followed by `world`.
  #
  # Other whitespace characters (e.g. newline, tab) in the pattern are
  # treated as ordinary literal characters.
  #
  # In order to match just one whitespace, use the character set syntax: `[ ]`.

  # |@ scankit.escape
  #
  # |@summary
  # Ways to escape special characters.
  #
  # |@block
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

  alias Scanner = Char | Category | Charset | Concat | Capture

  @[Flags]
  enum Category
    BinaryDigit
    OctalDigit
    DecimalDigit
    HexDigit
    DncaseLetter
    UpcaseLetter
    Punctuation
    Control
    Hspace
    Vspace
    Space
    Word
    Wildcard
    Alphabetic
    Emoji
    Numeric
    Grapheme
  end

  defrecord CharChoice,
    chars = Slice(Char).empty,
    ranges = Slice(Range(Char, Char)).empty,
    categories = Category::None

  defrecord Charset,
    min : UInt32,
    max : UInt32,
    positive : CharChoice,
    negative : CharChoice

  defrecord Concat, members : Slice(Scanner)
  defcase Capture, name : Term::Sym, member : Scanner

  private def category?(qual : Char) : Char | Category | Nil
    case qual
    when '_' then Category::Wildcard
    when 'a' then Category::Alphabetic
    when 'b' then Category::BinaryDigit
    when 'c' then Category::Control
    when 'd' then Category::DecimalDigit
    when 'e' then Category::Emoji
    when 'g' then Category::Grapheme
    when 'h' then Category::Hspace
    when 'L' then Category::UpcaseLetter
    when 'l' then Category::DncaseLetter
    when 'n' then Category::Numeric
    when 'o' then Category::OctalDigit
    when 'p' then Category::Punctuation
    when 's' then Category::Space
    when 'v' then Category::Vspace
    when 'w' then Category::Word
    when 'x' then Category::HexDigit
    when '^' then '^'
    when '[', ']'
      qual
    end
  end

  private def category?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    return unless row = category_unit?(pattern)

    category, pattern = row
    {category.as(Scanner), pattern}
  end

  private def category_unit?(pattern : Pf::StringSeln) : {Char | Category, Pf::StringSeln}?
    return unless pattern.starts_with?('%')
    pattern = pattern.rest
    # %⏏x  %⏏

    qual, pattern = pattern.first_and_rest
    # %x⏏  %⏏

    if qual.empty?
      # %⏏
      return '%', pattern
    end

    return unless category = category?(qual.chr)

    {category, pattern}
  end

  private def category_seq?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    return unless pattern.starts_with?("%|")
    pattern = pattern.rest.rest
    # %|⏏Lllll|

    members = Pf::Kit.stack_array(Scanner)

    loop do
      if pattern.starts_with?('|')
        pattern = pattern.rest
        # %|Lllll|⏏
        break
      end

      qual, pattern = pattern.first_and_rest
      # %|⏏dd-dd-dddd|
      # %|d⏏d-dd-dddd|
      # %|dd⏏-dd-dddd|
      # ...

      if qual.empty?
        # %|Llll⏏
        return
      end

      qual = qual.chr

      if category = category?(qual)
        # %|dd-dd-d⏏ddd|
        members << category
      else
        # %|dd⏏-dd-dddd|
        members << qual
      end
    end

    if member = members.single?
      return member, pattern
    end

    scanner = Concat.new(members.to_unsafe_readonly_slice!)
    {scanner.as(Scanner), pattern}
  end

  alias CharUnit = Char | Category | Range(Char, Char)

  private def char_unit?(pattern : Pf::StringSeln) : {CharUnit, Pf::StringSeln}?
    # ⏏a  ⏏ab  ⏏a-z  ⏏%x
    if row = category_unit?(pattern)
      return row
    end

    return if pattern.empty?

    # ⏏a  ⏏ab  ⏏a-z
    head0, pattern0 = pattern.first_and_rest
    head1, pattern1 = pattern0.first_and_rest

    unless head1 == '-'
      # ⏏a  ⏏ab
      return head0.chr, pattern0 # a⏏  a⏏b
    end

    if pattern1.empty?
      # ⏏a-
      return head0.chr, pattern0 # a⏏-
    end

    pattern = pattern1
    # a-⏏z

    head2, pattern = pattern.first_and_rest
    # a-z⏏

    #  head1
    #  v
    # a-z < head2
    # ^
    # head0

    {(head0.chr..head2.chr), pattern}
  end

  # ⏏a-zA-Z%d -> a-zA-Z%d⏏^
  # ⏏a-zA-Z%d -> a-zA-Z%d⏏]
  private def char_choice?(pattern : Pf::StringSeln) : {CharChoice, Pf::StringSeln}?
    chars = Pf::Kit.stack_array(Char, 8)
    ranges = Pf::Kit.stack_array(Range(Char, Char), 4)
    categories = Category::None

    loop do
      break if pattern.starts_with?(']') || pattern.starts_with?('^')

      # [⏏%]
      if pattern.starts_with?("%]")
        pattern = pattern.rest
        # [%⏏]
        chars << '%'
        break
      end

      if row = char_unit?(pattern)
        # [a-z⏏
        unit, pattern = row
        # [a-z⏏A-Z%d]
        # [a-zA-Z⏏%d]
        # [a-zA-Z%d⏏]

        case unit
        in Char
          chars << unit
        in Range(Char, Char)
          ranges << unit
        in Category
          categories |= unit
        end

        next
      end

      return
    end

    choice = CharChoice.new(
      chars.to_unsafe_readonly_slice!,
      ranges.to_unsafe_readonly_slice!,
      categories,
    )

    {choice, pattern}
  end

  private def charset?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    # ⏏[a-zA-Z%d]
    return unless pattern.starts_with?('[')
    pattern = pattern.rest
    # [⏏a-zA-Z%d]

    return unless row = char_choice?(pattern)

    positive, pattern = row
    negative = CharChoice.new

    if pattern.starts_with?('^')
      pattern = pattern.rest
      # [a-z^⏏a-f]

      if row = char_choice?(pattern)
        negative, pattern = row
      end
    end

    return unless pattern.starts_with?(']')
    pattern = pattern.rest
    # [a-z^a-f⏏]

    min = 1u32
    max = 1u32
    if row = quantifier?(pattern)
      min, max, pattern = row
    end

    scanner = Charset.new(min, max, positive, negative)
    {scanner.as(Scanner), pattern}
  end

  private def quantifier?(pattern : Pf::StringSeln) : {UInt32, UInt32, Pf::StringSeln}?
    case pattern
    when .starts_with?('?')
      pattern = pattern.rest
      # [a-zA-Z%d]?⏏
      {0u32, 1u32, pattern}
    when .starts_with?('+')
      pattern = pattern.rest
      # [a-zA-Z%d]+⏏
      {1u32, UInt32::MAX, pattern}
    when .starts_with?('*')
      pattern = pattern.rest
      # [a-zA-Z%d]*⏏
      {0u32, UInt32::MAX, pattern}
    when .starts_with?('{')
      # [a-zA-Z%d]⏏{1,}  [a-zA-Z%d]⏏{1,3}
      pattern = pattern.rest
      # {⏏,3}  {⏏1,}  {⏏1,3}

      _, lo_digits, pattern = pattern.skip_thru_seq { |chr| chr.ascii_number? ? chr.to_u32 : nil }

      # {1⏏ , 2}
      pattern = pattern.lstrip(" ")
      # {1 ⏏, 2}

      return unless pattern.starts_with?(',') # {1⏏  {⏏
      pattern = pattern.rest
      # {,⏏3}  {1,⏏}  {1,⏏3}

      # {1,⏏ 2}
      pattern = pattern.lstrip(" ")
      # {1, ⏏2}

      _, hi_digits, pattern = pattern.skip_thru_seq { |chr| chr.ascii_number? ? chr.to_u32 : nil }
      # {,3⏏}  {1,⏏}  {1,3⏏}

      return unless pattern.starts_with?('}') # {1,⏏
      pattern = pattern.rest
      # {,3}⏏  {1,}⏏  {1,3}⏏

      # Sanity: 0-9999.
      return unless lo_digits.size <= 4
      return unless hi_digits.size <= 4

      lo = lo_digits.reduce(0u32) { |n, digit| n*10 + digit }
      if hi_digits.empty?
        # {1,}
        hi = UInt32::MAX
      else
        hi = hi_digits.reduce(0u32) { |n, digit| n*10 + digit }
      end

      # Sanity.
      return unless lo <= hi

      {lo, hi, pattern}
    else
      # [a-zA-Z%d]⏏
    end
  end

  private def atom?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    category?(pattern) || category_seq?(pattern) || charset?(pattern)
  end

  private def capture?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    return unless pattern.starts_with?('(')
    pattern = pattern.rest
    # (⏏age←[%d]+)

    name, pattern = pattern.skip_thru("a-zA-Z-")
    # (age⏏←[%d]+)

    case pattern
    when .starts_with?('←')
      pattern = pattern.rest
      # (age←⏏[%d]+)
      # (age←⏏%x%d)

      members = Pf::Kit.stack_array(Scanner)

      loop do
        if pattern.starts_with?(')')
          pattern = pattern.rest
          # (age←[%d]+)⏏
          # (age←%x%d)⏏
          break
        end

        return if pattern.empty? # (x←%d%d%d⏏

        member, pattern = scanner(pattern)
        members << member
        # (age←[%d]+⏏)
        #
        # (age←%x⏏%d)
        # (age←%x%d⏏)
      end

      if member = members.single?
        successor = member
      else
        successor = Concat.new(members.to_unsafe_readonly_slice!)
      end
    when .starts_with?(')')
      pattern = pattern.rest
      # (age)⏏

      successor = Charset.new(
        min: 1u32,
        max: 1u32,
        positive: CharChoice.new(categories: Category::Wildcard),
        negative: CharChoice.new,
      )
    when .starts_with?("*)")
      pattern = pattern.rest.rest
      # (rest*)⏏

      successor = Charset.new(
        min: 0u32,
        max: UInt32::MAX,
        positive: CharChoice.new(categories: Category::Wildcard),
        negative: CharChoice.new,
      )
    else
      return
    end

    capture = Capture.new(Term::Sym.new(name.to_s), successor)
    {capture.as(Scanner), pattern}
  end

  private def scanner(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}
    if row = atom?(pattern) || capture?(pattern)
      return row
    end

    assert !pattern.empty?

    head, pattern = pattern.first_and_rest
    chr = head.chr

    # Whitespace in the pattern is equivalent to `[%s]+`.
    if chr == ' '
      scanner = Charset.new(min: 1u32, max: UInt32::MAX,
        positive: CharChoice.new(categories: :space),
        negative: CharChoice.new,
      )
      return scanner.as(Scanner), pattern
    end

    # Match *chr* literally.
    {chr.as(Scanner), pattern}
  end

  defrecord Empty
  defrecord Pattern, scanner : Scanner | Empty, anchor_l : Bool, anchor_r : Bool

  def recognize(pattern : Pf::StringSeln, anchor_l : Bool, anchor_r : Bool) : Pattern
    if pattern.empty?
      return Pattern.new(Empty.new, anchor_l, anchor_r)
    end

    members = Pf::Kit.stack_array(Scanner)

    loop do
      scanner, pattern = scanner(pattern)
      members << scanner
      break if pattern.empty?
    end

    if member = members.single?
      top = member
    else
      top = Concat.new(members.to_unsafe_readonly_slice!)
    end

    Pattern.new(top, anchor_l, anchor_r)
  end

  def recognize(pattern : String, anchor_l : Bool, anchor_r : Bool) : Pattern
    recognize(Pf::StringSeln.new(pattern), anchor_l, anchor_r)
  end

  def recognize(pattern : Pf::StringSeln) : Pattern
    anchor_l = true
    if pattern.starts_with?('…')
      anchor_l = false
      pattern = pattern.rest
      # …hello -> hello
      # …hello… -> hello…
    end

    anchor_r = true
    if pattern.ends_with?('…')
      anchor_r = false
      pattern = pattern.prior
      # hello… -> hello
      # …hello… -> hello… -> hello
    end

    recognize(pattern, anchor_l, anchor_r)
  end

  def recognize(pattern : String) : Pattern
    recognize(Pf::StringSeln.new(pattern))
  end

  private def nullable?(scanner : Char | Category) : Bool
    false
  end

  private def nullable?(scanner : Charset) : Bool
    scanner.min.zero?
  end

  private def nullable?(scanner : Concat) : Bool
    scanner.members.all? { |member| nullable?(member) }
  end

  private def nullable?(scanner : Capture) : Bool
    nullable?(scanner.member)
  end

  private def nullable?(scanner : Empty) : Bool
    true
  end

  # Returns `true` if *pattern* can match the empty string.
  def nullable?(pattern : Pattern) : Bool
    nullable?(pattern.scanner)
  end

  alias Journal = CaptureJournal | NoJournal

  defrecord NoJournal
  defrecord CaptureJournal, entries : Pf::Kit::HybridArray(CaptureEntry, 16)

  defrecord CaptureEntry, name : Term::Sym, capture : Pf::StringSeln

  private def append(journal : CaptureJournal, name : Term::Sym, capture : Pf::StringSeln) : Nil
    journal.entries << CaptureEntry.new(name, capture)
  end

  private def checkpoint(journal : CaptureJournal) : Int32
    journal.entries.size
  end

  private def render(journal : CaptureJournal) : Term::Dict
    Term::Dict.build do |commit|
      journal.entries.each do |entry|
        commit.with(entry.name, entry.capture)
      end
    end
  end

  private def rollback(journal : CaptureJournal, checkpoint : Int32) : Nil
    assert 0 <= journal.entries.size >= checkpoint

    (journal.entries.size - checkpoint).times do
      _ = journal.entries.pop
    end
  end

  private def empty?(choice : CharChoice) : Bool
    choice.chars.empty? && choice.ranges.empty? && choice.categories.none?
  end

  private def member?(category : Category, text : Pf::StringSeln) : Bool
    return false if text.empty?

    if category.wildcard?
      return true
    end

    chr = text.first_char

    if member?(category, chr)
      return true
    end

    # TODO: Check if category starts_with?("P")
    # if head && category.punctuation? && chr.punctuation?
    #   return true
    # end

    # TODO: Use emoji-test.txt from unicode/
    # if category.emoji? && (text starts with emoji)?
    #   return true
    # end

    false
  end

  private def member?(category : Category, chr : Char) : Bool
    if category.wildcard?
      return true
    end

    if (category.space? || category.hspace?) && chr.hspace?
      return true
    end

    if (category.space? || category.vspace?) && chr.vspace?
      return true
    end

    if category.alphabetic? && chr.letter?
      return true
    end

    if category.numeric? && chr.number?
      return true
    end

    if category.word? && (chr.letter? || chr.number? || chr == '_')
      return true
    end

    if category.binary_digit? && chr.in?('0', '1')
      return true
    end

    if category.octal_digit? && chr.in?('0'..'7')
      return true
    end

    if category.decimal_digit? && chr.in?('0'..'9')
      return true
    end

    if category.hex_digit? && (chr.in?('0'..'9') || chr.in?('a'..'f') || chr.in?('A'..'F'))
      return true
    end

    if category.dncase_letter? && chr.lowercase?
      return true
    end

    if category.upcase_letter? && chr.uppercase?
      return true
    end

    if category.control? && chr.control?
      return true
    end

    false
  end

  private def member?(choice : CharChoice, text : Pf::StringSeln) : Bool
    return false if text.empty?

    chr = text.first_char

    if choice.chars.any? { |candidate| candidate == chr }
      return true
    end

    if choice.ranges.any? { |range| chr.in?(range) }
      return true
    end

    if member?(choice.categories, chr)
      return true
    end

    false
  end

  private def member?(charset : Charset, text : Pf::StringSeln) : Bool
    return false if text.empty?

    # If the positive side is empty as in the pattern [^a-z], only consider
    # the negative side. Otherwise, the positive side would be a nevermatch
    # that blocks all progress.
    if empty?(charset.positive)
      return !member?(charset.negative, text)
    end

    member?(charset.positive, text) && !member?(charset.negative, text)
  end

  def match?(scanner : Char, journal : Journal, text : Pf::StringSeln) : Pf::StringSeln?
    return unless text.starts_with?(scanner)

    text.rest
  end

  def match?(scanner : Category, journal : Journal, text : Pf::StringSeln) : Pf::StringSeln?
    # TODO:
    # if scanner.grapheme?
    #   return skip grapheme
    # end

    return unless member?(scanner, text)

    text.rest
  end

  def match?(scanner : Concat, journal : CaptureJournal, text : Pf::StringSeln) : Pf::StringSeln?
    checkpoint = checkpoint(journal)

    scanner.members.each do |member|
      unless text = match?(member, journal, text)
        rollback(journal, checkpoint)
        return
      end
    end

    text
  end

  def match?(scanner : Concat, journal : NoJournal, text : Pf::StringSeln) : Pf::StringSeln?
    scanner.members.each do |member|
      return unless text = match?(member, journal, text)
    end

    text
  end

  def match?(scanner : Charset, journal : Journal, text : Pf::StringSeln) : Pf::StringSeln?
    assert scanner.min <= scanner.max

    # Match required part.
    scanner.min.times do
      return unless member?(scanner, text)

      text = text.rest
    end

    # Match optional part.
    (scanner.max - scanner.min).times do
      break unless member?(scanner, text)

      text = text.rest
    end

    text
  end

  def match?(scanner : Capture, journal : Journal, text : Pf::StringSeln) : Pf::StringSeln?
    return unless ahead = match?(scanner.member, journal, text)

    if journal.is_a?(CaptureJournal)
      append(journal, scanner.name, text.upto(ahead))
    end

    ahead
  end

  def match?(scanner : Empty, journal : Journal, text : Pf::StringSeln) : Pf::StringSeln?
    text
  end

  def match?(pattern : Pattern, text : Pf::StringSeln, &)
    journal_entries = Pf::Kit.stack_array(CaptureEntry)
    journal = CaptureJournal.new(journal_entries)

    text.each_before_and_after do |_before, after|
      checkpoint = checkpoint(journal)

      if ahead = match?(pattern.scanner, journal, after)
        if !pattern.anchor_r || ahead.empty?
          return (yield journal, ahead)
        end

        # If the pattern is anchored to the right and there are things ahead,
        # then this isn't a match.
        rollback(journal, checkpoint)
      end

      # If scanner did not match and we are anchored to the left, we can't
      # continue searching and have to halt.
      return if pattern.anchor_l

      # Otherwise, we continue searching. We don't need to rollback because
      # `match?` above rolls itself back on mismatch anyway, and the only
      # way we can get here is through its mismatch.
    end
  end

  def match?(pattern : Pattern, text : String, &)
    match?(pattern, Pf::StringSeln.new(text)) do |journal, ahead|
      yield journal, ahead
    end
  end

  def match?(pattern : Pattern, text, **kwargs, &)
    match?(pattern, text, **kwargs) do |journal, ahead|
      yield journal, ahead
    end
  end

  def match?(pattern : Pattern, text, **kwargs) : {Term::Dict, Pf::StringSeln}?
    match?(pattern, text, **kwargs) do |journal, ahead|
      {render(journal), ahead}
    end
  end

  def test?(pattern : Pattern, text : Pf::StringSeln) : Pf::StringSeln?
    text.each_before_and_after do |_, after|
      if ahead = match?(pattern.scanner, NoJournal.new, after)
        if !pattern.anchor_r || ahead.empty?
          return ahead
        end
        # If the pattern is anchored to the right and there are things ahead,
        # then this isn't a match.
      end

      # If scanner did not match and we are anchored to the left, we can't
      # continue searching and have to halt.
      return if pattern.anchor_l

      # Otherwise, we continue searching. We don't need to rollback because
      # `match?` above rolls itself back on mismatch anyway, and the only
      # way we can get here is through its mismatch.
    end
  end
end
