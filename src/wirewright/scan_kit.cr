# A tiny string pattern matching language. A companion of `ParseKit`.
module Ww::ScanKit
  extend self

  alias Scanner = Char | Category | Charset | Simultaneously | Concat | Capture

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
    Char
    Alphabetic
    Emoji
    Numeric
    Grapheme
  end

  defrecord Charset,
    chars : Slice(Char),
    ranges : Slice(Range(Char, Char)),
    categories : Category,
    min : UInt32,
    max : UInt32,
    answer : Bool

  defrecord Simultaneously, members : Slice(Scanner)
  defrecord Concat, members : Slice(Scanner)
  defcase Capture, name : Term::Sym, member : Scanner

  private def category?(qual : Char) : Category?
    case qual
    when '_' then Category::Char
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

  private def charset?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    # ⏏[a-zA-Z%d]
    return unless pattern.starts_with?('[')
    pattern = pattern.rest
    # [⏏a-zA-Z%d]

    if negated = pattern.starts_with?('^')
      pattern = pattern.rest
      # [^⏏a-zA-Z%d]
    end

    chars = Pf::Kit.stack_array(Char, 8)
    ranges = Pf::Kit.stack_array(Range(Char, Char), 4)
    categories = Category::None

    loop do
      if pattern.starts_with?(']')
        pattern = pattern.rest
        # [a-zA-Z%d]⏏
        break
      end

      # [⏏%]
      if pattern.starts_with?("%]")
        pattern = pattern.rest.rest
        # [%]⏏

        chars << '%'
        break
      end

      unless row = char_unit?(pattern)
        # [a-z⏏
        return
      end

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
    end

    min = 1u32
    max = 1u32
    if row = quantifier?(pattern)
      min, max, pattern = row
    end

    scanner = Charset.new(
      chars.to_unsafe_readonly_slice!,
      ranges.to_unsafe_readonly_slice!,
      categories,
      min, max,
      answer: negated ? false : true
    )

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

  private def group?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    return unless pattern.starts_with?('(')

    pattern = pattern.rest
    # (⏏%d%d%d)

    members = Pf::Kit.stack_array(Scanner)

    loop do
      if pattern.starts_with?(')')
        pattern = pattern.rest
        # (%d%d%d)⏏
        break
      end

      return if pattern.empty? # (%d%d%d⏏

      scanner, pattern = scanner(pattern)
      # (⏏%d%d%d)
      # (%d⏏%d%d)
      # (%d%d⏏%d)

      members << scanner
    end

    if member = members.single?
      return member, pattern
    end

    scanner = Concat.new(members.to_unsafe_readonly_slice!)
    {scanner.as(Scanner), pattern}
  end

  private def capture?(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    return if pattern.empty?

    if pattern.starts_with?('(')
      pattern = pattern.rest
      # (⏏age)←[%d]+

      name, pattern = pattern.skip_thru("a-zA-Z")
      return if name.empty?
      # (age⏏)←[%d]+

      return unless pattern.starts_with?(')')
      pattern = pattern.rest
      # (age)⏏←[%d]+
    else
      # ⏏l←[%d]+
      name, pattern = pattern.first_and_rest
      return unless name.chr.in_set?("a-zA-Z")
      # l⏏←[%d]+
    end

    return unless pattern.starts_with?('←')
    pattern = pattern.rest
    # (age)←⏏[%d]+

    return unless row = atom?(pattern) || group?(pattern)

    scanner, pattern = row
    # (age)←[%d]+⏏

    scanner = Capture.new(Term::Sym.new(name.to_s), scanner)
    {scanner.as(Scanner), pattern}
  end

  private def simult?(lhs : Scanner, pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}?
    rhs = Pf::Kit.stack_array(Scanner, 4)

    # x⏏~y
    while pattern.starts_with?('~') && pattern.size > 1
      ahead = pattern.rest
      break unless row = atom?(ahead) || capture?(ahead)

      pattern = pattern.rest
      # x~⏏y

      scanner, pattern = row
      rhs << scanner
    end

    return if rhs.empty?

    members = Slice[lhs] + rhs.to_unsafe_readonly_slice!

    {Simultaneously.new(members).as(Scanner), pattern}
  end

  private def scanner(pattern : Pf::StringSeln) : {Scanner, Pf::StringSeln}
    if row = atom?(pattern) || capture?(pattern)
      return simult?(*row) || row
    end

    assert !pattern.empty?

    head, pattern = pattern.first_and_rest
    chr = head.chr

    # Whitespace in the pattern is equivalent to `[%s]+`.
    if chr == ' '
      scanner = Charset.new(
        chars: Slice(Char).empty,
        ranges: Slice(Range(Char, Char)).empty,
        categories: Category::Space,
        min: 1u32,
        max: UInt32::MAX,
        answer: true,
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
    recognize(pattern.view, anchor_l, anchor_r)
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
    recognize(pattern.view)
  end

  alias Log = CaptureLog | NoLog

  defrecord NoLog
  defrecord CaptureLog, entries : Pf::Kit::HybridArray(LogEntry, 16)

  defrecord LogEntry, name : Term::Sym, capture : Pf::StringSeln

  private def append(log : CaptureLog, name : Term::Sym, capture : Pf::StringSeln) : Nil
    log.entries << LogEntry.new(name, capture)
  end

  private def safepoint(log : CaptureLog) : Int32
    log.entries.size
  end

  private def render(log : CaptureLog) : Term::Dict
    Term::Dict.build do |commit|
      log.entries.each do |entry|
        commit.with(entry.name, entry.capture)
      end
    end
  end

  private def rollback(log : CaptureLog, safepoint : Int32) : Nil
    assert 0 <= log.entries.size >= safepoint

    (log.entries.size - safepoint).times do
      _ = log.entries.pop
    end
  end

  private def member?(category : Category, text : Pf::StringSeln) : Bool
    return false if text.empty?

    if category.char?
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

  private def member?(charset : Charset, text : Pf::StringSeln) : Bool
    return false if text.empty?

    chr = text.first_char

    if charset.chars.any? { |candidate| chr == candidate }
      return charset.answer
    end

    if charset.ranges.any? { |range| chr.in?(range) }
      return charset.answer
    end

    if member?(charset.categories, text)
      return charset.answer
    end

    !charset.answer
  end

  def match?(scanner : Char, log : Log, text : Pf::StringSeln) : Pf::StringSeln?
    return unless text.starts_with?(scanner)

    text.rest
  end

  def match?(scanner : Category, log : Log, text : Pf::StringSeln) : Pf::StringSeln?
    # TODO:
    # if scanner.grapheme?
    #   return skip grapheme
    # end

    return unless member?(scanner, text)

    text.rest
  end

  def match?(scanner : Simultaneously, log : CaptureLog, text : Pf::StringSeln) : Pf::StringSeln?
    safepoint = safepoint(log)

    candidates = Pf::Kit.stack_array(Pf::StringSeln, 4)

    scanner.members.each do |member|
      unless ahead = match?(member, log, text)
        rollback(log, safepoint)
        return
      end

      candidates << ahead
    end

    candidates.max_by?(&.byte_end)
  end

  def match?(scanner : Simultaneously, log : NoLog, text : Pf::StringSeln) : Pf::StringSeln?
    candidates = Pf::Kit.stack_array(Pf::StringSeln, 4)

    scanner.members.each do |member|
      return unless ahead = match?(member, log, text)

      candidates << ahead
    end

    candidates.max_by?(&.byte_end)
  end

  def match?(scanner : Concat, log : CaptureLog, text : Pf::StringSeln) : Pf::StringSeln?
    safepoint = safepoint(log)

    scanner.members.each do |member|
      unless text = match?(member, log, text)
        rollback(log, safepoint)
        return
      end
    end

    text
  end

  def match?(scanner : Concat, log : NoLog, text : Pf::StringSeln) : Pf::StringSeln?
    scanner.members.each do |member|
      return unless text = match?(member, log, text)
    end

    text
  end

  def match?(scanner : Charset, log : Log, text : Pf::StringSeln) : Pf::StringSeln?
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

  def match?(scanner : Capture, log : Log, text : Pf::StringSeln) : Pf::StringSeln?
    return unless ahead = match?(scanner.member, log, text)

    if log.is_a?(CaptureLog)
      append(log, scanner.name, text.upto(ahead))
    end

    ahead
  end

  def match?(scanner : Empty, log : Log, text : Pf::StringSeln) : Pf::StringSeln?
    text
  end

  def match?(pattern : Pattern, text : Pf::StringSeln) : {Term::Dict, Pf::StringSeln}?
    log_entries = Pf::Kit.stack_array(LogEntry)
    log = CaptureLog.new(log_entries)

    text.each_before_and_after do |_, after|
      safepoint = safepoint(log)

      if ahead = match?(pattern.scanner, log, after)
        # If the pattern is anchored to the right and there are things ahead,
        # then this isn't a match.
        if pattern.anchor_r && !ahead.empty?
          rollback(log, safepoint)
          next
        end

        return render(log), ahead
      end

      # If scanner did not match and we are anchored to the left, we can't
      # continue searching and have to halt.
      return if pattern.anchor_l

      # Otherwise, we continue searching. We don't need to rollback because
      # `match?` above rolls itself back on mismatch anyway, and the only
      # way we can get here is through its mismatch.
    end
  end

  def match?(pattern : Pattern, text : String, **kwargs) : {Term::Dict, Pf::StringSeln}?
    match?(pattern, Pf::StringSeln.new(text), **kwargs)
  end

  def test?(pattern : Pattern, text : Pf::StringSeln) : Pf::StringSeln?
    text.each_before_and_after do |_, after|
      if ahead = match?(pattern.scanner, NoLog.new, after)
        # If the pattern is anchored to the right and there are things ahead,
        # then this isn't a match.
        next if pattern.anchor_r && !ahead.empty?
        return ahead
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
