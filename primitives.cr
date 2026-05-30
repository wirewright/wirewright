module StringSpan
  extend self

  # Reference: https://github.com/microsoft/vscode/blob/7dd556f54d68b8ac6c15ca27566acc6d0f3c1f9a/src/vs/editor/common/config/editorOptions.ts#L101
  #
  # Added WwML-specific delimiters.
  #
  # TODO: this set belongs to editR.codex.wwml, and must be read from the kernel
  # and configurable.
  def wsep?(char : Char)
    char.in_set?("~!@#$%^&*()\\-=+[{]}\\|;:'\",.<>/?←→↑↓¦⍊⟨⟩⟪⟫")
  end

  def wdrop(text : StringView, head : StringView -> Char, tail : StringView -> StringView)
    initial = text

    if text.nonempty? && head.call(text).vspace?
      return tail.call(text)
    end

    # Skip whitespace at which we're currently standing, if we are, as in
    # `hello⏏    world` -> `hello    ⏏world`, or in `hel⏏lo world` this would
    # be noop.
    while text.nonempty? && head.call(text).hspace?
      text = tail.call(text)
    end

    wseps = false

    # Like VSCode, skip word separators, if any.
    while text.nonempty? && wsep?(head.call(text))
      text = tail.call(text)
      wseps = true
    end

    # If we managed to skip some word separators, that's it.
    if wseps
      return text
    end

    # Skip until word separator.
    until text.empty? || (head.call(text).whitespace? || wsep?(head.call(text)))
      text = tail.call(text)
    end

    text
  end

  def lwdrop(text : StringView) : StringView
    wdrop(text, head: ->(view : StringView) { view.first_char }, tail: ->(view : StringView) { view.rest })
  end

  def rwdrop(text : StringView) : StringView
    wdrop(text, head: ->(view : StringView) { view.last_char }, tail: ->(view : StringView) { view.prior })
  end

  def lwdrop(text : StringView, n : Int) : StringView
    n.times { text = lwdrop(text) }

    text
  end

  def rwdrop(text : StringView, n : Int) : StringView
    n.times { text = rwdrop(text) }

    text
  end

  def lwtake(text : StringView, n : Int) : StringView
    rest = lwdrop(text, n)

    StringView.between(text.before_begin, rest.before_begin)
  end

  def rwtake(text : StringView, n : Int) : StringView
    rest = rwdrop(text, n)

    StringView.between(rest.after_end, text.after_end)
  end

  # NOTE: *e* is inclusive!
  def words(text : StringView, b : Int, e : Int)
    text = b.negative? ? rwtake(text, b.abs) : lwdrop(text, b)
    text = e.negative? ? rwdrop(text, e.abs - 1) : lwtake(text, e - b + 1)
    text
  end
end

# TODO: this ruleset is a seed for something that will later be known as *Nitrene*.
# Nitrene is a tiny embedded language for describing computations. I believe it
# should *not* have loops, or conditionals. However, it should allow to construct
# elaborate, long (or short!) "pipelines" from a vocabulary of extremely generic
# concepts (map, reduce, fold, whatever). No functions, too. I also don't think
# it should have lambdas. In a sense, it's supposed to be a "LEGO" for describing
# arbitrary computations, transformations, whatever (of terms). Something
# combinatorial if that's the right word, or maybe compositional; APL in spirit,
# although APL, too, has functions -- as far as I remember. Perhaps Clojure could
# serve as an inspiration, Rich Hickey and the Clojure communitiy appear to be doing
# a lot of related work.
PRIMITIVES = ProcRuleset.build do
  rulepi1 %[(+ args_number+)] { args.items.reduce { |a, b| a + b } }
  rulepi1 %[(- arg_number)] { -arg.as_n }
  rulepi1 %[(- args_number+)] { args.items.reduce { |a, b| a - b } }
  rulepi1 %[(* args_number+)] { args.items.reduce { |a, b| a * b } }

  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
  rulepi1 %[(// a_number (%all b_number (%not 0)))] { a // b }
  rulepi1 %[(mod a_number (%all b_number (%not 0)))] { a % b }

  rulepi1 %[(** a_number b_number)] { a.as_n ** b.as_n }

  rulepi1 %[(~ args_+)] do
    args.items.reduce(Term[""]) do |prefix, arg|
      suffix = arg.as_s? || Term[ML.display(arg, endl: false)]
      prefix.stitch(suffix)
    end
  end

  rulepi1 %[(~* arg_dict)] do
    arg.items.reduce(Term[""]) do |prefix, arg|
      suffix = arg.as_s? || Term[ML.display(arg, endl: false)]
      prefix.stitch(suffix)
    end
  end

  rulepi1 %[(x a_string n←(%number +i32))] do
    a.to(String) * n.to(Int32)
  end

  rulepi1 %[(ml/compact term_)] do
    Term[ML.compact(term)]
  end

  rulepi1 %[(term->ml term_)] do
    # TODO: use pretty print with forced inline
    Term[ML.display(term, endl: false)]
  end

  {% for entity in %w[term terms document] %}
    rulepi1 %[(ml/{{entity.id}} ml_string)] do
      Term.of(:ok, ML.{{entity.id}}(ml.to(String)))
    rescue e : ML::SyntaxError
      excerpt, line, column = ML::SyntaxError.lookaround(e.text)

      Term.of(:err, detail: e.detail, excerpt: excerpt, line: line, column: column)
    end
  {% end %}

  rulepi1 %[(escaped s_string)] do
    Term::Str.new(s.as_s.escaped)
  end

  rulepi1 %[(< a_number b_number)] do
    a.as_n < b.as_n
  end

  rulepi1 %[(<= a_number b_number)] do
    a.as_n <= b.as_n
  end

  rulepi1 %[(> a_number b_number)] do
    a.as_n > b.as_n
  end

  rulepi1 %[(>= a_number b_number)] do
    a.as_n >= b.as_n
  end

  rulepi1 %[(= a_ bs_+)] do
    bs.unsafe_as_d.items.all? { |b| a == b }
  end

  rulepi1 %[(not true)] { false }
  rulepi1 %[(not false)] { true }

  rulepi1 %[(and true true)] { true }
  rulepi1 %[(and _ _)] { false }

  rulepi1 %[(or false false)] { false }
  rulepi1 %[(or _ _)] { true }

  # E.g. (union {x: 1, y: 2} (entry z 3))
  rulepi1 %[(entry k_ v_)] do
    Term[].with(k, v)
  end

  rulepi1 %[(entries xs_dict)] do
    Term[xs.ee(ordered: true)]
  end

  rulepi1 %[(union xs_dict*)] do
    xs.items.reduce(Term.of) { |memo, dict| Term.union(memo, dict) }
  end

  rulepi1 %[(merge xs_dict ys_dict)] do
    Term.merge(xs, ys)
  end

  rulepi1 %[(intersection xs_dict mask_dict)] do
    Term.intersection(xs.as_d, mask.as_d)
  end

  # FIXME: This shouldn't exist. It's just that our mask stuff is degenerate.
  # And -f is to fix a heap of bugs related to invalid evaluation order in Alloy.
  # How the F did we get here? It's not that I don't know how to write interpreters...
  # We should move to Nitrene. We really should !!
  rulepi1 %[(select-f xs_dict pattern_)] do
    xs.transaction do |commit|
      xs.each_entry do |key, value|
        unless env = M1.match?(pattern, value)
          commit.without(key)
          next
        end

        commit.with(key, env[:value]? || value)
      end
    end
  end

  # multiset union
  rulepi1 %[(mset/union a_dict b_dict)] do
    if a.size < b.size
      sm, lg = {a, b}
    else
      sm, lg = {b, a}
    end

    lg.transaction do |commit|
      sm.each_entry do |key, sm_value|
        next unless sm_count = sm_value.as_n?
        next unless lg_count = lg[key]? || Term.of(0)
        next unless lg_count = lg_count.as_n?

        commit.with(key, sm_count + lg_count)
      end
    end
  end

  rulepi1 %[(value xs_dict key_)] do
    xs[key]? || Term.of(:value, xs, key)
  end

  rulepi1 %[(value xs_dict keys_* ⍊ default_)] do
    xs.as_d.follow?(keys.items) || default
  end

  rulepi1 %[(hashcode term_)] do
    Term.hashcode(term)
  end

  rulepi1 %[(hashcode/hex term_)] do
    Term.hashcode(term).to_s(base: 16)
  end

  rulepi1 %[(iota n←(%number +i32))] do
    Term::Dict.build do |commit|
      (0...n.to(Int32)).each do |i|
        commit << i
      end
    end
  end

  rulepi1 %[(itemspart xs_dict)] do
    xs.itemspart
  end

  rulepi1 %[(pairspart xs_dict)] do
    xs.pairspart
  end

  # Flattens itemspart of *xs*, its items and so on, recursively.
  rulepi1 %[(flatten xs_ ¦ () depth_: (%optional ∞ (%any° ∞ (%number +i32))))] do
    if depth == Term.of(:∞)
      Term.flatten(xs, depth: nil)
    else
      Term.flatten(xs, depth: depth.to(Int32))
    end
  end

  rulepi1 %[(charcount xs_string*)] do
    xs.items.sum(0, &.as_s.charcount)
  end

  rulepi1 %[(bytesize xs_blob*)] do
    xs.items.sum(0, &.as_blob.ubytesize64)
  end

  # TODO: sum, min, and max should probably ignore non-numbers, and they should operate
  # on dicts (as in, on entry values, not just items),
  rulepi1 %[(sum ())] { 0 }
  rulepi1 %[(sum (args_number+))] { args.items.reduce { |a, b| a.as_n + b.as_n } }

  rulepi1 %[(product ())] { 0 }
  rulepi1 %[(product (args_number+))] { args.items.reduce { |a, b| a.as_n * b.as_n } }

  rulepi1 %[(min args_number+)] { args.items.min_by(&.as_n) }
  rulepi1 %[(min (args_number+))] { args.items.min_by(&.as_n) }

  rulepi1 %[(max args_number+)] { args.items.max_by(&.as_n) }
  rulepi1 %[(max (args_number+))] { args.items.max_by(&.as_n) }

  rulepi1 %[(abs args_number+)] { args.items.reduce { |memo, arg| memo - arg }.abs }

  rulepi1 %[(floor arg_number)] { arg.as_n.floor }
  rulepi1 %[(ceil arg_number)] { arg.as_n.ceil }
  rulepi1 %[(round arg_number)] { arg.as_n.round }

  rulepi1 %[(upcase arg_string)] { arg.upcase }
  rulepi1 %[(dncase arg_string)] { arg.downcase }

  # TODO: Rename to `size`
  rulepi1 %[(tally args_dict+)] do
    args.items.sum(0, &.size)
  end

  rulepi1 %[(itemsize args_dict+)] do
    args.items.sum(0, &.itemsize)
  end

  rulepi1 %[(pairsize args_dict+)] do
    args.items.sum(0, &.pairsize)
  end

  rulepi1 %[(runes s_string b←(%number i32) ..= e←(%number i32))] do
    s.to(String)[b.to(Int32)..e.to(Int32)]? || Term.of("")
  end

  rulepi1 %[(runes s_string b←(%number +i32) ..< e←(%number +i32))] do
    s.to(String)[b.to(Int32)...e.to(Int32)]? || Term.of("")
  end

  rulepi1 %[(rune s_string i←(%number i32))] do
    s.to(String)[i.to(Int32)]? || Term.of("")
  end

  rulepi1 %[(words s_string b←(%number i32) ..= e←(%number i32))] do
    StringSpan.words(s.to(StringView), b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(word s_string b←e←(%number i32))] do
    StringSpan.words(s.to(StringView), b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(words s_string)] do
    words = Term::Dict.build do |commit|
      # TODO: Since we depend on libunibreak we should use it here!
      s.to(StringView).split_and_rest(' ') do |word, _, _|
        next if word.empty? # But *can* it be empty?
        commit << word
      end
    end
  end

  rulepi1 %[(line/stem s_string)] do
    view = s.to(StringView)
    l, sep, r = view.partition('\n')
    l
  end

  rulepi1 %[(line/rest s_string)] do
    view = s.to(StringView)
    l, sep, r = view.partition('\n')
    sep + r
  end

  rulepi1 %[(rline/stem s_string)] do
    view = s.to(StringView)
    l, sep, r = view.rpartition('\n')
    r
  end

  rulepi1 %[(rline/rest s_string)] do
    view = s.to(StringView)
    l, sep, r = view.rpartition('\n')
    l + sep
  end

  rulepi1 %{(includes? haystack_string needle_string)} do
    haystack.to(String).includes?(needle.to(String))
  end

  rulepi1 %{(any? haystack_dict needle_)} do
    haystack.as_d.any?(needle)
  end

  rulepi1 %{(prefix-run matchee_string prefix_string)} do
    matchee_ = matchee.to(StringView)
    prefix_ = prefix.to(StringView)

    run = String.build do |io|
      while matchee_.starts_with?(prefix_)
        matchee_ = matchee_.lskip(prefix_.size)
        io << prefix_
      end
    end

    Term.of(run)
  end

  # Converts *text* to a sequence of Unicode codepoints.
  rulepi1 %{(codepoints text_string)} do
    Term::Dict.build do |commit|
      string = text.to(String)
      string.each_char do |chr|
        commit << chr.ord
      end
    end
  end

  rulepi1 %{(repr ns←((%past (%number (whole _)))) (digits ¦ () alphabet_string))} do
    letters = alphabet.to(String)

    Term::Dict.build do |commit|
      ns.items.each do |n|
        Int.each_digit(n, base: letters.size) do |digit|
          assert digit.natural?

          commit << letters[digit.to(Int32)]
        end
      end
    end
  end

  rulepi1 %{(mask pattern_ d_dict)} do
    Term::Dict.build do |commit|
      d.each_item_with_index do |item, index|
        if M1.probe?(pattern, item)
          commit.with(index, true)
        end
      end
    end
  end

  rulepi1 %{(mask charset_string s_string)} do
    set = charset.to(String)

    mask = Term::Dict.build do |commit|
      s.to(StringView).each_char_with_index do |chr, index|
        next unless chr.in_set?(set)

        commit.with(index, true)
      end
    end

    {s, mask}
  end

  rulepi1 %{(matches d_dict mask_dict)} do
    Term::Dict.build do |commit|
      d.items.each_with_index do |item, index|
        next unless index.in?(mask)
        commit << item
      end
      d.each_entry(in: Term::Dict.pairspart) do |key, value|
        next unless key.in?(mask)
        commit.with(key, value)
      end
    end
  end

  rulepi1 %{(backmap d_dict pattern_ backspec_)} do
    M1.backmap(pattern, backspec, d)
  end
end

PRIMITIVES_REWRITER = callR(PRIMITIVES)

def primitivesR
  PRIMITIVES_REWRITER
end
