module StringSpan
  extend self

  def lwdrop(text : StringView) : StringView
    initial = text

    # Skip whitespace at which we're currently standing, if we are, as in
    # `hello⏏    world` -> `hello    ⏏world`, or in `hel⏏lo world` this would
    # be noop.
    while text.nonempty? && text.first_char.whitespace?
      text = text.rest
    end

    # Skip non-whitespace.
    until text.empty? || text.first_char.whitespace?
      text = text.rest
    end

    text
  end

  def rwdrop(text : StringView) : StringView
    initial = text

    # Skip whitespace at which we're currently standing, if we are, as in
    # `hello    ⏏world` -> `hello⏏    world`, or in `hello wor⏏ld` this would
    # be noop.
    while text.nonempty? && text.last_char.whitespace?
      text = text.prior
    end

    # Skip non-whitespace.
    until text.empty? || text.last_char.whitespace?
      text = text.prior
    end

    text
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
  rulepi1 %[(- arg_number)] { -arg.unsafe_as_n }
  rulepi1 %[(- args_number+)] { args.items.reduce { |a, b| a - b } }
  rulepi1 %[(* args_number+)] { args.items.reduce { |a, b| a * b } }

  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
  rulepi1 %[(// a_number (%all b_number (%not 0)))] { a // b }
  rulepi1 %[(mod a_number (%all b_number (%not 0)))] { a % b }

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

  rulepi1 %[(ml->term ml_string ¦ () shadow⋮ true)] do
    term = ML.term(ml.to(String))

    {:"ml/ok", term}
  rescue ML::SyntaxError
    # TODO: line col message
    {:"ml/err"}
  end

  rulepi1 %[(< a_number b_number)] do
    a.unsafe_as_n < b.unsafe_as_n
  end

  rulepi1 %[(<= a_number b_number)] do
    a.unsafe_as_n <= b.unsafe_as_n
  end

  rulepi1 %[(> a_number b_number)] do
    a.unsafe_as_n > b.unsafe_as_n
  end

  rulepi1 %[(>= a_number b_number)] do
    a.unsafe_as_n >= b.unsafe_as_n
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

  # FIXME: remove this flag
  {% if flag?(:soma6) %}
    # Converts an arbitrary term into its D7VR (Microfold unit) -> UIR (thus D7UIR)
    # code-only representation.
    #
    # D7VR is what you convert a term into, to then feed that to Microfold, then UIR,
    # then draw it, then paint the resulting draw commands using some kind of painting
    # backend (e.g. sfpaint).
    rulepi1 %[(d7uir term_ ¦ () rem_: (%number +i32) code-only: true)] do
      pipe(term, D7VR.term_unit, D7VR.uir(rem: rem.unsafe_as_n))
    end
  {% end %}

  # E.g. (union {x: 1, y: 2} (entry z 3))
  rulepi1 %[(entry k_ v_)] do
    Term[].with(k, v)
  end

  rulepi1 %[(entries xs_dict)] do
    Term[xs.ee(ordered: true)]
  end

  rulepi1 %[(dict entries list←((%past (_ _) min: 0)))] do
    Term::Dict.build do |commit|
      list.items.each do |(key, value)|
        commit.with(key, value)
      end
    end
  end

  rulepi1 %[(append x_dict keypath_+ term_)] do
    Term.morph(x, keypath.items) do |tip|
      if tip.type.dict?
        Term.of(tip.unsafe_as_d.append(term))
      else
        tip
      end
    end
  end

  rulepi1 %[(cat xs_dict+)] do
    Term::Dict.build do |commit|
      xs.items.each do |x|
        x = x.unsafe_as_d

        commit.concat(x.items)

        x.each_pair do |k, v|
          commit.with(k, v)
        end
      end
    end
  end

  rulepi1 %[(union xs_dict*)] do
    xs.items.reduce(Term[]) { |memo, dict| memo | dict }
  end

  # alias
  rulepi1 %[(∪ xs_dict*)] do
    xs.items.reduce(Term[]) { |memo, dict| memo | dict }
  end

  rulepi1 %[(∩ xs_dict ys_dict)] { xs.unsafe_as_d.msect(ys.unsafe_as_d) }

  rulepi1 %[(merge xs_dict ys_dict)] do
    Term.merge(xs, ys)
  end

  rulepi1 %[(value xs_dict key_)] do
    xs[key]? || Term.of(:value, xs, key)
  end

  rulepi1 %[(value? xs_dict key_)] do
    if value = xs[key]?
      Term.of(:some, value)
    else
      Term.of(:none)
    end
  end

  rulepi1 %[(hash term_)] do
    Term.hashcode(term)
  end

  # TODO: remove!!!!!! this leaks the fact that we don't actually have proper entry order!!
  rulepi1 %[(nth xs_dict n←(%number +i32))] do
    if response = xs.nth?(n.to(Int32))
      key, value = response
      Term.of(:some, {key, value})
    else
      Term.of({:none})
    end
  end

  rulepi1 %[(key xs←(%pipe tally 1))] do
    key, _ = xs.nth(0)
    key
  end

  rulepi1 %[(nth (range b_number e_number points: m_number) n_number)] do |n, m|
    n = n.floor
    m = m.floor
    n %= m
    b + n * ((e - b)/m)
  end

  rulepi1 %[(itemspart xs_dict)] do
    xs.itemspart
  end

  rulepi1 %[(pairspart xs_dict)] do
    xs.pairspart
  end

  # Flattens itemspart of *xs*, its items and so on, recursively.
  rulepi1 %[(flatten xs_)] do
    Term::Dict.build do |commit|
      Term.each_keypath_and_item_leaf(xs) do |_, leaf|
        commit << leaf

        true # Continue
      end
    end
  end

  rulepi1 %{(subseq whole_dict (from index←(%number +i32)))} do
    Term::Dict.build do |commit|
      commit.concat(whole.items.move(index.to(Int32)))
    end
  end

  rulepi1 %{(subseq whole_dict (pattern selector_))} do
    Term::Dict.build do |commit|
      commit.selected(whole.items) do |item|
        M1.probe?(selector, item)
      end
    end
  end

  # TODO: we must use this under `complement` somehow!!!!
  rulepi1 %{(-subseq whole_dict (pattern selector_))} do
    Term::Dict.build do |commit|
      commit.rejected(whole.items) do |item|
        M1.probe?(selector, item)
      end
    end
  end

  rulepi1 %{(part whole_dict (key key_))} do
    whole.pluck(key)
  end

  rulepi1 %{(part whole_dict (key key_ ¦ () default_))} do
    whole[key]? || default
  end

  rulepi1 %{(part whole_dict items)} do
    whole.itemspart
  end

  rulepi1 %{(part whole_dict pairs)} do
    whole.pairspart
  end

  rulepi1 %{(complement universe_dict subset_dict)} do
    universe.transaction do |commit|
      subset.each_entry do |key, _|
        commit.without(key)
      end
    end
  end

  rulepi1 %[(charcount xs_string+)] do
    xs.items.sum(0, &.unsafe_as_s.charcount)
  end

  rulepi1 %[(chunks arg_dict (pattern criterion_))] do
    chunk = nil
    chunks = Term[]

    arg.items.each do |item|
      unless M1.probe?(criterion, item)
        if chunk
          chunks = chunks.append(chunk)
          chunk = nil
        end
        chunks = chunks.append({:item, item})
        next
      end

      chunk ||= Term[{:chunk}]
      chunk = chunk.append(item)
    end

    chunks = chunks.append(chunk) if chunk
    chunks
  end

  rulepi1 %[(chunks arg_dict (group precursor_ member_))] do
    chunks = Term[]

    i = 0
    while i < arg.itemsize
      head = arg[i]
      unless M1.probe?(precursor, head)
        chunks = chunks.append({:item, head})
        i += 1
        next
      end

      i += 1
      n = 0
      (i...arg.itemsize).each do |j|
        jth = arg[j]
        break if M1.probe?(precursor, jth)
        break unless M1.probe?(member, jth)

        n += 1
      end

      if n.zero?
        chunks = chunks.append({:item, head})
        next
      end

      chunk = Term::Dict.build do |commit|
        commit << :chunk << head
        commit.concat(arg.items.move(i).begin.grow(n))
      end

      i += n
      chunks = chunks.append(chunk)
    end

    chunks
  end

  rulepi1 %[(sum ())] { 0 }
  rulepi1 %[(sum (args_number+))] { args.items.reduce { |a, b| a.unsafe_as_n + b.unsafe_as_n } }

  rulepi1 %[(min args_number+)] { args.items.min_by(&.unsafe_as_n) }
  rulepi1 %[(min (args_number+))] { args.items.min_by(&.unsafe_as_n) }

  rulepi1 %[(max args_number+)] { args.items.max_by(&.unsafe_as_n) }
  rulepi1 %[(max (args_number+))] { args.items.max_by(&.unsafe_as_n) }

  rulepi1 %[(floor arg_number)] { arg.unsafe_as_n.floor }
  rulepi1 %[(ceil arg_number)] { arg.unsafe_as_n.ceil }
  rulepi1 %[(round arg_number)] { arg.unsafe_as_n.round }

  # TODO: floor/ceil/round args_number+ is mass-floor
  # TODO: floor/ceil/round on list of numbers

  rulepi1 %[(upcase arg_string)] { arg.upcase }
  # TODO: downcase -> dncase for symmetry
  rulepi1 %[(downcase arg_string)] { arg.downcase }

  # TODO: upcase/dncase args_string is mass-upcase/dncase
  # TODO: upcase/dncase on list of strings

  rulepi1 %[(tally args_dict+)] do
    args.items.reduce(0) { |memo, arg| memo + arg.size }
  end

  rulepi1 %[(take s_string o←(%number +i32) span←(%number i32))] do
    mb, me = {o.to(Int32), (o + span).to(Int32)}.minmax

    l = Term::Str::Substring.runes(s.unsafe_as_s, 0, mb)
    m = Term::Str::Substring.runes(s.unsafe_as_s, mb, me)
    r = Term::Str::Substring.runes(s.unsafe_as_s, me, s.charcount)

    {l, m, r}
  end

  # TODO: these should be under `substring`, e.g. `(substring s (rune B) (word E))`.

  rulepi1 %[(runes s_string b←(%number i32) to e←(%number i32))] do
    Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end
  # alias
  rulepi1 %[(runes s_string b←(%number i32) ..= e←(%number i32))] do
    Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(runes s_string b←(%number +i32) ..< e←(%number +i32))] do
    if b == e
      Term.of("")
    else
      Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32) - 1)
    end
  end

  rulepi1 %[(rune s_string b←e←(%number i32))] do
    Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(words s_string b←(%number i32) to e←(%number i32))] do
    Term::Str::Substring.words(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end
  # alias
  rulepi1 %[(words s_string b←(%number i32) ..= e←(%number i32))] do
    StringSpan.words(s.to(StringView), b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(word s_string b←e←(%number i32))] do
    StringSpan.words(s.to(StringView), b.to(Int32), e.to(Int32))
  end

  # FIXME: this is too lame
  rulepi1 %[(wordwise s_string)] do
    Term::Dict.build do |commit|
      s.to(StringView).split_and_rest(' ') do |segment, sep, _|
        commit << segment
        commit << sep unless sep.empty?
      end
    end
  end

  rulepi1 %[(lines s_string b←(%number i32) to e←(%number i32))] do
    Term::Str::Substring.lines(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end
  # alias
  rulepi1 %[(lines s_string b←(%number i32) ..= e←(%number i32))] do
    Term::Str::Substring.lines(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  rulepi1 %[(line s_string b←e←(%number i32))] do
    Term::Str::Substring.lines(s.unsafe_as_s, b.to(Int32), e.to(Int32))
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

  # S₁S₂S₃S₄
  # M₁M₂
  # == S₁S₂
  #
  # S₁S₂S₃S₄
  # M₁M₂M₃M₄M₅M₆
  # == S₁S₂S₃S₄
  rulepi1 %[(prefix s_string m_string)] do
    Term::Str::Substring.runes(s.unsafe_as_s, 0, m.charcount - 1)
  end
  rulepi1 %[(prefix s_string "")] do
    ""
  end

  rulepi1 %[(suffix s_string m_string)] do
    Term::Str::Substring.runes(s.unsafe_as_s, m.charcount, -1)
  end

  rulepi1 %{(mask d_dict pattern_)} do
    Term::Dict.build do |commit|
      d.each_item_with_index do |item, index|
        if M1.probe?(pattern, item)
          commit.with(index, true)
        end
      end
    end
  end

  # TODO: these are really just generic keysect / key complement followed by
  # values perhaps. But currently there's hardly any way for us to do something
  # as "advanced" as (intersection _ (keys _)) or (complement _ (keys _)) evaluation-wise.

  rulepi1 %{(matches d_dict mask_dict)} do
    Term::Dict.build do |commit|
      d.items.each_with_index do |item, index|
        next unless index.in?(mask)
        commit << item
      end
      d.each_pair do |key, value|
        next unless key.in?(mask)
        commit.with(key, value)
      end
    end
  end

  rulepi1 %{(mismatches d_dict mask_dict)} do
    Term::Dict.build do |commit|
      d.items.each_with_index do |item, index|
        next if index.in?(mask)
        commit << item
      end
      d.each_pair do |key, value|
        next if key.in?(mask)
        commit.with(key, value)
      end
    end
  end

  rulepi1 %{(includes? haystack_string needle_string)} do
    haystack.to(String).includes?(needle.to(String))
  end
end

PRIMITIVES_REWRITER = callR(PRIMITIVES)

def primitivesR
  PRIMITIVES_REWRITER
end
