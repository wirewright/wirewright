require "./wirewright"

# TODO: these should probably be or have corresponding structs. We're using
# this enum as a dirt cheap alternative to a Set so it should probably remain
# the way it is. it must probably contain auto-generated members from structs and
# aliases. and must be able to translate them back and forth (from enum meber
# to struct and vice versa). Each "Style" struct, in turn, will have the corresponding
# render() method defined on it.
@[Flags]
enum DictStyles : UInt16
  # Renders the entries of a dict *inline*. One of the general-purpose styles
  # (can present any dict nicely).
  #
  # ```wwml
  # (text "Hello World 1" "Hello World 2" "Hello World 3" x: 100 y: 200)
  # ```
  DictInline

  # Renders the first item and pairs of a dict inline, then follows with the remaning
  # items padded, multiline.
  #
  # Requirements:
  #
  # - the dict has pairs,
  # - the dict has two or more items,
  # - the dict's first item is a symbol.
  #
  # ```wwml
  # (text x: 100 y: 200
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3")
  # ```
  BlockCallKeywordsInline

  # Renders the first item inline followed by a column of pairs, then all remaning
  # items padded, multiline.
  #
  # Requirements:
  #
  # - the dict has pairs,
  # - the dict has two or more items,
  # - the dict's first item is a symbol.
  #
  # ```wwml
  # (text x: 100
  #       y: 200
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3")
  # ```
  BlockCallKeywordsColumn

  # Renders the first and second items inline, then all pairs padded, multiline.
  #
  # Requirements:
  #
  # - the dict has pairs,
  # - the dict has exactly two items,
  # - the dict's first item is a symbol.
  #
  # ```wwml
  # (text "Hello World 1"
  #   x: 100
  #   y: 200)
  # ```
  KeywordBlockCall

  # Renders the first item inline followed by a column of all remaining items, each
  # item inline.
  #
  # Requirements:
  #
  # - the dict does not have pairs,
  # - the dict has two or more items,
  # - the dict's first item is a symbol.
  #
  # ```wwml
  # (text "Hello World 1"
  #       "Hello World 2"
  #       "Hello World 3")
  # ```
  CallColumn

  # Renders the entries of a dict on multiple lines. Items following the first one
  # are padded once. Only used when the first item of the dict is a symbol.
  #
  # ```wwml
  # (text
  #    "Hello World 1"
  #    "Hello World 2"
  #    "Hello World 3"
  #    x: 100
  #    y: 200)
  # ```
  CallIndented

  # Renders the entries of a dict on multiple lines. All items are lined up.
  #
  # ```wwml
  # (text
  #  "Hello World 1"
  #  "Hello World 2"
  #  "Hello World 3"
  #  x: 100
  #  y: 200)
  # ```
  CallAligned

  # Renders the entries of a pairsonly dict inline.
  #
  # Requirements:
  #
  # - the dict must be pairsonly,
  # - the dict must have at least one pair.
  #
  # ```wwml
  # {x: 100, y: 200, z: 300}
  # ```
  MapInline

  # Renders the entries of a pairsonly dict, each on a separate line.
  #
  # Requirements:
  #
  # - the dict must be pairsonly,
  # - the dict must have at least one pair.
  #
  # ```wwml
  # {x: 100,
  #  y: 200,
  #  z: 300}
  # ```
  MapMultiline

  Inline = MapInline | DictInline
end

# TODO: optimize
def thunk(subject : Term, myself : DictStyles, children : DictStyles, postfix : String)
  Term::Dict.build do |commit|
    commit << :thunk << subject << postfix
    commit << :myself
    myself.each do |style|
      commit << style.to_s
    end
    commit << :children
    children.each do |style|
      commit << style.to_s
    end
  end
end

def render_item(v, styles, postfix)
  render(v, styles, postfix)
end

def render_pair(k, v, styles, postfix)
  Term.of(:row,
    render(k, styles, ""),
    Term[:frag, ":", width: 1, height: 1],
    Term[:padding, render(v, styles, postfix), pl: 1])
end

def render_entry(dict, k, v, styles, postfix)
  if dict.index?(k)
    render_item(v, styles, postfix)
  else
    render_pair(k, v, styles, postfix)
  end
end

def render_dict_inline(dict, styles, postfix)
  Term::Dict.build do |inner|
    inner << :row
    inner.concat(0...dict.size - 1) do |index|
      render_entry(dict, *dict.ordnth(index), styles & DictStyles::Inline, "")
    end
    inner << render_entry(dict, *dict.ordnth(dict.size - 1), styles & DictStyles::Inline, postfix)
    inner.with(:gap, 1)
  end
end

def render_call_column(dict, styles, postfix)
  unless dict.itemsonly? && dict.size >= 2
    raise StyleNotApplicable.new
  end

  item_column = Term::Dict.build do |commit|
    commit << :col
    commit.concat(1...dict.itemsize - 1) do |index|
      render_entry(dict, *dict.ordnth(index), styles & DictStyles::Inline, "")
    end
    commit << render_entry(dict, *dict.ordnth(dict.hi), styles & DictStyles::Inline, postfix)
  end

  Term[:row, render_entry(dict, *dict.ordnth(0), styles & DictStyles::Inline, ""), item_column, gap: 1]
end

# TODO: if head does not fit inline, maybe we should force unpadded?
def render_multiline(dict, styles, postfix, *, padded = true)
  case dict.size
  when 0
    raise ArgumentError.new
  when 1
    render_entry(dict, *dict.ordnth(0), styles, postfix)
  else
    cursor = 0

    head = render_entry(dict, *dict.ordnth(cursor), styles, "")
    cursor += 1

    body = Term::Dict.build do |outer|
      outer << :col
      outer.concat(cursor...dict.size - 1) do |index|
        render_entry(dict, *dict.ordnth(index), styles, "")
      end
      cursor = Math.max(cursor, dict.size - 1)
    end

    tail = render_entry(dict, *dict.ordnth(cursor), styles, postfix)

    if padded
      Term[:col, head, Term[:padding, Term[:col, body, tail], pl: 1]]
    else
      Term[:col, head, body, tail]
    end
  end
end

def render_call_keywords_inline(dict, styles, postfix)
  unless dict.itemsize >= 2 && dict.pairsize > 0
    raise StyleNotApplicable.new
  end

  head = Term::Dict.build do |commit|
    commit << :row
    commit << render_entry(dict, *dict.ordnth(0), styles & DictStyles::Inline, "")
    commit.concat(dict.itemsize...dict.size) do |pair_index|
      render_entry(dict, *dict.ordnth(pair_index), styles & DictStyles::Inline, "")
    end
    commit.with(:gap, 1)
  end

  body = Term::Dict.build do |commit|
    commit << :col
    commit.concat(1...dict.itemsize - 1) do |item_index|
      render_entry(dict, *dict.ordnth(item_index), styles, "")
    end
  end

  tail = render_entry(dict, *dict.ordnth(dict.hi), styles, postfix)

  Term[:col, head, Term[:padding, Term[:col, body, tail], pl: 1]]
end

def render_call_keywords_column(dict, styles, postfix)
  unless dict.itemsize >= 2 && dict.pairsize > 0
    raise StyleNotApplicable.new
  end

  head = render_entry(dict, *dict.ordnth(0), styles & DictStyles::Inline, "")

  pairs_column = Term::Dict.build do |commit|
    commit << :col
    commit.concat(dict.itemsize...dict.size) do |pair_index|
      render_entry(dict, *dict.ordnth(pair_index), styles, "")
    end
  end

  body = Term::Dict.build do |commit|
    commit << :col
    commit.concat(1...dict.itemsize - 1) do |item_index|
      render_entry(dict, *dict.ordnth(item_index), styles, "")
    end
  end

  tail = render_entry(dict, *dict.ordnth(dict.hi), styles, postfix)

  Term[:col, Term[:row, head, pairs_column, gap: 1], Term[:padding, Term[:col, body, tail], pl: 1]]
end

def render_keyword_block_call(dict, styles, postfix)
  unless dict.itemsize == 2 && dict.pairsize > 0
    raise StyleNotApplicable.new
  end

  item0 = render_entry(dict, *dict.ordnth(0), styles & DictStyles::Inline, "")
  item1 = render_entry(dict, *dict.ordnth(1), styles & DictStyles::Inline, "")

  pairs = Term::Dict.build do |commit|
    commit << :col
    commit.concat(dict.itemsize...dict.size - 1) do |pair_index|
      render_entry(dict, *dict.ordnth(pair_index), styles, "")
    end
    commit << render_entry(dict, *dict.ordnth(dict.size - 1), styles, postfix)
  end

  Term[:col, Term[:row, item0, item1, gap: 1], Term[:padding, pairs, pl: 1]]
end

def render_map_inline(dict, styles, postfix)
  unless dict.pairsonly? && dict.pairsize > 0
    raise StyleNotApplicable.new
  end

  Term::Dict.build do |commit|
    commit << :row
    dict.ee.each_with_index do |(k, v), index|
      if index == dict.size - 1
        commit << render_pair(k, v, styles & DictStyles::Inline, postfix)
      else
        commit << render_pair(k, v, styles & DictStyles::Inline, ",")
      end
    end
    commit.with(:gap, 1)
  end
end

def render_map_multiline(dict, styles, postfix)
  unless dict.pairsonly? && dict.pairsize > 0
    raise StyleNotApplicable.new
  end

  Term::Dict.build do |commit|
    commit << :col
    dict.ee.each_with_index do |(k, v), index|
      if index == dict.size - 1
        commit << render_pair(k, v, styles, postfix)
      else
        commit << render_pair(k, v, styles, ",")
      end
    end
  end
end

def render_pp_pair(k, v, styles, postfix)
  Term.of_case(v, env: Term[k: k]) do
    matchpi %[(%'%let k_symbol realv_)] do
      Term.of(:row,
        render(k, styles, ""),
        Term[:frag, "_:", width: 2, height: 1],
        Term[:padding, render(realv, styles, postfix), pl: 1])
    end

    matchpi %[(%'%optional fallback_ realv_symbol)] do
      continue unless blank = realv.blank?
      continue unless k == blank.name?

      if fallback.type == blank.type # Inference will succeed
        Term.of(:row,
          render(k, styles, ""),
          Term[:frag, "⋮", width: 1, height: 1],
          Term[:padding, render(fallback, styles, postfix), pl: 1])
      elsif blank.type.any?
        Term.of(:row,
          render(k, styles, ""),
          Term[:frag, "_⋮", width: 2, height: 1],
          Term[:padding, render(fallback, styles, postfix), pl: 1])
      else
        continue
      end
    end

    matchpi %[(%'%- %'_ k_symbol)] do
      Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(k, styles, "_" + postfix))
    end

    matchpi %[(%'%- %'_number k_symbol)] do
      Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(k, styles, "_number" + postfix))
    end

    matchpi %[(%'%- %'_string k_symbol)] do
      Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(k, styles, "_string" + postfix))
    end

    matchpi %[(%'%- %'_symbol k_symbol)] do
      Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(k, styles, "_symbol" + postfix))
    end

    matchpi %[(%'%- %'_boolean k_symbol)] do
      Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(k, styles, "_boolean" + postfix))
    end

    matchpi %[(%'%- %'_dict k_symbol)] do
      Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(k, styles, "_dict" + postfix))
    end

    matchpi %[_symbol] do
      continue unless blank = v.blank?
      continue unless blank.name? == k

      render(k, styles, "#{blank.type.blank}" + postfix)
    end

    otherwise do
      Term.of(:row,
        render(k, styles, ""),
        Term[:frag, ":", width: 1, height: 1],
        Term[:padding, render(v, styles, postfix), pl: 1])
    end
  end
end

def render_pp_call_inline(dict, styles, postfix)
  Term::Dict.build do |commit|
    commit << :row
    dict.ee.each_with_index do |(k, v), index|
      if index == dict.size - 1
        commit << render_pp_pair(k, v, styles & DictStyles::Inline, postfix)
      else
        commit << render_pp_pair(k, v, styles & DictStyles::Inline, "")
      end
    end
    commit.with(:gap, 1)
  end
end

def render_pp_dict_multiline(dict, styles, postfix)
  Term::Dict.build do |commit|
    commit << :col
    dict.ee.each_with_index do |(k, v), index|
      if index == dict.size - 1
        commit << render_pp_pair(k, v, styles, postfix)
      else
        commit << render_pp_pair(k, v, styles, "")
      end
    end
  end
end

def render_pp_dict(dict, styles, postfix)
  Term[:choice,
    render_pp_call_inline(dict, styles, postfix),
    render_pp_dict_multiline(dict, styles, postfix)]
end

def render_pp(term : Term, style, postfix : String)
  Term.of_case(term) do
    matchpi %[(%'%layer below_ (¦ pp_dict))] do
      Term[:row, render(below, style, ""), render_pp(pp, style, postfix), gap: 1]
    end

    matchpi %[(¦ pp_dict)] do
      render_pp_dict(pp.unsafe_as_d, style, postfix)
    end

    otherwise do
      render(term, style, postfix)
    end
  end
end

# TODO: these should be organized into a chain, so that I can call chain.next and receive
# fallback handling instead of copy pasting stuff.
def render(term : Term, style = DictStyles::All, postfix : String = "")
  Term.of_case(term) do
    matchpi %[(backmap pattern_ backspec_)] do
      choice_a = Term[:row,
        Term[:info, render(pattern, style & DictStyles::Inline, ""), tag: :"backmap-pattern"],
        Term[:frag, "<>", width: 2, height: 1],
        render(backspec, style & DictStyles::Inline, postfix),
        gap: 1]

      choice_b = Term[:col,
        Term[:info, render(pattern, style & DictStyles::Inline, ""), tag: :"backmap-pattern"],
        Term[:padding,
          Term[:row,
            Term[:frag, "<>", width: 2, height: 1],
            render(backspec, style, postfix),
            gap: 1], pl: 2]]

      myself = DictStyles::Inline | DictStyles::CallColumn | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      choice_c = Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix))
      Term.of(:choice, choice_a, choice_b, choice_c)
    end

    matchpi %[(rule pattern_ body_)] do
      choice_a = Term[:row,
        render(pattern, style & DictStyles::Inline, ""),
        Term[:frag, "=>", width: 2, height: 1],
        render(body, style & DictStyles::Inline, postfix),
        gap: 1]

      choice_b = Term[:col,
        render(pattern, style & DictStyles::Inline, ""),
        Term[:padding,
          Term[:row,
            Term[:frag, "=>", width: 2, height: 1],
            render(body, style, postfix),
            gap: 1], pl: 2]]

      myself = DictStyles::Inline | DictStyles::CallColumn | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      choice_c = Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix))
      Term.of(:choice, choice_a, choice_b, choice_c)
    end

    matchpi %[(edge id←(%any° _symbol _number _string))] do
      string = "@#{id.inspect}#{postfix}"

      Term.of(:frag, string, width: string.size, height: 1)
    end

    matchpi %[($my capture_)] do
      Term.of(:row, Term[:frag, "→", width: 1, height: 1], render(capture, style, postfix))
    end

    matchpi %[($up capture_)] do
      Term.of(:row, Term[:frag, "↑", width: 1, height: 1], render(capture, style, postfix))
    end

    matchpi %[($down capture_)] do
      Term.of(:row, Term[:frag, "↓", width: 1, height: 1], render(capture, style, postfix))
    end

    matchpi %[(%'%let capture_ pattern_)] do
      Term.of(:row, render(capture, style, ""), Term[:frag, "←", width: 1, height: 1], render(pattern, style, postfix))
    end

    matchpi %[(%'%partition () %'_)] do
      string = "[]#{postfix}"

      Term.of(:frag, string, width: string.size, height: 1)
    end

    matchpi %[(%'%partition (itemspart_+) %'_)] do
      myself = DictStyles::Inline | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "[", width: 1, height: 1], thunk(itemspart, style & myself, style & children, "]" + postfix))
    end

    matchpi %[(%'%partition %'_ (%layer %'_ pp_dict))] do
      myself = DictStyles::Inline | DictStyles::CallColumn | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:choice,
        Term.of(:row, Term[:frag, "{_", width: 2, height: 1], Term[:padding, render_pp_dict(pp.unsafe_as_d, style, "}" + postfix), pl: 1]),
        Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix)))
    end

    matchpi %[(%'%partition (itemspart_+) pairspattern_)] do
      myself = DictStyles::Inline | DictStyles::CallColumn | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:choice,
        Term.of(:row, Term[:frag, "(", width: 1, height: 1],
          Term.of(:row,
            thunk(itemspart, style & DictStyles::Inline, style & children, ""),
            Term[:frag, "¦", width: 1, height: 1],
            render_pp(pairspattern, style & DictStyles::Inline, ")" + postfix),
            gap: 1)),
        Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix)))
    end

    matchpi %[(%'%item needles_+)] do
      myself = DictStyles::Inline | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "⟨", width: 1, height: 1], thunk(needles, style & myself, style & children, "⟩" + postfix))
    end

    matchpi %[(%'%item° needles_+)] do
      myself = DictStyles::Inline | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "⟨", width: 1, height: 1], thunk(needles, style & myself, style & children, "⟩°" + postfix))
    end

    matchpi %[(%'%slot capture_)] do
      Term.of(:row, Term[:frag, "⏏", width: 1, height: 1], render(capture, style, postfix))
    end

    matchpi %['value_] do
      Term.of(:row, Term[:frag, "'", width: 1, height: 1], render(value, style, postfix))
    end

    matchpi %[(%'%nonself value_)] do
      Term.of(:row, Term[:frag, "≡", width: 1, height: 1], render(value, style, postfix))
    end

    matchpi %[(%'%literal value_)] do
      Term.of(:row, Term[:frag, "%'", width: 2, height: 1], render(value, style, postfix))
    end

    matchpi %[()] do
      string = "()#{postfix}"

      Term.of(:frag, string, width: string.size, height: 1)
    end

    matchpi %[(¦ pairspart_)] do
      myself = DictStyles::MapInline | DictStyles::MapMultiline
      children = DictStyles::All

      Term.of(:row, Term[:frag, "{", width: 1, height: 1], thunk(term, style & myself, style & children, "}" + postfix))
    end

    matchpi %[(_symbol _ ¦ (%not ()))] do
      myself = DictStyles::Inline | DictStyles::BlockCallKeywordsInline | DictStyles::BlockCallKeywordsColumn | DictStyles::KeywordBlockCall | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix))
    end

    matchpi %[(_symbol _+)] do
      myself = DictStyles::Inline | DictStyles::CallColumn | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix))
    end

    matchpi %{(_symbol _+ ¦ (%not {}))} do
      myself = DictStyles::Inline | DictStyles::BlockCallKeywordsInline | DictStyles::BlockCallKeywordsColumn | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix))
    end

    matchpi %{[_symbol _*]} do
      myself = DictStyles::Inline | DictStyles::CallIndented | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix))
    end

    matchpi %{_dict} do
      myself = DictStyles::Inline | DictStyles::CallAligned
      children = DictStyles::All

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style & children, ")" + postfix))
    end

    matchpi %[_number], %[_symbol], %[_string], %[_boolean] do
      string = "#{term.inspect}#{postfix}"

      Term.of(:frag, string, width: string.size, height: 1)
    end
  end
end

def render(subject : Term::Dict, style : DictStyles, styles : DictStyles, postfix : String)
  case style
  when .dict_inline?
    Term.of(render_dict_inline(subject, styles, postfix))
  when .call_column?
    Term.of(render_call_column(subject, styles, postfix))
  when .block_call_keywords_inline?
    Term.of(render_call_keywords_inline(subject, styles, postfix))
  when .block_call_keywords_column?
    Term.of(render_call_keywords_column(subject, styles, postfix))
  when .keyword_block_call?
    Term.of(render_keyword_block_call(subject, styles, postfix))
  when .call_indented?
    Term.of(render_multiline(subject, styles, postfix, padded: true))
  when .call_aligned?
    Term.of(render_multiline(subject, styles, postfix, padded: false))
  when .map_inline?
    Term.of(render_map_inline(subject, styles, postfix))
  when .map_multiline?
    Term.of(render_map_multiline(subject, styles, postfix))
  else
    raise ArgumentError.new
  end
end

class StyleNotApplicable < Exception
  @callstack = Exception::CallStack.empty
end

def measure(ctx : DisplayContext, node : Term) : {Int32, Int32}
  ctx.measurements.put_if_absent(node) do
    Term.case(node) do
      matchpi(
        %{(frag _ ¦ _ width_: (%number +i32) height_: (%number +i32))},
        %{(block _ ¦ _ width_: (%number +i32) height_: (%number +i32))}
      ) do
        {width.to(Int32), height.to(Int32)}
      end

      matchpi %{[info child_]} do
        measure(ctx, child)
      end

      matchpi %{(padding child_ ¦ pl_: (%number +i32))} do
        child_width, child_height = measure(ctx, child)

        {child_width + pl.to(Int32), child_height}
      end

      matchpi %{[row]} do
        {0, 0}
      end

      matchpi %[(row children_* ¦ gap: (%optional 0 gap←(%number +i32)))] do
        width = height = 0

        children.items.each_with_index do |child, index|
          child_width, child_height = measure(ctx, child)

          width += gap.to(Int32) if index > 0
          width += child_width
          height = Math.max(height, child_height)
        end

        {width, height}
      end

      matchpi %{[col]} do
        {0, 0}
      end

      matchpi %[(col children_* ¦ gap: (%optional 0 gap←(%number +i32)))] do
        width = height = 0

        children.items.each_with_index do |child, index|
          child_width, child_height = measure(ctx, child)

          height += gap.to(Int32) if index > 0
          height += child_height
          width = Math.max(width, child_width)
        end

        {width, height}
      end
    end
  end
end

record DisplayContext, normal_maxwidth : Int32, backmap_pattern_maxwidth : Int32, styles : DictStyles, measurements = {} of Term => {Int32, Int32} do
  def shrink(amount : Int32) : DisplayContext
    copy_with(normal_maxwidth: normal_maxwidth - amount, backmap_pattern_maxwidth: backmap_pattern_maxwidth - amount)
  end
end

def flatten(ctx, node : Term)
  Term.of_case(node) do
    matchpi %{(frag _ ¦ _ width_: (%number +i32))} do
      if ctx.normal_maxwidth < width.to(Int32)
        raise StyleNotApplicable.new
      end
      node
    end

    matchpi %{[block _]} do
      node
    end

    matchpi %{(info child_ tag: backmap-pattern)} do
      flatten(ctx.copy_with(normal_maxwidth: ctx.backmap_pattern_maxwidth), child)
    end

    matchpi %{[info child_]} do
      flatten(ctx, child)
    end

    matchpi %[(padding child_ ¦ pl_: (%number +i32))] do
      Term.of(:padding, flatten(ctx.shrink(pl.to(Int32)), child), pl: pl)
    end

    matchpi %{(row children_* ¦ gap: (%optional 0 gap←(%number +i32)))} do
      node.pairspart.transaction do |commit|
        width = 0

        commit << :row
        children.items.each_with_index do |child, index|
          width += gap.to(Int32) if index > 0
          flattened = flatten(ctx.shrink(width), child)
          commit << flattened
          child_width, _ = measure(ctx, flattened)
          width += child_width
        end
      end
    end

    matchpi %{[col children_*]} do
      node.pairspart.transaction do |commit|
        commit << :col
        commit.concat(children.items) { |child| flatten(ctx, child) }
      end
    end

    matchpi %[(thunk subject_dict postfix_string myself style_options_self_string* children style_options_string*)] do
      options = DictStyles::None
      style_options.items.each do |style_id|
        options |= DictStyles.parse(style_id.to(String))
      end
      options &= ctx.styles

      options_self = DictStyles::None
      style_options_self.items.each do |style_id|
        options_self |= DictStyles.parse(style_id.to(String))
      end
      options_self &= ctx.styles

      options_self.each do |option|
        return flatten(ctx.copy_with(styles: options), render(subject.unsafe_as_d, option, options, postfix.to(String)))
      rescue StyleNotApplicable
      end

      raise StyleNotApplicable.new
    end

    matchpi %[(choice choices_+)] do
      choices.items.each do |choice|
        return flatten(ctx, choice)
      rescue StyleNotApplicable
      end

      raise StyleNotApplicable.new
    end
  end
end

def draw(ctx, screen, node : Term, x, y)
  Term.case(node) do
    matchpi %{[frag chars_string]} do
      chars.to(String).each_char do |char|
        screen.put(x, y, char)
        x += 1
      end
    end

    matchpi %{[block term_]} do
      term.inspect.each_char do |char|
        screen.put(x, y, char)
        x += 1
      end
    end

    matchpi %[(row children_* ¦ _ gap: (%optional 0 gap←(%number +i32)))] do
      children.items.each_with_index do |child, index|
        x += gap.to(Int32) if index > 0
        draw(ctx, screen, child, x, y)
        child_width, _ = measure(ctx, child)
        x += child_width
      end
    end

    matchpi %[(col children_* ¦ _ gap: (%optional 0 gap←(%number +i32)))] do
      children.items.each_with_index do |child, index|
        y += gap.to(Int32) if index > 0
        draw(ctx, screen, child, x, y)
        _, child_height = measure(ctx, child)
        y += child_height
      end
    end

    matchpi %[(padding child_ ¦ _ pl_: (%number +i32))] do
      draw(ctx, screen, child, x + pl.to(Int32), y)
    end
  end
end

# ?!?!?!?!?!
class Screen
  def initialize
    @cells = Hash({Int32, Int32}, Char).new
    @max_x = 0
    @max_y = 0
  end

  def clear : Nil
    @max_x = @max_y = 0
    @cells.clear
  end

  def put(x, y, ch : Char) : Nil
    @cells[{x, y}] = ch
    @max_x = Math.max(@max_x, x)
    @max_y = Math.max(@max_y, y)
  end

  def write(io : IO)
    (0..@max_y).each do |y|
      (0..@max_x).each do |x|
        char = @cells[{x, y}]? || ' '
        io << char
      end
      io.puts
    end
  end

  def string : String
    String.build(@max_x * @max_y) do |io|
      write(io)
    end
  end
end

# Some problems:
#   13. Handle toplevel StyleNotApplicable
#        Same behavior as in Python, "If a long object cannot be split, the specified width will be exceeded."
#   18. Key order in pairs and pairspart partition!!!!! Sort non-dict keys or singleton of non-dict keys
#       in lexicographic order. Dict keys go at the end, sorted by hash. This means we'll finally need to
#       switch the hash function to a stable one!
#   16. Backmap and rule formatting must be toplevel.
#   14. Dict set syntax && pretty printing
#   21. Partition pairspart multiline style.
#   23. Python's underscore_numbers: format thousands with _.
#   15. Have a way to toggle features/shorthands on or off in the context.
#   12. Use return value instead of StyleNotApplicable exception.
#   20. Line breaking in long strings (and the syntax that this requires!).
#   22. Have a mode for pretty printing where we remember where newlines and comments
#       were put by the user in the input string between **toplevel** terms; put them back
#       when pretty printing. Handle comments as well -- somehow ?! This will pave the way to using
#       this pretty printer as a code formatter.
#
#   TESTS??????!
ed = ML.terms(File.read("./editor.soma.wwml"))# Term.of(:+, {:*, 3, 4}, {2})
# ed = ML.terms(%{{_ x⋮ 100 y: 200 -z_}})

str = String.build do |io|
  screen = Screen.new

  ed.items.each do |sexp|
    screen.clear
    tree = render(sexp)
    ctx = DisplayContext.new(normal_maxwidth: 60, backmap_pattern_maxwidth: 120, styles: DictStyles::All)
    flat = flatten(ctx, tree)
    draw(ctx, screen, flat, 0, 0)
    screen.write(io)
    io.puts
  end
end

puts str
pp ed == ML.terms(str)

# [x] x_: 100 => x: (%let x 100)
# [x] x⋮ 100 => x: (%optional 100 x_number) ;; infers type
# [x] x_⋮ 100 => x: (%optional 100 x_) ;; does not infer type
# [x] x_string⋮ 100 ;; invalid. Either infer or any-blank
# [x] ¦ ... w_ ... => ... w: w_ ...
# [x] ¦ ... w_number ... => ... w: w_number ...
# [x] -x_ => (%- _ x)
# [x] -x_number => (%- _number x)
