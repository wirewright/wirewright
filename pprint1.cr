require "./wirewright"
require "./baz5"

module OrdDict
  # :nodoc:
  struct Unsorted
    include Indexable({Term, Term})

    def initialize(@dict : Term::Dict)
    end

    def unsafe_fetch(index)
      @dict.ordnth(index)
    end

    def size
      @dict.size
    end
  end

  # :nodoc:
  struct Sorted
    include Indexable({Term, Term})

    def initialize(@itemspart : Term::Dict, @ppsorted : Slice({Term, Term}), @pprest : Slice({Term, Term})?)
    end

    def unsafe_fetch(index)
      if index < @itemspart.size
        return Term.of(index), @itemspart[index]
      end

      index &-= @itemspart.size

      if index < @ppsorted.size
        return @ppsorted.unsafe_fetch(index)
      end

      index &-= @ppsorted.size

      @pprest.not_nil!.unsafe_fetch(index)
    end

    def size
      @itemspart.size + @ppsorted.size + (@pprest.try(&.size) || 0)
    end
  end

  # Returns a sorted indexable of *dict*'s entries.
  #
  # - Items are ordered by their index.
  # - Pairs with literal keys and singleton literal keys (e.g. `(x)`) are sorted by
  #   those keys lexicographically.
  # - Non-singleton dicts are ordered by their hash and are put after all sorted pairs.
  def self.sorted(dict : Term::Dict) : Indexable({Term, Term})
    if dict.itemsonly?
      return Unsorted.new(dict.itemspart)
    end

    if dict.pairsonly? && dict.size == 1
      return Unsorted.new(dict)
    end

    # Split pairs into sortable and unsortable ones.
    pairs_sortable = [] of {Term, Term}
    pairs_trailing = nil

    dict.each_pair do |key, value|
      if key.type.dict? && !(key.itemsonly? && key.size == 1 && !key[0].type.dict?)
        pairs_trailing ||= [] of {Term, Term}
        pairs_trailing << {key, value}
      else
        pairs_sortable << {key, value}
      end
    end

    # Sort the sortable pairs.
    #
    # TODO: Right now we do this naively, by sorting their .inspect()s. This works
    # for all literals and singleton dicts containing literals, but is obviously
    # really, really slow.
    pairs_sortable.sort_by! { |key, _| key.inspect }

    # Return as an indexable that is smart enough to point into the
    # appropriate array.
    Sorted.new(dict.itemspart,
      ppsorted: pairs_sortable.to_readonly_slice,
      pprest: pairs_trailing ? pairs_trailing.to_readonly_slice : nil,
    )
  end

  # Returns an unsorted indexable of *dict*'s entries.
  #
  # - Items are ordered by their index.
  # - Pairs are ordered by their hash.
  def self.unsorted(dict : Term::Dict) : Indexable({Term, Term})
    Unsorted.new(dict)
  end
end

# Order is important. 0 means most preferred. Max means least preferred.
@[Flags]
enum StyleSet : UInt16
  DictInline
  BlockCallKeywordsInline
  BlockCallKeywordsColumn
  KeywordBlockCall
  CallColumn
  MapInline
  MapMultiline
  CallIndented
  DictAligned
end

InlineStyles = StyleSet::MapInline | StyleSet::DictInline

module Style
  extend self

  # ```wwml
  # (text "Hello World 1" "Hello World 2" "Hello World 3" x: 100 y: 200)
  # ```
  DictInline = Spec.parse <<-WWML
    (style min-entries: 1
      (row gap: 1
        ($slot entries 0 -2 inline "")
        ($slot entries -2 -1 inline postfix)))
  WWML

  # ```wwml
  # (text
  #  "Hello World 1"
  #  "Hello World 2"
  #  "Hello World 3"
  #  x: 100
  #  y: 200)
  # ```
  DictAligned = Spec.parse <<-WWML
    (style min-entries: 1
      (col ($slot entries 0 1 * "")
           ($slot entries 1 -2 * "")
           ($slot entries -2 -1 * postfix)))
  WWML

  # ```wwml
  # (text
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3"
  #   x: 100
  #   y: 200)
  # ```
  CallIndented = Spec.parse <<-WWML
    (style min-entries: 2
      (col ($slot entries 0 1 inline "")
           (indented
             (col ($slot entries 1 -2 * "")
                  ($slot entries -2 -1 * postfix)))))
  WWML

  # ```wwml
  # (text "Hello World 1"
  #       "Hello World 2"
  #       "Hello World 3")
  # ```
  CallColumn = Spec.parse <<-WWML
    (style itemsonly: true min-items: 2
      (row gap: 1
        ($slot items 0 1 inline "")
        (col ($slot items 1 -2 inline "")
             ($slot items -2 -1 inline postfix))))
  WWML

  # ```wwml
  # (text x: 100 y: 200
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3")
  # ```
  BlockCallKeywordsInline = Spec.parse <<-WWML
    (style min-items: 2 min-pairs: 1
      (col (row gap: 1
             ($slot items 0 1 inline "")
             (col ($slot pairs 0 -1 inline "")))
           (indented
             (col ($slot items 1 -2 * "")
                  ($slot items -2 -1 * postfix)))))
  WWML

  # ```wwml
  # (text x: 100
  #       y: 200
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3")
  # ```
  BlockCallKeywordsColumn = Spec.parse <<-WWML
    (style min-items: 2 min-pairs: 1
      (col (row gap: 1
             ($slot items 0 1 inline "")
             (col ($slot pairs 0 -1 * "")))
           (indented
             (col ($slot items 1 -2 * "")
                  ($slot items -2 -1 * postfix)))))
  WWML

  # ```wwml
  # (text "Hello World 1"
  #   x: 100
  #   y: 200)
  # ```
  KeywordBlockCall = Spec.parse <<-WWML
    (style min-items: 3 min-pairs: 1
      (col (row gap: 1
             ($slot items 0 1 inline "")
             ($slot items 1 2 inline ""))
           (indented
             (col ($slot pairs 0 -2 * "")
                  ($slot pairs -2 -1 * postfix)))))
  WWML

  # ```wwml
  # {x: 100, y: 200, z: 300}
  # ```
  MapInline = Spec.parse <<-WWML
    (style pairsonly: true min-pairs: 1
      (row gap: 1
        ($slot pairs 0 -2 inline ",")
        ($slot pairs -2 -1 inline postfix)))
  WWML

  # ```wwml
  # {x: 100,
  #  y: 200,
  #  z: 300}
  # ```
  MapMultiline = Spec.parse <<-WWML
    (style pairsonly: trie min-pairs: 1
      (col ($slot pairs 0 -2 * ",")
           ($slot pairs -2 -1 * postfix)))
  WWML

  # Converts a single-element *styleset* into the corresponding `Style` object.
  # Raises `ArgumentError` if *styleset* is not a single-element set.
  def one(styleset : StyleSet)
    {% for style in StyleSet.constants %}
      {% unless {:None, :All}.includes?(style.id.symbolize) %}
        if styleset == StyleSet::{{style}}
          return {{style}}
        end
      {% end %}
    {% end %}

    raise ArgumentError.new
  end
end

# Style specs are a declarative way to describe dict styles.
struct Style::Spec
  # :nodoc:
  def initialize(@template : Term, @min_items : Int32, @min_pairs : Int32, @min_entries : Int32, @itemsonly : Bool, @pairsonly : Bool)
  end

  # Constructs a style spec from the given spec term *spec*.
  def self.new(spec : Term) : Spec
    Term.case(spec) do
      matchpi %[(style template_ ¦ min-pairs⋮ 0 min-items⋮ 0 min-entries⋮ 0 itemsonly⋮ false pairsonly⋮ false)] do
        new(template, min_items.to(Int32), min_pairs.to(Int32), min_entries.to(Int32), itemsonly.to(Bool), pairsonly.to(Bool))
      end
    end
  end

  # Parses spec WwML.
  def self.parse(ml : String) : Spec
    new(ML.term(ml))
  end

  # Returns `true` if *dict* satisfies all constraints set by this spec.
  private def applicable_to?(dict : Term::Dict) : Bool
    return false if @itemsonly && !dict.itemsonly?
    return false if @pairsonly && !dict.pairsonly?

    dict.itemsize >= @min_items && dict.pairsize >= @min_pairs && (dict.size - @min_items - @min_pairs) >= @min_entries
  end

  # FIXME: when multiple slots have overlapping ranges since the underlying dict
  # is too small, what should we do?
  private def translate(range, size : Int32)
    return 0, 0 if size.zero?

    if range.begin.negative?
      b = range.begin + size + 1
    else
      b = range.begin
    end

    if range.end.negative?
      e = range.end + size + 1
    else
      e = range.end
    end

    {b, e - b}
  end

  # Renders *dict* using this style. Returns `nil` if one of the constraints of this
  # style was not satisfied; in other words, if this style is not applicable to *dict*.
  def render?(ctx, dict : Term::Dict, styleset, postfix) : Term?
    return unless applicable_to?(dict)

    if ctx.features.sorted_pairs?
      entries = OrdDict.sorted(dict)
    else
      entries = OrdDict.unsorted(dict)
    end

    handler = ->(term : Term) do
      Term.case(term) do
        matchpi(
          %{($slot region←(%any items pairs entries)
                        b←(%number i8)
                        e←(%number i8)
                  allowed←(%any * inline)
                     sink←(%any° _string postfix))}
        ) do
          # Go from relative to concrete start index and count into the dictionary
          # we have.
          case region
          when Term.of(:items)
            start, count = translate(b.to(Int32)...e.to(Int32), dict.itemsize)
          when Term.of(:pairs)
            start, count = translate(b.to(Int32)...e.to(Int32), dict.pairsize)
            start += dict.itemsize
          when Term.of(:entries)
            start, count = translate(b.to(Int32)...e.to(Int32), dict.size)
          else
            unreachable
          end

          # Intersect with allowed style(s).
          case allowed
          when Term.of(:*)
          when Term.of(:inline)
            styleset &= InlineStyles
          else
            unreachable
          end

          # Determine whether to propagate postfix or use a hard-coded one.
          subpostfix = sink.type.string? ? sink.to(String) : postfix

          case count
          when 0
            Rewrite.many(Term[])
          when 1
            entry = render_entry(ctx, dict, *entries[start], styleset, subpostfix)

            Rewrite.one(entry)
          else
            offspring = Term::Dict.build do |commit|
              count.times do |offset|
                entry = render_entry(ctx, dict, *entries[start + offset], styleset, subpostfix)
                commit << entry
              end
            end

            Rewrite.many(offspring)
          end
        end

        otherwise { Rewrite.none }
      end
    end

    rewrite(@template, itemdfsR(callR(handler)))
  end
end

module Display
  # Lists the kinds of extra syntax features (shorthands etc.) the renderer can emit.
  #
  # You can enable/disable them individually or in bulk (e.g. `None`, `All`, `Pairspart`, `Pattern`).
  #
  # The most "minimal" setting is `None`, allowing only bare bones `(+ x y a: ... b: ...)`
  # syntax. It is useful if e.g. there is an unsophisticated parser on the receiving end.
  @[Flags]
  enum Features : UInt32
    # Render dictionary pairs sorted lexicographically. See also: `OrdDict.sorted`.
    SortedPairs

    # Render `x: (%let x ...)` as `x_: ...` in the pairspart partition.
    PairspartLet

    # Render `x: (%optional 100 x_)` as `x_⋮ 100`, `x: (%optional 100 x_number)` as
    # `x⋮ 0 in the pairspart partition`.
    PairspartOptional

    # Render `x: (%- _ x)` as `-x_`, `x: (%- _number x)` as `-x_number` (and so on
    # for other types) in the pairspart partition.
    PairspartNegation

    # Render `x: x_` as `x_`, `x: x_number` as `x_number` in the pairspart partition.
    PairspartBlank

    # Render `(%layer _ ...)` pairs pattern as `_ ...` in the pairspart partition (after `¦`).
    # Enables the rendering of pairspart members (see `PairspartMember`).
    PairspartOpen

    # Render pairsonly dict pairs pattern `{...}` as `...` in the pairspart partition (after `¦`).
    # Enables the rendering of pairspart members (see `PairspartMember`).
    PairspartClosed

    # Render `(%partition (...) pp_)` as `(... ¦ pp_)`. Gives way to `PairspartOpen`
    # and `PairspartClosed`.
    PairspartSplit

    # Render `(%partition (...) _)` as `[...]`.
    JustItemspartBrackets

    # Render `(%partition _ (%layer _ {...}))` as `{_ ...}`.
    JustOpenPairspartBrackets

    # Render toplevel `(rule <pattern> <template>)` as `<pattern> => <template>`.
    ToplevelRule

    # Render toplevel `(backmap <pattern> <backspec>)` as `<pattern> <> <backspec>`.
    ToplevelBackmap

    # Render `(edge x)` as `@x`.
    Edge

    # Render `($my x)` as `→x`.
    BackrefMy

    # Render `($up x)` as `↑x`.
    BackrefUp

    # Render `($down x)` as `↓x`.
    BackrefDown

    # Render `(%let x <pattern>) as `x←<pattern>`
    LetArrow

    # Render `(%item ...)` as `⟨...⟩`.
    ItemFirstBrackets

    # Render `(%item° ...)` as `⟨...⟩°`.
    ItemSourceBrackets

    # Render `(%slot x)` as `⏏x`.
    Slot

    # Render `(hold x)` as `'x`.
    Hold

    # Render `(%literal x)` as `%'x`.
    Literal

    # Render `(%nonself x)` as `≡x`.
    Nonself

    # Render `(x: a y: b)` as `{x: a, y: b}`.
    PairsonlyBrackets

    # Separate thousands in integers using `_`: renders `100000` as `100_000` etc.
    GroupThousands

    # Dicts whose first item is a symbol are trated as "calls" to which additional
    # `Style`s can apply. If disabled, only `Style::DictAligned` is used to
    # render dictionaries.
    Call
  end

  # Groups features related to backreferences.
  BackrefFeatures = Features::BackrefMy \
                  | Features::BackrefUp \
                  | Features::BackrefDown

  # Groups members of the dict pairspart syntax (e.g. `x⋮ 0`).
  PairspartMemberFeatures = Features::PairspartLet \
                          | Features::PairspartOptional \
                          | Features::PairspartNegation \
                          | Features::PairspartBlank

  # Groups features related to the dict pairspart syntax (`(... ¦ <pairspart>)`).
  PairspartFeatures = Features::PairspartSplit \
                    | Features::PairspartOpen \
                    | Features::PairspartClosed \
                    | Features::PairspartMember

  # Groups features related to patterns.
  PatternFeatures = Features::Pairspart \
                  | Features::JustItemspartBrackets \
                  | Features::JustOpenPairspartBrackets \
                  | Features::LetArrow \
                  | Features::ItemFirstBrackets \
                  | Features::ItemSourceBrackets \
                  | Features::Literal \
                  | Features::Nonself
end

# Generates a proc that fills compile-time known blanks in a WwML template *ml*.
#
# WARNING: you should enclose this macro in something that won't make the variables
# in it leak outside. Ideally, you should use it as the value of a constant.
macro fillable(ml)
  {% captures = ml.scan(/([a-z_]+)_/).map { |(_, capture)| capture } %}

  {% for capture in captures %}
    {{capture.id}} = nil
  {% end %}

  template = ML.term({{ml}})

  Term.each_keypath_and_leaf(template) do |keypath, leaf|
    {% for capture in captures %}
      if leaf.type.symbol? && (blank = leaf.unsafe_as_sym.blank?) && Term[{{capture.id.symbolize}}] == blank.name?
        {{capture.id}} = keypath.to_readonly_slice(&.itself)
      end
    {% end %}

    true
  end

  ->({% for _, index in captures %} v{{index}} : Term, {% end %}) do
    {% for capture, index in captures %}
      kp{{index}} = {{capture.id}}

      template = template.as_d.follow(kp{{index}} || raise ArgumentError.new) { v{{index}} }
    {% end %}

    template
  end
end

module Templates
  MY = fillable(%{(row (frag "→") capture_)})
  UP = fillable(%{(row (frag "↑") capture_)})
  DOWN = fillable(%{(row (frag "↓") capture_)})

  BACKMAP = fillable <<-WWML
  (choice
    (row gap: 1
      pattern_short_ (frag "<>") backspec_short_)
    (col (longer pattern_long_)
      (indented by: 2
        (row gap: 1
          (frag "<>") backspec_long_)))
    fallback_)
  WWML

  RULE = fillable <<-WWML
  (choice
    (row gap: 1
      pattern_short_ (frag "=>") body_short_)
    (col (longer pattern_long_)
      (indented by: 2
        (row gap: 1
          (frag "=>") body_long_)))
    fallback_)
  WWML

  LET_ARROW = fillable(%{(row capture_ (frag "←") pattern_)})

  JUST_ITEMSPART_LBRACKET = fillable(%{(row (frag "[") rest_)})

  ITEM_LBRACKET = fillable(%{(row (frag "⟨") rest_)})

  PAIRSONLY_LBRACKET = fillable(%[(row (frag "{") rest_)])
  LPAREN = fillable(%{(row (frag "(") rest_)})

  SLOT = fillable(%{(row (frag "⏏") rest_)})
  HOLD = fillable(%{(row (frag "'") rest_)})
  NONSELF = fillable(%{(row (frag "≡") rest_)})
  LITERAL = fillable(%{(row (frag "%'") rest_)})

  PAIRSPART_LET = fillable(%{(row k_ (frag "_:") (indented v_))})

  PAIRSPART_OPTIONAL_INFER = fillable(%{(row k_ (frag "⋮") (indented v_))})
  PAIRSPART_OPTIONAL_ANY = fillable(%{(row k_ (frag "_⋮") (indented v_))})

  PAIRSPART_NEGATION = fillable(%{(row (frag "-") k_)})
end

def thunk(subject : Term, myself : StyleSet, children : StyleSet, postfix : String)
  Term.of(:thunk, subject, postfix, myself.value, children.value)
end

def render_item(ctx, v, styles, postfix)
  render(ctx, v, styles, postfix)
end

# TODO: multiline variant
def render_pair(ctx, k, v, styles, postfix)
  Term.of(:row,
    render(ctx, k, styles, ""),
    Term[:frag, ":"],
    Term[:indented, render(ctx, v, styles, postfix)])
end

def render_entry(ctx, dict, k, v, styles, postfix)
  if dict.index?(k)
    render_item(ctx, v, styles, postfix)
  else
    render_pair(ctx, k, v, styles, postfix)
  end
end

def render_pp_pair(ctx, k, v, styles, postfix)
  Term.of_case(v, env: Term[k: k]) do
    if ctx.features.pairspart_let?
      matchpi %[(%'%let k_symbol realv_)] do
        Templates::PAIRSPART_LET.call(render(ctx, k, styles, ""), render(ctx, realv, styles, postfix))
      end
    end

    if ctx.features.pairspart_optional?
      matchpi %[(%'%optional fallback_ realv_symbol)] do
        continue unless blank = realv.blank?
        continue unless k == blank.name?

        if fallback.type == blank.type # Inference will succeed
          Templates::PAIRSPART_OPTIONAL_INFER.call(render(ctx, k, styles, ""), render(ctx, fallback, styles, postfix))
        elsif blank.type.any?
          Templates::PAIRSPART_OPTIONAL_ANY.call(render(ctx, k, styles, ""), render(ctx, fallback, styles, postfix))
        else
          continue
        end
      end
    end

    if ctx.features.pairspart_negation?
      matchpi %[(%'%- %'_ k_symbol)] do
        Templates::PAIRSPART_NEGATION.call(render(ctx, k, styles, "_" + postfix))
      end

      matchpi %[(%'%- %'_number k_symbol)] do
        Templates::PAIRSPART_NEGATION.call(render(ctx, k, styles, "_number" + postfix))
      end

      matchpi %[(%'%- %'_string k_symbol)] do
        Templates::PAIRSPART_NEGATION.call(render(ctx, k, styles, "_string" + postfix))
      end

      matchpi %[(%'%- %'_symbol k_symbol)] do
        Templates::PAIRSPART_NEGATION.call(render(ctx, k, styles, "_symbol" + postfix))
      end

      matchpi %[(%'%- %'_boolean k_symbol)] do
        Templates::PAIRSPART_NEGATION.call(render(ctx, k, styles, "_boolean" + postfix))
      end

      matchpi %[(%'%- %'_dict k_symbol)] do
        Templates::PAIRSPART_NEGATION.call(render(ctx, k, styles, "_dict" + postfix))
      end
    end

    if ctx.features.pairspart_blank?
      matchpi %[_symbol] do
        continue unless blank = v.blank?
        continue unless blank.name? == k

        render(ctx, k, styles, "#{blank.type.blank}" + postfix)
      end
    end

    otherwise do
      render_pair(ctx, k, v, styles, postfix)
    end
  end
end

def render_pp_call_inline(ctx, dict, styles, postfix)
  if ctx.features.sorted_pairs?
    entries = OrdDict.sorted(dict)
  else
    entries = OrdDict.unsorted(dict)
  end

  Term::Dict.build do |commit|
    commit << :row
    entries.each_with_index do |(k, v), index|
      if index == dict.size - 1
        commit << render_pp_pair(ctx, k, v, styles & InlineStyles, postfix)
      else
        commit << render_pp_pair(ctx, k, v, styles & InlineStyles, "")
      end
    end
    commit.with(:gap, 1)
  end
end

def render_pp_dict_multiline(ctx, dict, styles, postfix)
  if ctx.features.sorted_pairs?
    entries = OrdDict.sorted(dict)
  else
    entries = OrdDict.unsorted(dict)
  end

  Term::Dict.build do |commit|
    commit << :col
    entries.each_with_index do |(k, v), index|
      if index == dict.size - 1
        commit << render_pp_pair(ctx, k, v, styles, postfix)
      else
        commit << render_pp_pair(ctx, k, v, styles, "")
      end
    end
  end
end

def render_pp_dict(ctx, dict, styles, postfix)
  Term[:choice,
    render_pp_call_inline(ctx, dict, styles, postfix),
    render_pp_dict_multiline(ctx, dict, styles, postfix)]
end

def render_pp(ctx, term : Term, style, postfix : String)
  Term.of_case(term) do
    if ctx.features.pairspart_open?
      matchpi %[(%'%layer below_ (¦ pp_dict))] do
        Term[:row, render(ctx, below, style, ""), render_pp(ctx, pp, style, postfix), gap: 1]
      end
    end

    if ctx.features.pairspart_closed?
      matchpi %[(¦ pp_dict)] do
        render_pp_dict(ctx, pp.unsafe_as_d, style, postfix)
      end
    end

    otherwise do
      render(ctx, term, style, postfix)
    end
  end
end

# TODO: these should be organized into a chain, so that I can call chain.next and receive
# fallback handling instead of copy pasting stuff.
#   FeatureChain
# TODO: note how all of these are basically
#    feature -> pattern -> template -/-> successor.
#        feature : (pattern, template, successor)
#          checks if it is enabled
#          if it is, it checks if the term matches the feature's pattern. otherwise calls successor.
#          if it does, uses the template. otherwise calls successor.
#   Note how template.calls all have render()s in them. BUT some of them change style / postfix.
#    AND some of them are thunks.
def render(ctx, term : Term, style = StyleSet::All, postfix : String = "")
  Term.of_case(term) do
    # Humph?! This should actually be TOPLEVEL!
    if ctx.features.toplevel_backmap?
      # (feature (backmap pattern_ backspec_)
      #   (choice
      #     (row gap: 1
      #       ($embed pattern inline "")
      #       (atom "<>")
      #       ($embed backspec inline postfix))
      #     (col (longer ($embed pattern inline ""))
      #       (indented by: 2
      #         (row gap: 1
      #           (atom "<>")
      #           ($embed backspec * postfix))))
      #     $next))
      matchpi %[(backmap pattern_ backspec_)] do
        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned
        fallback = Templates::LPAREN.call(thunk(term, style & myself, style, ")" + postfix))

        pattern_inline = render(ctx, pattern, style & InlineStyles, "")

        Templates::BACKMAP.call(
          pattern_inline,
          render(ctx, backspec, style & InlineStyles, postfix),
          pattern_inline,
          render(ctx, backspec, style, postfix),
          fallback,
        )
      end
    end

    # Humph?! This should actually be TOPLEVEL!
    if ctx.features.toplevel_rule?
      # (feature (rule pattern_ body_)
      #   (choice
      #     (row gap: 1
      #       ($embed pattern inline "")
      #       (atom "=>")
      #       ($embed body inline postfix))
      #     (col (longer ($embed pattern inline ""))
      #       (indented by: 2
      #         (row gap: 1
      #           (atom "=>")
      #           ($embed body * postfix))))
      #     $next))
      matchpi %[(rule pattern_ body_)] do
        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned
        fallback = Templates::LPAREN.call(thunk(term, style & myself, style, ")" + postfix))

        pattern_inline = render(ctx, pattern, style & InlineStyles, "")

        Templates::RULE.call(
          pattern_inline,
          render(ctx, body, style & InlineStyles, postfix),
          pattern_inline,
          render(ctx, body, style, postfix),
          fallback,
        )
      end
    end

    if ctx.features.edge?
      # (feature (edge id←(%any° _symbol _number _string))
      #   (row (atom "@") (atom ($string id)) $postfix))
      matchpi %[(edge id←(%any° _symbol _number _string))] do
        string = "@#{id.inspect}#{postfix}"

        Term.of(:frag, string)
      end
    end

    if ctx.features.backref_my?
      # (feature ($my capture_)
      #   (row (atom "→") ($embed capture * postfix)))
      matchpi %[($my capture_)] do
        Templates::MY.call(render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.backref_up?
      # (feature ($up capture_)
      #   (row (atom "↑") ($embed capture * postfix)))
      matchpi %[($up capture_)] do
        Templates::UP.call(render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.backref_down?
      # (feature ($down capture_)
      #   (row (atom "↓") ($embed capture * postfix)))
      matchpi %[($down capture_)] do
        Templates::DOWN.call(render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.let_arrow?
      # (feature (%'%let capture_ pattern_)
      #   (row ($embed capture inline "") (atom "←") ($embed pattern * postfix)))
      matchpi %[(%'%let capture_ pattern_)] do
        Templates::LET_ARROW.call(render(ctx, capture, style, ""), render(ctx, pattern, style, postfix))
      end
    end

    if ctx.features.just_itemspart_brackets?
      # (feature (%'%partition () %'_)
      #   (row (atom "[]") $postfix))
      matchpi %[(%'%partition () %'_)] do
        string = "[]#{postfix}"

        Term.of(:frag, string)
      end

      # (feature (%'%partition (itemspart_+) %'_)
      #   (row (atom "[") ($thunk itemspart (inline dict-aligned) "]" postfix)))
      matchpi %[(%'%partition (itemspart_+) %'_)] do
        myself = InlineStyles | StyleSet::DictAligned

        Templates::JUST_ITEMSPART_LBRACKET.call(thunk(itemspart, style & myself, style, "]" + postfix))
      end
    end

    if ctx.features.just_open_pairspart_brackets?
      # TODO: ???
      matchpi %[(%'%partition %'_ (%layer %'_ pp_dict))] do
        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

        Term.of(:choice,
          Term.of(:row, Term[:frag, "{_"], Term[:indented, render_pp_dict(ctx, pp.unsafe_as_d, style, "}" + postfix), pl: 2]),
          Term.of(:row, Term[:frag, "("], thunk(term, style & myself, style, ")" + postfix)))
      end
    end

    if ctx.features.pairspart_split?
      # TODO: ???
      matchpi %[(%'%partition (itemspart_+) pairspattern_)] do
        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

        Term.of(:choice,
          Term.of(:row, Term[:frag, "("],
            Term.of(:row,
              thunk(itemspart, style & InlineStyles, style, ""),
              Term[:frag, "¦"],
              render_pp(ctx, pairspattern, style & InlineStyles, ")" + postfix),
              gap: 1)),
          Term.of(:row, Term[:frag, "("], thunk(term, style & myself, style, ")" + postfix)))
      end
    end

    if ctx.features.item_first_brackets?
      # (feature (%'%item needles_+)
      #   (row (atom "⟨") ($thunk needles (inline dict-aligned) "⟩" postfix)))
      matchpi %[(%'%item needles_+)] do
        myself = InlineStyles | StyleSet::DictAligned

        Templates::ITEM_LBRACKET.call(thunk(needles, style & myself, style, "⟩" + postfix))
      end
    end

    if ctx.features.item_source_brackets?
      # (feature (%'%item° needles_+)
      #   (row (atom "⟨") ($thunk needles (inline dict-aligned) "⟩°" postfix)))
      matchpi %[(%'%item° needles_+)] do
        myself = InlineStyles | StyleSet::DictAligned

        Templates::ITEM_LBRACKET.call(thunk(needles, style & myself, style, "⟩°" + postfix))
      end
    end

    if ctx.features.slot?
      # (feature (%'%slot capture_)
      #   (row (atom "⏏") ($embed capture * postfix)))
      matchpi %[(%'%slot capture_)] do
        Templates::SLOT.call(render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.hold?
      # (feature 'value_
      #   (row (atom "'") ($embed value * postfix)))
      matchpi %['value_] do
        Templates::HOLD.call(render(ctx, value, style, postfix))
      end
    end

    if ctx.features.nonself?
      # (feature (%'%nonself value_)
      #   (row (atom "≡") ($embed value * postfix)))
      matchpi %[(%'%nonself value_)] do
        Templates::NONSELF.call(render(ctx, value, style, postfix))
      end
    end

    if ctx.features.literal?
      # (feature (%'%literal value_)
      #   (row (atom "%'") ($embed value * postfix)))
      matchpi %[(%'%literal value_)] do
        Templates::LITERAL.call(render(ctx, value, style, postfix))
      end
    end

    # (feature ()
    #   (row (atom "()") $postfix))
    matchpi %[()] do
      string = "()#{postfix}"

      Term.of(:frag, string)
    end

    if ctx.features.pairsonly_brackets?
      # (feature T←(¦ _)
      #   (row (atom "{") ($thunk T (map-inline map-multiline) "}" postfix)))
      matchpi %[(¦ pairspart_)] do
        myself = StyleSet::MapInline | StyleSet::MapMultiline

        Templates::PAIRSONLY_LBRACKET.call(thunk(term, style & myself, style, "}" + postfix))
      end
    end

    if ctx.features.call?
      # (feature T←((%symbol nonblank) _ ¦ (%not ()))
      #   (row (atom "(") ($thunk T (inline block-call-keywords-inline block-call-keywords-column keyword-block-call call-indented dict-aligned) ")" postfix)))
      matchpi %[(head_symbol _ ¦ (%not ()))] do
        continue if head.blank?

        myself = InlineStyles | StyleSet::BlockCallKeywordsInline | StyleSet::BlockCallKeywordsColumn | StyleSet::KeywordBlockCall | StyleSet::CallIndented | StyleSet::DictAligned

        Templates::LPAREN.call(thunk(term, style & myself, style, ")" + postfix))
      end

      # (feature T←((%symbol nonblank) _+)
      #   (row (atom "(") ($thunk T (inline call-column call-indented dict-aligned) ")" postfix)))
      matchpi %[(head_symbol _+)] do
        continue if head.blank?

        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

        Templates::LPAREN.call(thunk(term, style & myself, style, ")" + postfix))
      end

      # (feature T←((%symbol nonblank) _+)
      #   (row (atom "(") ($thunk T (inline call-column call-indented dict-aligned) ")" postfix)))
      matchpi %{(head_symbol _+ ¦ (%not {}))} do
        continue if head.blank?

        myself = InlineStyles | StyleSet::BlockCallKeywordsInline | StyleSet::BlockCallKeywordsColumn | StyleSet::CallIndented | StyleSet::DictAligned

        Templates::LPAREN.call(thunk(term, style & myself, style, ")" + postfix))
      end

      # (feature T←[(%symbol nonblank) _*]
      #   (row (atom "(") ($thunk T (inline call-indented dict-aligned) ")" postfix)))
      matchpi %{[head_symbol _*]} do
        continue if head.blank?

        myself = InlineStyles | StyleSet::CallIndented | StyleSet::DictAligned

        Templates::LPAREN.call(thunk(term, style & myself, style, ")" + postfix))
      end
    end

    # (feature T_dict
    #   (row (atom "(") ($thunk T (inline dict-aligned) ")" postfix)))
    matchpi %{_dict} do
      myself = InlineStyles | StyleSet::DictAligned

      Templates::LPAREN.call(thunk(term, style & myself, style, ")" + postfix))
    end

    # TODO: these should be native-code features. We cannot do what they do
    # using (feature ...) specs.
    if ctx.features.group_thousands?
      # The conversion for this one is cheap so we handle it separately.
      matchpi %[(%number i32)] do
        string = "#{term.to(Int32).format(delimiter: '_')}#{postfix}"

        Term.of(:frag, string)
      end

      # The conversion for this one is expensive since we're going through BigInt.
      matchpi %[(%number (whole _))] do
        string = "#{term.to(BigInt).format(delimiter: '_')}#{postfix}"

        Term.of(:frag, string)
      end
    end

    # (feature T←(%any° _number _symbol _string _boolean)
    #   (row (atom ($string T)) $postfix))
    matchpi %[_number], %[_symbol], %[_string], %[_boolean] do
      string = "#{term.inspect}#{postfix}"

      Term.of(:frag, string)
    end
  end
end

def measure(ctx : DisplayContext, node : Term) : {Int32, Int32}
  ctx.measurements.put_if_absent(node) do
    Term.case(node) do
      matchpi %{[frag content_string]} do
        {content.charcount, 1}
      end

      matchpi %{(block {_ w_: (%number +i32), h_: (%number +i32)})} do
        {w.to(Int32), h.to(Int32)}
      end

      matchpi %{[longer child_]} do
        measure(ctx, child)
      end

      matchpi %{(indented child_ ¦ by: n←(%number +i32))} do
        child_width, child_height = measure(ctx, child)

        {child_width + n.to(Int32), child_height}
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

# TODO: It's not normal vs. backmap. It's "normal" vs. "longer" now.
record DisplayContext, normal_maxwidth : Int32, backmap_pattern_maxwidth : Int32, features : Display::Features, measurements = {} of Term => {Int32, Int32}

def flatten(ctx, node : Term, maxwidth : Int32, styles : StyleSet) : {Term, Int32}
  Term.case(node) do
    matchpi %{[frag _]} do
      width, _ = measure(ctx, node)
      {node, maxwidth - width}
    end

    matchpi %{[block _]} do
      {node, maxwidth}
    end

    matchpi %{(longer child_)} do
      flatten(ctx, child, maxwidth + (ctx.backmap_pattern_maxwidth - ctx.normal_maxwidth), styles)
    end

    matchpi %[(indented child_ ¦ by: (%optional 1 n←(%number +i32)))] do
      flattened, maxwidth = flatten(ctx, child, maxwidth - n.to(Int32), styles)

      {Term.of(:indented, flattened, by: n), maxwidth}
    end

    matchpi %{(row children_* ¦ gap: (%optional 0 gap←(%number +i32)))} do
      avail = maxwidth

      {Term.of(node.pairspart.transaction do |commit|
        commit << :row
        children.items.each_with_index do |child, index|
          avail -= gap.to(Int32) if index > 0
          flattened, avail = flatten(ctx, child, avail, styles)
          commit << flattened
        end
      end), avail}
    end

    matchpi %{[col children_*]} do
      min_rem = Int32::MAX

      col = node.pairspart.transaction do |commit|
        commit << :col
        commit.concat(children.items) do |child|
          flat, rem = flatten(ctx, child, maxwidth, styles)
          min_rem = Math.min(min_rem, rem)
          flat
        end
      end

      {Term.of(col), min_rem}
    end

    matchpi %[(thunk subject_dict postfix_string myself0←(%number u16) children0←(%number u16))] do |subject|
      subject = subject.unsafe_as_d
      myself = styles & StyleSet.from_value(myself0.to(UInt16))
      children = styles & StyleSet.from_value(children0.to(UInt16))

      max_rem = Int32::MIN
      max_flat = Term.of

      myself.each do |option|
        next unless rendered = Style.one(option).render?(ctx, subject, children, postfix.to(String))

        flat, rem = flatten(ctx, rendered, maxwidth, children)
        if rem > 0
          return flat, rem
        end

        # Equality does not override because for `choice` and `thunk`, earlier choices
        # must always be preferred.
        next if rem <= max_rem

        max_rem = rem
        max_flat = flat
      end

      if max_rem > Int32::MIN
        return max_flat, max_rem
      end

      raise "BUG: thunk presented no fallback styles, and all other styles have failed"
    end

    matchpi %[(choice choices_+)] do
      max_rem = Int32::MIN
      max_flat = Term.of

      choices.items.each do |choice|
        flat, rem = flatten(ctx, choice, maxwidth, styles)
        if rem > 0
          return flat, rem
        end

        # Equality does not override because for `choice` and `thunk`, earlier choices
        # must always be preferred.
        next if rem <= max_rem

        max_rem = rem
        max_flat = flat
      end

      {max_flat, max_rem}
    end
  end
end

def flatten(ctx, node : Term, styles : StyleSet = StyleSet::All) : {Term, Int32}
  flatten(ctx, node, maxwidth: ctx.normal_maxwidth, styles: styles)
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

    matchpi %[(indented child_ ¦ _ by: (%optional 1 n←(%number +i32)))] do
      draw(ctx, screen, child, x + n.to(Int32), y)
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
      # FIXME: this ends up inserting a bunch of spaces at the end of the string up to @max_x
      (0..@max_x).each do |x|
        char = @cells[{x, y}]? || ' '
        io << char
      end
      io.puts
    end
  end

  def string : String
    String.build do |io|
      write(io)
    end
  end
end

# Some problems:
#   21. Partition pairspart multiline style.
#       I haven't figured out how to write these myself so that's going to be hard!
#   16. Refactor render(): backmap and rule formatting must be toplevel.
#   25. Refactor render() to use chains of Features.
#   14. Dict set syntax && pretty printing
#   20. Line breaking in long strings (and the syntax that this requires!).
#   28. Have a way to incorporate syntax highlighting and in general "hooking into" the renderer.
#   22. Have a mode for pretty printing where we remember where newlines and comments
#       were put by the user in the input string between **toplevel** terms; put them back
#       when pretty printing. Handle comments as well -- somehow ?! This will pave the way to using
#       this pretty printer as a code formatter.
#
#   TESTS??????!
ed = ML.terms(File.read("./editor.soma.wwml"))# Term.of(:+, {:*, 3, 4}, {2})
# ed = ML.terms(%{(x: 100, y: (+ 1 2 3 4 5 6 7 a: 100 b: 200 c: 300 d: 400), z: 300)})

str = String.build do |io|
  screen = Screen.new

  ed.items.each do |sexp|
    screen.clear
    ctx = DisplayContext.new(normal_maxwidth: 60, backmap_pattern_maxwidth: 120, features: Display::Features::All)
    tree = render(ctx, sexp)
    flat, excess = flatten(ctx, tree)
    # puts excess
    # puts ML.display(flat)
    draw(ctx, screen, flat, 0, 0)
    screen.write(io)
    io.puts
  end
end

puts str
puts str == File.read("./pprint1.out.1")
pp ed == ML.terms(str)

# [x] x_: 100 => x: (%let x 100)
# [x] x⋮ 100 => x: (%optional 100 x_number) ;; infers type
# [x] x_⋮ 100 => x: (%optional 100 x_) ;; does not infer type
# [x] x_string⋮ 100 ;; invalid. Either infer or any-blank
# [x] ¦ ... w_ ... => ... w: w_ ...
# [x] ¦ ... w_number ... => ... w: w_number ...
# [x] -x_ => (%- _ x)
# [x] -x_number => (%- _number x)
