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
    Term[:frag, ":", width: 1, height: 1],
    Term[:padding, render(ctx, v, styles, postfix), pl: 1])
end

def render_entry(ctx, dict, k, v, styles, postfix)
  if dict.index?(k)
    render_item(ctx, v, styles, postfix)
  else
    render_pair(ctx, k, v, styles, postfix)
  end
end

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

  # Renders the entries of a dict *inline*. One of the general-purpose styles
  # (can present any dict without problems).
  #
  # ```wwml
  # (text "Hello World 1" "Hello World 2" "Hello World 3" x: 100 y: 200)
  # ```
  record DictInline do
    BLUEPRINT = Blueprint.build do
      template %{(row ⏏entries ⏏tail gap: 1)}
      slot :entries, :entries, 0, -2, InlineStyles, postfix: :none
      slot :tail, :entries, -2, -1, InlineStyles, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      true
    end
  end

  # Renders the entries of a dict on multiple lines. All items are lined up.
  # One of the general-purpose styles (can present any dict without problems).
  #
  # ```wwml
  # (text
  #  "Hello World 1"
  #  "Hello World 2"
  #  "Hello World 3"
  #  x: 100
  #  y: 200)
  # ```
  record DictAligned do
    BLUEPRINT = Blueprint.build do
      template %{(col ⏏head ⏏body ⏏tail)}
      slot :head, :entries, 0, 1, StyleSet::All, postfix: :none
      slot :body, :entries, 1, -2, StyleSet::All, postfix: :none
      slot :tail, :entries, -2, -1, StyleSet::All, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      true
    end
  end

  # Renders the entries of a dict on multiple lines. Items following the first one
  # are padded once. Only used when the first item of the dict is a symbol.
  #
  # ```wwml
  # (text
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3"
  #   x: 100
  #   y: 200)
  # ```
  record CallIndented do
    BLUEPRINT = Blueprint.build do
      template %{(col ⏏head (padding (col ⏏body ⏏tail) pl: 1))}
      slot :head, :entries, 0, 1, InlineStyles, postfix: :none
      slot :body, :entries, 1, -2, StyleSet::All, postfix: :none
      slot :tail, :entries, -2, -1, StyleSet::All, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      dict.itemsize > 0 && dict.size >= 2 && dict[0].type.symbol?
    end
  end

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
  record CallColumn do
    BLUEPRINT = Blueprint.build do
      template %{(row ⏏head (col ⏏body ⏏tail) gap: 1)}
      slot :head, :items, 0, 1, InlineStyles, postfix: :none
      slot :body, :items, 1, -2, InlineStyles, postfix: :none
      slot :tail, :items, -2, -1, InlineStyles, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      dict.itemsonly? && dict.size >= 2 && dict[0].type.symbol?
    end
  end

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
  record BlockCallKeywordsInline do
    BLUEPRINT = Blueprint.build do
      template %{(col (row ⏏head (col ⏏pairs) gap: 1) (padding (col ⏏body ⏏tail) pl: 1))}
      slot :head, :items, 0, 1, InlineStyles, postfix: :none
      slot :pairs, :pairs, 0, -1, InlineStyles, postfix: :none
      slot :body, :items, 1, -2, StyleSet::All, postfix: :none
      slot :tail, :items, -2, -1, StyleSet::All, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      dict.itemsize >= 2 && dict.pairsize > 0 && dict[0].type.symbol?
    end
  end

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
  record BlockCallKeywordsColumn do
    BLUEPRINT = Blueprint.build do
      template %{(col (row ⏏head (col ⏏pairs) gap: 1) (padding (col ⏏body ⏏tail) pl: 1))}
      slot :head, :items, 0, 1, InlineStyles, postfix: :none
      slot :pairs, :pairs, 0, -1, StyleSet::All, postfix: :none
      slot :body, :items, 1, -2, StyleSet::All, postfix: :none
      slot :tail, :items, -2, -1, StyleSet::All, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      dict.itemsize >= 2 && dict.pairsize > 0 && dict[0].type.symbol?
    end
  end

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
  record KeywordBlockCall do
    BLUEPRINT = Blueprint.build do
      template %{(col (row ⏏item0 ⏏item1 gap: 1) (padding (col ⏏pairs ⏏tail) pl: 1))}
      slot :item0, :items, 0, 1, InlineStyles, postfix: :none
      slot :item1, :items, 1, 2, InlineStyles, postfix: :none
      slot :pairs, :pairs, 0, -2, StyleSet::All, postfix: :none
      slot :tail, :pairs, -2, -1, StyleSet::All, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      dict.itemsize == 2 && dict.pairsize > 0 && dict[0].type.symbol?
    end
  end

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
  record MapInline do
    BLUEPRINT = Blueprint.build do
      template %{(row ⏏pairs ⏏tail gap: 1)}
      slot :pairs, :pairs, 0, -2, InlineStyles, postfix: :comma
      slot :tail, :pairs, -2, -1, InlineStyles, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      dict.pairsonly? && dict.pairsize > 0
    end
  end

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
  record MapMultiline do
    BLUEPRINT = Blueprint.build do
      template %{(col ⏏pairs ⏏tail)}
      slot :pairs, :pairs, 0, -2, StyleSet::All, postfix: :comma
      slot :tail, :pairs, -2, -1, StyleSet::All, postfix: :sink
    end

    def blueprint : Blueprint
      BLUEPRINT
    end

    def applicable_to?(dict : Term::Dict) : Bool
      dict.pairsonly? && dict.pairsize > 0
    end
  end

  # Converts a single-element *styleset* into the corresponding `Style` object.
  # Raises `ArgumentError` if *styleset* is not a single-element set.
  def one(styleset : StyleSet)
    {% for style in StyleSet.constants %}
      {% unless {:None, :All}.includes?(style.id.symbolize) %}
        if styleset == StyleSet::{{style}}
          return {{style}}.new
        end
      {% end %}
    {% end %}

    raise ArgumentError.new
  end
end

module Style
  # Blueprints are a standardized and declarative way to define dict styles.
  #
  # - Blueprints consist of templates and slot specs (`Slot`).
  # - Templates contain slots (e.g. `⏏qux` which is short for `(%slot qux)`).
  record Blueprint, template : Term, slots : Slice(Slot) do
    # Defines how a slot should be rewritten.
    #
    # - *id* is the name of the slot in the template.
    # - *region* is the part of the dictionary which the range *b*...*e* describes.
    # - *b*...*e* is the selected range, defined relatively (negative numbers count
    #   from the end of the region).
    # - *allowed* is the style set allowed for entries in the region.
    # - *postfix* specifies what postfix this slot passes to entries in the region.
    record Slot, id : Term::Sym, region : Region, b : Int8, e : Int8, allowed : StyleSet, postfix : Postfix do
      enum Region : UInt8
        Items
        Pairs
        Entries
      end

      enum Postfix : UInt8
        None
        Comma
        Sink
      end
    end

    class Builder
      # :nodoc:
      getter slots = [] of Slot

      # :nodoc:
      getter template = Term.of

      # Sets the template of the resulting blueprint.
      def template(ml : String) : Nil
        @template = ML.term(ml)
      end

      # Adds a slot spec to the blueprint.
      def slot(id, region : Slot::Region, b : Int8, e : Int8, allowed : StyleSet, *, postfix : Slot::Postfix) : Nil
        @slots << Slot.new(Term[id], region, b, e, allowed, postfix)
      end
    end

    # Yields with `Builder` that allows you to construct a `Blueprint` less clumsily.
    def self.build(&) : Blueprint
      with builder = Builder.new yield

      new(builder.template, builder.slots.to_readonly_slice)
    end
  end

  # :nodoc:
  struct RenderRewriter
    def initialize(@ctx : DisplayContext, @bp : Blueprint, @dict : Term::Dict, @styleset : StyleSet, @postfix : String)
    end

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

    private def translate(range, dict : Term::Dict, region)
      case region
      in .items?   then translate(range, @dict.itemsize)
      in .entries? then translate(range, @dict.size)
      in .pairs?
        start, count = translate(range, @dict.pairsize)

        {start + @dict.itemsize, count}
      end
    end

    def call(term : Term) : Rewrite::Any
      Term.case(term) do
        matchpi %[(%'%slot id_)] do
          slot = @bp.slots.find! { |slot| slot.id == id }
          start, count = translate(slot.b.to_i...slot.e.to_i, @dict, slot.region)

          if count.zero?
            if slot.postfix.sink?
              raise "BUG: empty postfix sink, loss of postfix"
            end

            return Rewrite.many(Term[])
          end

          case slot.postfix
          in .none?  then subpostfix = ""
          in .comma? then subpostfix = ","
          in .sink?  then subpostfix = @postfix
          end

          if @ctx.features.sorted_pairs?
            entries = OrdDict.sorted(@dict)
          else
            entries = OrdDict.unsorted(@dict)
          end

          case count
          when 1
            entry = render_entry(@ctx, @dict, *entries[start], @styleset & slot.allowed, subpostfix)

            Rewrite.one(entry)
          else
            offspring = Term::Dict.build do |commit|
              count.times do |offset|
                entry = render_entry(@ctx, @dict, *entries[start + offset], @styleset & slot.allowed, subpostfix)
                commit << entry
              end
            end

            Rewrite.many(offspring)
          end
        end

        otherwise { Rewrite.none }
      end
    end
  end

  # Renders *dict* using the given blueprint *bp*.
  def render(ctx, bp : Blueprint, dict : Term::Dict, styleset, postfix)
    rewrite(bp.template, itemdfsR(callR(RenderRewriter.new(ctx, bp, dict, styleset, postfix))))
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

def render_pp_pair(ctx, k, v, styles, postfix)
  Term.of_case(v, env: Term[k: k]) do
    if ctx.features.pairspart_let?
      matchpi %[(%'%let k_symbol realv_)] do
        Term.of(:row,
          render(ctx, k, styles, ""),
          Term[:frag, "_:", width: 2, height: 1],
          Term[:padding, render(ctx, realv, styles, postfix), pl: 1])
      end
    end

    if ctx.features.pairspart_optional?
      matchpi %[(%'%optional fallback_ realv_symbol)] do
        continue unless blank = realv.blank?
        continue unless k == blank.name?

        if fallback.type == blank.type # Inference will succeed
          Term.of(:row,
            render(ctx, k, styles, ""),
            Term[:frag, "⋮", width: 1, height: 1],
            Term[:padding, render(ctx, fallback, styles, postfix), pl: 1])
        elsif blank.type.any?
          Term.of(:row,
            render(ctx, k, styles, ""),
            Term[:frag, "_⋮", width: 2, height: 1],
            Term[:padding, render(ctx, fallback, styles, postfix), pl: 1])
        else
          continue
        end
      end
    end

    if ctx.features.pairspart_negation?
      matchpi %[(%'%- %'_ k_symbol)] do
        Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(ctx, k, styles, "_" + postfix))
      end

      matchpi %[(%'%- %'_number k_symbol)] do
        Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(ctx, k, styles, "_number" + postfix))
      end

      matchpi %[(%'%- %'_string k_symbol)] do
        Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(ctx, k, styles, "_string" + postfix))
      end

      matchpi %[(%'%- %'_symbol k_symbol)] do
        Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(ctx, k, styles, "_symbol" + postfix))
      end

      matchpi %[(%'%- %'_boolean k_symbol)] do
        Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(ctx, k, styles, "_boolean" + postfix))
      end

      matchpi %[(%'%- %'_dict k_symbol)] do
        Term.of(:row, Term[:frag, "-", width: 1, height: 1], render(ctx, k, styles, "_dict" + postfix))
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
      Term.of(:row,
        render(ctx, k, styles, ""),
        Term[:frag, ":", width: 1, height: 1],
        Term[:padding, render(ctx, v, styles, postfix), pl: 1])
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
# TODO: instead of myself = ... children = ... we should have a chain and a choice. choice a
# is our first preferred style, b second, etc., AS A THUNK. Then as the last choice we have
# chain.next.
def render(ctx, term : Term, style = StyleSet::All, postfix : String = "")
  Term.of_case(term) do
    # Humph?! This should actually be TOPLEVEL!
    if ctx.features.toplevel_backmap?
      matchpi %[(backmap pattern_ backspec_)] do
        choice_a = Term[:row,
          render(ctx, pattern, style & InlineStyles, ""),
          Term[:frag, "<>", width: 2, height: 1],
          render(ctx, backspec, style & InlineStyles, postfix),
          gap: 1]

        choice_b = Term[:col,
          Term[:info, render(ctx, pattern, style & InlineStyles, ""), tag: :"backmap-pattern"],
          Term[:padding,
            Term[:row,
              Term[:frag, "<>", width: 2, height: 1],
              render(ctx, backspec, style, postfix),
              gap: 1], pl: 2]]

        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

        choice_c = Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix))
        Term.of(:choice, choice_a, choice_b, choice_c)
      end
    end

    # Humph?! This should actually be TOPLEVEL!
    if ctx.features.toplevel_rule?
      matchpi %[(rule pattern_ body_)] do
        choice_a = Term[:row,
          render(ctx, pattern, style & InlineStyles, ""),
          Term[:frag, "=>", width: 2, height: 1],
          render(ctx, body, style & InlineStyles, postfix),
          gap: 1]

        choice_b = Term[:col,
          render(ctx, pattern, style & InlineStyles, ""),
          Term[:padding,
            Term[:row,
              Term[:frag, "=>", width: 2, height: 1],
              render(ctx, body, style, postfix),
              gap: 1], pl: 2]]

        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

        choice_c = Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix))
        Term.of(:choice, choice_a, choice_b, choice_c)
      end
    end

    if ctx.features.edge?
      matchpi %[(edge id←(%any° _symbol _number _string))] do
        string = "@#{id.inspect}#{postfix}"

        Term.of(:frag, string, width: string.size, height: 1)
      end
    end

    if ctx.features.backref_my?
      matchpi %[($my capture_)] do
        Term.of(:row, Term[:frag, "→", width: 1, height: 1], render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.backref_up?
      matchpi %[($up capture_)] do
        Term.of(:row, Term[:frag, "↑", width: 1, height: 1], render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.backref_down?
      matchpi %[($down capture_)] do
        Term.of(:row, Term[:frag, "↓", width: 1, height: 1], render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.let_arrow?
      matchpi %[(%'%let capture_ pattern_)] do
        Term.of(:row, render(ctx, capture, style, ""), Term[:frag, "←", width: 1, height: 1], render(ctx, pattern, style, postfix))
      end
    end

    if ctx.features.just_itemspart_brackets?
      matchpi %[(%'%partition () %'_)] do
        string = "[]#{postfix}"

        Term.of(:frag, string, width: string.size, height: 1)
      end

      matchpi %[(%'%partition (itemspart_+) %'_)] do
        myself = InlineStyles | StyleSet::DictAligned

        Term.of(:row, Term[:frag, "[", width: 1, height: 1], thunk(itemspart, style & myself, style, "]" + postfix))
      end
    end

    if ctx.features.just_open_pairspart_brackets?
      matchpi %[(%'%partition %'_ (%layer %'_ pp_dict))] do
        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

        Term.of(:choice,
          Term.of(:row, Term[:frag, "{_", width: 2, height: 1], Term[:padding, render_pp_dict(ctx, pp.unsafe_as_d, style, "}" + postfix), pl: 1]),
          Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix)))
      end
    end

    if ctx.features.pairspart_split?
      matchpi %[(%'%partition (itemspart_+) pairspattern_)] do
        myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

        Term.of(:choice,
          Term.of(:row, Term[:frag, "(", width: 1, height: 1],
            Term.of(:row,
              thunk(itemspart, style & InlineStyles, style, ""),
              Term[:frag, "¦", width: 1, height: 1],
              render_pp(ctx, pairspattern, style & InlineStyles, ")" + postfix),
              gap: 1)),
          Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix)))
      end
    end

    if ctx.features.item_first_brackets?
      matchpi %[(%'%item needles_+)] do
        myself = InlineStyles | StyleSet::DictAligned

        Term.of(:row, Term[:frag, "⟨", width: 1, height: 1], thunk(needles, style & myself, style, "⟩" + postfix))
      end
    end

    if ctx.features.item_source_brackets?
      matchpi %[(%'%item° needles_+)] do
        myself = InlineStyles | StyleSet::DictAligned

        Term.of(:row, Term[:frag, "⟨", width: 1, height: 1], thunk(needles, style & myself, style, "⟩°" + postfix))
      end
    end

    if ctx.features.slot?
      matchpi %[(%'%slot capture_)] do
        Term.of(:row, Term[:frag, "⏏", width: 1, height: 1], render(ctx, capture, style, postfix))
      end
    end

    if ctx.features.hold?
      matchpi %['value_] do
        Term.of(:row, Term[:frag, "'", width: 1, height: 1], render(ctx, value, style, postfix))
      end
    end

    if ctx.features.nonself?
      matchpi %[(%'%nonself value_)] do
        Term.of(:row, Term[:frag, "≡", width: 1, height: 1], render(ctx, value, style, postfix))
      end
    end

    if ctx.features.literal?
      matchpi %[(%'%literal value_)] do
        Term.of(:row, Term[:frag, "%'", width: 2, height: 1], render(ctx, value, style, postfix))
      end
    end

    matchpi %[()] do
      string = "()#{postfix}"

      Term.of(:frag, string, width: string.size, height: 1)
    end

    if ctx.features.pairsonly_brackets?
      matchpi %[(¦ pairspart_)] do
        myself = StyleSet::MapInline | StyleSet::MapMultiline

        Term.of(:row, Term[:frag, "{", width: 1, height: 1], thunk(term, style & myself, style, "}" + postfix))
      end
    end

    matchpi %[(head_symbol _ ¦ (%not ()))] do
      continue if head.blank?

      myself = InlineStyles | StyleSet::BlockCallKeywordsInline | StyleSet::BlockCallKeywordsColumn | StyleSet::KeywordBlockCall | StyleSet::CallIndented | StyleSet::DictAligned

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix))
    end

    matchpi %[(head_symbol _+)] do
      continue if head.blank?

      myself = InlineStyles | StyleSet::CallColumn | StyleSet::CallIndented | StyleSet::DictAligned

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix))
    end

    matchpi %{(head_symbol _+ ¦ (%not {}))} do
      continue if head.blank?

      myself = InlineStyles | StyleSet::BlockCallKeywordsInline | StyleSet::BlockCallKeywordsColumn | StyleSet::CallIndented | StyleSet::DictAligned

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix))
    end

    matchpi %{[head_symbol _*]} do
      continue if head.blank?

      myself = InlineStyles | StyleSet::CallIndented | StyleSet::DictAligned

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix))
    end

    matchpi %{_dict} do
      myself = InlineStyles | StyleSet::DictAligned

      Term.of(:row, Term[:frag, "(", width: 1, height: 1], thunk(term, style & myself, style, ")" + postfix))
    end

    if ctx.features.group_thousands?
      # The conversion for this one is cheap so we handle it separately.
      matchpi %[(%number i32)] do
        string = "#{term.to(Int32).format(delimiter: '_')}#{postfix}"

        Term.of(:frag, string, width: string.size, height: 1)
      end

      # The conversion for this one is expensive since we're going through BigInt.
      matchpi %[(%number (whole _))] do
        string = "#{term.to(BigInt).format(delimiter: '_')}#{postfix}"

        Term.of(:frag, string, width: string.size, height: 1)
      end
    end

    matchpi %[_number], %[_symbol], %[_string], %[_boolean] do
      string = "#{term.inspect}#{postfix}"

      Term.of(:frag, string, width: string.size, height: 1)
    end
  end
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

record DisplayContext, normal_maxwidth : Int32, backmap_pattern_maxwidth : Int32, features : Display::Features, measurements = {} of Term => {Int32, Int32}

def flatten(ctx, node : Term, maxwidth : Int32, styles : StyleSet) : {Term, Int32}
  Term.case(node) do
    matchpi %{(frag _ ¦ _ width_: (%number +i32))} do
      {node, maxwidth - width.to(Int32)}
    end

    matchpi %{[block _]} do
      {node, maxwidth}
    end

    matchpi %{(info child_ tag: backmap-pattern)} do
      flatten(ctx, child, maxwidth + (ctx.backmap_pattern_maxwidth - ctx.normal_maxwidth), styles)
    end

    matchpi %[(padding child_ ¦ pl_: (%number +i32))] do
      flattened, maxwidth = flatten(ctx, child, maxwidth - pl.to(Int32), styles)

      {Term.of(:padding, flattened, pl: pl), maxwidth}
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
        style = Style.one(option)
        next unless style.applicable_to?(subject)

        rendered = Style.render(ctx, style.blueprint, subject, children, postfix.to(String))

        flat, rem = flatten(ctx, rendered, maxwidth, styles)
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
#   21. Partition pairspart multiline style.
#       I haven't figured out how to write these myself so that's going to be hard!
#   16. Refactor render(): backmap and rule formatting must be toplevel.
#   25. Refactor render() to use chains.
#   27. Refactor render() to use templates.
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
# ed = ML.terms(%{(text x: 100 y: 200 z: 300 a: 1 b: 2 c: 3 "Hello World 1" "Hello World 2")})

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
pp ed == ML.terms(str)

# [x] x_: 100 => x: (%let x 100)
# [x] x⋮ 100 => x: (%optional 100 x_number) ;; infers type
# [x] x_⋮ 100 => x: (%optional 100 x_) ;; does not infer type
# [x] x_string⋮ 100 ;; invalid. Either infer or any-blank
# [x] ¦ ... w_ ... => ... w: w_ ...
# [x] ¦ ... w_number ... => ... w: w_number ...
# [x] -x_ => (%- _ x)
# [x] -x_number => (%- _number x)
