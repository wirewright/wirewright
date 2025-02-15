require "./wirewright"
require "./baz5"

# NOTE: the existence of LayoutSet should be re-evaluated. It appears to be an overkill.
# We *really* only check of Inline/Multiline. "Layouts" are more or less a hard-coded/thunked
# "choices". Can we perhaps separate LayoutSet from Layouts (turning it into a flag, inline or not)?

def fill(template : Term, &fn : Int32, Term::Dict::Commit ->)
  handler = ->(term : Term) do
    Term.case(term) do
      matchpi %{($slot n←(%number +i32))} do
        list = Term::Dict.build do |commit|
          fn.call(n.to(Int32), commit)
        end

        Rewrite.many(list)
      end

      otherwise do
        Rewrite.none
      end
    end
  end

  rewrite(template, itemdfsR(callR(handler)))
end

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

record DisplayContext,
  normal_width : Int32,
  longer_width : Int32,
  features : Chain(Feature),
  layouts : Chain(Layout),
  ppairs : Chain(Feature),
  layouts_allowed = LayoutSet::All,
  measurements = {} of Term => {Int32, Int32}

struct DisplayContext
  def inline : DisplayContext
    copy_with(layouts_allowed: layouts_allowed & (LayoutSet::DictInline | LayoutSet::MapInline))
  end

  def inline_only?
    (layouts_allowed & ~(LayoutSet::DictInline | LayoutSet::MapInline)).none?
  end
end

module Layout
  # ```wwml
  # (text "Hello World 1" "Hello World 2" "Hello World 3" x: 100 y: 200)
  # ```
  struct DictInline
    include Layout

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.dict_inline? && (dict = term.as_d?) && dict.size > 0
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      rendered = Term::Dict.build do |commit|
        commit << :row
        commit.with(:gap, 1)

        entries.each_with_last do |(key, value), last|
          if dict.index?(key)
            commit << ctx.features.call(ctx.inline, value, last ? postfix : "")
          else
            commit << Term[:row,
              ctx.features.call(ctx.inline, key, ":"),
              ctx.features.call(ctx.inline, value, last ? postfix : ""),
              gap: 1]
          end
        end
      end

      Term.of(rendered)
    end
  end

  # ```wwml
  # (text
  #  "Hello World 1"
  #  "Hello World 2"
  #  "Hello World 3"
  #  x: 100
  #  y: 200)
  # ```
  struct DictAligned
    include Layout

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.dict_aligned? && (dict = term.as_d?) && dict.size > 0
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      rendered = Term::Dict.build do |commit|
        commit << :col

        entries.each_with_last do |(key, value), last|
          if dict.index?(key)
            commit << ctx.features.call(ctx, value, last ? postfix : "")
          else
            commit << Term[:row,
              ctx.features.call(ctx.inline, key, ":"),
              ctx.features.call(ctx, value, last ? postfix : ""),
              gap: 1]
          end
        end
      end

      Term.of(rendered)
    end
  end

  # ```wwml
  # {x: 100, y: 200, z: 300}
  # ```
  struct MapInline
    include Layout

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.map_inline? && (dict = term.as_d?) && dict.pairsonly? && dict.size > 0
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      rendered = Term::Dict.build do |commit|
        commit << :row
        commit.with(:gap, 1)

        entries.each_with_last do |(key, value), last|
          commit << Term[:row,
            ctx.features.call(ctx.inline, key, ":"),
            ctx.features.call(ctx.inline, value, last ? postfix : ","),
            gap: 1]
        end
      end

      Term.of(rendered)
    end
  end

  # ```wwml
  # {x: 100,
  #  y: 200,
  #  z: 300}
  # ```
  struct MapMultiline
    include Layout

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.map_multiline? && (dict = term.as_d?) && dict.pairsonly? && dict.size > 0
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      rendered = Term::Dict.build do |commit|
        commit << :col

        entries.each_with_last do |(key, value), last|
          commit << Term[:row,
            ctx.features.call(ctx.inline, key, ":"),
            ctx.features.call(ctx, value, last ? postfix : ","),
            gap: 1]
        end
      end

      Term.of(rendered)
    end
  end

  # ```wwml
  # {x:
  #    100,
  #  y:
  #    200,
  #  z:
  #    300}
  # ```
  struct MapMultilineIndented
    include Layout

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.map_multiline_indented? && (dict = term.as_d?) && dict.pairsonly? && dict.size > 0
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      rendered = Term::Dict.build do |commit|
        commit << :col

        entries.each_with_last do |(key, value), last|
          commit << ctx.features.call(ctx.inline, key, ":")
          commit << Term[:indented, ctx.features.call(ctx, value, last ? postfix : ","), by: 2]
        end
      end

      Term.of(rendered)
    end
  end

  # ```wwml
  # (text
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3"
  #   x: 100
  #   y: 200)
  # ```
  struct CallIndented
    include Layout

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.call_indented? && (dict = term.as_d?) && dict.size >= 2
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      rendered = Term::Dict.build do |commit|
        commit << :col

        key0, value0 = entries[0]

        if dict.index?(key0)
          commit << ctx.features.call(ctx.inline, value0, "")
        else
          commit << Term[:row,
            ctx.features.call(ctx.inline, key0, ":"),
            ctx.features.call(ctx.inline, value0, ""),
            gap: 1]
        end

        commit << Term[:indented, Term::Dict.build do |inner|
          inner << :col

          (1...entries.size).each do |index|
            last = index == entries.size - 1
            key, value = entries[index]

            if dict.index?(key)
              inner << ctx.features.call(ctx, value, last ? postfix : "")
            else
              inner << Term[:row,
                ctx.features.call(ctx, key, ":"),
                ctx.features.call(ctx, value, last ? postfix : ""),
                gap: 1]
            end
          end
        end]
      end

      Term.of(rendered)
    end
  end

  # ```wwml
  # (text "Hello World 1"
  #       "Hello World 2"
  #       "Hello World 3")
  # ```
  struct CallColumn
    include Layout

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.call_column? && (dict = term.as_d?) && dict.itemsonly? && dict.size >= 2
        return rest.call(ctx, term, postfix)
      end

      rendered = Term::Dict.build do |commit|
        commit << :row
        commit.with(:gap, 1)

        commit << ctx.features.call(ctx.inline, dict[0], "")

        commit << Term::Dict.build do |inner|
          inner << :col

          dict.items.move(1).each_with_last do |item, last|
            inner << ctx.features.call(ctx.inline, item, last ? postfix : "")
          end
        end
      end

      Term.of(rendered)
    end
  end

  # ```wwml
  # (text x: 100 y: 200
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3")
  # ```
  struct CallKwargsInlineWithBlock
    include Layout

    TEMPLATE = ML.term <<-WWML
    (col (row gap: 1
           ($slot 0)
           (row ($slot 1) gap: 1))
         (indented
           (col ($slot 2)
                ($slot 3))))
    WWML

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.call_kwargs_inline_with_block? && (dict = term.as_d?) && dict.itemsize >= 2 && dict.pairsize >= 1
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      fill(TEMPLATE) do |slot, commit|
        case slot
        when 0
          _, item = entries[0]
          commit << ctx.features.call(ctx.inline, item, "")
        when 1
          (dict.itemsize...dict.size).each do |index|
            key, value = entries[index]

            commit << Term[:row,
              ctx.features.call(ctx.inline, key, ":"),
              ctx.features.call(ctx.inline, value, ""),
              gap: 1]
          end
        when 2
          (1...dict.itemsize - 1).each do |index|
            _, item = entries[index]
            commit << ctx.features.call(ctx, item, "")
          end
        when 3
          _, item = entries[dict.itemsize - 1]
          commit << ctx.features.call(ctx, item, postfix)
        else
          unreachable
        end
      end
    end
  end

  # ```wwml
  # (text x: 100
  #       y: 200
  #   "Hello World 1"
  #   "Hello World 2"
  #   "Hello World 3")
  # ```
  struct CallKwargsColumnWithBlock
    include Layout

    TEMPLATE = ML.term <<-WWML
    (col (row gap: 1
           ($slot 0)
           (col ($slot 1)))
         (indented
           (col ($slot 2)
                ($slot 3))))
    WWML

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.call_kwargs_column_with_block? && (dict = term.as_d?) && dict.itemsize >= 2 && dict.pairsize >= 1
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      fill(TEMPLATE) do |slot, commit|
        case slot
        when 0
          _, item = entries[0]
          commit << ctx.features.call(ctx.inline, item, "")
        when 1
          (dict.itemsize...dict.size).each do |index|
            key, value = entries[index]

            commit << Term[:row,
              ctx.features.call(ctx.inline, key, ":"),
              ctx.features.call(ctx.inline, value, ""),
              gap: 1]
          end
        when 2
          (1...dict.itemsize - 1).each do |index|
            _, item = entries[index]
            commit << ctx.features.call(ctx, item, "")
          end
        when 3
          _, item = entries[dict.itemsize - 1]
          commit << ctx.features.call(ctx, item, postfix)
        else
          unreachable
        end
      end
    end
  end

  # ```wwml
  # (text "Hello World 1"
  #   x: 100
  #   y: 200)
  # ```
  struct CallArgIndentedKwargs
    include Layout

    TEMPLATE = ML.term <<-WWML
    (col (row gap: 1
           ($slot 0)
           ($slot 1))
         (indented
           (col ($slot 2)
                ($slot 3))))
    WWML

    def call(ctx, term, postfix, head, rest) : Term
      unless ctx.layouts_allowed.call_arg_indented_kwargs? && (dict = term.as_d?) && dict.itemsize == 2 && dict.pairsize >= 1
        return rest.call(ctx, term, postfix)
      end

      entries = OrdDict.sorted(dict)

      fill(TEMPLATE) do |slot, commit|
        case slot
        when 0, 1
          _, item = entries[slot]
          commit << ctx.features.call(ctx.inline, item, "")
        when 2
          (2...entries.size - 1).each do |index|
            key, value = entries[index]

            commit << Term[:row,
              ctx.features.call(ctx.inline, key, ":"),
              ctx.features.call(ctx, value, ""),
              gap: 1]
          end
        when 3
          key, value = entries[entries.size - 1]

          commit << Term[:row,
            ctx.features.call(ctx.inline, key, ":"),
            ctx.features.call(ctx, value, postfix),
            gap: 1]
        else
          unreachable
        end
      end
    end
  end
end

# A dirt cheap, Enum-based set of `Layout` includer types.
@[Flags]
enum LayoutSet : UInt16
  {% for includer in Layout.includers %}
    {{ includer.name.split("::")[-1].id }}
  {% end %}

  # Returns a flatten-thunk for layout types in this layout set.
  def thunk(term : Term, postfix : String, myself) : Term
    Term.of(:thunk, term, postfix, self & LayoutSet.new(myself), self)
  end

  # Returns the `Layout` includer that corresponds to a single-element layout set.
  #
  # Raises `ArgumentError` if this set contains more than one element.
  def layout
    {% begin %}
      case self
      {% for includer in Layout.includers %}
      when {{ includer.name.split("::")[-1].id }}
        {{includer}}
      {% end %}
      else
        raise ArgumentError.new
      end
    {% end %}
  end
end

module Templates
  extend self

  def pair(ctx, key, colon, value, postfix)
    r_key = ctx.features.call(ctx.inline, key, colon)
    r_value = ctx.features.call(ctx, value, postfix)

    Term.of(:choice,
      Term.of(:row, r_key, r_value, gap: 1),
      Term[:col, r_key,
        Term[:indented, r_value, by: 2]])
  end
end

module Feature
  macro def_prefix(cls, pattern, prefix)
    struct {{cls}}
      include Feature

      # :nodoc:
      FRAG_PREFIX = Term.of(:frag, {{prefix}})

      def call(ctx, term, postfix, head, rest)
        Term.matchpi(term, {{pattern}}) do
          return Term.of(:row, FRAG_PREFIX, head.call(ctx, suffix, postfix))
        end

        rest.call(ctx, term, postfix)
      end
    end
  end

  macro def_literal(cls, tag, typeblank)
    struct {{cls}}
      include Feature

      def call(ctx, term, postfix, head, rest) : Term
        Term.matchpi(term, {{typeblank}}) do
          return Term.of(:row, Term.of(:frag, term.inspect, tag: {{tag}}), Term.of(:frag, postfix))
        end

        rest.call(ctx, term, postfix)
      end
    end
  end
end

module Feature
  # Renders `(backmap <pattern> <backspec>)` as `<pattern> <> <backspec>`.
  # FIXME: we should somehow only enable these at the top level.
  struct Backmap
    include Feature

    INLINE = ML.term <<-WWML
    (row gap: 1
      ($slot 0) (frag "<>") ($slot 1))
    WWML

    MULTILINE = ML.term <<-WWML
    (col (longer ($slot 0))
      (indented by: 2
        (row gap: 1
          (frag "<>") ($slot 1))))
    WWML

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(backmap pattern_ backspec_)}) do
        inline = fill(INLINE) do |slot, commit|
          case slot
          when 0 then commit << head.call(ctx.inline, pattern, "")
          when 1 then commit << head.call(ctx.inline, backspec, postfix)
          else
            unreachable
          end
        end

        if ctx.inline_only?
          return inline
        end

        multiline = fill(MULTILINE) do |slot, commit|
          case slot
          when 0 then commit << head.call(ctx.inline, pattern, "")
          when 1 then commit << head.call(ctx, backspec, postfix)
          else
            unreachable
          end
        end

        fallback = rest.call(ctx, term, postfix)

        return Term.of(:choice, inline, multiline, fallback)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(rule <pattern> <body>)` as `<pattern> => <body>`.
  # FIXME: we should somehow only enable these at the top level.
  struct Rule
    include Feature

    INLINE = ML.term <<-WWML
    (row gap: 1
      ($slot 0) (frag "=>") ($slot 1))
    WWML

    MULTILINE = ML.term <<-WWML
    (col (longer ($slot 0))
      (indented by: 2
        (row gap: 1
          (frag "=>") ($slot 1))))
    WWML

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(rule pattern_ body_)}) do
        inline = fill(INLINE) do |slot, commit|
          case slot
          when 0 then commit << head.call(ctx.inline, pattern, "")
          when 1 then commit << head.call(ctx.inline, body, postfix)
          else
            unreachable
          end
        end

        if ctx.inline_only?
          return inline
        end

        multiline = fill(MULTILINE) do |slot, commit|
          case slot
          when 0 then commit << head.call(ctx.inline, pattern, "")
          when 1 then commit << head.call(ctx, body, postfix)
          else
            unreachable
          end
        end

        fallback = rest.call(ctx, term, postfix)

        return Term.of(:choice, inline, multiline, fallback)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(edge x)` as `@x`.
  struct Edge
    include Feature

    def call(ctx, term, postfix, head, rest)
      Term.matchpi(term, %{(edge suffix←(%any° _symbol _number _string))}) do
        return Term.of(:row, Term.of(:frag, "@#{suffix.inspect}", tag: :edge), Term.of(:frag, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(%slot x)` as `⏏x`.
  def_prefix PatternSlot, %{(%'%slot suffix_)}, "⏏"
  # Renders `(%nonself x)` as `=x`
  def_prefix PatternNonself, %{(%'%nonself suffix_)}, "≡"
  # Renders `(%literal x)` as `%'x`
  def_prefix PatternLiteral, %{(%'%literal suffix_)}, "%'"
  # Renders `($my x)` as `→x`
  def_prefix BackrefMy, %{($my suffix_)}, "→"
  # Renders `($up x)` as `↑x`
  def_prefix BackrefUp, %{($up suffix_)}, "↑"
  # Renders `($down x)` as `↓x`
  def_prefix BackrefDown, %{($down suffix_)}, "↓"
  # Renders `(hold x)` as `'x`
  def_prefix Hold, %{(hold suffix_)}, "'"

  # Renders `(%let x ...)` as `x←...`
  struct PatternLet
    include Feature

    FRAG_LARROW = ML.term %{(frag "←")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%'%let capture_ pattern_)}) do
        return Term.of(:row, head.call(ctx, capture, ""), FRAG_LARROW, head.call(ctx, pattern, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(%item ...)` as `⟨...⟩`
  struct PatternItemFirst
    include Feature

    FRAG_LBRACKET = ML.term %{(frag "⟨")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%'%item needles_+)}) do
        thunk = ctx.layouts_allowed.thunk(needles, "⟩" + postfix, {:dict_inline, :dict_aligned})

        return Term.of(:row, FRAG_LBRACKET, thunk)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(%item° ...)` as `⟨...⟩°`
  struct PatternItemSource
    include Feature

    FRAG_LBRACKET = ML.term %{(frag "⟨")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%'%item° needles_+)}) do
        thunk = ctx.layouts_allowed.thunk(needles, "⟩°" + postfix, {:dict_inline, :dict_aligned})

        return Term.of(:row, FRAG_LBRACKET, thunk)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # - Renders `(%partition (...) pp_)` as `(... ¦ pp_)`.
  # - Renders `(%layer _ ...)` pp as `_ ...` in the pairspart partition (after `¦`).
  # - Renders `(%partition (...) _)` as `[...]`.
  # - Renders `(%partition _ (%layer _ {...}))` as `{_ ...}`.
  struct PatternPairspart
    include Feature

    FRAG_LPAREN = ML.term %{(frag "(")}
    FRAG_LBRACKET = ML.term %{(frag "[")}

    TEMPLATE_INLINE = ML.term <<-WWML
      (row gap: 1 ($slot 0) (frag "¦") ($slot 1) ($slot 2))
    WWML

    TEMPLATE_MULTILINE = ML.term <<-WWML
      (col
        (longer
          (row gap: 1 ($slot 0) (frag "¦") ($slot 1)))
        (indented
          (col ($slot 2))))
    WWML

    def call(ctx, term, postfix, head, rest) : Term
      Term.case(term) do
        # - Inline pairspatterns are easy:
        #  ```wwml
        #  (text caption_string ¦ _ bold: true x_number y_number)
        #  ```
        # - If the layer's side does not fit inline, we will format it differently:
        #  ```wwml
        #  (text caption_string ¦ _
        #    bold: true
        #    x_number
        #    y_number)
        #  ```
        matchpi %{(%'%partition (itemspart_+) pairspart←(%'%layer below_ (%all side_dict (%not ()))))} do
          inline = fill(TEMPLATE_INLINE) do |slot, commit|
            case slot
            when 0
              commit << ctx.layouts_allowed.thunk(itemspart, "", {:dict_inline})
            when 1
              commit << ctx.features.call(ctx.inline, below, "")
            when 2
              ppentries = OrdDict.sorted(side.unsafe_as_d)
              ppentries.each_with_last do |(key, value), last|
                commit << ctx.ppairs.call(ctx.inline, Term.of(key, value), last ? ")" + postfix : "")
              end
            else
              unreachable
            end
          end

          multiline = fill(TEMPLATE_MULTILINE) do |slot, commit|
            case slot
            when 0
              commit << ctx.layouts_allowed.thunk(itemspart, "", {:dict_inline})
            when 1
              commit << ctx.features.call(ctx.inline, below, "")
            when 2
              ppentries = OrdDict.sorted(side.unsafe_as_d)
              ppentries.each_with_last do |(key, value), last|
                commit << ctx.ppairs.call(ctx, Term.of(key, value), last ? ")" + postfix : "")
              end
            else
              unreachable
            end
          end

          if ctx.inline_only?
            Term.of(:row, FRAG_LPAREN, inline)
          else
            Term.of(:row, FRAG_LPAREN, Term.of(:choice, inline, multiline))
          end
        end

        # Render pairspart-ignored partition shorthand using dict-inline and dict-
        # aligned layouts.
        matchpi %{(%'%partition (itemspart_+) %'_)} do
          thunk = ctx.layouts_allowed.thunk(itemspart, "]" + postfix, {:dict_inline, :dict_aligned})

          Term.of(:row, FRAG_LBRACKET, thunk)
        end

        matchpi %{(%'%partition %'_ (%'%layer %'_ (%all pp_dict (%not ()))))} do
          inline = Term::Dict.build do |commit|
            commit << :row
            commit.with(:gap, 1)

            commit << {:frag, "{_"}

            ppentries = OrdDict.sorted(pp.unsafe_as_d)
            ppentries.each_with_last do |(key, value), last|
              commit << ctx.ppairs.call(ctx.inline, Term.of(key, value), last ? "}" + postfix : "")
            end
          end

          multiline = Term::Dict.build do |commit|
            commit << :row
            commit.with(:gap, 1)

            commit << {:frag, "{_"}
            commit << Term::Dict.build do |column|
              column << :col

              ppentries = OrdDict.sorted(pp.unsafe_as_d)
              ppentries.each_with_last do |(key, value), last|
                column << ctx.ppairs.call(ctx, Term.of(key, value), last ? "}" + postfix : "")
              end
            end
          end

          if ctx.inline_only?
            Term.of(inline)
          else
            Term.of(:choice, inline, multiline)
          end
        end

        # Render other kinds of (%partition)s, e.g. `(+ a_ b_ ¦ {a: 1, b: 2})`:
        #
        # - Inline: `(+ a_ b_ ¦ {a: 1, b: 2})`
        # - Multiline: fallback to `(%partition (+ a_ b_) {a: 1, b: 2})`.
        matchpi %{(%'%partition (itemspart_+) pp_)} do
          inline = Term::Dict.build do |commit|
            commit << :row
            commit.with(:gap, 1)

            commit << ctx.layouts_allowed.thunk(itemspart, "", {:dict_inline})
            commit << {:frag, "¦"}
            commit << ctx.features.call(ctx.inline, pp, ")" + postfix)
           end

          if ctx.inline_only?
            Term.of(:row, FRAG_LPAREN, inline)
          else
            Term.of(:choice, Term.of(:row, FRAG_LPAREN, inline), rest.call(ctx, term, postfix))
          end
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  # Renders `x: (%let x ...)` as `x_: ...` in the pairspart partition.
  struct PairspartLet
    include Feature

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(key_symbol (%'%let key_symbol value_))}) do
        return Templates.pair(ctx, key, "_:", value, postfix)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `x: (%optional 100 x_)` as `x_⋮ 100`, `x: (%optional 100 x_number)` as
  # `x⋮ 0` in the pairspart partition.
  struct PairspartOptional
    include Feature

    def call(ctx, term, postfix, head, rest) : Term
      Term.case(term) do
        matchpi %{(key_ (%'%optional fallback←(%pipe type t_) (%symbol blank key_ t_)))} do
          Templates.pair(ctx, key, "⋮", fallback, postfix)
        end

        matchpi %{(key_ (%'%optional fallback_ (%symbol blank key_ %'_)))} do
          Templates.pair(ctx, key, "_⋮", fallback, postfix)
        end

        otherwise { rest.call(ctx, term, postfix) }
      end
    end
  end

  # Renders `x: (%- _ x)` as `-x_`, `x: (%- _number x)` as `-x_number` (and so on
  # for other types) in the pairspart partition.
  struct PairspartNegation
    include Feature

    def call(ctx, term, postfix, head, rest) : Term
      Term.case(term) do
        {% for type in %w(_ _number _string _symbol _boolean _dict) %}
          matchpi %{(key_symbol (%'%- %'{{type.id}} key_))} do
            Term.of(:row, {:frag, "-"}, ctx.features.call(ctx.inline, key, {{type}} + postfix))
          end
        {% end %}

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  # Renders `x: x_` as `x_`, `x: x_number` as `x_number` in the pairspart partition.
  struct PairspartBlank
    include Feature

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(key_symbol value←(%symbol blank key_ _))}) do
        return ctx.features.call(ctx, value, postfix)
      end

      rest.call(ctx, term, postfix)
    end
  end

  struct PairspartPair
    include Feature

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(key_ value_)}) do
        return Templates.pair(ctx, key, ":", value, postfix)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders symbol literals.
  def_literal SymbolLiteral, :symbol, %{_symbol}

  # Renders number literals.
  struct NumberLiteral
    include Feature

    # Optionally renders thousands in integers with underscore, e.g. `1000000` is rendered
    # as `1_000_000`.
    def initialize(@underscore_thousands = true)
    end

    def call(ctx, term, postfix, head, rest) : Term
      Term.case(term) do
        if @underscore_thousands
          # The conversion for this one is cheap so we handle it separately.
          matchpi %{(%number i32)} do
            Term.of(:row, Term.of(:frag, "#{term.to(Int32).format(delimiter: '_')}", tag: :number), Term.of(:frag, postfix))
          end

          # The conversion for this one is expensive since we're going through BigInt.
          matchpi %{(%number (whole _))} do
            Term.of(:row, Term.of(:frag, "#{term.to(BigInt).format(delimiter: '_')}", tag: :number), Term.of(:frag, postfix))
          end
        end

        matchpi %{_number} do
          Term.of(:row, Term.of(:frag, term.inspect, tag: :number), Term.of(:frag, postfix))
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  # Renders string literals.
  def_literal StringLiteral, :string, %{_string}

  # Renders boolean literals.
  def_literal BooleanLiteral, :boolean, %{_boolean}

  # Renders `()`.
  struct EmptyDict
    include Feature

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{()}) do
        return Term.of(:frag, "()#{postfix}")
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders any call-like dict using one of the call layouts (see `Layout`).
  struct CallLike
    include Feature

    FRAG_LPAREN = ML.term %[(frag "(")]

    def call(ctx, term, postfix, head, rest) : Term
      Term.case(term) do
        matchpi %{((%symbol nonblank) _*)} do
          thunk = ctx.layouts_allowed.thunk(term, ")" + postfix, {:dict_inline, :call_column, :call_indented, :dict_aligned})

          Term.of(:row, FRAG_LPAREN, thunk)
        end

        matchpi %{[(%symbol nonblank) _*]} do
          thunk = ctx.layouts_allowed.thunk(term, ")" + postfix, {:dict_inline, :call_kwargs_inline_with_block, :call_kwargs_column_with_block, :call_arg_indented_kwargs, :call_indented, :dict_aligned})

          Term.of(:row, FRAG_LPAREN, thunk)
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  # Renders any nonempty pairsonly dict using one of the map layouts (see `Layout`).
  struct MapLike
    include Feature

    FRAG_LCURLY = ML.term %[(frag "{")]

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(¦ pairspart_)}) do
        thunk = ctx.layouts_allowed.thunk(term, "}" + postfix, {:map_inline, :map_multiline, :map_multiline_indented})

        return Term.of(:row, FRAG_LCURLY, thunk)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders any nonempty dict using one of two layouts: `Layout::DictInline`
  # or `Layout::DictAligned`.
  struct DictLiteral
    include Feature

    FRAG_LPAREN = ML.term %{(frag "(")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{_dict}) do
        thunk = ctx.layouts_allowed.thunk(term, ")" + postfix, {:dict_inline, :dict_aligned})

        return Term.of(:row, FRAG_LPAREN, thunk)
      end

      rest.call(ctx, term, postfix)
    end
  end
end

class TermPassthrough < Exception
  @callstack = CallStack.empty
end

# Returns `true` if *node* is a P-tree node that does not participate in
# normal positioning or sizing -- we call such nodes *floating*.
def floating?(node : Term) : Bool
  Term.case(node) do
    matchpi %{[block/floating _]} { true }
    otherwise { false }
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

      matchpi %{(block/floating _)} do
        {0, 0}
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
        prev_floating = nil

        children.items.each_with_index do |child, index|
          child_width, child_height = measure(ctx, child)
          floating = floating?(child)
          width += gap.to(Int32) if index > 0 && !(prev_floating || floating)
          prev_floating = floating
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
        prev_floating = nil

        children.items.each_with_index do |child, index|
          child_width, child_height = measure(ctx, child)
          floating = floating?(child)
          height += gap.to(Int32) if index > 0 && !(prev_floating || floating)
          prev_floating = floating
          height += child_height
          width = Math.max(width, child_width)
        end

        {width, height}
      end
    end
  end
end

def flatten(ctx, node : Term, maxwidth : Int32, layouts : LayoutSet) : {Term, Int32}
  Term.case(node) do
    matchpi %{[frag _]}, %{[block _]} do
      width, _ = measure(ctx, node)
      {node, maxwidth - width}
    end

    # Floating blocks are exempt from sizing
    matchpi %{[block/floating _]} do
      {node, maxwidth}
    end

    matchpi %{(longer child_)} do
      flatten(ctx, child, maxwidth + (ctx.longer_width - ctx.normal_width), layouts)
    end

    matchpi %[(indented child_ ¦ by: (%optional 1 n←(%number +i32)))] do
      flattened, maxwidth = flatten(ctx, child, maxwidth - n.to(Int32), layouts)

      {Term.of(:indented, flattened, by: n), maxwidth}
    end

    matchpi %{(row children_* ¦ gap: (%optional 0 gap←(%number +i32)))} do
      avail = maxwidth

      {Term.of(node.pairspart.transaction do |commit|
        commit << :row
        children.items.each_with_index do |child, index|
          avail -= gap.to(Int32) if index > 0
          flattened, avail = flatten(ctx, child, avail, layouts)
          commit << flattened
        end
      end), avail}
    end

    matchpi %{[col children_*]} do
      min_rem = Int32::MAX

      col = node.pairspart.transaction do |commit|
        commit << :col
        commit.concat(children.items) do |child|
          flat, rem = flatten(ctx, child, maxwidth, layouts)
          min_rem = Math.min(min_rem, rem)
          flat
        end
      end

      {Term.of(col), min_rem}
    end

    matchpi %[(thunk subject_dict postfix_string myself0←(%number u16) children0←(%number u16))] do |subject|
      myself = layouts & LayoutSet.from_value(myself0.to(UInt16))
      children = layouts & LayoutSet.from_value(children0.to(UInt16))

      max_rem = Int32::MIN
      max_flat = Term.of

      candidates = [] of Chain::Thunk(Layout)
      myself.each do |option|
        candidates << ctx.layouts.find(option.layout)
      end
      candidates.sort_by!(&.preference)
      candidates.each do |candidate|
        begin
          rendered = candidate.call(ctx.copy_with(layouts_allowed: children), subject, postfix.to(String))
        rescue TermPassthrough
          next
        end

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

      unreachable
    end

    matchpi %[(choice choices_+)] do
      max_rem = Int32::MIN
      max_flat = Term.of

      choices.items.each do |choice|
        flat, rem = flatten(ctx, choice, maxwidth, layouts)
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

def flatten(ctx, node : Term, layouts : LayoutSet = LayoutSet::All) : {Term, Int32}
  flatten(ctx, node, maxwidth: ctx.normal_width, layouts: layouts)
end

struct Chain(T)
  def initialize(@callables : Slice(T))
  end

  # Constructs a chain of *callables*.
  def self.new(*callables : T)
    Chain(T).new(callables.to_readonly_slice(&.as(T)))
  end

  # :nodoc:
  struct Thunk(T)
    def initialize(@chain : Chain(T), @index : Int32)
    end

    def preference : Int32
      @index
    end

    def call(ctx : DisplayContext, term : Term, postfix : String)
      @chain.call(ctx, term, postfix, index: @index)
    end
  end

  def call(ctx : DisplayContext, term : Term, postfix : String, *, index : Int32)
    unless index >= 0
      raise ArgumentError.new
    end

    if index >= @callables.size
      raise TermPassthrough.new
    end

    head = Thunk.new(self, 0)
    rest = Thunk.new(self, index + 1)

    @callables[index].call(ctx, term, postfix, head, rest)
  end

  def call(ctx : DisplayContext, term : Term, postfix : String)
    call(ctx, term, postfix, index: 0)
  end

  def find(needle)
    @callables.each_with_index do |callable, index|
      next unless needle === callable
      return Thunk(T).new(self, index)
    end

    raise ArgumentError.new("find: needle #{needle} not in the chain")
  end
end
