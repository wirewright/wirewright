require "./wirewright"

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

alias FeatureChain = DisplayContext, Term, String -> Term
alias LayoutChain = DisplayContext, LayoutSet, Term, String -> Term

record DisplayContext,
  normal_width : Int32,
  longer_width : Int32,
  features : FeatureChain,
  layouts : LayoutChain,
  layouts_allowed = LayoutSet::All,
  measurements = {} of Term => {Int32, Int32}

struct DisplayContext
  def inline : DisplayContext
    copy_with(layouts_allowed: layouts_allowed & (LayoutSet::DictInline | LayoutSet::MapInline))
  end
end

@[Flags]
enum LayoutSet : UInt16
  DictInline

  CallIndented

  MapInline
  MapMultiline
  MapMultilineIndented

  DictAligned

  def thunk(term : Term, postfix : String, myself) : Term
    Term.of(:thunk, term, postfix, self & LayoutSet.new(myself), self)
  end
end

module Layout
  # ```wwml
  # (text "Hello World 1" "Hello World 2" "Hello World 3" x: 100 y: 200)
  # ```
  struct DictInline
    def call(ctx, selector, term, postfix, head, rest) : Term
      unless selector.dict_inline? && ctx.layouts_allowed.dict_inline? && (dict = term.as_d?) && dict.size > 0
        return rest.call(ctx, selector, term, postfix)
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
    def call(ctx, selector, term, postfix, head, rest) : Term
      unless selector.dict_aligned? && ctx.layouts_allowed.dict_aligned? && (dict = term.as_d?) && dict.size > 0
        return rest.call(ctx, selector, term, postfix)
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
    def call(ctx, selector, term, postfix, head, rest) : Term
      unless selector.map_inline? && ctx.layouts_allowed.map_inline? && (dict = term.as_d?) && dict.pairsonly? && dict.size > 0
        return rest.call(ctx, selector, term, postfix)
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
    def call(ctx, selector, term, postfix, head, rest) : Term
      unless selector.map_multiline? && ctx.layouts_allowed.map_multiline? && (dict = term.as_d?) && dict.pairsonly? && dict.size > 0
        return rest.call(ctx, selector, term, postfix)
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
    def call(ctx, selector, term, postfix, head, rest) : Term
      unless selector.map_multiline_indented? && ctx.layouts_allowed.map_multiline_indented? && (dict = term.as_d?) && dict.pairsonly? && dict.size > 0
        return rest.call(ctx, selector, term, postfix)
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
    def call(ctx, selector, term, postfix, head, rest) : Term
      unless selector.call_indented? && ctx.layouts_allowed.call_indented? && (dict = term.as_d?) && dict.size >= 2
        return rest.call(ctx, selector, term, postfix)
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

  private def self.chain0(head, layout) : LayoutChain
    ->(ctx : DisplayContext, selector : LayoutSet, term : Term, postfix : String) do
      rest = ->(ctx : DisplayContext, selector : LayoutSet, term : Term, postfix : String) do
        raise TermPassthrough.new
      end

      layout.call(ctx, selector, term, postfix, head, rest)
    end
  end

  private def self.chain0(head, layout, *layouts) : LayoutChain
    ->(ctx : DisplayContext, selector : LayoutSet, term : Term, postfix : String) do
      rest = ->(ctx : DisplayContext, selector : LayoutSet, term : Term, postfix : String) do
        chain0(head, *layouts).call(ctx, selector, term, postfix)
      end
      layout.call(ctx, selector, term, postfix, head, rest)
    end
  end

  # Creates a chain of *layouts*.
  def self.chain(layout, *layouts) : LayoutChain
    head = nil
    head = ->(ctx : DisplayContext, selector : LayoutSet,  term : Term, postfix : String) do
      rest = chain0(head.not_nil!("cannot call head during chain initialization"), layout, *layouts)
      rest.call(ctx, selector, term, postfix)
    end
  end
end

module Feature
  extend self

  # Renders `(edge x)` as `@x`.
  struct Edge
    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(edge id←(%any° _symbol _number _string))}) do
        return Term.of(:frag, "@#{id.inspect}#{postfix}")
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(%slot x)` as `⏏x`.
  struct PatternSlot
    FRAG_SLOT = ML.term %{(frag "⏏")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%'%slot capture_)}) do
        return Term.of(:row, FRAG_SLOT, head.call(ctx, capture, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(%nonself x)` as `=x`
  struct PatternNonself
    FRAG_NONSELF = ML.term %{(frag "≡")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%'%nonself value_)}) do
        return Term.of(:row, FRAG_NONSELF, head.call(ctx, value, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(%literal x)` as `%'x`
  struct PatternLiteral
    FRAG_LITERAL = ML.term %{(frag "%'")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%'%literal value_)}) do
        return Term.of(:row, FRAG_LITERAL, head.call(ctx, value, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(%let x ...)` as `x←...`
  struct PatternLet
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
    FRAG_LBRACKET = ML.term %{(frag "⟨")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%'%item° needles_+)}) do
        thunk = ctx.layouts_allowed.thunk(needles, "⟩°" + postfix, {:dict_inline, :dict_aligned})

        return Term.of(:row, FRAG_LBRACKET, thunk)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders literals such as `100`, qux, `"Hello World"`, `true`, etc.
  struct Literal
    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(%any° _symbol _number _string _boolean)}) do
        return Term.of(:frag, "#{term.inspect}#{postfix}")
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders thousands in integers with underscore, e.g. `1000000` is rendered
  # as `1_000_000`.
  struct IntegerGroupThousands
    def call(ctx, term, postfix, head, rest) : Term
      Term.case(term) do
        # The conversion for this one is cheap so we handle it separately.
        matchpi %{(%number i32)} do
          Term.of(:frag, "#{term.to(Int32).format(delimiter: '_')}#{postfix}")
        end

        # The conversion for this one is expensive since we're going through BigInt.
        matchpi %{(%number (whole _))} do
          Term.of(:frag, "#{term.to(BigInt).format(delimiter: '_')}#{postfix}")
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  # Renders `($my x)` as `→x`
  struct BackrefMy
    FRAG_RARROW = ML.term %{(frag "→")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{($my capture_)}) do
        return Term.of(:row, FRAG_RARROW, head.call(ctx, capture, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `($up x)` as `↑x`
  struct BackrefUp
    FRAG_UARROW = ML.term %{(frag "↑")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{($up capture_)}) do
        return Term.of(:row, FRAG_UARROW, head.call(ctx, capture, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `($down x)` as `↓x`
  struct BackrefDown
    FRAG_DARROW = ML.term %{(frag "↓")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{($down capture_)}) do
        return Term.of(:row, FRAG_DARROW, head.call(ctx, capture, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `(hold x)` as `'x`
  struct Hold
    FRAG_TICK = ML.term %{(frag "'")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{(hold value_)}) do
        return Term.of(:row, FRAG_TICK, head.call(ctx, value, postfix))
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders `()`.
  struct EmptyDict
    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{()}) do
        return Term.of(:frag, "()#{postfix}")
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders any call-like dict using the `CallIndented` layout.
  struct Call
    FRAG_LPAREN = ML.term %[(frag "(")]

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{[head_symbol _*]}) do
        continue if head.blank?

        thunk = ctx.layouts_allowed.thunk(term, ")" + postfix, {:dict_inline, :call_indented, :dict_aligned})

        return Term.of(:row, FRAG_LPAREN, thunk)
      end

      rest.call(ctx, term, postfix)
    end
  end

  # Renders any nonempty pairsonly dict using one of the following layouts:
  #
  # - `Layout::MapInline`
  # - `Layout::MapMultiline`.
  # - `Layout::MapMultilineIndented`.
  struct DataMap
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
  struct DataDict
    FRAG_LPAREN = ML.term %{(frag "(")}

    def call(ctx, term, postfix, head, rest) : Term
      Term.matchpi(term, %{_dict}) do
        thunk = ctx.layouts_allowed.thunk(term, ")" + postfix, {:dict_inline, :dict_aligned})

        return Term.of(:row, FRAG_LPAREN, thunk)
      end

      rest.call(ctx, term, postfix)
    end
  end

  private def self.chain0(head, feature) : FeatureChain
    ->(ctx : DisplayContext, term : Term, postfix : String) do
      rest = ->(ctx : DisplayContext, term : Term, postfix : String) do
        raise TermPassthrough.new
      end

      feature.call(ctx, term, postfix, head, rest)
    end
  end

  private def self.chain0(head, feature, *features) : FeatureChain
    ->(ctx : DisplayContext, term : Term, postfix : String) do
      rest = ->(ctx : DisplayContext, term : Term, postfix : String) do
        chain0(head, *features).call(ctx, term, postfix)
      end
      feature.call(ctx, term, postfix, head, rest)
    end
  end

  # Creates a chain of *features*.
  def self.chain(feature, *features) : FeatureChain
    head = nil
    head = ->(ctx : DisplayContext, term : Term, postfix : String) do
      rest = chain0(head.not_nil!("cannot call head during chain initialization"), feature, *features)
      rest.call(ctx, term, postfix)
    end
  end
end

class TermPassthrough < Exception
  @callstack = CallStack.empty
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

def flatten(ctx, node : Term, maxwidth : Int32, layouts : LayoutSet) : {Term, Int32}
  Term.case(node) do
    matchpi %{[frag _]} do
      width, _ = measure(ctx, node)
      {node, maxwidth - width}
    end

    matchpi %{[block _]} do
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

      myself.each do |option|
        begin
          rendered = ctx.layouts.call(ctx.copy_with(layouts_allowed: children), option, subject, postfix.to(String))
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


layout_chain = Layout.chain(
  Layout::DictInline.new,
  Layout::CallIndented.new,
  Layout::MapInline.new,
  Layout::MapMultiline.new,
  Layout::MapMultilineIndented.new,
  Layout::DictAligned.new,
)

feature_chain = Feature.chain(
  Feature::Edge.new,
  Feature::BackrefMy.new,
  Feature::BackrefUp.new,
  Feature::BackrefDown.new,
  Feature::Hold.new,
  Feature::PatternSlot.new,
  Feature::PatternNonself.new,
  Feature::PatternLiteral.new,
  Feature::PatternLet.new,
  Feature::PatternItemFirst.new,
  Feature::PatternItemSource.new,
  Feature::IntegerGroupThousands.new,
  Feature::Literal.new,
  Feature::EmptyDict.new,
  Feature::Call.new,
  Feature::DataMap.new,
  Feature::DataDict.new
)

# pp flatten(ctx, feature_chain.call(ctx, Term.of(:"%item°", 100, 200, 300), ""))

# pp Feature::Edge.call(ctx, Term.of(:edge, 100), "))", ->(ctx : DisplayContext, term : Term, postfix : String) do
#                         raise "end of chain!!"
#                       end)

# ed = Term.of(JSON.parse(File.read("./data/people.json")))
ed = ML.terms(File.read("./editor.soma.wwml"))# Term.of(:+, {:*, 3, 4}, {2})
# ed = ML.terms %{(+ 1 2 3 4 5 x: 100 y: 200)}

str = String.build do |io|
  screen = Screen.new

  ed.items.each do |sexp|
    screen.clear
    ctx = DisplayContext.new(60, 120, feature_chain, layout_chain)
    tree = feature_chain.call(ctx, sexp, "")
    flat, excess = flatten(ctx, tree)
    # puts excess
    # puts ML.display(flat)
    draw(ctx, screen, flat, 0, 0)
    screen.write(io)
    io.puts
  end
end

puts str
# puts str == File.read("./pprint1.out.1")
pp ed == ML.terms(str)
