module Ww::M1
  # Tracing subsystem of M1.
  #
  # M1 *traces* explain how M1 got to a particular spot: the keys it followed,
  # the ranges it examined, the items it inserted, the entries it created or
  # removed, etc. on its way to the spot.
  #
  # Traces are like breadcrumb trails that M1 generates to let callers trace
  # the way later on, in case of a match.
  #
  # Traces are mainly constructed & maintained by `Tzip`, which is in turn used
  # by the rest of M1. Trace correctness depends on whether the trace is constructed
  # by `Tzip` (which basically guaranteed correctness, barring implementation errors).
  # Some traces that you can construct by hand are nonsense, the Trace equivalent of
  # the phrase "Colorless green ideas sleep furiously". On the other hand, traces
  # constructed by `Tzip` are guaranteed to make sense, since their extension
  # follows carefully the structure of the corresponding term.
  #
  # NOTE: You aren't expected to construct traces yourself, only inspect them, and
  # even then, the backmap engine will inspect them for you; so traces are a deep
  # internal of M1.
  module Trace
    extend self

    # Actions are the indivisible unit of traces.
    #
    # Actions are named in such a way as to answer the question: "What should
    # I do to proceed?" The client is asking this question repeatedly
    # as it processes traces.
    alias Action = ExamineKey |
                   ExamineValue |
                   ExamineBlankName |
                   ExamineBlankType |
                   ExamineItemspart |
                   ExaminePairspart |
                   ExamineRange |
                   ExamineResidue |
                   InsertEntry |
                   InsertItem

    # M1 descended into the key of a pair, *key* (i.e., as opposed to the *value*).
    #
    # This action is emitted by e.g. `(%entry ⏏k_⏏ v_)`. Notice how the capture *k*
    # refers to the key itself. Something like `(%entry (a_ b_) v_)` would descend
    # further into the key. With a matchee like `{(foo bar): 10}`,  the traces
    # associated with the key are going to be `examine key (foo bar)—examine value 0`
    # (for capture *a*) and `examine key (foo bar)—examine value 1` (for capture *b*, `bar`).
    defrecord ExamineKey, key : Term

    struct ExamineKey
      def inspect(io)
        io << "examine key " << key
      end
    end

    # M1 descended into the value of a pair with *key*.
    #
    # For example, given `{name: ⏏name_⏏, age: ⏏age_⏏}`, and matchee `{name: "John", age: 25}`,
    # you'll see capture *name* reached by `examine key name`, and similarly for *age*.
    defrecord ExamineValue, key : Term

    struct ExamineValue
      def inspect(io)
        io << "examine value " << key
      end
    end

    # M1 descended into an itemspart range defined by *begin* (inclusive)
    # and *end* (exclusive). Actions later in the sequence refer to items
    # in the range, so their 0 means *begin*, and their high end is *end*.
    #
    # This action is emitted as M1 descends into plurals (e.g. `(_ ⏏xs_*⏏ _)`),
    # groups (e.g. `(_ (%group G_ _*) _)`), etc. For groups in particular,
    # nested patterns as in `(_ (%group (a_ b_) _*) _)` are also under `ExamineRange`,
    # so the capture *a* would look like `examine range B..<E—examine key 0`, and
    # similarly for *b*.
    defrecord ExamineRange, begin : UInt32, end : UInt32, ord : UInt32 do
      assert @begin <= @end
    end

    struct ExamineRange
      def size : UInt32
        @end - @begin
      end

      def inspect(io)
        io << "examine range" << @ord.subscript << " " << @begin << "..<" << @end
      end
    end

    # M1 descended into symbol blank name (see also: `Term::Sym#blank?`)
    #
    # This action is emitted as M1 descends into `(%symbol blank ⏏name_⏏ type_)`.
    defrecord ExamineBlankName

    struct ExamineBlankName
      def inspect(io)
        io << "examine blank name"
      end
    end

    # M1 descended into symbol blank type (see also: `Term::Sym#blank?`)
    #
    # This action is emitted as M1 descends into `(%symbol blank name_ ⏏type_⏏)`.
    defrecord ExamineBlankType

    struct ExamineBlankType
      def inspect(io)
        io << "examine blank type"
      end
    end

    # M1 descended into dictionary itemspart.
    #
    # This action is emitted as M1 descends into `(%partition ⏏items_⏏ _)`
    # and related.
    defrecord ExamineItemspart

    struct ExamineItemspart
      def inspect(io)
        io << "examine itemspart"
      end
    end

    # M1 descended into dictionary pairspart.
    #
    # This action is emitted as M1 descends into `(%partition _ ⏏pairs_⏏)`
    # and related.
    defrecord ExaminePairspart

    struct ExaminePairspart
      def inspect(io)
        io << "examine pairspart"
      end
    end

    # M1 descended into the residue after removing *keys*.
    #
    # *keys* slice must be nonempty and read-only.
    #
    # This action is emitted as M1 descends into `(%layer ⏏rest_⏏ {x: x_, y: y_})`
    # and related (in this example removing `x` and `y`).
    defrecord ExamineResidue, removed : Slice(Term), part : Part = :any do
      assert @removed.size > 0
      assert @removed.read_only?
    end

    struct ExamineResidue
      enum Part
        # Undifferentiated `ExamineResidue`,
        Any
        # Itemspart residue. E.g. `(%partition (%layer ⏏rest_⏏ (_ _)) _)`.
        Itemspart
        # Pairspart residue. E.g. `(_* ¦ ⏏rest_⏏ x y)`
        Pairspart
      end

      def inspect(io)
        io << "examine " << part << " residue after removing "
        removed.join(io, ", ")
      end
    end

    # M1 created (pretended to create) an entry with *key* and descended
    # into its *value*.
    #
    # This action is emitted as M1 descends into entry optionals, for example
    # `{x: (%optional (0 0) ⏏(x_ y_)⏏)}`. Here, `x` is *key*, and `(0 0)`
    # is *value*, into which M1 descends to match `(x_ y_)`; both captures
    # *x* and *y* will follow this action, e.g. `insert entry x (0 0)—examine value 0`
    # for *x*.
    defrecord InsertEntry, key : Term, value : Term

    struct InsertEntry
      def inspect(io)
        io << "insert entry " << key << ": " << value
      end
    end

    # M1 inserted (pretended to insert) an item with the given *value*
    # before *index*.
    #
    # *ord* is used to order insertions before the same *index*.
    #
    # This action is emitted as M1 descends into sequence optionals and
    # related, for example: `(_ ⏏(%optional 0 x_number)⏏ ⏏(%optional 0 y_number)⏏ _)`.
    #
    # Notice how both optionals are designated for insertion before the same
    # index, `1`; if both are missing, *ord* is needed to disambiguate (it is
    # set based on their position in the pattern) -- so that `x_number` comes
    # before `y_number`.
    defrecord InsertItem, index : UInt32, ord : UInt32, value : Term

    struct InsertItem
      def inspect(io)
        io << "insert item" << ord.subscript << " " << value << " before " << index
      end
    end

    alias Any = None | Some
    alias Some = Primitive | Mapping
    alias Simple = None | Primitive
    alias Primitive = One | Many
    alias Sealed = SealedOne | SealedMany
    alias One = SeqOne | SealedOne
    alias Many = SeqMany | SealedMany
    # A single path through structure.
    alias SeqOne = IndexSeq | ActionSeq

    # Disables tracing/the zero trace/neutral element.
    record None do
      def inspect(io)
        io << "<ø>"
      end
    end

    # Optimizes storage for sequences of actions that consist of indices
    # (`UInt32`s) alone.
    record IndexSeq, indices : Pf::UPath32 do
      def inspect(io)
        io << "<"
        indices.join(io, "—") { |index| io << "examine value " << index }
        io << ">"
      end
    end

    # General action storage.
    record ActionSeq, actions : Slice(Action) do
      def inspect(io)
        io << "<"
        actions.join(io, "—")
        io << ">"
      end
    end

    # Storage for multiple `SeqOne`s.
    record SeqMany, children : Slice(One) do
      def inspect(io)
        io << "["
        children.join(io, "|")
        io << "]"
      end
    end

    # Sealed action storage.
    record SealedOne, seq : SeqOne do
      def inspect(io)
        io << "sealed"
        seq.inspect(io)
      end
    end

    # Storage for multiple `SealedOne`s.
    record SealedMany, children : Slice(SealedOne) do
      def inspect(io)
        io << "sealed["
        children.join(io, "|")
        io << "]"
      end
    end

    # A branching trace used for navigating *through* synthetic values back into
    # significant part(s) of the matchee (e.g. if you want to navigate through match
    # envs that `%items` or `%many` give to the successor pattern, to the corresponding
    # item or term).
    #
    # *handle* determines the trace that should be used for the synthetic value
    # itself (e.g. what does it mean to refer to the env that `%items` gives you *itself*,
    # as in *x* in `(%items (⏏x_⏏ y_) _)`, vs. what it means to refer to one of its entries,
    # which is what the *mapping* table answers).
    #
    # Since it would make no sense to append to *handle*, and the only valid/expected
    # appends are handled by *mapping*, we require *handle* to be `Sealed`. Therefore,
    # we can guarantee that mapping will never produce unexpected/invalid traces.
    #
    # "From here, different next actions correspond to different already-existing traces."
    defcase Mapping, handle : Sealed, mapping : Slice({ExamineKey | ExamineValue, Some}) do
      def inspect(io)
        handle.inspect(io)
        io << "{"
        mapping.join(io, ", ") do |(key, value)|
          io << "⸤"
          key.inspect(io)
          io << "⸣ => "
          value.inspect(io)
        end
        io << "}"
      end
    end

    # Constructs a `None` trace, which effectively disables tracing.
    def none : None
      None.new
    end

    # Constructs the appropriate empty root trace, which enables tracing.
    def root
      IndexSeq.new(Pf::UPath32[])
    end

    # :nodoc:
    def append(trace : None, action : Action)
      trace
    end

    # :nodoc:
    def append(trace : IndexSeq, action : Action)
      if action.is_a?(ExamineValue) && (i = index?(action.key))
        return IndexSeq.new(trace.indices.append(i))
      end

      actionsptr = Pointer(Action).malloc(trace.indices.size + 1)
      actionsptr[trace.indices.size] = action
      trace.indices.each_with_index do |index, i|
        actionsptr[i] = ExamineValue.new(Term.of(index))
      end

      actions = Slice.new(actionsptr, trace.indices.size + 1, read_only: true)
      ActionSeq.new(actions)
    end

    private def index?(term : Term) : UInt32?
      return unless n = term.as_n?

      n.index32?
    end

    # :nodoc:
    def append(trace : ActionSeq, action : Action)
      ActionSeq.new(trace.actions.append(action))
    end

    # :nodoc:
    def append(trace : SeqMany, action : Action)
      successors = Pf::Kit.stack_array(One, 8)

      trace.children.each do |child|
        successor = append(child, action)
        next unless successor.is_a?(One)

        successors << successor
      end

      case successors.size
      when 0 then none
      when 1 then successors.first
      else
        SeqMany.new(successors.to_readonly_slice(&.itself))
      end
    end

    # :nodoc:
    def append(trace : SealedMany, action : Action)
      none
    end

    # :nodoc:
    def partition(trace : Mapping) : {Mapping, Mapping}
      seq = Term[0]

      itemspart = Pf::Kit.stack_array({ExamineKey | ExamineValue, Some}, 4)
      pairspart = Pf::Kit.stack_array({ExamineKey | ExamineValue, Some}, 4)

      trace.mapping.each do |step, value|
        if (n = step.key.as_n?) && n.natural? && n <= seq
          itemspart << {step, value}
          seq += 1 if n == seq
        else
          pairspart << {step, value}
        end
      end

      {Mapping.new(trace.handle, itemspart.to_readonly_slice(&.itself)),
       Mapping.new(trace.handle, pairspart.to_readonly_slice(&.itself))}
    end

    # :nodoc:
    #
    # Support for things like [xs_*] on Mapping tzips.
    def append(trace : Mapping, action : ExamineItemspart)
      itemspart, _ = partition(trace)
      itemspart
    end

    # :nodoc:
    #
    # Support for things like {¦ y_} on Mapping tzips.
    def append(trace : Mapping, action : ExaminePairspart)
      _, pairspart = partition(trace)
      pairspart
    end

    # :nodoc:
    #
    # Support for things like (_ xs_*) or (l_* r_*) on Mapping tzips.
    def append(trace : Mapping, action : ExamineRange)
      entries = Pf::Kit.stack_array({ExamineKey | ExamineValue, Some}, 4)
      handle = Pf::Kit.stack_array(Trace::One, 4)

      trace.mapping.each do |step, value|
        next unless n = step.key.as_n?
        next unless index = n.index32?
        next unless action.begin <= index < action.end

        case step
        in ExamineKey
          entries << {ExamineKey.new(Term.of(index - action.begin)), value}
        in ExamineValue
          entries << {ExamineValue.new(Term.of(index - action.begin)), value}
          flatten(simplify(value)) do |one|
            handle << one
          end
        end
      end

      Mapping.new(
        handle: seal(SeqMany.new(handle.to_readonly_slice(&.itself))),
        mapping: entries.to_readonly_slice(&.itself),
      )
    end

    # :nodoc:
    def append(trace : Mapping, action : ExamineKey | ExamineValue)
      trace.mapping.each do |key, value|
        next unless key == action
        return value
      end

      none
    end

    # :nodoc:
    def append(trace : Mapping, action)
      none
    end

    # :nodoc:
    def append(trace : SealedOne, action)
      none
    end

    {% if flag?(:docs) %}
      # Appends *action* to *trace*.
      #
      # Append is the core operation on `trace`s.
      # - Sealed traces absorb it by returning `None`.
      # - Mappings pick the appropriate branch if possible; otherwise, they too
      #   return `None`.
      # - `Many`s append to their children.
      # - `SeqOne`s append to themselves, converting between each other appropriately.
      def append(trace : Any, action : Action) : Any
      end
    {% end %}

    # :nodoc:
    def flatten(trace : None, & : One ->) : Nil
    end

    # :nodoc:
    def flatten(trace : One | SealedOne, & : One ->) : Nil
      yield trace
    end

    # :nodoc:
    def flatten(trace : SeqMany | SealedMany, & : One ->) : Nil
      trace.children.each { |child| yield child }
    end

    {% if flag?(:docs) %}
      # Converts *trace* to a stream of `One` traces: each such trace is yielded.
      def flatten(trace : Simple, & : One ->) : Nil
      end
    {% end %}

    # Joins two traces *a* and *b* with an OR, in order. This returns `Many` in most
    # cases, unless one of *a* or *b* is `None`.
    def join(a : Any, b : Any) : Any
      # NOTE: Although I prefer overloads for this kind of stuff, here we'd spend
      # too many lines doing nothing in particular. case...in...then is much
      # more compact.
      case {a, b}
      in {_, None}                then a
      in {None, _}                then b
      in {One, One}               then SeqMany.new(Slice[a.as(One), b.as(One)])
      in {SeqMany, One}           then SeqMany.new(a.children.append(b))
      in {One, SeqMany}           then SeqMany.new(b.children.prepend(a))
      in {SealedMany, One}        then SeqMany.new(a.children.append(b, &.as(One)))
      in {One, SealedMany}        then SeqMany.new(b.children.prepend(a, &.as(One)))
      in {SealedMany, SealedOne}  then SealedMany.new(a.children.append(b))
      in {SealedOne, SealedMany}  then SealedMany.new(b.children.prepend(a))
      in {SeqMany, SeqMany}       then SeqMany.new(a.children + b.children)
      in {SealedMany, SealedMany} then SealedMany.new(a.children + b.children)
      in {SeqMany, SealedMany}    then SeqMany.new(a.children.append_many(b.children, &.as(One)))
      in {SealedMany, SeqMany}    then SeqMany.new(b.children.prepend_many(a.children, &.as(One)))
      in {Mapping, _}             then join(a.handle, b)
      in {_, Mapping}             then join(a, b.handle)
      end
    end

    # :nodoc:
    def seal(trace : Sealed | None) : Sealed | None
      trace
    end

    # :nodoc:
    def seal(trace : ActionSeq | IndexSeq) : Sealed | None
      SealedOne.new(trace)
    end

    # :nodoc:
    def seal(trace : SeqMany) : Sealed | None
      SealedMany.new(trace.children.map { |a| seal(a) })
    end

    {% if flag?(:docs) %}
      # Seals the given *trace*. Sealing prevents the trace from being appended to
      # in the future: appends to a sealed trace result in `None`.
      def seal(trace : Simple) : Sealed | None
      end
    {% end %}

    # Constructs a `SealedOne` or `SealedMany` trace for *objects*, which are
    # transformed to traces using the given block.
    def seal(objects : Enumerable(T), & : T -> Any) : Sealed | None forall T
      children = Pf::Kit.stack_array(SealedOne, 8)

      objects.each do |object|
        trace = yield object

        flatten(simplify(trace)) do |child|
          children << seal(child)
        end
      end

      case children.size
      when 0 then none
      when 1 then children.unsafe_fetch(0)
      else
        SealedMany.new(children.to_readonly_slice(&.itself))
      end
    end

    # :nodoc:
    def simplify(trace : Simple) : Simple
      trace
    end

    # :nodoc:
    def simplify(trace : Mapping) : Simple
      trace.handle
    end

    {% if flag?(:docs) %}
      # Simplifies `Any` trace to obtain a `Simple` trace, which most other `Trace`
      # methods expect.
      #
      # Since `Simple` is `Any` without `Mapping`, this method basically gets
      # rid of `Mapping` for you by returning its *handle*, which is
      # already primitive.
      def simplify(trace : Any) : Simple
      end
    {% end %}

    # :nodoc:
    def keypath?(trace : IndexSeq) : Term::Dict?
      Term::Dict.build do |commit|
        trace.indices.each { |index| commit << index }
      end
    end

    # :nodoc:
    def keypath?(trace : ActionSeq) : Term::Dict?
      return unless trace.actions.all?(ExamineValue)

      Term::Dict.build do |commit|
        trace.actions.each do |action|
          assert action.is_a?(ExamineValue)

          commit << action.key
        end
      end
    end

    # :nodoc:
    def keypath?(trace : SealedOne) : Term::Dict?
      keypath?(trace.seq)
    end

    # :nodoc:
    def keypath?(trace : Many) : Term::Dict?
      trace.children.leftmost? { |child| keypath?(child) }
    end

    {% if flag?(:docs) %}
      # Converts *trace* into a keypath if possible. Returns `nil` otherwise.
      def keypath?(trace : IndexSeq | ActionSeq | SealedOne | Many) : Term::Dict?
      end
    {% end %}

    # :nodoc:
    def size(trace : IndexSeq) : Int32
      trace.indices.size
    end

    # :nodoc:
    def size(trace : ActionSeq) : Int32
      trace.actions.size
    end

    {% if flag?(:docs) %}
      # Returns the number of actions in *trace*.
      def size(trace : SeqOne) : Int32
      end
    {% end %}

    # :nodoc:
    def nth(trace : IndexSeq, n : Int32) : Action
      ExamineValue.new(Term.of(trace.indices[n]))
    end

    # :nodoc:
    def nth(trace : ActionSeq, n : Int32) : Action
      trace.actions[n]
    end

    {% if flag?(:docs) %}
      # Returns the *n*th action in *trace*. Raises `IndexError` if *n* is out
      # of bounds.
      def nth(trace : SeqOne, n : Int32) : Action
      end
    {% end %}

    # A Slice-like wrapper around `SeqOne` traces.
    struct SeqSlice
      include Indexable(Action)

      def initialize(@trace : SeqOne)
        @begin = 0
      end

      # :nodoc:
      def initialize(@trace : SeqOne, @begin : Int32)
      end

      def size : Int32
        Trace.size(@trace) - @begin
      end

      def unsafe_fetch(index : Int)
        Trace.nth(@trace, @begin + index)
      end

      def +(offset : Int) : SeqSlice
        assert 0 <= offset <= size

        SeqSlice.new(@trace, @begin + offset)
      end
    end

    # :nodoc:
    def normalize(trace : IndexSeq) : SeqOne | None
      trace
    end

    # :nodoc:
    def normalize(trace : ActionSeq) : SeqOne | None
      prior = Pf::Kit.stack_array(Action, 16)
      last = nil
      changed = false

      trace.actions.each do |action|
        if last.nil?
          last = action
          next
        end

        case {last, action}
        when {ExamineItemspart, ExamineItemspart}
          # ExamineItemspart—ExamineItemspart -> ExamineItemspart
          changed = true
          next
        when {ExaminePairspart, ExaminePairspart}
          # ExaminePairspart—ExaminePairspart -> ExaminePairspart
          changed = true
          next
        when {ExamineItemspart, ExaminePairspart}
          # ExamineItemspart—ExaminePairspart -> abort with None
          return none
        when {ExaminePairspart, ExamineItemspart}
          # ExaminePairspart—ExamineItemspart -> abort with None
          return none
        when {ExamineItemspart, ExamineResidue}
          # ExamineItemspart—ExamineResidue(R) -> ExamineResidue(R, part: itemspart)
          last = ExamineResidue.new(action.removed, part: :itemspart)
          changed = true
          next
        when {ExaminePairspart, ExamineResidue}
          # ExamineItemspart—ExamineResidue(R) -> ExamineResidue(R, part: pairspart)
          last = ExamineResidue.new(action.removed, part: :pairspart)
          changed = true
          next
        when {ExamineItemspart, _}, {ExaminePairspart, _}
          # ExamineItemspart—X, ExaminePairspart—X
          last = action
          changed = true
          next
        when {ExamineRange, ExamineRange}
          # ExamineRange(b0, e0, ord0)—ExamineRange(b1, e1, ord1) -> ExamineRange(b0 + b1, b0 + e1, ord0)
          last = ExamineRange.new(
            begin: last.begin + action.begin,
            end: last.begin + action.end,
            ord: last.ord,
          )
          changed = true
          next
        when {ExamineRange, ExamineValue}
          # ExamineRange(b0, e0, ord0)—ExamineValue(key in b0...e0) -> ExamineValue(b0, e0)
          if (index = action.key.index32?) && index < last.size
            last = ExamineValue.new(Term.of(last.begin + index))
            changed = true
            next
          end
        end

        prior << last
        last = action
      end

      unless changed
        return trace
      end

      if last
        prior << last
      end

      ActionSeq.new(prior.to_readonly_slice(&.itself))
    end

    {% if flag?(:docs) %}
      # Normalizes the given *trace*.
      #
      # Normalization is especially important for the backmap engine. It removes
      # sequences of actions that are:
      #
      # - reduntant (e.g. `ExamineItemspart—ExamineItemspart`)
      # - un-backmappable (e.g. `ExamineItemspart—ExaminePairspart`; although this trace
      #   makes sense in general, it does not make sense for the backmap engine).
      # - cause surprising backmap behavior (e.g. nested `ExamineRange`s, as in
      #   `ExamineRange(3..<10)—ExamineRange(0..<2)` or `ExamineRange(3..<10)—ExamineValue(0)`).
      #
      # #### On sequences like `ExamineItemspart—ExaminePairspart`
      #
      # It makes no sense to refer to the pairspart of an itemspart in traces, not
      # because it's wrong (the pairspart of an itemspart is well-defined and is
      # the empty dict); but because you can't *manipulate* the pairspart of
      # an itemspart -- doing so would invalidate the itemspart'ness of
      # the itemspart, and so, the very premise/subject of manipulation.
      def normalize(trace : SeqOne) : SeqOne | None
      end
    {% end %}
  end
end
