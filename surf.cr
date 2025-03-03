require "./src/wirewright"

# Sensors:
#   pattern
#   -> normal
#   -> pattern skeleton
#   -> dnf (array of patterns)
#   -> pattern strands for each skeleton (dnf)
#   -> add to utrie
#   -> mount in to xgraph

# Binarizes and simplifies nested/long `%all` *node*.
 def all2(node) : Term
  Term.case(node) do
    matchpi %{(%'%all)} { M1::Normal::NORMAL_PASS }
    matchpi %{(%'%all a_)} { a }
    matchpi %{(%'%all a_ %'(%pass))} { a }
    matchpi %{(%'%all %'(%pass) b_)} { b }
    matchpi %{(%'%all _ _)} { Term.of(node) }
    matchpi %{(%'%all a_ b_ rest_+)} do
      a1 = Term.of(:"%all", a, b)
      b1 = rest.prepend(:"%all")

      all2(Term.of(:"%all", all2(a1), all2(b1)))
    end
  end
end

# *Pattern skeleton* is a restricted, more open subset of pattern matching constructs
# that we are able to index efficiently. Any M1 pattern can be converted into its skeleton
# with more or less loss.
#
# Pattern skeleton is guaranteed to consist only of the following nodes:
#
# - `(%'%value (%'%literal _) _)`
# - `(%'%any/source _+)`
# - `(%'%all a_ b_)`
# - `%'(%pass)`
# - `%'(%symbol)`
# - `%'(%string)`
# - `%'(%boolean)`
# - `%'(%dict)`
# - `%'(%number _)`
# - `(%'%literal X_)` with non-dict X
module Skeleton
  extend self

  # Generates a sequence of *subject* itemseq calls repeated *n* times.
  private def repeated(prefix, key, subject, n, ahead0) : Term
    if n.zero?
      return ahead0.call(prefix, key)
    end

    ahead1 = ->(prefix : Term::Dict, key : Term::Num) do
      repeated(prefix, key, subject, n - 1, ahead0)
    end

    itemseq(prefix, key, subject, ahead1)
  end

  # Returns the skeleton of an itemseq *item*.
  private def itemseq(prefix, key, item : Term, ahead) : Term
    Term.of_case(item) do
      # Fetch successor.
      matchpi %{(%'%singular successor_)} do
        prefix = prefix.append({:"%value", {:"%literal", key}, pattern(successor)})

        ahead.call(prefix, key + 1)
      end

      # Expand small bounded %plural's into a disjunction with each possible length.
      matchpi(
        %{((%any %plural %plural/min %plural/max) (%optional untitled _) ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8)
            type_symbol)}
      ) do
        case type.blank.type
        in .any?     then unit = M1::Normal::NORMAL_PASS
        in .number?  then unit = M1::Normal::NORMAL_BLANK_NUMBER
        in .string?  then unit = M1::Normal::NORMAL_BLANK_STRING
        in .symbol?  then unit = M1::Normal::NORMAL_BLANK_SYMBOL
        in .dict?    then unit = M1::Normal::NORMAL_BLANK_DICT
        in .boolean? then unit = M1::Normal::NORMAL_BLANK_BOOLEAN
        end

        Term::Dict.build do |disj|
          disj << :"%any/source"

          (min.to(Int32)..max.to(Int32)).each do |hi|
            variant = prefix

            hi.times do |length|
              variant = variant.append({:"%value", {:"%literal", key + length}, unit})
            end

            disj << ahead.call(variant, key + hi + 1)
          end
        end
      end

      # Dive into %group's.
      matchpi %{(%'%group _ children_+)} do
        itemseq(prefix, key, children.items, ahead)
      end

      # Expand small bounded %past and %many's into a disjunction with each
      # possible length.
      matchpi(
        %{(%'%past children_+ ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8)
            greedy_boolean: _)},
        %{(%'%many _ children_+ ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8))}
      ) do
        Term::Dict.build do |disj|
          disj << :"%any/source"

          (min.to(Int32)..max.to(Int32)).each do |hi|
            disj << repeated(prefix, key, children.items, hi, ahead)
          end
        end
      end

      # Optionals expand into a disjunction with and without the item.
      matchpi %{(%'%optional _ body_)} do
        variant0 = ahead.call(prefix, key)
        variant1 = ahead.call(prefix.append({:"%value", {:"%literal", key}, pattern(body)}), key + 1)

        Term.of(:"%any/source", variant0, variant1)
      end

      otherwise { prefix }
    end
  end

  # Returns the skeleton of an itemseq in *feed*.
  private def itemseq(prefix, key, feed : Term::Dict::ItemsView, ahead0) : Term
    unless item = feed.first?
      return ahead0.call(prefix, key)
    end

    ahead1 = ->(prefix : Term::Dict, key : Term::Num) do
      itemseq(prefix, key, feed.move(1), ahead0)
    end

    itemseq(prefix, key, item, ahead1)
  end

  # Returns the skeleton of an itemseq *seq*.
  def itemseq(seq : Term::Dict)
    ahead = ->(prefix : Term::Dict, key : Term::Num) { all2(prefix) }

    itemseq(Term.dict(:"%all"), Term[0], seq.items, ahead)
  end

  private def entry(prefix, key, value, ahead) : Term
    Term.of_case(value) do
      matchpi %{(%'%entry/required successor_)} do
        prefix = prefix.append({:"%value", {:"%literal", key}, pattern(successor)})

        ahead.call(prefix)
      end

      matchpi %{(%'%entry/optional _ successor_)} do
        variant0 = ahead.call(prefix)
        variant1 = ahead.call(prefix.append({:"%value", {:"%literal", key}, pattern(successor)}))

        {:"%any/source", variant0, variant1}
      end

      otherwise do
        ahead.call(prefix)
      end
    end
  end

  private def entries(prefix : Term::Dict, n, entries, ahead0) : Term
    unless entry = entries.nth?(n)
      return ahead0.call(prefix)
    end

    ahead1 = ->(prefix : Term::Dict) do
      entries(prefix, n + 1, entries, ahead0)
    end

    entry(prefix, *entry, ahead1)
  end

  # Returns the skeleton of *entries*.
  def entries(entries : Term::Dict) : Term
    if entries.empty?
      return Term.of({:"%dict"})
    end

    ahead = ->(prefix : Term::Dict) { all2(prefix) }

    entries(Term.dict(:"%all"), 0, entries, ahead)
  end

  # Returns the skeleton of a normal pattern *normp*.
  def pattern(normp : Term) : Term
    Term.of_case(normp) do
      matchpi %{(%'%pass)} { normp }
      matchpi %{(%'%symbol)} { normp }
      matchpi %{(%'%string)} { normp }
      matchpi %{%'(%number _)} { normp }
      matchpi %{(%'%boolean)} { normp }
      matchpi %{(%'%dict)} { normp }

      matchpi %{(%'%literal x_dict)} { pattern(M1.normal_escaped(x)) }
      matchpi %{(%'%literal _)}  { normp }

      matchpi %{(%'%let _ successor_)} do
        pattern(successor)
      end

      matchpi %{(%'%itemseq successors_+)} do
        itemseq(successors.unsafe_as_d)
      end

      matchpi %{(%'%partition itemspart_ pairspart_)} do
        all2(Term.of(:"%all", pattern(itemspart), pattern(pairspart)))
      end

      # In pattern skeleton, all layers are always open. So we cannot make
      # literal belows closed. However we still account them during matching
      # for precision.
      begin
        # Do not emit useless %dict checks for below.
        matchpi %{(%'%layer (%'%literal ()) side_dict)} do
          entries(side.unsafe_as_d)
        end

        matchpi %{(%'%layer below_ side_dict)} do
          all2(Term.of(:"%all", pattern(below), entries(side.unsafe_as_d)))
        end
      end

      matchpi %{(%'%any/source successors_+)} do
        Term::Dict.build do |commit|
          commit << :"%any/source"

          successors.each_item_unordered do |item|
            commit << pattern(item)
          end
        end
      end

      matchpi %{(%'%any/literal options_+)} do
        Term::Dict.build do |commit|
          commit << :"%any/source"

          options.each_item_unordered do |item|
            commit << pattern(M1.normal_escaped(item))
          end
        end
      end

      matchpi %{(%'%all a_ b_)} do
        all2(Term.of(:"%all", pattern(a), pattern(b)))
      end

      matchpi(
        %{(%'%number %'(whole _))},
        %{(%'%number _ _ _)},
        %{(%'%number _ _ _ _ _)},
        %{(%'%pipe (%barrier (+ _number)) _)},
        %{(%'%pipe (%barrier (- _number)) _)},
        %{(%'%pipe (%barrier (* _number)) _)},
        %{(%'%pipe (%barrier (/ _number)) _)},
        %{(%'%pipe (%barrier (div _number)) _)},
        %{(%'%pipe (%barrier (mod _number)) _)},
        %{(%'%pipe (%barrier (** _number)) _)},
      ) { M1::Normal::NORMAL_BLANK_NUMBER }

      matchpi(
        %{(%'%pipe (map _) _)},
        %{(%'%pipe type _)},
      ) { M1::Normal::NORMAL_PASS }

      matchpi %{(%'%pipe span _)} { M1::Normal::NORMAL_BLANK_STRING }
      matchpi %{(%'%pipe tally _)} { M1::Normal::NORMAL_BLANK_DICT }

      matchpi %{(%'%symbol nonblank)}, %{(%'%symbol blank _ _)} do
        M1::Normal::NORMAL_BLANK_SYMBOL
      end

      matchpi %{(%'%terminal node_)} do
        pattern(node)
      end

      otherwise do
        M1::Normal::NORMAL_PASS
      end
    end
  end
end

private def branches(skeleton : Term, ahead0 : Term ->) : Nil
  Term.case(skeleton) do
    matchpi %{(%'%value (%'%literal _) value_)} do
      ahead1 = ->(branch : Term) do
        ahead0.call(Term.of(skeleton.with(2, branch)))
      end

      branches(value, ahead1)
    end

    matchpi %{(%'%all a_ b_)} do
      ahead2 = ->(branch0 : Term) do
        ahead1 = ->(branch1 : Term) do
          ahead0.call(all2(Term.of(:"%all", branch0, branch1)))
        end

        branches(b, ahead1)
      end

      branches(a, ahead2)
    end

    matchpi %{(%'%any/source children_+)} do
      children.items.each do |child|
        branches(child, ahead0)
      end
    end

    otherwise do
      ahead0.call(skeleton)
    end
  end
end

# Normalizes pattern skeleton to DNF. Calls *sink* with each toplevel branch.
#
# As long as *skeleton* is a pattern skeleton, branches given to *sink* are guaranteed
# to be pattern skeletons without `%any/source`.
#
# Non-skeleton nodes are unexpected and will not be processed.
def branches(skeleton : Term, &sink : Term ->) : Nil
  branches(skeleton, sink)
end

private def strands(prefix : Term::Dict, branch : Term, sink) : Nil
  Term.case(branch) do
    matchpi %{(%'%pass)} { sink.call(prefix) }

    matchpi %{%'(%number _)}, %{%'(%string)}, %{%'(%symbol)}, %{%'(%boolean)}, %{%'(%dict)} do
      sink.call(prefix.append(branch))
    end

    matchpi %{(%'%literal _number)} do
      sink.call(prefix.append(M1::Normal::NORMAL_BLANK_NUMBER).append(branch))
    end

    matchpi %{(%'%literal _string)} do
      sink.call(prefix.append(M1::Normal::NORMAL_BLANK_STRING).append(branch))
    end

    matchpi %{(%'%literal _symbol)} do
      sink.call(prefix.append(M1::Normal::NORMAL_BLANK_SYMBOL).append(branch))
    end

    matchpi %{(%'%literal _boolean)} do
      sink.call(prefix.append(M1::Normal::NORMAL_BLANK_BOOLEAN).append(branch))
    end

    matchpi %{(%'%all a_ b_)} do
      strands(prefix, a, sink)
      strands(prefix, b, sink)
    end

    matchpi %{(%'%value (%'%literal _) successor_)} do
      prefix = prefix
        .append(M1::Normal::NORMAL_BLANK_DICT)
        .append(branch.without(2))

      strands(prefix, successor, sink)
    end
  end
end

# Calls *sink* with each strand (represented as an itemsonly dict) of *branch*.
#
# A strand is an exhaustive path through `%all` nodes in *branch*.
#
# Each strand consists of *bases*. The following list is an exhaustive list
# of bases:
#
# - `(%'%value (%'%literal _))`
# - `%'(%symbol)`
# - `%'(%string)`
# - `%'(%number _)`
# - `%'(%boolean)`
# - `%'(%dict)`
# - `(%'%literal _)`
def strands(branch : Term, &sink : Term::Dict ->)
  strands(Term[], branch, sink)
end

class Tbase
  module Key
  end

  module Value
  end
end

module Ubase
  alias Any = At | IsSym | IsStr | IsNum | IsBool | IsDict | Literal

  record At, key : Term
  record IsSym
  record IsStr
  record IsNum
  record IsBool
  record IsDict
  record Literal, value : Term

  def self.parse(base : Term)
    Term.case(base) do
      matchpi %{(%'%value (%'%literal key_))} { At.new(key) }
      matchpi %{%'(%symbol)} { IsSym.new }
      matchpi %{%'(%string)} { IsStr.new }
      matchpi %{%'(%number _)} { IsNum.new }
      matchpi %{%'(%boolean)} { IsBool.new }
      matchpi %{%'(%dict)} { IsDict.new }
      matchpi %{(%'%literal value_)} { Literal.new(value) }
    end
  end

  def self.from(type : TermType) : Ubase::Any
    case type
    in .any?     then raise ArgumentError.new
    in .boolean? then IsBool.new
    in .number?  then IsNum.new
    in .string?  then IsStr.new
    in .symbol?  then IsSym.new
    in .dict?    then IsDict.new
    end
  end
end

alias Label = UInt64
alias Refcount = UInt32

VERTEX_NONE = Label.new(0)
VERTEX_ROOT = VERTEX_NONE + 1
VERTEX_ZERO = VERTEX_ROOT + 1

struct Utrie
  record Node, pred : Label, base : Ubase::Any do
    include Tbase::Key
  end

  record Props, refcount : Refcount, successor : Label do
    include Tbase::Value

    def incref : Props
      copy_with(refcount: refcount + 1)
    end

    def decref? : {Props, Bool}
      {copy_with(refcount: refcount - 1), refcount == 1}
    end
  end

  def initialize(@storage : ExtrinsicMap(Node, Props))
  end

  def mount(pred : Label, base : Ubase::Any, fresh) : {Label, Bool}
    props = @storage.ref(Node.new(pred, base)) { Props.new(0u32, fresh.call) }

    {props.successor, props.refcount == 1}
  end

  def mount(strand : Enumerable(T), fresh, & : T -> Ubase::Any) : {Label, Bool} forall T
    added = false
    vertex = strand.reduce(VERTEX_ROOT) do |pred, base|
      # After the first added = true, all remaining mounts will also be
      # added = true.
      succ, added = mount(pred, (yield base), fresh)
      succ
    end

    {vertex, added}
  end

  def mount(strand : Enumerable(Ubase::Any), fresh) : {Label, Bool}
    mount(strand, fresh, &.itself)
  end

  # NOTE: the caller guarantees that *base* was mounted with *pred* before.
  def unmount(pred : Label, base : Ubase::Any) : {Label, Bool}
    props = @storage.unref(Node.new(pred, base))

    {props.successor, props.refcount.zero?}
  end

  def unmount(strand : Enumerable(T), & : T -> Ubase::Any) : {Label, Bool} forall T
    removed0 = false
    vertex = strand.reduce(VERTEX_ROOT) do |pred, base|
      succ, removed = unmount(pred, yield base)
      # Removal along the path would mean obstruction of the rest of the path
      # where (assuming all refcounts are proper) we have all refcounts = 1.
      removed0 ||= removed
      succ
    end

    {vertex, removed0}
  end

  def unmount(strand : Enumerable(Ubase::Any)) : {Label, Bool}
    unmount(strand, &.itself)
  end

  private def successor?(node : Node) : Label?
    props = @storage.get?(node)
    props ? props.successor : nil
  end

  {% for type, base in {Term::Num => Ubase::IsNum, Term::Str => Ubase::IsStr, Term::Sym => Ubase::IsSym, Term::Boolean => Ubase::IsBool} %}
    private def query(pred : Label, term : {{type}}, sink : Label ->) : Nil
      return unless successor0 = successor?(Node.new(pred, {{base}}.new))

      sink.call(successor0)

      if successor1 = successor?(Node.new(successor0, Ubase::Literal.new(Term.of(term))))
        sink.call(successor1)
      end
    end
  {% end %}

  # NOTE: dictionaries must be normalized into IsDict - At(), even literal ones.
  # We do not handle Literal(dict).
  private def query(pred : Label, term : Term::Dict, sink : Label ->) : Nil
    return unless successor0 = successor?(Node.new(pred, Ubase::IsDict.new))

    sink.call(successor0)

    term.each_entry do |key, value|
      next unless successor1 = successor?(Node.new(successor0, Ubase::At.new(key)))

      sink.call(successor1)

      query(successor1, value.downcast, sink)
    end
  end

  private def query(term : Term, sink : Label ->) : Nil
    query(VERTEX_ROOT, term.downcast, sink)
  end

  def query(term : Term, &sink : Label ->) : Nil
    query(term, sink)
  end
end

struct Xgraph
  record Node, a : Label, b : Label do
    include Tbase::Key
  end

  record Props, refcount : Refcount, successor : Label do
    include Tbase::Value

    def incref : Props
      copy_with(refcount: refcount + 1)
    end

    def decref? : {Props, Bool}
      {copy_with(refcount: refcount - 1), refcount == 1}
    end
  end

  def initialize(@data : ExtrinsicMap(Node, Props))
  end

  # Mounts an Xgraph rule *xrule*.
  #
  # *fresh* is a callable that generates fresh (Tspace-unique) Vertices.
  #
  # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
  # by passing it to this method.
  def mount(xrule : Deque(Label), fresh)
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      props = @data.ref(Node.new(a, b)) { Props.new(0u32, fresh.call) }

      xrule << props.successor
    end

    xrule[0]
  end

  # Unmounts an Xgraph rule *xrule*.
  #
  # Returns the vertex of the rule that was unmounted (so that the caller
  # perhaps deletes it in its own data structures).
  #
  # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
  # by passing it to this method.
  #
  # WARNING: the caller guarantees that *xrule* was mounted.
  def unmount(xrule : Deque(Label)) : Label
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      props = @data.unref(Node.new(a, b))

      xrule << props.successor
    end

    xrule[0]
  end

  # :nodoc:
  def conjs(vertices : Deque(Label), sink : Label ->)
    while a = vertices.shift?
      sink.call(a)

      (0...vertices.size).each do |i|
        b = vertices.unsafe_fetch(i)
        next unless props = @data.get?(Node.new(a, b))

        vertices << props.successor
      end
    end
  end

  # Calls *sink* with all mounted conjunction vertices in *vertices*.
  #
  # NOTE: *vertices* must be pre-sorted ascending. You lose ownership of *vertices*
  # by passing it to this method.
  def conjs(vertices : Deque(Label), &sink : Label ->)
    conjs(vertices, sink)
  end
end

struct Ttrie
  record Node, pred : Label, base : Ubase::Any do
    include Tbase::Key
  end

  record Props, refcount : Refcount, successor : Label do
    include Tbase::Value

    def incref : Props
      copy_with(refcount: refcount + 1)
    end

    def decref? : {Props, Bool}
      {copy_with(refcount: refcount - 1), refcount == 1}
    end
  end

  def initialize(@data : ExtrinsicMap(Node, Props))
  end

  def mount(pred : Label, base : Ubase::Any, fresh) : Label
    props = @data.ref(Node.new(pred, base)) { Props.new(0u32, fresh.call) }
    props.successor
  end

  # NOTE: the caller guarantees that *base* was mounted with *pred* before.
  def unmount(pred : Label, base : Ubase::Any) : Label
    props = @data.unref(Node.new(pred, base))
    props.successor
  end

  # Mounts *strand*.
  #
  # Returns the new *fresh* vertex.
  def mount(strand : Enumerable(Term), endpoint : Label, fresh) : Slice(Label)
    tip = nil

    path = [VERTEX_ROOT]

    strand.each do |term|
      if tip
        path << mount(path.last, Ubase::IsDict.new, fresh)
        path << mount(path.last, Ubase::At.new(tip), fresh)
      end

      tip = term
    end

    if tip
      path << mount(path.last, Ubase.from(tip.type), fresh)
      path << mount(path.last, Ubase::Literal.new(tip), fresh)
    end

    path << endpoint
    path.to_readonly_slice
  end

  # Unmounts *strand*.
  #
  # Returns the path to its endpoint.
  def unmount(strand : Enumerable(Term), endpoint : Label) : Slice(Label)
    tip = nil

    path = [VERTEX_ROOT]

    strand.each do |term|
      if tip
        path << unmount(path.last, Ubase::IsDict.new)
        path << unmount(path.last, Ubase::At.new(tip))
      end

      tip = term
    end

    if tip
      path << unmount(path.last, Ubase.from(tip.type))
      path << unmount(path.last, Ubase::Literal.new(tip))
    end

    path << endpoint
    path.to_readonly_slice
  end

  # Calls *fn* with the set of endpoints at the end of *strand*.
  def query?(strand : Enumerable(Ubase::Any)) : {Label, Label}?
    pred0 = VERTEX_ROOT
    pred1 = VERTEX_ROOT

    strand.each do |base|
      # The strand embedded in @data must be >= the query strand.
      return unless props = @data.get?(Node.new(pred1, base))

      pred0 = pred1
      pred1 = props.successor
    end

    {pred0, pred1}
  end
end

struct Etrace
  module Key
    include Tbase::Key
  end

  module Value
    include Tbase::Value
  end

  record Node, pred : Label, vertex : Label do
    include Key
  end

  record Props, refcount : Refcount, oid : Label do
    include Value
  end

  record SuccessorCount, oid : Label do
    include Key
  end

  record SuccessorList, oid : Label, index : Label do
    include Key
  end

  record Successor, oid : Label, vertex : Label do
    include Key
  end

  # TODO: Identity
  record Presence do
    include Value
  end

  record Count, value : Refcount do
    include Value
  end

  record Ref, vertex : Label do
    include Value
  end

  def initialize(@data : ExtrinsicMap(Key, Value))
  end

  # TODO: use submaps

  def mount(path : Slice(Label), fresh)
    if path.size < 2
      raise ArgumentError.new
    end

    if path[0] == VERTEX_ROOT
      path = path[1..]
    end

    pred = VERTEX_ROOT

    # Create nodes for each step and incref. This way we'll make sure they're
    # not removed by someone else while we're working at them later on.
    oids = path.map do |step|
      oid = @data.transaction(Node.new(pred, step)) do |tx|
        node0 = tx.value?.as(Props?)
        node1 = node0 ? node0.copy_with(refcount: node0.refcount + 1) : Props.new(1u32, fresh.call)
        tx.set(node1)
        node1.oid
      end
      pred = step
      oid
    end

    # If we succeed in adding a Successor, then we're responsible for
    # incrementing successor count and inserting into the SuccessorList.
    (0...path.size - 1).each do |index|
      oid = oids[index]

      successor = Successor.new(oid, w = path[index + 1])
      next if @data.get?(successor)

      @data.set(successor, Presence.new)

      # TODO: @data.ref
      count11 = @data.transaction(SuccessorCount.new(oid)) do |tx|
        count0 = tx.value?.as(Count?)
        count1 = count0 ? count0.copy_with(value: count0.value + 1) : Count.new(1u32)
        tx.set(count1)
        count1
      end

      @data.set(SuccessorList.new(oid, count11.value - 1), Ref.new(w))
    end
  end

  def unmount(path : Slice(Label))
    if path.size < 2
      raise ArgumentError.new
    end

    if path[0] == VERTEX_ROOT
      path = path[1..]
    end

    pred = VERTEX_ROOT

    oids = path.compact_map do |step|
      oid, removed = @data.transaction(Node.new(pred, step)) do |tx|
        current = tx.value.as(Props)
        if current.refcount == 1
          tx.del
          {current.oid, true}
        else
          tx.set current.copy_with(refcount: current.refcount - 1)
          {current.oid, false}
        end
      end

      pred = step

      # Keep only oids which we've removed. We're responsible for their
      # cleanup then.
      removed ? oid : nil
    end

    oids.each do |oid|
      # Successor list may not necessarily exist for endpoint vertices.
      # Deletion may fail, and we're fine with that.
      next unless count = @data.del?(SuccessorCount.new(oid)).as(Count?)

      (0u32...count.value).each do |index|
        successor = @data.del(SuccessorList.new(oid, index)).as(Ref)

        @data.del(Successor.new(oid, successor.vertex))
      end
    end
  end

  def walk(u : Label, v : Label, &fn : Label ->)
    fn.call(v)

    return unless props = @data.get?(Node.new(u, v)).as(Props?)

    # NOTE: while walking, the node along with its attributes could get deleted/
    # be in the process of being deleted. So at any point where we're reading
    # from the map, we must handle the absence-case, even if it seems like it
    # is impossible.
    return unless count = @data.get?(SuccessorCount.new(props.oid)).as(Count?)

    (0u32...count.value).each do |index|
      # We're fine with gaps. They could happen under some successor count
      # increment + successor list insert orderings. We're bounded anyway.
      next unless w = @data.get?(SuccessorList.new(props.oid, index)).as(Ref?)

      walk(v, w.vertex, &fn)
    end
  end
end

module ExtrinsicMap(K, V)
  class Transaction(V)
    property! value : V?

    def initialize(@value)
    end

    def set(value : V) : Nil
      @value = value
    end

    def del? : V?
      value0, @value = @value, nil
      value0
    end

    def del : V
      del? || raise KeyError.new
    end
  end

  abstract def get?(key : K) : V?
  abstract def transaction(key : K, & : Transaction(V) -> T) forall T

  def set(key : K, value : V)
    transaction(key, &.set(value))
  end

  def del?(key : K) : V?
    transaction(key, &.del?)
  end

  def del(key : K) : V
    del?(key) || raise KeyError.new
  end

  def ref(key : K, &zero : -> V) : V
    zerov = nil

    transaction(key) do |tx|
      value0 = tx.value?
      value1 = value0 ? value0 : (zerov ||= yield)
      value1 = value1.incref
      tx.set(value1)
      value1
    end
  end

  def unref(key : K) : V
    transaction(key) do |tx|
      value1, zero = tx.value.decref?
      zero ? tx.del : tx.set(value1)
      value1
    end
  end
end

struct HashMap(K, V)
  include ExtrinsicMap(K, V)

  def initialize
    @map = {} of K => V
  end

  def get?(key : K) : V?
    @map[key]?
  end

  def transaction(key : K, & : Transaction(V) -> T) : T forall T
    value0 = @map[key]?
    tx = Transaction(V).new(value0)
    result = yield tx
    if value1 = tx.value?
      @map[key] = value1
    else
      @map.delete(key)
    end
    result
  end
end

class AtomicMap(K, V)
  include ExtrinsicMap(K, V)

  def initialize
    @map = Atomic(Pf::MapBox(K, V)).new(Pf::MapBox(K, V).new)
  end

  def get?(key : K) : V?
    @map.get(:relaxed)[key]?
  end

  def transaction(key : K, & : Transaction(V) -> T) : T forall T
    map0 = @map.get(:relaxed)

    while true
      value0 = map0[key]?

      tx = Transaction(V).new(value0)

      result = yield tx

      if value1 = tx.value?
        map1 = map0.assoc(key, value1)
      else
        map1 = map0.dissoc(key)
      end

      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)
      if ok
        return result
      end
    end
  end
end

struct Submap(K, V, Ks, Vs)
  include ExtrinsicMap(K, V)

  def initialize(@map : ExtrinsicMap(Ks, Vs))
  end

  def get?(key : K) : V?
    @map.get?(key.as(Ks)).as(V?)
  end

  def transaction(key : K, & : Transaction(V) -> T) : T forall T
    @map.transaction(key.as(Ks)) do |tx0|
      tx1 = Transaction(V).new(tx0.value?.as(V?))
      result = yield tx1
      tx0.value = tx1.value?.as(Vs?)
      result
    end
  end
end

class Tbase
  module Sensor
    # Returns a Tspace-unique id of this sensor, that was obtained from
    # a monotonically increasing source.
    abstract def id : Label
    abstract def strands : Slice(Slice(Ubase::Any))
  end

  module Appearance
    # Returns a Tspace-unique id of this appearance, that was obtained from
    # a monotonically increasing source.
    abstract def id : Label
    abstract def value : Term
  end

  record ConjvRef::Node, vertex : Label do
    include Key
  end

  record ConjvRef::Props, refcount : Refcount do
    include Value

    def incref : Props
      copy_with(refcount: refcount + 1)
    end

    def decref? : {Props, Bool}
      {copy_with(refcount: refcount - 1), refcount == 1}
    end
  end

  record Identity do
    include Value
  end

  record StrandVertex, vertex : Label do
    include Key
  end

  record AppearanceVertex, vertex : Label do
    include Key
  end

  record SensorEncoder::Node, sensor : Label do
    include Key
  end

  record SensorEncoder::Props, conjv : Label do
    include Value
  end

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Key, Value))
  end

  private def udata
    Submap(Utrie::Node, Utrie::Props, Key, Value).new(@data)
  end

  private def xdata
    Submap(Xgraph::Node, Xgraph::Props, Key, Value).new(@data)
  end

  private def tdata
    Submap(Ttrie::Node, Ttrie::Props, Key, Value).new(@data)
  end

  private def edata
    Submap(Etrace::Key, Etrace::Value, Key, Value).new(@data)
  end

  private def strands
    Submap(StrandVertex, Identity, Key, Value).new(@data)
  end

  private def conjvrefs
    Submap(ConjvRef::Node, ConjvRef::Props, Key, Value).new(@data)
  end

  private def sensor_encode
    Submap(SensorEncoder::Node, SensorEncoder::Props, Key, Value).new(@data)
  end

  private def sensor_decode
    Submap(SensorDecoder::Node, SensorDecoder::Props, Key, Value).new(@data)
  end

  private def appearances
    Submap(AppearanceVertex, Identity, Key, Value).new(@data)
  end

  # Adds a sensor *subject* to this Tbase. The instant this method returns,
  # the caller must be capable of reacting to queries resulting in *subject*
  # coming from other threads. This is usually achieved through registering
  # a queue for *subject* before calling this method, which helps to preserve
  # the messages for further reading.
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a monotonically
  #   increasing source;
  # - that *subject*'s id was never in use before.
  #
  # Behavior is undefined if these guarantees are broken. Breaking of these
  # guarantees must be handled at a higher level.
  def mount(subject : Sensor) : Nil
    utrie = Utrie.new(udata)
    xgraph = Xgraph.new(xdata)

    conj = Deque(Label).new

    subject.strands.each do |strand|
      uvertex, added = utrie.mount(strand, @fresh)

      if added
        strands.set(StrandVertex.new(uvertex), Identity.new)
      end

      conj << uvertex
    end

    conj.unstable_sort!
    conjv = xgraph.mount(conj, @fresh)

    # "Publish" the sensor.
    #
    # NOTE: decode assignment MUST be done last because it serves as THE indication
    # of commitment. After the decode transaction finishes, the sensor becomes
    # reachable via querying.
    sensor_encode.set(
      SensorEncoder::Node.new(subject.id),
      SensorEncoder::Props.new(conjv),
    )

    conjvrefs.ref(ConjvRef::Node.new(conjv)) { ConjvRef::Props.new(0) }

    sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
    sensor_decoder.bind(conjv, subject.id)
  end

  # Deletes a sensor *subject* from this Tbase.
  #
  # NOTE: the caller guarantees that it mounted *subject* under the guarantees
  # given in `mount`. Behavior is undefined otherwise.
  #
  # NOTE: the caller must expect the visibility of *subject* to peers until this
  # method returns. Therefore, the caller is expected to somehow "blacklist"
  # *subject* on its end before calling this method, to ensure that *subject* is
  # unreachable through queries while this method is doing its work.
  def unmount(subject : Sensor) : Nil
    # "Unpublish" the sensor. We will need to know its conjunction vertex
    # first though.
    conjv = sensor_encode.del(SensorEncoder::Node.new(subject.id)).conjv

    sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
    sensor_decoder.unbind(conjv, subject.id)

    utrie = Utrie.new(udata)
    xgraph = Xgraph.new(xdata)

    conj = Deque(Label).new

    subject.strands.each do |strand|
      strand_vertex, removed = utrie.unmount(strand)
      if removed
        strands.del(StrandVertex.new(strand_vertex))
      end
      conj << strand_vertex
    end

    conj.unstable_sort!

    expect xgraph.unmount(conj) == conjv

    conjvref = conjvrefs.unref(ConjvRef::Node.new(conjv))

    return unless conjvref.refcount.zero?

    # If we've reached this point, `conjv` will never be used again. Thus we burn
    # the associated sensor encodings to avoid leaking memory.
    sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
    sensor_decoder.burn(conjv)
  end

  # Adds an appearance *subject* to this Tbase. The instant this method returns,
  # the caller must be capable of reacting to queries resulting in *subject*
  # coming from other threads. This is usually achieved through registering
  # a queue for *subject* before calling this method, which helps to preserve
  # the messages for further reading.
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a monotonically
  #   increasing source;
  # - that *subject*'s id was never in use before.
  #
  # Behavior is undefined if these guarantees are broken. Breaking of these
  # guarantees must be handled at a higher level.
  def mount(subject : Appearance) : Nil
    ttrie = Ttrie.new(tdata)
    etrace = Etrace.new(edata)

    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)
      path = ttrie.mount(keypath, subject.id, @fresh)
      keypath.pop

      etrace.mount(path, @fresh)

      true # Continue
    end

    # "Publish" the appearance
    appearances.set(AppearanceVertex.new(subject.id), Identity.new)
  end

  # Deletes an appearance *subject* from this Tbase.
  #
  # NOTE: the caller guarantees that it mounted *subject* under the guarantees
  # given in `mount`. Behavior is undefined otherwise.
  def unmount(subject : Appearance) : Nil
    # "Unpublish" the appearance. Since we're using subject ids we can do
    # that immediately.
    appearances.del(AppearanceVertex.new(subject.id))

    ttrie = Ttrie.new(tdata)
    etrace = Etrace.new(edata)

    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)
      path = ttrie.unmount(keypath, subject.id)
      keypath.pop

      etrace.unmount(path)

      true # Continue
    end
  end

  # Calls *fn* with appearance subject ids that the *subject* sensor matches.
  #
  # If *only_preds* is set to `true`, emits only subject ids that are older
  # than *subject* (that existed before *subject* was created).
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a monotonically
  #   increasing source;
  # - that it considers appearance subject ids given to *fn* immediately outdated.
  #   The caller must ensure that all actions taken upon them first check (or only
  #   proceed provided) the existence of the corresponding appearance.
  def query(subject : Sensor, *, only_preds = true, &fn : Label ->) : Nil
    ttrie = Ttrie.new(tdata)
    etrace = Etrace.new(edata)

    sets = [] of Set(Label)

    subject.strands.each do |strand|
      next unless edge = ttrie.query?(strand)

      hits = Set(Label).new
      sets << hits

      # NOTE: This walk is done asynchronously -- etrace is not driven by
      # a unified clock.
      etrace.walk(*edge) do |candidate|
        # Ensure candidate is a fuly added appearance.
        next unless appearances.get?(AppearanceVertex.new(candidate))
        next if only_preds && subject.id <= candidate

        hits << candidate
      end
    end

    return if sets.empty?

    sets.unstable_sort_by!(&.size)
    sets[0].each do |subject_id|
      next unless (1...sets.size).all? { |index| subject_id.in?(sets[index]) }

      fn.call(subject_id)
    end
  end

  # Calls *fn* with sensor subject ids that the *subject* sensor matches.
  #
  # If *only_preds* is set to `true`, emits only subject ids that are older
  # than *subject* (that existed before *subject* was created).
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a monotonically
  #   increasing source;
  # - that it considers sensor subject ids given to *fn* immediately outdated.
  #   The caller must ensure that all actions taken upon them first check (or only
  #   proceed provided) the existence of the corresponding sensor.
  def query(subject : Appearance, *, only_preds = true, &fn : Label ->) : Nil
    utrie = Utrie.new(udata)
    xgraph = Xgraph.new(xdata)

    hits = Deque(Label).new

    utrie.query(subject.value) do |hit|
      # Ensure the vertex hit is a fully added strand.
      next unless strands.get?(StrandVertex.new(hit))

      hits << hit
    end

    hits.unstable_sort!

    xgraph.conjs(hits) do |candidate|
      # Ensure the vertex hit is a fully added sensor whose subject
      # id we know.
      sensor_decoder = SensorDecoder.new(@fresh, sensor_decode)
      sensor_decoder.decode(candidate) do |candidate_subject_id|
        next if only_preds && subject.id <= candidate_subject_id

        fn.call(candidate_subject_id)
      end
    end
  end
end

class Tspace
  def initialize(@fresh : LabelGenerator, @tbase : Tbase)
    @senders = AtomicMap(Label, {Label, Label, Term?, Term}).new
    @receivers = AtomicMap(Label, {Label, Term?, (Activation ->)}).new
  end

  def bind(outbox, subject : Sensor, identity identity0 : Label, selector selector0 : Term?, callback : Activation ->)
    @receivers.set(subject.id, {identity0, selector0, callback})

    @tbase.mount(subject)
    @tbase.query(subject) do |pred|
      next unless sender = @senders.get?(pred)

      trigger, identity1, selector1, value = sender

      next unless selector0 == selector1

      outbox << {subject.id, callback.partial(StimulusPresence.new(identity0, trigger, identity1, VERTEX_NONE, pred, value))}
    end

    outbox
  end

  def unbind(subject : Sensor)
    @receivers.del(subject.id)
    @tbase.unmount(subject)
  end

  def bind(outbox, prev_subject_id, subject : Appearance, trigger : Label, identity : Label, selector selector0 : Term?)
    @senders.set(subject.id, {trigger, identity, selector0, subject.value})

    @tbase.mount(subject)
    @tbase.query(subject) do |pred|
      next unless receiver = @receivers.get?(pred)

      sensor, selector1, callback = receiver

      next unless selector0 == selector1

      outbox << {pred, callback.partial(StimulusPresence.new(sensor, trigger, identity, prev_subject_id, subject.id, subject.value))}
    end

    outbox
  end

  def unbind(subject : Appearance)
    @senders.del(subject.id)
    @tbase.unmount(subject)
  end

  def depart(outbox, subject : Appearance, message : Term?, selector selector0 : Term?, trigger : Label, identity : Label)
    @tbase.query(subject, only_preds: false) do |pred|
      next unless receiver = @receivers.get?(pred)

      sensor, selector1, callback = receiver

      next unless selector0 == selector1

      outbox << {pred, callback.partial(StimulusAbsence.new(sensor, trigger, identity, subject.id, message))}
    end

    outbox
  end
end

alias Activation = StimulusPresence | StimulusAbsence

record StimulusPresence, sensor : Label, trigger : Label, identity : Label, pred : Label, instant : Label, value : Term

# NOTE: in `StimulusAbsence`, the *farewell* term does not necessarily
# match the receiver sensor's pattern. They are given for reference. If the receiver
# can handle it, they should. Otherwise they may handle the absence itself.
record StimulusAbsence, sensor : Label, trigger : Label, identity : Label, instant : Label, farewell : Term?

alias Strand = Slice(Ubase::Any)
alias StrandList = Slice(Strand)
alias BranchList = Slice(StrandList)

class Tconn
  alias SurfaceData = SensorData | AppearanceData

  record SensorData, identity : Label, instant : Label, pattern : StrandList, selector : Term?
  record AppearanceData, identity : Label, instant : Label, value : Term, selector : Term?, tombstone : Term?

  record SensorMemberData, instant : Label, pattern : StrandList
  record SensorGroupData, identity : Label, members : Slice(SensorMemberData), selector : Term?

  def initialize(@fresh : LabelGenerator, @tspace : Tspace, @callback : Activation ->)
    @conid = @fresh.call

    @sensors = {} of Label => SensorGroupData
    @appearances = {} of Label => AppearanceData
  end

  def initialize(fresh, tspace, &callback : Activation ->)
    initialize(fresh, tspace, callback)
  end

  private def summon(outbox, surface : SensorData)
    @tspace.bind(outbox,
      subject: Sensor.new(surface.instant, surface.pattern),
      selector: surface.selector,
      identity: surface.identity,
      callback: @callback,
    )
  end

  private def summon(outbox, data : SensorGroupData)
    data.members.each do |member|
      summon(outbox, SensorData.new(data.identity, member.instant, member.pattern, data.selector))
    end
  end

  private def dismiss(data : SensorData)
    @tspace.unbind(Sensor.new(data.instant, data.pattern))
  end

  private def dismiss(data : SensorGroupData)
    data.members.each do |member|
      dismiss(SensorData.new(data.identity, member.instant, member.pattern, data.selector))
    end
  end

  private def summon(outbox, prev_subject_id, data : AppearanceData)
    @tspace.bind(outbox,
      prev_subject_id: prev_subject_id,
      subject: Appearance.new(data.instant, data.value),
      selector: data.selector,
      trigger: @conid,
      identity: data.identity,
    )
  end

  private def dismiss(outbox, data : AppearanceData)
    subject = Appearance.new(data.instant, data.value)

    @tspace.unbind(subject)
    @tspace.depart(outbox,
      subject: subject,
      message: data.tombstone,
      selector: data.selector,
      trigger: @conid,
      identity: data.identity,
    )
  end

  private def changes?(data : AppearanceData, value : Term, selector : Term?, tombstone : Term?)
    {data.value, data.selector, data.tombstone} != {value, selector, tombstone}
  end

  private def changes?(data : SensorGroupData, pattern : BranchList, selector : Term?)
    return true unless data.members.size == pattern.size
    return true unless data.selector == selector

    # - Members in data and branches in pattern are unordered.
    # - Most often when this code is reached, both will have size=1.
    pattern.all? do |branch|
      data.members.any? { |member| member.pattern == branch }
    end
  end

  # :nodoc:
  #
  # Replaces the surface at *identity* with a sensor group matching the given
  # *pattern* branch list.
  def add_sensor(identity : Label, *, pattern : BranchList, selector : Term?) : Nil
    outbox = [] of {Label, ->}

    if surface0 = @appearances[identity]?
      dismiss(outbox, surface0)
    elsif surface0 = @sensors[identity]?
      return unless changes?(surface0, pattern, selector)

      dismiss(surface0)
    end

    members = pattern.to_readonly_slice do |branch|
      SensorMemberData.new(@fresh.call, branch)
    end

    surface1 = SensorGroupData.new(identity, members, selector)

    @sensors[identity] = surface1

    summon(outbox, surface1)

    # Implicit assumptions:
    #   - We never notify the same vertex more than one time in summon() nor dismiss().
    #   - Crystal hash tables are ordered.
    outbox.to_h.each { |_, act| act.call }
  end

  def add_sensor(identity : Label, *, pattern : Term, selector : Term?) : Nil
    skeleton = pipe(pattern, M1.normal, Skeleton.pattern)

    strands = [] of Strand
    branches = [] of StrandList

    branches(skeleton) do |branch|
      strands(branch) do |strand|
        strands << strand.items.to_readonly_slice { |base| Ubase.parse(base) }
      end
      branches << strands.to_readonly_slice(&.itself)
      strands.clear
    end

    # Arrays may over-allocate so we make an additional copy with to_readonly_slice
    # to possibly free the over-allocation.
    add_sensor(identity, pattern: branches.to_readonly_slice(&.itself), selector: selector)
  end

  def add_appearance(identity : Label, *, value : Term, selector : Term?, tombstone : Term?) : Nil
    outbox = [] of {Label, ->}

    if surface0 = @sensors[identity]?
      dismiss(surface0)
    elsif surface0 = @appearances[identity]?
      return unless changes?(surface0, value, selector, tombstone)

      prev_subject_id = surface0.instant

      dismiss(outbox, surface0)
    end

    surface1 = AppearanceData.new(identity, @fresh.call, value, selector, tombstone)

    @appearances[identity] = surface1

    summon(outbox, prev_subject_id || VERTEX_NONE, surface1)

    # Implicit assumptions:
    #   - We never notify the same vertex more than one time in summon() nor dismiss().
    #   - Crystal hash tables are ordered.
    outbox.to_h.each { |_, act| act.call }
  end

  def delete(identity : Label) : Nil
    if data = @sensors.delete(identity)
      dismiss(data)
      return
    end

    unless data = @appearances.delete(identity)
      raise ArgumentError.new
    end

    outbox = [] of {Label, ->}

    dismiss(outbox, data)

    # Implicit assumptions:
    #   - We never notify the same vertex more than one time in summon() nor dismiss().
    #   - Crystal hash tables are ordered.
    outbox.to_h.each { |_, act| act.call }
  end
end

class LabelGenerator
  def initialize
    @counter = Atomic(Label).new(VERTEX_ZERO)
  end

  def call : Label
    @counter.add(1, :relaxed)
  end
end

# One-to-many map for decoding a conjunction vertex into the sensors that
# were bound to it.
struct SensorDecoder
  record Node, conjv : Label, id : Label do
    include Tbase::Key
  end

  record Props, sensor : Label, succ : Label, active : Bool do
    include Tbase::Value
  end

  def initialize(@fresh : LabelGenerator, @data : ExtrinsicMap(Node, Props))
  end

  # Registers *sensor* as one of decodings of *conjv*.
  #
  # Worst-case O(N) relative to the highest-ever number of decodings of *conjv*.
  #
  # NOTE: binds and unbinds leave "tombstones" for *conjv*-*sensor* combos.
  # Make sure to `burn` *conjv* when you're sure it's never going to be used
  # again to not leak memory.
  #
  # NOTE: the caller guarantees *conjv* was never bound to *sensor* before.
  def bind(conjv : Label, sensor : Label) : Nil
    id = VERTEX_NONE

    running = true

    while running
      @data.transaction(Node.new(conjv, id)) do |tx|
        props0 = tx.value?

        if props0.nil?
          # If head is free we insert immediately and finish. Note how
          # we have to allocate the successor here.
          running = false

          tx.set Props.new(sensor, @fresh.call, active: true)
        elsif !props0.active
          # If head is inactive we replace its now-defunct sensor with
          # the bound sensor, activate, and finish.
          running = false

          tx.set props0.copy_with(sensor: sensor, active: true)
        else
          # Otherwise we proceed to the successor node.
          id = props0.succ
        end
      end
    end
  end

  # Unregisters *sensor* from being one of the decodings of *conjv*.
  #
  # Worst-case O(N) relative to the highest-ever number of decodings of *conjv*.
  #
  # NOTE: binds and unbinds leave "tombstones" for *conjv*-*sensor* combos.
  # Make sure to `burn` *conjv* when you're sure it's never going to be used
  # again to not leak memory.
  #
  # NOTE: the caller guarantees *conjv* was bound to *sensor* before (`bind`).
  def unbind(conjv : Label, sensor : Label) : Nil
    id = VERTEX_NONE

    running = true

    while running
      @data.transaction(Node.new(conjv, id)) do |tx|
        props0 = tx.value

        if props0.sensor == sensor
          # Deactivate the sensor if we've found a match.
          running = false
          tx.set(props0.copy_with(active: false))
          next
        end

        # Otherwise we proceed to the successor node.
        id = props0.succ
      end
    end
  end

  # NOTE: this method "burns" *conjv*; the caller guarantees that *conjv*
  # will never be passed to `bind` or `unbind` again.
  def burn(conjv : Label) : Nil
    id = VERTEX_NONE

    keys = [] of Node

    while props0 = @data.get?(Node.new(conjv, id))
      keys << Node.new(conjv, id)
      id = props0.succ
    end

    keys.each { |key| @data.del(key) }
  end

  # Yields all decodings associated with *conjv*.
  def decode(conjv : Label, & : Label ->) : Nil
    id = VERTEX_NONE

    while props0 = @data.get?(Node.new(conjv, id))
      if props0.active
        yield props0.sensor
      end
      id = props0.succ
    end
  end
end

# fresh = LabelGenerator.new
# data = AtomicMap(SensorDecoder::Node, SensorDecoder::Props).new

# dec = SensorDecoder.new(fresh, data)

# dec.bind(Label.new(10), Label.new(100))
# dec.bind(Label.new(10), Label.new(200))
# dec.bind(Label.new(10), Label.new(300))

# dec.decode(Label.new(10)) do |sensor|
#   pp sensor
# end

# dec.unbind(Label.new(10), Label.new(200))
# pp dec
# dec.bind(Label.new(10), Label.new(400))
# pp dec

# # pp dec

# # dec.unbind(Label.new(10), Label.new(200))
# # pp dec
# dec.unbind(Label.new(10), Label.new(300))
# pp dec
# dec.bind(Label.new(20), Label.new(123))
# pp dec
# dec.bind(Label.new(20), Label.new(456))
# pp dec
# dec.decode(Label.new(10)) do |sensor|
#   pp sensor
# end
# dec.unbind(Label.new(10), Label.new(100))
# dec.decode(Label.new(20)) do |sensor|
#   pp sensor
# end
# pp dec

# dec.unbind(Label.new(20), Label.new(123))
# dec.unbind(Label.new(20), Label.new(456))
# dec.unbind(Label.new(10), Label.new(400))

# pp dec

# dec.bind(Label.new(10), Label.new(123))

# pp dec

# dec.burn(Label.new(10))
# dec.burn(Label.new(20))

# pp dec

# pp dec

# pattern = ML.term %{((%any div mod) a_ (%all b_number (%not 0)) ¦ precision⋮ 3)}
# normp = M1.normal(pattern)
# skeleton = Skeleton.pattern(normp)

record Sensor, id : Label, strands : Slice(Slice(Ubase::Any)) do
  include Tbase::Sensor

  # TODO: remove, here we have improper (and cannot have proper) handling of branches!!!!
  # Parsing should be done at a higher (sensor group) level!!!
  def self.parse(id : Label, pattern : Term)
    normp = M1.normal(pattern)
    skeleton = Skeleton.pattern(normp)

    strands = [] of Slice(Ubase::Any)

    strands(skeleton) do |strand|
      strands << strand.items.to_readonly_slice { |base| Ubase.parse(base) }
    end

    new(id, strands.to_readonly_slice(&.itself))
  end

  def self.parse(id : Label, ml : String)
    parse(id, ML.term(ml))
  end
end

record Appearance, id : Label, value : Term do
  include Tbase::Appearance
end

# TODO: remember to test: %any of dicts, %literal dict!!!!

fresh = LabelGenerator.new
data = HashMap(Tbase::Key, Tbase::Value).new
tbase = Tbase.new(fresh, data)
tspace = Tspace.new(fresh, tbase)

view = Term[]

# TODO: in reality, the callback is potentially called from another thread,
# a sleepy queue is needed instead of doing things right away.
#
# TODO: in reality we will send activations, not views. It is upto the client
# to merge them into view / handle farewells / cleanup absent appearances.
#
# TODO: in reality, the sensor at client will have to match the full pattern.
# act only provides pattern skeleton matches, whereas client sensors may be
# much more tighter & include match envs.
#
# FIXME: in reality, sensors may contain %any°, which matches multiple appearances
# simultaneously. We have to handle this somehow. Possibly by making view values
# be lists of values rather than single values. This can be handled by labeling
# StimulusPresence with pred=<instant>, and removing it in view.morph. Similarly
# StimulusAbsence will have to get labeled like that and will have to be triggered
# on sensor group members.
conn = Tconn.new(fresh, tspace) do |act|
  case act
  in StimulusPresence
    if act.pred != VERTEX_NONE
      view = view.morph({ {act.trigger, act.identity}, act.pred, nil})
    end

    view = view.morph({ {act.trigger, act.identity}, act.instant, act.value})
  in StimulusAbsence
    view = view.morph({ {act.trigger, act.identity}, nil})
  end
  pp view
end

# conn.sensor 0, pattern: ML.term(%{_number}), selector: nil
# conn.sensor 1, pattern: ML.term(%{_number}), selector: nil

# pp tbase

# conn.delete 0
# conn.delete 1

# pp tbase
# {% skip_file %}

conn.add_appearance 1, value: ML.term(%{1}), selector: nil, tombstone: Term.of("bye bye")
conn.add_sensor 0, pattern: ML.term(%{_number}), selector: nil
conn.add_sensor 3, pattern: ML.term(%{(%any° _number "bye bye")}), selector: nil
conn.add_sensor 2, pattern: ML.term(%{(%any 2 4 6 8 9)}), selector: Term.of(:qux)

1000.times do |i|
  if i == 300
    conn.add_sensor 4, pattern: ML.term(%{_number}), selector: Term.of(:qux)
  elsif i == 500
    conn.delete 4
  elsif i == 900
    conn.add_sensor 4, pattern: ML.term(%{_number}), selector: Term.of(:qux)
  end
  conn.add_appearance 1, value: Term.of(i), selector: Term.of(:qux), tombstone: nil
end
conn.delete 1
conn.delete 0
conn.delete 2
conn.delete 3
conn.delete 4
pp tbase

{% skip_file %}

# ctx = ExecutionContext::MultiThreaded.new("MT", 4)

# counter = Atomic(Int32).new(0)

# # Note how each client has some parts overlapping with others (type: "pixel")
# # over which we have contention-by-content (irresolvable). On the other hand
# # the Xs and Ys are all independent.
# client = ->(ord : Int32) do
#   (10*ord...10*(ord + 1)).each do |i|
#     (10*ord...10*(ord + 1)).each do |j|
#       app = Appearance.new(fresh.call, Term.of(type: "pixel", x: i, y: j, ord: ord))
#       tbase.mount(app)
#       sleep 100.milliseconds
#     end
#   end

#   counter.add(1)
# end

# 1000.times do |i|
#   ctx.spawn { client.call(i) }
# end

# ctx.spawn do
#   start = Time.monotonic

#   until counter.get == 1000
#     puts counter.get
#     puts "-- in #{(Time.monotonic - start).seconds}s"
#     sleep 500.milliseconds
#   end

#   puts "All 1000 done!"
# end

# puts "Here"

# while input = gets
#   s = Sensor.parse(fresh.call, ML.term(input))
#   dt = Time.measure do
#     tbase.query(s) do |hit|
#       puts "Hit: #{hit}!"
#     end
#   end
#   puts "Took ~#{dt.total_milliseconds}ms"
# end

# {% skip_file %}
s0 = Sensor.parse(5000, %[{x: _, y: _}])
s1 = Sensor.parse(1001, %[{x: 100}])
s2 = Sensor.parse(1000, %[{y: _}])

a0 = Appearance.new(1234, Term.of(x: 100, y: 200))
a1 = Appearance.new(1235, Term.of(x: 100, y: 201))
a2 = Appearance.new(1236, Term.of(x: 101, y: 200))
a3 = Appearance.new(1237, Term.of(x: 101, y: 201))

tbase.mount(a0)
tbase.mount(a1)
tbase.mount(a2)
tbase.mount(a3)

tbase.mount(s0)
tbase.mount(s1)
tbase.mount(s2)

puts "Population: 1234 1235 1236 1237"

puts "Query #{s0}"
tbase.query(s0) { |hit| pp hit }
puts "Query #{s1}"
tbase.query(s1) { |hit| pp hit }
puts "Query #{s2}"
tbase.query(s2) { |hit| pp hit }

puts "Query #{a0}"
tbase.query(a0) { |hit| pp hit }
puts "Query #{a1}"
tbase.query(a1) { |hit| pp hit }
puts "Query #{a2}"
tbase.query(a2) { |hit| pp hit }
puts "Query #{a3}"
tbase.query(a3) { |hit| pp hit }

tbase.unmount(a0)

puts "Population: 1235 1236 1237"
tbase.query(s0) do |hit|
  pp hit
end

tbase.unmount(a3)

puts "Population: 1235 1236"
tbase.query(s0) do |hit|
  pp hit
end

tbase.unmount(a2)

puts "Population: 1235"
tbase.query(s0) do |hit|
  pp hit
end

tbase.unmount(a1)

puts "Population: "
tbase.query(s0) do |hit|
  pp hit
end

puts "Query #{a0} without s1"
tbase.unmount(s1)
tbase.query(a0) do |hit|
  pp hit
end

puts "Query #{a0} without s2"
tbase.unmount(s2)
tbase.query(a0) do |hit|
  pp hit
end
