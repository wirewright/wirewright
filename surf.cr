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

      # In pattern skeleton, all layers are always open. So we cannot handle
      # literal belows.
      matchpi %{(%'%layer (%'%literal _dict) side_dict)} do
        entries(side.unsafe_as_d)
      end

      # If it is a pattern we use %all.
      matchpi %{(%'%layer below_ side_dict)} do
        all2(Term.of(:"%all", pattern(below), entries(side.unsafe_as_d)))
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
            commit << {:"%literal", item}
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

    matchpi %{%'(%number _)} { sink.call(prefix.append(branch)) }
    matchpi %{%'(%string)} { sink.call(prefix.append(branch)) }
    matchpi %{%'(%symbol)} { sink.call(prefix.append(branch)) }
    matchpi %{%'(%boolean)} { sink.call(prefix.append(branch)) }

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

module ExtrinsicMap(K, V)
  abstract def get?(key : K)
  abstract def ref(key : K, & : -> V)
  abstract def unref(key : K) : V
end

module ExtrinsicSet(T)
  abstract def includes?(object : T)
  abstract def add(object : T)
  abstract def delete(object : T)
end

alias Vertex = UInt32

VERTEX_NONE = Vertex.new(0)
VERTEX_ROOT = VERTEX_NONE + 1
VERTEX_ZERO = VERTEX_ROOT + 1

struct Utrie
  record Node, pred : Vertex, base : Ubase::Any
  record Props, refcount : UInt32, successor : Vertex do
    def incref : Props
      copy_with(refcount: refcount + 1)
    end

    def decref? : {Props, Bool}
      {copy_with(refcount: refcount - 1), refcount == 1}
    end
  end

  def initialize(@storage : ExtrinsicMap(Node, Props))
  end

  def mount(pred : Vertex, base : Ubase::Any, fresh) : {Vertex, Bool}
    props = @storage.ref(Node.new(pred, base)) { Props.new(0u32, fresh.call) }

    {props.successor, props.refcount == 1}
  end

  def mount(strand : Enumerable(T), fresh, & : T -> Ubase::Any) : {Vertex, Bool} forall T
    added = false
    vertex = strand.reduce(VERTEX_ROOT) do |pred, base|
      # After the first added = true, all remaining mounts will also be
      # added = true.
      succ, added = mount(pred, (yield base), fresh)
      succ
    end

    {vertex, added}
  end

  def mount(strand : Enumerable(Ubase::Any), fresh) : {Vertex, Bool}
    mount(strand, &.itself)
  end

  # NOTE: the caller guarantees that *base* was mounted with *pred* before.
  def unmount(pred : Vertex, base : Ubase::Any) : {Vertex, Bool}
    props = @storage.unref(Node.new(pred, base))

    {props.successor, props.refcount.zero?}
  end

  def unmount(strand : Enumerable(T), & : T -> Ubase::Any) : {Vertex, Bool} forall T
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

  def unmount(strand : Enumerable(Ubase::Any)) : {Vertex, Bool}
    unmount(strand, &.itself)
  end

  private def successor?(node : Node) : Vertex?
    props = @storage.get?(node)
    props ? props.successor : nil
  end

  {% for type, base in {Term::Num => Ubase::IsNum, Term::Str => Ubase::IsStr, Term::Sym => Ubase::IsSym, Term::Boolean => Ubase::IsBool} %}
    private def query(pred : Vertex, term : {{type}}, sink : Vertex ->) : Nil
      return unless successor0 = successor?(Node.new(pred, {{base}}.new))

      sink.call(successor0)

      if successor1 = successor?(Node.new(successor0, Ubase::Literal.new(Term.of(term))))
        sink.call(successor1)
      end
    end
  {% end %}

  # NOTE: dictionaries must be normalized into IsDict - At(), even literal ones.
  # We do not handle Literal(dict).
  private def query(pred : Vertex, term : Term::Dict, sink : Vertex ->) : Nil
    return unless successor0 = successor?(Node.new(pred, Ubase::IsDict.new))

    sink.call(successor0)

    term.each_entry do |key, value|
      next unless successor1 = successor?(Node.new(successor0, Ubase::At.new(key)))

      sink.call(successor1)

      query(successor1, value.downcast, sink)
    end
  end

  private def query(term : Term, sink : Vertex ->) : Nil
    query(VERTEX_ROOT, term.downcast, sink)
  end

  def query(term : Term, &sink : Vertex ->) : Nil
    query(term, sink)
  end
end

struct Xgraph
  record Node, a : Vertex, b : Vertex
  record Props, refcount : UInt32, successor : Vertex do
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
  def mount(xrule : Deque(Vertex), fresh)
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
  def unmount(xrule : Deque(Vertex)) : Vertex
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
  def conjs(vertices : Deque(Vertex), sink : Vertex ->)
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
  def conjs(vertices : Deque(Vertex), &sink : Vertex ->)
    conjs(vertices, sink)
  end
end

struct Ttrie
  record Node, pred : Vertex, base : Ubase::Any
  record Props, refcount : UInt32, successor : Vertex do
    def incref : Props
      copy_with(refcount: refcount + 1)
    end

    def decref? : {Props, Bool}
      {copy_with(refcount: refcount - 1), refcount == 1}
    end
  end

  def initialize(@data : ExtrinsicMap(Node, Props))
  end

  def mount(pred : Vertex, base : Ubase::Any, fresh) : Vertex
    props = @data.ref(Node.new(pred, base)) { Props.new(0u32, fresh.call) }
    props.successor
  end

  # NOTE: the caller guarantees that *base* was mounted with *pred* before.
  def unmount(pred : Vertex, base : Ubase::Any) : Vertex
    props = @data.unref(Node.new(pred, base))
    props.successor
  end

  # Mounts *strand*.
  #
  # Returns the new *fresh* vertex.
  def mount(strand : Enumerable(Term), endpoint : Vertex, fresh) : Slice(Vertex)
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
  def unmount(strand : Enumerable(Term), endpoint : Vertex) : Slice(Vertex)
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
  def query?(strand : Enumerable(Ubase::Any)) : {Vertex, Vertex}?
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
  alias Key = Node | SuccessorCount | SuccessorList | Successor
  alias Value = Props | UInt32 | Presence

  record Node, pred : Vertex, vertex : Vertex
  record Props, refcount : UInt32, oid : UInt32

  record SuccessorCount, oid : UInt32
  record SuccessorList, oid : UInt32, index : UInt32

  record Successor, oid : UInt32, vertex : Vertex
  record Presence

  def initialize(@data : ExtrinsicMap(Key, Value))
  end

  def mount(path : Slice(Vertex), fresh)
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
      _, node1 = @data.transaction(Node.new(pred, step)) do |node0|
        node0 = node0.as(Props?)
        node0 ? node0.copy_with(refcount: node0.refcount + 1) : Props.new(1u32, fresh.call)
      end
      pred = step
      node1.oid
    end

    # If we succeed in adding a Successor, then we're responsible for
    # incrementing successor count and inserting into the SuccessorList.
    (0...path.size - 1).each do |index|
      oid = oids[index]

      successor = Successor.new(oid, w = path[index + 1])
      next if @data.get?(successor)

      @data.transaction(successor) { Presence.new }

      _, count1 = @data.transaction(SuccessorCount.new(oid)) do |count0|
        count0 = count0.as(UInt32?)
        count0 ? count0 + 1 : 1u32
      end

      @data.transaction(SuccessorList.new(oid, count1 - 1)) { w }
    end
  end

  def unmount(path : Slice(Vertex))
    if path.size < 2
      raise ArgumentError.new
    end

    if path[0] == VERTEX_ROOT
      path = path[1..]
    end

    pred = VERTEX_ROOT

    oids = path.compact_map do |step|
      node0, node1 = @data.transaction(Node.new(pred, step)) do |current|
        current = current.as(Props)
        current.refcount == 1 ? nil : current.copy_with(refcount: current.refcount - 1)
      end

      pred = step

      # Keep only oids which we've removed. We're responsible for their
      # cleanup then.
      node1 ? nil : node0.as(Props).oid
    end

    oids.each do |oid|
      # Successor list may not necessarily exist for endpoint vertices.
      # Deletion may fail, and we're fine with that.
      next unless count = @data.delete?(SuccessorCount.new(oid)).as(UInt32?)

      (0u32...count).each do |index|
        successor = @data.delete(SuccessorList.new(oid, index)).as(UInt32)

        @data.delete(Successor.new(oid, successor))
      end
    end
  end

  def walk(u : Vertex, v : Vertex, &fn : Vertex ->)
    fn.call(v)

    return unless props = @data.get?(Node.new(u, v)).as(Props?)

    # NOTE: while walking, the node along with its attributes could get deleted/
    # be in the process of being deleted. So at any point where we're reading
    # from the map, we must handle the absence-case, even if it seems like it
    # is impossible.
    return unless count = @data.get?(SuccessorCount.new(props.oid)).as(UInt32?)

    (0u32...count).each do |index|
      # We're fine with gaps. They could happen under some successor count
      # increment + successor list insert orderings. We're bounded anyway.
      next unless w = @data.get?(SuccessorList.new(props.oid, index)).as(UInt32?)

      walk(v, w, &fn)
    end
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

  def transaction(key : K, & : V? -> T) : {V?, T} forall T
    map0 = @map.get(:relaxed)

    while true
      value0 = map0[key]?
      value1 = yield value0

      case {value0, value1}
      in {nil, nil}
        raise KeyError.new("value absent and not set during transaction: invalid state")
      in {V, nil}
        map1 = map0.dissoc(key)
      in {nil, V}, {V, V}
        map1 = map0.assoc(key, value1)
      end

      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)
      if ok
        return value0, value1
      end
    end
  end

  def delete(key : K) : V
    delete?(key) || raise KeyError.new
  end

  def delete?(key : K) : V?
    map0 = @map.get(:relaxed)

    while true
      value = map0[key]?
      return unless value

      map1 = map0.dissoc(key)
      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)

      return value if ok
    end
  end

  def assign(key : K, value : V)
    map0 = @map.get(:relaxed)

    while true
      map1 = map0.assoc(key, value)
      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)
      break if ok
    end
  end

  # Increments the refcount of *key* in a single, atomic transaction,
  # creating the pair using the block, if necessary.
  #
  # `V` instances must respond to `incref`.
  def ref(key : K, & : -> V) : V
    map0 = @map.get(:relaxed)
    default = nil

    while true
      value0 = map0[key]? || (default ||= yield)
      value1 = value0.incref
      map1 = map0.assoc(key, value1)
      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)
      return value1 if ok
    end
  end

  # Decrements the refcount of *key* in a single, atomic transaction,
  # creating the pair using the block, if necessary.
  #
  # `V` instances must respond to `decref?`.
  def unref(key : K) : V
    map0 = @map.get(:relaxed)

    while true
      unless value0 = map0[key]?
        raise KeyError.new
      end
      value1, zero = value0.decref?
      if zero
        map1 = map0.dissoc(key)
      else
        map1 = map0.assoc(key, value1)
      end
      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)
      return value1 if ok
    end
  end
end

class AtomicSet(T)
  include ExtrinsicSet(T)

  def initialize
    @set = Atomic(Pf::SetBox(T)).new(Pf::SetBox(T).new)
  end

  def includes?(object : T) : Bool
    @set.get(:relaxed).includes?(object)
  end

  def add(object : T)
    set0 = @set.get(:relaxed)

    while true
      set1 = set0.add(object)
      set0, ok = @set.compare_and_set(set0, set1, :relaxed, :relaxed)
      break if ok
    end
  end

  def delete(object : T)
    set0 = @set.get(:relaxed)

    while true
      set1 = set0.delete(object)
      set0, ok = @set.compare_and_set(set0, set1, :relaxed, :relaxed)
      break if ok
    end
  end
end

class Tbase
  class VertexGenerator
    def initialize
      @counter = Atomic(Vertex).new(VERTEX_ZERO)
    end

    def call : Vertex
      @counter.add(1, :relaxed)
    end
  end

  module Sensor
    # Returns a Tspace-unique id of this sensor, that was obtained from
    # a monotonically increasing source.
    abstract def id : Vertex
    abstract def branch : Term
  end

  module Appearance
    # Returns a Tspace-unique id of this appearance, that was obtained from
    # a monotonically increasing source.
    abstract def id : Vertex
    abstract def value : Term
  end

  def initialize
    @fresh = VertexGenerator.new

    @udata = AtomicMap(Utrie::Node, Utrie::Props).new
    @xdata = AtomicMap(Xgraph::Node, Xgraph::Props).new
    @tdata = AtomicMap(Ttrie::Node, Ttrie::Props).new
    @edata = AtomicMap(Etrace::Key, Etrace::Value).new

    @strands = AtomicSet(Vertex).new

    @sensor_encode = AtomicMap(Vertex, Vertex).new
    @sensor_decode = AtomicMap(Vertex, Vertex).new
    @appearances = AtomicSet(Vertex).new
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
    utrie = Utrie.new(@udata)
    xgraph = Xgraph.new(@xdata)

    conj = Deque(Vertex).new

    strands(subject.branch) do |strand|
      strand_vertex, added = utrie.mount(strand.items, @fresh) { |base| Ubase.parse(base) }
      if added
        @strands.add(strand_vertex)
      end
      conj << strand_vertex
    end

    conj.unstable_sort!
    conjv = xgraph.mount(conj, @fresh)

    # "Publish" the sensor.
    #
    # NOTE: decode assignment MUST be done last because it's the indication
    # of commitment. After the decode assignment is in place, the sensor becomes
    # reachable via querying.
    @sensor_encode.assign(subject.id, conjv)
    @sensor_decode.assign(conjv, subject.id)
  end

  # Deletes a sensor *subject* from this Tbase.
  #
  # NOTE: the caller guarantees that it mounted *subject* under the guarantees
  # given in `mount`. Behavior is undefined otherwise.
  def unmount(subject : Sensor) : Nil
    conjv = @sensor_encode.delete(subject.id)

    # "Unpublish" the sensor. We will need to know its conjunction vertex
    # first though.
    @sensor_decode.delete(conjv)

    utrie = Utrie.new(@udata)
    xgraph = Xgraph.new(@xdata)

    conj = Deque(Vertex).new

    strands(subject.branch) do |strand|
      strand_vertex, removed = utrie.unmount(strand.items) { |base| Ubase.parse(base) }
      if removed
        @strands.delete(strand_vertex)
      end
      conj << strand_vertex
    end

    conj.unstable_sort!

    expect xgraph.unmount(conj) == conjv
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
    ttrie = Ttrie.new(@tdata)
    etrace = Etrace.new(@edata)

    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)
      path = ttrie.mount(keypath, subject.id, @fresh)
      keypath.pop

      etrace.mount(path, @fresh)

      true # Continue
    end

    # "Publish" the appearance
    @appearances.add(subject.id)
  end

  # Deletes an appearance *subject* from this Tbase.
  #
  # NOTE: the caller guarantees that it mounted *subject* under the guarantees
  # given in `mount`. Behavior is undefined otherwise.
  def unmount(subject : Appearance) : Nil
    # "Unpublish" the appearance. Since we're using subject ids we can do
    # that immediately.
    @appearances.delete(subject.id)

    ttrie = Ttrie.new(@tdata)
    etrace = Etrace.new(@edata)

    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)
      path = ttrie.unmount(keypath, subject.id)
      keypath.pop

      etrace.unmount(path)

      true # Continue
    end
  end

  # Calls *fn* with appearance subject ids that the *subject* sensor matches,
  # that are older than *subject* (that existed before *subject* was created).
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a monotonically
  #   increasing source;
  # - that it considers appearance subject ids given to *fn* immediately outdated.
  #   The caller must ensure that all actions taken upon them first check (or only
  #   proceed provided) the existence of the corresponding appearance.
  def predecessors(subject : Sensor, &fn : Vertex ->) : Nil
    ttrie = Ttrie.new(@tdata)
    etrace = Etrace.new(@edata)

    sets = [] of Set(Vertex)

    strands(subject.branch) do |strand|
      next unless edge = ttrie.query?(strand.items.map { |base| Ubase.parse(base) })

      hits = Set(Vertex).new
      sets << hits

      # NOTE: This walk is done asynchronously -- etrace is not driven by
      # a unified clock.
      etrace.walk(*edge) do |candidate|
        # Ensure candidate is a fuly added appearance.
        next unless candidate.in?(@appearances)

        # Candidate must have been created before subject for subject to
        # see it.
        next unless candidate <= subject.id

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

  # Calls *fn* with appearance subject ids that the *subject* sensor matches,
  # that are older than *subject* (that existed before *subject* was created).
  #
  # NOTE: the caller guarantees the following:
  #
  # - that it owns *subject*;
  # - that *subject*'s id is Tspace-unique, and was obtained from a monotonically
  #   increasing source;
  # - that it considers sensor subject ids given to *fn* immediately outdated.
  #   The caller must ensure that all actions taken upon them first check (or only
  #   proceed provided) the existence of the corresponding sensor.
  def predecessors(subject : Appearance, &fn : Vertex ->) : Nil
    utrie = Utrie.new(@udata)
    xgraph = Xgraph.new(@xdata)

    hits = Deque(Vertex).new

    utrie.query(subject.value) do |hit|
      # Ensure the vertex hit is a fully added strand.
      next unless hit.in?(@strands)

      hits << hit
    end

    hits.unstable_sort!

    xgraph.conjs(hits) do |candidate|
      # Ensure the vertex hit is a fully added sensor whose subject
      # id we know.
      next unless candidate_subject_id = @sensor_decode.get?(candidate)

      # Candidate must have been created before subject for subject to
      # see it.
      next unless candidate_subject_id <= subject.id

      fn.call(candidate_subject_id)
    end
  end
end

# pattern = ML.term %{((%any div mod) a_ (%all b_number (%not 0)) ¦ precision⋮ 3)}
# normp = M1.normal(pattern)
# skeleton = Skeleton.pattern(normp)

record Sensor, id : Vertex, branch : Term do
  include Tbase::Sensor

  def self.parse(id : Vertex, pattern : Term)
    normp = M1.normal(pattern, dict_literals_allowed: false)
    skeleton = Skeleton.pattern(normp)

    new(id, skeleton)
  end

  def self.parse(id : Vertex, ml : String)
    parse(id, ML.term(ml))
  end
end

record Appearance, id : Vertex, value : Term do
  include Tbase::Appearance
end

tbase = Tbase.new

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
tbase.predecessors(s0) { |hit| pp hit }
puts "Query #{s1}"
tbase.predecessors(s1) { |hit| pp hit }
puts "Query #{s2}"
tbase.predecessors(s2) { |hit| pp hit }

puts "Query #{a0}"
tbase.predecessors(a0) { |hit| pp hit }
puts "Query #{a1}"
tbase.predecessors(a1) { |hit| pp hit }
puts "Query #{a2}"
tbase.predecessors(a2) { |hit| pp hit }
puts "Query #{a3}"
tbase.predecessors(a3) { |hit| pp hit }

tbase.unmount(a0)

puts "Population: 1235 1236 1237"
tbase.predecessors(s0) do |hit|
  pp hit
end

tbase.unmount(a3)

puts "Population: 1235 1236"
tbase.predecessors(s0) do |hit|
  pp hit
end

tbase.unmount(a2)

puts "Population: 1235"
tbase.predecessors(s0) do |hit|
  pp hit
end

tbase.unmount(a1)

puts "Population: "
tbase.predecessors(s0) do |hit|
  pp hit
end

puts "Query #{a0} without s1"
tbase.unmount(s1)
tbase.predecessors(a0) do |hit|
  pp hit
end

puts "Query #{a0} without s2"
tbase.unmount(s2)
tbase.predecessors(a0) do |hit|
  pp hit
end
