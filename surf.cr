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
    matchpi %{(%'%all %'(%pass) b_)} { b }
    matchpi %{(%'%all a_ %'(%pass))} { a }
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
# - `(%'%literal _)`
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
    matchpi %{(%'%pass)} do
      sink.call(prefix)
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

    otherwise do
      sink.call(prefix.append(branch))
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
  def conjunctions(vertices : Deque(Vertex), sink : Vertex ->)
    while a = vertices.shift?
      sink.call(a)

      (0...vertices.size).each do |i|
        b = vertices.unsafe_fetch(i)
        next unless successor = @successors.get?(Node.new(a, b))

        vertices << successor
      end
    end
  end

  # Calls *sink* with all mounted conjunction vertices in *vertices*.
  #
  # NOTE: *vertices* must be pre-sorted ascending. You lose ownership of *vertices*
  # by passing it to this method.
  def conjunctions(vertices : Deque(Vertex), &sink : Vertex ->)
    conjunctions(vertices, sink)
  end
end

struct StrandSet
  def initialize(@data : ExtrinsicSet(Vertex))
  end

  delegate :includes?, :add, :delete, to: @data
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
  def mount(strand : Enumerable(Term), fresh) : Slice(Vertex)
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

    path.to_readonly_slice
  end

  # Unmounts *strand*.
  #
  # Returns the path to its endpoint.
  def unmount(strand : Enumerable(Term)) : Slice(Vertex)
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

    path.to_readonly_slice
  end

  # Calls *fn* with the set of endpoints at the end of *strand*.
  def query(strand : Enumerable(Ubase::Any), &fn : SetSum ->) : Nil
    pred = VERTEX_ROOT

    strand.each do |base|
      unless pred = @successors.get?(Node.new(pred, base))
        return
      end
    end

    return unless endpoints = @endpoints.get?(pred)

    fn.call(endpoints)
  end

  # Calls *fn* with the sets of endpoints at the end of each *strand*.
  def query(strands : Enumerable(Enumerable(Ubase::Any)), &fn : SetSum ->) : Nil
    strands.each { |strand| query(strand, &fn) }
  end
end

# :nodoc:
abstract class SetSum
end

# :nodoc:
defcase SetLeaf < SetSum, id : UInt32, population : Int32, hashcode : UInt64, vertices : Pf::Set(Vertex), equality: false do
  def self.new(id : UInt32)
    new(id, 0u32, 0u64, Pf::Set(Vertex).new)
  end

  def self.[](id : UInt32, *vertices : Vertex)
    vertices.reduce(new(id)) { |leaf, vertex| leaf.add(vertex) }
  end

  def includes?(vertex : Vertex)
    @vertices.includes?(vertex)
  end

  def add(vertex : Vertex)
    vertices0 = @vertices
    vertices1 = @vertices.add(vertex)
    if vertices0.same?(vertices1)
      return self
    end

    copy_with(population: population + 1, hashcode: {id, vertices1}.hash, vertices: vertices1)
  end

  def delete(vertex : Vertex)
    vertices0 = @vertices
    vertices1 = @vertices.delete(vertex)
    if vertices0.same?(vertices1)
      return self
    end

    copy_with(population: population - 1, hashcode: {id, vertices1}.hash, vertices: vertices1)
  end

  def each(&fn : Vertex ->)
    vertices.each(&fn)
  end

  def inspect(io)
    io << "{" << population << "@" << id << "| "
    vertices.join(io, " ")
    io << "}"
  end

  def hash(hasher)
    @hashcode.hash(hasher)
  end

  def_equals @id, @vertices
end

# :nodoc:
defcase SetNode < SetSum, population : Int32, hashcode : UInt64, members : Pf::Set(SetSum), equality: false do
  def self.new
    new(0u32, 0u64, Pf::Set(SetSum).new)
  end

  def self.[](*members : SetSum)
    members.reduce(new) { |node, vertex| node.add(vertex) }
  end

  def includes?(vertex : Vertex)
    @members.any?(&.includes?(vertex))
  end

  def add(member : SetSum)
    members0 = @members
    members1 = members0.add(member)
    if members0.same?(members1)
      return self
    end

    copy_with(members: members1, hashcode: members1.hash, population: population + member.population)
  end

  def delete(member : SetSum)
    members0 = @members
    members1 = members0.delete(member)
    if members0.same?(members1)
      return self
    end

    copy_with(members: members1, hashcode: members1.hash, population: population - member.population)
  end

  def each(&fn : Vertex ->)
    members.each(&.each(&fn))
  end

  def inspect(io)
    io << "{" << population << "| "
    members.join(io, " ") do |member|
      member.inspect(io)
    end
    io << "}"
  end

  def hash(hasher)
    @hashcode.hash(hasher)
  end

  def_equals @members
end

# struct Etrace
#   alias Node = Vertex
#   alias Props = SetSum

#   def initialize(@data : ExtrinsicMap(Vertex, SetSum))
#   end

#   # Adds *endpoint* as one of the endpoints of each step in *path*.
#   def mount(path : Slice(Vertex), endpoint : Vertex, fresh) : Vertex
#     if path.empty?
#       raise ArgumentError.new
#     end

#     # Add endpoint.
#     node1 = @endpoints.modify(path.last) do |sum|
#       sum ? sum.as(SetLeaf).add(endpoint) : SetLeaf[fresh.call, endpoint]
#     end

#     # Propagate endpoint backwards through path.
#     path[...-1].reverse_each do |pred|
#       if parent0 = @endpoints.get?(pred)
#         parent0 = parent0.as(SetNode)
#         node1 = (node0 ? parent0.delete(node0) : parent0).add(node1)
#         node0 = parent0
#       else
#         node1 = SetNode[node1.as(SetSum)]
#         node0 = nil
#       end

#       @endpoints.assign(pred, node1)
#     end

#     fresh
#   end

#   # Removes *endpoint* from the set of endpoints of each step in *path*.
#   def unmount(path : Slice(Vertex), endpoint : Vertex) : Nil
#     if path.empty?
#       raise ArgumentError.new
#     end

#     # Remove endpoint
#     node0 = @endpoints.get?(path.last).as(SetLeaf)
#     node1 = node0.delete(endpoint)

#     if node1.population.zero?
#       @endpoints.unload(path.last)
#     else
#       @endpoints.assign(path.last, node1)
#     end

#     # Remove path (except for last step which we've already handled).
#     path[...-1].reverse_each do |pred|
#       parent0 = @endpoints.get?(pred).as(SetNode)

#       if node1.population.zero?
#         node1 = parent0.delete(node0)
#       else
#         node1 = parent0.delete(node0).add(node1)
#       end

#       node0 = parent0

#       if node1.population.zero?
#         @endpoints.unload(pred)
#       else
#         @endpoints.assign(pred, node1)
#       end
#     end
#   end
# end

def mount(ttrie : Ttrie, term : Term, endpoint : Vertex, fresh : Vertex) : Vertex
  Term.each_keypath_and_leaf(term) do |keypath, leaf|
    keypath.push(leaf)
    path, fresh = ttrie.mount(keypath, fresh)
    keypath.pop

    fresh = ttrie.mount_endpoint(path, endpoint, fresh)

    true # Continue
  end

  fresh
end

def unmount(ttrie : Ttrie, term : Term, endpoint : Vertex) : Nil
  Term.each_keypath_and_leaf(term) do |keypath, leaf|
    keypath.push(leaf)
    path = ttrie.unmount(keypath)
    keypath.pop

    ttrie.unmount_endpoint(path, endpoint)

    true # Continue
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

  # Increments the refcount of *key* in a single, atomic transaction,
  # creating the pair using the block, if necessary.
  #
  # `V` instances must respond to `incref`.
  def ref(key : K, & : -> V) : V
    map0 = @map.get(:relaxed)
    default = nil

    while true
      value = map0[key]? || (default ||= yield)
      map1 = map0.assoc(key, value.incref)
      map0, ok = @map.compare_and_set(map0, map1, :relaxed, :relaxed)
      return value if ok
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
      @counter = Atomic(Vertex).new(Vertex.new(0))
    end

    def call : Vertex
      @counter.add(1, :relaxed)
    end
  end

  module Sensor
    abstract def skeleton : Term
  end

  module Appearance
    # Returns a Tspace-unique id of this appearance.
    abstract def id : Vertex
    abstract def value : Term
  end

  record SensorGroup, members : Slice(Vertex)

  def initialize
    @fresh = VertexGenerator.new

    @udata = AtomicMap(Utrie::Node, Utrie::Props).new
    @xdata = AtomicMap(Xgraph::Node, Xgraph::Props).new
    @tdata = AtomicMap(Ttrie::Node, Ttrie::Props).new
    @strands = AtomicSet(Vertex).new
    # @edata = AtomicMap(Etrace::Node, Etrace::Props).new
  end

  # Adds a sensor *subject* to this Tbase. Returns a handle to the resulting
  # *sensor group*.
  #
  # NOTE: reference counting is used for duplicate *subjects*. The same number of
  # duplicate *subjects* you've mounted, you'll have to unmount.
  def mount(subject : Sensor) : SensorGroup
    utrie = Utrie.new(@udata)
    strands = StrandSet.new(@strands)
    xgraph = Xgraph.new(@xdata)

    members = [] of Vertex

    branches(subject.skeleton) do |branch|
      conjunction = Deque(Vertex).new

      strands(branch) do |strand|
        strand_vertex, added = utrie.mount(strand.items, @fresh) { |base| Ubase.parse(base) }
        if added
          strands.add(strand_vertex)
        end
        conjunction << strand_vertex
      end

      conjunction.unstable_sort!
      conjunction_vertex = xgraph.mount(conjunction, @fresh)

      members << conjunction_vertex

      # TODO: register sensor in registry under conjunction_vertex, current instant
    end

    SensorGroup.new(members.to_readonly_slice)
  end

  # Deletes a sensor *subject* from this Tbase.
  #
  # NOTE: the caller guarantees its ownership of *subject* and that *subject*
  # is currently mounted.
  def unmount(subject : Sensor) : Nil
    utrie = Utrie.new(@udata)
    strands = StrandSet.new(@strands)
    xgraph = Xgraph.new(@xdata)

    branches(subject.skeleton) do |branch|
      conjunction = Deque(Vertex).new

      strands(branch) do |strand|
        strand_vertex, removed = utrie.unmount(strand.items) { |base| Ubase.parse(base) }
        if removed
          strands.delete(strand_vertex)
        end
        conjunction << strand_vertex
      end

      conjunction.unstable_sort!
      conjunction_vertex = xgraph.unmount(conjunction)

      # TODO: unregister sensor in registry under conjunction_vertex
    end
  end

  def mount(subject : Appearance)
    ttrie = Ttrie.new(@tdata)
    # endpoints = Etrace.new(@edata)

    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf)
      path = ttrie.mount(keypath, @fresh)
      keypath.pop

      # Mount etrace

      pp path
      pp subject.id
      # endpoints.mount(path, subject.id, @fresh)

      true # Continue
    end
  end

  # TODO: unmount(subject : Appearance)
end

pattern = ML.term %{((%any div mod) a_ (%all b_number (%not 0)) ¦ precision⋮ 3)}
normp = M1.normal(pattern)
skeleton = Skeleton.pattern(normp)

record Sensor, skeleton : Term do
  include Tbase::Sensor
end

record Appearance, id : Vertex, value : Term do
  include Tbase::Appearance
end

tbase = Tbase.new
s1 = tbase.mount(Sensor.new(skeleton))
pp s1
s2 = tbase.mount(Sensor.new(skeleton))
pp s2

pp tbase

tbase.unmount(Sensor.new(skeleton))

pp tbase

tbase.unmount(Sensor.new(skeleton))

pp tbase
# tbase.unmount(Sensor.new(skeleton))
# a1 = tbase.mount(Appearance.new(1234, Term.of(x: 100, y: 200)))
# pp a1
# pp tbase

struct Etrace
  alias Node = Member | Successor | SuccessorCount

  record Member, step : Vertex, member : Vertex
  record Successor, step : Vertex, index : UInt32
  record SuccessorCount, step : Vertex

  def initialize
    @data = {} of Node => UInt32
  end

  def mount(path : Slice(Vertex))
    path.each_cons_pair do |curr, succ|
      membership = Member.new(curr, succ)

      # transaction {
      if refcount = @data[membership]?
        @data[membership] = refcount + 1
        next
      end

      @data[membership] = 1u32

      # Append to successors list
      successors = SuccessorCount.new(curr)

      size = @data[successors]? || 0u32

      @data[Successor.new(curr, size)] = succ
      @data[successors] = size + 1
      # }
    end
  end

  def unmount(path : Slice(Vertex))
    path.each_cons_pair do |curr, succ|
      membership = Member.new(curr, succ)

      unless refcount = @data[membership]?
        raise ArgumentError.new
      end

      if refcount > 1
        @data[membership] = refcount - 1
        next
      end

      @data.delete(membership)

      successors = SuccessorCount.new(curr)
      size = @data.delete(successors)
      unless size
        raise ArgumentError.new
      end

      (0u32...size).each do |index|
        @data.delete(Successor.new(curr, index))
      end
    end
  end
end

trace = Etrace.new
trace.mount(Slice[0u32, 17u32, 18u32, 19u32, 20u32, 1234u32])
trace.mount(Slice[0u32, 17u32, 21u32, 22u32, 23u32, 1234u32])
trace.mount(Slice[0u32, 17u32, 24u32, 25u32, 26u32, 4567u32])
trace.unmount(Slice[0u32, 17u32, 24u32, 25u32, 26u32, 4567u32])
# trace.each_child(0u32) do |ep|
#   pp ep
# end

pp trace
# trace.subscribe(path, subject.id)

# -> Sensor groups, appearances == model
# <- view

# each sensor has a scope and a query
# each appearance has a scope, a value, and a tombstone

{% skip_file %}

tally_v = ExtrinsicTally(Ttrie::Node).new
fresh = VERTEX_ZERO
ttrie = Ttrie.new(tally_v, ExtrinsicMap(Ttrie::Node, Vertex).new, ExtrinsicMap(Vertex, SetSum).new)
puts "Add"
endpoints = ExtrinsicMap(Term, UInt32).new
(0...2).each do |i|
  (0...2).each do |j|
    endpoint = fresh
    t = Term.of(x: i, y: j)
    endpoints.assign(t, endpoint)
    fresh += 1
    fresh = mount(ttrie, t, endpoint, fresh)
  end
end

puts "Query"
pp ttrie

sets = [] of SetSum
ttrie.query({Ubase::IsDict.new, Ubase::At.new(Term.of(:x)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(1))}) do |set|
  sets << set
end
ttrie.query({Ubase::IsDict.new, Ubase::At.new(Term.of(:y)), Ubase::IsNum.new, Ubase::Literal.new(Term.of(1))}) do |set|
  sets << set
end
hits = [] of Vertex
unless sets.empty?
  sets.unstable_sort_by!(&.population)

  pivot = sets.first
  pivot.each do |endpoint|
    next unless (1...sets.size).all? { |index| endpoint.in?(sets[index]) }

    hits << endpoint
  end
end
pp hits

puts "Delete"
(1...2).each do |i|
  (1...2).each do |j|
    t = Term.of(x: i, y: j)
    endpoint = endpoints.get?(t) || raise ""
    unmount(ttrie, t, endpoint)
    endpoints.unload(t)
  end
end
pp ttrie

# require "benchmark"

# Benchmark.ips do |x|
#   x.report("intersect") do

#   end
# end
{% skip_file %}

# id1, f = xg.mount([1, 2, 3] of Vertex, f)
# id2, f = xg.mount([1, 2, 5] of Vertex, f)
# id3, f = xg.mount([2, 3, 5] of Vertex, f)
# rules << id0
# rules << id1
# rules << id2
# rules << id3

# xg.unmount([1, 2, 3, 4] of Vertex)
# rules.delete(id0)

# xg.unmount([2, 3, 5] of Vertex)
# rules.delete(id3)

# xg.unmount([1, 2, 3] of Vertex)
# rules.delete(id1)

# xg.unmount([1, 2, 5] of Vertex)
# rules.delete(id2)

# pp xg

# xg.conjunctions(Deque(Vertex){1, 2, 3, 4, 5}) do |v|
#   next unless v.in?(rules)
#   pp v
# end

# {% skip_file %}

tally_xg = ExtrinsicTally(Xgraph::Node).new
tally_v = ExtrinsicTally(Utrie::Node).new
fresh = VERTEX_ZERO
utrie = Utrie.new(tally_v, ExtrinsicMap(Utrie::Node, Vertex).new)
strands = Pf::Set(UInt32).new
xg = Xgraph.new(tally_xg, ExtrinsicMap(Xgraph::Node, Vertex).new)
rules = Set(Vertex).new

# pattern = ML.term(%{((%any + -) (%any° a_number a_string) b_number ¦ () x_number)})
pattern = ML.term %{((%any div mod) a_ (%all b_number (%not 0)) ¦ precision⋮ 3)}
normp = M1.normal(pattern)
skeleton = Skeleton.pattern(normp)

# Add

puts "Add"

branches(skeleton) do |branch|
  puts "Sensor"

  conj = Deque(Vertex).new

  strands(branch) do |strand|
    id, fresh = utrie.mount(strand.items, fresh)
    strands = strands.add(id)
    puts "+  #{strand} #{id}"
    conj << id
  end

  conj.unstable_sort!

  id, fresh = xg.mount(conj, fresh)
  rules << id

  puts "Sensor #{branch} = #{id}"
end

# tally_t = ExtrinsicTally(Ttrie::Node).new
# ttrie = Ttrie.new(tally_t, ExtrinsicMap(Ttrie::Node, Vertex).new)

# sets = {} of Vertex => Set(Vertex)
# rsets = {} of Set(Vertex) => Vertex

# members, fresh = ttrie.mount(Term.of(:div, 100, 200, precision: 3), fresh)
# unless id = rsets[members]?
#   id = fresh
#   fresh += 1
# end
# sets.put_if_absent(id) { members }

# members, fresh = ttrie.mount(Term.of(:div, 200, 300, precision: 3), fresh)
# unless id = rsets[members]?
#   id = fresh
#   fresh += 1
# end
# sets.put_if_absent(id) { members }

# members, fresh = ttrie.mount(Term.of(:div, 300, 400, precision: 3), fresh)
# unless id = rsets[members]?
#   id = fresh
#   fresh += 1
# end
# sets.put_if_absent(id) { members }

# subordinates = Set(Vertex).new
# ttrie.query([Ubase::IsDict.new] of Ubase::Any) do |sub|
#   if subordinates.empty?
#     subordinates = sets[sub]
#   else
#     subordinates &= sets[sub]
#   end
# end
# pp subordinates

# --------------------------------

# puts "Delete"
# # Delete

# branches(skeleton) do |branch|
#   puts "Sensor"

#   conj = Deque(Vertex).new

#   strands(branch) do |strand|
#     pred = VERTEX_ROOT

#     strand.items.each do |base|
#       pred = utrie.unmount(pred, Ubase.parse(base))
#     end

#     strands = strands.delete(pred)
#     conj << pred
#   end

#   conj.unstable_sort!

#   id = xg.unmount(conj)
#   rules.delete(id)

#   puts "-  Sensor #{branch} = #{id}"
# end

# pp utrie
# pp xg

# hit = Deque(UInt32).new

# utrie.query(Term.of(:div, 100, 200, precision: 3)) do |id|
#   next unless id.in?(strands)
#   hit << id
# end

# hit.unstable_sort!

# pp hit
# xg.conjunctions(hit) do |id|
#   next unless id.in?(rules)
#   pp id
# end

# hit = Deque(UInt32).new

# utrie.query(Term.of(:mod, 100, 200, precision: 3)) do |id|
#   next unless id.in?(strands)
#   hit << id
# end

# hit.unstable_sort!

# pp hit
# xg.conjunctions(hit) do |id|
#   next unless id.in?(rules)
#   pp id
# end

# + %[(%let (%capture capture_) successor_)]
# + %[(%itemseq _*)]
# + %[(%pass)]
# + %[(%'%literal term_)]
# + %[(%'%partition itemspart_ pairspart_)]
# + {:"%string"}
# + {:"%symbol"}
# + {:"%boolean"}
# + {:"%dict"}
# * :"%keypath", {:"%capture", :capture_}}
# {:"%keypool", :_, :"_*"}
# { {:"%literal", :"%layer"}, :below_, :side_ }
# {:"%value", {:"%capture", :capture_}, :value_}
# {:"%-value", {:"%capture", :capture_}}
# {:"%-value", {:"%capture", :capture_}, {:"%barrier", :name_}
# + %[(%pipe (%barrier (+ n_number)) successor_)]
# + %[(%pipe (%barrier (- n_number)) successor_)]
# + %[(%pipe (%barrier (* n_number)) successor_)]
# + %[(%pipe (%barrier (/ n_number)) successor_)]
# + %[(%pipe (%barrier (div n_number)) successor_)]
# + %[(%pipe (%barrier (mod n_number)) successor_)]
# + %[(%pipe (%barrier (** n_number)) successor_)]
# + %[(%pipe (%barrier (map arg_dict)) successor_)]
# + %[(%pipe (%barrier span) successor_)]
# + %[(%pipe (%barrier tally) successor_)]
# + %[(%pipe (%barrier type) successor_)]
# {:"%items/first", :_, :"_*"}
# {:"%items/source", :_, :"_*"}
# {:"%partition",
#  {:"%items/all", {:"%capture", :capture_}, :_, :"_*"},
#  {min: :min0_, max: :max0_}},

# {:"%partition",
#  {:"%entries/all", {:"%capture", :capture_}, :k_, :v_},
#  {min: :min0_, max: :max0_}},

# {:"%entries/first", :k_, :v_}
# {:"%entries/source", :k_, :v_}
# Term[:"%leaves/first", :body_, in: :part_, order: :dfs, self: :depth0_boolean]
# Term[:"%leaves/first", :body_, in: :part_, order: :bfs, self: :depth0_boolean]
# Term[:"%leaves/source", :body_, in: :part_, order: :dfs, self: :depth0_boolean]
# Term[:"%leaves/all", {:"%capture", :capture_}, :body_, in: :part_, min: :min_, max: :max_, order: :dfs, self: :depth0_boolean]
# Term[:"%leaves/all", {:"%capture", :capture_}, :body_, in: :part_, min: :min_, max: :max_, order: :bfs, self: :depth0_boolean]
# + {:"%all", :a_, :b_}
# + %[(%any/literal _*)]
# + {:"%any/source", :a_}
# + {:"%any/source", :a_, :b_}
# + {:"%any/source", :a_, :_, :"_*"}
# {:"%edge", {:"%literal", :_}
# {:"%edge", {:"%literal", :_symbol}
# {:"%edge", {:"%literal", :_string}
# {:"%edge", {:"%literal", :_number}
# * {:"%not", :_, :"_*"}
# + {:"%number", {:"%literal", :_}
# + {:"%number", {:"%literal", {:whole, :_}}}
# + {:"%number", :x_, :op_symbol, :b_number}
# + {:"%number", :a_number, :lop_symbol, :x_, :rop_symbol, :b_number}
# * {:"%new", :pattern_}, {:"%new", :_, :pattern_}
# + %[(%symbol nonblank)]
# + %[(%symbol blank name_ type_)]
