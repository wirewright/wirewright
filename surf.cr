require "./src/wirewright"

# Sensors:
#   pattern
#   -> normal
#   -> pattern skeleton
#   -> dnf (array of patterns)
#   -> pattern strands for each skeleton (dnf)
#   -> add to utrie
#   -> register in to xgraph

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
# - `%'(%pass)`
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
  alias Any = At | IsAny | IsSym | IsStr | IsNum | IsBool | IsDict | Literal

  record At, key : Term
  record IsAny
  record IsSym
  record IsStr
  record IsNum
  record IsBool
  record IsDict
  record Literal, value : Term

  def self.parse(base : Term)
    Term.case(base) do
      matchpi %{(%'%value (%'%literal key_))} { At.new(key) }
      matchpi %{%'(%pass)} { IsAny.new }
      matchpi %{%'(%symbol)} { IsSym.new }
      matchpi %{%'(%string)} { IsStr.new }
      matchpi %{%'(%number _)} { IsNum.new }
      matchpi %{%'(%boolean)} { IsBool.new }
      matchpi %{%'(%dict)} { IsDict.new }
      matchpi %{(%'%literal value_)} { Literal.new(value) }
    end
  end
end

# FIXME: these are interfaces, not structs!!
struct ExtrinsicMap(K, V)
  def initialize(@map = Hash(K, V).new)
  end

  def get?(key : K)
    @map[key]?
  end

  def load(key : K, default : V) : V
    @map.put_if_absent(key) { default }
  end

  def unload(key : K) : Nil
    @map.delete(key)
  end
end

# FIXME: these are interfaces, not structs!!
struct ExtrinsicTally(T)
  def initialize(@tally = Hash(T, UInt32).new)
  end

  def tally?(object : T)
    @tally[object]? || 0u32
  end

  def incref(object : T)
    @tally[object] = (@tally[object]? || 0u32) + 1
  end

  def decref?(object : T)
    unless n = @tally[object]?
      raise KeyError.new
    end

    if n == 1
      @tally.delete(object)

      true
    else
      @tally[object] = n - 1

      false
    end
  end
end

alias Vertex = UInt32

VERTEX_ROOT = Vertex.new(0)
VERTEX_ZERO  = VERTEX_ROOT + 1

struct Utrie
  record Node, pred : Vertex, base : Ubase::Any

  def initialize(@tally : ExtrinsicTally(Node), @successors : ExtrinsicMap(Node, Vertex))
  end

  # :nodoc:
  def unregister(pred : Vertex, base : Ubase::IsAny) : Vertex
    pred
  end

  # Unregisters *base* with the given predecessor *pred*.
  #
  # Returns the successor vertex.
  #
  # NOTE: the caller guarantees that *base* was registered with *pred* before.
  def unregister(pred : Vertex, base : Ubase::Any) : Vertex
    node = Node.new(pred, base)
    successor = @successors.get?(node) || raise ArgumentError.new("base does not exist")

    if @tally.decref?(node)
      @successors.unload(node)
    end

    successor
  end

  # :nodoc:
  def register(pred : Vertex, base : Ubase::IsAny, fresh : Vertex) : {Vertex, Vertex}
    {pred, fresh}
  end

  # :nodoc:
  #
  # Registers *base* with the given predecessor *pred*.
  #
  # Returns the successor vertex and the new *fresh* vertex.
  def register(pred : Vertex, base : Ubase::Any, fresh : Vertex) : {Vertex, Vertex}
    node = Node.new(pred, base)

    @tally.incref(node)

    vertex = @successors.load(node, fresh)

    {vertex, vertex == fresh ? (fresh + 1) : fresh}
  end

  # Registers *strand*.
  #
  # Returns the successor vertex and the new *fresh* vertex.
  def register(strand : Enumerable(Term), fresh : Vertex) : {Vertex, Vertex}
    strand.reduce({VERTEX_ROOT, fresh}) do |(pred, fresh), base|
      register(pred, Ubase.parse(base), fresh)
    end
  end

  private def follow(pred : Vertex, term : Term::Num, sink : Vertex ->) : Nil
    return unless successor = @successors.get?(Node.new(pred, Ubase::IsNum.new))

    sink.call(successor)
  end

  private def follow(pred : Vertex, term : Term::Str, sink : Vertex ->) : Nil
    return unless successor = @successors.get?(Node.new(pred, Ubase::IsStr.new))

    sink.call(successor)
  end

  private def follow(pred : Vertex, term : Term::Sym, sink : Vertex ->) : Nil
    return unless successor = @successors.get?(Node.new(pred, Ubase::IsSym.new))

    sink.call(successor)
  end

  private def follow(pred : Vertex, term : Term::Boolean, sink : Vertex ->) : Nil
    return unless successor = @successors.get?(Node.new(pred, Ubase::IsBool.new))

    sink.call(successor)
  end

  private def follow(pred : Vertex, term : Term::Dict, sink : Vertex ->) : Nil
    return unless successor0 = @successors.get?(Node.new(pred, Ubase::IsDict.new))

    sink.call(successor0)

    term.each_entry do |key, value|
      next unless successor1 = @successors.get?(Node.new(successor0, Ubase::At.new(key)))

      sink.call(successor1)

      follow(successor1, value, sink)
    end
  end

  private def follow(pred : Vertex, term : Term, sink : Vertex ->) : Nil
    if successor = @successors.get?(Node.new(pred, Ubase::Literal.new(term)))
      sink.call(successor)
    end

    follow(pred, term.downcast, sink)
  end

  private def follow(term : Term, sink : Vertex ->) : Nil
    follow(VERTEX_ROOT, term, sink)
  end

  # Calls *sink* for each vertex hit by *term*.
  def follow(term : Term, &sink : Vertex ->) : Nil
    follow(term, sink)
  end
end

struct Xgraph
  record Node, a : Vertex, b : Vertex

  def initialize(@tally : ExtrinsicTally(Node), @successors : ExtrinsicMap(Node, Vertex))
  end

  # Registers an Xgraph rule *xrule*.
  #
  # *fresh* is the origin of fresh ids.
  #
  # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
  # by passing it to this method.
  def register(xrule : Deque(Vertex), fresh : Vertex)
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      node = Node.new(a, b)

      @tally.incref(node)

      successor = @successors.load(node, fresh)
      if successor == fresh
        fresh += 1
      end

      xrule << successor
    end

    {xrule[0], fresh}
  end

  # Unregisters an Xgraph rule *xrule*.
  #
  # Returns the vertex of the rule that was unregistered (so that the caller
  # perhaps deletes it in its own data structures).
  #
  # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
  # by passing it to this method.
  #
  # WARNING: the caller guarantees that *xrule* was registered.
  def unregister(xrule : Deque(Vertex)) : Vertex
    if xrule.empty?
      raise ArgumentError.new
    end

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      node = Node.new(a, b)

      unless successor = @successors.get?(node)
        raise ArgumentError.new("rule does not exist")
      end

      if @tally.decref?(node)
        @successors.unload(node)
      end

      xrule << successor
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

  # Calls *sink* with all registered conjunction vertices in *vertices*.
  #
  # NOTE: *vertices* must be pre-sorted ascending. You lose ownership of *vertices*
  # by passing it to this method.
  def conjunctions(vertices : Deque(Vertex), &sink : Vertex ->)
    conjunctions(vertices, sink)
  end
end

struct Ttrie
  # ...
end

# {% skip_file %}

# id1, f = xg.register([1, 2, 3] of Vertex, f)
# id2, f = xg.register([1, 2, 5] of Vertex, f)
# id3, f = xg.register([2, 3, 5] of Vertex, f)
# rules << id0
# rules << id1
# rules << id2
# rules << id3

# xg.unregister([1, 2, 3, 4] of Vertex)
# rules.delete(id0)

# xg.unregister([2, 3, 5] of Vertex)
# rules.delete(id3)

# xg.unregister([1, 2, 3] of Vertex)
# rules.delete(id1)

# xg.unregister([1, 2, 5] of Vertex)
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
    id, fresh = utrie.register(strand.items, fresh)
    strands = strands.add(id)
    puts "+  #{strand} #{id}"
    conj << id
  end

  conj.unstable_sort!

  id, fresh = xg.register(conj, fresh)
  rules << id

  puts "Sensor #{branch} = #{id}"
end

# tally_t = ExtrinsicTally(Ttrie::Node).new
# ttrie = Ttrie.new(tally_t, ExtrinsicMap(Ttrie::Node, Vertex).new)

# sets = {} of Vertex => Set(Vertex)
# rsets = {} of Set(Vertex) => Vertex

# members, fresh = ttrie.register(Term.of(:div, 100, 200, precision: 3), fresh)
# unless id = rsets[members]?
#   id = fresh
#   fresh += 1
# end
# sets.put_if_absent(id) { members }

# members, fresh = ttrie.register(Term.of(:div, 200, 300, precision: 3), fresh)
# unless id = rsets[members]?
#   id = fresh
#   fresh += 1
# end
# sets.put_if_absent(id) { members }

# members, fresh = ttrie.register(Term.of(:div, 300, 400, precision: 3), fresh)
# unless id = rsets[members]?
#   id = fresh
#   fresh += 1
# end
# sets.put_if_absent(id) { members }

# subordinates = Set(Vertex).new
# ttrie.follow([Ubase::IsDict.new] of Ubase::Any) do |sub|
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
#       pred = utrie.unregister(pred, Ubase.parse(base))
#     end

#     strands = strands.delete(pred)
#     conj << pred
#   end

#   conj.unstable_sort!

#   id = xg.unregister(conj)
#   rules.delete(id)

#   puts "-  Sensor #{branch} = #{id}"
# end

# pp utrie
# pp xg

# hit = Deque(UInt32).new

# utrie.follow(Term.of(:div, 100, 200, precision: 3)) do |id|
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

# utrie.follow(Term.of(:mod, 100, 200, precision: 3)) do |id|
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
