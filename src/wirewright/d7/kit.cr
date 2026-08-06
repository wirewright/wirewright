module Ww::D7
  defcase MatchTable, groups : Pf::Map(Term, MatchGroup) do
    # :nodoc:
    EMPTY = new(groups: Pf::Map(Term, MatchGroup).new)

    def self.new
      EMPTY
    end

    def self.assoc(key, value) : MatchTable
      EMPTY.assoc(key, value)
    end

    def assoc(key, object : MatchGroup) : MatchTable
      {% unless flag?(:release) %}
        assert !groups.has_key?(key)
      {% end %}

      copy_with(groups: groups.assoc(key, object))
    end
  end

  def group?(table : MatchTable, name : Term) : MatchGroup?
    table.groups[name]?
  end

  def group(table : MatchTable, name : Term) : MatchGroup
    group?(table, name) || raise KeyError.new
  end

  def degree(object) : Int32
    degree = 0
    each_match(object) do
      degree += 1
    end
    degree
  end

  def select(match_table : MatchTable, & : Match -> Bool) : MatchTable
    groups1 = match_table.groups.map_value do |group|
      self.select(group) { |match| yield match }
    end

    MatchTable.new(groups1)
  end

  def select(group : MatchGroup, & : Match -> Bool) : MatchGroup
    filtered = Pf::Kit.stack_array(Match)

    group.each do |match|
      next unless yield match

      filtered << match
    end

    if filtered.size == group.size
      return group
    end

    filtered.to_readonly_slice(&.itself)
  end

  alias MatchGroup = Slice(Match)

  defrecord Match, hg : Hypergraph, node : Node, env : Term::Dict

  # Returns the first `Match` in *object*.
  def match(object : Match) : Match
    object
  end

  # :ditto:
  def match(object : MatchGroup) : Match
    match(object.first)
  end

  def each_match(object : Match, &) : Nil
    yield object
  end

  def each_match(object : MatchGroup, &) : Nil
    object.each { |match| yield match }
  end

  def each_match(object : MatchTable, &) : Nil
    object.groups.each do |_, group|
      each_match(group) { |match| yield match }
    end
  end

  # Returns the first node in *object*.
  def node(object) : Node
    match(object).node
  end

  # Returns the identifier of the first node in *object*.
  def id(object) : NodeId
    node(object).id
  end

  # Shorthand for running `Term::Dict#[]?(*args)`on the first term in *object*.
  def part?(object, *args)
    node(object).term[*args]?
  end

  # Retrieves the value associated with *capture* in the first match env
  # in *object*.
  #
  # *capture* is converted to a term using `Term.of`.
  def fetch(object, capture) : Term
    match(object).env[capture]
  end

  # Retrieves the values associated with *captures* in the first match env
  # in *object*.
  def fetch(object, *captures) : Tuple
    captures.map { |capture| fetch(object, capture) }
  end

  def find?(match_group : MatchGroup, *, where capture : Term | Symbol, eq needle : AbsEdge) : Match?
    capture = Term.of(capture)

    match_group.find do |match|
      D7.resolve(fetch(match, capture), wrt: match) == needle
    end
  end

  # Same as `find?`, but raises `Enumerable::NotFoundError` if no matches
  # were found.
  def find(*args, **kwargs) : Match
    find?(*args, **kwargs) || raise Enumerable::NotFoundError.new
  end

  # FIXME: Not sure what this function is doing. Is there a better name?
  def permutation(dev : Match, src : MatchGroup, capture, arranged_like_in goal : Indexable(Term)) : Slice(Int32) forall T
    assert src.size == goal.size

    if src.size < 8 # Fast path
      permutation = src.to_readonly_slice do |match|
        goal.index! do |candidate|
          D7.resolve(candidate, wrt: dev) == D7.resolve(fetch(match, capture), wrt: match)
        end
      end
      return permutation
    end

    #  src  a c b
    # goal  b a c
    table = {} of AbsEdge => Int32
    src.each_with_index do |match, index|
      table[D7.resolve(fetch(match, capture), wrt: match)] = index
    end

    # table
    #   a 0
    #   c 1
    #   b 2
    # -->
    # b a c
    # -->
    # 2 0 1
    goal.to_readonly_slice { |term| table[D7.resolve(term, wrt: dev)] }
  end

  # See `Hypergraph#resolve`.
  def resolve(edge : Term, *, wrt match : Match) : AbsEdge
    match.hg.resolve(match.node.addr, edge)
  end

  # An immutable map of node ids to replacement terms.
  #
  # Disjoint changes to the same node are supported and will be properly merged.
  alias Patch = Pf::Map(NodeId, Term)

  # Constructs a patch that replaces all nodes in *object* with *term*.
  def replace(object : Node, term : Term) : Patch
    Pf::Map.assoc(object.id, term)
  end

  # :ditto:
  def replace(object : Match, term : Term) : Patch
    replace(object.node, term)
  end

  # :ditto:
  def replace(object : MatchGroup, term : Term) : Patch
    patches(object) { |match| replace(match, term) }
  end

  # Constructs a patch that morphs node terms in *object* according
  # to *morphseq*.
  #
  # See also `Term.morph`.
  def patch(object : Node, *morphseq) : Patch
    Patch.assoc(object.id, Term.morph(object.term, *morphseq))
  end

  # :ditto:
  def patch(object : Match, *morphseq) : Patch
    patch(object.node, *morphseq)
  end

  # :ditto:
  def patch(object : MatchGroup, *morphseq) : Patch
    patches(object) { |match| patch(match, *morphseq) }
  end

  # Constructs patches for each object in *objects* using the block;
  # merges the resulting patches into one final patch.
  #
  # Conflicting changes from later patches (*objects*-wise, with greater index)
  # win over former ones.
  def patches(objects : Enumerable(T), & : T, Int32 -> Patch) : Patch forall T
    patch = Patch.new
    objects.each_with_index do |object, index|
      patch = patch.merge(yield object, index)
    end
    patch
  end

  def patches(objects : Enumerable(Node), *morphseq) : Patch
    patches(objects) { |object| patch(object, *morphseq) }
  end

  # Shorthand for `patches` when all objects are patches already.
  def patches(objects : Enumerable(Patch)) : Patch
    patches(objects, &.itself)
  end

  # Shorthand that lets you list & merge multiple patches from the arguments.
  def patches(*objects : Patch) : Patch
    patches(objects)
  end

  # Performs *subframe fusion*.
  #
  # *Subframe fusion* is a fancy way of saying "If the next frame has all
  # changes of the current one, then we don't need to show the current one
  # to the user; they'll see the changes in the next frame anyway". In other
  # words, if the next frame *subsumes* the current one, the current one is skipped.
  #
  # Calls *fn* with frames to show to the user.
  #
  # This method may yield duplicate consecutive frames, and it is the caller's
  # responsibility to filter them out. We do not filter here because the caller
  # is likely to filter at frame-level anyway, so there is no need to do
  # the work on subframe-level.
  #
  # *ancestor* is the last frame seen by the user. Usually this would be the last
  # frame produced by this method. Otherwise it would be the very first circuit,
  # which the caller itself should show to the user as the first frame.
  def fuse(parser : Parser, ancestor : Term, subframes : Slice(Term), &fn : Term ->) : Nil
    if subframes.empty?
      fn.call(ancestor)
      return
    end

    ancestor_nodes = fuse_map(parser, ancestor)
    fuse(parser, ancestor, ancestor_nodes, subframes, &fn)
  end

  private def fuse(parser : Parser, ancestor : Term, ancestor_nodes : Hash(NodeAddr, Term), subframes : Slice(Term), &fn : Term ->) : Nil
    if subframes.empty? # Base case
      fn.call(ancestor)
      return
    end

    # Notice that this is an iterator.
    assessments = subframes.each.map do |subframe|
      nodes = fuse_map(parser, subframe)

      {subframe: subframe,
       nodes:    nodes,
       changes:  fuse_changeset(ancestor_nodes, nodes)}
    end

    acc = Set(NodeAddr).new

    assessments.each_with_index do |assessment, index|
      if acc.intersects?(assessment[:changes]) # This one is not disjoint wrt. acc, cut!
        assert index > 0

        fn.call(subframes[index - 1])

        # This leaves a hole for assessment[:subframe], which is now the new
        # ancestor. In case there are no more subframes past it, it is emitted
        # (see the base case below). If there are more subframes, they are either
        # accumulated, or if there is a cut immediately, the line above will emit
        # assessment[:subframe].

        return fuse(parser, assessment[:subframe], assessment[:nodes], subframes + index + 1, &fn)
      end

      # Changes are disjoint wrt. acc.
      acc.concat(assessment[:changes])
    end

    fn.call(subframes.last)
  end

  private def fuse_changeset(pred, succ) : Set(NodeAddr)
    changed = Set(NodeAddr).new

    pred.each do |addr, node|
      next if succ.has_key?(addr)

      changed << addr # Removed
    end

    succ.each do |addr, node|
      ancestor_node = pred[addr]?
      next if ancestor_node == node

      changed << addr # Added or updated
    end

    changed
  end

  private def fuse_map(parser : Parser, circuit : Term)
    nodes = {} of NodeAddr => Term

    feature_tree = parser.parse(circuit)
    D7.each_flat_feature_with_addr(feature_tree) do |feature, addr|
      nodes[addr] = feature.node
    end

    nodes
  end
end
