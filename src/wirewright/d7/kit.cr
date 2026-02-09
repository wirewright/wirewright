module Ww::D7
  alias MatchTable = Pf::Map(Term, MatchGroup)

  alias MatchGroup = Slice(Match)

  defrecord Match, node : Node, env : Term::Dict

  # Returns the first `Match` in *object*.
  def match(object : Match) : Match
    object
  end

  # :ditto:
  def match(object : MatchGroup) : Match
    match(object.first)
  end

  # Returns the first node in *object*.
  def node(object) : Node
    match(object).node
  end

  # Returns the identifier of the first node in *object*.
  def id(object) : NodeId
    node(object).id
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

  # Maps each term in *goal* to its index in *src*.
  #
  # NOTE: Assumes 1:1 correspondence. Extra items in *src*, *goal*, or
  # both raise.
  def permutation(src : MatchGroup, capture, goal : Indexable(Term)) : Slice(Int32) forall T
    assert src.size == goal.size

    if src.size < 8 # Fast path
      return src.to_readonly_slice { |match| goal.index!(fetch(match, capture)) }
    end

    #  src  a c b
    # goal  b a c
    table = {} of Term => Int32
    src.each_with_index do |match, index|
      table[fetch(match, capture)] = index
    end

    # table
    #   a 0
    #   c 1
    #   b 2
    # -->
    # b a c
    # -->
    # 2 0 1
    goal.to_readonly_slice { |term| table[term] }
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
  # See also `Term::Dict#morph` (in the future `Term.morph`).
  def patch(object : Node, *morphseq) : Patch
    result = object.term.morph(*morphseq)

    Pf::Map.assoc(object.id, Term.of(result))
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
  # changes of the current one, then we don't need to show the current frame
  # to the user; they'll see the changes in the next frame anyway". In other
  # words, if the next frame subsumes the current one, the current one is skipped.
  #
  # Yields frames to show to the user.
  #
  # This method may yield duplicate consecutive frames, and it is the caller's
  # responsibility to filter them out. We do not do it here because the caller is likely
  # to do that at frame-level anyway, so there is no need to do the work on subframes.
  #
  # *seen* must be the last frame seen by the user. Usually this would be the last
  # frame yielded by this method. Otherwise it would be the very first circuit,
  # which the caller itself should show to the user as the first frame. This method
  # will never yield *seen* (unless as a duplicate).
  def fuse(clf : Classifier, seen : Term, subframes : Indexable(Term), &) : Nil
    return if subframes.empty?

    changed = Set(NodeAddr).new

    ahead = Deque(Term).new
    ahead.concat(subframes)

    a = seen
    ns = node_map(clf, seen, split: false)

    while b = ahead.shift?
      ms = node_map(clf, b, split: false)

      # Cut if:
      # - New nodes were added or removed in the next subframe.
      # - A node that was already modified was modified in the next subframe.
      unless ns.size == ms.size && ns.all? { |addr, _| ms.has_key?(addr) } && ms.all? { |addr, m| !addr.in?(changed) || ns[addr] == m }
        yield a
        a = b
        ns = ms
        changed.clear
        next
      end

      # Changes are disjoint. We can skip showing A because B has all
      # the same changes.
      ms.each do |addr, m|
        n = ns[addr]?
        next if n == m

        changed << addr
      end

      a = b
      ns = ms
    end

    yield a
  end
end
