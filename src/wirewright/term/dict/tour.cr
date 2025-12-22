# `Tour` implements low-overhead, immutable "path increment" functions for
# traversing dictionary terms.
module Ww::Term::Dict::Tour
  extend self

  alias Any = DepthFirst | BreadthFirstInitial | BreadthFirst | Scan | Final

  # :nodoc:
  record DepthFirst, root : Term, path : Pf::UPath32, conf : Conf, itself : Bool

  # :nodoc:
  record BreadthFirstInitial, root : Term, conf : Conf

  # :nodoc:
  record BreadthFirst, root : Term, path : Pf::UPath32, conf : Conf

  # :nodoc:
  record Scan, root : Term, n : UInt32, conf : Conf

  # :nodoc:
  record Final

  # Short for *preference*. Determines the preferred way of handling items
  # and pairs in `Conf`.
  enum Pref
    # Lets you skip itemspart or pairspart.
    Skip

    # If items or pairs are `Unordered`, they are emitted in dict memory-order.
    Unordered

    # If items are `Ordered`, they are ordered by key (0 to itemsize). If pairs
    # are `Ordered`, they are ordered lexicographically (using `Term.compare`)
    Ordered
  end

  # NOTE: Traversal always proceeds first into a dict's itemspart; then moves to
  # its pairspart.
  defrecord Conf, items : Pref, pairs : Pref

  # Returns the default search configuration `Conf`.
  def default_conf : Conf
    Conf.new(items: :ordered, pairs: :unordered)
  end

  # Constructs depth-first traversal state for *root*.
  #
  # *itself* sets whether *root* itself should be emitted at the end of
  # traversal (since this is DFS).
  def dfs(root : Term, conf : Conf = default_conf, *, itself : Bool = true)
    DepthFirst.new(root, Pf::UPath32[0], conf, itself)
  end

  # Constructs breadth-first traversal state for *root*.
  #
  # *itself* sets whether *root* itself should be emitted at the beginning
  # of traversal (since this is BFS).
  def bfs(root : Term, conf : Conf = default_conf, *, itself : Bool = true)
    if itself
      BreadthFirstInitial.new(root, conf)
    else
      BreadthFirst.new(root, Pf::UPath32[], conf)
    end
  end

  # Constructs scan state for *root*.
  def scan(root : Term, conf : Conf = default_conf)
    Scan.new(root, 0, conf)
  end

  # Bridge to Dict's `nth?`.
  private def nth?(dict : Term::Dict, n : UInt32, conf : Conf) : {Term, Term}?
    n = n.to_i

    case conf.items
    in .skip?
    in .ordered?
      if value = dict.items[n]?
        return Term.of(n), value
      end

      assert n >= dict.itemsize
      n -= dict.itemsize
    in .unordered?
      if entry = dict.itemspart.nth?(n)
        _, value = entry
        return Term.of(n), value
      end

      assert n >= dict.itemsize
      n -= dict.itemsize
    end

    case conf.pairs
    in .skip?
    in .ordered?
      return unless n < dict.pairsize

      # TODO: Is there any better / more efficient way to do this? We can probably
      # min our way through, can't we?
      dict.pairspart.each_entry_ord do |key, value|
        if n.zero?
          return key, value
        end

        n -= 1
      end

      unreachable
    in .unordered?
      dict.pairspart.nth?(n)
    end
  end

  private def nth(dict : Term::Dict, n : UInt32, conf : Conf)
    nth?(dict, n, conf) || raise IndexError.new
  end

  # Bridge to Dict's enumeration methods.
  private def each_value(dict : Term::Dict, conf : Conf, &) : Nil
    case conf.items
    in .skip?
    in .ordered?   then dict.items.each { |item| yield item }
    in .unordered? then dict.each_item_unordered { |item| yield item }
    end

    case conf.pairs
    in .skip?
    in .ordered?   then dict.pairspart.each_entry_ord { |_, value| yield value }
    in .unordered? then dict.each_pair { |_, value| yield value }
    end
  end

  # :nodoc:
  def step?(state : DepthFirst) : {DepthFirst | Final, Pf::UPath32, Term}?
    path = state.path
    if path.empty?
      if state.itself
        return Final.new, Pf::UPath32[], state.root
      else
        return
      end
    end

    stack = Pf::Kit::HybridArray(Term::Dict, 32).new

    # Otherwise state.path is malformed, but we're in control of state.path
    # so it can't be malformed!
    stack << state.root.as_d

    path.prior.each do |n|
      _, node = nth(stack.top, n, state.conf)
      stack << node.as_d # ditto
    end

    loop do
      break unless node = stack.pop?

      n = path.tip
      unless entry = nth?(node, n, state.conf)
        path = path.prior
        break if path.size.zero?
        return state.copy_with(path: path.goto(path.tip + 1)), path, Term.of(node)
      end

      _, value = entry
      unless child = value.as_d?
        return state.copy_with(path: path.goto(n + 1)), path, value
      end

      # This is depth-first search. *value* will be emitted as we ascend
      # from it and move to its successor.
      path = path.append(0u32)
      stack << node
      stack << child
    end

    assert path.empty?
    assert stack.empty?

    return unless state.itself

    {Final.new, Pf::UPath32[], state.root}
  end

  # :nodoc:
  def step?(state : Final) : Nil
  end

  # Returns the first valid path of length *len* for *term*. Returns `nil`
  # if no such path exists.
  private def path0?(term : Term, len : UInt32, conf : Conf) : {Pf::UPath32, Term}?
    if len.zero?
      return Pf::UPath32[], term
    end

    return unless node = term.as_d?
    return if node.empty?

    # NOTE: nth() and each_value() are guaranteed to have the same order.
    n = 0u32
    each_value(node, conf) do |value|
      # We duplicate the checks here to avoid append() which could lead
      # to a pointless allocation if these checks fail in the recursive call.
      if row = path0?(value, len - 1, conf)
        path, value = row
        return path.prepend(n), value
      end

      n += 1
    end
  end

  # "Advances" *path* into *root* until it points to a valid path successor.
  # Returns a modified copy of *path* and the valid successor. Returns `nil`
  # if no such path exists.
  private def advance?(term : Term, path : Pf::UPath32, conf : Conf) : {Pf::UPath32, Term}?
    return if path.empty?
    return unless node = term.as_d?

    n = path.first
    _, child = nth(node, n, conf)
    if row = advance?(child, path.rest, conf)
      # Child was able to advance.
      subpath, value = row
      return subpath.prepend(n), value
    end

    # Try to advance ourselves.
    if entry = nth?(node, n + 1, conf)
      _, value = entry
      return Pf::UPath32[n + 1], value
    end

    # We failed to advance. Maybe the parent can.
  end

  private def successor?(root : Term, path : Pf::UPath32, depth : UInt32, conf : Conf) : {Pf::UPath32, Term}?
    return unless row = advance?(root, path, conf)

    prefix, value = row
    if prefix.size == depth
      return prefix, value
    end

    assert prefix.size < depth

    if row = path0?(value, len: depth - prefix.size, conf: conf)
      subpath, leaf = row
      subpath.each { |n| prefix = prefix.append(n) }
      return prefix, leaf
    end

    successor?(root, prefix, depth, conf)
  end

  # :nodoc:
  #
  # Zero for BFS.
  def step?(state : BreadthFirstInitial) : {BreadthFirst, Pf::UPath32, Term}?
    {BreadthFirst.new(state.root, Pf::UPath32[], state.conf), Pf::UPath32[], state.root}
  end

  # :nodoc:
  #
  # Successor function for BFS.
  def step?(state : BreadthFirst) : {BreadthFirst, Pf::UPath32, Term}?
    # Try to find a successor at the current path depth, trying the next
    # depth if we fail.
    return unless row = successor?(state.root, state.path, state.path.size, state.conf) ||
                        path0?(state.root, state.path.size + 1, state.conf)

    path, value = row

    {state.copy_with(path: path), path, value}
  end

  # :nodoc:
  def step?(state : Scan) : {Scan, Pf::UPath32, Term}?
    return unless dict = state.root.as_d?
    return unless entry = nth?(dict, state.n, state.conf)

    _, value = entry

    {Scan.new(state.root, state.n + 1, state.conf), Pf::UPath32[state.n], value}
  end

  {% if flag?(:docs) %}
    # Executes one step of traversal for *state* ("increments" *state*). Returns
    # the resulting copy of *state*, followed by the current path, followed by
    # the value at that path. Returns `nil` at the end of traversal (when no more
    # path-value pairs can be emitted).
    #
    # ```
    # state = Tour.scan(Term.of(:+, 1, 2, 3))
    #
    # loop do
    #   break unless row = Tour.step?(state)
    #
    #   state, path, value = row
    #   pp value
    #   # => +
    #   # => 1
    #   # => 2
    #   # => 3
    # end
    # ```
    def step?(state : Any) : {Any, Pf::UPath32, Term}?
    end
  {% end %}
end
