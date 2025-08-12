module Ww::Soma::Microfold
  # Responds to queries about a node's location in the tree.
  struct Locus
    @parent : Term

    def initialize(@root : Term, @keypath : Stack(Term))
      if @keypath.empty?
        raise ArgumentError.new
      end

      key = @keypath.pop
      begin
        @parent = @root.follow(@keypath)
      ensure
        @keypath << key
      end
    end

    private def key : Term
      @keypath.last
    end

    # Returns `true` if this node is the only child of its parent. Returns
    # `false` otherwise.
    def only_child? : Bool
      @parent.itemsize == 2 && key == Term.of(1)
    end

    # Returns `true` if this node is the first child of its parent. Returns
    # `false` otherwise.
    def first_child? : Bool
      key == Term.of(1)
    end

    # Returns `true` if this node is the last child of its parent. Returns
    # `false` otherwise.
    def last_child? : Bool
      key == Term.of(@parent.itemsize - 1)
    end

    # Returns `true` if this node’s position is within the first *n* children
    # of its parent. Returns `false` otherwise.
    def first_child?(n : Term::Num) : Bool
      return false unless index = key.as_n?

      index.in?(Term[1]...Term[1] + n)
    end

    # Returns `true` if this node’s position is within the last *n* children
    # of its parent. Returns `false` otherwise.
    def last_child?(n : Term::Num) : Bool
      return false unless index = key.as_n?

      index.in?(Term[@parent.itemsize] - n...@parent.itemsize)
    end

    # Returns `true` if this node is the first in its periodic group of size *p*.
    # Returns `false` otherwise.
    def child_of_period?(p : Term::Num) : Bool
      return false unless index = key.as_n?

      if p.zero?
        # E.g. `(period-0):bg-neutral-500` is the same as not writing anything at all.
        return false
      end

      # Subtract one so that the node's tag doesn't count toward period.
      (index - Term[1]) % p == Term[0]
    end
  end
end
