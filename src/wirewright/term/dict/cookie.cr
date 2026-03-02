class Ww::Term::Dict
  # A *cookie* is remotely analogous to cookies in HTTP. Assoc, dissoc, and
  # others leave their cookie on the nodes they create or copy. If they later
  # encounter a node with the same cookie as their own, they mutate the node
  # instead of copying it. Similarly for object buffers themselves (be it
  # ti erm buffer or a node buffer): if the enclosing node is recognized by
  # the function call, instead of being copied, it is modified in-place.
  #
  # Since both `UTermTrie32` and `TermTrie` are intended as immutable, thread-
  # safe data structures, cookies must be used in a very deliberate manner.
  # Something like `Commit` exists to manage cookies for you. A lot depends
  # on the uniqueness and ephemerality of a cookie.
  #
  # The basic, safe principle is this. We have a globally unique id source
  # for cookies. Locally (that is, in a function call), we ask the id source
  # to generate an id for us. Now, importantly, the cookie generated this way
  # must not outlive the function call. When using cookies, you must also guarantee
  # that the trie will not be published or committed before the cookie retires.
  # Again, I'm saying all this because I can; in practice, just use `Commit` and
  # you'll never have to worry about this. Cookies are only useful in circumstances
  # where `Commit` is inconvenient (e.g. nested commits, or recursive *reentrant*
  # mutation, etc).
  struct Cookie
    # :nodoc:
    NONE = 0u64

    # :nodoc:
    ZERO = 1u64

    private def initialize(@seq : UInt64)
    end

    @@source : Atomic(UInt64) = Atomic(UInt64).new(ZERO)

    def self.new
      new(@@source.add(1, :relaxed))
    end

    def self.none
      new(NONE)
    end

    def allows_mutation_by?(mutator : Cookie) : Bool
      self == mutator && @seq != NONE
    end
  end
end
