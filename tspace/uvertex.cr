module Ww
  # Represents a vertex served by unit trie `Utrie`.
  struct Uvertex
    include Comparable(Uvertex)

    # Reserved for `nil`-like Uvertices.
    NONE = Uvertex.new(0)

    # The first free Uvertex.
    ZERO = NONE + 1

    def initialize(@id : UInt32)
    end

    def <=>(other : Uvertex)
      @id <=> other.@id
    end

    # Shifts the underlying id integer by *offset*.
    def +(offset : UInt32) : Uvertex
      Uvertex.new(@id + offset)
    end
  end
end
