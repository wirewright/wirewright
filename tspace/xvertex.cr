module Ww
  # Reference to an Xgraph vertex served by `Xgraph`.
  struct Xvertex
    include Comparable(Xvertex)

    # Reserved for `nil`-like Xvertices.
    NONE = Xvertex.new(0)

    # The first free Xvertex.
    ZERO = NONE + 1

    def initialize(@id : UInt32)
    end

    def <=>(other : Xvertex)
      @id <=> other.@id
    end

    # Shifts the underlying id integer by *offset*.
    def +(offset : UInt32) : Xvertex
      Xvertex.new(@id + offset)
    end
  end
end
