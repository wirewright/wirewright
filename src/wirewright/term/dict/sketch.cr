class Ww::Term::Dict
  # Represents a dictionary sketch. There are three main dict sketches in
  # Wirewright: *symbol*, *key*, and *value* sketch.
  #
  # - *Symbol* sketches store symbols.
  # - *Key* sketches store symbol, string, boolean, and blob keys.
  # - *Value* sketches store string, number, boolean, and blob terms.
  #
  # All sketches are recursive, in that they are union-d with child dicts.
  # For example, my key sketch includes my keys as well as the keys of my
  # children (entry value dicts). This lets us propagate info about terms
  # and keys present in a dictionary up the tree, with more or less loss
  # depending on the size of dicts that we're passing. Sketches allow
  # patterns to reject most dicts early based on their "deep content".
  struct Sketch
    # :nodoc:
    alias Repr = UInt64

    # :nodoc:
    getter bits : Repr

    # :nodoc:
    def initialize(@bits : Repr)
    end

    # Constructs an empty sketch.
    def self.empty : Sketch
      new(Repr.new(0))
    end

    # :nodoc:
    def self.one(hashcode : UInt64) : Sketch
      new(Repr.new(1) << (hashcode % Repr.bit_width))
    end

    # Constructs a *key sketch* given a key *term* and its *hashcode*.
    def self.key(term : Term, hashcode : UInt64) : Sketch
      key(Term[term], hashcode)
    end

    # :ditto:
    def self.key(term : Term::Any, hashcode : UInt64) : Sketch
      case Term[term]
      in Term::Sym, Term::Str, Term::Boolean, Term::Blob
        one(hashcode)
      in Term::Num, Term::Dict
        empty
      end
    end

    # Constructs a *value sketch* given a *term* and its *hashcode*.
    def self.value(term : Term, hashcode : UInt64) : Sketch
      value(Term[term], hashcode)
    end

    # :ditto:
    def self.value(term : Term::Any, hashcode : UInt64) : Sketch
      case term
      in Term::Num
        # Approximate numbers are not included in the value sketch.
        if term.approx?
          return empty
        end

        one(hashcode)
      in Term::Str, Term::Boolean, Term::Blob
        one(hashcode)
      in Term::Sym
        empty
      in Term::Dict
        term.summary.value_sketch
      end
    end

    # Constructs a *symbol sketch* given a *term* and its *hashcode*.
    def self.symbol(term : Term, hashcode : UInt64) : Sketch
      symbol(Term[term], hashcode)
    end

    # :nodoc:
    SYM_I = Term[:I]

    # :ditto:
    def self.symbol(term : Term::Any, hashcode : UInt64) : Sketch
      case term
      in Term::Sym
        # We hard-code a slot for the symbol `I` (maybe we'll have other symbols
        # here in the future).
        #
        # Wirewright as a whole is completely oblivious to this (as it should be!).
        # However, this matters enormously for the performance of editR and MuSoma.
        #
        # The main reason is editR. Even though editR almost always relies on `I`,
        # which is quite rare a symbol, in very large terms, `I` is almost guaranteed
        # to collide with other symbols; therefore, editR is forced to take a number of
        # slow paths before it descends deep enough that collisions disappear, of leaves
        # are hit. Notice how this is exactly what we *don't* want editR to do: editR is
        # very, very expensive, and simultaneously, `I`s are very, very rare and often
        # singular. Most terms (including huge terms) do not contain `I` in practice.
        #
        # It is thus beneficial to hard-code a slot for `I`, so that editR isn't forced
        # to search for `I` in very large terms, but instead gets a precise answer every
        # time. Therefore, editR can basically "follow the trail" to `I`s instead of
        # visiting very large subterms.
        if term == SYM_I
          new(Repr.new(1) << 63)
        else
          new(Repr.new(1) << (hashcode % (Repr.bit_width - 1)))
        end
      in Term::Num, Term::Str, Term::Boolean, Term::Blob
        empty
      in Term::Dict
        term.summary.symbol_sketch
      end
    end

    # Returns the union of two sketches *a* and *b*.
    def self.union(a : Sketch, b : Sketch) : Sketch
      new(a.bits | b.bits)
    end

    # Returns `true` if this sketch contains no elements.
    def empty? : Bool
      self == Sketch.empty
    end

    # Shorthand for `!empty?`.
    def present? : Bool
      !empty?
    end

    # Returns `true` if all elements of this sketch are contained in *other*.
    def subset_of?(other : Sketch) : Bool
      (other.bits & bits) == bits
    end
  end
end
