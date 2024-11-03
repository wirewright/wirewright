module Ww
  alias Holder = Appearance::Id

  # Term trie is similar to `Utrie`, but holds terms rather than strings of `Ubase`s.
  class Ttrie
    alias Holders = Pf::Set(Holder)

    # :nodoc:
    abstract struct NumberEntry
      include Comparable(NumberEntry)

      abstract def number : Term::Num

      def <=>(other : NumberEntry)
        number <=> other.number
      end
    end

    # :nodoc:
    record NumberEntry::Stored < NumberEntry, number : Term::Num, holders : Holders do
      def inspect(io)
        io << number << " => " << holders
      end
    end

    # :nodoc:
    record NumberEntry::Probe < NumberEntry, number : Term::Num

    # :nodoc:
    NO_NUMBERS = RBTree(NumberEntry)[]

    # :nodoc:
    NO_STRINGS = Pf::Map(Term::Str, Holders).new

    # :nodoc:
    NO_SYMBOLS = Pf::Map(Term::Sym, Holders).new

    # :nodoc:
    NO_HOLDERS = Holders.new

    # :nodoc:
    NO_KEYS = Pf::Map(Term, Ttrie).new

    # :nodoc:
    EMPTY = new(
      numbers: NO_NUMBERS,
      numbersw: 0.0f32,
      strings: NO_STRINGS,
      stringsw: 0.0f32,
      symbols: NO_SYMBOLS,
      symbolsw: 0.0f32,
      true_: NO_HOLDERS,
      false_: NO_HOLDERS,
      voids: NO_HOLDERS,
      keys: NO_KEYS,
      keysw: 0.0f32,
    )

    protected def initialize(
      @numbers : RBTree(NumberEntry),
      @numbersw : Float32,
      @strings : Pf::Map(Term::Str, Holders),
      @stringsw : Float32,
      @symbols : Pf::Map(Term::Sym, Holders),
      @symbolsw : Float32,
      @true_ : Holders,
      @false_ : Holders,
      @voids : Holders,
      @keys : Pf::Map(Term, Ttrie),
      @keysw : Float32,
    )
    end

    # Constructs an empty term trie.
    def self.[] : Ttrie
      EMPTY
    end

    # :nodoc:
    def self.[](**kwargs) : Ttrie
      Ttrie[].change(**kwargs)
    end

    protected def_change

    # Returns `true` if this trie does not contain any terms. Returns `false` if
    # it contains some.
    def empty? : Bool
      weight.zero?
    end

    # Returns the *weight* of this trie.
    #
    # A trie's weight is the sum of sizes of holder sets in it and weights of all
    # sub-tries recursively.
    #
    # Weights mainly exist so we are able to estimate the maximum potential "yield"
    # of an operation to check the lightest (= cheapest) subtries for conjunction;
    # since conjunction is intersection this smallest subtrie will act as a filter
    # for all consecutive matches (and we'll only narrow it down; never grow), thus
    # saving memory.
    #
    # Weights can also help to determine the cheapest trie to do a full scan of
    # from a collection of tries.
    def weight : Float32
      {@numbersw, @stringsw, @symbolsw, @keysw, @true_.size, @false_.size, @voids.size}.sum(0.0f32)
    end

    # Yields holders present in this trie. Recurses to subtries. Will yield duplicate
    # holders present in multiple strands.
    def each_holder(&fn : Holder ->) : Nil
      @numbers.each do |entry|
        entry = entry.as(NumberEntry::Stored)
        entry.holders.each(&fn)
      end

      @strings.each_value(&.each(&fn))
      @symbols.each_value(&.each(&fn))

      @true_.each(&fn)
      @false_.each(&fn)
      @voids.each(&fn)

      @keys.each { |_, trie| trie.each_holder(&fn) }
    end

    protected def at(key : Term, trie0 : Ttrie, trie1 : Ttrie) : Ttrie
      return self if trie0.same?(trie1) # Unchanged

      if trie1.empty? # Deleted
        return change(keys: @keys.dissoc(key), keysw: @keysw - trie0.weight)
      end

      change(keys: @keys.assoc(key, trie1), keysw: @keysw + (trie1.weight - trie0.weight)) # Updated
    end

    # Follows through keys in *prefix* to the target trie node; returns whether
    # the target trie node exists, that target trie node (an empty trie node if
    # the target trie node does not exist), and a setter function that can be
    # used to change the target trie node in context.
    #
    # The setter function accepts a modified version of the target trie node, and
    # returns a modified version of `self` with that modifed target trie node
    # installed in the appropriate place.
    def at(prefix : BiList(Term)) : {Bool, Ttrie, (Ttrie -> Ttrie)}
      unless key = prefix.first?
        return true, self, ->(trie1 : Ttrie) { trie1 }
      end

      # Link exists between self and a neighbor trie through current key.
      if trie0 = @keys[key]?
        exists, target, nset = trie0.at(prefix.rest)
        set = ->(trie1 : Ttrie) { at(key, trie0, trie1: nset.call(trie1)) }

        return exists, target, set
      end

      # Link between self and a neighbor trie is missing. Allocate the rest of
      # the path on call to *set*.
      set = ->(trie1 : Ttrie) do
        return self if trie1.empty?

        prefix.reverse_each do |key, leftmost|
          if leftmost
            trie1 = at(key, trie0: Ttrie[], trie1: trie1)
          else
            trie1 = Ttrie[].at(key, trie0: Ttrie[], trie1: trie1)
          end
        end

        trie1
      end

      {false, Ttrie[], set}
    end

    private def assoc(v : Term::Num, holder) : Ttrie
      holders0 = NO_HOLDERS

      @numbers.query(NumberEntry::Probe.new(v)) do |entry|
        entry = entry.as(NumberEntry::Stored)
        holders0 = entry.holders
      end

      holders1 = holders0.add(holder)
      return self if holders0.same?(holders1)

      entry = NumberEntry::Stored.new(v, holders1)

      change(numbers: @numbers.with(entry), numbersw: @numbersw + 1)
    end

    private def assoc(v : Term::Str, holder) : Ttrie
      holders0 = @strings[v]? || NO_HOLDERS
      holders1 = holders0.add(holder)
      return self if holders0.same?(holders1)

      change(strings: @strings.assoc(v, holders1), stringsw: @stringsw + 1)
    end

    private def assoc(v : Term::Sym, holder) : Ttrie
      holders0 = @symbols[v]? || NO_HOLDERS
      holders1 = holders0.add(holder)
      return self if holders0.same?(holders1)

      change(symbols: @symbols.assoc(v, holders1), symbolsw: @symbolsw + 1)
    end

    private def assoc(v : Term::Boolean, holder) : Ttrie
      if v.true?
        holders1 = @true_.add(holder)
        holders1.same?(@true_) ? self : change(true_: holders1) # weight is computed from size
      else
        holders1 = @false_.add(holder)
        holders1.same?(@false_) ? self : change(false_: holders1) # weight is computed from size
      end
    end

    private def assoc(v : Term::Dict, holder) : Ttrie
      unless v.empty?
        raise ArgumentError.new
      end

      holders1 = @voids.add(holder)
      holders1.same?(@voids) ? self : change(voids: holders1) # weight is computed from size
    end

    # Returns a copy of `self` with *holder* installed as a holder of *v*.
    def assoc(v : Term, holder : Holder) : Ttrie
      assoc(v.downcast, holder)
    end

    private def dissoc(v : Term::Num, holder) : Ttrie
      @numbers.query(NumberEntry::Probe.new(v)) do |entry|
        entry = entry.as(NumberEntry::Stored)

        holders0 = entry.holders
        holders1 = holders0.delete(holder)
        return self if holders0.same?(holders1)

        # Last holder removed, remove the number itself from this trie node.
        if holders1.empty?
          return change(numbers: @numbers.without(NumberEntry::Probe.new(v)), numbersw: @numbersw - 1)
        end

        entry = NumberEntry::Stored.new(v, holders1)

        return change(numbers: @numbers.with(entry), numbersw: @numbersw - 1)
      end

      self
    end

    private def dissoc(v : Term::Str, holder) : Ttrie
      return self unless holders0 = @strings[v]?

      holders1 = holders0.delete(holder)
      return self if holders0.same?(holders1)

      # Last holder removed, remove the string itself from this trie node.
      if holders1.empty?
        return change(strings: @strings.dissoc(v), stringsw: @stringsw - 1)
      end

      change(strings: @strings.assoc(v, holders1), stringsw: @stringsw - 1)
    end

    private def dissoc(v : Term::Sym, holder) : Ttrie
      return self unless holders0 = @symbols[v]?

      holders1 = holders0.delete(holder)
      return self if holders0.same?(holders1)

      # Last holder removed, remove the symbol itself from this trie node.
      if holders1.empty?
        return change(symbols: @symbols.dissoc(v), symbolsw: @symbolsw - 1)
      end

      change(symbols: @symbols.assoc(v, holders1), symbolsw: @symbolsw - 1)
    end

    private def dissoc(v : Term::Boolean, holder) : Ttrie
      if v.true?
        holders1 = @true_.delete(holder)
        holders1.same?(@true_) ? self : change(true_: holders1) # weight is computed from size
      else
        holders1 = @false_.delete(holder)
        holders1.same?(@false_) ? self : change(false_: holders1) # weight is computed from size
      end
    end

    private def dissoc(v : Term::Dict, holder) : Ttrie
      unless v.empty?
        raise ArgumentError.new
      end

      holders1 = @voids.delete(holder)
      holders1.same?(@voids) ? self : change(voids: holders1) # weight is computed from size
    end

    # Returns a copy of `self` with *holder*'s hold of *v* uninstalled.
    def dissoc(v : Term, holder : Holder) : Ttrie
      dissoc(v.downcast, holder)
    end

    private def holds?(v : Term::Num, holder : Holder) : Bool
      @numbers.query(NumberEntry::Probe.new(v)) do |entry|
        entry = entry.as(NumberEntry::Stored)
        return holder.in?(entry.holders)
      end

      false
    end

    private def holds?(v : Term::Str, holder : Holder) : Bool
      return false unless holders = @strings[v]?

      holder.in?(holders)
    end

    private def holds?(v : Term::Sym, holder : Holder) : Bool
      return false unless holders = @symbols[v]?

      holder.in?(holders)
    end

    private def holds?(v : Term::Boolean, holder : Holder) : Bool
      holder.in?(v.true? ? @true_ : @false_)
    end

    private def holds?(v : Term::Dict, holder : Holder) : Bool
      unless v.empty?
        raise ArgumentError.new
      end

      holder.in?(@voids)
    end

    # Returns `true` if *holder* has a hold of *v* in this trie node. Returns
    # `false` otherwise.
    def holds?(v : Term, holder : Holder) : Bool
      holds?(v.downcast, holder)
    end

    # Follows through *prefix* keys, installs *holder*'s hold of *v* at the trie node
    # thus reached, starting from `self`. Returns a modified version of `self`.
    def mount(prefix : BiList(Term), v : Term, holder : Holder) : Ttrie
      _, dst0, set = at(prefix)

      dst1 = dst0.assoc(v, holder)
      dst1.same?(dst0) ? self : set.call(dst1)
    end

    # Follows through *prefix* keys, uninstalls *holder*'s hold of *v* at the trie node
    # thus reached, starting from `self`. Returns a modified version of `self`.
    def unmount(prefix : BiList(Term), v : Term, holder : Holder) : Ttrie
      exists, dst0, set = at(prefix)
      return self unless exists

      dst1 = dst0.dissoc(v, holder)
      dst1.same?(dst0) ? self : set.call(dst1)
    end

    # Returns a trie that only contains terms of the given term type.
    def select(cls : Term::Num.class) : Ttrie
      Ttrie[numbers: @numbers, numbersw: @numbersw]
    end

    # :ditto:
    def select(cls : Term::Str.class) : Ttrie
      Ttrie[strings: @strings, stringsw: @stringsw]
    end

    # :ditto:
    def select(cls : Term::Sym.class) : Ttrie
      Ttrie[symbols: @symbols, symbolsw: @symbolsw]
    end

    # :ditto:
    def select(cls : Term::Boolean.class) : Ttrie
      Ttrie[true_: @true_, false_: @false_] # weight is computed from size
    end

    # :ditto:
    def select(cls : Term::Dict.class) : Ttrie
      Ttrie[keys: @keys, voids: @voids, keysw: @keysw] # voids weight is computed from size
    end

    # Returns a copy of `self` where only holders of the given number *v* can
    # be found.
    private def select(v : Term::Num) : Ttrie
      @numbers.query(NumberEntry::Probe.new(v)) do |entry|
        entry = entry.as(NumberEntry::Stored)

        return Ttrie[numbers: NO_NUMBERS.with(entry), numbersw: entry.holders.size.to_f32]
      end

      Ttrie[]
    end

    # Returns a copy of `self` where only holders of the given string *v* can
    # be found.
    private def select(v : Term::Str) : Ttrie
      unless holders = @strings[v]?
        return Ttrie[]
      end

      Ttrie[strings: NO_STRINGS.assoc(v, holders), stringsw: holders.size.to_f32]
    end

    # Returns a copy of `self` where only holders of the given symbol *v* can
    # be found.
    private def select(v : Term::Sym) : Ttrie
      unless holders = @symbols[v]?
        return Ttrie[]
      end

      Ttrie[symbols: NO_SYMBOLS.assoc(v, holders), symbolsw: holders.size.to_f32]
    end

    # Returns a copy of `self` where only holders of the given boolean *v*
    # can be found.
    private def select(v : Term::Boolean) : Ttrie
      if v.true?
        Ttrie[true_: @true_] # weight is computed from size
      else
        Ttrie[false_: @false_] # weight is computed from size
      end
    end

    # Returns a copy of `self` where only holders of the empty dictionary *v* can
    # be found. If *v* is not an empty dictionary, raises `ArgumentError`.
    private def select(v : Term::Dict) : Ttrie
      unless v.empty?
        raise ArgumentError.new
      end

      Ttrie[voids: @voids] # weight is computed from size
    end

    # Returns a copy of `self` where only holders of the given *v* (with the
    # corresponding `Tencoder` *vref*) can be found. If *v* is absent returns
    # an empty trie.
    #
    # Raises `ArgumentError` if *v* is a nonempty dictionary.
    def select(v : Term) : Ttrie
      self.select(v.downcast)
    end

    # Returns a copy of `self` containing only values of *key* if *key* is present
    # in this trie. Otherwise, the returned trie is empty.
    def where(key : Term) : Ttrie
      @keys[key]? || Ttrie[]
    end

    # Returns a copy of `self` containing only numbers within the range specified
    # by *b* (beginning of the range) and *e* (end of the range). Beginning, end,
    # or both can be excluded using *bx* and *ex*.
    def between(b : Term::Num, e : Term::Num, *, bx : Bool, ex : Bool) : Ttrie
      return Ttrie[] unless b <= e # Sanity

      selection = NO_NUMBERS
      numbersw = 0.0f32

      @numbers.between(NumberEntry::Probe.new(b), NumberEntry::Probe.new(e), bx, ex) do |entry|
        entry = entry.as(NumberEntry::Stored)
        selection = selection.with(entry) # ?! Really really expensive to start from scratch
        numbersw += entry.holders.size
      end

      Ttrie[numbers: selection, numbersw: numbersw]
    end

    # Returns a trie containing all numbers from `self` less than *n* (and also
    # equal to *n* if *eq* is set to `true`).
    def lt(n : Term::Num, *, eq : Bool) : Ttrie
      selection = NO_NUMBERS
      numbersw = 0.0f32

      @numbers.lt(NumberEntry::Probe.new(n), eq: eq) do |entry|
        entry = entry.as(NumberEntry::Stored)
        selection = selection.with(entry) # ?! Really really expensive to start from scratch
        numbersw += entry.holders.size
      end

      Ttrie[numbers: selection, numbersw: numbersw]
    end

    # Returns a trie containing all numbers from `self` greater than *n* (and also
    # equal to *n* if *eq* is set to `true`).
    def gt(n : Term::Num, *, eq : Bool) : Ttrie
      selection = NO_NUMBERS
      numbersw = 0.0f32

      @numbers.gt(NumberEntry::Probe.new(n), eq: eq) do |entry|
        entry = entry.as(NumberEntry::Stored)
        selection = selection.with(entry) # ?! Really really expensive to start from scratch
        numbersw += entry.holders.size
      end

      Ttrie[numbers: selection, numbersw: numbersw]
    end

    # Calls *fn* with each number-typed term in this trie, followed by the set
    # of its holders.
    def scan(cls : Term::Num.class, &fn : Term::Num, Holders ->) : Nil
      @numbers.each do |entry|
        entry = entry.as(NumberEntry::Stored)

        fn.call(entry.number, entry.holders)
      end
    end

    # Calls *fn* with each string-typed term in this trie, followed by the set
    # of its holders.
    def scan(cls : Term::Str.class, &fn : Term::Str, Holders ->) : Nil
      @strings.each(&fn)
    end

    # Calls *fn* with each symbol-typed term in this trie, followed by the set
    # of its holders.
    def scan(cls : Term::Sym.class, &fn : Term::Sym, Holders ->) : Nil
      @symbols.each(&fn)
    end

    # Calls *fn* with each boolean-typed term in this trie, followed by the set
    # of its holders.
    def scan(cls : Term::Boolean.class, &fn : Term::Boolean, Holders ->) : Nil
      fn.call(Term[true], @true_) unless @true_.empty?
      fn.call(Term[false], @false_) unless @false_.empty?
    end

    # Calls *fn* with each empty dictionary term in this trie, followed by the set
    # of its holders.
    def scan(cls : Term::Dict.class, &fn : Term::Dict, Holders ->) : Nil
      fn.call(Term.of, @voids) unless @voids.empty?
    end

    # Calls *fn* with each number-typed term from this trie, keeps in the returned
    # trie only those for which *fn* returned `true`.
    def select(cls : Term::Num.class, &fn : Term -> Bool) : Ttrie
      selection = NO_NUMBERS
      numbersw = 0.0f32

      @numbers.each do |entry|
        entry = entry.as(NumberEntry::Stored)
        next unless fn.call(entry.number.upcast)

        selection = selection.with(entry)
        numbersw += 1
      end

      Ttrie[numbers: selection, numbersw: numbersw]
    end

    # Calls *fn* with each string-typed term from this trie, keeps in the returned
    # trie only those for which *fn* returned `true`.
    def select(cls : Term::Str.class, &fn : Term -> Bool) : Ttrie
      stringsw = 0.0f32
      selection = @strings.select do |string, holders|
        if ok = fn.call(string.upcast)
          stringsw += holders.size
        end
        ok
      end

      Ttrie[strings: selection, stringsw: stringsw]
    end

    # Calls *fn* with each symbol-typed term from this trie, keeps in the returned
    # trie only those for which *fn* returned `true`.
    def select(cls : Term::Sym.class, &fn : Term -> Bool) : Ttrie
      symbolsw = 0.0f32
      selection = @symbols.select do |symbol, holders|
        if ok = fn.call(symbol.upcast)
          symbolsw += holders.size
        end
        ok
      end

      Ttrie[symbols: selection, symbolsw: symbolsw]
    end

    # Calls *fn* with each boolean-typed term from this trie, keeps in the returned
    # trie only those for which *fn* returned `true`.
    def select(cls : Term::Boolean.class, &fn : Term -> Bool) : Ttrie
      true_ = fn.call(Tencoder::REF_TRUE) ? @true_ : NO_HOLDERS
      false_ = fn.call(Tencoder::REF_FALSE) ? @false_ : NO_HOLDERS

      Ttrie[true_: true_, false_: false_] # weight is computed from size
    end

    # Calls *fn* with each **empty dictionary** term from this trie, keeps in
    # the returned trie only those for which *fn* returned `true`.
    def select(cls : Term::Dict.class, &fn : Term -> Bool) : Ttrie
      voids = fn.call(Tencoder::REF_VOID) ? @voids : NO_HOLDERS

      Ttrie[voids: voids] # weight is computed from size
    end
  end
end
