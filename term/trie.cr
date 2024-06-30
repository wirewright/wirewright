module Ww
  # A trie that stores terms and allows efficient retrieval of terms by `Unit::Query`.
  class Term::Trie
    private alias Endpoint = Set(Term)

    private on_demand numbers : Array({Term::Num, Endpoint})
    private on_demand strings : Hash(Term::Str, Endpoint)
    private on_demand symbols : Hash(Term::Sym, Endpoint)
    private on_demand booleans : Hash(Term::Boolean, Endpoint)
    private on_demand entries : Hash(Term, Term::Trie)

    # :nodoc:
    def initialize(
      @numbers = nil,
      @strings = nil,
      @symbols = nil,
      @booleans = nil,
      @entries = nil,
      @empty_dicts = 0
    )
    end

    # Returns `true` if this trie contains no terms.
    def empty? : Bool
      !(has_numbers? || has_strings? || has_symbols? || has_booleans? || has_entries? || @empty_dicts > 0)
    end

    # Calls *fn* with each endpoint of this trie.
    def each_endpoint(&fn : Endpoint ->) : Nil
      numbers.each { |_, ep| fn.call(ep) } if has_numbers?
      strings.each { |_, ep| fn.call(ep) } if has_strings?
      symbols.each { |_, ep| fn.call(ep) } if has_symbols?
      booleans.each { |_, ep| fn.call(ep) } if has_booleans?
      entries.each { |k, v| v.each_endpoint(&fn) } if has_entries?

      fn.call(Endpoint{Term[].upcast}) if @empty_dicts > 0
    end

    def each_term(&fn : Term ->) : Nil
      each_endpoint &.each(&fn)
    end

    # Adds a number *term* to this trie.
    protected def add(term : Term::Num, root : Term) : Nil
      unless index = numbers.bsearch_index { |(other)| term <= other }
        numbers << {term, Endpoint{root}}
        return
      end

      other, ep = numbers[index]
      unless other == term
        numbers.insert(index, {term, Endpoint{root}})
        return
      end

      ep << root
    end

    # Adds a string *term* to this trie that points to *root*.
    protected def add(term : Term::Str, root : Term) : Nil
      ep = strings.put_if_absent(term) { Endpoint.new }
      ep << root
    end

    # Adds a symbol *term* to this trie that points to *root*.
    protected def add(term : Term::Sym, root : Term) : Nil
      ep = symbols.put_if_absent(term) { Endpoint.new }
      ep << root
    end

    # Adds a boolean *term* to this trie that points to *root*.
    protected def add(term : Term::Boolean, root : Term) : Nil
      ep = booleans.put_if_absent(term) { Endpoint.new }
      ep << root
    end

    # Adds a dictionary *term* to this trie that points to *root*.
    protected def add(term : Term::Dict, root : Term) : Nil
      if term.empty?
        @empty_dicts += 1
        return
      end

      term.each_entry do |k, v|
        trie = entries.put_if_absent(k) { Term::Trie.new }
        trie.add(v.downcast, root)
      end
    end

    # Removes a number *term* from this trie.
    protected def delete(term : Term::Num, root : Term) : Nil
      return unless has_numbers?
      return unless index = numbers.bsearch_index { |(other)| term <= other }

      other, ep = numbers[index]
      return unless term == other

      ep.delete(root)
      return unless ep.empty?

      numbers.delete_at(index)
    end

    # Removes a string *term* from this trie.
    protected def delete(term : Term::Str, root : Term) : Nil
      return unless has_strings?
      return unless ep = strings[term]?

      ep.delete(root)
      return unless ep.empty?

      strings.delete(term)
    end

    # Removes a symbol *term* from this trie.
    protected def delete(term : Term::Sym, root : Term) : Nil
      return unless has_symbols?
      return unless ep = symbols[term]?

      ep.delete(root)
      return unless ep.empty?

      symbols.delete(term)
    end

    # Removes a boolean *term* from this trie.
    protected def delete(term : Term::Boolean, root : Term) : Nil
      return unless has_booleans?
      return unless ep = booleans[term]?

      ep.delete(root)
      return unless ep.empty?

      booleans.delete(term)
    end

    # Removes a dictionary *term* from this trie.
    protected def delete(term : Term::Dict, root : Term) : Nil
      if term.empty? && @empty_dicts > 0
        @empty_dicts -= 1
        return
      end

      return unless has_entries?

      term.each_entry do |k, v|
        next unless trie = entries[k]?
        trie.delete(v.downcast, root)
        next unless trie.empty?
        entries.delete(k)
      end
    end

    # Adds *term* to this trie.
    #
    # ```
    # trie = Term::Trie.new
    # trie.add(Term[100])
    # trie.add(Term["hello world"])
    # trie.add(Term[x: 100, y: Term[1, 2, 3], ok: true])
    # ```
    def add(term : Term) : self
      add(term.downcast, root: term)

      self
    end

    # Removes *term* from this trie if it was there.
    #
    # ```
    # trie = Term::Trie.new
    # trie.add(Term[100])
    # trie.add(Term[200])
    # trie.add(Term[300])
    # trie.delete(Term[200])
    # trie # => Term::Trie with 100, 200
    # ```
    def delete(term : Term) : self
      delete(term.downcast, root: term)

      self
    end

    # :nodoc:
    def select(cls : Term::Num.class) : Term::Trie
      Trie.new(numbers: @numbers)
    end

    # :nodoc:
    def select(cls : Term::Str.class) : Term::Trie
      Trie.new(strings: @strings)
    end

    # :nodoc:
    def select(cls : Term::Sym.class) : Term::Trie
      Trie.new(symbols: @symbols)
    end

    # :nodoc:
    def select(cls : Term::Boolean.class) : Term::Trie
      Trie.new(booleans: @booleans)
    end

    # :nodoc:
    def select(cls : Term::Dict.class) : Term::Trie
      Trie.new(entries: @entries, empty_dicts: @empty_dicts)
    end

    # Returns a trie comprised of terms of the given class *cls*.
    def select(cls : Term.class)
      self
    end

    # :nodoc:
    def select(needle : Term::Num) : Term::Trie
      return Trie.new unless has_numbers?
      return Trie.new unless candidate = numbers.bsearch { |(other)| needle <= other }

      number, ep = candidate
      return Trie.new unless number == needle

      Trie.new(numbers: [candidate])
    end

    # :nodoc:
    def select(needle : Term::Str) : Term::Trie
      return Trie.new unless has_strings?
      return Trie.new unless ep = strings[needle]?

      Trie.new(strings: {needle => ep})
    end

    # :nodoc:
    def select(needle : Term::Sym) : Term::Trie
      return Trie.new unless has_symbols?
      return Trie.new unless ep = symbols[needle]?

      Trie.new(symbols: {needle => ep})
    end

    # :nodoc:
    def select(needle : Term::Boolean) : Term::Trie
      return Trie.new unless has_booleans?
      return Trie.new unless ep = booleans[needle]?

      Trie.new(booleans: {needle => ep})
    end

    # :nodoc:
    def select(needle : Term::Dict) : Term::Trie
      raise ArgumentError.new
    end

    # Returns a trie containing *needle* if *needle* is present in this trie.
    # Otherwise, the returned trie is empty.
    def select(needle : Term) : Term::Trie
      self.select(needle.downcast)
    end

    # Returns a trie containing values of the given *key*, assuming there
    # are some. Otherwise, returns an empty trie.
    def where(key : Term) : Term::Trie
      return Trie.new unless has_entries?
      return Trie.new unless trie = entries[key]?

      trie
    end
  end

  class Term::Trie
    # :nodoc:
    def select(base : Sparse::Unit::IsNum) : Term::Trie
      self.select(Term::Num)
    end

    # :nodoc:
    def select(base : Sparse::Unit::IsStr) : Term::Trie
      self.select(Term::Str)
    end

    # :nodoc:
    def select(base : Sparse::Unit::IsSym) : Term::Trie
      self.select(Term::Sym)
    end

    # :nodoc:
    def select(base : Sparse::Unit::IsBoolean) : Term::Trie
      self.select(Term::Boolean)
    end

    # :nodoc:
    def select(base : Sparse::Unit::IsDict) : Term::Trie
      self.select(Term::Dict)
    end

    # :nodoc:
    def select(base : Sparse::Unit::IsAny) : Term::Trie
      self.select(Term)
    end

    # :nodoc:
    def select(base : Sparse::Unit::Match) : Term::Trie
      self.select(base.term)
    end

    # :nodoc:
    def select(base : Sparse::Unit::Tf) : Term::Trie
      trie = Trie.new

      each_term do |term|
        next unless value = base.pass?(term)
        trie.add(value.downcast, root: term)
      end

      trie
    end

    # :nodoc:
    def select(base : Sparse::Unit::DivBy) : Term::Trie
      return Trie.new unless has_numbers?

      Trie.new(numbers: numbers.select { |n, _| n.whole? && n.divisible_by?(base.n) })
    end

    # :nodoc:
    def select(base : Sparse::Unit::InRange) : Term::Trie
      return Trie.new unless has_numbers?

      if b = base.b
        if base.bx
          from = numbers.bsearch_index { |(n)| n > b }
        else
          from = numbers.bsearch_index { |(n)| n >= b }
        end
      end

      return Trie.new if b && from.nil?

      if e = base.e
        if base.ex
          to = numbers.bsearch_index { |(n)| n > e }
        else
          to = numbers.bsearch_index { |(n)| n >= e }
        end
      end

      selection = [] of {Term::Num, Endpoint}

      numbers.each(within: from..to) do |n, ep|
        break if e && (base.ex ? n >= e : n > e)
        selection << {n, ep}
      end

      Trie.new(numbers: selection)
    end

    # :nodoc:
    def select(base : Sparse::Unit::WhereKey) : Term::Trie
      where(base.key)
    end

    # Returns a trie containing terms that satisfy *base*. Some bases
    # (such as `Sparse::Unit::WhereKey`) may navigate deeper into the trie.
    def select(base : Sparse::Unit::Base) : Term::Trie
      raise NotImplementedError.new(base)
    end
  end

  class Term::Trie
    # :nodoc:
    def estimate(base : Sparse::Unit::IsNum, &fn : Term::Trie -> Float64) : Float64
      fn.call(self.select(base))
    end

    # :nodoc:
    def estimate(base : Sparse::Unit::IsStr, &fn : Term::Trie -> Float64) : Float64
      fn.call(self.select(base))
    end

    # :nodoc:
    def estimate(base : Sparse::Unit::IsSym, &fn : Term::Trie -> Float64) : Float64
      fn.call(self.select(base))
    end

    # :nodoc:
    def estimate(base : Sparse::Unit::IsDict, &fn : Term::Trie -> Float64) : Float64
      fn.call(self.select(base))
    end

    # :nodoc:
    def estimate(base : Sparse::Unit::IsBoolean, &fn : Term::Trie -> Float64) : Float64
      fn.call(self.select(base))
    end

    # :nodoc:
    def estimate(base : Sparse::Unit::IsAny, &fn : Term::Trie -> Float64) : Float64
      fn.call(self.select(base))
    end

    # :nodoc:
    def estimate(base : Sparse::Unit::Match, &fn : Term::Trie -> Float64) : Float64
      fn.call(self.select(base))
    end

    # :nodoc:
    def estimate(unit : Sparse::Unit::WhereKey, &fn : Term::Trie -> Float64) : Float64
      return 0.0 unless has_entries?
      return 0.0 unless trie = entries[unit.key]?

      fn.call(trie)
    end

    # :nodoc:
    def estimate(base : Sparse::Unit::Base, &fn : Term::Trie -> Float64) : Float64
      base.probability * estimate(base.tout.check, &fn)
    end

    # :nodoc:
    def estimate(bases : MutView(Sparse::Unit::Base)) : Float64
      if bases.empty?
        estimate = 0.0
        each_endpoint { |ep| estimate += ep.size }
        return estimate
      end

      estimate(bases.first) { |trie| trie.estimate(bases.rest) }
    end
  end

  class Term::Trie
    # Returns `true` if *term* satisfies (is matched by) the given strand
    # view *view*.
    def self.pass?(view : MutView(Sparse::Unit::Base), term : Term) : Bool
      !!view.reduce(term) { |subj, base| base.pass?(subj) || return false }
    end

    # Returns `true` if *term* satisfies (is matched by) *every* strand view
    # in *views*.
    def self.pass?(views : MutView(MutView(Sparse::Unit::Base)), term : Term) : Bool
      views.all? { |branch| pass?(branch, term) }
    end

    private def match!(strand : MutView(Sparse::Unit::Base))
      selection = DeepSet(Term).new

      trie = strand.reduce(self) { |trie, base| trie.select(base) }
      trie.each_endpoint { |ep| selection.concat(ep) }

      selection
    end

    private def match!(strands : MutView(MutView(Sparse::Unit::Base)))
      # Sort branches by the estimate of how many results we think a branch
      # will have. This allows us to get the branch with the least amount of
      # results easily, and also allows us to reject quickly during the main
      # pass (since earlier branches have less "probability" of a match).
      strands.unstable_sort_by! { |branch| estimate(branch) }

      pivot, criteria = strands.first, strands.rest

      matches = Set(Term).new

      selection = match!(pivot)
      selection.each do |term|
        next if term.in?(matches)
        next unless criteria.all? { |branch| Term::Trie.pass?(branch, term) }
        matches << term
      end

      matches
    end

    # Returns the set of terms within this trie that match the given *unit*.
    def eval(unit : Sparse::Unit::Strand) : Set(Term)
      match!(unit.view).to_set
    end

    # :ditto:
    def eval(unit : Sparse::Unit::Xsect) : Set(Term)
      match!(unit.map { |branch| branch.view }.view)
    end

    # :ditto:
    def eval(unit : Sparse::Unit::Query) : Set(Term)
      positive = eval(unit.pattern)
      negatives = [] of MutView(Sparse::Unit::Base) | MutView(MutView(Sparse::Unit::Base))

      # We could do further eval()s on counterpatterns but it probably
      # won't be worth much most of the times, so do dumb `reject!` pass
      # over the positive matches.
      unit.each_counterpattern do |cp|
        case cp
        when Sparse::Unit::Strand
          strand = cp.view
          negatives << strand
        when Sparse::Unit::Xsect
          strands = cp.map { |branch| branch.view }.view
          strands.unstable_sort_by! { |branch| estimate(branch) }
          negatives << strands
        end
      end

      positive.reject! do |term|
        negatives.any? { |negative| Term::Trie.pass?(negative, term) }
      end
    end
  end
end
