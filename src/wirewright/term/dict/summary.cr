class Ww::Term::Dict
  # A dictionary summary contains metrics that describe a dictionary and possibly
  # nested dictionaries. Only value dictionaries are considered for the recursive
  # case (and not key dicts).
  #
  # Summaries let us propagate info about the various features of and in a dictionary
  # up. Summaries lose precision the higher they propagate. A summary can be compared
  # to a "smell" that a dictionary has. Patterns use summaries most actively. A pattern
  # first "smells" the dictionary before deciding whether to go in. As this happens
  # recursively, almost on every step on both the dict's and the pattern's ends, we
  # can get significant speed-ups from this even though the precision of metrics is
  # fairly limited.
  #
  # Summaries are updated live as dictionaries change. They are defined recursively,
  # which means dicts can do something called *summary-driven descent*. Depending
  # on the distribution of indexed data, this can give you huge or negligible
  # performance improvements.
  #
  # - *size* tells the number of entries in the dictionary of interest.
  #   It is precise. Overflow is a runtime error which will crash Wirewright.
  # - *maxdepth16* is the saturating depth of the deepest leaf of the dict.
  #   Use `maxdepth` to access in general-purpose code.
  # - *hashcode* is the hashcode. It is *unordered* for both tries. That is,
  #   hashes of entries are combined commutatively. This is in support of the basic
  #   idea behind dicts; in that even though they have all sorts of complexity &
  #   indexing under the hood, on the outside, they're just *sets of entries*.
  # - *key sketch* is a sketch of the keys of this dict and all nested dicts.
  #   For example, if a pattern expects to find the key `x` *somewhere* in a dict,
  #   or in one of its nested dicts, whatever; it will first check the key sketch.
  #   See also: `Sketch.key`.
  # - *value sketch* is similarly a sketch of dict values. See `Sketch.value`.
  # - *symbol sketch* is reserved for symbols exclusively. Since symbols are relatively
  #   sparsely distributed, symbol sketches are one of the most useful optimizations
  #   in Wirewright so far. A lot of patterns can reject early thanks to symbol sketches.
  # - *size set* is a set of dict sizes up to 64. Consider, for instance, the pattern
  #   `⟨(_ _)⟩`. Even though it doesn't provide any useful info in terms of content,
  #   we can still its very shape, which may or may not be rare in the matchee, and
  #   thus, provide shorthands & early rejections.
  # - *histogram* is an approximate (in the sense "precisely N", or "too many to count")
  #   measure of terms of each type in the dict and nested dicts.
  record Summary,
    size : UInt32,
    maxdepth16 : UInt16,
    size_set : Pf::BitSet16,
    hashcode : UInt64,
    key_sketch : Sketch,
    value_sketch : Sketch,
    symbol_sketch : Sketch,
    histogram : Histogram

  struct Summary
    # Converts `maxdepth16`, which is a saturating depth, to `Magnitude`;
    # the saturated state is represented by `Magnitude::INFINITY`. Depths
    # below that are precise.
    def maxdepth : Magnitude
      @maxdepth16 == UInt16::MAX ? Magnitude::INFINITY : Magnitude.new(@maxdepth16)
    end
  end

  struct Summary
    # Returns the "zero" or empty summary, often used as an initial summary.
    def self.zero : Summary
      Summary.new(
        size: 0u32,
        maxdepth16: 0u16,
        size_set: Pf::BitSet16.empty,
        hashcode: 0u64,
        key_sketch: Sketch.empty,
        value_sketch: Sketch.empty,
        symbol_sketch: Sketch.empty,
        histogram: Histogram.zero,
      )
    end

    # Returns the summary of a dict *item*.
    def self.of(item : Term) : Summary
      if dict = item.as_d?
        return dict.summary
      end

      hashcode = Term.hashcode(item)

      Summary.new(
        size: 1u32,
        maxdepth16: 0u16,
        size_set: Pf::BitSet16.empty,
        hashcode: hashcode,
        key_sketch: Sketch.empty,
        value_sketch: Sketch.value(item, hashcode),
        symbol_sketch: Sketch.symbol(item, hashcode),
        histogram: Histogram.of(item),
      )
    end

    # Returns the summary of an entry with the given *key* and *value*.
    #
    # For items, even though they are also thought of as entries elsewhere,
    # you must use `.of(Term)`.
    def self.of(key : {term: Term, hashcode: UInt64}, value : Term) : Summary
      if dict = value.as_d?
        return dict.summary
      end

      value_hashcode = Term.hashcode(value)
      pair_hashcode = Term.hashcode(key[:hashcode], value_hashcode)

      Summary.new(
        size: 1u32,
        maxdepth16: 0u16,
        size_set: Pf::BitSet16.empty,
        hashcode: pair_hashcode,
        key_sketch: Sketch.key(key[:term], key[:hashcode]),
        value_sketch: Sketch.value(value, value_hashcode),
        symbol_sketch: Sketch.symbol(value, value_hashcode),
        histogram: Histogram.of(value),
      )
    end

    # Returns the union of two summaries *a* and *b*.
    def self.union(a : Summary, b : Summary) : Summary
      Summary.new(
        size: a.size + b.size,
        maxdepth16: Math.max(a.maxdepth16, b.maxdepth16),
        size_set: a.size_set | b.size_set,
        # ? Do we need to strengthen this anyhow?
        hashcode: a.hashcode &+ b.hashcode,
        key_sketch: Sketch.union(a.key_sketch, b.key_sketch),
        value_sketch: Sketch.union(a.value_sketch, b.value_sketch),
        symbol_sketch: Sketch.union(a.symbol_sketch, b.symbol_sketch),
        histogram: Histogram.union(a.histogram, b.histogram),
      )
    end

    # Returns the union of all summaries of objects in *ee*. Uses the block
    # to summarize an object from *ee*.
    def self.union(ee : Enumerable(T), & : T -> Summary) : Summary forall T
      memo = zero
      ee.each do |object|
        memo = self.union(memo, yield object)
      end
      memo
    end

    # Associates *summary* with the given *dict*.
    #
    # This function is supposed to host finalization and recursive steps
    # for otherwise purely recursive dict metrics (such as maxdepth; that is,
    # nothing but dicts contribute to the metric).
    def self.assoc(summary : Summary, dict : Term::Dict) : Summary
      size_set = summary.size_set
      if summary.size < UInt16::MAX
        size_set = size_set.add(summary.size.to_u16)
      end

      maxdepth16 = summary.maxdepth16 &+ 1
      if maxdepth16.zero? # Overflow
        maxdepth16 = UInt16::MAX
      end

      summary.copy_with(maxdepth16: maxdepth16, size_set: size_set)
    end
  end
end
