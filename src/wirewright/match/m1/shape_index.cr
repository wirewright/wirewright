module Ww::M1
  # A pattern indexing data structure that uses `M1.shape`.
  #
  # ```
  # idx, pattern2component = M1::ShapeIndex.build([
  #   ML.term(%{(+ _ _)}),
  #   ML.term(%{(- _ _)}),
  #   ML.term(%{_}),
  # ])
  #
  # component2patterns = pattern2component.inverted_index
  #
  # components = idx.decompose(Term.of(:*, 1, 2, 3, 4))
  # components.each do |component|
  #   pp component2patterns[component]
  #   # => [2]
  # end
  #
  # components = idx.decompose(Term.of(:+, 1, 2, 3, 4))
  # components.each do |component|
  #   pp component2patterns[component]
  #   # => [0]
  #   # => [2]
  # end
  # ```
  struct ShapeIndex
    # :nodoc:
    module Base
      extend self

      alias Any = Key | Literal | Subtype | Feature

      defrecord Feature
      defrecord Key, term : Term
      defrecord Literal, term : Term
      defrecord Subtype, type : TermType

      def parse(unit : Term) : Any
        Term.case(unit) do
          matchpi %{(%'%value (%'%literal key_))} { Key.new(key) }
          matchpi %{(%'%literal value_)} { Literal.new(value) }
          matchpi %{%'(%any)} { Subtype.new(:any) }
          matchpi %{%'(%symbol)} { Subtype.new(:symbol) }
          matchpi %{%'(%string)} { Subtype.new(:string) }
          matchpi %{%'(%number _)} { Subtype.new(:number) }
          matchpi %{%'(%boolean)} { Subtype.new(:boolean) }
          matchpi %{%'(%dict)} { Subtype.new(:dict) }
        end
      end
    end

    # :nodoc:
    alias Vertex = UInt32

    # :nodoc:
    TRIE_ROOT = Vertex.new(0)

    # :nodoc:
    TRIE_ZERO = TRIE_ROOT.succ

    # :nodoc:
    #
    # - *features* contains the features (feature trie vertices) of this complex.
    # - *includers* contains ids of components that include this complex.
    defrecord Complex, features : Pf::USet32, includers : Pf::USet32

    # :nodoc:
    def initialize(
      @features : Hash(Vertex, Hash(Base::Any, Vertex)),
      @complexes : Slice(Complex),
    )
    end

    # Constructs a pattern shape index for *patterns*. Returns the shape index and
    # a transcription map to convert component vertices from `decompose` to indices
    # into *patterns*. The transcription map maps pattern index to the corresponding
    # component vertex. Different patterns may map to the same component vertex if
    # `ShapeIndex` considers them equal.
    def self.build(patterns : Indexable(Term)) : {ShapeIndex, Slice(UInt32)}
      # Makes a new vertex id.
      fresh = TRIE_ZERO
      mkv = -> do
        fresh, _ = fresh.succ, fresh
      end

      features = {} of Vertex => Hash(Base::Any, Vertex)
      complexes = {} of Pf::USet32 => Vertex
      components = {} of Pf::USet32 => Vertex

      # Connects *pivot* through *base* to a new or existing vertex. Returns
      # its id.
      connect = ->(pivot : Vertex, base : Base::Any) do
        features.put_if_absent(pivot, base, &mkv)
      end

      # If that complex is found, then propose those components.
      includers = {} of Vertex => Pf::USet32

      transcript = patterns.to_readonly_slice do |pattern|
        normp = M1.normal(pattern)
        shape = M1.shape(normp)

        # A component is a disjunction of complexes. "If you are able to find any of
        # those complexes, then you have found that component."
        component = Pf::USet32.transaction do |component|
          M1.branches(shape) do |branch|
            # A complex is a conjunction of strands. A strand corresponds to a path
            # through the feature trie. "If you are able to follow all those strands,
            # then you have found that complex."
            complex = Pf::USet32.transaction do |complex|
              M1.strands(branch) do |strand|
                assert strand.items.present?

                final = nil
                pivot = TRIE_ROOT

                strand.items.each do |unit|
                  final = base = Base.parse(unit)
                  pivot = connect.call(pivot, base)
                end

                # Insert an explicit end marker, Feature, if the strand does not
                # end with Literal, which already acts as an end marker.
                unless final.is_a?(Base::Literal)
                  pivot = connect.call(pivot, Base::Feature.new)
                end

                complex << pivot
              end
            end

            component << complexes.put_if_absent(complex, &mkv)
          end
        end

        component_id = components.put_if_absent(component, &mkv)
        component.each do |complex_id|
          includers.update(complex_id, Pf::USet32.new, &.add(component_id))
        end

        component_id
      end

      instance = new(
        features: features,
        complexes: complexes.to_readonly_slice { |features, id| Complex.new(features, includers[id]) }
      )

      {instance, transcript}
    end

    private def populate(features : Pf::USet32::Commit, matchee : Term, pivot : Vertex) : Nil
      return unless successors = @features[pivot]?

      # Feature: any value, any type.
      if feature = successors[Base::Feature.new]?
        features << feature
      end

      return unless pivot = successors[Base::Subtype.new(matchee.type)]?

      successors = @features[pivot]

      # Feature: any value, of specific type.
      if feature = successors[Base::Feature.new]?
        features << feature
      end

      # Feature: literal value, of specific type.
      if feature = successors[Base::Literal.new(matchee)]?
        features << feature
      end

      return unless matchee.type.dict?

      # Have a fast path in case the trie has less entries (most likely it would!)
      if successors.size <= matchee.size
        successors.each do |base, successor|
          next unless base.is_a?(Base::Key)
          next unless value = matchee[base.term]?

          populate(features, value, successor)
        end
        return
      end

      matchee.each_entry do |key, value|
        next unless successor = successors[Base::Key.new(key)]?

        populate(features, value, successor)
      end
    end

    private def populate(features : Pf::USet32::Commit, matchee : Term) : Nil
      return unless successors = @features[TRIE_ROOT]?

      # All strands begin with (root) - Any - ...
      return unless pivot = successors[Base::Subtype.new(:any)]?

      populate(features, matchee, pivot: pivot)
    end

    # Yields vertices corresponding to components that *respond positively*
    # to *matchee*.
    #
    # `ShapeIndex.build` returns a transcription map; you can use it to convert
    # component vertices to pattern indices in the indexable you've passed
    # to `ShapeIndex.build`.
    #
    # A *positive response* does not mean a *match*. It simply means the caller
    # should move on to more expensive checks. At the end the caller will have to
    # call M1 itself, which will give a definitive answer (but one expensive to
    # compute, especially on collections of patterns).
    #
    # NOTE: this method is allowed to emit duplicate vertices!
    def decompose(matchee : Term, & : UInt32 ->) : Nil
      features = Pf::USet32.transaction do |commit|
        populate(commit, matchee)
      end

      @complexes.each do |complex|
        next unless complex.features.subset_of?(features)

        complex.includers.each do |includer|
          yield includer
        end
      end
    end

    # Returns a `Pf::USet32` containing the decomposition of *matchee*.
    def decompose(matchee : Term) : Pf::USet32
      Pf::USet32.transaction do |commit|
        decompose(matchee) do |proposition|
          commit << proposition
        end
      end
    end
  end
end
