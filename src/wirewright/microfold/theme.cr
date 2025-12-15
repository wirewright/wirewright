module Ww::Microfold
  # :nodoc:
  class Theme
    getter flowR
    getter rem
    getter box_ruleset

    DESIGNATION_CACHE_CAPACITY   = 512
    DECOMPOSITION_CACHE_CAPACITY = 512

    # :nodoc:
    def initialize(
      @spec : Term::Dict,
      @rem : Term::Num,
      @flowR : Rewriter,
      @box_ranks : Hash(Term, Int32),
      @box_ruleset : Ruleset,
    )
      @decompositions = Hash({Term::Dict, Term}, Term::Dict).new(initial_capacity: DECOMPOSITION_CACHE_CAPACITY)
      @designations = Hash({Term::Dict, Term::Dict}, Term::Dict).new(initial_capacity: DESIGNATION_CACHE_CAPACITY)
    end

    def self.new(document : Term::Dict, rem : Term::Num) : Theme
      flowR = Pass::Flow.flowR(document[:flow]? || Term.of)

      ranks = {} of Term => Int32

      hierarchy = document[:boxes, :hierarchy]? || Term[]
      hierarchy.items.each_with_index do |box, rank|
        ranks[box] = rank
      end

      selector = ML.term %{[rule pattern_ template_]}
      box_ruleset = Ruleset.select(selector, document[:boxes]? || Term.of)

      Theme.new(document, rem, flowR, ranks, box_ruleset)
    end

    def box_rank?(box : Term)
      @box_ranks[box]?
    end

    def global?(name : Term) : Term?
      @spec[:globals, name]?
    end

    def table?(name : Term) : Term::Dict?
      return unless table = @spec[name]?

      table.as_d?
    end

    def cascade?(box : Term) : Bool
      !!@spec[:boxes, :cascade, box]?
    end

    def has_preset?(tag : Term) : Bool
      !!@spec[:presets, tag]?
    end

    def preset?(tag : Term, issues) : Term?
      issues.adjoin("preset for", tag) do |issues|
        return unless preset = @spec[:presets, tag]?

        unless preset.type.string?
          issues.severe("preset for `#{tag}` must be a string")
          return
        end

        preset
      end
    end

    def utility_spec?(id : Term) : Term?
      @spec[:utilities, id]?
    end

    def property_spec?(id : Term) : Term?
      @spec[:properties, id]?
    end

    def each_flow_key_to_prune(&)
      keys = @spec[:flow, :prune]?.try(&.as_d?) || Term[]
      keys.each_entry do |key, _|
        yield key
      end
    end

    def decomposition_of(pairs : Term::Dict, style : Term, & : Term::Dict -> {Bool, Term::Dict}) : Term::Dict
      if decomposition = @decompositions[{pairs, style}]?
        return decomposition
      end

      ok, decomposition = yield pairs
      unless ok
        # If it's not ok, this means it has issues. If it has issues, that's
        # a side effect; we cannot cache, because subsequent decompositions will
        # be quiet about those.
        return decomposition
      end

      if @decompositions.size >= DECOMPOSITION_CACHE_CAPACITY
        # Evict oldest.
        @decompositions.delete(@decompositions.first_key)
      end

      @decompositions[{pairs, style}] = decomposition
    end

    def designations_of(preset : Term::Dict, style : Term::Dict, & : Term::Dict, Term::Dict -> Term::Dict) : Term::Dict
      if @designations.size >= DESIGNATION_CACHE_CAPACITY
        # Evict oldest.
        @designations.delete(@designations.first_key)
      end

      @designations.put_if_absent({preset, style}) do
        yield preset, style
      end
    end
  end
end
