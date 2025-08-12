module Ww::Soma::Microfold
  # :nodoc:
  class Theme
    getter flowR
    getter rem
    getter box_ruleset

    # :nodoc:
    def initialize(
      @spec : Term::Dict,
      @rem : Term::Num,
      @flowR : Rewriter,
      @box_ranks : Hash(Term, Int32),
      @box_ruleset : Ruleset,
    )
    end

    def self.new(document : Term::Dict, rem : Term::Num) : Theme
      flowR = Pass::Flow.flowR(document[:flow]? || Term.of)

      ranks = {} of Term => Int32

      hierarchy = document[:boxes, :hierarchy]? || Term[]
      hierarchy.items.each_with_index do |box, rank|
        ranks[box] = rank
      end

      selector = ML.term %{[rule pattern_ template_]}
      box_ruleset = Ruleset.select(selector, document[:boxes])

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
  end
end
