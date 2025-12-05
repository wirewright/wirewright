module Ww
  # Short for *termpath*. The set of termpaths is a superset of the set of
  # keypaths. The set of keypaths is a superset of the set of itempaths.
  #
  # TODO: Termpaths will eventually succede `Backpath`s.
  struct Tpath
    alias Step = Key | Value

    record Key, key : Term do
      def inspect(io)
        io << "<" << key << ">"
      end
    end

    record Value, key : Term do
      def inspect(io)
        io << key
      end
    end

    include Indexable(Step)

    def initialize(@steps : Slice(Step))
    end

    def self.[] : Tpath
      Tpath.new(Slice(Step).empty)
    end

    def self.[](keypath : Indexable(Term)) : Tpath
      new(keypath.to_readonly_slice { |key| value(key).as(Step) })
    end

    def self.[](keypath : Term::Dict) : Tpath
      Tpath[keypath.items]
    end

    def self.[](itempath : Indexable(Int32)) : Tpath
      new(itempath.to_readonly_slice { |key| value(key).as(Step) })
    end

    def self.[](*steps : Step) : Tpath
      Tpath.new(Steps.new(steps.size) { |index| steps[index].as(Step) })
    end

    # Constructs a key step. Uses `Term.of` on *object* to obtain a term.
    def self.key(object)
      Key.new(Term.of(object))
    end

    # Constructs a value step. Uses `Term.of` on *object* to obtain a term.
    def self.value(object)
      Value.new(Term.of(object))
    end

    delegate :unsafe_fetch, :size, to: @steps

    {% for method in %w[prepend append] %}
      def {{method.id}}(step : Step)
        Tpath.new(@steps.{{method.id}}(step))
      end
    {% end %}

    def [](object) : Tpath
      Tpath.new(@steps[object])
    end

    def inspect(io)
      io << "Tpath["
      @steps.join(io, "-")
      io << "]"
    end
  end
end
