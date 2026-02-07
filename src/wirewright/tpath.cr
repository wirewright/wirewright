module Ww
  # Short for *termpath*. The set of termpaths is a superset of the set of
  # keypaths. The set of keypaths is a superset of the set of itempaths.
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

    def self.[](other : Tpath) : Tpath
      other
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
      Tpath.new(Slice(Step).new(steps.size) { |index| steps[index].as(Step) })
    end

    # Constructs a key step. Uses `Term.of` on *object* to obtain a term.
    def self.key(object)
      Key.new(Term.of(object))
    end

    # Constructs a value step. Uses `Term.of` on *object* to obtain a term.
    def self.value(object)
      Value.new(Term.of(object))
    end

    # Yields `Tpath`s and their corresponding values in *term* to the block.
    def self.each(term : Term, & : Tpath, Term ->)
      state_value = -2 # const
      state_key = -1   # const
      state_iter = 0   # and above

      stack = [{state: state_value, path: Tpath[], term: term}]

      while rec = stack.pop?
        case rec[:state]
        when state_key
          unless rec[:term].type.dict?
            yield rec[:path], rec[:term]
            next
          end

          stack << rec.merge(state: state_iter, term: rec[:term])
        when state_value
          unless rec[:term].type.dict?
            yield rec[:path], rec[:term]
            next
          end

          stack << rec.merge(state: state_iter, term: rec[:term])
        else
          # If state is zero or positive = N, this means nth(N) on the term
          # and an assertion that term is a dict.
          dict = rec[:term].unsafe_as_d
          next unless entry = dict.nth?(rec[:state])

          key, value = entry
          stack << (rec.merge(state: rec[:state] + 1, term: rec[:term]))
          stack << (rec.merge(state: state_value, term: value, path: rec[:path].append(value(key))))
          stack << (rec.merge(state: state_key, term: key, path: rec[:path].append(key(key))))
        end
      end
    end

    delegate :unsafe_fetch, :size, to: @steps

    {% for method in %w[prepend append] %}
      def {{method.id}}(step : Step)
        Tpath.new(@steps.{{method.id}}(step))
      end
    {% end %}

    def starts_with?(other : Tpath) : Bool
      return false unless size >= other.size

      @steps.starts_with?(other.@steps)
    end

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
