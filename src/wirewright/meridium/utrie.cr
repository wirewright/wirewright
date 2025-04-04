module Ww::Meridium
  # A trie of `Ubase`s. Each `Ubase` acts as a "gate-keeper" for terms. Arranging
  # Ubases in *strands* (chains of Ubases) allows one to filter terms with precision.
  # Each step in the chain is labeled, and this label is emitted at query-time.
  # Labels at the ends of strands are treated specially -- as confirmation of a certain
  # "fact" about the term. These "facts" are later combined conjunctively using `Xgraph`.
  #
  # During query, it is important to understand the following. Due to the nature of
  # the underlying map, key-value pairs may randomly disappear, thus blocking the passage
  # for activations by the query term. This means parts of Utrie may be temporarily
  # unreachable during `query`, depending on the health of the map. In other words,
  # depending on the health of the underlying map, the query may result in outdated labels,
  # or with too few labels (parts of the trie unreachable). If the Origin of the trie
  # degenerates, until it is restored, the entire trie will be unreachable for some time,
  # until the origin is restored by one of the peers. This cannot be fixed on the "higher-
  # order data structure" level. Introducing replication at the underlying map level
  # should help in practice, however. Instead of storing trie Origin on one node,
  # store it on three, or ten; so there's always someone to fall back on instead
  # of an immediate absence report.
  struct Utrie
    alias Key = Origin | Step

    # :nodoc:
    #
    # The consensus starting point of the Utrie in the underlying map. Corresponds
    # to the initial `Ubase::Trunk` in strands, whose purpose is basically to verify
    # that the term is "anything at all" (which it always is!). `Trunk` (and therefore
    # Origin) is also the point-of-interest for `_` queries (toplevel any), e.g. `x_`.
    record Origin do
      def encode(otype : Term.class) : Term
        Term.of(:utrie, :key, :origin)
      end

      def self.decode?(term : Term) : Key?
        Term.matchpi?(term, %{(utrie key origin)}) { new }
      end
    end

    # :nodoc:
    #
    # Represents each subsequent Ubase step following the `Origin`.
    record Step, id : Label, base : Ubase::Any do
      def encode(otype : Term.class) : Term
        Term.of(:utrie, :key, :step, id.encode(Term), Term.encode(base))
      end

      def self.decode?(term : Term) : Key?
        Term.matchpi?(term, %{(utrie key step id_ base_)}) do
          new(Label.decode?(id) || return, Term.decode?(Ubase::Any, base) || return)
        end
      end
    end

    # :nodoc:
    #
    # Utrie values store links to the successor (`Step#id`).
    record Value, succ : Label do
      def encode(otype : Term.class) : Term
        Term.of(:utrie, :value, :primary, succ.encode(Term))
      end

      def self.decode?(term : Term) : Value?
        Term.matchpi?(term, %{(utrie value primary succ_)}) do
          new(Label.decode?(succ) || return)
        end
      end
    end

    def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
    end

    # Mounts a *strand* of `Ubase`s for *referrer*. Returns a bag of pair
    # dependencies (an existing bag can be provided via *deps*) for removal or
    # maintenance; and the label of the step thus reached (`Step#id` of the last
    # base in *strand*).
    def mount(referrer : Label, strand : Strand, *, deps : D = Bag({Key, Value}).new) : {D, Label} forall D
      unless strand[0]? == Ubase::Trunk.new
        raise ArgumentError.new("expected a nonempty strand that starts with Trunk")
      end

      key = Origin.new
      origin = @map.inc(referrer, key, Value.new(@fresh.call))
      current = origin.succ
      deps << {key, origin}

      strand[1..].each do |base|
        key = Step.new(current, base)
        step = @map.inc(referrer, key, Value.new(@fresh.call))
        current = step.succ
        deps << {key, step}
      end

      {deps, current}
    end

    private def successor?(referrer : Label, key : Key) : Label?
      @map.latest?(referrer, key).try(&.succ)
    end

    {% for type, base in {Term::Num => Ubase::IsNum, Term::Str => Ubase::IsStr, Term::Sym => Ubase::IsSym, Term::Boolean => Ubase::IsBool} %}
      private def query(referrer : Label, id : Label, term : {{type}}, sink : Label ->) : Nil
        return unless succ0 = successor?(referrer, Step.new(id, {{base}}.new))

        sink.call(succ0)

        if succ1 = successor?(referrer, Step.new(succ0, Ubase::Literal.new(Term.of(term))))
          sink.call(succ1)
        end
      end
    {% end %}

    # NOTE: dictionaries must be normalized to IsDict - At(), even literal ones.
    # We do not handle Literal(dict).
    private def query(referrer : Label, id : Label, term : Term::Dict, sink : Label ->) : Nil
      return unless succ0 = successor?(referrer, Step.new(id, Ubase::IsDict.new))

      sink.call(succ0)

      term.each_entry do |key, value|
        next unless succ1 = successor?(referrer, Step.new(succ0, Ubase::At.new(key)))

        sink.call(succ1)

        query(referrer, succ1, value.downcast, sink)
      end
    end

    private def query(referrer : Label, term : Term, sink) : Nil
      return unless succ = successor?(referrer, Origin.new)

      sink.call(succ)

      query(referrer, succ, term.downcast, sink)
    end

    # Calls *sink* with all step ids activated by *term*; essentially, with all
    # "facts" that the trie can note in *term*.
    #
    # See also: `Utrie`.
    def query(referrer : Label, term : Term, &sink : Label ->) : Nil
      query(referrer, term, sink)
    end
  end
end
