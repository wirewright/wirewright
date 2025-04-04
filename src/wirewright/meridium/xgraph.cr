module Ww::Meridium
  # Xgraph takes the set of matched "facts" (see `Utrie`) and searches for
  # `mount`ed conjunctions (called *xrules*) that these facts satisfy.
  #
  # In a nutshell, thanks to `Utrie`, this turns into a subset search problem:
  # given a set of rules R where each rule r is a set of labels (e.g. integers),
  # and a set of "facts" known about the term, F, also a set of labels (e.g. integers),
  # find all rules r that are subset of F.
  #
  # The core problem here and the one Xgraph tries to solve to an extent, is that
  # we *really really* don't want to depend on the number of rules in R. R could
  # contain millions of rules; whereas in practice we see only several triggered
  # by a particular term. So we want an algorithm whose runtime depends on the size
  # of F for as long as possible and as much as possible; vs. one whose runtime
  # depends on R.
  struct Xgraph
    # Represents a connection between two "facts".
    record Key, a : Label, b : Label

    # Gives a name to a connection between two "facts". Thus a higher-order
    # "fact" emerges, that is the combination of two basis "facts".
    record Value, id : Label

    def initialize(@fresh : LabelGenerator, @map : IMap(Key, Value))
    end

    # Mounts an Xgraph rule (an *xrule*) for *referrer*. Returns a bag of pair
    # dependencies (an existing bag can be provided via *deps*) for removal or
    # maintenance; and the label of the resulting xrule. See also: `Xgraph`.
    #
    # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
    # until this method returns.
    def mount(referrer : Label, xrule : Deque(Label), *, deps : D = Bag({Key, Value}).new) : {D, Label} forall D
      if xrule.empty?
        raise ArgumentError.new
      end

      while xrule.size > 1
        a = xrule.shift
        b = xrule.shift

        key = Key.new(a, b)
        value = @map.inc(referrer, key, Value.new(@fresh.call))
        deps << {key, value}

        xrule << value.id
      end

      {deps, xrule[0]}
    end

    private def conjs(referrer : Label, vertices : Deque(Label), sink : Label ->) : Nil
      while a = vertices.shift?
        sink.call(a)

        (0...vertices.size).each do |i|
          b = vertices.unsafe_fetch(i)
          next unless value = @map.latest?(referrer, Key.new(a, b))

          vertices << value.id
        end
      end
    end

    # Calls *sink* with all mounted conjunction vertices whose corresponding
    # conjunctions are satisfied by *vertices*.
    #
    # NOTE: *vertices* must be pre-sorted ascending. You lose ownership of *vertices*
    # until this method returns.
    def conjs(referrer : Label, vertices : Deque(Label), &sink : Label ->) : Nil
      conjs(referrer, vertices, sink)
    end
  end

  # Encoding / decoding of Utrie keys, values to terms.

  struct ::Ww::Term
    def self.encode(src : Xgraph::Key) : Term
      Term.of(:xgraph, :key, :primary, encode(src.a), encode(src.b))
    end

    def self.decode?(dst : Xgraph::Key.class, term : Term) : Xgraph::Key?
      matchpi?(term, %{(xgraph key primary a_ b_)}) do
        Xgraph::Key.new(decode?(Label, a) || return, decode?(Label, b) || return)
      end
    end

    def self.encode(src : Xgraph::Value) : Term
      Term.of(:xgraph, :value, :primary, encode(src.id))
    end

    def self.decode?(dst : Xgraph::Value.class, term : Term) : Xgraph::Value?
      matchpi?(term, %{(xgraph value primary id_)}) do
        Xgraph::Value.new(Term.decode(Label, id) || return)
      end
    end
  end
end
