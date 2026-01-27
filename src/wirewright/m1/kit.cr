module Ww::M1
  # Auxiliary functions for M1 (especially the M1 pattern compiler).
  module Kit
    extend self

    # Attempts to interpret *term* as `Magnitude`.
    #
    # - `∞` gives `Magnitude::INFINITY`.
    # - Otherwise uses `Term#to`, which will raise `TypeCastError` on failure.
    def magn(term : Term) : Magnitude
      term == Term.of(:∞) ? Magnitude::INFINITY : term.to(Magnitude)
    end

    # Returns `true` if a normal *op* is the singular sequence operator.
    def singular?(op : Term) : Bool
      Term.case(op, engine: M0) do
        matchpi %{[%'%singular _]}, cue: :"%singular" { true }
        otherwise { false }
      end
    end

    # Returns `true` if all of normal *ops* are singular sequence operators.
    def singulars?(ops : Indexable(Term))
      ops.all? { |op| singular?(op) }
    end

    # Flattens `seq: true passable: true` non-sealed members of *op* (assumed to be
    # a normal `%seq`) by recursing into them; appends other `seq: true` members
    # to the resulting array.
    def flatseq(op : Term::Dict) : Array(Term::Dict)
      sink = [] of Term::Dict
      flatseq(op, sink)
      sink
    end

    # Flattens `seq: true passable: true` non-sealed members of *op* (assumed to be
    # a normal `%seq`) by recursing into them; appends other `seq: true` members
    # to *sink*.
    #
    # *sink* must respond to `<<`.
    def flatseq(seq : Term::Dict, sink) : Nil
      each_member(seq) do |member|
        next unless member[:seq]?
        next if member[:sealed]?

        if member[:passable]?
          flatseq(member, sink)
          next
        end

        sink << member
      end
    end

    # :nodoc:
    HEADSYMS_NONMEMBER = Set{Term[:"%ref"], Term[:"%capture"], Term[:"%key"], Term[:"%payload"]}

    # Returns `true` if *dict* could be considered a member of some parent
    # operator. This function is usually called by the parent operator to
    # find its members among sub-terms.
    def member?(dict : Term::Dict) : Bool
      return false unless head = dict.items.first?
      return false unless headsym = head.as_sym?
      return false unless headsym.prefixed_by?('%')
      return false if headsym.in?(HEADSYMS_NONMEMBER)

      true
    end

    # Returns `true` if *dict* is a `terminal: true` operator.
    def terminal?(dict : Term::Dict) : Bool
      return false unless head = dict.items.first?
      return false unless headsym = head.as_sym?
      return false unless headsym.prefixed_by?('%')

      !!dict[:terminal]?
    end

    # Yields each member of a known normal operator *op*. Also yields the index
    # of *member* _in *op*_.
    def each_member(op : Term::Dict, & : Term::Dict, Int32 ->) : Nil
      return if terminal?(op)

      op.items.each_with_index do |item, index|
        next unless dict = item.as_d?
        next unless member?(dict)
        yield dict, index
      end
    end

    # Collects the members of *op* in an array.
    #
    # See also: `each_member`.
    def members(op : Term::Dict) : Array(Term::Dict)
      members = [] of Term::Dict
      each_member(op) do |member, _|
        members << member
      end
      members
    end

    # A helper function to peform a single pass of bottom-up rewriting on a normal
    # operator *op* and its subtree.
    #
    # Each member of *op* is rewritten recursively, followed by *fn* on *op* itself.
    def ascend(op : Term::Dict, &fn : Term::Dict -> Term::Dict) : Term::Dict
      rw = op.transaction do |commit|
        each_member(op) do |member, index|
          commit.with(index, ascend(member, &fn))
        end
      end

      fn.call(rw)
    end

    # A helper function to *walk* the given normal pattern operator *op*
    # and its subtree.
    #
    # `walk` differs from other methods such as `ascend` in the following ways:
    #
    # - `walk` is more relaxed than e.g. `ascend` in that it also emits non-member
    #   children of operators (but does not descend further!) This means it also emits
    #   non-member nodes such as `%ref`, `%capture`, etc.
    # - `walk` does not descend into operators marked as `disjunction: true` (such as `%any°`).
    #   You are expected to handle disjunctions yourself, possibly by recursing on
    #   their branches. The disjunction itself *is* passed to *fn*.
    def walk(op : Term::Dict, &fn : Term::Dict ->) : Nil
      fn.call(op)
      return if op[:disjunction]? || terminal?(op) || !member?(op)

      op.items.each do |item|
        next unless dict = item.as_d?
        next unless head = dict.items.first?
        next unless headsym = head.as_sym?
        next unless headsym.prefixed_by?('%')

        walk(dict, &fn)
      end
    end
  end
end
