struct Ww::Term
  # Implementation of the term difference algorithm and associated utilities
  # (such as those for applying the differences to a term).
  #
  # See `Term.diff?`.
  module Diff
    extend self

    alias Action = ItemAction | PairAction
    alias ItemAction = WithItem | InsertItem | DeleteItem
    alias PairAction = WithPair | WithoutPair

    defrecord WithPair, prefix : Slice(Term), key : Term, value : Term, copying: true
    defrecord WithoutPair, prefix : Slice(Term), key : Term, copying: true
    defrecord WithItem, prefix : Slice(Term), index : UInt32, item : Term, copying: true
    defrecord InsertItem, prefix : Slice(Term), index : UInt32, item : Term, copying: true
    defrecord DeleteItem, prefix : Slice(Term), index : UInt32, copying: true

    {% for cls in %w[WithPair WithoutPair] %}
      struct {{cls.id}}
        # Returns the full path for this action (`prefix` plus `key`).
        def path : Slice(Term)
          prefix.append(key)
        end
      end
    {% end %}

    {% for cls in %w[WithItem InsertItem DeleteItem] %}
      struct {{cls.id}}
        # Returns the full path for this action (`prefix` plus `key`).
        def path : Slice(Term)
          prefix.append(Term.of(index))
        end
      end
    {% end %}

    defrecord DiffCell, decision : DiffDecision, cost : UInt32

    enum DiffDecision
      Keep
      Insert
      Delete
      Replace

      # NOTE: We penalize inserts and deletes because they are (currently) more
      # expensive to run on a dict.
      def cost : UInt32
        case self
        in .keep?    then 0u32
        in .insert?  then 2u32
        in .delete?  then 2u32
        in .replace? then 1u32
        end
      end
    end

    # Returns a sequence of `Action`s to transform *reference* into *successor*.
    # Returns `nil` if the required action is complete replacement.
    #
    # See `Term.diff?` for more info.
    def diff?(reference : Term, successor : Term, depth_limit : UInt32) : Array(Action)?
      return unless reference.type == successor.type

      if depth_limit.zero? || !reference.type.dict?
        # Types are equal, therefore !successor.type.dict?
        return reference == successor ? [] of Action : nil
      end

      # Types are equal, therefore successor.type.dict?
      reference = reference.as_d
      successor = successor.as_d

      actions = [] of Action

      # Deal with the easy stuff first: emit actions pertaining to
      # the pairspart.
      reference.each_entry(in: Term::Dict.pairspart) do |key, _|
        next if key.in?(successor)

        actions << WithoutPair.new(Slice(Term).empty, key)
      end

      successor.each_entry(in: Term::Dict.pairspart) do |key, value1|
        value0 = reference[key]?
        next if value0 == value1

        # Pair created.
        if value0.nil?
          actions << WithPair.new(Slice(Term).empty, key, value1)
          next
        end

        # Pair changed.
        unless subactions = diff?(value0, value1, depth_limit - 1)
          # E.g. 100 -> 200.
          actions << WithPair.new(Slice(Term).empty, key, value1)
          next
        end

        # E.g. (1 2 3) -> (2 3 4), where we can do a diff.
        subactions.each do |subaction|
          actions << subaction.copy_with(prefix: subaction.prefix.prepend(key))
        end
      end

      # Now we handle changes to the itemspart.
      h = reference.uitemsize + 1
      w = successor.uitemsize + 1
      matrix = Slice(DiffCell).new(h * w, DiffCell.new(:keep, cost: 0u32))

      (0u32...h).each do |y|
        (0u32...w).each do |x|
          cell = pass do
            # Initialize (0, 0).
            if x == 0 && y == 0
              next DiffCell.new(DiffDecision::Keep, cost: DiffDecision::Keep.cost)
            end

            # Initialize the first column.
            if x == 0
              next DiffCell.new(DiffDecision::Delete, cost: y * DiffDecision::Delete.cost)
            end

            # Initialize the first row.
            if y == 0
              next DiffCell.new(DiffDecision::Insert, cost: x * DiffDecision::Insert.cost)
            end

            source = reference[y - 1]
            target = successor[x - 1]

            if source == target
              decision = DiffDecision::Keep
              cost = matrix[(y - 1)*w + (x - 1)].cost
            else
              candidates = {
                {DiffDecision::Insert, matrix[y*w + (x - 1)].cost},
                {DiffDecision::Delete, matrix[(y - 1)*w + x].cost},
                {DiffDecision::Replace, matrix[(y - 1)*w + (x - 1)].cost},
              }
              decision, cost = candidates.min_by { |decision, cost| cost + decision.cost }
            end

            DiffCell.new(decision, cost + decision.cost)
          end

          matrix[y*w + x] = cell
        end
      end

      y = reference.uitemsize
      x = successor.uitemsize

      while y > 0 || x > 0
        if y == 0
          # NOTE: We unshift to avoid having to reverse the array. Crystal's Array
          # impl makes this cheap. Withs/Withouts for pairs do not care about order,
          # they are orthogonal wrt. itemspart actions.
          actions.unshift InsertItem.new(Slice(Term).empty, y, successor[x - 1])
          x -= 1
          next
        end

        if x == 0
          actions.unshift DeleteItem.new(Slice(Term).empty, y - 1)
          y -= 1
          next
        end

        source = reference[y - 1]
        target = successor[x - 1]

        if source == target
          x -= 1
          y -= 1
          next
        end

        cell = matrix[y*w + x]

        case cell.decision
        in .keep?
          x -= 1
          y -= 1
        in .replace?
          if subactions = diff?(source, target, depth_limit - 1)
            # Prepend sub-actions while maintaining their internal order.
            subactions.reverse_each do |subaction|
              actions.unshift subaction.copy_with(prefix: subaction.prefix.prepend(Term.of(y - 1)))
            end
          else
            actions.unshift WithItem.new(Slice(Term).empty, y - 1, target)
          end

          x -= 1
          y -= 1
        in .insert?
          actions.unshift InsertItem.new(Slice(Term).empty, y, target)
          x -= 1
        in .delete?
          actions.unshift DeleteItem.new(Slice(Term).empty, y - 1)
          y -= 1
        end
      end

      actions
    end

    # Returns `true` if actions in *l* are *compatible* with actions in *r*.
    #
    # Compatibility is defined as absence of conflicts. A conflict, in turn, is one
    # of the following.
    #
    # For equal prefixes, it is a conflict when:
    # - *l* wants a value V0 at K, while *r* wants V1 at K, V0 != V1.
    # - *l* wants a value V0 at K, and *r* wants to remove K.
    # - *l* wants an item to be I0, and *r* wants it to be I1, I0 != I1.
    # - *l* wants to delete an item that *r* updated, or vice versa.
    #
    # For different prefixes, it is a conflict when:
    # - *l* wants to update a prefix of an action in *r*, or vice versa. E.g.
    #   *l* wants to update 1, and *r* wants to delete 1-2-3. This is a conflict
    #   because *l* overwrites (stomps over) the changes made by *r*.
    # - *l* wants to delete a prefix of an action in *r*, or vice versa. E.g.,
    #   *l* wants to delete 1, and *r* wants to update 1-2-3. This is a conflict
    #   because *l* deletes the changes made by *r*.
    def compatible?(l : Array(Action), r : Array(Action)) : Bool
      if l.empty? || r.empty?
        return true # Either or both of the parties want no changes.
      end

      !l.any? do |laction|
        r.any? do |raction|
          conflict?(laction, raction)
        end
      end
    end

    # Returns `true` if two actions *l* and *r* are in conflict.
    def conflict?(l : Action, r : Action) : Bool
      lpath = l.path
      rpath = r.path

      case {l, r}
      in {InsertItem, _}, {_, InsertItem}
        false
      in {ItemAction, PairAction}, {PairAction, ItemAction}
        overwrites?(lpath, rpath)
      in {WithItem, WithItem}
        overwrites?(lpath, rpath) || (lpath == rpath && l.item != r.item)
      in {WithItem, DeleteItem}, {DeleteItem, WithItem}
        overwrites?(lpath, rpath) || lpath == rpath
      in {DeleteItem, DeleteItem}
        overwrites?(lpath, rpath) && lpath != rpath # Deletes of the same item do not conflict
      in {WithPair, WithPair}
        overwrites?(lpath, rpath) || (lpath == rpath && l.value != r.value)
      in {WithPair, WithoutPair}, {WithoutPair, WithPair}
        overwrites?(lpath, rpath) || lpath == rpath
      in {WithoutPair, WithoutPair}
        overwrites?(lpath, rpath)
      end
    end

    private def overwrites?(l : Slice(Term), r : Slice(Term)) : Bool
      l.starts_with?(r) || r.starts_with?(l)
    end

    # A container for actions concerning a reference dict, which is used to `apply`
    # those actions to the reference dict.
    struct Mutation
      def initialize
        @trie = {} of {UInt32, Term} => UInt32
        @fanout = [[] of Term]
        @actions = [[] of Term::Diff::Action]
      end

      def <<(action : Term::Diff::Action) : self
        current = 0u32 # root
        action.prefix.each do |key|
          @fanout[current] << key

          current = @trie.put_if_absent({current, key}) do
            @fanout << [] of Term
            @actions << [] of Term::Diff::Action
            @actions.size.to_u32 - 1
          end
        end

        @actions[current] << action

        self
      end

      # WARNING: *reference* must be the reference dict for all actions appended to
      # this mutation. That is, all action prefixes and keys must be valid
      # for *reference*.
      def apply(reference : Term::Dict) : Term::Dict
        apply(reference, current: 0u32) # root
      end

      private def apply(dict : Term::Dict, current : UInt32) : Term::Dict
        # Collect actions.
        updates = [] of Term::Diff::WithItem | Term::Diff::WithPair | Term::Diff::WithoutPair
        inserts = {} of UInt32 => Array(Term)
        deletes = Set(UInt32).new

        @actions[current].each do |action|
          case action
          in Term::Diff::WithItem,
             Term::Diff::WithPair,
             Term::Diff::WithoutPair
            updates << action
          in Term::Diff::InsertItem
            bucket = inserts.put_if_absent(action.index) { [] of Term }
            bucket << action.item
          in Term::Diff::DeleteItem
            deletes << action.index
          end
        end

        dict = dict.transaction do |commit|
          # Apply recursively.
          @fanout[current].each do |key|
            value0 = dict[key].as_d
            value1 = apply(value0, @trie[{current, key}])
            commit.with(key, value1)
          end

          # Apply updates.
          updates.each do |action|
            case action
            in Term::Diff::WithItem
              commit.with(action.index, action.item) # goes to a fast path overload
            in Term::Diff::WithPair
              commit.with(action.key, action.value)
            in Term::Diff::WithoutPair
              commit.without(action.key)
            end
          end
        end

        if inserts.empty? && deletes.empty?
          return dict
        end

        dict.pairspart.transaction do |commit|
          dict.items.each_with_index do |item, index|
            # Checkout if we have any items to insert before this one.
            if bucket = inserts[index]?
              commit.concat(bucket)
            end

            # Skip if this item was deleted.
            next if index.in?(deletes)

            commit << item
          end

          # Handle inserts at the end of the dict.
          next unless bucket = inserts[dict.itemsize]?

          commit.concat(bucket)
        end
      end
    end
  end

  # Returns a sequence of `Diff::Action`s to transform *reference* into *successor*.
  # Returns `nil` if the required action is complete replacement.
  #
  # - The simplest case to get a `nil` out of this function is two different
  #   terms, e.g. `100` and `200`.
  # - An empty action array is returned when the terms are equal, meaning no
  #   actions are necessary to turn one into the other. For example, setting
  #   *reference* to `100` and *successor* to `100` will return an empty
  #   action array.
  # - Simple changes such as adding or removing a pair are easily detected,
  #   and result in one `With` or `Without` for each added pair.
  # - Itemsparts go through a [diff algorithm](https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm).
  #
  # *depth limit* can be configured to treat dictionaries atomically beyond
  # a certain depth. For example, `0` treats all terms atomically, and turns
  # `diff?` into something resembling an equality check; `1` descends into
  # a top-level dictionary *reference*/*successor*, but no further.
  def self.diff?(reference : Term, successor : Term, *, depth_limit : UInt32 = UInt32::MAX) : Array(Diff::Action)?
    Diff.diff?(reference, successor, depth_limit)
  end
end
