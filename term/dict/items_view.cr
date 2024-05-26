module Ww
  # Sits on top of a dictionary (see `Dict`) and provides a view of its items
  # part. Most read/cursor-like operations can be completed without rebuilding
  # the dictionary. `ItemsView` does the bookkeeping to make that possible.
  struct Term::Dict::ItemsView
    include Enumerable(Term)

    def initialize(@dict : Dict, @b = 0, @e : Int32 = dict.size)
    end

    # Returns `true` if there are no items in this items view.
    def empty? : Bool
      size.zero?
    end

    # Returns the amount of items in this items view.
    def size : Int32
      @e - @b
    end

    # Returns the first term in this items view. If none, returns nil.
    def first? : Term?
      return if empty?

      @dict[@b]?
    end

    # Looks up *index*-th term in this items view.
    #
    # Raises `KeyError` if the term is not found.
    def [](index : Number | Term::Num) : Term
      @dict[@b + index]
    end

    # Returns an empty items view pointing at the end of this items view. Can be
    # used to e.g. detect when iteration reached the end of the items view.
    #
    # ```text
    # D        1 2 3
    # V1      [      .]
    # V1.end        [.]
    # ```
    def end : ItemsView
      ItemsView.new(@dict, @e, @e)
    end

    # Moves the beginning of this items view *delta* items forward (`delta > 0`) or
    # backward (`delta < 0`). `delta = 0` results in no change.
    #
    # The final begin index is clamped between the beginning of the underlying
    # dictionary and the end of this items view.
    #
    # ```text
    # D             1 2 3 4 5 6 7
    # V1               [    .]
    # V1.move(-1)    [      .]
    # V1.move(-2)  [        .]
    # V1.move(-3)  [        .]
    #
    # D             1 2 3 4 5 6 7
    # V1               [    .]
    # V1.move(1)         [  .]
    # V1.move(2)           [.]
    # V1.move(3)           [.]
    # ```
    def move(delta : Int32) : ItemsView
      ItemsView.new(@dict, (@b + delta).clamp(0..@e), @e)
    end

    # Builds and returns an itemsonly dictionary with items from this items view.
    def collect : Dict
      if @b == 0 && @e == @dict.size
        return @dict
      end

      Dict.build(itemsonly: true) do |commit|
        each do |item|
          commit.assoc(Term.of(commit.size), item)
        end
      end
    end

    # Yields each successive item from this items view.
    def each(& : Term ->) : Nil
      (@b...@e).each { |index| yield @dict[index] }
    end

    # Yields each consecutive item from this items view until the block returns
    # `false`, or no more items remain. Returns the view containing the remaining
    # of the items (empty in case no more items remain).
    #
    # ```text
    # D                      1 2 3 4 -5 -6 -7
    # V1                    [                 .]
    # V1.walk(&.positive?)   T T T T F (stop)
    # => [-5 -6 -7 .]
    # ```
    def walk(& : Term -> Bool) : ItemsView
      rest = self
      while item = rest.first?
        break unless yield item
        rest = rest.move(1)
      end
      rest
    end

    # Extends this items view up to the beginning of *other* (but excluding
    # the beginning!)
    #
    # Raises `ArgumentError` if the underlying dictionaries of this and *other*
    # differ by value.
    def upto(other : ItemsView) : ItemsView
      unless @dict == other.@dict
        raise ArgumentError.new
      end

      ItemsView.new(@dict, @b, Math.min(other.@b, @e))
    end

    # Passes each consecutive item from this items view through the block,
    # collecting block return values in an array until the block returns `nil`,
    # or no more items are left. Returns the resulting array followed by the view
    # of the remaining items.
    #
    # If the block returned `nil` immediately no array is allocated to save on
    # allocations. Instead, `nil` is returned instead of the array.
    def thru(& : Term -> T?) : {Array(T)?, ItemsView} forall T
      rest = self
      objects = nil
      while item = rest.first?
        break unless object = yield item
        objects ||= [] of T
        objects << object
        rest = rest.move(1)
      end
      {objects, rest}
    end

    # Same as `thru(&)`, but casts the incoming term to `T` before passing it
    # to the block. If the cast is unsuccessful, `thru` ends as if the block
    # returned `nil` (see `thru(&)` for details).
    def thru(cls : T.class, & : T -> U?) : {Array(U)?, ItemsView} forall T, U
      thru do |term|
        next unless term = term.downcast.as?(T)
        yield term
      end
    end

    # Splits this view into *n* equally-sized (if possible, otherwise left-
    # leaning) subviews.
    def split(n : Int32, & : ItemsView, Int32 ->) : Nil
      return if n.zero?
      return if size < n

      step, rem = size.divmod(n)
      from = @b
      n.times do |i|
        to = from + step - 1
        to += 1 if i < rem
        yield ItemsView.new(@dict, from, to + 1), i
        from = to + 1
      end
    end

    def inspect(io)
      io << "ItemsView("
      collect.inspect(io)
      io << ")"
    end

    def_equals_and_hash @dict, @b, @e
  end
end
