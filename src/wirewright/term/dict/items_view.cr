module Ww
  struct Term::Dict::ItemsView
    include Indexable(Term)

    def initialize(@dict : Dict, @b : Int32, @e : Int32)
      assert 0 <= @b <= @e <= @dict.itemsize # Sanity
    end

    # :nodoc:
    def_copy_with

    def size : Int32
      @e - @b
    end

    def unsafe_fetch(index : Int) : Term
      @dict[@b + index]
    end

    # Returns a view of first *n* items in this view.
    #
    # NOTE: *n* must fit.
    def first(n : Int) : ItemsView
      assert 0 <= n <= size

      copy_with(e: n)
    end

    # Returns a view of last *n* items in this view.
    #
    # NOTE: *n* must fit.
    def last(n : Int) : ItemsView
      assert 0 <= n <= size

      copy_with(b: @e - n)
    end

    # Alias of `first`. This is sometimes more readable, especially when paired
    # with `starting_at`.
    def before(n : Int) : ItemsView
      first(n)
    end

    def starting_at(n : Int) : ItemsView
      assert 0 <= n <= size

      copy_with(b: n)
    end

    # Returns an empty items view pointing at the beginning of this items view.
    #
    # ```text
    # D           1 2 3
    # V1        [      .]
    # V1.begin  [.]
    # ```
    def begin : ItemsView
      copy_with(e: @b)
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
      copy_with(b: @e)
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
      copy_with(b: (@b + delta).clamp(0..@e))
    end

    # Shorthand for `move`.
    def +(delta : Int32) : ItemsView
      move(delta)
    end

    # :ditto:
    def -(delta : Int32) : ItemsView
      move(-delta)
    end

    def grow(delta : Int32) : ItemsView
      copy_with(e: (@e + delta).clamp(@b..@dict.itemsize))
    end

    def remaining : ItemsView
      copy_with(b: @e, e: @dict.itemsize)
    end

    # Expands the view range to enclose all dictionary items.
    def expand : ItemsView
      copy_with(b: 0, e: @dict.itemsize)
    end

    def covers_fully? : Bool
      @b == 0 && @e == @dict.itemsize
    end

    # Builds and returns an itemsonly dictionary with items from this items view.
    def collect : Dict
      if covers_fully?
        if @dict.itemsonly?
          return @dict
        end

        return @dict.itemspart
      end

      Dict.build do |commit|
        each { |item| commit.with(commit.size, item) }
      end
    end

    def set : Dict
      Dict.build do |commit|
        each { |item| commit.with(item, true) }
      end
    end

    # Yields each consecutive item from this items view until the block returns
    # `false`, or no more items remain. Returns the view containing the remaining
    # items (empty in case no more items remain).
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
      unless expand == other.expand
        raise ArgumentError.new
      end

      copy_with(e: Math.min(other.@b, @e))
    end

    def past(other : ItemsView) : ItemsView
      unless expand == other.expand
        raise ArgumentError.new
      end

      copy_with(b: Math.max(other.@e, @b))
    end

    def join(other : ItemsView) : ItemsView
      unless expand == other.expand
        raise ArgumentError.new
      end

      copy_with(e: Math.max(other.@e, @e))
    end

    def thru? : {Term, ItemsView}?
      term = first?
      term ? {term, move(1)} : nil
    end

    def thru
      thru? || raise IndexError.new
    end

    def thru(n)
      rest = move(n)

      {upto(rest), rest}
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
        next unless term = Term[term].as?(T)
        yield term
      end
    end

    # Splits this view into *n* equally-sized (if possible, otherwise left-leaning)
    # subviews. Yields each subview followed by its index.
    #
    # *empty* can be specified to allow or disallow emission of empty subviews
    # (they are disallowed by default).
    def split(n : Int32, *, empty : Bool = false, & : ItemsView, Int32 ->) : Nil
      return unless n.positive?
      return unless empty || size >= n

      step, rem = size.divmod(n)
      from = @b
      n.times do |i|
        to = from + step - 1
        to += 1 if i < rem
        yield copy_with(b: from, e: to + 1), i
        from = to + 1
      end
    end

    # Returns `true` if this and *other* item views are equal by value, meaning
    # their underlying dictionaries are equal by value and the item views themselves
    # point to the same region of those dictionaries.
    def ==(other : ItemsView) : Bool
      return false unless @b == other.@b && @e == other.@e
      return true if @dict.same?(other.@dict)

      other = other.expand
      expand.each_with_index do |item, index|
        return false unless item == other[index]
      end

      true
    end

    # :nodoc:
    def ==(other) : Bool
      false
    end

    def hash(hasher)
      hasher = {@b, @e}.hash(hasher)
      expand.each_with_index do |item, index|
        hasher = {item, index}.hash(hasher)
      end
      hasher
    end

    def inspect(io)
      io << "ItemsView("
      join(io, ", ") { |item| io << item }
      io << ")"
    end
  end
end
