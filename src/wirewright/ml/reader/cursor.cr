struct Ww::ML::Reader
  # Cursor over a lexeme slice, skipping ignored tokens. Maintains a sliding window
  # (`behind` and `ahead`) between significant lexemes. Assumes BOI/EOI framing.
  class Cursor
    @ahead_index : Int32

    def initialize(@lexemes : Slice(Lexeme::Atom))
      unless (fst = @lexemes[0]?) && (lst = @lexemes[-1]?)
        raise ArgumentError.new("expected a slice containing at least two lexemes (BOI and EOI)")
      end

      unless fst.is_a?(Lexeme::Token) && fst.type.boi?
        raise ArgumentError.new("expected first lexeme to be BOI")
      end

      unless lst.is_a?(Lexeme::Token) && lst.type.eoi?
        raise ArgumentError.new("expected last lexeme to be EOI")
      end

      @behind_index = 0 # BOI
      @ahead_index = find(1)
    end

    # Returns `true` if *lexeme* is generally ignored by term reader cursors.
    # Returns `false` otherwise.
    def self.ignores?(lexeme : Lexeme::Token) : Bool
      case lexeme.type
      when .line_comment?, .blank_line?, .double_blank_line?, .white_rectangle?
        true
      else
        false
      end
    end

    # :ditto:
    def self.ignores?(lexeme : Lexeme::Datum) : Bool
      false
    end

    private def find(start : Int32) : Int32
      while start < @lexemes.size
        target = @lexemes[start]

        unless Cursor.ignores?(target)
          return start
        end

        start += 1
      end

      raise IndexError.new
    end

    # Returns the lexeme immediately preceding the cursor.
    def behind : Lexeme::Atom
      @lexemes.unsafe_fetch(@behind_index)
    end

    # Returns the lexeme immediately following the cursor.
    def ahead : Lexeme::Atom
      @lexemes.unsafe_fetch(@ahead_index)
    end

    # Moves the cursor past the lexeme ahead. Returns `true` if moved; or `false`
    # if did not (EOI ahead).
    def forward : Bool
      unless @ahead_index + 1 < @lexemes.size
        return false
      end

      @behind_index = @ahead_index
      @ahead_index = find(@ahead_index + 1)

      true
    end

    # Returns the state tuple of this cursor. It can be passed to `restore`
    # later, to restore the state to one at the time of calling this method.
    def state
      {@behind_index, @ahead_index}
    end

    # Restores the state of this cursor to the state at the time of calling `state`.
    def restore(state)
      @behind_index, @ahead_index = state
    end

    # Yields ignored tokens between `behind` and `ahead`, starting from the one
    # following `behind` and moving rightwards.
    def each_previous_ignored(& : Lexeme::Atom ->)
      (@behind_index + 1...@ahead_index).each do |index|
        yield @lexemes.unsafe_fetch(index)
      end
    end

    # Yields ignored tokens between `behind` and `ahead`, starting from the one
    # preceding `ahead` and moving leftwards.
    def reverse_each_previous_ignored(& : Lexeme::Atom ->)
      (@behind_index + 1...@ahead_index).reverse_each do |index|
        yield @lexemes.unsafe_fetch(index)
      end
    end

    def ignored?(& : Lexeme::Atom -> Bool) : Bool
      each_previous_ignored do |lexeme|
        return true if yield lexeme
      end

      false
    end
  end
end
