module Ww
  # Nullary bases are bases that do not receive any arguments. They are stored
  # as a byte representing their name ("determiner").
  record Ubase::Nullary, det : Det do
    include Ubase

    # Lists the available nullary base determiners.
    enum Det : UInt8
      # Type checks

      IsNum
      IsStr
      IsSym
      IsBoolean
      IsDict
      IsAny

      # Nullary transformations

      Sin
      Cos
      Tan

      Upcase
      Downcase
      Charcount
    end
  end

  # Unary bases are bases that receive one argument term *arg*.
  record Ubase::Unary, det : Det, arg : Term do
    include Ubase

    # Lists the available unary base determiners.
    enum Det : UInt8
      Offset
      Scale
      Pow

      # Beginless/endless ranges are currently not stored in the red-black tree,
      # and probably will never be since there's little point in overcomplicating
      # the situation so much.

      InRangeB
      InRangeBx

      InRangeE
      InRangeEx
    end
  end

  # Wraps a term *term* and instructs the unit trie that it should be matched
  # literally (similar to a hash table key).
  record Ubase::Match, term : Term do
    include Ubase
  end

  # Wraps a number term *divisor* and instructs the unit trie that it should be
  # treated as an allowed divisor.
  record Ubase::DivBy, divisor : Term::Num do
    include Ubase
  end

  # Wraps a term *key* and instructs the unit trie that the term should be
  # a literally matched key of an entry.
  record Ubase::WhereKey, key : Term do
    include Ubase
  end

  # Represents a segment range (a range where both ends are bounded). They are
  # stored differently from unbounded ranges for (slightly?) more efficient search,
  # since creating "frequency band"-like sensor/appearance receiver/transmitter
  # pairs is an expected use case; whereas open ranges are not, and thus require
  # a scan.
  record Ubase::Segment, b : Term::Num, e : Term::Num, bx : Bool, ex : Bool do
    include Ubase
  end
end
