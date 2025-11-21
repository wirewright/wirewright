module Ww
  # Represents a number. Uses `Int32` under the hood; when the value exceeds `Int32`'s
  # bounds or if it is not an integer, starts using `BigRational`.
  @[Term::Assoc(TermType::Number, :unsafe_as_n)]
  struct Term::Num
    include AutoUpcast
    include TypeConversion
    include Comparable(Num)
    include Comparable(Number)

    EPS = Term[0.001]

    struct ::Number
      include Comparable(::Ww::Term::Num)

      def <=>(other : ::Ww::Term::Num)
        ::Ww::Term::Num.new(self) <=> other
      end
    end

    def initialize(@k : Kernel)
    end

    # Constructs a new number term from the given *object*.
    def self.new(object : Number) : Num
      new(Kernel.from(object))
    end

    def self.zero : Num
      new(0)
    end

    @[Dncast]
    def <=>(other : self)
      Kernel.cmp(@k, other.@k)
    end

    @[Dncast]
    def <=>(other : Number)
      Kernel.cmp(@k, Kernel.from(other))
    end

    private def norm(object : Num) : Num
      object
    end

    private def norm(object) : Num
      Term.of(object).as_n
    end

    # Tries to convert this number to an `Int32`.
    #
    # May raise `OverflowError` if the conversion overflows.
    @[Dncast]
    def to_i : Int32
      Kernel.to_i(@k)
    end

    # Convert this number to a `Float64`.
    @[Dncast]
    def to_f64 : Float64
      Kernel.to_f64(@k)
    end

    # Converts this number to a `BigRational`.
    @[Dncast]
    def to_big_r : BigRational
      Kernel.to_big_r(@k)
    end

    private def convert?(type : T.class) forall T
      T.new(@k.v)
    rescue OverflowError
    end

    def to?(type : Number.class)
      convert?(type)
    end

    # WARNING: only enums up to Int32 are supported at the moment.
    def to?(type : Enum.class)
      type.from_value?(to_i)
    end

    # Converts this number to an `Int32` if this can be done quickly
    # (if the conversion is possible at all). Otherwise returns `nil`.
    #
    # Prefer to use `to?(Int32)` unless it's a *very* internal and performance-
    # sensitive place.
    @[Dncast]
    def fast_to_i32? : Int32?
      @k.int? ? @k.unsafe_as_i : nil
    end

    # :nodoc:
    def unsafe_as_i : Int32
      @k.unsafe_as_i
    end

    # Returns `true` if this number is zero.
    @[Dncast]
    def zero? : Bool
      Kernel.zero?(@k)
    end

    # Returns `true` if this number is positive.
    #
    # *Zero is considered positive.*
    @[Dncast]
    def positive? : Bool
      self >= 0
    end

    # Returns `true` if this number is negative.
    @[Dncast]
    def negative? : Bool
      self < 0
    end

    # Returns `true` if this number is a natural number.
    @[Dncast]
    def natural? : Bool
      whole? && positive?
    end

    @[Dncast]
    def natural_nonzero? : Bool
      natural? && !zero?
    end

    # Returns `true` if this number is an integer.
    @[Dncast]
    def whole? : Bool
      Kernel.integer?(@k)
    end

    # Returns `true` if this number is finite.
    @[Dncast]
    def finite? : Bool
      Kernel.finite?(@k)
    end

    # Returns `true` if this number is divisible by *other*.
    @[Dncast]
    def divisible_by?(other) : Bool
      Kernel.divisible_by?(@k, norm(other).@k)
    end

    # Returns `true` if this number is approximately equal to *other*.
    #
    # Epsilon (maximum distance) is given by *eps*.
    @[Dncast]
    def approx?(other, eps : Num = EPS) : Bool
      (self - other).abs <= eps
    end

    # Returns the absolute value of this number.
    @[Dncast]
    def abs : Num
      negative? ? self * -1 : self
    end

    # Returns the reciprocal (multiplicative inverse) of this number.
    @[Dncast]
    def reciprocal : Num
      Num.new(Kernel.reciprocal(@k))
    end

    # Yields each factor of this number in no particular order.
    #
    # Yields nothing if this number isn't an integer.
    @[Dncast]
    def each_factor(& : Num ->) : Nil
      Kernel.each_factor(@k) { |factor| yield Num.new(factor) }
    end

    # Returns the sum of `self` and *other*.
    @[Dncast]
    def +(other) : Num
      Num.new(Kernel.add(@k, norm(other).@k))
    end

    # Returns the negative of `self`.
    @[Dncast]
    def - : Num
      Num.new(Kernel.neg(@k))
    end

    # Returns the difference between `self` and *other*.
    @[Dncast]
    def -(other) : Num
      self + -norm(other)
    end

    # Returns the product of `self` and *other*.
    @[Dncast]
    def *(other) : Num
      Num.new(Kernel.mul(@k, norm(other).@k))
    end

    @[Dncast]
    def /(other) : Num
      other = norm(other)
      raise DivisionByZeroError.new if other.zero?

      Num.new(Kernel.div(@k, other.@k))
    end

    # Returns the quotient from the division of `self` by *other*
    #
    # If *other* is `0`, raises `DivisionByZeroError`.
    @[Dncast]
    def //(other) : Num
      other = norm(other)
      raise DivisionByZeroError.new if other.zero?

      Num.new(Kernel.idiv(@k, other.@k))
    end

    # Returns the remainder from the division of `self` by *other*.
    #
    # If *other* is `0`, raises `DivisionByZeroError`.
    @[Dncast]
    def %(other) : Num
      other = norm(other)
      raise DivisionByZeroError.new if other.zero?

      Num.new(Kernel.mod(@k, other.@k))
    end

    # Raises `self` to *other* power.
    #
    # May raise `MathDomainError` if it is impossible to raise `self` to *other* power.
    @[Dncast]
    def **(other) : Num
      Num.new(Kernel.pow(@k, norm(other).@k))
    end

    # Shorthand for `self + n`. Most useful when using the block shorthand syntax.
    @[Dncast]
    def succ(n = 1) : Num
      self + n
    end

    # Shorthand for `self - n`. Most useful when using the block shorthand syntax.
    @[Dncast]
    def pred(n = 1) : Num
      self - n
    end

    # Returns the square root of `self`.
    #
    # If negative, raises `MathDomainError`.
    @[Dncast]
    def sqrt : Num
      raise MathDomainError.new if negative?

      Num.new(Kernel.sqrt(@k))
    end

    # Integer square root
    @[Dncast]
    def isqrt : Num
      raise MathDomainError.new if negative?

      Num.new(Kernel.isqrt(@k))
    end

    @[Dncast]
    def floor
      Num.new(Kernel.floor(@k))
    end

    @[Dncast]
    def round
      Num.new(Kernel.round(@k))
    end

    @[Dncast]
    def ceil
      Num.new(Kernel.ceil(@k))
    end

    @[Dncast]
    def uszpair(other : Num)
      unless positive? && other.positive?
        raise ArgumentError.new
      end

      self >= other ? self**2 + self + other : self + other**2
    end

    @[Dncast]
    def uszunpair : {Num, Num}
      unless positive?
        raise ArgumentError.new
      end

      t1 = sqrt.floor
      t2 = self - t1**2
      t2 < t1 ? {t2, t1} : {t1, t2 - t1}
    end

    {% for name in %w(sin cos tan) %}
      @[Dncast]
      def {{name.id}} : Num
        Num.new(Kernel.{{name.id}}(@k))
      end
    {% end %}

    # Appends *digit* in *radix* to a copy of this number. Returns the copy.
    @[Dncast]
    def append(digit : Num, *, radix : Num) : Num
      self * radix + digit
    end

    # :nodoc:
    def inspect(io)
      Kernel.inspect(io, @k)
    end
  end
end

require "./num/kernel"
