module Ww
  # Raised by `Term::Num` on certain math errors, such as taking the square
  # root of a negative number.
  class MathDomainError < Exception
  end

  # Represents a number term.
  #
  # Wirewright's numbers can be *exact* or *approximate*. *Approximate*-ness (also
  # known as *inexactness*, in e.g. Scheme) is "contagious": arithmetic featuring
  # approximate numbers always results in an approximate number. Approximate numbers
  # are marked with the `≈` prefix in WwML, both in input and display. For example,
  # `≈100` and `≈1/3` are approximate numbers, whereas `100` and `1/3` are exact numbers.
  #
  # Wirewright's numeric tower consists of three levels:
  #
  # - *exact* 61-bit signed integer (known as `i61`, `Int61`, arithmetic done using `Int64`).
  # - *exact* rational (known as `rat`, arithmetic done using `BigRational`).
  # - *approximate* 61-bit float (known as `f61`, arithmetic done using `Float64`).
  #
  # 61-bit signed integer is used because `Term` can only fit 61 bits in the pointer;
  # the three remaining bits it uses as a tag. See `Term` for more info on how term
  # instances are encoded.
  #
  # Outside of `Term`, `Num` is a tagged union; it occupies 16 bytes (4 bytes for type
  # id, 4 bytes padding, and 8 bytes for the number itself). The number is stored as
  # an `Int64`, a `Float64`, or a pointer to `BigRational`.
  #
  # `BigRational` is behind a pointer because it is a large struct, occupying 32 bytes.
  # Most numbers will never be rationals, so it would be a waste of space to include those
  # 32 bytes in the size of `Term::Num`.
  #
  # Inspired by [R7RS](https://small.r7rs.org/attachment/r7rs.pdf).
  @[Term::Assoc(TermType::Number, :unsafe_as_n)]
  struct Term::Num
    include AutoUpcast
    include TypeConversion
    include Equality
    include Comparable(Num)
    include Comparable(Number)

    # :nodoc:
    alias Kernel = Exact | Approx
    # :nodoc:
    alias Exact = Int64 | BigRational*
    # :nodoc:
    alias Approx = Float64

    # Utilities and constants for working with i61 numbers.
    module Int61
      # :nodoc:
      #
      # Mask for bits 0...61 (61 is the sign bit)
      MASK = (1u64 << 61) &- 1

      # :nodoc:
      #
      # Mask for bit 61 (sign bit)
      SIGN = 1u64 << 60

      # Minimum value for an `Int61`.
      MIN = -(2i64**60)
      # Maximum value for an `Int61`.
      MAX = 2i64**60 - 1

      # Returns i61 bits for an i64 *n*.
      #
      # Raises `OverflowError` in case of overflow.
      def self.bits(n : Int64) : UInt64
        unless MIN <= n <= MAX
          raise OverflowError.new
        end

        n.unsafe_as(UInt64) & MASK
      end

      # Returns an i64 decoded from u64 *n*.
      def self.value(bits : UInt64) : Int64
        if (bits & SIGN) > 0
          return (bits | ~MASK).unsafe_as(Int64)
        end

        bits.unsafe_as(Int64)
      end
    end

    # Utilities and constants for working with f61 numbers.
    #
    # Three least significant bits of the mantissa are omitted, rounding to
    # nearest, ties to even.
    #
    # NOTE: This was written based on suggestions from ChatGPT. I have absolutely
    # no idea what I'm doing here wrt floating-point semantics.
    module Float61
      extend self

      # Returns f61 bits for an f64 *n*.
      def bits(value : Float64) : UInt64
        raw = value.unsafe_as(UInt64)
        trunk = raw & ~0b111u64
        lost = raw & 0b111u64

        # Round to nearest, ties to even.
        if lost > 0b100u64 || (lost == 0b100 && ((trunk >> 3) & 1))
          trunk &+= 0b1000
        end

        trunk >> 3
      end

      # Returns an f64 decoded from f61 *bits*.
      def value(bits : UInt64) : Float64
        (bits << 3).unsafe_as(Float64)
      end
    end

    # This constructor is private to make sure `Num`s are constructed by `.exact`
    # or `.approx` exclusively, which ensures @k is always the fittest representation.
    # Moreover, we must also ensure that if we choose @k : Int64, it's in i61's
    # bounds, because that's what `Term` can encode.
    private def initialize(@k : Kernel)
    end

    # :nodoc:
    def self.unsafe_new(k : Kernel)
      new(k)
    end

    # Constructs a rational number from Crystal number numerator *num* and
    # denominator *den*.
    def self.exact(num, den) : Num
      exact(BigRational.new(num, den))
    end

    # Constructs an exact number term from a Crystal number *n*.
    def self.exact(n : Float) : Num
      unless n.finite? # Infinite or NaN
        raise MathDomainError.new
      end

      if n.integer? && Int61::MIN <= n <= Int61::MAX
        return new(n.to_i64)
      end

      exact(n.to_big_r)
    end

    # :ditto:
    def self.exact(n : BigRational) : Num
      if n.integer? && Int61::MIN <= n <= Int61::MAX
        # Even though it's rat, it fits, so let's use Int61 because it could
        # be cheaper in the long run, assuming we're not thrashing near
        # the threshold.
        return new(n.numerator.to_i64)
      end

      ratptr = Pointer(BigRational).malloc(1)
      ratptr.value = n

      new(ratptr)
    end

    # :ditto:
    def self.exact(n : UInt8 | UInt16 | UInt32 | Int8 | Int16 | Int32) : Num
      exact(n.to_i64)
    end

    # :ditto:
    def self.exact(n : Int64) : Num
      if Int61::MIN <= n <= Int61::MAX
        return new(n)
      end

      exact(n.to_big_r)
    end

    # :ditto:
    def self.exact(n : UInt64 | UInt128 | Int128 | BigInt) : Num
      exact(n.to_big_r)
    end

    # Converts an approximate number term *n* into an exact number term
    # (if *n* is approximate; otherwise, does not change *n*).
    def self.exact(n : Num) : Num
      n.kmap { |k| exact(k) }
    end

    # Constructs an approximate number term from a Crystal float *n*.
    def self.approx(n : Float)
      unless n.finite? # Infinite or NaN
        raise MathDomainError.new
      end

      new(n.to_f64)
    end

    # Converts another number term *n* into an approximate number term.
    def self.approx(n : Num)
      approx(f64(n.@k))
    end

    # Returns the number zero.
    def self.zero : Num
      exact(0i64)
    end

    # :nodoc:
    @[AlwaysInline]
    def self.f64(n : Int64) : Float64
      n.to_f64
    end

    # :nodoc:
    @[AlwaysInline]
    def self.f64(n : BigRational*) : Float64
      n.value.to_f64
    end

    # :nodoc:
    @[AlwaysInline]
    def self.f64(n : Float64) : Float64
      n
    end

    # :nodoc:
    @[AlwaysInline]
    def self.rat(n : Int64) : BigRational
      n.to_big_r
    end

    # :nodoc:
    @[AlwaysInline]
    def self.rat(n : BigRational*) : BigRational
      n.value
    end

    protected def kmap(&)
      k = @k
      if k.is_a?(BigRational*)
        return yield k.value
      end

      # k : Int64 | Float64
      yield k
    end

    # Compares two number terms.
    #
    # - For exact numbers, this always gives the true result.
    # - For inexact numbers, the result depends on the bits of the floating-point
    #   representation. Here in particular we use `Float64`. This method does *not*
    #   perform approximate equality.
    @[Dncast]
    def <=>(other : Num)
      kmap { |a| other.kmap { |b| a <=> b } }
    end

    # Compares a number term and a Crystal number.
    @[Dncast]
    def <=>(other : Number)
      kmap { |a| a <=> other }
    end

    # Returns the decimal representation of this number term.
    def to?(type : String) : String?
      decimal
    end

    # Converts this number term to a Crystal number.
    def to?(type : Number.class)
      to_number?(type)
    end

    # Converts this number term to a Crystal enum.
    #
    # WARNING: only enums up to Int64 are supported at the moment.
    def to?(type : Enum.class)
      return unless value = to?(Int64)

      type.from_value?(value)
    end

    private def to_number?(type : T.class) : T? forall T
      kmap { |a| T.new(a) }
    rescue OverflowError
    end

    # Returns `true` if this number term is zero. Returns `false` otherwise.
    #
    # See also: `#<=>(other : Num)`.
    @[Dncast]
    def zero? : Bool
      kmap(&.zero?)
    end

    # Returns `true` if this number term is greater than or equal to zero.
    # Returns `false` otherwise.
    #
    # NOTE: Zero is considered positive.
    @[Dncast]
    def positive? : Bool
      zero? || kmap(&.positive?)
    end

    # Returns `true` if this number term is less than zero. Returns
    # `false` otherwise.
    @[Dncast]
    def negative? : Bool
      kmap(&.negative?)
    end

    # Returns `true` if this number term uses an exact representation, and
    # exact arithmetic is applied to it. Returns `false` otherwise.
    @[Dncast]
    def exact? : Bool
      @k.is_a?(Exact)
    end

    # Returns `true` if this number term uses an approximate representation, and
    # approximate arithmetic is applied to it. Returns `false` otherwise.
    @[Dncast]
    def approx? : Bool
      @k.is_a?(Approx)
    end

    # Returns `true` if this number term is a positive integer. Returns
    # `false` otherwise.
    #
    # NOTE: 0 is considered a natural number by this method.
    @[Dncast]
    def natural? : Bool
      positive? && integer?
    end

    # Returns `true` if this number term is a positive nonzero integer.
    # Returns `false` otherwise.
    @[Dncast]
    def natural_nonzero? : Bool
      natural? && !zero?
    end

    # Returns `true` if this number term is a whole number. Returns
    # `false` otherwise.
    #
    # NOTE: Like R7RS, this method returns `true` on an inexact number if
    # it is an integer (see also: `Float64#integer?`).
    @[Dncast]
    def integer? : Bool
      kmap(&.integer?)
    end

    # Returns `true` if this number term is a whole *exact* number. Returns
    # `false` otherwise (always for inexact numbers, even if they look exact,
    # e.g. `≈10`).
    @[Dncast]
    def exact_integer? : Bool
      @k.is_a?(Exact) && integer?
    end

    # Returns `true` if this number term has a finite decimal representation.
    # Returns `false` if it does not.
    @[Dncast]
    def finite10? : Bool
      unless ratptr = @k.as?(BigRational*)
        return true # finite, @k : Int64 | Float64
      end

      ratptr.value.denominator.each_prime_factor do |factor|
        next if factor.in?(2, 5)
        return false # infinite: any factor other than two and five means it's infinite.
      end

      true # finite
    end

    # :nodoc:
    def divisible_by?(other : Num) : Bool
      return false if other.zero? # 0/0 123/0
      return true if zero?        # 0/123

      (self / other).integer?
    end

    # Returns `true` if the result of dividing this number by *other*
    # is an integer. Returns `false` otherwise.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    @[Dncast]
    def divisible_by?(other) : Bool
      divisible_by?(Term[other].as(Num))
    end

    # Returns the absolute value of this number term.
    @[Dncast]
    def abs : Num
      case a = @k
      in Int64                then Num.exact(a.abs)
      in Pointer(BigRational) then Num.exact(a.value.abs)
      in Float64              then Num.approx(a.abs)
      end
    end

    # :nodoc:
    macro reduce(n, m, &block)
      case { %a = {{n}}.@k, %b = {{m}}.@k }
      in {Int64, Int64}
        begin
          %c = pass(%a, %b) {{block}}
        rescue OverflowError
          %c = pass(%a.to_big_r, %b.to_big_r) {{block}}
        end

        Num.exact(%c)
      in {Pointer(BigRational), Int64}
        %c = pass(%a.value, %b.to_big_r) {{block}}

        Num.exact(%c)
      in {Int64, Pointer(BigRational)}
        %c = pass(%a.to_big_r, %b.value) {{block}}

        Num.exact(%c)
      in {Pointer(BigRational), Pointer(BigRational)}
        %c = pass(%a.value, %b.value) {{block}}

        Num.exact(%c)
      in {_, Float64}, {Float64, _}
        %c = pass({{@type}}.f64(%a), {{@type}}.f64(%b)) {{block}}

        Num.approx(%c)
      end
    end

    # :nodoc:
    def +(other : Num) : Num
      reduce(self, other) { |a, b| a + b }
    end

    # Returns the sum of this and *other* number terms.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    @[Dncast]
    def +(other) : Num
      self + Term[other].as(Num)
    end

    # Returns the negative of this number term.
    @[Dncast]
    def - : Num
      Num.zero - self
    end

    # :nodoc:
    def -(other : Num) : Num
      reduce(self, other) { |a, b| a - b }
    end

    # Returns the difference of this and *other* number terms.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    @[Dncast]
    def -(other)
      self - Term[other].as(Num)
    end

    # :nodoc:
    def *(other : Num) : Num
      reduce(self, other) { |a, b| a * b }
    end

    # Returns the product of this and *other* number terms.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    @[Dncast]
    def *(other)
      self * Term[other].as(Num)
    end

    # :nodoc:
    def /(other : Num) : Num
      if other.zero?
        raise DivisionByZeroError.new
      end

      case {a = @k, b = other.@k}
      in {Int64, Int64}
        # Assume a divisibility check will be less expensive here than allocating
        # BigRational and downcasting.
        if a % b == 0
          # NOTE: Int64::MIN // -1 overflows, and so does Int61::MIN / -1, but we always have
          # a : Int61, b : Int61, and division is always done in Int64 (NOT in Int61), so
          # the overflow cannot happen.
          return Num.exact(a // b)
        end

        Num.exact(a.to_big_r / b.to_big_r)
      in {Int64, Pointer(BigRational)}
        Num.exact(a.to_big_r / b.value)
      in {Pointer(BigRational), Int64}
        Num.exact(a.value / b.to_big_r)
      in {Pointer(BigRational), Pointer(BigRational)}
        Num.exact(a.value / b.value)
      in {Float64, _}, {_, Float64}
        Num.approx(Num.f64(a) / Num.f64(b))
      end
    end

    # Returns the quotient of this and *other* number terms.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    #
    # Raises `DivisionByZeroError` on division by zero.
    @[Dncast]
    def /(other)
      self / Term[other].as(Num)
    end

    # :nodoc:
    def //(other : Num) : Num
      if other.zero?
        raise DivisionByZeroError.new
      end

      reduce(self, other) { |a, b| a // b }
    end

    # Returns the result of integer division for this and *other* number terms.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    #
    # Raises `DivisionByZeroError` if *other* is zero.
    @[Dncast]
    def //(other)
      self // Term[other].as(Num)
    end

    # :nodoc:
    def %(other : Num) : Num
      if other.zero?
        raise DivisionByZeroError.new
      end

      reduce(self, other) { |a, b| a % b }
    end

    # Returns remainder on division of this number term by *other*.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    #
    # Raises `DivisionByZeroError` if *other* is zero.
    @[Dncast]
    def %(other)
      self % Term[other].as(Num)
    end

    # :nodoc:
    def **(other : Num) : Num
      a, b = @k, other.@k

      # Fast path: both are integers, b is positive.
      if a.is_a?(Int64) && b.is_a?(Int64) && b >= 0
        begin
          c = a ** b
        rescue OverflowError
        else
          return Num.exact(c)
        end
      end

      # Approx path.
      if a.is_a?(Float64) || b.is_a?(Float64)
        return Num.approx(Num.f64(a) ** Num.f64(b))
      end

      # Rat path.
      if b.is_a?(Int64)
        return Num.exact(Num.rat(a) ** b)
      end

      Num.approx(Num.f64(a) ** Num.f64(b))
    end

    # Returns the result of raising this number to the power of *other*.
    #
    # *other* is passed through `Term.[]` to obtain a number term.
    #
    # Switches to approximate arithmetic if both numbers are rational. Raises
    # `MathDomainError` if approximate arithmetic evaluates to infinity
    # or NaN.
    @[Dncast]
    def **(other)
      self ** Term[other].as(Num)
    end

    # Returns the previous integer.
    #
    # Defined for integration with `Range`.
    #
    # This number must be an exact integer. Raises `MathDomainError` otherwise.
    @[Dncast]
    def pred : Num
      unless exact_integer?
        raise MathDomainError.new
      end

      self - Num.exact(1)
    end

    # Returns the next integer.
    #
    # Defined for integration with `Range`.
    #
    # This number must be an exact integer. Raises `MathDomainError` otherwise.
    @[Dncast]
    def succ : Num
      unless exact_integer?
        raise MathDomainError.new
      end

      self + Num.exact(1)
    end

    # Returns the approximate square root of this number term.
    #
    # Raises `MathDomainError` if this number is negative.
    @[Dncast]
    def sqrt : Num
      if negative?
        raise MathDomainError.new
      end

      Num.approx(Math.sqrt(f64(@k)))
    end

    # Returns the approximate integer square root of this number term.
    #
    # For non-integer number terms, this method returns the approximate floor
    # of their square root.
    #
    # Raises `MathDomainError` if this number is negative.
    @[Dncast]
    def isqrt : Num
      if negative?
        raise MathDomainError.new
      end

      case a = @k
      in Int64
        Num.exact(Math.isqrt(a))
      in Pointer(BigRational), Float64
        # Math.sqrt(BigRational) : BigFloat, so let's just do it on f64, I don't
        # see the point of going through BigFloat.
        Num.approx(Math.sqrt(f64(a)).floor)
      end
    end

    # Returns the greatest integer number term less than or equal to this
    # number term.
    @[Dncast]
    def floor : Num
      case a = @k
      in Int64                then self
      in Pointer(BigRational) then Num.exact(a.value.floor)
      in Float64              then Num.approx(a.floor)
      end
    end

    # Returns the integer number term closest to this number term (Banker's rounding).
    @[Dncast]
    def round : Num
      case a = @k
      in Int64                then self
      in Pointer(BigRational) then Num.exact(a.value.round(:ties_even))
      in Float64              then Num.approx(a.round(:ties_even))
      end
    end

    # Returns the smallest integer number term greater than or equal to this
    # number term.
    @[Dncast]
    def ceil : Num
      case a = @k
      in Int64                then self
      in Pointer(BigRational) then Num.exact(a.value.ceil)
      in Float64              then Num.approx(a.ceil)
      end
    end

    # Returns a number term that represents the pair of this and other number terms.
    #
    # Implements [Szudzik's pairing function](https://szudzik.com/ElegantPairing.pdf)
    @[Dncast]
    def pair(other : Num) : Num
      unless positive? && other.positive?
        raise MathDomainError.new
      end

      self >= other ? self**Num.exact(2) + self + other : self + other**Num.exact(2)
    end

    # Returns the pair of number terms that this number term represents.
    #
    # Implements [Szudzik's pairing function](https://szudzik.com/ElegantPairing.pdf)
    @[Dncast]
    def unpair : {Num, Num}
      unless positive?
        raise MathDomainError.new
      end

      t1 = sqrt.floor
      t2 = self - t1**Num.exact(2)
      t2 < t1 ? {t2, t1} : {t1, t2 - t1}
    end

    # Returns the approximate of the sine of this number term.
    @[Dncast]
    def sin : Num
      Num.approx(Math.sin(@k))
    end

    # Returns the approximate of the cosine of this number term.
    @[Dncast]
    def cos : Num
      Num.approx(Math.cos(@k))
    end

    # Returns the approximate of the tangent of this number term.
    @[Dncast]
    def tan : Num
      Num.approx(Math.tan(@k))
    end

    # Returns a copy of this number extended with a least-significant *digit*
    # in *radix*.
    @[Dncast]
    def append(digit : Num, *, radix : Num) : Num
      self * radix + digit
    end

    # Appends the decimal representation of this number to *io*.
    @[Dncast]
    def decimal(io)
      case a = @k
      in Int64, Float64
        io << a
      in Pointer(BigRational)
        unless finite10?
          io << a.value
          return
        end

        if a.value.integer?
          io << a.value.numerator
        else
          io << a.value.to_big_d
        end
      end
    end

    def inspect(io)
      ML.compact(io, self)
    end
  end
end

struct Number
  include Comparable(::Ww::Term::Num)

  def <=>(other : ::Ww::Term::Num)
    ::Ww::Term::Num.exact(self) <=> other
  end
end
