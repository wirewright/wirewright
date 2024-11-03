module Ww
  struct Term::Num::Kernel
    enum Tag : UInt8
      NumRat = Term::Tag::NumRat
      NumInt = Term::Tag::NumInt
    end

    def initialize(@mem : Void*)
    end

    def rat? : Bool
      tag.num_rat?
    end

    def int? : Bool
      tag.num_int?
    end

    def tag : Tag
      Tag.new((@mem.address & 0b111).to_u8)
    end

    def unsafe_as_i : Int32
      (@mem.address >> 3).unsafe_as(Int32)
    end

    def unsafe_as_rat : BigRational
      Box(BigRational).unbox(Pointer(Void).new(@mem.address >> 3 << 3))
    end

    def v : Int32 | BigRational
      case tag
      in .num_rat? then unsafe_as_rat
      in .num_int? then unsafe_as_i
      end
    end
  end

  struct Term::Num::Kernel
    {% for cls in %w(UInt8 Int8 UInt16 Int16) %}
      # Constructs an integer-typed kernel.
      def self.from(object : {{cls.id}})
        from(object.to_i)
      end
    {% end %}

    # Constructs an integer-typed kernel.
    def self.from(object : Int32)
      new(Pointer(Void).new((object.unsafe_as(UInt64) << 3) | Tag::NumInt.value))
    end

    # Constructs a rational-typed kernel.
    def self.from(object : BigRational)
      new(Pointer(Void).new(Box.box(object).address | Tag::NumRat.value))
    end

    def self.from(object : Int)
      if Int32::MIN <= object <= Int32::MAX
        return from(object.to_i)
      end

      from(object.to_big_d.to_big_r)
    end

    # Constructs a rational-typed kernel.
    def self.from(object : Float)
      from(object.to_big_d.to_big_r)
    end

    def self.from(object : BigDecimal)
      from(object.to_big_r)
    end

    def self.to_i(k : Kernel)
      case k.tag
      in .num_int?
        k.unsafe_as_i
      in .num_rat?
        k.unsafe_as_rat.to_f.to_i
      end
    end

    def self.to_big_r(k : Kernel)
      case k.tag
      in .num_int?
        k.unsafe_as_i.to_big_r
      in .num_rat?
        k.unsafe_as_rat
      end
    end

    def self.upcast(n : Kernel)
      n.rat? ? n : from(n.unsafe_as_i.to_big_r)
    end

    def self.eqcast(n : Kernel, ref : Kernel)
      ref.rat? ? upcast(n) : n
    end

    def self.zero?(a : Kernel) : Bool
      a.v.zero?
    end

    def self.cmp(a : Kernel, b : Kernel)
      x = eqcast(a, b)
      y = eqcast(b, a)
      x.v <=> y.v
    end

    def self.integer?(a : Kernel)
      a.int? || a.unsafe_as_rat.integer?
    end

    def self.finite?(a : Kernel)
      return true if integer?(a)

      rat = a.unsafe_as_rat
      rat.denominator.each_prime_factor do |factor|
        next if factor.in?(2, 5)
        return false # Any factor other than two or five means it's infinite.
      end

      true
    end

    def self.neg(a : Kernel)
      from(-a.v)
    end

    def self.add(a : Kernel, b : Kernel)
      a = eqcast(a, b)
      b = eqcast(b, a)
      begin
        from(a.v + b.v)
      rescue OverflowError
        add(upcast(a), upcast(b))
      end
    end

    def self.mul(a : Kernel, b : Kernel)
      a = eqcast(a, b)
      b = eqcast(b, a)
      begin
        from(a.v * b.v)
      rescue OverflowError
        mul(upcast(a), upcast(b))
      end
    end

    def self.div(a : Kernel, b : Kernel)
      if a.int? && b.int? && zero?(mod(a, b))
        return from(a.unsafe_as_i // b.unsafe_as_i)
      end

      a = a.v.to_big_r
      b = b.v.to_big_r
      from(a/b)
    end

    def self.idiv(a : Kernel, b : Kernel)
      from(eqcast(a, b).v // eqcast(b, a).v)
    end

    def self.mod(a : Kernel, b : Kernel)
      if a.int? && b.int?
        from(a.unsafe_as_i % b.unsafe_as_i)
      elsif a.rat? && b.rat?
        from(a.unsafe_as_rat % b.unsafe_as_rat)
      else
        mod(eqcast(a, b), eqcast(b, a))
      end
    end

    def self.pow(a : Kernel, b : Kernel)
      if a.int? && b.int?
        begin
          return from(a.unsafe_as_i ** b.unsafe_as_i)
        rescue OverflowError
        end
      end

      a = upcast(a)
      b = upcast(b)

      if integer?(b)
        return from(a.unsafe_as_rat ** b.unsafe_as_rat.to_big_i)
      end

      # This is the best we can do right now. Fix needed!
      result = to_f64(a) ** to_f64(b)

      raise MathDomainError.new unless result.finite?

      from(result)
    end

    def self.sqrt(a : Kernel)
      from(Math.sqrt(a.v).to_big_r)
    end

    def self.isqrt(a : Kernel)
      if a.int?
        from(Math.isqrt(a.unsafe_as_i))
      else
        from(Math.isqrt(a.unsafe_as_rat.to_big_i))
      end
    end

    def self.sin(a : Kernel)
      from(Math.sin(a.v))
    end

    def self.cos(a : Kernel)
      from(Math.cos(a.v))
    end

    def self.tan(a : Kernel)
      from(Math.tan(a.v))
    end

    def self.floor(a : Kernel)
      from(a.v.floor)
    end

    def self.divisible_by?(a : Kernel, b : Kernel)
      return false if zero?(b) || !integer?(b)
      return true if zero?(a)

      integer?(div(a, b))
    end

    def self.reciprocal(a : Kernel)
      div(from(1), a)
    end

    def self.each_factor(a : Kernel, &)
      if a.int?
        (1..Math.isqrt(a.unsafe_as_i)).each do |i|
          quo, rem = a.v.divmod(i)
          next unless rem.zero?
          yield from(i)
          yield from(quo) unless quo == i
        end
      else # rat
        return unless integer?(a)

        int = a.unsafe_as_rat.to_big_i

        (1..Math.isqrt(int)).each do |i|
          quo, rem = int.divmod(i)
          next unless rem.zero?
          yield from(i.to_big_r)
          yield from(quo.to_big_r) unless quo == i
        end
      end
    end

    def self.to_f64(a : Kernel)
      a.v.to_f64
    end

    def self.to_i(a : Kernel)
      to_f64(a).to_i
    end

    def self.inspect(io, a : Kernel)
      if a.int?
        io << a.unsafe_as_i
        return
      end

      rat = a.unsafe_as_rat

      unless Kernel.finite?(a)
        rat.to_s(io)
        return
      end

      if Kernel.integer?(a)
        rat.to_big_i.inspect(io)
      else
        rat.to_big_d.inspect(io)
      end
    end

    def self.serialize(a : Kernel)
      String.build do |io|
        if a.int?
          io << "I " << a.unsafe_as_i
          next
        end

        rat = a.unsafe_as_rat

        io << "R " << rat.numerator << " " << rat.denominator
      end
    end
  end
end
