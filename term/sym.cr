module Ww
  # Represents a symbol. Mainly for use in WwML.
  struct Term::Sym
    include ITerm

    # Represents the result of a successful interpretation of a symbol as a blank.
    #
    # This is a needlessly big struct; try not to store it on the heap. Instead
    # you can store the original symbol. It's more efficient to parse the symbol
    # anew than store and retrieve the parse.
    struct Blank
      # Returns the type associated with this blank.
      #
      # - For instance, `x_` has the type `TermType::Any`.
      # - On the other hand, `x_number` has the type `TermType::Number`, and `x_string`
      #   has the type `TermType::String`, etc.
      getter type : TermType

      # Returns the name of this blank if it has one.
      #
      # - For instance, `x_` and `x_number` are named blanks (both are named `x`).
      # - On the other hand, `_` and `_number` are unnamed blanks (both names are `nil`).
      getter? name : Sym?

      # Returns `true` if this blank is intended for matching multiple values.
      getter? poly : Bool

      # Returns `true` if this blank is intended for matching at least one value.
      getter? one : Bool

      def initialize(@type, @name, *, @poly, @one)
      end

      # Returns `true` if this blank has a type specified. Returns `false` otherwise.
      def typed? : Bool
        !@type.any?
      end

      # :nodoc:
      def inspect(io, name : Bytes)
        io.write(name) if name?
        io << '_'

        case @type
        in .any?
        in .number?  then io << "number"
        in .symbol?  then io << "symbol"
        in .string?  then io << "string"
        in .boolean? then io << "boolean"
        in .dict?    then io << "dict"
        end

        return unless poly?

        io << (one? ? '+' : '*')
      end
    end

    # TODO: split @@encode into multiple buckets hash-index them, lock individual
    # buckets not the entire hash.

    @@encode = Hash(Bytes, UInt32).new(initial_capacity: 1024)
    @@decode = Array(Bytes).new(initial_capacity: 1024)
    @@lock = Mutex.new

    # :nodoc:
    def self.encode(bytes : Bytes) : UInt32
      @@lock.synchronize do
        @@encode.put_if_absent(bytes) do
          @@decode << bytes
          @@decode.size.to_u32 - 1
        end
      end
    end

    # :nodoc:
    def self.decode(ref : UInt32) : Bytes
      @@lock.synchronize { @@decode[ref] }
    end

    # :nodoc:
    #
    #                    at least one?      blank?
    #                               v       v
    #   ... 0   0   0   0   0   0   0   0   0   0 < named?
    # <--------------   ---------       ^ poly?
    #    symbol ref     term type
    #     (25 bits)
    #
    # TODO: use bit 0 as blank?, if 0, store symbol ref (25 bits) at 1..; if 1,
    #       interpret further bits 1, 2, 3, etc. as currently. this will make most
    #       non-blanks e.g. x fit into u8 or u16 range which will help with
    #       the coming TermArray optimization which can pack terms on append;
    #       e.g. a dictionary such as (+ 1 2) will be stored as a byte array
    #       that consists of 3 bytes.
    def initialize(@spec : UInt32)
    end

    # :nodoc:
    def self.new(ref : UInt32, *, type : TermType, one = true, blank : Bool, poly : Bool, named : Bool) : Sym
      spec = ref
      spec <<= 7
      spec |= type.to_u32 << 4
      spec |= one ? 1u32 << 3 : 0u32 << 3
      spec |= poly ? 1u32 << 2 : 0u32 << 2
      spec |= blank ? 1u32 << 1 : 0u32 << 1
      spec |= named ? 1u32 : 0u32
      new(spec)
    end

    # Constructs a symbol term from the given *source* string.
    def self.new(source : String) : Sym
      if source.empty?
        raise ArgumentError.new
      end

      # Boo!

      if source.ends_with?('+')
        if source.ends_with?("r+")
          if source.ends_with?("_number+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8))
            return new(ref, type: TermType::Number, blank: true, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("l+")
          if source.ends_with?("_symbol+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8))
            return new(ref, type: TermType::Symbol, blank: true, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("g+")
          if source.ends_with?("_string+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8))
            return new(ref, type: TermType::String, blank: true, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("n+")
          if source.ends_with?("_boolean+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 9))
            return new(ref, type: TermType::Boolean, blank: true, poly: true, named: source.size > 9)
          end
        elsif source.ends_with?("t+")
          if source.ends_with?("_dict+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 6))
            return new(ref, type: TermType::Dict, blank: true, poly: true, named: source.size > 6)
          end
        elsif source.ends_with?("_+")
          ref = encode(source.unsafe_byte_slice(0, source.bytesize - 2))
          return new(ref, type: TermType::Any, blank: true, poly: true, named: source.size > 2)
        end
      elsif source.ends_with?('*')
        if source.ends_with?("r*")
          if source.ends_with?("_number*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8))
            return new(ref, type: TermType::Number, blank: true, one: false, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("l*")
          if source.ends_with?("_symbol*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8))
            return new(ref, type: TermType::Symbol, blank: true, one: false, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("g*")
          if source.ends_with?("_string*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8))
            return new(ref, type: TermType::String, blank: true, one: false, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("n*")
          if source.ends_with?("_boolean*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 9))
            return new(ref, type: TermType::Boolean, blank: true, one: false, poly: true, named: source.size > 9)
          end
        elsif source.ends_with?("t*")
          if source.ends_with?("_dict*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 6))
            return new(ref, type: TermType::Dict, blank: true, one: false, poly: true, named: source.size > 6)
          end
        elsif source.ends_with?("_*")
          ref = encode(source.unsafe_byte_slice(0, source.bytesize - 2))
          return new(ref, type: TermType::Any, blank: true, one: false, poly: true, named: source.size > 2)
        end
      else
        if source.ends_with?('r')
          if source.ends_with?("_number")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 7))
            return new(ref, type: TermType::Number, blank: true, poly: false, named: source.size > 7)
          end
        elsif source.ends_with?('l')
          if source.ends_with?("_symbol")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 7))
            return new(ref, type: TermType::Symbol, blank: true, poly: false, named: source.size > 7)
          end
        elsif source.ends_with?('g')
          if source.ends_with?("_string")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 7))
            return new(ref, type: TermType::String, blank: true, poly: false, named: source.size > 7)
          end
        elsif source.ends_with?('n')
          if source.ends_with?("_boolean")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8))
            return new(ref, type: TermType::Boolean, blank: true, poly: false, named: source.size > 8)
          end
        elsif source.ends_with?('t')
          if source.ends_with?("_dict")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 5))
            return new(ref, type: TermType::Dict, blank: true, poly: false, named: source.size > 5)
          end
        elsif source.ends_with?('_')
          ref = encode(source.unsafe_byte_slice(0, source.bytesize - 1))
          return new(ref, type: TermType::Any, blank: true, poly: false, named: source.size > 1)
        end
      end

      # Scary ain't it?

      new(encode(source.to_slice), type: TermType::Any, blank: false, poly: false, named: true)
    end

    private def ref : UInt32
      @spec.bits(7..)
    end

    private def name? : Sym?
      return unless @spec.bit(0) == 1

      Sym.new(ref, type: TermType::Any, poly: false, blank: false, named: true)
    end

    def blank?
      return unless @spec.bit(1) == 1

      Blank.new(TermType.new(@spec.bits(4...7).to_u8), name?, one: @spec.bit(3) == 1, poly: @spec.bit(2) == 1)
    end

    def blank : Blank
      blank? || raise "expected symbol to be a blank"
    end

    def to(type : String.class) : String
      inspect
    end

    def to_ir : IR
      IR.new(IR::Type::Symbol, inspect, children: nil)
    end

    def inspect(io)
      name = Sym.decode(ref)

      unless blank = blank?
        io.write(name)
        return
      end

      blank.inspect(io, name)
    end

    def_equals_and_hash @spec
  end

  # Frequently used symbols.

  SYMBOL_BIND_AT = Term[:"bind@"]
  SYMBOL_HELD_AT = Term[:"held@"]
  SYMBOL_HOLD    = Term[:hold]
  SYMBOL_PLACE   = Term[:place]
  SYMBOL_PASTE   = Term[:paste]
  SYMBOL_EMBED   = Term[:embed]
  SYM_EDGE       = Term.of(:edge)
end
