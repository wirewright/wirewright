struct Ww::Term::Sym
  module Spec
    extend self

    defrecord Repr, datum : Name | Blank

    def repr(datum) : Repr
      Repr.new(datum)
    end

    defrecord Blank, type : TermType, mult : BlankMult, amb : Bool, name : Name

    defrecord Name, datum : InlineName | RefName

    # At most `NAME_LIMIT` bits (currently 54). The current implementation of
    # `Spec` assumes fixed-size names. The underlying encoding, `Symcode`,
    # is variable-width, so it doesn't care much.
    defrecord InlineName, symcode : UInt64
    defrecord RefName, index : UInt32

    LEN = 62

    NAME_LIMIT = LEN - (REPR_LEN + BLANK_LEN + NAME_LEN)

    REPR_LEN   =    1
    REPR_NAME  = 0u64
    REPR_BLANK = 1u64

    BLANK_LEN      = BLANK_AMB_LEN + BLANK_TYPE_LEN + BLANK_MULT_LEN
    BLANK_AMB_LEN  = 1u64
    BLANK_TYPE_LEN = 3u64
    BLANK_MULT_LEN = 2u64
    BLANK_AMB_YES  = 1u64
    BLANK_AMB_NO   = 0u64

    NAME_LEN    =    1
    NAME_INLINE = 0u64
    NAME_REF    = 1u64

    @@string2ref = SyncHash(String, UInt32).new(128)
    @@ref2string = Sync::Shared(Array(String)).new(Array(String).new(initial_capacity: 128))

    # Parses a symbol name *arg*. Returns the resulting `Name`, followed by a boolean
    # indicating whether the name contains underscores.
    private def name_and_amb(arg : String | Bytes, *, limit : Int32) : {Name, Bool}
      bytes = arg.is_a?(String) ? arg.to_slice : arg

      symcode = Symcode.encode(bytes, limit: limit)
      unless symcode == Symcode::NO_ENCODING
        return Name.new(InlineName.new(symcode)), false
      end

      assert limit >= 32

      string = arg.is_a?(Bytes) ? String.new(arg) : arg

      index = @@string2ref.put_if_absent(string) do
        @@ref2string.lock do |ary|
          ary << string
          ary.size.to_u32 - 1
        end
      end

      {Name.new(RefName.new(index)), string.includes?('_')}
    end

    # Returns the symbol name that *ref* refers to, as a string.
    private def ref_string(ref : RefName) : String
      @@ref2string.shared { |ary| ary.unsafe_fetch(ref.index) }
    end

    # Exceeding this number of bytes will cause `compare` to start allocating
    # on the heap.
    #
    # 64 bytes is huge for symbols, so the heap alloc path is pessimized by
    # an exception and some wasted work.
    SYM_SMALL_BYTES = 64

    # :nodoc:
    class Sink(N)
      class DoesNotFit < Exception
        @callstack = CallStack.empty
      end

      def initialize
        @pos = 0u32
        @buffer = uninitialized UInt8[N]
      end

      def <<(object : Char) : Nil
        if @pos &+ object.bytesize > N
          raise DoesNotFit.new
        end

        object.each_byte do |byte|
          @buffer.unsafe_put(@pos, byte)
          @pos &+= 1
        end
      end

      def <<(object : String) : Nil
        if @pos &+ object.bytesize > N
          raise DoesNotFit.new
        end

        (@buffer.to_unsafe + @pos).copy_from(object.to_unsafe, object.bytesize)
        @pos &+= object.bytesize
      end

      def to_slice
        Slice.new(@buffer.to_unsafe, @pos, read_only: true)
      end
    end

    # Calls `write` on *a*, writing into possibly stack-allocated memory (fast path).
    # If the number of bytes written exceeds `SYM_SMALL_BYTES`, aborts, and uses
    # `IO::Memory` instead (slow path).
    private def unsafe_write_to_slice(a : Repr, & : Bytes ->)
      begin
        sinkmem0 = uninitialized ReferenceStorage(Sink(SYM_SMALL_BYTES))
        sink = Sink(SYM_SMALL_BYTES).unsafe_construct(pointerof(sinkmem0))
        write(sink, a)

        slice = sink.to_slice
      rescue Sink::DoesNotFit
        io = IO::Memory.new
        write(io, a)

        slice = io.to_slice
      end

      yield slice
    end

    def compare(a : Repr, b : Repr) : Int32
      unsafe_write_to_slice(a) do |l|
        unsafe_write_to_slice(b) do |r|
          (l <=> r).sign
        end
      end
    end

    # Constructs a blank repr for a blank with the given *name*. *type* sets
    # the blank's type (as in `x_number`), and *mult* its multiplicity (as in
    # `x_number` vs `x_number*` vs `x_number+`).
    def blank(name : String | Bytes, type : TermType, mult : BlankMult) : Repr
      name, amb = name_and_amb(name, limit: NAME_LIMIT)

      blank = Blank.new(type, mult, amb, name)

      repr(blank)
    end

    def blank(prev : Repr, type : TermType, mult : BlankMult) : Repr
      repr(blank(prev.datum, type, mult))
    end

    private def blank(prev : Name, type : TermType, mult : BlankMult) : Blank
      blank(prev.datum, type, mult)
    end

    private def blank(prev : InlineName, type : TermType, mult : BlankMult) : Blank
      Blank.new(type, mult, amb: false, name: Name.new(prev))
    end

    private def blank(prev : RefName, type : TermType, mult : BlankMult) : Blank
      string = ref_string(prev)

      Blank.new(type, mult, amb: string.includes?('_'), name: Name.new(prev))
    end

    private def blank(prev : Blank, type : TermType, mult : BlankMult) : Blank
      Blank.new(type, mult, prev.amb, prev.name)
    end

    # Constructs a nonblank repr for a symbol with the given *name*.
    def nonblank(name : String | Bytes) : Repr
      # String can contain `_`s but it's not amb, e.g. `foo_bar`. Its name is itself so
      # we simply throw away amb.
      datum, _ = name_and_amb(name, limit: NAME_LIMIT)

      repr(datum)
    end

    # Parses *string* as a symbol and returns the resulting bits.
    def parse(string : String) : Repr
      unless string.empty?
        bytes = string.to_slice

        # ..._
        bytes.rchop('_') do |bytes|
          return blank(bytes, :any, :one)
        end

        # ..._<type>
        bytes.rchop('_', 'n', 'u', 'm', 'b', 'e', 'r') do |bytes|
          return blank(bytes, :number, :one)
        end
        bytes.rchop('_', 's', 't', 'r', 'i', 'n', 'g') do |bytes|
          return blank(bytes, :string, :one)
        end
        bytes.rchop('_', 's', 'y', 'm', 'b', 'o', 'l') do |bytes|
          return blank(bytes, :symbol, :one)
        end
        bytes.rchop('_', 'b', 'o', 'o', 'l', 'e', 'a', 'n') do |bytes|
          return blank(bytes, :boolean, :one)
        end
        bytes.rchop('_', 'd', 'i', 'c', 't') do |bytes|
          return blank(bytes, :dict, :one)
        end

        # ..._...*
        bytes.rchop('*') do |bytes|
          bytes.rchop('_') do |bytes|
            return blank(bytes, :any, :zero_or_more)
          end
          bytes.rchop('_', 'n', 'u', 'm', 'b', 'e', 'r') do |bytes|
            return blank(bytes, :number, :zero_or_more)
          end
          bytes.rchop('_', 's', 't', 'r', 'i', 'n', 'g') do |bytes|
            return blank(bytes, :string, :zero_or_more)
          end
          bytes.rchop('_', 's', 'y', 'm', 'b', 'o', 'l') do |bytes|
            return blank(bytes, :symbol, :zero_or_more)
          end
          bytes.rchop('_', 'b', 'o', 'o', 'l', 'e', 'a', 'n') do |bytes|
            return blank(bytes, :boolean, :zero_or_more)
          end
          bytes.rchop('_', 'd', 'i', 'c', 't') do |bytes|
            return blank(bytes, :dict, :zero_or_more)
          end
        end

        # ..._...+
        bytes.rchop('+') do |bytes|
          bytes.rchop('_') do |bytes|
            return blank(bytes, :any, :one_or_more)
          end
          bytes.rchop('_', 'n', 'u', 'm', 'b', 'e', 'r') do |bytes|
            return blank(bytes, :number, :one_or_more)
          end
          bytes.rchop('_', 's', 't', 'r', 'i', 'n', 'g') do |bytes|
            return blank(bytes, :string, :one_or_more)
          end
          bytes.rchop('_', 's', 'y', 'm', 'b', 'o', 'l') do |bytes|
            return blank(bytes, :symbol, :one_or_more)
          end
          bytes.rchop('_', 'b', 'o', 'o', 'l', 'e', 'a', 'n') do |bytes|
            return blank(bytes, :boolean, :one_or_more)
          end
          bytes.rchop('_', 'd', 'i', 'c', 't') do |bytes|
            return blank(bytes, :dict, :one_or_more)
          end
        end
      end

      nonblank(string)
    end

    def parse(name : RefName) : Repr
      parse(ref_string(name))
    end

    # NOTE: Inline names can't contain underscores so they will never parse
    # into blanks.
    def parse(name : InlineName) : Repr
      repr(Name.new(name))
    end

    def parse(name : Name) : Repr
      parse(name.datum)
    end

    @[AlwaysInline]
    private def mask(size : Int)
      (1u64 << size) &- 1
    end

    # Returns the bits corresponding to *repr*.
    def pack(repr : Repr) : UInt64
      payload = pack(repr.datum)

      case repr.datum
      in Name  then (payload << REPR_LEN) | REPR_NAME
      in Blank then (payload << REPR_LEN) | REPR_BLANK
      end
    end

    # :ditto:
    def pack(repr : Blank) : UInt64
      payload = pack(repr.name)

      bits = payload

      bits <<= BLANK_AMB_LEN
      bits |= repr.amb ? BLANK_AMB_YES : BLANK_AMB_NO

      bits <<= BLANK_TYPE_LEN
      bits |= repr.type.value.to_u64

      bits <<= BLANK_MULT_LEN
      bits |= repr.mult.value.to_u64
    end

    # :ditto:
    def pack(repr : Name) : UInt64
      payload = pack(repr.datum)

      case repr.datum
      in InlineName then (payload << NAME_LEN) | NAME_INLINE
      in RefName    then (payload << NAME_LEN) | NAME_REF
      end
    end

    # :ditto:
    def pack(repr : InlineName) : UInt64
      repr.symcode
    end

    # :ditto:
    def pack(repr : RefName) : UInt64
      repr.index.to_u64
    end

    # Returns the `Repr` corresponding to *bits*.
    def unpack(cls : Repr.class, bits : UInt64) : Repr
      tag = bits & mask(REPR_LEN)
      payload = bits >> REPR_LEN

      case tag
      when REPR_NAME  then repr(unpack(Name, payload))
      when REPR_BLANK then repr(unpack(Blank, payload))
      else
        raise ArgumentError.new
      end
    end

    # :ditto:
    def unpack(cls : Blank.class, bits : UInt64) : Blank
      mult = BlankMult.new((bits & mask(BLANK_MULT_LEN)).to_u8)
      bits >>= BLANK_MULT_LEN

      type = TermType.new((bits & mask(BLANK_TYPE_LEN)).to_u8)
      bits >>= BLANK_TYPE_LEN

      case bits & mask(BLANK_AMB_LEN)
      when BLANK_AMB_YES
        amb = true
      when BLANK_AMB_NO
        amb = false
      else
        raise ArgumentError.new
      end
      bits >>= BLANK_AMB_LEN

      name = unpack(Name, bits)

      Blank.new(type, mult, amb, name)
    end

    # :ditto:
    def unpack(cls : Name.class, bits : UInt64) : Name
      tag = bits & mask(NAME_LEN)
      payload = bits >> NAME_LEN

      case tag
      when NAME_INLINE then Name.new(unpack(InlineName, payload))
      when NAME_REF    then Name.new(unpack(RefName, payload))
      else
        raise ArgumentError.new
      end
    end

    # :ditto:
    def unpack(cls : InlineName.class, bits : UInt64) : InlineName
      InlineName.new(bits)
    end

    # :ditto:
    def unpack(cls : RefName.class, bits : UInt64) : RefName
      RefName.new(bits.to_u32)
    end

    # Yields each name character in *repr*.
    def each_name_char(repr : Repr, & : Char ->) : Nil
      each_name_char(repr.datum) { |chr| yield chr }
    end

    # :ditto:
    def each_name_char(repr : Blank, & : Char ->) : Nil
      each_name_char(repr.name) { |chr| yield chr }
    end

    # :ditto:
    def each_name_char(repr : Name, & : Char ->) : Nil
      each_name_char(repr.datum) { |chr| yield chr }
    end

    # :ditto:
    def each_name_char(repr : InlineName, & : Char ->) : Nil
      Symcode.each_char(repr.symcode) { |chr| yield chr }
    end

    # :ditto:
    def each_name_char(repr : RefName, & : Char ->) : Nil
      string = ref_string(repr)
      string.each_char { |chr| yield chr }
    end

    def blank?(repr : Repr) : Blank?
      repr.datum.as?(Blank)
    end

    def write(io, repr : Repr) : Nil
      write(io, repr.datum)
    end

    def write(io, repr : Name) : Nil
      write(io, repr.datum)
    end

    def write(io, repr : Blank) : Nil
      write(io, repr.name)

      case repr.type
      in .any?     then io << "_"
      in .number?  then io << "_number"
      in .string?  then io << "_string"
      in .symbol?  then io << "_symbol"
      in .boolean? then io << "_boolean"
      in .dict?    then io << "_dict"
      end

      case repr.mult
      in .one?
      in .zero_or_more? then io << "*"
      in .one_or_more?  then io << "+"
      end
    end

    def write(io, repr : InlineName) : Nil
      Symcode.each_char(repr.symcode) do |chr|
        io << chr
      end
    end

    def write(io, repr : RefName) : Nil
      io << ref_string(repr)
    end
  end
end
