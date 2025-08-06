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

      def single? : Bool
        !poly?
      end

      # :nodoc:
      #
      # TODO: remove this. Symbol should be responsible for printing, or even better,
      # the specific implementation -- e.g. ML.compact!!
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

    @@encode = SyncHash(Bytes, UInt32).new(1024)
    @@decode = Sync::Shared(Array(Bytes)).new(Array(Bytes).new(initial_capacity: 1024))

    # Ref used for symbols such as `_`, `_number`, `_string`, etc. that have
    # an empty name component.
    EMPTY_BLANK_REF = 0u32

    # :nodoc:
    def self.encode(bytes : Bytes, *, blank : Bool) : UInt32
      if bytes.empty?
        unless blank
          raise ArgumentError.new("cannot encode empty nonblank")
        end

        # Empty blank is always unambiguous!
        return EMPTY_BLANK_REF
      end

      @@encode.put_if_absent(bytes) do
        ref = @@decode.write do |ary|
          ary << bytes
          # Store ref + 1 to avoid ref = 0 which we use for EMPTY_BLANK_REF.
          ary.size.to_u32 << 1
        end

        # Ambiguity matters only when we're a blank. Thus, force the caller to say
        # whether the symbol is a blank or not; and use that to prevent an O(n) pass
        # over the bytestring for the vast majority of calls.
        if blank
          ambiguous = bytes.any?({{'_'.ord}})
          ref |= ambiguous ? 1u32 : 0u32
        end

        ref
      end
    end

    # :nodoc:
    def self.decode(ref : UInt32) : Bytes
      if ref == EMPTY_BLANK_REF
        return Bytes.empty
      end

      @@decode.read { |ary| ary[(ref >> 1) - 1] }
    end

    # Returns `true` if *ref* was deemed "ambiguous" during encoding.
    #
    # We flag a ref as "ambiguous" when it is a blank and we want to force a reparse
    # of ref on `name?`.
    #
    # E.g. `x_number_number` is ambiguous, and simply omitting `_number` wouldn't yield
    # a valid name symbol; the name symbol must also be a blank and so on on. A hard
    # reparse is required for such "nested blanks" to be properly handled.
    #
    # User-land recommendation would be to not use underscores in symbols. Reparsing
    # wouldn't be cheap (a few nanoseconds cheap, that is).
    def self.ambiguous?(ref : UInt32) : Bool
      ref & 0b1 == 1
    end

    # :nodoc:
    #
    #          ambiguous?  at least one?      blank?
    #                  v              v       v
    #   ... 0   0   0  0  0   0   0   0   0   0   0 < named?
    # <-----------------  ---------       ^ poly?
    #    symbol ref       term type
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
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8), blank: true)
            return new(ref, type: TermType::Number, blank: true, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("l+")
          if source.ends_with?("_symbol+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8), blank: true)
            return new(ref, type: TermType::Symbol, blank: true, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("g+")
          if source.ends_with?("_string+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8), blank: true)
            return new(ref, type: TermType::String, blank: true, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("n+")
          if source.ends_with?("_boolean+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 9), blank: true)
            return new(ref, type: TermType::Boolean, blank: true, poly: true, named: source.size > 9)
          end
        elsif source.ends_with?("t+")
          if source.ends_with?("_dict+")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 6), blank: true)
            return new(ref, type: TermType::Dict, blank: true, poly: true, named: source.size > 6)
          end
        elsif source.ends_with?("_+")
          ref = encode(source.unsafe_byte_slice(0, source.bytesize - 2), blank: true)
          return new(ref, type: TermType::Any, blank: true, poly: true, named: source.size > 2)
        end
      elsif source.ends_with?('*')
        if source.ends_with?("r*")
          if source.ends_with?("_number*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8), blank: true)
            return new(ref, type: TermType::Number, blank: true, one: false, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("l*")
          if source.ends_with?("_symbol*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8), blank: true)
            return new(ref, type: TermType::Symbol, blank: true, one: false, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("g*")
          if source.ends_with?("_string*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8), blank: true)
            return new(ref, type: TermType::String, blank: true, one: false, poly: true, named: source.size > 8)
          end
        elsif source.ends_with?("n*")
          if source.ends_with?("_boolean*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 9), blank: true)
            return new(ref, type: TermType::Boolean, blank: true, one: false, poly: true, named: source.size > 9)
          end
        elsif source.ends_with?("t*")
          if source.ends_with?("_dict*")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 6), blank: true)
            return new(ref, type: TermType::Dict, blank: true, one: false, poly: true, named: source.size > 6)
          end
        elsif source.ends_with?("_*")
          ref = encode(source.unsafe_byte_slice(0, source.bytesize - 2), blank: true)
          return new(ref, type: TermType::Any, blank: true, one: false, poly: true, named: source.size > 2)
        end
      else
        if source.ends_with?('r')
          if source.ends_with?("_number")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 7), blank: true)
            return new(ref, type: TermType::Number, blank: true, poly: false, named: source.size > 7)
          end
        elsif source.ends_with?('l')
          if source.ends_with?("_symbol")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 7), blank: true)
            return new(ref, type: TermType::Symbol, blank: true, poly: false, named: source.size > 7)
          end
        elsif source.ends_with?('g')
          if source.ends_with?("_string")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 7), blank: true)
            return new(ref, type: TermType::String, blank: true, poly: false, named: source.size > 7)
          end
        elsif source.ends_with?('n')
          if source.ends_with?("_boolean")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 8), blank: true)
            return new(ref, type: TermType::Boolean, blank: true, poly: false, named: source.size > 8)
          end
        elsif source.ends_with?('t')
          if source.ends_with?("_dict")
            ref = encode(source.unsafe_byte_slice(0, source.bytesize - 5), blank: true)
            return new(ref, type: TermType::Dict, blank: true, poly: false, named: source.size > 5)
          end
        elsif source.ends_with?('_')
          ref = encode(source.unsafe_byte_slice(0, source.bytesize - 1), blank: true)
          return new(ref, type: TermType::Any, blank: true, poly: false, named: source.size > 1)
        end
      end

      # Scary ain't it?

      new(encode(source.to_slice, blank: false), type: TermType::Any, blank: false, poly: false, named: true)
    end

    # Maybe:
    #
    # TAG
    # 0 0 -- normal symbol
    # 0 1 -- blank
    # 1 0 -- ambiguous blank
    # 1 1 -- entity
    #
    # Entity:
    #   ENTITY TYPE (6 bits)
    #   ENTITY PAYLOAD -- 3 bytes
    #
    # ENTITY TYPE
    #    rule id [underscore]
    #    rule group id [underscore]
    #    reader byte start
    #    reader byte end
    #
    # NOTE: rule ids rely on the fact that *_blank variants are real blanks!!
    # I.e. note how <BYTE START>:◇_ is a real blank! Thus if we separate into
    # entities we must also support parsing blanks -- `#blank` must be aware
    # of some entities but not others!
    #
    # NOTE: It appears that some Entity symbols can be *parsed* from normal
    # ones, such as µ-* symbols for microfold; whereas others must be constructed
    # by hand in the native code, such as rule id/rule group id/etc.

    def self.rule_id(byte_start, *, blank : Bool) : Sym
      name = String.build do |io|
        byte_start.to_s(io, base: 16, upcase: true)
        io << ":◇"
        io << "_" if blank
      end

      new(name)
    end

    def self.rule_block_id(byte_start, *, blank : Bool) : Sym
      name = String.build do |io|
        byte_start.to_s(io, base: 16, upcase: true)
        io << ":▢"
        io << "_" if blank
      end

      new(name)
    end

    def self.byte_start : Sym
      new("(byte_start)")
    end

    def self.byte_end : Sym
      new("(byte_end)")
    end

    # Validates a symbol *name*. If this method returns `true`, you are safe to
    # call `new` with *name*; safe not in the sense of memory safety etc., but
    # in the sense of being able to give the symbol to any subsystem of Wirewright
    # and expect it to work, pretty-print, etc.
    def self.valid?(name : String) : Bool
      if name.empty?
        return false
      end

      name.each_char do |chr|
        rune = ML::Rune.new(chr)

        unless rune.symbolic?
          return false
        end
      end

      ML.can_represent_symbol?(name)
    end

    private def ref : UInt32
      @spec.bits(7..)
    end

    private def name? : Sym?
      return unless @spec.bit(0) == 1

      if Sym.ambiguous?(ref)
        return Sym.new(String.new(Sym.decode(ref)))
      end

      Sym.new(ref, type: TermType::Any, poly: false, blank: false, named: true)
    end

    def blank?
      return unless @spec.bit(1) == 1

      Blank.new(TermType.new(@spec.bits(4...7).to_u8), name?, one: @spec.bit(3) == 1, poly: @spec.bit(2) == 1)
    end

    def blank
      blank? || raise "expected symbol to be a blank"
    end

    record RuleId, byte_start : Int32 do
      def name
        "◇"
      end
    end

    record RuleIdBlank, byte_start : Int32 do
      def name
        "◇_"
      end
    end

    record RuleBlockId, byte_start : Int32 do
      def name
        "▢"
      end
    end

    record RuleBlockIdBlank, byte_start : Int32 do
      def name
        "▢_"
      end
    end

    def rule_id_sentinel? : RuleId?
      name = to(String)
      return unless name.ends_with?(":◇")

      start, _, _ = name.partition(':')
      RuleId.new(start.to_i(base: 16))
    end

    def rule_id_blank_sentinel? : RuleIdBlank?
      name = to(String)
      return unless name.ends_with?(":◇_")

      start, _, _ = name.partition(':')
      RuleIdBlank.new(start.to_i(base: 16))
    end

    def rule_block_id_sentinel? : RuleBlockId?
      name = to(String)
      return unless name.ends_with?(":▢")

      start, _, _ = name.partition(':')
      RuleBlockId.new(start.to_i(base: 16))
    end

    def rule_block_id_blank_sentinel? : RuleBlockIdBlank?
      name = to(String)
      return unless name.ends_with?(":▢_")

      start, _, _ = name.partition(':')
      RuleBlockIdBlank.new(start.to_i(base: 16))
    end

    # Returns `true` if this symbol is reserved for Microfold. Returns
    # `false` otherwise.
    def microfold? : Bool
      to(String).prefixed_by?("µ-")
    end

    def to(type : String.class) : String
      inspect
    end

    def inspect(io)
      name = Sym.decode(ref)

      unless blank = blank?
        io.write(name)
        return
      end

      blank.inspect(io, name)
    end

    def_equals @spec
  end

  # Frequently used symbols.

  SYM_EDGE = Term[:edge]

  SYM_BLANK_ANY     = Term[:_]
  SYM_BLANK_DICT    = Term[:_dict]
  SYM_BLANK_NUMBER  = Term[:_number]
  SYM_BLANK_SYMBOL  = Term[:_symbol]
  SYM_BLANK_STRING  = Term[:_string]
  SYM_BLANK_BOOLEAN = Term[:_boolean]
end
