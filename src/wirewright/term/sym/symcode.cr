# A compact, variable-length encoding for short symbol names. For symbols to use
# this encoding, they must be short (say, under 10 characters), and must use only
# the following characters:
#
# ```text
# aeionst-bcdfghjklmpqruvwxyz0123456789\/?%^ABCXYZPQRUVWMDEFIJKGHLNOST<=>+*~!&$|ΛλΔδΠπµ°∪
# ```
#
# This encoding is preferred mainly beacuse it reduces contention: symbols that use it
# will never have to touch the global symbol table, neither during parsing, nor during
# printing, nor sorting, nor checking, etc. Thus, more threads can do more work (assuming
# there are no other contention points, of course).
struct Ww::Term::Sym
  module Symcode
    extend self

    {% begin %}
      {%
        table = {
            0b0u64 => {1, 3, "\0aeionst"},                         # <
           0b01u64 => {2, 4, "-bcdfghjklmpqruv"},                  # < Must be single-byte
          0b011u64 => {3, 5, "wxyz0123456789\\/?%^ABCXYZPQRUVWM"}, # <
          0b111u64 => {3, 5, "DEFIJKGHLNOST<=>+*~!&$| ΛλΔδΠπµ°∪"},
          #                                          ^ separates single-byte from Unicode
        }

        char2code = {} of ::NoReturn => ::NoReturn
        code2char = {} of ::NoReturn => ::NoReturn

        table.each do |prefix, (shift, rem, charset)|
          (charset.chars - [' ']).each_with_index do |char, index|
            code = (index << shift) | prefix
            char2code[char.ord] = {code, shift + rem}
            code2char[code] = char.ord
          end
        end

        ordtab = [] of ::NoReturn
        chrtab = [] of ::NoReturn

        (0...128).each do |index|
          code, size = char2code[index] || {0, 0}
          ordtab << {code, size}
        end

        (0...128).each do |index|
          code = code2char[index] || UInt8::MAX
          chrtab << code
        end
      %}

      # :nodoc:
      #
      # ASCII-only.
      #
      # 256 bytes.
      ORDTAB = StaticArray[
        {% for row in ordtab %}\
          {% code, size = row %}\
          { {{code}}u8, {{size}}u8 },
        {% end %}
      ]

      # :nodoc:
      #
      # ASCII-only.
      #
      # 128 bytes. Some ASCII characters are not included in this table.
      CHRTAB = UInt8.static_array({{chrtab.splat}})

      def code(ord : Int32) : {UInt64, UInt64}
        code, size = ORDTAB.unsafe_fetch(ord)
        if size > 0
          {code.to_u64, size.to_u64}
        else
          {UInt64::MAX, 0u64}
        end
      end

      # Returns the encoding of *chr*. Returns `UInt64::MAX` it it cannot
      # be encoded.
      def code(chr : Char) : {UInt64, UInt64}
        if chr.ord < 0x80
          return code(chr.ord)
        end

        case chr
        {% for char, row in char2code %}\
          {% code, size = row %}\
          {% if char >= 0x80 %}\
            when {{char}} then { {{code}}u64, {{size}}u64 }
          {% end %}\
        {% end %}\
        else
          {UInt64::MAX, 0u64}
        end
      end

      # Returns the size of the char encoded by *code* in bytes.
      def bytesize(*, code : UInt64) : Int32
        if (code & 0b1u64) == 0u64
          # LSB 0xxx MSB
          # xxx always in byte bounds (see the table)
          return 1
        end

        if (code & 0b10u64) == 0u64
          # LSB 10xxxx MSB
          # xxxx always in byte bounds (see the table)
          return 1
        end

        if (code & 0b100u64) == 0u64
          # LSB 110xxxxx MSB
          # xxxxx always in byte bounds (see the table)
          return 1
        end

        # LSB 111xxxxx MSB
        index = code >> 3

        {%
          _, _, charset = table[0b111u64]
          single_byte, unicode = charset.split(" ")
        %}

        if index < {{single_byte.size}}
          return 1
        end

        case index
        {% for char, index in unicode.chars %}\
          {%
            if char.ord < 0x80
              # 0xxxxxxx
              bytesize = 1
            elsif char.ord <= 0x7ff
              # 110xxxxx  10xxxxxx
              bytesize = 2
            elsif char.ord <= 0xffff
              # 1110xxxx  10xxxxxx  10xxxxxx
              bytesize = 3
            else
              # 11110xxx  10xxxxxx  10xxxxxx  10xxxxxx
              bytesize = 4
            end
          %}\
          when {{single_byte.size + index}}u64 then {{bytesize}}
        {% end %}\
        else
          raise ArgumentError.new
        end
      end

      # Returns the char encoded by *code*. Raises `ArgumentError` if *code*
      # is invalid.
      def chr(code : UInt64) : Char
        if code < CHRTAB.size
          ord = CHRTAB.unsafe_fetch(code)
          if ord < UInt8::MAX
            return ord.chr
          end
        end

        case code
        {% for code, char in code2char %}\
          {% if code >= 0x80 %}\
          when {{code}} then {{char}}.chr
          {% end %}\
        {% end %}\
        else
          raise ArgumentError.new
        end
      end

      # Appends UTF-8 bytes of the char encoded by *code* to *buffer*, starting
      # at *index*, without performing range checks. Returns the next *index*,
      # pointing after the appended char's bytes.
      #
      # Raises `ArgumentError` if *code* is invalid.
      def unsafe_write(buffer : Bytes, index : Int32, code : UInt64) : Int32
        if code < CHRTAB.size
          ord = CHRTAB.unsafe_fetch(code)
          if ord < UInt8::MAX
            buffer.unsafe_put(index, ord)
            return index + 1
          end
        end

        case code
        {% for code, char in code2char %}\
          {% if code >= 0x80 %}\
            when {{code}}
              {% if char < 0x80 %}\
                buffer.unsafe_put(index, {{char}})
                index + 1
              {% else %}\
                {{char}}.chr.each_byte do |byte|
                  buffer.unsafe_put(index, byte)
                  index += 1
                end
                index
              {% end %}\
            {% end %}\
          {% end %}\
        else
          raise ArgumentError.new
        end
      end
    {% end %}

    BIT_LENGTH_UNICODE     = Int32::MAX - 1
    BIT_LENGTH_NO_ENCODING = Int32::MAX

    # Returns the number of bits needed to store *string* using Symcode. Returns
    # `BIT_LENGTH_NO_ENCODING` if one of its chars cannot be encoded, or if
    # bit length exceeds *max*.
    def bit_length(string : String, *, max : Int32) : Int32
      bit_length = 0

      string.each_char do |chr|
        code, size = code(chr)
        if code == UInt64::MAX
          return BIT_LENGTH_NO_ENCODING
        end

        bit_length += size
        if bit_length > max
          return BIT_LENGTH_NO_ENCODING
        end
      end

      bit_length
    end

    # Returns the number of bits needed to store *bytes* using Symcode. Returns
    # `BIT_LENGTH_NO_ENCODING` if one of the bytes cannot be encoded, or if
    # bit length exceeds *max*. Returns `BIT_LENGTH_UNICODE` if *bytes* contains
    # non-ASCII characters; in that case you should allocate a string and use
    # the other overload.
    def bit_length(bytes : Bytes, *, max : Int32) : Int32
      bit_length = 0

      bytes.each do |byte|
        if byte >= 0x80
          return BIT_LENGTH_UNICODE
        end

        code, size = code(byte.to_i)
        if code == UInt64::MAX
          return BIT_LENGTH_NO_ENCODING
        end

        bit_length += size
        if bit_length > max
          return BIT_LENGTH_NO_ENCODING
        end
      end

      bit_length
    end

    NO_ENCODING = UInt64::MAX

    # Returns the Symcode encoding of UTF-8 *bytes* using up to *limit* bits.
    # Returns `NO_ENCODING` if *bytes* cannot be encoded using Symcode.
    def encode(bytes : Bytes, *, limit : Int32) : UInt64
      case bit_length(bytes, max: limit)
      when BIT_LENGTH_NO_ENCODING
        NO_ENCODING
      when BIT_LENGTH_UNICODE
        # Slow path: allocate a string.
        encode(String.new(bytes), limit: limit)
      else
        bits = 0u64

        bytes.reverse_each do |byte|
          code, bit_size = code(byte.to_i)
          bits <<= bit_size
          bits |= code
        end

        bits
      end
    end

    # Returns the Symcode encoding of *string* using up to *limit* bits. Returns
    # `NO_ENCODING` if *string* cannot be encoded using Symcode.
    def encode(string : String, *, limit = BIT_WIDTH) : UInt64
      if bit_length(string, max: limit) > limit
        return NO_ENCODING
      end

      bits = 0u64
      if string.empty?
        return bits
      end

      reader = Char::Reader.new(at_end: string)

      loop do
        code, bit_size = code(reader.current_char)
        bits <<= bit_size
        bits |= code

        break if reader.pos.zero?
        reader.previous_char
      end

      bits
    end

    private def each_code_inner(bits : UInt64, & : UInt64 ->)
      16.times do # 64/4 = 16, 4 is the smallest char width.
        if (bits & 0b1u64) == 0u64
          # LSB 0xxx MSB
          yield bits & 0b1111u64
          bits >>= 4
          next
        end

        if ((bits >> 1) & 0b1u64) == 0u64
          # LSB 10xxxx MSB
          yield bits & 0b111111u64
          bits >>= 6
          next
        end

        # LSB 110xxxxx MSB
        # LSB 111xxxxx MSB
        yield bits & 0b11111111u64
        bits >>= 8
      end
    end

    private def each_code(bits : UInt64, & : UInt64 ->)
      each_code_inner(bits) do |code|
        break if code == 0u64 # We use \0 as a terminator.
        yield code
      end
    end

    # Returns the bytesize of the string encoded by *bits*.
    #
    # Raises `ArgumentError` on invalid *bits*.
    def bytesize(bits : UInt64) : Int32
      bytesize = 0

      each_code(bits) do |code|
        delta = bytesize(code: code)
        if delta == 0
          raise ArgumentError.new
        end

        bytesize += delta
      end

      bytesize
    end

    # Yields the sequence of chars encoded by *bits*.
    def each_char(bits : UInt64, & : Char ->) : Nil
      each_code(bits) do |code|
        yield chr(code)
      end
    end

    # Decodes Symcode *bits* into a Crystal string.
    #
    # Raises `ArgumentError` on invalid *bits*.
    def decode(bits : UInt64) : String
      if bits > 2u64**BIT_WIDTH
        raise ArgumentError.new
      end

      buffer = Bytes.new(bytesize(bits))
      index = 0

      each_code(bits) do |code|
        index = unsafe_write(buffer, index, code)
      end

      index += 1

      String.new(buffer)
    end
  end
end
