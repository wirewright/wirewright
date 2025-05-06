module Ww::Meridium::Axis
  module Proto
    # Maximum bytesize of data read and written. Includes terms, bitmaps, etc.
    MAX_DATA_BYTESIZE = 16 * 1024 # 16 KiB

    # Lists the tokens used in the protocol.
    enum Token : UInt8
      ERR
      ASK
      HAS
      ANS
      SUB
      UNS
      TXN
      ADD
      DEL
      STP
      STPS
      STA
      SRQ
      SRS
      SRSS
      OVR
    end

    # Lets callers read from an underlying IO using the Axis protocol.
    #
    # Callers should expect all methods to raise either `NetworkError` in case
    # the IO is broken or reading fails; or `ProtocolError` in case there is
    # something wrong with the format of things being exchanged. No other error
    # should be expected.
    struct Reader
      def initialize(@io : IO)
      end

      # A shorthand for raising a `ProtocolError`.
      def err(*args, **kwargs)
        raise ProtocolError.new(*args, **kwargs)
      end

      {% for type in %w[UInt8 UInt16 UInt32 UInt64 Int8 Int16 Int32 Int64] %}
        # Reads an object of type *cls* using the protocol.
        def read(cls : {{type.id}}.class) : {{type.id}}
          @io.read_bytes(cls, IO::ByteFormat::BigEndian)
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end
      {% end %}

      # :ditto:
      def read(cls : Token.class) : Token
        byte = @io.read_byte || raise IO::EOFError.new

        Token.from_value?(byte) || err "expected a token"
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # :ditto:
      def read(cls : WWID.class) : WWID
        scratch = uninitialized UInt8[WWID::BYTESIZE]

        @io.read_fully(scratch.to_slice)

        WWID.from_slice_be(scratch.to_slice)
      rescue e : WWID::ParseError
        err "invalid WWID", cause: e
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # :ditto:
      def read(cls : IWWID.class) : IWWID
        scratch = uninitialized UInt8[IWWID::BYTESIZE]

        @io.read_fully(scratch.to_slice)

        IWWID.from_slice_be(scratch.to_slice)
      rescue e : WWID::ParseError
        err "invalid IWWID", cause: e
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # :ditto:
      def read(cls : Atom.class) : Atom
        scratch = uninitialized UInt8[Atom::BYTESIZE]

        @io.read_fully(scratch.to_slice)

        Atom.from_slice_be(scratch.to_slice)
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # :ditto:
      def read(cls : Bytes.class) : Bytes
        bytesize = read(UInt32)
        if bytesize > MAX_DATA_BYTESIZE
          err "data bytesize limit exceeded"
        end

        slice = Bytes.new(bytesize)
        @io.read_fully(slice)
        slice
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # :ditto:
      def read(cls : String.class) : String
        bytesize = read(UInt32)
        if bytesize > MAX_DATA_BYTESIZE
          err "data bytesize limit exceeded"
        end

        @io.read_string(bytesize)
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # :ditto:
      def read(cls : BitList.class) : BitList
        bitsize, buckets = read(UInt32), read(Bytes)

        bitlist = BitList.new(bitsize)
        reader = BitReader.new(buckets)
        while bitlist.size < bitsize && (bit = reader.consume?)
          bitlist << (bit == 1)
        end

        bitlist
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # :ditto:
      def read(cls : Term.class) : Term
        ML.term(read(String))
      rescue e : ML::SyntaxError
        err "invalid or malformed term", cause: e
      end

      # :ditto:
      def read(cls : Enum.class)
        read_enum_impl(cls)
      end

      private def read_enum_impl(cls : T.class) : T forall T
        value = read(typeof({{T.constant(T.constants[0])}}))

        T.from_value?(value) || err "invalid enum value"
      end

      # Shorthand for `read(WWID)` that also makes sure that the resulting WWID
      # is a connection id.
      def conid : WWID
        wwid = read(WWID)
        unless wwid.conid?
          err "expected a conid"
        end
        wwid
      end

      # Reads a `Token` object and makes sure it is equal to *expected*.
      def expect(expected : Token) : Nil
        unless read(Token) == expected
          err "unexpected token, expected: #{expected}"
        end
      end
    end

    # Lets callers write to an underlying IO using the Axis protocol.
    class Writer
      struct Txn
        def initialize(@io : IO)
        end

        # Writes *object* to the underlying IO.
        def <<(object : Token) : self
          @io.write_byte(object.value)

          self
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end

        {% for type in %w[UInt8 UInt16 UInt32 UInt64 Int8 Int16 Int32 Int64] %}
          # :ditto:
          def <<(object : {{type.id}}) : self
            @io.write_bytes(object, IO::ByteFormat::BigEndian)

            self
          rescue e : IO::Error
            raise NetworkError.new(cause: e)
          end
        {% end %}

        # :ditto:
        def <<(object : BitList) : self
          writer = MutBitWriter.new
          object.each do |bit|
            writer << (bit ? 1u8 : 0u8)
          end

          @io.write_bytes(writer.bitsize.to_u32, IO::ByteFormat::BigEndian)
          @io.write_bytes(writer.bytesize.to_u32, IO::ByteFormat::BigEndian)

          writer.each_byte { |byte| @io.write_byte(byte) }

          self
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end

        # :ditto:
        def <<(object : Bytes) : self
          @io.write_bytes(object.size.to_u32, IO::ByteFormat::BigEndian)
          @io.write(object)

          self
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end

        # :ditto:
        def <<(object : WWID | IWWID) : self
          object.to_slice_be { |slice| @io.write(slice) }

          self
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end

        # :ditto:
        def <<(object : Term) : self
          bytesize = ML.compact_bytesize(object)

          @io.write_bytes(bytesize, IO::ByteFormat::BigEndian)

          ML.compact(@io, object)

          self
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end

        # :ditto:
        def <<(object : Atom) : self
          object.to_slice_be { |slice| @io.write(slice) }

          self
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end

        # :ditto:
        def <<(object : Enum) : self
          self << object.value
        end

        # Writes all of *objects* to the underlying IO inorder.
        def <<(objects : Tuple) : self
          objects.each { |object| self << object }

          self
        rescue e : IO::Error
          raise NetworkError.new(cause: e)
        end
      end

      def initialize(@io : IO)
        @lock = Mutex.new
      end

      # Gives the block exclusive write access to the underlying IO. Yields
      # a transaction object `Txn` to do the writing. Does not flush the IO
      # after the block.
      def sync(& : Txn ->) : Nil
        @lock.synchronize { yield Txn.new(@io) }
      end

      # Same as `sync`, but flushes the underlying IO after the block.
      def flush(& : Txn ->) : Nil
        sync do |txn|
          yield txn

          @io.flush
        end
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # Flushes the underlying IO.
      def flush
        sync { @io.flush }
      rescue e : IO::Error
        raise NetworkError.new(cause: e)
      end

      # Creates a transaction, writes all *objects* inorder, then flushes
      # the underlying IO.
      def send(*objects) : Nil
        flush { |txn| txn << objects }
      end

      # Same as `send`, but does not flush the underlying IO.
      def <<(object) : self
        sync { |txn| txn << object }

        self
      end
    end
  end
end
