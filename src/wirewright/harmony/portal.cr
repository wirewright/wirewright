class Ww::Harmony
  # Portal is a small, simple protocol used in `link: portal` (aka `PortalLink`).
  module Portal
    extend self

    alias Frame = Data | Accept | Ready | Busy

    defrecord Data, msgid : MsgId, payload : Term::Blob
    defrecord Accept, msgid : MsgId
    defrecord Ready
    defrecord Busy

    def serialize(io, msgid : MsgId) : Nil
      msgid.repr.to_s(io, base: 16)
    end

    def serialize(io, frame : Data) : Nil
      io << "DATA "
      serialize(io, frame.msgid)
      io << " "
      io.write(frame.payload.to_slice)
    end

    def serialize(io, frame : Accept)
      io << "ACCEPT "
      serialize(io, frame.msgid)
    end

    def serialize(io, frame : Ready)
      io << "READY"
    end

    def serialize(io, frame : Busy)
      io << "BUSY"
    end

    struct Reader
      def initialize(@data : Bytes)
      end

      def at_end? : Bool
        @data.empty?
      end

      def skip_to_end : Bytes
        @data, _ = @data + @data.size, @data
      end

      def read?(seq : Bytes) : Bool
        unless @data.starts_with?(seq)
          return false
        end

        @data += seq.size
        true
      end

      def read?(seq : String) : Bool
        read?(seq.to_slice)
      end

      def read?(cls : MsgId.class) : MsgId?
        cursor = @data
        repr = 0u64

        16.times do |size|
          break unless byte = cursor.first?
          break unless digit = byte.chr.hexdigit?

          repr <<= 4
          repr |= digit.to_u64
          cursor += 1
        end

        @data = cursor

        MsgId.new(repr)
      end

      def transaction(&)
        backup = @data

        begin
          result = yield
        ensure
          if result.nil?
            @data = backup
          end
        end
      end

      def read?(cls : Data.class) : Data?
        transaction do
          next unless read?("DATA ")
          next unless msgid = read?(MsgId)
          next unless read?(" ")

          payload = skip_to_end

          Data.new(msgid, Term::Blob.new(payload))
        end
      end

      def read?(cls : Accept.class) : Accept?
        transaction do
          next unless read?("ACCEPT ")
          next unless msgid = read?(MsgId)
          next unless at_end?

          Accept.new(msgid)
        end
      end

      def read?(cls : Ready.class) : Ready?
        transaction do
          next unless read?("READY") && at_end?

          Ready.new
        end
      end

      def read?(cls : Busy.class) : Busy?
        transaction do
          next unless read?("BUSY") && at_end?

          Busy.new
        end
      end

      def read?(cls : Frame.class) : Frame?
        read?(Data) || read?(Accept) || read?(Ready) || read?(Busy)
      end
    end

    def deserialize?(data : Bytes) : Frame?
      r = Reader.new(data)
      r.read?(Frame)
    end

    class Error < Exception
    end

    def deserialize(data : Bytes) : Frame
      deserialize?(data) || raise Error.new("invalid portal protocol frame")
    end
  end
end
