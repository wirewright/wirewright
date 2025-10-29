module Ww
  # Implements the netstring encoding to send strings back and forth between
  # network nodes. We use this specifically to send strings between IOs; on
  # the other hand with e.g. web sockets, we do not use netstrings since web
  # sockets handle strings already.
  #
  # https://cr.yp.to/proto/netstrings.txt
  module NetString
    # Maximum allowed string bytesize.
    MAX_BYTESIZE = 256 * 1024 # 256 KB

    # Raised when a string cannot be encoded. See `encode`.
    class EncodeError < Exception
    end

    # Raised when decoding fails due to invalid encoding. See `decode`.
    class DecodeError < Exception
    end

    # Encodes *payload* using the netstring format and writes the encoded version
    # to *io*.
    #
    # Raises `EncodeError` if an encoding error occurred, e.g. the string is too long.
    def self.encode(io : IO, payload : Bytes) : Nil
      bytesize = payload.size

      if bytesize > MAX_BYTESIZE
        raise EncodeError.new
      end

      bytesize.to_s(io, base: 10)

      io << ':'
      io.write(payload)
      io << ','
    end

    def self.encode(io : IO, payload : String) : Nil
      encode(io, payload.to_slice)
    end

    # Decodes a string from *io*. Returns `nil` if encountered EOF while decoding.
    # Raises `DecodeError` in case of an invalid encoding.
    def self.decode?(io : IO, cls : Bytes.class) : Bytes?
      return unless chr = io.read_char # non-EOF

      unless '0' <= chr <= '9'
        raise DecodeError.new
      end

      bytesize = chr - '0'

      while true
        if bytesize > MAX_BYTESIZE
          raise DecodeError.new
        end

        return unless chr = io.read_char # non-EOF

        if '0' <= chr <= '9'
          bytesize = bytesize * 10u32 + (chr - '0')
        elsif chr == ':'
          break
        else
          raise DecodeError.new
        end
      end

      begin
        payload = Bytes.new(bytesize)

        io.read(payload)
      rescue IO::EOFError
        return
      end

      return unless chr = io.read_char # non-EOF

      unless chr == ','
        raise DecodeError.new
      end

      payload
    end

    def self.decode?(io : IO, cls : String.class) : String?
      return unless data = decode?(io, Bytes)

      String.new(data)
    end
  end
end
