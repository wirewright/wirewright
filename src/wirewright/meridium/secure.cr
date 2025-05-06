module Ww::Meridium
  # Implements encryption/decryption of stimuli for secure transport.
  module Secure
    extend self

    Log = ::Log.for(self)

    enum Alg : UInt8
      ChaCha20_Poly1305
    end

    def encrypt(alg : Alg, secret : Term)
      case alg
      in .cha_cha20_poly1305?
        cipher = OpenSSL::Cipher.new("ChaCha20-Poly1305")
        cipher.encrypt
        cipher.iv = iv = Random::Secure.random_bytes(12)
        cipher.key = Digest::Blake3.digest(Meridium.secret_slice(secret)) # 32 bytes
        {cipher, iv}
      end
    end

    def decrypt?(alg : Alg, secret : Term, iv : Bytes)
      return unless iv.size == 12

      case alg
      in .cha_cha20_poly1305?
        cipher = OpenSSL::Cipher.new("ChaCha20-Poly1305")
        cipher.decrypt
        cipher.iv = iv
        cipher.key = Digest::Blake3.digest(Meridium.secret_slice(secret)) # 32 bytes
        cipher
      end
    end

    def to_secure?(alg : Alg, secret : Term, value : Term) : {Bytes, Bytes}?
      begin
        cipher, iv = encrypt(alg, secret)
      rescue e : OpenSSL::Cipher::Error | ArgumentError
        Log.warn(exception: e) { "could not encrypt" }
        return
      end

      plaintext = IO::Memory.new

      ML.compact(plaintext, value)

      checksum = Digest::CRC32.checksum(plaintext.to_slice)
      plaintext.write_bytes(checksum, IO::ByteFormat::BigEndian)

      # Encrypt
      ciphertext = IO::Memory.new
      ciphertext.write cipher.update(plaintext.to_slice)
      ciphertext.write cipher.final

      {iv, ciphertext.to_slice}
    end

    def from_secure?(alg : Alg, secret : Term, iv : Bytes, ciphertext : Bytes) : Term?
      begin
        unless cipher = decrypt?(alg, secret, iv)
          Log.debug { "failed to create cipher from secret and iv" }
          return
        end

        plaintext = IO::Memory.new
        plaintext.write cipher.update(ciphertext)
        plaintext.write cipher.final
      rescue e : OpenSSL::Cipher::Error | ArgumentError
        Log.warn(exception: e) { "could not decrypt" }
        return
      end

      plaintext = plaintext.to_slice

      if plaintext.size < 1 + sizeof(UInt32)
        Log.debug { "too small" }
        return
      end

      checksum0 = IO::ByteFormat::BigEndian.decode(UInt32, plaintext[-sizeof(UInt32)...])
      checksum1 = Digest::CRC32.checksum(plaintext[...-sizeof(UInt32)])

      unless checksum0 == checksum1
        Log.debug { "checksum mismatch (#{checksum0} != #{checksum1})" }
        return
      end

      ml = String.new(plaintext[0...-sizeof(UInt32)])

      begin
        ML.term(ml)
      rescue e : ML::SyntaxError
        Log.debug(exception: e) { "syntax error" }
      end
    end
  end
end
