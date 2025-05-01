module Ww::Meridium
  # Atom is currently a 256-bit hash split into 4 64-bit blocks. For some reason
  # this appears to be faster than passing the bytes as-is (e.g. as `u8[16]`).
  struct Atom
    include Comparable(Atom)

    # Algorithm used for hashing. Must be cryptographically secure.
    alias Hasher = Digest::Blake3

    # Bytesize of hashes produced by `Hasher`.
    BYTESIZE = 32

    def initialize(@blk0 : UInt64, @blk1 : UInt64, @blk2 : UInt64, @blk3 : UInt64)
    end

    # Constructs an atom from a byteslice *digest*.
    def self.of(digest : Bytes) : Atom
      unless digest.size == 32
        raise ArgumentError.new("expected digest to be 32 bytes")
      end

      blk0, blk1, blk2, blk3 = digest.unsafe_slice_of(UInt64)

      new(blk0, blk1, blk2, blk3)
    end

    # Constructs an atom from *string* by hashing that string and so on.
    def self.of(string : String) : Atom
      of(Hasher.digest(string))
    end

    # Generates a random atom using *random*.
    #
    # Atom bytes are generated as-is, without hashing.
    def self.rand(random : Random = Random::DEFAULT) : Atom
      new(random.rand(UInt64), random.rand(UInt64), random.rand(UInt64), random.rand(UInt64))
    end

    def <=>(other : Atom)
      {@blk0, @blk1, @blk2, @blk3} <=> {other.@blk0, other.@blk1, other.@blk2, other.@blk3}
    end

    # Copies the bytes of the hash to *target*.
    def copy_hash_to(target : Bytes) : Nil
      blks = target.unsafe_slice_of(UInt64)
      blks[0] = @blk0
      blks[1] = @blk1
      blks[2] = @blk2
      blks[3] = @blk3
    end

    def inspect(io)
      io << "Atom["
      @blk0.to_s(io, base: 32, precision: 13, upcase: true)
      io << "-"
      @blk1.to_s(io, base: 32, precision: 13, upcase: true)
      io << "-"
      @blk2.to_s(io, base: 32, precision: 13, upcase: true)
      io << "-"
      @blk3.to_s(io, base: 32, precision: 13, upcase: true)
      io << "]"
    end

    def hash(hasher)
      @blk0.hash(hasher)
    end

    def_equals @blk0, @blk1, @blk2, @blk3
  end
end
