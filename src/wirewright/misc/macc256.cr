module Ww
  # WARNING: there is nothing "cryptographic" or "cryptographically secure" about this.
  # That it doesn't collide is a miracle of math and probability. Refer to e.g. Bitcoin's
  # [MuHash](https://github.com/bitcoin/bitcoin/blob/ca1ce52a0f1eb9a9d73c21a64b76ef1276511e7d/src/crypto/muhash.cpp)
  # for something secure and proven. We are just messing around!
  struct Macc256
    # :nodoc:
    PRIME = 2u128 ** (127 - 1)

    # :nodoc:
    IV = begin
      buffer = uninitialized UInt128[4]
      Random::Secure.random_bytes(buffer.to_slice.unsafe_slice_of(UInt8))
      buffer
    end

    def initialize
      @sum0 = IV[0]
      @sum1 = IV[1]
      @sq0 = IV[2]
      @sq1 = IV[3]
    end

    # :nodoc:
    def initialize(@sum0 : UInt128, @sum1 : UInt128, @sq0 : UInt128, @sq1 : UInt128)
    end

    def h256 : Term::H256
      digest = Term::H256::ALGORITHM.new

      scratch = uninitialized UInt128[4]
      scratch[0] = @sum0
      scratch[1] = @sum1
      scratch[2] = @sq0
      scratch[3] = @sq1

      bytes = scratch.to_slice.unsafe_slice_of(UInt8)
      digest.update(bytes)
      digest.final(bytes[0...32])

      blks = bytes.unsafe_slice_of(UInt64)

      Term::H256.new(blks[0], blks[1], blks[2], blks[3])
    end

    private def addp(a : UInt128, b : UInt128)
      (a &+ b) % PRIME
    end

    private def subp(a : UInt128, b : UInt128)
      if a > b
        a &- b
      else
        (a &+ PRIME) &- b
      end
    end

    def add(term : Term) : Macc256
      h256 = Term.hashcode256(term)
      blk0, blk1 = h256.blks128

      Macc256.new(
        sum0: addp(@sum0, blk0),
        sum1: addp(@sum1, blk1),
        sq0: addp(@sq0, blk0 &** 2),
        sq1: addp(@sq1, blk1 &** 2),
      )
    end

    def delete(term : Term) : Macc256
      h256 = Term.hashcode256(term)
      blk0, blk1 = h256.blks128

      Macc256.new(
        sum0: subp(@sum0, blk0),
        sum1: subp(@sum1, blk1),
        sq0: subp(@sq0, blk0 &** 2),
        sq1: subp(@sq1, blk1 &** 2),
      )
    end
  end
end
