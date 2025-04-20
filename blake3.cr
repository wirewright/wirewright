@[Link(ldflags: "#{__DIR__}/libblake3.so")]
lib LibBlake3
  BLAKE3_BLOCK_LEN = 64
  BLAKE3_OUT_LEN = 32
  BLAKE3_MAX_DEPTH = 54
  BLAKE3_CV_SIZE = (BLAKE3_MAX_DEPTH + 1) * BLAKE3_OUT_LEN

  struct ChunkState
    cv : UInt32[8]
    chunk_counter : UInt64
    buf : UInt8[BLAKE3_BLOCK_LEN]
    buf_len : UInt8
    blocks_compressed : UInt8
    flags : UInt8
  end

  struct Hasher
    key : UInt32[8]
    chunk : ChunkState
    cv_stack_len : UInt8
    cv_stack : UInt8[BLAKE3_CV_SIZE]
  end

  fun hasher_init = blake3_hasher_init(state : Hasher*) : Void
  fun hasher_update = blake3_hasher_update(state : Hasher*, input : Void*, size : LibC::SizeT) : Void
  fun hasher_finalize = blake3_hasher_finalize(state : Hasher*, output : Void*, size : LibC::SizeT) : Void
  fun hasher_reset = blake3_hasher_reset(state : Hasher*) : Void
end

class Blake3
  def initialize
    @state = uninitialized LibBlake3::Hasher

    LibBlake3.hasher_init(pointerof(@state))
  end

  def self.final(*inputs, to output : Bytes) : Nil
    unless output.size == 32
      raise ArgumentError.new("unexpected output size")
    end

    instance = new
    inputs.each do |input|
      instance.update(input)
    end
    instance.final(output.to_voidptr)
  end

  def update(input : UInt8)
    LibBlake3.hasher_update(pointerof(@state), pointerof(input), 1)
  end

  def update(input : Void*, size)
    LibBlake3.hasher_update(pointerof(@state), input, size)
  end

  def update(input : Bytes | StaticArray(UInt8, _))
    update(input.to_unsafe.as(Void*), input.size)
  end

  def reset
    LibBlake3.hasher_reset(pointerof(@state))
  end

  def final(target : Void*) : Nil
    LibBlake3.hasher_finalize(pointerof(@state), target, 32)
  end

  def final : StaticArray(UInt8, 32)
    digest = uninitialized UInt8[32]
    LibBlake3.hasher_finalize(pointerof(@state), digest, 32)
    digest
  end

  def blockfinal : UInt64[4]
    digest = uninitialized UInt64[4]
    LibBlake3.hasher_finalize(pointerof(@state), digest, 32)
    digest
  end
end

# hasher = uninitialized LibBlake3::Hasher
# LibBlake3.hasher_init(pointerof(hasher))
# LibBlake3.hasher_update(pointerof(hasher), "helloworld".to_unsafe, 10)

# output = uninitialized UInt8[32]
# LibBlake3.hasher_finalize(pointerof(hasher), pointerof(output).as(Void*), 32)
# LibBlake3.hasher_update(pointerof(hasher), "world".to_unsafe, 5)

# output2 = uninitialized UInt8[32]
# LibBlake3.hasher_finalize(pointerof(hasher), pointerof(output2).as(Void*), 32)

# pp output
# pp output2
