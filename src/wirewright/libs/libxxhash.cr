{% if flag?(:syslibs) %}
  @[Link("xxhash")]
{% else %}
  @[Link(ldflags: "#{__DIR__}/../../../vendor/xxhash/lib/libxxhash.a")]
{% end %}
lib LibXXH64
  fun hashcode = XXH3_64bits(input : Void*, length : LibC::SizeT) : UInt64
end

{% if flag?(:syslibs) %}
  @[Link("xxhash")]
{% else %}
  @[Link(ldflags: "#{__DIR__}/../../../vendor/xxhash/lib/libxxhash.a")]
{% end %}
lib LibXXH128
  type State = Void*

  enum ErrorCode
    Ok    = 0
    Error
  end

  struct Hash128
    low64 : UInt64
    high64 : UInt64
  end

  fun create_state = XXH3_createState : State
  fun free_state = XXH3_freeState(state : State) : ErrorCode
  fun reset = XXH3_128bits_reset_withSeed(state : State, seed : UInt64) : ErrorCode
  fun update = XXH3_128bits_update(state : State, input : UInt8*, len : LibC::SizeT) : ErrorCode
  fun digest = XXH3_128bits_digest(state : State) : Hash128
end
