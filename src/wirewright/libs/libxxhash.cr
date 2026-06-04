{% if flag?(:syslibs) %}
  @[Link("xxhash")]
{% else %}
  @[Link(ldflags: "#{__DIR__}/../../../vendor/xxhash/lib/libxxhash.a")]
{% end %}
lib LibXXH64
  type State = Void*

  fun hashcode = XXH3_64bits(input : Void*, length : LibC::SizeT) : UInt64
end
