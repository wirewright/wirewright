{% if flag?(:syslibs) %}
  @[Link("unibreak")]
{% else %}
  @[Link(ldflags: "#{__DIR__}/../../../vendor/unibreak/lib/libunibreak.a")]
{% end %}
lib Unibreak
  enum LineBreak : UInt8
    MustBreak
    AllowBreak
    NoBreak
    Unfinished
    Indeterminate
  end

  fun set_linebreaks_utf8(text : UInt8*, len : LibC::SizeT, lang : LibC::Char*, brks : LineBreak*)
end
