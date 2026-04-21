@[Link(ldflags: "#{__DIR__}/../../../vendor/unibreak/lib/libunibreak.a")]
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
