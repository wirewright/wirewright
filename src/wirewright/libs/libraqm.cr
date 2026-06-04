@[Link("fribidi")]
@[Link("harfbuzz")]
{% if flag?(:syslibs) %}
  @[Link("raqm")]
{% else %}
  @[Link(ldflags: "#{__DIR__}/../../../vendor/raqm/lib/libraqm.a")]
{% end %}
lib Raqm
  type Handle = Void*

  struct Glyph
    index : LibC::UInt
    x_advance : LibC::Int
    y_advance : LibC::Int
    x_offset : LibC::Int
    y_offset : LibC::Int
    cluster : UInt32
    ftface : FreeType::Face
  end

  enum Direction
    Default
    RTL
    LTR
    TTB
  end

  fun create = raqm_create : Handle
  fun destroy = raqm_destroy(rq : Handle)
  fun set_text_utf8 = raqm_set_text_utf8(rq : Handle, text : LibC::Char*, len : LibC::SizeT) : Bool
  fun set_ft_face = raqm_set_freetype_face(rq : Handle, face : FreeType::Face) : Bool
  fun set_letter_spacing_range = raqm_set_letter_spacing_range(rq : Handle, spacing : LibC::Int, start : LibC::SizeT, len : LibC::SizeT) : Bool
  fun layout = raqm_layout(rq : Handle) : Bool
  fun get_glyphs = raqm_get_glyphs(rq : Handle, length : LibC::SizeT*) : Glyph*
  fun clear_contents = raqm_clear_contents(rq : Handle)
end
