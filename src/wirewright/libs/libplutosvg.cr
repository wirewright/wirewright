# :showdoc:
#
# Reference: https://github.com/sammycage/plutosvg
#
# WARNING: I don't know if PlutoSVG is thread-safe or not (in the sense of maybe
# using  some kind of mutable global). In any case, it's best to assume it's
# thread-unsafe and lock appropriately.
{% if flag?(:syslibs) %}
  @[Link("plutosvg")]
  @[Link("plutovg")]
{% else %}
  @[Link(ldflags: "#{__DIR__}/../../../vendor/plutosvg/lib/libplutosvg.a #{__DIR__}/../../../vendor/plutovg/lib/libplutovg.a")]
{% end %}
lib PlutoSVG
  type Document = Void*

  fun document_load_from_data = plutosvg_document_load_from_data(
    data : UInt8*,
    length : LibC::Int,
    width : LibC::Float,
    height : LibC::Float,
    destroy_func : Void*,
    closure : Void*,
  ) : Document

  fun document_render = plutosvg_document_render(
    document : Document,
    id : UInt8*,
    canvas : PlutoVG::Canvas,
    current_color : PlutoVG::Color*,
    palette_func : Void*,
    closure : Void*,
  ) : Bool

  fun document_destroy = plutosvg_document_destroy(document : Document)

  fun document_extents = plutosvg_document_extents(
    document : Document,
    id : UInt8*,
    extents : PlutoVG::Rect*,
  ) : Bool
end
