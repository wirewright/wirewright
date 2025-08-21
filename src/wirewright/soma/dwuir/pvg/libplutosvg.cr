@[Link(ldflags: "#{__DIR__}/libplutosvg.a")]
lib PlutoSVG
  type Document = Void*

  fun document_load_from_data = plutosvg_document_load_from_data(data : UInt8*, length : LibC::Int, width : LibC::Float, height : LibC::Float,
                                                                 destroy_func : Void*, closure : Void*) : Document

  fun document_render = plutosvg_document_render(document : Document, id : UInt8*, canvas : PlutoVG::Canvas, current_color : PlutoVG::Color*, palette_func : Void*, closure : Void*) : Bool
  fun document_destroy = plutosvg_document_destroy(document : Document)
  fun document_extents = plutosvg_document_extents(document : Document, id : UInt8*, extents : PlutoVG::Rect*) : Bool
end
