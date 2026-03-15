# :showdoc:
#
# Reference: https://github.com/sammycage/plutovg
#
# WARNING: I don't know if PlutoVG is thread-safe or not (in the sense of maybe
# using  some kind of mutable global). In any case, it's best to assume it's
# thread-unsafe and lock appropriately.
@[Link(ldflags: "#{__DIR__}/libplutovg.a")]
@[Link("m")]
lib PlutoVG
  type Surface = Void*
  type Canvas = Void*
  type FontFace = Void*
  type Paint = Void*
  type Path = Void*

  struct Matrix
    a : LibC::Float
    b : LibC::Float
    c : LibC::Float
    d : LibC::Float
    e : LibC::Float
    f : LibC::Float
  end

  struct Point
    x : LibC::Float
    y : LibC::Float
  end

  struct Rect
    x : LibC::Float
    y : LibC::Float
    w : LibC::Float
    h : LibC::Float
  end

  struct Color
    r : LibC::Float
    g : LibC::Float
    b : LibC::Float
    a : LibC::Float
  end

  enum TextEncoding
    Latin1
    UTF8
    UTF16
    UTF32
  end

  enum TextureType
    Plain
    Tiled
  end

  enum Operator
    # Clears the destination (resulting in a fully transparent image).
    Clear
    # Source replaces destination.
    Src
    # Destination is kept, source is ignored.
    Dst
    # Source is composited over destination.
    SrcOver
    # Destination is composited over source.
    DstOver
    # Source within destination (only the overlapping part of source is shown).
    SrcIn
    # Destination within source.
    DstIn
    # Source outside destination (non-overlapping part of source is shown).
    SrcOut
    # Destination outside source.
    DstOut
    # Source atop destination (source shown over destination but only in the destination's bounds).
    SrcAtop
    # Destination atop source (destination shown over source but only in the source's bounds).
    DstAtop
    # Source and destination are combined, but their overlapping regions are cleared.
    Xor
  end

  enum FillRule
    NonZero
    EvenOdd
  end

  fun surface_create = plutovg_surface_create(width : LibC::Int, height : LibC::Int) : Surface
  fun surface_load_from_image_data = plutovg_surface_load_from_image_data(data : Void*, length : LibC::Int) : Surface
  fun surface_destroy = plutovg_surface_destroy(surface : Surface)
  fun surface_write_to_png = plutovg_surface_write_to_png(surface : Surface, filename : UInt8*) : Bool
  fun surface_create_for_data = plutovg_surface_create_for_data(data : UInt8*, width : LibC::Int, height : LibC::Int, stride : LibC::Int) : Surface

  fun surface_get_data = plutovg_surface_get_data(surface : Surface) : UInt8*
  fun surface_get_stride = plutovg_surface_get_stride(surface : Surface) : LibC::Int
  fun surface_get_width = plutovg_surface_get_width(surface : Surface) : LibC::Int
  fun surface_get_height = plutovg_surface_get_height(surface : Surface) : LibC::Int
  fun surface_clear = plutovg_surface_clear(surface : Surface, color : Color*)

  fun face_from_file = plutovg_font_face_load_from_file(filename : UInt8*, ttcindex : LibC::Int) : FontFace
  fun face_from_data = plutovg_font_face_load_from_data(data : UInt8*, length : LibC::Int, ttcindex : LibC::Int, destroy_func : Void*, closure : Void*) : FontFace
  fun face_destroy = plutovg_font_face_destroy(face : FontFace)
  fun face_get_glyph_metrics = plutovg_font_face_get_glyph_metrics(face : FontFace, size : LibC::Float, codepoint : UInt32, advance_width : Float32*, left_side_bearing : Float32*, extents : Rect*)
  fun face_get_kerning = plutovg_font_face_get_kerning(face : FontFace, size : LibC::Float, codepoint1 : UInt32, codepoint2 : UInt32, kerning : Int32*)

  fun canvas_create = plutovg_canvas_create(surface : Surface) : Canvas
  fun canvas_destroy = plutovg_canvas_destroy(canvas : Canvas)
  fun canvas_fill = plutovg_canvas_fill(canvas : Canvas)
  fun canvas_paint = plutovg_canvas_paint(canvas : Canvas)
  fun canvas_stroke = plutovg_canvas_stroke(canvas : Canvas)
  fun canvas_stroke_preserve = plutovg_canvas_stroke_preserve(canvas : Canvas)
  fun canvas_save = plutovg_canvas_save(canvas : Canvas)
  fun canvas_restore = plutovg_canvas_restore(canvas : Canvas)
  fun canvas_fill_text = plutovg_canvas_fill_text(canvas : Canvas, text : Void*, length : LibC::Int, encoding : TextEncoding, x : LibC::Float, y : LibC::Float) : LibC::Float
  fun canvas_add_text = plutovg_canvas_add_text(canvas : Canvas, text : Void*, length : LibC::Int, encoding : TextEncoding, x : LibC::Float, y : LibC::Float) : LibC::Float
  fun canvas_set_font = plutovg_canvas_set_font(canvas : Canvas, face : FontFace, size : LibC::Float)
  fun canvas_set_paint = plutovg_canvas_set_paint(canvas : Canvas, paint : Paint)
  fun canvas_set_opacity = plutovg_canvas_set_opacity(canvas : Canvas, opacity : LibC::Float)
  fun canvas_scale = plutovg_canvas_scale(canvas : Canvas, sx : LibC::Float, sy : LibC::Float)
  fun canvas_translate = plutovg_canvas_translate(canvas : Canvas, tx : LibC::Float, ty : LibC::Float)
  fun canvas_add_path = plutovg_canvas_add_path(canvas : Canvas, path : Path)
  fun canvas_add_rect = plutovg_canvas_rect(canvas : Canvas, x : LibC::Float, y : LibC::Float, w : LibC::Float, h : LibC::Float)
  fun canvas_add_glyph = plutovg_canvas_add_glyph(canvas : Canvas, codepoint : UInt32, x : LibC::Float, y : LibC::Float)
  fun canvas_set_color = plutovg_canvas_set_color(canvas : Canvas, color : Color*)
  fun canvas_set_fill_rule = plutovg_canvas_set_fill_rule(canvas : Canvas, winding : FillRule)

  fun canvas_set_rgba = plutovg_canvas_set_rgba(canvas : Canvas, r : LibC::Float, g : LibC::Float, b : LibC::Float, a : LibC::Float)
  fun canvas_set_texture = plutovg_canvas_set_texture(canvas : Canvas, surface : Surface, type : TextureType, opacity : LibC::Float, matrix : Matrix*)

  enum SpreadMethod
    Pad
    Reflect
    Repeat
  end

  struct GradientStop
    offset : LibC::Float
    color : Color
  end

  fun canvas_set_linear_gradient = plutovg_canvas_set_linear_gradient(
    canvas : Canvas,
    x1 : LibC::Float,
    y1 : LibC::Float,
    x2 : LibC::Float,
    y2 : LibC::Float,
    spread : SpreadMethod,
    stops : GradientStop*,
    nstops : LibC::Int,
    matrix : Matrix*,
  )

  fun canvas_set_radial_gradient = plutovg_canvas_set_radial_gradient(
    canvas : Canvas,
    cx : LibC::Float,
    cy : LibC::Float,
    cr : LibC::Float,
    fx : LibC::Float,
    fy : LibC::Float,
    fr : LibC::Float,
    spread : SpreadMethod,
    stops : GradientStop*,
    nstops : LibC::Int,
    matrix : Matrix*,
  )

  fun canvas_close_path = plutovg_canvas_close_path(canvas : Canvas)
  fun canvas_set_line_width = plutovg_canvas_set_line_width(canvas : Canvas, line_width : LibC::Float)
  fun canvas_clip_rect = plutovg_canvas_clip_rect(canvas : Canvas, x : LibC::Float, y : LibC::Float, w : LibC::Float, h : LibC::Float)
  fun canvas_move_to = plutovg_canvas_move_to(canvas : Canvas, x : LibC::Float, y : LibC::Float)
  fun canvas_line_to = plutovg_canvas_line_to(canvas : Canvas, x : LibC::Float, y : LibC::Float)
  fun canvas_arc_to = plutovg_canvas_arc_to(canvas : Canvas, rx : LibC::Float, ry : LibC::Float, angle : LibC::Float, large_arc_flag : Bool, sweep_flag : Bool, x : LibC::Float, y : LibC::Float)
  fun canvas_clip = plutovg_canvas_clip(canvas : Canvas)
  fun canvas_clip_preserve = plutovg_canvas_clip_preserve(canvas : Canvas)
  fun canvas_fill_preserve = plutovg_canvas_fill_preserve(canvas : Canvas)
  fun canvas_rotate = plutovg_canvas_rotate(canvas : Canvas, angle : LibC::Float)
  fun canvas_transform = plutovg_canvas_transform(canvas : Canvas, matrix : Matrix*)
  fun canvas_set_matrix = plutovg_canvas_set_matrix(canvas : Canvas, matrix : Matrix*)
  fun canvas_reset_matrix = plutovg_canvas_reset_matrix(canvas : Canvas)

  fun canvas_set_operator = plutovg_canvas_set_operator(canvas : Canvas, operator : Operator)

  fun canvas_fill_rect = plutovg_canvas_fill_rect(canvas : Canvas, x : LibC::Float, y : LibC::Float, w : LibC::Float, h : LibC::Float)
  fun canvas_stroke_rect = plutovg_canvas_stroke_rect(canvas : Canvas, x : LibC::Float, y : LibC::Float, w : LibC::Float, h : LibC::Float)
  fun canvas_map = plutovg_canvas_map(canvas : Canvas, x : LibC::Float, y : LibC::Float, xx : LibC::Float*, yy : LibC::Float*)

  fun font_face_text_extents = plutovg_font_face_text_extents(face : FontFace, size : LibC::Float, text : Void*, length : LibC::Int, encoding : TextEncoding, extent : Rect*)

  fun convert_argb_to_rgba = plutovg_convert_argb_to_rgba(dst : UInt8*, src : UInt8*, width : LibC::Int, height : LibC::Int, stride : LibC::Int)
  fun canvas_round_rect = plutovg_canvas_round_rect(canvas : Canvas, x : LibC::Float, y : LibC::Float, w : LibC::Float, h : LibC::Float, rx : LibC::Float, ry : LibC::Float)

  fun matrix_rotate = plutovg_matrix_rotate(matrix : Matrix*, angle : LibC::Float)
  fun matrix_scale = plutovg_matrix_scale(matrix : Matrix*, sx : LibC::Float, sy : LibC::Float)
  fun matrix_translate = plutovg_matrix_translate(matrix : Matrix*, tx : LibC::Float, ty : LibC::Float)
  fun matrix_init_identity = plutovg_matrix_init_identity(matrix : Matrix*)
  fun matrix_map_point = plutovg_matrix_map_point(matrix : Matrix*, src : Point*, dst : Point*)

  fun paint_create_rgba = plutovg_paint_create_rgba(r : LibC::Float, g : LibC::Float, b : LibC::Float, a : LibC::Float) : Paint
  fun paint_create_texture = plutovg_paint_create_texture(surface : Surface, type : TextureType, opacity : LibC::Float, matrix : Matrix*) : Paint
  fun paint_create_linear_gradient = plutovg_paint_create_linear_gradient(
    x1 : LibC::Float,
    y1 : LibC::Float,
    x2 : LibC::Float,
    y2 : LibC::Float,
    spread : SpreadMethod,
    stops : GradientStop*,
    nstops : LibC::Int,
    matrix : Matrix*,
  ) : Paint
  fun paint_create_radial_gradient = plutovg_paint_create_radial_gradient(
    cx : LibC::Float,
    cy : LibC::Float,
    cr : LibC::Float,
    fx : LibC::Float,
    fy : LibC::Float,
    fr : LibC::Float,
    spread : SpreadMethod,
    stops : GradientStop*,
    nstops : LibC::Int,
    matrix : Matrix*,
  ) : Paint
  fun paint_destroy = plutovg_paint_destroy(paint : Paint)
end
