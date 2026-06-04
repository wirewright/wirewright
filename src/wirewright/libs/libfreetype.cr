@[Link("freetype")]
lib FreeType
  type Library = Void*
  type Face = Void*
  type Outline = Void*

  LOAD_NO_HINTING = 1u32 << 1
  LOAD_NO_BITMAP  = 1u32 << 3

  alias Pos = LibC::Long

  struct Vector
    x : Pos
    y : Pos
  end

  struct OutlineFuncs
    move_to : Vector*, Void* -> LibC::Int
    line_to : Vector*, Void* -> LibC::Int
    conic_to : Vector*, Vector*, Void* -> LibC::Int
    cubic_to : Vector*, Vector*, Vector*, Void* -> LibC::Int
    shift : LibC::Int
    delta : Pos
  end

  fun init_freetype = FT_Init_FreeType(library : Library*) : LibC::Int
  fun new_memory_face = FT_New_Memory_Face(library : Library, data : LibC::Char*, length : LibC::Int, index : LibC::Int, face : Face*) : LibC::Int
  fun set_char_size = FT_Set_Char_Size(face : Face, char_width : LibC::Int, char_height : LibC::Int, horz_resolution : LibC::UInt, vert_resolution : LibC::UInt) : LibC::Int
  fun done_face = FT_Done_Face(face : Face) : LibC::Int
  fun done_freetype = FT_Done_FreeType(library : Library) : LibC::Int
  fun load_glyph = FT_Load_Glyph(face : Face, glyph_index : LibC::UInt, load_flags : UInt32) : LibC::Int
  fun get_char_index = FT_Get_Char_Index(face : Face, charcode : LibC::ULong) : LibC::UInt
end

@[Link(ldflags: "#{__DIR__}/../../../vendor/ftx/lib/libftx.a")]
lib FreeType
  fun get_glyph_metrics = FT_Get_Glyph_Metrics(face : Face, advance : Pos*, extents_x : Pos*, extents_y : Pos*, extents_w : Pos*, extents_h : Pos*)
  fun get_font_metrics = FT_Get_Font_Metrics(face : Face, ascent : Pos*, descent : Pos*, line_gap : Pos*)
  fun outline_decompose_face = FT_Outline_Decompose_Face(face : Face, func_interface : OutlineFuncs*, user : Void*) : LibC::Int
end
