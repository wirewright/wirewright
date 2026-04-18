@[Link("freetype")]
lib FreeType
  type Library = Void*
  type Face = Void*

  fun init_freetype = FT_Init_FreeType(library : Library*) : LibC::Int
  fun new_memory_face = FT_New_Memory_Face(library : Library, data : LibC::Char*, length : LibC::Int, index : LibC::Int, face : Face*) : LibC::Int
  fun set_char_size = FT_Set_Char_Size(face : Face, char_width : LibC::Int, char_height : LibC::Int, horz_resolution : LibC::UInt, vert_resolution : LibC::UInt) : LibC::Int
  fun done_face = FT_Done_Face(face : Face) : LibC::Int
  fun done_freetype = FT_Done_FreeType(library : Library) : LibC::Int
  fun load_glyph = FT_Load_Glyph(face : Face, glyph_index : LibC::UInt, load_flags : Int32) : LibC::Int
end
