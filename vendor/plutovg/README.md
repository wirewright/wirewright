# PlutoVG

Reference: https://github.com/sammycage/plutovg

PlutoVG at commit `dabf47c`. I've added the following functions:

- `void plutovg_font_face_get_kerning(plutovg_font_face_t* face, float size, int codepoint1, int codepoint2, int* kerning)`
- `void plutovg_font_face_pixels_to_scale(plutovg_font_face_t* face, float pixels, float* scale)`
- `void plutovg_font_face_get_glyph_metrics_by_index(plutovg_font_face_t* face, float size, int index, float* advance_width, float* left_side_bearing, plutovg_rect_t* extents)`
- `int plutovg_font_face_get_glyph_index(plutovg_font_face_t* face, plutovg_codepoint_t codepoint)`
- `float plutovg_font_face_get_glyph_path_by_index(plutovg_font_face_t* face, float size, float x, float y, int index, plutovg_path_t* path)`
- `float plutovg_font_face_traverse_glyph_path_by_index(plutovg_font_face_t* face, float size, float x, float y, int index, plutovg_path_traverse_func_t traverse_func, void* closure)`